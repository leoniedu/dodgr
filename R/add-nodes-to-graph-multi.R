#' Add nodes to a graph, allowing multiple points per edge
#'
#' This function extends `add_nodes_to_graph` by supporting multiple points on the same edge,
#' splitting edges as needed, and connecting every point to the closest (possibly new) vertex.
#'
#' @param graph A dodgr street network data frame.
#' @param xy A two-column matrix or data frame of coordinates (x, y).
#' @param highway Character; highway type for new edges (see weight_streetnet).
#' @param wt_profile Character; weight profile to use for new edges (see weight_streetnet).
#' @param wt_profile_file Character; custom weight profile CSV file (see weight_streetnet).
#' @param debug Logical; if TRUE, print simple debug output.
#' @param ... Additional arguments (reserved for future compatibility).
#' @return Modified graph with new nodes and edges.
#' @export
#' Robust version of calc_edge_time that works by column name
calc_edge_time_by_name <- function(edge, wt_profile) {
    required <- c("d", "d_weighted", "maxspeed")
    missing <- setdiff(required, names(edge))
    if (length(missing) > 0) {
        stop("Missing columns in edge: ", paste(missing, collapse = ", "))
    }
    speed_m_per_s <- edge$maxspeed * 1000 / 3600
    edge$time <- edge$d / speed_m_per_s
    edge$time_weighted <- edge$d_weighted / speed_m_per_s
    if ("dz" %in% names(edge) && wt_profile %in% c("foot", "bicycle")) {
        # Optionally handle incline adjustment here if needed
        # edge <- times_by_incline(edge, wt_profile)
    }
    edge$maxspeed <- NULL
    edge
}

add_nodes_to_graph_multi <- function(
    graph,
    xy,
    highway = "primary",
    wt_profile = NULL,
    wt_profile_file = NULL,
    debug = FALSE,
    dist_tol = 1e-6,
    intersections_only = FALSE,
    dist_min = dist_tol#, ...
) {
    if (debug) cat("Starting add_nodes_to_graph_multi with", nrow(xy), "points\n")
    
    # Step 1: Standardize graph columns and preprocess points
    gr_cols <- dodgr_graph_cols(graph)
    gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
    graph_std <- graph[, gr_cols]
    names(graph_std) <- names(gr_cols)
    
    xy <- pre_process_xy(xy)
    xy_unique <- unique(xy)  # Remove duplicated input points (by x/y)
    
    pts_removed <- nrow(xy) - nrow(xy_unique)
    if (pts_removed > 0) {
        warning(pts_removed, " duplicated points removed.")
    }
    
    
    # Step: Filter points based on distance to nearest vertex (only when creating edges)
    # Get all vertices from the graph
    graph_verts <- dodgr_vertices(graph_std)

    # First pass: match points to vertices
    vts <- match_points_to_verts(verts = graph_verts, xy = xy_unique)

    # Get coordinates of matched vertices
    vertex_coords <- graph_verts[vts, c("x", "y"), drop = FALSE]
        
    # Calculate distances from points to their matched vertices using paired distances
    pt_to_vertex_dists <- geodist::geodist(
        x = xy_unique,
        y = vertex_coords,
        paired = TRUE,
        measure = get_geodist_measure(graph)
        )
        
    # Keep only points that are far enough from vertices
    keep_points <- pt_to_vertex_dists > dist_min
    pts_removed_v <- sum(!keep_points)
    if (pts_removed_v > 0) {
        warning(pts_removed_v, " points removed for being within ", dist_min, 
               " units of a vertex")
        xy_unique <- xy_unique[keep_points, , drop = FALSE]
    }
    
    if (debug) cat("After vertex distance filtering:", nrow(xy_unique), "points remain\n")
    
    # Early return if no points remain after filtering
    if (nrow(xy_unique) == 0) {
        if (debug) cat("No points remain after vertex filtering, returning original graph\n")
        return(graph)
    }
    
    # Step: Match points to graph edges
    pts <- match_pts_to_graph(graph_std, xy_unique, distances = TRUE)
    pts$pt_id <- seq_len(nrow(pts))  # Assign a unique point ID to each input point
    pts$x0 <- xy_unique[, 1]
    pts$y0 <- xy_unique[, 2]
    vu <- unique(vertex_coords)
    vu$matches_vertex <- TRUE
    pts <- merge(pts, vu, by=c("x", "y"), all.x=TRUE)
    
    if (debug) cat("Points matched to", length(unique(pts$index)), "different edges\n")
    
    # Group points by edge and identify which edges need processing
    edge_matches <- split(seq_len(nrow(pts)), pts$index)
    edges_with_pts <- as.integer(names(edge_matches))
    
    # Split the graph into untouched edges vs edges that need processing
    untouched_idx <- setdiff(seq_len(nrow(graph)), edges_with_pts)
    untouched_graph <- graph[untouched_idx, , drop = FALSE]
    
    if (debug) cat("Processing", length(edges_with_pts), "edges,", nrow(untouched_graph), "edges untouched\n")
    
    # Prepare to collect split and point-to-vertex edges
    new_rows <- list()
    
    # Initialize mapping of points to split vertices
    pt_vertex_map <- data.frame(
        pt_index = integer(0),
        split_vertex_id = character(0),
        edge_idx = integer(0),
        edge_id = character(0),
        stringsAsFactors = FALSE
    )

    # For each edge with points, split and connect
    for (edge_idx in edges_with_pts) {
        edge_row_std <- graph_std[edge_idx, ]  
        matched_pts_idx <- edge_matches[[as.character(edge_idx)]]
        matched_pts <- pts[matched_pts_idx, ]

        # Sort points along the edge (use geometry from graph_std)
        start_pt <- c(edge_row_std$xfr, edge_row_std$yfr)
        end_pt <- c(edge_row_std$xto, edge_row_std$yto)
        edge_vec <- end_pt - start_pt
        edge_len <- sqrt(sum(edge_vec^2))
        
        # Calculate projection distances
        proj_dists <- vapply(seq_len(nrow(matched_pts)), function(i) {
            pt_vec <- c(matched_pts$x[i], matched_pts$y[i]) - start_pt
            sum(pt_vec * edge_vec) / edge_len
        }, numeric(1))
        
        # Sort points by their projection distance along the edge
        ord <- order(proj_dists)
        proj_dists <- proj_dists[ord]

        matched_pts <- matched_pts[ord, , drop = FALSE]
        

        # Create projection coordinates with additional metadata
        graph_verts$is_vertex <- TRUE
        proj_data <- merge(
            data.frame(
                x = matched_pts$x,
                y = matched_pts$y,
                pt_id = matched_pts$pt_id,  # Keep track of original point IDs
                stringsAsFactors = FALSE
        ),
        graph_verts, by=c("x", "y"), all.x=TRUE)
        proj_data$is_vertex <- if_else(is.na(proj_data$is_vertex), FALSE, proj_data$is_vertex)
        
        # Debug output
        if (debug && any(!proj_data$is_vertex)) {
            cat(sum(proj_data$is_vertex), "points projected to existing vertices\n")
        }
        
        # Calculate distances between consecutive points in the projection space
        proj_data$split <- TRUE
        if (nrow(proj_data) > 1) {
            # Calculate squared distances to avoid sqrt for comparison
            dx <- diff(proj_data$x)
            dy <- diff(proj_data$y)
            sq_dists <- dx^2 + dy^2
            
            # Split that are far enough from previous kept point
            proj_data$split <- sq_dists > (dist_tol^2)                                    
        } 
        # Create the final projection coordinates
        split_vertices <- proj_data[proj_data$split&(!proj_data$is_vertex), c("id", "x", "y"), drop=FALSE]
        n_split <- nrow(split_vertices)
        if (n_split > 0) {
            split_vertices$id = paste0(edge_row_std$edge_id, "_split", seq_len(n_split))
        }
        # # Map each input point to the index of its split vertex
        # proj_idx_map <- match(paste(matched_pts$x, matched_pts$y), paste(split_vertices$x, split_vertices$y))
        # 
        # # Get the vertex matches we found earlier
        # vts <- match_points_to_verts(verts = graph_verts, xy = matched_pts[, c("x", "y")])
        # 
        # # Add all points on this edge to pt_vertex_map (once per edge, not per segment)
        # if (nrow(matched_pts) > 0) {
        #     for (pt_i in seq_len(nrow(matched_pts))) {
        #         split_vertex_idx <- proj_idx_map[pt_i]
        #         
        #         # Use the original vertex ID if this point matches a vertex, otherwise use the split vertex ID
        #         vertex_id <- if (!is.na(vts[pt_i])) {
        #             as.character(graph_verts$id[vts[pt_i]])
        #         } else if (!is.na(split_vertex_idx)) {
        #             split_vertices$id[split_vertex_idx]
        #         } else {
        #             next  # Skip if no valid vertex ID found
        #         }
        #         
        #         pt_vertex_map <- rbind(
        #             pt_vertex_map,
        #             data.frame(
        #                 pt_index = matched_pts$pt_id[pt_i],
        #                 split_vertex_id = vertex_id,
        #                 edge_idx = edge_idx,
        #                 edge_id = as.character(edge_row_std$edge_id),
        #                 stringsAsFactors = FALSE
        #             )
        #         )
        #     }
        # }

        # Build segments (split edge into n+1 parts)
        segment_starts <- rbind(
            data.frame(x = edge_row_std$xfr, y = edge_row_std$yfr),
            split_vertices[, c("x", "y")]
        )
        segment_ends <- rbind(
            split_vertices[, c("x", "y")],
            data.frame(x = edge_row_std$xto, y = edge_row_std$yto)
        )
        n_segs <- nrow(segment_starts)
        for (i in seq_len(n_segs)) {
            # Always use standardized edge row for new row construction
            seg <- edge_row_std
            seg["orig_idx"] <- edge_idx  # mapping to original full graph row
            # Preserve ratios
            d_ratio <- edge_row_std$d_weighted / edge_row_std$d
            time_ratio <- edge_row_std$time_weighted / edge_row_std$time
            if (n_segs>1) {
                from_id <- if (i == 1) edge_row_std$from else split_vertices$id[i - 1]
                to_id <- if (i <= n_split) split_vertices$id[i] else edge_row_std$to
                seg_xfr <- segment_starts$x[i]
                seg_yfr <- segment_starts$y[i]
                seg_xto <- segment_ends$x[i]
                seg_yto <- segment_ends$y[i]
                # Compute segment length
                seg_len <- geodist::geodist(
                    data.frame(x = c(seg_xfr, seg_xto), y = c(seg_yfr, seg_yto)),
                    measure = get_geodist_measure(graph)
                )[1, 2]
                seg["from"] <- from_id
                seg["to"] <- to_id
                seg["xfr"] <- seg_xfr
                seg["yfr"] <- seg_yfr
                seg["xto"] <- seg_xto
                seg["yto"] <- seg_yto
                seg["d"] <- seg_len
                seg["d_weighted"] <- seg_len * d_ratio
                seg["time"] <- seg_len * (edge_row_std["time"] / edge_row_std["d"])
                seg["time_weighted"] <- seg_len * ( edge_row_std["time"] / edge_row_std["d"]) * time_ratio
                seg["edge_id"] <- paste0(edge_row_std["edge_id"], "_seg", i)
                seg$split_edge <- TRUE  # Mark as split edge for downstream separation
            } else {
                seg$split_edge <- FALSE
            }
            new_rows[[length(new_rows) + 1]] <- seg
        }
        # Connect each unique input point to its split vertex
        # For each unique input point (by coordinates)
        input_coords <- data.frame(x0 = matched_pts$x0, y0 = matched_pts$y0)
        unique_input_idx <- !duplicated(input_coords)
        

        for (i in which(unique_input_idx)) {
            # Use the robust pt_id for point-to-vertex edge ID
            pt_id <- paste0("pt_", matched_pts$pt_id[i], "_", edge_row_std$edge_id)
            v_id <- split_vertices$id[proj_idx_map[i]]
            for (dir in c("forward", "backward")) {
                # Create edge from point to vertex (bidirectional)

                pt_from <- if (dir == "forward") pt_id else v_id
                pt_to <- if (dir == "forward") v_id else pt_id
                pt_xfr <- if (dir == "forward") matched_pts$x0[i] else matched_pts$x[i]
                pt_yfr <- if (dir == "forward") matched_pts$y0[i] else matched_pts$y[i]
                pt_xto <- if (dir == "forward") matched_pts$x[i] else matched_pts$x0[i]
                pt_yto <- if (dir == "forward") matched_pts$y[i] else matched_pts$y0[i]
                pt_len <- geodist::geodist(
                    data.frame(x = c(pt_xfr, pt_xto), y = c(pt_yfr, pt_yto)),
                    measure = get_geodist_measure(graph)
                )[1, 2]
                pt_edge <- edge_row_std
                pt_edge[["from"]] <- pt_from
                pt_edge[["to"]] <- pt_to
                pt_edge[["xfr"]] <- pt_xfr
                pt_edge[["yfr"]] <- pt_yfr
                pt_edge[["xto"]] <- pt_xto
                pt_edge[["yto"]] <- pt_yto
                pt_edge$d <- pt_len
                # Weighting/profile logic
                if (!is.null(wt_profile) || !is.null(wt_profile_file)) {
                    wp <- get_profile(wt_profile = wt_profile, file = wt_profile_file)
                    way_wt <- wp$value[wp$way == highway]
                    if (length(way_wt) == 0) stop("Highway type not found in weight profile")
                    pt_edge$d_weighted <- pt_len / way_wt
                    pt_edge$highway <- highway
                    pt_edge <- set_maxspeed(pt_edge, wt_profile, wt_profile_file)
                    pt_edge <- weight_by_num_lanes(pt_edge, wt_profile)
                    pt_edge <- calc_edge_time_by_name(pt_edge, wt_profile)
                } else {
                    pt_edge$d_weighted <- pt_len * d_ratio
                    pt_edge$highway <- edge_row_std$highway
                    pt_edge$time <- pt_len * (edge_row_std$time / edge_row_std$d)
                    pt_edge$time_weighted <- pt_len * (edge_row_std$time / edge_row_std$d) * time_ratio
                }
                pt_edge["edge_id"] <- paste0("ptedge_", pt_id, "_", dir)
                pt_edge["orig_idx"] <- edge_idx  # mapping to original full graph row
                # Do NOT set split_edge for pt-to-vertex edges
                new_rows[[length(new_rows) + 1]] <- pt_edge
                pt_edge_std <- edge_row_std
                pt_edge_std["from"] <- pt_from
                pt_edge_std["to"] <- pt_to
                pt_edge_std["xfr"] <- pt_xfr
                pt_edge_std["yfr"] <- pt_yfr
                pt_edge_std["xto"] <- pt_xto
                pt_edge_std["yto"] <- pt_yto
                pt_edge_std["d"] <- pt_len
                # Weighting/profile logic as before
                pt_edge_std["d_weighted"] <- pt_edge$d_weighted
                pt_edge_std["time"] <- pt_edge$time
                pt_edge_std["time_weighted"] <- pt_edge$time_weighted
                pt_edge_std["highway"] <- pt_edge$highway
                pt_edge_std["edge_id"] <- paste0("ptedge_", pt_id, "_", dir)
                pt_edge_std["orig_idx"] <- edge_idx  # mapping to original full graph row
                new_rows[[length(new_rows) + 1]] <- pt_edge_std
            }
            
        }
        
    }
    # Keep split edges and point-to-vertex edges separate
    split_edges <- list()
    pt_edges <- list()
    for (row in new_rows) {
        if (!is.null(row$split_edge) ) {
            split_edges[[length(split_edges) + 1]] <- row
        } else if (!is.null(row$pt_edge) && row$pt_edge) {
            # Only include point-to-vertex edges if intersections_only is FALSE
            if (!intersections_only) {
                pt_edges[[length(pt_edges) + 1]] <- row
            }
        }
    }

    # --- After main loop, create all pt-to-vertex edges using pt_vertex_map ---
    # Only create point-to-vertex edges if intersections_only is FALSE
    if (!intersections_only && nrow(pt_vertex_map) > 0) {
        if (debug) {
            cat("Creating point-to-vertex edges (intersections_only = FALSE)\n")
        }
        # Step 1: Add unique point IDs and deduplicate connections
        pt_vertex_map$pt_unique_id <- pts$pt_id[pt_vertex_map$pt_index]
        unique_key <- paste0(pt_vertex_map$pt_unique_id, "_", pt_vertex_map$split_vertex_id)
        dup_connections <- duplicated(unique_key)
        if (any(dup_connections)) {
            warning(sum(dup_connections), " duplicated point-to-vertex connections found and removed.")
            pt_vertex_map <- pt_vertex_map[!dup_connections, ]
        }
        
        # Step 2: Create bidirectional edges for each connection
        for (i in seq_len(nrow(pt_vertex_map))) {
            # Extract connection info
            pt_idx <- pt_vertex_map$pt_index[i]
            vertex_id <- pt_vertex_map$split_vertex_id[i]
            edge_idx <- pt_vertex_map$edge_idx[i]
            pt_unique_id <- pt_vertex_map$pt_unique_id[i]
            
            # Skip if this is a point-to-vertex connection and intersections_only is TRUE
            if (intersections_only) {
                if (debug) {
                    cat("Skipping point-to-vertex edge for point", pt_unique_id, 
                        "(intersections_only = TRUE)\n")
                }
                next
            }
            
            # For points that match existing vertices, we create connections if intersections_only is FALSE
            if (vertex_id %in% graph_verts$id) {
                if (debug) {
                    cat("Point", pt_unique_id, "matches existing vertex", vertex_id, "(creating connection)\n")
                }
            }
            
            # Get original edge data
            orig_edge <- graph_std[edge_idx, ]
            
            # Calculate base ratios once
            d_ratio <- orig_edge$d_weighted / orig_edge$d
            time_ratio <- orig_edge$time_weighted / orig_edge$time
            time_per_distance <- orig_edge$time / orig_edge$d
            
            # Get point coordinates
            pt_x0 <- pts$x0[pt_idx]
            pt_y0 <- pts$y0[pt_idx]
            pt_x <- pts$x[pt_idx]
            pt_y <- pts$y[pt_idx]
            
            # Create point ID
            pt_id <- paste0("pt_", pt_unique_id, "_", pt_vertex_map$edge_id[i])
            
            # Create forward edge: point -> vertex
            forward_edge <- orig_edge
            forward_edge$from <- pt_id
            forward_edge$to <- vertex_id
            forward_edge$xfr <- pt_x0
            forward_edge$yfr <- pt_y0
            forward_edge$xto <- pt_x
            forward_edge$yto <- pt_y
            
            # Calculate distance and times for forward edge
            forward_dist <- geodist::geodist(
                data.frame(x = c(pt_x0, pt_x), y = c(pt_y0, pt_y)),
                measure = get_geodist_measure(graph)
            )[1, 2]
            forward_edge$d <- forward_dist
            forward_edge$d_weighted <- forward_dist * d_ratio
            forward_edge$time <- forward_dist * time_per_distance
            forward_edge$time_weighted <- forward_dist * time_per_distance * time_ratio
            forward_edge$edge_id <- paste0("ptedge_", pt_id, "_forward")
            forward_edge$orig_idx <- edge_idx
            
            # Create backward edge: vertex -> point
            backward_edge <- orig_edge
            backward_edge$from <- vertex_id
            backward_edge$to <- pt_id
            backward_edge$xfr <- pt_x
            backward_edge$yfr <- pt_y
            backward_edge$xto <- pt_x0
            backward_edge$yto <- pt_y0
            backward_edge$d <- forward_dist  # Same distance
            backward_edge$d_weighted <- forward_dist * d_ratio
            backward_edge$time <- forward_dist * time_per_distance
            backward_edge$time_weighted <- forward_dist * time_per_distance * time_ratio
            backward_edge$edge_id <- paste0("ptedge_", pt_id, "_backward")
            backward_edge$orig_idx <- edge_idx
            
            # Add both edges to list
            pt_edges[[length(pt_edges) + 1]] <- forward_edge
            pt_edges[[length(pt_edges) + 1]] <- backward_edge
        }
    }
    # Create data frames from the edge lists
    split_edges_df <- if (length(split_edges) > 0) do.call(rbind, split_edges) else NULL
    pt_edges_df <- if (length(pt_edges) > 0) do.call(rbind, pt_edges) else NULL
    
    # Check for duplicate point-to-vertex edges
    if (!is.null(pt_edges_df)) {
        dup_idx <- duplicated(pt_edges_df[, c("from", "to")])
        if (any(dup_idx)) {
            warning(sum(dup_idx), " duplicate point-to-vertex edges detected. Removing duplicates.")
            pt_edges_df <- pt_edges_df[!dup_idx, ]
        }
    }

    # Process split edges - preserve original highway values
    split_edges_final <- NULL
    if (!is.null(split_edges_df)) {
        split_edges_final <- graph[split_edges_df$orig_idx, , drop = FALSE]
        gr_cols <- dodgr_graph_cols(graph)
        gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
        
        # Update all columns except highway
        for (col_name in names(gr_cols)) {
            if (col_name != "highway") {
                split_edges_final[, gr_cols[col_name]] <- split_edges_df[[col_name]]
            }
        }
    }
    
    # Process point-to-vertex edges - use highway argument value  
    pt_edges_final <- NULL
    if (!is.null(pt_edges_df)) {
        pt_edges_final <- graph[pt_edges_df$orig_idx, , drop = FALSE]
        gr_cols <- dodgr_graph_cols(graph)
        gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
        
        # Update all columns including highway
        for (col_name in names(gr_cols)) {
            pt_edges_final[, gr_cols[col_name]] <- pt_edges_df[[col_name]]
        }
        pt_edges_final$highway <- highway
    }
    
    # Combine all edges
    result_graph <- rbind(untouched_graph, split_edges_final, pt_edges_final)
    rownames(result_graph) <- NULL
    return(result_graph)
}
