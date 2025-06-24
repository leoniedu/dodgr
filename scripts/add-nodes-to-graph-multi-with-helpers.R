# ============================================================================
# Helper Functions
# ============================================================================


#' Calculate edge times based on distance and maxspeed
#' 
#' Robust version of calc_edge_time that works by column name
#' @param edge Data frame containing edge data
#' @param wt_profile Character; weight profile name
#' @noRd
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

# ============================================================================
# Main Function
# ============================================================================

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
#' @param dist_tol Numeric; tolerance for point matching.
#' @param intersections_only Logical; only split at intersections.
#' @param dist_min Numeric; minimum distance for point filtering.
#' @return Modified graph with new nodes and edges.
#' @export
add_nodes_to_graph_multi <- function(
    graph,
    xy,
    highway = "primary",
    wt_profile = NULL,
    wt_profile_file = NULL,
    debug = FALSE,
    dist_tol = 1e-6,
    intersections_only = FALSE,
    dist_min = dist_tol
) {
    
    # 1. Input Validation and Initialization -------------------
    
    if (debug) cat("Starting add_nodes_to_graph_multi with", nrow(xy), "points\n")
    
    # ------------------------------------------------------------------------
    # 1.1 Standardize Graph and Preprocess Points
    # ------------------------------------------------------------------------
    gr_cols <- dodgr_graph_cols(graph)
    gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
    graph_std <- graph[, gr_cols]
    names(graph_std) <- names(gr_cols)
    
    # Validate and preprocess input points
    if (is.null(xy)) {
        stop("xy cannot be NULL")
    }
    
    # Convert to data.frame if it's a matrix
    if (is.matrix(xy)) {
        xy <- as.data.frame(xy)
    }
    
    # Handle sf objects
    if (inherits(xy, "sf")) {
        if (!"geometry" %in% names(xy)) {
            stop("sf object must have a 'geometry' column")
        }
        coords <- sf::st_coordinates(xy)
        xy <- data.frame(x = coords[,1], y = coords[,2])
    } 
    # Handle data.frames and other 2-column inputs
    else if (is.data.frame(xy)) {
        if (ncol(xy) < 2) {
            stop("xy must have at least 2 columns (x and y coordinates)")
        }
        # Use first two columns if more than two
        xy <- data.frame(x = xy[[1]], y = xy[[2]])
    } else {
        stop("xy must be a data.frame, matrix, or sf object with point geometries")
    }
    
    # Remove any NA coordinates
    na_rows <- is.na(xy$x) | is.na(xy$y)
    if (any(na_rows)) {
        warning(sum(na_rows), " points with NA coordinates removed")
        xy <- xy[!na_rows, ]
    }
    
    # Remove duplicate points
    xy_unique <- unique(xy)
    pts_removed <- nrow(xy) - nrow(xy_unique)
    if (pts_removed > 0) {
        message(pts_removed, " duplicate points removed")
    }
    
    # ========================================================================
    # 2. Point Processing and Vertex Matching
    # ========================================================================
    # Skip vertex distance filtering if we're only interested in intersections
    if (!intersections_only) {
        # Get all vertices from the graph for point matching
        graph_verts <- dodgr_vertices(graph_std)
        
        # Match points to nearest vertices
        vts <- match_points_to_verts(verts = graph_verts, xy = xy_unique)
        
        # Calculate distances from points to their matched vertices
        vertex_coords <- graph_verts[vts, c("x", "y"), drop = FALSE]
        pt_to_vertex_dists <- geodist::geodist(
            x = xy_unique,
            y = vertex_coords,
            paired = TRUE,
            measure = get_geodist_measure(graph)
        )
        
        # Filter points that are too close to existing vertices
        keep_points <- pt_to_vertex_dists > dist_min
        pts_removed_v <- sum(!keep_points)
        if (pts_removed_v > 0) {
            warning(pts_removed_v, " points removed for being within ", dist_min, 
                   " units of a vertex")
            xy_unique <- xy_unique[keep_points, , drop = FALSE]
            
            # Update vertex matches for remaining points
            if (nrow(xy_unique) > 0) {
                vts <- unique(match_points_to_verts(verts = graph_verts, xy = xy_unique))
                vertex_coords <- graph_verts[vts, c("x", "y"), drop = FALSE]
            }
        }
    }
    
    if (debug) cat("After vertex distance filtering:", nrow(xy_unique), "points remain\n")
    
    # Early return if no points remain after filtering
    if (nrow(xy_unique) == 0) {
        if (debug) cat("No points remain after vertex filtering, returning original graph\n")
        return(graph)
    }
    
    # ========================================================================
    # 3. Match Points to Graph and Prepare for Processing
    # ========================================================================
    # Match points to the nearest edges in the graph
    pts <- match_pts_to_graph(graph_std, xy_unique, distances = TRUE)
    pts$pt_id <- seq_len(nrow(pts))  # Assign a unique point ID to each input point
    pts$x0 <- xy_unique[, 1]  # Original x-coordinates
    pts$y0 <- xy_unique[, 2]  # Original y-coordinates
    
    if (debug) {
        cat("Points matched to", length(unique(pts$index)), "different edges\n")
    }
    
    # Group points by edge for batch processing
    edge_matches <- split(seq_len(nrow(pts)), pts$index)
    edges_with_pts <- as.integer(names(edge_matches))
    
    # Split the graph into edges that need processing vs those that don't
    untouched_idx <- setdiff(seq_len(nrow(graph)), edges_with_pts)
    untouched_graph <- graph[untouched_idx, , drop = FALSE]
    
    if (debug) {
        cat("Processing", length(edges_with_pts), "edges with points,", 
            nrow(untouched_graph), "edges untouched\n")
    }
    
    # Initialize data structures for new graph elements
    new_rows <- list()  # Will store all new edge segments
    pt_vertex_map <- data.frame(
        pt_index = integer(0),    # Index of the point in the original input
        split_vertex_id = character(0),  # ID of the new vertex created for this point
        edge_idx = integer(0),    # Index of the edge in the original graph
        edge_id = character(0),   # ID of the edge in the original graph
        stringsAsFactors = FALSE
    )

    # ========================================================================
    # 4. Process Edges with Points (Helper Functions)
    # ========================================================================
    
    #' Process all edges with matched points
    #' @param edges_with_pts Vector of edge indices that have points
    #' @param graph_std Standardized graph data
    #' @param pts All points data
    #' @param edge_matches List mapping edge indices to point indices
    #' @param debug Logical flag for debug output
    #' @return List of processed edges
    process_all_edges <- function(edges_with_pts, graph_std, pts, edge_matches, debug = FALSE) {
        lapply(edges_with_pts, function(edge_idx) {
            process_single_edge(edge_idx, graph_std, pts, edge_matches, debug)
        })
    }
    
    #' Process a single edge with its matched points
    #' @param edge_idx Index of the edge in the graph
    #' @param graph_std Standardized graph data
    #' @param pts All points data
    #' @param edge_matches List mapping edge indices to point indices
    #' @param debug Logical flag for debug output
    #' @return List containing processed edge data
    process_single_edge <- function(edge_idx, graph_std, pts, edge_matches, debug = FALSE) {
        if (debug) {
            cat("Processing edge", edge_idx, "with", 
                length(edge_matches[[as.character(edge_idx)]]), "points\n")
        }
        
        # Get the standardized edge data
        edge_row_std <- graph_std[edge_idx, ]
        
        # Get all points matched to this edge
        matched_pts_idx <- edge_matches[[as.character(edge_idx)]]
        matched_pts <- pts[matched_pts_idx, ]
        
        # Process the edge and return results
        process_edge_geometry(edge_row_std, matched_pts, edge_idx, debug)
    }
    
    #' Process edge geometry and points
    #' @param edge_row_std Standardized edge data
    #' @param matched_pts Data frame of points matched to this edge
    #' @param edge_idx Original edge index
    #' @param debug Logical flag for debug output
    #' @return List containing processed segments and vertices
    process_edge_geometry <- function(edge_row_std, matched_pts, edge_idx, debug = FALSE) {
        # Project points onto edge and sort
        start_pt <- c(edge_row_std$xfr, edge_row_std$yfr)
        end_pt <- c(edge_row_std$xto, edge_row_std$yto)
        edge_vec <- end_pt - start_pt
        edge_len <- sqrt(sum(edge_vec^2))
        
        # Project points onto the edge vector
        proj_data <- project_points_to_edge(matched_pts, start_pt, edge_vec, edge_len)
        
        # Sort points by distance along edge
        ord <- order(proj_data$d_along_edge)
        matched_pts <- matched_pts[ord, ]
        proj_data <- lapply(proj_data, `[`, ord)
        
        # Create split vertices
        split_vertices <- create_split_vertices(proj_data, matched_pts, edge_row_std, edge_idx, debug)
        
        # Create new edges between split points
        new_edges <- create_split_edges(edge_row_std, split_vertices, proj_data, edge_idx, debug)
        
        # Create point-to-vertex connections
        pt_edges <- create_point_vertex_edges(matched_pts, split_vertices, proj_data, 
                                            edge_row_std, edge_idx, debug)
        
        list(
            split_edges = new_edges,
            pt_edges = pt_edges,
            split_vertices = split_vertices
        )
    }
    
    #' Project points onto edge vector
    #' @param pts Points data frame
    #' @param start_pt Starting point of edge (x,y)
    #' @param edge_vec Edge vector (x,y)
    #' @param edge_len Length of edge
    #' @return List with projection data
    project_points_to_edge <- function(pts, start_pt, edge_vec, edge_len) {
        # Vector from start point to each point
        pt_vecs <- cbind(pts$x - start_pt[1], pts$y - start_pt[2])
        
        # Projection of each point onto edge vector
        proj <- (pt_vecs %*% edge_vec) / edge_len^2
        
        # Distance along edge (0 to 1)
        d_along_edge <- pmin(pmax(proj, 0), 1)
        
        # Perpendicular distance from edge
        perp_dist <- sqrt(rowSums((pt_vecs - cbind(proj, proj) * edge_vec)^2))
        
        list(
            d_along_edge = d_along_edge,
            perp_dist = perp_dist,
            is_vertex = (abs(proj) < 1e-10 | abs(proj - 1) < 1e-10)
        )
    }
    
    #' Create split vertices at point locations
    #' @param proj_data Projection data from project_points_to_edge
    #' @param matched_pts Matched points data
    #' @param edge_row_std Standardized edge data
    #' @param edge_idx Original edge index
    #' @param debug Debug flag
    #' @return Data frame of split vertices
    create_split_vertices <- function(proj_data, matched_pts, edge_row_std, edge_idx, debug) {
        # Create new vertices at split points
        split_vertices <- data.frame(
            x = matched_pts$x,
            y = matched_pts$y,
            edge_idx = edge_idx,
            pt_id = matched_pts$pt_id,
            d_along_edge = proj_data$d_along_edge,
            stringsAsFactors = FALSE
        )
        
        # Generate unique vertex IDs
        split_vertices$id <- paste0(
            "split_", 
            edge_idx, "_", 
            seq_len(nrow(split_vertices))
        )
        
        split_vertices
    }
    
    #' Create edges between split points
    #' @param edge_row_std Standardized edge data
    #' @param split_vertices Split vertices data frame
    #' @param proj_data Projection data
    #' @param edge_idx Original edge index
    #' @param debug Debug flag
    #' @return Data frame of new edges
    create_split_edges <- function(edge_row_std, split_vertices, proj_data, edge_idx, debug) {
        # Create edges between split points
        n_pts <- nrow(split_vertices)
        if (n_pts == 0) return(data.frame())
        
        # Create edges from start to first point, between points, and from last point to end
        edges <- data.frame(
            from_id = c(
                as.character(edge_row_std$from_id),
                split_vertices$id[-n_pts]
            ),
            to_id = c(
                split_vertices$id[1],
                split_vertices$id[-1],
                as.character(edge_row_std$to_id)
            ),
            edge_id = paste0("split_", edge_idx, "_", seq_len(n_pts + 1)),
            edge_idx = edge_idx,
            split_edge = TRUE,
            stringsAsFactors = FALSE
        )
        
        # Copy other edge attributes
        for (col in setdiff(names(edge_row_std), c("from_id", "to_id", "edge_id"))) {
            edges[[col]] <- edge_row_std[[col]]
        }
        
        edges
    }
    
    #' Create edges connecting points to vertices
    #' @param matched_pts Matched points data
    #' @param split_vertices Split vertices data frame
    #' @param proj_data Projection data
    #' @param edge_row_std Standardized edge data
    #' @param edge_idx Original edge index
    #' @param debug Debug flag
    #' @return Data frame of point-to-vertex edges
    create_point_vertex_edges <- function(matched_pts, split_vertices, proj_data, 
                                        edge_row_std, edge_idx, debug) {
        # Create edges from points to vertices
        pt_edges <- data.frame(
            from_id = as.character(matched_pts$pt_id),
            to_id = split_vertices$id,
            edge_id = paste0("pt_edge_", edge_idx, "_", seq_along(matched_pts$pt_id)),
            edge_idx = edge_idx,
            pt_edge = TRUE,
            stringsAsFactors = FALSE
        )
        
        # Copy other edge attributes
        for (col in setdiff(names(edge_row_std), c("from_id", "to_id", "edge_id"))) {
            pt_edges[[col]] <- edge_row_std[[col]]
        }
        
        pt_edges
    }
    
    #' Process all edges with matched points
    #' @param edges_with_pts Vector of edge indices that have points
    #' @param graph_std Standardized graph data
    #' @param pts All points data
    #' @param edge_matches List mapping edge indices to point indices
    #' @param debug Logical flag for debug output
    #' @return List of processed edges
    process_all_edges <- function(edges_with_pts, graph_std, pts, edge_matches, debug = FALSE) {
        lapply(edges_with_pts, function(edge_idx) {
            process_single_edge(edge_idx, graph_std, pts, edge_matches, debug)
        })
    }
    
    #' Process a single edge with its matched points
    #' @param edge_idx Index of the edge in the graph
    #' @param graph_std Standardized graph data
    #' @param pts All points data
    #' @param edge_matches List mapping edge indices to point indices
    #' @param debug Logical flag for debug output
    #' @return List containing processed edge data
    process_single_edge <- function(edge_idx, graph_std, pts, edge_matches, debug = FALSE) {
        if (debug) {
            cat("Processing edge", edge_idx, "with", 
                length(edge_matches[[as.character(edge_idx)]]), "points\n")
        }
        
        # Get the standardized edge data
        edge_row_std <- graph_std[edge_idx, ]
        
        # Get all points matched to this edge
        matched_pts_idx <- edge_matches[[as.character(edge_idx)]]
        matched_pts <- pts[matched_pts_idx, ]
        
        # Process the edge and return results
        process_edge_geometry(edge_row_std, matched_pts, edge_idx, debug)
    }
    
    #' Process edge geometry and points
    #' @param edge_row_std Standardized edge data
    #' @param matched_pts Data frame of points matched to this edge
    #' @param edge_idx Original edge index
    #' @param debug Logical flag for debug output
    #' @return List containing processed segments and vertices
    process_edge_geometry <- function(edge_row_std, matched_pts, edge_idx, debug = FALSE) {
        # Project points onto edge and sort
        start_pt <- c(edge_row_std$xfr, edge_row_std$yfr)
        end_pt <- c(edge_row_std$xto, edge_row_std$yto)
        edge_vec <- end_pt - start_pt
        edge_len <- sqrt(sum(edge_vec^2))
        
        # Calculate projection distances of points onto the edge
        proj_dists <- vapply(seq_len(nrow(matched_pts)), function(i) {
            pt_vec <- c(matched_pts$x[i], matched_pts$y[i]) - start_pt
            sum(pt_vec * edge_vec) / edge_len
        }, numeric(1))
        
        # Sort points by their projection distance along the edge
        ord <- order(proj_dists)
        matched_pts <- matched_pts[ord, , drop = FALSE]
        proj_dists <- proj_dists[ord]
        
        # ====================================================================
        # 4.2 Match Points to Existing Vertices
        # ====================================================================
        # Check if points coincide with existing graph vertices
        if (!exists("graph_verts")) {
            graph_verts <- dodgr_vertices(graph_std)
        }
        
        # Match points to existing vertices
        vts <- match_points_to_verts(verts = graph_verts, xy = matched_pts[, c("x", "y")])
        is_vertex <- !is.na(vts)
        
        # ====================================================================
        # 4.3 Filter Points to Avoid Zero-Length Segments
        # ====================================================================
        # For points that aren't vertices, check for and remove any that would
        # create zero-length or very short segments
        if (length(proj_dists) > 0 && any(!is_vertex)) {
            # Get indices of non-vertex points
            non_vertex_idx <- which(!is_vertex)
            
            if (length(non_vertex_idx) > 1) {
                # Get projection distances for non-vertex points
                non_vertex_dists <- proj_dists[non_vertex_idx]
                ord <- order(non_vertex_dists)
                non_vertex_dists <- non_vertex_dists[ord]
                
                # Calculate distances between consecutive points
                seg_starts <- c(0, non_vertex_dists[-length(non_vertex_dists)])
                seg_ends <- non_vertex_dists
                seg_lengths <- seg_ends - seg_starts
                
                # Keep first point and points that are far enough from previous
                keep_non_vertex <- c(TRUE, seg_lengths[-1] >= dist_tol)
                
                if (any(!keep_non_vertex)) {
                    if (debug) {
                        cat("Removing", sum(!keep_non_vertex), 
                            "non-vertex points that would create zero-length segments\n")
                    }
                    # Update which non-vertex points to keep
                    keep_idx <- rep(TRUE, length(proj_dists))
                    keep_idx[non_vertex_idx[!keep_non_vertex]] <- FALSE
                    
                    # Update matched_pts and related vectors
                    matched_pts <- matched_pts[keep_idx, , drop = FALSE]
                    proj_dists <- proj_dists[keep_idx]
                    vts <- vts[keep_idx]
                    is_vertex <- is_vertex[keep_idx]
                }
            }
        }

        # ====================================================================
        # 4.4 Create Projection Data and Filter Duplicates
        # ====================================================================
        # Create a data frame with projection coordinates and metadata
        proj_data <- data.frame(
            x = matched_pts$x,
            y = matched_pts$y,
            pt_id = matched_pts$pt_id,  # Track original point IDs
            is_vertex = !is.na(vts),    # TRUE if point matches a vertex
            vertex_id = ifelse(!is.na(vts), 
                             as.character(graph_verts$id[vts]), 
                             NA_character_),
            stringsAsFactors = FALSE
        )
        
        if (debug && any(proj_data$is_vertex)) {
            #cat("Matched", sum(proj_data$is_vertex), "points to existing vertices\n")
        }
        
        # Filter out points that are too close to each other
        if (nrow(proj_data) > 1) {
            # Calculate squared distances between consecutive points
            dx <- diff(proj_data$x)
            dy <- diff(proj_data$y)
            sq_dists <- dx^2 + dy^2
            
            # Keep first point and points that are far enough from previous kept point
            # Always keep original vertices (is_vertex = TRUE)
            keep_idx <- c(TRUE, sq_dists > (dist_tol^2) | proj_data$is_vertex[-1])
            
            # Ensure we don't drop any existing vertices
            if (any(proj_data$is_vertex)) {
                keep_idx <- keep_idx | proj_data$is_vertex
            }
            
            if (any(!keep_idx) && debug) {
                cat("Removing", sum(!keep_idx), "duplicate projection points\n")
            }
            
            # Update the matched points and projection data
            matched_pts <- matched_pts[keep_idx, , drop = FALSE]
            proj_data <- proj_data[keep_idx, , drop = FALSE]
        }
        
        # ====================================================================
        # 4.5 Create New Vertices for Split Points
        # ====================================================================
        # Get coordinates for points that need new vertices (non-vertex points)
        proj_coords <- proj_data[!proj_data$is_vertex, c("x", "y"), drop = FALSE]
        
        # Initialize empty data frame for new vertices
        split_vertices <- data.frame(
            id = character(),
            x = numeric(),
            y = numeric(),
            stringsAsFactors = FALSE
        )
        
        # Create new vertices for points that aren't existing vertices
        if (nrow(proj_coords) > 0) {
            split_vertices <- data.frame(
                id = paste0(edge_row_std$edge_id, "_split", seq_len(nrow(proj_coords))),
                x = proj_coords$x,
                y = proj_coords$y,
                stringsAsFactors = FALSE
            )
        }
        
        n_split <- nrow(split_vertices)  # Number of new vertices created

        # Map each input point to the index of its split vertex
        proj_idx_map <- match(paste(matched_pts$x, matched_pts$y), paste(split_vertices$x, split_vertices$y))
        
        # Get the vertex matches we found earlier
        vts <- match_points_to_verts(verts = graph_verts, xy = matched_pts[, c("x", "y")])
        
        # Add all points on this edge to pt_vertex_map (once per edge, not per segment)
        if (nrow(matched_pts) > 0) {
            for (pt_i in seq_len(nrow(matched_pts))) {
                split_vertex_idx <- proj_idx_map[pt_i]
                
                # Use the original vertex ID if this point matches a vertex, otherwise use the split vertex ID
                vertex_id <- if (!is.na(vts[pt_i])) {
                    as.character(graph_verts$id[vts[pt_i]])
                } else if (!is.na(split_vertex_idx)) {
                    split_vertices$id[split_vertex_idx]
                } else {
                    next  # Skip if no valid vertex ID found
                }
                
                # Add to point-vertex mapping
                pt_vertex_map <- rbind(
                    pt_vertex_map,
                    data.frame(
                        pt_index = matched_pts$pt_id[pt_i],
                        split_vertex_id = vertex_id,
                        edge_idx = edge_idx,
                        stringsAsFactors = FALSE
                    )
                )
            }
        }
        
        # Calculate relative positions of all points along the edge
        all_points <- rbind(
            data.frame(x = edge_row_std$xfr, y = edge_row_std$yfr),
            split_vertices[, c("x", "y")],
            data.frame(x = edge_row_std$xto, y = edge_row_std$yto)
        )
        
        edge_vec <- c(edge_row_std$xto - edge_row_std$xfr, 
                     edge_row_std$yto - edge_row_std$yfr)
        edge_len <- sqrt(sum(edge_vec^2))
        
        rel_pos <- vapply(seq_len(nrow(all_points)), function(i) {
            pt_vec <- c(all_points$x[i] - edge_row_std$xfr, 
                       all_points$y[i] - edge_row_std$yfr)
            sum(pt_vec * edge_vec) / edge_len^2
        }, numeric(1))
        
        # ====================================================================
        # 4.8 Create Point-to-Vertex Connection Edges
        # ====================================================================
        # Track created (pt_id, v_id, dir) to ensure edge uniqueness
        created_pt_edges <- new.env(parent = emptyenv())
        
        # Identify unique input points for this edge
        # We need to process each unique point only once, even if it appears multiple times
        # (e.g., from different edges or directions)
        if (nrow(matched_pts) > 0) {
            # Create a unique identifier for each point based on its coordinates
            pt_coords <- paste0(round(matched_pts$x, 8), "_", round(matched_pts$y, 8))
            unique_input_idx <- !duplicated(pt_coords)
        } else {
            unique_input_idx <- logical(0)
        }
        
        # Process each unique input point that needs connection
        for (i in which(unique_input_idx)) {
            # Create a unique ID for the point-to-vertex connection
            pt_id <- paste0("pt_", matched_pts$pt_id[i], "_", edge_row_std$edge_id)
            v_id <- split_vertices$id[proj_idx_map[i]]
            
            # Create edges in both directions (bidirectional graph)
            for (dir in c("forward", "backward")) {
                # Skip if we've already created this edge
                key <- paste(pt_id, v_id, dir, sep = "::")
                if (exists(key, envir = created_pt_edges, inherits = FALSE)) next
                assign(key, TRUE, envir = created_pt_edges)
                
                # ============================================================
                # 4.8.1 Set Up Edge Geometry
                # ============================================================
                # Determine edge direction and coordinates
                if (dir == "forward") {
                    pt_from <- pt_id
                    pt_to <- v_id
                    pt_xfr <- matched_pts$x0[i]
                    pt_yfr <- matched_pts$y0[i]
                    pt_xto <- matched_pts$x[i]
                    pt_yto <- matched_pts$y[i]
                } else {
                    pt_from <- v_id
                    pt_to <- pt_id
                    pt_xfr <- matched_pts$x[i]
                    pt_yfr <- matched_pts$y[i]
                    pt_xto <- matched_pts$x0[i]
                    pt_yto <- matched_pts$y0[i]
                }
                
                # Calculate edge length using appropriate geodistance measure
                pt_len <- geodist::geodist(
                    data.frame(x = c(pt_xfr, pt_xto), y = c(pt_yfr, pt_yto)),
                    measure = get_geodist_measure(graph)
                )[1, 2]
                
                # ============================================================
                # 4.8.2 Create New Edge with Basic Properties
                # ============================================================
                pt_edge <- edge_row_std
                pt_edge[["from"]] <- pt_from
                pt_edge[["to"]] <- pt_to
                pt_edge[["xfr"]] <- pt_xfr
                pt_edge[["yfr"]] <- pt_yfr
                pt_edge[["xto"]] <- pt_xto
                pt_edge[["yto"]] <- pt_yto
                pt_edge$d <- pt_len
                
                # ============================================================
                # 4.8.3 Apply Weighting and Profile Logic
                # ============================================================
                if (!is.null(wt_profile) || !is.null(wt_profile_file)) {
                    # Use provided weight profile for edge properties
                    wp <- get_profile(wt_profile = wt_profile, file = wt_profile_file)
                    way_wt <- wp$value[wp$way == highway]
                    if (length(way_wt) == 0) stop("Highway type not found in weight profile")
                    
                    pt_edge$d_weighted <- pt_len / way_wt
                    pt_edge$highway <- highway
                    pt_edge <- set_maxspeed(pt_edge, wt_profile, wt_profile_file)
                    pt_edge <- weight_by_num_lanes(pt_edge, wt_profile)
                    pt_edge <- calc_edge_time_by_name(pt_edge, wt_profile)
                } else {
                    # Fallback to simple ratio-based weighting if no profile provided
                    pt_edge$d_weighted <- pt_len * d_ratio
                    pt_edge$highway <- edge_row_std$highway
                    pt_edge$time <- pt_len * (edge_row_std$time / edge_row_std$d)
                    pt_edge$time_weighted <- pt_len * (edge_row_std$time / edge_row_std$d) * time_ratio
                }
                
            }  # End of dir loop (forward/backward)
        }  # End of unique_input_idx loop
        
        # ====================================================================
        # 5. Assemble Final Graph
        # ====================================================================
        # This section combines all processed edges and vertices into the final graph
        # structure, ensuring proper handling of split edges and point-to-vertex
        # connections while maintaining graph integrity.
    }  # End of edges_with_pts loop
    
    # ====================================================================
    # 5. Process Edges with Points
    # ====================================================================
    if (debug) cat("Processing", length(edges_with_pts), "edges with points\n")
    
    # Process all edges with points using our helper functions
    processed_edges <- process_all_edges(edges_with_pts, graph_std, pts, edge_matches, debug)
    
    # Extract results
    split_edges <- lapply(processed_edges, `[[`, "split_edges")
    pt_edges <- lapply(processed_edges, `[[`, "pt_edges")
    split_vertices <- lapply(processed_edges, `[[`, "split_vertices")
    
    # Remove NULL entries and flatten lists
    split_edges <- unlist(split_edges[!sapply(split_edges, is.null)], recursive = FALSE)
    pt_edges <- unlist(pt_edges[!sapply(pt_edges, is.null)], recursive = FALSE)
    split_vertices <- unlist(split_vertices[!sapply(split_vertices, is.null)], recursive = FALSE)
    
    if (debug) {
        cat("Processed", length(split_edges), "split edges and", 
            length(pt_edges), "point-to-vertex edges\n")
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
                    #cat("Point", pt_unique_id, "matches existing vertex", vertex_id, "(creating connection)\n")
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

    # ====================================================================
    # 5. Final Graph Assembly
    # ====================================================================
    # This section combines all processed edges into the final graph structure,
    # ensuring proper handling of split edges and point-to-vertex connections.
    
    # ====================================================================
    # 5.1 Convert Edge Lists to Data Frames
    # ====================================================================
    # Convert lists of edges to data frames for final assembly
    split_edges_df <- if (length(split_edges) > 0) do.call(rbind, split_edges) else NULL
    pt_edges_df <- if (length(pt_edges) > 0) do.call(rbind, pt_edges) else NULL
    
    # ====================================================================
    # 5.2 Remove Duplicate Point-to-Vertex Edges
    # ====================================================================
    # Ensure no duplicate connections between the same points
    if (!is.null(pt_edges_df)) {
        dup_idx <- duplicated(pt_edges_df[, c("from", "to")])
        if (any(dup_idx)) {
            warning(sum(dup_idx), " duplicate point-to-vertex edges detected. Removing duplicates.")
            pt_edges_df <- pt_edges_df[!dup_idx, ]
        }
    }

    # ====================================================================
    # 5.3 Process Split Edges
    # ====================================================================
    # Handle edges created by splitting original edges at points
    split_edges_final <- NULL
    if (!is.null(split_edges_df)) {
        # Initialize with original edge data
        split_edges_final <- graph[split_edges_df$orig_idx, , drop = FALSE]
        
        # Get graph column mappings
        gr_cols <- dodgr_graph_cols(graph)
        gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
        
        # Update all columns except highway to preserve original values
        for (col_name in names(gr_cols)) {
            if (col_name != "highway") {
                split_edges_final[, gr_cols[col_name]] <- split_edges_df[[col_name]]
            }
        }
    }
    
    # ====================================================================
    # 5.4 Process Point-to-Vertex Edges
    # ====================================================================
    # Handle new edges connecting input points to the graph
    pt_edges_final <- NULL
    if (!is.null(pt_edges_df)) {
        # Initialize with original edge data structure
        pt_edges_final <- graph[pt_edges_df$orig_idx, , drop = FALSE]
        
        # Get graph column mappings
        gr_cols <- dodgr_graph_cols(graph)
        gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
        
        # Update all columns, including highway from the provided argument
        for (col_name in names(gr_cols)) {
            pt_edges_final[, gr_cols[col_name]] <- pt_edges_df[[col_name]]
        }
        pt_edges_final$highway <- highway
    }
    
    # ====================================================================
    # 5.5 Combine All Edge Types
    # ====================================================================
    # Combine in order: untouched edges, split edges, point-to-vertex edges
    result_graph <- rbind(untouched_graph, split_edges_final, pt_edges_final)
    
    # Clean up row names for consistency
    rownames(result_graph) <- NULL
    
    # Debug output
    if (debug) {
        cat("Final graph assembly complete.\n")
        cat("- Untouched edges:", nrow(untouched_graph), "\n")
        cat("- Split edges:", if (!is.null(split_edges_final)) nrow(split_edges_final) else 0, "\n")
        cat("- Point-to-vertex edges:", if (!is.null(pt_edges_final)) nrow(pt_edges_final) else 0, "\n")
        cat("Total edges in result:", nrow(result_graph), "\n")
    }
    
    return(result_graph)
}
