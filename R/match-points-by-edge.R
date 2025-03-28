#' Add nodes to a graph by matching them to the nearest edges, and processing
#' edges with multiple points in one go. This is more efficient than
#' \link{add_nodes_to_graph} when multiple points match to the same edge.
#'
#' @inheritParams add_nodes_to_graph
#' @param wt_profile Character string specifying the weight profile to use for edges connecting to original points.
#' @param wt_profile_file Character string specifying the file path to a weight profile CSV file.
#' @param highway Character string specifying the highway type for edges connecting to original points.
#' @param max_distance Numeric value specifying the maximum distance for edges connecting to original points.
#' @param replace_component Logical value specifying whether to replace the component of the graph with the new edges.
#' @return Modified version of graph with nodes added at specified locations
#' @export
#' @examples
#' \dontrun{
#' graph <- weight_streetnet (hampi, wt_profile = "foot")
#' npts <- 100
#' verts <- dodgr_vertices (graph)
#' xy <- data.frame (
#'     x = min (verts$x) + runif (npts) * diff (range (verts$x)),
#'     y = min (verts$y) + runif (npts) * diff (range (verts$y))
#' )
#'
#' graph <- add_nodes_to_graph_by_edge (graph, xy)
#' dim (graph) # more edges than original
#' }
add_nodes_to_graph_by_edge <- function (graph,
                                        xy,
                                        dist_tol = 1e-6, # Kept for backward compatibility but not used
                                        intersections_only = FALSE,
                                        xy_id = NULL, 
                                        wt_profile = NULL,
                                        wt_profile_file = NULL,
                                        highway = NULL,
                                        max_distance = Inf,
                                        replace_component = TRUE) {
    
    # Function to generate unique IDs
    genhash <- function (len = 16L) {
        paste0 (sample (c (0:9, letters, LETTERS), size = len, replace = TRUE), collapse = "")
    }
    genhashv <- Vectorize(genhash)
    
    # Add x0 and y0 columns to xy
    xy <- pre_process_xy (xy)
    
    # Standardize graph for calculations
    gr_cols <- dodgr_graph_cols (graph)
    gr_cols <- unlist (gr_cols [which (!is.na (gr_cols))])
    graph_std <- graph [, gr_cols] # standardise column names
    names (graph_std) <- names (gr_cols)
    
    # Match points to the standardized graph
    pts <- match_pts_to_graph (graph_std, xy, distances = TRUE)
    projids <- unique(pts[, c("x", "y")])  # Select distinct (x, y) pairs
    projids$proj_id <- paste0("proj_", genhash(10), "_", seq_len(nrow(projids)))  # Generate proj_id
    pts <- merge(pts, projids)
    if (is.null(xy_id)) {
        pts$xy_id <- genhashv(rep(10L, nrow(pts)))
    } else {
        pts$xy_id <- xy_id
    }
    # Return original graph if no points match
    if (nrow (pts) == 0) {
        return (graph)
    }
    graph$tmp_graph_index <- 1:nrow(graph)
    # Add original coordinates to pts
    pts$x0 <- xy [, 1]
    pts$y0 <- xy [, 2]
    pts$pts_index <- seq_len (nrow (pts))
    pts$from <- graph_std$from[pts$index]
    pts$to <- graph_std$to[pts$index]
    
    # Check if any original points are too close to edge endpoints
    if (dist_tol > 1e-6) {  # Only do this check for larger tolerance values
        # Create a list to store indices of points to remove
        points_to_remove <- integer(0)
        
        for (i in seq_len(nrow(pts))) {
            # Get the edge that this point matches to
            edge_i <- graph_std[pts$index[i], ]
            
            # Create a distance matrix between original point and edge endpoints
            xy_i <- data.frame(
                x = c(pts$x0[i], edge_i$xfr, edge_i$xto),
                y = c(pts$y0[i], edge_i$yfr, edge_i$yto)
            )
            dmat <- geodist::geodist(xy_i, measure = "geodesic")
            
            # If original point is too close to an endpoint, mark for removal
            if (dmat[1, 2] < dist_tol || dmat[1, 3] < dist_tol) {
                points_to_remove <- c(points_to_remove, i)
            }
        }
        
        # Remove points that are too close to edge endpoints
        if (length(points_to_remove) > 0) {
            pts <- pts[-points_to_remove, ]
            # Return original graph if no points remain
            if (nrow(pts) == 0) {
                return(graph)
            }
        }
    }
    
    # Create a lookup table from the graph for efficient matching
    graph_lookup <- data.frame(
        from = graph_std$from,
        to = graph_std$to,
        index = seq_len(nrow(graph_std))
    )
    
    # Create bidirectional version with swapped from/to
    pts_bi <- pts[, setdiff(names(pts), "index")]  # Remove index column
    
    # Find matching indices using merge with swapped column order
    pts_bi <- merge(
        graph_lookup,
        pts_bi,
        by.y = c("to", "from"),  # Swap the order in the merge operation
        by.x = c("from", "to")
        #, all.x = FALSE  # Only keep rows that match
    )
    # Combine with original points
    pts <- unique(rbind(pts, pts_bi[names(pts)]))
    # Extract edges that need to be split
    edges_to_split <- graph_std [pts$index, ]
    # Add index for tracking
    edges_to_split$n <- pts$pts_index
    
    graph_to_add <- graph [pts$index, ]
    
    # Remove edges to be split from the graph
    graph_std <- graph_std [-pts$index, ]
    graph <- graph [-pts$index, ]
    
    # Group edges by edge_id
    unique_edges <- unique(edges_to_split$edge_id)
    all_edges_split <- list()

    # Process each unique edge
    for (edge_id in unique_edges) {
        # Get all instances of this edge
        current_edge <- edges_to_split[edges_to_split$edge_id == edge_id, ]
        current_edge_1 <- current_edge[1,]
        #  Get all points that match this edge instance
        edge_pts <- pts[pts$pts_index %in% current_edge$n, ]
        edge_pts <- edge_pts[!duplicated(edge_pts$proj_id), ]
        
        # stop if no points
        if (nrow(edge_pts) == 0) stop("No points found.")
        # Store the original ratios - these will be used directly
        orig_d_weighted <- current_edge_1$d_weighted
        orig_d <- current_edge_1$d
        orig_ratio <- orig_d_weighted / orig_d
        orig_time_ratio <- current_edge_1$time_weighted / current_edge_1$time
        
        # Sort points along the edge
        # Calculate distance from start of edge to each projection point
        start_point <- c(current_edge$xfr[1], current_edge$yfr[1])
        end_point <- c(current_edge$xto[1], current_edge$yto[1])
        
        # Calculate vector from start to end
        edge_vector <- end_point - start_point
        edge_length <- sqrt(sum(edge_vector^2))
        
        # Calculate projection of each point onto the edge
        proj_distances <- numeric(nrow(edge_pts))
        for (p in seq_len(nrow(edge_pts))) {
            point_vector <- c(edge_pts$x[p], edge_pts$y[p]) - start_point
            # Projection distance along the edge
            proj_distances[p] <- sum(point_vector * edge_vector) / edge_length
        }
        
        # Sort points by projection distance
        sorted_indices <- order(proj_distances)
        edge_pts <- edge_pts[sorted_indices, ]
        proj_distances <- proj_distances[sorted_indices]
        
        # Process all points normally without special handling for close points
        # Create segments for each point
        all_segments <- NULL
        
        # If not intersections_only, add connections to original points
        if (!intersections_only) {
            for (p in seq_len(nrow(edge_pts))) {
                # Create edges to original point (bidirectional)
                new_edges <- rbind(current_edge_1, current_edge_1)
                ## HERE
                # Set up connection to original point
                new_edges$from[1] <- new_edges$to[2] <-  edge_pts$xy_id[p]
                new_edges$to[1] <- new_edges$from[2] <- edge_pts$proj_id[p]
                new_edges$xfr[1] <- new_edges$xto[2] <- edge_pts$x0[p]
                new_edges$yfr[1] <- new_edges$yto[2] <- edge_pts$y0[p]
                new_edges$xto[1] <- new_edges$xfr[2] <- edge_pts$x[p]
                new_edges$yto[1] <- new_edges$yfr[2] <- edge_pts$y[p]

                # Calculate distance using geodesic distance
                d_i <- geodist::geodist(
                    data.frame(
                        x = c(new_edges$xfr[1], new_edges$xto[1]),
                        y = c(new_edges$yfr[1], new_edges$yto[1])
                    ),
                    measure = "geodesic"
                )[1, 2]
                
                # Ensure no zero distances
                if (d_i < 1e-9) d_i <- 1e-9
                
                # Check if distance exceeds maximum allowed
                if (d_i > max_distance) {
                    next  # Skip this connection
                }
                
                # Apply custom weight profile if provided
                if (!is.null(wt_profile) || !is.null(wt_profile_file)) {
                    # Get weight profile
                    wp <- dodgr:::get_profile(wt_profile = wt_profile, file = wt_profile_file)
                    way_wt <- wp$value[wp$way == highway]
                    
                    if (length(way_wt) == 0) {
                        stop("Highway type '", highway, "' not found in weight profile")
                    }
                    
                    # Calculate weights using the profile
                    new_edges$d <- d_i
                    new_edges$d_weighted <- d_i / way_wt
                    new_edges$highway <- highway
                    # Apply additional weighting functions
                    new_edges <- dodgr:::set_maxspeed(new_edges, wt_profile, wt_profile_file) |>
                        dodgr:::weight_by_num_lanes(wt_profile) |>
                        dodgr:::calc_edge_time(wt_profile)
                    
                } else {
                    # Use original edge's weight ratios
                    new_edges$d <- d_i
                    new_edges$d_weighted <- d_i * orig_ratio
                    new_edges$time <- d_i * (current_edge_1$time / current_edge_1$d)
                    new_edges$time_weighted <- d_i * (current_edge_1$time / current_edge_1$d) * orig_time_ratio
                    new_edges$highway <- unique(graph_to_add[graph_to_add$tmp_graph_index==edge_pts$index[p], "highway"])
                }
                
                # Generate unique edge IDs
                new_edges$edge_id <- c(
                    paste0("to_orig_", p, "_", genhash(5)),
                    paste0("from_orig_", p, "_", genhash(5))
                )
                
                # Add to all segments
                all_segments <- rbind(all_segments, new_edges)
            }
        }
        
        # Now split the edge into segments
        n_segments <- nrow(edge_pts) + 1
        
        # Create a list to store all segments
        segments <- vector("list", n_segments)
        
        # Create first segment
        segments[[1]] <- current_edge_1
        segments[[1]]$to <- edge_pts$proj_id[1]
        segments[[1]]$xto <- edge_pts$x[1]
        segments[[1]]$yto <- edge_pts$y[1]
        
        # Create middle segments
        if (n_segments > 2) {
            for (s in seq_len(n_segments-2)) {
                segments[[s+1]] <- current_edge_1
                segments[[s+1]]$from <- segments[[s]]$to
                segments[[s+1]]$to <- edge_pts$proj_id[s]
                segments[[s+1]]$xfr <- edge_pts$x[s]
                segments[[s+1]]$yfr <- edge_pts$y[s]
                segments[[s+1]]$xto <- edge_pts$x[s+1]
                segments[[s+1]]$yto <- edge_pts$y[s+1]
                
            }
        }
        
        # Create last segment
        segments[[n_segments]] <- current_edge_1
        segments[[n_segments]]$from <- if (n_segments > 1) segments[[n_segments-1]]$to else segments[[1]]$to
        segments[[n_segments]]$xfr <- edge_pts$x[n_segments-1]
        segments[[n_segments]]$yfr <- edge_pts$y[n_segments-1]
        
        # Calculate distances and update weights for each segment
        for (s in seq_len(n_segments)) {
            # Calculate distance for this segment using geodesic distance
            segment_xy <- data.frame(
                x = c(segments[[s]]$xfr, segments[[s]]$xto),
                y = c(segments[[s]]$yfr, segments[[s]]$yto)
            )
            segment_dist <- geodist::geodist(segment_xy, measure = "geodesic")[1, 2]
            
            # Ensure no zero distances (use a small value if distance is zero)
            if (segment_dist < 1e-9) segment_dist <- 1e-9
            
            # Update segment properties - preserve weight ratios exactly
            segments[[s]]$d <- segment_dist
            segments[[s]]$d_weighted <- segment_dist * orig_ratio
            segments[[s]]$time <- segment_dist * (current_edge_1$time / current_edge_1$d)
            segments[[s]]$time_weighted <- segment_dist * (current_edge_1$time / current_edge_1$d) * orig_time_ratio
            
            # Update edge_id to make it unique
            segments[[s]]$edge_id <- paste0(segments[[s]]$edge_id, "_", LETTERS[s])
            segments[[s]]$highway <- unique(graph_to_add[graph_to_add$tmp_graph_index==edge_pts$index[1], "highway"])
        }
        
        # Combine all segments
        all_segments <- rbind(all_segments, do.call(rbind, segments))
        
        # Add to all edges split
        all_edges_split[[length(all_edges_split) + 1L]] <- all_segments
    }
    
    # Combine all split edges
    if (length(all_edges_split) == 0) {
        return(graph)  # No edges were split
    }
    
    edges_split <- do.call(rbind, all_edges_split)
    
    # Then match edges_split back on to original graph:
    graph_to_add <- graph_to_add[edges_split$n, ]
    gr_cols <- gr_cols[which(!is.na(gr_cols))]
    for (g in seq_along(gr_cols)) {
        graph_to_add[, gr_cols[g]] <- edges_split[[names(gr_cols)[g]]]
    }
    graph_to_add[, "highway"] <- edges_split[["highway"]]
    
    # Combine original graph with new edges
    result_graph <- rbind(graph, graph_to_add)
    
    # Update component IDs if requested
    if (replace_component) {
        result_graph$component <- NULL
        result_graph <- dodgr::dodgr_components(result_graph)
    }
    result_graph$tmp_graph_index <- NULL
    return(result_graph)
}
