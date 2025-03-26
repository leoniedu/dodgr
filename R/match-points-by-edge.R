#' Add nodes to a graph by processing edges
#'
#' This function is similar to \link{add_nodes_to_graph}, but processes edges rather
#' than points. This is more efficient when multiple points match the same edge.
#'
#' @inheritParams add_nodes_to_graph
#' @return A modified version of the input graph with additional nodes and edges
#' connecting the points specified in \code{xy} to the nearest points on the graph.
#' @export
#' @examples
#' \dontrun{
#' graph <- weight_streetnet (hampi)
#' verts <- dodgr_vertices (graph)
#' set.seed (2)
#' npts <- 10
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
                                        dist_tol = 1e-6,
                                        intersections_only = FALSE) {
    
    # Add x0 and y0 columns to xy
    xy <- pre_process_xy (xy)
    
    # Standardize graph for calculations
    gr_cols <- dodgr_graph_cols (graph)
    gr_cols <- unlist (gr_cols [which (!is.na (gr_cols))])
    graph_std <- graph [, gr_cols] # standardise column names
    names (graph_std) <- names (gr_cols)
    
    # Match points to the standardized graph
    pts <- match_pts_to_graph (graph_std, xy, distances = TRUE)
    # Return original graph if no points match
    if (nrow (pts) == 0) {
        return (graph)
    }
    
    # Add original coordinates to pts
    pts$x0 <- xy [, 1]
    pts$y0 <- xy [, 2]
    pts$pts_index <- seq_len (nrow (pts))
    pts$from <- graph_std$from[pts$index]
    pts$to <- graph_std$to[pts$index]
    pts_bi <- pts %>%
        select(-index) %>%
        dplyr::rename (from=to, to=from
                       #, x0=x, x=x0, y0=y,y=y0
                       ) %>%
        left_join(graph_std %>% 
                      mutate(index=1:n())%>%
                      select(from,to, index))
    pts <- unique(rbind(pts, pts_bi[names(pts)]))
    
    # Function to generate unique IDs
    genhash <- function (len = 16L) {
        paste0 (sample (c (0:9, letters, LETTERS), size = len, replace = TRUE), collapse = "")
    }
    
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
        edge_pts <- edge_pts%>%distinct(pts_index, .keep_all = TRUE)
        # Skip if no points
        if (nrow(edge_pts) == 0) stop("No points found.")
        
        # Store the original ratios - these will be used directly
        orig_d_weighted <- current_edge_1$d_weighted
        orig_d <- current_edge_1$d
        orig_ratio <- orig_d_weighted / orig_d
        orig_time_ratio <- current_edge_1$time_weighted / current_edge_1$time
        
        # Sort points along the edge
        # Calculate distance from start of edge to each projection point
        start_point <- c(current_edge$xfr, current_edge$yfr)
        end_point <- c(current_edge$xto, current_edge$yto)
        
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
        # Now split the edge into segments
        n_segments <- nrow(edge_pts) + 1
        
        # Create a list to store all segments
        segments <- vector("list", n_segments)
        
        # Create first segment
        segments[[1]] <- current_edge_1
        segments[[1]]$to <- genhash()
        segments[[1]]$xto <- edge_pts$x[1]
        segments[[1]]$yto <- edge_pts$y[1]
        
        # Create middle segments
        if (n_segments > 2) {
            for (s in 2:(n_segments-1)) {
                segments[[s]] <- current_edge_1
                segments[[s]]$from <- segments[[s-1]]$to
                segments[[s]]$to <- genhash()
                segments[[s]]$xfr <- edge_pts$x[s-1]
                segments[[s]]$yfr <- edge_pts$y[s-1]
                segments[[s]]$xto <- edge_pts$x[s]
                segments[[s]]$yto <- edge_pts$y[s]
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
        }
        
        # Combine all segments
        all_segments <- do.call(rbind, segments)
        
        # If not intersections_only, add connections to original points
        if (!intersections_only) {
            # For each projection point, add edges to the original point
            for (p in seq_len(nrow(edge_pts))) {
                # Create edges to original point (bidirectional)
                new_edges <- rbind(current_edge_1, current_edge_1)
                
                # Set up connection to original point
                new_id <- genhash(10L)
                new_edges$from[1] <- new_edges$to[2] <- new_id
                new_edges$xfr[1] <- new_edges$xto[2] <- edge_pts$x0[p]
                new_edges$yfr[1] <- new_edges$yto[2] <- edge_pts$y0[p]
                new_edges$to[1] <- new_edges$from[2] <- if (p < n_segments) segments[[p]]$to else segments[[n_segments]]$from
                new_edges$xto[1] <- new_edges$xfr[2] <- edge_pts$x[p]
                new_edges$yto[1] <- new_edges$yfr[2] <- edge_pts$y[p]
                
                # Calculate distance using geodesic distance
                d_i <- geodist::geodist(
                    data.frame(
                        x = c(edge_pts$x[p], edge_pts$x0[p]),
                        y = c(edge_pts$y[p], edge_pts$y0[p])
                    ),
                    measure = "geodesic"
                )[1, 2]
                
                # Ensure no zero distances
                if (d_i < 1e-9) d_i <- 1e-9
                
                # Update distances and weights - preserve weight ratios exactly
                new_edges$d <- d_i
                new_edges$d_weighted <- d_i * orig_ratio
                new_edges$time <- d_i * (current_edge_1$time / current_edge_1$d)
                new_edges$time_weighted <- d_i * (current_edge_1$time / current_edge_1$d) * orig_time_ratio
                
                # Generate unique edge IDs
                new_edges$edge_id <- c(
                    paste0("to_orig_", p, "_", genhash(5)),
                    paste0("from_orig_", p, "_", genhash(5))
                )
                
                # Add to all segments
                all_segments <- rbind(all_segments, new_edges)
            }
        }
        
        # Add to all edges split
        all_edges_split[[length(all_edges_split) + 1]] <- all_segments
    }
    
    # Combine all split edges
    if (length(all_edges_split) == 0) {
        return(graph)  # No edges were split
    }
    
    edges_split <- do.call(rbind, all_edges_split)
    
    # Then match edges_split back on to original graph:
    graph_to_add <- graph_to_add [edges_split$n, ]
    gr_cols <- gr_cols [which (!is.na (gr_cols))]
    for (g in seq_along (gr_cols)) {
        graph_to_add [, gr_cols [g]] <- edges_split [[names (gr_cols) [g]]]
    }
    
    return (rbind (graph, graph_to_add))
    
    
    # Convert edges_split back to original graph format
    new_edges <- edges_split[,names(graph_std)]
    for (g in seq_along(gr_cols)) {
        if (names(gr_cols)[g] %in% names(new_edges)) {
            new_edges[[gr_cols[g]]] <- new_edges[[names(gr_cols)[g]]]
        }
    }
    # Combine the modified graph with the original
    result <- rbind(graph, new_edges[, names(graph)])
    
    return(result)
}
