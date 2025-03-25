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

    pts <- match_pts_to_graph (graph, xy, distances = TRUE)
    xy <- pre_process_xy (xy)
    pts$x0 <- xy [, 1]
    pts$y0 <- xy [, 2]

    gr_cols <- dodgr_graph_cols (graph)
    gr_cols <- unlist (gr_cols [which (!is.na (gr_cols))])
    graph_std <- graph [, gr_cols] # standardise column names
    names (graph_std) <- names (gr_cols)

    # Create a unique identifier for each edge (ignoring direction)
    edge_ids <- paste0 (
        pmin (graph_std$from, graph_std$to),
        "-",
        pmax (graph_std$from, graph_std$to)
    )
    
    # Create a mapping from points to edges
    point_to_edge <- lapply (seq_along (pts$index), function (i) {
        # Find all instances of this edge (including bi-directional)
        matched_edge <- pts$index [i]
        edge_from <- graph_std$from [matched_edge]
        edge_to <- graph_std$to [matched_edge]
        
        # Find all edges that match (including reverse direction)
        edge_indices <- which (
            (graph_std$from == edge_from & graph_std$to == edge_to) |
            (graph_std$from == edge_to & graph_std$to == edge_from)
        )
        
        # Return point index and all matching edge indices
        list (
            point_index = i,
            edge_indices = edge_indices,
            edge_id = edge_ids [edge_indices [1]] # Use first instance for grouping
        )
    })
    
    # Create a mapping from edges to points
    unique_edge_ids <- unique (sapply (point_to_edge, function (x) x$edge_id))
    edge_to_points <- lapply (unique_edge_ids, function (eid) {
        # Find all points that match this edge
        point_indices <- which (sapply (point_to_edge, function (x) x$edge_id == eid))
        
        # Get all edge instances for this edge ID
        all_edge_indices <- unique (unlist (
            lapply (point_to_edge [point_indices], function (x) x$edge_indices)
        ))
        
        list (
            edge_id = eid,
            edge_indices = all_edge_indices,
            point_indices = sapply (point_to_edge [point_indices], function (x) x$point_index)
        )
    })
    
    # Prepare edges to split
    all_edge_indices <- unique (unlist (lapply (edge_to_points, function (x) x$edge_indices)))
    edges_to_split <- graph_std [all_edge_indices, ]
    graph_to_add <- graph [all_edge_indices, ]
    
    # Remove these edges from the graph
    graph_std <- graph_std [-all_edge_indices, ]
    graph <- graph [-all_edge_indices, ]

    genhash <- function (len = 10) {
        paste0 (sample (c (0:9, letters, LETTERS), size = len), collapse = "")
    }
    
    # Process each unique edge with all its matching points
    edges_split <- lapply (edge_to_points, function (edge_group) {
        
        # Get all instances of this edge
        edge_indices <- edge_group$edge_indices
        edge_template <- edges_to_split [edge_indices [1], ]
        
        # Get all points that match this edge
        point_indices <- edge_group$point_indices
        
        # If there's only one point, use a simplified approach
        if (length (point_indices) == 1) {
            i <- point_indices
            
            # Get all instances of this edge (might be bi-directional)
            edges_i <- edges_to_split [edge_indices, ]
            
            new_edges_i <- lapply (seq_len (nrow (edges_i)), function (e) {
                
                # Split edges either side of perpendicular points of intersection:
                edge_i <- edges_i [c (e, e), ]
                edge_i$to [1] <- edge_i$from [2] <- genhash ()
                edge_i$xto [1] <- pts$x [i]
                edge_i$yto [1] <- pts$y [i]
                edge_i$xfr [2] <- pts$x [i]
                edge_i$yfr [2] <- pts$y [i]
                d_wt <- edge_i$d_weighted / edge_i$d
                t_wt <- edge_i$time_weighted / edge_i$time
                t_scale <- edge_i$time / edge_i$d
                
                xy_i <- data.frame (
                    x = as.numeric (c (edge_i [1, "xfr"], edge_i [1, "xto"], edge_i [2, "xto"])),
                    y = as.numeric (c (edge_i [1, "yfr"], edge_i [1, "yto"], edge_i [2, "yto"]))
                )
                dmat <- geodist::geodist (xy_i, measure = "geodesic")
                
                d_i <- geodist::geodist (
                    pts [i, c ("x", "y")],
                    pts [i, c ("x0", "y0")],
                    measure = "geodesic"
                )
                d_i <- as.numeric (d_i [1, 1])
                
                if (any (dmat [upper.tri (dmat)] < dist_tol)) {
                    
                    edge_i <- edges_i [e, ]
                    edge_i_new <- rbind (edge_i, edge_i) # for edges to new point
                    # Reverse 2nd edge:
                    edge_i_new$from [2] <- edge_i_new$to [1]
                    edge_i_new$to [2] <- edge_i_new$from [1]
                    edge_i_new$xfr [2] <- edge_i_new$xto [1]
                    edge_i_new$xto [2] <- edge_i_new$xfr [1]
                    edge_i_new$yfr [2] <- edge_i_new$yto [1]
                    edge_i_new$yto [2] <- edge_i_new$yfr [1]
                    
                    d_i_min <- c (1, 1, 2) [which.min (dmat [upper.tri (dmat)])]
                    if (d_i_min == 1) {
                        edge_i_new <- edge_i_new [2:1, ]
                    }
                    
                } else {
                    
                    edge_i$d [1] <- dmat [1, 2]
                    edge_i$d [2] <- dmat [2, 3]
                    
                    edge_i$d_weighted <- edge_i$d * d_wt
                    edge_i$time <- edge_i$d * t_scale
                    edge_i$time_weighted <- edge_i$time * t_wt
                    
                    edge_i$edge_id <- paste0 (
                        edge_i$edge_id,
                        "_",
                        LETTERS [seq_len (nrow (edge_i))]
                    )
                    
                    edge_i_new <- edge_i # already 2 rows
                }
                
                if (!intersections_only) {
                    
                    # Then add edges out to new point:
                    edge_i_new$from [1] <- edge_i_new$to [2] <- genhash (10L)
                    edge_i_new$xfr [1] <- pts$x0 [i]
                    edge_i_new$yfr [1] <- pts$y0 [i]
                    edge_i_new$xto [2] <- pts$x0 [i]
                    edge_i_new$yto [2] <- pts$y0 [i]
                    
                    edge_i_new$d <- d_i
                    edge_i_new$d_weighted <- d_i * d_wt
                    edge_i_new$time <- d_i * t_scale
                    edge_i_new$time_weighted <- edge_i_new$time * t_wt
                    
                    edge_i_new$edge_id <- vapply (
                        seq_len (nrow (edge_i_new)),
                        function (i) genhash (10),
                        character (1L)
                    )
                    
                    edge_i <- rbind (edge_i, edge_i_new)
                }
                
                return (edge_i)
            })
            
            return (do.call (rbind, new_edges_i))
        } else {
            # Multiple points match this edge - process them together
            
            # Process all instances of this edge (might be bi-directional)
            all_new_edges <- lapply (edge_indices, function (edge_idx) {
                edge_template <- edges_to_split [edge_idx, ]
                
                # Extract coordinates of the edge endpoints
                from_x <- edge_template$xfr
                from_y <- edge_template$yfr
                to_x <- edge_template$xto
                to_y <- edge_template$yto
                
                # Calculate edge length for reference
                edge_length <- sqrt ((to_x - from_x)^2 + (to_y - from_y)^2)
                
                # Calculate distances along the edge for each point
                # This helps order the points from start to end of the edge
                point_positions <- lapply (point_indices, function (i) {
                    # Get projection point coordinates
                    proj_x <- pts$x [i]
                    proj_y <- pts$y [i]
                    
                    # Calculate distance from 'from' node to projection
                    # Using vector projection to handle non-straight edges correctly
                    edge_vec_x <- to_x - from_x
                    edge_vec_y <- to_y - from_y
                    point_vec_x <- proj_x - from_x
                    point_vec_y <- proj_y - from_y
                    
                    # Project point vector onto edge vector
                    dot_product <- edge_vec_x * point_vec_x + edge_vec_y * point_vec_y
                    edge_length_sq <- edge_vec_x^2 + edge_vec_y^2
                    projection_ratio <- dot_product / edge_length_sq
                    
                    # Clamp ratio between 0 and 1 to handle points that project outside the edge
                    projection_ratio <- max (0, min (1, projection_ratio))
                    
                    # Calculate distance along the edge
                    dist_from_start <- projection_ratio * edge_length
                    
                    list (
                        index = i,
                        x = proj_x,
                        y = proj_y,
                        x0 = pts$x0 [i],
                        y0 = pts$y0 [i],
                        dist_from_start = dist_from_start,
                        projection_ratio = projection_ratio
                    )
                })
                
                # Sort points by their position along the edge
                point_positions <- point_positions [order (sapply (point_positions, function (p) p$dist_from_start))]
                
                # Now create edge segments for each point
                d_wt <- edge_template$d_weighted / edge_template$d
                t_wt <- edge_template$time_weighted / edge_template$time
                t_scale <- edge_template$time / edge_template$d
                
                # Create segments between points
                segments <- list ()
                
                # First segment: from original start to first point
                first_segment <- edge_template
                first_segment$to <- genhash ()
                first_segment$xto <- point_positions [[1]]$x
                first_segment$yto <- point_positions [[1]]$y
                segments [[1]] <- first_segment
                
                # Middle segments: between consecutive points
                if (length (point_positions) > 1) {
                    for (j in 1:(length (point_positions) - 1)) {
                        mid_segment <- edge_template
                        mid_segment$from <- segments [[j]]$to
                        mid_segment$to <- genhash ()
                        mid_segment$xfr <- point_positions [[j]]$x
                        mid_segment$yfr <- point_positions [[j]]$y
                        mid_segment$xto <- point_positions [[j + 1]]$x
                        mid_segment$yto <- point_positions [[j + 1]]$y
                        segments [[j + 1]] <- mid_segment
                    }
                }
                
                # Last segment: from last point to original end
                last_segment <- edge_template
                last_segment$from <- segments [[length (segments)]]$to
                last_segment$xfr <- point_positions [[length (point_positions)]]$x
                last_segment$yfr <- point_positions [[length (point_positions)]]$y
                segments [[length (segments) + 1]] <- last_segment
                
                # Convert segments to data frame
                segments_df <- do.call (rbind, segments)
                
                # Update distances and weights for all segments
                for (j in 1:nrow (segments_df)) {
                    xy_i <- data.frame (
                        x = c (segments_df$xfr [j], segments_df$xto [j]),
                        y = c (segments_df$yfr [j], segments_df$yto [j])
                    )
                    dmat <- geodist::geodist (xy_i, measure = "geodesic")
                    segments_df$d [j] <- as.numeric (dmat [1, 2])
                    segments_df$d_weighted [j] <- segments_df$d [j] * d_wt
                    segments_df$time [j] <- segments_df$d [j] * t_scale
                    segments_df$time_weighted [j] <- segments_df$time [j] * t_wt
                    segments_df$edge_id [j] <- paste0 (edge_template$edge_id, "_", LETTERS [j])
                }
                
                # If we need to add connections to original points
                if (!intersections_only) {
                    point_connections <- lapply (point_positions, function (p) {
                        # Create two edges (to and from) for each point
                        conn <- rbind (edge_template, edge_template)
                        
                        # Generate a unique ID for the original point
                        point_id <- genhash (10L)
                        
                        # Edge from projection to original point
                        conn$from [1] <- segments_df$to [which (segments_df$xto == p$x & segments_df$yto == p$y) [1]]
                        conn$to [1] <- point_id
                        conn$xfr [1] <- p$x
                        conn$yfr [1] <- p$y
                        conn$xto [1] <- p$x0
                        conn$yto [1] <- p$y0
                        
                        # Edge from original point to projection
                        conn$from [2] <- point_id
                        conn$to [2] <- conn$from [1]
                        conn$xfr [2] <- p$x0
                        conn$yfr [2] <- p$y0
                        conn$xto [2] <- p$x
                        conn$yto [2] <- p$y
                        
                        # Calculate distance from projection to original point
                        d_i <- geodist::geodist (
                            data.frame (x = p$x, y = p$y),
                            data.frame (x = p$x0, y = p$y0),
                            measure = "geodesic"
                        )
                        d_i <- as.numeric (d_i [1, 1])
                        
                        # Update distances and weights
                        conn$d <- d_i
                        conn$d_weighted <- d_i * d_wt
                        conn$time <- d_i * t_scale
                        conn$time_weighted <- conn$time * t_wt
                        
                        # Generate unique edge IDs
                        conn$edge_id <- vapply (
                            1:2,
                            function (i) genhash (10),
                            character (1L)
                        )
                        
                        return (conn)
                    })
                    
                    # Combine segments with point connections
                    all_edges <- rbind (segments_df, do.call (rbind, point_connections))
                    return (all_edges)
                } else {
                    return (segments_df)
                }
            })
            
            return (do.call (rbind, all_new_edges))
        }
    })
    
    edges_split <- do.call (rbind, edges_split)
    
    # Add original edge IDs for matching back to the graph
    edges_split$edge_id_orig <- rep (
        edges_to_split$edge_id,
        times = sapply (edge_to_points, function (x) nrow (edges_split) / length (all_edge_indices))
    )
    
    # Then match edges_split back on to original graph:
    graph_to_add <- graph_to_add [match (edges_split$edge_id_orig, graph_to_add$edge_id), ]
    gr_cols <- gr_cols [which (!is.na (gr_cols))]
    for (g in seq_along (gr_cols)) {
        graph_to_add [, gr_cols [g]] <- edges_split [[names (gr_cols) [g]]]
    }
    
    return (rbind (graph, graph_to_add))
}
