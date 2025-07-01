#' Split graph edges at projection points
#'
#' This function finds the closest projection points from supplied coordinates to graph edges
#' and splits those edges at the projection points. Projection points that coincide with 
#' existing graph vertices are not created as new split points.
#'
#' @param graph A dodgr street network data frame.
#' @param xy A two-column matrix or data frame of coordinates (x, y).
#' @param dist_tol Numeric; minimum distance between split points and from existing vertices in meters (default 1.0).
#' @param debug Logical; if TRUE, print debug output.
#' @return Modified graph with edges split at projection points.
#' @export
split_edges_at_projections <- function(
    graph,
    xy,
    dist_tol = 1.0,
    debug = FALSE
) {
    measure <- get_geodist_measure (graph)
    if (debug) cat("Starting split_edges_at_projections with", nrow(xy), "points\n")
    
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
    
    # Step 2: Match points to graph edges
    pts <- match_pts_to_graph(graph_std, xy_unique, distances = TRUE)
    pts <- unique(pts[, c("index", "x", "y")])  # Only need edge index and projection coordinates
    
    # Step 3: Filter out projection points too close to edge vertices
    if (nrow(pts) > 0) {
        # Get all edge vertices for the matched edges
        edge_indices <- pts$index
        start_vertices <- data.frame(
            x = graph_std$xfr[edge_indices], 
            y = graph_std$yfr[edge_indices]
        )
        end_vertices <- data.frame(
            x = graph_std$xto[edge_indices], 
            y = graph_std$yto[edge_indices]
        )
        pt_coords <- data.frame(x = pts$x, y = pts$y)
        
        # Calculate distances to start and end vertices (vectorized)
        dist_to_start <- geodist::geodist(
            x = pt_coords,
            y = start_vertices,
            paired = TRUE,
            measure = measure
        )
        
        dist_to_end <- geodist::geodist(
            x = pt_coords,
            y = end_vertices,
            paired = TRUE,
            measure = measure
        )
        
        # Keep points that are far enough from both vertices
        keep_pts <- (dist_to_start > dist_tol) & (dist_to_end > dist_tol)
        pts <- pts[keep_pts, , drop = FALSE]
    }
    
    pts$pt_id <- seq_len(nrow(pts))

    if (debug) cat(nrow(pts), "points matched to", length(unique(pts$index)), "different edges\n")

    # Calculate precise allocation size for memory optimization
    edges_with_pts <- unique(pts$index)
    pts_per_edge <- table(pts$index)
    
    # Build edge lookup tables for fast reverse edge detection
    edge_key <- function(fr, to) paste(fr, to, sep = "_")
    edge_lookup <- setNames(seq_len(nrow(graph_std)), edge_key(graph_std$from, graph_std$to))
    reverse_lookup <- setNames(seq_len(nrow(graph_std)), edge_key(graph_std$to, graph_std$from))
    
    # Calculate total segments needed: forward + reverse
    forward_segments <- sum(pts_per_edge + 1)  # Each edge with n points creates n+1 segments
    
    # Estimate reverse segments as equal to forward (most edges are bidirectional)
    # This is a conservative over-estimate that avoids expensive reverse edge lookup
    reverse_segments <- forward_segments
    
    # Pre-allocate only for split segments (not untouched edges)
    total_split_segments <- forward_segments + reverse_segments
    
    if (debug) cat("Pre-allocating for", total_split_segments, "split segments (", forward_segments, "forward +", reverse_segments, "reverse)\n")
    
    # Pre-allocate result matrix for split segments only
    if (total_split_segments > 0) {
        split_graph <- graph[rep(1, total_split_segments), , drop = FALSE]
    } else {
        split_graph <- graph[0, , drop = FALSE]  # Empty with same structure
    }
    result_idx <- 0
    
    # Get column indices for efficient updates
    gr_cols <- dodgr_graph_cols(graph)
    gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
    
    processed <- rep(FALSE, nrow(graph_std))
    if (debug) cat("starting loop over", length(edges_with_pts), "edges\n")
    edge_counter <- 0
    for (edge_idx in edges_with_pts) {
        if (processed[edge_idx]) next  # Already handled as reverse
        
        edge_counter <- edge_counter + 1
        if (debug && edge_counter %% 100 == 0) {
            cat("Processing edge", edge_counter, "of", length(edges_with_pts), "\n")
        }

        from_id <- graph_std$from[edge_idx]
        to_id   <- graph_std$to[edge_idx]
        reverse_key <- edge_key(to_id, from_id)
        reverse_idx <- reverse_lookup[[reverse_key]]

        edge_pts_idx <- which(pts$index == edge_idx)
        proj_pts <- pts[edge_pts_idx, , drop = FALSE]
        
        # Since we already filtered out points too close to vertices in Step 4,
        # we can work directly with projection points without including vertices
        edge_pts <- proj_pts
        
        # Calculate edge length and vector for projection calculations
        start_pt <- c(graph_std$xfr[edge_idx], graph_std$yfr[edge_idx])
        end_pt <- c(graph_std$xto[edge_idx], graph_std$yto[edge_idx])
        edge_vec <- end_pt - start_pt
        edge_len <- geodist::geodist(
            x = data.frame(x = c(start_pt[1], end_pt[1]), y = c(start_pt[2], end_pt[2])),
            measure = measure
        )[1,2]
        
        if (edge_len > 0) {
            # Vectorized geodesic projection distance calculation
            start_coords <- data.frame(
                x = rep(start_pt[1], nrow(edge_pts)),
                y = rep(start_pt[2], nrow(edge_pts))
            )
            proj_coords <- data.frame(x = edge_pts$x, y = edge_pts$y)
            
            proj_dists <- geodist::geodist(
                x = start_coords,
                y = proj_coords, 
                paired = TRUE,
                measure = measure
            )
            
            # Sort all points by projection distance
            ord <- order(proj_dists)
            edge_pts <- edge_pts[ord, , drop = FALSE]
            proj_dists <- proj_dists[ord]
        } else {
            next  # Skip degenerate edges
        }
        
        # Apply point spacing filter if we have multiple points
        n_edge_pts <- nrow(edge_pts)
        if (n_edge_pts > 1) {
            pt_coords <- data.frame(x = edge_pts$x, y = edge_pts$y)
            pt_dists <- geodist::geodist(
                x = pt_coords[-nrow(pt_coords), ],
                y = pt_coords[-1, ],
                paired = TRUE,
                measure = measure
            )
            keep_spacing <- c(TRUE, pt_dists > dist_tol)
            edge_pts <- edge_pts[keep_spacing, , drop = FALSE]
        }

        # Check if we have any points left after filtering
        n_total_pts <- nrow(edge_pts)
        if (n_total_pts == 0) {
            # No projection points remain - skip this edge
            next
        }
        
        # Generate unique split node IDs
        split_ids <- paste0(graph_std$edge_id[edge_idx], "_split", seq_len(n_total_pts))

        # Create segments: from original vertex -> through projections -> to original vertex
        # We need n_splits + 1 segments total
        n_segments <- n_total_pts + 1
        
        # Pre-calculate ratios for this edge
        d_ratio <- graph_std$d_weighted[edge_idx] / graph_std$d[edge_idx]
        t_ratio <- graph_std$time_weighted[edge_idx] / graph_std$time[edge_idx]
        
        for (i in seq_len(n_segments)) {
            result_idx <- result_idx + 1
            
            # Copy mother edge properties
            split_graph[result_idx, ] <- graph[edge_idx, ]
            
            # Determine segment endpoints
            if (i == 1) {
                # First segment: from original start vertex to first projection point
                seg_from <- from_id
                seg_to <- split_ids[1]
                seg_xfr <- graph_std$xfr[edge_idx]
                seg_yfr <- graph_std$yfr[edge_idx]
                seg_xto <- edge_pts$x[1]
                seg_yto <- edge_pts$y[1]
            } else if (i == n_segments) {
                # Last segment: from last projection point to original end vertex
                seg_from <- split_ids[n_total_pts]
                seg_to <- to_id
                seg_xfr <- edge_pts$x[n_total_pts]
                seg_yfr <- edge_pts$y[n_total_pts]
                seg_xto <- graph_std$xto[edge_idx]
                seg_yto <- graph_std$yto[edge_idx]
            } else {
                # Middle segments: between consecutive projection points
                seg_from <- split_ids[i - 1]
                seg_to <- split_ids[i]
                seg_xfr <- edge_pts$x[i - 1]
                seg_yfr <- edge_pts$y[i - 1]
                seg_xto <- edge_pts$x[i]
                seg_yto <- edge_pts$y[i]
            }
            
            # Calculate segment length
            seg_len <- geodist::geodist(
                data.frame(x = c(seg_xfr, seg_xto), y = c(seg_yfr, seg_yto)),
                measure = measure
            )[1, 2]
            
            # Update only the columns that change
            split_graph[result_idx, gr_cols["from"]] <- seg_from
            split_graph[result_idx, gr_cols["to"]] <- seg_to
            split_graph[result_idx, gr_cols["xfr"]] <- seg_xfr
            split_graph[result_idx, gr_cols["yfr"]] <- seg_yfr
            split_graph[result_idx, gr_cols["xto"]] <- seg_xto
            split_graph[result_idx, gr_cols["yto"]] <- seg_yto
            split_graph[result_idx, gr_cols["d"]] <- seg_len
            split_graph[result_idx, gr_cols["d_weighted"]] <- seg_len * d_ratio
            split_graph[result_idx, gr_cols["time"]] <- seg_len * (graph_std$time[edge_idx] / graph_std$d[edge_idx])
            split_graph[result_idx, gr_cols["time_weighted"]] <- seg_len * (graph_std$time[edge_idx] / graph_std$d[edge_idx]) * t_ratio
            split_graph[result_idx, gr_cols["edge_id"]] <- paste0(graph_std$edge_id[edge_idx], "_seg", i)
        }
        processed[edge_idx] <- TRUE

        # If reverse edge exists, split it using the same split points but reversed
        if (!is.null(reverse_idx) && !processed[reverse_idx]) {
            rev_split_ids <- rev(split_ids)
            
            # Use the same filtered edge_pts but in reverse order
            rev_edge_pts <- edge_pts[n_total_pts:1, , drop = FALSE]
            
            # Pre-calculate ratios for reverse edge
            d_ratio_rev <- graph_std$d_weighted[reverse_idx] / graph_std$d[reverse_idx]
            t_ratio_rev <- graph_std$time_weighted[reverse_idx] / graph_std$time[reverse_idx]
            
            for (i in seq_len(n_segments)) {
                result_idx <- result_idx + 1
                
                # Copy mother edge properties
                split_graph[result_idx, ] <- graph[reverse_idx, ]
                
                # Determine segment endpoints (reverse direction)
                if (i == 1) {
                    # First segment: from original end vertex to first projection point (reversed)
                    seg_from <- to_id
                    seg_to <- rev_split_ids[1]
                    seg_xfr <- graph_std$xto[edge_idx]
                    seg_yfr <- graph_std$yto[edge_idx]
                    seg_xto <- rev_edge_pts$x[1]
                    seg_yto <- rev_edge_pts$y[1]
                } else if (i == n_segments) {
                    # Last segment: from last projection point to original start vertex (reversed)
                    seg_from <- rev_split_ids[n_total_pts]
                    seg_to <- from_id
                    seg_xfr <- rev_edge_pts$x[n_total_pts]
                    seg_yfr <- rev_edge_pts$y[n_total_pts]
                    seg_xto <- graph_std$xfr[edge_idx]
                    seg_yto <- graph_std$yfr[edge_idx]
                } else {
                    # Middle segments: between consecutive projection points (reversed)
                    seg_from <- rev_split_ids[i - 1]
                    seg_to <- rev_split_ids[i]
                    seg_xfr <- rev_edge_pts$x[i - 1]
                    seg_yfr <- rev_edge_pts$y[i - 1]
                    seg_xto <- rev_edge_pts$x[i]
                    seg_yto <- rev_edge_pts$y[i]
                }
                
                # Calculate segment length
                seg_len <- geodist::geodist(
                    data.frame(x = c(seg_xfr, seg_xto), y = c(seg_yfr, seg_yto)),
                    measure = measure
                )[1, 2]
                
                # Update only the columns that change
                split_graph[result_idx, gr_cols["from"]] <- seg_from
                split_graph[result_idx, gr_cols["to"]] <- seg_to
                split_graph[result_idx, gr_cols["xfr"]] <- seg_xfr
                split_graph[result_idx, gr_cols["yfr"]] <- seg_yfr
                split_graph[result_idx, gr_cols["xto"]] <- seg_xto
                split_graph[result_idx, gr_cols["yto"]] <- seg_yto
                split_graph[result_idx, gr_cols["d"]] <- seg_len
                split_graph[result_idx, gr_cols["d_weighted"]] <- seg_len * d_ratio_rev
                split_graph[result_idx, gr_cols["time"]] <- seg_len * (graph_std$time[reverse_idx] / graph_std$d[reverse_idx])
                split_graph[result_idx, gr_cols["time_weighted"]] <- seg_len * (graph_std$time[reverse_idx] / graph_std$d[reverse_idx]) * t_ratio_rev
                split_graph[result_idx, gr_cols["edge_id"]] <- paste0(graph_std$edge_id[reverse_idx], "_seg", i)
            }
            processed[reverse_idx] <- TRUE
        }
    }
    if (debug) cat("finished loop over", length(edges_with_pts), "edges\n")
    # Trim split_graph to actual segments created (in case filtering reduced the count)
    if (total_split_segments > 0 && result_idx < total_split_segments) {
        split_graph <- split_graph[1:result_idx, , drop = FALSE]
    }
    # Add untouched edges to remaining positions
    untouched_idx <- which(!processed)
    if (length(untouched_idx) > 0) {
        untouched_graph <- graph[untouched_idx, ]
        result_graph <- rbind(split_graph, untouched_graph)
    } else {
        result_graph <- split_graph
    }
    
    rownames(result_graph) <- NULL
    
    if (debug) {
        cat("Original graph had", nrow(graph), "edges\n")
        cat("Result graph has", nrow(result_graph), "edges\n")
        cat("Added", nrow(result_graph) - nrow(graph), "edges from splitting\n")
    }
    return(result_graph)
}
