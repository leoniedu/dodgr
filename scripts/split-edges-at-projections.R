# Helper function to generate unique IDs
genhash <- function(len = 10) {
    paste0(sample(c(0:9, letters, LETTERS), size = len, replace = TRUE), collapse = "")
}

# Helper to map edge segments calculated on the standardised graph
# back onto rows with full original columns. It takes a prototype row
# from the original `graph` (which contains *all* columns), and for
# every row in `segments_std` (which only has the standard columns), a
# copy of the prototype is made with the standard columns overwritten
# by the values from `segments_std`.
convert_segments_to_original <- function(proto_row, segments_std, gr_cols) {
    segs <- lapply(seq_len(nrow(segments_std)), function(i) {
        seg <- proto_row
        for (g in seq_along(gr_cols)) {
            seg[[gr_cols[g]]] <- segments_std[[names(gr_cols)[g]]][i]
        }
        seg
    })
    do.call(rbind, segs)
}

#' Split graph edges at projection points
#'
#' This function finds the closest projection points from supplied coordinates to graph edges
#' and splits those edges at the projection points. Projection points that coincide with 
#' existing graph vertices are not created as new split points.
#'
#' @param graph A dodgr street network data frame.
#' @param xy A two-column matrix or data frame of coordinates (x, y).
#' @param dist_tol Numeric; minimum distance between split points (default 1e-6).
#' @param dist_min Numeric; minimum distance from existing vertices to create splits (default dist_tol).
#' @param debug Logical; if TRUE, print debug output.
#' @return Modified graph with edges split at projection points.
#' @export
split_edges_at_projections <- function(
    graph,
    xy,
    dist_tol = 1e-6,
    dist_min = dist_tol,
    debug = FALSE
) {
    if (debug) cat("Starting split_edges_at_projections with", nrow(xy), "points\n")
    
    # Get standard column indices
    gr_cols <- dodgr_graph_cols(graph)
    gr_cols <- unlist(gr_cols[!is.na(gr_cols)])
    
    # Create standardized version like in match-points.R
    graph_std <- graph[, gr_cols]  # standardise column names
    names(graph_std) <- names(gr_cols)
    
    # Preprocess points
    xy <- pre_process_xy(xy)
    xy_unique <- unique(xy)
    
    if (debug && nrow(xy) > nrow(xy_unique)) {
        warning(nrow(xy) - nrow(xy_unique), " duplicated points removed.")
    }
    
    # Filter points too close to existing vertices
    graph_verts <- dodgr_vertices(graph_std)
    vts <- match_points_to_verts(verts = graph_verts, xy = xy_unique)
    vertex_coords <- graph_verts[vts, c("x", "y"), drop = FALSE]
    
    pt_to_vertex_dists <- geodist::geodist(
        x = xy_unique,
        y = vertex_coords,
        paired = TRUE,
        measure = get_geodist_measure(graph)
    )
    
    keep_points <- pt_to_vertex_dists > dist_min
    if (debug && sum(!keep_points) > 0) {
        warning(sum(!keep_points), " points removed for being within ", dist_min, 
               " units of existing vertices")
    }
    xy_unique <- xy_unique[keep_points, , drop = FALSE]
    
    if (nrow(xy_unique) == 0) {
        if (debug) cat("No valid points remain after filtering\n")
        return(graph)
    }
    
    # Match points to graph edges
    pts <- match_pts_to_graph(graph_std, xy_unique, distances = TRUE)
    pts$pt_id <- seq_len(nrow(pts))
    
    # Group points by edge and find bidirectional pairs
    edge_matches <- split(seq_len(nrow(pts)), pts$index)
    edges_with_pts <- as.integer(names(edge_matches))
    
    # Find bidirectional edge pairs
    edge_pairs <- lapply(edges_with_pts, function(i) {
        e1 <- graph_std[i, ]
        e2 <- which(graph_std$from == e1$to & graph_std$to == e1$from)
        if (length(e2) > 0) c(i, e2) else i
    })
    
    # Process each unique edge or edge pair
    processed_edges <- logical(nrow(graph_std))
    segments_all <- list()  # store std segments
    
    for (edge_set in edge_pairs) {
        if (all(processed_edges[edge_set])) next
        
        # Mark edges as processed
        processed_edges[edge_set] <- TRUE
        
        # Get the edges to process
        edge_indices <- edge_set[1]
        if (length(edge_set) > 1) {
            edge_indices <- c(edge_indices, edge_set[2])
        }
        
        # Get points for these edges (including both directions)
        edge_pts <- pts[pts$index %in% edge_indices, , drop = FALSE]
        
        if (nrow(edge_pts) == 0) {
            # No points to split, keep original edges
            segments_std <- graph_std[edge_indices, ]
            segments_std$idx <- edge_indices
            segments_all[[length(segments_all)+1]] <- segments_std
            next
        }
        
        # Process each edge in the set
        for (i in seq_along(edge_indices)) {
            edge_idx <- edge_indices[i]
            edge_row_orig <- graph[edge_idx, ]
            
            # Get points for this specific edge
            edge_pts_i <- edge_pts[edge_pts$index == edge_idx, , drop = FALSE]
            
            if (nrow(edge_pts_i) > 0) {
                # Process the edge on the *standardised* graph
                segments_std <- process_edge_with_splits(
                    edge_row_std = graph_std[edge_idx, ],
                    reverse_edge = NULL,
                    edge_pts = edge_pts_i,
                    graph_verts = graph_verts,
                    graph = graph_std,
                    gr_cols = gr_cols,
                    dist_tol = dist_tol,
                    debug = debug
                )
                
                segments_std$idx <- edge_idx
                segments_all[[length(segments_all)+1]] <- segments_std
            } else {
                # No splits, keep original edge
                segments_std <- graph_std[edge_idx, ]
                segments_std$idx <- edge_idx
                segments_all[[length(segments_all)+1]] <- segments_std
            }
        }
    }
    
    # Construct final graph following match-points.R pattern
    if (length(segments_all) > 0) {
        edges_split <- do.call(rbind, segments_all)
        graph_to_add <- graph[edges_split$idx, , drop = FALSE]
        gr_cols_vec <- gr_cols[!is.na(gr_cols)]
        for (g in seq_along(gr_cols_vec)) {
            graph_to_add[, gr_cols_vec[g]] <- edges_split[[names(gr_cols_vec)[g]]]
        }
        graph_out <- rbind(graph[-unique(edges_split$idx), , drop = FALSE], graph_to_add)
    } else {
        graph_out <- graph
    }
    
    rownames(graph_out) <- NULL
    
    if (debug) {
        cat("Original graph had", nrow(graph), "edges\n")
        cat("Result graph has", nrow(graph_out), "edges\n")
    }
    
    return(graph_out)
}

# Helper function to process an edge and its reverse
process_edge_with_splits <- function(edge_row_std, reverse_edge, edge_pts, 
                                     graph_verts, graph, gr_cols, 
                                     dist_tol, debug) {
    
    # Calculate weight ratios
    d_wt <- if (!is.null(edge_row_std$d_weighted)) edge_row_std$d_weighted / edge_row_std$d else NA_real_
    t_wt <- if (!is.null(edge_row_std$time_weighted) && !is.null(edge_row_std$time)) edge_row_std$time_weighted / edge_row_std$time else NA_real_
    t_scale <- if (!is.null(edge_row_std$time)) edge_row_std$time / edge_row_std$d else NA_real_
    
    # Sort points along the edge
    start_pt <- c(edge_row_std$xfr, edge_row_std$yfr)
    end_pt <- c(edge_row_std$xto, edge_row_std$yto)
    edge_vec <- end_pt - start_pt
    edge_len <- sqrt(sum(edge_vec^2))
    
    # Calculate projection distances and sort
    proj_dists <- vapply(seq_len(nrow(edge_pts)), function(i) {
        pt_vec <- c(edge_pts$x[i], edge_pts$y[i]) - start_pt
        sum(pt_vec * edge_vec) / edge_len
    }, numeric(1))
    
    ord <- order(proj_dists)
    proj_dists <- proj_dists[ord]
    edge_pts <- edge_pts[ord, , drop = FALSE]
    
    # Filter points too close to vertices
    proj_coords <- data.frame(x = edge_pts$x, y = edge_pts$y)
    vertex_matches <- match_points_to_verts(verts = graph_verts, xy = proj_coords)
    vertex_coords_matched <- graph_verts[vertex_matches, c("x", "y"), drop = FALSE]
    
    proj_to_vertex_dists <- geodist::geodist(
        x = proj_coords,
        y = vertex_coords_matched,
        paired = TRUE,
        measure = get_geodist_measure(graph)
    )
    
    keep_projs <- proj_to_vertex_dists > dist_tol
    edge_pts <- edge_pts[keep_projs, , drop = FALSE]
    
    # Filter points too close to each other
    if (nrow(edge_pts) > 1) {
        dx <- diff(edge_pts$x)
        dy <- diff(edge_pts$y)
        sq_dists <- dx^2 + dy^2
        keep_spacing <- c(TRUE, sq_dists > (dist_tol^2))
        edge_pts <- edge_pts[keep_spacing, , drop = FALSE]
    }
    
    # Create split vertices
    n_splits <- nrow(edge_pts)
    if (n_splits == 0) {
        # No splits, just return the original edge row as data frame
        return(as.data.frame(edge_row_std))
    }
    
    # Create edge segments
    split_ids <- genhash(n_splits)
    new_edges <- create_edge_segments(
        edge_row_std, edge_pts, split_ids, 
        gr_cols, graph,
        is_reverse = FALSE
    )
    
    # Process reverse edge if it exists
    if (!is.null(reverse_edge)) {
        rev_idx <- rev(seq_len(nrow(edge_pts)))
        reverse_edges <- create_edge_segments(
            reverse_edge, 
            edge_pts[rev_idx, , drop = FALSE],  # Reverse point order
            rev(split_ids),  # Reverse split IDs to maintain correspondence
            gr_cols, graph,
            is_reverse = TRUE
        )
        new_edges <- rbind(new_edges, reverse_edges)
    }
    
    return(new_edges)
}

# Helper function to create edge segments
create_edge_segments <- function(edge_row_std, edge_pts, split_ids, 
                                gr_cols, graph,
                                is_reverse = FALSE) {
    
    n_splits <- nrow(edge_pts)
    new_edges <- vector("list", n_splits + 1)
    
    # Get the column types from the original edge
    col_types <- vapply(edge_row_std, class, character(1))
    
    for (i in seq_len(n_splits + 1)) {
        # Create a new row with all original columns and their values
        seg <- edge_row_std[1, ]
        
        # Preserve all original column values that aren't being explicitly set below
        # We'll update the specific columns that need to change for each segment
        # This ensures all additional columns (like highway, way_id, etc.) are preserved
        
        # Get column indices from gr_cols
        from_col <- gr_cols["from"]
        to_col <- gr_cols["to"]
        xfr_col <- gr_cols["xfr"]
        yfr_col <- gr_cols["yfr"]
        xto_col <- gr_cols["xto"]
        yto_col <- gr_cols["yto"]
        d_col <- gr_cols["d"]
        d_wt_col <- gr_cols["d_weighted"]
        time_col <- gr_cols["time"]
        time_wt_col <- gr_cols["time_weighted"]
        edge_id_col <- gr_cols["edge_id"]
        
        # Set from and to coordinates for this segment
        if (i == 1) {
            seg_xfr <- if (is_reverse) edge_row_std[[xto_col]] else edge_row_std[[xfr_col]]
            seg_yfr <- if (is_reverse) edge_row_std[[yto_col]] else edge_row_std[[yfr_col]]
        } else {
            seg_xfr <- edge_pts$x[i-1]
            seg_yfr <- edge_pts$y[i-1]
        }
        
        if (i == n_splits + 1) {
            seg_xto <- if (is_reverse) edge_row_std[[xfr_col]] else edge_row_std[[xto_col]]
            seg_yto <- if (is_reverse) edge_row_std[[yfr_col]] else edge_row_std[[yto_col]]
        } else {
            seg_xto <- edge_pts$x[i]
            seg_yto <- edge_pts$y[i]
        }
        
        # Create proper data frames for coordinates
        from_pt <- data.frame(x = as.numeric(seg_xfr), y = as.numeric(seg_yfr))
        to_pt <- data.frame(x = as.numeric(seg_xto), y = as.numeric(seg_yto))
        
        # Calculate segment length
        seg_len <- geodist::geodist(
            x = from_pt,
            y = to_pt,
            paired = TRUE,
            measure = get_geodist_measure(graph)
        )
        
        # Set from and to vertex IDs
        if (i == 1) {
            from_id <- if (is_reverse) edge_row_std[[to_col]] else edge_row_std[[from_col]]
        } else {
            from_id <- split_ids[i-1]
        }
        
        if (i == n_splits + 1) {
            to_id <- if (is_reverse) edge_row_std[[from_col]] else edge_row_std[[to_col]]
        } else {
            to_id <- split_ids[i]
        }
        
        # Calculate time and weighted-time for this segment only if columns exist
        seg_time <- NA_real_
        seg_time_wt <- NA_real_
        if (!is.na(time_col)) {
            t_scale <- edge_row_std[[time_col]] / edge_row_std[[d_col]]
            seg_time <- seg_len * t_scale
            if (!is.na(time_wt_col)) {
                t_wt <- edge_row_std[[time_wt_col]] / edge_row_std[[time_col]]
                seg_time_wt <- seg_time * t_wt
            }
        }
        
        # Update segment properties using column indices
        seg[[from_col]] <- from_id
        seg[[to_col]] <- to_id
        seg[[xfr_col]] <- as.numeric(seg_xfr)
        seg[[yfr_col]] <- as.numeric(seg_yfr)
        seg[[xto_col]] <- as.numeric(seg_xto)
        seg[[yto_col]] <- as.numeric(seg_yto)  # Added missing yto column
        seg[[d_col]] <- seg_len
        
        # Update coordinate columns if they exist
        if ("from_lon" %in% names(edge_row_std)) seg$from_lon <- as.numeric(seg_xfr)
        if ("from_lat" %in% names(edge_row_std)) seg$from_lat <- as.numeric(seg_yfr)
        if ("to_lon" %in% names(edge_row_std)) seg$to_lon <- as.numeric(seg_xto)
        if ("to_lat" %in% names(edge_row_std)) seg$to_lat <- as.numeric(seg_yto)
        
        # Update weighted distance if the column exists (following match-points.R pattern)
        if (!is.na(d_wt_col) && d_wt_col <= ncol(edge_row_std)) {
            d_wt <- edge_row_std[[d_wt_col]] / edge_row_std[[d_col]]
            seg[[d_wt_col]] <- as.numeric(seg_len * d_wt)
        }
        
        # Set the calculated times on the segment
        if (!is.na(time_col)) {
            seg[[time_col]] <- seg_time
        }
        
        if (!is.na(time_wt_col)) {
            seg[[time_wt_col]] <- seg_time_wt
        }
        
        # Update edge ID with segment number if edge_id column exists
        if (!is.na(edge_id_col)) {
            seg[[edge_id_col]] <- as.character(paste0(edge_row_std[[edge_id_col]], "_s", i, "_", genhash(4)))
        }
        
        # Preserve all other columns from the original edge
        other_cols <- setdiff(names(edge_row_std), 
                            c(names(gr_cols), "from", "to", "xfr", "yfr", "xto", "yto", 
                              "from_lon", "from_lat", "to_lon", "to_lat",
                              "d", "d_weighted", "time", "time_weighted", "edge_id"))
        for (col in other_cols) {
            if (col %in% names(seg) && col %in% names(edge_row_std)) {
                seg[[col]] <- edge_row_std[[col]][1]  # Take first element in case it's a list
            }
        }
        
        new_edges[[i]] <- seg
    }
    
    do.call(rbind, new_edges)
}
