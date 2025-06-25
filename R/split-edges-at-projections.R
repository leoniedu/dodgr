#' Split graph edges at projection points
#'
#' This function finds the closest projection points from supplied coordinates to graph edges
#' and splits those edges at the projection points. Projection points that coincide with 
#' existing graph vertices are not created as new split points.
#'
#' @param graph A dodgr street network data frame.
#' @param xy A two-column matrix or data frame of coordinates (x, y).
#' @param dist_tol Numeric; minimum distance between split points and from existing vertices (default 1e-6).
#' @param debug Logical; if TRUE, print debug output.
#' @return Modified graph with edges split at projection points.
#' @export
split_edges_at_projections <- function(
    graph,
    xy,
    dist_tol = 1e-6,
    debug = FALSE
) {
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
    
    # Step 2: Filter points that are too close to existing vertices
    graph_verts <- dodgr_vertices(graph_std)
    vts <- match_points_to_verts(verts = graph_verts, xy = xy_unique)
    vertex_coords <- graph_verts[vts, c("x", "y"), drop = FALSE]
    
    pt_to_vertex_dists <- geodist::geodist(
        x = xy_unique,
        y = vertex_coords,
        paired = TRUE,
        measure = get_geodist_measure(graph)
    )
    
    keep_points <- pt_to_vertex_dists > dist_tol
    if (debug && sum(!keep_points) > 0) {
        warning(sum(!keep_points), " points removed for being within ", dist_tol, 
               " units of existing vertices")
    }
    xy_unique <- xy_unique[keep_points, , drop = FALSE]
    
    if (debug) cat("After vertex distance filtering:", nrow(xy_unique), "points remain\n")
    
    # Early return if no points remain after filtering
    if (nrow(xy_unique) == 0) {
        if (debug) cat("No points remain after vertex filtering, returning original graph\n")
        return(graph)
    }
    
    # Step 3: Match points to graph edges
    pts <- match_pts_to_graph(graph_std, xy_unique, distances = TRUE)
    pts$pt_id <- seq_len(nrow(pts))

    if (debug) cat("Points matched to", length(unique(pts$index)), "different edges\n")

    # --- Build edge lookup tables for fast reverse edge detection ---
    edge_key <- function(fr, to) paste(fr, to, sep = "_")
    edge_lookup <- setNames(seq_len(nrow(graph_std)), edge_key(graph_std$from, graph_std$to))
    reverse_lookup <- setNames(seq_len(nrow(graph_std)), edge_key(graph_std$to, graph_std$from))
    processed <- rep(FALSE, nrow(graph_std))
    split_edges <- list()

    for (edge_idx in unique(pts$index)) {
        if (processed[edge_idx]) next  # Already handled as reverse

        from_id <- graph_std$from[edge_idx]
        to_id   <- graph_std$to[edge_idx]
        reverse_key <- edge_key(to_id, from_id)
        reverse_idx <- reverse_lookup[[reverse_key]]

        # Get split points for this edge (in order along the edge)
        edge_pts_idx <- which(pts$index == edge_idx)
        edge_pts <- pts[edge_pts_idx, , drop = FALSE]

        # Sort edge_pts along the edge
        start_pt <- c(graph_std$xfr[edge_idx], graph_std$yfr[edge_idx])
        end_pt   <- c(graph_std$xto[edge_idx], graph_std$yto[edge_idx])
        edge_vec <- end_pt - start_pt
        edge_len <- sqrt(sum(edge_vec^2))
        proj_dists <- vapply(seq_len(nrow(edge_pts)), function(i) {
            pt_vec <- c(edge_pts$x[i], edge_pts$y[i]) - start_pt
            sum(pt_vec * edge_vec) / edge_len
        }, numeric(1))
        ord <- order(proj_dists)
        edge_pts <- edge_pts[ord, , drop = FALSE]

        # Filter out points too close to each other (optional: add vertex/spacing filtering here)
        if (nrow(edge_pts) > 1) {
            dx <- diff(edge_pts$x)
            dy <- diff(edge_pts$y)
            sq_dists <- dx^2 + dy^2
            keep_spacing <- c(TRUE, sq_dists > (dist_tol^2))
            edge_pts <- edge_pts[keep_spacing, , drop = FALSE]
        }

        n_splits <- nrow(edge_pts)
        if (n_splits == 0) {
            split_edges[[length(split_edges) + 1]] <- graph[edge_idx, ]
            processed[edge_idx] <- TRUE
            next
        }

        # --- Generate unique split node IDs (in order along edge) ---
        split_ids <- paste0(graph_std$edge_id[edge_idx], "_split", seq_len(n_splits))

        # --- Split the forward edge (from_id -> to_id) ---
        seg_starts <- rbind(
            data.frame(x = graph_std$xfr[edge_idx], y = graph_std$yfr[edge_idx]),
            data.frame(x = edge_pts$x, y = edge_pts$y)
        )
        seg_ends <- rbind(
            data.frame(x = edge_pts$x, y = edge_pts$y),
            data.frame(x = graph_std$xto[edge_idx], y = graph_std$yto[edge_idx])
        )
        for (i in seq_len(nrow(seg_starts))) {
            seg <- graph[edge_idx, ]
            seg_from <- if (i == 1) from_id else split_ids[i - 1]
            seg_to   <- if (i > n_splits) to_id else split_ids[i]
            seg_xfr <- seg_starts$x[i]
            seg_yfr <- seg_starts$y[i]
            seg_xto <- seg_ends$x[i]
            seg_yto <- seg_ends$y[i]
            seg_len <- geodist::geodist(
                data.frame(x = c(seg_xfr, seg_xto), y = c(seg_yfr, seg_yto)),
                measure = get_geodist_measure(graph)
            )[1, 2]
            d_ratio <- graph_std$d_weighted[edge_idx] / graph_std$d[edge_idx]
            t_ratio <- graph_std$time_weighted[edge_idx] / graph_std$time[edge_idx]
            seg[ , gr_cols["from"]] <- seg_from
            seg[ , gr_cols["to"]] <- seg_to
            seg[ , gr_cols["xfr"]] <- seg_xfr
            seg[ , gr_cols["yfr"]] <- seg_yfr
            seg[ , gr_cols["xto"]] <- seg_xto
            seg[ , gr_cols["yto"]] <- seg_yto
            seg[ , gr_cols["d"]] <- seg_len
            seg[ , gr_cols["d_weighted"]] <- seg_len * d_ratio
            seg[ , gr_cols["time"]] <- seg_len * (graph_std$time[edge_idx] / graph_std$d[edge_idx])
            seg[ , gr_cols["time_weighted"]] <- seg_len * (graph_std$time[edge_idx] / graph_std$d[edge_idx]) * t_ratio
            seg[ , gr_cols["edge_id"]] <- paste0(graph_std$edge_id[edge_idx], "_seg", i)
            split_edges[[length(split_edges) + 1]] <- seg
        }
        processed[edge_idx] <- TRUE

        # --- If reverse edge exists, split it using the same split points but reversed ---
        if (!is.null(reverse_idx) && !processed[reverse_idx]) {
            rev_split_ids <- rev(split_ids)
            rev_edge_pts  <- edge_pts[nrow(edge_pts):1, , drop = FALSE]
            seg_starts_rev <- rbind(
                data.frame(x = graph_std$xfr[reverse_idx], y = graph_std$yfr[reverse_idx]),
                data.frame(x = rev_edge_pts$x, y = rev_edge_pts$y)
            )
            seg_ends_rev <- rbind(
                data.frame(x = rev_edge_pts$x, y = rev_edge_pts$y),
                data.frame(x = graph_std$xto[reverse_idx], y = graph_std$yto[reverse_idx])
            )
            for (i in seq_len(nrow(seg_starts_rev))) {
                seg <- graph[reverse_idx, ]
                seg_from <- if (i == 1) to_id else rev_split_ids[i - 1]
                seg_to   <- if (i > n_splits) from_id else rev_split_ids[i]
                seg_xfr <- seg_starts_rev$x[i]
                seg_yfr <- seg_starts_rev$y[i]
                seg_xto <- seg_ends_rev$x[i]
                seg_yto <- seg_ends_rev$y[i]
                seg_len <- geodist::geodist(
                    data.frame(x = c(seg_xfr, seg_xto), y = c(seg_yfr, seg_yto)),
                    measure = get_geodist_measure(graph)
                )[1, 2]
                d_ratio <- graph_std$d_weighted[reverse_idx] / graph_std$d[reverse_idx]
                t_ratio <- graph_std$time_weighted[reverse_idx] / graph_std$time[reverse_idx]
                seg[ , gr_cols["from"]] <- seg_from
                seg[ , gr_cols["to"]] <- seg_to
                seg[ , gr_cols["xfr"]] <- seg_xfr
                seg[ , gr_cols["yfr"]] <- seg_yfr
                seg[ , gr_cols["xto"]] <- seg_xto
                seg[ , gr_cols["yto"]] <- seg_yto
                seg[ , gr_cols["d"]] <- seg_len
                seg[ , gr_cols["d_weighted"]] <- seg_len * d_ratio
                seg[ , gr_cols["time"]] <- seg_len * (graph_std$time[reverse_idx] / graph_std$d[reverse_idx])
                seg[ , gr_cols["time_weighted"]] <- seg_len * (graph_std$time[reverse_idx] / graph_std$d[reverse_idx]) * t_ratio
                seg[ , gr_cols["edge_id"]] <- paste0(graph_std$edge_id[reverse_idx], "_seg", i)
                split_edges[[length(split_edges) + 1]] <- seg
            }
            processed[reverse_idx] <- TRUE
        }
    }

    # Add untouched edges
    untouched_idx <- which(!processed)
    untouched_graph <- graph[untouched_idx, , drop = FALSE]
    result_graph <- rbind(untouched_graph, do.call(rbind, split_edges))
    rownames(result_graph) <- NULL
    if (debug) {
        cat("Original graph had", nrow(graph), "edges\n")
        cat("Result graph has", nrow(result_graph), "edges\n")
        cat("Added", nrow(result_graph) - nrow(graph), "edges from splitting\n")
    }
    return(result_graph)
}
