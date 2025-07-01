#' Add edges between specified points and closest graph vertices
#'
#' Creates new edges connecting specified point coordinates to their closest
#' vertices in the graph, as determined by \code{match_pts_to_verts}. The new
#' edges inherit properties from the first edge found that contains the matched
#' vertex, ensuring consistent weight and time calculations.
#'
#' @param graph A \code{data.frame} representing the graph with the specified
#' columns of a \code{dodgr} graph.
#' @param xy Coordinates of points to be connected to the graph. Can be either
#' a matrix or \code{data.frame} with two columns (longitude and latitude),
#' a vector of length two, or an \code{sf} object. If a \code{data.frame},
#' it may optionally include an \code{id} column to specify custom IDs for
#' the points.
#' @param highway Character string specifying the highway type for the new edges.
#' This determines the weight calculations for \code{d_weighted}, \code{time},
#' and \code{time_weighted}. Original graph edges retain their existing highway
#' and other column values.
#' @param wt_profile Weight profile for the specified mode of transport (see
#' \link{weight_streetnet}). If \code{NULL} (default), edge properties are
#' inherited from the matched edges in the graph.
#' @param wt_profile_file Custom profile file for weights (see
#' \link{weight_streetnet}). Only used when \code{wt_profile} is specified.
#' @param bidirectional If \code{TRUE} (default), creates bidirectional edges
#' (to and from each point). If \code{FALSE}, creates only unidirectional edges
#' from points to vertices.
#'
#' @details The function matches each point to its closest vertex using
#' \code{match_pts_to_verts}, then creates new edges with properties based on
#' the first existing edge found that contains the matched vertex. When
#' \code{wt_profile} is \code{NULL}, edge weights and times are calculated by
#' inheriting ratios from the matched edges. When a weight profile is specified,
#' standard weight calculations are applied based on the highway type and profile.
#'
#' @return A modified graph with additional edges connecting the specified
#' points to their closest vertices.
#'
#' @family graph modification
#' @export
#'
#' @examples
#' \dontrun{
#' graph <- weight_streetnet(hampi)
#' pts <- matrix(c(76.46, 15.34, 76.47, 15.35), ncol = 2)
#' graph_with_edges <- add_edges_to_graph(graph, pts, highway = "residential")
#' 
#' # With custom IDs
#' pts_with_id <- data.frame(
#'   x = c(76.46, 76.47), 
#'   y = c(15.34, 15.35),
#'   id = c("point1", "point2")
#' )
#' graph_with_edges <- add_edges_to_graph(graph, pts_with_id, highway = "residential")
#' }
add_edges_to_graph <- function(graph,
                              xy,
                              highway,
                              wt_profile = NULL,
                              wt_profile_file = NULL,
                              bidirectional = TRUE) {

    # Validate inputs
    if (missing(highway)) {
        stop("'highway' argument is required")
    }
    
    gr_cols <- dodgr_graph_cols(graph)
    measure <- get_geodist_measure(graph)
    
    # Check for id column in xy input before processing
    has_id_col <- FALSE
    point_ids <- NULL
    xy_for_processing <- xy
    if (is.data.frame(xy) && "id" %in% names(xy)) {
        stopifnot(all(!duplicated(xy$id)))
        has_id_col <- TRUE
        point_ids <- as.character(xy$id)
        # Remove id column for coordinate processing
        xy_for_processing <- xy[, !names(xy) %in% "id", drop = FALSE]
    }
    
    # Get vertices and match points to them
    verts <- dodgr_vertices(graph)
    cli::cli_inform("Matching points to vertices...")
    matched_indices <- match_pts_to_verts(verts, xy_for_processing)
    cli::cli_inform("Matching points to vertices... done")
    
    # Pre-process xy coordinates
    xy_processed <- pre_process_xy(xy_for_processing)
    
    # Standardize graph column names following add_nodes_to_graph pattern
    gr_cols <- dodgr_graph_cols(graph)
    gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
    graph_std <- graph[, gr_cols] # standardise column names
    names(graph_std) <- names(gr_cols)
    
    # TODO: genhash is duplicated in multiple files - should be consolidated to utils
    genhash <- function(len = 10) {
        paste0(sample(c(0:9, letters, LETTERS), size = len), collapse = "")
    }
    
    # Get weight profile if specified
    wp <- NULL
    if (!is.null(wt_profile) || !is.null(wt_profile_file)) {
        wp <- get_profile(wt_profile = wt_profile, file = wt_profile_file)
        way_wt <- wp$value[wp$way == highway]
        if (length(way_wt) == 0) {
            stop("Highway type '", highway, "' not found in weight profile")
        }
    } else {
        way_wt <- 1
    }
    
    # Create pts structure similar to add_nodes_to_graph
    # pts$index points to the first edge containing each matched vertex
    # Find first edge containing each matched vertex
    cli::cli_inform("Finding first edge containing each matched vertex...")
    edges <- data.frame(vertice_id=c(graph_std$from,graph_std$to), edge_id=c(graph_std$edge_id,graph_std$edge_id), index=c(1:nrow(graph_std),1:nrow(graph_std)))
    edges <- edges[!duplicated(edges$vertice_id), ]
    pts <- data.frame(
        x0 = xy_processed[, 1],
        y0 = xy_processed[, 2],
        stringsAsFactors = FALSE
    )
    pts$vertice_id <- verts[matched_indices,"id"]
    pts <- merge(pts, verts, by.x="vertice_id", by.y="id")
    pts <- merge(pts, edges, by="vertice_id")
    pts$d <- geodist::geodist(x = data.frame(x=pts$x0, y=pts$y0), y = data.frame(x=pts$x,y=pts$y), paired = TRUE, measure = measure)
    stopifnot(nrow(pts) == nrow(xy_processed))
    # Create point IDs - use custom IDs if provided, otherwise generate with genhash
    pt_ids <- if (has_id_col) {
        point_ids
    } else {
        sapply(seq_len(nrow(pts)), function(i) paste0("pt_", genhash()))
    }
    
    # Process edges using the index structure (following add_nodes_to_graph pattern)
    pts$n <- seq_len(nrow(pts))
    pts$pt_id <- pt_ids
    cli::cli_inform("Creating new edges...") 
    # Create new edges based on the index
    ##new_edges <- purrr::pmap(pts, create_edge, .progress = "text")
    new_edges <- furrr::future_pmap(
        .l = pts,
        .f = create_edge,
        .progress = TRUE,
        .options = furrr::furrr_options(
            globals = list(
                graph_std = graph_std,
                wp = wp,
                gr_cols = gr_cols,
                highway = highway,
                bidirectional = bidirectional,
                wt_profile = wt_profile,
                wt_profile_file = wt_profile_file,
                way_wt = way_wt
            )
        )
    )
    
    new_edges_df <- purrr::list_rbind(new_edges)
    # Following add_nodes_to_graph pattern: match back to original structure
    graph_to_add <- graph[new_edges_df$index, ]
    gr_cols <- gr_cols[which(!is.na(gr_cols))]
    
    # Only update columns that exist in new_edges_df
    available_cols <- names(gr_cols)[names(gr_cols) %in% names(new_edges_df)]
    gr_cols_subset <- gr_cols[available_cols]
    
    for (g in seq_along(gr_cols_subset)) {
        graph_to_add[, gr_cols_subset[g]] <- new_edges_df[[names(gr_cols_subset)[g]]]
    }
    
    # Set highway column to user-specified highway type for new edges
    if ("highway" %in% names(graph_to_add)) {
        graph_to_add$highway <- highway
    }
    
    return(rbind(graph, graph_to_add))
}


#' Helper function from add-nodes-to-graph-multi.R
#' Robust version of calc_edge_time that works by column name
#' @noRd
calc_edge_time_by_name <- function(edge, wt_profile) {
    # Get the column names for time and time_weighted
    time_col <- names(edge)[grepl("^time$", names(edge), ignore.case = TRUE)]
    time_weighted_col <- names(edge)[grepl("^time_weighted$", names(edge), ignore.case = TRUE)]
    
    if (length(time_col) == 0 || length(time_weighted_col) == 0) {
        return(edge)
    }
    
    # Use calc_edge_time if available, otherwise calculate manually
    if (exists("calc_edge_time", mode = "function")) {
        return(calc_edge_time(edge, wt_profile))
    } else {
        # Fallback calculation
        if ("maxspeed" %in% names(edge) && !is.na(edge$maxspeed) && edge$maxspeed > 0) {
            speed <- edge$maxspeed # km/h
        } else {
            # Default speeds by profile
            default_speeds <- list(
                bicycle = 15,
                foot = 5,
                motorcar = 50
            )
            speed <- default_speeds[[wt_profile]] %||% 15
        }
        
        # Convert to m/s and calculate time in seconds
        speed_ms <- speed * 1000 / 3600
        edge[[time_col]] <- edge$d / speed_ms
        edge[[time_weighted_col]] <- edge[[time_col]]
        
        return(edge)
    }
}


create_edge <- function(vertice_id, x0,y0,x,y,n,edge_id,index,d,pt_id,...) {
    # Create edges for this point (to and optionally from)
    new_edges_for_point <- list()
    # Edge from point to vertex
    new_edge_to <- graph_std[index,]%>%
        dplyr::mutate(edge_id = paste0(pt_id, "_to_", vertice_id),
               from = pt_id,
               to = vertice_id,
               xfr = x0,
               yfr = y0,
               xto = x,
               yto = y,
               d = d,
               n = n,
               index = index
        )
    # Calculate weights and times
    if (!is.null(wp)) {
        # Use weight profile calculations with the specified highway type
        new_edge_to$d_weighted <- d / way_wt
        
        # Create temporary edge with highway for weight calculations
        temp_edge <- new_edge_to
        temp_edge$highway <- highway
        temp_edge <- set_maxspeed(temp_edge, wt_profile, wt_profile_file)
        temp_edge <- weight_by_num_lanes(temp_edge, wt_profile)
        
        # Copy calculated values back (excluding highway column)
        if ("d_weighted" %in% names(temp_edge)) {
            new_edge_to$d_weighted <- temp_edge$d_weighted
        }
        
        if (!is.na(gr_cols["time"])) {
            temp_edge <- calc_edge_time(temp_edge, wt_profile)
            if ("time" %in% names(temp_edge)) {
                new_edge_to$time <- temp_edge$time
            }
            if ("time_weighted" %in% names(temp_edge)) {
                new_edge_to$time_weighted <- temp_edge$time_weighted
            }
        }
    } else {
        # Inherit properties from matched edge
        matched_edge <- graph_std[index, ]  # Use first edge from index
        if (!is.na(gr_cols["d_weighted"]) ) {
            weight_ratio <- matched_edge$d_weighted / matched_edge$d
            new_edge_to$d_weighted <- d * weight_ratio
        } else {
            new_edge_to$d_weighted <- d
        }
        if (!is.na(gr_cols["time"])) {
            time_per_distance <- matched_edge$time / matched_edge$d
            new_edge_to$time <- d * time_per_distance
            
            if (!is.na(gr_cols["time_weighted"])) {
                time_weight_ratio <- matched_edge$time_weighted / matched_edge$time
                new_edge_to$time_weighted <- new_edge_to$time * time_weight_ratio
            }
        }
    }
    
    new_edges_for_point[[1]] <- new_edge_to
    
    # Add reverse edge if bidirectional
    if (bidirectional) {
        new_edge_from <- new_edge_to
        new_edge_from <- dplyr::mutate(new_edge_from,
                                       edge_id=paste0(vertice_id, "_to_", pt_id),
                                       to = pt_id,
                                       from  = vertice_id,
                                       xfr = x,
                                       yfr = y,
                                       xto = x0,
                                       yto = y0,
                                       d = d)
        new_edges_for_point[[2]] <- new_edge_from
    }
    do.call(rbind, new_edges_for_point)
}
