#' Add edges between specified points and closest graph vertices
#'
#' Creates new edges connecting specified point coordinates to their closest
#' vertices in the graph, as determined by \code{match_pts_to_verts}.
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
#' and \code{time_weighted}.
#' @param wt_profile Weight profile for the specified mode of transport (see
#' \link{weight_streetnet}).
#' @param wt_profile_file Custom profile file for weights (see
#' \link{weight_streetnet}).
#' @param bidirectional If \code{TRUE} (default), creates bidirectional edges
#' (to and from each point). If \code{FALSE}, creates only unidirectional edges
#' from points to vertices.
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
                              wt_profile = "bicycle",
                              wt_profile_file = NULL,
                              bidirectional = TRUE) {

    # Validate inputs
    if (missing(highway)) {
        stop("'highway' argument is required")
    }
    
    # Convert to data.frame if needed and get graph columns
    graph_t <- tbl_to_df(graph)
    gr_cols <- dodgr_graph_cols(graph_t)
    
    # Check for id column in xy input before processing
    has_id_col <- FALSE
    point_ids <- NULL
    xy_for_processing <- xy
    if (is.data.frame(xy) && "id" %in% names(xy)) {
        has_id_col <- TRUE
        point_ids <- as.character(xy$id)
        # Remove id column for coordinate processing
        xy_for_processing <- xy[, !names(xy) %in% "id", drop = FALSE]
    }
    
    # Get vertices and match points to them
    verts <- dodgr_vertices(graph_t)
    matched_indices <- match_pts_to_verts(verts, xy_for_processing)
    
    # Pre-process xy coordinates
    xy_processed <- pre_process_xy(xy_for_processing)
    if (nrow(xy_processed) != length(matched_indices)) {
        stop("Mismatch between number of points and matched vertices")
    }
    
    # Generate unique IDs for the new point vertices
    charvec <- c(letters, LETTERS, 0:9)
    randid <- function(len = 10) {
        paste0(sample(charvec, len, replace = TRUE), collapse = "")
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
    
    # Create new edges
    new_edges <- list()
    
    for (i in seq_len(nrow(xy_processed))) {
        matched_vertex <- verts[matched_indices[i], ]
        pt_x <- xy_processed[i, 1]
        pt_y <- xy_processed[i, 2]
        pt_id <- if (has_id_col) point_ids[i] else paste0("pt_", randid())
        
        # Calculate distance between point and matched vertex
        if (is_graph_spatial(graph_t)) {
            measure <- get_geodist_measure(graph_t)
            d <- geodist::geodist(
                data.frame(x = c(pt_x, matched_vertex$x), 
                          y = c(pt_y, matched_vertex$y)),
                measure = measure
            )[1, 2]
        } else {
            d <- sqrt((pt_x - matched_vertex$x)^2 + (pt_y - matched_vertex$y)^2)
        }
        
        # Create base edge template
        edge_template <- data.frame(
            edge_id = character(1),
            from = character(1),
            to = character(1),
            xfr = numeric(1),
            yfr = numeric(1),
            xto = numeric(1),
            yto = numeric(1),
            d = d,
            d_weighted = numeric(1),
            highway = highway,
            stringsAsFactors = FALSE
        )
        
        # Add time columns if they exist in the graph
        if (!is.na(gr_cols$time)) {
            edge_template$time <- numeric(1)
            edge_template$time_weighted <- numeric(1)
        }
        
        # Create edge from point to vertex
        edge_pt_to_vert <- edge_template
        edge_pt_to_vert$edge_id <- paste0("pt_edge_", i, "_to_vert")
        edge_pt_to_vert$from <- pt_id
        edge_pt_to_vert$to <- matched_vertex$id
        edge_pt_to_vert$xfr <- pt_x
        edge_pt_to_vert$yfr <- pt_y
        edge_pt_to_vert$xto <- matched_vertex$x
        edge_pt_to_vert$yto <- matched_vertex$y
        
        # Calculate weights and times
        if (!is.null(wp)) {
            edge_pt_to_vert$d_weighted <- d / way_wt
            
            # Apply weight profile calculations
            edge_pt_to_vert <- set_maxspeed(edge_pt_to_vert, wt_profile, wt_profile_file)
            edge_pt_to_vert <- weight_by_num_lanes(edge_pt_to_vert, wt_profile)
            
            if (!is.na(gr_cols$time)) {
                edge_pt_to_vert <- calc_edge_time_by_name(edge_pt_to_vert, wt_profile)
            }
        } else {
            edge_pt_to_vert$d_weighted <- d
            if (!is.na(gr_cols$time)) {
                # Use default speed if no profile specified (assume 15 km/h for bicycle)
                default_speed <- 15 # km/h
                edge_pt_to_vert$time <- d / (default_speed * 1000 / 3600) # convert to seconds
                edge_pt_to_vert$time_weighted <- edge_pt_to_vert$time
            }
        }
        
        new_edges[[length(new_edges) + 1]] <- edge_pt_to_vert
        
        # Create bidirectional edge if requested
        if (bidirectional) {
            edge_vert_to_pt <- edge_template
            edge_vert_to_pt$edge_id <- paste0("pt_edge_", i, "_from_vert")
            edge_vert_to_pt$from <- matched_vertex$id
            edge_vert_to_pt$to <- pt_id
            edge_vert_to_pt$xfr <- matched_vertex$x
            edge_vert_to_pt$yfr <- matched_vertex$y
            edge_vert_to_pt$xto <- pt_x
            edge_vert_to_pt$yto <- pt_y
            
            # Apply same weight calculations
            if (!is.null(wp)) {
                edge_vert_to_pt$d_weighted <- d / way_wt
                edge_vert_to_pt <- set_maxspeed(edge_vert_to_pt, wt_profile, wt_profile_file)
                edge_vert_to_pt <- weight_by_num_lanes(edge_vert_to_pt, wt_profile)
                
                if (!is.na(gr_cols$time)) {
                    edge_vert_to_pt <- calc_edge_time_by_name(edge_vert_to_pt, wt_profile)
                }
            } else {
                edge_vert_to_pt$d_weighted <- d
                if (!is.na(gr_cols$time)) {
                    default_speed <- 15 # km/h
                    edge_vert_to_pt$time <- d / (default_speed * 1000 / 3600)
                    edge_vert_to_pt$time_weighted <- edge_vert_to_pt$time
                }
            }
            
            new_edges[[length(new_edges) + 1]] <- edge_vert_to_pt
        }
    }
    
    # Combine new edges with original graph
    if (length(new_edges) > 0) {
        new_edges_df <- do.call(rbind, new_edges)
        
        # Ensure all columns match between original graph and new edges
        missing_cols_in_new <- setdiff(names(graph_t), names(new_edges_df))
        missing_cols_in_orig <- setdiff(names(new_edges_df), names(graph_t))
        
        # Add missing columns to new edges with appropriate default values
        for (col in missing_cols_in_new) {
            if (is.character(graph_t[[col]])) {
                new_edges_df[[col]] <- NA_character_
            } else if (is.numeric(graph_t[[col]])) {
                new_edges_df[[col]] <- NA_real_
            } else if (is.logical(graph_t[[col]])) {
                new_edges_df[[col]] <- NA
            } else {
                new_edges_df[[col]] <- NA
            }
        }
        
        # Add missing columns to original graph if needed
        for (col in missing_cols_in_orig) {
            if (is.character(new_edges_df[[col]])) {
                graph_t[[col]] <- NA_character_
            } else if (is.numeric(new_edges_df[[col]])) {
                graph_t[[col]] <- NA_real_
            } else if (is.logical(new_edges_df[[col]])) {
                graph_t[[col]] <- NA
            } else {
                graph_t[[col]] <- NA
            }
        }
        
        # Reorder columns to match
        new_edges_df <- new_edges_df[, names(graph_t)]
        
        # Combine graphs
        result_graph <- rbind(graph_t, new_edges_df)
        
        # Update hash
        attr(result_graph, "hash") <- get_hash(result_graph, contracted = FALSE, force = TRUE)
        
        return(result_graph)
    } else {
        return(graph_t)
    }
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
