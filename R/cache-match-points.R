#' Caching wrappers for point-to-graph and point-to-vertex matching
#'
#' These functions cache the results of match_pts_to_verts and match_points_to_graph
#' using hashed filenames and RDS files in the temp directory, following the dodgr cache conventions.
#'
#' @noRd
match_pts_to_verts_cached <- function(verts, xy, connected = FALSE, force = FALSE) {
    # Use verts hash if present
    verts_hash <- attr(verts, "hash")
    args_for_hash <- list(verts_hash, xy, connected)
    hash <- digest::digest(args_for_hash)
    cache_file <- fs::path(fs::path_temp(), paste0("dodgr_match_verts_", hash, ".Rds"))
    if (is_dodgr_cache_on() && fs::file_exists(cache_file) && !force) {
        return(readRDS(cache_file))
    }
    result <- match_pts_to_verts_original(verts, xy, connected)
    if (is_dodgr_cache_on()) {
        saveRDS(result, cache_file)
    }
    return(result)

}

#' Cached version of match_points_to_graph
#' @noRd
match_points_to_graph_cached <- function(graph, xy, connected = FALSE, distances = FALSE, force = FALSE) {
    graph_hash <- attr(graph, "hash")
    args_for_hash <- list(graph_hash, xy, connected, distances)
    hash <- digest::digest(args_for_hash)
    cache_file <- fs::path(fs::path_temp(), paste0("dodgr_match_graph_", hash, ".Rds"))
    if (is_dodgr_cache_on() && fs::file_exists(cache_file) && !force) {
        return(readRDS(cache_file))
    }
    result <- match_points_to_graph_original(graph, xy, connected, distances)
    if (is_dodgr_cache_on()) {
        saveRDS(result, cache_file)
    }
    return(result)
}
