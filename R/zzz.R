# nocov start
#match_points_to_graph_original <- match_points_to_graph
#match_pts_to_verts_original <- match_pts_to_verts

.onLoad <- function (libname, pkgname) { # nolint

    Sys.setenv ("RCPP_PARALLEL_BACKEND" = "tinythread")
    # Assign cached point-matching functions globally using memoise idiom
    match_pts_to_verts <<- memoise::memoise(match_pts_to_verts)
    match_points_to_graph <<- memoise::memoise(match_points_to_graph)
    invisible ()
}
# nocov end
