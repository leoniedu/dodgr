# Miscellaneous **non-exported** graph functions

#' Get graph columns containing the from vertex
#' "vx0" is silicate vertex
#' @noRd
find_fr_col <- function (graph) {

    cols <- grep ("^fr|fr$|^sta|.vx0", names (graph), ignore.case = TRUE)
    # Remove any "z" columns with altitude values:
    cols [which (!grepl ("\\_z$", names (graph) [cols], ignore.case = TRUE))]
}

#' Get graph columns containing the to vertex
#' "vx1" is silicate vertex
#' @noRd
find_to_col <- function (graph) {

    cols <- grep ("^to|to$|^sto|.vx1", names (graph), ignore.case = TRUE)
    cols [which (!grepl ("\\_z$", names (graph) [cols], ignore.case = TRUE))]
}

#' Get single graph column containing the ID of the from vertex
#' @noRd
find_fr_id_col <- function (graph) {

    fr_col <- find_fr_col (graph)
    if (is_graph_spatial (graph)) {
        frx_col <- find_xy_col (graph, fr_col, x = TRUE)
        fry_col <- find_xy_col (graph, fr_col, x = FALSE)
        fr_col <- fr_col [which (!fr_col %in%
            c (frx_col, fry_col))]
    }

    if (length (fr_col) != 1) {
        fr_col <- fr_col [grep ("id", names (graph) [fr_col])] # nolint
        if (length (fr_col) != 1) {
            stop ("Unable to determine column with ID of from vertices")
        }
    }

    return (fr_col)
}

#' Get single graph column containing the ID of the to vertex
#' @noRd
find_to_id_col <- function (graph) {

    to_col <- find_to_col (graph)
    if (is_graph_spatial (graph)) {
        tox_col <- find_xy_col (graph, to_col, x = TRUE)
        toy_col <- find_xy_col (graph, to_col, x = FALSE)
        to_col <- to_col [which (!to_col %in%
            c (tox_col, toy_col))]
    }

    if (length (to_col) != 1) {
        to_col <- to_col [grep ("id|vx", names (graph) [to_col])] # nolint
        if (length (to_col) != 1) {
            stop ("Unable to determine column with ID of to vertices")
        }
    }

    return (to_col)
}

#' find_xy_col
#'
#' Find columns in graph containing lon and lat coordinates
#' @param indx columns of graph containing either to or from values, so xy
#' columns can be returned separately for each case
#' @noRd
find_xy_col <- function (graph, indx, x = TRUE) {

    if (x) {
        coli <- grep ("x|lon", names (graph) [indx], ignore.case = TRUE)
        if (length (coli) > 1) { # silicate has $.vx0, $.vx1
            coli <- grep ("x$", names (graph) [indx], ignore.case = TRUE)
        }
    } else {
        coli <- grep ("y|lat", names (graph) [indx], ignore.case = TRUE)
        if (length (coli) > 1) { # silicate only matches once here, so nocov:
            coli <- grep ("y$", names (graph) [indx], # nocov
                ignore.case = TRUE
            )
        } # nocov
    }

    indx [coli]
}

find_d_col <- function (graph) {

    d_col <- which (
        tolower (substring (names (graph), 1, 1)) == "d" &
            tolower (substring (names (graph), 1, 2)) != "dz" &
            tolower (substring (names (graph), 2, 2)) != "w" &
            tolower (substring (names (graph), 2, 2)) != "_"
    )
    if (length (d_col) != 1) {
        d_nms <- names (graph) [d_col]
        d_col <- d_col [which (nchar (d_nms) == 1 |
            tolower (substring (d_nms, 1L, 4L)) == "dist")]
    }

    if (length (d_col) != 1) {
        stop ("Unable to determine distance column in graph")
    }

    return (d_col)
}

find_w_col <- function (graph) {

    w_col <- match (c ("w", "wt"), names (graph))
    if (all (is.na (w_col)) || length (w_col) != 1) {
        w_col <- grep ("weight", names (graph))
    }
    if (length (w_col) != 1) {
        w_col <- which (tolower (substring (names (graph), 1, 2)) == "dw" |
            tolower (substring (names (graph), 1, 3)) == "d_w")
    }

    if (length (w_col) > 1) {
        stop ("Unable to determine weight column in graph")
    }

    return (w_col)
}

#' find_xy_col_simple
#'
#' Find the x and y cols of a simple data.frame of verts of xy points (used only
#' in match_pts_to_verts).
#' @param dfr Either the result of `dodgr_vertices`, or a `data.frame`
#' or equivalent structure (matrix, \pkg{tibble}) of spatial points.
#' @return Vector of two values of location of x and y columns
#' @noRd
find_xy_col_simple <- function (dfr) {

    nms <- names (dfr)
    if (is.null (nms)) {
        nms <- colnames (dfr)
    }

    ix <- iy <- NULL
    if (!is.null (nms)) {
        ix <- which (grepl ("x", nms, ignore.case = TRUE) |
            grepl ("lon", nms, ignore.case = TRUE))
        iy <- which (grepl ("y", nms, ignore.case = TRUE) |
            grepl ("lat", nms, ignore.case = TRUE))
    }

    if (length (ix) == 0 || length (iy) == 0) {
        message ("xy has no named columns; assuming order is x then y")
        ix <- 1
        iy <- 2
    }

    return (c (ix, iy))
}

# vertices randomly selected from a graph without turn penalties may be
# submitted to functions along with the corresponding graph with turn angles.
# The latter version appends vertex IDs with "_start" and "_end" for the starts
# and ends of compound turn angle junctions. This function finds any instances
# of `pts` that map on to these, and appends the appropriate suffix so these
# points can be used in routines with the turn-penalty graph.
remap_verts_with_turn_penalty <- function (graph, pts, from = TRUE) {

    if (!methods::is (graph, "dodgr_streetnet_sc")) {
        stop (
            "vertices with turn angles can only be re-mapped for ", # nocov
            "street networks obtained via 'dodgr_streetnet_sc' -> ", # nocov
            "'weight_streetnet'"
        )
    } # nocov

    suffix <- ifelse (from, "_start", "_end")
    suffix_rgx <- paste0 (suffix, "$")
    vcol <- ifelse (from, ".vx0", ".vx1")

    index <- grep (suffix_rgx, graph [[vcol]])
    all_pts <- gsub (suffix_rgx, "", graph [[vcol]] [index])
    pts [pts %in% all_pts] <- paste0 (pts [pts %in% all_pts], suffix)

    return (pts)
}

#' Get single graph column containing the edge ID
#' @noRd
find_edge_id_col <- function (graph) {
    # First try exact matches
    edge_id_col <- grep ("^edge\\_id$", names (graph), ignore.case = TRUE)
    # If no exact match, try broader patterns
    if (length (edge_id_col) != 1) {
        # Look for columns ending in edge_id or edge
        edge_id_col <- grep ("^edge\\_$|edge$", names (graph), ignore.case = TRUE)
        if (length (edge_id_col) > 1) {
            stop ("Unable to determine unique column for edge IDs")
        } else if (length (edge_id_col) == 0) {
            edge_id_col <- NA_integer_
        }
    }
    return (edge_id_col)
}

#' Get single graph column containing component IDs
#' @noRd
find_component_col <- function (graph) {
    # First try exact match
    comp_col <- grep ("^component$", names (graph), ignore.case = TRUE)
    # If no exact match, try broader pattern
    if (length (comp_col) != 1) {
        comp_col <- grep ("comp", names (graph), ignore.case = TRUE)
        if (length (comp_col) > 1) {
            stop ("Unable to determine unique column for components")
        } else if (length (comp_col) == 0) {
            comp_col <- NA_integer_
        }
    }
    return (comp_col)
}
