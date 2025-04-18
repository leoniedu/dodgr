# definitions used in weight_streetnet.sc, including functions for time-based
# network weighting.

has_elevation <- function (x) {

    "z_" %in% names (x$vertex)
}

check_sc <- function (x) {

    if (!"osmdata_sc" %in% class (x)) {
        stop (
            "weight_streetnet currently only works for 'sc'-class objects ",
            "extracted with osmdata::osmdata_sc."
        )
    }
}

# First step of edge extraction: join x and y coordinates
extract_sc_edges_xy <- function (x) {

    rename0 <- c (.vx0_x = "x_", .vx0_y = "y_", .vx0_z = "z_")
    rename1 <- c (.vx1_x = "x_", .vx1_y = "y_", .vx1_z = "z_")
    if (!has_elevation (x)) {
        rename0 <- rename0 [1:2]
        rename1 <- rename1 [1:2]
    }

    dplyr::left_join (
        x$edge, x$vertex,
        by = c (".vx0" = "vertex_"),
        multiple = "all"
    ) %>%
        dplyr::rename (!!rename0) %>%
        dplyr::left_join (
            x$vertex,
            by = c (".vx1" = "vertex_"),
            multiple = "all"
        ) %>%
        dplyr::rename (!!rename1)
}

sc_edge_dist <- function (graph) {

    # no visible binding notes:
    .vx0_z <- .vx1_z <- NULL

    # Get geodist measure, noting that graph has no hash at this stage, so full
    # calculation will be executed.
    measure <- get_geodist_measure (graph)

    xy0 <- as.data.frame (graph [, c (".vx0_x", ".vx0_y")])
    xy1 <- as.data.frame (graph [, c (".vx1_x", ".vx1_y")])
    graph$d <- geodist::geodist (xy0, xy1, paired = TRUE, measure = measure)
    if (".vx0_z" %in% names (graph) && ".vx1_z" %in% names (graph)) {
        graph <- dplyr::mutate (graph, "dz" = .vx1_z - .vx0_z) %>%
            dplyr::select (-c (.vx0_z, .vx1_z))
    }
    return (graph)
}

extract_sc_edges_highways <- function (graph, x, wt_profile, wt_profile_file,
                                       keep_cols) {

    # no visible binding notes:
    native_ <- key <- `:=` <- value <- NULL # nolint

    surface <- get_surface_speeds (wt_profile, wt_profile_file)
    if (nrow (surface) > 0) {
        keep_cols <- c (keep_cols, unique (surface$key))
    }

    if (length (keep_cols) > 0L) {

        keys <- unique (x$object$key)
        keep_names <- unique (keep_cols [which (keep_cols %in% keys)])
    }

    graph <- dplyr::left_join (graph, x$object_link_edge, by = "edge_") %>%
        dplyr::select (-native_)
    for (k in keep_names) {
        objs <- dplyr::filter (x$object, key == k)
        graph <- dplyr::left_join (graph, objs, by = "object_") %>%
            dplyr::rename (!!dplyr::quo_name (k) := value) %>%
            dplyr::select (-key)
    }

    convert_hw_types_to_bool (graph, wt_profile)
}

convert_hw_types_to_bool <- function (graph, wt_profile) {

    if (!"oneway" %in% names (graph)) {
        return (graph)
    }
    if (is.logical (graph$oneway)) {
        return (graph)
    }

    if (!(is.character (wt_profile) || is.data.frame (wt_profile))) {
        return (graph)
    }

    if (!is.character (wt_profile)) {
        wt_profile <- unique (wt_profile$name)
    }

    bikeflags <- grep ("oneway.*bicycle|bicycle.*oneway", names (graph))
    if ("oneway" %in% names (graph) ||
        (length (bikeflags) == 1 && wt_profile == "bicycle")) {

        graph <- set_oneway_bike_flags (graph, bikeflags, wt_profile)
    }

    return (graph)
}

set_oneway_bike_flags <- function (graph, bikeflags, wt_profile) {

    index <- which (!graph$oneway %in% c ("no", "yes"))
    if (length (index) > 0) {
        graph$oneway [index] <- "no"
    }
    graph$oneway <- ifelse (graph$oneway == "no", FALSE, TRUE)

    if (length (bikeflags) == 1) {
        # oneway:bicycle doesn't enquote properly, so:
        names (graph) [bikeflags] <- "oneway_bicycle"

        index <- which (!graph$oneway_bicycle %in% c ("no", "yes"))
        if (length (index) > 0) {
            graph$oneway_bicycle [index] <- "no"
        }
        graph$oneway_bicycle <-
            ifelse (graph$oneway_bicycle == "no", FALSE, TRUE)

        if (wt_profile == "bicycle") {
            graph$oneway <- graph$oneway_bicycle
            graph$oneway_bicycle <- NULL
        }
    }

    return (graph)
}

weight_sc_edges <- function (graph, wt_profile, wt_profile_file) {

    # no visible binding notes:
    value <- d <- d_weighted <- NULL

    wp <- get_profile (wt_profile, wt_profile_file)
    wp <- wp [, c ("way", "value")]

    res <- dplyr::left_join (graph, wp, by = c ("highway" = "way")) %>%
        dplyr::filter (!is.na (value)) %>%
        dplyr::mutate (d_weighted = ifelse (value == 0, NA, d / value)) %>%
        dplyr::filter (!is.na (d_weighted)) %>%
        dplyr::select (-value)

    if (wt_profile %in% c ("foot", "bicycle")) {
        index <- which (res [[wt_profile]] == "no")
        if (length (index) > 0L) {
            res <- res [-index, ]
        }
        # Plus remove any untagged "motorway" or "trunk" edges
        index <- grep ("^(motorway|trunk)", res$highway)
        if (length (index) > 0L) {
            res <- res [-index, ]
        }
    }

    return (res)
}

# Set maximum speed for each edge.
set_maxspeed <- function (graph, wt_profile, wt_profile_file) {

    if (!"maxspeed" %in% names (graph)) {
        graph$maxspeed <- NA_real_
    } # nocov
    if (!"highway" %in% names (graph)) {
        return (graph)
    } # nocov

    maxspeed <- rep (NA_real_, nrow (graph))
    index <- grep ("mph", graph$maxspeed)
    maxspeed [index] <- as.numeric (gsub (
        "[^[:digit:]. ]", "",
        graph$maxspeed [index]
    ))
    maxspeed [index] <- maxspeed [index] * 1.609344

    index <- seq_len (nrow (graph)) [!(seq_len (nrow (graph)) %in% index)]
    maxspeed_char <- graph$maxspeed [index] # character string
    # some maxspeeds have two values, where the 1st is generally the "default"
    # value. This gsub extracts those only:
    maxspeed_char <- gsub ("[[:punct:]].*$", "", maxspeed_char)
    # some (mostly Austria and Germany) have "maxspeed:walk" for living streets.
    # This has no numeric value, but is replaced here with 10km/h
    maxspeed_char <- gsub ("walk", "10", maxspeed_char)
    maxspeed_char <- gsub ("none", NA, maxspeed_char)
    index2 <- which (!(is.na (maxspeed_char) |
        maxspeed_char == "" |
        maxspeed_char == "NA"))
    index2 <- index2 [which (!grepl ("[[:alpha:]]", maxspeed_char [index2]))]

    maxspeed_numeric <- rep (NA_real_, length (index))
    maxspeed_numeric [index2] <- as.numeric (maxspeed_char [index2])
    maxspeed [index] <- maxspeed_numeric

    graph$maxspeed <- maxspeed
    # Those are the OSM values. Edges which do not specify maxspeed values are
    # then allocated the values defined by the specified profile, except for
    # "motorcar" profiles, which replace maxspeeds with median values for each
    # way type.
    if (wt_profile == "motorcar") {

        med_speeds <- vapply (
            unique (graph$highway), function (h) {
                stats::median (graph$maxspeed [graph$highway == h],
                    na.rm = TRUE
                )
            },
            numeric (1L)
        )
        wp_index <- match (graph$highway, names (med_speeds))
        index <- which (is.na (graph$maxspeed))
        graph$maxspeed [index] <- med_speeds [wp_index [index]]

    }

    # Then fill any NA maxspeed values from weight profile
    wp <- get_profile (wt_profile, wt_profile_file)

    wp_index <- match (graph$highway, wp$way)
    graph_index <- which (!is.na (wp_index))
    wp_index <- wp_index [graph_index]
    maxspeed <- cbind (graph$maxspeed, rep (NA, nrow (graph)))
    maxspeed [graph_index, 2] <- wp$max_speed [wp_index]

    if (wt_profile == "motorcar") {
        index <- which (is.na (maxspeed [, 1]))
        graph$maxspeed [index] <- maxspeed [index, 2]
    } else {
        # choose minimal maxspeed value
        graph$maxspeed <- apply (maxspeed, 1, function (i) {
            ifelse (all (is.na (i)),
                NA_real_,
                min (i, na.rm = TRUE)
            )
        })
    }

    na_highways <- wp$way [which (is.na (wp$max_speed))]
    graph$maxspeed [graph$highway %in% na_highways] <- NA_real_
    # Also set weighted distance for all these to NA:
    # gr_cols <- dodgr_graph_cols (graph)
    # graph [[gr_cols$d_weighted]] [graph$highway %in% na_highways] <- NA_real_

    if (wt_profile %in% c ("horse", "wheelchair") ||
        !"surface" %in% names (graph)) {
        return (graph)
    }

    # And then repeat for max speeds according to surface profiles
    if (wt_profile != "motorcar") {
        s <- get_surface_speeds (wt_profile, wt_profile_file)
        s <- s [s$name == wt_profile, c ("key", "value", "max_speed")]
        surf_vals <- unique (graph$surface [graph$surface != "NA"])
        surf_speeds <- s$max_speed [match (surf_vals, s$value)]
        surf_vals <- surf_vals [!is.na (surf_speeds)]
        surf_speeds <- surf_speeds [!is.na (surf_speeds)]

        surf_index <- match (graph$surface, surf_vals)
        graph_index <- which (!is.na (surf_index))
        surf_index <- surf_index [graph_index]
        maxspeed <- cbind (
            as.numeric (graph$maxspeed),
            rep (NA_real_, nrow (graph))
        )
        maxspeed [graph_index, 2] <- surf_speeds [surf_index]
        graph$maxspeed <- apply (maxspeed, 1, function (i) {
            ifelse (all (is.na (i)),
                NA_real_,
                min (i, na.rm = TRUE)
            )
        })
    }

    graph$surface <- NULL

    return (graph)
}

# adjust weighted distances according to numbers of lanes
weight_by_num_lanes <- function (graph, wt_profile) {

    # only weight these profiles:
    profile_names <- c ("foot", "bicycle", "wheelchair", "horse")
    if (!(wt_profile %in% profile_names && "lanes" %in% names (graph))) {
        return (graph)
    } # nocov

    lns <- c (4, 5, 6, 7, 8)
    wts <- c (0.05, 0.05, 0.1, 0.1, 0.2)
    for (i in seq (lns)) {
        index <- which (graph$lanes == lns [i])
        if (i == length (lns)) {
            index <- which (graph$lanes >= lns [i])
        }
        graph$d_weighted [index] <- graph$d_weighted [index] * (1 + wts [i])
    }

    graph$lanes <- NULL

    return (graph)
}

# Convert distances in metres to time in seconds. Up to this point, distances
# have been weighted for type of way (via
# weighting_profiles$weighting_profiles), and there is a maxspeed column
# reflecting profile values plus effect of different surfaces.
# The time is distance scaled by maxspeed, and time_weighted is d_weighted
# scaled by maxspeed
calc_edge_time <- function (graph, wt_profile) {

    gr_cols <- dodgr_graph_cols (graph)
    speed_m_per_s <- graph$maxspeed * 1000 / 3600 # maxspeeds are km/hr
    graph$time <- graph [[gr_cols$d]] / speed_m_per_s
    graph$time_weighted <- graph [[gr_cols$d_weighted]] / speed_m_per_s

    if ("dz" %in% names (graph) &&
        wt_profile %in% c ("foot", "bicycle")) {
        graph <- times_by_incline (graph, wt_profile)
    }
    graph$maxspeed <- NULL

    return (graph)
}

# increase both real and weighted times according to elevation increases:
times_by_incline <- function (graph, wt_profile) {

    cost_tobler <- function (dz, cost0) {
        cost <- 1 / (6 * exp (-3.5 * abs (dz + 0.05)))
        cost / cost0
    }

    if (wt_profile == "foot") {
        # Used to just be
        # [Naismith's Rule](https://en.wikipedia.org/wiki/Naismith%27s_rule)
        # time <- time + dz / 10
        # but updated to Tobler's hiking rule; see issue #124
        if ("dz" %in% names (graph)) {
            cost0 <- 1 / (6 * exp (-3.5 * 0.05))
            graph$time <- graph$time * cost_tobler (graph$dz / graph$d, cost0)
        }

    } else if (wt_profile == "bicycle") {
        # http://theclimbingcyclist.com/gradients-and-cycling-how-much-harder-are-steeper-climbs/ # nolint
        # http://cycleseven.org/effect-of-hills-on-cycling-effort
        # The latter argues for a linear relationship with a reduction in speed
        # of "about 11% for every 1% change in steepness". For 0.01 to translate
        # to 0.11, it needs to be multiplied by 0.11 / 0.01, or 11
        if ("dz" %in% names (graph)) {
            index <- which (graph$dz > 0)
            graph$time [index] <- graph$time [index] *
                (1 + 11 * graph$dz [index] / graph$d [index])
            graph$time_weighted [index] <- graph$time_weighted [index] *
                (1 + 11 * graph$dz [index] / graph$d [index])
        }
        # ... TODO: Downhill
        # http://www.sportsci.org/jour/9804/dps.html
        # downhill cycling speed ~ sqrt (slope)
    }
    return (graph)
}

sc_traffic_lights <- function (graph, x, wt_profile, wt_profile_file) {

    # no visible binding NOTES:
    object_ <- NULL

    wait <- get_turn_penalties (wt_profile, wt_profile_file)$traffic_lights
    if (length (wait) == 0) wait <- 0

    # first for intersections marked as crossings
    crossings <- traffic_light_objs (x) # way IDs
    objs <- x$object %>% dplyr::filter (object_ %in% crossings$crossings)
    oles <- x$object_link_edge %>% dplyr::filter (object_ %in% objs$object_)
    # Then the actual nodes with the traffic lights
    nodes <- traffic_signal_nodes (x)
    # Increment waiting times for edges ending at those nodes
    index <- which (graph$edge_ %in% oles$edge_ &
        graph$.vx1 %in% nodes)
    graph$time [index] <- graph$time [index] + wait

    # then all others with nodes simply marked as traffic lights - match
    # those to *start* nodes and simply add the waiting time
    index2 <- which (graph$.vx0 %in% nodes &
        !graph$.vx0 %in% graph$.vx0 [index])
    graph$time [index2] <- graph$time [index2] + wait

    return (graph)
}

rm_duplicated_edges <- function (graph) {

    gr_cols <- dodgr_graph_cols (graph)
    ft <- paste0 (graph [[gr_cols$from]], "-", graph [[gr_cols$to]])
    ft_un <- unique (ft)
    ft_dupl <- ft [which (duplicated (ft))]

    # Get dual indices into ft of duplicate entries:
    i1 <- match (ft_dupl, ft)
    ft_mod <- ft
    ft_mod [i1] <- paste0 (ft_mod [i1], "---")
    i2 <- match (ft_dupl, ft_mod)

    index <- cbind (i1, i2)

    removes <- apply (index, 1, function (i) {
        ifelse (
            graph [[gr_cols$d]] [i [1]] > graph [[gr_cols$d]] [i [2]],
            i [1],
            i [2]
        )
    })
    if (length (removes) > 0L) {
        graph <- graph [-removes, ]
    }
    return (graph)
}

# up to that point, all edges are non-duplicated, and so need to be duplicated
# for non-oneway. Note that strict accordance with oneway flags for "bicycle"
# routing can generate unroutable networks. The following implements the more
# realistic procedure of duplicting oneway edges for bicycle routing, but at
# twice the weighted distance/time values.
sc_duplicate_edges <- function (x, wt_profile) {

    oneway_modes <- c (
        "bicycle", "moped", "motorcycle", "motorcar", "goods",
        "hgv", "psv"
    )

    index <- seq_len (nrow (x))
    if (wt_profile %in% oneway_modes) {
        if ("junction" %in% names (x)) {
            x$oneway [x$junction == "roundabout"] <- TRUE # #175
        }
        index <- which (!x$oneway)
    }

    xnew <- x [index, ]
    xnew <- swap_cols (xnew, ".vx0", ".vx1")
    xnew <- swap_cols (xnew, ".vx0_x", ".vx1_x")
    xnew <- swap_cols (xnew, ".vx0_y", ".vx1_y")
    xnew$edge_ <- rcpp_gen_hash (nrow (xnew), 10)

    if (wt_profile == "bicycle") {
        index <- which (x$oneway)
        xnew2 <- x [index, ]
        xnew2 <- swap_cols (xnew2, ".vx0", ".vx1")
        xnew2 <- swap_cols (xnew2, ".vx0_x", ".vx1_x")
        xnew2 <- swap_cols (xnew2, ".vx0_y", ".vx1_y")
        xnew2$edge_ <- rcpp_gen_hash (nrow (xnew2), 10)
        xnew2$d_weighted <- xnew2$d * 2
        xnew2$time_weighted <- xnew2$time * 2

        xnew <- rbind (xnew, xnew2)
    }

    res <- rbind (x, xnew)
    res$oneway <- NULL

    return (res)
}

swap_cols <- function (x, cola, colb) {

    temp <- x [[cola]]
    x [[cola]] <- x [[colb]]
    x [[colb]] <- temp
    return (x)
}



# traffic lights for pedestrians
# https://wiki.openstreetmap.org/wiki/Tag:highway%3Dtraffic_signals#Complex_intersections # nolint

# return silicate "object" instances -> OSM ways IDs asosicated with given sets
# of key-val pairs
get_key_val_pair <- function (x, kv) {

    # no visible binding notes:
    key <- value <- object_ <- NULL

    xo <- lapply (kv, function (i) {
        dplyr::filter (x$object, key == i [1], value == i [2]) %>%
            dplyr::select (object_) %>%
            dplyr::pull (object_)
    })
    xo <- table (do.call (c, xo))

    res <- NULL
    if (any (xo == length (kv))) {
        res <- names (xo) [which (xo == length (kv))]
    } # nocov - not tested

    return (res)
}

get_key_val_pair_node <- function (x, kv) {

    # no visible binding notes:
    key <- value <- vertex_ <- NULL

    if (is.null (x$nodes)) {
        return (NULL)
    }

    xo <- lapply (kv, function (i) {
        dplyr::filter (x$nodes, key == i [1], value == i [2]) %>%
            dplyr::select (vertex_) %>%
            dplyr::pull (vertex_)
    })
    unique (unlist (xo))
}

# Get all OSM way IDs associated with traffic lights from osmdata_sc object x
traffic_light_objs <- function (x) {

    # 1. Traffic signal without intersection (e.g. before bridge), no pedestrian
    # crossing
    x1 <- get_key_val_pair (
        x,
        list (
            c ("highway", "traffic_signals"),
            c ("crossing", "no")
        )
    )

    # 2. Pedestrian crossing without intersection
    x2a <- get_key_val_pair (
        x,
        list (
            c ("highway", "crossing"),
            c ("crossing", "traffic_signals")
        )
    )
    x2b <- get_key_val_pair (
        x,
        list (
            c ("highway", "traffic_signals"),
            c ("crossing", "traffic_signals")
        )
    )
    x2_forw <- get_key_val_pair (
        x,
        list (
            c ("highway", "traffic_signals"),
            c ("crossing", "no"),
            c ("traffic_signals:direction", "forward")
        )
    )
    x2_back <- get_key_val_pair (
        x,
        list (
            c ("highway", "traffic_signals"),
            c ("crossing", "no"),
            c ("traffic_signals:direction", "backward")
        )
    )

    # 3. Simple Intersection
    x3a <- get_key_val_pair (
        x,
        list (
            c ("highway", "traffic_signals"),
            c ("crossing", "traffic_signals")
        )
    )
    x3b <- get_key_val_pair (
        x,
        list (
            c ("highway", "traffic_signals"),
            c ("crossing", "no")
        )
    )
    x3c <- get_key_val_pair (
        x,
        list (
            c ("highway", "crossing"),
            c ("crossing", "traffic_signals")
        )
    )

    # 4. Intersection of divided and undivided highway with no lights
    crossings <- get_key_val_pair (
        x,
        list (
            c ("highway", "footway"),
            c ("footway", "crossing")
        )
    )

    xout <- unique (c (x1, x2a, x2b, x3a, x3b, x3c))
    list (
        "both" = xout,
        "forward" = x2_forw,
        "back" = x2_back,
        "crossings" = crossings
    )
}

# Get all OSM node IDs that are traffic lights from osmdata_sc object x
traffic_signal_nodes <- function (x) {

    x1 <- get_key_val_pair_node (x, list (c ("highway", "traffic_signals")))
    x2 <- get_key_val_pair_node (x, list (
        c ("highway", "crossing"),
        c ("crossing", "traffic_signals")
    ))
    unique (c (x1, x2))
}
