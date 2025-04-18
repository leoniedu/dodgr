dodgr_cache_off ()
clear_dodgr_cache ()

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))
# used below in a skip_if call

test_that ("streetnet bbox", {

    set.seed (1)
    n <- 12
    bbox <- cbind (runif (n), 2 * runif (n))
    bb <- process_bbox (bbox, NULL, 0)
    expect_is (bb, "list")
    expect_length (bb, 2)
    expect_equal (nrow (bb$bbox), 2)
    expect_equal (nrow (bb$bbox_poly), n)

    bbox2 <- apply (bbox, 2, range)
    bb2 <- process_bbox (bbox2, NULL, 0)
    expect_identical (bb$bbox, bb2$bbox)

    rownames (bbox2) <- c ("min", "max")
    colnames (bbox2) <- c ("x", "y")
    expect_silent (bb3 <- process_bbox (bbox2, NULL, 0))
    expect_true (!identical (bb2, bb3))

    colnames (bbox) <- c ("x", "y")
    bb4 <- process_bbox (bbox, expand = 0)
    expect_identical (bb$bbox, bb4$bbox)

    # causes bbox to be tranposed:
    colnames (bbox) <- c ("min", "max")
    bb5 <- process_bbox (bbox, expand = 0)
    expect_identical (bb$bbox, bb5$bbox)

    expect_silent (bb2 <- process_bbox (list (bbox), NULL, 0))
    expect_true (!identical (bb, bb2))

    bbox <- list (matrix (letters [ceiling (runif (n) * 26)],
        ncol = 2
    ))
    expect_error (
        bb <- process_bbox (bbox, NULL, 0),
        "bbox is a list, so items must be numeric"
    )
    bbox <- runif (6)
    expect_error (
        bb <- process_bbox (bbox, NULL, 0),
        "bbox must have four numeric values"
    )

    bbox <- bbox [1:4]
    expect_silent (bb <- process_bbox (bbox, NULL, 0))

    expect_error (
        bb <- process_bbox (pts = NULL),
        "Either bbox or pts must be specified"
    )
})

test_that ("streetnet pts", {

    set.seed (1)
    n <- 12
    pts <- cbind (runif (n), 2 * runif (n))
    expect_error (
        bb <- process_bbox (pts = pts, expand = 0),
        paste0 (
            "Can not unambiguously determine ",
            "coordinates in graph"
        )
    )

    colnames (pts) <- c ("x", "y")
    expect_silent (bb <- process_bbox (pts = pts, expand = 0))
    # This gives wrong result:
    expect_silent (bb2 <- process_bbox (bbox = pts, expand = 0))
    expect_true (!identical (bb$bbox, bb2$bbox))
})


test_that ("streetnet column names", {

    h <- hampi
    h$geometry <- NULL
    expect_error (graph <- weight_streetnet (h))
    # error with no sf is: "Unable to determine geometry column", but with sf, h
    # is de-classes, so error is "Unknown class"

    h <- hampi
    h$osm_id <- NULL
    expect_message (
        graph <- weight_streetnet (h),
        paste0 (
            "x appears to have no ID column; ",
            "sequential edge numbers will be used"
        )
    )
    expect_true ("way_id" %in% names (graph))

    names (h$geometry) <- NULL
    expect_message (
        graph <- weight_streetnet (h),
        paste0 (
            "x appears to have no ID column; ",
            "sequential edge numbers will be used"
        )
    )
    expect_true ("way_id" %in% names (graph))

    h <- hampi
    names (h) [names (h) == "osm_id"] <- "id1"
    h$id2 <- h$id1
    expect_error (
        graph <- weight_streetnet (h),
        "Multiple potential ID columns"
    )

    h <- hampi
    h$geom <- 1
    expect_error (
        graph <- weight_streetnet (h),
        "Unable to determine geometry column"
    )

    skip_if (!test_all)

    h <- hampi
    h$geometry1 <- 1
    expect_silent (graph <- weight_streetnet (h))

    h <- hampi
    osm_id <- h$osm_id
    h$osm_id <- NULL
    h$osm_id <- osm_id
    expect_silent (graph <- weight_streetnet (h))

    graph0 <- weight_streetnet (hampi, wt_profile = "bicycle")
    # add some fake oneway paths:
    h <- hampi
    index <- which (hampi$highway == "path")
    index <- index [sample (length (index) / 2)]
    h$oneway [index] <- "yes"
    graph1 <- weight_streetnet (h, wt_profile = "bicycle")
    expect_true (nrow (graph1) < nrow (graph0))

    h ["oneway.bicycle"] <- h$oneway
    h [["oneway.bicycle"]] [index] <- "yes"
    graph2 <- weight_streetnet (h, wt_profile = "bicycle")
    expect_true (nrow (graph2) == nrow (graph1))

    h ["oneway.bicycle"] <- NULL
    h ["oneway:bicycle"] <- h$oneway
    h [["oneway:bicycle"]] [index] <- "yes"
    graph3 <- weight_streetnet (h, wt_profile = "bicycle")
    expect_identical (nrow (graph2), nrow (graph3))

    # change "oneway", but with wt_profile == "bicycle", only "oneway*bicycle"
    # should affect result:
    index <- which (hampi$highway == "path")
    index <- index [sample (length (index) / 2)]
    h$oneway <- ""
    h$oneway [index] <- "yes"
    graph4 <- weight_streetnet (h, wt_profile = "bicycle")
    expect_identical (nrow (graph2), nrow (graph4))
})

test_that ("wt_profile", {
    expect_silent (graph <- weight_streetnet (hampi, wt_profile = 1))
    expect_identical (graph$d, graph$d_weighted)
})

test_that ("streetnet highway types", {
    # these are based on partial matches, so modifications to highway types
    # sholuld have no effect:
    graph0 <- weight_streetnet (hampi)
    n <- 10
    index <- sample (nrow (hampi), n)
    h <- hampi
    h$highway [index] <- paste0 (h$highway [index], sample (letters, n))
    graph <- weight_streetnet (h)

    expect_identical (table (graph$highway), table (graph0$highway))

    h$highway [sample (nrow (h), 1)] <- "invalid_type"
    expect_message (
        graph <- weight_streetnet (h),
        "The following highway types are present in data yet lack"
    )
})

test_that ("hash generation", {
    skip_on_cran ()
    # The following test fails on GitHub windows machines for some reason?
    is_windows <- Sys.info () [["sysname"]] == "Windows"
    if (!is_windows) {

        graph <- weight_streetnet (hampi)
        graphc <- dodgr_contract_graph (graph)
        attr (graph, "hash") <- NULL
        graphc2 <- dodgr_contract_graph (graph)
        expect_identical (graphc, graphc2)
    }
})

test_that ("streetnet times", {
    expect_error (
        graph <- weight_streetnet (hampi,
            turn_penalty = TRUE
        ),
        paste0 (
            "Turn-penalty calculations only currently ",
            "implemented for street network data ",
            "generated with"
        )
    )
    expect_silent (graph <- weight_streetnet (hampi))
    h <- hampi
    names (h) [names (h) == "osm_id"] <- "id"
    expect_silent (graph2 <- weight_streetnet (h, id_col = "id"))
    attr (graph, "px") <- NULL
    attr (graph2, "px") <- NULL
    expect_identical (graph, graph2)

    h$id <- NULL
    msg <- paste (
        "x appears to have no ID column;",
        "sequential edge numbers will be used."
    )
    expect_message (graph3 <- weight_streetnet (h), msg)

    h <- hampi
    names (h$geometry) <- NULL
    graph4 <- weight_streetnet (h)
    expect_identical (graph$edge_id, seq (nrow (graph)))

    h$oneway_bicycle <- h$oneway
    graph5 <- weight_streetnet (h)
    attr (graph4, "px") <- NULL
    attr (graph5, "px") <- NULL
    expect_identical (graph5, graph4)

    expect_error (
        weight_streetnet (hampi,
            wt_profile = list (1)
        ),
        "Custom named profiles must be vectors"
    )
})

test_that ("weight_streetnet runs with non-LINESTRING sfc", {
    # See #246
    skip_if_not_installed ("sf")

    toy <- sf::st_as_sf (
        data.frame (highway = "primary", osm_id = 1:2),
        geometry = sf::st_sfc (
            sf::st_linestring (rbind (c (0, 0), c (1, 1))),
            sf::st_multilinestring (
                list (
                    sf::st_linestring (rbind (c (1, 1), c (2, 2)))
                )
            )
        )
    )
    expect_warning (
        weight_streetnet (toy),
        "not a LINESTRING"
    )
})

test_that ("geodesic distances", {

    skip_if_not_installed ("sf")

    expect_error (
        get_geodist_measure ("a"),
        "Graph appears to be spatial yet unable to extract coordinates"
    )


    options ("dodgr_dist_measure" = NULL)
    graph <- weight_streetnet (hampi)
    expect_equal (get_geodist_measure (graph), "cheap")

    msg <- paste0 (
        "Only graphs created from this point on with ",
        "\\'weight_streetnet\\(\\)\\' will use the geodesic ",
        "measure."
    )
    # graphs don't exist in test environments:
    # expect_message (dodgr_streetnet_geodesic (), msg)
    dodgr_streetnet_geodesic ()
    expect_equal (get_geodist_measure (graph), "geodesic")
    op <- getOption ("dodgr_dist_measure")
    expect_length (op, 1L)
    expect_named (op, "all")
    expect_equal (op, c (all = "geodesic"))

    graph_gd <- weight_streetnet (hampi)
    dist_diff <- graph$d - graph_gd$d
    # SD of difference in distances should be > 10cm:
    expect_true (sd (dist_diff) > 0.1)

    msg <- paste0 (
        "Only graphs created from this point on with ",
        "\\'weight_streetnet\\(\\)\\' will revert to ",
        "default measures"
    )
    # expect_message (dodgr_streetnet_geodesic (unset = TRUE), msg)
    dodgr_streetnet_geodesic (unset = TRUE)
    op <- getOption ("dodgr_dist_measure")
    expect_null (op)
})
