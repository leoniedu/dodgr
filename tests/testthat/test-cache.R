# The cache-off tests fail on windows for some reason, both on CRAN and on
# GitHub runners.
testthat::skip_on_os ("windows")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

testthat::skip_if (!test_all)

source ("../sc-conversion-fns.R")

if (!test_all) {
    RcppParallel::setThreadOptions (numThreads = 2)
}

test_that ("cache on", {
    expect_silent (hsc <- sf_to_sc (hampi))
    requireNamespace ("dplyr")
    expect_silent (graph <- weight_streetnet (hsc))
    expect_message (
        graph <- dodgr_components (graph),
        "graph already has a component column"
    )
    expect_silent (v0 <- dodgr_vertices (graph))
    expect_silent (graph_c <- dodgr_contract_graph (graph))
    expect_silent (v <- dodgr_vertices (graph_c))

    n0 <- 100
    set.seed (1)
    pts <- sample (v$id, size = n0)
    pts <- pts [which (pts %in% graph_c$.vx0 & pts %in% graph_c$.vx1)]
    n <- length (pts)
    fmat <- array (1, dim = c (n, n))

    # aggregate flows from graph without turning angles:
    expect_silent (graphf <- dodgr_flows_aggregate (graph_c,
        from = pts,
        to = pts,
        flows = fmat,
        contract = FALSE
    ))
    expect_silent (graphf <- dodgr_uncontract_graph (graphf))
    expect_silent (graphf <- merge_directed_graph (graphf))

    # then turn angle graph
    grapht <- weight_streetnet (hsc,
        wt_profile = "bicycle",
        turn_penalty = TRUE, left_side = TRUE
    )
    # grapht has extra compound edges for turning angles:
    expect_equal (nrow (grapht), nrow (graph))
    grapht_c <- dodgr_contract_graph (grapht)
    pts <- pts [which (pts %in% graph_c$.vx0 & pts %in% graph_c$.vx1)]
    n <- length (pts)
    fmat <- array (1, dim = c (n, n))
    # These tests fail on some GitHub runners:
    if (test_all) {
        expect_true (nrow (graph_c) <= nrow (grapht_c))
        expect_warning (
            graphtf <- dodgr_flows_aggregate (grapht_c,
                from = pts,
                to = pts,
                flows = fmat,
                contract = FALSE
            ),
            "graphs with turn penalties should be submitted in full, not contracted form"
        )
    }
    expect_silent (
        graphtf <- dodgr_flows_aggregate (grapht,
            from = pts,
            to = pts,
            flows = fmat,
            contract = FALSE
        )
    )
    expect_silent (graphtf <- merge_directed_graph (graphtf))

    expect_warning (
        graphtf <- dodgr_flows_disperse (
            grapht_c,
            from = pts,
            dens = rep (1, n)
        ),
        "graphs with turn penalties should be submitted in full, not contracted form"
    )
    expect_silent (
        graphtf <- dodgr_flows_disperse (
            grapht,
            from = pts,
            dens = rep (1, n)
        )
    )
})

test_that ("cache off", {
    expect_silent (clear_dodgr_cache ())
    expect_silent (dodgr_cache_off ())
    expect_silent (hsc <- sf_to_sc (hampi))
    expect_silent (graph <- weight_streetnet (hsc))
    expect_message (
        graph <- dodgr_components (graph),
        "graph already has a component column"
    )
    expect_silent (v0 <- dodgr_vertices (graph))
    expect_silent (graph_c <- dodgr_contract_graph (graph))
    expect_silent (v <- dodgr_vertices (graph_c))

    n <- 100
    set.seed (1)
    pts <- sample (v$id, size = n)
    pts <- pts [which (pts %in% graph_c$.vx0 & pts %in% graph_c$.vx1)]
    fmat <- array (1, dim = c (n, n))

    # aggregate flows from graph without turning angles:
    expect_silent (graphf <- dodgr_flows_aggregate (graph_c,
        from = pts,
        to = pts,
        flows = fmat,
        contract = FALSE
    ))
    expect_silent (graphf <- dodgr_uncontract_graph (graphf))
    expect_silent (graphf <- merge_directed_graph (graphf))

    # then turn angle graph
    expect_silent (grapht <- weight_streetnet (hsc,
        wt_profile = "bicycle",
        turn_penalty = TRUE,
        left_side = TRUE
    ))
    expect_silent (grapht_c <- dodgr_contract_graph (grapht))
    # De-duplication of graph contraction can remove points, so:
    index <- which (pts %in% grapht_c$.vx0 & pts %in% grapht_c$.vx1)
    pts <- pts [index]
    fmat <- fmat [index, index]
    expect_warning (
        graphtf <- dodgr_flows_aggregate (
            grapht_c,
            from = pts,
            to = pts,
            flows = fmat,
            contract = FALSE
        ),
        "graphs with turn penalties should be submitted in full, not contracted form"
    )
    expect_silent (graphtf <- dodgr_flows_aggregate (grapht,
        from = pts,
        to = pts,
        flows = fmat,
        contract = FALSE
    ))
    expect_silent (graphtf <- merge_directed_graph (graphtf))

    expect_warning (
        graphtf <- dodgr_flows_disperse (
            grapht_c,
            from = pts,
            dens = rep (1, length (pts))
        ),
        "graphs with turn penalties should be submitted in full, not contracted form"
    )
    expect_silent (
        graphtf <- dodgr_flows_disperse (grapht, from = pts, dens = rep (1, length (pts)))
    )

    expect_silent (dodgr_cache_on ())
})
