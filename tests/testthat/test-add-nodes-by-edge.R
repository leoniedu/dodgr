# test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
#                identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))
# 
# skip_if (!test_all)

dodgr_cache_off ()
clear_dodgr_cache ()

test_that ("add_nodes_to_graph_by_edge single point per edge", {
  
  # Load a sample graph
  graph <- weight_streetnet (hampi, wt_profile = "foot")%>%
    mutate(graph_index=1:n())
  verts <- dodgr_vertices (graph)
  
  # Create a small set of points that will likely match to different edges
  set.seed (1)
  npts <- 5
  xy <- data.frame (
    x = min (verts$x) + runif (npts) * diff (range (verts$x)),
    y = min (verts$y) + runif (npts) * diff (range (verts$y))
  )
  library(dplyr)
  # Match points to graph to verify they match to different edges
  pts <- match_pts_to_graph (graph, xy, distances = TRUE)%>%
    mutate(xy_index=1:n())%>%
    group_by(index)%>%
    dplyr::filter(n()==1)

  # Filter to keep only points that match to unique edges
  xy_single <- xy [pts$xy_index, ]
  
  # find bidirectional edges
  bi <- graph[pts$index,]%>%
    select(from_id=to_id, to_id=from_id, graph_index)%>%
    inner_join(graph%>%rename(graph_index_bi=graph_index))
  bi_index <- sort(c(bi$graph_index, bi$graph_index_bi))
  
  # Process with both functions
  graph1 <- add_nodes_to_graph (graph, xy_single, intersections_only = TRUE, dist_tol = 0)
  # graph1%>%filter(graph_index%in%bi_index)
  # graph2%>%filter(graph_index%in%bi_index)
  graph2 <- add_nodes_to_graph_by_edge (graph, xy_single, intersections_only = TRUE, dist_tol = 0)
  
  # nrow(graph1)+nrow(xy_single)
  # Compare results
  expect_equal (nrow (graph1), nrow (graph2))
  
  # Compare total distance in the graph
  total_dist1 <- sum (graph1$d)
  total_dist2 <- sum (graph2$d)
  expect_equal (total_dist1, total_dist2, tolerance = 1e-6)
  
  # Compare structure
  expect_equal (ncol (graph1), ncol (graph2))
  
  # Compare number of unique vertices
  verts1 <- unique (c (graph1$from, graph1$to))
  verts2 <- unique (c (graph2$from, graph2$to))
  expect_equal (length (verts1), length (verts2))
})

test_that ("add_nodes_to_graph_by_edge multiple points per edge", {
  
  # Load a sample graph
  graph <- weight_streetnet (hampi, wt_profile = "foot")
  verts <- dodgr_vertices (graph)
  
  # Create a set of points where multiple points will match to the same edge
  set.seed (2)
  npts <- 20 # More points increases chance of multiple matches
  xy <- data.frame (
    x = min (verts$x) + runif (npts) * diff (range (verts$x)),
    y = min (verts$y) + runif (npts) * diff (range (verts$y))
  )
  
  # Match points to graph
  pts <- match_pts_to_graph (graph, xy, distances = TRUE)
  edge_counts <- table (pts$index)
  
  # Identify edges with multiple points
  multi_point_edges <- as.integer (names (edge_counts [edge_counts > 1]))
  
  # Verify we have at least one edge with multiple points
  expect_true (length (multi_point_edges) > 0)
  
  # Create a dataset with multiple points per edge
  multi_point_indices <- which (pts$index %in% multi_point_edges)
  xy_multi <- xy [multi_point_indices, ]
  
  # Process with both functions
  graph1 <- add_nodes_to_graph (graph, xy_multi)
  graph2 <- add_nodes_to_graph_by_edge (graph, xy_multi)
  
  # Compare results
  
  # The edge-based approach should be more efficient with multiple points
  # So the total distance should be less or equal
  total_dist1 <- sum (graph1$d)
  total_dist2 <- sum (graph2$d)
  
  # The edge-based approach should create fewer edges
  expect_true (nrow (graph2) <= nrow (graph1))
  
  # The total distance should be less or equal for the edge-based approach
  expect_true (total_dist2 <= total_dist1 * 1.001) # Allow small tolerance
  
  # Print the difference for diagnostic purposes
  cat ("\nMultiple points per edge comparison:\n")
  cat ("Original graph edges:", nrow (graph), "\n")
  cat ("add_nodes_to_graph edges:", nrow (graph1), "\n")
  cat ("add_nodes_to_graph_by_edge edges:", nrow (graph2), "\n")
  cat ("add_nodes_to_graph total distance:", total_dist1, "\n")
  cat ("add_nodes_to_graph_by_edge total distance:", total_dist2, "\n")
  cat ("Distance ratio (edge/point):", total_dist2 / total_dist1, "\n")
})

test_that ("add_nodes_to_graph_by_edge with mixed point distribution", {
  
  # Load a sample graph
  graph <- weight_streetnet (hampi, wt_profile = "foot")
  verts <- dodgr_vertices (graph)
  
  # Create a larger set of points with mixed distribution
  set.seed (3)
  npts <- 30
  xy <- data.frame (
    x = min (verts$x) + runif (npts) * diff (range (verts$x)),
    y = min (verts$y) + runif (npts) * diff (range (verts$y))
  )
  
  # Process with both functions
  graph1 <- add_nodes_to_graph (graph, xy)
  graph2 <- add_nodes_to_graph_by_edge (graph, xy)
  
  # Compare results
  
  # The edge-based approach should generally be more efficient
  total_dist1 <- sum (graph1$d)
  total_dist2 <- sum (graph2$d)
  
  # Print the difference for diagnostic purposes
  cat ("\nMixed point distribution comparison:\n")
  cat ("Original graph edges:", nrow (graph), "\n")
  cat ("add_nodes_to_graph edges:", nrow (graph1), "\n")
  cat ("add_nodes_to_graph_by_edge edges:", nrow (graph2), "\n")
  cat ("add_nodes_to_graph total distance:", total_dist1, "\n")
  cat ("add_nodes_to_graph_by_edge total distance:", total_dist2, "\n")
  cat ("Distance ratio (edge/point):", total_dist2 / total_dist1, "\n")
  
  # Calculate efficiency metrics
  edge_increase1 <- nrow (graph1) - nrow (graph)
  edge_increase2 <- nrow (graph2) - nrow (graph)
  
  cat ("Edge increase (point method):", edge_increase1, "\n")
  cat ("Edge increase (edge method):", edge_increase2, "\n")
  cat ("Edge efficiency ratio:", edge_increase2 / edge_increase1, "\n")
  
  # The edge-based approach should be more efficient in terms of edges added
  expect_true (edge_increase2 <= edge_increase1)
  
  # Verify that both graphs have the same number of unique vertices
  # (excluding the intermediate vertices created during edge splitting)
  verts1 <- unique (c (graph1$from_id, graph1$to_id))
  verts2 <- unique (c (graph2$from_id, graph2$to_id))
  
  # The number of vertices might differ slightly due to different splitting approaches
  # but the difference should be small relative to the total
  vertex_diff_ratio <- abs (length (verts1) - length (verts2)) / length (verts1)
  expect_true (vertex_diff_ratio < 0.1) # Allow up to 10% difference
})

test_that ("add_nodes_to_graph_by_edge preserves edge properties", {
  
  # Load a sample graph
  graph <- weight_streetnet (hampi, wt_profile = "foot")
  
  # Create a small set of test points
  set.seed (4)
  npts <- 10
  verts <- dodgr_vertices (graph)
  xy <- data.frame (
    x = min (verts$x) + runif (npts) * diff (range (verts$x)),
    y = min (verts$y) + runif (npts) * diff (range (verts$y))
  )
  
  # Process with both functions
  graph1 <- add_nodes_to_graph (graph, xy)
  graph2 <- add_nodes_to_graph_by_edge (graph, xy)
  
  # Check that all required columns are preserved
  expect_equal (sort (names (graph)), sort (names (graph1)))
  expect_equal (sort (names (graph)), sort (names (graph2)))
  
  # Check that the d_weighted/d ratio is preserved (within tolerance)
  # For the original edges
  orig_ratio <- mean (graph$d_weighted / graph$d, na.rm = TRUE)
  
  # For the new edges in graph1
  new_edges1 <- setdiff (
    paste (graph1$from_id, graph1$to_id, sep = "-"),
    paste (graph$from_id, graph$to_id, sep = "-")
  )
  new_idx1 <- which (paste (graph1$from_id, graph1$to_id, sep = "-") %in% new_edges1)
  new_ratio1 <- mean (graph1$d_weighted [new_idx1] / graph1$d [new_idx1], na.rm = TRUE)
  
  # For the new edges in graph2
  new_edges2 <- setdiff (
    paste (graph2$from_id, graph2$to_id, sep = "-"),
    paste (graph$from_id, graph$to_id, sep = "-")
  )
  new_idx2 <- which (paste (graph2$from_id, graph2$to_id, sep = "-") %in% new_edges2)
  new_ratio2 <- mean (graph2$d_weighted [new_idx2] / graph2$d [new_idx2], na.rm = TRUE)
  
  # The ratios should be similar
  expect_equal (orig_ratio, new_ratio1, tolerance = 0.1)
  expect_equal (orig_ratio, new_ratio2, tolerance = 0.1)
  
  # Check that time and time_weighted are also preserved correctly
  time_ratio_orig <- mean (graph$time_weighted / graph$time, na.rm = TRUE)
  time_ratio1 <- mean (graph1$time_weighted [new_idx1] / graph1$time [new_idx1], na.rm = TRUE)
  time_ratio2 <- mean (graph2$time_weighted [new_idx2] / graph2$time [new_idx2], na.rm = TRUE)
  
  expect_equal (time_ratio_orig, time_ratio1, tolerance = 0.1)
  expect_equal (time_ratio_orig, time_ratio2, tolerance = 0.1)
})
