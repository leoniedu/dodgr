test_that("add_nodes_to_graph with multiple points on same edge", {
    # Create a simple graph with one edge
  graph <- weight_streetnet(hampi)
  graph <- graph[which.max(graph$d),]
  n_points <- 1
  # Create points that will match to this edge
  xy <- data.frame(
    x = runif(n_points, min=graph$from_lon, max=graph$to_lon),
    y = runif(n_points, min=graph$from_lat, max=graph$to_lat))

  # Run the function and examine output
  result <- add_nodes_to_graph(graph, xy
                               , intersections_only=TRUE
  )
  
  # Print details about the result
  print(paste("Number of rows in result:", nrow(result)))
  print("Result coordinates:")
  print(result[, c("from_lon", "from_lat", "to_lon", "to_lat")])
  
  # The result should have the same length
  expect_equal(sum(result$d), sum(graph$d))
  # The result should have 6 edges (2 original edges each split into 3 parts)
  expect_equal(nrow(result), 6)
  
    # Check that coordinates are continuous
    result_forward <- result[result$from == "a", ]
    result_forward <- result_forward[order(result_forward$xfr), ]
    
    # Each edge should end where the next begins
    expect_equal(result_forward$xto[1], result_forward$xfr[2])
    expect_equal(result_forward$yto[1], result_forward$yfr[2])
    expect_equal(result_forward$xto[2], result_forward$xfr[3])
    expect_equal(result_forward$yto[2], result_forward$yfr[3])
})
