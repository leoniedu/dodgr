test_that ("break_long_edges", {
  # Create weighted street network
  net <- weight_streetnet(hampi)

  # View initial distribution of edge lengths
  summary(net$d)
  hist(net$d, breaks = 50, main = "Original edge length distribution")

  # Break long edges using default threshold
  net_modified <- break_long_edges(net)  # Default verbose = TRUE

  # Run quietly
  net_modified2 <- break_long_edges(net, max_d = 100, verbose = FALSE)
})