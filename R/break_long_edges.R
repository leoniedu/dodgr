#' Break Long Edges in a Graph
#' 
#' This function breaks long edges in a graph by inserting vertices along edges that
#' exceed a specified maximum distance.
#' 
#' @param graph A data frame or dodgr_streetnet object with columns 'from_id', 'to_id', 'd', and 'component'
#' @param max_d Numeric. Maximum allowed edge length/distance. If NULL (default), uses the mean(distance)+sd(distance)
#'   distance of all edges in the graph
#' @param verbose Logical. If TRUE, displays progress information. Default is TRUE.
#' 
#' @return A modified graph with long edges broken into shorter segments
#'   by inserting new vertices. The returned object preserves the class of the input graph.
#' 
#' @details The function iteratively processes edges longer than `max_d` by inserting
#'   new vertices using `dodgr_insert_vertex`. This process continues until no edges
#'   exceed the maximum distance threshold. Note that for edges longer than 2*max_d,
#'   the process may create new edges that still exceed max_d, requiring additional
#'   iterations. The process is guaranteed to finish as each split operation reduces
#'   edge lengths by approximately half.
#'
#' @examples
#' \dontrun{
#' # Load sample data from dodgr
#' data(hampi, package = "dodgr")
#' 
#' # Create weighted street network
#' net <- dodgr::weight_streetnet(hampi)
#' 
#' # View initial distribution of edge lengths
#' summary(net$d)
#' hist(net$d, breaks = 50, main = "Original edge length distribution")
#' 
#' # Break long edges using default threshold
#' net_modified <- break_long_edges(net)  # Default verbose = TRUE
#' 
#' # Run quietly
#' net_modified2 <- break_long_edges(net, max_d = 100, verbose = FALSE)
#' }
#' 
#' @importFrom cli cli_inform cli_progress_step
#' @importFrom glue glue
#' @importFrom checkmate assert_data_frame assert_subset assert_numeric assert_number test_class assert_flag
#' @importFrom dodgr dodgr_cache_off dodgr_cache_on
#' 
#' @export
break_long_edges <- function(graph, max_d = NULL, verbose = TRUE) {
  # Input validation
  checkmate::assert_data_frame(graph)
  if (!(checkmate::test_class(graph, "dodgr_streetnet"))) {
    warning("Input graph is not a dodgr_streetnet object. Results may be unexpected.")
  }
  checkmate::assert_subset(c("from_id", "to_id", "d", "component"), names(graph))
  checkmate::assert_numeric(graph$d, finite = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(graph$component, finite = TRUE, any.missing = FALSE)
  checkmate::assert_flag(verbose)
  
  # Save original class and dodgr cache state
  original_class <- class(graph)
  cache_status <- dodgr::dodgr_cache_off()
  on.exit({
    # Restore dodgr cache status
    if (cache_status) {
      dodgr::dodgr_cache_on()
    }
  }, add = TRUE)
  
  if (!is.null(max_d)) {
    checkmate::assert_number(max_d, lower = 0, finite = TRUE)
  }
  
  # Initialize max_d if NULL
  if (is.null(max_d)) {
    max_d <- mean(graph$d)+sd(graph$d)
  }
  
  # Find initial edges exceeding max_d
  long_edges <- graph[graph$d > max_d, ]
  initial_long_edges <- nrow(long_edges)
  
  if (initial_long_edges == 0) {
    if (verbose) {
      cli::cli_inform("No edges exceed the maximum distance threshold")
    }
    return(graph)
  }
  
  if (verbose) {
    cli::cli_inform(c(
      "Starting with {initial_long_edges} edges exceeding max_d = {round(max_d, 2)}",
      "Average edge length <= {round(mean(graph$d),2)}"
    ))
  }
  
  graph_modified <- graph
  iteration <- 0
  report_freq <- max(1, floor(initial_long_edges / 10))
  
  while (nrow(long_edges) > 0) {
    iteration <- iteration + 1
    
    # Get the longest edge first (this helps reduce total iterations needed)
    long_edges <- long_edges[order(long_edges$d, decreasing = TRUE), ]
    current_edge <- long_edges[1, ]
    
    # Report progress
    if ((verbose) && ((iteration %% report_freq) == 0)) {
      cli::cli_inform(
        glue::glue(
          "Iteration {iteration}: {nrow(long_edges)} edges > max_d, ",
          "longest = {round(max(long_edges$d), 2)}"
        )
      )
    }
    
    # Process the edge
    graph_modified <- dodgr::dodgr_insert_vertex(
      graph = graph_modified,
      v1 = current_edge$to_id,
      v2 = current_edge$from_id
    )
    
    # Update long edges
    long_edges <- graph_modified[graph_modified$d > max_d, ]
  }
  
  if (verbose) {
    new_edges <- nrow(graph_modified) - nrow(graph)
    cli::cli_inform(c(
      "i" = "Processing completed after {iteration} iterations:",
      "*" = "Added {new_edges} new edges",
      "*" = "All edges are now <= {round(max_d, 2)}",
      "*" = "New average edge length <= {round(mean(graph_modified$d),2)}"
    ))
  }
  
  # Restore class
  class(graph_modified) <- original_class
  graph_modified
}
