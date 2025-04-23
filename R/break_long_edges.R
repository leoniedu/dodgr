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
#'   new vertices using `dodgr_insert_vertex_parallel`. This process continues until no edges
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
#' net <- weight_streetnet(hampi)
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
#' 
#' @export
break_long_edges <- function(graph, max_d = NULL, verbose = TRUE, cl = NULL) {
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
  cache_status <- dodgr_cache_off()
  on.exit({
    # Restore dodgr cache status
    if (cache_status) {
      dodgr_cache_on()
    }
  }, add = TRUE)
  
  # Initialize graph columns
  gr_cols <- dodgr_graph_cols(graph)
  
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
    
    # Report progress
    if ((verbose) && ((iteration %% report_freq) == 0)) {
      cli::cli_inform(
        glue::glue(
          "Iteration {iteration}: {nrow(long_edges)} edges > max_d, ",
          "longest = {round(max(long_edges$d), 2)}"
        )
      )
    }
    
    # Process all long edges in parallel batches
    edge_pairs <- lapply(seq_len(nrow(long_edges)), function(i) {
      list(v1 = long_edges$to_id[i], v2 = long_edges$from_id[i])
    })
    graph_modified <- insert_edges_parallel(
      graph = graph_modified,
      indices = edge_pairs,
      gr_cols = gr_cols,
      cl = cl
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


#' Insert vertices into multiple edges in parallel
#'
#' @param graph A data.frame containing the graph edges
#' @param indices Vector of edge indices to split
#' @param x,y Vectors of x,y coordinates for new vertices (or NULL for midpoints)
#' @param gr_cols Column names of graph structure
#' @param cl Optional cluster object for parallel processing
#' @return Modified graph with new vertices inserted
#' @noRd
insert_edges_parallel <- function(graph, indices, x = NULL, y = NULL, gr_cols, cl = NULL) {
  n_splits <- length(indices)
  if (n_splits == 0) return(graph)
  
  # Calculate midpoints if x,y not provided
  if (is.null(x) && is.null(y)) {
    x <- (graph[[gr_cols$xfr]][indices] + graph[[gr_cols$xto]][indices]) / 2
    y <- (graph[[gr_cols$yfr]][indices] + graph[[gr_cols$yto]][indices]) / 2
  }
  
  # Create new graph with duplicated rows for splits
  # Use rbind to preserve column types and attributes
  result <- rbind(graph, graph[indices, ])
  
  # Prepare worker tasks with target indices for updates
  measure <- if (is_graph_spatial(graph)) get_geodist_measure(graph) else NULL
  # Create tasks vectorized
  tasks <- list(
    edge = graph[indices, ],  # All edges at once
    x = x,
    y = y,
    gr_cols = list(gr_cols),  # List to repeat for each task
    measure = list(measure),   # List to repeat for each task
    target_idx = Map(c, indices, nrow(graph) + seq_along(indices))
  )
  # Convert from columnar to row-wise format
  tasks <- Map(list, 
               edge = asplit(tasks$edge, 1),
               x = tasks$x,
               y = tasks$y,
               gr_cols = tasks$gr_cols,
               measure = tasks$measure,
               target_idx = tasks$target_idx
  )
  
  # Process edge splits in parallel
  worker_fn <- function(task) {
    # Extract parameters needed by split_edge_worker
    split_args <- list(
      edge = task$edge,
      x = task$x,
      y = task$y,
      gr_cols = task$gr_cols,
      measure = task$measure
    )
    # Combine split results with target indices
    c(do.call(split_edge_worker, split_args), 
      list(target_idx = task$target_idx))
  }
  
  if (!is.null(cl)) {
    split_results <- parallel::parLapply(cl, tasks, worker_fn)
  } else {
    split_results <- lapply(tasks, worker_fn)
  }
  
  # Update the result with split edges
  for (res in split_results) {
    # Get columns that were modified by split_edge_worker
    modified_cols <- c(
      gr_cols$xfr, gr_cols$yfr, gr_cols$xto, gr_cols$yto,  # coordinates
      gr_cols$d, gr_cols$d_weighted,                          # distances
      'edge_id'                                               # edge IDs
    )
    # Add time columns if they exist
    if (!is.na(gr_cols$time)) {
      modified_cols <- c(modified_cols, 
                         gr_cols$time, gr_cols$time_weighted)
    }
    
    # Update only modified columns
    result[res$target_idx[1], modified_cols] <- res$edge1[modified_cols]
    result[res$target_idx[2], modified_cols] <- res$edge2[modified_cols]
  }
  
  return(result)
}



#' Insert a new vertex between two vertices in a graph
#'
#' @inheritParams dodgr_insert_vertex
#' @param cl Optional cluster object for parallel processing
#' @return Modified version of graph with new vertex inserted between nominated vertices
#' @examples 
#' graph <- weight_streetnet (hampi)
#' e1 <- sample (nrow (graph), 1)
#' v1 <- graph$from_id [e1]
#' v2 <- graph$to_id [e1]
#' # insert new vertex in the middle of that randomly-selected edge:
#' graph2 <- dodgr_insert_vertex_parallel (graph, v1, v2)
#' nrow (graph)
#' nrow (graph2) # new edges added to graph2

#' @export
dodgr_insert_vertex_parallel <- function(graph, v1, v2, x = NULL, y = NULL, cl = NULL) {
  if (methods::is(graph, "dodgr_contracted")) {
    stop("dodgr_insert_vertex_parallel cannot be used with contracted graphs. Please uncontract the graph first.")
  }
  graph_t <- tbl_to_df(graph)
  gr_cols <- dodgr_graph_cols(graph_t)
  
  ##Fix: assure just one edge per v1, v2 combination

  # Find edges in both directions
  indices <- rbind(data.frame(from=v1, to=v2),
                     data.frame(from=v2, to=v1))
  browser()
  graph_index <- merge(indices, data.frame(graph_index=seq_len(nrow(graph)), 
                            from=graph[[gr_cols$from]],
                            to=graph[[gr_cols$to]]
                            ))
  
  # indices <- lapply(seq_len(nrow(graph)), function(i) list(
  #   i12=which(graph[[gr_cols$from]] == v1 & graph[[gr_cols$to]] == v2)),
  #   i21=which(graph[[gr_cols$from]] == v2 & graph[[gr_cols$to]] == v1))
  if (nrow(graph_index)==0) {
    stop("Nominated vertices do not define any edges in graph")
  }
  if (any(duplicated(graph_index))) {
    stop("Duplicated edges")
  }
  if ((!is.null(x) && is.null(y)) || (is.null(x) && !is.null(y))) {
    stop("Either both x and y must be NULL, or both must be specified")
  }
  

  # Split edges in parallel
  graph <- insert_edges_parallel(graph, indices = graph_index, x, y, gr_cols, cl)
  
  # Generate and assign new vertex IDs
  charvec <- c(letters, LETTERS, 0:9)
  randid <- function(n, charvec, len = 10) {
    replicate(n, paste0(sample(charvec, len, replace = TRUE), collapse = ""))
  }
  
  # Calculate all adjusted indices and generate IDs
  n_splits <- length(indices)
  adj_indices <- indices + seq(0, by = 1, length.out = n_splits)
  new_ids <- randid(n_splits, charvec, 10)
  
  # Vectorized assignment
  graph[adj_indices, gr_cols$to] <- new_ids
  graph[adj_indices + 1, gr_cols$from] <- new_ids
  
  attr(graph, "hash") <- get_hash(graph, contracted = FALSE, force = TRUE)
  return(graph)
}