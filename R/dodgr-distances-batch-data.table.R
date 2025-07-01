#' Batch Distance Calculation with Arrow Parquet Output (data.table version)
#'
#' Calculates pairwise distances in batches and writes each batch as a Parquet file (Arrow Dataset).
#' Uses data.table::melt for fast pivoting.
#' All batches are stored in an output directory. No CSV fallback.
#'
#' @param from Matrix or data.frame of origin coordinates (rownames required)
#' @param to Matrix or data.frame of destination coordinates (rownames required)
#' @param graph A dodgr graph object
#' @param batch_size Approximate number of from-to pairs per batch
#' @param output_dir Output directory for Arrow Parquet files (will be created/cleaned)
#' @param replace If TRUE, overwrite output_dir if it exists
#' @param append Ignored (kept for backward compat)
#' @param ... Passed to dodgr::dodgr_distances
#' @importFrom arrow write_parquet open_dataset
#' @importFrom fs dir_create dir_delete dir_exists
#' @importFrom data.table as.data.table melt setnames
#' @return An Arrow Dataset object (see arrow::open_dataset) pointing to the output_dir
#' @examples
#' ds <- dodgr_distances_batch_dt(...)
#' dplyr::collect(ds)
dodgr_distances_batch <- function(graph, from, to, ..., batch_size = 1e6, output_dir = "distances_parquet", replace=FALSE, append=FALSE, calculate_time=TRUE, shortest=TRUE) {
  if (fs::dir_exists(output_dir)) {
    if (replace) {
      fs::dir_delete(output_dir)
      fs::dir_create(output_dir)
    } else {
      stop("Output directory exists. Use replace=TRUE to overwrite.")
    }
  } else {
    fs::dir_create(output_dir)
  }
  
  if (calculate_time) {
    stopifnot(all(c('time', 'time_weighted')%in%names(graph)))
    graph_t <- graph
    graph_t$d <- graph_t$time
  }
  
  # Get dimensions and IDs
  n_from <- nrow(from)
  n_to <- nrow(to)
  id_from <- rownames(from)
  id_to <- rownames(to)
  if (is.null(id_from) | is.null(id_to)) stop("must have rownames")
  
  # Swap if "to" is larger than "from"
  swapped <- FALSE
  if (n_to > n_from) {
    # Swap from and to
    temp_from <- from
    temp_id_from <- id_from
    temp_n_from <- n_from
    
    from <- to
    id_from <- id_to
    n_from <- n_to
    
    to <- temp_from
    id_to <- temp_id_from
    n_to <- temp_n_from
    
    swapped <- TRUE
  }
  
  # Calculate batch size
  batch_r <- floor(batch_size / n_to )
  if (batch_r < 1) batch_r <- 1  # Ensure at least one row per batch
  
  # Calculate total batches
  total_batches <- ceiling(n_from / batch_r)
  for (i in 1:total_batches) {
    cat("Processing batch", i, "of", total_batches, "\n")
    
    # Batch by "from" and use all "to"
    from_start <- (i-1) * batch_r + 1
    from_end <- min(i * batch_r, n_from)
    batch_from_data <- from[from_start:from_end, ]
    batch_from_ids <- id_from[from_start:from_end]
    
    # Calculate distances
    batch_results <- dodgr::dodgr_distances(
      from = batch_from_data,
      to = to,
      graph = graph,
      shortest = shortest, 
      ...
    )
    batch_results <- round(batch_results)
    rownames(batch_results) <- batch_from_ids
    colnames(batch_results) <- id_to
    dd <- data.table::as.data.table(batch_results, keep.rownames = "from_id")
    batch_df <- data.table::melt(dd, id.vars = "from_id", variable.name = "to_id", value.name = "distance")
    
    # Handle time calculation if needed
    if (calculate_time) {
      batch_results_t <- dodgr::dodgr_distances(
        from = batch_from_data,
        to = to,
        graph = graph_t,
        shortest = shortest, 
        ...
      )
      batch_results_t <- round(batch_results_t)
      # Since batch_df already has the correct structure and order from the distance calculation,
      # we can directly assign the time values without intermediate transformations
      # Using as.vector() without transpose to match data.table::melt order
      batch_df[, time := as.vector(batch_results_t)]
    }
    
    # If we swapped, swap back the column names in the result
    if (swapped) {
      # Rename columns to reflect the original input order using setnames
      data.table::setnames(batch_df, c("from_id", "to_id"), c("to_id", "from_id"))
    }
    # Write each batch to a separate file
    batch_file <- file.path(output_dir, sprintf("batch_%04d.parquet", i))
    arrow::write_parquet(batch_df, batch_file)
  }
  arrow::open_dataset(output_dir)
}
