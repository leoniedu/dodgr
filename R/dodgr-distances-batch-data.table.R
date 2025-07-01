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
#' @importFrom data.table as.data.table melt
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
  n_from <- nrow(from)
  n_to <- nrow(to)
  id_from <- rownames(from)
  id_to <- rownames(to)
  if (is.null(id_from) | is.null(id_to)) stop("must have rownames")
  total_pairs <- n_from * n_to
  batch_from <- min(n_from, ceiling(sqrt(batch_size * n_from / n_to)))
  batch_to <- min(n_to, ceiling(sqrt(batch_size * n_to / n_from)))

  total_from_batches <- ceiling(n_from / batch_from)
  total_to_batches <- ceiling(n_to / batch_to)

  batch_counter <- 0
  total_batches <- total_from_batches * total_to_batches

  for (i in 1:total_from_batches) {
    from_start <- (i-1) * batch_from + 1
    from_end <- min(i * batch_from, n_from)
    batch_from <- from[from_start:from_end, ]
    batch_from_ids <- id_from[from_start:from_end]
    

    for (j in 1:total_to_batches) {
      batch_counter <- batch_counter + 1
      cat("Processing batch", batch_counter, "of", total_batches, "\n")
      to_start <- (j-1) * batch_to + 1
      to_end <- min(j * batch_to, n_to)
      batch_to <- to[to_start:to_end, ]
      batch_to_ids <- id_to[to_start:to_end]
      batch_results <- dodgr::dodgr_distances(
        from = batch_from,
        to = batch_to,
        graph = graph,
        shortest = shortest, 
        ...
      )
      batch_results <- round(batch_results)
      rownames(batch_results) <- batch_from_ids
      colnames(batch_results) <- batch_to_ids
      dd <- data.table::as.data.table(batch_results, keep.rownames = "from_id")
      batch_df <- data.table::melt(dd, id.vars = "from_id", variable.name = "to_id", value.name = "distance")
      if (calculate_time) {
        batch_results_t <- dodgr::dodgr_distances(
          from = batch_from,
          to = batch_to,
          graph = graph_t,
          shortest = shortest, 
          ...
        )
        batch_results_t <- round(batch_results_t)
        rownames(batch_results_t) <- batch_from_ids
        colnames(batch_results_t) <- batch_to_ids
        dt <- data.table::as.data.table(batch_results_t, keep.rownames = "from_id")
        batch_df_t <- data.table::melt(dt, id.vars = "from_id", variable.name = "to_id", value.name = "time")
        batch_df[,time:=batch_df_t$time]
      }
      batch_file <- file.path(output_dir, sprintf("batch_%04d.parquet", batch_counter))
      arrow::write_parquet(batch_df, batch_file)
      rm(batch_results, batch_df, dt)
      gc()
    }
  }
  arrow::open_dataset(output_dir)
}
