#' Plot Shortest Path on a Network Graph
#'
#' This function computes and plots the shortest path(s) between coordinates on a street network,
#' with flexible and robust argument checking and support for custom network and road geometries.
#'
#' @param network A dodgr network data frame (edges, must include from/to coordinates and IDs).
#' @param from_coords Numeric vector (length 2) or matrix/data.frame (n x 2) of origin coordinates (x/lon, y/lat).
#' @param to_coords Numeric vector (length 2) or matrix/data.frame (n x 2) of destination coordinates (x/lon, y/lat).
#' @param roads_sf An sf object of road geometries to plot as background.

#' @param crop_roads Logical; if TRUE, crop roads_sf to the extent of the path and endpoints.
#' @param path_col Colour for the path line.
#' @param path_size Width for the path line.
#' @param ... Additional arguments passed to ggplot2::geom_sf or geom_segment.
#' @return A ggplot object showing the network, roads, and shortest path(s).
#' @importFrom ggplot2 ggplot geom_sf geom_segment geom_point  geom_sf_label
#' @importFrom dplyr distinct
#' @export
path_map <- function(network, from_coords, to_coords, roads_sf, crop_roads = TRUE, path_col = 'pink', path_size = 2, ...) {
  # Argument checks
  stopifnot(is.data.frame(network),
            inherits(roads_sf, 'sf'),
            is.numeric(from_coords) || is.matrix(from_coords) || is.data.frame(from_coords),
            is.numeric(to_coords)   || is.matrix(to_coords)   || is.data.frame(to_coords))
  # Compute path
  path <- try(dodgr_paths(network, from = from_coords, to = to_coords, vertices = TRUE), silent = TRUE)
  if (inherits(path, 'try-error')) stop('dodgr_paths failed: ', as.character(path))
  path_idx <- unlist(path)
  if (length(path_idx) == 0) stop('No path found between given points')
  v <- dodgr_vertices(network)
  path_df <- v[match(path_idx, v$id),]
  fc <- as.data.frame(from_coords)
  tc <- as.data.frame(to_coords)
  names(fc) <- names(tc) <- c("x", "y")
  tc$label <- "to"
  fc$label <- "from"
  coords <- rbind(
    fc,
    data.frame(x = path_df$x, y = path_df$y, label=NA_character_),
    tc
  )
  # Optionally crop roads_sf to bounding box of path and endpoints
  if (crop_roads) {
    # Collect all coordinates: from, to, and path edge endpoints
    # Ensure all coordinate frames have columns named x and y
    bbox <- sf::st_bbox(
      c(
        xmin = min(coords$x),
        xmax = max(coords$x),
        ymin = min(coords$y),
        ymax = max(coords$y)
      ),
      crs = sf::st_crs(roads_sf)
    )
    # Crop roads_sf to bbox
    roads_sf <- suppressWarnings(sf::st_crop(roads_sf, bbox))
  }

  # Plot
  # Prepare from/to points for plotting
  labs <- coords%>%filter(!is.na(label))
  p <- ggplot() +
    geom_sf(data = roads_sf, color = 'gray', ...)
  p <- p +
    geom_path(aes(x = x, y = y), data = coords, color = path_col, linewidth = path_size) +
    geom_point(data = labs, aes(x = x, y = y), color = 'blue', size = 3) +
    ggrepel::geom_label_repel(data = labs, aes(x = x, y = y, label = label), color = 'blue', size = 4)
  return(p)
}
