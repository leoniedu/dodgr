library(sf)
devtools::load_all()
library(ggplot2)
library(dplyr)

path_map <- function(network, points_sf, roads_sf) {
  path <- try(dodgr_paths(network, 
                          from = head(points_sf%>%st_coordinates(),1),
                          to = tail(points_sf%>%st_coordinates(),1),
                          vertices = FALSE))
  print(network[unlist(path),])
  # vertices <- dodgr_vertices(network)%>%
  #   st_as_sf(coords=c("x", "y"), remove=FALSE)
  ggplot(network) +
    geom_sf(data=roads_sf, color="gray")+
    #geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), color="orange", alpha=1/2)+
    geom_label_repel(aes(x = from_lon, y=from_lat, label=from_id),data=network%>%distinct(from_lon, from_lat, from_id))+
    #geom_sf_label(aes(label=id),data=points_sf)+
    geom_sf_label(aes(label=id),data=points_sf)+
    geom_point(aes(x = x, y=y), color="red", data=points_sf)+
    geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=network[unlist(path),], color="pink", linewidth=2)  
}

# # 1. Create a single line
# road <- st_linestring(matrix(c(0,0, 10,0), ncol=2, byrow=TRUE))
# roads <- st_sf(
#   geometry = st_sfc(road, crs = 4326),
#   highway = "residential",
#   id = 1,
#   artificial = FALSE
# )
# 
# 
# # Print initial geometry
# message("\nInitial road geometry:")
# print(roads)
# 
# # 2. Create two points near but not on the line
# point1 <- st_point(c(2, 1))
# point2 <- st_point(c(8, -1))
# points <- st_sf(
#   geometry = st_sfc(point1, point2, crs = 4326),
#   id = c(2, 3)
# )
# points <- tibble(x=c(2,8), y=c(1,-1), id=c(2,3))%>%
#   sf::st_as_sf(coords=c("x", "y"), remove=FALSE)
# 
# # Print points geometry
# message("\nPoints geometry:")
# print(points)
# 
# # Create initial dodgr network
# net_initial <- weight_streetnet(roads, id_col = "id")
# v_initial <- dodgr_vertices(net_initial)
# message("\nInitial network structure:")
# message("- Number of vertices: ", nrow(v_initial))
# message("- Number of edges: ", nrow(net_initial))
# 
# # Create new vertices
# xy <- with(net_initial, cbind(mean(c(from_lon,to_lon)), mean(c(from_lat, to_lat))))
# net_b1 <- net_initial%>%
#   add_nodes_to_graph(xy = xy, intersections_only = TRUE)
# net_b2 <- net_initial%>%
#   add_nodes_to_graph_by_edge(xy = xy, intersections_only = TRUE)
# 
# 
# net_b2 <- net_initial%>%
#   add_nodes_to_graph_by_edge(xy = with(net_initial, cbind(to_lon+.00001, to_lat+.00001)), intersections_only = FALSE, xy_id = paste(1:nrow(net_initial), "pt"))
# 
# # Calculate path between points
# from_to <- data.frame(
#   from_id = points$id[1],
#   to_id = points$id[2]
# )
# 
# initial_paths <- try(dodgr_paths(net_b1, from = from_to$from_id, to = from_to$to_id, vertices = FALSE))
# message("\nInitial path attempt result:")
# print(initial_paths)
# net_b1[unlist(initial_paths),]
# ggplot(net_b1)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat))+
#   geom_point(aes(x = from_lon, y=from_lat), color="blue")+
#   geom_point(aes(x = x, y=y), color="red", data=points)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_b1[unlist(initial_paths),], color="pink")
# 
# # 5. Connect points to network
# net_connected1 <- add_nodes_to_graph(net_b1, st_coordinates(points))
# net_connected2 <- add_nodes_to_graph_by_edge(net_b1, st_coordinates(points))
# 
# library(ggplot2)
# ggplot(net_connected1)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat))+
#   geom_point(aes(x = from_lon, y=from_lat), color="blue")+
#   geom_point(aes(x = x, y=y), color="red", data=points)
# 
# 
# # 6. Calculate path
# paths_v1 <- dodgr_paths(net_connected1, from = st_coordinates(points)[1,], to = st_coordinates(points)[2,], vertices = TRUE)
# paths_e1 <- dodgr_paths(net_connected1, from = st_coordinates(points)[1,], to = st_coordinates(points)[2,], vertices = FALSE)
# message("Paths results:")
# print(paths_v1)
# print(paths_e1)
# ggplot(net_connected1)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat))+
#   geom_point(aes(x = from_lon, y=from_lat), color="blue")+
#   geom_point(aes(x = x, y=y), color="red", data=points)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_connected1[unlist(paths_e1),], color="pink")
# 
# 
# paths_v2 <- dodgr_paths(net_connected2, from = st_coordinates(points)[1,], to = st_coordinates(points)[2,], vertices = TRUE)
# paths_e2 <- dodgr_paths(net_connected2, from = st_coordinates(points)[1,], to = st_coordinates(points)[2,], vertices = FALSE)
# message("Paths results:")
# print(paths_v2)
# print(paths_e2)
# ggplot(net_connected2)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat))+
#   geom_point(aes(x = from_lon, y=from_lat), color="blue")+
#   geom_point(aes(x = x, y=y), color="red", data=points)+
#   geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_connected2[unlist(paths_e2),], color="pink")
# 


# Create a curved road and a crossing road
curved_road <- st_linestring(matrix(c(
  0, 0,
  2, 1,
  4, 1,
  6, 0,
  8, 0
), ncol=2, byrow=TRUE))

crossing_road <- st_linestring(matrix(c(
  4, -1,
  4, 2
), ncol=2, byrow=TRUE))

disconnected_road <- st_linestring(matrix(c(0,-2, 8,-2), ncol=2, byrow=TRUE))

roads_sf <- st_sf(
  geometry = st_sfc(curved_road,
                    crossing_road,
                    disconnected_road, crs=4326),
  highway = c("residential", "residential", "residential"),
  id = c(1,
         2,
         3)
)
roads_sf <- roads_sf[c(1,3),]


# Create points that should connect to different parts
points_sf <- st_sf(
  geometry = st_sfc(
    st_point(c(0, -3)),    # Should connect to disconnected road
    st_point(c(2, 0)),    # Should connect to curved road
    st_point(c(4, 0.5)),  # Should connect to both roads
    st_point(c(6, 1)),    # Should connect to curved road
    crs = 4326
  ),
  id = paste0("p_", 1:4
))
points_sf$x <- st_coordinates(points_sf)[,1]
points_sf$y <- st_coordinates(points_sf)[,2]


# ggplot(data=roads_sf)+geom_sf() + geom_sf(data=points_sf)


# Create initial dodgr network
net_b <- weight_streetnet(roads_sf, id_col = "id")%>%
  mutate(edge_id=as.character(edge_id))
net_b%>%filter(from_id==1)%>%distinct(from_id,from_lat, from_lon)
v_initial <- dodgr_vertices(net_b)
message("\nInitial network structure:")
message("- Number of vertices: ", nrow(v_initial))
message("- Number of edges: ", nrow(net_b))

# Calculate path between points
# initial_paths <- try(dodgr_paths(net_b, from = points_sf%>%st_coordinates(), vertices = FALSE))
# message("\nInitial path attempt result:")
# print(initial_paths)
# net_b[unlist(initial_paths),]
library(ggrepel)

path_map(network=net_b, points_sf = points_sf%>%filter(id%in%c("p_1","p_4")), roads_sf = roads_sf)


# connect networks
v2 <- v_initial%>%filter(component==2)%>%
  st_as_sf(coords=c("x", "y"), remove=FALSE)%>%
  slice(1)

net_b_1 <- net_b%>%filter(component==1)
net_b_2 <- net_b%>%filter(component==2)
net_b_12p <- add_nodes_to_graph_by_edge(net_b_1, v2, 
                                        #xy_id = v2$id, 
                                        replace_component = TRUE)
net_b_12p%>%filter(from_id==1)%>%distinct(from_id,from_lat, from_lon)

net_b_12p%>%anti_join(net_b%>%mutate(edge_id=as.character(edge_id)))
# net_b_12 <- net_b_12p%>%
#   bind_rows(net_b_2)%>%
#   select(-component)%>%
#   dodgr_components()

path_map(network=net_b_12p, points_sf = points_sf%>%filter(id%in%c("p_1","p_4")), roads_sf = roads_sf)

net_b_12p2 <- net_b_12p%>%
  add_nodes_to_graph_by_edge(xy=points_sf%>%filter(id%in%c("p_1","p_4")))

net_b_12p_artificial <- net_b_12p%>%
  add_nodes_to_graph_by_edge(xy=points_sf%>%filter(id%in%c("p_1","p_4")), highway = "artificial", wt_profile_file = "~/github/dodgrconnect/data-raw/profile.json")

path_map(network=net_b_12p2, points_sf = points_sf%>%filter(id%in%c("p_1","p_4")), roads_sf = roads_sf)

path_map(network=net_b_12p2, points_sf = points_sf%>%filter(id%in%c("p_1","p_4")), roads_sf = roads_sf)

# Calculate path between points
initial_paths <- try(dodgr_paths(net_b, from = points_sf%>%st_coordinates(), vertices = FALSE))
message("\nInitial path attempt result:")
print(initial_paths)
net_b[unlist(initial_paths),]


ggplot(net_b_12) +
  geom_sf(data=roads_sf, color="gray")+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), color="orange", alpha=1/2)+
  geom_label_repel(aes(x = from_lon, y=from_lat, label=from_id),data=net_b_12%>%distinct(from_lon, from_lat, from_id))+
  geom_point(aes(x = x, y=y), color="red", data=points_sf)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_b[unlist(initial_paths),], color="pink")

# Create new vertices

# net_b <- net_initial%>%
#   break_long_edges(max_d = 400000)

ggplot(net_b_12)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat))+
  geom_point(aes(x = from_lon, y=from_lat), color="blue")+
  geom_point(aes(x = x, y=y), color="red", data=points_sf)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_b_12[unlist(initial_paths),], color="pink")

add_nodes_fn <- function (graph, xy,..., xy_id) add_nodes_to_graph_by_edge(graph, xy=st_coordinates(xy),..., xy_id=xy$id)
# 5. Connect points to network
net_connected1 <- add_nodes_fn(net_b, points_sf%>%head(2))%>%
  ungroup()%>%
  mutate(edge_id=1:n())
net_connected2 <- net_connected1%>%
  add_nodes_fn(points_sf%>%tail(2))%>%
  ungroup()%>%
  mutate(edge_id=1:n())
net_connected3 <- net_connected2%>%
  filter(component==1)%>%
  add_nodes_fn(dodgr_vertices(net_b%>%filter(component==2)%>%head(2))%>%st_as_sf(coords=c("x", "y")))%>%
  ungroup()%>%
  bind_rows(net_connected2%>%
              mutate(edge_id=as.character(edge_id))%>%
              filter(component==2))%>%
  mutate(edge_id=1:n(), component=NULL)%>%
  dodgr::dodgr_components()

ggplot(net_connected3)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat))+
  geom_point(aes(x = from_lon, y=from_lat), color="blue")+
  geom_point(aes(x = x, y=y), color="red", data=points_sf)


# 6. Calculate path
paths_v <- dodgr_paths(net_connected3, from = st_coordinates(points_sf%>%filter(id==6)), to = st_coordinates(points_sf%>%filter(id==3)), vertices = TRUE)
paths_e <- dodgr_paths(net_connected3, from = st_coordinates(points_sf%>%filter(id==6)), to = st_coordinates(points_sf%>%filter(id==3)), vertices = FALSE)
message("Paths results:")
print(paths_v)
print(paths_e)
ggplot(net_connected3)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)))+
  geom_point(aes(x = from_lon, y=from_lat))+
  #geom_point(aes(x = x, y=y), color="red", data=points_sf)+
  geom_label(aes(x = x, y=y, label=id), color="black", data=points_sf)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_connected3[unlist(paths_e),], color="orange", linewidth=3)






# Connect vertices of component 2 to vertices of component 1
net_connected <- net_initial
net_split <- net_connected%>%
  mutate(edge_id=as.character(edge_id))%>%
  split(.$component)
# net_split2 <- net_connected[,c("edge_id",  "from_lon", "from_lat", "to_lon", "to_lat", "d", "d_weighted", "highway", "component", "time", "time_weighted", "from_id", "to_id")]%>%
#   mutate(edge_id=as.character(edge_id))%>%
#   split(.$component)
v_split <- dodgr::dodgr_vertices(net_connected)%>%split(.$component)%>%lapply(function(x) sf::st_as_sf(x, coords=c("x", "y"), remove=FALSE))

net_connected_comp12 <- add_nodes_to_graph3(graph=net_b,xy =v_split[[2]][1,c("x", "y")])%>%#bind_rows(net_split[[2]]%>%mutate(component=1, edge_id=as.character(edge_id)))%>%
  ungroup()#
# net_connected_comp12 <- add_nodes_to_graph(graph=net_split2[[1]],xy =v_split[[2]][1,c("x", "y")])%>%#bind_rows(net_split[[2]]%>%mutate(component=1, edge_id=as.character(edge_id)))%>%
#   ungroup()#%>%mutate(edge_id=1:n())
paths_v <- dodgr_paths(net_connected_comp12, from = st_coordinates(points_sf%>%filter(id==6)), to = st_coordinates(points_sf%>%filter(id==4)), vertices = TRUE)
paths_e <- dodgr_paths(net_connected_comp12, from = st_coordinates(points_sf%>%filter(id==6)), to = st_coordinates(points_sf%>%filter(id==4)), vertices = FALSE)
message("Paths results:")
print(paths_v)
print(paths_e)
ggplot(net_connected_comp12)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)))+
  geom_point(aes(x = from_lon, y=from_lat))+
  #geom_point(aes(x = x, y=y), color="red", data=points_sf)+
  geom_label(aes(x = x, y=y, label=id), color="black", data=points_sf)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_connected[unlist(paths_e),], color="orange")



ggplot(net_connected_comp12)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)))+
  geom_point(aes(x = from_lon, y=from_lat))+
  geom_label(aes(x = x, y=y, label=id), color="black", data=points_sf)

paths_v <- dodgr_paths(net_connected_comp12, from = st_coordinates(points_sf%>%filter(id==3)), to = st_coordinates(points_sf%>%filter(id==4)), vertices = TRUE)
paths_e <- dodgr_paths(net_connected_comp12, from = st_coordinates(points_sf%>%filter(id==3)), to = st_coordinates(points_sf%>%filter(id==4)), vertices = FALSE)


ggplot(net_connected_comp12)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)))+
  geom_point(aes(x = from_lon, y=from_lat))+
  geom_label(aes(x = x, y=y, label=id), color="black", data=points_sf)+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), data=net_connected[unlist(paths_e),], color="blue")



# Verify connections
expect_equal(nrow(result$artificial_segments), 4) # Should have 4 connections (one point connects twice)

# Check that point 2 connects to both roads
if (nrow(result$artificial_segments) > 0) {
  point2_connections <- result$artificial_segments %>%
    filter(st_intersects(geometry, st_geometry(points_sf[2,]), sparse=FALSE)[,1])
  expect_equal(nrow(point2_connections), 2)
}

# Check that the network is fully connected
if (nrow(result$complete_network) > 0) {
  graph <- dodgr::weight_streetnet(result$complete_network, id_col="id", wt_profile="foot")
  components <- dodgr::dodgr_components(graph)
  expect_equal(length(unique(components$component)), 1)
}

