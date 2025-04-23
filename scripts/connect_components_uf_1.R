source("scripts/connect_components_uf_0.R")

if ((!file.exists(complete_net_0_fname))|(!use_cache)) {
  if ((!file.exists(complete_sf_fname))|(!use_cache)) {
    complete_map <- arrow::open_dataset(complete_map_fname)%>%
      mutate(highway=if_else(is.na(highway) & (waterway%in%waterways), waterway, highway))%>%
      filter(!is.na(highway))
    complete_map_sf <- complete_map%>%
      st_as_sf(crs=4326)%>%
      st_intersection(uf_buffer%>%select(geom))%>%
      dodgrconnect::std_geometry()
    rm(complete_map)
    readr::write_rds(complete_map_sf, complete_sf_fname)
  } else {
    complete_map_sf <- readr::read_rds(complete_sf_fname)
  }
  complete_map_sf_r <- complete_map_sf%>%
    filter(!highway%in%waterway)
  complete_map_sf_w <- complete_map_sf%>%
    filter(highway%in%waterway)
  complete_net_r <- dodgr::weight_streetnet(complete_map_sf_r, id_col = "id", wt_profile_file = "data-raw/profile.json", wt_profile = "motorcar")
  complete_net_r <- complete_net_r%>%
    #mutate(graph_index=1:n())%>%
    mutate(edge_id=as.character(edge_id))
  complete_net_w <- dodgr::weight_streetnet(complete_map_sf_w, id_col = "id", wt_profile_file = "data-raw/profile.json", wt_profile = "motorcar")
  dim(complete_net_w)
  complete_net_w <- complete_net_w%>%
    #mutate(graph_index=paste0("w_", 1:n()))%>%
    mutate(across(ends_with("_id"), ~paste0("w_",as.character(.))))
  complete_net_0 <- bind_rows(complete_net_r, complete_net_w)%>%
    select(-component)%>%
    mutate(graph_index=1:n())%>%
    dodgr::dodgr_components()
  rm(complete_net_w)
  rm(complete_net_r)
  rm(complete_map_sf_r)
  rm(complete_map_sf_w)
  rm(complete_map_sf)
  readr::write_rds(complete_net_0, complete_net_0_fname)
} else {
  complete_net_0 <- readr::read_rds(complete_net_0_fname)
}

complete_net_1 <- complete_net_0%>%
  group_by(component)%>%
  filter(sum(d)>dmin_component)%>%
  ungroup()#%>%filter(grepl("w_",edge_id))
dim(complete_net_0)-dim(complete_net_1)
rm(complete_net_0)


## Lets find out which components connect to points
pts <- match_pts_to_graph (complete_net_1, tracts, distances = TRUE)
pts$component <- complete_net_1$component[pts$index]
pts$xy_index <- 1:nrow(pts)
## Important: pts$index always refers to the original graph
pts$index <- complete_net_1$graph_index[pts$index]

complete_net_2 <- complete_net_1%>%
  filter(component%in%pts$component)%>%
  ungroup()
rm(complete_net_1)
gc()


## assign pts to components (not necessarily the closest!)
pts_all <- pts[0,]
cmp_all <- table(pts$component)%>%sort(decreasing = TRUE)%>%names()%>%as.numeric()
#cmp_all <- cmp_all[cmp_all<=5]
tracts$xy_index <- 1:nrow(tracts)
for (i in cmp_all) {
  print(i)
  complete_net_i <- complete_net_2%>%
    filter(component==i)
  pts_cmp <- pts%>%filter(component==i)%>%
    anti_join(pts_all, by='xy_index')
  if (length(pts_all$xy_index)==0) {
    tractsnow <- tracts
  } else {
    tractsnow <- tracts[-pts_all$xy_index,]
  }
  tractsnow <- tractsnow%>%anti_join(pts_cmp, by="xy_index")
  if (nrow(tractsnow)>0) {
    pts_i <- match_pts_to_graph (complete_net_i, tractsnow, distances = TRUE)
    pts_i$component <- i
    pts_i$xy_index <- tractsnow$xy_index
    pts_i$index <- complete_net_i$graph_index[pts_i$index]
    pts_i <- pts_i%>%filter(abs(d_signed)<(dmin_component/2))
    pts_now <- bind_rows(pts_cmp,pts_i)
  } else {
    pts_now <- pts_cmp
  }
  pts_now <- pts_now%>%distinct(xy_index,.keep_all = TRUE)
  pts_all <- rbind(pts_all, pts_now)
}
pts_all <- pts_all%>%
  left_join(complete_net_2%>%select(index=graph_index, highway))
stopifnot(0==(pts_all%>%filter(is.na(highway))%>%nrow()))
#sf::st_write(pts_all%>%filter(component>1)%>%st_as_sf(coords=c("x", "y")), "~/tmp/my_points.kml", delete_layer = TRUE)


## connect components
cmps_to_join <- pts_all$component%>%table%>%sort(decreasing = TRUE)%>%names%>%as.numeric()
print(cmps_to_join)
complete_net_all <- complete_net_2%>%filter(component==cmps_to_join[1])
for (j in cmps_to_join[-1]) {
  print(j)
  complete_net_j <- complete_net_2%>%
    filter(component==j)%>%
    ### we break long edges of components (except main component)
    ### since later we will connect components by vertices
    break_long_edges(max_d=dmin_component)
  vertices_j <- dodgr_vertices(complete_net_j)
  complete_net_all <- add_nodes_to_graph_by_edge(
    graph=complete_net_all,
    xy=vertices_j[,c("x","y")],
    intersections_only=FALSE,
    wt_profile = "motorcar", highway = "artificial", wt_profile_file = "data-raw/profile.json",
    xy_id=vertices_j$id, max_distance=100, replace_component = FALSE)%>%
    bind_rows(complete_net_j)
}

## calculate component
complete_net_all <- complete_net_all%>%
  select(-component)%>%
  dodgr::dodgr_components()

table(complete_net_all$component,complete_net_all$highway%in%waterways)


## create edges to the points and agencias
## separe edges to water and to roads
tract_agencias <- bind_rows(
  tracts%>%transmute(id=paste0("setor_", setor), x=setor_lon, y=setor_lat),
  agencias%>%transmute(id=paste0('agencia_', agencia_codigo), x=agencia_lon, y=agencia_lat)
)
complete_connected_w <- complete_net_all%>%
  filter(component==1)%>%
  filter(highway%in%waterways)
if(nrow(complete_connected_w)>0) {
  ## FIX: artificial connections to rivers have to be (much) more expensive than connections to roads
  complete_connected_w <- complete_connected_w%>%
    add_nodes_to_graph_by_edge(xy=tract_agencias, wt_profile = "motorcar", highway = "artificial_river", wt_profile_file = "data-raw/profile.json", xy_id = tract_agencias$id )
}
complete_connected_r <- complete_net_all%>%
  filter(!highway%in%waterways)%>%
  filter(component==1)
gc()


complete_connected_r <- complete_connected_r%>%
  add_nodes_to_graph_by_edge(xy=tract_agencias, wt_profile = "motorcar", highway = "artificial_road", wt_profile_file = "data-raw/profile.json", xy_id = tract_agencias$id)


complete_connected <- bind_rows(complete_connected_w,complete_connected_r)%>%
  select(-component)%>%
  dodgr::dodgr_components()
stopifnot(max(complete_connected$component)==1)
rm(complete_connected_w)
rm(complete_connected_r)
gc()

pts_connected <- match_pts_to_graph (complete_connected, tracts, distances = TRUE)
pts_connected$component <- complete_connected[pts_connected$index, "component"]
# sf::st_write(pts_connected%>%filter(component>1)%>%st_as_sf(coords=c("x", "y")), "~/tmp/my_points.kml", delete_layer = TRUE)
# sf::st_write(pts_connected%>%st_as_sf(coords=c("x", "y")), "~/tmp/all_points.kml", delete_layer = TRUE)

readr::write_rds(tracts, fname_tracts)
readr::write_rds(pts_connected, fname_pts)
readr::write_rds(complete_connected, fname_complete)

