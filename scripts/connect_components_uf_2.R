source("scripts/connect_components_uf_0.R")



complete_connected <- readr::read_rds(fname_complete)
pts_connected <- readr::read_rds(fname_pts)
tracts <- readr::read_rds(fname_tracts)

stop()

paths_agencias_0 <- dodgr_paths(complete_connected
  , 
  #from = agencias%>%filter(grepl("HUMA", agencia_nome))%>%st_coordinates(),
  #to = agencias%>%filter(grepl("CAR", agencia_nome))%>%st_coordinates(),
  from = agencias%>%head(1)%>%st_coordinates(),
  to = agencias%>%tail(1)%>%st_coordinates(),
  vertices=FALSE
)
table(complete_connected[unlist(paths_agencias_0),"highway"])

d_agencias_setores_nearest <- dodgr_dists_nearest(
  graph = complete_connected%>%
    transmute(edge_id, from_id, from_lat, from_lon, 
              to_id, to_lat, to_lon, 
              ## use time_weighted as d, so we find the closest agency
              ## using time weights
              d=time_weighted), 
                                                  from = pts_connected%>%select(x,y), 
                                                  to=agencias%>%
                                                    st_coordinates(), shortest=TRUE)


 
 
tmp <- dodgr_paths(complete_connected%>%
                     transmute(edge_id, 
                               from_id, from_lat, from_lon, 
                               to_id, to_lat, to_lon, 
                               d=time_weighted, 
                               highway, edge_type=case_when(highway%in%waterways ~ "waterway",
                                                            highway%in%"artificial" ~ "artificial",
                                                            TRUE ~ 'road'
                               )), from=
                     pts_connected[243,c("x", "y")]
                   , to=
                     pts_connected[680,c("x", "y")]
                   , vertices = FALSE)

#tmpg <- complete_connected[complete_connected$edge_id%in%unlist(tmp),]
tmpg <- complete_connected[unlist(tmp),]
tracts%>%st_write("~/tmp/my_tracts.kml", delete_layer = TRUE)
tmpg%>%select(from_lon, from_lat)%>%st_as_sf(coords=c("from_lon", "from_lat"))%>%st_write("~/tmp/my_path.kml", delete_layer = TRUE)


dist_setor_agencia_cat <- dodgr_dists_categorical(
  complete_connected%>%
    filter(!highway%in%waterways)%>%
    transmute(edge_id, 
              from_id, from_lat, from_lon, 
              to_id, to_lat, to_lon, 
              d=time_weighted, 
              highway, edge_type=case_when(highway%in%waterways ~ "waterway",
                               highway%in%"artificial" ~ "artificial",
                               TRUE ~ 'road'
                               )),
                to = agencias%>%st_coordinates()%>%head(),
                from = tracts%>%st_coordinates()%>%head(),
  proportions_only=FALSE
                )


dist_setor_agencia <- dodgr_dists(
  complete_connected, 
  to = agencias%>%st_coordinates(), 
  from = tracts%>%st_coordinates()
)

times_setor_agencia_w <- dodgr_times(
  complete_connected%>%select(from_id, from_lat, from_lon, to_id, to_lat, to_lon, d, time_weighted),
  to = agencias%>%st_coordinates(), 
  from = tracts%>%st_coordinates()
)

tracts_sel <- tracts%>%arrange(setor_lon)%>%head(1)



paths_setor_agencia_0 <- dodgr_paths(
  complete_connected%>%transmute(from_id, to_id, from_lon, from_lat, to_lon, to_lat, d=d_weighted), 
  to = agencias%>%st_coordinates(), 
  from = tracts_sel%>%st_coordinates(), 
  #from = c(-71.88958523509078,-9.208782799862957),
  vertices=FALSE
)
paths_setor_agencia <- paths_setor_agencia_0[[1]][[1]]

complete_map_sf <- readr::read_rds(complete_sf_fname)
ggplot(complete_map_sf, aes(color=highway%in%waterways))+
  geom_sf(alpha=1/2)+
  geom_sf(data=tracts, color="black", alpha=1/10)+
  geom_sf(data=tracts_sel, color="black", alpha=1/2)+
  geom_sf_label(data=agencias, color="black", aes(label=agencia_nome), size=2)+
  ggthemes::theme_map()+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat),
               data=complete_connected[unlist(paths_setor_agencia),], color="purple")+
  geom_label(aes(x=lon, y=lat, label=component), data=complete_connected%>%group_by(component)%>%summarise(lat=mean(to_lat+from_lat)/2, lon=mean(to_lon+from_lon)/2), color="black")


(complete_connected[unlist(paths_setor_agencia),])%>%
  group_by(highway)%>%
  summarise(d_km=sum(d)/1000,
            t_h=sum(time)/60/60,
            v=d_km/t_h)%>%
  arrange(-d_km)


stop()












# complete_map_sf_with_comp <- complete_map_sf%>%
#   left_join(complete_net%>%distinct(id=way_id, component))

## connect points to network
complete_connected_0 <- complete_net%>%
  filter(highway%in%waterways)%>%
  add_nodes_to_graph_by_edge(xy=tracts%>%select(id=setor), xy_id = tracts$setor, 
                             highway = "artificial_river", wt_profile = "motorcar", wt_profile_file = "data-raw/profile.json", replace_component=FALSE)

complete_connected_1 <- complete_net%>%
  filter(!highway%in%waterways)%>%
  add_nodes_to_graph_by_edge(xy=tracts%>%select(id=setor), xy_id = tracts$setor, 
                             highway = "artificial_road", wt_profile = "motorcar", wt_profile_file = "data-raw/profile.json", replace_component=FALSE)

  
complete_connected <- bind_rows(complete_connected_0%>%
                                  mutate(edge_id=paste0("w_", edge_id)),
                                complete_connected_1%>%
                                  mutate(edge_id=paste0("r_", edge_id))
                                )

agencias <- orce::agencias_bdo%>%filter(substr(agencia_codigo,1,2)==uf$uf_codigo)

ggplot(complete_map_sf, aes(color=highway%in%waterways))+
  geom_sf()+
  geom_sf(data=tracts, color="black", alpha=1/5)+
  geom_sf_label(data=agencias, color="black", aes(label=agencia_nome), size=2)+
  ggthemes::theme_map()

# dist_setor_agencia_ag <- dodgr_dists_categorical(
#   complete_connected%>%
#     mutate(edge_type=case_when(highway%in%waterways ~ "waterway",
#                                highway%in%"artificial" ~ "artificial",
#                                TRUE ~ 'road'
#                                )), 
#                 to = agencias%>%st_coordinates(), 
#                 from = tracts%>%st_coordinates(),
#   proportions_only=TRUE
#                 )

dist_setor_agencia <- dodgr_dists(
  complete_connected, 
  to = agencias%>%st_coordinates(), 
  from = tracts%>%st_coordinates()
)

times_setor_agencia <- dodgr_times(
  complete_connected, 
  to = agencias%>%st_coordinates(), 
  from = tracts%>%st_coordinates()
)

tracts_sel <- tracts%>%arrange(setor_lon)%>%head()

paths_setor_agencia <- dodgr_paths(
  complete_connected, 
  to = agencias%>%st_coordinates(), 
  from = tracts_sel%>%st_coordinates(), 
  vertices=FALSE
)


ggplot(complete_map_sf, aes(color=highway%in%waterways))+
  geom_sf(alpha=1/2)+
  geom_sf(data=tracts, color="black", alpha=1/10)+
  geom_sf(data=tracts_sel, color="black", alpha=1/2)+
  geom_sf_label(data=agencias, color="black", aes(label=agencia_nome), size=2)+
  ggthemes::theme_map()+
  geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat),
  data=complete_connected[unlist(paths_setor_agencia[[1]][[1]]),], color="purple")
+
  geom_label(aes(x=lon, y=lat, label=component), data=complete_net%>%group_by(component)%>%summarise(lat=mean(to_lat+from_lat)/2, lon=mean(to_lon+from_lon)/2), color="black")


(complete_connected[unlist(paths_setor_agencia[[1]]),])%>%
  group_by(highway)%>%
  summarise(d_km=sum(d)/1000,
            t_h=sum(time)/60/60,
            v=d_km/t_h)%>%
  arrange(-d_km)


stop()
comps_0 <- complete_connected_0%>%
  filter(highway=="artificial")%>%
  count(component)%>%
  mutate(p=n/sum(n))%>%
  arrange(-p)
comps <- comps_0%>%
  pull(component)%>%unique%>%sort

complete_connected <- vector(mode="list", length=length(comps))
connected_tracts <- data.frame()
disconnected_tracts <- tracts%>%select(id=setor)
i <- 1
ml <- 25
while((nrow(disconnected_tracts)>0)&(comp<=max(comps))) {
  comp <- comps[i]
  print(comp)
  complete_connected[[comp]] <- add_nodes_to_graph2(complete_net%>%filter(component==comp), xy = disconnected_tracts, intersections_only = FALSE, new_edge_type = "artificial", wt_profile = "motorcar", wt_profile_file = "data-raw/profile.json", max_length = ml)
  connected_tracts <- rbind(connected_tracts, disconnected_tracts%>%semi_join(complete_connected[[comp]], by=c("id"='from_id')))
  disconnected_tracts <- disconnected_tracts%>%anti_join(connected_tracts%>%st_drop_geometry(), by="id")
  i <- i+1
  if (comp>=max(comps)){
    stop()
    i <- 1
    ml <- ml*2
    complete_connected <- vector(mode="list", length=length(comps))
    connected_tracts <- data.frame()
    disconnected_tracts <- tracts%>%select(id=setor)
  }
}
complete_connected1 <- add_nodes_to_graph2(complete_net%>%filter(component==1), xy = tracts%>%select(id=setor), intersections_only = FALSE, new_edge_type = "artificial", wt_profile = "motorcar", wt_profile_file = "data-raw/profile.json", max_length = 25)
disconnected_tracts <- tracts%>%filter(!setor%in%complete_connected1$from_id)

complete_connected2 <- add_nodes_to_graph2(complete_net%>%filter(component==2), xy = disconnected_tracts%>%select(id=setor), intersections_only = FALSE, new_edge_type = "artificial", wt_profile = "motorcar", wt_profile_file = "data-raw/profile.json", max_length = 25)



tract_components <- complete_connected1%>%filter(highway=="artificial")%>%distinct(component)%>%pull(component)%>%unique

## plot disconnected components relevant to the tracts
ggplot(data=complete_map_sf_with_comp%>%
         filter(component%in%tract_components))+
  geom_sf(aes(color=factor(component), alpha=component!=1))


## distances from unconnected tracts to component 1
tract_with_components <- tracts%>%
  left_join(complete_connected%>%distinct(setor=from_id, component))
tract_out <-tract_with_components[tract_with_components$component!=1, ]
comp1 <- complete_connected[complete_connected$component==1, ]
g_dist <- dodgr::match_pts_to_graph(graph = comp1, tract_out%>%st_coordinates(), distances = TRUE)
g_closest <- comp1[g_dist$index,]


complete_sf1 <- complete_sf[complete_sf$component==1,]
grw_p0_r <- connect_points_to_network(points_sf = tracts_points
                                      , lines_sf = complete_sf1[!complete_sf1$highway%in%valid_waterways,],
                                      way_id_column = "id", track_memory = TRUE)
grw_p0_w <- connect_points_to_network(points_sf = tracts_points
                                      , lines_sf = complete_sf1[complete_sf1$highway%in%valid_waterways,],
                                      way_id_column = "id", track_memory = TRUE)

grw_p0_rw <- bind_rows(grw_p0_r$complete_network, grw_p0_w$complete_network%>%mutate(id=paste0("w",id)))
grw_p0_rw <- grw_p0_rw%>%distinct(geometry, highway, id)

grw_p2_rw <- bind_rows(grw_p0_r$complete_network2, grw_p0_w$complete_network2%>%mutate(id=paste0("w",id)))
grw_p2_rw <- grw_p2_rw%>%distinct(geometry, highway, id)


rm(grw_p0_r, grw_p0_w,complete_sf1)
gc()

## dodgr net with connections
grw_p1 <- weight_streetnet (grw_p0_rw,
                            wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")
## FIX: pq tem mais components mesmo filtrando pelo primeiro componente?
table(grw_p1$component)
prop.table(table(grw_p1$component))%>%head()


grw_p2 <- weight_streetnet (grw_p2_rw,
                            wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")
## FIX: pq tem mais components mesmo filtrando pelo primeiro componente?
table(grw_p2$component)
prop.table(table(grw_p2$component))%>%head()

stop()

grw_connected1 <- grw_p1[grw_p1$component==1,]

grw_connected1_sf <- grw_p0_rw[grw_p0_rw$id%in%unique(grw_connected1$way_id),]

# grw_connected1_sf_r <- grw_connected1_sf[!grw_connected1_sf$highway%in%valid_waterways,]
#
# ## connect points to road network
# grw_connected2_sf_r <- connect_points_to_network(points_sf = tracts_points
#                                     , lines_sf = grw_connected1_sf_r,
#                                     way_id_column = "id", track_memory = TRUE)
#
# grw_connected1_sf_w <- grw_connected1_sf[grw_connected1_sf$highway%in%valid_waterways,]
#
# grw_connected2_sf_w <- connect_points_to_network(points_sf = tracts_points
#                                                  , lines_sf = grw_connected1_sf_w,
#                                                  way_id_column = "id", track_memory = TRUE)
#
# grw_connected2_sf <- grw_connected2_sf_w$complete_network%>%
#   bind_rows(grw_connected2_sf_r$complete_network)%>%
#   distinct(geometry, highway, feature_id, .keep_all = TRUE)

# grw_connected2 <- weight_streetnet (grw_connected2_sf,
#                             wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id", keep_cols = c('feature_id'))


## connect points (Fix problem)
# Error in `$<-.data.frame`(`*tmp*`, "d_weighted", value = c(0, 0, 0, 0)) :
#   replacement has 4 rows, data has 2
# tmp <- add_nodes_to_graph(graph = grw_connected1, xy = tracts_points, intersections_only = FALSE, dist_tol = 1)

## calcula distancias para agencias
agencias <- orce::agencias_bdo%>%filter(substr(agencia_codigo,1,2)==ufnow$uf_codigo)

ggplot(complete_sf, aes(color=recode_way(highway))) +
  geom_sf()+
  geom_sf(data=grw_connected1_sf%>%filter(highway=="artificial")%>%mutate(color=recode_way(highway)))+
  geom_sf(data=uf, fill="transparent", lwd=2, color="black") +
  geom_sf(data=tracts_points, color="black", alpha=1/10, pch=1) +
  #geom_sf(data=tracts_points, color="black", pch=".", alpha=1/2, size=10) +
  geom_sf_label(data=agencias, color="black", aes(label=agencia_nome), size=2)+
  ggthemes::theme_map()




# p <- tracts_points[(!tracts_points$from_id%in%d_agencias_setores$from),]

# grw_connected1_r <- grw_connected1[!grw_connected1$highway%in%valid_waterways,]
# #Calculate vector of shortest distances from a series of 'from' points to nearest one of series of 'to' points.
agencias$vertice_id <- grw_connected1[match_points_to_graph(grw_connected1, xy=agencias),"from_id"]

## only one agencias per vertice
## FIX: deal with these cases
agencias <- agencias%>%
  distinct(vertice_id, .keep_all = TRUE)


tracts_points$vertice_id <- grw_connected1[match_points_to_graph(grw_connected1, xy=tracts_points),"from_id"]
## only one setor per vertice
## FIX: deal with these cases
dim(tracts_points)
tracts_points <- tracts_points%>%group_by(vertice_id)%>%
  add_count(name="n_setores_vertice_id")
tracts_points <- tracts_points%>%distinct(vertice_id, .keep_all = TRUE)
dim(tracts_points)

## agencias mais proximas
#  If an additional column named weight or wt is present, shortest paths are calculated according to values specified in that column; otherwise according to dist values.
## FIX: needs subsetting and selecting just necessary variables
## otherwise gives wrong results

grw_connected1_save <- grw_connected1
if (ufnow$uf_codigo=="13") {
  p <- tracts_points%>%filter(setor=="130014405000001")#
  agencia1 <- agencias%>%filter(agencia_codigo=="130170400")
  agencia2 <- agencias%>%filter(agencia_codigo=="130110000")
  grw_connected1 <- grw_connected1_save[, c(
    "from_id", "to_id",
    "from_lon", "from_lat", "to_lon", "to_lat")]
  grw_connected1$dist <- grw_connected1_save$d
  d_agencias_setores_01ll <- dodgr_dists_nearest(graph = grw_connected1, to = p%>%st_coordinates(), from=agencia1%>%st_coordinates(), shortest=TRUE)
  ##Fix: different results using edge or lat lon!!!
  d_agencias_setores_01 <- dodgr_dists_nearest(graph = grw_connected1, to = p$vertice_id, from=agencia1$vertice_id, shortest=TRUE)
  d_agencias_setores_02 <- dodgr_dists_nearest(graph = grw_connected1, to = p$vertice_id, from=agencia2$vertice_id, shortest=TRUE)
  grw_connected1$weight <- grw_connected1_save$time_weighted
  d_agencias_setores_11 <- dodgr_dists_nearest(graph = grw_connected1, to = p$vertice_id, from=agencia1$vertice_id, shortest=TRUE)
  d_agencias_setores_12 <- dodgr_dists_nearest(graph = grw_connected1, to = p$vertice_id, from=agencia2$vertice_id, shortest=TRUE)
  stop()
}


#d_agencias_setores_0 <- dodgr_dists_nearest(graph = grw_connected1, to = agencias$vertice_id, from=tracts_points$vertice_id)
d_agencias_setores_0 <- dodgr_dists_nearest(graph = grw_connected1, to = agencias%>%st_coordinates(), from=tracts_points%>%st_coordinates())

class(grw_connected1) <- "data.frame"
agencias_vertices <- tibble(vertice_id=unique(d_agencias_setores_0$to))%>%left_join(grw_connected1%>%distinct(vertice_id=from_id, from_lon, from_lat))%>%
  st_as_sf(coords=c('from_lon', 'from_lat'))
st_crs(agencias_vertices) <- st_crs(agencias)
agencias_vertices2 <- bind_cols(agencias[agencias_vertices%>%
                                           st_nearest_feature(agencias),],agencias_vertices%>%rename(vertice_id2=vertice_id)%>%st_drop_geometry())
# agencias_vertices2$vertices2 <- dodgr::match_pts_to_verts(dodgr_vertices(grw_connected1), xy = agencias, connected = FALSE)

agencias$vertice_id <-
  st_nearest_points(agencias, d_agencias_setores_0%>%distinct())
grw_connected1[match_points_to_graph(grw_connected1, xy=agencias),"from_id"]


d_agencias_setores <- d_agencias_setores_0%>%
  left_join(agencias%>%select(agencia_codigo, vertice_id)%>%sf::st_drop_geometry(),by=c("to"="vertice_id"))%>%
  left_join(tracts_points%>%distinct(setor, vertice_id)%>%sf::st_drop_geometry(),by=c("from"="vertice_id"))%>%
  mutate(municipio_codigo=substr(setor,1,7))

d_agencias_setores_ag <- d_agencias_setores%>%
  group_by(municipio_codigo=substr(setor,1,7))%>%
  count(agencia_codigo)%>%
  arrange(desc(n))%>%
  slice(1)

## agencias jurisdição

d_agencia_jurisdicao_setores <- dodgr_distances(graph = grw_connected1, to = agencias$vertice_id, from=tracts_points$vertice_id, shortest=TRUE)%>%
  as.table()%>%
  as.data.frame(stringsAsFactors = FALSE)%>%
  setNames(c("vertice_id_setor", "vertice_id_agencia", "distancia"))%>%
  left_join(agencias%>%
              sf::st_drop_geometry()%>%
              transmute(agencia_codigo, vertice_id_agencia=vertice_id))%>%
  left_join(tracts_points%>%
              sf::st_drop_geometry()%>%
              transmute(setor, vertice_id_setor=vertice_id))%>%
  mutate(municipio_codigo=substr(setor,1,7))%>%
  ## filtra agência de jurisdição
  semi_join(orce::agencias_bdo_mun%>%sf::st_drop_geometry()%>%select(agencia_codigo, municipio_codigo))


# do this to get a summary of the (water/road/artificial) used
# takes a while, though
# grw_connected1$edge_type <- recode_way(grw_connected1$highway)
# d_agencia_jurisdicao_setores_cat <- dodgr_dists_categorical(graph = grw_connected1, to = agencias$vertice_id, from=tracts_points$vertice_id)


d <- d_agencia_jurisdicao_setores%>%
  mutate(municipio_codigo=substr(setor,1,7))%>%
  distinct(setor, municipio_codigo, agencia_codigo_jurisdicao=agencia_codigo)%>%
  left_join(
    d_agencias_setores%>%select(agencia_codigo_proxima=agencia_codigo, setor), by="setor")%>%
  mutate(agencia_diferente=agencia_codigo_proxima!=agencia_codigo_jurisdicao)%>%
  left_join(agencias%>%sf::st_drop_geometry()%>%select(agencia_codigo_jurisdicao=agencia_codigo, agencia_nome_jurisdicao=agencia_nome), by="agencia_codigo_jurisdicao")%>%
  left_join(agencias%>%sf::st_drop_geometry()%>%select(agencia_codigo_proxima=agencia_codigo, agencia_nome_proxima=agencia_nome), by="agencia_codigo_proxima")


tracts_points%>%
  inner_join(d, by="setor") ->
  setores_agencias_diferentes





ggplot(complete_sf) +
  geom_sf(aes(color=recode_way(highway)), alpha=1/3, linetype=2)+
  geom_sf(data=grw_connected1_sf, aes(color=recode_way(highway)), alpha=2/3)+
  #geom_sf(data=grw_p0$complete_network%>%filter(highway=="artificial")%>%mutate(color=recode_way(highway)))+
  geom_sf(data=uf, fill="transparent", lwd=2, color="black") +
  geom_sf(data=setores_agencias_diferentes, aes(color=agencia_diferente))+
  #geom_sf(data=tracts_points, color="black", pch=".", alpha=1/2, size=10) +
  geom_sf_label(data=agencias, color="black", aes(label=agencia_nome), size=2)+
  ggthemes::theme_map()


dnow_0 <- setores_agencias_diferentes%>%
  sf::st_drop_geometry()
dnow_1 <- dnow_0%>%
  filter(municipio_codigo=='1302702')
dnow <- setores_agencias_diferentes%>%semi_join(
  dnow_0%>%
    semi_join(dnow_1, by="agencia_codigo_proxima")%>%
    semi_join(dnow_1, by="agencia_codigo_jurisdicao"),by="setor")
agencias_sel <- agencias%>%filter(agencia_codigo%in%unique(c(dnow$agencia_codigo_proxima, dnow$agencia_codigo_jurisdicao)))

ggplot(data=dnow) +
  geom_sf(aes(shape=paste(agencia_nome_jurisdicao, agencia_nome_proxima)))+
  geom_sf_label(data=agencias_sel
                , aes(label=agencia_nome), color="black")+
  #geom_sf(data=uf, fill="transparent", lwd=2, color="black")+
  labs(title=dnow$agencia_nome_jurisdica[1])+
  geom_sf(aes(color=recode_way(highway)), alpha=1/3, linetype=1, data=grw_connected1_sf%>%filter(highway!="artificial"))+
  coord_sf(xlim = range(c(st_coordinates(dnow)[,1], st_coordinates(agencias_sel)[,1])),
           ylim=range(c(st_coordinates(dnow)[,2], st_coordinates(agencias_sel)[,2])), expand=10)+
  scale_color_viridis_d()

dnow_sel <- dnow%>%filter(setor_lat< -6.8, setor_lat> -7.2, setor_lon> -60, agencia_codigo_jurisdicao!=agencia_codigo_proxima)%>%head(1)
# +
#   geom_sf(data=grw_p0$complete_network%>%filter(highway=="artificial")%>%semi_join(dnow%>%sf::st_drop_geometry()), color="red")

# %>%
#   as.table()%>%
#   as.data.frame(stringsAsFactors = FALSE)%>%
#   setNames(c("vertice_id_setor", "vertice_id_agencia", "distancia"))%>%
#   left_join(agencias%>%
#               sf::st_drop_geometry()%>%
#               transmute(agencia_codigo, vertice_id_agencia=vertice_id))%>%
#   left_join(tracts_points%>%
#               sf::st_drop_geometry()%>%
#               transmute(setor, vertice_id_setor=vertice_id))%>%
#   mutate(municipio_codigo=substr(setor,1,7))%>%
#   semi_join(orce::agencias_bdo_mun%>%sf::st_drop_geometry()%>%select(agencia_codigo, municipio_codigo))


## calculate path
path1 <- dodgr::dodgr_paths(grw_connected1, from = dnow_sel$vertice_id, to=agencias_sel$vertice_id[1], vertices=FALSE)
path2 <- dodgr::dodgr_paths(grw_connected1, from = dnow_sel$vertice_id, to=agencias_sel$vertice_id[2], vertices=FALSE)
d1 <- grw_connected1[unlist(path1),]
d1$seq <- 1:nrow(d1)
d2 <- grw_connected1[unlist(path2),]
d2$seq <- 1:nrow(d2)


# d_ag <- dodgr::dodgr_distances(grw_connected1, from=st_coordinates(dnow_sel), to=st_coordinates(agencias_sel))
# d_ag1 <- dodgr::dodgr_distances(grw_connected1, from=st_coordinates(dnow_sel), to=st_coordinates(agencias_sel)[1,])


library(ggplot2)
ggplot(data=grw_connected1_sf)+
  #geom_sf(aes(color=recode_way(highway)), alpha=1/5) +
  #scale_color_manual(values=c("gray", "pink"))+
  #geom_sf(data=map_connected_p$artificial_edges, color="purple", linewidth=1)+
  #geom_sf(data=p, color="lightblue", alpha=1/5)+
  #geom_sf(data=cp$artificial_edges, color="red", linetype=2)+
  geom_sf(data=dnow_sel, color="blue", size=1) +
  geom_sf(data=agencias_sel, color="red", size=3)+
  geom_sf_label(data=agencias_sel, aes(label=agencia_nome))+
  geom_segment(data=grw_connected1[unlist(path),], color="gray", linewidth=2, alpha=1/4,
               aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat))
table(grw_connected1[unlist(path),"highway"])


#agencias <- tracts_points%>%head(2)



grw_connected2 <- grw_connected1
#grw_connected2$weight <- grw_connected2$time_weighted
path <- dodgr::dodgr_paths(grw_connected2, from = st_coordinates(p), to=st_coordinates(agencias), vertices=FALSE)

d <- grw_connected2[unlist(path),]
d$seq <- 1:nrow(d)


ggplot(data=grw_p0$complete_network%>%
         mutate(
           way=recode_way(highway),
           is_waterway=!is.na(waterway),
           artificial=coalesce(artificial, FALSE)))+
  geom_sf(aes(linetype=is_waterway, color=way), alpha=1/5)+
  #scale_color_manual(values=c("gray", "pink"))+
  geom_sf(data=agencias%>%mutate(is_waterway=FALSE))+
  #geom_sf(data=map_connected_p$artificial_edges, color="purple", linewidth=1)+
  #geom_sf(data=p, color="lightblue", alpha=1/5)+
  #geom_sf(data=cp$artificial_edges, color="red", linetype=2)+
  geom_sf(data=p, color="blue", size=1) +
  #geom_sf(data=p[c(1,nrow(p)), ], color="red", size=3)+
  geom_segment(data=(grw_connected1[unlist(path),])%>%as_tibble()%>%mutate(is_waterway=highway%in%c("river", "canal"), way=case_match(highway,
                                                                                                                                      c("river", "canal") ~ "waterway",
                                                                                                                                      "artificial" ~ "artificial",
                                                                                                                                      .default = "road")),
               #color="red",
               linewidth=1, alpha=1,
               aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat, color=way))

table(grw_connected1[unlist(path),"highway"])



# highways_sf_simple <- st_simplify_net(highways_sf)
# highways_v_simple <- st_extract_vertices(highways_sf_simple)
#
#
# waterways_sf_simple <- st_simplify_net(waterways_sf)
#
#
# waterways_sf$id <- 1:nrow(waterways_sf)
# waterways_sf$highway <- waterways_sf$waterway
# waterways_v <- spatialEco::extract.vertices(waterways_sf)
#
# waterways_sf <- waterways_sf%>%
#   filter(highway%in%wibge$weighting_profiles$way)
#

# highways_net <- weight_streetnet (highways_sf,
#                                   wt_profile_file = "profile.json",
#                                   wt_profile="motorcar",
#                                   id_col = "id")
# waterways_net <- weight_streetnet (waterways_sf,
#                                    wt_profile_file = "profile.json",
#                                    wt_profile="motorcar",
#                                    id_col = "id")
#
# waterways_vertices_0 <- dodgr::dodgr_vertices(waterways_net)%>%
#   sf::st_as_sf(coords=c("x", "y"))
# highways_vertices_0 <- dodgr::dodgr_vertices(highways_net)%>%
#   sf::st_as_sf(coords=c("x", "y"))


# waterways_links <- st_nearest_feature(waterways_vertices_0, highways_vertices_0)
# waterways_links_dists <- sf::st_distance(waterways_vertices_0, highways_vertices_0[waterways_links,], by_element = TRUE)

# waterways_vertices <- waterways_vertices_0%>%
#   group_by(component)%>%
#   slice(as.integer(seq(1,n(), length=pmax(1, n()/1000))))
#
# pts_0 <- match_pts_to_graph(highways_net, st_coordinates(waterways_vertices), distances = TRUE)%>%
#   ungroup%>%
#   mutate(row_id=1:n())
# pts <- pts_0%>%filter(abs(d_signed)<1000
#                       , abs(d_signed)>0
#                       )
# hw_net <- add_nodes_to_graph_labeled(highways_net, st_coordinates(waterways_vertices)[pts$row_id,], )




## points to connect

p0 <- tracts_points%>%arrange(setor_lat)
p1 <- tracts_points%>%arrange(setor_lon)
p <- bind_rows(#head(p0), tail(p0),
  head(p1), tail(p1))%>%unique()


## connect points to network
# grw_p0 <- connect_points_to_network(points_sf = p
#                                     , lines_sf = complete_sf,
#                                     way_id_column = "id", track_memory = TRUE)

## idea: connect all components that are assigned to points
## components to connect
v <- unique(c(table(grw_p0$complete_network$component)%>%sort()%>%tail(2)%>%names,grw_p0$artificial_edges$component))

## filter lines_sf
filtered_sf <- grw_p0$complete_network[grw_p0$complete_network$component%in%v,]

## dodgr net
# grw_filtered <- weight_streetnet (filtered_sf, wt_profile = wibge, id_col = "id")


## connect components
# tic()
# tmp2 <- connect_network_components(lines_sf = filtered_sf, dodgr_net = grw_filtered, way_id_column = "id", track_memory = TRUE)
# toc()


## new dodgr net with the connected components

## dodgr net
grw_connected <- weight_streetnet (tmp2$complete_network, wt_profile = wibge, id_col = "id")

## Fix: for some reason (investigate), it finds more than one component, despite connections.
table(grw_connected$component)
## keeping the first component
stop()
grw_connected1 <- grw_connected[grw_connected$component==1,]

## calculate path
# path <- dodgr::dodgr_paths(grw_connected1, from = st_coordinates(p[1,]), to=st_coordinates(p[nrow(p),]), vertices=FALSE)
#
# d <- grw_connected1[unlist(path),];d$seq <- 1:nrow(d)
#
# ## calculate distance
# dodgr::dodgr_distances(grw_connected1, from=st_coordinates(p[1,]), to=st_coordinates(p[nrow(p),]))
#
# library(ggplot2)
# ggplot(data=tmp2$complete_network%>%
#          mutate(
#            is_waterway=!is.na(waterway),
#            artificial=coalesce(artificial, FALSE))) +
#   geom_sf(aes(color=waterway))+
#   scale_color_manual(values=c("gray", "pink"))+
#   #geom_sf(data=map_connected_p$artificial_edges, color="purple", linewidth=1)+
#   geom_sf(data=p, color="lightblue", alpha=1/5)+
#   #geom_sf(data=cp$artificial_edges, color="red", linetype=2)+
#   geom_sf(data=p, color="blue", size=1) +
#   geom_sf(data=p[c(1,nrow(p)), ], color="red", size=3)+
#   geom_segment(data=grw_connected1[unlist(path),], color="red", linewidth=1, alpha=1/3,
#                aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat))
# stop()



library(tictoc)

# print(v)
# print(length(v))

grw_p0 <- connect_points_to_network(points_sf = tracts_points#%>%slice_sample(n=100)
                                    , lines_sf = complete_sf,
                                    way_id_column = "id", track_memory = TRUE)

grw_p1 <- weight_streetnet (grw_p0$complete_network, wt_profile = "motorcar", wt_profile_file = "profile.js", id_col = "id")





grw <- grw_p1

comps <- grw%>%group_by(component)%>%summarise(d=sum(d))%>%
  arrange(component)%>%
  mutate(p=d/sum(d), cp=cumsum(p))
v <- comps%>%
  filter(cp<.95)%>%
  pull(component)
if(length(v)==0) v <- 1

library(tictoc)

print(v)
print(length(v))

grw_p0 <- connect_points_to_network(points_sf = tracts_points%>%slice_sample(n=100)
                                    , lines_sf = complete_sf,
                                    way_id_column = "id", track_memory = TRUE)

grw_p1 <- weight_streetnet (grw_p0$complete_network, wt_profile = wibge, id_col = "id")




tic()
tmp2 <- connect_network_components(lines_sf = complete_sf, dodgr_net = grw[grw$component%in%v,], way_id_column = "id", track_memory = TRUE)
toc()





g <- grw[grw$component%in%v,]
p <- ggplot(data=g) +
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component), alpha=!highway%in%c("river", "canal")))+
  #guides(color="none")+
  geom_sf(data=tracts_points, alpha=1/10)+
  geom_sf(data=uf, lwd=1, fill="transparent")+
  geom_text(aes(x=lon, y=lat, label=component), data=g%>%group_by(component)%>%summarise(lat=mean(to_lat), lon=mean(to_lon)))
p+theme_minimal()

##testing point connections
tmp3 <- connect_points_to_network(points_sf = tracts_points#%>%slice_sample(n=10)
                                  , lines_sf = tmp2$complete_network,
                                  way_id_column = "id", track_memory = TRUE)

tmp4 <- weight_streetnet (tmp3$complete_network, wt_profile = wibge, id_col = "id")




p <- ggplot(data=grw[grw$component%in%v,]) +
  #geom_sf(data=waterways_sf, color="gray", alpha=.5)+
  geom_sf(data=tmp2$artificial_edges, color="purple", linetype=2, size=10)+
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component), alpha=!highway%in%c("river", "canal")))+
  geom_sf(data=tracts_points, pch="*")
p

##testing point connections
tmp <- connect_points(points = tracts_points%>%slice_sample(n=10), streets = tmp2$complete_network, street_id_col = "id", point_id_col = "setor", waterways = waterways_sf)%>%filter(distance_to_street>1000)

d <- tmp%>%arrange(desc(distance_to_street))%>%slice(10)
bb <- st_bbox(d)
#p <- ggplot(data=tmp%>%arrange(desc(distance_to_street))%>%slice(1))+
p <- ggplot(data=d)+
  geom_sf(data=waterways_sf, color="lightblue", alpha=.8)+
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)), data=grw)+
  geom_sf(data=d, color="purple", linetype=2) +
  geom_sf(data=tracts_points)+  coord_sf(xlim=bb[c(1,3)], ylim=bb[c(2,4)])
p



p <- ggplot(data=grw[grw$component>2,]) +
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)))+
  geom_sf(data=tmp2$artificial_edges)
p
p+geom_point(data=gr[gr$way_id=="1037",][1,], aes(x=from_lon, y=from_lat), size=3, color=scales::alpha("red",.1))

# bb <- st_bbox(tmp2$complete_network)
# p <- ggmap::get_map(location = dodgr_vertices(gr)%>%select(lon=x,lat=y), source = "google", maptype = "roadmap", zoom = 6)
# ggmap::ggmap(p)+
#   geom_point(aes(x=from_lon, y=from_lat, color=factor(component)), data=gr[gr$component%in%v,], pch=".")+
#   geom_path(data=as_tibble(st_coordinates(tmp2$artificial_edges))%>%rename(lon=X, lat=Y)%>%mutate(feature_id=rep(1:(n()/2), each=2)), aes(group=feature_id))







visualize_network_connections(tmp2$complete_network, tmp2$artificial_edges, id_roads = "id")

# stop()
# 
# 
# 
# 
# 
# ggplot(data=tmp2$complete_network[1:10,]) + geom_sf()
# 
# library(furrr)
# plan(multisession, workers = 4)
# 
# 
# 
# 
# 
# 
# 
# # agencies <- orce::agencias_bdo%>%dplyr::filter(grepl(glue::glue("^{ufnow$uf_codigo}"), agencia_codigo))
# 
# library (osmplotr)
# map <- osm_basemap (mc, bg = "gray95") %>%
#   #add_osm_objects (mc, col = "gray5") %>%
#   print_osm_map ()
# 
# 
# vt <- dodgr_vertices (grc)
# ggplot(data=vt%>%filter(component<=6), aes(x=x, y=y, color=factor(component)), alpha=1/3)
# +
#   geom_point() +
#   geom_point(aes(x=agencia_lon, y=agencia_lat), data=agencies, color="black")
# grc1 <- grc[grc$component==1,]
# rm(grc)
# gc()
# 
# gc()
# 
# 
# 
# gc()
# 
# agencies_ll <- agencies%>%sf::st_coordinates()
# rownames(agencies_ll) <- agencies$agencia_codigo
# 
# dd <- dodgr_dists(grc1, to = agencies_ll, from = tracts_points%>%sf::st_coordinates(), shortest = FALSE)
# dt <- dodgr_times(grc1, to = agencies_ll, from = tracts_points%>%sf::st_coordinates(), shortest = FALSE)
# which(is.na(dd), arr.ind = TRUE)
# 
# r <- data.frame(
#   agencia_codigo = rep(agencies$agencia_codigo, each = nrow(tracts)),
#   setor = rep(tracts$setor, times = nrow(agencies)),
#   distancia_km = round(as.vector(dd) / 1000, 2),
#   duracao_horas = round(as.vector(dt) / 60, 2)
# )
# 
# library(tictoc)
# tic()
# cp <- connect_points(streets = mc, points = tracts_points%>%slice_sample(n=30), street_id_col = "id", point_id_col = "setor", waterways = waterways_sf)
# toc()
# 
# plot_connections(connections = cp%>%filter(#!is.na(waterway_type),
#   distance_to_street>500)
#   , streets = mc, street_id_col = "id", tracts_sf = tracts, tract_id="setor")
# 
# 
# stop()
# mc_con <- bind_rows(mc%>%mutate(id=as.character(id)), cp)
# graph <- weight_streetnet (mc_con, wt_profile = "motorcar", id_col = "id")
# 
# dd <- dodgr_dists(grc1_con, to = agencies%>%sf::st_coordinates(), from = tracts%>%sf::st_coordinates(), shortest = FALSE)
# 
# stop()
# 
# p <- tracts%>%inner_join(r, by="setor")%>%filter(is.na(distancia_km))%>%distinct(setor, geometry)
# p_in <- tracts%>%semi_join(r%>%filter(!is.na(distancia_km)), by="setor")%>%distinct(setor, geometry)
# if (nrow(p)>0) {
#   cp <- connect_points(streets = mc, points = p, id_col = "id")
# } else {
#   cp <- mc
# }
# plot_connections(streets = cp, points = p_in%>%head(20), id_col = "id")
