library(tictoc)
library(cli)
tic()
cli::cli_inform(as.character(Sys.time()))
ufsiglanow <- "se"
library(geoarrow)
library(arrow)
library(sf)
library(ggplot2)
library(dplyr)
devtools::load_all("../dodgr")
#devtools::load_all("../dodgrconnect/")
#devtools::load_all()
dodgr_cache_off()


# Value:
# Highway preference
# The highway preference specified by the user is a percentage, these are scaled so that the most preferred highway type has a weighted preference of 1.0 (0% always has a weighted preference of 0.0). The calculated score for a segment is divided by this weighted preference.

wmin <- weighting_profiles$weighting_profiles%>%
  group_by(way)%>%
  summarise(max_speed=min(max_speed, na.rm=TRUE), value=.2)
wibge <- list()
wibge$weighting_profiles <-
  wmin%>%
  rows_upsert(weighting_profiles$weighting_profiles%>%
                filter(name=="motorcar", !is.na(max_speed))%>%
                select(way, value, max_speed), "way")%>%
  rows_upsert(
    bind_rows(
      tibble(way=c("artificial_road", "artificial_river", "ferry"),
             value=c(.0002,.0001,.01),
             max_speed=c(1.5,1,5))), "way")%>%
  mutate(name="motorcar")%>%
  ungroup()
wibge$surface_speeds <- weighting_profiles$surface_speeds%>%
  group_by(key, value, name="motorcar")%>%
  summarise(max_speed=max(max_speed))%>%
  ungroup()
wibge$penalties <- weighting_profiles$penalties%>%
  filter(name=="motorcar")%>%
  mutate(name="motorcar")

wpj <- jsonlite::toJSON(wibge, pretty = TRUE)
writeLines(wpj, "scripts/profile_hw.json")

cli::cli_inform(as.character(Sys.time()))

uf <- orce::ufs%>%
  filter(uf_sigla==toupper(ufsiglanow))
uf_buffer <- st_buffer(readr::read_rds("~/github/tractdistancesbr/data-raw/br_states.rds")%>%
                         filter(abbrev_state==toupper(ufsiglanow)), dist = units::set_units(10000, "m"))%>%
  st_transform(crs = 4326)

agencias <- orce::agencias_bdo%>%filter(substr(agencia_codigo,1,2)==uf$uf_codigo)
setores <- orce::pontos_setores%>%
  filter(substr(setor,1,2)==uf$uf_codigo)
set.seed(1)

cli::cli_inform(as.character(Sys.time()))

sf_fname <- file.path(here::here("data-raw", paste0(tolower(ufsiglanow), "_sf_r.rds")))
net_r1_fname <- file.path(here::here("data-raw", paste0(tolower(ufsiglanow), "_net_r1.rds")))
parquet_fname <- glue::glue("../tractdistancesbr/data-raw/{tolower(uf$uf_sigla)}.parquet")



cli::cli_inform(as.character(Sys.time()))

parquet <- arrow::open_dataset(parquet_fname)%>%
  filter(!is.na(highway))
map_sf <- parquet%>%
  st_as_sf(crs=4326)%>%
  st_intersection(uf_buffer%>%select(geom))%>%
  dodgrconnect::std_geometry()
rm(parquet)

cli::cli_inform(as.character(Sys.time()))

net_r <- dodgr::weight_streetnet(map_sf, id_col = "id", wt_profile_file = "scripts/profile_hw.json", wt_profile = "motorcar")

cli::cli_inform(as.character(Sys.time()))

net_r <- net_r%>%
  mutate(edge_id=as.character(edge_id))%>%
  dodgr::dodgr_components()

cli::cli_inform(as.character(Sys.time()))

net_r1 <- net_r%>%
  filter(component==1)

## create edges to the points and agencias
setores_agencias <- bind_rows(
  setores%>%transmute(id=paste0("setor_", setor), x=setor_lon, y=setor_lat),
  agencias%>%transmute(id=paste0('agencia_', agencia_codigo), x=agencia_lon, y=agencia_lat)
)

net_connected_r1 <- net_r1%>%
  slice_sample(n=2)%>%
  add_nodes_to_graph_multi(xy=setores_agencias%>%head(20), wt_profile = "motorcar", highway = "artificial_road", wt_profile_file = "scripts/profile_hw.json", debug = TRUE, dist_tol = 1, dist_min = 10, intersections_only = FALSE);dim(net_connected_r1);dim(net_connected_r1%>%unique())
                                                                                                                                                                                                                                                   e)
stop()

net_connected_r1 <- net_r1%>%
  head(1)%>%
  add_nodes_to_graph_multi(xy=setores_agencias%>%head(20), wt_profile = "motorcar", highway = "artificial_road", wt_profile_file = "scripts/profile_hw.json", xy_id = setores_agencias$id%>%head(20))

  add_nodes_to_graph_by_edge(xy=setores_agencias%>%head(20), wt_profile = "motorcar", highway = "artificial_road", wt_profile_file = "scripts/profile_hw.json", xy_id = setores_agencias$id%>%head(20))


d_agencias_setores <- dodgr_dists_nearest(graph = net_connected_r1, to = setores%>%head()%>%st_coordinates(), from=agencias%>%st_coordinates(), shortest=TRUE)


toc()
