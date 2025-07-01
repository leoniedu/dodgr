library(dplyr)
library(sf)
ufsiglanow <- "ba"

devtools::load_all(here::here("../dodgr"))

uf <- orce::ufs%>%
  filter(uf_sigla==toupper(ufsiglanow))

agencias <- orce::agencias_bdo%>%filter(substr(agencia_codigo,1,2)==uf$uf_codigo)%>%
  mutate(municipio_codigo=substr(agencia_codigo,1,7))%>%
  ungroup()

setores <- orce::pontos_setores%>%
  filter(substr(setor,1,2)==uf$uf_codigo)%>%
  mutate(municipio_codigo=substr(setor,1,7))%>%
  left_join(orce::agencias_mun)%>%
  ungroup()


net_connected_r1 <- readRDS(here::here(paste0("scripts/", tolower(ufsiglanow), "_connected_1.rds")))




setores_agencias <- bind_rows(
  setores%>%
    transmute(id=paste0("setor_", setor), x=setor_lon, y=setor_lat, agencia_municipio_codigo),
  agencias%>%transmute(id=paste0('agencia_', agencia_codigo), x=agencia_lon, y=agencia_lat, agencia_municipio_codigo=substr(agencia_codigo,1,7))
)

library(ggplot2)
a <- agencias%>%filter(agencia_codigo=="290320100")
s <- setores%>%filter(setor=="291040405000019")

d <- dodgr_distances(net_connected_r1, from = a%>%st_coordinates(), to = s%>%st_coordinates())
dt <- dodgr_distances(net_connected_r1, from = a%>%st_coordinates(), to = s%>%st_coordinates(), shortest = FALSE)

path_map(net_connected_r1, from_coords =s%>%st_coordinates(),  
         to_coords = a%>%st_coordinates(), roads_sf=uf)


path_map(net_connected_r1, from_coords =setores%>%slice(100)%>%st_coordinates(),  
         to_coords = agencias%>%slice(20)%>%st_coordinates(), roads_sf=uf)


net_connected_r1_vertices <- dodgr_vertices(net_connected_r1)
net_connected_r1_xy_vertices <- unique(net_connected_r1_vertices$id[match_pts_to_verts(verts = net_connected_r1_vertices, xy=setores_agencias)])



library(tictoc)
tic()
net_contracted_r1 <- dodgr_contract_graph(graph = net_connected_r1, verts = net_connected_r1_xy_vertices)
toc()
dodgr::dodgr_save_streetnet(net = net_contracted_r1, here::here(paste0("scripts/", tolower(ufsiglanow), "_contracted_1.rds")))


stop()
net_contracted_r1 <- dodgr::dodgr_load_streetnet(here::here(paste0("scripts/", ufsiglanow, "_contracted_1.rds")))

path_map(net_contracted_r1, from_coords =setores%>%slice(1)%>%st_coordinates(),  
         to_coords = agencias%>%slice(10)%>%st_coordinates(), roads_sf=uf) +
  geom_

d_agencias_setores <- dodgr_dists_nearest(
  graph = 
    net_connected_r1%>%
    mutate(
      d=time_weighted,
      edge_type=if_else(grepl("artificial", highway), "artificial", "real"))
  , from = setores%>%st_coordinates(), to=agencias%>%st_coordinates())%>%
  bind_cols(setores%>%st_drop_geometry()%>%select(agencia_municipio_codigo))
d_agencias_setores%>%
  count(to, agencia_municipio_codigo)

toc()
