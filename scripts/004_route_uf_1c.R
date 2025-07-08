devtools::load_all(".")
library(dplyr)
library(sf)

ufsiglanow <- "ba"

devtools::load_all(here::here("../dodgr"))

uf <- orce::ufs%>%
  filter(uf_sigla==toupper(ufsiglanow))

agencias <- orce::agencias_bdo%>%filter(substr(agencia_codigo,1,2)==uf$uf_codigo)%>%
  mutate(municipio_codigo=substr(agencia_codigo,1,7))

a <- agencias%>%st_coordinates()
rownames(a) <- agencias$agencia_codigo


setores <- orce::pontos_setores%>%
  filter(substr(setor,1,2)==uf$uf_codigo)%>%
  mutate(municipio_codigo=substr(setor,1,7))%>%
  left_join(orce::agencias_mun)

s <- setores%>%st_coordinates()
rownames(s) <- setores$setor


net_contracted_r1 <- dodgr::dodgr_load_streetnet("~/github/dodgr/scripts/ba_contracted_1.rds")


RcppParallel::setThreadOptions(numThreads = 6)

r_a_s <- dodgr_distances_batch(graph = net_contracted_r1, from = a, to = s, replace = TRUE, output_dir = "data-raw/ba_dists_agencia_setor", batch_size = 1e6, shortest=FALSE, calculate_time = TRUE)

distancias_ag_ba_0 <- r_a_s%>%transmute(setor=from_id, agencia_codigo=to_id, distancia_km=distance/1000, duracao_h=time/60/60)%>%
  collect()%>%
  arrange(agencia_codigo,setor)


#gmaps_url = glue::glue("https://www.google.com/maps/dir/{setor_lat},{setor_lon}/{agencia_lat},{agencia_lon}"),

distancias_ag_ba <- distancias_ag_ba_0%>%
  left_join(orce::pontos_setores%>%sf::st_drop_geometry()%>%select(setor, setor_lat, setor_lon), by=c("setor"))%>%
  left_join(orce::agencias_bdo%>%sf::st_drop_geometry()%>%select(agencia_codigo, agencia_lat, agencia_lon), by="agencia_codigo")%>%
  mutate(municipio_codigo=substr(setor,1,7))%>%
  left_join(orce::agencias_municipios_diaria, by=c("agencia_codigo", "municipio_codigo"))%>%
  mutate(gmaps=glue::glue("https://www.google.com/maps/dir/{setor_lat},{setor_lon}/{agencia_lat},{agencia_lon}"))%>%
  arrange(agencia_codigo,setor)

usethis::use_data(distancias_ag_ba, overwrite = TRUE)

distancias_ag_ba_mun <- distancias_ag_ba%>%
  distinct(agencia_codigo, municipio_codigo, diaria_municipio)%>%
  arrange(agencia_codigo,municipio_codigo)

openxlsx2::write_xlsx(list(
     distancia=tidyr::pivot_wider(distancias_ag_ba_0%>%select(-duracao_h), values_from = distancia_km, names_from = agencia_codigo),
     duracao=tidyr::pivot_wider(distancias_ag_ba_0%>%select(-distancia_km), values_from = duracao_h, names_from = agencia_codigo),
     mapa=tidyr::pivot_wider(distancias_ag_ba%>%
                               select(agencia_codigo, setor, gmaps), values_from = gmaps, names_from = agencia_codigo),
     diaria_municipio=tidyr::pivot_wider(distancias_ag_ba_mun, values_from = diaria_municipio, names_from = agencia_codigo)
     
     ), '~/tmp/distancias_ag_ba.xlsx')




# Process coordinates in batches
setores_coords <- st_coordinates(setores)
agencias_coords <- st_coordinates(agencias)

# Add IDs to coordinates
setores_ids <- setores$setor
agencias_ids <- agencias$agencia_codigo

# Create output file path
output_file <- here::here(paste0("scripts/", tolower(ufsiglanow), "_distances.csv"))

# Run the processing with dynamic batch sizing
d_setor_agencia <- dodgr_distances_batch(
  from_coords = setores_coords%>%head(),
  to_coords = agencias_coords,
  network = net_contracted_r1,
  batch_size = 10000,
  output_file = output_file,
  shortest = FALSE
)
toc()
```

library(mapgl)
#g <- net_contracted_r1
g <- net_connected_r1
p <- dodgr::dodgr_paths(from=s, to=a, graph=g, vertices = TRUE)%>%unlist()
v <- dodgr::dodgr_vertices(g)
pv <- (v[v$id%in%p,])%>%st_as_sf(coords=c("x", "y"), crs=st_crs(uf))
pvl <- st_cast(pv, "LINESTRING")
pv_path <- pv %>%
  arrange(match(id, p)) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  mutate(id=1)

maplibre() %>%
  add_line_layer(source = pv_path, line_color = "purple", line_width = 3, line_opacity = 0.7, id="id")
%>%
  add_circle_layer(source = pv, id = "id", circle_color = "purple", circle_opacity = 0.1, circle_stroke_opacity = 0.1)
  add_circle_layer(source=pv, id="id", circle_color = "purple", circle_opacity = .1, circle_stroke_opacity = .1)


net_contracted_r2 <- dodgrconnect::consolidate_vertex_ids(net_contracted_r1%>%as_tibble())

p <- dodgr::dodgr_paths(from=s, to=a, graph=net_contracted_r2, vertices = FALSE)
p <- net_connected_r1[unlist(p),]
