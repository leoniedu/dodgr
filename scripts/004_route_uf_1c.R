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

stop()

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
