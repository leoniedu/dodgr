ufsiglanow <- "ac"
n_sample <- 100
## following parameter used to
# break_long_edges
# remove small components
# associate points to components if d< (dmin_component/2)
# otherwise connect points to closest component
dmin_component <- 1000

use_cache <- FALSE

library(geoarrow)
library(arrow)
library(sf)
library(ggplot2)
library(dplyr)
devtools::load_all("../dodgr")
devtools::load_all("../dodgrconnect/")
#devtools::load_all()
dodgr_cache_off()


uf <- orce::ufs%>%
  filter(uf_sigla==toupper(ufsiglanow))
uf_buffer <- st_buffer(readr::read_rds("~/github/tractdistancesbr/data-raw/br_states.rds")%>%
                         filter(abbrev_state==toupper(ufsiglanow)), dist = units::set_units(10000, "m"))%>%
  st_transform(crs = 4326)

agencias <- orce::agencias_bdo%>%filter(substr(agencia_codigo,1,2)==uf$uf_codigo)

set.seed(1)
tracts <- orce::pontos_setores%>%
  filter(grepl(paste0("^",uf$uf_codigo), setor))%>%
  slice_sample(n=n_sample)

fname_tracts <- paste0("data-raw/", ufsiglanow, "_tracts.rds")
fname_pts <- paste0("data-raw/", ufsiglanow, "_pts_connected.rds")
fname_complete <- paste0("data-raw/", ufsiglanow, "_complete_connected.rds")
complete_sf_fname <- file.path(here::here("data-raw", paste0(toupper(ufsiglanow), "_sf.rds")))
waterways <- c("river", "canal")
complete_net_0_fname <- file.path(here::here('data-raw'), paste0(toupper(ufsiglanow), "_complete.rds"))
complete_map_fname <- glue::glue("../tractdistancesbr/data-raw/{toupper(uf$uf_sigla)}.parquet")

