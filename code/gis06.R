if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               terra,
               tidyterra,
               exactextractr,
               here)

#5 Vector Raster Interactions ----------------------------------------------------

## finsync survey site
sf_site <- readRDS(here("data/sf_finsync_nc.rds"))

sf_site

## county polygons
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

sf_nc_county

## precipitation raster
spr_prec_nc <- rast(here("data/spr_prec_nc.tif"))

spr_prec_nc

ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_site) +
  scale_fill_viridis_c() +
  theme_bw()

