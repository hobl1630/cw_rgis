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

spr_site_prec <- extract(x = spr_prec_nc,
                         y= sf_site,
                         bind = TRUE)
spr_site_prec

sf_site_prec <- st_as_sf(spr_site_prec)

sf_site_prec

ggplot()+
  geom_sf(data = sf_nc_county,
          fill = "wheat1")+
  geom_sf(data = sf_site_prec,
          aes(color = precipitation))+
  scale_color_viridis_c()+
  theme_bw()

#5.4 Zonal Statistics

sf_nc_county_proj <- st_transform(sf_nc_county,
                                  crs = 32617)

sf_nc_county_proj

spr_prec_nc_proj <- terra::project(x = spr_prec_nc, 
                                   y = crs(sf_nc_county_proj),
                                   method = "bilinear") 

spr_prec_nc_proj

(df_prec_county <- exact_extract(x = spr_prec_nc_proj,
                                 y = sf_nc_county_proj,
                                 fun = "mean",
                                 append_cols = TRUE,
                                 progress = FALSE) %>% 
    as_tibble() %>% 
    rename(precipitation = mean))

sf_nc_county_prec <- sf_nc_county %>% 
    left_join(df_prec_county,
              by = "county")

sf_nc_county_prec

ggplot() +
  geom_sf(data = sf_nc_county_prec,
          aes(fill = precipitation)) +
  scale_fill_viridis_c() +
  theme_bw()

(df_prec_county_alt <- exact_extract(x = spr_prec_nc,
                                     y = sf_nc_county,
                                     fun = "weighted_mean",
                                     weights = "area",
                                     append_cols = TRUE,
                                     progress = FALSE) %>% 
    as_tibble() %>%
    rename(precipitation = weighted_mean))

sf_site_proj <- sf_site %>%
  st_transform(crs = 32617)

sf_site_proj

sf_site_buff_proj <- sf_site_proj %>%
  st_buffer(dist = 10000)

sf_site_buff_proj

sf_site_buff <- sf_site_buff_proj %>%
  st_transform(crs = 4326)

sf_site_buff

ggplot() +
  geom_sf(data = sf_nc_county,
          fill = "wheat1") +
  geom_sf(data = sf_site_buff,
          fill = "mediumorchid") +
  geom_sf(data = sf_site) +
  theme_bw()

(df_prec_buff <- exact_extract(x = spr_prec_nc_proj,
                              y = sf_site_buff_proj,
                              fun = "mean",
                              append_cols = TRUE,
                              progress = FALSE) %>%
  as_tibble() %>%
  rename(precipitation = mean))

(sf_site_prec_buff <- sf_site %>% 
    left_join(df_prec_buff,
              by = "site_id"))

ggplot() +
  geom_sf(data = sf_nc_county,
          fill = "wheat1") + 
  geom_sf(data = sf_site_prec_buff,
          aes(color = precipitation)) +
  scale_color_viridis_c() +
  theme_bw()

