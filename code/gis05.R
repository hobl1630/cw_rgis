if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)
##4.3------------------------------------------------------------------------------
#Crop - reduce the number of layers in the raster file to defined extent

spr_prec <- rast(here("data/spr_prec_us.tif"))
spr_prec

ext(spr_prec)

ggplot()+
  geom_spatraster(dat = spr_prec)

#bound with the -80 to -75 and latitudes 34 to 37

spr_prec_crop <- crop(x = spr_prec,
                      y = c(-80, -75, 34, 37))
## load county vector 
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

ggplot() +
  geom_spatraster(data = spr_prec_crop) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25) ## alpha = 0.25 polygon layer transparent

spr_prec_nc <- crop(x = spr_prec,
                    y = sf_nc_county)
spr_prec_nc

ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
#Merge

spr_nw <- rast(here("data/spr_prec_ncnw.tif")) # Northwest NC
spr_ne <- rast(here("data/spr_prec_ncne.tif")) # Northeast NC
spr_sw <- rast(here("data/spr_prec_ncsw.tif")) # Southwest NC
spr_se <- rast(here("data/spr_prec_ncse.tif")) # Southeast NC

ggplot() +
  geom_spatraster(data = spr_nw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
ggplot() +
  geom_spatraster(data = spr_ne) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
ggplot() +
  geom_spatraster(data = spr_sw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
ggplot() +
  geom_spatraster(data = spr_se) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

spr_n <- merge(spr_nw, 
               spr_ne)

ggplot() +
  geom_spatraster(data = spr_n) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

spr_s <- merge(spr_sw,
               spr_se)
ggplot() +
  geom_spatraster(data = spr_s) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#faster way for multiple 

list_spr <- list(spr_nw,
                 spr_ne,
                 spr_sw,
                 spr_se)

spr_col <- sprc(list_spr)

spr_merge <- merge(spr_col)

ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

writeRaster(spr_merge, 
            filename = here("data/spr_prec_nc.tif"),
            overwrite = TRUE)

#Stack

spr_prec_nc <- rast(here("data/spr_prec_nc.tif"))
spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))

spr_prec_nc

spr_tmp_nc

spr_pt_nc <- c(spr_prec_nc,
                spr_tmp_nc)
spr_pt_nc

#can access each layer separately using $ operator

spr_pt_nc$precipitation

#Reprojection

print(spr_prec_nc)

spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617")
spr_prec_nc_proj

#Exercise----------------------------------------------------------------------
#MERGE
spr_temp_nw <- rast(here("data/spr_tmp_ncnw.tif")) # Northwest NC
spr_temp_ne <- rast(here("data/spr_tmp_ncne.tif")) # Northeast NC
spr_temp_sw <- rast(here("data/spr_tmp_ncsw.tif")) # Southwest NC
spr_temp_se <- rast(here("data/spr_tmp_ncse.tif")) # Southeast NC

spr_temp_list<- list(spr_temp_ne,
                     spr_temp_nw,
                     spr_temp_se,
                     spr_temp_sw)

spr_col_temp<-sprc(spr_temp_list)

spr_temp_merge<- merge(spr_col_temp)

spr_temp_merge

ggplot() +
  geom_spatraster(data = spr_temp_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
#CROP
sf_nc_county<-readRDS(here("data/sf_nc_county.rds"))

sf_nc_county

sf_camden<-sf_nc_county %>% 
  filter(county == "camden")

sf_camden

ext(sf_camden)

spr_temp_camden <- crop(x = spr_temp_merge,
                        y = sf_camden)


spr_temp_camden <- crop(x = spr_temp_merge,
                      y = c(-76.5632550080924,
                            -75.9568384754626,
                            36.1698027698011,
                            36.5561260977251))
ggplot()+
  geom_spatraster(data = spr_temp_camden)+
  geom_sf(data = sf_camden,
          alpha = 0.25)

ggplot()+
  geom_spatraster(data = spr_temp_camden)+
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

?project


spr_temp_camden_proj<-project(spr_temp_camden,
                              y = "EPSG:32618",
                              method = "bilinear")
print(spr_temp_camden_proj)
