if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)

# Chapter 4 - Raster ------------------------------------------------------

spr_ex <- rast(here("data/spr_example.tif"))
spr_ex

writeRaster(x=spr_ex,
            filename =here("data/spr_elev.tif"),
            overwrite = TRUE)

#Mapping Raster Data

ggplot() + 
  geom_spatraster(data = spr_ex)

#convert from spatraster to stars (enables use of mapview)

star_ex <- st_as_stars(spr_ex)

mapview(star_ex)

#Continuous vs Discrete Raster Data----------------------------
#Continuous
v_elev <- values(spr_ex)

head(v_elev, 10)

na.omit(v_elev) %>% 
  mean()

##extract gets data from specific locations

extract(spr_ex, y = cbind(6.0000, 50.0000)) #elevation at these coordinates

df_point <- tibble(lon = c(6, 5.9),
                   lat = c(50, 49.96))
df_point

extract(spr_ex,
        y = df_point)

#Discrete --------------------------------------------

spr_for <- rast(here("data/spr_forest_nc.tif"))

spr_for

ggplot()+
  geom_spatraster(data = spr_for)

#confirm categorical nature of data using unique()
unique(spr_for)

#use binary to find average - using mean()

v_binary<- values(spr_for)

p_forest<- mean(v_binary)

p_forest #68.42346%

#can also refer to multiple not just binary

spr_land <- rast(here("data/spr_land_reclass.tif"))

spr_land

unique(spr_land)
#encoded for specific categorical codes - 
# 1001 - Forest
# 1010 - Crop
# 1100 - Urban

#landuse data for Sulvian building using coordinates

extract(spr_land, cbind(-79.8063, 36.0701))
#shows that Suilivan is listed as urban (accurate)

#Reclassification (using classify())-----------------------------------------

#changing the multiple to binary categories
# 0 = Forest, 1 = Not Forest

# write a conversion matrix
# left is original value - right is value after conversion

(cm <- cbind(c(0, 1001, 1010, 1100),
             c(0, 1, 0, 0)))

spr_bin <- classify(spr_land,
                    rcl = cm)
spr_bin

v_bin <- values(spr_bin)

mean(v_bin)

#Exercise-----------------------------------------------------------------------

spr_prec_ncne<-rast(here("data/spr_prec_ncne.tif"))

spr_prec_ncne

#Number of Rows: 162
#Number of Columns: 532
#Resolution: 0.0083, 0.0083
#Spatial Extent- Xmin: -79.89181 Xmax: -75.45847 Ymin:35.24153 Ymax: 36.519153
#Coordinate Reference: Long/Lat WGS 84
#Precipitation Values: Min - 1063.1 Max - 1501.5

ggplot()+
  geom_spatraster(data = spr_prec_ncne)

#extract values
sf_site<-read_rds(here("data/sf_finsync_nc.rds"))
sf_site

df_xy<-st_coordinates(sf_site)

df_land<- extract(spr_land,
                  y = df_xy)
df_land

#reclassify

(cu <- cbind(c(0, 1001, 1010, 1100),
             c(0, 0, 0, 1)))

spr_urban <- classify(spr_land,
         rcl = cu)

values(spr_urban) %>% 
  mean()
#3.169528%

# *Worked Ahead* Raster Data Manipulation ------------------------------------------------
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

#Stack

#spr_prec_nc <- rast(here("data/spr_prec_nc.tif")) File isnt present assigned to spr_merge object
spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))
