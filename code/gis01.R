## 2 Coordinate Systems --------------------------------------------------------

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)

df_fish <- read_csv(here::here("data/data_finsync_nc.csv"))


sf_site<- df_fish %>% 
  distinct(site_id, lon, lat) %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

# st_as_sf coverts regular dataframe into geospatial dataframe, crs is magic idk
#crs is coordinate reference system - number corresponds to spherical or various flat projections

sf_site

#data on the map

mapview(sf_site,
        legend = FALSE)

#exporting the data to the folder as an independent file that is accessible outside of R
saveRDS(sf_site,
        file = here::here("data/sf_finsync_nc.rds"))

#Geodetic vs Projected CRS 
#Geodetic is 3D spherical, 4326 is usually good for degree based (long, lat) but not for distance

##Conversion From Geodetic to Projected ----------------------------------------

sf_site %>% 
  slice(c(1,2))

sf_ft_wgs<-sf_site %>% 
  slice(c(1,2))

#conversion
sf_ft_wgs %>% 
  st_transform(crs = 32617)

sf_ft_utm <- sf_ft_wgs %>% 
  st_transform(crs = 32617)

st_distance(sf_ft_utm)

mapview(sf_ft_wgs)

##Exercise 2.6 ---------------------------------------------------------------------
#data as tibble
df_quakes<-quakes %>% as_tibble()
df_quakes

#convert dataframe into spatial object
sf_quakes<- df_quakes %>% 
  st_as_sf(coords = c("long","lat"),crs = 4326)

sf_quakes

#map of sf_quakes
mapview(sf_quakes)

#calculate the distance between two points
#1st is to isolate the first two data points
sf_ft_quakes<-sf_quakes %>% 
  slice(c(1,2))

sf_ft_quakes

#Convert to Projected - UTM 60 S for Projection
sf_ft_quakes_proj<-sf_ft_quakes %>% 
  st_transform(crs = 32760)

sf_ft_quakes_proj

st_distance(sf_ft_quakes_proj)

saveRDS(sf_quakes, file = here::here("data/sf_quakes.rds"))

#-------------------------------------------------------------------------------

