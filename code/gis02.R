##Vector Data--------------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, sf,mapview)

# read a shapefile (e.g., ESRI Shapefile format)
# `quiet = TRUE` just for cleaner output
(sf_nc_county <- st_read(dsn = here::here("data/nc.shp"), 
                         quiet = TRUE))

# save as shapefile (overwrites by setting append = FALSE)
st_write(sf_nc_county,
         dsn = here::here("data/sf_nc_county.shp"),
         append = FALSE)

# save as Geopackage (overwrites by setting append = FALSE)
st_write(sf_nc_county,
         dsn = here::here("data/sf_nc_county.gpkg"),
         append = FALSE)

#For use within R, it is often convenient to save spatial data in .rds format using the saveRDS() function.
# save as an RDS file (compact and efficient for use within R)
saveRDS(sf_nc_county,
        file = here::here("data/sf_nc_county.rds"))

#Read the rds file using readRDS()
sf_nc_county<- readRDS(file = here::here("data/sf_nc_county.rds"))

# Point Data 
sf_site<-readRDS(file = here::here("data/sf_finsync_nc.rds"))
sf_site

mapview(sf_site,
        col.regions = "black",
        legend = FALSE)
(sf_str <- readRDS(here::here("data/sf_stream_gi.rds")))

#line
mapview(sf_str,
        color = "steelblue",
        legend = FALSE)
#1st ten lines
(sf_str_f10 <- sf_str %>% 
    slice(1:10))

mapview(sf_str_f10,
        color = "steelblue",
        legend = FALSE)

#polygon
mapview(sf_nc_county,
        col.regions = "lightblue",
        legend = FALSE)

sf_nc_gc<-sf_nc_county %>% 
  filter(county == "guilford")

mapview(sf_nc_gc,
        col.regions = "lightblue",
        legend = FALSE)
#ggplot to visualize the data on a map (not a great one though)

ggplot()+geom_sf(data=sf_nc_county)

ggplot()+geom_sf(data=sf_nc_county) + geom_sf(data=sf_str)

ggplot()+geom_sf(data=sf_nc_county) + geom_sf(data=sf_str) + geom_sf(data=sf_site)

#For a better map, zoom into guilford specifically using the filtered polygon data from earlier

ggplot() +
  geom_sf(data=sf_nc_gc) + geom_sf(data = sf_str)

##Exercies----------------------------------------------------------------------
sf_str_as<-read_rds(here::here("data/sf_stream_as.rds"))

sf_nc_county
sf_str_as
#Both are CRS WGS 84 (should work together)

ggplot()+
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str_as)

sf_nc_as <-sf_nc_county %>% 
  filter(county == "ashe")

ggplot()+
  geom_sf(data = sf_nc_as) +
  geom_sf(data = sf_str_as)


