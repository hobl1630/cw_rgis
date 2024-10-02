## library you may use for vector analysis
## - sf: for vector analysis
## - tidyverse: sf is compatible with tidyverse
pacman::p_load(tidyverse,
               sf)

# basic exercise ----------------------------------------------------------

## this is data pre-loaded with sf package
sf_nc <- st_read(system.file("shape/nc.shp", package="sf"))
sf_nc <- st_read("data/nc.shp")

## visualize
mapview::mapview(sf_nc)

## some simple calculation
## - sf feature can use most of dplyr functions

## - arrange by area
sf_nc %>% 
  arrange(AREA)

## - extract column info
range(sf_nc$AREA)

## - subset polygon by area
sf_nc_sub <- sf_nc %>% 
  filter(AREA > 0.1)

mapview::mapview(sf_nc_sub)


# calculate spatial properties --------------------------------------------

sf_nc0 <- sf_nc %>% 
  dplyr::select(NULL)

## - calculate area of polygons
sf_nc0 %>% 
  mutate(area = st_area(.) %>% 
           units::set_units("km^2"),
         peri = st_perimeter(.))

sf_nc0 %>% 
  st_transform(crs = 5070) %>% 
  mutate(area = st_area(.) %>% 
           units::set_units("km^2"))


# cross two layers --------------------------------------------------------

# - generate random points
sf_point <- tibble() %>% 
  bind_rows(c(st_bbox(sf_nc))) %>% # extract the boundary box (bbox) with "st_bbox()" 
  reframe(Y = runif(100, ymin, ymax),
          X = runif(100, xmin, xmax)) %>% # random points within the bbox
  mutate(point_id = row_number()) %>% 
  st_as_sf(coords = c("X", "Y")) %>% # define as sf object
  st_set_crs(st_crs(sf_nc)) 

sf_nc1 <- sf_nc0 %>% 
  mutate(id = row_number())

sf_point_sub <- st_join(sf_point, sf_nc1) %>% 
  drop_na(id) %>% 
  group_by(id) %>% 
  tally()

mapview::mapview(sf_nc1) + mapview::mapview(sf_point_sub)

