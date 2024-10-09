## library you may use for vector analysis
## - sf: for vector analysis
## - terra: for raster analysis
## - exactextractr: for raster x vector cross reference
## - tidyverse: sf is compatible with tidyverse
pacman::p_load(tidyverse,
               sf,
               terra,
               exactextractr,
               mapview)


# read raster data --------------------------------------------------------

## you can use terra::rast("pathtofile") to read .tif file
## sr_prec is a precipitation layer
sr_prec <- list.files("data",
                      full.names = TRUE, 
                      pattern = "\\.tif") %>% 
  rast()

## vector data
# sf_nc <- st_read(system.file("shape/nc.shp", package="sf"))
# st_write(sf_nc, "data/nc.shp")
sf_nc <- st_read("data/nc.shp") %>% 
  st_transform(crs = 4326) %>% 
  dplyr::select(NULL) %>% 
  mutate(fid = row_number())

## summary statistics in each polygon extracted from raster
## - exactextractr::exact_extract() cross vector and raster layers
exact_extract(x = sr_prec,
              y = sf_nc,
              fun = "weighted_mean",
              append_cols = TRUE,
              weights = "area")

exact_extract(x = sr_prec,
              y = sf_nc,
              fun = "mean",
              append_cols = TRUE)

