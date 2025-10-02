if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, sf,mapview, here,MetBrewer)


# Spatial Join ------------------------------------------------------------

#point vector
sf_site<-readRDS(here("data/sf_finsync_nc.rds"))

#polygon vector
sf_nc_county<-readRDS(file=here("data/sf_nc_county.rds"))

#st_join() evaluates two geometry layers
sf_site_join <- st_join(x=sf_site,
                        y=sf_nc_county)

sf_one<- sf_site %>% 
  slice(1)

mapview(sf_nc_county)+mapview(sf_one)

#subset in guilford

sf_site_guilford<-sf_site_join %>% 
  filter(county== "guilford")

sf_nc_guilford<-sf_nc_county %>% 
  filter(county=="guilford")

sf_str <- readRDS(here("data/sf_stream_gi.rds"))

ggplot()+
  geom_sf(data=sf_nc_guilford)+
  geom_sf(data=sf_str)+
  geom_sf(data=sf_site_guilford)+
  theme_dark()

ggplot() +
  geom_sf(data = sf_nc_guilford) +
  geom_sf(data = sf_str,
          color = "steelblue") +
  geom_sf(data = sf_site_guilford,
          color = "yellow") +
  theme_dark()

#find the county with most sites


df_n<-sf_site_join %>% 
  as_tibble() %>%
  group_by(county) %>% 
  summarize(n_site = n()) %>% 
  arrange(desc(n_site))

mapview(sf_site_join)+ mapview(sf_nc_county)

#geospatial object and df_n (count object) combine them with left_join()

sf_nc_n<-sf_nc_county %>% 
  left_join(df_n, by = "county") %>% 
  mutate(n_site = ifelse(is.na(n_site),0,n_site))

#mapping

  ggplot()+geom_sf(data = sf_nc_n,aes(fill = n_site))
  

# Geometric Analysis ------------------------------------------------------
#Length Calculation
sf_str_proj<- st_transform(sf_str, crs = 32617)

v_str_1<- st_length(sf_str_proj)
head(v_str_1)

sf_str_w_len<-sf_str %>% 
  mutate(length = as.numeric(v_str_1))

ggplot() + geom_sf(data = sf_str_w_len, aes(color = length))

#Area Calculation
sf_nc_county_proj<-st_transform(sf_nc_county, 
                                crs = 32617)
v_area<-st_area(sf_nc_county_proj)
v_area

sf_nc_county_w_area<-sf_nc_county %>% 
  mutate(area = as.numeric(v_area))

ggplot()+
  geom_sf(data = sf_nc_county_w_area, 
          aes(fill = area))+
  MetBrewer::scale_fill_met_c("VanGogh3")

# Exercise ----------------------------------------------------------------
sf_quakes<-readRDS(here("data/sf_quakes.rds"))
sf_nz<-readRDS(here("data/sf_nz.rds"))

mapview(sf_nz)+
  mapview(sf_quakes)

sf_quakes_join<-st_join(x=sf_nz,
        y=sf_quakes)

sf_quakes_join

sf_quakes_nz<-drop_na(sf_quakes_join,
        fid)

nrow(sf_quakes_nz)


df_n<-sf_site_join %>%
  as_tibble() %>% 
  group_by(county) %>% 
  summarize(n_site = n()) %>% 
  arrange(desc(n_site))

sf_n_site<-sf_site_join %>% left_join(df_n,by = "county")

sf_n10<- sf_n_site %>% 
  filter(n_site > 10)
sf_n10

ggplot()+
  geom_sf(data = sf_n_site, 
          color = "grey")+
  geom_sf(data = sf_n10, 
          color = "salmon")+
  theme_bw()
          