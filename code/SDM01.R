if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here)

#6 GIS and Ecology--------------------------------------------------------------
(df_finsync <- read_csv(here("data/data_finsync_nc.csv")))

#examine the first site (site ID finsync_nrs_nc-10013)
(df_st1 <- df_finsync %>% 
    filter(site_id == "finsync_nrs_nc-10013"))

#model the distribution of a single species from our dataset as a function of air temperature

#we must reshape the dataset to include complete presence/absence information for a single species across all survey sites

#create a dataframe listing species presence/absence across all survey sites. 
#To make it clear that each row represents a presence record, let me add a column of “presence”

(df_finsync %>%
  pivot_wider(id_cols = c(site_id, 
                          lon, 
                          lat),
              names_from = latin,
              values_from = presence))

(df_finsync %>%
    pivot_wider(id_cols = c(site_id, 
                            lon, 
                            lat),
                names_from = latin,
                values_from = presence,
                values_fill = 0)) #replacing NA with 0

#focus on a single species for this exercise. We’ll extract the presence/absence information for Redbreasted Sunfish

(df_rbs<- df_finsync %>%
    pivot_wider(id_cols = c(site_id, 
                            lon, 
                            lat),
                names_from = latin,
                values_from = presence,
                values_fill = 0) %>% 
  select(site_id,
         lon,
         lat,
         "Lepomis auritus") %>% 
  rename(y = "Lepomis auritus"))

#Linking to the Environment-----------------------------------------------------

#create an sf object listing unique site coordinates along with site_id

(sf_rbs <- df_rbs %>% 
  st_as_sf(coords = c("lon", 
                      "lat"),
           crs = 4326))

(spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif")))

(sf_rbs_tmp <- extract(x = spr_tmp_nc,
                       y = sf_rbs,
                       bind = TRUE))
st_as_sf(sf_rbs_tmp)

(df_rbs_tmp <- as_tibble(sf_rbs_tmp))


#Visualization------------------------------------------------------------------

ggplot() +
  geom_spatraster(data = spr_tmp_nc) + 
  geom_sf(data = sf_rbs,
          aes(color = factor(y))) + 
  scale_fill_viridis_c() +   
  theme_bw()

df_rbs_tmp %>%
  ggplot(aes(y = y, 
             x = temperature)) +
  geom_point() +
  labs(y = "Lepomis auritus Presence",
       x = "Temperature (C)")+
  theme_minimal()

#Analysis ----------------------------------------------------------------------

#using binomial regression model, because it is a binary

(m_rbs <- glm(y ~ temperature,
          data = df_rbs_tmp,
          family = "binomial"))

summary(m_rbs)

(df_pred <- ggpredict(m_rbs,
                     terms = "temperature [all]"))

ggplot() +
  geom_point(data = df_rbs_tmp,
             aes(x = temperature,
                 y = y)) +
  geom_line(data = df_pred,
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "mediumorchid3",
              alpha = 0.2) +
  labs(x = "Air Temperature (C)",
       y = "Probability of Occurrence - Lepomis auritus") +
  theme_minimal()

#Final Project -----------------------------------------------------------------
df_finsync %>% 
  pull(latin) %>% 
  unique()

df_finsync %>% 
  filter(latin == "Ameiurus natalis")
