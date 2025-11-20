if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here)

(df_finsync <- read_csv(here("data/data_finsync_nc.csv")))

df_finsync %>% 
  filter(latin == "Ameiurus natalis")

(df_ybull<- df_finsync %>%
    pivot_wider(id_cols = c(site_id, 
                            lon, 
                            lat),
                names_from = latin,
                values_from = presence,
                values_fill = 0) %>% 
    select(site_id,
           lon,
           lat,
           "Ameiurus natalis") %>% 
    rename(y = "Ameiurus natalis"))

(sf_ybull <- df_ybull %>% 
    st_as_sf(coords = c("lon", 
                        "lat"),
             crs = 4326))
#Temperature and Presence--------------------------------------------------------------------

(spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif")))

(sf_ybull_tmp <- extract(x = spr_tmp_nc,
                       y = sf_ybull,
                       bind = TRUE))
st_as_sf(sf_ybull_tmp)

(df_ybull_tmp <- as_tibble(sf_ybull_tmp))

#Visualization
ggplot() +
  geom_spatraster(data = spr_tmp_nc) + 
  geom_sf(data = sf_ybull,
          aes(color = factor(y))) + 
  scale_fill_viridis_c() +   
  theme_bw()

df_ybull_tmp %>%
  ggplot(aes(y = y, 
             x = temperature)) +
  geom_point() +
  labs(y = "Ameiurus natalis Presence",
       x = "Temperature (C)")+
  theme_minimal()

#Analysis

(m_ybull <- glm(y ~ temperature,
              data = df_ybull_tmp,
              family = "binomial"))

summary(m_ybull)

(df_pred_tmp <- ggpredict(m_ybull,
                      terms = "temperature [all]"))

ggplot() +
  geom_point(data = df_ybull_tmp,
             aes(x = temperature,
                 y = y)) +
  geom_line(data = df_pred_tmp,
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_pred_tmp,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "mediumorchid3",
              alpha = 0.2) +
  labs(x = "Air Temperature (C)",
       y = "Probability of Occurrence - Ameiurus natalis") +
  theme_minimal()

#Landuse and Presence------------------------------------------------------------------------
(spr_land<- rast(here("data/spr_land_reclass.tif")))

unique(values(spr_land))

crs(spr_land)

#specifically interested in urban development
(cm <- matrix(c(1100, 
               1100,
               1),
             ncol = 3,
             byrow = TRUE))

spr_bin <- classify(spr_land,
                    rcl = cm,
                    others = 0)


sf_ybull_land <- extract(
  x = spr_bin,
  y = sf_ybull,
  bind = TRUE)

df_ybull_land <- as_tibble(sf_ybull_land)

df_finsync %>% filter(latin == "Ameiurus natalis") %>% nrow()

yv <- vect(sf_ybull)

sf_ybull_land <- extract(spr_bin, yv, bind = TRUE)

sf_ybull_land <- st_as_sf(sf_ybull_land)

table(sf_ybull_land[[2]])


##Troubleshooting
sf_ybull %>% nrow()

sf_ybull_land %>% nrow()

freq(spr_bin)

sum(!is.na(sf_ybull_land[[2]]))

names(df_ybull_land)[2] <- "y"
names(df_ybull_land)[3] <- "urban"

df_ybull_land

str(sf_ybull_land)

sf_ybull %>% summary()
st_bbox(sf_ybull)

ext(spr_land)
crs(spr_land)
unique(values(spr_land))

table(df_ybull_land$urban, df_ybull_land$y)


##Visualization

mapview(sf_ybull_land,
        zcol = names(sf_ybull_land)[2],
        col.regions = c("lightblue",
                        "salmon"))

ggplot() +
  geom_spatraster(data = spr_bin) +
  geom_sf(data = sf_ybull_land, aes(color = factor(y))) +
  scale_fill_viridis_c() +
  labs(color = "Presence (y)") +
  theme_bw()

df_ybull_land %>%
  ggplot(aes(x = urban, y = y)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7) +
  labs(x = "Urban (1) / Non-urban (0)",
       y = "Ameiurus natalis Presence") +
  theme_minimal()

##Analysis
m_ybull_land <- glm(y ~ urban, 
               data = df_ybull_land,
               family = "binomial")
summary(m_ybull_land)
##Not sure how to fix this going to focus on temperature,



