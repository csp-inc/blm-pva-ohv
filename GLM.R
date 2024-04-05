rm(list=ls())
library(tidyverse)
library(sf)
# library(sp)
# library(rgdal)
# library(rgeos)
library(spgwr)
library(grid)
library(gridExtra)


# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(digits=4, scipen=999)


values_df <- st_read("./other_data/master/master_cells.shp")

names(values_df) <- c("1970","1980","2010","2020","rstr_cl","grd_cll","state","geometry")


values_df_long <- values_df %>% 
  pivot_longer(
    cols = c("1970","1980","2010","2020"), 
    names_to = "year",
    values_to = "value"
  )

values_df_long <- values_df_long[complete.cases(values_df_long$value), ]

values_df_long$value[values_df_long$value == 4] <- 3

sampled_values <- sample(unique(values_df_long$rstr_cl), 50000, replace = FALSE)

values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)

values_df_sub$year <- as.numeric(values_df_sub$year)
values_df_sub$year_s <- scale(values_df_sub$year)
values_df_sub$value_s <- scale(values_df_sub$value)
values_df_sub$year <- as.factor(values_df_sub$year)
values_df_sub$state <- as.factor(values_df_sub$state)

library(lme4)
model <- glmer(value ~ year_s + (1 | state), data = values_df_sub, family = "poisson", control = glmerControl(optCtrl = list(maxfun = 400)))
summary(model)
ranef(model)

resids <- residuals(model)

values_df_sub <- values_df_sub %>% cbind(resids)

library(ggspatial)

dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub, aes(col = resids),size = 1)



