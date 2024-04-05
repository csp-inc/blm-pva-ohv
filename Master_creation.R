rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","tidyr","lme4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


n70 <- rast("./output_layers/n70_04052024.tif")
n80 <- rast("./output_layers/n80_04052024.tif")
N10 <- rast("./output_layers/N10_04052024.tif")
N20 <- rast("./output_layers/N20_04052024.tif")

# Stacking
stack <- c(n70,n80,N10,N20)


# Getting values from each cell in each decade raster

val_70 <- values(stack[[1]])
val_80 <- values(stack[[2]])
val_10 <- values(stack[[3]])
val_20 <- values(stack[[4]])

# Turning into a dataframe
values_df <- as.data.frame(cbind(val_70,val_80,val_20,val_10))
names(values_df) <- c("V70","V80","V10","V20")

# Reclassifying NaN as NA
values_df <- values_df %>% mutate_all(~ifelse(is.nan(.), NA, .))

# Creating cell IDs
values_df$raster_cell <- as.character(c(1:nrow(values_df)))

library(stars)
# Read your raster data
raster <- stack[[1]]
raster <- classify(raster, cbind(0, 5, 1), right=FALSE)
raster <- classify(raster, cbind(NaN, 1))

# Convert raster to sf object
raster_sf <- st_as_stars(raster)
raster_sf <- st_as_sf(raster_sf) # takes a minute

# Calculate centroids
centroids <- st_centroid(raster_sf)
centroids <- centroids[,-1]
centroids$raster_cell <- values_df$raster_cell

grid <- st_read("./NETR_lookup/grid_full_dtrange_crop.shp")


points_w_grid <- st_join(centroids, grid)


states <- st_read("./shapefiles/us_states/states_web.shp")

states <- states[,c("NAME","geometry")]


points_w_grid_state <- st_join(points_w_grid, states)

# Removing cells that are all NA (meaning they are outside the range)
values_df <- values_df[!rowSums(is.na(values_df)) == 4, ]


values_df <- values_df %>% left_join(points_w_grid_state, by = "raster_cell")
head(values_df)
unique(values_df$V70)

names(values_df) <- c("V70","V80","V10","V20","raster_cell","grid_cell","state","geometry")

if(dir.exists("./other_data/master") == FALSE){dir.create("./other_data/master")}
st_write(values_df,"./other_data/master/master_cells.shp")

# removing geometry to save as a .csv

values_df_no_geom <- as.data.frame(values_df[,-8])
head(values_df_no_geom)

write.csv(values_df_no_geom,"./other_data/master/master_cells.csv")
