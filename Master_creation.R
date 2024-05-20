# This script takes the OHV layers and formats them for modeling and trend analysis

rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","tidyr","lme4","stars")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

### Part 1: extracting OHV density values for each chip for each decade -----
# Choose which layers you want to load in to create the master csv

# n1970 <- rast("./output_layers/n70_04052024.tif")
# n1980 <- rast("./output_layers/n80_04052024.tif")
# N2010 <- rast("./output_layers/N10_04052024.tif")
# N2020 <- rast("./output_layers/N20_04052024.tif")

# n1970 <- rast("./output_layers/netr_1970_masked_9.tif")
# n1980 <- rast("./output_layers/netr_1980_masked_9.tif")
# N2010 <- rast("./output_layers/NAIP_2010_masked_9.tif")
# N2020 <- rast("./output_layers/NAIP_2020_masked_9.tif")

n1970 <- rast("./output_layers/netr_1970_masked_9_nlcdmask_roadsmask.tif")
n1980 <- rast("./output_layers/netr_1980_masked_9_nlcdmask_roadsmask.tif")
N2010 <- rast("./output_layers/NAIP_2010_masked_9_nlcdmask_roadsmask.tif")
N2020 <- rast("./output_layers/NAIP_2020_masked_9_nlcdmask_roadsmask.tif")

# Stacking
stack <- c(n1970,n1980,N2010,N2020)

# Getting values from each cell in each decade raster
val_1970 <- values(stack[[1]])
val_1980 <- values(stack[[2]])
val_2010 <- values(stack[[3]])
val_2020 <- values(stack[[4]])

# Turning into a dataframe
values_df <- as.data.frame(cbind(val_1970,val_1980,val_2010,val_2020))
names(values_df) <- c("V1970","V1980","V2010","V2020")

# Reclassifying NaN as NA
values_df <- values_df %>% mutate_all(~ifelse(is.nan(.), NA, .))

# Creating cell IDs
values_df$raster_cell <- as.character(c(1:nrow(values_df)))


### Part 2: creating geometry (point) associated with each chip -----

# Using the 1970s raster as template
raster <- stack[[1]]
# Turning all values to 1
raster <- classify(raster, cbind(0, 5, 1), right=FALSE)
raster <- classify(raster, cbind(NaN, 1))

# Convert raster to sf object
raster_sf <- st_as_stars(raster)
raster_sf <- st_as_sf(raster_sf) # takes a minute

# Calculate centroids
centroids <- st_centroid(raster_sf)
centroids <- centroids[,-1]
centroids$raster_cell <- values_df$raster_cell

### Part 3: Adding other chip information -----
# Load in the shapefile for the tiles used in computer vision
grid <- st_read("./NETR_lookup/grid_full_dtrange_crop.shp")

# Find the tile ID each chip intersects with
points_w_grid <- st_join(centroids, grid)

# Load in states shapefile
states <- st_read("./shapefiles/us_states/states_web.shp")
states <- states[,c("NAME","geometry")]

# Find the state that each chip intersects with
points_w_grid_state <- st_join(points_w_grid, states)

# Removing cells that are all NA (meaning they are outside the range)
values_df <- values_df[!rowSums(is.na(values_df)) == 4, ]

# Joining all data together
values_df <- values_df %>% left_join(points_w_grid_state, by = "raster_cell")
head(values_df)
unique(values_df$V70)

names(values_df) <- c("V1970","V1980","V2010","V2020","raster_cell","grid_cell","state","geometry")

### Part 4: Saving outputs -----
# Saving as a shapefile with geometry
if(dir.exists("./other_data/master") == FALSE){dir.create("./other_data/master")}
st_write(values_df,"./other_data/master/master_cells_cleaned.shp",append=FALSE)

# Removing geometry to save as a .csv
values_df_no_geom <- as.data.frame(values_df[,-8])
head(values_df_no_geom)

write.csv(values_df_no_geom,"./other_data/master/master_cells_cleaned.csv")
