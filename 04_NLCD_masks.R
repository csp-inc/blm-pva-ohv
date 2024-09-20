## ---------------------------
##
## Script name: 04_NLCD_masks.R
##
## This script creates masks for the OHV layers using the NLCD data.
##
## Author: Madeline Standen
##
## Date Created: 02/__/2024
## Date last updated: 09/20/2024
##
## Email contact: madi[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
# This script creates masks for the OHV layers using the NLCD data.
# This data is downloaded from GCS, filtered for certain land cover types, and then turned into a mask layer.
# These raster masks are then saved and can be applied using functions created in the Stats_Func script.

rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","googleCloudStorageR","googleAuthR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Use the JSON file to authenticate communication between RStudio and GCS -----
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

## Create necessary local folders -----
if(dir.exists("./other_data/nlcd") == FALSE){dir.create("./other_data/nlcd")}
if(dir.exists("./other_data/masks") == FALSE){dir.create("./other_data/masks")}

# Loading in the MDT range
dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

# Downloading the NLCD data from GCS
# 1) Backcast data for 1979 and 1989
# https://www.sciencebase.gov/catalog/item/59d3c73de4b05fe04cc3d1d1
backcast_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/03_environmental/LULC/Sohl_Backcast")
backcast_contents <- backcast_contents$name
backcast_contents <- backcast_contents[grepl("1979|1989",backcast_contents)]

purrr::map(backcast_contents, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./other_data/nlcd/",basename(x))))

# 2) Contemporary data for 2013 and 2021
# https://www.mrlc.gov/data
contemporary_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/03_environmental/LULC/NLCD_contemporary")
contemporary_contents <- contemporary_contents$name
contemporary_contents <- contemporary_contents[grepl("2013|2021",contemporary_contents)]

purrr::map(contemporary_contents, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./other_data/nlcd/",basename(x))))

# Reading in the saved data
nlcd_1970 <- rast("./other_data/nlcd/MDT_LULC_sohl_backcast_1979.tif")
nlcd_1980 <- rast("./other_data/nlcd/MDT_LULC_sohl_backcast_1989.tif")
nlcd_2010 <- rast("./other_data/nlcd/MDT_NLCD_2013.tif")
nlcd_2020 <- rast("./other_data/nlcd/MDT_NLCD_2021.tif")

# Projecting the 240m into 30m to align with contemporary
nlcd_1970 <- project(nlcd_1970,nlcd_2010, method = "near")
nlcd_1980 <- project(nlcd_1980,nlcd_2010, method = "near")

# Reclassifying water and developed for mask
nlcd_1970 <- classify(nlcd_1970, cbind(0, 3, NaN), right=FALSE)

nlcd_1980 <- classify(nlcd_1980, cbind(0, 3, NaN), right=FALSE)

nlcd_2010 <- classify(nlcd_2010, cbind(22, 25, NaN), right=FALSE)
nlcd_2010 <- classify(nlcd_2010, cbind(11, 12, NaN), right=FALSE)

nlcd_2020 <- classify(nlcd_2020, cbind(22, 25, NaN), right=FALSE)
nlcd_2020 <- classify(nlcd_2020, cbind(11, 12, NaN), right=FALSE)

# Creating a template raster
r <- rast("./other_data/nlcd/MDT_NLCD_2021.tif")
# Replacing all non-NA values with 1
r[!is.na(r)] <- 1
plot(r)
# Projecting to web mercator
r <- project(r,"EPSG:3857", method = "near")
# Masking and cropping to MDT range
r_mdt <- mask(crop(r,dt_range),dt_range)

rast_list <- list()
rast_list[[1]] <- nlcd_1970
rast_list[[2]] <- nlcd_1980
rast_list[[3]] <- nlcd_2010
rast_list[[4]] <- nlcd_2020

# Loading in the 2010 layer as a template for NLCD masks
r <- rast("./output_layers/NAIP_2010_cat.tif")

mask_list <- list()
for(i in 1:4){
  # Projecting the NLCD data to web mercator
  rast_web <- project(rast_list[[i]], "EPSG:3857", method = "near")
  # Masking and cropping to the MDT range
  rast_mdt <- mask(crop(rast_web,dt_range),dt_range)
  # Using the NLCD data to mask the blank raster and create a mask
  mask_mdt <- mask(r_mdt,rast_mdt, inverse = TRUE, updatevalue = NA)
  # Creating a layer that represents the number of 30m NLCD cells intersect with each 150m OHV cell
  mask_list[[i]] <- project(mask_mdt, ohv_rast, method = 'sum')
}

par(mfrow = c(2,2))
plot(mask_list[[1]],main = "1970s Mask")
plot(mask_list[[2]],main ="1980s Mask")
plot(mask_list[[3]],main ="2010s Mask")
plot(mask_list[[4]],main ="2020s Mask")

# Saving these masks for potential use later for more selective masking
writeRaster(mask_list[[1]],"./other_data/masks/NLCD/mask_1970_sum.tif")
writeRaster(mask_list[[2]],"./other_data/masks/NLCD/mask_1980_sum.tif")
writeRaster(mask_list[[3]],"./other_data/masks/NLCD/mask_2010_sum.tif")
writeRaster(mask_list[[4]],"./other_data/masks/NLCD/mask_2020_sum.tif")


# # Reclassifying the the sum masks to any cell with 1 30m nlcd cell is classified as 1 to be masked
for (i in 1:4){
  mask_list[[i]] <- classify(mask_list[[i]], cbind(0, 1, NaN), right=FALSE)
  mask_list[[i]] <- classify(mask_list[[i]], cbind(1, 25, 1), right=FALSE)
}

par(mfrow = c(2,2))
plot(mask_list[[1]],main = "1970s Mask")
plot(mask_list[[2]],main ="1980s Mask")
plot(mask_list[[3]],main ="2010s Mask")
plot(mask_list[[4]],main ="2020s Mask")


writeRaster(mask_list[[1]],"./other_data/masks/NLCD/mask_1970_n21.tif")
writeRaster(mask_list[[2]],"./other_data/masks/NLCD/mask_1980_n21.tif")
writeRaster(mask_list[[3]],"./other_data/masks/NLCD/mask_2010_n21.tif")
writeRaster(mask_list[[4]],"./other_data/masks/NLCD/mask_2020_n21.tif")




