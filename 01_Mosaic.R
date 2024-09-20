## ---------------------------
##
## Script name: 01_Mosaic.R
##
## This script pulls in the inference output tiles from GCS and merges them together to form
## range wide OHV layers.
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
##
## This script pulls in the inference output tiles from GCS and merges them together to form
## range wide OHV layers.
##
## For the 2010 and 2020 decades, the inference outputs live in a single folder in GCS.
## For the 1970 and 1980 decades, the inference outputs live in different folders,
## which represent CA and non-CA extents of the range.
##
## In addition, for the 1970s decade only, there BLM NOC data is used to augment coverage
## for this time step. This data is pulled in separately from the NETR data and 
## then merged together.


rm(list=ls())

## Load packages -----

list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer","googleCloudStorageR","googleAuthR",
                      "remotes","purrr","foreach","doParallel","furrr","tictoc","exactextractr","stars")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Use the JSON file to authenticate communication between RStudio and GCS -----
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name <- "gs://pva_image_processing"

## Create necessary local folders -----
if(dir.exists("./output_layers") == FALSE){dir.create("./output_layers")}

##### 2010 -----
# Create a new folder
if(dir.exists("./raw_inference/NAIP_2010_2012") == FALSE){dir.create("./raw_inference/NAIP_2010_2012")}

# Get contents of folder in GCS you wish to download to local device
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NAIP/NAIP_2010_2012_test3857_max_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/NAIP_2010_2012","/",basename(x))))


# query all cleaned OHV output tiles
ohv_2010_cleaned.files = Sys.glob("./raw_inference/NAIP_2010_2012/*.tif")
# coerce tiles to spatraster collection object
ohv_2010_cleaned.sprc = sprc(ohv_2010_cleaned.files) 
# merge spatraster collection
ohv_2010_cleaned.merged = merge(ohv_2010_cleaned.sprc)
# three identical layers, so plot only the first one
plot(ohv_2010_cleaned.merged[[1]], background="black")
# separating the first layer
NAIP_2010_mosaic <- ohv_2010_cleaned.merged[[1]]

# Loading in the MDT range
# Creating a directory to hold the shapefile
if(dir.exists("./shapefiles") == FALSE){dir.create("./shapefiles")}
if(dir.exists("./shapefiles/DTrange") == FALSE){dir.create("./shapefiles/DTrange")}

contents <- gcs_list_objects(bucket = "gs://csp_tortoisehub",
                             prefix = "data/02_tortoise/DTrange/")
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/DTrange","/",basename(x))))

dt_range_web <- st_read("./shapefiles/DTrange/dtrange.shp") %>% st_transform("EPSG:3857")
st_write(dt_range_web,"./shapefiles/DTrange/dtrange_web.shp",append=FALSE)

dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")

# Masking and cropping to the MDT range
NAIP_2010_mosaic <- mask(crop(NAIP_2010_mosaic,dt_range_web),dt_range_web)
plot(NAIP_2010_mosaic,background="black")

# Writing output layer
writeRaster(NAIP_2010_mosaic,"./output_layers/NAIP_2010_rerun_web.tif", overwrite = TRUE)

# Loading in shapefile for cropping created in QGIS that removes areas where inference failed
crop_2010 <- st_read("./shapefiles/cropping/2010_crop.shp")
# Masking and cropping to this layer
NAIP_2010_mosaic_cropped <- mask(NAIP_2010_mosaic,crop_2010)

# Writing output layer
writeRaster(NAIP_2010_mosaic_cropped,"./output_layers/NAIP_2010_cropped.tif", overwrite = TRUE)

##### 2020 ------

# Process for 2020 is identical to 2010

if(dir.exists("./raw_inference/NAIP_2019_2021") == FALSE){dir.create("./raw_inference/NAIP_2019_2021")}

contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NAIP/NAIP_2020_2022_test3857_max_output/")

folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/NAIP_2019_2021","/",basename(x))))


ohv_2020_cleaned.files = Sys.glob("./raw_inference/NAIP_2019_2021/*.tif") 
ohv_2020_cleaned.sprc = sprc(ohv_2020_cleaned.files) 
ohv_2020_cleaned.merged = merge(ohv_2020_cleaned.sprc) 

plot(ohv_2020_cleaned.merged[[1]], background="black") 
NAIP_2020_mosaic <- ohv_2020_cleaned.merged[[1]]

dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")

NAIP_2020_mosaic <- mask(crop(NAIP_2020_mosaic,dt_range_web),dt_range_web)
plot(NAIP_2020_mosaic,background="black")

# Classifying values to coerce raster to be categorical (not changing actual values)
NAIP_2020_mosaic <- classify(NAIP_2020_mosaic, cbind(0, 1, 0), right=FALSE)
NAIP_2020_mosaic <- classify(NAIP_2020_mosaic, cbind(1, 2, 1), right=FALSE)
NAIP_2020_mosaic <- classify(NAIP_2020_mosaic, cbind(2, 4, 2), right=FALSE)
NAIP_2020_mosaic <- classify(NAIP_2020_mosaic, cbind(4, 5, 4), right=FALSE)


writeRaster(NAIP_2020_mosaic,"./output_layers/NAIP_2020_rerun_web.tif", overwrite = TRUE)

crop_2020 <- st_read("./shapefiles/cropping/2020_crop.shp")
NAIP_2020_mosaic_cropped <- mask(NAIP_2020_mosaic,crop_2020)

writeRaster(NAIP_2020_mosaic_cropped,"./output_layers/NAIP_2020_cropped.tif", overwrite = TRUE)


##### 1970s -----

## Loading in some cropping polygons created in QGIS for clipping historical data
if(dir.exists("./shapefiles/cropping") == FALSE){dir.create("./shapefiles/cropping")}

# Get contents of folder in GCS you wish to download to local device
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "cropping/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/cropping","/",basename(x))))

# Netr data
if(dir.exists("./raw_inference/netr_70s") == FALSE){dir.create("./raw_inference/netr_70s")}

# Get contents of 70s outputs for CA extent
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NETR/NETR_70s_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/netr_70s","/",basename(x))))


# Get contents of 70s outputs for non-ca extent
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NETR/NETR_70s_non-ca_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/netr_70s","/",basename(x))))

ohv_70_cleaned.files = Sys.glob("./raw_inference/netr_70s/*.tif") 
ohv_70_cleaned.sprc = sprc(ohv_70_cleaned.files)

# Because of spatial overlap in coverage due to inference process for 70s images
# the collection must be mosaiced and the maximum pixel value should be chosen.
# This ensures that no all 0 inference areas replace actual inference.
ohv_70_cleaned.mosaic = mosaic(ohv_70_cleaned.sprc, fun = "max")

plot(ohv_70_cleaned.mosaic[[1]], background = "black")

# Cropping to the df range
dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")

NETR_1970_full_netr_web <- crop(ohv_70_cleaned.mosaic[[1]],dt_range_web)
NETR_1970_full_netr_web <- mask(NETR_1970_full_netr_web,dt_range_web)
plot(NETR_1970_full_netr_web)

# Classifying values to coerce raster to be categorical (not changing actual values)
NETR_1970_full_netr_web <- classify(NETR_1970_full_netr_web, cbind(0, 1, 0), right=FALSE)
NETR_1970_full_netr_web <- classify(NETR_1970_full_netr_web, cbind(1, 2, 1), right=FALSE)
NETR_1970_full_netr_web <- classify(NETR_1970_full_netr_web, cbind(2, 4, 2), right=FALSE)
NETR_1970_full_netr_web <- classify(NETR_1970_full_netr_web, cbind(4, 5, 4), right=FALSE)

plot(NETR_1970_full_netr_web)

# Saving output cropped to mdt range
writeRaster(NETR_1970_full_netr_web,"./output_layers/NETR_1970_full_netr_web.tif", overwrite = TRUE)

# Load in 1970s coverage shapefiles created in qGIS for the NETR only data
NETR_all <- st_read("./shapefiles/cropping/1970_netr_all.shp")

# Crop the netr 1970s data with the polygon and save
NETR_1970_full_netr_web_cropped <- mask(NETR_1970_full_netr_web,NETR_all)
plot(NETR_1970_full_netr_web_cropped)
writeRaster(NETR_1970_full_netr_web_cropped,"./output_layers/NETR_1970_full_netr_web_cropped.tif", overwrite = TRUE)

# Noc data
if(dir.exists("./raw_inference/noc_70s") == FALSE){dir.create("./raw_inference/noc_70s")}

# Get contents of folder in GCS you wish to download to local device
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NOC/NOC_70s_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/noc_70s","/",basename(x))))


# Get contents of folder in GCS you wish to download to local device
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NOC/NOC_70s_non-ca_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/noc_70s","/",basename(x))))


ohv_70_cleaned.files = Sys.glob("./raw_inference/noc_70s/*.tif") 
ohv_70_cleaned.sprc = sprc(ohv_70_cleaned.files) 
# Again, needing to use mosaic to ensure inference is used
ohv_70_cleaned.mosaic = mosaic(ohv_70_cleaned.sprc,fun = "max")

noc_70_mosaic <- ohv_70_cleaned.mosaic[[1]]
plot(ohv_70_cleaned.mosaic[[1]], background="black") 

writeRaster(noc_70_mosaic,"./output_layers/NOC_1970_full_noc_web.tif")

# Merging the noc and the netr data in the 70s for a full image

# Bring in the netr data
n70 <- rast("./output_layers/NETR_1970_full_netr_web_cropped.tif")

# Bring in the shapefile for the extent of the netr data
NETR_all <- st_read("./shapefiles/cropping/1970_netr_all.shp")
plot(NETR_all)

# Mask the noc data in reverse using this mask, to ensure we are only
# using noc data where there is no netr data available
noc_70_mosaic_masked <- mask(noc_70_mosaic,NETR_all, inverse = TRUE)
plot(noc_70_mosaic_masked)

# Merging the two datasets
NETR_1970s_all <- merge(n70,noc_70_mosaic_masked)
plot(NETR_1970s_all)

# Masking the final raster to the mdt range
dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")
NETR_1970s_all <- mask(NETR_1970s_all,dt_range_web)
plot(NETR_1970s_all)

# Classifying values to coerce raster to be categorical (not changing actual values)
NETR_1970s_all <- classify(NETR_1970s_all, cbind(0, 1, 0), right=FALSE)
NETR_1970s_all <- classify(NETR_1970s_all, cbind(1, 2, 1), right=FALSE)
NETR_1970s_all <- classify(NETR_1970s_all, cbind(2, 4, 2), right=FALSE)
NETR_1970s_all <- classify(NETR_1970s_all, cbind(4, 5, 4), right=FALSE)

plot(NETR_1970s_all)
plot(n70)

writeRaster(NETR_1970s_all,"./output_layers/NETR_NOC_1970_full_web.tif", overwrite = TRUE)

# Load in 1970s coverage shapefiles created in qGIS for the NETR and NOC data
final_70s_crop <- st_read("./shapefiles/cropping/1970_netr_noc_all.shp")

NETR_1970s_all_cropped <- mask(NETR_1970s_all,final_70s_crop)
plot(NETR_1970s_all_cropped)

writeRaster(NETR_1970s_all_cropped,"./output_layers/NETR_NOC_1970_full_web_cropped.tif", overwrite = TRUE)

##### 1980s -----

# Netr data
# Create a new folder on local device 
if(dir.exists("./raw_inference/netr_80s") == FALSE){dir.create("./raw_inference/netr_80s")}

# Get contents of 80s outputs for Ca extent
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NETR/NETR_80s_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/netr_80s","/",basename(x))))


# Get contents of 80s outputs for non-ca extent
contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NETR/NETR_80s_non-ca_output/")

# Uses library purrr to download all contents of folder
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./raw_inference/netr_80s","/",basename(x))))



ohv_80_cleaned.files = Sys.glob("./raw_inference/netr_80s/*.tif") 
ohv_80_cleaned.sprc = sprc(ohv_80_cleaned.files)
ohv_80_cleaned.mosaic = mosaic(ohv_80_cleaned.sprc, fun = "max")

plot(ohv_80_cleaned.mosaic[[1]], background = "black")

dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")
plot(dt_range_web$geometry)

NETR_1980_full_web <- crop(ohv_80_cleaned.mosaic[[1]],dt_range_web)
NETR_1980_full_web <- mask(NETR_1980_full_web,dt_range_web)
plot(NETR_1980_full_web)

# Classifying values to coerce raster to be categorical (not changing actual values)
NETR_1980_full_web <- classify(NETR_1980_full_web, cbind(0, 1, 0), right=FALSE)
NETR_1980_full_web <- classify(NETR_1980_full_web, cbind(1, 2, 1), right=FALSE)
NETR_1980_full_web <- classify(NETR_1980_full_web, cbind(2, 4, 2), right=FALSE)
NETR_1980_full_web <- classify(NETR_1980_full_web, cbind(4, 5, 4), right=FALSE)


writeRaster(NETR_1980_full_web,"./output_layers/NETR_1980_full_web.tif", overwrite = TRUE)

NETR_80s <- st_read("./shapefiles/cropping/1980_netr_all.shp")

NETR_1980_full_web_cropped <- mask(NETR_1980_full_web,NETR_80s)
plot(NETR_1980_full_web_cropped)

writeRaster(NETR_1980_full_web_cropped,"./output_layers/NETR_1980_full_web_cropped.tif", overwrite = TRUE)

