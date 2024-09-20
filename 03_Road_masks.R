## ---------------------------
##
## Script name: 03_Road_masks.R
##
## This script creates masks for the OHV layers using the TIGER line shapefiles.
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
## The roads data is downloaded from GCS, filtered for certain road classes, and then rasterized.
## These raster masks are then saved and can be applied using functions created in the Stats_Func script.

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
if(dir.exists("./other_data") == FALSE){dir.create("./other_data")}
if(dir.exists("./other_data/roads") == FALSE){dir.create("./other_data/roads")}

# Load the mdt range for use later
dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")

# Downloading roads data from GCS
# 1) 1992 data
roads1992_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/05_covariate_outputs/TIGER_roads/TIGER_roads_1992")
roads1992_contents <- roads1992_contents$name
roads1992_contents <- roads1992_contents[grepl("merged",roads1992_contents)]

purrr::map(roads1992_contents, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./other_data/roads/",basename(x))))


# 2) 2012 data
roads2012_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/05_covariate_outputs/TIGER_roads/TIGER_roads_2012")
roads2012_contents <- roads2012_contents$name

purrr::map(roads2012_contents, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./other_data/roads/",basename(x))))

# 3) 2022 data
roads2022_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/05_covariate_outputs/TIGER_roads/TIGER_roads_2022")
roads2022_contents <- roads2022_contents$name

purrr::map(roads2022_contents, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./other_data/roads/",basename(x))))


# Reading in roads data
road_list <- list()
road_list[[1]] <- st_read("./other_data/roads/merged_filtered_roads1992.shp")
road_list[[2]] <- st_read("./other_data/roads/TIGER_2012_roads_merged.shp")
road_list[[3]] <- st_read("./other_data/roads/TIGER_2022_roads_merged.shp")

# Use the 2010 layer as a template to rasterize the roads into
r <- rast("./output_layers/NAIP_2010_cat.tif")

# Create local directories to store masks
if(dir.exists("./other_data/masks") == FALSE){dir.create("./other_data/masks")}
if(dir.exists("./other_data/masks/TIGER") == FALSE){dir.create("./other_data/masks/TIGER")}

# Create output file names
file_names <- c("roads1992_mask.tif","roads2012_mask.tif","roads2022_mask.tif")

# Creating the masks
for (i in 1:length(road_list)){
  # Filter for road types you want (highways and paved county roads)
  road_filt <- road_list[[i]] %>% filter(MTFCC == "S1100" | MTFCC == "S1200" | MTFCC == "S1400")
  # Transform to web mercator
  road_web <- st_transform(road_filt, "EPSG:3857")
  # Crop to the MDT range
  road_crop <- st_crop(road_web,dt_range_web)
  # Vectorize
  road_vect <- vect(road_crop)
  # Rasterize
  road_rast <- rasterize(road_vect, r, field=1)
  # Mask to the MDT range
  road_rast_masked <- mask(road_rast,dt_range_web)
  # Write mask 
  writeRaster(road_rast_masked, paste0("./other_data/masks/TIGER/",file_names[i]))
}

