## ---------------------------
##
## Script name: 07_All_masks.R
##
## This script uses the OHV layers that have been cleaned and masked for NLCD and roads and creates rasters that can be 
## used for creating plots for the OHV manuscript
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
if(dir.exists("./other_data/masks") == FALSE){dir.create("./other_data/masks")}
if(dir.exists("./other_data/masks/all_masks") == FALSE){dir.create("./other_data/masks/all_masks")}


n1970 <- rast("./output_layers/netr_1970_cat_masked_9_nlcdmask_roadsmask.tif")
n1980 <- rast("./output_layers/netr_1980_cat_masked_9_nlcdmask_roadsmask.tif")
N2010 <- rast("./output_layers/NAIP_2010_cat_masked_9_nlcdmask_roadsmask.tif")
N2020 <- rast("./output_layers/NAIP_2020_cat_masked_9_nlcdmask_roadsmask.tif")

rast_list <- list()
rast_list[[1]] <- n1970
rast_list[[2]] <- n1980 
rast_list[[3]] <- N2010
rast_list[[4]] <- N2020

blank_rast <- N2020

values(blank_rast) <- 1
plot(blank_rast)
dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

blank_rast <- mask(blank_rast,dt_range)
plot(blank_rast)

for(i in 1:length(rast_list)){
  mask <- mask(blank_rast,rast_list[[i]], inverse = TRUE)
  mask <- classify(mask, cbind(0, 5, 1), right=FALSE)
  writeRaster(mask ,paste0("./other_data/masks/all_masks/",names(rast_list[[i]]),"_MASK.tif"))
}

# Making merged 1980s and 2020s mask for mapping

mask_80s <- rast("./other_data/masks/all_masks/netr_1980_masked_9_nlcdmask_roadsmask_MASK.tif")

mask_2020s <- rast("./other_data/masks/all_masks/NAIP_2020_masked_9_nlcdmask_roadsmask_MASK.tif")

merged_mask <- terra::mosaic(mask_80s,mask_2020s,fun = "max")
plot(merged_mask)
writeRaster(merged_mask, "./other_data/masks/all_masks/Merged_80s_2020s_masked_9_nlcdmask_roadsmask_MASK.tif")


