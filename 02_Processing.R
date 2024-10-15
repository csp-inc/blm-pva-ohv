## ---------------------------
##
## Script name: 02_Processing.R
##
## This processes the the range-wide OHV inference rasters by reclassifying the OHV route density values.
##
## Author: Madeline Standen
##
## Date Created: 02/__/2024
## Date last updated: 10/10/2024
##
## Email contact: madi[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##


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


# Load in OHV layers created in Mosaic.R

n70 <- rast("./output_layers/NETR_NOC_1970_full_web_cropped.tif")
n80 <- rast("./output_layers/NETR_1980_full_web_cropped.tif")

N10 <- rast("./output_layers/NAIP_2010_cropped.tif")
N20 <- rast("./output_layers/NAIP_2020_cropped.tif")


# Stack check
stack <- c(n70,n80,N10,N20)

names(stack) <- c("netr_1970_cat","netr_1980_cat","NAIP_2010_cat","NAIP_2020_cat")

plot(stack,main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack[[1]],"./output_layers/netr_1970_cat.tif", overwrite = TRUE)
writeRaster(stack[[2]],"./output_layers/netr_1980_cat.tif", overwrite = TRUE)
writeRaster(stack[[3]],"./output_layers/NAIP_2010_cat.tif", overwrite = TRUE)
writeRaster(stack[[4]],"./output_layers/NAIP_2020_cat.tif", overwrite = TRUE)

# Looking at coverage for each decade
decade_coverage <- classify(stack, cbind(0, 5, 1), right=FALSE)
plot(decade_coverage)

# Creating a mask layer that represents all NAs across time
mask <- N20
mask <- mask(mask,N10)
mask <- mask(mask,n80)
mask <- mask(mask,n70)
mask <- classify(mask, cbind(0, 5, 1), right=FALSE)
plot(mask)
writeRaster(mask,"./other_data/masks/small_ext.tif",overwrite=TRUE)


# Create raster stacks classified in different ways

# 1) Binary (presence absence layer)
stack_binary <- classify(stack, cbind(1, 5, 1), right=FALSE)

names(stack_binary) <- c("netr_1970_bin","netr_1980_bin","NAIP_2010_bin","NAIP_2020_bin")

plot(stack_binary, main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack_binary[[1]],"./output_layers/netr_1970_bin.tif", overwrite = TRUE)
writeRaster(stack_binary[[2]],"./output_layers/netr_1980_bin.tif", overwrite = TRUE)
writeRaster(stack_binary[[3]],"./output_layers/NAIP_2010_bin.tif", overwrite = TRUE)
writeRaster(stack_binary[[4]],"./output_layers/NAIP_2020_bin.tif", overwrite = TRUE)

# 2) High (ohv class high (4) is 1, else is 0)
stack_high <- classify(stack, cbind(0, 3, 0), right=FALSE)
stack_high <- classify(stack_high, cbind(4, 5, 1), right=FALSE)

names(stack_high) <- c("netr_1970_high","netr_1980_high","NAIP_2010_high","NAIP_2020_high")

plot(stack_high, main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack_high[[1]],"./output_layers/netr_1970_high.tif", overwrite = TRUE)
writeRaster(stack_high[[2]],"./output_layers/netr_1980_high.tif", overwrite = TRUE)
writeRaster(stack_high[[3]],"./output_layers/NAIP_2010_high.tif", overwrite = TRUE)
writeRaster(stack_high[[4]],"./output_layers/NAIP_2020_high.tif", overwrite = TRUE)

# 3) Merged (ohv class low and medium (1 and 2) is merged into 2)
stack_merged <- classify(stack, cbind(1, 3, 2), right=FALSE)

names(stack_merged) <- c("netr_1970_merged","netr_1980_merged","NAIP_2010_merged","NAIP_2020_merged")

plot(stack_merged, main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack_merged[[1]],"./output_layers/netr_1970_merged.tif", overwrite = TRUE)
writeRaster(stack_merged[[2]],"./output_layers/netr_1980_merged.tif", overwrite = TRUE)
writeRaster(stack_merged[[3]],"./output_layers/NAIP_2010_merged.tif", overwrite = TRUE)
writeRaster(stack_merged[[4]],"./output_layers/NAIP_2020_merged.tif", overwrite = TRUE)


# Uploaded to data > 05 covariate outputs > OHV

