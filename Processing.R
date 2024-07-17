rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","googleCloudStorageR","googleAuthR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Use the JSON file to authenticate communication between RStudio and GCS
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)

if(dir.exists("./other_data") == FALSE){dir.create("./other_data")}
if(dir.exists("./other_data/masks") == FALSE){dir.create("./other_data/masks")}
# Load in OHV layers

n70 <- rast("./output_layers/NETR_NOC_1970_full_web_cropped.tif")
n80 <- rast("./output_layers/NETR_1980_full_web_cropped.tif")

N10 <- rast("./output_layers/NAIP_2010_cropped.tif")
N20 <- rast("./output_layers/NAIP_2020_cropped.tif")


# Stack check
stack <- c(n70,n80,N10,N20)

names(stack) <- c("netr_1970","netr_1980","NAIP_2010","NAIP_2020")

plot(stack,main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack[[1]],"./output_layers/n70.tif", overwrite = TRUE)
writeRaster(stack[[2]],"./output_layers/n80.tif", overwrite = TRUE)
writeRaster(stack[[3]],"./output_layers/N10.tif", overwrite = TRUE)
writeRaster(stack[[4]],"./output_layers/N20.tif", overwrite = TRUE)

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
writeRaster(mask,"./other_data/masks/small_ext.tif")


# Create binary raster stack and then run both categorical and binary layers through ohv sum with radius 400m and radius 200m

stack_binary <- classify(stack, cbind(1, 5, 1), right=FALSE)
names(stack_binary) <- c("netr_1970","netr_1980","NAIP_2010","NAIP_2020")

plot(stack_binary, main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack_binary[[1]],"./output_layers/n70_bin.tif", overwrite = TRUE)
writeRaster(stack_binary[[2]],"./output_layers/n80_bin.tif", overwrite = TRUE)
writeRaster(stack_binary[[3]],"./output_layers/N10_bin.tif", overwrite = TRUE)
writeRaster(stack_binary[[4]],"./output_layers/N20_bin.tif", overwrite = TRUE)


stack_high <- classify(stack, cbind(0, 3, 0), right=FALSE)
stack_high <- classify(stack_high, cbind(4, 5, 1), right=FALSE)

names(stack_high) <- c("netr_1970","netr_1980","NAIP_2010","NAIP_2020")

plot(stack_high, main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack_high[[1]],"./output_layers/n70_high.tif", overwrite = TRUE)
writeRaster(stack_high[[2]],"./output_layers/n80_high.tif", overwrite = TRUE)
writeRaster(stack_high[[3]],"./output_layers/N10_high.tif", overwrite = TRUE)
writeRaster(stack_high[[4]],"./output_layers/N20_high.tif", overwrite = TRUE)


# Uploaded to data > 05 covariate outputs > OHV


# Apply moving window

source("./Functions.R")


stack_foc <- sum_window(stack, radius = 400, writeR = FALSE)
plot(stack_foc)
writeRaster(stack_foc,"./output_layers/OHV_categorical_sum_800m.tif")


stack_foc_200 <- sum_window(stack, radius = 200, writeR = FALSE)
plot(stack_foc_200)
writeRaster(stack_foc_200,"./output_layers/OHV_categorical_sum_400m.tif")


stack_foc <- sum_window(stack_binary, radius = 400, writeR = FALSE)
plot(stack_foc)
writeRaster(stack_foc,"./output_layers/OHV_binary_sum_800m.tif")


stack_foc_200 <- sum_window(stack_binary, radius = 200, writeR = FALSE)
plot(stack_foc_200)
writeRaster(stack_foc_200,"./output_layers/OHV_binary_sum_400m.tif")


