rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# Load in OHV layers

n70 <- rast("./output_layers/NETR_NOC_1970_full_web_cropped.tif")
n80 <- rast("./output_layers/NETR_1980_full_web_cropped.tif")

N10 <- rast("./output_layers/NAIP_2010_cropped.tif")
N20 <- rast("./output_layers/NAIP_2020_cropped.tif")


# Stack check
stack <- c(n70,n80,N10,N20)

names(stack) <- c("netr_1970","netr_1980","NAIP_2010","NAIP_2020")

plot(stack,main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack[[1]],"./output_layers/n70_04052024.tif", overwrite = TRUE)
writeRaster(stack[[2]],"./output_layers/n80_04052024.tif", overwrite = TRUE)
writeRaster(stack[[3]],"./output_layers/N10_04052024.tif", overwrite = TRUE)
writeRaster(stack[[4]],"./output_layers/N20_04052024.tif", overwrite = TRUE)

# Create binary raster stack and then run both categorical and binary layers through ohv sum with radius 400m and radius 200m

stack_binary <- classify(stack, cbind(1, 5, 1), right=FALSE)
names(stack_binary) <- c("netr_1970","netr_1980","NAIP_2010","NAIP_2020")

plot(stack_binary, main = c("netr_1970","netr_1980","NAIP_2010","NAIP_2020"))

writeRaster(stack_binary[[1]],"./output_layers/n70_bin_04052024.tif", overwrite = TRUE)
writeRaster(stack_binary[[2]],"./output_layers/n80_bin_04052024.tif", overwrite = TRUE)
writeRaster(stack_binary[[3]],"./output_layers/N10_bin_04052024.tif", overwrite = TRUE)
writeRaster(stack_binary[[4]],"./output_layers/N20_bin_04052024.tif", overwrite = TRUE)

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




# 
# 
# 
# for(i in 1:nlyr(stack_binary)){
# raster <- stack_binary[[i]]
# focal_num<-400
# focal_shape<-'circle'
# foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
# foc_mat[foc_mat>0] <- 1
# stack_binary_sum[[i]] <-focal(raster, foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE)
# 
# raster <- stack[[i]]
# focal_num<-400
# focal_shape<-'circle'
# foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
# foc_mat[foc_mat>0] <- 1
# stack_sum[[i]] <-focal(raster, foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE)
# }
# 
# names(stack_binary_sum) <- c("NETR_full_1970_bin_sum_400","NETR_full_1980_bin_sum_400","NAIP_full_2010_bin_sum_400","NAIP_full_2020_bin_sum_400")
# names(stack_sum) <- c("NETR_full_1970_sum_400","NETR_full_1980_sum_400","NAIP_full_2010_sum_400","NAIP_full_2020_sum_400")
# 
# 
# plot(stack_binary_sum)
# plot(stack_sum)
# 
# writeRaster(stack_binary_sum,"./output_layers/OHV_binary_sum_800m.tif")
# writeRaster(stack_sum,"./output_layers/OHV_categorical_sum_800m.tif")
# 
# 
# 
# stack_binary_sum <- stack_binary
# stack_sum <- stack
# 
# for(i in 1:nlyr(stack_binary)){
#   raster <- stack_binary[[i]]
#   focal_num<-200
#   focal_shape<-'circle'
#   foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
#   foc_mat[foc_mat>0] <- 1
#   stack_binary_sum[[i]] <-focal(raster, foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE)
#   
#   raster <- stack[[i]]
#   focal_num<-200
#   focal_shape<-'circle'
#   foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
#   foc_mat[foc_mat>0] <- 1
#   stack_sum[[i]] <-focal(raster, foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE)
# }
# 
# names(stack_binary_sum) <- c("NETR_full_1970_bin_sum_200","NETR_full_1980_bin_sum_200","NAIP_full_2010_bin_sum_200","NAIP_full_2020_bin_sum_200")
# names(stack_sum) <- c("NETR_full_1970_sum_200","NETR_full_1980_sum_200","NAIP_full_2010_sum_200","NAIP_full_2020_sum_200")
# 
# plot(stack_binary_sum)
# plot(stack_sum)
# 
# writeRaster(stack_binary_sum,"./output_layers/OHV_binary_sum_400m.tif")
# writeRaster(stack_sum,"./output_layers/OHV_categorical_sum_400m.tif")
# 
