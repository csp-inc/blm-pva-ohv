rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","purrr","foreach","doParallel","furrr","tictoc","exactextractr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Extracting decade stats -----

n70 <- rast("./output_layers/n70_04052024.tif")
n80 <- rast("./output_layers/n80_04052024.tif")
N10 <- rast("./output_layers/N10_04052024.tif")
N20 <- rast("./output_layers/N20_04052024.tif")

# Stacking
stack <- c(n70,n80,N10,N20)
writeRaster(stack,"./output_layers/Final_Stack.tif",overwrite=TRUE)

par(mfrow = c(2,2))
plot(n70, main = "netr 70s",colNA="black")
plot(n80, main = "netr 80s",colNA="black")
plot(N10, main = "NAIP 2010",colNA="black")
plot(N20, main = "NAIP 2020",colNA="black")

dev.off()


# https://stackoverflow.com/questions/41554006/r-focal-raster-conditional-filter-only-run-if-window-center-is-value-1


foc_mat<-matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)
  
  gol_fun <- function(x) {
    
    #Get the center cell
    center <- x[5]
    
    # if the center is 0 or NA, dont run
    if (center==0 | is.na(center)) { 
      return(center)
    } else {
    
    # Find the sum of all the cells, minus value of center cell
    ncells <- sum(x, na.rm=TRUE)
    ncells  <- ncells - x[5]
    
    # If sum of all cells around center is 0, make center cell 1
    if (ncells==0) { 
      return(1)
    } # If sum of cells around center is greater than 1, make 0
    else { 
      return(0)
    }
    }
  }
  
  # Create the function saltNpepper
  saltNpepper <- function(x) {
    f <- focal(x, w=foc_mat, fun=gol_fun)
  }
  

for (i in 1:4){
  raster <- stack[[i]]
  salt_mask <- saltNpepper(raster)
  pepper_mask<- classify(salt_mask, cbind(0,NA), right=FALSE)
  raster_masked <- mask(raster,pepper_mask, inverse = TRUE, updatevalue = 0)
  writeRaster(raster_masked, file = paste0("./output_layers/",names(raster),"_masked_9.tif"),overwrite = TRUE)
}
  
  
## Cleaning for the NLCD data
  
salt_clean_files <- list.files(path = "./output_layers", recursive = TRUE, full.names = TRUE, pattern = "masked")
salt_clean_files <- salt_clean_files[c(3,4,1,2)]
salt_clean_stack <- rast(salt_clean_files)
# plot(salt_clean_stack)

nlcd_mask_files <-  list.files(path = "./other_data/NLCD_masks", recursive = TRUE, full.names = TRUE)
nlcd_mask_stack <- rast(nlcd_mask_files)
# plot(nlcd_mask_stack)

dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

salt_nlcd_clean_stack <- list()
for (i in 1:4){
  layer <- extend(salt_clean_stack[[i]],dt_range)
  salt_nlcd_clean_stack[[i]] <- mask(layer,nlcd_mask_stack[[i]], inverse = TRUE)
}
  
salt_nlcd_clean_stack <- rast(salt_nlcd_clean_stack)
plot(salt_nlcd_clean_stack)







# test_rast = rast(ncols=25, nrows=25, nlyrs=1, xmin=-13239466, xmax=-13238716, ymin=3876765, ymax=3877515, names=c('test'), crs='EPSG:3857')
# test_rast
# 
# possible_values <- c(1,1,2,4,0,0,0,0,0,0,0,0,0,0,0,NA)
# 
# values(test_rast) <- c(replicate(625, sample(possible_values)))[1:625]
# 
# plot(test_rast, background = "black")
# 
# 
# foc_mat<-matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)
# 
# gol_fun <- function(x) {
#   
#   #Get the center cell
#   center <- x[5]
#   
#   # if the center is 0 or NA, dont run
#   if (center==0 | is.na(center)) { 
#     return(center)
#   } else {
#     
#     # Find the sum of all the cells, minus value of center cell
#     ncells <- sum(x, na.rm=TRUE)
#     ncells  <- ncells - x[5]
#     
#     # If sum of all cells around center is 0, make center cell 1
#     if (ncells==0) { 
#       return(1)
#     } # If sum of cells around center is greater than 1, make 0
#     else { 
#       return(0)
#     }
#   }
# }
# 
# result <- saltNpepper(test_rast)
# 
# plot(result)
# pepper_mask<- classify(result, cbind(0,NA), right=FALSE)
# 
# test_masked <- mask(test_rast,pepper_mask, inverse = TRUE, updatevalue = 0)
# 
# par(mfrow = c(1,2))
# plot(test_rast)
# plot(test_masked)


