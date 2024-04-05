## Creating masks for OHV using NLCD landcover

gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

# Create a new folder on local device 
if(dir.exists("./other_data/nlcd") == FALSE){dir.create("./other_data/nlcd")}
if(dir.exists("./other_data/masks") == FALSE){dir.create("./other_data/masks")}

# Loading in the contemporary LULC data
gcs_get_object("gs://csp_tortoisehub/data/05_covariate_outputs/LULC/LULC_allyrs_8class_30m.tif",
               saveToDisk = "./other_data/nlcd/LULC_allyrs_8class_30m.tif", overwrite = TRUE)

land_use_stack <- rast("./other_data/nlcd/LULC_allyrs_8class_30m.tif")

names(land_use_stack)

land_use_stack <- classify(land_use_stack, cbind(1, 3, 1), right=FALSE) # turning all water and developed to 1
land_use_stack <- classify(land_use_stack, cbind(3, 25, NA), right=FALSE) # all other classes to NA

land_use_1970 <- max(land_use_stack[[1:10]], na.rm = TRUE) # 1970-1979
land_use_1980 <- max(land_use_stack[[11:20]], na.rm = TRUE) # 1980-1989
land_use_2010 <- max(land_use_stack[[41:43]], na.rm = TRUE) # 2010-2012
land_use_2020 <- max(land_use_stack[[50:53]], na.rm = TRUE) # 2019-2022


plot(land_use_1970)
dec_list <- list()
dec_list[[1]] <- land_use_1970
dec_list[[2]] <- land_use_1980
dec_list[[3]] <- land_use_2010
dec_list[[4]] <- land_use_2020

dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

ohv_rast <- rast("./output_layers/NAIP_2010_cropped.tif")


mask_list <- list()
for (i in 1:4){
  rast <- dec_list[[i]]
  rast_web <- project(rast, "EPSG:3857", method = "near")
  rast_mdt <- mask(crop(rast_web,dt_range),dt_range)
  mask_list[[i]] <- project(rast_mdt, ohv_rast, method = 'sum')
}

# # Reclassifying the 150m NLCD depending on the cut off for the mask
for (i in 1:4){
mask_list[[i]] <- classify(mask_list[[i]], cbind(0, 1, NaN), right=FALSE)
mask_list[[i]] <- classify(mask_list[[i]], cbind(1, 25, 1), right=FALSE)
}


par(mfrow = c(2,2))
plot(mask_list[[1]],main = "1970s Mask")
plot(mask_list[[2]],main ="1980s Mask")
plot(mask_list[[3]],main ="2010s Mask")
plot(mask_list[[4]],main ="2020s Mask")

if(dir.exists("./other_data/masks/NLCD") == FALSE){dir.create("./other_data/masks/NLCD")}
writeRaster(mask_list[[1]],"./other_data/masks/NLCD/mask_1970.tif")
writeRaster(mask_list[[2]],"./other_data/masks/NLCD/mask_1980.tif")
writeRaster(mask_list[[3]],"./other_data/masks/NLCD/mask_2010.tif")
writeRaster(mask_list[[4]],"./other_data/masks/NLCD/mask_2020.tif")

