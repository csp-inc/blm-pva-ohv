## ---------------------------
##
## Script name: 05_Stats_func.R
##
## This script applies functions stored in script Functions.R to the OHV layers and creates visualizations.
##
## Author: Madeline Standen
##
## Date Created: 02/__/2024
## Date last updated: 1/9/2025
##
## Email contact: madi[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
## This is where the moving windows are applied to the various reclassified/cleaned OHV layers.
## The outputs of these moving window functions should be uploaded to data > 06_covariates_post_focal > OHV_routes_roads


rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","googleCloudStorageR","googleAuthR","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Use the JSON file to authenticate communication between RStudio and GCS -----
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

## Create necessary local folders -----

## Load in the necessary functions -----
source("./Functions.R")

n1970 <- rast("./output_layers/netr_1970_cat.tif")
n1980 <- rast("./output_layers/netr_1980_cat.tif")
N2010 <- rast("./output_layers/NAIP_2010_cat.tif")
N2020 <- rast("./output_layers/NAIP_2020_cat.tif")

stack <- c(n1970,n1980,N2010,N2020)


## Part I: Creating summary visualizations -----
output <- class_summary(stack)


# # Proportion of total mdt area with each OHV class
# ggplot(output, aes(fill=Class, y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
#   ylab("Percent of Range Coverage") +
#   scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\ndensity category",
#                     labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
#                                                                                      axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))

# Proportion of classified area with each OHV class
ggplot(output, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 5, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of classified crea") + ggtitle("Stats raw")+
  scale_fill_manual(values = c("#440154","#31688e","#35b779","#fde725"),name = "OHV route\ndensity category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black", size = 12),
                                                                                     axis.text.y = element_text(color = "black", size = 12),legend.title = element_text(face = "bold"))

# Joins classes medium and high into one category
output_join <- join2_4(output)

# # Proportion of total mdt area with each OHV class
# ggplot(output_join, aes(fill= factor(Class, c("0","1","2")), y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Range Coverage") +
#   scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
#                     name="OHV route\ndensity category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
#                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))
# 

# Proportion of classified area with each OHV class
ggplot(output_join, aes(fill= factor(Class, c("0","1","2")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
                    name="OHV route\ndensity category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                    axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


# Joins classes low, medium and high into one category
output_binary <- join1_4(output)

# # Proportion of total mdt area with each OHV class
# ggplot(output_binary, aes(fill= factor(Class, c("0","1")), y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Range Coverage") +
#   scale_fill_manual(values = c("#30123b","#28bceb"),labels=c("None","≥1 Track"),
#                     name="OHV route\ndensity category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
#                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))
# 
# Proportion of classified area with each OHV class
ggplot(output_binary, aes(fill= factor(Class, c("0","1")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb"),labels=c("None","≥1 Track"),
                    name="OHV route\ndensity category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                    axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


## Part II: Creating visualizations with cleaned layers -----

# Function created in Functions.R
# This function turns any cell with a value greater to 0 to 0 IF that cell is surrounded by 0 (aka it removes floating road pixels)
salt_cleaned_stack <- salt_clean(stack, writeR = FALSE)

plot(salt_cleaned_stack)

output_salt_cleaned <- class_summary(salt_cleaned_stack)

# Proportion of classified area with each OHV class
ggplot(output_salt_cleaned, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\ndensity category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))

# Function created in Functions.R
# This function applies the nlcd mask created in 03_NLCD_masks.R to the ohv layer
# Creates "cleaned"
stack_masked_nlcd <- nlcd_mask(salt_cleaned_stack, writeR = FALSE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
plot(stack_masked_nlcd)
output_nlcd_mask <- class_summary(stack_masked_nlcd)


# Proportion of classified area with each OHV class
ggplot(output_nlcd_mask, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed and nlcd mask") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\ndensity category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


# Function created in Functions.R
# This function applies the roads mask created in 03_Road_masks.R to the ohv layer
# Creates "cleaned2"
stack_masked_roads <- roads_mask(salt_cleaned_stack, writeR = FALSE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
plot(stack_masked_roads)
output_roads_mask <- class_summary(stack_masked_roads)


# Proportion of classified area with each OHV class
ggplot(output_roads_mask, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed and roads mask") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\ndensity category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))

# Applies the roads mask function to the stack already masked for NLCD
# Creates "cleaned3"
stack_masked_nlcd_roads <- roads_mask(stack_masked_nlcd, writeR = FALSE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
plot(stack_masked_nlcd_roads)
output_nlcd_roads_mask <- class_summary(stack_masked_nlcd_roads)


# Proportion of classified area with each OHV class
ggplot(output_nlcd_roads_mask, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed and nlcd and roads mask") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\ndensity category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



## Part III: Running loop to clean layers and run moving window over them and save output layers -----

classifications <- c("cat","bin","high","merged")
cleaning <- c("","cleaned","cleaned2","cleaned3")
window_rad <- c(200,400)
window_size <- c(400,800)


n1970 <- rast("./output_layers/netr_1970_cat.tif")
n1980 <- rast("./output_layers/netr_1980_cat.tif")
N2010 <- rast("./output_layers/NAIP_2010_cat.tif")
N2020 <- rast("./output_layers/NAIP_2020_cat.tif")

stack_cat <- c(n1970,n1980,N2010,N2020)
salt_cleaned_stack_cat <- salt_clean(stack_cat, writeR = FALSE)


cropping_shps <- list()

cropping_shps[[1]] <- st_read("./shapefiles/cropping/1970_netr_noc_all.shp")
cropping_shps[[2]] <- st_read("./shapefiles/cropping/1980_netr_all.shp")
cropping_shps[[3]] <- st_read("./shapefiles/cropping/2010_crop.shp")
cropping_shps[[4]] <- st_read("./shapefiles/cropping/2020_crop.shp")

par(mfrow = c(2,2))
plot(cropping_shps[[1]]$geometry)
plot(cropping_shps[[2]]$geometry)
plot(cropping_shps[[3]]$geometry)
plot(cropping_shps[[4]]$geometry)


for(i in 1:length(classifications)){
  if(classifications[i] == "cat"){
    stack <- stack_cat
    salt_cleaned_stack <- salt_cleaned_stack_cat
  }
  if(classifications[i] == "bin"){
    stack <- stack_cat
    names(stack) <- sub("cat","bin",names(stack))
    stack <- classify(stack, cbind(1, 5, 1), right=FALSE)
    
    salt_cleaned_stack <- salt_cleaned_stack_cat
    names(salt_cleaned_stack) <- sub("cat","bin",names(salt_cleaned_stack))
    salt_cleaned_stack <- classify(salt_cleaned_stack, cbind(1, 5, 1), right=FALSE)
  }
  if(classifications[i] == "merged"){
    stack <- stack_cat
    names(stack) <- sub("cat","merged",names(stack))
    stack <- classify(stack, cbind(1, 3, 2), right=FALSE)
    
    salt_cleaned_stack <- salt_cleaned_stack_cat
    names(salt_cleaned_stack) <- sub("cat","merged",names(salt_cleaned_stack))
    salt_cleaned_stack <- classify(salt_cleaned_stack, cbind(1, 3, 2), right=FALSE)
  }
  if(classifications[i] == "high"){
    stack <- stack_cat
    names(stack) <- sub("cat","high",names(stack))
    stack <- classify(stack, cbind(0, 3, 0), right=FALSE)
    stack <- classify(stack, cbind(4, 5, 1), right=FALSE)
    
    salt_cleaned_stack <- salt_cleaned_stack_cat
    names(salt_cleaned_stack) <- sub("cat","high",names(salt_cleaned_stack))
    salt_cleaned_stack <- classify(salt_cleaned_stack, cbind(0, 3, 0), right=FALSE)
    salt_cleaned_stack <- classify(salt_cleaned_stack, cbind(4, 5, 1), right=FALSE)
  }
  
  for(j in 1:length(cleaning)){ #1:length(cleaning)
    if(j == 1){
      stack_masked <- stack
    }
    if(j == 2){
      stack_masked <- nlcd_mask(salt_cleaned_stack, writeR = TRUE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
    }
    if(j == 3){
      stack_masked <- roads_mask(salt_cleaned_stack, writeR = TRUE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
    }
    if(j == 4){
      stack_masked <- nlcd_mask(salt_cleaned_stack, writeR = FALSE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
      stack_masked <- roads_mask(stack_masked, writeR = TRUE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
    }
    for(k in 1:length(window_rad)){ 
      if(j == 1){
      stack_max <- max_window(stack_masked, radius = window_rad[k], writeR = FALSE)
      for(l in 1:4){
        stack_max[[l]] <- mask(stack_max[[l]],cropping_shps[[l]])
      }
      writeRaster(stack_max, paste0("./output_layers/OHV_", classifications[i],cleaning[j],"_max_", window_size[k],"m.tif"),overwrite = TRUE)
      
      stack_mode <- mode_window(stack_masked, radius = window_rad[k], writeR = FALSE)
      for(l in 1:4){
        stack_mode[[l]] <- mask(stack_mode[[l]],cropping_shps[[l]])
      }
      writeRaster(stack_mode, paste0("./output_layers/OHV_", classifications[i],cleaning[j],"_mode_", window_size[k],"m.tif"),overwrite = TRUE)
      
      stack_sum <- sum_window(stack_masked, radius = window_rad[k], writeR = FALSE)
      for(l in 1:4){
        stack_sum[[l]] <- mask(stack_sum[[l]],cropping_shps[[l]])
      }
      writeRaster(stack_sum, paste0("./output_layers/OHV_", classifications[i],cleaning[j],"_sum_", window_size[k],"m.tif"),overwrite = TRUE)
      } else {
        stack_max <- max_window(stack_masked, radius = window_rad[k], writeR = FALSE)
        for(l in 1:4){
          stack_max[[l]] <- mask(stack_max[[l]],cropping_shps[[l]])
        }
        writeRaster(stack_max, paste0("./output_layers/OHV_", classifications[i],"_",cleaning[j],"_max_", window_size[k],"m.tif"),overwrite = TRUE)
        
        stack_mode <- mode_window(stack_masked, radius = window_rad[k], writeR = FALSE)
        for(l in 1:4){
          stack_mode[[l]] <- mask(stack_mode[[l]],cropping_shps[[l]])
        }
        writeRaster(stack_mode, paste0("./output_layers/OHV_", classifications[i],"_",cleaning[j],"_mode_", window_size[k],"m.tif"),overwrite = TRUE)
        
        stack_sum <- sum_window(stack_masked, radius = window_rad[k], writeR = FALSE)
        for(l in 1:4){
          stack_sum[[l]] <- mask(stack_sum[[l]],cropping_shps[[l]])
        }
        writeRaster(stack_sum, paste0("./output_layers/OHV_", classifications[i],"_",cleaning[j],"_sum_", window_size[k],"m.tif"),overwrite = TRUE)
      }
    }
  }
}


# Upload the cleaned/cleaned2/cleaned3 rasters to data > 05 covariate outputs > OHV
# Upload the moving window rasters to data > 06 covariates post focal > OHV_routes_roads

