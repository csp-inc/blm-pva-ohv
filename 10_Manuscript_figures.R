## ---------------------------
##
## Script name: 10_Manuscript_figures.R
##
## This script creates figures used for the OHV manuscript
##
## Author: Madeline Standen
##
## Date Created: 02/__/2024
## Date last updated: 03/17/2025
##
## Email contact: madi[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##


# This script creates plots for the OHV manuscript
rm(list=ls())
## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","ggplot2","ggeffects","googleCloudStorageR","googleAuthR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Use the JSON file to authenticate communication between RStudio and GCS -----
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

## Create necessary local folders -----
if(dir.exists("./figures") == FALSE){dir.create("./figures")}
source("./Functions.R")

# Loads in the mdt range
dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")
st_area(dt_range) # 2.49582e+11 [m^2]

# Download and load in the OHV open use areas, US states, and world boundaries

if(dir.exists("./shapefiles/OHV_openuse") == FALSE){dir.create("./shapefiles/OHV_openuse")}

contents <- gcs_list_objects(bucket = "gs://csp_tortoisehub",
                             prefix = "data/01_administrative_boundaries/open-use-areas/")
folder_to_download <- contents$name
folder_to_download <- folder_to_download[grepl("web",folder_to_download)]
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/OHV_openuse","/",basename(x))))

if(dir.exists("./shapefiles/TCA") == FALSE){dir.create("./shapefiles/TCA")}

contents <- gcs_list_objects(bucket = "gs://csp_tortoisehub",
                             prefix = "data/01_administrative_boundaries/MDT_TCA/")
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/TCA","/",basename(x))))


if(dir.exists("./shapefiles/us_states") == FALSE){dir.create("./shapefiles/us_states")}

contents <- gcs_list_objects(bucket = "gs://csp_tortoisehub",
                             prefix = "data/01_administrative_boundaries/us_states/")
folder_to_download <- contents$name
folder_to_download <- folder_to_download[grepl("web",folder_to_download)]
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/us_states","/",basename(x))))

if(dir.exists("./shapefiles/world-administrative-boundaries") == FALSE){dir.create("./shapefiles/world-administrative-boundaries")}

contents <- gcs_list_objects(bucket = "gs://csp_tortoisehub",
                             prefix = "data/01_administrative_boundaries/world-administrative-boundaries/")
folder_to_download <- contents$name
folder_to_download <- folder_to_download[grepl("web",folder_to_download)]
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/world-administrative-boundaries","/",basename(x))))


if(dir.exists("./shapefiles/2011RecoveryUnits") == FALSE){dir.create("./shapefiles/2011RecoveryUnits")}

contents <- gcs_list_objects(bucket = "gs://csp_tortoisehub",
                             prefix = "data/02_tortoise/2011RecoveryUnits/")
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
                 saveToDisk = paste0("./shapefiles/2011RecoveryUnits","/",basename(x))))

### Figure 4 -----

# Loads in original layers created in scripts "Mosaick.R" and "Processing.R"
n1970 <- rast("./output_layers/netr_1970_cat.tif")
n1980 <- rast("./output_layers/netr_1980_cat.tif")
N2010 <- rast("./output_layers/NAIP_2010_cat.tif")
N2020 <- rast("./output_layers/NAIP_2020_cat.tif")

# Stacks
stack <- c(n1970,n1980,N2010,N2020)
# Applies masking and cleaning procedure
stack <- salt_clean(stack, writeR = FALSE)
stack <- nlcd_mask(stack, writeR = FALSE, update0 = FALSE, updateNA = TRUE, remask = TRUE)
stack_masked_nlcd_roads <- roads_mask(stack, writeR = FALSE, update0 = FALSE, updateNA = TRUE, remask = TRUE)

# Figure was created in Q GIS

### Figure 6 -----

## Using consistent extent to calculate OHV route length stats for each year
values_df <- read.csv("./other_data/master/master_cells_cleaned3.csv")

values_const <- values_df[complete.cases(values_df), ]

values_const <- values_const %>%
  mutate(V1970_mean = case_when(V1970 == 1 ~ 75,
                           V1970 == 2 ~ 300,
                           V1970 == 4 ~ 11475,
                           TRUE ~ V1970))
values_const <- values_const %>%
  mutate(V1980_mean = case_when(V1980 == 1 ~ 75,
                           V1980 == 2 ~ 300,
                           V1980 == 4 ~ 11475,
                           TRUE ~ V1980))
values_const <- values_const %>%
  mutate(V2010_mean = case_when(V2010 == 1 ~ 75,
                           V2010 == 2 ~ 300,
                           V2010 == 4 ~ 11475,
                           TRUE ~ V2010))
values_const <- values_const %>%
  mutate(V2020_mean = case_when(V2020 == 1 ~ 75,
                           V2020 == 2 ~ 300,
                           V2020 == 4 ~ 11475,
                           TRUE ~ V2020))

values_const <- values_const %>%
  mutate(V1970_min = case_when(V1970 == 1 ~ 1,
                                V1970 == 2 ~ 151,
                                V1970 == 4 ~ 451,
                                TRUE ~ V1970))
values_const <- values_const %>%
  mutate(V1980_min = case_when(V1980 == 1 ~ 1,
                                V1980 == 2 ~ 151,
                                V1980 == 4 ~ 451,
                                TRUE ~ V1980))
values_const <- values_const %>%
  mutate(V2010_min = case_when(V2010 == 1 ~ 1,
                                V2010 == 2 ~ 151,
                                V2010 == 4 ~ 451,
                                TRUE ~ V2010))
values_const <- values_const %>%
  mutate(V2020_min = case_when(V2020 == 1 ~ 1,
                                V2020 == 2 ~ 151,
                                V2020 == 4 ~ 451,
                                TRUE ~ V2020))

values_const <- values_const %>%
  mutate(V1970_max = case_when(V1970 == 1 ~ 150,
                               V1970 == 2 ~ 450,
                               V1970 == 4 ~ 22500,
                               TRUE ~ V1970))
values_const <- values_const %>%
  mutate(V1980_max = case_when(V1980 == 1 ~ 150,
                               V1980 == 2 ~ 450,
                               V1980 == 4 ~ 22500,
                               TRUE ~ V1980))
values_const <- values_const %>%
  mutate(V2010_max = case_when(V2010 == 1 ~ 150,
                               V2010 == 2 ~ 450,
                               V2010 == 4 ~ 22500,
                               TRUE ~ V2010))
values_const <- values_const %>%
  mutate(V2020_max = case_when(V2020 == 1 ~ 150,
                               V2020 == 2 ~ 450,
                               V2020 == 4 ~ 22500,
                               TRUE ~ V2020))


mean_length <- colSums(values_const[,9:12])
min_length <- colSums(values_const[,13:16])
max_length <- colSums(values_const[,17:20])

decades <- c("1970","1980","2010","2020")

tot_length <- as.data.frame(cbind(min_length,mean_length,max_length))

tot_length_long <- tot_length %>% 
  pivot_longer(
    cols = c("min_length","mean_length","max_length"), 
    names_to = "scale",
    values_to = "length"
  )

tot_length <- cbind(tot_length,decades)
tot_length_long$decades <- c("1970","1970","1970","1980","1980","1980","2010","2010","2010","2020","2020","2020")


tot_length_long2 <- tot_length_long %>% filter(scale != "max_length")


ggplot(tot_length_long2 , aes(x=decades, y=length/1000, color = scale, fill = scale)) +
  geom_col(position = "dodge") + ylab("Total OHV route length (km)") + xlab("Decade") +
  theme_classic() + theme(axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color = "black",size=12),
                          legend.title = element_text(face = "bold",size=12),axis.title.y = element_text(color="black",size=12),
                          axis.title.x = element_text(color="black",size=12),legend.text = element_text(color="black",size=12))+
  scale_fill_manual(values = c("#528970","#18416e"),labels=c("Median","Minimum"),name="") +
  scale_color_manual(values = c("#528970","#18416e"),labels=c("Median","Minimum"),name="") +
  scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000), name = "Total OHV route length (million km)",labels = c("0","0.5","1","1.5","2","2.5"))+ 
  scale_x_discrete(name = "Decade", label = c("1970s","1980s","2010s","2020s"))


ggsave(filename = "./figures/figure_6.jpeg",height = 4.5, width = 6)

### Figure 7 -----
# Loads in cleaned and masked layers for 1980s and 2020s
n80 <- stack_masked_nlcd_roads[[2]]
N20 <- stack_masked_nlcd_roads[[4]]

# Assigning medium and high classifications to the minimum length of ohv route
n80 <- classify(n80, cbind(2, 3, 151), right=FALSE)
n80 <- classify(n80, cbind(4, 5, 451), right=FALSE)

N20 <- classify(N20, cbind(2, 3, 151), right=FALSE)
N20 <- classify(N20, cbind(4, 5, 451), right=FALSE)

# Apply moving window to 80s
raster <- n80
focal_num<-1000
focal_shape<-'circle'
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1


result<-focal(raster, foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
n80 <- mask(result,dt_range)


# Apply moving window to 2020s
raster <- N20
focal_num<-1000
focal_shape<-'circle'
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1


result<-focal(raster, foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
N20 <- mask(result, dt_range)

# Find the difference
change_mean <- N20-n80
# Reclassify negative values to 0
change_mean <- classify(change_mean, cbind(-500, 0, 0), right=FALSE)
writeRaster(change_mean,"./output_layers/Change_Magnitude_1980_2020_cleaned3.tif",overwrite=TRUE)

# Loads in merged mask for 1980s and 2020s created in "All_Masks.R"
mask <- rast("./other_data/masks/all_masks/Merged_80s_2020s_cleaned3_MASK.tif")
change_mean <- rast("./output_layers/Change_Magnitude_1980_2020_cleaned3.tif")

# Mask the change raster with the combined mask
change_mean_masked <- mask(change_mean,mask, inverse = TRUE)
writeRaster(change_mean_masked,"./output_layers/Change_Magnitude_1980_2020_cleaned3.tif", overwrite = TRUE)


# Figure was created in Q GIS

### Stats for figure 7 -----
if(dir.exists("./shapefiles/2011RecoveryUnits") == FALSE){dir.create("./shapefiles/2011RecoveryUnits")}

RU <- st_read("./shapefiles/2011RecoveryUnits/2011RecoveryUnits.shp") %>% st_transform("EPSG:3857")
st_area(RU)/1000/1000

clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}


layers <- stack_masked_nlcd_roads[[-1]]
layers

RU_stats <- list()

for(i in 1:nrow(RU)){
  shape <- vect(RU[i,])
  # area <- st_area(RU[i,])
  layers_cropped <- crop(mask(layers,shape),shape)
  info_1980 <- as.data.frame(table(values(layers_cropped[[1]])))
  info_2010 <- as.data.frame(table(values(layers_cropped[[2]])))
  info_2020 <- as.data.frame(table(values(layers_cropped[[3]])))
  info <- cbind(info_1980,info_2010$Freq,info_2020$Freq)
  names(info) <- c("OHV_dens","freq1980","freq2010","freq2020")
  info <- ((info[,2:4]*150*150)/1000)/1000
  info$OHV <- c("none","low","medium","high")
  info$RU <- unlist(as.vector(rep(unname(st_drop_geometry(RU[i,4])),times = 4)))
  RU_stats[[i]] <- info
}
names(RU_stats) <- RU$Unit_Name
RU_stats <- bind_rows(RU_stats)



TCA <- st_read("./shapefiles/TCA/MDT_TCA.shp")%>%st_transform("EPSG:3857")
TCA 
st_area(TCA)/1000/1000

TCA_stats <- list()

for(i in 1:nrow(TCA)){
  shape <- vect(TCA[i,])
  # area <- st_area(TCA[i,])
  layers_cropped <- crop(mask(layers,shape),shape)
  info_1980 <- as.data.frame(table(values(layers_cropped[[1]])))
  info_2010 <- as.data.frame(table(values(layers_cropped[[2]])))
  info_2020 <- as.data.frame(table(values(layers_cropped[[3]])))
  info <- cbind(info_1980,info_2010$Freq,info_2020$Freq)
  names(info) <- c("OHV_dens","freq1980","freq2010","freq2020")
  info <- ((info[,2:4]*150*150)/1000)/1000
  info$OHV <- c("none","low","medium","high")
  info$TCA<- unlist(as.vector(rep(unname(st_drop_geometry(TCA[i,2])),times = 4)))
  TCA_stats[[i]] <- info
}
names(TCA_stats) <- TCA$NAME
TCA_stats <- bind_rows(TCA_stats)


val_1980 <- values(layers[[1]])
val_2010 <- values(layers[[2]])
val_2020 <- values(layers[[3]])

vals_table <- as.data.frame(cbind(val_1980,val_2010,val_2020))
unique(vals_table)

Change <- c("NaNNaN","NaN0","NaN1","NaN2","NaN4","00","01","02","04","10","11","12","14","20","21","22","24","40","41","42","44","0NaN","1NaN","2NaN","4NaN")
change_cols <- c(NA,NA,NA,NA,NA,0,2,2,2,1,0,2,2,1,1,0,2,1,1,1,0,NA,NA,NA,NA)

change_codes <- as.data.frame(cbind(Change,change_cols))


# Making a change raster for 2010 to 2020 specifically
change_col <- paste0(vals_table[,2],vals_table[,3])
change_col <- as.data.frame(change_col)
names(change_col) <- "Change"
change_col_df <- change_col %>% left_join(change_codes)
change_rast <- layers[[1]]
values(change_rast) <- change_col_df$change_cols
plot(change_rast)
hist(values(change_rast))
values(change_rast) <- values(change_rast)-1
hist(values(change_rast))
writeRaster(change_rast,"./output_layers/Change_Raster_Binary_2010_2020_cleaned3.tif",overwrite = TRUE)



# Making a change raster for 1980 to 2020 specifically 
change_col <- paste0(vals_table[,1],vals_table[,3])
change_col <- as.data.frame(change_col)
names(change_col) <- "Change"
change_col_df <- change_col %>% left_join(change_codes)
change_rast <- layers[[1]]
values(change_rast) <- change_col_df$change_cols
plot(change_rast)
hist(values(change_rast))
values(change_rast) <- values(change_rast)-1
hist(values(change_rast))
writeRaster(change_rast,"./output_layers/Change_Raster_Binary_1980_2020_cleaned3.tif",overwrite = TRUE)

change_rast <- rast("./output_layers/Change_Raster_Binary_1980_2020_cleaned3.tif")

RU_change_stats <- list()
TCA_change_stats <- list()

for(i in 1:nrow(RU)){
  shape <- vect(RU[i,])
  # area <- st_area(RU[i,])
  change_cropped <- crop(mask(change_rast,shape),shape)
  info_change <- as.data.frame(table(values(change_cropped)))
  names(info_change) <- c("OHV_dens","Freq")
  info_change$area <- ((info_change[,2]*150*150)/1000)/1000
  info_change$RU <- unlist(as.vector(rep(unname(st_drop_geometry(RU[i,4])),times = 3)))
  RU_change_stats[[i]] <- info_change
}
RU_change_stats <- bind_rows(RU_change_stats)
RU_change_stats <- filter(RU_change_stats,OHV_dens == 2)

RU_change_stats$RU_area <- st_area(RU)/1000/1000
RU_change_stats$RU_area <- clean_units(RU_change_stats$RU_area)

RU_change_stats$prop <- 100*(RU_change_stats$area/RU_change_stats$RU_area)


for(i in 1:nrow(TCA)){
  shape <- vect(TCA[i,])
  # area <- st_area(TCA[i,])
  change_cropped <- crop(mask(change_rast,shape),shape)
  info_change <- as.data.frame(table(values(change_cropped)))
  names(info_change) <- c("OHV_dens","Freq")
  info_change$area <- ((info_change[,2]*150*150)/1000)/1000
  info_change$TCA<- unlist(as.vector(rep(unname(st_drop_geometry(TCA[i,2])),times = 3)))
  TCA_change_stats[[i]] <- info_change
}
TCA_change_stats <- bind_rows(TCA_change_stats)
TCA_change_stats <- filter(TCA_change_stats,OHV_dens == 2)

TCA_change_stats$TCA_area <- st_area(TCA)/1000/1000
TCA_change_stats$TCA_area <- clean_units(TCA_change_stats$TCA_area)

TCA_change_stats$prop <- 100*(TCA_change_stats$area/TCA_change_stats$TCA_area)

### Smoothed layers for supplement -----


n1970 <- rast("./output_layers/netr_1970_cat.tif")
n1980 <- rast("./output_layers/netr_1980_cat.tif")
N2010 <- rast("./output_layers/NAIP_2010_cat.tif")
N2020 <- rast("./output_layers/NAIP_2020_cat.tif")

mask_list <- list()
mask_list[[1]] <- rast("./other_data/masks/all_masks/netr_1970_cleaned3_MASK.tif")
mask_list[[2]] <- rast("./other_data/masks/all_masks/netr_1980_cleaned3_MASK.tif")
mask_list[[3]] <- rast("./other_data/masks/all_masks/NAIP_2010_cleaned3_MASK.tif")
mask_list[[4]] <- rast("./other_data/masks/all_masks/NAIP_2020_cleaned3_MASK.tif")
plot(mask_list[[1]])

# Stacks
stack <- c(n1970,n1980,N2010,N2020)

stack_salt <- salt_clean(stack, writeR = FALSE)

# Applies masking and cleaning procedure
stack <- salt_clean(stack, writeR = FALSE)
stack <- nlcd_mask(stack, writeR = FALSE, update0 = TRUE, updateNA = FALSE, remask = TRUE)
stack <- roads_mask(stack, writeR = FALSE, update0 = TRUE, updateNA = FALSE, remask = TRUE)

# Running the mean moving window function
stack_mean <- mean_window(stack, radius = 1000, writeR = FALSE)

# Masking out the layers again with NA this time
for(i in 1:nlyr(stack)){
  stack_mean[[i]] <- mask(stack_mean[[i]],mask_list[[i]],inverse = TRUE)
}

# Checking
plot(stack_mean[[4]])

# Creating new names for output layers
names(stack) <- c("NETR_1970_OHV_mean1kmr","NETR_1980_OHV_mean1kmr","NAIP_2010_OHV_mean1kmr","NAIP_2020_OHV_mean1kmr")

# Re-naming the layers and writing outputs
for(i in 1:4){
  names(stack_mean[[i]]) <- c(names(stack)[i])
  writeRaster(stack_mean[[i]],paste0("./output_layers/",names(stack_mean)[i],".tif"))
}

