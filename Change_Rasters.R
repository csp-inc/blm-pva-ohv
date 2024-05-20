rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","tidyr","lme4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


n1970 <- rast("./output_layers/n70_04052024.tif")
n1980 <- rast("./output_layers/n80_04052024.tif")
N2010 <- rast("./output_layers/N10_04052024.tif")
N2020 <- rast("./output_layers/N20_04052024.tif")

# Stacking
stack <- c(n1970,n1980,N2010,N2020)

par(mfrow = c(2,2))
plot(n1970, main = "1970",colNA="black")
plot(n1980, main = "1980",colNA="black")
plot(N2010, main = "2010",colNA="black")
plot(N2020, main = "2020",colNA="black")


# Creating a mask layer that represents all NAs across time
mask <- N2020
mask <- mask(mask,N2010)
mask <- mask(mask,n1980)
mask <- mask(mask,n1970)
mask <- classify(mask, cbind(0, 5, 1), right=FALSE)
plot(mask)

writeRaster(mask,"./other_data/masks/small_ext.tif")

# Loading in the CA half of MDT range shapefile
ca_dt_range <- st_read("./shapefiles/CA_DTrange/ca_dtrange_web.shp")
dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")


# Looking at pixel level change classes 

val_70 <- values(stack[[1]])
val_80 <- values(stack[[2]])
val_10 <- values(stack[[3]])
val_20 <- values(stack[[4]])

values_df <- as.data.frame(cbind(val_70,val_80,val_20,val_10))
names(values_df) <- c("V70","V80","V10","V20")

# Removing rows that are all NA
values_df <- values_df[rowSums(is.na(values_df)) != ncol(values_df), ]



change_class <- data.frame(matrix(nrow = nrow(values_df), ncol = 3))
names(change_class) <- c("70s-80s","80s-2010s","2010s-2020s")
change_class$`70s-80s` <- paste0(values_df[,1]," to ",values_df[,2])
change_class$`80s-2010s` <- paste0(values_df[,2]," to ",values_df[,3])
change_class$`2010s-2020s` <- paste0(values_df[,3]," to ",values_df[,4])

# par(mfrow = c(1,3))
# 
# barplot(table(change_class$`70s-80s`))
# barplot(table(change_class$`80s-2010s`))
# barplot(table(change_class$`2010s-2020s`))
# 



dat_1 <- as.data.frame(table(change_class$`70s-80s`))
dat_1$Change <- "1970-1980"
dat_2 <- as.data.frame(table(change_class$`80s-2010s`))
dat_2$Change <- "1980-2010"
dat_3 <- as.data.frame(table(change_class$`2010s-2020s`))
dat_3$Change <- "2010-2020"

change_data_all <- rbind(dat_1,dat_2,dat_3)
names(change_data_all) <- c("Class","Freq","Timestep")

change_data_all <- change_data_all[!grepl("NaN",change_data_all$Class),]


positions <- c("4 to 0","4 to 1","4 to 2","2 to 0","2 to 1","1 to 0","0 to 0","1 to 1","2 to 2","4 to 4","0 to 1","1 to 2","2 to 4","0 to 2",
               "1 to 4","0 to 4")

change_data_all %>%
  ggplot(aes(fill= Timestep, y = Freq, x = as.factor(Class)))+
  geom_col(position = "dodge")+
  scale_x_discrete(name = "Change Class", limits = positions)+
  ggsci::scale_fill_jco(name = "Change Timestep")+
  scale_fill_manual(values = c("#A2CD5A","#CD6090","#00B2EE"),labels=c("1970-1980", "1980-2010","2010-2020"),name="Change Timestep") +
  theme(axis.title.x = element_blank(), legend.position = "right")+
  theme_classic() + theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




# Split data by decade 

split <-split(change_data_all, change_data_all$Timestep)


for (i in 1:length(split)){
  split[[i]]$Prop <- split[[i]]$Freq/sum(split[[i]]$Freq)
}

change_data_all <- bind_rows(split)


change_data_all %>%
  ggplot(aes(fill= Timestep, y = Prop, x = as.factor(Class)))+
  geom_col(position = "dodge")+
  scale_x_discrete(name = "Change Class", limits = positions)+
  ggsci::scale_fill_jco(name = "Change Timestep")+
  scale_fill_manual(values = c("#A2CD5A","#CD6090","#00B2EE"),labels=c("1970-1980", "1980-2010","2010-2020"),name="Change Timestep") +
  theme(axis.title.x = element_blank(), legend.position = "right")+
  theme_classic() + theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




## Creating change raster with only increase and decrease classes for each time step change

stack_reclass <- classify(stack, cbind(3, 5, 2), right=FALSE)

val_70 <- values(stack_reclass[[1]])
val_80 <- values(stack_reclass[[2]])
val_10 <- values(stack_reclass[[3]])
val_20 <- values(stack_reclass[[4]])

vals_table <- as.data.frame(cbind(val_70,val_80,val_10,val_20))

change_class <- data.frame(matrix(nrow = nrow(vals_table), ncol = 3))
for(i in 2:4){
  change_class[,i-1] <- paste0(vals_table[,i-1],vals_table[,i])
}

names(change_class) <- c("70s-80s","80s-2010s","2010s-2020s")
unique(change_class$`70s-80s`)

Change <- c("NaNNaN","NaN0","NaN1","NaN2","01","11","00","02","12","10","21","22","20","0NaN","1NaN","2NaN")
change_cols <- c(NA,NA,NA,NA,2,0,0,2,2,1,1,0,1,NA,NA,NA)

change_codes <- as.data.frame(cbind(Change,change_cols))


output_names <- c("./output_layers/Change_Raster_Binary_1970_1980.tif","./output_layers/Change_Raster_Binary_1980_2010.tif","./output_layers/Change_Raster_Binary_2010_2020.tif")
for(i in 1:3){
  change_col <- as.data.frame(change_class[,i])
  names(change_col) <- "Change"
  change_col_df <- change_col %>% left_join(change_codes)
  change_rast <- stack_reclass[[1]]
  values(change_rast) <- change_col_df$change_cols
  plot(change_rast)
  hist(values(change_rast))
  values(change_rast) <- values(change_rast)-1
  hist(values(change_rast))
  writeRaster(change_rast,output_names[i],overwrite = TRUE)
}


# Making a change raster for 1980 to 2020 specifically 
change_col <- paste0(vals_table[,2],vals_table[,4])
change_col <- as.data.frame(change_col)
names(change_col) <- "Change"
change_col_df <- change_col %>% left_join(change_codes)
change_rast <- stack_reclass[[1]]
values(change_rast) <- change_col_df$change_cols
plot(change_rast)
hist(values(change_rast))
values(change_rast) <- values(change_rast)-1
hist(values(change_rast))
writeRaster(change_rast,"./output_layers/Change_Raster_Binary_1980_2020.tif",overwrite = TRUE)



## Moving window to make change hotspot map ----

## Load in binary change layers
c_70_80 <- rast("./output_layers/Change_Raster_Binary_1970_1980.tif")
c_80_2010 <- rast("./output_layers/Change_Raster_Binary_1980_2010.tif")
c_2010_2020 <- rast("./output_layers/Change_Raster_Binary_2010_2020.tif")
c_80_2020 <- rast("./output_layers/Change_Raster_Binary_1980_2020.tif")


# Apply moving window
raster <- c_80_2020
focal_num<-1000
focal_shape<-'circle'
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1


result<-focal(raster, foc_mat, fun = "modal", na.policy = "all", na.rm = TRUE)
clip_result <- mask(result, dt_range)

plot(clip_result, background = "black")


hist(values(clip_result))

# Save output
# Be sure to change name depending on radius and input change raster
writeRaster(clip_result,"./output_layers/Window1000m_mode_change_1980_2020.tif",overwrite = TRUE)


# Creating magnitude increase map for manuscript 

# First run mean moving window over original layers with a radius of 1000m 

n80 <- rast("./output_layers/netr_1980_masked_9_nlcdmask_roadsmask.tif")
N20 <- rast("./output_layers/NAIP_2020_masked_9_nlcdmask_roadsmask.tif")

n80 <- classify(n80, cbind(2, 3, 151), right=FALSE)
n80 <- classify(n80, cbind(4, 5, 451), right=FALSE)

N20 <- classify(N20, cbind(2, 3, 151), right=FALSE)
N20 <- classify(N20, cbind(4, 5, 451), right=FALSE)

# Apply moving window
raster <- n80
focal_num<-1000
focal_shape<-'circle'
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1


result<-focal(raster, foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
n80 <- mask(result,dt_range)


# Apply moving window
raster <- N20
focal_num<-1000
focal_shape<-'circle'
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1


result<-focal(raster, foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
N20 <- mask(result, dt_range)


change_mean <- N20-n80
change_mean <- classify(change_mean, cbind(-500, 0, 0), right=FALSE)
writeRaster(change_mean,"./output_layers/Change_Magnitude_1980_2020_cleaned.tif",overwrite=TRUE)

mask <- rast("./other_data/masks/all_masks/Merged_80s_2020s_masked_9_nlcdmask_roadsmask_MASK.tif")
change_mean <- rast("./output_layers/Change_Magnitude_1980_2020_cleaned.tif")

change_mean_masked <- mask(change_mean,mask, inverse = TRUE)
writeRaster(change_mean_masked,"./output_layers/Change_Magnitude_1980_2020_cleaned.tif", overwrite = TRUE)

plot(change_mean_masked)
