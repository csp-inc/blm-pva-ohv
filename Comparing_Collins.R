rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","tidyr","lme4","googleCloudStorageR","googleAuthR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Use the JSON file to authenticate communication between RStudio and GCS
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)

bucket_name <- "gs://pva_image_processing"

# Loading in the new 2010 and 2020 layers
N2010 <- rast("./output_layers/N10_04052024.tif")
N2020 <- rast("./output_layers/N20_04052024.tif")

# Loading in the old 2010 and 2020 layers to do some comparison

contents <- gcs_list_objects(bucket = bucket_name,
                             prefix = "NAIP/NAIP_collins_rasters/")

if(dir.exists("./Old_OHV") == FALSE){dir.create("./Old_OHV")}
folder_to_download <- contents$name
purrr::map(folder_to_download, function(x)
  gcs_get_object(x, bucket = bucket_name, overwrite = TRUE,
                 saveToDisk = paste0("./Old_OHV","/",basename(x))))


N10_old <- rast("./Old_OHV/NAIP_2010_collins.tif")
N20_old <- rast("./Old_OHV/NAIP_2019_collins.tif")
N20_old <- N20_old[[1]]


# Plotting to see extents
par(mfrow = c(2,2))
plot(N10_old, main = "2010 OLD",colNA="black")
plot(N10, main = "2010 NEW",colNA="black")
plot(N20_old, main = "2020 OLD",colNA="black")
plot(N20, main = "2020 NEW",colNA="black")

# Creating a change raster between the old and the new 2010 layers
cont_list <- list()
cont_list[[1]] <- N10_old
cont_list[[2]] <- N2010
cont_list[[3]] <- N20_old
cont_list[[4]] <- N2020


# Taking the mean of
for (i in 1:4){
focal_num<-250
focal_shape<-'circle'
raster <- cont_list[[i]]
raster <- crop(mask(raster,ca_dt_range),ca_dt_range)
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1
cont_list[[i]] <-focal(raster, foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
}



cont_list[[1]] <- project(cont_list[[1]],cont_list[[2]])
cont_list[[3]] <- project(cont_list[[3]],cont_list[[4]])


diff_2010 <- cont_list[[2]] - cont_list[[1]] # new - old

diff_2020 <- cont_list[[4]] - cont_list[[3]] # new - old

# positive numbers mean new run had higher OHV
# negative numbers mean new run had lower OHV

writeRaster(diff_2010,"./output_layers/2010_differences_mean250.tif", overwrite = TRUE)
writeRaster(diff_2020,"./output_layers/2020_differences_mean250.tif",overwrite = TRUE)

focal_num<-500
focal_shape<-'circle'
foc_mat<-focalMat(diff_2010, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1
diff_2010_sum_500 <-focal(diff_2010, foc_mat, fun = "sum", na.policy = "omit", na.rm = TRUE)


focal_num<-500
focal_shape<-'circle'
foc_mat<-focalMat(diff_2020, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1
diff_2020_sum_500 <-focal(diff_2020, foc_mat, fun = "sum", na.policy = "omit", na.rm = TRUE)



# Comparing old and new layers and their total area of each OHV class

decades <- c("2010 OLD","2010 NEW","2020 OLD","2020 NEW")
prop_list <- list()


prop_list[[1]] <- N10_old
prop_list[[2]] <- N2010
prop_list[[3]] <- N20_old
prop_list[[4]] <- N2020


for(i in 1:length(prop_list)){
  prop_list[[i]] <- crop(mask(prop_list[[i]],ca_dt_range),ca_dt_range)
  prop <- as.data.frame(table(values(prop_list[[i]])))
  prop$sum <- sum(prop$Freq)
  if (i %in% c(2,4)){
  prop$area <- prop$Freq*(150*150)
  } else {
    prop$area <- prop$Freq*(150*122)
  }
  prop$dec_area <- sum(prop$area)
  prop$Decade <- decades[i]
  prop_list[[i]] <- prop
}

prop_years <- bind_rows(prop_list)

prop_years$Proportion <- prop_years$Freq/prop_years$sum

st_area(ca_dt_range) 

prop_years$CA_area_prop <- prop_years$area/as.numeric(st_area(dt_range))
prop_years$dec_area_prop <- prop_years$area/prop_years$dec_area
names(prop_years) <- c("Class","Freq","sum","area","dec_area","Decade","Proportion","CA_area_prop","dec_area_prop")

prop_years$Class <- as.character(prop_years$Class)


ggplot(prop_years, aes(fill=Class, y=dec_area_prop, x=Decade, label = paste0(round(100*dec_area_prop,1),"%"))) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Decadal Area Coverage") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV density\ncategory",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                          axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))
