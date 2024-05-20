# This script creates plots for the OHV manuscript

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source("./Functions.R")

# Loads in the mdt range
dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")


### Figure 3 -----
# Loads in csv created in script "Master_creation.R"
values_sample <- read.csv("./other_data/master/master_cells_cleaned.csv")

final_random <- random_sampling(values_sample,small_ext = TRUE)

year_order <- c("V1970","V1980","V2010","V2020")
final_random %>%
  ggplot(aes(fill= OHV_val, y = mean, x = as.factor(year),label = paste0(round(100*mean,1),"%")))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                position = position_dodge(0.9), width = .3)+
  scale_x_discrete(name = "Decade", label = c("1970s","1980s","2010s","2020s"),limits = year_order)+
  ggsci::scale_fill_jco(name = "OHV route\ndensity category")+ 
  geom_text(size = 3.5, color = "black", position = position_dodge2(width = 4),vjust=-2.5, hjust=.4) +
  scale_fill_manual(values = c("#a69d8b","#fae51e","darkorange","red"),labels=c("None", "Low","Medium","High"),name="OHV route\ndensity category") +
  theme(legend.position = "right")+
  scale_y_continuous(breaks = seq(0,1,.2), name = "Percent of random sample",labels = c("0","20","40","60","80","100"))  + theme_classic() + theme(axis.text.x = element_text(color="black",size=12),
                                                                                                                                                   axis.text.y = element_text(color = "black",size=12),legend.title = element_text(face = "bold",size=12),
                                                                                                                                                   axis.title.y = element_text(color="black",size=12),axis.title.x = element_text(color="black",size=12),
                                                                                                                                                   legend.text = element_text(color="black",size=12))
ggsave(filename = "./figure_3.jpeg",height = 7.5, width = 10)


### Figure 4 -----

## Using consistent extent to calculate OHV route length stats for each year

values_const <- values_df[complete.cases(values_df), ]

values_const <- values_const %>%
  mutate(V1970 = case_when(V1970 == 2 ~ 151, 
                           V1970 == 4 ~ 451,
                           TRUE ~ V1970))
values_const <- values_const %>%
  mutate(V1980 = case_when(V1980 == 2 ~ 151, 
                           V1980 == 4 ~ 451,
                           TRUE ~ V1980))
values_const <- values_const %>%
  mutate(V2010 = case_when(V2010 == 2 ~ 151, 
                           V2010 == 4 ~ 451,
                           TRUE ~ V2010))
values_const <- values_const %>%
  mutate(V2020 = case_when(V2020 == 2 ~ 151, 
                           V2020 == 4 ~ 451,
                           TRUE ~ V2020))


tot_length <- colSums(values_const[,2:5])

decades <- c("1970s","1980s","2010s","2020s")

tot_length <- as.data.frame(tot_length)

tot_length <- cbind(tot_length,decades)

ggplot(tot_length , aes(x=decades, y=tot_length/1000)) + 
  geom_bar(stat = "identity") +ylab("Minimum total OHV route length (km)") + xlab("Decade") +theme_classic() + theme(axis.text.x = element_text(color="black",size=12),
                                                                                                                     axis.text.y = element_text(color = "black",size=12),legend.title = element_text(face = "bold",size=12),
                                                                                                                     axis.title.y = element_text(color="black",size=12),axis.title.x = element_text(color="black",size=12),
                                                                                                                     legend.text = element_text(color="black",size=12))


ggsave(filename = "./figure_4.jpeg",height = 7.5, width = 10)


### Figure 5 -----

# Loads in original layers created in scripts "Mosaick.R" and "Processing.R"
n1970 <- rast("./output_layers/n70_04052024.tif")
n1980 <- rast("./output_layers/n80_04052024.tif")
N2010 <- rast("./output_layers/N10_04052024.tif")
N2020 <- rast("./output_layers/N20_04052024.tif")

# Stacks
stack <- c(n1970,n1980,N2010,N2020)
# Applies masking and cleaning procedure
salt_cleaned_stack <- salt_clean(stack, writeR = FALSE)
stack_masked_nlcd <- nlcd_mask(salt_cleaned_stack, writeR = FALSE)
stack_masked_nlcd_roads <- roads_mask(stack_masked_nlcd, writeR = TRUE) # Saves layers

### Figure 6 -----
# Loads in cleaned and masked layers for 1980s and 2020s
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

# Loads in merged mask for 1980s and 2020s created in "All_Masks.R"
mask <- rast("./other_data/masks/all_masks/Merged_80s_2020s_masked_9_nlcdmask_roadsmask_MASK.tif")
change_mean <- rast("./output_layers/Change_Magnitude_1980_2020_cleaned.tif")

change_mean_masked <- mask(change_mean,mask, inverse = TRUE)
writeRaster(change_mean_masked,"./output_layers/Change_Magnitude_1980_2020_cleaned.tif", overwrite = TRUE)

plot(change_mean_masked)
