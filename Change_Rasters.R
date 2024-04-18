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




## Using consistent extent to calculate OHV route length stats for each year


decades <- c("1970s","1980s","2010s","2020s")
prop_list <- list()
for(i in 1:nlyr(stack_masked)){
  prop <- as.data.frame(table(values(stack_masked[[i]])))
  prop$sum <- sum(prop$Freq)
  prop$area <- prop$Freq*(150*150)
  prop$Decade <- decades[i]
  prop_list[[i]] <- prop
}

prop_years <- bind_rows(prop_list)

prop_years$Proportion <- prop_years$Freq/prop_years$sum



split <-split(prop_years, prop_years$Var1)

for(i in 1:4){
  if(i == 1){
    split[[i]]$Length <- 0
  } else {
    split[[i]]$Length <- split[[i]]$Freq*as.numeric(split[[i]][1,1])*150
  }
}

prop_years <- bind_rows(split)


split <-split(prop_years, prop_years$Decade)

for(i in 1:4){
  split[[i]]$tot_length <- sum(split[[i]]$Length)
  split[[i]]$prop_length <- split[[i]]$Length / sum(split[[i]]$Length)
}


prop_years <- bind_rows(split)

prop_years <- prop_years %>% filter(Var1 == "1"|Var1 == "2"|Var1 == "4")


ggplot(prop_years, aes(fill= factor(Var1, c("1","2","4")), y=Length, x=Decade)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylab("Length contributed by each OHV class") +
  scale_fill_manual(values = c("#28bceb","#a4fc3c","#fb7e21"),labels=c("1 Track","2-3 Tracks","4 + Tracks"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




ggplot(prop_years, aes(fill= factor(Var1, c("1","2","4")), y=prop_length, x=Decade)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylab("Proportion of total length contributed by each OHV class") +
  scale_fill_manual(values = c("#28bceb","#a4fc3c","#fb7e21"),labels=c("1 Track","2-3 Tracks","4 + Tracks"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




split <-split(prop_years, prop_years$Decade)
tot_length <- c()
for(i in 1:4){
  tot_length[i] <- sum(split[[i]]$Length)
}

tot_length <- as.data.frame(tot_length)

tot_length <- cbind(tot_length,decades)

ggplot(tot_length , aes(x=decades, y=tot_length)) + 
  geom_bar(stat = "identity") +ylab("Total OHV route length in consistent area")




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


# Creating magnitude increase map 

# First run mean moving window over original layers with a radius of 1000m 

n80 <- stack[[2]]
N20 <- stack[[4]]

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
change_mean <- classify(change_mean, cbind(-5, 0, 0), right=FALSE)
writeRaster(change_mean,"./output_layers/Change_Magnitude_1980_2020.tif")


### Random sampling of values from layers

values_df <- read.csv("./other_data/master/master_cells.csv")

# values_df <- values_df[complete.cases(values_df), ]
# 
# # Sampling 1000 cells 1000 times from each decade
# 
# values_df_70 <- values_df[,c(2,6)]
# values_df_70 <- values_df_70[complete.cases(values_df_70$V70), ]
# nrow(values_df_70)
# nrow(values_df_70)*(150)/1000
# 
# values_df_80 <- values_df[,c(3,6)]
# values_df_80 <- values_df_80[complete.cases(values_df_80$V80), ]
# nrow(values_df_80)
# nrow(values_df_80)*(150)/1000
# 
# values_df_10 <- values_df[,c(4,6)]
# values_df_10 <- values_df_10[complete.cases(values_df_10$V10), ]
# nrow(values_df_10)
# nrow(values_df_10)*(150)/1000
# 
# values_df_20 <- values_df[,c(5,6)]
# values_df_20 <- values_df_20[complete.cases(values_df_20$V20), ]
# nrow(values_df_20)
# nrow(values_df_20)*(150)/1000
# 
# list <- list()
# list[[1]] <- values_df_70
# list[[2]] <- values_df_80
# list[[3]] <- values_df_10
# list[[4]] <- values_df_20
# 
# output_mean <- list()
# output_sd <- list()
# 
# set.seed(41)
# for (i in 1:4){
#   df <- list[[i]]
#   prop_df <- data.frame(matrix(nrow = 4, ncol = 1000))
#   for (j in 1:1000){
#     # Generate 1000 random row indices
#     random_indices <- sample(1:nrow(df), 1000, replace = FALSE)
#     # Subset the dataset with the randomly selected indices
#     subset_df <- df[random_indices, ]
#     values <- as.data.frame(table(subset_df[,1]))
#     values <- values[,2]/sum(values[,2])
#     prop_df[,j] <- values
#   }
#   output_mean[[i]] <- rowMeans(prop_df)
#   prop_mat <- as.matrix(prop_df)
#   output_sd[[i]] <- apply(prop_mat, 1, sd)
# }
# 
# output_mean[[1]]
# output_mean[[2]]
# output_mean[[3]]
# output_mean[[4]]
# 
# final_prop <- as.data.frame(cbind(output_mean[[1]],
#       output_mean[[2]],
#       output_mean[[3]],
#       output_mean[[4]]))
# 
# final_sd <- as.data.frame(cbind(output_sd[[1]],
#                                   output_sd[[2]],
#                                   output_sd[[3]],
#                                   output_sd[[4]]))
# 
# 
# final_prop$OHV_val <- c("0","1","2","4")
# 
# final_prop <- final_prop[,c(5,1:4)]
# 
# names(final_prop) <- c("OHV_val","V1970","V1980","V2010","V2020")
# # write.csv(final_prop,"./sample1000.csv", row.names = FALSE)
# 
# 
# final_sd$OHV_val <- c("0","1","2","4")
# 
# final_sd <- final_sd[,c(5,1:4)]
# 
# names(final_sd) <- c("OHV_val","sd1970","sd1980","sd2010","sd2020")
# 
# mean_long <- final_prop %>% 
#   pivot_longer(
#     cols = c("V1970","V1980","V2010","V2020"), 
#     names_to = "year",
#     values_to = "mean"
#   )
# 
# sd_long <- final_sd %>% 
#   pivot_longer(
#     cols = c("sd1970","sd1980","sd2010","sd2020"), 
#     names_to = "year",
#     values_to = "sd"
#   )
# 
# final_random <- cbind(mean_long,sd_long[,3])
# 
# year_order <- c("V1970","V1980","V2010","V2020")

year_order <- c("V1970","V1980","V2010","V2020")
final_random %>%
  ggplot(aes(fill= OHV_val, y = mean, x = as.factor(year),label = paste0(round(100*mean,1),"%")))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                position = position_dodge(0.9), width = .3)+
  scale_x_discrete(name = "Decade", label = c("1970s","1980s","2010s","2020s"),limits = year_order)+
  ggsci::scale_fill_jco(name = "OHV route\ndensity category")+ 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-2.5, hjust=.4) +
  scale_fill_manual(values = c("#a69d8b","#fae51e","darkorange","red"),labels=c("None", "Low","Medium","High"),name="OHV route\nabundance category") +
  theme(axis.title.x = element_blank(), legend.position = "right")+
  scale_y_continuous(breaks = seq(0,1,.2), name = "Percent of random sample in Consistent Area",labels = c("0","20","40","60","80","100"))  + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                                                                                   axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


# Combining classes 2-3 and 4




output_mean_merge <- list()
output_sd_merge <- list()

set.seed(41)
for (i in 1:4){
  df <- list[[i]]
  prop_df <- data.frame(matrix(nrow = 3, ncol = 1000))
  for (j in 1:1000){
    # Generate 1000 random row indices
    random_indices <- sample(1:nrow(df), 1000, replace = FALSE)
    # Subset the dataset with the randomly selected indices
    subset_df <- df[random_indices, ]
    values <- as.data.frame(table(subset_df[,1]))
    vals_high <- values %>% filter(Var1 == "2"|Var1 == "4")
    vals_high_sum <- as.data.frame(cbind("3",sum(vals_high$Freq)))
    names(vals_high_sum) <- names(values)
    vals_low <- values %>% filter(Var1 == "0"|Var1 == "1")
    values <- rbind(vals_low,vals_high_sum)
    values$Freq <- as.integer(values$Freq)
    values <- values[,2]/sum(values[,2])
    prop_df[,j] <- values
  }
  output_mean_merge[[i]] <- rowMeans(prop_df)
  prop_mat <- as.matrix(prop_df)
  output_sd_merge[[i]] <- apply(prop_mat, 1, sd)
}

output_mean_merge[[1]]
output_mean_merge[[2]]
output_mean_merge[[3]]
output_mean_merge[[4]]

final_prop_merge <- as.data.frame(cbind(output_mean_merge[[1]],
                                  output_mean_merge[[2]],
                                  output_mean_merge[[3]],
                                  output_mean_merge[[4]]))

final_sd_merge <- as.data.frame(cbind(output_sd_merge[[1]],
                                output_sd_merge[[2]],
                                output_sd_merge[[3]],
                                output_sd_merge[[4]]))


final_prop_merge$OHV_val <- c("0","1","3")

final_prop_merge <- final_prop_merge[,c(5,1:4)]

names(final_prop_merge) <- c("OHV_val","V1970","V1980","V2010","V2020")
# write.csv(final_prop,"./sample1000.csv", row.names = FALSE)


final_sd_merge$OHV_val <- c("0","1","3")

final_sd_merge <- final_sd_merge[,c(5,1:4)]

names(final_sd_merge) <- c("OHV_val","sd1970","sd1980","sd2010","sd2020")

mean_long_merge <- final_prop_merge %>% 
  pivot_longer(
    cols = c("V1970","V1980","V2010","V2020"), 
    names_to = "year",
    values_to = "mean"
  )

sd_long_merge <- final_sd_merge %>% 
  pivot_longer(
    cols = c("sd1970","sd1980","sd2010","sd2020"), 
    names_to = "year",
    values_to = "sd"
  )

final_random_merge <- cbind(mean_long_merge,sd_long_merge[,3])

year_order <- c("V1970","V1980","V2010","V2020")


final_random_merge %>%
  ggplot(aes(fill= OHV_val, y = mean, x = as.factor(year),label = paste0(round(100*mean,1),"%")))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                position = position_dodge(0.9), width = .3)+
  scale_x_discrete(name = "Decade", label = c("1970s","1980s","2010s","2020s"),limits = year_order)+
  ggsci::scale_fill_jco(name = "OHV route\ndensity category")+ 
  geom_text(size = 3, color = "black", position = position_dodge2(width = 4),vjust=-2.5, hjust=.4) +
  scale_fill_manual(values = c("#a69d8b","#fae51e","#ff681e"),labels=c("None", "Low","Medium/High"),name="OHV route\nabundance category") +
  theme(axis.title.x = element_blank(), legend.position = "right")+
  scale_y_continuous(breaks = seq(0,1,.2), name = "Percent of random sample",labels = c("0","20","40","60","80","100"))  + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                                                                                   axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))

