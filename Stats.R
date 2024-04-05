rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr","tidyr","lme4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


n70 <- rast("./output_layers/n70_04052024.tif")
n80 <- rast("./output_layers/n80_04052024.tif")
N10 <- rast("./output_layers/N10_04052024.tif")
N20 <- rast("./output_layers/N20_04052024.tif")
# Stacking
stack <- c(n70,n80,N10,N20)

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

# Loading in the CA half of MDT range shapefile
ca_dt_range <- st_read("./shapefiles/CA_DTrange/ca_dtrange_web.shp")
dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

# Loading in the old 2010 and 2020 layers to do some comparison
N10_old <- rast("./Old_OHV/NAIP_2010_collins.tif")
N20_old <- rast("./Old_OHV/NAIP_2019_collins.tif")
N20_old <- N20_old[[1]]


# Plotting to see extents
par(mfrow = c(2,2))
plot(N10_old, main = "2010 OLD",colNA="black")
plot(N10, main = "2010 NEW",colNA="black")
plot(N20_old, main = "2020 OLD",colNA="black")
plot(N20, main = "2020 NEW",colNA="black")


par(mfrow = c(2,2))
plot(n70, main = "1970",colNA="black")
plot(n80, main = "1980",colNA="black")
plot(N10, main = "2010",colNA="black")
plot(N20, main = "2020",colNA="black")

## Creating a change raster between the old and the new 2010 layers
cont_list <- list()
cont_list[[1]] <- N10_old
cont_list[[2]] <- N10
cont_list[[3]] <- N20_old
cont_list[[4]] <- N20


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

# focal_num<-500
# focal_shape<-'circle'
# foc_mat<-focalMat(diff_2010, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
# foc_mat[foc_mat>0] <- 1
# diff_2010_sum_500 <-focal(diff_2010, foc_mat, fun = "sum", na.policy = "omit", na.rm = TRUE)
# 
# 
# focal_num<-500
# focal_shape<-'circle'
# foc_mat<-focalMat(diff_2020, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
# foc_mat[foc_mat>0] <- 1
# diff_2020_sum_500 <-focal(diff_2020, foc_mat, fun = "sum", na.policy = "omit", na.rm = TRUE)
# 




# Comparing old and new layers and their total area of each OHV class

decades <- c("2010 OLD","2010 NEW","2020 OLD","2020 NEW")
prop_list <- list()


prop_list[[1]] <- N10_old
prop_list[[2]] <- N10
prop_list[[3]] <- N20_old
prop_list[[4]] <- N20



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

st_area(ca_dt_range) #153893198778 meters squared

prop_years$CA_area_prop <- prop_years$area/153893198778
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


# Looking at all full layers and their total area and relative area of each OHV class

decades <- c("1970s","1980s","2010s","2020s")
prop_list <- list()


## DEPENDING ON WHICH LAYER YOU WANT (masked or not) ------------------------------------
prop_list[[1]] <- n70
prop_list[[2]] <- n80
prop_list[[3]] <- N10
prop_list[[4]] <- N20

#------------------------------------

for(i in 1:length(prop_list)){
  prop <- as.data.frame(table(values(prop_list[[i]])))
  prop$sum <- sum(prop$Freq)
  prop$area <- prop$Freq*(150*150)
  prop$dec_area <- sum(prop$area)
  prop$Decade <- decades[i]
  prop_list[[i]] <- prop
}
prop_years <- bind_rows(prop_list)

prop_years$Proportion <- prop_years$Freq/prop_years$sum

st_area(dt_range)

prop_years$area_prop <- prop_years$area/2.49582e+11
prop_years$dec_area_prop <- prop_years$area/prop_years$dec_area
names(prop_years) <- c("Class","Freq","sum","area","dec_area","Decade","Proportion","Range_area_prop","dec_area_prop")

prop_years$Class <- as.character(prop_years$Class)


# Proportion of total mdt area with each OHV class
ggplot(prop_years, aes(fill=Class, y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Range Coverage") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV density\ncategory",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


# Proportion of classified area with each OHV class
ggplot(prop_years, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV density\ncategory",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))





# Joining classes 2 and 4
decades <- c("1970s","1980s","2010s","2020s")
joined_list <- list()
for (i in 1:length(decades)){
  year <- filter(prop_years, Decade == decades[i])
  high <- filter(year, Class == "2" | Class == "4")
  high_prop <- colSums(high[,c("Freq","Proportion","area","Range_area_prop","dec_area_prop")])
  high_prop_df <- t(as.data.frame(c("2",as.numeric(unname(high_prop[1])),high[1,3],as.numeric(unname(high_prop[3])),high[1,5],decades[i],as.numeric(unname(high_prop[2])),as.numeric(unname(high_prop[4])),as.numeric(unname(high_prop[5])))))
  high_prop_df <- as.data.frame(high_prop_df)
  colnames(high_prop_df) <- colnames(high)
  rownames(high_prop_df) <- 3
  high_prop_df$Proportion <- as.numeric(high_prop_df$Proportion)
  high_prop_df$Range_area_prop <- as.numeric(high_prop_df$Range_area_prop)
  joined_list[[i]] <- rbind(year[1:2,],high_prop_df)
}
prop_years_high <- bind_rows(joined_list)


ggplot(prop_years_high, aes(fill= factor(Class, c("0","1","2")), y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Total Area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


ggplot(prop_years_high, aes(fill= factor(Class, c("0","1","2")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



# Joining classes 1, 2 and 4
decades <- c("1970s","1980s","2010s","2020s")
joined_list <- list()
for (i in 1:length(decades)){
  year <- filter(prop_years, Decade == decades[i])
  high <- filter(year, Class == "1" | Class == "2" | Class == "4")
  high_prop <- colSums(high[,c("Freq","Proportion","area","Range_area_prop","dec_area_prop")])
  high_prop_df <- t(as.data.frame(c("2",as.numeric(unname(high_prop[1])),high[1,3],as.numeric(unname(high_prop[3])),high[1,5],decades[i],as.numeric(unname(high_prop[2])),as.numeric(unname(high_prop[4])),as.numeric(unname(high_prop[5])))))
  high_prop_df <- as.data.frame(high_prop_df)
  colnames(high_prop_df) <- colnames(high)
  rownames(high_prop_df) <- 1
  high_prop_df$Proportion <- as.numeric(high_prop_df$Proportion)
  high_prop_df$Range_area_prop <- as.numeric(high_prop_df$Range_area_prop)
  joined_list[[i]] <- rbind(year[1,],high_prop_df)
}
prop_years_binary <- bind_rows(joined_list)


ggplot(prop_years_binary, aes(fill= factor(Class, c("0","1")), y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Total Area") +
  scale_fill_manual(values = c("#30123b","#28bceb"),labels=c("None","≥1 Track"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


ggplot(prop_years_binary, aes(fill= factor(Class, c("0","1")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb"),labels=c("None","≥1 Track"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))






# Stats from in the smallest spatial coverage area

stack_masked <- mask(stack,mask)

prop_list <- list()
prop_list[[1]] <- stack_masked[[1]]
prop_list[[2]] <- stack_masked[[2]]
prop_list[[3]] <- stack_masked[[3]]
prop_list[[4]] <- stack_masked[[4]]


for(i in 1:length(prop_list)){
  prop <- as.data.frame(table(values(prop_list[[i]])))
  prop$sum <- sum(prop$Freq)
  prop$area <- prop$Freq*(150*150)
  prop$dec_area <- sum(prop$area)
  prop$Decade <- decades[i]
  prop_list[[i]] <- prop
}
prop_years <- bind_rows(prop_list)

prop_years$Proportion <- prop_years$Freq/prop_years$sum

st_area(dt_range)

prop_years$area_prop <- prop_years$area/2.49582e+11
prop_years$dec_area_prop <- prop_years$area/prop_years$dec_area
names(prop_years) <- c("Class","Freq","sum","area","dec_area","Decade","Proportion","Range_area_prop","dec_area_prop")

prop_years$Class <- as.character(prop_years$Class)



# Proportion of classified area with each OHV class
ggplot(prop_years, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of consistent area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV density\ncategory",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




# Joining classes 2 and 4
decades <- c("1970s","1980s","2010s","2020s")
joined_list <- list()
for (i in 1:length(decades)){
  year <- filter(prop_years, Decade == decades[i])
  high <- filter(year, Class == "2" | Class == "4")
  high_prop <- colSums(high[,c("Freq","Proportion","area","Range_area_prop","dec_area_prop")])
  high_prop_df <- t(as.data.frame(c("2",as.numeric(unname(high_prop[1])),high[1,3],as.numeric(unname(high_prop[3])),high[1,5],decades[i],as.numeric(unname(high_prop[2])),as.numeric(unname(high_prop[4])),as.numeric(unname(high_prop[5])))))
  high_prop_df <- as.data.frame(high_prop_df)
  colnames(high_prop_df) <- colnames(high)
  rownames(high_prop_df) <- 3
  high_prop_df$Proportion <- as.numeric(high_prop_df$Proportion)
  high_prop_df$Range_area_prop <- as.numeric(high_prop_df$Range_area_prop)
  joined_list[[i]] <- rbind(year[1:2,],high_prop_df)
}
prop_years_high <- bind_rows(joined_list)


ggplot(prop_years_high, aes(fill= factor(Class, c("0","1","2")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of consistent area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
                    name="OHV density\ncategory") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                            axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



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

## Run this manually and change the output raster name
for(i in 1:3){
  i <- 3
  change_col <- as.data.frame(change_class[,i])
  names(change_col) <- "Change"
  change_col_df <- change_col %>% left_join(change_codes)
  change_rast <- stack_reclass[[1]]
  values(change_rast) <- change_col_df$change_cols
  plot(change_rast)
  hist(values(change_rast))
  values(change_rast) <- values(change_rast)-1
  hist(values(change_rast))
  writeRaster(change_rast,"./output_layers/Change_Raster_Binary_2010_2020.tif",overwrite = TRUE)
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
c_70_80 <- rast("./OHV_Change/Change_Raster_Binary_1970_1980.tif")
c_80_2010 <- rast("./OHV_Change/Change_Raster_Binary_1980_2010.tif")
c_2010_2020 <- rast("./OHV_Change/Change_Raster_Binary_2010_2020.tif")
c_80_2020 <- rast("./OHV_Change/Change_Raster_Binary_1980_2020.tif")


# Apply moving window
c_2010_2020
focal_num<-500
focal_shape<-'circle'
foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
foc_mat[foc_mat>0] <- 1


result<-focal(raster, foc_mat, fun = "modal", na.policy = "all", na.rm = TRUE)
clip_result <- mask(result, dt_range)

plot(clip_result, background = "black")


hist(values(clip_result))

# Save output
# Be sure to change name depending on radius and input change raster
writeRaster(clip_result,"./OHV_Change/Window500m_mode_change_2010_2020.tif",overwrite = TRUE)


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


### 

values_df <- read.csv("./other_data/master/master_cells.csv")

# values_df <- values_df[complete.cases(values_df), ]

# Sampling 1000 cells 1000 times from each decade

values_df_70 <- values_df[,c(2,6)]
values_df_70 <- values_df_70[complete.cases(values_df_70$V70), ]

values_df_80 <- values_df[,c(3,6)]
values_df_80 <- values_df_80[complete.cases(values_df_80$V80), ]

values_df_10 <- values_df[,c(4,6)]
values_df_10 <- values_df_10[complete.cases(values_df_10$V10), ]

values_df_20 <- values_df[,c(5,6)]
values_df_20 <- values_df_20[complete.cases(values_df_20$V20), ]

list <- list()
list[[1]] <- values_df_70
list[[2]] <- values_df_80
list[[3]] <- values_df_10
list[[4]] <- values_df_20

output_mean <- list()
output_sd <- list()

set.seed(42)
for (i in 1:4){
  df <- list[[i]]
  prop_df <- data.frame(matrix(nrow = 4, ncol = 1000))
  for (j in 1:1000){
    # Generate 1000 random row indices
    random_indices <- sample(1:nrow(df), 1000, replace = FALSE)
    # Subset the dataset with the randomly selected indices
    subset_df <- df[random_indices, ]
    values <- as.data.frame(table(subset_df[,1]))
    values <- values[,2]/sum(values[,2])
    prop_df[,j] <- values
  }
  output_mean[[i]] <- rowMeans(prop_df)
  prop_mat <- as.matrix(prop_df)
  output_sd[[i]] <- apply(prop_mat, 1, sd)
}

output_mean[[1]]
output_mean[[2]]
output_mean[[3]]
output_mean[[4]]

final_prop <- as.data.frame(cbind(output_mean[[1]],
      output_mean[[2]],
      output_mean[[3]],
      output_mean[[4]]))

final_sd <- as.data.frame(cbind(output_sd[[1]],
                                  output_sd[[2]],
                                  output_sd[[3]],
                                  output_sd[[4]]))


final_prop$OHV_val <- c("0","1","2","4")

final_prop <- final_prop[,c(5,1:4)]

names(final_prop) <- c("OHV_val","V70","V80","V10","V20")
# write.csv(final_prop,"./sample1000.csv", row.names = FALSE)


final_sd$OHV_val <- c("0","1","2","4")

final_sd <- final_sd[,c(5,1:4)]

names(final_sd) <- c("OHV_val","sd70","sd80","sd10","sd20")

mean_long <- final_prop %>% 
  pivot_longer(
    cols = c("V70","V80","V10","V20"), 
    names_to = "year",
    values_to = "mean"
  )

sd_long <- final_sd %>% 
  pivot_longer(
    cols = c("sd70","sd80","sd10","sd20"), 
    names_to = "year",
    values_to = "sd"
  )

final_random <- cbind(mean_long,sd_long[,3])

year_order <- c("V70","V80","V10","V20")


final_random %>%
  ggplot(aes(fill= OHV_val, y = mean, x = as.factor(year),label = paste0(round(100*mean,1),"%")))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                position = position_dodge(0.9), width = .3)+
  scale_x_discrete(name = "Decade", label = c("1970s","1980s","2010s","2020s"),limits = year_order)+
  ggsci::scale_fill_jco(name = "OHV route\ndensity category")+ 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-2.5, hjust=.4) +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),labels=c("None", "1","2-3","4+"),name="OHV route\ndensity category") +
  theme(axis.title.x = element_blank(), legend.position = "right")+
  scale_y_continuous(breaks = seq(0,1,.2), name = "Percent of random sample \n in Classified Area",labels = c("0","20","40","60","80","100"))  + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                                                                                   axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))






## Creating the long dataframe from the master sheet

values_df <- read.csv("./OHV_SHP/master_cells.csv")

values_df_long <- values_df[,2:8] %>% 
    pivot_longer(
       cols = c("V70","V80","V10","V20"), 
       names_to = "year",
        values_to = "value"
      )

values_df_long <- values_df_long[complete.cases(values_df_long$value), ]

# To sample 25% of total cells from all cells need 2730884

sampled_values <- sample(unique(values_df_long$raster_cell), 50000, replace = FALSE)

values_df_sub <- values_df_long %>% filter(raster_cell %in% sampled_values)
str(values_df_sub)

library(lme4)
model <- glmer(value ~ year + (1 | state), data = values_df_sub, family = "poisson")
summary(model)
ranef(model)

val_2010 <- exp(-1.05205) # 0.3492211
val_2020 <- exp(0.05783) # 1.059535 = 1.408756
val_1970 <- exp(-0.35575) # 0.7006478 = 1.049869
val_1980 <- exp(-0.20280) # 0.8164415 = 1.165663


library(sjPlot)
library(effects)

plot_model(model, type = "est", show.values = TRUE)

par(mfrow=c(2,2))
plot(model)

residuals <- residuals(model)

# Fit a 0-inflated poisson in glmer
model <- glmer(cbind(value == 0, value) ~ year + (1 | state),
               data = values_df_sub,
               family = binomial(link = "logit"))
summary(model)

plot(model)

plot_model(model, type = "est", show.values = TRUE)





trails <- rast("./Routes/Route_density.tif")
trails <- classify(trails, cbind(0, NaN), right=FALSE)
trails <- mask(crop(trails,dt_range),dt_range)
plot(trails)

N10_masked_4trails <- mask(N10,trails)
plot(N10_masked_4trails)

length(values(trails))
length(values(N10_masked_4trails))

trail_dens <- values(trails)
N10_OHV <- values(N10_masked_4trails)

trail_df <- as.data.frame(cbind(trail_dens,N10_OHV))

trail_df <- trail_df %>% mutate(include = rowSums(is.na(trail_df)),
                                cell_id = c(1:nrow(trail_df)))

trail_df_model <- trail_df %>% filter(!include == 2)


model <- glm(N10_OHV ~ trail_dens, data = trail_df_model, family = "poisson")
summary(model)

resid <- residuals(model)

trail_df_model <- cbind(trail_df_model,resid)

trail_df_resid <- trail_df %>% left_join(trail_df_model[,c("cell_id","resid")], by = "cell_id")
trail_df_resid$resid_exp <- exp(trail_df_resid$resid)

resid_spat <- N10_masked_4trails
values(resid_spat) <- trail_df_resid$resid

hist(values(resid_spat))

plot(resid_spat)
setwd("/Users/madicsp/Desktop")
writeRaster(resid_spat, "./Routes/model_resid.tif")


S1400_dist <- rast("./Routes/TIGER_2010_S1400_dist.tif")
S1400_dist_150 <- project(S1400_dist,resid_spat)
S1400_dist_150_vals <- values(S1400_dist_150)

highway_dist <- rast("./Routes/TIGER_2010_highway_dist.tif")
highway_dist_150 <- project(highway_dist,resid_spat)
highway_dist_150_vals <- values(highway_dist_150)

trail_df_resid$S1400_dist <- S1400_dist_150_vals
trail_df_resid$highway_dist <- highway_dist_150_vals


trail_df_model2 <- trail_df_resid %>% filter(!include == 2)

model_S1400 <- lm(resid ~ S1400_dist, data = trail_df_model2)
summary(model_S1400)


model_highway <- lm(resid ~ highway_dist, data = trail_df_model2)
summary(model_highway)



