
## Loading in packages -----
list.of.packages <- c("tidyverse","sf","terra","dplyr","devtools", "RColorBrewer",
                      "remotes","purrr","nngeo","RColorBrewer","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



source("./Functions.R")

n1970 <- rast("./output_layers/n70_04052024.tif")
n1980 <- rast("./output_layers/n80_04052024.tif")
N2010 <- rast("./output_layers/N10_04052024.tif")
N2020 <- rast("./output_layers/N20_04052024.tif")

stack <- c(n1970,n1980,N2010,N2020)

output <- class_summary(stack)


# # Proportion of total mdt area with each OHV class
# ggplot(output, aes(fill=Class, y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
#   ylab("Percent of Range Coverage") +
#   scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\nabundance category",
#                     labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
#                                                                                      axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))

# Proportion of classified area with each OHV class
ggplot(output, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle("Stats raw")+
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\nabundance category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



output_join <- join2_4(output)

# # Proportion of total mdt area with each OHV class
# ggplot(output_join, aes(fill= factor(Class, c("0","1","2")), y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Range Coverage") +
#   scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
#                     name="OHV route\nabundance category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
#                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))
# 

# Proportion of classified area with each OHV class
ggplot(output_join, aes(fill= factor(Class, c("0","1","2")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c"),labels=c("None","1 Track",">1 Track"),
                    name="OHV route\nabundance category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                    axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



output_binary <- join1_4(output)

# # Proportion of total mdt area with each OHV class
# ggplot(output_binary, aes(fill= factor(Class, c("0","1")), y=Range_area_prop, x=Decade, label = paste0(round(100*Range_area_prop,1),"%"))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Range Coverage") +
#   scale_fill_manual(values = c("#30123b","#28bceb"),labels=c("None","≥1 Track"),
#                     name="OHV route\nabundance category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
#                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))
# 
# Proportion of classified area with each OHV class
ggplot(output_binary, aes(fill= factor(Class, c("0","1")), y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge2(width = 4),vjust=-.5, hjust=.5) + ylab("Percent of Classified Area") +
  scale_fill_manual(values = c("#30123b","#28bceb"),labels=c("None","≥1 Track"),
                    name="OHV route\nabundance category") + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                    axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



salt_cleaned_stack <- salt_clean(stack, writeR = FALSE)

plot(salt_cleaned_stack)


output_salt_cleaned <- class_summary(salt_cleaned_stack)

# Proportion of classified area with each OHV class
ggplot(output_salt_cleaned, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\nabundance category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))


stack_masked_nlcd <- nlcd_mask(salt_cleaned_stack, writeR = FALSE)
plot(stack_masked_nlcd)
output_nlcd_mask <- class_summary(stack_masked_nlcd)


# Proportion of classified area with each OHV class
ggplot(output_nlcd_mask, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed and nlcd mask") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\nabundance category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




stack_masked_roads <- roads_mask(salt_cleaned_stack, writeR = FALSE)
plot(stack_masked_roads)
output_roads_mask <- class_summary(stack_masked_roads)


# Proportion of classified area with each OHV class
ggplot(output_roads_mask, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed and roads mask") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\nabundance category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))



stack_masked_nlcd_roads <- roads_mask(stack_masked_nlcd, writeR = FALSE)
plot(stack_masked_nlcd_roads)
output_nlcd_roads_mask <- class_summary(stack_masked_nlcd_roads)


# Proportion of classified area with each OHV class
ggplot(output_nlcd_roads_mask, aes(fill=Class, y=Proportion, x=Decade, label = paste0(round(100*Proportion,1),"%"))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(size = 2.75, color = "black", position = position_dodge(width = .9), vjust = -.5) +
  ylab("Percent of Classified Area") + ggtitle ("Stats w/ salt removed and nlcd and roads mask") +
  scale_fill_manual(values = c("#30123b","#28bceb","#a4fc3c","#fb7e21"),name = "OHV route\nabundance category",
                    labels=c("None","Low", "Med", "High")) + theme_classic() + theme(axis.text.x = element_text(color="black"),
                                                                                     axis.text.y = element_text(color = "black"),legend.title = element_text(face = "bold"))




# Random sampling

values_df <- read.csv("./other_data/master/master_cells.csv")

final_random <- random_sampling(values_df,small_ext = TRUE)

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



# Running the moving window on whichever stack you want

# NOTE, you must call the stack you wantt to use "stack_masked" 
stack_masked <- 

stack_foc <- sum_window(stack_masked, radius = 400, writeR = FALSE)
plot(stack_foc)
writeRaster(stack_foc,"./output_layers/OHV_categorical_sum_800m_cleaned3.tif")


stack_foc_200 <- sum_window(stack_masked, radius = 200, writeR = FALSE)
plot(stack_foc_200)
writeRaster(stack_foc_200,"./output_layers/OHV_categorical_sum_400m_cleaned3.tif")



stack_masked_binary <- classify(stack_masked, cbind(1, 5, 1), right=FALSE)

stack_foc <- sum_window(stack_masked_binary, radius = 400, writeR = FALSE)
plot(stack_foc)
writeRaster(stack_foc,"./output_layers/OHV_binary_sum_800m_cleaned3.tif")


stack_foc_200 <- sum_window(stack_masked_binary, radius = 200, writeR = FALSE)
plot(stack_foc_200)
writeRaster(stack_foc_200,"./output_layers/OHV_binary_sum_400m_cleaned3.tif")


stack_masked_high <- classify(stack_masked, cbind(0, 3, 0), right=FALSE)
stack_masked_high <- classify(stack_masked_high, cbind(4, 5, 1), right=FALSE)

stack_foc <- sum_window(stack_masked_high, radius = 400, writeR = FALSE)
plot(stack_foc)
writeRaster(stack_foc,"./output_layers/OHV_high_sum_800m_cleaned3.tif")


stack_foc_200 <- sum_window(stack_masked_high, radius = 200, writeR = FALSE)
plot(stack_foc_200)
writeRaster(stack_foc_200,"./output_layers/OHV_high_sum_400m_cleaned3.tif")



