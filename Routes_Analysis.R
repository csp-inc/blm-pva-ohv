rm(list=ls())
library(googleAuthR)
library(googleCloudStorageR)
library(terra)
library(sf)
library(dplyr)

gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")
# This script uses the WEMO routes layer to validate the OHV output

if(dir.exists("./models/linear") == FALSE){dir.create("./models/linear")}

# Load in the route density layer created in QGIS
# This was created using a search radius of 107
# sqrt(150^2+150^2)/2 = 106.066 ~ 107
trails <- rast("./other_data/routes/Route_dens_recalc.tif")
trails <- classify(trails, cbind(0, NaN), right=FALSE)
trails <- mask(crop(trails,dt_range_web),dt_range_web)

# plot(trails)
# hist(values(trails), na.rm = TRUE)

N10 <- rast("./output_layers/N10_04052024.tif")
trails_reproj <- project(trails,N10,"bilinear")

# Using the trails layer to mask the 2010 OHV layer
# Only cells with a route density value will remain
N10_masked_4trails <- mask(N10,trails_reproj)


# length(values(trails_reproj))
# length(values(N10_masked_4trails))

# Getting the cell values for use in the glm
trail_dens <- values(trails_reproj)
N10_OHV <- values(N10_masked_4trails)

trail_df <- as.data.frame(cbind(trail_dens,N10_OHV))
names(trail_df) <- c("trail_dens","N10_OHV")

trail_df <- trail_df %>% mutate(include = rowSums(is.na(trail_df)),
                                cell_id = c(1:nrow(trail_df)))

# Removing data where both trails and OHV values were NA
trail_df_model <- trail_df %>% filter(!include == 2) %>% filter(!include == 1)

cols_with_na <- colSums(is.na(trail_df_model)) > 0

# # Calculate mean and variance
# mean_count <- mean(trail_df_model$N10_OHV, na.rm = TRUE)
# var_count <- var(trail_df_model$N10_OHV, na.rm = TRUE)
# dispersion_index <- var_count / mean_count

trail_df_model$trail_dens_m <- trail_df_model$trail_dens*35968.09
# hist(trail_df_model$trail_dens)

# trail_df_model$trail_dens_m_wrong <- trail_df_model$trail_dens*22500

# Model 1
model <- glm(N10_OHV ~ trail_dens_m, data = trail_df_model, family = "poisson")
summary(model)

# exp(model$coefficients)
# exp(model$coefficients*100)
# 
# exp(confint(model)[2,1]*100)
# exp(confint(model)[2,2]*100)

saveRDS(model,"./models/linear/OHV_WEMO_R107_QGIS.RDS")


# model_old <- glm(N10_OHV ~ trail_dens_m_wrong, data = trail_df_model, family = "poisson")
# summary(model_old)
# 
# saveRDS(model_old,"./models/linear/OHV_WEMO_R107_QGIS_old.RDS")

# Model 2

routes_line <- st_read("./other_data/routes/Routes_web.shp")
routes_sum <- rasterize(routes_line,N10, fun = "Sum")
plot(routes_sum)
hist(values(routes_sum))
writeRaster(routes_sum, "./other_data/routes/Rroutes_sum.tif",overwrite=TRUE)

N10_masked_4trails <- mask(N10,routes_sum)

# Getting the cell values for use in the glm
trail_count <- values(routes_sum)
N10_OHV <- values(N10_masked_4trails)

trail_df2 <- as.data.frame(cbind(trail_count,N10_OHV))
names(trail_df2) <- c("trail_count","N10_OHV")

trail_df2 <- trail_df2 %>% mutate(include = rowSums(is.na(trail_df2)),
                                cell_id = c(1:nrow(trail_df2)))

# Removing data where both trails and OHV values were NA
trail_df2_model <- trail_df2 %>% filter(!include == 2) %>% filter(!include == 1)


# Running a glm
model2 <- glm(N10_OHV ~ trail_count, data = trail_df2_model, family = "poisson")
summary(model2)


# Model 3

routes_line <- st_read("./other_data/routes/Routes_web.shp")
routes_sum_length <- rasterize(routes_line,N10, field = 7, fun = "Sum")
plot(routes_sum_length)
hist(values(routes_sum_length))
writeRaster(routes_sum_length, "./other_data/routes/Rroutes_sum_lenth.tif",overwrite=TRUE)

N10_masked_4trails <- mask(N10,routes_sum_length)

# Getting the cell values for use in the glm
trail_length <- values(routes_sum_length)
N10_OHV <- values(N10_masked_4trails)

trail_df3 <- as.data.frame(cbind(trail_length,N10_OHV))
names(trail_df3) <- c("trail_length","N10_OHV")

trail_df3 <- trail_df3 %>% mutate(include = rowSums(is.na(trail_df3)),
                                  cell_id = c(1:nrow(trail_df3)))

# Removing data where both trails and OHV values were NA
trail_df3_model <- trail_df3 %>% filter(!include == 2) %>% filter(!include == 1)


# Running a glm
model3 <- glm(N10_OHV ~ trail_length, data = trail_df3_model, family = "poisson")
summary(model3)



library(ggeffects)

density_fake <- seq(min(trail_df_model$trail_dens_m), max(trail_df_model$trail_dens_m), length.out = 25)

# Create a data frame with the values of trail_dens
new_data <- data.frame(trail_dens_m = c(0.004362798,  101.818953514,  203.633544230,  305.448134946,  407.262725662,  509.077316378,
                                        610.891907094,  712.706497810))

pred_dat <- predict_response(model, terms = new_data)
plot1 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) +
  xlab("Total route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal() + ggtitle("Model 1 (normal poisson)")

plot1


count_fake <- seq(min(trail_df2_model$trail_count), max(trail_df2_model$trail_count), length.out = 25)
# Create a data frame with the values of trail_dens
new_data2 <- data.frame(trail_count = c(1,2,3,4,5))

pred_dat2 <- predict_response(model2, terms = new_data2)


plot1.1 <- ggplot(data = pred_dat2, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) +
  xlab("Total number of known routes")+ylab("Predicted OHV route abundance value") + theme_minimal() + ggtitle("Model 1 (normal poisson)")

plot1.1


length_fake <- seq(min(trail_df3_model$trail_length), max(trail_df3_model$trail_length), length.out = 25)


# Create a data frame with the values of trail_dens
new_data3 <- data.frame(trail_length = c(5,15,30,45,60,75))

pred_dat3 <- predict_response(model3, terms = new_data3)

plot1.2<- ggplot(data = pred_dat3, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) +
  xlab("Total length of known routes")+ylab("Predicted OHV route abundance value") + theme_minimal() + ggtitle("Model 1 (normal poisson)")

plot1.2



# Getting Residuals
resid <- residuals(model)

# Adding them to the data frame
trail_df_model <- cbind(trail_df_model,resid)

trail_df_resid <- trail_df %>% left_join(trail_df_model[,c("cell_id","resid")], by = "cell_id")

# Creating a raster where the residual values are the values 
resid_spat <- N10_masked_4trails
values(resid_spat) <- trail_df_resid$resid

# Looking at the distribution of the residuals
hist(values(resid_spat))

plot(resid_spat)

plot(trail_df_model$trail_dens_m,trail_df_model$resid)


# Creating layer for residuals in space
writeRaster(resid_spat, "./other_data/routes/model_resid.tif", overwrite = TRUE)

resid_spat <- rast("./other_data/routes/model_resid.tif")
hist(values(resid_spat))




# 
# if(dir.exists("./other_data/roads") == FALSE){dir.create("./other_data/roads")}
# 
# roads2012_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/05_covariate_outputs/TIGER_roads/TIGER_roads_2012")
# roads2012_contents <- roads2012_contents$name
# 
# purrr::map(roads2012_contents, function(x)
#   gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
#                  saveToDisk = paste0("./other_data/roads/",basename(x))))

# template_rast_web <- rast("./output_layers/N10_04052024.tif")
# 
# roads <- st_read("./other_data/roads/TIGER_2012_roads_merged.shp")
# road_filt <- roads %>% filter(MTFCC == "S1100" | MTFCC == "S1200" | MTFCC == "S1400")
# road_web <- st_transform(road_filt, "EPSG:3857")
# road_crop <- st_crop(road_web,dt_range_web)
# road_vect <- vect(road_crop)
# road_rast <- rasterize(road_vect, template_rast_web,cover=TRUE)
# 
# road_dist <- distance(road_rast)
# writeRaster(road_dist,"./other_data/routes/TIGER_2010_road_dist.tif",overwrite=TRUE)


road_dist <- rast("./other_data/routes/TIGER_2010_road_dist.tif")
road_dist_150 <- project(road_dist,resid_spat)
road_dist_150_vals <- as.data.frame(values(road_dist_150))
plot(road_dist)

trail_df_resid$road_dist <- road_dist_150_vals$layer


trail_df_model2 <- trail_df_resid %>% filter(!include == 2) %>% filter(!include == 1)

hist(trail_df_model2$resid)

model_roads <- lm(resid ~ road_dist, data = trail_df_model2)
summary(model_roads)

saveRDS(model_roads,"./models/linear/resid_Roads_linear.RDS")
# model_roads <- readRDS("./models/linear/resid_Roads_linear.RDS")
# summary(model_roads)

dist_fake <- seq(min(trail_df_model2$road_dist),max(trail_df_model2$road_dist), length.out = 25)
dist_fake


# Create a data frame with the values of trail_dens
new_data2 <- data.frame(road_dist = c(0,375,750, 1125, 1500, 1875, 2250, 2625, 3000, 3375, 3750, 4125, 4500, 4875, 5250, 5625, 6000, 6375,
                                        6750, 7125, 7500, 7875, 8250, 8625, 9000))



pred_dat2 <- predict_response(model_roads, terms = new_data2)



plot2 <- ggplot(data = pred_dat2, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) +
  xlab("Distance from road (m)")+ylab(" Predicted Model 1 residual value") + theme_minimal() + ggtitle("Model 2")

plot2

plot(trail_df_model2$road_dist,trail_df_model2$resid)

library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)


## REVAMP -----------------------

N10 <- rast("./output_layers/N10_04052024.tif")

library(stars)

# Convert raster to sf object
N10_stars <- st_as_stars(N10)
N10_sf <- st_as_sf(N10_stars) # takes a minute
N10_sf$ID <- c(1:nrow(N10_sf))
names(N10_sf) <- c("OHV_2010","geometry","cell_ID")
st_write(N10_sf,"./other_data/routes/N10_grid.shp", append = FALSE)
# N10_sf <- st_read("./other_data/routes/N10_grid.shp")



# In QGIS use grid object to intersect with routes shapefile
routes_clipped <- st_read("./other_data/routes/Routes_wholegrid_clipped.shp")
routes_clipped$new_length <- NA

for(i in 1:nrow(routes_clipped)){
  routes_clipped[i,5] <- st_length(routes_clipped[i,])
}

length(unique(routes_clipped$cell_ID))

cell_list <- split(routes_clipped, routes_clipped$cell_ID)

cell_lengths_tot <- c()
for(i in 1:length(cell_list)){
  cell_lengths_tot[i] <- sum(cell_list[[i]]$new_length)
}

cell_number_tot <- c()
for(i in 1:length(cell_list)){
  cell_number_tot[i] <- nrow(cell_list[[i]])
}

cell_ohv_val <- c()
for(i in 1:length(cell_list)){
  cell_ohv_val[i] <- unique(cell_list[[i]]$OHV_2010)
}


routes_length <- cell_lengths_tot
routes_number <- cell_number_tot
OHV_2010 <- cell_ohv_val
cell_ID <- names(cell_list)
route_dens_df <- as.data.frame(cbind(routes_length,routes_number,OHV_2010,cell_ID))


trail_df_model_revamp <- route_dens_df

trail_df_model_revamp <- trail_df_model_revamp %>%
  mutate_at(c('OHV_2010','routes_length','routes_number'), as.numeric)

names(trail_df_model_revamp) <- c("routes_length","routes_number","OHV_dens","cell_ID")

trail_df_model_revamp <- trail_df_model_revamp[complete.cases(trail_df_model_revamp), ]


# Running a glm
model_test <- glm(OHV_dens ~ routes_length, data = trail_df_model_revamp, family = "poisson")
summary(model_test)


routes_length_fake <- seq(min(trail_df_model_revamp$routes_length, na.rm = TRUE), max(trail_df_model_revamp$routes_length, na.rm = TRUE), length.out = 25)


# Create a data frame with the values of trail_dens
new_data_test <- data.frame(routes_length = c(0.001932627,78.360526381,156.719120135,235.077713889,313.436307644,391.794901398,
                                        470.153495152,548.512088906,626.870682660,705.229276414))

pred_dat <- predict_response(model_test, terms = new_data_test)
plot1 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) +
  xlab("Total route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1



