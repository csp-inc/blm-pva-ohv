library(googleAuthR)
library(googleCloudStorageR)
library(terra)
library(sf)
library(dplyr)

gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")
# This script uses the WEMO routes layer to validate the OHV output

# Load in the route density layer created in QGIS
trails <- rast("./other_data/routes/Route_density.tif")
trails <- classify(trails, cbind(0, NaN), right=FALSE)
trails <- mask(crop(trails,dt_range_web),dt_range_web)
plot(trails)

N10 <- rast("./output_layers/N10_04052024.tif")

# Using the trails layer to mask the 2010 OHV layer
# Only cells with a route density value will remain
N10_masked_4trails <- mask(N10,trails)
plot(N10_masked_4trails)

length(values(trails))
length(values(N10_masked_4trails))

# Getting the cell values for use in the glm
trail_dens <- values(trails)
N10_OHV <- values(N10_masked_4trails)

trail_df <- as.data.frame(cbind(trail_dens,N10_OHV))

trail_df <- trail_df %>% mutate(include = rowSums(is.na(trail_df)),
                                cell_id = c(1:nrow(trail_df)))

# Removing data where both trails and OHV values were NA
trail_df_model <- trail_df %>% filter(!include == 2)

cols_with_na <- colSums(is.na(trail_df_model)) > 0

# Running a glm
model <- glm(N10_OHV ~ trail_dens, data = trail_df_model, family = "poisson")
summary(model)

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

# Creating layer for residuals in space
writeRaster(resid_spat, "./other_data/routes/model_resid.tif", overwrite = TRUE)

resid <- rast("./other_data/routes/model_resid.tif")
hist(values(resid))




# 
# if(dir.exists("./other_data/roads") == FALSE){dir.create("./other_data/roads")}
# 
# roads2012_contents <- gcs_list_objects(bucket = bucket_name, prefix = "data/05_covariate_outputs/TIGER_roads/TIGER_roads_2012")
# roads2012_contents <- roads2012_contents$name
# 
# purrr::map(roads2012_contents, function(x)
#   gcs_get_object(x, bucket = "gs://csp_tortoisehub", overwrite = TRUE,
#                  saveToDisk = paste0("./other_data/roads/",basename(x))))

template_rast_web <- rast("./output_layers/N10_04052024.tif")

roads <- st_read("./other_data/roads/TIGER_2012_roads_merged.shp")
road_filt <- roads %>% filter(MTFCC == "S1100" | MTFCC == "S1200" | MTFCC == "S1400")
road_web <- st_transform(road_filt, "EPSG:3857")
road_crop <- st_crop(road_web,dt_range_web)
road_vect <- vect(road_crop)
road_rast <- rasterize(road_vect, template_rast_web,cover=TRUE)

road_dist <- distance(road_rast)
writeRaster(road_dist,"./other_data/routes/TIGER_2010_road_dist.tif",overwrite=TRUE)
plot(road_dist)

# This layer had the distance function performed on it in QGIS

road_dist <- rast("./other_data/routes/TIGER_2010_road_dist.tif")
road_dist_150 <- project(road_dist,resid)
road_dist_150_vals <- values(road_dist_150)


trail_df_resid$road_dist <- road_dist_150_vals

trail_df_model2 <- trail_df_resid %>% filter(!include == 2)

model_roads <- lm(resid ~ road_dist, data = trail_df_model2)
summary(model_roads)


if(dir.exists("./models/linear") == FALSE){dir.create("./models/linear")}

saveRDS(model,"./models/linear/OHV_WEMO_linear.RDS")
saveRDS(model_roads,"./models/linear/resid_Roads_linear.RDS")
