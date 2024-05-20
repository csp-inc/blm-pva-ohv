rm(list=ls())
# This script uses the WEMO routes layer to validate the OHV output

## Loading in packages -----
list.of.packages <- c("googleAuthR","googleCloudStorageR","terra","sf","dplyr", "performance",
                      "stars","ggeffects","MASS","DHARMa","pscl","AICcmodavg","lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Authorizing GCS access
gcs_auth(json_file = "csp-inc.json", token = NULL, email = NULL)
bucket_name<-"gs://csp_tortoisehub"

# Loading in dt range
dt_range_web <- st_read("./shapefiles/DTrange/dtrange_web.shp")

if(dir.exists("./models/Routes") == FALSE){dir.create("./models/Routes")}

## Creating the dataframe -----
# Loading in the 2010 layer you want to use
# N10 <- rast("./output_layers/N10_04052024.tif")
# N10 <- rast("./output_layers/NAIP_2010_masked_9.tif")
N10 <- rast("./output_layers/NAIP_2010_masked_9_nlcdmask_roadsmask.tif")


# Convert raster to sf object
N10_stars <- st_as_stars(N10)
N10_sf <- st_as_sf(N10_stars) # takes a minute
N10_sf$ID <- c(1:nrow(N10_sf))
names(N10_sf) <- c("OHV_2010","geometry","cell_ID")

# Save the grid object
# st_write(N10_sf,"./other_data/routes/N10_grid.shp", append = FALSE)
st_write(N10_sf,"./other_data/routes/N10_grid_cleaned.shp", append = FALSE)


# In QGIS use grid object to intersect with routes shapefile
# routes_clipped <- st_read("./other_data/routes/Routes_wholegrid_clipped.shp")
routes_clipped <- st_read("./other_data/routes/Routes_wholegridcleaned_clipped.shp")
routes_clipped$new_length <- NA

# Find the length of each road segment clipped to raster cells
for(i in 1:nrow(routes_clipped)){
  routes_clipped[i,5] <- st_length(routes_clipped[i,])
}

length(unique(routes_clipped$cell_ID))

# Split by cell ID
cell_list <- split(routes_clipped, routes_clipped$cell_ID)

# Find the total length of all routes in a cell
cell_lengths_tot <- c()
for(i in 1:length(cell_list)){
  cell_lengths_tot[i] <- sum(cell_list[[i]]$new_length)
}

# Find the total number of unique routes in a cell
cell_number_tot <- c()
for(i in 1:length(cell_list)){
  cell_number_tot[i] <- nrow(cell_list[[i]])
}

# Find the OHV value in the cell
cell_ohv_val <- c()
for(i in 1:length(cell_list)){
  cell_ohv_val[i] <- unique(cell_list[[i]]$OHV_2010)
}

# Create the dataframe
routes_length <- cell_lengths_tot
routes_number <- cell_number_tot
OHV_2010 <- cell_ohv_val
cell_ID <- names(cell_list)
route_dens_df <- as.data.frame(cbind(routes_length,routes_number,OHV_2010,cell_ID))

# Format the dataframe
trail_df_model <- route_dens_df
trail_df_model <- trail_df_model %>%
  mutate_at(c('OHV_2010','routes_length','routes_number'), as.numeric)

names(trail_df_model) <- c("routes_length","routes_number","OHV_dens","cell_ID")

trail_df_model <- trail_df_model[complete.cases(trail_df_model), ]

# Save the dataframe
# write.csv(trail_df_model,"./other_data/routes/routes_data_model.csv", row.names = FALSE)
write.csv(trail_df_model,"./other_data/routes/routes_data_cleaned_model.csv", row.names = FALSE)


## Part I: WEMO known OHV routes analysis -----
# Load in the saved model data
trail_df_model <- read.csv("./other_data/routes/routes_data_cleaned_model.csv")
# trail_df_model <- read.csv("./other_data/routes/routes_data_model.csv")

##### Links -----

# https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
# https://webhome.auburn.edu/~tds0009/Articles/Martin%20et%20al.%202005.pdf
# https://www.sciencedirect.com/science/article/abs/pii/S0378375809000810#:~:text=Without%20confusion%2C%20overdispersion%20can%20be,accounted%20for%20through%20zero%2Dinflation.

#### Ordinal regression -----
# https://www.bookdown.org/rwnahhas/RMPH/blr-ordinal.html

trail_df_model <- trail_df_model %>% mutate(val_ord = as.factor(OHV_dens))
levels(trail_df_model$val_ord)

ord_model <- polr(val_ord ~ routes_length, data = trail_df_model, Hess = T)
summary(ord_model)


exp(ord_model$zeta)/(1+exp(ord_model$zeta))

CI    <- confint(ord_model)
TSTAT <- summary(ord_model)$coef[1, "t value"]
data.frame(
  AOR   = exp(ord_model$coefficients),
  lower = exp(CI[1]),
  upper = exp(CI[2]),
  p     = 2*pnorm(abs(TSTAT), lower.tail = F)
)


car::Anova(ord_model, type = 3)


exp(ord_model$coefficients*100)
exp(CI[1]*100)
exp(CI[2]*100)
# For each 100m increase in known OHV route length, a given chip has 89% greater odds of having a higher OHV route density classification

exp(ord_model$coefficients)
exp(CI[1])
exp(CI[2])


new_data_test <- data.frame(routes_length = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650))

pred_dat <- predict_response(ord_model, terms = new_data_test)

response.level <- c("0","1","2","4")
OHV.density <- c("None","Low","Medium","High")
datf <- as.data.frame(cbind(response.level,OHV.density))
pred_dat <- pred_dat %>% left_join(datf)

# Define your colors for each category
category_colors <- c("None" = "#a69d8b", "Low" = "#fae51e", "Medium" = "darkorange","High" ="red")
legend_order <- c("None", "Low", "Medium", "High")

plot1 <- ggplot(data = pred_dat, aes(x = x, y = predicted, colour = factor(OHV.density,levels = legend_order)))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(OHV.density, levels = legend_order)), alpha = 0.1, linetype = "dashed", size = 0.25)+
  geom_line(size = 0.75) + 
  scale_color_manual(values = category_colors) + # Assign colors manually
  scale_fill_manual(values = category_colors) + # Match fill colors to line colors
  xlab("Density of known OHV routes (total length (m) in 150-m² chip)") + ylab("Probability of OHV route density category") + 
  theme_classic() +
  theme(
    legend.position = "top",  # Set legend position to top
    legend.box = "horizontal"  # Arrange legend items horizontally
    # text = element_text(family = "Times New Roman") # Set font to Times New Roman
  )

plot1


ggsave("./routes_figure.jpeg")

#### poisson -----
model <- glm(OHV_dens ~ routes_length, data = trail_df_model, family = "poisson")
summary(model)

AIC(model)
AICc(model)
BIC(model)
exp(model$coefficients)
-2*logLik(model)


exp(model$coefficients) #IRR

# For each 100m increase in road...
exp(model$coefficients*100)
exp(confint(model)[2,1]*100)
exp(confint(model)[2,2]*100)

saveRDS(model,"./models/Routes/model_WEMO_pois.RDS")

model <- readRDS("./models/Routes/model_WEMO_pois.RDS")

# From DARMa package
# Generating residuals
model_resid <- simulateResiduals(fittedModel = model, plot = F)

plot(model_resid)
testOutliers(model,type = 'bootstrap') 
testDispersion(model_resid) # Model is overdispersed (1.6)
testZeroInflation(model_resid) # model is 0-inflated (underfitting, 1.3)

routes_length_fake <- seq(min(trail_df_model$routes_length, na.rm = TRUE), max(trail_df_model$routes_length, na.rm = TRUE), length.out = 25)

new_data_test <- data.frame(routes_length = c(0.001933,78.360526,156.719120,235.077714,313.436308,391.794901,470.153495,548.512089,626.870683))

pred_dat <- predict_response(model, terms = new_data_test)
plot1.1 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Poisson")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.1

#### negative binomial -----
# https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/#:~:text=Negative%20binomial%20regression%20analysis,estimate%20a%20negative%20binomial%20regression.&text=R%20first%20displays%20the%20call,scores%2C%20and%20p%2Dvalues.
# https://rdrr.io/cran/lme4/man/glmer.nb.html

model_nb <- glm.nb(OHV_dens ~ routes_length, data = trail_df_model)
summary(model_nb)
saveRDS(model_nb,"./models/Routes/model_WEMO_nb.RDS")

AIC(model_nb)
AICc(model_nb)
BIC(model_nb)
exp(model_nb$coefficients)
-2*logLik(model_nb)


exp(model_nb$coefficients) # IRR

# For each 100m increase in road...
exp(model_nb$coefficients*100)
exp(confint(model_nb)[2,1]*100)
exp(confint(model_nb)[2,2]*100)

model_nb <- readRDS("./models/Routes/model_WEMO_nb.RDS")

# Likelihood ratio test
lrtest(model, model_nb) # neg binomial better than poisson

testOutliers(model_nb,type = 'bootstrap') 

model_nb_resid <- simulateResiduals(fittedModel = model_nb, plot = F)
# a non-parametric test that compares the variance of the simulated residuals to the observed residuals

testDispersion(model_nb_resid) # underdispersed ( 0.98) but not significantly
testZeroInflation(model_nb_resid) # not 0-inflated

plotQQunif(simulationOutput = model_nb_resid, 
           testDispersion = FALSE,
           testUniformity = FALSE,
           testOutliers = FALSE)

ggsave("./routes_qq.jpeg")

# Using GGpredict
pred_dat <- predict_response(model_nb, terms = new_data_test)
plot1.2 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Negative Binomial")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.2

ggsave("./routes_figure.jpeg")

#### quasi-poisson -----
# https://www.utstat.toronto.edu/~guerzhoy/303/lec/lec9/binomial_poisson.html
# Lets the variance of poisson increase

model_qp <- glm(OHV_dens ~ routes_length, data = trail_df_model, family = "quasipoisson")
summary(model_qp)

exp(model_qp$coefficients)

AIC(model_qp)
AICc(model_qp)
BIC(model_qp)
-2*logLik(model_qp)

saveRDS(model_qp,"./models/Routes/model_WEMO_nb.RDS")

pred_dat <- predict_response(model_qp, terms = new_data_test)
plot1.3 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Quasi-Poisson")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.3

check_overdispersion(model_qp) # overdispersion (1.358)
check_zeroinflation(model_qp) # underfitting 0s
# AIC = NA

# Cannot compare to other models so excluding this as an option


#### 0-inflated models -----
###### Poisson -----
model_ZIP <- zeroinfl(OHV_dens ~ routes_length | ## Predictor for the Poisson process
                        routes_length, ## Predictor for the Bernoulli process;
                      dist = 'poisson',
                      data = trail_df_model)

summary(model_ZIP)
saveRDS(model_ZIP,"./models/Routes/model_WEMO_zip.RDS")

# Dispersion statistic
E2 <- resid(model_ZIP, type = "pearson")
N  <- nrow(trail_df_model)
p  <- length(coef(model_ZIP))  
sum(E2^2) / (N - p) #1.023


AIC(model_ZIP)
AICc(model_ZIP)
BIC(model_ZIP)
-2*logLik(model_ZIP)

exp(model_ZIP$coefficients$count) ## IRR = 1.001

exp(model_ZIP$coefficients$zero) ## OR = 0.9896
exp(model_ZIP$coefficients$zero)

pred_dat <- predict_response(model_ZIP, terms = new_data_test)
plot1.4 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Zero Inflated Poisson")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.4

plot(residuals(model_ZIP, type = "pearson") ~ fitted(model_ZIP))
expected_ZIP <- predict(model_ZIP, type = "response")
hist(expected_ZIP)
hist(trail_df_model$OHV_dens)

# vuong test
vuong(model_nb,model_ZIP)
# zero inflated poisson better (p < 0.0000000000000002)

###### negative binomial -----
# https://stats.oarc.ucla.edu/r/dae/zinb/
model_ZINB <- zeroinfl(OHV_dens ~ routes_length |
                            routes_length,
                          dist = 'negbin',
                          data = trail_df_model)
summary(model_ZINB)
saveRDS(model_ZINB,"./models/Routes/model_WEMO_zinb.RDS")

# Dispersion statistic
E2 <- resid(model_ZINB, type = "pearson")
N  <- nrow(trail_df_model)
p  <- length(coef(model_ZINB)) + 1 # '+1' is due to theta
sum(E2^2) / (N - p) # 1.005

# Extract residuals
E2  <- residuals(model_ZINB, type = "pearson")  # Choose the appropriate type of residuals
# Compute quantile residuals manually
quantile_residuals <- qnorm(pnorm(E2))


testDispersion(quantile_residuals)
str()

# Calculate the degrees of freedom
n_obs <- length(E2)  # Number of observations
n_params <- length(coef(model_ZINB))  # Number of parameters estimated in the model
df <- n_obs - n_params  # Degrees of freedom

# Calculate the p-value using the Pearson's chi-squared statistic
pearson_chi_squared <- sum((E2 / sqrt(18.016 ))^2)  # Pearson's chi-squared statistic
p_value <- pchisq(pearson_chi_squared, df, lower.tail = FALSE)  # Calculate p-value

# Print the result
print(p_value)


# log(theta) = 2.891249

AIC(model_ZINB)
AICc(model_ZINB)
BIC(model_ZINB)
-2*logLik(model_ZINB)

new_data_test <- data.frame(routes_length = c(0.001933,78.360526,156.719120,235.077714,313.436308,391.794901,470.153495,548.512089,626.870683,705.229276,783.587870))

pred_dat <- predict_response(model_ZINB, terms = new_data_test)
plot1.5 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Zero Inflated Negative Binomial")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.5

exp(model_ZINB$coefficients$count) ## IRR = 1.001
exp(model_ZINB$coefficients$count*100) ## for every 100 road density, 1.152 times as many predicted OHV routes
exp(confint(model_ZINB)[2,1]*100)
exp(confint(model_ZINB)[2,2]*100)

model_ZINB$coefficients$zero
exp(model_ZINB$coefficients$zero) ## OR = 0.9912 
exp(model_ZINB$coefficients$zero*100) ## for every 100 road density, 0.3339 increase in odds
exp(confint(model_ZINB)[4,1]*100)
exp(confint(model_ZINB)[4,2]*100)


# Likelihood ratio test
lrtest(model_ZIP, model_ZINB) # neg binomial is better than poisson

# vuong test
vuong(model_ZIP, model_ZINB)
# Negative binomial better (p = 0.0072)

plot(residuals(model_ZINB, type = "pearson") ~ fitted(model_ZINB))
expected_ZINB <- predict(model_ZINB, type = "response")

hist(expected_ZINB)
hist(trail_df_model$OHV_dens)



# Links for reporting of ZINB
# https://stats.oarc.ucla.edu/r/dae/zinb/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7880198/#:~:text=The%20Zero%2DInflated%20Negative%20Binomial%20Model,under%20a%20standard%20count%20model.


#### Hurdle models -----
# https://search.r-project.org/CRAN/refmans/pscl/html/hurdle.html
# Positive coefficients in the hurdle component indicate that an increase in 
# the regressor increases the probability of a non-zero count.

# Dont assume that excess 0s are product of two processes

###### poisson -----
model_hurdle_pois <- hurdle(OHV_dens ~ routes_length |
                            routes_length,
                          dist = 'poisson',
                          data = trail_df_model)
summary(model_hurdle_pois)


AIC(model_hurdle_pois)
AICc(model_hurdle_pois)
BIC(model_hurdle_pois)
-2*logLik(model_hurdle_pois)

exp(model_hurdle_pois$coefficients$count)
exp(model_hurdle_pois$coefficients$zero)


# Dispersion index
E2 <- resid(model_hurdle_pois, type = "pearson")
N  <- nrow(trail_df_model)
p  <- length(coef(model_hurdle_pois))  
sum(E2^2) / (N - p) #1.014


pred_dat <- predict_response(model_hurdle_pois, terms = new_data_test)
plot1.6 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Poisson Hurdle model")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.6

###### geometric poisson -----
model_hurdle_geom <- hurdle(OHV_dens ~ routes_length |
                              routes_length,
                            dist = 'geometric',
                            data = trail_df_model)
summary(model_hurdle_geom)

pred_dat <- predict_response(model_hurdle_geom, terms = new_data_test)
plot1.7 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Geometric Poisson Hurdle model")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.7


# Testing which hurdle models perform best in lrtest
lrtest(model_hurdle_pois,model_hurdle_geom) #poisson is better than geometric poisson

###### Negative binomial -----
model_hurdle_nb <- hurdle(OHV_dens ~ routes_length |
                            routes_length,
                          dist = 'negbin', link = "logit",
                          data = trail_df_model)
summary(model_hurdle_nb)

AIC(model_hurdle_nb)
AICc(model_hurdle_nb)
BIC(model_hurdle_nb)
-2*logLik(model_hurdle_nb)

# Dispersion index
E2 <- resid(model_hurdle_nb, type = "pearson")
N  <- nrow(trail_df_model)
p  <- length(coef(model_hurdle_nb))  
sum(E2^2) / (N - p) #1.014


# log(theta) = 6.2182821

exp(model_hurdle_nb$coefficients$count)
exp(model_hurdle_nb$coefficients$zero)
model_hurdle_nb$coefficients$zero*100

exp(model_hurdle_nb$coefficients$count*100)
exp(model_hurdle_nb$coefficients$zero*100)

exp(confint(model_hurdle_nb)[2,1]*100)
exp(confint(model_hurdle_nb)[2,2]*100)


pred_dat <- predict_response(model_hurdle_nb, terms = new_data_test)
plot1.8 <- ggplot(data = pred_dat, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) + ggtitle("Negative Binomial Hurdle model")+
  xlab("Total known route length (m)")+ylab("Predicted OHV route abundance value") + theme_minimal()

plot1.8

# Likelihood ratio test
lrtest(model_hurdle_pois,model_hurdle_nb) #no difference in poisson and neg binomial hurdle

# Vuong test
vuong(model_hurdle_pois,model_hurdle_nb) #no difference in poisson and neg binomial hurdle


# Vuong test
vuong(model_hurdle_pois,model_ZINB) #ZINB <0.0000000000000002

vuong(model_hurdle_nb,model_ZINB) #ZINB <0.0000000000000002



# Seems that ZINB is the best model for 0-inflated models
summary(model_ZINB)


## Part II: Designated roads as predictors of model performance  -----
# Getting Residuals (pearson, raw residuals standardized by the square root of the variance function)
resid <- residuals(model_ZINB)
hist(resid)



# Adding them to the data frame
trail_df_model <- cbind(trail_df_model,resid)
trail_df_model$cell_ID <- as.integer(trail_df_model$cell_ID)

trail_df_resid <- N10_sf %>% left_join(trail_df_model[,c("cell_ID","resid")], by = "cell_ID")

trail_df_resid <- trail_df_resid[complete.cases(trail_df_resid$resid), ]

# Looking at the distribution of the residuals
hist(trail_df_resid$resid)

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
library(terra)
trail_df_resid$road_dist <- c(terra::extract(road_dist,trail_df_resid, fun = "mean")[,2])


trail_df_resid <- st_drop_geometry(trail_df_resid)
write.csv(trail_df_resid,"./other_data/routes/routes_data_model2.csv", row.names = FALSE)

model_roads <- lm(resid ~ road_dist, data = trail_df_resid)
summary(model_roads)

saveRDS(model_roads,"./models/Routes/model_TIGER_lm.RDS")
# model_roads <- readRDS("./models/Routes/model_TIGER_lm.RDS")
# summary(model_roads)

dist_fake <- seq(min(trail_df_resid$road_dist),max(trail_df_resid$road_dist), length.out = 25)
dist_fake


# Create a data frame with the values of trail_dens
new_data2 <- data.frame(road_dist = c(0,375,750, 1125, 1500, 1875, 2250, 2625, 3000, 3375, 3750, 4125, 4500, 4875, 5250, 5625, 6000, 6375,
                                        6750, 7125, 7500, 7875, 8250, 8625, 9000))



pred_dat2 <- predict_response(model_roads, terms = new_data2)



plot2 <- ggplot(data = pred_dat2, aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =predicted), size = 0.75) +
  xlab("Distance from road (m)")+ylab(" Predicted Model 1 residual value") + theme_minimal() 

plot2

plot(trail_df_resid$road_dist,trail_df_resid$resid)

library(gridExtra)
grid.arrange(plot1.3, plot2, ncol=2)


