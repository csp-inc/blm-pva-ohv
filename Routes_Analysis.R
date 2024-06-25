rm(list=ls())
# This script uses the WEMO routes layer to validate the OHV output

## Loading in packages -----
list.of.packages <- c("googleAuthR","googleCloudStorageR","terra","sf","dplyr", "performance",
                      "stars","ggeffects","MASS","DHARMa","pscl","AICcmodavg","lmtest","ggplot2")
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

nrow(trail_df_model)*150*150 # 2528302500 = total area in m2

st_area(dt_range_web)
(2528302500/2.49582e+11)*100 # 1.01 = % of mdt range in analysis

# Load in the saved model data
trail_df_model <- read.csv("./other_data/routes/routes_data_cleaned_model.csv")
# trail_df_model <- read.csv("./other_data/routes/routes_data_model.csv")

## Links -----

# https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
# https://webhome.auburn.edu/~tds0009/Articles/Martin%20et%20al.%202005.pdf
# https://www.sciencedirect.com/science/article/abs/pii/S0378375809000810#:~:text=Without%20confusion%2C%20overdispersion%20can%20be,accounted%20for%20through%20zero%2Dinflation.

## Ordinal regression -----
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


new_data_test <- data.frame(routes_length = c(0,50,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600,625,650))

pred_dat <- predict_response(ord_model, terms = new_data_test)

response.level <- c("0","1","2","4")
OHV.density <- c("None","Low","Medium","High")
datf <- as.data.frame(cbind(response.level,OHV.density))
pred_dat <- pred_dat %>% left_join(datf)

# Define your colors for each category
category_colors <- c("None" = "#a69d8b", "Low" = "#fae51e", "Medium" = "darkorange","High" ="red")
legend_order <- c("None", "Low", "Medium", "High")


### Figure 7 ------
plot1 <- ggplot(data = pred_dat, aes(x = x, y = predicted, colour = factor(OHV.density,levels = legend_order)))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(OHV.density, levels = legend_order)), alpha = 0.1, linetype = "dashed", size = 0.25)+
  geom_line(size = 0.75) + 
  scale_color_manual(values = category_colors) + # Assign colors manually
  scale_fill_manual(values = category_colors) + # Match fill colors to line colors
  xlab("Density of known OHV routes (total length (m) in 150-m² cell)") + ylab("Probability of OHV route density category") + 
  theme_classic() +
  theme(
    legend.position = "top",  # Set legend position to top
    legend.box = "horizontal",  # Arrange legend items horizontally
    # text = element_text(family = "Times New Roman") # Set font to Times New Roman
    axis.text.x = element_text(color="black",size=12),
    axis.text.y = element_text(color = "black",size=12),
    axis.title.y = element_text(color="black",size=12),axis.title.x = element_text(color="black",size=12),
    legend.text = element_text(color="black",size=12)
  )

plot1


class_list <- split(pred_dat,pred_dat$response.level)
function_list <- list()
for(i in 2:4){
  function_list[[i-1]] <- approxfun(class_list[[i]]$x,class_list[[i]]$predicted)
}

x <- c(0,50,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600,625,650)
low_med <- uniroot(function(x)  function_list[[1]](x) - function_list[[2]](x)  , c(0,400), tol=1e-8)     #214
med_high <- uniroot(function(x)  function_list[[2]](x) - function_list[[3]](x)  , c(0,400), tol=1e-8)    #392 

ggsave("./figures/routes_figure.jpeg",height = 5, width = 7.7)


# Finding actual density class distribution by known route length

library("reshape2")

data <- trail_df_model %>% filter(routes_length < 700)

data<- melt(data)

max(data$value)


ggplot(data,aes(x=routes_length, fill=val_ord)) + geom_density(alpha=0.25)
ggplot(data,aes(x=routes_length, fill=val_ord)) + geom_histogram(alpha=0.25)
ggplot(data,aes(x=val_ord, y=routes_length, fill=val_ord)) + geom_boxplot()





