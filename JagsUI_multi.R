##### Run a model -----

# Clear memory (if necessary)
rm(list=ls())
options(scipen=999)

# Load packages and function
library(lubridate)
library(jagsUI)
library(parallel)
library(terra)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)


# Load the functions used to process telemetry and estimate survival
source("./multi_jags_functions.R")


# Read in dataframe
values_df <- read.csv("./other_data/master/master_cells.csv")
values_df <- values_df[,-1]

# Find the change between each decade time step (trend), and the starting value
values_df <- values_df %>% mutate(C1 = paste0(V1980 - V1970,V1970),
                                  C2 = paste0(V2010 - V1980,V1980),
                                  C3 = paste0(V2020 - V2010,V2010))

# Pivot dataframe to be in the long direction
values_df_long <- values_df[,c(5:10)] %>%
  pivot_longer(
    cols = c("C1","C2","C3"),
    names_to = "change_class",
    values_to = "trend"
  )


# Separate the change value and the starting value
values_df_long$start_val <- str_sub(as.character(values_df_long$trend),-1,-1)
values_df_long$trend <- substr(values_df_long$trend, 1, nchar(values_df_long$trend) - 1)

values_df_long$trend[values_df_long$trend == "NA"] <- NA
values_df_long$trend[values_df_long$trend == "NaN"] <- NA
values_df_long$trend[values_df_long$trend == "NAN"] <- NA

# Remove rows where the change was NA
values_df_long <- values_df_long[complete.cases(values_df_long$trend), ]

values_df_long$trend <- as.numeric(values_df_long$trend)

# Making change values multinomial with 3 values
values_df_long$trend[values_df_long$trend == 1] <- "Inc"
values_df_long$trend[values_df_long$trend == 2] <- "Inc"
values_df_long$trend[values_df_long$trend == 3] <- "Inc"
values_df_long$trend[values_df_long$trend == 4] <- "Inc"
values_df_long$trend[values_df_long$trend == -1] <- "Dec"
values_df_long$trend[values_df_long$trend == -2] <- "Dec"
values_df_long$trend[values_df_long$trend == -3] <- "Dec"
values_df_long$trend[values_df_long$trend == -4] <- "Dec"
values_df_long$trend[values_df_long$trend == 0] <- "Stable"

unique(values_df_long$trend)

# Here is where you can change the number of cells you are considering
sampled_values <- sample(unique(values_df_long$raster_cell), 50000, replace = FALSE)

values_df_long <- values_df_long %>% filter(raster_cell %in% sampled_values)


# Creating columns for jags model

trend <- c("Inc","Stable","Dec")
trendID_jags <- c(1:length(trend))

trendkey <- as.data.frame(cbind(trend,trendID_jags))

raster_cell <- sort(c(unique(values_df_long$raster_cell)))
rasterID_jags <- c(1:length(raster_cell))

rasterkey <- as.data.frame(cbind(raster_cell,rasterID_jags))

grid_cell <- sort(c(unique(values_df_long$grid_cell)))
gridID_jags <- c(1:length(grid_cell))

gridkey <- as.data.frame(cbind(grid_cell,gridID_jags))

state <- sort(c(unique(values_df_long$state)))
stateID_jags <- c(1:length(state))

statekey <- as.data.frame(cbind(state,stateID_jags))

change_class <- sort(c(unique(values_df_long$change_class)))
change_classID_jags <- c(1:length(change_class))
change_end_year <- c(1970,1980,2010)

changekey <- as.data.frame(cbind(change_class,change_classID_jags,change_end_year))

start_val <- sort(c(unique(values_df_long$start_val)))
startID_jags <- c(1:length(start_val))

startkey <- as.data.frame(cbind(start_val,startID_jags))

values_df_long_Jags <- values_df_long %>% left_join(rasterkey, by = "raster_cell") %>%
  left_join(gridkey, by = "grid_cell") %>%
  left_join(statekey, by = "state") %>%
  left_join(changekey, by = "change_class") %>%
  left_join(trendkey, by = "trend") %>%
  left_join(startkey, by = "start_val")

names(values_df_long_Jags)
str(values_df_long_Jags)

values_df_long_Jags <- values_df_long_Jags %>%
  mutate_at(c('rasterID_jags','gridID_jags', 'stateID_jags', 'change_classID_jags','trendID_jags','startID_jags','change_end_year','start_val'), as.numeric)

str(values_df_long_Jags)

data <- values_df_long_Jags

# Creating some scaled numeric variables
data$start_val_s <- scale(data$start_val)
data$year_end_s <- scale(data$change_end_year)

write.csv(data,"./models/multinomial_jagsUI/Data_for_jags.csv", row.names = FALSE)


# Prepare the data for model
dfj_t <- PrepDataForJAGS_OHV()


# Select the model to run

# bugsname <- WriteJAGS_changecat_starts_gridRE()

# bugsname <- WriteJAGS_changecat_starts_rasterRE()

bugsname <- WriteJAGS_changecat_starts_rasterRE_gridRE()

# Specify values for model run
# nc <- 3; na <- 1000; ni <- 15000; nb <- 2000; nt <- 10 
# nc <- 3; na <- 500; ni <- 7500; nb <- 1000; nt <- 5
nc <- 3; na <- 100; ni <- 1500; nb <- 200; nt <- 5

# Run the model
system.time(
  mod_ohv <- jagsUI(
    data = dfj_t$data.for.bugs,
    inits = dfj_t$init.bugs,
    parameters.to.save = dfj_t$tomonitor,
    model.file = bugsname,
    n.chains = nc,
    n.adapt = na,
    n.iter = ni,
    n.burnin = nb,
    n.thin = nt,
    parallel = TRUE,
    verbose = TRUE
  )
)

# Examine the results
print(mod_ohv)

# Save the telemetry data object and model results
saveRDS(dfj_t, "./models/multinomial_jagsUI/output/data_start.s_change_cat.RDS")
saveRDS(mod_ohv, "./models/multinomial_jagsUI/output/model_start.s_change_cat.RDS")
