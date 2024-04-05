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

# Find the change between each decade time step, and the starting value
values_df <- values_df %>% mutate(C1 = paste0(V80 - V70,V70),
                                  C2 = paste0(V10 - V80,V80),
                                  C3 = paste0(V20 - V10,V10))

# Pivot dataframe to be in the long direction
values_df_long <- values_df[,c(5:10)] %>%
  pivot_longer(
    cols = c("C1","C2","C3"),
    names_to = "Change_Class",
    values_to = "Trend"
  )


# Separate the change value and the starting value
values_df_long$Start <- str_sub(as.character(values_df_long$Trend),-1,-1)
values_df_long$Trend <- substr(values_df_long$Trend, 1, nchar(values_df_long$Trend) - 1)

values_df_long$Trend[values_df_long$Trend == "NA"] <- NA
values_df_long$Trend[values_df_long$Trend == "NaN"] <- NA
values_df_long$Trend[values_df_long$Trend == "NAN"] <- NA

# Remove rows where the change was NA
values_df_long <- values_df_long[complete.cases(values_df_long$Trend), ]

values_df_long$Trend <- as.numeric(values_df_long$Trend)

# Making change values multinomial with 3 values
values_df_long$Trend[values_df_long$Trend == 1] <- "Inc"
values_df_long$Trend[values_df_long$Trend == 2] <- "Inc"
values_df_long$Trend[values_df_long$Trend == 3] <- "Inc"
values_df_long$Trend[values_df_long$Trend == 4] <- "Inc"
values_df_long$Trend[values_df_long$Trend == -1] <- "Dec"
values_df_long$Trend[values_df_long$Trend == -2] <- "Dec"
values_df_long$Trend[values_df_long$Trend == -3] <- "Dec"
values_df_long$Trend[values_df_long$Trend == -4] <- "Dec"
values_df_long$Trend[values_df_long$Trend == 0] <- "Stable"

unique(values_df_long$Trend)

# Here is where you can change the number of cells you are considering
sampled_values <- sample(unique(values_df_long$raster_cell), 500000, replace = FALSE)

values_df_long <- values_df_long %>% filter(raster_cell %in% sampled_values)


# Creating columns for jags model

Trend <- c("Inc","Stable","Dec")
TrendID_jags <- c(1:length(Trend))

trendkey <- as.data.frame(cbind(Trend,TrendID_jags))

grid_cell <- sort(c(unique(values_df_long$grid_cell)))
gridID_jags <- c(1:length(grid_cell))

gridkey <- as.data.frame(cbind(grid_cell,gridID_jags))

state <- sort(c(unique(values_df_long$state)))
stateID_jags <- c(1:length(state))

statekey <- as.data.frame(cbind(state,stateID_jags))

Change_Class <- sort(c(unique(values_df_long$Change_Class)))
Change_ClassID_jags <- c(1:length(Change_Class))
Change_year <- c(1970,1980,2010)

changekey <- as.data.frame(cbind(Change_Class,Change_ClassID_jags,Change_year))

Start <- sort(c(unique(values_df_long$Start)))
StartID_jags <- c(1:length(Start))

startkey <- as.data.frame(cbind(Start,StartID_jags))



values_df_long_Jags <- values_df_long %>% left_join(gridkey, by = "grid_cell") %>%
  left_join(statekey, by = "state") %>%
  left_join(changekey, by = "Change_Class") %>%
  left_join(trendkey, by = "Trend") %>%
  left_join(startkey, by = "Start")

names(values_df_long_Jags)
str(values_df_long_Jags)

values_df_long_Jags <- values_df_long_Jags %>%
  mutate_at(c('gridID_jags', 'stateID_jags', 'Change_ClassID_jags','TrendID_jags','StartID_jags',"Change_year","Start"), as.numeric)

str(values_df_long_Jags)

data <- values_df_long_Jags

# Creating some scaled numeric variables
data$Start_Scale <- scale(data$Start)
data$Year_Scale <- scale(data$Change_year)

write.csv(data,"./models/multinomial_jagsUI/Data_for_jags.csv", row.names = FALSE)


# Prepare the data for model
dfj_t <- PrepDataForJAGS_OHV()


# Select the model to run
bugsname <- WriteJAGS_change.cat_start.s()



# Specify values for model run
nc <- 3; na <- 1000; ni <- 15000; nb <- 2000; nt <- 10 

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
saveRDS(dfj_t, "./models/multinomial_jagsUI/output/results/data_start.s_change_cat.RDS")
saveRDS(mod_ohv, "./models/multinomial_jagsUI/output/model_start.s_change_cat.RDS")
