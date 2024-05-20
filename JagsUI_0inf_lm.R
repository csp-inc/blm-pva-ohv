##### Run a model -----

# Clear memory (if necessary)
rm(list=ls())
options(scipen=999)

## Loading in packages -----
list.of.packages <- c("tidyverse","lubridate","terra","sf","dplyr", "jagsUI",
                      "parallel","ggeffects","tidyr","stringr","pscl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


# Load the functions used to process telemetry and estimate survival
source("./GLMM_jags_functions.R")

# Reading in the dataframe with cell values
# values_df <- st_read("./other_data/master/master_cells.shp")

values_df_cleaned <- st_read("./other_data/master/master_cells_cleaned.shp")
values_df <- values_df_cleaned
head(values_df)

# Choosing columns
names(values_df) <- c("1970","1980","2010","2020","rstr_cl","grd_cll","state","geometry")

# Converting dataframe to long
values_df_long <- values_df %>% 
  pivot_longer(
    cols = c("1970","1980","2010","2020"), 
    names_to = "year",
    values_to = "value"
  )

# Removing rows with no OHV value in that year
values_df_long <- values_df_long[complete.cases(values_df_long$value), ]

values_df_long <- st_read("./other_data/master/master_cells_cleaned_long.shp")


# Seeing how many cells comprise different % of data
# nrow(values_df)*.001

# Randomly sampling cell IDs
sampled_values <- sample(unique(values_df_long$rstr_cl), nrow(values_df)*.0001, replace = FALSE) # 10% of the data

# Subsetting the data to those randomly sampled IDs
values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)

# Creating columns for year scaled 
values_df_sub$year <- as.numeric(values_df_sub$year)
values_df_sub$year_s <- scale(values_df_sub$year)
values_df_sub$year_sc <- as.numeric(values_df_sub$year_s[,1])

# Creating categorical columns for year, state, raster cell and gridcell year
values_df_sub$year <- as.character(values_df_sub$year)
values_df_sub$state <- as.character(values_df_sub$state)
values_df_sub$grd_cll_yr <- paste0(values_df_sub$grd_cll,"_",values_df_sub$year)

values_df_sub$grd_cll_yr <- as.character(values_df_sub$grd_cll_yr)
values_df_sub$rstr_cl <- as.character(values_df_sub$rstr_cl)

# Creating columns for jags
rstr_cl <- sort(c(unique(values_df_sub$rstr_cl)))
rasterID_jags <- c(1:length(rstr_cl))

rasterkey <- as.data.frame(cbind(rstr_cl,rasterID_jags))

grd_cll_yr <- sort(c(unique(values_df_sub$grd_cll_yr)))
gridID_jags <- c(1:length(grd_cll_yr))

gridkey <- as.data.frame(cbind(grd_cll_yr,gridID_jags))

state <- sort(c(unique(values_df_sub$state)))
stateID_jags <- c(1:length(state))

statekey <- as.data.frame(cbind(state,stateID_jags))


values_df_sub_Jags <- values_df_sub %>% left_join(rasterkey, by = "rstr_cl") %>%
  left_join(gridkey, by = "grd_cll_yr") %>%
  left_join(statekey, by = "state") 

names(values_df_sub_Jags)
str(values_df_sub_Jags)

values_df_sub_Jags <- values_df_sub_Jags %>%
  mutate_at(c('rasterID_jags','gridID_jags', 'stateID_jags'), as.numeric)

str(values_df_sub_Jags)

data <- values_df_sub_Jags


if(dir.exists("./models/0inf_jagsUI") == FALSE){dir.create("./models/0inf_jagsUI")}
if(dir.exists("./models/0inf_jagsUI/output") == FALSE){dir.create("./models/0inf_jagsUI/output")}

# st_write(data ,"./models/0inf_jagsUI/Data_for_jags.shp")

data <- st_drop_geometry(data)

write.csv(data ,"./models/0inf_jagsUI/Data_for_jags.csv")


data <- read.csv("./models/0inf_jagsUI/Data_for_jags.csv")

# Checking dispersion of the data
mean_count <- mean(data$value, na.rm = TRUE)
var_count <- var(data$value, na.rm = TRUE)
dispersion_index <- var_count / mean_count
dispersion_index #2.191

hist(data$value)

source("./GLMM_jags_functions.R")

# Prepare the data for model
dfj_t <- PrepDataForJAGS_glm_OHV()

# Select the model to run
bugsname <- WriteJAGS_year_sc_rasterRE_gridRE_zip()
# bugsname <- WriteJAGS_year_sc_rasterRE_gridRE_zinb()


# Specify values for model run
nc <- 3; na <- 1000; ni <- 15000; nb <- 5000; nt <- 10 
# nc <- 3; na <- 500; ni <- 7500; nb <- 1000; nt <- 5
# nc <- 3; na <- 100; ni <- 1500; nb <- 200; nt <- 5

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
# saveRDS(dfj_t, "./models/0inf_jagsUI/output/data_start.s_change_cat.RDS")
saveRDS(mod_ohv, "./models/0inf_jagsUI/output/model.0001zip.RDS")

mod_ohv <- readRDS("./models/0inf_jagsUI/output/model.0001zip.RDS")
print(mod_ohv$summary)

# For every 1 year increase
exp(mod_ohv$mean$beta1 * sd(data$year))
exp(mod_ohv$q2.5$beta1 * sd(data$year))
exp(mod_ohv$q97.5$beta1 * sd(data$year))

exp(mod_ohv$mean$beta1)

mod_001 <- readRDS("./models/0inf_jagsUI/output/model.001zip.RDS")


# mod_01 <- readRDS("./models/0inf_jagsUI/output/model.01zip.RDS")
# print(mod_01)

