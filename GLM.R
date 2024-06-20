rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","spgwr","terra","sf","dplyr", "performance",
                      "grid","ggeffects","MASS","DHARMa","pscl","AICcmodavg","lmtest","gridExtra","Matrix","lme4",
                      "furrr","future","foreach","tictoc","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(digits=4, scipen=999)

## Getting dataframe ready -----
# Reading in the dataframe with cell values
# values_df <- st_read("./other_data/master/master_cells.shp")

values_df <- st_read("./other_data/master/master_cells_cleaned3.shp")

head(values_df)

# Choosing columns
names(values_df) <- c("1979","1989","2012","2022","rstr_cl","grd_cll","state","geometry")

# Converting dataframe to long
values_df_long <- values_df %>% 
  pivot_longer(
    cols = c("1979","1989","2012","2022"), 
    names_to = "year",
    values_to = "value"
  )

# Removing rows with no OHV value in that year
values_df_long <- values_df_long[complete.cases(values_df_long$value), ]

st_write(values_df_long,"./other_data/master/master_cells_cleaned_long.shp",append=FALSE)
values_df_long <- st_read("./other_data/master/master_cells_cleaned_long.shp")


values_df_long <- values_df_long %>% mutate(value_bin = case_when(
  value == 0 ~ 0,
  value == 1 ~ 1,
  value == 2 ~ 1,
  value == 4 ~ 1
))

# # IF you want to subset the data
# # Seeing how many cells comprise different % of data
# # nrow(values_df)*.01
# 
# # Randomly sampling cell IDs
# sampled_values <- sample(unique(values_df_long$rstr_cl), nrow(values_df)*.01, replace = FALSE) # 10% of the data
# 
# # Subsetting the data to those randomly sampled IDs
# values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)

# # If you want to use all the data
values_df_sub <- values_df_long

# Creating columns for year scaled and grid cell x year (factor)
values_df_sub$year <- as.numeric(values_df_sub$year)
values_df_sub$year_s <- scale(values_df_sub$year)

values_df_sub$year_sc <- as.numeric(values_df_sub$year_s[,1])

values_df_sub$value_s <- scale(values_df_sub$value)
values_df_sub$year_f <- as.factor(values_df_sub$year)
values_df_sub$state <- as.factor(values_df_sub$state)
values_df_sub$grd_cll_yr <- paste0(values_df_sub$grd_cll,"_",values_df_sub$year_f)
values_df_sub$grd_cll_yr <- as.factor(values_df_sub$grd_cll_yr)
values_df_sub$rstr_cl <- as.factor(values_df_sub$rstr_cl)

values_df_sub <- values_df_sub %>% mutate(nyrs = case_when(
  year == "1979" | year == "1989" ~ 10,
  year == "2012" ~ 3,
  year == "2022" ~ 4
))


## binomial model ----
# https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/

MOD_bin <- glmer(value_bin ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr), data = values_df_sub, family = binomial(link = "logit"),control = glmerControl(optimizer = "Nelder_Mead"))

saveRDS(MOD_bin,"./models/GLM/mod_100_nm.RDS")

MOD_bin <- readRDS("./models/GLM/mod_100_nm.RDS")

# Confirming convergence warning is false positive
# https://joshua-nugent.github.io/allFit/
mod_fits <- allFit(MOD_bin)

diff_optims_OK <- mod_fits[sapply(mod_fits, is, "merMod")]
lapply(diff_optims_OK, function(x) x@optinfo$conv$lme4$messages)

convergence_results <- lapply(diff_optims_OK, function(x) x@optinfo$conv$lme4$messages)
working_indices <- sapply(convergence_results, is.null)
if(sum(working_indices) == 0){
  print("No algorithms from allFit converged. You may still be able to use the results, but proceed with extreme caution.")
  first_fit <- NULL
} else {
  first_fit <- mod_fits[working_indices][[1]]
}
first_fit

summary(MOD_bin)

# Examining effect of year
summary(MOD_bin)$coefficients
# OR (exponentiated coefficient)
exp(fixef(MOD_bin)[2])
# For each year that passed, the odds that a pixel contained OHV grew 5.8 times
exp(fixef(MOD_bin)[2]*sd(values_df_sub$year)) #5.796 
# Finding 95% CI
exp((fixef(MOD_bin)[2]+0.004504)*sd(values_df_sub$year)) #6.253 
exp((fixef(MOD_bin)[2]-0.004504)*sd(values_df_sub$year)) #5.372 


# OR (exponentiated coefficient)
exp(fixef(MOD_bin)[1])
# For each year that passed, the odds that a pixel contained OHV grew 5.8 times
exp(fixef(MOD_bin)[1]*sd(values_df_sub$year)) #5.796 
# Finding 95% CI
exp((fixef(MOD_bin)[1]+0.004504)*sd(values_df_sub$year)) #6.253 
exp((fixef(MOD_bin)[1]-0.004504)*sd(values_df_sub$year)) #5.372 


# # Making fake data for predictions
# years_fake <- seq(min(values_df_sub$year_sc, na.rm = TRUE), max(values_df_sub$year_sc, na.rm = TRUE), length.out = 10)
# new_data_test <- data.frame(year_sc = c(-1.33185, -1.04845, -0.76504, -0.48164, -0.19824,  0.08517,  0.36857,  0.65197,  0.93538,  1.21878))
# 
# # Making predicted datasets
# # Conditioning on fixed effects 
# pred_dat_MOD <- predict_response(MOD_bin, terms = new_data_test, type = "fe")
# 
# new_data_test$year <- (new_data_test$year_sc * sd(values_df_sub$year)) + mean(values_df_sub$year)
# 
# 
# # Plotting
# plot <- ggplot(data = pred_dat_MOD, aes(x = x, y = exp(predicted*sd(values_df_sub$year))))+
#   geom_ribbon(aes(ymin = exp(conf.low*sd(values_df_sub$year)), ymax = exp(conf.high*sd(values_df_sub$year))), alpha = 0.1, linetype = "dashed", size = 0.25)+
#   geom_line(size = 0.75) + 
#   xlab("Year") + ylab("Probability of chip containing OHV routes") + 
#   theme_minimal()
# 
# plot

## Running model iterations on smaller subsample of the data
# values_df_long <- st_read("./other_data/master/master_cells_cleaned_long.shp")
# 
# values_df_long <- values_df_long %>% mutate(value_bin = case_when(
#   value == 0 ~ 0,
#   value == 1 ~ 1,
#   value == 2 ~ 1,
#   value == 4 ~ 1
# ))
# 
# for(i in 1:50){
#   # values_df_long <- st_read("./other_data/master/master_cells_cleaned_long.shp")
#   sd_csv <- read.csv("./models/GLM/model_info.csv", header = TRUE)
#   
#   # values_df_long <- values_df_long %>% mutate(value_bin = case_when(
#   #   value == 0 ~ 0,
#   #   value == 1 ~ 1,
#   #   value == 2 ~ 1,
#   #   value == 4 ~ 1
#   # ))
#   
#   
#   # Randomly sampling cell IDs
#   sampled_values <- sample(unique(values_df_long$rstr_cl), 10451143 *.01, replace = FALSE) # 10% of the data
#   
#   # Subsetting the data to those randomly sampled IDs
#   values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)
#   
#   # Creating columns for year scaled and grid cell x year (factor)
#   values_df_sub$year <- as.numeric(values_df_sub$year)
#   values_df_sub$year_s <- scale(values_df_sub$year)
#   
#   values_df_sub$year_sc <- as.numeric(values_df_sub$year_s[,1])
#   
#   values_df_sub$value_s <- scale(values_df_sub$value)
#   values_df_sub$year_f <- as.factor(values_df_sub$year)
#   values_df_sub$state <- as.factor(values_df_sub$state)
#   values_df_sub$grd_cll_yr <- paste0(values_df_sub$grd_cll,"_",values_df_sub$year_f)
#   values_df_sub$grd_cll_yr <- as.factor(values_df_sub$grd_cll_yr)
#   values_df_sub$rstr_cl <- as.factor(values_df_sub$rstr_cl)
#   
#   MOD_bin <- glmer(value_bin ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr), data = values_df_sub, family = binomial(link = "logit"))
#   
#   sd_csv[i,1] <- i
#   sd_csv[i,2] <- sd(values_df_sub$year)
#   sd_csv[i,3] <- mean(values_df_sub$year)
#   
#   write.csv(sd_csv,"./models/GLM/model_info.csv",row.names = FALSE)
#   saveRDS(MOD_bin,paste0("./models/GLM/mod_10_",i,".RDS"))
# }
# 
# 
# sd_csv <- read.csv("./models/GLM/model_info.csv", header = TRUE)
# 
