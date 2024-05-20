rm(list=ls())

## Loading in packages -----
list.of.packages <- c("tidyverse","spgwr","terra","sf","dplyr", "performance",
                      "grid","ggeffects","MASS","DHARMa","pscl","AICcmodavg","lmtest","gridExtra","Matrix","lme4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(digits=4, scipen=999)

## Getting dataframe ready -----
# Reading in the dataframe with cell values
# values_df <- st_read("./other_data/master/master_cells.shp")

values_df_cleaned <- st_read("./other_data/master/master_cells_cleaned.shp")

values_df <- values_df_cleaned
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

# Seeing how many cells comprise different % of data
# nrow(values_df)*.01

# # Randomly sampling cell IDs
# sampled_values <- sample(unique(values_df_long$rstr_cl), nrow(values_df)*.5, replace = FALSE) # 10% of the data
# 
# # Subsetting the data to those randomly sampled IDs
# values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)

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

summary(MOD_bin)$coefficients
# OR
exp(fixef(MOD_bin)[2])

# For each year that passed, the odds that a pixel contained OHV grew 5.54 times
exp(fixef(MOD_bin)[2]*sd(values_df_sub$year)) #5.796 
exp(0.004504*sd(values_df_sub$year)) 

exp((fixef(MOD_bin)[2]+0.004504)*sd(values_df_sub$year)) #6.253 
exp((fixef(MOD_bin)[2]-0.004504)*sd(values_df_sub$year)) #5.372 


# Making fake data for predictions
years_fake <- seq(min(values_df_sub$year_sc, na.rm = TRUE), max(values_df_sub$year_sc, na.rm = TRUE), length.out = 10)
new_data_test <- data.frame(year_sc = c(-1.33185, -1.04845, -0.76504, -0.48164, -0.19824,  0.08517,  0.36857,  0.65197,  0.93538,  1.21878))
new_data_test$year <- (new_data_test$year_sc * sd(values_df_sub$year)) + mean(values_df_sub$year)

# Making predicted datasets
# Conditioning on random effects
pred_dat_MOD_re <- predict_response(MOD_bin, terms = new_data_test, type = "re")

# Plotting
plot <- ggplot(data = pred_dat_MOD, aes(x = x, y = exp(predicted*sd(values_df_sub$year))))+
  geom_ribbon(aes(ymin = exp(conf.low*sd(values_df_sub$year)), ymax = exp(conf.high*sd(values_df_sub$year))), alpha = 0.1, linetype = "dashed", size = 0.25)+
  geom_line(size = 0.75) + 
  xlab("Year") + ylab("Probability of chip containing OHV routes") + 
  theme_minimal()

plot


# # Residuals
# resids_year_s <- residuals(MOD)
# 
# # Adding residuals to the dataframe
# values_df_sub <- values_df_sub %>% cbind(resids_year_s)
# 
# library(ggspatial)
# 
# dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")
# 
# # Breaking up the dataframe by year
# values_df_sub_year <-split(values_df_sub, values_df_sub$year)
# 
# # Plotting the residuals in space by year
# ggplot() +
#   annotation_spatial(dt_range) +
#   layer_spatial(values_df_sub_year[[1]], aes(col = resids_year_s),size = 1)
# 
# ggplot() +
#   annotation_spatial(dt_range) +
#   layer_spatial(values_df_sub_year[[2]], aes(col = resids_year_s),size = 1)
# 
# ggplot() +
#   annotation_spatial(dt_range) +
#   layer_spatial(values_df_sub_year[[3]], aes(col = resids_year_s),size = 1)
# 
# ggplot() +
#   annotation_spatial(dt_range) +
#   layer_spatial(values_df_sub_year[[4]], aes(col = resids_year_s),size = 1)
# 
# 

### Geographically weighted regression -----
library(GWmodel)

values_sp <- as_Spatial(values_df_sub) # 10% of the data

band <- gwr.sel(value_bin ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr), data = values_sp, family = binomial(link = "logit"), adapt = T)

gwr.model = gwr.mixed(value_bin ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr),
                data = values_sp,
                adapt=band,
                hatmatrix=TRUE,
                se.fit=TRUE) 


# https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf

# # install.packages("TMB")
# # install.packages("Matrix")
# # install.packages("glmmTMB")
# 
# # library(glmmTMB)
# # library(bbmle) 
# # library(ggplot2)

# This package is not working due to package version incompatbilities between Matrix and TMB


# # ## cosmetic
# # theme_set(theme_bw()+
# #             theme(panel.spacing=grid::unit(0,"lines")))
# # 
# # 
# # 
# # fit_zipoisson <- glmmTMB(value ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr),
# #                          data=values_df_sub,
# #                          ziformula=~1,
# #                          family=poisson)
# # 
# # fit_nbinom <- glmmTMB(value ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr),
# #                          data=values_df_sub,
# #                          ziformula=~1,
# #                          family=nbinom2)
