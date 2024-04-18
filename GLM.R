rm(list=ls())
library(tidyverse)
library(sf)
library(spgwr)
library(grid)
library(gridExtra)


# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(digits=4, scipen=999)

# Reading in the dataframe with cell values
values_df <- st_read("./other_data/master/master_cells.shp")

# values_df_cleaned <- st_read("./other_data/master/master_cells_cleaned.shp")
# 
# values_df <- values_df_cleaned
# head(values_df)

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

# values_df_long$value[values_df_long$value == 4] <- 3

# Seeing how many cells comprise different % of data
nrow(values_df)*.1

# Randomly sampling cell IDs
sampled_values <- sample(unique(values_df_long$rstr_cl), 1095424, replace = FALSE) # 10% of the data

# Subsetting the data to those randomly sampled IDs
values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)

# Creating columns for year scaled and grid cell x year (factor)
values_df_sub$year <- as.numeric(values_df_sub$year)
values_df_sub$year_s <- scale(values_df_sub$year)

values_df_sub$year_sc <- as.numeric(values_df_sub$year_s[,1])

values_df_sub$value_s <- scale(values_df_sub$value)
values_df_sub$year <- as.factor(values_df_sub$year)
values_df_sub$state <- as.factor(values_df_sub$state)
values_df_sub$grd_cll_yr <- paste0(values_df_sub$grd_cll,"_",values_df_sub$year)
values_df_sub$grd_cll_yr <- as.factor(values_df_sub$grd_cll_yr)
values_df_sub$rstr_cl <- as.factor(values_df_sub$rstr_cl)

library(lme4)
# control <- glmerControl(optimizer = c("bobyqa", "Nelder_Mead"), optCtrl = list(maxfun = 2000, maxIter = 1000))
model <- glmer(value ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr), data = values_df_sub, family = "poisson",control = glmerControl(optCtrl = list(maxfun = 4000)))
summary(model)

# Residuals
resids_year_s <- residuals(model)

# Adding residuals to the dataframe
values_df_sub <- values_df_sub %>% cbind(resids_year_s)

library(ggspatial)

dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

# Breaking up the dataframe by year
values_df_sub_year <-split(values_df_sub, values_df_sub$year)

# Plotting the residuals in space by year
ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub_year[[1]], aes(col = resids_year_s),size = 1)

ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub_year[[2]], aes(col = resids_year_s),size = 1)

ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub_year[[3]], aes(col = resids_year_s),size = 1)

ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub_year[[4]], aes(col = resids_year_s),size = 1)


library(ggeffects)

years_fake <- seq(min(values_df_sub$year_s), max(values_df_sub$year_s), length.out = 10)

pred_dat <- as.data.frame(ggpredict(model, term = c("year_s[-1.29849, -1.02384, -0.74918, -0.47453, -0.19987,  0.07478,  0.34944,  0.62409,  0.89875,  1.17340]"),type = "re"))

pred_dat$year_unscale <- pred_dat$x*attr(values_df_sub$year_s, 'scaled:scale') + attr(values_df_sub$year_s, 'scaled:center')
pred_dat$pred_e <- exp(pred_dat$predicted)
pred_dat$pred_e_conflow <- exp(pred_dat$conf.low)
pred_dat$pred_e_confhigh <- exp(pred_dat$conf.high)

ggplot(data = pred_dat, aes(x = year_unscale, y = pred_e)) 
  geom_ribbon(aes(ymin = pred_e_conflow, ymax = pred_e_confhigh), linetype = "dashed", alpha = 0.1)+ 
  geom_line(aes(y =pred_e), size = 0.75) +
  xlab("Decade")+ylab("Predicted OHV density value") + theme_minimal()
  
  
# 0 inflated poisson model
install.packages("Matrix_1.6-5.tar.gz", repos = NULL, type = "source")
install.packages('TMB', type = 'source')
library(glmmTMB)
library(bbmle) ## for AICtab
library(ggplot2)
  
## cosmetic
theme_set(theme_bw()+
            theme(panel.spacing=grid::unit(0,"lines")))



fit_zipoisson <- glmmTMB(value ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr),
                         data=values_df_sub,
                         ziformula=~1,
                         family=poisson)

