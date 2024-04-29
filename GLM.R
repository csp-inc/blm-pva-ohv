rm(list=ls())
library(tidyverse)
library(sf)
library(spgwr)
library(grid)
library(gridExtra)
# install.packages("Matrix" ,type = "source")
# install.packages("lme4",type = "source")
library(Matrix)
library(lme4)


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


# Seeing how many cells comprise different % of data
nrow(values_df)*.2

# Randomly sampling cell IDs
sampled_values <- sample(unique(values_df_long$rstr_cl), 2190848, replace = FALSE) # 10% of the data

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

# poisson GLMM
model <- glmer(value ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr), data = values_df_sub, family = "poisson",control = glmerControl(optCtrl = list(maxfun = 4000)))
summary(model)


# https://rdrr.io/cran/lme4/man/glmer.nb.html
library(MASS)

# negative binomial GLMM
model_nb <- glmer.nb(value ~ year_sc + (1 | rstr_cl) + (1| grd_cll_yr), data = values_df_sub,control = glmerControl(optCtrl = list(maxfun = 4000)))
summary(model_nb)


# # Residuals
# resids_year_s <- residuals(model)
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
library(ggeffects)

years_fake <- seq(min(values_df_sub$year_sc), max(values_df_sub$year_sc), length.out = 10)

pred_dat <- as.data.frame(ggpredict(model, term = c("year_sc[-1.29849, -1.02384, -0.74918, -0.47453, -0.19987,  0.07478,  0.34944,  0.62409,  0.89875,  1.17340]"),type = "re"))

pred_dat$year_unscale <- pred_dat$x*attr(values_df_sub$year_s, 'scaled:scale') + attr(values_df_sub$year_s, 'scaled:center')
pred_dat$pred_e <- exp(pred_dat$predicted)
pred_dat$pred_e_conflow <- exp(pred_dat$conf.low)
pred_dat$pred_e_confhigh <- exp(pred_dat$conf.high)

ggplot(data = pred_dat, aes(x = year_unscale, y = pred_e))
  geom_ribbon(aes(ymin = pred_e_conflow, ymax = pred_e_confhigh), linetype = "dashed", alpha = 0.1)+
  geom_line(aes(y =pred_e), size = 0.75) +
  xlab("Decade")+ylab("Predicted OHV density value") + theme_minimal()

  
# https://fukamilab.github.io/BIO202/04-C-zero-data.html
library(pscl)
# This package cannot handle random effects
# If I use this I need to filter the data so raster cells are not repeated
  
  
year_list_values_df <- split(values_df_long, values_df_long$year)

cells <- unique(values_df_long$rstr_cl)
  
sampled_values_1 <- sample(cells, length(cells)*.25, replace = FALSE)

cells2 <- cells[!cells %in% sampled_values_1]
  
sampled_values_2 <- sample(cells2, length(cells)*.25, replace = FALSE)

cells3 <- cells2[!cells2 %in% sampled_values_2]

sampled_values_3 <- sample(cells3, length(cells)*.25, replace = FALSE)

cells4 <- cells3[!cells3 %in% sampled_values_2]

sampled_values_4 <- sample(cells4, length(cells)*.25, replace = FALSE)

  
year_samples <- list()
year_samples[[1]] <- sampled_values_1
year_samples[[2]] <- sampled_values_2
year_samples[[3]] <- sampled_values_3
year_samples[[4]] <- sampled_values_4

for(i in 1:4){
  year_list_values_df[[i]] <- year_list_values_df[[i]] %>% filter(rstr_cl %in% year_samples[[i]])
}

values_df_sub2 <- bind_rows(year_list_values_df)

values_df_sub2$year <- as.numeric(values_df_sub2$year)
values_df_sub2$year_s <- scale(values_df_sub2$year)
values_df_sub2$year_sc <- as.numeric(values_df_sub2$year_s[,1])

# 0-inflated poisson GLM
model_ZIM <- zeroinfl(value ~ year_sc | ## Predictor for the Poisson process
                        year_sc, ## Predictor for the Bernoulli process;
                 dist = 'poisson',
                 data = values_df_sub2)
  
summary(model_ZIM)
saveRDS(model_ZIM,"./models/GLM/pscl_zip.RDS")

# Dispersion statistic
E2 <- resid(model_ZIM, type = "pearson")
N  <- nrow(values_df_sub2)
p  <- length(coef(model_ZIM))  
sum(E2^2) / (N - p)
  
# 0-inflated negative binomial GLM
model_ZIM_bin <- zeroinfl(value ~ year_sc |
                            year_sc,
               dist = 'negbin',
               data = values_df_sub2)
summary(model_ZIM_bin)
saveRDS(model_ZIM_bin,"./models/GLM/pscl_nb.RDS")
  
E2 <- resid(model_ZIM_bin, type = "pearson")
N  <- nrow(values_df_sub2)
p  <- length(coef(model_ZIM_bin)) + 1 # '+1' is due to theta
sum(E2^2) / (N - p)

library(lmtest)
lrtest(model_ZIM, model_ZIM_bin)


# # https://rdrr.io/github/nyiuab/NBZIMM/f/vignettes/zinbmms.Rmd
# # install.packages("remotes")
# # remotes::install_github("nyiuab/NBZIMM")
# library(NBZIMM)
# 
# # 0 inflated negative binomial GLMM
# model_NBZIMM <- glmm.zinb(fixed = value ~ year_sc, 
#               random = ~ 1 | rstr_cl, data = values_df_sub, zi_fixed = ~1) 
# 
# summary(f)
# summary(f$zi.fit)
# 
# # Not getting convergence




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
