rm(list=ls())
library(tidyverse)
library(sf)
# library(sp)
# library(rgdal)
# library(rgeos)
library(spgwr)
library(grid)
library(gridExtra)


# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(digits=4, scipen=999)

# Reading in the dataframe with cell values
values_df <- st_read("./other_data/master/master_cells.shp")

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

# values_df_long$value[values_df_long$value == 4] <- 3

sampled_values <- sample(unique(values_df_long$rstr_cl), 50000, replace = FALSE)

values_df_sub <- values_df_long %>% filter(rstr_cl %in% sampled_values)

values_df_sub$year <- as.numeric(values_df_sub$year)
values_df_sub$year_s <- scale(values_df_sub$year)
values_df_sub$value_s <- scale(values_df_sub$value)
values_df_sub$year <- as.factor(values_df_sub$year)
values_df_sub$state <- as.factor(values_df_sub$state)

library(lme4)
model <- glmer(value ~ year_s + (1 | state), data = values_df_sub, family = "poisson", control = glmerControl(optCtrl = list(maxfun = 400)))
summary(model)
ranef(model)

resids <- residuals(model)

values_df_sub <- values_df_sub %>% cbind(resids)

library(ggspatial)

dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")

ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub, aes(col = resids),size = 1)


# Using ggpredict

library(ggeffects)

pred_dat <- as.data.frame(ggpredict(model, term = c("year_s[all]","state") , type = "re"))

pred_dat$year_unscale <- pred_dat$x*attr(values_df_sub$year_s, 'scaled:scale') + attr(values_df_sub$year_s, 'scaled:center')
pred_dat$pred_e <- exp(pred_dat$predicted)
pred_dat$pred_e_conflow <- exp(pred_dat$conf.low)
pred_dat$pred_e_confhigh <- exp(pred_dat$conf.high)

ggplot(data = pred_dat, aes(x = year_unscale, y = pred_e, color = group)) +
  geom_ribbon(aes(ymin = pred_e_conflow, ymax = pred_e_confhigh, fill = group), linetype = "dashed", alpha = 0.1)+
  scale_fill_manual(values = c("#FF7F24","#EEAD0E","#EE2C2C","#8B6914"), name = "State")+
  geom_line(aes(y =pred_e), size = 0.75) +
    scale_color_manual(values = c("#FF7F24","#EEAD0E","#EE2C2C","#8B6914"),name = "State")+
  xlab("Decade")+ylab("Predicted OHV density value") + theme_minimal()


# GLM model with year as categorical variable

model2 <- glmer(value ~ year + (1 | state), data = values_df_sub, family = "poisson", control = glmerControl(optCtrl = list(maxfun = 400)))
summary(model2)
ranef(model2)



resids2 <- residuals(model2)

values_df_sub_mod2 <- values_df_sub %>% cbind(resids2)

ggplot() +
  annotation_spatial(dt_range) +
  layer_spatial(values_df_sub_mod2, aes(col = resids2),size = .75)


mod2 <- summary(model2)
# Getting the fixed effect coeffifients
coef2 <- as.data.frame(mod2$coefficients)
# Getting the random effect coefficients
ranef2 <- as.data.frame(ranef(model2))

states <- c("AZ","CA","NV","UT")

# Adding values to intercept
for(i in 2:4){
  coef2[i,1] <- coef2[i,1]+coef2[1,1]
}

# Combining effects of fixed and random and getting confidence intervals
effect_list <- list()
for(i in 1:4){
  df <- coef2
  df$year <- c("1970","1980","2010","2020")
  df$state <- states[i]
  df$Estimate <- df$Estimate + ranef2[i,4]
  df$`Std. Error` <- df$`Std. Error`+ ranef2[i,5]
  df$high <- df$Estimate+df$`Std. Error`
  df$low <- df$Estimate-df$`Std. Error`
  effect_list[[i]] <- df
}

effect_df <- bind_rows(effect_list)


ggplot(effect_df, aes(x = year, y = exp(Estimate), ymin = exp(low), ymax = exp(high), color = state)) +
  geom_line() +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 3) +
  geom_errorbar(width = 0.1) +
  labs(x = "Year", y = "Effect") +
  scale_color_manual(values = c("#FF7F24","#EEAD0E","#EE2C2C","#8B6914"), name = "State") +  # Change colors as needed
  theme_minimal()


# Using ggpredict 

pred_dat2 <- as.data.frame(ggpredict(model2, term = c("year","state") , type = "re"))

pred_dat2$pred_e <- exp(pred_dat2$predicted)
pred_dat2$pred_e_conflow <- exp(pred_dat2$conf.low)
pred_dat2$pred_e_confhigh <- exp(pred_dat2$conf.high)


ggplot(pred_dat2, aes(x = x, y = pred_e, ymin = pred_e_conflow, ymax = pred_e_confhigh, color = group)) +
  geom_line() +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 3) +
  geom_errorbar(width = 0.1) +
  labs(x = "Year", y = "Predicted OHV density value") +
  scale_color_manual(values = c("#FF7F24","#EEAD0E","#EE2C2C","#8B6914"), name = "State") +  # Change colors as needed
  theme_minimal()

