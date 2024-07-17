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

if(dir.exists("./models") == FALSE){dir.create("./models")}
if(dir.exists("./models/multinomial_jagsUI") == FALSE){dir.create("./models/multinomial_jagsUI")}
if(dir.exists("./models/multinomial_jagsUI/output") == FALSE){dir.create("./models/multinomial_jagsUI/output")}


# Load the functions used to process telemetry and estimate survival
source("./multi_jags_functions.R")


# Read in dataframe
values_df <- read.csv("./other_data/master/master_cells_cleaned3.csv")
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

# nrow(values_df)*.0001

# Here is where you can change the number of cells you are considering
sampled_values <- sample(unique(values_df_long$raster_cell), nrow(values_df)*.001, replace = FALSE)

values_df_long <- values_df_long %>% filter(raster_cell %in% sampled_values)


# Creating columns for jags model

trend <- c("Inc","Stable","Dec")
trendID_jags <- c(1:length(trend))

trendkey <- as.data.frame(cbind(trend,trendID_jags))

raster_cell <- sort(c(unique(values_df_long$raster_cell)))
rasterID_jags <- c(1:length(raster_cell))

rasterkey <- as.data.frame(cbind(raster_cell,rasterID_jags))

values_df_long$grid_cell <- paste0(values_df_long$grid_cell,values_df_long$change_class)
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
# dfj_t <- PrepDataForJAGS_multi_OHV()

# dfj_t <- PrepDataForJAGS_multi_OHV_cat()

dfj_t <- PrepDataForJAGS_multi_OHV_cat2()

# Select the model to run

# bugsname <- WriteJAGS_changecat_starts_gridRE()

# bugsname <- WriteJAGS_changecat_starts_rasterRE()

# bugsname <- WriteJAGS_changecat_starts_rasterRE_gridRE()

# bugsname <- WriteJAGS_changexstart_rasterRE_gridRE()

bugsname <- WriteJAGS_change_start_rasterRE_gridRE()

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

whiskerplot(mod_ohv,"alphaA")
whiskerplot(mod_ohv,"betaB")
whiskerplot(mod_ohv,"gammaC")

whiskerplot(mod_ohv,"alpha")
whiskerplot(mod_ohv,"beta")
whiskerplot(mod_ohv,"gamma")

whiskerplot(mod_ohv,"a")
whiskerplot(mod_ohv,"b")
whiskerplot(mod_ohv,"c")

means <- as.data.frame(mod_ohv$mean[10:21])
means<- as.data.frame(t(means))
# means<- as.data.frame(means[13:24,1])
colnames(means) <- "mean"
means$start <- rep(c("None","Low","Medium","High"),3)
means$change <- c("1970-1980","1970-1980","1970-1980","1970-1980",
                  "1980-2010","1980-2010","1980-2010","1980-2010",
                  "2010-2020","2010-2020","2010-2020","2010-2020")

LCI <- as.data.frame(mod_ohv$q2.5[10:21])
LCI<- as.data.frame(t(LCI))
# LCI<- as.data.frame(LCI[13:24,1])
colnames(LCI) <- "conf.lower"

UCI <- as.data.frame(mod_ohv$q97.5[10:21])
UCI<- as.data.frame(t(UCI))
# UCI <- as.data.frame(UCI[13:24,1])
colnames(UCI) <- "conf.upper"

# colnames(means_exp) <- c("1970-1980","1980-2010","2010-2020")
# means_exp$start <- rep(c("none","low","medium","high"),3)
# 
# means_exp_long <- means_exp %>% 
#   pivot_longer(
#     cols = c("1970-1980","1980-2010","2010-2020"), 
#     names_to = "years",
#     values_to = "coeff"
#   )
# 
# 
# colnames(LCI_exp) <- c("1970-1980","1980-2010","2010-2020")
# LCI_exp$start <- c("none","low","medium","high")
# 
# LCI_exp_long <- LCI_exp %>% 
#   pivot_longer(
#     cols = c("1970-1980","1980-2010","2010-2020"), 
#     names_to = "years",
#     values_to = "LCI"
#   )
# 
# colnames(UCI_exp) <- c("1970-1980","1980-2010","2010-2020")
# UCI_exp$start <- c("none","low","medium","high")
# 
# UCI_exp_long <- UCI_exp %>% 
#   pivot_longer(
#     cols = c("1970-1980","1980-2010","2010-2020"), 
#     names_to = "years",
#     values_to = "UCI"
#   )

plot_dat <- as.data.frame(cbind(means,LCI,UCI))
plot_dat_probinc <- filter(plot_dat,plot_dat$start != "High")
legend_order <- c("None", "Low", "Medium")
category_colors <- c("None" = "#a69d8b", "Low" = "#fae51e", "Medium" = "darkorange")

pd <- position_dodge(0.1)
Increase <- ggplot(plot_dat_probinc, aes(x=change, y=mean, colour=factor(start, levels = legend_order), group=factor(start,levels = legend_order))) + 
  geom_errorbar(aes(ymin=conf.lower, ymax=conf.upper), colour="black", width=.1, position=pd) +
  # geom_line(position=pd) + 
  xlab("") + ylab("Probability of increase") +
  scale_color_manual(values = category_colors, name = "Starting OHV route \ndensity category") +
  geom_point(position=pd, size=3) +
  theme_classic()

plot(Increase)


means <- as.data.frame(mod_ohv$mean)
means_exp <- as.data.frame(t(exp(means)))
means_exp <- means_exp[1:12,]

LCI <- as.data.frame(mod_ohv$q2.5)
LCI_exp <- as.data.frame(t(exp(LCI)))
LCI_exp <- LCI_exp[1:12,]

UCI <- as.data.frame(mod_ohv$q97.5)
UCI_exp <- as.data.frame(t(exp(UCI)))
UCI_exp <- UCI_exp[1:12,]

colnames(means_exp) <- c("1970-1980","1980-2010","2010-2020")
means_exp$start <- rep(c("None","Low","Medium","High"),3)
means_exp$change <- c("Inc","Inc","Inc","Inc","Stable","Stable","Stable","Stable","Dec","Dec","Dec","Dec")

means_exp_long <- means_exp %>% 
  pivot_longer(
    cols = c("1970-1980","1980-2010","2010-2020"), 
    names_to = "years",
    values_to = "coeff"
  )


colnames(LCI_exp) <- c("1970-1980","1980-2010","2010-2020")
LCI_exp$start <- rep(c("None","Low","Medium","High"),3)
LCI_exp$change <- c("Inc","Inc","Inc","Inc","Stable","Stable","Stable","Stable","Dec","Dec","Dec","Dec")

LCI_exp_long <- LCI_exp %>% 
  pivot_longer(
    cols = c("1970-1980","1980-2010","2010-2020"), 
    names_to = "years",
    values_to = "LCI"
  )

colnames(UCI_exp) <- c("1970-1980","1980-2010","2010-2020")
UCI_exp$start <- rep(c("None","Low","Medium","High"),3)
UCI_exp$change <- c("Inc","Inc","Inc","Inc","Stable","Stable","Stable","Stable","Dec","Dec","Dec","Dec")

UCI_exp_long <- UCI_exp %>% 
  pivot_longer(
    cols = c("1970-1980","1980-2010","2010-2020"), 
    names_to = "years",
    values_to = "UCI"
  )

plot_dat <- as.data.frame(cbind(means_exp_long,LCI_exp_long$LCI,UCI_exp_long$UCI))
colnames(plot_dat) <- c("start","change","years","coeff","LCI","UCI")

plot_dat_eff <- filter(plot_dat,plot_dat$start != "High")
plot_dat_eff <- filter(plot_dat_eff,plot_dat_eff$change == "Inc")
legend_order <- c("None", "Low", "Medium")
category_colors <- c("None" = "#a69d8b", "Low" = "#fae51e", "Medium" = "darkorange")

pd <- position_dodge(0.1)
Effect <- ggplot(plot_dat_eff, aes(x=years, y=coeff, colour=factor(start, levels = legend_order), group=factor(start,levels = legend_order))) + #, shape = factor(change)
  geom_errorbar(aes(ymin=LCI, ymax=UCI), colour="black", width=.1, position=pd) +
  # geom_line(position=pd) + 
  xlab("") + ylab("Effect") +
  scale_color_manual(values = category_colors, name = "Starting OHV route \ndensity category") +
  geom_point(position=pd, size=3) +
  theme_classic()

plot(Effect)



# Save the telemetry data object and model results
saveRDS(dfj_t, "./models/multinomial_jagsUI/output/data_startxchange_cat.RDS")
saveRDS(mod_ohv, "./models/multinomial_jagsUI/output/model_startxchange_cat.RDS")
