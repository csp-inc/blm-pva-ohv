# This script creates functions to run various multinomial models for OHV

# Data prep functions
PrepDataForJAGS_multi_OHV <- function(){
  
  # Load in the saved dataframe
  data <- read.csv("./models/multinomial_jagsUI/Data_for_jags.csv")
  
  ### Create objects that can be used for predict in the model
  
  # Categorical
  N<-nrow(data)
  TREND <- data$trendID_jags
  raster <- data$rasterID_jags
  grid <- data$gridID_jags
  state <- data$stateID_jags
  change_class <- data$change_classID_jags
  start_val <- data$startID_jags
  
  # Matirx of responses
  trend<-matrix(data = 0, nrow = N, ncol = 3)
  for(i in 1:N){
    for(j in 1:3){
      if(isTRUE(TREND[i] == "1")) {trend[i,1]<-1}  
      if(isTRUE(TREND[i] == "2")) {trend[i,2]<-1}
      if(isTRUE(TREND[i] == "3")) {trend[i,3]<-1}
    }
  }
  
  # Numeric
  start_val_num <- data$start_val
  year_end <- data$change_end_year
  
  # Numeric scaled
  start_val_num_s <-data$start_val_s
  year_end_s <- data$year_end_s
  
  # Other data values needed for estimation
  
  nTiles <- length(unique(data$gridID_jags))
  nCells <- length(unique(data$rasterID_jags))
  
  data_C1 <- data %>% filter(change_class == "C1") 
  C1_start_mean <- mean(data_C1$start_val_s)
  
  data_C2 <- data %>% filter(change_class == "C2") 
  C2_start_mean <- mean(data_C2$start_val_s)
  
  data_C3 <- data %>% filter(change_class == "C3") 
  C3_start_mean <- mean(data_C3$start_val_s)
  
  # Predictions
  # Create range of values for in-modeling predictions, both scaled and not scaled
  year_predict <- as.numeric(seq(min(year_end_s), max(year_end_s), length.out = 50))
  start_predict <- as.numeric(seq(min(start_val_num_s), max(start_val_num_s), length.out = 50))
  
  year_predict_ns <- as.numeric(seq(min(year_end), max(year_end), length.out = 50))
  start_predict_ns <- as.numeric(seq(min(start_val_num), max(start_val_num), length.out = 50))
  
  
  
  ##### Prep data for jags (dfj) -----
  dfj_t <- list()
  
  # Save the data for JAGS
  dfj_t$data.for.bugs <- list(N = N,
                              # Categorical
                              trend = trend,
                              raster = raster,
                              grid = grid,
                              state = state,
                              change_class = change_class,
                              start_val = start_val,
                              # Numerical
                              start_val_num_s = start_val_num_s,
                              year_end_s = year_end_s,
                              # Other data
                              nCells = nCells,
                              nTiles = nTiles,
                              C1_start_mean = C1_start_mean,
                              C2_start_mean = C2_start_mean,
                              C3_start_mean = C3_start_mean,
                              # Prediction dataframes for covariates (scaled)
                              start_predict = start_predict,
                              year_predict = year_predict
                              
                              
  )
  
  # Save unscaled prediction objects
  dfj_t$predict <- list(start_predict_ns = start_predict_ns,
                        year_predict_ns = year_predict_ns)
  
  # Save raw data for covariates
  dfj_t$raw_covs <- list()
  
  # Specifiy initial values
  dfj_t$init.bugs <- function(){list(alpha=runif(1,-0.01,0.01),
                                     beta=runif(1,-0.01,0.01),
                                     gamma=runif(1,-0.01,0.01),
                                     alphaA=runif(3,-0.01,0.01),
                                     betaB=runif(3,-0.01,0.01),
                                     gammaC=runif(3,-0.01,0.01)

  )}
  
  
  # Parameters to monitor
  dfj_t$tomonitor <- c("alpha", "alphaA","beta","betaB", "gamma","gammaC","p1_C1_prob","p1_C2_prob","p1_C3_prob","p2_C1_prob","p2_C2_prob","p2_C3_prob","p3_C1_prob","p3_C2_prob","p3_C3_prob")
  
  return(dfj_t)
  
}



# MODEL functions

# Model 1: RE of grid ID
WriteJAGS_changecat_starts_gridRE <- function(){
  BUGSfilename_mult <- "./models/multinomial_jagsUI/code.OHV.txt"
  cat("
    
    model{
   
     for(i in 1:N){
        
        trend[i,1:3]~dmulti(p[i,1:3],1) #likelihood
        
        #process model
        
        #Increasae
        log(p0[i,1]) <- alphaA[change_class[i]] + alpha * start_val_num_s[i] + gridRE[grid[i]] 
        
        #Stable
        log(p0[i,2]) <- betaB[change_class[i]] + beta * start_val_num_s[i] + gridRE[grid[i]]
        
        #Decrease
        log(p0[i,3]) <- gammaC[change_class[i]] + gamma * start_val_num_s[i] + gridRE[grid[i]]  
        
        for(j in 1:3){
        p[i,j]<-p0[i,j]/sum(p0[i, ])
        }

   }

#derived parameters
log(p1_C1) <- (alpha * C1_start_mean) + alphaA[1]
log(p1_C2) <- (alpha * C2_start_mean) + alphaA[2]
log(p1_C3) <- (alpha * C3_start_mean) + alphaA[3]

log(p2_C1) <- (beta * C1_start_mean) + betaB[1]
log(p2_C2) <- (beta * C2_start_mean) + betaB[2]
log(p2_C3) <- (beta * C3_start_mean) + betaB[3]

log(p3_C1) <- (gamma * C1_start_mean) + gammaC[1]
log(p3_C2) <- (gamma * C2_start_mean) + gammaC[2]
log(p3_C3) <- (gamma * C3_start_mean) + gammaC[3]

# Adding the probabilities for all trends for each change class
sum_p_C1 <- sum(p1_C1, p2_C1, p3_C1)
sum_p_C2 <- sum(p1_C2, p2_C2, p3_C2)
sum_p_C3 <- sum(p1_C3, p2_C3, p3_C3)

# Normalizing probabilities for each trend for each change class
#Increase
p1_C1_prob <- p1_C1/sum_p_C1
p2_C1_prob <- p2_C1/sum_p_C1
p3_C1_prob <- p3_C1/sum_p_C1

#Stable
p1_C2_prob <- p1_C2/sum_p_C2
p2_C2_prob <- p2_C2/sum_p_C2
p3_C2_prob <- p3_C2/sum_p_C2

#Decrease
p1_C3_prob <- p1_C3/sum_p_C3
p2_C3_prob <- p2_C3/sum_p_C3
p3_C3_prob <- p3_C3/sum_p_C3



#priors
alpha~dnorm(0,0.0001)
beta~dnorm(0,0.0001)
gamma~dnorm(0,0.0001)

for(i in 1:3){ ## the fixed effect of start year
  alphaA[i]~dnorm(0,1)
  betaB[i]~dnorm(0,1)
  gammaC[i]~dnorm(0,1)
} 


#priors for RE of grid cell id

gridRE.sd ~ dgamma(0.1, 0.1)
gridRE.prec <- pow(gridRE.sd, -2)
for(i in 1:nTiles){
  gridRE[i] ~ dnorm(0, gridRE.prec)
}

# for(i in 1:nTiles){ 
#   gridRE[i]~dnorm(0,tau) #Random effects
# } 


}",file=BUGSfilename_mult)
  return(BUGSfilename_mult)
}

# Model 2: RE of raster cell ID
WriteJAGS_changecat_starts_rasterRE <- function(){
  BUGSfilename_mult <- "./models/multinomial_jagsUI/code.OHV.txt"
  cat("
    
    model{
   
     for(i in 1:N){
        
        trend[i,1:3]~dmulti(p[i,1:3],1) #likelihood
        
        #process model
        
        #Increasae
        log(p0[i,1]) <- alphaA[change_class[i]] + alpha * start_val_num_s[i] + rasterRE[raster[i]] 
        
        #Stable
        log(p0[i,2]) <- betaB[change_class[i]] + beta * start_val_num_s[i] + rasterRE[raster[i]]
        
        #Decrease
        log(p0[i,3]) <- gammaC[change_class[i]] + gamma * start_val_num_s[i] + rasterRE[raster[i]]  
        
        for(j in 1:3){
        p[i,j]<-p0[i,j]/sum(p0[i, ])
        }

   }

#derived parameters
log(p1_C1) <- (alpha * C1_start_mean) + alphaA[1]
log(p1_C2) <- (alpha * C2_start_mean) + alphaA[2]
log(p1_C3) <- (alpha * C3_start_mean) + alphaA[3]

log(p2_C1) <- (beta * C1_start_mean) + betaB[1]
log(p2_C2) <- (beta * C2_start_mean) + betaB[2]
log(p2_C3) <- (beta * C3_start_mean) + betaB[3]

log(p3_C1) <- (gamma * C1_start_mean) + gammaC[1]
log(p3_C2) <- (gamma * C2_start_mean) + gammaC[2]
log(p3_C3) <- (gamma * C3_start_mean) + gammaC[3]

# Adding the probabilities for all trends for each change class
sum_p_C1 <- sum(p1_C1, p2_C1, p3_C1)
sum_p_C2 <- sum(p1_C2, p2_C2, p3_C2)
sum_p_C3 <- sum(p1_C3, p2_C3, p3_C3)

# Normalizing probabilities for each trend for each change class
#Increase
p1_C1_prob <- p1_C1/sum_p_C1
p2_C1_prob <- p2_C1/sum_p_C1
p3_C1_prob <- p3_C1/sum_p_C1

#Stable
p1_C2_prob <- p1_C2/sum_p_C2
p2_C2_prob <- p2_C2/sum_p_C2
p3_C2_prob <- p3_C2/sum_p_C2

#Decrease
p1_C3_prob <- p1_C3/sum_p_C3
p2_C3_prob <- p2_C3/sum_p_C3
p3_C3_prob <- p3_C3/sum_p_C3



#priors
alpha~dnorm(0,0.0001)
beta~dnorm(0,0.0001)
gamma~dnorm(0,0.0001)

for(i in 1:3){ ## the fixed effect of start year
  alphaA[i]~dnorm(0,1)
  betaB[i]~dnorm(0,1)
  gammaC[i]~dnorm(0,1)
} 


#priors for RE of raster cell id

rasterRE.sd ~ dgamma(0.1, 0.1)
rasterRE.prec <- pow(rasterRE.sd, -2)
for(i in 1:nCells){
  rasterRE[i] ~ dnorm(0, rasterRE.prec)
}

# for(i in 1:nCells){ 
#   rasterRE[i]~dnorm(0,tau) #Random effects
# } 


}",file=BUGSfilename_mult)
  return(BUGSfilename_mult)
}

# Model 3: RE of raster cell ID and grid cell ID
WriteJAGS_changecat_starts_rasterRE_gridRE <- function(){
  BUGSfilename_mult <- "./models/multinomial_jagsUI/code.OHV.txt"
  cat("
    
    model{
   
     for(i in 1:N){
        
        trend[i,1:3]~dmulti(p[i,1:3],1) #likelihood
        
        #process model
        
        #Increasae
        log(p0[i,1]) <- alphaA[change_class[i]] + alpha * start_val_num_s[i] + rasterRE[raster[i]] + gridRE[grid[i]]
        
        #Stable
        log(p0[i,2]) <- betaB[change_class[i]] + beta * start_val_num_s[i] + rasterRE[raster[i]] + gridRE[grid[i]]
        
        #Decrease
        log(p0[i,3]) <- gammaC[change_class[i]] + gamma * start_val_num_s[i] + rasterRE[raster[i]] + gridRE[grid[i]]
        
        for(j in 1:3){
        p[i,j]<-p0[i,j]/sum(p0[i, ])
        }

   }

#derived parameters
log(p1_C1) <- (alpha * C1_start_mean) + alphaA[1]
log(p1_C2) <- (alpha * C2_start_mean) + alphaA[2]
log(p1_C3) <- (alpha * C3_start_mean) + alphaA[3]

log(p2_C1) <- (beta * C1_start_mean) + betaB[1]
log(p2_C2) <- (beta * C2_start_mean) + betaB[2]
log(p2_C3) <- (beta * C3_start_mean) + betaB[3]

log(p3_C1) <- (gamma * C1_start_mean) + gammaC[1]
log(p3_C2) <- (gamma * C2_start_mean) + gammaC[2]
log(p3_C3) <- (gamma * C3_start_mean) + gammaC[3]

# Adding the probabilities for all trends for each change class
sum_p_C1 <- sum(p1_C1, p2_C1, p3_C1)
sum_p_C2 <- sum(p1_C2, p2_C2, p3_C2)
sum_p_C3 <- sum(p1_C3, p2_C3, p3_C3)

# Normalizing probabilities for each trend for each change class
#Increase
p1_C1_prob <- p1_C1/sum_p_C1
p2_C1_prob <- p2_C1/sum_p_C1
p3_C1_prob <- p3_C1/sum_p_C1

#Stable
p1_C2_prob <- p1_C2/sum_p_C2
p2_C2_prob <- p2_C2/sum_p_C2
p3_C2_prob <- p3_C2/sum_p_C2

#Decrease
p1_C3_prob <- p1_C3/sum_p_C3
p2_C3_prob <- p2_C3/sum_p_C3
p3_C3_prob <- p3_C3/sum_p_C3



#priors
alpha~dnorm(0,0.0001)
beta~dnorm(0,0.0001)
gamma~dnorm(0,0.0001)

for(i in 1:3){ ## the fixed effect of start year
  alphaA[i]~dnorm(0,1)
  betaB[i]~dnorm(0,1)
  gammaC[i]~dnorm(0,1)
} 


#priors for RE of raster cell id

rasterRE.sd ~ dgamma(0.1, 0.1)
rasterRE.prec <- pow(rasterRE.sd, -2)
for(i in 1:nCells){
  rasterRE[i] ~ dnorm(0, rasterRE.prec)
}

# for(i in 1:nCells){ 
#   rasterRE[i]~dnorm(0,tau) #Random effects
# } 

#priors for RE of grid cell id

gridRE.sd ~ dgamma(0.1, 0.1)
gridRE.prec <- pow(gridRE.sd, -2)
for(i in 1:nTiles){
  gridRE[i] ~ dnorm(0, gridRE.prec)
}

# for(i in 1:nTiles){ 
#   gridRE[i]~dnorm(0,tau) #Random effects
# } 


}",file=BUGSfilename_mult)
  return(BUGSfilename_mult)
}
