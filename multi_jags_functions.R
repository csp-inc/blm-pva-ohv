# This script creates functions to run various multinomial models for OHV

# Data prep functions
PrepDataForJAGS_OHV <- function(){
  
  # Load in the saved dataframe
  data <- read.csv("./models/multinomial_jagsUI/Data_for_jags.csv")
  
  ### Create objects that can be used for predict in the model
  
  # Categorical
  N<-nrow(data)
  TREND <- data$TrendID_jags
  GRID <- data$gridID_jags
  nTiles <- length(unique(data$gridID_jags))
  STATE <- data$stateID_jags
  CHANGE <- data$Change_ClassID_jags
  START <- data$StartID_jags
  
  # Numeric
  start_s<-data$Start_Scale
  year_s <- data$Year_Scale
  start <- data$Start
  year <- data$Change_year
  
  
  # Matirx of responses
  trend<-matrix(data = 0, nrow = N, ncol = 3)
  for(i in 1:N){
    for(j in 1:3){
      if(isTRUE(TREND[i] == "1")) {trend[i,1]<-1}  
      if(isTRUE(TREND[i] == "2")) {trend[i,2]<-1}
      if(isTRUE(TREND[i] == "3")) {trend[i,3]<-1}
    }
  }
  
  # Other data values needed for estimation
  data_C1 <- data %>% filter(Change_Class == "C1") 
  C1_start_mean <- mean(data_C1$Start_Scale)
  
  data_C2 <- data %>% filter(Change_Class == "C2") 
  C2_start_mean <- mean(data_C2$Start_Scale)
  
  data_C3 <- data %>% filter(Change_Class == "C3") 
  C3_start_mean <- mean(data_C3$Start_Scale)
  
  
  # Create range of values for in-modeling predictions, both scaled and not scaled
  year_predict <- as.numeric(seq(min(year_s), max(year_s), length.out = 50))
  year_predict_ns <- as.numeric(seq(min(year), max(year), length.out = 50))
  
  start_predict <- as.numeric(seq(min(start_s), max(start_s), length.out = 50))
  start_predict_ns <- as.numeric(seq(min(start), max(start), length.out = 50))
  
  
  
  
  ##### Prep data for jags (dfj) -----
  dfj_t <- list()
  
  # Save the data for JAGS
  dfj_t$data.for.bugs <- list(N = N,
                              # TREND = TREND,
                              trend = trend,
                              GRID = GRID,
                              STATE = STATE,
                              CHANGE = CHANGE,
                              START = START,
                              nTiles = nTiles,
                              start_s = start_s,
                              year_s = year_s,
                              start = start,
                              year = year,
                              C1_start_mean = C1_start_mean,
                              C2_start_mean = C2_start_mean,
                              C3_start_mean = C3_start_mean,
                              # Prediction dataframes for covariates
                              start_predict = start_predict,
                              year_predict = year_predict
                              
                              
  )
  
  # Save unscaled prediction objects
  dfj_t$predict <- list(start_predict_ns = start_predict_ns,
                        year_predict_ns = year_predict_ns)
  
  # Save raw data for covariates
  # What do I put here?
  dfj_t$raw_covs <- list()
  
  # Specifiy initial values
  dfj_t$init.bugs <- function(){list(alpha=runif(1,-0.01,0.01),
                                     beta=runif(1,-0.01,0.01),
                                     gamma=runif(1,-0.01,0.01),
                                     alphaA=runif(3,-0.01,0.01),
                                     betaB=runif(3,-0.01,0.01),
                                     gammaC=runif(3,-0.01,0.01)
                                     # A=runif(1,-0.01,0.01),
                                     # B=runif(1,-0.01,0.01),
                                     # C=runif(1,-0.01,0.01),
                                     # tau=runif(1,-0.01,0.01),
                                     # Atau=runif(1,-0.01,0.01),
                                     # Btau=runif(1,-0.01,0.01),
                                     # Ctau=runif(1,-0.01,0.01)
  )}
  
  
  # Parameters to monitor
  dfj_t$tomonitor <- c("alpha", "alphaA","beta","betaB", "gamma","gammaC","p1_C1_prob","p1_C2_prob","p1_C3_prob","p2_C1_prob","p2_C2_prob","p2_C3_prob","p3_C1_prob","p3_C2_prob","p3_C3_prob")
  
  return(dfj_t)
  
}




# MODEL functions
WriteJAGS_change.cat_start.s <- function(){
  BUGSfilename_mult <- "./models/multinomial_jagsUI/code.OHV.txt"
  cat("
    
    model{
   
     for(i in 1:N){
        
        trend[i,1:3]~dmulti(p[i,1:3],1) #likelihood
        
        #process model
        
        #Increasae
        log(p0[i,1]) <- alphaA[CHANGE[i]] + alpha[1] * start_s[i] + gridRE[GRID[i]] 
        
        #Stable
        log(p0[i,2]) <- betaB[CHANGE[i]] + beta[1] * start_s[i] + gridRE[GRID[i]]
        
        #Decrease
        log(p0[i,3]) <- gammaC[CHANGE[i]] + gamma[1] * start_s[i] + gridRE[GRID[i]]  
        
        for(j in 1:3){
        p[i,j]<-p0[i,j]/sum(p0[i, ])
        }

   }

# #derived parameters
# log(p1_C1) <- (alpha[1] * C1_start_mean) + alphaA[1]
# log(p1_C2) <- (alpha[1] * C2_start_mean) + alphaA[2]
# log(p1_C3) <- (alpha[1] * C3_start_mean) + alphaA[3]
# 
# log(p2_C1) <- (beta[1] * C1_start_mean) + betaB[1]
# log(p2_C2) <- (beta[1] * C2_start_mean) + betaB[2]
# log(p2_C3) <- (beta[1] * C3_start_mean) + betaB[3]
# 
# log(p3_C1) <- (gamma[1] * C1_start_mean) + gammaC[1]
# log(p3_C2) <- (gamma[1] * C2_start_mean) + gammaC[2]
# log(p3_C3) <- (gamma[1] * C3_start_mean) + gammaC[3]
# 
# # Adding the probabilities for all trends for each change class
# sum_p_C1 <- sum(p1_C1, p2_C1, p3_C1)
# sum_p_C2 <- sum(p1_C2, p2_C2, p3_C2)
# sum_p_C3 <- sum(p1_C3, p2_C3, p3_C3)
# 
# # Normalizing probabilities for each trend for each change class
# #Increase
# p1_C1_prob <- p1_C1/sum_p_C1
# p2_C1_prob <- p2_C1/sum_p_C1
# p3_C1_prob <- p3_C1/sum_p_C1
# 
# #Stable
# p1_C2_prob <- p1_C2/sum_p_C2
# p2_C2_prob <- p2_C2/sum_p_C2
# p3_C2_prob <- p3_C2/sum_p_C2
# 
# #Decrease
# p1_C3_prob <- p1_C3/sum_p_C3
# p2_C3_prob <- p2_C3/sum_p_C3
# p3_C3_prob <- p3_C3/sum_p_C3



#priors
alpha~dnorm(0,0.0001)
beta~dnorm(0,0.0001)
gamma~dnorm(0,0.0001)

for(i in 1:3){ ## the fixed effect of start year
  alphaA[i]~dnorm(0,1)
  betaB[i]~dnorm(0,1)
  gammaC[i]~dnorm(0,1)
} 

# # Hyperparameters for the fixed effect
# A~dnorm(0,0.0001)
# B~dnorm(0,0.0001)
# C~dnorm(0,0.0001)

#priors for RE of grid cell id

gridRE.sd ~ dgamma(0.1, 0.1)
gridRE.prec <- pow(gridRE.sd, -2)
for(i in 1:nTiles){
  gridRE[i] ~ dnorm(0, gridRE.prec)
}

# for(i in 1:nTiles){ 
#   gridRE[i]~dnorm(0,tau) #Random effects
# } 

#variance for random effect and for hyper parameters for the fixed effect of tenure type
# tau~dgamma(0.0001,0.0001)
# Atau~dgamma(0.0001,0.0001)
# Btau~dgamma(0.0001,0.0001)
# Ctau~dgamma(0.0001,0.0001)

}",file=BUGSfilename_mult)
  return(BUGSfilename_mult)
}
