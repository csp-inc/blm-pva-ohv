
# Data prep functions
PrepDataForJAGS_glm_OHV <- function(){
  
  # Load in the saved dataframe
  data <- read.csv("./models/0inf_jagsUI/Data_for_jags.csv")
  
  ### Create objects that can be used for predict in the model
  
  # Categorical
  N<-nrow(data)
  raster <- data$rasterID_jags
  grid <- data$gridID_jags
  state <- data$stateID_jags

  # Numeric
  value <- data$value
  year <- as.numeric(data$year)
  
  # Numeric scaled
  year_sc <- data$year_sc
  
  # Other data values needed for estimation
  
  nCells <- length(unique(data$rasterID_jags))
  nTiles <- length(unique(data$gridID_jags))
  nStates <- length(unique(data$stateID_jags))
  
  # Predictions
  # Create range of values for in-modeling predictions, both scaled and not scaled
  year_predict <- as.numeric(seq(min(year_sc), max(year_sc), length.out = 50))
  year_predict_ns <- as.numeric(seq(min(year), max(year), length.out = 50))
  
  
  
  
  ##### Prep data for jags (dfj) -----
  dfj_t <- list()
  
  # Save the data for JAGS
  dfj_t$data.for.bugs <- list(N = N,
                              #Response
                              value = value,
                              # Categorical
                              raster = raster,
                              grid = grid,
                              state = state,
                              # Numerical
                              year_sc = year_sc,
                              # Other data
                              nCells = nCells,
                              nTiles = nTiles,
                              nStates = nStates,
                              # Prediction dataframes for covariates (scaled)
                              year_predict = year_predict
                              
                              
  )
  
  # Save unscaled prediction objects
  dfj_t$predict <- list(year_predict_ns = year_predict_ns)
  
  # Save raw data for covariates
  dfj_t$raw_covs <- list()
  
  # Specifiy initial values
  dfj_t$init.bugs <- function(){list(  beta0 = 0,
                                       beta1 = 0,
                                       rasterRE = rep(0, nCells),
                                       gridRE = rep(0, nTiles),
                                       tau_rast = 1,
                                       tau_grid = 1,
                                       z = 0.5,  # Initial value for zero-inflation parameter,
                                       phi = 1 # Initial value for negative binomial size parameter
                                     
  )}
  
  
  # Parameters to monitor
  dfj_t$tomonitor <- c("beta0", "beta1","z")
  
  return(dfj_t)
  
}



WriteJAGS_year_sc_rasterRE_gridRE_zip <- function(){
  BUGSfilename_zip <- "./models/0inf_jagsUI/code.OHV.txt"
  cat("
    
    model {
        # Likelihood
        for (i in 1:N) {
        
          # Logit model for certain 0s
          mu[i] <- exp(beta0 + beta1 * year_sc[i] + rasterRE[raster[i]] + gridRE[grid[i]])
          
          # Bernouli distribution, determines if i is 0-inflated or not
          zi[i] ~ dbern(z)
          
          # Inflation model
          value[i] ~ dpois(mu[i] * (1 - zi[i]))

        }
      
        # Priors
        beta0 ~ dnorm(0, 0.01)
        beta1 ~ dnorm(0, 0.01)
        
        for (j in 1:nCells) {
          rasterRE[j] ~ dnorm(0, tau_rast)
        }
        
        for (k in 1:nTiles) {
          gridRE[k] ~ dnorm(0, tau_grid)
        }
        
        tau_rast ~ dgamma(0.001, 0.001)
        tau_grid ~ dgamma(0.001, 0.001)
        
        z ~ dbeta(1, 1)  # Prior for zero-inflation parameter
        
      }",file=BUGSfilename_zip)
  return(BUGSfilename_zip)
}

WriteJAGS_year_sc_rasterRE_gridRE_zinb <- function(){
  BUGSfilename_zinb <- "./models/0inf_jagsUI/code.OHV.txt"
  cat("
    
    model {
        # Likelihood
        for (i in 1:N) {
        
          # Log-linear model for the mean
          mu[i] <- exp(beta0 + beta1 * year_sc[i] + rasterRE[raster[i]] + gridRE[grid[i]])
          
          # Bernoulli distribution, determines if i is 0-inflated or not
          zi[i] ~ dbern(z)
          
          # Zero-inflated negative binomial model
          # value[i] ~ dnbin(mu[i], phi) * (1 - zi[i]) + dpois(0) * zi[i]
          # value[i] ~ dpois(mu[i] * (1 - zi[i])) + dzinb(mu[i], phi, zi[i])
          # value[i] ~ dpois(mu[i] * (1 - zi[i])) + dnbin(mu[i], phi) * zi[i]
          
          value[i] ~ dpois(mu[i] * (1 - zi[i])) + (1 - zi[i]) * dnbin(mu[i], phi)
          value[i] ~ dpois(0) * zi[i]  # Point mass at zero component
          
        }
        
      
        # Priors
        beta0 ~ dnorm(0, 0.01)
        beta1 ~ dnorm(0, 0.01)
        phi ~ dgamma(0.001, 0.001)  # Prior for overdispersion parameter phi
        tau_rast ~ dgamma(0.001, 0.001)
        tau_grid ~ dgamma(0.001, 0.001)
        z ~ dbeta(1, 1)  # Prior for zero-inflation parameter
        
        
        for (j in 1:nCells) {
          rasterRE[j] ~ dnorm(0, tau_rast)
        }
        
        for (k in 1:nTiles) {
          gridRE[k] ~ dnorm(0, tau_grid)
        }
        
      
        
      }",file=BUGSfilename_zinb)
  return(BUGSfilename_zinb)
}

