
# MCMC with Basic Ricker Model
RunMCMC_Ricker <- function(Dat_MCMC, Scale){
  
  # Set up data and starting value lists list to go into model
  data <- list()
  data$S <- Dat_MCMC$S/Scale 
  data$logA_mean <- 1
  data$logA_tau <- 1
  data$Sig_Gam_Dist <- 0.001
  data$logSmax_tau <- 1
  data$logSmax_mean <- log(quantile(Dat_MCMC$S, 0.8)/Scale)
  data$R_Obs <- Dat_MCMC$R/Scale
  data$N <- dim(Dat_MCMC)[1]
  
  # function to set initial values
  inits <- function(){
    param <- list()
    param$logA <- rnorm(1, 1, 0.5)
    param$Smax <- runif(1, min = quantile(data$S, 0.8), max = max(data$S)*1.2)
    param$tau <- runif(1, 0.001, 10)
    param
  }
  
  JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = Ricker.model.MCMC, 
                  n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                  parameters.to.save = c("R_Fit", "R_Pred", "logA", "Smax", "sigma"))
  
  # Turn into Data Frame
  All_Ests <- data.frame(JagsFit$BUGSoutput$summary)
  All_Ests$Param <- row.names(All_Ests)
  
  # Prep A and Smax posteriors for outputs
  A_Post <- exp(JagsFit$BUGSoutput$sims.list$logA)
  Smax_Post <- JagsFit$BUGSoutput$sims.list$Smax * Scale
  
  R_Ests_Jags <- All_Ests[grepl("R_Fit", All_Ests$Param),  ]
  R_Preds_Jags <- All_Ests[grepl("R_Pred", All_Ests$Param),  ]
  
  FitsDF <- data.frame(S = Dat_MCMC$S, R = Dat_MCMC$R, Fit = R_Ests_Jags$X50. * Scale, 
                       Year = Dat_MCMC$yr,   Mod = "SimpleRicker",
                       CI_up = R_Ests_Jags$X97.5. * Scale,
                       CI_low = R_Ests_Jags$X2.5. * Scale,
                       Pred = R_Preds_Jags$X50. * Scale,
                       Pred_low = R_Preds_Jags$X2.5. * Scale,
                       Pred_up = R_Preds_Jags$X97.5. * Scale)
  out <- list(FitsDF=FitsDF, A_Post=A_Post, Smax_Post=Smax_Post)
  return(out)
}

# MCMC with RickerCov Model
RunMCMC_RickerCov <- function(Dat_MCMC, Scale, CovData){
  
  # Set up data and starting value lists list to go into model
  data <- list()
  data$S <- Dat_MCMC$S/Scale 
  data$logA_mean <- 1
  data$logA_tau <- 1
  data$Sig_Gam_Dist <- 0.001
  data$logSmax_tau <- 1
  data$logSmax_mean <- log(quantile(Dat_MCMC$S, 0.8)/Scale)
  data$R_Obs <- Dat_MCMC$R/Scale
  data$N <- dim(Dat_MCMC)[1]
  data$env <- CovData[1:dim(Dat_MCMC)[1]]
  data$g_mean <- 0
  data$g_tau <- 0.001
  
  # function to set initial values
  inits <- function(){
    param <- list()
    param$logA <- rnorm(1, 1, 0.5)
    param$Smax <- runif(1, min = quantile(data$S, 0.8), max = max(data$S)*1.2)
    param$tau <- runif(1, 0.001, 10)
    param$g <- rnorm(1,0,1000)
    param
  }
  
  JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = RickerCov.model.MCMC, 
                  n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                  parameters.to.save = c("R_Fit", "R_Pred", "logA", "Smax", "sigma","g"))
  
  # Turn into Data Frame
  All_Ests <- data.frame(JagsFit$BUGSoutput$summary)
  All_Ests$Param <- row.names(All_Ests)
  
  # Prep A and Smax posteriors for outputs
  A_Post <- exp(JagsFit$BUGSoutput$sims.list$logA)
  Smax_Post <- JagsFit$BUGSoutput$sims.list$Smax * Scale
  g <- JagsFit$BUGSoutput$sims.list$g
  
  R_Ests_Jags <- All_Ests[grepl("R_Fit", All_Ests$Param),  ]
  R_Preds_Jags <- All_Ests[grepl("R_Pred", All_Ests$Param),  ]
  
  FitsDF <- data.frame(S = Dat_MCMC$S, R = Dat_MCMC$R,
                       Fit = R_Ests_Jags$X50. * Scale, 
                       Year = Dat_MCMC$yr,   Mod = "CovRicker",
                       CI_up = R_Ests_Jags$X97.5. * Scale,
                       CI_low = R_Ests_Jags$X2.5. * Scale,
                       Pred = R_Preds_Jags$X50. * Scale,
                       Pred_low = R_Preds_Jags$X2.5. * Scale,
                       Pred_up = R_Preds_Jags$X97.5. * Scale)
  out <- list(FitsDF=FitsDF, A_Post=A_Post, Smax_Post=Smax_Post, g=g)
  return(out)
}

# MCMC with Power Model
RunMCMC_Power <- function(Dat_MCMC, Scale){
  
  # Set up data and starting value lists list to go into model
  data <- list()
  data$S <- Dat_MCMC$S/Scale 
  data$A_mean <- 1
  data$A_tau <- 0.001
  data$B_mean <- 1
  data$B_tau <- 0.001
  data$Sig_Gam_Dist <- 0.001
  data$R_Obs <- Dat_MCMC$R/Scale
  data$N <- dim(Dat_MCMC)[1]
  
  # function to set initial values
  inits <- function(){
    param <- list()
    param$A <- rnorm(1, 1, 100)
    param$B <- rnorm(1, 1, 100)
    param$tau <- runif(1, 0.001, 10)
    param
  }
  
  JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = Power.model.MCMC, 
                  n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                  parameters.to.save = c("R_Fit", "R_Pred", "A", "B", "sigma"))
  
  # Turn into Data Frame
  All_Ests <- data.frame(JagsFit$BUGSoutput$summary)
  All_Ests$Param <- row.names(All_Ests)
  
  # Prep A and B posteriors for outputs
  A_Post <- JagsFit$BUGSoutput$sims.list$A
  B_Post <- JagsFit$BUGSoutput$sims.list$B
  
  R_Ests_Jags <- All_Ests[grepl("R_Fit", All_Ests$Param),  ]
  R_Preds_Jags <- All_Ests[grepl("R_Pred", All_Ests$Param),  ]
  
  FitsDF <- data.frame(S = Dat_MCMC$S, R = Dat_MCMC$R, Fit = R_Ests_Jags$X50. * Scale, 
                       Year = Dat_MCMC$yr,   Mod = "Power",
                       CI_up = R_Ests_Jags$X97.5. * Scale,
                       CI_low = R_Ests_Jags$X2.5. * Scale,
                       Pred = R_Preds_Jags$X50. * Scale,
                       Pred_low = R_Preds_Jags$X2.5. * Scale,
                       Pred_up = R_Preds_Jags$X97.5. * Scale)
  out <- list(FitsDF=FitsDF, A_Post=A_Post, B_Post=B_Post)
  return(out)
}

# PowerCov model 
PowerCov.model.MCMC <- function(){
  for (i in 1:N) {                             # loop over N sample points
    R_Obs[i] ~ dlnorm(logR_Fit[i], tau)          # likelihood -> predicted value for NA in data set
    logR_Fit[i] <- A + B * log(S[i]) + g * env[i]       # power model with env covariate
    R_Fit[i] <- exp(logR_Fit[i])
    R_Pred[i] ~ dlnorm(logR_Fit[i],tau)
  }
  
  g ~ dnorm(g_mean,g_tau)     #prior for g
  A ~ dnorm(A_mean, A_tau)             # prior for alpha
  B ~ dnorm(B_mean, B_tau)                # prior for beta
  tau ~ dgamma(Sig_Gam_Dist,Sig_Gam_Dist)    # prior for precision parameter
  sigma <- 1/sqrt(tau)   		                  	
  
}

# MCMC with PowerCov Model
RunMCMC_PowerCov <- function(Dat_MCMC, Scale, CovData){
  
  # Set up data and starting value lists list to go into model
  data <- list()
  data$S <- Dat_MCMC$S/Scale 
  data$A_mean <- 1
  data$A_tau <- 0.001
  data$B_mean <- 1
  data$B_tau <- 0.001
  data$Sig_Gam_Dist <- 0.001
  data$R_Obs <- Dat_MCMC$R/Scale
  data$N <- dim(Dat_MCMC)[1]
  data$env <- CovData[1:dim(Dat_MCMC)[1]]
  data$g_mean <- 0
  data$g_tau <- 0.001
  
  # function to set initial values
  inits <- function(){
    param <- list()
    param$A <- rnorm(1, 1, 0.5)
    param$B <- rnorm(1, 1, 0.5)
    param$tau <- runif(1, 0.001, 10)
    param$g <- rnorm(1,0,1000)
    param
  }
  
  JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = PowerCov.model.MCMC, 
                  n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                  parameters.to.save = c("R_Fit", "R_Pred", "A", "B", "sigma","g"))
  
  # Turn into Data Frame
  All_Ests <- data.frame(JagsFit$BUGSoutput$summary)
  All_Ests$Param <- row.names(All_Ests)
  
  # Prep A and Smax posteriors for outputs
  A_Post <- exp(JagsFit$BUGSoutput$sims.list$A)
  B_Post <- exp(JagsFit$BUGSoutput$sims.list$B)
  Smax_Post <- JagsFit$BUGSoutput$sims.list$Smax * Scale
  g <- JagsFit$BUGSoutput$sims.list$g
  
  R_Ests_Jags <- All_Ests[grepl("R_Fit", All_Ests$Param),  ]
  R_Preds_Jags <- All_Ests[grepl("R_Pred", All_Ests$Param),  ]
  
  FitsDF <- data.frame(S = Dat_MCMC$S, R = Dat_MCMC$R,
                       Fit = R_Ests_Jags$X50. * Scale, 
                       Year = Dat_MCMC$yr,   Mod = "CovPower",
                       CI_up = R_Ests_Jags$X97.5. * Scale,
                       CI_low = R_Ests_Jags$X2.5. * Scale,
                       Pred = R_Preds_Jags$X50. * Scale,
                       Pred_low = R_Preds_Jags$X2.5. * Scale,
                       Pred_up = R_Preds_Jags$X97.5. * Scale)
  out <- list(FitsDF=FitsDF, A_Post=A_Post, B_Post=B_Post, g=g)
  return(out)
}


# Run Models Retrospectively
RunModRetro_new <- function(Dat, Pred_Year, Model, CovData=NA) {
  
  # for year X forecast, would have return data up to year X-1
  # age-4 return (rec4) up to brood year X-5, age-5 return (rec5) up to brood year X-6
  
  Data_Retro <- Dat %>% filter(yr <= (Pred_Year-4))
  Data_Retro[Data_Retro$yr == (Pred_Year-5), c("rec5", "R")] <- NA
  Data_Retro[Data_Retro$yr == (Pred_Year-4), c("rec4", "rec5", "R")] <- NA
  
  # Want to scale down obs to make models more stable
  # only really required for TMB, but will use for all types
  Scale <- 10^(floor(log(mean(Data_Retro$R, na.rm=T), 10))) # most numbers should be below 10
  

  if(Model=="Ricker"){
    MCMC_out <- RunMCMC_Ricker(Data_Retro, Scale)
  }else if(Model=="RickerCov"){
    MCMC_out <- RunMCMC_RickerCov(Data_Retro, Scale, CovData)
  }else if(Model=="Power"){
    MCMC_out <- RunMCMC_Power(Data_Retro, Scale)
  }else if(Model=="PowerCov"){
    MCMC_out <- RunMCMC_PowerCov(Data_Retro, Scale, CovData)
  }else{
    print("Cannot recognize the model.")
    return()
  }
  FitsDF <- MCMC_out$FitsDF
  
  # get age proportion estimates
  P4 <- mean(Data_Retro$rec4/Data_Retro$R, na.rm=T)
  # This is quite a crude way to do this and doesn't take into account
  # uncertainty, can you think of a better way to do this?
  
  # apply to brood years 4 and 5 years before Pred_Year
  FitsDF$Pred[FitsDF$Year == Pred_Year-4]
  
  Preds_Out <- data.frame(Pred_Year = Pred_Year, Mod = Model,  ModType="Bio",
                          Pred4 = FitsDF$Pred[FitsDF$Year == Pred_Year-4]*P4, 
                          Pred5 = FitsDF$Pred[FitsDF$Year == Pred_Year-5]*(1-P4),
                          Pred = FitsDF$Pred[FitsDF$Year == Pred_Year-4]*P4 + FitsDF$Pred[FitsDF$Year == Pred_Year-5]*(1-P4),
                          Pred_low = FitsDF$Pred_low[FitsDF$Year == Pred_Year-4]*P4 + FitsDF$Pred_low[FitsDF$Year == Pred_Year-5]*(1-P4),
                          Pred_up = FitsDF$Pred_up[FitsDF$Year == Pred_Year-4]*P4 + FitsDF$Pred_up[FitsDF$Year == Pred_Year-5]*(1-P4))
  
  out <- list()
  out$Preds_Out <- Preds_Out
  out$Fit <- FitsDF
  # out$A_Post <- A_Post
  # out$Smax_Post <- Smax_Post
  out
  
}
