library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))
source("../Code/Functions.R")

RunModRetro.new <- function(Dat, Pred_Year, method) {
  
  # for year X forecast, would have return data up to year X-1
  # age-4 return (rec4) up to brood year X-5, age-5 return (rec5) up to brood year X-6
  
  Data_Retro <- Dat %>% filter(yr <= (Pred_Year-4))
  Data_Retro[Data_Retro$yr == (Pred_Year-5), c("rec5", "R")] <- NA
  Data_Retro[Data_Retro$yr == (Pred_Year-4), c("rec4", "rec5", "R")] <- NA
  
  # Want to scale down obs to make models more stable
  # only really required for TMB, but will use for all types
  Scale <- 10^(floor(log(mean(Data_Retro$R, na.rm=T), 10))) # most numbers should be below 10
  
  # Set up data and starting value lists list to go into model
  data <- list()
  data$R_Obs <- Data_Retro$R/Scale
  data$N <- dim(Data_Retro)[1]
  
  if (method == "BasicRicker"){ #### Basic Ricker ====================================
    data$S <- Data_Retro$S/Scale 
    data$logA_mean <- 1
    data$logA_tau <- 1
    data$Sig_Gam_Dist <- 0.001
    data$logSmax_tau <- 1
    data$logSmax_mean <- log(quantile(Data_Retro$S, 0.8)/Scale)
    
    inits <- function(){
      param <- list()
      param$tau <- runif(1, 0.001, 10)
      param$logA <- rnorm(1, 1, 0.5)
      param$Smax <- runif(1, min = quantile(data$S, 0.8), max = max(data$S)*1.2)
      param
    }
    
    JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = Ricker.model.MCMC, 
                    n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                    parameters.to.save = c("R_Fit", "R_Pred", "logA", "Smax", "sigma"))
    Mod <- "BasicRicker"
  }else if (method == "Ricker"){ #### Ricker env ========================================
    data$env <- Data_Retro$envcov #/Scale?
    data$g_mean <- 0
    data$g_tau <- 1
    data$S <- Data_Retro$S/Scale 
    data$logA_mean <- 1
    data$logA_tau <- 1
    data$Sig_Gam_Dist <- 0.001
    data$logSmax_tau <- 1
    data$logSmax_mean <- log(quantile(Data_Retro$S, 0.8)/Scale)
    
    inits <- function(){
      param <- list()
      param$tau <- runif(1, 0.001, 10)
      param$g <- rnorm(1, 0, 0.5)
      param$logA <- rnorm(1, 1, 0.5)
      param$Smax <- runif(1, min = quantile(data$S, 0.8), max = max(data$S)*1.2)
      param
    }
    
    JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = RickerCov.model.MCMC, 
                    n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                    parameters.to.save = c("R_Fit", "R_Pred", "logA", "Smax", "sigma", "g"))
    Mod <- "Ricker Env"
  }else if (method == "Power"){ #### Power =======================================
    data$A_mean <- 1
    data$A_tau <- 1
    data$B_mean <- 1
    data$B_tau <- 1
    data$Sig_Gam_Dist <- 0.001
    data$S <- Data_Retro$S/Scale 
    
    inits <- function(){
      param <- list()
      param$tau <- runif(1, 0.001, 10)
      param$A <- rnorm(1, 1, 0.5)
      param$B <- rnorm(1, 1, 0.5)
      param
    }
    
    JagsFit <- jags(data, inits = list(inits(), inits(), inits()), model.file = Power.model.MCMC, 
                    n.chains =3, n.iter=10000, n.burnin = 4000, n.thin = 3, 
                    parameters.to.save = c("R_Fit", "R_Pred", "A", "B", "sigma"))
    Mod <- "Power"
  }
  
  
  # Turn into Data Frame
  All_Ests <- data.frame(JagsFit$BUGSoutput$summary)
  All_Ests$Param <- row.names(All_Ests)
  
  R_Ests_Jags <- All_Ests[grepl("R_Fit", All_Ests$Param),  ]
  R_Preds_Jags <- All_Ests[grepl("R_Pred", All_Ests$Param),  ]
  
  FitsDF <- data.frame(S = Data_Retro$S, R = Data_Retro$R, Fit = R_Ests_Jags$X50. * Scale, 
                       Year = Data_Retro$yr,   Mod = Mod,
                       CI_up = R_Ests_Jags$X97.5. * Scale,
                       CI_low = R_Ests_Jags$X2.5. * Scale,
                       Pred = R_Preds_Jags$X50. * Scale,
                       Pred_low = R_Preds_Jags$X2.5. * Scale,
                       Pred_up = R_Preds_Jags$X97.5. * Scale)
  
  # get age proportion estimates
  P4 <- mean(Data_Retro$rec4/Data_Retro$R, na.rm=T)
  # This is quite a crude way to do this and doesn't take into account
  # uncertainty, can you think of a better way to do this?
  
  # apply to brood years 4 and 5 years before Pred_Year
  FitsDF$Pred[FitsDF$Year == Pred_Year-4]
  
  Preds_Out <- data.frame(Pred_Year = Pred_Year, Mod = Mod,  ModType="Bio",
                          Pred4 = FitsDF$Pred[FitsDF$Year == Pred_Year-4]*P4, 
                          Pred5 = FitsDF$Pred[FitsDF$Year == Pred_Year-5]*(1-P4))
  
  out <- list()
  out$Preds_Out <- Preds_Out
  out$Fit <- FitsDF
  out
  
}
