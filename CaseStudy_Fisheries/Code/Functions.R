
#  =============================================================================================
#' Simulate a single stock of Ricker SR data
#'
#' @param leng Length of time series to simulate
#' @param Sig_Ricker SD around Ricker model
#' @param true_a Underlying alpha parameter
#' @param true_b Underlying beta parameter
#' @param hr_min Minimum harvest rate, default 0.2
#' @param hr_max Maximum harvest rate, default 0.8
#' @param lnorm_corr Use lognormal correction TRUE/FALSE
#' @param autoCorr Include temporal autocorrelation TRUE/FALSE
#' @param rho Level of temporal autocorrelation
#' @param covariate 
#' @param age Generation length ie. what age do they return at
#'
#' @return A list with the following elements: 
#'  - `true_a`: underlying alpha parameter
#'  - `true_b`: underlying beta parameter
#'  - `sigma`: simulated SD around Ricker
#'  - `SMSY`: Estimated Spawners at max. sustainable yield, using Hilborn approximation
#'  - `DF_Out`: data frame containing data, fitted values (useful to plot "true" relationship), and catch



Sim_Ricker_SR_Data <- function( leng=20, age=4, Sig_Ricker = 0.2, true_a = 3, true_b=1/5000,
                          hr_min = 0.2, hr_max = 0.8, lnorm_corr = F, autoCorr = F, rho=NA){

  
  # initiate population somwhere between 100 and Smax
  init <- round(runif(age, 100, 1/true_b))
  hr_vec <- runif(leng+age, hr_min, hr_max)
  
  esc<-rep(NA,leng+age)
  esc[1:age] <- init
  catch<-rep(NA,leng+age)
  rec<-rep(NA,leng+age)
  eps <- rep(NA,leng+age)
  
  for(i in (age+1):(leng+age)){
    R_mean <- true_a*esc[i-age] * exp(-true_b*esc[i-age])
    # random recruitment residual
    if(autoCorr == F){
       eps[i] <- rnorm(1, 0, Sig_Ricker)
       Sig <- Sig_Ricker
    } else {
      if(i == age+1){
        # if first year just simulate random resid
        eps[i] <- rnorm(1, 0, Sig_Ricker)
        Sig <- Sig_Ricker
      } else {
        # in subsequent years start from previous e
        eps[i] <- rnorm(1, rho*eps[i-1], Sig_Ricker*sqrt(1-rho^2))
        Sig <- Sig_Ricker*sqrt(1-rho^2)
      }
    }
    if(lnorm_corr == F){ 
      rec[i] <- max(R_mean*exp(eps[i]), 100) 
    } else {
      rec[i] <- max(R_mean*exp(eps[i]-0.5*Sig^2), 100)
    }
    # want to find way to make sure pop gets knocked down every once in a while
    # so that we have some resolution in the data
    # if stock gets up to smsy for 4 gens knock it down
    
    SRep<- log(true_a) / true_b
    # Estimate spawners at MSY (approximation; Hilborn and Walters 1992)
    # approximation is fine for this application
     SMSY <- SRep * ( 0.5 - 0.07*log(true_a) )
     # knock down pop if hanging out around MSY too long
    if(sum(esc[(i-age):(i-1)] > SMSY) == 4) { hr_vec[i] <- hr_max}
     
    esc[i] <- max((1-hr_vec[i])*rec[i], 100)
    catch[i] <- hr_vec[i]*rec[i]
  }

  # get "true" values to plot curve from
  true_S = seq(from=min(esc), to=max(esc),length.out=leng)
  true_R = true_a * true_S * exp(-true_S*true_b)
  
  DF_Out <- data.frame(S = round(esc[1:leng]), R = rec[(age+1):(leng+age)], Year = 1:leng, 
                          S_Fit = true_S, R_Fit = true_R, Catch = catch[(age+1):(leng+age)])
  
  
  output<-list(true_a=true_a, true_b=true_b, sigma=Sig_Ricker, SMSY = SMSY, DF_Out = DF_Out)
  return(output)
  
}

#=============================================================================

#Calculate Benchmarks
GetRickerBMs <- function(SRDat=NA, a=NA, B=NA, Hilborn_approx=F){
  
   # if haven't provided a and B, need to estimate
   # quick and dirty estimation using lm
   if(is.na(a)==T){ 
    SRDat$RPS <- SRDat$R/SRDat$S
    SRDat$logRPS <-log(SRDat$RPS)
    fit <- lm( logRPS ~ S, data=SRDat )
    # Get the Ricker alpha and beta parameters
    a <- exp( as.numeric(coef(fit)[1]) )  # Intercept, log(Ricker A)
    B <- -1 * as.numeric( coef(fit)[2] )  # Slope, - Ricker b
    # Add R_Est to dataframs
    SRDat <- SRDat %>% mutate(R_Est = a*S*exp(-B*S))
   }
    
  if(Hilborn_approx==T){
    # Calculate spawners at MSY (approximation; Hilborn and Walters 1992)
    # calculate spawners at replacement
    SRep<- log(a) / B
    SMSY <- SRep * ( 0.5 - 0.07*log(a) )
  } else {
    # Calculate using Scheurell 2016 exact approach
    SMSY <- (1 - gsl::lambert_W0(exp(1 - log(a)))) / B
  }
    
    # solve for Sgen
    ObjectiveSGen <- function( S, SpMSY, alpha, beta ) {
      # Recruits to get to SMSY (Holt et al. 2009)
      R <- alpha * S * exp( -beta * S )
      # Residual difference between ln(SMSY) and ln(R)
      delta <- log( SpMSY ) - log( R )
      # Calculate the negative log-likelihood
      negLL <- -1 * dnorm( x=delta, mean=0, sd=1, log=TRUE )
      # Return the negative log-likelihood (this is the value to minimize)
      return( negLL )
    }  # End ObjectiveSGen function
    
    opt <- optimize( f=ObjectiveSGen, interval=c(0, SMSY), SpMSY=SMSY,
                     alpha=a, beta=B )
    
    # Get Sgen from the optimized output (i.e., minimum neg log-like)
    Sgen <- opt$minimum
    
  
  out <- list(dat = SRDat, alpha = a, beta = B, SMSY = SMSY, Sgen=Sgen)
  out
} # end GetRickerBMs function



#===========================================================================

# Run Models Retrospectively
RunModRetro <- function(Dat, Pred_Year) {
 
  # for year X forecast, would have return data up to year X-1
   # age-4 return (rec4) up to brood year X-5, age-5 return (rec5) up to brood year X-6

    Data_Retro <- Dat %>% filter(yr <= (Pred_Year-4))
    Data_Retro[Data_Retro$yr == (Pred_Year-5), c("rec5", "R")] <- NA
    Data_Retro[Data_Retro$yr == (Pred_Year-4), c("rec4", "rec5", "R")] <- NA
 

 # Basic Ricker=================================
      
      # Want to scale down obs to make models more stable
      # only really required for TMB, but will use for all types
      Scale <- 10^(floor(log(mean(Data_Retro$R, na.rm=T), 10))) # most numbers should be below 10
    
      # Set up data and starting value lists list to go into model
      data <- list()
      data$S <- Data_Retro$S/Scale 
      data$logA_mean <- 1
      data$logA_tau <- 1
      data$Sig_Gam_Dist <- 0.001
      data$logSmax_tau <- 1
      data$logSmax_mean <- log(quantile(Data_Retro$S, 0.8)/Scale)
      data$R_Obs <- Data_Retro$R/Scale
      data$N <- dim(Data_Retro)[1]
      
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
    
    R_Ests_Jags <- All_Ests[grepl("R_Fit", All_Ests$Param),  ]
    R_Preds_Jags <- All_Ests[grepl("R_Pred", All_Ests$Param),  ]
    
    FitsDF <- data.frame(S = Data_Retro$S, R = Data_Retro$R, Fit = R_Ests_Jags$X50. * Scale, 
                         Year = Data_Retro$yr,   Mod = "SimpleRicker",
                         CI_up = R_Ests_Jags$X97.5. * Scale,
                         CI_low = R_Ests_Jags$X2.5. * Scale,
                         Pred = R_Preds_Jags$X50. * Scale,
                         Pred_low = R_Preds_Jags$X2.5. * Scale,
                         Pred_up = R_Preds_Jags$X97.5. * Scale)
    
    # Prep A and Smax posteriors for outputs
    A_Post <- exp(JagsFit$BUGSoutput$sims.list$logA)
    Smax_Post <- JagsFit$BUGSoutput$sims.list$Smax * Scale
    
    # get age proportion estimates
    P4 <- mean(Data_Retro$rec4/Data_Retro$R, na.rm=T)
    # This is quite a crude way to do this and doesn't take into account
    # uncertainty, can you think of a better way to do this?
    
    # apply to brood years 4 and 5 years before Pred_Year
    FitsDF$Pred[FitsDF$Year == Pred_Year-4]
  
  Preds_Out <- data.frame(Pred_Year = Pred_Year, Mod = "Ricker",  ModType="Bio",
                       Pred4 = FitsDF$Pred[FitsDF$Year == Pred_Year-4]*P4, 
                       Pred5 = FitsDF$Pred[FitsDF$Year == Pred_Year-5]*(1-P4))
  
  out <- list()
  out$Preds_Out <- Preds_Out
  out$Fit <- FitsDF
  out$A_Post <- A_Post
  out$Smax_Post <- Smax_Post
  out
  
}

#=========================================================================

# Basic Ricker model, for use with JAGS
Ricker.model.MCMC <- function(){
  
  #Priors
  logA ~ dnorm(logA_mean, logA_tau)                  # normal prior on logA        
  beta <-1/Smax					   # prior for beta
  Smax ~ dlnorm(logSmax_mean, logSmax_tau)       			# prior on Smax instead of beta 
  tau ~ dgamma(Sig_Gam_Dist,Sig_Gam_Dist)            #prior for precision parameter
  sigma <- 1/sqrt(tau) 		 
  
  
  
  for (i in 1:N) {                       #loop over N sample points
    R_Obs[i] ~ dlnorm(logR_Fit[i], tau)          #likelihood -> predicted value for NA in data set
    logR_Fit[i] <-  logA - beta * S[i] + log(S[i])         # calc log(R) - fitted values  
    R_Fit[i] <- exp(logR_Fit[i])
    R_Pred[i] ~ dlnorm(logR_Fit[i],tau)     
  }
  
}

#============================================================================

# Some other simple JAGS models that could be candidates

# Ricker model with env. covariate
RickerCov.model.MCMC <- function(){
  for (i in 1:N) {                                    #loop over N sample points      
    R_Obs[i] ~ dlnorm(logR_Fit[i],tau)                   #likelihood 
    logR_Fit[i] <- logA - beta *S[i] + log(S[i])  + g * env[i] 
    R_Fit[i] <- exp(logR_Fit[i])
    R_Pred[i] ~ dlnorm(logR_Fit[i], tau)
  }
  
  g ~ dnorm(g_mean,g_tau)     
  logA ~ dnorm(logA_mean, logA_tau)               #prior for alpha
  beta <- 1/Smax				   # prior for Smax
  Smax ~ dlnorm(logSmax_mean, logSmax_tau)       			    
  tau ~ dgamma(Sig_Gam_Dist, Sig_Gam_Dist)                      #prior for precision parameter
  sigma <- 1/sqrt(tau)                                 		
}

#---------------------------------
# Power model 
Power.model.MCMC <- function(){
  for (i in 1:N) {                             # loop over N sample points
    R_Obs[i] ~ dlnorm(logR_Fit[i], tau)          # likelihood -> predicted value for NA in data set
    logR_Fit[i] <- A + B * log(S[i])       # power model
    R_Fit[i] <- exp(logR_Fit[i])
    R_Pred[i] ~ dlnorm(logR_Fit[i],tau)
  }
  
  A ~ dnorm(A_mean, A_tau)             # prior for alpha
  B ~ dnorm(B_mean, B_tau)                # prior for beta
  tau ~ dgamma(Sig_Gam_Dist,Sig_Gam_Dist)    # prior for precision parameter
  sigma <- 1/sqrt(tau)   		                  	
  
}

#---------------
# Larkin Model
Larkin.model.MCMC <- function(){
  for(i in 4:N) {               		#loop over N sample points
    logR_Fit[i-3] <- logA + log(S[i])-beta0*S[i]-beta1*S[i-1]-beta2*S[i-2]-beta3*S[i-3]
    R_Obs[i] ~ dlnorm(logR_Fit[i-3], tau)	 # likelihood
    R_Pred[i-3] ~ dlnorm(logR_Fit[i-3],tau)
    R_Fit[i-3] <- exp(logR_Fit[i-3])
  }
  
  logA ~ dnorm(logA_mean,logA_tau)     	# prior for alpha
  beta0 ~ dnorm(B_means[1],B_taus[1])	# prior for beta0  
  beta1 ~ dnorm(B_means[2],B_taus[2])# prior for beta1   
  beta2 ~ dnorm(B_means[3],B_taus[3])# prior for beta2   
  beta3 ~ dnorm(B_means[4],B_taus[4])	# prior for beta3  
  tau ~ dgamma(Sig_Gam_Dist, Sig_Gam_Dist)		# prior for precision parameter
  sigma <- 1/sqrt(tau)		# transformation of precision to sd
  
}

#===================================================================
# sibling model -- this one is a bit different, it's used only to
# predict age-5 return component, and must be combined with another model

Sibling.Model <- function(){
  
  #Priors
  alpha ~ dnorm(0, 0.04)                      
  beta ~ dnorm(0, 0.04)	
  tau ~ dgamma(0.001,0.001) 
  
  for (i in 1:N) {    
    
    ln_age5_obs[i] ~ dnorm(ln_R_Fit[i], tau) #loop over N sample points
    ln_R_Fit[i] <- alpha - beta*ln_age4[i];
    R_Pred[i] ~ dlnorm(ln_R_Fit[i], tau);
    
  }
  
}

# Naive models ======================================================================

Run.Naive.Mods <- function(Data_Retro, Pred_Year){
  
  Results <- data.frame(Year = numeric(), Mod = character(), ModType = character(), 
                        Pred4 = numeric(), Pred5 = numeric())
  
  N_Obs <- dim(Data_Retro)[1]
  P4 <- mean(Data_Retro$rec4/Data_Retro$R, na.rm=T)
  
  #Lognormal MRS - apply mean log-recruits-per-spawner
  logRPS<-log(Data_Retro$R/Data_Retro$S)
  m_p<-mean(logRPS, na.rm=T)
  s_p<-sd(logRPS, na.rm=T)
  #forecast
  lMRS_Est4 <- P4*qlnorm(0.5, meanlog=m_p, sdlog=s_p)*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  lMRS_Est5 <- (1-P4)*qnorm(0.5, m_p, s_p)*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "MRS_Log", ModType = "Naive", Pred4 = lMRS_Est4, Pred5 = lMRS_Est5)
  Results <- bind_rows(Results, new.row)
  
  #Normal MRS - apply mean recruits-per-spawner
  RPS<-Data_Retro$R/Data_Retro$S
  m_p<-mean(RPS, na.rm=T)
  s_p<-sd(RPS, na.rm=T)
  #forecast
  MRS_Est4 <- P4*qnorm(0.5, m_p, s_p)*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  MRS_Est5 <- (1-P4)*qnorm(0.5, m_p, s_p)*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "MRS", ModType = "Naive", Pred4 = MRS_Est4, Pred5 = MRS_Est5)
  Results <- bind_rows(Results, new.row)
  
  #RS1 - use last complete brood's recruits-per-spawner
  RPS_Last <- Data_Retro %>% filter(yr == (Pred_Year-5)) %>% mutate(RPS = R/S) %>% pull(RPS)   #Data_Retro$R[yr-5]/Data_Retro$S[yr-5]
  RS1_Est4 <- P4*RPS_Last*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  RS1_Est5 <- (1-P4)*RPS_Last*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RS1", ModType = "Naive", Pred4 = RS1_Est4 , Pred5 = RS1_Est5)
  Results <- bind_rows(Results, new.row)
  
  #RS2 - use last 2 complete brood's recruits-per-spawner
  #RPS_Last2 <- mean(Data_Retro$R[(Pred_Year-5-1):(Pred_Year-5)]/Data_Retro$S[(Pred_Year-5-1):(Pred_Year-5)])
  RPS_Last2 <- Data_Retro %>% filter(yr %in% c(Pred_Year-5-1, Pred_Year-5)) %>% 
    mutate(RPS = R/S) %>% summarise(meanRPS = mean(RPS)) %>% pull(meanRPS)
  RS2_Est4 <- P4*RPS_Last2*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  RS2_Est5 <- (1-P4)*RPS_Last2*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RS2", ModType = "Naive", Pred4 = RS2_Est4 , Pred5 = RS2_Est5)
  Results <- bind_rows(Results, new.row)
  
  #RS4 - use last 4-year-cycle's recruits-per-spawner
  RPS_Last4 <- Data_Retro %>% filter(yr %in% c((Pred_Year-5-3):(Pred_Year-5))) %>% 
                mutate(RPS = R/S) %>% summarise(meanRPS = mean(RPS)) %>% pull(meanRPS)
  RS4_Est4 <- P4*RPS_Last4*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  RS4_Est5 <- (1-P4)*RPS_Last4*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RS4", ModType = "Naive", Pred4 = RS4_Est4 , Pred5 = RS4_Est5)
  Results <- bind_rows(Results, new.row)
  
  #RS8 - use last 2 4-year-cycle's (8 years) recruits-per-spawner
  RPS_Last8 <- Data_Retro %>% filter(yr %in% c((Pred_Year-5-7):(Pred_Year-5))) %>% 
    mutate(RPS = R/S) %>% summarise(meanRPS = mean(RPS)) %>% pull(meanRPS)
  RS8_Est4 <- P4*RPS_Last8*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  RS8_Est5 <- (1-P4)*RPS_Last8*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RS8", ModType = "Naive", Pred4 = RS8_Est4 , Pred5 = RS4_Est5)
  Results <- bind_rows(Results, new.row)
  
  # RSC - same but along cycle line
  # age 4 cycle line - will need to start 8 years ago, since 4 years ago brood won't be complete yet
  Yrs4 <- seq(Pred_Year-8, min(Data_Retro$yr), -4)
  # can start 9 years ago, since 5 year ago brood won't be complete
  Yrs5 <- seq(Pred_Year-9, min(Data_Retro$yr), -4)
  RPS_4 <- mean(Data_Retro$R[Data_Retro$yr %in% Yrs4]/Data_Retro$S[Data_Retro$yr %in% Yrs4])
  RPS_5 <- mean(Data_Retro$R[Data_Retro$yr %in% Yrs5]/Data_Retro$S[Data_Retro$yr %in% Yrs5])
  RSC_Est4 <- P4*RPS_4*Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  RSC_Est5 <- (1-P4)*RPS_5*Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RSC", ModType = "Naive", Pred4 = RSC_Est4 , Pred5 = RSC_Est5)
  Results <- bind_rows(Results, new.row)
 
  Results 
}

#====================================================================================

# Some example performance metrics used in retrospective (one-step-ahead)

# Mean Raw Error
MRE <- function(obs, pred){
  sum(pred-obs, na.rm=TRUE)/length(obs)
}

# Mean Absolute Error
MAE <- function(obs, pred){
  sum(abs(pred-obs), na.rm=TRUE)/length(obs)
}

# Mean Proportional error
MPE <- function(obs, pred){
  sum((pred-obs)/obs, na.rm=TRUE)/length(obs)
}

# Root Mean square error
RMSE <- function(obs, pred){
  sqrt(sum((pred-obs)^2, na.rm=TRUE)/length(obs))
}

# Mean Absolute percent error
MAPE <- function(obs, pred){
  sum(abs(pred-obs)/obs, na.rm=TRUE)/length(obs)
}

# Mean arctangent absolute percentage error
MAAPE <- function(obs, pred){
  sum(atan(abs(obs-pred)/obs), na.rm = TRUE)/length(obs)
}

# Mean absolute scaled error - not suitable for cyclic
MASE <- function(obs, pred){
  Diff <- NULL
  for(i in 1:(length(obs)-1)){
    Diff[i] <- abs(obs[i+1] - obs[i])
  }
  Denom <- sum(Diff)/(length(obs)-1)
  (sum(abs(pred-obs), na.rm=TRUE)/length(obs)) / Denom
}

