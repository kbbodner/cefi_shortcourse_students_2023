# unclean - power forecast

Dat = data 
Pred_Year=2023
library(tidyverse)

# for year X forecast, would have return data up to year X-1
# age-4 return (rec4) up to brood year X-5, age-5 return (rec5) up to brood year X-6

Data_Retro <- Dat %>% filter(yr <= (Pred_Year-4))
Data_Retro[Data_Retro$yr == (Pred_Year-5), c("rec5", "R")] <- NA
Data_Retro[Data_Retro$yr == (Pred_Year-4), c("rec4", "rec5", "R")] <- NA

# Want to scale down obs to make models more stable
# only really required for TMB, but will use for all types
Scale <- 10^(floor(log(mean(Data_Retro$R, na.rm=T), 10))) # most numbers should be below 10

# Set up data and starting value lists list to go into model
data2 <- list()
data2$R_Obs <- Data_Retro$R/Scale
data2$N <- dim(Data_Retro)[1]

data2$A_mean <- 1
data2$A_tau <- 1
data2$B_mean <- 1
data2$B_tau <- 1
data2$Sig_Gam_Dist <- 0.001
data2$S <- Data_Retro$S/Scale 
data2$P4 <- mean(Data_Retro$rec4 / Data_Retro$R, na.rm=T)

# define power model formula
power <- "
model{
A ~ dnorm(A_mean, A_tau)             # prior for alpha
B ~ dnorm(B_mean, B_tau)                # prior for beta
tau ~ dgamma(Sig_Gam_Dist,Sig_Gam_Dist)    # prior for precision parameter
sigma <- 1/sqrt(tau)


for (i in 1:N) {                             # loop over N sample points
      R_Obs[i] ~ dlnorm(logR_Fit[i], tau)          # likelihood -> predicted value for NA in data set
      logR_Fit[i] <- A + B * log(S[i])       # power model
      R_Fit[i] <- exp(logR_Fit[i])
      R_Pred[i] ~ dlnorm(logR_Fit[i],tau)
}
R_Pred_Tot <- R_Pred[N] * P4 + R_Pred[N-1] * (1-P4)
}
"


# run jags model
j.model2   <- jags.model(file = textConnection(power),
                         data = data2,
                         n.chains = 3)

# resample from the posterior
var.out2 <- coda.samples (model = j.model2,
                          variable.names = c("A", "B","tau", "R_Pred_Tot"),
                          n.iter = 5000)

# check data frame
head(var.out2)


## assess convergence and remove burn-in before doing other diagnostics
GBR <- gelman.plot(var.out2)

## convert to matrix
var.mat2      <- as.data.frame(var.out2[[3]])
var.mat.scale <-  var.mat2 |>
  mutate(R_Pred_Tot=R_Pred_Tot * Scale)
  
head(var.mat.scale) 
dim(var.mat.scale)
hist(var.mat.scale$R_Pred_Tot, breaks=50)
median(var.mat.scale$R_Pred_Tot)


#Is this how we get the confi interval?
power.mod.2023 <- RunModRetro.new(data, 2023, "Power")
power.mod.2023$Preds_Out

####
hist(var.mat.scale$R_Pred_Tot, breaks=1000)
median(var.mat.scale$R_Pred_Tot)

sorted_pred <- sort(var.mat.scale$R_Pred_Tot)
ci_p <- c(0.025, 0.975)
posit <- (ci_p * length(var.mat.scale$R_Pred_Tot))

sorted_pred[posit]


# CI/PI test 2
## Pairwise scatter plots & correlation
pairs(var.mat2)	## pairs plot to evaluate parameter correlation
cor(var.mat2)  

nsamp <- 5000
samp <- sample.int(nrow(var.mat2),nsamp)
xpred <- 2022 					## sequence of x values we're going to
npred <- length(xpred)				##      make predictions for
ypred <- matrix(0.0,nrow=nsamp,ncol=npred)	## storage for predictive interval
ycred <- matrix(0.0,nrow=nsamp,ncol=npred)	## storage for credible interval


for(g in seq_len(nsamp)){
  theta = var.mat2[samp[g],]
  ycred[g,] <- theta["A"] + theta["B"]*xpred
  ypred[g,] <- rnorm(npred,ycred[g,],1/sqrt(theta["tau"]))
}

ci <- apply(ycred,2,quantile,c(0.025,0.5,0.975))  ## credible interval and median
pi <- apply(ypred,2,quantile,c(0.025,0.975))

j.model2$R_Pred
summary(var.out2)$statistics
plot(Year,  )