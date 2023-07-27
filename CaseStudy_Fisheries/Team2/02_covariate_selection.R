# covariate selection

rm(list=ls())
gc()

# Packages

require(dplyr)
library(Hmisc)
library(corrplot)



# data df comes from 01_preparation script
# Delete yeas without response R
data <- data[!is.na(data$R),]


# **#*****************************************************************
#***  importance covariates according to pearson correlation   **
#*# *******************************************************************

matCor <-  cor(data[,5:18] )
ploCor <- corrplot(matCor, method = 'color')
corrplot(matCor, method = 'number')

print("Best covariates according to pearson correlation: ")
print(paste0("- aflow (Average monthly discharge (m3/s)", 
"/n - peak(Peak annual discharge(m3/s)",
"/n - jnesst(june Sea surface temperature (C)), and",
           "/n  pdo(Pacific Decadal Oscillation)"))

# **#*****************************************************************
#***  importance covariates according to lm(RR ~ covariate)   **
#*# *******************************************************************


coVar <- as.list(names(data)[6:18])

# lm for all covariates
allModelsList <- lapply(paste("R ~", coVar), as.formula)
allModelsResults <- lapply(allModelsList, function(x) lm(x, data = data)) 
allModelsSummaries = lapply(allModelsResults, summary) 


# Vectors to keeps results

coVa <- names(data)[6:18]
r2V <- c()
fV <- c()
pV <- c()

for(co in 1:length(coVar)){    #  co <- 1
  r2V <- c(r2V,allModelsSummaries[[co]][[9]] )
  fV <- c(fV,as.numeric(allModelsSummaries[[co]][[10]][1] ))
  pV <- c(pV,allModelsSummaries[[co]][[4]][8] )
  print(allModelsSummaries[[co]][[9]])
  print(allModelsSummaries[[co]][[10]][1])
  print(allModelsSummaries[[co]][[4]][8])
  print("@----------------")
}

resLM_TestCov <- data.frame(coVa, r2V, fV,pV)
names(resLM_TestCov) <- c("Covariate", 'Rsquared', "Fstatist", "pvalue")
resLM_TestCov$corrR <- matCor[1,2:14]
resLM_TestCov <- resLM_TestCov[order(resLM_TestCov$Rsquared, decreasing = TRUE),]
resLM_TestCov$rankR2 <- seq(1:nrow(resLM_TestCov))
resLM_TestCov <- resLM_TestCov[order(abs(resLM_TestCov$corrR), decreasing = TRUE),]
resLM_TestCov$rankCorrPear <- seq(1:nrow(resLM_TestCov))

# write.csv(resLM, 'resultsLM_cov.csv', row.names = FALSE)
dfBestCov <- resLM_TestCov[1:5,]  ####  best 5 (4+1 covariates)
bestCov <- as.vector(dfBestCov$Covariate)

# **#*****************************************************************
#***  Micro matrix correlation with best covariates   **
#*# *******************************************************************

matCorBC <-  cor(dfVar[,c("R", bestCov)] )
ploCor <- corrplot(matCorBC, method = 'color')
corrplot(matCorBC, method = 'number')

###we do not keep "pd0" car less influent covariate and moderatelu correlated with peak
# and jnesst so we keep peak, aflow, jnesst
finalBestCov <- bestCov[!bestCov =="pdo"]
finalBestCov


### average sea surface temperatures by station
# stations denoted by _e or _p 
# _e refers to entrance island lighthouse and is an average over april to june
#_p refers to Pine island lighthouse and is an average over april to july

data <- data %>% 
  filter(Pop_Name ==  "Early Stuart" & yr < 2020)  |> 
  group_by(yr) |> 
  mutate(avg_sst_e = mean(apesst:jnesst, na.rm = TRUE),
         avg_sst_p = mean(appsst:jlpsst, na.rm = TRUE))


