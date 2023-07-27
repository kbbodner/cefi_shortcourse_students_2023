# Ricker with Env Covariate
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))
source("03d_multiple_model.R")
library(dplyr)

#Define prediction year
pred.year <- 2021

#Define one environmental covariate: jnesst

#Run the Ricker Env model
newvar <- c("Pop_Name", "yr", "rec4", "rec5", "R", "S", "aflow")
data.envcov <- data %>% select(all_of(newvar)) %>% rename(envcov = aflow) %>% as.data.frame()
ricker.env.mod <- RunModRetro.new(Dat=data.envcov, Pred_Year=pred.year, method = "Ricker") 
ricker.env.mod$Preds_Out

