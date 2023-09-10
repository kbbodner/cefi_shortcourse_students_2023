# Ricker with Env Covariate
library("rstudioapi")
library(here)
#setwd(dirname(getActiveDocumentContext()$path))
source(here("./CaseStudy_Fisheries/team-2/R/03d_multiple_model.R"))
library(dplyr)


#Define one environmental covariate: jnesst
newvar <- c("Pop_Name", "yr", "rec4", "rec5", "R", "S", "jnesst")
data.envcov <- data %>% select(all_of(newvar)) %>% rename(envcov = jnesst) %>% as.data.frame()

#Run the Ricker Env model
ricker.env.mod.all <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(ricker.env.mod.all) <- c("Pred_Year", "Mod", "ModType", "Pred4", "Pred5")

#
for(y in  data$yr[8]:2021) {
  ricker.env.mod.all[nrow(ricker.env.mod.all) + 1,] <- 
    RunModRetro.new(data.envcov, y, method = "Ricker")$Preds_Out
}

ricker.env.mod.all <- ricker.env.mod.all |>
  mutate(R_Pred=Pred4+Pred5)
