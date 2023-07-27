# Runs the baseline ricker model
# @baseline.mod.all is the model output

#Run the baseline Ricker model
baseline.mod.all <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(baseline.mod.all) <- c("Pred_Year", "Mod", "ModType", "Pred4", "Pred5")

#
for(y in  data$yr[8]:2021) {
  baseline.mod.all[nrow(baseline.mod.all) + 1,] <- RunModRetro(data, y)$Preds_Out
}

baseline.mod.all <- baseline.mod.all |>
  mutate(R_Pred=Pred4+Pred5)
