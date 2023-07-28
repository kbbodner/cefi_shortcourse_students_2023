# Script to determine the performance of various model predictions 
#   according to different metrics
#

#function that carries out all the performance metrics for all functions in @param funs
run.all.performances <- function(obs, pred, funs){
  perf <- data.frame(metric=names(funs))
  perf$performance <-  mapply(do.call, funs, list(list(obs, pred)))
  perf
}

#Create a list of all the performance metrics we can use
performance.functions <- list("MRE" = MRE, 
                              "MAE" = MAE,
                              "MPE" = MPE,
                              "RMSE" = RMSE,
                              "MAPE" = MAPE, 
                              "MAAPE" = MAAPE,
                              "MASE" = MASE)

#Observations
observ <- c()
for(y in baseline.mod.all$Pred_Year) {
  observ <- c(observ, data$rec4[data$yr==(y-4)] + data$rec5[data$yr==(y-5)])
}

###Run the performance on all models:

#Baseline
perf.baseline <- run.all.performances(observ, baseline.mod.all$R_Pred,
                                      performance.functions)

#Baseline + covariate
perf.ricker.env <- run.all.performances(observ, ricker.env.mod.all$R_Pred,
                                      performance.functions)

#Power
perf.power <- run.all.performances(observ, power.mod.all$R_Pred,
                                    performance.functions)

#Naive models
perf.naive <- list()

for(n in naive.mod.all[[1]]$Mod) {
  predictions <- c()
  for(i in 1:length(naive.mod.all)) {
    predictions <- c(predictions, naive.mod.all[[i]]$R_Pred[naive.mod.all[[i]]$Mod == n])
  }
  perf.naive[[length(perf.naive) + 1]] <- run.all.performances(observ , predictions,
                                                               performance.functions)
}
names(perf.naive) <- naive.mod$Mod

#Linear naive model

perf.naive.LM <- list()

for(n in naive.mod.LM[[1]]$Mod) {
  predictions <- c()
  for(i in 1:length(naive.mod.LM)) {
    predictions <- c(predictions, naive.mod.LM[[i]]$Pred45[naive.mod.LM[[i]]$Mod == n])
  }
  perf.naive.LM[[length(perf.naive.LM) + 1]] <- run.all.performances(observ , predictions,
                                                               performance.functions)
}
names(perf.naive) <- naive.mod$Mod

####TODO: Fix everything below

# best naive model:

# sort model performance for naive model
perf.naive.clean <- data.table::rbindlist(perf.naive, idcol = TRUE)

best.naive.model <- perf.naive.clean |> 
  filter(metric == "RMSE") |> 
  arrange(desc(performance)) |> 
  slice_min(performance)

#Compare with other models:
baseline.RMSE <- perf.baseline |>
  filter(metric == "RMSE")

ricker.env.RMSE <- perf.ricker.env |>
  filter(metric == "RMSE")

power.RMSE <- perf.power |>
  filter(metric == "RMSE")



