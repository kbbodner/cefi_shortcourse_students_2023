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


#created helper variables/added new column for naive predictions
obs.2021 <- c(data$rec4[data$yr == pred.year-4], data$rec5[data$yr == pred.year-5])
baseline.pred <- c(baseline.mod$Preds_Out$Pred4, baseline.mod$Preds_Out$Pred5)

naive.mod.ext <- naive.mod |>
  mutate(R_pred = Pred4 + Pred5)

###Run the performance on all models:

#Baseline
perf.Ricker <- run.all.performances(sum(obs.2021), sum(baseline.pred),
                                    performance.functions)

#Naive models
perf.naive <- list()
for(i in 1:nrow(perf.Naive)) {
  perf.naive[[i]] <- run.all.performances(sum(obs.2021), naive.mod.ext$R_pred[i],
                                          performance.functions)
}
names(perf.naive) <- naive.mod.ext$Mod


