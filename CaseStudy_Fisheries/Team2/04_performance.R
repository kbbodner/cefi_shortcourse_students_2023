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

#TODO: implement performance on all models
