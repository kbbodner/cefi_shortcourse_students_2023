# Runs all kinds of naive models
# @naive.mod is the model output

#Define prediction year
pred.year <- 2021

#Run the baseline Ricker model
naive.mod <- Run.Naive.Mods(data, pred.year)
