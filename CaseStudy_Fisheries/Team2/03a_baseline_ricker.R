# Runs the baseline ricker model
# @baseline.mod is the model output

#Define prediction year
pred.year <- 2021

#Run the baseline Ricker model
baseline.mod <- RunModRetro(data, pred.year)
