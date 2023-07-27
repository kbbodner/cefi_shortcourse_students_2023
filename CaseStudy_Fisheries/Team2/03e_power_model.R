# Runs the Power model
# @power.mod is the model output

#Define prediction year
pred.year <- 2021

#Run the baseline Ricker model
power.mod <- RunModRetro.new(data, 2021, "Power")
