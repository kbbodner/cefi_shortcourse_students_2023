# Runs all kinds of naive models
# @naive.mod.all is the model output

naive.mod.all <- list()

#Run the naive models
for(i in 10:nrow(data)) {
  naive.mod.all[[i]] <- naive.mod <- Run.Naive.Mods(data, data$yr[i])
  naive.mod.all[[i]] <- naive.mod.all[[i]] |>
    mutate(R_Pred= Pred4 + Pred5)
}
