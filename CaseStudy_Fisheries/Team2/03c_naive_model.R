# Runs all kinds of naive models
# @naive.mod.all is the model output

naive.mod.all <- list()

#Run the naive models
for(y in data$yr[10]:2021) {
  naive.mod.all[[length(naive.mod.all) + 1]] <- Run.Naive.Mods(data, y)
  naive.mod.all[[length(naive.mod.all)]] <- naive.mod.all[[length(naive.mod.all)]] |>
    mutate(R_Pred= Pred4 + Pred5)
}
