forec <- data.frame(yr=c(2022,2023),
                    forecast= c(243951,median(var.mat.scale$R_Pred_Tot)),
                    for_CI_low=c(243951,conf.interv[1]),
                    for_CI_up=c(243951,conf.interv[2]))

forec.bar <- forec |>
  filter(yr==2023)

data.obs <- data.frame(yr = 1955:2022)
                      

R_obs <- c()
k1 <- which(data$yr == 1955)
k2 <- which(data$yr == 2020) + 2

for(k in k1:k2) {
  print(k)
  R_obs <- c(R_obs, data$rec4[k-4] + data$rec5[k-5])
}
data.obs$R_obs <- R_obs
data.obs$R_obs[nrow(data.obs)] <- 239542	+ 4409

ggplot(data.obs) +
  geom_line(aes(x=yr, y=R_obs))+
   ylab("Returns") +
  xlab("Year") +
  ggtitle("Yearly returns for Early Stuart") +
  theme_classic()
ggsave(here("./CaseStudy_Fisheries/team-2/Presentation/timeseries.png"))

ggplot(data.obs) +
  geom_line(aes(x=yr, y=R_obs))+
  geom_ribbon(data=forec, aes(yr, ymax=for_CI_up, ymin=for_CI_low), fill="red", alpha=0.35) +
  geom_line(data=forec, aes(x=yr, y=forecast), col="blue", size=1.1, linetype = "solid") +
  ylab("Returns") +
  xlab("Year") +
  ggtitle("Yearly returns for Early Stuart") +
  theme_classic()
ggsave(here("./CaseStudy_Fisheries/team-2/Presentation/timeseries+pred.png"))

ggplot(data.obs[60:69,]) +
  geom_line(aes(x=yr, y=R_obs))+
  geom_ribbon(data=forec, aes(yr, ymax=for_CI_up, ymin=for_CI_low), fill="red", alpha=0.35) +
  geom_line(data=forec, aes(x=yr, y=forecast), col="blue", size=0.8, linetype = "solid") +
  ylab("Returns") +
  xlab("Year") +
  ggtitle("Yearly returns for Early Stuart") +
  theme_classic()
ggsave(here("./CaseStudy_Fisheries/team-2/Presentation/timeseries+pred+zoom.png"))
  
  
#performance plots
best.naive.lm.model <- best.naive.lm.model |> 
  rename(model = ".id") |> 
  as.data.frame()  |> 
  mutate(model = "Naive LM")

baseline.RMSE <- baseline.RMSE |> 
  mutate(model = rep("Baseline_Ricker"))|> 
  relocate(model, .before = metric)

power.RMSE <- power.RMSE |>
  mutate(model = rep("Power")) |> 
  relocate(model, .before = metric) 



best.naive.model <- best.naive.model |> 
  rename(model = ".id") |> as.data.frame()  |> 
  mutate(model = "Naive")

ricker.env.RMSE <- ricker.env.RMSE |> 
  mutate(model = rep("Env_Ricker"))|> 
  relocate(model, .before = metric) 


mod.performance <- rbind(baseline.RMSE, power.RMSE, best.naive.model, best.naive.lm.model, ricker.env.RMSE )

# plot performance values
mod.performance |> 
  ggplot() + 
  geom_col(aes(x = reorder(model, -performance), y = performance)) +
  ggtitle("Predictive Performance") + 
  ylab("Difference between Predicted and Observed") +
  xlab("Model Type")+ theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) 

