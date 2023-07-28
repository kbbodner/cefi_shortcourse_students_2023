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
  ggtitle("Yearly returns for Early Stuart")

ggplot(data.obs) +
  geom_line(aes(x=yr, y=R_obs))+
  geom_ribbon(data=forec, aes(yr, ymax=for_CI_up, ymin=for_CI_low), fill="red", alpha=0.35) +
  geom_line(data=forec, aes(x=yr, y=forecast), col="blue", size=1.1, linetype = "solid") +
  ylab("Returns") +
  xlab("Year") +
  ggtitle("Yearly returns for Early Stuart")

ggplot(data.obs[60:69,]) +
  geom_line(aes(x=yr, y=R_obs))+
  geom_ribbon(data=forec, aes(yr, ymax=for_CI_up, ymin=for_CI_low), fill="red", alpha=0.35) +
  geom_line(data=forec, aes(x=yr, y=forecast), col="blue", size=0.8, linetype = "solid") +
  ylab("Returns") +
  xlab("Year") +
  ggtitle("Yearly returns for Early Stuart")
  
  

