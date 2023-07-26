# explore some stuff

dat <- DataIn




# explore proportion year 4 vs year 5 -------------------------------------

dat %>% glimpse()


dat %>%
  mutate(prop4 = rec4/(R)) %>%
  ggplot(aes(x=yr, y = prop4, color = Pop_Name)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~Pop_Name) +
  theme_bw() +
  theme(legend.position = "none")

dat %>%
  mutate(R_prop = R/S) %>%
  ggplot(aes(x=yr, y=R_prop, color = Pop_Name)) +
  geom_line() +
  facet_wrap(~Pop_Name) 



# evaluate model ----------------------------------------------------------


# This function is still pretty crude -- can you fix it up a bit to give estimates with uncertainty around them?


# ok, we want to use RunModRetro to assess the last 10 years of data
last_10_pred <- list()

for(i in 2006:2021){
  pred <-  RunModRetro(Dat=ESData, Pred_Year=i)
  last_10_pred[[paste0("year_",i)]] <- pred$Preds_Out
}

pred_df <-  as.data.frame(do.call(rbind, last_10_pred))

pred_df <- pred_df %>% mutate(yr = stringr::str_remove(rownames(.),"year_"))

ESData %>%
  ggplot(aes(x=yr)) +
  geom_line(aes(y=rec4), color = "darkmagenta") +
  geom_line(aes(y=rec5), color = "transparent")  +
  geom_line(data = pred_df, aes(x=Pred_Year,
                                y = Pred4),
            color = "darkmagenta", linetype = "dashed") +
  
  geom_line(data = pred_df, aes(x=Pred_Year,
                                y = Pred5),
            color = "transparent", linetype = "dashed")+
  scale_y_log10() +
  theme_bw() +
  labs(x = "Year",
       y = "Recruits")

ESData %>%
  ggplot(aes(x=S, y=R)) +
  geom_point() +s
geom_abline(slope = 1)
abline(slope = 1)


ESData %>%
  ggplot(aes(x=S,y=R)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10()

ESData <- ESData %>%
  mutate(logS = log(S),
         logR = log(R))

ESData %>%
  ggplot(aes(x = logS, y = logR)) +
  geom_point() +
  geom_smooth(method = "lm")

mod <- lm( logR ~ logS ,
   data = ESData)

summary(mod)


env <- read.csv(here::here("CaseStudy_Fisheries","DataIn",
                           "FC_Environmental_Data.csv"))
env %>%
  mutate()




# run with new functions --------------------------------------------------

  pred <-  RunModRetro(Dat=ESData, Pred_Year=i)
  last_10_pred[[paste0("year_",i)]] <- pred$Preds_Out

  
  RunModRetro_new(Dat = ESData, 
                  Pred_Year = 2011,
                  Model = "")