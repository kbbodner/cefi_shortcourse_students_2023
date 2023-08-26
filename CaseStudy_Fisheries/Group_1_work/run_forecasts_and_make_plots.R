# script to run 3 forecasted models to predict salmon recruits in 2022,
# check model fits, and run models for 2023

# SORRY, the code is not that efficient, but it works. 


# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(R2jags)

# global theme and color palette
theme_set(ggthemes::theme_few())
pal <- wesanderson::wes_palette("Zissou1")[c(1,4,5)]



# upload data -------------------------------------------------------------

# full dataset
data <- read.csv(here::here("CaseStudy_Fisheries",
                            "DataIn","FraserSockeyeData2022.csv"))
# subset to Early Stuart
ESData <- data %>%
  filter(Pop_Name == "Early Stuart")

# add column for true R 
# (sum of y4 recruits from T-4 and y5 recruits from T-5 )
ESData <- ESData %>% 
  mutate(True_R = lag(rec4,4) + lag(rec5,5)) %>%
  # add column for 2021 bc lag didn't calculate it
  rbind(data.frame(Pop_Name = "Early Stuart",
                   yr = 2021,
                   rec4 = NA_integer_,
                   rec5 = NA_integer_,
                   R = NA_integer_,
                   S = NA_integer_, 
                   True_R = 77923 +  154)) 

# remove full dataset
rm(data)


# upload environmental data
env <- read.csv(here::here("CaseStudy_Fisheries","DataIn",
                           "FC_Environmental_Data.csv"))

# get a vector of average summer sst
cov_sst <- env %>% 
  mutate(sst_avg = (apesst + maesst + jnesst)/3) %>%
  pull(sst_avg)

# also plot summer sst for good measure
env %>% 
  mutate(sst_avg = (apesst + maesst + jnesst)/3) %>%
  ggplot(aes(x=yr, y = sst_avg)) +
  geom_line() +
  coord_cartesian(xlim = c(2000,2025))
  
rm(env)
  


# upload functions --------------------------------------------------------
source(here::here("CaseStudy_Fisheries","Group_1_work","Function_new.R"))
source(here::here("CaseStudy_Fisheries","Code","Functions.R"))




# try in a loop -----------------------------------------------------------

# create empty list to store values in
last_10_pred_ricker <- list()

# predict values for years 2011-2022
for(i in 2011:2022){
  
  pred <-  RunModRetro_new(Dat=ESData, Pred_Year=i, Model = "Ricker")
  last_10_pred_ricker[[paste0("year_",i)]] <- pred$Preds_Out
  
}


# convert to dataframe and manually add a column for 2010 only to easily 
# plot a ribbon plot to show the origin of our error shadow
pred_df_ricker <- as.data.frame(do.call(rbind, last_10_pred_ricker)) %>%
  rbind(c(Pred_Year = 2010,
          Mod = NA,
          ModType = NA,
          Pred4 = NA,
          Pred5 = NA,
          Pred = ESData[ESData$yr == 2010,"True_R"],
          Pred_low = ESData[ESData$yr == 2010,"True_R"],
          Pred_up = ESData[ESData$yr == 2010,"True_R"]
  ))


# plot true values + 2011-2022 predictions
output_ricker <- ggplot(data = ESData[ESData$yr <= 2021,]) +
  geom_line(aes(x = yr, y = True_R)) +
  geom_ribbon(data = pred_df_ricker[pred_df_ricker$Pred_Year<2022,],
              aes(x = Pred_Year,
                  ymin = Pred_low,
                  ymax = Pred_up),
              fill = pal[1],
              color = "transparent",
              alpha = .25) +
  geom_line(data = pred_df_ricker[pred_df_ricker$Pred_Year<2022,],
            aes(x=Pred_Year,
                y = Pred),
            color = pal[1]) +
  labs(x = NULL,
       y = "Returns",
       title = "Ricker Model") +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(xlim = c(1952, 2023))

output_ricker


# repeat for power model --------------------------------------------------

# create empty list
last_10_pred_power <- list()

# predict for 2011:2022
for(i in 2011:2022){
  
  pred <-  RunModRetro_new(Dat=ESData, Pred_Year=i, Model = "Power")
  last_10_pred_power[[paste0("year_",i)]] <- pred$Preds_Out
  
}


pred_df_power <-  as.data.frame(do.call(rbind, last_10_pred_power)) %>%
  rbind(c(Pred_Year = 2010,
          Mod = NA,
          ModType = NA,
          Pred4 = NA,
          Pred5 = NA,
          Pred = ESData[ESData$yr == 2010,"True_R"],
          Pred_low = ESData[ESData$yr == 2010,"True_R"],
          Pred_up = ESData[ESData$yr == 2010,"True_R"]
  ))


output_power <- ggplot(data = ESData[ESData$yr <= 2021,]) +
  geom_line(aes(x = yr, y = True_R)) +
  geom_ribbon(data = pred_df_power[pred_df_power$Pred_Year<2022,],
              aes(x = Pred_Year,
                  ymin = Pred_low,
                  ymax = Pred_up),
              fill = pal[2],
              color = "transparent",
              alpha = .25) +
  geom_line(data = pred_df_power[pred_df_power$Pred_Year<2022,],
            aes(x=Pred_Year,
                y = Pred),
            color = pal[2]) +
  labs(x = NULL,
       y = "Returns",
       title = "Power Model") +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(xlim = c(1952, 2023))

output_power


# repeat for power model --------------------------------------------------

last_10_pred_powercov <- list()
for(i in 2011:2022){
  
  pred <-  RunModRetro_new(Dat=ESData, Pred_Year=i, Model = "PowerCov", Cov = cov_sst)
  last_10_pred_powercov[[paste0("year_",i)]] <- pred$Preds_Out
  
}


pred_df_power_cov <-  as.data.frame(do.call(rbind, last_10_pred_powercov)) %>%
  rbind(c(Pred_Year = 2010,
          Mod = NA,
          ModType = NA,
          Pred4 = NA,
          Pred5 = NA,
          Pred = ESData[ESData$yr == 2010,"R"],
          Pred_low = ESData[ESData$yr == 2010,"R"],
          Pred_up = ESData[ESData$yr == 2010,"R"]
  ))


output_power_cov <- ggplot(data = ESData[ESData$yr <= 2021,]) +
  geom_line(aes(x = yr, y = True_R)) +
  geom_ribbon(data = pred_df_power_cov[pred_df_power_cov$Pred_Year<2022,],
              aes(x = Pred_Year,
                  ymin = Pred_low,
                  ymax = Pred_up),
              fill = pal[3],
              color = "transparent",
              alpha = .25) +
  geom_line(data = pred_df_power_cov[pred_df_power_cov$Pred_Year<2022,],
            aes(x=Pred_Year,
                y = Pred),
            color = pal[3]) +
  labs(x = NULL,
       y = "Returns",
       title = "Power & Temperature Model") +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(xlim = c(1952, 2023))

output_power_cov
  
  
library(patchwork)
full_output_plot <- output_ricker / output_power / output_power_cov

ggview::ggview(full_output_plot,
               width = 6, height = 5,
               unit=  "in")

ggsave(full_output_plot,
       width = 6, height = 5,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","model_fits_plot.png"))




# summarize error of all models -------------------------------------------

# format predicted values and join with original dataset values - RICKER
ricker_final_pred <- 
  pred_df_ricker %>%
  rename(yr = Pred_Year) %>%
  select(yr, Mod, Pred_low, Pred, Pred_up) %>%
  left_join(ESData %>% select(yr, True_R))

# format predicted values and join with original dataset values - POWER
power_final_pred <- 
  pred_df_power %>%
  rename(yr = Pred_Year) %>%
  select(yr, Mod, Pred_low, Pred, Pred_up) %>%
  left_join(ESData %>% select(yr, True_R))
  
# format predicted values and join with original dataset values - POWERCOV
powercov_final_pred <- 
  pred_df_power_cov %>%
  rename(yr = Pred_Year) %>%
  select(yr, Mod, Pred_low, Pred, Pred_up) %>%
  left_join(ESData %>% select(yr, True_R))


# bind formatted datasets per model together
final <- rbind(ricker_final_pred, power_final_pred, powercov_final_pred) %>%
  arrange(Mod,yr)

rm(ricker_final_pred, power_final_pred, powercov_final_pred)

final <- final %>%
  # calculate values of predicted - true value 
  # (to test how far off predictions were)
  mutate(offset =  Pred - True_R,
         offset_up = Pred_up - True_R,
         offset_low = Pred_low - True_R,
         abs_offset = abs(Pred - True_R),
         percent_offset = abs(Pred - True_R)/True_R * 100) %>%
  
  # change model names and order
  mutate(Mod = recode(Mod,
                      "Power" = "Power Model",
                      "Ricker" = "Ricker Model",
                      "PowerCov" = "Power & Temperature Model")) %>%
  mutate(Mod = factor(Mod,
                         levels = c("Ricker Model","Power Model","Power & Temperature Model")))
  

offset_plot <- final %>%
  tidyr::drop_na(Mod) %>%
  ggplot(aes(x=yr, y = offset, color = Mod, fill = Mod)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_line() +
  geom_area(size = 1, alpha = .2, position = "jitter") +
  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  
  
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank(),
        plot.title.position = "plot") +
  labs(x = NULL,
       y = "Returns",
       title = "Offset from Real Value",
       color = NULL, 
       fill = NULL)

ggview::ggview(offset_plot,
               width = 6,
               height = 3.5,
               unit = "in")

ggsave(offset_plot,
       width = 6, height = 3.5,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","predicted_real_offset_plot.png"))


# save out model predictions 
write.csv(final,
          row.names = F, 
          file = here::here("CaseStudy_Fisheries",
                            "Group_1_work",
                            "model_predictions_2011_2023.csv"))

range(final$offset, na.rm=T)

final_sum <- final %>%
  group_by(Mod) %>% 
  tidyr::drop_na(Mod, offset) %>% 
  summarise(mean_offset = mean(offset),
            mean_abs_offset = mean(abs_offset),
            mean_percent_offset = mean(percent_offset)) %>%
  tidyr::pivot_longer(cols = c(mean_offset, mean_abs_offset, mean_percent_offset),
                      names_to = "Metric",
                      values_to = "Value") 

model_perf <- final_sum %>%
  group_by(Metric) %>%
  arrange(Value) %>%
  mutate(Metric = case_when(Metric == "mean_abs_offset" ~ "Mean Absolute Offset",
                            Metric == "mean_offset" ~ "Mean Offset",
                            Metric == "mean_percent_offset" ~ "Percent Offset")) %>%
  ggplot(aes(x = Mod, y = Value, color = Mod)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = Mod, yend = 0 ), size = 1.5) +
  facet_wrap(~Metric, scales = "free_y") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted",size = .25),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  labs(x = NULL,
       y = NULL,
       color = "Model",
       title = "Model Offsets from True Values") +
  scale_color_manual(values = pal) +
  theme(strip.clip = "off",
        strip.background = element_blank()) +
  coord_cartesian(clip = "off")

ggview::ggview(model_perf,
               width = 6,
               height = 4,
               unit = "in")


ggsave(model_perf,
       width = 6, height = 4,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","model_performances.png"))

final %>%
  tidyr::drop_na(Mod, offset) %>%
  mutate(outside_error = case_when(
    True_R > Pred_low & True_R < Pred_up ~ 0,
    TRUE ~ 1
  )) %>%
  group_by(Mod) %>%
  summarize(sum = sum(outside_error))
  
rm(last_10_pred_power, last_10_pred_powercov, last_10_pred_ricker,
   model_perf, ESData, full_output_plot, offset_plot, output_power, 
   output_power_cov, output_ricker, pred_df_power, 
   pred_df_power_cov, pred_df_ricker,
   final_sum, pred)

# forecast for 2022 AND CHECK-------------------------------------------------------

# upload 2023 data
data23 <- read.csv(here::here("CaseStudy_Fisheries",
                            "DataIn","FraserSockeyeData2023_Wednesday.csv"))
# subset to Early Stuart
ESData23 <- data23 %>%
  filter(Pop_Name == "Early Stuart")

# add column for true R
ESData23 <- ESData23 %>% 
  mutate(True_R = lag(rec4,4) + lag(rec5,5)) %>%
  # add rows for 2021 and 2022 bc lag doesnt add extra rows 
  rbind(data.frame(Pop_Name = "Early Stuart",
                   yr = 2021,
                   rec4 = NA_integer_,
                   rec5 = NA_integer_,
                   R = NA_integer_,
                   S = NA_integer_, 
                   True_R = 77923 +  154)) %>%
  rbind(data.frame(Pop_Name = "Early Stuart",
                   yr = 2022,
                   rec4 = NA_integer_,
                   rec5 = NA_integer_,
                   R = NA_integer_,
                   S = NA_integer_, 
                   True_R = 239542 +  4409))
rm(data23)


ribbon_data <- final[final$yr %in% c(2021,2022),] %>%
  mutate(Pred_up = case_when(yr == 2021 ~ Pred,
                             TRUE ~ Pred_up),
         Pred_low = case_when(yr == 2021 ~ Pred,
                             TRUE ~ Pred_low)
         ) %>%
  mutate(Mod = recode(Mod,
                      "Power" = "Power Model",
                      "Ricker" = "Ricker Model",
                      "PowerCov" = "Power & Temperature Model")) %>%
  mutate(Mod = factor(Mod,
                      levels = c("Ricker Model","Power Model","Power & Temperature Model")))

  
# plot 2022 predictions WITHOUT real 2022 data
pred_2022 <- ESData23[ESData23$yr<2022,] %>%
  ggplot(aes(x=yr, y=True_R)) +
  geom_line() +
  geom_ribbon(data = ribbon_data,
              aes(x = yr, 
                  ymin = Pred_low,
                  ymax = Pred_up, 
                  fill = Mod),
              color = "transparent",
              alpha = .4) +
  geom_segment(data = final[final$yr == 2022,],
               aes(x = 2021, 
                   xend = yr, 
                   y = ESData23$True_R[ESData23$yr == 2021],
                   yend = Pred,
                   color = Mod)) +
  geom_point(data = final[final$yr == 2022,],
               aes( x = yr, 
                   y = Pred,
                   color = Mod),
             size = .5) +
  ggrepel::geom_text_repel(data = final[final$yr == 2022,],
                            aes(x= yr, y = Pred, color = Mod,
                                label = formatC(Pred, format="f", big.mark=",", digits=0)),
                           nudge_x = 1, nudge_y = 2000,
                           direction = "y", hjust = "left",
                           show.legend = F,
                           segment.color = "grey80") +
  ggthemes::theme_few() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        strip.text = element_text(face = "bold")) +
  coord_cartesian(xlim = c(1950,2035)) +
  facet_wrap(~Mod, ncol = 1) +
  labs(x = "Year",
       y = "Returns",
       title = "2022 Predictions",
       caption = "True 2022 Return: 243,951") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)


ggview::ggview(pred_2022,
               width = 5, 
               height = 5,
               unit = "in")

ggsave(pred_2022,
       width = 5, height = 5,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","pred_plot_2022_without_real_value.png"))



# Repeat exact same plot WITH real 2022 data
pred_2022_w_real <- ESData23 %>%
  ggplot(aes(x=yr, y=True_R)) +
  geom_line() +
  geom_ribbon(data = ribbon_data,
              aes(x = yr, 
                  ymin = Pred_low,
                  ymax = Pred_up, 
                  fill = Mod),
              color = "transparent",
              alpha = .4) +
  geom_segment(data = final[final$yr == 2022,],
               aes(x = 2021, 
                   xend = yr, 
                   y = ESData23$True_R[ESData23$yr == 2021],
                   yend = Pred,
                   color = Mod)) +
  geom_point(data = final[final$yr == 2022,],
             aes( x = yr, 
                  y = Pred,
                  color = Mod),
             size = .5) +
  ggrepel::geom_text_repel(data = final[final$yr == 2022,],
                           aes(x= yr, y = Pred, color = Mod,
                               label = formatC(Pred, format="f", big.mark=",", digits=0)),
                           nudge_x = 1, nudge_y = 2000,
                           direction = "y", hjust = "left",
                           show.legend = F,
                           segment.color = "grey80") +
  ggthemes::theme_few() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        strip.text = element_text(face = "bold")) +
  coord_cartesian(xlim = c(1950,2035)) +
  facet_wrap(~Mod, ncol = 1) +
  labs(x = "Year",
       y = "Returns",
       title = "2022 Predictions",
       caption = "True 2022 Return: 243,951") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)


ggview::ggview(pred_2022_w_real,
               width = 5, 
               height = 5,
               unit = "in")

ggsave(pred_2022_w_real,
       width = 5, height = 5,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","pred_plot_2022_with_real_value.png"))





# forecast for 2023 -------------------------------------------------------

# create empty list to store values
pred23 <- list()

# predict 2023 ricker 
pred <-  RunModRetro_new(Dat=ESData23, Pred_Year=2023, Model = "Ricker")
pred23[[paste0("Ricker",2023)]] <- pred$Preds_Out


# predict 2023 power 
pred <-  RunModRetro_new(Dat=ESData23, Pred_Year=2023, Model = "Power")
pred23[[paste0("Power",2023)]] <- pred$Preds_Out

# predict cov 2023 power 
pred <-  RunModRetro_new(Dat=ESData23, Pred_Year=2023, Model = "PowerCov", Cov = cov_sst)
pred23[[paste0("PowerCov",2023)]] <- pred$Preds_Out

# change list to dataframe
pred23_df <- as.data.frame(do.call(rbind, pred23)) 


# plot predictions 

# format data to make ribbon plots 
ribbon_data_23 <- pred23_df %>%
  select(-ModType) %>%
  # manually bind 2022 values to give error shadows a starting point
  rbind(
    data.frame(
      Pred_Year = c(2022),
      Mod = c("Ricker","Power","PowerCov"),
      Pred4 = NA, 
      Pred5 = NA,
      Pred = ESData23$True_R[ESData23$yr == 2022],
      Pred_low = ESData23$True_R[ESData23$yr == 2022],
      Pred_up = ESData23$True_R[ESData23$yr == 2022]
      
    )
  ) %>%
  
  # change and order model names
  mutate(Mod = recode(Mod,
                      "Power" = "Power Model",
                      "Ricker" = "Ricker Model",
                      "PowerCov" = "Power & Temperature Model")) %>%
  mutate(Mod = factor(Mod,
                      levels = c("Ricker Model","Power Model","Power & Temperature Model")))


 
# plot 2023 predictions
pred_2023 <- ESData23 %>%
  ggplot(aes(x=yr, y=True_R)) +
  geom_line() +
  geom_ribbon(data = ribbon_data_23,
              inherit.aes=F,
              aes(x = Pred_Year, 
                  ymin = Pred_low,
                  ymax = Pred_up, 
                  fill = Mod),
              color = "transparent",
              alpha = .4) +
  geom_segment(data = ribbon_data_23,
               aes(x = 2022, 
                   xend = Pred_Year, 
                   y = ESData23$True_R[ESData23$yr == 2022],
                   yend = Pred,
                   color = Mod)) +
  geom_point(data = ribbon_data_23[ribbon_data_23$Pred_Year == 2023,],
             aes( x = Pred_Year, 
                  y = Pred,
                  color = Mod)) +
  ggrepel::geom_text_repel(data = ribbon_data_23[ribbon_data_23$Pred_Year == 2023,],
                           aes(x= Pred_Year, y = Pred, color = Mod,
                               label = formatC(Pred, format="f", big.mark=",", digits=0)),
                           nudge_x = 1, direction = "y", hjust = "left",
                           show.legend = F,
                           segment.color = "grey80") +
  ggthemes::theme_few() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        strip.text = element_text(face = "bold")) +
  coord_cartesian(xlim = c(1950,2035)) +
  facet_wrap(~Mod, ncol = 1) +
  labs(x = "Year",
       y = "Returns",
       title = "2023 Predictions") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = pal)


ggview::ggview(pred_2023,
               width = 5, 
               height = 5,
               unit = "in")

ggsave(pred_2023,
       width = 5, height = 5,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","pred_plot_2023.png"))




# investigate why 2023 predictions are so low -----------------------------
# 2023 predictions are uncharactaristically low. This applied to all 3 models,
# indicating it was a function of the S(T-4) value because that's the only 
# value that would change predictions in all 3 models. 
# here, we plot that S value over time to see if recent values are anomalous
# in a way that might explain our low R model predicitons


# plot ES Spawners
S_plot <- ESData23 %>%
  ggplot(aes(x=yr,
             y=S)) +
  #geom_rect(inherit.aes=F,
  #          xmin = 2019, xmax = Inf,
  #              ymin = -Inf, ymax = Inf, stat = "unique",
  #          fill = "tomato2", alpha = .2) +
  geom_point(inherit.aes=F,
             aes(y = S[yr == 2019],
             x = 2019), size = 2) +
  ggrepel::geom_text_repel(
    inherit.aes=F, stat  = "unique",
    aes(y = S[yr == 2019],
        x = 2019, label = "Lowest Value\nEver Recorded\n(as of then)"),
    nudge_x = -5, direction = "y", hjust = "right",
    point.padding = .5,
    lineheight = .8
    ) +
  geom_line() +
  scale_y_log10(#breaks = scales::pretty_breaks(),
                labels = scales::comma) +
  labs(y = NULL,
       x = NULL,
       title = "Log Spawners per Year") +
  theme(plot.title.position = "plot")


ggview::ggview(S_plot,
               width = 5, height = 3, unit = "in")

ggsave(S_plot,
       width = 5, height = 3,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","Spawners_plot_2023.png"))

rm(pred_2022, pred_2023, ribbon_data, ribbon_data_23, S_plot)


# find stock status -------------------------------------------------------

# here, we will use the mean value of recruits over time
# to define the stock status thresholds, then plot the true
# values until 2022, and our prediction for 2023 


# find mean R and SD R
mean_R <- mean(ESData23$True_R, na.rm=T)
sd_R <- sd(ESData23$True_R, na.rm = T)

low_threshold_R <- mean_R - .5*sd_R
high_threshold_R <- mean_R + .5*sd_R



# plot ---------------------------------------------------------------------
stock_status <- ggplot() +
  # plot lower box
  geom_rect(aes(xmin = -Inf, 
            xmax = Inf,
            ymin = -Inf,
            ymax = low_threshold_R),
            fill = "#FF3700", alpha = .15) +
  # plot middle box
  geom_rect(aes(xmin = -Inf, 
            xmax = Inf,
            ymin = low_threshold_R,
            ymax = high_threshold_R),
            fill = "#FFeC00", alpha = .15,
            stat = "unique")  +
  # plot top box
  geom_rect(aes(xmin = -Inf, 
            xmax = Inf,
            ymin = high_threshold_R,
            ymax = Inf),
            fill = "#00FF00", alpha = .15,
            stat = "unique")  +
  geom_hline(yintercept = c(high_threshold_R, low_threshold_R),
             linetype = "dotted", size = .25) +
  
  
  geom_line(data = ESData23, size = .4,
            aes(x=yr, y =True_R))  +
  geom_segment(inherit.aes=F, 
               aes(x = 2022, 
                   xend = 2023,
                   y = ESData23$True_R[ESData23$yr == 2022],
                   yend = pred23_df$Pred[pred23_df$Mod=="PowerCov"]),
               linetype = "dashed", size = .4) +
  geom_point(inherit.aes=F,
             aes(x = 2023, 
                 y = pred23_df$Pred[pred23_df$Mod=="PowerCov"])) +
  labs(y = "Recruits",
       x = NULL,
       title = "Stock Status over Time",
       caption = "*Showing Power + Covariate Model Prediction") +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title.position = "plot") 


ggview::ggview(stock_status,
               width = 5, height = 4,
               unit = "in")

ggsave(stock_status,
       width = 6, height = 5,
       units = "in",
       file = here::here("CaseStudy_Fisheries","Group_1_work",
                         "plots","stock_status.png"))
