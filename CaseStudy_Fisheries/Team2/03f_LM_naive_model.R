# Runs different models tryng to linearise Naive model
# some models R and some rec4 and rec5
# df reponse has one extra column
# @LM_naive.mod is the model output

#Define prediction year
pred.year <- 2021

require(dplyr)
#***************************************************************************
Run.LM_Naive.Mods <- function(Data_Retro, Pred_Year){
  
  Results <- data.frame(Year = numeric(), Mod = character(), ModType = character(), 
                        Pred4 = numeric(), Pred5 = numeric(), Pred45 = numeric())
  
  
  #Linear model for rec_4 and rec_5 
  
  # S values corrected -4 or -5 prepare data avecvaraible S_4 and S_5
  vecS5 <-  Data_Retro %>% filter(yr > 1947 & yr < 2012) %>% pull(S)
  vecS4 <-  Data_Retro %>% filter(yr > 1948 & yr < 2013) %>% pull(S)
  
  
  dfCut <- Data_Retro %>% filter(yr > 1952 & yr < 2017)
  dfCut$S_4 <-vecS4
  dfCut$S_5 <- vecS5
  
  #Models 
  # Linear model  RS4S5
  lmFit45 <-  lm(R ~ -1 + vecS4 + vecS5, data = dfCut)
  
  #forecast
  RS4S5_Est4 <- 0
  RS4S5_Est5 <- 0
  RS4S5_Est45 <- as.numeric(coef(lmFit45)[1]) * Data_Retro$S[Data_Retro$yr == Pred_Year-4] +
    as.numeric(coef(lmFit45)[2]) * Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RS4S5", ModType = "LM",
                       Pred4 = RS4S5_Est4, Pred5 = RS4S5_Est5, Pred45 =  RS4S5_Est45  )
  Results <- bind_rows(Results, new.row)
  
  
  # Linear model  RS4plusS5
  lmFit4 <- lm(rec4 ~ -1 + vecS4 , data = dfCut)
  lmFit5 <- lm(rec5~ -1 + vecS5 , data = dfCut)
  
  #forecast
  RS4plusS5_Est4 <- as.numeric(coef(lmFit4)[1]) * Data_Retro$S[Data_Retro$yr == Pred_Year-4]
  RS4plusS5_Est5  <- as.numeric(coef(lmFit5)[1]) * Data_Retro$S[Data_Retro$yr == Pred_Year-5]
  new.row = data.frame(Year = Pred_Year, Mod = "RS4plusS5", ModType = "LM",
                       Pred4 = RS4plusS5_Est4, Pred5 = RS4plusS5_Est5, Pred45 =  RS4plusS5_Est4 + RS4plusS5_Est5  )
  Results <- bind_rows(Results, new.row)
  
  # Linear model  RS4S53C
  lmFit45_3c <-  lm(R ~ -1 + vecS4 + vecS5 + peak + aflow + jnesst, data = dfCut)
  
  #forecast
  RS4S53C_Est4 <- 0
  RS4S53C_Est5 <- 0
  RS4S53C_Est45 <- as.numeric(coef(lmFit45_3c)[1]) * Data_Retro$S[Data_Retro$yr == Pred_Year-4] +
    as.numeric(coef(lmFit45_3c)[2]) * Data_Retro$S[Data_Retro$yr == Pred_Year-5] +
    as.numeric(coef(lmFit45_3c)[3]) * Data_Retro$peak[Data_Retro$yr == Pred_Year-2] +
    as.numeric(coef(lmFit45_3c)[4]) * Data_Retro$aflow[Data_Retro$yr == Pred_Year-2] +
    as.numeric(coef(lmFit45_3c)[5]) * Data_Retro$jnesst[Data_Retro$yr == Pred_Year-2] 
  new.row = data.frame(Year = Pred_Year, Mod = "RS4S53C", ModType = "LM",
                       Pred4 = RS4S53C_Est4, Pred5 = RS4S53C_Est5, Pred45 =  RS4S53C_Est45  )
  Results <- bind_rows(Results, new.row)
  
  
  # Linear model  RS4S53C
  lmFit45_3c <-  lm(R ~ -1 + vecS4 + vecS5 + peak + aflow + jnesst, data = dfCut)
  
  #forecast
  RS4S53C_Est4 <- 0
  RS4S53C_Est5 <- 0
  RS4S53C_Est45 <- as.numeric(coef(lmFit45_3c)[1]) * Data_Retro$S[Data_Retro$yr == Pred_Year-4] +
    as.numeric(coef(lmFit45_3c)[2]) * Data_Retro$S[Data_Retro$yr == Pred_Year-5] +
    as.numeric(coef(lmFit45_3c)[3]) * Data_Retro$peak[Data_Retro$yr == Pred_Year-2] +
    as.numeric(coef(lmFit45_3c)[4]) * Data_Retro$aflow[Data_Retro$yr == Pred_Year-2] +
    as.numeric(coef(lmFit45_3c)[5]) * Data_Retro$jnesst[Data_Retro$yr == Pred_Year-2] 
  new.row = data.frame(Year = Pred_Year, Mod = "RS4S53C", ModType = "LM",
                       Pred4 = RS4S53C_Est4, Pred5 = RS4S53C_Est5, Pred45 =  RS4S53C_Est45  )
  Results <- bind_rows(Results, new.row)
  
  
  
  # Linear model  RS4S5_jnesst
  lmFit45_jnesst <-  lm(R ~ -1 + vecS4 + vecS5 + jnesst, data = dfCut)
  
  #forecast
  RS4S5_jnesst_Est4 <- 0
  RS4S5_jnesst_Est5 <- 0
  RS4S5_jnesst_Est45 <- as.numeric(coef(lmFit45_jnesst)[1]) * Data_Retro$S[Data_Retro$yr == Pred_Year-4] +
    as.numeric(coef(lmFit45_jnesst)[2]) * Data_Retro$S[Data_Retro$yr == Pred_Year-5] +
    as.numeric(coef(lmFit45_jnesst)[3]) * Data_Retro$jnesst[Data_Retro$yr == Pred_Year-2] 
  new.row = data.frame(Year = Pred_Year, Mod = "RS4S53C", ModType = "LM",
                       Pred4 = RS4S5_jnesst_Est4, Pred5 = RS4S5_jnesst_Est4, Pred45 =RS4S5_jnesst_Est45  )
  Results <- bind_rows(Results, new.row)
  
  
  Results 
}
#****************************************************************************

#Run the baseline Ricker model
LM_naive.mod <- Run.LM_Naive.Mods(data, pred.year)

