datToolbox <- datToolbox %>% 
  group_by(Date) %>% 
  pivot_wider(names_from = Reach, values_from = Tot_DCU_cfs:ten_day_avg_URGWOM_cfs) %>% 
  select(Date, Year, Month.n, Day, Ag_DCU_cfs_5:OpenWater_DCU_cfs_8, Rain_cfs_5:Rain_cfs_8)

complete(Date1 = seq.Date(min(Date1), max(Date1), by = "day"), 
         fill = list(value = NA))

### pivot predictors
library(tidyverse)

dat <- read.csv("Data/Processed/Predictors.csv")
resp <- read.csv("Data/Processed/DailyDryRM.csv")

temp <- resp %>% 
  full_join(dat, by=c("Date")) %>% 
  select(-c("Year", "Month", "Condition", "RM_tenth"))

dat <- dat %>% 
  select(!MeanPERCN:MeanCACCN)

dat %>% 
  pivot_longer(cols = Isleta_mean_daily_div_cfs:Rain_cfs_8, names_to = "Predictor", values_to = "cfs") %>% 
  mutate(Predictor=str_replace(string = Predictor, pattern = "Isleta_mean_daily_div_cfs", replacement = "IsletaDiv_DCU_cfs_5")) %>% 
  mutate(Predictor=str_replace(string = Predictor, pattern = "SanAcacia_mean_daily_div_cfs", replacement = "SanAcaciaDiv_DCU_cfs_7")) %>% 
  mutate(Predictor=str_replace(string = Predictor, pattern = "Mean_cfs_SanAcacia", replace = "SanAcaciaGage_DCU_cfs_7")) %>% 
  mutate(Predictor=str_replace(string = Predictor, pattern = "Mean_cfs_BosFarms", replace = "BosFarmsGage_DCU_cfs_5")) %>% 
  mutate(Predictor=str_replace(string = Predictor, pattern = "Rain", replace = "Rain_DCU")) %>%
  separate(col=Predictor, into = c("Type","Abrev","Abrev2", "Reach"))

Mod -> glmer(Condition ~ SanAcacia_div + SanAcaciaGage + AgDepletion + Rain + Reach + (1|RM), data= dat, family="binomial")
