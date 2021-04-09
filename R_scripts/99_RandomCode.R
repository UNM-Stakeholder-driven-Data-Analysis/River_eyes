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
mod2 <- lmer(log(Sum_days_rm_dry+1) ~ Isleta_div + SanA_div + SanA_gage + Ag_yr_mn + (1|Year) +(1|RM), data=dat, REML = FALSE)
qqnorm(resid(mod1))
qqline(resid(mod1))
car::vif(mod1)

#comparing models and selection ####
bbmle::AICtab(mod_null, mod_full, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)
bbmle::AICctab(mod_null, mod_full, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)
bbmle::BICtab(mod_null, mod_full, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)

#AICc and BIC have mod_ID_SD (Isleta and San Acacia diversions) as best AIC shows null
car::Anova(mod_ID_SD, type = "III")
mod_ID_SD

#plot effects ####

 #simple predictions with CIs
plot(predictorEffect("Isleta_div_sum", mod_ID_SD))
plot(predictorEffect("SanAcacia_div_sum", mod_ID_SD))
plot(predictorEffects(mod_ID_SD, ~Isleta_div_sum + SanAcacia_div_sum))

 #for Isleta with data points, effects, and CI
effects_Isleta <- effects::effect(term="Isleta_div_sum", mod=mod_ID_SD)
summary(effects_Isleta)
x_Isleta <- as.data.frame(effects_Isleta)

ggplot()+
  geom_point(data=dat74rev, aes(Isleta_div_sum, DaysDry))+
  geom_point(data=x_Isleta, aes(x=Isleta_div_sum, y=fit), color = "blue")+
  geom_line(data=x_Isleta, aes(x=Isleta_div_sum, y=fit), color = "blue")+
  geom_ribbon(data=x_Isleta, aes(x=Isleta_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="blue")+
  labs(x="Isleta total annual diversion (cfs)", y="Days in a year river mile 74 was dry")+
  scale_x_continuous(labels = scales::comma)

#for SanAcacia with data points, effects, and CI
effects_SanAcacia <- effects::effect(term="SanAcacia_div_sum", mod=mod_ID_SD)
summary(effects_SanAcacia)
x_SanAcacia <- as.data.frame(effects_SanAcacia)

ggplot()+
  geom_point(data=dat74rev, aes(SanAcacia_div_sum, DaysDry))+
  geom_point(data=x_SanAcacia, aes(x=SanAcacia_div_sum, y=fit), color = "blue")+
  geom_line(data=x_SanAcacia, aes(x=SanAcacia_div_sum, y=fit), color = "blue")+
  geom_ribbon(data=x_SanAcacia, aes(x=SanAcacia_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="blue")+
  labs(x="SanAcacia total annual diversion (cfs)", y="Days in a year river mile 74 was dry")+
  scale_x_continuous(labels = scales::comma)

