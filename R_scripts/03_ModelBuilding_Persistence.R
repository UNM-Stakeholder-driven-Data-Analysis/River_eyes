#Read me ####
#The purpose of this script is to construct linear models to predict
#the extent of drying 
#1) make 3 data sets (All river miles; river mile = 74 to 100; river mile = 74)
#2) assess correlation among predictors
#3) construct lme models on single river miles and assess assumptions 
#4) conduct model selection and Anova of top models
#5) visualize effects
#6) construct and test models for larger datasets


#Libraries ####
library(tidyverse)
library(lubridate)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans)
library(DHARMa)
library(psych)
library(forecast)
library(nlme)
library(effects)
library(nlme)
library(cowplot)

#1) Load and wrangle data sets ####
resp_dat <- read.csv('Data/Processed/AnnualDryRM.csv') 
  
  #data for river mile 74 and 100
resp_dat74_100 <- resp_dat %>% 
  filter(RM>=74 & RM<=100) %>% 
  arrange(Year)

  #data for single river miles
resp_dat74 <- resp_dat %>% 
  filter(RM==74) %>% 
  arrange(Year)

resp_dat80 <- resp_dat %>% 
  filter(RM==80) %>% 
  arrange(Year)

resp_dat155 <- resp_dat %>% 
  filter(RM==155) %>% 
  arrange(Year)

resp_dat134 <- resp_dat %>% 
  filter(RM==134) %>% 
  arrange(Year)
  
  #predictor data wrangleing
pred_gages <- read.csv("Data/Processed/Predictors.csv")
pred_gages_sum <- pred_gages%>% 
  select(Date, Isleta_mean_daily_div_cfs, SanAcacia_mean_daily_div_cfs, Mean_cfs_SanAcacia) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  group_by(Year) %>% 
  summarise(across(Isleta_mean_daily_div_cfs:Mean_cfs_SanAcacia, sum, na.rm=TRUE)) %>% 
  arrange(Year)
otowi <- read.csv("Data/Processed/OtowiIndex.csv") %>% 
  select(!X) %>% 
  arrange(Year)

  #combine response and predictors
#all river miles
dat_full <- cbind(resp_dat$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

#river miles 74 to 100
dat_full2 <- cbind(resp_dat74_100$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat74_100$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7]) 

#for River Mile 74
dat_74 <- cbind(resp_dat74$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat74$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

#for River Mile 80
dat_80 <- cbind(resp_dat80$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat80$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

#for River Mile 155
dat_155 <- cbind(resp_dat155$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat155$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

#for River Mile 134
dat_134<- cbind(resp_dat134$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat134$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

#2) Assess predictor correlations and reduce as needed ####

pairs.panels(dat74[,3:6], scale=T)
pairs.panels(dat74[,3:6], scale=F) #SanAcaciaGage and Otowi 0.96 and removed, #SanAGage and diversion/Otow 0.58 and 0.55 can I keep...I kept for now

dat74rev <- dat74 %>% 
  select(!SanAcacia_gage_sum) %>% 
  mutate(Year.f = as.character(Year))

dat80rev <- dat80 %>% 
  select(!SanAcacia_gage_sum) %>% 
  mutate(Year.f = as.character(Year))

#3) Test lme to develop model set assumptions ####
#River mile 74 and PASSES model assumptions although heteroscedasticity looks a little positively linear...####
mod_null <- lme(DaysDry ~ 1,
                   data = dat_74, 
                   method = "ML",
                   random = ~1|Year)
mod_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_74, 
                   method = "ML",
                   random = ~1|Year)
mod_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_74, 
                 method = "ML",
                 random = ~1|Year)
mod_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_74, 
                 method = "ML",
                 random = ~1|Year)
mod_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_74, 
                 method = "ML",
                 random = ~1|Year)
mod_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_74, 
              method = "ML",
              random = ~1|Year)
mod_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_74, 
              method = "ML",
              random = ~1|Year)
mod_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_74, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(1,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_SD_OT), main = "RM 74 mod_ID_SD_OT")
qqline(resid(mod_ID_SD_OT))
plot(resid(mod_ID_SD_OT, type = "normalized") ~fitted(mod_ID_SD_OT)) 
Acf(resid(mod_ID_SD_OT), main = "")

par(mfrow=c(1,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_SD), main = "RM 74 mod_ID_SD")
qqline(resid(mod_ID_SD))
plot(resid(mod_ID_SD, type = "normalized") ~fitted(mod_ID_SD)) 
Acf(resid(mod_ID_SD), main = "")

par(mfrow=c(1,3))#Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_OT), main = "RM 74 mod_ID_OT")
qqline(resid(mod_ID_OT))
plot(resid(mod_ID_OT, type = "normalized") ~fitted(mod_ID_OT)) 
Acf(resid(mod_ID_OT), main = "")

par(mfrow=c(1,3))
qqnorm(resid(mod_SD_OT), main = "RM 74 mod_SD_OT")
qqline(resid(mod_SD_OT))
plot(resid(mod_SD_OT, type = "normalized") ~fitted(mod_SD_OT)) 
Acf(resid(mod_SD_OT), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_ID), main = "RM 74 mod_ID")
qqline(resid(mod_ID))
plot(resid(mod_ID, type = "normalized") ~fitted(mod_ID)) 
Acf(resid(mod_ID), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_SD), main = "RM 74 mod_SD")
qqline(resid(mod_SD))
plot(resid(mod_SD, type = "normalized") ~fitted(mod_SD)) 
Acf(resid(mod_SD), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_OT), main = "RM 74 mod_OT")
qqline(resid(mod_OT))
plot(resid(mod_OT, type = "normalized") ~fitted(mod_OT)) 
Acf(resid(mod_OT), main = "")

#River mile 80 similar output as river mile 74...####
mod_null <- lme(DaysDry ~ 1,
                data = dat_80, 
                method = "ML",
                random = ~1|Year)
mod_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_80, 
                    method = "ML",
                    random = ~1|Year)
mod_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_80, 
                 method = "ML",
                 random = ~1|Year)
mod_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_80, 
                 method = "ML",
                 random = ~1|Year)
mod_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_80, 
                 method = "ML",
                 random = ~1|Year)
mod_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_80, 
              method = "ML",
              random = ~1|Year)
mod_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_80, 
              method = "ML",
              random = ~1|Year)
mod_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_80, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(1,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_SD_OT), main = "RM 80 mod_ID_SD_OT")
qqline(resid(mod_ID_SD_OT))
plot(resid(mod_ID_SD_OT, type = "normalized") ~fitted(mod_ID_SD_OT)) 
Acf(resid(mod_ID_SD_OT), main = "")

par(mfrow=c(1,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_SD), main = "RM 80 mod_ID_SD")
qqline(resid(mod_ID_SD))
plot(resid(mod_ID_SD, type = "normalized") ~fitted(mod_ID_SD)) 
Acf(resid(mod_ID_SD), main = "")

par(mfrow=c(1,3))#Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_OT), main = "RM 80 mod_ID_OT")
qqline(resid(mod_ID_OT))
plot(resid(mod_ID_OT, type = "normalized") ~fitted(mod_ID_OT)) 
Acf(resid(mod_ID_OT), main = "")

par(mfrow=c(1,3))
qqnorm(resid(mod_SD_OT), main = "RM 80 mod_SD_OT")
qqline(resid(mod_SD_OT))
plot(resid(mod_SD_OT, type = "normalized") ~fitted(mod_SD_OT)) 
Acf(resid(mod_SD_OT), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_ID), main = "RM 80 mod_ID")
qqline(resid(mod_ID))
plot(resid(mod_ID, type = "normalized") ~fitted(mod_ID)) 
Acf(resid(mod_ID), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_SD), main = "RM 80 mod_SD")
qqline(resid(mod_SD))
plot(resid(mod_SD, type = "normalized") ~fitted(mod_SD)) 
Acf(resid(mod_SD), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_OT), main = "RM 80 mod_OT")
qqline(resid(mod_OT))
plot(resid(mod_OT, type = "normalized") ~fitted(mod_OT)) 
Acf(resid(mod_OT), main = "")

#River mile 155 similar output as river mile 74 ...####
mod_null <- lme(DaysDry ~ 1,
                data = dat_155, 
                method = "ML",
                random = ~1|Year)
mod_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_155, 
                    method = "ML",
                    random = ~1|Year)
mod_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_155, 
                 method = "ML",
                 random = ~1|Year)
mod_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_155, 
                 method = "ML",
                 random = ~1|Year)
mod_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_155, 
                 method = "ML",
                 random = ~1|Year)
mod_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_155, 
              method = "ML",
              random = ~1|Year)
mod_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_155, 
              method = "ML",
              random = ~1|Year)
mod_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_155, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(1,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_SD_OT), main = "RM 155 mod_ID_SD_OT")
qqline(resid(mod_ID_SD_OT))
plot(resid(mod_ID_SD_OT, type = "normalized") ~fitted(mod_ID_SD_OT)) 
Acf(resid(mod_ID_SD_OT), main = "")

par(mfrow=c(1,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_SD), main = "RM 155 mod_ID_SD")
qqline(resid(mod_ID_SD))
plot(resid(mod_ID_SD, type = "normalized") ~fitted(mod_ID_SD)) 
Acf(resid(mod_ID_SD), main = "")

par(mfrow=c(1,3))#Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod_ID_OT), main = "RM 155 mod_ID_OT")
qqline(resid(mod_ID_OT))
plot(resid(mod_ID_OT, type = "normalized") ~fitted(mod_ID_OT)) 
Acf(resid(mod_ID_OT), main = "")

par(mfrow=c(1,3))
qqnorm(resid(mod_SD_OT), main = "RM 155 mod_SD_OT")
qqline(resid(mod_SD_OT))
plot(resid(mod_SD_OT, type = "normalized") ~fitted(mod_SD_OT)) 
Acf(resid(mod_SD_OT), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_ID), main = "RM 155 mod_ID")
qqline(resid(mod_ID))
plot(resid(mod_ID, type = "normalized") ~fitted(mod_ID)) 
Acf(resid(mod_ID), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_SD), main = "RM 155 mod_SD")
qqline(resid(mod_SD))
plot(resid(mod_SD, type = "normalized") ~fitted(mod_SD)) 
Acf(resid(mod_SD), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_OT), main = "RM 155 mod_OT")
qqline(resid(mod_OT))
plot(resid(mod_OT, type = "normalized") ~fitted(mod_OT)) 
Acf(resid(mod_OT), main = "")

#River mile 134 models less able to pass assumptions and appearance of an outlier ...####
mod_null <- lme(DaysDry ~ 1,
                data = dat_134, 
                method = "ML",
                random = ~1|Year)
mod_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_134, 
                    method = "ML",
                    random = ~1|Year)
mod_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_134, 
                 method = "ML",
                 random = ~1|Year)
mod_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_134, 
                 method = "ML",
                 random = ~1|Year)
mod_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_134, 
                 method = "ML",
                 random = ~1|Year)
mod_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_134, 
              method = "ML",
              random = ~1|Year)
mod_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_134, 
              method = "ML",
              random = ~1|Year)
mod_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_134, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(1,3)) #qq and heteroscedasticity not that great
qqnorm(resid(mod_ID_SD_OT), main = "RM 134 mod_ID_SD_OT")
qqline(resid(mod_ID_SD_OT))
plot(resid(mod_ID_SD_OT, type = "normalized") ~fitted(mod_ID_SD_OT)) 
Acf(resid(mod_ID_SD_OT), main = "")

par(mfrow=c(1,3)) #same as mod_ID_SD_OT
qqnorm(resid(mod_ID_SD), main = "RM 134 mod_ID_SD")
qqline(resid(mod_ID_SD))
plot(resid(mod_ID_SD, type = "normalized") ~fitted(mod_ID_SD)) 
Acf(resid(mod_ID_SD), main = "")

par(mfrow=c(1,3))#seems to have an outlier and otherwise might be okay
qqnorm(resid(mod_ID_OT), main = "RM 134 mod_ID_OT")
qqline(resid(mod_ID_OT))
plot(resid(mod_ID_OT, type = "normalized") ~fitted(mod_ID_OT)) 
Acf(resid(mod_ID_OT), main = "")

par(mfrow=c(1,3))
qqnorm(resid(mod_SD_OT), main = "RM 134 mod_SD_OT")
qqline(resid(mod_SD_OT))
plot(resid(mod_SD_OT, type = "normalized") ~fitted(mod_SD_OT)) 
Acf(resid(mod_SD_OT), main = "")

par(mfrow=c(1,3)) #a bit off on qqplot and outlier on hetero
qqnorm(resid(mod_ID), main = "RM 134 mod_ID")
qqline(resid(mod_ID))
plot(resid(mod_ID, type = "normalized") ~fitted(mod_ID)) 
Acf(resid(mod_ID), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_SD), main = "RM 134 mod_SD")
qqline(resid(mod_SD))
plot(resid(mod_SD, type = "normalized") ~fitted(mod_SD)) 
Acf(resid(mod_SD), main = "")

par(mfrow=c(1,3)) #kind of not so great on qqplot and same issues with heteroscedasticity
qqnorm(resid(mod_OT), main = "RM 134 mod_OT")
qqline(resid(mod_OT))
plot(resid(mod_OT, type = "normalized") ~fitted(mod_OT)) 
Acf(resid(mod_OT), main = "")

#4) Model selection and Anova tables of top models ####
 #compare models using AIC (seemed like all models mostly passed assumptions)
bbmle::AICtab(mod_null, mod_ID_SD_OT, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)
bbmle::AICctab(mod_null, mod_ID_SD_OT, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)
bbmle::BICtab(mod_null, mod_ID_SD_OT, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)

 #since there were only 18 data points I relied more on AICc and BIC 
 #these just had mod_OT within 2.0 however the null model was too (AIC had mod_OT, mod_SD_OT, mod_SD,
    #mod_ID_OT and null)
 #within the top candidate models for RM 74 and 80 .....
 #null model appeared to be best candidate for RM 134 but mod_OT was in dBIC

car::Anova(mod_OT, type = "III") #p-value significant
mod_OT
(CI_mod_OT <- intervals(mod_OT)) #Does not overlap 0


#5) Visualize effects ####

#simple predictions with CIs mod_ID and mod_OT
plot(predictorEffect("Isleta_div_sum", mod_ID))
plot(predictorEffect("OtowiIndex", mod_OT))
plot(predictorEffects(mod_ID_OT, ~Isleta_div_sum + OtowiIndex))
plot(predictorEffects(mod_ID_SD, ~Isleta_div_sum + SanAcacia_div_sum))

#plot of mod_ID
effects_Isleta_b <- effects::effect(term="Isleta_div_sum", mod=mod_ID)
summary(effects_Isleta_b)
x_Isleta_b <- as.data.frame(effects_Isleta_b)

plot_ID <- ggplot()+
  geom_point(data=dat_80, aes(Isleta_div_sum, DaysDry, color = "black"))+
  geom_point(data=x_Isleta_b, aes(x=Isleta_div_sum, y=fit), color = "black")+
  geom_line(data=x_Isleta_b, aes(x=Isleta_div_sum, y=fit), color = "black")+
  geom_ribbon(data=x_Isleta_b, aes(x=Isleta_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="black")+
  labs(x="Total annual diversion (cfs)", y="Days in a year river mile 80 was dry")+
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1))+
  scale_color_manual(values = c("black"), name = "", labels=(c("Isleta diversion")))

#plot of mod_OT
effects_Otowi_a <- effects::effect(term="OtowiIndex", mod=mod_OT)
summary(effects_Otowi_a)
effects_Otowi_a <- as.data.frame(effects_Otowi_a)

plot_OT <-ggplot()+
  geom_point(data=dat_155, aes(OtowiIndex, DaysDry, color = "grey"))+
  geom_point(data=effects_Otowi_a, aes(x=OtowiIndex, y=fit), color = "grey")+
  geom_line(data=effects_Otowi_a, aes(x=OtowiIndex, y=fit), color = "grey")+
  geom_ribbon(data=effects_Otowi_a, aes(x=OtowiIndex, ymin=lower, ymax=upper), alpha=0.3, fill="grey")+
  labs(x="Annual delivery (kaf)", y="Days in a year river mile 155 was dry")+
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1))+
  scale_color_manual(values = c("grey"), name = "", labels=(c("Otowi Index")))

#combin plots into one figure
plot_grid(plot_ID, plot_OT)
plot_OT


#6) model building and testing datasets with more river miles using lme Correlation Structures #####

  #make a time step for each river mile which is 16 for the 16 years of data
dat_full$t <- c(rep(1:16, times=114))
dat_full2$t <- c(rep(1:16, times=27)) 


  #All river miles with correlation structures do not meet assumptions -  
mod_full_un <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                  data = dat_full, 
                  method = "ML",
                  random = ~1|Year)

mod_full_cs <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                                  data = dat_full, 
                                  method = "ML",
                                  correlation = corCompSymm(form = ~t|RiverMile),
                                  random = ~1|RiverMile)

mod_full_AR1 <- lme(log(DaysDry+1) ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                                  data = dat_full, 
                                  method = "ML",
                                  correlation = corAR1(form = ~t|RiverMile),
                                  random = ~1|RiverMile)

 #tried p=1 and 2, 3 gave me a convergence error; tried q=2,3, 1 and values of 
mod_full_ARMAp2q1 <- lme(log(DaysDry+1) ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_full, 
                    method = "ML",
                    correlation = corARMA(value=c(-0.3,-0.2,0.3,0.8),
                                         p=2, q=2, form = ~t|RiverMile),
                    random = ~1|RiverMile)

par(mfrow=c(1,3))
qqnorm(resid(mod_full_ARMAp2q1), main = "All river miles mod Iselta + San Acacia + \n Otowi 'Year'= random")
qqline(resid(mod_full_ARMAp2q1))
plot(resid(mod_full_ARMAp2q1, type = "normalized")~fitted(mod_full_ARMAp2q1), main = "")
Acf(resid(mod_full_ARMAp2q1))



#River miles 74-100 
mod_full2_un <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_full2, 
                   method = "ML",
                   random = ~1|Year)
   #qqplot and heteroscedasticity better than full river mile set but acf remains strong in 3 and 4 lags
mod_full2_cs <- lme(log(DaysDry+1) ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_full2, 
                   method = "ML",
                   correlation = corCompSymm(form = ~t|RiverMile),
                   random = ~1|RiverMile)
   #same as CompSymm
mod_full2_AR1 <- lme(log(DaysDry+1) ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_full2, 
                    method = "ML",
                    correlation = corAR1(form = ~t|RiverMile),
                    random = ~1|RiverMile)

#tried p=3 and 2 a 1 gave me a convergence error; tried q=2 and 1 a 3 gave me a convergence error 
mod_full2_ARMAp2q1 <- lme(log(DaysDry+1) ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                         data = dat_full2, 
                         method = "ML",
                         correlation = corARMA(value=c(0.3, 0.1, 0.4, -0.3, 0.2),
                                               p=2, q=3, form = ~t|RiverMile),
                         random = ~1|RiverMile)

par(mfrow=c(1,3))
qqnorm(resid(mod_full2_ARMAp2q1), main = "River miles 74-100 mod Iselta + San Acacia + \n Otowi 'Year'= random")
qqline(resid(mod_full2_ARMAp2q1))
plot(resid(mod_full2_ARMAp2q1, type = "normalized")~fitted(mod_full2_ARMAp2q1), main = "Heteroscedasticity")
Acf(resid(mod_full2_ARMAp2q1))

plot(mod_full2_AR1, resid(., type = "normalized") ~ fitted(.) | RiverMile, abline = 0)
plot(mod_full2_AR1, RiverMile ~ resid(., type = "normalized"))
plot(mod_full2_AR1, log(DaysDry+1) ~ fitted(.) | RiverMile, abline = c(0,1))
