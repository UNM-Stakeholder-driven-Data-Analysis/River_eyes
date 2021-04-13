#Read me ####
#The purpose of this script is to construct linear models to predict
#the extent of drying 
#1) make 3 data sets (All river miles; river mile = 74 to 100; river mile = 74)
#2) assess correlation among predictors
#3) construct lme models on RM 74 and assess assumptions 
#4) conduct model selection and Anova of top models
#5) visualize effects


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
  #data for river mile 74 only
resp_dat74 <- resp_dat %>% 
  filter(RM==74) %>% 
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
dat_full <- cbind(resp_dat$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

dat_full2 <- cbind(resp_dat74_100$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat74_100$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

dat_74 <- cbind(resp_dat74$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat74$RM) %>% 
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

#3) Test lme to develop model set assumptions ####
  #River mile 74 and PASSES model assumptions although heteroscedasticity looks a little positively linear...
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

#4) Model selection and Anova tables of top models ####
 #compare models using AIC (seemed like all models mostly passed assumptions)
bbmle::AICtab(mod_null, mod_ID_SD_OT, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)
bbmle::AICctab(mod_null, mod_ID_SD_OT, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)
bbmle::BICtab(mod_null, mod_ID_SD_OT, mod_ID_SD, mod_ID_OT, mod_SD_OT, mod_ID, mod_SD, mod_OT)

 #since there were only 18 data points I relied more on AICc and BIC 
 #these had mod_ID, mod_OT, mod_ID_OT, and mod_ID_SD within 2.0 however the null model was
 #within the top candidate models.....

car::Anova(mod_ID, type = "III") #p-value significant
mod_ID
(CI_mod_ID <- intervals(mod_ID)) #Does not overlap 0

car::Anova(mod_OT, type = "III") #p-value significant
mod_OT
(CI_mod_OT <- intervals(mod_OT)) #Does not overlap 0

car::Anova(mod_ID_OT, type = "III") #both p-value significant
mod_ID_OT
(CI_mod_ID_OT <- intervals(mod_ID_OT)) #Overlaps 0

car::Anova(mod_ID_SD, type = "III") #only Isleta singifcant
mod_ID_SD
(CI_mod_ID_SD <- intervals(mod_ID_SD)) #only Isleta doesn't overlap 0

#5) Visualize effects ####

#simple predictions with CIs mod_ID, mod_OT, mod_ID_OT, and mod_ID_SD 
plot(predictorEffect("Isleta_div_sum", mod_ID))
plot(predictorEffect("OtowiIndex", mod_OT))
plot(predictorEffects(mod_ID_OT, ~Isleta_div_sum + OtowiIndex))
plot(predictorEffects(mod_ID_SD, ~Isleta_div_sum + SanAcacia_div_sum))

#plot of mod_ID
effects_Isleta_b <- effects::effect(term="Isleta_div_sum", mod=mod_ID)
summary(effects_Isleta_b)
x_Isleta_b <- as.data.frame(effects_Isleta_b)

plot_ID <- ggplot()+
  geom_point(data=dat_74, aes(Isleta_div_sum, DaysDry, color = "black"))+
  geom_point(data=x_Isleta_b, aes(x=Isleta_div_sum, y=fit), color = "black")+
  geom_line(data=x_Isleta_b, aes(x=Isleta_div_sum, y=fit), color = "black")+
  geom_ribbon(data=x_Isleta_b, aes(x=Isleta_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="black")+
  labs(x="Total annual diversion (cfs)", y="Days in a year river mile 74 was dry")+
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
  geom_point(data=dat_74, aes(OtowiIndex, DaysDry, color = "grey"))+
  geom_point(data=effects_Otowi_a, aes(x=OtowiIndex, y=fit), color = "grey")+
  geom_line(data=effects_Otowi_a, aes(x=OtowiIndex, y=fit), color = "grey")+
  geom_ribbon(data=effects_Otowi_a, aes(x=OtowiIndex, ymin=lower, ymax=upper), alpha=0.3, fill="grey")+
  labs(x="Annual delivery (kaf)", y="")+
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1))+
  scale_color_manual(values = c("grey"), name = "", labels=(c("Otowi Index")))

#plot of mod_ID_OT
effects_Isleta_a <- effects::effect(term="Isleta_div_sum", mod=mod_ID_OT)
summary(effects_Isleta_a)
x_Isleta_a <- as.data.frame(effects_Isleta_a)

effects_Otowi <- effects::effect(term="OtowiIndex", mod=mod_ID_OT)
summary(effects_Otowi)
x_Otowi <- as.data.frame(effects_Otowi)

plot_ID_OT <- ggplot()+
  geom_point(data=dat_74, aes(Isleta_div_sum, DaysDry, color = "blue"))+
  geom_point(data=x_Isleta_a, aes(x=Isleta_div_sum, y=fit), color = "blue")+
  geom_line(data=x_Isleta_a, aes(x=Isleta_div_sum, y=fit), color = "blue")+
  geom_ribbon(data=x_Isleta_a, aes(x=Isleta_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="blue")+
  geom_point(data=dat_74, aes(OtowiIndex, DaysDry, color = "red"))+
  geom_point(data=x_Otowi, aes(x=OtowiIndex, y=fit), color = "red")+
  geom_line(data=x_Otowi, aes(x=OtowiIndex, y=fit), color = "red")+
  geom_ribbon(data=x_Otowi, aes(x=OtowiIndex, ymin=lower, ymax=upper), alpha=0.3, fill="red")+
  labs(x="Total annual diversion (cfs)", y="Days in a year river mile 74 was dry")+
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1))+
  scale_color_manual(values = c("blue", "red"), name = "", labels=(c("Isleta diversion", "Otowi Index")))

 #plot of mod_ID_SD
effects_Isleta <- effects::effect(term="Isleta_div_sum", mod=mod_ID_SD)
summary(effects_Isleta)
x_Isleta <- as.data.frame(effects_Isleta)

effects_SanAcacia <- effects::effect(term="SanAcacia_div_sum", mod=mod_ID_SD)
summary(effects_SanAcacia)
x_SanAcacia <- as.data.frame(effects_SanAcacia)

plot_ID_SD <- ggplot()+
  geom_point(data=dat_74, aes(Isleta_div_sum, DaysDry, color = "blue"))+
  geom_point(data=x_Isleta, aes(x=Isleta_div_sum, y=fit), color = "blue")+
  geom_line(data=x_Isleta, aes(x=Isleta_div_sum, y=fit), color = "blue")+
  geom_ribbon(data=x_Isleta, aes(x=Isleta_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="blue")+
  geom_point(data=dat_74, aes(SanAcacia_div_sum, DaysDry, color = "orange"))+
  geom_point(data=x_SanAcacia, aes(x=SanAcacia_div_sum, y=fit), color = "orange")+
  geom_line(data=x_SanAcacia, aes(x=SanAcacia_div_sum, y=fit), color = "orange")+
  geom_ribbon(data=x_SanAcacia, aes(x=SanAcacia_div_sum, ymin=lower, ymax=upper), alpha=0.3, fill="orange")+
  labs(x="Total annual diversion (cfs)", y="")+
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1))+
  scale_color_manual(values = c("blue", "orange"), name = "", 
                     labels=(c("Isleta diversion", "San Acacia diversion")))
  
plot_grid(plot_ID, plot_OT)


#6) model building and testing datasets with more river miles #####
#All river miles Year random -  DOES NOT pass model assumptions
mod_full_y <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                  data = dat_full, 
                  method = "ML",
                  random = ~1|Year)
par(mfrow=c(2,3))
qqnorm(resid(mod_full_y), main = "All river miles mod Iselta + San Acacia + \n Otowi 'Year'= random")
qqline(resid(mod_full_y))
plot(resid(mod_full_y, type = "normalized")~fitted(mod_full_y), main = "")
Acf(resid(mod_full_y))

#All river miles Year and River Miles as random and DOES NOT pass model assumptions
mod_full_yrm <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_full, 
                    method = "ML",
                    random = list (Year = ~1, RiverMile = ~1))
par(mfrow=c(1,3))
qqnorm(resid(mod_full_yrm), main = "All river miles mod Iselta + San Acacia + \n Otowi 'Year' & 'River mile' = random")
qqline(resid(mod_full_yrm))
plot(resid(mod_full_yrm, type = "normalized")~fitted(mod_full_yrm))
Acf(resid(mod_full_yrm), main="")

#River miles 74-100 Year random -  DOES NOT pass model assumptions
mod_full2_y <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_full2, 
                   method = "ML",
                   random = ~1|Year)
par(mfrow=c(2,3))
qqnorm(resid(mod_full2_y), main = "River miles 74-100 mod Iselta + San Acacia + \n Otowi 'Year'= random")
qqline(resid(mod_full2_y))
plot(resid(mod_full2_y, type = "normalized")~fitted(mod_full2_y), main = "Heteroscedasticity")
Acf(resid(mod_full2_y))

#River miles 74-100 Year and River Miles as random and DOES NOT pass model assumptions
mod_full2_yrm <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                     data = dat_full2, 
                     method = "ML",
                     random = list (Year = ~1, RiverMile = ~1))
par(mfrow=c(1,3))
qqnorm(resid(mod_full2_yrm), main = "River miles 74-100 mod Iselta + San Acacia + \n Otowi 'Year' & 'River mile' = random")
qqline(resid(mod_full2_yrm))
plot(resid(mod_full2_yrm, type = "normalized")~fitted(mod_full2_yrm), main = "Heteroscedasticity")
Acf(resid(mod_full2_yrm))