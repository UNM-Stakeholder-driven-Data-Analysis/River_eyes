#Read me ####
#The purpose of this script is to construct linear models to predict
#the extent of drying 
#1) load and make data sets (All river miles; river mile = 74 to 100; river miles individually = 74, 80, 155, and 134)
#2) assess correlation among predictors
#3) construct models for RM 74 and assess assumptions
#4) conduct model selection for RM 74 and determine effects
#5) plot effects for variables predicting RM 74 drying
#6) construct models for RM 80 and assess assumptions
#7) conduct model selection for RM 80 and determine effects
#8) construct models for RM 155 and assess assumptions
#9) conduct model selection for RM 80 and determine effects
#10)construct models for RM 134 and assess assumptions
#11)construct models that incorporate time for all river miles and river miles 74-100 


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

#1) Load and make data sets ####
resp_dat <- read.csv('Data/Processed/PersistenceDryRM.csv') 
resp_dat <- resp_dat %>% 
  group_by(RM) %>% 
  arrange(Year)

  #data for river mile 74 and 100
resp_dat74_100 <- resp_dat %>% 
  filter(RM>=74 & RM<=100) %>% 
  group_by(RM) %>% 
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
dat_full <- resp_dat %>%
  left_join(pred_gages_sum, by=c("Year")) %>% 
  left_join(otowi, by=c("Year")) %>% 
  rename(DaysDry=Sum_days_rm_dry, Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = Index_kaf, RiverMile=RM) %>% 
  arrange(RiverMile)

#river miles 74 to 100
dat_full2 <- resp_dat74_100 %>%
  left_join(pred_gages_sum, by=c("Year")) %>% 
  left_join(otowi, by=c("Year"))%>% 
  rename(DaysDry=Sum_days_rm_dry, Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = Index_kaf, RiverMile=RM) %>% 
  arrange(RiverMile)

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

#2) Assess predictor correlations ####

pairs.panels(dat_74[,3:6], scale=T)
pairs.panels(dat_74[,3:6], scale=F) #SanAcaciaGage and Otowi 0.96, SanAcaciaGage and SanAcaciaDiversion and Otowi 0.58 and 0.55, respectively 
                                   #Because of the high correlations I removed SanAcaciaGage from further model development.
                                   #However, it might be interesting to retain it and remove

#3) Construct models for RM 74 and assess assumptions ####  
mod74_null <- lme(DaysDry ~ 1,
                   data = dat_74, 
                   method = "ML",
                   random = ~1|Year)
mod74_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_74, 
                   method = "ML",
                   random = ~1|Year)
mod74_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_74, 
                 method = "ML",
                 random = ~1|Year)
mod74_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_74, 
                 method = "ML",
                 random = ~1|Year)
mod74_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_74, 
                 method = "ML",
                 random = ~1|Year)
mod74_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_74, 
              method = "ML",
              random = ~1|Year)
mod74_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_74, 
              method = "ML",
              random = ~1|Year)
mod74_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_74, 
              method = "ML",
              random = ~1|Year)

# extract and assess residuals
par(mfrow=c(1,3)) 
qqnorm(resid(mod74_ID_SD_OT), main = "RM 74 mod_ID_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod74_ID_SD_OT))$statistic,2)))
qqline(resid(mod74_ID_SD_OT))
plot(resid(mod74_ID_SD_OT) ~ fitted(mod74_ID_SD_OT)) 
Acf(resid(mod74_ID_SD_OT), main = "")

par(mfrow=c(1,3)) 
qqnorm(resid(mod74_ID_SD), main = "RM 74 mod_ID_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod74_ID_SD))$statistic,2)))
qqline(resid(mod74_ID_SD))
plot(resid(mod74_ID_SD, type = "normalized") ~fitted(mod74_ID_SD)) 
Acf(resid(mod74_ID_SD), main = "")

par(mfrow=c(1,3))
qqnorm(resid(mod74_ID_OT), main = "RM 74 mod_ID_OT",
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod74_ID_OT))$statistic,2)))
qqline(resid(mod74_ID_OT))
plot(resid(mod74_ID_OT) ~ fitted(mod74_ID_OT)) 
Acf(resid(mod74_ID_OT), main = "")

par(mfrow=c(1,3))
qqnorm(resid(mod74_SD_OT), main = "RM 74 mod_SD_OT",
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod74_SD_OT))$statistic,2)))
qqline(resid(mod74_SD_OT))
plot(resid(mod74_SD_OT) ~fitted(mod74_SD_OT)) 
Acf(resid(mod74_SD_OT), main = "")

par(mfrow=c(1,3)) 
qqnorm(resid(mod74_ID), main = "RM 74 mod_ID",
       xlab = paste("shapiro test: ", round(shapiro.test(resid(mod74_ID))$statistic, 2)))
qqline(resid(mod74_ID))
plot(resid(mod74_ID) ~fitted(mod74_ID)) 
Acf(resid(mod74_ID), main = "")

par(mfrow=c(1,3)) 
qqnorm(resid(mod74_SD), main = "RM 74 mod_SD",
       xlab = paste("shapiro test: ", round(shapiro.test(resid(mod74_SD))$statistic, 2)))
qqline(resid(mod74_SD))
plot(resid(mod74_SD) ~fitted(mod74_SD)) 
Acf(resid(mod74_SD), main = "")

par(mfrow=c(1,3)) 
qqnorm(resid(mod74_OT), main = "RM 74 mod_OT",
       xlab = paste("shapiro test: ", round(shapiro.test(resid(mod74_OT))$statistic, 2)))
qqline(resid(mod74_OT))
plot(resid(mod74_OT) ~fitted(mod74_OT)) 
Acf(resid(mod74_OT), main = "")

#4) Conduct model selection for RM 74 and determine effects #####
tbAIC <- as.data.frame(bbmle::AICtab(mod74_null, mod74_ID_SD_OT, mod74_ID_SD, mod74_ID_OT, mod74_SD_OT, mod74_ID, mod74_SD, mod74_OT)) %>% 
  rownames_to_column("Model")

#since there were only 18 data points I relied more on AICc and BIC 
tbAICc <- as.data.frame(bbmle::AICctab(mod74_null, mod74_ID_SD_OT, mod74_ID_SD, mod74_ID_OT, mod74_SD_OT, mod74_ID, mod74_SD, mod74_OT)) %>% 
  rownames_to_column("Model")
tbBIC <- as.data.frame(bbmle::BICtab(mod74_null, mod74_ID_SD_OT, mod74_ID_SD, mod74_ID_OT, mod74_SD_OT, mod74_ID, mod74_SD, mod74_OT)) %>% 
  rownames_to_column("Model")
mod74_tb <- tbAICc %>% 
  left_join(tbBIC, by="Model") %>% 
  rename(df_AIC=df.x, df_BIC=df.y) %>% 
  mutate(across(2:4, round, 1))
mod74_tb

#assess confidence intervals and p-values for interpretable models ID_OT, ID_SD, SD_OT ID_SD_OT, ID, OT, 
car::Anova(mod74_ID_OT, type = "III") 
(CI_mod74_ID_OT <- intervals(mod74_ID_OT)) 

car::Anova(mod74_ID_SD, type = "III") 
(CI_mod74_ID_SD <- intervals(mod74_ID_SD)) 

car::Anova(mod74_SD_OT, type = "III") 
(CI_mod74_SD_OT <- intervals(mod74_SD_OT)) 

car::Anova(mod74_ID_SD_OT, type = "III") 
(CI_mod74_ID_SD_OT <- intervals(mod74_ID_SD_OT)) 

car::Anova(mod74_ID_SD_OT, type = "III") 
(CI_mod74_ID_SD_OT <- intervals(mod74_ID_SD_OT)) 

car::Anova(mod74_ID_SD_OT, type = "III") 
(CI_mod74_ID_SD_OT <- intervals(mod74_ID_SD_OT)) 

car::Anova(mod74_ID_SD_OT, type = "III") 
(CI_mod74_ID_SD_OT <- intervals(mod74_ID_SD_OT)) 

#interpreted models with Isleta and Otowi individual as they were only variables that were significant and removed the scaling
#so the estimates were interpretable

mod74_ID_ns <- lme(DaysDry ~ Isleta_div_sum,
                data = dat_74, 
                method = "ML",
                random = ~1|Year)
mod74_OT_ns <- lme(DaysDry ~ OtowiIndex,
                data = dat_74, 
                method = "ML",
                random = ~1|Year)

car::Anova(mod74_ID_ns, type = "III") #p-value significant
(CI_mod74_ID_ns <- intervals(mod74_ID_ns)) #Does not overlap 0

car::Anova(mod74_OT_ns, type = "III") #p-value significant
(CI_mod74_OT_ns <- intervals(mod74_OT_ns)) #Does not overlap 0

#5) Plot effects for variables predicting RM 74 drying ####

#plot of mod74_ID effects no scaling
effects_Isleta_b <- effects::effect(term="Isleta_div_sum", mod=mod74_ID_ns)
summary(effects_Isleta_b)
x_Isleta_b <- as.data.frame(effects_Isleta_b)

plot_ID <- ggplot()+
  geom_point(data=dat_80, aes(Isleta_div_sum, DaysDry, color = "black"))+
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
  ylim(-40,150)+
  scale_color_manual(values = c("black"), name = "", labels=(c("Isleta diversion")))

#plot of mod_OT
effects_Otowi_a <- effects::effect(term="OtowiIndex", mod=mod74_OT_ns)
summary(effects_Otowi_a)
effects_Otowi_a <- as.data.frame(effects_Otowi_a)

plot_OT <-ggplot()+
  geom_point(data=dat_155, aes(OtowiIndex, DaysDry, color = "grey"))+
  geom_point(data=effects_Otowi_a, aes(x=OtowiIndex, y=fit), color = "grey")+
  geom_line(data=effects_Otowi_a, aes(x=OtowiIndex, y=fit), color = "grey")+
  geom_ribbon(data=effects_Otowi_a, aes(x=OtowiIndex, ymin=lower, ymax=upper), alpha=0.3, fill="grey")+
  labs(x="Annual delivery (kaf)", y="Days in a year river mile 74 was dry")+
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1))+
  ylim(-40,150)+
  scale_color_manual(values = c("grey"), name = "", labels=(c("Otowi Index")))

#combine plots into one figure and save to file
RM74plots <- plot_grid(plot_OT, plot_ID)
ggsave2("Figures/Fig4_RM74LinearModelEffects.png", RM74plots) #saving as a pdf does not seem to fully function


#6) Construct models for RM 80 and assess assumptions####
mod80_null <- lme(DaysDry ~ 1,
                data = dat_80, 
                method = "ML",
                random = ~1|Year)
mod80_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_80, 
                    method = "ML",
                    random = ~1|Year)
mod80_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_80, 
                 method = "ML",
                 random = ~1|Year)
mod80_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_80, 
                 method = "ML",
                 random = ~1|Year)
mod80_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_80, 
                 method = "ML",
                 random = ~1|Year)
mod80_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_80, 
              method = "ML",
              random = ~1|Year)
mod80_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_80, 
              method = "ML",
              random = ~1|Year)
mod80_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_80, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(3,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod80_ID_SD_OT), main = "RM 80 mod_ID_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_ID_SD_OT))$statistic,2)))
qqline(resid(mod80_ID_SD_OT))
plot(resid(mod80_ID_SD_OT) ~fitted (mod80_ID_SD_OT)) 
Acf(resid(mod80_ID_SD_OT), main = "")

qqnorm(resid(mod80_ID_SD), main = "RM 80 mod_ID_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_ID_SD))$statistic,2)))
qqline(resid(mod80_ID_SD))
plot(resid(mod80_ID_SD) ~fitted(mod80_ID_SD)) 
Acf(resid(mod80_ID_SD), main = "")

qqnorm(resid(mod80_ID_OT), main = "RM 80 mod_ID_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_ID_OT))$statistic,2)))
qqline(resid(mod80_ID_OT))
plot(resid(mod80_ID_OT) ~fitted(mod80_ID_OT)) 
Acf(resid(mod80_ID_OT), main = "")

par(mfrow=c(4,3))
qqnorm(resid(mod80_SD_OT), main = "RM 80 mod_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_SD_OT))$statistic,2)))
qqline(resid(mod80_SD_OT))
plot(resid(mod80_SD_OT) ~fitted(mod80_SD_OT)) 
Acf(resid(mod80_SD_OT), main = "")

qqnorm(resid(mod80_ID), main = "RM 80 mod_ID", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_ID))$statistic,2)))
qqline(resid(mod80_ID))
plot(resid(mod80_ID) ~fitted(mod80_ID)) 
Acf(resid(mod80_ID), main = "")

qqnorm(resid(mod80_SD), main = "RM 80 mod_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_SD))$statistic,2)))
qqline(resid(mod80_SD))
plot(resid(mod80_SD) ~fitted(mod80_SD)) 
Acf(resid(mod80_SD), main = "")

qqnorm(resid(mod80_OT), main = "RM 80 mod_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod80_OT))$statistic,2)))
qqline(resid(mod80_OT))
plot(resid(mod80_OT) ~fitted(mod80_OT)) 
Acf(resid(mod80_OT), main = "")

#7) Conduct model selection for RM 80 and determine effects #####
tbAIC <- as.data.frame(bbmle::AICtab(mod80_null, mod80_ID_SD_OT, mod80_ID_SD, mod80_ID_OT, mod80_SD_OT, mod80_ID, mod80_SD, mod80_OT)) %>% 
  rownames_to_column("Model")

#since there were only 18 data points I relied more on AICc and BIC 
tbAICc <- as.data.frame(bbmle::AICctab(mod80_null, mod80_ID_SD_OT, mod80_ID_SD, mod80_ID_OT, mod80_SD_OT, mod80_ID, mod80_SD, mod80_OT)) %>% 
  rownames_to_column("Model")
tbBIC <- as.data.frame(bbmle::BICtab(mod80_null, mod80_ID_SD_OT, mod80_ID_SD, mod80_ID_OT, mod80_SD_OT, mod80_ID, mod80_SD, mod80_OT)) %>% 
  rownames_to_column("Model")
mod80_tb <- tbAICc %>% 
  left_join(tbBIC, by="Model") %>% 
  rename(df_AIC=df.x, df_BIC=df.y) %>% 
  mutate(across(2:4, round, 1))
mod80_tb

#assess confidence intervals and p-values for interpretable models ID, OT, ID_OT, ID_SD
car::Anova(mod80_ID, type = "III")
(CI_mod80_ID <- intervals(mod80_ID)) 

car::Anova(mod80_OT, type = "III") 
(CI_mod80_OT <- intervals(mod80_OT)) 

car::Anova(mod80_ID_OT, type = "III") 
(CI_mod80_ID_OT <- intervals(mod80_ID_OT)) 

car::Anova(mod80_ID_SD, type = "III") 
(CI_mod80_ID_SD <- intervals(mod80_ID_SD)) 

#8) Construct models for RM 155 and assess assumptions####
mod155_null <- lme(DaysDry ~ 1,
                data = dat_155, 
                method = "ML",
                random = ~1|Year)
mod155_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_155, 
                    method = "ML",
                    random = ~1|Year)
mod155_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_155, 
                 method = "ML",
                 random = ~1|Year)
mod155_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_155, 
                 method = "ML",
                 random = ~1|Year)
mod155_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_155, 
                 method = "ML",
                 random = ~1|Year)
mod155_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_155, 
              method = "ML",
              random = ~1|Year)
mod155_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_155, 
              method = "ML",
              random = ~1|Year)
mod155_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_155, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(3,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod155_ID_SD_OT), main = "RM 155 mod_ID_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_ID_SD_OT))$statistic,2)))
qqline(resid(mod155_ID_SD_OT))
plot(resid(mod155_ID_SD_OT) ~fitted (mod155_ID_SD_OT)) 
Acf(resid(mod155_ID_SD_OT), main = "")

qqnorm(resid(mod155_ID_SD), main = "RM 155 mod_ID_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_ID_SD))$statistic,2)))
qqline(resid(mod155_ID_SD))
plot(resid(mod155_ID_SD) ~fitted(mod155_ID_SD)) 
Acf(resid(mod155_ID_SD), main = "")

qqnorm(resid(mod155_ID_OT), main = "RM 155 mod_ID_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_ID_OT))$statistic,2)))
qqline(resid(mod155_ID_OT))
plot(resid(mod155_ID_OT) ~fitted(mod155_ID_OT)) 
Acf(resid(mod155_ID_OT), main = "")

par(mfrow=c(4,3))
qqnorm(resid(mod155_SD_OT), main = "RM 155 mod_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_SD_OT))$statistic,2)))
qqline(resid(mod155_SD_OT))
plot(resid(mod155_SD_OT) ~fitted(mod155_SD_OT)) 
Acf(resid(mod155_SD_OT), main = "")

qqnorm(resid(mod155_ID), main = "RM 155 mod_ID", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_ID))$statistic,2)))
qqline(resid(mod155_ID))
plot(resid(mod155_ID) ~fitted(mod155_ID)) 
Acf(resid(mod155_ID), main = "")

qqnorm(resid(mod155_SD), main = "RM 155 mod_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_SD))$statistic,2)))
qqline(resid(mod155_SD))
plot(resid(mod155_SD) ~fitted(mod155_SD)) 
Acf(resid(mod155_SD), main = "")

qqnorm(resid(mod155_OT), main = "RM 155 mod_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod155_OT))$statistic,2)))
qqline(resid(mod155_OT))
plot(resid(mod155_OT) ~fitted(mod155_OT)) 
Acf(resid(mod155_OT), main = "")

#9) Conduct model selection for RM 155 and determine effects #####
tbAIC <- as.data.frame(bbmle::AICtab(mod155_null, mod155_ID_SD_OT, mod155_ID_SD, mod155_ID_OT, mod155_SD_OT, mod155_ID, mod155_SD, mod155_OT)) %>% 
  rownames_to_column("Model")

#since there were only 18 data points I relied more on AICc and BIC 
tbAICc <- as.data.frame(bbmle::AICctab(mod155_null, mod155_ID_SD_OT, mod155_ID_SD, mod155_ID_OT, mod155_SD_OT, mod155_ID, mod155_SD, mod155_OT)) %>% 
  rownames_to_column("Model")
tbBIC <- as.data.frame(bbmle::BICtab(mod155_null, mod155_ID_SD_OT, mod155_ID_SD, mod155_ID_OT, mod155_SD_OT, mod155_ID, mod155_SD, mod155_OT)) %>% 
  rownames_to_column("Model")
mod155_tb <- tbAICc %>% 
  left_join(tbBIC, by="Model") %>% 
  rename(df_AIC=df.x, df_BIC=df.y) %>% 
  mutate(across(2:4, round, 1))
mod155_tb

#assess confidence intervals and p-values for interpretable models ID, OT, ID_OT, ID_SD

car::Anova(mod155_OT, type = "III") 
(CI_mod155_OT <- intervals(mod155_OT)) 

#10) Construct models for RM 134 and assess assumptions####
mod134_null <- lme(DaysDry ~ 1,
                data = dat_134, 
                method = "ML",
                random = ~1|Year)
mod134_ID_SD_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_134, 
                    method = "ML",
                    random = ~1|Year)
mod134_ID_SD <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum),
                 data = dat_134, 
                 method = "ML",
                 random = ~1|Year)
mod134_ID_OT <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex),
                 data = dat_134, 
                 method = "ML",
                 random = ~1|Year)
mod134_SD_OT <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
                 data = dat_134, 
                 method = "ML",
                 random = ~1|Year)
mod134_ID <- lme(DaysDry ~ scale(Isleta_div_sum),
              data = dat_134, 
              method = "ML",
              random = ~1|Year)
mod134_SD <- lme(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex),
              data = dat_134, 
              method = "ML",
              random = ~1|Year)
mod134_OT <- lme(DaysDry ~ scale(OtowiIndex),
              data = dat_134, 
              method = "ML",
              random = ~1|Year)

par(mfrow=c(3,3)) #Seems pretty good maybe off on heteroscedasticity
qqnorm(resid(mod134_ID_SD_OT), main = "RM 134 mod_ID_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_ID_SD_OT))$statistic,2)))
qqline(resid(mod134_ID_SD_OT))
plot(resid(mod134_ID_SD_OT) ~fitted (mod134_ID_SD_OT)) 
Acf(resid(mod134_ID_SD_OT), main = "")

qqnorm(resid(mod134_ID_SD), main = "RM 134 mod_ID_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_ID_SD))$statistic,2)))
qqline(resid(mod134_ID_SD))
plot(resid(mod134_ID_SD) ~fitted(mod134_ID_SD)) 
Acf(resid(mod134_ID_SD), main = "")

qqnorm(resid(mod134_ID_OT), main = "RM 134 mod_ID_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_ID_OT))$statistic,2)))
qqline(resid(mod134_ID_OT))
plot(resid(mod134_ID_OT) ~fitted(mod134_ID_OT)) 
Acf(resid(mod134_ID_OT), main = "")

par(mfrow=c(4,3))
qqnorm(resid(mod134_SD_OT), main = "RM 134 mod_SD_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_SD_OT))$statistic,2)))
qqline(resid(mod134_SD_OT))
plot(resid(mod134_SD_OT) ~fitted(mod134_SD_OT)) 
Acf(resid(mod134_SD_OT), main = "")

qqnorm(resid(mod134_ID), main = "RM 134 mod_ID", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_ID))$statistic,2)))
qqline(resid(mod134_ID))
plot(resid(mod134_ID) ~fitted(mod134_ID)) 
Acf(resid(mod134_ID), main = "")

qqnorm(resid(mod134_SD), main = "RM 134 mod_SD", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_SD))$statistic,2)))
qqline(resid(mod134_SD))
plot(resid(mod134_SD) ~fitted(mod134_SD)) 
Acf(resid(mod134_SD), main = "")

qqnorm(resid(mod134_OT), main = "RM 134 mod_OT", 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod134_OT))$statistic,2)))
qqline(resid(mod134_OT))
plot(resid(mod134_OT) ~fitted(mod134_OT)) 
Acf(resid(mod134_OT), main = "")

#11) model building and testing datasets with more river miles using lme Correlation Structures #####

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

mod_full_AR1 <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                                  data = dat_full, 
                                  method = "ML",
                                  correlation = corAR1(form = ~t|RiverMile),
                                  random = ~1|RiverMile)

 #tried p=1 and 2, 3 gave me a convergence error; tried q=2,3, 1 and values of 
mod_full_ARMAp2q1 <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                    data = dat_full, 
                    method = "ML",
                    correlation = corARMA(value=c(-0.3,-0.2,0.3,0.8),
                                         p=2, q=2, form = ~t|RiverMile),
                    random = ~1|RiverMile)

par(mfrow=c(1,3))
qqnorm(resid(mod_full_un, type = "normalized"), main = "All river miles mod Iselta + San Acacia + \n Otowi 'Year'= random",
       xlab = paste("shapiro test: ", round(shapiro.test(resid(mod_full_un, type = "normalized"))$statistic,2)))
qqline(resid(mod_full_un))
plot(resid(mod_full_un, type = "normalized")~fitted(mod_full_cs), main = "")
Acf(resid(mod_full_un))


#River miles 74-100 
mod_full2_un <- lme(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_full2, 
                   method = "ML",
                   random = ~1|RiverMile)
   #qqplot and heteroscedasticity better than full river mile set but acf remains strong in 3 and 4 lags
mod_full2_cs <- lme(log(DaysDry+1) ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex),
                   data = dat_full2, 
                   method = "ML",
                   correlation = corCompSymm(form = ~t|RiverMile))
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
qqnorm(resid(mod_full2_un, type = "normalized"), main = "River miles 74-100 mod Iselta + San Acacia + \n Otowi 'Year'= random",
       xlab = paste("shapiro test: ", round(shapiro.test(resid(mod_full2_un, type = "normalized"))$statistic,2)))
qqline(resid(mod_full2_un))
plot(resid(mod_full2_un, type = "normalized")~fitted(mod_full2_un), main = "Heteroscedasticity")
Acf(resid(mod_full2_un))

plot(mod_full2_AR1, resid(., type = "normalized") ~ fitted(.) | RiverMile, abline = 0)
plot(mod_full2_AR1, RiverMile ~ resid(., type = "normalized"))
plot(mod_full2_AR1, log(DaysDry+1) ~ fitted(.) | RiverMile, abline = c(0,1))
