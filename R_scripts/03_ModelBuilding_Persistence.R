#Read me ####
#The purpose of this script is to construct linear models to predict
#the extent of drying 
#1) first with section of river RM 100 to 74 that often has drying using a lmer with year and RM as random with non_ET toolbox predictors
#2) assess temporal autocorrelation 
#3) assess correlation among predictors
#4) construct glmer with predictors
#5) assess residuals of models for heteroscedactisty/normality/autocorrelation
#6) AIC model selection
#7) look at top model summary

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
library(sjPlot)
library(effects)

#Load and wrangle data sets ####
resp_dat <- read.csv('Data/Processed/AnnualDryRM.csv')
resp_dat74 <- resp_dat %>% 
  filter(RM==74) %>% 
  arrange(Year)

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

dat74 <- cbind(resp_dat74$Sum_days_rm_dry,pred_gages_sum, otowi$Index_kaf,resp_dat74$RM) %>% 
  select(Year, everything()) %>% 
  rename(DaysDry=names(.)[2], Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = names(.)[6], RiverMile=names(.)[7])

#Assess predictor correlations and reduce ####

pairs.panels(dat74[,3:6], scale=T)
pairs.panels(dat74[,3:6], scale=F) #SanAcaciaGage and Otowi 0.96 and removed, #SanAGage and diversion/Otow 0.58 and 0.55 can I keep...I kept for now

dat74rev <- dat74 %>% 
  select(!SanAcacia_gage_sum) %>% 
  mutate(Year.f = as.character(Year))

#GLM build models mile 74 DOESN'T PASS ASSUMPTIONS ####
 #building all possible models
mod_null <- glm(DaysDry ~ 1 +(1|Year), data = dat74rev)

mod_full <- glm(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex) +(1|Year), data = dat74rev) 
qqnorm(resid(mod_full)) #Passes kind of but looks sinusoidal
qqline(resid(mod_full))
plot(resid(mod_full)) # Does not look random, rather like a sign wave
Acf(resid(mod_full)) # Passes

mod_ID_SD <- glm(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + 
                     (1|Year), data = dat74rev) 
qqnorm(resid(mod_ID_SD)) #Passes kind of but looks sinusoidal
qqline(resid(mod_ID_SD))
plot(resid(mod_ID_SD)) # Does not look random, rather like a sign wave
Acf(resid(mod_ID_SD)) #Passes

mod_ID_OT <- glm(DaysDry ~ scale(Isleta_div_sum) + scale(OtowiIndex) + 
                     (1|Year), data = dat74rev)
qqnorm(resid(mod_ID_OT)) #Passes kind of but looks sinusoidal
qqline(resid(mod_ID_OT))
plot(resid(mod_ID_OT)) # Does not look random, rather like a sign wave
Acf(resid(mod_ID_OT)) #Passes

mod_SD_OT <- glm(DaysDry ~ scale(SanAcacia_div_sum) + scale(OtowiIndex) + 
                     (1|Year), data = dat74rev)
qqnorm(resid(mod_SD_OT)) #Passes kind of but looks sinusoidal
qqline(resid(mod_SD_OT))
plot(resid(mod_SD_OT)) # Does not look random, rather like a sign wave
Acf(resid(mod_SD_OT)) #Passes

mod_ID <- glm(DaysDry ~ scale(Isleta_div_sum) + (1|Year), data = dat74rev)
qqnorm(resid(mod_ID)) # Doesn't really passes
qqline(resid(mod_ID))
plot(resid(mod_ID)) # Does not look random, rather like a sign wave
Acf(resid(mod_ID)) #Passes

mod_SD <- glm(DaysDry ~ scale(SanAcacia_div_sum) + (1|Year),  data = dat74rev)
qqnorm(resid(mod_SD)) # Not sure really passes
qqline(resid(mod_SD))
plot(resid(mod_SD)) # Does not look random, rather like a sign wave
Acf(resid(mod_SD)) #Passes

mod_OT <- glm(DaysDry ~ scale(OtowiIndex) + (1|Year),  data = dat74rev)
qqnorm(resid(mod_OT)) # Not sure if it really passes
qqline(resid(mod_OT))
plot(resid(mod_OT)) # Does not look random, rather like a sign wave
Acf(resid(mod_OT)) #Passes

#DON'T REALLY THINK ANY OF THESE PASS

#Load and wrangle data sets ####
resp_dat <- read.csv('Data/Processed/AnnualDryRM.csv')
resp_dat74_100 <- resp_dat %>% 
  filter(RM>=74 & RM<=100) %>% 
  arrange(Year)


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
  rename(Year2=Year)
  arrange(Year2)
predictors <- cbind(pred_gages_sum, otowi) %>% 
  select(!Year2)

dat_full <- resp_dat %>% 
  full_join(predictors, by =c("Year")) %>% 
  rename(DaysDry=Sum_days_rm_dry, Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = Index_kaf, RiverMile=RM)

dat_full2 <- resp_dat74_100 %>% 
  full_join(predictors, by =c("Year")) %>% 
  rename(DaysDry=Sum_days_rm_dry, Isleta_div_sum=Isleta_mean_daily_div_cfs, 
         SanAcacia_div_sum=SanAcacia_mean_daily_div_cfs, SanAcacia_gage_sum=Mean_cfs_SanAcacia,
         OtowiIndex = Index_kaf, RiverMile=RM)

#GLMER with river miles 74 to 100 and full data set DOESN'T PASS ASSUMPTIONS #####
mod_null <- glmer(DaysDry ~ 1 + (1|Year) + (1|RiverMile), family = "poisson", data = dat_full2)
simulateResiduals(mod_null, plot = T)
Acf(resid(mod_null))

mod_full <- glmer((DaysDry)^2 ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex) + 
                    (1|Year), family = "poisson", data = dat_full2) 
simulateResiduals(mod_full, plot = T) #Does not pass with raw, log, or square root and too few data points
Acf(resid(mod_full)) #Does not pass

mod_null <- glmer(DaysDry ~ 1 + (1|Year) + (1|RiverMile), family = "poisson", data = dat_full2)
simulateResiduals(mod_null, plot = T)
Acf(resid(mod_null))

mod_full <- glmer((DaysDry)^2 ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + scale(OtowiIndex) + 
                    (1|Year), family = "poisson", data = dat_full2) 
simulateResiduals(mod_full, plot = T) #Does not pass with raw, log, or square root and too few data points
Acf(resid(mod_full)) #Does not pass

#LMER DOESN'T work ##### 
#mod_ID_SD <- lmer(DaysDry ~ scale(Isleta_div_sum) + scale(SanAcacia_div_sum) + 
                   # (1|Year.f), data = dat74rev) 
#"ERROR: NUMBER OF LEVELS OF EACH GROUPING FACTOR MUST BE <NUMBER OF OBSERVATIONS (PROBLEMS:YEAR)
