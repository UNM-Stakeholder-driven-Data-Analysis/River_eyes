#Read me ####
#The purpose of this script is to construct linear models to predict
#the extent of drying

#Libraries ####
library(tidyverse)
library(lubridate)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans)
library(DHARMa)

#Load data and combine response and predictors ####
    #summed the extent dry by reach for each day
response <- read.csv("Data/Processed/DailyExtentDry.csv") %>% 
  group_by(Date) %>% 
  summarise(across(DistanceDry, sum)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  filter(Year < 2012)

predictors <- read.csv("Data/Processed/Predictors_mn_sum.csv") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  filter(Year < 2012)

dat <- response %>% 
  full_join(predictors, by=c("Date")) %>% 
  select(Date:Mean_cfs_SanAcacia, Ag_mn:Rain_sum, everything()) %>% 
  rename(Isleta_div = Isleta_mean_daily_div_cfs, SanA_div = SanAcacia_mean_daily_div_cfs,
         SanA_gage = Mean_cfs_SanAcacia)

#Initial plots
par(mfrow=c(2,1))
plot(dat$DistanceDry~dat$Date, main = "Distance Dry by Day")
hist(dat$DistanceDry, main = "Histogram - Daily Distances River Dry")

par(mfrow=c(2,4))
plot(dat$Isleta_div~dat$DistanceDry, main="IsletaDiv")
plot(dat$SanA_div~dat$DistanceDry, main="SanAcaciaDiv")
plot(dat$SanA_gage~dat$DistanceDry, main="SanAcaciaGage")
plot(dat$Ag_sum~dat$DistanceDry, main="Ag")
plot(dat$Rip_sum~dat$DistanceDry, main="Riparian")
plot(dat$OW_sum~dat$DistanceDry, main="OpenWater")
plot(dat$Rain_sum~dat$DistanceDry, main="Rain")

ggplot(predictors, aes(x=Date, y=Ag_sum))+
  geom_point()

#Build models and check assumptions
mod1 <- lmer(log(DistanceDry+1) ~ Isleta_div + SanA_div + SanA_gage + Ag_mn + Rip_mn + OW_mn +
              Rain_mn + (1|Year), data=dat, REML = FALSE)

 #check assumptions
par(mfrow=c(1,1))
simulateResiduals(mod1, plot = T)

par(mfrow=c(1,2))
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))

summary(mod1)
car::vif(mod1)

#remove Rip_mn because of vif
mod2 <- lmer(log(DistanceDry+1) ~ Isleta_div + SanA_div + SanA_gage +  Ag_mn+ OW_mn +
               Rain_mn + (1|Year), data=dat, REML = FALSE)

simulateResiduals(mod2, plot = T)
plot(mod2)
qqnorm(resid(mod2))
qqline(resid(mod2))
summary(mod2)
car::vif(mod2)
