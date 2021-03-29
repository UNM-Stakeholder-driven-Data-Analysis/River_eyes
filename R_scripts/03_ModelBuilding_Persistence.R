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
response <- read.csv("Data/Processed/AnnualDryRM.csv") %>% 
  filter(Year < 2012)

predictors <- read.csv("Data/Processed/Predictors_mn_sum.csv") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  select(Year, Isleta_mean_daily_div_cfs:Mean_cfs_SanAcacia,Ag_sum:Rain_sum, everything() ) %>% 
  filter(Year < 2012)

predictors2 <- predictors %>% 
  group_by(Year)%>% 
  summarise(across(Isleta_mean_daily_div_cfs:Rain_sum, mean, na.rm=TRUE)) %>% 
  rename(Ag_yr_mn = Ag_sum, Rip_yr_mn = Rip_sum, OW_yr_mn = OW_sum, Rain_yr_mn = Rain_sum,
         Isleta_div = Isleta_mean_daily_div_cfs, SanA_div = SanAcacia_mean_daily_div_cfs,
         SanA_gage = Mean_cfs_SanAcacia)

dat <- response %>% 
  full_join(predictors2, by=c("Year")) 

#Initial plots
par(mfrow=c(2,1))
plot(dat$Sum_days_rm_dry~dat$Year, main = "")
hist(dat$Sum_days_rm_dry, main = "")

par(mfrow=c(2,4))
plot(dat$Isleta_div~dat$Sum_days_rm_dry, main="IsletaDiv")
plot(dat$SanA_div~dat$Sum_days_rm_dry, main="SanAcaciaDiv")
plot(dat$SanA_gage~dat$Sum_days_rm_dry, main="SanAcaciaGage")
plot(dat$Ag_yr_mn~dat$Sum_days_rm_dry, main="Ag")
plot(dat$Rip_yr_mn~dat$Sum_days_rm_dry, main="Riparian")
plot(dat$OW_yr_mn~dat$Sum_days_rm_dry, main="OpenWater")
plot(dat$Rain_yr_mn~dat$Sum_days_rm_dry, main="Rain")



#Build models and check assumptions
mod1 <- lmer(log(Sum_days_rm_dry+1) ~ Isleta_div + SanA_div + SanA_gage + Ag_yr_mn + Rip_yr_mn + OW_yr_mn +
              Rain_yr_mn + (1|Year) +(1|RM), data=dat, REML = FALSE)

 #check assumptions
par(mfrow=c(1,1))
simulateResiduals(mod1, plot = T)

plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))

summary(mod1)
car::vif(mod1)

#remove variable because of vif
mod2 <- lmer(log(Sum_days_rm_dry+1) ~ Isleta_div + SanA_div + SanA_gage + Ag_yr_mn + (1|Year) +(1|RM), data=dat, REML = FALSE)

par(mfrow=c(1,1))
simulateResiduals(mod1, plot = T)

plot(mod2)
qqnorm(resid(mod2))
qqline(resid(mod2))

summary(mod2)
car::vif(mod2)
