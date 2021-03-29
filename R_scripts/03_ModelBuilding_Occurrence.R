#Read me ####
#The purpose of this script is to construct linear models to determine whether
#the occurrence of dry river miles can be predicted

#Libraries ####
library(tidyverse)
library(lubridate)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans)
library(DHARMa)

#Load data and combine response and predictors ####
response <- read.csv("Data/Processed/DailyDryRM.csv")
predictors <- read.csv("Data/Processed/Predictors_mn_sum.csv")

dat <- response %>% 
  full_join(predictors, by=c("Date")) %>% 
  select(-c("Month", "RM_tenth")) %>% 
  mutate(RM = as.factor(RM), Reach = as.factor(Reach), Date = as.Date(Date))

#Build models and check assumptions ####
mod1 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                (1|Reach/RM),family = "binomial", data = dat)

summary(mod1) #all variables significant
simulateResiduals(mod1, plot = T) #fails assumptions
plot(resid(mod1))
car::vif(mod1) #no variables showing a vif of >10 - greatest was 4.26 (San Acacia gage)
