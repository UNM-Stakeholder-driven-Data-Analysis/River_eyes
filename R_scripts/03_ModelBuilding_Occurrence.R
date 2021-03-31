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

#All river miles ####
mod1 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                (1|Reach/RM) +(1|Year),family = "binomial", data = dat)

summary(mod1) #all variables significant
simulateResiduals(mod1, plot = T)
car::vif(mod1) #no variables have a vif of >10 - greatest was 4.26 (San Acacia gage)

mod2 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                (1|Reach/RM), family = "binomial", data = dat)

# RM 74 2003-2011 ####
dat2 <- dat %>% 
  filter(RM == 74) %>% 
  filter(Year < 2012)

mod3 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year),family = "binomial", data = dat2)

summary(mod3) 
simulateResiduals(mod3, plot = T)
car::vif(mod3) 

#remove riparian to see if it removes VIF issues
mod3_1 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year),family = "binomial", data = dat2)

summary(mod3_1) 
simulateResiduals(mod3_1, plot = T)
car::vif(mod3_1) 

mod3_2 <- glmer(Condition.b ~ scale(Mean_cfs_SanAcacia) + scale(Rain_sum)+
                  +(1|Year),family = "binomial", data = dat2)

summary(mod3_2) 
simulateResiduals(mod3_2, plot = T)

mod3_3 <- glmer(Condition.b ~ scale(Mean_cfs_SanAcacia) 
                  +(1|Year),family = "binomial", data = dat2)

summary(mod3_3) 
simulateResiduals(mod3_3, plot = T)

mod3_4 <- glmer(Condition.b ~ scale(Rain_sum)+
                  +(1|Year),family = "binomial", data = dat2)

summary(mod3_4) 
simulateResiduals(mod3_4, plot = T)


MuMIn::AICc(mod3_1, mod3_2, mod3_3, mod3_4)
anova(mod3_1, mod3_2, mod3_3, mod3_4) #error models were not all fitted to the same size of dataset
car::Anova(mod3_1, type = "III")



# RM 74 2012-2018 ####
dat4 <- dat %>% 
  filter(RM == 74) %>% 
  filter(Year > 2011)

mod7 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year),family = "binomial", data = dat4)

summary(mod7) 
simulateResiduals(mod7, plot = T)
car::vif(mod7) 

# removed riparian because of VIF
mod8 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year),family = "binomial", data = dat4)

summary(mod8) 
simulateResiduals(mod8, plot = T)
car::vif(mod8) 


# RM 152 2003-2011 ####
dat3 <- dat %>% 
  filter(RM == 152) %>% 
  filter(Year < 2012)

mod5 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year), family = "binomial", data = dat3)

summary(mod5) #all variables significant
simulateResiduals(mod5, plot = T)
car::vif(mod5) 

# removed SanAcacia variables and ag because of VIF was 1 across the board and ag > 10 when just San Acacia removed
mod6 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(Rip_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year), family = "binomial", data = dat3)

summary(mod6) #all variables significant
simulateResiduals(mod6, plot = T)
car::vif(mod6) 

# RM 152 2012-2018
dat5 <- dat %>% 
  filter(RM == 152) %>% 
  filter(Year > 2011)

mod9 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(SanAcacia_mean_daily_div_cfs) +
                scale(Mean_cfs_SanAcacia) + scale(Ag_sum) + scale(Rip_sum)+ scale(OW_sum) + scale(Rain_sum)+
                +(1|Year), family = "binomial", data = dat5)

summary(mod9) #all variables significant
simulateResiduals(mod9, plot = T)
car::vif(mod9) 

mod10 <- glmer(Condition.b ~ scale(Isleta_mean_daily_div_cfs) + scale(Ag_sum) + scale(OW_sum) + scale(Rain_sum)+
                +(1|Year), family = "binomial", data = dat5)

summary(mod10) #all variables significant
simulateResiduals(mod10, plot = T)
car::vif(mod10) 
