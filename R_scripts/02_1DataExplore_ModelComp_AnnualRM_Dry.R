#READ ME #####
#The purpose of this script is to use linear modeling to explore the 
#response of the number of days a river mile was dry "AnnualDryTM" from
#various predictors (ET toolbox and gage data)

#load libraries ####
library(tidyverse) 
library(lubridate)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting
library(janitor)

#format dataframe testing for RM 74 #####

#get RM 74: this RM is in Reach 7
datResp <- read_csv("Data/Processed/PersistenceDryRM.csv")
datRM_74 <- datResp %>% 
  filter(RM==74) %>% 
  rename(YearRM = Year)

#get predictors and format to annual ####
datPred <- read.csv("Data/Processed/Predictors.csv")
datPred <- datPred %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  select(!MeanPERCN:MeanCACCN) %>% 
  select(!Date) %>% 
  group_by(Year) %>% 
  summarise_at(vars(Isleta_mean_daily_div_cfs:Rain_cfs_8), 
    mean, na.rm=TRUE)
datPred[is.na(datPred)] <- NA

datPredSum <- datPred %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  select(!MeanPERCN:MeanCACCN) %>% 
  select(!Date) %>% 
  group_by(Year) %>% 
  summarise_at(vars(Isleta_mean_daily_div_cfs:Rain_cfs_8), 
               sum, na.rm=TRUE)
datPred[is.na(datPred)] <- NA

pairs(datPred) #not totally helpful but can see some multicolinearity between predictors

datOtowi <- read.csv("Data/Processed/OtowiIndex.csv")
datOtowi <- datOtowi %>% 
  select(!X) %>% 
  rename(YearOt = Year, OtowiIndex_kaf = Index_kaf)

#combine to one dataframe ####
dat <- cbind(datRM_74,datPred, datOtowi) %>% 
  select(Year, everything()) %>% 
  select(!YearRM) %>% 
  select(!YearOt)

#plots of RM 74 versus possible predictors ####

par(mfrow=c(2,4))
plot(dat$SanAcacia_mean_daily_div_cfs~dat$Sum_days_rm_dry, ylab="cfs",main = "San Acacia Diversion", 
     xlab="", pch=16, cex=1.5) 
plot(dat$Isleta_mean_daily_div_cfs~dat$Sum_days_rm_dry, ylab="cfs", main="Isleta Diversion", 
     xlab="",pch=16, cex=1.5)
plot(dat$Mean_cfs_SanAcacia~dat$Sum_days_rm_dry, ylab="cfs", main ="San Acacia Gage", xlab="",
     pch=16, cex=1.5)  
plot(dat$Ag_DCU_cfs_7~dat$Sum_days_rm_dry, ylab="cfs", main="Agriculture Depletions R7", xlab="",
     pch=16, cex=1.5)  
plot(dat$Riparian_DCU_cfs_7~dat$Sum_days_rm_dry, ylab="cfs", main="Riparian Evapotranspiration R7", 
     xlab="", pch=16, cex=1.5)  
plot(dat$OpenWater_DCU_cfs_7~dat$Sum_days_rm_dry, ylab="cfs",main="Water Evaporation R7", 
     xlab="Sum days dry rm 74", pch=16, cex=1.5)  
plot(dat$Rain_cfs_7~dat$Sum_days_rm_dry, ylab="cfs", main="Precipitation R7", 
     xlab="Sum days dry rm 74",pch=16, cex=1.5)
plot(dat$OtowiIndex_kaf~dat$Sum_days_rm_dry, ylab="cfs", main="Otowi Index", 
     xlab="Sum days dry rm 74",pch=16, cex=1.5)

#linear models with means####
par(mfrow=c(1,1))
mod <- lm(Sum_days_rm_dry ~ SanAcacia_mean_daily_div_cfs + Isleta_mean_daily_div_cfs +
            Mean_cfs_SanAcacia + Ag_DCU_cfs_7 + Riparian_DCU_cfs_7 + OpenWater_DCU_cfs_7 + 
            +Rain_cfs_7 + OtowiIndex_kaf, data=dat)
plot(mod)
Anova(mod, type=3)

car::vif(mod)

mod2 <- lm(Sum_days_rm_dry ~ Isleta_mean_daily_div_cfs + Ag_DCU_cfs_7 + Riparian_DCU_cfs_7 + OpenWater_DCU_cfs_7 
           +Rain_cfs_7, data = dat)
plot(mod2)
Anova(mod2, type=3)

car::vif(mod2)

#linear models with sums####
mod3 <- lm(Sum_days_rm_dry ~ SanAcacia_mean_daily_div_cfs + Isleta_mean_daily_div_cfs +
            Mean_cfs_SanAcacia + Ag_DCU_cfs_7 + Riparian_DCU_cfs_7 + OpenWater_DCU_cfs_7 + 
            Rain_cfs_7, data=dat)
plot(mod3)
Anova(mod3, type=3)

car::vif(mod3)

mod4 <- lm(Sum_days_rm_dry ~ SanAcacia_mean_daily_div_cfs + Isleta_mean_daily_div_cfs +
             Mean_cfs_SanAcacia + Rain_cfs_7, data=dat)
plot(mod4)
Anova(mod4, type=3)

car::vif(mod4)

#format dataframe for extent of dry river mile each day #####
datExtDry <- read.csv("Data/Processed/DailyExpansionDry.csv")
datExtDryR7 <- datExtDry %>% 
  filter(Reach==7) %>% 
  group_by(Date) %>% 
  summarise_at(vars(DistanceDry),sum, na.rm=TRUE)
  

#get predictors and combine #####
datPred <- read.csv("Data/Processed/Predictors.csv")
datPred <- datPred %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA))

dat2 <- cbind(datExtDryR7,datPred) 

# variable plots ####
par(mfrow=c(2,4))
plot(dat2$SanAcacia_mean_daily_div_cfs~dat2$DistanceDry, ylab="cfs",main = "San Acacia Diversion", 
     xlab="", pch=16, cex=1.5) 
plot(dat2$Isleta_mean_daily_div_cfs~dat2$DistanceDry, ylab="cfs", main="Isleta Diversion", 
     xlab="",pch=16, cex=1.5)
plot(dat2$Mean_cfs_SanAcacia~dat2$DistanceDry, ylab="cfs", main ="San Acacia Gage", xlab="",
     pch=16, cex=1.5)  
plot(dat2$Ag_DCU_cfs_7~dat2$DistanceDry, ylab="cfs", main="Agriculture Depletions R7", xlab="",
     pch=16, cex=1.5)  
plot(dat2$Riparian_DCU_cfs_7~dat2$DistanceDry, ylab="cfs", main="Riparian Evapotranspiration R7", 
     xlab="Extent River Dry Reach 7", pch=16, cex=1.5)  
plot(dat2$OpenWater_DCU_cfs_7~dat2$DistanceDry, ylab="cfs",main="Water Evaporation R7", 
     xlab="Extent River Dry Reach 7", pch=16, cex=1.5)  
plot(dat2$Rain_cfs_7~dat2$DistanceDry, ylab="cfs", main="Precipitation R7", 
     xlab="Extent River Dry Reach 7",pch=16, cex=1.5)

#linear model####
mod5 <- lm(DistanceDry ~ scale(SanAcacia_mean_daily_div_cfs) + scale(Isleta_mean_daily_div_cfs) +
             scale(Mean_cfs_SanAcacia) + scale(Ag_DCU_cfs_7) + scale(Riparian_DCU_cfs_7) +
             scale(OpenWater_DCU_cfs_7) + scale(Rain_cfs_7), data=dat2)
par(mfrow=c(2,2))

plot(mod5)
Anova(mod5, type=3)

car::vif(mod5)

mod6 <- lm(DistanceDry ~ scale(SanAcacia_mean_daily_div_cfs) + scale(Isleta_mean_daily_div_cfs) + 
             scale(Mean_cfs_SanAcacia) + scale(OpenWater_DCU_cfs_7) + scale(Rain_cfs_7), data=dat2)
par(mfrow=c(2,2))

plot(mod6)
Anova(mod6, type=3)