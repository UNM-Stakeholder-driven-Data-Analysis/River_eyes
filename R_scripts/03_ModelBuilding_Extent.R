#Read me ####
#The purpose of this script is to construct linear models to predict
#the daily extent of drying in the Rio Grande between Isleta diversion dam
#and Elephant Butte
#because the ET toolbox changed modeling methodology after 2012 I used the last 2012-2018 and am calling it "contemporary"

#Libraries ####
library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)
library(xts)
library(WaveletComp)
library(nlme)
library(MARSS)
library(beepr)
library(visreg)
library(psych)

#Load data and combine response and predictors ####
    #summed the extent dry by reach for each day
resp_contemp <- read.csv("Data/Processed/DailyExtentDry.csv") %>% 
  group_by(Date) %>% 
  summarise(across(DistanceDry, sum)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  filter(Year >= 2012)

pred_contemp <- read.csv("Data/Processed/Predictors_mn_sum.csv") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = lubridate::year(Date)) %>% 
  filter(Year >= 2012)

dat_contemp <- resp_contemp %>% 
  full_join(pred_contemp) %>% 
  select(Date:Mean_cfs_SanAcacia, Ag_mn:Rain_sum, everything()) %>% 
  rename(Isleta_div = Isleta_mean_daily_div_cfs, SanA_div = SanAcacia_mean_daily_div_cfs,
         SanA_gage = Mean_cfs_SanAcacia) %>% 
  select(Date:Rain_mn)

#Fill NAs with spline interpolation ####
 #count number of NAs in predictors -Isleta diversion has 584 and SanAcacia diversion has 672 the gage only has 1 and ET toolbox each have 20
sum(is.na(dat_contemp$Isleta_div)); sum(is.na(dat_contemp$SanA_div)); sum(is.na(dat_contemp$SanA_gage))
sum(is.na(dat_contemp$Ag_mn));sum(is.na(dat_contemp$Rip_mn)); sum(is.na(dat_contemp$OW_mn)); sum(is.na(dat_contemp$Rain_mn))

 #check percent of dataset with NAs - 23% Isleta and 26% SanA_div
sum(is.na(dat_contemp$Isleta_div))/nrow(dat_contemp)*100 
sum(is.na(dat_contemp$SanA_div))/nrow(dat_contemp)*100 

  #making a vector of variables to automate tasks
varz <- c("Isleta_div", "SanA_div", "SanA_gage", "Ag_mn", "Rip_mn", "OW_mn", "Rain_mn")

  #fill gaps with spline interpolation with max gap of 5 days
list_filled = list()
for(v in varz){
  result_nested <- list()
  tryCatch({
      temp = dat_contemp %>% select(Date, all_of(v))
      ts.temp<-read.zoo(temp, index.column=1, format="%Y-%m-%d")                # Make univariate zoo time series #
      splineinterp = na.spline(ts.temp, na.rm = T, maxgap = 3)                  # Apply NA interpolation method with a possible gap of 3 days
      splineinterp_df = as.data.frame(splineinterp)                             # revert back to df
      splineinterp_df$date_timeAK = as.Date(row.names(splineinterp_df))
      names(splineinterp_df) = c(paste(colnames(temp)[2],"filled",sep="_"),
                                 colnames(temp)[1])
          }, error=function(e){cat("Error in ",conditionMessage(e), "\n")})
    result_nested = splineinterp_df
  list_filled[[v]] = result_nested
}

   # combine site lists
dat_filled <-  plyr::join_all(list_filled, 
                            by = c("Date"), 
                            match="first")

   # join filled data to unfilled data
dat_filled <-  left_join(dat_contemp, dat_filled, by = c("Date"))         #has value in scientific notation for Isleta diversion
dat_filled <- dat_filled %>% 
  mutate(Isleta_div_filled = if_else(Isleta_div_filled < 0, 0, Isleta_div_filled))

#count number of NAs in predictors - Isleta from 584 to 357 (14%), SanA_div 672 to 634 (25%), filled only NA in SanA_gage and none for ET toolbox
sum(is.na(dat_filled$Isleta_div_filled)); sum(is.na(dat_filled$SanA_div_filled)); sum(is.na(dat_filled$SanA_gage_filled))
sum(is.na(dat_filled$Ag_mn_filled)); sum(is.na(dat_filled$Rip_mn_filled)); sum(is.na(dat_filled$OW_mn_filled)); 
sum(is.na(dat_filled$Rain_mn_filled))

#check percent of dataset with NAs - 
sum(is.na(dat_filled$Isleta_div_filled))/nrow(dat_filled)*100 
sum(is.na(dat_filled$SanA_div_filled))/nrow(dat_filled)*100 
  
   # check gap-filling with plots
ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=Isleta_div_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=Isleta_div), color="red")

ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=SanA_div_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=SanA_div), color="red")

ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=SanA_gage_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=SanA_gage), color="red")

ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=Ag_mn_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=Ag_mn), color="red")

ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=Rip_mn_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=Rip_mn), color="red")

ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=OW_mn_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=OW_mn), color="red")

ggplot() +
  geom_line(data = dat_filled,
            aes(x=Date, y=Rain_mn_filled), color="blue") +
  geom_line(data = dat_filled,
            aes(x=Date, y=Rain_mn), color="red")
 
#nlme regression ####
dat_filled %>% arrange(Date)

# figure out the most contiguous time steps 
summary(dat_filled %>% na.contiguous(Isleta_div) %>% select(Date))
summary(dat_filled %>% na.contiguous(SanA_div_filled) %>% select(Date))
summary(dat_filled %>% na.contiguous(SanA_gage_filled) %>% select(Date))
summary(dat_filled %>% na.contiguous(Ag_mn_filled) %>% select(Date)) #this would be the same for all ET toolbox
