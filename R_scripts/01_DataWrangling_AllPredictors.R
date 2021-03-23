#READ ME #####
#The purpose of this script is to combine all predictors with the exception of Otowi index
#into the same dataframe

#load libraries
library(tidyverse)

#load data sets #####

#ET toolbox: Ag depletion, riparian ET, water evap, precip
datToolbox <- read_csv("Data/Processed/ET_Toolbox_R_6_8.csv")

temp <- data.frame(rep(NA, each=5844))
colnames(temp)[1] <- "empty"
temp$"Date" <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")

datToolboxR5 <- datToolbox %>% 
  filter(Reach==5) %>% 
  select(Date, Ag_DCU_cfs:OpenWater_DCU_cfs, Rain_cfs) %>% 
  rename(Ag_DCU_cfs_5=Ag_DCU_cfs, Riparian_DCU_cfs_5=Riparian_DCU_cfs,
         OpenWater_DCU_cfs_5=OpenWater_DCU_cfs, Rain_cfs_5=Rain_cfs) 

datToolboxR5 <- temp %>% 
  left_join(datToolboxR5, by=c("Date")) %>% 
  select(Date, Ag_DCU_cfs_5:Rain_cfs_5) %>% 
  rename(Date1=Date)

datToolboxR6 <- datToolbox %>% 
  filter(Reach==6) %>% 
  select(Date, Ag_DCU_cfs:OpenWater_DCU_cfs, Rain_cfs) %>% 
  rename(Ag_DCU_cfs_6=Ag_DCU_cfs, Riparian_DCU_cfs_6=Riparian_DCU_cfs,
         OpenWater_DCU_cfs_6=OpenWater_DCU_cfs, Rain_cfs_6=Rain_cfs) 

datToolboxR6 <- temp %>% 
  left_join(datToolboxR6, by=c("Date")) %>% 
  select(Date, Ag_DCU_cfs_6:Rain_cfs_6) %>% 
  rename(Date2=Date)

datToolboxR7 <- datToolbox %>% 
  filter(Reach==7) %>% 
  select(Date, Ag_DCU_cfs:OpenWater_DCU_cfs, Rain_cfs) %>% 
  rename(Ag_DCU_cfs_7=Ag_DCU_cfs, Riparian_DCU_cfs_7=Riparian_DCU_cfs,
         OpenWater_DCU_cfs_7=OpenWater_DCU_cfs, Rain_cfs_7=Rain_cfs) 

datToolboxR7 <- temp %>% 
  left_join(datToolboxR7, by=c("Date")) %>% 
  select(Date, Ag_DCU_cfs_7:Rain_cfs_7) %>% 
  rename(Date3=Date)
  
datToolboxR8 <- datToolbox %>% 
  filter(Reach==8) %>% 
  select(Date, Ag_DCU_cfs:OpenWater_DCU_cfs, Rain_cfs) %>% 
  rename(Ag_DCU_cfs_8=Ag_DCU_cfs, Riparian_DCU_cfs_8=Riparian_DCU_cfs,
         OpenWater_DCU_cfs_8=OpenWater_DCU_cfs, Rain_cfs_8=Rain_cfs) 

datToolboxR8 <- temp %>% 
  left_join(datToolboxR8, by=c("Date")) %>% 
  select(Date, Ag_DCU_cfs_8:Rain_cfs_8) %>% 
  rename(Date4=Date)

#Diversions: these are Isleta and San Acacia
datDiversions <- read_csv("Data/Processed/Diversion_discharges.csv") %>% 
  rename(Date5=Date)

#River gages: these are Bosque Farms (only available 2007-2018) and San Acacia (full 2003-2018)
datGages <- read_csv("Data/Processed/RioGrandeGages2003_2018.csv", col_types = cols(Mean_cfs_BosFarms = col_double()))

#combine data sets ####
Predictors <- cbind(datDiversions, datGages, datToolboxR5, datToolboxR6, datToolboxR7, datToolboxR8)%>% 
  select(-c(Date1, Date2, Date3, Date4, Date5)) %>% 
  select(Date, everything())

#write to file #####
write.csv(Predictors,"Data/Processed/Predictors.csv",row.names = FALSE)

dat <- read.csv("Data/Raw/Otowi Index Supply.csv", header = FALSE)
datOtowi <- dat[-(1),]
datOtowi <- datOtowi %>% 
  rename(Year=V1,Index_kaf=V2) %>% 
  select(Year, Index_kaf) %>% 
  filter(Year>2002 & Year<2019)

write.csv(datOtowi,"Data/Processed/OtowiIndex.csv")
  
