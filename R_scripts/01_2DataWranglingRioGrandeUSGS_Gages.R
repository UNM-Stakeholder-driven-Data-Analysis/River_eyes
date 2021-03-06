#Read Me####
#The purpose of this script is to format the data from the USGS gage at Bosque Farms 
#https://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=08331160
#Data at this site was only available since 2006-03-16 AND USGS gage at San Acacia 
#https://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=08354900 and data for this site was available
#since 1990 so I got 2003-01-01 to 2018-12-31 data. Both sets were from the daily data summary statistic 
#available through USGS


#Libraries####
library(tidyverse)
library(splitstackshape)
library(stringi)

#Load data Bosque Farms gage data####
dat <- read_csv("Data/Raw/BosqueFarmsGage2007_2018.csv")

#Format Bosque Farms gage data to columns####
#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
dat <- as.data.frame(dat)

#remove rows and columns that aren't needed
dat <- dat[-c(5:15)] 
dat <- dat[-c(1:2)] 
dat <- dat[-(1:29),]

#name columns
dat <- dat %>% 
  rename(Date=X1_03, Mean_cfs=X1_04) %>% 
  mutate(Date = as.Date(Date))

#Load data San Acacia gage data####
dat2 <- read_csv("Data/Raw/SanAcaciaGage2003_2018.csv")

#Format San Acacia gage data columns####
#rename column
colnames(dat2)[1] <- "X1"

#text to columns 
dat2 <- cSplit(dat2, "X1", sep = " ", type.convert = F)
dat2 <- as.data.frame(dat2)

#remove rows and columns that aren't needed
dat2 <- dat2[-c(5:15)] 
dat2 <- dat2[-c(1:2)] 
dat2 <- dat2[-(1:30),]

#name columns
dat2 <- dat2 %>% 
  rename(Date=X1_03, Mean_cfs=X1_04) %>% 
  mutate(Mean_cfs = as.numeric(Mean_cfs)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  rename(Mean_cfs_SanAcacia = Mean_cfs)


#join the two gages to one dataframe####
join_data <- dat2 %>% 
  left_join(dat, by="Date") %>% 
  rename(Mean_cfs_BosFarms = Mean_cfs)


#write to file
write.csv(join_data,"Data/Processed/RioGrandeGages2003_2018.csv",row.names = FALSE)