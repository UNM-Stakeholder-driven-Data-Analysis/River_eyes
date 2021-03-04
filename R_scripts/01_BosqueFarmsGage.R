#Read Me####
#The purpose of this script is to format the data from the USGS gage at Bosque Farms 
#https://waterdata.usgs.gov/nwis/inventory/?site_no=08331160&agency_cd=USGS
#Data at this site was only available since 2006-03-16

#Libraries####
library(tidyverse)
library(splitstackshape)
library(stringi)

#Load data####
dat <- read_csv("Data/Raw/BosqueFarmsGage2007_2018.csv")

#Format to columns####
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

#write to file
write.csv(dat,"Data/Processed/BosqueGage2007_2018.csv",row.names = FALSE)


