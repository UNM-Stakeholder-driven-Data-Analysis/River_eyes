#Read me ####
#The purpose of this scripts is to load the ET toolbox data from Reach 5
#https://www.usbr.gov/uc/albuq/water/ETtoolbox/rg/newreaches/
#had to do a select all and copy in to an excel
#and then clean the data so it can be used

#load libraries####
library(tidyverse)
library(splitstackshape)
library(stringi)

##### R5 2018 345 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2018.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)] 
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2018 <- dat #345 obs

##### R5 2017 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2017.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)] 
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2017 <- dat #345 obs


##### R5 2016 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2016.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)] 
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2016 <- dat #345 obs
##### R5 2015 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2015.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2015) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2015 <- dat #345 obs
##### R5 2014 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2014.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2014) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2014 <- dat #345 obs
##### R5 2013 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2013.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2013) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2013 <- dat #345 obs
##### R5 2012 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2012.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2012) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2012 <- dat #345 obs
##### R5 2011 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2011.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)] 
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2011) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2011 <- dat #345 obs
##### R5 2010 265 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2010.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2010) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2010 <- dat #345 obs
##### R5 2009 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2009.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2009) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2009 <- dat #345 obs
##### R5 2008 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2008.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2008) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2008 <- dat #345 obs
##### R5 2007 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2007.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2007) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2007 <- dat #345 obs
##### R5 2006 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2006.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2006) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2006 <- dat #345 obs
##### R5 2005 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2005.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2005) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2005 <- dat #345 obs
##### R5 2004 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2004.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2004) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2004 <- dat #345 obs
##### R5 2003 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R5/ET_toolbox_R5_2003.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2003) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2003 <- dat #345 obs
#### R5 combine years to a file ####
Et_data_R5 <- rbind(dat2018, dat2017, dat2016, dat2015, dat2014,
                    dat2013, dat2012, dat2011, dat2010, dat2009, dat2008,
                    dat2007, dat2006, dat2005, dat2004, dat2003)
Et_data_R5 <- Et_data_R5 %>% 
  mutate(Reach = "5")

write.csv(Et_data_R5,"Data/Processed/ET_data_R5.csv", row.names = FALSE)


##### R6 2018 345 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2018.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)] 
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2018 <- dat #345 obs

##### R6 2017 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2017.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)] 
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2017 <- dat #345 obs


##### R6 2016 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2016.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)] 
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2016 <- dat #345 obs
##### R6 2015 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2015.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2015) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2015 <- dat #345 obs
##### R6 2014 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2014.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2014) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2014 <- dat #345 obs
##### R6 2013 174 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2013.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2013) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2013 <- dat #345 obs
##### R6 2012 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2012.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2012) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2012 <- dat #345 obs
##### R6 2011 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2011.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)] 
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2011) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2011 <- dat #345 obs
##### R6 2010 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2010.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2010) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2010 <- dat #345 obs
##### R6 2009 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2009.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2009) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2009 <- dat #345 obs
##### R6 2008 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2008.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2008) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2008 <- dat #345 obs
##### R6 2007 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2007.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2007) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2007 <- dat #345 obs
##### R6 2006 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2006.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2006) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2006 <- dat #345 obs
##### R6 2005 338 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2005.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2005) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2005 <- dat #345 obs
##### R6 2004 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2004.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2004) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2004 <- dat #345 obs
##### R6 2003 355 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R6/ET_toolbox_R6_2003.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2003) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2003 <- dat #345 obs
#### R6 combine years to a file ####
Et_data_R6 <- rbind(dat2018, dat2017, dat2016, dat2015, dat2014,
                    dat2013, dat2012, dat2011, dat2010, dat2009, dat2008,
                    dat2007, dat2006, dat2005, dat2004, dat2003)
Et_data_R6 <- Et_data_R6 %>% 
  mutate(Reach = "6")

write.csv(Et_data_R6,"Data/Processed/ET_data_R6.csv", row.names = FALSE)

##### R7 2018 345 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2018.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:32)]
dat <- dat[-(1:18),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2018 <- dat #345 obs

##### R7 2017 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2017.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:32)]
dat <- dat[-(1:18),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2017 <- dat #345 obs


##### R7 2016 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2016.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:32)]
dat <- dat[-(1:18),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2016 <- dat #345 obs
##### R7 2015 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2015.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:32)]
dat <- dat[-(1:18),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2015) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2015 <- dat #345 obs
##### R7 2014 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2014.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2014) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2014 <- dat #345 obs
##### R7 2013 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2013.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2013) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2013 <- dat #345 obs
##### R7 2012 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2012.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2012) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2012 <- dat #345 obs
##### R7 2011 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2011.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2011) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2011 <- dat #345 obs
##### R7 2010 265 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2010.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2010) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2010 <- dat #345 obs
##### R7 2009 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2009.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2009) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2009 <- dat #345 obs
##### R7 2008 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2008.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2008) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2008 <- dat #345 obs
##### R7 2007 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2007.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2007) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2007 <- dat #345 obs
##### R7 2006 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2006.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2006) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2006 <- dat #345 obs
##### R7 2005 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2005.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2005) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2005 <- dat #345 obs
##### R7 2004 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2004.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2004) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2004 <- dat #345 obs
##### R7 2003 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R7/ET_toolbox_R7_2003.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2003) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2003 <- dat #345 obs
#### R7 combine years to a file ####
Et_data_R7 <- rbind(dat2018, dat2017, dat2016, dat2015, dat2014,
                    dat2013, dat2012, dat2011, dat2010, dat2009, dat2008,
                    dat2007, dat2006, dat2005, dat2004, dat2003)
Et_data_R7 <- Et_data_R7 %>% 
  mutate(Reach = "7")

write.csv(Et_data_R7,"Data/Processed/ET_data_R7.csv", row.names = FALSE)
##### R8 2018 345 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2018.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2018 <- dat #345 obs

##### R8 2017 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2017.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2017 <- dat #345 obs


##### R8 2016 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2016.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2016 <- dat #345 obs
##### R8 2015 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2015.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2015) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2015 <- dat #345 obs
##### R8 2014 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2014.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2014) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2014 <- dat #345 obs
##### R8 2013 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2013.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2013) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2013 <- dat #345 obs
##### R8 2012 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2012.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:20)]
dat <- dat[-(1:11),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2012) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2012 <- dat #345 obs
##### R8 2011 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2011.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2011) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2011 <- dat #345 obs
##### R8 2010 265 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2010.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2010) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2010 <- dat #345 obs
##### R8 2009 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2009.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2009) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2009 <- dat #345 obs
##### R8 2008 366 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2008.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2008) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2008 <- dat #345 obs
##### R8 2007 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2007.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2007) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2007 <- dat #345 obs
##### R8 2006 365 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2006.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:10),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2006) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2006 <- dat #345 obs
##### R8 2005 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2005.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:08),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2005) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2005 <- dat #345 obs
##### R8 2004 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2004.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2004) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2004 <- dat #345 obs
##### R8 2003 364 obs ####
dat <- read_csv("Data/Raw/ET_toolbox/ET_toolbox_R8/ET_toolbox_R8_2003.csv")
#rename column
colnames(dat)[1] <- "X1"
#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
#used to move some columns to the left that got split because of "text to columns"
dat$X1_03 <- as.numeric(dat$X1_03)
dat <- t(apply(dat,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])
}))
dat <- as.data.frame(dat)
#remove columns and rows that aren't needed
dat <- dat[-c(13:22)]
dat <- dat[-(1:8),]
#formatting for column names, dates, variable classes
dat <- dat %>% 
  drop_na() %>% 
  rename(Month=V1, Day=V2, Tot_DCU_cfs=V3, Ag_DCU_cfs=V4, Riparian_DCU_cfs=V5,
         OpenWater_DCU_cfs=V6, Urban_DCU_cfs=V7, Rain_cfs=V8, Tot_URGWOM_cfs=V9, 
         five_day_avg_URGWOM_cfs=V10, ten_day_avg_URGWOM_cfs=V11,Use_to_date_since_Jan1=V12) %>% 
  mutate(Month = str_replace(Month, pattern = "\\.", "")) %>% 
  add_column(Year = 2003) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Tot_DCU_cfs", "Ag_DCU_cfs", "Riparian_DCU_cfs",
              "OpenWater_DCU_cfs", "Urban_DCU_cfs", "Rain_cfs", "Tot_URGWOM_cfs", 
              "five_day_avg_URGWOM_cfs", "ten_day_avg_URGWOM_cfs","Use_to_date_since_Jan1"), as.numeric)
#to make month abbreviate to three letters
dat$Month.n <- as.numeric(match(dat$Month,month.abb))
#select needed columns
dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)
#to combine and write files #
dat2003 <- dat #345 obs
#### R8 combine years to a file ####
Et_data_R8 <- rbind(dat2018, dat2017, dat2016, dat2015, dat2014,
                    dat2013, dat2012, dat2011, dat2010, dat2009, dat2008,
                    dat2007, dat2006, dat2005, dat2004, dat2003)
Et_data_R8 <- Et_data_R8 %>% 
  mutate(Reach = "8")

write.csv(Et_data_R8,"Data/Processed/ET_data_R8.csv", row.names = FALSE)

#### COMBINE All REACHES TO A FILE #####
R5 <- read_csv("Data/Processed/ET_data_R5.csv")
R6 <- read_csv("Data/Processed/ET_data_R6.csv")
R7 <- read_csv("Data/Processed/ET_data_R7.csv")
R8 <- read_csv("Data/Processed/ET_data_R8.csv")

ET_Toolbox_R_6_8 <- rbind(R5, R6,R7, R8)

write.csv(ET_Toolbox_R_6_8, "Data/Processed/ET_Toolbox_R_6_8.csv", row.names = FALSE )

test <- read_csv("Data/Processed/ET_Toolbox_R_6_8.csv")



