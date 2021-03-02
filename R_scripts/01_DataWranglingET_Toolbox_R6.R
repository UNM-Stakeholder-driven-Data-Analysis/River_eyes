#Read me ####
#The purpose of this scripts is to load the ET toolbox data from Reach 6
#https://www.usbr.gov/uc/albuq/water/ETtoolbox/rg/newreaches/
#had to do a select all and copy in to an excel
#and then clean the data so it can be used

#load libraries####
library(tidyverse)
library(splitstackshape)
library(stringi)

##### 2018 345 obs ####
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

##### 2017 365 obs ####
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


##### 2016 366 obs ####
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
##### 2015 365 obs ####
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
##### 2014 365 obs ####
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
##### 2013 174 obs ####
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
##### 2012 366 obs ####
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
##### 2011 365 obs ####
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
##### 2010 365 obs ####
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
##### 2009 365 obs ####
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
##### 2008 366 obs ####
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
##### 2007 365 obs ####
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
##### 2006 365 obs ####
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
##### 2005 338 obs ####
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
##### 2004 364 obs ####
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
##### 2003 355 obs ####
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
#### combine years to a file ####
Et_data_R6 <- rbind(dat2018, dat2017, dat2016, dat2015, dat2014,
                    dat2013, dat2012, dat2011, dat2010, dat2009, dat2008,
                    dat2007, dat2006, dat2005, dat2004, dat2003)
Et_data_R6 <- Et_data_R6 %>% 
  mutate(Reach = "6")

write.csv(Et_data_R6,"Data/Processed/ET_data_R6.csv", row.names = FALSE)
