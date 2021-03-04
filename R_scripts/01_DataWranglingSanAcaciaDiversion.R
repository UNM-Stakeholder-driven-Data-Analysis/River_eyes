#Read Me####
#The purpose of this script is to import and format
#the data from the San Acacia Diversion (identified as SNAN5)
#https://www.usbr.gov/uc/albuq/water/ETtoolbox/rg/PROD/gage/archive/gage/

#libraries####
library(readxl)
library(tidyverse)
library(splitstackshape)
library(stringi)

#load data 2003####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2003")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2003) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2003 <- dat

#load data 2004####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2004")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2004) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2004 <- dat

#load data 2005####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2005")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2005) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2005 <- dat

#load data 2006####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2006")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2006) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2006 <- dat

#load data 2007####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2007")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2007) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2007 <- dat

#load data 2008####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2008")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2008) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2008 <- dat

#load data 2009####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2009")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)

#remove rows that aren't needed
dat <- dat[-(1:5),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2009) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2009 <- dat

#load data 2010####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2010")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:9)]
dat <- dat[-(1:7),]


#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2010) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2010 <- dat

#load data 2011####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2011")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:9)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2011) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2011 <- dat

#load data 2012####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2012")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:9)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2012) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2012 <- dat

#load data 2013####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2013")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:9)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2013) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2013 <- dat

#load data 2014####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2014")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:9)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2014) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2014 <- dat

#load data 2015####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2015")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:9)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2015) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2015 <- dat

#load data 2016####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2016")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:10)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2016 <- dat

#load data 2017####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2017")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:10)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2017 <- dat

#load data 2018####
dat <- read_excel("Data/Raw/SanAcaciaDiversion2003_2018.xlsx", sheet = "2018")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:10)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2018 <- dat

#Combine all files to one csv
SanAcaciaDiv <- rbind(dat2003, dat2004, dat2005, dat2006, dat2007, dat2008, dat2009, dat2010,
                      dat2011, dat2012, dat2013, dat2014, dat2015, dat2016, dat2017, dat2018)

SanAcaciaDiv <- SanAcaciaDiv %>% 
  group_by(Date) %>% 
  summarise(Mean_daily_discharge_cfs = mean(Discharge_cfs))

write.csv(SanAcaciaDiv, "Data/Processed/SanAcaciaDiv.csv", row.names = FALSE )

