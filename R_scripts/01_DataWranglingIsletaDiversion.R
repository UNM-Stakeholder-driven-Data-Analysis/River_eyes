#Read Me####
#The purpose of this script is to import and format
#the data from the Isleta Diversion (used the largest canals BELCN + PERCN + CHICN)
#https://www.usbr.gov/uc/albuq/water/ETtoolbox/rg/PROD/gage/archive/gage/

#libraries####
library(readxl)
library(tidyverse)
library(splitstackshape)
library(stringi)

#load data 2003 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2003")

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

#load data 2004 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2004")

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

#load data 2005 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2005")

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

#load data 2006 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2006")

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

#load data 2007 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2007")

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

#load data 2008 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2008")

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

#load data 2009 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2009")

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

#load data 2010 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2010")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]


#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2010) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2010 <- dat

#load data 2011 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2011")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2011) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2011 <- dat

#load data 2012 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2012")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2012) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2012 <- dat

#load data 2013 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2013")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2013) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2013 <- dat

#load data 2014 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2014")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2014) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2014 <- dat

#load data 2015 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2015")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_01, Day=X1_02, Time=X1_03, Height_ft=X1_04, Discharge_cfs=X1_05)%>% 
  add_column(Year = 2015) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2015 <- dat

#load data 2016 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2016")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2017 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2017")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2018 BELCN####
dat <- read_excel("Data/Raw/Isleta_BELCN2003_2018.xlsx", sheet = "2018")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#Combine all BELCN files to one csv ####
BELCN <- rbind(dat2003, dat2004, dat2005, dat2006, dat2007, dat2008, dat2009, dat2010,
                      dat2011, dat2012, dat2013, dat2014, dat2015, dat2016, dat2017, dat2018)

BELCN <- BELCN %>% 
  group_by(Date) %>% 
  summarise(Mean_daily_discharge_cfs = mean(Discharge_cfs))



#load data 2003 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2003")

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

#load data 2004 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2004")

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

#load data 2005 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2005")

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

#load data 2006 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2006")

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

#load data 2007 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2007")

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

#load data 2008 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2008")

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

#load data 2009 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2009")

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

#load data 2010 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2010")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2011 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2011")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2012 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2012")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2013 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2013")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2014 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2014")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2015 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2015")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2016 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2016")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2016 <- dat

#load data 2017 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2017")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2017 <- dat

#load data 2018 PERCN####
dat <- read_excel("Data/Raw/Isleta_PERCN2003_2018.xlsx", sheet = "2018")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2018 <- dat

#Combine all PERCN files to one csv ####
PERCN <- rbind(dat2003, dat2004, dat2005, dat2006, dat2007, dat2008, dat2009, dat2010,
               dat2011, dat2012, dat2013, dat2014, dat2015, dat2016, dat2017, dat2018)

PERCN <- PERCN %>% 
  group_by(Date) %>% 
  summarise(Mean_daily_discharge_cfs = mean(Discharge_cfs))

#load data 2003 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2003")

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

#load data 2004 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2004")

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

#load data 2005 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2005")

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

#load data 2006 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2006")

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

#load data 2007 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2007")

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

#load data 2008 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2008")

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

#load data 2009 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2009")

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

#load data 2010 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2010")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2011 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2011")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2012 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2012")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2013 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2013")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2014 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2014")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2015 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2015")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2016 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2016")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2016 <- dat

#load data 2017 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2017")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2017 <- dat

#load data 2018 CHICN####
dat <- read_excel("Data/Raw/Isleta_CHICN2003_2018.xlsx", sheet = "2018")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2018 <- dat

#Combine all CHICN files to one csv ####
CHICN <- rbind(dat2003, dat2004, dat2005, dat2006, dat2007, dat2008, dat2009, dat2010,
               dat2011, dat2012, dat2013, dat2014, dat2015, dat2016, dat2017, dat2018)

CHICN <- CHICN %>% 
  group_by(Date) %>% 
  summarise(Mean_daily_discharge_cfs = mean(Discharge_cfs))



#load data 2003 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2003")

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

#load data 2004 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2004")

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

#load data 2005 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2005")

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

#load data 2006 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2006")

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

#load data 2007 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2007")

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

#load data 2008 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2008")

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

#load data 2009 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2009")

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

#load data 2010 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2010")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2011 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2011")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2012 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2012")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2013 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2013")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2014 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2014")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2015 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2015")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2016 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2016")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>%
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2016 <- dat

#load data 2017 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2017")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>%
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2017 <- dat

#load data 2018 CHACN####
dat <- read_excel("Data/Raw/Isleta_CHACN2003_2018.xlsx", sheet = "2018")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>%
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2018 <- dat

#Combine all CHACN files to one csv ####
CHACN <- rbind(dat2003, dat2004, dat2005, dat2006, dat2007, dat2008, dat2009, dat2010,
               dat2011, dat2012, dat2013, dat2014, dat2015, dat2016, dat2017, dat2018)

CHACN <- CHACN %>% 
  group_by(Date) %>% 
  summarise(Mean_daily_discharge_cfs = mean(Discharge_cfs))

#load data 2003 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2003")

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

#load data 2004 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2004")

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

#load data 2005 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2005")

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

#load data 2006 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2006")

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

#load data 2007 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2007")

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

#load data 2008 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2008")

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

#load data 2009 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2009")

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

#load data 2010 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2010")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2011 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2011")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2012 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2012")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2013 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2013")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2014 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2014")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2015 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2015")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
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

#load data 2016 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2016")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2016) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2016 <- dat

#load data 2017 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2017")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>%
  add_column(Year = 2017) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2017 <- dat

#load data 2018 CACCN####
dat <- read_excel("Data/Raw/Isleta_CACCN2003_2018.xlsx", sheet = "2018")

#rename column
colnames(dat)[1] <- "X1"

#text to columns 
dat <- cSplit(dat, "X1", sep = " ", type.convert = F)
class(dat)
dat <- as.data.frame(dat)

#remove rows that aren't needed
dat <- dat[-c(6:13)]
dat <- dat[-(1:7),]

#formatting 
dat <- dat %>% 
  rename(Month=X1_1, Day=X1_2, Time=X1_3, Height_ft=X1_4, Discharge_cfs=X1_5)%>% 
  add_column(Year = 2018) %>%                          ######NEED to CHANGE YEARS FOR EACH FILE
  mutate(Date = (paste(Year, Month, Day, sep="-"))) %>% 
  mutate(Date = as.POSIXct(Date, format="%Y-%b-%d", tz="MST")) %>% 
  select(Date:Year,everything()) %>% 
  mutate_at(c("Day","Height_ft","Discharge_cfs"), as.numeric)

dat2018 <- dat

#Combine all CACCN files to one csv ####
CACCN <- rbind(dat2003, dat2004, dat2005, dat2006, dat2007, dat2008, dat2009, dat2010,
               dat2011, dat2012, dat2013, dat2014, dat2015, dat2016, dat2017, dat2018)

CACCN <- CACCN %>% 
  group_by(Date) %>% 
  summarise(Mean_daily_discharge_cfs = mean(Discharge_cfs))

#Combine the five gages and get a total diversion ####
PERCN <- PERCN[-(3696),]

PERCN1 <- PERCN %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  rename(MeanPERCN = Mean_daily_discharge_cfs, Date1 = Date)

CHICN <- CHICN[-(3494),]
CHICN1 <- CHICN %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  filter(Date > "2003-06-25") %>% 
  rename(MeanCHICN = Mean_daily_discharge_cfs, Date2 = Date)

BELCN <- BELCN[-(3384),]
BELCN1 <- BELCN %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  filter(Date > "2003-06-25") %>% 
  rename(MeanBELCN = Mean_daily_discharge_cfs, Date3 = Date)

CHACN <- CHACN[-(5036),]
CHACN1 <- CHACN %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  filter(Date > "2003-06-25") %>% 
  rename(MeanCHACN = Mean_daily_discharge_cfs, Date4 = Date)

CACCN <- CACCN[-(4709),]
CACCN1 <- CACCN %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  filter(Date > "2003-06-25") %>% 
  rename(MeanCACCN = Mean_daily_discharge_cfs, Date5 = Date)

IsletaDiv <- cbind(PERCN1, CHICN1, BELCN1, CHACN1, CACCN1)

IsletaDiv <- IsletaDiv %>% 
  mutate(Tot_diversion_cfs = MeanPERCN + MeanCHICN + MeanBELCN + MeanCHACN + MeanCACCN ) %>% 
  select(Date1,Tot_diversion_cfs, MeanPERCN, MeanCHICN, MeanBELCN, MeanCHACN, MeanCACCN) %>% 
  rename(Date = Date1)
  

write.csv(IsletaDiv, "Data/Processed/IsletaDiv.csv", row.names = FALSE )

temp <- read.csv("Data/Processed/IsletaDiv.csv")














