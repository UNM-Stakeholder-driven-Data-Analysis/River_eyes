#Read me ####
#The purpose of this scripts is to load the ET toolbox data from Reach 6
#https://www.usbr.gov/uc/albuq/water/ETtoolbox/rg/newreaches/
#had to do a select all and copy in to an excel
#and then clean the data so it can be used

#load libraries####
library(tidyverse)
library(splitstackshape)
library(stringi)

#load data####
dat <- read_csv("Data/Raw/ET_toolbox_R6_2019.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2018.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2017.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2016.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2015.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2014.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2013.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2012.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2011.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2010.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2009.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2008.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2007.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2006.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2005.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2004.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2003.csv", skip = 18)
dat <- read_csv("Data/Raw/ET_toolbox_R6_2002.csv", skip = 18)

#format data into a readable data frame####
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
dat <- dat[-c(13)]

             #2011, 2009, 2005, 2004, 2003, 2002 data
            dat <- dat[-c(13:14)]
             #2010, 2008, 2007, 2006 data
            dat <- dat[-c(13)]

#formatting for column names, dates, variable classes####
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

dat$Month.n <- as.numeric(match(dat$Month,month.abb))

dat <- dat %>% 
  select(Date:Month, Month.n, Day:Use_to_date_since_Jan1)

#to combine and write files
dat2019 <- dat #346 obs
dat2018 <- dat #345 obs
dat2017 <- dat #365 obs
dat2016 <- dat #365 obs
dat2015 <- dat #365 obs
dat2014 <- dat #365 obs
dat2013 <- dat #174 obs
dat2012 <- dat #366 obs
dat2011 <- dat #363 obs
dat2010 <- dat #363 obs
dat2009 <- dat #363 obs
dat2008 <- dat #364 obs
dat2007 <- dat #363 obs
dat2006 <- dat #363 obs
dat2005 <- dat #333 obs
dat2004 <- dat #359 obs
dat2003 <- dat #350 obs
dat2002 <- dat #360 obs

Et_data <- rbind(dat2019,dat2018, dat2017, dat2016, dat2015, dat2014,
                 dat2013, dat2012, dat2011, dat2010, dat2009, dat2008,
                 dat2007, dat2006, dat2005, dat2004, dat2003, dat2002)
write.csv(Et_data,"Data/Processed/ET_data.csv", row.names = FALSE)
 








              