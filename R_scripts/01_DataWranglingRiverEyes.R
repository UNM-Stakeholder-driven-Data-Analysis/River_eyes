#Read me ####
#The purpose of this scripts is to clean up the data files I'll be using and
#saving them to processed data folder

#Libraries####
library(tidyverse)
library(lubridate)

#Load data sets ####
  
dat <- read.csv(file.choose()) #River Eyes compilation 2002 -2018 from raw 
dat <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Raw/RiverEyes_compilation_2002_2018.csv", header = TRUE)
dat1 <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Raw/RiverEyes_compilation_1996_2019.csv", header = TRUE)

#Combine data sets and format dates####
   #subset 2019 from dat1 and format date
dat1 <- dat1 %>% 
  select(Date, URM, LRM, Distance, Year) %>% 
  filter(Year==2019)
 
dat1$Date <- as.Date.character(dat1$Date, "%m/%d/%y")

   #formate date dat
dat$Date <- as.Date.character(dat$Date, "%Y-%m-%d")

   #bind the two datasets
dat2 <- rbind(dat, dat1)

#Explore date/time and other formatting ####
class(dat2$Date)
head(dat2$Date)
tail(dat2$Date)
View(dat3)

#write processed data set####
write.csv(dat2,"~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Processed/RiverEyesDat2.csv", row.names = FALSE)

  #had to set working directory
dat3 <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Processed/RiverEyesDat2.csv", header = TRUE)
