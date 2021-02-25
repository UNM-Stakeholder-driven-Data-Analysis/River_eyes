#Read me ####
#The purpose of this scripts is to clean up the data files I'll be using and
#saving them to processed data folder

#Libraries####
library(tidyverse)
library(lubridate)

#Load compilation data sets ####
  
dat <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Raw/RiverEyes_compilation_2002_2018.csv", header = TRUE)
dat1 <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Raw/RiverEyes_compilation_1996_2019.csv", header = TRUE)

#Combine compilation data sets and format dates####
   #subset 2019 from dat1 and format date
dat1 <- dat1 %>% 
  select(Date, URM, LRM, Distance, Year) %>% 
  filter(Year==2019)
 
dat1$Date <- as.Date.character(dat1$Date, "%m/%d/%y")

   #formate date dat
dat$Date <- as.Date.character(dat$Date, "%Y-%m-%d")

   #bind the two datasets
dat2 <- rbind(dat, dat1)

#Explore date/time and other formatting compilation data ####
class(dat2$Date)
head(dat2$Date)
tail(dat2$Date)
View(dat3)

#

#write processed compilation data set####
write.csv(dat2,"~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Processed/RiverEyesCompilation.csv", row.names = FALSE)




#Load summary dry river miles data####
dat <- read.csv("Data/Raw/Rio.Grande.Dry.RM.CSV") #don't use read_csv as it messes the header/columns

#format date####
dat <- dat %>% 
  mutate(RmSeq = trunc(mile))%>% 
  filter(RmSeq %in% (116:130)) %>% 
  mutate(DateSeq = as.Date(Date)) %>% 
  rename(Mile = mile) %>% 
  mutate(Month = lubridate::month(DateSeq)) %>% 
  mutate(Day = lubridate::day(DateSeq)) 

#Make dummny variables for Reach 6 river mile 130 to 116 and study dates
test2 <- data.frame(rep(116:130, each=5845))
colnames(test2)[1] <- "RmSeq"
test2$"DateSeq" <- seq(as.Date("2002-10-14") , as.Date("2018-10-14"), "day")

#Join to get presence absence of drying by river mile by day
LJoinRm <- test2 %>% 
  left_join(y=dat, by=c("DateSeq", "RmSeq"))

FJoinRM <- test2 %>% 
  full_join(y=dat, by=c("DateSeq", "RmSeq"))
  
  ###

RJoinRm2 <- dat %>% 
  right_join(y=test2, by=c("DateSeq", "RmSeq"))

FJoinRM2 <- dat %>% 
  full_join(y=test2, by=c("DateSeq", "RmSeq"))
  
  ###
Join <- dat %>% 
  left_join(test2)
Join <- test2 %>% 
  left_join(dat)



