#Read me ####
#The purpose of this script is to format the raw data into response variables
#Used River Miles 116-130 because this is Reach 6
#Only included 2003-2018 as these were the common years among raw datasets
#Saved processed data folder

#Libraries####
library(tidyverse)
library(lubridate)
library(janitor)

#Load data for Daily Extent Dry ####
  
dat <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Raw/RiverEyes_compilation_2002_2018.csv", header = TRUE)
dat1 <- read.csv("~/UNM/Stakeholders/ISC_RiverEyes_RioGrande/River_eyes/Data/Raw/RiverEyes_compilation_1996_2019.csv", header = TRUE)

#Combine compilation data sets and format data####
   #subset 2019 from dat1 and format date
dat1 <- dat1 %>% 
  select(Date, URM, LRM, Distance, Year) %>% 
  filter(Year==2019)
 
dat1$Date <- as.Date.character(dat1$Date, "%m/%d/%y")

   #formate date dat
dat$Date <- as.Date.character(dat$Date, "%Y-%m-%d")

   #bind the two datasets
dat2 <- rbind(dat, dat1)

dat3 <- dat2 %>% 
  group_by(Date) %>% 
  filter(Distance == max(Distance)) %>% 
  rename(DistanceDry = Distance) %>% 
  filter(Year > 2002 & Year < 2019) %>% 
  select(Date, DistanceDry, Year)

#Make dummy variables for study dates####
test1 <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")
test1 <- as.data.frame(test1)
test1 <- test1 %>% 
  rename(Date = test1) %>% 
  mutate(Year = lubridate::year(Date))

#Join to get extent of drying by day####
join_dat1 <- dat3 %>% 
  full_join(test1, by=c("Date", "Year")) %>% 
  replace_na(list(DistanceDry = 0)) %>% 
  distinct(Date, .keep_all = TRUE)

#No duplicate dates####
dat2013 <- join_dat1 %>% 
  filter(Year==2013) %>% 
  get_dupes(Date)

#write csv processed compilation data set####
write.csv(join_dat1,"Data/Processed/DailyExtentDry.csv", row.names = FALSE)







#Load data for Daily Dry River Miles data####
dat <- read.csv("Data/Raw/Rio.Grande.Dry.RM.CSV") #don't use read_csv as it messes the header/columns

#format data####
dat <- dat %>% 
  mutate(RmSeq = trunc(mile))%>% 
  filter(RmSeq %in% (116:130)) %>% 
  mutate(DateSeq = as.Date(Date)) %>% 
  rename(Mile = mile) %>% 
  filter(Year > 2002)


#Make dummy variables for Reach 6 river mile 130 to 116 and study dates####
test2 <- data.frame(rep(116:130, each=5844))
colnames(test2)[1] <- "RmSeq"
test2$"DateSeq" <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")

#Join to get presence absence of drying by river mile by day####
join_data <- dat %>%
  mutate(Condition = "Dry") %>%
  full_join(test2, by=c("DateSeq", "RmSeq")) %>%
  replace_na(list(Condition = "Wet")) %>%
  mutate(Condition.b = case_when(Condition == "Wet" ~ 0,
                           Condition == "Dry" ~ 1,)) %>% 
  select(RmSeq, DateSeq, Condition, Condition.b) %>% 
  select(DateSeq,everything()) %>% 
  distinct() %>% 
  mutate(Year = lubridate::year(DateSeq)) %>%
  mutate(Month = lubridate::month(DateSeq)) %>% 
  distinct(DateSeq,RmSeq, .keep_all = TRUE)

#Test for duplicate dates####
dat2013 <- get_dupes(join_data, DateSeq, RmSeq)

#Write csv individual RMs dry####
write.csv(join_data,"Data/Processed/DailyDryRM.csv", row.names = FALSE)












#Format data for Annual Dry by RM from Daily Dry River Miles file####
Annual_dry_rm <- join_data %>% 
  mutate(Yr = lubridate::year(DateSeq)) %>%
  group_by(Year, RmSeq) %>% 
  summarise(Sum_days_rm_dry = sum(Condition.b)) %>% 
  filter(Year > 2002)

#Write csv annual total times a river mile was dry####
write.csv(Annual_dry_rm,"Data/Processed/AnnualDryRM.csv", row.names = FALSE)








