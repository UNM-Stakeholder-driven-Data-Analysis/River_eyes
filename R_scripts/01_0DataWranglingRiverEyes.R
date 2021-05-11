#Read me ####
#The purpose of this script is to format the raw data into response variables
##Only included 2003-2018 as these were the common years among raw datasets
#Saved processed data folder
#1) data to assess expansion or extent of river drying each day
#2) data to assess occurrence of river drying each mile each day
#3) format data for annual persistence of drying



#Libraries####
library(tidyverse)
library(lubridate)
library(janitor)

#1) data to assess expansion or extent of river drying ####
  #load data for expansion or daily extent of river drying 
dat <- read.csv("Data/Raw/River_Eyes/RiverEyes_compilation_2002_2018.csv", header = TRUE)

   #format date
dat$Date <- as.Date.character(dat$Date, "%Y-%m-%d")

   #filter to 2002 and greater and create a new column associate with upper river mile (URM) and assign reaches
dat1 <- dat %>% 
  group_by(Date, LRM) %>% 
  filter(Distance == max(Distance)) %>% 
  rename(DistanceDry = Distance) %>% 
  filter(Year > 2002) %>% 
  mutate(RM = trunc(URM)) %>% 
  mutate(Reach = case_when(RM >=129 ~ 5,
                           RM <=128 & RM >=116 ~ 6,
                           RM <=115 & RM >=68 ~ 7,
                           RM <=67 ~ 8))

  #Make date sequence to make sure all days in the time period are represented
seq1 <- data.frame(rep(5:8, each=5844))
colnames(seq1)[1] <- "Reach"
seq1$"Date" <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")
seq1 <- seq1 %>% 
  mutate(Year = lubridate::year(Date))

 #Use "Join" to get extent of drying by day
join_dat1 <- dat1 %>% 
  full_join(seq1, by=c("Date", "Year", "Reach")) %>% 
  replace_na(list(DistanceDry = 0))%>% 
  distinct(Date, Reach, .keep_all = TRUE) %>% 
  select(Date, DistanceDry, Year, Reach)

 #Test for duplicate dates - none 
dat2013 <- join_dat1 %>% 
  filter(Year==2013) %>% 
  get_dupes(Date, Reach)

 #write csv processed compilation data set
write.csv(join_dat1,"Data/Processed/DailyExpansionDry.csv", row.names = FALSE)


#2) data to assess occurrence of river drying each mile each day #####

 #Load data for Daily Dry River Miles data
dat <- read.csv("Data/Raw/River_Eyes/Rio.Grande.Dry.RM.CSV") #don't use read_csv as it messes the header/columns

 #Format data to full mile, date, time period of study, and river miles below isleta diversion dam
dat <- dat %>% 
  mutate(RMSeq = trunc(mile))%>% 
  mutate(DateSeq = as.Date(Date)) %>% 
  rename(RM_tenth = mile) %>% 
  filter(Year > 2002) %>% 
  filter(RMSeq < 168)


  #Make sequences each river mile 130 to 116 for each study date  
seq2 <- data.frame(rep(54:167, each=5844))
colnames(seq2)[1] <- "RMSeq"
seq2$"DateSeq" <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")

 #Join to get occurrence or drying by river mile by day
join_data <- dat %>%
  mutate(Condition = "Dry") %>%
  full_join(seq2, by=c("DateSeq", "RMSeq")) %>%
  replace_na(list(Condition = "Wet")) %>%
  mutate(Condition.b = case_when(Condition == "Wet" ~ 0,
                           Condition == "Dry" ~ 1,)) %>% 
  select(RMSeq, DateSeq, Condition, Condition.b, RM_tenth) %>% 
  select(DateSeq,everything()) %>% 
  distinct() %>% 
  mutate(Year = lubridate::year(DateSeq)) %>%
  mutate(Month = lubridate::month(DateSeq)) %>% 
  distinct(DateSeq,RMSeq, .keep_all = TRUE) %>% 
  rename(Date = DateSeq) %>% 
  rename(RM = RMSeq) %>% 
  mutate(Reach = case_when(RM >=129 ~ 5,
                           RM <=130 & RM >=116 ~ 6,
                           RM <=115 & RM >=68 ~ 7,
                           RM <=67 ~ 8))
  

  #Test for duplicate dates - none 
check_duplications <- get_dupes(join_data, Date, RM)

  #Write csv 
write.csv(join_data,"Data/Processed/DailyOccurrenceDryRM.csv", row.names = FALSE)


#3) format data for annual persistence of drying ####
 
 #formal data for year and sum all the days the river mile was dry throughout the year 
 #using occurrence data 
Annual_dry_rm <- join_data %>% 
  mutate(Yr = lubridate::year(Date)) %>%
  group_by(Year, RM) %>% 
  summarise(Sum_days_rm_dry = sum(Condition.b))

  #testing to make sure "sum" worked
join_data %>% 
  filter(Year==2018) %>% 
  filter(RM==77) %>% 
  filter(Condition.b==1) %>% 
  arrange(Date)

 #Write csv 
write.csv(Annual_dry_rm,"Data/Processed/PersistenceDryRM.csv", row.names = FALSE)