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
  
dat <- read.csv("Data/Raw/River_Eyes/RiverEyes_compilation_2002_2018.csv", header = TRUE)

   #formate date dat
dat$Date <- as.Date.character(dat$Date, "%Y-%m-%d")

dat1 <- dat %>% 
  group_by(Date) %>% 
  filter(Distance == max(Distance)) %>% 
  rename(DistanceDry = Distance) %>% 
  filter(Year > 2002) %>% 
  mutate(RM = trunc(URM)) %>% 
  mutate(Reach = case_when(RM >=129 ~ 5,
                           RM <=130 & RM >=116 ~ 6,
                           RM <=115 & RM >=68 ~ 7,
                           RM <=67 ~ 8
  ))

dat2 <- dat %>% 
  group_by(Date) %>% 
  filter(Distance == max(Distance)) %>% 
  rename(DistanceDry = Distance) %>% 
  filter(Year > 2002) %>% 
  mutate(RM = URM) %>% 
  mutate(Reach = case_when(RM >=129 ~ 5,
                           RM <=130 & RM >=116 ~ 6,
                           RM <=115 & RM >=68 ~ 7,
                           RM <=67 ~ 8
  ))


#Make dummy variables for study dates####
test1 <- data.frame(rep(5:8, each=5844))
colnames(test1)[1] <- "Reach"
test1$"Date" <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")
test1 <- test1 %>% 
  mutate(Year = lubridate::year(Date))

#Join to get extent of drying by day####
join_dat1 <- dat1 %>% 
  full_join(test1, by=c("Date", "Year", "Reach")) %>% 
  replace_na(list(DistanceDry = 0))%>% 
  distinct(Date, Reach, .keep_all = TRUE) %>% 
  select(Date, DistanceDry, Year, Reach)

#Look for duplicate dates####
dat2013 <- join_dat1 %>% 
  filter(Year==2013) %>% 
  get_dupes(Date, Reach)

#write csv processed compilation data set####
write.csv(join_dat1,"Data/Processed/DailyExtentDry.csv", row.names = FALSE)







#Load data for Daily Dry River Miles data####
dat <- read.csv("Data/Raw/Rio.Grande.Dry.RM.CSV") #don't use read_csv as it messes the header/columns

#format data####
dat <- dat %>% 
  mutate(RMSeq = trunc(mile))%>% 
  mutate(DateSeq = as.Date(Date)) %>% 
  rename(RM_tenth = mile) %>% 
  filter(Year > 2002) %>% 
  filter(RMSeq < 168)


#Make dummy variables for Reach 6 river mile 130 to 116 and study dates####
test2 <- data.frame(rep(54:167, each=5844))
colnames(test2)[1] <- "RMSeq"
test2$"DateSeq" <- seq(as.Date("2003-01-01") , as.Date("2018-12-31"), "day")

#Join to get presence absence of drying by river mile by day####
join_data <- dat %>%
  mutate(Condition = "Dry") %>%
  full_join(test2, by=c("DateSeq", "RMSeq")) %>%
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
                           RM <=67 ~ 8
  ))
  

#Test for duplicate dates####
check_duplications <- get_dupes(join_data, Date, RM)

#Write csv individual RMs dry####
write.csv(join_data,"Data/Processed/DailyDryRM.csv", row.names = FALSE)












#Format data for Annual Dry by RM from Daily Dry River Miles file####
Annual_dry_rm <- join_data %>% 
  mutate(Yr = lubridate::year(Date)) %>%
  group_by(Year, RM) %>% 
  summarise(Sum_days_rm_dry = sum(Condition.b))

  #testing to make sure "sum" worked....seems like a lot of days dry
join_data %>% 
  filter(Year==2018) %>% 
  filter(RM==77) %>% 
  filter(Condition.b==1) %>% 
  arrange(Date)

#Write csv annual total times a river mile was dry####
write.csv(Annual_dry_rm,"Data/Processed/AnnualDryRM.csv", row.names = FALSE)








