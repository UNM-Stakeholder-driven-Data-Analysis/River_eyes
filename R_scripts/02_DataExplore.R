#Read me####
#The purpose of this script is to explore the River Eyes data set

#Libraries####

library(tidyverse)
library(car)
library(lubridate)

#Load data Daily Extent Dry ####
#use read_csv for date format to be automatic but this makes it a tibble
dat <- read_csv("Data/Processed/DailyExtentDry.csv")

  ###I AM LOSING SOME DATA IN MY WRANGLINGE BUT CAN'T FIGURE OUT WHY...
  ###THERE SHOULD BE 5 INSTANCES OF DRYING IN REACH 6 RATHER THAN 3
dat %>% 
  filter(Reach == 6) %>% 
  filter(DistanceDry > 0) %>% 
  View()

# add day of year for plotting
dat$Day <-  lubridate::yday(dat$Date)

#Summary explore Daily Extent Dry
summary(dat)
summary(dat$Year:dat$DistanceDry)
dat %>% 
  group_by(Year) %>% 
  summarise(DistMn = mean(DistanceDry), n = length(DistanceDry)) %>% 
  view()

#Plot data Daily Extent Dry
plot(dat$DistanceDry~dat$Date)
hist(dat$DistanceDry, main="Histogram", 
     xlab="Distance of river drying events (total river miles)") #zero inflated
hist(log(dat$DistanceDry+1), main = "Histogram log(DistanceDry)",
     xlab="Distance of river drying events (total river miles)")
boxplot(dat$DistanceDry~dat$Year, main="Boxplot", xlab = "Year",
        ylab="Distance of river drying events (total river miles)")

ggplot(dat, aes(DistanceDry))+
  geom_histogram()+
  facet_wrap(~Year, scale="free") +
  ggtitle("Histograms by Year")+
  xlab("Distance of river drying events (total river miles)")

dat %>% 
  filter(DistanceDry > 0) %>% 
  ggplot(aes(DistanceDry))+
  geom_histogram()+
  ggtitle("Histograms by Year Zeros Removed")+
  xlab("Distance of river drying events (total river miles)")

dat %>% 
  filter(DistanceDry > 0) %>% 
  ggplot(aes(DistanceDry))+
  geom_histogram()+
  facet_wrap(~Year, scale="free")+
  ggtitle("Histograms by Year Zeros Removed")+
  xlab("Distance of river drying events (total river miles)")

ggplot(data=dat, aes(x=Day, y=DistanceDry))+
  geom_point()+
  facet_wrap(~Year, scales="free_y")+
  theme_bw()

ggplot(data=dat, aes(x=Day, y=DistanceDry))+
  geom_point()+
  facet_wrap(~Reach, scales="free_y")+
  theme_bw()

ggplot(data=dat, aes(x=Date, y=DistanceDry))+
  geom_point()+
  facet_wrap(~Reach, scales="free_y")+
  theme_bw()

#Determine distribution
dat_r_R5 <- dat %>% 
  filter(Reach == 5) %>% 
  sample_n(5000) 
qqPlot(dat_r_R5$DistanceDry); shapiro.test(dat_r_R5$DistanceDry) #NOT normal zero inflated
qqPlot(log10(dat_r_R5$DistanceDry+1)); shapiro.test(log10(dat_r_R5$DistanceDry+1)) #Log transform NOT normal zero inflated

dat_r_R6 <- dat %>% 
  filter(Reach == 5) %>% 
  sample_n(5000) 
qqPlot(dat_r_R6$DistanceDry); shapiro.test(dat_r_R6$DistanceDry) #NOT normal zero inflated
qqPlot(log10(dat_r_R6$DistanceDry+1)); shapiro.test(log10(dat_r_R6$DistanceDry+1)) #Log transform NOT normal zero inflated

dat_r_R7 <- dat %>% 
  filter(Reach == 5) %>% 
  sample_n(5000) 
qqPlot(dat_r_R7$DistanceDry); shapiro.test(dat_r_R7$DistanceDry) #NOT normal zero inflated
qqPlot(log10(dat_r_R7$DistanceDry+1)); shapiro.test(log10(dat_r_R7$DistanceDry+1)) #Log transform NOT normal zero inflated

dat_r_R8 <- dat %>% 
  filter(Reach == 5) %>% 
  sample_n(5000) 
qqPlot(dat_r_R8$DistanceDry); shapiro.test(dat_r_R8$DistanceDry) #NOT normal zero inflated
qqPlot(log10(dat_r_R8$DistanceDry+1)); shapiro.test(log10(dat_r_R8$DistanceDry+1)) #Log transform NOT normal zero inflated


#Load data Annual Dry river mile ####
dat1 <- read_csv("Data/Processed/AnnualDryRM.csv")

#Summary explore Annual Dry river mile
summary(dat1)
with(dat1, table(Sum_days_rm_dry, RM))

dat1 %>% 
  group_by(Year) %>% 
  summarise(DistMn = mean(Sum_days_rm_dry)) %>% 
  view()

#Plot data Annual Dry river mile
plot(dat1$Sum_days_rm_dry~dat1$RM)
hist(dat1$Sum_days_rm_dry)

# plot
ggplot(data=dat1, aes(x=RM, y=Sum_days_rm_dry))+
  geom_point() + geom_path()+
  facet_wrap(~Year, scales="free_y")

#Load data Daily Dry RM####
dat2 <- read_csv("Data/Processed/DailyDryRM.csv")








