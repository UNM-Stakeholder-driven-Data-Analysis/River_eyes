#Read me ####
#The purpose of this script is to explore the River Eyes data set

#Libraries ####

library(tidyverse)
library(car) #qq plot fxn
library(lubridate) #dates
library(psych) #correlation
library(janitor) #duplication

library(tsibble) # useful for creating time series objects
library(forecast) # for autotemporal Acf fxn

library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting

#Response Var - Daily Extent Dry ####
#use read_csv for date format to be automatic but this makes it a tibble
dat <- read_csv("Data/Processed/DailyExtentDry.csv")

# add day of year for plotting
dat$Day <-  lubridate::yday(dat$Date)
dat <- dat %>% 
  select(Date, DistanceDry,Year, Reach, Day) %>% 
  mutate(Reach = as.factor(as.character(Reach)))
str(dat)

#Summary explore Daily Extent Dry
summary(dat)
summary(dat$Year:dat$DistanceDry)
dat %>% 
  group_by(Year, Reach) %>% 
  summarise(DistMn = mean(DistanceDry), n = length(DistanceDry)) %>% 
  view()

#Plot data Daily Extent Dry
plot(dat$DistanceDry~dat$Date)

hist(dat$DistanceDry, main="Histogram", 
     xlab="Dry river (total miles)") #zero inflated

hist(log(dat$DistanceDry+1), main = "Histogram log(DistanceDry)",
     xlab="Distance of river drying events (total river miles)")

boxplot(dat$DistanceDry~dat$Year, main="Boxplot", xlab = "Year",
        ylab="Distance of river drying events (total river miles)")

dat <- dat %>% 
  mutate(DistanceDryL=log(DistanceDry+1))  

ggplot(dat, aes(DistanceDryL))+
  geom_histogram()+
  facet_grid(~Reach, scale="free") +
  xlab("Log distance river dry (total miles)")

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
  sample_n(5000) %>% 
  group_by(Day)
qqPlot(dat_r_R5$DistanceDry); shapiro.test(dat_r_R5$DistanceDry) 
qqPlot(log10(dat_r_R5$DistanceDry+1)); shapiro.test(log10(dat_r_R5$DistanceDry+1)) 
qqPlot((dat_r_R5$DistanceDry)^2); shapiro.test((dat_r_R5$DistanceDry)^2)
qqPlot(sqrt(dat_r_R5$DistanceDry)); shapiro.test((dat_r_R5$DistanceDry))


dat_r_R6 <- dat %>% 
  filter(Reach == 6) %>% 
  sample_n(5000) 
qqPlot(dat_r_R6$DistanceDry); shapiro.test(dat_r_R6$DistanceDry) 
qqPlot(log10(dat_r_R6$DistanceDry+1)); shapiro.test(log10(dat_r_R6$DistanceDry+1))
qqPlot((dat_r_R6$DistanceDry)^2); shapiro.test((dat_r_R6$DistanceDry)^2)
qqPlot(sqrt(dat_r_R6$DistanceDry)); shapiro.test((dat_r_R6$DistanceDry))

dat_r_R7 <- dat %>% 
  filter(Reach == 7) %>% 
  sample_n(5000) 
qqPlot(dat_r_R7$DistanceDry); shapiro.test(dat_r_R7$DistanceDry) 
qqPlot(log10(dat_r_R7$DistanceDry+1)); shapiro.test(log10(dat_r_R7$DistanceDry+1)) 
qqPlot((dat_r_R7$DistanceDry)^2); shapiro.test((dat_r_R7$DistanceDry)^2)
qqPlot(sqrt(dat_r_R7$DistanceDry)); shapiro.test((dat_r_R7$DistanceDry))

dat_r_R8 <- dat %>% 
  filter(Reach == 8) %>% 
  sample_n(5000) 
qqPlot(dat_r_R8$DistanceDry); shapiro.test(dat_r_R8$DistanceDry) 
qqPlot(log10(dat_r_R8$DistanceDry+1)); shapiro.test(log10(dat_r_R8$DistanceDry+1)) 
qqPlot((dat_r_R8$DistanceDry)^2); shapiro.test((dat_r_R8$DistanceDry)^2)
qqPlot(sqrt(dat_r_R8$DistanceDry)); shapiro.test((dat_r_R8$DistanceDry))


#Response Var - Daily Extent Dry Temporal Auotcorrelation Reaches 5 & 7 ####
#Reach 5 
dat10 <- read_csv("Data/Processed/DailyExtentDry.csv")

#organizing data to get a single date and distance dry
#date duplication even when by reach
dat11 <- dat10 %>% 
  filter(Reach==5)  

#decided to sum by day and create a time series 
dat11 <- dat10 %>% 
  group_by(Date) %>% 
  summarise(Sum_dist_dry= sum(DistanceDry)) %>% 
  arrange(Date) %>% 
  as_tsibble(index = Date)

class(dat11)
head(dat11)
minday <- as.Date("2003-01-01")

dat11_ts = ts(dat11$Sum_dist_dry, frequency = 365, start = c(year(minday), 
                                                             as.numeric(format(minday, "%j"))))
print(dat11_ts, calendar = T) #not sure if this is correct.....:( 
plot(dat11_ts)

### check for temporal autocorrelation in the ts
# using forecast pkg's Acf 
# Note the different options for dealing with NAs and how this changes the results 
# (see ?na.fail and ?Acf for details). 
forecast::Acf(dat11_ts, lag.max = 365, na.action = na.contiguous) 
forecast::Pacf(dat11_ts, lag.max = 365, na.action = na.contiguous)

#See what happens when you group by month
dat11 <- dat10 %>% 
  filter(Reach==5) 

dat12_ts <- dat11 %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  group_by(Reach, Year, Month) %>%
  summarise(Sum_dist_dry = sum(DistanceDry)) %>%
  mutate(Date2 = paste(Year, Month, "01", sep="-")) %>%
  mutate(Date2 = as.Date(Date2)) %>% 
  arrange(Date2) %>% 
  as_tsibble()

class(dat12_ts)
head(dat12_ts)
minday <- as.Date("2003-01-01")

dat12_ts = ts(dat12_ts$Sum_dist_dry, frequency = 12, start = c(2003,01))
print(dat12_ts, calendar = T) #not sure if this is correct.....:( 
plot(dat12_ts)

forecast::Acf(dat12_ts, lag.max = 12, na.action = na.contiguous) 
forecast::Pacf(dat12_ts, lag.max = 12, na.action = na.contiguous)

#Reach 7 
dat10 <- read_csv("Data/Processed/DailyExtentDry.csv")

#organizing data to get a single date and distance dry
#date duplication even when by reach
dat13 <- dat10 %>% 
  filter(Reach==7)  

#decided to sum by day and create a time series 
dat13 <- dat10 %>% 
  group_by(Date) %>% 
  summarise(Sum_dist_dry= sum(DistanceDry)) %>% 
  arrange(Date) %>% 
  as_tsibble(index = Date)

class(dat13)
head(dat13)
minday <- as.Date("2003-01-01")

dat13_ts = ts(dat13$Sum_dist_dry, frequency = 365, start = c(year(minday), 
                                                             as.numeric(format(minday, "%j"))))
print(dat13_ts, calendar = T) #not sure if this is correct.....:( 
plot(dat13_ts)

### check for temporal autocorrelation in the ts
# using forecast pkg's Acf 
# Note the different options for dealing with NAs and how this changes the results 
# (see ?na.fail and ?Acf for details). 
forecast::Acf(dat13_ts, lag.max = 365, na.action = na.contiguous) 
forecast::Pacf(dat13_ts, lag.max = 365, na.action = na.contiguous)

#See what happens when you group by month
dat14 <- dat10 %>% 
  filter(Reach==7) 

dat15_ts <- dat14 %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  group_by(Reach, Year, Month) %>%
  summarise(Sum_dist_dry = sum(DistanceDry)) %>%
  mutate(Date2 = paste(Year, Month, "01", sep="-")) %>%
  mutate(Date2 = as.Date(Date2)) %>% 
  arrange(Date2) %>% 
  as_tsibble()

class(dat15_ts)
head(dat15_ts)

dat15_ts = ts(dat15_ts$Sum_dist_dry, frequency = 15, start = c(2003,01))
print(dat15_ts, calendar = T) #not sure if this is correct.....:( 
plot(dat15_ts)

forecast::Acf(dat15_ts, lag.max = 12, na.action = na.contiguous) 
forecast::Pacf(dat15_ts, lag.max = 12, na.action = na.contiguous)

#Response Var - Annual Dry River Mile ####
dat1 <- read_csv("Data/Processed/AnnualDryRM.csv")

#Summary explore Annual Dry river mile
summary(dat1)
with(dat1, table(Sum_days_rm_dry, RM))

hist(dat1$Sum_days_rm_dry, xlab = "Number of days a given river mile was dry", main = (""))

dat1 %>% 
  group_by(Year) %>% 
  summarise(DistMn = mean(Sum_days_rm_dry)) %>% 
  view()

#Plot data Annual Dry river mile
plot(dat1$Sum_days_rm_dry~dat1$RM, xlab="River Mile", ylab="Number of days dry")


# plot
ggplot(data=dat1, aes(x=RM, y=Sum_days_rm_dry))+
  geom_point()+
  ylab("Number of days dry")+
  xlab("River Mile")

#normality
qqPlot(dat1$Sum_days_rm_dry); shapiro.test(dat1$Sum_days_rm_dry)
qqPlot(log(dat1$Sum_days_rm_dry+1)); shapiro.test(log(dat1$Sum_days_rm_dry+1))
qqPlot((dat1$Sum_days_rm_dry)^2); shapiro.test((dat1$Sum_days_rm_dry)^2)
qqPlot(sqrt(dat1$Sum_days_rm_dry)); shapiro.test(sqrt(dat1$Sum_days_rm_dry))

#Response Var - Annual Dry River Mile Temporal Auotcorrelation for RM 74####
dat16 <- read_csv("Data/Processed/AnnualDryRM.csv")

#looking at a river miles that had extensive drying each year 
dat17 <- dat16 %>% 
  filter(RM == 74)

dat18_ts <- dat17 %>% 
  mutate(Date3 = paste(Year, "01", "01", sep = "-")) %>% 
  mutate(Date3 = as.Date((Date3))) %>% 
  arrange(Date3) %>% 
  as_tsibble(index = Date3)

class(dat18_ts)
head(dat18_ts)

dat18_ts = ts(dat18_ts$Sum_days_rm_dry, frequency = 1, start = c(2003, 01))

print(dat18_ts, calendar = T) #not sure if this is correct.....:( 
plot(dat18_ts)

### check for temporal autocorrelation in the ts
# using forecast pkg's Acf 
# Note the different options for dealing with NAs and how this changes the results 
# (see ?na.fail and ?Acf for details). 
forecast::Acf(dat18_ts, lag.max = 365, na.action = na.contiguous) 
forecast::Pacf(dat18_ts, lag.max = 365, na.action = na.contiguous)

#Response Var - Daily Dry RM ####
dat2 <- read_csv("Data/Processed/DailyDryRM.csv")

#summaries

summary(dat2)
temp <- as.data.frame(with(dat2, table(RM, Condition.b)))

ggplot(data = temp, aes(x=RM, y=Freq, color=Condition.b))+
  geom_point()+
  scale_color_discrete(name="River Condition", labels=(c("Wet", "Dry")))+
  scale_x_discrete(breaks = seq(50,170,10))+
  labs(x="River mile")

dat2 %>% 
  filter(Reach==5) %>% 
  ggplot(., aes(x=Date, y=Condition.b)) +
  geom_point()+
  facet_wrap(~RM)+
  ggtitle("River miles in Reach 5")+
  ylab("River dry (1) or wet (0)")+
  scale_y_continuous(breaks = seq(0,1))


dat2 %>% 
  filter(Reach==6) %>% 
  ggplot(., aes(x=Date, y=Condition.b)) +
  geom_point()+
  facet_wrap(~RM)+
  ggtitle("River miles in Reach 6")+
  ylab("River dry (1) or wet (0)")+
  scale_y_continuous(breaks = seq(0,1))

dat2 %>% 
  filter(Reach==7) %>% 
  ggplot(., aes(x=Date, y=Condition.b)) +
  geom_point()+
  facet_wrap(~RM)+
  ggtitle("River miles in Reach 7")+
  ylab("River dry (1) or wet (0)")+
  scale_y_continuous(breaks = seq(0,1))

dat2 %>% 
  filter(Reach==8) %>% 
  ggplot(., aes(x=Date, y=Condition.b)) +
  geom_point()+
  facet_wrap(~RM)+
  ggtitle("River miles in Reach 8")+
  ylab("River dry (1) or wet (0)")+
  scale_y_continuous(breaks = seq(0,1))

#Response Var - Daily Extent Dry Temporal Auotcorrelation for RM 74####
dat19 <- read_csv("Data/Processed/DailyDryRM.csv")

#looking at a river miles that had extensive drying each year 
dat20 <- dat19 %>% 
  filter(RM == 74)

dat20_ts <- dat20 %>% 
  arrange(Date) %>% 
  as_tsibble(index = Date)

class(dat20_ts)
head(dat20_ts)
minday <- as.Date("2003-01-01")

dat20_ts = ts(dat20_ts$Condition.b, frequency = 365, start = c(year(minday), 
                                                               as.numeric(format(minday, "%j"))))

print(dat20_ts, calendar = T) #not sure if this is correct.....:( 
plot(dat20_ts)

forecast::Acf(dat20_ts, lag.max = 365, na.action = na.contiguous) 
forecast::Pacf(dat20_ts, lag.max = 365, na.action = na.contiguous)

#Predictor Var - ET Toolbox Reach 6-8####
dat3 <- read_csv("Data/Processed/ET_Toolbox_R_6_8.csv")
dat3$Reach <- as.factor(dat3$Reach)

with(dat3, table(Reach, Ag_DCU_cfs))

ggplot(dat3, aes(x=Date, y=Ag_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Agriculture")+
  ylab("Depletion (cfs)")

ggplot(dat3, aes(x=Date, y=Riparian_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Riparian")+
  ylab("Depletion (cfs)")

ggplot(dat3, aes(x=Date, y=OpenWater_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande - Open water")+
  ylab("Depletion (cfs)")

ggplot(dat3, aes(x=Date, y=Urban_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Urban")+
  ylab("Depletion (cfs)")

ggplot(dat3, aes(x=Date, y=Rain_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Rainfall")+
  ylab("Addition (cfs)")

ggplot(dat3, aes(x=Date, y=five_day_avg_URGWOM_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - 5 day avg Upper Rio Grande Water Operations Model ")+
  ylab("depletions (cfs)")

#Filter to Reaches and 5000 sample for normality testing
dat3_r_R5 <- dat3 %>% 
  filter(Reach == 5) %>% 
  sample_n(5000) 

dat3_r_R8 <- dat3 %>% 
  filter(Reach == 6) %>% 
  sample_n(5000) 

dat3_r_R8 <- dat3 %>% 
  filter(Reach == 8) %>% 
  sample_n(5000) 

dat3_r_R7 <- dat3 %>% 
  filter(Reach == 7) %>% 
  sample_n(5000) 

#Determine distribution Ag depletion
qqPlot(dat3_r_R5$Ag_DCU_cfs); shapiro.test(dat3_r_R5$Ag_DCU_cfs) 
qqPlot(log10(dat3_r_R5$Ag_DCU_cfs+1)); shapiro.test(log10(dat3_r_R5$Ag_DCU_cfs+1)) 
qqPlot((dat3_r_R5$Ag_DCU_cfs)^2); shapiro.test((dat3_r_R5$Ag_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R5$Ag_DCU_cfs)); shapiro.test(sqrt(dat3_r_R5$Ag_DCU_cfs))

qqPlot(dat3_r_R8$Ag_DCU_cfs); shapiro.test(dat3_r_R8$Ag_DCU_cfs) 
qqPlot(log10(dat3_r_R8$Ag_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$Ag_DCU_cfs+1)) 
qqPlot((dat3_r_R8$Ag_DCU_cfs)^2); shapiro.test((dat3_r_R8$Ag_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Ag_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$Ag_DCU_cfs))

qqPlot(dat3_r_R8$Ag_DCU_cfs); shapiro.test(dat3_r_R8$Ag_DCU_cfs) 
qqPlot(log10(dat3_r_R8$Ag_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$Ag_DCU_cfs+1)) 
qqPlot((dat3_r_R7$Ag_DCU_cfs)^2); shapiro.test((dat3_r_R7$Ag_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R7$Ag_DCU_cfs)); shapiro.test(sqrt(dat3_r_R7$Ag_DCU_cfs))

qqPlot(dat3_r_R7$Ag_DCU_cfs); shapiro.test(dat3_r_R7$Ag_DCU_cfs) 
qqPlot(log10(dat3_r_R7$Ag_DCU_cfs+1)); shapiro.test(log10(dat3_r_R7$Ag_DCU_cfs+1)) 
qqPlot((dat3_r_R8$Ag_DCU_cfs)^2); shapiro.test((dat3_r_R8$Ag_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Ag_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$Ag_DCU_cfs))

#Determine distribution Riparian depletion
qqPlot(dat3_r_R5$Riparian_DCU_cfs); shapiro.test(dat3_r_R5$Riparian_DCU_cfs) 
qqPlot(log10(dat3_r_R5$Riparian_DCU_cfs+1)); shapiro.test(log10(dat3_r_R5$Riparian_DCU_cfs+1)) 
qqPlot((dat3_r_R5$Riparian_DCU_cfs)^2); shapiro.test((dat3_r_R5$Riparian_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R5$Riparian_DCU_cfs)); shapiro.test(sqrt(dat3_r_R5$Riparian_DCU_cfs))

qqPlot(dat3_r_R8$Riparian_DCU_cfs); shapiro.test(dat3_r_R8$Riparian_DCU_cfs) 
qqPlot(log10(dat3_r_R8$Riparian_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$Riparian_DCU_cfs+1)) 
qqPlot((dat3_r_R8$Riparian_DCU_cfs)^2); shapiro.test((dat3_r_R8$Riparian_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Riparian_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$Riparian_DCU_cfs))

qqPlot(dat3_r_R7$Riparian_DCU_cfs); shapiro.test(dat3_r_R7$Riparian_DCU_cfs) 
qqPlot(log10(dat3_r_R7$Riparian_DCU_cfs+1)); shapiro.test(log10(dat3_r_R7$Riparian_DCU_cfs+1)) 
qqPlot((dat3_r_R7$Riparian_DCU_cfs)^2); shapiro.test((dat3_r_R7$Riparian_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R7$Riparian_DCU_cfs)); shapiro.test(sqrt(dat3_r_R7$Riparian_DCU_cfs))

qqPlot(dat3_r_R8$Riparian_DCU_cfs); shapiro.test(dat3_r_R8$Riparian_DCU_cfs) 
qqPlot(log10(dat3_r_R8$Riparian_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$Riparian_DCU_cfs+1)) 
qqPlot((dat3_r_R8$Riparian_DCU_cfs)^2); shapiro.test((dat3_r_R8$Riparian_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Riparian_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$Riparian_DCU_cfs))

#Determine distribution OpenWater depletion - Wasn't so bad some reaches close to normal
qqPlot(dat3_r_R5$OpenWater_DCU_cfs); shapiro.test(dat3_r_R5$OpenWater_DCU_cfs) 
qqPlot(log10(dat3_r_R5$OpenWater_DCU_cfs+1)); shapiro.test(log10(dat3_r_R5$OpenWater_DCU_cfs+1)) 
qqPlot((dat3_r_R5$OpenWater_DCU_cfs)^2); shapiro.test((dat3_r_R5$OpenWater_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R5$OpenWater_DCU_cfs)); shapiro.test(sqrt(dat3_r_R5$OpenWater_DCU_cfs))

qqPlot(dat3_r_R8$OpenWater_DCU_cfs); shapiro.test(dat3_r_R8$OpenWater_DCU_cfs) 
qqPlot(log10(dat3_r_R8$OpenWater_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$OpenWater_DCU_cfs+1))
qqPlot((dat3_r_R8$OpenWater_DCU_cfs)^2); shapiro.test((dat3_r_R8$OpenWater_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$OpenWater_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$OpenWater_DCU_cfs))

qqPlot(dat3_r_R7$OpenWater_DCU_cfs); shapiro.test(dat3_r_R7$OpenWater_DCU_cfs) 
qqPlot(log10(dat3_r_R7$OpenWater_DCU_cfs+1)); shapiro.test(log10(dat3_r_R7$OpenWater_DCU_cfs+1)) 
qqPlot((dat3_r_R7$OpenWater_DCU_cfs)^2); shapiro.test((dat3_r_R7$OpenWater_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R7$OpenWater_DCU_cfs)); shapiro.test(sqrt(dat3_r_R7$OpenWater_DCU_cfs))

qqPlot(dat3_r_R8$OpenWater_DCU_cfs); shapiro.test(dat3_r_R8$OpenWater_DCU_cfs) 
qqPlot(log10(dat3_r_R8$OpenWater_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$OpenWater_DCU_cfs+1)) 
qqPlot((dat3_r_R8$OpenWater_DCU_cfs)^2); shapiro.test((dat3_r_R8$OpenWater_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$OpenWater_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$OpenWater_DCU_cfs))

#Determine distribution Urban depletion - 
qqPlot(dat3_r_R5$Urban_DCU_cfs); shapiro.test(dat3_r_R5$Urban_DCU_cfs) 
qqPlot(log10(dat3_r_R5$Urban_DCU_cfs+1)); shapiro.test(log10(dat3_r_R5$Urban_DCU_cfs+1)) 
qqPlot((dat3_r_R5$Urban_DCU_cfs)^2); shapiro.test((dat3_r_R5$Urban_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R5$Urban_DCU_cfs)); shapiro.test(sqrt(dat3_r_R5$Urban_DCU_cfs))

qqPlot(dat3_r_R8$Urban_DCU_cfs); shapiro.test(dat3_r_R8$Urban_DCU_cfs) 
qqPlot(log10(dat3_r_R8$Urban_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$Urban_DCU_cfs+1))
qqPlot((dat3_r_R8$Urban_DCU_cfs)^2); shapiro.test((dat3_r_R8$Urban_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Urban_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$Urban_DCU_cfs))

qqPlot(dat3_r_R7$Urban_DCU_cfs); shapiro.test(dat3_r_R7$Urban_DCU_cfs) 
qqPlot(log10(dat3_r_R7$Urban_DCU_cfs+1)); shapiro.test(log10(dat3_r_R7$Urban_DCU_cfs+1)) 
qqPlot((dat3_r_R7$Urban_DCU_cfs)^2); shapiro.test((dat3_r_R7$Urban_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R7$Urban_DCU_cfs)); shapiro.test(sqrt(dat3_r_R7$Urban_DCU_cfs))

qqPlot(dat3_r_R8$Urban_DCU_cfs); shapiro.test(dat3_r_R8$Urban_DCU_cfs) 
qqPlot(log10(dat3_r_R8$Urban_DCU_cfs+1)); shapiro.test(log10(dat3_r_R8$Urban_DCU_cfs+1)) 
qqPlot((dat3_r_R8$Urban_DCU_cfs)^2); shapiro.test((dat3_r_R8$Urban_DCU_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Urban_DCU_cfs)); shapiro.test(sqrt(dat3_r_R8$Urban_DCU_cfs))

#Determine distribution Rainfall - 
qqPlot(dat3_r_R5$Rain_cfs); shapiro.test(dat3_r_R5$Rain_cfs) 
qqPlot(log10(dat3_r_R5$Rain_cfs+1)); shapiro.test(log10(dat3_r_R5$Rain_cfs+1)) 
qqPlot((dat3_r_R5$Rain_cfs)^2); shapiro.test((dat3_r_R5$Rain_cfs)^2)
qqPlot(sqrt(dat3_r_R5$Rain_cfs)); shapiro.test(sqrt(dat3_r_R5$Rain_cfs))

qqPlot(dat3_r_R8$Rain_cfs); shapiro.test(dat3_r_R8$Rain_cfs) 
qqPlot(log10(dat3_r_R8$Rain_cfs+1)); shapiro.test(log10(dat3_r_R8$Rain_cfs+1))
qqPlot((dat3_r_R8$Rain_cfs)^2); shapiro.test((dat3_r_R8$Rain_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Rain_cfs)); shapiro.test(sqrt(dat3_r_R8$Rain_cfs))

qqPlot(dat3_r_R7$Rain_cfs); shapiro.test(dat3_r_R7$Rain_cfs) 
qqPlot(log10(dat3_r_R7$Rain_cfs+1)); shapiro.test(log10(dat3_r_R7$Rain_cfs+1)) 
qqPlot((dat3_r_R7$Rain_cfs)^2); shapiro.test((dat3_r_R7$Rain_cfs)^2)
qqPlot(sqrt(dat3_r_R7$Rain_cfs)); shapiro.test(sqrt(dat3_r_R7$Rain_cfs))

qqPlot(dat3_r_R8$Rain_cfs); shapiro.test(dat3_r_R8$Rain_cfs) 
qqPlot(log10(dat3_r_R8$Rain_cfs+1)); shapiro.test(log10(dat3_r_R8$Rain_cfs+1)) 
qqPlot((dat3_r_R8$Rain_cfs)^2); shapiro.test((dat3_r_R8$Rain_cfs)^2)
qqPlot(sqrt(dat3_r_R8$Rain_cfs)); shapiro.test(sqrt(dat3_r_R8$Rain_cfs))

#Determine distribution - Avg total Upper Rio Grande Water Operations Model  - Some not horrible
qqPlot(dat3_r_R5$five_day_avg_URGWOM_cfs); shapiro.test(dat3_r_R5$five_day_avg_URGWOM_cfs) 
qqPlot(log10(dat3_r_R5$five_day_avg_URGWOM_cfs+1)); shapiro.test(log10(dat3_r_R5$five_day_avg_URGWOM_cfs+1)) 
qqPlot((dat3_r_R5$five_day_avg_URGWOM_cfs)^2); shapiro.test((dat3_r_R5$five_day_avg_URGWOM_cfs)^2)
qqPlot(sqrt(dat3_r_R5$five_day_avg_URGWOM_cfs)); shapiro.test(sqrt(dat3_r_R5$five_day_avg_URGWOM_cfs))

qqPlot(dat3_r_R8$five_day_avg_URGWOM_cfs); shapiro.test(dat3_r_R8$five_day_avg_URGWOM_cfs) 
qqPlot(log10(dat3_r_R8$five_day_avg_URGWOM_cfs+1)); shapiro.test(log10(dat3_r_R8$five_day_avg_URGWOM_cfs+1))
qqPlot((dat3_r_R8$five_day_avg_URGWOM_cfs)^2); shapiro.test((dat3_r_R8$five_day_avg_URGWOM_cfs)^2)
qqPlot(sqrt(dat3_r_R8$five_day_avg_URGWOM_cfs)); shapiro.test(sqrt(dat3_r_R8$five_day_avg_URGWOM_cfs))

qqPlot(dat3_r_R7$five_day_avg_URGWOM_cfs); shapiro.test(dat3_r_R7$five_day_avg_URGWOM_cfs) 
qqPlot(log10(dat3_r_R7$five_day_avg_URGWOM_cfs+1)); shapiro.test(log10(dat3_r_R7$five_day_avg_URGWOM_cfs+1)) 
qqPlot((dat3_r_R7$five_day_avg_URGWOM_cfs)^2); shapiro.test((dat3_r_R7$five_day_avg_URGWOM_cfs)^2)
qqPlot(sqrt(dat3_r_R7$five_day_avg_URGWOM_cfs)); shapiro.test(sqrt(dat3_r_R7$five_day_avg_URGWOM_cfs))

qqPlot(dat3_r_R8$five_day_avg_URGWOM_cfs); shapiro.test(dat3_r_R8$five_day_avg_URGWOM_cfs) 
qqPlot(log10(dat3_r_R8$five_day_avg_URGWOM_cfs+1)); shapiro.test(log10(dat3_r_R8$five_day_avg_URGWOM_cfs+1)) 
qqPlot((dat3_r_R8$five_day_avg_URGWOM_cfs)^2); shapiro.test((dat3_r_R8$five_day_avg_URGWOM_cfs)^2)
qqPlot(sqrt(dat3_r_R8$five_day_avg_URGWOM_cfs)); shapiro.test(sqrt(dat3_r_R8$five_day_avg_URGWOM_cfs))

#Predictor Var - ET Toolbox Correlations ####

#ET toolbox predictors
dat3 <- read_csv("Data/Processed/ET_Toolbox_R_6_8.csv")

# reduce data down to one Reach5
temp <-  dat3 %>% filter(Reach=="5") 

# plot correlations (of data columns only)
pairs.panels(temp[,7:13], scale=T)
pairs.panels(temp[,7:13], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab <-  round(as.data.frame(cor(cov(temp[,7:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] <-  "no_corr"
View(tab)

write.csv(tab, "Data/Processed/R5_toolbox_corr.csv")

# reduce data down to one Reach6
temp <-  dat3 %>% filter(Reach=="6") 

# plot correlations (of data columns only)
pairs.panels(temp[,7:13], scale=T)
pairs.panels(temp[,7:13], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab <-  round(as.data.frame(cor(cov(temp[,7:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] <-  "no_corr"
View(tab)

write.csv(tab, "Data/Processed/R8_toolbox_corr.csv")

# reduce data down to one Reach7
temp <-  dat3 %>% filter(Reach=="7") 

# plot correlations (of data columns only)
pairs.panels(temp[,7:13], scale=T)
pairs.panels(temp[,7:13], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab <-  round(as.data.frame(cor(cov(temp[,7:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] <-  "no_corr"
View(tab)

write.csv(tab, "Data/Processed/R7_toolbox_corr.csv")

# reduce data down to one Reach8
temp <-  dat3 %>% filter(Reach=="8") 

# plot correlations (of data columns only)
pairs.panels(temp[,7:13], scale=T)
pairs.panels(temp[,7:13], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab <-  round(as.data.frame(cor(cov(temp[,7:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] <-  "no_corr"
View(tab)

write.csv(tab, "Data/Processed/R8_toolbox_corr.csv")

#Predictor Var - Otowi Index ####
dat4 <- read_csv("Data/Raw/Otowi Index Supply.csv")
names(dat4) <- c("Year","Index_KAF","Null")
dat4 <- dat4 %>% 
  select(Year, Index_KAF) %>% 
  filter(Year > 2002 & Year < 2019)

#plot
ggplot(dat4, aes(x=Year, y = Index_KAF))+
  geom_point()+ geom_line()+
  ylab("Otowi index (KAF)")

#normality
qqPlot(dat4$Index_KAF); shapiro.test(dat4$Index_KAF) 

#Predictor Var - Bosque Gage ####
#load data
dat5 <- read_csv("Data/Processed/BosqueGage2007_2018.csv")

#plot data
ggplot(dat5, aes(x=Date, y=Mean_cfs))+
  geom_point()

#normality
qqPlot(dat5$Mean_cfs); shapiro.test(dat5$Mean_cfs) 
qqPlot(log10(dat5$Mean_cfs+1)); shapiro.test(log10(dat5$Mean_cfs+1)) 
qqPlot((dat5$Mean_cfs)^2); shapiro.test((dat5$Mean_cfs)^2)
qqPlot(sqrt(dat5$Mean_cfs)); shapiro.test(sqrt(dat5$Mean_cfs))

#Predictor Var - San Acacia Diversion ####
dat6 <- read_csv("Data/Processed/SanAcaciaDiv.csv")

ggplot(dat6, aes(x=Date, y=Mean_daily_discharge_cfs))+
  geom_point()

qqPlot(dat6$Mean_daily_discharge_cfs); shapiro.test(dat6$Mean_daily_discharge_cfs)
qqPlot(log10(dat6$Mean_daily_discharge_cfs+1)); shapiro.test(log10(dat6$Mean_daily_discharge_cfs+1)) 
qqPlot((dat6$Mean_daily_discharge_cfs)^2); shapiro.test((dat6$Mean_daily_discharge_cfs)^2)
qqPlot(sqrt(dat6$Mean_daily_discharge_cfs)); shapiro.test(sqrt(dat6$Mean_daily_discharge_cfs))

#Predictor Var - Isleta Diversion ####
dat7 <- read_csv("Data/Processed/IsletaDiv.csv")

ggplot(dat7, aes(x=Date, y=Tot_diversion_cfs))+
  geom_point()

qqPlot(dat7$Tot_diversion_cfs); shapiro.test(dat7$Tot_diversion_cfs)
qqPlot(log10(dat7$Tot_diversion_cfs+1)); shapiro.test(log10(dat7$Tot_diversion_cfs+1)) 
qqPlot((dat7$Tot_diversion_cfs)^2); shapiro.test((dat7$Tot_diversion_cfs)^2)
qqPlot(sqrt(dat7$Tot_diversion_cfs)); shapiro.test(sqrt(dat7$Tot_diversion_cfs))

#Predictor Var - Diversion Correlations ####
dat8 <- read_csv("Data/Processed/SanAcaciaDiv.csv")
dat9 <- read_csv("Data/Processed/IsletaDiv.csv")

dat8 <- dat8[-(4011),]
dat8 <- dat8 %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  filter(Date > "2003-06-25")

Diversions <- cbind(dat8, dat9)
Diversions <- Diversions %>% 
  select(Mean_daily_discharge_cfs,Tot_diversion_cfs) %>% 
  rename(SanAcaciaDiv = Mean_daily_discharge_cfs, IsletaDiv = Tot_diversion_cfs )

# plot correlations (of data columns only)
pairs.panels(Diversions[,1:2], scale=T)
pairs.panels(Diversions[,1:2], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab <-  round(as.data.frame(cor(cov(Diversions[,1:2], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] <-  "no_corr"
View(tab)



