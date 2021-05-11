#Read me ####
#The purpose of this script is to explore the River Eyes data set

#Libraries ####

library(tidyverse)
library(cowplot)
library(car) #qq plot fxn
library(lubridate) #dates
library(psych) #correlation
library(janitor) #duplication
library(forecast) # for autotemporal Acf fxn
library(tsibble)

#Response Var - Daily Expansion of Dry river miles ####
#use read_csv for date format to be automatic but this makes it a tibble
dat <- read_csv("Data/Processed/DailyExpansionDry.csv")

# add day of year for plotting
dat$Day <-  lubridate::yday(dat$Date)
dat <- dat %>% 
  select(Date, DistanceDry,Year, Reach, Day) %>% 
  mutate(Reach = as.factor(as.character(Reach)))

#Plot data Daily Expansion of Dry river miles
pl1 <- ggplot(data=dat, aes(x=Day, y=DistanceDry))+
  geom_point()+
  facet_wrap(~Reach, scales="free_y")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1), strip.background = element_blank())

pl2 <- ggplot(data=dat, aes(x=Date, y=DistanceDry))+
  geom_point()+
  facet_wrap(~Reach, scales="free_y")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1), strip.background = element_blank())+
  ylab("")

#combine plots into one figure and save to file
ExtentDryPlots <- plot_grid(pl1, pl2)
ggsave2("Figures/Fig3_ExtentOfDrying.png", ExtentDryPlots) #saving to pdf did not fully work

#Response Var - Daily Expansion of Dry River miles Temporal Auotcorrelation Reaches 5 & 7 ####
#Reach 5 
dat10 <- read_csv("Data/Processed/DailyExpansionDry.csv")

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
dat10 <- read_csv("Data/Processed/DailyExpansionDry.csv")

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

#Response Var - Persistence of Dry River Mile over the year ####
dat1 <- read_csv("Data/Processed/PersistenceDryRM.csv")

#Summary explore persistence Dry river mile
summary(dat1)
with(dat1, table(Sum_days_rm_dry, RM))

hist(dat1$Sum_days_rm_dry, xlab = "Number of days a given river mile was dry", main = (""))

dat1 %>% 
  group_by(Year) %>% 
  summarise(DistMn = mean(Sum_days_rm_dry))

#Plot data persistence Dry river mile
plot(dat1$Sum_days_rm_dry~dat1$RM, xlab="River Mile", ylab="Number of days dry")


# plot
RMDryPlot <- ggplot(data=dat1, aes(x=RM, y=Sum_days_rm_dry))+
  geom_point()+
  geom_line()+
  facet_wrap(~Year, scale="free")+
  ylab("Number of days dry")+
  xlab("River Mile")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                          axis.line = element_line(color="black", size=1), panel.background = element_blank(), text=element_text(size =12),
        strip.background = element_blank())

#save to file
ggsave("Figures/Fig2_AnnualDryingRM_Yr.pdf", RMDryPlot)

#normality
qqPlot(dat1$Sum_days_rm_dry); shapiro.test(dat1$Sum_days_rm_dry)
qqPlot(log(dat1$Sum_days_rm_dry+1)); shapiro.test(log(dat1$Sum_days_rm_dry+1))
qqPlot((dat1$Sum_days_rm_dry)^2); shapiro.test((dat1$Sum_days_rm_dry)^2)
qqPlot(sqrt(dat1$Sum_days_rm_dry)); shapiro.test(sqrt(dat1$Sum_days_rm_dry))

#Response Var - Persistence of Annual Dry River Mile Temporal Auotcorrelation for RM 74####
dat16 <- read_csv("Data/Processed/PersistenceDryRM.csv")

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

#Response Var - Occurrence of Dry RM each day ####
dat2 <- read_csv("Data/Processed/DailyOccurrenceDryRM.csv")

#temporary data frame to plot river mile condition
temp <- as.data.frame(with(dat2, table(RM, Condition.b)))

RiverMileCondition <-ggplot(data = temp, aes(x=RM, y=Freq, color=Condition.b))+
  geom_point()+
  scale_color_manual(values=c("grey", "black"),
                       name="Condition", labels=(c("Wet", "Dry")))+
  scale_x_discrete(breaks = seq(40,180,10))+
  labs(x="River mile", y="Number of days (2003-2018)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), text=element_text(size =18),
        legend.text = element_text(size=18))+
  scale_y_continuous(labels = scales::comma)+
  annotate("segment", x=20, xend =20, y=2300, yend=200, colour="black", size=1, alpha=0.8, arrow=arrow())+
  annotate("segment", x=98, xend =98, y=2300, yend=200, colour="black", size=1, alpha=0.8, arrow=arrow())+
  annotate("segment", x=79, xend =79, y=2300, yend=200, colour="black", size=1, alpha=0.8, arrow=arrow())+
  annotate("text", x = c(20,79,98), y = c(2700,2700, 2700), label = c("Low conveyance \npumping to river", "Subsurface \nreturn", "Peralta irrigation \nreturn"),
           color="black", size=4 , angle=0)

ggsave2("Figures/Fig1_RiverMileCondition.pdf", RiverMileCondition) 
  
#Response Var - Daily Occurrence Dry Temporal Auotcorrelation for RM 74####
dat19 <- read_csv("Data/Processed/DailyOccurrenceDryRM.csv")

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
dat3 <- read_csv("Data/Processed/Predictors_mn_sum.csv")
dat4 <- read_csv("Data/Processed/ET_Toolbox_R_6_8.csv")

ET_pl1 <-ggplot(dat4, aes(x=Date, y=Ag_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Agriculture")+
  ylab("Evapotranspiration (cfs)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1), strip.background = element_blank())

ET_pl2 <- ggplot(dat4, aes(x=Date, y=Riparian_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Riparian")+
  ylab("Evapotranspiration (cfs)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1), strip.background = element_blank())

ET_pl3 <-ggplot(dat4, aes(x=Date, y=OpenWater_DCU_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande - Open water")+
  ylab("Evaporation (cfs)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1), strip.background = element_blank())

ET_pl4 <- ggplot(dat4, aes(x=Date, y=Rain_cfs))+
  geom_point()+
  facet_wrap(~Reach)+
  ggtitle("Rio Grande reaches - Rainfall")+
  ylab("Addition (cfs)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color="black", size=1), panel.background = element_blank(), 
        text=element_text(family = "Times New Roman", size =12),
        legend.text = element_text(size=12),
        legend.position = c(1,1), legend.justification = c(1,1), strip.background = element_blank())

plot_grid(ET_pl1, ET_pl2, ET_pl3, ET_pl4)

#Filter to Reaches and 5000 sample for normality testing for the agriculture sector
dat3_r_R5 <- dat3 %>% 
  select(Ag_DCU_cfs_5) %>% 
  sample_n(5000) 

dat3_r_R6 <- dat3 %>% 
  select(Ag_DCU_cfs_6) %>%  
  sample_n(5000) 

dat3_r_R7 <- dat3 %>% 
  select(Ag_DCU_cfs_7) %>%  
  sample_n(5000) 

dat3_r_R8 <- dat3 %>% 
  select(Ag_DCU_cfs_8) %>%  
  sample_n(5000) 

#Determine distribution Ag depletion
qqPlot(dat3_r_R5$Ag_DCU_cfs_5); shapiro.test(dat3_r_R5$Ag_DCU_cfs_5) 
qqPlot(log10(dat3_r_R5$Ag_DCU_cfs_5+1)); shapiro.test(log10(dat3_r_R5$Ag_DCU_cfs_5+1)) 
qqPlot((dat3_r_R5$Ag_DCU_cfs_5)^2); shapiro.test((dat3_r_R5$Ag_DCU_cfs_5)^2)
qqPlot(sqrt(dat3_r_R5$Ag_DCU_cfs_5)); shapiro.test(sqrt(dat3_r_R5$Ag_DCU_cfs_5))

qqPlot(dat3_r_R6$Ag_DCU_cfs_6); shapiro.test(dat3_r_R6$Ag_DCU_cfs_6) 
qqPlot(log10(dat3_r_R6$Ag_DCU_cfs_6+1)); shapiro.test(log10(dat3_r_R6$Ag_DCU_cfs_6+1)) 
qqPlot((dat3_r_R6$Ag_DCU_cfs_6)^2); shapiro.test((dat3_r_R6$Ag_DCU_cfs_6)^2)
qqPlot(sqrt(dat3_r_R6$Ag_DCU_cfs_6)); shapiro.test(sqrt(dat3_r_R6$Ag_DCU_cfs_6))

qqPlot(dat3_r_R7$Ag_DCU_cfs_7); shapiro.test(dat3_r_R7$Ag_DCU_cfs_7) 
qqPlot(log10(dat3_r_R7$Ag_DCU_cfs_7+1)); shapiro.test(log10(dat3_r_R7$Ag_DCU_cfs_7+1)) 
qqPlot((dat3_r_R7$Ag_DCU_cfs_7)^2); shapiro.test((dat3_r_R7$Ag_DCU_cfs_7)^2)
qqPlot(sqrt(dat3_r_R7$Ag_DCU_cfs_7)); shapiro.test(sqrt(dat3_r_R7$Ag_DCU_cfs_7))

qqPlot(dat3_r_R8$Ag_DCU_cfs_8); shapiro.test(dat3_r_R8$Ag_DCU_cfs_8) 
qqPlot(log10(dat3_r_R8$Ag_DCU_cfs_8+1)); shapiro.test(log10(dat3_r_R8$Ag_DCU_cfs_8+1)) 
qqPlot((dat3_r_R8$Ag_DCU_cfs_8)^2); shapiro.test((dat3_r_R8$Ag_DCU_cfs_8)^2)
qqPlot(sqrt(dat3_r_R8$Ag_DCU_cfs_8)); shapiro.test(sqrt(dat3_r_R8$Ag_DCU_cfs_8))


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

write.csv(tab, "Data/Processed/R6_toolbox_corr.csv")

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
par(mfrow=c(1,1))
qqPlot(dat4$Index_KAF); shapiro.test(dat4$Index_KAF) 

#Predictor Var - Bosque Farms Gage ####
#load data
dat5 <- read.csv("Data/Processed/Predictors.csv")

#plot data
ggplot(dat5, aes(x=Date, y=Mean_cfs_BosFarms))+
  geom_point()

#normality
qqPlot(dat5$Mean_cfs_BosFarms); shapiro.test(dat5$Mean_cfs_BosFarms) 
qqPlot(log10(dat5$Mean_cfs_BosFarms+1)); shapiro.test(log10(dat5$Mean_cfs_BosFarms+1)) 
qqPlot((dat5$Mean_cfs_BosFarms)^2); shapiro.test((dat5$Mean_cfs_BosFarms)^2)
qqPlot(sqrt(dat5$Mean_cfs_BosFarms)); shapiro.test(sqrt(dat5$Mean_cfs_BosFarms))

#Predictor Var - San Acacia Gage ####
 
#get data and format to get year and day for plotting
dat15 <- read.csv("Data/Processed/RioGrandeGages2003_2018.csv") %>% 
  mutate(Yr = lubridate::year(Date)) %>% 
  mutate(Dy = lubridate::yday(Date))

#plotting
  #plot to see annual distribution 
  ggplot(dat15, aes(x=Date, y=Mean_cfs_SanAcacia))+
  geom_point()

  #plot data by year to visualize annual distribution
  ggplot(dat15, aes(Mean_cfs_SanAcacia))+
    geom_histogram()+
    facet_wrap(~Yr)
  #log10 transformed to visualize normality BETTER
  ggplot(dat15, aes(log10(Mean_cfs_SanAcacia)+1))+
    geom_histogram()+
    facet_wrap(~Yr)
  #square transformed to visualize normality NOT GOOD
  ggplot(dat15, aes((Mean_cfs_SanAcacia)^2))+
    geom_histogram()+
    facet_wrap(~Yr)
  #square root transformed to visualize normality NOT AS GOOD AS LOG10
  ggplot(dat15, aes(sqrt(Mean_cfs_SanAcacia)))+
    geom_histogram()+
    facet_wrap(~Yr)
  
#test for normality with transformations
dat15_short <- dat15 %>% 
  sample_n(5000)

  qqPlot(dat15$Mean_cfs_SanAcacia); shapiro.test(dat15_short$Mean_cfs_SanAcacia) #W=0.7 p<0.5
  qqPlot(log10(dat15$Mean_cfs_SanAcacia+1)); shapiro.test(log10(dat15_short$Mean_cfs_SanAcacia+1)) #W=0.9 p<0.5
  qqPlot((dat15$Mean_cfs_SanAcacia)^2); shapiro.test((dat15_short$Mean_cfs_SanAcacia)^2) #W=0.4 p<0.5
  qqPlot(sqrt(dat15$Mean_cfs_SanAcacia)); shapiro.test(sqrt(dat15_short$Mean_cfs_SanAcacia)) #W=0.9 p<0.5

#Predictor Var - San Acacia Diversion ####
dat <- read_csv("Data/Processed/Diversion_discharges.csv")

 #get San Acacia data and format to get year and day for plotting
dat6 <- dat %>% 
  select(Date, SanAcacia_mean_daily_div_cfs) %>% 
  mutate(Yr = lubridate::year(Date)) %>% 
  mutate(Dy = lubridate::yday(Date))

 #plot all dates
ggplot(dat6, aes(x=Date, y=SanAcacia_mean_daily_div_cfs))+
  geom_point()

 #plot data by year to visualize annual distribution
ggplot(dat6, aes(SanAcacia_mean_daily_div_cfs))+
  geom_histogram()+
  facet_wrap(~Yr)
       #log10 transformed to visualize normality BETTER
ggplot(dat6, aes(log10(SanAcacia_mean_daily_div_cfs)+1))+
  geom_histogram()+
  facet_wrap(~Yr)
      #square transformed to visualize normality NOT GOOD
ggplot(dat6, aes((SanAcacia_mean_daily_div_cfs)^2))+
  geom_histogram()+
  facet_wrap(~Yr)
      #square root transformed to visualize normality SIMILAR TO LOG10
ggplot(dat6, aes(sqrt(SanAcacia_mean_daily_div_cfs)))+
  geom_histogram()+
  facet_wrap(~Yr)

 #test for normality with transformations
qqPlot(dat6$SanAcacia_mean_daily_div_cfs); shapiro.test(dat6$SanAcacia_mean_daily_div_cfs) #W=0.7 p<0.5
qqPlot(log10(dat6$SanAcacia_mean_daily_div_cfs+1)); shapiro.test(log10(dat6$SanAcacia_mean_daily_div_cfs+1)) #W=0.9 p<0.5
qqPlot((dat6$SanAcacia_mean_daily_div_cfs)^2); shapiro.test((dat6$SanAcacia_mean_daily_div_cfs)^2) #W=0.3 p<0.5
qqPlot(sqrt(dat6$SanAcacia_mean_daily_div_cfs)); shapiro.test(sqrt(dat6$SanAcacia_mean_daily_div_cfs)) #W=0.9 p<0.5

#Predictor Var - Isleta Diversion ####
dat7 <- read_csv("Data/Processed/Diversion_discharges.csv")

ggplot(dat7, aes(x=Date, y=Isleta_mean_daily_div_cfs))+
  geom_point()

qqPlot(dat7$Isleta_mean_daily_div_cfs); shapiro.test(dat7$Isleta_mean_daily_div_cfs)
qqPlot(log10(dat7$Isleta_mean_daily_div_cfs+1)); shapiro.test(log10(dat7$Isleta_mean_daily_div_cfs+1)) 
qqPlot((dat7$Isleta_mean_daily_div_cfs)^2); shapiro.test((dat7$Isleta_mean_daily_div_cfs)^2)
qqPlot(sqrt(dat7$Isleta_mean_daily_div_cfs)); shapiro.test(sqrt(dat7$Isleta_mean_daily_div_cfs))

#Predictor Var - Correlations ####
dat8 <- dat7 %>% 
  mutate(Date = as.Date(Date)) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day"), 
           fill = list(value = NA)) %>% 
  filter(Date > "2003-06-25")

# plot correlations (of data columns only)
pairs.panels(dat8[,2:3], scale=T)
pairs.panels(dat8[,2:3], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab <-  round(as.data.frame(cor(cov(dat8[,2:3], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] <-  "no_corr"
View(tab)



