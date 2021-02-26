#Read me####
#The purpose of this script is to explore the River Eyes data set

#Libraries####
library(tidyverse)

#Load data ####
  #use read_csv for date format to be automatic but this makes it a tibble
dat <- read_csv("Data/Processed/RiverEyesDat2.csv")


#Format date####
dat$Date <- as.Date(dat$Date, format("%Y-%m-%d"))
dat$Julian <- as.integer(format(dat$Date, "%j"))


#Summary explore####
summary(dat)

#Plot data####
plot(dat$Distance~dat$Date)
ggplot(dat, aes(x=Julian, y=URM))+
  facet_wrap(vars(Year))+
  geom_point()+
  scale_x_continuous(breaks = seq(0, 365, by=50))

hist(dat$Distance)

par(mfrow = c(2,1))
hist(dat$URM, xlab="Upper river mile dry", main="")
hist(dat$LRM, xlab="Lowest river mile dry", main="")
  
#histogram of distance of dry rivers by year
ggplot(dat, aes(Distance))+
  facet_wrap(vars(Year))+
  geom_histogram()
  
 #relationship drying distance to upstream appearance
ggplot(dat, aes(x=Distance, y=URM))+
  geom_point()
        #by year
ggplot(dat, aes(x=Distance, y=URM))+
  facet_wrap(vars(Year))+
  geom_point()

  #relationship drying distance to downstream end
ggplot(dat, aes(x=Distance, y=LRM))+
  geom_point()
        #by year
ggplot(dat, aes(x=Distance, y=LRM))+
  facet_wrap(vars(Year))+
  geom_point()
