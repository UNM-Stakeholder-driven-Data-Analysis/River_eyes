#Read me####
#The purpose of this script is to explore the River Eyes data set

#Libraries####
library(tidyverse)

#Load data Daily Dry river mile####
  #use read_csv for date format to be automatic but this makes it a tibble
dat <- read_csv("Data/Processed/DailyDryRM.csv")
dat$RmSeq.f <- as.factor(as.character(dat$RmSeq.f))
dat$Condition.f <- as.factor(as.character(dat$Condition.b))
dat$Year.f <- as.factor(as.character(dat$Year))

#Summary explore####
summary(dat)


#Plot data####
ggplot(dat)+
  geom_bar(aes(x=RmSeq.f, fill=Condition.f), stat = "count")) +
  facet_wrap(~Year.f)


ggplot(dat, aes(x=Year.f, y=RmSeq))+
  facet_wrap(vars(Condition.f))+
  geom_point()+
  scale_x_continuous(breaks = seq(0, 365, by=50))

hist(dat$Distance)

par(mfrow = c(2,1))
hist(dat$, xlab="Upper river mile dry", main="")
hist(dat$LRM, xlab="Lowest river mile dry", main="")
  
#histogram of distance of dry rivers by year
ggplot(dat, aes(Condition.b))+
  facet_wrap(vars(RmSeq))+
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


#Load data Daily Extent Dry river mile####
dat2 <- read_csv("Data/Processed/DailyExtentDry.csv")

#Summary explore####
summary(dat2)

#Plot data####
hist(dat2$Distance)
plot(dat2$Distance~dat2$Year)

ggplot(dat2, aes(Distance))+
  geom_histogram()+
  facet_wrap(~Year, scale="free") +
  ggtitle("Histograms by Year")+
  xlab("Distance of river drying events (total river mile)")
