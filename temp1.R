#Set working directory, load the data, and load the dplyer library
setwd("R:/5-Reproducible/RepData_PeerAssessment1")
rawdata <- data.frame(read.csv("activity.csv"))
library(dplyr)

#Summarize total steps by date
stepsByDate <- rawdata %>% 
      filter(!is.na(steps)) %>% 
      group_by(date) %>% 
      select(steps) %>% 
      summarize(totalSteps = sum(steps))

hist(stepsByDate$totalSteps, main="Histogram of Steps per Day", xlab="Steps per Day") #Print this, steps per day 1
mean(stepsByDate$totalSteps)  #Print this, steps per day 2
median(stepsByDate$totalSteps) #Print this, steps per day 2

stepsByInterval <- rawdata %>% 
      filter(!is.na(steps)) %>% 
      group_by(interval) %>% 
      select(steps) %>% 
      summarize(meanSteps = mean(steps))

plot(stepsByInterval$interval, stepsByInterval$meanSteps, type="l", 
     xlab="Interval Number", ylab="Mean Steps Per Interval")  #Print this, daily activity pattern 1

maxMeanSteps <- max(stepsByInterval$meanSteps)
intervalWithMaxMeanSteps <- (stepsByInterval %>% filter(meanSteps == maxMeanSteps))$interval
intervalWithMaxMeanSteps #Print this, daily activity pattern 2

hist(stepsByInterval$meanSteps) #Print this

#Number of rows with NA values
length((rawdata %>% filter(is.na(steps)))$steps)  #Print this, missing values 1

rawMeanStepsPerDay <- mean(stepsByDate$totalSteps)
rawMedianStepsPerDay <- median(stepsByDate$totalSteps)
rawMeanStepsPerDay #Print this
rawMedianStepsPerDay #Print this


cleandata <- rawdata


meanStepsForInterval <- sapply(cleandata$interval, 
      function(x){
            as.numeric(stepsByInterval[ which(stepsByInterval$interval == x), 2])
            })

cleandata$meanStepsForInterval <- meanStepsForInterval

cleandata$steps <- with(cleandata, ifelse(is.na(steps), meanStepsForInterval, steps))

cleandata$meanStepsForInterval <- NULL


cleanStepsByDate <- cleandata %>% 
      group_by(date) %>% 
      select(steps) %>% 
      summarize(totalSteps = sum(steps))

hist(cleanStepsByDate$totalSteps)  #Print this, missing 4

cleanMeanStepsPerDay <- mean(cleanStepsByDate$totalSteps)
cleanMedianStepsPerDay <- median(cleanStepsByDate$totalSteps)

cleanMeanStepsPerDay  #Print this, missing 4
cleanMedianStepsPerDay  #Print this, missing 4

cleandata <- cleandata %>% mutate(DayType = as.factor(ifelse(weekdays(as.POSIXlt(date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")))

cleanStepsByInterval <- cleandata %>% 
      group_by(interval,DayType) %>% 
      select(steps) %>% 
      summarize(meanSteps = mean(steps))

xyplot(meanSteps~interval | factor(DayType), 
       data=cleanStepsByInterval, type="l",layout=c(1,2),
       ylab="Mean Number of Steps per Interval", xlab="Interval", 
       main="Comparison of weekday and weekend steps by interval")
#Print this, differences 2

#cleanIntervalWithMaxAverageSteps <- max(cleanStepsByInterval$meanSteps)
#cleanStepsByInterval %>% filter(meanSteps == cleanIntervalWithMaxAverageSteps)
#hist(cleanStepsByInterval$meanSteps)


