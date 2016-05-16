echo=TRUE
install.packages("knitr")
install.packages("lubridate")
install.packages("ggplot2")           
                 
                 
library(knitr)
library(lubridate)
library(ggplot2)
#library(dplyr)
#rm(list=ls())
rawdata <- read.csv("activity.csv",header = TRUE, sep = ',', colClasses = c("numeric", "character","integer"))
rawdata$date <- ymd(rawdata$date)
head(rawdata)
steps.date <- aggregate(steps ~ date, rawdata, sum)
head(steps.date)
ggplot(steps.date, aes(x = steps))
+     geom_histogram(fill = "blue", binwidth = 1000), labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
mean_steps <- mean(steps.date$steps, na.rm = TRUE)
median_steps <- median(steps.date$steps, na.rm = TRUE)
completedata <- na.omit(rawdata)
avgSteps <- aggregate(completedata$steps, list(interval = as.numeric(as.character(completedata$interval))), FUN = "mean")

names(avgSteps)[2] <- "meanOfSteps"
p <- ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

print(p)
int4maxsteps <- subset(avgSteps, avgSteps[,2] == max(avgSteps[,2]))
print(int4maxsteps)
sum(!complete.cases(rawdata))
rawdata_full <- rawdata
nas <- is.na(rawdata_full$steps)
avg_interval <- tapply(rawdata_full$steps, rawdata_full$interval, mean, na.rm=TRUE, simplify=TRUE)
rawdata_full$steps[nas] <- avg_interval[as.character(rawdata_full$interval[nas])]
                                        
sum(!complete.cases(rawdata_full))                                        
                       
steps.date <- aggregate(steps ~ date, rawdata_full, sum)

barplot(steps.date$steps, names.arg=steps.date$date, ylim=c(0, 25000), 
          +         xlab="date", ylab="sum(steps)",) 

