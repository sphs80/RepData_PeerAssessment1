This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

    summary(cars)

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

You can also embed plots, for example:

![](susmitanew_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

#### The program starts from here

echo=TRUE

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
====================================================

Loading the necessary packages
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    library(rmarkdown)
    library(datasets)
    library(knitr)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(ggplot2)
    library(lattice)
    rm(list=ls())

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
============================================

Reading the data from a comma separated file into a dataframe and
cleaning it \#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    rawdata <- read.csv("activity.csv",header = TRUE, sep = ',', colClasses = c("numeric", "character","integer"))
    rawdata$date <- ymd(rawdata$date)
    head(rawdata)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==============================================================

Histogram of total no of steps taken per day

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==============================================================

    steps.date <- aggregate(steps ~ date, rawdata, sum)

    hist(steps.date$steps, 
         breaks=seq(from=0, to=25000, by=2500),
         col="green", 
         xlab="Total number of steps", 
         ylim=c(0, 20), 
         main="Histogram of the total number of steps taken each day")

<img src="susmitanew_files/figure-markdown_strict/histogram-1.png" width="400" />

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==============================================================

Calculating mean and median Total no of steps taken per day
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    mean_steps <- mean(steps.date$steps, na.rm = TRUE)
    median_steps <- median(steps.date$steps, na.rm = TRUE)

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
====================================================================

Average daily activity pattern
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    completedata <- na.omit(rawdata)
    avgSteps <- aggregate(completedata$steps, list(interval = as.numeric(as.character(completedata$interval))), FUN = "mean")

    names(avgSteps)[2] <- "meanOfSteps"

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==========================================================

Time-series plot
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    p <- ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

    print(p)

![](susmitanew_files/figure-markdown_strict/time_series-1.png)

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
====================================================================================================================

5-minute interval, on average across all the days in the dataset,that
contains the maximum number of steps
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    int4maxsteps <- subset(avgSteps, avgSteps[,2] == max(avgSteps[,2]))
    print(int4maxsteps)

    ##     interval meanOfSteps
    ## 104      835    206.1698

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==============================================================================

Inputing missing values
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==========================================

Total no of missing values(records with missing values)
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    sum(!complete.cases(rawdata))

    ## [1] 2304

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
======================================================

Devising a strategy to put back the missing values
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    rawdata_full <- rawdata
    nas <- is.na(rawdata_full$steps)
    avg_interval <- tapply(rawdata_full$steps, rawdata_full$interval, mean, na.rm=TRUE, simplify=TRUE)
    rawdata_full$steps[nas] <- avg_interval[as.character(rawdata_full$interval[nas])]

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
============================================================================

proof that database created by putting back the missing values has no
such record

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==============================================================================

    sum(!complete.cases(rawdata_full)) 

    ## [1] 0

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
==========================================================

a histogram of the total number of steps taken each day
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    steps.date <- aggregate(steps ~ date, rawdata_full, sum)
    names(steps.date) <- c("date", "total")
    hist(steps.date$total, 
         breaks=seq(from=0, to=25000, by=2500),
         col="blue", 
         xlab="Total no of steps",
         ylim=c(0, 20), 
         main="Histogram of the total number of steps taken each day(NA replaced by mean value)")

![](susmitanew_files/figure-markdown_strict/histogram_revised-1.png)

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
============================================================

mean and median calculated here
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    mean(steps.date$total)

    ## [1] 10766.19

    median(steps.date$total)

    ## [1] 10766.19

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
====================================================================

Creating factor variables indicating whether the date is a weekday or
weekend.
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    rawdata_full$weekdays <- factor(format(steps.date$date, "%A"))
    levels(rawdata_full$weekdays)  <- list(weekday = c("Monday", "Tuesday",
                                                  "Wednesday", 
                                                  "Thursday", "Friday"),
                                      weekend = c("Saturday", "Sunday"))


    levels(rawdata_full$weekdays)

    ## [1] "weekday" "weekend"

    table(rawdata_full$weekdays)

    ## 
    ## weekday weekend 
    ##   12960    4608

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
============================================================================

a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis)
\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    averagesteps_day <- aggregate(rawdata_full$steps, 
                              list(interval = as.numeric(as.character(rawdata_full$interval)), 
                                   weekdays = rawdata_full$weekdays),
                              FUN = "mean")
                              
    names(averagesteps_day)[3] <- "av_OfSteps"

    plot2 <- xyplot(averagesteps_day$av_OfSteps ~ averagesteps_day$interval | averagesteps_day$weekdays, 
                    layout = c(1, 2), type = "l", 
                    xlab = "Interval", ylab = "Number of steps")
    print(plot2)
