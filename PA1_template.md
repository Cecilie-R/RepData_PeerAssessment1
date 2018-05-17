---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Here the data is loaded, making sure that the date column is automatically read by R as dates in the format YYYY-MM-DD. The Steps and Interval columns are classed as numeric values.

```r
library(readr)
activity <- read_csv("~/GitHub/RepData_PeerAssessment1/activity.zip", 
    col_types = cols(date = col_date(format = "%Y-%m-%d"), 
        interval = col_number(), steps = col_number()), 
    na = "NA")
```


## What is mean total number of steps taken per day?

```r
#Calcualte the total number of steps each day
SumStepsDay<-aggregate(activity$steps, by=list(Category=activity$date), FUN=sum)

#Remove rows with NA values
SumStepsDay<-na.omit(SumStepsDay)

#create a histogram plot of the total number of steps each day.
hist(SumStepsDay$x)
```

![](PA1_template_files/figure-html/steps-1.png)<!-- -->

```r
#calculate the mean and median number of steps each day.
meanSteps<-mean(SumStepsDay$x)
medianSteps<-median(SumStepsDay$x)
```

ANSWER: The mean number of steps is 1.0766189\times 10^{4} and the median number of steps is 1.0765\times 10^{4}.

## What is the average daily activity pattern?

```r
library(ggplot2)
library(data.table)
#calculate the mean number of steps taken in each time interval over the recorded days
MeanIntervalSteps<- aggregate(x=list(MeanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)

#Create the time interval plot
ggplot(data=MeanIntervalSteps, aes(x=interval, y=MeanSteps)) +
    geom_line() +
    xlab("recorded 5-minute intervals") +
    ylab("average number of steps taken") 
```

![](PA1_template_files/figure-html/interval-1.png)<!-- -->

```r
#Look up which interval had the most steps over the recorded duration
maxSteps<-subset(MeanIntervalSteps, MeanSteps==max(MeanIntervalSteps$MeanSteps))

#Extract the value
interval<-maxSteps$interval
steps<-maxSteps$MeanSteps
```
On average the time interval of 835 had the most steps, with an average of 206.1698113.

## Imputing missing values

```r
nrow(subset(activity, is.na(steps)))
```

```
## [1] 2304
```
## Are there differences in activity patterns between weekdays and weekends?
