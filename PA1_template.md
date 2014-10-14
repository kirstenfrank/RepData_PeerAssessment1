---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
unzip("activity.zip")  #Unzip data.
activity<-read.csv("activity.csv")  ## read file into R
```

Convert the dates into proper dates, using as.Date and as.factor.


```r
activity$date<-as.Date(activity$date)
activity$date<-as.factor(activity$date)
library(lubridate)
activity$weekday<-wday(activity$date, label=TRUE)
```

```
## Error: unused argument (label = TRUE)
```
## What is mean total number of steps taken per day?

The best way to determine this is to convert days into a factor and make a new dataframe that is the result of summing the step in each day. Then use this dataframe to make a mean over all days.


```r
library(dplyr)
activity<-tbl_df(activity)
activity_day<-group_by(activity,date)
results<-summarize(activity_day,sum(steps,na.rm=TRUE))
### Datatable "results" keeps the information.
colnames(results)<-c("date","dailysteps")
mean(results$dailysteps)
```

```
## [1] 9354
```

```r
median(results$dailysteps)
```

```
## [1] 10395
```

```r
hist(results$dailysteps, breaks = 10, main = "Frequency of days with steps in range")
```

![plot of chunk histogram](figure/histogram.png) 

Results show a normal-looking distribution with possible outliers at zero and >20,000. The mean number of steps per day is 9354. The median number of steps per day is 10395.

## What is the average daily activity pattern?

The only information about time in the day is contained in the interval number. 


```r
activity_minutes<-group_by(activity,interval)
### This produces an average that we use for imputed steps--in the next code chunk
stepstime<-summarize(activity_minutes,mean(steps,na.rm=TRUE))
names(stepstime)<-c("interval","avesteps")
stepstime$interval<-as.integer(stepstime$interval)
plot(stepstime$interval,stepstime$avesteps,type="l")
```

![plot of chunk dailyactivity](figure/dailyactivity.png) 

```r
which.max(stepstime$avesteps)
```

```
## [1] 104
```

```r
length(unique(activity$date))
```

```
## [1] 61
```
Conclusions: the highest activity period (on average over the 61 days) is at 835.


## Imputing missing values

The technique for calculating a value to impute for missing values is calculate an average for that time of day. This information is in the dataset stepstime and needs to be transferred back to the dataset activity. 

First, how many values are NA?
There are 17568 rows in the dataset activity. There are 15264 complete cases. Therefore, there are 2304 incomplete cases.  

I want to use the mean for each 5 minute interval across the days to fill in the missing values.


```r
## Use merge to fill up the table with avesteps as a new variable.

activity_arranged<-arrange(activity,interval)
stepstime_arranged<-arrange(stepstime,interval)
newdata<-merge(activity_arranged,stepstime_arranged,by="interval")

## newdata is arranged by interval.
newdata$imputesteps<-ifelse(is.na(newdata$steps), newdata$avesteps,newdata$steps)

###YAYYYYYYY, this actually fills in the missing values.

newdata_group<-group_by(newdata,date)
newresults<-summarize(newdata_group, sum(imputesteps))
names(newresults)<-c("date","imputesteps")
### Datatable "results" keeps the information.
mean(newresults$imputesteps)
```

```
## [1] 10766
```

```r
median(newresults$imputesteps)
```

```
## [1] 10766
```

```r
hist(newresults$imputesteps, breaks = 10, main = "Frequency of days with steps in range using imputed data",xlab="Steps with imputed steps")
```

![plot of chunk imputesteps](figure/imputesteps.png) 


The mean steps per day are 10766 and the median steps per data are 10766.  These are higher than before, which makes sense. The NA's were treated like 0s in the previous sum. The mean equals the median, which makes me think that some days had no non-NA values and have now been replaced with the average at every interval. There are many fewer days with lower numbers of steps in the histogram. 

## Are there differences in activity patterns between weekdays and weekends?

First figure which days are weekend and which are not. Then calculate the average for each type of day. Notice that we are using the imputed values whenever there was an NA in the original data. 


```r
## Setup weekend vs weekday as a factor.
newdata$weekend<-
    ifelse(newdata$weekday %in% c("Mon","Tues","Wed","Thurs","Fri"),"weekday","weekend")
```

```
## Error: replacement has 0 rows, data has 17568
```

```r
newdata$weekend<-as.factor(newdata$weekend)
```

```
## Error: replacement has 0 rows, data has 17568
```

```r
### group_by can use more than one factor, summarize will work by the latest factor grouped
newdata_weekend_int<-group_by(newdata,weekend,interval)
```

```
## Error: index out of bounds
```

```r
newdata_weekend<-summarize(newdata_weekend_int,mean(steps,na.rm=TRUE))
```

```
## Error: object 'newdata_weekend_int' not found
```

```r
names(newdata_weekend)<-c("weekend","interval","meansteps")
```

```
## Error: object 'newdata_weekend' not found
```

Plot one plot with two panels of average steps vs. time on weekdays, and average steps vs. time on weekend days.


```r
library(ggplot2)
ggplot(newdata_weekend,aes(interval,meansteps)) + geom_line() + facet_grid(.~weekend)
```

```
## Error: object 'newdata_weekend' not found
```
