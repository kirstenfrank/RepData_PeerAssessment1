# Reproducible Research: Peer Assessment 1


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
## What is mean total number of steps taken per day?

The best way to determine this is to convert days into a factor and make a new dataframe that is the result of summing the step in each day. Then use this dataframe to make a mean over all days.


```r
results<-tapply(activity$steps,activity$date, sum, na.rm=TRUE)
### Datatable "results" keeps the information.
mean(results)
```

```
## [1] 9354
```

```r
median(results)
```

```
## [1] 10395
```

```r
hist(results, breaks = 10, main = "Frequency of days with steps in range")
```

![plot of chunk histogram](./PA1_template_files/figure-html/histogram.png) 

Results show a normal-looking distribution with possible outliers at zero and >20,000. The mean number of steps per day is 9354. The median number of steps per day is 10395.

## What is the average daily activity pattern?

The only information about time in the day is contained in the interval number. 


```r
activity$intlevels<-as.ordered(activity$interval)
stepstime<-tapply(activity$steps,activity$intlevels,mean,na.rm=TRUE)
stepstime<-as.data.frame(stepstime)
stepstime$interval<-row.names(stepstime)
stepstime$avesteps<-stepstime$stepstime
stepstime$interval<-as.integer(stepstime$interval)
plot(stepstime$interval,stepstime$avesteps,type="l")
```

![plot of chunk dailyactivity](./PA1_template_files/figure-html/dailyactivity.png) 

```r
which.max(stepstime$avesteps)
```

```
## 835 
## 104
```
Conclusions: the highest activity is at 835.


## Imputing missing values

The technique for calculating a value to impute for missing values is calculate an average for that time of day. This information is in the dataset stepstime and needs to be transferred back to the dataset activity. 

First, how many values are NA?
There are 17568. There are 15264 complete cases. Therefore, there are 2304.  

I want to use the mean for that 5 minute interval across the days to fill in the missing values.


```r
## Use merge to fill up the table with avesteps as a new variable.
newdata<-merge(activity,stepstime,by="interval")

## Note that order has been changed, but that isn't important.

newdata$imputesteps<-ifelse(is.na(newdata$steps), newdata$avesteps,newdata$steps)

###YAYYYYYYY, this actually fills in the missing values
```


Now to repeat the analysis with imputed values.


```r
newresults<-tapply(newdata$imputesteps,newdata$date, sum, na.rm=TRUE)
### Datatable "results" keeps the information.
mean(newresults)
```

```
## [1] 10766
```

```r
median(newresults)
```

```
## [1] 10766
```

```r
hist(newresults, breaks = 10, main = "Frequency of days with steps in range using imputed data")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 


The mean steps per day are 1.0766 &times; 10<sup>4</sup> and the median steps per data are 1.0766 &times; 10<sup>4</sup>.  These are higher than before, which makes sense. The mean equals the median, which makes me think there is a mistake in the calculation. But I can't find it yet. There are many fewer days with lower numbers of steps. 

## Are there differences in activity patterns between weekdays and weekends?

First figure which days are weekend and which are not. 


```r
## Setup weekend vs weekday as a factor.
activity$weekend<-
    ifelse(activity$weekday %in% c("Mon","Tues","Wed","Thurs","Fri"),"weekday","weekend")
activity$weekend<-as.factor(activity$weekend)
```

Then break into two datasets, one for weekends and one for weekdays.


```r
weekdayset<-subset(activity, activity$weekend=="weekday")
weekendset<-subset(activity, activity$weekend=="weekend")
## average activity over time for weekdays
aveweekday<-tapply(weekdayset$steps,weekdayset$intlevels,mean,na.rm=TRUE)
aveweekend<-tapply(weekendset$steps,weekendset$intlevels,mean,na.rm=TRUE)

aveweekday<-as.data.frame(aveweekday)
aveweekday$interval<-row.names(aveweekday)
aveweekday$avesteps<-aveweekday$aveweekday
aveweekday$interval<-as.integer(aveweekday$interval)
aveweekend<-as.data.frame(aveweekend)
aveweekend$interval<-row.names(aveweekend)
aveweekend$avesteps<-aveweekend$aveweekend
aveweekend$interval<-as.integer(aveweekend$interval)
newplot<-merge(aveweekday,aveweekend,by="interval")
library(ggplot2)
```

Plot two diagrams of average steps vs. time on weekdays, and average steps vs. time on weekend days.


```r
ggplot(newplot,aes(interval,aveweekday)) + geom_line()
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-51.png) 

```r
ggplot(newplot,aes(interval,aveweekend)) + geom_line()
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-52.png) 
