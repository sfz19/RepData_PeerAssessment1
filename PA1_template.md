---
title: "Reproducible Research: Peer Assessment 1"
author: "sfz"
date: "25/08/2021"
output: 
  html_document :
    keep_md: true
---

# INTRODUCTION  

This is R markdown file for coursera Reproducible research project 1.

## Loading and preprocessing the data


```r
unzip("./activity.zip")
activity <- read.csv("./activity.csv",colClasses=c("numeric", "Date", "numeric"))
summary(activity)  
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
head(activity)  
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity) 
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```



## What is mean total number of steps taken per day?

Total no. of steps per Day and make histogram. 


```r
stepsperday<-aggregate(steps~date,activity,sum,na.rm=TRUE)
hist(stepsperday$steps,col = "blue",main = "Histogram for total Number of steps per Day",xlab = "steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
 
Mean and Median for total number of steps per day.


```r
meanperday<-mean(stepsperday$steps)
print(meanperday)
```

```
## [1] 10766.19
```

```r
medianperday<-median(stepsperday$steps)
print(medianperday)
```

```
## [1] 10765
```

The mean per day is 1.0766189\times 10^{4} and median per day is 1.0765\times 10^{4}.


## What is the average daily activity pattern?

Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
StepsInterval <- aggregate(steps ~ interval, data=activity, mean)
plot(x=StepsInterval$interval, y=StepsInterval$steps,type="l",main="Time Series Plot of Average Steps Taken per Interval",
     ylab="The average number of steps taken across all days", xlab="5 mins Interval",
     col="blue", lwd=1.5)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps


```r
MaxSteps <-StepsInterval[which.max(StepsInterval$steps),]$interval
MaxSteps
```

```
## [1] 835
```

5-minute interval which contain maximum no.of interval is 835 .


## Imputing missing values

The total number of missing values in the dataset`.


```r
missingvalues <- sum(is.na(activity$steps))
missingvalues
```

```
## [1] 2304
```

Total missing values are 2304 .

Devise a strategy for filling in all of the missing values in the dataset. 
So,we will use mean to fill the missing values in dataset. Here’s the function that will return the mean value for a particular interval.


```r
ImputedStepsInterval<-function(interval){
    StepsInterval[StepsInterval$interval==interval,]$steps
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_new<-activity
for(i in 1:nrow(activity_new)){
    if(is.na(activity_new[i,]$steps)){
        activity_new[i,]$steps<-ImputedStepsInterval(activity_new[i,]$interval)
    }
}
head(activity_new)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsperday_new <- aggregate(steps ~ date, data=activity_new, sum)
hist(stepsperday_new$steps,col = "blue",main = "Histogram for total Number of steps per Day",xlab = "steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
meanperday_new<-mean(stepsperday_new$steps)
print(meanperday_new)
```

```
## [1] 10766.19
```

```r
medianperday_new<-median(stepsperday_new$steps)
print(medianperday_new)
```

```
## [1] 10766.19
```
There is not much change i.e. mean remains same but median increased by 1.19.

To see the impact we can compare both the histogram


```r
par(mfrow=c(1,2))
hist(stepsperday$steps,col = "blue",xlab = "steps per Day",main="")
hist(stepsperday_new$steps,col = "blue",xlab = "steps per Day",main="")
mtext("Histogram for total Number of steps per Day",adj=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

We can see that there is little bit increase in frequency due to imputed values.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_new$day <- weekdays(activity_new$date)
for (i in 1:nrow(activity_new)) {
    if (activity_new[i,]$day %in% c("Saturday","Sunday")) {
        activity_new[i,]$day<-"weekend"
    }
    else{
        activity_new[i,]$day<-"weekday"
    }
}
activity_new$day<-as.factor(activity_new$day)
head(activity_new)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days .


```r
steps_Day <- aggregate(activity_new$steps ~ activity_new$interval + activity_new$day, activity_new, mean)
names(steps_Day) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, steps_Day, type = "l", layout = c(1, 2), 
    xlab = "5 min Interval", ylab = "Average Number of steps",main="Time series plot of average steps across all weekday days and weekend days")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
