# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Loading the data form the csv file, then transform the date column to standard time format in R

```r
data<-read.csv("activity.csv",header=TRUE)
data$date<-as.Date(as.character(data$date),"%Y-%m-%d")
```


## What is mean total number of steps taken per day?

Caculate the total number per day, draw the histgram and calulate its mean and median


```r
totalstep<-sapply(split(data$steps,data$date),sum,na.rm=TRUE)
hist(totalstep,15)
```

![](PA1_template_files/figure-html/totalsteps-1.png) 

```r
mean(totalstep,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalstep, na.rm=TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

Calculate the mean of each interval acorss all days and draw a time series plot then find the interval have largest mean steps


```r
factor<-as.factor(data$interval)
intervalmean<-sapply(split(data$steps,factor),mean,na.rm=TRUE)
plot(names(intervalmean),intervalmean,type="l", xlab="time in one day (min)",ylab="mean steps")
```

![](PA1_template_files/figure-html/pattern-1.png) 

```r
names(which.max(intervalmean))
```

```
## [1] "835"
```
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
