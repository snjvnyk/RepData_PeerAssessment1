---
title: "Reproducible Research "
author: "Anisha Pal"
date: "29 March 2019"
output: 
  html_document: 
    keep_md: yes
---



## Reproducible Research Assignment

1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


## Code for reading in the dataset and/or processing the data

```r
df <- read.csv("activity.csv")

df$date <- as.POSIXct(df$date, "%Y-%m-%d")
wday <- weekdays(df$date)
df <- cbind(df,wday)

summary(df)
```

```
##      steps             date               interval             wday     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Friday   :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Monday   :2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Saturday :2304  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Sunday   :2304  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thursday :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Tuesday  :2592  
##  NA's   :2304                                           Wednesday:2592
```
## Histogram steps

```r
steps <- with(df, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(steps) <- c("date", "steps")
hist(steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue")
```
![steps](instructions_fig/plot1.png) <!-- -->

```r
dev.copy (png,filename="plot1.png")
```

```
## png 
##   3
```

```r
dev.off ()
```

```
## png 
##   2
```

## Mean and Median of steps taken

```r
mean(steps$steps)
```

```
## [1] 9354.23
```

```r
median(steps$steps)
```

```
## [1] 10395
```

## average number of steps taken

```r
average <- aggregate(df$steps, by=list(df$interval), FUN=mean, na.rm=TRUE)
names(average) <- c("interval", "mean")
plot(average$interval, average$mean, type = "l", xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![average number of steps taken](instructions_fig/plot2.png) <!-- -->

```r
dev.copy (png,filename="plot2.png")
```

```
## png 
##   3
```

```r
dev.off ()
```

```
## png 
##   2
```

## 5-minute interval that, on average, contains the maximum number of steps

```r
average[which.max(average$mean), ]$interval
```

```
## [1] 835
```

## Code to describe and show a strategy for imputing missing data and Histogram of the total number of steps taken each day after missing values are imputed

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
imputed_steps <- average$mean[match(df$interval, average$interval)]
df_imputed <- transform(df, steps = ifelse(is.na(df$steps), yes = imputed_steps, no = df$steps))
total_steps_imputed <- aggregate(steps ~ date, df_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
hist(total_steps_imputed$daily_steps, xlab = "Total steps per day", main = "Total number of steps taken each day")
```

![total number of steps taken each day after missing values are imputed](instructions_fig/plot3.png) <!-- -->

```r
dev.copy (png,filename="plot3.png")
```

```
## png 
##   3
```

```r
dev.off ()
```

```
## png 
##   2
```

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
library(ggplot2)
df$date <- as.Date(strptime(df$date, format="%Y-%m-%d"))
df$datetype <- sapply(df$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
df_by_date <- aggregate(steps~interval + datetype, df, mean, na.rm = TRUE)
plot<- ggplot(df_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![average number of steps taken per 5-minute interval across weekdays and weekends](instructions_fig/plot4.png) <!-- -->

```r
dev.copy (png,filename="plot4.png")
```

```
## png 
##   3
```

```r
dev.off ()
```

```
## png 
##   2
```

