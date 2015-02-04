Reproducible Research - Peer Assessment 1
========================================================
### Loading and preprocessing the data
1. Download the data, and unzip it and load it into memory
2. convert date from character to Date


```r
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip")
unzip("activity.zip")

activity <- read.csv("activity.csv", as.is = T)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```


### What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate the mean and median of the total number of steps taken per day.


```r
total <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(total$steps, main = "Total steps per day", xlab = "Number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
mediansteps <- median(total$steps)
meansteps <- mean(total$steps)
```
So the mean and median of the total number of steps taken per day are **1.0766189 &times; 10<sup>4</sup>** and **10765** respectively.

### What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Find the 5-minute interval that contains the maximum number of steps?


```r
avg <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(avg$steps~avg$interval, type = "l", xlab = "interval", ylab = "Number of steps", main = "Average number of steps across all days")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
ind <- avg$interval[which.max(avg$steps)]
```
Thus the **835** interval contains the maximum number of steps.

### Imputing missing values
1. Calculate the total number of missing values in the dataset.
2. Use the mean for the 5-minute interval to imput the missing values.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate the mean and median total number of steps taken per day. 


```r
nmiss = sum(is.na(activity$steps))
imputed_steps = ifelse(is.na(activity$steps), avg$steps[match(activity$interval, avg$interval)], activity$steps)
imputed_data <- transform(activity, steps = imputed_steps)
total <- aggregate(steps ~ date, data = imputed_data, sum, na.rm = TRUE)
hist(total$steps, main = "Total steps per day", xlab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
median(total$steps)
```

```
## [1] 10766.19
```

```r
mean(total$steps)
```

```
## [1] 10766.19
```

The total number of missing values in the dataset is **2304**.
The mean is the same as the the estimates from the first part of the assignment, i.e., no impact of imputation on mean since we use the mean to impute missing values.
However, the median is increased a little bit, and now it's the same as the mean.

### Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
MonToFri <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekday <- as.factor(ifelse(is.element(weekdays(imputed_data$date), MonToFri), "weekday", "weekend"))
imputed_data <- cbind(imputed_data, weekday)

avg <- aggregate(steps ~ interval + weekday, data = imputed_data, mean, na.rm = TRUE)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```

```r
xyplot(steps~interval|weekday, data=avg, type = "l", layout=c(1,2),
       ylab="Number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
