# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip(zipfile="/Users/yanze/github/RepData_PeerAssessment1/activity.zip")
activity_dat <- read.csv("/Users/yanze/github/RepData_PeerAssessment1/activity.csv")
#print(activity_dat)
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
total_steps <- tapply(activity_dat$steps, activity_dat$date, FUN = sum, na.rm = TRUE)
qplot(total_steps, binwidth = 1000, xlab = "total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_daily_steps <- mean(total_steps, na.rm = TRUE)
print(mean_daily_steps)
```

```
## [1] 9354.23
```

## What is the average daily activity pattern?


```r
library(ggplot2)
average_steps <- aggregate(x = list(steps = activity_dat$steps), by = list(interval =  activity_dat$interval), 
    FUN = mean, na.rm = TRUE)
ggplot(data = average_steps, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
average_max <- average_steps[which.max(average_steps$steps), ]
print(paste("maximum steps for a 5-minite internal: ", average_max$steps))
```

```
## [1] "maximum steps for a 5-minite internal:  206.169811320755"
```

```r
average_min <-average_steps[which.min(average_steps$steps), ]
print(paste("minimum steps for a 5-minite internal: ", average_min$steps))
```

```
## [1] "minimum steps for a 5-minite internal:  0"
```
## Inputing missing values

```r
#coded as NA in the input data.
missing <- is.na(activity_dat$steps)
# Show missing table
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
#filled the mean 5-minute interval value for all the missing values.
fill_value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) {
        filled <- c(steps) 
    } else {
        filled <- (average_steps[average_steps$interval == interval, "steps"])
    }
    return(filled)
}
filled_data <- activity_dat
filled_data$steps <- mapply(fill_value, activity_dat$steps, activity_dat$interval)

#histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
total_steps <- tapply(filled_data$steps, filled_data$date, FUN = sum)
qplot(total_steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_steps <- mean(total_steps)
print(paste("mean steps: ", mean_steps))
```

```
## [1] "mean steps:  10766.1886792453"
```

```r
median_steps <- median(total_steps)
print(paste("median steps: ", median_steps))
```

```
## [1] "median steps:  10766.1886792453"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#function to detect weekdays and weekend
detect_weekdays <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
        return("weekday") 
    } else if (day %in% c("Saturday", "Sunday")) {
        return("weekend") 
    } else {
      stop("invalid")
    }
}
filled_data$date <- as.Date(filled_data$date)
filled_data$day <- sapply(filled_data$date, FUN = detect_weekdays)

average_steps <- aggregate(steps ~ interval + day, data = filled_data, mean)
ggplot(average_steps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
