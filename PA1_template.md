---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
---

Aurthor: Zaw Htet Wai  
Source code repo:  
https://github.com/zawhtetwai/RepData_PeerAssessment1   
Reproducible output:  
https://github.com/zawhtetwai/RepData_PeerAssessment1/blob/master/PA1_template.html

## Loading and preprocessing the data  
The source data for this assignment can be downloaded from the course web site:  
Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]  

Below is the code to unzip and read the data as the CSV file.  
And have a look at its summary.  

```{r, echo=TRUE}
library(plyr)
library(ggplot2)
# Read the data from zip file  
activity <- read.csv(unz("activity.zip", "activity.csv"))
# Preview the summary of the data
summary(activity)
```

## What is mean total number of steps taken per day?  
Calculate the total steps are taken each day for the analysis  
1. Plot the analysis as the histogram  

```{r, echo=TRUE}
total <- ddply(activity, .(date), summarize, steps=sum(steps))
barplot(total$steps, names.arg = total$date, main ="Number of Steps Taken Each Day\n", xlab ="Date", ylab = "Steps")
```

2. Mean and median based on the total number of steps taken per day
```{r, echo=TRUE}
mean_steps <- mean(total$steps, na.rm=TRUE)
round(mean_steps, digits = 0)
median_steps <- median(total$steps, na.rm=TRUE)
round(median_steps, digits = 0)
```


## What is the average daily activity pattern?  
1. Calculate the average number of steps taken and create the time series plot of the 5-minute interval on average across all the days in the dataset  


```{r, echo=TRUE}
daily_avg = aggregate(steps ~ interval, activity, mean)
ggplot(daily_avg, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute Interval") + ylab("Steps")
```

2. Find the 5-minute interval that contains the maximum number of steps  

```{r, echo=TRUE}
daily_avg[which.max(daily_avg$steps), "interval"]
```

## Imputing missing values  
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```{r, echo=TRUE}
missing_val <- sum(is.na(activity))
print(missing_val)
```
There are total `r missing_val` missing values.  

2. Following the instructions in the assignment,  
**I propose the imputing strategy of filling these missing values with their mean of 5-minute interval on that day**.  

```{r, echo=TRUE}
impute_func <- function(x, y) {    
    if (!is.na(x)) 
        res <- c(as.numeric(x))
     else res <- as.numeric(daily_avg[daily_avg$interval == y, "steps"])
    return(res)
}
```

3. Below code create a new dataset that is equal to the original dataset but with the missing data filled in.  
And have a look at the new dataset summary.

```{r, echo=TRUE}
new_data <- activity
new_data$steps <- mapply(impute_func, new_data$steps, new_data$interval)
summary(new_data)
```

Then check to ensure there is no record with missing value (NA)  
```{r, echo=TRUE}
sum(is.na(new_data))
```

Calculate the total number of steps taken using the new dataset with missing values filled in.  
```{r, echo=TRUE}
new_total <- ddply(new_data, .(date), summarize, steps=sum(steps))
```

4. Plot a histogram for the total number of steps taken each day.  

```{r, echo=TRUE}
barplot(new_total$steps, names.arg = total$date, main ="Number of Steps Taken Each Day\n", xlab = "Date", ylab = "Steps")
```

calculate the mean and median on the total number of steps taken each day.  
```{r, echo=TRUE}
mean_steps2 <- mean(new_total$steps, na.rm=TRUE)
round(mean_steps2, digits = 0)
median_steps2 <- median(new_total$steps, na.rm=TRUE)
round(median_steps2, digits = 0)
```
- After imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean. However the new median of total steps taken per day is greater than that of the old median.
- From these observations, it seems that The impact of imputing missing values is rather low, at least when estimating the total number of steps per day.  

## Are there differences in activity patterns between weekdays and weekends?
1. Create a factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  

```{r, echo=TRUE}
day_of_week <- function(x) {    
   if (weekdays(as.Date(x)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
```

Apply the day factor to the dataset  

```{r, echo=TRUE}
new_data$day <- as.factor(sapply(new_data$date, day_of_week))
```

Calculate the average number of steps taken, averaged across all weekday days or weekend days.   

```{r, echo=TRUE}
weekday_weekend_activity <- aggregate(steps ~ interval + day, data = new_data, mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

```{r, echo=TRUE}
ggplot(weekday_weekend_activity, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute Interval") + ylab("Avarage Number of Steps")
```
