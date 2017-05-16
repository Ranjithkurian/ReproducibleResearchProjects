Reproducible Research: Peer Assessment 1

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset:[Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

## Assignment 
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

Loading and preprocessing the data

Download, unzip and load data into data frame
```{r}
if(!file.exists("data")) {dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filepath <- file.path(getwd(),"/data/activity.zip")
download.file(fileUrl,filepath)

unzip(zipfile="data/activity.zip",exdir = "./data")

data <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

Sum steps by day, create Histogram, and calculate mean and median.
```{r}
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```
The mean is r rmean and the median is r rmedian

if image is not displayed then click on the link

[Plot1_Total Steps Each Day](/Project_1/Plot/plot1.png?raw=true "Optional Title")

The mean is 10766.19 and median is 10765

## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` {r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

if image is not displayed then click on the link

[Plot2_Average Number of Steps per Day by Interval](/Project_1/Plot/plot2.png?raw=true "Optional Title")

``` {r}
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
missing_vals <- sum(is.na(data$steps))
```

The total number of missing values are 2304.

* Strategy for filling in all of the missing values in the dataset

To populate missing values, we choose to replace them with the mean value at the same interval across days.
for example if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA.

```{r}
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```
* Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
sum(is.na(imputed_data$steps))
```
Zero output shows that there are NO MISSING VALUES.

* histogram of the total number of steps taken each day
```{r}
fill_steps_per_day <- aggregate(steps ~ date, imputed_data, sum)
hist(fill_steps_per_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```
if image is not displayed then click on the link

[Plot3_Total Steps Each Day](/Project_1/Plot/plot3.png?raw=true "Optional Title")

* Calculate new mean and median for imputed data.
```{r}
fill_rmean <- mean(fill_steps_per_day$steps)
fill_rmedian <- median(fill_steps_per_day$steps)
```

The mean is 10766.19 and median is 10766.19.

* Do these values differ from the estimates from the first part of the assignment?

Yes, these values do differ slightly.

Before filling the data

Mean : 10766.19
Median: 10765
After filling the data

Mean : 10766.19
Median: 10766.19
We see that the values after filling the data mean and median are equal.

* What is the impact of imputing missing data on the estimates of the total daily number of steps?

As you can see, comparing with the calculations done in the first section of this document, we observe that while the mean value remains unchanged, the median value has shifted and virtual matches to the mean.

Since our data has shown a t-student distribution (see both histograms), it seems that the impact of imputing missing values has increase our peak, but it's not affect negatively our predictions.

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` {r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
if image is not displayed then click on the link

[Plot4_Total Steps Each Day](/Project_1/Plot/plot4.png?raw=true "Optional Title")
