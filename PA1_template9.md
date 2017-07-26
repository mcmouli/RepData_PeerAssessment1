# Reproducible Research: Peer Assessment 1
Chandramouli M  


## Activity Monitor Data Assignment


```r
library(knitr, quiet=T)
library(plyr, quiet=T)  
library(ggplot2, quiet=T) 
opts_chunk$set(dev="png", fig.path="figure/")
```

## Loading and preprocessing the data




## What is mean total number of steps taken per day?

![](figure/stepsperday-1.png)<!-- -->

```
## The mean number of steps per day is 9354 and the median is 10395 (ignoring missing values
```




## What is the average daily activity pattern?

![](figure/dailyactivitypattern-1.png)<!-- -->


## Imputing missing values


```
## Number of rows with missing data: 2304
```

```
## For the raw data the mean and median per day are: 9354.23 and 10395
```

```
## For the imputed the mean and median per day are: 10766.19 and 10766.19
```

![](figure/ImputingMissing-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

![](figure/weekdayend-1.png)<!-- -->


