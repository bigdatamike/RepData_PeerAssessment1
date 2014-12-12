---
title: "Activity Monitoring Device Analysis"
output: html_document
---

###Reading the activity monitoring data into R.
```{r}
repdata <- read.csv("activity.csv", header=T)
dim(repdata)
head(repdata)
summary(repdata)

```
###Data transformation
The following code creates and transforms the data by removing NA from the data, and creating a dataframe that consists of the sum of the steps taken for each day. 

```{r}
stepdata <- repdata[complete.cases(repdata),]## this removes NA from the data
stepdata <- aggregate(steps ~ date, data = stepdata, FUN=sum)## calculating the number of steps taken for each day
```
###Mean total number of steps taken per day.
The histogram below shows the total number of steps taken each day. 
```{r fig.hieght = 4}
hist(stepdata$steps, ylab = "Frequency", xlab = "Total # of Steps Taken each Day", main = "Total Number of Steps Taken each Day", col = "green")
```


The mean and median  total number of steps taken per day is:

- Mean
```{r, echo=FALSE}
mean(stepdata$steps)
```
- Median
```{r, echo=FALSE}
median(stepdata$steps)
```

###The average daily activity pattern 
The time series graph below shows the average number of steps saken across all days(avg) by 5-minute Interval.
```{r}
intervaldata <- repdata[complete.cases(repdata),]## this removes NA from the data
intervaldata <- aggregate(steps ~ interval, data = intervaldata, FUN=mean)##  the number of steps taken at each interval
```

```{r, echo=FALSE}
plot(intervaldata, type = "l", lwd = 2, col = "blue",xlab = "5 Minute Interval",ylab = "Average # of Steps Taken Across all Days(avg)",main = "Daily Actitivy Pattern")
```


The 5-minute interval which contains the maximum number of steps, on average across all the days is:
```{r}
intervaldata <- intervaldata[order(-intervaldata$step),] 
intervaldata[1,1] 
```
###Imputing missing values into dataset

Since there is missing data in the dataset, I will use the mean(grouped by interval) of the number of steps taken to replace the NA's in the "steps" column of the dataset. The histogram below shows the total number of steps taken each day, using the transformed dataset. The means/medians of the dataset, before and after the replacement of the NA's are shown below, showing that the data sets are essentially equal. 

The number of NA's in the dataset before replacing the NA's with the mean.
```{r}
sum(is.na(repdata))##determines how many NA's are in dataset
```

```{r}
library(plyr)
repdata2 <- repdata
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
repdata2 <- ddply(repdata2, ~ interval, transform, steps = impute.mean(steps))
```


Histogram of the total number of steps taken each day
```{r fig.hieght = 4}
stepdata2 <- aggregate(steps ~ date, data = repdata2, FUN=sum)## calculating the number of steps taken for each day
hist(stepdata2$steps, col = "red", ylab = "Frequency", xlab = "Total # of Steps Taken each Day", main = "Total Number of Steps Taken each Day")
```

Showing that the NA's have been removed from the dataset.
```{r}
sum(is.na(repdata2))##determines how many NA's are in dataset
```




The mean and median  total number of steps taken per day is:

- Mean - the mean from before and after the removal of the NA's are  the same.
```{r}
mean(stepdata$steps)## this is before the NA's were removed from the data
mean(stepdata2$steps)## this is after the NA's were removed from the data
```
- Median - the median from before and after the removal of the NA's are slightly differnt.
```{r}
median(stepdata$steps)## this is before the NA's were removed from the data
median(stepdata2$steps)## this is after the NA's were removed from the data
```

###Are there differences in activity patterns between Weekdays and Weekends? 
I will use two time series plots, labeled Weekend and Weekday to look at the differences between activity patterns.

```{r}
intervaldata2 <- repdata2 ## this is the imputed mean dataset.
intervaldata2$pdate <- as.POSIXlt(intervaldata2$date,format="%Y-%m-%d")##1=mon,2,3w,4th,5f,6s,0=sun
intervaldata2[,4] <- intervaldata2[,4]$wday## numbers for converting dates to weekend/weekday
intervaldata2[,4]<-ifelse(intervaldata2[,4]<1 | intervaldata2[,4]>5,"Weekend", "Weekday")
intervaldata2 <- aggregate(steps ~ interval+pdate, intervaldata2, mean)
```

```{r}

library(lattice)
xyplot(steps ~ interval | pdate, data = intervaldata2, grid = T, type = "l", col.line = "lightblue", lwd = 2, main = "Activity Steps")
```

