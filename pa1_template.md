---
title: "PA1_template"
author: "C. Jackson"
date: "November 23, 2018"
output: html_document

---



# Reproducible Results, Week 2
## C. Jackson

### Question 1 
Loading and preprocessing the Daily Activity Data  
File Input: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

```r
myactivity<-read.csv("C:/Users/wacja/Documents/Colleen work/Data science coursera/activity.csv")

myactivity$mydate<-as.Date(myactivity$date, format = "%Y-%m-%d")
```

### Question 2
Histogram of Total Daily Steps


```r
# Sum the steps by date
actday<-rowsum(myactivity$steps,myactivity$mydate, na.rm=TRUE)
actday <- cbind(rownames(actday), data.frame(actday, row.names=NULL))
colnames(actday)<-c("Date","Steps")

hist(actday$Steps,main="Total Daily Steps",xlab="Steps")
```

![plot of chunk section2](figure/section2-1.png)


### Question 3
Mean and Median of Total Daily Steps

```r
mean(actday$Steps)
```

```
## [1] 9354.23
```

```r
median(actday$Steps)
```

```
## [1] 10395
```

### Question 4
Time-Series Plot: Time Intervals Over Average Daily Steps

```r
intervals<-aggregate(myactivity$steps, by=list(myactivity$interval), FUN=mean, na.rm=TRUE)
names(intervals)<-c("Interval","Step.Mean") 

plot(intervals$Interval, y=intervals$Step.Mean, type="l", main="Avg Steps per Time Interval", xlab="Interval", ylab="Avg Steps", xaxt="n")
axis(1, at=seq(0, 2355, by=50))
```

![plot of chunk section4](figure/section4-1.png)

### Question 5
The 5-minute interval that, on average, contains the maximum number of steps is the 835 Interval which is a morning hour interval

### Question 6
Imputing Missing Values

Number of Missing Values in Data:

```r
sum(is.na(myactivity$steps))
```

```
## [1] 2304
```

Histogram of Total Daily Steps with Imputed NA values  
Imputation Strategy: Take the mean of each interval computed in Question 3, and use those values for the NA values for each time interval that is NA

```r
myact<-myactivity
# Peel off the rows with NA step values
bad<-subset(myact,is.na(myact$steps))
# peel off the rows without NA step values and save them
good<-subset(myact,!is.na(myact$steps))

# Merge the bad value dataframe with the dataframe of avg steps per Interval
myimp<-merge(bad,intervals, by.x="interval", by.y="Interval")
# Drop the original steps column in the imputed dataframe
myimp$steps<- NULL
names(myimp)[4] <- "steps"

# Combine the good and the new imputed dataframes back together
newact<-rbind(good, myimp)

# Sum the steps by date
newsum<-rowsum(newact$steps,newact$mydate, na.rm=TRUE)
newsum <- cbind(rownames(newsum), data.frame(newsum, row.names=NULL))
newsum<- as.data.frame(newsum)

newsum$mydate<-as.Date(newsum$`rownames(newsum)`, format = "%Y-%m-%d")
colnames(newsum)<-c("Date","Steps", "mydate")
```

### Question 7
Histogram of Total Daily Steps with Imputed Values

```r
hist(newsum$Steps,main="Total Daily Steps",xlab="Steps")
```

![plot of chunk section 7a](figure/section 7a-1.png)

Mean and Median of Total Daily Steps

```r
mean(newsum$Steps)
```

```
## [1] 10766.19
```

```r
median(newsum$Steps)
```

```
## [1] 10766.19
```

Answer to question: Imputing the NA values normalizes the value curve in the histogram; mean and median each are now much higher than the data containing NA's, and they appear to be the same value

### Question 8
Activity Patterns: Weekdays vs. Weekends; using Imputed Values


```r
# Add weekdays to dataframe
newact$mday<-weekdays(newact$mydate)

# Create new dataframes, one for weekend and one for weekdays
wkend<-subset(newact, mday %in% c("Saturday","Sunday"))
wkday<-subset(newact, !(mday %in% c("Saturday","Sunday")))

# Set time intervals for weekdays
dayintervals<-aggregate(wkday$steps, by=list(wkday$interval), FUN=mean, na.rm=TRUE)
names(dayintervals)<-c("Interval","Step.Mean")

# Set time intervals for weekends
endintervals<-aggregate(wkend$steps, by=list(wkend$interval), FUN=mean, na.rm=TRUE)
names(endintervals)<-c("Interval","Step.Mean")

# Plot the weekend and weekdays in one frame
par(mfrow=c(2,1))
plot(dayintervals$Interval, y=dayintervals$Step.Mean, type="l", main="Weekday Avg Steps per Time Interval", xlab="Interval", ylab="Avg Steps", xaxt="n")
axis(1, at=seq(0, 2355, by=50))

plot(endintervals$Interval, y=endintervals$Step.Mean, type="l", main="Weekend Avg Steps per Time Interval", xlab="Interval", ylab="Avg Steps", xaxt="n")
axis(1, at=seq(0, 2355, by=50))
```

![plot of chunk section8](figure/section8-1.png)


Comparison shows that the activity on the weekends is generally higher than the activity during the weekdays
