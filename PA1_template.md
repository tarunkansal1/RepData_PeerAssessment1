---
title: "Peer assignment-1 reproducible research"
author: "TARUN"
date: "26/06/2020"
output:
  html_document:
    keep_md: yes
  keep_md: true
---
### Frist we will read some data and modify dataset.  


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date , "%F")
```

### Now we make histogram for totla number of steps each day.

```r
sum <- tapply(activity$steps,activity$date,sum)
sum <- as.data.frame(sum)
sum$date <- as.Date(rownames(sum),"%F")
rownames(sum) <- 1:nrow(sum)
hist(sum$sum,xlab = "Number of steps",main = "Total steps each day",col = "red")  
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### The mean and median of number of steps each day...

```r
meansteps <- mean(sum$sum,na.rm = TRUE)
mediansteps <- median(sum$sum , na.rm = TRUE)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps
```

```
## [1] 10765
```

### Time series plot of the average number of steps taken

```r
avrg_acrossdays <- tapply(activity$steps,activity$interval,mean,na.rm = TRUE)
avrg_acrossdays <- as.data.frame(avrg_acrossdays)
avrg_acrossdays$interval <- as.numeric(rownames(avrg_acrossdays))
rownames(avrg_acrossdays) <- 1:nrow(avrg_acrossdays)
with(avrg_acrossdays,plot(interval,avrg_acrossdays,type = "l",xlab = "Interval"
                          ,ylab = "Number of steps",main = "Time series plot"
     ,col = "blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Hence The 5-minute interval that, on average, contains the maximum number of steps

```r
avrg_acrossdays[avrg_acrossdays$avrg_acrossdays == max(avrg_acrossdays$avrg_acrossdays),"interval"]
```

```
## [1] 835
```

## Code to describe and show a strategy for imputing missing data  

```r
number_of_missing_values <- sum(!complete.cases(activity))
number_of_missing_values
```

```
## [1] 2304
```

### Inputting in the missing values

```r
steps <- vector("numeric",length = nrow(activity))
for (i in 1:nrow(activity))
{
    if (is.na(activity[i,]$steps))
    {
      interval <- activity[i,"interval"]
      steps[i] <- avrg_acrossdays[avrg_acrossdays$interval == interval,"avrg_acrossdays"]
    }
  else{
    steps[i] <- activity[i,"steps"]
  }
}
```

### Now we will create a new dataset with included values

```r
new_activity <- activity
new_activity$steps <- steps
```

### Histogram of the total number of steps taken each day after missing values are imputed

```r
sum2 <- tapply(new_activity$steps,new_activity$date,sum)
sum2 <- as.data.frame(sum2)
sum2$date <- as.Date(rownames(sum2),"%F")
rownames(sum2) <- 1:nrow(sum2)
hist(sum2$sum2,xlab = "Number of steps",main = "Total steps each day",col = "blue")
hist(sum$sum,xlab = "Number of steps",main = "Total steps each day",col = "red",add = T)
legend("topright",col = c("blue","red"),legend = c("new data","old data"),pch = 19)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Mean and median after missing values are imputed

```r
meansteps2 <- mean(sum2$sum2)
mediansteps2 <- median(sum2$sum2)
meansteps2
```

```
## [1] 10766.19
```

```r
mediansteps2
```

```
## [1] 10766.19
```

### The impact of imputing missing data on the estimates of the total daily number of steps

```r
meandiff <- meansteps - meansteps2
meandiff
```

```
## [1] 0
```

```r
mediandiff <- mediansteps - mediansteps2
mediandiff
```

```
## [1] -1.188679
```

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
weekdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
new_activity$day <- as.factor(ifelse(is.element(weekdays(new_activity$date),weekdays),"weekday","weekend"))
averaged <- aggregate(steps ~ interval + day, new_activity, mean)
library(ggplot2)
g <- ggplot(aes(interval,steps, col = day),data = averaged)
g + facet_grid(day~.) + geom_path(stat = "identity",aes(col = day))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
