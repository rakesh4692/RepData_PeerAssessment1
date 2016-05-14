### step-1

Loading and preprocessing data

    library(knitr)
    library(markdown)
    activity<-read.csv("./activity.csv",colClasses = c("integer","Date","integer"))

Loading the data with removed NAs for calculation

    remove_na_activity<-activity[complete.cases(activity),]

### step-2

1.calculating number of steps taken per day

    library(plyr)
    sum_steps<-ddply(remove_na_activity,.(date),summarise,steps_total=sum(steps))
    head(sum_steps)

    ##         date steps_total
    ## 1 2012-10-02         126
    ## 2 2012-10-03       11352
    ## 3 2012-10-04       12116
    ## 4 2012-10-05       13294
    ## 5 2012-10-06       15420
    ## 6 2012-10-07       11015

1.  making Histogram of no of steps taken each day

<!-- -->

    barplot(sum_steps$steps_total,xlab = "Date wise no. of steps", ylab = "Frequency",col = "wheat",main = "Histogram of no of steps taken each day",ylim = c(0,25000))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)
3.calculating mean and median of total number of steps taken per Day

-   mean of total no. of steps taken per day

<!-- -->

    mean(sum_steps$steps_total)

    ## [1] 10766.19

-   median of total no of steps taken per day

<!-- -->

    median(sum_steps$steps_total)

    ## [1] 10765

### step 3

1.Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    mean_steps<-ddply(remove_na_activity,.(interval),summarise,steps_mean=mean(steps))
    head(mean_steps)

    ##   interval steps_mean
    ## 1        0  1.7169811
    ## 2        5  0.3396226
    ## 3       10  0.1320755
    ## 4       15  0.1509434
    ## 5       20  0.0754717
    ## 6       25  2.0943396

    plot(mean_steps$interval,mean_steps$steps_mean,xlab = "interval",ylab = "steps",type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

2.Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps.

    library(plyr)
    max_step_interval<-ddply(remove_na_activity,.(date),summarise,max_interval=max(steps))
    head(max_step_interval)

    ##         date max_interval
    ## 1 2012-10-02          117
    ## 2 2012-10-03          613
    ## 3 2012-10-04          547
    ## 4 2012-10-05          555
    ## 5 2012-10-06          526
    ## 6 2012-10-07          523

-   maximum step

<!-- -->

    max(max_step_interval$max_interval)

    ## [1] 806

-   corresponding interval

<!-- -->

    remove_na_activity$interval[grep("806",remove_na_activity$steps)]

    ## [1] 615

### step-4

1.Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

    y<-sum(is.na(activity))
    y

    ## [1] 2304

Total no. of mission values in dataset is 2304

2.Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc. 3. Create a new dataset that is equal to the
original dataset but with the missing data filled in.

    ### here i am replacing NAs to zero as it is not necessary to use mean/median etc
    ## its pretty straight forward
    new_activity <-activity 
    new_activity$steps[which(is.na(new_activity$steps))]<-0
    head(new_activity)

    ##   steps       date interval
    ## 1     0 2012-10-01        0
    ## 2     0 2012-10-01        5
    ## 3     0 2012-10-01       10
    ## 4     0 2012-10-01       15
    ## 5     0 2012-10-01       20
    ## 6     0 2012-10-01       25

4.Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

    sum_steps_new<-ddply(new_activity,.(date),summarise,steps_total=sum(steps))
    barplot(sum_steps_new$steps_total,xlab = "Date wise no. of steps with NAs Replaced", ylab = "Frequency",col = "wheat",main = "Histogram of no of steps taken each day",ylim = c(0,25000))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

*mean and median are going to change because i use zero instead of mean
while replacing the NAs value . If i use mean than it won't change.*

-   mean of total no. of steps taken per day

<!-- -->

    mean(sum_steps_new$steps_total)

    ## [1] 9354.23

-   median of total no of steps taken per day

<!-- -->

    median(sum_steps_new$steps_total)

    ## [1] 10395

### step-5

1.Create a new factor variable in the dataset with two levels weekday
and weekend indicating whether a given date is a weekday or weekend day.

    day_type <- function(dates) {
      week_factor <- function(date) {
        if (weekdays(date) %in% c("Saturday", "Sunday")) {
          "weekend"
        }
        else {
          "weekday"
        }
      }
      sapply(dates, week_factor)
    }

    new_activity$day_type <- as.factor(day_type(new_activity$date))
    str(new_activity)

    ## 'data.frame':    17568 obs. of  4 variables:
    ##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ day_type: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...

    head(new_activity)

    ##   steps       date interval day_type
    ## 1     0 2012-10-01        0  weekday
    ## 2     0 2012-10-01        5  weekday
    ## 3     0 2012-10-01       10  weekday
    ## 4     0 2012-10-01       15  weekday
    ## 5     0 2012-10-01       20  weekday
    ## 6     0 2012-10-01       25  weekday

2.Make a panel plot containing a time series plot (i.e. type = "l") of
the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis). See the
README file in the GitHub repository to see an example of what this plot
should look like using simulated data.

    mean_steps_new<-ddply(new_activity,.(day_type,interval),summarise,steps_mean=mean(steps))
    head(mean_steps_new)

    ##   day_type interval steps_mean
    ## 1  weekday        0 2.02222222
    ## 2  weekday        5 0.40000000
    ## 3  weekday       10 0.15555556
    ## 4  weekday       15 0.17777778
    ## 5  weekday       20 0.08888889
    ## 6  weekday       25 1.31111111

    library(lattice)
    xyplot(steps_mean~interval | day_type ,data = mean_steps_new,layout(1,2),type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-17-1.png)
