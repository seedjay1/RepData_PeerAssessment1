# script to perform the PA actions

# download data if it isn't already in place in current directory
filename <- "activity.csv"
zipurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if (!file.exists(filename)) {
  download <- download.file(zipurl, destfile = "temp.zip")
  unzip("temp.zip")
  unlink("temp.zip")
}

data.raw <- read.csv(filename, header=TRUE)

data.proc <- data.raw
data.proc$id <- seq(1:nrow(data.proc))
data.proc$date <- as.Date(data.proc$date)
data.proc$dow <- weekdays(data.proc$date, abbreviate=TRUE)
data.proc.complete <- data.proc[!is.na(data.proc$steps),]

# =================================================
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day
# =================================================
stepsperday <- aggregate(data.proc.complete$steps, by=list(data.proc.complete$date), FUN=sum)
names(stepsperday) <- c("date", "steps")
par(bg = "grey")
hist(stepsperday$steps, breaks=10, main="", xaxt = "n", xlab = "# Of Steps", col = "cornflowerblue")
axis(1, at=seq(0, 25000, 5000), cex.axis =.75)
title(main="Histogram Of Total Steps Per Day")
mtext(bquote(mu 
             ~ "=" 
             ~ .(format(mean(stepsperday$steps), big.mark=","))
             ~ ", " 
             ~ sigma 
             ~ "=" 
             ~ .(format(sd(stepsperday$steps), big.mark = ","))
             ~ ", median ="
             ~ .(format(median(stepsperday$steps), big.mark = ","))
             )
      )
box()


# ==========================================
# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
# ==========================================
library(TTR) # for exponentially weighted moving average

avgstepsperinterval <- aggregate(data.proc.complete$steps, by=list(data.proc.complete$interval), FUN=mean)
names(avgstepsperinterval) <- c("interval", "avg_steps")
avgstepsperinterval$expMA <- EMA(avgstepsperinterval$avg_steps)

themean <- mean(avgstepsperinterval$avg_steps)
themax <- max(avgstepsperinterval$avg_steps)
themaxpre <- avgstepsperinterval$interval[avgstepsperinterval$avg_steps == themax]
thesd <- sd(avgstepsperinterval$avg_steps)
plot.new()
plot(avgstepsperinterval$interval, avgstepsperinterval$avg_steps, type="l", col="blue", xaxt = "n", xlab="Interval", ylab="Avg. Daily Steps")
axis(1, at=seq(0, 3500, 100), cex.axis =.75)
points(x=themaxpre, y=themax, pch=19, col="forestgreen")
text(x=1.05 * themaxpre
     , y=.99 * themax
     , labels=bquote("maximum: (interval, avg steps) = (" 
                     ~ .(themaxpre)
                     ~ ", " 
                     ~ .(round(x=themax, digits=2))
                     ~ ")" 
     )
     , adj = c(0,0)
     , cex=.75)
title(main="Average Steps Per Day, By Time Interval")

# add exponential moving average line for some smoothing visualization
lines(x=avgstepsperinterval$interval, y=avgstepsperinterval$expMA, type="l", col="brown1")
rect(xleft=0, xright=750, ybottom=160, ytop=205)
legend(1, 210, legend=c("Data", "Exponential\nRunning Average"), col=c("blue", "brown1"), lty=c(1, 1), cex=0.8, bty="n")



#data.complete <- data.raw[!is.na(data.raw$steps),]

# > md.pattern(data.raw)
#         date interval id steps     
# 15264      1        1  1     1    0
# 2304       1        1  1     0    1
#            0        0  0  2304 2304


# use the standard package for imputing values
library(mice)



