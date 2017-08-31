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

# ===============================
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day
stepsperday <- aggregate(data.proc.complete$steps, by=list(data.proc.complete$date), FUN=sum)
names(stepsperday) <- c("date", "steps")
par(bg = "grey")
hist(stepsperday$steps, breaks=7, main="", xaxt = "n", xlab = "# Of Steps", col = "cornflowerblue")
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







#data.complete <- data.raw[!is.na(data.raw$steps),]

# > md.pattern(data.raw)
#         date interval id steps     
# 15264      1        1  1     1    0
# 2304       1        1  1     0    1
#            0        0  0  2304 2304


# use the standard package for imputing values
library(mice)

# this is a change

