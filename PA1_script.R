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
data.raw$id <- seq(1:nrow(data.raw))

data.complete <- data.raw[!is.na(data.raw$steps),]

# > md.pattern(data.raw)
#         date interval id steps     
# 15264      1        1  1     1    0
# 2304       1        1  1     0    1
#            0        0  0  2304 2304


# use the standard package for imputing values
library(mice)

# this is a change

