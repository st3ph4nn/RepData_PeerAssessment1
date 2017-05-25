library(dplyr)
library(dtplyr)
library(ggplot2)
library(lubridate)

# download and unzipping file
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "~/RepdataActivity.zip", mode = "wb", quiet = TRUE)
unzip("./RepdataActivity.zip", exdir = "~")
file.remove("~/RepdataActivity.zip")

# loading data
activitydata <- read.csv("~/activity.csv", header = TRUE)

# calculates the total number of steps each day
stepsday <- select(activitydata, steps, date) %>% 
        group_by(date) %>% 
        summarise(sumsteps = sum(steps))

# make histogram and create the mean and median.
png(filename = "1) histogram total steps per day.png")
hist(stepsday$sumsteps, 
     main = "Total steps per day", 
     xlab = "Total steps")
dev.off()
mean(stepsday$sumsteps, na.rm = TRUE)
median(stepsday$sumsteps, na.rm = TRUE)


# average number of steps taken over the 5 min interval of all days
avesteps <- select(activitydata, interval, steps) %>% 
        group_by(interval) %>% 
        summarise(meansteps = mean(steps, na.rm=TRUE))

# activity graph
png(filename = "2) activity graph.png")
g0 <- ggplot(avesteps, aes(interval, meansteps)) + 
        geom_line() + 
        labs(x = "interval", y = "Average steps", title = "Average steps taken per 5 minute interval")
g0
dev.off()

# returns which interval has the highest step
avesteps[which.max(avesteps$meansteps),]

# total number of rows that has NA values
length(which(is.na(activitydata$steps)))

# filling all NA values with the average steps over the 5 min interval of all days
imputedata <- activitydata
imputedata[is.na(imputedata$steps),]$steps <- avesteps$meansteps

# calculate the total of steps per day with the complete dataset
imputestepsday <- select(imputedata, steps, date) %>% 
                group_by(date) %>% 
                summarise(sumsteps = sum(steps))

# create 2 histogram next to each other. One with NA values and one with the impute data
png(filename = "3) histogram with NA values and without.png")
par(mfrow = c(1, 2))
hist(stepsday$sumsteps, 
     main = "Total steps per day with NA", 
     xlab = "Total steps",
     ylim = c(0, 35),
     breaks = 5)

hist(imputestepsday$sumsteps,
     main = "Total steps per day without NA",
     xlab = "Total steps")
dev.off()

## Compare mean with NA and without NA:
mean(stepsday$sumsteps, na.rm = TRUE)
mean(imputestepsday$sumsteps, na.rm = TRUE)

## Compare median with NA and without NA:
median(stepsday$sumsteps, na.rm = TRUE)
median(imputestepsday$sumsteps, na.rm = TRUE)


#using lubridate package to create new column and create a vector with the weekdays
imputedata$days <- wday(ymd(imputedata$date), label = TRUE, abbr = FALSE)
weekdays0 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# create factor column to see which date is a weekday and which is the weekend
imputedata$weekdays <- factor((imputedata$days %in% weekdays0), 
                              levels = c(TRUE,FALSE),
                              labels = c("Weekdays", "Weekend"))

# calculate the average number of steps during weekdays and weekends over a 5 min interval
imputedata1 <- select(imputedata, steps, interval, weekdays) %>%
        group_by(interval, weekdays) %>%
        summarise(averagestepsweekdays = mean(steps))

# create an activity graph during weekdays and weekends
png(filename = "4) activity graph during weekdays and weekends.png")
g1 <- ggplot(imputedata1, aes(interval, averagestepsweekdays)) +
        geom_line() +
        facet_grid(weekdays~.) +
        labs(y = "Average steps", title = "Average steps per day for 5 min interval")
g1
dev.off()
