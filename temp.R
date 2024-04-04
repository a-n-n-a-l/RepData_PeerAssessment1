library(dplyr)
#library(lubridate)

if (!exists("activity.zip")) {unzip("activity.zip")}    #if the file hasn't been unzipped before, unzip it
activity <- read.csv("activity.csv", colClasses = c(steps="numeric", date="Date", interval="numeric"))

# meanSteps <- activity %>% group_by(date) %>% filter(!is.na(steps)) %>% summarize(mean=mean(steps))
# meanSteps2 <- activity %>% group_by(date) %>% summarize(mean=mean(steps))

totalSteps <- activity %>% group_by(date) %>% filter(!is.na(steps)) %>% summarize(total=sum(steps))
meanSteps <- format(mean(totalSteps$total),  big.mark = ",", scientific=FALSE)
medianSteps <- format(median(totalSteps$total),  big.mark = ",", scientific=FALSE)


hist(totalSteps$total, breaks = 16, main = "Frequency distribution of total steps per day", xlab = "Total steps")
boxplot(totalSteps$total, col="navajowhite", main="Total steps")
mtext(paste("The median is", medianSteps, "and the mean is", meanSteps), side = 1, line = 1)



avgIntervals <- activity %>% group_by(interval) %>% filter(!is.na(steps)) %>% summarise(steps=mean(steps))
maxSteps <- format((avgIntervals$steps[which.max(avgIntervals$steps)]), digits=4)
maxStepsWhen <- (avgIntervals$interval[which.max(avgIntervals$steps)])
formattedLabels <- c("12:00 AM", "1:42 AM", "3:24 AM", "5:06 AM", "6:48 AM", "8:30 AM", "10:12 AM", 
                     "11:54 AM", "1:36 PM", "3:18 PM", "5:00 PM", "6:42 PM", "8:24 PM", "10:06 PM")
plot(x = avgIntervals$interval, y = avgIntervals$steps, type = "l", main="Average daily activity from 12:00 AM to 11:59 PM", 
     xlab="", ylab = "Number of steps", xaxt="n")
axis(1, at=seq(0, 2400, by=180), labels = formattedLabels)

print(paste("The interval with the averaged maximum number of steps (",maxSteps,") is", maxStepsWhen, 
            "which corresponds to 8:35-8:40 AM"), side = 1, line = 4)


NAs <- which(rowSums(is.na(activity)) > 0)
print(paste("The total number of rows with missing values is", format(length(NAs), big.mark=",")))
onlyNAs <- activity[NAs,]
print(length(unique(onlyNAs$date)))
print(paste("Since each day has 288 5-minute intervals, we can figure out how many intervals are ....", length(unique(onlyNAs$date))*288))

activityNoNAs <- activity

for (i in NAs){
        interval <- activity$interval[i] 
        stepRow <- match(interval, avgIntervals$interval)
        activityNoNAs$steps[i] <- avgIntervals$steps[stepRow]
}

totalStepsNN <- activityNoNAs %>% group_by(date) %>% filter(!is.na(steps)) %>% summarize(total=sum(steps))
meanStepsNN <- format(mean(totalStepsNN$total),  big.mark = ",", scientific=FALSE)
medianStepsNN <- format(median(totalStepsNN$total),  big.mark = ",", scientific=FALSE)


hist(totalStepsNN$total, breaks = 16, main = "Frequency distribution of total steps per day", xlab = "Total steps")
boxplot(totalStepsNN$total, col="navajowhite", main="Total steps")
mtext(paste("The median with no missing values is", medianStepsNN, "was", medianSteps,"."), side = 1, line = 1)
mtext(paste("The mean with no missing values is", meanStepsNN, "was", meanSteps), side = 1, line = 2)


##############################################
#       Weekdays vs. weekends                #
##############################################

activity <- activity %>% mutate(dayOfWeek=weekdays(activity$date))
for (i in 1:nrow(activity)) {
        if (activity$dayOfWeek[i] %in% c("Saturday", "Sunday")) {
            activity$dayOfWeek[i] <- "weekend"
        }
        else {
            activity$dayOfWeek[i] <- "weekday"
        }
}
activity$dayOfWeek <- as.factor(activity$dayOfWeek)
avgWeekdayIntervals <- activity %>% 
        filter(dayOfWeek == "weekday") %>% 
        group_by(interval) %>% 
        filter(!is.na(steps)) %>% 
        summarise(steps=mean(steps))
avgWeekdaySteps <- mean(avgWeekdayIntervals$steps)
avgWeekendIntervals <- activity %>% 
        filter(dayOfWeek == "weekend") %>% 
        group_by(interval) %>% 
        filter(!is.na(steps)) %>% 
        summarise(steps=mean(steps))
avgWeekendSteps <- mean(avgWeekendIntervals$steps)


# set up parameters to create 2 graphs in 1 window and have margins large enough to see labels
par(mfrow=c(2,1), oma=c(1,1,1,1), mar=c(2,4,2,2))
plot(avgWeekdayIntervals$interval, avgWeekdayIntervals$steps,
     type="l",
        main="Weekday steps by interval (average)", cex=1,
     xlab="", ylab = "Number of steps", xaxt="n")
axis(1, at=seq(0, 2400, by=180), labels = formattedLabels)
abline(h=avgWeekdaySteps)
plot(avgWeekendIntervals$interval, avgWeekendIntervals$steps,
     type="l",
     main="Weekend steps by interval (average)", cex=1,
     xlab="", ylab = "Number of steps", xaxt="n")
axis(1, at=seq(0, 2400, by=180), labels = formattedLabels)
abline(h=avgWeekendSteps)
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(5,4,4,2))



