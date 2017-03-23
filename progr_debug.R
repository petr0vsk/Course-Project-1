##clear memory
rm(list=ls())
library(tidyr)
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
# ----- 1. Loading and preprocessing the data -----------------------------------------------
project.dir <- "/home/petr0vsk/Project3"
stopifnot( dir.exists(file.path(project.dir))  )
setwd(file.path(project.dir))
#steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
#date: The date on which the measurement was taken in YYYY-MM-DD format
#interval: Identifier for the 5-minute interval in which measurement was taken
steps.raw <- read.csv("activity.csv",  header = TRUE) 
str(steps.raw)
# ================================ 2. What is mean total number of steps taken per day? =============
# ---- What is mean total number of steps taken per day? ----
sum.steps.per.day <- steps.raw %>%
    group_by(date) %>%
    summarise_each(funs( sum(steps, na.rm = T) ), steps = steps) %>%
    as.data.frame()
# ---- Histogram of the total number of steps taken each day -------------
plot.new()
hist(sum.steps.per.day$steps, main = "Total number of steps taken each day with NA", 
     breaks = 61,
     xlab = "Steps per day", ylab = "Frequency",
     col="black",
     border="white")
box(bty="l")
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
rug(sum.steps.per.day$steps)
# ------ Calculate and report the mean and median total number of steps taken per day ---
mn.NA <- round(mean(sum.steps.per.day$steps, na.rm = T),2)
md.NA <- median(sum.steps.per.day$steps, na.rm = TRUE)
print(paste0("mean.with.NA = ", mn.NA))
print(paste0("median.with.NA = ", md.NA))

# ==3. What is the average daily activity pattern? ==========================
daily.activity.average.with.NA <- steps.raw %>%
    group_by(interval) %>%
    summarise_each(funs(mean(steps, na.rm = TRUE)), steps = steps)  
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max.steps <- daily.activity.average.with.NA[which.max(daily.activity.average.with.NA$steps),]
# vector for x-axes labels 
brake.vec <- as.vector(seq(1,288,by=12))
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
plot.new()
ggplot(daily.activity.average.with.NA, aes(x=interval, y=steps)) +
    geom_line(colour = "darkblue") +                                                                                                          # breaks  = 2335
    geom_point(size=0.7) +
    scale_x_continuous(name = "Time interval (by 5-minyt step)", limits = c(0,tail(daily.activity.average.with.NA$interval, n=1)), breaks = daily.activity.average.with.NA$interval[brake.vec])  +
    scale_y_continuous(name = "Time series plot of the average number of steps taken", limits = c(0,max(daily.activity.average.with.NA$steps))) +
    ggtitle("Mean total number of steps taken per day") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
    stat_smooth(colour="green", method = 'loess', na.rm=TRUE) 
# 5-minute interval contains the maximum number of steps   
print(max.steps$interval)
# ==== 4. Imputing missing values  =====================
# --All of the missing values are replased with mean value for that 5-minute interval -----
# Calculate and report the total number of missing values in the dataset
sum(is.na(steps.raw$steps)) 
# Create a new dataset that is equal to the original dataset but with the missing data filled in
# replase NA with median of interval
steps.raw.clear <- steps.raw
# replase value ----
for(i in 1:length(steps.raw.clear$steps))   {
    if( is.na(steps.raw.clear$steps[i]) )        {
        steps.raw.clear$steps[i]<- filter(daily.activity.average.with.NA, interval==steps.raw.clear$interval[i])%>%
                  select(steps) %>%
                  round(2)
    }#if..
}#for..
steps.raw.clear$steps <- unlist(steps.raw.clear$steps)
# ==== 5. Are there differences in activity patterns between weekdays and weekends? ======
# ---- What is mean total number of steps taken per day? ----
sum.steps.per.day.clear <- steps.raw.clear %>%
    group_by(date) %>%
    summarise_each(funs( sum(steps) ), steps = steps) 
    
mn <- round(mean(sum.steps.per.day.clear$steps),2)
md <- median(sum.steps.per.day.clear$steps)
# ---- Histogram of the total number of steps taken each day without NA ---


plot.new()
par(mfrow= c(2,1))
# - - - - - - - - - - without NA ????????? mean or sum? Добавить mean and median в легенду
hist(sum.steps.per.day.clear$steps, main = "Total number of steps taken each day after missing values are imputed", 
     breaks = 61,
     xlab = "Steps per day", ylab = "Frequency",
     col="black",
     border="white")
box(bty="l")
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
rug(sum.steps.per.day.clear$steps)
#  - - - - -with NA ????????? mean or sum?
hist(sum.steps.per.day$steps, main = "Total number of steps taken each day with NA", 
     breaks = 61,
     xlab = "Steps per day", ylab = "Frequency",
     col="black",
     border="white")
box(bty="l")
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
rug(sum.steps.per.day$steps)

# ------ mean and median ---
print(paste0("mean.with.NA = ", mn.NA))
print(paste0("median.with.NA = ", md.NA))
print(paste0("after missing values are imputed mean = ", mn))
print(paste0("after missing values are imputed median = ", md))

# - -----  find the day of the week for each measurement in the dataset ---
steps.raw.clear$date <- as.Date(steps.raw.clear$date)
# add column with 'weekday' or 'weekend'
for(i in 1:length(steps.raw.clear$steps))   {
      if( weekdays(steps.raw.clear$date[i]) %in% c("Saturday", "Sunday")  )   {
        steps.raw.clear$weekday[i] <-  "weekend"     
                    }#if..
        else {
            steps.raw.clear$weekday[i] <-  "weekday"
            #print("weekend")    
            
        } 
}#for..
# Panel plot comparing the average number of steps taken per 5-minute interval 
# across weekdays and weekends --
mean.steps.raw.clear <- steps.raw.clear %>%
    group_by(interval,weekday) %>%
    summarise_each(funs( mean(steps)), steps = steps, weekday = weekday) 

plot.new()
ggplot(mean.steps.raw.clear, aes(x=interval, y=steps)) +
    geom_line(colour = "darkblue") +  
    facet_grid(weekday ~ .) + 
    geom_point(size=0.7) +
    scale_x_continuous(name = "Time interval (by 5-minyt step)", limits = c(0,tail(mean.steps.raw.clear$interval, n=1)), breaks = daily.activity.average.with.NA$interval[brake.vec])  +
    scale_y_continuous(name = "Time series plot of the average number of steps taken", limits = c(0,max(mean.steps.raw.clear$steps))) +
    ggtitle("Mean total number of steps taken per day") +
    theme(plot.title = element_text(hjust = 0.5)) +
    #geom_vline(xintercept=max.steps$interval, linetype = 2, color="red") +
    #geom_text(aes(max.steps$interval, label="\ninterval = 853;   steps = 206.2", y=50), angle=90, color="red") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
    stat_smooth(colour="green", method = 'loess', na.rm=TRUE) 








