#loading the file
        if(!file.exists('activity.csv')){
                unzip('activity.zip')
        }
#loading packages
        library(lubridate)
        library(dplyr)
        library(ggplot2)
        
#dare data processing
        activity.data <- read.csv('activity.csv')
        activity.data$interval<- as.character(activity.data$interval)
        activity.data$interval<- ifelse(nchar(activity.data$interval)==1,
                                        paste('000',activity.data$interval, '00',sep = ''),
                                        activity.data$interval)
        activity.data$interval<- ifelse(nchar(activity.data$interval)==2, 
                                        paste('00',activity.data$interval, '00',sep = ''), 
                                        activity.data$interval)
        activity.data$interval<- ifelse(nchar(activity.data$interval)==3, 
                                        paste('0',activity.data$interval, '00',sep = ''), 
                                        activity.data$interval)
        activity.data$interval<- ifelse(nchar(activity.data$interval)==4,
                                        paste(activity.data$interval, '00',sep = ''), 
                                        activity.data$interval)
        activity.data$interval<- paste(
                substr(activity.data$interval, 1,2),':',
                substr(activity.data$interval, 3,4),':',
                substr(activity.data$interval, 5,6), sep='')
        activity.data$interval<- paste(activity.data$date, activity.data$interval)
        activity.data$interval<- ymd_hms(activity.data$interval)
        activity.data$date<- ymd(activity.data$date)
#total number of steps per day
        total.steps.daily<- activity.data %>%
                group_by(date) %>%
                summarise(steps=sum(steps, na.rm = T))
#What is mean total number of steps taken per day?
        hist(total.steps.daily$steps, 
             breaks = 20, 
             xlab='steps in a day (counts)', 
             ylab='number of days (count)')
        abline(v=mean(total.steps.daily$steps), 
               col='red', lty=2)
        text(x=mean(total.steps.daily$steps), 
             y=10, 
             paste('mean=',floor(mean(total.steps.daily$steps))) , 
             col='red')
        abline(v=median(total.steps.daily$steps), 
               col='blue', 
               lty=2)
        text(x=median(total.steps.daily$steps),
             y=9,
             paste('median=',floor(median(total.steps.daily$steps))) ,
             col='blue')
#mean and median steps per day
        steps.daily.median <- median(total.steps.daily$steps)
        steps.daily.mean <- mean(total.steps.daily$steps)


#activity pattern
        activity.data$interval <- substr(as.character(activity.data$interval), 12, 19)
        int.steps.avg<- activity.data %>%
                group_by(interval) %>%
                summarise(int.avg=mean(steps, na.rm = T))
        plot(x=strptime(int.steps.avg$interval, format = '%H:%M:%S'), 
             main=' average daily activity pattern',
             y=int.steps.avg$int.avg,type = 'l',
             xlab = 'Time of the day, 5-minute intervals, (HH:MM)',
             ylab = 'average staps (count)')
        points(x=strptime(int.steps.avg$interval[which.max(int.steps.avg$int.avg)], format = '%H:%M:%S'), 
               y=int.steps.avg$int.avg[which.max(int.steps.avg$int.avg)], 
               col='red', 
               pch='X')
        text(x=strptime(int.steps.avg$interval[which.max(int.steps.avg$int.avg)], format = '%H:%M:%S'),
             y=int.steps.avg$int.avg[which.max(int.steps.avg$int.avg)], col='red',
             paste('time=',int.steps.avg$interval[which.max(int.steps.avg$int.avg)],
                   '\n', 
                   'avg steps=',
                   floor(int.steps.avg$int.avg[which.max(int.steps.avg$int.avg)])), pos = 1)
#how many missing values
        not.complete.cases <-sum( !complete.cases(activity.data))        
        sum(is.na(activity.data$steps))
#filling in missing values with the corresponding average value over the two month period
        activity.data1<-activity.data
        for (i in 1: 17568) {
                if (is.na(activity.data1$steps[i])) {
                        activity.data1$steps[i] <- int.steps.avg$int.avg[ 
                                which(int.steps.avg$interval==activity.data1$interval[i])] 
        
                }
        }
#mean and median fjor the acticvity.data1 dataset
        #total number of steps per day
        total.steps.daily1<- activity.data1 %>%
                group_by(date) %>%
                summarise(steps=sum(steps, na.rm = T))
                
                
        hist(total.steps.daily1$steps, 
             breaks = 20, 
             xlab='steps in a day (counts)', 
             ylab='number of days (count)')
        abline(v=mean(total.steps.daily1$steps), 
               col='red',
               lty=2,
               lwd=3)
        text(x=mean(total.steps.daily1$steps), 
             y=10, 
             paste('mean=',floor(mean(total.steps.daily1$steps))) , 
             col='red')
        abline(v=median(total.steps.daily1$steps), 
               col='blue', 
               lty=2)
        text(x=median(total.steps.daily1$steps),
             y=9,
             paste('median=',floor(median(total.steps.daily$steps))) ,
             col='blue')

#mean and median steps per day
        steps.daily.median1 <- median(total.steps.daily1$steps)
        steps.daily.mean1 <- mean(total.steps.daily1$steps)
#What is the impact of filling in the missing values?
#as missing values are filled out in by the averages over the 2 month period; the mean and median shifts towards the mean
activity.data1$days<- weekdays(activity.data$date)        
total.steps.daily1$days<- weekdays(total.steps.daily1$date)        
activity.data1$days<-ifelse(activity.data1$days=='Saturday' | activity.data1$days=='Sunday', 
       'Weekend', 
       'Weekday')

head(total.steps.daily1)
interval.rec<- read.csv('activity.csv')
interval.rec$interval->activity.data1$interval
head(activity.data1)
avg.steps.days<- activity.data1 %>%
        group_by(days, interval) %>%
        summarise(avg.steps=mean(steps))

ggplot(data = avg.steps.days, 
       aes(x= interval, 
           y=avg.steps)) + 
        geom_line(aes(color=days)) 
ggplot(data = avg.steps.days, 
       aes(x= interval, 
           y=avg.steps)) + 
        geom_line()+
        facet_grid(rows = vars(days))
