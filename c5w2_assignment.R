#Downloading data
        file.name <- 'activity.csv'
        zip.file.name<- 'activity.zip'
        zip.file.url<- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
        if (!file.exists(zip.file.name)) {download.file(url=zip.file.url, destfile = zip.file.name)}
        if (!file.exists(file.name)) {unzip(zip.file.name)}
#Loading and preprocessing the data
df$date <- as.Date(df$date, format= "%Y-%m-%d")
library(dplyr)
#What is mean total number of steps taken per day?
mean.steps.each.day <- df%>%
        group_by(date) %>%
        summarise(mean.steps=mean(steps, na.rm = T))
total.steps.each.day <- df%>%
        group_by(date) %>%
        summarise(sum(steps, na.rm = T))
plot(x=total.steps.each.day$date, y=total.steps.each.day$`sum(steps, na.rm = T)`)
df
