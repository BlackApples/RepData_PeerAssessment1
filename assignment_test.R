unzip("activity.zip")
data = read.csv("activity.csv")

sum_dates=tapply(data$steps,data$date,function(x){sum(x,na.rm=TRUE)})
hist(sum_dates,main="Frequency of total steps per day",xlab="Total steps per day")
print(mean(sum_dates,na.rm=TRUE))
print(median(sum_dates,na.rm=TRUE))

avg_intervals=tapply(data$steps,data$interval,function(x){mean(x,na.rm=TRUE)})

nas = is.na(data$steps)

data[is.na(data$steps),1]<-avg_intervals[as.character(data[is.na(data$steps),3])]

days = c(rep("weekday",5),rep("weekend",2))
names(days)<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
data=data.frame(data,day=days[weekdays(as.Date(data$date))])

avg_weekday=tapply(data[data$day=="weekday",1],data[data$day=="weekday",3],mean)
avg_weekend=tapply(data[data$day=="weekend",1],data[data$day=="weekend",3],mean)
par(mfcol=c(2,1))
plot(names(avg_weekend),avg_weekend,type="l",main="Weekend Averages",xlab="Interval (min)",ylab="Average number of steps")
plot(names(avg_weekday),avg_weekday,type="l",main="Weekday Averages",xlab="Interval (min)",ylab="Average number of steps")
