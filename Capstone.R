install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("skimr")
install.packages("scales")
install.packages("purrr")
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(skimr)
library(scales)
library(purrr)

daily_act <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight_log <- read.csv("weightLogInfo_merged.csv")

head(daily_act)
colnames(daily_act)

head(weight_log)
colnames(weight_log)

head(sleep_day)
colnames(sleep_day)
class(daily_act$ActivityDate)

write.csv(daily_act_new,file='/Users/Gina/Documents/Capstone/daily_act.csv', row.names=FALSE)
write.csv(sleep_day_new,file='/Users/Gina/Documents/Capstone/sleep_day.csv', row.names=FALSE)
write.csv(weight_log_new,file='/Users/Gina/Documents/Capstone/weight_log.csv', row.names=FALSE)

daily_act_new <- daily_act %>%
  filter(TotalSteps != 0)%>%
  arrange(ActivityDate) %>%
  select(-LoggedActivitiesDistance,-VeryActiveDistance,-ModeratelyActiveDistance,-LightActiveDistance,-SedentaryActiveDistance)

n_distinct(daily_act_new$Id)

weight_log_new <- weight_log %>%
  arrange(Date)%>%
  separate(Date, c("Date", "Time"), " ") %>%
  select(-Fat,-WeightKg)

sleep_day_new <- sleep_day %>%
  arrange(SleepDay)%>%
  separate(SleepDay, c("Date", "Time"), " ") %>%
  select(-TotalSleepRecords)

#Analysis Stage
summary(daily_act_new)
#This shows that the average steps people have taken are 8319. The Average distance is 5.98. The tracked very active minutes 
# is an average of 23.02 and the sedentary minutes is averaged at 955.8. The average caloried burned from the data is 2361. 
summary(weight_log_new)
#From the weight log, the average weight of the users is 158.8 and the BMI is 25.19
summary(sleep_day_new)
#The average amount of time asleep tracked is 419.5 and the total of minutes spent in bed is 458.6

#Creating Visualizations
Act_sleep <- merge(daily_act_new, sleep_day_new, by="Id")
#This variable will allow us to cross reference values from the daily activity data and the sleep data
ggplot(data=daily_act_new)+geom_point(mapping=aes(x=TotalSteps,y=Calories,alpha = 0.5)) + 
  labs(title="Total Steps Taken And Calories Burned")

ggplot(data=daily_act_new)+geom_point(mapping=aes(x=TrackerDistance,y=Calories)) + geom_smooth(mapping=aes(x=TrackerDistance,y=Calories))+
  labs(title="Distanced Walked and Calories Burned")

ggplot(data=Act_sleep)+geom_point(mapping=aes(x=TotalMinutesAsleep,y=VeryActiveMinutes)) + 
  labs(title="High Activity Levels and Minutes Asleep ")

ggplot(data=Act_sleep)+geom_point(mapping=aes(x=TotalMinutesAsleep,y=SedentaryMinutes)) + 
  labs(title="Low Activity Levels and Minutes Asleep ")

ggplot(data=Act_sleep)+geom_point(mapping=aes(x=TotalMinutesAsleep,y=TotalSteps)) + 
  geom_smooth(mapping=aes(x=TotalMinutesAsleep,y=TotalSteps))+
  labs(title="Relationship Between Walking and Sleep Time ")
