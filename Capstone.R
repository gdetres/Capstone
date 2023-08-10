#Prepare by downloading packages needed for analyzing, reading the csv's to check out the structure of the three main sources. I will use daily 
#activity, sleep, and weight log.The ROCCC is fitting the low criteria on its credibility, reliability, citing and current scale as it's through 
#a third party provider for data, it's about 7 years old so pretty outdated and there's only 30 users in the program.
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

#Process - tools being used are filtering, arranging, and selecting which columns will be used in the analysis as there seem to be some data with 
#0 reports and and columns that are not as important to the overall health analysis I will be doing.Arranging the data by date can help track the 
#records easily when looking at the difference between months and days that people were active so I applied it to each three categories. Overall 
#the data was a little sloppy in the way it was recorded so I filtered out data that wasn't needed and also cleaned up the columns. 
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

#Share
Act_sleep <- merge(daily_act_new, sleep_day_new, by="Id")
#This variable will allow us to cross reference values from the daily activity data and the sleep data
ggplot(data=daily_act_new)+geom_point(mapping=aes(x=TotalSteps,y=Calories,alpha = 0.5)) + 
  labs(title="Total Steps Taken And Calories Burned")
#This specific visualization lets us know that there is a positive relationship between the 
#total amount of steps taken and calories burned off. People who walked between the 0-10000 range of 
#steps per day, were burning at lease 1500-3000 calories as day. This can be helpful for people who
#workout and tend to go running. 

ggplot(data=daily_act_new)+geom_point(mapping=aes(x=TrackerDistance,y=Calories)) + geom_smooth(mapping=aes(x=TrackerDistance,y=Calories))+
  labs(title="Distanced Walked and Calories Burned")
#The second visualization lets us know the same type of information from the first one, just 
#basing the calculations more off of distance rather than steps taken. This can let people know 
#or see a positive relationship in the longer you go, the more calories you burn. This can be helpful
#information for people who go hiking and love to walk, calories being burned doesn't 
#always result in running. There can be many ways to burn calories, you don't have to stick to running
#to achieve the same results. 

ggplot(data=Act_sleep)+geom_point(mapping=aes(x=TotalMinutesAsleep,y=VeryActiveMinutes)) + 
  labs(title="High Activity Levels and Minutes Asleep ")
#The third visualization shows an interesting correlation between high activity levels and sleeping.
#It would seem that if you burned off energy and were very active during the day, that you would knock
#out and get more time asleep but it doesn't really show that much of a relationship to the two.

ggplot(data=Act_sleep)+geom_point(mapping=aes(x=TotalMinutesAsleep,y=SedentaryMinutes)) + 
  labs(title="Low Activity Levels and Minutes Asleep ")
#The lowest activity level visualization actually shows quite the opposite of high, being that 
#people with more stagnant energy in the 500-1500 range fall into the middle range of minutes of sleep. While the high
#level show the range of 0-100 as being the most amount of sleep that people get.

ggplot(data=Act_sleep)+geom_point(mapping=aes(x=TotalMinutesAsleep,y=TotalSteps)) + 
  geom_smooth(mapping=aes(x=TotalMinutesAsleep,y=TotalSteps))+
  labs(title="Relationship Between Walking and Sleep Time ")
#The last visual shows a pretty steady graph of the amount of steps being taken and the total minutes asleep.
#It shows a pretty constant flow of steps taken and the amount of sleep people get, it seems to level out
#for each measurement range. 

#Act 
#For trends in the data, I don't think the data was strong enough or consistent enough to provide accurate 
#trends for people who use the smart device. However, some trends that were founded was that people were 
#found to spend more time in bed than being active throughout the day as the sedentary active minutes was 955.8 while 
#the average active minute was 23.02. As well as the total of 458.6 minutes being spend in bed. 
#These findings show that most of the people don't fall into the range of 10,000 steps people are suppose to 
#achieve in a day as the average is about 8319. Overall the health data is a good way for people to track 
#their activity throughout the day and I think it could be insightful for people to think of ways to get 
#more active and reach their fitness goals. It would be benefical to explore ways to remind people to 
#track their activity while using the device as that's its primary purpose but the research has shown that 
#people weren't tracking it successfully everyday. The sample size could be a lot bigger and generated over more 
#time to provide more effective results for how to improve or change the companies health device. They could 
#also provide more of an achievement based program that lets their customers know when they have reached that point
#and some tips and tricks to help themselves more.
