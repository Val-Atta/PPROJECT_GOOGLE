#Load Required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotrix)
# Install git2r package
install.packages("git2r")

# Load git2r package
library(git2r)

# Clone repository & load the datasets
repo <- clone("https://github.com/Val-Atta/PPROJECT_GOOGLE.git", "local/path/to/repo")
#Change the 'start_station_id' & 'end_station_id' attribute in the dataframes to integer data type to ease the next action, merging individual dataframes to one big dataframe.

r_202012$start_station_id <- as.integer(r_202012$start_station_id)
r_202101$start_station_id <- as.integer(r_202101$start_station_id)
r_202102$start_station_id <- as.integer(r_202102$start_station_id)
r_202103$start_station_id <- as.integer(r_202103$start_station_id)
r_202012$end_station_id <- as.integer(r_202012$end_station_id)
r_202101$end_station_id <- as.integer(r_202101$end_station_id)
r_202102$end_station_id <- as.integer(r_202102$end_station_id)
r_202103$end_station_id <- as.integer(r_202103$end_station_id)

#Merge individual month's data frames into one big data frame
all_trips <- bind_rows(r_202004, r_202005, r_202006, r_202007, r_202008, r_202009, r_202010, r_202011, r_202012, r_202101, r_202102, r_202103)
#Check the structure of the consolidated data frame

str(all_trips)

# Add columns that list the date, month, day, and year of each ride & this will allow to aggregate ride data for each month, day, or year.

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%B")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- weekdays(all_trips$date)
#Check member_casual and rideable_type column to if there are any discrepancies on the rider and bike type

unique(all_trips$member_casual)

unique(all_trips$rideable_type)

#Clean the new dataset
all_trips <- drop_na(all_trips)

all_trips <- (filter(all_trips, !(start_station_name == "WATSON TESTING - DIVVY" | start_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" | start_station_name =="hubbard_test_lws" | start_station_name =="")))

#Create a data frame of stations with start station name and latitude and longitude. I will use this data frame while analyzing the popular top 10 stations
all_trips_stations <- all_trips[,c(5,9,10)]

#Continue cleaning dataset
all_trips_stations <- all_trips_stations[!duplicated(all_trips_stations$start_station_name),]
Let's next focus on the average length of each ride, this time in minutes. We see that on average, each ride is close to 30 minutes. We'll then break that down by casual riders versus members.

## ----ride_length calculations

summary(all_trips$ride_length)/60
--------------------------------------------
Looking at casual riders versus members, we can see that the average casual ride is about 46 minutes compared to the members' average ride of 16 minutes. The median rides are 22 minutes and 12 minutes respectively.

## ----Members vs. Casual Riders----------------------------------------------
aggregate(all_trips$ride_length/60 ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length/60 ~ all_trips$member_casual, FUN = median)
Using the statistical mode, we see that the most common day for renting bikes is Saturday.

## ----Mode of data-----------------------------------------------------------
aggregate(all_trips$day_of_week ~ all_trips$member_casual, FUN = mfv)

Also, we can take a look at the average ride time by day for members and casual riders with duration again in minutes. Regardless of day of the week, casual users ride 2.7x to 3x longer than members, with both groups riding longer on weekends.

## ---------------------------------------------------------------------------
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips$ride_length/60 ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
## ---------------------------------------------------------------------------
options(warning=-1)
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()  
  ,average_duration = mean(ride_length/60)) %>% 		
  arrange(member_casual, weekday)
--------------------------------------------Create effective data visualizations.
## ----Number of rides by rider type------------------------------------------
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 1: Number of Rides by Day and Rider Type") + 
  ylab("Number of Rides (1e+05 = 100,000)") + 
  xlab("Day of Week")
## ----Average duration-------------------------------------------------------
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 2: Average Ride Duration by Day and Rider Type") + 
  ylab("Average Duration (minutes)") + 
  xlab("Day of Week")
## ----Number of rides by bike type and rider type----------------------------
all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 3: Number of Rides by Bike Type and Rider Type") + 
  ylab("Number of Rides (5e+05 = 500,000)") + 
  xlab("Bike Type")

## ----Number of rides by day and bike type-----------------------------------
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 4: Number of Rides by Day and Bike Type") + 
  ylab("Number of Rides (1e+05 = 100,000)") + 
  xlab("Day of Week")

## ----Average duration by bike type------------------------------------------
all_trips %>% 
mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(rideable_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 5: Average Ride Duration by Day and Bike Type") + 
  ylab("Average Duration (minutes)") + 
  xlab("Day of Week")

## ----Number of rides by month and rider type--------------------------------
all_trips %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) + 
  geom_point() +
  labs(title = "Table 6: Number of Rides by Month and Rider Type") + 
  ylab("Number of Rides (1e+05 = 100,000)") + 
  xlab("Month")
