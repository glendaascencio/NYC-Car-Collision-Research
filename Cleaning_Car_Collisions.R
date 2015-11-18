##By: Glenda Ascencio                             Car Collision Independent Research 2015

#########################################################################################
##                          Cleaning the Motor and Collisions Dataset                 ##                       
#########################################################################################

### Loading libraries
library(dplyr)
library(ggplot2)

### Setting the work directory
setwd("~/Desktop/Collision_Analysis")
data_dir <- "."

### Reading the collisions dataset
motor_collisions <- data.frame(read.csv("data/NYPD_Motor_Vehicle_Collisions.csv"), 
                         stringsAsFactors = F, sep=',', header=F)

###########################################################################################
##                                      Data Cleaning                                    ##
###########################################################################################

### Selecting the useful colums
collisions <- motor_collisions[ , c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 25)]

### names on collisions
names(collisions)

### Changing the names on the car collision columns into a readable format
columns <- c("date", "time","borough","zip_code","latitude","longitude","location",
             "on_street_name", "cross_street_name", "total_people_injured",
             "total_people_killed", "total_pedestrians_injured", "total_pedestrians_killed",
             "total_cyclist_injured", "total_cyclist_killed","total_motorist_injured",
             "total_motorist_killed", "reason_vehicle1_crashed","type_of_vehicle")
colnames(collisions) <- columns

### Getting rid of the empty rows in location
collisions <-  filter(collisions, !is.na(latitude) & 
                  !is.na(longitude) &
                  !is.na(zip_code) & 
                  !is.na(borough))

### Add a column for year/month/day (with time of day)
collisions <- transform(collisions, DateTime = as.character(paste(date, time, sep = " ")))
collisions$DateTime <- as.POSIXct(collisions$DateTime, format = "%m/%d/%Y %H:%M")
collisions$date <- NULL 
collisions$time <- NULL

### Bring the DateTime to the front = collision is the clean data
collisions <- collisions[ , c(18, 1:17)]

### Arranging the rows in decending order to obtain the latest and the oldest car accident
collisions <- arrange(collisions, desc(DateTime)) 

### Adding the day of the week to the df
collisions <- mutate(collisions, day = strftime(collisions$DateTime, format = "%A"))

### Adding another column that has the y, m, and day
collisions <- transform(collisions, date = strftime(DateTime, format = "%Y-%m-%d")) 

### Adding a column that has only the y and the month on it
collisions <- transform(collisions, year_month = strftime(DateTime, format = "%Y-%m"))

### Adding a column that has only the month on it
collisions <- transform(collisions, month = strftime(DateTime, format = "%m"))

### Adding a column that has only the year on it
collisions <- transform(collisions, year = strftime(DateTime, format = "%Y"))

## Adding the season column to the collisions df
collisions <- transform(collisions, season = ifelse(month == "12" | month == "01" | month == "02","WINTER",
                                                          ifelse(month == "03" | month == "04" | month == "05","SPRING",
                                                                 ifelse(month == "06" | month == "07" | month == "08","SUMMER", 
                                                                        "FALL"))))

### Bring the last columns to the first ones
car_collision <- collisions[ , c(19:24, 2:18)]


### Save the clean data frame
save(car_collision, file = sprintf('%s/car_collisions.RData', data_dir))


