#By: Glenda Ascencio                                                              Independent Research 2015
##############################################################################################################
##                          Analysing Some Questions On the Motor Collisions Dataset                       ##                        
##############################################################################################################

### Loading libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
# library(maps)
# library(ggmap)
library(scales)
library(gridExtra)

### Load and set the working directory to get the motor collisions clean dataset
setwd("~/Desktop/Collision_Analysis")
load("car_collisions.RData")

########################################################################################
##        Answering Analytical Questions About the Motor Collisions Dataset       ##
### Driver Distraction and Inattention: Advances in Research by professor Michael Regan
# did this research on car collision with the purpose to answer the question 1) what potentaly 
#distracting task/behaviors do CMV drivers engage in and do they increase risk? 2) What is
#the impact of the task/behavior on drawing the drive's eyes away from the forward roadway? 
# Rll = he found the 71% of the crashed occured, 46% of the crashes-near, and 60% of the SCEs
# involve in the driver engaging in a non-driving task just before the occurance of the kinematics
#trigger. 77% occured by texting where the driver's eyes were off the forward roadway. Thus
#texting had the highest mean duratin road, listing and talking which also caused not gaze
#concentration and cognitive distration
## Faltal crashes per borough => No data available for NYC
## Highways
### Change in both directions from daylight to 
##It would be good to analyze:
# 1)The total # of miles rode per borough = > accidents per miles driven 
# 2) Police report car collision accidents == available only for sept 2015 
# 3) Precinct car collisions report == only available for 2004-2009 which is unimportant
# 4) Take the difference between the change in accidents => Difference in spring < fall

#########################################################################################
# Introduction: This research is about investigating the causes, the seasons, location, year and months that there 
#are more car accidents in NYC. We obtained our data from the Motor Vehicle Collisions in New York City provided by 
#the Police Department (NYPD) which has a detailed information of more than 665,000 of reported car collisions in NYC.

# Dataset: The variables have information about the boroughs, locations, street names, total people injured, total 
#people killed, total pedestrians injured, total pedestrians killed, total cyclist injured, total cyclist killed, 
#total motorist injured, total motorist killed, reason vehicle crashed, and type of vehicle. All this data was 
#imported, cleaned, and vizualize into R and saved as car_collision for easier analyzation in the future.

library(ggplot2)
library(gridExtra)
library(scales)
library(gridExtra)
library(dplyr)

### Loading and seting the working directory to get the clean motor collisions dataset
setwd("~/Desktop/Collision_Analysis")
load("car_collisions.RData")


#1) Trying to find the seasons with the highest number of car collisions. According to our results, Brooklyn has the
  #highest number of car collisions on May 05, 2015 with 4507 compared to the other seasons less than 4300 and 
  #neighborhoods which had less than 3850 number of car collisions. In Brooklyn, after we filter the data so we can 
  #obtain the numbers of car collision greater than 4200 for each season, we were able to observe that the season 
  #with the highest number of car collision is the Spring 2015 with 17354, the summer with 12857, the fall with 4221,
  #and winter with lowest less than 4200. *Spring: mar-may *summer: jun-Aug *Autumn: Sep-Nov *Winter: Dec-Feb 

# a) Searching for answer
seasons <- car_collision %>%
  group_by(borough, season, year_month) %>%
  summarize(season_collision=n())

# b) Filtering in order to obtain the highest car collisions above 4200
seasons1 <- filter(seasons, season_collision >= 4200)
seasons1

# c) Plotting the total number of car collision for each season
ggplot(seasons, aes(x = year_month, y = season_collision, colour = borough)) +
  geom_point()+
  ggtitle("Total Number of Car Collision For Each Season")+
  xlab("Seasons")+
  ylab("Total Number of Accidents")+
  facet_wrap(~season)+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=40, hjust=1),
        legend.background = element_rect(fill = "transparent"))

#2) Since we saw that the Spring has the highest number of car collision, we tried to investigate if the Daylight Saving
 #Time caused more car collisions in NYC. We know that the DST changes do not necessarily occur on the same date 
  #every year. For instance, on March is when the clock is turned one hour forward and on November is when the clock
  #is turned one hour backward. Therefore, we procedeed to investigate the years from 2012 to 2015, comparing the 
  #number of crashes on the Monday after the shift from Daylight Saving Time to Standard time and the previous Monday, 
  #allowing us to compare Monday to Monday with the only variable being the time shift.
daylight_saving <- car_collision %>%
  group_by(date, season, borough) %>%
  summarize(dls=n())  

  # a) We didn't had the March 11 DST in information in 2012. That is why we just proceded in analyzing Monday November 05 after the shift from Daylight Saving Time to Standard time and compare it to the previous Monday October 29, 2015. Our results shows that for each borough there was a huge number of increasement after the daylight saving.

nov_12 <- daylight_saving[daylight_saving$date == "2012-11-05" |daylight_saving$date == "2012-10-29", ] 

    # -> Comparing each individualy borough for the two Mondays 
ggplot(nov_12, aes(x = date, y = dls, colour = borough)) +
  geom_point(aes(shape = as.factor(borough), size = 5))+
  scale_size_area()+
  ggtitle("Monday November 05 vs Monday October 29, 2012")+
  xlab("November 2012 Daylight Saving")+
  ylab("Total Number of Accidents")

    # -> Comparing the two dates in average
ggplot(na.omit(nov_12), aes(x = date, y = dls)) + 
  geom_boxplot(aes(fill = date), alpha = 0.1) + 
  guides(fill=FALSE) + 
  labs(x = "Nov 2012 Daylight Saving", 
       y = "Total Number of Car Collisions", 
       title = "Monday November 05 vs Monday October 29, 2012") 

    # Since we wanted to be precise on how much the Daylight Savings affected the number of car collision, we decided to subtracting the day after the Daylight Saving on Monday, Nov 5 2012 with the previous Monday, Oct 29 2015
bk_n12 = filter(nov_12, borough=="BROOKLYN")
bk_n12_diff <- bk_n12[2,4] - bk_n12[1,4]
bk_n12_diff

bx_n12 = filter(nov_12, borough=="BRONX")
bx_n12_diff <- bx_n12[2,4] - bx_n12[1,4]
bx_n12_diff

qn_n12 = filter(nov_12, borough=="QUEENS")
qn_n12_diff <- qn_n12[2,4] - qn_n12[1,4]
qn_n12_diff

mn_n12 = filter(nov_12, borough=="MANHATTAN")
mn_n12_diff <- mn_n12[2,4] - mn_n12[1,4]
mn_n12_diff

st_n12= filter(nov_12, borough == "STATEN ISLAND")
st_n12_diff <- st_n12[2,4] - st_n12[1,4]
st_n12_diff

  # b) The Daylight Saving occured on Sunday, March 10, 2:00 AM	Sunday, November 3, 2:00 AM 2013. Therefore, we analyzed March 11 vs March 4 and Nov 4 vs Oct 28 2013 to see if the number of car collisions were augmented when the clock was moved clockwise and counterclockwise. On March 2013, our result shows that only on the Bronx the Daylight Saving time increased the number of car collisons. On the other hand, on November 2013 the data shows that on Brooklyn, Bronx, and Queens the DLS tremendously increase the number of car collisions.  
mar_and_nov_13 <- daylight_saving[daylight_saving$date == "2013-03-11" |
                                    daylight_saving$date == "2013-03-04" |
                                    daylight_saving$date == "2013-11-04" |
                                    daylight_saving$date == "2013-10-28", ] 
    # -> Comparing each individualy borough for the two Mondays 
ggplot(mar_and_nov_13, aes(x = date, y = dls, colour = borough)) +
  geom_point(aes(shape = as.factor(borough), size = 5))+
  scale_size_area() +
  ggtitle("March 11 vs March 4 and Nov 4 vs Oct 28 2013")+
  xlab("March and November 2013 Daylight Saving")+
  ylab("Total Number of Accidents")

      # -> Comparing the two dates in average
ggplot(na.omit(mar_and_nov_13), aes(x = date, y = dls)) + 
  geom_boxplot(aes(fill = date), alpha = 0.1) + 
  guides(fill=FALSE) + 
  labs(x = "March and November 2013 Daylight Saving", 
       y = "Total Number of Car Collisions", 
       title = "March 11 vs March 4 and Nov 4 vs Oct 28 2013") 


      # Since we wanted to be precise on how much the Daylight Savings affected the number of car collision, we 
      #decided to subtracting the day after the Daylight Saving on Monday, March 11 with the previous Monday, March 4 2013
bk_m13 = filter(mar_and_nov_13, borough=="BROOKLYN")
bk_m13_diff <- bk_m13[2,4] - bk_m13[1,4]
bk_m13_diff 

bx_m13 = filter(mar_and_nov_13, borough=="BRONX")
bx_m13_diff <- bx_m13[2,4] - bx_m13[1,4]
bx_m13_diff

qn_m13 = filter(mar_and_nov_13, borough=="QUEENS")
qn_m13_diff <- qn_m13[2,4] - qn_m13[1,4]
qn_m13_diff

mn_m13 = filter(mar_and_nov_13, borough=="MANHATTAN")
mn_m13_diff <- mn_m13[2,4] - mn_m13[1,4]
mn_m13_diff

st_m13= filter(mar_and_nov_13, borough == "STATEN ISLAND")
st_m13_diff <- st_m13[2,4] - st_m13[1,4]
st_m13_diff

      # Since we wanted to be precise on how much the Daylight Savings affected the number of car collision, we 
      #decided to subtracting the day after the Daylight Saving on Nov 4 with the previous Oct 28 2013
bk_n13 = filter(mar_and_nov_13, borough=="BROOKLYN")
bk_n13_diff <- bk_n13[4,4] - bk_n13[3,4]
bk_n13_diff

bx_n13 = filter(mar_and_nov_13, borough=="BRONX")
bx_n13_diff <- bx_n13[4,4] - bx_n13[3,4]
bx_n13_diff

qn_n13 = filter(mar_and_nov_13, borough=="QUEENS")
qn_n13_diff <- qn_n13[4,4] - qn_n13[3,4]
qn_n13_diff

mn_n13 = filter(mar_and_nov_13, borough=="MANHATTAN")
mn_n13_diff <- mn_n13[4,4] - mn_n13[3,4]
mn_n13_diff

st_n13= filter(mar_and_nov_13, borough == "STATEN ISLAND")
st_n13_diff <- st_n13[4,4] - st_n13[3,4]
st_n13_diff


    # c) The Daylight Saving occured on Sunday, Sunday, March 9, 2:00 AM	Sunday, November 2, 2:00 AM 2014. Therefore, 
      #we analyzed March 10 vs March 3 and Nov 3 vs Oct 27 2014 to see if the number of car collisions were augmented when the clock was moved clockwise and counterclockwise. On March 2014, our result shows that only on Brooklyn, Bronx, Staten Island and Manhattan the DLS tremendously increased the number of car collisions. Similarly, on November 2014, Staten Island and Queen were the only affected boroughs were the Daylight Saving Time augmented the number of car collisions.

mar_and_nov_14 <- daylight_saving[daylight_saving$date == "2014-03-10" |
                                    daylight_saving$date == "2014-03-03" |
                                    daylight_saving$date == "2014-11-03" |
                                    daylight_saving$date == "2014-10-27", ] 

    # -> Comparing each individualy borough for the two Mondays 
ggplot(mar_and_nov_14, aes(x = date, y = dls, colour = borough)) +
  geom_point(aes(shape = as.factor(borough), size = 5))+
  scale_size_area()+
  ggtitle("March 10 vs March 3 and	Sunday, Oct 27 vs November 3, 2014")+
  xlab("March and November 2014")+
  ylab("Total Number of Accidents")

    # -> Comparing the two dates in average
ggplot(na.omit(mar_and_nov_14), aes(x = date, y = dls)) + 
  geom_boxplot(aes(fill = date), alpha = 0.1) + 
  guides(fill=FALSE) + 
  labs(x = "March and November 2014 Daylight Saving", 
       y = "Total Number of Accidents", 
       title = "March 10 vs March 3 and	Sunday, Oct 27 vs November 3, 2014") 


      # Since we wanted to be precise on how much the Daylight Savings affected the number of car collision, we 
      #decided to subtracting the day after the Daylight Saving on Monday, March 10 with the previous Monday, March 3 2013
bk_m14 = filter(mar_and_nov_14, borough=="BROOKLYN")
bk_m14_diff <- bk_m14[2,4] - bk_m14[1,4]
bk_m14_diff

bx_m14 = filter(mar_and_nov_14, borough=="BRONX")
bx_m14_diff <- bx_m14[2,4] - bx_m14[1,4]
bx_m14_diff

qn_m14 = filter(mar_and_nov_14, borough=="QUEENS")
qn_m14_diff <- qn_m14[2,4] - qn_m14[1,4]
qn_m14_diff

mn_m14 = filter(mar_and_nov_14, borough=="MANHATTAN")
mn_m14_diff <- mn_m14[2,4] - mn_m14[1,4]
mn_m14_diff

st_m14= filter(mar_and_nov_14, borough == "STATEN ISLAND")
st_m14_diff <- st_m14[2,4] - st_m14[1,4]
st_m14_diff

    # Since we wanted to be precise on how much the Daylight Savings affected the number of car collision, we 
    #decided to subtracting the day after the Daylight Saving on Monday, Nov 3 with the previous Monday, Oct 27 2014

bk_n14 = filter(mar_and_nov_14, borough=="BROOKLYN")
bk_n14_diff <- bk_n14[4,4] - bk_n14[3,4]
bk_n14_diff

bx_n14 = filter(mar_and_nov_14, borough=="BRONX")
bx_n14_diff <- bx_n14[4,4] - bx_n14[3,4]
bx_n14_diff

qn_n14 = filter(mar_and_nov_14, borough=="QUEENS")
qn_n14_diff <- qn_n14[4,4] - qn_n14[3,4]
qn_n14_diff

mn_n14 = filter(mar_and_nov_14, borough=="MANHATTAN")
mn_n14_diff <- mn_n14[4,4] - mn_n14[3,4]
mn_n14_diff

st_n14= filter(mar_and_nov_14, borough == "STATEN ISLAND")
st_n14_diff <- st_n14[4,4] - st_n14[3,4]
st_n14_diff


    # d) The Daylight Saving Time in NYC was on Sunday, March 8, 2:00 AM	Sunday, November 1, 2:00 AM 2015. Since we are currently in November 2015 the NYC Open Data haven't displayed the dataset online. Therefore, we just focused on analyzing March 9 and March 2, 2015.

mar_15 <- daylight_saving[daylight_saving$date == "2015-03-09" |
                            daylight_saving$date == "2015-03-02", ] 

    # -> Comparing each individualy borough for the two Mondays 
ggplot(mar_15, aes(x = date, y = dls, colour = borough)) +
  geom_point(aes(shape = as.factor(borough), size = 5))+
  scale_size_area()+
  ggtitle("March 2 vs March 9 2015 Daylight Saving")+
  xlab("March 2015 Daylight Saving")+
  ylab("Total Number of Accidents")

    # -> Comparing the two dates in average
ggplot(na.omit(mar_15), aes(x = date, y = dls)) + 
  geom_boxplot(aes(fill = date), alpha = 0.1) + 
  guides(fill=FALSE) + 
  labs(x = "March 2015 Daylight Saving", 
       y = "Total Number of Car Collisions", 
       title = "March 2 vs March 9 2015 Daylight Saving") 

    # Since we wanted to be precise on how much the Daylight Savings affected the number of car collision, we decided to subtracting the day after the Daylight Saving on Monday, March 9 with the previous Monday, March 2 2015
bk_m15 = filter(mar_15, borough=="BROOKLYN")
bk_m15_diff <- bk_m15[2,4] - bk_m15[1,4]
bk_m15_diff

bx_m15 = filter(mar_15, borough=="BRONX")
bx_m15_diff <- bx_m15[2,4] - bx_m15[1,4]
bx_m15_diff

qn_m15 = filter(mar_15, borough=="QUEENS")
qn_m15_diff <- qn_m15[2,4] - qn_m15[1,4]
qn_m15_diff

mn_m15 = filter(mar_15, borough=="MANHATTAN")
mn_m15_diff <- mn_m15[2,4] - mn_m15[1,4]
mn_m15_diff

st_m15= filter(mar_15, borough == "STATEN ISLAND")
st_m15_diff <- st_m15[2,4] - st_m15[1,4]
st_m15_diff


### 3) Most of the time, the Daylight Saving Time on November increases the number of car collision. Therefore, we would like to investigate further the reasons why most of the car accidents occurs in NYC
    # a) Calculations
reasons_type <- car_collision %>% 
  group_by(borough, year_month, season, reason_vehicle1_crashed) %>% 
  summarize(reasons = n())

    # b) Arranging the data so I can obtain the high reasons that caused car collisions. We know that for each 
    #reason 631 is the max number of car collisions. Therefore, in order to find the top 5 reasons I will filter the data 
reasons_type <- filter(reasons_type, reason_vehicle1_crashed != "Unspecified")
reasons_type <- filter(reasons_type, reasons >= 220)


    # c) Plot the highest causes of car collisions for each borough
ggplot(reasons_type, aes(x = reason_vehicle1_crashed, y = reasons, color = as.factor(season)))+
  geom_point(aes(shape = as.factor(season), size = 2))+
  ggtitle("Reasons That Most Car Accidents Occured")+
  xlab("Reasons They That They Were Killed")+
  ylab("Total Number of invididuals killed")+
  facet_wrap(~ borough)+
  scale_y_continuous(limits=c(100, 600))+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

    # d) Plotting the highest reason for the number of car collision 
ggplot(reasons_type, aes(x = reason_vehicle1_crashed, y = reasons, color = as.factor(season)))+
  geom_point(aes(shape = as.factor(season), size = 2))+
  ggtitle("Reasons That Most Car Accidents Occured")+
  xlab("Reasons They That They Were Killed")+
  ylab("Total Number of invididuals killed")+
  scale_y_continuous(limits=c(100, 600))+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

### 4) Since there's a plethora of individuals who would like to to know in what locations we have the more number 
  #of collisions so we can drive carefully in those areas. Our result shows that in June 2015, East 138 Street Bronx 10464 had the highest number of car collisions. 	
total_accidents <- car_collision %>% 
  group_by(borough, zip_code, year_month, on_street_name, location) %>% 
  summarize(accidents_per_bz = n())

    # a) Since the max of total accidents is 29. Therefore, I decided to take the 50% and up of the data == highest # of CC
total_accidents <- filter(total_accidents, accidents_per_bz >= 14.5)

    # b) Plot the number of accidents for each borouh
ggplot(total_accidents, aes(x = location, y = accidents_per_bz, colour = as.factor(borough), size = 3)) +
  geom_point(aes(shape = as.factor(zip_code), size = 15))+
  ggtitle("Total Number of Car Collision for Each Zip Code, Borough, Year and Month")+
  xlab("Locations")+
  ylab("Total Number of Accidents")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


#5) What was the type of vehicle that the most individuals had the highest number of car collisions?
    # a) Calculations
vehicle_type <- car_collision %>% 
  group_by(borough, year_month, reason_vehicle1_crashed, type_of_vehicle) %>% 
  summarize(vehicle_types = n())

    # b) Arranging the data so I can plot the highest vehicle class reasons and getting rid of unspefied
vehicle <- filter(vehicle_type, vehicle_types >= 30  & 
                    reason_vehicle1_crashed != "Unspecified")

    # Graphing the type of vehicle
ggplot(vehicle, aes(x = year_month, y = vehicle_types, colour = type_of_vehicle))+
  geom_point()+
  ggtitle("Type of Vehicle that Most Car Accidents Occured")+
  xlab("Month of Year of the Accident")+
  ylab("Total Number of invididuals killed")+
  facet_wrap(~ borough)+
  scale_y_continuous(limits=c(30, 340))+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

6) What is the name of the location where pedestrian were injured the most?
      # a) calculations
injured <- car_collision %>% 
  group_by(borough, season, on_street_name, year_month, total_people_injured) %>% 
  summarize(t_p_injured = n())

injured <- injured[!(is.na(injured$on_street_name) | injured$on_street_name == ""), ]

injured <- filter(injured, t_p_injured >= 70)

injured$on_street_name <- factor(injured$on_street_name, 
                                 levels = c("ATLANTIC AVENUE", 
                                            "NORTHERN BOULEVARD", 
                                            "BROADWAY",
                                            "FLATBUSH AVENUE",
                                            "QUEENS BOULEVARD",
                                            "2 AVENUE",
                                            "10 AVENUE"), 
                                 ordered = T)

      # b) Plotting
ggplot(injured, aes(x = on_street_name, y = t_p_injured, color = as.factor(borough)))+
  geom_point()+
  ggtitle("Locations Where Pedestrian Have Been Injured The Most")+
  xlab("The Name of The Street")+
  ylab("Total Number of People Injured")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


#7) Perilous boroughs, year and months where more motorist accidents has happended the most?

    # a) Calculations
motorist_accidents <- car_collision %>% 
  group_by(borough, on_street_name, location, year_month, total_motorist_killed) %>% 
  summarize(motorist_killed = n())
motorist_accidents <- filter(motorist_accidents, motorist_killed >= 10)
motorist_accidents<- motorist_accidents[!(is.na(motorist_accidents$on_street_name) | 
                                            motorist_accidents$on_street_name == ""), ]

      # b) Plotting
ggplot(motorist_accidents, aes(x = on_street_name, y = motorist_killed, color = as.factor(borough)))+
  geom_point()+
  ggtitle("Location Where Most Motorist Were Killed The Most")+
  xlab("Name of the Street")+
  ylab("Total Number of Motorist Killed")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


#8) Every how many minutes, how many accidents do we have?

recent_year <- car_collision$DateTime[1]
old_year <- car_collision$DateTime[nrow(car_collision)]
diff_seconds <- as.numeric(recent_year) - as.numeric(old_year) 
diff_seconds #Rll = 94607400
diff_seconds / (60*60*24)   #Rll = 1094.02 
#1094 days have passed. There are 614,437 accidents in total. 
accidents_per_minute <- 614437/1094
accidents_per_minute #Rll = 561.6. That's means that every 2.4 minutes there's an accident