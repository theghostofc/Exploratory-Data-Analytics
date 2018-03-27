#Uber Supply and Demand Gap
setwd("C:/Bhumit_Documents/Upgrad/Course2/M4 Assignment - Uber Supply- Demand Gap")
library(dplyr)
library(tidyr)
library(lubridate) # for parse_date_time
library(ggplot2)
library(grid) # for 2 plots
uber_df = read.csv("Uber Request Data.csv")

View(uber_df)
#Creating separate column for hours, so we can find frequency of request made during each hour
uber_df$Request.timestamp =pmax(
                              as.POSIXct(uber_df$Request.timestamp, format="%m/%d/%Y %H:%M", tz="UTC"),
                              as.POSIXct(uber_df$Request.timestamp, format="%d-%m-%Y %H:%M", tz="UTC"),
                              na.rm=TRUE
                            )

uber_df$Request.date = format(uber_df$Request.timestamp, "%d-%m-%Y")
uber_df$Request.hour = format(uber_df$Request.timestamp, "%H")

View(uber_df)
#total requests raised from city  and airpot
#this gives us an idea at which time of the day the demand is highest
#we can see form graph for pick from City is more durin Morning and Aiport is High during evening to Night.
#Plot1
ggplot(uber_df, aes(x = uber_df$Request.hour, fill = uber_df$Pickup.point )  ) + geom_bar(position="dodge")
#We will keep out foucs on these timings and derive our time slots accordingly

#Adding time slot column to df 
#to know in whcih slot frequency is highest

# as you find the high ranging bars keep that in slot, here we can se for city pickup high bars are during early hours
# and for Airport its during the evening hours
#so we will create our time slots like that
uber_df$Slots = rep(1,nrow(uber_df))
uber_df$Slots[ which(as.numeric(uber_df$Request.hour) >= 5 & as.numeric(uber_df$Request.hour) < 11 ) ] = "Morning Time";
uber_df$Slots[ which(as.numeric(uber_df$Request.hour) >= 11 & as.numeric(uber_df$Request.hour) < 17 ) ] = "Afternoon Time";
uber_df$Slots[ which( as.numeric(uber_df$Request.hour) >= 17 & as.numeric(uber_df$Request.hour) < 23 ) ] = "Evening Time";
uber_df$Slots[ which( as.numeric(uber_df$Request.hour) >= 23) ] = "Night Time";
uber_df$Slots[ which( as.numeric(uber_df$Request.hour) < 5) ] = "Night Time";

#plot using time slots
#Plot2
ggplot(uber_df, aes(x = uber_df$Slots, fill = uber_df$Status )  ) + geom_bar() + facet_wrap(~uber_df$Pickup.point)



#to see freq of No show and cancel cabs creating a sub data frame
uber_not_Complete = uber_df[ which( uber_df$Status != "Trip Completed" ), ]
#p2 = ggplot(uber_cancelled, aes(x = uber_cancelled$Request.hour, fill = factor(uber_cancelled$Status))) + geom_bar(position = "stack")
#Plot3
ggplot(uber_not_Complete, aes(x = uber_not_Complete$Request.hour, fill = uber_not_Complete$Status)) + geom_bar() + facet_wrap(~uber_not_Complete$Pickup.point)



#By this plot we can in which slot the highest number of problem persist
#here Airport to city ride  shows lac of service during Morning and pick up from City show lack of service during Eveing time
#Plot 4
ggplot(uber_not_Complete, aes(x = uber_not_Complete$Slots, fill = uber_not_Complete$Status)) + geom_bar() + facet_wrap(~uber_not_Complete$Pickup.point)


#-----Poin 2: Find out the gap between supply and demand and show the same using plots.------------------------------------------------------------

#2.1 - Find the time slots when the highest gap exists
#Plot3
ggplot(uber_df, aes(x = uber_df$Slots, fill = factor(uber_df$Status))) + geom_bar(position = "stack")
#By viewing the above graph we can see that highest gap exists in Evening Time slot i.e 17 to 23 Hours


#2.2 - Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

#From the 2nd graph plotted above we can conclude that Pick ups from City have higer gap during Evening Time slot


#facet_wrap(~pickpoint, ) slot adu airpot and city graps
#fill = status
#to get status of request made and

  







