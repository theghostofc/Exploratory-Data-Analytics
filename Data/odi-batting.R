
# Loading data 
odi <- read.csv("odi-batting.csv",stringsAsFactors = F)


# Strucuture of odi 
str(odi)

# Converting to character type "MatchDate" to date type
odi$MatchDate<- as.Date(odi$MatchDate,format = "%d-%m-%Y")

# Type Driven Metrics: Storing "years" from date in year variable
odi$year <- format(odi$MatchDate,"%Y")

# Business Driven Metric: Centuries

odi$hundred <- ifelse(odi$Runs>=100,1,0)

# Name of the player scored maximum number of 100's in an odi career
max_hundred_player <- aggregate(hundred~Country+Player,odi,sum)
# ordering with hundreds
max_hundred_player <- max_hundred_player[order(max_hundred_player$hundred,decreasing = T),]


# Name of the player scored maximum number of 50's in an odi career
odi$fifty <- ifelse(odi$Runs>=50&odi$Runs<100,1,0)
max_fifty_player <- aggregate(fifty~Country+Player,odi,sum)
# ordering with hundreds
max_fifty_player <- max_hundred_player[order(max_fifty_player$fifty,decreasing = T),]



# Business Driven Variable: Deriving Strike_rate  

odi$strike_rate<- round((odi$Runs/odi$Balls)*100,digits = 2)


# How many centuries did the played scored who is top in the list of making highest centuries ?
max_hundred_player <- aggregate(hundred~Country+Player,odi,sum)


# Subseting

Indian_ply <- subset(odi,Country=="India")
century_year <- aggregate(hundred~year,Indian_ply,sum)




