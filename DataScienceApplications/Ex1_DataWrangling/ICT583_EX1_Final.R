                                   ####################
                                   ####  ICT 583  #####
                                   ####################

## Author       : Rajesh JYOTHI
## Student ID   : 33669079                                   
## Topic        : Data Wrangling - Exercise 1  


install.packages("ggplot2") ## For visualization
library(ggplot2)

install.packages("RColorBrewer") ## for creating colorful plots and graphs
library(RColorBrewer)

install.packages("nycflights13") ## For accessing Flights dataset 
library('nycflights13')

install.packages('dplyr') ## To perform Basic Data Wrangling operations on a Data set
library(dplyr)

install.packages("Lahman") ## For accessing Teams Data set
library(Lahman)


# Loading the flights data set
Flights_data <- flights

# View the flights Data
View(Flights_data)

# Check the diminsions of the data frame
dim(Flights_data) # 336776     19

# Check the column names
colnames(Flights_data)


####################
###  Que 1  ###
####################

# What plane (specified by the tailnum variable) traveled the most times from 
# New York City airports in 2013? 

# Finding unique values in the origin column:
unique(Flights_data$origin) # "EWR" "LGA" "JFK"

# LGA : New York Municipal Airport-LaGuardia Field
# JFK : New York International Airport
# EWR : Newark Liberty International Airport

############################################## -------  From LGA Airport:

From_LGA <-  filter(Flights_data, origin == 'LGA')  %>% count(tailnum)  %>% na.omit()
# View results
From_LGA

# Assigning the column names:
colnames(From_LGA) <- c('Flight_Number', 'Freq')

# using arrange function and omit the NA values
From_LGA <-  arrange(From_LGA, desc(Freq)) %>% head()
head(From_LGA)

# Result:
# N725MQ          567

ggplot(From_LGA, aes(x = Flight_Number, y=Freq )) + 
  geom_bar(aes(fill=Flight_Number), stat="identity", width=0.5)  +
  geom_text(aes(label=Freq), vjust=-0.3) + ggtitle("Top six Flights from LGA Airport")

############################################## -------  From JFK  Airport:

From_JFK <- Flights_data %>% filter(Flights_data$origin == 'JFK') %>% count(tailnum) %>% na.omit()
# View results
max(From_JFK$n, na.rm = T)

# Assigning the column names:
colnames(From_JFK) <- c('Flight_Number', 'Freq')

# using arrange function and omit the NA's values
From_JFK <-  arrange(From_JFK, desc(From_JFK$Freq)) %>% head()
From_JFK
# Result:
# N328AA          393

ggplot(From_JFK, aes(x = Flight_Number, y= Freq)) +geom_bar(aes(fill=Flight_Number), stat="identity", width = 0.5)  +
  geom_text(aes(label = Freq), vjust=-0.3) + ggtitle("Top six Flights from JFK Airport")

############################################## -------  From EWR Airport:

From_EWR <- Flights_data %>% filter(Flights_data$origin == 'EWR') %>% count(tailnum) %>% na.omit()

# View results
From_EWR

# Assigning the column names:
colnames(From_EWR) <- c('Flight_Number', 'Freq')

# using arrange function and omit the NA's values
From_EWR <-  arrange(From_EWR, desc(Freq))  %>% head()
From_EWR
# Result:
#N15980          314

ggplot(From_EWR, aes(x = Flight_Number, y=Freq)) + geom_bar(aes(fill=Flight_Number), stat="identity", width=0.5)  +
  geom_text(aes(label=Freq), vjust=-0.3) + ggtitle("Top six Flights from EWR Airport")



####### ---------  From NYC Airports (EWR, LGA, JFK)

From_NYC <- rbind(head(From_LGA,1), head(From_JFK,1), head(From_EWR,1) )
Origin <- c("LGA","JFK", "EWR")
From_NYC <- cbind(From_NYC, Origin) 
From_NYC
attach(From_NYC)

ggplot(From_NYC, aes(x = Flight_Number, y=Freq)) + geom_bar(aes(fill = Origin), stat="identity", width=0.4)  +
  geom_text(aes(label=Freq), vjust=-0.3) + 
  ggtitle("Flights flew most number of times from all three NYC Airports: LGA, JFK, EWR")


####################
###  Que 2  ###         
####################

# On how many days was there precipitation in the New York area (What are the airports covered in new your area) in 2013? 
# Were there differences in the mean visibility (visib) based on the day of the week and/or month of the year?

# loading the weather data set to 'Weather_data' table:
Weather_data <- weather

# View the data set
View(Weather_data)

attach(Weather_data)

NYC_precp <- Weather_data %>% filter(precip > 0) 

# Total days with precipitation in New York area.
Num_precip.days <- NYC_precp %>% group_by(month) %>%  summarise(Num_Precip.days = length(unique(day)))
Num_precip.days

# Total number of days in NYC had precipitation in 2013
Precip_num.by.year <-  sum(Num_precip.days$Num_Precip.days)
Precip_num.by.year


ggplot(Num_precip.days, aes(x = factor(month), y = Num_Precip.days)) + 
  geom_bar(aes(fill = month), stat="identity", width=0.5) +
  ggtitle("Precipitation recorded in NYC montlhy in the year 2013")


######################################################################################

# Mean visibility based on month

mean.by.month <- function(m){
  res3 <- c()
  for(i in 1:m){
    res1 <-  filter(NYC_precp, NYC_precp$month == i)
    res2 <-  mean(res1$visib, na.rm = T)
    res3 <-  c(res3,res2)
  }      
  return(res3)
}

mean.by.month(max(NYC_precp$month))

#  ----  OR ---- 

########  ------------ Mean visibility based on month of an year:

mean_precip_mon <- NYC_precp %>% group_by(month) %>% summarise(mean_pre.Mon = mean(visib))
mean_precip_mon

ggplot(mean_precip_mon, aes(x = factor(month), y = mean_pre.Mon)) +
  geom_point() + 
  geom_line(aes(color = month, group = 1)) +
  ggtitle("Mean visibility based on Month of the year 2013 in NYC")

########  ------------ Mean visibility based on Day of an week:

mean_precip_day <- NYC_precp %>% group_by(day) %>% summarise(mean_pre.Day = mean(visib))
View(mean_precip_day)

ggplot(mean_precip_day, aes(x = factor(day), y = mean_pre.Day)) +
  geom_point() + 
  geom_line(aes(color = day, group = 1)) + 
  ggtitle("Mean visibility based on days of the month in 2013 in NYC")
 


##### TESTING MEAN OF MONTHS

res1 <-  filter(weather, weather$month == 2)
res2 <- mean(res1$visib, na.rm = T)

####################
###  Que 3.1  ###       
####################


# Loading Team data set 
Team_data <- Teams

# View the Team data
View(Team_data)

# Attaching the "Team_data"
attach(Team_data)

# View the column names:
colnames(Team_data)

###########################################################################
# Check for na's 
sum(is.na(AB)== TRUE)
# Check for 0's in the data
sum(Team_data$AB == 0)
############## Replace with if condition
# Check for na's 
sum(is.na(H)== TRUE)
# Check for 0's in the data
sum(Team_data$H == 0)

##############----- Batting Average(BA), Slugging Percent(SLG) --------#############
# Creating a New column with BA - Batting Average
Team_data <- mutate(Team_data, BA = (H/AB)*100, Total_Bases = (H + (2*X2B) + (3*X3B) + (4*HR)), 
                    SLG_Percent = (Total_Bases/AB)*100)
attach(Team_data)
Team_BA.SLG <- select(Team_data, teamID, BA, SLG_Percent)
head(Team_BA.SLG)

####################
###  Que 3.2  ###       
####################
# Top 15 teams in slugging percentage in MLB history

# Creating a subset in from yearID > 1969
SLG_in_MLB.hist <- subset(Team_data, Team_data$yearID >= 1969)
View(SLG_in_MLB.hist)

# Checking the minimum yearID in the data frame
min(SLG_in_MLB.hist$yearID) #1969
max(SLG_in_MLB.hist$yearID) #2018

# We have data from  1969 to 2018 in the subset.
attach(SLG_in_MLB.hist)
Top15_SLG.in.MLB <- select(SLG_in_MLB.hist, yearID, teamID, SLG_Percent)  %>% as_tibble(SLG_in_MLB.hist) %>% 
  group_by(yearID)  %>% top_n(15, SLG_Percent) 

# Sorting the results based on yearID and SLG_Percent: 
Top15_SLG.in.MLB <- Top15_SLG.in.MLB[order(Top15_SLG.in.MLB$yearID, -Top15_SLG.in.MLB$SLG_Percent), ]
View(Top15_SLG.in.MLB)


####################
###  Que 3.3  ###       
####################

# Grouping by year and summing the Home Runs:
BaseBall.HR.by.year <- Team_data %>% group_by(yearID) %>% summarise(HRs_by_year = sum(HR))

# Ordering the Data Frame based on yearID (Just for safe code): 
BaseBall.HR.by.year[order(BaseBall.HR.by.year$yearID), ]

# This can be done ini couple of ways:

######### ~~~~~~ method 1 ~~~~~~~~ #########  using tibble

# Checking the minimum yearID
min(BaseBall.HR.by.year$yearID) #1871
max(BaseBall.HR.by.year$yearID) #2018

2018 - 1870
148/4 = 37 
## i.e., we have 37 levels of $ year terms.

HR.year.DF_M1 <- tibble(
  Year = BaseBall.HR.by.year$yearID,
  Election = factor(rep(c(1:37), each  = 4, levels = c(1:37))),
  Home_Runs = BaseBall.HR.by.year$HRs_by_year
)

HR.year.DF_M1
dim(HR.year.DF_M1) # 148   3

# Expoting for checking the results -- for my reference :
# write.csv(data.frame(HR.year.DF_M1), "HR.year.DF_M1.csv")
# getwd()

HR_by_Election.M1 <-  HR.year.DF_M1 %>% group_by(Election) %>% summarise(HRs_Term = sum(Home_Runs))
View(HR_by_Election.M1)

# To get the maximum Home Runs scored in a presidential term
HR_by_Election.M1 <- arrange(HR_by_Election.M1, desc(HR_by_Election.M1$HRs_Term))
HR_by_Election.M1

tail(HR_by_Election.M1,1)
tail(HR.year.DF_M1, 4)

# Most runs are scored in 37th presidential term.

######### ~~~~~~ END of method 1  ~~~~~~~~ ######### 

######### ~~~~~~ method 2 ~~~~~~~~ #########  using mutate and rep function

HR.year.DF_M2 <- BaseBall.HR.by.year %>% mutate(Election = factor(rep(c(1:37), each  = 4, levels = c(1:37))))
HR.year.DF_M2
colnames(HR.year.DF_M2)

HR_by_Election_M2 <-  HR.year.DF_M2 %>% group_by(Election) %>% summarise(HRs_Term = sum(HRs_by_year))
View(HR_by_Election_M2)

# To get the maximum Home Runs scored in a presidential term
HR_by_Election_M2 <- arrange(HR_by_Election_test, desc(HR_by_Election_test$HRs_Term))
HR_by_Election_M2

tail(HR_by_Election_M2, 1)
tail(HR.year.DF_M2, 4)

# Most runs are scored in 37th presidential term.


######### ~~~~~~ END of method 2  ~~~~~~~~ ######### 

