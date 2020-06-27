
# Set the path
getwd()
setwd("E:/ICT583/Assignments/Rajesh/Excersise2/Exercise_2")

# Installing masaicData
install.packages("mosaicData") ## for Galton data
library(mosaicData)

install.packages("ggplot2") ## fOR visualizing
library(ggplot2)

install.packages("dplyr") ## for data manipulation 
library(dplyr)

install.packages("nasaweather") ### -- For storms data
library(nasaweather)

install.packages("macleish") ### -- for whately_2015  data 
library(macleish)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Question 1

# Loadng heights data into "Galton_HT_Data"  data frame
Galton_HT_Data <- Galton

View(Galton_HT_Data)

# Checking columns of the data frame.
colnames(Galton_HT_Data)

str(Galton_HT_Data)

attach(Galton_HT_Data)


# 1.1. Create a scatterplot of each male child’s height against their mother’s height
Male_data <- Galton_HT_Data %>% filter(sex == 'M') 
View(Male_data)

ggplot(Male_data, aes(x = height, y = mother)) + geom_point() + 
  labs(title="Heights of mother's Vs son's",x="Height of son",y="Height of mother",caption="By- Rajesh RJ") + 
  theme(plot.title = element_text(family="Roboto Condensed Bold"),
        plot.caption = element_text(color="#AAAAAA", size=10)) + theme_minimal(base_size=9)

# 1.2. Separate your plot into facts by nkids
ggplot(Male_data, aes(x = height, y = mother)) + geom_point(aes(col = factor(nkids))) + facet_wrap(~nkids) + 
   labs(title="Heights of mother's Vs son's",x="Height of son",y="Height of mother",caption="By- Rajesh RJ") + 
  theme(plot.title = element_text(family="Roboto Condensed Bold"),
        plot.caption = element_text(color="#AAAAAA", size=10)) + theme_minimal(base_size=9)


# 1.3. Add regression lines to all your facts
ggplot(Male_data, aes(x = height, y = mother)) + geom_point(aes(col = factor(nkids))) + facet_wrap(~nkids) +  
  stat_smooth(method = "lm", se = F, col = "deeppink3") + theme_minimal(base_size=9) + 
  labs(title="Heights of mother's Vs son's",x="Height of son",y="Height of mother",caption="By- Rajesh RJ") + 
  theme(plot.title = element_text(family="Roboto Condensed Bold"),
        plot.caption = element_text(color="#AAAAAA", size=10)) + theme_minimal(base_size=9)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Question 2
# View the storms data
storms_NASA <- nasaweather :: storms  ## Since dplyr package is also having a storms data with different variables
View(storms_NASA)

# Check the data types of the dataframe.
str(storms_NASA)

# Check the column names of the dataframe
colnames(storms_NASA)
?nasaweather :: storms

attach(storms_NASA)

# 2.1 create a scatterplot between wind and pressure, with color being used to distinguish the type of storm
ggplot(storms_NASA, aes(x = pressure, y = wind)) + geom_point(aes(col = factor(type)))


# 2.2 You might notice there are lots of overlapping data points in the scatterplot 
# due to a comparatively large sample size, How would you improve your visualization? 

## using facet_wrap()
# ggplot(storms_NASA, aes(x = pressure, y = wind)) + geom_point(aes(col = factor(type))) + facet_grid(~ type) + 
#  labs(title="Pressure VS Wind for Different storms",x="Pressure",y="Wind",caption="By- Rajesh RJ") + 
#  theme(plot.title = element_text(family="Roboto Condensed Bold"),
#        plot.caption = element_text(color="#AAAAAA", size=10)) + theme_minimal(base_size=9)

## ------------- or 

ggplot(storms_NASA, aes(x = pressure, y = wind)) + geom_point(aes(col = factor(type))) + facet_wrap(~ type) + 
  labs(title="Pressure VS Wind for Different storms",x="Pressure",y="Wind",caption="By- Rajesh RJ") + 
  theme(plot.title = element_text(family="Roboto Condensed Bold"),
        plot.caption = element_text(color="#AAAAAA", size=10)) + theme_minimal(base_size=9)
  



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Question 3
## reading data description 
?whately_2015

# Loading "whately_2015" to MFS_wether_data dataframe 
MFS_wether_data <-  whately_2015 %>% na.omit()

# check the dataset
View(MFS_wether_data)

colnames(MFS_wether_data)

# Check the data type of the data
str(MFS_wether_data)


write.csv(MFS_wether_data, "MFS_wether_data.csv")

attach(MFS_wether_data)

Weather_Connect_lines <- ggplot(MFS_wether_data, aes(x = when, y = temperature)) + geom_point() + geom_path()
Weather_Connect_lines + geom_smooth() + labs(title="Graphic The average temperature over each 10-minute interval",
                                             x="Day of a year", y="Temperature", caption="By- Rajesh RJ") + theme_minimal(base_size=9)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Question 4

### Loading data to "TS_COVID19" dataframe
TS_COVID19 <- read.csv("time_series_covid_19_confirmed.csv")

# Checking the NA values.
which(is.na(TS_COVID19$X3.23.20))

# Replacing NA's with Zeros, so that we get the exact number of cases
TS_COVID19$X3.23.20 <- TS_COVID19$X3.23.20 %>% replace_na(0)


# NA are replaced with Zeros for NO NA's in this column
which(is.na(TS_COVID19$X3.23.20))

## attach the dataframe to the R session
attach(TS_COVID19)

## select country | latest date column | group by country | order | Top 10
COVID19_COUNTRY <-  TS_COVID19 %>% select(Country.Region, X3.23.20) %>% group_by(Country.Region) %>% summarise(COVID_cases = sum(X3.23.20))

COVID19_COUNTRY_Top10 <- arrange(COVID19_COUNTRY, desc(COVID19_COUNTRY$COVID_cases)) %>% head(10)
COVID19_COUNTRY_Top10

write.csv(COVID19_COUNTRY, "E:/ICT583/Assignments/Rajesh/Excersise2/Exercise_2COVID19_COUNTRY.csv")

# COVID19_COUNTRY <- arrange(COVID19_COUNTRY, desc(X3.23.20))
ggplot(COVID19_COUNTRY_Top10, aes(x = reorder(Country.Region, -COVID_cases), y = COVID_cases, fill = Country.Region)) + 
  geom_bar(stat = "identity") + theme_minimal(base_size=9) + 
  labs(title="TOP 10 COUNTRIES EFFECTED WITH COVID19",
       subtitle="As per 23rd March 2020",
       x="COUNTRIES", 
       y="TOTAL NUMBER OF CASES",
       caption="By- Rajesh RJ") +
  theme(plot.subtitle = element_text(color="#666666"),
        plot.title = element_text(family="Roboto Condensed Bold"),
        plot.caption = element_text(color="#AAAAAA", size=10))




