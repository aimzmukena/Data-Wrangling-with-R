# Data Wrangling in R
# Apple Mobility

# load libraries 

library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)

# load file into environment

file<- "/Users/aimemukenajr/Downloads/applemobilitytrends-2020-04-14.csv"

apple <- read_csv(file)

# let's look at the dataset
glimpse(apple)

# let's start with making the dataset long
applelong <- gather(apple, date, mobility_trends, - geo_type, - region, - transportation_type)

# checking results
print(applelong)

# statistical summary of data
summary(applelong)

# we have correct data types and no missing values

# outliers analysis

# let's plot a boxplot to visualize outliers
ggplot(data=applelong)+
  geom_boxplot(mapping = aes("Outliers", mobility_trends))

#use subset() function to find values above 300 
subset(applelong, mobility_trends > 300)

# it seems like mobility was high in Spain and Slovenia prior to states of emergency


# inspect the region column
unique(applelong$region)

#we have cities and countries in the same column in violation of tidy rules

#let's create a vector containing all country names in the dataset

countries <- c("Albania", "Argentina", "Australia", "Austria", "Belgium",
               "Brazil", "Bulgaria", "Cambodia", "Canada", "Chile", "Colombia",
               "Croatia", "Czech Republic", "Denmark", "Egypt", "Estonia",
               "Finland", "France", "Germany", "Greece", "Hong Kong", "Hungary",
               "Iceland", "India", "Indonesia", "Ireland", "Israel", "Italy",
               "Japan", "Latvia", "Lithuania", "Luxembourg", "Macao", "Malaysia",
               "Mexico", "Morocco", "Netherlands", "New Zealand", "Norway",
               "Philippines", "Poland", "Portugal", "Republic of Korea", "Romania",
               "Russia", "Saudi Arabia", "Serbia", "Singapore", "Slovakia",
               "Slovenia", "South Africa", "Spain", "Sweden", "Switzerland",
               "Taiwan", "Thailand", "Turkey", "UK", "United States", "Ukraine",
               "United Arab Emirates", "Uruguay", "Vietnam")

# let's then use the match() function and !is.na() function wrapped in the which() function to locate all country values
# save it to "matches"
matches <- which(!is.na(match(applelong$region, countries)))

# create a tibble with cities and a tibble with countries
apple_city <- applelong[-matches,]

apple_country <- applelong[matches,]

# rename column "country/region" to "city"
colnames(apple_city)[2] <- "city"

# rename column "country/region" to "country"
colnames(apple_country)[2] <- "country"

apple_city
subset(apple_country, mobility_trends > 300 & transportation_type == "walking")

# let's drop columns we don't need
apple_city <- apple_city[-c(1)]
apple_country <- apple_country[-c(1)]

# arrange dates in desc format

apple_city %>% 
  arrange(desc(date))


