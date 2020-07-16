# Data Wrangling in R
# African Economic Outlook

# Loading libraries
library(tidyverse)
library(readxl)

# reading file into R

file<-"/Users/aimemukenajr/Desktop/Data Wrangling Essentials/Data Wrangling R/african-economic-outlook-january-2019-xlsx-1.xlsx"

africa_data <- read_excel(file)

# first look at the dataset

glimpse(africa_data)
view(africa_data)

# dataset is not tidy: violation of the tidy rules: variables containing more than one obs
# dataset is too wide, needs to be transformed

# start with changing some column names

colnames(africa_data)[1] <- "CountryRegions"
colnames(africa_data)[2] <- "CountryRegionsNames"
colnames(africa_data)[3] <- "ID"
colnames(africa_data)[5] <- "IndicatorsName"

# make dataset long

africa_long <- gather(africa_data, Year, GDP, - CountryRegions, - CountryRegionsNames, - ID, - IndicatorsName, - Scale, - Units, - Indicators)

# Check if the changes registered
print(africa_long)

# Let's subset the dataset so that it shows one indicator only: 'Real GDP growth (annual %)'

africa_subset <- subset(africa_long, IndicatorsName == 'Real GDP growth (annual %)')

glimpse(africa_subset)

# there are columns that aren't useful for the sake of this analysis so let's drop them

africa2 <- select(africa_subset, - CountryRegions, - ID, - Indicators, - IndicatorsName, - Scale, - Units)

glimpse(africa2)

# now we are down to 3 variables only
# let's look at unique values in the 'CountryRegionsNames' column

unique(africa2$CountryRegionsNames)

# it seems like there's a violation of tidy rules here: countries and regions are listed together
# let's clean this variable to only contain country names

# let's start with creating a vector of region names
noncountries<- c("East Africa", "Sub-Saharan Africa", "North Africa", "Africa", "West Africa", "Southern Africa",
                 "Central Africa")

# let's use the match function to find noncountries
# wrap the function in !is.na() to exclude NA
# wrap the function in the which() function and save it to matches

matches <- which(!is.na(match(africa2$CountryRegionsNames, noncountries)))

# create a tibble containing rows with only countries excluding continents

africa_clean <- africa2[-matches,]
africa_region <- africa2[matches,]

unique(africa_region$CountryRegionsNames)

# let's check if changes registered
unique(africa_clean$CountryRegionsNames)

# rename column name
colnames(africa_clean)[1]<- "Country"
colnames(africa_clean)[3]<- "Growth_rate"

#let's check changes
glimpse(africa_clean)

# Year has the wrong data type, let's fix it

africa_clean$Year <- as.numeric(africa_clean$Year)

# let's get a different perspective of the data by using the summary() function

summary(africa_clean)

# Outliers

#using boxplot to visualize outliers 

ggplot(data=africa_clean)+
  geom_boxplot(mapping = aes("Outliers", Growth_rate))

subset(africa_clean, Growth_rate > 100)

# Research corroborates that Libya and Eq Guinea had a growth rate > 100 in 2012 and 1997

subset(africa_clean, Growth_rate < -50)

# Research corroborates that Libya and South Sudan  had a growth rate < -50 in 2011,2012,2014

# Missing Values
# there are 127 missing values

# create vector with rows containing missing values
no_records<-which(is.na(africa_clean$Growth_rate))

# access rows which contain missing values
africa_clean[no_records,]

# create new tibble excluding missing values
africa_tidy<-africa_clean[-no_records,]

summary(africa_tidy)

# dataset is tidy and ready for analysis

# simple line graph to visualize data

ggplot(africa_tidy, mapping = aes(x= Year, y= Growth_rate))+
  geom_line(mapping=aes(color=Country))





