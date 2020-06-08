#loading necessary packages
library(tidyr)
library(ggplot2)
library(magrittr)
library(readr)
library(dplyr)

# Install historydata package
install.packages("historydata")

# Load historydata package
library(historydata)

data("early_colleges")

#viewing the data
early_colleges

#changing Harvard's sponsorship to 'Congregational'

early_colleges[1,6]<-"Congregational"

#getting rid of the original_name column

early_colleges%>%
  dplyr::select(-original_name)

#adding a column that combines state and city together and save to location

early_colleges%>%mutate(location=paste(city,state, sep=","))

early_colleges_with_location<-early_colleges%>%
  mutate(location=paste(city, state, sep=","))%>%
  dplyr::select(-city)%>%
  dplyr::select(-state)%>%
  dplyr::select(-original_name)

early_colleges_with_location

#arrange the data in a new way using arrange function in dplyr

early_colleges_with_location %>%
  arrange(desc(established))

early_colleges_with_location%>%summarise(mean(established))

#view final cleansed data

early_colleges_clean<-early_colleges_with_location
View(early_colleges_clean)
