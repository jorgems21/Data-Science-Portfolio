library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggvis)
library(RColorBrewer)
library(DT)
library(tidyr)
library(reshape2)
library(ggthemes)
library(zoo)
library(RColorBrewer)
library(ggmap)
library(shinydashboard)
library(dplyr)

df <- Crime_Incident_Reports_August_2015_September_2020_
# 
str(df)
is.na(df)
summary(df)
unique(df$OFFENSE_CODE_GROUP)

unique(df$DISTRICT)

#Cleaning NA's from df and drop other cols
clean_df <- df %>% drop_na(OFFENSE_CODE_GROUP, DISTRICT)

clean_df$SHOOTING[is.na(clean_df$SHOOTING)]<- 0

clean_df$SHOOTING[clean_df$SHOOTING=="Y"] <- 1

clean_df <- clean_df %>% subset(select = -c(INCIDENT_NUMBER,REPORTING_AREA, UCR_PART))

#Map 
