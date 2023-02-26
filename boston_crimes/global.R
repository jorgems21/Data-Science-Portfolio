library(shiny)
library(shinydashboard)
library(shinythemes)
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
library(hrbrthemes)

df <- Crime_Incident_Reports_August_2015_September_2020_
# 
# str(clean_df)
# is.na(df)
# summary(df)
# unique(df$OFFENSE_CODE_GROUP)
# 
# unique(df$DISTRICT)

#Cleaning NA's from df and drop other cols
clean_df <- df %>% drop_na(OFFENSE_CODE_GROUP, DISTRICT)

clean_df$SHOOTING[is.na(clean_df$SHOOTING)]<- 0

clean_df$SHOOTING[clean_df$SHOOTING=="Y"] <- 1

clean_df <- clean_df %>% subset(select = -c(INCIDENT_NUMBER,REPORTING_AREA, UCR_PART, OFFENSE_CODE))

#Map 
clean_df$OFFENSE_CODE_GROUP<- as.factor(clean_df$OFFENSE_CODE_GROUP)

clean_df$DISTRICT <- gsub('A1', 'Downtown', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('A15', 'Charlestown', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('A7', 'East Boston', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('B2', 'Roxbury', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('B3', 'Mattapan', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('C6', 'South Boston', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('C11', 'Dorchester',clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('D4', 'South End', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('D14', 'Brighton', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('E18', 'Hyde Park', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('E13', 'Jamaica Plain', clean_df$DISTRICT)
clean_df$DISTRICT <- gsub('E5', 'West Roxbury', clean_df$DISTRICT)

districts <- unique(clean_df$DISTRICT)

clean_df$OCCURRED_ON_DATE <- as.Date(clean_df$OCCURRED_ON_DATE)
#Top 10 crimes
top10crimes <- clean_df %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  slice(1:10)

filter(clean_df, OFFENSE_CODE_GROUP == top10crimes$OFFENSE_CODE_GROUP)
# d <- filter(myDf, OFFENSE_CODE_GROUP == "Robbery" | 
#               OFFENSE_CODE_GROUP == "Homicide" | 
#               OFFENSE_CODE_GROUP == "Residential Burglary" |
#               OFFENSE_CODE_GROUP == "Aggravated Assault" |
#               OFFENSE_CODE_GROUP == "Simple Assault" |
#               OFFENSE_CODE_GROUP == "Other Burglary" |
#               OFFENSE_CODE_GROUP == "HOME INVASION" |
 #             OFFENSE_CODE_GROUP == "Burglary - No Property Taken")