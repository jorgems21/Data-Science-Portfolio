
#### Nashville Housing Analysis

### Author: Jorge Montoya
### Domain: Real Estate
### Timeframe: Academic Project (Northeastern University)
### Date: December 2022
### Last Revision Date: January 2025
### Project Description:
##### This project looks to identify what is the likelihood of finding a good deal 
##### for home purchasing. The Nashville Housing dataset provided includes information from 2013-2016.
#### Notes:
##### Last revision was made to add header information in this file prior to uploading to Github.


install_required_packages <- function() {
  required_packages <- c("shiny", "tidyverse", "caret", "rpart", "randomForest", "gbm", "ggplot2")
  installed_packages <- rownames(installed.packages())
  to_install <- setdiff(required_packages, installed_packages)
  if (length(to_install) > 0) {
    install.packages(to_install)
  }
  lapply(required_packages, library, character.only = TRUE)
}

install_required_packages()

library(shiny)
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
library(gbm)
library(ggplot2)

# Load dataset
df <- read.csv('data/Nashville_housing_data_2013_2016.csv')

# Remove rows with missing values except for 'Suite/ Condo   #' column
# Remove rows with missing values
df <- df %>% drop_na()

