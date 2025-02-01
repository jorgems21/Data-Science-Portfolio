
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
  required_packages <- c("shiny", "tidyverse", "caret", "rpart", "randomForest","plotly",
                         "gbm", "ggplot2","xgboost", "bslib","DT","tidyr","markdown","officer")
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
library(xgboost)
library(randomForest)
library(ggplot2)
library(bslib)
library(dplyr)
library(DT)
library(tidyr)
library(markdown)
library(officer)
library(plotly)

source("functions/global_functions.R")

# Load dataset
file_path <- 'data/Nashville_housing_data_2013_2016.csv'

df <- load_and_preprocess_data(file_path)
getwd()




