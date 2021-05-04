# Load libraries
library(dplyr)
library(skimr)

# Load data
suicide_data <- read.csv("../data/national/Age-standardized suicide rates.csv", check.names=F)
crude_suicide_data <- read.csv("../data/national/Crude suicide rates.csv", check.names=F)
facilities_data <- read.csv("../data/national/Facilities.csv")
HR_data <- read.csv("../data/national/Human Resources.csv", check.names=F)

# Analyze data
summary(suicide_data)
summary(crude_suicide_data)
summary(facilities_data)
summary(HR_data)

skim(suicide_data)
skim(crude_suicide_data)
skim(facilities_data)
skim(HR_data)
