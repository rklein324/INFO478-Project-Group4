# Load libraries
library(dplyr)
library(skimr)

# Load data
suicide_data <- read.csv("../data/national/Age-standardized suicide rates.csv", check.names=F)
crude_suicide_data <- read.csv("../data/national/Crude suicide rates.csv", check.names=F)
facilities_data <- read.csv("../data/national/Facilities.csv")
HR_data <- read.csv("../data/national/Human Resources.csv", check.names=F)

# Analyze data
skim(suicide_data)
skim(crude_suicide_data)
skim(facilities_data)
skim(HR_data)

# Notes

# suicide_data:
# age-standardized data on suicide rates per 
 # -country (183)
 # -sex (male, female, both)
 # -year (2000, 2010, 2015, 2016)
# skewed right
# mean over the years is increasing
# one row has 0 value (Antigua and Barbuda), 
  # probably because of rounding/smaller population
# no missing values

# crude_suicide_data:
# suicide rate per 100,000 population per
 # -country (183)
 # -sex (male, female, both)
 # -age (80_above, 70to79, 60to69, 50to59, 40to49, 30to39, 20to29, 10to19)
# does not include the (relatively few) suicides below age of 10
# skewed right
# mean increases as age increases
# multiple rows with 0 values, probably because of rounding/smaller populations
# no missing values

# facilities_data:
# number of specific facilities per 100,000 population per country (112)
 # -mental hospitals (22 missing, 80.4% complete)
 # -health units (10 missing, 91.1% complete)
 # -outpatient facilities (12 missing, 89.3% complete)
 # -day treatment facilities (61 missing, 45.5% complete, won't include)
 # -residential facilities (67 missing, 40.2% complete, won't include)
# heavily skewed right
# one row has 0 value (Tonga), probably because of rounding/smaller population

# HR_data:
# number of human resources working in mental health sector 
  #per 100,000 population per country (107)
# -Psychiatrists (3 missing, 97.2% complete)
# -Nurses (16 missing, 85.0% complete)
# -Social workers (39 missing, 63.6% complete, won't include)
# -Psychologists (23 missing, 78.5% complete, may not include)
# heavily skewed right
# two rows has 0 value
 # -Guyana: psychologists, probably because of rounding 
            # (currently only have 27 psychologists and psychiatrists)
 # -Columbia: nurses, reason is unknown - possibly accurate
