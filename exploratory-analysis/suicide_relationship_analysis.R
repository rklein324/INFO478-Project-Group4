# Load libraries
library(dplyr)
library(skimr)
library(plotly)
library(reshape)

# Load data
suicide_data <- read.csv("../data/national/Age-standardized suicide rates.csv",
                         check.names = F)
crude_suicide_data <- read.csv("../data/national/Crude suicide rates.csv",
                               check.names = F)
facilities_data <- read.csv("../data/national/Facilities.csv")
HR_data <- read.csv("../data/national/Human Resources.csv", check.names = F)

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

# Process data for graphics

suicide_data_2016 <- suicide_data[c("Country", "Sex", "2016")]
suicide_data_2016 <- dplyr::rename(suicide_data_2016, suicide_rate = "2016")
joined_suicide_data <- merge(suicide_data_2016,
                             subset(facilities_data, select = -c(Year)),
                             by = "Country")
joined_suicide_data <- merge(joined_suicide_data,
                             subset(HR_data, select = -c(Year)),
                             by = "Country")

long_crude_suicide_data <- melt(crude_suicide_data, id = c("Country", "Sex"))

joined_crude_suicide_data <- merge(long_crude_suicide_data,
                                   subset(facilities_data, select = -c(Year)),
                                   by = "Country")
joined_crude_suicide_data <- merge(joined_crude_suicide_data,
                                   subset(HR_data, select = -c(Year)),
                                   by = "Country")

# Create plot

suicide_relationship_plot <- plot_ly(
    data = joined_suicide_data,
    type = "scatter",
    x = ~suicide_rate,
    y = ~Psychiatrists,
    mode = "markers",
    text = paste("Suicide Rate :", joined_suicide_data$suicide_rate,
                 "<br>Country :", joined_suicide_data$Country),
    hoverinfo = "text",
    transforms = list(
      list(
        type = "filter",
        target = ~Sex,
        operation = "=",
        value = unique(joined_suicide_data$Sex)[1]
      ))) %>%
  layout(
    title = "Suicide Rates vs Facility and Human Resources",
    xaxis = list(title = "Suicide Rate"),
    yaxis = list(title = "Psychiatrists per 100,000 pop"),
    updatemenus = list(
      list(
        y = 1,
        buttons = list(
          list(method = "update",
            args = list(list(y = list(joined_suicide_data$Psychiatrists)),
                  list(yaxis = list(title = "Psychiatrists per 100,000 pop"))),
            label = "Psychiatrists"),
          list(method = "update",
               args = list(list(y = list(joined_suicide_data$Nurses)),
                      list(yaxis = list(title =
                                  "Mental Health Nurses per 100,000 pop"))),
               label = "Nurses"),
          list(method = "update",
            args = list(list(y = list(joined_suicide_data$Mental._hospitals)),
                list(yaxis = list(title = "Mental Hospitals per 100,000 pop"))),
            label = "Mental Hospitals"),
          list(method = "update",
            args = list(list(y = list(joined_suicide_data$health_units)),
                    list(yaxis = list(title = "Health Units per 100,000 pop"))),
            label = "Health Units"),
          list(method = "update",
            args = list(list(y =
                        list(joined_suicide_data$outpatient._facilities)),
                  list(yaxis = list(title =
                              "Outpatient Facilities per 100,000 pop"))),
            label = "Outpatient Facilities"))),
      list(
        y = .9, # depending on the size of the plot, this should be changed
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value",
                           unique(joined_suicide_data$Sex)[1]),
               label = unique(joined_suicide_data$Sex)[1]),
          list(method = "restyle",
               args = list("transforms[0].value",
                           unique(joined_suicide_data$Sex)[2]),
               label = unique(joined_suicide_data$Sex)[2]),
          list(method = "restyle",
               args = list("transforms[0].value",
                           unique(joined_suicide_data$Sex)[3]),
               label = unique(joined_suicide_data$Sex)[3])
        )
      )
    ))
