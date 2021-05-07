library(dplyr)
library(tidyr)
library(ggplot2)
library(lintr)

# load in data
suicide_year <- read.csv("../data/national/Age-standardized suicide rates.csv",
                         stringsAsFactors = FALSE)
suicide_crude <- read.csv("../data/national/Crude suicide rates.csv",
                         stringsAsFactors = FALSE)
facilities <- read.csv("../data/national/Facilities.csv", stringsAsFactors = FALSE)
hr <- read.csv("../data/national/Human Resources.csv", stringsAsFactors = FALSE)


suicide_year$Sex <- trimws(suicide_year$Sex, which = c("both"))

suicide_year_gathered <- gather(suicide_year, "year", "value", 3:6)

# summaries and distribution of variables
age_summary <- suicide_year_gathered %>%
    summarize(
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value)
  )

year_summary <- suicide_year_gathered %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value)
  )
  
sex_summary <- suicide_year_gathered %>%
  group_by(Sex) %>%
  summarize(
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value)
  )

# filter data to plot
sex_year_mean <- suicide_year_gathered %>%
  group_by(Sex, year) %>%
  summarize(
    mean = mean(value)
  )

mean_suicide_gender <- ggplot(sex_year_mean, aes(x = year, y = mean,
                                                 fill = Sex)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_discrete(labels = c("2000", "2010", "2015", "2016")) +
  scale_fill_manual(labels = c("Males", "Females", "Both Sexes"),
                    values = c("dodgerblue", "indianred1", "seagreen2")) +
  labs(title = "Mean Age-Standardized Suicide Rates over time by Sex",
       x = "Year",
       y = "Mean Age-Standardized Suicide Rates")
