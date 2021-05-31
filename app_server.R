# load packages 
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyverse)


# load data
age_standardized_suicide <- read.csv("./data/national/Age-standardized suicide rates.csv")
facilities <- read.csv("./data/national/Facilities.csv")
human_resources <- read.csv("./data/national/Human Resources.csv")

# ------- CLEAN DATA --------- 
# remove leading & trailing spaces
age_standardized_suicide$Sex <- trimws(age_standardized_suicide$Sex, which = c("both"))

# drop unused variables
facilities <- subset(facilities, select = c("Country", "Mental._hospitals", "outpatient._facilities", "health_units"))
human_resources <- subset(human_resources, select = c("Country", "Psychiatrists", "Nurses", "Psychologists", "Social_workers"))

# remove missings
facilities <- facilities %>% 
  drop_na()

human_resources <- human_resources %>% 
  drop_na()

# rename variables
facilities <- facilities %>% 
  rename("mental_hospitals" = "Mental._hospitals",
         "outpatient_facilities" = "outpatient._facilities")

age_standardized_suicide <- age_standardized_suicide %>% 
  filter(Sex == "Both sexes") %>% 
  rename("suicide_rate" = X2016) %>% 
  select(Country, "suicide_rate")

# join data
plot_data <- inner_join(age_standardized_suicide, facilities)
join_data <- inner_join(age_standardized_suicide, human_resources, by = "Country")
plot_data <- inner_join(plot_data, human_resources)

# calculate weight for each resource
max_suicide <- plot_data %>%
  filter(suicide_rate == max(suicide_rate))
max_mental_hospitals <- plot_data %>%
  filter(mental_hospitals == max(mental_hospitals))
max_outpatient_facilities <- plot_data %>%
  filter(outpatient_facilities == max(outpatient_facilities))
max_health_units <- plot_data %>%
  filter(health_units == max(health_units))
max_Psychiatrists <- plot_data %>%
  filter(Psychiatrists == max(Psychiatrists))
max_Nurses <- plot_data %>%
  filter(Nurses == max(Nurses))
max_Psychologists <- plot_data %>%
  filter(Psychologists == max(Psychologists))

resources <- c("mental_hospitals", "outpatient_facilities", "health_units", "Psychiatrists", "Nurses", "Psychologists")

resource_a <- c(max_suicide$mental_hospitals,
                max_suicide$outpatient_facilities,
                max_suicide$health_units,
                max_suicide$Psychiatrists,
                max_suicide$Nurses,
                max_suicide$Psychologists)

suicide_a <- max_suicide$suicide_rate

resource_b <- c(max_mental_hospitals$mental_hospitals,
                max_outpatient_facilities$outpatient_facilities,
                max_health_units$health_units,
                max_Psychiatrists$Psychiatrists,
                max_Nurses$Nurses,
                max_Psychologists$Psychologists)

suicide_b <- c(max_mental_hospitals$suicide_rate,
               max_outpatient_facilities$suicide_rate,
               max_health_units$suicide_rate,
               max_Psychiatrists$suicide_rate,
               max_Nurses$suicide_rate,
               max_Psychologists$suicide_rate)

resource_weights <- data.frame(resource_a, suicide_a, resource_b, suicide_b)
row.names(resource_weights) <- resources

resource_weights <- resource_weights %>%
  mutate(weight = (suicide_b - suicide_a) / (resource_b - resource_a) * -1)

# add aggregated columns for resource type
plot_data <- plot_data %>%
  mutate(total_facilities = mental_hospitals + outpatient_facilities + health_units) %>%
  mutate(total_hr = Psychiatrists + Nurses + Psychologists) %>%
  mutate(total_facilities_wt = (mental_hospitals * resource_weights$weight[[1]]) + (outpatient_facilities * resource_weights$weight[[2]]) + (health_units * resource_weights$weight[[3]])) %>%
  mutate(total_hr_wt = (Psychiatrists * resource_weights$weight[[4]]) + (Nurses * resource_weights$weight[[5]]) + (Psychologists * resource_weights$weight[[6]]))


# ------- INTERACTIVE VISUALIZAION PLOT ------- 
server <- function(input, output) {
  output$viz1 <- renderPlot({
    p <- ggplot(
      data = plot_data,
      mapping = aes_string(x = "suicide_rate", y = input$facility_type)
    ) +
      geom_point() +
      # geom_smooth(mapping = aes_string(x = "suicide_rate", y = input$facility_type)) +
      geom_text(label=plot_data$Country, nudge_y = 0.2) +
      xlab("Suicide Rates") +
      ylab("Number of Facilities")
    return(p)
  })
  
  output$viz2 <- renderPlot({
    p <- ggplot(
      data = join_data,
      mapping = aes_string(x = "suicide_rate", y = input$human_resources)
    ) +
      geom_point() +
      geom_smooth(mapping = aes_string(x = "suicide_rate", y = input$human_resources)) +
      geom_text(label=join_data$Country, nudge_y = 0.2) +
      xlab("Suicide Rates") +
      ylab("Number of Human Resources")
    return(p)
  })
  
  output$viz3 <- renderPlot({
    p <- ggplot(
        data = plot_data,
        mapping = aes_string(x = input$total_selection, y = "suicide_rate")
      ) +
        geom_point() +
        geom_smooth(mapping = aes_string(x = input$total_selection, y = "suicide_rate")) +
        geom_text(label=plot_data$Country, nudge_y = 0.2) +
        xlab("Number of Resources") +
        ylab("Suicide Rates")
      return(p)
  })
}
