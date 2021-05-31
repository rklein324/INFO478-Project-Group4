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
human_resources <- subset(human_resources, select = c("Country", "Psychiatrists", "Nurses", "Psychologists"))

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
plot_data <- inner_join(plot_data, human_resources)

# ------- INTERACTIVE VISUALIZAION PLOT ------- 
server <- function(input, output) {
  output$viz1 <- renderPlot({
    # p <- plot_ly(
    #   data = plot_data, 
    #   mode = "markers", 
    #   type = "scatter",
    #   x = ~suicide_rate, 
    #   y = ~input$facility_type
    # ) %>% layout(
    #   title = "Suicide rates vs Facilities",
    #   xaxis = list(title = "Suicide Rates"),
    #   yaxis = list(title = "Number of Facilities")
    # )
    p <- ggplot(
      data = plot_data,
      mapping = aes_string(x = "suicide_rate", y = input$facility_type)
    ) +
      geom_point() +
      geom_smooth(mapping = aes_string(x = "suicide_rate", y = input$facility_type)) +
      geom_text(label=plot_data$Country, nudge_y = 0.2) +
      xlab("Suicide Rates") +
      ylab("Number of Facilities")
    #   labs(
    #   title = "Suicide rates vs Facilities",
    #   xlab = "Suicide Rates",
    #   ylab = "Number of Facilities"
    # )
    return(p)
  })
}
