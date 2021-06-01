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

# calculate weight for each resource
resources <- c("mental_hospitals", "outpatient_facilities", "health_units", "Psychiatrists", "Nurses", "Psychologists")

resource_weights <- data.frame(x1=double(), y1=double(), x2=double(), y2=double())

for (r in resources) {
  max_suicide <- plot_data %>%
    filter(!is.na(plot_data[r])) %>%
    filter(suicide_rate == max(suicide_rate, na.rm = TRUE))
  max_r <- plot_data %>%
    filter(plot_data[r] == max(plot_data[r], na.rm = TRUE))
  resource_weights[r, "x1"] <- max_suicide$suicide_rate
  resource_weights[r, "y1"] <- max_suicide[r]
  resource_weights[r, "x2"] <- max_r$suicide_rate
  resource_weights[r, "y2"] <- max_r[r]
}

resource_weights <- resource_weights %>%
  mutate(weight = (y2 - y1) / (x2 - x1) * -1)

# add aggregated columns for resource type
plot_data_totals <- plot_data %>%
  drop_na() %>%
  mutate(total_facilities = mental_hospitals + outpatient_facilities + health_units) %>%
  mutate(total_hr = Psychiatrists + Nurses + Psychologists) %>%
  mutate(total_facilities_wt = (mental_hospitals * resource_weights$weight[[1]]) + (outpatient_facilities * resource_weights$weight[[2]]) + (health_units * resource_weights$weight[[3]])) %>%
  mutate(total_hr_wt = (Psychiatrists * resource_weights$weight[[4]]) + (Nurses * resource_weights$weight[[5]]) + (Psychologists * resource_weights$weight[[6]]))

# ------- INTERACTIVE VISUALIZAION PLOT ------- 
server <- function(input, output) {
  output$viz1 <- renderPlot({
    x_offset = 0.0
    if(input$facility_type == "mental_hospitals") {
      x_offset = 0.4
    }

    if(input$facility_type == "health_units") {
      x_offset = 0.15
    }

    if(input$facility_type == "outpatient_facilities") {
      x_offset = 1
    }

    
    viz_data <- subset(plot_data, select = c("Country", "suicide_rate", input$facility_type))
    viz_data <- viz_data %>% 
        drop_na()
    
    
    line_plot_data_mental_hospitals <- plot_data %>% 
      select(Country, suicide_rate, mental_hospitals) %>% 
      drop_na() %>% 
      filter(Country == "Guyana" | Country == "Japan")
      
    line_plot_data_health_units <- plot_data %>% 
      select(Country, suicide_rate, health_units) %>% 
      drop_na() %>% 
      filter(Country == "Guyana" | Country == "Hungary")
      
    line_plot_data_outpatient_facilities <- plot_data %>% 
      select(Country, suicide_rate, outpatient_facilities) %>%
      drop_na() %>% 
      filter(Country == "Guyana" | Country == "Saint Lucia")
      
    
    p <- ggplot(
      data = viz_data,
      mapping = aes_string(x = input$facility_type, y = "suicide_rate")
    ) +
      geom_point() +
      geom_text(label=viz_data$Country, nudge_x = x_offset, check_overlap = TRUE) +
      xlab("Number of Facilities") +
      ylab("Suicide Rates") +
      ggtitle("Number of Facilities vs Suicide Rates")
    
    if(input$facility_type == "mental_hospitals") {
      p <- p + geom_line(data = line_plot_data_mental_hospitals, 
                         aes(x = mental_hospitals, y = suicide_rate),
                         color = "blue")
    }
    
    if(input$facility_type == "health_units") {
      p <- p + geom_line(data = line_plot_data_health_units, 
                         aes(x = health_units, y = suicide_rate), 
                         color = "blue")
    }
    
    if(input$facility_type == "outpatient_facilities") {
      p <- p + geom_line(data = line_plot_data_outpatient_facilities, 
                         aes(x = outpatient_facilities, y = suicide_rate),
                         color = "blue")
    }
    
    return(p)
  })
  
  output$table1 <- renderTable({
    viz_data <- subset(plot_data, select = c("Country", "suicide_rate", input$facility_type))
    viz_data <- viz_data %>% 
      drop_na()
    
    viz_data %>% 
      select(Country, input$facility_type) %>% 
      arrange(desc(!!rlang::sym(input$facility_type)))
  })
  
  output$viz2 <- renderPlot({
    viz_data <- subset(plot_data, select = c("Country", "suicide_rate", input$human_resources))
    viz_data <- viz_data %>% 
      drop_na()
    
    p <- ggplot(
      data = viz_data,
      mapping = aes_string(x = input$human_resources, y = "suicide_rate")
    ) +
      geom_point() +
      geom_smooth(mapping = aes_string(x = "suicide_rate", y = input$human_resources)) +
      geom_text(label=viz_data$Country, nudge_y = 1, check_overlap = TRUE) +
      xlab("Number of Human Resources") +
      ylab("Suicide Rates") +
      ggtitle("Number of Human Resources vs Suicide Rates")
    return(p)
  })
  
  output$viz3 <- renderPlot({
 p <- ggplot(
        data = plot_data_totals,
        mapping = aes_string(x = "suicide_rate", y = input$total_selection)
      ) +
        geom_point() +
        geom_smooth(mapping = aes_string(x = "suicide_rate", y = input$total_selection)) +
        geom_text(label=plot_data_totals$Country, nudge_y = 1, check_overlap = TRUE) +
        xlab("Suicide Rates") +
        ylab("Number of Resources")+
   ggtitle("Number of Resources vs Suicide Rates")
      return(p)
  })
}
