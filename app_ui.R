#install packages
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

facilities_choices <- list(
  "Mental hospitals" = "mental_hospitals",
  "Health units" = "health_units",
  "Outpatient facilities" = "outpatient_facilities"
)

hr_choices <- c("Psychiatrists", "Nurses", "Psychologists")

# --------- CREATE WIDGETS ---------- 
# drop down widget for viz #1
viz1_widget <- selectInput(inputId = "facility_type", label = h3("Select Facility Type"), 
            choices = facilities_choices)

# drop down widget for viz #2
viz2_widget <- selectInput(inputId = "human_resources", label = h3("Select Human Resource Type"), 
                         choices = hr_choices)

# drop down widget for viz #3
viz3_widget <- selectInput(inputId = "total_selection", label = h3("Select Resource Type"), 
                           choices = list("Facilities (unweighted)" = "total_facilities",
                                          "Facilities (weighted)" = "total_facilities_wt",
                                          "Human Resources (unweighted)" = "total_hr",
                                          "Human Resources (weighted)" = "total_hr_wt"))

# --------- CREATE PAGES ---------- 

page_one <- tabPanel(
  "Introduction"
)

page_two <- tabPanel(
  "Viz 1",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz1_widget
    ),
    mainPanel(
      plotOutput("viz1")
    )
  )
)

page_three <- tabPanel(
  "Viz 2",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz2_widget
    ),
    mainPanel(
      plotOutput("viz2")
    )
  )
)

page_four <- tabPanel(
  "Viz 3",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz3_widget
    ),
    mainPanel(
      plotOutput("viz3")
    )
  )
)

page_five <- tabPanel(
  "Conclusion",
  titlePanel("")
)


# --------- DEFINING UI: PUTTING PAGES TOGETHER ---------- 
ui <- navbarPage(
  "Title",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)

