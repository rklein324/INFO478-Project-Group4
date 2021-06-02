#install packages
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

facilities_choices <- list(
  "Mental Hospitals" = "mental_hospitals",
  "Health Units" = "health_units",
  "Outpatient Facilities" = "outpatient_facilities"
)

hr_choices <- c("Psychiatrists", "Nurses", "Psychologists")

comparison_choices <-list("Facilities (unweighted)" = "total_facilities",
                          "Facilities (weighted)" = "total_facilities_wt",
                          "Human Resources (unweighted)" = "total_hr",
                          "Human Resources (weighted)" = "total_hr_wt")

# --------- CREATE WIDGETS ---------- 
# drop down widget for viz #1
viz1_widget <- selectInput(inputId = "facility_type", label = h3("Select Facility Type"), 
            choices = facilities_choices)

# drop down widget for viz #2
viz2_widget <- selectInput(inputId = "human_resources",
                           label = h3("Select Human Resource Type"), 
                           choices = hr_choices)

# drop down widget for viz #3
viz3_widget <- selectInput(inputId = "total_selection",
                           label = h3("Select Resource Type"), 
                           choices = comparison_choices)

# --------- CREATE PAGES ---------- 

page_one <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
  mainPanel(
    h2(strong("Objective")),
    p("The main objective of this report is to look at suicide
      rates globally to investigate the best mental health care
      strategies to implement at a policy level. Individuals can
      enact methods to benefit their mental health on a personal
      level, but resources also need to be made available by
      government bodies. This requires policy intervention in
      order to meet",
      a("public health goals", href =
          "https://www.who.int/news-room/fact-sheets/detail/suicide"),
      "and to create healthy trends for future generations."),
    p("Countries around the world have their own health measures
      in place for physical needs as well as mental health. They
      vary in the care they provide and in how much is made
      widely available. By looking at the suicide rates per
      country provided by the World Health Organization (WHO),
      the effectiveness of the mental health care can be compared
      across many nations. The differences lie with the mental
      health facilities and the human resources trained for
      mental health care. Having more of each resource would be
      beneficial, but policy makers and public health officials
      need to prioritize implementing the most effective mental
      health care strategy that funding will allow them."),
    h2(strong("Dataset")),
    p("The data was retrieved from",
      a("Kaggle.com", href =
          "https://www.kaggle.com/twinkle0705/mental-health-and-suicide-rates?select=Age-standardized+suicide+rates.csv"),
      "by a user who utilized data from WHO. There are four files
      included with values broken down by country. Two of the
      tables look at human resources and mental health
      facilities per country and the remaining two look at crude
      suicide rates and age-standardized suicide rates per
      country. The data is reported from 2016 and covers suicide
      rates across gender and age ranges."),
    h2(strong("Sections of Analysis")),
    p("To get a clearer picture, we will breakdown which type of
      mental health facilities and human resources seem to have
      the most positive effect at reducing suicide rates. In the
      final visualization, we aggregate the data into facility
      types and human resource types with the option
      to weigh the resource based on its effectiveness in
      reducing suicide rates for a given country.")
  )
)

page_two <- tabPanel(
  "Facilities for Mental Health",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz1_widget,
      tableOutput("table1")
    ),
    mainPanel(
      plotOutput("viz1"),
      h3("Analysis"),
      h4("Analysis on Mental Hospitals and Suicide Rates"),
      p("Filler"),
      br(),
      h4("Analysis on Health Units and Suicide Rates"),
      p("Filler"),
      br(),
      h4("Analysis on Outpatient Facilities and Suicide Rates"),
      p("Filler")
    )
  )
)

page_three <- tabPanel(
  "Human Resources for
      Mental Health",
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
  "Mental Health Resources Analysis",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)
