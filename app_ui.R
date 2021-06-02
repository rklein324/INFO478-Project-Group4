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
      br(),
      plotlyOutput("viz1.1"),
      h3("Analysis"),
      p("There are three mental health facility types users can select: 
        Mental Hospitals, Health Units, and Outpatient Facilities. When a 
        facility type is selected, the table below the facility type selection 
        input gets updated to display the top 20 countries with the most facilities
        of the selected facility type along with the country's suicide rate."),
      p("In the first chart, the scatterplot shows the number of mental health facilities and the 
        suicide rate for each country. For all three facility types, we see that 
        the countries of Guyana and Lithuania have the highest suicide rates
        and some of the fewest number of facilities per 100,000 people. The country with the 
        most number of facilities changes each time depending on the facility type. For mental
        hospitals, Japan has the most number of facilities while Hungary has the most number 
        of health units and Saint Lucia has the most outpatient facilities. Generally,
        as the number of facilities increases, the suicide rates decreases."),
      p("The second chart is a clustered bar chart which plots the top 10 countries
        with the most facilities of the selected facility type and its suicide rate.
        For mental hospitals and health units, we see that generally, the suicide rates are higher than
        the number of facilities. For outpatient facilities, however,
        we see that sometimes the suicide rate is actually lower than the number of outpatient 
        facilities. Comparing the three different facility types, we see that health units are the most
        lacking facility type since the suicide rates are so much higher than the number 
        of health units, followed by mental hospitals.")
    )
  )
)

page_three <- tabPanel(
  "Human Resources for Mental Health",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz2_widget,
      tableOutput("table2")
    ),
    mainPanel(
      plotOutput("viz2"),
      plotlyOutput("viz2.1")
    )
  )
)

page_four <- tabPanel(
  "Facilities vs. Human Resources",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz3_widget,
      tableOutput("table3")
    ),
    mainPanel(
      plotOutput("viz3"),
      plotlyOutput("viz3.1")
    )
  )
)

page_five <- tabPanel(
  "Conclusion",
  titlePanel(""),
  h2("Strengths"),
  p(),
  h2("Limitations"),
  p("For the visualizations in the 'Facilities for Mental Health' and 'Human Resources for
    Mental Health' tabs, we had to remove missings and NA values from our dataset. As a result,
    the visualizations don't include every country, so insights gained from
    these visualizations may not apply to every country. Additionally, since the visualizations
    plot a line between the maximum suicide rate and maximum number of facilities to be used in the weights,
    viewers may not see a one to one relationship between the number of facilities and suicide rates.
    Moreover, for the visualizations in the 'Facilities vs Human Resources' tab, any missing
    values were replaced with zeros rather than being removed in order to show every country on the visualization.")
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
