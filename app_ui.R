#install packages
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# read datasets 
age_standardized_suicide <- read.csv("./data/national/Age-standardized suicide rates.csv")
facilities <- read.csv("./data/national/Facilities.csv")

# remove leading & trailing spaces
age_standardized_suicide$Sex <- trimws(age_standardized_suicide$Sex, which = c("both"))

# remove missings
facilities <- facilities %>% 
  drop_na()


facilities <- facilities %>% 
  rename("mental_hospitals" = "Mental._hospitals",
         "outpatient_facilities" = "outpatient._facilities")

facilities_choices <- list(
  "Mental hospitals" = "mental_hospitals",
  "Health units" = "health_units",
  "Outpatient facilities" = "outpatient_facilities",
  "Residential facilities" = "residential_facilities"
)

age_standardized_suicide <- age_standardized_suicide %>%
  filter(Sex == "Both sexes") %>%
  rename("suicide_rate" = X2016) %>%
  select(Country, "suicide_rate")

plot_data <- inner_join(age_standardized_suicide, facilities)
#   
# p <- plot_ly(
#   data = plot_data,
#   mode = "markers",
#   type = "scatter",
#   x = ~mental_hospitals,
#   y = ~suicide_rate
# ) %>% layout(
#   title = "Suicide rates vs Facilities",
#   xaxis = list(title = "Number of Facilities"), 
#   yaxis = list(title = "Suicide Rates")
# )

# p <- ggplot(
#   data = plot_data,
#   mapping = aes(x = suicide_rate, y = mental_hospitals)
# ) +
#   geom_point() +
#   labs(
#   title = "Suicide rates vs Facilities",
#   xlab = "Suicide Rates",
#   ylab = "Number of Facilities"
# )

# --------- CREATE WIDGETS ---------- 
# drop down widget for viz #1
viz1_widget <- selectInput(inputId = "facility_type", label = h3("Select Facility Type"), 
            choices = facilities_choices)

viz3_widget <- selectInput(inputId = "total_selection", label = h3("Select Resource Type"), 
                           choices = list("Facilities (unweighted)" = "total_facilities",
                                          "Facilities (weighted)" = "total_facilities_wt",
                                          "Human Resources (unweighted)" = "total_hr",
                                          "Human Resources (weighted)" = "total_hr_wt"))

# --------- CREATE PAGES ---------- 

page_one <- tabPanel(
  "Introduction",
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
      # widgets
    ),
    mainPanel(
    # plot
    )
  )
)

page_four <- tabPanel(
  "Viz 3",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      viz3_widget),
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

