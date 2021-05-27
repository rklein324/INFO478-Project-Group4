#install packages
library(shiny)
library(ggplot2)
library(plotly)

# read dataset 


# --------- CREATE WIDGETS ---------- 



# --------- CREATE PAGES ---------- 

page_one <- tabPanel(
  "Introduction",
)

page_two <- tabPanel(
  "Viz 1",
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
      # widgets
      ),
    mainPanel(
      # plot
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

