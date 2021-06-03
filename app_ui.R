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
    h2(strong("Research Question")),
    p("What is more effective at preventing suicide: increasing
      mental health facilities or human resources?"),
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
      plotlyOutput("viz2.1"),
      br(),
      h3("Analysis"),
      p("There are three types of mental health human resources to choose from: 
        psychiatrists, nurses, and psychologists. After selecting a human resource type, 
        it will display a table of the top 20 countries with the most mental health human 
        resource of the selected human resource type, the number of available human resources, 
        and suicide rates. For example, by selecting psychiatrists, a table will show the top 
        20 countries with the most mental health psychiatrists, the number of available psychiatrists, 
        and the suicide rate in those countries."),
      p("The first chart is a scatter plot showing the number of the selected mental health human 
        resources and suicide rates in each country. Two data points are selected for each human 
        resource type to form a line to observe the relationship between mental health human 
        resources and the suicide rate in a given country. One data point is the country 
        with the highest suicide rate, and the other is the country with the highest number 
        of the selected human resource type. By doing so, we can see that the two variables 
        are not a one-to-one relationship. Instead, the trend is that as the number of human 
        resources increases, the suicide rate decreases, and vice versa. Norway, for example, 
        has the highest number of mental health psychiatrists and a relatively low suicide rate. 
        On the other hand, Guyana has the highest suicide rate but rather few mental 
        health psychiatrists and psychologists."),
      p("The second chart is a cluster bar chart that includes the top 10 countries with the 
        most human resources of the selected type and suicide rates. In the chart, we can see 
        that the trend of having more mental health human resources leading to a lower suicide 
        rate is not apparent in psychiatrists, but it is evident in nurses and psychologists. 
        One reason may be that there are more nurses and psychologists available because their 
        profession includes a broader range."),
      p("Comparing the three types of human resources and suicide rates, we discovered that 
        having more mental health nurses has a more significant impact in reducing suicide rates. 
        Understanding which human resources are most effective in reducing suicide rates is very 
        helpful to guide countries in how to best allocate their resources.")
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
      br(),
      plotlyOutput("viz3.1"),
      h3("Weights"),
      p("Weights are based on the slope of the lines used in the visualizations
         in the previous two tabs. The line connects the 2 points with the
         highest suicide rate and the highest number of resources. The
         coordinates of the points and the resulting weights are shown below."),
      tableOutput("weightTable1"),
      p("In order to have enough data points to work with, any NA values for the
        visualization on this tab are replaced with 0. In order to get the
        weighted totals, each value for number of resources is multiplied by its
        corresponding weight."),
      h3("Analysis"),
      p("The first chart, the scatter plot, shows the relationship between the
        number of either facilities or human resources (in the mental health
        field) and the suicide rate in that country in 2016. Similar to
        comparing individual resources, this chart shows that as the number of
        resources increase the MAXIMUM suicide rate decreases. This is
        demonstrated by the segment line and the axes making a right triangle.
        While some countries have few resources AND low suicide rates, no
        countries have many resources and high suicide rates. "),
      p("This is also shown in the grouped bar chart showing the countries with
        the top 10 highest number of mental health resources. While suicide
        rates do not linearly increase as resources decrease, there is a trend
        showing that suicide rates tend to be closer to or higher than the
        number of resources as the number of resources decreases."),
      p("This demonstrates that while investing in mental health facilities and
        human resources is important in preventing suicide, it is not the only
        factor, and these other factors are also important at reducing the rates
        of suicide."),
      p("When it comes to deciding whether to more heavily invest in mental
        health facilities or human resources, you can compare the slope of the
        connecting lines showing the (general) maximum suicide rate. (Again, the
        steeper the slope, the more theoretically effective the resource is in
        preventing suicide.) These weights are shown below."),
      tableOutput("weightTable2"),
      p("While the slope for facilities when unweighted is steeper than human
        resources (both unweighted and weighted), the slope for weighted
        facilities is slightly shallower than for weighted human resources. This
        implies that if you have to choose between investing in mental health
        facilities or human resources, it is best to add more personnel, rather
        than spread out the personnel you already have.")
    )
  )
)

page_five <- tabPanel(
  "Conclusion",
  titlePanel("Conclusion"),
  h2("Limitations"),
  p("For the visualizations in the 'Facilities for Mental Health' and 'Human Resources for
    Mental Health' tabs, we had to remove missings and NA values from our dataset. As a result,
    the visualizations don't include every country, so insights gained from
    these visualizations may not apply to every country. Additionally, since the visualizations
    plot a line between the maximum suicide rate and maximum number of facilities to be used in the weights,
    viewers may not see a one to one relationship between the number of facilities and suicide rates.
    This line also may not be the best one to accuratly demonstrate the relationship
    between the number of resources and the maximum suicide rate.
    Moreover, for the visualizations in the 'Facilities vs Human Resources' tab, any missing
    values were replaced with zeros rather than being removed in order to show every country on the visualization."),
  h2("Strengths"),
  p("Although the data sets have some limitations where some countries have missing data, 
    we were still able to use them and discover some critical findings. Our goal is to study 
    the global suicide rate to investigate the best mental health care strategy to be implemented 
    at the policy level. Therefore, effective observation of the relationship between mental health 
    facilities and human resources and the suicide rate in a specific country can significantly 
    assist the allocation of resources to reduce suicide rates. From the visualizations, we can see 
    that with the increase of suicide rate, mental health resources decrease, and vice versa. Mental 
    health facilities and human resources have a significant impact on the suicide rate of a country. 
    This finding can be an important consideration for public policy decision-making around mental health. 
    If public policymakers take into account key findings, they can significantly reduce the suicide rate 
    and improve the country's mental health."),
  h2("Discussion"),
  p("Global suicide rates is a complicated issue that has been studied for a
    long time. Individual countries have different factors that often play a
    role such as macroeconomic indicators, cultural variables, and their
    curative medical solutions toward mental health issues. You can read",
    a("this paper", href = "https://www.sciencedirect.com/science/article/pii/S0160252713000587"),
    "for more information and how researchers have approached suicide rates
    in the past."),
  p("Our general analysis relies somewhat on the assumption that the mental
    health resources we looked at have an impact at reducing suicide rates at
    all. The data from our exploratory analysis does seem to point to a
    relationship, but prior research has conflicting results as to what
    solutions are still the most effective. In general, there should be a call
    for long-term public health initiatives and further investigation into
    what can realistically be implemented for a given country. There are an
    excess of problems that contribute to a country's mental wellness, but
    ensuring that help is available in the first place at a basic level seems
    like a decent place to start.")
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
