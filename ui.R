
library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    #Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1"),
      selectInput("column", "Choose an X-axis column", choices = "Age", "Monthly Income")
        )
      )
    )
)

