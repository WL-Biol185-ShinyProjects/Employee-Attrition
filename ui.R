
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

dashboardPage(skin = "green", 
  dashboardHeader(title = "Healthcare Attrition", titleWidth = 750),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "Graphs"),
      menuItem("History", tabName = "History")
    )
  ),
  dashboardBody(
    #Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("densityplot"),
          selectInput("column", "Choose an X-axis column", choices = "Age", "Monthly Income")
          )
      ),
    
    tabItems(
      tabItem(tabName = "Graphs"),
      tabItem(tabName = "History")
    )
)
)
    
    


