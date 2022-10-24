
library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)

#Application Title 
titlePanel("Healthcare Employee Attrition")

#Show a plot of the generated distribution 
mainPanel(
  plotOutput("densityplot"), 
  selectInput("var", "Choose an X-axis column", choices = colnames())
)

dashboardPage(skin = green,
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
      box(plotOutput("plot1"),
          selectInput("column", "Choose an X-axis column", choices = c("Age", "Monthly Income"))
      )
    ),
    
    tabItems(
      tabItem(tabName = "Graphs"),
      tabItem(tabName = "History")
    )
  )
)