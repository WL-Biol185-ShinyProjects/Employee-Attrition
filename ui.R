
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
            ),
    
    tabItems(
      tabItem(tabName = "Graphs", h2("This is a graph"),
              box(plotOutput("plot1"),
                  selectInput("column", "Choose an X-axis column", choices = c("Age", "Monthly Income")
                              )
                  )
              ),
      tabItem(tabName = "History", h2("Let's learn some history"), x = "In the United States, healthcare employees experience some of the highest rates of burnout of any industry. This is due to long hours and sad outcomes.")
    )
  )
)