
library(shiny)
library(shinydashboard)

dashboardPage(
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
      tabItem(tabName = "History", h2("Let's learn some history"))
    )
  )
)

