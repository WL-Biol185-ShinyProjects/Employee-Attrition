
library(shiny)
library(shinydashboard)
library(ggplot2)

#Show a plot of the generated distribution 
mainPanel(
)

dashboardPage(skin = "green",
  dashboardHeader(title = "Healthcare Attrition", titleWidth = 750),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "Graphs"),
      menuItem("History", tabName = "History"),
      menuItem("Self-Help", tabName = "Self-Help")
    )
  ),

    dashboardBody(
      
      #Boxes need to be put in a row (or column)
      fluidRow(
      ),
      
      tabItems(
        tabItem(tabName = "Graphs", h2("This is a graph"),
                box(plotOutput("densityplot"),
                    selectInput("attrition_data", "Choose an X-axis", choices = colnames(watson_healthcare_modified)
                                ))
        ),
        
        tabItem(tabName = "History",
                h2("Let's learn some history"),
                h3("In the United States, healthcare employees experience some of the highest rates of burnout of any industry. 
                                                                        This is due to long hours and sad outcomes.")
        )
        
        tabItem(tabName = "Self-Help",
                h("How to Measure Your Level of Burnout"),
                h("What You Can Do to Minimize Burnout"))
    )       
  )
)

