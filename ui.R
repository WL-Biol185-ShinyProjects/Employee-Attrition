
library(shiny)
library(shinydashboard)
library(ggplot2)

#The general layout of dashboard page contains a dashboardHeader and dashboardSidebar
dashboardPage(skin = "green",
  dashboardHeader(title = "Healthcare Attrition"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "Graphs"),
      menuItem("History", tabName = "History"),
      menuItem("About", tabName = "About our page")
    )
  ),

    dashboardBody(
      
      #Boxes need to be put in a row (or column)
      fluidRow(
              ),
      #Below tabs are named and their contents is specified
      tabItems(
        tabItem(tabName = "Graphs", h2("Graphs"),
                box(plotOutput("histogramplot"),
                    selectInput("histogram_data", "Choose an X-axis", choices = c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", 
                                                                                  "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", 
                                                                                  "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", 
                                                                                  "YearsAtCompany", "YearsInCurrentRole"))),
                box(plotOutput("densityplot"),
                    selectInput("density_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike")))),
        tabItem(tabName = "History", h2("Let's learn some history"), h3("In the United States, 
                                                                        healthcare employees experience some of the highest rates of burnout of any industry. 
                                                                        This is due to long hours and sad outcomes.")
      ),
        tabItem(tabName = "About our page", h2("Thank you for visiting our page."))
    )       
  )
)