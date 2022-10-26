
library(shiny)
library(shinydashboard)
library(ggplot2)

#Show a plot of the generated distribution 
mainPanel(
)

#The general layout of dashboard page contains a dashboardHeader and dashboardSidebar
dashboardPage(skin = "green",
  dashboardHeader(title = "Healthcare Attrition"),
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
      
      #Below tabs are named and their contents is specified\
      #The first tab is the graphs tab
      tabItems(
        tabItem(tabName = "Graphs", 
                h2("Histograms"),
                box(plotOutput("histogramplot"),
                    selectInput("histogram_data", 
                                "Choose an X-axis", 
                                choices = c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", 
                                                                                  "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", 
                                                                                  "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", 
                                                                                  "YearsAtCompany", "YearsInCurrentRole")
                    )
                ),
                
                h2("Density Plots"),

                box(plotOutput("densityplot"),
                    selectInput("density_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany")
                                )
                    )
                ),
        
        #The Second Tab is the History Tab
        tabItem(tabName = "History", h2("Let's learn some history"), h3("In the United States, 
                                                                        healthcare employees experience some of the highest rates of burnout of any industry. 
                                                                        This is due to long hours and sad outcomes.")
                ), 

                   
        tabItem(tabName = "Self-Help",
                h2("Estimating Your Chance of Quitting! Happiness"),
                h2("What You Can Do to Minimize Burnout")
        )
    )       
  )
)

