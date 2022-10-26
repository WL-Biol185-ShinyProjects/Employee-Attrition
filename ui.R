
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
                h2("Histogram"),
                
                #Histogram
                box(plotOutput("histogramplot"),
                    selectInput("histogram_data", 
                                "Choose an X-axis", 
                                choices = c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", 
                                                                                  "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", 
                                                                                  "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", 
                                                                                  "YearsAtCompany", "YearsInCurrentRole")
                                )
                ),
                
                h2("Density Plot"),
                
                #Density Plot 
                box(plotOutput("densityplot"),
                    selectInput("density_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany")
                                )
                    ),
                #Scatter Plot
                box(plotOutput("scatterplot"), 
                    selectInput("x_scatter_data", "Choose an X-axis", choices = c("MonthlyIncome", "Percent Salary Hike")),
                    selectInput("y_scatter_data", "Choose a Y-axis", choices = c("TotalWorkingYears", "YearsAtCompany")
                                )
                )
                ),
        
        #The Second Tab is the History Tab
        tabItem(tabName = "History", h2("Let's learn some history"), h3("In the United States, 
                                                                        healthcare employees experience some of the highest rates of burnout of any industry. 
                                                                        This is due to long hours and sad outcomes.")
                ), 

        #The Third Tab is the Self-Help Tab          
        tabItem(tabName = "Self-Help",
                h2("Estimating Your Chance of Quitting! Happiness"),
                
                box(
                  sliderInput("Age", "What is Your Age?", 18, 60, 40, step = 1),
                  selectizeInput("BusinessTravel", "How Often Do you Travel?", c("Non-Travel", "Travel_Rarely", "Travel_Frequently")),
                  selectizeInput("EducationField", "What Field Did You Study?", c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other")),
                  sliderInput("EnvironmentSatisfaction", "How Satisfied Are You with Your Job Enivironment?", 1, 4, 2, step = 1),
                  selectizeInput("Gener", "What Is Your Gender?", c("Male", "Female")),
                  sliderInput("JobSatisfaction", "How Satisfied Are You with Your Job?", 1, 4, 2, step = 1),
                  selectizeInput("MaritalStatus", "What's Your Marital Status?", c("Single", "Married", "Divorced")),
                  width = 12
                ),
                
                h2("What You Can Do to Minimize Burnout")
        )
    )       
  )
)



