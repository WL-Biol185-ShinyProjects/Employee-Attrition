
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)


#The general layout of dashboard page contains a dashboardHeader and dashboardSidebar
dashboardPage(skin = "green",
  dashboardHeader(title = "Healthcare Attrition"),
  dashboardSidebar(
    sidebarMenu(
                 menuItem("Graphs", tabName = "Graphs"),
                 menuItem("History", tabName = "History"),
                 menuItem("About", tabName = "About"),
                 menuItem("Self-Help", tabName = "Self-Help"),
                 menuItem("Employee Lifestyle", tabName= "Employee Lifestyle")
               )
                  ),

  dashboardBody(
      
#Boxes need to be put in a row (or column)
    fluidRow(
  
    
#Below tabs are named and their contents is specified
#The first tab is the graphs tab
      tabItems(
                tabItem(tabName = "Graphs", 
                  h2("Histogram"),
#Histogram
                  box( plotOutput("histogramplot"),
                       selectInput( "histogram_data", 
                                    "Choose an X-axis", 
                                     choices = c( "Age", 
                                                  "BusinessTravel", 
                                                  "EducationField", 
                                                  "EnvironmentSatisfaction", 
                                                  "Gender", 
                                                  "JobSatisfaction", 
                                                  "MaritalStatus", 
                                                  "OverTime", 
                                                  "PercentSalaryHike", 
                                                  "TotalWorkingYears",
                                                  "WorkLifeBalance", 
                                                  "YearsAtCompany", 
                                                  "YearsInCurrentRole"
                                               )
                                  )
                            
                      ),
              
                   h2("Density Plot"),
              
#Density Plot 
                   box( plotOutput( "densityplot"),
                        selectInput( "density_data", 
                                     "Choose an X-axis", 
                                      choices = c( "MonthlyIncome", 
                                                   "PercentSalaryHike"
                                                  )
                                   )
                      ),
    
                   h2("Scatter Plot"),
#Scatter Plot
              
                   box( plotOutput("scatterplot"), 
                        selectInput( "x_scatter_data", 
                                    "Choose an X-axis", 
                                     choices = c( "MonthlyIncome", 
                                                  "PercentSalaryHike"
                                                )
                                   ),
                        selectInput( "y_scatter_data", 
                                     "Choose a Y-axis", 
                                     choices = c( "TotalWorkingYears", 
                                                "YearsAtCompany"
                                                )
                                   )
                       )
                     ),
      
#The Second Tab Is the History Tab
                  tabItem( tabName ="History", 
                           h2("Let's learn some history"), 
                           h3("In the United States, healthcare employees experience some of the highest rates of burnout of any industry. This is due to long hours and sad outcomes."
                              )
                         ),
      
#The Third Tab Is the About Tab
                  tabItem( tabName = "About", 
                    box(
                      selectInput( "attrition_data",
                                   "Choose an X-axis", 
                                    choices = c( "MonthlyIncome", 
                                                 "PercentSalaryHike"
                                              )
                                  ),
                      selectInput( "density_data", 
                                   "Choose an X-axis", 
                                   choices = c( "MonthlyIncome", 
                                                "PercentSalaryHike", 
                                                 "TotalWorkingYears", 
                                                 "YearsAtCompany"
                                              )
                                  )
                        )
              
                          ),

      #The Fourth Tab is the Self-Help Tab          
                  

                  tabItem(tabName = "Employee Lifestlye", 
                    box(
                      plotOutput( "barbusinesstravel"
                                 )
                        )
                         )
              )
            )
                )
             )



