
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)


#The general layout of dashboard page contains a dashboardHeader and dashboardSidebar
dashboardPage(skin = "green",
  dashboardHeader(title = "Healthcare Attrition"),
  dashboardSidebar(
    sidebarMenu(
                 menuItem("Graphs", tabName = "Graphs"),
                 menuItem("History", tabName = "History"),
                 menuItem("About", tabName = "About"),
                 menuItem("SelfHelp", tabName = "SelfHelp"),
                 menuItem("EmployeeLifestyle", tabName = "EmployeeLifestyle")
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
                  box( plotOutput("HistogramPlot"),
                       selectInput( "HistogramData", 
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
                   box( plotOutput( "DensityPlot"),
                        selectInput( "DensityData", 
                                     "Choose an X-axis", 
                                      choices = c( "MonthlyIncome", 
                                                   "PercentSalaryHike"
                                                  )
                                   )
                      ),
    
                   h2("Scatter Plot"),
#Scatter Plot
              
                   box( plotOutput("ScatterPlot"), 
                        selectInput( "XScatterData", 
                                    "Choose an X-axis", 
                                     choices = c( "MonthlyIncome", 
                                                  "PercentSalaryHike"
                                                )
                                   ),
                        selectInput( "YScatterData", 
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
                    h2("This project was created by Sadie Charles Calame, Ellen Dulin, Mary Jane McConnell, and Dylan Walmsley"),
                    h3("Our data was obtained from", a(href = "https://www.kaggle.com/datasets/jpmiller/employee-attrition-for-healthcare", "kaggle"),
                       "and pertains to healthcare employees quitting their jobs")
                    
              
                          ),


      #The Fourth Tab is the Self-Help Tab          
                  tabItem( tabName = "SelfHelp",
                            h2( "Estimating Your Chance of Quitting! Happiness"),
        #Slider         
                            box(
                                 sliderInput( "Age", 
                                              "What is Your Age?", 
                                              18, 
                                              60, 
                                              40, 
                                              step = 1
                                             ),
                                 selectizeInput( "BusinessTravel", 
                                                 "How Often Do you Travel?", 
                                                 c( "Non-Travel", 
                                                    "Travel_Rarely", 
                                                    "Travel_Frequently"
                                                   ), 
                                                 
                                                ),
                                 selectizeInput( "EducationField", 
                                                 "What Field Did You Study?", 
                                                 c( "Life Sciences", 
                                                    "Medical", 
                                                    "Marketing", 
                                                    "Technical Degree", 
                                                    "Human Resources", 
                                                    "Other"
                                                   ), 
                                                 selected = NULL
                                                ),
                                 sliderInput( "EnvironmentSatisfaction", 
                                              "How Satisfied Are You with Your Job Environment?", 
                                              1, 
                                              4, 
                                              2, 
                                              step = 1
                                             ),
                                 selectizeInput( "Gender", 
                                                 "What Is Your Gender?", 
                                                 c( "Male", 
                                                    "Female"
                                                   ), 
                                                 selected = NULL
                                                ),
                                 sliderInput( "JobSatisfaction", 
                                              "How Satisfied Are You with Your Job?", 
                                              1, 
                                              4, 
                                              2, 
                                              step = 1
                                            ),
                                 selectizeInput( "MaritalStatus", 
                                                "What's Your Marital Status?", 
                                                c( "Single", 
                                                   "Married", 
                                                   "Divorced"
                                                  ), 
                                                selected = NULL
                                                ),
                                 sliderInput( "MonthlyIncome", 
                                             "What Is Your Monthly Income?", 
                                             1000,  
                                             20000, 
                                             10000
                                            ),
                                 selectizeInput( "Overtime", "Do You Work Overtime Often?", 
                                                c( "Yes", 
                                                   "No"
                                                  ), 
                                                selected = NULL
                                               ),
                                 sliderInput( "PercentSalaryHike",  
                                             "How Much Has Your Salary Increased over Your Career?", 
                                             10, 
                                             25, 
                                             17
                                            ),
                                 sliderInput( "TotalWorkingYears", 
                                             "How Many Years Have You Worked?", 
                                             0, 
                                             40, 
                                             20, 
                                             step = 1
                                            ),
                                 sliderInput( "WorkLifeBalance", 
                                             "How Is Your Work/Life Balance?", 
                                             1, 
                                             4, 
                                             2, 
                                             step = 1
                                            ),
                                 sliderInput( "YearsAtCompany", 
                                             "How Many Years Have You Worked at Your Current Company?", 
                                             0, 
                                             40, 
                                             20, 
                                             step = 1
                                            ),
                                 sliderInput( "YearsInCurrentRole", 
                                             "How Many Years Have You Worked in Your Current Role?", 
                                             0, 
                                             20, 
                                             10, 
                                             step = 1),
                                             width = 12
                               ),
              
                          h2("Rank How Important Each Item Is to You."),
              
                          box(
                              selectInput( "Rank1", "First", choices = c("", "Age", 
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
                                                                  ),
                                           selected = ""
                                           ),
                              selectInput( "Rank2", "Second", choices = c("")),
                              selectInput( "Rank3", "Third", choices = c("")),
                              selectInput( "Rank4", "Fourth", choices = c("")),
                              selectInput( "Rank5", "Fifth", choices = c("")),
                              selectInput( "Rank6", "Sixth", choices = c("")),
                              selectInput( "Rank7", "Seventh", choices = c("")),
                              selectInput( "Rank8", "Eighth", choices = c("")),
                              selectInput( "Rank9", "Ninth", choices = c("")),
                              selectInput( "Rank10", "Tength", choices = c("")),
                              selectInput( "Rank11", "Eleventh", choices = c("")),
                              selectInput( "Rank12", "Twelfth", choices = c("")),
                              selectInput( "Rank13", "Thirteenth", choices = c("")),
                              selectInput( "Rank14", "Fourteenth", choices = c("")),
              
                           #box(
                                 #textOutput("AttritionEstimation")
                               #),
              
                           h2("What You Can Do to Minimize Burnout")
<<<<<<< HEAD
                           )
=======
                         )
>>>>>>> ca0cd91e48ded0385bc2e0ba9d7747a0e6d0dab9
                  ),

                  tabItem(tabName = "EmployeeLifestyle", 
                    box(plotOutput( "BarBusinessTravel"
                                  )
                        ), 
                    box(plotOutput( "BarOvertime") 
                       )
<<<<<<< HEAD
                          ),

                  tabItem(tabName = "EmployeeLifestlye", 
                    box(
                      plotOutput( "barbusinesstravel"
                                 )
                       )
                         )
=======
                          )
                  )
              )
>>>>>>> ca0cd91e48ded0385bc2e0ba9d7747a0e6d0dab9
            )
          )

