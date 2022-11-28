
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)


#The general layout of dashboard page contains a dashboardHeader and dashboardSidebar
dashboardPage(skin = "blue", 
              dashboardHeader(title = "Healthcare Attrition"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Home", tabName = "Home"), 
                  menuItem("Who Quits?", tabName = "WhoQuits"),
                  menuItem("Self Help", tabName = "SelfHelp"),
                  menuItem("What Factors Matter the Most?", tabName = "WhatFactorsMattertheMost"),
                  menuItem("Graphs", tabName = "Graphs"),
                  menuItem("History", tabName = "History"),
                  menuItem("Employee Lifestyle", tabName = "EmployeeLifestyle")
                  
                )
              ),
              dashboardBody(
                
                #Boxes need to be put in a row (or column)
                fluidRow(
                  
                  
                  #Below tabs are named and their contents is specified
                  #The first tab is the home tab
                  tabItems(
                    tabItem(tabName = "Home",
                            box(
                              title = "Welcome!", background = "red", width = "12", status = "primary",
                              HTML('<center><img src="group picture.jpeg" width="800"></center>'
                              ),
                              h3(p("This project was created by Sadie Charles Calame, Ellen Dulin, Mary Jane McConnell, and Dylan Walmsley."),
                                 br(),
                                 p("Healthcare employee attrition is a growing problem in the United States.Our data was obtained from", a(href = "https://www.kaggle.com/datasets/jpmiller/employee-attrition-for-healthcare",  
                                                                                                                                           "kaggle"),
                                   "and pertains to healthcare employees quitting their jobs."
                                 )
                              )
                            )
                    ),
                    
                    #The second tab is Who Quits
                    tabItem( tabName = "WhoQuits",
                             box( plotOutput("BarCategoricalComparison"
                             ),
                             selectInput( "XCategoricalComparisonData",
                                          "Choose an X-axis",
                                          choices = c( "BusinessTravel",
                                                       "EducationField",
                                                       "EnvironmentSatisfaction",
                                                       "Gender",
                                                       "JobSatisfaction",
                                                       "MaritalStatus", 
                                                       "WorkLifeBalance"
                                          )
                             )
                             )             
                    ),
                    #The third tab is the self-help tab          
                    tabItem( tabName = "SelfHelp",
                             h2( "Estimating Your Chance of Quitting! Happiness"
                             ),
                             #Slider         
                             box(
                               numericInput( "Age", 
                                            "What is Your Age?",
                                            40,
                                            18, 
                                            60,
                                            step = 1
                               ),
                               selectizeInput( "BusinessTravel", 
                                               "How Often Do you Travel?", 
                                               c( "Non-Travel", 
                                                  "Travel_Rarely", 
                                                  "Travel_Frequently"
                                               ),
                                               selected = NULL
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
                               numericInput( "EnvironmentSatisfaction", 
                                            "How Satisfied Are You with Your Job Environment?", 
                                            2,
                                            1, 
                                            4, 
                                            step = 1
                               ),
                               selectizeInput( "Gender", 
                                               "What Is Your Gender?", 
                                               c( "Male", 
                                                  "Female"
                                               ), 
                                               selected = NULL
                               ),
                               numericInput( "JobSatisfaction", 
                                            "How Satisfied Are You with Your Job?", 
                                            2,
                                            1, 
                                            4, 
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
                               numericInput( "MonthlyIncome", 
                                            "What Is Your Monthly Income?", 
                                            10000,
                                            1000,  
                                            20000,
                               ),
                               selectizeInput( "OverTime", "Do You Work Overtime Often?", 
                                               c( "Yes", 
                                                  "No"
                                               ), 
                                               selected = NULL
                               ),
                               numericInput( "PercentSalaryHike",  
                                            "How Much Has Your Salary Increased over Your Career?", 
                                            17,
                                            10, 
                                            25,
                               ),
                               numericInput( "TotalWorkingYears", 
                                            "How Many Years Have You Worked?", 
                                            20,
                                            0, 
                                            40, 
                                            step = 1
                               ),
                               numericInput( "WorkLifeBalance", 
                                            "How Is Your Work/Life Balance?", 
                                            2,
                                            1, 
                                            4, 
                                            step = 1
                               ),
                               numericInput( "YearsAtCompany", 
                                            "How Many Years Have You Worked at Your Current Company?", 
                                            20,
                                            0, 
                                            40, 
                                            step = 1
                               ),
                               numericInput( "YearsInCurrentRole", 
                                            "How Many Years Have You Worked in Your Current Role?",
                                            10,
                                            0, 
                                            20, 
                                            step = 1
                               ),
                               width = 12
                             ),
                             
                             h2("Rank How Important Each Item Is to You."
                             ),
                             
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
                               selectInput( "Rank2", "Second", choices = c(""
                               )
                               ),
                               selectInput( "Rank3", "Third", choices = c(""
                               )
                               ),
                               selectInput( "Rank4", "Fourth", choices = c(""
                               )
                               ),
                               selectInput( "Rank5", "Fifth", choices = c(""
                               )
                               ),
                               selectInput( "Rank6", "Sixth", choices = c(""
                               )
                               ),
                               selectInput( "Rank7", "Seventh", choices = c(""
                               )
                               ),
                               selectInput( "Rank8", "Eighth", choices = c(""
                               )
                               ),
                               selectInput( "Rank9", "Ninth", choices = c(""
                               )
                               ),
                               selectInput( "Rank10", "Tength", choices = c(""
                               )
                               ),
                               selectInput( "Rank11", "Eleventh", choices = c(""
                               )
                               ),
                               selectInput( "Rank12", "Twelfth", choices = c(""
                               )
                               ),
                               selectInput( "Rank13", "Thirteenth", choices = c(""
                               )
                               ),
                               selectInput( "Rank14", "Fourteenth", choices = c(""
                               )
                               ),
                               width = 6
                             ),
                             
                             box(
                               h2("Your Likelihood of Quitting Is..."
                               ),
                               textOutput("AttritionEstimation"),
                               width = 6
                             ),
                             
                             h2("What You Can Do to Minimize Burnout"
                                
                             )
                             
                    ),
                    tabItem( tabName = "WhatFactorsMattertheMost",
                             box(tableOutput("CategoricalRegression")
                             )
                    ),
                    #The fourth tab is the Graphs                
                    tabItem(tabName = "Graphs", 
                            
                            #Histogram
                            box( h2("Histogram"),
                                 plotOutput("HistogramPlot"),
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
                            
                            
                            
                            #Density Plot 
                            box( h2("Density Plot"),
                                 plotOutput( "DensityPlot"),
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
                            ),
                            
                            h2("Summary Table"),
                            #Summary Table
                            box( tableOutput("SummaryTable"),
                                 selectInput(inputId = "SummaryData",
                                             "Choose an x-axis",
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
                            )
                    ),
                    
                    #The fifth tab is the history tab
                    tabItem( tabName ="History", 
                             box(
                               title = "COVID-19 and Healthcare Attrition:", background = "teal", solidHeader = TRUE, width = 6, status = "primary", 
                               tags$ul(
                                 tags$li("Healthcare burnout was a crisis long before the coronavirus pandemic hit.
                                   Rooted in the healthcare system, these systemic shortfalls include escalating workloads, inadequate support, underinvestment in public health infrastructure, etc.  "),
                                 tags$li("Burnout symptoms include “insomnia, depression, anxiety, post-traumatic stress disorder, and other mental health challenges (Murthy, 2022, N Engl J Med).” 
                                   According to the New England Journal of Medicine, “some 52% of nurses (according to the American Nurses Foundation and 20% of doctors (Mayo Clinic Proceedings) say they are planning to leave their clinical practices (2022).”"),
                                 tags$li("The coronavirus pandemic escalated burnout symptoms in health care workers and left many of them on the brink of quitting. 
                                   The pandemic caused acute staffing shortages, intensified safety protocol, increased uncertainty around available resources, and increased workloads. "),
                                 tags$li("The Delta and Omicron surges sent many unvaccinated patients into hospitals where health care employees were already emotionally and physically exhausted from the last 18 months of the pandemic. 
                                 Additionally, health care workers had to make unprecedented decisions, risk infection for themselves and their families, and experience the pain of losing many patients.") 
                               )
                             ),
                             
                             
                             box(
                               title = "Steps to Take:", background = "green", width = 6, status = "primary",
                               tags$ol(
                                 tags$li(tags$b("This data set shows the critical need for changes to the healthcare system.")),
                                 tags$li(tags$b("Health care workers need to be protected and valued."), 
                                         "This involves ensuring proper PPE, which was not available during the beginning of the pandemic, adequate sick leave, and protection of staff from verbal and physical abuse."),
                                 tags$li(tags$b("We need to help reduce the administrative burdens."), 
                                         "According to one study, “outpatient physicians spend nearly 2 hours on the electronic health record and desk work during the day for every 1 hours spent with patients (Arndt, et al., 2017, Ann Fam Med). 
                                             This takes away valuable time that could be spent with patients."),
                                 tags$li(tags$b("We need to increase access to mental healthcare."), 
                                         "Due to their lack of schedule flexibility for visits and long working hours, many health care workers do not prioritize their mental health. "),
                                 tags$li(tags$b("We need public investments in health infrastructure"), "that focus on addressing the social determinants of health which would reduce the demands on the healthcare system. "),
                                 tags$li(tags$b("We need to change the narrative that suffering is just an essential component of health professions."), 
                                         "Health care workers should be able to voice their mental health concerns or seek help without feeling isolated or embarrassed (Murthy, 2022, N Engl J Med). ")
                               )
                               
                             ),
                             box(
                               title = "Consequences of Burnout on Health Care Workers:", background = "blue", width = "12", status = "primary",
                               HTML('<center><img src="consequencesofhealthcareattrition.png" width="800"></center>'
                               ),
                               br(),
                               p("Leo, C. G., Sabina, S., Tumolo, M. R., Bodini, A., Ponzini, G., Sabato, E., & Mincarone, P. (2020). Burnout Among Healthcare Workers in the COVID 19 Era: A Review of the Existing Literature. Frontiers in Public Health, 9. https://doi.org/10.3389/fpubh.2021.750529")
                             )
                             
                    ),
                    
                    #The sixth tab is the Employee Lifestyle tab
                    tabItem(tabName = "EmployeeLifestyle", 
                            box(plotOutput( "BarBusinessTravel"
                            )
                            ), 
                            box(plotOutput( "BarOvertime"
                            ) 
                            ),
                            box(plotOutput("BarEnvirSatisfaction"
                            )
                            ),
                            box(plotOutput("BarJobSatisfaction"
                            )
                            ),
                            box(plotOutput("BarWorkLifeBalance"
                            )
                            )
                            
                    )                                      
                  )
                )
              )
)

                           
                           

                    

