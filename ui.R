library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)

KaggleUrl <- a( "kaggle", href = "https://www.kaggle.com/datasets/jpmiller/employee-attrition-for-healthcare", 
                target = "_blank"
              )
RSquaredTag <- tags$ul( tags$li( "An R squared value (also known as a coefficient
                                 of determination) represents the proportion of
                                 variation in the dependent variable that can be
                                 predicted from the independent variable.
                                 The adjusted R squared value differs from the
                                 R squared variable because R squared assumes that
                                 all independent variables affect the result of the
                                 model, however, adjusted R squared only considers
                                 the independent variables that have an effect on the model.
                                 The adjusted R squared is a better measure for
                                 models with different numbers of variables because
                                 an R squared will always increase if the number
                                 of variables increases"
                              ),
                         br(),
                         tags$li( "In this scenario the R squared value represents
                                  the proportion of variation in Attrition that
                                  can be predicted by Gender, Marital Status, Over Time, Business Travel, and Education Field. 
                                  The R Squared value falls between 0 and 1. 
                                  The closer the value is to 1, the better the 
                                  indepedent variable predicts variation in the dependent variable. 
                                  The independent variables shown above 
                                  (Gender, Marital Status, Over Time, Business Travel, and Education Field)
                                  all have R squared values of about 0.5 indicating that 
                                  they are all equally marginal predictors of variation in Attrition."
                                )
                     )

# HistogramChoices <- choices = c( "Age", "BusinessTravel",
#                                  "EducationField",
#                                  "EnvironmentSatisfaction", "Gender",
#                                  "JobSatisfaction",
#                                  "MaritalStatus", "OverTime",
#                                  "PercentSalaryHike",
#                                  "TotalWorkingYears",
#                                  "WorkLifeBalance",
#                                  "YearsAtCompany",
#                                  "YearsInCurrentRole"
#                                )
HistoryTags1 <- tags$ul( tags$li( "Healthcare burnout was a crisis long before
                                  the coronavirus pandemic hit.Rooted in the
                                  healthcare system, these systemic shortfalls
                                  include escalating workloads, inadequate support,
                                  underinvestment in public health infrastructure, etc."
                                ),
                         tags$li( "Burnout symptoms include “insomnia, depression,
                                  anxiety, post-traumatic stress disorder,
                                  and other mental health challenges
                                  (Murthy, 2022, N Engl J Med).” According to
                                  the New England Journal of Medicine,
                                  “some 52% of nurses (according to the
                                  American Nurses Foundation and 20% of
                                  doctors (Mayo Clinic Proceedings)
                                  say they are planning to leave their
                                  clinical practices (2022).”"
                                ),
                         tags$li( "The coronavirus pandemic escalated burnout
                                  symptoms in health care workers and left many
                                  of them on the brink of quitting.
                                  The pandemic caused acute staffing shortages,
                                  intensified safety protocol, increased uncertainty
                                  around available resources, and increased workloads."
                                ),
                         tags$li( "The Delta and Omicron surges sent many
                                  unvaccinated patients into hospitals where
                                  health care employees were already emotionally
                                  and physically exhausted from the last 18 months
                                  of the pandemic. Additionally, health care workers
                                  had to make unprecedented decisions,
                                  risk infection for themselves and their families,
                                  and experience the pain of losing many patients."
                                )
                        )
HistoryTags2 <- tags$ul( tags$li( tags$b( "This data set shows the critical need
                                          for changes to the healthcare system."
                                        )
                                ),
                         tags$li( tags$b( "Health care workers need to be
                                          protected and valued."
                                        ),
                                          "This involves ensuring proper PPE,
                                          which was not available during the
                                          beginning of the pandemic, adequate
                                          sick leave, and protection of staff
                                          from verbal and physical abuse."
                                ),
                         tags$li( tags$b( "We need to help reduce
                                          the administrative burdens."
                                        ),
                                          "According to one study,
                                          “outpatient physicians spend
                                          nearly 2 hours on the electronic
                                          health record and desk work during
                                          the day for every 1 hours spent with
                                          patients (Arndt, et al., 2017, Ann Fam Med).
                                          This takes away valuable time
                                          that could be spent with patients."
                                ),
                         tags$li( tags$b( "We need to increase access to mental healthcare."),
                                          "Due to their lack of schedule
                                          flexibility for visits and long working hours,
                                          many health care workers do not prioritize their mental health."
                                ),
                         tags$li( tags$b( "We need public investments in health
                                          infrastructure"
                                        ),
                                          "That focus on addressing the social
                                          determinants of health which would
                                          reduce the demands on the healthcare system."
                                ),
                         tags$li( tags$b( "We need to change the narrative that
                                          suffering is just an essential component
                                          of health professions."
                                        ),
                                  "Health care workers should be able to voice
                                  their mental health concerns or seek help
                                  without feeling isolated or embarrassed
                                  (Murthy, 2022, N Engl J Med)."
                               )
                      )
DashboardHeader <- dashboardHeader(title = "Healthcare Attrition")

SideBar <- dashboardSidebar(
                            sidebarMenu(
                                         menuItem( "Home", tabName = "Home"), 
                                         menuItem( "Who Quits?", 
                                                  tabName = "WhoQuits"
                                                 ),
                                         menuItem( "Your Likelihood of Quitting", 
                                                  tabName = "SelfHelp"
                                                 ),
                                         menuItem( "What Factors Matter the Most?", 
                                                  tabName = 
                                                  "WhatFactorsMatterTheMost"
                                                 ),
                                         menuItem( "Interactive Plots", 
                                                  tabName = "Graphs"
                                                 ),
                                         menuItem( "History", 
                                                   tabName = "History")
                                      )
                          )


HomeTab <- tabItem( tabName = "Home",
                    box( title = "Welcome!", background = "navy", width = "12",
                         status = "primary",
                         HTML( '<center><img src="group picture.jpeg" width="800"></center>'),
                         h3( p(  "This project was created by Sadie Charles Calame, 
                                 Ellen Dulin, Mary Jane McConnell, and Dylan Walmsley."
                              )
                           )
                      ), 
                    box(
                         title = "Quick Facts:", background = "navy", 
                         width = "12", status = "primary",
                         tags$ul(
                                  tags$li( "Healthcare employee attrition is a growing 
                                           problem in the United States. The COVID-19 
                                           pandemic has exacerbated this issue 
                                           in the past few years."), 
                                  br(),
                                  tags$li( "According to the 2022 NSI report, 
                                           the 2021 turnover by position was:"
                                         ),
                                  br(),
                                  tags$li( "Staff RNs:", tags$b( "27.1%")),
                                  tags$li( "Certified Nursing Assistant:", 
                                           tags$b( "35.5%")),
                                  tags$li( "Physician Assistant:", 
                                           tags$b( "10.7%")),
                                  tags$li( "Physical Therapist:", 
                                           tags$b( "13.6%")),
                                  tags$li( "Pharmacist:", tags$b( "10%")),
                                  tags$li( "Patient Care Tech:", 
                                           tags$b( "38.1%")),
                                  tags$li( "Radiologic Technologist:", 
                                           tags$b("17.5%")),
                                  br(),
                                  tags$li( "Our data was obtained from ", KaggleUrl, 
                                           "and pertains to healthcare employees quitting their jobs. 
                                           Here is the attrition rate based on the dataset explored:",
                                           h4( p( "There is a",
                                                 tags$b("12%"), "attrition rate."
                                               )
                                            )
                                        ),
                                 br(),
                                 box( plotOutput( "BarAttrition"))
                              )  
                        )
                  )
WhoQuitsTab <- tabItem( tabName = "WhoQuits", 
                        box( width = 7, plotOutput("BarCategoricalComparison"),
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
                          ),
                        
                        box(
                             h2( "Employee Lifestyle Effects:"),
                             plotOutput( "BarBusinessTravel"), 
                             plotOutput( "BarOvertime"),
                             plotOutput( "BarEnvirSatisfaction"),
                             plotOutput( "BarJobSatisfaction"),
                             plotOutput( "BarWorkLifeBalance")
                           ),
                        
                        #Summary Table
                        box( 
                             h2( "Summary Table"), tableOutput( "SummaryTable"),
                             selectInput( inputId = "SummaryData", 
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
                      )
                         
SelfHelpTab <- tabItem( tabName = "SelfHelp",
                        box( h2( "Estimating Your Chance of Quitting"),
                             width = 12),
                        box(
                          status = "primary",
                          title = "How the Percentage is Calculated",
                          background = "blue",
                          "We used a weighted average calculation to produce this estimate. 
                               First, your input values are matched against our dataset, and the percent of people in our dataset
                               who had that exact value AND quit is returned. Next, that percentage is multipled by a weighting factor. 
                               The weighting factor it is multipled by depends on your rankings. 
                               For example, if you answered 'How satisfied are you with your job?' with '3' and ranked 'JobSatisfaction' as the most important, 
                               the percentage of people who quit their job and had a job satisfaction of 3 would be multipled by the greatest weighting factor.
                               The nine percentages multipled by their corresponding weighting factors are then summed, and that sum is divided
                               by the sum of the weighting factors.",
                          "Don't worry if your estimation is high! This dataset is limited, and, therefore, our estimation is not perfect. It is simply an estimation.",
                          width = 12
                        ),
#Input values for the estimation         
                       box(
                            title =  "Input Your Values", background = "blue",
                            numericInput( "Age", "What is Your Age?", 40, 18, 60,
                                          step = 1
                                        ),
             
                            selectizeInput( "BusinessTravel",
                                            "How Often Do you Travel?", 
                                            c( "Non-Travel", "Travel_Rarely", 
                                               "Travel_Frequently"
                                              ),
                                            selected = NULL
                                          ),
             
                            selectizeInput( "Gender", "What Is Your Gender?", 
                                            c( "Male", "Female"), 
                                            selected = NULL
                                          ),
             
                            selectizeInput( "JobSatisfaction", 
                                          "How Satisfied Are You with Your Job?", 
                                          c( "Low", "Medium", "High", "Very High")
                                        ),
             
                            selectizeInput( "MaritalStatus", 
                                            "What's Your Marital Status?", 
                                            c( "Single", "Married", "Divorced"), 
                                            selected = NULL
                                          ),
             
                            selectizeInput( "MonthlyIncome", 
                                            "What Is Your Monthly Income?", 
                                            c( "0-3999", "4000-7999","8000-11999",
                                               "12000-15999", "16000-20000"
                                              )
                                          ),
             
                            numericInput( "TotalWorkingYears", 
                                          "How Many Years Have You Worked?", 
                                          20, 0, 40, step = 1
                                        ),
             
                            selectizeInput( "WorkLifeBalance", 
                                          "How Is Your Work/Life Balance?",
                                          c( "Bad", "Good", "Better", "Best")
                                        ),
             
                            numericInput( "YearsInCurrentRole", "How Many Years 
                                          Have You Worked in Your Current Role?",
                                          10, 0, 18, step = 1
                                        ),
                            width = 4
                          ),
           
#Ranking them to assign each value a weighting factor
                      box( title = "Importance Rank in Work Life",
                           background = "blue",
                           selectInput( "Rank1", "First", 
                                        choices = c( "", "Age", 
                                                     "BusinessTravel",
                                                     "Gender", 
                                                     "JobSatisfaction", 
                                                     "MaritalStatus",
                                                     "MonthlyIncome", 
                                                     "TotalWorkingYears",
                                                     "WorkLifeBalance", 
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
                            width = 4
                          ),
           
#Displaying the weighted average of the user's attrition values
                      box( title = "Your Likelihood of Quitting Is...",
                           background = "blue",
                           h2( textOutput(  "AttritionEstimation")),
                           width = 4
                         )
                      )
WhatFactorsMatterTheMostTab <- tabItem( tabName = "WhatFactorsMatterTheMost",
                                        box( plotOutput( "CategoricalRegression")),
                                        box( title = "Categorical Regression Explanation", 
                                             background = "navy", width = "12", 
                                             status = "primary", RSquaredTag
                                           )
                                      )
GraphsTab <- tabItem( tabName = "Graphs", 
                      #Histogram
                      box( h2( "Histogram"), plotOutput( "HistogramPlot"),
                                selectInput( "HistogramData","Choose an X-axis",
                                             choices = c( "Age", "BusinessTravel",
                                                          "EducationField",
                                                          "EnvironmentSatisfaction", "Gender",
                                                          "JobSatisfaction",
                                                          "MaritalStatus", "OverTime",
                                                          "PercentSalaryHike",
                                                          "TotalWorkingYears",
                                                          "WorkLifeBalance",
                                                          "YearsAtCompany",
                                                          "YearsInCurrentRole"
                                                         )
                                          )
             
                         ),
                      #Density Plot 
                      box( h2( "Density Plot"), plotOutput( "DensityPlot"),
                           selectInput( "DensityData", "Choose an X-axis", 
                                        choices = c( "MonthlyIncome", 
                                                     "PercentSalaryHike"
                                                   )
                                      )
                         ),
        
        
                      #Scatter Plot
                      box( h2( "Scatter Plot"), plotOutput( "ScatterPlot"), 
                           selectInput( "XScatterData", "Choose an X-axis", 
                                        choices = c( "MonthlyIncome", 
                                                     "PercentSalaryHike"
                                                   )
                                      ),
                           selectInput( "YScatterData", "Choose a Y-axis", 
                                        choices = c( "TotalWorkingYears", 
                                                     "YearsAtCompany"
                                                   )
                                      )
             
                        ),
                      #US Map
                      box( h2( "US Attrition by Region: June-Sept 2022"),
                           br(),
                           tags$li( "This data was obtained from the ", 
                                    a( "Bureau of Labor Statistics", 
                                       href = "https://www.bls.gov/news.release/pdf/jolts.pdf", 
                                       target = "_blank"
                                     )
                                  ),
                          leafletOutput( "UsMap"), 
                          selectInput( "MapMonth", "Choose a month",
                                        choices = c( "June", "July", "August",
                                                     "September"
                                                   )
                                     )
                        )
        
        
                    )
HistoryTab <- tabItem( tabName = "History", 
                       box( title = "COVID-19 and Healthcare Attrition:", 
                            background = "teal", solidHeader = TRUE,
                            width = 6, status = "primary", HistoryTags1
                          ),
                       box( title = "Steps to Take:", background = "green", 
                            width = 6, status = "primary", HistoryTags2
                          ),
                       box( title = "Consequences of Burnout on Health Care Workers:", 
                            background = "blue", width = "12", status = "primary",
                            HTML( '<center><img src="consequencesofhealthcareattrition.png" width="800"></center>'
                                ),
                            br(),
                            p( "Leo, C. G., Sabina, S., Tumolo, M. R., Bodini, 
                               A., Ponzini, G., Sabato, E., & Mincarone, P. (2020). 
                               Burnout Among Healthcare Workers in the COVID 19 Era:
                               A Review of the Existing Literature. 
                               Frontiers in Public Health, 9. 
                               https://doi.org/10.3389/fpubh.2021.750529"
                             )
                        ),
                       box( title = "Citations:", background = "blue", 
                            width = "12", status = "primary",
                            p( "Arndt, B. G., Beasley, J. W., Watkinson, M. D., 
                               Temte, J. L., Tuan, W. J., Sinsky, C. A., & Gilchrist, V. J. (2017). 
                               Tethered to the EHR: Primary Care Physician 
                               Workload Assessment Using EHR Event Log Data and 
                               Time-Motion Observations. 
                               Annals of family medicine, 15(5), 419–426. 
                               https://doi.org/10.1370/afm.2121"
                             ),
                            br(),
                            p( "Frogner BK, Dill JS. Tracking Turnover Among Health 
                               Care Workers During the COVID-19 Pandemic: 
                               A Cross-sectional Study. 
                               JAMA Health Forum. 2022;3(4):e220371. 
                              doi:10.1001/jamahealthforum.2022.0371"),
                           br(), 
                           p( "Leo, C. G., Sabina, S., Tumolo, M. R., Bodini, A., Ponzini, G., Sabato, E., & Mincarone, P. (2021). 
                                 Burnout Among Healthcare Workers in the COVID 19 Era: A Review of the Existing Literature. 
                                 Frontiers in public health, 9, 750529. https://doi.org/10.3389/fpubh.2021.750529"),
                          br(),
                          p( "Murthy, V. H., Interview with Dr. Vivek Murthy on approaches to supporting well-being among health workers. (13:50), 
                                 Author AffiliationsFrom the Office of the Surgeon General, Interview with Dr. Vivek Murthy on approaches to supporting well-being among health workers. (13:50), 
                                 Salmi, L., Longo, R. L. S. and D. L., & Shireman, R. (2022, August 18). Confronting health worker burnout and well-being: Nejm. New England Journal of Medicine.
                                 Retrieved December 7, 2022, from https://www.nejm.org/doi/full/10.1056/NEJMp2207252 ")
                        )
         
                    )

#The general layout of dashboard page contains a dashboardHeader and dashboardSidebar
dashboardPage( skin = "blue", 
               DashboardHeader, 
               SideBar, 
               dashboardBody( fluidRow(
                                         tabItems( HomeTab, WhoQuitsTab, 
                                                   SelfHelpTab, 
                                                   WhatFactorsMatterTheMostTab, 
                                                   GraphsTab, HistoryTab
                                                 )
                                      )                                      
                            )
            )



