
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
      menuItem("About", tabName = "About"),
      menuItem("Self-Help", tabName = "Self-Help")
    )
  ),

  dashboardBody(
      
    #Boxes need to be put in a row (or column)
    fluidRow(
    ),
    
    #Below tabs are named and their contents is specified
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
                  selectInput("density_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike"))
              )
      ),
      
      #The Second Tab Is the History Tab
      tabItem(tabName = "History", h2("Let's learn some history"), h3("In the United States, 
                                                                      healthcare employees experience some of the highest rates of burnout of any industry. 
                                                                      This is due to long hours and sad outcomes.")
      ),
      
      #The Third Tab Is the About Tab
      tabItem(tabName = "About", 
              box(
                h2("Thank you for visiting our page."),
                selectInput("attrition_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike")),
                selectInput("density_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany")),
              ),
                  
              #Scatter Plot
              box(plotOutput("scatterplot"), 
                  selectInput("x_scatter_data", "Choose an X-axis", choices = c("MonthlyIncome", "Percent Salary Hike")),
                  selectInput("y_scatter_data", "Choose a Y-axis", choices = c("TotalWorkingYears", "YearsAtCompany"))
              )
      ),

      #The Fourth Tab is the Self-Help Tab          
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
                sliderInput("MonthlyIncome", "What Is Your Monthly Income?", 1000,  20000, 10000),
                selectizeInput("Overtime", "Do You Work Overtime Often?", c("Yes", "No")),
                sliderInput("PercentSalaryHike", "How Much Has Your Salary Increased over Your Career?", 10, 25, 17),
                sliderInput("TotalWorkingYears", "How Many Years Have You Worked?", 0, 40, 20, step = 1),
                sliderInput("WorkLifeBalance", "How Is Your Work/Life Balance?", 1, 4, 2, step = 1),
                sliderInput("YearsAtCompany", "How Many Years Have You Worked at Your Current Company?", 0, 40, 20, step = 1),
                sliderInput("YearsInCurrentRole", "How Many Years Have You Worked in Your Current Role?", 0, 20, 10, step = 1),
                width = 12
              ),
              
              box(
                textOutput("AttritionEstimation")
              ),
              
              h2("What You Can Do to Minimize Burnout")
      )
    )
  )
)



