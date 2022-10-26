
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
      
      #Below tabs are named and their contents is specified
      tabItems(
        tabItem(tabName = "Graphs", 
                h2("Histograms"),
                box(plotOutput("histogramplot"),
<<<<<<< HEAD
                    selectInput("attrition_data", 
                                "Choose an X-axis", 
                                choices = c("EmployeeID", "Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", 
                                                                                  "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", 
                                                                                  "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", 
                                                                                  "YearsAtCompany", "YearsInCurrentRole")
                    )
                ),
                
                h2("Scatterplots"),
                
                box(plotOutput("scatterplot"),
                    selectInput("attrition_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike")
                    )
                )
        ),
 
        
        tabItem(tabName = "History",
                h2("Let's learn some history"),
                h3("In the United States, healthcare employees experience some of the highest rates of burnout of any industry.")
        ),
        
        tabItem(tabName = "Self-Help",
                h("How to Measure Your Level of Burnout"),
                h("What You Can Do to Minimize Burnout")
        )
=======
                    selectInput("attrition_data", "Choose an X-axis", choices = c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", 
                                                                                  "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", 
                                                                                  "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", 
                                                                                  "YearsAtCompany", "YearsInCurrentRole"))),
                box(plotOutput("densityplot"),
                    selectInput("attrition_data", "Choose an X-axis", choices = c("MonthlyIncome", "PercentSalaryHike")))),
        tabItem(tabName = "History", h2("Let's learn some history"), h3("In the United States, 
                                                                        healthcare employees experience some of the highest rates of burnout of any industry. 
                                                                        This is due to long hours and sad outcomes.")
      ) 
>>>>>>> db38bb78a3896f4c6adf5f78cc1a606caaab773e
    )       
  )
)

