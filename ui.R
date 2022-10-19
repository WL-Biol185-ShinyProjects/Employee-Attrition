
library(shiny)
library(shinydashboard)

<<<<<<< HEAD
#Application Title 
titlePanel("Healthcare Employee Attrition")

#Show a plot of the generated distribution 
mainPanel(
  plotOutput("Plot"), 
  selectInput("column", "Choose an X-axis column", choices = colnames())
)

=======
dashboardPage(
  dashboardHeader(title = "Healthcare Attrition", titleWidth = 750),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "Graphs"),
      menuItem("History", tabName = "History")
    )
  ),
  dashboardBody(
    #Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1"),
          selectInput("column", "Choose an X-axis column", choices = "Age", "Monthly Income")
          )
      )
    
    tabItems(
      tabItem(tabName = "Graphs"),
      tabItem(tabName = "History")
    )
  )
)
<<<<<<< HEAD
>>>>>>> dd06e43792a2cf7799672320d1eb2dd7097b4623
>>>>>>> 69380c63a9950c6b52e4982811524b0bb29e4ad0
=======

>>>>>>> ec6b977ebf4a528808d82ee7171dd5e332271c6d
