library(shiny)

#Application Title 
titlePanel("Healthcare Employee Attrition")

#Show a plot of the generated distribution 
mainPanel(
  plotOutput("Plot"), 
  selectInput("column", "Choose an X-axis column", choices = colnames())
)