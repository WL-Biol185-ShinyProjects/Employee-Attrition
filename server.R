
# Define server logic required to draw a histogram

server <- function(input, output) {
  
watson_healthcare_clean$Attrition <- factor(watson_healthcare_clean$Attrition)
watson_healthcare_clean$BusinessTravel <- factor(watson_healthcare_clean$BusinessTravel)
output$histogramplot <- renderPlot({
  ggplot(watson_healthcare_clean, aes_string(input$attrition_data, fill = watson_healthcare_clean$Attrition)) +
    geom_histogram(stat = "count") + 
    ggtitle("Potential Predictors of Healthcare Employee Attrition")})

output$scatterplot <-renderPlot({
 ggplot(watson_healthcare_clean, aes_string(input$attrition_data, "EmployeeID")) +
    geom_point(stat = "identity")


output$densityplot <-renderPlot({
 ggplot(watson_healthcare_clean, aes_string(input$attrition_data, fill = watson_healthcare_clean$Attrition)) +
    geom_density(stat = "density", position = "identity")

  })
}

