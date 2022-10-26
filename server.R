
# Define server logic required to draw a histogram

server <- function(input, output) {

<<<<<<< HEAD
      watson_healthcare_modified$Attrition <- factor(watson_healthcare_modified$Attrition)
      output$densityplot <- renderPlot({
        ggplot(watson_healthcare_modified, aes_string(input$attrition_data, fill = watson_healthcare_modified$Attrition)) +
        geom_histogram()  
      })
}
=======
watson_healthcare_clean$Attrition <- factor(watson_healthcare_clean$Attrition)
watson_healthcare_clean$BusinessTravel <- factor(watson_healthcare_clean$BusinessTravel)
output$histogramplot <- renderPlot({
  ggplot(watson_healthcare_clean, aes_string(input$histogram_data, fill = watson_healthcare_clean$Attrition)) +
    geom_histogram(stat = "count") + 
    ggtitle("Potential Predictors of Healthcare Employee Attrition")})
output$densityplot <-renderPlot({
 ggplot(watson_healthcare_clean, aes_string(input$density_data, fill = watson_healthcare_clean$Attrition)) +
    geom_density()
  })
}

>>>>>>> 1e51e246a3b84b960aeb32cf1d4165bfb186fad4
