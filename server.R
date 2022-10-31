
# Define server logic required to draw a histogram

server <- function(input, output) {
  #Converting Character Vectors to Factors
  watson_healthcare_clean$Attrition <- factor(watson_healthcare_clean$Attrition)
  watson_healthcare_clean$BusinessTravel <- factor(watson_healthcare_clean$BusinessTravel)
  
  #Output for Histogram
  output$histogramplot <- renderPlot({
    ggplot(watson_healthcare_clean, aes_string(input$histogram_data, fill = watson_healthcare_clean$Attrition)) +
      geom_histogram(stat = "count") + 
      ggtitle("Potential Predictors of Healthcare Employee Attrition")
  })
  
  #Output for Density Plot 
  output$densityplot <- renderPlot({
   ggplot(watson_healthcare_clean, aes_string(input$density_data, fill = watson_healthcare_clean$Attrition)) +
      geom_density()+
      ggtitle("Density of Employee Attrition Versus Various Employee Characteristics")
  })
  
  #Output for Scatter Plot 
  output$scatterplot  <- renderPlot({
    ggplot(watson_healthcare_clean, aes_string(input$x_scatter_data, input$y_scatter_data)) +
      geom_point(stat = "identity")
  })
  
  #Output for Estimation Feature
  output$AttritionEstimation <- renderText({
    watson_healthcare_clean
      
  })
  
}

