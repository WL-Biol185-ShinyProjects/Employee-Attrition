
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
  
  #Output for Bar Graphs 
  output$barbusinesstravel <- renderPlot({
    watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
    watson_healthcare_clean %>%
      group_by(BusinessTravel, Attrition) %>%
      summarise(cnt = n()) %>%
      mutate(freq = (cnt / sum(cnt))*100) %>%
      ggplot(aes(x = BusinessTravel, y = freq, fill = Attrition)) + 
      geom_bar(position = position_stack(), stat = "identity", width = .7) +
      geom_text(aes(label = paste0(round(freq,0), "%")), 
                position = position_stack(vjust = 0.5), size = 3) +
      scale_x_discrete(breaks = c("Travel_Rarely", "Travel_Frequently", "Non-Travel"),
                       labels = c("Travel Rarely", "Travel Frequently", "Non Travel")) +
      scale_fill_manual(values = c("#fde725", "#21918c"))
  })
}
  
  #Output for Estimation Feature
  # output$AttritionEstimation <- renderText({
  #   watson_healthcare_clean %>%
  

