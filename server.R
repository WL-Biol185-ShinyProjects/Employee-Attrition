
# Define server logic required to draw a histogram

server <- function(input, output, session) {
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
  #output$AttritionEstimation <- renderText({
  #  watson_healthcare_clean %>%
  #    group_by(input$Age)
  #})
  
  #Updating the ranking input choices
  oldChoices <- colnames(watson_healthcare_clean)
  
  observeEvent(input$Rank2, {
      
      newChoices <- setdiff(oldChoices, input$Rank1)
      
      updateSelectInput(sesson, "Rank2", choices = newChoices)
    }
  )
  
  observeEvent(input$Rank3, {
    
    newChoices <- setdiff(oldChoices, c(input$Rank1, input$Rank2))
    
    updateSelectInput(sesson, "Rank3", choices = newChoices)
    }
  )
  
  observeEvent(input$Rank4, {
    
    newChoices <- setdiff(oldChoices, c(input$Rank1, input$Rank2, input$Rank3))
    
    updateSelectInput(sesson, "Rank4", choices = newChoices)
    }
  )
  
  observeEvent(input$Rank5, {
    
    newChoices <- setdiff(oldChoices, c(input$Rank1, input$Rank2, input$Rank3, input$Rank4))
    
    updateSelectInput(sesson, "Rank5", choices = newChoices)
    }
  )
  
  observeEvent(input$Rank6, {
    
    newChoices <- setdiff(oldChoices, c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5))
    
    updateSelectInput(sesson, "Rank6", choices = newChoices)
    }
  )
  
  observeEvent(input$Rank7, {
    
    newChoices <- setdiff(oldChoices, c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6))
    
    updateSelectInput(sesson, "Rank7", choices = newChoices)
  }
  )
  
  observeEvent(input$Rank8, {
    
    newChoices <- setdiff(oldChoices, c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7))
    
    updateSelectInput(sesson, "Rank8", choices = newChoices)
  }
  )
  
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
  

