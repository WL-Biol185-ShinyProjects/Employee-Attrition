
# Define server logic required to draw a histogram

server <- function(input, output) {

  watson_healthcare_modified$Attrition <- factor(watson_healthcare_modified$Attrition)
  output$densityplot <- renderPlot({
    ggplot(watson_healthcare_modified, aes_string(input$var, fill = watson_healthcare_modified$Attrition)) +
      geom_histogram()  
  })
    
    
  }

