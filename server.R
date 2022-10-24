
# Define server logic required to draw a histogram

server <- function(input, output) {

  
  output$densityplot <- renderPlot({
    ggplot(watson_healthcare_modified, aes_string(input$var, fill = watson_healthcare_modified$Attrition)) +
      geom_histogram()  
  })
    
    
  }

