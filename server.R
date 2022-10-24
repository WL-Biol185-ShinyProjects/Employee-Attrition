
# Define server logic required to draw a histogram

server <- function(input, output) {

  
  output$densityplot <- renderPlot({
    ggplot(watson_healthcare_modified, aes_string(input$var)) +
      geom_density(adjust = 1.5, alpha = 0.4, fill = 1) 
  })
    
    
  }

