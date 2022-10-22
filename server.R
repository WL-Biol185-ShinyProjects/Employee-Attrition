
server <- function(input, output) {

  
  output$densityplot <- renderPlot({
    ggplot(watson_healthcare_modified, aes(x=output$var)) +
      geom_density(adjust = 1.5, alpha = 0.4). )}
    
    
  })

