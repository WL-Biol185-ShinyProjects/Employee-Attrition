













library(shiny)
library(leaflet)
library(tidyverse)

# Define server logic to plot various variables  
function(input, output) {
  
}
#example that he did on the board
output$plot <- renderPlot ({
  ggplot(watson_healthcare_modified, aes _string(input$column, "mpg")) + geom_point()

  })

}