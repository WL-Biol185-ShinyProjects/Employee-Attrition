
# Define server logic required to draw a histogram

server <- function(input, output) {

watson_healthcare_modified$Attrition <- factor(watson_healthcare_modified$Attrition)
watson_healthcare_modified$BusinessTravel <- factor(watson_healthcare_modified$BusinessTravel)
output$densityplot <- renderPlot({
  ggplot(watson_healthcare_modified, aes_string(input$attrition_data, fill = watson_healthcare_modified$Attrition)) +
  geom_histogram(stat = "count")  
  })
}

