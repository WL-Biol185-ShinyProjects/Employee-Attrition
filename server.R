<<<<<<< HEAD
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}
=======
>>>>>>> dd06e43792a2cf7799672320d1eb2dd7097b4623
