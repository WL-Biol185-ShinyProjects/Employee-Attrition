
# Define server logic required to draw a histogram
function(input, output) {
  output$Plot <- renderPlot({
    
  })
}
=======
<<<<<<< HEAD
=======

>>>>>>> 03c39dfa7a83cadb6d1aff60cc21ff06b0c9af9f
=======


>>>>>>> db1bfb4957548c6f01812d8567bf51e12bb0a42b
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
<<<<<<< HEAD
}
=======
>>>>>>> dd06e43792a2cf7799672320d1eb2dd7097b4623
>>>>>>> 69380c63a9950c6b52e4982811524b0bb29e4ad0
=======
}
>>>>>>> 03c39dfa7a83cadb6d1aff60cc21ff06b0c9af9f
