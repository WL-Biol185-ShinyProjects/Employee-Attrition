
  # Define server logic required to draw a histogram

server <- function(input, output, session
                   ) 
{
  #Converting Character Vectors to Factors
  watson_healthcare_clean$Attrition <- factor(watson_healthcare_clean$Attrition
                                              )
  watson_healthcare_clean$BusinessTravel <- factor(watson_healthcare_clean$BusinessTravel
                                                   )
  
  #Output for Histogram
  output$HistogramPlot <- renderPlot(
    {
      ggplot(watson_healthcare_clean,
           aes_string(input$HistogramData, 
                      fill = watson_healthcare_clean$Attrition
                      )
           ) +
       geom_histogram(stat = "count"
                          ) + 
       ggtitle("Potential Predictors of Healthcare Employee Attrition"
                   )
     }
                                       )
  
  #Output for Density Plot 
  output$DensityPlot <- renderPlot(
    {
      ggplot(watson_healthcare_clean, 
          aes_string(input$DensityData, 
          fill = watson_healthcare_clean$Attrition
                     )
          ) +
          geom_density(
          ) +
          ggtitle("Density of Employee Attrition Versus Various Employee Characteristics")
     }
                                   )
  
  #Output for Scatter Plot 
  output$ScatterPlot <- renderPlot(
    {
      ggplot(watson_healthcare_clean, 
           aes_string(input$XScatterData, input$YScatterData)) +
           geom_point(stat = "identity")
     }
                                   )
  
  #Output for Estimation Feature
  #output$AttritionEstimation <- renderText({
  #  watson_healthcare_clean %>%
  #    group_by(input$Age)
  #})
  
  #Updating the ranking input choices


     watson_healthcare_clean %>%

  oldChoices <- colnames(watson_healthcare_clean)
  
  observeEvent(input$Rank2, 
               {
      
                  newChoices <- setdiff( oldChoices, 
                                         c(input$Rank1)
                                        )
                  if (input$Rank1 != "") {
                    
                    updateSelectInput(session, "Rank2", choices = newChoices
                    )
                  }
               }
  )
      
  observeEvent(input$Rank3, 
               {
    
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2
                                         )
                                       )
    
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank3", 
                                   choices = newChoices
                                   )
                 }
                }
               )
  
  observeEvent(input$Rank4, 
               {
    
                newChoices <- setdiff(oldChoices, 
                                      c(input$Rank1, 
                                        input$Rank2, 
                                        input$Rank3
                                        )
                                      )
    
                if(input$Rank1 != "") {
                  updateSelectInput(session, 
                                  "Rank4", 
                                  choices = newChoices
                                  )
                }
                }
               )
  
  observeEvent(input$Rank5, 
               {
    
                newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4
                                         )
                                      )
    
                if(input$Rank1 != "") {
                  updateSelectInput(session, 
                                  "Rank5", 
                                  choices = newChoices
                                  )
                }
                }
              )
  
  observeEvent(input$Rank6, 
               {
    
                newChoices <- setdiff(oldChoices, 
                                      c(input$Rank1, 
                                        input$Rank2, 
                                        input$Rank3, 
                                        input$Rank4, 
                                        input$Rank5
                                        )
                                      )
    
                if(input$Rank1 != "") {
                  updateSelectInput(session, 
                                   "Rank6", 
                                   choices = newChoices
                                   )
                }
               }
              )
  
  observeEvent(input$Rank7, 
               {
    
                newChoices <- setdiff(oldChoices, 
                                      c(input$Rank1, 
                                        input$Rank2, 
                                        input$Rank3, 
                                        input$Rank4, 
                                        input$Rank5, 
                                        input$Rank6
                                        )
                                      )
    
                if(input$Rank1 != "") {
                  updateSelectInput(session, 
                                  "Rank7", 
                                  choices = newChoices
                                  )
                }
               }
              )
  
  observeEvent(input$Rank8, 
               {
    
                newChoices <- setdiff(oldChoices, 
                                      c(input$Rank1, 
                                        input$Rank2, 
                                        input$Rank3, 
                                        input$Rank4, 
                                        input$Rank5, 
                                        input$Rank6, 
                                        input$Rank7
                                        )
                                      )
    
                if(input$Rank1 != "") {
                  updateSelectInput(session, 
                                  "Rank8", 
                                  choices = newChoices
                                  )
                }
               }
              )
  observeEvent(input$Rank9, 
               {
                 
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4, 
                                         input$Rank5, 
                                         input$Rank6,
                                         input$Rank7,
                                         input$Rank8
                                       )
                 )
                 
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank9", 
                                   choices = newChoices
                 )
                 }
               }
  
              )
  observeEvent(input$Rank10, 
               {
                 
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4, 
                                         input$Rank5, 
                                         input$Rank6,
                                         input$Rank7,
                                         input$Rank8,
                                         input$Rank9
                                       )
                 )
                 
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank10", 
                                   choices = newChoices
                  )
                 }
               }
               
             )
  observeEvent(input$Rank11, 
               {
                 
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4, 
                                         input$Rank5, 
                                         input$Rank6,
                                         input$Rank7,
                                         input$Rank8,
                                         input$Rank9,
                                         input$Rank10
                                       )
                 )
                 
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank11", 
                                   choices = newChoices
                 )
                 }
               }
               
             )
  observeEvent(input$Rank12, 
               {
                 
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4, 
                                         input$Rank5, 
                                         input$Rank6,
                                         input$Rank7,
                                         input$Rank8,
                                         input$Rank9,
                                         input$Rank10,
                                         input$Rank11
                                       )
                 )
                 
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank12", 
                                   choices = newChoices
                 )
                 }
               }
               
            )
  observeEvent(input$Rank13, 
               {
                 
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4, 
                                         input$Rank5, 
                                         input$Rank6,
                                         input$Rank7,
                                         input$Rank8,
                                         input$Rank9,
                                         input$Rank10,
                                         input$Rank11,
                                         input$Rank12
                                       )
                 )
                 
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank13", 
                                   choices = newChoices
                 )
                 }
               }
               
            )
  observeEvent(input$Rank14, 
               {
                 
                 newChoices <- setdiff(oldChoices, 
                                       c(input$Rank1, 
                                         input$Rank2, 
                                         input$Rank3, 
                                         input$Rank4, 
                                         input$Rank5, 
                                         input$Rank6,
                                         input$Rank7,
                                         input$Rank8,
                                         input$Rank9,
                                         input$Rank10,
                                         input$Rank11,
                                         input$Rank12,
                                         input$Rank13
                                       )
                 )
                 
                 if(input$Rank1 != "") {
                   updateSelectInput(session, 
                                   "Rank14", 
                                   choices = newChoices
                 )
                 }
               }
               
             )
  
  #Output for Bar Graphs 
  output$BarBusinessTravel <- renderPlot({
    watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, 
                                               levels = c("Yes", 
                                                          "No"
                                                          )
                                               )
 
