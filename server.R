
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
          geom_density(alpha = 0.3
          ) +
          ggtitle("Density of Employee Attrition Versus Various Employee Characteristics")
     }
                                   )
  
  #Output for Scatter Plot 
  output$ScatterPlot <- renderPlot(
    {
      ggplot(watson_healthcare_clean, 
           aes_string(input$XScatterData, input$YScatterData)) +
           geom_point(stat = "identity") +
           geom_smooth()
     }
                                   )
#Output for Summary Table
  output$SummaryTable <- renderTable(
    {
    # CountYes <- watson_healthcare_clean$Attrition == "Yes"      
    watson_healthcare_clean %>%
      group_by_at(input$SummaryData) %>%
      summarize(PercentAttrition = ((sum(Attrition == "Yes")) / n())*100) %>%
      arrange(desc(PercentAttrition))
     }
                                        )
  
  #Updating the ranking input choices
  oldChoices <- colnames(watson_healthcare_clean)
  
  observeEvent(input$Rank1, 
               {
      
                  newChoices <- setdiff( oldChoices, 
                                         c(input$Rank1)
                                        )
                  if (input$Rank1 != "") {
                    
                    updateSelectInput(session, 
                                      "Rank2", 
                                      choices = newChoices
                    )
                  }
               }
  )
      

  observeEvent( c(input$Rank1, input$Rank2), 
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
  
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3), 
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
  
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4), 
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
  
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5), 
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
  
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6), 
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
  
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7), 
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
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7, input$Rank8), 
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
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7, input$Rank8, input$Rank9), 
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
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7, input$Rank8, input$Rank9, input$Rank10), 
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
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7, input$Rank8, input$Rank9, input$Rank10, input$Rank11), 
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
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7, input$Rank8, input$Rank9, input$Rank10, input$Rank11, input$Rank12), 
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
  observeEvent( c(input$Rank1, input$Rank2, input$Rank3, input$Rank4, input$Rank5, input$Rank6, input$Rank7, input$Rank8, input$Rank9, input$Rank10, input$Rank11, input$Rank12, input$Rank13), 
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
  
  #Output for Estimation Feature
  
  output$AttritionEstimation <- renderText({
    
    ranked_columns <- c(input$Rank1,
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
                        input$Rank13,
                        input$Rank14
    )
    
    weight_factors <- c(10.4, 9.9, 9.4, 8.9, 8.4, 7.9, 7.4, 6.9, 6.4, 5.9, 5.4, 4.9, 4.4, 3.9)
    
    names(weight_factors) <- ranked_columns
    
    whc_length <- nrow(watson_healthcare_clean)
    
    percents <- c()
    
    groupedOnAge <- group_by_at(watson_healthcare_clean, "Age")
    summarizedByAge <- summarize(groupedOnAge, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent1 <- filter(summarizedByAge, Age == input$Age)
    percentAge <- percent1$AttritionPercent[1]
    weightedPercentAge <- percentAge * weight_factors["Age"]
    percents <- append(percents, weightedPercentAge)
    
    groupedOnBT <- group_by_at(watson_healthcare_clean, "BusinessTravel")
    summarizedByBT <- summarize(groupedOnBT, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent2 <- filter(summarizedByBT, BusinessTravel == input$BusinessTravel)
    percentBT <- percent2$AttritionPercent[1]
    weightedPercentBT <- percentBT * weight_factors["BusinessTravel"]
    percents <- append(percents, weightedPercentBT)
    
    groupedOnEF <- group_by_at(watson_healthcare_clean, "EducationField")
    summarizedByEF <- summarize(groupedOnEF, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent3 <- filter(summarizedByEF, EducationField == input$EducationField)
    percentEF <- percent3$AttritionPercent[1]
    weightedPercentEF <- percentEF * weight_factors["EducationField"]
    percents <- append(percents, weightedPercentEF)
    
    groupedOnES <- group_by_at(watson_healthcare_clean, "EnvironmentSatisfaction")
    summarizedByES <- summarize(groupedOnES, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent4 <- filter(summarizedByES, EnvironmentSatisfaction == input$EnvironmentSatisfaction)
    percentES <- percent4$AttritionPercent[1]
    weightedPercentES <- percentES * weight_factors["EnvironmentSatisfaction"]
    percents <- append(percents, weightedPercentES)
    
    groupedOnGen <- group_by_at(watson_healthcare_clean, "Gender")
    summarizedByGen <- summarize(groupedOnGen, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent5 <- filter(summarizedByGen, Gender == input$Gender)
    percentGen <- percent5$AttritionPercent[1]
    weightedPercentGen <- percentGen * weight_factors["Gender"]
    percents <- append(percents, weightedPercentGen)
    
    groupedOnJS <- group_by_at(watson_healthcare_clean, "JobSatisfaction")
    summarizedByJS <- summarize(groupedOnJS, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent6 <- filter(summarizedByJS, JobSatisfaction == input$JobSatisfaction)
    percentJS <- percent6$AttritionPercent[1]
    weightedPercentJS <- percentJS * weight_factors["JobSatisfaction"]
    percents <- append(percents, weightedPercentJS)
    
    groupedOnMS <- group_by_at(watson_healthcare_clean, "MaritalStatus")
    summarizedByMS <- summarize(groupedOnMS, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent7 <- filter(summarizedByMS, MaritalStatus == input$MaritalStatus)
    percentMS <- percent7$AttritionPercent[1]
    weightedPercentMS <- percentMS * weight_factors["MaritalStatus"]
    percents <- append(percents, weightedPercentMS)
    
    groupedOnMI <- group_by_at(watson_healthcare_clean, "MonthlyIncome")
    summarizedByMI <- summarize(groupedOnMI, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent8 <- filter(summarizedByMI, MonthlyIncome == input$MonthlyIncome)
    percentMI <- percent8$AttritionPercent[1]
    weightedPercentMI <- percentMI * weight_factors["MonthlyIncome"]
    percents <- append(percents, weightedPercentMI)
    
    groupedOnOT <- group_by_at(watson_healthcare_clean, "OverTime")
    summarizedByOT <- summarize(groupedOnOT, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent9 <- filter(summarizedByOT, OverTime == input$OverTime)
    percentOT <- percent9$AttritionPercent[1]
    weightedPercentOT <- percentOT * weight_factors["OverTime"]
    percents <- append(percents, weightedPercentOT)
    
    #attrition <- mean(percents)
    
    # filtered <- filter(watson_healthcare_clean, Age == input$Age & Attrition == "Yes")
    # filtered_length <- nrow(filtered)
    # percent <- filtered_length / whc_length
    # weighted_percent <- percent * weight_factors["Age"]
    # weighted_percent
    
  })
  
  #Output for Bar Graphs 
  output$BarBusinessTravel <- renderPlot({
    watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, 
                                               levels = c("Yes", 
                                                          "No"
                                                          )
                                               )
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
    
                                        }
                                       )
  
  output$BarOvertime <- renderPlot(
                                  {
    watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
    
    watson_healthcare_clean %>% 
      group_by(OverTime, Attrition) %>% 
      summarise(cnt = n()) %>% 
      mutate(freq = (cnt / sum(cnt))*100) %>% 
      ggplot(aes(x = OverTime, y = freq, fill = Attrition)) +
      geom_bar(position = position_stack(), stat = "identity", width = .7) +
      geom_text(aes(label = paste0(round(freq,0), "%")), 
                position = position_stack(vjust = 0.5), size = 3) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(title = "Over Time and Attrition", x = "Over Time", y = "Percentage") +
      scale_fill_manual(values = c("#fde725",  "#21918c"))
                                    }
                                    )
  
    
 #Output for Categorical Comparison Bar Graph
  output$BarCategoricalComparison <- renderPlot(
                                               {

    watson_healthcare_clean %>%
    group_by_at(input$XCategoricalComparisonData) %>%
    summarize(AttritionByCategory = ((sum(Attrition == "Yes")) / n()) * 100) %>%
    # arrange("AttritionByCategory") %>%
    # mutate(input$XCategoricalComparisonData = factor(input$XCategoricalComparisonData, levels = input$XCategoricalComparisonData, ordered = TRUE)) %>%
    ggplot(aes_string(input$XCategoricalComparisonData, "AttritionByCategory")) +
    geom_bar(stat = 'identity') + 
    labs(title = "Employee Attrition by Category", x = "Category", y = "Attrition Count"
        )
          
                                               }
                                               )
}
