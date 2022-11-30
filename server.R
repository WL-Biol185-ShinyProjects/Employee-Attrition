
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
        labs(title = "Employee Attrition Count", y = "Count")
        
                   
     }
                                       )
  
  #Output for Density Plot 
  output$DensityPlot <- renderPlot(
    {
      ggplot(watson_healthcare_clean, 
          aes_string(input$DensityData, 
          fill = watson_healthcare_clean$Attrition,
        
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
    watson_healthcare_clean %>%
      group_by_at(input$SummaryData) %>%
      summarise(PercentAttrition = ((sum(Attrition == "Yes")) / n()) * 100) %>%
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
                    
                    updateSelectInput(session, "Rank2", choices = newChoices
                    )
                  }
               }
  )
      

  observeEvent( c(input$Rank1, input$Rank2), 
               {
                newChoices <- setdiff( oldChoices, 
                                        c( input$Rank1, 
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
    
    groupedOnPSH <- group_by_at(watson_healthcare_clean, "PercentSalaryHike")
    summarizedByPSH <- summarize(groupedOnPSH, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent10 <- filter(summarizedByPSH, PercentSalaryHike == input$PercentSalaryHike)
    percentPSH <- percent10$AttritionPercent[1]
    weightedPercentPSH <- percentPSH * weight_factors["PercentSalaryHike"]
    percents <- append(percents, weightedPercentPSH)
    
    groupedOnTWY <- group_by_at(watson_healthcare_clean, "TotalWorkingYears")
    summarizedByTWY <- summarize(groupedOnTWY, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent11 <- filter(summarizedByTWY, TotalWorkingYears == input$TotalWorkingYears)
    percentTWY <- percent11$AttritionPercent[1]
    weightedPercentTWY <- percentTWY * weight_factors["TotalWorkingYears"]
    percents <- append(percents, weightedPercentTWY)
    
    groupedOnWLB <- group_by_at(watson_healthcare_clean, "WorkLifeBalance")
    summarizedByWLB <- summarize(groupedOnWLB, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent12 <- filter(summarizedByWLB, WorkLifeBalance == input$WorkLifeBalance)
    percentWLB <- percent12$AttritionPercent[1]
    weightedPercentWLB <- percentWLB * weight_factors["WorkLifeBalance"]
    percents <- append(percents, weightedPercentWLB)
    
    groupedOnYAC <- group_by_at(watson_healthcare_clean, "YearsAtCompany")
    summarizedByYAC <- summarize(groupedOnYAC, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent13 <- filter(summarizedByYAC, YearsAtCompany == input$YearsAtCompany)
    percentYAC <- percent13$AttritionPercent[1]
    weightedPercentYAC <- percentYAC * weight_factors["YearsAtCompany"]
    percents <- append(percents, weightedPercentYAC)
    
    groupedOnYCR <- group_by_at(watson_healthcare_clean, "YearsInCurrentRole")
    summarizedByYCR <- summarize(groupedOnYCR, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent14 <- filter(summarizedByYCR, YearsInCurrentRole == input$YearsInCurrentRole)
    percentYCR <- percent14$AttritionPercent[1]
    weightedPercentYCR <- percentYCR * weight_factors["YearsInCurrentRole"]
    percents <- append(percents, weightedPercentYCR)
    
    #attrition <- mean(percents)
    
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
  #Environment Satisfaction and Attrition
  watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
  
  watson_healthcare_clean$EnvironmentSatisfaction = factor(watson_healthcare_clean$EnvironmentSatisfaction, levels = c("Low",
                                                                                                                       "Medium",
                                                                                                                       "High",
                                                                                                                       "Very High"))
  
  
  watson_healthcare_clean %>% 
    group_by(EnvironmentSatisfaction, Attrition) %>% 
    summarise(cnt = n()) %>% 
    mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = EnvironmentSatisfaction, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Environment Satisfaction and Attrition", x = "Environment Satisfaction", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))
    
  
  #Job Satisfaction and Attrition
  watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
  watson_healthcare_clean$JobSatisfaction = factor(watson_healthcare_clean$JobSatisfaction, levels = c("Low",
                                                                                                       "Medium",
                                                                                                       "High",
                                                                                                       "Very High"))
  
  watson_healthcare_clean %>% 
    dplyr::group_by(JobSatisfaction, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = JobSatisfaction, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Job Satisfaction and Attrition", x = "Job Satisfaction", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))
  
  #Work Life Balance and Attrition
  watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
  watson_healthcare_clean$WorkLifeBalance = factor(watson_healthcare_clean$WorkLifeBalance, levels = c("Bad",
                                                                                                       "Good",
                                                                                                       "Better",
                                                                                                       "Best"))
  
  watson_healthcare_clean %>% 
    dplyr::group_by(WorkLifeBalance, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = WorkLifeBalance, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Work Life Balance and Attrition", x = "Work Life Balance", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))
  
  
 #Output for Categorical Comparison Bar Graph
  output$BarCategoricalComparison <- renderPlot(
                                               {

 sum1 <-  watson_healthcare_clean %>%
    group_by_at(input$XCategoricalComparisonData) %>%
    summarize(AttritionByCategory = ((sum(Attrition == "Yes")) / n()) * 100) 
  
 colnames(sum1) [1] <- "PercentAttrition"
 
 sum1 %>%      
    arrange(AttritionByCategory) %>%
    mutate(PercentAttrition = factor(PercentAttrition, levels = PercentAttrition, ordered = TRUE)) %>%
    ggplot(aes(PercentAttrition, AttritionByCategory)) +
    geom_bar(stat = 'identity', color = "darkorchid1", fill = "blue3") +
    labs(title = "Percent Employee Attrition by Category", x = "Category", y = "Percent Attrition"
        )
                                               }
   watson_healthcare_reg$Attrition <- TRUE                                            )
   modelGender <- glm(Attrition ~ Gender, data = watson_healthcare_reg)
   coef(modelGender)
   modelEdu <- glm(Attrition ~ EducationField, data = watson_healthcare_reg)
   coef(modelEdu)

#Output for Regressions
   output$CategoricalRegression <- renderPlot(
                                             {
   watson_healthcare_reg %>%
   ggplot(aes(Gender, Attrition)) +
   geom_abline(slope =  1.000000e+00, intercept =  6.703746e-17) 
   
   
                                               
  


                                              }
                                              )
                                               
}
