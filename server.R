
  # Define server logic 

server <- function(input, output, session
                   ) 
{
  
  library(readr)
  library(dplyr)
  watson_healthcare_modified <- read_csv("watson_healthcare_modified.csv")


  watson_healthcare_clean <- read_csv("watson_healthcare_clean.csv")
  
  
  #Recoding EnvironmentSatisfaction numerical vector as character vector 
  envNumVec <- c(1:4)
  envNewVec <- recode_factor(envNumVec, `1` = "Low", `2` = "Medium", `3` = "High", `4` = "Very High")
  envSatisfaction <- watson_healthcare_clean$EnvironmentSatisfaction
  envSatisfactionCharacter <- envNewVec[envSatisfaction]
  watson_healthcare_clean$EnvironmentSatisfaction <- envSatisfactionCharacter
  
  #Recoding JobSatisfaction numerical vector as a character vector 
  jobNumVec <- c(1:4)
  jobNewVec <- recode_factor(jobNumVec, '1' = "Low", '2' = "Medium", '3' = "High", '4' = "Very High")
  jobSatisfaction <- watson_healthcare_clean$JobSatisfaction
  jobSatisfactionCharacter <- jobNewVec[jobSatisfaction]
  watson_healthcare_clean$JobSatisfaction <- jobSatisfactionCharacter
  
  #Recoding WorkLifeBalance numerical vector as a character vector 
  WLBNumVec <- c(1:4)
  WLBNewVec <- recode_factor(WLBNumVec, '1' = "Bad", '2' = "Good", '3' = "Better", '4' = "Best")
  WLB <- watson_healthcare_clean$WorkLifeBalance
  WLBCharacter <- WLBNewVec [WLB]
  watson_healthcare_clean$WorkLifeBalance <- WLBCharacter
  
  
  #Creating a dataset so that the MonthlyIncome column can be a categorical variable
  #Allows the attrition estimation function to work
  whc <- read_csv("whc.csv")
  whc$MonthlyIncomeCat <- NA
  whc$MonthlyIncomeCat[whc$MonthlyIncome > 0 & whc$MonthlyIncome < 3999] <- "0-3999"
  whc$MonthlyIncomeCat[whc$MonthlyIncome > 4000 & whc$MonthlyIncome < 7999] <- "4000-7999"
  whc$MonthlyIncomeCat[whc$MonthlyIncome > 8000 & whc$MonthlyIncome < 11999] <- "8000-11999"
  whc$MonthlyIncomeCat[whc$MonthlyIncome > 12000 & whc$MonthlyIncome < 15999] <- "12000-15999"
  whc$MonthlyIncomeCat[whc$MonthlyIncome > 16000 & whc$MonthlyIncome < 20000] <- "16000-20000"
  
  #Converting Character Vectors to Factors
  watson_healthcare_clean$Attrition <- factor(watson_healthcare_clean$Attrition
                                              )
  watson_healthcare_clean$BusinessTravel <- factor(watson_healthcare_clean$BusinessTravel
                                                   )

  #usmap output
  #data
  Map_Data_New <- data.frame(June = c(7447, 22035, 11075, 12042),
                             July = c(7034, 25431,	10740, 11893), 
                             August = c(6901, 20738,	9870,	10690),
                             September = c(7406, 23805,	10076, 11424),
                             Region = c("Northeast", "South", "Midwest", "West"),
                             color = c("red", "orange", "palegreen", "lightblue"),
                             lat = c(42, 32, 39, 41),
                             lon = c(-75, -88, -103, -120)) %>%
    pivot_longer(cols = 1:4, 
                 names_to = "Month",
                 values_to = "Number")
  
 
  output$UsMap <- renderLeaflet(
    
    {
      Map_Data_New %>%
        filter(Month == input$MapMonth) %>%
        leaflet() %>%
        addTiles() %>%
        setView(-98.58, 38.82,  zoom = 3) %>%
        addCircleMarkers(
          fill = TRUE,
          fillColor = ~color,
          radius = ~Number/1000,
          stroke = FALSE,
          label = ~Number) %>%
        addLegend(data = Map_Data_New,
                  title = "Employee Attrition by Region",
                  colors = c("red", "orange", "palegreen", "lightblue"),
                  labels = c("Northeast", "South", "Midwest", "West"))
        

    }
    
    
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
           geom_smooth(method = "gam")
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
  oldChoices <- c("Age", "BusinessTravel", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "TotalWorkingYears", "WorkLifeBalance", "YearsInCurrentRole")
  
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
                        input$Rank9
    )
    
    weight_factors <- c(18, 16, 14, 12, 10, 8, 6, 4, 2)
    
    names(weight_factors) <- ranked_columns
    
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
    
    groupedOnMIC <- group_by_at(whc, "MonthlyIncomeCat")
    summarizedByMIC <- summarize(groupedOnMIC, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent8 <- filter(summarizedByMIC, MonthlyIncomeCat == input$MonthlyIncome)
    percentMIC <- percent8$AttritionPercent[1]
    weightedPercentMIC <- percentMIC * weight_factors["MonthlyIncome"]
    percents <- append(percents, weightedPercentMIC)
    
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
    
    groupedOnYCR <- group_by_at(watson_healthcare_clean, "YearsInCurrentRole")
    summarizedByYCR <- summarize(groupedOnYCR, AttritionPercent = ((sum(Attrition == "Yes")) / n())*100)
    percent14 <- filter(summarizedByYCR, YearsInCurrentRole == input$YearsInCurrentRole)
    percentYCR <- percent14$AttritionPercent[1]
    weightedPercentYCR <- percentYCR * weight_factors["YearsInCurrentRole"]
    percents <- append(percents, weightedPercentYCR)
    
    attrition <- (sum(percents) / 90)
    
  })
  
  #Output for Bar Graphs 
  
  #Business Travel and Attrition
   output$BarBusinessTravel <- renderPlot(
                                         {
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
       labs(title = "Business Travel and Attrition", x = "Business Travel", y = "Percentage") +
       scale_x_discrete(breaks = c("Travel_Rarely", "Travel_Frequently", "Non-Travel"),
                        labels = c("Travel Rarely", "Travel Frequently", "Non Travel")) +
       scale_fill_manual(values = c("skyblue1", "darkseagreen1"))
     
                                         }
                                        )
 #Overtime and Attrition 
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
      labs(title = "Over Time and Attrition", x = "Over Time", y = "Percentage") +
      scale_fill_manual(values = c("skyblue1",  "darkseagreen1"))
                                    }
                                    )

  #Environment Satisfaction and Attrition
  output$BarEnvirSatisfaction <- renderPlot(
                                           {
  watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
  
  
  watson_healthcare_clean %>% 
    group_by(EnvironmentSatisfaction, Attrition) %>% 
    summarise(cnt = n()) %>% 
    mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = EnvironmentSatisfaction, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    labs(title = "Environment Satisfaction and Attrition", 
         x = "Environment Satisfaction", y = "Percentage") +
    
    scale_fill_manual(values = c("skyblue1",  "darkseagreen1"))
                                           }
                                           )
    
  
  #Job Satisfaction and Attrition
  output$BarJobSatisfaction <- renderPlot(
                                         {
  watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))

  
  watson_healthcare_clean %>% 
    group_by(JobSatisfaction, Attrition) %>% 
    summarise(cnt = n()) %>% 
    mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = JobSatisfaction, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    labs(title = "Job Satisfaction and Attrition", 
    x = "Job Satisfaction", y = "Percentage") +
    
    scale_fill_manual(values = c("skyblue1",  "darkseagreen1"))
                                         }
                                         )
  
  #Work Life Balance and Attrition
  output$BarWorkLifeBalance <- renderPlot(
                                         {
  watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
 
  
  watson_healthcare_clean %>% 
    group_by(WorkLifeBalance, Attrition) %>% 
    summarise(cnt = n()) %>% 
    mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = WorkLifeBalance, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    labs(title = "Work Life Balance and Attrition", 
         x = "Work Life Balance", y = "Percentage") +
    
    scale_fill_manual(values = c("skyblue1",  "darkseagreen1"))
                                         }
                                         )

  
  
#Percent Attrition Bar Graphs
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
                                               )


#Loading watson_healthcare_reg dataset 
watson_healthcare_reg <- read_csv("watson_healthcare_reg.csv")

#Making Attrition a Boolean vector
watson_healthcare_reg$Attrition <- TRUE

#Calculating adjusted Rsquared values and placing them into variables

summaryModelGender <- summary(lm(Attrition ~ Gender, data = watson_healthcare_reg))$adj.r.squared
summaryModelEdu <- summary(lm(Attrition ~ EducationField, data = watson_healthcare_reg))$adj.r.squared
summaryModelMarital <- summary(lm(Attrition ~ MaritalStatus,  data = watson_healthcare_reg))$adj.r.squared                          
summaryModelOverTime <- summary(lm(Attrition ~ OverTime,  data = watson_healthcare_reg))$adj.r.squared
summaryModelBusinessTravel <- summary(lm(Attrition ~ BusinessTravel,  data = watson_healthcare_reg))$adj.r.squared

#Generating new data frame 
regressionData <- data.frame( Category = c( "Gender", 
                                           "EducationField", 
                                           "MaritalStatus", 
                                           "OverTime", 
                                           "BusinessTravel"
                                          ),
                              AdjustedRSquared = c( summaryModelGender, 
                                                     summaryModelEdu, 
                                                     summaryModelMarital, 
                                                     summaryModelOverTime, 
                                                     summaryModelBusinessTravel
                                                    )
                             )

#Output for bar graph that displays R squared values
 output$CategoricalRegression <- renderPlot(
                                           {
 regressionData %>%
    ggplot(aes(Category, AdjustedRSquared)) +
    geom_bar(stat = "identity")
                                           }
                                           )
 

                                               
#Output for Graph on Home Tab showing Attrition


output$BarAttrition <- renderPlot(
  {
    
    watson_healthcare_clean %>%
      group_by(Attrition) %>%
      summarise(cnt = n()) %>%
      mutate(freq = (cnt / sum(cnt))*100) %>%
      ggplot(aes(x = Attrition, y = freq, fill = Attrition)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(freq,0), "%")),
                position = position_stack(vjust = 0.5), size = 3) +
      labs(title = "Attrition", x = "Attrition", y = "Percentage") +
      scale_fill_manual(values = c("lightblue", "lightgreen"))
    
  })
    
} 

