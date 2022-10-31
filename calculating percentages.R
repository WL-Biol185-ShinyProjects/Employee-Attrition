#Calculating Percentages

#Business Travel and Attrition
library(dplyr)
watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))
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

#Gender and Attrition
watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))

watson_healthcare_clean %>% 
  group_by(Gender, Attrition) %>% 
  summarise(cnt = n()) %>% 
  mutate(freq = (cnt / sum(cnt))*100) %>% 
  ggplot(aes(x = Gender, y = freq, fill = Attrition)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = paste0(round(freq,0), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Gender and Attrition", x = "Gender", y = "Percentage") +
  scale_fill_manual(values = c("#fde725",  "#21918c"))


#Overtime and Attrition
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



#Environment Satisfaction and Attrition
watson_healthcare_clean$Attrition = factor(watson_healthcare_clean$Attrition, levels = c("Yes", "No"))

watson_healthcare_clean$EnvironmentSatisfaction = factor(hr_data$EnvironmentSatisfaction, levels = c("Low",
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
watson_healthcare_clean$JobSatisfaction = factor(hr_data$JobSatisfaction, levels = c("Low",
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
watson_healthcare_clean$WorkLifeBalance = factor(hr_data$WorkLifeBalance, levels = c("Bad",
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


??newChoices
