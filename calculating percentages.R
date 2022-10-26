#Calculating Percentages
library(dplyr)
watson_healthcare_clean %>%
    group_by( MaritalStatus, Attrition ) %>%
    summarise( Freq=n())

#spread yes and no for attrition