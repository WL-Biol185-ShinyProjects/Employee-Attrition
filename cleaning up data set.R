#Cleaning up data set 
library(tidyverse)

watson_healthcare_clean <- watson_healthcare_modified %>% 
  select ( c( "Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"
, "Attrition"))

#write.csv(watson_healthcare_clean, file = "watson_healthcare_clean.csv")


watson_healthcare_reg <- watson_healthcare_modified %>% 
  select ( c( "Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"
              , "Attrition"))

