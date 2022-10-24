#Cleaning up data set 

library(tidyverse)

watson_healthcare_clean <- watson_healthcare_modified %>% 
  select ( c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"))

