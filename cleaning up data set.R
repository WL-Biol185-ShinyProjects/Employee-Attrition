#Cleaning up data set 
library(tidyverse)
<<<<<<< HEAD
watson_healthcare_modified %>% select ( c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"))
=======

watson_healthcare_clean <- watson_healthcare_modified %>% 
  select ( c( "Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"
, "Attrition"))


>>>>>>> db38bb78a3896f4c6adf5f78cc1a606caaab773e


