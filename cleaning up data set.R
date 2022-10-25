#Cleaning up data set 
library(tidyverse)

watson_healthcare_clean <- watson_healthcare_modified %>% 
  select ( c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"))

<<<<<<< HEAD
write.csv(watson_healthcare_clean, file = "healthcare_clean.csv")
=======




>>>>>>> 7d44f5ab4cf9ea5d840609386bb8af6fdffc47a5
