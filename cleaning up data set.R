#Cleaning up data set 
<<<<<<< HEAD
library(tidyverse)
watson_healthcare_modified %>% select ( c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", 
                                          "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", 
                                          "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", 
                                          "YearsAtCompany", "YearsInCurrentRole"))
=======
>>>>>>> 55d1d06c8b31e20278d3477ebb6b42e2557324fe

library(tidyverse)

watson_healthcare_clean <- watson_healthcare_modified %>% 
  select ( c("Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"))
