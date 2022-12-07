#Cleaning up data set 
library(tidyverse)

watson_healthcare_clean <- watson_healthcare_modified %>% 
  select( c( "Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"
, "Attrition"))


write.csv(watson_healthcare_clean, file = "watson_healthcare_clean.csv")

watson_healthcare_reg <- watson_healthcare_modified %>% 
  select ( c( "Age", "BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "OverTime", "PercentSalaryHike", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole"
              , "Attrition"))

write.csv(watson_healthcare_reg, file = "watson_healthcare_reg.csv")

#Creating a dataset so that the MonthlyIncome column can be a categorical variable
#Allows the attrition estimation function to work
whc <- watson_healthcare_clean
write.csv(whc, file = "whc.csv")
whc$MonthlyIncomeCat <- NA
whc$MonthlyIncomeCat[whc$MonthlyIncome > 0 & whc$MonthlyIncome < 3999] <- "0-3999"
whc$MonthlyIncomeCat[whc$MonthlyIncome > 4000 & whc$MonthlyIncome < 7999] <- "4000-7999"
whc$MonthlyIncomeCat[whc$MonthlyIncome > 8000 & whc$MonthlyIncome < 11999] <- "8000-11999"
whc$MonthlyIncomeCat[whc$MonthlyIncome > 12000 & whc$MonthlyIncome < 15999] <- "12000-15999"
whc$MonthlyIncomeCat[whc$MonthlyIncome > 16000 & whc$MonthlyIncome < 20000] <- "16000-20000"


  


