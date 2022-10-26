#Summary Table
#summary table that worked
summarytable <- data.frame(Category=c ("BusinessTravel"), 
                           Subcategory=c ("TravelRarely", "TravelFrequently", "Non-Travel"),
                           PercentageYesToAttrition=c (1, 1, 1),
                           PercentageNoToAttrition=c (1, 1, 1))

write.csv(summarytable, file = "summarytable.csv")
#Questions to ask: How to calculate the percentages? and How to split cells to make three rows in one cell?


#summary table that I was messing with 
summarytable <- data.frame(Category=c ("BusinessTravel", "EducationField", "EnvironmentSatisfaction", "Gender", "JobSatisfaction", "MaritalStatus", "OverTime", "WorkLifeBalance"), 
                  Subcategory=c ("TravelRarely", "TravelFrequently", "Non-Travel", "LifeSciences", "Other", "Medical", "Marketing", "TechnicalDegree", "HumanResources"),
                  PercentageYesToAttrition=c (1, 1, 1),
                  PercentageNoToAttrition=c (1, 1, 1))

 
write.csv(summarytable, file = "summarytable.csv")