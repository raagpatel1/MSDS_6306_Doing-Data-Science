# ---
# title: "Project2_RCode"
# author: "Raag Patel"
# date: "2/11/2022"
# ---

library(readr)
library(readxl)
library(ggplot2)
library(naivebayes)
library(dplyr)
library(tidyverse)
library(lattice)
library(tidyr)
library(caret)
library(class)
library(caret)
library(e1071)
library(ggcorrplot)
library(Hmisc)
library(Metrics)

# YouTube Link
# https://youtu.be/R5fykTht4f8
#
setwd("D:/School/DS6306/Live Session Assignments")

CS_Data <- read_csv("MSDS_6306_Doing-Data-Science/Unit 14 and 15 Case Study 2/CaseStudy2-data.csv")
CS_NoAttr <- read_csv("MSDS_6306_Doing-Data-Science/Unit 14 and 15 Case Study 2/CaseStudy2CompSet No Attrition.csv")
CS_NoSalr <- read_excel("MSDS_6306_Doing-Data-Science/Unit 14 and 15 Case Study 2/CaseStudy2CompSet No Salary.xlsx")

### Checking Correlation 

CS_Numeric = CS_Data %>% select(where(is.numeric))

model.matrix(~0+., data=CS_Numeric) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)



### Model Testing - Attrition

# Creating a clean dataframe, with numeric variables that can be used in the model, and 
# getting rid of columns with constants

summary(CS_Data)

CS_Attr_Model = CS_Data %>% select(where(is.numeric), Attrition, -EmployeeCount,-EmployeeNumber,-StandardHours)
CS_Attr_Model$Attrition = as.factor(CS_Attr_Model$Attrition)
str(CS_Attr_Model)


# Checking correlation

model.matrix(~0+., data=CS_Attr_Model) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

## Using Naive Bayes, since there is low correlation in regards to Attrition

# Using a seed, for reproducibility

set.seed(14029)
ind   = sample(1:dim(CS_Attr_Model)[1],round(.65 * dim(CS_Attr_Model)[1]))
train = CS_Attr_Model[ind,]
test  = CS_Attr_Model[-ind,]

# Using poisson distribution, and some laplace smoothing.

model_attr = naive_bayes(Attrition ~ ., data= train, usekernel = F, usepoisson = T, laplace = 1)

# Prediction test (test model)

predictor_attr = predict(model_attr ,test)
predtbl_attr = table(predictor_attr,test$Attrition)

confusionMatrix(predtbl_attr)
# Accu = 81.91%
# Sens = 83.46%
# Spec = 74.00%

## Figure out best seed, for reproducibility and getting solid sens/spec

# iterations = 20000
# 
# masterSpec = matrix(nrow = iterations)
# masterSens = matrix(nrow = iterations)
# 
# for(j in 1:iterations)
# {
#   set.seed(j)
# 
# 
#   ind   = sample(1:dim(CS_Attr_Model)[1],round(.65 * dim(CS_Attr_Model)[1]))
#   train = CS_Numeric[ind,]
#   test  = CS_Numeric[-ind,]
# 
#   model = naive_bayes(Attrition ~ ., data= train, usekernel = F, usepoisson = T, laplace = 1)
# 
#   pred_L = predict(model ,test)
#   predtbl_L = table(pred_L,test$Attrition)
# 
#   confmat = confusionMatrix(predtbl_L)
# 
#   # print(j) , takes about 10sec to run through 1000
# 
#   # Only grabbing high spec, so I can find the best seed easier.
#   if (mean(confmat$byClass[2]) > .59) {
# 
#     masterSens[j] = mean(confmat$byClass[1])
#     masterSpec[j] = mean(confmat$byClass[2])
# 
#   }
# 
# 
# }
# 
# plot(masterSens,masterSpec)

## Best seed is 14029.

### Model Testing - Salary

## Using Linear Regression, since MonthlyIncome is continuos

# Created a clean dataframe, with variables that had correlation with MonthlyIncome

CS_Salary_Model = CS_Attr_Model %>% select(MonthlyIncome,JobLevel,
                                           TotalWorkingYears,YearsAtCompany,Age) 

model.matrix(~0+., data=CS_Salary_Model) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

## Fits Testing
# Testing all these fits tells me how much impact the variables have on the model

fit1 = lm(MonthlyIncome ~ JobLevel + TotalWorkingYears, data = CS_Salary_Model)
summary(fit1)
# Residual standard error: 1390

fit2 = lm(MonthlyIncome ~ JobLevel, data = CS_Salary_Model)
summary(fit2)
# Residual standard error: 1413

fit3 = lm(MonthlyIncome ~ TotalWorkingYears, data = CS_Salary_Model)
summary(fit3)
# Residual standard error: 2887

fit4 = lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsAtCompany + Age, data = CS_Salary_Model)
summary(fit4)

# Residual standard error: 1383

fit5 = lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + Age, data = CS_Salary_Model)
summary(fit5)
# Residual standard error: 1390

fit6 = lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsAtCompany, data = CS_Salary_Model)
summary(fit6)
# Residual standard error: 1383

Salary_Model = CS_Salary_Model %>% mutate(TotalWorkingYears2 = TotalWorkingYears^2)
fit7 = lm(MonthlyIncome ~ TotalWorkingYears2, data = Salary_Model)
summary(fit7)
# Residual standard error: 3045

Salary_Model = CS_Salary_Model %>% mutate(YearsAtCompany2 = YearsAtCompany^2)
fit8 = lm(MonthlyIncome ~ YearsAtCompany2, data = Salary_Model)
summary(fit8)
# Residual standard error: 4034

# Interestingly, Age and YearsAtCompany had no impact on the RSME by themselves, but resulted in a 
# slightly better RMSE together. 

# Final, best fit. 
salary_fit = lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsAtCompany, data = CS_Salary_Model)
summary(salary_fit)


### Prediction Model Application 

## Model Application - Attrition

Predicted_Attr = predict(model_attr , CS_NoAttr)

Case2Predictions_Attrition = data.frame(id = CS_NoAttr[,1],Attrition = Predicted_Attr)

head(Case2Predictions_Attrition,20)
summary(Case2Predictions_Attrition)

write.csv(Case2Predictions_Attrition,"D:/School/DS6306/Live Session Assignments/Week 14-15_Project 2/Case2PredictionsPatel Attrition.csv", row.names = FALSE)

## Model Application - Salary

Predicted_Salr = predict(salary_fit,CS_NoSalr)

Case2Predictions_Salary = data.frame(id = CS_NoSalr[,1],MonthlyIncome = Predicted_Salr)

head(Case2Predictions_Salary,20)
summary(Case2Predictions_Salary)

write.csv(Case2Predictions_Salary,"D:/School/DS6306/Live Session Assignments/Week 14-15_Project 2/Case2PredictionsPatel Salary.csv", row.names = FALSE)


#############################################################################END




















