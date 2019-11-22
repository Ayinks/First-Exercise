churn <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/churn.csv')
library(plyr)
library(readr)
library(dplyr)
install.packages("pillar")
library(caret)
library(pillar)
glimpse(churn)
###to factor dataset
names <- c(1,2,5,6,8,10)
dat[,names] <- lapply(dat[,names] , factor)
##data partitioning
library(caTools)
##to split the dataset
spl = sample.split(churn$churn, SplitRatio = 0.6)
train = subset(churn, spl==TRUE)
test = subset(churn, spl==FALSE)
print(dim(train)); print(dim(test))
##first instantiate the algorithm
model_churn = glm(churn ~ . , family="binomial", data = train)
##summary of the trained model
summary(model_churn)
##checking baseline accuracy i.e reality in the original data
prop.table(table(train$churn))
##prediction of the training set
predictTrain = predict(model_churn, data = train, type = "response")
##confusion matrix with 0.5 threshold yes prediction forchurn response
table(train$churn, predictTrain >= 0.5)
##adding up true negative and true positive divided by nrow to determine accuracy of the model
(2508+100)/nrow(train)
##predict on test data
predictTest = predict(model_churn, newdata = test, type = "response")
table(test$churn, predictTest >= 0.5)
(1669+44)/nrow(test)
##accuracy for train and test are 87% and 86% respectively as against the baseline of 14% and 86%
##conclusion is the model work well