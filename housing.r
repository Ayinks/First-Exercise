housing <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/housing.csv')
head(housing)
glimpse(housing)
new_housing <- na.omit(housing)
summary(housing[,1:9])
library(caTools) #for data partitioning
spl = sample.split(new_housing$median_house_value, SplitRatio = 0.5)
train = subset(new_housing, spl==TRUE)
test = subset(new_housing, spl==FALSE)
print(dim(train)); print(dim(test))
##check for correlation, the closer to 1 the stronger +ve cor
## -ve cor indicate inverse relationship
cor(train$median_house_value, train$median_income)
housing_graph <- ggplot(train, aes(x=median_house_value, y=median_income))+geom_point()
library(hexbin)
# hexbin plot for clearer view
hbin <- hexbin(x=train$median_house_value, y=train$median_income, xbins = 40)  
plot(hbin)
## building linear model i.e training the train data
housing_lm <- lm(median_income ~ median_house_value, data=train)
print(housing_lm)
#from the coefficients, b0 is the intercept, b1 is median_house_value
## this means we now have a formula to calculate median income if median_house_value is know
##i.e median_income = 1.542 + 1.124e-05.median_house_value
##to ensure the model is statistically significant, check the p-value
##model is considered statistically significant if both p-values are less than 0.05
## a larger t-value means the coeff is less likely not 
##equal to zero by chance. so the higher the t-value, the better
##Pr(>|t|) is the prob that you get a t-value as high or higher than the observed value when H0 is true
summary(housing_lm)
##R squared checks for variation in the dependent variable, the higher the better
##bcos the total info in a variable is the amount of variation it contains
##R squared adjusted is required to check the variation added through addition of new variables
##predict on the test data
income_prediction <- predict(housing_lm, test)
# make actuals_prediction dataframe to compare with original income data
actual_prediction <- data.frame(cbind(actuals=test$median_income, predicted=income_prediction))  
##check correlation
income_accuracy <- cor(actual_prediction)  # 68%
print(income_accuracy)
head(actual_prediction)
