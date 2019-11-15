setwd("C:/Users/Khalid/Desktop/Data Science")
basket <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/store_data.csv')
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
library(tidyverse)
##applying the apriori rule on the transaction data
basket_rule <- apriori(basket,parameter = list(sup = 0.3, conf = 0.8, target="rules"))
### to check and detach a package
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm")
  {
  detach(package:tm, unload=TRUE)
}
## to check the rule
inspect(basket_rule[1:20])
###subseting redundant rules
basket_subset <- which(colSums(is.subset(basket_rule, basket_rule)) > 1)
###remove redundant rules
reduced_basket <- basket_rule[-basket_subset]
## check length of the subset
length(basket_subset)
### interested in items customers who bought yam also bought
Yams_rule <- 
  apriori(basket, parameter = list(supp=0.3, conf=0.8),appearance = list(default="lhs",rhs="yams="))
inspect(basket_rule_two[1:20]) ### there are similar rules with almost equal conf and lift
### summary of rule
summary(basket_rule_two)
### subset basket data into 2 halves
basket_one <- basket[1:3700,]
basket_two <- basket[3701:7500,]
## apply apriori on the two datasets
basket_rule_one <- apriori(basket_one,parameter = list(sup = 0.3, conf = 0.8, target="rules"))
basket_rule_two <- apriori(basket_two,parameter = list(sup = 0.3, conf = 0.8, target="rules"))

