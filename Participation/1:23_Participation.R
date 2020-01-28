bankData = read.csv(
  "https://msudataanalytics.github.io//SSC442/Labs/data/bank.csv",
  header = TRUE)

# Removes column stating if the person subscribed to the product
bankSubset = bankData[,1:14]

# Since R creates dummy variables for us, we do not have do do anything to the factor variables

# Additionally, lets start by defining our first null hypothesis as 1 for comparison 
null1 <- lm(balance ~ 1, data = bankSubset)
full1 <- lm(balance ~ age, data = bankSubset)

# Run F test to see which model fits the data better : Note that Pr(>F) is the P value 
anova(null1, full1)
# Since p value is low reject the null hypothesis and include it into balance model

# The following code is repeating this process for each variable in the bank set

# Testing job
null2 <- lm(balance ~ age, data = bankSubset)
full2 <- lm(balance ~ age + job, data = bankSubset)
anova(null2, full2)
# p value is low --> reject null hypthesis --> include job into model

# Testing Marital 
null3 <- lm(balance ~ age + job, data = bankSubset)
full3 <- lm(balance ~ age + job + marital, data = bankSubset)
anova(null3, full3)
# p value is low --> reject null hypthesis --> include marital into model

# Testing Education '
null4 <- lm(balance ~ age + job + marital, data = bankSubset)
full4 <- lm(balance ~ age + job + marital + education, data = bankSubset)
anova(null4,full4)
# p value is low --> reject null hypthesis --> include eduaction into model


# Testing Default
null5 <- lm(balance ~ age + job + marital + education, data = bankSubset)
full5 <- lm(balance ~ age + job + marital + education + default, data = bankSubset)
anova(null5,full5)
# p value is low --> reject null hypthesis --> include defualt into model

# Testing Housing 
null6 <- lm(balance ~ age + job + marital + education + default, data = bankSubset)
full6 <- lm(balance ~ age + job + marital + education + default + housing, data = bankSubset)
anova(null6,full6)
# p value is not significant --> do not reject null hypthesis --> do not include housing into model

# Testing Loan 
null7 <- lm(balance ~ age + job + marital + education + default, data = bankSubset)
full7 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
anova(null7,full7)
# p value is low --> reject null hypthesis --> include loan into model

# Testing Contact 
null8 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
full8 <- lm(balance ~ age + job + marital + education + default + loan + contact, data = bankSubset)
anova(null8,full8)
# p value is not significant --> do not reject null hypthesis --> do not include contact into model

# Testing day 
null9 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
full9 <- lm(balance ~ age + job + marital + education + default + loan + day, data = bankSubset)
anova(null9,full9)
# p value is not significant --> do not reject null hypthesis --> do not include day into model 

# Testing month 
null10 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
full10 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
anova(null10,full10)
# p value is low --> reject null hypthesis --> include month into model
# Note: Although the F tests includes says the full10 model fits the data better, we might not that the month is seemingly irrelevant and exclude it from our model 

# Testing Duration 
null11 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
full11 <- lm(balance ~ age + job + marital + education + default + loan + month + duration, data = bankSubset)
anova(null11,full11)
# p value is not significant --> do not reject null hypthesis --> do not include duration into model 

# Testing campaign
null12 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
full12 <- lm(balance ~ age + job + marital + education + default + loan + month + campaign, data = bankSubset)
anova(null12,full12)
# p value is not significant --> do not reject null hypthesis --> do not include campaign into model 

# Testing Previous 
null13 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
full13 <- lm(balance ~ age + job + marital + education + default + loan + month + previous, data = bankSubset)
anova(null13, full13)
# p value is not significant --> do not reject null hypthesis --> do not include previous into model 

# According to these tests the linear regression that fits the data the most is 
fit_balance_model<- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)

# However, I believe that month is insignificant in the actual interpretation of this data, therefore I would omit it from the 
# variables in the regression and use this model instead
# The final model includes the variables age, job, marital, education, default, and loan to predict balance
final_balance_model <- lm(balance ~ age + job + marital + education + default + loan , data = bankSubset)



