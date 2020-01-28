bankData = read.csv(
  "https://msudataanalytics.github.io//SSC442/Labs/data/bank.csv",
  header = TRUE)

# Removes column stating if the person subscribed to the product
bankSubset = bankData[,1:14]

# Since R creates dummy variables for us, we do not have do do anything to the factor variables

# Define the reduced and full models 
red1 <- lm(balance ~ 1, data = bankSubset)
full1 <- lm(balance ~ age, data = bankSubset)

# Run F test to see which model fits the data better : Note that Pr(>F) is the P value 
# Also our Ho is that models do not differ significantly in all cases 
# Also our Ha is that the full model is significantly better in all cases
anova(red1, full1)
# Since p value is low reject the null hypothesis and say the full model is significantly better --> therefore include age in our model

# The following code is repeating this process for each variable in the bank set

# Testing job
red2 <- lm(balance ~ age, data = bankSubset)
full2 <- lm(balance ~ age + job, data = bankSubset)
anova(red2, full2)
# p value is low --> reject null hypthesis --> full model is significantly better --> include job into model

# Testing Marital 
red3 <- lm(balance ~ age + job, data = bankSubset)
full3 <- lm(balance ~ age + job + marital, data = bankSubset)
anova(red3, full3)
# p value is low --> reject null hypthesis --> full model is significantly better --> include marital into model

# Testing Education '
red4 <- lm(balance ~ age + job + marital, data = bankSubset)
full4 <- lm(balance ~ age + job + marital + education, data = bankSubset)
anova(red4,full4)
# p value is low --> reject null hypthesis --> full model is significantly better --> include education into model


# Testing Default
red5 <- lm(balance ~ age + job + marital + education, data = bankSubset)
full5 <- lm(balance ~ age + job + marital + education + default, data = bankSubset)
anova(red5,full5)
# p value is low --> reject null hypthesis --> full model is significantly better --> include defualt into model

# Testing Housing 
red6 <- lm(balance ~ age + job + marital + education + default, data = bankSubset)
full6 <- lm(balance ~ age + job + marital + education + default + housing, data = bankSubset)
anova(red6,full6)
# p value is not significant --> do not reject null hypthesis --> models do not differ significantly --> do not include housing into model

# Testing Loan 
red7 <- lm(balance ~ age + job + marital + education + default, data = bankSubset)
full7 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
anova(red7,full7)
# p value is low --> reject null hypthesis --> full model is significantly better --> include loan into model

# Testing Contact 
red8 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
full8 <- lm(balance ~ age + job + marital + education + default + loan + contact, data = bankSubset)
anova(red8,full8)
# p value is not significant --> do not reject null hypthesis --> models do not differ significantly --> do not include contact into model

# Testing day 
red9 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
full9 <- lm(balance ~ age + job + marital + education + default + loan + day, data = bankSubset)
anova(red9,full9)
# p value is not significant --> do not reject null hypthesis --> models do not differ significantly --> do not include day into model 

# Testing month 
red10 <- lm(balance ~ age + job + marital + education + default + loan, data = bankSubset)
full10 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
anova(red10,full10)
# p value is low --> reject null hypthesis --> full model is significantly better --> include month into model
# Note: Although the F tests includes says the full10 model fits the data better, we might not that the month is seemingly irrelevant and exclude it from our model 

# Testing Duration 
red11 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
full11 <- lm(balance ~ age + job + marital + education + default + loan + month + duration, data = bankSubset)
anova(red11,full11)
# p value is not significant --> do not reject null hypthesis --> models do not differ significantly --> do not include duration into model 

# Testing campaign
red12 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
full12 <- lm(balance ~ age + job + marital + education + default + loan + month + campaign, data = bankSubset)
anova(red12,full12)
# p value is not significant --> do not reject null hypthesis --> models do not differ significantly --> do not include campaign into model 

# Testing Previous 
red13 <- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)
full13 <- lm(balance ~ age + job + marital + education + default + loan + month + previous, data = bankSubset)
anova(red13, full13)
# p value is not significant --> do not reject null hypthesis --> models do not differ significantly --> do not include previous into model 

# According to these tests the linear regression that fits the data the most is 
fit_balance_model<- lm(balance ~ age + job + marital + education + default + loan + month, data = bankSubset)

# However, I believe that month is insignificant in the actual interpretation of this data, therefore I would omit it from the 
# variables in the regression and use this model instead
# The final model includes the variables age, job, marital, education, default, and loan to predict balance
final_balance_model <- lm(balance ~ age + job + marital + education + default + loan , data = bankSubset)



