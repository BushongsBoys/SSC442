# Lab 1 - Group 17

# Load 
library(dplyr)
library(fastDummies)


# Exersize 1

# Load data from github
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Remove any non-integer variables
amesnums <- Filter(is.numeric, ameslist)

# Create a vector of the names of variables we do not have a reasonable intuition for
drops <- c("MSSubClass", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmntUnfSF", "LowQualFinSF", "X3SsnPorch", "MiscVal")

# Remove those variavles from the dataset
amesnums <-amesnums[ , !(names(amesnums) %in% drops)]

# Assign the variable Ames to list amesnum
Ames  <- amesnums

# Save file as an RData and csv file
save(Ames, file = "Ames.RData")
write.csv(Ames, 'Ames.csv')

range(Ames$GrLivArea, na.rm = TRUE)

# Create a vector of headers we are interested in comparing to Sale Price
subAmes <- select(Ames, "SalePrice", "YrSold", "GrLivArea", "TotalBsmtSF", "OverallCond","OverallQual","LotArea","YearBuilt","YearRemodAdd","BedroomAbvGr","TotRmsAbvGrd","GarageCars")

# Create a plot matrix with the 12 variables defined in subAmes
pairs(subAmes)

# Create a correlation matrix defined by 12 variables in subAmes
cor(subAmes)
#Most of the correlations matched my earlier belief, all the variables that imply a bigger house are strongly correlated with an increase in sale price,
#newer and higher quality houses also sold for more, I expected condition to be positively correlated with sale price, I am unsure why this has a negative correlation, 
#perhaps because condition is negatively correlated with basement size and YearBuilt. I expected Yrsold to be positively correlated with the saleprice
#but considering the timing of the dataset around the recession a negative correlation makes sense. 


# Creates a linear regression model comparing with sale price as the response and Above ground live areas as the predictor 
attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea)

# Changes scales from scientific notation to numerical 
options(scipen = 5)
plot(subAmes$GrLivArea, subAmes$SalePrice, main = "Sale Price vs Above Ground Living Space", ylab = "Sale Price", xlab = "Above Ground Living Space") + abline(lm.fit)

# Gives the risiduals of all the points - Use absolute 
resid <- abs(lm.fit$residual)

#This find the row that is the largest outlier in our model.
unlist(Ames[match(max(resid), resid),])

#########################################################

# Exersize 2

#1 

# Creates a column in Ames that determines if garage is connected or not
ameslist <- fastDummies::dummy_cols(ameslist, select_columns = c("GarageType"))
ameslist$GarageOutside <- ifelse(ameslist$GarageType_Detchd == 1 | ameslist$GarageType_CarPort == 1, 1, 0)

# Creates a regression with Sale Price as the response and Garage outside as the predictor
lmGarage.fit = lm(SalePrice ~ ameslist$GarageOutside)


#2 
# Create linear regression model with all variables in Ames as predictor variables
sp_model <- lm(SalePrice ~ ., data = Ames)
summary(sp_model)

# Creates diagnostic plots for regression
plot(sp_model)

#4
inter_model1 <- lm(SalePrice ~ OverallQual+ GrLivArea + OverallQual*GrLivArea, data=Ames)
summary(inter_model1)
#statistically significant interaction between quality and above ground living area
inter_model2 <- lm(SalePrice ~ GrLivArea + OverallQual + YearBuilt+ YearRemodAdd + YearBuilt*YearRemodAdd, data=Ames)
summary(inter_model2)
#no statistically significant interaction between yearbuilt and year remodeled
inter_model3 <- lm(SalePrice ~ GrLivArea + OverallQual + LotArea + OverallQual*LotArea, data=Ames)
summary(inter_model3)
#interestingly not a significant interaction between lot area and quality
inter_model4 <- lm(SalePrice ~ GrLivArea + OverallQual + LotArea + LotFrontage +  LotFrontage*LotArea, data=Ames)
summary(inter_model4)
#statistically significant but small interaction between lot area and lot frontage.
#it may make sense to include the 2 significant interactions in the final model

#5
transformer_model1 <- lm(log(SalePrice) ~ ., data = Ames)
summary(transformer_model1)
#using log of sale price allows us to see percent change effects of the variables

transformer_model2 <- lm(SalePrice ~ log(GrLivArea), data = Ames)
summary(transformer_model2)
#allows you to see the effects of a percent change in GrLivArea

transformer_model3 <- lm(SalePrice ~ GrLivArea + I(GrLivArea^2), data = Ames)
summary(transformer_model3)
#as above ground living area increases it becomes less significant

transformer_model4 <- lm(SalePrice ~ LotArea + I(LotArea^2), data = Ames)
summary(transformer_model4)
#Same for lot area

transformer_model5 <- lm(SalePrice ~ sqrt(LotArea), data = Ames)
summary(transformer_model5)
#Taking the sqrt of lot area increases the statistical significance but I am still not sure why you would want to do it. 