# Lab 1 


# Exersize 1

# Load data from github
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Remove any non-integer variables
amesnums <- Filter(is.numeric, ameslist)
library(dplyr)
library(fastDummies)

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

subAmes <- select(Ames, "SalePrice", "YrSold", "GrLivArea", "TotalBsmtSF", "OverallCond","OverallQual","LotArea","YearBuilt","YearRemodAdd","BedroomAbvGr","TotRmsAbvGrd","GarageCars")
pairs(subAmes)
cor(subAmes)

#Most of the correlations matched my earlier belief, all the variables that imply a bigger house are strongly correlated with an increase in sale price,
#newer and higher quality houses also sold for more, I expected condition to be positively correlated with sale price, I am unsure why this has a negative correlation, 
#perhaps because condition is negatively correlated with basement size and YearBuilt. I expected Yrsold to be positively correlated with the saleprice
#but considering the timing of the dataset around the recession a negative correlation makes sense. 


attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea)
options(scipen = 5)
plot(subAmes$GrLivArea, subAmes$SalePrice, main = "Sale Price vs Above Ground Living Space", ylab = "Sale Price", xlab = "Above Ground Living Space") + abline(lm.fit)
resid <- lm.fit$residual
unlist(Ames[match(max(resid), resid),])

#This find the row that is the largest outlier in our model.

# Exersize 2

# From notes 


#1 
ameslist <- fastDummies::dummy_cols(ameslist, select_columns = c("GarageType"))
ameslist$GarageOutside <- ifelse(ameslist$GarageType_Detchd == 1 | ameslist$GarageType_CarPort == 1, 1, 0)

lmGarage.fit = lm(SalePrice ~ ameslist$GarageOutside)
#as you can see having an outdoor garage is associated with a drop in the 
#sale price of 72859

#2 
# Create linear regression model with all variables in Ames as predictor variables
sp_model <- lm(SalePrice ~ ., data = Ames)
summary(sp_model)

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

transformer_model3 <- lm(SalePrice ~ GrLivArea + LotArea + LotArea*LotArea, data = Ames)
summary(transformer_model2)