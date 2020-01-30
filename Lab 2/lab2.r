# Lab 1 


# Exersize 1

# Load data from github
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Remove any non-integer variables
amesnums <- Filter(is.numeric, ameslist)
library(dplyr)

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
GarageTemp = model.matrix( ~ ameslist$GarageType - 1)
ameslist <- cbind(ameslist, GarageTemp)


#2 

# Create linear regression model with all variables in Ames as predictor variables
sp_model <- lm(SalePrice ~ ., data = Ames)
summary(sp_model)




