# Lab 3 

# Load Libraries 
library(dplyr)
library(plyr)


# Function for calculating root means squared error of a model
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


# Function that assigns a number ot model complexity --> based on number of predictors in the model
get_complexity = function(model) {
  length(coef(model)) - 1
}

# Exersize 1

# Load Ames data from github
ames <- read.table("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Lab%203/ames1.csv",
                   header = TRUE,
                   sep = ",")

# Remove columns and one factor variables
ames <- ames[, -(which(names(ames) %in% c("OverallCond", "OverallQual")))]


# Forward selection processes -- example from online source 
nullModel <- lm(mpg ~ 1,data = mtcars)
fullModel  <- lm(mpg ~ .,data = mtcars)

step(nullModel, direction = "forward", scope = formula(fullModel))
