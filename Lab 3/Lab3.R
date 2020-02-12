# Lab 3 

# Load Libraries 
library(dplyr)
library(plyr)
library(tidyverse)
library(ggplot2)



# Function for calculating root means squared error of a model
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


# Function that assigns a number ot model complexity -> based on number of predictors in the model
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

# Fills NA values with previous value in column --> allows us to run a fullModel
ames_t <- as_tibble(ames)
ames_t <- ames_t %>% fill(names(.))

# Forward selection process 
nullModel <- lm(SalePrice ~ 1, data = ames_t)
fullModel <- lm(SalePrice ~ ., data = ames_t)

# Detects the variable to add to the null model that results in the lowest RSS 
#step(nullModel, direction = "forward", scope = formula(fullModel))

# Models based on step fucntion up to complexity 15 in a list format 
models <- list(model1 <- lm(SalePrice ~ Neighborhood, data = ames_t),
model2 <- lm(SalePrice ~ Neighborhood + GrLivArea, data = ames_t),
model3 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual, data = ames_t),
model4 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual + BsmtExposure, data = ames_t),
model5 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl, data = ames_t),
model6 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model7 <- lm(SalePrice ~ BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model8 <- lm(SalePrice ~ Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model9 <- lm(SalePrice ~ BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model10 <- lm(SalePrice ~ BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model11 <- lm(SalePrice ~ YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model12 <- lm(SalePrice ~ ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model13 <- lm(SalePrice ~ Functional + ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model14 <- lm(SalePrice ~ LotArea + Functional + ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t),
model15 <- lm(SalePrice ~ YearRemodAdd + LotArea + Functional + ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = ames_t))

# Creates a dataframe containg the complexity and RMSE of each model
modelCompData <- data.frame(
  "Complexity" = complexities <- sapply(models, get_complexity),
  "RMSE" = modrmse <- sapply(models, function(x){rmse(ames_t$SalePrice, predict(x, ames_t))}))

# Create plot comparing RMSE to Complexity
modelCompPlot <- ggplot(data = modelCompData, mapping = aes(x= Complexity, y = RMSE)) + 
  geom_line() + 
  labs(title = "RMSE vs Model Complexity") + 
  theme(plot.title = element_text(hjust = .5, size = 18)) 


##################################

# Excercize 2 

set.seed(96)
num_obs = nrow(ames_t)

train_index = sample(num_obs, size = trunc(.9 * num_obs))
train_data = ames_t[train_index, ]
test_data = ames_t[-train_index, ]

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}
#models2
models2 <- list(model1 <- lm(SalePrice ~ Neighborhood, data = train_data),
               model2 <- lm(SalePrice ~ Neighborhood + GrLivArea, data = train_data),
               model3 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual, data = train_data),
               model4 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual + BsmtExposure, data = train_data),
               model5 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl, data = train_data),
               model6 <- lm(SalePrice ~ Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model7 <- lm(SalePrice ~ BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model8 <- lm(SalePrice ~ Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model9 <- lm(SalePrice ~ BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model10 <- lm(SalePrice ~ BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model11 <- lm(SalePrice ~ YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model12 <- lm(SalePrice ~ ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model13 <- lm(SalePrice ~ Functional + ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model14 <- lm(SalePrice ~ LotArea + Functional + ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data),
               model15 <- lm(SalePrice ~ YearRemodAdd + LotArea + Functional + ExterQual + YearBuilt + BldgType + BsmtFinSF1 + Condition2 + BsmtQual + Neighborhood + GrLivArea + KitchenQual + BsmtExposure + RoofMatl + TotalBsmtSF, data = train_data))

train_rmse = sapply(models2, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(models2, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(models2, get_complexity)
plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")
min(test_rmse)


#excercize 2 
set.seed(9)
num_obs = nrow(ames_t)

train_index = sample(num_obs, size = trunc(.5 * num_obs))
train_data = ames_t[train_index, ]
test_data = ames_t[-train_index, ]
#right now best is 31718.8
drops <- c("Neighborhood","Condition2","RoofMatl", "Exterior1st", "Exterior2nd")
ames_dropped <- ames_t
ames_dropped <- ames_dropped[ , !(names(ames_dropped) %in% drops)]
nullModel <- lm(SalePrice ~ 1, data = ames_dropped)
fullModel <- lm(SalePrice ~ ., data = ames_dropped)

#step(nullModel, direction = "forward", scope = formula(fullModel))
#Used steps to pick best variables, neighborhood and rooftype seemed to overfit and removing variables after lot frontage to reduce overfitting
best_model <- lm(formula = SalePrice ~ GrLivArea+ GrLivArea*GrLivArea + ExterQual + BsmtQual + GarageCars + 
                   BsmtFinSF1 + KitchenQual + MSSubClass + BsmtExposure + YearBuilt + 
                   Fireplaces + Functional + Condition1 + LotShape + LandContour + 
                   KitchenAbvGr + YearRemodAdd + MasVnrArea + MSZoning + LotFrontage, 
   data = train_data)
get_rmse(best_model, train_data, 'SalePrice')
get_rmse(best_model, test_data, 'SalePrice')
summary(best_model)
