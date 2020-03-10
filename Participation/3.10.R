library(rpart)
library(tidyverse)
library(caret)

# Load Ames data from github
ames <- read.table("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Lab%203/ames1.csv",
                   header = TRUE,
                   sep = ",")


# Fills NA values with previous value in column --> allows us to run a fullModel
ames_t <- as_tibble(ames)
ames_t <- ames_t %>% fill(names(.))

# Change fireplace to determine whether or not it has fireplace (0 is no, 1 is yes)
fireplaceChange = function(value){
  ifelse(value == 0, value <- 0, value <- 1)
}

# Switch fireplaces to only yes or no and factor variables
ames_t$Fireplaces <- sapply(ames_t$Fireplaces, fireplaceChange)
ames_t$Fireplaces <- as.factor(ames_t$Fireplaces)

# Creating test and training data
set.seed(96)
num_obs = nrow(ames_t)

train_index = sample(num_obs, size = trunc(.9 * num_obs))
train_data = ames_t[train_index, ]
test_data = ames_t[-train_index, ]

# Forward selection process 
nullModel <- lm(Fireplaces ~ 1, data = train_data)
fullModel <- lm(Fireplaces ~ ., data = train_data)

step(nullModel, direction = "forward", scope = formula(fullModel))

mylogit <- glm(Fireplaces ~ SalePrice + Neighborhood + GrLivArea + BldgType + Exterior1st + OverallQual, 
               data = train_data, family = "binomial")


predictfireplace <- predict(object = mylogit, data = test_data)
confusionMatrix(testpred, test_data)

