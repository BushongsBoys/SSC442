library(rpart)
library(tidyverse)
library(caret)

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
nullModel <- lm(Fireplaces ~ 1, data = ames_t)
fullModel <- lm(Fireplaces ~ ., data = ames_t)

step(nullModel, direction = "forward", scope = formula(fullModel))



set.seed(96)
num_obs = nrow(ames_t)

train_index = sample(num_obs, size = trunc(.9 * num_obs))
train_data = ames_t[train_index, ]
test_data = ames_t[-train_index, ]


tree_slr = rpart(Fireplaces ~ SalePrice + GrLivArea + X1stFlrSF , data = train_data)

rpart.plot::rpart.plot(tree_slr)


testpred <- predict(object = tree_slr, data = test_data, method = "class")
isfireplace <- sapply(test_data, ifelse(Fireplaces == 1 | Fireplaces ==2, Fireplaces <- 1))
confusionMatrix(testpred, ames$Fireplaces)




