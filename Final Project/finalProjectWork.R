# Final Project 

# Load Libraries 
library(tidyverse)
library(ggplot2)
library(caret)
library(glmnet)
#https://www.hockey-reference.com/leagues/NHL_2017_standings.html#site_menu_link
#https://www.capfriendly.com/
######################################### Data Cleaning #################################################

# Load NHL Dataset 
WebsiteTrain <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/train.csv", 
                         na.strings = c(""," ", "NA" ))

WebsiteTest <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/test.csv",
                        na.strings = c(""," ", "NA" ))

Salary <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/test_salaries.csv")

# Combine salary column to rest of website test 
WebsiteTest2 <- cbind(Salary, WebsiteTest) 

# Combinde the website split test and train data 
hockeystats <- rbind(WebsiteTrain, WebsiteTest2)

# NA per column in hockey stats
sapply(hockeystats, function(x){sum(is.na(x))})
# Columns with large NA counts: Pr.St, DftYr, DftRd, Ovrl, isDist.1 

# Anyone out of born out of country would not have a providence/stat --> remove this column 
hockeystats <- hockeystats[, -which(names(hockeystats) %in% "Pr.St")]

# Draft Statistics NA values are result of undrafted players - Solutiuon - Remove draft year and make draft round an imaginary 8 and overall 218 
hockeystats <- hockeystats[, -which(names(hockeystats) %in% "DftYr")]
hockeystats$DftRd[is.na(hockeystats$DftRd)] <- 8
hockeystats$Ovrl[is.na(hockeystats$Ovrl)] <- 218

# Not a lot of use for a second average shot distance variable
hockeystats <- hockeystats[, -which(names(hockeystats) %in% "sDist.1")]

# Check the rows that have too many NA values and remove the ones that do 
apply(hockeystats,1, function(x){sum(is.na(x))})
many_na <- apply(hockeystats,1, function(x){sum(is.na(x)) > 6})
hockeystats <- hockeystats[-which(many_na),]

# Replace rest of NA values with the mean  
for(i in 1:ncol(hockeystats)){
  hockeystats[is.na(hockeystats[,i]), i] <- round(mean(hockeystats[,i], na.rm = TRUE))
}

############ End of Data Cleaning: Run All code above when doing visuals/modeling ####################

# Create a series of visualizations from our data

# Create a data frame with only the desired variables
HtSalary1 <- hockeystats %>% select(Ht, Position, Salary)

# Assign a new varaible identifying all the Offesive and Defensive position types
HtSalary1 <- HtSalary1 %>%
  mutate(Pos = ifelse(Position != "D", "Offensive Player",
                    ifelse(Position == "D", "Defensive Player", NA)))

# Group by Height and Position while summarising average salary in millions
HtSalary = HtSalary1 %>% 
  group_by(Ht, Pos) %>% 
  summarise(AvgSalaryInMillions = mean(Salary)/1000000)

# Create a bar plot with the Height and Average Salary variables
HtSalaryPlot <- ggplot(
  data=HtSalary, # data object 
  aes(
    x=Ht, # x aesthetic 
    y=AvgSalaryInMillions, # y aesthetic
  )
) + 
  labs(
    x='Player Height(Inches)',
    y='Average Salary (Millions)',
    title="Average Salary by Player Height"
  ) +
  theme_minimal()+
  theme(
    axis.ticks=element_blank(),
    legend.position = 'top') +
  facet_wrap(~Pos) +
  geom_bar(stat='identity', fill="steelblue")
HtSalaryPlot


# Create our own split of test vs train data 
set.seed(42)
hockeystats_idx = createDataPartition(hockeystats$Salary, p = 0.75, list = FALSE)
stat_trn = hockeystats[hockeystats_idx, ]
stat_tst = hockeystats[-hockeystats_idx, ]

# Functions for later RMSE testing 
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}


# Start exploring relationships between salary and some predictors we are interested in 
# Predictors to look at-
# Obvious: Position: Goals, Assists, Points 
# Non-obvious: Draft/Draft Overall, Games Played, +/-, Penalty minutes, Position

# Remove any non-numeric variables for data exploration purposes only
stat_trn_numeric <- Filter(is.numeric, stat_trn)

# Correlative plots of goals, assists, and points
cormat <- round(cor(stat_trn_numeric, use = "complete.obs"), 2)

# Create a ordinary linear regresssion using train function
# 5 Fold Cross Validation
set.seed(45)
cv_5 = trainControl(method = "cv", 5)

best_elastic_regression = train(
            form = Salary ~ ., 
            data = stat_trn,
            method = "glmnet", 
            trControl = cv_5 
)

# For an OLR model with penalized regression 
get_rmse(best_elastic_regression, stat_trn, 'Salary')
get_rmse(best_elastic_regression, stat_tst, 'Salary')

# Create an ordinary linear regression model with step function 





# Create a KNN model with train 

# Create a Decision Tree model using train 






