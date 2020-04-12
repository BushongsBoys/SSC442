# Final Project 

# Load Libraries 
library(tidyverse)

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

# Create our own split of test vs train data 
set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
hockeystats_idx = sample(nrow(hockeystats), round(nrow(hockeystats)*.75))
stat_trn = hockeystats[hockeystats_idx, ]
stat_tst = hockeystats[-hockeystats_idx,]

# Start exploring relationships between salary and some predictors we are interested in 
# Predictors to look at-
# Obvious: Position: Goals, Assists, Points 
# Non-obvious: Draft/Draft Overall, Games Played, +/-, Penalty minutes, Position

# Remove any non-numeric variables for data exploration purposes only
stat_trn_numeric <- Filter(is.numeric, stat_trn)

# Correlative plots of goals, assists, and points
cormat <- round(cor(stat_trn_numeric, use = "complete.obs"), 2)





