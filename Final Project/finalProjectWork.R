# Final Project 

# Load Libraries 
library(tidyverse)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)

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

# Remove rookies who are on entry level contracts(2014,2015,2016)
isRookie <- apply(hockeystats,1, function(x){hockeystats$DftYr > 2014})
hockeystats <- hockeystats[-which(isRookie),]

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


# Visual 1

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

# Visual 2

#Create a data frame with only the desired variables
AgeSalary <- hockeystats %>% select(Salary, Born)

#Add a Age variable calculated from the birth date variable
AgeSalary$Born <- as.Date(AgeSalary$Born)
AgeSalary$Age <- age_calc(AgeSalary$Born, enddate = Sys.Date(), units = "years", precise = TRUE)
AgeSalary$Age <- (AgeSalary$Age-1900)
AgeSalary$Age <- round(AgeSalary$Age, 0)
AgeSalary <- AgeSalary %>% filter(Age < 75)


#Group the data by age and summarise by average salary
AgeSalary = AgeSalary %>% 
  group_by(Age) %>% 
  summarise(AvgSalaryInMillions = mean(Salary)/1000000)


#Create a line plot with the age and average salary variables
AgeSalaryPlot <- ggplot(
  data=AgeSalary, # data object 
  aes(
    x=Age, # x aesthetic 
    y=AvgSalaryInMillions, # y aesthetic
  )
) + 
  labs(
    x='Player Age(Years)',
    y='Average Salary (Millions)',
    title="Average Salary by Player Age"
  ) +
  theme_minimal()+
  theme(
    axis.ticks=element_blank(),
    legend.position = 'top') +
  geom_line(stat='identity', color="steelblue")
AgeSalaryPlot

## Visual 3

CntrySalary <- hockeystats %>% select(Cntry, Salary)
CntrySalary <- CntrySalary %>%
  group_by(Cntry) %>%
  summarise(Salary = mean(Salary)/1000000)

CntrySalaryPlot<-ggplot(data=CntrySalary, aes(x=Cntry, y=Salary)) +
  geom_bar(stat="identity", color="steelblue", width = .8) +
  labs(
    x='Players Country',
    y='Average Salary (Millions)',
    title="Average Salary by Country") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks=element_blank())
CntrySalaryPlot

# Functions for later RMSE testing 
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


## Create a Ordinary Linear Regression Model 

# For Regression purposes we will do additional data cleaning and explainations 

# Remove Catagorical Data with large number of levels that have seemingly no impact on salary 
# Catagories Removed: Born, City, Last Name, First Name, Last Name, Team, Nat, Cntry
# Team might have an impact on how much the player gets paid, but we are interested in a market value hughes is worth, so it was also excluded
regressionStats <- hockeystats[, -which(names(hockeystats) %in% c("Born", "City", "Last.Name", "First.Name", "Team", "Nat", "Cntry"))]

# Change the positions assignments to Forward or Defence (F or D)
regressionStats$Position <- factor(regressionStats$Position, levels = c(levels(regressionStats$Position), "F"))
regressionStats$Position[
      which(regressionStats$Position %in% c("C", "C/D", "C/LW", "C/LW/RW", "C/RW", "C/RW/LW","LW/C", "LW", "LW/C/RW", "LW/RW", "LW/RW/C", "RW", "RW/C", "RW/C/LW", "RW/LW", "RW/LW/C", "C/LW/C"))] <- "F"

regressionStats$Position[
  which(regressionStats$Position %in% c("D", "D/LW","D/RW"))] <- "D"

str(droplevels(regressionStats))

# Create our own split of test vs train data 
set.seed(42)
hockeystats_idx = createDataPartition(regressionStats$Salary, p = 0.75, list = FALSE)
stat_trn = regressionStats[hockeystats_idx, ]
stat_tst = regressionStats[-hockeystats_idx, ]


# Create a random forrest to locate important vairables and make an plot for it
salaryForest <- randomForest(Salary ~ ., data = stat_trn, mtry = 143, importance = TRUE, ntree = 500 )
importance(salaryForest, type = 1)
varImpPlot(salaryForest, sort = TRUE, n.var = 20, type = 1)

# Use step function to find other important variables to include in lenear model 
# Forward selection process 
nullModel <- lm(Salary ~ 1, data = stat_trn)
fullModel <- lm(Salary ~ ., data = stat_trn)

# Detects the variable to add to the null model that results in the lowest RSS 
step(nullModel, direction = "forward", scope = formula(fullModel))

# Include variables from max step function 
Best_Model <- lm(formula = Salary ~ xGF + iHA + FOW + TKA + Wt + HF + FOL + 
                  iHDf + TOI.GP.1 + Position + NPD + Ht + ozFOW + FOL.Up + 
                  GVA + E... + TOI. + DSF + S.Slap + DPS + iTKA + iHF + Match + 
                  Game + S.Dflct + Shifts + HA + iSCF + iDS + iCF + G + iFF + 
                  TOI.GP + PTS + OPS + GP + GS + iBLK.1 + GS.G + Ovrl + PENT + A + A1 + A2 + Shifts +
                  TOI, data = stat_trn)

Test_Model <- lm(formula = Salary ~ xGF + iHA + FOW + TKA + Wt + HF + FOL + 
                   iHDf + TOI.GP.1 + Position + NPD + Ht + ozFOW + FOL.Up + 
                   GVA + E... + TOI. + DSF + S.Slap + DPS + iTKA + iHF + Match + 
                   Game + S.Dflct + Shifts + HA + iSCF + iDS + iCF + G + iFF + 
                   TOI.GP + PTS + OPS + GP + GS + iBLK.1 + GS.G + Ovrl + PENT + A + A1 + A2 + Shifts +
                   TOI + X... + PIM + IPP. + Diff + iRS  + iHF  + iBLK + ozFOL + GWG +  G.Tip +
                   Over + S.Tip  + Min + Misc+  FF + SF+ GF + GA + RBA +
                    OTOI  + GS.G, data = stat_trn)

predict1 <- predict(Best_Model, stat_trn)
predict2 <- predict(Best_Model, stat_tst)
predict3 <- predict(Test_Model, stat_tst)
rmse(stat_trn$Salary, predict1)
rmse(stat_tst$Salary, predict2)
rmse(stat_tst$Salary, predict3)

set.seed(45)
cv_5 = trainControl(method = "cv", 5)


best_elastic_regression = train(
    form = Salary ~ ., 
    data = stat_trn,
    method = "glmnet", 
    trControl = cv_5, 
    tuneLength = 10
  )
  
predict5 <- predict(best_elastic_regression, stat_trn)
predict6 <- predict(best_elastic_regression, stat_tst)
rmse(stat_trn$Salary, predict5)
rmse(stat_tst$Salary, predict6)



