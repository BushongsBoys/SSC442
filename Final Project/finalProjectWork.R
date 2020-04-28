# Final Project 

# Load Libraries 
library(tidyverse)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)
library(xgboost)

#https://www.hockey-reference.com/leagues/NHL_2017_standings.html#site_menu_link
#https://www.capfriendly.com/

######################################### Data Cleaning #################################################

# Load NHL Dataset 
WebsiteTrain <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/train.csv", 
                         na.strings = c(""," ", "NA" ))

WebsiteTest <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/test.csv",
                        na.strings = c(""," ", "NA" ))

Salary <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/test_salaries.csv")


Records <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/records.csv")


quinnHughes <- read.csv("https://raw.githubusercontent.com/BushongsBoys/SSC442/master/Final%20Project/Data/quinnHughes.csv")

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

# Remove injured players who were injured for a large portion of the season(taking into account salary)
hockeystats <- hockeystats[-which(hockeystats$Last.Name %in% c("Stamkos", "Myers", "Huberdeau","MacArthur", "Despres", "Stoner", "Dorsett", "Richardson", "Hemsky", "Callahan" )),]

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

# Distribution Exploration 
salaryDist <- ggplot(hockeystats, aes(x= Salary)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Salary)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

Goaldist <- ggplot(hockeystats, aes(x= G)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(G)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

newstat <- hockeystats[-which(hockeystats$Salary < 1000000),]
salaryDist <- ggplot(hockeystats, aes(x= Salary)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Salary)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")


TOIDist <- ggplot(hockeystats, aes(x= TOIX)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(TOIX)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

Shiftdist <- ggplot(hockeystats, aes(x= Shifts)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Shifts)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

ShiftOvrl <- ggplot(hockeystats, aes(x= Ovrl)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Ovrl)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

DftRdOvrl <- ggplot(hockeystats, aes(x= DftRd)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(DftRd)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")


# From this we can conclude that most of our numeric 


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

# Remove all Faceoff Data since it only applies to centers 
regressionStats <- regressionStats[, -which(names(regressionStats) %in% c("iFOW", "CBar", "Post", "iFOL", "iFOW.1", "iFOL.1", "FO.", "X.FOT", "dzFOW", "dzFOL", "nzFOW", "nzFOL", "ozFOW", "ozFOL",  "FOW.Up", "FOL.Up", "FOW.Down", "FOL.Down", "FOW.Close", "FOL.Close"))]

# Remove all variables where there are two of them sDist and sDist.1 for example 
regressionStats <- regressionStats[, -which(names(regressionStats) %in% c("FOW", "FOL", "SH.", "SV." ,"TOI.GP.1", "iCF.1", "iSF.1", "iSF.2", "iHF.1", "iGVA.1", "iTKA.1", "iBLK.1"))]

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
salaryForest <- randomForest(Salary ~ ., data = stat_trn, mtry = 111, importance = TRUE, ntree = 500 )
importance(salaryForest, type = 1)
varImpPlot(salaryForest, sort = TRUE, n.var = 20, type = 1)
predictrf <- predict(salaryForest, stat_tst)
rmse(stat_tst$Salary, predictrf)

# Use step function to find other important variables to include in lenear model 
# Forward selection process 
nullModel <- lm(Salary ~ 1, data = stat_trn)
fullModel <- lm(Salary ~ ., data = stat_trn)

# Detects the variable to add to the null model that results in the lowest RSS 
step(nullModel, direction = "forward", scope = formula(fullModel))


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

plot(Records$CAP, Records$W, pch = 16, cex = 1.3, col = "blue", main = "Salary cap versus wins", xlab = "Salary Cap", ylab = "Wins")
abline(lm(Records$W~Records$CAP), col="red") # regression line (y~x)

count = c()
for(team in 1:nrow(Records)){
  count <- c(count, (sum(hockeystats$Salary > 6000000 & Records[team,"ABREV"] == hockeystats$Team)))
}
counts = round(count, 0)
Records$Stars = counts
plot(Records$Stars, Records$W, pch = 16, cex = 1.3, col = "blue", main = "Salaray cap versus wins", xlab = "Salary Cap", ylab = "Wins")
model1 = lm(Records$W~Records$CAP)
model2 = lm(W~CAP + Stars + , Records)
summary(model1)
summary(model2)







# Calculate residuals on test data and create a dataframe with the two
residuals <- stat_tst$Salary - predict6
plotSet <-  cbind(residuals, stat_tst)

residualsOnSalary <- ggplot(data = plotSet, aes(x=Salary, y= residuals, color= Position))+
  geom_point()


# Using gradient boosted trees 
set.seed(123)
model <- train(
  Salary ~ Position + Ovrl + iBLK  + TOI + GP + xGF + Shifts + S.Wrst + S.Slap + CF + ixG + iTKA+ A
  + E... + iCF + G + PTS + PS + PIM + GWG + G, 
  data = stat_trn, 
  method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

predict5 <- predict(model, stat_trn)
predict6 <- predict(model, stat_tst)
rmse(stat_trn$Salary, predict5)
rmse(stat_tst$Salary, predict6)


# Calculate residuals on test data and create a dataframe with the two
residuals <- stat_tst$Salary - predict6
plotSet <-  cbind(residuals, stat_tst)

residualsOnSalary <- ggplot(data = plotSet, aes(x=Salary, y= residuals, color= Position))+
  geom_point()


# KNN observations 
set.seed(123)
model2 <- train(
  Salary ~ Position + Ovrl + iBLK  + TOI + GP + xGF + Shifts + S.Wrst + S.Slap + CF + ixG + iTKA+ A
  + E... + iCF + G + PTS + PS + PIM + GWG + G, 
  data = stat_trn, 
  method = "knn",
  trControl = trainControl("cv", number = 10), 
  tuneLength = 20
)

predict5 <- predict(model2, stat_trn)
predict6 <- predict(model2, stat_tst)
rmse(stat_trn$Salary, predict5)
rmse(stat_tst$Salary, predict6)


# Calculate residuals on test data and create a dataframe with the two
residuals <- stat_tst$Salary - predict6
plotSet <-  cbind(residuals, stat_tst)

residualsOnSalary <- ggplot(data = plotSet, aes(x=Salary, y= residuals, color= Position))+
  geom_point()


