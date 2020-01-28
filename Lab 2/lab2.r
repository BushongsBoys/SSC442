ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
amesnums <- Filter(is.numeric, ameslist)
library(dplyr)
drops <- c("Id", "MSSubClass", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmntUnfSF", "LowQualFinSF", "X3SsnPorch", "MiscVal")
amesnums <-amesnums[ , !(names(amesnums) %in% drops)]
Ames  <- amesnums
save(Ames, file = "Ames.RData")
range(Ames$GrLivArea, na.rm = TRUE)
