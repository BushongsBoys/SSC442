# Create a split between forward and defence 
defenceregression <- regressionStats[which(regressionStats$Position %in% "D"),]
defenceregression <- defenceregression[which(defenceregression$Salary>1000000),]

# Create Split of Defence Data
set.seed(42)
defence_idx = createDataPartition(defenceregression$Salary, p = 0.75, list = FALSE)
defence_trn = defenceregression[defence_idx, ]
defence_tst = defenceregression[-defence_idx, ]


# Create logistic transformation 
factordata <- regressionStats[,-which(names(regressionStats) %in% c("G", "A", "A1", "A2", "PTS", "PIM","iCF", "iCF.1", "iFF", "iSF", "iSF.1", "iSF.2", "ixG", "iSCF", "iRB", "iRS", "iRS", "iGVA", "iTKA", "iBLK", "iGVA.1", "iTKA.1", "OTG", "X1G", "GWG", "ENG", "PSG", "PSA", "G.Bkhd", "G.Dflct", "G.Slap", "G.Snap", "G.Tip", "G.Wrap", "G.Wrst","Over", "Wide", "S.Bkhd", "S.Dflct", "S.Slap", "S.Snap", "S.Tip", "S.Wrap", "Maj", "Match", "Misc", "Game", "CF", "DPS", "OPS", "PS"  ))]
numericdata <- regressionStats[,which(names(regressionStats) %in% c("G", "A", "A1", "A2", "PTS", "PIM","iCF", "iCF.1", "iFF", "iSF", "iSF.1", "iSF.2", "ixG", "iSCF", "iRB", "iRS", "iRS", "iGVA", "iTKA", "iBLK", "iGVA.1", "iTKA.1", "OTG", "X1G", "GWG", "ENG", "PSG", "PSA", "G.Bkhd", "G.Dflct", "G.Slap", "G.Snap", "G.Tip", "G.Wrap", "G.Wrst","Over", "Wide", "S.Bkhd", "S.Dflct", "S.Slap", "S.Snap", "S.Tip", "S.Wrap", "Maj", "Match", "Misc", "Game", "CF", "DPS", "OPS", "PS"  ))]
logdata <- sign(numericdata)*(abs(numericdata))^(1/3)
logdata <- cbind(factordata, logdata)
logdata <- as.data.frame(logdata)

salaryDist <- ggplot(hockeystats, aes(x= )) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(DPS)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

Goaldist <- ggplot(logdata, aes(x= G)) + 
  geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(G)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

set.seed(42)
defence_idx = createDataPartition(logdata$Salary, p = 0.75, list = FALSE)
stat_trn = logdata[defence_idx, ]
stat_tst = logdata[-defence_idx, ]

