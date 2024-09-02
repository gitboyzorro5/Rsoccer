#by gitboyzorro5
#create divisions and order by date
library('lubridate')
allteams20242025 <- read.csv('allteams20242025SOT.csv')

#change date strings to Date objects
allteams20242025$Date <- dmy(allteams20242025$Date)
allteams20242025 <- allteams20242025[order(as.Date(allteams20242025$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20242025$TG <- allteams20242025$FTHG + allteams20242025$FTAG
allteams20242025$TC <- allteams20242025$HCO + allteams20242025$ACO
allteams20242025$TF <- allteams20242025$HF + allteams20242025$AF
allteams20242025$COSC <- paste(allteams20242025$HCO,allteams20242025$ACO,sep = "-")
allteams20242025$OV15 <- ifelse(allteams20242025$TG >= 2,"Y","N")
allteams20242025$OV25 <- ifelse(allteams20242025$TG >= 3,"Y","N")
allteams20242025$OV35 <- ifelse(allteams20242025$TG >= 4,"Y","N")
allteams20242025$TY <- allteams20242025$HY + allteams20242025$AY
allteams20242025$TR <- allteams20242025$HR + allteams20242025$AR

#create divisions subsets
B1 <- subset(allteams20242025, Div == "B1")
D1 <- subset(allteams20242025, Div == "D1")
D2 <- subset(allteams20242025, Div == "D2")
E0 <- subset(allteams20242025, Div == "E0")
E1 <- subset(allteams20242025, Div == "E1")
E2 <- subset(allteams20242025, Div == "E2")
E3 <- subset(allteams20242025, Div == "E3")
EC <- subset(allteams20242025, Div == "EC")
F1 <- subset(allteams20242025, Div == "F1")
F2 <- subset(allteams20242025, Div == "F2")
G1 <- subset(allteams20242025, Div == "G1")
I1 <- subset(allteams20242025, Div == "I1")
I2 <- subset(allteams20242025, Div == "I2")
N1 <- subset(allteams20242025, Div == "N1")
P1 <- subset(allteams20242025, Div == "P1")
SC0 <- subset(allteams20242025, Div == "SC0")
SC1 <- subset(allteams20242025, Div == "SC1")
SC2 <- subset(allteams20242025, Div == "SC2")
SC3 <- subset(allteams20242025, Div == "SC3")
SP1 <- subset(allteams20242025, Div == "SP1")
SP2 <- subset(allteams20242025, Div == "SP2")
T1 <- subset(allteams20242025, Div == "T1")

View(allteams20242025)
