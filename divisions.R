#by gitboyzorro5
#create divisions and order by date
library('lubridate')
allteams20222023 <- read.csv('allteams20222023SOT.csv')

#change date strings to Date objects
allteams20222023$Date <- dmy(allteams20222023$Date)
allteams20222023 <- allteams20222023[order(as.Date(allteams20222023$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20222023$TG <- allteams20222023$FTHG + allteams20222023$FTAG
allteams20222023$TC <- allteams20222023$HCO + allteams20222023$ACO
allteams20222023$COSC <- paste(allteams20222023$HCO,allteams20222023$ACO,sep = "-")
allteams20222023$OV15 <- ifelse(allteams20222023$TG >= 2,"Y","N")
allteams20222023$OV25 <- ifelse(allteams20222023$TG >= 3,"Y","N")
allteams20222023$OV35 <- ifelse(allteams20222023$TG >= 4,"Y","N")
allteams20222023$TY <- allteams20222023$HY + allteams20222023$AY
allteams20222023$TR <- allteams20222023$HR + allteams20222023$AR

#create divisions subsets
B1 <- subset(allteams20222023, Div == "B1")
D1 <- subset(allteams20222023, Div == "D1")
D2 <- subset(allteams20222023, Div == "D2")
E0 <- subset(allteams20222023, Div == "E0")
E1 <- subset(allteams20222023, Div == "E1")
E2 <- subset(allteams20222023, Div == "E2")
E3 <- subset(allteams20222023, Div == "E3")
EC <- subset(allteams20222023, Div == "EC")
F1 <- subset(allteams20222023, Div == "F1")
F2 <- subset(allteams20222023, Div == "F2")
G1 <- subset(allteams20222023, Div == "G1")
I1 <- subset(allteams20222023, Div == "I1")
I2 <- subset(allteams20222023, Div == "I2")
N1 <- subset(allteams20222023, Div == "N1")
P1 <- subset(allteams20222023, Div == "P1")
SC0 <- subset(allteams20222023, Div == "SC0")
SC1 <- subset(allteams20222023, Div == "SC1")
SC2 <- subset(allteams20222023, Div == "SC2")
SC3 <- subset(allteams20222023, Div == "SC3")
SP1 <- subset(allteams20222023, Div == "SP1")
SP2 <- subset(allteams20222023, Div == "SP2")
T1 <- subset(allteams20222023, Div == "T1")

