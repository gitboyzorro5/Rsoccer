#by gitboyzorro5
#create divisions and order by date
library('lubridate')
allteams20212022 <- read.csv('allteams20212022SOT.csv')

#change date strings to Date objects
allteams20212022$Date <- dmy(allteams20212022$Date)
allteams20212022 <- allteams20212022[order(as.Date(allteams20212022$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20212022$TG <- allteams20212022$FTHG + allteams20212022$FTAG
allteams20212022$OV15 <- ifelse(allteams20212022$TG >= 2,"Y","N")
allteams20212022$OV25 <- ifelse(allteams20212022$TG >= 3,"Y","N")
allteams20212022$OV35 <- ifelse(allteams20212022$TG >= 4,"Y","N")
allteams20212022$TY <- allteams20212022$HY + allteams20212022$AY
allteams20212022$TR <- allteams20212022$HR + allteams20212022$AR

#create divisions subsets
B1 <- subset(allteams20212022, Div == "B1")
D1 <- subset(allteams20212022, Div == "D1")
D2 <- subset(allteams20212022, Div == "D2")
E0 <- subset(allteams20212022, Div == "E0")
E1 <- subset(allteams20212022, Div == "E1")
E2 <- subset(allteams20212022, Div == "E2")
E3 <- subset(allteams20212022, Div == "E3")
EC <- subset(allteams20212022, Div == "EC")
F1 <- subset(allteams20212022, Div == "F1")
F2 <- subset(allteams20212022, Div == "F2")
G1 <- subset(allteams20212022, Div == "G1")
I1 <- subset(allteams20212022, Div == "I1")
I2 <- subset(allteams20212022, Div == "I2")
N1 <- subset(allteams20212022, Div == "N1")
P1 <- subset(allteams20212022, Div == "P1")
SC0 <- subset(allteams20212022, Div == "SC0")
SC1 <- subset(allteams20212022, Div == "SC1")
SC2 <- subset(allteams20212022, Div == "SC2")
SC3 <- subset(allteams20212022, Div == "SC3")
SP1 <- subset(allteams20212022, Div == "SP1")
SP2 <- subset(allteams20212022, Div == "SP2")
T1 <- subset(allteams20212022, Div == "T1")

