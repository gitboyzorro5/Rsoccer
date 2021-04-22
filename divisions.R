#by gitboyzorro5
allteams20202021 <- read.csv('allteams20202021SOT.csv')
library('lubridate')
#change date strings to Date objects
allteams20202021$Date <- dmy(allteams20202021$Date)
allteams20202021 <- allteams20202021[order(as.Date(allteams20202021$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20202021$TG <- allteams20202021$FTHG + allteams20202021$FTAG
allteams20202021$OV15 <- ifelse(allteams20202021$TG >= 2,"Y","N")
allteams20202021$OV25 <- ifelse(allteams20202021$TG >= 3,"Y","N")
allteams20202021$OV35 <- ifelse(allteams20202021$TG >= 4,"Y","N")

#create divisions subsets
B1 <- subset(allteams20202021, Div == "B1")
D1 <- subset(allteams20202021, Div == "D1")
D2 <- subset(allteams20202021, Div == "D2")
E0 <- subset(allteams20202021, Div == "E0")
E1 <- subset(allteams20202021, Div == "E1")
E2 <- subset(allteams20202021, Div == "E2")
E3 <- subset(allteams20202021, Div == "E3")
EC <- subset(allteams20202021, Div == "EC")
F1 <- subset(allteams20202021, Div == "F1")
F2 <- subset(allteams20202021, Div == "F2")
G1 <- subset(allteams20202021, Div == "G1")
I1 <- subset(allteams20202021, Div == "I1")
I2 <- subset(allteams20202021, Div == "I2")
N1 <- subset(allteams20202021, Div == "N1")
P1 <- subset(allteams20202021, Div == "P1")
SC0 <- subset(allteams20202021, Div == "SC0")
SC1 <- subset(allteams20202021, Div == "SC1")
SC2 <- subset(allteams20202021, Div == "SC2")
SC3 <- subset(allteams20202021, Div == "SC3")
SP1 <- subset(allteams20202021, Div == "SP1")
SP2 <- subset(allteams20202021, Div == "SP2")
T1 <- subset(allteams20202021, Div == "T1")

attributes(allteams20202021)


latest_10 <- subset(allteams21, Date <= max(Date))
