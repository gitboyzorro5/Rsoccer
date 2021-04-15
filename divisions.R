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


latest_10 <- subset(allteams21, Date <= max(Date))
