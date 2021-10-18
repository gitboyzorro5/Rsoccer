library('lubridate')
library('sqldf')

allteams2010presentdf <- read.csv('../FDAS/allteams2010-present.csv')

allteams2010presentdf$Date <- dmy(allteams2010presentdf$Date)

allteams2010presentdf$TG <- allteams2010presentdf$FTHG + allteams2010presentdf$FTAG
# allteams2010presentdf$OV15 <- ifelse(allteams2010presentdf$TG >= 2,"Y","N")
# allteams2010presentdf$OV25 <- ifelse(allteams2010presentdf$TG >= 3,"Y","N")
# allteams2010presentdf$OV35 <- ifelse(allteams2010presentdf$TG >= 4,"Y","N")

allteams2010presentdf$matchkey <- paste(allteams2010presentdf$HomeTeam,allteams2010presentdf$AwayTeam,sep = "-")
require('RH2')

allteams2010presentdf$meetings <- "0"

allteams2010presentdf$meetings <- sqldf("select count(*) from allteams2010presentdf where HomeTeam = allteams2010presentdf.HomeTeam AND AwayTeam = allteams2010presentdf.AwayTeam group by matchkey")

allteams2010presentdf
