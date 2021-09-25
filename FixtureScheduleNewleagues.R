library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
unlink('myfixturesnewleagues.csv')
#read the data
AUT_schedule20212022 <- read.csv('../../../Leonard/Downloads/AUT_schedule20212022.csv')
DNK_schedule20212022 <- read.csv('../../../Leonard/Downloads/DNK_schedule20212022.csv')
POL_schedule20212022 <- read.csv('../../../Leonard/Downloads/POL_schedule20212022.csv')
ROU_schedule20212022 <- read.csv('../../../Leonard/Downloads/ROU_schedule20212022.csv')
RUS_schedule20212022 <- read.csv('../../../Leonard/Downloads/RUS_schedule20212022.csv')
SWZ_schedule20212022 <- read.csv('../../../Leonard/Downloads/SWZ_schedule20212022.csv')

#parse the dates
AUT_schedule20212022$Date <- mdy(AUT_schedule20212022$Date)
DNK_schedule20212022$Date <- mdy(DNK_schedule20212022$Date)
POL_schedule20212022$Date <- mdy(POL_schedule20212022$Date)
ROU_schedule20212022$Date <- mdy(ROU_schedule20212022$Date)
RUS_schedule20212022$Date <- mdy(RUS_schedule20212022$Date)
SWZ_schedule20212022$Date <- mdy(SWZ_schedule20212022$Date)

#insert divisions
AUT_schedule20212022$Div <- "Admiral Bundesliga"
DNK_schedule20212022$Div <- "superliga"
POL_schedule20212022$Div <- "Ekstraklasa"
ROU_schedule20212022$Div <- "Liga 1"
RUS_schedule20212022$Div <- "Premier League"
SWZ_schedule20212022$Div <- "Swiss"

#bind the fixtures
all_schedule20212022newleagues <- rbind(AUT_schedule20212022,DNK_schedule20212022,POL_schedule20212022,ROU_schedule20212022,RUS_schedule20212022,SWZ_schedule20212022)

all_schedule20212022newleagues <- all_schedule20212022newleagues[,c(7,5,6,4)]

myfixturesnewleagues <- all_schedule20212022newleagues[all_schedule20212022newleagues$Date >= '2021-09-21' & all_schedule20212022newleagues$Date <= '2021-09-28',]

write.csv(myfixturesnewleagues,'myfixturesnewleagues.csv')

