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
E0_schedule20212022 <- read.csv('../../../Leonard/Downloads/E0_schedule20212022.csv')
E1_schedule20212022 <- read.csv('../../../Leonard/Downloads/E1_schedule20212022.csv')
E2_schedule20212022 <- read.csv('../../../Leonard/Downloads/E2_schedule20212022.csv')

#parse the dates
AUT_schedule20212022$Date <- mdy(AUT_schedule20212022$Date)
DNK_schedule20212022$Date <- mdy(DNK_schedule20212022$Date)
POL_schedule20212022$Date <- mdy(POL_schedule20212022$Date)
E0_schedule20212022$Date <- dmy(E0_schedule20212022$Date)
E1_schedule20212022$Date <- dmy(E1_schedule20212022$Date)
E2_schedule20212022$Date <- mdy(E2_schedule20212022$Date)

#insert divisions
AUT_schedule20212022$Div <- "Admiral Bundesliga"
DNK_schedule20212022$Div <- "superliga"
POL_schedule20212022$Div <- "POL"
E0_schedule20212022$Div <- "E0"
E1_schedule20212022$Div <- "E1"
E2_schedule20212022$Div <- "E2"

#bind the fixtures
all_schedule20212022newleagues <- rbind(AUT_schedule20212022,DNK_schedule20212022,POL_schedule20212022)

all_schedule20212022newleagues <- all_schedule20212022newleagues[,c(7,5,6,3)]

myfixturesnewleagues <- all_schedule20212022newleagues[all_schedule20212022newleagues$Date >= '2021-09-21' & all_schedule20212022newleagues$Date <= '2021-09-28',]

write.csv(myfixturesnewleagues,'myfixturesnewleagues.csv')

AUT_schedule20212022
