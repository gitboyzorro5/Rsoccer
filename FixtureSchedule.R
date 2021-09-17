library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
unlink('myfixtures.csv')
#read the data
D1_schedule20212022 <- read.csv('../../../Leonard/Downloads/D1_schedule20212022.csv')
E0_schedule20212022 <- read.csv('../../../Leonard/Downloads/E0_schedule20212022.csv')
E1_schedule20212022 <- read.csv('../../../Leonard/Downloads/E1_schedule20212022.csv')
F1_schedule20212022 <- read.csv('../../../Leonard/Downloads/F1_schedule20212022.csv')
UCL_schedule20212022 <- read.csv('../../../Leonard/Downloads/UCL_schedule20212022.csv')
I1_schedule20212022 <- read.csv('../../../Leonard/Downloads/I1_schedule20212022.csv')
SP1_schedule20212022 <- read.csv('../../../Leonard/Downloads/SP1_schedule20212022.csv')
T1_schedule20212022 <- read.csv('../../../Leonard/Downloads/T1_schedule20212022.csv')
UEL_schedule20212022 <- read.csv('../../../Leonard/Downloads/UEL_schedule20212022.csv')
#parse the dates
D1_schedule20212022$Date <- dmy(D1_schedule20212022$Date)
E0_schedule20212022$Date <- dmy(E0_schedule20212022$Date)
E1_schedule20212022$Date <- dmy(E1_schedule20212022$Date)
F1_schedule20212022$Date <- dmy(F1_schedule20212022$Date)
UCL_schedule20212022$Date_ucl <- dmy(UCL_schedule20212022$Date_ucl)
I1_schedule20212022$Date <- dmy(I1_schedule20212022$Date)
SP1_schedule20212022$Date <- dmy(SP1_schedule20212022$Date)
T1_schedule20212022$Date <- dmy(T1_schedule20212022$Date)
UEL_schedule20212022$Date_uel <- dmy(UEL_schedule20212022$Date_uel)
#insert divisions
D1_schedule20212022$Div <- "D1"
E0_schedule20212022$Div <- "E0"
E1_schedule20212022$Div <- "E1"
F1_schedule20212022$Div <- "F1"
I1_schedule20212022$Div <- "I1"
SP1_schedule20212022$Div <- "SP1"
T1_schedule20212022$Div <- "T1"
#bind the fixtures
all_schedule20212022 <- rbind(D1_schedule20212022,E0_schedule20212022,E1_schedule20212022,F1_schedule20212022,I1_schedule20212022,SP1_schedule20212022,T1_schedule20212022)

all_schedule20212022 <- all_schedule20212022[,c(7,5,6,3)]

myfixtures <- all_schedule20212022[all_schedule20212022$Date >= '2021-09-17' & all_schedule20212022$Date <= '2021-09-20',]

write.csv(myfixtures,'myfixtures.csv')



