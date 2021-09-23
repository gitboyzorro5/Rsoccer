library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
unlink('myfixtures.csv')
#read the data
B1_schedule20212022 <- read.csv('../../../Leonard/Downloads/B1_schedule20212022.csv')
D1_schedule20212022 <- read.csv('../../../Leonard/Downloads/D1_schedule20212022.csv')
D2_schedule20212022 <- read.csv('../../../Leonard/Downloads/D2_schedule20212022.csv')
E0_schedule20212022 <- read.csv('../../../Leonard/Downloads/E0_schedule20212022.csv')
E1_schedule20212022 <- read.csv('../../../Leonard/Downloads/E1_schedule20212022.csv')
E2_schedule20212022 <- read.csv('../../../Leonard/Downloads/E2_schedule20212022.csv')
E3_schedule20212022 <- read.csv('../../../Leonard/Downloads/E3_schedule20212022.csv')
EC_schedule20212022 <- read.csv('../../../Leonard/Downloads/EC_schedule20212022.csv')
F1_schedule20212022 <- read.csv('../../../Leonard/Downloads/F1_schedule20212022.csv')
F2_schedule20212022 <- read.csv('../../../Leonard/Downloads/F2_schedule20212022.csv')
UCL_schedule20212022 <- read.csv('../../../Leonard/Downloads/UCL_schedule20212022.csv')
I1_schedule20212022 <- read.csv('../../../Leonard/Downloads/I1_schedule20212022.csv')
N1_schedule20212022 <- read.csv('../../../Leonard/Downloads/N1_schedule20212022.csv')
P1_schedule20212022 <- read.csv('../../../Leonard/Downloads/P1_schedule20212022.csv')
SC0_schedule20212022 <- read.csv('../../../Leonard/Downloads/SC0_schedule20212022.csv')
SC1_schedule20212022 <- read.csv('../../../Leonard/Downloads/SC1_schedule20212022.csv')
SP1_schedule20212022 <- read.csv('../../../Leonard/Downloads/SP1_schedule20212022.csv')
SP2_schedule20212022 <- read.csv('../../../Leonard/Downloads/SP2_schedule20212022.csv')
T1_schedule20212022 <- read.csv('../../../Leonard/Downloads/T1_schedule20212022.csv')
UEL_schedule20212022 <- read.csv('../../../Leonard/Downloads/UEL_schedule20212022.csv')
#parse the dates
B1_schedule20212022$Date <- mdy(B1_schedule20212022$Date)
D1_schedule20212022$Date <- dmy(D1_schedule20212022$Date)
D2_schedule20212022$Date <- mdy(D2_schedule20212022$Date)
E0_schedule20212022$Date <- dmy(E0_schedule20212022$Date)
E1_schedule20212022$Date <- dmy(E1_schedule20212022$Date)
E2_schedule20212022$Date <- mdy(E2_schedule20212022$Date)
E3_schedule20212022$Date <- mdy(E3_schedule20212022$Date)
EC_schedule20212022$Date <- mdy(EC_schedule20212022$Date)
F1_schedule20212022$Date <- dmy(F1_schedule20212022$Date)
F2_schedule20212022$Date <- mdy(F2_schedule20212022$Date)
UCL_schedule20212022$Date_ucl <- dmy(UCL_schedule20212022$Date_ucl)
I1_schedule20212022$Date <- dmy(I1_schedule20212022$Date)
N1_schedule20212022$Date <- mdy(N1_schedule20212022$Date)
P1_schedule20212022$Date <- mdy(P1_schedule20212022$Date)
SC0_schedule20212022$Date <- mdy(SC0_schedule20212022$Date)
SC1_schedule20212022$Date <- mdy(SC1_schedule20212022$Date)
SP1_schedule20212022$Date <- dmy(SP1_schedule20212022$Date)
SP2_schedule20212022$Date <- dmy(SP2_schedule20212022$Date)
T1_schedule20212022$Date <- dmy(T1_schedule20212022$Date)
UEL_schedule20212022$Date_uel <- dmy(UEL_schedule20212022$Date_uel)
#insert divisions
B1_schedule20212022$Div <- "B1"
D1_schedule20212022$Div <- "D1"
D2_schedule20212022$Div <- "D2"
E0_schedule20212022$Div <- "E0"
E1_schedule20212022$Div <- "E1"
E2_schedule20212022$Div <- "E2"
E3_schedule20212022$Div <- "E3"
EC_schedule20212022$Div <- "EC"
F1_schedule20212022$Div <- "F1"
F2_schedule20212022$Div <- "F2"
I1_schedule20212022$Div <- "I1"
N1_schedule20212022$Div <- "N1"
P1_schedule20212022$Div <- "P1"
SC0_schedule20212022$Div <- "SC0"
SC1_schedule20212022$Div <- "SC1"
SP1_schedule20212022$Div <- "SP1"
SP2_schedule20212022$Div <- "SP2"
T1_schedule20212022$Div <- "T1"
#bind the fixtures
all_schedule20212022 <- rbind(B1_schedule20212022,D1_schedule20212022,D2_schedule20212022,E0_schedule20212022,E1_schedule20212022,E2_schedule20212022,E3_schedule20212022,EC_schedule20212022,F1_schedule20212022,F2_schedule20212022,I1_schedule20212022,N1_schedule20212022,P1_schedule20212022,SC0_schedule20212022,SC1_schedule20212022,SP1_schedule20212022,T1_schedule20212022)

all_schedule20212022 <- all_schedule20212022[,c(7,5,6,3)]

myfixtures <- all_schedule20212022[all_schedule20212022$Date >= '2021-09-21' & all_schedule20212022$Date <= '2021-09-28',]

write.csv(myfixtures,'myfixtures.csv')
