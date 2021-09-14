library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')

D1_schedule20212022 <- read.csv('../../../Leonard/Downloads/D1_schedule20212022.csv')
E0_schedule20212022 <- read.csv('../../../Leonard/Downloads/E0_schedule20212022.csv')
E1_schedule20212022 <- read.csv('../../../Leonard/Downloads/E1_schedule20212022.csv')
F1_schedule20212022 <- read.csv('../../../Leonard/Downloads/F1_schedule20212022.csv')
UCL_schedule20212022 <- read.csv('../../../Leonard/Downloads/UCL_schedule20212022.csv')

D1_schedule20212022$Date_d1 <- dmy(D1_schedule20212022$Date_d1)
E0_schedule20212022$Date_e0 <- dmy(E0_schedule20212022$Date_e0)
E1_schedule20212022$Date_e1 <- dmy(E1_schedule20212022$Date_e1)
F1_schedule20212022$Date_f1 <- dmy(F1_schedule20212022$Date_f1)
UCL_schedule20212022$Date_ucl <- dmy(UCL_schedule20212022$Date_ucl)

D1_schedule20212022[D1_schedule20212022$RoundNumber_d1 == "5",]
E0_schedule20212022[E0_schedule20212022$RoundNumber_e0 == "5",]
E1_schedule20212022[E1_schedule20212022$RoundNumber_e1 == "7",]
F1_schedule20212022[F1_schedule20212022$RoundNumber_f1 == "6",]
UCL_schedule20212022[UCL_schedule20212022$RoundNumber_ucl == "1",]

