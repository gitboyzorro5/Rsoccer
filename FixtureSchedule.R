library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')
unlink('myfixtures.csv')
#read the data
B1_schedule20252026 <- read.csv('../../Downloads/B1_schedule20252026.csv')
B1_schedule20252026 <- B1_schedule20252026[,c(-1)]
D1_schedule20252026 <- read.csv('../../Downloads/D1_schedule20252026.csv')
D1_schedule20252026 <- D1_schedule20252026[,c(-1)]
D2_schedule20252026 <- read.csv('../../Downloads/D2_schedule20252026.csv')
D2_schedule20252026 <- D2_schedule20252026[,c(-1)]
E0_schedule20252026 <- read.csv('../../Downloads/E0_schedule20252026.csv')
E0_schedule20252026 <- E0_schedule20252026[,c(-1)]
E1_schedule20252026 <- read.csv('../../Downloads/E1_schedule20252026.csv')
E1_schedule20252026 <- E1_schedule20252026[,c(-1)]
E2_schedule20252026 <- read.csv('../../Downloads/E2_schedule20252026.csv')
E2_schedule20252026 <- E2_schedule20252026[,c(-1)]
E3_schedule20252026 <- read.csv('../../Downloads/E3_schedule20252026.csv')
E3_schedule20252026 <- E3_schedule20252026[,c(-1)]
EC_schedule20252026 <- read.csv('../../Downloads/EC_schedule20252026.csv')
EC_schedule20252026 <- EC_schedule20252026[,c(-1)]
F1_schedule20252026 <- read.csv('../../Downloads/F1_schedule20252026.csv')
F1_schedule20252026 <- F1_schedule20252026[,c(-1)]
F2_schedule20252026 <- read.csv('../../Downloads/F2_schedule20252026.csv')
F2_schedule20252026 <- F2_schedule20252026[,c(-1)]
#G1_schedule20252026 <- read.csv('../../Downloads/G1_schedule20252026.csv')
#G1_schedule20252026 <- G1_schedule20252026[,c(-1)]
#UCL_schedule20252026 <- read.csv('Downloads/UCL_schedule20252026.csv')
I1_schedule20252026 <- read.csv('../../Downloads/I1_schedule20252026.csv')
I1_schedule20252026 <- I1_schedule20252026[,c(-1)]
I2_schedule20252026 <- read.csv('../../Downloads/I2_schedule20252026.csv')
I2_schedule20252026 <- I2_schedule20252026[,c(-1)]
N1_schedule20252026 <- read.csv('../../Downloads/N1_schedule20252026.csv')
N1_schedule20252026 <- N1_schedule20252026[,c(-1)]
P1_schedule20252026 <- read.csv('../../Downloads/P1_schedule20252026.csv')
P1_schedule20252026 <- P1_schedule20252026[,c(-1)]
SP1_schedule20252026 <- read.csv('../../Downloads/SP1_schedule20252026.csv')
SP1_schedule20252026 <- SP1_schedule20252026[,c(-1)]
SC0_schedule20252026 <- read.csv('../../Downloads/SC0_schedule20252026.csv')
SC0_schedule20252026 <- SC0_schedule20252026[,c(-1)]
SC1_schedule20252026 <- read.csv('../../Downloads/SC1_schedule20252026.csv')
SC1_schedule20252026 <- SC1_schedule20252026[,c(-1)]
# SC2_schedule20252026 <- read.csv('../Downloads/SC2_schedule20252026.csv')
# SC2_schedule20252026 <- SC2_schedule20252026[,c(-1)]
# SC3_schedule20252026 <- read.csv('../Downloads/SC3_schedule20252026.csv')
# SC3_schedule20252026 <- SC3_schedule20252026[,c(-1)]
SP2_schedule20252026 <- read.csv('../../Downloads/SP2_schedule20252026.csv')
SP2_schedule20252026 <- SP2_schedule20252026[,c(-1)]
T1_schedule20252026 <- read.csv('../../Downloads/T1_schedule20252026.csv')
T1_schedule20252026 <- T1_schedule20252026[,c(-1)]
#UEL_schedule20252026 <- read.csv('../Downloads/UEL_schedule20252026.csv')
#parse the dates
B1_schedule20252026$Date <- ymd(B1_schedule20252026$Date)
D1_schedule20252026$Date <- ymd(D1_schedule20252026$Date)
D2_schedule20252026$Date <- ymd(D2_schedule20252026$Date)
E0_schedule20252026$Date <- ymd(E0_schedule20252026$Date)
E1_schedule20252026$Date <- ymd(E1_schedule20252026$Date)
E2_schedule20252026$Date <- ymd(E2_schedule20252026$Date)
E3_schedule20252026$Date <- ymd(E3_schedule20252026$Date)
EC_schedule20252026$Date <- ymd(EC_schedule20252026$Date)
F1_schedule20252026$Date <- ymd(F1_schedule20252026$Date)
F2_schedule20252026$Date <- ymd(F2_schedule20252026$Date)
#G1_schedule20252026$Date <- ymd(G1_schedule20252026$Date)
#UCL_schedule20252026$Date_ucl <- mdy(UCL_schedule20252026$Date_ucl)
I1_schedule20252026$Date <- ymd(I1_schedule20252026$Date)
I2_schedule20252026$Date <- ymd(I2_schedule20252026$Date)
N1_schedule20252026$Date <- ymd(N1_schedule20252026$Date)
P1_schedule20252026$Date <- ymd(P1_schedule20252026$Date)
SC0_schedule20252026$Date <- ymd(SC0_schedule20252026$Date)
SC1_schedule20252026$Date <- ymd(SC1_schedule20252026$Date)
# SC2_schedule20252026$Date <- dmy(SC2_schedule20252026$Date)
# SC3_schedule20252026$Date <- dmy(SC3_schedule20252026$Date)
SP1_schedule20252026$Date <- ymd(SP1_schedule20252026$Date)
SP2_schedule20252026$Date <- ymd(SP2_schedule20252026$Date)
T1_schedule20252026$Date <- ymd(T1_schedule20252026$Date)
#UEL_schedule20252026$Date_uel <- dmy(UEL_schedule20252026$Date_uel)
#insert divisions
# B1_schedule20252026$Div <- "B1"
# D1_schedule20252026$Div <- "D1"
# D2_schedule20252026$Div <- "D2"
# #E0_schedule20252026$Div <- "E0"
# E1_schedule20252026$Div <- "E1"
# E2_schedule20252026$Div <- "E2"
# E3_schedule20252026$Div <- "E3"
# EC_schedule20252026$Div <- "EC"
# F1_schedule20252026$Div <- "F1"
# F2_schedule20252026$Div <- "F2"
# G1_schedule20252026$Div <- "G1"
# I1_schedule20252026$Div <- "I1"
# I2_schedule20252026$Div <- "I2"
# N1_schedule20252026$Div <- "N1"
# P1_schedule20252026$Div <- "P1"
# SC0_schedule20252026$Div <- "SC0"
# SC1_schedule20252026$Div <- "SC1"
# SC2_schedule20252026$Div <- "SC2"
# SC3_schedule20252026$Div <- "SC3"
# SP1_schedule20252026$Div <- "SP1"
# SP2_schedule20252026$Div <- "SP2"
#T1_schedule20252026$Div <- "T1"
#bind the fixtures

#all_schedule20252026 <- rbind(B1_schedule20252026,D2_schedule20252026,D1_schedule20252026,E0_schedule20252026,E1_schedule20252026,E2_schedule20252026,E3_schedule20252026,EC_schedule20252026,F1_schedule20252026,F2_schedule20252026,G1_schedule20252026,I1_schedule20252026,I2_schedule20252026,N1_schedule20252026,P1_schedule20252026,SC0_schedule20252026,SC1_schedule20252026,SC2_schedule20252026,SC3_schedule20252026,SP1_schedule20252026,SP2_schedule20252026,T1_schedule20252026)

all_schedule20252026 <- rbind(B1_schedule20252026,E0_schedule20252026,D1_schedule20252026,D2_schedule20252026,E1_schedule20252026,E2_schedule20252026,E3_schedule20252026,EC_schedule20252026,F1_schedule20252026,F2_schedule20252026,I1_schedule20252026,I2_schedule20252026,N1_schedule20252026,SP1_schedule20252026,SC0_schedule20252026,SC1_schedule20252026,SP2_schedule20252026,T1_schedule20252026,P1_schedule20252026)

myfixtures <- all_schedule20252026[all_schedule20252026$Date >= '2026-04-17' & all_schedule20252026$Date <= '2026-04-20',]

write.csv(myfixtures,'myfixtures.csv')
View(myfixtures)

