library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
unlink('myfixtures.csv')
#read the data
B1_schedule20222023 <- read.csv('../../Downloads/B1_schedule20222023.csv')
D1_schedule20222023 <- read.csv('../../Downloads/D1_schedule20222023.csv')
D2_schedule20222023 <- read.csv('../../Downloads/D2_schedule20222023.csv')
E0_schedule20222023 <- read.csv('../../Downloads/E0_schedule20222023.csv')
E1_schedule20222023 <- read.csv('../../Downloads/E1_schedule20222023.csv')
E2_schedule20222023 <- read.csv('../../Downloads/E2_schedule20222023.csv')
E3_schedule20222023 <- read.csv('../../Downloads/E3_schedule20222023.csv')
EC_schedule20222023 <- read.csv('../../Downloads/EC_schedule20222023.csv')
F1_schedule20222023 <- read.csv('../../Downloads/F1_schedule20222023.csv')
F2_schedule20222023 <- read.csv('../../Downloads/F2_schedule20222023.csv')
#UCL_schedule20222023 <- read.csv('../Downloads/UCL_schedule20222023.csv')
I1_schedule20222023 <- read.csv('../../Downloads/I1_schedule20222023.csv')
N1_schedule20222023 <- read.csv('../../Downloads/N1_schedule20222023.csv')
P1_schedule20222023 <- read.csv('../../Downloads/P1_schedule20222023.csv')
SC0_schedule20222023 <- read.csv('../../Downloads/SC0_schedule20222023.csv')
SC1_schedule20222023 <- read.csv('../../Downloads/SC1_schedule20222023.csv')
SP1_schedule20222023 <- read.csv('../../Downloads/SP1_schedule20222023.csv')
SP2_schedule20222023 <- read.csv('../../Downloads/SP2_schedule20222023.csv')
T1_schedule20222023 <- read.csv('../../Downloads/T1_schedule20222023.csv')
#UEL_schedule20222023 <- read.csv('../Downloads/UEL_schedule20222023.csv')
#parse the dates
B1_schedule20222023$Date <- mdy(B1_schedule20222023$Date)
D1_schedule20222023$Date <- mdy(D1_schedule20222023$Date)
D2_schedule20222023$Date <- mdy(D2_schedule20222023$Date)
E0_schedule20222023$Date <- mdy(E0_schedule20222023$Date)
E1_schedule20222023$Date <- mdy(E1_schedule20222023$Date)
E2_schedule20222023$Date <- mdy(E2_schedule20222023$Date)
E3_schedule20222023$Date <- mdy(E3_schedule20222023$Date)
EC_schedule20222023$Date <- mdy(EC_schedule20222023$Date)
F1_schedule20222023$Date <- mdy(F1_schedule20222023$Date)
F2_schedule20222023$Date <- mdy(F2_schedule20222023$Date)
#UCL_schedule20222023$Date_ucl <- mdy(UCL_schedule20222023$Date_ucl)
I1_schedule20222023$Date <- mdy(I1_schedule20222023$Date)
N1_schedule20222023$Date <- mdy(N1_schedule20222023$Date)
P1_schedule20222023$Date <- mdy(P1_schedule20222023$Date)
SC0_schedule20222023$Date <- mdy(SC0_schedule20222023$Date)
SC1_schedule20222023$Date <- mdy(SC1_schedule20222023$Date)
SP1_schedule20222023$Date <- mdy(SP1_schedule20222023$Date)
SP2_schedule20222023$Date <- mdy(SP2_schedule20222023$Date)
T1_schedule20222023$Date <- mdy(T1_schedule20222023$Date)
#UEL_schedule20222023$Date_uel <- dmy(UEL_schedule20222023$Date_uel)
#insert divisions
B1_schedule20222023$Div <- "B1"
D1_schedule20222023$Div <- "D1"
D2_schedule20222023$Div <- "D2"
E0_schedule20222023$Div <- "E0"
E1_schedule20222023$Div <- "E1"
E2_schedule20222023$Div <- "E2"
E3_schedule20222023$Div <- "E3"
EC_schedule20222023$Div <- "EC"
F1_schedule20222023$Div <- "F1"
F2_schedule20222023$Div <- "F2"
I1_schedule20222023$Div <- "I1"
N1_schedule20222023$Div <- "N1"
P1_schedule20222023$Div <- "P1"
SC0_schedule20222023$Div <- "SC0"
SC1_schedule20222023$Div <- "SC1"
SP1_schedule20222023$Div <- "SP1"
SP2_schedule20222023$Div <- "SP2"
T1_schedule20222023$Div <- "T1"
#bind the fixtures
all_schedule20222023 <- rbind(B1_schedule20222023,D1_schedule20222023,D2_schedule20222023,E0_schedule20222023,E1_schedule20222023,E2_schedule20222023,E3_schedule20222023,EC_schedule20222023,F1_schedule20222023,F2_schedule20222023,I1_schedule20222023,N1_schedule20222023,P1_schedule20222023,SC0_schedule20222023,SC1_schedule20222023,SP1_schedule20222023,SP2_schedule20222023,T1_schedule20222023)

all_schedule20222023 <- all_schedule20222023[,c(9,6,7,8)]

myfixtures <- all_schedule20222023[all_schedule20222023$Date >= '2022-07-28' & all_schedule20222023$Date <= '2022-08-01',]

write.csv(myfixtures,'myfixtures.csv')


