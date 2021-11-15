library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
################################
####delete current file#########
unlink('finalscore.xlsx')
unlink('finalscore_newleagues.xlsx')
allteams20212022scores <- allteams20212022
myoddscores <- readxl::read_excel('../FDAS/myodds_20212022.xlsx', sheet = '3way')

myoddscores$matchid <- paste(myoddscores$HT,myoddscores$AT, sep = "-")
allteams20212022scores$matchid <- paste(allteams20212022scores$HomeTeam,allteams20212022scores$AwayTeam, sep = "-")

#allteams20212022scores$Date <- ymd(allteams20212022scores$Date)
#myoddscores$Date <- ymd(myoddscores$Date)
#allteams20212022scores <- allteams20212022scores[allteams20212022scores$Date >= '2021-08-28',]
#myoddscores <- myoddscores[myoddscores$Date >= '2021-08-28',]
myoddscores <- myoddscores[,c(24,25,32,29)]
allteams20212022scores <- allteams20212022scores[,c(3,4,30,15,24)]

finalscore <- dplyr::left_join(myoddscores,allteams20212022scores)
write.xlsx(finalscore,'finalscore.xlsx')
rm(myoddscores)
rm(allteams20212022scores)
####################################################################
#########add correct scores########################################
AUT <- subset(AUT,Season == "2021/2022")
ARG <- subset(ARG,Season == "2021")
BRA <- subset(BRA,Season == "2021")
CHN <- subset(CHN,Season == "2021")
DNK <- subset(DNK,Season == "2021/2022")
FIN <- subset(FIN,Season == "2021")
IRL <- subset(IRL,Season == "2021")
JPN <- subset(JPN,Season == "2021")
MEX <- subset(MEX,Season == "2021/2022")
NOR <- subset(NOR,Season == "2021")
POL <- subset(POL,Season == "2021/2022")
ROU <- subset(ROU,Season == "2021/2022")
RUS <- subset(RUS,Season == "2021/2022")
SWE <- subset(SWE,Season == "2021")
MLS <- subset(MLS,Season == "2021")
SWZ <- subset(SWZ,Season == "2021/2022")

# AUT$CS <- paste(AUT$HG,AUT$AG, sep = "-")
# ARG$CS <- paste(ARG$HG,ARG$AG, sep = "-")
# BRA$CS <- paste(BRA$HG,BRA$AG, sep = "-")
# CHN$CS <- paste(CHN$HG,CHN$AG, sep = "-")
# DNK$CS <- paste(DNK$HG,DNK$AG, sep = "-")
# FIN$CS <- paste(FIN$HG,FIN$AG, sep = "-")
# IRL$CS <- paste(IRL$HG,IRL$AG, sep = "-")
# JPN$CS <- paste(JPN$HG,JPN$AG, sep = "-")
# MEX$CS <- paste(MEX$HG,MEX$AG, sep = "-")
# NOR$CS <- paste(NOR$HG,NOR$AG, sep = "-")
# POL$CS <- paste(POL$HG,POL$AG, sep = "-")
# ROU$CS <- paste(ROU$HG,ROU$AG, sep = "-")
# RUS$CS <- paste(RUS$HG,RUS$AG, sep = "-")
# SWE$CS <- paste(SWE$HG,SWE$AG, sep = "-")
# MLS$CS <- paste(MLS$HG,MLS$AG, sep = "-")
# SWZ$CS <- paste(SWZ$HG,SWZ$AG, sep = "-")

allteams20212022scores_newleagues <- rbind(AUT,ARG,BRA,CHN,DNK,FIN,IRL,JPN,MEX,NOR,POL,ROU,RUS,SWE,MLS,SWZ)
allteams20212022scores_newleagues$CS <- paste(allteams20212022scores_newleagues$HG,allteams20212022scores_newleagues$AG,sep = "-")
allteams20212022scores_newleagues$Date <- dmy(allteams20212022scores_newleagues$Date)

myoddscores_newleagues <- readxl::read_excel('../FDAS/myodds_20212022_newleagues.xlsx', sheet = '3way')
myoddscores_newleagues$Date <- dmy(myoddscores_newleagues$Date)

myoddscores_newleagues$matchid <- paste(myoddscores_newleagues$HT,myoddscores_newleagues$AT,myoddscores_newleagues$Date, sep = "-")
allteams20212022scores_newleagues$matchid <- paste(allteams20212022scores_newleagues$Home,allteams20212022scores_newleagues$Away,allteams20212022scores_newleagues$Date, sep = "-")

myoddscores_newleagues <- myoddscores_newleagues[,c(24,25,32,26,29)]
allteams20212022scores_newleagues <- allteams20212022scores_newleagues[,c(6,7,23,22,20)]

finalscore_newleagues <- dplyr::left_join(myoddscores_newleagues,allteams20212022scores_newleagues)

write.xlsx(finalscore_newleagues,'finalscore_newleagues.xlsx')

rm(myoddscores_newleagues)
rm(allteams20212022scores_newleagues)

