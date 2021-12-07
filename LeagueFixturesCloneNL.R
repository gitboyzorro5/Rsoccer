library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
options(java.parameters = "-Xmx2048m")
library('xlsx')
library('scales')
library('lubridate')
#########################################################################################################################################
unlink('clonedprediction_nl.xlsx')
#########################################################################################################################################
AUT_fixtures_clone <- AUT_fixtures
colnames(AUT_fixtures_clone)[61] <- "Hwin"
colnames(AUT_fixtures_clone)[62] <- "Draw"
colnames(AUT_fixtures_clone)[63] <- "Awin"

AUT_fixtures_clone$Hwinodds <-   AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
  AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
  AUT_fixtures$aut_5_0 + AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
  AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
  AUT_fixtures$aut_6_5
AUT_fixtures_clone$Hwinodds <- round(1/AUT_fixtures_clone$Hwinodds, digits = 3)

AUT_fixtures_clone$Drawodds <-  AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 + AUT_fixtures$aut_4_4 +
  AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6

AUT_fixtures_clone$Drawodds <- round(1/AUT_fixtures_clone$Drawodds, digits = 3)

AUT_fixtures_clone$Awinodds <-   AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
  AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
  AUT_fixtures$aut_0_5 + AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
  AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
  AUT_fixtures$aut_5_6

AUT_fixtures_clone$Awinodds <- round(1/AUT_fixtures_clone$Awinodds, digits = 3)

colnames(AUT_fixtures_clone)[15] <- "CS_1-1"
colnames(AUT_fixtures_clone)[13] <- "CS_1-0"
colnames(AUT_fixtures_clone)[14] <- "CS_0-1"
colnames(AUT_fixtures_clone)[16] <- "CS_2-0"
colnames(AUT_fixtures_clone)[17] <- "CS_0-2"
colnames(AUT_fixtures_clone)[19] <- "CS_2-1"
colnames(AUT_fixtures_clone)[20] <- "CS_1-2"

AUT_fixtures_clone$`CS_1-1` <- round(1/AUT_fixtures_clone$`CS_1-1`, digits = 3)
AUT_fixtures_clone$`CS_1-0` <- round(1/AUT_fixtures_clone$`CS_1-0`, digits = 3)
AUT_fixtures_clone$`CS_0-1` <- round(1/AUT_fixtures_clone$`CS_0-1`, digits = 3)
AUT_fixtures_clone$`CS_2-0` <- round(1/AUT_fixtures_clone$`CS_2-0`, digits = 3)
AUT_fixtures_clone$`CS_0-2` <- round(1/AUT_fixtures_clone$`CS_0-2`, digits = 3)
AUT_fixtures_clone$`CS_2-1` <- round(1/AUT_fixtures_clone$`CS_2-1`, digits = 3)
AUT_fixtures_clone$`CS_1-2` <- round(1/AUT_fixtures_clone$`CS_1-2`, digits = 3)

colnames(AUT_fixtures_clone)[1] <- "league"
colnames(AUT_fixtures_clone)[2] <- "Hometeam"
colnames(AUT_fixtures_clone)[3] <- "Awayteam"
colnames(AUT_fixtures_clone)[92] <- "predscore"
colnames(AUT_fixtures_clone)[64] <- "ov25"
colnames(AUT_fixtures_clone)[66] <- "ov25odds"
colnames(AUT_fixtures_clone)[65] <- "un25"
colnames(AUT_fixtures_clone)[67] <- "un25odds"
colnames(AUT_fixtures_clone)[68] <- "BTTSY"
colnames(AUT_fixtures_clone)[69] <- "BTTSN"
colnames(AUT_fixtures_clone)[70] <- "BTTSYodds"
colnames(AUT_fixtures_clone)[71] <- "BTTSNodds"

AUT_fixtures_clone$matchid <- paste(AUT_fixtures_clone$Hometeam,AUT_fixtures_clone$Awayteam,sep = '-')

AUT_fixtures_clone <- AUT_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]

#######################################################################################################################
#######################################################################################################################
#ARG
ARG_fixtures_clone <- ARG_fixtures
colnames(ARG_fixtures_clone)[61] <- "Hwin"
colnames(ARG_fixtures_clone)[62] <- "Draw"
colnames(ARG_fixtures_clone)[63] <- "Awin"

ARG_fixtures_clone$Hwinodds <-   ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
  ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
  ARG_fixtures$arg_5_0 + ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
  ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
  ARG_fixtures$arg_6_5
ARG_fixtures_clone$Hwinodds <- round(1/ARG_fixtures_clone$Hwinodds, digits = 3)

ARG_fixtures_clone$Drawodds <-  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 + ARG_fixtures$arg_4_4 +
  ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6

ARG_fixtures_clone$Drawodds <- round(1/ARG_fixtures_clone$Drawodds, digits = 3)

ARG_fixtures_clone$Awinodds <-   ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
  ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
  ARG_fixtures$arg_0_5 + ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
  ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
  ARG_fixtures$arg_5_6

ARG_fixtures_clone$Awinodds <- round(1/ARG_fixtures_clone$Awinodds, digits = 3)

colnames(ARG_fixtures_clone)[15] <- "CS_1-1"
colnames(ARG_fixtures_clone)[13] <- "CS_1-0"
colnames(ARG_fixtures_clone)[14] <- "CS_0-1"
colnames(ARG_fixtures_clone)[16] <- "CS_2-0"
colnames(ARG_fixtures_clone)[17] <- "CS_0-2"
colnames(ARG_fixtures_clone)[19] <- "CS_2-1"
colnames(ARG_fixtures_clone)[20] <- "CS_1-2"

ARG_fixtures_clone$`CS_1-1` <- round(1/ARG_fixtures_clone$`CS_1-1`, digits = 3)
ARG_fixtures_clone$`CS_1-0` <- round(1/ARG_fixtures_clone$`CS_1-0`, digits = 3)
ARG_fixtures_clone$`CS_0-1` <- round(1/ARG_fixtures_clone$`CS_0-1`, digits = 3)
ARG_fixtures_clone$`CS_2-0` <- round(1/ARG_fixtures_clone$`CS_2-0`, digits = 3)
ARG_fixtures_clone$`CS_0-2` <- round(1/ARG_fixtures_clone$`CS_0-2`, digits = 3)
ARG_fixtures_clone$`CS_2-1` <- round(1/ARG_fixtures_clone$`CS_2-1`, digits = 3)
ARG_fixtures_clone$`CS_1-2` <- round(1/ARG_fixtures_clone$`CS_1-2`, digits = 3)

colnames(ARG_fixtures_clone)[1] <- "league"
colnames(ARG_fixtures_clone)[2] <- "Hometeam"
colnames(ARG_fixtures_clone)[3] <- "Awayteam"
colnames(ARG_fixtures_clone)[92] <- "predscore"
colnames(ARG_fixtures_clone)[64] <- "ov25"
colnames(ARG_fixtures_clone)[66] <- "ov25odds"
colnames(ARG_fixtures_clone)[65] <- "un25"
colnames(ARG_fixtures_clone)[67] <- "un25odds"
colnames(ARG_fixtures_clone)[68] <- "BTTSY"
colnames(ARG_fixtures_clone)[69] <- "BTTSN"
colnames(ARG_fixtures_clone)[70] <- "BTTSYodds"
colnames(ARG_fixtures_clone)[71] <- "BTTSNodds"

ARG_fixtures_clone$matchid <- paste(ARG_fixtures_clone$Hometeam,ARG_fixtures_clone$Awayteam,sep = '-')

ARG_fixtures_clone <- ARG_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
######################################################################################################################
######################################################################################################################
#BRA
BRA_fixtures_clone <- BRA_fixtures
colnames(BRA_fixtures_clone)[61] <- "Hwin"
colnames(BRA_fixtures_clone)[62] <- "Draw"
colnames(BRA_fixtures_clone)[63] <- "Awin"

BRA_fixtures_clone$Hwinodds <-   BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
  BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
  BRA_fixtures$bra_5_0 + BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
  BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
  BRA_fixtures$bra_6_5
BRA_fixtures_clone$Hwinodds <- round(1/BRA_fixtures_clone$Hwinodds, digits = 3)

BRA_fixtures_clone$Drawodds <-  BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 + BRA_fixtures$bra_4_4 +
  BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6

BRA_fixtures_clone$Drawodds <- round(1/BRA_fixtures_clone$Drawodds, digits = 3)

BRA_fixtures_clone$Awinodds <-   BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
  BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
  BRA_fixtures$bra_0_5 + BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
  BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
  BRA_fixtures$bra_5_6

BRA_fixtures_clone$Awinodds <- round(1/BRA_fixtures_clone$Awinodds, digits = 3)

colnames(BRA_fixtures_clone)[15] <- "CS_1-1"
colnames(BRA_fixtures_clone)[13] <- "CS_1-0"
colnames(BRA_fixtures_clone)[14] <- "CS_0-1"
colnames(BRA_fixtures_clone)[16] <- "CS_2-0"
colnames(BRA_fixtures_clone)[17] <- "CS_0-2"
colnames(BRA_fixtures_clone)[19] <- "CS_2-1"
colnames(BRA_fixtures_clone)[20] <- "CS_1-2"

BRA_fixtures_clone$`CS_1-1` <- round(1/BRA_fixtures_clone$`CS_1-1`, digits = 3)
BRA_fixtures_clone$`CS_1-0` <- round(1/BRA_fixtures_clone$`CS_1-0`, digits = 3)
BRA_fixtures_clone$`CS_0-1` <- round(1/BRA_fixtures_clone$`CS_0-1`, digits = 3)
BRA_fixtures_clone$`CS_2-0` <- round(1/BRA_fixtures_clone$`CS_2-0`, digits = 3)
BRA_fixtures_clone$`CS_0-2` <- round(1/BRA_fixtures_clone$`CS_0-2`, digits = 3)
BRA_fixtures_clone$`CS_2-1` <- round(1/BRA_fixtures_clone$`CS_2-1`, digits = 3)
BRA_fixtures_clone$`CS_1-2` <- round(1/BRA_fixtures_clone$`CS_1-2`, digits = 3)

colnames(BRA_fixtures_clone)[1] <- "league"
colnames(BRA_fixtures_clone)[2] <- "Hometeam"
colnames(BRA_fixtures_clone)[3] <- "Awayteam"
colnames(BRA_fixtures_clone)[92] <- "predscore"
colnames(BRA_fixtures_clone)[64] <- "ov25"
colnames(BRA_fixtures_clone)[66] <- "ov25odds"
colnames(BRA_fixtures_clone)[65] <- "un25"
colnames(BRA_fixtures_clone)[67] <- "un25odds"
colnames(BRA_fixtures_clone)[68] <- "BTTSY"
colnames(BRA_fixtures_clone)[69] <- "BTTSN"
colnames(BRA_fixtures_clone)[70] <- "BTTSYodds"
colnames(BRA_fixtures_clone)[71] <- "BTTSNodds"

BRA_fixtures_clone$matchid <- paste(BRA_fixtures_clone$Hometeam,BRA_fixtures_clone$Awayteam,sep = '-')

BRA_fixtures_clone <- BRA_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]

######################################################################################################################
######################################################################################################################
#CHN
CHN_fixtures_clone <- CHN_fixtures
colnames(CHN_fixtures_clone)[61] <- "Hwin"
colnames(CHN_fixtures_clone)[62] <- "Draw"
colnames(CHN_fixtures_clone)[63] <- "Awin"

CHN_fixtures_clone$Hwinodds <-   CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
  CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
  CHN_fixtures$chn_5_0 + CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
  CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
  CHN_fixtures$chn_6_5
CHN_fixtures_clone$Hwinodds <- round(1/CHN_fixtures_clone$Hwinodds, digits = 3)

CHN_fixtures_clone$Drawodds <-  CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 + CHN_fixtures$chn_4_4 +
  CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6

CHN_fixtures_clone$Drawodds <- round(1/CHN_fixtures_clone$Drawodds, digits = 3)

CHN_fixtures_clone$Awinodds <-   CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
  CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
  CHN_fixtures$chn_0_5 + CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
  CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
  CHN_fixtures$chn_5_6

CHN_fixtures_clone$Awinodds <- round(1/CHN_fixtures_clone$Awinodds, digits = 3)

colnames(CHN_fixtures_clone)[15] <- "CS_1-1"
colnames(CHN_fixtures_clone)[13] <- "CS_1-0"
colnames(CHN_fixtures_clone)[14] <- "CS_0-1"
colnames(CHN_fixtures_clone)[16] <- "CS_2-0"
colnames(CHN_fixtures_clone)[17] <- "CS_0-2"
colnames(CHN_fixtures_clone)[19] <- "CS_2-1"
colnames(CHN_fixtures_clone)[20] <- "CS_1-2"

CHN_fixtures_clone$`CS_1-1` <- round(1/CHN_fixtures_clone$`CS_1-1`, digits = 3)
CHN_fixtures_clone$`CS_1-0` <- round(1/CHN_fixtures_clone$`CS_1-0`, digits = 3)
CHN_fixtures_clone$`CS_0-1` <- round(1/CHN_fixtures_clone$`CS_0-1`, digits = 3)
CHN_fixtures_clone$`CS_2-0` <- round(1/CHN_fixtures_clone$`CS_2-0`, digits = 3)
CHN_fixtures_clone$`CS_0-2` <- round(1/CHN_fixtures_clone$`CS_0-2`, digits = 3)
CHN_fixtures_clone$`CS_2-1` <- round(1/CHN_fixtures_clone$`CS_2-1`, digits = 3)
CHN_fixtures_clone$`CS_1-2` <- round(1/CHN_fixtures_clone$`CS_1-2`, digits = 3)

colnames(CHN_fixtures_clone)[1] <- "league"
colnames(CHN_fixtures_clone)[2] <- "Hometeam"
colnames(CHN_fixtures_clone)[3] <- "Awayteam"
colnames(CHN_fixtures_clone)[92] <- "predscore"
colnames(CHN_fixtures_clone)[64] <- "ov25"
colnames(CHN_fixtures_clone)[66] <- "ov25odds"
colnames(CHN_fixtures_clone)[65] <- "un25"
colnames(CHN_fixtures_clone)[67] <- "un25odds"
colnames(CHN_fixtures_clone)[68] <- "BTTSY"
colnames(CHN_fixtures_clone)[69] <- "BTTSN"
colnames(CHN_fixtures_clone)[70] <- "BTTSYodds"
colnames(CHN_fixtures_clone)[71] <- "BTTSNodds"

CHN_fixtures_clone$matchid <- paste(CHN_fixtures_clone$Hometeam,CHN_fixtures_clone$Awayteam,sep = '-')

CHN_fixtures_clone <- CHN_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
######################################################################################################################
#DNK
DNK_fixtures_clone <- DNK_fixtures
colnames(DNK_fixtures_clone)[61] <- "Hwin"
colnames(DNK_fixtures_clone)[62] <- "Draw"
colnames(DNK_fixtures_clone)[63] <- "Awin"

DNK_fixtures_clone$Hwinodds <-   DNK_fixtures$dnk_1_0 + DNK_fixtures$dnk_2_0 + DNK_fixtures$dnk_2_1 + DNK_fixtures$dnk_3_0 + DNK_fixtures$dnk_3_1 +
  DNK_fixtures$dnk_3_2 + DNK_fixtures$dnk_4_0 + DNK_fixtures$dnk_4_1 + DNK_fixtures$dnk_4_2 + DNK_fixtures$dnk_4_3 +
  DNK_fixtures$dnk_5_0 + DNK_fixtures$dnk_5_1 + DNK_fixtures$dnk_5_2 + DNK_fixtures$dnk_5_3 + DNK_fixtures$dnk_5_4 +
  DNK_fixtures$dnk_6_0 + DNK_fixtures$dnk_6_1 + DNK_fixtures$dnk_6_2 + DNK_fixtures$dnk_6_3 + DNK_fixtures$dnk_6_4 +
  DNK_fixtures$dnk_6_5
DNK_fixtures_clone$Hwinodds <- round(1/DNK_fixtures_clone$Hwinodds, digits = 3)

DNK_fixtures_clone$Drawodds <-  DNK_fixtures$dnk_0_0 + DNK_fixtures$dnk_1_1 + DNK_fixtures$dnk_2_2 + DNK_fixtures$dnk_3_3 + DNK_fixtures$dnk_4_4 +
  DNK_fixtures$dnk_5_5 + DNK_fixtures$dnk_6_6

DNK_fixtures_clone$Drawodds <- round(1/DNK_fixtures_clone$Drawodds, digits = 3)

DNK_fixtures_clone$Awinodds <-   DNK_fixtures$dnk_0_1 + DNK_fixtures$dnk_0_2 + DNK_fixtures$dnk_1_2 + DNK_fixtures$dnk_0_3 + DNK_fixtures$dnk_1_3 +
  DNK_fixtures$dnk_2_3 + DNK_fixtures$dnk_0_4 + DNK_fixtures$dnk_1_4 + DNK_fixtures$dnk_2_4 + DNK_fixtures$dnk_3_4 +
  DNK_fixtures$dnk_0_5 + DNK_fixtures$dnk_1_5 + DNK_fixtures$dnk_2_5 + DNK_fixtures$dnk_3_5 + DNK_fixtures$dnk_4_5 +
  DNK_fixtures$dnk_0_6 + DNK_fixtures$dnk_1_6 + DNK_fixtures$dnk_2_6 + DNK_fixtures$dnk_3_6 + DNK_fixtures$dnk_4_6 +
  DNK_fixtures$dnk_5_6

DNK_fixtures_clone$Awinodds <- round(1/DNK_fixtures_clone$Awinodds, digits = 3)

colnames(DNK_fixtures_clone)[15] <- "CS_1-1"
colnames(DNK_fixtures_clone)[13] <- "CS_1-0"
colnames(DNK_fixtures_clone)[14] <- "CS_0-1"
colnames(DNK_fixtures_clone)[16] <- "CS_2-0"
colnames(DNK_fixtures_clone)[17] <- "CS_0-2"
colnames(DNK_fixtures_clone)[19] <- "CS_2-1"
colnames(DNK_fixtures_clone)[20] <- "CS_1-2"

DNK_fixtures_clone$`CS_1-1` <- round(1/DNK_fixtures_clone$`CS_1-1`, digits = 3)
DNK_fixtures_clone$`CS_1-0` <- round(1/DNK_fixtures_clone$`CS_1-0`, digits = 3)
DNK_fixtures_clone$`CS_0-1` <- round(1/DNK_fixtures_clone$`CS_0-1`, digits = 3)
DNK_fixtures_clone$`CS_2-0` <- round(1/DNK_fixtures_clone$`CS_2-0`, digits = 3)
DNK_fixtures_clone$`CS_0-2` <- round(1/DNK_fixtures_clone$`CS_0-2`, digits = 3)
DNK_fixtures_clone$`CS_2-1` <- round(1/DNK_fixtures_clone$`CS_2-1`, digits = 3)
DNK_fixtures_clone$`CS_1-2` <- round(1/DNK_fixtures_clone$`CS_1-2`, digits = 3)

colnames(DNK_fixtures_clone)[1] <- "league"
colnames(DNK_fixtures_clone)[2] <- "Hometeam"
colnames(DNK_fixtures_clone)[3] <- "Awayteam"
colnames(DNK_fixtures_clone)[92] <- "predscore"
colnames(DNK_fixtures_clone)[64] <- "ov25"
colnames(DNK_fixtures_clone)[66] <- "ov25odds"
colnames(DNK_fixtures_clone)[65] <- "un25"
colnames(DNK_fixtures_clone)[67] <- "un25odds"
colnames(DNK_fixtures_clone)[68] <- "BTTSY"
colnames(DNK_fixtures_clone)[69] <- "BTTSN"
colnames(DNK_fixtures_clone)[70] <- "BTTSYodds"
colnames(DNK_fixtures_clone)[71] <- "BTTSNodds"

DNK_fixtures_clone$matchid <- paste(DNK_fixtures_clone$Hometeam,DNK_fixtures_clone$Awayteam,sep = '-')

DNK_fixtures_clone <- DNK_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#######################################################################################################################
#FIN
FIN_fixtures_clone <- FIN_fixtures
colnames(FIN_fixtures_clone)[61] <- "Hwin"
colnames(FIN_fixtures_clone)[62] <- "Draw"
colnames(FIN_fixtures_clone)[63] <- "Awin"

FIN_fixtures_clone$Hwinodds <-   FIN_fixtures$fin_1_0 + FIN_fixtures$fin_2_0 + FIN_fixtures$fin_2_1 + FIN_fixtures$fin_3_0 + FIN_fixtures$fin_3_1 +
  FIN_fixtures$fin_3_2 + FIN_fixtures$fin_4_0 + FIN_fixtures$fin_4_1 + FIN_fixtures$fin_4_2 + FIN_fixtures$fin_4_3 +
  FIN_fixtures$fin_5_0 + FIN_fixtures$fin_5_1 + FIN_fixtures$fin_5_2 + FIN_fixtures$fin_5_3 + FIN_fixtures$fin_5_4 +
  FIN_fixtures$fin_6_0 + FIN_fixtures$fin_6_1 + FIN_fixtures$fin_6_2 + FIN_fixtures$fin_6_3 + FIN_fixtures$fin_6_4 +
  FIN_fixtures$fin_6_5
FIN_fixtures_clone$Hwinodds <- round(1/FIN_fixtures_clone$Hwinodds, digits = 3)

FIN_fixtures_clone$Drawodds <-  FIN_fixtures$fin_0_0 + FIN_fixtures$fin_1_1 + FIN_fixtures$fin_2_2 + FIN_fixtures$fin_3_3 + FIN_fixtures$fin_4_4 +
  FIN_fixtures$fin_5_5 + FIN_fixtures$fin_6_6

FIN_fixtures_clone$Drawodds <- round(1/FIN_fixtures_clone$Drawodds, digits = 3)

FIN_fixtures_clone$Awinodds <-   FIN_fixtures$fin_0_1 + FIN_fixtures$fin_0_2 + FIN_fixtures$fin_1_2 + FIN_fixtures$fin_0_3 + FIN_fixtures$fin_1_3 +
  FIN_fixtures$fin_2_3 + FIN_fixtures$fin_0_4 + FIN_fixtures$fin_1_4 + FIN_fixtures$fin_2_4 + FIN_fixtures$fin_3_4 +
  FIN_fixtures$fin_0_5 + FIN_fixtures$fin_1_5 + FIN_fixtures$fin_2_5 + FIN_fixtures$fin_3_5 + FIN_fixtures$fin_4_5 +
  FIN_fixtures$fin_0_6 + FIN_fixtures$fin_1_6 + FIN_fixtures$fin_2_6 + FIN_fixtures$fin_3_6 + FIN_fixtures$fin_4_6 +
  FIN_fixtures$fin_5_6

FIN_fixtures_clone$Awinodds <- round(1/FIN_fixtures_clone$Awinodds, digits = 3)

colnames(FIN_fixtures_clone)[15] <- "CS_1-1"
colnames(FIN_fixtures_clone)[13] <- "CS_1-0"
colnames(FIN_fixtures_clone)[14] <- "CS_0-1"
colnames(FIN_fixtures_clone)[16] <- "CS_2-0"
colnames(FIN_fixtures_clone)[17] <- "CS_0-2"
colnames(FIN_fixtures_clone)[19] <- "CS_2-1"
colnames(FIN_fixtures_clone)[20] <- "CS_1-2"

FIN_fixtures_clone$`CS_1-1` <- round(1/FIN_fixtures_clone$`CS_1-1`, digits = 3)
FIN_fixtures_clone$`CS_1-0` <- round(1/FIN_fixtures_clone$`CS_1-0`, digits = 3)
FIN_fixtures_clone$`CS_0-1` <- round(1/FIN_fixtures_clone$`CS_0-1`, digits = 3)
FIN_fixtures_clone$`CS_2-0` <- round(1/FIN_fixtures_clone$`CS_2-0`, digits = 3)
FIN_fixtures_clone$`CS_0-2` <- round(1/FIN_fixtures_clone$`CS_0-2`, digits = 3)
FIN_fixtures_clone$`CS_2-1` <- round(1/FIN_fixtures_clone$`CS_2-1`, digits = 3)
FIN_fixtures_clone$`CS_1-2` <- round(1/FIN_fixtures_clone$`CS_1-2`, digits = 3)

colnames(FIN_fixtures_clone)[1] <- "league"
colnames(FIN_fixtures_clone)[2] <- "Hometeam"
colnames(FIN_fixtures_clone)[3] <- "Awayteam"
colnames(FIN_fixtures_clone)[92] <- "predscore"
colnames(FIN_fixtures_clone)[64] <- "ov25"
colnames(FIN_fixtures_clone)[66] <- "ov25odds"
colnames(FIN_fixtures_clone)[65] <- "un25"
colnames(FIN_fixtures_clone)[67] <- "un25odds"
colnames(FIN_fixtures_clone)[68] <- "BTTSY"
colnames(FIN_fixtures_clone)[69] <- "BTTSN"
colnames(FIN_fixtures_clone)[70] <- "BTTSYodds"
colnames(FIN_fixtures_clone)[71] <- "BTTSNodds"

FIN_fixtures_clone$matchid <- paste(FIN_fixtures_clone$Hometeam,FIN_fixtures_clone$Awayteam,sep = '-')

FIN_fixtures_clone <- FIN_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#######################################################################################################################
#IRL
IRL_fixtures_clone <- IRL_fixtures
colnames(IRL_fixtures_clone)[61] <- "Hwin"
colnames(IRL_fixtures_clone)[62] <- "Draw"
colnames(IRL_fixtures_clone)[63] <- "Awin"

IRL_fixtures_clone$Hwinodds <-   IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
  IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
  IRL_fixtures$irl_5_0 + IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
  IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
  IRL_fixtures$irl_6_5
IRL_fixtures_clone$Hwinodds <- round(1/IRL_fixtures_clone$Hwinodds, digits = 3)

IRL_fixtures_clone$Drawodds <-  IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 + IRL_fixtures$irl_4_4 +
  IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6

IRL_fixtures_clone$Drawodds <- round(1/IRL_fixtures_clone$Drawodds, digits = 3)

IRL_fixtures_clone$Awinodds <-   IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
  IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
  IRL_fixtures$irl_0_5 + IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
  IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
  IRL_fixtures$irl_5_6

IRL_fixtures_clone$Awinodds <- round(1/IRL_fixtures_clone$Awinodds, digits = 3)

colnames(IRL_fixtures_clone)[15] <- "CS_1-1"
colnames(IRL_fixtures_clone)[13] <- "CS_1-0"
colnames(IRL_fixtures_clone)[14] <- "CS_0-1"
colnames(IRL_fixtures_clone)[16] <- "CS_2-0"
colnames(IRL_fixtures_clone)[17] <- "CS_0-2"
colnames(IRL_fixtures_clone)[19] <- "CS_2-1"
colnames(IRL_fixtures_clone)[20] <- "CS_1-2"

IRL_fixtures_clone$`CS_1-1` <- round(1/IRL_fixtures_clone$`CS_1-1`, digits = 3)
IRL_fixtures_clone$`CS_1-0` <- round(1/IRL_fixtures_clone$`CS_1-0`, digits = 3)
IRL_fixtures_clone$`CS_0-1` <- round(1/IRL_fixtures_clone$`CS_0-1`, digits = 3)
IRL_fixtures_clone$`CS_2-0` <- round(1/IRL_fixtures_clone$`CS_2-0`, digits = 3)
IRL_fixtures_clone$`CS_0-2` <- round(1/IRL_fixtures_clone$`CS_0-2`, digits = 3)
IRL_fixtures_clone$`CS_2-1` <- round(1/IRL_fixtures_clone$`CS_2-1`, digits = 3)
IRL_fixtures_clone$`CS_1-2` <- round(1/IRL_fixtures_clone$`CS_1-2`, digits = 3)

colnames(IRL_fixtures_clone)[1] <- "league"
colnames(IRL_fixtures_clone)[2] <- "Hometeam"
colnames(IRL_fixtures_clone)[3] <- "Awayteam"
colnames(IRL_fixtures_clone)[92] <- "predscore"
colnames(IRL_fixtures_clone)[64] <- "ov25"
colnames(IRL_fixtures_clone)[66] <- "ov25odds"
colnames(IRL_fixtures_clone)[65] <- "un25"
colnames(IRL_fixtures_clone)[67] <- "un25odds"
colnames(IRL_fixtures_clone)[68] <- "BTTSY"
colnames(IRL_fixtures_clone)[69] <- "BTTSN"
colnames(IRL_fixtures_clone)[70] <- "BTTSYodds"
colnames(IRL_fixtures_clone)[71] <- "BTTSNodds"

IRL_fixtures_clone$matchid <- paste(IRL_fixtures_clone$Hometeam,IRL_fixtures_clone$Awayteam,sep = '-')

IRL_fixtures_clone <- IRL_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
########################################################################################################################
#Ec
JPN_fixtures_clone <- JPN_fixtures
colnames(JPN_fixtures_clone)[61] <- "Hwin"
colnames(JPN_fixtures_clone)[62] <- "Draw"
colnames(JPN_fixtures_clone)[63] <- "Awin"

JPN_fixtures_clone$Hwinodds <-   JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
  JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
  JPN_fixtures$jpn_5_0 + JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
  JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
  JPN_fixtures$jpn_6_5
JPN_fixtures_clone$Hwinodds <- round(1/JPN_fixtures_clone$Hwinodds, digits = 3)

JPN_fixtures_clone$Drawodds <-  JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 + JPN_fixtures$jpn_4_4 +
  JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6

JPN_fixtures_clone$Drawodds <- round(1/JPN_fixtures_clone$Drawodds, digits = 3)

JPN_fixtures_clone$Awinodds <-   JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
  JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
  JPN_fixtures$jpn_0_5 + JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
  JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
  JPN_fixtures$jpn_5_6

JPN_fixtures_clone$Awinodds <- round(1/JPN_fixtures_clone$Awinodds, digits = 3)

colnames(JPN_fixtures_clone)[15] <- "CS_1-1"
colnames(JPN_fixtures_clone)[13] <- "CS_1-0"
colnames(JPN_fixtures_clone)[14] <- "CS_0-1"
colnames(JPN_fixtures_clone)[16] <- "CS_2-0"
colnames(JPN_fixtures_clone)[17] <- "CS_0-2"
colnames(JPN_fixtures_clone)[19] <- "CS_2-1"
colnames(JPN_fixtures_clone)[20] <- "CS_1-2"

JPN_fixtures_clone$`CS_1-1` <- round(1/JPN_fixtures_clone$`CS_1-1`, digits = 3)
JPN_fixtures_clone$`CS_1-0` <- round(1/JPN_fixtures_clone$`CS_1-0`, digits = 3)
JPN_fixtures_clone$`CS_0-1` <- round(1/JPN_fixtures_clone$`CS_0-1`, digits = 3)
JPN_fixtures_clone$`CS_2-0` <- round(1/JPN_fixtures_clone$`CS_2-0`, digits = 3)
JPN_fixtures_clone$`CS_0-2` <- round(1/JPN_fixtures_clone$`CS_0-2`, digits = 3)
JPN_fixtures_clone$`CS_2-1` <- round(1/JPN_fixtures_clone$`CS_2-1`, digits = 3)
JPN_fixtures_clone$`CS_1-2` <- round(1/JPN_fixtures_clone$`CS_1-2`, digits = 3)

colnames(JPN_fixtures_clone)[1] <- "league"
colnames(JPN_fixtures_clone)[2] <- "Hometeam"
colnames(JPN_fixtures_clone)[3] <- "Awayteam"
colnames(JPN_fixtures_clone)[92] <- "predscore"
colnames(JPN_fixtures_clone)[64] <- "ov25"
colnames(JPN_fixtures_clone)[66] <- "ov25odds"
colnames(JPN_fixtures_clone)[65] <- "un25"
colnames(JPN_fixtures_clone)[67] <- "un25odds"
colnames(JPN_fixtures_clone)[68] <- "BTTSY"
colnames(JPN_fixtures_clone)[69] <- "BTTSN"
colnames(JPN_fixtures_clone)[70] <- "BTTSYodds"
colnames(JPN_fixtures_clone)[71] <- "BTTSNodds"

JPN_fixtures_clone$matchid <- paste(JPN_fixtures_clone$Hometeam,JPN_fixtures_clone$Awayteam,sep = '-')

JPN_fixtures_clone <- JPN_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
########################################################################################################################
#MEX
MEX_fixtures_clone <- MEX_fixtures
colnames(MEX_fixtures_clone)[61] <- "Hwin"
colnames(MEX_fixtures_clone)[62] <- "Draw"
colnames(MEX_fixtures_clone)[63] <- "Awin"

MEX_fixtures_clone$Hwinodds <-   MEX_fixtures$mex_1_0 + MEX_fixtures$mex_2_0 + MEX_fixtures$mex_2_1 + MEX_fixtures$mex_3_0 + MEX_fixtures$mex_3_1 +
  MEX_fixtures$mex_3_2 + MEX_fixtures$mex_4_0 + MEX_fixtures$mex_4_1 + MEX_fixtures$mex_4_2 + MEX_fixtures$mex_4_3 +
  MEX_fixtures$mex_5_0 + MEX_fixtures$mex_5_1 + MEX_fixtures$mex_5_2 + MEX_fixtures$mex_5_3 + MEX_fixtures$mex_5_4 +
  MEX_fixtures$mex_6_0 + MEX_fixtures$mex_6_1 + MEX_fixtures$mex_6_2 + MEX_fixtures$mex_6_3 + MEX_fixtures$mex_6_4 +
  MEX_fixtures$mex_6_5
MEX_fixtures_clone$Hwinodds <- round(1/MEX_fixtures_clone$Hwinodds, digits = 3)

MEX_fixtures_clone$Drawodds <-  MEX_fixtures$mex_0_0 + MEX_fixtures$mex_1_1 + MEX_fixtures$mex_2_2 + MEX_fixtures$mex_3_3 + MEX_fixtures$mex_4_4 +
  MEX_fixtures$mex_5_5 + MEX_fixtures$mex_6_6

MEX_fixtures_clone$Drawodds <- round(1/MEX_fixtures_clone$Drawodds, digits = 3)

MEX_fixtures_clone$Awinodds <-   MEX_fixtures$mex_0_1 + MEX_fixtures$mex_0_2 + MEX_fixtures$mex_1_2 + MEX_fixtures$mex_0_3 + MEX_fixtures$mex_1_3 +
  MEX_fixtures$mex_2_3 + MEX_fixtures$mex_0_4 + MEX_fixtures$mex_1_4 + MEX_fixtures$mex_2_4 + MEX_fixtures$mex_3_4 +
  MEX_fixtures$mex_0_5 + MEX_fixtures$mex_1_5 + MEX_fixtures$mex_2_5 + MEX_fixtures$mex_3_5 + MEX_fixtures$mex_4_5 +
  MEX_fixtures$mex_0_6 + MEX_fixtures$mex_1_6 + MEX_fixtures$mex_2_6 + MEX_fixtures$mex_3_6 + MEX_fixtures$mex_4_6 +
  MEX_fixtures$mex_5_6

MEX_fixtures_clone$Awinodds <- round(1/MEX_fixtures_clone$Awinodds, digits = 3)

colnames(MEX_fixtures_clone)[15] <- "CS_1-1"
colnames(MEX_fixtures_clone)[13] <- "CS_1-0"
colnames(MEX_fixtures_clone)[14] <- "CS_0-1"
colnames(MEX_fixtures_clone)[16] <- "CS_2-0"
colnames(MEX_fixtures_clone)[17] <- "CS_0-2"
colnames(MEX_fixtures_clone)[19] <- "CS_2-1"
colnames(MEX_fixtures_clone)[20] <- "CS_1-2"

MEX_fixtures_clone$`CS_1-1` <- round(1/MEX_fixtures_clone$`CS_1-1`, digits = 3)
MEX_fixtures_clone$`CS_1-0` <- round(1/MEX_fixtures_clone$`CS_1-0`, digits = 3)
MEX_fixtures_clone$`CS_0-1` <- round(1/MEX_fixtures_clone$`CS_0-1`, digits = 3)
MEX_fixtures_clone$`CS_2-0` <- round(1/MEX_fixtures_clone$`CS_2-0`, digits = 3)
MEX_fixtures_clone$`CS_0-2` <- round(1/MEX_fixtures_clone$`CS_0-2`, digits = 3)
MEX_fixtures_clone$`CS_2-1` <- round(1/MEX_fixtures_clone$`CS_2-1`, digits = 3)
MEX_fixtures_clone$`CS_1-2` <- round(1/MEX_fixtures_clone$`CS_1-2`, digits = 3)

colnames(MEX_fixtures_clone)[1] <- "league"
colnames(MEX_fixtures_clone)[2] <- "Hometeam"
colnames(MEX_fixtures_clone)[3] <- "Awayteam"
colnames(MEX_fixtures_clone)[92] <- "predscore"
colnames(MEX_fixtures_clone)[64] <- "ov25"
colnames(MEX_fixtures_clone)[66] <- "ov25odds"
colnames(MEX_fixtures_clone)[65] <- "un25"
colnames(MEX_fixtures_clone)[67] <- "un25odds"
colnames(MEX_fixtures_clone)[68] <- "BTTSY"
colnames(MEX_fixtures_clone)[69] <- "BTTSN"
colnames(MEX_fixtures_clone)[70] <- "BTTSYodds"
colnames(MEX_fixtures_clone)[71] <- "BTTSNodds"

MEX_fixtures_clone$matchid <- paste(MEX_fixtures_clone$Hometeam,MEX_fixtures_clone$Awayteam,sep = '-')

MEX_fixtures_clone <- MEX_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
######################################################################################################################
######################################################################################################################
#NOR
NOR_fixtures_clone <- NOR_fixtures
colnames(NOR_fixtures_clone)[61] <- "Hwin"
colnames(NOR_fixtures_clone)[62] <- "Draw"
colnames(NOR_fixtures_clone)[63] <- "Awin"

NOR_fixtures_clone$Hwinodds <-   NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
  NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
  NOR_fixtures$nor_5_0 + NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
  NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
  NOR_fixtures$nor_6_5
NOR_fixtures_clone$Hwinodds <- round(1/NOR_fixtures_clone$Hwinodds, digits = 3)

NOR_fixtures_clone$Drawodds <-  NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 + NOR_fixtures$nor_4_4 +
  NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6

NOR_fixtures_clone$Drawodds <- round(1/NOR_fixtures_clone$Drawodds, digits = 3)

NOR_fixtures_clone$Awinodds <-   NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
  NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
  NOR_fixtures$nor_0_5 + NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
  NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
  NOR_fixtures$nor_5_6

NOR_fixtures_clone$Awinodds <- round(1/NOR_fixtures_clone$Awinodds, digits = 3)

colnames(NOR_fixtures_clone)[15] <- "CS_1-1"
colnames(NOR_fixtures_clone)[13] <- "CS_1-0"
colnames(NOR_fixtures_clone)[14] <- "CS_0-1"
colnames(NOR_fixtures_clone)[16] <- "CS_2-0"
colnames(NOR_fixtures_clone)[17] <- "CS_0-2"
colnames(NOR_fixtures_clone)[19] <- "CS_2-1"
colnames(NOR_fixtures_clone)[20] <- "CS_1-2"

NOR_fixtures_clone$`CS_1-1` <- round(1/NOR_fixtures_clone$`CS_1-1`, digits = 3)
NOR_fixtures_clone$`CS_1-0` <- round(1/NOR_fixtures_clone$`CS_1-0`, digits = 3)
NOR_fixtures_clone$`CS_0-1` <- round(1/NOR_fixtures_clone$`CS_0-1`, digits = 3)
NOR_fixtures_clone$`CS_2-0` <- round(1/NOR_fixtures_clone$`CS_2-0`, digits = 3)
NOR_fixtures_clone$`CS_0-2` <- round(1/NOR_fixtures_clone$`CS_0-2`, digits = 3)
NOR_fixtures_clone$`CS_2-1` <- round(1/NOR_fixtures_clone$`CS_2-1`, digits = 3)
NOR_fixtures_clone$`CS_1-2` <- round(1/NOR_fixtures_clone$`CS_1-2`, digits = 3)

colnames(NOR_fixtures_clone)[1] <- "league"
colnames(NOR_fixtures_clone)[2] <- "Hometeam"
colnames(NOR_fixtures_clone)[3] <- "Awayteam"
colnames(NOR_fixtures_clone)[92] <- "predscore"
colnames(NOR_fixtures_clone)[64] <- "ov25"
colnames(NOR_fixtures_clone)[66] <- "ov25odds"
colnames(NOR_fixtures_clone)[65] <- "un25"
colnames(NOR_fixtures_clone)[67] <- "un25odds"
colnames(NOR_fixtures_clone)[68] <- "BTTSY"
colnames(NOR_fixtures_clone)[69] <- "BTTSN"
colnames(NOR_fixtures_clone)[70] <- "BTTSYodds"
colnames(NOR_fixtures_clone)[71] <- "BTTSNodds"

NOR_fixtures_clone$matchid <- paste(NOR_fixtures_clone$Hometeam,NOR_fixtures_clone$Awayteam,sep = '-')

NOR_fixtures_clone <- NOR_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#POL
POL_fixtures_clone <- POL_fixtures
colnames(POL_fixtures_clone)[61] <- "Hwin"
colnames(POL_fixtures_clone)[62] <- "Draw"
colnames(POL_fixtures_clone)[63] <- "Awin"

POL_fixtures_clone$Hwinodds <-   POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
  POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
  POL_fixtures$pol_5_0 + POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
  POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
  POL_fixtures$pol_6_5
POL_fixtures_clone$Hwinodds <- round(1/POL_fixtures_clone$Hwinodds, digits = 3)

POL_fixtures_clone$Drawodds <-  POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 + POL_fixtures$pol_4_4 +
  POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6

POL_fixtures_clone$Drawodds <- round(1/POL_fixtures_clone$Drawodds, digits = 3)

POL_fixtures_clone$Awinodds <-   POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
  POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
  POL_fixtures$pol_0_5 + POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
  POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
  POL_fixtures$pol_5_6

POL_fixtures_clone$Awinodds <- round(1/POL_fixtures_clone$Awinodds, digits = 3)

colnames(POL_fixtures_clone)[15] <- "CS_1-1"
colnames(POL_fixtures_clone)[13] <- "CS_1-0"
colnames(POL_fixtures_clone)[14] <- "CS_0-1"
colnames(POL_fixtures_clone)[16] <- "CS_2-0"
colnames(POL_fixtures_clone)[17] <- "CS_0-2"
colnames(POL_fixtures_clone)[19] <- "CS_2-1"
colnames(POL_fixtures_clone)[20] <- "CS_1-2"

POL_fixtures_clone$`CS_1-1` <- round(1/POL_fixtures_clone$`CS_1-1`, digits = 3)
POL_fixtures_clone$`CS_1-0` <- round(1/POL_fixtures_clone$`CS_1-0`, digits = 3)
POL_fixtures_clone$`CS_0-1` <- round(1/POL_fixtures_clone$`CS_0-1`, digits = 3)
POL_fixtures_clone$`CS_2-0` <- round(1/POL_fixtures_clone$`CS_2-0`, digits = 3)
POL_fixtures_clone$`CS_0-2` <- round(1/POL_fixtures_clone$`CS_0-2`, digits = 3)
POL_fixtures_clone$`CS_2-1` <- round(1/POL_fixtures_clone$`CS_2-1`, digits = 3)
POL_fixtures_clone$`CS_1-2` <- round(1/POL_fixtures_clone$`CS_1-2`, digits = 3)

colnames(POL_fixtures_clone)[1] <- "league"
colnames(POL_fixtures_clone)[2] <- "Hometeam"
colnames(POL_fixtures_clone)[3] <- "Awayteam"
colnames(POL_fixtures_clone)[92] <- "predscore"
colnames(POL_fixtures_clone)[64] <- "ov25"
colnames(POL_fixtures_clone)[66] <- "ov25odds"
colnames(POL_fixtures_clone)[65] <- "un25"
colnames(POL_fixtures_clone)[67] <- "un25odds"
colnames(POL_fixtures_clone)[68] <- "BTTSY"
colnames(POL_fixtures_clone)[69] <- "BTTSN"
colnames(POL_fixtures_clone)[70] <- "BTTSYodds"
colnames(POL_fixtures_clone)[71] <- "BTTSNodds"

POL_fixtures_clone$matchid <- paste(POL_fixtures_clone$Hometeam,POL_fixtures_clone$Awayteam,sep = '-')

POL_fixtures_clone <- POL_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
ROU_fixtures_clone <- ROU_fixtures
colnames(ROU_fixtures_clone)[61] <- "Hwin"
colnames(ROU_fixtures_clone)[62] <- "Draw"
colnames(ROU_fixtures_clone)[63] <- "Awin"

ROU_fixtures_clone$Hwinodds <-   ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
  ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
  ROU_fixtures$rou_5_0 + ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
  ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
  ROU_fixtures$rou_6_5
ROU_fixtures_clone$Hwinodds <- round(1/ROU_fixtures_clone$Hwinodds, digits = 3)

ROU_fixtures_clone$Drawodds <-  ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 + ROU_fixtures$rou_4_4 +
  ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6

ROU_fixtures_clone$Drawodds <- round(1/ROU_fixtures_clone$Drawodds, digits = 3)

ROU_fixtures_clone$Awinodds <-   ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
  ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
  ROU_fixtures$rou_0_5 + ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
  ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
  ROU_fixtures$rou_5_6

ROU_fixtures_clone$Awinodds <- round(1/ROU_fixtures_clone$Awinodds, digits = 3)

colnames(ROU_fixtures_clone)[15] <- "CS_1-1"
colnames(ROU_fixtures_clone)[13] <- "CS_1-0"
colnames(ROU_fixtures_clone)[14] <- "CS_0-1"
colnames(ROU_fixtures_clone)[16] <- "CS_2-0"
colnames(ROU_fixtures_clone)[17] <- "CS_0-2"
colnames(ROU_fixtures_clone)[19] <- "CS_2-1"
colnames(ROU_fixtures_clone)[20] <- "CS_1-2"

ROU_fixtures_clone$`CS_1-1` <- round(1/ROU_fixtures_clone$`CS_1-1`, digits = 3)
ROU_fixtures_clone$`CS_1-0` <- round(1/ROU_fixtures_clone$`CS_1-0`, digits = 3)
ROU_fixtures_clone$`CS_0-1` <- round(1/ROU_fixtures_clone$`CS_0-1`, digits = 3)
ROU_fixtures_clone$`CS_2-0` <- round(1/ROU_fixtures_clone$`CS_2-0`, digits = 3)
ROU_fixtures_clone$`CS_0-2` <- round(1/ROU_fixtures_clone$`CS_0-2`, digits = 3)
ROU_fixtures_clone$`CS_2-1` <- round(1/ROU_fixtures_clone$`CS_2-1`, digits = 3)
ROU_fixtures_clone$`CS_1-2` <- round(1/ROU_fixtures_clone$`CS_1-2`, digits = 3)

colnames(ROU_fixtures_clone)[1] <- "league"
colnames(ROU_fixtures_clone)[2] <- "Hometeam"
colnames(ROU_fixtures_clone)[3] <- "Awayteam"
colnames(ROU_fixtures_clone)[92] <- "predscore"
colnames(ROU_fixtures_clone)[64] <- "ov25"
colnames(ROU_fixtures_clone)[66] <- "ov25odds"
colnames(ROU_fixtures_clone)[65] <- "un25"
colnames(ROU_fixtures_clone)[67] <- "un25odds"
colnames(ROU_fixtures_clone)[68] <- "BTTSY"
colnames(ROU_fixtures_clone)[69] <- "BTTSN"
colnames(ROU_fixtures_clone)[70] <- "BTTSYodds"
colnames(ROU_fixtures_clone)[71] <- "BTTSNodds"

ROU_fixtures_clone$matchid <- paste(ROU_fixtures_clone$Hometeam,ROU_fixtures_clone$Awayteam,sep = '-')

ROU_fixtures_clone <- ROU_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#RUS
RUS_fixtures_clone <- RUS_fixtures
colnames(RUS_fixtures_clone)[61] <- "Hwin"
colnames(RUS_fixtures_clone)[62] <- "Draw"
colnames(RUS_fixtures_clone)[63] <- "Awin"

RUS_fixtures_clone$Hwinodds <-   RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
  RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
  RUS_fixtures$rus_5_0 + RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
  RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
  RUS_fixtures$rus_6_5
RUS_fixtures_clone$Hwinodds <- round(1/RUS_fixtures_clone$Hwinodds, digits = 3)

RUS_fixtures_clone$Drawodds <-  RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 + RUS_fixtures$rus_4_4 +
  RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6

RUS_fixtures_clone$Drawodds <- round(1/RUS_fixtures_clone$Drawodds, digits = 3)

RUS_fixtures_clone$Awinodds <-   RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
  RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
  RUS_fixtures$rus_0_5 + RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
  RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
  RUS_fixtures$rus_5_6

RUS_fixtures_clone$Awinodds <- round(1/RUS_fixtures_clone$Awinodds, digits = 3)

colnames(RUS_fixtures_clone)[15] <- "CS_1-1"
colnames(RUS_fixtures_clone)[13] <- "CS_1-0"
colnames(RUS_fixtures_clone)[14] <- "CS_0-1"
colnames(RUS_fixtures_clone)[16] <- "CS_2-0"
colnames(RUS_fixtures_clone)[17] <- "CS_0-2"
colnames(RUS_fixtures_clone)[19] <- "CS_2-1"
colnames(RUS_fixtures_clone)[20] <- "CS_1-2"

RUS_fixtures_clone$`CS_1-1` <- round(1/RUS_fixtures_clone$`CS_1-1`, digits = 3)
RUS_fixtures_clone$`CS_1-0` <- round(1/RUS_fixtures_clone$`CS_1-0`, digits = 3)
RUS_fixtures_clone$`CS_0-1` <- round(1/RUS_fixtures_clone$`CS_0-1`, digits = 3)
RUS_fixtures_clone$`CS_2-0` <- round(1/RUS_fixtures_clone$`CS_2-0`, digits = 3)
RUS_fixtures_clone$`CS_0-2` <- round(1/RUS_fixtures_clone$`CS_0-2`, digits = 3)
RUS_fixtures_clone$`CS_2-1` <- round(1/RUS_fixtures_clone$`CS_2-1`, digits = 3)
RUS_fixtures_clone$`CS_1-2` <- round(1/RUS_fixtures_clone$`CS_1-2`, digits = 3)

colnames(RUS_fixtures_clone)[1] <- "league"
colnames(RUS_fixtures_clone)[2] <- "Hometeam"
colnames(RUS_fixtures_clone)[3] <- "Awayteam"
colnames(RUS_fixtures_clone)[92] <- "predscore"
colnames(RUS_fixtures_clone)[64] <- "ov25"
colnames(RUS_fixtures_clone)[66] <- "ov25odds"
colnames(RUS_fixtures_clone)[65] <- "un25"
colnames(RUS_fixtures_clone)[67] <- "un25odds"
colnames(RUS_fixtures_clone)[68] <- "BTTSY"
colnames(RUS_fixtures_clone)[69] <- "BTTSN"
colnames(RUS_fixtures_clone)[70] <- "BTTSYodds"
colnames(RUS_fixtures_clone)[71] <- "BTTSNodds"

RUS_fixtures_clone$matchid <- paste(RUS_fixtures_clone$Hometeam,RUS_fixtures_clone$Awayteam,sep = '-')

RUS_fixtures_clone <- RUS_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#SWE
SWE_fixtures_clone <- SWE_fixtures
colnames(SWE_fixtures_clone)[61] <- "Hwin"
colnames(SWE_fixtures_clone)[62] <- "Draw"
colnames(SWE_fixtures_clone)[63] <- "Awin"

SWE_fixtures_clone$Hwinodds <-   SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
  SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
  SWE_fixtures$swe_5_0 + SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
  SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
  SWE_fixtures$swe_6_5
SWE_fixtures_clone$Hwinodds <- round(1/SWE_fixtures_clone$Hwinodds, digits = 3)

SWE_fixtures_clone$Drawodds <-  SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 + SWE_fixtures$swe_4_4 +
  SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6

SWE_fixtures_clone$Drawodds <- round(1/SWE_fixtures_clone$Drawodds, digits = 3)

SWE_fixtures_clone$Awinodds <-   SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
  SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
  SWE_fixtures$swe_0_5 + SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
  SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
  SWE_fixtures$swe_5_6

SWE_fixtures_clone$Awinodds <- round(1/SWE_fixtures_clone$Awinodds, digits = 3)

colnames(SWE_fixtures_clone)[15] <- "CS_1-1"
colnames(SWE_fixtures_clone)[13] <- "CS_1-0"
colnames(SWE_fixtures_clone)[14] <- "CS_0-1"
colnames(SWE_fixtures_clone)[16] <- "CS_2-0"
colnames(SWE_fixtures_clone)[17] <- "CS_0-2"
colnames(SWE_fixtures_clone)[19] <- "CS_2-1"
colnames(SWE_fixtures_clone)[20] <- "CS_1-2"

SWE_fixtures_clone$`CS_1-1` <- round(1/SWE_fixtures_clone$`CS_1-1`, digits = 3)
SWE_fixtures_clone$`CS_1-0` <- round(1/SWE_fixtures_clone$`CS_1-0`, digits = 3)
SWE_fixtures_clone$`CS_0-1` <- round(1/SWE_fixtures_clone$`CS_0-1`, digits = 3)
SWE_fixtures_clone$`CS_2-0` <- round(1/SWE_fixtures_clone$`CS_2-0`, digits = 3)
SWE_fixtures_clone$`CS_0-2` <- round(1/SWE_fixtures_clone$`CS_0-2`, digits = 3)
SWE_fixtures_clone$`CS_2-1` <- round(1/SWE_fixtures_clone$`CS_2-1`, digits = 3)
SWE_fixtures_clone$`CS_1-2` <- round(1/SWE_fixtures_clone$`CS_1-2`, digits = 3)

colnames(SWE_fixtures_clone)[1] <- "league"
colnames(SWE_fixtures_clone)[2] <- "Hometeam"
colnames(SWE_fixtures_clone)[3] <- "Awayteam"
colnames(SWE_fixtures_clone)[92] <- "predscore"
colnames(SWE_fixtures_clone)[64] <- "ov25"
colnames(SWE_fixtures_clone)[66] <- "ov25odds"
colnames(SWE_fixtures_clone)[65] <- "un25"
colnames(SWE_fixtures_clone)[67] <- "un25odds"
colnames(SWE_fixtures_clone)[68] <- "BTTSY"
colnames(SWE_fixtures_clone)[69] <- "BTTSN"
colnames(SWE_fixtures_clone)[70] <- "BTTSYodds"
colnames(SWE_fixtures_clone)[71] <- "BTTSNodds"

SWE_fixtures_clone$matchid <- paste(SWE_fixtures_clone$Hometeam,SWE_fixtures_clone$Awayteam,sep = '-')

SWE_fixtures_clone <- SWE_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#MLS
MLS_fixtures_clone <- MLS_fixtures
colnames(MLS_fixtures_clone)[61] <- "Hwin"
colnames(MLS_fixtures_clone)[62] <- "Draw"
colnames(MLS_fixtures_clone)[63] <- "Awin"

MLS_fixtures_clone$Hwinodds <-   MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
  MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
  MLS_fixtures$mls_5_0 + MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
  MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
  MLS_fixtures$mls_6_5
MLS_fixtures_clone$Hwinodds <- round(1/MLS_fixtures_clone$Hwinodds, digits = 3)

MLS_fixtures_clone$Drawodds <-  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 + MLS_fixtures$mls_4_4 +
  MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6

MLS_fixtures_clone$Drawodds <- round(1/MLS_fixtures_clone$Drawodds, digits = 3)

MLS_fixtures_clone$Awinodds <-   MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
  MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
  MLS_fixtures$mls_0_5 + MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
  MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
  MLS_fixtures$mls_5_6

MLS_fixtures_clone$Awinodds <- round(1/MLS_fixtures_clone$Awinodds, digits = 3)

colnames(MLS_fixtures_clone)[15] <- "CS_1-1"
colnames(MLS_fixtures_clone)[13] <- "CS_1-0"
colnames(MLS_fixtures_clone)[14] <- "CS_0-1"
colnames(MLS_fixtures_clone)[16] <- "CS_2-0"
colnames(MLS_fixtures_clone)[17] <- "CS_0-2"
colnames(MLS_fixtures_clone)[19] <- "CS_2-1"
colnames(MLS_fixtures_clone)[20] <- "CS_1-2"

MLS_fixtures_clone$`CS_1-1` <- round(1/MLS_fixtures_clone$`CS_1-1`, digits = 3)
MLS_fixtures_clone$`CS_1-0` <- round(1/MLS_fixtures_clone$`CS_1-0`, digits = 3)
MLS_fixtures_clone$`CS_0-1` <- round(1/MLS_fixtures_clone$`CS_0-1`, digits = 3)
MLS_fixtures_clone$`CS_2-0` <- round(1/MLS_fixtures_clone$`CS_2-0`, digits = 3)
MLS_fixtures_clone$`CS_0-2` <- round(1/MLS_fixtures_clone$`CS_0-2`, digits = 3)
MLS_fixtures_clone$`CS_2-1` <- round(1/MLS_fixtures_clone$`CS_2-1`, digits = 3)
MLS_fixtures_clone$`CS_1-2` <- round(1/MLS_fixtures_clone$`CS_1-2`, digits = 3)

colnames(MLS_fixtures_clone)[1] <- "league"
colnames(MLS_fixtures_clone)[2] <- "Hometeam"
colnames(MLS_fixtures_clone)[3] <- "Awayteam"
colnames(MLS_fixtures_clone)[92] <- "predscore"
colnames(MLS_fixtures_clone)[64] <- "ov25"
colnames(MLS_fixtures_clone)[66] <- "ov25odds"
colnames(MLS_fixtures_clone)[65] <- "un25"
colnames(MLS_fixtures_clone)[67] <- "un25odds"
colnames(MLS_fixtures_clone)[68] <- "BTTSY"
colnames(MLS_fixtures_clone)[69] <- "BTTSN"
colnames(MLS_fixtures_clone)[70] <- "BTTSYodds"
colnames(MLS_fixtures_clone)[71] <- "BTTSNodds"

MLS_fixtures_clone$matchid <- paste(MLS_fixtures_clone$Hometeam,MLS_fixtures_clone$Awayteam,sep = '-')

MLS_fixtures_clone <- MLS_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
#######################################################################################################################
#SWZ
SWZ_fixtures_clone <- SWZ_fixtures
colnames(SWZ_fixtures_clone)[61] <- "Hwin"
colnames(SWZ_fixtures_clone)[62] <- "Draw"
colnames(SWZ_fixtures_clone)[63] <- "Awin"

SWZ_fixtures_clone$Hwinodds <-   SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
  SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
  SWZ_fixtures$swz_5_0 + SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
  SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
  SWZ_fixtures$swz_6_5
SWZ_fixtures_clone$Hwinodds <- round(1/SWZ_fixtures_clone$Hwinodds, digits = 3)

SWZ_fixtures_clone$Drawodds <-  SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 + SWZ_fixtures$swz_4_4 +
  SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6

SWZ_fixtures_clone$Drawodds <- round(1/SWZ_fixtures_clone$Drawodds, digits = 3)

SWZ_fixtures_clone$Awinodds <-   SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
  SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
  SWZ_fixtures$swz_0_5 + SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
  SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
  SWZ_fixtures$swz_5_6

SWZ_fixtures_clone$Awinodds <- round(1/SWZ_fixtures_clone$Awinodds, digits = 3)

colnames(SWZ_fixtures_clone)[15] <- "CS_1-1"
colnames(SWZ_fixtures_clone)[13] <- "CS_1-0"
colnames(SWZ_fixtures_clone)[14] <- "CS_0-1"
colnames(SWZ_fixtures_clone)[16] <- "CS_2-0"
colnames(SWZ_fixtures_clone)[17] <- "CS_0-2"
colnames(SWZ_fixtures_clone)[19] <- "CS_2-1"
colnames(SWZ_fixtures_clone)[20] <- "CS_1-2"

SWZ_fixtures_clone$`CS_1-1` <- round(1/SWZ_fixtures_clone$`CS_1-1`, digits = 3)
SWZ_fixtures_clone$`CS_1-0` <- round(1/SWZ_fixtures_clone$`CS_1-0`, digits = 3)
SWZ_fixtures_clone$`CS_0-1` <- round(1/SWZ_fixtures_clone$`CS_0-1`, digits = 3)
SWZ_fixtures_clone$`CS_2-0` <- round(1/SWZ_fixtures_clone$`CS_2-0`, digits = 3)
SWZ_fixtures_clone$`CS_0-2` <- round(1/SWZ_fixtures_clone$`CS_0-2`, digits = 3)
SWZ_fixtures_clone$`CS_2-1` <- round(1/SWZ_fixtures_clone$`CS_2-1`, digits = 3)
SWZ_fixtures_clone$`CS_1-2` <- round(1/SWZ_fixtures_clone$`CS_1-2`, digits = 3)

colnames(SWZ_fixtures_clone)[1] <- "league"
colnames(SWZ_fixtures_clone)[2] <- "Hometeam"
colnames(SWZ_fixtures_clone)[3] <- "Awayteam"
colnames(SWZ_fixtures_clone)[92] <- "predscore"
colnames(SWZ_fixtures_clone)[64] <- "ov25"
colnames(SWZ_fixtures_clone)[66] <- "ov25odds"
colnames(SWZ_fixtures_clone)[65] <- "un25"
colnames(SWZ_fixtures_clone)[67] <- "un25odds"
colnames(SWZ_fixtures_clone)[68] <- "BTTSY"
colnames(SWZ_fixtures_clone)[69] <- "BTTSN"
colnames(SWZ_fixtures_clone)[70] <- "BTTSYodds"
colnames(SWZ_fixtures_clone)[71] <- "BTTSNodds"

SWZ_fixtures_clone$matchid <- paste(SWZ_fixtures_clone$Hometeam,SWZ_fixtures_clone$Awayteam,sep = '-')

SWZ_fixtures_clone <- SWZ_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
##########################################################################################################################

#######################################################################################################################
#######################################################################################################################

allteams20212022_clonefixtures_nl <- rbind(AUT_fixtures_clone,ARG_fixtures_clone,BRA_fixtures_clone,CHN_fixtures_clone,DNK_fixtures_clone,FIN_fixtures_clone,
                                        IRL_fixtures_clone,JPN_fixtures_clone,MEX_fixtures_clone,MLS_fixtures_clone,NOR_fixtures_clone,POL_fixtures_clone,ROU_fixtures_clone,
                                        RUS_fixtures_clone,SWE_fixtures_clone,SWZ_fixtures_clone)
# write.xlsx(allteams20212022_clonefixtures,'allteams20212022clonedfixtures.xlsx')
mycloned_prediction_nl <- dplyr::left_join(myodds_fixtures_nl,allteams20212022_clonefixtures_nl)
write.xlsx(mycloned_prediction_nl,'clonedprediction_nl.xlsx')


dplyr::left_join(myodds_fixtures_nl,allteams20212022_clonefixtures_nl)





















