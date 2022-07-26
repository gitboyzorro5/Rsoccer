library('plyr')
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
options(java.parameters = "-Xmx2048m")
library('xlsx')
unlink('NL/Simulations.xlsx')
#simulation of league tables
##################################################################################################################
###################################################################################################################
#AUT
AUT <- subset(AUT,Season == "2021/2022")
AUT_sim <- AUT
colnames(AUT_fixtures)
AUT_sim$matchid <- paste(AUT_sim$Home,AUT_sim$Away,sep = "-")
AUT_fixtures$matchid <- paste(AUT_fixtures$HomeTeam_aut,AUT_fixtures$AwayTeam_aut,sep = "-")
AUT_fixtures$aut_FTR <- sapply(AUT_fixtures$aut_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

AUT_fixtures$aut_gamestatus <- ifelse(AUT_fixtures$matchid %in% AUT_sim$matchid,"played","notplayed")

aut_home_wins_sim <- c()
aut_away_wins_sim <- c()
aut_home_draws_sim <- c()
aut_away_draws_sim <- c()
aut_home_loss_sim <- c()
aut_away_loss_sim <- c()



for (i_aut_wins_sim in 1:length(aut_teams))
{

  aut_home_wins_sim[i_aut_wins_sim] <- nrow(AUT_fixtures[AUT_fixtures$HomeTeam_aut == aut_teams[i_aut_wins_sim] & AUT_fixtures$aut_FTR == "H" & AUT_fixtures$aut_gamestatus =="notplayed",])
  aut_away_wins_sim[i_aut_wins_sim] <- nrow(AUT_fixtures[AUT_fixtures$AwayTeam_aut == aut_teams[i_aut_wins_sim] & AUT_fixtures$aut_FTR == "A" & AUT_fixtures$aut_gamestatus == "notplayed",])
  aut_home_draws_sim[i_aut_wins_sim] <- nrow(AUT_fixtures[AUT_fixtures$HomeTeam_aut == aut_teams[i_aut_wins_sim] & AUT_fixtures$aut_FTR == "D" & AUT_fixtures$aut_gamestatus == "notplayed",])
  aut_away_draws_sim[i_aut_wins_sim] <- nrow(AUT_fixtures[AUT_fixtures$AwayTeam_aut == aut_teams[i_aut_wins_sim] & AUT_fixtures$aut_FTR == "D" & AUT_fixtures$aut_gamestatus == "notplayed",])
  aut_home_loss_sim[i_aut_wins_sim] <- nrow(AUT_fixtures[AUT_fixtures$HomeTeam_aut == aut_teams[i_aut_wins_sim] & AUT_fixtures$aut_FTR == "A" & AUT_fixtures$aut_gamestatus == "notplayed",])
  aut_away_loss_sim[i_aut_wins_sim] <- nrow(AUT_fixtures[AUT_fixtures$AwayTeam_aut == aut_teams[i_aut_wins_sim] & AUT_fixtures$aut_FTR == "H" & AUT_fixtures$aut_gamestatus == "notplayed", ])

}

aut_total_wins_sim <- aut_home_wins_sim + aut_away_wins_sim
aut_total_draws_sim <- aut_home_draws_sim + aut_away_draws_sim
aut_total_loss_sim <- aut_home_loss_sim + aut_away_loss_sim

aut_home_games_sim <- c()
aut_away_games_sim <-c()

for (i_aut_sim in 1:length(aut_teams))
{

  aut_home_games_sim[i_aut_sim] <- nrow(AUT_fixtures[AUT_fixtures$HomeTeam_aut == aut_teams[i_aut_sim] & AUT_fixtures$aut_gamestatus == "notplayed",])
  aut_away_games_sim[i_aut_sim]  <- nrow(AUT_fixtures[AUT_fixtures$AwayTeam_aut == aut_teams[i_aut_sim] & AUT_fixtures$aut_gamestatus == "notplayed",])

}

aut_games_played_sim <- aut_home_games_sim + aut_away_games_sim

aut_league_table_sim <- cbind(aut_teams,aut_games_played_sim,aut_total_wins_sim,aut_total_draws_sim,aut_total_loss_sim)
aut_PTS_sim <- (aut_total_wins_sim*3) + (aut_total_draws_sim*1)
aut_league_table_sim <- cbind(aut_league_table_sim,aut_PTS_sim)

aut_games_played_simfinal <- aut_games_played + aut_games_played_sim
aut_total_wins_simfinal <- aut_total_wins + aut_total_wins_sim
aut_total_draws_simfinal <- aut_total_draws + aut_total_draws_sim
aut_total_loss_simfinal <- aut_total_loss + aut_total_loss_sim
aut_PTS_simfinal <- aut_PTS + aut_PTS_sim

aut_league_table_simfinal <- cbind(aut_teams,aut_games_played_simfinal,aut_total_wins_simfinal,aut_total_draws_simfinal,aut_total_loss_simfinal,aut_PTS_simfinal)
aut_league_table_simfinal <- as.data.frame(aut_league_table_simfinal)
names(aut_league_table_simfinal)[names(aut_league_table_simfinal) == "aut_teams"] <- "Team_f"
names(aut_league_table_simfinal)[names(aut_league_table_simfinal) == "aut_games_played_simfinal"] <- "P_f"
names(aut_league_table_simfinal)[names(aut_league_table_simfinal) == "aut_total_wins_simfinal"] <- "W_f"
names(aut_league_table_simfinal)[names(aut_league_table_simfinal) == "aut_total_draws_simfinal"] <- "D_f"
names(aut_league_table_simfinal)[names(aut_league_table_simfinal) == "aut_total_loss_simfinal"] <- "L_f"
names(aut_league_table_simfinal)[names(aut_league_table_simfinal) == "aut_PTS_simfinal"] <- "PTS_f"
points_aut_sim <-  aut_league_table_simfinal[order(as.numeric(aut_league_table_simfinal$PTS_f), decreasing = TRUE),]

AUT_notplayed <- AUT_fixtures[AUT_fixtures$aut_gamestatus == "notplayed",]

write.xlsx(points_aut,'NL/Simulations.xlsx', sheetName = "AUT_Table",append = TRUE)
write.xlsx(aut_league_table_sim,'NL/Simulations.xlsx', sheetName = "AUT_sim",append = TRUE)
write.xlsx(points_aut_sim,'NL/Simulations.xlsx', sheetName = "AUT_simfinal",append = TRUE)
write.xlsx(AUT_notplayed,'NL/Simulations.xlsx', sheetName = "AUT_notplayed",append = TRUE)
############################################################################################################################################################
############################################################################################################################################################
#ARG
ARG <- subset(ARG,Season == "2022")
ARG_sim <- ARG
ARG_sim$matchid <- paste(ARG_sim$Home,ARG_sim$Away,sep = "-")
ARG_fixtures$matchid <- paste(ARG_fixtures$HomeTeam_arg,ARG_fixtures$AwayTeam_arg,sep = "-")
ARG_fixtures$arg_FTR <- sapply(ARG_fixtures$arg_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

ARG_fixtures$arg_gamestatus <- ifelse(ARG_fixtures$matchid %in% ARG_sim$matchid,"played","notplayed")

arg_home_wins_sim <- c()
arg_away_wins_sim <- c()
arg_home_draws_sim <- c()
arg_away_draws_sim <- c()
arg_home_loss_sim <- c()
arg_away_loss_sim <- c()



for (i_arg_wins_sim in 1:length(arg_teams))
{

  arg_home_wins_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "H" & ARG_fixtures$arg_gamestatus =="notplayed",])
  arg_away_wins_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "A" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_home_draws_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "D" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_away_draws_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "D" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_home_loss_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "A" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_away_loss_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "H" & ARG_fixtures$arg_gamestatus == "notplayed", ])

}

arg_total_wins_sim <- arg_home_wins_sim + arg_away_wins_sim
arg_total_draws_sim <- arg_home_draws_sim + arg_away_draws_sim
arg_total_loss_sim <- arg_home_loss_sim + arg_away_loss_sim

arg_home_games_sim <- c()
arg_away_games_sim <-c()

for (i_arg_sim in 1:length(arg_teams))
{

  arg_home_games_sim[i_arg_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_sim] & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_away_games_sim[i_arg_sim]  <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_sim] & ARG_fixtures$arg_gamestatus == "notplayed",])

}

arg_games_played_sim <- arg_home_games_sim + arg_away_games_sim

arg_league_table_sim <- cbind(arg_teams,arg_games_played_sim,arg_total_wins_sim,arg_total_draws_sim,arg_total_loss_sim)
arg_PTS_sim <- (arg_total_wins_sim*3) + (arg_total_draws_sim*1)
arg_league_table_sim <- cbind(arg_league_table_sim,arg_PTS_sim)

arg_games_played_simfinal <- arg_games_played + arg_games_played_sim
arg_total_wins_simfinal <- arg_total_wins + arg_total_wins_sim
arg_total_draws_simfinal <- arg_total_draws + arg_total_draws_sim
arg_total_loss_simfinal <- arg_total_loss + arg_total_loss_sim
arg_PTS_simfinal <- arg_PTS + arg_PTS_sim

arg_league_table_simfinal <- cbind(arg_teams,arg_games_played_simfinal,arg_total_wins_simfinal,arg_total_draws_simfinal,arg_total_loss_simfinal,arg_PTS_simfinal)
arg_league_table_simfinal <- as.data.frame(arg_league_table_simfinal)
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_teams"] <- "Team_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_games_played_simfinal"] <- "P_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_total_wins_simfinal"] <- "W_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_total_draws_simfinal"] <- "D_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_total_loss_simfinal"] <- "L_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_PTS_simfinal"] <- "PTS_f"
points_arg_sim <-  arg_league_table_simfinal[order(as.numeric(arg_league_table_simfinal$PTS_f), decreasing = TRUE),]

ARG_notplayed <- ARG_fixtures[ARG_fixtures$arg_gamestatus == "notplayed",]

write.xlsx(points_arg,'NL/Simulations.xlsx', sheetName = "ARG_table",append = TRUE)
write.xlsx(arg_league_table_sim,'NL/Simulations.xlsx', sheetName = "ARG_sim",append = TRUE)
write.xlsx(points_arg_sim,'NL/Simulations.xlsx', sheetName = "ARG_simfinal",append = TRUE)
write.xlsx(ARG_notplayed,'NL/Simulations.xlsx', sheetName = "ARG_notplayed",append = TRUE)
############################################################################################################################################################
############################################################################################################################################################
#BRA
BRA <- subset(BRA,Season == "2022")
BRA_sim <- BRA
BRA_sim$matchid <- paste(BRA_sim$Home,BRA_sim$Away,sep = "-")
BRA_fixtures$matchid <- paste(BRA_fixtures$HomeTeam_bra,BRA_fixtures$AwayTeam_bra,sep = "-")
BRA_fixtures$bra_FTR <- sapply(BRA_fixtures$bra_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

BRA_fixtures$bra_gamestatus <- ifelse(BRA_fixtures$matchid %in% BRA_sim$matchid,"played","notplayed")

bra_home_wins_sim <- c()
bra_away_wins_sim <- c()
bra_home_draws_sim <- c()
bra_away_draws_sim <- c()
bra_home_loss_sim <- c()
bra_away_loss_sim <- c()



for (i_bra_wins_sim in 1:length(bra_teams))
{

  bra_home_wins_sim[i_bra_wins_sim] <- nrow(BRA_fixtures[BRA_fixtures$HomeTeam_bra == bra_teams[i_bra_wins_sim] & BRA_fixtures$bra_FTR == "H" & BRA_fixtures$bra_gamestatus =="notplayed",])
  bra_away_wins_sim[i_bra_wins_sim] <- nrow(BRA_fixtures[BRA_fixtures$AwayTeam_bra == bra_teams[i_bra_wins_sim] & BRA_fixtures$bra_FTR == "A" & BRA_fixtures$bra_gamestatus == "notplayed",])
  bra_home_draws_sim[i_bra_wins_sim] <- nrow(BRA_fixtures[BRA_fixtures$HomeTeam_bra == bra_teams[i_bra_wins_sim] & BRA_fixtures$bra_FTR == "D" & BRA_fixtures$bra_gamestatus == "notplayed",])
  bra_away_draws_sim[i_bra_wins_sim] <- nrow(BRA_fixtures[BRA_fixtures$AwayTeam_bra == bra_teams[i_bra_wins_sim] & BRA_fixtures$bra_FTR == "D" & BRA_fixtures$bra_gamestatus == "notplayed",])
  bra_home_loss_sim[i_bra_wins_sim] <- nrow(BRA_fixtures[BRA_fixtures$HomeTeam_bra == bra_teams[i_bra_wins_sim] & BRA_fixtures$bra_FTR == "A" & BRA_fixtures$bra_gamestatus == "notplayed",])
  bra_away_loss_sim[i_bra_wins_sim] <- nrow(BRA_fixtures[BRA_fixtures$AwayTeam_bra == bra_teams[i_bra_wins_sim] & BRA_fixtures$bra_FTR == "H" & BRA_fixtures$bra_gamestatus == "notplayed", ])

}

bra_total_wins_sim <- bra_home_wins_sim + bra_away_wins_sim
bra_total_draws_sim <- bra_home_draws_sim + bra_away_draws_sim
bra_total_loss_sim <- bra_home_loss_sim + bra_away_loss_sim

bra_home_games_sim <- c()
bra_away_games_sim <-c()

for (i_bra_sim in 1:length(bra_teams))
{

  bra_home_games_sim[i_bra_sim] <- nrow(BRA_fixtures[BRA_fixtures$HomeTeam_bra == bra_teams[i_bra_sim] & BRA_fixtures$bra_gamestatus == "notplayed",])
  bra_away_games_sim[i_bra_sim]  <- nrow(BRA_fixtures[BRA_fixtures$AwayTeam_bra == bra_teams[i_bra_sim] & BRA_fixtures$bra_gamestatus == "notplayed",])

}

bra_games_played_sim <- bra_home_games_sim + bra_away_games_sim

bra_league_table_sim <- cbind(bra_teams,bra_games_played_sim,bra_total_wins_sim,bra_total_draws_sim,bra_total_loss_sim)
bra_PTS_sim <- (bra_total_wins_sim*3) + (bra_total_draws_sim*1)
bra_league_table_sim <- cbind(bra_league_table_sim,bra_PTS_sim)

bra_games_played_simfinal <- bra_games_played + bra_games_played_sim
bra_total_wins_simfinal <- bra_total_wins + bra_total_wins_sim
bra_total_draws_simfinal <- bra_total_draws + bra_total_draws_sim
bra_total_loss_simfinal <- bra_total_loss + bra_total_loss_sim
bra_PTS_simfinal <- bra_PTS + bra_PTS_sim

bra_league_table_simfinal <- cbind(bra_teams,bra_games_played_simfinal,bra_total_wins_simfinal,bra_total_draws_simfinal,bra_total_loss_simfinal,bra_PTS_simfinal)
bra_league_table_simfinal <- as.data.frame(bra_league_table_simfinal)
names(bra_league_table_simfinal)[names(bra_league_table_simfinal) == "bra_teams"] <- "Team_f"
names(bra_league_table_simfinal)[names(bra_league_table_simfinal) == "bra_games_played_simfinal"] <- "P_f"
names(bra_league_table_simfinal)[names(bra_league_table_simfinal) == "bra_total_wins_simfinal"] <- "W_f"
names(bra_league_table_simfinal)[names(bra_league_table_simfinal) == "bra_total_draws_simfinal"] <- "D_f"
names(bra_league_table_simfinal)[names(bra_league_table_simfinal) == "bra_total_loss_simfinal"] <- "L_f"
names(bra_league_table_simfinal)[names(bra_league_table_simfinal) == "bra_PTS_simfinal"] <- "PTS_f"
points_bra_sim <-  bra_league_table_simfinal[order(as.numeric(bra_league_table_simfinal$PTS_f), decreasing = TRUE),]

BRA_notplayed <- BRA_fixtures[BRA_fixtures$bra_gamestatus == "notplayed",]


write.xlsx(points_bra,'NL/Simulations.xlsx', sheetName = "BRA_table",append = TRUE)
write.xlsx(bra_league_table_sim,'NL/Simulations.xlsx', sheetName = "BRA_sim",append = TRUE)
write.xlsx(points_bra_sim,'NL/Simulations.xlsx', sheetName = "BRA_simfinal",append = TRUE)
write.xlsx(BRA_notplayed,'NL/Simulations.xlsx', sheetName = "BRA_notplayed",append = TRUE)
############################################################################################################################
###########################################################################################################################
#CHN
CHN <- subset(CHN,Season == "2022")
CHN_sim <- CHN
CHN_sim$matchid <- paste(CHN_sim$Home,CHN_sim$Away,sep = "-")
CHN_fixtures$matchid <- paste(CHN_fixtures$HomeTeam_chn,CHN_fixtures$AwayTeam_chn,sep = "-")
CHN_fixtures$chn_FTR <- sapply(CHN_fixtures$chn_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

CHN_fixtures$chn_gamestatus <- ifelse(CHN_fixtures$matchid %in% CHN_sim$matchid,"played","notplayed")

chn_home_wins_sim <- c()
chn_away_wins_sim <- c()
chn_home_draws_sim <- c()
chn_away_draws_sim <- c()
chn_home_loss_sim <- c()
chn_away_loss_sim <- c()



for (i_chn_wins_sim in 1:length(chn_teams))
{

  chn_home_wins_sim[i_chn_wins_sim] <- nrow(CHN_fixtures[CHN_fixtures$HomeTeam_chn == chn_teams[i_chn_wins_sim] & CHN_fixtures$chn_FTR == "H" & CHN_fixtures$chn_gamestatus =="notplayed",])
  chn_away_wins_sim[i_chn_wins_sim] <- nrow(CHN_fixtures[CHN_fixtures$AwayTeam_chn == chn_teams[i_chn_wins_sim] & CHN_fixtures$chn_FTR == "A" & CHN_fixtures$chn_gamestatus == "notplayed",])
  chn_home_draws_sim[i_chn_wins_sim] <- nrow(CHN_fixtures[CHN_fixtures$HomeTeam_chn == chn_teams[i_chn_wins_sim] & CHN_fixtures$chn_FTR == "D" & CHN_fixtures$chn_gamestatus == "notplayed",])
  chn_away_draws_sim[i_chn_wins_sim] <- nrow(CHN_fixtures[CHN_fixtures$AwayTeam_chn == chn_teams[i_chn_wins_sim] & CHN_fixtures$chn_FTR == "D" & CHN_fixtures$chn_gamestatus == "notplayed",])
  chn_home_loss_sim[i_chn_wins_sim] <- nrow(CHN_fixtures[CHN_fixtures$HomeTeam_chn == chn_teams[i_chn_wins_sim] & CHN_fixtures$chn_FTR == "A" & CHN_fixtures$chn_gamestatus == "notplayed",])
  chn_away_loss_sim[i_chn_wins_sim] <- nrow(CHN_fixtures[CHN_fixtures$AwayTeam_chn == chn_teams[i_chn_wins_sim] & CHN_fixtures$chn_FTR == "H" & CHN_fixtures$chn_gamestatus == "notplayed", ])

}

chn_total_wins_sim <- chn_home_wins_sim + chn_away_wins_sim
chn_total_draws_sim <- chn_home_draws_sim + chn_away_draws_sim
chn_total_loss_sim <- chn_home_loss_sim + chn_away_loss_sim

chn_home_games_sim <- c()
chn_away_games_sim <-c()

for (i_chn_sim in 1:length(chn_teams))
{

  chn_home_games_sim[i_chn_sim] <- nrow(CHN_fixtures[CHN_fixtures$HomeTeam_chn == chn_teams[i_chn_sim] & CHN_fixtures$chn_gamestatus == "notplayed",])
  chn_away_games_sim[i_chn_sim]  <- nrow(CHN_fixtures[CHN_fixtures$AwayTeam_chn == chn_teams[i_chn_sim] & CHN_fixtures$chn_gamestatus == "notplayed",])

}

chn_games_played_sim <- chn_home_games_sim + chn_away_games_sim

chn_league_table_sim <- cbind(chn_teams,chn_games_played_sim,chn_total_wins_sim,chn_total_draws_sim,chn_total_loss_sim)
chn_PTS_sim <- (chn_total_wins_sim*3) + (chn_total_draws_sim*1)
chn_league_table_sim <- cbind(chn_league_table_sim,chn_PTS_sim)

chn_games_played_simfinal <- chn_games_played + chn_games_played_sim
chn_total_wins_simfinal <- chn_total_wins + chn_total_wins_sim
chn_total_draws_simfinal <- chn_total_draws + chn_total_draws_sim
chn_total_loss_simfinal <- chn_total_loss + chn_total_loss_sim
chn_PTS_simfinal <- chn_PTS + chn_PTS_sim

chn_league_table_simfinal <- cbind(chn_teams,chn_games_played_simfinal,chn_total_wins_simfinal,chn_total_draws_simfinal,chn_total_loss_simfinal,chn_PTS_simfinal)
chn_league_table_simfinal <- as.data.frame(chn_league_table_simfinal)
names(chn_league_table_simfinal)[names(chn_league_table_simfinal) == "chn_teams"] <- "Team_f"
names(chn_league_table_simfinal)[names(chn_league_table_simfinal) == "chn_games_played_simfinal"] <- "P_f"
names(chn_league_table_simfinal)[names(chn_league_table_simfinal) == "chn_total_wins_simfinal"] <- "W_f"
names(chn_league_table_simfinal)[names(chn_league_table_simfinal) == "chn_total_draws_simfinal"] <- "D_f"
names(chn_league_table_simfinal)[names(chn_league_table_simfinal) == "chn_total_loss_simfinal"] <- "L_f"
names(chn_league_table_simfinal)[names(chn_league_table_simfinal) == "chn_PTS_simfinal"] <- "PTS_f"
points_chn_sim <-  chn_league_table_simfinal[order(as.numeric(chn_league_table_simfinal$PTS_f), decreasing = TRUE),]

CHN_notplayed <- CHN_fixtures[CHN_fixtures$chn_gamestatus == "notplayed",]


write.xlsx(points_chn,'NL/Simulations.xlsx', sheetName = "CHN_table",append = TRUE)
write.xlsx(chn_league_table_sim,'NL/Simulations.xlsx', sheetName = "CHN_sim",append = TRUE)
write.xlsx(points_chn_sim,'NL/Simulations.xlsx', sheetName = "CHN_simfinal",append = TRUE)
write.xlsx(CHN_notplayed,'NL/Simulations.xlsx', sheetName = "CHN_notplayed",append = TRUE)
#########################################################################################################################################
#########################################################################################################################################
###################################################################################################################
#DNK
DNK <- subset(DNK,Season == "2021/2022")
DNK_sim <- DNK
DNK_sim$matchid <- paste(DNK_sim$Home,DNK_sim$Away,sep = "-")
DNK_fixtures$matchid <- paste(DNK_fixtures$HomeTeam_dnk,DNK_fixtures$AwayTeam_dnk,sep = "-")
DNK_fixtures$dnk_FTR <- sapply(DNK_fixtures$dnk_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

DNK_fixtures$dnk_gamestatus <- ifelse(DNK_fixtures$matchid %in% DNK_sim$matchid,"played","notplayed")

dnk_home_wins_sim <- c()
dnk_away_wins_sim <- c()
dnk_home_draws_sim <- c()
dnk_away_draws_sim <- c()
dnk_home_loss_sim <- c()
dnk_away_loss_sim <- c()



for (i_dnk_wins_sim in 1:length(dnk_teams))
{

  dnk_home_wins_sim[i_dnk_wins_sim] <- nrow(DNK_fixtures[DNK_fixtures$HomeTeam_dnk == dnk_teams[i_dnk_wins_sim] & DNK_fixtures$dnk_FTR == "H" & DNK_fixtures$dnk_gamestatus =="notplayed",])
  dnk_away_wins_sim[i_dnk_wins_sim] <- nrow(DNK_fixtures[DNK_fixtures$AwayTeam_dnk == dnk_teams[i_dnk_wins_sim] & DNK_fixtures$dnk_FTR == "A" & DNK_fixtures$dnk_gamestatus == "notplayed",])
  dnk_home_draws_sim[i_dnk_wins_sim] <- nrow(DNK_fixtures[DNK_fixtures$HomeTeam_dnk == dnk_teams[i_dnk_wins_sim] & DNK_fixtures$dnk_FTR == "D" & DNK_fixtures$dnk_gamestatus == "notplayed",])
  dnk_away_draws_sim[i_dnk_wins_sim] <- nrow(DNK_fixtures[DNK_fixtures$AwayTeam_dnk == dnk_teams[i_dnk_wins_sim] & DNK_fixtures$dnk_FTR == "D" & DNK_fixtures$dnk_gamestatus == "notplayed",])
  dnk_home_loss_sim[i_dnk_wins_sim] <- nrow(DNK_fixtures[DNK_fixtures$HomeTeam_dnk == dnk_teams[i_dnk_wins_sim] & DNK_fixtures$dnk_FTR == "A" & DNK_fixtures$dnk_gamestatus == "notplayed",])
  dnk_away_loss_sim[i_dnk_wins_sim] <- nrow(DNK_fixtures[DNK_fixtures$AwayTeam_dnk == dnk_teams[i_dnk_wins_sim] & DNK_fixtures$dnk_FTR == "H" & DNK_fixtures$dnk_gamestatus == "notplayed", ])

}

dnk_total_wins_sim <- dnk_home_wins_sim + dnk_away_wins_sim
dnk_total_draws_sim <- dnk_home_draws_sim + dnk_away_draws_sim
dnk_total_loss_sim <- dnk_home_loss_sim + dnk_away_loss_sim

dnk_home_games_sim <- c()
dnk_away_games_sim <-c()

for (i_dnk_sim in 1:length(dnk_teams))
{

  dnk_home_games_sim[i_dnk_sim] <- nrow(DNK_fixtures[DNK_fixtures$HomeTeam_dnk == dnk_teams[i_dnk_sim] & DNK_fixtures$dnk_gamestatus == "notplayed",])
  dnk_away_games_sim[i_dnk_sim]  <- nrow(DNK_fixtures[DNK_fixtures$AwayTeam_dnk == dnk_teams[i_dnk_sim] & DNK_fixtures$dnk_gamestatus == "notplayed",])

}

dnk_games_played_sim <- dnk_home_games_sim + dnk_away_games_sim

dnk_league_table_sim <- cbind(dnk_teams,dnk_games_played_sim,dnk_total_wins_sim,dnk_total_draws_sim,dnk_total_loss_sim)
dnk_PTS_sim <- (dnk_total_wins_sim*3) + (dnk_total_draws_sim*1)
dnk_league_table_sim <- cbind(dnk_league_table_sim,dnk_PTS_sim)

dnk_games_played_simfinal <- dnk_games_played + dnk_games_played_sim
dnk_total_wins_simfinal <- dnk_total_wins + dnk_total_wins_sim
dnk_total_draws_simfinal <- dnk_total_draws + dnk_total_draws_sim
dnk_total_loss_simfinal <- dnk_total_loss + dnk_total_loss_sim
dnk_PTS_simfinal <- dnk_PTS + dnk_PTS_sim

dnk_league_table_simfinal <- cbind(dnk_teams,dnk_games_played_simfinal,dnk_total_wins_simfinal,dnk_total_draws_simfinal,dnk_total_loss_simfinal,dnk_PTS_simfinal)
dnk_league_table_simfinal <- as.data.frame(dnk_league_table_simfinal)
names(dnk_league_table_simfinal)[names(dnk_league_table_simfinal) == "dnk_teams"] <- "Team_f"
names(dnk_league_table_simfinal)[names(dnk_league_table_simfinal) == "dnk_games_played_simfinal"] <- "P_f"
names(dnk_league_table_simfinal)[names(dnk_league_table_simfinal) == "dnk_total_wins_simfinal"] <- "W_f"
names(dnk_league_table_simfinal)[names(dnk_league_table_simfinal) == "dnk_total_draws_simfinal"] <- "D_f"
names(dnk_league_table_simfinal)[names(dnk_league_table_simfinal) == "dnk_total_loss_simfinal"] <- "L_f"
names(dnk_league_table_simfinal)[names(dnk_league_table_simfinal) == "dnk_PTS_simfinal"] <- "PTS_f"
points_dnk_sim <-  dnk_league_table_simfinal[order(as.numeric(dnk_league_table_simfinal$PTS_f), decreasing = TRUE),]

DNK_notplayed <- DNK_fixtures[DNK_fixtures$dnk_gamestatus == "notplayed",]


write.xlsx(points_dnk,'NL/Simulations.xlsx', sheetName = "DNK_table",append = TRUE)
write.xlsx(dnk_league_table_sim,'NL/Simulations.xlsx', sheetName = "DNK_sim",append = TRUE)
write.xlsx(points_dnk_sim,'NL/Simulations.xlsx', sheetName = "DNK_simfinal",append = TRUE)
write.xlsx(DNK_notplayed,'NL/Simulations.xlsx', sheetName = "DNK_notplayed",append = TRUE)
###############################################################################################################################
###############################################################################################################################
#FIN
FIN <- subset(FIN,Season == "2022")
FIN_sim <- FIN
FIN_sim$matchid <- paste(FIN_sim$Home,FIN_sim$Away,sep = "-")
FIN_fixtures$matchid <- paste(FIN_fixtures$HomeTeam_fin,FIN_fixtures$AwayTeam_fin,sep = "-")
FIN_fixtures$fin_FTR <- sapply(FIN_fixtures$fin_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

FIN_fixtures$fin_gamestatus <- ifelse(FIN_fixtures$matchid %in% FIN_sim$matchid,"played","notplayed")

fin_home_wins_sim <- c()
fin_away_wins_sim <- c()
fin_home_draws_sim <- c()
fin_away_draws_sim <- c()
fin_home_loss_sim <- c()
fin_away_loss_sim <- c()



for (i_fin_wins_sim in 1:length(fin_teams))
{

  fin_home_wins_sim[i_fin_wins_sim] <- nrow(FIN_fixtures[FIN_fixtures$HomeTeam_fin == fin_teams[i_fin_wins_sim] & FIN_fixtures$fin_FTR == "H" & FIN_fixtures$fin_gamestatus =="notplayed",])
  fin_away_wins_sim[i_fin_wins_sim] <- nrow(FIN_fixtures[FIN_fixtures$AwayTeam_fin == fin_teams[i_fin_wins_sim] & FIN_fixtures$fin_FTR == "A" & FIN_fixtures$fin_gamestatus == "notplayed",])
  fin_home_draws_sim[i_fin_wins_sim] <- nrow(FIN_fixtures[FIN_fixtures$HomeTeam_fin == fin_teams[i_fin_wins_sim] & FIN_fixtures$fin_FTR == "D" & FIN_fixtures$fin_gamestatus == "notplayed",])
  fin_away_draws_sim[i_fin_wins_sim] <- nrow(FIN_fixtures[FIN_fixtures$AwayTeam_fin == fin_teams[i_fin_wins_sim] & FIN_fixtures$fin_FTR == "D" & FIN_fixtures$fin_gamestatus == "notplayed",])
  fin_home_loss_sim[i_fin_wins_sim] <- nrow(FIN_fixtures[FIN_fixtures$HomeTeam_fin == fin_teams[i_fin_wins_sim] & FIN_fixtures$fin_FTR == "A" & FIN_fixtures$fin_gamestatus == "notplayed",])
  fin_away_loss_sim[i_fin_wins_sim] <- nrow(FIN_fixtures[FIN_fixtures$AwayTeam_fin == fin_teams[i_fin_wins_sim] & FIN_fixtures$fin_FTR == "H" & FIN_fixtures$fin_gamestatus == "notplayed", ])

}

fin_total_wins_sim <- fin_home_wins_sim + fin_away_wins_sim
fin_total_draws_sim <- fin_home_draws_sim + fin_away_draws_sim
fin_total_loss_sim <- fin_home_loss_sim + fin_away_loss_sim

fin_home_games_sim <- c()
fin_away_games_sim <-c()

for (i_fin_sim in 1:length(fin_teams))
{

  fin_home_games_sim[i_fin_sim] <- nrow(FIN_fixtures[FIN_fixtures$HomeTeam_fin == fin_teams[i_fin_sim] & FIN_fixtures$fin_gamestatus == "notplayed",])
  fin_away_games_sim[i_fin_sim]  <- nrow(FIN_fixtures[FIN_fixtures$AwayTeam_fin == fin_teams[i_fin_sim] & FIN_fixtures$fin_gamestatus == "notplayed",])

}

fin_games_played_sim <- fin_home_games_sim + fin_away_games_sim

fin_league_table_sim <- cbind(fin_teams,fin_games_played_sim,fin_total_wins_sim,fin_total_draws_sim,fin_total_loss_sim)
fin_PTS_sim <- (fin_total_wins_sim*3) + (fin_total_draws_sim*1)
fin_league_table_sim <- cbind(fin_league_table_sim,fin_PTS_sim)

fin_games_played_simfinal <- fin_games_played + fin_games_played_sim
fin_total_wins_simfinal <- fin_total_wins + fin_total_wins_sim
fin_total_draws_simfinal <- fin_total_draws + fin_total_draws_sim
fin_total_loss_simfinal <- fin_total_loss + fin_total_loss_sim
fin_PTS_simfinal <- fin_PTS + fin_PTS_sim

fin_league_table_simfinal <- cbind(fin_teams,fin_games_played_simfinal,fin_total_wins_simfinal,fin_total_draws_simfinal,fin_total_loss_simfinal,fin_PTS_simfinal)
fin_league_table_simfinal <- as.data.frame(fin_league_table_simfinal)
names(fin_league_table_simfinal)[names(fin_league_table_simfinal) == "fin_teams"] <- "Team_f"
names(fin_league_table_simfinal)[names(fin_league_table_simfinal) == "fin_games_played_simfinal"] <- "P_f"
names(fin_league_table_simfinal)[names(fin_league_table_simfinal) == "fin_total_wins_simfinal"] <- "W_f"
names(fin_league_table_simfinal)[names(fin_league_table_simfinal) == "fin_total_draws_simfinal"] <- "D_f"
names(fin_league_table_simfinal)[names(fin_league_table_simfinal) == "fin_total_loss_simfinal"] <- "L_f"
names(fin_league_table_simfinal)[names(fin_league_table_simfinal) == "fin_PTS_simfinal"] <- "PTS_f"
points_fin_sim <-  fin_league_table_simfinal[order(as.numeric(fin_league_table_simfinal$PTS_f), decreasing = TRUE),]

FIN_notplayed <- FIN_fixtures[FIN_fixtures$fin_gamestatus == "notplayed",]
FIN_notplayed
write.xlsx(points_fin,'NL/Simulations.xlsx', sheetName = "FIN_table",append = TRUE)
write.xlsx(fin_league_table_sim,'NL/Simulations.xlsx', sheetName = "FIN_sim",append = TRUE)
write.xlsx(points_fin_sim,'NL/Simulations.xlsx', sheetName = "FIN_simfinal",append = TRUE)
write.xlsx(FIN_notplayed,'NL/Simulations.xlsx', sheetName = "FIN_notplayed",append = TRUE)

###################################################################################################################################
###################################################################################################################
#IRL
IRL <- subset(IRL,Season == "2022")
IRL_sim <- IRL
IRL_sim$matchid <- paste(IRL_sim$Home,IRL_sim$Away,sep = "-")
IRL_fixtures$matchid <- paste(IRL_fixtures$HomeTeam_irl,IRL_fixtures$AwayTeam_irl,sep = "-")
IRL_fixtures$irl_FTR <- sapply(IRL_fixtures$irl_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

IRL_fixtures$irl_gamestatus <- ifelse(IRL_fixtures$matchid %in% IRL_sim$matchid,"played","notplayed")

irl_home_wins_sim <- c()
irl_away_wins_sim <- c()
irl_home_draws_sim <- c()
irl_away_draws_sim <- c()
irl_home_loss_sim <- c()
irl_away_loss_sim <- c()



for (i_irl_wins_sim in 1:length(irl_teams))
{

  irl_home_wins_sim[i_irl_wins_sim] <- nrow(IRL_fixtures[IRL_fixtures$HomeTeam_irl == irl_teams[i_irl_wins_sim] & IRL_fixtures$irl_FTR == "H" & IRL_fixtures$irl_gamestatus =="notplayed",])
  irl_away_wins_sim[i_irl_wins_sim] <- nrow(IRL_fixtures[IRL_fixtures$AwayTeam_irl == irl_teams[i_irl_wins_sim] & IRL_fixtures$irl_FTR == "A" & IRL_fixtures$irl_gamestatus == "notplayed",])
  irl_home_draws_sim[i_irl_wins_sim] <- nrow(IRL_fixtures[IRL_fixtures$HomeTeam_irl == irl_teams[i_irl_wins_sim] & IRL_fixtures$irl_FTR == "D" & IRL_fixtures$irl_gamestatus == "notplayed",])
  irl_away_draws_sim[i_irl_wins_sim] <- nrow(IRL_fixtures[IRL_fixtures$AwayTeam_irl == irl_teams[i_irl_wins_sim] & IRL_fixtures$irl_FTR == "D" & IRL_fixtures$irl_gamestatus == "notplayed",])
  irl_home_loss_sim[i_irl_wins_sim] <- nrow(IRL_fixtures[IRL_fixtures$HomeTeam_irl == irl_teams[i_irl_wins_sim] & IRL_fixtures$irl_FTR == "A" & IRL_fixtures$irl_gamestatus == "notplayed",])
  irl_away_loss_sim[i_irl_wins_sim] <- nrow(IRL_fixtures[IRL_fixtures$AwayTeam_irl == irl_teams[i_irl_wins_sim] & IRL_fixtures$irl_FTR == "H" & IRL_fixtures$irl_gamestatus == "notplayed", ])

}

irl_total_wins_sim <- irl_home_wins_sim + irl_away_wins_sim
irl_total_draws_sim <- irl_home_draws_sim + irl_away_draws_sim
irl_total_loss_sim <- irl_home_loss_sim + irl_away_loss_sim

irl_home_games_sim <- c()
irl_away_games_sim <-c()

for (i_irl_sim in 1:length(irl_teams))
{

  irl_home_games_sim[i_irl_sim] <- nrow(IRL_fixtures[IRL_fixtures$HomeTeam_irl == irl_teams[i_irl_sim] & IRL_fixtures$irl_gamestatus == "notplayed",])
  irl_away_games_sim[i_irl_sim]  <- nrow(IRL_fixtures[IRL_fixtures$AwayTeam_irl == irl_teams[i_irl_sim] & IRL_fixtures$irl_gamestatus == "notplayed",])

}

irl_games_played_sim <- irl_home_games_sim + irl_away_games_sim

irl_league_table_sim <- cbind(irl_teams,irl_games_played_sim,irl_total_wins_sim,irl_total_draws_sim,irl_total_loss_sim)
irl_PTS_sim <- (irl_total_wins_sim*3) + (irl_total_draws_sim*1)
irl_league_table_sim <- cbind(irl_league_table_sim,irl_PTS_sim)

irl_games_played_simfinal <- irl_games_played + irl_games_played_sim
irl_total_wins_simfinal <- irl_total_wins + irl_total_wins_sim
irl_total_draws_simfinal <- irl_total_draws + irl_total_draws_sim
irl_total_loss_simfinal <- irl_total_loss + irl_total_loss_sim
irl_PTS_simfinal <- irl_PTS + irl_PTS_sim

irl_league_table_simfinal <- cbind(irl_teams,irl_games_played_simfinal,irl_total_wins_simfinal,irl_total_draws_simfinal,irl_total_loss_simfinal,irl_PTS_simfinal)
irl_league_table_simfinal <- as.data.frame(irl_league_table_simfinal)
names(irl_league_table_simfinal)[names(irl_league_table_simfinal) == "irl_teams"] <- "Team_f"
names(irl_league_table_simfinal)[names(irl_league_table_simfinal) == "irl_games_played_simfinal"] <- "P_f"
names(irl_league_table_simfinal)[names(irl_league_table_simfinal) == "irl_total_wins_simfinal"] <- "W_f"
names(irl_league_table_simfinal)[names(irl_league_table_simfinal) == "irl_total_draws_simfinal"] <- "D_f"
names(irl_league_table_simfinal)[names(irl_league_table_simfinal) == "irl_total_loss_simfinal"] <- "L_f"
names(irl_league_table_simfinal)[names(irl_league_table_simfinal) == "irl_PTS_simfinal"] <- "PTS_f"
points_irl_sim <-  irl_league_table_simfinal[order(as.numeric(irl_league_table_simfinal$PTS_f), decreasing = TRUE),]

IRL_notplayed <- IRL_fixtures[IRL_fixtures$irl_gamestatus == "notplayed",]

write.xlsx(points_irl,'NL/Simulations.xlsx', sheetName = "IRL_table",append = TRUE)
write.xlsx(irl_league_table_sim,'NL/Simulations.xlsx', sheetName = "IRL_sim",append = TRUE)
write.xlsx(points_irl_sim,'NL/Simulations.xlsx', sheetName = "IRL_simfinal",append = TRUE)
write.xlsx(IRL_notplayed,'NL/Simulations.xlsx', sheetName = "IRL_notplayed",append = TRUE)
###############################################################################################################################
###############################################################################################################################
#JPN
JPN <- subset(JPN,Season == "2022")
JPN_sim <- JPN
JPN_sim$matchid <- paste(JPN_sim$Home,JPN_sim$Away,sep = "-")
JPN_fixtures$matchid <- paste(JPN_fixtures$HomeTeam_jpn,JPN_fixtures$AwayTeam_jpn,sep = "-")
JPN_fixtures$jpn_FTR <- sapply(JPN_fixtures$jpn_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

JPN_fixtures$jpn_gamestatus <- ifelse(JPN_fixtures$matchid %in% JPN_sim$matchid,"played","notplayed")

jpn_home_wins_sim <- c()
jpn_away_wins_sim <- c()
jpn_home_draws_sim <- c()
jpn_away_draws_sim <- c()
jpn_home_loss_sim <- c()
jpn_away_loss_sim <- c()



for (i_jpn_wins_sim in 1:length(jpn_teams))
{

  jpn_home_wins_sim[i_jpn_wins_sim] <- nrow(JPN_fixtures[JPN_fixtures$HomeTeam_jpn == jpn_teams[i_jpn_wins_sim] & JPN_fixtures$jpn_FTR == "H" & JPN_fixtures$jpn_gamestatus =="notplayed",])
  jpn_away_wins_sim[i_jpn_wins_sim] <- nrow(JPN_fixtures[JPN_fixtures$AwayTeam_jpn == jpn_teams[i_jpn_wins_sim] & JPN_fixtures$jpn_FTR == "A" & JPN_fixtures$jpn_gamestatus == "notplayed",])
  jpn_home_draws_sim[i_jpn_wins_sim] <- nrow(JPN_fixtures[JPN_fixtures$HomeTeam_jpn == jpn_teams[i_jpn_wins_sim] & JPN_fixtures$jpn_FTR == "D" & JPN_fixtures$jpn_gamestatus == "notplayed",])
  jpn_away_draws_sim[i_jpn_wins_sim] <- nrow(JPN_fixtures[JPN_fixtures$AwayTeam_jpn == jpn_teams[i_jpn_wins_sim] & JPN_fixtures$jpn_FTR == "D" & JPN_fixtures$jpn_gamestatus == "notplayed",])
  jpn_home_loss_sim[i_jpn_wins_sim] <- nrow(JPN_fixtures[JPN_fixtures$HomeTeam_jpn == jpn_teams[i_jpn_wins_sim] & JPN_fixtures$jpn_FTR == "A" & JPN_fixtures$jpn_gamestatus == "notplayed",])
  jpn_away_loss_sim[i_jpn_wins_sim] <- nrow(JPN_fixtures[JPN_fixtures$AwayTeam_jpn == jpn_teams[i_jpn_wins_sim] & JPN_fixtures$jpn_FTR == "H" & JPN_fixtures$jpn_gamestatus == "notplayed", ])

}

jpn_total_wins_sim <- jpn_home_wins_sim + jpn_away_wins_sim
jpn_total_draws_sim <- jpn_home_draws_sim + jpn_away_draws_sim
jpn_total_loss_sim <- jpn_home_loss_sim + jpn_away_loss_sim

jpn_home_games_sim <- c()
jpn_away_games_sim <-c()

for (i_jpn_sim in 1:length(jpn_teams))
{

  jpn_home_games_sim[i_jpn_sim] <- nrow(JPN_fixtures[JPN_fixtures$HomeTeam_jpn == jpn_teams[i_jpn_sim] & JPN_fixtures$jpn_gamestatus == "notplayed",])
  jpn_away_games_sim[i_jpn_sim]  <- nrow(JPN_fixtures[JPN_fixtures$AwayTeam_jpn == jpn_teams[i_jpn_sim] & JPN_fixtures$jpn_gamestatus == "notplayed",])

}

jpn_games_played_sim <- jpn_home_games_sim + jpn_away_games_sim

jpn_league_table_sim <- cbind(jpn_teams,jpn_games_played_sim,jpn_total_wins_sim,jpn_total_draws_sim,jpn_total_loss_sim)
jpn_PTS_sim <- (jpn_total_wins_sim*3) + (jpn_total_draws_sim*1)
jpn_league_table_sim <- cbind(jpn_league_table_sim,jpn_PTS_sim)

jpn_games_played_simfinal <- jpn_games_played + jpn_games_played_sim
jpn_total_wins_simfinal <- jpn_total_wins + jpn_total_wins_sim
jpn_total_draws_simfinal <- jpn_total_draws + jpn_total_draws_sim
jpn_total_loss_simfinal <- jpn_total_loss + jpn_total_loss_sim
jpn_PTS_simfinal <- jpn_PTS + jpn_PTS_sim

jpn_league_table_simfinal <- cbind(jpn_teams,jpn_games_played_simfinal,jpn_total_wins_simfinal,jpn_total_draws_simfinal,jpn_total_loss_simfinal,jpn_PTS_simfinal)
jpn_league_table_simfinal <- as.data.frame(jpn_league_table_simfinal)
names(jpn_league_table_simfinal)[names(jpn_league_table_simfinal) == "jpn_teams"] <- "Team_f"
names(jpn_league_table_simfinal)[names(jpn_league_table_simfinal) == "jpn_games_played_simfinal"] <- "P_f"
names(jpn_league_table_simfinal)[names(jpn_league_table_simfinal) == "jpn_total_wins_simfinal"] <- "W_f"
names(jpn_league_table_simfinal)[names(jpn_league_table_simfinal) == "jpn_total_draws_simfinal"] <- "D_f"
names(jpn_league_table_simfinal)[names(jpn_league_table_simfinal) == "jpn_total_loss_simfinal"] <- "L_f"
names(jpn_league_table_simfinal)[names(jpn_league_table_simfinal) == "jpn_PTS_simfinal"] <- "PTS_f"
points_jpn_sim <-  jpn_league_table_simfinal[order(as.numeric(jpn_league_table_simfinal$PTS_f), decreasing = TRUE),]

JPN_notplayed <- JPN_fixtures[JPN_fixtures$jpn_gamestatus == "notplayed",]

write.xlsx(points_jpn,'NL/Simulations.xlsx', sheetName = "JPN_table",append = TRUE)
write.xlsx(jpn_league_table_sim,'NL/Simulations.xlsx', sheetName = "JPN_sim",append = TRUE)
write.xlsx(points_jpn_sim,'NL/Simulations.xlsx', sheetName = "JPN_simfinal",append = TRUE)
write.xlsx(JPN_notplayed,'NL/Simulations.xlsx', sheetName = "JPN_notplayed",append = TRUE)
##############################################################################################################################
##############################################################################################################################
#MEX
MEX <- subset(MEX,Season == "2020/2021")
MEX_sim <- MEX
MEX_sim$matchid <- paste(MEX_sim$Home,MEX_sim$Away,sep = "-")
MEX_fixtures$matchid <- paste(MEX_fixtures$HomeTeam_mex,MEX_fixtures$AwayTeam_mex,sep = "-")
MEX_fixtures$mex_FTR <- sapply(MEX_fixtures$mex_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

MEX_fixtures$mex_gamestatus <- ifelse(MEX_fixtures$matchid %in% MEX_sim$matchid,"played","notplayed")

mex_home_wins_sim <- c()
mex_away_wins_sim <- c()
mex_home_draws_sim <- c()
mex_away_draws_sim <- c()
mex_home_loss_sim <- c()
mex_away_loss_sim <- c()



for (i_mex_wins_sim in 1:length(mex_teams))
{

  mex_home_wins_sim[i_mex_wins_sim] <- nrow(MEX_fixtures[MEX_fixtures$HomeTeam_mex == mex_teams[i_mex_wins_sim] & MEX_fixtures$mex_FTR == "H" & MEX_fixtures$mex_gamestatus =="notplayed",])
  mex_away_wins_sim[i_mex_wins_sim] <- nrow(MEX_fixtures[MEX_fixtures$AwayTeam_mex == mex_teams[i_mex_wins_sim] & MEX_fixtures$mex_FTR == "A" & MEX_fixtures$mex_gamestatus == "notplayed",])
  mex_home_draws_sim[i_mex_wins_sim] <- nrow(MEX_fixtures[MEX_fixtures$HomeTeam_mex == mex_teams[i_mex_wins_sim] & MEX_fixtures$mex_FTR == "D" & MEX_fixtures$mex_gamestatus == "notplayed",])
  mex_away_draws_sim[i_mex_wins_sim] <- nrow(MEX_fixtures[MEX_fixtures$AwayTeam_mex == mex_teams[i_mex_wins_sim] & MEX_fixtures$mex_FTR == "D" & MEX_fixtures$mex_gamestatus == "notplayed",])
  mex_home_loss_sim[i_mex_wins_sim] <- nrow(MEX_fixtures[MEX_fixtures$HomeTeam_mex == mex_teams[i_mex_wins_sim] & MEX_fixtures$mex_FTR == "A" & MEX_fixtures$mex_gamestatus == "notplayed",])
  mex_away_loss_sim[i_mex_wins_sim] <- nrow(MEX_fixtures[MEX_fixtures$AwayTeam_mex == mex_teams[i_mex_wins_sim] & MEX_fixtures$mex_FTR == "H" & MEX_fixtures$mex_gamestatus == "notplayed", ])

}

mex_total_wins_sim <- mex_home_wins_sim + mex_away_wins_sim
mex_total_draws_sim <- mex_home_draws_sim + mex_away_draws_sim
mex_total_loss_sim <- mex_home_loss_sim + mex_away_loss_sim

mex_home_games_sim <- c()
mex_away_games_sim <-c()

for (i_mex_sim in 1:length(mex_teams))
{

  mex_home_games_sim[i_mex_sim] <- nrow(MEX_fixtures[MEX_fixtures$HomeTeam_mex == mex_teams[i_mex_sim] & MEX_fixtures$mex_gamestatus == "notplayed",])
  mex_away_games_sim[i_mex_sim]  <- nrow(MEX_fixtures[MEX_fixtures$AwayTeam_mex == mex_teams[i_mex_sim] & MEX_fixtures$mex_gamestatus == "notplayed",])

}

mex_games_played_sim <- mex_home_games_sim + mex_away_games_sim

mex_league_table_sim <- cbind(mex_teams,mex_games_played_sim,mex_total_wins_sim,mex_total_draws_sim,mex_total_loss_sim)
mex_PTS_sim <- (mex_total_wins_sim*3) + (mex_total_draws_sim*1)
mex_league_table_sim <- cbind(mex_league_table_sim,mex_PTS_sim)

mex_games_played_simfinal <- mex_games_played + mex_games_played_sim
mex_total_wins_simfinal <- mex_total_wins + mex_total_wins_sim
mex_total_draws_simfinal <- mex_total_draws + mex_total_draws_sim
mex_total_loss_simfinal <- mex_total_loss + mex_total_loss_sim
mex_PTS_simfinal <- mex_PTS + mex_PTS_sim

mex_league_table_simfinal <- cbind(mex_teams,mex_games_played_simfinal,mex_total_wins_simfinal,mex_total_draws_simfinal,mex_total_loss_simfinal,mex_PTS_simfinal)
mex_league_table_simfinal <- as.data.frame(mex_league_table_simfinal)
names(mex_league_table_simfinal)[names(mex_league_table_simfinal) == "mex_teams"] <- "Team_f"
names(mex_league_table_simfinal)[names(mex_league_table_simfinal) == "mex_games_played_simfinal"] <- "P_f"
names(mex_league_table_simfinal)[names(mex_league_table_simfinal) == "mex_total_wins_simfinal"] <- "W_f"
names(mex_league_table_simfinal)[names(mex_league_table_simfinal) == "mex_total_draws_simfinal"] <- "D_f"
names(mex_league_table_simfinal)[names(mex_league_table_simfinal) == "mex_total_loss_simfinal"] <- "L_f"
names(mex_league_table_simfinal)[names(mex_league_table_simfinal) == "mex_PTS_simfinal"] <- "PTS_f"
points_mex_sim <-  mex_league_table_simfinal[order(as.numeric(mex_league_table_simfinal$PTS_f), decreasing = TRUE),]

MEX_notplayed <- MEX_fixtures[MEX_fixtures$mex_gamestatus == "notplayed",]

write.xlsx(points_mex,'NL/Simulations.xlsx', sheetName = "MEX_table",append = TRUE)
write.xlsx(mex_league_table_sim,'NL/Simulations.xlsx', sheetName = "MEX_sim",append = TRUE)
write.xlsx(points_mex_sim,'NL/Simulations.xlsx', sheetName = "MEX_simfinal",append = TRUE)
write.xlsx(MEX_notplayed,'NL/Simulations.xlsx', sheetName = "MEX_notplayed",append = TRUE)
#################################################################################################################################
#################################################################################################################################
#MLS
MLS <- subset(MLS,Season == "2022")
MLS_sim <- MLS
MLS_sim$matchid <- paste(MLS_sim$Home,MLS_sim$Away,sep = "-")
MLS_fixtures$matchid <- paste(MLS_fixtures$HomeTeam_mls,MLS_fixtures$AwayTeam_mls,sep = "-")
MLS_fixtures$mls_FTR <- sapply(MLS_fixtures$mls_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

MLS_fixtures$mls_gamestatus <- ifelse(MLS_fixtures$matchid %in% MLS_sim$matchid,"played","notplayed")

mls_home_wins_sim <- c()
mls_away_wins_sim <- c()
mls_home_draws_sim <- c()
mls_away_draws_sim <- c()
mls_home_loss_sim <- c()
mls_away_loss_sim <- c()



for (i_mls_wins_sim in 1:length(mls_teams))
{

  mls_home_wins_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "H" & MLS_fixtures$mls_gamestatus =="notplayed",])
  mls_away_wins_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "A" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_home_draws_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "D" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_away_draws_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "D" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_home_loss_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "A" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_away_loss_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "H" & MLS_fixtures$mls_gamestatus == "notplayed", ])

}

mls_total_wins_sim <- mls_home_wins_sim + mls_away_wins_sim
mls_total_draws_sim <- mls_home_draws_sim + mls_away_draws_sim
mls_total_loss_sim <- mls_home_loss_sim + mls_away_loss_sim

mls_home_games_sim <- c()
mls_away_games_sim <-c()

for (i_mls_sim in 1:length(mls_teams))
{

  mls_home_games_sim[i_mls_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_sim] & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_away_games_sim[i_mls_sim]  <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_sim] & MLS_fixtures$mls_gamestatus == "notplayed",])

}

mls_games_played_sim <- mls_home_games_sim + mls_away_games_sim

mls_league_table_sim <- cbind(mls_teams,mls_games_played_sim,mls_total_wins_sim,mls_total_draws_sim,mls_total_loss_sim)
mls_PTS_sim <- (mls_total_wins_sim*3) + (mls_total_draws_sim*1)
mls_league_table_sim <- cbind(mls_league_table_sim,mls_PTS_sim)

mls_games_played_simfinal <- mls_games_played + mls_games_played_sim
mls_total_wins_simfinal <- mls_total_wins + mls_total_wins_sim
mls_total_draws_simfinal <- mls_total_draws + mls_total_draws_sim
mls_total_loss_simfinal <- mls_total_loss + mls_total_loss_sim
mls_PTS_simfinal <- mls_PTS + mls_PTS_sim

mls_league_table_simfinal <- cbind(mls_teams,mls_games_played_simfinal,mls_total_wins_simfinal,mls_total_draws_simfinal,mls_total_loss_simfinal,mls_PTS_simfinal)
mls_league_table_simfinal <- as.data.frame(mls_league_table_simfinal)
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_teams"] <- "Team_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_games_played_simfinal"] <- "P_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_total_wins_simfinal"] <- "W_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_total_draws_simfinal"] <- "D_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_total_loss_simfinal"] <- "L_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_PTS_simfinal"] <- "PTS_f"
points_mls_sim <-  mls_league_table_simfinal[order(as.numeric(mls_league_table_simfinal$PTS_f), decreasing = TRUE),]

MLS_notplayed <- MLS_fixtures[MLS_fixtures$mls_gamestatus == "notplayed",]

write.xlsx(points_mls,'NL/Simulations.xlsx', sheetName = "MLS_table",append = TRUE)
write.xlsx(mls_league_table_sim,'NL/Simulations.xlsx', sheetName = "MLS_sim",append = TRUE)
write.xlsx(points_mls_sim,'NL/Simulations.xlsx', sheetName = "MLS_simfinal",append = TRUE)
write.xlsx(MLS_notplayed,'NL/Simulations.xlsx', sheetName = "MLS_notplayed",append = TRUE)
####################################################################################################################################
###################################################################################################################
#NOR
NOR <- subset(NOR,Season == "2022")
NOR_sim <- NOR
NOR_sim$matchid <- paste(NOR_sim$Home,NOR_sim$Away,sep = "-")
NOR_fixtures$matchid <- paste(NOR_fixtures$HomeTeam_nor,NOR_fixtures$AwayTeam_nor,sep = "-")
NOR_fixtures$nor_FTR <- sapply(NOR_fixtures$nor_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

NOR_fixtures$nor_gamestatus <- ifelse(NOR_fixtures$matchid %in% NOR_sim$matchid,"played","notplayed")

nor_home_wins_sim <- c()
nor_away_wins_sim <- c()
nor_home_draws_sim <- c()
nor_away_draws_sim <- c()
nor_home_loss_sim <- c()
nor_away_loss_sim <- c()



for (i_nor_wins_sim in 1:length(nor_teams))
{

  nor_home_wins_sim[i_nor_wins_sim] <- nrow(NOR_fixtures[NOR_fixtures$HomeTeam_nor == nor_teams[i_nor_wins_sim] & NOR_fixtures$nor_FTR == "H" & NOR_fixtures$nor_gamestatus =="notplayed",])
  nor_away_wins_sim[i_nor_wins_sim] <- nrow(NOR_fixtures[NOR_fixtures$AwayTeam_nor == nor_teams[i_nor_wins_sim] & NOR_fixtures$nor_FTR == "A" & NOR_fixtures$nor_gamestatus == "notplayed",])
  nor_home_draws_sim[i_nor_wins_sim] <- nrow(NOR_fixtures[NOR_fixtures$HomeTeam_nor == nor_teams[i_nor_wins_sim] & NOR_fixtures$nor_FTR == "D" & NOR_fixtures$nor_gamestatus == "notplayed",])
  nor_away_draws_sim[i_nor_wins_sim] <- nrow(NOR_fixtures[NOR_fixtures$AwayTeam_nor == nor_teams[i_nor_wins_sim] & NOR_fixtures$nor_FTR == "D" & NOR_fixtures$nor_gamestatus == "notplayed",])
  nor_home_loss_sim[i_nor_wins_sim] <- nrow(NOR_fixtures[NOR_fixtures$HomeTeam_nor == nor_teams[i_nor_wins_sim] & NOR_fixtures$nor_FTR == "A" & NOR_fixtures$nor_gamestatus == "notplayed",])
  nor_away_loss_sim[i_nor_wins_sim] <- nrow(NOR_fixtures[NOR_fixtures$AwayTeam_nor == nor_teams[i_nor_wins_sim] & NOR_fixtures$nor_FTR == "H" & NOR_fixtures$nor_gamestatus == "notplayed", ])

}

nor_total_wins_sim <- nor_home_wins_sim + nor_away_wins_sim
nor_total_draws_sim <- nor_home_draws_sim + nor_away_draws_sim
nor_total_loss_sim <- nor_home_loss_sim + nor_away_loss_sim

nor_home_games_sim <- c()
nor_away_games_sim <-c()

for (i_nor_sim in 1:length(nor_teams))
{

  nor_home_games_sim[i_nor_sim] <- nrow(NOR_fixtures[NOR_fixtures$HomeTeam_nor == nor_teams[i_nor_sim] & NOR_fixtures$nor_gamestatus == "notplayed",])
  nor_away_games_sim[i_nor_sim]  <- nrow(NOR_fixtures[NOR_fixtures$AwayTeam_nor == nor_teams[i_nor_sim] & NOR_fixtures$nor_gamestatus == "notplayed",])

}

nor_games_played_sim <- nor_home_games_sim + nor_away_games_sim

nor_league_table_sim <- cbind(nor_teams,nor_games_played_sim,nor_total_wins_sim,nor_total_draws_sim,nor_total_loss_sim)
nor_PTS_sim <- (nor_total_wins_sim*3) + (nor_total_draws_sim*1)
nor_league_table_sim <- cbind(nor_league_table_sim,nor_PTS_sim)

nor_games_played_simfinal <- nor_games_played + nor_games_played_sim
nor_total_wins_simfinal <- nor_total_wins + nor_total_wins_sim
nor_total_draws_simfinal <- nor_total_draws + nor_total_draws_sim
nor_total_loss_simfinal <- nor_total_loss + nor_total_loss_sim
nor_PTS_simfinal <- nor_PTS + nor_PTS_sim

nor_league_table_simfinal <- cbind(nor_teams,nor_games_played_simfinal,nor_total_wins_simfinal,nor_total_draws_simfinal,nor_total_loss_simfinal,nor_PTS_simfinal)
nor_league_table_simfinal <- as.data.frame(nor_league_table_simfinal)
names(nor_league_table_simfinal)[names(nor_league_table_simfinal) == "nor_teams"] <- "Team_f"
names(nor_league_table_simfinal)[names(nor_league_table_simfinal) == "nor_games_played_simfinal"] <- "P_f"
names(nor_league_table_simfinal)[names(nor_league_table_simfinal) == "nor_total_wins_simfinal"] <- "W_f"
names(nor_league_table_simfinal)[names(nor_league_table_simfinal) == "nor_total_draws_simfinal"] <- "D_f"
names(nor_league_table_simfinal)[names(nor_league_table_simfinal) == "nor_total_loss_simfinal"] <- "L_f"
names(nor_league_table_simfinal)[names(nor_league_table_simfinal) == "nor_PTS_simfinal"] <- "PTS_f"
points_nor_sim <-  nor_league_table_simfinal[order(as.numeric(nor_league_table_simfinal$PTS_f), decreasing = TRUE),]

NOR_notplayed <- NOR_fixtures[NOR_fixtures$nor_gamestatus == "notplayed",]


write.xlsx(points_nor,'NL/Simulations.xlsx', sheetName = "NOR_Table",append = TRUE)
write.xlsx(nor_league_table_sim,'NL/Simulations.xlsx', sheetName = "NOR_sim",append = TRUE)
write.xlsx(points_nor_sim,'NL/Simulations.xlsx', sheetName = "NOR_simfinal",append = TRUE)
write.xlsx(NOR_notplayed,'NL/Simulations.xlsx', sheetName = "NOR_notplayed",append = TRUE)
############################################################################################################################################################
############################################################################################################################################################
#POL
POL <- subset(POL,Season == "2021/2022")
POL_sim <- POL
POL_sim$matchid <- paste(POL_sim$Home,POL_sim$Away,sep = "-")
POL_fixtures$matchid <- paste(POL_fixtures$HomeTeam_pol,POL_fixtures$AwayTeam_pol,sep = "-")
POL_fixtures$pol_FTR <- sapply(POL_fixtures$pol_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

POL_fixtures$pol_gamestatus <- ifelse(POL_fixtures$matchid %in% POL_sim$matchid,"played","notplayed")

pol_home_wins_sim <- c()
pol_away_wins_sim <- c()
pol_home_draws_sim <- c()
pol_away_draws_sim <- c()
pol_home_loss_sim <- c()
pol_away_loss_sim <- c()



for (i_pol_wins_sim in 1:length(pol_teams))
{

  pol_home_wins_sim[i_pol_wins_sim] <- nrow(POL_fixtures[POL_fixtures$HomeTeam_pol == pol_teams[i_pol_wins_sim] & POL_fixtures$pol_FTR == "H" & POL_fixtures$pol_gamestatus =="notplayed",])
  pol_away_wins_sim[i_pol_wins_sim] <- nrow(POL_fixtures[POL_fixtures$AwayTeam_pol == pol_teams[i_pol_wins_sim] & POL_fixtures$pol_FTR == "A" & POL_fixtures$pol_gamestatus == "notplayed",])
  pol_home_draws_sim[i_pol_wins_sim] <- nrow(POL_fixtures[POL_fixtures$HomeTeam_pol == pol_teams[i_pol_wins_sim] & POL_fixtures$pol_FTR == "D" & POL_fixtures$pol_gamestatus == "notplayed",])
  pol_away_draws_sim[i_pol_wins_sim] <- nrow(POL_fixtures[POL_fixtures$AwayTeam_pol == pol_teams[i_pol_wins_sim] & POL_fixtures$pol_FTR == "D" & POL_fixtures$pol_gamestatus == "notplayed",])
  pol_home_loss_sim[i_pol_wins_sim] <- nrow(POL_fixtures[POL_fixtures$HomeTeam_pol == pol_teams[i_pol_wins_sim] & POL_fixtures$pol_FTR == "A" & POL_fixtures$pol_gamestatus == "notplayed",])
  pol_away_loss_sim[i_pol_wins_sim] <- nrow(POL_fixtures[POL_fixtures$AwayTeam_pol == pol_teams[i_pol_wins_sim] & POL_fixtures$pol_FTR == "H" & POL_fixtures$pol_gamestatus == "notplayed", ])

}

pol_total_wins_sim <- pol_home_wins_sim + pol_away_wins_sim
pol_total_draws_sim <- pol_home_draws_sim + pol_away_draws_sim
pol_total_loss_sim <- pol_home_loss_sim + pol_away_loss_sim

pol_home_games_sim <- c()
pol_away_games_sim <-c()

for (i_pol_sim in 1:length(pol_teams))
{

  pol_home_games_sim[i_pol_sim] <- nrow(POL_fixtures[POL_fixtures$HomeTeam_pol == pol_teams[i_pol_sim] & POL_fixtures$pol_gamestatus == "notplayed",])
  pol_away_games_sim[i_pol_sim]  <- nrow(POL_fixtures[POL_fixtures$AwayTeam_pol == pol_teams[i_pol_sim] & POL_fixtures$pol_gamestatus == "notplayed",])

}

pol_games_played_sim <- pol_home_games_sim + pol_away_games_sim

pol_league_table_sim <- cbind(pol_teams,pol_games_played_sim,pol_total_wins_sim,pol_total_draws_sim,pol_total_loss_sim)
pol_PTS_sim <- (pol_total_wins_sim*3) + (pol_total_draws_sim*1)
pol_league_table_sim <- cbind(pol_league_table_sim,pol_PTS_sim)

pol_games_played_simfinal <- pol_games_played + pol_games_played_sim
pol_total_wins_simfinal <- pol_total_wins + pol_total_wins_sim
pol_total_draws_simfinal <- pol_total_draws + pol_total_draws_sim
pol_total_loss_simfinal <- pol_total_loss + pol_total_loss_sim
pol_PTS_simfinal <- pol_PTS + pol_PTS_sim

pol_league_table_simfinal <- cbind(pol_teams,pol_games_played_simfinal,pol_total_wins_simfinal,pol_total_draws_simfinal,pol_total_loss_simfinal,pol_PTS_simfinal)
pol_league_table_simfinal <- as.data.frame(pol_league_table_simfinal)
names(pol_league_table_simfinal)[names(pol_league_table_simfinal) == "pol_teams"] <- "Team_f"
names(pol_league_table_simfinal)[names(pol_league_table_simfinal) == "pol_games_played_simfinal"] <- "P_f"
names(pol_league_table_simfinal)[names(pol_league_table_simfinal) == "pol_total_wins_simfinal"] <- "W_f"
names(pol_league_table_simfinal)[names(pol_league_table_simfinal) == "pol_total_draws_simfinal"] <- "D_f"
names(pol_league_table_simfinal)[names(pol_league_table_simfinal) == "pol_total_loss_simfinal"] <- "L_f"
names(pol_league_table_simfinal)[names(pol_league_table_simfinal) == "pol_PTS_simfinal"] <- "PTS_f"
points_pol_sim <-  pol_league_table_simfinal[order(as.numeric(pol_league_table_simfinal$PTS_f), decreasing = TRUE),]

POL_notplayed <- POL_fixtures[POL_fixtures$pol_gamestatus == "notplayed",]


write.xlsx(points_pol,'NL/Simulations.xlsx', sheetName = "POL_table",append = TRUE)
write.xlsx(pol_league_table_sim,'NL/Simulations.xlsx', sheetName = "POL_sim",append = TRUE)
write.xlsx(points_pol_sim,'NL/Simulations.xlsx', sheetName = "POL_simfinal",append = TRUE)
write.xlsx(POL_notplayed,'NL/Simulations.xlsx', sheetName = "POL_notplayed",append = TRUE)
##################################################################################################################################
##################################################################################################################################
#ROU
ROU <- subset(ROU,Season == "2021/2022")
ROU_sim <- ROU
ROU_sim$matchid <- paste(ROU_sim$Home,ROU_sim$Away,sep = "-")
ROU_fixtures$matchid <- paste(ROU_fixtures$HomeTeam_rou,ROU_fixtures$AwayTeam_rou,sep = "-")
ROU_fixtures$rou_FTR <- sapply(ROU_fixtures$rou_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

ROU_fixtures$rou_gamestatus <- ifelse(ROU_fixtures$matchid %in% ROU_sim$matchid,"played","notplayed")

rou_home_wins_sim <- c()
rou_away_wins_sim <- c()
rou_home_draws_sim <- c()
rou_away_draws_sim <- c()
rou_home_loss_sim <- c()
rou_away_loss_sim <- c()



for (i_rou_wins_sim in 1:length(rou_teams))
{

  rou_home_wins_sim[i_rou_wins_sim] <- nrow(ROU_fixtures[ROU_fixtures$HomeTeam_rou == rou_teams[i_rou_wins_sim] & ROU_fixtures$rou_FTR == "H" & ROU_fixtures$rou_gamestatus =="notplayed",])
  rou_away_wins_sim[i_rou_wins_sim] <- nrow(ROU_fixtures[ROU_fixtures$AwayTeam_rou == rou_teams[i_rou_wins_sim] & ROU_fixtures$rou_FTR == "A" & ROU_fixtures$rou_gamestatus == "notplayed",])
  rou_home_draws_sim[i_rou_wins_sim] <- nrow(ROU_fixtures[ROU_fixtures$HomeTeam_rou == rou_teams[i_rou_wins_sim] & ROU_fixtures$rou_FTR == "D" & ROU_fixtures$rou_gamestatus == "notplayed",])
  rou_away_draws_sim[i_rou_wins_sim] <- nrow(ROU_fixtures[ROU_fixtures$AwayTeam_rou == rou_teams[i_rou_wins_sim] & ROU_fixtures$rou_FTR == "D" & ROU_fixtures$rou_gamestatus == "notplayed",])
  rou_home_loss_sim[i_rou_wins_sim] <- nrow(ROU_fixtures[ROU_fixtures$HomeTeam_rou == rou_teams[i_rou_wins_sim] & ROU_fixtures$rou_FTR == "A" & ROU_fixtures$rou_gamestatus == "notplayed",])
  rou_away_loss_sim[i_rou_wins_sim] <- nrow(ROU_fixtures[ROU_fixtures$AwayTeam_rou == rou_teams[i_rou_wins_sim] & ROU_fixtures$rou_FTR == "H" & ROU_fixtures$rou_gamestatus == "notplayed", ])

}

rou_total_wins_sim <- rou_home_wins_sim + rou_away_wins_sim
rou_total_draws_sim <- rou_home_draws_sim + rou_away_draws_sim
rou_total_loss_sim <- rou_home_loss_sim + rou_away_loss_sim

rou_home_games_sim <- c()
rou_away_games_sim <-c()

for (i_rou_sim in 1:length(rou_teams))
{

  rou_home_games_sim[i_rou_sim] <- nrow(ROU_fixtures[ROU_fixtures$HomeTeam_rou == rou_teams[i_rou_sim] & ROU_fixtures$rou_gamestatus == "notplayed",])
  rou_away_games_sim[i_rou_sim]  <- nrow(ROU_fixtures[ROU_fixtures$AwayTeam_rou == rou_teams[i_rou_sim] & ROU_fixtures$rou_gamestatus == "notplayed",])

}

rou_games_played_sim <- rou_home_games_sim + rou_away_games_sim

rou_league_table_sim <- cbind(rou_teams,rou_games_played_sim,rou_total_wins_sim,rou_total_draws_sim,rou_total_loss_sim)
rou_PTS_sim <- (rou_total_wins_sim*3) + (rou_total_draws_sim*1)
rou_league_table_sim <- cbind(rou_league_table_sim,rou_PTS_sim)

rou_games_played_simfinal <- rou_games_played + rou_games_played_sim
rou_total_wins_simfinal <- rou_total_wins + rou_total_wins_sim
rou_total_draws_simfinal <- rou_total_draws + rou_total_draws_sim
rou_total_loss_simfinal <- rou_total_loss + rou_total_loss_sim
rou_PTS_simfinal <- rou_PTS + rou_PTS_sim

rou_league_table_simfinal <- cbind(rou_teams,rou_games_played_simfinal,rou_total_wins_simfinal,rou_total_draws_simfinal,rou_total_loss_simfinal,rou_PTS_simfinal)
rou_league_table_simfinal <- as.data.frame(rou_league_table_simfinal)
names(rou_league_table_simfinal)[names(rou_league_table_simfinal) == "rou_teams"] <- "Team_f"
names(rou_league_table_simfinal)[names(rou_league_table_simfinal) == "rou_games_played_simfinal"] <- "P_f"
names(rou_league_table_simfinal)[names(rou_league_table_simfinal) == "rou_total_wins_simfinal"] <- "W_f"
names(rou_league_table_simfinal)[names(rou_league_table_simfinal) == "rou_total_draws_simfinal"] <- "D_f"
names(rou_league_table_simfinal)[names(rou_league_table_simfinal) == "rou_total_loss_simfinal"] <- "L_f"
names(rou_league_table_simfinal)[names(rou_league_table_simfinal) == "rou_PTS_simfinal"] <- "PTS_f"
points_rou_sim <-  rou_league_table_simfinal[order(as.numeric(rou_league_table_simfinal$PTS_f), decreasing = TRUE),]

ROU_notplayed <- ROU_fixtures[ROU_fixtures$rou_gamestatus == "notplayed",]

write.xlsx(points_rou,'NL/Simulations.xlsx', sheetName = "ROU_table",append = TRUE)
write.xlsx(rou_league_table_sim,'NL/Simulations.xlsx', sheetName = "ROU_sim",append = TRUE)
write.xlsx(points_rou_sim,'NL/Simulations.xlsx', sheetName = "ROU_simfinal",append = TRUE)
write.xlsx(ROU_notplayed,'NL/Simulations.xlsx', sheetName = "ROU_simfinal",append = TRUE)
#########################################################################################################################################
#########################################################################################################################################
#RUS
RUS <- subset(RUS,Season == "2021/2022")
RUS_sim <- RUS
RUS <- subset(RUS,Season == "2021/2022")
RUS_sim$matchid <- paste(RUS_sim$HomeTeam,RUS_sim$AwayTeam,sep = "-")
RUS_fixtures$matchid <- paste(RUS_fixtures$HomeTeam_rus,RUS_fixtures$AwayTeam_rus,sep = "-")
RUS_fixtures$rus_FTR <- sapply(RUS_fixtures$rus_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

RUS_fixtures$rus_gamestatus <- ifelse(RUS_fixtures$matchid %in% RUS_sim$matchid,"played","notplayed")

rus_home_wins_sim <- c()
rus_away_wins_sim <- c()
rus_home_draws_sim <- c()
rus_away_draws_sim <- c()
rus_home_loss_sim <- c()
rus_away_loss_sim <- c()



for (i_rus_wins_sim in 1:length(rus_teams))
{

  rus_home_wins_sim[i_rus_wins_sim] <- nrow(RUS_fixtures[RUS_fixtures$HomeTeam_rus == rus_teams[i_rus_wins_sim] & RUS_fixtures$rus_FTR == "H" & RUS_fixtures$rus_gamestatus =="notplayed",])
  rus_away_wins_sim[i_rus_wins_sim] <- nrow(RUS_fixtures[RUS_fixtures$AwayTeam_rus == rus_teams[i_rus_wins_sim] & RUS_fixtures$rus_FTR == "A" & RUS_fixtures$rus_gamestatus == "notplayed",])
  rus_home_draws_sim[i_rus_wins_sim] <- nrow(RUS_fixtures[RUS_fixtures$HomeTeam_rus == rus_teams[i_rus_wins_sim] & RUS_fixtures$rus_FTR == "D" & RUS_fixtures$rus_gamestatus == "notplayed",])
  rus_away_draws_sim[i_rus_wins_sim] <- nrow(RUS_fixtures[RUS_fixtures$AwayTeam_rus == rus_teams[i_rus_wins_sim] & RUS_fixtures$rus_FTR == "D" & RUS_fixtures$rus_gamestatus == "notplayed",])
  rus_home_loss_sim[i_rus_wins_sim] <- nrow(RUS_fixtures[RUS_fixtures$HomeTeam_rus == rus_teams[i_rus_wins_sim] & RUS_fixtures$rus_FTR == "A" & RUS_fixtures$rus_gamestatus == "notplayed",])
  rus_away_loss_sim[i_rus_wins_sim] <- nrow(RUS_fixtures[RUS_fixtures$AwayTeam_rus == rus_teams[i_rus_wins_sim] & RUS_fixtures$rus_FTR == "H" & RUS_fixtures$rus_gamestatus == "notplayed", ])

}

rus_total_wins_sim <- rus_home_wins_sim + rus_away_wins_sim
rus_total_draws_sim <- rus_home_draws_sim + rus_away_draws_sim
rus_total_loss_sim <- rus_home_loss_sim + rus_away_loss_sim

rus_home_games_sim <- c()
rus_away_games_sim <-c()

for (i_rus_sim in 1:length(rus_teams))
{

  rus_home_games_sim[i_rus_sim] <- nrow(RUS_fixtures[RUS_fixtures$HomeTeam_rus == rus_teams[i_rus_sim] & RUS_fixtures$rus_gamestatus == "notplayed",])
  rus_away_games_sim[i_rus_sim]  <- nrow(RUS_fixtures[RUS_fixtures$AwayTeam_rus == rus_teams[i_rus_sim] & RUS_fixtures$rus_gamestatus == "notplayed",])

}

rus_games_played_sim <- rus_home_games_sim + rus_away_games_sim

rus_league_table_sim <- cbind(rus_teams,rus_games_played_sim,rus_total_wins_sim,rus_total_draws_sim,rus_total_loss_sim)
rus_PTS_sim <- (rus_total_wins_sim*3) + (rus_total_draws_sim*1)
rus_league_table_sim <- cbind(rus_league_table_sim,rus_PTS_sim)

rus_games_played_simfinal <- rus_games_played + rus_games_played_sim
rus_total_wins_simfinal <- rus_total_wins + rus_total_wins_sim
rus_total_draws_simfinal <- rus_total_draws + rus_total_draws_sim
rus_total_loss_simfinal <- rus_total_loss + rus_total_loss_sim
rus_PTS_simfinal <- rus_PTS + rus_PTS_sim

rus_league_table_simfinal <- cbind(rus_teams,rus_games_played_simfinal,rus_total_wins_simfinal,rus_total_draws_simfinal,rus_total_loss_simfinal,rus_PTS_simfinal)
rus_league_table_simfinal <- as.data.frame(rus_league_table_simfinal)
names(rus_league_table_simfinal)[names(rus_league_table_simfinal) == "rus_teams"] <- "Team_f"
names(rus_league_table_simfinal)[names(rus_league_table_simfinal) == "rus_games_played_simfinal"] <- "P_f"
names(rus_league_table_simfinal)[names(rus_league_table_simfinal) == "rus_total_wins_simfinal"] <- "W_f"
names(rus_league_table_simfinal)[names(rus_league_table_simfinal) == "rus_total_draws_simfinal"] <- "D_f"
names(rus_league_table_simfinal)[names(rus_league_table_simfinal) == "rus_total_loss_simfinal"] <- "L_f"
names(rus_league_table_simfinal)[names(rus_league_table_simfinal) == "rus_PTS_simfinal"] <- "PTS_f"
points_rus_sim <-  rus_league_table_simfinal[order(as.numeric(rus_league_table_simfinal$PTS_f), decreasing = TRUE),]

RUS_notplayed <- RUS_fixtures[RUS_fixtures$rus_gamestatus == "notplayed",]

write.xlsx(points_rus,'NL/Simulations.xlsx', sheetName = "RUS_table",append = TRUE)
write.xlsx(rus_league_table_sim,'NL/Simulations.xlsx', sheetName = "RUS_sim",append = TRUE)
write.xlsx(points_rus_sim,'NL/Simulations.xlsx', sheetName = "RUS_simfinal",append = TRUE)
write.xlsx(RUS_notplayed,'NL/Simulations.xlsx', sheetName = "RUS_notplayed",append = TRUE)
###################################################################################################################################
###################################################################################################################################
#SWE
SWE_sim <- SWE
SWE_sim$matchid <- paste(SWE_sim$Home,SWE_sim$Away,sep = "-")
SWE_fixtures$matchid <- paste(SWE_fixtures$HomeTeam_swe,SWE_fixtures$AwayTeam_swe,sep = "-")
SWE_fixtures$swe_FTR <- sapply(SWE_fixtures$swe_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

SWE_fixtures$swe_gamestatus <- ifelse(SWE_fixtures$matchid %in% SWE_sim$matchid,"played","notplayed")

swe_home_wins_sim <- c()
swe_away_wins_sim <- c()
swe_home_draws_sim <- c()
swe_away_draws_sim <- c()
swe_home_loss_sim <- c()
swe_away_loss_sim <- c()



for (i_swe_wins_sim in 1:length(swe_teams))
{

  swe_home_wins_sim[i_swe_wins_sim] <- nrow(SWE_fixtures[SWE_fixtures$HomeTeam_swe == swe_teams[i_swe_wins_sim] & SWE_fixtures$swe_FTR == "H" & SWE_fixtures$swe_gamestatus =="notplayed",])
  swe_away_wins_sim[i_swe_wins_sim] <- nrow(SWE_fixtures[SWE_fixtures$AwayTeam_swe == swe_teams[i_swe_wins_sim] & SWE_fixtures$swe_FTR == "A" & SWE_fixtures$swe_gamestatus == "notplayed",])
  swe_home_draws_sim[i_swe_wins_sim] <- nrow(SWE_fixtures[SWE_fixtures$HomeTeam_swe == swe_teams[i_swe_wins_sim] & SWE_fixtures$swe_FTR == "D" & SWE_fixtures$swe_gamestatus == "notplayed",])
  swe_away_draws_sim[i_swe_wins_sim] <- nrow(SWE_fixtures[SWE_fixtures$AwayTeam_swe == swe_teams[i_swe_wins_sim] & SWE_fixtures$swe_FTR == "D" & SWE_fixtures$swe_gamestatus == "notplayed",])
  swe_home_loss_sim[i_swe_wins_sim] <- nrow(SWE_fixtures[SWE_fixtures$HomeTeam_swe == swe_teams[i_swe_wins_sim] & SWE_fixtures$swe_FTR == "A" & SWE_fixtures$swe_gamestatus == "notplayed",])
  swe_away_loss_sim[i_swe_wins_sim] <- nrow(SWE_fixtures[SWE_fixtures$AwayTeam_swe == swe_teams[i_swe_wins_sim] & SWE_fixtures$swe_FTR == "H" & SWE_fixtures$swe_gamestatus == "notplayed", ])

}

swe_total_wins_sim <- swe_home_wins_sim + swe_away_wins_sim
swe_total_draws_sim <- swe_home_draws_sim + swe_away_draws_sim
swe_total_loss_sim <- swe_home_loss_sim + swe_away_loss_sim

swe_home_games_sim <- c()
swe_away_games_sim <-c()

for (i_swe_sim in 1:length(swe_teams))
{

  swe_home_games_sim[i_swe_sim] <- nrow(SWE_fixtures[SWE_fixtures$HomeTeam_swe == swe_teams[i_swe_sim] & SWE_fixtures$swe_gamestatus == "notplayed",])
  swe_away_games_sim[i_swe_sim]  <- nrow(SWE_fixtures[SWE_fixtures$AwayTeam_swe == swe_teams[i_swe_sim] & SWE_fixtures$swe_gamestatus == "notplayed",])

}

swe_games_played_sim <- swe_home_games_sim + swe_away_games_sim

swe_league_table_sim <- cbind(swe_teams,swe_games_played_sim,swe_total_wins_sim,swe_total_draws_sim,swe_total_loss_sim)
swe_PTS_sim <- (swe_total_wins_sim*3) + (swe_total_draws_sim*1)
swe_league_table_sim <- cbind(swe_league_table_sim,swe_PTS_sim)

swe_games_played_simfinal <- swe_games_played + swe_games_played_sim
swe_total_wins_simfinal <- swe_total_wins + swe_total_wins_sim
swe_total_draws_simfinal <- swe_total_draws + swe_total_draws_sim
swe_total_loss_simfinal <- swe_total_loss + swe_total_loss_sim
swe_PTS_simfinal <- swe_PTS + swe_PTS_sim

swe_league_table_simfinal <- cbind(swe_teams,swe_games_played_simfinal,swe_total_wins_simfinal,swe_total_draws_simfinal,swe_total_loss_simfinal,swe_PTS_simfinal)
swe_league_table_simfinal <- as.data.frame(swe_league_table_simfinal)
names(swe_league_table_simfinal)[names(swe_league_table_simfinal) == "swe_teams"] <- "Team_f"
names(swe_league_table_simfinal)[names(swe_league_table_simfinal) == "swe_games_played_simfinal"] <- "P_f"
names(swe_league_table_simfinal)[names(swe_league_table_simfinal) == "swe_total_wins_simfinal"] <- "W_f"
names(swe_league_table_simfinal)[names(swe_league_table_simfinal) == "swe_total_draws_simfinal"] <- "D_f"
names(swe_league_table_simfinal)[names(swe_league_table_simfinal) == "swe_total_loss_simfinal"] <- "L_f"
names(swe_league_table_simfinal)[names(swe_league_table_simfinal) == "swe_PTS_simfinal"] <- "PTS_f"
points_swe_sim <-  swe_league_table_simfinal[order(as.numeric(swe_league_table_simfinal$PTS_f), decreasing = TRUE),]

SWE_notplayed <- SWE_fixtures[SWE_fixtures$swe_gamestatus == "notplayed",]


write.xlsx(points_swe,'NL/Simulations.xlsx', sheetName = "SWE_table",append = TRUE)
write.xlsx(swe_league_table_sim,'NL/Simulations.xlsx', sheetName = "SWE_sim",append = TRUE)
write.xlsx(points_swe_sim,'NL/Simulations.xlsx', sheetName = "SWE_simfinal",append = TRUE)
write.xlsx(SWE_notplayed,'NL/Simulations.xlsx', sheetName = "SWE_notplayed",append = TRUE)
##########################################################################################################################################
###################################################################################################################
#SWZ
SWZ <- subset(SWZ,Season == "2021/2022")
SWZ_sim <- SWZ
SWZ_sim$matchid <- paste(SWZ_sim$Home,SWZ_sim$Away,sep = "-")
SWZ_fixtures$matchid <- paste(SWZ_fixtures$HomeTeam_swz,SWZ_fixtures$AwayTeam_swz,sep = "-")
SWZ_fixtures$swz_FTR <- sapply(SWZ_fixtures$swz_pscore,switch,
                               '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                               '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                               '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

SWZ_fixtures$swz_gamestatus <- ifelse(SWZ_fixtures$matchid %in% SWZ_sim$matchid,"played","notplayed")

swz_home_wins_sim <- c()
swz_away_wins_sim <- c()
swz_home_draws_sim <- c()
swz_away_draws_sim <- c()
swz_home_loss_sim <- c()
swz_away_loss_sim <- c()



for (i_swz_wins_sim in 1:length(swz_teams))
{

  swz_home_wins_sim[i_swz_wins_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$HomeTeam_swz == swz_teams[i_swz_wins_sim] & SWZ_fixtures$swz_FTR == "H" & SWZ_fixtures$swz_gamestatus =="notplayed",])
  swz_away_wins_sim[i_swz_wins_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$AwayTeam_swz == swz_teams[i_swz_wins_sim] & SWZ_fixtures$swz_FTR == "A" & SWZ_fixtures$swz_gamestatus == "notplayed",])
  swz_home_draws_sim[i_swz_wins_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$HomeTeam_swz == swz_teams[i_swz_wins_sim] & SWZ_fixtures$swz_FTR == "D" & SWZ_fixtures$swz_gamestatus == "notplayed",])
  swz_away_draws_sim[i_swz_wins_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$AwayTeam_swz == swz_teams[i_swz_wins_sim] & SWZ_fixtures$swz_FTR == "D" & SWZ_fixtures$swz_gamestatus == "notplayed",])
  swz_home_loss_sim[i_swz_wins_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$HomeTeam_swz == swz_teams[i_swz_wins_sim] & SWZ_fixtures$swz_FTR == "A" & SWZ_fixtures$swz_gamestatus == "notplayed",])
  swz_away_loss_sim[i_swz_wins_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$AwayTeam_swz == swz_teams[i_swz_wins_sim] & SWZ_fixtures$swz_FTR == "H" & SWZ_fixtures$swz_gamestatus == "notplayed", ])

}

swz_total_wins_sim <- swz_home_wins_sim + swz_away_wins_sim
swz_total_draws_sim <- swz_home_draws_sim + swz_away_draws_sim
swz_total_loss_sim <- swz_home_loss_sim + swz_away_loss_sim

swz_home_games_sim <- c()
swz_away_games_sim <-c()

for (i_swz_sim in 1:length(swz_teams))
{

  swz_home_games_sim[i_swz_sim] <- nrow(SWZ_fixtures[SWZ_fixtures$HomeTeam_swz == swz_teams[i_swz_sim] & SWZ_fixtures$swz_gamestatus == "notplayed",])
  swz_away_games_sim[i_swz_sim]  <- nrow(SWZ_fixtures[SWZ_fixtures$AwayTeam_swz == swz_teams[i_swz_sim] & SWZ_fixtures$swz_gamestatus == "notplayed",])

}

swz_games_played_sim <- swz_home_games_sim + swz_away_games_sim

swz_league_table_sim <- cbind(swz_teams,swz_games_played_sim,swz_total_wins_sim,swz_total_draws_sim,swz_total_loss_sim)
swz_PTS_sim <- (swz_total_wins_sim*3) + (swz_total_draws_sim*1)
swz_league_table_sim <- cbind(swz_league_table_sim,swz_PTS_sim)

swz_games_played_simfinal <- swz_games_played + swz_games_played_sim
swz_total_wins_simfinal <- swz_total_wins + swz_total_wins_sim
swz_total_draws_simfinal <- swz_total_draws + swz_total_draws_sim
swz_total_loss_simfinal <- swz_total_loss + swz_total_loss_sim
swz_PTS_simfinal <- swz_PTS + swz_PTS_sim

swz_league_table_simfinal <- cbind(swz_teams,swz_games_played_simfinal,swz_total_wins_simfinal,swz_total_draws_simfinal,swz_total_loss_simfinal,swz_PTS_simfinal)
swz_league_table_simfinal <- as.data.frame(swz_league_table_simfinal)
names(swz_league_table_simfinal)[names(swz_league_table_simfinal) == "swz_teams"] <- "Team_f"
names(swz_league_table_simfinal)[names(swz_league_table_simfinal) == "swz_games_played_simfinal"] <- "P_f"
names(swz_league_table_simfinal)[names(swz_league_table_simfinal) == "swz_total_wins_simfinal"] <- "W_f"
names(swz_league_table_simfinal)[names(swz_league_table_simfinal) == "swz_total_draws_simfinal"] <- "D_f"
names(swz_league_table_simfinal)[names(swz_league_table_simfinal) == "swz_total_loss_simfinal"] <- "L_f"
names(swz_league_table_simfinal)[names(swz_league_table_simfinal) == "swz_PTS_simfinal"] <- "PTS_f"
points_swz_sim <-  swz_league_table_simfinal[order(as.numeric(swz_league_table_simfinal$PTS_f), decreasing = TRUE),]

SWZ_notplayed <- SWZ_fixtures[SWZ_fixtures$swz_gamestatus == "notplayed",]


write.xlsx(points_swz,'NL/Simulations.xlsx', sheetName = "SWZ_table",append = TRUE)
write.xlsx(swz_league_table_sim,'NL/Simulations.xlsx', sheetName = "SWZ_sim",append = TRUE)
write.xlsx(points_swz_sim,'NL/Simulations.xlsx', sheetName = "SWZ_simfinal",append = TRUE)
write.xlsx(SWZ_notplayed,'NL/Simulations.xlsx', sheetName = "SWZ_notplayed",append = TRUE)
#####################################################################################################################################
###################################################################################################################
