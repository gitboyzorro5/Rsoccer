library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')

unlink('myodds_fixtures_prediction_nl.csv')
unlink('picks_fixtures_prediction_nl.csv')
################################################################
#AUT
AUT_fixtures$Hometeam_aut_index <- match(AUT_fixtures$HomeTeam_aut,aut_teams)
AUT_fixtures$Awayteam_aut_index <- match(AUT_fixtures$AwayTeam_aut,aut_teams)
aut_prediction <- c()
aut_HWM <- c()
aut_AWM <- c()
aut_HWMLM <- c()
aut_AWMLM <- c()
for(aut_row in 1:nrow(AUT_fixtures))
{

  aut_hometeamindex <- AUT_fixtures[aut_row,"Hometeam_aut_index"]
  aut_awayteamindex <- AUT_fixtures[aut_row,"Awayteam_aut_index"]
  #analyse team form
  #home team
  aut_form_vec_ht <- as.vector(aut_form_h[aut_hometeamindex,])
  aut_form_vec_ht[is.na(aut_form_vec_ht)] <- ""
  aut_form_vec_ht <- aut_form_vec_ht[aut_form_vec_ht != ""]
  aut_form_vec_ht  <-tail(aut_form_vec_ht,6)
  aut_ht_numberof_wins <- length(which(aut_form_vec_ht == "W"))
  aut_ht_numberof_draws <- length(which(aut_form_vec_ht == "D"))
  aut_ht_numberof_loss <- length(which(aut_form_vec_ht == "L"))
  #awayteam
  aut_form_vec_at <- as.vector(aut_form_h[aut_awayteamindex,])
  aut_form_vec_at[is.na(aut_form_vec_at)] <- ""
  aut_form_vec_at <- aut_form_vec_at[aut_form_vec_at != ""]
  aut_form_vec_at  <-tail(aut_form_vec_at,6)
  aut_at_numberof_wins <- length(which(aut_form_vec_at == "W"))
  aut_at_numberof_draws <- length(which(aut_form_vec_at == "D"))
  aut_at_numberof_loss <- length(which(aut_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  aut_goalscored_vec_ht <- as.vector(aut_goalscored_h[aut_hometeamindex,])
  aut_goalscored_vec_ht[is.na(aut_goalscored_vec_ht)] <- ""
  aut_goalscored_vec_ht <- aut_goalscored_vec_ht[aut_goalscored_vec_ht != ""]
  aut_goalscored_vec_ht  <-tail(aut_goalscored_vec_ht,6)
  aut_goalscored_vec_ht  <- as.numeric(aut_goalscored_vec_ht)
  aut_ht_totalgoalscored <- sum(aut_goalscored_vec_ht)
  aut_ht_matches_scoring <- length(which(aut_goalscored_vec_ht > 0))
  aut_ht_matches_without_scoring <- length(which(aut_goalscored_vec_ht == "0"))
  #awayteam
  aut_goalscored_vec_at <- as.vector(aut_goalscored_h[aut_awayteamindex,])
  aut_goalscored_vec_at[is.na(aut_goalscored_vec_at)] <- ""
  aut_goalscored_vec_at <- aut_goalscored_vec_at[aut_goalscored_vec_at != ""]
  aut_goalscored_vec_at  <-tail(aut_goalscored_vec_at,6)
  aut_goalscored_vec_at  <- as.numeric(aut_goalscored_vec_at)
  aut_at_totalgoalscored <- sum(aut_goalscored_vec_at)
  aut_at_matches_scoring <- length(which(aut_goalscored_vec_at > 0))
  aut_at_matches_without_scoring <- length(which(aut_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  aut_goalconceded_vec_ht <- as.vector(aut_goalconceded_h[aut_hometeamindex,])
  aut_goalconceded_vec_ht[is.na(aut_goalconceded_vec_ht)] <- ""
  aut_goalconceded_vec_ht <- aut_goalconceded_vec_ht[aut_goalconceded_vec_ht != ""]
  aut_goalconceded_vec_ht  <-tail(aut_goalconceded_vec_ht,6)
  aut_goalconceded_vec_ht  <- as.numeric(aut_goalconceded_vec_ht)
  aut_goalconceded_vec_ht
  aut_ht_totalgoalconceded <- sum(aut_goalconceded_vec_ht)
  aut_ht_matches_concede <- length(which(aut_goalconceded_vec_ht > 0))
  aut_ht_matches_without_concede <- length(which(aut_goalconceded_vec_ht == "0"))
  #awayteam
  aut_goalconceded_vec_at <- as.vector(aut_goalconceded_h[aut_awayteamindex,])
  aut_goalconceded_vec_at[is.na(aut_goalconceded_vec_at)] <- ""
  aut_goalconceded_vec_at <- aut_goalconceded_vec_at[aut_goalconceded_vec_at != ""]
  aut_goalconceded_vec_at  <-tail(aut_goalconceded_vec_at,6)
  aut_goalconceded_vec_at  <- as.numeric(aut_goalconceded_vec_at)
  aut_at_totalgoalconceded <- sum(aut_goalconceded_vec_at)
  aut_at_matches_concede <- length(which(aut_goalconceded_vec_at > 0))
  aut_at_matches_without_concede <- length(which(aut_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  aut_totalgoals_vec_ht <- as.vector(aut_totalgoals_h[aut_hometeamindex,])
  aut_totalgoals_vec_ht[is.na(aut_totalgoals_vec_ht)] <- ""
  aut_totalgoals_vec_ht <- aut_totalgoals_vec_ht[aut_totalgoals_vec_ht != ""]
  aut_totalgoals_vec_ht  <-tail(aut_totalgoals_vec_ht,6)
  aut_totalgoals_vec_ht  <- as.numeric(aut_totalgoals_vec_ht)
  aut_totalgoals_vec_ht
  aut_ht_totalgoals <- sum(aut_totalgoals_vec_ht)
  aut_ht_avgtotalgoals <- (aut_ht_totalgoals/6)
  aut_ht_no_of_ov25 <- length(which(aut_totalgoals_vec_ht >= 3))
  aut_ht_no_of_un25 <- length(which(aut_totalgoals_vec_ht <= 2))
  #awayteam
  aut_totalgoals_vec_at <- as.vector(aut_totalgoals_h[aut_awayteamindex,])
  aut_totalgoals_vec_at[is.na(aut_totalgoals_vec_at)] <- ""
  aut_totalgoals_vec_at <- aut_totalgoals_vec_at[aut_totalgoals_vec_at != ""]
  aut_totalgoals_vec_at  <-tail(aut_totalgoals_vec_at,6)
  aut_totalgoals_vec_at  <- as.numeric(aut_totalgoals_vec_at)
  aut_totalgoals_vec_at
  aut_at_totalgoals <- sum(aut_totalgoals_vec_at)
  aut_at_avgtotalgoals <- (aut_at_totalgoals/6)
  aut_at_no_of_ov25 <- length(which(aut_totalgoals_vec_at >= 3))
  aut_at_no_of_un25 <- length(which(aut_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  aut_winmargin_vec_ht <- as.vector(aut_winmargin_h[aut_hometeamindex,])
  aut_winmargin_vec_ht[is.na(aut_winmargin_vec_ht)] <- ""
  aut_winmargin_vec_ht <- aut_winmargin_vec_ht[aut_winmargin_vec_ht != ""]
  aut_winmargin_vec_ht  <-tail(aut_winmargin_vec_ht,6)
  aut_winmargin_vec_ht  <- as.numeric(aut_winmargin_vec_ht)

  aut_ht_totalwinmargin <- sum(aut_winmargin_vec_ht)
  aut_ht_no_of_winmargin_ov0 <- length(which(aut_winmargin_vec_ht >= 0))
  aut_ht_no_of_winmargin_ov1 <- length(which(aut_winmargin_vec_ht >= 1))
  aut_ht_no_of_winmargin_un0 <- length(which(aut_winmargin_vec_ht <= 0))
  aut_ht_no_of_winmargin_un1 <- length(which(aut_winmargin_vec_ht <= 1))
  #awayteam
  aut_winmargin_vec_at <- as.vector(aut_winmargin_h[aut_awayteamindex,])
  aut_winmargin_vec_at[is.na(aut_winmargin_vec_at)] <- ""
  aut_winmargin_vec_at <- aut_winmargin_vec_at[aut_winmargin_vec_at != ""]
  aut_winmargin_vec_at  <-tail(aut_winmargin_vec_at,6)
  aut_winmargin_vec_at  <- as.numeric(aut_winmargin_vec_at)

  aut_at_totalwinmargin <- sum(aut_winmargin_vec_at)
  aut_at_no_of_winmargin_ov0 <- length(which(aut_winmargin_vec_at >= 0))
  aut_at_no_of_winmargin_ov1 <- length(which(aut_winmargin_vec_at >= 1))
  aut_at_no_of_winmargin_un0 <- length(which(aut_winmargin_vec_at <= 0))
  aut_at_no_of_winmargin_un1 <- length(which(aut_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  aut_winmargin_vec_ht_lm <- as.vector(aut_winmargin_h[aut_hometeamindex,])
  aut_winmargin_vec_ht_lm[is.na(aut_winmargin_vec_ht_lm)] <- ""
  aut_winmargin_vec_ht_lm <- aut_winmargin_vec_ht_lm[aut_winmargin_vec_ht_lm != ""]
  aut_winmargin_vec_ht_lm  <-tail(aut_winmargin_vec_ht_lm,1)
  #awayteam
  aut_winmargin_vec_at_lm <- as.vector(aut_winmargin_h[aut_awayteamindex,])
  aut_winmargin_vec_at_lm[is.na(aut_winmargin_vec_at_lm)] <- ""
  aut_winmargin_vec_at_lm <- aut_winmargin_vec_at_lm[aut_winmargin_vec_at_lm != ""]
  aut_winmargin_vec_at_lm  <-tail(aut_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  aut_ht_last6points <- aut_ht_numberof_wins*3 + aut_ht_numberof_draws*1
  aut_at_last6points <- aut_at_numberof_wins*3 + aut_at_numberof_draws*1

  if(aut_ht_last6points > aut_at_last6points) {aut_3waypick <- "1"}  else {aut_3waypick <- "X2"}

  if(aut_at_last6points > aut_ht_last6points ) {aut_3waypick <- "2"} else {aut_3waypick <- "1X"}

  if(aut_ht_no_of_ov25 + aut_at_no_of_ov25 >= 6) {aut_goalspick <- "ov25"} else {aut_goalspick <- "un25"}

  if(aut_ht_no_of_un25 + aut_at_no_of_un25 >= 6) {aut_goalspick <- "un25"} else {aut_goalspick <- "ov25"}

  if(aut_ht_matches_scoring >= 4 && aut_at_matches_scoring >=4) {aut_btts <- "BTTS-Y"} else {aut_btts <- "BTTS-N"}


  aut_prediction[aut_row] <- rbind(paste(aut_3waypick,aut_goalspick,aut_btts,sep = ","))
  aut_HWM[aut_row] <- aut_ht_totalwinmargin
  aut_AWM[aut_row] <- aut_at_totalwinmargin

  aut_HWMLM[aut_row] <- aut_winmargin_vec_ht_lm
  aut_AWMLM[aut_row] <- aut_winmargin_vec_at_lm


}

aut_prediction <- as.data.frame(aut_prediction)
colnames(aut_prediction) <- "prediction"

aut_HWM <- as.data.frame(aut_HWM)
colnames(aut_HWM) <- "HWM"

aut_AWM <- as.data.frame(aut_AWM)
colnames(aut_AWM) <- "AWM"

aut_HWMLM <- as.data.frame(aut_HWMLM)
colnames(aut_HWMLM) <- "HWMLM"

aut_AWMLM <- as.data.frame(aut_AWMLM)
colnames(aut_AWMLM) <- "AWMLM"

aut_picks <- cbind(AUT_fixtures$Div,AUT_fixtures$HomeTeam_aut,AUT_fixtures$AwayTeam_aut,aut_prediction,aut_HWM,aut_AWM,aut_HWMLM,aut_AWMLM)
colnames(aut_picks)[1] <- "picks_Div"
colnames(aut_picks)[2] <- "picks_HomeTeam"
colnames(aut_picks)[3] <- "picks_AwayTeam"
aut_picks$matchid <- paste(aut_picks$picks_HomeTeam,aut_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of AUT
aut_picks
############################################################################################
#ARG
ARG_fixtures$Hometeam_arg_index <- match(ARG_fixtures$HomeTeam_arg,arg_teams)
ARG_fixtures$Awayteam_arg_index <- match(ARG_fixtures$AwayTeam_arg,arg_teams)
arg_prediction <- c()
arg_HWM <- c()
arg_AWM <- c()
arg_HWMLM <- c()
arg_AWMLM <- c()
for(arg_row in 1:nrow(ARG_fixtures))
{

  arg_hometeamindex <- ARG_fixtures[arg_row,"Hometeam_arg_index"]
  arg_awayteamindex <- ARG_fixtures[arg_row,"Awayteam_arg_index"]
  #analyse team form
  #home team
  arg_form_vec_ht <- as.vector(arg_form_h[arg_hometeamindex,])
  arg_form_vec_ht[is.na(arg_form_vec_ht)] <- ""
  arg_form_vec_ht <- arg_form_vec_ht[arg_form_vec_ht != ""]
  arg_form_vec_ht  <-tail(arg_form_vec_ht,6)
  arg_ht_numberof_wins <- length(which(arg_form_vec_ht == "W"))
  arg_ht_numberof_draws <- length(which(arg_form_vec_ht == "D"))
  arg_ht_numberof_loss <- length(which(arg_form_vec_ht == "L"))
  #awayteam
  arg_form_vec_at <- as.vector(arg_form_h[arg_awayteamindex,])
  arg_form_vec_at[is.na(arg_form_vec_at)] <- ""
  arg_form_vec_at <- arg_form_vec_at[arg_form_vec_at != ""]
  arg_form_vec_at  <-tail(arg_form_vec_at,6)
  arg_at_numberof_wins <- length(which(arg_form_vec_at == "W"))
  arg_at_numberof_draws <- length(which(arg_form_vec_at == "D"))
  arg_at_numberof_loss <- length(which(arg_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  arg_goalscored_vec_ht <- as.vector(arg_goalscored_h[arg_hometeamindex,])
  arg_goalscored_vec_ht[is.na(arg_goalscored_vec_ht)] <- ""
  arg_goalscored_vec_ht <- arg_goalscored_vec_ht[arg_goalscored_vec_ht != ""]
  arg_goalscored_vec_ht  <-tail(arg_goalscored_vec_ht,6)
  arg_goalscored_vec_ht  <- as.numeric(arg_goalscored_vec_ht)
  arg_ht_totalgoalscored <- sum(arg_goalscored_vec_ht)
  arg_ht_matches_scoring <- length(which(arg_goalscored_vec_ht > 0))
  arg_ht_matches_without_scoring <- length(which(arg_goalscored_vec_ht == "0"))
  #awayteam
  arg_goalscored_vec_at <- as.vector(arg_goalscored_h[arg_awayteamindex,])
  arg_goalscored_vec_at[is.na(arg_goalscored_vec_at)] <- ""
  arg_goalscored_vec_at <- arg_goalscored_vec_at[arg_goalscored_vec_at != ""]
  arg_goalscored_vec_at  <-tail(arg_goalscored_vec_at,6)
  arg_goalscored_vec_at  <- as.numeric(arg_goalscored_vec_at)
  arg_at_totalgoalscored <- sum(arg_goalscored_vec_at)
  arg_at_matches_scoring <- length(which(arg_goalscored_vec_at > 0))
  arg_at_matches_without_scoring <- length(which(arg_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  arg_goalconceded_vec_ht <- as.vector(arg_goalconceded_h[arg_hometeamindex,])
  arg_goalconceded_vec_ht[is.na(arg_goalconceded_vec_ht)] <- ""
  arg_goalconceded_vec_ht <- arg_goalconceded_vec_ht[arg_goalconceded_vec_ht != ""]
  arg_goalconceded_vec_ht  <-tail(arg_goalconceded_vec_ht,6)
  arg_goalconceded_vec_ht  <- as.numeric(arg_goalconceded_vec_ht)
  arg_goalconceded_vec_ht
  arg_ht_totalgoalconceded <- sum(arg_goalconceded_vec_ht)
  arg_ht_matches_concede <- length(which(arg_goalconceded_vec_ht > 0))
  arg_ht_matches_without_concede <- length(which(arg_goalconceded_vec_ht == "0"))
  #awayteam
  arg_goalconceded_vec_at <- as.vector(arg_goalconceded_h[arg_awayteamindex,])
  arg_goalconceded_vec_at[is.na(arg_goalconceded_vec_at)] <- ""
  arg_goalconceded_vec_at <- arg_goalconceded_vec_at[arg_goalconceded_vec_at != ""]
  arg_goalconceded_vec_at  <-tail(arg_goalconceded_vec_at,6)
  arg_goalconceded_vec_at  <- as.numeric(arg_goalconceded_vec_at)
  arg_at_totalgoalconceded <- sum(arg_goalconceded_vec_at)
  arg_at_matches_concede <- length(which(arg_goalconceded_vec_at > 0))
  arg_at_matches_without_concede <- length(which(arg_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  arg_totalgoals_vec_ht <- as.vector(arg_totalgoals_h[arg_hometeamindex,])
  arg_totalgoals_vec_ht[is.na(arg_totalgoals_vec_ht)] <- ""
  arg_totalgoals_vec_ht <- arg_totalgoals_vec_ht[arg_totalgoals_vec_ht != ""]
  arg_totalgoals_vec_ht  <-tail(arg_totalgoals_vec_ht,6)
  arg_totalgoals_vec_ht  <- as.numeric(arg_totalgoals_vec_ht)
  arg_totalgoals_vec_ht
  arg_ht_totalgoals <- sum(arg_totalgoals_vec_ht)
  arg_ht_avgtotalgoals <- (arg_ht_totalgoals/6)
  arg_ht_no_of_ov25 <- length(which(arg_totalgoals_vec_ht >= 3))
  arg_ht_no_of_un25 <- length(which(arg_totalgoals_vec_ht <= 2))
  #awayteam
  arg_totalgoals_vec_at <- as.vector(arg_totalgoals_h[arg_awayteamindex,])
  arg_totalgoals_vec_at[is.na(arg_totalgoals_vec_at)] <- ""
  arg_totalgoals_vec_at <- arg_totalgoals_vec_at[arg_totalgoals_vec_at != ""]
  arg_totalgoals_vec_at  <-tail(arg_totalgoals_vec_at,6)
  arg_totalgoals_vec_at  <- as.numeric(arg_totalgoals_vec_at)
  arg_totalgoals_vec_at
  arg_at_totalgoals <- sum(arg_totalgoals_vec_at)
  arg_at_avgtotalgoals <- (arg_at_totalgoals/6)
  arg_at_no_of_ov25 <- length(which(arg_totalgoals_vec_at >= 3))
  arg_at_no_of_un25 <- length(which(arg_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  arg_winmargin_vec_ht <- as.vector(arg_winmargin_h[arg_hometeamindex,])
  arg_winmargin_vec_ht[is.na(arg_winmargin_vec_ht)] <- ""
  arg_winmargin_vec_ht <- arg_winmargin_vec_ht[arg_winmargin_vec_ht != ""]
  arg_winmargin_vec_ht  <-tail(arg_winmargin_vec_ht,6)
  arg_winmargin_vec_ht  <- as.numeric(arg_winmargin_vec_ht)

  arg_ht_totalwinmargin <- sum(arg_winmargin_vec_ht)
  arg_ht_no_of_winmargin_ov0 <- length(which(arg_winmargin_vec_ht >= 0))
  arg_ht_no_of_winmargin_ov1 <- length(which(arg_winmargin_vec_ht >= 1))
  arg_ht_no_of_winmargin_un0 <- length(which(arg_winmargin_vec_ht <= 0))
  arg_ht_no_of_winmargin_un1 <- length(which(arg_winmargin_vec_ht <= 1))
  #awayteam
  arg_winmargin_vec_at <- as.vector(arg_winmargin_h[arg_awayteamindex,])
  arg_winmargin_vec_at[is.na(arg_winmargin_vec_at)] <- ""
  arg_winmargin_vec_at <- arg_winmargin_vec_at[arg_winmargin_vec_at != ""]
  arg_winmargin_vec_at  <-tail(arg_winmargin_vec_at,6)
  arg_winmargin_vec_at  <- as.numeric(arg_winmargin_vec_at)

  arg_at_totalwinmargin <- sum(arg_winmargin_vec_at)
  arg_at_no_of_winmargin_ov0 <- length(which(arg_winmargin_vec_at >= 0))
  arg_at_no_of_winmargin_ov1 <- length(which(arg_winmargin_vec_at >= 1))
  arg_at_no_of_winmargin_un0 <- length(which(arg_winmargin_vec_at <= 0))
  arg_at_no_of_winmargin_un1 <- length(which(arg_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  arg_winmargin_vec_ht_lm <- as.vector(arg_winmargin_h[arg_hometeamindex,])
  arg_winmargin_vec_ht_lm[is.na(arg_winmargin_vec_ht_lm)] <- ""
  arg_winmargin_vec_ht_lm <- arg_winmargin_vec_ht_lm[arg_winmargin_vec_ht_lm != ""]
  arg_winmargin_vec_ht_lm  <-tail(arg_winmargin_vec_ht_lm,1)
  #awayteam
  arg_winmargin_vec_at_lm <- as.vector(arg_winmargin_h[arg_awayteamindex,])
  arg_winmargin_vec_at_lm[is.na(arg_winmargin_vec_at_lm)] <- ""
  arg_winmargin_vec_at_lm <- arg_winmargin_vec_at_lm[arg_winmargin_vec_at_lm != ""]
  arg_winmargin_vec_at_lm  <-tail(arg_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  arg_ht_last6points <- arg_ht_numberof_wins*3 + arg_ht_numberof_draws*1
  arg_at_last6points <- arg_at_numberof_wins*3 + arg_at_numberof_draws*1

  if(arg_ht_last6points > arg_at_last6points) {arg_3waypick <- "1"}  else {arg_3waypick <- "X2"}

  if(arg_at_last6points > arg_ht_last6points ) {arg_3waypick <- "2"} else {arg_3waypick <- "1X"}

  if(arg_ht_no_of_ov25 + arg_at_no_of_ov25 >= 6) {arg_goalspick <- "ov25"} else {arg_goalspick <- "un25"}

  if(arg_ht_no_of_un25 + arg_at_no_of_un25 >= 6) {arg_goalspick <- "un25"} else {arg_goalspick <- "ov25"}

  if(arg_ht_matches_scoring >= 4 && arg_at_matches_scoring >=4) {arg_btts <- "BTTS-Y"} else {arg_btts <- "BTTS-N"}


  arg_prediction[arg_row] <- rbind(paste(arg_3waypick,arg_goalspick,arg_btts,sep = ","))
  arg_HWM[arg_row] <- arg_ht_totalwinmargin
  arg_AWM[arg_row] <- arg_at_totalwinmargin

  arg_HWMLM[arg_row] <- arg_winmargin_vec_ht_lm
  arg_AWMLM[arg_row] <- arg_winmargin_vec_at_lm


}

arg_prediction <- as.data.frame(arg_prediction)
colnames(arg_prediction) <- "prediction"

arg_HWM <- as.data.frame(arg_HWM)
colnames(arg_HWM) <- "HWM"

arg_AWM <- as.data.frame(arg_AWM)
colnames(arg_AWM) <- "AWM"

arg_HWMLM <- as.data.frame(arg_HWMLM)
colnames(arg_HWMLM) <- "HWMLM"

arg_AWMLM <- as.data.frame(arg_AWMLM)
colnames(arg_AWMLM) <- "AWMLM"

arg_picks <- cbind(ARG_fixtures$Div,ARG_fixtures$HomeTeam_arg,ARG_fixtures$AwayTeam_arg,arg_prediction,arg_HWM,arg_AWM,arg_HWMLM,arg_AWMLM)
colnames(arg_picks)[1] <- "picks_Div"
colnames(arg_picks)[2] <- "picks_HomeTeam"
colnames(arg_picks)[3] <- "picks_AwayTeam"
arg_picks$matchid <- paste(arg_picks$picks_HomeTeam,arg_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of ARG
arg_picks
############################################################################################
#BRA
BRA_fixtures$Hometeam_bra_index <- match(BRA_fixtures$HomeTeam_bra,bra_teams)
BRA_fixtures$Awayteam_bra_index <- match(BRA_fixtures$AwayTeam_bra,bra_teams)
bra_prediction <- c()
bra_HWM <- c()
bra_AWM <- c()
bra_HWMLM <- c()
bra_AWMLM <- c()
for(bra_row in 1:nrow(BRA_fixtures))
{

  bra_hometeamindex <- BRA_fixtures[bra_row,"Hometeam_bra_index"]
  bra_awayteamindex <- BRA_fixtures[bra_row,"Awayteam_bra_index"]
  #analyse team form
  #home team
  bra_form_vec_ht <- as.vector(bra_form_h[bra_hometeamindex,])
  bra_form_vec_ht[is.na(bra_form_vec_ht)] <- ""
  bra_form_vec_ht <- bra_form_vec_ht[bra_form_vec_ht != ""]
  bra_form_vec_ht  <-tail(bra_form_vec_ht,6)
  bra_ht_numberof_wins <- length(which(bra_form_vec_ht == "W"))
  bra_ht_numberof_draws <- length(which(bra_form_vec_ht == "D"))
  bra_ht_numberof_loss <- length(which(bra_form_vec_ht == "L"))
  #awayteam
  bra_form_vec_at <- as.vector(bra_form_h[bra_awayteamindex,])
  bra_form_vec_at[is.na(bra_form_vec_at)] <- ""
  bra_form_vec_at <- bra_form_vec_at[bra_form_vec_at != ""]
  bra_form_vec_at  <-tail(bra_form_vec_at,6)
  bra_at_numberof_wins <- length(which(bra_form_vec_at == "W"))
  bra_at_numberof_draws <- length(which(bra_form_vec_at == "D"))
  bra_at_numberof_loss <- length(which(bra_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  bra_goalscored_vec_ht <- as.vector(bra_goalscored_h[bra_hometeamindex,])
  bra_goalscored_vec_ht[is.na(bra_goalscored_vec_ht)] <- ""
  bra_goalscored_vec_ht <- bra_goalscored_vec_ht[bra_goalscored_vec_ht != ""]
  bra_goalscored_vec_ht  <-tail(bra_goalscored_vec_ht,6)
  bra_goalscored_vec_ht  <- as.numeric(bra_goalscored_vec_ht)
  bra_ht_totalgoalscored <- sum(bra_goalscored_vec_ht)
  bra_ht_matches_scoring <- length(which(bra_goalscored_vec_ht > 0))
  bra_ht_matches_without_scoring <- length(which(bra_goalscored_vec_ht == "0"))
  #awayteam
  bra_goalscored_vec_at <- as.vector(bra_goalscored_h[bra_awayteamindex,])
  bra_goalscored_vec_at[is.na(bra_goalscored_vec_at)] <- ""
  bra_goalscored_vec_at <- bra_goalscored_vec_at[bra_goalscored_vec_at != ""]
  bra_goalscored_vec_at  <-tail(bra_goalscored_vec_at,6)
  bra_goalscored_vec_at  <- as.numeric(bra_goalscored_vec_at)
  bra_at_totalgoalscored <- sum(bra_goalscored_vec_at)
  bra_at_matches_scoring <- length(which(bra_goalscored_vec_at > 0))
  bra_at_matches_without_scoring <- length(which(bra_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  bra_goalconceded_vec_ht <- as.vector(bra_goalconceded_h[bra_hometeamindex,])
  bra_goalconceded_vec_ht[is.na(bra_goalconceded_vec_ht)] <- ""
  bra_goalconceded_vec_ht <- bra_goalconceded_vec_ht[bra_goalconceded_vec_ht != ""]
  bra_goalconceded_vec_ht  <-tail(bra_goalconceded_vec_ht,6)
  bra_goalconceded_vec_ht  <- as.numeric(bra_goalconceded_vec_ht)
  bra_goalconceded_vec_ht
  bra_ht_totalgoalconceded <- sum(bra_goalconceded_vec_ht)
  bra_ht_matches_concede <- length(which(bra_goalconceded_vec_ht > 0))
  bra_ht_matches_without_concede <- length(which(bra_goalconceded_vec_ht == "0"))
  #awayteam
  bra_goalconceded_vec_at <- as.vector(bra_goalconceded_h[bra_awayteamindex,])
  bra_goalconceded_vec_at[is.na(bra_goalconceded_vec_at)] <- ""
  bra_goalconceded_vec_at <- bra_goalconceded_vec_at[bra_goalconceded_vec_at != ""]
  bra_goalconceded_vec_at  <-tail(bra_goalconceded_vec_at,6)
  bra_goalconceded_vec_at  <- as.numeric(bra_goalconceded_vec_at)
  bra_at_totalgoalconceded <- sum(bra_goalconceded_vec_at)
  bra_at_matches_concede <- length(which(bra_goalconceded_vec_at > 0))
  bra_at_matches_without_concede <- length(which(bra_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  bra_totalgoals_vec_ht <- as.vector(bra_totalgoals_h[bra_hometeamindex,])
  bra_totalgoals_vec_ht[is.na(bra_totalgoals_vec_ht)] <- ""
  bra_totalgoals_vec_ht <- bra_totalgoals_vec_ht[bra_totalgoals_vec_ht != ""]
  bra_totalgoals_vec_ht  <-tail(bra_totalgoals_vec_ht,6)
  bra_totalgoals_vec_ht  <- as.numeric(bra_totalgoals_vec_ht)
  bra_totalgoals_vec_ht
  bra_ht_totalgoals <- sum(bra_totalgoals_vec_ht)
  bra_ht_avgtotalgoals <- (bra_ht_totalgoals/6)
  bra_ht_no_of_ov25 <- length(which(bra_totalgoals_vec_ht >= 3))
  bra_ht_no_of_un25 <- length(which(bra_totalgoals_vec_ht <= 2))
  #awayteam
  bra_totalgoals_vec_at <- as.vector(bra_totalgoals_h[bra_awayteamindex,])
  bra_totalgoals_vec_at[is.na(bra_totalgoals_vec_at)] <- ""
  bra_totalgoals_vec_at <- bra_totalgoals_vec_at[bra_totalgoals_vec_at != ""]
  bra_totalgoals_vec_at  <-tail(bra_totalgoals_vec_at,6)
  bra_totalgoals_vec_at  <- as.numeric(bra_totalgoals_vec_at)
  bra_totalgoals_vec_at
  bra_at_totalgoals <- sum(bra_totalgoals_vec_at)
  bra_at_avgtotalgoals <- (bra_at_totalgoals/6)
  bra_at_no_of_ov25 <- length(which(bra_totalgoals_vec_at >= 3))
  bra_at_no_of_un25 <- length(which(bra_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  bra_winmargin_vec_ht <- as.vector(bra_winmargin_h[bra_hometeamindex,])
  bra_winmargin_vec_ht[is.na(bra_winmargin_vec_ht)] <- ""
  bra_winmargin_vec_ht <- bra_winmargin_vec_ht[bra_winmargin_vec_ht != ""]
  bra_winmargin_vec_ht  <-tail(bra_winmargin_vec_ht,6)
  bra_winmargin_vec_ht  <- as.numeric(bra_winmargin_vec_ht)

  bra_ht_totalwinmargin <- sum(bra_winmargin_vec_ht)
  bra_ht_no_of_winmargin_ov0 <- length(which(bra_winmargin_vec_ht >= 0))
  bra_ht_no_of_winmargin_ov1 <- length(which(bra_winmargin_vec_ht >= 1))
  bra_ht_no_of_winmargin_un0 <- length(which(bra_winmargin_vec_ht <= 0))
  bra_ht_no_of_winmargin_un1 <- length(which(bra_winmargin_vec_ht <= 1))
  #awayteam
  bra_winmargin_vec_at <- as.vector(bra_winmargin_h[bra_awayteamindex,])
  bra_winmargin_vec_at[is.na(bra_winmargin_vec_at)] <- ""
  bra_winmargin_vec_at <- bra_winmargin_vec_at[bra_winmargin_vec_at != ""]
  bra_winmargin_vec_at  <-tail(bra_winmargin_vec_at,6)
  bra_winmargin_vec_at  <- as.numeric(bra_winmargin_vec_at)

  bra_at_totalwinmargin <- sum(bra_winmargin_vec_at)
  bra_at_no_of_winmargin_ov0 <- length(which(bra_winmargin_vec_at >= 0))
  bra_at_no_of_winmargin_ov1 <- length(which(bra_winmargin_vec_at >= 1))
  bra_at_no_of_winmargin_un0 <- length(which(bra_winmargin_vec_at <= 0))
  bra_at_no_of_winmargin_un1 <- length(which(bra_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  bra_winmargin_vec_ht_lm <- as.vector(bra_winmargin_h[bra_hometeamindex,])
  bra_winmargin_vec_ht_lm[is.na(bra_winmargin_vec_ht_lm)] <- ""
  bra_winmargin_vec_ht_lm <- bra_winmargin_vec_ht_lm[bra_winmargin_vec_ht_lm != ""]
  bra_winmargin_vec_ht_lm  <-tail(bra_winmargin_vec_ht_lm,1)
  #awayteam
  bra_winmargin_vec_at_lm <- as.vector(bra_winmargin_h[bra_awayteamindex,])
  bra_winmargin_vec_at_lm[is.na(bra_winmargin_vec_at_lm)] <- ""
  bra_winmargin_vec_at_lm <- bra_winmargin_vec_at_lm[bra_winmargin_vec_at_lm != ""]
  bra_winmargin_vec_at_lm  <-tail(bra_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  bra_ht_last6points <- bra_ht_numberof_wins*3 + bra_ht_numberof_draws*1
  bra_at_last6points <- bra_at_numberof_wins*3 + bra_at_numberof_draws*1

  if(bra_ht_last6points > bra_at_last6points) {bra_3waypick <- "1"}  else {bra_3waypick <- "X2"}

  if(bra_at_last6points > bra_ht_last6points ) {bra_3waypick <- "2"} else {bra_3waypick <- "1X"}

  if(bra_ht_no_of_ov25 + bra_at_no_of_ov25 >= 6) {bra_goalspick <- "ov25"} else {bra_goalspick <- "un25"}

  if(bra_ht_no_of_un25 + bra_at_no_of_un25 >= 6) {bra_goalspick <- "un25"} else {bra_goalspick <- "ov25"}

  if(bra_ht_matches_scoring >= 4 && bra_at_matches_scoring >=4) {bra_btts <- "BTTS-Y"} else {bra_btts <- "BTTS-N"}


  bra_prediction[bra_row] <- rbind(paste(bra_3waypick,bra_goalspick,bra_btts,sep = ","))
  bra_HWM[bra_row] <- bra_ht_totalwinmargin
  bra_AWM[bra_row] <- bra_at_totalwinmargin

  bra_HWMLM[bra_row] <- bra_winmargin_vec_ht_lm
  bra_AWMLM[bra_row] <- bra_winmargin_vec_at_lm


}

bra_prediction <- as.data.frame(bra_prediction)
colnames(bra_prediction) <- "prediction"

bra_HWM <- as.data.frame(bra_HWM)
colnames(bra_HWM) <- "HWM"

bra_AWM <- as.data.frame(bra_AWM)
colnames(bra_AWM) <- "AWM"

bra_HWMLM <- as.data.frame(bra_HWMLM)
colnames(bra_HWMLM) <- "HWMLM"

bra_AWMLM <- as.data.frame(bra_AWMLM)
colnames(bra_AWMLM) <- "AWMLM"

bra_picks <- cbind(BRA_fixtures$Div,BRA_fixtures$HomeTeam_bra,BRA_fixtures$AwayTeam_bra,bra_prediction,bra_HWM,bra_AWM,bra_HWMLM,bra_AWMLM)
colnames(bra_picks)[1] <- "picks_Div"
colnames(bra_picks)[2] <- "picks_HomeTeam"
colnames(bra_picks)[3] <- "picks_AwayTeam"
bra_picks$matchid <- paste(bra_picks$picks_HomeTeam,bra_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of BRA
bra_picks
############################################################################################
#CHN
CHN_fixtures$Hometeam_chn_index <- match(CHN_fixtures$HomeTeam_chn,chn_teams)
CHN_fixtures$Awayteam_chn_index <- match(CHN_fixtures$AwayTeam_chn,chn_teams)
chn_prediction <- c()
chn_HWM <- c()
chn_AWM <- c()
chn_HWMLM <- c()
chn_AWMLM <- c()
for(chn_row in 1:nrow(CHN_fixtures))
{

  chn_hometeamindex <- CHN_fixtures[chn_row,"Hometeam_chn_index"]
  chn_awayteamindex <- CHN_fixtures[chn_row,"Awayteam_chn_index"]
  #analyse team form
  #home team
  chn_form_vec_ht <- as.vector(chn_form_h[chn_hometeamindex,])
  chn_form_vec_ht[is.na(chn_form_vec_ht)] <- ""
  chn_form_vec_ht <- chn_form_vec_ht[chn_form_vec_ht != ""]
  chn_form_vec_ht  <-tail(chn_form_vec_ht,6)
  chn_ht_numberof_wins <- length(which(chn_form_vec_ht == "W"))
  chn_ht_numberof_draws <- length(which(chn_form_vec_ht == "D"))
  chn_ht_numberof_loss <- length(which(chn_form_vec_ht == "L"))
  #awayteam
  chn_form_vec_at <- as.vector(chn_form_h[chn_awayteamindex,])
  chn_form_vec_at[is.na(chn_form_vec_at)] <- ""
  chn_form_vec_at <- chn_form_vec_at[chn_form_vec_at != ""]
  chn_form_vec_at  <-tail(chn_form_vec_at,6)
  chn_at_numberof_wins <- length(which(chn_form_vec_at == "W"))
  chn_at_numberof_draws <- length(which(chn_form_vec_at == "D"))
  chn_at_numberof_loss <- length(which(chn_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  chn_goalscored_vec_ht <- as.vector(chn_goalscored_h[chn_hometeamindex,])
  chn_goalscored_vec_ht[is.na(chn_goalscored_vec_ht)] <- ""
  chn_goalscored_vec_ht <- chn_goalscored_vec_ht[chn_goalscored_vec_ht != ""]
  chn_goalscored_vec_ht  <-tail(chn_goalscored_vec_ht,6)
  chn_goalscored_vec_ht  <- as.numeric(chn_goalscored_vec_ht)
  chn_ht_totalgoalscored <- sum(chn_goalscored_vec_ht)
  chn_ht_matches_scoring <- length(which(chn_goalscored_vec_ht > 0))
  chn_ht_matches_without_scoring <- length(which(chn_goalscored_vec_ht == "0"))
  #awayteam
  chn_goalscored_vec_at <- as.vector(chn_goalscored_h[chn_awayteamindex,])
  chn_goalscored_vec_at[is.na(chn_goalscored_vec_at)] <- ""
  chn_goalscored_vec_at <- chn_goalscored_vec_at[chn_goalscored_vec_at != ""]
  chn_goalscored_vec_at  <-tail(chn_goalscored_vec_at,6)
  chn_goalscored_vec_at  <- as.numeric(chn_goalscored_vec_at)
  chn_at_totalgoalscored <- sum(chn_goalscored_vec_at)
  chn_at_matches_scoring <- length(which(chn_goalscored_vec_at > 0))
  chn_at_matches_without_scoring <- length(which(chn_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  chn_goalconceded_vec_ht <- as.vector(chn_goalconceded_h[chn_hometeamindex,])
  chn_goalconceded_vec_ht[is.na(chn_goalconceded_vec_ht)] <- ""
  chn_goalconceded_vec_ht <- chn_goalconceded_vec_ht[chn_goalconceded_vec_ht != ""]
  chn_goalconceded_vec_ht  <-tail(chn_goalconceded_vec_ht,6)
  chn_goalconceded_vec_ht  <- as.numeric(chn_goalconceded_vec_ht)
  chn_goalconceded_vec_ht
  chn_ht_totalgoalconceded <- sum(chn_goalconceded_vec_ht)
  chn_ht_matches_concede <- length(which(chn_goalconceded_vec_ht > 0))
  chn_ht_matches_without_concede <- length(which(chn_goalconceded_vec_ht == "0"))
  #awayteam
  chn_goalconceded_vec_at <- as.vector(chn_goalconceded_h[chn_awayteamindex,])
  chn_goalconceded_vec_at[is.na(chn_goalconceded_vec_at)] <- ""
  chn_goalconceded_vec_at <- chn_goalconceded_vec_at[chn_goalconceded_vec_at != ""]
  chn_goalconceded_vec_at  <-tail(chn_goalconceded_vec_at,6)
  chn_goalconceded_vec_at  <- as.numeric(chn_goalconceded_vec_at)
  chn_at_totalgoalconceded <- sum(chn_goalconceded_vec_at)
  chn_at_matches_concede <- length(which(chn_goalconceded_vec_at > 0))
  chn_at_matches_without_concede <- length(which(chn_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  chn_totalgoals_vec_ht <- as.vector(chn_totalgoals_h[chn_hometeamindex,])
  chn_totalgoals_vec_ht[is.na(chn_totalgoals_vec_ht)] <- ""
  chn_totalgoals_vec_ht <- chn_totalgoals_vec_ht[chn_totalgoals_vec_ht != ""]
  chn_totalgoals_vec_ht  <-tail(chn_totalgoals_vec_ht,6)
  chn_totalgoals_vec_ht  <- as.numeric(chn_totalgoals_vec_ht)
  chn_totalgoals_vec_ht
  chn_ht_totalgoals <- sum(chn_totalgoals_vec_ht)
  chn_ht_avgtotalgoals <- (chn_ht_totalgoals/6)
  chn_ht_no_of_ov25 <- length(which(chn_totalgoals_vec_ht >= 3))
  chn_ht_no_of_un25 <- length(which(chn_totalgoals_vec_ht <= 2))
  #awayteam
  chn_totalgoals_vec_at <- as.vector(chn_totalgoals_h[chn_awayteamindex,])
  chn_totalgoals_vec_at[is.na(chn_totalgoals_vec_at)] <- ""
  chn_totalgoals_vec_at <- chn_totalgoals_vec_at[chn_totalgoals_vec_at != ""]
  chn_totalgoals_vec_at  <-tail(chn_totalgoals_vec_at,6)
  chn_totalgoals_vec_at  <- as.numeric(chn_totalgoals_vec_at)
  chn_totalgoals_vec_at
  chn_at_totalgoals <- sum(chn_totalgoals_vec_at)
  chn_at_avgtotalgoals <- (chn_at_totalgoals/6)
  chn_at_no_of_ov25 <- length(which(chn_totalgoals_vec_at >= 3))
  chn_at_no_of_un25 <- length(which(chn_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  chn_winmargin_vec_ht <- as.vector(chn_winmargin_h[chn_hometeamindex,])
  chn_winmargin_vec_ht[is.na(chn_winmargin_vec_ht)] <- ""
  chn_winmargin_vec_ht <- chn_winmargin_vec_ht[chn_winmargin_vec_ht != ""]
  chn_winmargin_vec_ht  <-tail(chn_winmargin_vec_ht,6)
  chn_winmargin_vec_ht  <- as.numeric(chn_winmargin_vec_ht)

  chn_ht_totalwinmargin <- sum(chn_winmargin_vec_ht)
  chn_ht_no_of_winmargin_ov0 <- length(which(chn_winmargin_vec_ht >= 0))
  chn_ht_no_of_winmargin_ov1 <- length(which(chn_winmargin_vec_ht >= 1))
  chn_ht_no_of_winmargin_un0 <- length(which(chn_winmargin_vec_ht <= 0))
  chn_ht_no_of_winmargin_un1 <- length(which(chn_winmargin_vec_ht <= 1))
  #awayteam
  chn_winmargin_vec_at <- as.vector(chn_winmargin_h[chn_awayteamindex,])
  chn_winmargin_vec_at[is.na(chn_winmargin_vec_at)] <- ""
  chn_winmargin_vec_at <- chn_winmargin_vec_at[chn_winmargin_vec_at != ""]
  chn_winmargin_vec_at  <-tail(chn_winmargin_vec_at,6)
  chn_winmargin_vec_at  <- as.numeric(chn_winmargin_vec_at)

  chn_at_totalwinmargin <- sum(chn_winmargin_vec_at)
  chn_at_no_of_winmargin_ov0 <- length(which(chn_winmargin_vec_at >= 0))
  chn_at_no_of_winmargin_ov1 <- length(which(chn_winmargin_vec_at >= 1))
  chn_at_no_of_winmargin_un0 <- length(which(chn_winmargin_vec_at <= 0))
  chn_at_no_of_winmargin_un1 <- length(which(chn_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  chn_winmargin_vec_ht_lm <- as.vector(chn_winmargin_h[chn_hometeamindex,])
  chn_winmargin_vec_ht_lm[is.na(chn_winmargin_vec_ht_lm)] <- ""
  chn_winmargin_vec_ht_lm <- chn_winmargin_vec_ht_lm[chn_winmargin_vec_ht_lm != ""]
  chn_winmargin_vec_ht_lm  <-tail(chn_winmargin_vec_ht_lm,1)
  #awayteam
  chn_winmargin_vec_at_lm <- as.vector(chn_winmargin_h[chn_awayteamindex,])
  chn_winmargin_vec_at_lm[is.na(chn_winmargin_vec_at_lm)] <- ""
  chn_winmargin_vec_at_lm <- chn_winmargin_vec_at_lm[chn_winmargin_vec_at_lm != ""]
  chn_winmargin_vec_at_lm  <-tail(chn_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  chn_ht_last6points <- chn_ht_numberof_wins*3 + chn_ht_numberof_draws*1
  chn_at_last6points <- chn_at_numberof_wins*3 + chn_at_numberof_draws*1

  if(chn_ht_last6points > chn_at_last6points) {chn_3waypick <- "1"}  else {chn_3waypick <- "X2"}

  if(chn_at_last6points > chn_ht_last6points ) {chn_3waypick <- "2"} else {chn_3waypick <- "1X"}

  if(chn_ht_no_of_ov25 + chn_at_no_of_ov25 >= 6) {chn_goalspick <- "ov25"} else {chn_goalspick <- "un25"}

  if(chn_ht_no_of_un25 + chn_at_no_of_un25 >= 6) {chn_goalspick <- "un25"} else {chn_goalspick <- "ov25"}

  if(chn_ht_matches_scoring >= 4 && chn_at_matches_scoring >=4) {chn_btts <- "BTTS-Y"} else {chn_btts <- "BTTS-N"}


  chn_prediction[chn_row] <- rbind(paste(chn_3waypick,chn_goalspick,chn_btts,sep = ","))
  chn_HWM[chn_row] <- chn_ht_totalwinmargin
  chn_AWM[chn_row] <- chn_at_totalwinmargin

  chn_HWMLM[chn_row] <- chn_winmargin_vec_ht_lm
  chn_AWMLM[chn_row] <- chn_winmargin_vec_at_lm


}

chn_prediction <- as.data.frame(chn_prediction)
colnames(chn_prediction) <- "prediction"

chn_HWM <- as.data.frame(chn_HWM)
colnames(chn_HWM) <- "HWM"

chn_AWM <- as.data.frame(chn_AWM)
colnames(chn_AWM) <- "AWM"

chn_HWMLM <- as.data.frame(chn_HWMLM)
colnames(chn_HWMLM) <- "HWMLM"

chn_AWMLM <- as.data.frame(chn_AWMLM)
colnames(chn_AWMLM) <- "AWMLM"

chn_picks <- cbind(CHN_fixtures$Div,CHN_fixtures$HomeTeam_chn,CHN_fixtures$AwayTeam_chn,chn_prediction,chn_HWM,chn_AWM,chn_HWMLM,chn_AWMLM)
colnames(chn_picks)[1] <- "picks_Div"
colnames(chn_picks)[2] <- "picks_HomeTeam"
colnames(chn_picks)[3] <- "picks_AwayTeam"
chn_picks$matchid <- paste(chn_picks$picks_HomeTeam,chn_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of CHN
chn_picks
############################################################################################
#DNK
DNK_fixtures$Hometeam_dnk_index <- match(DNK_fixtures$HomeTeam_dnk,dnk_teams)
DNK_fixtures$Awayteam_dnk_index <- match(DNK_fixtures$AwayTeam_dnk,dnk_teams)
dnk_prediction <- c()
dnk_HWM <- c()
dnk_AWM <- c()
dnk_HWMLM <- c()
dnk_AWMLM <- c()
for(dnk_row in 1:nrow(DNK_fixtures))
{

  dnk_hometeamindex <- DNK_fixtures[dnk_row,"Hometeam_dnk_index"]
  dnk_awayteamindex <- DNK_fixtures[dnk_row,"Awayteam_dnk_index"]
  #analyse team form
  #home team
  dnk_form_vec_ht <- as.vector(dnk_form_h[dnk_hometeamindex,])
  dnk_form_vec_ht[is.na(dnk_form_vec_ht)] <- ""
  dnk_form_vec_ht <- dnk_form_vec_ht[dnk_form_vec_ht != ""]
  dnk_form_vec_ht  <-tail(dnk_form_vec_ht,6)
  dnk_ht_numberof_wins <- length(which(dnk_form_vec_ht == "W"))
  dnk_ht_numberof_draws <- length(which(dnk_form_vec_ht == "D"))
  dnk_ht_numberof_loss <- length(which(dnk_form_vec_ht == "L"))
  #awayteam
  dnk_form_vec_at <- as.vector(dnk_form_h[dnk_awayteamindex,])
  dnk_form_vec_at[is.na(dnk_form_vec_at)] <- ""
  dnk_form_vec_at <- dnk_form_vec_at[dnk_form_vec_at != ""]
  dnk_form_vec_at  <-tail(dnk_form_vec_at,6)
  dnk_at_numberof_wins <- length(which(dnk_form_vec_at == "W"))
  dnk_at_numberof_draws <- length(which(dnk_form_vec_at == "D"))
  dnk_at_numberof_loss <- length(which(dnk_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  dnk_goalscored_vec_ht <- as.vector(dnk_goalscored_h[dnk_hometeamindex,])
  dnk_goalscored_vec_ht[is.na(dnk_goalscored_vec_ht)] <- ""
  dnk_goalscored_vec_ht <- dnk_goalscored_vec_ht[dnk_goalscored_vec_ht != ""]
  dnk_goalscored_vec_ht  <-tail(dnk_goalscored_vec_ht,6)
  dnk_goalscored_vec_ht  <- as.numeric(dnk_goalscored_vec_ht)
  dnk_ht_totalgoalscored <- sum(dnk_goalscored_vec_ht)
  dnk_ht_matches_scoring <- length(which(dnk_goalscored_vec_ht > 0))
  dnk_ht_matches_without_scoring <- length(which(dnk_goalscored_vec_ht == "0"))
  #awayteam
  dnk_goalscored_vec_at <- as.vector(dnk_goalscored_h[dnk_awayteamindex,])
  dnk_goalscored_vec_at[is.na(dnk_goalscored_vec_at)] <- ""
  dnk_goalscored_vec_at <- dnk_goalscored_vec_at[dnk_goalscored_vec_at != ""]
  dnk_goalscored_vec_at  <-tail(dnk_goalscored_vec_at,6)
  dnk_goalscored_vec_at  <- as.numeric(dnk_goalscored_vec_at)
  dnk_at_totalgoalscored <- sum(dnk_goalscored_vec_at)
  dnk_at_matches_scoring <- length(which(dnk_goalscored_vec_at > 0))
  dnk_at_matches_without_scoring <- length(which(dnk_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  dnk_goalconceded_vec_ht <- as.vector(dnk_goalconceded_h[dnk_hometeamindex,])
  dnk_goalconceded_vec_ht[is.na(dnk_goalconceded_vec_ht)] <- ""
  dnk_goalconceded_vec_ht <- dnk_goalconceded_vec_ht[dnk_goalconceded_vec_ht != ""]
  dnk_goalconceded_vec_ht  <-tail(dnk_goalconceded_vec_ht,6)
  dnk_goalconceded_vec_ht  <- as.numeric(dnk_goalconceded_vec_ht)
  dnk_goalconceded_vec_ht
  dnk_ht_totalgoalconceded <- sum(dnk_goalconceded_vec_ht)
  dnk_ht_matches_concede <- length(which(dnk_goalconceded_vec_ht > 0))
  dnk_ht_matches_without_concede <- length(which(dnk_goalconceded_vec_ht == "0"))
  #awayteam
  dnk_goalconceded_vec_at <- as.vector(dnk_goalconceded_h[dnk_awayteamindex,])
  dnk_goalconceded_vec_at[is.na(dnk_goalconceded_vec_at)] <- ""
  dnk_goalconceded_vec_at <- dnk_goalconceded_vec_at[dnk_goalconceded_vec_at != ""]
  dnk_goalconceded_vec_at  <-tail(dnk_goalconceded_vec_at,6)
  dnk_goalconceded_vec_at  <- as.numeric(dnk_goalconceded_vec_at)
  dnk_at_totalgoalconceded <- sum(dnk_goalconceded_vec_at)
  dnk_at_matches_concede <- length(which(dnk_goalconceded_vec_at > 0))
  dnk_at_matches_without_concede <- length(which(dnk_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  dnk_totalgoals_vec_ht <- as.vector(dnk_totalgoals_h[dnk_hometeamindex,])
  dnk_totalgoals_vec_ht[is.na(dnk_totalgoals_vec_ht)] <- ""
  dnk_totalgoals_vec_ht <- dnk_totalgoals_vec_ht[dnk_totalgoals_vec_ht != ""]
  dnk_totalgoals_vec_ht  <-tail(dnk_totalgoals_vec_ht,6)
  dnk_totalgoals_vec_ht  <- as.numeric(dnk_totalgoals_vec_ht)
  dnk_totalgoals_vec_ht
  dnk_ht_totalgoals <- sum(dnk_totalgoals_vec_ht)
  dnk_ht_avgtotalgoals <- (dnk_ht_totalgoals/6)
  dnk_ht_no_of_ov25 <- length(which(dnk_totalgoals_vec_ht >= 3))
  dnk_ht_no_of_un25 <- length(which(dnk_totalgoals_vec_ht <= 2))
  #awayteam
  dnk_totalgoals_vec_at <- as.vector(dnk_totalgoals_h[dnk_awayteamindex,])
  dnk_totalgoals_vec_at[is.na(dnk_totalgoals_vec_at)] <- ""
  dnk_totalgoals_vec_at <- dnk_totalgoals_vec_at[dnk_totalgoals_vec_at != ""]
  dnk_totalgoals_vec_at  <-tail(dnk_totalgoals_vec_at,6)
  dnk_totalgoals_vec_at  <- as.numeric(dnk_totalgoals_vec_at)
  dnk_totalgoals_vec_at
  dnk_at_totalgoals <- sum(dnk_totalgoals_vec_at)
  dnk_at_avgtotalgoals <- (dnk_at_totalgoals/6)
  dnk_at_no_of_ov25 <- length(which(dnk_totalgoals_vec_at >= 3))
  dnk_at_no_of_un25 <- length(which(dnk_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  dnk_winmargin_vec_ht <- as.vector(dnk_winmargin_h[dnk_hometeamindex,])
  dnk_winmargin_vec_ht[is.na(dnk_winmargin_vec_ht)] <- ""
  dnk_winmargin_vec_ht <- dnk_winmargin_vec_ht[dnk_winmargin_vec_ht != ""]
  dnk_winmargin_vec_ht  <-tail(dnk_winmargin_vec_ht,6)
  dnk_winmargin_vec_ht  <- as.numeric(dnk_winmargin_vec_ht)

  dnk_ht_totalwinmargin <- sum(dnk_winmargin_vec_ht)
  dnk_ht_no_of_winmargin_ov0 <- length(which(dnk_winmargin_vec_ht >= 0))
  dnk_ht_no_of_winmargin_ov1 <- length(which(dnk_winmargin_vec_ht >= 1))
  dnk_ht_no_of_winmargin_un0 <- length(which(dnk_winmargin_vec_ht <= 0))
  dnk_ht_no_of_winmargin_un1 <- length(which(dnk_winmargin_vec_ht <= 1))
  #awayteam
  dnk_winmargin_vec_at <- as.vector(dnk_winmargin_h[dnk_awayteamindex,])
  dnk_winmargin_vec_at[is.na(dnk_winmargin_vec_at)] <- ""
  dnk_winmargin_vec_at <- dnk_winmargin_vec_at[dnk_winmargin_vec_at != ""]
  dnk_winmargin_vec_at  <-tail(dnk_winmargin_vec_at,6)
  dnk_winmargin_vec_at  <- as.numeric(dnk_winmargin_vec_at)

  dnk_at_totalwinmargin <- sum(dnk_winmargin_vec_at)
  dnk_at_no_of_winmargin_ov0 <- length(which(dnk_winmargin_vec_at >= 0))
  dnk_at_no_of_winmargin_ov1 <- length(which(dnk_winmargin_vec_at >= 1))
  dnk_at_no_of_winmargin_un0 <- length(which(dnk_winmargin_vec_at <= 0))
  dnk_at_no_of_winmargin_un1 <- length(which(dnk_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  dnk_winmargin_vec_ht_lm <- as.vector(dnk_winmargin_h[dnk_hometeamindex,])
  dnk_winmargin_vec_ht_lm[is.na(dnk_winmargin_vec_ht_lm)] <- ""
  dnk_winmargin_vec_ht_lm <- dnk_winmargin_vec_ht_lm[dnk_winmargin_vec_ht_lm != ""]
  dnk_winmargin_vec_ht_lm  <-tail(dnk_winmargin_vec_ht_lm,1)
  #awayteam
  dnk_winmargin_vec_at_lm <- as.vector(dnk_winmargin_h[dnk_awayteamindex,])
  dnk_winmargin_vec_at_lm[is.na(dnk_winmargin_vec_at_lm)] <- ""
  dnk_winmargin_vec_at_lm <- dnk_winmargin_vec_at_lm[dnk_winmargin_vec_at_lm != ""]
  dnk_winmargin_vec_at_lm  <-tail(dnk_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  dnk_ht_last6points <- dnk_ht_numberof_wins*3 + dnk_ht_numberof_draws*1
  dnk_at_last6points <- dnk_at_numberof_wins*3 + dnk_at_numberof_draws*1

  if(dnk_ht_last6points > dnk_at_last6points) {dnk_3waypick <- "1"}  else {dnk_3waypick <- "X2"}

  if(dnk_at_last6points > dnk_ht_last6points ) {dnk_3waypick <- "2"} else {dnk_3waypick <- "1X"}

  if(dnk_ht_no_of_ov25 + dnk_at_no_of_ov25 >= 6) {dnk_goalspick <- "ov25"} else {dnk_goalspick <- "un25"}

  if(dnk_ht_no_of_un25 + dnk_at_no_of_un25 >= 6) {dnk_goalspick <- "un25"} else {dnk_goalspick <- "ov25"}

  if(dnk_ht_matches_scoring >= 4 && dnk_at_matches_scoring >=4) {dnk_btts <- "BTTS-Y"} else {dnk_btts <- "BTTS-N"}


  dnk_prediction[dnk_row] <- rbind(paste(dnk_3waypick,dnk_goalspick,dnk_btts,sep = ","))
  dnk_HWM[dnk_row] <- dnk_ht_totalwinmargin
  dnk_AWM[dnk_row] <- dnk_at_totalwinmargin

  dnk_HWMLM[dnk_row] <- dnk_winmargin_vec_ht_lm
  dnk_AWMLM[dnk_row] <- dnk_winmargin_vec_at_lm


}

dnk_prediction <- as.data.frame(dnk_prediction)
colnames(dnk_prediction) <- "prediction"

dnk_HWM <- as.data.frame(dnk_HWM)
colnames(dnk_HWM) <- "HWM"

dnk_AWM <- as.data.frame(dnk_AWM)
colnames(dnk_AWM) <- "AWM"

dnk_HWMLM <- as.data.frame(dnk_HWMLM)
colnames(dnk_HWMLM) <- "HWMLM"

dnk_AWMLM <- as.data.frame(dnk_AWMLM)
colnames(dnk_AWMLM) <- "AWMLM"

dnk_picks <- cbind(DNK_fixtures$Div,DNK_fixtures$HomeTeam_dnk,DNK_fixtures$AwayTeam_dnk,dnk_prediction,dnk_HWM,dnk_AWM,dnk_HWMLM,dnk_AWMLM)
colnames(dnk_picks)[1] <- "picks_Div"
colnames(dnk_picks)[2] <- "picks_HomeTeam"
colnames(dnk_picks)[3] <- "picks_AwayTeam"
dnk_picks$matchid <- paste(dnk_picks$picks_HomeTeam,dnk_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of DNK
dnk_picks
############################################################################################
#FIN
FIN_fixtures$Hometeam_fin_index <- match(FIN_fixtures$HomeTeam_fin,fin_teams)
FIN_fixtures$Awayteam_fin_index <- match(FIN_fixtures$AwayTeam_fin,fin_teams)
fin_prediction <- c()
fin_HWM <- c()
fin_AWM <- c()
fin_HWMLM <- c()
fin_AWMLM <- c()
for(fin_row in 1:nrow(FIN_fixtures))
{

  fin_hometeamindex <- FIN_fixtures[fin_row,"Hometeam_fin_index"]
  fin_awayteamindex <- FIN_fixtures[fin_row,"Awayteam_fin_index"]
  #analyse team form
  #home team
  fin_form_vec_ht <- as.vector(fin_form_h[fin_hometeamindex,])
  fin_form_vec_ht[is.na(fin_form_vec_ht)] <- ""
  fin_form_vec_ht <- fin_form_vec_ht[fin_form_vec_ht != ""]
  fin_form_vec_ht  <-tail(fin_form_vec_ht,6)
  fin_ht_numberof_wins <- length(which(fin_form_vec_ht == "W"))
  fin_ht_numberof_draws <- length(which(fin_form_vec_ht == "D"))
  fin_ht_numberof_loss <- length(which(fin_form_vec_ht == "L"))
  #awayteam
  fin_form_vec_at <- as.vector(fin_form_h[fin_awayteamindex,])
  fin_form_vec_at[is.na(fin_form_vec_at)] <- ""
  fin_form_vec_at <- fin_form_vec_at[fin_form_vec_at != ""]
  fin_form_vec_at  <-tail(fin_form_vec_at,6)
  fin_at_numberof_wins <- length(which(fin_form_vec_at == "W"))
  fin_at_numberof_draws <- length(which(fin_form_vec_at == "D"))
  fin_at_numberof_loss <- length(which(fin_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  fin_goalscored_vec_ht <- as.vector(fin_goalscored_h[fin_hometeamindex,])
  fin_goalscored_vec_ht[is.na(fin_goalscored_vec_ht)] <- ""
  fin_goalscored_vec_ht <- fin_goalscored_vec_ht[fin_goalscored_vec_ht != ""]
  fin_goalscored_vec_ht  <-tail(fin_goalscored_vec_ht,6)
  fin_goalscored_vec_ht  <- as.numeric(fin_goalscored_vec_ht)
  fin_ht_totalgoalscored <- sum(fin_goalscored_vec_ht)
  fin_ht_matches_scoring <- length(which(fin_goalscored_vec_ht > 0))
  fin_ht_matches_without_scoring <- length(which(fin_goalscored_vec_ht == "0"))
  #awayteam
  fin_goalscored_vec_at <- as.vector(fin_goalscored_h[fin_awayteamindex,])
  fin_goalscored_vec_at[is.na(fin_goalscored_vec_at)] <- ""
  fin_goalscored_vec_at <- fin_goalscored_vec_at[fin_goalscored_vec_at != ""]
  fin_goalscored_vec_at  <-tail(fin_goalscored_vec_at,6)
  fin_goalscored_vec_at  <- as.numeric(fin_goalscored_vec_at)
  fin_at_totalgoalscored <- sum(fin_goalscored_vec_at)
  fin_at_matches_scoring <- length(which(fin_goalscored_vec_at > 0))
  fin_at_matches_without_scoring <- length(which(fin_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  fin_goalconceded_vec_ht <- as.vector(fin_goalconceded_h[fin_hometeamindex,])
  fin_goalconceded_vec_ht[is.na(fin_goalconceded_vec_ht)] <- ""
  fin_goalconceded_vec_ht <- fin_goalconceded_vec_ht[fin_goalconceded_vec_ht != ""]
  fin_goalconceded_vec_ht  <-tail(fin_goalconceded_vec_ht,6)
  fin_goalconceded_vec_ht  <- as.numeric(fin_goalconceded_vec_ht)
  fin_goalconceded_vec_ht
  fin_ht_totalgoalconceded <- sum(fin_goalconceded_vec_ht)
  fin_ht_matches_concede <- length(which(fin_goalconceded_vec_ht > 0))
  fin_ht_matches_without_concede <- length(which(fin_goalconceded_vec_ht == "0"))
  #awayteam
  fin_goalconceded_vec_at <- as.vector(fin_goalconceded_h[fin_awayteamindex,])
  fin_goalconceded_vec_at[is.na(fin_goalconceded_vec_at)] <- ""
  fin_goalconceded_vec_at <- fin_goalconceded_vec_at[fin_goalconceded_vec_at != ""]
  fin_goalconceded_vec_at  <-tail(fin_goalconceded_vec_at,6)
  fin_goalconceded_vec_at  <- as.numeric(fin_goalconceded_vec_at)
  fin_at_totalgoalconceded <- sum(fin_goalconceded_vec_at)
  fin_at_matches_concede <- length(which(fin_goalconceded_vec_at > 0))
  fin_at_matches_without_concede <- length(which(fin_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  fin_totalgoals_vec_ht <- as.vector(fin_totalgoals_h[fin_hometeamindex,])
  fin_totalgoals_vec_ht[is.na(fin_totalgoals_vec_ht)] <- ""
  fin_totalgoals_vec_ht <- fin_totalgoals_vec_ht[fin_totalgoals_vec_ht != ""]
  fin_totalgoals_vec_ht  <-tail(fin_totalgoals_vec_ht,6)
  fin_totalgoals_vec_ht  <- as.numeric(fin_totalgoals_vec_ht)
  fin_totalgoals_vec_ht
  fin_ht_totalgoals <- sum(fin_totalgoals_vec_ht)
  fin_ht_avgtotalgoals <- (fin_ht_totalgoals/6)
  fin_ht_no_of_ov25 <- length(which(fin_totalgoals_vec_ht >= 3))
  fin_ht_no_of_un25 <- length(which(fin_totalgoals_vec_ht <= 2))
  #awayteam
  fin_totalgoals_vec_at <- as.vector(fin_totalgoals_h[fin_awayteamindex,])
  fin_totalgoals_vec_at[is.na(fin_totalgoals_vec_at)] <- ""
  fin_totalgoals_vec_at <- fin_totalgoals_vec_at[fin_totalgoals_vec_at != ""]
  fin_totalgoals_vec_at  <-tail(fin_totalgoals_vec_at,6)
  fin_totalgoals_vec_at  <- as.numeric(fin_totalgoals_vec_at)
  fin_totalgoals_vec_at
  fin_at_totalgoals <- sum(fin_totalgoals_vec_at)
  fin_at_avgtotalgoals <- (fin_at_totalgoals/6)
  fin_at_no_of_ov25 <- length(which(fin_totalgoals_vec_at >= 3))
  fin_at_no_of_un25 <- length(which(fin_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  fin_winmargin_vec_ht <- as.vector(fin_winmargin_h[fin_hometeamindex,])
  fin_winmargin_vec_ht[is.na(fin_winmargin_vec_ht)] <- ""
  fin_winmargin_vec_ht <- fin_winmargin_vec_ht[fin_winmargin_vec_ht != ""]
  fin_winmargin_vec_ht  <-tail(fin_winmargin_vec_ht,6)
  fin_winmargin_vec_ht  <- as.numeric(fin_winmargin_vec_ht)

  fin_ht_totalwinmargin <- sum(fin_winmargin_vec_ht)
  fin_ht_no_of_winmargin_ov0 <- length(which(fin_winmargin_vec_ht >= 0))
  fin_ht_no_of_winmargin_ov1 <- length(which(fin_winmargin_vec_ht >= 1))
  fin_ht_no_of_winmargin_un0 <- length(which(fin_winmargin_vec_ht <= 0))
  fin_ht_no_of_winmargin_un1 <- length(which(fin_winmargin_vec_ht <= 1))
  #awayteam
  fin_winmargin_vec_at <- as.vector(fin_winmargin_h[fin_awayteamindex,])
  fin_winmargin_vec_at[is.na(fin_winmargin_vec_at)] <- ""
  fin_winmargin_vec_at <- fin_winmargin_vec_at[fin_winmargin_vec_at != ""]
  fin_winmargin_vec_at  <-tail(fin_winmargin_vec_at,6)
  fin_winmargin_vec_at  <- as.numeric(fin_winmargin_vec_at)

  fin_at_totalwinmargin <- sum(fin_winmargin_vec_at)
  fin_at_no_of_winmargin_ov0 <- length(which(fin_winmargin_vec_at >= 0))
  fin_at_no_of_winmargin_ov1 <- length(which(fin_winmargin_vec_at >= 1))
  fin_at_no_of_winmargin_un0 <- length(which(fin_winmargin_vec_at <= 0))
  fin_at_no_of_winmargin_un1 <- length(which(fin_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  fin_winmargin_vec_ht_lm <- as.vector(fin_winmargin_h[fin_hometeamindex,])
  fin_winmargin_vec_ht_lm[is.na(fin_winmargin_vec_ht_lm)] <- ""
  fin_winmargin_vec_ht_lm <- fin_winmargin_vec_ht_lm[fin_winmargin_vec_ht_lm != ""]
  fin_winmargin_vec_ht_lm  <-tail(fin_winmargin_vec_ht_lm,1)
  #awayteam
  fin_winmargin_vec_at_lm <- as.vector(fin_winmargin_h[fin_awayteamindex,])
  fin_winmargin_vec_at_lm[is.na(fin_winmargin_vec_at_lm)] <- ""
  fin_winmargin_vec_at_lm <- fin_winmargin_vec_at_lm[fin_winmargin_vec_at_lm != ""]
  fin_winmargin_vec_at_lm  <-tail(fin_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  fin_ht_last6points <- fin_ht_numberof_wins*3 + fin_ht_numberof_draws*1
  fin_at_last6points <- fin_at_numberof_wins*3 + fin_at_numberof_draws*1

  if(fin_ht_last6points > fin_at_last6points) {fin_3waypick <- "1"}  else {fin_3waypick <- "X2"}

  if(fin_at_last6points > fin_ht_last6points ) {fin_3waypick <- "2"} else {fin_3waypick <- "1X"}

  if(fin_ht_no_of_ov25 + fin_at_no_of_ov25 >= 6) {fin_goalspick <- "ov25"} else {fin_goalspick <- "un25"}

  if(fin_ht_no_of_un25 + fin_at_no_of_un25 >= 6) {fin_goalspick <- "un25"} else {fin_goalspick <- "ov25"}

  if(fin_ht_matches_scoring >= 4 && fin_at_matches_scoring >=4) {fin_btts <- "BTTS-Y"} else {fin_btts <- "BTTS-N"}


  fin_prediction[fin_row] <- rbind(paste(fin_3waypick,fin_goalspick,fin_btts,sep = ","))
  fin_HWM[fin_row] <- fin_ht_totalwinmargin
  fin_AWM[fin_row] <- fin_at_totalwinmargin

  fin_HWMLM[fin_row] <- fin_winmargin_vec_ht_lm
  fin_AWMLM[fin_row] <- fin_winmargin_vec_at_lm


}

fin_prediction <- as.data.frame(fin_prediction)
colnames(fin_prediction) <- "prediction"

fin_HWM <- as.data.frame(fin_HWM)
colnames(fin_HWM) <- "HWM"

fin_AWM <- as.data.frame(fin_AWM)
colnames(fin_AWM) <- "AWM"

fin_HWMLM <- as.data.frame(fin_HWMLM)
colnames(fin_HWMLM) <- "HWMLM"

fin_AWMLM <- as.data.frame(fin_AWMLM)
colnames(fin_AWMLM) <- "AWMLM"

fin_picks <- cbind(FIN_fixtures$Div,FIN_fixtures$HomeTeam_fin,FIN_fixtures$AwayTeam_fin,fin_prediction,fin_HWM,fin_AWM,fin_HWMLM,fin_AWMLM)
colnames(fin_picks)[1] <- "picks_Div"
colnames(fin_picks)[2] <- "picks_HomeTeam"
colnames(fin_picks)[3] <- "picks_AwayTeam"
fin_picks$matchid <- paste(fin_picks$picks_HomeTeam,fin_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of FIN
fin_picks
############################################################################################
#end of FIN
#IRL
IRL_fixtures$Hometeam_irl_index <- match(IRL_fixtures$HomeTeam_irl,irl_teams)
IRL_fixtures$Awayteam_irl_index <- match(IRL_fixtures$AwayTeam_irl,irl_teams)
irl_prediction <- c()
irl_HWM <- c()
irl_AWM <- c()
irl_HWMLM <- c()
irl_AWMLM <- c()
for(irl_row in 1:nrow(IRL_fixtures))
{

  irl_hometeamindex <- IRL_fixtures[irl_row,"Hometeam_irl_index"]
  irl_awayteamindex <- IRL_fixtures[irl_row,"Awayteam_irl_index"]
  #analyse team form
  #home team
  irl_form_vec_ht <- as.vector(irl_form_h[irl_hometeamindex,])
  irl_form_vec_ht[is.na(irl_form_vec_ht)] <- ""
  irl_form_vec_ht <- irl_form_vec_ht[irl_form_vec_ht != ""]
  irl_form_vec_ht  <-tail(irl_form_vec_ht,6)
  irl_ht_numberof_wins <- length(which(irl_form_vec_ht == "W"))
  irl_ht_numberof_draws <- length(which(irl_form_vec_ht == "D"))
  irl_ht_numberof_loss <- length(which(irl_form_vec_ht == "L"))
  #awayteam
  irl_form_vec_at <- as.vector(irl_form_h[irl_awayteamindex,])
  irl_form_vec_at[is.na(irl_form_vec_at)] <- ""
  irl_form_vec_at <- irl_form_vec_at[irl_form_vec_at != ""]
  irl_form_vec_at  <-tail(irl_form_vec_at,6)
  irl_at_numberof_wins <- length(which(irl_form_vec_at == "W"))
  irl_at_numberof_draws <- length(which(irl_form_vec_at == "D"))
  irl_at_numberof_loss <- length(which(irl_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  irl_goalscored_vec_ht <- as.vector(irl_goalscored_h[irl_hometeamindex,])
  irl_goalscored_vec_ht[is.na(irl_goalscored_vec_ht)] <- ""
  irl_goalscored_vec_ht <- irl_goalscored_vec_ht[irl_goalscored_vec_ht != ""]
  irl_goalscored_vec_ht  <-tail(irl_goalscored_vec_ht,6)
  irl_goalscored_vec_ht  <- as.numeric(irl_goalscored_vec_ht)
  irl_ht_totalgoalscored <- sum(irl_goalscored_vec_ht)
  irl_ht_matches_scoring <- length(which(irl_goalscored_vec_ht > 0))
  irl_ht_matches_without_scoring <- length(which(irl_goalscored_vec_ht == "0"))
  #awayteam
  irl_goalscored_vec_at <- as.vector(irl_goalscored_h[irl_awayteamindex,])
  irl_goalscored_vec_at[is.na(irl_goalscored_vec_at)] <- ""
  irl_goalscored_vec_at <- irl_goalscored_vec_at[irl_goalscored_vec_at != ""]
  irl_goalscored_vec_at  <-tail(irl_goalscored_vec_at,6)
  irl_goalscored_vec_at  <- as.numeric(irl_goalscored_vec_at)
  irl_at_totalgoalscored <- sum(irl_goalscored_vec_at)
  irl_at_matches_scoring <- length(which(irl_goalscored_vec_at > 0))
  irl_at_matches_without_scoring <- length(which(irl_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  irl_goalconceded_vec_ht <- as.vector(irl_goalconceded_h[irl_hometeamindex,])
  irl_goalconceded_vec_ht[is.na(irl_goalconceded_vec_ht)] <- ""
  irl_goalconceded_vec_ht <- irl_goalconceded_vec_ht[irl_goalconceded_vec_ht != ""]
  irl_goalconceded_vec_ht  <-tail(irl_goalconceded_vec_ht,6)
  irl_goalconceded_vec_ht  <- as.numeric(irl_goalconceded_vec_ht)
  irl_goalconceded_vec_ht
  irl_ht_totalgoalconceded <- sum(irl_goalconceded_vec_ht)
  irl_ht_matches_concede <- length(which(irl_goalconceded_vec_ht > 0))
  irl_ht_matches_without_concede <- length(which(irl_goalconceded_vec_ht == "0"))
  #awayteam
  irl_goalconceded_vec_at <- as.vector(irl_goalconceded_h[irl_awayteamindex,])
  irl_goalconceded_vec_at[is.na(irl_goalconceded_vec_at)] <- ""
  irl_goalconceded_vec_at <- irl_goalconceded_vec_at[irl_goalconceded_vec_at != ""]
  irl_goalconceded_vec_at  <-tail(irl_goalconceded_vec_at,6)
  irl_goalconceded_vec_at  <- as.numeric(irl_goalconceded_vec_at)
  irl_at_totalgoalconceded <- sum(irl_goalconceded_vec_at)
  irl_at_matches_concede <- length(which(irl_goalconceded_vec_at > 0))
  irl_at_matches_without_concede <- length(which(irl_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  irl_totalgoals_vec_ht <- as.vector(irl_totalgoals_h[irl_hometeamindex,])
  irl_totalgoals_vec_ht[is.na(irl_totalgoals_vec_ht)] <- ""
  irl_totalgoals_vec_ht <- irl_totalgoals_vec_ht[irl_totalgoals_vec_ht != ""]
  irl_totalgoals_vec_ht  <-tail(irl_totalgoals_vec_ht,6)
  irl_totalgoals_vec_ht  <- as.numeric(irl_totalgoals_vec_ht)
  irl_totalgoals_vec_ht
  irl_ht_totalgoals <- sum(irl_totalgoals_vec_ht)
  irl_ht_avgtotalgoals <- (irl_ht_totalgoals/6)
  irl_ht_no_of_ov25 <- length(which(irl_totalgoals_vec_ht >= 3))
  irl_ht_no_of_un25 <- length(which(irl_totalgoals_vec_ht <= 2))
  #awayteam
  irl_totalgoals_vec_at <- as.vector(irl_totalgoals_h[irl_awayteamindex,])
  irl_totalgoals_vec_at[is.na(irl_totalgoals_vec_at)] <- ""
  irl_totalgoals_vec_at <- irl_totalgoals_vec_at[irl_totalgoals_vec_at != ""]
  irl_totalgoals_vec_at  <-tail(irl_totalgoals_vec_at,6)
  irl_totalgoals_vec_at  <- as.numeric(irl_totalgoals_vec_at)
  irl_totalgoals_vec_at
  irl_at_totalgoals <- sum(irl_totalgoals_vec_at)
  irl_at_avgtotalgoals <- (irl_at_totalgoals/6)
  irl_at_no_of_ov25 <- length(which(irl_totalgoals_vec_at >= 3))
  irl_at_no_of_un25 <- length(which(irl_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  irl_winmargin_vec_ht <- as.vector(irl_winmargin_h[irl_hometeamindex,])
  irl_winmargin_vec_ht[is.na(irl_winmargin_vec_ht)] <- ""
  irl_winmargin_vec_ht <- irl_winmargin_vec_ht[irl_winmargin_vec_ht != ""]
  irl_winmargin_vec_ht  <-tail(irl_winmargin_vec_ht,6)
  irl_winmargin_vec_ht  <- as.numeric(irl_winmargin_vec_ht)

  irl_ht_totalwinmargin <- sum(irl_winmargin_vec_ht)
  irl_ht_no_of_winmargin_ov0 <- length(which(irl_winmargin_vec_ht >= 0))
  irl_ht_no_of_winmargin_ov1 <- length(which(irl_winmargin_vec_ht >= 1))
  irl_ht_no_of_winmargin_un0 <- length(which(irl_winmargin_vec_ht <= 0))
  irl_ht_no_of_winmargin_un1 <- length(which(irl_winmargin_vec_ht <= 1))
  #awayteam
  irl_winmargin_vec_at <- as.vector(irl_winmargin_h[irl_awayteamindex,])
  irl_winmargin_vec_at[is.na(irl_winmargin_vec_at)] <- ""
  irl_winmargin_vec_at <- irl_winmargin_vec_at[irl_winmargin_vec_at != ""]
  irl_winmargin_vec_at  <-tail(irl_winmargin_vec_at,6)
  irl_winmargin_vec_at  <- as.numeric(irl_winmargin_vec_at)

  irl_at_totalwinmargin <- sum(irl_winmargin_vec_at)
  irl_at_no_of_winmargin_ov0 <- length(which(irl_winmargin_vec_at >= 0))
  irl_at_no_of_winmargin_ov1 <- length(which(irl_winmargin_vec_at >= 1))
  irl_at_no_of_winmargin_un0 <- length(which(irl_winmargin_vec_at <= 0))
  irl_at_no_of_winmargin_un1 <- length(which(irl_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  irl_winmargin_vec_ht_lm <- as.vector(irl_winmargin_h[irl_hometeamindex,])
  irl_winmargin_vec_ht_lm[is.na(irl_winmargin_vec_ht_lm)] <- ""
  irl_winmargin_vec_ht_lm <- irl_winmargin_vec_ht_lm[irl_winmargin_vec_ht_lm != ""]
  irl_winmargin_vec_ht_lm  <-tail(irl_winmargin_vec_ht_lm,1)
  #awayteam
  irl_winmargin_vec_at_lm <- as.vector(irl_winmargin_h[irl_awayteamindex,])
  irl_winmargin_vec_at_lm[is.na(irl_winmargin_vec_at_lm)] <- ""
  irl_winmargin_vec_at_lm <- irl_winmargin_vec_at_lm[irl_winmargin_vec_at_lm != ""]
  irl_winmargin_vec_at_lm  <-tail(irl_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  irl_ht_last6points <- irl_ht_numberof_wins*3 + irl_ht_numberof_draws*1
  irl_at_last6points <- irl_at_numberof_wins*3 + irl_at_numberof_draws*1

  if(irl_ht_last6points > irl_at_last6points) {irl_3waypick <- "1"}  else {irl_3waypick <- "X2"}

  if(irl_at_last6points > irl_ht_last6points ) {irl_3waypick <- "2"} else {irl_3waypick <- "1X"}

  if(irl_ht_no_of_ov25 + irl_at_no_of_ov25 >= 6) {irl_goalspick <- "ov25"} else {irl_goalspick <- "un25"}

  if(irl_ht_no_of_un25 + irl_at_no_of_un25 >= 6) {irl_goalspick <- "un25"} else {irl_goalspick <- "ov25"}

  if(irl_ht_matches_scoring >= 4 && irl_at_matches_scoring >=4) {irl_btts <- "BTTS-Y"} else {irl_btts <- "BTTS-N"}


  irl_prediction[irl_row] <- rbind(paste(irl_3waypick,irl_goalspick,irl_btts,sep = ","))
  irl_HWM[irl_row] <- irl_ht_totalwinmargin
  irl_AWM[irl_row] <- irl_at_totalwinmargin

  irl_HWMLM[irl_row] <- irl_winmargin_vec_ht_lm
  irl_AWMLM[irl_row] <- irl_winmargin_vec_at_lm


}

irl_prediction <- as.data.frame(irl_prediction)
colnames(irl_prediction) <- "prediction"

irl_HWM <- as.data.frame(irl_HWM)
colnames(irl_HWM) <- "HWM"

irl_AWM <- as.data.frame(irl_AWM)
colnames(irl_AWM) <- "AWM"

irl_HWMLM <- as.data.frame(irl_HWMLM)
colnames(irl_HWMLM) <- "HWMLM"

irl_AWMLM <- as.data.frame(irl_AWMLM)
colnames(irl_AWMLM) <- "AWMLM"

irl_picks <- cbind(IRL_fixtures$Div,IRL_fixtures$HomeTeam_irl,IRL_fixtures$AwayTeam_irl,irl_prediction,irl_HWM,irl_AWM,irl_HWMLM,irl_AWMLM)
colnames(irl_picks)[1] <- "picks_Div"
colnames(irl_picks)[2] <- "picks_HomeTeam"
colnames(irl_picks)[3] <- "picks_AwayTeam"
irl_picks$matchid <- paste(irl_picks$picks_HomeTeam,irl_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of IRL
irl_picks
############################################################################################
#end of irl
#JPN
JPN_fixtures$Hometeam_jpn_index <- match(JPN_fixtures$HomeTeam_jpn,jpn_teams)
JPN_fixtures$Awayteam_jpn_index <- match(JPN_fixtures$AwayTeam_jpn,jpn_teams)
jpn_prediction <- c()
jpn_HWM <- c()
jpn_AWM <- c()
jpn_HWMLM <- c()
jpn_AWMLM <- c()
for(jpn_row in 1:nrow(JPN_fixtures))
{

  jpn_hometeamindex <- JPN_fixtures[jpn_row,"Hometeam_jpn_index"]
  jpn_awayteamindex <- JPN_fixtures[jpn_row,"Awayteam_jpn_index"]
  #analyse team form
  #home team
  jpn_form_vec_ht <- as.vector(jpn_form_h[jpn_hometeamindex,])
  jpn_form_vec_ht[is.na(jpn_form_vec_ht)] <- ""
  jpn_form_vec_ht <- jpn_form_vec_ht[jpn_form_vec_ht != ""]
  jpn_form_vec_ht  <-tail(jpn_form_vec_ht,6)
  jpn_ht_numberof_wins <- length(which(jpn_form_vec_ht == "W"))
  jpn_ht_numberof_draws <- length(which(jpn_form_vec_ht == "D"))
  jpn_ht_numberof_loss <- length(which(jpn_form_vec_ht == "L"))
  #awayteam
  jpn_form_vec_at <- as.vector(jpn_form_h[jpn_awayteamindex,])
  jpn_form_vec_at[is.na(jpn_form_vec_at)] <- ""
  jpn_form_vec_at <- jpn_form_vec_at[jpn_form_vec_at != ""]
  jpn_form_vec_at  <-tail(jpn_form_vec_at,6)
  jpn_at_numberof_wins <- length(which(jpn_form_vec_at == "W"))
  jpn_at_numberof_draws <- length(which(jpn_form_vec_at == "D"))
  jpn_at_numberof_loss <- length(which(jpn_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  jpn_goalscored_vec_ht <- as.vector(jpn_goalscored_h[jpn_hometeamindex,])
  jpn_goalscored_vec_ht[is.na(jpn_goalscored_vec_ht)] <- ""
  jpn_goalscored_vec_ht <- jpn_goalscored_vec_ht[jpn_goalscored_vec_ht != ""]
  jpn_goalscored_vec_ht  <-tail(jpn_goalscored_vec_ht,6)
  jpn_goalscored_vec_ht  <- as.numeric(jpn_goalscored_vec_ht)
  jpn_ht_totalgoalscored <- sum(jpn_goalscored_vec_ht)
  jpn_ht_matches_scoring <- length(which(jpn_goalscored_vec_ht > 0))
  jpn_ht_matches_without_scoring <- length(which(jpn_goalscored_vec_ht == "0"))
  #awayteam
  jpn_goalscored_vec_at <- as.vector(jpn_goalscored_h[jpn_awayteamindex,])
  jpn_goalscored_vec_at[is.na(jpn_goalscored_vec_at)] <- ""
  jpn_goalscored_vec_at <- jpn_goalscored_vec_at[jpn_goalscored_vec_at != ""]
  jpn_goalscored_vec_at  <-tail(jpn_goalscored_vec_at,6)
  jpn_goalscored_vec_at  <- as.numeric(jpn_goalscored_vec_at)
  jpn_at_totalgoalscored <- sum(jpn_goalscored_vec_at)
  jpn_at_matches_scoring <- length(which(jpn_goalscored_vec_at > 0))
  jpn_at_matches_without_scoring <- length(which(jpn_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  jpn_goalconceded_vec_ht <- as.vector(jpn_goalconceded_h[jpn_hometeamindex,])
  jpn_goalconceded_vec_ht[is.na(jpn_goalconceded_vec_ht)] <- ""
  jpn_goalconceded_vec_ht <- jpn_goalconceded_vec_ht[jpn_goalconceded_vec_ht != ""]
  jpn_goalconceded_vec_ht  <-tail(jpn_goalconceded_vec_ht,6)
  jpn_goalconceded_vec_ht  <- as.numeric(jpn_goalconceded_vec_ht)
  jpn_goalconceded_vec_ht
  jpn_ht_totalgoalconceded <- sum(jpn_goalconceded_vec_ht)
  jpn_ht_matches_concede <- length(which(jpn_goalconceded_vec_ht > 0))
  jpn_ht_matches_without_concede <- length(which(jpn_goalconceded_vec_ht == "0"))
  #awayteam
  jpn_goalconceded_vec_at <- as.vector(jpn_goalconceded_h[jpn_awayteamindex,])
  jpn_goalconceded_vec_at[is.na(jpn_goalconceded_vec_at)] <- ""
  jpn_goalconceded_vec_at <- jpn_goalconceded_vec_at[jpn_goalconceded_vec_at != ""]
  jpn_goalconceded_vec_at  <-tail(jpn_goalconceded_vec_at,6)
  jpn_goalconceded_vec_at  <- as.numeric(jpn_goalconceded_vec_at)
  jpn_at_totalgoalconceded <- sum(jpn_goalconceded_vec_at)
  jpn_at_matches_concede <- length(which(jpn_goalconceded_vec_at > 0))
  jpn_at_matches_without_concede <- length(which(jpn_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  jpn_totalgoals_vec_ht <- as.vector(jpn_totalgoals_h[jpn_hometeamindex,])
  jpn_totalgoals_vec_ht[is.na(jpn_totalgoals_vec_ht)] <- ""
  jpn_totalgoals_vec_ht <- jpn_totalgoals_vec_ht[jpn_totalgoals_vec_ht != ""]
  jpn_totalgoals_vec_ht  <-tail(jpn_totalgoals_vec_ht,6)
  jpn_totalgoals_vec_ht  <- as.numeric(jpn_totalgoals_vec_ht)
  jpn_totalgoals_vec_ht
  jpn_ht_totalgoals <- sum(jpn_totalgoals_vec_ht)
  jpn_ht_avgtotalgoals <- (jpn_ht_totalgoals/6)
  jpn_ht_no_of_ov25 <- length(which(jpn_totalgoals_vec_ht >= 3))
  jpn_ht_no_of_un25 <- length(which(jpn_totalgoals_vec_ht <= 2))
  #awayteam
  jpn_totalgoals_vec_at <- as.vector(jpn_totalgoals_h[jpn_awayteamindex,])
  jpn_totalgoals_vec_at[is.na(jpn_totalgoals_vec_at)] <- ""
  jpn_totalgoals_vec_at <- jpn_totalgoals_vec_at[jpn_totalgoals_vec_at != ""]
  jpn_totalgoals_vec_at  <-tail(jpn_totalgoals_vec_at,6)
  jpn_totalgoals_vec_at  <- as.numeric(jpn_totalgoals_vec_at)
  jpn_totalgoals_vec_at
  jpn_at_totalgoals <- sum(jpn_totalgoals_vec_at)
  jpn_at_avgtotalgoals <- (jpn_at_totalgoals/6)
  jpn_at_no_of_ov25 <- length(which(jpn_totalgoals_vec_at >= 3))
  jpn_at_no_of_un25 <- length(which(jpn_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  jpn_winmargin_vec_ht <- as.vector(jpn_winmargin_h[jpn_hometeamindex,])
  jpn_winmargin_vec_ht[is.na(jpn_winmargin_vec_ht)] <- ""
  jpn_winmargin_vec_ht <- jpn_winmargin_vec_ht[jpn_winmargin_vec_ht != ""]
  jpn_winmargin_vec_ht  <-tail(jpn_winmargin_vec_ht,6)
  jpn_winmargin_vec_ht  <- as.numeric(jpn_winmargin_vec_ht)

  jpn_ht_totalwinmargin <- sum(jpn_winmargin_vec_ht)
  jpn_ht_no_of_winmargin_ov0 <- length(which(jpn_winmargin_vec_ht >= 0))
  jpn_ht_no_of_winmargin_ov1 <- length(which(jpn_winmargin_vec_ht >= 1))
  jpn_ht_no_of_winmargin_un0 <- length(which(jpn_winmargin_vec_ht <= 0))
  jpn_ht_no_of_winmargin_un1 <- length(which(jpn_winmargin_vec_ht <= 1))
  #awayteam
  jpn_winmargin_vec_at <- as.vector(jpn_winmargin_h[jpn_awayteamindex,])
  jpn_winmargin_vec_at[is.na(jpn_winmargin_vec_at)] <- ""
  jpn_winmargin_vec_at <- jpn_winmargin_vec_at[jpn_winmargin_vec_at != ""]
  jpn_winmargin_vec_at  <-tail(jpn_winmargin_vec_at,6)
  jpn_winmargin_vec_at  <- as.numeric(jpn_winmargin_vec_at)

  jpn_at_totalwinmargin <- sum(jpn_winmargin_vec_at)
  jpn_at_no_of_winmargin_ov0 <- length(which(jpn_winmargin_vec_at >= 0))
  jpn_at_no_of_winmargin_ov1 <- length(which(jpn_winmargin_vec_at >= 1))
  jpn_at_no_of_winmargin_un0 <- length(which(jpn_winmargin_vec_at <= 0))
  jpn_at_no_of_winmargin_un1 <- length(which(jpn_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  jpn_winmargin_vec_ht_lm <- as.vector(jpn_winmargin_h[jpn_hometeamindex,])
  jpn_winmargin_vec_ht_lm[is.na(jpn_winmargin_vec_ht_lm)] <- ""
  jpn_winmargin_vec_ht_lm <- jpn_winmargin_vec_ht_lm[jpn_winmargin_vec_ht_lm != ""]
  jpn_winmargin_vec_ht_lm  <-tail(jpn_winmargin_vec_ht_lm,1)
  #awayteam
  jpn_winmargin_vec_at_lm <- as.vector(jpn_winmargin_h[jpn_awayteamindex,])
  jpn_winmargin_vec_at_lm[is.na(jpn_winmargin_vec_at_lm)] <- ""
  jpn_winmargin_vec_at_lm <- jpn_winmargin_vec_at_lm[jpn_winmargin_vec_at_lm != ""]
  jpn_winmargin_vec_at_lm  <-tail(jpn_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  jpn_ht_last6points <- jpn_ht_numberof_wins*3 + jpn_ht_numberof_draws*1
  jpn_at_last6points <- jpn_at_numberof_wins*3 + jpn_at_numberof_draws*1

  if(jpn_ht_last6points > jpn_at_last6points) {jpn_3waypick <- "1"}  else {jpn_3waypick <- "X2"}

  if(jpn_at_last6points > jpn_ht_last6points ) {jpn_3waypick <- "2"} else {jpn_3waypick <- "1X"}

  if(jpn_ht_no_of_ov25 + jpn_at_no_of_ov25 >= 6) {jpn_goalspick <- "ov25"} else {jpn_goalspick <- "un25"}

  if(jpn_ht_no_of_un25 + jpn_at_no_of_un25 >= 6) {jpn_goalspick <- "un25"} else {jpn_goalspick <- "ov25"}

  if(jpn_ht_matches_scoring >= 4 && jpn_at_matches_scoring >=4) {jpn_btts <- "BTTS-Y"} else {jpn_btts <- "BTTS-N"}


  jpn_prediction[jpn_row] <- rbind(paste(jpn_3waypick,jpn_goalspick,jpn_btts,sep = ","))
  jpn_HWM[jpn_row] <- jpn_ht_totalwinmargin
  jpn_AWM[jpn_row] <- jpn_at_totalwinmargin

  jpn_HWMLM[jpn_row] <- jpn_winmargin_vec_ht_lm
  jpn_AWMLM[jpn_row] <- jpn_winmargin_vec_at_lm


}

jpn_prediction <- as.data.frame(jpn_prediction)
colnames(jpn_prediction) <- "prediction"

jpn_HWM <- as.data.frame(jpn_HWM)
colnames(jpn_HWM) <- "HWM"

jpn_AWM <- as.data.frame(jpn_AWM)
colnames(jpn_AWM) <- "AWM"

jpn_HWMLM <- as.data.frame(jpn_HWMLM)
colnames(jpn_HWMLM) <- "HWMLM"

jpn_AWMLM <- as.data.frame(jpn_AWMLM)
colnames(jpn_AWMLM) <- "AWMLM"

jpn_picks <- cbind(JPN_fixtures$Div,JPN_fixtures$HomeTeam_jpn,JPN_fixtures$AwayTeam_jpn,jpn_prediction,jpn_HWM,jpn_AWM,jpn_HWMLM,jpn_AWMLM)
colnames(jpn_picks)[1] <- "picks_Div"
colnames(jpn_picks)[2] <- "picks_HomeTeam"
colnames(jpn_picks)[3] <- "picks_AwayTeam"
jpn_picks$matchid <- paste(jpn_picks$picks_HomeTeam,jpn_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of JPN
jpn_picks
#MEX
MEX_fixtures$Hometeam_mex_index <- match(MEX_fixtures$HomeTeam_mex,mex_teams)
MEX_fixtures$Awayteam_mex_index <- match(MEX_fixtures$AwayTeam_mex,mex_teams)
mex_prediction <- c()
mex_HWM <- c()
mex_AWM <- c()
mex_HWMLM <- c()
mex_AWMLM <- c()
for(mex_row in 1:nrow(MEX_fixtures))
{

  mex_hometeamindex <- MEX_fixtures[mex_row,"Hometeam_mex_index"]
  mex_awayteamindex <- MEX_fixtures[mex_row,"Awayteam_mex_index"]
  #analyse team form
  #home team
  mex_form_vec_ht <- as.vector(mex_form_h[mex_hometeamindex,])
  mex_form_vec_ht[is.na(mex_form_vec_ht)] <- ""
  mex_form_vec_ht <- mex_form_vec_ht[mex_form_vec_ht != ""]
  mex_form_vec_ht  <-tail(mex_form_vec_ht,6)
  mex_ht_numberof_wins <- length(which(mex_form_vec_ht == "W"))
  mex_ht_numberof_draws <- length(which(mex_form_vec_ht == "D"))
  mex_ht_numberof_loss <- length(which(mex_form_vec_ht == "L"))
  #awayteam
  mex_form_vec_at <- as.vector(mex_form_h[mex_awayteamindex,])
  mex_form_vec_at[is.na(mex_form_vec_at)] <- ""
  mex_form_vec_at <- mex_form_vec_at[mex_form_vec_at != ""]
  mex_form_vec_at  <-tail(mex_form_vec_at,6)
  mex_at_numberof_wins <- length(which(mex_form_vec_at == "W"))
  mex_at_numberof_draws <- length(which(mex_form_vec_at == "D"))
  mex_at_numberof_loss <- length(which(mex_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  mex_goalscored_vec_ht <- as.vector(mex_goalscored_h[mex_hometeamindex,])
  mex_goalscored_vec_ht[is.na(mex_goalscored_vec_ht)] <- ""
  mex_goalscored_vec_ht <- mex_goalscored_vec_ht[mex_goalscored_vec_ht != ""]
  mex_goalscored_vec_ht  <-tail(mex_goalscored_vec_ht,6)
  mex_goalscored_vec_ht  <- as.numeric(mex_goalscored_vec_ht)
  mex_ht_totalgoalscored <- sum(mex_goalscored_vec_ht)
  mex_ht_matches_scoring <- length(which(mex_goalscored_vec_ht > 0))
  mex_ht_matches_without_scoring <- length(which(mex_goalscored_vec_ht == "0"))
  #awayteam
  mex_goalscored_vec_at <- as.vector(mex_goalscored_h[mex_awayteamindex,])
  mex_goalscored_vec_at[is.na(mex_goalscored_vec_at)] <- ""
  mex_goalscored_vec_at <- mex_goalscored_vec_at[mex_goalscored_vec_at != ""]
  mex_goalscored_vec_at  <-tail(mex_goalscored_vec_at,6)
  mex_goalscored_vec_at  <- as.numeric(mex_goalscored_vec_at)
  mex_at_totalgoalscored <- sum(mex_goalscored_vec_at)
  mex_at_matches_scoring <- length(which(mex_goalscored_vec_at > 0))
  mex_at_matches_without_scoring <- length(which(mex_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  mex_goalconceded_vec_ht <- as.vector(mex_goalconceded_h[mex_hometeamindex,])
  mex_goalconceded_vec_ht[is.na(mex_goalconceded_vec_ht)] <- ""
  mex_goalconceded_vec_ht <- mex_goalconceded_vec_ht[mex_goalconceded_vec_ht != ""]
  mex_goalconceded_vec_ht  <-tail(mex_goalconceded_vec_ht,6)
  mex_goalconceded_vec_ht  <- as.numeric(mex_goalconceded_vec_ht)
  mex_goalconceded_vec_ht
  mex_ht_totalgoalconceded <- sum(mex_goalconceded_vec_ht)
  mex_ht_matches_concede <- length(which(mex_goalconceded_vec_ht > 0))
  mex_ht_matches_without_concede <- length(which(mex_goalconceded_vec_ht == "0"))
  #awayteam
  mex_goalconceded_vec_at <- as.vector(mex_goalconceded_h[mex_awayteamindex,])
  mex_goalconceded_vec_at[is.na(mex_goalconceded_vec_at)] <- ""
  mex_goalconceded_vec_at <- mex_goalconceded_vec_at[mex_goalconceded_vec_at != ""]
  mex_goalconceded_vec_at  <-tail(mex_goalconceded_vec_at,6)
  mex_goalconceded_vec_at  <- as.numeric(mex_goalconceded_vec_at)
  mex_at_totalgoalconceded <- sum(mex_goalconceded_vec_at)
  mex_at_matches_concede <- length(which(mex_goalconceded_vec_at > 0))
  mex_at_matches_without_concede <- length(which(mex_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  mex_totalgoals_vec_ht <- as.vector(mex_totalgoals_h[mex_hometeamindex,])
  mex_totalgoals_vec_ht[is.na(mex_totalgoals_vec_ht)] <- ""
  mex_totalgoals_vec_ht <- mex_totalgoals_vec_ht[mex_totalgoals_vec_ht != ""]
  mex_totalgoals_vec_ht  <-tail(mex_totalgoals_vec_ht,6)
  mex_totalgoals_vec_ht  <- as.numeric(mex_totalgoals_vec_ht)
  mex_totalgoals_vec_ht
  mex_ht_totalgoals <- sum(mex_totalgoals_vec_ht)
  mex_ht_avgtotalgoals <- (mex_ht_totalgoals/6)
  mex_ht_no_of_ov25 <- length(which(mex_totalgoals_vec_ht >= 3))
  mex_ht_no_of_un25 <- length(which(mex_totalgoals_vec_ht <= 2))
  #awayteam
  mex_totalgoals_vec_at <- as.vector(mex_totalgoals_h[mex_awayteamindex,])
  mex_totalgoals_vec_at[is.na(mex_totalgoals_vec_at)] <- ""
  mex_totalgoals_vec_at <- mex_totalgoals_vec_at[mex_totalgoals_vec_at != ""]
  mex_totalgoals_vec_at  <-tail(mex_totalgoals_vec_at,6)
  mex_totalgoals_vec_at  <- as.numeric(mex_totalgoals_vec_at)
  mex_totalgoals_vec_at
  mex_at_totalgoals <- sum(mex_totalgoals_vec_at)
  mex_at_avgtotalgoals <- (mex_at_totalgoals/6)
  mex_at_no_of_ov25 <- length(which(mex_totalgoals_vec_at >= 3))
  mex_at_no_of_un25 <- length(which(mex_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  mex_winmargin_vec_ht <- as.vector(mex_winmargin_h[mex_hometeamindex,])
  mex_winmargin_vec_ht[is.na(mex_winmargin_vec_ht)] <- ""
  mex_winmargin_vec_ht <- mex_winmargin_vec_ht[mex_winmargin_vec_ht != ""]
  mex_winmargin_vec_ht  <-tail(mex_winmargin_vec_ht,6)
  mex_winmargin_vec_ht  <- as.numeric(mex_winmargin_vec_ht)

  mex_ht_totalwinmargin <- sum(mex_winmargin_vec_ht)
  mex_ht_no_of_winmargin_ov0 <- length(which(mex_winmargin_vec_ht >= 0))
  mex_ht_no_of_winmargin_ov1 <- length(which(mex_winmargin_vec_ht >= 1))
  mex_ht_no_of_winmargin_un0 <- length(which(mex_winmargin_vec_ht <= 0))
  mex_ht_no_of_winmargin_un1 <- length(which(mex_winmargin_vec_ht <= 1))
  #awayteam
  mex_winmargin_vec_at <- as.vector(mex_winmargin_h[mex_awayteamindex,])
  mex_winmargin_vec_at[is.na(mex_winmargin_vec_at)] <- ""
  mex_winmargin_vec_at <- mex_winmargin_vec_at[mex_winmargin_vec_at != ""]
  mex_winmargin_vec_at  <-tail(mex_winmargin_vec_at,6)
  mex_winmargin_vec_at  <- as.numeric(mex_winmargin_vec_at)

  mex_at_totalwinmargin <- sum(mex_winmargin_vec_at)
  mex_at_no_of_winmargin_ov0 <- length(which(mex_winmargin_vec_at >= 0))
  mex_at_no_of_winmargin_ov1 <- length(which(mex_winmargin_vec_at >= 1))
  mex_at_no_of_winmargin_un0 <- length(which(mex_winmargin_vec_at <= 0))
  mex_at_no_of_winmargin_un1 <- length(which(mex_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  mex_winmargin_vec_ht_lm <- as.vector(mex_winmargin_h[mex_hometeamindex,])
  mex_winmargin_vec_ht_lm[is.na(mex_winmargin_vec_ht_lm)] <- ""
  mex_winmargin_vec_ht_lm <- mex_winmargin_vec_ht_lm[mex_winmargin_vec_ht_lm != ""]
  mex_winmargin_vec_ht_lm  <-tail(mex_winmargin_vec_ht_lm,1)
  #awayteam
  mex_winmargin_vec_at_lm <- as.vector(mex_winmargin_h[mex_awayteamindex,])
  mex_winmargin_vec_at_lm[is.na(mex_winmargin_vec_at_lm)] <- ""
  mex_winmargin_vec_at_lm <- mex_winmargin_vec_at_lm[mex_winmargin_vec_at_lm != ""]
  mex_winmargin_vec_at_lm  <-tail(mex_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  mex_ht_last6points <- mex_ht_numberof_wins*3 + mex_ht_numberof_draws*1
  mex_at_last6points <- mex_at_numberof_wins*3 + mex_at_numberof_draws*1

  if(mex_ht_last6points > mex_at_last6points) {mex_3waypick <- "1"}  else {mex_3waypick <- "X2"}

  if(mex_at_last6points > mex_ht_last6points ) {mex_3waypick <- "2"} else {mex_3waypick <- "1X"}

  if(mex_ht_no_of_ov25 + mex_at_no_of_ov25 >= 6) {mex_goalspick <- "ov25"} else {mex_goalspick <- "un25"}

  if(mex_ht_no_of_un25 + mex_at_no_of_un25 >= 6) {mex_goalspick <- "un25"} else {mex_goalspick <- "ov25"}

  if(mex_ht_matches_scoring >= 4 && mex_at_matches_scoring >=4) {mex_btts <- "BTTS-Y"} else {mex_btts <- "BTTS-N"}


  mex_prediction[mex_row] <- rbind(paste(mex_3waypick,mex_goalspick,mex_btts,sep = ","))
  mex_HWM[mex_row] <- mex_ht_totalwinmargin
  mex_AWM[mex_row] <- mex_at_totalwinmargin

  mex_HWMLM[mex_row] <- mex_winmargin_vec_ht_lm
  mex_AWMLM[mex_row] <- mex_winmargin_vec_at_lm


}

mex_prediction <- as.data.frame(mex_prediction)
colnames(mex_prediction) <- "prediction"

mex_HWM <- as.data.frame(mex_HWM)
colnames(mex_HWM) <- "HWM"

mex_AWM <- as.data.frame(mex_AWM)
colnames(mex_AWM) <- "AWM"

mex_HWMLM <- as.data.frame(mex_HWMLM)
colnames(mex_HWMLM) <- "HWMLM"

mex_AWMLM <- as.data.frame(mex_AWMLM)
colnames(mex_AWMLM) <- "AWMLM"

mex_picks <- cbind(MEX_fixtures$Div,MEX_fixtures$HomeTeam_mex,MEX_fixtures$AwayTeam_mex,mex_prediction,mex_HWM,mex_AWM,mex_HWMLM,mex_AWMLM)
colnames(mex_picks)[1] <- "picks_Div"
colnames(mex_picks)[2] <- "picks_HomeTeam"
colnames(mex_picks)[3] <- "picks_AwayTeam"
mex_picks$matchid <- paste(mex_picks$picks_HomeTeam,mex_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of MEX
mex_picks
############################################################################################
#end of mex
#MLS
MLS_fixtures$Hometeam_mls_index <- match(MLS_fixtures$HomeTeam_mls,mls_teams)
MLS_fixtures$Awayteam_mls_index <- match(MLS_fixtures$AwayTeam_mls,mls_teams)
mls_prediction <- c()
mls_HWM <- c()
mls_AWM <- c()
mls_HWMLM <- c()
mls_AWMLM <- c()
for(mls_row in 1:nrow(MLS_fixtures))
{

  mls_hometeamindex <- MLS_fixtures[mls_row,"Hometeam_mls_index"]
  mls_awayteamindex <- MLS_fixtures[mls_row,"Awayteam_mls_index"]
  #analyse team form
  #home team
  mls_form_vec_ht <- as.vector(mls_form_h[mls_hometeamindex,])
  mls_form_vec_ht[is.na(mls_form_vec_ht)] <- ""
  mls_form_vec_ht <- mls_form_vec_ht[mls_form_vec_ht != ""]
  mls_form_vec_ht  <-tail(mls_form_vec_ht,6)
  mls_ht_numberof_wins <- length(which(mls_form_vec_ht == "W"))
  mls_ht_numberof_draws <- length(which(mls_form_vec_ht == "D"))
  mls_ht_numberof_loss <- length(which(mls_form_vec_ht == "L"))
  #awayteam
  mls_form_vec_at <- as.vector(mls_form_h[mls_awayteamindex,])
  mls_form_vec_at[is.na(mls_form_vec_at)] <- ""
  mls_form_vec_at <- mls_form_vec_at[mls_form_vec_at != ""]
  mls_form_vec_at  <-tail(mls_form_vec_at,6)
  mls_at_numberof_wins <- length(which(mls_form_vec_at == "W"))
  mls_at_numberof_draws <- length(which(mls_form_vec_at == "D"))
  mls_at_numberof_loss <- length(which(mls_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  mls_goalscored_vec_ht <- as.vector(mls_goalscored_h[mls_hometeamindex,])
  mls_goalscored_vec_ht[is.na(mls_goalscored_vec_ht)] <- ""
  mls_goalscored_vec_ht <- mls_goalscored_vec_ht[mls_goalscored_vec_ht != ""]
  mls_goalscored_vec_ht  <-tail(mls_goalscored_vec_ht,6)
  mls_goalscored_vec_ht  <- as.numeric(mls_goalscored_vec_ht)
  mls_ht_totalgoalscored <- sum(mls_goalscored_vec_ht)
  mls_ht_matches_scoring <- length(which(mls_goalscored_vec_ht > 0))
  mls_ht_matches_without_scoring <- length(which(mls_goalscored_vec_ht == "0"))
  #awayteam
  mls_goalscored_vec_at <- as.vector(mls_goalscored_h[mls_awayteamindex,])
  mls_goalscored_vec_at[is.na(mls_goalscored_vec_at)] <- ""
  mls_goalscored_vec_at <- mls_goalscored_vec_at[mls_goalscored_vec_at != ""]
  mls_goalscored_vec_at  <-tail(mls_goalscored_vec_at,6)
  mls_goalscored_vec_at  <- as.numeric(mls_goalscored_vec_at)
  mls_at_totalgoalscored <- sum(mls_goalscored_vec_at)
  mls_at_matches_scoring <- length(which(mls_goalscored_vec_at > 0))
  mls_at_matches_without_scoring <- length(which(mls_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  mls_goalconceded_vec_ht <- as.vector(mls_goalconceded_h[mls_hometeamindex,])
  mls_goalconceded_vec_ht[is.na(mls_goalconceded_vec_ht)] <- ""
  mls_goalconceded_vec_ht <- mls_goalconceded_vec_ht[mls_goalconceded_vec_ht != ""]
  mls_goalconceded_vec_ht  <-tail(mls_goalconceded_vec_ht,6)
  mls_goalconceded_vec_ht  <- as.numeric(mls_goalconceded_vec_ht)
  mls_goalconceded_vec_ht
  mls_ht_totalgoalconceded <- sum(mls_goalconceded_vec_ht)
  mls_ht_matches_concede <- length(which(mls_goalconceded_vec_ht > 0))
  mls_ht_matches_without_concede <- length(which(mls_goalconceded_vec_ht == "0"))
  #awayteam
  mls_goalconceded_vec_at <- as.vector(mls_goalconceded_h[mls_awayteamindex,])
  mls_goalconceded_vec_at[is.na(mls_goalconceded_vec_at)] <- ""
  mls_goalconceded_vec_at <- mls_goalconceded_vec_at[mls_goalconceded_vec_at != ""]
  mls_goalconceded_vec_at  <-tail(mls_goalconceded_vec_at,6)
  mls_goalconceded_vec_at  <- as.numeric(mls_goalconceded_vec_at)
  mls_at_totalgoalconceded <- sum(mls_goalconceded_vec_at)
  mls_at_matches_concede <- length(which(mls_goalconceded_vec_at > 0))
  mls_at_matches_without_concede <- length(which(mls_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  mls_totalgoals_vec_ht <- as.vector(mls_totalgoals_h[mls_hometeamindex,])
  mls_totalgoals_vec_ht[is.na(mls_totalgoals_vec_ht)] <- ""
  mls_totalgoals_vec_ht <- mls_totalgoals_vec_ht[mls_totalgoals_vec_ht != ""]
  mls_totalgoals_vec_ht  <-tail(mls_totalgoals_vec_ht,6)
  mls_totalgoals_vec_ht  <- as.numeric(mls_totalgoals_vec_ht)
  mls_totalgoals_vec_ht
  mls_ht_totalgoals <- sum(mls_totalgoals_vec_ht)
  mls_ht_avgtotalgoals <- (mls_ht_totalgoals/6)
  mls_ht_no_of_ov25 <- length(which(mls_totalgoals_vec_ht >= 3))
  mls_ht_no_of_un25 <- length(which(mls_totalgoals_vec_ht <= 2))
  #awayteam
  mls_totalgoals_vec_at <- as.vector(mls_totalgoals_h[mls_awayteamindex,])
  mls_totalgoals_vec_at[is.na(mls_totalgoals_vec_at)] <- ""
  mls_totalgoals_vec_at <- mls_totalgoals_vec_at[mls_totalgoals_vec_at != ""]
  mls_totalgoals_vec_at  <-tail(mls_totalgoals_vec_at,6)
  mls_totalgoals_vec_at  <- as.numeric(mls_totalgoals_vec_at)
  mls_totalgoals_vec_at
  mls_at_totalgoals <- sum(mls_totalgoals_vec_at)
  mls_at_avgtotalgoals <- (mls_at_totalgoals/6)
  mls_at_no_of_ov25 <- length(which(mls_totalgoals_vec_at >= 3))
  mls_at_no_of_un25 <- length(which(mls_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  mls_winmargin_vec_ht <- as.vector(mls_winmargin_h[mls_hometeamindex,])
  mls_winmargin_vec_ht[is.na(mls_winmargin_vec_ht)] <- ""
  mls_winmargin_vec_ht <- mls_winmargin_vec_ht[mls_winmargin_vec_ht != ""]
  mls_winmargin_vec_ht  <-tail(mls_winmargin_vec_ht,6)
  mls_winmargin_vec_ht  <- as.numeric(mls_winmargin_vec_ht)

  mls_ht_totalwinmargin <- sum(mls_winmargin_vec_ht)
  mls_ht_no_of_winmargin_ov0 <- length(which(mls_winmargin_vec_ht >= 0))
  mls_ht_no_of_winmargin_ov1 <- length(which(mls_winmargin_vec_ht >= 1))
  mls_ht_no_of_winmargin_un0 <- length(which(mls_winmargin_vec_ht <= 0))
  mls_ht_no_of_winmargin_un1 <- length(which(mls_winmargin_vec_ht <= 1))
  #awayteam
  mls_winmargin_vec_at <- as.vector(mls_winmargin_h[mls_awayteamindex,])
  mls_winmargin_vec_at[is.na(mls_winmargin_vec_at)] <- ""
  mls_winmargin_vec_at <- mls_winmargin_vec_at[mls_winmargin_vec_at != ""]
  mls_winmargin_vec_at  <-tail(mls_winmargin_vec_at,6)
  mls_winmargin_vec_at  <- as.numeric(mls_winmargin_vec_at)

  mls_at_totalwinmargin <- sum(mls_winmargin_vec_at)
  mls_at_no_of_winmargin_ov0 <- length(which(mls_winmargin_vec_at >= 0))
  mls_at_no_of_winmargin_ov1 <- length(which(mls_winmargin_vec_at >= 1))
  mls_at_no_of_winmargin_un0 <- length(which(mls_winmargin_vec_at <= 0))
  mls_at_no_of_winmargin_un1 <- length(which(mls_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  mls_winmargin_vec_ht_lm <- as.vector(mls_winmargin_h[mls_hometeamindex,])
  mls_winmargin_vec_ht_lm[is.na(mls_winmargin_vec_ht_lm)] <- ""
  mls_winmargin_vec_ht_lm <- mls_winmargin_vec_ht_lm[mls_winmargin_vec_ht_lm != ""]
  mls_winmargin_vec_ht_lm  <-tail(mls_winmargin_vec_ht_lm,1)
  #awayteam
  mls_winmargin_vec_at_lm <- as.vector(mls_winmargin_h[mls_awayteamindex,])
  mls_winmargin_vec_at_lm[is.na(mls_winmargin_vec_at_lm)] <- ""
  mls_winmargin_vec_at_lm <- mls_winmargin_vec_at_lm[mls_winmargin_vec_at_lm != ""]
  mls_winmargin_vec_at_lm  <-tail(mls_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  mls_ht_last6points <- mls_ht_numberof_wins*3 + mls_ht_numberof_draws*1
  mls_at_last6points <- mls_at_numberof_wins*3 + mls_at_numberof_draws*1

  if(mls_ht_last6points > mls_at_last6points) {mls_3waypick <- "1"}  else {mls_3waypick <- "X2"}

  if(mls_at_last6points > mls_ht_last6points ) {mls_3waypick <- "2"} else {mls_3waypick <- "1X"}

  if(mls_ht_no_of_ov25 + mls_at_no_of_ov25 >= 6) {mls_goalspick <- "ov25"} else {mls_goalspick <- "un25"}

  if(mls_ht_no_of_un25 + mls_at_no_of_un25 >= 6) {mls_goalspick <- "un25"} else {mls_goalspick <- "ov25"}

  if(mls_ht_matches_scoring >= 4 && mls_at_matches_scoring >=4) {mls_btts <- "BTTS-Y"} else {mls_btts <- "BTTS-N"}


  mls_prediction[mls_row] <- rbind(paste(mls_3waypick,mls_goalspick,mls_btts,sep = ","))
  mls_HWM[mls_row] <- mls_ht_totalwinmargin
  mls_AWM[mls_row] <- mls_at_totalwinmargin

  mls_HWMLM[mls_row] <- mls_winmargin_vec_ht_lm
  mls_AWMLM[mls_row] <- mls_winmargin_vec_at_lm


}

mls_prediction <- as.data.frame(mls_prediction)
colnames(mls_prediction) <- "prediction"

mls_HWM <- as.data.frame(mls_HWM)
colnames(mls_HWM) <- "HWM"

mls_AWM <- as.data.frame(mls_AWM)
colnames(mls_AWM) <- "AWM"

mls_HWMLM <- as.data.frame(mls_HWMLM)
colnames(mls_HWMLM) <- "HWMLM"

mls_AWMLM <- as.data.frame(mls_AWMLM)
colnames(mls_AWMLM) <- "AWMLM"

mls_picks <- cbind(MLS_fixtures$Div,MLS_fixtures$HomeTeam_mls,MLS_fixtures$AwayTeam_mls,mls_prediction,mls_HWM,mls_AWM,mls_HWMLM,mls_AWMLM)
colnames(mls_picks)[1] <- "picks_Div"
colnames(mls_picks)[2] <- "picks_HomeTeam"
colnames(mls_picks)[3] <- "picks_AwayTeam"
mls_picks$matchid <- paste(mls_picks$picks_HomeTeam,mls_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of MLS
mls_picks
############################################################################################
#end of MLS
#NOR
NOR_fixtures$Hometeam_nor_index <- match(NOR_fixtures$HomeTeam_nor,nor_teams)
NOR_fixtures$Awayteam_nor_index <- match(NOR_fixtures$AwayTeam_nor,nor_teams)
nor_prediction <- c()
nor_HWM <- c()
nor_AWM <- c()
nor_HWMLM <- c()
nor_AWMLM <- c()
for(nor_row in 1:nrow(NOR_fixtures))
{

  nor_hometeamindex <- NOR_fixtures[nor_row,"Hometeam_nor_index"]
  nor_awayteamindex <- NOR_fixtures[nor_row,"Awayteam_nor_index"]
  #analyse team form
  #home team
  nor_form_vec_ht <- as.vector(nor_form_h[nor_hometeamindex,])
  nor_form_vec_ht[is.na(nor_form_vec_ht)] <- ""
  nor_form_vec_ht <- nor_form_vec_ht[nor_form_vec_ht != ""]
  nor_form_vec_ht  <-tail(nor_form_vec_ht,6)
  nor_ht_numberof_wins <- length(which(nor_form_vec_ht == "W"))
  nor_ht_numberof_draws <- length(which(nor_form_vec_ht == "D"))
  nor_ht_numberof_loss <- length(which(nor_form_vec_ht == "L"))
  #awayteam
  nor_form_vec_at <- as.vector(nor_form_h[nor_awayteamindex,])
  nor_form_vec_at[is.na(nor_form_vec_at)] <- ""
  nor_form_vec_at <- nor_form_vec_at[nor_form_vec_at != ""]
  nor_form_vec_at  <-tail(nor_form_vec_at,6)
  nor_at_numberof_wins <- length(which(nor_form_vec_at == "W"))
  nor_at_numberof_draws <- length(which(nor_form_vec_at == "D"))
  nor_at_numberof_loss <- length(which(nor_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  nor_goalscored_vec_ht <- as.vector(nor_goalscored_h[nor_hometeamindex,])
  nor_goalscored_vec_ht[is.na(nor_goalscored_vec_ht)] <- ""
  nor_goalscored_vec_ht <- nor_goalscored_vec_ht[nor_goalscored_vec_ht != ""]
  nor_goalscored_vec_ht  <-tail(nor_goalscored_vec_ht,6)
  nor_goalscored_vec_ht  <- as.numeric(nor_goalscored_vec_ht)
  nor_ht_totalgoalscored <- sum(nor_goalscored_vec_ht)
  nor_ht_matches_scoring <- length(which(nor_goalscored_vec_ht > 0))
  nor_ht_matches_without_scoring <- length(which(nor_goalscored_vec_ht == "0"))
  #awayteam
  nor_goalscored_vec_at <- as.vector(nor_goalscored_h[nor_awayteamindex,])
  nor_goalscored_vec_at[is.na(nor_goalscored_vec_at)] <- ""
  nor_goalscored_vec_at <- nor_goalscored_vec_at[nor_goalscored_vec_at != ""]
  nor_goalscored_vec_at  <-tail(nor_goalscored_vec_at,6)
  nor_goalscored_vec_at  <- as.numeric(nor_goalscored_vec_at)
  nor_at_totalgoalscored <- sum(nor_goalscored_vec_at)
  nor_at_matches_scoring <- length(which(nor_goalscored_vec_at > 0))
  nor_at_matches_without_scoring <- length(which(nor_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  nor_goalconceded_vec_ht <- as.vector(nor_goalconceded_h[nor_hometeamindex,])
  nor_goalconceded_vec_ht[is.na(nor_goalconceded_vec_ht)] <- ""
  nor_goalconceded_vec_ht <- nor_goalconceded_vec_ht[nor_goalconceded_vec_ht != ""]
  nor_goalconceded_vec_ht  <-tail(nor_goalconceded_vec_ht,6)
  nor_goalconceded_vec_ht  <- as.numeric(nor_goalconceded_vec_ht)
  nor_goalconceded_vec_ht
  nor_ht_totalgoalconceded <- sum(nor_goalconceded_vec_ht)
  nor_ht_matches_concede <- length(which(nor_goalconceded_vec_ht > 0))
  nor_ht_matches_without_concede <- length(which(nor_goalconceded_vec_ht == "0"))
  #awayteam
  nor_goalconceded_vec_at <- as.vector(nor_goalconceded_h[nor_awayteamindex,])
  nor_goalconceded_vec_at[is.na(nor_goalconceded_vec_at)] <- ""
  nor_goalconceded_vec_at <- nor_goalconceded_vec_at[nor_goalconceded_vec_at != ""]
  nor_goalconceded_vec_at  <-tail(nor_goalconceded_vec_at,6)
  nor_goalconceded_vec_at  <- as.numeric(nor_goalconceded_vec_at)
  nor_at_totalgoalconceded <- sum(nor_goalconceded_vec_at)
  nor_at_matches_concede <- length(which(nor_goalconceded_vec_at > 0))
  nor_at_matches_without_concede <- length(which(nor_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  nor_totalgoals_vec_ht <- as.vector(nor_totalgoals_h[nor_hometeamindex,])
  nor_totalgoals_vec_ht[is.na(nor_totalgoals_vec_ht)] <- ""
  nor_totalgoals_vec_ht <- nor_totalgoals_vec_ht[nor_totalgoals_vec_ht != ""]
  nor_totalgoals_vec_ht  <-tail(nor_totalgoals_vec_ht,6)
  nor_totalgoals_vec_ht  <- as.numeric(nor_totalgoals_vec_ht)
  nor_totalgoals_vec_ht
  nor_ht_totalgoals <- sum(nor_totalgoals_vec_ht)
  nor_ht_avgtotalgoals <- (nor_ht_totalgoals/6)
  nor_ht_no_of_ov25 <- length(which(nor_totalgoals_vec_ht >= 3))
  nor_ht_no_of_un25 <- length(which(nor_totalgoals_vec_ht <= 2))
  #awayteam
  nor_totalgoals_vec_at <- as.vector(nor_totalgoals_h[nor_awayteamindex,])
  nor_totalgoals_vec_at[is.na(nor_totalgoals_vec_at)] <- ""
  nor_totalgoals_vec_at <- nor_totalgoals_vec_at[nor_totalgoals_vec_at != ""]
  nor_totalgoals_vec_at  <-tail(nor_totalgoals_vec_at,6)
  nor_totalgoals_vec_at  <- as.numeric(nor_totalgoals_vec_at)
  nor_totalgoals_vec_at
  nor_at_totalgoals <- sum(nor_totalgoals_vec_at)
  nor_at_avgtotalgoals <- (nor_at_totalgoals/6)
  nor_at_no_of_ov25 <- length(which(nor_totalgoals_vec_at >= 3))
  nor_at_no_of_un25 <- length(which(nor_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  nor_winmargin_vec_ht <- as.vector(nor_winmargin_h[nor_hometeamindex,])
  nor_winmargin_vec_ht[is.na(nor_winmargin_vec_ht)] <- ""
  nor_winmargin_vec_ht <- nor_winmargin_vec_ht[nor_winmargin_vec_ht != ""]
  nor_winmargin_vec_ht  <-tail(nor_winmargin_vec_ht,6)
  nor_winmargin_vec_ht  <- as.numeric(nor_winmargin_vec_ht)

  nor_ht_totalwinmargin <- sum(nor_winmargin_vec_ht)
  nor_ht_no_of_winmargin_ov0 <- length(which(nor_winmargin_vec_ht >= 0))
  nor_ht_no_of_winmargin_ov1 <- length(which(nor_winmargin_vec_ht >= 1))
  nor_ht_no_of_winmargin_un0 <- length(which(nor_winmargin_vec_ht <= 0))
  nor_ht_no_of_winmargin_un1 <- length(which(nor_winmargin_vec_ht <= 1))
  #awayteam
  nor_winmargin_vec_at <- as.vector(nor_winmargin_h[nor_awayteamindex,])
  nor_winmargin_vec_at[is.na(nor_winmargin_vec_at)] <- ""
  nor_winmargin_vec_at <- nor_winmargin_vec_at[nor_winmargin_vec_at != ""]
  nor_winmargin_vec_at  <-tail(nor_winmargin_vec_at,6)
  nor_winmargin_vec_at  <- as.numeric(nor_winmargin_vec_at)

  nor_at_totalwinmargin <- sum(nor_winmargin_vec_at)
  nor_at_no_of_winmargin_ov0 <- length(which(nor_winmargin_vec_at >= 0))
  nor_at_no_of_winmargin_ov1 <- length(which(nor_winmargin_vec_at >= 1))
  nor_at_no_of_winmargin_un0 <- length(which(nor_winmargin_vec_at <= 0))
  nor_at_no_of_winmargin_un1 <- length(which(nor_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  nor_winmargin_vec_ht_lm <- as.vector(nor_winmargin_h[nor_hometeamindex,])
  nor_winmargin_vec_ht_lm[is.na(nor_winmargin_vec_ht_lm)] <- ""
  nor_winmargin_vec_ht_lm <- nor_winmargin_vec_ht_lm[nor_winmargin_vec_ht_lm != ""]
  nor_winmargin_vec_ht_lm  <-tail(nor_winmargin_vec_ht_lm,1)
  #awayteam
  nor_winmargin_vec_at_lm <- as.vector(nor_winmargin_h[nor_awayteamindex,])
  nor_winmargin_vec_at_lm[is.na(nor_winmargin_vec_at_lm)] <- ""
  nor_winmargin_vec_at_lm <- nor_winmargin_vec_at_lm[nor_winmargin_vec_at_lm != ""]
  nor_winmargin_vec_at_lm  <-tail(nor_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  nor_ht_last6points <- nor_ht_numberof_wins*3 + nor_ht_numberof_draws*1
  nor_at_last6points <- nor_at_numberof_wins*3 + nor_at_numberof_draws*1

  if(nor_ht_last6points > nor_at_last6points) {nor_3waypick <- "1"}  else {nor_3waypick <- "X2"}

  if(nor_at_last6points > nor_ht_last6points ) {nor_3waypick <- "2"} else {nor_3waypick <- "1X"}

  if(nor_ht_no_of_ov25 + nor_at_no_of_ov25 >= 6) {nor_goalspick <- "ov25"} else {nor_goalspick <- "un25"}

  if(nor_ht_no_of_un25 + nor_at_no_of_un25 >= 6) {nor_goalspick <- "un25"} else {nor_goalspick <- "ov25"}

  if(nor_ht_matches_scoring >= 4 && nor_at_matches_scoring >=4) {nor_btts <- "BTTS-Y"} else {nor_btts <- "BTTS-N"}


  nor_prediction[nor_row] <- rbind(paste(nor_3waypick,nor_goalspick,nor_btts,sep = ","))
  nor_HWM[nor_row] <- nor_ht_totalwinmargin
  nor_AWM[nor_row] <- nor_at_totalwinmargin

  nor_HWMLM[nor_row] <- nor_winmargin_vec_ht_lm
  nor_AWMLM[nor_row] <- nor_winmargin_vec_at_lm


}

nor_prediction <- as.data.frame(nor_prediction)
colnames(nor_prediction) <- "prediction"

nor_HWM <- as.data.frame(nor_HWM)
colnames(nor_HWM) <- "HWM"

nor_AWM <- as.data.frame(nor_AWM)
colnames(nor_AWM) <- "AWM"

nor_HWMLM <- as.data.frame(nor_HWMLM)
colnames(nor_HWMLM) <- "HWMLM"

nor_AWMLM <- as.data.frame(nor_AWMLM)
colnames(nor_AWMLM) <- "AWMLM"

nor_picks <- cbind(NOR_fixtures$Div,NOR_fixtures$HomeTeam_nor,NOR_fixtures$AwayTeam_nor,nor_prediction,nor_HWM,nor_AWM,nor_HWMLM,nor_AWMLM)
colnames(nor_picks)[1] <- "picks_Div"
colnames(nor_picks)[2] <- "picks_HomeTeam"
colnames(nor_picks)[3] <- "picks_AwayTeam"
nor_picks$matchid <- paste(nor_picks$picks_HomeTeam,nor_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of NOR
nor_picks
############################################################################################
#end of NOR
#POL
POL_fixtures$Hometeam_pol_index <- match(POL_fixtures$HomeTeam_pol,pol_teams)
POL_fixtures$Awayteam_pol_index <- match(POL_fixtures$AwayTeam_pol,pol_teams)
pol_prediction <- c()
pol_HWM <- c()
pol_AWM <- c()
pol_HWMLM <- c()
pol_AWMLM <- c()
for(pol_row in 1:nrow(POL_fixtures))
{

  pol_hometeamindex <- POL_fixtures[pol_row,"Hometeam_pol_index"]
  pol_awayteamindex <- POL_fixtures[pol_row,"Awayteam_pol_index"]
  #analyse team form
  #home team
  pol_form_vec_ht <- as.vector(pol_form_h[pol_hometeamindex,])
  pol_form_vec_ht[is.na(pol_form_vec_ht)] <- ""
  pol_form_vec_ht <- pol_form_vec_ht[pol_form_vec_ht != ""]
  pol_form_vec_ht  <-tail(pol_form_vec_ht,6)
  pol_ht_numberof_wins <- length(which(pol_form_vec_ht == "W"))
  pol_ht_numberof_draws <- length(which(pol_form_vec_ht == "D"))
  pol_ht_numberof_loss <- length(which(pol_form_vec_ht == "L"))
  #awayteam
  pol_form_vec_at <- as.vector(pol_form_h[pol_awayteamindex,])
  pol_form_vec_at[is.na(pol_form_vec_at)] <- ""
  pol_form_vec_at <- pol_form_vec_at[pol_form_vec_at != ""]
  pol_form_vec_at  <-tail(pol_form_vec_at,6)
  pol_at_numberof_wins <- length(which(pol_form_vec_at == "W"))
  pol_at_numberof_draws <- length(which(pol_form_vec_at == "D"))
  pol_at_numberof_loss <- length(which(pol_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  pol_goalscored_vec_ht <- as.vector(pol_goalscored_h[pol_hometeamindex,])
  pol_goalscored_vec_ht[is.na(pol_goalscored_vec_ht)] <- ""
  pol_goalscored_vec_ht <- pol_goalscored_vec_ht[pol_goalscored_vec_ht != ""]
  pol_goalscored_vec_ht  <-tail(pol_goalscored_vec_ht,6)
  pol_goalscored_vec_ht  <- as.numeric(pol_goalscored_vec_ht)
  pol_ht_totalgoalscored <- sum(pol_goalscored_vec_ht)
  pol_ht_matches_scoring <- length(which(pol_goalscored_vec_ht > 0))
  pol_ht_matches_without_scoring <- length(which(pol_goalscored_vec_ht == "0"))
  #awayteam
  pol_goalscored_vec_at <- as.vector(pol_goalscored_h[pol_awayteamindex,])
  pol_goalscored_vec_at[is.na(pol_goalscored_vec_at)] <- ""
  pol_goalscored_vec_at <- pol_goalscored_vec_at[pol_goalscored_vec_at != ""]
  pol_goalscored_vec_at  <-tail(pol_goalscored_vec_at,6)
  pol_goalscored_vec_at  <- as.numeric(pol_goalscored_vec_at)
  pol_at_totalgoalscored <- sum(pol_goalscored_vec_at)
  pol_at_matches_scoring <- length(which(pol_goalscored_vec_at > 0))
  pol_at_matches_without_scoring <- length(which(pol_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  pol_goalconceded_vec_ht <- as.vector(pol_goalconceded_h[pol_hometeamindex,])
  pol_goalconceded_vec_ht[is.na(pol_goalconceded_vec_ht)] <- ""
  pol_goalconceded_vec_ht <- pol_goalconceded_vec_ht[pol_goalconceded_vec_ht != ""]
  pol_goalconceded_vec_ht  <-tail(pol_goalconceded_vec_ht,6)
  pol_goalconceded_vec_ht  <- as.numeric(pol_goalconceded_vec_ht)
  pol_goalconceded_vec_ht
  pol_ht_totalgoalconceded <- sum(pol_goalconceded_vec_ht)
  pol_ht_matches_concede <- length(which(pol_goalconceded_vec_ht > 0))
  pol_ht_matches_without_concede <- length(which(pol_goalconceded_vec_ht == "0"))
  #awayteam
  pol_goalconceded_vec_at <- as.vector(pol_goalconceded_h[pol_awayteamindex,])
  pol_goalconceded_vec_at[is.na(pol_goalconceded_vec_at)] <- ""
  pol_goalconceded_vec_at <- pol_goalconceded_vec_at[pol_goalconceded_vec_at != ""]
  pol_goalconceded_vec_at  <-tail(pol_goalconceded_vec_at,6)
  pol_goalconceded_vec_at  <- as.numeric(pol_goalconceded_vec_at)
  pol_at_totalgoalconceded <- sum(pol_goalconceded_vec_at)
  pol_at_matches_concede <- length(which(pol_goalconceded_vec_at > 0))
  pol_at_matches_without_concede <- length(which(pol_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  pol_totalgoals_vec_ht <- as.vector(pol_totalgoals_h[pol_hometeamindex,])
  pol_totalgoals_vec_ht[is.na(pol_totalgoals_vec_ht)] <- ""
  pol_totalgoals_vec_ht <- pol_totalgoals_vec_ht[pol_totalgoals_vec_ht != ""]
  pol_totalgoals_vec_ht  <-tail(pol_totalgoals_vec_ht,6)
  pol_totalgoals_vec_ht  <- as.numeric(pol_totalgoals_vec_ht)
  pol_totalgoals_vec_ht
  pol_ht_totalgoals <- sum(pol_totalgoals_vec_ht)
  pol_ht_avgtotalgoals <- (pol_ht_totalgoals/6)
  pol_ht_no_of_ov25 <- length(which(pol_totalgoals_vec_ht >= 3))
  pol_ht_no_of_un25 <- length(which(pol_totalgoals_vec_ht <= 2))
  #awayteam
  pol_totalgoals_vec_at <- as.vector(pol_totalgoals_h[pol_awayteamindex,])
  pol_totalgoals_vec_at[is.na(pol_totalgoals_vec_at)] <- ""
  pol_totalgoals_vec_at <- pol_totalgoals_vec_at[pol_totalgoals_vec_at != ""]
  pol_totalgoals_vec_at  <-tail(pol_totalgoals_vec_at,6)
  pol_totalgoals_vec_at  <- as.numeric(pol_totalgoals_vec_at)
  pol_totalgoals_vec_at
  pol_at_totalgoals <- sum(pol_totalgoals_vec_at)
  pol_at_avgtotalgoals <- (pol_at_totalgoals/6)
  pol_at_no_of_ov25 <- length(which(pol_totalgoals_vec_at >= 3))
  pol_at_no_of_un25 <- length(which(pol_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  pol_winmargin_vec_ht <- as.vector(pol_winmargin_h[pol_hometeamindex,])
  pol_winmargin_vec_ht[is.na(pol_winmargin_vec_ht)] <- ""
  pol_winmargin_vec_ht <- pol_winmargin_vec_ht[pol_winmargin_vec_ht != ""]
  pol_winmargin_vec_ht  <-tail(pol_winmargin_vec_ht,6)
  pol_winmargin_vec_ht  <- as.numeric(pol_winmargin_vec_ht)

  pol_ht_totalwinmargin <- sum(pol_winmargin_vec_ht)
  pol_ht_no_of_winmargin_ov0 <- length(which(pol_winmargin_vec_ht >= 0))
  pol_ht_no_of_winmargin_ov1 <- length(which(pol_winmargin_vec_ht >= 1))
  pol_ht_no_of_winmargin_un0 <- length(which(pol_winmargin_vec_ht <= 0))
  pol_ht_no_of_winmargin_un1 <- length(which(pol_winmargin_vec_ht <= 1))
  #awayteam
  pol_winmargin_vec_at <- as.vector(pol_winmargin_h[pol_awayteamindex,])
  pol_winmargin_vec_at[is.na(pol_winmargin_vec_at)] <- ""
  pol_winmargin_vec_at <- pol_winmargin_vec_at[pol_winmargin_vec_at != ""]
  pol_winmargin_vec_at  <-tail(pol_winmargin_vec_at,6)
  pol_winmargin_vec_at  <- as.numeric(pol_winmargin_vec_at)

  pol_at_totalwinmargin <- sum(pol_winmargin_vec_at)
  pol_at_no_of_winmargin_ov0 <- length(which(pol_winmargin_vec_at >= 0))
  pol_at_no_of_winmargin_ov1 <- length(which(pol_winmargin_vec_at >= 1))
  pol_at_no_of_winmargin_un0 <- length(which(pol_winmargin_vec_at <= 0))
  pol_at_no_of_winmargin_un1 <- length(which(pol_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  pol_winmargin_vec_ht_lm <- as.vector(pol_winmargin_h[pol_hometeamindex,])
  pol_winmargin_vec_ht_lm[is.na(pol_winmargin_vec_ht_lm)] <- ""
  pol_winmargin_vec_ht_lm <- pol_winmargin_vec_ht_lm[pol_winmargin_vec_ht_lm != ""]
  pol_winmargin_vec_ht_lm  <-tail(pol_winmargin_vec_ht_lm,1)
  #awayteam
  pol_winmargin_vec_at_lm <- as.vector(pol_winmargin_h[pol_awayteamindex,])
  pol_winmargin_vec_at_lm[is.na(pol_winmargin_vec_at_lm)] <- ""
  pol_winmargin_vec_at_lm <- pol_winmargin_vec_at_lm[pol_winmargin_vec_at_lm != ""]
  pol_winmargin_vec_at_lm  <-tail(pol_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  pol_ht_last6points <- pol_ht_numberof_wins*3 + pol_ht_numberof_draws*1
  pol_at_last6points <- pol_at_numberof_wins*3 + pol_at_numberof_draws*1

  if(pol_ht_last6points > pol_at_last6points) {pol_3waypick <- "1"}  else {pol_3waypick <- "X2"}

  if(pol_at_last6points > pol_ht_last6points ) {pol_3waypick <- "2"} else {pol_3waypick <- "1X"}

  if(pol_ht_no_of_ov25 + pol_at_no_of_ov25 >= 6) {pol_goalspick <- "ov25"} else {pol_goalspick <- "un25"}

  if(pol_ht_no_of_un25 + pol_at_no_of_un25 >= 6) {pol_goalspick <- "un25"} else {pol_goalspick <- "ov25"}

  if(pol_ht_matches_scoring >= 4 && pol_at_matches_scoring >=4) {pol_btts <- "BTTS-Y"} else {pol_btts <- "BTTS-N"}


  pol_prediction[pol_row] <- rbind(paste(pol_3waypick,pol_goalspick,pol_btts,sep = ","))
  pol_HWM[pol_row] <- pol_ht_totalwinmargin
  pol_AWM[pol_row] <- pol_at_totalwinmargin

  pol_HWMLM[pol_row] <- pol_winmargin_vec_ht_lm
  pol_AWMLM[pol_row] <- pol_winmargin_vec_at_lm


}

pol_prediction <- as.data.frame(pol_prediction)
colnames(pol_prediction) <- "prediction"

pol_HWM <- as.data.frame(pol_HWM)
colnames(pol_HWM) <- "HWM"

pol_AWM <- as.data.frame(pol_AWM)
colnames(pol_AWM) <- "AWM"

pol_HWMLM <- as.data.frame(pol_HWMLM)
colnames(pol_HWMLM) <- "HWMLM"

pol_AWMLM <- as.data.frame(pol_AWMLM)
colnames(pol_AWMLM) <- "AWMLM"

pol_picks <- cbind(POL_fixtures$Div,POL_fixtures$HomeTeam_pol,POL_fixtures$AwayTeam_pol,pol_prediction,pol_HWM,pol_AWM,pol_HWMLM,pol_AWMLM)
colnames(pol_picks)[1] <- "picks_Div"
colnames(pol_picks)[2] <- "picks_HomeTeam"
colnames(pol_picks)[3] <- "picks_AwayTeam"
pol_picks$matchid <- paste(pol_picks$picks_HomeTeam,pol_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of POL
pol_picks
############################################################################################
#end of POL
#ROU
ROU_fixtures$Hometeam_rou_index <- match(ROU_fixtures$HomeTeam_rou,rou_teams)
ROU_fixtures$Awayteam_rou_index <- match(ROU_fixtures$AwayTeam_rou,rou_teams)
rou_prediction <- c()
rou_HWM <- c()
rou_AWM <- c()
rou_HWMLM <- c()
rou_AWMLM <- c()
for(rou_row in 1:nrow(ROU_fixtures))
{

  rou_hometeamindex <- ROU_fixtures[rou_row,"Hometeam_rou_index"]
  rou_awayteamindex <- ROU_fixtures[rou_row,"Awayteam_rou_index"]
  #analyse team form
  #home team
  rou_form_vec_ht <- as.vector(rou_form_h[rou_hometeamindex,])
  rou_form_vec_ht[is.na(rou_form_vec_ht)] <- ""
  rou_form_vec_ht <- rou_form_vec_ht[rou_form_vec_ht != ""]
  rou_form_vec_ht  <-tail(rou_form_vec_ht,6)
  rou_ht_numberof_wins <- length(which(rou_form_vec_ht == "W"))
  rou_ht_numberof_draws <- length(which(rou_form_vec_ht == "D"))
  rou_ht_numberof_loss <- length(which(rou_form_vec_ht == "L"))
  #awayteam
  rou_form_vec_at <- as.vector(rou_form_h[rou_awayteamindex,])
  rou_form_vec_at[is.na(rou_form_vec_at)] <- ""
  rou_form_vec_at <- rou_form_vec_at[rou_form_vec_at != ""]
  rou_form_vec_at  <-tail(rou_form_vec_at,6)
  rou_at_numberof_wins <- length(which(rou_form_vec_at == "W"))
  rou_at_numberof_draws <- length(which(rou_form_vec_at == "D"))
  rou_at_numberof_loss <- length(which(rou_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  rou_goalscored_vec_ht <- as.vector(rou_goalscored_h[rou_hometeamindex,])
  rou_goalscored_vec_ht[is.na(rou_goalscored_vec_ht)] <- ""
  rou_goalscored_vec_ht <- rou_goalscored_vec_ht[rou_goalscored_vec_ht != ""]
  rou_goalscored_vec_ht  <-tail(rou_goalscored_vec_ht,6)
  rou_goalscored_vec_ht  <- as.numeric(rou_goalscored_vec_ht)
  rou_ht_totalgoalscored <- sum(rou_goalscored_vec_ht)
  rou_ht_matches_scoring <- length(which(rou_goalscored_vec_ht > 0))
  rou_ht_matches_without_scoring <- length(which(rou_goalscored_vec_ht == "0"))
  #awayteam
  rou_goalscored_vec_at <- as.vector(rou_goalscored_h[rou_awayteamindex,])
  rou_goalscored_vec_at[is.na(rou_goalscored_vec_at)] <- ""
  rou_goalscored_vec_at <- rou_goalscored_vec_at[rou_goalscored_vec_at != ""]
  rou_goalscored_vec_at  <-tail(rou_goalscored_vec_at,6)
  rou_goalscored_vec_at  <- as.numeric(rou_goalscored_vec_at)
  rou_at_totalgoalscored <- sum(rou_goalscored_vec_at)
  rou_at_matches_scoring <- length(which(rou_goalscored_vec_at > 0))
  rou_at_matches_without_scoring <- length(which(rou_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  rou_goalconceded_vec_ht <- as.vector(rou_goalconceded_h[rou_hometeamindex,])
  rou_goalconceded_vec_ht[is.na(rou_goalconceded_vec_ht)] <- ""
  rou_goalconceded_vec_ht <- rou_goalconceded_vec_ht[rou_goalconceded_vec_ht != ""]
  rou_goalconceded_vec_ht  <-tail(rou_goalconceded_vec_ht,6)
  rou_goalconceded_vec_ht  <- as.numeric(rou_goalconceded_vec_ht)
  rou_goalconceded_vec_ht
  rou_ht_totalgoalconceded <- sum(rou_goalconceded_vec_ht)
  rou_ht_matches_concede <- length(which(rou_goalconceded_vec_ht > 0))
  rou_ht_matches_without_concede <- length(which(rou_goalconceded_vec_ht == "0"))
  #awayteam
  rou_goalconceded_vec_at <- as.vector(rou_goalconceded_h[rou_awayteamindex,])
  rou_goalconceded_vec_at[is.na(rou_goalconceded_vec_at)] <- ""
  rou_goalconceded_vec_at <- rou_goalconceded_vec_at[rou_goalconceded_vec_at != ""]
  rou_goalconceded_vec_at  <-tail(rou_goalconceded_vec_at,6)
  rou_goalconceded_vec_at  <- as.numeric(rou_goalconceded_vec_at)
  rou_at_totalgoalconceded <- sum(rou_goalconceded_vec_at)
  rou_at_matches_concede <- length(which(rou_goalconceded_vec_at > 0))
  rou_at_matches_without_concede <- length(which(rou_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  rou_totalgoals_vec_ht <- as.vector(rou_totalgoals_h[rou_hometeamindex,])
  rou_totalgoals_vec_ht[is.na(rou_totalgoals_vec_ht)] <- ""
  rou_totalgoals_vec_ht <- rou_totalgoals_vec_ht[rou_totalgoals_vec_ht != ""]
  rou_totalgoals_vec_ht  <-tail(rou_totalgoals_vec_ht,6)
  rou_totalgoals_vec_ht  <- as.numeric(rou_totalgoals_vec_ht)
  rou_totalgoals_vec_ht
  rou_ht_totalgoals <- sum(rou_totalgoals_vec_ht)
  rou_ht_avgtotalgoals <- (rou_ht_totalgoals/6)
  rou_ht_no_of_ov25 <- length(which(rou_totalgoals_vec_ht >= 3))
  rou_ht_no_of_un25 <- length(which(rou_totalgoals_vec_ht <= 2))
  #awayteam
  rou_totalgoals_vec_at <- as.vector(rou_totalgoals_h[rou_awayteamindex,])
  rou_totalgoals_vec_at[is.na(rou_totalgoals_vec_at)] <- ""
  rou_totalgoals_vec_at <- rou_totalgoals_vec_at[rou_totalgoals_vec_at != ""]
  rou_totalgoals_vec_at  <-tail(rou_totalgoals_vec_at,6)
  rou_totalgoals_vec_at  <- as.numeric(rou_totalgoals_vec_at)
  rou_totalgoals_vec_at
  rou_at_totalgoals <- sum(rou_totalgoals_vec_at)
  rou_at_avgtotalgoals <- (rou_at_totalgoals/6)
  rou_at_no_of_ov25 <- length(which(rou_totalgoals_vec_at >= 3))
  rou_at_no_of_un25 <- length(which(rou_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  rou_winmargin_vec_ht <- as.vector(rou_winmargin_h[rou_hometeamindex,])
  rou_winmargin_vec_ht[is.na(rou_winmargin_vec_ht)] <- ""
  rou_winmargin_vec_ht <- rou_winmargin_vec_ht[rou_winmargin_vec_ht != ""]
  rou_winmargin_vec_ht  <-tail(rou_winmargin_vec_ht,6)
  rou_winmargin_vec_ht  <- as.numeric(rou_winmargin_vec_ht)

  rou_ht_totalwinmargin <- sum(rou_winmargin_vec_ht)
  rou_ht_no_of_winmargin_ov0 <- length(which(rou_winmargin_vec_ht >= 0))
  rou_ht_no_of_winmargin_ov1 <- length(which(rou_winmargin_vec_ht >= 1))
  rou_ht_no_of_winmargin_un0 <- length(which(rou_winmargin_vec_ht <= 0))
  rou_ht_no_of_winmargin_un1 <- length(which(rou_winmargin_vec_ht <= 1))
  #awayteam
  rou_winmargin_vec_at <- as.vector(rou_winmargin_h[rou_awayteamindex,])
  rou_winmargin_vec_at[is.na(rou_winmargin_vec_at)] <- ""
  rou_winmargin_vec_at <- rou_winmargin_vec_at[rou_winmargin_vec_at != ""]
  rou_winmargin_vec_at  <-tail(rou_winmargin_vec_at,6)
  rou_winmargin_vec_at  <- as.numeric(rou_winmargin_vec_at)

  rou_at_totalwinmargin <- sum(rou_winmargin_vec_at)
  rou_at_no_of_winmargin_ov0 <- length(which(rou_winmargin_vec_at >= 0))
  rou_at_no_of_winmargin_ov1 <- length(which(rou_winmargin_vec_at >= 1))
  rou_at_no_of_winmargin_un0 <- length(which(rou_winmargin_vec_at <= 0))
  rou_at_no_of_winmargin_un1 <- length(which(rou_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  rou_winmargin_vec_ht_lm <- as.vector(rou_winmargin_h[rou_hometeamindex,])
  rou_winmargin_vec_ht_lm[is.na(rou_winmargin_vec_ht_lm)] <- ""
  rou_winmargin_vec_ht_lm <- rou_winmargin_vec_ht_lm[rou_winmargin_vec_ht_lm != ""]
  rou_winmargin_vec_ht_lm  <-tail(rou_winmargin_vec_ht_lm,1)
  #awayteam
  rou_winmargin_vec_at_lm <- as.vector(rou_winmargin_h[rou_awayteamindex,])
  rou_winmargin_vec_at_lm[is.na(rou_winmargin_vec_at_lm)] <- ""
  rou_winmargin_vec_at_lm <- rou_winmargin_vec_at_lm[rou_winmargin_vec_at_lm != ""]
  rou_winmargin_vec_at_lm  <-tail(rou_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  rou_ht_last6points <- rou_ht_numberof_wins*3 + rou_ht_numberof_draws*1
  rou_at_last6points <- rou_at_numberof_wins*3 + rou_at_numberof_draws*1

  if(rou_ht_last6points > rou_at_last6points) {rou_3waypick <- "1"}  else {rou_3waypick <- "X2"}

  if(rou_at_last6points > rou_ht_last6points ) {rou_3waypick <- "2"} else {rou_3waypick <- "1X"}

  if(rou_ht_no_of_ov25 + rou_at_no_of_ov25 >= 6) {rou_goalspick <- "ov25"} else {rou_goalspick <- "un25"}

  if(rou_ht_no_of_un25 + rou_at_no_of_un25 >= 6) {rou_goalspick <- "un25"} else {rou_goalspick <- "ov25"}

  if(rou_ht_matches_scoring >= 4 && rou_at_matches_scoring >=4) {rou_btts <- "BTTS-Y"} else {rou_btts <- "BTTS-N"}


  rou_prediction[rou_row] <- rbind(paste(rou_3waypick,rou_goalspick,rou_btts,sep = ","))
  rou_HWM[rou_row] <- rou_ht_totalwinmargin
  rou_AWM[rou_row] <- rou_at_totalwinmargin

  rou_HWMLM[rou_row] <- rou_winmargin_vec_ht_lm
  rou_AWMLM[rou_row] <- rou_winmargin_vec_at_lm


}

rou_prediction <- as.data.frame(rou_prediction)
colnames(rou_prediction) <- "prediction"

rou_HWM <- as.data.frame(rou_HWM)
colnames(rou_HWM) <- "HWM"

rou_AWM <- as.data.frame(rou_AWM)
colnames(rou_AWM) <- "AWM"

rou_HWMLM <- as.data.frame(rou_HWMLM)
colnames(rou_HWMLM) <- "HWMLM"

rou_AWMLM <- as.data.frame(rou_AWMLM)
colnames(rou_AWMLM) <- "AWMLM"

rou_picks <- cbind(ROU_fixtures$Div,ROU_fixtures$HomeTeam_rou,ROU_fixtures$AwayTeam_rou,rou_prediction,rou_HWM,rou_AWM,rou_HWMLM,rou_AWMLM)
colnames(rou_picks)[1] <- "picks_Div"
colnames(rou_picks)[2] <- "picks_HomeTeam"
colnames(rou_picks)[3] <- "picks_AwayTeam"
rou_picks$matchid <- paste(rou_picks$picks_HomeTeam,rou_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of ROU
rou_picks
############################################################################################
#end of ROU
#RUS
RUS_fixtures$Hometeam_rus_index <- match(RUS_fixtures$HomeTeam_rus,rus_teams)
RUS_fixtures$Awayteam_rus_index <- match(RUS_fixtures$AwayTeam_rus,rus_teams)
rus_prediction <- c()
rus_HWM <- c()
rus_AWM <- c()
rus_HWMLM <- c()
rus_AWMLM <- c()
for(rus_row in 1:nrow(RUS_fixtures))
{

  rus_hometeamindex <- RUS_fixtures[rus_row,"Hometeam_rus_index"]
  rus_awayteamindex <- RUS_fixtures[rus_row,"Awayteam_rus_index"]
  #analyse team form
  #home team
  rus_form_vec_ht <- as.vector(rus_form_h[rus_hometeamindex,])
  rus_form_vec_ht[is.na(rus_form_vec_ht)] <- ""
  rus_form_vec_ht <- rus_form_vec_ht[rus_form_vec_ht != ""]
  rus_form_vec_ht  <-tail(rus_form_vec_ht,6)
  rus_ht_numberof_wins <- length(which(rus_form_vec_ht == "W"))
  rus_ht_numberof_draws <- length(which(rus_form_vec_ht == "D"))
  rus_ht_numberof_loss <- length(which(rus_form_vec_ht == "L"))
  #awayteam
  rus_form_vec_at <- as.vector(rus_form_h[rus_awayteamindex,])
  rus_form_vec_at[is.na(rus_form_vec_at)] <- ""
  rus_form_vec_at <- rus_form_vec_at[rus_form_vec_at != ""]
  rus_form_vec_at  <-tail(rus_form_vec_at,6)
  rus_at_numberof_wins <- length(which(rus_form_vec_at == "W"))
  rus_at_numberof_draws <- length(which(rus_form_vec_at == "D"))
  rus_at_numberof_loss <- length(which(rus_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  rus_goalscored_vec_ht <- as.vector(rus_goalscored_h[rus_hometeamindex,])
  rus_goalscored_vec_ht[is.na(rus_goalscored_vec_ht)] <- ""
  rus_goalscored_vec_ht <- rus_goalscored_vec_ht[rus_goalscored_vec_ht != ""]
  rus_goalscored_vec_ht  <-tail(rus_goalscored_vec_ht,6)
  rus_goalscored_vec_ht  <- as.numeric(rus_goalscored_vec_ht)
  rus_ht_totalgoalscored <- sum(rus_goalscored_vec_ht)
  rus_ht_matches_scoring <- length(which(rus_goalscored_vec_ht > 0))
  rus_ht_matches_without_scoring <- length(which(rus_goalscored_vec_ht == "0"))
  #awayteam
  rus_goalscored_vec_at <- as.vector(rus_goalscored_h[rus_awayteamindex,])
  rus_goalscored_vec_at[is.na(rus_goalscored_vec_at)] <- ""
  rus_goalscored_vec_at <- rus_goalscored_vec_at[rus_goalscored_vec_at != ""]
  rus_goalscored_vec_at  <-tail(rus_goalscored_vec_at,6)
  rus_goalscored_vec_at  <- as.numeric(rus_goalscored_vec_at)
  rus_at_totalgoalscored <- sum(rus_goalscored_vec_at)
  rus_at_matches_scoring <- length(which(rus_goalscored_vec_at > 0))
  rus_at_matches_without_scoring <- length(which(rus_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  rus_goalconceded_vec_ht <- as.vector(rus_goalconceded_h[rus_hometeamindex,])
  rus_goalconceded_vec_ht[is.na(rus_goalconceded_vec_ht)] <- ""
  rus_goalconceded_vec_ht <- rus_goalconceded_vec_ht[rus_goalconceded_vec_ht != ""]
  rus_goalconceded_vec_ht  <-tail(rus_goalconceded_vec_ht,6)
  rus_goalconceded_vec_ht  <- as.numeric(rus_goalconceded_vec_ht)
  rus_goalconceded_vec_ht
  rus_ht_totalgoalconceded <- sum(rus_goalconceded_vec_ht)
  rus_ht_matches_concede <- length(which(rus_goalconceded_vec_ht > 0))
  rus_ht_matches_without_concede <- length(which(rus_goalconceded_vec_ht == "0"))
  #awayteam
  rus_goalconceded_vec_at <- as.vector(rus_goalconceded_h[rus_awayteamindex,])
  rus_goalconceded_vec_at[is.na(rus_goalconceded_vec_at)] <- ""
  rus_goalconceded_vec_at <- rus_goalconceded_vec_at[rus_goalconceded_vec_at != ""]
  rus_goalconceded_vec_at  <-tail(rus_goalconceded_vec_at,6)
  rus_goalconceded_vec_at  <- as.numeric(rus_goalconceded_vec_at)
  rus_at_totalgoalconceded <- sum(rus_goalconceded_vec_at)
  rus_at_matches_concede <- length(which(rus_goalconceded_vec_at > 0))
  rus_at_matches_without_concede <- length(which(rus_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  rus_totalgoals_vec_ht <- as.vector(rus_totalgoals_h[rus_hometeamindex,])
  rus_totalgoals_vec_ht[is.na(rus_totalgoals_vec_ht)] <- ""
  rus_totalgoals_vec_ht <- rus_totalgoals_vec_ht[rus_totalgoals_vec_ht != ""]
  rus_totalgoals_vec_ht  <-tail(rus_totalgoals_vec_ht,6)
  rus_totalgoals_vec_ht  <- as.numeric(rus_totalgoals_vec_ht)
  rus_totalgoals_vec_ht
  rus_ht_totalgoals <- sum(rus_totalgoals_vec_ht)
  rus_ht_avgtotalgoals <- (rus_ht_totalgoals/6)
  rus_ht_no_of_ov25 <- length(which(rus_totalgoals_vec_ht >= 3))
  rus_ht_no_of_un25 <- length(which(rus_totalgoals_vec_ht <= 2))
  #awayteam
  rus_totalgoals_vec_at <- as.vector(rus_totalgoals_h[rus_awayteamindex,])
  rus_totalgoals_vec_at[is.na(rus_totalgoals_vec_at)] <- ""
  rus_totalgoals_vec_at <- rus_totalgoals_vec_at[rus_totalgoals_vec_at != ""]
  rus_totalgoals_vec_at  <-tail(rus_totalgoals_vec_at,6)
  rus_totalgoals_vec_at  <- as.numeric(rus_totalgoals_vec_at)
  rus_totalgoals_vec_at
  rus_at_totalgoals <- sum(rus_totalgoals_vec_at)
  rus_at_avgtotalgoals <- (rus_at_totalgoals/6)
  rus_at_no_of_ov25 <- length(which(rus_totalgoals_vec_at >= 3))
  rus_at_no_of_un25 <- length(which(rus_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  rus_winmargin_vec_ht <- as.vector(rus_winmargin_h[rus_hometeamindex,])
  rus_winmargin_vec_ht[is.na(rus_winmargin_vec_ht)] <- ""
  rus_winmargin_vec_ht <- rus_winmargin_vec_ht[rus_winmargin_vec_ht != ""]
  rus_winmargin_vec_ht  <-tail(rus_winmargin_vec_ht,6)
  rus_winmargin_vec_ht  <- as.numeric(rus_winmargin_vec_ht)

  rus_ht_totalwinmargin <- sum(rus_winmargin_vec_ht)
  rus_ht_no_of_winmargin_ov0 <- length(which(rus_winmargin_vec_ht >= 0))
  rus_ht_no_of_winmargin_ov1 <- length(which(rus_winmargin_vec_ht >= 1))
  rus_ht_no_of_winmargin_un0 <- length(which(rus_winmargin_vec_ht <= 0))
  rus_ht_no_of_winmargin_un1 <- length(which(rus_winmargin_vec_ht <= 1))
  #awayteam
  rus_winmargin_vec_at <- as.vector(rus_winmargin_h[rus_awayteamindex,])
  rus_winmargin_vec_at[is.na(rus_winmargin_vec_at)] <- ""
  rus_winmargin_vec_at <- rus_winmargin_vec_at[rus_winmargin_vec_at != ""]
  rus_winmargin_vec_at  <-tail(rus_winmargin_vec_at,6)
  rus_winmargin_vec_at  <- as.numeric(rus_winmargin_vec_at)

  rus_at_totalwinmargin <- sum(rus_winmargin_vec_at)
  rus_at_no_of_winmargin_ov0 <- length(which(rus_winmargin_vec_at >= 0))
  rus_at_no_of_winmargin_ov1 <- length(which(rus_winmargin_vec_at >= 1))
  rus_at_no_of_winmargin_un0 <- length(which(rus_winmargin_vec_at <= 0))
  rus_at_no_of_winmargin_un1 <- length(which(rus_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  rus_winmargin_vec_ht_lm <- as.vector(rus_winmargin_h[rus_hometeamindex,])
  rus_winmargin_vec_ht_lm[is.na(rus_winmargin_vec_ht_lm)] <- ""
  rus_winmargin_vec_ht_lm <- rus_winmargin_vec_ht_lm[rus_winmargin_vec_ht_lm != ""]
  rus_winmargin_vec_ht_lm  <-tail(rus_winmargin_vec_ht_lm,1)
  #awayteam
  rus_winmargin_vec_at_lm <- as.vector(rus_winmargin_h[rus_awayteamindex,])
  rus_winmargin_vec_at_lm[is.na(rus_winmargin_vec_at_lm)] <- ""
  rus_winmargin_vec_at_lm <- rus_winmargin_vec_at_lm[rus_winmargin_vec_at_lm != ""]
  rus_winmargin_vec_at_lm  <-tail(rus_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  rus_ht_last6points <- rus_ht_numberof_wins*3 + rus_ht_numberof_draws*1
  rus_at_last6points <- rus_at_numberof_wins*3 + rus_at_numberof_draws*1

  if(rus_ht_last6points > rus_at_last6points) {rus_3waypick <- "1"}  else {rus_3waypick <- "X2"}

  if(rus_at_last6points > rus_ht_last6points ) {rus_3waypick <- "2"} else {rus_3waypick <- "1X"}

  if(rus_ht_no_of_ov25 + rus_at_no_of_ov25 >= 6) {rus_goalspick <- "ov25"} else {rus_goalspick <- "un25"}

  if(rus_ht_no_of_un25 + rus_at_no_of_un25 >= 6) {rus_goalspick <- "un25"} else {rus_goalspick <- "ov25"}

  if(rus_ht_matches_scoring >= 4 && rus_at_matches_scoring >=4) {rus_btts <- "BTTS-Y"} else {rus_btts <- "BTTS-N"}


  rus_prediction[rus_row] <- rbind(paste(rus_3waypick,rus_goalspick,rus_btts,sep = ","))
  rus_HWM[rus_row] <- rus_ht_totalwinmargin
  rus_AWM[rus_row] <- rus_at_totalwinmargin

  rus_HWMLM[rus_row] <- rus_winmargin_vec_ht_lm
  rus_AWMLM[rus_row] <- rus_winmargin_vec_at_lm


}

rus_prediction <- as.data.frame(rus_prediction)
colnames(rus_prediction) <- "prediction"

rus_HWM <- as.data.frame(rus_HWM)
colnames(rus_HWM) <- "HWM"

rus_AWM <- as.data.frame(rus_AWM)
colnames(rus_AWM) <- "AWM"

rus_HWMLM <- as.data.frame(rus_HWMLM)
colnames(rus_HWMLM) <- "HWMLM"

rus_AWMLM <- as.data.frame(rus_AWMLM)
colnames(rus_AWMLM) <- "AWMLM"

rus_picks <- cbind(RUS_fixtures$Div,RUS_fixtures$HomeTeam_rus,RUS_fixtures$AwayTeam_rus,rus_prediction,rus_HWM,rus_AWM,rus_HWMLM,rus_AWMLM)
colnames(rus_picks)[1] <- "picks_Div"
colnames(rus_picks)[2] <- "picks_HomeTeam"
colnames(rus_picks)[3] <- "picks_AwayTeam"
rus_picks$matchid <- paste(rus_picks$picks_HomeTeam,rus_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of RUS
rus_picks
############################################################################################
#end of RUS
#SWE
SWE_fixtures$Hometeam_swe_index <- match(SWE_fixtures$HomeTeam_swe,swe_teams)
SWE_fixtures$Awayteam_swe_index <- match(SWE_fixtures$AwayTeam_swe,swe_teams)
swe_prediction <- c()
swe_HWM <- c()
swe_AWM <- c()
swe_HWMLM <- c()
swe_AWMLM <- c()
for(swe_row in 1:nrow(SWE_fixtures))
{

  swe_hometeamindex <- SWE_fixtures[swe_row,"Hometeam_swe_index"]
  swe_awayteamindex <- SWE_fixtures[swe_row,"Awayteam_swe_index"]
  #analyse team form
  #home team
  swe_form_vec_ht <- as.vector(swe_form_h[swe_hometeamindex,])
  swe_form_vec_ht[is.na(swe_form_vec_ht)] <- ""
  swe_form_vec_ht <- swe_form_vec_ht[swe_form_vec_ht != ""]
  swe_form_vec_ht  <-tail(swe_form_vec_ht,6)
  swe_ht_numberof_wins <- length(which(swe_form_vec_ht == "W"))
  swe_ht_numberof_draws <- length(which(swe_form_vec_ht == "D"))
  swe_ht_numberof_loss <- length(which(swe_form_vec_ht == "L"))
  #awayteam
  swe_form_vec_at <- as.vector(swe_form_h[swe_awayteamindex,])
  swe_form_vec_at[is.na(swe_form_vec_at)] <- ""
  swe_form_vec_at <- swe_form_vec_at[swe_form_vec_at != ""]
  swe_form_vec_at  <-tail(swe_form_vec_at,6)
  swe_at_numberof_wins <- length(which(swe_form_vec_at == "W"))
  swe_at_numberof_draws <- length(which(swe_form_vec_at == "D"))
  swe_at_numberof_loss <- length(which(swe_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  swe_goalscored_vec_ht <- as.vector(swe_goalscored_h[swe_hometeamindex,])
  swe_goalscored_vec_ht[is.na(swe_goalscored_vec_ht)] <- ""
  swe_goalscored_vec_ht <- swe_goalscored_vec_ht[swe_goalscored_vec_ht != ""]
  swe_goalscored_vec_ht  <-tail(swe_goalscored_vec_ht,6)
  swe_goalscored_vec_ht  <- as.numeric(swe_goalscored_vec_ht)
  swe_ht_totalgoalscored <- sum(swe_goalscored_vec_ht)
  swe_ht_matches_scoring <- length(which(swe_goalscored_vec_ht > 0))
  swe_ht_matches_without_scoring <- length(which(swe_goalscored_vec_ht == "0"))
  #awayteam
  swe_goalscored_vec_at <- as.vector(swe_goalscored_h[swe_awayteamindex,])
  swe_goalscored_vec_at[is.na(swe_goalscored_vec_at)] <- ""
  swe_goalscored_vec_at <- swe_goalscored_vec_at[swe_goalscored_vec_at != ""]
  swe_goalscored_vec_at  <-tail(swe_goalscored_vec_at,6)
  swe_goalscored_vec_at  <- as.numeric(swe_goalscored_vec_at)
  swe_at_totalgoalscored <- sum(swe_goalscored_vec_at)
  swe_at_matches_scoring <- length(which(swe_goalscored_vec_at > 0))
  swe_at_matches_without_scoring <- length(which(swe_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  swe_goalconceded_vec_ht <- as.vector(swe_goalconceded_h[swe_hometeamindex,])
  swe_goalconceded_vec_ht[is.na(swe_goalconceded_vec_ht)] <- ""
  swe_goalconceded_vec_ht <- swe_goalconceded_vec_ht[swe_goalconceded_vec_ht != ""]
  swe_goalconceded_vec_ht  <-tail(swe_goalconceded_vec_ht,6)
  swe_goalconceded_vec_ht  <- as.numeric(swe_goalconceded_vec_ht)
  swe_goalconceded_vec_ht
  swe_ht_totalgoalconceded <- sum(swe_goalconceded_vec_ht)
  swe_ht_matches_concede <- length(which(swe_goalconceded_vec_ht > 0))
  swe_ht_matches_without_concede <- length(which(swe_goalconceded_vec_ht == "0"))
  #awayteam
  swe_goalconceded_vec_at <- as.vector(swe_goalconceded_h[swe_awayteamindex,])
  swe_goalconceded_vec_at[is.na(swe_goalconceded_vec_at)] <- ""
  swe_goalconceded_vec_at <- swe_goalconceded_vec_at[swe_goalconceded_vec_at != ""]
  swe_goalconceded_vec_at  <-tail(swe_goalconceded_vec_at,6)
  swe_goalconceded_vec_at  <- as.numeric(swe_goalconceded_vec_at)
  swe_at_totalgoalconceded <- sum(swe_goalconceded_vec_at)
  swe_at_matches_concede <- length(which(swe_goalconceded_vec_at > 0))
  swe_at_matches_without_concede <- length(which(swe_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  swe_totalgoals_vec_ht <- as.vector(swe_totalgoals_h[swe_hometeamindex,])
  swe_totalgoals_vec_ht[is.na(swe_totalgoals_vec_ht)] <- ""
  swe_totalgoals_vec_ht <- swe_totalgoals_vec_ht[swe_totalgoals_vec_ht != ""]
  swe_totalgoals_vec_ht  <-tail(swe_totalgoals_vec_ht,6)
  swe_totalgoals_vec_ht  <- as.numeric(swe_totalgoals_vec_ht)
  swe_totalgoals_vec_ht
  swe_ht_totalgoals <- sum(swe_totalgoals_vec_ht)
  swe_ht_avgtotalgoals <- (swe_ht_totalgoals/6)
  swe_ht_no_of_ov25 <- length(which(swe_totalgoals_vec_ht >= 3))
  swe_ht_no_of_un25 <- length(which(swe_totalgoals_vec_ht <= 2))
  #awayteam
  swe_totalgoals_vec_at <- as.vector(swe_totalgoals_h[swe_awayteamindex,])
  swe_totalgoals_vec_at[is.na(swe_totalgoals_vec_at)] <- ""
  swe_totalgoals_vec_at <- swe_totalgoals_vec_at[swe_totalgoals_vec_at != ""]
  swe_totalgoals_vec_at  <-tail(swe_totalgoals_vec_at,6)
  swe_totalgoals_vec_at  <- as.numeric(swe_totalgoals_vec_at)
  swe_totalgoals_vec_at
  swe_at_totalgoals <- sum(swe_totalgoals_vec_at)
  swe_at_avgtotalgoals <- (swe_at_totalgoals/6)
  swe_at_no_of_ov25 <- length(which(swe_totalgoals_vec_at >= 3))
  swe_at_no_of_un25 <- length(which(swe_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  swe_winmargin_vec_ht <- as.vector(swe_winmargin_h[swe_hometeamindex,])
  swe_winmargin_vec_ht[is.na(swe_winmargin_vec_ht)] <- ""
  swe_winmargin_vec_ht <- swe_winmargin_vec_ht[swe_winmargin_vec_ht != ""]
  swe_winmargin_vec_ht  <-tail(swe_winmargin_vec_ht,6)
  swe_winmargin_vec_ht  <- as.numeric(swe_winmargin_vec_ht)

  swe_ht_totalwinmargin <- sum(swe_winmargin_vec_ht)
  swe_ht_no_of_winmargin_ov0 <- length(which(swe_winmargin_vec_ht >= 0))
  swe_ht_no_of_winmargin_ov1 <- length(which(swe_winmargin_vec_ht >= 1))
  swe_ht_no_of_winmargin_un0 <- length(which(swe_winmargin_vec_ht <= 0))
  swe_ht_no_of_winmargin_un1 <- length(which(swe_winmargin_vec_ht <= 1))
  #awayteam
  swe_winmargin_vec_at <- as.vector(swe_winmargin_h[swe_awayteamindex,])
  swe_winmargin_vec_at[is.na(swe_winmargin_vec_at)] <- ""
  swe_winmargin_vec_at <- swe_winmargin_vec_at[swe_winmargin_vec_at != ""]
  swe_winmargin_vec_at  <-tail(swe_winmargin_vec_at,6)
  swe_winmargin_vec_at  <- as.numeric(swe_winmargin_vec_at)

  swe_at_totalwinmargin <- sum(swe_winmargin_vec_at)
  swe_at_no_of_winmargin_ov0 <- length(which(swe_winmargin_vec_at >= 0))
  swe_at_no_of_winmargin_ov1 <- length(which(swe_winmargin_vec_at >= 1))
  swe_at_no_of_winmargin_un0 <- length(which(swe_winmargin_vec_at <= 0))
  swe_at_no_of_winmargin_un1 <- length(which(swe_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  swe_winmargin_vec_ht_lm <- as.vector(swe_winmargin_h[swe_hometeamindex,])
  swe_winmargin_vec_ht_lm[is.na(swe_winmargin_vec_ht_lm)] <- ""
  swe_winmargin_vec_ht_lm <- swe_winmargin_vec_ht_lm[swe_winmargin_vec_ht_lm != ""]
  swe_winmargin_vec_ht_lm  <-tail(swe_winmargin_vec_ht_lm,1)
  #awayteam
  swe_winmargin_vec_at_lm <- as.vector(swe_winmargin_h[swe_awayteamindex,])
  swe_winmargin_vec_at_lm[is.na(swe_winmargin_vec_at_lm)] <- ""
  swe_winmargin_vec_at_lm <- swe_winmargin_vec_at_lm[swe_winmargin_vec_at_lm != ""]
  swe_winmargin_vec_at_lm  <-tail(swe_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  swe_ht_last6points <- swe_ht_numberof_wins*3 + swe_ht_numberof_draws*1
  swe_at_last6points <- swe_at_numberof_wins*3 + swe_at_numberof_draws*1

  if(swe_ht_last6points > swe_at_last6points) {swe_3waypick <- "1"}  else {swe_3waypick <- "X2"}

  if(swe_at_last6points > swe_ht_last6points ) {swe_3waypick <- "2"} else {swe_3waypick <- "1X"}

  if(swe_ht_no_of_ov25 + swe_at_no_of_ov25 >= 6) {swe_goalspick <- "ov25"} else {swe_goalspick <- "un25"}

  if(swe_ht_no_of_un25 + swe_at_no_of_un25 >= 6) {swe_goalspick <- "un25"} else {swe_goalspick <- "ov25"}

  if(swe_ht_matches_scoring >= 4 && swe_at_matches_scoring >=4) {swe_btts <- "BTTS-Y"} else {swe_btts <- "BTTS-N"}


  swe_prediction[swe_row] <- rbind(paste(swe_3waypick,swe_goalspick,swe_btts,sep = ","))
  swe_HWM[swe_row] <- swe_ht_totalwinmargin
  swe_AWM[swe_row] <- swe_at_totalwinmargin

  swe_HWMLM[swe_row] <- swe_winmargin_vec_ht_lm
  swe_AWMLM[swe_row] <- swe_winmargin_vec_at_lm


}

swe_prediction <- as.data.frame(swe_prediction)
colnames(swe_prediction) <- "prediction"

swe_HWM <- as.data.frame(swe_HWM)
colnames(swe_HWM) <- "HWM"

swe_AWM <- as.data.frame(swe_AWM)
colnames(swe_AWM) <- "AWM"

swe_HWMLM <- as.data.frame(swe_HWMLM)
colnames(swe_HWMLM) <- "HWMLM"

swe_AWMLM <- as.data.frame(swe_AWMLM)
colnames(swe_AWMLM) <- "AWMLM"

swe_picks <- cbind(SWE_fixtures$Div,SWE_fixtures$HomeTeam_swe,SWE_fixtures$AwayTeam_swe,swe_prediction,swe_HWM,swe_AWM,swe_HWMLM,swe_AWMLM)
colnames(swe_picks)[1] <- "picks_Div"
colnames(swe_picks)[2] <- "picks_HomeTeam"
colnames(swe_picks)[3] <- "picks_AwayTeam"
swe_picks$matchid <- paste(swe_picks$picks_HomeTeam,swe_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SWE
swe_picks
############################################################################################
#end of SWE
#SWZ
SWZ_fixtures$Hometeam_swz_index <- match(SWZ_fixtures$HomeTeam_swz,swz_teams)
SWZ_fixtures$Awayteam_swz_index <- match(SWZ_fixtures$AwayTeam_swz,swz_teams)
swz_prediction <- c()
swz_HWM <- c()
swz_AWM <- c()
swz_HWMLM <- c()
swz_AWMLM <- c()
for(swz_row in 1:nrow(SWZ_fixtures))
{

  swz_hometeamindex <- SWZ_fixtures[swz_row,"Hometeam_swz_index"]
  swz_awayteamindex <- SWZ_fixtures[swz_row,"Awayteam_swz_index"]
  #analyse team form
  #home team
  swz_form_vec_ht <- as.vector(swz_form_h[swz_hometeamindex,])
  swz_form_vec_ht[is.na(swz_form_vec_ht)] <- ""
  swz_form_vec_ht <- swz_form_vec_ht[swz_form_vec_ht != ""]
  swz_form_vec_ht  <-tail(swz_form_vec_ht,6)
  swz_ht_numberof_wins <- length(which(swz_form_vec_ht == "W"))
  swz_ht_numberof_draws <- length(which(swz_form_vec_ht == "D"))
  swz_ht_numberof_loss <- length(which(swz_form_vec_ht == "L"))
  #awayteam
  swz_form_vec_at <- as.vector(swz_form_h[swz_awayteamindex,])
  swz_form_vec_at[is.na(swz_form_vec_at)] <- ""
  swz_form_vec_at <- swz_form_vec_at[swz_form_vec_at != ""]
  swz_form_vec_at  <-tail(swz_form_vec_at,6)
  swz_at_numberof_wins <- length(which(swz_form_vec_at == "W"))
  swz_at_numberof_draws <- length(which(swz_form_vec_at == "D"))
  swz_at_numberof_loss <- length(which(swz_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  swz_goalscored_vec_ht <- as.vector(swz_goalscored_h[swz_hometeamindex,])
  swz_goalscored_vec_ht[is.na(swz_goalscored_vec_ht)] <- ""
  swz_goalscored_vec_ht <- swz_goalscored_vec_ht[swz_goalscored_vec_ht != ""]
  swz_goalscored_vec_ht  <-tail(swz_goalscored_vec_ht,6)
  swz_goalscored_vec_ht  <- as.numeric(swz_goalscored_vec_ht)
  swz_ht_totalgoalscored <- sum(swz_goalscored_vec_ht)
  swz_ht_matches_scoring <- length(which(swz_goalscored_vec_ht > 0))
  swz_ht_matches_without_scoring <- length(which(swz_goalscored_vec_ht == "0"))
  #awayteam
  swz_goalscored_vec_at <- as.vector(swz_goalscored_h[swz_awayteamindex,])
  swz_goalscored_vec_at[is.na(swz_goalscored_vec_at)] <- ""
  swz_goalscored_vec_at <- swz_goalscored_vec_at[swz_goalscored_vec_at != ""]
  swz_goalscored_vec_at  <-tail(swz_goalscored_vec_at,6)
  swz_goalscored_vec_at  <- as.numeric(swz_goalscored_vec_at)
  swz_at_totalgoalscored <- sum(swz_goalscored_vec_at)
  swz_at_matches_scoring <- length(which(swz_goalscored_vec_at > 0))
  swz_at_matches_without_scoring <- length(which(swz_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  swz_goalconceded_vec_ht <- as.vector(swz_goalconceded_h[swz_hometeamindex,])
  swz_goalconceded_vec_ht[is.na(swz_goalconceded_vec_ht)] <- ""
  swz_goalconceded_vec_ht <- swz_goalconceded_vec_ht[swz_goalconceded_vec_ht != ""]
  swz_goalconceded_vec_ht  <-tail(swz_goalconceded_vec_ht,6)
  swz_goalconceded_vec_ht  <- as.numeric(swz_goalconceded_vec_ht)
  swz_goalconceded_vec_ht
  swz_ht_totalgoalconceded <- sum(swz_goalconceded_vec_ht)
  swz_ht_matches_concede <- length(which(swz_goalconceded_vec_ht > 0))
  swz_ht_matches_without_concede <- length(which(swz_goalconceded_vec_ht == "0"))
  #awayteam
  swz_goalconceded_vec_at <- as.vector(swz_goalconceded_h[swz_awayteamindex,])
  swz_goalconceded_vec_at[is.na(swz_goalconceded_vec_at)] <- ""
  swz_goalconceded_vec_at <- swz_goalconceded_vec_at[swz_goalconceded_vec_at != ""]
  swz_goalconceded_vec_at  <-tail(swz_goalconceded_vec_at,6)
  swz_goalconceded_vec_at  <- as.numeric(swz_goalconceded_vec_at)
  swz_at_totalgoalconceded <- sum(swz_goalconceded_vec_at)
  swz_at_matches_concede <- length(which(swz_goalconceded_vec_at > 0))
  swz_at_matches_without_concede <- length(which(swz_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  swz_totalgoals_vec_ht <- as.vector(swz_totalgoals_h[swz_hometeamindex,])
  swz_totalgoals_vec_ht[is.na(swz_totalgoals_vec_ht)] <- ""
  swz_totalgoals_vec_ht <- swz_totalgoals_vec_ht[swz_totalgoals_vec_ht != ""]
  swz_totalgoals_vec_ht  <-tail(swz_totalgoals_vec_ht,6)
  swz_totalgoals_vec_ht  <- as.numeric(swz_totalgoals_vec_ht)
  swz_totalgoals_vec_ht
  swz_ht_totalgoals <- sum(swz_totalgoals_vec_ht)
  swz_ht_avgtotalgoals <- (swz_ht_totalgoals/6)
  swz_ht_no_of_ov25 <- length(which(swz_totalgoals_vec_ht >= 3))
  swz_ht_no_of_un25 <- length(which(swz_totalgoals_vec_ht <= 2))
  #awayteam
  swz_totalgoals_vec_at <- as.vector(swz_totalgoals_h[swz_awayteamindex,])
  swz_totalgoals_vec_at[is.na(swz_totalgoals_vec_at)] <- ""
  swz_totalgoals_vec_at <- swz_totalgoals_vec_at[swz_totalgoals_vec_at != ""]
  swz_totalgoals_vec_at  <-tail(swz_totalgoals_vec_at,6)
  swz_totalgoals_vec_at  <- as.numeric(swz_totalgoals_vec_at)
  swz_totalgoals_vec_at
  swz_at_totalgoals <- sum(swz_totalgoals_vec_at)
  swz_at_avgtotalgoals <- (swz_at_totalgoals/6)
  swz_at_no_of_ov25 <- length(which(swz_totalgoals_vec_at >= 3))
  swz_at_no_of_un25 <- length(which(swz_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  swz_winmargin_vec_ht <- as.vector(swz_winmargin_h[swz_hometeamindex,])
  swz_winmargin_vec_ht[is.na(swz_winmargin_vec_ht)] <- ""
  swz_winmargin_vec_ht <- swz_winmargin_vec_ht[swz_winmargin_vec_ht != ""]
  swz_winmargin_vec_ht  <-tail(swz_winmargin_vec_ht,6)
  swz_winmargin_vec_ht  <- as.numeric(swz_winmargin_vec_ht)

  swz_ht_totalwinmargin <- sum(swz_winmargin_vec_ht)
  swz_ht_no_of_winmargin_ov0 <- length(which(swz_winmargin_vec_ht >= 0))
  swz_ht_no_of_winmargin_ov1 <- length(which(swz_winmargin_vec_ht >= 1))
  swz_ht_no_of_winmargin_un0 <- length(which(swz_winmargin_vec_ht <= 0))
  swz_ht_no_of_winmargin_un1 <- length(which(swz_winmargin_vec_ht <= 1))
  #awayteam
  swz_winmargin_vec_at <- as.vector(swz_winmargin_h[swz_awayteamindex,])
  swz_winmargin_vec_at[is.na(swz_winmargin_vec_at)] <- ""
  swz_winmargin_vec_at <- swz_winmargin_vec_at[swz_winmargin_vec_at != ""]
  swz_winmargin_vec_at  <-tail(swz_winmargin_vec_at,6)
  swz_winmargin_vec_at  <- as.numeric(swz_winmargin_vec_at)

  swz_at_totalwinmargin <- sum(swz_winmargin_vec_at)
  swz_at_no_of_winmargin_ov0 <- length(which(swz_winmargin_vec_at >= 0))
  swz_at_no_of_winmargin_ov1 <- length(which(swz_winmargin_vec_at >= 1))
  swz_at_no_of_winmargin_un0 <- length(which(swz_winmargin_vec_at <= 0))
  swz_at_no_of_winmargin_un1 <- length(which(swz_winmargin_vec_at <= 1))
  ##################################################################################
  ##################################################################################
  ##################################################################################
  #very last win margin
  #hometeam
  swz_winmargin_vec_ht_lm <- as.vector(swz_winmargin_h[swz_hometeamindex,])
  swz_winmargin_vec_ht_lm[is.na(swz_winmargin_vec_ht_lm)] <- ""
  swz_winmargin_vec_ht_lm <- swz_winmargin_vec_ht_lm[swz_winmargin_vec_ht_lm != ""]
  swz_winmargin_vec_ht_lm  <-tail(swz_winmargin_vec_ht_lm,1)
  #awayteam
  swz_winmargin_vec_at_lm <- as.vector(swz_winmargin_h[swz_awayteamindex,])
  swz_winmargin_vec_at_lm[is.na(swz_winmargin_vec_at_lm)] <- ""
  swz_winmargin_vec_at_lm <- swz_winmargin_vec_at_lm[swz_winmargin_vec_at_lm != ""]
  swz_winmargin_vec_at_lm  <-tail(swz_winmargin_vec_at_lm,1)
  #################################################################################
  ####we need to decide ############
  #winner goals
  swz_ht_last6points <- swz_ht_numberof_wins*3 + swz_ht_numberof_draws*1
  swz_at_last6points <- swz_at_numberof_wins*3 + swz_at_numberof_draws*1

  if(swz_ht_last6points > swz_at_last6points) {swz_3waypick <- "1"}  else {swz_3waypick <- "X2"}

  if(swz_at_last6points > swz_ht_last6points ) {swz_3waypick <- "2"} else {swz_3waypick <- "1X"}

  if(swz_ht_no_of_ov25 + swz_at_no_of_ov25 >= 6) {swz_goalspick <- "ov25"} else {swz_goalspick <- "un25"}

  if(swz_ht_no_of_un25 + swz_at_no_of_un25 >= 6) {swz_goalspick <- "un25"} else {swz_goalspick <- "ov25"}

  if(swz_ht_matches_scoring >= 4 && swz_at_matches_scoring >=4) {swz_btts <- "BTTS-Y"} else {swz_btts <- "BTTS-N"}


  swz_prediction[swz_row] <- rbind(paste(swz_3waypick,swz_goalspick,swz_btts,sep = ","))
  swz_HWM[swz_row] <- swz_ht_totalwinmargin
  swz_AWM[swz_row] <- swz_at_totalwinmargin

  swz_HWMLM[swz_row] <- swz_winmargin_vec_ht_lm
  swz_AWMLM[swz_row] <- swz_winmargin_vec_at_lm


}

swz_prediction <- as.data.frame(swz_prediction)
colnames(swz_prediction) <- "prediction"

swz_HWM <- as.data.frame(swz_HWM)
colnames(swz_HWM) <- "HWM"

swz_AWM <- as.data.frame(swz_AWM)
colnames(swz_AWM) <- "AWM"

swz_HWMLM <- as.data.frame(swz_HWMLM)
colnames(swz_HWMLM) <- "HWMLM"

swz_AWMLM <- as.data.frame(swz_AWMLM)
colnames(swz_AWMLM) <- "AWMLM"

swz_picks <- cbind(SWZ_fixtures$Div,SWZ_fixtures$HomeTeam_swz,SWZ_fixtures$AwayTeam_swz,swz_prediction,swz_HWM,swz_AWM,swz_HWMLM,swz_AWMLM)
colnames(swz_picks)[1] <- "picks_Div"
colnames(swz_picks)[2] <- "picks_HomeTeam"
colnames(swz_picks)[3] <- "picks_AwayTeam"
swz_picks$matchid <- paste(swz_picks$picks_HomeTeam,swz_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SWZ
swz_picks
############################################################################################
#end of SWZ
############################################################################################
#combine picks from divisions
allteams20212022picks_nl <- rbind(aut_picks,arg_picks,bra_picks,chn_picks,dnk_picks,fin_picks,irl_picks,jpn_picks,mex_picks,mls_picks,nor_picks,pol_picks,rou_picks,rus_picks,swe_picks,swz_picks)
#join the data
#######myodds file##########
myodds_fixtures_nl <- readxl::read_excel('../FDAS/myodds_20212022_newleagues.xlsx', sheet = '3way')
myodds_fixtures_nl$matchid <- paste(myodds_fixtures_nl$HT,myodds_fixtures_nl$AT, sep = "-")
myodds_fixtures_prediction_nl <- dplyr::left_join(myodds_fixtures_nl,allteams20212022picks_nl)
write.csv(myodds_fixtures_prediction_nl,'myodds_fixtures_prediction_nl.csv')
############################
picks_fixtures_nl <- read.csv('myfixturesnewleagues.csv')
picks_fixtures_nl$matchid <- paste(picks_fixtures_nl$Home_Team,picks_fixtures_nl$Away_Team, sep = "-")
picks_fixtures_prediction_nl <- dplyr::left_join(picks_fixtures_nl,allteams20212022picks_nl)
write.csv(picks_fixtures_prediction_nl,'picks_fixtures_prediction_nl.csv')
###########################
#reset allteams20212022picks
rm(allteams20212022picks_nl)




