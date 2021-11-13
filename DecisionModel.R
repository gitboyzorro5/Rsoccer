library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')

unlink('myodds_fixtures_prediction.csv')
unlink('picks_fixtures_prediction.csv')
################################################################
#B1
B1_fixtures$Hometeam_b1_index <- match(B1_fixtures$HomeTeam_b1,b1_teams)
B1_fixtures$Awayteam_b1_index <- match(B1_fixtures$AwayTeam_b1,b1_teams)
b1_prediction <- c()
for(b1_row in 1:nrow(B1_fixtures))
{

b1_hometeamindex <- B1_fixtures[b1_row,"Hometeam_b1_index"]
b1_awayteamindex <- B1_fixtures[b1_row,"Awayteam_b1_index"]
#analyse team form
#home team
b1_form_vec_ht <- as.vector(b1_form_h[b1_hometeamindex,])
b1_form_vec_ht[is.na(b1_form_vec_ht)] <- ""
b1_form_vec_ht <- b1_form_vec_ht[b1_form_vec_ht != ""]
b1_form_vec_ht  <-tail(b1_form_vec_ht,6)
b1_ht_numberof_wins <- length(which(b1_form_vec_ht == "W"))
b1_ht_numberof_draws <- length(which(b1_form_vec_ht == "D"))
b1_ht_numberof_loss <- length(which(b1_form_vec_ht == "L"))
#awayteam
b1_form_vec_at <- as.vector(b1_form_h[b1_awayteamindex,])
b1_form_vec_at[is.na(b1_form_vec_at)] <- ""
b1_form_vec_at <- b1_form_vec_at[b1_form_vec_at != ""]
b1_form_vec_at  <-tail(b1_form_vec_at,6)
b1_at_numberof_wins <- length(which(b1_form_vec_at == "W"))
b1_at_numberof_draws <- length(which(b1_form_vec_at == "D"))
b1_at_numberof_loss <- length(which(b1_form_vec_at == "L"))

######################################################################
#analyse goals scored
#hometeam
b1_goalscored_vec_ht <- as.vector(b1_goalscored_h[b1_hometeamindex,])
b1_goalscored_vec_ht[is.na(b1_goalscored_vec_ht)] <- ""
b1_goalscored_vec_ht <- b1_goalscored_vec_ht[b1_goalscored_vec_ht != ""]
b1_goalscored_vec_ht  <-tail(b1_goalscored_vec_ht,6)
b1_goalscored_vec_ht  <- as.numeric(b1_goalscored_vec_ht)
b1_ht_totalgoalscored <- sum(b1_goalscored_vec_ht)
b1_ht_matches_scoring <- length(which(b1_goalscored_vec_ht > 0))
b1_ht_matches_without_scoring <- length(which(b1_goalscored_vec_ht == "0"))
#awayteam
b1_goalscored_vec_at <- as.vector(b1_goalscored_h[b1_awayteamindex,])
b1_goalscored_vec_at[is.na(b1_goalscored_vec_at)] <- ""
b1_goalscored_vec_at <- b1_goalscored_vec_at[b1_goalscored_vec_at != ""]
b1_goalscored_vec_at  <-tail(b1_goalscored_vec_at,6)
b1_goalscored_vec_at  <- as.numeric(b1_goalscored_vec_at)
b1_at_totalgoalscored <- sum(b1_goalscored_vec_at)
b1_at_matches_scoring <- length(which(b1_goalscored_vec_at > 0))
b1_at_matches_without_scoring <- length(which(b1_goalscored_vec_at == "0"))
#####################################################################################
#analyse goals conceded
#hometeam
b1_goalconceded_vec_ht <- as.vector(b1_goalconceded_h[b1_hometeamindex,])
b1_goalconceded_vec_ht[is.na(b1_goalconceded_vec_ht)] <- ""
b1_goalconceded_vec_ht <- b1_goalconceded_vec_ht[b1_goalconceded_vec_ht != ""]
b1_goalconceded_vec_ht  <-tail(b1_goalconceded_vec_ht,6)
b1_goalconceded_vec_ht  <- as.numeric(b1_goalconceded_vec_ht)
b1_goalconceded_vec_ht
b1_ht_totalgoalconceded <- sum(b1_goalconceded_vec_ht)
b1_ht_matches_concede <- length(which(b1_goalconceded_vec_ht > 0))
b1_ht_matches_without_concede <- length(which(b1_goalconceded_vec_ht == "0"))
#awayteam
b1_goalconceded_vec_at <- as.vector(b1_goalconceded_h[b1_awayteamindex,])
b1_goalconceded_vec_at[is.na(b1_goalconceded_vec_at)] <- ""
b1_goalconceded_vec_at <- b1_goalconceded_vec_at[b1_goalconceded_vec_at != ""]
b1_goalconceded_vec_at  <-tail(b1_goalconceded_vec_at,6)
b1_goalconceded_vec_at  <- as.numeric(b1_goalconceded_vec_at)
b1_at_totalgoalconceded <- sum(b1_goalconceded_vec_at)
b1_at_matches_concede <- length(which(b1_goalconceded_vec_at > 0))
b1_at_matches_without_concede <- length(which(b1_goalconceded_vec_at == "0"))

####################################################################################
#analyse total combined goals
#hometeam
b1_totalgoals_vec_ht <- as.vector(b1_totalgoals_h[b1_hometeamindex,])
b1_totalgoals_vec_ht[is.na(b1_totalgoals_vec_ht)] <- ""
b1_totalgoals_vec_ht <- b1_totalgoals_vec_ht[b1_totalgoals_vec_ht != ""]
b1_totalgoals_vec_ht  <-tail(b1_totalgoals_vec_ht,6)
b1_totalgoals_vec_ht  <- as.numeric(b1_totalgoals_vec_ht)
b1_totalgoals_vec_ht
b1_ht_totalgoals <- sum(b1_totalgoals_vec_ht)
b1_ht_avgtotalgoals <- (b1_ht_totalgoals/6)
b1_ht_no_of_ov25 <- length(which(b1_totalgoals_vec_ht >= 3))
b1_ht_no_of_un25 <- length(which(b1_totalgoals_vec_ht <= 2))
#awayteam
b1_totalgoals_vec_at <- as.vector(b1_totalgoals_h[b1_awayteamindex,])
b1_totalgoals_vec_at[is.na(b1_totalgoals_vec_at)] <- ""
b1_totalgoals_vec_at <- b1_totalgoals_vec_at[b1_totalgoals_vec_at != ""]
b1_totalgoals_vec_at  <-tail(b1_totalgoals_vec_at,6)
b1_totalgoals_vec_at  <- as.numeric(b1_totalgoals_vec_at)
b1_totalgoals_vec_at
b1_at_totalgoals <- sum(b1_totalgoals_vec_at)
b1_at_avgtotalgoals <- (b1_at_totalgoals/6)
b1_at_no_of_ov25 <- length(which(b1_totalgoals_vec_at >= 3))
b1_at_no_of_un25 <- length(which(b1_totalgoals_vec_at <= 2))
################################################################################
#analyse win margin
#hometeam
b1_winmargin_vec_ht <- as.vector(b1_winmargin_h[b1_hometeamindex,])
b1_winmargin_vec_ht[is.na(b1_winmargin_vec_ht)] <- ""
b1_winmargin_vec_ht <- b1_winmargin_vec_ht[b1_winmargin_vec_ht != ""]
b1_winmargin_vec_ht  <-tail(b1_winmargin_vec_ht,6)
b1_winmargin_vec_ht  <- as.numeric(b1_winmargin_vec_ht)

b1_ht_totalwinmargin <- sum(b1_winmargin_vec_ht)
b1_ht_no_of_winmargin_ov0 <- length(which(b1_winmargin_vec_ht >= 0))
b1_ht_no_of_winmargin_ov1 <- length(which(b1_winmargin_vec_ht >= 1))
b1_ht_no_of_winmargin_un0 <- length(which(b1_winmargin_vec_ht <= 0))
b1_ht_no_of_winmargin_un1 <- length(which(b1_winmargin_vec_ht <= 1))
#awayteam
b1_winmargin_vec_at <- as.vector(b1_winmargin_h[b1_awayteamindex,])
b1_winmargin_vec_at[is.na(b1_winmargin_vec_at)] <- ""
b1_winmargin_vec_at <- b1_winmargin_vec_at[b1_winmargin_vec_at != ""]
b1_winmargin_vec_at  <-tail(b1_winmargin_vec_at,6)
b1_winmargin_vec_at  <- as.numeric(b1_winmargin_vec_at)

b1_at_totalwinmargin <- sum(b1_winmargin_vec_at)
b1_at_no_of_winmargin_ov0 <- length(which(b1_winmargin_vec_at >= 0))
b1_at_no_of_winmargin_ov1 <- length(which(b1_winmargin_vec_at >= 1))
b1_at_no_of_winmargin_un0 <- length(which(b1_winmargin_vec_at <= 0))
b1_at_no_of_winmargin_un1 <- length(which(b1_winmargin_vec_at <= 1))
##################################################################################
##########################################
####we need to decide ############
#winner goals
b1_ht_last6points <- b1_ht_numberof_wins*3 + b1_ht_numberof_draws*1
b1_at_last6points <- b1_at_numberof_wins*3 + b1_at_numberof_draws*1

ifelse(b1_ht_last6points > b1_at_last6points && b1_ht_totalwinmargin > b1_at_totalwinmargin,b1_3waypick <- "1",b1_3waypick <- "X2")

ifelse(b1_at_last6points > b1_ht_last6points && b1_at_totalwinmargin > b1_ht_totalwinmargin,b1_3waypick <- "2",b1_3waypick <- "1X")

if(b1_ht_no_of_ov25 + b1_at_no_of_ov25 >= 6) {b1_goalspick <- "ov25"} else {b1_goalspick <- "un25"}

if(b1_ht_no_of_un25 + b1_at_no_of_un25 >= 6) {b1_goalspick <- "un25"} else {b1_goalspick <- "ov25"}


b1_prediction[b1_row] <- rbind(paste(b1_3waypick,b1_goalspick,sep = ","))

}

b1_prediction <- as.data.frame(b1_prediction)
colnames(b1_prediction) <- "prediction"
b1_prediction

b1_picks <- cbind(B1_fixtures$Div,B1_fixtures$HomeTeam_b1,B1_fixtures$AwayTeam_b1,b1_prediction)
colnames(b1_picks)[1] <- "picks_Div"
colnames(b1_picks)[2] <- "picks_HomeTeam"
colnames(b1_picks)[3] <- "picks_AwayTeam"
b1_picks$matchid <- paste(b1_picks$picks_HomeTeam,b1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of B1
############################################################################################
#D1
D1_fixtures$Hometeam_d1_index <- match(D1_fixtures$HomeTeam_d1,d1_teams)
D1_fixtures$Awayteam_d1_index <- match(D1_fixtures$AwayTeam_d1,d1_teams)
d1_prediction <- c()
for(d1_row in 1:nrow(D1_fixtures))
{

  d1_hometeamindex <- D1_fixtures[d1_row,"Hometeam_d1_index"]
  d1_awayteamindex <- D1_fixtures[d1_row,"Awayteam_d1_index"]
  #analyse team form
  #home team
  d1_form_vec_ht <- as.vector(d1_form_h[d1_hometeamindex,])
  d1_form_vec_ht[is.na(d1_form_vec_ht)] <- ""
  d1_form_vec_ht <- d1_form_vec_ht[d1_form_vec_ht != ""]
  d1_form_vec_ht  <-tail(d1_form_vec_ht,6)
  d1_ht_numberof_wins <- length(which(d1_form_vec_ht == "W"))
  d1_ht_numberof_draws <- length(which(d1_form_vec_ht == "D"))
  d1_ht_numberof_loss <- length(which(d1_form_vec_ht == "L"))
  #awayteam
  d1_form_vec_at <- as.vector(d1_form_h[d1_awayteamindex,])
  d1_form_vec_at[is.na(d1_form_vec_at)] <- ""
  d1_form_vec_at <- d1_form_vec_at[d1_form_vec_at != ""]
  d1_form_vec_at  <-tail(d1_form_vec_at,6)
  d1_at_numberof_wins <- length(which(d1_form_vec_at == "W"))
  d1_at_numberof_draws <- length(which(d1_form_vec_at == "D"))
  d1_at_numberof_loss <- length(which(d1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  d1_goalscored_vec_ht <- as.vector(d1_goalscored_h[d1_hometeamindex,])
  d1_goalscored_vec_ht[is.na(d1_goalscored_vec_ht)] <- ""
  d1_goalscored_vec_ht <- d1_goalscored_vec_ht[d1_goalscored_vec_ht != ""]
  d1_goalscored_vec_ht  <-tail(d1_goalscored_vec_ht,6)
  d1_goalscored_vec_ht  <- as.numeric(d1_goalscored_vec_ht)
  d1_ht_totalgoalscored <- sum(d1_goalscored_vec_ht)
  d1_ht_matches_scoring <- length(which(d1_goalscored_vec_ht > 0))
  d1_ht_matches_without_scoring <- length(which(d1_goalscored_vec_ht == "0"))
  #awayteam
  d1_goalscored_vec_at <- as.vector(d1_goalscored_h[d1_awayteamindex,])
  d1_goalscored_vec_at[is.na(d1_goalscored_vec_at)] <- ""
  d1_goalscored_vec_at <- d1_goalscored_vec_at[d1_goalscored_vec_at != ""]
  d1_goalscored_vec_at  <-tail(d1_goalscored_vec_at,6)
  d1_goalscored_vec_at  <- as.numeric(d1_goalscored_vec_at)
  d1_at_totalgoalscored <- sum(d1_goalscored_vec_at)
  d1_at_matches_scoring <- length(which(d1_goalscored_vec_at > 0))
  d1_at_matches_without_scoring <- length(which(d1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  d1_goalconceded_vec_ht <- as.vector(d1_goalconceded_h[d1_hometeamindex,])
  d1_goalconceded_vec_ht[is.na(d1_goalconceded_vec_ht)] <- ""
  d1_goalconceded_vec_ht <- d1_goalconceded_vec_ht[d1_goalconceded_vec_ht != ""]
  d1_goalconceded_vec_ht  <-tail(d1_goalconceded_vec_ht,6)
  d1_goalconceded_vec_ht  <- as.numeric(d1_goalconceded_vec_ht)
  d1_goalconceded_vec_ht
  d1_ht_totalgoalconceded <- sum(d1_goalconceded_vec_ht)
  d1_ht_matches_concede <- length(which(d1_goalconceded_vec_ht > 0))
  d1_ht_matches_without_concede <- length(which(d1_goalconceded_vec_ht == "0"))
  #awayteam
  d1_goalconceded_vec_at <- as.vector(d1_goalconceded_h[d1_awayteamindex,])
  d1_goalconceded_vec_at[is.na(d1_goalconceded_vec_at)] <- ""
  d1_goalconceded_vec_at <- d1_goalconceded_vec_at[d1_goalconceded_vec_at != ""]
  d1_goalconceded_vec_at  <-tail(d1_goalconceded_vec_at,6)
  d1_goalconceded_vec_at  <- as.numeric(d1_goalconceded_vec_at)
  d1_at_totalgoalconceded <- sum(d1_goalconceded_vec_at)
  d1_at_matches_concede <- length(which(d1_goalconceded_vec_at > 0))
  d1_at_matches_without_concede <- length(which(d1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  d1_totalgoals_vec_ht <- as.vector(d1_totalgoals_h[d1_hometeamindex,])
  d1_totalgoals_vec_ht[is.na(d1_totalgoals_vec_ht)] <- ""
  d1_totalgoals_vec_ht <- d1_totalgoals_vec_ht[d1_totalgoals_vec_ht != ""]
  d1_totalgoals_vec_ht  <-tail(d1_totalgoals_vec_ht,6)
  d1_totalgoals_vec_ht  <- as.numeric(d1_totalgoals_vec_ht)
  d1_totalgoals_vec_ht
  d1_ht_totalgoals <- sum(d1_totalgoals_vec_ht)
  d1_ht_avgtotalgoals <- (d1_ht_totalgoals/6)
  d1_ht_no_of_ov25 <- length(which(d1_totalgoals_vec_ht >= 3))
  d1_ht_no_of_un25 <- length(which(d1_totalgoals_vec_ht <= 2))
  #awayteam
  d1_totalgoals_vec_at <- as.vector(d1_totalgoals_h[d1_awayteamindex,])
  d1_totalgoals_vec_at[is.na(d1_totalgoals_vec_at)] <- ""
  d1_totalgoals_vec_at <- d1_totalgoals_vec_at[d1_totalgoals_vec_at != ""]
  d1_totalgoals_vec_at  <-tail(d1_totalgoals_vec_at,6)
  d1_totalgoals_vec_at  <- as.numeric(d1_totalgoals_vec_at)
  d1_totalgoals_vec_at
  d1_at_totalgoals <- sum(d1_totalgoals_vec_at)
  d1_at_avgtotalgoals <- (d1_at_totalgoals/6)
  d1_at_no_of_ov25 <- length(which(d1_totalgoals_vec_at >= 3))
  d1_at_no_of_un25 <- length(which(d1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  d1_winmargin_vec_ht <- as.vector(d1_winmargin_h[d1_hometeamindex,])
  d1_winmargin_vec_ht[is.na(d1_winmargin_vec_ht)] <- ""
  d1_winmargin_vec_ht <- d1_winmargin_vec_ht[d1_winmargin_vec_ht != ""]
  d1_winmargin_vec_ht  <-tail(d1_winmargin_vec_ht,6)
  d1_winmargin_vec_ht  <- as.numeric(d1_winmargin_vec_ht)

  d1_ht_totalwinmargin <- sum(d1_winmargin_vec_ht)
  d1_ht_no_of_winmargin_ov0 <- length(which(d1_winmargin_vec_ht >= 0))
  d1_ht_no_of_winmargin_ov1 <- length(which(d1_winmargin_vec_ht >= 1))
  d1_ht_no_of_winmargin_un0 <- length(which(d1_winmargin_vec_ht <= 0))
  d1_ht_no_of_winmargin_un1 <- length(which(d1_winmargin_vec_ht <= 1))
  #awayteam
  d1_winmargin_vec_at <- as.vector(d1_winmargin_h[d1_awayteamindex,])
  d1_winmargin_vec_at[is.na(d1_winmargin_vec_at)] <- ""
  d1_winmargin_vec_at <- d1_winmargin_vec_at[d1_winmargin_vec_at != ""]
  d1_winmargin_vec_at  <-tail(d1_winmargin_vec_at,6)
  d1_winmargin_vec_at  <- as.numeric(d1_winmargin_vec_at)

  d1_at_totalwinmargin <- sum(d1_winmargin_vec_at)
  d1_at_no_of_winmargin_ov0 <- length(which(d1_winmargin_vec_at >= 0))
  d1_at_no_of_winmargin_ov1 <- length(which(d1_winmargin_vec_at >= 1))
  d1_at_no_of_winmargin_un0 <- length(which(d1_winmargin_vec_at <= 0))
  d1_at_no_of_winmargin_un1 <- length(which(d1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  d1_ht_last6points <- d1_ht_numberof_wins*3 + d1_ht_numberof_draws*1
  d1_at_last6points <- d1_at_numberof_wins*3 + d1_at_numberof_draws*1

  ifelse(d1_ht_last6points > d1_at_last6points && d1_ht_totalwinmargin > d1_at_totalwinmargin,d1_3waypick <- "1",d1_3waypick <- "X2")

  ifelse(d1_at_last6points > d1_ht_last6points && d1_at_totalwinmargin > d1_ht_totalwinmargin,d1_3waypick <- "2",d1_3waypick <- "1X")

  if(d1_ht_no_of_ov25 + d1_at_no_of_ov25 >= 6) {d1_goalspick <- "ov25"} else {d1_goalspick <- "un25"}

  if(d1_ht_no_of_un25 + d1_at_no_of_un25 >= 6) {d1_goalspick <- "un25"} else {d1_goalspick <- "ov25"}

  d1_prediction[d1_row] <- rbind(paste(d1_3waypick,d1_goalspick,sep = ","))

}

d1_prediction <- as.data.frame(d1_prediction)
colnames(d1_prediction) <- "prediction"
d1_prediction

d1_picks <- cbind(D1_fixtures$Div,D1_fixtures$HomeTeam_d1,D1_fixtures$AwayTeam_d1,d1_prediction)
colnames(d1_picks)[1] <- "picks_Div"
colnames(d1_picks)[2] <- "picks_HomeTeam"
colnames(d1_picks)[3] <- "picks_AwayTeam"
d1_picks$matchid <- paste(d1_picks$picks_HomeTeam,d1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of D1
#D2
D2_fixtures$Hometeam_d2_index <- match(D2_fixtures$HomeTeam_d2,d2_teams)
D2_fixtures$Awayteam_d2_index <- match(D2_fixtures$AwayTeam_d2,d2_teams)
d2_prediction <- c()
for(d2_row in 1:nrow(D2_fixtures))
{

  d2_hometeamindex <- D2_fixtures[d2_row,"Hometeam_d2_index"]
  d2_awayteamindex <- D2_fixtures[d2_row,"Awayteam_d2_index"]
  #analyse team form
  #home team
  d2_form_vec_ht <- as.vector(d2_form_h[d2_hometeamindex,])
  d2_form_vec_ht[is.na(d2_form_vec_ht)] <- ""
  d2_form_vec_ht <- d2_form_vec_ht[d2_form_vec_ht != ""]
  d2_form_vec_ht  <-tail(d2_form_vec_ht,6)
  d2_ht_numberof_wins <- length(which(d2_form_vec_ht == "W"))
  d2_ht_numberof_draws <- length(which(d2_form_vec_ht == "D"))
  d2_ht_numberof_loss <- length(which(d2_form_vec_ht == "L"))
  #awayteam
  d2_form_vec_at <- as.vector(d2_form_h[d2_awayteamindex,])
  d2_form_vec_at[is.na(d2_form_vec_at)] <- ""
  d2_form_vec_at <- d2_form_vec_at[d2_form_vec_at != ""]
  d2_form_vec_at  <-tail(d2_form_vec_at,6)
  d2_at_numberof_wins <- length(which(d2_form_vec_at == "W"))
  d2_at_numberof_draws <- length(which(d2_form_vec_at == "D"))
  d2_at_numberof_loss <- length(which(d2_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  d2_goalscored_vec_ht <- as.vector(d2_goalscored_h[d2_hometeamindex,])
  d2_goalscored_vec_ht[is.na(d2_goalscored_vec_ht)] <- ""
  d2_goalscored_vec_ht <- d2_goalscored_vec_ht[d2_goalscored_vec_ht != ""]
  d2_goalscored_vec_ht  <-tail(d2_goalscored_vec_ht,6)
  d2_goalscored_vec_ht  <- as.numeric(d2_goalscored_vec_ht)
  d2_ht_totalgoalscored <- sum(d2_goalscored_vec_ht)
  d2_ht_matches_scoring <- length(which(d2_goalscored_vec_ht > 0))
  d2_ht_matches_without_scoring <- length(which(d2_goalscored_vec_ht == "0"))
  #awayteam
  d2_goalscored_vec_at <- as.vector(d2_goalscored_h[d2_awayteamindex,])
  d2_goalscored_vec_at[is.na(d2_goalscored_vec_at)] <- ""
  d2_goalscored_vec_at <- d2_goalscored_vec_at[d2_goalscored_vec_at != ""]
  d2_goalscored_vec_at  <-tail(d2_goalscored_vec_at,6)
  d2_goalscored_vec_at  <- as.numeric(d2_goalscored_vec_at)
  d2_at_totalgoalscored <- sum(d2_goalscored_vec_at)
  d2_at_matches_scoring <- length(which(d2_goalscored_vec_at > 0))
  d2_at_matches_without_scoring <- length(which(d2_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  d2_goalconceded_vec_ht <- as.vector(d2_goalconceded_h[d2_hometeamindex,])
  d2_goalconceded_vec_ht[is.na(d2_goalconceded_vec_ht)] <- ""
  d2_goalconceded_vec_ht <- d2_goalconceded_vec_ht[d2_goalconceded_vec_ht != ""]
  d2_goalconceded_vec_ht  <-tail(d2_goalconceded_vec_ht,6)
  d2_goalconceded_vec_ht  <- as.numeric(d2_goalconceded_vec_ht)
  d2_goalconceded_vec_ht
  d2_ht_totalgoalconceded <- sum(d2_goalconceded_vec_ht)
  d2_ht_matches_concede <- length(which(d2_goalconceded_vec_ht > 0))
  d2_ht_matches_without_concede <- length(which(d2_goalconceded_vec_ht == "0"))
  #awayteam
  d2_goalconceded_vec_at <- as.vector(d2_goalconceded_h[d2_awayteamindex,])
  d2_goalconceded_vec_at[is.na(d2_goalconceded_vec_at)] <- ""
  d2_goalconceded_vec_at <- d2_goalconceded_vec_at[d2_goalconceded_vec_at != ""]
  d2_goalconceded_vec_at  <-tail(d2_goalconceded_vec_at,6)
  d2_goalconceded_vec_at  <- as.numeric(d2_goalconceded_vec_at)
  d2_at_totalgoalconceded <- sum(d2_goalconceded_vec_at)
  d2_at_matches_concede <- length(which(d2_goalconceded_vec_at > 0))
  d2_at_matches_without_concede <- length(which(d2_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  d2_totalgoals_vec_ht <- as.vector(d2_totalgoals_h[d2_hometeamindex,])
  d2_totalgoals_vec_ht[is.na(d2_totalgoals_vec_ht)] <- ""
  d2_totalgoals_vec_ht <- d2_totalgoals_vec_ht[d2_totalgoals_vec_ht != ""]
  d2_totalgoals_vec_ht  <-tail(d2_totalgoals_vec_ht,6)
  d2_totalgoals_vec_ht  <- as.numeric(d2_totalgoals_vec_ht)
  d2_totalgoals_vec_ht
  d2_ht_totalgoals <- sum(d2_totalgoals_vec_ht)
  d2_ht_avgtotalgoals <- (d2_ht_totalgoals/6)
  d2_ht_no_of_ov25 <- length(which(d2_totalgoals_vec_ht >= 3))
  d2_ht_no_of_un25 <- length(which(d2_totalgoals_vec_ht <= 2))
  #awayteam
  d2_totalgoals_vec_at <- as.vector(d2_totalgoals_h[d2_awayteamindex,])
  d2_totalgoals_vec_at[is.na(d2_totalgoals_vec_at)] <- ""
  d2_totalgoals_vec_at <- d2_totalgoals_vec_at[d2_totalgoals_vec_at != ""]
  d2_totalgoals_vec_at  <-tail(d2_totalgoals_vec_at,6)
  d2_totalgoals_vec_at  <- as.numeric(d2_totalgoals_vec_at)
  d2_totalgoals_vec_at
  d2_at_totalgoals <- sum(d2_totalgoals_vec_at)
  d2_at_avgtotalgoals <- (d2_at_totalgoals/6)
  d2_at_no_of_ov25 <- length(which(d2_totalgoals_vec_at >= 3))
  d2_at_no_of_un25 <- length(which(d2_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  d2_winmargin_vec_ht <- as.vector(d2_winmargin_h[d2_hometeamindex,])
  d2_winmargin_vec_ht[is.na(d2_winmargin_vec_ht)] <- ""
  d2_winmargin_vec_ht <- d2_winmargin_vec_ht[d2_winmargin_vec_ht != ""]
  d2_winmargin_vec_ht  <-tail(d2_winmargin_vec_ht,6)
  d2_winmargin_vec_ht  <- as.numeric(d2_winmargin_vec_ht)

  d2_ht_totalwinmargin <- sum(d2_winmargin_vec_ht)
  d2_ht_no_of_winmargin_ov0 <- length(which(d2_winmargin_vec_ht >= 0))
  d2_ht_no_of_winmargin_ov1 <- length(which(d2_winmargin_vec_ht >= 1))
  d2_ht_no_of_winmargin_un0 <- length(which(d2_winmargin_vec_ht <= 0))
  d2_ht_no_of_winmargin_un1 <- length(which(d2_winmargin_vec_ht <= 1))
  #awayteam
  d2_winmargin_vec_at <- as.vector(d2_winmargin_h[d2_awayteamindex,])
  d2_winmargin_vec_at[is.na(d2_winmargin_vec_at)] <- ""
  d2_winmargin_vec_at <- d2_winmargin_vec_at[d2_winmargin_vec_at != ""]
  d2_winmargin_vec_at  <-tail(d2_winmargin_vec_at,6)
  d2_winmargin_vec_at  <- as.numeric(d2_winmargin_vec_at)

  d2_at_totalwinmargin <- sum(d2_winmargin_vec_at)
  d2_at_no_of_winmargin_ov0 <- length(which(d2_winmargin_vec_at >= 0))
  d2_at_no_of_winmargin_ov1 <- length(which(d2_winmargin_vec_at >= 1))
  d2_at_no_of_winmargin_un0 <- length(which(d2_winmargin_vec_at <= 0))
  d2_at_no_of_winmargin_un1 <- length(which(d2_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  d2_ht_last6points <- d2_ht_numberof_wins*3 + d2_ht_numberof_draws*1
  d2_at_last6points <- d2_at_numberof_wins*3 + d2_at_numberof_draws*1

  ifelse(d2_ht_last6points > d2_at_last6points && d2_ht_totalwinmargin > d2_at_totalwinmargin,d2_3waypick <- "1",d2_3waypick <- "X2")

  ifelse(d2_at_last6points > d2_ht_last6points && d2_at_totalwinmargin > d2_ht_totalwinmargin,d2_3waypick <- "2",d2_3waypick <- "1X")

  if(d2_ht_no_of_ov25 + d2_at_no_of_ov25 >= 6) {d2_goalspick <- "ov25"} else {d2_goalspick <- "un25"}

  if(d2_ht_no_of_un25 + d2_at_no_of_un25 >= 6) {d2_goalspick <- "un25"} else {d2_goalspick <- "ov25"}

  d2_prediction[d2_row] <- rbind(paste(d2_3waypick,d2_goalspick,sep = ","))

}

d2_prediction <- as.data.frame(d2_prediction)
colnames(d2_prediction) <- "prediction"
d2_prediction

d2_picks <- cbind(D2_fixtures$Div,D2_fixtures$HomeTeam_d2,D2_fixtures$AwayTeam_d2,d2_prediction)
colnames(d2_picks)[1] <- "picks_Div"
colnames(d2_picks)[2] <- "picks_HomeTeam"
colnames(d2_picks)[3] <- "picks_AwayTeam"
d2_picks$matchid <- paste(d2_picks$picks_HomeTeam,d2_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of D2
############################################################################################
#E0
E0_fixtures$Hometeam_e0_index <- match(E0_fixtures$HomeTeam_e0,e0_teams)
E0_fixtures$Awayteam_e0_index <- match(E0_fixtures$AwayTeam_e0,e0_teams)
e0_prediction <- c()
for(e0_row in 1:nrow(E0_fixtures))
{

  e0_hometeamindex <- E0_fixtures[e0_row,"Hometeam_e0_index"]
  e0_awayteamindex <- E0_fixtures[e0_row,"Awayteam_e0_index"]
  #analyse team form
  #home team
  e0_form_vec_ht <- as.vector(e0_form_h[e0_hometeamindex,])
  e0_form_vec_ht[is.na(e0_form_vec_ht)] <- ""
  e0_form_vec_ht <- e0_form_vec_ht[e0_form_vec_ht != ""]
  e0_form_vec_ht  <-tail(e0_form_vec_ht,6)
  e0_ht_numberof_wins <- length(which(e0_form_vec_ht == "W"))
  e0_ht_numberof_draws <- length(which(e0_form_vec_ht == "D"))
  e0_ht_numberof_loss <- length(which(e0_form_vec_ht == "L"))
  #awayteam
  e0_form_vec_at <- as.vector(e0_form_h[e0_awayteamindex,])
  e0_form_vec_at[is.na(e0_form_vec_at)] <- ""
  e0_form_vec_at <- e0_form_vec_at[e0_form_vec_at != ""]
  e0_form_vec_at  <-tail(e0_form_vec_at,6)
  e0_at_numberof_wins <- length(which(e0_form_vec_at == "W"))
  e0_at_numberof_draws <- length(which(e0_form_vec_at == "D"))
  e0_at_numberof_loss <- length(which(e0_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  e0_goalscored_vec_ht <- as.vector(e0_goalscored_h[e0_hometeamindex,])
  e0_goalscored_vec_ht[is.na(e0_goalscored_vec_ht)] <- ""
  e0_goalscored_vec_ht <- e0_goalscored_vec_ht[e0_goalscored_vec_ht != ""]
  e0_goalscored_vec_ht  <-tail(e0_goalscored_vec_ht,6)
  e0_goalscored_vec_ht  <- as.numeric(e0_goalscored_vec_ht)
  e0_ht_totalgoalscored <- sum(e0_goalscored_vec_ht)
  e0_ht_matches_scoring <- length(which(e0_goalscored_vec_ht > 0))
  e0_ht_matches_without_scoring <- length(which(e0_goalscored_vec_ht == "0"))
  #awayteam
  e0_goalscored_vec_at <- as.vector(e0_goalscored_h[e0_awayteamindex,])
  e0_goalscored_vec_at[is.na(e0_goalscored_vec_at)] <- ""
  e0_goalscored_vec_at <- e0_goalscored_vec_at[e0_goalscored_vec_at != ""]
  e0_goalscored_vec_at  <-tail(e0_goalscored_vec_at,6)
  e0_goalscored_vec_at  <- as.numeric(e0_goalscored_vec_at)
  e0_at_totalgoalscored <- sum(e0_goalscored_vec_at)
  e0_at_matches_scoring <- length(which(e0_goalscored_vec_at > 0))
  e0_at_matches_without_scoring <- length(which(e0_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  e0_goalconceded_vec_ht <- as.vector(e0_goalconceded_h[e0_hometeamindex,])
  e0_goalconceded_vec_ht[is.na(e0_goalconceded_vec_ht)] <- ""
  e0_goalconceded_vec_ht <- e0_goalconceded_vec_ht[e0_goalconceded_vec_ht != ""]
  e0_goalconceded_vec_ht  <-tail(e0_goalconceded_vec_ht,6)
  e0_goalconceded_vec_ht  <- as.numeric(e0_goalconceded_vec_ht)
  e0_goalconceded_vec_ht
  e0_ht_totalgoalconceded <- sum(e0_goalconceded_vec_ht)
  e0_ht_matches_concede <- length(which(e0_goalconceded_vec_ht > 0))
  e0_ht_matches_without_concede <- length(which(e0_goalconceded_vec_ht == "0"))
  #awayteam
  e0_goalconceded_vec_at <- as.vector(e0_goalconceded_h[e0_awayteamindex,])
  e0_goalconceded_vec_at[is.na(e0_goalconceded_vec_at)] <- ""
  e0_goalconceded_vec_at <- e0_goalconceded_vec_at[e0_goalconceded_vec_at != ""]
  e0_goalconceded_vec_at  <-tail(e0_goalconceded_vec_at,6)
  e0_goalconceded_vec_at  <- as.numeric(e0_goalconceded_vec_at)
  e0_at_totalgoalconceded <- sum(e0_goalconceded_vec_at)
  e0_at_matches_concede <- length(which(e0_goalconceded_vec_at > 0))
  e0_at_matches_without_concede <- length(which(e0_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  e0_totalgoals_vec_ht <- as.vector(e0_totalgoals_h[e0_hometeamindex,])
  e0_totalgoals_vec_ht[is.na(e0_totalgoals_vec_ht)] <- ""
  e0_totalgoals_vec_ht <- e0_totalgoals_vec_ht[e0_totalgoals_vec_ht != ""]
  e0_totalgoals_vec_ht  <-tail(e0_totalgoals_vec_ht,6)
  e0_totalgoals_vec_ht  <- as.numeric(e0_totalgoals_vec_ht)
  e0_totalgoals_vec_ht
  e0_ht_totalgoals <- sum(e0_totalgoals_vec_ht)
  e0_ht_avgtotalgoals <- (e0_ht_totalgoals/6)
  e0_ht_no_of_ov25 <- length(which(e0_totalgoals_vec_ht >= 3))
  e0_ht_no_of_un25 <- length(which(e0_totalgoals_vec_ht <= 2))
  #awayteam
  e0_totalgoals_vec_at <- as.vector(e0_totalgoals_h[e0_awayteamindex,])
  e0_totalgoals_vec_at[is.na(e0_totalgoals_vec_at)] <- ""
  e0_totalgoals_vec_at <- e0_totalgoals_vec_at[e0_totalgoals_vec_at != ""]
  e0_totalgoals_vec_at  <-tail(e0_totalgoals_vec_at,6)
  e0_totalgoals_vec_at  <- as.numeric(e0_totalgoals_vec_at)
  e0_totalgoals_vec_at
  e0_at_totalgoals <- sum(e0_totalgoals_vec_at)
  e0_at_avgtotalgoals <- (e0_at_totalgoals/6)
  e0_at_no_of_ov25 <- length(which(e0_totalgoals_vec_at >= 3))
  e0_at_no_of_un25 <- length(which(e0_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  e0_winmargin_vec_ht <- as.vector(e0_winmargin_h[e0_hometeamindex,])
  e0_winmargin_vec_ht[is.na(e0_winmargin_vec_ht)] <- ""
  e0_winmargin_vec_ht <- e0_winmargin_vec_ht[e0_winmargin_vec_ht != ""]
  e0_winmargin_vec_ht  <-tail(e0_winmargin_vec_ht,6)
  e0_winmargin_vec_ht  <- as.numeric(e0_winmargin_vec_ht)

  e0_ht_totalwinmargin <- sum(e0_winmargin_vec_ht)
  e0_ht_no_of_winmargin_ov0 <- length(which(e0_winmargin_vec_ht >= 0))
  e0_ht_no_of_winmargin_ov1 <- length(which(e0_winmargin_vec_ht >= 1))
  e0_ht_no_of_winmargin_un0 <- length(which(e0_winmargin_vec_ht <= 0))
  e0_ht_no_of_winmargin_un1 <- length(which(e0_winmargin_vec_ht <= 1))
  #awayteam
  e0_winmargin_vec_at <- as.vector(e0_winmargin_h[e0_awayteamindex,])
  e0_winmargin_vec_at[is.na(e0_winmargin_vec_at)] <- ""
  e0_winmargin_vec_at <- e0_winmargin_vec_at[e0_winmargin_vec_at != ""]
  e0_winmargin_vec_at  <-tail(e0_winmargin_vec_at,6)
  e0_winmargin_vec_at  <- as.numeric(e0_winmargin_vec_at)

  e0_at_totalwinmargin <- sum(e0_winmargin_vec_at)
  e0_at_no_of_winmargin_ov0 <- length(which(e0_winmargin_vec_at >= 0))
  e0_at_no_of_winmargin_ov1 <- length(which(e0_winmargin_vec_at >= 1))
  e0_at_no_of_winmargin_un0 <- length(which(e0_winmargin_vec_at <= 0))
  e0_at_no_of_winmargin_un1 <- length(which(e0_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  e0_ht_last6points <- e0_ht_numberof_wins*3 + e0_ht_numberof_draws*1
  e0_at_last6points <- e0_at_numberof_wins*3 + e0_at_numberof_draws*1

  ifelse(e0_ht_last6points > e0_at_last6points && e0_ht_totalwinmargin > e0_at_totalwinmargin,e0_3waypick <- "1",e0_3waypick <- "X2")

  ifelse(e0_at_last6points > e0_ht_last6points && e0_at_totalwinmargin > e0_ht_totalwinmargin,e0_3waypick <- "2",e0_3waypick <- "1X")


  if(e0_ht_no_of_ov25 + e0_at_no_of_ov25 >= 6) {e0_goalspick <- "ov25"} else {e0_goalspick <- "un25"}

  if(e0_ht_no_of_un25 + e0_at_no_of_un25 >= 6) {e0_goalspick <- "un25"} else {e0_goalspick <- "ov25"}

  e0_prediction[e0_row] <- rbind(paste(e0_3waypick,e0_goalspick,sep = ","))

}

e0_prediction <- as.data.frame(e0_prediction)
colnames(e0_prediction) <- "prediction"
e0_prediction

e0_picks <- cbind(E0_fixtures$Div,E0_fixtures$HomeTeam_e0,E0_fixtures$AwayTeam_e0,e0_prediction)
colnames(e0_picks)[1] <- "picks_Div"
colnames(e0_picks)[2] <- "picks_HomeTeam"
colnames(e0_picks)[3] <- "picks_AwayTeam"
e0_picks$matchid <- paste(e0_picks$picks_HomeTeam,e0_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of E0
#E1
E1_fixtures$Hometeam_e1_index <- match(E1_fixtures$HomeTeam_e1,e1_teams)
E1_fixtures$Awayteam_e1_index <- match(E1_fixtures$AwayTeam_e1,e1_teams)
e1_prediction <- c()
for(e1_row in 1:nrow(E1_fixtures))
{

  e1_hometeamindex <- E1_fixtures[e1_row,"Hometeam_e1_index"]
  e1_awayteamindex <- E1_fixtures[e1_row,"Awayteam_e1_index"]
  #analyse team form
  #home team
  e1_form_vec_ht <- as.vector(e1_form_h[e1_hometeamindex,])
  e1_form_vec_ht[is.na(e1_form_vec_ht)] <- ""
  e1_form_vec_ht <- e1_form_vec_ht[e1_form_vec_ht != ""]
  e1_form_vec_ht  <-tail(e1_form_vec_ht,6)
  e1_ht_numberof_wins <- length(which(e1_form_vec_ht == "W"))
  e1_ht_numberof_draws <- length(which(e1_form_vec_ht == "D"))
  e1_ht_numberof_loss <- length(which(e1_form_vec_ht == "L"))
  #awayteam
  e1_form_vec_at <- as.vector(e1_form_h[e1_awayteamindex,])
  e1_form_vec_at[is.na(e1_form_vec_at)] <- ""
  e1_form_vec_at <- e1_form_vec_at[e1_form_vec_at != ""]
  e1_form_vec_at  <-tail(e1_form_vec_at,6)
  e1_at_numberof_wins <- length(which(e1_form_vec_at == "W"))
  e1_at_numberof_draws <- length(which(e1_form_vec_at == "D"))
  e1_at_numberof_loss <- length(which(e1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  e1_goalscored_vec_ht <- as.vector(e1_goalscored_h[e1_hometeamindex,])
  e1_goalscored_vec_ht[is.na(e1_goalscored_vec_ht)] <- ""
  e1_goalscored_vec_ht <- e1_goalscored_vec_ht[e1_goalscored_vec_ht != ""]
  e1_goalscored_vec_ht  <-tail(e1_goalscored_vec_ht,6)
  e1_goalscored_vec_ht  <- as.numeric(e1_goalscored_vec_ht)
  e1_ht_totalgoalscored <- sum(e1_goalscored_vec_ht)
  e1_ht_matches_scoring <- length(which(e1_goalscored_vec_ht > 0))
  e1_ht_matches_without_scoring <- length(which(e1_goalscored_vec_ht == "0"))
  #awayteam
  e1_goalscored_vec_at <- as.vector(e1_goalscored_h[e1_awayteamindex,])
  e1_goalscored_vec_at[is.na(e1_goalscored_vec_at)] <- ""
  e1_goalscored_vec_at <- e1_goalscored_vec_at[e1_goalscored_vec_at != ""]
  e1_goalscored_vec_at  <-tail(e1_goalscored_vec_at,6)
  e1_goalscored_vec_at  <- as.numeric(e1_goalscored_vec_at)
  e1_at_totalgoalscored <- sum(e1_goalscored_vec_at)
  e1_at_matches_scoring <- length(which(e1_goalscored_vec_at > 0))
  e1_at_matches_without_scoring <- length(which(e1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  e1_goalconceded_vec_ht <- as.vector(e1_goalconceded_h[e1_hometeamindex,])
  e1_goalconceded_vec_ht[is.na(e1_goalconceded_vec_ht)] <- ""
  e1_goalconceded_vec_ht <- e1_goalconceded_vec_ht[e1_goalconceded_vec_ht != ""]
  e1_goalconceded_vec_ht  <-tail(e1_goalconceded_vec_ht,6)
  e1_goalconceded_vec_ht  <- as.numeric(e1_goalconceded_vec_ht)
  e1_goalconceded_vec_ht
  e1_ht_totalgoalconceded <- sum(e1_goalconceded_vec_ht)
  e1_ht_matches_concede <- length(which(e1_goalconceded_vec_ht > 0))
  e1_ht_matches_without_concede <- length(which(e1_goalconceded_vec_ht == "0"))
  #awayteam
  e1_goalconceded_vec_at <- as.vector(e1_goalconceded_h[e1_awayteamindex,])
  e1_goalconceded_vec_at[is.na(e1_goalconceded_vec_at)] <- ""
  e1_goalconceded_vec_at <- e1_goalconceded_vec_at[e1_goalconceded_vec_at != ""]
  e1_goalconceded_vec_at  <-tail(e1_goalconceded_vec_at,6)
  e1_goalconceded_vec_at  <- as.numeric(e1_goalconceded_vec_at)
  e1_at_totalgoalconceded <- sum(e1_goalconceded_vec_at)
  e1_at_matches_concede <- length(which(e1_goalconceded_vec_at > 0))
  e1_at_matches_without_concede <- length(which(e1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  e1_totalgoals_vec_ht <- as.vector(e1_totalgoals_h[e1_hometeamindex,])
  e1_totalgoals_vec_ht[is.na(e1_totalgoals_vec_ht)] <- ""
  e1_totalgoals_vec_ht <- e1_totalgoals_vec_ht[e1_totalgoals_vec_ht != ""]
  e1_totalgoals_vec_ht  <-tail(e1_totalgoals_vec_ht,6)
  e1_totalgoals_vec_ht  <- as.numeric(e1_totalgoals_vec_ht)
  e1_totalgoals_vec_ht
  e1_ht_totalgoals <- sum(e1_totalgoals_vec_ht)
  e1_ht_avgtotalgoals <- (e1_ht_totalgoals/6)
  e1_ht_no_of_ov25 <- length(which(e1_totalgoals_vec_ht >= 3))
  e1_ht_no_of_un25 <- length(which(e1_totalgoals_vec_ht <= 2))
  #awayteam
  e1_totalgoals_vec_at <- as.vector(e1_totalgoals_h[e1_awayteamindex,])
  e1_totalgoals_vec_at[is.na(e1_totalgoals_vec_at)] <- ""
  e1_totalgoals_vec_at <- e1_totalgoals_vec_at[e1_totalgoals_vec_at != ""]
  e1_totalgoals_vec_at  <-tail(e1_totalgoals_vec_at,6)
  e1_totalgoals_vec_at  <- as.numeric(e1_totalgoals_vec_at)
  e1_totalgoals_vec_at
  e1_at_totalgoals <- sum(e1_totalgoals_vec_at)
  e1_at_avgtotalgoals <- (e1_at_totalgoals/6)
  e1_at_no_of_ov25 <- length(which(e1_totalgoals_vec_at >= 3))
  e1_at_no_of_un25 <- length(which(e1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  e1_winmargin_vec_ht <- as.vector(e1_winmargin_h[e1_hometeamindex,])
  e1_winmargin_vec_ht[is.na(e1_winmargin_vec_ht)] <- ""
  e1_winmargin_vec_ht <- e1_winmargin_vec_ht[e1_winmargin_vec_ht != ""]
  e1_winmargin_vec_ht  <-tail(e1_winmargin_vec_ht,6)
  e1_winmargin_vec_ht  <- as.numeric(e1_winmargin_vec_ht)

  e1_ht_totalwinmargin <- sum(e1_winmargin_vec_ht)
  e1_ht_no_of_winmargin_ov0 <- length(which(e1_winmargin_vec_ht >= 0))
  e1_ht_no_of_winmargin_ov1 <- length(which(e1_winmargin_vec_ht >= 1))
  e1_ht_no_of_winmargin_un0 <- length(which(e1_winmargin_vec_ht <= 0))
  e1_ht_no_of_winmargin_un1 <- length(which(e1_winmargin_vec_ht <= 1))
  #awayteam
  e1_winmargin_vec_at <- as.vector(e1_winmargin_h[e1_awayteamindex,])
  e1_winmargin_vec_at[is.na(e1_winmargin_vec_at)] <- ""
  e1_winmargin_vec_at <- e1_winmargin_vec_at[e1_winmargin_vec_at != ""]
  e1_winmargin_vec_at  <-tail(e1_winmargin_vec_at,6)
  e1_winmargin_vec_at  <- as.numeric(e1_winmargin_vec_at)

  e1_at_totalwinmargin <- sum(e1_winmargin_vec_at)
  e1_at_no_of_winmargin_ov0 <- length(which(e1_winmargin_vec_at >= 0))
  e1_at_no_of_winmargin_ov1 <- length(which(e1_winmargin_vec_at >= 1))
  e1_at_no_of_winmargin_un0 <- length(which(e1_winmargin_vec_at <= 0))
  e1_at_no_of_winmargin_un1 <- length(which(e1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  e1_ht_last6points <- e1_ht_numberof_wins*3 + e1_ht_numberof_draws*1
  e1_at_last6points <- e1_at_numberof_wins*3 + e1_at_numberof_draws*1

  ifelse(e1_ht_last6points > e1_at_last6points && e1_ht_totalwinmargin > e1_at_totalwinmargin,e1_3waypick <- "1",e1_3waypick <- "X2")

  ifelse(e1_at_last6points > e1_ht_last6points && e1_at_totalwinmargin > e1_ht_totalwinmargin,e1_3waypick <- "2",e1_3waypick <- "1X")


  if(e1_ht_no_of_ov25 + e1_at_no_of_ov25 >= 6) {e1_goalspick <- "ov25"} else {e1_goalspick <- "un25"}

  if(e1_ht_no_of_un25 + e1_at_no_of_un25 >= 6) {e1_goalspick <- "un25"} else {e1_goalspick <- "ov25"}


  e1_prediction[e1_row] <- rbind(paste(e1_3waypick,e1_goalspick,sep = ","))

}

e1_prediction <- as.data.frame(e1_prediction)
colnames(e1_prediction) <- "prediction"
e1_prediction

e1_picks <- cbind(E1_fixtures$Div,E1_fixtures$HomeTeam_e1,E1_fixtures$AwayTeam_e1,e1_prediction)
colnames(e1_picks)[1] <- "picks_Div"
colnames(e1_picks)[2] <- "picks_HomeTeam"
colnames(e1_picks)[3] <- "picks_AwayTeam"
e1_picks$matchid <- paste(e1_picks$picks_HomeTeam,e1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of E1
#E2
E2_fixtures$Hometeam_e2_index <- match(E2_fixtures$HomeTeam_e2,e2_teams)
E2_fixtures$Awayteam_e2_index <- match(E2_fixtures$AwayTeam_e2,e2_teams)
e2_prediction <- c()
for(e2_row in 1:nrow(E2_fixtures))
{

  e2_hometeamindex <- E2_fixtures[e2_row,"Hometeam_e2_index"]
  e2_awayteamindex <- E2_fixtures[e2_row,"Awayteam_e2_index"]
  #analyse team form
  #home team
  e2_form_vec_ht <- as.vector(e2_form_h[e2_hometeamindex,])
  e2_form_vec_ht[is.na(e2_form_vec_ht)] <- ""
  e2_form_vec_ht <- e2_form_vec_ht[e2_form_vec_ht != ""]
  e2_form_vec_ht  <-tail(e2_form_vec_ht,6)
  e2_ht_numberof_wins <- length(which(e2_form_vec_ht == "W"))
  e2_ht_numberof_draws <- length(which(e2_form_vec_ht == "D"))
  e2_ht_numberof_loss <- length(which(e2_form_vec_ht == "L"))
  #awayteam
  e2_form_vec_at <- as.vector(e2_form_h[e2_awayteamindex,])
  e2_form_vec_at[is.na(e2_form_vec_at)] <- ""
  e2_form_vec_at <- e2_form_vec_at[e2_form_vec_at != ""]
  e2_form_vec_at  <-tail(e2_form_vec_at,6)
  e2_at_numberof_wins <- length(which(e2_form_vec_at == "W"))
  e2_at_numberof_draws <- length(which(e2_form_vec_at == "D"))
  e2_at_numberof_loss <- length(which(e2_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  e2_goalscored_vec_ht <- as.vector(e2_goalscored_h[e2_hometeamindex,])
  e2_goalscored_vec_ht[is.na(e2_goalscored_vec_ht)] <- ""
  e2_goalscored_vec_ht <- e2_goalscored_vec_ht[e2_goalscored_vec_ht != ""]
  e2_goalscored_vec_ht  <-tail(e2_goalscored_vec_ht,6)
  e2_goalscored_vec_ht  <- as.numeric(e2_goalscored_vec_ht)
  e2_ht_totalgoalscored <- sum(e2_goalscored_vec_ht)
  e2_ht_matches_scoring <- length(which(e2_goalscored_vec_ht > 0))
  e2_ht_matches_without_scoring <- length(which(e2_goalscored_vec_ht == "0"))
  #awayteam
  e2_goalscored_vec_at <- as.vector(e2_goalscored_h[e2_awayteamindex,])
  e2_goalscored_vec_at[is.na(e2_goalscored_vec_at)] <- ""
  e2_goalscored_vec_at <- e2_goalscored_vec_at[e2_goalscored_vec_at != ""]
  e2_goalscored_vec_at  <-tail(e2_goalscored_vec_at,6)
  e2_goalscored_vec_at  <- as.numeric(e2_goalscored_vec_at)
  e2_at_totalgoalscored <- sum(e2_goalscored_vec_at)
  e2_at_matches_scoring <- length(which(e2_goalscored_vec_at > 0))
  e2_at_matches_without_scoring <- length(which(e2_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  e2_goalconceded_vec_ht <- as.vector(e2_goalconceded_h[e2_hometeamindex,])
  e2_goalconceded_vec_ht[is.na(e2_goalconceded_vec_ht)] <- ""
  e2_goalconceded_vec_ht <- e2_goalconceded_vec_ht[e2_goalconceded_vec_ht != ""]
  e2_goalconceded_vec_ht  <-tail(e2_goalconceded_vec_ht,6)
  e2_goalconceded_vec_ht  <- as.numeric(e2_goalconceded_vec_ht)
  e2_goalconceded_vec_ht
  e2_ht_totalgoalconceded <- sum(e2_goalconceded_vec_ht)
  e2_ht_matches_concede <- length(which(e2_goalconceded_vec_ht > 0))
  e2_ht_matches_without_concede <- length(which(e2_goalconceded_vec_ht == "0"))
  #awayteam
  e2_goalconceded_vec_at <- as.vector(e2_goalconceded_h[e2_awayteamindex,])
  e2_goalconceded_vec_at[is.na(e2_goalconceded_vec_at)] <- ""
  e2_goalconceded_vec_at <- e2_goalconceded_vec_at[e2_goalconceded_vec_at != ""]
  e2_goalconceded_vec_at  <-tail(e2_goalconceded_vec_at,6)
  e2_goalconceded_vec_at  <- as.numeric(e2_goalconceded_vec_at)
  e2_at_totalgoalconceded <- sum(e2_goalconceded_vec_at)
  e2_at_matches_concede <- length(which(e2_goalconceded_vec_at > 0))
  e2_at_matches_without_concede <- length(which(e2_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  e2_totalgoals_vec_ht <- as.vector(e2_totalgoals_h[e2_hometeamindex,])
  e2_totalgoals_vec_ht[is.na(e2_totalgoals_vec_ht)] <- ""
  e2_totalgoals_vec_ht <- e2_totalgoals_vec_ht[e2_totalgoals_vec_ht != ""]
  e2_totalgoals_vec_ht  <-tail(e2_totalgoals_vec_ht,6)
  e2_totalgoals_vec_ht  <- as.numeric(e2_totalgoals_vec_ht)
  e2_totalgoals_vec_ht
  e2_ht_totalgoals <- sum(e2_totalgoals_vec_ht)
  e2_ht_avgtotalgoals <- (e2_ht_totalgoals/6)
  e2_ht_no_of_ov25 <- length(which(e2_totalgoals_vec_ht >= 3))
  e2_ht_no_of_un25 <- length(which(e2_totalgoals_vec_ht <= 2))
  #awayteam
  e2_totalgoals_vec_at <- as.vector(e2_totalgoals_h[e2_awayteamindex,])
  e2_totalgoals_vec_at[is.na(e2_totalgoals_vec_at)] <- ""
  e2_totalgoals_vec_at <- e2_totalgoals_vec_at[e2_totalgoals_vec_at != ""]
  e2_totalgoals_vec_at  <-tail(e2_totalgoals_vec_at,6)
  e2_totalgoals_vec_at  <- as.numeric(e2_totalgoals_vec_at)
  e2_totalgoals_vec_at
  e2_at_totalgoals <- sum(e2_totalgoals_vec_at)
  e2_at_avgtotalgoals <- (e2_at_totalgoals/6)
  e2_at_no_of_ov25 <- length(which(e2_totalgoals_vec_at >= 3))
  e2_at_no_of_un25 <- length(which(e2_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  e2_winmargin_vec_ht <- as.vector(e2_winmargin_h[e2_hometeamindex,])
  e2_winmargin_vec_ht[is.na(e2_winmargin_vec_ht)] <- ""
  e2_winmargin_vec_ht <- e2_winmargin_vec_ht[e2_winmargin_vec_ht != ""]
  e2_winmargin_vec_ht  <-tail(e2_winmargin_vec_ht,6)
  e2_winmargin_vec_ht  <- as.numeric(e2_winmargin_vec_ht)

  e2_ht_totalwinmargin <- sum(e2_winmargin_vec_ht)
  e2_ht_no_of_winmargin_ov0 <- length(which(e2_winmargin_vec_ht >= 0))
  e2_ht_no_of_winmargin_ov1 <- length(which(e2_winmargin_vec_ht >= 1))
  e2_ht_no_of_winmargin_un0 <- length(which(e2_winmargin_vec_ht <= 0))
  e2_ht_no_of_winmargin_un1 <- length(which(e2_winmargin_vec_ht <= 1))
  #awayteam
  e2_winmargin_vec_at <- as.vector(e2_winmargin_h[e2_awayteamindex,])
  e2_winmargin_vec_at[is.na(e2_winmargin_vec_at)] <- ""
  e2_winmargin_vec_at <- e2_winmargin_vec_at[e2_winmargin_vec_at != ""]
  e2_winmargin_vec_at  <-tail(e2_winmargin_vec_at,6)
  e2_winmargin_vec_at  <- as.numeric(e2_winmargin_vec_at)

  e2_at_totalwinmargin <- sum(e2_winmargin_vec_at)
  e2_at_no_of_winmargin_ov0 <- length(which(e2_winmargin_vec_at >= 0))
  e2_at_no_of_winmargin_ov1 <- length(which(e2_winmargin_vec_at >= 1))
  e2_at_no_of_winmargin_un0 <- length(which(e2_winmargin_vec_at <= 0))
  e2_at_no_of_winmargin_un1 <- length(which(e2_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  e2_ht_last6points <- e2_ht_numberof_wins*3 + e2_ht_numberof_draws*1
  e2_at_last6points <- e2_at_numberof_wins*3 + e2_at_numberof_draws*1

  ifelse(e2_ht_last6points > e2_at_last6points && e2_ht_totalwinmargin > e2_at_totalwinmargin,e2_3waypick <- "1",e2_3waypick <- "X2")

  ifelse(e2_at_last6points > e2_ht_last6points && e2_at_totalwinmargin > e2_ht_totalwinmargin,e2_3waypick <- "2",e2_3waypick <- "1X")

  if(e2_ht_no_of_ov25 + e2_at_no_of_ov25 >= 6) {e2_goalspick <- "ov25"} else {e2_goalspick <- "un25"}

  if(e2_ht_no_of_un25 + e2_at_no_of_un25 >= 6) {e2_goalspick <- "un25"} else {e2_goalspick <- "ov25"}

  e2_prediction[e2_row] <- rbind(paste(e2_3waypick,e2_goalspick,sep = ","))

}

e2_prediction <- as.data.frame(e2_prediction)
colnames(e2_prediction) <- "prediction"
e2_prediction

e2_picks <- cbind(E2_fixtures$Div,E2_fixtures$HomeTeam_e2,E2_fixtures$AwayTeam_e2,e2_prediction)
colnames(e2_picks)[1] <- "picks_Div"
colnames(e2_picks)[2] <- "picks_HomeTeam"
colnames(e2_picks)[3] <- "picks_AwayTeam"
e2_picks$matchid <- paste(e2_picks$picks_HomeTeam,e2_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of E2
#E3
E3_fixtures$Hometeam_e3_index <- match(E3_fixtures$HomeTeam_e3,e3_teams)
E3_fixtures$Awayteam_e3_index <- match(E3_fixtures$AwayTeam_e3,e3_teams)
e3_prediction <- c()
for(e3_row in 1:nrow(E3_fixtures))
{

  e3_hometeamindex <- E3_fixtures[e3_row,"Hometeam_e3_index"]
  e3_awayteamindex <- E3_fixtures[e3_row,"Awayteam_e3_index"]
  #analyse team form
  #home team
  e3_form_vec_ht <- as.vector(e3_form_h[e3_hometeamindex,])
  e3_form_vec_ht[is.na(e3_form_vec_ht)] <- ""
  e3_form_vec_ht <- e3_form_vec_ht[e3_form_vec_ht != ""]
  e3_form_vec_ht  <-tail(e3_form_vec_ht,6)
  e3_ht_numberof_wins <- length(which(e3_form_vec_ht == "W"))
  e3_ht_numberof_draws <- length(which(e3_form_vec_ht == "D"))
  e3_ht_numberof_loss <- length(which(e3_form_vec_ht == "L"))
  #awayteam
  e3_form_vec_at <- as.vector(e3_form_h[e3_awayteamindex,])
  e3_form_vec_at[is.na(e3_form_vec_at)] <- ""
  e3_form_vec_at <- e3_form_vec_at[e3_form_vec_at != ""]
  e3_form_vec_at  <-tail(e3_form_vec_at,6)
  e3_at_numberof_wins <- length(which(e3_form_vec_at == "W"))
  e3_at_numberof_draws <- length(which(e3_form_vec_at == "D"))
  e3_at_numberof_loss <- length(which(e3_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  e3_goalscored_vec_ht <- as.vector(e3_goalscored_h[e3_hometeamindex,])
  e3_goalscored_vec_ht[is.na(e3_goalscored_vec_ht)] <- ""
  e3_goalscored_vec_ht <- e3_goalscored_vec_ht[e3_goalscored_vec_ht != ""]
  e3_goalscored_vec_ht  <-tail(e3_goalscored_vec_ht,6)
  e3_goalscored_vec_ht  <- as.numeric(e3_goalscored_vec_ht)
  e3_ht_totalgoalscored <- sum(e3_goalscored_vec_ht)
  e3_ht_matches_scoring <- length(which(e3_goalscored_vec_ht > 0))
  e3_ht_matches_without_scoring <- length(which(e3_goalscored_vec_ht == "0"))
  #awayteam
  e3_goalscored_vec_at <- as.vector(e3_goalscored_h[e3_awayteamindex,])
  e3_goalscored_vec_at[is.na(e3_goalscored_vec_at)] <- ""
  e3_goalscored_vec_at <- e3_goalscored_vec_at[e3_goalscored_vec_at != ""]
  e3_goalscored_vec_at  <-tail(e3_goalscored_vec_at,6)
  e3_goalscored_vec_at  <- as.numeric(e3_goalscored_vec_at)
  e3_at_totalgoalscored <- sum(e3_goalscored_vec_at)
  e3_at_matches_scoring <- length(which(e3_goalscored_vec_at > 0))
  e3_at_matches_without_scoring <- length(which(e3_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  e3_goalconceded_vec_ht <- as.vector(e3_goalconceded_h[e3_hometeamindex,])
  e3_goalconceded_vec_ht[is.na(e3_goalconceded_vec_ht)] <- ""
  e3_goalconceded_vec_ht <- e3_goalconceded_vec_ht[e3_goalconceded_vec_ht != ""]
  e3_goalconceded_vec_ht  <-tail(e3_goalconceded_vec_ht,6)
  e3_goalconceded_vec_ht  <- as.numeric(e3_goalconceded_vec_ht)
  e3_goalconceded_vec_ht
  e3_ht_totalgoalconceded <- sum(e3_goalconceded_vec_ht)
  e3_ht_matches_concede <- length(which(e3_goalconceded_vec_ht > 0))
  e3_ht_matches_without_concede <- length(which(e3_goalconceded_vec_ht == "0"))
  #awayteam
  e3_goalconceded_vec_at <- as.vector(e3_goalconceded_h[e3_awayteamindex,])
  e3_goalconceded_vec_at[is.na(e3_goalconceded_vec_at)] <- ""
  e3_goalconceded_vec_at <- e3_goalconceded_vec_at[e3_goalconceded_vec_at != ""]
  e3_goalconceded_vec_at  <-tail(e3_goalconceded_vec_at,6)
  e3_goalconceded_vec_at  <- as.numeric(e3_goalconceded_vec_at)
  e3_at_totalgoalconceded <- sum(e3_goalconceded_vec_at)
  e3_at_matches_concede <- length(which(e3_goalconceded_vec_at > 0))
  e3_at_matches_without_concede <- length(which(e3_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  e3_totalgoals_vec_ht <- as.vector(e3_totalgoals_h[e3_hometeamindex,])
  e3_totalgoals_vec_ht[is.na(e3_totalgoals_vec_ht)] <- ""
  e3_totalgoals_vec_ht <- e3_totalgoals_vec_ht[e3_totalgoals_vec_ht != ""]
  e3_totalgoals_vec_ht  <-tail(e3_totalgoals_vec_ht,6)
  e3_totalgoals_vec_ht  <- as.numeric(e3_totalgoals_vec_ht)
  e3_totalgoals_vec_ht
  e3_ht_totalgoals <- sum(e3_totalgoals_vec_ht)
  e3_ht_avgtotalgoals <- (e3_ht_totalgoals/6)
  e3_ht_no_of_ov25 <- length(which(e3_totalgoals_vec_ht >= 3))
  e3_ht_no_of_un25 <- length(which(e3_totalgoals_vec_ht <= 2))
  #awayteam
  e3_totalgoals_vec_at <- as.vector(e3_totalgoals_h[e3_awayteamindex,])
  e3_totalgoals_vec_at[is.na(e3_totalgoals_vec_at)] <- ""
  e3_totalgoals_vec_at <- e3_totalgoals_vec_at[e3_totalgoals_vec_at != ""]
  e3_totalgoals_vec_at  <-tail(e3_totalgoals_vec_at,6)
  e3_totalgoals_vec_at  <- as.numeric(e3_totalgoals_vec_at)
  e3_totalgoals_vec_at
  e3_at_totalgoals <- sum(e3_totalgoals_vec_at)
  e3_at_avgtotalgoals <- (e3_at_totalgoals/6)
  e3_at_no_of_ov25 <- length(which(e3_totalgoals_vec_at >= 3))
  e3_at_no_of_un25 <- length(which(e3_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  e3_winmargin_vec_ht <- as.vector(e3_winmargin_h[e3_hometeamindex,])
  e3_winmargin_vec_ht[is.na(e3_winmargin_vec_ht)] <- ""
  e3_winmargin_vec_ht <- e3_winmargin_vec_ht[e3_winmargin_vec_ht != ""]
  e3_winmargin_vec_ht  <-tail(e3_winmargin_vec_ht,6)
  e3_winmargin_vec_ht  <- as.numeric(e3_winmargin_vec_ht)

  e3_ht_totalwinmargin <- sum(e3_winmargin_vec_ht)
  e3_ht_no_of_winmargin_ov0 <- length(which(e3_winmargin_vec_ht >= 0))
  e3_ht_no_of_winmargin_ov1 <- length(which(e3_winmargin_vec_ht >= 1))
  e3_ht_no_of_winmargin_un0 <- length(which(e3_winmargin_vec_ht <= 0))
  e3_ht_no_of_winmargin_un1 <- length(which(e3_winmargin_vec_ht <= 1))
  #awayteam
  e3_winmargin_vec_at <- as.vector(e3_winmargin_h[e3_awayteamindex,])
  e3_winmargin_vec_at[is.na(e3_winmargin_vec_at)] <- ""
  e3_winmargin_vec_at <- e3_winmargin_vec_at[e3_winmargin_vec_at != ""]
  e3_winmargin_vec_at  <-tail(e3_winmargin_vec_at,6)
  e3_winmargin_vec_at  <- as.numeric(e3_winmargin_vec_at)

  e3_at_totalwinmargin <- sum(e3_winmargin_vec_at)
  e3_at_no_of_winmargin_ov0 <- length(which(e3_winmargin_vec_at >= 0))
  e3_at_no_of_winmargin_ov1 <- length(which(e3_winmargin_vec_at >= 1))
  e3_at_no_of_winmargin_un0 <- length(which(e3_winmargin_vec_at <= 0))
  e3_at_no_of_winmargin_un1 <- length(which(e3_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  e3_ht_last6points <- e3_ht_numberof_wins*3 + e3_ht_numberof_draws*1
  e3_at_last6points <- e3_at_numberof_wins*3 + e3_at_numberof_draws*1

  ifelse(e3_ht_last6points > e3_at_last6points && e3_ht_totalwinmargin > e3_at_totalwinmargin,e3_3waypick <- "1",e3_3waypick <- "X2")

  ifelse(e3_at_last6points > e3_ht_last6points && e3_at_totalwinmargin > e3_ht_totalwinmargin,e3_3waypick <- "2",e3_3waypick <- "1X")

  if(e3_ht_no_of_ov25 + e3_at_no_of_ov25 >= 6) {e3_goalspick <- "ov25"} else {e3_goalspick <- "un25"}

  if(e3_ht_no_of_un25 + e3_at_no_of_un25 >= 6) {e3_goalspick <- "un25"} else {e3_goalspick <- "ov25"}

  e3_prediction[e3_row] <- rbind(paste(e3_3waypick,e3_goalspick,sep = ","))

}

e3_prediction <- as.data.frame(e3_prediction)
colnames(e3_prediction) <- "prediction"
e3_prediction

e3_picks <- cbind(E3_fixtures$Div,E3_fixtures$HomeTeam_e3,E3_fixtures$AwayTeam_e3,e3_prediction)
colnames(e3_picks)[1] <- "picks_Div"
colnames(e3_picks)[2] <- "picks_HomeTeam"
colnames(e3_picks)[3] <- "picks_AwayTeam"
e3_picks$matchid <- paste(e3_picks$picks_HomeTeam,e3_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of E3
#EC
EC_fixtures$Hometeam_ec_index <- match(EC_fixtures$HomeTeam_ec,ec_teams)
EC_fixtures$Awayteam_ec_index <- match(EC_fixtures$AwayTeam_ec,ec_teams)
ec_prediction <- c()
for(ec_row in 1:nrow(EC_fixtures))
{

  ec_hometeamindex <- EC_fixtures[ec_row,"Hometeam_ec_index"]
  ec_awayteamindex <- EC_fixtures[ec_row,"Awayteam_ec_index"]
  #analyse team form
  #home team
  ec_form_vec_ht <- as.vector(ec_form_h[ec_hometeamindex,])
  ec_form_vec_ht[is.na(ec_form_vec_ht)] <- ""
  ec_form_vec_ht <- ec_form_vec_ht[ec_form_vec_ht != ""]
  ec_form_vec_ht  <-tail(ec_form_vec_ht,6)
  ec_ht_numberof_wins <- length(which(ec_form_vec_ht == "W"))
  ec_ht_numberof_draws <- length(which(ec_form_vec_ht == "D"))
  ec_ht_numberof_loss <- length(which(ec_form_vec_ht == "L"))
  #awayteam
  ec_form_vec_at <- as.vector(ec_form_h[ec_awayteamindex,])
  ec_form_vec_at[is.na(ec_form_vec_at)] <- ""
  ec_form_vec_at <- ec_form_vec_at[ec_form_vec_at != ""]
  ec_form_vec_at  <-tail(ec_form_vec_at,6)
  ec_at_numberof_wins <- length(which(ec_form_vec_at == "W"))
  ec_at_numberof_draws <- length(which(ec_form_vec_at == "D"))
  ec_at_numberof_loss <- length(which(ec_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  ec_goalscored_vec_ht <- as.vector(ec_goalscored_h[ec_hometeamindex,])
  ec_goalscored_vec_ht[is.na(ec_goalscored_vec_ht)] <- ""
  ec_goalscored_vec_ht <- ec_goalscored_vec_ht[ec_goalscored_vec_ht != ""]
  ec_goalscored_vec_ht  <-tail(ec_goalscored_vec_ht,6)
  ec_goalscored_vec_ht  <- as.numeric(ec_goalscored_vec_ht)
  ec_ht_totalgoalscored <- sum(ec_goalscored_vec_ht)
  ec_ht_matches_scoring <- length(which(ec_goalscored_vec_ht > 0))
  ec_ht_matches_without_scoring <- length(which(ec_goalscored_vec_ht == "0"))
  #awayteam
  ec_goalscored_vec_at <- as.vector(ec_goalscored_h[ec_awayteamindex,])
  ec_goalscored_vec_at[is.na(ec_goalscored_vec_at)] <- ""
  ec_goalscored_vec_at <- ec_goalscored_vec_at[ec_goalscored_vec_at != ""]
  ec_goalscored_vec_at  <-tail(ec_goalscored_vec_at,6)
  ec_goalscored_vec_at  <- as.numeric(ec_goalscored_vec_at)
  ec_at_totalgoalscored <- sum(ec_goalscored_vec_at)
  ec_at_matches_scoring <- length(which(ec_goalscored_vec_at > 0))
  ec_at_matches_without_scoring <- length(which(ec_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  ec_goalconceded_vec_ht <- as.vector(ec_goalconceded_h[ec_hometeamindex,])
  ec_goalconceded_vec_ht[is.na(ec_goalconceded_vec_ht)] <- ""
  ec_goalconceded_vec_ht <- ec_goalconceded_vec_ht[ec_goalconceded_vec_ht != ""]
  ec_goalconceded_vec_ht  <-tail(ec_goalconceded_vec_ht,6)
  ec_goalconceded_vec_ht  <- as.numeric(ec_goalconceded_vec_ht)
  ec_goalconceded_vec_ht
  ec_ht_totalgoalconceded <- sum(ec_goalconceded_vec_ht)
  ec_ht_matches_concede <- length(which(ec_goalconceded_vec_ht > 0))
  ec_ht_matches_without_concede <- length(which(ec_goalconceded_vec_ht == "0"))
  #awayteam
  ec_goalconceded_vec_at <- as.vector(ec_goalconceded_h[ec_awayteamindex,])
  ec_goalconceded_vec_at[is.na(ec_goalconceded_vec_at)] <- ""
  ec_goalconceded_vec_at <- ec_goalconceded_vec_at[ec_goalconceded_vec_at != ""]
  ec_goalconceded_vec_at  <-tail(ec_goalconceded_vec_at,6)
  ec_goalconceded_vec_at  <- as.numeric(ec_goalconceded_vec_at)
  ec_at_totalgoalconceded <- sum(ec_goalconceded_vec_at)
  ec_at_matches_concede <- length(which(ec_goalconceded_vec_at > 0))
  ec_at_matches_without_concede <- length(which(ec_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  ec_totalgoals_vec_ht <- as.vector(ec_totalgoals_h[ec_hometeamindex,])
  ec_totalgoals_vec_ht[is.na(ec_totalgoals_vec_ht)] <- ""
  ec_totalgoals_vec_ht <- ec_totalgoals_vec_ht[ec_totalgoals_vec_ht != ""]
  ec_totalgoals_vec_ht  <-tail(ec_totalgoals_vec_ht,6)
  ec_totalgoals_vec_ht  <- as.numeric(ec_totalgoals_vec_ht)
  ec_totalgoals_vec_ht
  ec_ht_totalgoals <- sum(ec_totalgoals_vec_ht)
  ec_ht_avgtotalgoals <- (ec_ht_totalgoals/6)
  ec_ht_no_of_ov25 <- length(which(ec_totalgoals_vec_ht >= 3))
  ec_ht_no_of_un25 <- length(which(ec_totalgoals_vec_ht <= 2))
  #awayteam
  ec_totalgoals_vec_at <- as.vector(ec_totalgoals_h[ec_awayteamindex,])
  ec_totalgoals_vec_at[is.na(ec_totalgoals_vec_at)] <- ""
  ec_totalgoals_vec_at <- ec_totalgoals_vec_at[ec_totalgoals_vec_at != ""]
  ec_totalgoals_vec_at  <-tail(ec_totalgoals_vec_at,6)
  ec_totalgoals_vec_at  <- as.numeric(ec_totalgoals_vec_at)
  ec_totalgoals_vec_at
  ec_at_totalgoals <- sum(ec_totalgoals_vec_at)
  ec_at_avgtotalgoals <- (ec_at_totalgoals/6)
  ec_at_no_of_ov25 <- length(which(ec_totalgoals_vec_at >= 3))
  ec_at_no_of_un25 <- length(which(ec_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  ec_winmargin_vec_ht <- as.vector(ec_winmargin_h[ec_hometeamindex,])
  ec_winmargin_vec_ht[is.na(ec_winmargin_vec_ht)] <- ""
  ec_winmargin_vec_ht <- ec_winmargin_vec_ht[ec_winmargin_vec_ht != ""]
  ec_winmargin_vec_ht  <-tail(ec_winmargin_vec_ht,6)
  ec_winmargin_vec_ht  <- as.numeric(ec_winmargin_vec_ht)

  ec_ht_totalwinmargin <- sum(ec_winmargin_vec_ht)
  ec_ht_no_of_winmargin_ov0 <- length(which(ec_winmargin_vec_ht >= 0))
  ec_ht_no_of_winmargin_ov1 <- length(which(ec_winmargin_vec_ht >= 1))
  ec_ht_no_of_winmargin_un0 <- length(which(ec_winmargin_vec_ht <= 0))
  ec_ht_no_of_winmargin_un1 <- length(which(ec_winmargin_vec_ht <= 1))
  #awayteam
  ec_winmargin_vec_at <- as.vector(ec_winmargin_h[ec_awayteamindex,])
  ec_winmargin_vec_at[is.na(ec_winmargin_vec_at)] <- ""
  ec_winmargin_vec_at <- ec_winmargin_vec_at[ec_winmargin_vec_at != ""]
  ec_winmargin_vec_at  <-tail(ec_winmargin_vec_at,6)
  ec_winmargin_vec_at  <- as.numeric(ec_winmargin_vec_at)

  ec_at_totalwinmargin <- sum(ec_winmargin_vec_at)
  ec_at_no_of_winmargin_ov0 <- length(which(ec_winmargin_vec_at >= 0))
  ec_at_no_of_winmargin_ov1 <- length(which(ec_winmargin_vec_at >= 1))
  ec_at_no_of_winmargin_un0 <- length(which(ec_winmargin_vec_at <= 0))
  ec_at_no_of_winmargin_un1 <- length(which(ec_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  ec_ht_last6points <- ec_ht_numberof_wins*3 + ec_ht_numberof_draws*1
  ec_at_last6points <- ec_at_numberof_wins*3 + ec_at_numberof_draws*1

  ifelse(ec_ht_last6points > ec_at_last6points && ec_ht_totalwinmargin > ec_at_totalwinmargin,ec_3waypick <- "1",ec_3waypick <- "X2")

  ifelse(ec_at_last6points > ec_ht_last6points && ec_at_totalwinmargin > ec_ht_totalwinmargin,ec_3waypick <- "2",ec_3waypick <- "1X")

  if(ec_ht_no_of_ov25 + ec_at_no_of_ov25 >= 6) {ec_goalspick <- "ov25"} else {ec_goalspick <- "un25"}

  if(ec_ht_no_of_un25 + ec_at_no_of_un25 >= 6) {ec_goalspick <- "un25"} else {ec_goalspick <- "ov25"}

  ec_prediction[ec_row] <- rbind(paste(ec_3waypick,ec_goalspick,sep = ","))

}

ec_prediction <- as.data.frame(ec_prediction)
colnames(ec_prediction) <- "prediction"
ec_prediction

ec_picks <- cbind(EC_fixtures$Div,EC_fixtures$HomeTeam_ec,EC_fixtures$AwayTeam_ec,ec_prediction)
colnames(ec_picks)[1] <- "picks_Div"
colnames(ec_picks)[2] <- "picks_HomeTeam"
colnames(ec_picks)[3] <- "picks_AwayTeam"
ec_picks$matchid <- paste(ec_picks$picks_HomeTeam,ec_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of EC
#F1
F1_fixtures$Hometeam_f1_index <- match(F1_fixtures$HomeTeam_f1,f1_teams)
F1_fixtures$Awayteam_f1_index <- match(F1_fixtures$AwayTeam_f1,f1_teams)
f1_prediction <- c()
for(f1_row in 1:nrow(F1_fixtures))
{

  f1_hometeamindex <- F1_fixtures[f1_row,"Hometeam_f1_index"]
  f1_awayteamindex <- F1_fixtures[f1_row,"Awayteam_f1_index"]
  #analyse team form
  #home team
  f1_form_vec_ht <- as.vector(f1_form_h[f1_hometeamindex,])
  f1_form_vec_ht[is.na(f1_form_vec_ht)] <- ""
  f1_form_vec_ht <- f1_form_vec_ht[f1_form_vec_ht != ""]
  f1_form_vec_ht  <-tail(f1_form_vec_ht,6)
  f1_ht_numberof_wins <- length(which(f1_form_vec_ht == "W"))
  f1_ht_numberof_draws <- length(which(f1_form_vec_ht == "D"))
  f1_ht_numberof_loss <- length(which(f1_form_vec_ht == "L"))
  #awayteam
  f1_form_vec_at <- as.vector(f1_form_h[f1_awayteamindex,])
  f1_form_vec_at[is.na(f1_form_vec_at)] <- ""
  f1_form_vec_at <- f1_form_vec_at[f1_form_vec_at != ""]
  f1_form_vec_at  <-tail(f1_form_vec_at,6)
  f1_at_numberof_wins <- length(which(f1_form_vec_at == "W"))
  f1_at_numberof_draws <- length(which(f1_form_vec_at == "D"))
  f1_at_numberof_loss <- length(which(f1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  f1_goalscored_vec_ht <- as.vector(f1_goalscored_h[f1_hometeamindex,])
  f1_goalscored_vec_ht[is.na(f1_goalscored_vec_ht)] <- ""
  f1_goalscored_vec_ht <- f1_goalscored_vec_ht[f1_goalscored_vec_ht != ""]
  f1_goalscored_vec_ht  <-tail(f1_goalscored_vec_ht,6)
  f1_goalscored_vec_ht  <- as.numeric(f1_goalscored_vec_ht)
  f1_ht_totalgoalscored <- sum(f1_goalscored_vec_ht)
  f1_ht_matches_scoring <- length(which(f1_goalscored_vec_ht > 0))
  f1_ht_matches_without_scoring <- length(which(f1_goalscored_vec_ht == "0"))
  #awayteam
  f1_goalscored_vec_at <- as.vector(f1_goalscored_h[f1_awayteamindex,])
  f1_goalscored_vec_at[is.na(f1_goalscored_vec_at)] <- ""
  f1_goalscored_vec_at <- f1_goalscored_vec_at[f1_goalscored_vec_at != ""]
  f1_goalscored_vec_at  <-tail(f1_goalscored_vec_at,6)
  f1_goalscored_vec_at  <- as.numeric(f1_goalscored_vec_at)
  f1_at_totalgoalscored <- sum(f1_goalscored_vec_at)
  f1_at_matches_scoring <- length(which(f1_goalscored_vec_at > 0))
  f1_at_matches_without_scoring <- length(which(f1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  f1_goalconceded_vec_ht <- as.vector(f1_goalconceded_h[f1_hometeamindex,])
  f1_goalconceded_vec_ht[is.na(f1_goalconceded_vec_ht)] <- ""
  f1_goalconceded_vec_ht <- f1_goalconceded_vec_ht[f1_goalconceded_vec_ht != ""]
  f1_goalconceded_vec_ht  <-tail(f1_goalconceded_vec_ht,6)
  f1_goalconceded_vec_ht  <- as.numeric(f1_goalconceded_vec_ht)
  f1_goalconceded_vec_ht
  f1_ht_totalgoalconceded <- sum(f1_goalconceded_vec_ht)
  f1_ht_matches_concede <- length(which(f1_goalconceded_vec_ht > 0))
  f1_ht_matches_without_concede <- length(which(f1_goalconceded_vec_ht == "0"))
  #awayteam
  f1_goalconceded_vec_at <- as.vector(f1_goalconceded_h[f1_awayteamindex,])
  f1_goalconceded_vec_at[is.na(f1_goalconceded_vec_at)] <- ""
  f1_goalconceded_vec_at <- f1_goalconceded_vec_at[f1_goalconceded_vec_at != ""]
  f1_goalconceded_vec_at  <-tail(f1_goalconceded_vec_at,6)
  f1_goalconceded_vec_at  <- as.numeric(f1_goalconceded_vec_at)
  f1_at_totalgoalconceded <- sum(f1_goalconceded_vec_at)
  f1_at_matches_concede <- length(which(f1_goalconceded_vec_at > 0))
  f1_at_matches_without_concede <- length(which(f1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  f1_totalgoals_vec_ht <- as.vector(f1_totalgoals_h[f1_hometeamindex,])
  f1_totalgoals_vec_ht[is.na(f1_totalgoals_vec_ht)] <- ""
  f1_totalgoals_vec_ht <- f1_totalgoals_vec_ht[f1_totalgoals_vec_ht != ""]
  f1_totalgoals_vec_ht  <-tail(f1_totalgoals_vec_ht,6)
  f1_totalgoals_vec_ht  <- as.numeric(f1_totalgoals_vec_ht)
  f1_totalgoals_vec_ht
  f1_ht_totalgoals <- sum(f1_totalgoals_vec_ht)
  f1_ht_avgtotalgoals <- (f1_ht_totalgoals/6)
  f1_ht_no_of_ov25 <- length(which(f1_totalgoals_vec_ht >= 3))
  f1_ht_no_of_un25 <- length(which(f1_totalgoals_vec_ht <= 2))
  #awayteam
  f1_totalgoals_vec_at <- as.vector(f1_totalgoals_h[f1_awayteamindex,])
  f1_totalgoals_vec_at[is.na(f1_totalgoals_vec_at)] <- ""
  f1_totalgoals_vec_at <- f1_totalgoals_vec_at[f1_totalgoals_vec_at != ""]
  f1_totalgoals_vec_at  <-tail(f1_totalgoals_vec_at,6)
  f1_totalgoals_vec_at  <- as.numeric(f1_totalgoals_vec_at)
  f1_totalgoals_vec_at
  f1_at_totalgoals <- sum(f1_totalgoals_vec_at)
  f1_at_avgtotalgoals <- (f1_at_totalgoals/6)
  f1_at_no_of_ov25 <- length(which(f1_totalgoals_vec_at >= 3))
  f1_at_no_of_un25 <- length(which(f1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  f1_winmargin_vec_ht <- as.vector(f1_winmargin_h[f1_hometeamindex,])
  f1_winmargin_vec_ht[is.na(f1_winmargin_vec_ht)] <- ""
  f1_winmargin_vec_ht <- f1_winmargin_vec_ht[f1_winmargin_vec_ht != ""]
  f1_winmargin_vec_ht  <-tail(f1_winmargin_vec_ht,6)
  f1_winmargin_vec_ht  <- as.numeric(f1_winmargin_vec_ht)

  f1_ht_totalwinmargin <- sum(f1_winmargin_vec_ht)
  f1_ht_no_of_winmargin_ov0 <- length(which(f1_winmargin_vec_ht >= 0))
  f1_ht_no_of_winmargin_ov1 <- length(which(f1_winmargin_vec_ht >= 1))
  f1_ht_no_of_winmargin_un0 <- length(which(f1_winmargin_vec_ht <= 0))
  f1_ht_no_of_winmargin_un1 <- length(which(f1_winmargin_vec_ht <= 1))
  #awayteam
  f1_winmargin_vec_at <- as.vector(f1_winmargin_h[f1_awayteamindex,])
  f1_winmargin_vec_at[is.na(f1_winmargin_vec_at)] <- ""
  f1_winmargin_vec_at <- f1_winmargin_vec_at[f1_winmargin_vec_at != ""]
  f1_winmargin_vec_at  <-tail(f1_winmargin_vec_at,6)
  f1_winmargin_vec_at  <- as.numeric(f1_winmargin_vec_at)

  f1_at_totalwinmargin <- sum(f1_winmargin_vec_at)
  f1_at_no_of_winmargin_ov0 <- length(which(f1_winmargin_vec_at >= 0))
  f1_at_no_of_winmargin_ov1 <- length(which(f1_winmargin_vec_at >= 1))
  f1_at_no_of_winmargin_un0 <- length(which(f1_winmargin_vec_at <= 0))
  f1_at_no_of_winmargin_un1 <- length(which(f1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  f1_ht_last6points <- f1_ht_numberof_wins*3 + f1_ht_numberof_draws*1
  f1_at_last6points <- f1_at_numberof_wins*3 + f1_at_numberof_draws*1

  ifelse(f1_ht_last6points > f1_at_last6points && f1_ht_totalwinmargin > f1_at_totalwinmargin,f1_3waypick <- "1",f1_3waypick <- "X2")

  ifelse(f1_at_last6points > f1_ht_last6points && f1_at_totalwinmargin > f1_ht_totalwinmargin,f1_3waypick <- "2",f1_3waypick <- "1X")


  if(f1_ht_no_of_ov25 + f1_at_no_of_ov25 >= 6) {f1_goalspick <- "ov25"} else {f1_goalspick <- "un25"}

  if(f1_ht_no_of_un25 + f1_at_no_of_un25 >= 6) {f1_goalspick <- "un25"} else {f1_goalspick <- "ov25"}

  f1_prediction[f1_row] <- rbind(paste(f1_3waypick,f1_goalspick,sep = ","))

}

f1_prediction <- as.data.frame(f1_prediction)
colnames(f1_prediction) <- "prediction"
f1_prediction

f1_picks <- cbind(F1_fixtures$Div,F1_fixtures$HomeTeam_f1,F1_fixtures$AwayTeam_f1,f1_prediction)
colnames(f1_picks)[1] <- "picks_Div"
colnames(f1_picks)[2] <- "picks_HomeTeam"
colnames(f1_picks)[3] <- "picks_AwayTeam"
f1_picks$matchid <- paste(f1_picks$picks_HomeTeam,f1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of F1
#F2
F2_fixtures$Hometeam_f2_index <- match(F2_fixtures$HomeTeam_f2,f2_teams)
F2_fixtures$Awayteam_f2_index <- match(F2_fixtures$AwayTeam_f2,f2_teams)
f2_prediction <- c()
for(f2_row in 1:nrow(F2_fixtures))
{

  f2_hometeamindex <- F2_fixtures[f2_row,"Hometeam_f2_index"]
  f2_awayteamindex <- F2_fixtures[f2_row,"Awayteam_f2_index"]
  #analyse team form
  #home team
  f2_form_vec_ht <- as.vector(f2_form_h[f2_hometeamindex,])
  f2_form_vec_ht[is.na(f2_form_vec_ht)] <- ""
  f2_form_vec_ht <- f2_form_vec_ht[f2_form_vec_ht != ""]
  f2_form_vec_ht  <-tail(f2_form_vec_ht,6)
  f2_ht_numberof_wins <- length(which(f2_form_vec_ht == "W"))
  f2_ht_numberof_draws <- length(which(f2_form_vec_ht == "D"))
  f2_ht_numberof_loss <- length(which(f2_form_vec_ht == "L"))
  #awayteam
  f2_form_vec_at <- as.vector(f2_form_h[f2_awayteamindex,])
  f2_form_vec_at[is.na(f2_form_vec_at)] <- ""
  f2_form_vec_at <- f2_form_vec_at[f2_form_vec_at != ""]
  f2_form_vec_at  <-tail(f2_form_vec_at,6)
  f2_at_numberof_wins <- length(which(f2_form_vec_at == "W"))
  f2_at_numberof_draws <- length(which(f2_form_vec_at == "D"))
  f2_at_numberof_loss <- length(which(f2_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  f2_goalscored_vec_ht <- as.vector(f2_goalscored_h[f2_hometeamindex,])
  f2_goalscored_vec_ht[is.na(f2_goalscored_vec_ht)] <- ""
  f2_goalscored_vec_ht <- f2_goalscored_vec_ht[f2_goalscored_vec_ht != ""]
  f2_goalscored_vec_ht  <-tail(f2_goalscored_vec_ht,6)
  f2_goalscored_vec_ht  <- as.numeric(f2_goalscored_vec_ht)
  f2_ht_totalgoalscored <- sum(f2_goalscored_vec_ht)
  f2_ht_matches_scoring <- length(which(f2_goalscored_vec_ht > 0))
  f2_ht_matches_without_scoring <- length(which(f2_goalscored_vec_ht == "0"))
  #awayteam
  f2_goalscored_vec_at <- as.vector(f2_goalscored_h[f2_awayteamindex,])
  f2_goalscored_vec_at[is.na(f2_goalscored_vec_at)] <- ""
  f2_goalscored_vec_at <- f2_goalscored_vec_at[f2_goalscored_vec_at != ""]
  f2_goalscored_vec_at  <-tail(f2_goalscored_vec_at,6)
  f2_goalscored_vec_at  <- as.numeric(f2_goalscored_vec_at)
  f2_at_totalgoalscored <- sum(f2_goalscored_vec_at)
  f2_at_matches_scoring <- length(which(f2_goalscored_vec_at > 0))
  f2_at_matches_without_scoring <- length(which(f2_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  f2_goalconceded_vec_ht <- as.vector(f2_goalconceded_h[f2_hometeamindex,])
  f2_goalconceded_vec_ht[is.na(f2_goalconceded_vec_ht)] <- ""
  f2_goalconceded_vec_ht <- f2_goalconceded_vec_ht[f2_goalconceded_vec_ht != ""]
  f2_goalconceded_vec_ht  <-tail(f2_goalconceded_vec_ht,6)
  f2_goalconceded_vec_ht  <- as.numeric(f2_goalconceded_vec_ht)
  f2_goalconceded_vec_ht
  f2_ht_totalgoalconceded <- sum(f2_goalconceded_vec_ht)
  f2_ht_matches_concede <- length(which(f2_goalconceded_vec_ht > 0))
  f2_ht_matches_without_concede <- length(which(f2_goalconceded_vec_ht == "0"))
  #awayteam
  f2_goalconceded_vec_at <- as.vector(f2_goalconceded_h[f2_awayteamindex,])
  f2_goalconceded_vec_at[is.na(f2_goalconceded_vec_at)] <- ""
  f2_goalconceded_vec_at <- f2_goalconceded_vec_at[f2_goalconceded_vec_at != ""]
  f2_goalconceded_vec_at  <-tail(f2_goalconceded_vec_at,6)
  f2_goalconceded_vec_at  <- as.numeric(f2_goalconceded_vec_at)
  f2_at_totalgoalconceded <- sum(f2_goalconceded_vec_at)
  f2_at_matches_concede <- length(which(f2_goalconceded_vec_at > 0))
  f2_at_matches_without_concede <- length(which(f2_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  f2_totalgoals_vec_ht <- as.vector(f2_totalgoals_h[f2_hometeamindex,])
  f2_totalgoals_vec_ht[is.na(f2_totalgoals_vec_ht)] <- ""
  f2_totalgoals_vec_ht <- f2_totalgoals_vec_ht[f2_totalgoals_vec_ht != ""]
  f2_totalgoals_vec_ht  <-tail(f2_totalgoals_vec_ht,6)
  f2_totalgoals_vec_ht  <- as.numeric(f2_totalgoals_vec_ht)
  f2_totalgoals_vec_ht
  f2_ht_totalgoals <- sum(f2_totalgoals_vec_ht)
  f2_ht_avgtotalgoals <- (f2_ht_totalgoals/6)
  f2_ht_no_of_ov25 <- length(which(f2_totalgoals_vec_ht >= 3))
  f2_ht_no_of_un25 <- length(which(f2_totalgoals_vec_ht <= 2))
  #awayteam
  f2_totalgoals_vec_at <- as.vector(f2_totalgoals_h[f2_awayteamindex,])
  f2_totalgoals_vec_at[is.na(f2_totalgoals_vec_at)] <- ""
  f2_totalgoals_vec_at <- f2_totalgoals_vec_at[f2_totalgoals_vec_at != ""]
  f2_totalgoals_vec_at  <-tail(f2_totalgoals_vec_at,6)
  f2_totalgoals_vec_at  <- as.numeric(f2_totalgoals_vec_at)
  f2_totalgoals_vec_at
  f2_at_totalgoals <- sum(f2_totalgoals_vec_at)
  f2_at_avgtotalgoals <- (f2_at_totalgoals/6)
  f2_at_no_of_ov25 <- length(which(f2_totalgoals_vec_at >= 3))
  f2_at_no_of_un25 <- length(which(f2_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  f2_winmargin_vec_ht <- as.vector(f2_winmargin_h[f2_hometeamindex,])
  f2_winmargin_vec_ht[is.na(f2_winmargin_vec_ht)] <- ""
  f2_winmargin_vec_ht <- f2_winmargin_vec_ht[f2_winmargin_vec_ht != ""]
  f2_winmargin_vec_ht  <-tail(f2_winmargin_vec_ht,6)
  f2_winmargin_vec_ht  <- as.numeric(f2_winmargin_vec_ht)

  f2_ht_totalwinmargin <- sum(f2_winmargin_vec_ht)
  f2_ht_no_of_winmargin_ov0 <- length(which(f2_winmargin_vec_ht >= 0))
  f2_ht_no_of_winmargin_ov1 <- length(which(f2_winmargin_vec_ht >= 1))
  f2_ht_no_of_winmargin_un0 <- length(which(f2_winmargin_vec_ht <= 0))
  f2_ht_no_of_winmargin_un1 <- length(which(f2_winmargin_vec_ht <= 1))
  #awayteam
  f2_winmargin_vec_at <- as.vector(f2_winmargin_h[f2_awayteamindex,])
  f2_winmargin_vec_at[is.na(f2_winmargin_vec_at)] <- ""
  f2_winmargin_vec_at <- f2_winmargin_vec_at[f2_winmargin_vec_at != ""]
  f2_winmargin_vec_at  <-tail(f2_winmargin_vec_at,6)
  f2_winmargin_vec_at  <- as.numeric(f2_winmargin_vec_at)

  f2_at_totalwinmargin <- sum(f2_winmargin_vec_at)
  f2_at_no_of_winmargin_ov0 <- length(which(f2_winmargin_vec_at >= 0))
  f2_at_no_of_winmargin_ov1 <- length(which(f2_winmargin_vec_at >= 1))
  f2_at_no_of_winmargin_un0 <- length(which(f2_winmargin_vec_at <= 0))
  f2_at_no_of_winmargin_un1 <- length(which(f2_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  f2_ht_last6points <- f2_ht_numberof_wins*3 + f2_ht_numberof_draws*1
  f2_at_last6points <- f2_at_numberof_wins*3 + f2_at_numberof_draws*1

  ifelse(f2_ht_last6points > f2_at_last6points && f2_ht_totalwinmargin > f2_at_totalwinmargin,f2_3waypick <- "1",f2_3waypick <- "X2")

  ifelse(f2_at_last6points > f2_ht_last6points && f2_at_totalwinmargin > f2_ht_totalwinmargin,f2_3waypick <- "2",f2_3waypick <- "1X")


  if(f2_ht_no_of_ov25 + f2_at_no_of_ov25 >= 6) {f2_goalspick <- "ov25"} else {f2_goalspick <- "un25"}

  if(f2_ht_no_of_un25 + f2_at_no_of_un25 >= 6) {f2_goalspick <- "un25"} else {f2_goalspick <- "ov25"}

  f2_prediction[f2_row] <- rbind(paste(f2_3waypick,f2_goalspick,sep = ","))

}

f2_prediction <- as.data.frame(f2_prediction)
colnames(f2_prediction) <- "prediction"
f2_prediction

f2_picks <- cbind(F2_fixtures$Div,F2_fixtures$HomeTeam_f2,F2_fixtures$AwayTeam_f2,f2_prediction)
colnames(f2_picks)[1] <- "picks_Div"
colnames(f2_picks)[2] <- "picks_HomeTeam"
colnames(f2_picks)[3] <- "picks_AwayTeam"
f2_picks$matchid <- paste(f2_picks$picks_HomeTeam,f2_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of F2
#G1
G1_fixtures$Hometeam_g1_index <- match(G1_fixtures$HomeTeam_g1,g1_teams)
G1_fixtures$Awayteam_g1_index <- match(G1_fixtures$AwayTeam_g1,g1_teams)
g1_prediction <- c()
for(g1_row in 1:nrow(G1_fixtures))
{

  g1_hometeamindex <- G1_fixtures[g1_row,"Hometeam_g1_index"]
  g1_awayteamindex <- G1_fixtures[g1_row,"Awayteam_g1_index"]
  #analyse team form
  #home team
  g1_form_vec_ht <- as.vector(g1_form_h[g1_hometeamindex,])
  g1_form_vec_ht[is.na(g1_form_vec_ht)] <- ""
  g1_form_vec_ht <- g1_form_vec_ht[g1_form_vec_ht != ""]
  g1_form_vec_ht  <-tail(g1_form_vec_ht,6)
  g1_ht_numberof_wins <- length(which(g1_form_vec_ht == "W"))
  g1_ht_numberof_draws <- length(which(g1_form_vec_ht == "D"))
  g1_ht_numberof_loss <- length(which(g1_form_vec_ht == "L"))
  #awayteam
  g1_form_vec_at <- as.vector(g1_form_h[g1_awayteamindex,])
  g1_form_vec_at[is.na(g1_form_vec_at)] <- ""
  g1_form_vec_at <- g1_form_vec_at[g1_form_vec_at != ""]
  g1_form_vec_at  <-tail(g1_form_vec_at,6)
  g1_at_numberof_wins <- length(which(g1_form_vec_at == "W"))
  g1_at_numberof_draws <- length(which(g1_form_vec_at == "D"))
  g1_at_numberof_loss <- length(which(g1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  g1_goalscored_vec_ht <- as.vector(g1_goalscored_h[g1_hometeamindex,])
  g1_goalscored_vec_ht[is.na(g1_goalscored_vec_ht)] <- ""
  g1_goalscored_vec_ht <- g1_goalscored_vec_ht[g1_goalscored_vec_ht != ""]
  g1_goalscored_vec_ht  <-tail(g1_goalscored_vec_ht,6)
  g1_goalscored_vec_ht  <- as.numeric(g1_goalscored_vec_ht)
  g1_ht_totalgoalscored <- sum(g1_goalscored_vec_ht)
  g1_ht_matches_scoring <- length(which(g1_goalscored_vec_ht > 0))
  g1_ht_matches_without_scoring <- length(which(g1_goalscored_vec_ht == "0"))
  #awayteam
  g1_goalscored_vec_at <- as.vector(g1_goalscored_h[g1_awayteamindex,])
  g1_goalscored_vec_at[is.na(g1_goalscored_vec_at)] <- ""
  g1_goalscored_vec_at <- g1_goalscored_vec_at[g1_goalscored_vec_at != ""]
  g1_goalscored_vec_at  <-tail(g1_goalscored_vec_at,6)
  g1_goalscored_vec_at  <- as.numeric(g1_goalscored_vec_at)
  g1_at_totalgoalscored <- sum(g1_goalscored_vec_at)
  g1_at_matches_scoring <- length(which(g1_goalscored_vec_at > 0))
  g1_at_matches_without_scoring <- length(which(g1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  g1_goalconceded_vec_ht <- as.vector(g1_goalconceded_h[g1_hometeamindex,])
  g1_goalconceded_vec_ht[is.na(g1_goalconceded_vec_ht)] <- ""
  g1_goalconceded_vec_ht <- g1_goalconceded_vec_ht[g1_goalconceded_vec_ht != ""]
  g1_goalconceded_vec_ht  <-tail(g1_goalconceded_vec_ht,6)
  g1_goalconceded_vec_ht  <- as.numeric(g1_goalconceded_vec_ht)
  g1_goalconceded_vec_ht
  g1_ht_totalgoalconceded <- sum(g1_goalconceded_vec_ht)
  g1_ht_matches_concede <- length(which(g1_goalconceded_vec_ht > 0))
  g1_ht_matches_without_concede <- length(which(g1_goalconceded_vec_ht == "0"))
  #awayteam
  g1_goalconceded_vec_at <- as.vector(g1_goalconceded_h[g1_awayteamindex,])
  g1_goalconceded_vec_at[is.na(g1_goalconceded_vec_at)] <- ""
  g1_goalconceded_vec_at <- g1_goalconceded_vec_at[g1_goalconceded_vec_at != ""]
  g1_goalconceded_vec_at  <-tail(g1_goalconceded_vec_at,6)
  g1_goalconceded_vec_at  <- as.numeric(g1_goalconceded_vec_at)
  g1_at_totalgoalconceded <- sum(g1_goalconceded_vec_at)
  g1_at_matches_concede <- length(which(g1_goalconceded_vec_at > 0))
  g1_at_matches_without_concede <- length(which(g1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  g1_totalgoals_vec_ht <- as.vector(g1_totalgoals_h[g1_hometeamindex,])
  g1_totalgoals_vec_ht[is.na(g1_totalgoals_vec_ht)] <- ""
  g1_totalgoals_vec_ht <- g1_totalgoals_vec_ht[g1_totalgoals_vec_ht != ""]
  g1_totalgoals_vec_ht  <-tail(g1_totalgoals_vec_ht,6)
  g1_totalgoals_vec_ht  <- as.numeric(g1_totalgoals_vec_ht)
  g1_totalgoals_vec_ht
  g1_ht_totalgoals <- sum(g1_totalgoals_vec_ht)
  g1_ht_avgtotalgoals <- (g1_ht_totalgoals/6)
  g1_ht_no_of_ov25 <- length(which(g1_totalgoals_vec_ht >= 3))
  g1_ht_no_of_un25 <- length(which(g1_totalgoals_vec_ht <= 2))
  #awayteam
  g1_totalgoals_vec_at <- as.vector(g1_totalgoals_h[g1_awayteamindex,])
  g1_totalgoals_vec_at[is.na(g1_totalgoals_vec_at)] <- ""
  g1_totalgoals_vec_at <- g1_totalgoals_vec_at[g1_totalgoals_vec_at != ""]
  g1_totalgoals_vec_at  <-tail(g1_totalgoals_vec_at,6)
  g1_totalgoals_vec_at  <- as.numeric(g1_totalgoals_vec_at)
  g1_totalgoals_vec_at
  g1_at_totalgoals <- sum(g1_totalgoals_vec_at)
  g1_at_avgtotalgoals <- (g1_at_totalgoals/6)
  g1_at_no_of_ov25 <- length(which(g1_totalgoals_vec_at >= 3))
  g1_at_no_of_un25 <- length(which(g1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  g1_winmargin_vec_ht <- as.vector(g1_winmargin_h[g1_hometeamindex,])
  g1_winmargin_vec_ht[is.na(g1_winmargin_vec_ht)] <- ""
  g1_winmargin_vec_ht <- g1_winmargin_vec_ht[g1_winmargin_vec_ht != ""]
  g1_winmargin_vec_ht  <-tail(g1_winmargin_vec_ht,6)
  g1_winmargin_vec_ht  <- as.numeric(g1_winmargin_vec_ht)

  g1_ht_totalwinmargin <- sum(g1_winmargin_vec_ht)
  g1_ht_no_of_winmargin_ov0 <- length(which(g1_winmargin_vec_ht >= 0))
  g1_ht_no_of_winmargin_ov1 <- length(which(g1_winmargin_vec_ht >= 1))
  g1_ht_no_of_winmargin_un0 <- length(which(g1_winmargin_vec_ht <= 0))
  g1_ht_no_of_winmargin_un1 <- length(which(g1_winmargin_vec_ht <= 1))
  #awayteam
  g1_winmargin_vec_at <- as.vector(g1_winmargin_h[g1_awayteamindex,])
  g1_winmargin_vec_at[is.na(g1_winmargin_vec_at)] <- ""
  g1_winmargin_vec_at <- g1_winmargin_vec_at[g1_winmargin_vec_at != ""]
  g1_winmargin_vec_at  <-tail(g1_winmargin_vec_at,6)
  g1_winmargin_vec_at  <- as.numeric(g1_winmargin_vec_at)

  g1_at_totalwinmargin <- sum(g1_winmargin_vec_at)
  g1_at_no_of_winmargin_ov0 <- length(which(g1_winmargin_vec_at >= 0))
  g1_at_no_of_winmargin_ov1 <- length(which(g1_winmargin_vec_at >= 1))
  g1_at_no_of_winmargin_un0 <- length(which(g1_winmargin_vec_at <= 0))
  g1_at_no_of_winmargin_un1 <- length(which(g1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  g1_ht_last6points <- g1_ht_numberof_wins*3 + g1_ht_numberof_draws*1
  g1_at_last6points <- g1_at_numberof_wins*3 + g1_at_numberof_draws*1

  ifelse(g1_ht_last6points > g1_at_last6points && g1_ht_totalwinmargin > g1_at_totalwinmargin,g1_3waypick <- "1",g1_3waypick <- "X2")

  ifelse(g1_at_last6points > g1_ht_last6points && g1_at_totalwinmargin > g1_ht_totalwinmargin,g1_3waypick <- "2",g1_3waypick <- "1X")

  if(g1_ht_no_of_ov25 + g1_at_no_of_ov25 >= 6) {g1_goalspick <- "ov25"} else {g1_goalspick <- "un25"}

  if(g1_ht_no_of_un25 + g1_at_no_of_un25 >= 6) {g1_goalspick <- "un25"} else {g1_goalspick <- "ov25"}

  g1_prediction[g1_row] <- rbind(paste(g1_3waypick,g1_goalspick,sep = ","))

}

g1_prediction <- as.data.frame(g1_prediction)
colnames(g1_prediction) <- "prediction"
g1_prediction

g1_picks <- cbind(G1_fixtures$Div,G1_fixtures$HomeTeam_g1,G1_fixtures$AwayTeam_g1,g1_prediction)
colnames(g1_picks)[1] <- "picks_Div"
colnames(g1_picks)[2] <- "picks_HomeTeam"
colnames(g1_picks)[3] <- "picks_AwayTeam"
g1_picks$matchid <- paste(g1_picks$picks_HomeTeam,g1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of G1
#I1
I1_fixtures$Hometeam_i1_index <- match(I1_fixtures$HomeTeam_i1,i1_teams)
I1_fixtures$Awayteam_i1_index <- match(I1_fixtures$AwayTeam_i1,i1_teams)
i1_prediction <- c()
for(i1_row in 1:nrow(I1_fixtures))
{

  i1_hometeamindex <- I1_fixtures[i1_row,"Hometeam_i1_index"]
  i1_awayteamindex <- I1_fixtures[i1_row,"Awayteam_i1_index"]
  #analyse team form
  #home team
  i1_form_vec_ht <- as.vector(i1_form_h[i1_hometeamindex,])
  i1_form_vec_ht[is.na(i1_form_vec_ht)] <- ""
  i1_form_vec_ht <- i1_form_vec_ht[i1_form_vec_ht != ""]
  i1_form_vec_ht  <-tail(i1_form_vec_ht,6)
  i1_ht_numberof_wins <- length(which(i1_form_vec_ht == "W"))
  i1_ht_numberof_draws <- length(which(i1_form_vec_ht == "D"))
  i1_ht_numberof_loss <- length(which(i1_form_vec_ht == "L"))
  #awayteam
  i1_form_vec_at <- as.vector(i1_form_h[i1_awayteamindex,])
  i1_form_vec_at[is.na(i1_form_vec_at)] <- ""
  i1_form_vec_at <- i1_form_vec_at[i1_form_vec_at != ""]
  i1_form_vec_at  <-tail(i1_form_vec_at,6)
  i1_at_numberof_wins <- length(which(i1_form_vec_at == "W"))
  i1_at_numberof_draws <- length(which(i1_form_vec_at == "D"))
  i1_at_numberof_loss <- length(which(i1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  i1_goalscored_vec_ht <- as.vector(i1_goalscored_h[i1_hometeamindex,])
  i1_goalscored_vec_ht[is.na(i1_goalscored_vec_ht)] <- ""
  i1_goalscored_vec_ht <- i1_goalscored_vec_ht[i1_goalscored_vec_ht != ""]
  i1_goalscored_vec_ht  <-tail(i1_goalscored_vec_ht,6)
  i1_goalscored_vec_ht  <- as.numeric(i1_goalscored_vec_ht)
  i1_ht_totalgoalscored <- sum(i1_goalscored_vec_ht)
  i1_ht_matches_scoring <- length(which(i1_goalscored_vec_ht > 0))
  i1_ht_matches_without_scoring <- length(which(i1_goalscored_vec_ht == "0"))
  #awayteam
  i1_goalscored_vec_at <- as.vector(i1_goalscored_h[i1_awayteamindex,])
  i1_goalscored_vec_at[is.na(i1_goalscored_vec_at)] <- ""
  i1_goalscored_vec_at <- i1_goalscored_vec_at[i1_goalscored_vec_at != ""]
  i1_goalscored_vec_at  <-tail(i1_goalscored_vec_at,6)
  i1_goalscored_vec_at  <- as.numeric(i1_goalscored_vec_at)
  i1_at_totalgoalscored <- sum(i1_goalscored_vec_at)
  i1_at_matches_scoring <- length(which(i1_goalscored_vec_at > 0))
  i1_at_matches_without_scoring <- length(which(i1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  i1_goalconceded_vec_ht <- as.vector(i1_goalconceded_h[i1_hometeamindex,])
  i1_goalconceded_vec_ht[is.na(i1_goalconceded_vec_ht)] <- ""
  i1_goalconceded_vec_ht <- i1_goalconceded_vec_ht[i1_goalconceded_vec_ht != ""]
  i1_goalconceded_vec_ht  <-tail(i1_goalconceded_vec_ht,6)
  i1_goalconceded_vec_ht  <- as.numeric(i1_goalconceded_vec_ht)
  i1_goalconceded_vec_ht
  i1_ht_totalgoalconceded <- sum(i1_goalconceded_vec_ht)
  i1_ht_matches_concede <- length(which(i1_goalconceded_vec_ht > 0))
  i1_ht_matches_without_concede <- length(which(i1_goalconceded_vec_ht == "0"))
  #awayteam
  i1_goalconceded_vec_at <- as.vector(i1_goalconceded_h[i1_awayteamindex,])
  i1_goalconceded_vec_at[is.na(i1_goalconceded_vec_at)] <- ""
  i1_goalconceded_vec_at <- i1_goalconceded_vec_at[i1_goalconceded_vec_at != ""]
  i1_goalconceded_vec_at  <-tail(i1_goalconceded_vec_at,6)
  i1_goalconceded_vec_at  <- as.numeric(i1_goalconceded_vec_at)
  i1_at_totalgoalconceded <- sum(i1_goalconceded_vec_at)
  i1_at_matches_concede <- length(which(i1_goalconceded_vec_at > 0))
  i1_at_matches_without_concede <- length(which(i1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  i1_totalgoals_vec_ht <- as.vector(i1_totalgoals_h[i1_hometeamindex,])
  i1_totalgoals_vec_ht[is.na(i1_totalgoals_vec_ht)] <- ""
  i1_totalgoals_vec_ht <- i1_totalgoals_vec_ht[i1_totalgoals_vec_ht != ""]
  i1_totalgoals_vec_ht  <-tail(i1_totalgoals_vec_ht,6)
  i1_totalgoals_vec_ht  <- as.numeric(i1_totalgoals_vec_ht)
  i1_totalgoals_vec_ht
  i1_ht_totalgoals <- sum(i1_totalgoals_vec_ht)
  i1_ht_avgtotalgoals <- (i1_ht_totalgoals/6)
  i1_ht_no_of_ov25 <- length(which(i1_totalgoals_vec_ht >= 3))
  i1_ht_no_of_un25 <- length(which(i1_totalgoals_vec_ht <= 2))
  #awayteam
  i1_totalgoals_vec_at <- as.vector(i1_totalgoals_h[i1_awayteamindex,])
  i1_totalgoals_vec_at[is.na(i1_totalgoals_vec_at)] <- ""
  i1_totalgoals_vec_at <- i1_totalgoals_vec_at[i1_totalgoals_vec_at != ""]
  i1_totalgoals_vec_at  <-tail(i1_totalgoals_vec_at,6)
  i1_totalgoals_vec_at  <- as.numeric(i1_totalgoals_vec_at)
  i1_totalgoals_vec_at
  i1_at_totalgoals <- sum(i1_totalgoals_vec_at)
  i1_at_avgtotalgoals <- (i1_at_totalgoals/6)
  i1_at_no_of_ov25 <- length(which(i1_totalgoals_vec_at >= 3))
  i1_at_no_of_un25 <- length(which(i1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  i1_winmargin_vec_ht <- as.vector(i1_winmargin_h[i1_hometeamindex,])
  i1_winmargin_vec_ht[is.na(i1_winmargin_vec_ht)] <- ""
  i1_winmargin_vec_ht <- i1_winmargin_vec_ht[i1_winmargin_vec_ht != ""]
  i1_winmargin_vec_ht  <-tail(i1_winmargin_vec_ht,6)
  i1_winmargin_vec_ht  <- as.numeric(i1_winmargin_vec_ht)

  i1_ht_totalwinmargin <- sum(i1_winmargin_vec_ht)
  i1_ht_no_of_winmargin_ov0 <- length(which(i1_winmargin_vec_ht >= 0))
  i1_ht_no_of_winmargin_ov1 <- length(which(i1_winmargin_vec_ht >= 1))
  i1_ht_no_of_winmargin_un0 <- length(which(i1_winmargin_vec_ht <= 0))
  i1_ht_no_of_winmargin_un1 <- length(which(i1_winmargin_vec_ht <= 1))
  #awayteam
  i1_winmargin_vec_at <- as.vector(i1_winmargin_h[i1_awayteamindex,])
  i1_winmargin_vec_at[is.na(i1_winmargin_vec_at)] <- ""
  i1_winmargin_vec_at <- i1_winmargin_vec_at[i1_winmargin_vec_at != ""]
  i1_winmargin_vec_at  <-tail(i1_winmargin_vec_at,6)
  i1_winmargin_vec_at  <- as.numeric(i1_winmargin_vec_at)

  i1_at_totalwinmargin <- sum(i1_winmargin_vec_at)
  i1_at_no_of_winmargin_ov0 <- length(which(i1_winmargin_vec_at >= 0))
  i1_at_no_of_winmargin_ov1 <- length(which(i1_winmargin_vec_at >= 1))
  i1_at_no_of_winmargin_un0 <- length(which(i1_winmargin_vec_at <= 0))
  i1_at_no_of_winmargin_un1 <- length(which(i1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  i1_ht_last6points <- i1_ht_numberof_wins*3 + i1_ht_numberof_draws*1
  i1_at_last6points <- i1_at_numberof_wins*3 + i1_at_numberof_draws*1

  ifelse(i1_ht_last6points > i1_at_last6points && i1_ht_totalwinmargin > i1_at_totalwinmargin,i1_3waypick <- "1",i1_3waypick <- "X2")

  ifelse(i1_at_last6points > i1_ht_last6points && i1_at_totalwinmargin > i1_ht_totalwinmargin,i1_3waypick <- "2",i1_3waypick <- "1X")

  if(i1_ht_no_of_ov25 + i1_at_no_of_ov25 >= 6) {i1_goalspick <- "ov25"} else {i1_goalspick <- "un25"}

  if(i1_ht_no_of_un25 + i1_at_no_of_un25 >= 6) {i1_goalspick <- "un25"} else {i1_goalspick <- "ov25"}

  i1_prediction[i1_row] <- rbind(paste(i1_3waypick,i1_goalspick,sep = ","))

}

i1_prediction <- as.data.frame(i1_prediction)
colnames(i1_prediction) <- "prediction"
i1_prediction

i1_picks <- cbind(I1_fixtures$Div,I1_fixtures$HomeTeam_i1,I1_fixtures$AwayTeam_i1,i1_prediction)
colnames(i1_picks)[1] <- "picks_Div"
colnames(i1_picks)[2] <- "picks_HomeTeam"
colnames(i1_picks)[3] <- "picks_AwayTeam"
i1_picks$matchid <- paste(i1_picks$picks_HomeTeam,i1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of I1
#I2
I2_fixtures$Hometeam_i2_index <- match(I2_fixtures$HomeTeam_i2,i2_teams)
I2_fixtures$Awayteam_i2_index <- match(I2_fixtures$AwayTeam_i2,i2_teams)
i2_prediction <- c()
for(i2_row in 1:nrow(I2_fixtures))
{

  i2_hometeamindex <- I2_fixtures[i2_row,"Hometeam_i2_index"]
  i2_awayteamindex <- I2_fixtures[i2_row,"Awayteam_i2_index"]
  #analyse team form
  #home team
  i2_form_vec_ht <- as.vector(i2_form_h[i2_hometeamindex,])
  i2_form_vec_ht[is.na(i2_form_vec_ht)] <- ""
  i2_form_vec_ht <- i2_form_vec_ht[i2_form_vec_ht != ""]
  i2_form_vec_ht  <-tail(i2_form_vec_ht,6)
  i2_ht_numberof_wins <- length(which(i2_form_vec_ht == "W"))
  i2_ht_numberof_draws <- length(which(i2_form_vec_ht == "D"))
  i2_ht_numberof_loss <- length(which(i2_form_vec_ht == "L"))
  #awayteam
  i2_form_vec_at <- as.vector(i2_form_h[i2_awayteamindex,])
  i2_form_vec_at[is.na(i2_form_vec_at)] <- ""
  i2_form_vec_at <- i2_form_vec_at[i2_form_vec_at != ""]
  i2_form_vec_at  <-tail(i2_form_vec_at,6)
  i2_at_numberof_wins <- length(which(i2_form_vec_at == "W"))
  i2_at_numberof_draws <- length(which(i2_form_vec_at == "D"))
  i2_at_numberof_loss <- length(which(i2_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  i2_goalscored_vec_ht <- as.vector(i2_goalscored_h[i2_hometeamindex,])
  i2_goalscored_vec_ht[is.na(i2_goalscored_vec_ht)] <- ""
  i2_goalscored_vec_ht <- i2_goalscored_vec_ht[i2_goalscored_vec_ht != ""]
  i2_goalscored_vec_ht  <-tail(i2_goalscored_vec_ht,6)
  i2_goalscored_vec_ht  <- as.numeric(i2_goalscored_vec_ht)
  i2_ht_totalgoalscored <- sum(i2_goalscored_vec_ht)
  i2_ht_matches_scoring <- length(which(i2_goalscored_vec_ht > 0))
  i2_ht_matches_without_scoring <- length(which(i2_goalscored_vec_ht == "0"))
  #awayteam
  i2_goalscored_vec_at <- as.vector(i2_goalscored_h[i2_awayteamindex,])
  i2_goalscored_vec_at[is.na(i2_goalscored_vec_at)] <- ""
  i2_goalscored_vec_at <- i2_goalscored_vec_at[i2_goalscored_vec_at != ""]
  i2_goalscored_vec_at  <-tail(i2_goalscored_vec_at,6)
  i2_goalscored_vec_at  <- as.numeric(i2_goalscored_vec_at)
  i2_at_totalgoalscored <- sum(i2_goalscored_vec_at)
  i2_at_matches_scoring <- length(which(i2_goalscored_vec_at > 0))
  i2_at_matches_without_scoring <- length(which(i2_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  i2_goalconceded_vec_ht <- as.vector(i2_goalconceded_h[i2_hometeamindex,])
  i2_goalconceded_vec_ht[is.na(i2_goalconceded_vec_ht)] <- ""
  i2_goalconceded_vec_ht <- i2_goalconceded_vec_ht[i2_goalconceded_vec_ht != ""]
  i2_goalconceded_vec_ht  <-tail(i2_goalconceded_vec_ht,6)
  i2_goalconceded_vec_ht  <- as.numeric(i2_goalconceded_vec_ht)
  i2_goalconceded_vec_ht
  i2_ht_totalgoalconceded <- sum(i2_goalconceded_vec_ht)
  i2_ht_matches_concede <- length(which(i2_goalconceded_vec_ht > 0))
  i2_ht_matches_without_concede <- length(which(i2_goalconceded_vec_ht == "0"))
  #awayteam
  i2_goalconceded_vec_at <- as.vector(i2_goalconceded_h[i2_awayteamindex,])
  i2_goalconceded_vec_at[is.na(i2_goalconceded_vec_at)] <- ""
  i2_goalconceded_vec_at <- i2_goalconceded_vec_at[i2_goalconceded_vec_at != ""]
  i2_goalconceded_vec_at  <-tail(i2_goalconceded_vec_at,6)
  i2_goalconceded_vec_at  <- as.numeric(i2_goalconceded_vec_at)
  i2_at_totalgoalconceded <- sum(i2_goalconceded_vec_at)
  i2_at_matches_concede <- length(which(i2_goalconceded_vec_at > 0))
  i2_at_matches_without_concede <- length(which(i2_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  i2_totalgoals_vec_ht <- as.vector(i2_totalgoals_h[i2_hometeamindex,])
  i2_totalgoals_vec_ht[is.na(i2_totalgoals_vec_ht)] <- ""
  i2_totalgoals_vec_ht <- i2_totalgoals_vec_ht[i2_totalgoals_vec_ht != ""]
  i2_totalgoals_vec_ht  <-tail(i2_totalgoals_vec_ht,6)
  i2_totalgoals_vec_ht  <- as.numeric(i2_totalgoals_vec_ht)
  i2_totalgoals_vec_ht
  i2_ht_totalgoals <- sum(i2_totalgoals_vec_ht)
  i2_ht_avgtotalgoals <- (i2_ht_totalgoals/6)
  i2_ht_no_of_ov25 <- length(which(i2_totalgoals_vec_ht >= 3))
  i2_ht_no_of_un25 <- length(which(i2_totalgoals_vec_ht <= 2))
  #awayteam
  i2_totalgoals_vec_at <- as.vector(i2_totalgoals_h[i2_awayteamindex,])
  i2_totalgoals_vec_at[is.na(i2_totalgoals_vec_at)] <- ""
  i2_totalgoals_vec_at <- i2_totalgoals_vec_at[i2_totalgoals_vec_at != ""]
  i2_totalgoals_vec_at  <-tail(i2_totalgoals_vec_at,6)
  i2_totalgoals_vec_at  <- as.numeric(i2_totalgoals_vec_at)
  i2_totalgoals_vec_at
  i2_at_totalgoals <- sum(i2_totalgoals_vec_at)
  i2_at_avgtotalgoals <- (i2_at_totalgoals/6)
  i2_at_no_of_ov25 <- length(which(i2_totalgoals_vec_at >= 3))
  i2_at_no_of_un25 <- length(which(i2_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  i2_winmargin_vec_ht <- as.vector(i2_winmargin_h[i2_hometeamindex,])
  i2_winmargin_vec_ht[is.na(i2_winmargin_vec_ht)] <- ""
  i2_winmargin_vec_ht <- i2_winmargin_vec_ht[i2_winmargin_vec_ht != ""]
  i2_winmargin_vec_ht  <-tail(i2_winmargin_vec_ht,6)
  i2_winmargin_vec_ht  <- as.numeric(i2_winmargin_vec_ht)

  i2_ht_totalwinmargin <- sum(i2_winmargin_vec_ht)
  i2_ht_no_of_winmargin_ov0 <- length(which(i2_winmargin_vec_ht >= 0))
  i2_ht_no_of_winmargin_ov1 <- length(which(i2_winmargin_vec_ht >= 1))
  i2_ht_no_of_winmargin_un0 <- length(which(i2_winmargin_vec_ht <= 0))
  i2_ht_no_of_winmargin_un1 <- length(which(i2_winmargin_vec_ht <= 1))
  #awayteam
  i2_winmargin_vec_at <- as.vector(i2_winmargin_h[i2_awayteamindex,])
  i2_winmargin_vec_at[is.na(i2_winmargin_vec_at)] <- ""
  i2_winmargin_vec_at <- i2_winmargin_vec_at[i2_winmargin_vec_at != ""]
  i2_winmargin_vec_at  <-tail(i2_winmargin_vec_at,6)
  i2_winmargin_vec_at  <- as.numeric(i2_winmargin_vec_at)

  i2_at_totalwinmargin <- sum(i2_winmargin_vec_at)
  i2_at_no_of_winmargin_ov0 <- length(which(i2_winmargin_vec_at >= 0))
  i2_at_no_of_winmargin_ov1 <- length(which(i2_winmargin_vec_at >= 1))
  i2_at_no_of_winmargin_un0 <- length(which(i2_winmargin_vec_at <= 0))
  i2_at_no_of_winmargin_un1 <- length(which(i2_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  i2_ht_last6points <- i2_ht_numberof_wins*3 + i2_ht_numberof_draws*1
  i2_at_last6points <- i2_at_numberof_wins*3 + i2_at_numberof_draws*1

  ifelse(i2_ht_last6points > i2_at_last6points && i2_ht_totalwinmargin > i2_at_totalwinmargin,i2_3waypick <- "1",i2_3waypick <- "X2")

  ifelse(i2_at_last6points > i2_ht_last6points && i2_at_totalwinmargin > i2_ht_totalwinmargin,i2_3waypick <- "2",i2_3waypick <- "1X")

  if(i2_ht_no_of_ov25 + i2_at_no_of_ov25 >= 6) {i2_goalspick <- "ov25"} else {i2_goalspick <- "un25"}

  if(i2_ht_no_of_un25 + i2_at_no_of_un25 >= 6) {i2_goalspick <- "un25"} else {i2_goalspick <- "ov25"}

  i2_prediction[i2_row] <- rbind(paste(i2_3waypick,i2_goalspick,sep = ","))

}

i2_prediction <- as.data.frame(i2_prediction)
colnames(i2_prediction) <- "prediction"
i2_prediction

i2_picks <- cbind(I2_fixtures$Div,I2_fixtures$HomeTeam_i2,I2_fixtures$AwayTeam_i2,i2_prediction)
colnames(i2_picks)[1] <- "picks_Div"
colnames(i2_picks)[2] <- "picks_HomeTeam"
colnames(i2_picks)[3] <- "picks_AwayTeam"
i2_picks$matchid <- paste(i2_picks$picks_HomeTeam,i2_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of I2
#N1
N1_fixtures$Hometeam_n1_index <- match(N1_fixtures$HomeTeam_n1,n1_teams)
N1_fixtures$Awayteam_n1_index <- match(N1_fixtures$AwayTeam_n1,n1_teams)
n1_prediction <- c()
for(n1_row in 1:nrow(N1_fixtures))
{

  n1_hometeamindex <- N1_fixtures[n1_row,"Hometeam_n1_index"]
  n1_awayteamindex <- N1_fixtures[n1_row,"Awayteam_n1_index"]
  #analyse team form
  #home team
  n1_form_vec_ht <- as.vector(n1_form_h[n1_hometeamindex,])
  n1_form_vec_ht[is.na(n1_form_vec_ht)] <- ""
  n1_form_vec_ht <- n1_form_vec_ht[n1_form_vec_ht != ""]
  n1_form_vec_ht  <-tail(n1_form_vec_ht,6)
  n1_ht_numberof_wins <- length(which(n1_form_vec_ht == "W"))
  n1_ht_numberof_draws <- length(which(n1_form_vec_ht == "D"))
  n1_ht_numberof_loss <- length(which(n1_form_vec_ht == "L"))
  #awayteam
  n1_form_vec_at <- as.vector(n1_form_h[n1_awayteamindex,])
  n1_form_vec_at[is.na(n1_form_vec_at)] <- ""
  n1_form_vec_at <- n1_form_vec_at[n1_form_vec_at != ""]
  n1_form_vec_at  <-tail(n1_form_vec_at,6)
  n1_at_numberof_wins <- length(which(n1_form_vec_at == "W"))
  n1_at_numberof_draws <- length(which(n1_form_vec_at == "D"))
  n1_at_numberof_loss <- length(which(n1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  n1_goalscored_vec_ht <- as.vector(n1_goalscored_h[n1_hometeamindex,])
  n1_goalscored_vec_ht[is.na(n1_goalscored_vec_ht)] <- ""
  n1_goalscored_vec_ht <- n1_goalscored_vec_ht[n1_goalscored_vec_ht != ""]
  n1_goalscored_vec_ht  <-tail(n1_goalscored_vec_ht,6)
  n1_goalscored_vec_ht  <- as.numeric(n1_goalscored_vec_ht)
  n1_ht_totalgoalscored <- sum(n1_goalscored_vec_ht)
  n1_ht_matches_scoring <- length(which(n1_goalscored_vec_ht > 0))
  n1_ht_matches_without_scoring <- length(which(n1_goalscored_vec_ht == "0"))
  #awayteam
  n1_goalscored_vec_at <- as.vector(n1_goalscored_h[n1_awayteamindex,])
  n1_goalscored_vec_at[is.na(n1_goalscored_vec_at)] <- ""
  n1_goalscored_vec_at <- n1_goalscored_vec_at[n1_goalscored_vec_at != ""]
  n1_goalscored_vec_at  <-tail(n1_goalscored_vec_at,6)
  n1_goalscored_vec_at  <- as.numeric(n1_goalscored_vec_at)
  n1_at_totalgoalscored <- sum(n1_goalscored_vec_at)
  n1_at_matches_scoring <- length(which(n1_goalscored_vec_at > 0))
  n1_at_matches_without_scoring <- length(which(n1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  n1_goalconceded_vec_ht <- as.vector(n1_goalconceded_h[n1_hometeamindex,])
  n1_goalconceded_vec_ht[is.na(n1_goalconceded_vec_ht)] <- ""
  n1_goalconceded_vec_ht <- n1_goalconceded_vec_ht[n1_goalconceded_vec_ht != ""]
  n1_goalconceded_vec_ht  <-tail(n1_goalconceded_vec_ht,6)
  n1_goalconceded_vec_ht  <- as.numeric(n1_goalconceded_vec_ht)
  n1_goalconceded_vec_ht
  n1_ht_totalgoalconceded <- sum(n1_goalconceded_vec_ht)
  n1_ht_matches_concede <- length(which(n1_goalconceded_vec_ht > 0))
  n1_ht_matches_without_concede <- length(which(n1_goalconceded_vec_ht == "0"))
  #awayteam
  n1_goalconceded_vec_at <- as.vector(n1_goalconceded_h[n1_awayteamindex,])
  n1_goalconceded_vec_at[is.na(n1_goalconceded_vec_at)] <- ""
  n1_goalconceded_vec_at <- n1_goalconceded_vec_at[n1_goalconceded_vec_at != ""]
  n1_goalconceded_vec_at  <-tail(n1_goalconceded_vec_at,6)
  n1_goalconceded_vec_at  <- as.numeric(n1_goalconceded_vec_at)
  n1_at_totalgoalconceded <- sum(n1_goalconceded_vec_at)
  n1_at_matches_concede <- length(which(n1_goalconceded_vec_at > 0))
  n1_at_matches_without_concede <- length(which(n1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  n1_totalgoals_vec_ht <- as.vector(n1_totalgoals_h[n1_hometeamindex,])
  n1_totalgoals_vec_ht[is.na(n1_totalgoals_vec_ht)] <- ""
  n1_totalgoals_vec_ht <- n1_totalgoals_vec_ht[n1_totalgoals_vec_ht != ""]
  n1_totalgoals_vec_ht  <-tail(n1_totalgoals_vec_ht,6)
  n1_totalgoals_vec_ht  <- as.numeric(n1_totalgoals_vec_ht)
  n1_totalgoals_vec_ht
  n1_ht_totalgoals <- sum(n1_totalgoals_vec_ht)
  n1_ht_avgtotalgoals <- (n1_ht_totalgoals/6)
  n1_ht_no_of_ov25 <- length(which(n1_totalgoals_vec_ht >= 3))
  n1_ht_no_of_un25 <- length(which(n1_totalgoals_vec_ht <= 2))
  #awayteam
  n1_totalgoals_vec_at <- as.vector(n1_totalgoals_h[n1_awayteamindex,])
  n1_totalgoals_vec_at[is.na(n1_totalgoals_vec_at)] <- ""
  n1_totalgoals_vec_at <- n1_totalgoals_vec_at[n1_totalgoals_vec_at != ""]
  n1_totalgoals_vec_at  <-tail(n1_totalgoals_vec_at,6)
  n1_totalgoals_vec_at  <- as.numeric(n1_totalgoals_vec_at)
  n1_totalgoals_vec_at
  n1_at_totalgoals <- sum(n1_totalgoals_vec_at)
  n1_at_avgtotalgoals <- (n1_at_totalgoals/6)
  n1_at_no_of_ov25 <- length(which(n1_totalgoals_vec_at >= 3))
  n1_at_no_of_un25 <- length(which(n1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  n1_winmargin_vec_ht <- as.vector(n1_winmargin_h[n1_hometeamindex,])
  n1_winmargin_vec_ht[is.na(n1_winmargin_vec_ht)] <- ""
  n1_winmargin_vec_ht <- n1_winmargin_vec_ht[n1_winmargin_vec_ht != ""]
  n1_winmargin_vec_ht  <-tail(n1_winmargin_vec_ht,6)
  n1_winmargin_vec_ht  <- as.numeric(n1_winmargin_vec_ht)

  n1_ht_totalwinmargin <- sum(n1_winmargin_vec_ht)
  n1_ht_no_of_winmargin_ov0 <- length(which(n1_winmargin_vec_ht >= 0))
  n1_ht_no_of_winmargin_ov1 <- length(which(n1_winmargin_vec_ht >= 1))
  n1_ht_no_of_winmargin_un0 <- length(which(n1_winmargin_vec_ht <= 0))
  n1_ht_no_of_winmargin_un1 <- length(which(n1_winmargin_vec_ht <= 1))
  #awayteam
  n1_winmargin_vec_at <- as.vector(n1_winmargin_h[n1_awayteamindex,])
  n1_winmargin_vec_at[is.na(n1_winmargin_vec_at)] <- ""
  n1_winmargin_vec_at <- n1_winmargin_vec_at[n1_winmargin_vec_at != ""]
  n1_winmargin_vec_at  <-tail(n1_winmargin_vec_at,6)
  n1_winmargin_vec_at  <- as.numeric(n1_winmargin_vec_at)

  n1_at_totalwinmargin <- sum(n1_winmargin_vec_at)
  n1_at_no_of_winmargin_ov0 <- length(which(n1_winmargin_vec_at >= 0))
  n1_at_no_of_winmargin_ov1 <- length(which(n1_winmargin_vec_at >= 1))
  n1_at_no_of_winmargin_un0 <- length(which(n1_winmargin_vec_at <= 0))
  n1_at_no_of_winmargin_un1 <- length(which(n1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  n1_ht_last6points <- n1_ht_numberof_wins*3 + n1_ht_numberof_draws*1
  n1_at_last6points <- n1_at_numberof_wins*3 + n1_at_numberof_draws*1

  ifelse(n1_ht_last6points > n1_at_last6points && n1_ht_totalwinmargin > n1_at_totalwinmargin,n1_3waypick <- "1",n1_3waypick <- "X2")

  ifelse(n1_at_last6points > n1_ht_last6points && n1_at_totalwinmargin > n1_ht_totalwinmargin,n1_3waypick <- "2",n1_3waypick <- "1X")


  if(n1_ht_no_of_ov25 + n1_at_no_of_ov25 >= 6) {n1_goalspick <- "ov25"} else {n1_goalspick <- "un25"}

  if(n1_ht_no_of_un25 + n1_at_no_of_un25 >= 6) {n1_goalspick <- "un25"} else {n1_goalspick <- "ov25"}

  n1_prediction[n1_row] <- rbind(paste(n1_3waypick,n1_goalspick,sep = ","))

}

n1_prediction <- as.data.frame(n1_prediction)
colnames(n1_prediction) <- "prediction"
n1_prediction

n1_picks <- cbind(N1_fixtures$Div,N1_fixtures$HomeTeam_n1,N1_fixtures$AwayTeam_n1,n1_prediction)
colnames(n1_picks)[1] <- "picks_Div"
colnames(n1_picks)[2] <- "picks_HomeTeam"
colnames(n1_picks)[3] <- "picks_AwayTeam"
n1_picks$matchid <- paste(n1_picks$picks_HomeTeam,n1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of N1
#P1
P1_fixtures$Hometeam_p1_index <- match(P1_fixtures$HomeTeam_p1,p1_teams)
P1_fixtures$Awayteam_p1_index <- match(P1_fixtures$AwayTeam_p1,p1_teams)
p1_prediction <- c()
for(p1_row in 1:nrow(P1_fixtures))
{

  p1_hometeamindex <- P1_fixtures[p1_row,"Hometeam_p1_index"]
  p1_awayteamindex <- P1_fixtures[p1_row,"Awayteam_p1_index"]
  #analyse team form
  #home team
  p1_form_vec_ht <- as.vector(p1_form_h[p1_hometeamindex,])
  p1_form_vec_ht[is.na(p1_form_vec_ht)] <- ""
  p1_form_vec_ht <- p1_form_vec_ht[p1_form_vec_ht != ""]
  p1_form_vec_ht  <-tail(p1_form_vec_ht,6)
  p1_ht_numberof_wins <- length(which(p1_form_vec_ht == "W"))
  p1_ht_numberof_draws <- length(which(p1_form_vec_ht == "D"))
  p1_ht_numberof_loss <- length(which(p1_form_vec_ht == "L"))
  #awayteam
  p1_form_vec_at <- as.vector(p1_form_h[p1_awayteamindex,])
  p1_form_vec_at[is.na(p1_form_vec_at)] <- ""
  p1_form_vec_at <- p1_form_vec_at[p1_form_vec_at != ""]
  p1_form_vec_at  <-tail(p1_form_vec_at,6)
  p1_at_numberof_wins <- length(which(p1_form_vec_at == "W"))
  p1_at_numberof_draws <- length(which(p1_form_vec_at == "D"))
  p1_at_numberof_loss <- length(which(p1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  p1_goalscored_vec_ht <- as.vector(p1_goalscored_h[p1_hometeamindex,])
  p1_goalscored_vec_ht[is.na(p1_goalscored_vec_ht)] <- ""
  p1_goalscored_vec_ht <- p1_goalscored_vec_ht[p1_goalscored_vec_ht != ""]
  p1_goalscored_vec_ht  <-tail(p1_goalscored_vec_ht,6)
  p1_goalscored_vec_ht  <- as.numeric(p1_goalscored_vec_ht)
  p1_ht_totalgoalscored <- sum(p1_goalscored_vec_ht)
  p1_ht_matches_scoring <- length(which(p1_goalscored_vec_ht > 0))
  p1_ht_matches_without_scoring <- length(which(p1_goalscored_vec_ht == "0"))
  #awayteam
  p1_goalscored_vec_at <- as.vector(p1_goalscored_h[p1_awayteamindex,])
  p1_goalscored_vec_at[is.na(p1_goalscored_vec_at)] <- ""
  p1_goalscored_vec_at <- p1_goalscored_vec_at[p1_goalscored_vec_at != ""]
  p1_goalscored_vec_at  <-tail(p1_goalscored_vec_at,6)
  p1_goalscored_vec_at  <- as.numeric(p1_goalscored_vec_at)
  p1_at_totalgoalscored <- sum(p1_goalscored_vec_at)
  p1_at_matches_scoring <- length(which(p1_goalscored_vec_at > 0))
  p1_at_matches_without_scoring <- length(which(p1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  p1_goalconceded_vec_ht <- as.vector(p1_goalconceded_h[p1_hometeamindex,])
  p1_goalconceded_vec_ht[is.na(p1_goalconceded_vec_ht)] <- ""
  p1_goalconceded_vec_ht <- p1_goalconceded_vec_ht[p1_goalconceded_vec_ht != ""]
  p1_goalconceded_vec_ht  <-tail(p1_goalconceded_vec_ht,6)
  p1_goalconceded_vec_ht  <- as.numeric(p1_goalconceded_vec_ht)
  p1_goalconceded_vec_ht
  p1_ht_totalgoalconceded <- sum(p1_goalconceded_vec_ht)
  p1_ht_matches_concede <- length(which(p1_goalconceded_vec_ht > 0))
  p1_ht_matches_without_concede <- length(which(p1_goalconceded_vec_ht == "0"))
  #awayteam
  p1_goalconceded_vec_at <- as.vector(p1_goalconceded_h[p1_awayteamindex,])
  p1_goalconceded_vec_at[is.na(p1_goalconceded_vec_at)] <- ""
  p1_goalconceded_vec_at <- p1_goalconceded_vec_at[p1_goalconceded_vec_at != ""]
  p1_goalconceded_vec_at  <-tail(p1_goalconceded_vec_at,6)
  p1_goalconceded_vec_at  <- as.numeric(p1_goalconceded_vec_at)
  p1_at_totalgoalconceded <- sum(p1_goalconceded_vec_at)
  p1_at_matches_concede <- length(which(p1_goalconceded_vec_at > 0))
  p1_at_matches_without_concede <- length(which(p1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  p1_totalgoals_vec_ht <- as.vector(p1_totalgoals_h[p1_hometeamindex,])
  p1_totalgoals_vec_ht[is.na(p1_totalgoals_vec_ht)] <- ""
  p1_totalgoals_vec_ht <- p1_totalgoals_vec_ht[p1_totalgoals_vec_ht != ""]
  p1_totalgoals_vec_ht  <-tail(p1_totalgoals_vec_ht,6)
  p1_totalgoals_vec_ht  <- as.numeric(p1_totalgoals_vec_ht)
  p1_totalgoals_vec_ht
  p1_ht_totalgoals <- sum(p1_totalgoals_vec_ht)
  p1_ht_avgtotalgoals <- (p1_ht_totalgoals/6)
  p1_ht_no_of_ov25 <- length(which(p1_totalgoals_vec_ht >= 3))
  p1_ht_no_of_un25 <- length(which(p1_totalgoals_vec_ht <= 2))
  #awayteam
  p1_totalgoals_vec_at <- as.vector(p1_totalgoals_h[p1_awayteamindex,])
  p1_totalgoals_vec_at[is.na(p1_totalgoals_vec_at)] <- ""
  p1_totalgoals_vec_at <- p1_totalgoals_vec_at[p1_totalgoals_vec_at != ""]
  p1_totalgoals_vec_at  <-tail(p1_totalgoals_vec_at,6)
  p1_totalgoals_vec_at  <- as.numeric(p1_totalgoals_vec_at)
  p1_totalgoals_vec_at
  p1_at_totalgoals <- sum(p1_totalgoals_vec_at)
  p1_at_avgtotalgoals <- (p1_at_totalgoals/6)
  p1_at_no_of_ov25 <- length(which(p1_totalgoals_vec_at >= 3))
  p1_at_no_of_un25 <- length(which(p1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  p1_winmargin_vec_ht <- as.vector(p1_winmargin_h[p1_hometeamindex,])
  p1_winmargin_vec_ht[is.na(p1_winmargin_vec_ht)] <- ""
  p1_winmargin_vec_ht <- p1_winmargin_vec_ht[p1_winmargin_vec_ht != ""]
  p1_winmargin_vec_ht  <-tail(p1_winmargin_vec_ht,6)
  p1_winmargin_vec_ht  <- as.numeric(p1_winmargin_vec_ht)

  p1_ht_totalwinmargin <- sum(p1_winmargin_vec_ht)
  p1_ht_no_of_winmargin_ov0 <- length(which(p1_winmargin_vec_ht >= 0))
  p1_ht_no_of_winmargin_ov1 <- length(which(p1_winmargin_vec_ht >= 1))
  p1_ht_no_of_winmargin_un0 <- length(which(p1_winmargin_vec_ht <= 0))
  p1_ht_no_of_winmargin_un1 <- length(which(p1_winmargin_vec_ht <= 1))
  #awayteam
  p1_winmargin_vec_at <- as.vector(p1_winmargin_h[p1_awayteamindex,])
  p1_winmargin_vec_at[is.na(p1_winmargin_vec_at)] <- ""
  p1_winmargin_vec_at <- p1_winmargin_vec_at[p1_winmargin_vec_at != ""]
  p1_winmargin_vec_at  <-tail(p1_winmargin_vec_at,6)
  p1_winmargin_vec_at  <- as.numeric(p1_winmargin_vec_at)

  p1_at_totalwinmargin <- sum(p1_winmargin_vec_at)
  p1_at_no_of_winmargin_ov0 <- length(which(p1_winmargin_vec_at >= 0))
  p1_at_no_of_winmargin_ov1 <- length(which(p1_winmargin_vec_at >= 1))
  p1_at_no_of_winmargin_un0 <- length(which(p1_winmargin_vec_at <= 0))
  p1_at_no_of_winmargin_un1 <- length(which(p1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  p1_ht_last6points <- p1_ht_numberof_wins*3 + p1_ht_numberof_draws*1
  p1_at_last6points <- p1_at_numberof_wins*3 + p1_at_numberof_draws*1

  ifelse(p1_ht_last6points > p1_at_last6points && p1_ht_totalwinmargin > p1_at_totalwinmargin,p1_3waypick <- "1",p1_3waypick <- "X2")

  ifelse(p1_at_last6points > p1_ht_last6points && p1_at_totalwinmargin > p1_ht_totalwinmargin,p1_3waypick <- "2",p1_3waypick <- "1X")

  if(p1_ht_no_of_ov25 + p1_at_no_of_ov25 >= 6) {p1_goalspick <- "ov25"} else {p1_goalspick <- "un25"}

  if(p1_ht_no_of_un25 + p1_at_no_of_un25 >= 6) {p1_goalspick <- "un25"} else {p1_goalspick <- "ov25"}


  p1_prediction[p1_row] <- rbind(paste(p1_3waypick,p1_goalspick,sep = ","))

}

p1_prediction <- as.data.frame(p1_prediction)
colnames(p1_prediction) <- "prediction"
p1_prediction

p1_picks <- cbind(P1_fixtures$Div,P1_fixtures$HomeTeam_p1,P1_fixtures$AwayTeam_p1,p1_prediction)
colnames(p1_picks)[1] <- "picks_Div"
colnames(p1_picks)[2] <- "picks_HomeTeam"
colnames(p1_picks)[3] <- "picks_AwayTeam"
p1_picks$matchid <- paste(p1_picks$picks_HomeTeam,p1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of P1
#SP1
SP1_fixtures$Hometeam_sp1_index <- match(SP1_fixtures$HomeTeam_sp1,sp1_teams)
SP1_fixtures$Awayteam_sp1_index <- match(SP1_fixtures$AwayTeam_sp1,sp1_teams)
sp1_prediction <- c()
for(sp1_row in 1:nrow(SP1_fixtures))
{

  sp1_hometeamindex <- SP1_fixtures[sp1_row,"Hometeam_sp1_index"]
  sp1_awayteamindex <- SP1_fixtures[sp1_row,"Awayteam_sp1_index"]
  #analyse team form
  #home team
  sp1_form_vec_ht <- as.vector(sp1_form_h[sp1_hometeamindex,])
  sp1_form_vec_ht[is.na(sp1_form_vec_ht)] <- ""
  sp1_form_vec_ht <- sp1_form_vec_ht[sp1_form_vec_ht != ""]
  sp1_form_vec_ht  <-tail(sp1_form_vec_ht,6)
  sp1_ht_numberof_wins <- length(which(sp1_form_vec_ht == "W"))
  sp1_ht_numberof_draws <- length(which(sp1_form_vec_ht == "D"))
  sp1_ht_numberof_loss <- length(which(sp1_form_vec_ht == "L"))
  #awayteam
  sp1_form_vec_at <- as.vector(sp1_form_h[sp1_awayteamindex,])
  sp1_form_vec_at[is.na(sp1_form_vec_at)] <- ""
  sp1_form_vec_at <- sp1_form_vec_at[sp1_form_vec_at != ""]
  sp1_form_vec_at  <-tail(sp1_form_vec_at,6)
  sp1_at_numberof_wins <- length(which(sp1_form_vec_at == "W"))
  sp1_at_numberof_draws <- length(which(sp1_form_vec_at == "D"))
  sp1_at_numberof_loss <- length(which(sp1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  sp1_goalscored_vec_ht <- as.vector(sp1_goalscored_h[sp1_hometeamindex,])
  sp1_goalscored_vec_ht[is.na(sp1_goalscored_vec_ht)] <- ""
  sp1_goalscored_vec_ht <- sp1_goalscored_vec_ht[sp1_goalscored_vec_ht != ""]
  sp1_goalscored_vec_ht  <-tail(sp1_goalscored_vec_ht,6)
  sp1_goalscored_vec_ht  <- as.numeric(sp1_goalscored_vec_ht)
  sp1_ht_totalgoalscored <- sum(sp1_goalscored_vec_ht)
  sp1_ht_matches_scoring <- length(which(sp1_goalscored_vec_ht > 0))
  sp1_ht_matches_without_scoring <- length(which(sp1_goalscored_vec_ht == "0"))
  #awayteam
  sp1_goalscored_vec_at <- as.vector(sp1_goalscored_h[sp1_awayteamindex,])
  sp1_goalscored_vec_at[is.na(sp1_goalscored_vec_at)] <- ""
  sp1_goalscored_vec_at <- sp1_goalscored_vec_at[sp1_goalscored_vec_at != ""]
  sp1_goalscored_vec_at  <-tail(sp1_goalscored_vec_at,6)
  sp1_goalscored_vec_at  <- as.numeric(sp1_goalscored_vec_at)
  sp1_at_totalgoalscored <- sum(sp1_goalscored_vec_at)
  sp1_at_matches_scoring <- length(which(sp1_goalscored_vec_at > 0))
  sp1_at_matches_without_scoring <- length(which(sp1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  sp1_goalconceded_vec_ht <- as.vector(sp1_goalconceded_h[sp1_hometeamindex,])
  sp1_goalconceded_vec_ht[is.na(sp1_goalconceded_vec_ht)] <- ""
  sp1_goalconceded_vec_ht <- sp1_goalconceded_vec_ht[sp1_goalconceded_vec_ht != ""]
  sp1_goalconceded_vec_ht  <-tail(sp1_goalconceded_vec_ht,6)
  sp1_goalconceded_vec_ht  <- as.numeric(sp1_goalconceded_vec_ht)
  sp1_goalconceded_vec_ht
  sp1_ht_totalgoalconceded <- sum(sp1_goalconceded_vec_ht)
  sp1_ht_matches_concede <- length(which(sp1_goalconceded_vec_ht > 0))
  sp1_ht_matches_without_concede <- length(which(sp1_goalconceded_vec_ht == "0"))
  #awayteam
  sp1_goalconceded_vec_at <- as.vector(sp1_goalconceded_h[sp1_awayteamindex,])
  sp1_goalconceded_vec_at[is.na(sp1_goalconceded_vec_at)] <- ""
  sp1_goalconceded_vec_at <- sp1_goalconceded_vec_at[sp1_goalconceded_vec_at != ""]
  sp1_goalconceded_vec_at  <-tail(sp1_goalconceded_vec_at,6)
  sp1_goalconceded_vec_at  <- as.numeric(sp1_goalconceded_vec_at)
  sp1_at_totalgoalconceded <- sum(sp1_goalconceded_vec_at)
  sp1_at_matches_concede <- length(which(sp1_goalconceded_vec_at > 0))
  sp1_at_matches_without_concede <- length(which(sp1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  sp1_totalgoals_vec_ht <- as.vector(sp1_totalgoals_h[sp1_hometeamindex,])
  sp1_totalgoals_vec_ht[is.na(sp1_totalgoals_vec_ht)] <- ""
  sp1_totalgoals_vec_ht <- sp1_totalgoals_vec_ht[sp1_totalgoals_vec_ht != ""]
  sp1_totalgoals_vec_ht  <-tail(sp1_totalgoals_vec_ht,6)
  sp1_totalgoals_vec_ht  <- as.numeric(sp1_totalgoals_vec_ht)
  sp1_totalgoals_vec_ht
  sp1_ht_totalgoals <- sum(sp1_totalgoals_vec_ht)
  sp1_ht_avgtotalgoals <- (sp1_ht_totalgoals/6)
  sp1_ht_no_of_ov25 <- length(which(sp1_totalgoals_vec_ht >= 3))
  sp1_ht_no_of_un25 <- length(which(sp1_totalgoals_vec_ht <= 2))
  #awayteam
  sp1_totalgoals_vec_at <- as.vector(sp1_totalgoals_h[sp1_awayteamindex,])
  sp1_totalgoals_vec_at[is.na(sp1_totalgoals_vec_at)] <- ""
  sp1_totalgoals_vec_at <- sp1_totalgoals_vec_at[sp1_totalgoals_vec_at != ""]
  sp1_totalgoals_vec_at  <-tail(sp1_totalgoals_vec_at,6)
  sp1_totalgoals_vec_at  <- as.numeric(sp1_totalgoals_vec_at)
  sp1_totalgoals_vec_at
  sp1_at_totalgoals <- sum(sp1_totalgoals_vec_at)
  sp1_at_avgtotalgoals <- (sp1_at_totalgoals/6)
  sp1_at_no_of_ov25 <- length(which(sp1_totalgoals_vec_at >= 3))
  sp1_at_no_of_un25 <- length(which(sp1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  sp1_winmargin_vec_ht <- as.vector(sp1_winmargin_h[sp1_hometeamindex,])
  sp1_winmargin_vec_ht[is.na(sp1_winmargin_vec_ht)] <- ""
  sp1_winmargin_vec_ht <- sp1_winmargin_vec_ht[sp1_winmargin_vec_ht != ""]
  sp1_winmargin_vec_ht  <-tail(sp1_winmargin_vec_ht,6)
  sp1_winmargin_vec_ht  <- as.numeric(sp1_winmargin_vec_ht)

  sp1_ht_totalwinmargin <- sum(sp1_winmargin_vec_ht)
  sp1_ht_no_of_winmargin_ov0 <- length(which(sp1_winmargin_vec_ht >= 0))
  sp1_ht_no_of_winmargin_ov1 <- length(which(sp1_winmargin_vec_ht >= 1))
  sp1_ht_no_of_winmargin_un0 <- length(which(sp1_winmargin_vec_ht <= 0))
  sp1_ht_no_of_winmargin_un1 <- length(which(sp1_winmargin_vec_ht <= 1))
  #awayteam
  sp1_winmargin_vec_at <- as.vector(sp1_winmargin_h[sp1_awayteamindex,])
  sp1_winmargin_vec_at[is.na(sp1_winmargin_vec_at)] <- ""
  sp1_winmargin_vec_at <- sp1_winmargin_vec_at[sp1_winmargin_vec_at != ""]
  sp1_winmargin_vec_at  <-tail(sp1_winmargin_vec_at,6)
  sp1_winmargin_vec_at  <- as.numeric(sp1_winmargin_vec_at)

  sp1_at_totalwinmargin <- sum(sp1_winmargin_vec_at)
  sp1_at_no_of_winmargin_ov0 <- length(which(sp1_winmargin_vec_at >= 0))
  sp1_at_no_of_winmargin_ov1 <- length(which(sp1_winmargin_vec_at >= 1))
  sp1_at_no_of_winmargin_un0 <- length(which(sp1_winmargin_vec_at <= 0))
  sp1_at_no_of_winmargin_un1 <- length(which(sp1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  sp1_ht_last6points <- sp1_ht_numberof_wins*3 + sp1_ht_numberof_draws*1
  sp1_at_last6points <- sp1_at_numberof_wins*3 + sp1_at_numberof_draws*1

  ifelse(sp1_ht_last6points > sp1_at_last6points && sp1_ht_totalwinmargin > sp1_at_totalwinmargin,sp1_3waypick <- "1",sp1_3waypick <- "X2")

  ifelse(sp1_at_last6points > sp1_ht_last6points && sp1_at_totalwinmargin > sp1_ht_totalwinmargin,sp1_3waypick <- "2",sp1_3waypick <- "1X")


  if(sp1_ht_no_of_ov25 + sp1_at_no_of_ov25 >= 6) {sp1_goalspick <- "ov25"} else {sp1_goalspick <- "un25"}

  if(sp1_ht_no_of_un25 + sp1_at_no_of_un25 >= 6) {sp1_goalspick <- "un25"} else {sp1_goalspick <- "ov25"}


  sp1_prediction[sp1_row] <- rbind(paste(sp1_3waypick,sp1_goalspick,sep = ","))

}

sp1_prediction <- as.data.frame(sp1_prediction)
colnames(sp1_prediction) <- "prediction"
sp1_prediction

sp1_picks <- cbind(SP1_fixtures$Div,SP1_fixtures$HomeTeam_sp1,SP1_fixtures$AwayTeam_sp1,sp1_prediction)
colnames(sp1_picks)[1] <- "picks_Div"
colnames(sp1_picks)[2] <- "picks_HomeTeam"
colnames(sp1_picks)[3] <- "picks_AwayTeam"
sp1_picks$matchid <- paste(sp1_picks$picks_HomeTeam,sp1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SP1
#SP2
SP2_fixtures$Hometeam_sp2_index <- match(SP2_fixtures$HomeTeam_sp2,sp2_teams)
SP2_fixtures$Awayteam_sp2_index <- match(SP2_fixtures$AwayTeam_sp2,sp2_teams)
sp2_prediction <- c()
for(sp2_row in 1:nrow(SP2_fixtures))
{

  sp2_hometeamindex <- SP2_fixtures[sp2_row,"Hometeam_sp2_index"]
  sp2_awayteamindex <- SP2_fixtures[sp2_row,"Awayteam_sp2_index"]
  #analyse team form
  #home team
  sp2_form_vec_ht <- as.vector(sp2_form_h[sp2_hometeamindex,])
  sp2_form_vec_ht[is.na(sp2_form_vec_ht)] <- ""
  sp2_form_vec_ht <- sp2_form_vec_ht[sp2_form_vec_ht != ""]
  sp2_form_vec_ht  <-tail(sp2_form_vec_ht,6)
  sp2_ht_numberof_wins <- length(which(sp2_form_vec_ht == "W"))
  sp2_ht_numberof_draws <- length(which(sp2_form_vec_ht == "D"))
  sp2_ht_numberof_loss <- length(which(sp2_form_vec_ht == "L"))
  #awayteam
  sp2_form_vec_at <- as.vector(sp2_form_h[sp2_awayteamindex,])
  sp2_form_vec_at[is.na(sp2_form_vec_at)] <- ""
  sp2_form_vec_at <- sp2_form_vec_at[sp2_form_vec_at != ""]
  sp2_form_vec_at  <-tail(sp2_form_vec_at,6)
  sp2_at_numberof_wins <- length(which(sp2_form_vec_at == "W"))
  sp2_at_numberof_draws <- length(which(sp2_form_vec_at == "D"))
  sp2_at_numberof_loss <- length(which(sp2_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  sp2_goalscored_vec_ht <- as.vector(sp2_goalscored_h[sp2_hometeamindex,])
  sp2_goalscored_vec_ht[is.na(sp2_goalscored_vec_ht)] <- ""
  sp2_goalscored_vec_ht <- sp2_goalscored_vec_ht[sp2_goalscored_vec_ht != ""]
  sp2_goalscored_vec_ht  <-tail(sp2_goalscored_vec_ht,6)
  sp2_goalscored_vec_ht  <- as.numeric(sp2_goalscored_vec_ht)
  sp2_ht_totalgoalscored <- sum(sp2_goalscored_vec_ht)
  sp2_ht_matches_scoring <- length(which(sp2_goalscored_vec_ht > 0))
  sp2_ht_matches_without_scoring <- length(which(sp2_goalscored_vec_ht == "0"))
  #awayteam
  sp2_goalscored_vec_at <- as.vector(sp2_goalscored_h[sp2_awayteamindex,])
  sp2_goalscored_vec_at[is.na(sp2_goalscored_vec_at)] <- ""
  sp2_goalscored_vec_at <- sp2_goalscored_vec_at[sp2_goalscored_vec_at != ""]
  sp2_goalscored_vec_at  <-tail(sp2_goalscored_vec_at,6)
  sp2_goalscored_vec_at  <- as.numeric(sp2_goalscored_vec_at)
  sp2_at_totalgoalscored <- sum(sp2_goalscored_vec_at)
  sp2_at_matches_scoring <- length(which(sp2_goalscored_vec_at > 0))
  sp2_at_matches_without_scoring <- length(which(sp2_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  sp2_goalconceded_vec_ht <- as.vector(sp2_goalconceded_h[sp2_hometeamindex,])
  sp2_goalconceded_vec_ht[is.na(sp2_goalconceded_vec_ht)] <- ""
  sp2_goalconceded_vec_ht <- sp2_goalconceded_vec_ht[sp2_goalconceded_vec_ht != ""]
  sp2_goalconceded_vec_ht  <-tail(sp2_goalconceded_vec_ht,6)
  sp2_goalconceded_vec_ht  <- as.numeric(sp2_goalconceded_vec_ht)
  sp2_goalconceded_vec_ht
  sp2_ht_totalgoalconceded <- sum(sp2_goalconceded_vec_ht)
  sp2_ht_matches_concede <- length(which(sp2_goalconceded_vec_ht > 0))
  sp2_ht_matches_without_concede <- length(which(sp2_goalconceded_vec_ht == "0"))
  #awayteam
  sp2_goalconceded_vec_at <- as.vector(sp2_goalconceded_h[sp2_awayteamindex,])
  sp2_goalconceded_vec_at[is.na(sp2_goalconceded_vec_at)] <- ""
  sp2_goalconceded_vec_at <- sp2_goalconceded_vec_at[sp2_goalconceded_vec_at != ""]
  sp2_goalconceded_vec_at  <-tail(sp2_goalconceded_vec_at,6)
  sp2_goalconceded_vec_at  <- as.numeric(sp2_goalconceded_vec_at)
  sp2_at_totalgoalconceded <- sum(sp2_goalconceded_vec_at)
  sp2_at_matches_concede <- length(which(sp2_goalconceded_vec_at > 0))
  sp2_at_matches_without_concede <- length(which(sp2_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  sp2_totalgoals_vec_ht <- as.vector(sp2_totalgoals_h[sp2_hometeamindex,])
  sp2_totalgoals_vec_ht[is.na(sp2_totalgoals_vec_ht)] <- ""
  sp2_totalgoals_vec_ht <- sp2_totalgoals_vec_ht[sp2_totalgoals_vec_ht != ""]
  sp2_totalgoals_vec_ht  <-tail(sp2_totalgoals_vec_ht,6)
  sp2_totalgoals_vec_ht  <- as.numeric(sp2_totalgoals_vec_ht)
  sp2_totalgoals_vec_ht
  sp2_ht_totalgoals <- sum(sp2_totalgoals_vec_ht)
  sp2_ht_avgtotalgoals <- (sp2_ht_totalgoals/6)
  sp2_ht_no_of_ov25 <- length(which(sp2_totalgoals_vec_ht >= 3))
  sp2_ht_no_of_un25 <- length(which(sp2_totalgoals_vec_ht <= 2))
  sp2_at_avgtotalgoals
  #awayteam
  sp2_totalgoals_vec_at <- as.vector(sp2_totalgoals_h[sp2_awayteamindex,])
  sp2_totalgoals_vec_at[is.na(sp2_totalgoals_vec_at)] <- ""
  sp2_totalgoals_vec_at <- sp2_totalgoals_vec_at[sp2_totalgoals_vec_at != ""]
  sp2_totalgoals_vec_at  <-tail(sp2_totalgoals_vec_at,6)
  sp2_totalgoals_vec_at  <- as.numeric(sp2_totalgoals_vec_at)
  sp2_totalgoals_vec_at
  sp2_at_totalgoals <- sum(sp2_totalgoals_vec_at)
  sp2_at_avgtotalgoals <- (sp2_at_totalgoals/6)
  sp2_at_no_of_ov25 <- length(which(sp2_totalgoals_vec_at >= 3))
  sp2_at_no_of_un25 <- length(which(sp2_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  sp2_winmargin_vec_ht <- as.vector(sp2_winmargin_h[sp2_hometeamindex,])
  sp2_winmargin_vec_ht[is.na(sp2_winmargin_vec_ht)] <- ""
  sp2_winmargin_vec_ht <- sp2_winmargin_vec_ht[sp2_winmargin_vec_ht != ""]
  sp2_winmargin_vec_ht  <-tail(sp2_winmargin_vec_ht,6)
  sp2_winmargin_vec_ht  <- as.numeric(sp2_winmargin_vec_ht)

  sp2_ht_totalwinmargin <- sum(sp2_winmargin_vec_ht)
  sp2_ht_no_of_winmargin_ov0 <- length(which(sp2_winmargin_vec_ht >= 0))
  sp2_ht_no_of_winmargin_ov1 <- length(which(sp2_winmargin_vec_ht >= 1))
  sp2_ht_no_of_winmargin_un0 <- length(which(sp2_winmargin_vec_ht <= 0))
  sp2_ht_no_of_winmargin_un1 <- length(which(sp2_winmargin_vec_ht <= 1))
  #awayteam
  sp2_winmargin_vec_at <- as.vector(sp2_winmargin_h[sp2_awayteamindex,])
  sp2_winmargin_vec_at[is.na(sp2_winmargin_vec_at)] <- ""
  sp2_winmargin_vec_at <- sp2_winmargin_vec_at[sp2_winmargin_vec_at != ""]
  sp2_winmargin_vec_at  <-tail(sp2_winmargin_vec_at,6)
  sp2_winmargin_vec_at  <- as.numeric(sp2_winmargin_vec_at)

  sp2_at_totalwinmargin <- sum(sp2_winmargin_vec_at)
  sp2_at_no_of_winmargin_ov0 <- length(which(sp2_winmargin_vec_at >= 0))
  sp2_at_no_of_winmargin_ov1 <- length(which(sp2_winmargin_vec_at >= 1))
  sp2_at_no_of_winmargin_un0 <- length(which(sp2_winmargin_vec_at <= 0))
  sp2_at_no_of_winmargin_un1 <- length(which(sp2_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  sp2_ht_last6points <- sp2_ht_numberof_wins*3 + sp2_ht_numberof_draws*1
  sp2_at_last6points <- sp2_at_numberof_wins*3 + sp2_at_numberof_draws*1

  if(sp2_ht_last6points > sp2_at_last6points && sp2_ht_totalwinmargin > sp2_at_totalwinmargin) {sp2_3waypick <- "1"} else {sp2_3waypick <- "X2"}

  if(sp2_at_last6points > sp2_ht_last6points && sp2_at_totalwinmargin > sp2_ht_totalwinmargin){sp2_3waypick <- "2"} else {sp2_3waypick <- "1X"}

  if(sp2_ht_no_of_ov25 + sp2_at_no_of_ov25 >= 6) {sp2_goalspick <- "ov25"} else {sp2_goalspick <- "un25"}

  if(sp2_ht_no_of_un25 + sp2_at_no_of_un25 >= 6) {sp2_goalspick <- "un25"} else {sp2_goalspick <- "ov25"}


  sp2_prediction[sp2_row] <- rbind(paste(sp2_3waypick,sp2_goalspick,sep = ","))

}

sp2_prediction <- as.data.frame(sp2_prediction)
colnames(sp2_prediction) <- "prediction"
sp2_prediction

sp2_picks <- cbind(SP2_fixtures$Div,SP2_fixtures$HomeTeam_sp2,SP2_fixtures$AwayTeam_sp2,sp2_prediction)
colnames(sp2_picks)[1] <- "picks_Div"
colnames(sp2_picks)[2] <- "picks_HomeTeam"
colnames(sp2_picks)[3] <- "picks_AwayTeam"
sp2_picks$matchid <- paste(sp2_picks$picks_HomeTeam,sp2_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SP2
#SC0
SC0_fixtures$Hometeam_sc0_index <- match(SC0_fixtures$HomeTeam_sc0,sc0_teams)
SC0_fixtures$Awayteam_sc0_index <- match(SC0_fixtures$AwayTeam_sc0,sc0_teams)
sc0_prediction <- c()
for(sc0_row in 1:nrow(SC0_fixtures))
{

  sc0_hometeamindex <- SC0_fixtures[sc0_row,"Hometeam_sc0_index"]
  sc0_awayteamindex <- SC0_fixtures[sc0_row,"Awayteam_sc0_index"]
  #analyse team form
  #home team
  sc0_form_vec_ht <- as.vector(sc0_form_h[sc0_hometeamindex,])
  sc0_form_vec_ht[is.na(sc0_form_vec_ht)] <- ""
  sc0_form_vec_ht <- sc0_form_vec_ht[sc0_form_vec_ht != ""]
  sc0_form_vec_ht  <-tail(sc0_form_vec_ht,6)
  sc0_ht_numberof_wins <- length(which(sc0_form_vec_ht == "W"))
  sc0_ht_numberof_draws <- length(which(sc0_form_vec_ht == "D"))
  sc0_ht_numberof_loss <- length(which(sc0_form_vec_ht == "L"))
  #awayteam
  sc0_form_vec_at <- as.vector(sc0_form_h[sc0_awayteamindex,])
  sc0_form_vec_at[is.na(sc0_form_vec_at)] <- ""
  sc0_form_vec_at <- sc0_form_vec_at[sc0_form_vec_at != ""]
  sc0_form_vec_at  <-tail(sc0_form_vec_at,6)
  sc0_at_numberof_wins <- length(which(sc0_form_vec_at == "W"))
  sc0_at_numberof_draws <- length(which(sc0_form_vec_at == "D"))
  sc0_at_numberof_loss <- length(which(sc0_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  sc0_goalscored_vec_ht <- as.vector(sc0_goalscored_h[sc0_hometeamindex,])
  sc0_goalscored_vec_ht[is.na(sc0_goalscored_vec_ht)] <- ""
  sc0_goalscored_vec_ht <- sc0_goalscored_vec_ht[sc0_goalscored_vec_ht != ""]
  sc0_goalscored_vec_ht  <-tail(sc0_goalscored_vec_ht,6)
  sc0_goalscored_vec_ht  <- as.numeric(sc0_goalscored_vec_ht)
  sc0_ht_totalgoalscored <- sum(sc0_goalscored_vec_ht)
  sc0_ht_matches_scoring <- length(which(sc0_goalscored_vec_ht > 0))
  sc0_ht_matches_without_scoring <- length(which(sc0_goalscored_vec_ht == "0"))
  #awayteam
  sc0_goalscored_vec_at <- as.vector(sc0_goalscored_h[sc0_awayteamindex,])
  sc0_goalscored_vec_at[is.na(sc0_goalscored_vec_at)] <- ""
  sc0_goalscored_vec_at <- sc0_goalscored_vec_at[sc0_goalscored_vec_at != ""]
  sc0_goalscored_vec_at  <-tail(sc0_goalscored_vec_at,6)
  sc0_goalscored_vec_at  <- as.numeric(sc0_goalscored_vec_at)
  sc0_at_totalgoalscored <- sum(sc0_goalscored_vec_at)
  sc0_at_matches_scoring <- length(which(sc0_goalscored_vec_at > 0))
  sc0_at_matches_without_scoring <- length(which(sc0_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  sc0_goalconceded_vec_ht <- as.vector(sc0_goalconceded_h[sc0_hometeamindex,])
  sc0_goalconceded_vec_ht[is.na(sc0_goalconceded_vec_ht)] <- ""
  sc0_goalconceded_vec_ht <- sc0_goalconceded_vec_ht[sc0_goalconceded_vec_ht != ""]
  sc0_goalconceded_vec_ht  <-tail(sc0_goalconceded_vec_ht,6)
  sc0_goalconceded_vec_ht  <- as.numeric(sc0_goalconceded_vec_ht)
  sc0_goalconceded_vec_ht
  sc0_ht_totalgoalconceded <- sum(sc0_goalconceded_vec_ht)
  sc0_ht_matches_concede <- length(which(sc0_goalconceded_vec_ht > 0))
  sc0_ht_matches_without_concede <- length(which(sc0_goalconceded_vec_ht == "0"))
  #awayteam
  sc0_goalconceded_vec_at <- as.vector(sc0_goalconceded_h[sc0_awayteamindex,])
  sc0_goalconceded_vec_at[is.na(sc0_goalconceded_vec_at)] <- ""
  sc0_goalconceded_vec_at <- sc0_goalconceded_vec_at[sc0_goalconceded_vec_at != ""]
  sc0_goalconceded_vec_at  <-tail(sc0_goalconceded_vec_at,6)
  sc0_goalconceded_vec_at  <- as.numeric(sc0_goalconceded_vec_at)
  sc0_at_totalgoalconceded <- sum(sc0_goalconceded_vec_at)
  sc0_at_matches_concede <- length(which(sc0_goalconceded_vec_at > 0))
  sc0_at_matches_without_concede <- length(which(sc0_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  sc0_totalgoals_vec_ht <- as.vector(sc0_totalgoals_h[sc0_hometeamindex,])
  sc0_totalgoals_vec_ht[is.na(sc0_totalgoals_vec_ht)] <- ""
  sc0_totalgoals_vec_ht <- sc0_totalgoals_vec_ht[sc0_totalgoals_vec_ht != ""]
  sc0_totalgoals_vec_ht  <-tail(sc0_totalgoals_vec_ht,6)
  sc0_totalgoals_vec_ht  <- as.numeric(sc0_totalgoals_vec_ht)
  sc0_totalgoals_vec_ht
  sc0_ht_totalgoals <- sum(sc0_totalgoals_vec_ht)
  sc0_ht_avgtotalgoals <- (sc0_ht_totalgoals/6)
  sc0_ht_no_of_ov25 <- length(which(sc0_totalgoals_vec_ht >= 3))
  sc0_ht_no_of_un25 <- length(which(sc0_totalgoals_vec_ht <= 2))
  #awayteam
  sc0_totalgoals_vec_at <- as.vector(sc0_totalgoals_h[sc0_awayteamindex,])
  sc0_totalgoals_vec_at[is.na(sc0_totalgoals_vec_at)] <- ""
  sc0_totalgoals_vec_at <- sc0_totalgoals_vec_at[sc0_totalgoals_vec_at != ""]
  sc0_totalgoals_vec_at  <-tail(sc0_totalgoals_vec_at,6)
  sc0_totalgoals_vec_at  <- as.numeric(sc0_totalgoals_vec_at)
  sc0_totalgoals_vec_at
  sc0_at_totalgoals <- sum(sc0_totalgoals_vec_at)
  sc0_at_avgtotalgoals <- (sc0_at_totalgoals/6)
  sc0_at_no_of_ov25 <- length(which(sc0_totalgoals_vec_at >= 3))
  sc0_at_no_of_un25 <- length(which(sc0_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  sc0_winmargin_vec_ht <- as.vector(sc0_winmargin_h[sc0_hometeamindex,])
  sc0_winmargin_vec_ht[is.na(sc0_winmargin_vec_ht)] <- ""
  sc0_winmargin_vec_ht <- sc0_winmargin_vec_ht[sc0_winmargin_vec_ht != ""]
  sc0_winmargin_vec_ht  <-tail(sc0_winmargin_vec_ht,6)
  sc0_winmargin_vec_ht  <- as.numeric(sc0_winmargin_vec_ht)

  sc0_ht_totalwinmargin <- sum(sc0_winmargin_vec_ht)
  sc0_ht_no_of_winmargin_ov0 <- length(which(sc0_winmargin_vec_ht >= 0))
  sc0_ht_no_of_winmargin_ov1 <- length(which(sc0_winmargin_vec_ht >= 1))
  sc0_ht_no_of_winmargin_un0 <- length(which(sc0_winmargin_vec_ht <= 0))
  sc0_ht_no_of_winmargin_un1 <- length(which(sc0_winmargin_vec_ht <= 1))
  #awayteam
  sc0_winmargin_vec_at <- as.vector(sc0_winmargin_h[sc0_awayteamindex,])
  sc0_winmargin_vec_at[is.na(sc0_winmargin_vec_at)] <- ""
  sc0_winmargin_vec_at <- sc0_winmargin_vec_at[sc0_winmargin_vec_at != ""]
  sc0_winmargin_vec_at  <-tail(sc0_winmargin_vec_at,6)
  sc0_winmargin_vec_at  <- as.numeric(sc0_winmargin_vec_at)

  sc0_at_totalwinmargin <- sum(sc0_winmargin_vec_at)
  sc0_at_no_of_winmargin_ov0 <- length(which(sc0_winmargin_vec_at >= 0))
  sc0_at_no_of_winmargin_ov1 <- length(which(sc0_winmargin_vec_at >= 1))
  sc0_at_no_of_winmargin_un0 <- length(which(sc0_winmargin_vec_at <= 0))
  sc0_at_no_of_winmargin_un1 <- length(which(sc0_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  sc0_ht_last6points <- sc0_ht_numberof_wins*3 + sc0_ht_numberof_draws*1
  sc0_at_last6points <- sc0_at_numberof_wins*3 + sc0_at_numberof_draws*1

  ifelse(sc0_ht_last6points > sc0_at_last6points && sc0_ht_totalwinmargin > sc0_at_totalwinmargin,sc0_3waypick <- "1",sc0_3waypick <- "X2")

  ifelse(sc0_at_last6points > sc0_ht_last6points && sc0_at_totalwinmargin > sc0_ht_totalwinmargin,sc0_3waypick <- "2",sc0_3waypick <- "1X")


  if(sc0_ht_no_of_ov25 + sc0_at_no_of_ov25 >= 6) {sc0_goalspick <- "ov25"} else {sc0_goalspick <- "un25"}

  if(sc0_ht_no_of_un25 + sc0_at_no_of_un25 >= 6) {sc0_goalspick <- "un25"} else {sc0_goalspick <- "ov25"}

  sc0_prediction[sc0_row] <- rbind(paste(sc0_3waypick,sc0_goalspick,sep = ","))

}

sc0_prediction <- as.data.frame(sc0_prediction)
colnames(sc0_prediction) <- "prediction"
sc0_prediction

sc0_picks <- cbind(SC0_fixtures$Div,SC0_fixtures$HomeTeam_sc0,SC0_fixtures$AwayTeam_sc0,sc0_prediction)
colnames(sc0_picks)[1] <- "picks_Div"
colnames(sc0_picks)[2] <- "picks_HomeTeam"
colnames(sc0_picks)[3] <- "picks_AwayTeam"
sc0_picks$matchid <- paste(sc0_picks$picks_HomeTeam,sc0_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SC0
#SC1
SC1_fixtures$Hometeam_sc1_index <- match(SC1_fixtures$HomeTeam_sc1,sc1_teams)
SC1_fixtures$Awayteam_sc1_index <- match(SC1_fixtures$AwayTeam_sc1,sc1_teams)
sc1_prediction <- c()
for(sc1_row in 1:nrow(SC1_fixtures))
{

  sc1_hometeamindex <- SC1_fixtures[sc1_row,"Hometeam_sc1_index"]
  sc1_awayteamindex <- SC1_fixtures[sc1_row,"Awayteam_sc1_index"]
  #analyse team form
  #home team
  sc1_form_vec_ht <- as.vector(sc1_form_h[sc1_hometeamindex,])
  sc1_form_vec_ht[is.na(sc1_form_vec_ht)] <- ""
  sc1_form_vec_ht <- sc1_form_vec_ht[sc1_form_vec_ht != ""]
  sc1_form_vec_ht  <-tail(sc1_form_vec_ht,6)
  sc1_ht_numberof_wins <- length(which(sc1_form_vec_ht == "W"))
  sc1_ht_numberof_draws <- length(which(sc1_form_vec_ht == "D"))
  sc1_ht_numberof_loss <- length(which(sc1_form_vec_ht == "L"))
  #awayteam
  sc1_form_vec_at <- as.vector(sc1_form_h[sc1_awayteamindex,])
  sc1_form_vec_at[is.na(sc1_form_vec_at)] <- ""
  sc1_form_vec_at <- sc1_form_vec_at[sc1_form_vec_at != ""]
  sc1_form_vec_at  <-tail(sc1_form_vec_at,6)
  sc1_at_numberof_wins <- length(which(sc1_form_vec_at == "W"))
  sc1_at_numberof_draws <- length(which(sc1_form_vec_at == "D"))
  sc1_at_numberof_loss <- length(which(sc1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  sc1_goalscored_vec_ht <- as.vector(sc1_goalscored_h[sc1_hometeamindex,])
  sc1_goalscored_vec_ht[is.na(sc1_goalscored_vec_ht)] <- ""
  sc1_goalscored_vec_ht <- sc1_goalscored_vec_ht[sc1_goalscored_vec_ht != ""]
  sc1_goalscored_vec_ht  <-tail(sc1_goalscored_vec_ht,6)
  sc1_goalscored_vec_ht  <- as.numeric(sc1_goalscored_vec_ht)
  sc1_ht_totalgoalscored <- sum(sc1_goalscored_vec_ht)
  sc1_ht_matches_scoring <- length(which(sc1_goalscored_vec_ht > 0))
  sc1_ht_matches_without_scoring <- length(which(sc1_goalscored_vec_ht == "0"))
  #awayteam
  sc1_goalscored_vec_at <- as.vector(sc1_goalscored_h[sc1_awayteamindex,])
  sc1_goalscored_vec_at[is.na(sc1_goalscored_vec_at)] <- ""
  sc1_goalscored_vec_at <- sc1_goalscored_vec_at[sc1_goalscored_vec_at != ""]
  sc1_goalscored_vec_at  <-tail(sc1_goalscored_vec_at,6)
  sc1_goalscored_vec_at  <- as.numeric(sc1_goalscored_vec_at)
  sc1_at_totalgoalscored <- sum(sc1_goalscored_vec_at)
  sc1_at_matches_scoring <- length(which(sc1_goalscored_vec_at > 0))
  sc1_at_matches_without_scoring <- length(which(sc1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  sc1_goalconceded_vec_ht <- as.vector(sc1_goalconceded_h[sc1_hometeamindex,])
  sc1_goalconceded_vec_ht[is.na(sc1_goalconceded_vec_ht)] <- ""
  sc1_goalconceded_vec_ht <- sc1_goalconceded_vec_ht[sc1_goalconceded_vec_ht != ""]
  sc1_goalconceded_vec_ht  <-tail(sc1_goalconceded_vec_ht,6)
  sc1_goalconceded_vec_ht  <- as.numeric(sc1_goalconceded_vec_ht)
  sc1_goalconceded_vec_ht
  sc1_ht_totalgoalconceded <- sum(sc1_goalconceded_vec_ht)
  sc1_ht_matches_concede <- length(which(sc1_goalconceded_vec_ht > 0))
  sc1_ht_matches_without_concede <- length(which(sc1_goalconceded_vec_ht == "0"))
  #awayteam
  sc1_goalconceded_vec_at <- as.vector(sc1_goalconceded_h[sc1_awayteamindex,])
  sc1_goalconceded_vec_at[is.na(sc1_goalconceded_vec_at)] <- ""
  sc1_goalconceded_vec_at <- sc1_goalconceded_vec_at[sc1_goalconceded_vec_at != ""]
  sc1_goalconceded_vec_at  <-tail(sc1_goalconceded_vec_at,6)
  sc1_goalconceded_vec_at  <- as.numeric(sc1_goalconceded_vec_at)
  sc1_at_totalgoalconceded <- sum(sc1_goalconceded_vec_at)
  sc1_at_matches_concede <- length(which(sc1_goalconceded_vec_at > 0))
  sc1_at_matches_without_concede <- length(which(sc1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  sc1_totalgoals_vec_ht <- as.vector(sc1_totalgoals_h[sc1_hometeamindex,])
  sc1_totalgoals_vec_ht[is.na(sc1_totalgoals_vec_ht)] <- ""
  sc1_totalgoals_vec_ht <- sc1_totalgoals_vec_ht[sc1_totalgoals_vec_ht != ""]
  sc1_totalgoals_vec_ht  <-tail(sc1_totalgoals_vec_ht,6)
  sc1_totalgoals_vec_ht  <- as.numeric(sc1_totalgoals_vec_ht)
  sc1_totalgoals_vec_ht
  sc1_ht_totalgoals <- sum(sc1_totalgoals_vec_ht)
  sc1_ht_avgtotalgoals <- (sc1_ht_totalgoals/6)
  sc1_ht_no_of_ov25 <- length(which(sc1_totalgoals_vec_ht >= 3))
  sc1_ht_no_of_un25 <- length(which(sc1_totalgoals_vec_ht <= 2))
  #awayteam
  sc1_totalgoals_vec_at <- as.vector(sc1_totalgoals_h[sc1_awayteamindex,])
  sc1_totalgoals_vec_at[is.na(sc1_totalgoals_vec_at)] <- ""
  sc1_totalgoals_vec_at <- sc1_totalgoals_vec_at[sc1_totalgoals_vec_at != ""]
  sc1_totalgoals_vec_at  <-tail(sc1_totalgoals_vec_at,6)
  sc1_totalgoals_vec_at  <- as.numeric(sc1_totalgoals_vec_at)
  sc1_totalgoals_vec_at
  sc1_at_totalgoals <- sum(sc1_totalgoals_vec_at)
  sc1_at_avgtotalgoals <- (sc1_at_totalgoals/6)
  sc1_at_no_of_ov25 <- length(which(sc1_totalgoals_vec_at >= 3))
  sc1_at_no_of_un25 <- length(which(sc1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  sc1_winmargin_vec_ht <- as.vector(sc1_winmargin_h[sc1_hometeamindex,])
  sc1_winmargin_vec_ht[is.na(sc1_winmargin_vec_ht)] <- ""
  sc1_winmargin_vec_ht <- sc1_winmargin_vec_ht[sc1_winmargin_vec_ht != ""]
  sc1_winmargin_vec_ht  <-tail(sc1_winmargin_vec_ht,6)
  sc1_winmargin_vec_ht  <- as.numeric(sc1_winmargin_vec_ht)

  sc1_ht_totalwinmargin <- sum(sc1_winmargin_vec_ht)
  sc1_ht_no_of_winmargin_ov0 <- length(which(sc1_winmargin_vec_ht >= 0))
  sc1_ht_no_of_winmargin_ov1 <- length(which(sc1_winmargin_vec_ht >= 1))
  sc1_ht_no_of_winmargin_un0 <- length(which(sc1_winmargin_vec_ht <= 0))
  sc1_ht_no_of_winmargin_un1 <- length(which(sc1_winmargin_vec_ht <= 1))
  #awayteam
  sc1_winmargin_vec_at <- as.vector(sc1_winmargin_h[sc1_awayteamindex,])
  sc1_winmargin_vec_at[is.na(sc1_winmargin_vec_at)] <- ""
  sc1_winmargin_vec_at <- sc1_winmargin_vec_at[sc1_winmargin_vec_at != ""]
  sc1_winmargin_vec_at  <-tail(sc1_winmargin_vec_at,6)
  sc1_winmargin_vec_at  <- as.numeric(sc1_winmargin_vec_at)

  sc1_at_totalwinmargin <- sum(sc1_winmargin_vec_at)
  sc1_at_no_of_winmargin_ov0 <- length(which(sc1_winmargin_vec_at >= 0))
  sc1_at_no_of_winmargin_ov1 <- length(which(sc1_winmargin_vec_at >= 1))
  sc1_at_no_of_winmargin_un0 <- length(which(sc1_winmargin_vec_at <= 0))
  sc1_at_no_of_winmargin_un1 <- length(which(sc1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  sc1_ht_last6points <- sc1_ht_numberof_wins*3 + sc1_ht_numberof_draws*1
  sc1_at_last6points <- sc1_at_numberof_wins*3 + sc1_at_numberof_draws*1

  ifelse(sc1_ht_last6points > sc1_at_last6points && sc1_ht_totalwinmargin > sc1_at_totalwinmargin,sc1_3waypick <- "1",sc1_3waypick <- "X2")

  ifelse(sc1_at_last6points > sc1_ht_last6points && sc1_at_totalwinmargin > sc1_ht_totalwinmargin,sc1_3waypick <- "2",sc1_3waypick <- "1X")


  if(sc1_ht_no_of_ov25 + sc1_at_no_of_ov25 >= 6) {sc1_goalspick <- "ov25"} else {sc1_goalspick <- "un25"}

  if(sc1_ht_no_of_un25 + sc1_at_no_of_un25 >= 6) {sc1_goalspick <- "un25"} else {sc1_goalspick <- "ov25"}

  sc1_prediction[sc1_row] <- rbind(paste(sc1_3waypick,sc1_goalspick,sep = ","))

}

sc1_prediction <- as.data.frame(sc1_prediction)
colnames(sc1_prediction) <- "prediction"
sc1_prediction

sc1_picks <- cbind(SC1_fixtures$Div,SC1_fixtures$HomeTeam_sc1,SC1_fixtures$AwayTeam_sc1,sc1_prediction)
colnames(sc1_picks)[1] <- "picks_Div"
colnames(sc1_picks)[2] <- "picks_HomeTeam"
colnames(sc1_picks)[3] <- "picks_AwayTeam"
sc1_picks$matchid <- paste(sc1_picks$picks_HomeTeam,sc1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SC1
#SC2
SC2_fixtures$Hometeam_sc2_index <- match(SC2_fixtures$HomeTeam_sc2,sc2_teams)
SC2_fixtures$Awayteam_sc2_index <- match(SC2_fixtures$AwayTeam_sc2,sc2_teams)
sc2_prediction <- c()
for(sc2_row in 1:nrow(SC2_fixtures))
{

  sc2_hometeamindex <- SC2_fixtures[sc2_row,"Hometeam_sc2_index"]
  sc2_awayteamindex <- SC2_fixtures[sc2_row,"Awayteam_sc2_index"]
  #analyse team form
  #home team
  sc2_form_vec_ht <- as.vector(sc2_form_h[sc2_hometeamindex,])
  sc2_form_vec_ht[is.na(sc2_form_vec_ht)] <- ""
  sc2_form_vec_ht <- sc2_form_vec_ht[sc2_form_vec_ht != ""]
  sc2_form_vec_ht  <-tail(sc2_form_vec_ht,6)
  sc2_ht_numberof_wins <- length(which(sc2_form_vec_ht == "W"))
  sc2_ht_numberof_draws <- length(which(sc2_form_vec_ht == "D"))
  sc2_ht_numberof_loss <- length(which(sc2_form_vec_ht == "L"))
  #awayteam
  sc2_form_vec_at <- as.vector(sc2_form_h[sc2_awayteamindex,])
  sc2_form_vec_at[is.na(sc2_form_vec_at)] <- ""
  sc2_form_vec_at <- sc2_form_vec_at[sc2_form_vec_at != ""]
  sc2_form_vec_at  <-tail(sc2_form_vec_at,6)
  sc2_at_numberof_wins <- length(which(sc2_form_vec_at == "W"))
  sc2_at_numberof_draws <- length(which(sc2_form_vec_at == "D"))
  sc2_at_numberof_loss <- length(which(sc2_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  sc2_goalscored_vec_ht <- as.vector(sc2_goalscored_h[sc2_hometeamindex,])
  sc2_goalscored_vec_ht[is.na(sc2_goalscored_vec_ht)] <- ""
  sc2_goalscored_vec_ht <- sc2_goalscored_vec_ht[sc2_goalscored_vec_ht != ""]
  sc2_goalscored_vec_ht  <-tail(sc2_goalscored_vec_ht,6)
  sc2_goalscored_vec_ht  <- as.numeric(sc2_goalscored_vec_ht)
  sc2_ht_totalgoalscored <- sum(sc2_goalscored_vec_ht)
  sc2_ht_matches_scoring <- length(which(sc2_goalscored_vec_ht > 0))
  sc2_ht_matches_without_scoring <- length(which(sc2_goalscored_vec_ht == "0"))
  #awayteam
  sc2_goalscored_vec_at <- as.vector(sc2_goalscored_h[sc2_awayteamindex,])
  sc2_goalscored_vec_at[is.na(sc2_goalscored_vec_at)] <- ""
  sc2_goalscored_vec_at <- sc2_goalscored_vec_at[sc2_goalscored_vec_at != ""]
  sc2_goalscored_vec_at  <-tail(sc2_goalscored_vec_at,6)
  sc2_goalscored_vec_at  <- as.numeric(sc2_goalscored_vec_at)
  sc2_at_totalgoalscored <- sum(sc2_goalscored_vec_at)
  sc2_at_matches_scoring <- length(which(sc2_goalscored_vec_at > 0))
  sc2_at_matches_without_scoring <- length(which(sc2_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  sc2_goalconceded_vec_ht <- as.vector(sc2_goalconceded_h[sc2_hometeamindex,])
  sc2_goalconceded_vec_ht[is.na(sc2_goalconceded_vec_ht)] <- ""
  sc2_goalconceded_vec_ht <- sc2_goalconceded_vec_ht[sc2_goalconceded_vec_ht != ""]
  sc2_goalconceded_vec_ht  <-tail(sc2_goalconceded_vec_ht,6)
  sc2_goalconceded_vec_ht  <- as.numeric(sc2_goalconceded_vec_ht)
  sc2_goalconceded_vec_ht
  sc2_ht_totalgoalconceded <- sum(sc2_goalconceded_vec_ht)
  sc2_ht_matches_concede <- length(which(sc2_goalconceded_vec_ht > 0))
  sc2_ht_matches_without_concede <- length(which(sc2_goalconceded_vec_ht == "0"))
  #awayteam
  sc2_goalconceded_vec_at <- as.vector(sc2_goalconceded_h[sc2_awayteamindex,])
  sc2_goalconceded_vec_at[is.na(sc2_goalconceded_vec_at)] <- ""
  sc2_goalconceded_vec_at <- sc2_goalconceded_vec_at[sc2_goalconceded_vec_at != ""]
  sc2_goalconceded_vec_at  <-tail(sc2_goalconceded_vec_at,6)
  sc2_goalconceded_vec_at  <- as.numeric(sc2_goalconceded_vec_at)
  sc2_at_totalgoalconceded <- sum(sc2_goalconceded_vec_at)
  sc2_at_matches_concede <- length(which(sc2_goalconceded_vec_at > 0))
  sc2_at_matches_without_concede <- length(which(sc2_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  sc2_totalgoals_vec_ht <- as.vector(sc2_totalgoals_h[sc2_hometeamindex,])
  sc2_totalgoals_vec_ht[is.na(sc2_totalgoals_vec_ht)] <- ""
  sc2_totalgoals_vec_ht <- sc2_totalgoals_vec_ht[sc2_totalgoals_vec_ht != ""]
  sc2_totalgoals_vec_ht  <-tail(sc2_totalgoals_vec_ht,6)
  sc2_totalgoals_vec_ht  <- as.numeric(sc2_totalgoals_vec_ht)
  sc2_totalgoals_vec_ht
  sc2_ht_totalgoals <- sum(sc2_totalgoals_vec_ht)
  sc2_ht_avgtotalgoals <- (sc2_ht_totalgoals/6)
  sc2_ht_no_of_ov25 <- length(which(sc2_totalgoals_vec_ht >= 3))
  sc2_ht_no_of_un25 <- length(which(sc2_totalgoals_vec_ht <= 2))
  #awayteam
  sc2_totalgoals_vec_at <- as.vector(sc2_totalgoals_h[sc2_awayteamindex,])
  sc2_totalgoals_vec_at[is.na(sc2_totalgoals_vec_at)] <- ""
  sc2_totalgoals_vec_at <- sc2_totalgoals_vec_at[sc2_totalgoals_vec_at != ""]
  sc2_totalgoals_vec_at  <-tail(sc2_totalgoals_vec_at,6)
  sc2_totalgoals_vec_at  <- as.numeric(sc2_totalgoals_vec_at)
  sc2_totalgoals_vec_at
  sc2_at_totalgoals <- sum(sc2_totalgoals_vec_at)
  sc2_at_avgtotalgoals <- (sc2_at_totalgoals/6)
  sc2_at_no_of_ov25 <- length(which(sc2_totalgoals_vec_at >= 3))
  sc2_at_no_of_un25 <- length(which(sc2_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  sc2_winmargin_vec_ht <- as.vector(sc2_winmargin_h[sc2_hometeamindex,])
  sc2_winmargin_vec_ht[is.na(sc2_winmargin_vec_ht)] <- ""
  sc2_winmargin_vec_ht <- sc2_winmargin_vec_ht[sc2_winmargin_vec_ht != ""]
  sc2_winmargin_vec_ht  <-tail(sc2_winmargin_vec_ht,6)
  sc2_winmargin_vec_ht  <- as.numeric(sc2_winmargin_vec_ht)

  sc2_ht_totalwinmargin <- sum(sc2_winmargin_vec_ht)
  sc2_ht_no_of_winmargin_ov0 <- length(which(sc2_winmargin_vec_ht >= 0))
  sc2_ht_no_of_winmargin_ov1 <- length(which(sc2_winmargin_vec_ht >= 1))
  sc2_ht_no_of_winmargin_un0 <- length(which(sc2_winmargin_vec_ht <= 0))
  sc2_ht_no_of_winmargin_un1 <- length(which(sc2_winmargin_vec_ht <= 1))
  #awayteam
  sc2_winmargin_vec_at <- as.vector(sc2_winmargin_h[sc2_awayteamindex,])
  sc2_winmargin_vec_at[is.na(sc2_winmargin_vec_at)] <- ""
  sc2_winmargin_vec_at <- sc2_winmargin_vec_at[sc2_winmargin_vec_at != ""]
  sc2_winmargin_vec_at  <-tail(sc2_winmargin_vec_at,6)
  sc2_winmargin_vec_at  <- as.numeric(sc2_winmargin_vec_at)

  sc2_at_totalwinmargin <- sum(sc2_winmargin_vec_at)
  sc2_at_no_of_winmargin_ov0 <- length(which(sc2_winmargin_vec_at >= 0))
  sc2_at_no_of_winmargin_ov1 <- length(which(sc2_winmargin_vec_at >= 1))
  sc2_at_no_of_winmargin_un0 <- length(which(sc2_winmargin_vec_at <= 0))
  sc2_at_no_of_winmargin_un1 <- length(which(sc2_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  sc2_ht_last6points <- sc2_ht_numberof_wins*3 + sc2_ht_numberof_draws*1
  sc2_at_last6points <- sc2_at_numberof_wins*3 + sc2_at_numberof_draws*1

  ifelse(sc2_ht_last6points > sc2_at_last6points && sc2_ht_totalwinmargin > sc2_at_totalwinmargin,sc2_3waypick <- "1",sc2_3waypick <- "X2")

  ifelse(sc2_at_last6points > sc2_ht_last6points && sc2_at_totalwinmargin > sc2_ht_totalwinmargin,sc2_3waypick <- "2",sc2_3waypick <- "1X")

  if(sc2_ht_no_of_ov25 + sc2_at_no_of_ov25 >= 6) {sc2_goalspick <- "ov25"} else {sc2_goalspick <- "un25"}

  if(sc2_ht_no_of_un25 + sc2_at_no_of_un25 >= 6) {sc2_goalspick <- "un25"} else {sc2_goalspick <- "ov25"}

  sc2_prediction[sc2_row] <- rbind(paste(sc2_3waypick,sc2_goalspick,sep = ","))

}

sc2_prediction <- as.data.frame(sc2_prediction)
colnames(sc2_prediction) <- "prediction"
sc2_prediction

sc2_picks <- cbind(SC2_fixtures$Div,SC2_fixtures$HomeTeam_sc2,SC2_fixtures$AwayTeam_sc2,sc2_prediction)
colnames(sc2_picks)[1] <- "picks_Div"
colnames(sc2_picks)[2] <- "picks_HomeTeam"
colnames(sc2_picks)[3] <- "picks_AwayTeam"
sc2_picks$matchid <- paste(sc2_picks$picks_HomeTeam,sc2_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SC2
#SC3
SC3_fixtures$Hometeam_sc3_index <- match(SC3_fixtures$HomeTeam_sc3,sc3_teams)
SC3_fixtures$Awayteam_sc3_index <- match(SC3_fixtures$AwayTeam_sc3,sc3_teams)
sc3_prediction <- c()
for(sc3_row in 1:nrow(SC3_fixtures))
{

  sc3_hometeamindex <- SC3_fixtures[sc3_row,"Hometeam_sc3_index"]
  sc3_awayteamindex <- SC3_fixtures[sc3_row,"Awayteam_sc3_index"]
  #analyse team form
  #home team
  sc3_form_vec_ht <- as.vector(sc3_form_h[sc3_hometeamindex,])
  sc3_form_vec_ht[is.na(sc3_form_vec_ht)] <- ""
  sc3_form_vec_ht <- sc3_form_vec_ht[sc3_form_vec_ht != ""]
  sc3_form_vec_ht  <-tail(sc3_form_vec_ht,6)
  sc3_ht_numberof_wins <- length(which(sc3_form_vec_ht == "W"))
  sc3_ht_numberof_draws <- length(which(sc3_form_vec_ht == "D"))
  sc3_ht_numberof_loss <- length(which(sc3_form_vec_ht == "L"))
  #awayteam
  sc3_form_vec_at <- as.vector(sc3_form_h[sc3_awayteamindex,])
  sc3_form_vec_at[is.na(sc3_form_vec_at)] <- ""
  sc3_form_vec_at <- sc3_form_vec_at[sc3_form_vec_at != ""]
  sc3_form_vec_at  <-tail(sc3_form_vec_at,6)
  sc3_at_numberof_wins <- length(which(sc3_form_vec_at == "W"))
  sc3_at_numberof_draws <- length(which(sc3_form_vec_at == "D"))
  sc3_at_numberof_loss <- length(which(sc3_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  sc3_goalscored_vec_ht <- as.vector(sc3_goalscored_h[sc3_hometeamindex,])
  sc3_goalscored_vec_ht[is.na(sc3_goalscored_vec_ht)] <- ""
  sc3_goalscored_vec_ht <- sc3_goalscored_vec_ht[sc3_goalscored_vec_ht != ""]
  sc3_goalscored_vec_ht  <-tail(sc3_goalscored_vec_ht,6)
  sc3_goalscored_vec_ht  <- as.numeric(sc3_goalscored_vec_ht)
  sc3_ht_totalgoalscored <- sum(sc3_goalscored_vec_ht)
  sc3_ht_matches_scoring <- length(which(sc3_goalscored_vec_ht > 0))
  sc3_ht_matches_without_scoring <- length(which(sc3_goalscored_vec_ht == "0"))
  #awayteam
  sc3_goalscored_vec_at <- as.vector(sc3_goalscored_h[sc3_awayteamindex,])
  sc3_goalscored_vec_at[is.na(sc3_goalscored_vec_at)] <- ""
  sc3_goalscored_vec_at <- sc3_goalscored_vec_at[sc3_goalscored_vec_at != ""]
  sc3_goalscored_vec_at  <-tail(sc3_goalscored_vec_at,6)
  sc3_goalscored_vec_at  <- as.numeric(sc3_goalscored_vec_at)
  sc3_at_totalgoalscored <- sum(sc3_goalscored_vec_at)
  sc3_at_matches_scoring <- length(which(sc3_goalscored_vec_at > 0))
  sc3_at_matches_without_scoring <- length(which(sc3_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  sc3_goalconceded_vec_ht <- as.vector(sc3_goalconceded_h[sc3_hometeamindex,])
  sc3_goalconceded_vec_ht[is.na(sc3_goalconceded_vec_ht)] <- ""
  sc3_goalconceded_vec_ht <- sc3_goalconceded_vec_ht[sc3_goalconceded_vec_ht != ""]
  sc3_goalconceded_vec_ht  <-tail(sc3_goalconceded_vec_ht,6)
  sc3_goalconceded_vec_ht  <- as.numeric(sc3_goalconceded_vec_ht)
  sc3_goalconceded_vec_ht
  sc3_ht_totalgoalconceded <- sum(sc3_goalconceded_vec_ht)
  sc3_ht_matches_concede <- length(which(sc3_goalconceded_vec_ht > 0))
  sc3_ht_matches_without_concede <- length(which(sc3_goalconceded_vec_ht == "0"))
  #awayteam
  sc3_goalconceded_vec_at <- as.vector(sc3_goalconceded_h[sc3_awayteamindex,])
  sc3_goalconceded_vec_at[is.na(sc3_goalconceded_vec_at)] <- ""
  sc3_goalconceded_vec_at <- sc3_goalconceded_vec_at[sc3_goalconceded_vec_at != ""]
  sc3_goalconceded_vec_at  <-tail(sc3_goalconceded_vec_at,6)
  sc3_goalconceded_vec_at  <- as.numeric(sc3_goalconceded_vec_at)
  sc3_at_totalgoalconceded <- sum(sc3_goalconceded_vec_at)
  sc3_at_matches_concede <- length(which(sc3_goalconceded_vec_at > 0))
  sc3_at_matches_without_concede <- length(which(sc3_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  sc3_totalgoals_vec_ht <- as.vector(sc3_totalgoals_h[sc3_hometeamindex,])
  sc3_totalgoals_vec_ht[is.na(sc3_totalgoals_vec_ht)] <- ""
  sc3_totalgoals_vec_ht <- sc3_totalgoals_vec_ht[sc3_totalgoals_vec_ht != ""]
  sc3_totalgoals_vec_ht  <-tail(sc3_totalgoals_vec_ht,6)
  sc3_totalgoals_vec_ht  <- as.numeric(sc3_totalgoals_vec_ht)
  sc3_totalgoals_vec_ht
  sc3_ht_totalgoals <- sum(sc3_totalgoals_vec_ht)
  sc3_ht_avgtotalgoals <- (sc3_ht_totalgoals/6)
  sc3_ht_no_of_ov25 <- length(which(sc3_totalgoals_vec_ht >= 3))
  sc3_ht_no_of_un25 <- length(which(sc3_totalgoals_vec_ht <= 2))
  #awayteam
  sc3_totalgoals_vec_at <- as.vector(sc3_totalgoals_h[sc3_awayteamindex,])
  sc3_totalgoals_vec_at[is.na(sc3_totalgoals_vec_at)] <- ""
  sc3_totalgoals_vec_at <- sc3_totalgoals_vec_at[sc3_totalgoals_vec_at != ""]
  sc3_totalgoals_vec_at  <-tail(sc3_totalgoals_vec_at,6)
  sc3_totalgoals_vec_at  <- as.numeric(sc3_totalgoals_vec_at)
  sc3_totalgoals_vec_at
  sc3_at_totalgoals <- sum(sc3_totalgoals_vec_at)
  sc3_at_avgtotalgoals <- (sc3_at_totalgoals/6)
  sc3_at_no_of_ov25 <- length(which(sc3_totalgoals_vec_at >= 3))
  sc3_at_no_of_un25 <- length(which(sc3_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  sc3_winmargin_vec_ht <- as.vector(sc3_winmargin_h[sc3_hometeamindex,])
  sc3_winmargin_vec_ht[is.na(sc3_winmargin_vec_ht)] <- ""
  sc3_winmargin_vec_ht <- sc3_winmargin_vec_ht[sc3_winmargin_vec_ht != ""]
  sc3_winmargin_vec_ht  <-tail(sc3_winmargin_vec_ht,6)
  sc3_winmargin_vec_ht  <- as.numeric(sc3_winmargin_vec_ht)

  sc3_ht_totalwinmargin <- sum(sc3_winmargin_vec_ht)
  sc3_ht_no_of_winmargin_ov0 <- length(which(sc3_winmargin_vec_ht >= 0))
  sc3_ht_no_of_winmargin_ov1 <- length(which(sc3_winmargin_vec_ht >= 1))
  sc3_ht_no_of_winmargin_un0 <- length(which(sc3_winmargin_vec_ht <= 0))
  sc3_ht_no_of_winmargin_un1 <- length(which(sc3_winmargin_vec_ht <= 1))
  #awayteam
  sc3_winmargin_vec_at <- as.vector(sc3_winmargin_h[sc3_awayteamindex,])
  sc3_winmargin_vec_at[is.na(sc3_winmargin_vec_at)] <- ""
  sc3_winmargin_vec_at <- sc3_winmargin_vec_at[sc3_winmargin_vec_at != ""]
  sc3_winmargin_vec_at  <-tail(sc3_winmargin_vec_at,6)
  sc3_winmargin_vec_at  <- as.numeric(sc3_winmargin_vec_at)

  sc3_at_totalwinmargin <- sum(sc3_winmargin_vec_at)
  sc3_at_no_of_winmargin_ov0 <- length(which(sc3_winmargin_vec_at >= 0))
  sc3_at_no_of_winmargin_ov1 <- length(which(sc3_winmargin_vec_at >= 1))
  sc3_at_no_of_winmargin_un0 <- length(which(sc3_winmargin_vec_at <= 0))
  sc3_at_no_of_winmargin_un1 <- length(which(sc3_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  sc3_ht_last6points <- sc3_ht_numberof_wins*3 + sc3_ht_numberof_draws*1
  sc3_at_last6points <- sc3_at_numberof_wins*3 + sc3_at_numberof_draws*1

  ifelse(sc3_ht_last6points > sc3_at_last6points && sc3_ht_totalwinmargin > sc3_at_totalwinmargin,sc3_3waypick <- "1",sc3_3waypick <- "X2")

  ifelse(sc3_at_last6points > sc3_ht_last6points && sc3_at_totalwinmargin > sc3_ht_totalwinmargin,sc3_3waypick <- "2",sc3_3waypick <- "1X")


  if(sc3_ht_no_of_ov25 + sc3_at_no_of_ov25 >= 6) {sc3_goalspick <- "ov25"} else {sc3_goalspick <- "un25"}

  if(sc3_ht_no_of_un25 + sc3_at_no_of_un25 >= 6) {sc3_goalspick <- "un25"} else {sc3_goalspick <- "ov25"}

  sc3_prediction[sc3_row] <- rbind(paste(sc3_3waypick,sc3_goalspick,sep = ","))

}

sc3_prediction <- as.data.frame(sc3_prediction)
colnames(sc3_prediction) <- "prediction"
sc3_prediction

sc3_picks <- cbind(SC3_fixtures$Div,SC3_fixtures$HomeTeam_sc3,SC3_fixtures$AwayTeam_sc3,sc3_prediction)
colnames(sc3_picks)[1] <- "picks_Div"
colnames(sc3_picks)[2] <- "picks_HomeTeam"
colnames(sc3_picks)[3] <- "picks_AwayTeam"
sc3_picks$matchid <- paste(sc3_picks$picks_HomeTeam,sc3_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SC3
#T1
T1_fixtures$Hometeam_t1_index <- match(T1_fixtures$HomeTeam_t1,t1_teams)
T1_fixtures$Awayteam_t1_index <- match(T1_fixtures$AwayTeam_t1,t1_teams)
t1_prediction <- c()
for(t1_row in 1:nrow(T1_fixtures))
{

  t1_hometeamindex <- T1_fixtures[t1_row,"Hometeam_t1_index"]
  t1_awayteamindex <- T1_fixtures[t1_row,"Awayteam_t1_index"]
  #analyse team form
  #home team
  t1_form_vec_ht <- as.vector(t1_form_h[t1_hometeamindex,])
  t1_form_vec_ht[is.na(t1_form_vec_ht)] <- ""
  t1_form_vec_ht <- t1_form_vec_ht[t1_form_vec_ht != ""]
  t1_form_vec_ht  <-tail(t1_form_vec_ht,6)
  t1_ht_numberof_wins <- length(which(t1_form_vec_ht == "W"))
  t1_ht_numberof_draws <- length(which(t1_form_vec_ht == "D"))
  t1_ht_numberof_loss <- length(which(t1_form_vec_ht == "L"))
  #awayteam
  t1_form_vec_at <- as.vector(t1_form_h[t1_awayteamindex,])
  t1_form_vec_at[is.na(t1_form_vec_at)] <- ""
  t1_form_vec_at <- t1_form_vec_at[t1_form_vec_at != ""]
  t1_form_vec_at  <-tail(t1_form_vec_at,6)
  t1_at_numberof_wins <- length(which(t1_form_vec_at == "W"))
  t1_at_numberof_draws <- length(which(t1_form_vec_at == "D"))
  t1_at_numberof_loss <- length(which(t1_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  t1_goalscored_vec_ht <- as.vector(t1_goalscored_h[t1_hometeamindex,])
  t1_goalscored_vec_ht[is.na(t1_goalscored_vec_ht)] <- ""
  t1_goalscored_vec_ht <- t1_goalscored_vec_ht[t1_goalscored_vec_ht != ""]
  t1_goalscored_vec_ht  <-tail(t1_goalscored_vec_ht,6)
  t1_goalscored_vec_ht  <- as.numeric(t1_goalscored_vec_ht)
  t1_ht_totalgoalscored <- sum(t1_goalscored_vec_ht)
  t1_ht_matches_scoring <- length(which(t1_goalscored_vec_ht > 0))
  t1_ht_matches_without_scoring <- length(which(t1_goalscored_vec_ht == "0"))
  #awayteam
  t1_goalscored_vec_at <- as.vector(t1_goalscored_h[t1_awayteamindex,])
  t1_goalscored_vec_at[is.na(t1_goalscored_vec_at)] <- ""
  t1_goalscored_vec_at <- t1_goalscored_vec_at[t1_goalscored_vec_at != ""]
  t1_goalscored_vec_at  <-tail(t1_goalscored_vec_at,6)
  t1_goalscored_vec_at  <- as.numeric(t1_goalscored_vec_at)
  t1_at_totalgoalscored <- sum(t1_goalscored_vec_at)
  t1_at_matches_scoring <- length(which(t1_goalscored_vec_at > 0))
  t1_at_matches_without_scoring <- length(which(t1_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  t1_goalconceded_vec_ht <- as.vector(t1_goalconceded_h[t1_hometeamindex,])
  t1_goalconceded_vec_ht[is.na(t1_goalconceded_vec_ht)] <- ""
  t1_goalconceded_vec_ht <- t1_goalconceded_vec_ht[t1_goalconceded_vec_ht != ""]
  t1_goalconceded_vec_ht  <-tail(t1_goalconceded_vec_ht,6)
  t1_goalconceded_vec_ht  <- as.numeric(t1_goalconceded_vec_ht)
  t1_goalconceded_vec_ht
  t1_ht_totalgoalconceded <- sum(t1_goalconceded_vec_ht)
  t1_ht_matches_concede <- length(which(t1_goalconceded_vec_ht > 0))
  t1_ht_matches_without_concede <- length(which(t1_goalconceded_vec_ht == "0"))
  #awayteam
  t1_goalconceded_vec_at <- as.vector(t1_goalconceded_h[t1_awayteamindex,])
  t1_goalconceded_vec_at[is.na(t1_goalconceded_vec_at)] <- ""
  t1_goalconceded_vec_at <- t1_goalconceded_vec_at[t1_goalconceded_vec_at != ""]
  t1_goalconceded_vec_at  <-tail(t1_goalconceded_vec_at,6)
  t1_goalconceded_vec_at  <- as.numeric(t1_goalconceded_vec_at)
  t1_at_totalgoalconceded <- sum(t1_goalconceded_vec_at)
  t1_at_matches_concede <- length(which(t1_goalconceded_vec_at > 0))
  t1_at_matches_without_concede <- length(which(t1_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  t1_totalgoals_vec_ht <- as.vector(t1_totalgoals_h[t1_hometeamindex,])
  t1_totalgoals_vec_ht[is.na(t1_totalgoals_vec_ht)] <- ""
  t1_totalgoals_vec_ht <- t1_totalgoals_vec_ht[t1_totalgoals_vec_ht != ""]
  t1_totalgoals_vec_ht  <-tail(t1_totalgoals_vec_ht,6)
  t1_totalgoals_vec_ht  <- as.numeric(t1_totalgoals_vec_ht)
  t1_totalgoals_vec_ht
  t1_ht_totalgoals <- sum(t1_totalgoals_vec_ht)
  t1_ht_avgtotalgoals <- (t1_ht_totalgoals/6)
  t1_ht_no_of_ov25 <- length(which(t1_totalgoals_vec_ht >= 3))
  t1_ht_no_of_un25 <- length(which(t1_totalgoals_vec_ht <= 2))
  #awayteam
  t1_totalgoals_vec_at <- as.vector(t1_totalgoals_h[t1_awayteamindex,])
  t1_totalgoals_vec_at[is.na(t1_totalgoals_vec_at)] <- ""
  t1_totalgoals_vec_at <- t1_totalgoals_vec_at[t1_totalgoals_vec_at != ""]
  t1_totalgoals_vec_at  <-tail(t1_totalgoals_vec_at,6)
  t1_totalgoals_vec_at  <- as.numeric(t1_totalgoals_vec_at)
  t1_totalgoals_vec_at
  t1_at_totalgoals <- sum(t1_totalgoals_vec_at)
  t1_at_avgtotalgoals <- (t1_at_totalgoals/6)
  t1_at_no_of_ov25 <- length(which(t1_totalgoals_vec_at >= 3))
  t1_at_no_of_un25 <- length(which(t1_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  t1_winmargin_vec_ht <- as.vector(t1_winmargin_h[t1_hometeamindex,])
  t1_winmargin_vec_ht[is.na(t1_winmargin_vec_ht)] <- ""
  t1_winmargin_vec_ht <- t1_winmargin_vec_ht[t1_winmargin_vec_ht != ""]
  t1_winmargin_vec_ht  <-tail(t1_winmargin_vec_ht,6)
  t1_winmargin_vec_ht  <- as.numeric(t1_winmargin_vec_ht)

  t1_ht_totalwinmargin <- sum(t1_winmargin_vec_ht)
  t1_ht_no_of_winmargin_ov0 <- length(which(t1_winmargin_vec_ht >= 0))
  t1_ht_no_of_winmargin_ov1 <- length(which(t1_winmargin_vec_ht >= 1))
  t1_ht_no_of_winmargin_un0 <- length(which(t1_winmargin_vec_ht <= 0))
  t1_ht_no_of_winmargin_un1 <- length(which(t1_winmargin_vec_ht <= 1))
  #awayteam
  t1_winmargin_vec_at <- as.vector(t1_winmargin_h[t1_awayteamindex,])
  t1_winmargin_vec_at[is.na(t1_winmargin_vec_at)] <- ""
  t1_winmargin_vec_at <- t1_winmargin_vec_at[t1_winmargin_vec_at != ""]
  t1_winmargin_vec_at  <-tail(t1_winmargin_vec_at,6)
  t1_winmargin_vec_at  <- as.numeric(t1_winmargin_vec_at)

  t1_at_totalwinmargin <- sum(t1_winmargin_vec_at)
  t1_at_no_of_winmargin_ov0 <- length(which(t1_winmargin_vec_at >= 0))
  t1_at_no_of_winmargin_ov1 <- length(which(t1_winmargin_vec_at >= 1))
  t1_at_no_of_winmargin_un0 <- length(which(t1_winmargin_vec_at <= 0))
  t1_at_no_of_winmargin_un1 <- length(which(t1_winmargin_vec_at <= 1))
  ##################################################################################
  ##########################################
  ####we need to decide ############
  #winner goals
  t1_ht_last6points <- t1_ht_numberof_wins*3 + t1_ht_numberof_draws*1
  t1_at_last6points <- t1_at_numberof_wins*3 + t1_at_numberof_draws*1

  ifelse(t1_ht_last6points > t1_at_last6points && t1_ht_totalwinmargin > t1_at_totalwinmargin,t1_3waypick <- "1",t1_3waypick <- "X2")

  ifelse(t1_at_last6points > t1_ht_last6points && t1_at_totalwinmargin > t1_ht_totalwinmargin,t1_3waypick <- "2",t1_3waypick <- "1X")

  if(t1_ht_no_of_ov25 + t1_at_no_of_ov25 >= 6) {t1_goalspick <- "ov25"} else {t1_goalspick <- "un25"}

  if(t1_ht_no_of_un25 + t1_at_no_of_un25 >= 6) {t1_goalspick <- "un25"} else {t1_goalspick <- "ov25"}

  t1_prediction[t1_row] <- rbind(paste(t1_3waypick,t1_goalspick,sep = ","))

}

t1_prediction <- as.data.frame(t1_prediction)
colnames(t1_prediction) <- "prediction"
t1_prediction

t1_picks <- cbind(T1_fixtures$Div,T1_fixtures$HomeTeam_t1,T1_fixtures$AwayTeam_t1,t1_prediction)
colnames(t1_picks)[1] <- "picks_Div"
colnames(t1_picks)[2] <- "picks_HomeTeam"
colnames(t1_picks)[3] <- "picks_AwayTeam"
t1_picks$matchid <- paste(t1_picks$picks_HomeTeam,t1_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of T1
############################################################################################
#combine picks from divisions
allteams20212022picks <- rbind(b1_picks,d1_picks,d2_picks,e0_picks,e1_picks,e2_picks,e3_picks,ec_picks,f1_picks,f2_picks,g1_picks,i1_picks,i2_picks,n1_picks,p1_picks,sp1_picks,sp2_picks,sc0_picks,sc1_picks,sc2_picks,sc3_picks,t1_picks)
#join the data
#######myodds file##########
myodds_fixtures <- readxl::read_excel('../FDAS/myodds_20212022.xlsx', sheet = '3way')
myodds_fixtures$matchid <- paste(myodds_fixtures$HT,myodds_fixtures$AT, sep = "-")
myodds_fixtures_prediction <- dplyr::left_join(myodds_fixtures,allteams20212022picks)
write.csv(myodds_fixtures_prediction,'myodds_fixtures_prediction.csv')
############################
picks_fixtures <- read.csv('myfixtures.csv')
picks_fixtures$matchid <- paste(picks_fixtures$Home_Team,picks_fixtures$Away_Team, sep = "-")
picks_fixtures_prediction <- dplyr::left_join(picks_fixtures,allteams20212022picks)
write.csv(picks_fixtures_prediction,'picks_fixtures_prediction.csv')
###########################
#reset allteams20212022picks
rm(allteams20212022picks)



