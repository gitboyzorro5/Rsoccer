library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')

################################################################
#B1
# b1_hometeamindex <- match(c("Anderlecht"),b1_teams)
# b1_awayteamindex <- match(c("Genk"),b1_teams)
b1_teams_indexes <- cbind(b1_teams[],1:length(b1_teams))
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
if(b1_ht_last6points > b1_at_last6points & b1_ht_totalwinmargin > b1_at_totalwinmargin)
{
  b1_3waypick <- "home win"
}

if(b1_at_last6points > b1_ht_last6points & b1_at_totalwinmargin > b1_ht_totalwinmargin)
{
  b1_3waypick <- "away win"
}

if(b1_ht_avgtotalgoals >=2.3 & b1_at_avgtotalgoals >= 2.3 & b1_ht_no_of_ov25 + b1_at_no_of_ov25 >= 8)
{
  b1_goalspick <- "ov25"
}


if(b1_ht_avgtotalgoals <=2.3 & b1_at_avgtotalgoals <= 2.3 & b1_ht_no_of_ov25 + b1_at_no_of_ov25 <= 8)
{
  b1_goalspick <- "un25"
}

b1_prediction[b1_row] <- rbind(paste(b1_3waypick,b1_goalspick,sep = ","))

}

b1_prediction <- as.data.frame(b1_prediction)
colnames(b1_prediction) <- "prediction"
b1_prediction

b1_picks <- cbind(B1_fixtures$Div,B1_fixtures$HomeTeam_b1,B1_fixtures$AwayTeam_b1,b1_prediction)
colnames(b1_picks)[1] <- "b1picks_Div"
colnames(b1_picks)[2] <- "b1picks_HomeTeam"
colnames(b1_picks)[3] <- "b1picks_AwayTeam"
b1_picks$matchid <- paste(b1_picks$b1picks_HomeTeam,b1_picks$b1picks_AwayTeam,sep = "-")
b1_picks
