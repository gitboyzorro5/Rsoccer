#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
#
# first_df <- E1_rounds[E1_rounds$e1_matchday > 28,]
# second_df <- E0_rounds[E0_rounds$e0_matchday > 26,]
# # # third_df <- I1_rounds[I1_rounds$i1_matchday > 29,]
# first_df <- first_df[,-37]
# second_df <- second_df[,-37]
# # #third_df <- third_df[,-37]
# # #
# BRAZILSERIEA <- rbind(first_df,second_df)

BRAZILSERIEA <-BRA_rounds[BRA_rounds$bra_matchday > 6,]

#goaltotals v2
brazilseriea_goaltotalsv2 <- tapply(BRAZILSERIEA$TG, BRAZILSERIEA[c("Home", "Away")],mean)
brazilseriea_hgtotals <- rowSums(brazilseriea_goaltotalsv2, na.rm = T)
brazilseriea_agtotals <- colSums(brazilseriea_goaltotalsv2, na.rm = T)
brazilseriea_goaltotalsv2 <- cbind(brazilseriea_goaltotalsv2,brazilseriea_hgtotals,brazilseriea_agtotals)
brazilseriea_totalgoals <- brazilseriea_hgtotals + brazilseriea_agtotals
brazilseriea_goaltotalsv2 <- cbind(brazilseriea_goaltotalsv2,brazilseriea_totalgoals)
brazilseriea_teams <- sort(unique(BRAZILSERIEA$Home))
brazilseriea_home_games <- c()
brazilseriea_away_games <-c()
for (i_brazilseriea in 1:length(brazilseriea_teams))
{

  brazilseriea_home_games[i_brazilseriea] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea],])
  brazilseriea_away_games[i_brazilseriea]  <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea],])

}
brazilseriea_games_played <- brazilseriea_home_games + brazilseriea_away_games
brazilseriea_goaltotalsv2 <- cbind(brazilseriea_goaltotalsv2,brazilseriea_games_played)
brazilseriea_avg_totalgoals <- round((brazilseriea_totalgoals/ brazilseriea_games_played), digits = 4)
brazilseriea_goaltotalsv2[is.na(brazilseriea_goaltotalsv2)] <- ""
brazilseriea_goaltotalsv2 <- cbind(brazilseriea_goaltotalsv2,brazilseriea_avg_totalgoals)

############################################################################################################
#Cornertotals v2
############################################################################################################
#GS matrix
brazilseriea_goalscored_h <- tapply(BRAZILSERIEA$HG, BRAZILSERIEA[c("Home", "Date")],mean)
brazilseriea_goalscored_a <- tapply(BRAZILSERIEA$AG, BRAZILSERIEA[c("Away", "Date")],mean)
brazilseriea_goalscored_h[is.na(brazilseriea_goalscored_h)] <- ""
brazilseriea_goalscored_a[is.na(brazilseriea_goalscored_a)] <- ""
for(brazilseriea_rowhgs in 1:nrow(brazilseriea_goalscored_h)) {
  for(brazilseriea_colhgs in 1:ncol(brazilseriea_goalscored_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowags in 1:nrow(brazilseriea_goalscored_a)) {
      for(brazilseriea_colags in 1:ncol(brazilseriea_goalscored_a)) {
        ifelse(!brazilseriea_goalscored_a[brazilseriea_rowags,brazilseriea_colags]=="",brazilseriea_goalscored_h[brazilseriea_rowags,brazilseriea_colags] <- brazilseriea_goalscored_a[brazilseriea_rowags,brazilseriea_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
brazilseriea_goalconceded_h <- tapply(BRAZILSERIEA$AG, BRAZILSERIEA[c("Home", "Date")],mean)
brazilseriea_goalconceded_a <- tapply(BRAZILSERIEA$HG, BRAZILSERIEA[c("Away", "Date")],mean)
brazilseriea_goalconceded_h[is.na(brazilseriea_goalconceded_h)] <- ""
brazilseriea_goalconceded_a[is.na(brazilseriea_goalconceded_a)] <- ""
for(brazilseriea_rowhgc in 1:nrow(brazilseriea_goalconceded_h)) {
  for(brazilseriea_colhgc in 1:ncol(brazilseriea_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowagc in 1:nrow(brazilseriea_goalconceded_a)) {
      for(brazilseriea_colagc in 1:ncol(brazilseriea_goalconceded_a)) {
        ifelse(!brazilseriea_goalconceded_a[brazilseriea_rowagc,brazilseriea_colagc]=="",brazilseriea_goalconceded_h[brazilseriea_rowagc,brazilseriea_colagc] <- brazilseriea_goalconceded_a[brazilseriea_rowagc,brazilseriea_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
###################################################################################################################################
#corners awarded
#######################################################################################################################################
#corners conceded
############################################################################################################################################
#corners form
#create home and away coscform matrices
################################################################################################################################################
#winmargin
brazilseriea_winmargin_h <- tapply(BRAZILSERIEA$HG - BRAZILSERIEA$AG, BRAZILSERIEA[c("Home", "Date")],mean)
brazilseriea_winmargin_a <- tapply(BRAZILSERIEA$AG - BRAZILSERIEA$HG, BRAZILSERIEA[c("Away", "Date")],mean)
brazilseriea_winmargin_h[is.na(brazilseriea_winmargin_h)] <- ""
brazilseriea_winmargin_a[is.na(brazilseriea_winmargin_a)] <- ""
#BRAZILSERIEA
for(brazilseriea_rowhwm in 1:nrow(brazilseriea_winmargin_h)) {
  for(brazilseriea_colhwm in 1:ncol(brazilseriea_winmargin_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowawm in 1:nrow(brazilseriea_winmargin_a)) {
      for(brazilseriea_colawm in 1:ncol(brazilseriea_winmargin_a)) {
        ifelse(!brazilseriea_winmargin_a[brazilseriea_rowawm,brazilseriea_colawm]=="",brazilseriea_winmargin_h[brazilseriea_rowawm,brazilseriea_colawm] <- brazilseriea_winmargin_a[brazilseriea_rowawm,brazilseriea_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
###############################################################################################################################################
#red card matrix
####################################################################################################################################################
#red totals
############################################################################################################################################################
#yellowtotals
##################################################################################################################################################
#team form
brazilseriea_form_h <- tapply(BRAZILSERIEA$FTR, BRAZILSERIEA[c("Home", "Date")],median)
brazilseriea_form_a <- tapply(BRAZILSERIEA$FTR, BRAZILSERIEA[c("Away", "Date")],median)
brazilseriea_form_h[is.na(brazilseriea_form_h)] <- ""
brazilseriea_form_a[is.na(brazilseriea_form_a)] <- ""
brazilseriea_form_h <- sub("A","L",brazilseriea_form_h)
brazilseriea_form_h <- sub("H","W",brazilseriea_form_h)
brazilseriea_form_a <- sub("A","W",brazilseriea_form_a)
brazilseriea_form_a <- sub("H","L",brazilseriea_form_a)
for(brazilseriea_rowh_f in 1:nrow(brazilseriea_form_h)) {
  for(brazilseriea_colh_f in 1:ncol(brazilseriea_form_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowa_f in 1:nrow(brazilseriea_form_a)) {
      for(brazilseriea_cola_f in 1:ncol(brazilseriea_form_a)) {
        ifelse(!brazilseriea_form_a[brazilseriea_rowa_f,brazilseriea_cola_f]=="",brazilseriea_form_h[brazilseriea_rowa_f,brazilseriea_cola_f] <- brazilseriea_form_a[brazilseriea_rowa_f,brazilseriea_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
brazilseriea_csform_h <- tapply(BRAZILSERIEA$CS, BRAZILSERIEA[c("Home", "Date")],median)
brazilseriea_csform_a <- tapply(BRAZILSERIEA$CS, BRAZILSERIEA[c("Away", "Date")],median)
brazilseriea_csform_h[is.na(brazilseriea_csform_h)] <- ""
brazilseriea_csform_a[is.na(brazilseriea_csform_a)] <- ""
#BRAZILSERIEA
for(brazilseriea_rowh_f_cs in 1:nrow(brazilseriea_csform_h)) {
  for(brazilseriea_colh_f_cs in 1:ncol(brazilseriea_csform_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowa_f_cs in 1:nrow(brazilseriea_csform_a)) {
      for(brazilseriea_cola_f_cs in 1:ncol(brazilseriea_csform_a)) {
        ifelse(!brazilseriea_csform_a[brazilseriea_rowa_f_cs,brazilseriea_cola_f_cs]=="",brazilseriea_csform_h[brazilseriea_rowa_f_cs,brazilseriea_cola_f_cs] <- brazilseriea_csform_a[brazilseriea_rowa_f_cs,brazilseriea_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
brazilseriea_totalgoals_h <- tapply(BRAZILSERIEA$TG, BRAZILSERIEA[c("Home", "Date")],mean)
brazilseriea_totalgoals_a <- tapply(BRAZILSERIEA$TG, BRAZILSERIEA[c("Away", "Date")],mean)
brazilseriea_totalgoals_h[is.na(brazilseriea_totalgoals_h)] <- ""
brazilseriea_totalgoals_a[is.na(brazilseriea_totalgoals_a)] <- ""
for(brazilseriea_rowh in 1:nrow(brazilseriea_totalgoals_h)) {
  for(brazilseriea_colh in 1:ncol(brazilseriea_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowa in 1:nrow(brazilseriea_totalgoals_a)) {
      for(brazilseriea_cola in 1:ncol(brazilseriea_totalgoals_a)) {
        ifelse(!brazilseriea_totalgoals_a[brazilseriea_rowa,brazilseriea_cola]=="",brazilseriea_totalgoals_h[brazilseriea_rowa,brazilseriea_cola] <- brazilseriea_totalgoals_a[brazilseriea_rowa,brazilseriea_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#BRAZILSERIEA
brazilseriea_un05_home <- c()
brazilseriea_un05_away <- c()
brazilseriea_ov05_home <- c()
brazilseriea_ov05_away <- c()

brazilseriea_un15_home <- c()
brazilseriea_un15_away <- c()
brazilseriea_ov15_home <- c()
brazilseriea_ov15_away <- c()

brazilseriea_un25_home <- c()
brazilseriea_un25_away <- c()
brazilseriea_ov25_home <- c()
brazilseriea_ov25_away <- c()

brazilseriea_un35_home <- c()
brazilseriea_un35_away <- c()
brazilseriea_ov35_home <- c()
brazilseriea_ov35_away <- c()

brazilseriea_un45_home <- c()
brazilseriea_un45_away <- c()
brazilseriea_ov45_home <- c()
brazilseriea_ov45_away <- c()

brazilseriea_un55_home <- c()
brazilseriea_un55_away <- c()
brazilseriea_ov55_home <- c()
brazilseriea_ov55_away <- c()

for (i_brazilseriea_tg in 1:length(brazilseriea_teams))
{

  brazilseriea_un05_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG == 0,])
  brazilseriea_un05_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG == 0,])

  brazilseriea_ov05_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG > 0,])
  brazilseriea_ov05_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG > 0,])

  brazilseriea_un15_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 1,])
  brazilseriea_un15_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 1,])

  brazilseriea_ov15_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 2,])
  brazilseriea_ov15_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 2,])

  brazilseriea_un25_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 2,])
  brazilseriea_un25_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 2,])

  brazilseriea_ov25_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >=3,])
  brazilseriea_ov25_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >=3,])

  brazilseriea_un35_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 3,])
  brazilseriea_un35_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 3,])

  brazilseriea_ov35_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 4,])
  brazilseriea_ov35_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 4,])

  brazilseriea_un45_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 4,])
  brazilseriea_un45_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 4,])

  brazilseriea_ov45_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 5,])
  brazilseriea_ov45_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 5,])

  brazilseriea_un55_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 5,])
  brazilseriea_un55_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG <= 5,])

  brazilseriea_ov55_home[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 6,])
  brazilseriea_ov55_away[i_brazilseriea_tg] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_tg] & BRAZILSERIEA$TG >= 6,])


}

brazilseriea_un05 <- brazilseriea_un05_home + brazilseriea_un05_away
brazilseriea_ov05 <- brazilseriea_ov05_home + brazilseriea_ov05_away

brazilseriea_un15 <- brazilseriea_un15_home + brazilseriea_un15_away
brazilseriea_ov15 <- brazilseriea_ov15_home + brazilseriea_ov15_away

brazilseriea_un25 <- brazilseriea_un25_home + brazilseriea_un25_away
brazilseriea_ov25 <- brazilseriea_ov25_home + brazilseriea_ov25_away

brazilseriea_un35 <- brazilseriea_un35_home + brazilseriea_un35_away
brazilseriea_ov35 <- brazilseriea_ov35_home + brazilseriea_ov35_away

brazilseriea_un45 <- brazilseriea_un45_home + brazilseriea_un45_away
brazilseriea_ov45 <- brazilseriea_ov45_home + brazilseriea_ov45_away

brazilseriea_un55 <- brazilseriea_un55_home + brazilseriea_un55_away
brazilseriea_ov55 <- brazilseriea_ov55_home + brazilseriea_ov55_away

brazilseriea_ovundata <- cbind(brazilseriea_teams,brazilseriea_un05,brazilseriea_ov05,brazilseriea_un15,brazilseriea_ov15,brazilseriea_un25,brazilseriea_ov25,brazilseriea_un35,brazilseriea_ov35,brazilseriea_un45,brazilseriea_ov45,brazilseriea_un55,brazilseriea_ov55)
#################################################################################################################################################################
#team against
brazilseriea_form_team_against_h <- tapply(BRAZILSERIEA$Away, BRAZILSERIEA[c("Home", "Date")],median)
brazilseriea_form_team_against_a <- tapply(BRAZILSERIEA$Home, BRAZILSERIEA[c("Away", "Date")],median)
brazilseriea_form_team_against_h[is.na(brazilseriea_form_team_against_h)] <- ""
brazilseriea_form_team_against_a[is.na(brazilseriea_form_team_against_a)] <- ""
#BRAZILSERIEA
for(brazilseriea_rowh_f_against in 1:nrow(brazilseriea_form_team_against_h)) {
  for(brazilseriea_colh_f_against in 1:ncol(brazilseriea_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(brazilseriea_rowa_f_against in 1:nrow(brazilseriea_form_team_against_a)) {
      for(brazilseriea_cola_f_against in 1:ncol(brazilseriea_form_team_against_a)) {
        ifelse(!brazilseriea_form_team_against_a[brazilseriea_rowa_f_against,brazilseriea_cola_f_against]=="",brazilseriea_form_team_against_h[brazilseriea_rowa_f_against,brazilseriea_cola_f_against] <- brazilseriea_form_team_against_a[brazilseriea_rowa_f_against,brazilseriea_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#home goals scored
brazilseriea_home_gs <- aggregate(BRAZILSERIEA$HG, by = list(BRAZILSERIEA$Home), FUN = sum)
brazilseriea_home_gs_avg <- aggregate(BRAZILSERIEA$HG, by = list(BRAZILSERIEA$Home),mean)
brazilseriea_home_scoring <- merge(brazilseriea_home_gs,brazilseriea_home_gs_avg, by='Group.1',all = T)
names(brazilseriea_home_scoring)[names(brazilseriea_home_scoring) == "x.x"] <- "TFthg"
names(brazilseriea_home_scoring)[names(brazilseriea_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
brazilseriea_away_gs <- aggregate(BRAZILSERIEA$AG, by = list(BRAZILSERIEA$Away), FUN = sum)
brazilseriea_away_gs_avg <- aggregate(BRAZILSERIEA$AG, by = list(BRAZILSERIEA$Away),mean)
brazilseriea_away_scoring <- merge(brazilseriea_away_gs,brazilseriea_away_gs_avg, by='Group.1',all = T)
names(brazilseriea_away_scoring)[names(brazilseriea_away_scoring) == "x.x"] <- "TFtag"
names(brazilseriea_away_scoring)[names(brazilseriea_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
brazilseriea_scoring <- merge(brazilseriea_home_scoring,brazilseriea_away_scoring,by='Group.1',all = T)
brazilseriea_scoring$TGS <- brazilseriea_scoring$TFthg + brazilseriea_scoring$TFtag

#homegoals conceded
brazilseriea_home_gc <- aggregate(BRAZILSERIEA$AG, by = list(BRAZILSERIEA$Home), FUN = sum)
brazilseriea_home_gc_avg <- aggregate(BRAZILSERIEA$AG, by = list(BRAZILSERIEA$Home),mean)
brazilseriea_home_conceding <- merge(brazilseriea_home_gc,brazilseriea_home_gc_avg, by='Group.1',all = T)
names(brazilseriea_home_conceding)[names(brazilseriea_home_conceding) == "x.x"] <- "TFthc"
names(brazilseriea_home_conceding)[names(brazilseriea_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
brazilseriea_away_gc <- aggregate(BRAZILSERIEA$HG, by = list(BRAZILSERIEA$Away), FUN = sum)
brazilseriea_away_gc_avg <- aggregate(BRAZILSERIEA$HG, by = list(BRAZILSERIEA$Away),mean)
brazilseriea_away_conceding <- merge(brazilseriea_away_gc,brazilseriea_away_gc_avg, by='Group.1',all = T)
names(brazilseriea_away_conceding)[names(brazilseriea_away_conceding) == "x.x"] <- "TFtac"
names(brazilseriea_away_conceding)[names(brazilseriea_away_conceding) == "x.y"] <- "Avg_Ftac"

#total goals conceded
brazilseriea_conceding <- merge(brazilseriea_home_conceding,brazilseriea_away_conceding,by='Group.1',all = T)
brazilseriea_conceding$TGC <- brazilseriea_conceding$TFthc + brazilseriea_conceding$TFtac
##################################################################################################################################################
##
#make div form uniform in entire data frame
BRAZILSERIEA$Div <- "BRAZILSERIEA"
##
###################################################################################################################################################
#poisson cards
######################################################################################################################################################
#league table
#B1
#hwins and away wins
brazilseriea_home_wins <- c()
brazilseriea_away_wins <- c()
brazilseriea_home_draws <- c()
brazilseriea_away_draws <- c()
brazilseriea_home_loss <- c()
brazilseriea_away_loss <- c()


for (i_brazilseriea_wins in 1:length(brazilseriea_teams))
{

  brazilseriea_home_wins[i_brazilseriea_wins] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_wins] & BRAZILSERIEA$FTR == "H",])
  brazilseriea_away_wins[i_brazilseriea_wins] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_wins] & BRAZILSERIEA$FTR == "A",])
  brazilseriea_home_draws[i_brazilseriea_wins] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_wins] & BRAZILSERIEA$FTR == "D",])
  brazilseriea_away_draws[i_brazilseriea_wins] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_wins] & BRAZILSERIEA$FTR == "D",])
  brazilseriea_home_loss[i_brazilseriea_wins] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Home == brazilseriea_teams[i_brazilseriea_wins] & BRAZILSERIEA$FTR == "A",])
  brazilseriea_away_loss[i_brazilseriea_wins] <- nrow(BRAZILSERIEA[BRAZILSERIEA$Away == brazilseriea_teams[i_brazilseriea_wins] & BRAZILSERIEA$FTR == "H",])

}

brazilseriea_total_wins <- brazilseriea_home_wins + brazilseriea_away_wins
brazilseriea_total_draws <- brazilseriea_home_draws + brazilseriea_away_draws
brazilseriea_total_loss <- brazilseriea_home_loss + brazilseriea_away_loss

brazilseriea_league_table <- cbind(brazilseriea_teams,brazilseriea_games_played,brazilseriea_total_wins,brazilseriea_total_draws,brazilseriea_total_loss)
brazilseriea_GS <- brazilseriea_scoring$TGS
brazilseriea_GC <-brazilseriea_conceding$TGC
brazilseriea_GD <- brazilseriea_scoring$TGS - brazilseriea_conceding$TGC
brazilseriea_PTS <- (brazilseriea_total_wins*3) + (brazilseriea_total_draws*1)
brazilseriea_league_table <- cbind(brazilseriea_league_table,brazilseriea_GS,brazilseriea_GC,brazilseriea_GD,brazilseriea_PTS)
brazilseriea_league_table <- as.data.frame(brazilseriea_league_table)
#rename the columns
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_teams"] <- "Team"
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_games_played"] <- "P"
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_total_wins"] <- "W"
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_total_draws"] <- "D"
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_total_loss"] <- "L"
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_GS"] <- "F"
names(brazilseriea_league_table)[names(brazilseriea_league_table) == "brazilseriea_GC"] <- "A"
points_brazilseriea <- brazilseriea_league_table[order(as.numeric(brazilseriea_league_table$brazilseriea_PTS), decreasing = TRUE),]
points_brazilseriea$brazilseriea_rank <- 1:length(brazilseriea_teams)
row.names(points_brazilseriea) <- points_brazilseriea$brazilseriea_rank
#create final_brazilseriea_hf_against with team ranks in brackets
for(brazilseriea_rowhrank in 1:nrow(brazilseriea_form_team_against_h)) {
  for(brazilseriea_colhrank in 1:ncol(brazilseriea_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!brazilseriea_form_team_against_h[brazilseriea_rowhrank,brazilseriea_colhrank]=="",brazilseriea_form_team_against_h[brazilseriea_rowhrank,brazilseriea_colhrank] <- paste(brazilseriea_form_team_against_h[brazilseriea_rowhrank,brazilseriea_colhrank],"(",points_brazilseriea$brazilseriea_rank[points_brazilseriea$Team ==brazilseriea_form_team_against_h[brazilseriea_rowhrank,brazilseriea_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])



  }
}
View(points_brazilseriea)
#################################################################################################################################################
#################################################################################################################################################
#poisson model
brazilseriea_GP <- nrow(BRAZILSERIEA)

#Calculate total home goals for each division
brazilseriea_T_HG <- sum(brazilseriea_home_gs$x)

#calculate average home goal
brazilseriea_avg_HG <- round(brazilseriea_T_HG /brazilseriea_GP, digits = 4)
############################################################
#Calculate total away goals for each division
brazilseriea_T_AG <- sum(brazilseriea_away_gs$x)
#calculate average away goal
brazilseriea_avg_AG <- round(brazilseriea_T_AG /brazilseriea_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
brazilseriea_home_as <- round(((brazilseriea_home_gs$x/brazilseriea_home_games))/brazilseriea_avg_HG, digits = 4)
#calculate away attack strength
brazilseriea_away_as <- round(((brazilseriea_away_gs$x/brazilseriea_away_games))/brazilseriea_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
brazilseriea_avg_HC <- round(brazilseriea_T_AG /brazilseriea_GP, digits = 4)
#avg away concede
brazilseriea_avg_AC <- round(brazilseriea_T_HG /brazilseriea_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
brazilseriea_home_ds <- round(((brazilseriea_home_gc$x/brazilseriea_home_games))/brazilseriea_avg_HC, digits = 4)
#away defense strength
brazilseriea_away_ds <- round(((brazilseriea_away_gc$x/brazilseriea_away_games))/brazilseriea_avg_AC, digits = 4)
#############################################################################
#home poisson data
#brazilseriea
brazilseriea_division <- c()
brazilseriea_division[1:length(brazilseriea_teams)] <- "BRAZILSERIEA"
brazilseriea_home_poisson <- cbind(brazilseriea_division,brazilseriea_teams,brazilseriea_avg_HG,brazilseriea_home_as,brazilseriea_home_ds)
#################################################################################
#away poisson data
#brazilseriea
brazilseriea_division <- c()
brazilseriea_division[1:length(brazilseriea_teams)] <- "BRAZILSERIEA"
brazilseriea_away_poisson <- cbind(brazilseriea_division,brazilseriea_teams,brazilseriea_avg_AG,brazilseriea_away_as,brazilseriea_away_ds)

#BRAZILSERIEA
Home_brazilseriea <- rep(brazilseriea_teams, each = length(brazilseriea_teams))
Away_brazilseriea <- rep(brazilseriea_teams, length(brazilseriea_teams))
BRAZILSERIEA_fixtures <- cbind(Home_brazilseriea,Away_brazilseriea)
BRAZILSERIEA_fixtures <- as.data.frame(BRAZILSERIEA_fixtures)
BRAZILSERIEA_fixtures <- BRAZILSERIEA_fixtures[!BRAZILSERIEA_fixtures$Home_brazilseriea == BRAZILSERIEA_fixtures$Away_brazilseriea,]
rownames(BRAZILSERIEA_fixtures) <- NULL
BRAZILSERIEA_fixtures$Div <- "BRAZILSERIEA"
BRAZILSERIEA_fixtures <- BRAZILSERIEA_fixtures[,c(3,1,2)]

BRAZILSERIEA_fixtures$avg_HG_brazilseriea <- brazilseriea_avg_HG

BRAZILSERIEA_fixtures$brazilseriea_homeas <- rep(brazilseriea_home_as,each = length(brazilseriea_teams)-1)

brazilseriea_awayds_lookup <- cbind(brazilseriea_teams,brazilseriea_away_ds)

brazilseriea_awayds_lookup <- as.data.frame(brazilseriea_awayds_lookup)

colnames(brazilseriea_awayds_lookup) <- c("Away_brazilseriea","brazilseriea_awayds")


require('RH2')
BRAZILSERIEA_fixtures$brazilseriea_awayds <- sqldf("SELECT brazilseriea_awayds_lookup.brazilseriea_awayds FROM brazilseriea_awayds_lookup INNER JOIN BRAZILSERIEA_fixtures ON brazilseriea_awayds_lookup.Away_brazilseriea = BRAZILSERIEA_fixtures.Away_brazilseriea")

BRAZILSERIEA_fixtures$avg_AG_brazilseriea <- brazilseriea_avg_AG

brazilseriea_awayas_lookup <- cbind(brazilseriea_teams,brazilseriea_away_as)

brazilseriea_awayas_lookup <- as.data.frame(brazilseriea_awayas_lookup)

colnames(brazilseriea_awayas_lookup) <- c("Away_brazilseriea","brazilseriea_awayas")


BRAZILSERIEA_fixtures$brazilseriea_awayas <- sqldf("SELECT brazilseriea_awayas_lookup.brazilseriea_awayas FROM brazilseriea_awayas_lookup INNER JOIN BRAZILSERIEA_fixtures ON brazilseriea_awayas_lookup.Away_brazilseriea = BRAZILSERIEA_fixtures.Away_brazilseriea")

BRAZILSERIEA_fixtures$brazilseriea_homeds <- rep(brazilseriea_home_ds,each = length(brazilseriea_teams)-1)

BRAZILSERIEA_fixtures$brazilseriea_awayds <- as.numeric(unlist(BRAZILSERIEA_fixtures$brazilseriea_awayds))
#xGH
BRAZILSERIEA_fixtures$brazilseriea_xGH <- BRAZILSERIEA_fixtures$avg_HG_brazilseriea * BRAZILSERIEA_fixtures$brazilseriea_homeas * BRAZILSERIEA_fixtures$brazilseriea_awayds

#xGA

BRAZILSERIEA_fixtures$brazilseriea_awayas <- as.numeric(unlist(BRAZILSERIEA_fixtures$brazilseriea_awayas))

BRAZILSERIEA_fixtures$brazilseriea_xGA <- BRAZILSERIEA_fixtures$avg_AG_brazilseriea * BRAZILSERIEA_fixtures$brazilseriea_awayas * BRAZILSERIEA_fixtures$brazilseriea_homeds

BRAZILSERIEA_fixtures$brazilseriea_0_0 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_0 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_0_1 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_1 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_0 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_0_2 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_2 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_1 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_2 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_3 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_0 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_1 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_2 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_0_3 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_3 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_3 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_4 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_0 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_1 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_2 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_3 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_0_4 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_4 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_4 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_4 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_5 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_0 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_1 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_2 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_3 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_4 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_0_5 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_5 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_5 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_5 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_5 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_6 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_0 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_1 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_2 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_3 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_4 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_6_5 <- round(stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_0_6 <- round(stats::dpois(0,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_1_6 <- round(stats::dpois(1,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_2_6 <- round(stats::dpois(2,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_3_6 <- round(stats::dpois(3,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_4_6 <- round(stats::dpois(4,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
BRAZILSERIEA_fixtures$brazilseriea_5_6 <- round(stats::dpois(5,BRAZILSERIEA_fixtures$brazilseriea_xGH) * stats::dpois(6,BRAZILSERIEA_fixtures$brazilseriea_xGA), digits = 4)
#Home win
BRAZILSERIEA_fixtures$brazilseriea_H <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_0 + BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5
)

BRAZILSERIEA_fixtures$brazilseriea_H <- percent(BRAZILSERIEA_fixtures$brazilseriea_H, accuracy = 0.1)

#Draw
BRAZILSERIEA_fixtures$brazilseriea_D <- (

  BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 + BRAZILSERIEA_fixtures$brazilseriea_4_4 +
    BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6
)

BRAZILSERIEA_fixtures$brazilseriea_D <- percent(BRAZILSERIEA_fixtures$brazilseriea_D, accuracy = 0.1)

#Away

BRAZILSERIEA_fixtures$brazilseriea_A <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 + BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6
)

BRAZILSERIEA_fixtures$brazilseriea_A <- percent(BRAZILSERIEA_fixtures$brazilseriea_A, accuracy = 0.1)

#ov25
BRAZILSERIEA_fixtures$brazilseriea_ov25 <- (
  BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 + BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 +
    BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 + BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_0 +
    BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 + BRAZILSERIEA_fixtures$brazilseriea_0_5 +
    BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 + BRAZILSERIEA_fixtures$brazilseriea_5_5 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5 + BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 +
    BRAZILSERIEA_fixtures$brazilseriea_4_6 + BRAZILSERIEA_fixtures$brazilseriea_5_6 + BRAZILSERIEA_fixtures$brazilseriea_6_6
)
#un25
BRAZILSERIEA_fixtures$brazilseriea_un25 <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_0_2
)
#odds
BRAZILSERIEA_fixtures$brazilseriea_ov25_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_ov25),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_un25_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_un25),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_ov25_odds
BRAZILSERIEA_fixtures$brazilseriea_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
BRAZILSERIEA_fixtures$brazilseriea_BTTSY <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_3_1 + BRAZILSERIEA_fixtures$brazilseriea_3_2 +
    BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_1_3 + BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_3_3 + BRAZILSERIEA_fixtures$brazilseriea_4_4 +
    BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_3 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 +
    BRAZILSERIEA_fixtures$brazilseriea_3_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_4 + BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_6_6 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6
)
#BTTSN
BRAZILSERIEA_fixtures$brazilseriea_BTTSN <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_0_2 +
    BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_5_0 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 + BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_0_6
)

BRAZILSERIEA_fixtures$brazilseriea_BTTSY_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_BTTSY),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_BTTSN_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_BTTSN),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_BTTSY <- percent(BRAZILSERIEA_fixtures$brazilseriea_BTTSY, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_BTTSN <- percent(BRAZILSERIEA_fixtures$brazilseriea_BTTSN, accuracy = 0.1)
#odds
BRAZILSERIEA_fixtures$brazilseriea_BTTSY_odds
BRAZILSERIEA_fixtures$brazilseriea_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
BRAZILSERIEA_fixtures$brazilseriea_AH_0_H <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_0 +BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5 + BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6
)
#AH_0_A
BRAZILSERIEA_fixtures$brazilseriea_AH_0_A <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 +BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6 + BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6
)

#odds
BRAZILSERIEA_fixtures$brazilseriea_AH_0_H_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_0_H),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_AH_0_A_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_0_A),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_AH_0_H_odds
BRAZILSERIEA_fixtures$brazilseriea_AH_0_A_odds
#percentages
BRAZILSERIEA_fixtures$brazilseriea_AH_0_H <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_0_H, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_AH_0_A <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_H <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_0 +BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5
)
#AH_n075_A
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_A <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 +BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6
)

#odds
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_H_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_n075_H),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_A_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_n075_A),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_AH_n075_H_odds
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_A_odds
#percentages
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_H <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_n075_H, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_AH_n075_A <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
BRAZILSERIEA_fixtures$brazilseriea_AH_075_H <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_0 +BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5 + BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6 + BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_1_2 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_3_4 + BRAZILSERIEA_fixtures$brazilseriea_4_5 + BRAZILSERIEA_fixtures$brazilseriea_5_6
)
#AH_075_A
BRAZILSERIEA_fixtures$brazilseriea_AH_075_A <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 +BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6 + BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6 + BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 + BRAZILSERIEA_fixtures$brazilseriea_6_5
)

#odds
BRAZILSERIEA_fixtures$brazilseriea_AH_075_H_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_075_H),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_AH_075_A_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_075_A),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_AH_075_H_odds
BRAZILSERIEA_fixtures$brazilseriea_AH_075_A_odds
#percentages
BRAZILSERIEA_fixtures$brazilseriea_AH_075_H <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_075_H, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_AH_075_A <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_H <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_0 +BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5
)
#AH_n125_A
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_A <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 +BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6
)

#odds
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_H_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_n125_H),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_A_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_n125_A),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_AH_n125_H_odds
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_A_odds
#percentages
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_H <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_n125_H, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_AH_n125_A <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
BRAZILSERIEA_fixtures$brazilseriea_AH_125_H <- (
  BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
    BRAZILSERIEA_fixtures$brazilseriea_5_0 +BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
    BRAZILSERIEA_fixtures$brazilseriea_6_5 + BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6 + BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_1_2 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_3_4 + BRAZILSERIEA_fixtures$brazilseriea_4_5 + BRAZILSERIEA_fixtures$brazilseriea_5_6
)
#AH_125_A
BRAZILSERIEA_fixtures$brazilseriea_AH_125_A <- (
  BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
    BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
    BRAZILSERIEA_fixtures$brazilseriea_0_5 +BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
    BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
    BRAZILSERIEA_fixtures$brazilseriea_5_6 + BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 +
    BRAZILSERIEA_fixtures$brazilseriea_4_4 + BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6 + BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 +
    BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 + BRAZILSERIEA_fixtures$brazilseriea_6_5
)

#odds
BRAZILSERIEA_fixtures$brazilseriea_AH_125_H_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_125_H),digits = 2)
BRAZILSERIEA_fixtures$brazilseriea_AH_125_A_odds <- round((1/BRAZILSERIEA_fixtures$brazilseriea_AH_125_A),digits = 2)

BRAZILSERIEA_fixtures$brazilseriea_AH_125_H_odds
BRAZILSERIEA_fixtures$brazilseriea_AH_125_A_odds
#percentages
BRAZILSERIEA_fixtures$brazilseriea_AH_125_H <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_125_H, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_AH_125_A <- percent(BRAZILSERIEA_fixtures$brazilseriea_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
BRAZILSERIEA_fixtures$brazilseriea_ov25 <- percent(BRAZILSERIEA_fixtures$brazilseriea_ov25, accuracy = 0.1)

BRAZILSERIEA_fixtures$brazilseriea_un25 <- percent(BRAZILSERIEA_fixtures$brazilseriea_un25, accuracy = 0.1)
BRAZILSERIEA_fixtures$brazilseriea_pscore <- paste(round(BRAZILSERIEA_fixtures$brazilseriea_xGH,digits = 0),round(BRAZILSERIEA_fixtures$brazilseriea_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
brazilseriea_last_n_games <- 6

#create final_brazilseriea_hf object
final_brazilseriea_hf <- c()
for(index_brazilseriea_hf in 1:length(brazilseriea_teams))
{
  index_brazilseriea_hf <- row.names(brazilseriea_form_h) == brazilseriea_teams[index_brazilseriea_hf]
  form_brazilseriea_hf <- brazilseriea_form_h[index_brazilseriea_hf]
  deleted_form_brazilseriea_hf <- form_brazilseriea_hf[!form_brazilseriea_hf[] == ""]
  l6_form_brazilseriea_hf <- tail(deleted_form_brazilseriea_hf,brazilseriea_last_n_games)
  l6_form_brazilseriea_hf <- paste(l6_form_brazilseriea_hf,collapse = " ")
  final_brazilseriea_hf[index_brazilseriea_hf] <- rbind(paste(brazilseriea_teams[index_brazilseriea_hf],l6_form_brazilseriea_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_form)

}

#change column nam
final_brazilseriea_hf <- as.data.frame(final_brazilseriea_hf)
colnames(final_brazilseriea_hf) <- "Form"
#goals scored
#create final_brazilseriea_gs object
final_brazilseriea_gs <- c()
suml6_brazilseriea_gs <- c()
for(index_brazilseriea_gs in 1:length(brazilseriea_teams))
{
  index_brazilseriea_gs <- row.names(brazilseriea_goalscored_h) == brazilseriea_teams[index_brazilseriea_gs]
  form_brazilseriea_gs <- brazilseriea_goalscored_h[index_brazilseriea_gs]
  deleted_form_brazilseriea_gs <- form_brazilseriea_gs[!form_brazilseriea_gs[] == ""]
  l6_form_brazilseriea_gs <- tail(deleted_form_brazilseriea_gs,brazilseriea_last_n_games)
  l6_form_brazilseriea_gs <- as.numeric(l6_form_brazilseriea_gs)
  suml6_brazilseriea_gs[index_brazilseriea_gs] <- sum(l6_form_brazilseriea_gs)
  suml6_brazilseriea_gs[index_brazilseriea_gs] <- paste("(",suml6_brazilseriea_gs[index_brazilseriea_gs],")",sep = "")
  l6_form_brazilseriea_gs <- paste(l6_form_brazilseriea_gs,collapse = " ")
  final_brazilseriea_gs[index_brazilseriea_gs] <- rbind(paste(brazilseriea_teams[index_brazilseriea_gs],l6_form_brazilseriea_gs,suml6_brazilseriea_gs[index_brazilseriea_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_form)

}
final_brazilseriea_gs
#change column names
final_brazilseriea_gs <- as.data.frame(final_brazilseriea_gs)
colnames(final_brazilseriea_gs) <- "Goals scored"
#goal conceded
#create final_brazilseriea_gc object
final_brazilseriea_gc <- c()
suml6_brazilseriea_gc <- c()
for(index_brazilseriea_gc in 1:length(brazilseriea_teams))
{
  index_brazilseriea_gc <- row.names(brazilseriea_goalconceded_h) == brazilseriea_teams[index_brazilseriea_gc]
  form_brazilseriea_gc <- brazilseriea_goalconceded_h[index_brazilseriea_gc]
  deleted_form_brazilseriea_gc <- form_brazilseriea_gc[!form_brazilseriea_gc[] == ""]
  l6_form_brazilseriea_gc <- tail(deleted_form_brazilseriea_gc,brazilseriea_last_n_games)
  l6_form_brazilseriea_gc <- as.numeric(l6_form_brazilseriea_gc)
  suml6_brazilseriea_gc[index_brazilseriea_gc] <- sum(l6_form_brazilseriea_gc)
  suml6_brazilseriea_gc[index_brazilseriea_gc] <- paste("(",suml6_brazilseriea_gc[index_brazilseriea_gc],")",sep = "")
  l6_form_brazilseriea_gc <- paste(l6_form_brazilseriea_gc,collapse = " ")
  final_brazilseriea_gc[index_brazilseriea_gc] <- rbind(paste(brazilseriea_teams[index_brazilseriea_gc],l6_form_brazilseriea_gc,suml6_brazilseriea_gc[index_brazilseriea_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_form)

}
#change column names
final_brazilseriea_gc <- as.data.frame(final_brazilseriea_gc)
colnames(final_brazilseriea_gc) <- "Goals conceded"


toString(l6_form_brazilseriea_gc)
#total goals
#create final_brazilseriea_tg object
final_brazilseriea_tg <- c()
suml6_brazilseriea_tg <- c()
for(index_brazilseriea_tg in 1:length(brazilseriea_teams))
{
  index_brazilseriea_tg <- row.names(brazilseriea_totalgoals_h) == brazilseriea_teams[index_brazilseriea_tg]
  form_brazilseriea_tg <- brazilseriea_totalgoals_h[index_brazilseriea_tg]
  deleted_form_brazilseriea_tg <- form_brazilseriea_tg[!form_brazilseriea_tg[] == ""]
  l6_form_brazilseriea_tg <- tail(deleted_form_brazilseriea_tg,brazilseriea_last_n_games)
  l6_form_brazilseriea_tg <- as.numeric(l6_form_brazilseriea_tg)
  suml6_brazilseriea_tg[index_brazilseriea_tg] <- sum(l6_form_brazilseriea_tg)
  suml6_brazilseriea_tg[index_brazilseriea_tg] <- paste("(",suml6_brazilseriea_tg[index_brazilseriea_tg],")",sep = "")
  l6_form_brazilseriea_tg <- paste(l6_form_brazilseriea_tg,collapse = " ")
  final_brazilseriea_tg[index_brazilseriea_tg] <- rbind(paste(brazilseriea_teams[index_brazilseriea_tg],l6_form_brazilseriea_tg,suml6_brazilseriea_tg[index_brazilseriea_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_form)

}
#change column names
final_brazilseriea_tg <- as.data.frame(final_brazilseriea_tg)
colnames(final_brazilseriea_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_brazilseriea_hf object
final_brazilseriea_cs <- c()
for(index_brazilseriea_cs in 1:length(brazilseriea_teams))
{
  index_brazilseriea_cs <- row.names(brazilseriea_csform_h) == brazilseriea_teams[index_brazilseriea_cs]
  csform_brazilseriea_cs <- brazilseriea_csform_h[index_brazilseriea_cs]
  deleted_csform_brazilseriea_cs <- csform_brazilseriea_cs[!csform_brazilseriea_cs[] == ""]
  l6_csform_brazilseriea_cs <- tail(deleted_csform_brazilseriea_cs,brazilseriea_last_n_games)
  l6_csform_brazilseriea_cs <- paste(l6_csform_brazilseriea_cs,collapse = " ")
  final_brazilseriea_cs[index_brazilseriea_cs] <- rbind(paste(brazilseriea_teams[index_brazilseriea_cs],l6_csform_brazilseriea_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_csform)

}

#change column names
final_brazilseriea_cs <- as.data.frame(final_brazilseriea_cs)
colnames(final_brazilseriea_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_brazilseriea_wm object
final_brazilseriea_wm <- c()
suml6_brazilseriea_wm <- c()
for(index_brazilseriea_wm in 1:length(brazilseriea_teams))
{
  index_brazilseriea_wm <- row.names(brazilseriea_winmargin_h) == brazilseriea_teams[index_brazilseriea_wm]
  form_brazilseriea_wm <- brazilseriea_winmargin_h[index_brazilseriea_wm]
  deleted_form_brazilseriea_wm <- form_brazilseriea_wm[!form_brazilseriea_wm[] == ""]
  l6_form_brazilseriea_wm <- tail(deleted_form_brazilseriea_wm,brazilseriea_last_n_games)
  l6_form_brazilseriea_wm <- as.numeric(l6_form_brazilseriea_wm)
  suml6_brazilseriea_wm[index_brazilseriea_wm] <- sum(l6_form_brazilseriea_wm)
  suml6_brazilseriea_wm[index_brazilseriea_wm] <- paste("(",suml6_brazilseriea_wm[index_brazilseriea_wm],")",sep = "")
  l6_form_brazilseriea_wm <- paste(l6_form_brazilseriea_wm,collapse = " ")
  final_brazilseriea_wm[index_brazilseriea_wm] <- rbind(paste(brazilseriea_teams[index_brazilseriea_wm],l6_form_brazilseriea_wm,suml6_brazilseriea_wm[index_brazilseriea_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_form)

}
final_brazilseriea_wm
#change column names
final_brazilseriea_wm <- as.data.frame(final_brazilseriea_wm)
colnames(final_brazilseriea_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
##################################################
##################################################
#corners awarded
#create final_brazilseriea_ca object
##################################################
##################################################
#corners form
##################################################
#total corners
#create final_brazilseriea_tcorners object

###################################################
#Team against
#create final_brazilseriea_hf_against
final_brazilseriea_hf_against <- c()
for(index_brazilseriea_hf_against in 1:length(brazilseriea_teams))
{
  index_brazilseriea_hf_against <- row.names(brazilseriea_form_team_against_h) == brazilseriea_teams[index_brazilseriea_hf_against]
  form_brazilseriea_hf_against <- brazilseriea_form_team_against_h[index_brazilseriea_hf_against]
  deleted_form_brazilseriea_hf_against <- form_brazilseriea_hf_against[!form_brazilseriea_hf_against[] == ""]
  l6_form_brazilseriea_hf_against <- tail(deleted_form_brazilseriea_hf_against,brazilseriea_last_n_games)
  l6_form_brazilseriea_hf_against <- paste(l6_form_brazilseriea_hf_against,collapse = " ")
  final_brazilseriea_hf_against[index_brazilseriea_hf_against] <- rbind(paste(brazilseriea_teams[index_brazilseriea_hf_against],l6_form_brazilseriea_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",brazilseriea_teams[index],l6_form)

}
final_brazilseriea_hf_against <- as.data.frame(final_brazilseriea_hf_against)
colnames(final_brazilseriea_hf_against) <- "Team against"
#combine the columns
final_brazilseriea_all <- cbind(final_brazilseriea_hf,final_brazilseriea_gs,final_brazilseriea_gc,final_brazilseriea_tg,final_brazilseriea_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#BRAZILSERIEA
BRAZILSERIEA_sim <- BRAZILSERIEA
BRAZILSERIEA_sim$matchid <- paste(BRAZILSERIEA_sim$Home,BRAZILSERIEA_sim$Away,sep = "-")
BRAZILSERIEA_fixtures$matchid <- paste(BRAZILSERIEA_fixtures$Home_brazilseriea,BRAZILSERIEA_fixtures$Away_brazilseriea,sep = "-")
BRAZILSERIEA_fixtures$brazilseriea_FTR <- sapply(BRAZILSERIEA_fixtures$brazilseriea_pscore,switch,
                               '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                               '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                               '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

BRAZILSERIEA_fixtures$brazilseriea_gamestatus <- ifelse(BRAZILSERIEA_fixtures$matchid %in% BRAZILSERIEA_sim$matchid,"played","notplayed")

brazilseriea_home_wins_sim <- c()
brazilseriea_away_wins_sim <- c()
brazilseriea_home_draws_sim <- c()
brazilseriea_away_draws_sim <- c()
brazilseriea_home_loss_sim <- c()
brazilseriea_away_loss_sim <- c()

View(final_brazilseriea_all)

for (i_brazilseriea_wins_sim in 1:length(brazilseriea_teams))
{

  brazilseriea_home_wins_sim[i_brazilseriea_wins_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Home_brazilseriea == brazilseriea_teams[i_brazilseriea_wins_sim] & BRAZILSERIEA_fixtures$brazilseriea_FTR == "H" & BRAZILSERIEA_fixtures$brazilseriea_gamestatus =="notplayed",])
  brazilseriea_away_wins_sim[i_brazilseriea_wins_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Away_brazilseriea == brazilseriea_teams[i_brazilseriea_wins_sim] & BRAZILSERIEA_fixtures$brazilseriea_FTR == "A" & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",])
  brazilseriea_home_draws_sim[i_brazilseriea_wins_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Home_brazilseriea == brazilseriea_teams[i_brazilseriea_wins_sim] & BRAZILSERIEA_fixtures$brazilseriea_FTR == "D" & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",])
  brazilseriea_away_draws_sim[i_brazilseriea_wins_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Away_brazilseriea == brazilseriea_teams[i_brazilseriea_wins_sim] & BRAZILSERIEA_fixtures$brazilseriea_FTR == "D" & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",])
  brazilseriea_home_loss_sim[i_brazilseriea_wins_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Home_brazilseriea == brazilseriea_teams[i_brazilseriea_wins_sim] & BRAZILSERIEA_fixtures$brazilseriea_FTR == "A" & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",])
  brazilseriea_away_loss_sim[i_brazilseriea_wins_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Away_brazilseriea == brazilseriea_teams[i_brazilseriea_wins_sim] & BRAZILSERIEA_fixtures$brazilseriea_FTR == "H" & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed", ])

}

brazilseriea_total_wins_sim <- brazilseriea_home_wins_sim + brazilseriea_away_wins_sim
brazilseriea_total_draws_sim <- brazilseriea_home_draws_sim + brazilseriea_away_draws_sim
brazilseriea_total_loss_sim <- brazilseriea_home_loss_sim + brazilseriea_away_loss_sim

brazilseriea_home_games_sim <- c()
brazilseriea_away_games_sim <-c()

for (i_brazilseriea_sim in 1:length(brazilseriea_teams))
{

  brazilseriea_home_games_sim[i_brazilseriea_sim] <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Home_brazilseriea == brazilseriea_teams[i_brazilseriea_sim] & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",])
  brazilseriea_away_games_sim[i_brazilseriea_sim]  <- nrow(BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$Away_brazilseriea == brazilseriea_teams[i_brazilseriea_sim] & BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",])

}

brazilseriea_games_played_sim <- brazilseriea_home_games_sim + brazilseriea_away_games_sim

brazilseriea_league_table_sim <- cbind(brazilseriea_teams,brazilseriea_games_played_sim,brazilseriea_total_wins_sim,brazilseriea_total_draws_sim,brazilseriea_total_loss_sim)
brazilseriea_PTS_sim <- (brazilseriea_total_wins_sim*3) + (brazilseriea_total_draws_sim*1)
brazilseriea_league_table_sim <- cbind(brazilseriea_league_table_sim,brazilseriea_PTS_sim)

brazilseriea_games_played_simfinal <- brazilseriea_games_played + brazilseriea_games_played_sim
brazilseriea_total_wins_simfinal <- brazilseriea_total_wins + brazilseriea_total_wins_sim
brazilseriea_total_draws_simfinal <- brazilseriea_total_draws + brazilseriea_total_draws_sim
brazilseriea_total_loss_simfinal <- brazilseriea_total_loss + brazilseriea_total_loss_sim
brazilseriea_PTS_simfinal <- brazilseriea_PTS + brazilseriea_PTS_sim

brazilseriea_league_table_simfinal <- cbind(brazilseriea_teams,brazilseriea_games_played_simfinal,brazilseriea_total_wins_simfinal,brazilseriea_total_draws_simfinal,brazilseriea_total_loss_simfinal,brazilseriea_PTS_simfinal)
brazilseriea_league_table_simfinal <- as.data.frame(brazilseriea_league_table_simfinal)
names(brazilseriea_league_table_simfinal)[names(brazilseriea_league_table_simfinal) == "brazilseriea_teams"] <- "Team_f"
names(brazilseriea_league_table_simfinal)[names(brazilseriea_league_table_simfinal) == "brazilseriea_games_played_simfinal"] <- "P_f"
names(brazilseriea_league_table_simfinal)[names(brazilseriea_league_table_simfinal) == "brazilseriea_total_wins_simfinal"] <- "W_f"
names(brazilseriea_league_table_simfinal)[names(brazilseriea_league_table_simfinal) == "brazilseriea_total_draws_simfinal"] <- "D_f"
names(brazilseriea_league_table_simfinal)[names(brazilseriea_league_table_simfinal) == "brazilseriea_total_loss_simfinal"] <- "L_f"
names(brazilseriea_league_table_simfinal)[names(brazilseriea_league_table_simfinal) == "brazilseriea_PTS_simfinal"] <- "PTS_f"
points_brazilseriea_sim <-  brazilseriea_league_table_simfinal[order(as.numeric(brazilseriea_league_table_simfinal$PTS_f), decreasing = TRUE),]

BRAZILSERIEA_notplayed <- BRAZILSERIEA_fixtures[BRAZILSERIEA_fixtures$brazilseriea_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#BRAZILSERIEA
BRAZILSERIEA_fixtures$Hometeam_brazilseriea_index <- match(BRAZILSERIEA_fixtures$Home_brazilseriea,brazilseriea_teams)
BRAZILSERIEA_fixtures$Awayteam_brazilseriea_index <- match(BRAZILSERIEA_fixtures$Away_brazilseriea,brazilseriea_teams)
brazilseriea_prediction <- c()
brazilseriea_HWM <- c()
brazilseriea_AWM <- c()
brazilseriea_HWMLM <- c()
brazilseriea_AWMLM <- c()
brazilseriea_HY <- c()
brazilseriea_AY <- c()
brazilseriea_HCO <- c()
brazilseriea_ACO <- c()
brazilseriea_HXSC <- c()
brazilseriea_AXSC <- c()
brazilseriea_HYCPF <- c()
brazilseriea_AYCPF <- c()
for(brazilseriea_row in 1:nrow(BRAZILSERIEA_fixtures))
{

  brazilseriea_hometeamindex <- BRAZILSERIEA_fixtures[brazilseriea_row,"Hometeam_brazilseriea_index"]
  brazilseriea_awayteamindex <- BRAZILSERIEA_fixtures[brazilseriea_row,"Awayteam_brazilseriea_index"]
  #analyse team form
  #home team
  brazilseriea_form_vec_ht <- as.vector(brazilseriea_form_h[brazilseriea_hometeamindex,])
  brazilseriea_form_vec_ht[is.na(brazilseriea_form_vec_ht)] <- ""
  brazilseriea_form_vec_ht <- brazilseriea_form_vec_ht[brazilseriea_form_vec_ht != ""]
  brazilseriea_form_vec_ht  <-tail(brazilseriea_form_vec_ht,6)
  brazilseriea_ht_numberof_wins <- length(which(brazilseriea_form_vec_ht == "W"))
  brazilseriea_ht_numberof_draws <- length(which(brazilseriea_form_vec_ht == "D"))
  brazilseriea_ht_numberof_loss <- length(which(brazilseriea_form_vec_ht == "L"))
  #awayteam
  brazilseriea_form_vec_at <- as.vector(brazilseriea_form_h[brazilseriea_awayteamindex,])
  brazilseriea_form_vec_at[is.na(brazilseriea_form_vec_at)] <- ""
  brazilseriea_form_vec_at <- brazilseriea_form_vec_at[brazilseriea_form_vec_at != ""]
  brazilseriea_form_vec_at  <-tail(brazilseriea_form_vec_at,6)
  brazilseriea_at_numberof_wins <- length(which(brazilseriea_form_vec_at == "W"))
  brazilseriea_at_numberof_draws <- length(which(brazilseriea_form_vec_at == "D"))
  brazilseriea_at_numberof_loss <- length(which(brazilseriea_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  brazilseriea_goalscored_vec_ht <- as.vector(brazilseriea_goalscored_h[brazilseriea_hometeamindex,])
  brazilseriea_goalscored_vec_ht[is.na(brazilseriea_goalscored_vec_ht)] <- ""
  brazilseriea_goalscored_vec_ht <- brazilseriea_goalscored_vec_ht[brazilseriea_goalscored_vec_ht != ""]
  brazilseriea_goalscored_vec_ht  <-tail(brazilseriea_goalscored_vec_ht,6)
  brazilseriea_goalscored_vec_ht  <- as.numeric(brazilseriea_goalscored_vec_ht)
  brazilseriea_ht_totalgoalscored <- sum(brazilseriea_goalscored_vec_ht)
  brazilseriea_ht_matches_scoring <- length(which(brazilseriea_goalscored_vec_ht > 0))
  brazilseriea_ht_matches_without_scoring <- length(which(brazilseriea_goalscored_vec_ht == "0"))
  #awayteam
  brazilseriea_goalscored_vec_at <- as.vector(brazilseriea_goalscored_h[brazilseriea_awayteamindex,])
  brazilseriea_goalscored_vec_at[is.na(brazilseriea_goalscored_vec_at)] <- ""
  brazilseriea_goalscored_vec_at <- brazilseriea_goalscored_vec_at[brazilseriea_goalscored_vec_at != ""]
  brazilseriea_goalscored_vec_at  <-tail(brazilseriea_goalscored_vec_at,6)
  brazilseriea_goalscored_vec_at  <- as.numeric(brazilseriea_goalscored_vec_at)
  brazilseriea_at_totalgoalscored <- sum(brazilseriea_goalscored_vec_at)
  brazilseriea_at_matches_scoring <- length(which(brazilseriea_goalscored_vec_at > 0))
  brazilseriea_at_matches_without_scoring <- length(which(brazilseriea_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  brazilseriea_goalconceded_vec_ht <- as.vector(brazilseriea_goalconceded_h[brazilseriea_hometeamindex,])
  brazilseriea_goalconceded_vec_ht[is.na(brazilseriea_goalconceded_vec_ht)] <- ""
  brazilseriea_goalconceded_vec_ht <- brazilseriea_goalconceded_vec_ht[brazilseriea_goalconceded_vec_ht != ""]
  brazilseriea_goalconceded_vec_ht  <-tail(brazilseriea_goalconceded_vec_ht,6)
  brazilseriea_goalconceded_vec_ht  <- as.numeric(brazilseriea_goalconceded_vec_ht)
  brazilseriea_goalconceded_vec_ht
  brazilseriea_ht_totalgoalconceded <- sum(brazilseriea_goalconceded_vec_ht)
  brazilseriea_ht_matches_concede <- length(which(brazilseriea_goalconceded_vec_ht > 0))
  brazilseriea_ht_matches_without_concede <- length(which(brazilseriea_goalconceded_vec_ht == "0"))
  #awayteam
  brazilseriea_goalconceded_vec_at <- as.vector(brazilseriea_goalconceded_h[brazilseriea_awayteamindex,])
  brazilseriea_goalconceded_vec_at[is.na(brazilseriea_goalconceded_vec_at)] <- ""
  brazilseriea_goalconceded_vec_at <- brazilseriea_goalconceded_vec_at[brazilseriea_goalconceded_vec_at != ""]
  brazilseriea_goalconceded_vec_at  <-tail(brazilseriea_goalconceded_vec_at,6)
  brazilseriea_goalconceded_vec_at  <- as.numeric(brazilseriea_goalconceded_vec_at)
  brazilseriea_at_totalgoalconceded <- sum(brazilseriea_goalconceded_vec_at)
  brazilseriea_at_matches_concede <- length(which(brazilseriea_goalconceded_vec_at > 0))
  brazilseriea_at_matches_without_concede <- length(which(brazilseriea_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  brazilseriea_totalgoals_vec_ht <- as.vector(brazilseriea_totalgoals_h[brazilseriea_hometeamindex,])
  brazilseriea_totalgoals_vec_ht[is.na(brazilseriea_totalgoals_vec_ht)] <- ""
  brazilseriea_totalgoals_vec_ht <- brazilseriea_totalgoals_vec_ht[brazilseriea_totalgoals_vec_ht != ""]
  brazilseriea_totalgoals_vec_ht  <-tail(brazilseriea_totalgoals_vec_ht,6)
  brazilseriea_totalgoals_vec_ht  <- as.numeric(brazilseriea_totalgoals_vec_ht)
  brazilseriea_totalgoals_vec_ht
  brazilseriea_ht_totalgoals <- sum(brazilseriea_totalgoals_vec_ht)
  brazilseriea_ht_avgtotalgoals <- (brazilseriea_ht_totalgoals/6)
  brazilseriea_ht_no_of_ov25 <- length(which(brazilseriea_totalgoals_vec_ht >= 3))
  brazilseriea_ht_no_of_un25 <- length(which(brazilseriea_totalgoals_vec_ht <= 2))
  #awayteam
  brazilseriea_totalgoals_vec_at <- as.vector(brazilseriea_totalgoals_h[brazilseriea_awayteamindex,])
  brazilseriea_totalgoals_vec_at[is.na(brazilseriea_totalgoals_vec_at)] <- ""
  brazilseriea_totalgoals_vec_at <- brazilseriea_totalgoals_vec_at[brazilseriea_totalgoals_vec_at != ""]
  brazilseriea_totalgoals_vec_at  <-tail(brazilseriea_totalgoals_vec_at,6)
  brazilseriea_totalgoals_vec_at  <- as.numeric(brazilseriea_totalgoals_vec_at)
  brazilseriea_totalgoals_vec_at
  brazilseriea_at_totalgoals <- sum(brazilseriea_totalgoals_vec_at)
  brazilseriea_at_avgtotalgoals <- (brazilseriea_at_totalgoals/6)
  brazilseriea_at_no_of_ov25 <- length(which(brazilseriea_totalgoals_vec_at >= 3))
  brazilseriea_at_no_of_un25 <- length(which(brazilseriea_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  brazilseriea_winmargin_vec_ht <- as.vector(brazilseriea_winmargin_h[brazilseriea_hometeamindex,])
  brazilseriea_winmargin_vec_ht[is.na(brazilseriea_winmargin_vec_ht)] <- ""
  brazilseriea_winmargin_vec_ht <- brazilseriea_winmargin_vec_ht[brazilseriea_winmargin_vec_ht != ""]
  brazilseriea_winmargin_vec_ht  <-tail(brazilseriea_winmargin_vec_ht,6)
  brazilseriea_winmargin_vec_ht  <- as.numeric(brazilseriea_winmargin_vec_ht)

  brazilseriea_ht_totalwinmargin <- sum(brazilseriea_winmargin_vec_ht)
  brazilseriea_ht_no_of_winmargin_ov0 <- length(which(brazilseriea_winmargin_vec_ht >= 0))
  brazilseriea_ht_no_of_winmargin_ov1 <- length(which(brazilseriea_winmargin_vec_ht >= 1))
  brazilseriea_ht_no_of_winmargin_un0 <- length(which(brazilseriea_winmargin_vec_ht <= 0))
  brazilseriea_ht_no_of_winmargin_un1 <- length(which(brazilseriea_winmargin_vec_ht <= 1))
  #awayteam
  brazilseriea_winmargin_vec_at <- as.vector(brazilseriea_winmargin_h[brazilseriea_awayteamindex,])
  brazilseriea_winmargin_vec_at[is.na(brazilseriea_winmargin_vec_at)] <- ""
  brazilseriea_winmargin_vec_at <- brazilseriea_winmargin_vec_at[brazilseriea_winmargin_vec_at != ""]
  brazilseriea_winmargin_vec_at  <-tail(brazilseriea_winmargin_vec_at,6)
  brazilseriea_winmargin_vec_at  <- as.numeric(brazilseriea_winmargin_vec_at)

  brazilseriea_at_totalwinmargin <- sum(brazilseriea_winmargin_vec_at)
  brazilseriea_at_no_of_winmargin_ov0 <- length(which(brazilseriea_winmargin_vec_at >= 0))
  brazilseriea_at_no_of_winmargin_ov1 <- length(which(brazilseriea_winmargin_vec_at >= 1))
  brazilseriea_at_no_of_winmargin_un0 <- length(which(brazilseriea_winmargin_vec_at <= 0))
  brazilseriea_at_no_of_winmargin_un1 <- length(which(brazilseriea_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  brazilseriea_winmargin_vec_ht_lm <- as.vector(brazilseriea_winmargin_h[brazilseriea_hometeamindex,])
  brazilseriea_winmargin_vec_ht_lm[is.na(brazilseriea_winmargin_vec_ht_lm)] <- ""
  brazilseriea_winmargin_vec_ht_lm <- brazilseriea_winmargin_vec_ht_lm[brazilseriea_winmargin_vec_ht_lm != ""]
  brazilseriea_winmargin_vec_ht_lm  <-tail(brazilseriea_winmargin_vec_ht_lm,1)
  #awayteam
  brazilseriea_winmargin_vec_at_lm <- as.vector(brazilseriea_winmargin_h[brazilseriea_awayteamindex,])
  brazilseriea_winmargin_vec_at_lm[is.na(brazilseriea_winmargin_vec_at_lm)] <- ""
  brazilseriea_winmargin_vec_at_lm <- brazilseriea_winmargin_vec_at_lm[brazilseriea_winmargin_vec_at_lm != ""]
  brazilseriea_winmargin_vec_at_lm  <-tail(brazilseriea_winmargin_vec_at_lm,1)
  ################################################################################

  #################################################################################
  #################################################################################
  #pick xpected shots conversion
  #################################################################################

  ####we need to decide ############
  #winner goals
  brazilseriea_ht_last6points <- brazilseriea_ht_numberof_wins*3 + brazilseriea_ht_numberof_draws*1
  brazilseriea_at_last6points <- brazilseriea_at_numberof_wins*3 + brazilseriea_at_numberof_draws*1

  if(brazilseriea_ht_last6points > brazilseriea_at_last6points) {brazilseriea_3waypick <- "1"}  else {brazilseriea_3waypick <- "X2"}

  if(brazilseriea_at_last6points > brazilseriea_ht_last6points ) {brazilseriea_3waypick <- "2"} else {brazilseriea_3waypick <- "1X"}

  if(brazilseriea_ht_no_of_ov25 + brazilseriea_at_no_of_ov25 >= 6) {brazilseriea_goalspick <- "ov25"} else {brazilseriea_goalspick <- "un25"}

  if(brazilseriea_ht_no_of_un25 + brazilseriea_at_no_of_un25 >= 6) {brazilseriea_goalspick <- "un25"} else {brazilseriea_goalspick <- "ov25"}

  if(brazilseriea_ht_matches_scoring >= 4 && brazilseriea_at_matches_scoring >=4) {brazilseriea_btts <- "BTTS-Y"} else {brazilseriea_btts <- "BTTS-N"}


  brazilseriea_prediction[brazilseriea_row] <- rbind(paste(brazilseriea_3waypick,brazilseriea_goalspick,brazilseriea_btts,sep = ","))
  brazilseriea_HWM[brazilseriea_row] <- brazilseriea_ht_totalwinmargin
  brazilseriea_AWM[brazilseriea_row] <- brazilseriea_at_totalwinmargin

  brazilseriea_HWMLM[brazilseriea_row] <- brazilseriea_winmargin_vec_ht_lm
  brazilseriea_AWMLM[brazilseriea_row] <- brazilseriea_winmargin_vec_at_lm


}

brazilseriea_prediction <- as.data.frame(brazilseriea_prediction)
colnames(brazilseriea_prediction) <- "prediction"

brazilseriea_HWM <- as.data.frame(brazilseriea_HWM)
colnames(brazilseriea_HWM) <- "HWM"

brazilseriea_AWM <- as.data.frame(brazilseriea_AWM)
colnames(brazilseriea_AWM) <- "AWM"

brazilseriea_HWMLM <- as.data.frame(brazilseriea_HWMLM)
colnames(brazilseriea_HWMLM) <- "HWMLM"

brazilseriea_AWMLM <- as.data.frame(brazilseriea_AWMLM)
colnames(brazilseriea_AWMLM) <- "AWMLM"

brazilseriea_picks <- cbind(BRAZILSERIEA_fixtures$Div,BRAZILSERIEA_fixtures$Home_brazilseriea,BRAZILSERIEA_fixtures$Away_brazilseriea,brazilseriea_prediction,brazilseriea_HWM,brazilseriea_AWM,brazilseriea_HWMLM,brazilseriea_AWMLM)

colnames(brazilseriea_picks)[1] <- "picks_Div"
colnames(brazilseriea_picks)[2] <- "picks_Home"
colnames(brazilseriea_picks)[3] <- "picks_Away"
brazilseriea_picks$matchid <- paste(brazilseriea_picks$picks_Home,brazilseriea_picks$picks_Away,sep = "-")
############################################################################################
#end of BRAZILSERIEA
brazilseriea_picks
#############################################################################################################################################################################
#clone fixtures
BRAZILSERIEA_fixtures_clone <- BRAZILSERIEA_fixtures
colnames(BRAZILSERIEA_fixtures_clone)[61] <- "Hwin"
colnames(BRAZILSERIEA_fixtures_clone)[62] <- "Draw"
colnames(BRAZILSERIEA_fixtures_clone)[63] <- "Awin"

BRAZILSERIEA_fixtures_clone$Hwinodds <-   BRAZILSERIEA_fixtures$brazilseriea_1_0 + BRAZILSERIEA_fixtures$brazilseriea_2_0 + BRAZILSERIEA_fixtures$brazilseriea_2_1 + BRAZILSERIEA_fixtures$brazilseriea_3_0 + BRAZILSERIEA_fixtures$brazilseriea_3_1 +
  BRAZILSERIEA_fixtures$brazilseriea_3_2 + BRAZILSERIEA_fixtures$brazilseriea_4_0 + BRAZILSERIEA_fixtures$brazilseriea_4_1 + BRAZILSERIEA_fixtures$brazilseriea_4_2 + BRAZILSERIEA_fixtures$brazilseriea_4_3 +
  BRAZILSERIEA_fixtures$brazilseriea_5_0 + BRAZILSERIEA_fixtures$brazilseriea_5_1 + BRAZILSERIEA_fixtures$brazilseriea_5_2 + BRAZILSERIEA_fixtures$brazilseriea_5_3 + BRAZILSERIEA_fixtures$brazilseriea_5_4 +
  BRAZILSERIEA_fixtures$brazilseriea_6_0 + BRAZILSERIEA_fixtures$brazilseriea_6_1 + BRAZILSERIEA_fixtures$brazilseriea_6_2 + BRAZILSERIEA_fixtures$brazilseriea_6_3 + BRAZILSERIEA_fixtures$brazilseriea_6_4 +
  BRAZILSERIEA_fixtures$brazilseriea_6_5
BRAZILSERIEA_fixtures_clone$Hwinodds <- round(1/BRAZILSERIEA_fixtures_clone$Hwinodds, digits = 3)

BRAZILSERIEA_fixtures_clone$Drawodds <-  BRAZILSERIEA_fixtures$brazilseriea_0_0 + BRAZILSERIEA_fixtures$brazilseriea_1_1 + BRAZILSERIEA_fixtures$brazilseriea_2_2 + BRAZILSERIEA_fixtures$brazilseriea_3_3 + BRAZILSERIEA_fixtures$brazilseriea_4_4 +
  BRAZILSERIEA_fixtures$brazilseriea_5_5 + BRAZILSERIEA_fixtures$brazilseriea_6_6

BRAZILSERIEA_fixtures_clone$Drawodds <- round(1/BRAZILSERIEA_fixtures_clone$Drawodds, digits = 3)

BRAZILSERIEA_fixtures_clone$Awinodds <-   BRAZILSERIEA_fixtures$brazilseriea_0_1 + BRAZILSERIEA_fixtures$brazilseriea_0_2 + BRAZILSERIEA_fixtures$brazilseriea_1_2 + BRAZILSERIEA_fixtures$brazilseriea_0_3 + BRAZILSERIEA_fixtures$brazilseriea_1_3 +
  BRAZILSERIEA_fixtures$brazilseriea_2_3 + BRAZILSERIEA_fixtures$brazilseriea_0_4 + BRAZILSERIEA_fixtures$brazilseriea_1_4 + BRAZILSERIEA_fixtures$brazilseriea_2_4 + BRAZILSERIEA_fixtures$brazilseriea_3_4 +
  BRAZILSERIEA_fixtures$brazilseriea_0_5 + BRAZILSERIEA_fixtures$brazilseriea_1_5 + BRAZILSERIEA_fixtures$brazilseriea_2_5 + BRAZILSERIEA_fixtures$brazilseriea_3_5 + BRAZILSERIEA_fixtures$brazilseriea_4_5 +
  BRAZILSERIEA_fixtures$brazilseriea_0_6 + BRAZILSERIEA_fixtures$brazilseriea_1_6 + BRAZILSERIEA_fixtures$brazilseriea_2_6 + BRAZILSERIEA_fixtures$brazilseriea_3_6 + BRAZILSERIEA_fixtures$brazilseriea_4_6 +
  BRAZILSERIEA_fixtures$brazilseriea_5_6

BRAZILSERIEA_fixtures_clone$Awinodds <- round(1/BRAZILSERIEA_fixtures_clone$Awinodds, digits = 3)

colnames(BRAZILSERIEA_fixtures_clone)[15] <- "CS_1-1"
colnames(BRAZILSERIEA_fixtures_clone)[13] <- "CS_1-0"
colnames(BRAZILSERIEA_fixtures_clone)[14] <- "CS_0-1"
colnames(BRAZILSERIEA_fixtures_clone)[16] <- "CS_2-0"
colnames(BRAZILSERIEA_fixtures_clone)[17] <- "CS_0-2"
colnames(BRAZILSERIEA_fixtures_clone)[19] <- "CS_2-1"
colnames(BRAZILSERIEA_fixtures_clone)[20] <- "CS_1-2"

BRAZILSERIEA_fixtures_clone$`CS_1-1` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_1-1`, digits = 3)
BRAZILSERIEA_fixtures_clone$`CS_1-0` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_1-0`, digits = 3)
BRAZILSERIEA_fixtures_clone$`CS_0-1` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_0-1`, digits = 3)
BRAZILSERIEA_fixtures_clone$`CS_2-0` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_2-0`, digits = 3)
BRAZILSERIEA_fixtures_clone$`CS_0-2` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_0-2`, digits = 3)
BRAZILSERIEA_fixtures_clone$`CS_2-1` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_2-1`, digits = 3)
BRAZILSERIEA_fixtures_clone$`CS_1-2` <- round(1/BRAZILSERIEA_fixtures_clone$`CS_1-2`, digits = 3)

colnames(BRAZILSERIEA_fixtures_clone)[1] <- "league"
colnames(BRAZILSERIEA_fixtures_clone)[2] <- "Hometeam"
colnames(BRAZILSERIEA_fixtures_clone)[3] <- "Awayteam"
colnames(BRAZILSERIEA_fixtures_clone)[92] <- "predscore"
colnames(BRAZILSERIEA_fixtures_clone)[64] <- "ov25"
colnames(BRAZILSERIEA_fixtures_clone)[66] <- "ov25odds"
colnames(BRAZILSERIEA_fixtures_clone)[65] <- "un25"
colnames(BRAZILSERIEA_fixtures_clone)[67] <- "un25odds"
colnames(BRAZILSERIEA_fixtures_clone)[68] <- "BTTSY"
colnames(BRAZILSERIEA_fixtures_clone)[69] <- "BTTSN"
colnames(BRAZILSERIEA_fixtures_clone)[70] <- "BTTSYodds"
colnames(BRAZILSERIEA_fixtures_clone)[71] <- "BTTSNodds"

BRAZILSERIEA_fixtures_clone <- BRAZILSERIEA_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
BRAZILSERIEA_fixtures_clone$matchid <- paste(BRAZILSERIEA_fixtures_clone$Hometeam,BRAZILSERIEA_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
BRAZILSERIEA_fixtures_clone_final <- BRAZILSERIEA_fixtures_clone[,-c(8,9,10,27)]
BRAZILSERIEA_fixtures_clone_final[,'sep'] <- ''

brazilseriea_dmprediction <-  brazilseriea_picks[,c(4,5,6,7,8)]
brazilseriea_dmprediction[,'sep2'] <- ''



brazilseriea_goals <- BRAZILSERIEA_fixtures[,c(10,11)]
brazilseriea_goals$brazilseriea_xGH <- round(brazilseriea_goals$brazilseriea_xGH, digits = 2)
brazilseriea_goals$brazilseriea_xGA <- round(brazilseriea_goals$brazilseriea_xGA, digits = 2)
brazilseriea_goals$brazilseriea_TxG <- brazilseriea_goals$brazilseriea_xGH + brazilseriea_goals$brazilseriea_xGA
brazilseriea_goals[,'sep5'] <- ''

BRAZILSERIEA_all <- cbind(BRAZILSERIEA_fixtures_clone_final,brazilseriea_dmprediction,brazilseriea_goals)
unlink('NL/BRAZILSERIEA.xlsx')
write.xlsx(BRAZILSERIEA_all,'NL/BRAZILSERIEA.xlsx', sheetName = "BRAZILSERIEA_all", append = TRUE)
write.xlsx(points_brazilseriea,'NL/BRAZILSERIEA.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(brazilseriea_goaltotalsv2,'NL/BRAZILSERIEA.xlsx', sheetName = "Goaltotals", append = TRUE)


