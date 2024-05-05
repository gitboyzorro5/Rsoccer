#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
sp1_currentround

#first_df <- D1_rounds[D1_rounds$d1_matchday > 25,]
#second_df <- F1_rounds[F1_rounds$f1_matchday > 25,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#LALIGA <- rbind(first_df,second_df)
LALIGA <- SP1_rounds[SP1_rounds$sp1_matchday > 27,]
#LALIGA <- na.omit(LALIGA)
#goaltotals v2
laliga_goaltotalsv2 <- tapply(LALIGA$TG, LALIGA[c("HomeTeam", "AwayTeam")],mean)
laliga_hgtotals <- rowSums(laliga_goaltotalsv2, na.rm = T)
laliga_agtotals <- colSums(laliga_goaltotalsv2, na.rm = T)
laliga_goaltotalsv2 <- cbind(laliga_goaltotalsv2,laliga_hgtotals,laliga_agtotals)
laliga_totalgoals <- laliga_hgtotals + laliga_agtotals
laliga_goaltotalsv2 <- cbind(laliga_goaltotalsv2,laliga_totalgoals)
laliga_teams <- sort(unique(LALIGA$HomeTeam))
laliga_home_games <- c()
laliga_away_games <-c()
for (i_laliga in 1:length(laliga_teams))
{

  laliga_home_games[i_laliga] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga],])
  laliga_away_games[i_laliga]  <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga],])

}
laliga_games_played <- laliga_home_games + laliga_away_games
laliga_goaltotalsv2 <- cbind(laliga_goaltotalsv2,laliga_games_played)
laliga_avg_totalgoals <- round((laliga_totalgoals/ laliga_games_played), digits = 4)
laliga_goaltotalsv2[is.na(laliga_goaltotalsv2)] <- ""
laliga_goaltotalsv2 <- cbind(laliga_goaltotalsv2,laliga_avg_totalgoals)

############################################################################################################
#Cornertotals v2
laliga_cornertotalsv2 <- tapply(LALIGA$TC, LALIGA[c("HomeTeam", "AwayTeam")],mean)
laliga_hcototals <- rowSums(laliga_cornertotalsv2, na.rm = T)
laliga_acototals <- colSums(laliga_cornertotalsv2, na.rm = T)
laliga_cornertotalsv2 <- cbind(laliga_cornertotalsv2,laliga_hcototals,laliga_acototals)
laliga_totalcorners <- laliga_hcototals + laliga_acototals
laliga_cornertotalsv2 <- cbind(laliga_cornertotalsv2,laliga_totalcorners)
laliga_cornertotalsv2 <- cbind(laliga_cornertotalsv2,laliga_games_played)
laliga_avg_totalcorners <- round((laliga_totalcorners/ laliga_games_played), digits = 4)
laliga_cornertotalsv2[is.na(laliga_cornertotalsv2)] <- ""
laliga_cornertotalsv2 <- cbind(laliga_cornertotalsv2,laliga_avg_totalcorners)
############################################################################################################
#GS matrix
laliga_goalscored_h <- tapply(LALIGA$FTHG, LALIGA[c("HomeTeam", "Date")],mean)
laliga_goalscored_a <- tapply(LALIGA$FTAG, LALIGA[c("AwayTeam", "Date")],mean)
laliga_goalscored_h[is.na(laliga_goalscored_h)] <- ""
laliga_goalscored_a[is.na(laliga_goalscored_a)] <- ""
for(laliga_rowhgs in 1:nrow(laliga_goalscored_h)) {
  for(laliga_colhgs in 1:ncol(laliga_goalscored_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowags in 1:nrow(laliga_goalscored_a)) {
      for(laliga_colags in 1:ncol(laliga_goalscored_a)) {
        ifelse(!laliga_goalscored_a[laliga_rowags,laliga_colags]=="",laliga_goalscored_h[laliga_rowags,laliga_colags] <- laliga_goalscored_a[laliga_rowags,laliga_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
laliga_goalconceded_h <- tapply(LALIGA$FTAG, LALIGA[c("HomeTeam", "Date")],mean)
laliga_goalconceded_a <- tapply(LALIGA$FTHG, LALIGA[c("AwayTeam", "Date")],mean)
laliga_goalconceded_h[is.na(laliga_goalconceded_h)] <- ""
laliga_goalconceded_a[is.na(laliga_goalconceded_a)] <- ""
for(laliga_rowhgc in 1:nrow(laliga_goalconceded_h)) {
  for(laliga_colhgc in 1:ncol(laliga_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowagc in 1:nrow(laliga_goalconceded_a)) {
      for(laliga_colagc in 1:ncol(laliga_goalconceded_a)) {
        ifelse(!laliga_goalconceded_a[laliga_rowagc,laliga_colagc]=="",laliga_goalconceded_h[laliga_rowagc,laliga_colagc] <- laliga_goalconceded_a[laliga_rowagc,laliga_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
laliga_totalcorners_h <- tapply(LALIGA$TC, LALIGA[c("HomeTeam", "Date")],mean)
laliga_totalcorners_a <- tapply(LALIGA$TC, LALIGA[c("AwayTeam", "Date")],mean)
laliga_totalcorners_h[is.na(laliga_totalcorners_h)] <- ""
laliga_totalcorners_a[is.na(laliga_totalcorners_a)] <- ""
#LALIGA
for(laliga_rowTC in 1:nrow(laliga_totalcorners_h)) {
  for(laliga_colTC in 1:ncol(laliga_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowTC in 1:nrow(laliga_totalcorners_a)) {
      for(laliga_colTC in 1:ncol(laliga_totalcorners_a)) {
        ifelse(!laliga_totalcorners_a[laliga_rowTC,laliga_colTC]=="",laliga_totalcorners_h[laliga_rowTC,laliga_colTC] <- laliga_totalcorners_a[laliga_rowTC,laliga_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
laliga_coawarded_h <- tapply(LALIGA$HCO, LALIGA[c("HomeTeam", "Date")],mean)
laliga_coawarded_a <- tapply(LALIGA$ACO, LALIGA[c("AwayTeam", "Date")],mean)
laliga_coawarded_h[is.na(laliga_coawarded_h)] <- ""
laliga_coawarded_a[is.na(laliga_coawarded_a)] <- ""
#LALIGA
for(laliga_rowhco in 1:nrow(laliga_coawarded_h)) {
  for(laliga_colhco in 1:ncol(laliga_coawarded_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowaco in 1:nrow(laliga_coawarded_a)) {
      for(laliga_colaco in 1:ncol(laliga_coawarded_a)) {
        ifelse(!laliga_coawarded_a[laliga_rowaco,laliga_colaco]=="",laliga_coawarded_h[laliga_rowaco,laliga_colaco] <- laliga_coawarded_a[laliga_rowaco,laliga_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
laliga_cornersconceded_h <- tapply(LALIGA$ACO, LALIGA[c("HomeTeam", "Date")],mean)
laliga_cornersconceded_a <- tapply(LALIGA$HCO, LALIGA[c("AwayTeam", "Date")],mean)
laliga_cornersconceded_h[is.na(laliga_cornersconceded_h)] <- ""
laliga_cornersconceded_a[is.na(laliga_cornersconceded_a)] <- ""
#LALIGA
for(laliga_rowhcc in 1:nrow(laliga_cornersconceded_h)) {
  for(laliga_colhcc in 1:ncol(laliga_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowacc in 1:nrow(laliga_cornersconceded_a)) {
      for(laliga_colacc in 1:ncol(laliga_cornersconceded_a)) {
        ifelse(!laliga_cornersconceded_a[laliga_rowacc,laliga_colacc]=="",laliga_cornersconceded_h[laliga_rowacc,laliga_colacc] <- laliga_cornersconceded_a[laliga_rowacc,laliga_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
laliga_coscform_h <- tapply(LALIGA$COSC, LALIGA[c("HomeTeam", "Date")],median)
laliga_coscform_a <- tapply(LALIGA$COSC, LALIGA[c("AwayTeam", "Date")],median)
laliga_coscform_h[is.na(laliga_coscform_h)] <- ""
laliga_coscform_a[is.na(laliga_coscform_a)] <- ""
#LALIGA
for(laliga_rowh_f_cosc in 1:nrow(laliga_coscform_h)) {
  for(laliga_colh_f_cosc in 1:ncol(laliga_coscform_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowa_f_cosc in 1:nrow(laliga_coscform_a)) {
      for(laliga_cola_f_cosc in 1:ncol(laliga_coscform_a)) {
        ifelse(!laliga_coscform_a[laliga_rowa_f_cosc,laliga_cola_f_cosc]=="",laliga_coscform_h[laliga_rowa_f_cosc,laliga_cola_f_cosc] <- laliga_coscform_a[laliga_rowa_f_cosc,laliga_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
laliga_winmargin_h <- tapply(LALIGA$FTHG - LALIGA$FTAG, LALIGA[c("HomeTeam", "Date")],mean)
laliga_winmargin_a <- tapply(LALIGA$FTAG - LALIGA$FTHG, LALIGA[c("AwayTeam", "Date")],mean)
laliga_winmargin_h[is.na(laliga_winmargin_h)] <- ""
laliga_winmargin_a[is.na(laliga_winmargin_a)] <- ""
#LALIGA
for(laliga_rowhwm in 1:nrow(laliga_winmargin_h)) {
  for(laliga_colhwm in 1:ncol(laliga_winmargin_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowawm in 1:nrow(laliga_winmargin_a)) {
      for(laliga_colawm in 1:ncol(laliga_winmargin_a)) {
        ifelse(!laliga_winmargin_a[laliga_rowawm,laliga_colawm]=="",laliga_winmargin_h[laliga_rowawm,laliga_colawm] <- laliga_winmargin_a[laliga_rowawm,laliga_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
laliga_yellowscored_h <- tapply(LALIGA$HY, LALIGA[c("HomeTeam", "Date")],mean)
laliga_yellowscored_a <- tapply(LALIGA$AY, LALIGA[c("AwayTeam", "Date")],mean)
laliga_yellowscored_h[is.na(laliga_yellowscored_h)] <- ""
laliga_yellowscored_a[is.na(laliga_yellowscored_a)] <- ""
#LALIGA
for(laliga_rowhys in 1:nrow(laliga_yellowscored_h)) {
  for(laliga_colhys in 1:ncol(laliga_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(laliga_roways in 1:nrow(laliga_yellowscored_a)) {
      for(laliga_colays in 1:ncol(laliga_yellowscored_a)) {
        ifelse(!laliga_yellowscored_a[laliga_roways,laliga_colays]=="",laliga_yellowscored_h[laliga_roways,laliga_colays] <- laliga_yellowscored_a[laliga_roways,laliga_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
laliga_redscored_h <- tapply(LALIGA$HR, LALIGA[c("HomeTeam", "Date")],mean)
laliga_redscored_a <- tapply(LALIGA$AR, LALIGA[c("AwayTeam", "Date")],mean)
laliga_redscored_h[is.na(laliga_redscored_h)] <- ""
laliga_redscored_a[is.na(laliga_redscored_a)] <- ""
for(laliga_rowhrs in 1:nrow(laliga_redscored_h)) {
  for(laliga_colhrs in 1:ncol(laliga_redscored_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowars in 1:nrow(laliga_redscored_a)) {
      for(laliga_colars in 1:ncol(laliga_redscored_a)) {
        ifelse(!laliga_redscored_a[laliga_rowars,laliga_colars]=="",laliga_redscored_h[laliga_rowars,laliga_colars] <- laliga_redscored_a[laliga_rowars,laliga_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
laliga_redtotalsv2 <- tapply(LALIGA$TR, LALIGA[c("HomeTeam", "AwayTeam")],mean)
laliga_hrtotals <- rowSums(laliga_redtotalsv2, na.rm = T)
laliga_artotals <- colSums(laliga_redtotalsv2, na.rm = T)
laliga_redtotalsv2 <- cbind(laliga_redtotalsv2,laliga_hrtotals,laliga_artotals)
laliga_totalreds <- laliga_hrtotals + laliga_artotals
laliga_redtotalsv2 <- cbind(laliga_redtotalsv2,laliga_totalreds)
laliga_redtotalsv2 <- cbind(laliga_redtotalsv2,laliga_games_played)
laliga_avg_totalreds <- round((laliga_totalreds/ laliga_games_played), digits = 4)
laliga_redtotalsv2[is.na(laliga_redtotalsv2)] <- ""
laliga_redtotalsv2 <- cbind(laliga_redtotalsv2,laliga_avg_totalreds)
############################################################################################################################################################
#yellowtotals
laliga_yellowtotalsv2 <- tapply(LALIGA$TY, LALIGA[c("HomeTeam", "AwayTeam")],mean)
laliga_hytotals <- rowSums(laliga_yellowtotalsv2, na.rm = T)
laliga_aytotals <- colSums(laliga_yellowtotalsv2, na.rm = T)
laliga_yellowtotalsv2 <- cbind(laliga_yellowtotalsv2,laliga_hytotals,laliga_aytotals)
laliga_totalyellows <- laliga_hytotals + laliga_aytotals
laliga_yellowtotalsv2 <- cbind(laliga_yellowtotalsv2,laliga_totalyellows)
laliga_yellowtotalsv2 <- cbind(laliga_yellowtotalsv2,laliga_games_played)
laliga_avg_totalyellows <- round((laliga_totalyellows/ laliga_games_played), digits = 4)
laliga_yellowtotalsv2[is.na(laliga_yellowtotalsv2)] <- ""
laliga_yellowtotalsv2 <- cbind(laliga_yellowtotalsv2,laliga_avg_totalyellows)
##################################################################################################################################################
#team form
laliga_form_h <- tapply(LALIGA$FTR, LALIGA[c("HomeTeam", "Date")],median)
laliga_form_a <- tapply(LALIGA$FTR, LALIGA[c("AwayTeam", "Date")],median)
laliga_form_h[is.na(laliga_form_h)] <- ""
laliga_form_a[is.na(laliga_form_a)] <- ""
laliga_form_h <- sub("A","L",laliga_form_h)
laliga_form_h <- sub("H","W",laliga_form_h)
laliga_form_a <- sub("A","W",laliga_form_a)
laliga_form_a <- sub("H","L",laliga_form_a)
for(laliga_rowh_f in 1:nrow(laliga_form_h)) {
  for(laliga_colh_f in 1:ncol(laliga_form_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowa_f in 1:nrow(laliga_form_a)) {
      for(laliga_cola_f in 1:ncol(laliga_form_a)) {
        ifelse(!laliga_form_a[laliga_rowa_f,laliga_cola_f]=="",laliga_form_h[laliga_rowa_f,laliga_cola_f] <- laliga_form_a[laliga_rowa_f,laliga_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
laliga_csform_h <- tapply(LALIGA$CS, LALIGA[c("HomeTeam", "Date")],median)
laliga_csform_a <- tapply(LALIGA$CS, LALIGA[c("AwayTeam", "Date")],median)
laliga_csform_h[is.na(laliga_csform_h)] <- ""
laliga_csform_a[is.na(laliga_csform_a)] <- ""
#LALIGA
for(laliga_rowh_f_cs in 1:nrow(laliga_csform_h)) {
  for(laliga_colh_f_cs in 1:ncol(laliga_csform_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowa_f_cs in 1:nrow(laliga_csform_a)) {
      for(laliga_cola_f_cs in 1:ncol(laliga_csform_a)) {
        ifelse(!laliga_csform_a[laliga_rowa_f_cs,laliga_cola_f_cs]=="",laliga_csform_h[laliga_rowa_f_cs,laliga_cola_f_cs] <- laliga_csform_a[laliga_rowa_f_cs,laliga_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
laliga_totalgoals_h <- tapply(LALIGA$TG, LALIGA[c("HomeTeam", "Date")],mean)
laliga_totalgoals_a <- tapply(LALIGA$TG, LALIGA[c("AwayTeam", "Date")],mean)
laliga_totalgoals_h[is.na(laliga_totalgoals_h)] <- ""
laliga_totalgoals_a[is.na(laliga_totalgoals_a)] <- ""
for(laliga_rowh in 1:nrow(laliga_totalgoals_h)) {
  for(laliga_colh in 1:ncol(laliga_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowa in 1:nrow(laliga_totalgoals_a)) {
      for(laliga_cola in 1:ncol(laliga_totalgoals_a)) {
        ifelse(!laliga_totalgoals_a[laliga_rowa,laliga_cola]=="",laliga_totalgoals_h[laliga_rowa,laliga_cola] <- laliga_totalgoals_a[laliga_rowa,laliga_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#LALIGA
laliga_un05_home <- c()
laliga_un05_away <- c()
laliga_ov05_home <- c()
laliga_ov05_away <- c()

laliga_un15_home <- c()
laliga_un15_away <- c()
laliga_ov15_home <- c()
laliga_ov15_away <- c()

laliga_un25_home <- c()
laliga_un25_away <- c()
laliga_ov25_home <- c()
laliga_ov25_away <- c()

laliga_un35_home <- c()
laliga_un35_away <- c()
laliga_ov35_home <- c()
laliga_ov35_away <- c()

laliga_un45_home <- c()
laliga_un45_away <- c()
laliga_ov45_home <- c()
laliga_ov45_away <- c()

laliga_un55_home <- c()
laliga_un55_away <- c()
laliga_ov55_home <- c()
laliga_ov55_away <- c()

for (i_laliga_tg in 1:length(laliga_teams))
{

  laliga_un05_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG == 0,])
  laliga_un05_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG == 0,])

  laliga_ov05_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG > 0,])
  laliga_ov05_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG > 0,])

  laliga_un15_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 1,])
  laliga_un15_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 1,])

  laliga_ov15_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 2,])
  laliga_ov15_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 2,])

  laliga_un25_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 2,])
  laliga_un25_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 2,])

  laliga_ov25_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >=3,])
  laliga_ov25_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >=3,])

  laliga_un35_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 3,])
  laliga_un35_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 3,])

  laliga_ov35_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 4,])
  laliga_ov35_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 4,])

  laliga_un45_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 4,])
  laliga_un45_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 4,])

  laliga_ov45_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 5,])
  laliga_ov45_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 5,])

  laliga_un55_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 5,])
  laliga_un55_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG <= 5,])

  laliga_ov55_home[i_laliga_tg] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 6,])
  laliga_ov55_away[i_laliga_tg] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_tg] & LALIGA$TG >= 6,])


}

laliga_un05 <- laliga_un05_home + laliga_un05_away
laliga_ov05 <- laliga_ov05_home + laliga_ov05_away

laliga_un15 <- laliga_un15_home + laliga_un15_away
laliga_ov15 <- laliga_ov15_home + laliga_ov15_away

laliga_un25 <- laliga_un25_home + laliga_un25_away
laliga_ov25 <- laliga_ov25_home + laliga_ov25_away

laliga_un35 <- laliga_un35_home + laliga_un35_away
laliga_ov35 <- laliga_ov35_home + laliga_ov35_away

laliga_un45 <- laliga_un45_home + laliga_un45_away
laliga_ov45 <- laliga_ov45_home + laliga_ov45_away

laliga_un55 <- laliga_un55_home + laliga_un55_away
laliga_ov55 <- laliga_ov55_home + laliga_ov55_away

laliga_ovundata <- cbind(laliga_teams,laliga_un05,laliga_ov05,laliga_un15,laliga_ov15,laliga_un25,laliga_ov25,laliga_un35,laliga_ov35,laliga_un45,laliga_ov45,laliga_un55,laliga_ov55)
#################################################################################################################################################################
#team against
laliga_form_team_against_h <- tapply(LALIGA$AwayTeam, LALIGA[c("HomeTeam", "Date")],median)
laliga_form_team_against_a <- tapply(LALIGA$HomeTeam, LALIGA[c("AwayTeam", "Date")],median)
laliga_form_team_against_h[is.na(laliga_form_team_against_h)] <- ""
laliga_form_team_against_a[is.na(laliga_form_team_against_a)] <- ""
#LALIGA
for(laliga_rowh_f_against in 1:nrow(laliga_form_team_against_h)) {
  for(laliga_colh_f_against in 1:ncol(laliga_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(laliga_rowa_f_against in 1:nrow(laliga_form_team_against_a)) {
      for(laliga_cola_f_against in 1:ncol(laliga_form_team_against_a)) {
        ifelse(!laliga_form_team_against_a[laliga_rowa_f_against,laliga_cola_f_against]=="",laliga_form_team_against_h[laliga_rowa_f_against,laliga_cola_f_against] <- laliga_form_team_against_a[laliga_rowa_f_against,laliga_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#LALIGA
#home goals scored
laliga_home_gs <- aggregate(LALIGA$FTHG, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_home_gs_avg <- aggregate(LALIGA$FTHG, by = list(LALIGA$HomeTeam),mean)
laliga_home_scoring <- merge(laliga_home_gs,laliga_home_gs_avg, by='Group.1',all = T)
names(laliga_home_scoring)[names(laliga_home_scoring) == "x.x"] <- "TFthg"
names(laliga_home_scoring)[names(laliga_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
laliga_away_gs <- aggregate(LALIGA$FTAG, by = list(LALIGA$AwayTeam), FUN = sum)
laliga_away_gs_avg <- aggregate(LALIGA$FTAG, by = list(LALIGA$AwayTeam),mean)
laliga_away_scoring <- merge(laliga_away_gs,laliga_away_gs_avg, by='Group.1',all = T)
names(laliga_away_scoring)[names(laliga_away_scoring) == "x.x"] <- "TFtag"
names(laliga_away_scoring)[names(laliga_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
laliga_scoring <- merge(laliga_home_scoring,laliga_away_scoring,by='Group.1',all = T)
laliga_scoring$TGS <- laliga_scoring$TFthg + laliga_scoring$TFtag

#Home shots on target
laliga_home_hst <- aggregate(LALIGA$HST, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_ast <- aggregate(LALIGA$AST, by = list(LALIGA$AwayTeam), FUN = sum)
laliga_tst <- merge(laliga_home_hst,laliga_away_ast, by='Group.1',all = T)
names(laliga_tst)[names(laliga_tst) == "x.x"] <- "hst"
names(laliga_tst)[names(laliga_tst) == "x.y"] <- "ast"
laliga_tst$TST <- laliga_tst$hst + laliga_tst$ast
#merge goals scored and shots on target
laliga_scoring_conversion <- merge(laliga_tst,laliga_scoring,by='Group.1',all = T)
#add HSC ASC TSC
laliga_scoring_conversion$HSTC <- percent(laliga_scoring_conversion$TFthg/laliga_scoring_conversion$hst, accuracy = 0.01)
laliga_scoring_conversion$ASTC <- percent(laliga_scoring_conversion$TFtag/laliga_scoring_conversion$ast, accuracy = 0.01)
laliga_scoring_conversion$TSTC <- percent(laliga_scoring_conversion$TGS/laliga_scoring_conversion$TST, accuracy = 0.01)
#merge games played
laliga_scoring_conversion <- cbind(laliga_scoring_conversion,laliga_games_played)
#create the second part
#home goals conceded
laliga_home_gc <- aggregate(LALIGA$FTAG, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_home_gc_avg <- aggregate(LALIGA$FTAG, by = list(LALIGA$HomeTeam),mean)
laliga_home_conceding <- merge(laliga_home_gc,laliga_home_gc_avg, by='Group.1',all = T)
names(laliga_home_conceding)[names(laliga_home_conceding) == "x.x"] <- "TFthc"
names(laliga_home_conceding)[names(laliga_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
laliga_away_gc <- aggregate(LALIGA$FTHG, by = list(LALIGA$AwayTeam), FUN = sum)
laliga_away_gc_avg <- aggregate(LALIGA$FTHG, by = list(LALIGA$AwayTeam),mean)
laliga_away_conceding <- merge(laliga_away_gc,laliga_away_gc_avg, by='Group.1',all = T)
names(laliga_away_conceding)[names(laliga_away_conceding) == "x.x"] <- "TFtac"
names(laliga_away_conceding)[names(laliga_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
laliga_conceding <- merge(laliga_home_conceding,laliga_away_conceding,by='Group.1',all = T)
laliga_conceding$TGC <- laliga_conceding$TFthc + laliga_conceding$TFtac
laliga_home_hst
#Home shots conceded
laliga_home_hsc <- aggregate(LALIGA$AST, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_asc <- aggregate(LALIGA$HST, by = list(LALIGA$AwayTeam), FUN = sum)
laliga_tsc <- merge(laliga_home_hsc,laliga_away_asc, by='Group.1',all = T)
names(laliga_tsc)[names(laliga_tsc) == "x.x"] <- "hsc"
names(laliga_tsc)[names(laliga_tsc) == "x.y"] <- "asc"
laliga_tsc$TSC <- laliga_tsc$hsc + laliga_tsc$asc
#merge goals conceded and shots conceded
laliga_conceding_conversion <- merge(laliga_tsc,laliga_conceding,by='Group.1',all = T)

#add HSC ASC TSC
laliga_conceding_conversion$HSCC <- percent(laliga_conceding_conversion$TFthc/laliga_conceding_conversion$hsc, accuracy = 0.01)
laliga_conceding_conversion$ASCC <- percent(laliga_conceding_conversion$TFtac/laliga_conceding_conversion$asc, accuracy = 0.01)
laliga_conceding_conversion$TSCC <- percent(laliga_conceding_conversion$TGC/laliga_conceding_conversion$TSC, accuracy = 0.01)
laliga_conceding_conversion$XSTC <- round(laliga_scoring$TGS/(laliga_tst$TST - laliga_scoring$TGS), digits = 2)

#merge the two parts
laliga_shots_analysis <- merge(laliga_scoring_conversion,laliga_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#LALIGA
#home fouls for
laliga_home_fouls <- aggregate(LALIGA$HF, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_home_fouls_avg <- aggregate(LALIGA$HF, by = list(LALIGA$HomeTeam),mean)
laliga_home_foulsdata <- merge(laliga_home_fouls,laliga_home_fouls_avg, by='Group.1',all = T)
names(laliga_home_foulsdata)[names(laliga_home_foulsdata) == "x.x"] <- "THfouls"
names(laliga_home_foulsdata)[names(laliga_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
laliga_away_fouls <- aggregate(LALIGA$HF, by = list(LALIGA$AwayTeam), FUN = sum)
laliga_away_fouls_avg <- aggregate(LALIGA$HF, by = list(LALIGA$AwayTeam),mean)
laliga_away_foulsdata <- merge(laliga_away_fouls,laliga_away_fouls_avg, by='Group.1',all = T)
names(laliga_away_foulsdata)[names(laliga_away_foulsdata) == "x.x"] <- "TAfouls"
names(laliga_away_foulsdata)[names(laliga_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
laliga_fouls <- merge(laliga_home_foulsdata,laliga_away_foulsdata,by='Group.1',all = T)
laliga_fouls$TotalFouls <- laliga_fouls$THfouls + laliga_fouls$TAfouls

#yellow cards
laliga_home_hyc <- aggregate(LALIGA$HY, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_ayc <- aggregate(LALIGA$AY, by = list(LALIGA$AwayTeam), FUN = sum)
laliga_tyc <- merge(laliga_home_hyc,laliga_away_ayc, by='Group.1',all = T)
names(laliga_tyc)[names(laliga_tyc) == "x.x"] <- "hyc"
names(laliga_tyc)[names(laliga_tyc) == "x.y"] <- "ayc"
laliga_tyc$TotalYellows <- laliga_tyc$hyc + laliga_tyc$ayc

#merge fouls for and yellow cards
laliga_fouls_conversion <- merge(laliga_tyc,laliga_fouls,by='Group.1',all = T)
laliga_fouls_conversion$YcPerfoul <- round((laliga_fouls_conversion$TotalYellows/laliga_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
LALIGA$Div <- "LALIGA"
##
###################################################################################################################################################
#poisson cards
laliga_GP <- nrow(LALIGA)
#Calculate total home goals for each division
laliga_T_HY <- sum(laliga_home_hyc$x)
#calculate average home goal
laliga_avg_HY <- round(laliga_T_HY /laliga_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laliga_T_AY <- sum(laliga_away_ayc$x)
#calculate average away goal
laliga_avg_AY <- round(laliga_T_AY /laliga_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laliga_home_yas <- round(((laliga_home_hyc$x/laliga_home_games))/laliga_avg_HY, digits = 4)
#calculate away attack strength
laliga_away_yas <- round(((laliga_away_ayc$x/laliga_away_games))/laliga_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
laliga_avg_HYC <- round(laliga_T_AY /laliga_GP, digits = 4)
#avg away concede
laliga_avg_AYC <- round(laliga_T_HY /laliga_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
laliga_home_ycc <- aggregate(LALIGA$AY, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_ycc <- aggregate(LALIGA$HY, by = list(LALIGA$AwayTeam), FUN = sum)
#home defense strength
laliga_home_yds <- round(((laliga_home_ycc$x/laliga_home_games))/laliga_avg_HYC, digits = 4)
#away defense strength
laliga_away_yds <- round(((laliga_away_ycc$x/laliga_away_games))/laliga_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_home_poisson_yc <- cbind(laliga_division,laliga_teams,laliga_avg_HY,laliga_home_yas,laliga_home_yds)
#away poisson data
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_away_poisson_yc <- cbind(laliga_division,laliga_teams,laliga_avg_AY,laliga_away_yas,laliga_away_yds)
###
HomeTeam_laliga_yc <- rep(laliga_teams, each = length(laliga_teams))
AwayTeam_laliga_yc <- rep(laliga_teams, length(laliga_teams))
LALIGA_fixtures_yc <- cbind(HomeTeam_laliga_yc,AwayTeam_laliga_yc)
LALIGA_fixtures_yc <- as.data.frame(LALIGA_fixtures_yc)
LALIGA_fixtures_yc <- LALIGA_fixtures_yc[!LALIGA_fixtures_yc$HomeTeam_laliga_yc == LALIGA_fixtures_yc$AwayTeam_laliga_yc,]
rownames(LALIGA_fixtures_yc) <- NULL
LALIGA_fixtures_yc$Div <- "LALIGA"
LALIGA_fixtures_yc <- LALIGA_fixtures_yc[,c(3,1,2)]

LALIGA_fixtures_yc$avg_HY_laliga <- laliga_avg_HY

LALIGA_fixtures_yc$laliga_homeyas <- rep(laliga_home_yas,each = length(laliga_teams)-1)

laliga_awayyds_lookup <- cbind(laliga_teams,laliga_away_yds)

laliga_awayyds_lookup <- as.data.frame(laliga_awayyds_lookup)

colnames(laliga_awayyds_lookup) <- c("AwayTeam_laliga_yc","laliga_awayyds")


require('RH2')
LALIGA_fixtures_yc$laliga_awayyds <- sqldf("SELECT laliga_awayyds_lookup.laliga_awayyds FROM laliga_awayyds_lookup INNER JOIN LALIGA_fixtures_yc ON laliga_awayyds_lookup.AwayTeam_laliga_yc = LALIGA_fixtures_yc.AwayTeam_laliga_yc")

LALIGA_fixtures_yc$avg_AY_laliga <- laliga_avg_AY

laliga_awayyas_lookup <- cbind(laliga_teams,laliga_away_yas)

laliga_awayyas_lookup <- as.data.frame(laliga_awayyas_lookup)

colnames(laliga_awayyas_lookup) <- c("AwayTeam_laliga_yc","laliga_awayyas")

LALIGA_fixtures_yc$laliga_awayyas <- sqldf("SELECT laliga_awayyas_lookup.laliga_awayyas FROM laliga_awayyas_lookup INNER JOIN LALIGA_fixtures_yc ON laliga_awayyas_lookup.AwayTeam_laliga_yc = LALIGA_fixtures_yc.AwayTeam_laliga_yc")

LALIGA_fixtures_yc$laliga_homeyds <- rep(laliga_home_yds,each = length(laliga_teams)-1)

LALIGA_fixtures_yc$laliga_awayyds <- as.numeric(unlist(LALIGA_fixtures_yc$laliga_awayyds))
#xGH
LALIGA_fixtures_yc$laliga_xHYC <- LALIGA_fixtures_yc$avg_HY_laliga * LALIGA_fixtures_yc$laliga_homeyas * LALIGA_fixtures_yc$laliga_awayyds
#xGA

LALIGA_fixtures_yc$laliga_awayyas <- as.numeric(unlist(LALIGA_fixtures_yc$laliga_awayyas))

LALIGA_fixtures_yc$laliga_xAYC <- LALIGA_fixtures_yc$avg_AY_laliga * LALIGA_fixtures_yc$laliga_awayyas * LALIGA_fixtures_yc$laliga_homeyds

LALIGA_fixtures_yc$laliga_0_0 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_0 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_0_1 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_1 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_0 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_0_2 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_2 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_1 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_2 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_3 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_0 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_1 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_2 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_0_3 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_3 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_3 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_4 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_0 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_1 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_2 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_3 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_0_4 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_4 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_4 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_4 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_5 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_0 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_1 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_2 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_3 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_4 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_0_5 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_5 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_5 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_5 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_5 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_6 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_0 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(0,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_1 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(1,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_2 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(2,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_3 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(3,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_4 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(4,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_6_5 <- round(stats::dpois(6,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(5,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_0_6 <- round(stats::dpois(0,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_1_6 <- round(stats::dpois(1,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_2_6 <- round(stats::dpois(2,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_3_6 <- round(stats::dpois(3,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_4_6 <- round(stats::dpois(4,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
LALIGA_fixtures_yc$laliga_5_6 <- round(stats::dpois(5,LALIGA_fixtures_yc$laliga_xHYC) * stats::dpois(6,LALIGA_fixtures_yc$laliga_xAYC), digits = 4)
#Home win
LALIGA_fixtures_yc$laliga_H <- (
  LALIGA_fixtures_yc$laliga_1_0 + LALIGA_fixtures_yc$laliga_2_0 + LALIGA_fixtures_yc$laliga_2_1 + LALIGA_fixtures_yc$laliga_3_0 + LALIGA_fixtures_yc$laliga_3_1 +
    LALIGA_fixtures_yc$laliga_3_2 + LALIGA_fixtures_yc$laliga_4_0 + LALIGA_fixtures_yc$laliga_4_1 + LALIGA_fixtures_yc$laliga_4_2 + LALIGA_fixtures_yc$laliga_4_3 +
    LALIGA_fixtures_yc$laliga_5_0 + LALIGA_fixtures_yc$laliga_5_1 + LALIGA_fixtures_yc$laliga_5_2 + LALIGA_fixtures_yc$laliga_5_3 + LALIGA_fixtures_yc$laliga_5_4 +
    LALIGA_fixtures_yc$laliga_6_0 + LALIGA_fixtures_yc$laliga_6_1 + LALIGA_fixtures_yc$laliga_6_2 + LALIGA_fixtures_yc$laliga_6_3 + LALIGA_fixtures_yc$laliga_6_4 +
    LALIGA_fixtures_yc$laliga_6_5
)

LALIGA_fixtures_yc$laliga_H <- percent(LALIGA_fixtures_yc$laliga_H, accuracy = 0.1)

#Draw
LALIGA_fixtures_yc$laliga_D <- (

  LALIGA_fixtures_yc$laliga_0_0 + LALIGA_fixtures_yc$laliga_1_1 + LALIGA_fixtures_yc$laliga_2_2 + LALIGA_fixtures_yc$laliga_3_3 + LALIGA_fixtures_yc$laliga_4_4 +
    LALIGA_fixtures_yc$laliga_5_5 + LALIGA_fixtures_yc$laliga_6_6
)

LALIGA_fixtures_yc$laliga_D <- percent(LALIGA_fixtures_yc$laliga_D, accuracy = 0.1)

#Away

LALIGA_fixtures_yc$laliga_A <- (
  LALIGA_fixtures_yc$laliga_0_1 + LALIGA_fixtures_yc$laliga_0_2 + LALIGA_fixtures_yc$laliga_1_2 + LALIGA_fixtures_yc$laliga_0_3 + LALIGA_fixtures_yc$laliga_1_3 +
    LALIGA_fixtures_yc$laliga_2_3 + LALIGA_fixtures_yc$laliga_0_4 + LALIGA_fixtures_yc$laliga_1_4 + LALIGA_fixtures_yc$laliga_2_4 + LALIGA_fixtures_yc$laliga_3_4 +
    LALIGA_fixtures_yc$laliga_0_5 + LALIGA_fixtures_yc$laliga_1_5 + LALIGA_fixtures_yc$laliga_2_5 + LALIGA_fixtures_yc$laliga_3_5 + LALIGA_fixtures_yc$laliga_4_5 +
    LALIGA_fixtures_yc$laliga_0_6 + LALIGA_fixtures_yc$laliga_1_6 + LALIGA_fixtures_yc$laliga_2_6 + LALIGA_fixtures_yc$laliga_3_6 + LALIGA_fixtures_yc$laliga_4_6 +
    LALIGA_fixtures_yc$laliga_5_6
)

LALIGA_fixtures_yc$laliga_A <- percent(LALIGA_fixtures_yc$laliga_A, accuracy = 0.1)

#ov25
LALIGA_fixtures_yc$laliga_ov25 <- (
  LALIGA_fixtures_yc$laliga_2_1 + LALIGA_fixtures_yc$laliga_1_2 + LALIGA_fixtures_yc$laliga_2_2 + LALIGA_fixtures_yc$laliga_3_0 + LALIGA_fixtures_yc$laliga_3_1 +
    LALIGA_fixtures_yc$laliga_3_2 + LALIGA_fixtures_yc$laliga_0_3 + LALIGA_fixtures_yc$laliga_1_3 + LALIGA_fixtures_yc$laliga_2_3 + LALIGA_fixtures_yc$laliga_3_3 +
    LALIGA_fixtures_yc$laliga_4_0 + LALIGA_fixtures_yc$laliga_4_1 + LALIGA_fixtures_yc$laliga_4_2 + LALIGA_fixtures_yc$laliga_4_3 + LALIGA_fixtures_yc$laliga_0_4 +
    LALIGA_fixtures_yc$laliga_1_4 + LALIGA_fixtures_yc$laliga_2_4 + LALIGA_fixtures_yc$laliga_3_4 + LALIGA_fixtures_yc$laliga_4_4 + LALIGA_fixtures_yc$laliga_5_0 +
    LALIGA_fixtures_yc$laliga_5_1 + LALIGA_fixtures_yc$laliga_5_2 + LALIGA_fixtures_yc$laliga_5_3 + LALIGA_fixtures_yc$laliga_5_4 + LALIGA_fixtures_yc$laliga_0_5 +
    LALIGA_fixtures_yc$laliga_1_5 + LALIGA_fixtures_yc$laliga_2_5 + LALIGA_fixtures_yc$laliga_3_5 + LALIGA_fixtures_yc$laliga_4_5 + LALIGA_fixtures_yc$laliga_5_5 +
    LALIGA_fixtures_yc$laliga_6_0 + LALIGA_fixtures_yc$laliga_6_1 + LALIGA_fixtures_yc$laliga_6_2 + LALIGA_fixtures_yc$laliga_6_3 + LALIGA_fixtures_yc$laliga_6_4 +
    LALIGA_fixtures_yc$laliga_6_5 + LALIGA_fixtures_yc$laliga_0_6 + LALIGA_fixtures_yc$laliga_1_6 + LALIGA_fixtures_yc$laliga_2_6 + LALIGA_fixtures_yc$laliga_3_6 +
    LALIGA_fixtures_yc$laliga_4_6 + LALIGA_fixtures_yc$laliga_5_6 + LALIGA_fixtures_yc$laliga_6_6
)
#un25
LALIGA_fixtures_yc$laliga_un25 <- (
  LALIGA_fixtures_yc$laliga_0_0 + LALIGA_fixtures_yc$laliga_1_0 + LALIGA_fixtures_yc$laliga_0_1 + LALIGA_fixtures_yc$laliga_1_1 + LALIGA_fixtures_yc$laliga_2_0 + LALIGA_fixtures_yc$laliga_0_2
)
#odds
LALIGA_fixtures_yc$laliga_ov25_odds <- round((1/LALIGA_fixtures_yc$laliga_ov25),digits = 2)
LALIGA_fixtures_yc$laliga_un25_odds <- round((1/LALIGA_fixtures_yc$laliga_un25),digits = 2)

LALIGA_fixtures_yc$laliga_ov25_odds
LALIGA_fixtures_yc$laliga_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGA_fixtures_yc$laliga_ov25 <- percent(LALIGA_fixtures_yc$laliga_ov25, accuracy = 0.1)

LALIGA_fixtures_yc$laliga_un25 <- percent(LALIGA_fixtures_yc$laliga_un25, accuracy = 0.1)
LALIGA_fixtures_yc$laliga_pscore <- paste(round(LALIGA_fixtures_yc$laliga_xHYC,digits = 0),round(LALIGA_fixtures_yc$laliga_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
laliga_GP <- nrow(LALIGA)
#Calculate total home corners for each division
laliga_home_corners <- aggregate(LALIGA$HCO, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_corners <- aggregate(LALIGA$ACO, by = list(LALIGA$AwayTeam), FUN = sum)
###############################################################################
laliga_T_HCO <- sum(laliga_home_corners$x)
#calculate average home corners
laliga_avg_HCO <- round(laliga_T_HCO /laliga_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laliga_T_ACO <- sum(laliga_away_corners$x)
#calculate average away goal
laliga_avg_ACO <- round(laliga_T_ACO /laliga_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laliga_home_coas <- round(((laliga_home_corners$x/laliga_home_games))/laliga_avg_HCO, digits = 4)
#calculate away attack strength
laliga_away_coas <- round(((laliga_away_corners$x/laliga_away_games))/laliga_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
laliga_avg_HCOC <- round(laliga_T_ACO /laliga_GP, digits = 4)
#avg away concede
laliga_avg_ACOC <- round(laliga_T_HCO /laliga_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
laliga_home_coc <- aggregate(LALIGA$ACO, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_coc <- aggregate(LALIGA$HCO, by = list(LALIGA$AwayTeam), FUN = sum)
#home defense strength
laliga_home_cods <- round(((laliga_home_coc$x/laliga_home_games))/laliga_avg_HCOC, digits = 4)
#away defense strength
laliga_away_cods <- round(((laliga_away_coc$x/laliga_away_games))/laliga_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_home_poisson_corners <- cbind(laliga_division,laliga_teams,laliga_avg_HCO,laliga_home_coas,laliga_home_cods)
#################################################################################
#away poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_away_poisson_corners <- cbind(laliga_division,laliga_teams,laliga_avg_ACO,laliga_away_coas,laliga_away_cods)

#LALIGA
HomeTeam_laliga_co <- rep(laliga_teams, each = length(laliga_teams))
AwayTeam_laliga_co <- rep(laliga_teams, length(laliga_teams))
LALIGA_fixtures_co <- cbind(HomeTeam_laliga_co,AwayTeam_laliga_co)
LALIGA_fixtures_co <- as.data.frame(LALIGA_fixtures_co)
LALIGA_fixtures_co <- LALIGA_fixtures_co[!LALIGA_fixtures_co$HomeTeam_laliga_co == LALIGA_fixtures_co$AwayTeam_laliga_co,]
rownames(LALIGA_fixtures_co) <- NULL
LALIGA_fixtures_co$Div <- "LALIGA"
LALIGA_fixtures_co <- LALIGA_fixtures_co[,c(3,1,2)]

LALIGA_fixtures_co$avg_HCO_laliga <- laliga_avg_HCO

LALIGA_fixtures_co$laliga_homecoas <- rep(laliga_home_coas,each = length(laliga_teams)-1)

laliga_awaycods_lookup <- cbind(laliga_teams,laliga_away_cods)

laliga_awaycods_lookup <- as.data.frame(laliga_awaycods_lookup)

colnames(laliga_awaycods_lookup) <- c("AwayTeam_laliga_co","laliga_awaycods")


require('RH2')
LALIGA_fixtures_co$laliga_awaycods <- sqldf("SELECT laliga_awaycods_lookup.laliga_awaycods FROM laliga_awaycods_lookup INNER JOIN LALIGA_fixtures_co ON laliga_awaycods_lookup.AwayTeam_laliga_co = LALIGA_fixtures_co.AwayTeam_laliga_co")

LALIGA_fixtures_co$avg_ACO_laliga <- laliga_avg_ACO

laliga_awaycoas_lookup <- cbind(laliga_teams,laliga_away_coas)

laliga_awaycoas_lookup <- as.data.frame(laliga_awaycoas_lookup)

colnames(laliga_awaycoas_lookup) <- c("AwayTeam_laliga_co","laliga_awaycoas")

LALIGA_fixtures_co$laliga_awaycoas <- sqldf("SELECT laliga_awaycoas_lookup.laliga_awaycoas FROM laliga_awaycoas_lookup INNER JOIN LALIGA_fixtures_co ON laliga_awaycoas_lookup.AwayTeam_laliga_co = LALIGA_fixtures_co.AwayTeam_laliga_co")

LALIGA_fixtures_co$laliga_homecods <- rep(laliga_home_cods,each = length(laliga_teams)-1)

LALIGA_fixtures_co$laliga_awaycods <- as.numeric(unlist(LALIGA_fixtures_co$laliga_awaycods))
#xGH
LALIGA_fixtures_co$laliga_xHCOC <- LALIGA_fixtures_co$avg_HCO_laliga * LALIGA_fixtures_co$laliga_homecoas * LALIGA_fixtures_co$laliga_awaycods
#xGA

LALIGA_fixtures_co$laliga_awaycoas <- as.numeric(unlist(LALIGA_fixtures_co$laliga_awaycoas))

LALIGA_fixtures_co$laliga_xACOC <- LALIGA_fixtures_co$avg_ACO_laliga * LALIGA_fixtures_co$laliga_awaycoas * LALIGA_fixtures_co$laliga_homecods

LALIGA_fixtures_co$laliga_0_0 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_0 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_0_1 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_1 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_0 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_0_2 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_2 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_1 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_2 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_3 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_0 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_1 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_2 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_0_3 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_3 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_3 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_4 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_0 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_1 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_2 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_3 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_0_4 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_4 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_4 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_4 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_5 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_0 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_1 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_2 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_3 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_4 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_0_5 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_5 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_5 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_5 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_5 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_6 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_0 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(0,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_1 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(1,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_2 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(2,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_3 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(3,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_4 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(4,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_6_5 <- round(stats::dpois(6,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(5,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_0_6 <- round(stats::dpois(0,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_1_6 <- round(stats::dpois(1,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_2_6 <- round(stats::dpois(2,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_3_6 <- round(stats::dpois(3,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_4_6 <- round(stats::dpois(4,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
LALIGA_fixtures_co$laliga_5_6 <- round(stats::dpois(5,LALIGA_fixtures_co$laliga_xHCOC) * stats::dpois(6,LALIGA_fixtures_co$laliga_xACOC), digits = 4)
#Home win
LALIGA_fixtures_co$laliga_H <- (
  LALIGA_fixtures_co$laliga_1_0 + LALIGA_fixtures_co$laliga_2_0 + LALIGA_fixtures_co$laliga_2_1 + LALIGA_fixtures_co$laliga_3_0 + LALIGA_fixtures_co$laliga_3_1 +
    LALIGA_fixtures_co$laliga_3_2 + LALIGA_fixtures_co$laliga_4_0 + LALIGA_fixtures_co$laliga_4_1 + LALIGA_fixtures_co$laliga_4_2 + LALIGA_fixtures_co$laliga_4_3 +
    LALIGA_fixtures_co$laliga_5_0 + LALIGA_fixtures_co$laliga_5_1 + LALIGA_fixtures_co$laliga_5_2 + LALIGA_fixtures_co$laliga_5_3 + LALIGA_fixtures_co$laliga_5_4 +
    LALIGA_fixtures_co$laliga_6_0 + LALIGA_fixtures_co$laliga_6_1 + LALIGA_fixtures_co$laliga_6_2 + LALIGA_fixtures_co$laliga_6_3 + LALIGA_fixtures_co$laliga_6_4 +
    LALIGA_fixtures_co$laliga_6_5
)

LALIGA_fixtures_co$laliga_H <- percent(LALIGA_fixtures_co$laliga_H, accuracy = 0.1)

#Draw
LALIGA_fixtures_co$laliga_D <- (

  LALIGA_fixtures_co$laliga_0_0 + LALIGA_fixtures_co$laliga_1_1 + LALIGA_fixtures_co$laliga_2_2 + LALIGA_fixtures_co$laliga_3_3 + LALIGA_fixtures_co$laliga_4_4 +
    LALIGA_fixtures_co$laliga_5_5 + LALIGA_fixtures_co$laliga_6_6
)

LALIGA_fixtures_co$laliga_D <- percent(LALIGA_fixtures_co$laliga_D, accuracy = 0.1)

#Away

LALIGA_fixtures_co$laliga_A <- (
  LALIGA_fixtures_co$laliga_0_1 + LALIGA_fixtures_co$laliga_0_2 + LALIGA_fixtures_co$laliga_1_2 + LALIGA_fixtures_co$laliga_0_3 + LALIGA_fixtures_co$laliga_1_3 +
    LALIGA_fixtures_co$laliga_2_3 + LALIGA_fixtures_co$laliga_0_4 + LALIGA_fixtures_co$laliga_1_4 + LALIGA_fixtures_co$laliga_2_4 + LALIGA_fixtures_co$laliga_3_4 +
    LALIGA_fixtures_co$laliga_0_5 + LALIGA_fixtures_co$laliga_1_5 + LALIGA_fixtures_co$laliga_2_5 + LALIGA_fixtures_co$laliga_3_5 + LALIGA_fixtures_co$laliga_4_5 +
    LALIGA_fixtures_co$laliga_0_6 + LALIGA_fixtures_co$laliga_1_6 + LALIGA_fixtures_co$laliga_2_6 + LALIGA_fixtures_co$laliga_3_6 + LALIGA_fixtures_co$laliga_4_6 +
    LALIGA_fixtures_co$laliga_5_6
)

LALIGA_fixtures_co$laliga_A <- percent(LALIGA_fixtures_co$laliga_A, accuracy = 0.1)

#ov25
LALIGA_fixtures_co$laliga_ov25 <- (
  LALIGA_fixtures_co$laliga_2_1 + LALIGA_fixtures_co$laliga_1_2 + LALIGA_fixtures_co$laliga_2_2 + LALIGA_fixtures_co$laliga_3_0 + LALIGA_fixtures_co$laliga_3_1 +
    LALIGA_fixtures_co$laliga_3_2 + LALIGA_fixtures_co$laliga_0_3 + LALIGA_fixtures_co$laliga_1_3 + LALIGA_fixtures_co$laliga_2_3 + LALIGA_fixtures_co$laliga_3_3 +
    LALIGA_fixtures_co$laliga_4_0 + LALIGA_fixtures_co$laliga_4_1 + LALIGA_fixtures_co$laliga_4_2 + LALIGA_fixtures_co$laliga_4_3 + LALIGA_fixtures_co$laliga_0_4 +
    LALIGA_fixtures_co$laliga_1_4 + LALIGA_fixtures_co$laliga_2_4 + LALIGA_fixtures_co$laliga_3_4 + LALIGA_fixtures_co$laliga_4_4 + LALIGA_fixtures_co$laliga_5_0 +
    LALIGA_fixtures_co$laliga_5_1 + LALIGA_fixtures_co$laliga_5_2 + LALIGA_fixtures_co$laliga_5_3 + LALIGA_fixtures_co$laliga_5_4 + LALIGA_fixtures_co$laliga_0_5 +
    LALIGA_fixtures_co$laliga_1_5 + LALIGA_fixtures_co$laliga_2_5 + LALIGA_fixtures_co$laliga_3_5 + LALIGA_fixtures_co$laliga_4_5 + LALIGA_fixtures_co$laliga_5_5 +
    LALIGA_fixtures_co$laliga_6_0 + LALIGA_fixtures_co$laliga_6_1 + LALIGA_fixtures_co$laliga_6_2 + LALIGA_fixtures_co$laliga_6_3 + LALIGA_fixtures_co$laliga_6_4 +
    LALIGA_fixtures_co$laliga_6_5 + LALIGA_fixtures_co$laliga_0_6 + LALIGA_fixtures_co$laliga_1_6 + LALIGA_fixtures_co$laliga_2_6 + LALIGA_fixtures_co$laliga_3_6 +
    LALIGA_fixtures_co$laliga_4_6 + LALIGA_fixtures_co$laliga_5_6 + LALIGA_fixtures_co$laliga_6_6
)
#un25
LALIGA_fixtures_co$laliga_un25 <- (
  LALIGA_fixtures_co$laliga_0_0 + LALIGA_fixtures_co$laliga_1_0 + LALIGA_fixtures_co$laliga_0_1 + LALIGA_fixtures_co$laliga_1_1 + LALIGA_fixtures_co$laliga_2_0 + LALIGA_fixtures_co$laliga_0_2
)
#odds
LALIGA_fixtures_co$laliga_ov25_odds <- round((1/LALIGA_fixtures_co$laliga_ov25),digits = 2)
LALIGA_fixtures_co$laliga_un25_odds <- round((1/LALIGA_fixtures_co$laliga_un25),digits = 2)

LALIGA_fixtures_co$laliga_ov25_odds
LALIGA_fixtures_co$laliga_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGA_fixtures_co$laliga_ov25 <- percent(LALIGA_fixtures_co$laliga_ov25, accuracy = 0.1)

LALIGA_fixtures_co$laliga_un25 <- percent(LALIGA_fixtures_co$laliga_un25, accuracy = 0.1)
LALIGA_fixtures_co$laliga_pscore <- paste(round(LALIGA_fixtures_co$laliga_xHCOC,digits = 0),round(LALIGA_fixtures_co$laliga_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
laliga_GP <- nrow(LALIGA)
#Calculate total home goals for each division
laliga_T_HF <- sum(laliga_home_fouls$x)
#calculate average home goal
laliga_avg_HF <- round(laliga_T_HF /laliga_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laliga_T_AF <- sum(laliga_away_fouls$x)
#calculate average away goal
laliga_avg_AF <- round(laliga_T_AF /laliga_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laliga_home_fas <- round(((laliga_home_fouls$x/laliga_home_games))/laliga_avg_HF, digits = 4)
#calculate away attack strength
laliga_away_fas <- round(((laliga_away_fouls$x/laliga_away_games))/laliga_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
laliga_avg_HFC <- round(laliga_T_AF /laliga_GP, digits = 4)
#avg away concede
laliga_avg_AFC <- round(laliga_T_HF /laliga_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
laliga_home_fcc <- aggregate(LALIGA$AF, by = list(LALIGA$HomeTeam), FUN = sum)
laliga_away_fcc <- aggregate(LALIGA$HF, by = list(LALIGA$AwayTeam), FUN = sum)

#home defense strength
laliga_home_fds <- round(((laliga_home_fcc$x/laliga_home_games))/laliga_avg_HFC, digits = 4)

#away defense strength
laliga_away_fds <- round(((laliga_away_fcc$x/laliga_away_games))/laliga_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_home_poisson_fo <- cbind(laliga_division,laliga_teams,laliga_avg_HF,laliga_home_fas,laliga_home_fds)

#################################################################################
#away poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_away_poisson_fo <- cbind(laliga_division,laliga_teams,laliga_avg_AF,laliga_away_fas,laliga_away_fds)

#LALIGA
HomeTeam_laliga_fo <- rep(laliga_teams, each = length(laliga_teams))
AwayTeam_laliga_fo <- rep(laliga_teams, length(laliga_teams))
LALIGA_fixtures_fo <- cbind(HomeTeam_laliga_fo,AwayTeam_laliga_fo)
LALIGA_fixtures_fo <- as.data.frame(LALIGA_fixtures_fo)
LALIGA_fixtures_fo <- LALIGA_fixtures_fo[!LALIGA_fixtures_fo$HomeTeam_laliga_fo == LALIGA_fixtures_fo$AwayTeam_laliga_fo,]
rownames(LALIGA_fixtures_fo) <- NULL
LALIGA_fixtures_fo$Div <- "LALIGA"
LALIGA_fixtures_fo <- LALIGA_fixtures_fo[,c(3,1,2)]

LALIGA_fixtures_fo$avg_HF_laliga <- laliga_avg_HF

LALIGA_fixtures_fo$laliga_homefas <- rep(laliga_home_fas,each = length(laliga_teams)-1)

laliga_awayfds_lookup <- cbind(laliga_teams,laliga_away_fds)

laliga_awayfds_lookup <- as.data.frame(laliga_awayfds_lookup)

colnames(laliga_awayfds_lookup) <- c("AwayTeam_laliga_fo","laliga_awayfds")


require('RH2')
LALIGA_fixtures_fo$laliga_awayfds <- sqldf("SELECT laliga_awayfds_lookup.laliga_awayfds FROM laliga_awayfds_lookup INNER JOIN LALIGA_fixtures_fo ON laliga_awayfds_lookup.AwayTeam_laliga_fo = LALIGA_fixtures_fo.AwayTeam_laliga_fo")

LALIGA_fixtures_fo$avg_AF_laliga <- laliga_avg_AF

laliga_awayfas_lookup <- cbind(laliga_teams,laliga_away_fas)

laliga_awayfas_lookup <- as.data.frame(laliga_awayfas_lookup)

colnames(laliga_awayfas_lookup) <- c("AwayTeam_laliga_fo","laliga_awayfas")

LALIGA_fixtures_fo$laliga_awayfas <- sqldf("SELECT laliga_awayfas_lookup.laliga_awayfas FROM laliga_awayfas_lookup INNER JOIN LALIGA_fixtures_fo ON laliga_awayfas_lookup.AwayTeam_laliga_fo = LALIGA_fixtures_fo.AwayTeam_laliga_fo")

LALIGA_fixtures_fo$laliga_homefds <- rep(laliga_home_fds,each = length(laliga_teams)-1)

LALIGA_fixtures_fo$laliga_awayfds <- as.numeric(unlist(LALIGA_fixtures_fo$laliga_awayfds))
#xGH
LALIGA_fixtures_fo$laliga_xHF <- LALIGA_fixtures_fo$avg_HF_laliga * LALIGA_fixtures_fo$laliga_homefas * LALIGA_fixtures_fo$laliga_awayfds
#xGA

LALIGA_fixtures_fo$laliga_awayfas <- as.numeric(unlist(LALIGA_fixtures_fo$laliga_awayfas))

LALIGA_fixtures_fo$laliga_xAF <- LALIGA_fixtures_fo$avg_AF_laliga * LALIGA_fixtures_fo$laliga_awayfas * LALIGA_fixtures_fo$laliga_homefds

LALIGA_fixtures_fo$laliga_0_0 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_0 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_0_1 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_1 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_0 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_0_2 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_2 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_1 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_2 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_3 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_0 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_1 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_2 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_0_3 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_3 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_3 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_4 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_0 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_1 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_2 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_3 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_0_4 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_4 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_4 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_4 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_5 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_0 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_1 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_2 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_3 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_4 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_0_5 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_5 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_5 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_5 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_5 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_6 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_0 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(0,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_1 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(1,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_2 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(2,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_3 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(3,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_4 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(4,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_6_5 <- round(stats::dpois(6,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(5,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_0_6 <- round(stats::dpois(0,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_1_6 <- round(stats::dpois(1,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_2_6 <- round(stats::dpois(2,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_3_6 <- round(stats::dpois(3,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_4_6 <- round(stats::dpois(4,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
LALIGA_fixtures_fo$laliga_5_6 <- round(stats::dpois(5,LALIGA_fixtures_fo$laliga_xHF) * stats::dpois(6,LALIGA_fixtures_fo$laliga_xAF), digits = 4)
#Home win
LALIGA_fixtures_fo$laliga_H <- (
  LALIGA_fixtures_fo$laliga_1_0 + LALIGA_fixtures_fo$laliga_2_0 + LALIGA_fixtures_fo$laliga_2_1 + LALIGA_fixtures_fo$laliga_3_0 + LALIGA_fixtures_fo$laliga_3_1 +
    LALIGA_fixtures_fo$laliga_3_2 + LALIGA_fixtures_fo$laliga_4_0 + LALIGA_fixtures_fo$laliga_4_1 + LALIGA_fixtures_fo$laliga_4_2 + LALIGA_fixtures_fo$laliga_4_3 +
    LALIGA_fixtures_fo$laliga_5_0 + LALIGA_fixtures_fo$laliga_5_1 + LALIGA_fixtures_fo$laliga_5_2 + LALIGA_fixtures_fo$laliga_5_3 + LALIGA_fixtures_fo$laliga_5_4 +
    LALIGA_fixtures_fo$laliga_6_0 + LALIGA_fixtures_fo$laliga_6_1 + LALIGA_fixtures_fo$laliga_6_2 + LALIGA_fixtures_fo$laliga_6_3 + LALIGA_fixtures_fo$laliga_6_4 +
    LALIGA_fixtures_fo$laliga_6_5
)

LALIGA_fixtures_fo$laliga_H <- percent(LALIGA_fixtures_fo$laliga_H, accuracy = 0.1)

#Draw
LALIGA_fixtures_fo$laliga_D <- (

  LALIGA_fixtures_fo$laliga_0_0 + LALIGA_fixtures_fo$laliga_1_1 + LALIGA_fixtures_fo$laliga_2_2 + LALIGA_fixtures_fo$laliga_3_3 + LALIGA_fixtures_fo$laliga_4_4 +
    LALIGA_fixtures_fo$laliga_5_5 + LALIGA_fixtures_fo$laliga_6_6
)

LALIGA_fixtures_fo$laliga_D <- percent(LALIGA_fixtures_fo$laliga_D, accuracy = 0.1)

#Away

LALIGA_fixtures_fo$laliga_A <- (
  LALIGA_fixtures_fo$laliga_0_1 + LALIGA_fixtures_fo$laliga_0_2 + LALIGA_fixtures_fo$laliga_1_2 + LALIGA_fixtures_fo$laliga_0_3 + LALIGA_fixtures_fo$laliga_1_3 +
    LALIGA_fixtures_fo$laliga_2_3 + LALIGA_fixtures_fo$laliga_0_4 + LALIGA_fixtures_fo$laliga_1_4 + LALIGA_fixtures_fo$laliga_2_4 + LALIGA_fixtures_fo$laliga_3_4 +
    LALIGA_fixtures_fo$laliga_0_5 + LALIGA_fixtures_fo$laliga_1_5 + LALIGA_fixtures_fo$laliga_2_5 + LALIGA_fixtures_fo$laliga_3_5 + LALIGA_fixtures_fo$laliga_4_5 +
    LALIGA_fixtures_fo$laliga_0_6 + LALIGA_fixtures_fo$laliga_1_6 + LALIGA_fixtures_fo$laliga_2_6 + LALIGA_fixtures_fo$laliga_3_6 + LALIGA_fixtures_fo$laliga_4_6 +
    LALIGA_fixtures_fo$laliga_5_6
)

LALIGA_fixtures_fo$laliga_A <- percent(LALIGA_fixtures_fo$laliga_A, accuracy = 0.1)

#ov25
LALIGA_fixtures_fo$laliga_ov25 <- (
  LALIGA_fixtures_fo$laliga_2_1 + LALIGA_fixtures_fo$laliga_1_2 + LALIGA_fixtures_fo$laliga_2_2 + LALIGA_fixtures_fo$laliga_3_0 + LALIGA_fixtures_fo$laliga_3_1 +
    LALIGA_fixtures_fo$laliga_3_2 + LALIGA_fixtures_fo$laliga_0_3 + LALIGA_fixtures_fo$laliga_1_3 + LALIGA_fixtures_fo$laliga_2_3 + LALIGA_fixtures_fo$laliga_3_3 +
    LALIGA_fixtures_fo$laliga_4_0 + LALIGA_fixtures_fo$laliga_4_1 + LALIGA_fixtures_fo$laliga_4_2 + LALIGA_fixtures_fo$laliga_4_3 + LALIGA_fixtures_fo$laliga_0_4 +
    LALIGA_fixtures_fo$laliga_1_4 + LALIGA_fixtures_fo$laliga_2_4 + LALIGA_fixtures_fo$laliga_3_4 + LALIGA_fixtures_fo$laliga_4_4 + LALIGA_fixtures_fo$laliga_5_0 +
    LALIGA_fixtures_fo$laliga_5_1 + LALIGA_fixtures_fo$laliga_5_2 + LALIGA_fixtures_fo$laliga_5_3 + LALIGA_fixtures_fo$laliga_5_4 + LALIGA_fixtures_fo$laliga_0_5 +
    LALIGA_fixtures_fo$laliga_1_5 + LALIGA_fixtures_fo$laliga_2_5 + LALIGA_fixtures_fo$laliga_3_5 + LALIGA_fixtures_fo$laliga_4_5 + LALIGA_fixtures_fo$laliga_5_5 +
    LALIGA_fixtures_fo$laliga_6_0 + LALIGA_fixtures_fo$laliga_6_1 + LALIGA_fixtures_fo$laliga_6_2 + LALIGA_fixtures_fo$laliga_6_3 + LALIGA_fixtures_fo$laliga_6_4 +
    LALIGA_fixtures_fo$laliga_6_5 + LALIGA_fixtures_fo$laliga_0_6 + LALIGA_fixtures_fo$laliga_1_6 + LALIGA_fixtures_fo$laliga_2_6 + LALIGA_fixtures_fo$laliga_3_6 +
    LALIGA_fixtures_fo$laliga_4_6 + LALIGA_fixtures_fo$laliga_5_6 + LALIGA_fixtures_fo$laliga_6_6
)
#un25
LALIGA_fixtures_fo$laliga_un25 <- (
  LALIGA_fixtures_fo$laliga_0_0 + LALIGA_fixtures_fo$laliga_1_0 + LALIGA_fixtures_fo$laliga_0_1 + LALIGA_fixtures_fo$laliga_1_1 + LALIGA_fixtures_fo$laliga_2_0 + LALIGA_fixtures_fo$laliga_0_2
)
#odds
LALIGA_fixtures_fo$laliga_ov25_odds <- round((1/LALIGA_fixtures_fo$laliga_ov25),digits = 2)
LALIGA_fixtures_fo$laliga_un25_odds <- round((1/LALIGA_fixtures_fo$laliga_un25),digits = 2)

LALIGA_fixtures_fo$laliga_ov25_odds
LALIGA_fixtures_fo$laliga_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGA_fixtures_fo$laliga_ov25 <- percent(LALIGA_fixtures_fo$laliga_ov25, accuracy = 0.1)

LALIGA_fixtures_fo$laliga_un25 <- percent(LALIGA_fixtures_fo$laliga_un25, accuracy = 0.1)
LALIGA_fixtures_fo$laliga_psfore <- paste(round(LALIGA_fixtures_fo$laliga_xHF,digits = 0),round(LALIGA_fixtures_fo$laliga_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
laliga_GP <- nrow(LALIGA)

#Calculate total home goals for each division
laliga_T_HST <- sum(laliga_home_hst$x)
#calculate average home goal

laliga_avg_HST <- round(laliga_T_HST /laliga_GP, digits = 4)

############################################################
#Calculate total away goals for each division
laliga_T_AST <- sum(laliga_away_ast$x)
#calculate average away goal
laliga_avg_AST <- round(laliga_T_AST /laliga_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laliga_home_sotas <- round(((laliga_home_hst$x/laliga_home_games))/laliga_avg_HST, digits = 4)
#calculate away attack strength
laliga_away_sotas <- round(((laliga_away_ast$x/laliga_away_games))/laliga_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
laliga_avg_HSC <- round(laliga_T_AST /laliga_GP, digits = 4)

#avg away concede
laliga_avg_ASC <- round(laliga_T_HST /laliga_GP, digits = 4)
#home defense strength
laliga_home_sods <- round(((laliga_home_hsc$x/laliga_home_games))/laliga_avg_HSC, digits = 4)

#away defense strength
laliga_away_sods <- round(((laliga_away_ast$x/laliga_away_games))/laliga_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_home_poisson_sot <- cbind(laliga_division,laliga_teams,laliga_avg_HST,laliga_home_sotas,laliga_home_sods)

#################################################################################
#away poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_away_poisson_sot <- cbind(laliga_division,laliga_teams,laliga_avg_AST,laliga_away_sotas,laliga_away_sods)

#LALIGA
HomeTeam_laliga_sot <- rep(laliga_teams, each = length(laliga_teams))
AwayTeam_laliga_sot <- rep(laliga_teams, length(laliga_teams))
LALIGA_fixtures_sot <- cbind(HomeTeam_laliga_sot,AwayTeam_laliga_sot)
LALIGA_fixtures_sot <- as.data.frame(LALIGA_fixtures_sot)
LALIGA_fixtures_sot <- LALIGA_fixtures_sot[!LALIGA_fixtures_sot$HomeTeam_laliga_sot == LALIGA_fixtures_sot$AwayTeam_laliga_sot,]
rownames(LALIGA_fixtures_sot) <- NULL
LALIGA_fixtures_sot$Div <- "LALIGA"
LALIGA_fixtures_sot <- LALIGA_fixtures_sot[,c(3,1,2)]

LALIGA_fixtures_sot$avg_HST_laliga <- laliga_avg_HST

LALIGA_fixtures_sot$laliga_homesotas <- rep(laliga_home_sotas,each = length(laliga_teams)-1)

laliga_awaysods_lookup <- cbind(laliga_teams,laliga_away_sods)

laliga_awaysods_lookup <- as.data.frame(laliga_awaysods_lookup)

colnames(laliga_awaysods_lookup) <- c("AwayTeam_laliga_sot","laliga_awaysods")


require('RH2')
LALIGA_fixtures_sot$laliga_awaysods <- sqldf("SELECT laliga_awaysods_lookup.laliga_awaysods FROM laliga_awaysods_lookup INNER JOIN LALIGA_fixtures_sot ON laliga_awaysods_lookup.AwayTeam_laliga_sot = LALIGA_fixtures_sot.AwayTeam_laliga_sot")

LALIGA_fixtures_sot$avg_AST_laliga <- laliga_avg_AST

laliga_awaysotas_lookup <- cbind(laliga_teams,laliga_away_sotas)

laliga_awaysotas_lookup <- as.data.frame(laliga_awaysotas_lookup)

colnames(laliga_awaysotas_lookup) <- c("AwayTeam_laliga_sot","laliga_awaysotas")

LALIGA_fixtures_sot$laliga_awaysotas <- sqldf("SELECT laliga_awaysotas_lookup.laliga_awaysotas FROM laliga_awaysotas_lookup INNER JOIN LALIGA_fixtures_sot ON laliga_awaysotas_lookup.AwayTeam_laliga_sot = LALIGA_fixtures_sot.AwayTeam_laliga_sot")

LALIGA_fixtures_sot$laliga_homesods <- rep(laliga_home_sods,each = length(laliga_teams)-1)

LALIGA_fixtures_sot$laliga_awaysods <- as.numeric(unlist(LALIGA_fixtures_sot$laliga_awaysods))
#xGH
LALIGA_fixtures_sot$laliga_xHST <- LALIGA_fixtures_sot$avg_HST_laliga * LALIGA_fixtures_sot$laliga_homesotas * LALIGA_fixtures_sot$laliga_awaysods
#xGA

LALIGA_fixtures_sot$laliga_awaysotas <- as.numeric(unlist(LALIGA_fixtures_sot$laliga_awaysotas))

LALIGA_fixtures_sot$laliga_xAST <- LALIGA_fixtures_sot$avg_AST_laliga * LALIGA_fixtures_sot$laliga_awaysotas * LALIGA_fixtures_sot$laliga_homesods

LALIGA_fixtures_sot$laliga_0_0 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_0 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_0_1 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_1 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_0 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_0_2 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_2 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_1 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_2 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_3 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_0 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_1 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_2 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_0_3 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_3 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_3 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_4 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_0 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_1 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_2 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_3 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_0_4 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_4 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_4 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_4 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_5 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_0 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_1 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_2 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_3 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_4 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_0_5 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_5 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_5 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_5 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_5 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_6 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_0 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(0,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_1 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(1,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_2 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(2,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_3 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(3,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_4 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(4,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_6_5 <- round(stats::dpois(6,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(5,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_0_6 <- round(stats::dpois(0,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_1_6 <- round(stats::dpois(1,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_2_6 <- round(stats::dpois(2,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_3_6 <- round(stats::dpois(3,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_4_6 <- round(stats::dpois(4,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
LALIGA_fixtures_sot$laliga_5_6 <- round(stats::dpois(5,LALIGA_fixtures_sot$laliga_xHST) * stats::dpois(6,LALIGA_fixtures_sot$laliga_xAST), digits = 4)
#Home win
LALIGA_fixtures_sot$laliga_H <- (
  LALIGA_fixtures_sot$laliga_1_0 + LALIGA_fixtures_sot$laliga_2_0 + LALIGA_fixtures_sot$laliga_2_1 + LALIGA_fixtures_sot$laliga_3_0 + LALIGA_fixtures_sot$laliga_3_1 +
    LALIGA_fixtures_sot$laliga_3_2 + LALIGA_fixtures_sot$laliga_4_0 + LALIGA_fixtures_sot$laliga_4_1 + LALIGA_fixtures_sot$laliga_4_2 + LALIGA_fixtures_sot$laliga_4_3 +
    LALIGA_fixtures_sot$laliga_5_0 + LALIGA_fixtures_sot$laliga_5_1 + LALIGA_fixtures_sot$laliga_5_2 + LALIGA_fixtures_sot$laliga_5_3 + LALIGA_fixtures_sot$laliga_5_4 +
    LALIGA_fixtures_sot$laliga_6_0 + LALIGA_fixtures_sot$laliga_6_1 + LALIGA_fixtures_sot$laliga_6_2 + LALIGA_fixtures_sot$laliga_6_3 + LALIGA_fixtures_sot$laliga_6_4 +
    LALIGA_fixtures_sot$laliga_6_5
)

LALIGA_fixtures_sot$laliga_H <- percent(LALIGA_fixtures_sot$laliga_H, accuracy = 0.1)

#Draw
LALIGA_fixtures_sot$laliga_D <- (

  LALIGA_fixtures_sot$laliga_0_0 + LALIGA_fixtures_sot$laliga_1_1 + LALIGA_fixtures_sot$laliga_2_2 + LALIGA_fixtures_sot$laliga_3_3 + LALIGA_fixtures_sot$laliga_4_4 +
    LALIGA_fixtures_sot$laliga_5_5 + LALIGA_fixtures_sot$laliga_6_6
)

LALIGA_fixtures_sot$laliga_D <- percent(LALIGA_fixtures_sot$laliga_D, accuracy = 0.1)

#Away

LALIGA_fixtures_sot$laliga_A <- (
  LALIGA_fixtures_sot$laliga_0_1 + LALIGA_fixtures_sot$laliga_0_2 + LALIGA_fixtures_sot$laliga_1_2 + LALIGA_fixtures_sot$laliga_0_3 + LALIGA_fixtures_sot$laliga_1_3 +
    LALIGA_fixtures_sot$laliga_2_3 + LALIGA_fixtures_sot$laliga_0_4 + LALIGA_fixtures_sot$laliga_1_4 + LALIGA_fixtures_sot$laliga_2_4 + LALIGA_fixtures_sot$laliga_3_4 +
    LALIGA_fixtures_sot$laliga_0_5 + LALIGA_fixtures_sot$laliga_1_5 + LALIGA_fixtures_sot$laliga_2_5 + LALIGA_fixtures_sot$laliga_3_5 + LALIGA_fixtures_sot$laliga_4_5 +
    LALIGA_fixtures_sot$laliga_0_6 + LALIGA_fixtures_sot$laliga_1_6 + LALIGA_fixtures_sot$laliga_2_6 + LALIGA_fixtures_sot$laliga_3_6 + LALIGA_fixtures_sot$laliga_4_6 +
    LALIGA_fixtures_sot$laliga_5_6
)

LALIGA_fixtures_sot$laliga_A <- percent(LALIGA_fixtures_sot$laliga_A, accuracy = 0.1)

#ov25
LALIGA_fixtures_sot$laliga_ov25 <- (
  LALIGA_fixtures_sot$laliga_2_1 + LALIGA_fixtures_sot$laliga_1_2 + LALIGA_fixtures_sot$laliga_2_2 + LALIGA_fixtures_sot$laliga_3_0 + LALIGA_fixtures_sot$laliga_3_1 +
    LALIGA_fixtures_sot$laliga_3_2 + LALIGA_fixtures_sot$laliga_0_3 + LALIGA_fixtures_sot$laliga_1_3 + LALIGA_fixtures_sot$laliga_2_3 + LALIGA_fixtures_sot$laliga_3_3 +
    LALIGA_fixtures_sot$laliga_4_0 + LALIGA_fixtures_sot$laliga_4_1 + LALIGA_fixtures_sot$laliga_4_2 + LALIGA_fixtures_sot$laliga_4_3 + LALIGA_fixtures_sot$laliga_0_4 +
    LALIGA_fixtures_sot$laliga_1_4 + LALIGA_fixtures_sot$laliga_2_4 + LALIGA_fixtures_sot$laliga_3_4 + LALIGA_fixtures_sot$laliga_4_4 + LALIGA_fixtures_sot$laliga_5_0 +
    LALIGA_fixtures_sot$laliga_5_1 + LALIGA_fixtures_sot$laliga_5_2 + LALIGA_fixtures_sot$laliga_5_3 + LALIGA_fixtures_sot$laliga_5_4 + LALIGA_fixtures_sot$laliga_0_5 +
    LALIGA_fixtures_sot$laliga_1_5 + LALIGA_fixtures_sot$laliga_2_5 + LALIGA_fixtures_sot$laliga_3_5 + LALIGA_fixtures_sot$laliga_4_5 + LALIGA_fixtures_sot$laliga_5_5 +
    LALIGA_fixtures_sot$laliga_6_0 + LALIGA_fixtures_sot$laliga_6_1 + LALIGA_fixtures_sot$laliga_6_2 + LALIGA_fixtures_sot$laliga_6_3 + LALIGA_fixtures_sot$laliga_6_4 +
    LALIGA_fixtures_sot$laliga_6_5 + LALIGA_fixtures_sot$laliga_0_6 + LALIGA_fixtures_sot$laliga_1_6 + LALIGA_fixtures_sot$laliga_2_6 + LALIGA_fixtures_sot$laliga_3_6 +
    LALIGA_fixtures_sot$laliga_4_6 + LALIGA_fixtures_sot$laliga_5_6 + LALIGA_fixtures_sot$laliga_6_6
)
#un25
LALIGA_fixtures_sot$laliga_un25 <- (
  LALIGA_fixtures_sot$laliga_0_0 + LALIGA_fixtures_sot$laliga_1_0 + LALIGA_fixtures_sot$laliga_0_1 + LALIGA_fixtures_sot$laliga_1_1 + LALIGA_fixtures_sot$laliga_2_0 + LALIGA_fixtures_sot$laliga_0_2
)
#odds
LALIGA_fixtures_sot$laliga_ov25_odds <- round((1/LALIGA_fixtures_sot$laliga_ov25),digits = 2)
LALIGA_fixtures_sot$laliga_un25_odds <- round((1/LALIGA_fixtures_sot$laliga_un25),digits = 2)

LALIGA_fixtures_sot$laliga_ov25_odds
LALIGA_fixtures_sot$laliga_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGA_fixtures_sot$laliga_ov25 <- percent(LALIGA_fixtures_sot$laliga_ov25, accuracy = 0.1)

LALIGA_fixtures_sot$laliga_un25 <- percent(LALIGA_fixtures_sot$laliga_un25, accuracy = 0.1)
LALIGA_fixtures_sot$laliga_pssotre <- paste(round(LALIGA_fixtures_sot$laliga_xHST,digits = 0),round(LALIGA_fixtures_sot$laliga_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
laliga_home_wins <- c()
laliga_away_wins <- c()
laliga_home_draws <- c()
laliga_away_draws <- c()
laliga_home_loss <- c()
laliga_away_loss <- c()



for (i_laliga_wins in 1:length(laliga_teams))
{

  laliga_home_wins[i_laliga_wins] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_wins] & LALIGA$FTR == "H",])
  laliga_away_wins[i_laliga_wins] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_wins] & LALIGA$FTR == "A",])
  laliga_home_draws[i_laliga_wins] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_wins] & LALIGA$FTR == "D",])
  laliga_away_draws[i_laliga_wins] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_wins] & LALIGA$FTR == "D",])
  laliga_home_loss[i_laliga_wins] <- nrow(LALIGA[LALIGA$HomeTeam == laliga_teams[i_laliga_wins] & LALIGA$FTR == "A",])
  laliga_away_loss[i_laliga_wins] <- nrow(LALIGA[LALIGA$AwayTeam == laliga_teams[i_laliga_wins] & LALIGA$FTR == "H",])

}

laliga_total_wins <- laliga_home_wins + laliga_away_wins
laliga_total_draws <- laliga_home_draws + laliga_away_draws
laliga_total_loss <- laliga_home_loss + laliga_away_loss

laliga_league_table <- cbind(laliga_teams,laliga_games_played,laliga_total_wins,laliga_total_draws,laliga_total_loss)
laliga_GS <- laliga_scoring$TGS
laliga_GC <-laliga_conceding$TGC
laliga_GD <- laliga_scoring$TGS - laliga_conceding$TGC
laliga_PTS <- (laliga_total_wins*3) + (laliga_total_draws*1)
laliga_league_table <- cbind(laliga_league_table,laliga_GS,laliga_GC,laliga_GD,laliga_PTS)
laliga_league_table <- as.data.frame(laliga_league_table)
#rename the columns
names(laliga_league_table)[names(laliga_league_table) == "laliga_teams"] <- "Team"
names(laliga_league_table)[names(laliga_league_table) == "laliga_games_played"] <- "P"
names(laliga_league_table)[names(laliga_league_table) == "laliga_total_wins"] <- "W"
names(laliga_league_table)[names(laliga_league_table) == "laliga_total_draws"] <- "D"
names(laliga_league_table)[names(laliga_league_table) == "laliga_total_loss"] <- "L"
names(laliga_league_table)[names(laliga_league_table) == "laliga_GS"] <- "F"
names(laliga_league_table)[names(laliga_league_table) == "laliga_GC"] <- "A"
points_laliga <- laliga_league_table[order(as.numeric(laliga_league_table$laliga_PTS), decreasing = TRUE),]
points_laliga$laliga_rank <- 1:length(laliga_teams)
row.names(points_laliga) <- points_laliga$laliga_rank
#create final_laliga_hf_against with team ranks in brackets
for(laliga_rowhrank in 1:nrow(laliga_form_team_against_h)) {
  for(laliga_colhrank in 1:ncol(laliga_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!laliga_form_team_against_h[laliga_rowhrank,laliga_colhrank]=="",laliga_form_team_against_h[laliga_rowhrank,laliga_colhrank] <- paste(laliga_form_team_against_h[laliga_rowhrank,laliga_colhrank],"(",points_laliga$laliga_rank[points_laliga$Team ==laliga_form_team_against_h[laliga_rowhrank,laliga_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
laliga_GP <- nrow(LALIGA)

#Calculate total home goals for each division
laliga_T_HG <- sum(laliga_home_gs$x)

#calculate average home goal
laliga_avg_HG <- round(laliga_T_HG /laliga_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laliga_T_AG <- sum(laliga_away_gs$x)
#calculate average away goal
laliga_avg_AG <- round(laliga_T_AG /laliga_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laliga_home_as <- round(((laliga_home_gs$x/laliga_home_games))/laliga_avg_HG, digits = 4)
#calculate away attack strength
laliga_away_as <- round(((laliga_away_gs$x/laliga_away_games))/laliga_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
laliga_avg_HC <- round(laliga_T_AG /laliga_GP, digits = 4)
#avg away concede
laliga_avg_AC <- round(laliga_T_HG /laliga_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
laliga_home_ds <- round(((laliga_home_gc$x/laliga_home_games))/laliga_avg_HC, digits = 4)
#away defense strength
laliga_away_ds <- round(((laliga_away_gc$x/laliga_away_games))/laliga_avg_AC, digits = 4)
#############################################################################
#home poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_home_poisson <- cbind(laliga_division,laliga_teams,laliga_avg_HG,laliga_home_as,laliga_home_ds)
#################################################################################
#away poisson data
#laliga
laliga_division <- c()
laliga_division[1:length(laliga_teams)] <- "LALIGA"
laliga_away_poisson <- cbind(laliga_division,laliga_teams,laliga_avg_AG,laliga_away_as,laliga_away_ds)

#LALIGA
HomeTeam_laliga <- rep(laliga_teams, each = length(laliga_teams))
AwayTeam_laliga <- rep(laliga_teams, length(laliga_teams))
LALIGA_fixtures <- cbind(HomeTeam_laliga,AwayTeam_laliga)
LALIGA_fixtures <- as.data.frame(LALIGA_fixtures)
LALIGA_fixtures <- LALIGA_fixtures[!LALIGA_fixtures$HomeTeam_laliga == LALIGA_fixtures$AwayTeam_laliga,]
rownames(LALIGA_fixtures) <- NULL
LALIGA_fixtures$Div <- "LALIGA"
LALIGA_fixtures <- LALIGA_fixtures[,c(3,1,2)]

LALIGA_fixtures$avg_HG_laliga <- laliga_avg_HG

LALIGA_fixtures$laliga_homeas <- rep(laliga_home_as,each = length(laliga_teams)-1)

laliga_awayds_lookup <- cbind(laliga_teams,laliga_away_ds)

laliga_awayds_lookup <- as.data.frame(laliga_awayds_lookup)

colnames(laliga_awayds_lookup) <- c("AwayTeam_laliga","laliga_awayds")


require('RH2')
LALIGA_fixtures$laliga_awayds <- sqldf("SELECT laliga_awayds_lookup.laliga_awayds FROM laliga_awayds_lookup INNER JOIN LALIGA_fixtures ON laliga_awayds_lookup.AwayTeam_laliga = LALIGA_fixtures.AwayTeam_laliga")

LALIGA_fixtures$avg_AG_laliga <- laliga_avg_AG

laliga_awayas_lookup <- cbind(laliga_teams,laliga_away_as)

laliga_awayas_lookup <- as.data.frame(laliga_awayas_lookup)

colnames(laliga_awayas_lookup) <- c("AwayTeam_laliga","laliga_awayas")


LALIGA_fixtures$laliga_awayas <- sqldf("SELECT laliga_awayas_lookup.laliga_awayas FROM laliga_awayas_lookup INNER JOIN LALIGA_fixtures ON laliga_awayas_lookup.AwayTeam_laliga = LALIGA_fixtures.AwayTeam_laliga")

LALIGA_fixtures$laliga_homeds <- rep(laliga_home_ds,each = length(laliga_teams)-1)

LALIGA_fixtures$laliga_awayds <- as.numeric(unlist(LALIGA_fixtures$laliga_awayds))
#xGH
LALIGA_fixtures$laliga_xGH <- LALIGA_fixtures$avg_HG_laliga * LALIGA_fixtures$laliga_homeas * LALIGA_fixtures$laliga_awayds

#xGA

LALIGA_fixtures$laliga_awayas <- as.numeric(unlist(LALIGA_fixtures$laliga_awayas))

LALIGA_fixtures$laliga_xGA <- LALIGA_fixtures$avg_AG_laliga * LALIGA_fixtures$laliga_awayas * LALIGA_fixtures$laliga_homeds

LALIGA_fixtures$laliga_0_0 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_0 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_0_1 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_1 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_0 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_0_2 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_2 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_1 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_2 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_3 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_0 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_1 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_2 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_0_3 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_3 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_3 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_4 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_0 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_1 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_2 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_3 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_0_4 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_4 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_4 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_4 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_5 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_0 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_1 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_2 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_3 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_4 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_0_5 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_5 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_5 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_5 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_5 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_6 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_0 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(0,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_1 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(1,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_2 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(2,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_3 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(3,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_4 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(4,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_6_5 <- round(stats::dpois(6,LALIGA_fixtures$laliga_xGH) * stats::dpois(5,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_0_6 <- round(stats::dpois(0,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_1_6 <- round(stats::dpois(1,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_2_6 <- round(stats::dpois(2,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_3_6 <- round(stats::dpois(3,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_4_6 <- round(stats::dpois(4,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
LALIGA_fixtures$laliga_5_6 <- round(stats::dpois(5,LALIGA_fixtures$laliga_xGH) * stats::dpois(6,LALIGA_fixtures$laliga_xGA), digits = 4)
#Home win
LALIGA_fixtures$laliga_H <- (
  LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
    LALIGA_fixtures$laliga_5_0 + LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5
)

LALIGA_fixtures$laliga_H <- percent(LALIGA_fixtures$laliga_H, accuracy = 0.1)

#Draw
LALIGA_fixtures$laliga_D <- (

  LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 + LALIGA_fixtures$laliga_4_4 +
    LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6
)

LALIGA_fixtures$laliga_D <- percent(LALIGA_fixtures$laliga_D, accuracy = 0.1)

#Away

LALIGA_fixtures$laliga_A <- (
  LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
    LALIGA_fixtures$laliga_0_5 + LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6
)

LALIGA_fixtures$laliga_A <- percent(LALIGA_fixtures$laliga_A, accuracy = 0.1)

#ov25
LALIGA_fixtures$laliga_ov25 <- (
  LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 + LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 + LALIGA_fixtures$laliga_0_4 +
    LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 + LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_0 +
    LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 + LALIGA_fixtures$laliga_0_5 +
    LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 + LALIGA_fixtures$laliga_5_5 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5 + LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 +
    LALIGA_fixtures$laliga_4_6 + LALIGA_fixtures$laliga_5_6 + LALIGA_fixtures$laliga_6_6
)
#un25
LALIGA_fixtures$laliga_un25 <- (
  LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_0_2
)
#odds
LALIGA_fixtures$laliga_ov25_odds <- round((1/LALIGA_fixtures$laliga_ov25),digits = 2)
LALIGA_fixtures$laliga_un25_odds <- round((1/LALIGA_fixtures$laliga_un25),digits = 2)

LALIGA_fixtures$laliga_ov25_odds
LALIGA_fixtures$laliga_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
LALIGA_fixtures$laliga_BTTSY <- (
  LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_3_1 + LALIGA_fixtures$laliga_3_2 +
    LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_1_3 + LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_3_3 + LALIGA_fixtures$laliga_4_4 +
    LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_3 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 +
    LALIGA_fixtures$laliga_3_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 +
    LALIGA_fixtures$laliga_5_4 + LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_6_6 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6
)
#BTTSN
LALIGA_fixtures$laliga_BTTSN <- (
  LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_0_2 +
    LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_5_0 +
    LALIGA_fixtures$laliga_0_5 + LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_0_6
)

LALIGA_fixtures$laliga_BTTSY_odds <- round((1/LALIGA_fixtures$laliga_BTTSY),digits = 2)
LALIGA_fixtures$laliga_BTTSN_odds <- round((1/LALIGA_fixtures$laliga_BTTSN),digits = 2)

LALIGA_fixtures$laliga_BTTSY <- percent(LALIGA_fixtures$laliga_BTTSY, accuracy = 0.1)
LALIGA_fixtures$laliga_BTTSN <- percent(LALIGA_fixtures$laliga_BTTSN, accuracy = 0.1)
#odds
LALIGA_fixtures$laliga_BTTSY_odds
LALIGA_fixtures$laliga_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
LALIGA_fixtures$laliga_AH_0_H <- (
  LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
    LALIGA_fixtures$laliga_5_0 +LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5 + LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6
)
#AH_0_A
LALIGA_fixtures$laliga_AH_0_A <- (
  LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
    LALIGA_fixtures$laliga_0_5 +LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6 + LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6
)

#odds
LALIGA_fixtures$laliga_AH_0_H_odds <- round((1/LALIGA_fixtures$laliga_AH_0_H),digits = 2)
LALIGA_fixtures$laliga_AH_0_A_odds <- round((1/LALIGA_fixtures$laliga_AH_0_A),digits = 2)

LALIGA_fixtures$laliga_AH_0_H_odds
LALIGA_fixtures$laliga_AH_0_A_odds
#percentages
LALIGA_fixtures$laliga_AH_0_H <- percent(LALIGA_fixtures$laliga_AH_0_H, accuracy = 0.1)
LALIGA_fixtures$laliga_AH_0_A <- percent(LALIGA_fixtures$laliga_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
LALIGA_fixtures$laliga_AH_n075_H <- (
  LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
    LALIGA_fixtures$laliga_5_0 +LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5
)
#AH_n075_A
LALIGA_fixtures$laliga_AH_n075_A <- (
  LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
    LALIGA_fixtures$laliga_0_5 +LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6
)

#odds
LALIGA_fixtures$laliga_AH_n075_H_odds <- round((1/LALIGA_fixtures$laliga_AH_n075_H),digits = 2)
LALIGA_fixtures$laliga_AH_n075_A_odds <- round((1/LALIGA_fixtures$laliga_AH_n075_A),digits = 2)

LALIGA_fixtures$laliga_AH_n075_H_odds
LALIGA_fixtures$laliga_AH_n075_A_odds
#percentages
LALIGA_fixtures$laliga_AH_n075_H <- percent(LALIGA_fixtures$laliga_AH_n075_H, accuracy = 0.1)
LALIGA_fixtures$laliga_AH_n075_A <- percent(LALIGA_fixtures$laliga_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
LALIGA_fixtures$laliga_AH_075_H <- (
  LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
    LALIGA_fixtures$laliga_5_0 +LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5 + LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6 + LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_1_2 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_3_4 + LALIGA_fixtures$laliga_4_5 + LALIGA_fixtures$laliga_5_6
)
#AH_075_A
LALIGA_fixtures$laliga_AH_075_A <- (
  LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
    LALIGA_fixtures$laliga_0_5 +LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6 + LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6 + LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_3 + LALIGA_fixtures$laliga_5_4 + LALIGA_fixtures$laliga_6_5
)

#odds
LALIGA_fixtures$laliga_AH_075_H_odds <- round((1/LALIGA_fixtures$laliga_AH_075_H),digits = 2)
LALIGA_fixtures$laliga_AH_075_A_odds <- round((1/LALIGA_fixtures$laliga_AH_075_A),digits = 2)

LALIGA_fixtures$laliga_AH_075_H_odds
LALIGA_fixtures$laliga_AH_075_A_odds
#percentages
LALIGA_fixtures$laliga_AH_075_H <- percent(LALIGA_fixtures$laliga_AH_075_H, accuracy = 0.1)
LALIGA_fixtures$laliga_AH_075_A <- percent(LALIGA_fixtures$laliga_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
LALIGA_fixtures$laliga_AH_n125_H <- (
  LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
    LALIGA_fixtures$laliga_5_0 +LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5
)
#AH_n125_A
LALIGA_fixtures$laliga_AH_n125_A <- (
  LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
    LALIGA_fixtures$laliga_0_5 +LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6
)

#odds
LALIGA_fixtures$laliga_AH_n125_H_odds <- round((1/LALIGA_fixtures$laliga_AH_n125_H),digits = 2)
LALIGA_fixtures$laliga_AH_n125_A_odds <- round((1/LALIGA_fixtures$laliga_AH_n125_A),digits = 2)

LALIGA_fixtures$laliga_AH_n125_H_odds
LALIGA_fixtures$laliga_AH_n125_A_odds
#percentages
LALIGA_fixtures$laliga_AH_n125_H <- percent(LALIGA_fixtures$laliga_AH_n125_H, accuracy = 0.1)
LALIGA_fixtures$laliga_AH_n125_A <- percent(LALIGA_fixtures$laliga_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
LALIGA_fixtures$laliga_AH_125_H <- (
  LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
    LALIGA_fixtures$laliga_5_0 +LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
    LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
    LALIGA_fixtures$laliga_6_5 + LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6 + LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_1_2 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_3_4 + LALIGA_fixtures$laliga_4_5 + LALIGA_fixtures$laliga_5_6
)
#AH_125_A
LALIGA_fixtures$laliga_AH_125_A <- (
  LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
    LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
    LALIGA_fixtures$laliga_0_5 +LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
    LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
    LALIGA_fixtures$laliga_5_6 + LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 +
    LALIGA_fixtures$laliga_4_4 + LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6 + LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_1 +
    LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_3 + LALIGA_fixtures$laliga_5_4 + LALIGA_fixtures$laliga_6_5
)

#odds
LALIGA_fixtures$laliga_AH_125_H_odds <- round((1/LALIGA_fixtures$laliga_AH_125_H),digits = 2)
LALIGA_fixtures$laliga_AH_125_A_odds <- round((1/LALIGA_fixtures$laliga_AH_125_A),digits = 2)

LALIGA_fixtures$laliga_AH_125_H_odds
LALIGA_fixtures$laliga_AH_125_A_odds
#percentages
LALIGA_fixtures$laliga_AH_125_H <- percent(LALIGA_fixtures$laliga_AH_125_H, accuracy = 0.1)
LALIGA_fixtures$laliga_AH_125_A <- percent(LALIGA_fixtures$laliga_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
LALIGA_fixtures$laliga_ov25 <- percent(LALIGA_fixtures$laliga_ov25, accuracy = 0.1)

LALIGA_fixtures$laliga_un25 <- percent(LALIGA_fixtures$laliga_un25, accuracy = 0.1)
LALIGA_fixtures$laliga_pscore <- paste(round(LALIGA_fixtures$laliga_xGH,digits = 0),round(LALIGA_fixtures$laliga_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
laliga_last_n_games <- 6

#create final_laliga_hf object
final_laliga_hf <- c()
for(index_laliga_hf in 1:length(laliga_teams))
{
  index_laliga_hf <- row.names(laliga_form_h) == laliga_teams[index_laliga_hf]
  form_laliga_hf <- laliga_form_h[index_laliga_hf]
  deleted_form_laliga_hf <- form_laliga_hf[!form_laliga_hf[] == ""]
  l6_form_laliga_hf <- tail(deleted_form_laliga_hf,laliga_last_n_games)
  l6_form_laliga_hf <- paste(l6_form_laliga_hf,collapse = " ")
  final_laliga_hf[index_laliga_hf] <- rbind(paste(laliga_teams[index_laliga_hf],l6_form_laliga_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}

#change column nam
final_laliga_hf <- as.data.frame(final_laliga_hf)
colnames(final_laliga_hf) <- "Form"
#goals scored
#create final_laliga_gs object
final_laliga_gs <- c()
suml6_laliga_gs <- c()
for(index_laliga_gs in 1:length(laliga_teams))
{
  index_laliga_gs <- row.names(laliga_goalscored_h) == laliga_teams[index_laliga_gs]
  form_laliga_gs <- laliga_goalscored_h[index_laliga_gs]
  deleted_form_laliga_gs <- form_laliga_gs[!form_laliga_gs[] == ""]
  l6_form_laliga_gs <- tail(deleted_form_laliga_gs,laliga_last_n_games)
  l6_form_laliga_gs <- as.numeric(l6_form_laliga_gs)
  suml6_laliga_gs[index_laliga_gs] <- sum(l6_form_laliga_gs)
  suml6_laliga_gs[index_laliga_gs] <- paste("(",suml6_laliga_gs[index_laliga_gs],")",sep = "")
  l6_form_laliga_gs <- paste(l6_form_laliga_gs,collapse = " ")
  final_laliga_gs[index_laliga_gs] <- rbind(paste(laliga_teams[index_laliga_gs],l6_form_laliga_gs,suml6_laliga_gs[index_laliga_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
final_laliga_gs
#change column names
final_laliga_gs <- as.data.frame(final_laliga_gs)
colnames(final_laliga_gs) <- "Goals scored"
#goal conceded
#create final_laliga_gc object
final_laliga_gc <- c()
suml6_laliga_gc <- c()
for(index_laliga_gc in 1:length(laliga_teams))
{
  index_laliga_gc <- row.names(laliga_goalconceded_h) == laliga_teams[index_laliga_gc]
  form_laliga_gc <- laliga_goalconceded_h[index_laliga_gc]
  deleted_form_laliga_gc <- form_laliga_gc[!form_laliga_gc[] == ""]
  l6_form_laliga_gc <- tail(deleted_form_laliga_gc,laliga_last_n_games)
  l6_form_laliga_gc <- as.numeric(l6_form_laliga_gc)
  suml6_laliga_gc[index_laliga_gc] <- sum(l6_form_laliga_gc)
  suml6_laliga_gc[index_laliga_gc] <- paste("(",suml6_laliga_gc[index_laliga_gc],")",sep = "")
  l6_form_laliga_gc <- paste(l6_form_laliga_gc,collapse = " ")
  final_laliga_gc[index_laliga_gc] <- rbind(paste(laliga_teams[index_laliga_gc],l6_form_laliga_gc,suml6_laliga_gc[index_laliga_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
#change column names
final_laliga_gc <- as.data.frame(final_laliga_gc)
colnames(final_laliga_gc) <- "Goals conceded"


toString(l6_form_laliga_gc)
#total goals
#create final_laliga_tg object
final_laliga_tg <- c()
suml6_laliga_tg <- c()
for(index_laliga_tg in 1:length(laliga_teams))
{
  index_laliga_tg <- row.names(laliga_totalgoals_h) == laliga_teams[index_laliga_tg]
  form_laliga_tg <- laliga_totalgoals_h[index_laliga_tg]
  deleted_form_laliga_tg <- form_laliga_tg[!form_laliga_tg[] == ""]
  l6_form_laliga_tg <- tail(deleted_form_laliga_tg,laliga_last_n_games)
  l6_form_laliga_tg <- as.numeric(l6_form_laliga_tg)
  suml6_laliga_tg[index_laliga_tg] <- sum(l6_form_laliga_tg)
  suml6_laliga_tg[index_laliga_tg] <- paste("(",suml6_laliga_tg[index_laliga_tg],")",sep = "")
  l6_form_laliga_tg <- paste(l6_form_laliga_tg,collapse = " ")
  final_laliga_tg[index_laliga_tg] <- rbind(paste(laliga_teams[index_laliga_tg],l6_form_laliga_tg,suml6_laliga_tg[index_laliga_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
#change column names
final_laliga_tg <- as.data.frame(final_laliga_tg)
colnames(final_laliga_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_laliga_hf object
final_laliga_cs <- c()
for(index_laliga_cs in 1:length(laliga_teams))
{
  index_laliga_cs <- row.names(laliga_csform_h) == laliga_teams[index_laliga_cs]
  csform_laliga_cs <- laliga_csform_h[index_laliga_cs]
  deleted_csform_laliga_cs <- csform_laliga_cs[!csform_laliga_cs[] == ""]
  l6_csform_laliga_cs <- tail(deleted_csform_laliga_cs,laliga_last_n_games)
  l6_csform_laliga_cs <- paste(l6_csform_laliga_cs,collapse = " ")
  final_laliga_cs[index_laliga_cs] <- rbind(paste(laliga_teams[index_laliga_cs],l6_csform_laliga_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",laliga_teams[index],l6_csform)

}

#change column names
final_laliga_cs <- as.data.frame(final_laliga_cs)
colnames(final_laliga_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_laliga_wm object
final_laliga_wm <- c()
suml6_laliga_wm <- c()
for(index_laliga_wm in 1:length(laliga_teams))
{
  index_laliga_wm <- row.names(laliga_winmargin_h) == laliga_teams[index_laliga_wm]
  form_laliga_wm <- laliga_winmargin_h[index_laliga_wm]
  deleted_form_laliga_wm <- form_laliga_wm[!form_laliga_wm[] == ""]
  l6_form_laliga_wm <- tail(deleted_form_laliga_wm,laliga_last_n_games)
  l6_form_laliga_wm <- as.numeric(l6_form_laliga_wm)
  suml6_laliga_wm[index_laliga_wm] <- sum(l6_form_laliga_wm)
  suml6_laliga_wm[index_laliga_wm] <- paste("(",suml6_laliga_wm[index_laliga_wm],")",sep = "")
  l6_form_laliga_wm <- paste(l6_form_laliga_wm,collapse = " ")
  final_laliga_wm[index_laliga_wm] <- rbind(paste(laliga_teams[index_laliga_wm],l6_form_laliga_wm,suml6_laliga_wm[index_laliga_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
final_laliga_wm
#change column names
final_laliga_wm <- as.data.frame(final_laliga_wm)
colnames(final_laliga_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_laliga_ca object
final_laliga_ca <- c()
suml6_laliga_ca <- c()
for(index_laliga_ca in 1:length(laliga_teams))
{
  index_laliga_ca <- row.names(laliga_coawarded_h) == laliga_teams[index_laliga_ca]
  form_laliga_ca <- laliga_coawarded_h[index_laliga_ca]
  deleted_form_laliga_ca <- form_laliga_ca[!form_laliga_ca[] == ""]
  l6_form_laliga_ca <- tail(deleted_form_laliga_ca,laliga_last_n_games)
  l6_form_laliga_ca <- as.numeric(l6_form_laliga_ca)
  suml6_laliga_ca[index_laliga_ca] <- sum(l6_form_laliga_ca)
  suml6_laliga_ca[index_laliga_ca] <- paste("(",suml6_laliga_ca[index_laliga_ca],")",sep = "")
  l6_form_laliga_ca <- paste(l6_form_laliga_ca,collapse = " ")
  final_laliga_ca[index_laliga_ca] <- rbind(paste(laliga_teams[index_laliga_ca],l6_form_laliga_ca,suml6_laliga_ca[index_laliga_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
final_laliga_ca
#change column names
final_laliga_ca <- as.data.frame(final_laliga_ca)
colnames(final_laliga_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_laliga_ca object
final_laliga_cc <- c()
suml6_laliga_cc <- c()
for(index_laliga_cc in 1:length(laliga_teams))
{
  index_laliga_cc <- row.names(laliga_cornersconceded_h) == laliga_teams[index_laliga_cc]
  form_laliga_cc <- laliga_cornersconceded_h[index_laliga_cc]
  deleted_form_laliga_cc <- form_laliga_cc[!form_laliga_cc[] == ""]
  l6_form_laliga_cc <- tail(deleted_form_laliga_cc,laliga_last_n_games)
  l6_form_laliga_cc <- as.numeric(l6_form_laliga_cc)
  suml6_laliga_cc[index_laliga_cc] <- sum(l6_form_laliga_cc)
  suml6_laliga_cc[index_laliga_cc] <- paste("(",suml6_laliga_cc[index_laliga_cc],")",sep = "")
  l6_form_laliga_cc <- paste(l6_form_laliga_cc,collapse = " ")
  final_laliga_cc[index_laliga_cc] <- rbind(paste(laliga_teams[index_laliga_cc],l6_form_laliga_cc,suml6_laliga_cc[index_laliga_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
final_laliga_cc
#change column names
final_laliga_cc <- as.data.frame(final_laliga_cc)
colnames(final_laliga_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_laliga_cosc <- c()
for(index_laliga_cosc in 1:length(laliga_teams))
{
  index_laliga_cosc <- row.names(laliga_coscform_h) == laliga_teams[index_laliga_cosc]
  coscform_laliga_cosc <- laliga_coscform_h[index_laliga_cosc]
  deleted_coscform_laliga_cosc <- coscform_laliga_cosc[!coscform_laliga_cosc[] == ""]
  l6_coscform_laliga_cosc <- tail(deleted_coscform_laliga_cosc,laliga_last_n_games)
  l6_coscform_laliga_cosc <- paste(l6_coscform_laliga_cosc,collapse = " ")
  final_laliga_cosc[index_laliga_cosc] <- rbind(paste(laliga_teams[index_laliga_cosc],l6_coscform_laliga_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",laliga_teams[index],l6_coscform)

}
final_laliga_cosc
#change column names
final_laliga_cosc <- as.data.frame(final_laliga_cosc)
colnames(final_laliga_cosc) <- "CornersForm"
##################################################
#total corners
#create final_laliga_tcorners object
final_laliga_tcorners <- c()
suml6_laliga_tcorners <- c()
for(index_laliga_tcorners in 1:length(laliga_teams))
{
  index_laliga_tcorners <- row.names(laliga_totalcorners_h) == laliga_teams[index_laliga_tcorners]
  form_laliga_tcorners <- laliga_totalcorners_h[index_laliga_tcorners]
  deleted_form_laliga_tcorners <- form_laliga_tcorners[!form_laliga_tcorners[] == ""]
  l6_form_laliga_tcorners <- tail(deleted_form_laliga_tcorners,laliga_last_n_games)
  l6_form_laliga_tcorners <- as.numeric(l6_form_laliga_tcorners)
  suml6_laliga_tcorners[index_laliga_tcorners] <- sum(l6_form_laliga_tcorners)
  suml6_laliga_tcorners[index_laliga_tcorners] <- paste("(",suml6_laliga_tcorners[index_laliga_tcorners],")",sep = "")
  l6_form_laliga_tcorners <- paste(l6_form_laliga_tcorners,collapse = " ")
  final_laliga_tcorners[index_laliga_tcorners] <- rbind(paste(laliga_teams[index_laliga_tcorners],l6_form_laliga_tcorners,suml6_laliga_tcorners[index_laliga_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
#change column names
final_laliga_tcorners <- as.data.frame(final_laliga_tcorners)
colnames(final_laliga_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_laliga_hf_against
final_laliga_hf_against <- c()
for(index_laliga_hf_against in 1:length(laliga_teams))
{
  index_laliga_hf_against <- row.names(laliga_form_team_against_h) == laliga_teams[index_laliga_hf_against]
  form_laliga_hf_against <- laliga_form_team_against_h[index_laliga_hf_against]
  deleted_form_laliga_hf_against <- form_laliga_hf_against[!form_laliga_hf_against[] == ""]
  l6_form_laliga_hf_against <- tail(deleted_form_laliga_hf_against,laliga_last_n_games)
  l6_form_laliga_hf_against <- paste(l6_form_laliga_hf_against,collapse = " ")
  final_laliga_hf_against[index_laliga_hf_against] <- rbind(paste(laliga_teams[index_laliga_hf_against],l6_form_laliga_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laliga_teams[index],l6_form)

}
final_laliga_hf_against <- as.data.frame(final_laliga_hf_against)
colnames(final_laliga_hf_against) <- "Team against"
#combine the columns
final_laliga_all <- cbind(final_laliga_hf,final_laliga_gs,final_laliga_gc,final_laliga_tg,final_laliga_ca,final_laliga_cc,final_laliga_tcorners,final_laliga_cosc,final_laliga_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#LALIGA
LALIGA_sim <- LALIGA
LALIGA_sim$matchid <- paste(LALIGA_sim$HomeTeam,LALIGA_sim$AwayTeam,sep = "-")
LALIGA_fixtures$matchid <- paste(LALIGA_fixtures$HomeTeam_laliga,LALIGA_fixtures$AwayTeam_laliga,sep = "-")
LALIGA_fixtures$laliga_FTR <- sapply(LALIGA_fixtures$laliga_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

LALIGA_fixtures$laliga_gamestatus <- ifelse(LALIGA_fixtures$matchid %in% LALIGA_sim$matchid,"played","notplayed")

laliga_home_wins_sim <- c()
laliga_away_wins_sim <- c()
laliga_home_draws_sim <- c()
laliga_away_draws_sim <- c()
laliga_home_loss_sim <- c()
laliga_away_loss_sim <- c()



for (i_laliga_wins_sim in 1:length(laliga_teams))
{

  laliga_home_wins_sim[i_laliga_wins_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$HomeTeam_laliga == laliga_teams[i_laliga_wins_sim] & LALIGA_fixtures$laliga_FTR == "H" & LALIGA_fixtures$laliga_gamestatus =="notplayed",])
  laliga_away_wins_sim[i_laliga_wins_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$AwayTeam_laliga == laliga_teams[i_laliga_wins_sim] & LALIGA_fixtures$laliga_FTR == "A" & LALIGA_fixtures$laliga_gamestatus == "notplayed",])
  laliga_home_draws_sim[i_laliga_wins_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$HomeTeam_laliga == laliga_teams[i_laliga_wins_sim] & LALIGA_fixtures$laliga_FTR == "D" & LALIGA_fixtures$laliga_gamestatus == "notplayed",])
  laliga_away_draws_sim[i_laliga_wins_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$AwayTeam_laliga == laliga_teams[i_laliga_wins_sim] & LALIGA_fixtures$laliga_FTR == "D" & LALIGA_fixtures$laliga_gamestatus == "notplayed",])
  laliga_home_loss_sim[i_laliga_wins_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$HomeTeam_laliga == laliga_teams[i_laliga_wins_sim] & LALIGA_fixtures$laliga_FTR == "A" & LALIGA_fixtures$laliga_gamestatus == "notplayed",])
  laliga_away_loss_sim[i_laliga_wins_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$AwayTeam_laliga == laliga_teams[i_laliga_wins_sim] & LALIGA_fixtures$laliga_FTR == "H" & LALIGA_fixtures$laliga_gamestatus == "notplayed", ])

}

laliga_total_wins_sim <- laliga_home_wins_sim + laliga_away_wins_sim
laliga_total_draws_sim <- laliga_home_draws_sim + laliga_away_draws_sim
laliga_total_loss_sim <- laliga_home_loss_sim + laliga_away_loss_sim

laliga_home_games_sim <- c()
laliga_away_games_sim <-c()

for (i_laliga_sim in 1:length(laliga_teams))
{

  laliga_home_games_sim[i_laliga_sim] <- nrow(LALIGA_fixtures[LALIGA_fixtures$HomeTeam_laliga == laliga_teams[i_laliga_sim] & LALIGA_fixtures$laliga_gamestatus == "notplayed",])
  laliga_away_games_sim[i_laliga_sim]  <- nrow(LALIGA_fixtures[LALIGA_fixtures$AwayTeam_laliga == laliga_teams[i_laliga_sim] & LALIGA_fixtures$laliga_gamestatus == "notplayed",])

}

laliga_games_played_sim <- laliga_home_games_sim + laliga_away_games_sim

laliga_league_table_sim <- cbind(laliga_teams,laliga_games_played_sim,laliga_total_wins_sim,laliga_total_draws_sim,laliga_total_loss_sim)
laliga_PTS_sim <- (laliga_total_wins_sim*3) + (laliga_total_draws_sim*1)
laliga_league_table_sim <- cbind(laliga_league_table_sim,laliga_PTS_sim)

laliga_games_played_simfinal <- laliga_games_played + laliga_games_played_sim
laliga_total_wins_simfinal <- laliga_total_wins + laliga_total_wins_sim
laliga_total_draws_simfinal <- laliga_total_draws + laliga_total_draws_sim
laliga_total_loss_simfinal <- laliga_total_loss + laliga_total_loss_sim
laliga_PTS_simfinal <- laliga_PTS + laliga_PTS_sim

laliga_league_table_simfinal <- cbind(laliga_teams,laliga_games_played_simfinal,laliga_total_wins_simfinal,laliga_total_draws_simfinal,laliga_total_loss_simfinal,laliga_PTS_simfinal)
laliga_league_table_simfinal <- as.data.frame(laliga_league_table_simfinal)
names(laliga_league_table_simfinal)[names(laliga_league_table_simfinal) == "laliga_teams"] <- "Team_f"
names(laliga_league_table_simfinal)[names(laliga_league_table_simfinal) == "laliga_games_played_simfinal"] <- "P_f"
names(laliga_league_table_simfinal)[names(laliga_league_table_simfinal) == "laliga_total_wins_simfinal"] <- "W_f"
names(laliga_league_table_simfinal)[names(laliga_league_table_simfinal) == "laliga_total_draws_simfinal"] <- "D_f"
names(laliga_league_table_simfinal)[names(laliga_league_table_simfinal) == "laliga_total_loss_simfinal"] <- "L_f"
names(laliga_league_table_simfinal)[names(laliga_league_table_simfinal) == "laliga_PTS_simfinal"] <- "PTS_f"
points_laliga_sim <-  laliga_league_table_simfinal[order(as.numeric(laliga_league_table_simfinal$PTS_f), decreasing = TRUE),]

LALIGA_notplayed <- LALIGA_fixtures[LALIGA_fixtures$laliga_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#LALIGA
LALIGA_fixtures$Hometeam_laliga_index <- match(LALIGA_fixtures$HomeTeam_laliga,laliga_teams)
LALIGA_fixtures$Awayteam_laliga_index <- match(LALIGA_fixtures$AwayTeam_laliga,laliga_teams)
laliga_prediction <- c()
laliga_HWM <- c()
laliga_AWM <- c()
laliga_HWMLM <- c()
laliga_AWMLM <- c()
laliga_HY <- c()
laliga_AY <- c()
laliga_HCO <- c()
laliga_ACO <- c()
laliga_HXSC <- c()
laliga_AXSC <- c()
laliga_HYCPF <- c()
laliga_AYCPF <- c()
for(laliga_row in 1:nrow(LALIGA_fixtures))
{

  laliga_hometeamindex <- LALIGA_fixtures[laliga_row,"Hometeam_laliga_index"]
  laliga_awayteamindex <- LALIGA_fixtures[laliga_row,"Awayteam_laliga_index"]
  #analyse team form
  #home team
  laliga_form_vec_ht <- as.vector(laliga_form_h[laliga_hometeamindex,])
  laliga_form_vec_ht[is.na(laliga_form_vec_ht)] <- ""
  laliga_form_vec_ht <- laliga_form_vec_ht[laliga_form_vec_ht != ""]
  laliga_form_vec_ht  <-tail(laliga_form_vec_ht,6)
  laliga_ht_numberof_wins <- length(which(laliga_form_vec_ht == "W"))
  laliga_ht_numberof_draws <- length(which(laliga_form_vec_ht == "D"))
  laliga_ht_numberof_loss <- length(which(laliga_form_vec_ht == "L"))
  #awayteam
  laliga_form_vec_at <- as.vector(laliga_form_h[laliga_awayteamindex,])
  laliga_form_vec_at[is.na(laliga_form_vec_at)] <- ""
  laliga_form_vec_at <- laliga_form_vec_at[laliga_form_vec_at != ""]
  laliga_form_vec_at  <-tail(laliga_form_vec_at,6)
  laliga_at_numberof_wins <- length(which(laliga_form_vec_at == "W"))
  laliga_at_numberof_draws <- length(which(laliga_form_vec_at == "D"))
  laliga_at_numberof_loss <- length(which(laliga_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  laliga_goalscored_vec_ht <- as.vector(laliga_goalscored_h[laliga_hometeamindex,])
  laliga_goalscored_vec_ht[is.na(laliga_goalscored_vec_ht)] <- ""
  laliga_goalscored_vec_ht <- laliga_goalscored_vec_ht[laliga_goalscored_vec_ht != ""]
  laliga_goalscored_vec_ht  <-tail(laliga_goalscored_vec_ht,6)
  laliga_goalscored_vec_ht  <- as.numeric(laliga_goalscored_vec_ht)
  laliga_ht_totalgoalscored <- sum(laliga_goalscored_vec_ht)
  laliga_ht_matches_scoring <- length(which(laliga_goalscored_vec_ht > 0))
  laliga_ht_matches_without_scoring <- length(which(laliga_goalscored_vec_ht == "0"))
  #awayteam
  laliga_goalscored_vec_at <- as.vector(laliga_goalscored_h[laliga_awayteamindex,])
  laliga_goalscored_vec_at[is.na(laliga_goalscored_vec_at)] <- ""
  laliga_goalscored_vec_at <- laliga_goalscored_vec_at[laliga_goalscored_vec_at != ""]
  laliga_goalscored_vec_at  <-tail(laliga_goalscored_vec_at,6)
  laliga_goalscored_vec_at  <- as.numeric(laliga_goalscored_vec_at)
  laliga_at_totalgoalscored <- sum(laliga_goalscored_vec_at)
  laliga_at_matches_scoring <- length(which(laliga_goalscored_vec_at > 0))
  laliga_at_matches_without_scoring <- length(which(laliga_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  laliga_goalconceded_vec_ht <- as.vector(laliga_goalconceded_h[laliga_hometeamindex,])
  laliga_goalconceded_vec_ht[is.na(laliga_goalconceded_vec_ht)] <- ""
  laliga_goalconceded_vec_ht <- laliga_goalconceded_vec_ht[laliga_goalconceded_vec_ht != ""]
  laliga_goalconceded_vec_ht  <-tail(laliga_goalconceded_vec_ht,6)
  laliga_goalconceded_vec_ht  <- as.numeric(laliga_goalconceded_vec_ht)
  laliga_goalconceded_vec_ht
  laliga_ht_totalgoalconceded <- sum(laliga_goalconceded_vec_ht)
  laliga_ht_matches_concede <- length(which(laliga_goalconceded_vec_ht > 0))
  laliga_ht_matches_without_concede <- length(which(laliga_goalconceded_vec_ht == "0"))
  #awayteam
  laliga_goalconceded_vec_at <- as.vector(laliga_goalconceded_h[laliga_awayteamindex,])
  laliga_goalconceded_vec_at[is.na(laliga_goalconceded_vec_at)] <- ""
  laliga_goalconceded_vec_at <- laliga_goalconceded_vec_at[laliga_goalconceded_vec_at != ""]
  laliga_goalconceded_vec_at  <-tail(laliga_goalconceded_vec_at,6)
  laliga_goalconceded_vec_at  <- as.numeric(laliga_goalconceded_vec_at)
  laliga_at_totalgoalconceded <- sum(laliga_goalconceded_vec_at)
  laliga_at_matches_concede <- length(which(laliga_goalconceded_vec_at > 0))
  laliga_at_matches_without_concede <- length(which(laliga_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  laliga_totalgoals_vec_ht <- as.vector(laliga_totalgoals_h[laliga_hometeamindex,])
  laliga_totalgoals_vec_ht[is.na(laliga_totalgoals_vec_ht)] <- ""
  laliga_totalgoals_vec_ht <- laliga_totalgoals_vec_ht[laliga_totalgoals_vec_ht != ""]
  laliga_totalgoals_vec_ht  <-tail(laliga_totalgoals_vec_ht,6)
  laliga_totalgoals_vec_ht  <- as.numeric(laliga_totalgoals_vec_ht)
  laliga_totalgoals_vec_ht
  laliga_ht_totalgoals <- sum(laliga_totalgoals_vec_ht)
  laliga_ht_avgtotalgoals <- (laliga_ht_totalgoals/6)
  laliga_ht_no_of_ov25 <- length(which(laliga_totalgoals_vec_ht >= 3))
  laliga_ht_no_of_un25 <- length(which(laliga_totalgoals_vec_ht <= 2))
  #awayteam
  laliga_totalgoals_vec_at <- as.vector(laliga_totalgoals_h[laliga_awayteamindex,])
  laliga_totalgoals_vec_at[is.na(laliga_totalgoals_vec_at)] <- ""
  laliga_totalgoals_vec_at <- laliga_totalgoals_vec_at[laliga_totalgoals_vec_at != ""]
  laliga_totalgoals_vec_at  <-tail(laliga_totalgoals_vec_at,6)
  laliga_totalgoals_vec_at  <- as.numeric(laliga_totalgoals_vec_at)
  laliga_totalgoals_vec_at
  laliga_at_totalgoals <- sum(laliga_totalgoals_vec_at)
  laliga_at_avgtotalgoals <- (laliga_at_totalgoals/6)
  laliga_at_no_of_ov25 <- length(which(laliga_totalgoals_vec_at >= 3))
  laliga_at_no_of_un25 <- length(which(laliga_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  laliga_winmargin_vec_ht <- as.vector(laliga_winmargin_h[laliga_hometeamindex,])
  laliga_winmargin_vec_ht[is.na(laliga_winmargin_vec_ht)] <- ""
  laliga_winmargin_vec_ht <- laliga_winmargin_vec_ht[laliga_winmargin_vec_ht != ""]
  laliga_winmargin_vec_ht  <-tail(laliga_winmargin_vec_ht,6)
  laliga_winmargin_vec_ht  <- as.numeric(laliga_winmargin_vec_ht)

  laliga_ht_totalwinmargin <- sum(laliga_winmargin_vec_ht)
  laliga_ht_no_of_winmargin_ov0 <- length(which(laliga_winmargin_vec_ht >= 0))
  laliga_ht_no_of_winmargin_ov1 <- length(which(laliga_winmargin_vec_ht >= 1))
  laliga_ht_no_of_winmargin_un0 <- length(which(laliga_winmargin_vec_ht <= 0))
  laliga_ht_no_of_winmargin_un1 <- length(which(laliga_winmargin_vec_ht <= 1))
  #awayteam
  laliga_winmargin_vec_at <- as.vector(laliga_winmargin_h[laliga_awayteamindex,])
  laliga_winmargin_vec_at[is.na(laliga_winmargin_vec_at)] <- ""
  laliga_winmargin_vec_at <- laliga_winmargin_vec_at[laliga_winmargin_vec_at != ""]
  laliga_winmargin_vec_at  <-tail(laliga_winmargin_vec_at,6)
  laliga_winmargin_vec_at  <- as.numeric(laliga_winmargin_vec_at)

  laliga_at_totalwinmargin <- sum(laliga_winmargin_vec_at)
  laliga_at_no_of_winmargin_ov0 <- length(which(laliga_winmargin_vec_at >= 0))
  laliga_at_no_of_winmargin_ov1 <- length(which(laliga_winmargin_vec_at >= 1))
  laliga_at_no_of_winmargin_un0 <- length(which(laliga_winmargin_vec_at <= 0))
  laliga_at_no_of_winmargin_un1 <- length(which(laliga_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  laliga_winmargin_vec_ht_lm <- as.vector(laliga_winmargin_h[laliga_hometeamindex,])
  laliga_winmargin_vec_ht_lm[is.na(laliga_winmargin_vec_ht_lm)] <- ""
  laliga_winmargin_vec_ht_lm <- laliga_winmargin_vec_ht_lm[laliga_winmargin_vec_ht_lm != ""]
  laliga_winmargin_vec_ht_lm  <-tail(laliga_winmargin_vec_ht_lm,1)
  #awayteam
  laliga_winmargin_vec_at_lm <- as.vector(laliga_winmargin_h[laliga_awayteamindex,])
  laliga_winmargin_vec_at_lm[is.na(laliga_winmargin_vec_at_lm)] <- ""
  laliga_winmargin_vec_at_lm <- laliga_winmargin_vec_at_lm[laliga_winmargin_vec_at_lm != ""]
  laliga_winmargin_vec_at_lm  <-tail(laliga_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  laliga_yellowtotals_vec_ht <- as.vector(laliga_yellowtotalsv2[laliga_hometeamindex,])
  laliga_yellowtotals_vec_ht[is.na(laliga_yellowtotals_vec_ht)] <- ""
  laliga_yellowtotals_vec_ht <- laliga_yellowtotals_vec_ht[laliga_yellowtotals_vec_ht != ""]
  laliga_yellowtotals_vec_ht  <-tail(laliga_yellowtotals_vec_ht,1)
  #awayteam
  laliga_yellowtotals_vec_at <- as.vector(laliga_yellowtotalsv2[laliga_awayteamindex,])
  laliga_yellowtotals_vec_at[is.na(laliga_yellowtotals_vec_at)] <- ""
  laliga_yellowtotals_vec_at <- laliga_yellowtotals_vec_at[laliga_yellowtotals_vec_at != ""]
  laliga_yellowtotals_vec_at  <-tail(laliga_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  laliga_cornertotals_vec_ht <- as.vector(laliga_cornertotalsv2[laliga_hometeamindex,])
  laliga_cornertotals_vec_ht[is.na(laliga_cornertotals_vec_ht)] <- ""
  laliga_cornertotals_vec_ht <- laliga_cornertotals_vec_ht[laliga_cornertotals_vec_ht != ""]
  laliga_cornertotals_vec_ht  <-tail(laliga_cornertotals_vec_ht,1)
  #awayteam
  laliga_cornertotals_vec_at <- as.vector(laliga_cornertotalsv2[laliga_awayteamindex,])
  laliga_cornertotals_vec_at[is.na(laliga_cornertotals_vec_at)] <- ""
  laliga_cornertotals_vec_at <- laliga_cornertotals_vec_at[laliga_cornertotals_vec_at != ""]
  laliga_cornertotals_vec_at  <-tail(laliga_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  laliga_xshotsconversion_vec_ht <- as.vector(laliga_shots_analysis[laliga_hometeamindex,])
  laliga_xshotsconversion_vec_ht[is.na(laliga_xshotsconversion_vec_ht)] <- ""
  laliga_xshotsconversion_vec_ht <- laliga_xshotsconversion_vec_ht[laliga_xshotsconversion_vec_ht != ""]
  laliga_xshotsconversion_vec_ht  <-tail(laliga_xshotsconversion_vec_ht,1)
  #awayteam
  laliga_xshotsconversion_vec_at <- as.vector(laliga_shots_analysis[laliga_awayteamindex,])
  laliga_xshotsconversion_vec_at[is.na(laliga_xshotsconversion_vec_at)] <- ""
  laliga_xshotsconversion_vec_at <- laliga_xshotsconversion_vec_at[laliga_xshotsconversion_vec_at != ""]
  laliga_xshotsconversion_vec_at  <-tail(laliga_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  laliga_fouls_conversion_vec_ht <- as.vector(laliga_fouls_conversion[laliga_hometeamindex,])
  laliga_fouls_conversion_vec_ht[is.na(laliga_fouls_conversion_vec_ht)] <- ""
  laliga_fouls_conversion_vec_ht <- laliga_fouls_conversion_vec_ht[laliga_fouls_conversion_vec_ht != ""]
  laliga_fouls_conversion_vec_ht  <-tail(laliga_fouls_conversion_vec_ht,1)
  #awayteam
  laliga_fouls_conversion_vec_at <- as.vector(laliga_fouls_conversion[laliga_awayteamindex,])
  laliga_fouls_conversion_vec_at[is.na(laliga_fouls_conversion_vec_at)] <- ""
  laliga_fouls_conversion_vec_at <- laliga_fouls_conversion_vec_at[laliga_fouls_conversion_vec_at != ""]
  laliga_fouls_conversion_vec_at  <-tail(laliga_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  laliga_ht_last6points <- laliga_ht_numberof_wins*3 + laliga_ht_numberof_draws*1
  laliga_at_last6points <- laliga_at_numberof_wins*3 + laliga_at_numberof_draws*1

  if(laliga_ht_last6points > laliga_at_last6points) {laliga_3waypick <- "1"}  else {laliga_3waypick <- "X2"}

  if(laliga_at_last6points > laliga_ht_last6points ) {laliga_3waypick <- "2"} else {laliga_3waypick <- "1X"}

  if(laliga_ht_no_of_ov25 + laliga_at_no_of_ov25 >= 6) {laliga_goalspick <- "ov25"} else {laliga_goalspick <- "un25"}

  if(laliga_ht_no_of_un25 + laliga_at_no_of_un25 >= 6) {laliga_goalspick <- "un25"} else {laliga_goalspick <- "ov25"}

  if(laliga_ht_matches_scoring >= 4 && laliga_at_matches_scoring >=4) {laliga_btts <- "BTTS-Y"} else {laliga_btts <- "BTTS-N"}


  laliga_prediction[laliga_row] <- rbind(paste(laliga_3waypick,laliga_goalspick,laliga_btts,sep = ","))
  laliga_HWM[laliga_row] <- laliga_ht_totalwinmargin
  laliga_AWM[laliga_row] <- laliga_at_totalwinmargin

  laliga_HWMLM[laliga_row] <- laliga_winmargin_vec_ht_lm
  laliga_AWMLM[laliga_row] <- laliga_winmargin_vec_at_lm

  laliga_HY[laliga_row] <- laliga_yellowtotals_vec_ht
  laliga_AY[laliga_row] <- laliga_yellowtotals_vec_at

  laliga_HCO[laliga_row] <- laliga_cornertotals_vec_ht
  laliga_ACO[laliga_row] <- laliga_cornertotals_vec_at

  laliga_HXSC[laliga_row] <- laliga_xshotsconversion_vec_ht
  laliga_AXSC[laliga_row] <- laliga_xshotsconversion_vec_at

  laliga_HYCPF[laliga_row] <- laliga_fouls_conversion_vec_ht
  laliga_AYCPF[laliga_row] <- laliga_fouls_conversion_vec_at
}

laliga_prediction <- as.data.frame(laliga_prediction)
colnames(laliga_prediction) <- "prediction"

laliga_HWM <- as.data.frame(laliga_HWM)
colnames(laliga_HWM) <- "HWM"

laliga_AWM <- as.data.frame(laliga_AWM)
colnames(laliga_AWM) <- "AWM"

laliga_HWMLM <- as.data.frame(laliga_HWMLM)
colnames(laliga_HWMLM) <- "HWMLM"

laliga_AWMLM <- as.data.frame(laliga_AWMLM)
colnames(laliga_AWMLM) <- "AWMLM"

laliga_HY <- as.data.frame(laliga_HY)
colnames(laliga_HY) <- "AVGHY"

laliga_AY <- as.data.frame(laliga_AY)
colnames(laliga_AY) <- "AVGAY"

laliga_HCO <- as.data.frame(laliga_HCO)
colnames(laliga_HCO) <- "AVGHCO"

laliga_ACO <- as.data.frame(laliga_ACO)
colnames(laliga_ACO) <- "AVGACO"

laliga_HXSC <- as.data.frame(laliga_HXSC)
colnames(laliga_HXSC) <- "HXSC"

laliga_AXSC <- as.data.frame(laliga_AXSC)
colnames(laliga_AXSC) <- "AXSC"

laliga_HYCPF <- as.data.frame(laliga_HYCPF)
colnames(laliga_HYCPF) <- "HYCPF"

laliga_AYCPF <- as.data.frame(laliga_AYCPF)
colnames(laliga_AYCPF) <- "AYCPF"

laliga_picks <- cbind(LALIGA_fixtures$Div,LALIGA_fixtures$HomeTeam_laliga,LALIGA_fixtures$AwayTeam_laliga,laliga_prediction,laliga_HWM,laliga_AWM,laliga_HWMLM,laliga_AWMLM,laliga_HY,laliga_AY,laliga_HCO,laliga_ACO,laliga_HXSC,laliga_AXSC,laliga_HYCPF,laliga_AYCPF)

colnames(laliga_picks)[1] <- "picks_Div"
colnames(laliga_picks)[2] <- "picks_HomeTeam"
colnames(laliga_picks)[3] <- "picks_AwayTeam"
laliga_picks$matchid <- paste(laliga_picks$picks_HomeTeam,laliga_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of LALIGA
laliga_picks
#############################################################################################################################################################################
#clone fixtures
LALIGA_fixtures_clone <- LALIGA_fixtures
colnames(LALIGA_fixtures_clone)[61] <- "Hwin"
colnames(LALIGA_fixtures_clone)[62] <- "Draw"
colnames(LALIGA_fixtures_clone)[63] <- "Awin"

LALIGA_fixtures_clone$Hwinodds <-   LALIGA_fixtures$laliga_1_0 + LALIGA_fixtures$laliga_2_0 + LALIGA_fixtures$laliga_2_1 + LALIGA_fixtures$laliga_3_0 + LALIGA_fixtures$laliga_3_1 +
  LALIGA_fixtures$laliga_3_2 + LALIGA_fixtures$laliga_4_0 + LALIGA_fixtures$laliga_4_1 + LALIGA_fixtures$laliga_4_2 + LALIGA_fixtures$laliga_4_3 +
  LALIGA_fixtures$laliga_5_0 + LALIGA_fixtures$laliga_5_1 + LALIGA_fixtures$laliga_5_2 + LALIGA_fixtures$laliga_5_3 + LALIGA_fixtures$laliga_5_4 +
  LALIGA_fixtures$laliga_6_0 + LALIGA_fixtures$laliga_6_1 + LALIGA_fixtures$laliga_6_2 + LALIGA_fixtures$laliga_6_3 + LALIGA_fixtures$laliga_6_4 +
  LALIGA_fixtures$laliga_6_5
LALIGA_fixtures_clone$Hwinodds <- round(1/LALIGA_fixtures_clone$Hwinodds, digits = 3)

LALIGA_fixtures_clone$Drawodds <-  LALIGA_fixtures$laliga_0_0 + LALIGA_fixtures$laliga_1_1 + LALIGA_fixtures$laliga_2_2 + LALIGA_fixtures$laliga_3_3 + LALIGA_fixtures$laliga_4_4 +
  LALIGA_fixtures$laliga_5_5 + LALIGA_fixtures$laliga_6_6

LALIGA_fixtures_clone$Drawodds <- round(1/LALIGA_fixtures_clone$Drawodds, digits = 3)

LALIGA_fixtures_clone$Awinodds <-   LALIGA_fixtures$laliga_0_1 + LALIGA_fixtures$laliga_0_2 + LALIGA_fixtures$laliga_1_2 + LALIGA_fixtures$laliga_0_3 + LALIGA_fixtures$laliga_1_3 +
  LALIGA_fixtures$laliga_2_3 + LALIGA_fixtures$laliga_0_4 + LALIGA_fixtures$laliga_1_4 + LALIGA_fixtures$laliga_2_4 + LALIGA_fixtures$laliga_3_4 +
  LALIGA_fixtures$laliga_0_5 + LALIGA_fixtures$laliga_1_5 + LALIGA_fixtures$laliga_2_5 + LALIGA_fixtures$laliga_3_5 + LALIGA_fixtures$laliga_4_5 +
  LALIGA_fixtures$laliga_0_6 + LALIGA_fixtures$laliga_1_6 + LALIGA_fixtures$laliga_2_6 + LALIGA_fixtures$laliga_3_6 + LALIGA_fixtures$laliga_4_6 +
  LALIGA_fixtures$laliga_5_6

LALIGA_fixtures_clone$Awinodds <- round(1/LALIGA_fixtures_clone$Awinodds, digits = 3)

colnames(LALIGA_fixtures_clone)[15] <- "CS_1-1"
colnames(LALIGA_fixtures_clone)[13] <- "CS_1-0"
colnames(LALIGA_fixtures_clone)[14] <- "CS_0-1"
colnames(LALIGA_fixtures_clone)[16] <- "CS_2-0"
colnames(LALIGA_fixtures_clone)[17] <- "CS_0-2"
colnames(LALIGA_fixtures_clone)[19] <- "CS_2-1"
colnames(LALIGA_fixtures_clone)[20] <- "CS_1-2"

LALIGA_fixtures_clone$`CS_1-1` <- round(1/LALIGA_fixtures_clone$`CS_1-1`, digits = 3)
LALIGA_fixtures_clone$`CS_1-0` <- round(1/LALIGA_fixtures_clone$`CS_1-0`, digits = 3)
LALIGA_fixtures_clone$`CS_0-1` <- round(1/LALIGA_fixtures_clone$`CS_0-1`, digits = 3)
LALIGA_fixtures_clone$`CS_2-0` <- round(1/LALIGA_fixtures_clone$`CS_2-0`, digits = 3)
LALIGA_fixtures_clone$`CS_0-2` <- round(1/LALIGA_fixtures_clone$`CS_0-2`, digits = 3)
LALIGA_fixtures_clone$`CS_2-1` <- round(1/LALIGA_fixtures_clone$`CS_2-1`, digits = 3)
LALIGA_fixtures_clone$`CS_1-2` <- round(1/LALIGA_fixtures_clone$`CS_1-2`, digits = 3)

colnames(LALIGA_fixtures_clone)[1] <- "league"
colnames(LALIGA_fixtures_clone)[2] <- "Hometeam"
colnames(LALIGA_fixtures_clone)[3] <- "Awayteam"
colnames(LALIGA_fixtures_clone)[92] <- "predscore"
colnames(LALIGA_fixtures_clone)[64] <- "ov25"
colnames(LALIGA_fixtures_clone)[66] <- "ov25odds"
colnames(LALIGA_fixtures_clone)[65] <- "un25"
colnames(LALIGA_fixtures_clone)[67] <- "un25odds"
colnames(LALIGA_fixtures_clone)[68] <- "BTTSY"
colnames(LALIGA_fixtures_clone)[69] <- "BTTSN"
colnames(LALIGA_fixtures_clone)[70] <- "BTTSYodds"
colnames(LALIGA_fixtures_clone)[71] <- "BTTSNodds"

LALIGA_fixtures_clone <- LALIGA_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
LALIGA_fixtures_clone$matchid <- paste(LALIGA_fixtures_clone$Hometeam,LALIGA_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
LALIGA_fixtures_clone_final <- LALIGA_fixtures_clone[,-c(8,9,10,27)]
LALIGA_fixtures_clone_final[,'sep'] <- ''

laliga_dmprediction <-  laliga_picks[,c(4,5,6,7,8)]
laliga_dmprediction[,'sep2'] <- ''

laliga_avgyellow <- laliga_picks[,c(9,10)]
laliga_avgyellow[,'sep3'] <- ''

laliga_avgcorners <- laliga_picks[,c(11,12)]
laliga_avgcorners[,'sep4'] <- ''

laliga_goals <- LALIGA_fixtures[,c(10,11)]
laliga_goals$laliga_xGH <- round(laliga_goals$laliga_xGH, digits = 2)
laliga_goals$laliga_xGA <- round(laliga_goals$laliga_xGA, digits = 2)
laliga_goals$laliga_TxG <- laliga_goals$laliga_xGH + laliga_goals$laliga_xGA
laliga_goals[,'sep5'] <- ''

laliga_shots <- LALIGA_fixtures_sot[,c(10,11)]
laliga_shots$laliga_xHST <- round(laliga_shots$laliga_xHST, digits = 2)
laliga_shots$laliga_xAST <- round(laliga_shots$laliga_xAST, digits = 2)
laliga_shots$TxSOT <- laliga_shots$laliga_xHST + laliga_shots$laliga_xAST
laliga_shots[,'sep6'] <- ''

laliga_fouls <- LALIGA_fixtures_fo[,c(10,11)]
laliga_fouls$laliga_xHF <- round(laliga_fouls$laliga_xHF, digits = 2)
laliga_fouls$laliga_xAF <- round(laliga_fouls$laliga_xAF, digits = 2)
laliga_fouls$laliga_TxF <- laliga_fouls$laliga_xHF + laliga_fouls$laliga_xAF

laliga_ycpf <- laliga_picks[,c(15,16)]
laliga_fouls <- cbind(laliga_fouls,laliga_ycpf)
laliga_fouls$HYCPF <- as.numeric(laliga_fouls$HYCPF)
laliga_fouls$AYCPF <- as.numeric(laliga_fouls$AYCPF)
laliga_fouls$x_hyc <- (laliga_fouls$laliga_xHF) * (laliga_fouls$HYCPF)
laliga_fouls$x_ayc <- (laliga_fouls$laliga_xAF) * (laliga_fouls$AYCPF)
laliga_fouls$x_TYC <- round((laliga_fouls$x_hyc + laliga_fouls$x_ayc),digits = 2)
laliga_fouls[,'sep7'] <- ''

laliga_bookings <- LALIGA_fixtures_yc[,c(10,11)]
laliga_bookings$laliga_xHYC <- round(laliga_bookings$laliga_xHYC, digits = 2)
laliga_bookings$laliga_xAYC <- round(laliga_bookings$laliga_xAYC, digits = 2)
laliga_bookings$laliga_TYcards <- laliga_bookings$laliga_xHYC + laliga_bookings$laliga_xAYC
laliga_bookings[,'sep8'] <- ''

laliga_corners <- LALIGA_fixtures_co[,c(10,11)]
laliga_corners$laliga_xHCOC <- round(laliga_corners$laliga_xHCOC, digits = 2)
laliga_corners$laliga_xACOC <- round(laliga_corners$laliga_xACOC, digits = 2)
laliga_corners$laliga_TCOs <- laliga_corners$laliga_xHCOC + laliga_corners$laliga_xACOC
laliga_corners[,'sep9'] <- ''

laliga_shotsconversion <- laliga_picks[,c(13,14)]
laliga_shotsconversion <- cbind(laliga_shotsconversion,laliga_shots)
laliga_shotsconversion$HXSC <- as.numeric(laliga_shotsconversion$HXSC)
laliga_shotsconversion$AXSC <- as.numeric(laliga_shotsconversion$AXSC)
laliga_shotsconversion$laliga_hXgoals <- round((laliga_shotsconversion$HXSC * laliga_shotsconversion$laliga_xHST), digits = 2)
laliga_shotsconversion$laliga_aXgoals <- round((laliga_shotsconversion$AXSC * laliga_shotsconversion$laliga_xAST), digits = 2)
laliga_shotsconversion$Xgoals <- laliga_shotsconversion$laliga_hXgoals + laliga_shotsconversion$laliga_aXgoals
options(java.parameters = "-Xmx4g")
LALIGA_all <- cbind(LALIGA_fixtures_clone_final,laliga_dmprediction,laliga_avgyellow,laliga_avgcorners,laliga_goals,laliga_shots,laliga_fouls,laliga_bookings,laliga_corners,laliga_shotsconversion)
unlink('Divisions/LALIGA.xlsx')
write.xlsx(LALIGA_all,'Divisions/LALIGA.xlsx', sheetName = "LALIGA_all", append = TRUE)
write.xlsx(points_laliga,'Divisions/LALIGA.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(laliga_cornertotalsv2,'Divisions/LALIGA.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(laliga_goaltotalsv2,'Divisions/LALIGA.xlsx', sheetName = "Goaltotals", append = TRUE)
write.xlsx(laliga_yellowtotalsv2,'Divisions/LALIGA.xlsx', sheetName = "Yellowtotals", append = TRUE)


##write.csv(LALIGA_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
