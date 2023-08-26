#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
t1_currentround
#first_df <- E0_rounds[E0_rounds$e0_matchday > 32,]
#second_df <- SP1_rounds[SP1_rounds$sp1_matchday > 32,]
# # # third_df <- I1_rounds[I1_rounds$i1_matchday > 29,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
# #third_df <- third_df[,-37]
#SUPERLIG <- rbind(first_df,second_df)

SUPERLIG <- T1_rounds[T1_rounds$t1_matchday > 28,]
SUPERLIG <- na.omit(SUPERLIG)
#goaltotals v2
superlig_goaltotalsv2 <- tapply(SUPERLIG$TG, SUPERLIG[c("HomeTeam", "AwayTeam")],mean)
superlig_hgtotals <- rowSums(superlig_goaltotalsv2, na.rm = T)
superlig_agtotals <- colSums(superlig_goaltotalsv2, na.rm = T)
superlig_goaltotalsv2 <- cbind(superlig_goaltotalsv2,superlig_hgtotals,superlig_agtotals)
superlig_totalgoals <- superlig_hgtotals + superlig_agtotals
superlig_goaltotalsv2 <- cbind(superlig_goaltotalsv2,superlig_totalgoals)
superlig_teams <- sort(unique(SUPERLIG$HomeTeam))
superlig_home_games <- c()
superlig_away_games <-c()
for (i_superlig in 1:length(superlig_teams))
{

  superlig_home_games[i_superlig] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig],])
  superlig_away_games[i_superlig]  <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig],])

}
superlig_games_played <- superlig_home_games + superlig_away_games
superlig_goaltotalsv2 <- cbind(superlig_goaltotalsv2,superlig_games_played)
superlig_avg_totalgoals <- round((superlig_totalgoals/ superlig_games_played), digits = 4)
superlig_goaltotalsv2[is.na(superlig_goaltotalsv2)] <- ""
superlig_goaltotalsv2 <- cbind(superlig_goaltotalsv2,superlig_avg_totalgoals)

############################################################################################################
#Cornertotals v2
superlig_cornertotalsv2 <- tapply(SUPERLIG$TC, SUPERLIG[c("HomeTeam", "AwayTeam")],mean)
superlig_hcototals <- rowSums(superlig_cornertotalsv2, na.rm = T)
superlig_acototals <- colSums(superlig_cornertotalsv2, na.rm = T)
superlig_cornertotalsv2 <- cbind(superlig_cornertotalsv2,superlig_hcototals,superlig_acototals)
superlig_totalcorners <- superlig_hcototals + superlig_acototals
superlig_cornertotalsv2 <- cbind(superlig_cornertotalsv2,superlig_totalcorners)
superlig_cornertotalsv2 <- cbind(superlig_cornertotalsv2,superlig_games_played)
superlig_avg_totalcorners <- round((superlig_totalcorners/ superlig_games_played), digits = 4)
superlig_cornertotalsv2[is.na(superlig_cornertotalsv2)] <- ""
superlig_cornertotalsv2 <- cbind(superlig_cornertotalsv2,superlig_avg_totalcorners)
############################################################################################################
#GS matrix
superlig_goalscored_h <- tapply(SUPERLIG$FTHG, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_goalscored_a <- tapply(SUPERLIG$FTAG, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_goalscored_h[is.na(superlig_goalscored_h)] <- ""
superlig_goalscored_a[is.na(superlig_goalscored_a)] <- ""
for(superlig_rowhgs in 1:nrow(superlig_goalscored_h)) {
  for(superlig_colhgs in 1:ncol(superlig_goalscored_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowags in 1:nrow(superlig_goalscored_a)) {
      for(superlig_colags in 1:ncol(superlig_goalscored_a)) {
        ifelse(!superlig_goalscored_a[superlig_rowags,superlig_colags]=="",superlig_goalscored_h[superlig_rowags,superlig_colags] <- superlig_goalscored_a[superlig_rowags,superlig_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
superlig_goalconceded_h <- tapply(SUPERLIG$FTAG, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_goalconceded_a <- tapply(SUPERLIG$FTHG, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_goalconceded_h[is.na(superlig_goalconceded_h)] <- ""
superlig_goalconceded_a[is.na(superlig_goalconceded_a)] <- ""
for(superlig_rowhgc in 1:nrow(superlig_goalconceded_h)) {
  for(superlig_colhgc in 1:ncol(superlig_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowagc in 1:nrow(superlig_goalconceded_a)) {
      for(superlig_colagc in 1:ncol(superlig_goalconceded_a)) {
        ifelse(!superlig_goalconceded_a[superlig_rowagc,superlig_colagc]=="",superlig_goalconceded_h[superlig_rowagc,superlig_colagc] <- superlig_goalconceded_a[superlig_rowagc,superlig_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
superlig_totalcorners_h <- tapply(SUPERLIG$TC, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_totalcorners_a <- tapply(SUPERLIG$TC, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_totalcorners_h[is.na(superlig_totalcorners_h)] <- ""
superlig_totalcorners_a[is.na(superlig_totalcorners_a)] <- ""
#SUPERLIG
for(superlig_rowTC in 1:nrow(superlig_totalcorners_h)) {
  for(superlig_colTC in 1:ncol(superlig_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowTC in 1:nrow(superlig_totalcorners_a)) {
      for(superlig_colTC in 1:ncol(superlig_totalcorners_a)) {
        ifelse(!superlig_totalcorners_a[superlig_rowTC,superlig_colTC]=="",superlig_totalcorners_h[superlig_rowTC,superlig_colTC] <- superlig_totalcorners_a[superlig_rowTC,superlig_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
superlig_coawarded_h <- tapply(SUPERLIG$HCO, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_coawarded_a <- tapply(SUPERLIG$ACO, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_coawarded_h[is.na(superlig_coawarded_h)] <- ""
superlig_coawarded_a[is.na(superlig_coawarded_a)] <- ""
#SUPERLIG
for(superlig_rowhco in 1:nrow(superlig_coawarded_h)) {
  for(superlig_colhco in 1:ncol(superlig_coawarded_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowaco in 1:nrow(superlig_coawarded_a)) {
      for(superlig_colaco in 1:ncol(superlig_coawarded_a)) {
        ifelse(!superlig_coawarded_a[superlig_rowaco,superlig_colaco]=="",superlig_coawarded_h[superlig_rowaco,superlig_colaco] <- superlig_coawarded_a[superlig_rowaco,superlig_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
superlig_cornersconceded_h <- tapply(SUPERLIG$ACO, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_cornersconceded_a <- tapply(SUPERLIG$HCO, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_cornersconceded_h[is.na(superlig_cornersconceded_h)] <- ""
superlig_cornersconceded_a[is.na(superlig_cornersconceded_a)] <- ""
#SUPERLIG
for(superlig_rowhcc in 1:nrow(superlig_cornersconceded_h)) {
  for(superlig_colhcc in 1:ncol(superlig_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowacc in 1:nrow(superlig_cornersconceded_a)) {
      for(superlig_colacc in 1:ncol(superlig_cornersconceded_a)) {
        ifelse(!superlig_cornersconceded_a[superlig_rowacc,superlig_colacc]=="",superlig_cornersconceded_h[superlig_rowacc,superlig_colacc] <- superlig_cornersconceded_a[superlig_rowacc,superlig_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
superlig_coscform_h <- tapply(SUPERLIG$COSC, SUPERLIG[c("HomeTeam", "Date")],median)
superlig_coscform_a <- tapply(SUPERLIG$COSC, SUPERLIG[c("AwayTeam", "Date")],median)
superlig_coscform_h[is.na(superlig_coscform_h)] <- ""
superlig_coscform_a[is.na(superlig_coscform_a)] <- ""
#SUPERLIG
for(superlig_rowh_f_cosc in 1:nrow(superlig_coscform_h)) {
  for(superlig_colh_f_cosc in 1:ncol(superlig_coscform_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowa_f_cosc in 1:nrow(superlig_coscform_a)) {
      for(superlig_cola_f_cosc in 1:ncol(superlig_coscform_a)) {
        ifelse(!superlig_coscform_a[superlig_rowa_f_cosc,superlig_cola_f_cosc]=="",superlig_coscform_h[superlig_rowa_f_cosc,superlig_cola_f_cosc] <- superlig_coscform_a[superlig_rowa_f_cosc,superlig_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
superlig_winmargin_h <- tapply(SUPERLIG$FTHG - SUPERLIG$FTAG, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_winmargin_a <- tapply(SUPERLIG$FTAG - SUPERLIG$FTHG, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_winmargin_h[is.na(superlig_winmargin_h)] <- ""
superlig_winmargin_a[is.na(superlig_winmargin_a)] <- ""
#SUPERLIG
for(superlig_rowhwm in 1:nrow(superlig_winmargin_h)) {
  for(superlig_colhwm in 1:ncol(superlig_winmargin_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowawm in 1:nrow(superlig_winmargin_a)) {
      for(superlig_colawm in 1:ncol(superlig_winmargin_a)) {
        ifelse(!superlig_winmargin_a[superlig_rowawm,superlig_colawm]=="",superlig_winmargin_h[superlig_rowawm,superlig_colawm] <- superlig_winmargin_a[superlig_rowawm,superlig_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
superlig_yellowscored_h <- tapply(SUPERLIG$HY, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_yellowscored_a <- tapply(SUPERLIG$AY, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_yellowscored_h[is.na(superlig_yellowscored_h)] <- ""
superlig_yellowscored_a[is.na(superlig_yellowscored_a)] <- ""
#SUPERLIG
for(superlig_rowhys in 1:nrow(superlig_yellowscored_h)) {
  for(superlig_colhys in 1:ncol(superlig_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(superlig_roways in 1:nrow(superlig_yellowscored_a)) {
      for(superlig_colays in 1:ncol(superlig_yellowscored_a)) {
        ifelse(!superlig_yellowscored_a[superlig_roways,superlig_colays]=="",superlig_yellowscored_h[superlig_roways,superlig_colays] <- superlig_yellowscored_a[superlig_roways,superlig_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
superlig_redscored_h <- tapply(SUPERLIG$HR, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_redscored_a <- tapply(SUPERLIG$AR, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_redscored_h[is.na(superlig_redscored_h)] <- ""
superlig_redscored_a[is.na(superlig_redscored_a)] <- ""
for(superlig_rowhrs in 1:nrow(superlig_redscored_h)) {
  for(superlig_colhrs in 1:ncol(superlig_redscored_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowars in 1:nrow(superlig_redscored_a)) {
      for(superlig_colars in 1:ncol(superlig_redscored_a)) {
        ifelse(!superlig_redscored_a[superlig_rowars,superlig_colars]=="",superlig_redscored_h[superlig_rowars,superlig_colars] <- superlig_redscored_a[superlig_rowars,superlig_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
superlig_redtotalsv2 <- tapply(SUPERLIG$TR, SUPERLIG[c("HomeTeam", "AwayTeam")],mean)
superlig_hrtotals <- rowSums(superlig_redtotalsv2, na.rm = T)
superlig_artotals <- colSums(superlig_redtotalsv2, na.rm = T)
superlig_redtotalsv2 <- cbind(superlig_redtotalsv2,superlig_hrtotals,superlig_artotals)
superlig_totalreds <- superlig_hrtotals + superlig_artotals
superlig_redtotalsv2 <- cbind(superlig_redtotalsv2,superlig_totalreds)
superlig_redtotalsv2 <- cbind(superlig_redtotalsv2,superlig_games_played)
superlig_avg_totalreds <- round((superlig_totalreds/ superlig_games_played), digits = 4)
superlig_redtotalsv2[is.na(superlig_redtotalsv2)] <- ""
superlig_redtotalsv2 <- cbind(superlig_redtotalsv2,superlig_avg_totalreds)
############################################################################################################################################################
#yellowtotals
superlig_yellowtotalsv2 <- tapply(SUPERLIG$TY, SUPERLIG[c("HomeTeam", "AwayTeam")],mean)
superlig_hytotals <- rowSums(superlig_yellowtotalsv2, na.rm = T)
superlig_aytotals <- colSums(superlig_yellowtotalsv2, na.rm = T)
superlig_yellowtotalsv2 <- cbind(superlig_yellowtotalsv2,superlig_hytotals,superlig_aytotals)
superlig_totalyellows <- superlig_hytotals + superlig_aytotals
superlig_yellowtotalsv2 <- cbind(superlig_yellowtotalsv2,superlig_totalyellows)
superlig_yellowtotalsv2 <- cbind(superlig_yellowtotalsv2,superlig_games_played)
superlig_avg_totalyellows <- round((superlig_totalyellows/ superlig_games_played), digits = 4)
superlig_yellowtotalsv2[is.na(superlig_yellowtotalsv2)] <- ""
superlig_yellowtotalsv2 <- cbind(superlig_yellowtotalsv2,superlig_avg_totalyellows)
##################################################################################################################################################
#team form
superlig_form_h <- tapply(SUPERLIG$FTR, SUPERLIG[c("HomeTeam", "Date")],median)
superlig_form_a <- tapply(SUPERLIG$FTR, SUPERLIG[c("AwayTeam", "Date")],median)
superlig_form_h[is.na(superlig_form_h)] <- ""
superlig_form_a[is.na(superlig_form_a)] <- ""
superlig_form_h <- sub("A","L",superlig_form_h)
superlig_form_h <- sub("H","W",superlig_form_h)
superlig_form_a <- sub("A","W",superlig_form_a)
superlig_form_a <- sub("H","L",superlig_form_a)
for(superlig_rowh_f in 1:nrow(superlig_form_h)) {
  for(superlig_colh_f in 1:ncol(superlig_form_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowa_f in 1:nrow(superlig_form_a)) {
      for(superlig_cola_f in 1:ncol(superlig_form_a)) {
        ifelse(!superlig_form_a[superlig_rowa_f,superlig_cola_f]=="",superlig_form_h[superlig_rowa_f,superlig_cola_f] <- superlig_form_a[superlig_rowa_f,superlig_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
superlig_csform_h <- tapply(SUPERLIG$CS, SUPERLIG[c("HomeTeam", "Date")],median)
superlig_csform_a <- tapply(SUPERLIG$CS, SUPERLIG[c("AwayTeam", "Date")],median)
superlig_csform_h[is.na(superlig_csform_h)] <- ""
superlig_csform_a[is.na(superlig_csform_a)] <- ""
#SUPERLIG
for(superlig_rowh_f_cs in 1:nrow(superlig_csform_h)) {
  for(superlig_colh_f_cs in 1:ncol(superlig_csform_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowa_f_cs in 1:nrow(superlig_csform_a)) {
      for(superlig_cola_f_cs in 1:ncol(superlig_csform_a)) {
        ifelse(!superlig_csform_a[superlig_rowa_f_cs,superlig_cola_f_cs]=="",superlig_csform_h[superlig_rowa_f_cs,superlig_cola_f_cs] <- superlig_csform_a[superlig_rowa_f_cs,superlig_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
superlig_totalgoals_h <- tapply(SUPERLIG$TG, SUPERLIG[c("HomeTeam", "Date")],mean)
superlig_totalgoals_a <- tapply(SUPERLIG$TG, SUPERLIG[c("AwayTeam", "Date")],mean)
superlig_totalgoals_h[is.na(superlig_totalgoals_h)] <- ""
superlig_totalgoals_a[is.na(superlig_totalgoals_a)] <- ""
for(superlig_rowh in 1:nrow(superlig_totalgoals_h)) {
  for(superlig_colh in 1:ncol(superlig_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowa in 1:nrow(superlig_totalgoals_a)) {
      for(superlig_cola in 1:ncol(superlig_totalgoals_a)) {
        ifelse(!superlig_totalgoals_a[superlig_rowa,superlig_cola]=="",superlig_totalgoals_h[superlig_rowa,superlig_cola] <- superlig_totalgoals_a[superlig_rowa,superlig_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#SUPERLIG
superlig_un05_home <- c()
superlig_un05_away <- c()
superlig_ov05_home <- c()
superlig_ov05_away <- c()

superlig_un15_home <- c()
superlig_un15_away <- c()
superlig_ov15_home <- c()
superlig_ov15_away <- c()

superlig_un25_home <- c()
superlig_un25_away <- c()
superlig_ov25_home <- c()
superlig_ov25_away <- c()

superlig_un35_home <- c()
superlig_un35_away <- c()
superlig_ov35_home <- c()
superlig_ov35_away <- c()

superlig_un45_home <- c()
superlig_un45_away <- c()
superlig_ov45_home <- c()
superlig_ov45_away <- c()

superlig_un55_home <- c()
superlig_un55_away <- c()
superlig_ov55_home <- c()
superlig_ov55_away <- c()

for (i_superlig_tg in 1:length(superlig_teams))
{

  superlig_un05_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG == 0,])
  superlig_un05_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG == 0,])

  superlig_ov05_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG > 0,])
  superlig_ov05_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG > 0,])

  superlig_un15_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 1,])
  superlig_un15_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 1,])

  superlig_ov15_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 2,])
  superlig_ov15_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 2,])

  superlig_un25_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 2,])
  superlig_un25_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 2,])

  superlig_ov25_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >=3,])
  superlig_ov25_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >=3,])

  superlig_un35_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 3,])
  superlig_un35_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 3,])

  superlig_ov35_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 4,])
  superlig_ov35_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 4,])

  superlig_un45_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 4,])
  superlig_un45_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 4,])

  superlig_ov45_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 5,])
  superlig_ov45_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 5,])

  superlig_un55_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 5,])
  superlig_un55_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG <= 5,])

  superlig_ov55_home[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 6,])
  superlig_ov55_away[i_superlig_tg] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_tg] & SUPERLIG$TG >= 6,])


}

superlig_un05 <- superlig_un05_home + superlig_un05_away
superlig_ov05 <- superlig_ov05_home + superlig_ov05_away

superlig_un15 <- superlig_un15_home + superlig_un15_away
superlig_ov15 <- superlig_ov15_home + superlig_ov15_away

superlig_un25 <- superlig_un25_home + superlig_un25_away
superlig_ov25 <- superlig_ov25_home + superlig_ov25_away

superlig_un35 <- superlig_un35_home + superlig_un35_away
superlig_ov35 <- superlig_ov35_home + superlig_ov35_away

superlig_un45 <- superlig_un45_home + superlig_un45_away
superlig_ov45 <- superlig_ov45_home + superlig_ov45_away

superlig_un55 <- superlig_un55_home + superlig_un55_away
superlig_ov55 <- superlig_ov55_home + superlig_ov55_away

superlig_ovundata <- cbind(superlig_teams,superlig_un05,superlig_ov05,superlig_un15,superlig_ov15,superlig_un25,superlig_ov25,superlig_un35,superlig_ov35,superlig_un45,superlig_ov45,superlig_un55,superlig_ov55)
#################################################################################################################################################################
#team against
superlig_form_team_against_h <- tapply(SUPERLIG$AwayTeam, SUPERLIG[c("HomeTeam", "Date")],median)
superlig_form_team_against_a <- tapply(SUPERLIG$HomeTeam, SUPERLIG[c("AwayTeam", "Date")],median)
superlig_form_team_against_h[is.na(superlig_form_team_against_h)] <- ""
superlig_form_team_against_a[is.na(superlig_form_team_against_a)] <- ""
#SUPERLIG
for(superlig_rowh_f_against in 1:nrow(superlig_form_team_against_h)) {
  for(superlig_colh_f_against in 1:ncol(superlig_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(superlig_rowa_f_against in 1:nrow(superlig_form_team_against_a)) {
      for(superlig_cola_f_against in 1:ncol(superlig_form_team_against_a)) {
        ifelse(!superlig_form_team_against_a[superlig_rowa_f_against,superlig_cola_f_against]=="",superlig_form_team_against_h[superlig_rowa_f_against,superlig_cola_f_against] <- superlig_form_team_against_a[superlig_rowa_f_against,superlig_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#SUPERLIG
#home goals scored
superlig_home_gs <- aggregate(SUPERLIG$FTHG, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_home_gs_avg <- aggregate(SUPERLIG$FTHG, by = list(SUPERLIG$HomeTeam),mean)
superlig_home_scoring <- merge(superlig_home_gs,superlig_home_gs_avg, by='Group.1',all = T)
names(superlig_home_scoring)[names(superlig_home_scoring) == "x.x"] <- "TFthg"
names(superlig_home_scoring)[names(superlig_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
superlig_away_gs <- aggregate(SUPERLIG$FTAG, by = list(SUPERLIG$AwayTeam), FUN = sum)
superlig_away_gs_avg <- aggregate(SUPERLIG$FTAG, by = list(SUPERLIG$AwayTeam),mean)
superlig_away_scoring <- merge(superlig_away_gs,superlig_away_gs_avg, by='Group.1',all = T)
names(superlig_away_scoring)[names(superlig_away_scoring) == "x.x"] <- "TFtag"
names(superlig_away_scoring)[names(superlig_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
superlig_scoring <- merge(superlig_home_scoring,superlig_away_scoring,by='Group.1',all = T)
superlig_scoring$TGS <- superlig_scoring$TFthg + superlig_scoring$TFtag

#Home shots on target
superlig_home_hst <- aggregate(SUPERLIG$HST, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_ast <- aggregate(SUPERLIG$AST, by = list(SUPERLIG$AwayTeam), FUN = sum)
superlig_tst <- merge(superlig_home_hst,superlig_away_ast, by='Group.1',all = T)
names(superlig_tst)[names(superlig_tst) == "x.x"] <- "hst"
names(superlig_tst)[names(superlig_tst) == "x.y"] <- "ast"
superlig_tst$TST <- superlig_tst$hst + superlig_tst$ast
#merge goals scored and shots on target
superlig_scoring_conversion <- merge(superlig_tst,superlig_scoring,by='Group.1',all = T)
#add HSC ASC TSC
superlig_scoring_conversion$HSTC <- percent(superlig_scoring_conversion$TFthg/superlig_scoring_conversion$hst, accuracy = 0.01)
superlig_scoring_conversion$ASTC <- percent(superlig_scoring_conversion$TFtag/superlig_scoring_conversion$ast, accuracy = 0.01)
superlig_scoring_conversion$TSTC <- percent(superlig_scoring_conversion$TGS/superlig_scoring_conversion$TST, accuracy = 0.01)
#merge games played
superlig_scoring_conversion <- cbind(superlig_scoring_conversion,superlig_games_played)
#create the second part
#home goals conceded
superlig_home_gc <- aggregate(SUPERLIG$FTAG, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_home_gc_avg <- aggregate(SUPERLIG$FTAG, by = list(SUPERLIG$HomeTeam),mean)
superlig_home_conceding <- merge(superlig_home_gc,superlig_home_gc_avg, by='Group.1',all = T)
names(superlig_home_conceding)[names(superlig_home_conceding) == "x.x"] <- "TFthc"
names(superlig_home_conceding)[names(superlig_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
superlig_away_gc <- aggregate(SUPERLIG$FTHG, by = list(SUPERLIG$AwayTeam), FUN = sum)
superlig_away_gc_avg <- aggregate(SUPERLIG$FTHG, by = list(SUPERLIG$AwayTeam),mean)
superlig_away_conceding <- merge(superlig_away_gc,superlig_away_gc_avg, by='Group.1',all = T)
names(superlig_away_conceding)[names(superlig_away_conceding) == "x.x"] <- "TFtac"
names(superlig_away_conceding)[names(superlig_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
superlig_conceding <- merge(superlig_home_conceding,superlig_away_conceding,by='Group.1',all = T)
superlig_conceding$TGC <- superlig_conceding$TFthc + superlig_conceding$TFtac
superlig_home_hst
#Home shots conceded
superlig_home_hsc <- aggregate(SUPERLIG$AST, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_asc <- aggregate(SUPERLIG$HST, by = list(SUPERLIG$AwayTeam), FUN = sum)
superlig_tsc <- merge(superlig_home_hsc,superlig_away_asc, by='Group.1',all = T)
names(superlig_tsc)[names(superlig_tsc) == "x.x"] <- "hsc"
names(superlig_tsc)[names(superlig_tsc) == "x.y"] <- "asc"
superlig_tsc$TSC <- superlig_tsc$hsc + superlig_tsc$asc
#merge goals conceded and shots conceded
superlig_conceding_conversion <- merge(superlig_tsc,superlig_conceding,by='Group.1',all = T)

#add HSC ASC TSC
superlig_conceding_conversion$HSCC <- percent(superlig_conceding_conversion$TFthc/superlig_conceding_conversion$hsc, accuracy = 0.01)
superlig_conceding_conversion$ASCC <- percent(superlig_conceding_conversion$TFtac/superlig_conceding_conversion$asc, accuracy = 0.01)
superlig_conceding_conversion$TSCC <- percent(superlig_conceding_conversion$TGC/superlig_conceding_conversion$TSC, accuracy = 0.01)
superlig_conceding_conversion$XSTC <- round(superlig_scoring$TGS/(superlig_tst$TST - superlig_scoring$TGS), digits = 2)

#merge the two parts
superlig_shots_analysis <- merge(superlig_scoring_conversion,superlig_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#SUPERLIG
#home fouls for
superlig_home_fouls <- aggregate(SUPERLIG$HF, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_home_fouls_avg <- aggregate(SUPERLIG$HF, by = list(SUPERLIG$HomeTeam),mean)
superlig_home_foulsdata <- merge(superlig_home_fouls,superlig_home_fouls_avg, by='Group.1',all = T)
names(superlig_home_foulsdata)[names(superlig_home_foulsdata) == "x.x"] <- "THfouls"
names(superlig_home_foulsdata)[names(superlig_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
superlig_away_fouls <- aggregate(SUPERLIG$HF, by = list(SUPERLIG$AwayTeam), FUN = sum)
superlig_away_fouls_avg <- aggregate(SUPERLIG$HF, by = list(SUPERLIG$AwayTeam),mean)
superlig_away_foulsdata <- merge(superlig_away_fouls,superlig_away_fouls_avg, by='Group.1',all = T)
names(superlig_away_foulsdata)[names(superlig_away_foulsdata) == "x.x"] <- "TAfouls"
names(superlig_away_foulsdata)[names(superlig_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
superlig_fouls <- merge(superlig_home_foulsdata,superlig_away_foulsdata,by='Group.1',all = T)
superlig_fouls$TotalFouls <- superlig_fouls$THfouls + superlig_fouls$TAfouls

#yellow cards
superlig_home_hyc <- aggregate(SUPERLIG$HY, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_ayc <- aggregate(SUPERLIG$AY, by = list(SUPERLIG$AwayTeam), FUN = sum)
superlig_tyc <- merge(superlig_home_hyc,superlig_away_ayc, by='Group.1',all = T)
names(superlig_tyc)[names(superlig_tyc) == "x.x"] <- "hyc"
names(superlig_tyc)[names(superlig_tyc) == "x.y"] <- "ayc"
superlig_tyc$TotalYellows <- superlig_tyc$hyc + superlig_tyc$ayc

#merge fouls for and yellow cards
superlig_fouls_conversion <- merge(superlig_tyc,superlig_fouls,by='Group.1',all = T)
superlig_fouls_conversion$YcPerfoul <- round((superlig_fouls_conversion$TotalYellows/superlig_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
SUPERLIG$Div <- "SUPERLIG"
##
###################################################################################################################################################
#poisson cards
superlig_GP <- nrow(SUPERLIG)
#Calculate total home goals for each division
superlig_T_HY <- sum(superlig_home_hyc$x)
#calculate average home goal
superlig_avg_HY <- round(superlig_T_HY /superlig_GP, digits = 4)
############################################################
#Calculate total away goals for each division
superlig_T_AY <- sum(superlig_away_ayc$x)
#calculate average away goal
superlig_avg_AY <- round(superlig_T_AY /superlig_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
superlig_home_yas <- round(((superlig_home_hyc$x/superlig_home_games))/superlig_avg_HY, digits = 4)
#calculate away attack strength
superlig_away_yas <- round(((superlig_away_ayc$x/superlig_away_games))/superlig_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
superlig_avg_HYC <- round(superlig_T_AY /superlig_GP, digits = 4)
#avg away concede
superlig_avg_AYC <- round(superlig_T_HY /superlig_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
superlig_home_ycc <- aggregate(SUPERLIG$AY, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_ycc <- aggregate(SUPERLIG$HY, by = list(SUPERLIG$AwayTeam), FUN = sum)
#home defense strength
superlig_home_yds <- round(((superlig_home_ycc$x/superlig_home_games))/superlig_avg_HYC, digits = 4)
#away defense strength
superlig_away_yds <- round(((superlig_away_ycc$x/superlig_away_games))/superlig_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_home_poisson_yc <- cbind(superlig_division,superlig_teams,superlig_avg_HY,superlig_home_yas,superlig_home_yds)
#away poisson data
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_away_poisson_yc <- cbind(superlig_division,superlig_teams,superlig_avg_AY,superlig_away_yas,superlig_away_yds)
###
HomeTeam_superlig_yc <- rep(superlig_teams, each = length(superlig_teams))
AwayTeam_superlig_yc <- rep(superlig_teams, length(superlig_teams))
SUPERLIG_fixtures_yc <- cbind(HomeTeam_superlig_yc,AwayTeam_superlig_yc)
SUPERLIG_fixtures_yc <- as.data.frame(SUPERLIG_fixtures_yc)
SUPERLIG_fixtures_yc <- SUPERLIG_fixtures_yc[!SUPERLIG_fixtures_yc$HomeTeam_superlig_yc == SUPERLIG_fixtures_yc$AwayTeam_superlig_yc,]
rownames(SUPERLIG_fixtures_yc) <- NULL
SUPERLIG_fixtures_yc$Div <- "SUPERLIG"
SUPERLIG_fixtures_yc <- SUPERLIG_fixtures_yc[,c(3,1,2)]

SUPERLIG_fixtures_yc$avg_HY_superlig <- superlig_avg_HY

SUPERLIG_fixtures_yc$superlig_homeyas <- rep(superlig_home_yas,each = length(superlig_teams)-1)

superlig_awayyds_lookup <- cbind(superlig_teams,superlig_away_yds)

superlig_awayyds_lookup <- as.data.frame(superlig_awayyds_lookup)

colnames(superlig_awayyds_lookup) <- c("AwayTeam_superlig_yc","superlig_awayyds")


require('RH2')
SUPERLIG_fixtures_yc$superlig_awayyds <- sqldf("SELECT superlig_awayyds_lookup.superlig_awayyds FROM superlig_awayyds_lookup INNER JOIN SUPERLIG_fixtures_yc ON superlig_awayyds_lookup.AwayTeam_superlig_yc = SUPERLIG_fixtures_yc.AwayTeam_superlig_yc")

SUPERLIG_fixtures_yc$avg_AY_superlig <- superlig_avg_AY

superlig_awayyas_lookup <- cbind(superlig_teams,superlig_away_yas)

superlig_awayyas_lookup <- as.data.frame(superlig_awayyas_lookup)

colnames(superlig_awayyas_lookup) <- c("AwayTeam_superlig_yc","superlig_awayyas")

SUPERLIG_fixtures_yc$superlig_awayyas <- sqldf("SELECT superlig_awayyas_lookup.superlig_awayyas FROM superlig_awayyas_lookup INNER JOIN SUPERLIG_fixtures_yc ON superlig_awayyas_lookup.AwayTeam_superlig_yc = SUPERLIG_fixtures_yc.AwayTeam_superlig_yc")

SUPERLIG_fixtures_yc$superlig_homeyds <- rep(superlig_home_yds,each = length(superlig_teams)-1)

SUPERLIG_fixtures_yc$superlig_awayyds <- as.numeric(unlist(SUPERLIG_fixtures_yc$superlig_awayyds))
#xGH
SUPERLIG_fixtures_yc$superlig_xHYC <- SUPERLIG_fixtures_yc$avg_HY_superlig * SUPERLIG_fixtures_yc$superlig_homeyas * SUPERLIG_fixtures_yc$superlig_awayyds
#xGA

SUPERLIG_fixtures_yc$superlig_awayyas <- as.numeric(unlist(SUPERLIG_fixtures_yc$superlig_awayyas))

SUPERLIG_fixtures_yc$superlig_xAYC <- SUPERLIG_fixtures_yc$avg_AY_superlig * SUPERLIG_fixtures_yc$superlig_awayyas * SUPERLIG_fixtures_yc$superlig_homeyds

SUPERLIG_fixtures_yc$superlig_0_0 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_0 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_0_1 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_1 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_0 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_0_2 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_2 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_1 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_2 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_3 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_0 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_1 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_2 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_0_3 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_3 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_3 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_4 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_0 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_1 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_2 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_3 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_0_4 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_4 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_4 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_4 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_5 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_0 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_1 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_2 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_3 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_4 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_0_5 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_5 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_5 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_5 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_5 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_6 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_0 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_1 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_2 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_3 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_4 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_6_5 <- round(stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_0_6 <- round(stats::dpois(0,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_1_6 <- round(stats::dpois(1,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_2_6 <- round(stats::dpois(2,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_3_6 <- round(stats::dpois(3,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_4_6 <- round(stats::dpois(4,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
SUPERLIG_fixtures_yc$superlig_5_6 <- round(stats::dpois(5,SUPERLIG_fixtures_yc$superlig_xHYC) * stats::dpois(6,SUPERLIG_fixtures_yc$superlig_xAYC), digits = 4)
#Home win
SUPERLIG_fixtures_yc$superlig_H <- (
  SUPERLIG_fixtures_yc$superlig_1_0 + SUPERLIG_fixtures_yc$superlig_2_0 + SUPERLIG_fixtures_yc$superlig_2_1 + SUPERLIG_fixtures_yc$superlig_3_0 + SUPERLIG_fixtures_yc$superlig_3_1 +
    SUPERLIG_fixtures_yc$superlig_3_2 + SUPERLIG_fixtures_yc$superlig_4_0 + SUPERLIG_fixtures_yc$superlig_4_1 + SUPERLIG_fixtures_yc$superlig_4_2 + SUPERLIG_fixtures_yc$superlig_4_3 +
    SUPERLIG_fixtures_yc$superlig_5_0 + SUPERLIG_fixtures_yc$superlig_5_1 + SUPERLIG_fixtures_yc$superlig_5_2 + SUPERLIG_fixtures_yc$superlig_5_3 + SUPERLIG_fixtures_yc$superlig_5_4 +
    SUPERLIG_fixtures_yc$superlig_6_0 + SUPERLIG_fixtures_yc$superlig_6_1 + SUPERLIG_fixtures_yc$superlig_6_2 + SUPERLIG_fixtures_yc$superlig_6_3 + SUPERLIG_fixtures_yc$superlig_6_4 +
    SUPERLIG_fixtures_yc$superlig_6_5
)

SUPERLIG_fixtures_yc$superlig_H <- percent(SUPERLIG_fixtures_yc$superlig_H, accuracy = 0.1)

#Draw
SUPERLIG_fixtures_yc$superlig_D <- (

  SUPERLIG_fixtures_yc$superlig_0_0 + SUPERLIG_fixtures_yc$superlig_1_1 + SUPERLIG_fixtures_yc$superlig_2_2 + SUPERLIG_fixtures_yc$superlig_3_3 + SUPERLIG_fixtures_yc$superlig_4_4 +
    SUPERLIG_fixtures_yc$superlig_5_5 + SUPERLIG_fixtures_yc$superlig_6_6
)

SUPERLIG_fixtures_yc$superlig_D <- percent(SUPERLIG_fixtures_yc$superlig_D, accuracy = 0.1)

#Away

SUPERLIG_fixtures_yc$superlig_A <- (
  SUPERLIG_fixtures_yc$superlig_0_1 + SUPERLIG_fixtures_yc$superlig_0_2 + SUPERLIG_fixtures_yc$superlig_1_2 + SUPERLIG_fixtures_yc$superlig_0_3 + SUPERLIG_fixtures_yc$superlig_1_3 +
    SUPERLIG_fixtures_yc$superlig_2_3 + SUPERLIG_fixtures_yc$superlig_0_4 + SUPERLIG_fixtures_yc$superlig_1_4 + SUPERLIG_fixtures_yc$superlig_2_4 + SUPERLIG_fixtures_yc$superlig_3_4 +
    SUPERLIG_fixtures_yc$superlig_0_5 + SUPERLIG_fixtures_yc$superlig_1_5 + SUPERLIG_fixtures_yc$superlig_2_5 + SUPERLIG_fixtures_yc$superlig_3_5 + SUPERLIG_fixtures_yc$superlig_4_5 +
    SUPERLIG_fixtures_yc$superlig_0_6 + SUPERLIG_fixtures_yc$superlig_1_6 + SUPERLIG_fixtures_yc$superlig_2_6 + SUPERLIG_fixtures_yc$superlig_3_6 + SUPERLIG_fixtures_yc$superlig_4_6 +
    SUPERLIG_fixtures_yc$superlig_5_6
)

SUPERLIG_fixtures_yc$superlig_A <- percent(SUPERLIG_fixtures_yc$superlig_A, accuracy = 0.1)

#ov25
SUPERLIG_fixtures_yc$superlig_ov25 <- (
  SUPERLIG_fixtures_yc$superlig_2_1 + SUPERLIG_fixtures_yc$superlig_1_2 + SUPERLIG_fixtures_yc$superlig_2_2 + SUPERLIG_fixtures_yc$superlig_3_0 + SUPERLIG_fixtures_yc$superlig_3_1 +
    SUPERLIG_fixtures_yc$superlig_3_2 + SUPERLIG_fixtures_yc$superlig_0_3 + SUPERLIG_fixtures_yc$superlig_1_3 + SUPERLIG_fixtures_yc$superlig_2_3 + SUPERLIG_fixtures_yc$superlig_3_3 +
    SUPERLIG_fixtures_yc$superlig_4_0 + SUPERLIG_fixtures_yc$superlig_4_1 + SUPERLIG_fixtures_yc$superlig_4_2 + SUPERLIG_fixtures_yc$superlig_4_3 + SUPERLIG_fixtures_yc$superlig_0_4 +
    SUPERLIG_fixtures_yc$superlig_1_4 + SUPERLIG_fixtures_yc$superlig_2_4 + SUPERLIG_fixtures_yc$superlig_3_4 + SUPERLIG_fixtures_yc$superlig_4_4 + SUPERLIG_fixtures_yc$superlig_5_0 +
    SUPERLIG_fixtures_yc$superlig_5_1 + SUPERLIG_fixtures_yc$superlig_5_2 + SUPERLIG_fixtures_yc$superlig_5_3 + SUPERLIG_fixtures_yc$superlig_5_4 + SUPERLIG_fixtures_yc$superlig_0_5 +
    SUPERLIG_fixtures_yc$superlig_1_5 + SUPERLIG_fixtures_yc$superlig_2_5 + SUPERLIG_fixtures_yc$superlig_3_5 + SUPERLIG_fixtures_yc$superlig_4_5 + SUPERLIG_fixtures_yc$superlig_5_5 +
    SUPERLIG_fixtures_yc$superlig_6_0 + SUPERLIG_fixtures_yc$superlig_6_1 + SUPERLIG_fixtures_yc$superlig_6_2 + SUPERLIG_fixtures_yc$superlig_6_3 + SUPERLIG_fixtures_yc$superlig_6_4 +
    SUPERLIG_fixtures_yc$superlig_6_5 + SUPERLIG_fixtures_yc$superlig_0_6 + SUPERLIG_fixtures_yc$superlig_1_6 + SUPERLIG_fixtures_yc$superlig_2_6 + SUPERLIG_fixtures_yc$superlig_3_6 +
    SUPERLIG_fixtures_yc$superlig_4_6 + SUPERLIG_fixtures_yc$superlig_5_6 + SUPERLIG_fixtures_yc$superlig_6_6
)
#un25
SUPERLIG_fixtures_yc$superlig_un25 <- (
  SUPERLIG_fixtures_yc$superlig_0_0 + SUPERLIG_fixtures_yc$superlig_1_0 + SUPERLIG_fixtures_yc$superlig_0_1 + SUPERLIG_fixtures_yc$superlig_1_1 + SUPERLIG_fixtures_yc$superlig_2_0 + SUPERLIG_fixtures_yc$superlig_0_2
)
#odds
SUPERLIG_fixtures_yc$superlig_ov25_odds <- round((1/SUPERLIG_fixtures_yc$superlig_ov25),digits = 2)
SUPERLIG_fixtures_yc$superlig_un25_odds <- round((1/SUPERLIG_fixtures_yc$superlig_un25),digits = 2)

SUPERLIG_fixtures_yc$superlig_ov25_odds
SUPERLIG_fixtures_yc$superlig_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
SUPERLIG_fixtures_yc$superlig_ov25 <- percent(SUPERLIG_fixtures_yc$superlig_ov25, accuracy = 0.1)

SUPERLIG_fixtures_yc$superlig_un25 <- percent(SUPERLIG_fixtures_yc$superlig_un25, accuracy = 0.1)
SUPERLIG_fixtures_yc$superlig_pscore <- paste(round(SUPERLIG_fixtures_yc$superlig_xHYC,digits = 0),round(SUPERLIG_fixtures_yc$superlig_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
superlig_GP <- nrow(SUPERLIG)
#Calculate total home corners for each division
superlig_home_corners <- aggregate(SUPERLIG$HCO, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_corners <- aggregate(SUPERLIG$ACO, by = list(SUPERLIG$AwayTeam), FUN = sum)
###############################################################################
superlig_T_HCO <- sum(superlig_home_corners$x)
#calculate average home corners
superlig_avg_HCO <- round(superlig_T_HCO /superlig_GP, digits = 4)
############################################################
#Calculate total away goals for each division
superlig_T_ACO <- sum(superlig_away_corners$x)
#calculate average away goal
superlig_avg_ACO <- round(superlig_T_ACO /superlig_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
superlig_home_coas <- round(((superlig_home_corners$x/superlig_home_games))/superlig_avg_HCO, digits = 4)
#calculate away attack strength
superlig_away_coas <- round(((superlig_away_corners$x/superlig_away_games))/superlig_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
superlig_avg_HCOC <- round(superlig_T_ACO /superlig_GP, digits = 4)
#avg away concede
superlig_avg_ACOC <- round(superlig_T_HCO /superlig_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
superlig_home_coc <- aggregate(SUPERLIG$ACO, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_coc <- aggregate(SUPERLIG$HCO, by = list(SUPERLIG$AwayTeam), FUN = sum)
#home defense strength
superlig_home_cods <- round(((superlig_home_coc$x/superlig_home_games))/superlig_avg_HCOC, digits = 4)
#away defense strength
superlig_away_cods <- round(((superlig_away_coc$x/superlig_away_games))/superlig_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_home_poisson_corners <- cbind(superlig_division,superlig_teams,superlig_avg_HCO,superlig_home_coas,superlig_home_cods)
#################################################################################
#away poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_away_poisson_corners <- cbind(superlig_division,superlig_teams,superlig_avg_ACO,superlig_away_coas,superlig_away_cods)

#SUPERLIG
HomeTeam_superlig_co <- rep(superlig_teams, each = length(superlig_teams))
AwayTeam_superlig_co <- rep(superlig_teams, length(superlig_teams))
SUPERLIG_fixtures_co <- cbind(HomeTeam_superlig_co,AwayTeam_superlig_co)
SUPERLIG_fixtures_co <- as.data.frame(SUPERLIG_fixtures_co)
SUPERLIG_fixtures_co <- SUPERLIG_fixtures_co[!SUPERLIG_fixtures_co$HomeTeam_superlig_co == SUPERLIG_fixtures_co$AwayTeam_superlig_co,]
rownames(SUPERLIG_fixtures_co) <- NULL
SUPERLIG_fixtures_co$Div <- "SUPERLIG"
SUPERLIG_fixtures_co <- SUPERLIG_fixtures_co[,c(3,1,2)]

SUPERLIG_fixtures_co$avg_HCO_superlig <- superlig_avg_HCO

SUPERLIG_fixtures_co$superlig_homecoas <- rep(superlig_home_coas,each = length(superlig_teams)-1)

superlig_awaycods_lookup <- cbind(superlig_teams,superlig_away_cods)

superlig_awaycods_lookup <- as.data.frame(superlig_awaycods_lookup)

colnames(superlig_awaycods_lookup) <- c("AwayTeam_superlig_co","superlig_awaycods")


require('RH2')
SUPERLIG_fixtures_co$superlig_awaycods <- sqldf("SELECT superlig_awaycods_lookup.superlig_awaycods FROM superlig_awaycods_lookup INNER JOIN SUPERLIG_fixtures_co ON superlig_awaycods_lookup.AwayTeam_superlig_co = SUPERLIG_fixtures_co.AwayTeam_superlig_co")

SUPERLIG_fixtures_co$avg_ACO_superlig <- superlig_avg_ACO

superlig_awaycoas_lookup <- cbind(superlig_teams,superlig_away_coas)

superlig_awaycoas_lookup <- as.data.frame(superlig_awaycoas_lookup)

colnames(superlig_awaycoas_lookup) <- c("AwayTeam_superlig_co","superlig_awaycoas")

SUPERLIG_fixtures_co$superlig_awaycoas <- sqldf("SELECT superlig_awaycoas_lookup.superlig_awaycoas FROM superlig_awaycoas_lookup INNER JOIN SUPERLIG_fixtures_co ON superlig_awaycoas_lookup.AwayTeam_superlig_co = SUPERLIG_fixtures_co.AwayTeam_superlig_co")

SUPERLIG_fixtures_co$superlig_homecods <- rep(superlig_home_cods,each = length(superlig_teams)-1)

SUPERLIG_fixtures_co$superlig_awaycods <- as.numeric(unlist(SUPERLIG_fixtures_co$superlig_awaycods))
#xGH
SUPERLIG_fixtures_co$superlig_xHCOC <- SUPERLIG_fixtures_co$avg_HCO_superlig * SUPERLIG_fixtures_co$superlig_homecoas * SUPERLIG_fixtures_co$superlig_awaycods
#xGA

SUPERLIG_fixtures_co$superlig_awaycoas <- as.numeric(unlist(SUPERLIG_fixtures_co$superlig_awaycoas))

SUPERLIG_fixtures_co$superlig_xACOC <- SUPERLIG_fixtures_co$avg_ACO_superlig * SUPERLIG_fixtures_co$superlig_awaycoas * SUPERLIG_fixtures_co$superlig_homecods

SUPERLIG_fixtures_co$superlig_0_0 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_0 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_0_1 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_1 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_0 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_0_2 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_2 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_1 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_2 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_3 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_0 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_1 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_2 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_0_3 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_3 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_3 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_4 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_0 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_1 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_2 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_3 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_0_4 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_4 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_4 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_4 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_5 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_0 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_1 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_2 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_3 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_4 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_0_5 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_5 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_5 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_5 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_5 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_6 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_0 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(0,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_1 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(1,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_2 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(2,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_3 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(3,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_4 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(4,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_6_5 <- round(stats::dpois(6,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(5,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_0_6 <- round(stats::dpois(0,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_1_6 <- round(stats::dpois(1,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_2_6 <- round(stats::dpois(2,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_3_6 <- round(stats::dpois(3,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_4_6 <- round(stats::dpois(4,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
SUPERLIG_fixtures_co$superlig_5_6 <- round(stats::dpois(5,SUPERLIG_fixtures_co$superlig_xHCOC) * stats::dpois(6,SUPERLIG_fixtures_co$superlig_xACOC), digits = 4)
#Home win
SUPERLIG_fixtures_co$superlig_H <- (
  SUPERLIG_fixtures_co$superlig_1_0 + SUPERLIG_fixtures_co$superlig_2_0 + SUPERLIG_fixtures_co$superlig_2_1 + SUPERLIG_fixtures_co$superlig_3_0 + SUPERLIG_fixtures_co$superlig_3_1 +
    SUPERLIG_fixtures_co$superlig_3_2 + SUPERLIG_fixtures_co$superlig_4_0 + SUPERLIG_fixtures_co$superlig_4_1 + SUPERLIG_fixtures_co$superlig_4_2 + SUPERLIG_fixtures_co$superlig_4_3 +
    SUPERLIG_fixtures_co$superlig_5_0 + SUPERLIG_fixtures_co$superlig_5_1 + SUPERLIG_fixtures_co$superlig_5_2 + SUPERLIG_fixtures_co$superlig_5_3 + SUPERLIG_fixtures_co$superlig_5_4 +
    SUPERLIG_fixtures_co$superlig_6_0 + SUPERLIG_fixtures_co$superlig_6_1 + SUPERLIG_fixtures_co$superlig_6_2 + SUPERLIG_fixtures_co$superlig_6_3 + SUPERLIG_fixtures_co$superlig_6_4 +
    SUPERLIG_fixtures_co$superlig_6_5
)

SUPERLIG_fixtures_co$superlig_H <- percent(SUPERLIG_fixtures_co$superlig_H, accuracy = 0.1)

#Draw
SUPERLIG_fixtures_co$superlig_D <- (

  SUPERLIG_fixtures_co$superlig_0_0 + SUPERLIG_fixtures_co$superlig_1_1 + SUPERLIG_fixtures_co$superlig_2_2 + SUPERLIG_fixtures_co$superlig_3_3 + SUPERLIG_fixtures_co$superlig_4_4 +
    SUPERLIG_fixtures_co$superlig_5_5 + SUPERLIG_fixtures_co$superlig_6_6
)

SUPERLIG_fixtures_co$superlig_D <- percent(SUPERLIG_fixtures_co$superlig_D, accuracy = 0.1)

#Away

SUPERLIG_fixtures_co$superlig_A <- (
  SUPERLIG_fixtures_co$superlig_0_1 + SUPERLIG_fixtures_co$superlig_0_2 + SUPERLIG_fixtures_co$superlig_1_2 + SUPERLIG_fixtures_co$superlig_0_3 + SUPERLIG_fixtures_co$superlig_1_3 +
    SUPERLIG_fixtures_co$superlig_2_3 + SUPERLIG_fixtures_co$superlig_0_4 + SUPERLIG_fixtures_co$superlig_1_4 + SUPERLIG_fixtures_co$superlig_2_4 + SUPERLIG_fixtures_co$superlig_3_4 +
    SUPERLIG_fixtures_co$superlig_0_5 + SUPERLIG_fixtures_co$superlig_1_5 + SUPERLIG_fixtures_co$superlig_2_5 + SUPERLIG_fixtures_co$superlig_3_5 + SUPERLIG_fixtures_co$superlig_4_5 +
    SUPERLIG_fixtures_co$superlig_0_6 + SUPERLIG_fixtures_co$superlig_1_6 + SUPERLIG_fixtures_co$superlig_2_6 + SUPERLIG_fixtures_co$superlig_3_6 + SUPERLIG_fixtures_co$superlig_4_6 +
    SUPERLIG_fixtures_co$superlig_5_6
)

SUPERLIG_fixtures_co$superlig_A <- percent(SUPERLIG_fixtures_co$superlig_A, accuracy = 0.1)

#ov25
SUPERLIG_fixtures_co$superlig_ov25 <- (
  SUPERLIG_fixtures_co$superlig_2_1 + SUPERLIG_fixtures_co$superlig_1_2 + SUPERLIG_fixtures_co$superlig_2_2 + SUPERLIG_fixtures_co$superlig_3_0 + SUPERLIG_fixtures_co$superlig_3_1 +
    SUPERLIG_fixtures_co$superlig_3_2 + SUPERLIG_fixtures_co$superlig_0_3 + SUPERLIG_fixtures_co$superlig_1_3 + SUPERLIG_fixtures_co$superlig_2_3 + SUPERLIG_fixtures_co$superlig_3_3 +
    SUPERLIG_fixtures_co$superlig_4_0 + SUPERLIG_fixtures_co$superlig_4_1 + SUPERLIG_fixtures_co$superlig_4_2 + SUPERLIG_fixtures_co$superlig_4_3 + SUPERLIG_fixtures_co$superlig_0_4 +
    SUPERLIG_fixtures_co$superlig_1_4 + SUPERLIG_fixtures_co$superlig_2_4 + SUPERLIG_fixtures_co$superlig_3_4 + SUPERLIG_fixtures_co$superlig_4_4 + SUPERLIG_fixtures_co$superlig_5_0 +
    SUPERLIG_fixtures_co$superlig_5_1 + SUPERLIG_fixtures_co$superlig_5_2 + SUPERLIG_fixtures_co$superlig_5_3 + SUPERLIG_fixtures_co$superlig_5_4 + SUPERLIG_fixtures_co$superlig_0_5 +
    SUPERLIG_fixtures_co$superlig_1_5 + SUPERLIG_fixtures_co$superlig_2_5 + SUPERLIG_fixtures_co$superlig_3_5 + SUPERLIG_fixtures_co$superlig_4_5 + SUPERLIG_fixtures_co$superlig_5_5 +
    SUPERLIG_fixtures_co$superlig_6_0 + SUPERLIG_fixtures_co$superlig_6_1 + SUPERLIG_fixtures_co$superlig_6_2 + SUPERLIG_fixtures_co$superlig_6_3 + SUPERLIG_fixtures_co$superlig_6_4 +
    SUPERLIG_fixtures_co$superlig_6_5 + SUPERLIG_fixtures_co$superlig_0_6 + SUPERLIG_fixtures_co$superlig_1_6 + SUPERLIG_fixtures_co$superlig_2_6 + SUPERLIG_fixtures_co$superlig_3_6 +
    SUPERLIG_fixtures_co$superlig_4_6 + SUPERLIG_fixtures_co$superlig_5_6 + SUPERLIG_fixtures_co$superlig_6_6
)
#un25
SUPERLIG_fixtures_co$superlig_un25 <- (
  SUPERLIG_fixtures_co$superlig_0_0 + SUPERLIG_fixtures_co$superlig_1_0 + SUPERLIG_fixtures_co$superlig_0_1 + SUPERLIG_fixtures_co$superlig_1_1 + SUPERLIG_fixtures_co$superlig_2_0 + SUPERLIG_fixtures_co$superlig_0_2
)
#odds
SUPERLIG_fixtures_co$superlig_ov25_odds <- round((1/SUPERLIG_fixtures_co$superlig_ov25),digits = 2)
SUPERLIG_fixtures_co$superlig_un25_odds <- round((1/SUPERLIG_fixtures_co$superlig_un25),digits = 2)

SUPERLIG_fixtures_co$superlig_ov25_odds
SUPERLIG_fixtures_co$superlig_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
SUPERLIG_fixtures_co$superlig_ov25 <- percent(SUPERLIG_fixtures_co$superlig_ov25, accuracy = 0.1)

SUPERLIG_fixtures_co$superlig_un25 <- percent(SUPERLIG_fixtures_co$superlig_un25, accuracy = 0.1)
SUPERLIG_fixtures_co$superlig_pscore <- paste(round(SUPERLIG_fixtures_co$superlig_xHCOC,digits = 0),round(SUPERLIG_fixtures_co$superlig_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
superlig_GP <- nrow(SUPERLIG)
#Calculate total home goals for each division
superlig_T_HF <- sum(superlig_home_fouls$x)
#calculate average home goal
superlig_avg_HF <- round(superlig_T_HF /superlig_GP, digits = 4)
############################################################
#Calculate total away goals for each division
superlig_T_AF <- sum(superlig_away_fouls$x)
#calculate average away goal
superlig_avg_AF <- round(superlig_T_AF /superlig_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
superlig_home_fas <- round(((superlig_home_fouls$x/superlig_home_games))/superlig_avg_HF, digits = 4)
#calculate away attack strength
superlig_away_fas <- round(((superlig_away_fouls$x/superlig_away_games))/superlig_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
superlig_avg_HFC <- round(superlig_T_AF /superlig_GP, digits = 4)
#avg away concede
superlig_avg_AFC <- round(superlig_T_HF /superlig_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
superlig_home_fcc <- aggregate(SUPERLIG$AF, by = list(SUPERLIG$HomeTeam), FUN = sum)
superlig_away_fcc <- aggregate(SUPERLIG$HF, by = list(SUPERLIG$AwayTeam), FUN = sum)

#home defense strength
superlig_home_fds <- round(((superlig_home_fcc$x/superlig_home_games))/superlig_avg_HFC, digits = 4)

#away defense strength
superlig_away_fds <- round(((superlig_away_fcc$x/superlig_away_games))/superlig_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_home_poisson_fo <- cbind(superlig_division,superlig_teams,superlig_avg_HF,superlig_home_fas,superlig_home_fds)

#################################################################################
#away poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_away_poisson_fo <- cbind(superlig_division,superlig_teams,superlig_avg_AF,superlig_away_fas,superlig_away_fds)

#SUPERLIG
HomeTeam_superlig_fo <- rep(superlig_teams, each = length(superlig_teams))
AwayTeam_superlig_fo <- rep(superlig_teams, length(superlig_teams))
SUPERLIG_fixtures_fo <- cbind(HomeTeam_superlig_fo,AwayTeam_superlig_fo)
SUPERLIG_fixtures_fo <- as.data.frame(SUPERLIG_fixtures_fo)
SUPERLIG_fixtures_fo <- SUPERLIG_fixtures_fo[!SUPERLIG_fixtures_fo$HomeTeam_superlig_fo == SUPERLIG_fixtures_fo$AwayTeam_superlig_fo,]
rownames(SUPERLIG_fixtures_fo) <- NULL
SUPERLIG_fixtures_fo$Div <- "SUPERLIG"
SUPERLIG_fixtures_fo <- SUPERLIG_fixtures_fo[,c(3,1,2)]

SUPERLIG_fixtures_fo$avg_HF_superlig <- superlig_avg_HF

SUPERLIG_fixtures_fo$superlig_homefas <- rep(superlig_home_fas,each = length(superlig_teams)-1)

superlig_awayfds_lookup <- cbind(superlig_teams,superlig_away_fds)

superlig_awayfds_lookup <- as.data.frame(superlig_awayfds_lookup)

colnames(superlig_awayfds_lookup) <- c("AwayTeam_superlig_fo","superlig_awayfds")


require('RH2')
SUPERLIG_fixtures_fo$superlig_awayfds <- sqldf("SELECT superlig_awayfds_lookup.superlig_awayfds FROM superlig_awayfds_lookup INNER JOIN SUPERLIG_fixtures_fo ON superlig_awayfds_lookup.AwayTeam_superlig_fo = SUPERLIG_fixtures_fo.AwayTeam_superlig_fo")

SUPERLIG_fixtures_fo$avg_AF_superlig <- superlig_avg_AF

superlig_awayfas_lookup <- cbind(superlig_teams,superlig_away_fas)

superlig_awayfas_lookup <- as.data.frame(superlig_awayfas_lookup)

colnames(superlig_awayfas_lookup) <- c("AwayTeam_superlig_fo","superlig_awayfas")

SUPERLIG_fixtures_fo$superlig_awayfas <- sqldf("SELECT superlig_awayfas_lookup.superlig_awayfas FROM superlig_awayfas_lookup INNER JOIN SUPERLIG_fixtures_fo ON superlig_awayfas_lookup.AwayTeam_superlig_fo = SUPERLIG_fixtures_fo.AwayTeam_superlig_fo")

SUPERLIG_fixtures_fo$superlig_homefds <- rep(superlig_home_fds,each = length(superlig_teams)-1)

SUPERLIG_fixtures_fo$superlig_awayfds <- as.numeric(unlist(SUPERLIG_fixtures_fo$superlig_awayfds))
#xGH
SUPERLIG_fixtures_fo$superlig_xHF <- SUPERLIG_fixtures_fo$avg_HF_superlig * SUPERLIG_fixtures_fo$superlig_homefas * SUPERLIG_fixtures_fo$superlig_awayfds
#xGA

SUPERLIG_fixtures_fo$superlig_awayfas <- as.numeric(unlist(SUPERLIG_fixtures_fo$superlig_awayfas))

SUPERLIG_fixtures_fo$superlig_xAF <- SUPERLIG_fixtures_fo$avg_AF_superlig * SUPERLIG_fixtures_fo$superlig_awayfas * SUPERLIG_fixtures_fo$superlig_homefds

SUPERLIG_fixtures_fo$superlig_0_0 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_0 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_0_1 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_1 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_0 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_0_2 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_2 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_1 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_2 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_3 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_0 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_1 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_2 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_0_3 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_3 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_3 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_4 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_0 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_1 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_2 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_3 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_0_4 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_4 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_4 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_4 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_5 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_0 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_1 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_2 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_3 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_4 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_0_5 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_5 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_5 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_5 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_5 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_6 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_0 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_1 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_2 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_3 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_4 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_6_5 <- round(stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_0_6 <- round(stats::dpois(0,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_1_6 <- round(stats::dpois(1,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_2_6 <- round(stats::dpois(2,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_3_6 <- round(stats::dpois(3,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_4_6 <- round(stats::dpois(4,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
SUPERLIG_fixtures_fo$superlig_5_6 <- round(stats::dpois(5,SUPERLIG_fixtures_fo$superlig_xHF) * stats::dpois(6,SUPERLIG_fixtures_fo$superlig_xAF), digits = 4)
#Home win
SUPERLIG_fixtures_fo$superlig_H <- (
  SUPERLIG_fixtures_fo$superlig_1_0 + SUPERLIG_fixtures_fo$superlig_2_0 + SUPERLIG_fixtures_fo$superlig_2_1 + SUPERLIG_fixtures_fo$superlig_3_0 + SUPERLIG_fixtures_fo$superlig_3_1 +
    SUPERLIG_fixtures_fo$superlig_3_2 + SUPERLIG_fixtures_fo$superlig_4_0 + SUPERLIG_fixtures_fo$superlig_4_1 + SUPERLIG_fixtures_fo$superlig_4_2 + SUPERLIG_fixtures_fo$superlig_4_3 +
    SUPERLIG_fixtures_fo$superlig_5_0 + SUPERLIG_fixtures_fo$superlig_5_1 + SUPERLIG_fixtures_fo$superlig_5_2 + SUPERLIG_fixtures_fo$superlig_5_3 + SUPERLIG_fixtures_fo$superlig_5_4 +
    SUPERLIG_fixtures_fo$superlig_6_0 + SUPERLIG_fixtures_fo$superlig_6_1 + SUPERLIG_fixtures_fo$superlig_6_2 + SUPERLIG_fixtures_fo$superlig_6_3 + SUPERLIG_fixtures_fo$superlig_6_4 +
    SUPERLIG_fixtures_fo$superlig_6_5
)

SUPERLIG_fixtures_fo$superlig_H <- percent(SUPERLIG_fixtures_fo$superlig_H, accuracy = 0.1)

#Draw
SUPERLIG_fixtures_fo$superlig_D <- (

  SUPERLIG_fixtures_fo$superlig_0_0 + SUPERLIG_fixtures_fo$superlig_1_1 + SUPERLIG_fixtures_fo$superlig_2_2 + SUPERLIG_fixtures_fo$superlig_3_3 + SUPERLIG_fixtures_fo$superlig_4_4 +
    SUPERLIG_fixtures_fo$superlig_5_5 + SUPERLIG_fixtures_fo$superlig_6_6
)

SUPERLIG_fixtures_fo$superlig_D <- percent(SUPERLIG_fixtures_fo$superlig_D, accuracy = 0.1)

#Away

SUPERLIG_fixtures_fo$superlig_A <- (
  SUPERLIG_fixtures_fo$superlig_0_1 + SUPERLIG_fixtures_fo$superlig_0_2 + SUPERLIG_fixtures_fo$superlig_1_2 + SUPERLIG_fixtures_fo$superlig_0_3 + SUPERLIG_fixtures_fo$superlig_1_3 +
    SUPERLIG_fixtures_fo$superlig_2_3 + SUPERLIG_fixtures_fo$superlig_0_4 + SUPERLIG_fixtures_fo$superlig_1_4 + SUPERLIG_fixtures_fo$superlig_2_4 + SUPERLIG_fixtures_fo$superlig_3_4 +
    SUPERLIG_fixtures_fo$superlig_0_5 + SUPERLIG_fixtures_fo$superlig_1_5 + SUPERLIG_fixtures_fo$superlig_2_5 + SUPERLIG_fixtures_fo$superlig_3_5 + SUPERLIG_fixtures_fo$superlig_4_5 +
    SUPERLIG_fixtures_fo$superlig_0_6 + SUPERLIG_fixtures_fo$superlig_1_6 + SUPERLIG_fixtures_fo$superlig_2_6 + SUPERLIG_fixtures_fo$superlig_3_6 + SUPERLIG_fixtures_fo$superlig_4_6 +
    SUPERLIG_fixtures_fo$superlig_5_6
)

SUPERLIG_fixtures_fo$superlig_A <- percent(SUPERLIG_fixtures_fo$superlig_A, accuracy = 0.1)

#ov25
SUPERLIG_fixtures_fo$superlig_ov25 <- (
  SUPERLIG_fixtures_fo$superlig_2_1 + SUPERLIG_fixtures_fo$superlig_1_2 + SUPERLIG_fixtures_fo$superlig_2_2 + SUPERLIG_fixtures_fo$superlig_3_0 + SUPERLIG_fixtures_fo$superlig_3_1 +
    SUPERLIG_fixtures_fo$superlig_3_2 + SUPERLIG_fixtures_fo$superlig_0_3 + SUPERLIG_fixtures_fo$superlig_1_3 + SUPERLIG_fixtures_fo$superlig_2_3 + SUPERLIG_fixtures_fo$superlig_3_3 +
    SUPERLIG_fixtures_fo$superlig_4_0 + SUPERLIG_fixtures_fo$superlig_4_1 + SUPERLIG_fixtures_fo$superlig_4_2 + SUPERLIG_fixtures_fo$superlig_4_3 + SUPERLIG_fixtures_fo$superlig_0_4 +
    SUPERLIG_fixtures_fo$superlig_1_4 + SUPERLIG_fixtures_fo$superlig_2_4 + SUPERLIG_fixtures_fo$superlig_3_4 + SUPERLIG_fixtures_fo$superlig_4_4 + SUPERLIG_fixtures_fo$superlig_5_0 +
    SUPERLIG_fixtures_fo$superlig_5_1 + SUPERLIG_fixtures_fo$superlig_5_2 + SUPERLIG_fixtures_fo$superlig_5_3 + SUPERLIG_fixtures_fo$superlig_5_4 + SUPERLIG_fixtures_fo$superlig_0_5 +
    SUPERLIG_fixtures_fo$superlig_1_5 + SUPERLIG_fixtures_fo$superlig_2_5 + SUPERLIG_fixtures_fo$superlig_3_5 + SUPERLIG_fixtures_fo$superlig_4_5 + SUPERLIG_fixtures_fo$superlig_5_5 +
    SUPERLIG_fixtures_fo$superlig_6_0 + SUPERLIG_fixtures_fo$superlig_6_1 + SUPERLIG_fixtures_fo$superlig_6_2 + SUPERLIG_fixtures_fo$superlig_6_3 + SUPERLIG_fixtures_fo$superlig_6_4 +
    SUPERLIG_fixtures_fo$superlig_6_5 + SUPERLIG_fixtures_fo$superlig_0_6 + SUPERLIG_fixtures_fo$superlig_1_6 + SUPERLIG_fixtures_fo$superlig_2_6 + SUPERLIG_fixtures_fo$superlig_3_6 +
    SUPERLIG_fixtures_fo$superlig_4_6 + SUPERLIG_fixtures_fo$superlig_5_6 + SUPERLIG_fixtures_fo$superlig_6_6
)
#un25
SUPERLIG_fixtures_fo$superlig_un25 <- (
  SUPERLIG_fixtures_fo$superlig_0_0 + SUPERLIG_fixtures_fo$superlig_1_0 + SUPERLIG_fixtures_fo$superlig_0_1 + SUPERLIG_fixtures_fo$superlig_1_1 + SUPERLIG_fixtures_fo$superlig_2_0 + SUPERLIG_fixtures_fo$superlig_0_2
)
#odds
SUPERLIG_fixtures_fo$superlig_ov25_odds <- round((1/SUPERLIG_fixtures_fo$superlig_ov25),digits = 2)
SUPERLIG_fixtures_fo$superlig_un25_odds <- round((1/SUPERLIG_fixtures_fo$superlig_un25),digits = 2)

SUPERLIG_fixtures_fo$superlig_ov25_odds
SUPERLIG_fixtures_fo$superlig_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
SUPERLIG_fixtures_fo$superlig_ov25 <- percent(SUPERLIG_fixtures_fo$superlig_ov25, accuracy = 0.1)

SUPERLIG_fixtures_fo$superlig_un25 <- percent(SUPERLIG_fixtures_fo$superlig_un25, accuracy = 0.1)
SUPERLIG_fixtures_fo$superlig_psfore <- paste(round(SUPERLIG_fixtures_fo$superlig_xHF,digits = 0),round(SUPERLIG_fixtures_fo$superlig_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
superlig_GP <- nrow(SUPERLIG)

#Calculate total home goals for each division
superlig_T_HST <- sum(superlig_home_hst$x)
#calculate average home goal

superlig_avg_HST <- round(superlig_T_HST /superlig_GP, digits = 4)

############################################################
#Calculate total away goals for each division
superlig_T_AST <- sum(superlig_away_ast$x)
#calculate average away goal
superlig_avg_AST <- round(superlig_T_AST /superlig_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
superlig_home_sotas <- round(((superlig_home_hst$x/superlig_home_games))/superlig_avg_HST, digits = 4)
#calculate away attack strength
superlig_away_sotas <- round(((superlig_away_ast$x/superlig_away_games))/superlig_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
superlig_avg_HSC <- round(superlig_T_AST /superlig_GP, digits = 4)

#avg away concede
superlig_avg_ASC <- round(superlig_T_HST /superlig_GP, digits = 4)
#home defense strength
superlig_home_sods <- round(((superlig_home_hsc$x/superlig_home_games))/superlig_avg_HSC, digits = 4)

#away defense strength
superlig_away_sods <- round(((superlig_away_ast$x/superlig_away_games))/superlig_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_home_poisson_sot <- cbind(superlig_division,superlig_teams,superlig_avg_HST,superlig_home_sotas,superlig_home_sods)

#################################################################################
#away poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_away_poisson_sot <- cbind(superlig_division,superlig_teams,superlig_avg_AST,superlig_away_sotas,superlig_away_sods)

#SUPERLIG
HomeTeam_superlig_sot <- rep(superlig_teams, each = length(superlig_teams))
AwayTeam_superlig_sot <- rep(superlig_teams, length(superlig_teams))
SUPERLIG_fixtures_sot <- cbind(HomeTeam_superlig_sot,AwayTeam_superlig_sot)
SUPERLIG_fixtures_sot <- as.data.frame(SUPERLIG_fixtures_sot)
SUPERLIG_fixtures_sot <- SUPERLIG_fixtures_sot[!SUPERLIG_fixtures_sot$HomeTeam_superlig_sot == SUPERLIG_fixtures_sot$AwayTeam_superlig_sot,]
rownames(SUPERLIG_fixtures_sot) <- NULL
SUPERLIG_fixtures_sot$Div <- "SUPERLIG"
SUPERLIG_fixtures_sot <- SUPERLIG_fixtures_sot[,c(3,1,2)]

SUPERLIG_fixtures_sot$avg_HST_superlig <- superlig_avg_HST

SUPERLIG_fixtures_sot$superlig_homesotas <- rep(superlig_home_sotas,each = length(superlig_teams)-1)

superlig_awaysods_lookup <- cbind(superlig_teams,superlig_away_sods)

superlig_awaysods_lookup <- as.data.frame(superlig_awaysods_lookup)

colnames(superlig_awaysods_lookup) <- c("AwayTeam_superlig_sot","superlig_awaysods")


require('RH2')
SUPERLIG_fixtures_sot$superlig_awaysods <- sqldf("SELECT superlig_awaysods_lookup.superlig_awaysods FROM superlig_awaysods_lookup INNER JOIN SUPERLIG_fixtures_sot ON superlig_awaysods_lookup.AwayTeam_superlig_sot = SUPERLIG_fixtures_sot.AwayTeam_superlig_sot")

SUPERLIG_fixtures_sot$avg_AST_superlig <- superlig_avg_AST

superlig_awaysotas_lookup <- cbind(superlig_teams,superlig_away_sotas)

superlig_awaysotas_lookup <- as.data.frame(superlig_awaysotas_lookup)

colnames(superlig_awaysotas_lookup) <- c("AwayTeam_superlig_sot","superlig_awaysotas")

SUPERLIG_fixtures_sot$superlig_awaysotas <- sqldf("SELECT superlig_awaysotas_lookup.superlig_awaysotas FROM superlig_awaysotas_lookup INNER JOIN SUPERLIG_fixtures_sot ON superlig_awaysotas_lookup.AwayTeam_superlig_sot = SUPERLIG_fixtures_sot.AwayTeam_superlig_sot")

SUPERLIG_fixtures_sot$superlig_homesods <- rep(superlig_home_sods,each = length(superlig_teams)-1)

SUPERLIG_fixtures_sot$superlig_awaysods <- as.numeric(unlist(SUPERLIG_fixtures_sot$superlig_awaysods))
#xGH
SUPERLIG_fixtures_sot$superlig_xHST <- SUPERLIG_fixtures_sot$avg_HST_superlig * SUPERLIG_fixtures_sot$superlig_homesotas * SUPERLIG_fixtures_sot$superlig_awaysods
#xGA

SUPERLIG_fixtures_sot$superlig_awaysotas <- as.numeric(unlist(SUPERLIG_fixtures_sot$superlig_awaysotas))

SUPERLIG_fixtures_sot$superlig_xAST <- SUPERLIG_fixtures_sot$avg_AST_superlig * SUPERLIG_fixtures_sot$superlig_awaysotas * SUPERLIG_fixtures_sot$superlig_homesods

SUPERLIG_fixtures_sot$superlig_0_0 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_0 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_0_1 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_1 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_0 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_0_2 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_2 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_1 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_2 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_3 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_0 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_1 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_2 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_0_3 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_3 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_3 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_4 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_0 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_1 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_2 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_3 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_0_4 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_4 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_4 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_4 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_5 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_0 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_1 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_2 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_3 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_4 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_0_5 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_5 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_5 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_5 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_5 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_6 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_0 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_1 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_2 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_3 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_4 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_6_5 <- round(stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_0_6 <- round(stats::dpois(0,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_1_6 <- round(stats::dpois(1,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_2_6 <- round(stats::dpois(2,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_3_6 <- round(stats::dpois(3,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_4_6 <- round(stats::dpois(4,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
SUPERLIG_fixtures_sot$superlig_5_6 <- round(stats::dpois(5,SUPERLIG_fixtures_sot$superlig_xHST) * stats::dpois(6,SUPERLIG_fixtures_sot$superlig_xAST), digits = 4)
#Home win
SUPERLIG_fixtures_sot$superlig_H <- (
  SUPERLIG_fixtures_sot$superlig_1_0 + SUPERLIG_fixtures_sot$superlig_2_0 + SUPERLIG_fixtures_sot$superlig_2_1 + SUPERLIG_fixtures_sot$superlig_3_0 + SUPERLIG_fixtures_sot$superlig_3_1 +
    SUPERLIG_fixtures_sot$superlig_3_2 + SUPERLIG_fixtures_sot$superlig_4_0 + SUPERLIG_fixtures_sot$superlig_4_1 + SUPERLIG_fixtures_sot$superlig_4_2 + SUPERLIG_fixtures_sot$superlig_4_3 +
    SUPERLIG_fixtures_sot$superlig_5_0 + SUPERLIG_fixtures_sot$superlig_5_1 + SUPERLIG_fixtures_sot$superlig_5_2 + SUPERLIG_fixtures_sot$superlig_5_3 + SUPERLIG_fixtures_sot$superlig_5_4 +
    SUPERLIG_fixtures_sot$superlig_6_0 + SUPERLIG_fixtures_sot$superlig_6_1 + SUPERLIG_fixtures_sot$superlig_6_2 + SUPERLIG_fixtures_sot$superlig_6_3 + SUPERLIG_fixtures_sot$superlig_6_4 +
    SUPERLIG_fixtures_sot$superlig_6_5
)

SUPERLIG_fixtures_sot$superlig_H <- percent(SUPERLIG_fixtures_sot$superlig_H, accuracy = 0.1)

#Draw
SUPERLIG_fixtures_sot$superlig_D <- (

  SUPERLIG_fixtures_sot$superlig_0_0 + SUPERLIG_fixtures_sot$superlig_1_1 + SUPERLIG_fixtures_sot$superlig_2_2 + SUPERLIG_fixtures_sot$superlig_3_3 + SUPERLIG_fixtures_sot$superlig_4_4 +
    SUPERLIG_fixtures_sot$superlig_5_5 + SUPERLIG_fixtures_sot$superlig_6_6
)

SUPERLIG_fixtures_sot$superlig_D <- percent(SUPERLIG_fixtures_sot$superlig_D, accuracy = 0.1)

#Away

SUPERLIG_fixtures_sot$superlig_A <- (
  SUPERLIG_fixtures_sot$superlig_0_1 + SUPERLIG_fixtures_sot$superlig_0_2 + SUPERLIG_fixtures_sot$superlig_1_2 + SUPERLIG_fixtures_sot$superlig_0_3 + SUPERLIG_fixtures_sot$superlig_1_3 +
    SUPERLIG_fixtures_sot$superlig_2_3 + SUPERLIG_fixtures_sot$superlig_0_4 + SUPERLIG_fixtures_sot$superlig_1_4 + SUPERLIG_fixtures_sot$superlig_2_4 + SUPERLIG_fixtures_sot$superlig_3_4 +
    SUPERLIG_fixtures_sot$superlig_0_5 + SUPERLIG_fixtures_sot$superlig_1_5 + SUPERLIG_fixtures_sot$superlig_2_5 + SUPERLIG_fixtures_sot$superlig_3_5 + SUPERLIG_fixtures_sot$superlig_4_5 +
    SUPERLIG_fixtures_sot$superlig_0_6 + SUPERLIG_fixtures_sot$superlig_1_6 + SUPERLIG_fixtures_sot$superlig_2_6 + SUPERLIG_fixtures_sot$superlig_3_6 + SUPERLIG_fixtures_sot$superlig_4_6 +
    SUPERLIG_fixtures_sot$superlig_5_6
)

SUPERLIG_fixtures_sot$superlig_A <- percent(SUPERLIG_fixtures_sot$superlig_A, accuracy = 0.1)

#ov25
SUPERLIG_fixtures_sot$superlig_ov25 <- (
  SUPERLIG_fixtures_sot$superlig_2_1 + SUPERLIG_fixtures_sot$superlig_1_2 + SUPERLIG_fixtures_sot$superlig_2_2 + SUPERLIG_fixtures_sot$superlig_3_0 + SUPERLIG_fixtures_sot$superlig_3_1 +
    SUPERLIG_fixtures_sot$superlig_3_2 + SUPERLIG_fixtures_sot$superlig_0_3 + SUPERLIG_fixtures_sot$superlig_1_3 + SUPERLIG_fixtures_sot$superlig_2_3 + SUPERLIG_fixtures_sot$superlig_3_3 +
    SUPERLIG_fixtures_sot$superlig_4_0 + SUPERLIG_fixtures_sot$superlig_4_1 + SUPERLIG_fixtures_sot$superlig_4_2 + SUPERLIG_fixtures_sot$superlig_4_3 + SUPERLIG_fixtures_sot$superlig_0_4 +
    SUPERLIG_fixtures_sot$superlig_1_4 + SUPERLIG_fixtures_sot$superlig_2_4 + SUPERLIG_fixtures_sot$superlig_3_4 + SUPERLIG_fixtures_sot$superlig_4_4 + SUPERLIG_fixtures_sot$superlig_5_0 +
    SUPERLIG_fixtures_sot$superlig_5_1 + SUPERLIG_fixtures_sot$superlig_5_2 + SUPERLIG_fixtures_sot$superlig_5_3 + SUPERLIG_fixtures_sot$superlig_5_4 + SUPERLIG_fixtures_sot$superlig_0_5 +
    SUPERLIG_fixtures_sot$superlig_1_5 + SUPERLIG_fixtures_sot$superlig_2_5 + SUPERLIG_fixtures_sot$superlig_3_5 + SUPERLIG_fixtures_sot$superlig_4_5 + SUPERLIG_fixtures_sot$superlig_5_5 +
    SUPERLIG_fixtures_sot$superlig_6_0 + SUPERLIG_fixtures_sot$superlig_6_1 + SUPERLIG_fixtures_sot$superlig_6_2 + SUPERLIG_fixtures_sot$superlig_6_3 + SUPERLIG_fixtures_sot$superlig_6_4 +
    SUPERLIG_fixtures_sot$superlig_6_5 + SUPERLIG_fixtures_sot$superlig_0_6 + SUPERLIG_fixtures_sot$superlig_1_6 + SUPERLIG_fixtures_sot$superlig_2_6 + SUPERLIG_fixtures_sot$superlig_3_6 +
    SUPERLIG_fixtures_sot$superlig_4_6 + SUPERLIG_fixtures_sot$superlig_5_6 + SUPERLIG_fixtures_sot$superlig_6_6
)
#un25
SUPERLIG_fixtures_sot$superlig_un25 <- (
  SUPERLIG_fixtures_sot$superlig_0_0 + SUPERLIG_fixtures_sot$superlig_1_0 + SUPERLIG_fixtures_sot$superlig_0_1 + SUPERLIG_fixtures_sot$superlig_1_1 + SUPERLIG_fixtures_sot$superlig_2_0 + SUPERLIG_fixtures_sot$superlig_0_2
)
#odds
SUPERLIG_fixtures_sot$superlig_ov25_odds <- round((1/SUPERLIG_fixtures_sot$superlig_ov25),digits = 2)
SUPERLIG_fixtures_sot$superlig_un25_odds <- round((1/SUPERLIG_fixtures_sot$superlig_un25),digits = 2)

SUPERLIG_fixtures_sot$superlig_ov25_odds
SUPERLIG_fixtures_sot$superlig_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
SUPERLIG_fixtures_sot$superlig_ov25 <- percent(SUPERLIG_fixtures_sot$superlig_ov25, accuracy = 0.1)

SUPERLIG_fixtures_sot$superlig_un25 <- percent(SUPERLIG_fixtures_sot$superlig_un25, accuracy = 0.1)
SUPERLIG_fixtures_sot$superlig_pssotre <- paste(round(SUPERLIG_fixtures_sot$superlig_xHST,digits = 0),round(SUPERLIG_fixtures_sot$superlig_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
superlig_home_wins <- c()
superlig_away_wins <- c()
superlig_home_draws <- c()
superlig_away_draws <- c()
superlig_home_loss <- c()
superlig_away_loss <- c()



for (i_superlig_wins in 1:length(superlig_teams))
{

  superlig_home_wins[i_superlig_wins] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_wins] & SUPERLIG$FTR == "H",])
  superlig_away_wins[i_superlig_wins] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_wins] & SUPERLIG$FTR == "A",])
  superlig_home_draws[i_superlig_wins] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_wins] & SUPERLIG$FTR == "D",])
  superlig_away_draws[i_superlig_wins] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_wins] & SUPERLIG$FTR == "D",])
  superlig_home_loss[i_superlig_wins] <- nrow(SUPERLIG[SUPERLIG$HomeTeam == superlig_teams[i_superlig_wins] & SUPERLIG$FTR == "A",])
  superlig_away_loss[i_superlig_wins] <- nrow(SUPERLIG[SUPERLIG$AwayTeam == superlig_teams[i_superlig_wins] & SUPERLIG$FTR == "H",])

}

superlig_total_wins <- superlig_home_wins + superlig_away_wins
superlig_total_draws <- superlig_home_draws + superlig_away_draws
superlig_total_loss <- superlig_home_loss + superlig_away_loss

superlig_league_table <- cbind(superlig_teams,superlig_games_played,superlig_total_wins,superlig_total_draws,superlig_total_loss)
superlig_GS <- superlig_scoring$TGS
superlig_GC <-superlig_conceding$TGC
superlig_GD <- superlig_scoring$TGS - superlig_conceding$TGC
superlig_PTS <- (superlig_total_wins*3) + (superlig_total_draws*1)
superlig_league_table <- cbind(superlig_league_table,superlig_GS,superlig_GC,superlig_GD,superlig_PTS)
superlig_league_table <- as.data.frame(superlig_league_table)
#rename the columns
names(superlig_league_table)[names(superlig_league_table) == "superlig_teams"] <- "Team"
names(superlig_league_table)[names(superlig_league_table) == "superlig_games_played"] <- "P"
names(superlig_league_table)[names(superlig_league_table) == "superlig_total_wins"] <- "W"
names(superlig_league_table)[names(superlig_league_table) == "superlig_total_draws"] <- "D"
names(superlig_league_table)[names(superlig_league_table) == "superlig_total_loss"] <- "L"
names(superlig_league_table)[names(superlig_league_table) == "superlig_GS"] <- "F"
names(superlig_league_table)[names(superlig_league_table) == "superlig_GC"] <- "A"
points_superlig <- superlig_league_table[order(as.numeric(superlig_league_table$superlig_PTS), decreasing = TRUE),]
points_superlig$superlig_rank <- 1:length(superlig_teams)
row.names(points_superlig) <- points_superlig$superlig_rank
#create final_superlig_hf_against with team ranks in brackets
for(superlig_rowhrank in 1:nrow(superlig_form_team_against_h)) {
  for(superlig_colhrank in 1:ncol(superlig_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!superlig_form_team_against_h[superlig_rowhrank,superlig_colhrank]=="",superlig_form_team_against_h[superlig_rowhrank,superlig_colhrank] <- paste(superlig_form_team_against_h[superlig_rowhrank,superlig_colhrank],"(",points_superlig$superlig_rank[points_superlig$Team ==superlig_form_team_against_h[superlig_rowhrank,superlig_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
superlig_GP <- nrow(SUPERLIG)

#Calculate total home goals for each division
superlig_T_HG <- sum(superlig_home_gs$x)

#calculate average home goal
superlig_avg_HG <- round(superlig_T_HG /superlig_GP, digits = 4)
############################################################
#Calculate total away goals for each division
superlig_T_AG <- sum(superlig_away_gs$x)
#calculate average away goal
superlig_avg_AG <- round(superlig_T_AG /superlig_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
superlig_home_as <- round(((superlig_home_gs$x/superlig_home_games))/superlig_avg_HG, digits = 4)
#calculate away attack strength
superlig_away_as <- round(((superlig_away_gs$x/superlig_away_games))/superlig_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
superlig_avg_HC <- round(superlig_T_AG /superlig_GP, digits = 4)
#avg away concede
superlig_avg_AC <- round(superlig_T_HG /superlig_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
superlig_home_ds <- round(((superlig_home_gc$x/superlig_home_games))/superlig_avg_HC, digits = 4)
#away defense strength
superlig_away_ds <- round(((superlig_away_gc$x/superlig_away_games))/superlig_avg_AC, digits = 4)
#############################################################################
#home poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_home_poisson <- cbind(superlig_division,superlig_teams,superlig_avg_HG,superlig_home_as,superlig_home_ds)
#################################################################################
#away poisson data
#superlig
superlig_division <- c()
superlig_division[1:length(superlig_teams)] <- "SUPERLIG"
superlig_away_poisson <- cbind(superlig_division,superlig_teams,superlig_avg_AG,superlig_away_as,superlig_away_ds)

#SUPERLIG
HomeTeam_superlig <- rep(superlig_teams, each = length(superlig_teams))
AwayTeam_superlig <- rep(superlig_teams, length(superlig_teams))
SUPERLIG_fixtures <- cbind(HomeTeam_superlig,AwayTeam_superlig)
SUPERLIG_fixtures <- as.data.frame(SUPERLIG_fixtures)
SUPERLIG_fixtures <- SUPERLIG_fixtures[!SUPERLIG_fixtures$HomeTeam_superlig == SUPERLIG_fixtures$AwayTeam_superlig,]
rownames(SUPERLIG_fixtures) <- NULL
SUPERLIG_fixtures$Div <- "SUPERLIG"
SUPERLIG_fixtures <- SUPERLIG_fixtures[,c(3,1,2)]

SUPERLIG_fixtures$avg_HG_superlig <- superlig_avg_HG

SUPERLIG_fixtures$superlig_homeas <- rep(superlig_home_as,each = length(superlig_teams)-1)

superlig_awayds_lookup <- cbind(superlig_teams,superlig_away_ds)

superlig_awayds_lookup <- as.data.frame(superlig_awayds_lookup)

colnames(superlig_awayds_lookup) <- c("AwayTeam_superlig","superlig_awayds")


require('RH2')
SUPERLIG_fixtures$superlig_awayds <- sqldf("SELECT superlig_awayds_lookup.superlig_awayds FROM superlig_awayds_lookup INNER JOIN SUPERLIG_fixtures ON superlig_awayds_lookup.AwayTeam_superlig = SUPERLIG_fixtures.AwayTeam_superlig")

SUPERLIG_fixtures$avg_AG_superlig <- superlig_avg_AG

superlig_awayas_lookup <- cbind(superlig_teams,superlig_away_as)

superlig_awayas_lookup <- as.data.frame(superlig_awayas_lookup)

colnames(superlig_awayas_lookup) <- c("AwayTeam_superlig","superlig_awayas")


SUPERLIG_fixtures$superlig_awayas <- sqldf("SELECT superlig_awayas_lookup.superlig_awayas FROM superlig_awayas_lookup INNER JOIN SUPERLIG_fixtures ON superlig_awayas_lookup.AwayTeam_superlig = SUPERLIG_fixtures.AwayTeam_superlig")

SUPERLIG_fixtures$superlig_homeds <- rep(superlig_home_ds,each = length(superlig_teams)-1)

SUPERLIG_fixtures$superlig_awayds <- as.numeric(unlist(SUPERLIG_fixtures$superlig_awayds))
#xGH
SUPERLIG_fixtures$superlig_xGH <- SUPERLIG_fixtures$avg_HG_superlig * SUPERLIG_fixtures$superlig_homeas * SUPERLIG_fixtures$superlig_awayds

#xGA

SUPERLIG_fixtures$superlig_awayas <- as.numeric(unlist(SUPERLIG_fixtures$superlig_awayas))

SUPERLIG_fixtures$superlig_xGA <- SUPERLIG_fixtures$avg_AG_superlig * SUPERLIG_fixtures$superlig_awayas * SUPERLIG_fixtures$superlig_homeds

SUPERLIG_fixtures$superlig_0_0 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_0 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_0_1 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_1 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_0 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_0_2 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_2 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_1 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_2 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_3 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_0 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_1 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_2 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_0_3 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_3 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_3 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_4 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_0 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_1 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_2 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_3 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_0_4 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_4 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_4 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_4 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_5 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_0 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_1 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_2 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_3 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_4 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_0_5 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_5 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_5 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_5 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_5 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_6 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_0 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(0,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_1 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(1,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_2 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(2,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_3 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(3,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_4 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(4,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_6_5 <- round(stats::dpois(6,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(5,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_0_6 <- round(stats::dpois(0,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_1_6 <- round(stats::dpois(1,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_2_6 <- round(stats::dpois(2,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_3_6 <- round(stats::dpois(3,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_4_6 <- round(stats::dpois(4,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
SUPERLIG_fixtures$superlig_5_6 <- round(stats::dpois(5,SUPERLIG_fixtures$superlig_xGH) * stats::dpois(6,SUPERLIG_fixtures$superlig_xGA), digits = 4)
#Home win
SUPERLIG_fixtures$superlig_H <- (
  SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
    SUPERLIG_fixtures$superlig_5_0 + SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5
)

SUPERLIG_fixtures$superlig_H <- percent(SUPERLIG_fixtures$superlig_H, accuracy = 0.1)

#Draw
SUPERLIG_fixtures$superlig_D <- (

  SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 + SUPERLIG_fixtures$superlig_4_4 +
    SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6
)

SUPERLIG_fixtures$superlig_D <- percent(SUPERLIG_fixtures$superlig_D, accuracy = 0.1)

#Away

SUPERLIG_fixtures$superlig_A <- (
  SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
    SUPERLIG_fixtures$superlig_0_5 + SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6
)

SUPERLIG_fixtures$superlig_A <- percent(SUPERLIG_fixtures$superlig_A, accuracy = 0.1)

#ov25
SUPERLIG_fixtures$superlig_ov25 <- (
  SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 + SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 + SUPERLIG_fixtures$superlig_0_4 +
    SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 + SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_0 +
    SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 + SUPERLIG_fixtures$superlig_0_5 +
    SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 + SUPERLIG_fixtures$superlig_5_5 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5 + SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 +
    SUPERLIG_fixtures$superlig_4_6 + SUPERLIG_fixtures$superlig_5_6 + SUPERLIG_fixtures$superlig_6_6
)
#un25
SUPERLIG_fixtures$superlig_un25 <- (
  SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_0_2
)
#odds
SUPERLIG_fixtures$superlig_ov25_odds <- round((1/SUPERLIG_fixtures$superlig_ov25),digits = 2)
SUPERLIG_fixtures$superlig_un25_odds <- round((1/SUPERLIG_fixtures$superlig_un25),digits = 2)

SUPERLIG_fixtures$superlig_ov25_odds
SUPERLIG_fixtures$superlig_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
SUPERLIG_fixtures$superlig_BTTSY <- (
  SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_3_1 + SUPERLIG_fixtures$superlig_3_2 +
    SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_1_3 + SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_3_3 + SUPERLIG_fixtures$superlig_4_4 +
    SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_3 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 +
    SUPERLIG_fixtures$superlig_3_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 +
    SUPERLIG_fixtures$superlig_5_4 + SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_6_6 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6
)
#BTTSN
SUPERLIG_fixtures$superlig_BTTSN <- (
  SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_0_2 +
    SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_5_0 +
    SUPERLIG_fixtures$superlig_0_5 + SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_0_6
)

SUPERLIG_fixtures$superlig_BTTSY_odds <- round((1/SUPERLIG_fixtures$superlig_BTTSY),digits = 2)
SUPERLIG_fixtures$superlig_BTTSN_odds <- round((1/SUPERLIG_fixtures$superlig_BTTSN),digits = 2)

SUPERLIG_fixtures$superlig_BTTSY <- percent(SUPERLIG_fixtures$superlig_BTTSY, accuracy = 0.1)
SUPERLIG_fixtures$superlig_BTTSN <- percent(SUPERLIG_fixtures$superlig_BTTSN, accuracy = 0.1)
#odds
SUPERLIG_fixtures$superlig_BTTSY_odds
SUPERLIG_fixtures$superlig_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
SUPERLIG_fixtures$superlig_AH_0_H <- (
  SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
    SUPERLIG_fixtures$superlig_5_0 +SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5 + SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6
)
#AH_0_A
SUPERLIG_fixtures$superlig_AH_0_A <- (
  SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
    SUPERLIG_fixtures$superlig_0_5 +SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6 + SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6
)

#odds
SUPERLIG_fixtures$superlig_AH_0_H_odds <- round((1/SUPERLIG_fixtures$superlig_AH_0_H),digits = 2)
SUPERLIG_fixtures$superlig_AH_0_A_odds <- round((1/SUPERLIG_fixtures$superlig_AH_0_A),digits = 2)

SUPERLIG_fixtures$superlig_AH_0_H_odds
SUPERLIG_fixtures$superlig_AH_0_A_odds
#percentages
SUPERLIG_fixtures$superlig_AH_0_H <- percent(SUPERLIG_fixtures$superlig_AH_0_H, accuracy = 0.1)
SUPERLIG_fixtures$superlig_AH_0_A <- percent(SUPERLIG_fixtures$superlig_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
SUPERLIG_fixtures$superlig_AH_n075_H <- (
  SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
    SUPERLIG_fixtures$superlig_5_0 +SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5
)
#AH_n075_A
SUPERLIG_fixtures$superlig_AH_n075_A <- (
  SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
    SUPERLIG_fixtures$superlig_0_5 +SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6
)

#odds
SUPERLIG_fixtures$superlig_AH_n075_H_odds <- round((1/SUPERLIG_fixtures$superlig_AH_n075_H),digits = 2)
SUPERLIG_fixtures$superlig_AH_n075_A_odds <- round((1/SUPERLIG_fixtures$superlig_AH_n075_A),digits = 2)

SUPERLIG_fixtures$superlig_AH_n075_H_odds
SUPERLIG_fixtures$superlig_AH_n075_A_odds
#percentages
SUPERLIG_fixtures$superlig_AH_n075_H <- percent(SUPERLIG_fixtures$superlig_AH_n075_H, accuracy = 0.1)
SUPERLIG_fixtures$superlig_AH_n075_A <- percent(SUPERLIG_fixtures$superlig_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
SUPERLIG_fixtures$superlig_AH_075_H <- (
  SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
    SUPERLIG_fixtures$superlig_5_0 +SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5 + SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6 + SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_1_2 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_3_4 + SUPERLIG_fixtures$superlig_4_5 + SUPERLIG_fixtures$superlig_5_6
)
#AH_075_A
SUPERLIG_fixtures$superlig_AH_075_A <- (
  SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
    SUPERLIG_fixtures$superlig_0_5 +SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6 + SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6 + SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_3 + SUPERLIG_fixtures$superlig_5_4 + SUPERLIG_fixtures$superlig_6_5
)

#odds
SUPERLIG_fixtures$superlig_AH_075_H_odds <- round((1/SUPERLIG_fixtures$superlig_AH_075_H),digits = 2)
SUPERLIG_fixtures$superlig_AH_075_A_odds <- round((1/SUPERLIG_fixtures$superlig_AH_075_A),digits = 2)

SUPERLIG_fixtures$superlig_AH_075_H_odds
SUPERLIG_fixtures$superlig_AH_075_A_odds
#percentages
SUPERLIG_fixtures$superlig_AH_075_H <- percent(SUPERLIG_fixtures$superlig_AH_075_H, accuracy = 0.1)
SUPERLIG_fixtures$superlig_AH_075_A <- percent(SUPERLIG_fixtures$superlig_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
SUPERLIG_fixtures$superlig_AH_n125_H <- (
  SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
    SUPERLIG_fixtures$superlig_5_0 +SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5
)
#AH_n125_A
SUPERLIG_fixtures$superlig_AH_n125_A <- (
  SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
    SUPERLIG_fixtures$superlig_0_5 +SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6
)

#odds
SUPERLIG_fixtures$superlig_AH_n125_H_odds <- round((1/SUPERLIG_fixtures$superlig_AH_n125_H),digits = 2)
SUPERLIG_fixtures$superlig_AH_n125_A_odds <- round((1/SUPERLIG_fixtures$superlig_AH_n125_A),digits = 2)

SUPERLIG_fixtures$superlig_AH_n125_H_odds
SUPERLIG_fixtures$superlig_AH_n125_A_odds
#percentages
SUPERLIG_fixtures$superlig_AH_n125_H <- percent(SUPERLIG_fixtures$superlig_AH_n125_H, accuracy = 0.1)
SUPERLIG_fixtures$superlig_AH_n125_A <- percent(SUPERLIG_fixtures$superlig_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
SUPERLIG_fixtures$superlig_AH_125_H <- (
  SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
    SUPERLIG_fixtures$superlig_5_0 +SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
    SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
    SUPERLIG_fixtures$superlig_6_5 + SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6 + SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_1_2 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_3_4 + SUPERLIG_fixtures$superlig_4_5 + SUPERLIG_fixtures$superlig_5_6
)
#AH_125_A
SUPERLIG_fixtures$superlig_AH_125_A <- (
  SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
    SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
    SUPERLIG_fixtures$superlig_0_5 +SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
    SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
    SUPERLIG_fixtures$superlig_5_6 + SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 +
    SUPERLIG_fixtures$superlig_4_4 + SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6 + SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_1 +
    SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_3 + SUPERLIG_fixtures$superlig_5_4 + SUPERLIG_fixtures$superlig_6_5
)

#odds
SUPERLIG_fixtures$superlig_AH_125_H_odds <- round((1/SUPERLIG_fixtures$superlig_AH_125_H),digits = 2)
SUPERLIG_fixtures$superlig_AH_125_A_odds <- round((1/SUPERLIG_fixtures$superlig_AH_125_A),digits = 2)

SUPERLIG_fixtures$superlig_AH_125_H_odds
SUPERLIG_fixtures$superlig_AH_125_A_odds
#percentages
SUPERLIG_fixtures$superlig_AH_125_H <- percent(SUPERLIG_fixtures$superlig_AH_125_H, accuracy = 0.1)
SUPERLIG_fixtures$superlig_AH_125_A <- percent(SUPERLIG_fixtures$superlig_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
SUPERLIG_fixtures$superlig_ov25 <- percent(SUPERLIG_fixtures$superlig_ov25, accuracy = 0.1)

SUPERLIG_fixtures$superlig_un25 <- percent(SUPERLIG_fixtures$superlig_un25, accuracy = 0.1)
SUPERLIG_fixtures$superlig_pscore <- paste(round(SUPERLIG_fixtures$superlig_xGH,digits = 0),round(SUPERLIG_fixtures$superlig_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
superlig_last_n_games <- 6

#create final_superlig_hf object
final_superlig_hf <- c()
for(index_superlig_hf in 1:length(superlig_teams))
{
  index_superlig_hf <- row.names(superlig_form_h) == superlig_teams[index_superlig_hf]
  form_superlig_hf <- superlig_form_h[index_superlig_hf]
  deleted_form_superlig_hf <- form_superlig_hf[!form_superlig_hf[] == ""]
  l6_form_superlig_hf <- tail(deleted_form_superlig_hf,superlig_last_n_games)
  l6_form_superlig_hf <- paste(l6_form_superlig_hf,collapse = " ")
  final_superlig_hf[index_superlig_hf] <- rbind(paste(superlig_teams[index_superlig_hf],l6_form_superlig_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}

#change column nam
final_superlig_hf <- as.data.frame(final_superlig_hf)
colnames(final_superlig_hf) <- "Form"
#goals scored
#create final_superlig_gs object
final_superlig_gs <- c()
suml6_superlig_gs <- c()
for(index_superlig_gs in 1:length(superlig_teams))
{
  index_superlig_gs <- row.names(superlig_goalscored_h) == superlig_teams[index_superlig_gs]
  form_superlig_gs <- superlig_goalscored_h[index_superlig_gs]
  deleted_form_superlig_gs <- form_superlig_gs[!form_superlig_gs[] == ""]
  l6_form_superlig_gs <- tail(deleted_form_superlig_gs,superlig_last_n_games)
  l6_form_superlig_gs <- as.numeric(l6_form_superlig_gs)
  suml6_superlig_gs[index_superlig_gs] <- sum(l6_form_superlig_gs)
  suml6_superlig_gs[index_superlig_gs] <- paste("(",suml6_superlig_gs[index_superlig_gs],")",sep = "")
  l6_form_superlig_gs <- paste(l6_form_superlig_gs,collapse = " ")
  final_superlig_gs[index_superlig_gs] <- rbind(paste(superlig_teams[index_superlig_gs],l6_form_superlig_gs,suml6_superlig_gs[index_superlig_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
final_superlig_gs
#change column names
final_superlig_gs <- as.data.frame(final_superlig_gs)
colnames(final_superlig_gs) <- "Goals scored"
#goal conceded
#create final_superlig_gc object
final_superlig_gc <- c()
suml6_superlig_gc <- c()
for(index_superlig_gc in 1:length(superlig_teams))
{
  index_superlig_gc <- row.names(superlig_goalconceded_h) == superlig_teams[index_superlig_gc]
  form_superlig_gc <- superlig_goalconceded_h[index_superlig_gc]
  deleted_form_superlig_gc <- form_superlig_gc[!form_superlig_gc[] == ""]
  l6_form_superlig_gc <- tail(deleted_form_superlig_gc,superlig_last_n_games)
  l6_form_superlig_gc <- as.numeric(l6_form_superlig_gc)
  suml6_superlig_gc[index_superlig_gc] <- sum(l6_form_superlig_gc)
  suml6_superlig_gc[index_superlig_gc] <- paste("(",suml6_superlig_gc[index_superlig_gc],")",sep = "")
  l6_form_superlig_gc <- paste(l6_form_superlig_gc,collapse = " ")
  final_superlig_gc[index_superlig_gc] <- rbind(paste(superlig_teams[index_superlig_gc],l6_form_superlig_gc,suml6_superlig_gc[index_superlig_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
#change column names
final_superlig_gc <- as.data.frame(final_superlig_gc)
colnames(final_superlig_gc) <- "Goals conceded"


toString(l6_form_superlig_gc)
#total goals
#create final_superlig_tg object
final_superlig_tg <- c()
suml6_superlig_tg <- c()
for(index_superlig_tg in 1:length(superlig_teams))
{
  index_superlig_tg <- row.names(superlig_totalgoals_h) == superlig_teams[index_superlig_tg]
  form_superlig_tg <- superlig_totalgoals_h[index_superlig_tg]
  deleted_form_superlig_tg <- form_superlig_tg[!form_superlig_tg[] == ""]
  l6_form_superlig_tg <- tail(deleted_form_superlig_tg,superlig_last_n_games)
  l6_form_superlig_tg <- as.numeric(l6_form_superlig_tg)
  suml6_superlig_tg[index_superlig_tg] <- sum(l6_form_superlig_tg)
  suml6_superlig_tg[index_superlig_tg] <- paste("(",suml6_superlig_tg[index_superlig_tg],")",sep = "")
  l6_form_superlig_tg <- paste(l6_form_superlig_tg,collapse = " ")
  final_superlig_tg[index_superlig_tg] <- rbind(paste(superlig_teams[index_superlig_tg],l6_form_superlig_tg,suml6_superlig_tg[index_superlig_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
#change column names
final_superlig_tg <- as.data.frame(final_superlig_tg)
colnames(final_superlig_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_superlig_hf object
final_superlig_cs <- c()
for(index_superlig_cs in 1:length(superlig_teams))
{
  index_superlig_cs <- row.names(superlig_csform_h) == superlig_teams[index_superlig_cs]
  csform_superlig_cs <- superlig_csform_h[index_superlig_cs]
  deleted_csform_superlig_cs <- csform_superlig_cs[!csform_superlig_cs[] == ""]
  l6_csform_superlig_cs <- tail(deleted_csform_superlig_cs,superlig_last_n_games)
  l6_csform_superlig_cs <- paste(l6_csform_superlig_cs,collapse = " ")
  final_superlig_cs[index_superlig_cs] <- rbind(paste(superlig_teams[index_superlig_cs],l6_csform_superlig_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",superlig_teams[index],l6_csform)

}

#change column names
final_superlig_cs <- as.data.frame(final_superlig_cs)
colnames(final_superlig_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_superlig_wm object
final_superlig_wm <- c()
suml6_superlig_wm <- c()
for(index_superlig_wm in 1:length(superlig_teams))
{
  index_superlig_wm <- row.names(superlig_winmargin_h) == superlig_teams[index_superlig_wm]
  form_superlig_wm <- superlig_winmargin_h[index_superlig_wm]
  deleted_form_superlig_wm <- form_superlig_wm[!form_superlig_wm[] == ""]
  l6_form_superlig_wm <- tail(deleted_form_superlig_wm,superlig_last_n_games)
  l6_form_superlig_wm <- as.numeric(l6_form_superlig_wm)
  suml6_superlig_wm[index_superlig_wm] <- sum(l6_form_superlig_wm)
  suml6_superlig_wm[index_superlig_wm] <- paste("(",suml6_superlig_wm[index_superlig_wm],")",sep = "")
  l6_form_superlig_wm <- paste(l6_form_superlig_wm,collapse = " ")
  final_superlig_wm[index_superlig_wm] <- rbind(paste(superlig_teams[index_superlig_wm],l6_form_superlig_wm,suml6_superlig_wm[index_superlig_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
final_superlig_wm
#change column names
final_superlig_wm <- as.data.frame(final_superlig_wm)
colnames(final_superlig_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_superlig_ca object
final_superlig_ca <- c()
suml6_superlig_ca <- c()
for(index_superlig_ca in 1:length(superlig_teams))
{
  index_superlig_ca <- row.names(superlig_coawarded_h) == superlig_teams[index_superlig_ca]
  form_superlig_ca <- superlig_coawarded_h[index_superlig_ca]
  deleted_form_superlig_ca <- form_superlig_ca[!form_superlig_ca[] == ""]
  l6_form_superlig_ca <- tail(deleted_form_superlig_ca,superlig_last_n_games)
  l6_form_superlig_ca <- as.numeric(l6_form_superlig_ca)
  suml6_superlig_ca[index_superlig_ca] <- sum(l6_form_superlig_ca)
  suml6_superlig_ca[index_superlig_ca] <- paste("(",suml6_superlig_ca[index_superlig_ca],")",sep = "")
  l6_form_superlig_ca <- paste(l6_form_superlig_ca,collapse = " ")
  final_superlig_ca[index_superlig_ca] <- rbind(paste(superlig_teams[index_superlig_ca],l6_form_superlig_ca,suml6_superlig_ca[index_superlig_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
final_superlig_ca
#change column names
final_superlig_ca <- as.data.frame(final_superlig_ca)
colnames(final_superlig_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_superlig_ca object
final_superlig_cc <- c()
suml6_superlig_cc <- c()
for(index_superlig_cc in 1:length(superlig_teams))
{
  index_superlig_cc <- row.names(superlig_cornersconceded_h) == superlig_teams[index_superlig_cc]
  form_superlig_cc <- superlig_cornersconceded_h[index_superlig_cc]
  deleted_form_superlig_cc <- form_superlig_cc[!form_superlig_cc[] == ""]
  l6_form_superlig_cc <- tail(deleted_form_superlig_cc,superlig_last_n_games)
  l6_form_superlig_cc <- as.numeric(l6_form_superlig_cc)
  suml6_superlig_cc[index_superlig_cc] <- sum(l6_form_superlig_cc)
  suml6_superlig_cc[index_superlig_cc] <- paste("(",suml6_superlig_cc[index_superlig_cc],")",sep = "")
  l6_form_superlig_cc <- paste(l6_form_superlig_cc,collapse = " ")
  final_superlig_cc[index_superlig_cc] <- rbind(paste(superlig_teams[index_superlig_cc],l6_form_superlig_cc,suml6_superlig_cc[index_superlig_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
final_superlig_cc
#change column names
final_superlig_cc <- as.data.frame(final_superlig_cc)
colnames(final_superlig_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_superlig_cosc <- c()
for(index_superlig_cosc in 1:length(superlig_teams))
{
  index_superlig_cosc <- row.names(superlig_coscform_h) == superlig_teams[index_superlig_cosc]
  coscform_superlig_cosc <- superlig_coscform_h[index_superlig_cosc]
  deleted_coscform_superlig_cosc <- coscform_superlig_cosc[!coscform_superlig_cosc[] == ""]
  l6_coscform_superlig_cosc <- tail(deleted_coscform_superlig_cosc,superlig_last_n_games)
  l6_coscform_superlig_cosc <- paste(l6_coscform_superlig_cosc,collapse = " ")
  final_superlig_cosc[index_superlig_cosc] <- rbind(paste(superlig_teams[index_superlig_cosc],l6_coscform_superlig_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",superlig_teams[index],l6_coscform)

}
final_superlig_cosc
#change column names
final_superlig_cosc <- as.data.frame(final_superlig_cosc)
colnames(final_superlig_cosc) <- "CornersForm"
##################################################
#total corners
#create final_superlig_tcorners object
final_superlig_tcorners <- c()
suml6_superlig_tcorners <- c()
for(index_superlig_tcorners in 1:length(superlig_teams))
{
  index_superlig_tcorners <- row.names(superlig_totalcorners_h) == superlig_teams[index_superlig_tcorners]
  form_superlig_tcorners <- superlig_totalcorners_h[index_superlig_tcorners]
  deleted_form_superlig_tcorners <- form_superlig_tcorners[!form_superlig_tcorners[] == ""]
  l6_form_superlig_tcorners <- tail(deleted_form_superlig_tcorners,superlig_last_n_games)
  l6_form_superlig_tcorners <- as.numeric(l6_form_superlig_tcorners)
  suml6_superlig_tcorners[index_superlig_tcorners] <- sum(l6_form_superlig_tcorners)
  suml6_superlig_tcorners[index_superlig_tcorners] <- paste("(",suml6_superlig_tcorners[index_superlig_tcorners],")",sep = "")
  l6_form_superlig_tcorners <- paste(l6_form_superlig_tcorners,collapse = " ")
  final_superlig_tcorners[index_superlig_tcorners] <- rbind(paste(superlig_teams[index_superlig_tcorners],l6_form_superlig_tcorners,suml6_superlig_tcorners[index_superlig_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
#change column names
final_superlig_tcorners <- as.data.frame(final_superlig_tcorners)
colnames(final_superlig_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_superlig_hf_against
final_superlig_hf_against <- c()
for(index_superlig_hf_against in 1:length(superlig_teams))
{
  index_superlig_hf_against <- row.names(superlig_form_team_against_h) == superlig_teams[index_superlig_hf_against]
  form_superlig_hf_against <- superlig_form_team_against_h[index_superlig_hf_against]
  deleted_form_superlig_hf_against <- form_superlig_hf_against[!form_superlig_hf_against[] == ""]
  l6_form_superlig_hf_against <- tail(deleted_form_superlig_hf_against,superlig_last_n_games)
  l6_form_superlig_hf_against <- paste(l6_form_superlig_hf_against,collapse = " ")
  final_superlig_hf_against[index_superlig_hf_against] <- rbind(paste(superlig_teams[index_superlig_hf_against],l6_form_superlig_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",superlig_teams[index],l6_form)

}
final_superlig_hf_against <- as.data.frame(final_superlig_hf_against)
colnames(final_superlig_hf_against) <- "Team against"
#combine the columns
final_superlig_all <- cbind(final_superlig_hf,final_superlig_gs,final_superlig_gc,final_superlig_tg,final_superlig_ca,final_superlig_cc,final_superlig_tcorners,final_superlig_cosc,final_superlig_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#SUPERLIG
SUPERLIG_sim <- SUPERLIG
SUPERLIG_sim$matchid <- paste(SUPERLIG_sim$HomeTeam,SUPERLIG_sim$AwayTeam,sep = "-")
SUPERLIG_fixtures$matchid <- paste(SUPERLIG_fixtures$HomeTeam_superlig,SUPERLIG_fixtures$AwayTeam_superlig,sep = "-")
SUPERLIG_fixtures$superlig_FTR <- sapply(SUPERLIG_fixtures$superlig_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

SUPERLIG_fixtures$superlig_gamestatus <- ifelse(SUPERLIG_fixtures$matchid %in% SUPERLIG_sim$matchid,"played","notplayed")

superlig_home_wins_sim <- c()
superlig_away_wins_sim <- c()
superlig_home_draws_sim <- c()
superlig_away_draws_sim <- c()
superlig_home_loss_sim <- c()
superlig_away_loss_sim <- c()



for (i_superlig_wins_sim in 1:length(superlig_teams))
{

  superlig_home_wins_sim[i_superlig_wins_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$HomeTeam_superlig == superlig_teams[i_superlig_wins_sim] & SUPERLIG_fixtures$superlig_FTR == "H" & SUPERLIG_fixtures$superlig_gamestatus =="notplayed",])
  superlig_away_wins_sim[i_superlig_wins_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$AwayTeam_superlig == superlig_teams[i_superlig_wins_sim] & SUPERLIG_fixtures$superlig_FTR == "A" & SUPERLIG_fixtures$superlig_gamestatus == "notplayed",])
  superlig_home_draws_sim[i_superlig_wins_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$HomeTeam_superlig == superlig_teams[i_superlig_wins_sim] & SUPERLIG_fixtures$superlig_FTR == "D" & SUPERLIG_fixtures$superlig_gamestatus == "notplayed",])
  superlig_away_draws_sim[i_superlig_wins_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$AwayTeam_superlig == superlig_teams[i_superlig_wins_sim] & SUPERLIG_fixtures$superlig_FTR == "D" & SUPERLIG_fixtures$superlig_gamestatus == "notplayed",])
  superlig_home_loss_sim[i_superlig_wins_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$HomeTeam_superlig == superlig_teams[i_superlig_wins_sim] & SUPERLIG_fixtures$superlig_FTR == "A" & SUPERLIG_fixtures$superlig_gamestatus == "notplayed",])
  superlig_away_loss_sim[i_superlig_wins_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$AwayTeam_superlig == superlig_teams[i_superlig_wins_sim] & SUPERLIG_fixtures$superlig_FTR == "H" & SUPERLIG_fixtures$superlig_gamestatus == "notplayed", ])

}

superlig_total_wins_sim <- superlig_home_wins_sim + superlig_away_wins_sim
superlig_total_draws_sim <- superlig_home_draws_sim + superlig_away_draws_sim
superlig_total_loss_sim <- superlig_home_loss_sim + superlig_away_loss_sim

superlig_home_games_sim <- c()
superlig_away_games_sim <-c()

for (i_superlig_sim in 1:length(superlig_teams))
{

  superlig_home_games_sim[i_superlig_sim] <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$HomeTeam_superlig == superlig_teams[i_superlig_sim] & SUPERLIG_fixtures$superlig_gamestatus == "notplayed",])
  superlig_away_games_sim[i_superlig_sim]  <- nrow(SUPERLIG_fixtures[SUPERLIG_fixtures$AwayTeam_superlig == superlig_teams[i_superlig_sim] & SUPERLIG_fixtures$superlig_gamestatus == "notplayed",])

}

superlig_games_played_sim <- superlig_home_games_sim + superlig_away_games_sim

superlig_league_table_sim <- cbind(superlig_teams,superlig_games_played_sim,superlig_total_wins_sim,superlig_total_draws_sim,superlig_total_loss_sim)
superlig_PTS_sim <- (superlig_total_wins_sim*3) + (superlig_total_draws_sim*1)
superlig_league_table_sim <- cbind(superlig_league_table_sim,superlig_PTS_sim)

superlig_games_played_simfinal <- superlig_games_played + superlig_games_played_sim
superlig_total_wins_simfinal <- superlig_total_wins + superlig_total_wins_sim
superlig_total_draws_simfinal <- superlig_total_draws + superlig_total_draws_sim
superlig_total_loss_simfinal <- superlig_total_loss + superlig_total_loss_sim
superlig_PTS_simfinal <- superlig_PTS + superlig_PTS_sim

superlig_league_table_simfinal <- cbind(superlig_teams,superlig_games_played_simfinal,superlig_total_wins_simfinal,superlig_total_draws_simfinal,superlig_total_loss_simfinal,superlig_PTS_simfinal)
superlig_league_table_simfinal <- as.data.frame(superlig_league_table_simfinal)
names(superlig_league_table_simfinal)[names(superlig_league_table_simfinal) == "superlig_teams"] <- "Team_f"
names(superlig_league_table_simfinal)[names(superlig_league_table_simfinal) == "superlig_games_played_simfinal"] <- "P_f"
names(superlig_league_table_simfinal)[names(superlig_league_table_simfinal) == "superlig_total_wins_simfinal"] <- "W_f"
names(superlig_league_table_simfinal)[names(superlig_league_table_simfinal) == "superlig_total_draws_simfinal"] <- "D_f"
names(superlig_league_table_simfinal)[names(superlig_league_table_simfinal) == "superlig_total_loss_simfinal"] <- "L_f"
names(superlig_league_table_simfinal)[names(superlig_league_table_simfinal) == "superlig_PTS_simfinal"] <- "PTS_f"
points_superlig_sim <-  superlig_league_table_simfinal[order(as.numeric(superlig_league_table_simfinal$PTS_f), decreasing = TRUE),]

SUPERLIG_notplayed <- SUPERLIG_fixtures[SUPERLIG_fixtures$superlig_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#SUPERLIG
SUPERLIG_fixtures$Hometeam_superlig_index <- match(SUPERLIG_fixtures$HomeTeam_superlig,superlig_teams)
SUPERLIG_fixtures$Awayteam_superlig_index <- match(SUPERLIG_fixtures$AwayTeam_superlig,superlig_teams)
superlig_prediction <- c()
superlig_HWM <- c()
superlig_AWM <- c()
superlig_HWMLM <- c()
superlig_AWMLM <- c()
superlig_HY <- c()
superlig_AY <- c()
superlig_HCO <- c()
superlig_ACO <- c()
superlig_HXSC <- c()
superlig_AXSC <- c()
superlig_HYCPF <- c()
superlig_AYCPF <- c()
for(superlig_row in 1:nrow(SUPERLIG_fixtures))
{

  superlig_hometeamindex <- SUPERLIG_fixtures[superlig_row,"Hometeam_superlig_index"]
  superlig_awayteamindex <- SUPERLIG_fixtures[superlig_row,"Awayteam_superlig_index"]
  #analyse team form
  #home team
  superlig_form_vec_ht <- as.vector(superlig_form_h[superlig_hometeamindex,])
  superlig_form_vec_ht[is.na(superlig_form_vec_ht)] <- ""
  superlig_form_vec_ht <- superlig_form_vec_ht[superlig_form_vec_ht != ""]
  superlig_form_vec_ht  <-tail(superlig_form_vec_ht,6)
  superlig_ht_numberof_wins <- length(which(superlig_form_vec_ht == "W"))
  superlig_ht_numberof_draws <- length(which(superlig_form_vec_ht == "D"))
  superlig_ht_numberof_loss <- length(which(superlig_form_vec_ht == "L"))
  #awayteam
  superlig_form_vec_at <- as.vector(superlig_form_h[superlig_awayteamindex,])
  superlig_form_vec_at[is.na(superlig_form_vec_at)] <- ""
  superlig_form_vec_at <- superlig_form_vec_at[superlig_form_vec_at != ""]
  superlig_form_vec_at  <-tail(superlig_form_vec_at,6)
  superlig_at_numberof_wins <- length(which(superlig_form_vec_at == "W"))
  superlig_at_numberof_draws <- length(which(superlig_form_vec_at == "D"))
  superlig_at_numberof_loss <- length(which(superlig_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  superlig_goalscored_vec_ht <- as.vector(superlig_goalscored_h[superlig_hometeamindex,])
  superlig_goalscored_vec_ht[is.na(superlig_goalscored_vec_ht)] <- ""
  superlig_goalscored_vec_ht <- superlig_goalscored_vec_ht[superlig_goalscored_vec_ht != ""]
  superlig_goalscored_vec_ht  <-tail(superlig_goalscored_vec_ht,6)
  superlig_goalscored_vec_ht  <- as.numeric(superlig_goalscored_vec_ht)
  superlig_ht_totalgoalscored <- sum(superlig_goalscored_vec_ht)
  superlig_ht_matches_scoring <- length(which(superlig_goalscored_vec_ht > 0))
  superlig_ht_matches_without_scoring <- length(which(superlig_goalscored_vec_ht == "0"))
  #awayteam
  superlig_goalscored_vec_at <- as.vector(superlig_goalscored_h[superlig_awayteamindex,])
  superlig_goalscored_vec_at[is.na(superlig_goalscored_vec_at)] <- ""
  superlig_goalscored_vec_at <- superlig_goalscored_vec_at[superlig_goalscored_vec_at != ""]
  superlig_goalscored_vec_at  <-tail(superlig_goalscored_vec_at,6)
  superlig_goalscored_vec_at  <- as.numeric(superlig_goalscored_vec_at)
  superlig_at_totalgoalscored <- sum(superlig_goalscored_vec_at)
  superlig_at_matches_scoring <- length(which(superlig_goalscored_vec_at > 0))
  superlig_at_matches_without_scoring <- length(which(superlig_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  superlig_goalconceded_vec_ht <- as.vector(superlig_goalconceded_h[superlig_hometeamindex,])
  superlig_goalconceded_vec_ht[is.na(superlig_goalconceded_vec_ht)] <- ""
  superlig_goalconceded_vec_ht <- superlig_goalconceded_vec_ht[superlig_goalconceded_vec_ht != ""]
  superlig_goalconceded_vec_ht  <-tail(superlig_goalconceded_vec_ht,6)
  superlig_goalconceded_vec_ht  <- as.numeric(superlig_goalconceded_vec_ht)
  superlig_goalconceded_vec_ht
  superlig_ht_totalgoalconceded <- sum(superlig_goalconceded_vec_ht)
  superlig_ht_matches_concede <- length(which(superlig_goalconceded_vec_ht > 0))
  superlig_ht_matches_without_concede <- length(which(superlig_goalconceded_vec_ht == "0"))
  #awayteam
  superlig_goalconceded_vec_at <- as.vector(superlig_goalconceded_h[superlig_awayteamindex,])
  superlig_goalconceded_vec_at[is.na(superlig_goalconceded_vec_at)] <- ""
  superlig_goalconceded_vec_at <- superlig_goalconceded_vec_at[superlig_goalconceded_vec_at != ""]
  superlig_goalconceded_vec_at  <-tail(superlig_goalconceded_vec_at,6)
  superlig_goalconceded_vec_at  <- as.numeric(superlig_goalconceded_vec_at)
  superlig_at_totalgoalconceded <- sum(superlig_goalconceded_vec_at)
  superlig_at_matches_concede <- length(which(superlig_goalconceded_vec_at > 0))
  superlig_at_matches_without_concede <- length(which(superlig_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  superlig_totalgoals_vec_ht <- as.vector(superlig_totalgoals_h[superlig_hometeamindex,])
  superlig_totalgoals_vec_ht[is.na(superlig_totalgoals_vec_ht)] <- ""
  superlig_totalgoals_vec_ht <- superlig_totalgoals_vec_ht[superlig_totalgoals_vec_ht != ""]
  superlig_totalgoals_vec_ht  <-tail(superlig_totalgoals_vec_ht,6)
  superlig_totalgoals_vec_ht  <- as.numeric(superlig_totalgoals_vec_ht)
  superlig_totalgoals_vec_ht
  superlig_ht_totalgoals <- sum(superlig_totalgoals_vec_ht)
  superlig_ht_avgtotalgoals <- (superlig_ht_totalgoals/6)
  superlig_ht_no_of_ov25 <- length(which(superlig_totalgoals_vec_ht >= 3))
  superlig_ht_no_of_un25 <- length(which(superlig_totalgoals_vec_ht <= 2))
  #awayteam
  superlig_totalgoals_vec_at <- as.vector(superlig_totalgoals_h[superlig_awayteamindex,])
  superlig_totalgoals_vec_at[is.na(superlig_totalgoals_vec_at)] <- ""
  superlig_totalgoals_vec_at <- superlig_totalgoals_vec_at[superlig_totalgoals_vec_at != ""]
  superlig_totalgoals_vec_at  <-tail(superlig_totalgoals_vec_at,6)
  superlig_totalgoals_vec_at  <- as.numeric(superlig_totalgoals_vec_at)
  superlig_totalgoals_vec_at
  superlig_at_totalgoals <- sum(superlig_totalgoals_vec_at)
  superlig_at_avgtotalgoals <- (superlig_at_totalgoals/6)
  superlig_at_no_of_ov25 <- length(which(superlig_totalgoals_vec_at >= 3))
  superlig_at_no_of_un25 <- length(which(superlig_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  superlig_winmargin_vec_ht <- as.vector(superlig_winmargin_h[superlig_hometeamindex,])
  superlig_winmargin_vec_ht[is.na(superlig_winmargin_vec_ht)] <- ""
  superlig_winmargin_vec_ht <- superlig_winmargin_vec_ht[superlig_winmargin_vec_ht != ""]
  superlig_winmargin_vec_ht  <-tail(superlig_winmargin_vec_ht,6)
  superlig_winmargin_vec_ht  <- as.numeric(superlig_winmargin_vec_ht)

  superlig_ht_totalwinmargin <- sum(superlig_winmargin_vec_ht)
  superlig_ht_no_of_winmargin_ov0 <- length(which(superlig_winmargin_vec_ht >= 0))
  superlig_ht_no_of_winmargin_ov1 <- length(which(superlig_winmargin_vec_ht >= 1))
  superlig_ht_no_of_winmargin_un0 <- length(which(superlig_winmargin_vec_ht <= 0))
  superlig_ht_no_of_winmargin_un1 <- length(which(superlig_winmargin_vec_ht <= 1))
  #awayteam
  superlig_winmargin_vec_at <- as.vector(superlig_winmargin_h[superlig_awayteamindex,])
  superlig_winmargin_vec_at[is.na(superlig_winmargin_vec_at)] <- ""
  superlig_winmargin_vec_at <- superlig_winmargin_vec_at[superlig_winmargin_vec_at != ""]
  superlig_winmargin_vec_at  <-tail(superlig_winmargin_vec_at,6)
  superlig_winmargin_vec_at  <- as.numeric(superlig_winmargin_vec_at)

  superlig_at_totalwinmargin <- sum(superlig_winmargin_vec_at)
  superlig_at_no_of_winmargin_ov0 <- length(which(superlig_winmargin_vec_at >= 0))
  superlig_at_no_of_winmargin_ov1 <- length(which(superlig_winmargin_vec_at >= 1))
  superlig_at_no_of_winmargin_un0 <- length(which(superlig_winmargin_vec_at <= 0))
  superlig_at_no_of_winmargin_un1 <- length(which(superlig_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  superlig_winmargin_vec_ht_lm <- as.vector(superlig_winmargin_h[superlig_hometeamindex,])
  superlig_winmargin_vec_ht_lm[is.na(superlig_winmargin_vec_ht_lm)] <- ""
  superlig_winmargin_vec_ht_lm <- superlig_winmargin_vec_ht_lm[superlig_winmargin_vec_ht_lm != ""]
  superlig_winmargin_vec_ht_lm  <-tail(superlig_winmargin_vec_ht_lm,1)
  #awayteam
  superlig_winmargin_vec_at_lm <- as.vector(superlig_winmargin_h[superlig_awayteamindex,])
  superlig_winmargin_vec_at_lm[is.na(superlig_winmargin_vec_at_lm)] <- ""
  superlig_winmargin_vec_at_lm <- superlig_winmargin_vec_at_lm[superlig_winmargin_vec_at_lm != ""]
  superlig_winmargin_vec_at_lm  <-tail(superlig_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  superlig_yellowtotals_vec_ht <- as.vector(superlig_yellowtotalsv2[superlig_hometeamindex,])
  superlig_yellowtotals_vec_ht[is.na(superlig_yellowtotals_vec_ht)] <- ""
  superlig_yellowtotals_vec_ht <- superlig_yellowtotals_vec_ht[superlig_yellowtotals_vec_ht != ""]
  superlig_yellowtotals_vec_ht  <-tail(superlig_yellowtotals_vec_ht,1)
  #awayteam
  superlig_yellowtotals_vec_at <- as.vector(superlig_yellowtotalsv2[superlig_awayteamindex,])
  superlig_yellowtotals_vec_at[is.na(superlig_yellowtotals_vec_at)] <- ""
  superlig_yellowtotals_vec_at <- superlig_yellowtotals_vec_at[superlig_yellowtotals_vec_at != ""]
  superlig_yellowtotals_vec_at  <-tail(superlig_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  superlig_cornertotals_vec_ht <- as.vector(superlig_cornertotalsv2[superlig_hometeamindex,])
  superlig_cornertotals_vec_ht[is.na(superlig_cornertotals_vec_ht)] <- ""
  superlig_cornertotals_vec_ht <- superlig_cornertotals_vec_ht[superlig_cornertotals_vec_ht != ""]
  superlig_cornertotals_vec_ht  <-tail(superlig_cornertotals_vec_ht,1)
  #awayteam
  superlig_cornertotals_vec_at <- as.vector(superlig_cornertotalsv2[superlig_awayteamindex,])
  superlig_cornertotals_vec_at[is.na(superlig_cornertotals_vec_at)] <- ""
  superlig_cornertotals_vec_at <- superlig_cornertotals_vec_at[superlig_cornertotals_vec_at != ""]
  superlig_cornertotals_vec_at  <-tail(superlig_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  superlig_xshotsconversion_vec_ht <- as.vector(superlig_shots_analysis[superlig_hometeamindex,])
  superlig_xshotsconversion_vec_ht[is.na(superlig_xshotsconversion_vec_ht)] <- ""
  superlig_xshotsconversion_vec_ht <- superlig_xshotsconversion_vec_ht[superlig_xshotsconversion_vec_ht != ""]
  superlig_xshotsconversion_vec_ht  <-tail(superlig_xshotsconversion_vec_ht,1)
  #awayteam
  superlig_xshotsconversion_vec_at <- as.vector(superlig_shots_analysis[superlig_awayteamindex,])
  superlig_xshotsconversion_vec_at[is.na(superlig_xshotsconversion_vec_at)] <- ""
  superlig_xshotsconversion_vec_at <- superlig_xshotsconversion_vec_at[superlig_xshotsconversion_vec_at != ""]
  superlig_xshotsconversion_vec_at  <-tail(superlig_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  superlig_fouls_conversion_vec_ht <- as.vector(superlig_fouls_conversion[superlig_hometeamindex,])
  superlig_fouls_conversion_vec_ht[is.na(superlig_fouls_conversion_vec_ht)] <- ""
  superlig_fouls_conversion_vec_ht <- superlig_fouls_conversion_vec_ht[superlig_fouls_conversion_vec_ht != ""]
  superlig_fouls_conversion_vec_ht  <-tail(superlig_fouls_conversion_vec_ht,1)
  #awayteam
  superlig_fouls_conversion_vec_at <- as.vector(superlig_fouls_conversion[superlig_awayteamindex,])
  superlig_fouls_conversion_vec_at[is.na(superlig_fouls_conversion_vec_at)] <- ""
  superlig_fouls_conversion_vec_at <- superlig_fouls_conversion_vec_at[superlig_fouls_conversion_vec_at != ""]
  superlig_fouls_conversion_vec_at  <-tail(superlig_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  superlig_ht_last6points <- superlig_ht_numberof_wins*3 + superlig_ht_numberof_draws*1
  superlig_at_last6points <- superlig_at_numberof_wins*3 + superlig_at_numberof_draws*1

  if(superlig_ht_last6points > superlig_at_last6points) {superlig_3waypick <- "1"}  else {superlig_3waypick <- "X2"}

  if(superlig_at_last6points > superlig_ht_last6points ) {superlig_3waypick <- "2"} else {superlig_3waypick <- "1X"}

  if(superlig_ht_no_of_ov25 + superlig_at_no_of_ov25 >= 6) {superlig_goalspick <- "ov25"} else {superlig_goalspick <- "un25"}

  if(superlig_ht_no_of_un25 + superlig_at_no_of_un25 >= 6) {superlig_goalspick <- "un25"} else {superlig_goalspick <- "ov25"}

  if(superlig_ht_matches_scoring >= 4 && superlig_at_matches_scoring >=4) {superlig_btts <- "BTTS-Y"} else {superlig_btts <- "BTTS-N"}


  superlig_prediction[superlig_row] <- rbind(paste(superlig_3waypick,superlig_goalspick,superlig_btts,sep = ","))
  superlig_HWM[superlig_row] <- superlig_ht_totalwinmargin
  superlig_AWM[superlig_row] <- superlig_at_totalwinmargin

  superlig_HWMLM[superlig_row] <- superlig_winmargin_vec_ht_lm
  superlig_AWMLM[superlig_row] <- superlig_winmargin_vec_at_lm

  superlig_HY[superlig_row] <- superlig_yellowtotals_vec_ht
  superlig_AY[superlig_row] <- superlig_yellowtotals_vec_at

  superlig_HCO[superlig_row] <- superlig_cornertotals_vec_ht
  superlig_ACO[superlig_row] <- superlig_cornertotals_vec_at

  superlig_HXSC[superlig_row] <- superlig_xshotsconversion_vec_ht
  superlig_AXSC[superlig_row] <- superlig_xshotsconversion_vec_at

  superlig_HYCPF[superlig_row] <- superlig_fouls_conversion_vec_ht
  superlig_AYCPF[superlig_row] <- superlig_fouls_conversion_vec_at
}

superlig_prediction <- as.data.frame(superlig_prediction)
colnames(superlig_prediction) <- "prediction"

superlig_HWM <- as.data.frame(superlig_HWM)
colnames(superlig_HWM) <- "HWM"

superlig_AWM <- as.data.frame(superlig_AWM)
colnames(superlig_AWM) <- "AWM"

superlig_HWMLM <- as.data.frame(superlig_HWMLM)
colnames(superlig_HWMLM) <- "HWMLM"

superlig_AWMLM <- as.data.frame(superlig_AWMLM)
colnames(superlig_AWMLM) <- "AWMLM"

superlig_HY <- as.data.frame(superlig_HY)
colnames(superlig_HY) <- "AVGHY"

superlig_AY <- as.data.frame(superlig_AY)
colnames(superlig_AY) <- "AVGAY"

superlig_HCO <- as.data.frame(superlig_HCO)
colnames(superlig_HCO) <- "AVGHCO"

superlig_ACO <- as.data.frame(superlig_ACO)
colnames(superlig_ACO) <- "AVGACO"

superlig_HXSC <- as.data.frame(superlig_HXSC)
colnames(superlig_HXSC) <- "HXSC"

superlig_AXSC <- as.data.frame(superlig_AXSC)
colnames(superlig_AXSC) <- "AXSC"

superlig_HYCPF <- as.data.frame(superlig_HYCPF)
colnames(superlig_HYCPF) <- "HYCPF"

superlig_AYCPF <- as.data.frame(superlig_AYCPF)
colnames(superlig_AYCPF) <- "AYCPF"

superlig_picks <- cbind(SUPERLIG_fixtures$Div,SUPERLIG_fixtures$HomeTeam_superlig,SUPERLIG_fixtures$AwayTeam_superlig,superlig_prediction,superlig_HWM,superlig_AWM,superlig_HWMLM,superlig_AWMLM,superlig_HY,superlig_AY,superlig_HCO,superlig_ACO,superlig_HXSC,superlig_AXSC,superlig_HYCPF,superlig_AYCPF)

colnames(superlig_picks)[1] <- "picks_Div"
colnames(superlig_picks)[2] <- "picks_HomeTeam"
colnames(superlig_picks)[3] <- "picks_AwayTeam"
superlig_picks$matchid <- paste(superlig_picks$picks_HomeTeam,superlig_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of SUPERLIG
superlig_picks
#############################################################################################################################################################################
#clone fixtures
SUPERLIG_fixtures_clone <- SUPERLIG_fixtures
colnames(SUPERLIG_fixtures_clone)[61] <- "Hwin"
colnames(SUPERLIG_fixtures_clone)[62] <- "Draw"
colnames(SUPERLIG_fixtures_clone)[63] <- "Awin"

SUPERLIG_fixtures_clone$Hwinodds <-   SUPERLIG_fixtures$superlig_1_0 + SUPERLIG_fixtures$superlig_2_0 + SUPERLIG_fixtures$superlig_2_1 + SUPERLIG_fixtures$superlig_3_0 + SUPERLIG_fixtures$superlig_3_1 +
  SUPERLIG_fixtures$superlig_3_2 + SUPERLIG_fixtures$superlig_4_0 + SUPERLIG_fixtures$superlig_4_1 + SUPERLIG_fixtures$superlig_4_2 + SUPERLIG_fixtures$superlig_4_3 +
  SUPERLIG_fixtures$superlig_5_0 + SUPERLIG_fixtures$superlig_5_1 + SUPERLIG_fixtures$superlig_5_2 + SUPERLIG_fixtures$superlig_5_3 + SUPERLIG_fixtures$superlig_5_4 +
  SUPERLIG_fixtures$superlig_6_0 + SUPERLIG_fixtures$superlig_6_1 + SUPERLIG_fixtures$superlig_6_2 + SUPERLIG_fixtures$superlig_6_3 + SUPERLIG_fixtures$superlig_6_4 +
  SUPERLIG_fixtures$superlig_6_5
SUPERLIG_fixtures_clone$Hwinodds <- round(1/SUPERLIG_fixtures_clone$Hwinodds, digits = 3)

SUPERLIG_fixtures_clone$Drawodds <-  SUPERLIG_fixtures$superlig_0_0 + SUPERLIG_fixtures$superlig_1_1 + SUPERLIG_fixtures$superlig_2_2 + SUPERLIG_fixtures$superlig_3_3 + SUPERLIG_fixtures$superlig_4_4 +
  SUPERLIG_fixtures$superlig_5_5 + SUPERLIG_fixtures$superlig_6_6

SUPERLIG_fixtures_clone$Drawodds <- round(1/SUPERLIG_fixtures_clone$Drawodds, digits = 3)

SUPERLIG_fixtures_clone$Awinodds <-   SUPERLIG_fixtures$superlig_0_1 + SUPERLIG_fixtures$superlig_0_2 + SUPERLIG_fixtures$superlig_1_2 + SUPERLIG_fixtures$superlig_0_3 + SUPERLIG_fixtures$superlig_1_3 +
  SUPERLIG_fixtures$superlig_2_3 + SUPERLIG_fixtures$superlig_0_4 + SUPERLIG_fixtures$superlig_1_4 + SUPERLIG_fixtures$superlig_2_4 + SUPERLIG_fixtures$superlig_3_4 +
  SUPERLIG_fixtures$superlig_0_5 + SUPERLIG_fixtures$superlig_1_5 + SUPERLIG_fixtures$superlig_2_5 + SUPERLIG_fixtures$superlig_3_5 + SUPERLIG_fixtures$superlig_4_5 +
  SUPERLIG_fixtures$superlig_0_6 + SUPERLIG_fixtures$superlig_1_6 + SUPERLIG_fixtures$superlig_2_6 + SUPERLIG_fixtures$superlig_3_6 + SUPERLIG_fixtures$superlig_4_6 +
  SUPERLIG_fixtures$superlig_5_6

SUPERLIG_fixtures_clone$Awinodds <- round(1/SUPERLIG_fixtures_clone$Awinodds, digits = 3)

colnames(SUPERLIG_fixtures_clone)[15] <- "CS_1-1"
colnames(SUPERLIG_fixtures_clone)[13] <- "CS_1-0"
colnames(SUPERLIG_fixtures_clone)[14] <- "CS_0-1"
colnames(SUPERLIG_fixtures_clone)[16] <- "CS_2-0"
colnames(SUPERLIG_fixtures_clone)[17] <- "CS_0-2"
colnames(SUPERLIG_fixtures_clone)[19] <- "CS_2-1"
colnames(SUPERLIG_fixtures_clone)[20] <- "CS_1-2"

SUPERLIG_fixtures_clone$`CS_1-1` <- round(1/SUPERLIG_fixtures_clone$`CS_1-1`, digits = 3)
SUPERLIG_fixtures_clone$`CS_1-0` <- round(1/SUPERLIG_fixtures_clone$`CS_1-0`, digits = 3)
SUPERLIG_fixtures_clone$`CS_0-1` <- round(1/SUPERLIG_fixtures_clone$`CS_0-1`, digits = 3)
SUPERLIG_fixtures_clone$`CS_2-0` <- round(1/SUPERLIG_fixtures_clone$`CS_2-0`, digits = 3)
SUPERLIG_fixtures_clone$`CS_0-2` <- round(1/SUPERLIG_fixtures_clone$`CS_0-2`, digits = 3)
SUPERLIG_fixtures_clone$`CS_2-1` <- round(1/SUPERLIG_fixtures_clone$`CS_2-1`, digits = 3)
SUPERLIG_fixtures_clone$`CS_1-2` <- round(1/SUPERLIG_fixtures_clone$`CS_1-2`, digits = 3)

colnames(SUPERLIG_fixtures_clone)[1] <- "league"
colnames(SUPERLIG_fixtures_clone)[2] <- "Hometeam"
colnames(SUPERLIG_fixtures_clone)[3] <- "Awayteam"
colnames(SUPERLIG_fixtures_clone)[92] <- "predscore"
colnames(SUPERLIG_fixtures_clone)[64] <- "ov25"
colnames(SUPERLIG_fixtures_clone)[66] <- "ov25odds"
colnames(SUPERLIG_fixtures_clone)[65] <- "un25"
colnames(SUPERLIG_fixtures_clone)[67] <- "un25odds"
colnames(SUPERLIG_fixtures_clone)[68] <- "BTTSY"
colnames(SUPERLIG_fixtures_clone)[69] <- "BTTSN"
colnames(SUPERLIG_fixtures_clone)[70] <- "BTTSYodds"
colnames(SUPERLIG_fixtures_clone)[71] <- "BTTSNodds"

SUPERLIG_fixtures_clone <- SUPERLIG_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
SUPERLIG_fixtures_clone$matchid <- paste(SUPERLIG_fixtures_clone$Hometeam,SUPERLIG_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
SUPERLIG_fixtures_clone_final <- SUPERLIG_fixtures_clone[,-c(8,9,10,27)]
SUPERLIG_fixtures_clone_final[,'sep'] <- ''

superlig_dmprediction <-  superlig_picks[,c(4,5,6,7,8)]
superlig_dmprediction[,'sep2'] <- ''

superlig_avgyellow <- superlig_picks[,c(9,10)]
superlig_avgyellow[,'sep3'] <- ''

superlig_avgcorners <- superlig_picks[,c(11,12)]
superlig_avgcorners[,'sep4'] <- ''

superlig_goals <- SUPERLIG_fixtures[,c(10,11)]
superlig_goals$superlig_xGH <- round(superlig_goals$superlig_xGH, digits = 2)
superlig_goals$superlig_xGA <- round(superlig_goals$superlig_xGA, digits = 2)
superlig_goals$superlig_TxG <- superlig_goals$superlig_xGH + superlig_goals$superlig_xGA
superlig_goals[,'sep5'] <- ''

superlig_shots <- SUPERLIG_fixtures_sot[,c(10,11)]
superlig_shots$superlig_xHST <- round(superlig_shots$superlig_xHST, digits = 2)
superlig_shots$superlig_xAST <- round(superlig_shots$superlig_xAST, digits = 2)
superlig_shots$TxSOT <- superlig_shots$superlig_xHST + superlig_shots$superlig_xAST
superlig_shots[,'sep6'] <- ''

superlig_fouls <- SUPERLIG_fixtures_fo[,c(10,11)]
superlig_fouls$superlig_xHF <- round(superlig_fouls$superlig_xHF, digits = 2)
superlig_fouls$superlig_xAF <- round(superlig_fouls$superlig_xAF, digits = 2)
superlig_fouls$superlig_TxF <- superlig_fouls$superlig_xHF + superlig_fouls$superlig_xAF

superlig_ycpf <- superlig_picks[,c(15,16)]
superlig_fouls <- cbind(superlig_fouls,superlig_ycpf)
superlig_fouls$HYCPF <- as.numeric(superlig_fouls$HYCPF)
superlig_fouls$AYCPF <- as.numeric(superlig_fouls$AYCPF)
superlig_fouls$x_hyc <- (superlig_fouls$superlig_xHF) * (superlig_fouls$HYCPF)
superlig_fouls$x_ayc <- (superlig_fouls$superlig_xAF) * (superlig_fouls$AYCPF)
superlig_fouls$x_TYC <- round((superlig_fouls$x_hyc + superlig_fouls$x_ayc),digits = 2)
superlig_fouls[,'sep7'] <- ''

superlig_bookings <- SUPERLIG_fixtures_yc[,c(10,11)]
superlig_bookings$superlig_xHYC <- round(superlig_bookings$superlig_xHYC, digits = 2)
superlig_bookings$superlig_xAYC <- round(superlig_bookings$superlig_xAYC, digits = 2)
superlig_bookings$superlig_TYcards <- superlig_bookings$superlig_xHYC + superlig_bookings$superlig_xAYC
superlig_bookings[,'sep8'] <- ''

superlig_corners <- SUPERLIG_fixtures_co[,c(10,11)]
superlig_corners$superlig_xHCOC <- round(superlig_corners$superlig_xHCOC, digits = 2)
superlig_corners$superlig_xACOC <- round(superlig_corners$superlig_xACOC, digits = 2)
superlig_corners$superlig_TCOs <- superlig_corners$superlig_xHCOC + superlig_corners$superlig_xACOC
superlig_corners[,'sep9'] <- ''

superlig_shotsconversion <- superlig_picks[,c(13,14)]
superlig_shotsconversion <- cbind(superlig_shotsconversion,superlig_shots)
superlig_shotsconversion$HXSC <- as.numeric(superlig_shotsconversion$HXSC)
superlig_shotsconversion$AXSC <- as.numeric(superlig_shotsconversion$AXSC)
superlig_shotsconversion$superlig_hXgoals <- round((superlig_shotsconversion$HXSC * superlig_shotsconversion$superlig_xHST), digits = 2)
superlig_shotsconversion$superlig_aXgoals <- round((superlig_shotsconversion$AXSC * superlig_shotsconversion$superlig_xAST), digits = 2)
superlig_shotsconversion$Xgoals <- superlig_shotsconversion$superlig_hXgoals + superlig_shotsconversion$superlig_aXgoals

SUPERLIG_all <- cbind(SUPERLIG_fixtures_clone_final,superlig_dmprediction,superlig_avgyellow,superlig_avgcorners,superlig_goals,superlig_shots,superlig_fouls,superlig_bookings,superlig_corners,superlig_shotsconversion)
unlink('Divisions/SUPERLIG.xlsx')
write.xlsx(SUPERLIG_all,'Divisions/SUPERLIG.xlsx', sheetName = "SUPERLIG_all", append = TRUE)
write.xlsx(points_superlig,'Divisions/SUPERLIG.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(superlig_cornertotalsv2,'Divisions/SUPERLIG.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(superlig_goaltotalsv2,'Divisions/SUPERLIG.xlsx', sheetName = "Goaltotals", append = TRUE)


