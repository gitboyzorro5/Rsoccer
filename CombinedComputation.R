# #load the new data frames
# Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
# library('xlsx')
# library('sqldf')
# library('scales')
# source('divisions.R')
# source('Matchday.R')
# i1_currentround
#
# first_df <- E0_rounds[E0_rounds$e0_matchday > 0,]
# second_df <- I1_rounds[I1_rounds$i1_matchday > 0,]
# #third_df <- E0_rounds[E0_rounds$e0_matchday > 33,]
# first_df <- first_df[,-37]
# second_df <- second_df[,-37]
# #third_df <- third_df[,-37]
# UCL <- rbind(first_df,second_df)
# View(UCL)
#UCL <- SP1_rounds[SP1_rounds$sp1_matchday > 27,]
#UCL <- na.omit(UCL)
#goaltotals v2
ucl_goaltotalsv2 <- tapply(UCL$TG, UCL[c("HomeTeam", "AwayTeam")],mean)
ucl_hgtotals <- rowSums(ucl_goaltotalsv2, na.rm = T)
ucl_agtotals <- colSums(ucl_goaltotalsv2, na.rm = T)
ucl_goaltotalsv2 <- cbind(ucl_goaltotalsv2,ucl_hgtotals,ucl_agtotals)
ucl_totalgoals <- ucl_hgtotals + ucl_agtotals
ucl_goaltotalsv2 <- cbind(ucl_goaltotalsv2,ucl_totalgoals)
ucl_teams <- sort(unique(UCL$HomeTeam))
ucl_home_games <- c()
ucl_away_games <-c()
for (i_ucl in 1:length(ucl_teams))
{

  ucl_home_games[i_ucl] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl],])
  ucl_away_games[i_ucl]  <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl],])

}
ucl_games_played <- ucl_home_games + ucl_away_games
ucl_goaltotalsv2 <- cbind(ucl_goaltotalsv2,ucl_games_played)
ucl_avg_totalgoals <- round((ucl_totalgoals/ ucl_games_played), digits = 4)
ucl_goaltotalsv2[is.na(ucl_goaltotalsv2)] <- ""
ucl_goaltotalsv2 <- cbind(ucl_goaltotalsv2,ucl_avg_totalgoals)

############################################################################################################
#Cornertotals v2
ucl_cornertotalsv2 <- tapply(UCL$TC, UCL[c("HomeTeam", "AwayTeam")],mean)
ucl_hcototals <- rowSums(ucl_cornertotalsv2, na.rm = T)
ucl_acototals <- colSums(ucl_cornertotalsv2, na.rm = T)
ucl_cornertotalsv2 <- cbind(ucl_cornertotalsv2,ucl_hcototals,ucl_acototals)
ucl_totalcorners <- ucl_hcototals + ucl_acototals
ucl_cornertotalsv2 <- cbind(ucl_cornertotalsv2,ucl_totalcorners)
ucl_cornertotalsv2 <- cbind(ucl_cornertotalsv2,ucl_games_played)
ucl_avg_totalcorners <- round((ucl_totalcorners/ ucl_games_played), digits = 4)
ucl_cornertotalsv2[is.na(ucl_cornertotalsv2)] <- ""
ucl_cornertotalsv2 <- cbind(ucl_cornertotalsv2,ucl_avg_totalcorners)
############################################################################################################
#GS matrix
ucl_goalscored_h <- tapply(UCL$FTHG, UCL[c("HomeTeam", "Date")],mean)
ucl_goalscored_a <- tapply(UCL$FTAG, UCL[c("AwayTeam", "Date")],mean)
ucl_goalscored_h[is.na(ucl_goalscored_h)] <- ""
ucl_goalscored_a[is.na(ucl_goalscored_a)] <- ""
for(ucl_rowhgs in 1:nrow(ucl_goalscored_h)) {
  for(ucl_colhgs in 1:ncol(ucl_goalscored_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowags in 1:nrow(ucl_goalscored_a)) {
      for(ucl_colags in 1:ncol(ucl_goalscored_a)) {
        ifelse(!ucl_goalscored_a[ucl_rowags,ucl_colags]=="",ucl_goalscored_h[ucl_rowags,ucl_colags] <- ucl_goalscored_a[ucl_rowags,ucl_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
ucl_goalconceded_h <- tapply(UCL$FTAG, UCL[c("HomeTeam", "Date")],mean)
ucl_goalconceded_a <- tapply(UCL$FTHG, UCL[c("AwayTeam", "Date")],mean)
ucl_goalconceded_h[is.na(ucl_goalconceded_h)] <- ""
ucl_goalconceded_a[is.na(ucl_goalconceded_a)] <- ""
for(ucl_rowhgc in 1:nrow(ucl_goalconceded_h)) {
  for(ucl_colhgc in 1:ncol(ucl_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowagc in 1:nrow(ucl_goalconceded_a)) {
      for(ucl_colagc in 1:ncol(ucl_goalconceded_a)) {
        ifelse(!ucl_goalconceded_a[ucl_rowagc,ucl_colagc]=="",ucl_goalconceded_h[ucl_rowagc,ucl_colagc] <- ucl_goalconceded_a[ucl_rowagc,ucl_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
ucl_totalcorners_h <- tapply(UCL$TC, UCL[c("HomeTeam", "Date")],mean)
ucl_totalcorners_a <- tapply(UCL$TC, UCL[c("AwayTeam", "Date")],mean)
ucl_totalcorners_h[is.na(ucl_totalcorners_h)] <- ""
ucl_totalcorners_a[is.na(ucl_totalcorners_a)] <- ""
#UCL
for(ucl_rowTC in 1:nrow(ucl_totalcorners_h)) {
  for(ucl_colTC in 1:ncol(ucl_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowTC in 1:nrow(ucl_totalcorners_a)) {
      for(ucl_colTC in 1:ncol(ucl_totalcorners_a)) {
        ifelse(!ucl_totalcorners_a[ucl_rowTC,ucl_colTC]=="",ucl_totalcorners_h[ucl_rowTC,ucl_colTC] <- ucl_totalcorners_a[ucl_rowTC,ucl_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
ucl_coawarded_h <- tapply(UCL$HCO, UCL[c("HomeTeam", "Date")],mean)
ucl_coawarded_a <- tapply(UCL$ACO, UCL[c("AwayTeam", "Date")],mean)
ucl_coawarded_h[is.na(ucl_coawarded_h)] <- ""
ucl_coawarded_a[is.na(ucl_coawarded_a)] <- ""
#UCL
for(ucl_rowhco in 1:nrow(ucl_coawarded_h)) {
  for(ucl_colhco in 1:ncol(ucl_coawarded_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowaco in 1:nrow(ucl_coawarded_a)) {
      for(ucl_colaco in 1:ncol(ucl_coawarded_a)) {
        ifelse(!ucl_coawarded_a[ucl_rowaco,ucl_colaco]=="",ucl_coawarded_h[ucl_rowaco,ucl_colaco] <- ucl_coawarded_a[ucl_rowaco,ucl_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
ucl_cornersconceded_h <- tapply(UCL$ACO, UCL[c("HomeTeam", "Date")],mean)
ucl_cornersconceded_a <- tapply(UCL$HCO, UCL[c("AwayTeam", "Date")],mean)
ucl_cornersconceded_h[is.na(ucl_cornersconceded_h)] <- ""
ucl_cornersconceded_a[is.na(ucl_cornersconceded_a)] <- ""
#UCL
for(ucl_rowhcc in 1:nrow(ucl_cornersconceded_h)) {
  for(ucl_colhcc in 1:ncol(ucl_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowacc in 1:nrow(ucl_cornersconceded_a)) {
      for(ucl_colacc in 1:ncol(ucl_cornersconceded_a)) {
        ifelse(!ucl_cornersconceded_a[ucl_rowacc,ucl_colacc]=="",ucl_cornersconceded_h[ucl_rowacc,ucl_colacc] <- ucl_cornersconceded_a[ucl_rowacc,ucl_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
ucl_coscform_h <- tapply(UCL$COSC, UCL[c("HomeTeam", "Date")],median)
ucl_coscform_a <- tapply(UCL$COSC, UCL[c("AwayTeam", "Date")],median)
ucl_coscform_h[is.na(ucl_coscform_h)] <- ""
ucl_coscform_a[is.na(ucl_coscform_a)] <- ""
#UCL
for(ucl_rowh_f_cosc in 1:nrow(ucl_coscform_h)) {
  for(ucl_colh_f_cosc in 1:ncol(ucl_coscform_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowa_f_cosc in 1:nrow(ucl_coscform_a)) {
      for(ucl_cola_f_cosc in 1:ncol(ucl_coscform_a)) {
        ifelse(!ucl_coscform_a[ucl_rowa_f_cosc,ucl_cola_f_cosc]=="",ucl_coscform_h[ucl_rowa_f_cosc,ucl_cola_f_cosc] <- ucl_coscform_a[ucl_rowa_f_cosc,ucl_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
ucl_winmargin_h <- tapply(UCL$FTHG - UCL$FTAG, UCL[c("HomeTeam", "Date")],mean)
ucl_winmargin_a <- tapply(UCL$FTAG - UCL$FTHG, UCL[c("AwayTeam", "Date")],mean)
ucl_winmargin_h[is.na(ucl_winmargin_h)] <- ""
ucl_winmargin_a[is.na(ucl_winmargin_a)] <- ""
#UCL
for(ucl_rowhwm in 1:nrow(ucl_winmargin_h)) {
  for(ucl_colhwm in 1:ncol(ucl_winmargin_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowawm in 1:nrow(ucl_winmargin_a)) {
      for(ucl_colawm in 1:ncol(ucl_winmargin_a)) {
        ifelse(!ucl_winmargin_a[ucl_rowawm,ucl_colawm]=="",ucl_winmargin_h[ucl_rowawm,ucl_colawm] <- ucl_winmargin_a[ucl_rowawm,ucl_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
ucl_yellowscored_h <- tapply(UCL$HY, UCL[c("HomeTeam", "Date")],mean)
ucl_yellowscored_a <- tapply(UCL$AY, UCL[c("AwayTeam", "Date")],mean)
ucl_yellowscored_h[is.na(ucl_yellowscored_h)] <- ""
ucl_yellowscored_a[is.na(ucl_yellowscored_a)] <- ""
#UCL
for(ucl_rowhys in 1:nrow(ucl_yellowscored_h)) {
  for(ucl_colhys in 1:ncol(ucl_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(ucl_roways in 1:nrow(ucl_yellowscored_a)) {
      for(ucl_colays in 1:ncol(ucl_yellowscored_a)) {
        ifelse(!ucl_yellowscored_a[ucl_roways,ucl_colays]=="",ucl_yellowscored_h[ucl_roways,ucl_colays] <- ucl_yellowscored_a[ucl_roways,ucl_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
ucl_redscored_h <- tapply(UCL$HR, UCL[c("HomeTeam", "Date")],mean)
ucl_redscored_a <- tapply(UCL$AR, UCL[c("AwayTeam", "Date")],mean)
ucl_redscored_h[is.na(ucl_redscored_h)] <- ""
ucl_redscored_a[is.na(ucl_redscored_a)] <- ""
for(ucl_rowhrs in 1:nrow(ucl_redscored_h)) {
  for(ucl_colhrs in 1:ncol(ucl_redscored_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowars in 1:nrow(ucl_redscored_a)) {
      for(ucl_colars in 1:ncol(ucl_redscored_a)) {
        ifelse(!ucl_redscored_a[ucl_rowars,ucl_colars]=="",ucl_redscored_h[ucl_rowars,ucl_colars] <- ucl_redscored_a[ucl_rowars,ucl_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
ucl_redtotalsv2 <- tapply(UCL$TR, UCL[c("HomeTeam", "AwayTeam")],mean)
ucl_hrtotals <- rowSums(ucl_redtotalsv2, na.rm = T)
ucl_artotals <- colSums(ucl_redtotalsv2, na.rm = T)
ucl_redtotalsv2 <- cbind(ucl_redtotalsv2,ucl_hrtotals,ucl_artotals)
ucl_totalreds <- ucl_hrtotals + ucl_artotals
ucl_redtotalsv2 <- cbind(ucl_redtotalsv2,ucl_totalreds)
ucl_redtotalsv2 <- cbind(ucl_redtotalsv2,ucl_games_played)
ucl_avg_totalreds <- round((ucl_totalreds/ ucl_games_played), digits = 4)
ucl_redtotalsv2[is.na(ucl_redtotalsv2)] <- ""
ucl_redtotalsv2 <- cbind(ucl_redtotalsv2,ucl_avg_totalreds)
############################################################################################################################################################
#yellowtotals
ucl_yellowtotalsv2 <- tapply(UCL$TY, UCL[c("HomeTeam", "AwayTeam")],mean)
ucl_hytotals <- rowSums(ucl_yellowtotalsv2, na.rm = T)
ucl_aytotals <- colSums(ucl_yellowtotalsv2, na.rm = T)
ucl_yellowtotalsv2 <- cbind(ucl_yellowtotalsv2,ucl_hytotals,ucl_aytotals)
ucl_totalyellows <- ucl_hytotals + ucl_aytotals
ucl_yellowtotalsv2 <- cbind(ucl_yellowtotalsv2,ucl_totalyellows)
ucl_yellowtotalsv2 <- cbind(ucl_yellowtotalsv2,ucl_games_played)
ucl_avg_totalyellows <- round((ucl_totalyellows/ ucl_games_played), digits = 4)
ucl_yellowtotalsv2[is.na(ucl_yellowtotalsv2)] <- ""
ucl_yellowtotalsv2 <- cbind(ucl_yellowtotalsv2,ucl_avg_totalyellows)
##################################################################################################################################################
#team form
ucl_form_h <- tapply(UCL$FTR, UCL[c("HomeTeam", "Date")],median)
ucl_form_a <- tapply(UCL$FTR, UCL[c("AwayTeam", "Date")],median)
ucl_form_h[is.na(ucl_form_h)] <- ""
ucl_form_a[is.na(ucl_form_a)] <- ""
ucl_form_h <- sub("A","L",ucl_form_h)
ucl_form_h <- sub("H","W",ucl_form_h)
ucl_form_a <- sub("A","W",ucl_form_a)
ucl_form_a <- sub("H","L",ucl_form_a)
for(ucl_rowh_f in 1:nrow(ucl_form_h)) {
  for(ucl_colh_f in 1:ncol(ucl_form_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowa_f in 1:nrow(ucl_form_a)) {
      for(ucl_cola_f in 1:ncol(ucl_form_a)) {
        ifelse(!ucl_form_a[ucl_rowa_f,ucl_cola_f]=="",ucl_form_h[ucl_rowa_f,ucl_cola_f] <- ucl_form_a[ucl_rowa_f,ucl_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
ucl_csform_h <- tapply(UCL$CS, UCL[c("HomeTeam", "Date")],median)
ucl_csform_a <- tapply(UCL$CS, UCL[c("AwayTeam", "Date")],median)
ucl_csform_h[is.na(ucl_csform_h)] <- ""
ucl_csform_a[is.na(ucl_csform_a)] <- ""
#UCL
for(ucl_rowh_f_cs in 1:nrow(ucl_csform_h)) {
  for(ucl_colh_f_cs in 1:ncol(ucl_csform_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowa_f_cs in 1:nrow(ucl_csform_a)) {
      for(ucl_cola_f_cs in 1:ncol(ucl_csform_a)) {
        ifelse(!ucl_csform_a[ucl_rowa_f_cs,ucl_cola_f_cs]=="",ucl_csform_h[ucl_rowa_f_cs,ucl_cola_f_cs] <- ucl_csform_a[ucl_rowa_f_cs,ucl_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
ucl_totalgoals_h <- tapply(UCL$TG, UCL[c("HomeTeam", "Date")],mean)
ucl_totalgoals_a <- tapply(UCL$TG, UCL[c("AwayTeam", "Date")],mean)
ucl_totalgoals_h[is.na(ucl_totalgoals_h)] <- ""
ucl_totalgoals_a[is.na(ucl_totalgoals_a)] <- ""
for(ucl_rowh in 1:nrow(ucl_totalgoals_h)) {
  for(ucl_colh in 1:ncol(ucl_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowa in 1:nrow(ucl_totalgoals_a)) {
      for(ucl_cola in 1:ncol(ucl_totalgoals_a)) {
        ifelse(!ucl_totalgoals_a[ucl_rowa,ucl_cola]=="",ucl_totalgoals_h[ucl_rowa,ucl_cola] <- ucl_totalgoals_a[ucl_rowa,ucl_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#UCL
ucl_un05_home <- c()
ucl_un05_away <- c()
ucl_ov05_home <- c()
ucl_ov05_away <- c()

ucl_un15_home <- c()
ucl_un15_away <- c()
ucl_ov15_home <- c()
ucl_ov15_away <- c()

ucl_un25_home <- c()
ucl_un25_away <- c()
ucl_ov25_home <- c()
ucl_ov25_away <- c()

ucl_un35_home <- c()
ucl_un35_away <- c()
ucl_ov35_home <- c()
ucl_ov35_away <- c()

ucl_un45_home <- c()
ucl_un45_away <- c()
ucl_ov45_home <- c()
ucl_ov45_away <- c()

ucl_un55_home <- c()
ucl_un55_away <- c()
ucl_ov55_home <- c()
ucl_ov55_away <- c()

for (i_ucl_tg in 1:length(ucl_teams))
{

  ucl_un05_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG == 0,])
  ucl_un05_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG == 0,])

  ucl_ov05_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG > 0,])
  ucl_ov05_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG > 0,])

  ucl_un15_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 1,])
  ucl_un15_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 1,])

  ucl_ov15_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 2,])
  ucl_ov15_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 2,])

  ucl_un25_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 2,])
  ucl_un25_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 2,])

  ucl_ov25_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG >=3,])
  ucl_ov25_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG >=3,])

  ucl_un35_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 3,])
  ucl_un35_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 3,])

  ucl_ov35_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 4,])
  ucl_ov35_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 4,])

  ucl_un45_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 4,])
  ucl_un45_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 4,])

  ucl_ov45_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 5,])
  ucl_ov45_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 5,])

  ucl_un55_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 5,])
  ucl_un55_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG <= 5,])

  ucl_ov55_home[i_ucl_tg] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 6,])
  ucl_ov55_away[i_ucl_tg] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_tg] & UCL$TG >= 6,])


}

ucl_un05 <- ucl_un05_home + ucl_un05_away
ucl_ov05 <- ucl_ov05_home + ucl_ov05_away

ucl_un15 <- ucl_un15_home + ucl_un15_away
ucl_ov15 <- ucl_ov15_home + ucl_ov15_away

ucl_un25 <- ucl_un25_home + ucl_un25_away
ucl_ov25 <- ucl_ov25_home + ucl_ov25_away

ucl_un35 <- ucl_un35_home + ucl_un35_away
ucl_ov35 <- ucl_ov35_home + ucl_ov35_away

ucl_un45 <- ucl_un45_home + ucl_un45_away
ucl_ov45 <- ucl_ov45_home + ucl_ov45_away

ucl_un55 <- ucl_un55_home + ucl_un55_away
ucl_ov55 <- ucl_ov55_home + ucl_ov55_away

ucl_ovundata <- cbind(ucl_teams,ucl_un05,ucl_ov05,ucl_un15,ucl_ov15,ucl_un25,ucl_ov25,ucl_un35,ucl_ov35,ucl_un45,ucl_ov45,ucl_un55,ucl_ov55)
#################################################################################################################################################################
#team against
ucl_form_team_against_h <- tapply(UCL$AwayTeam, UCL[c("HomeTeam", "Date")],median)
ucl_form_team_against_a <- tapply(UCL$HomeTeam, UCL[c("AwayTeam", "Date")],median)
ucl_form_team_against_h[is.na(ucl_form_team_against_h)] <- ""
ucl_form_team_against_a[is.na(ucl_form_team_against_a)] <- ""
#UCL
for(ucl_rowh_f_against in 1:nrow(ucl_form_team_against_h)) {
  for(ucl_colh_f_against in 1:ncol(ucl_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(ucl_rowa_f_against in 1:nrow(ucl_form_team_against_a)) {
      for(ucl_cola_f_against in 1:ncol(ucl_form_team_against_a)) {
        ifelse(!ucl_form_team_against_a[ucl_rowa_f_against,ucl_cola_f_against]=="",ucl_form_team_against_h[ucl_rowa_f_against,ucl_cola_f_against] <- ucl_form_team_against_a[ucl_rowa_f_against,ucl_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#UCL
#home goals scored
ucl_home_gs <- aggregate(UCL$FTHG, by = list(UCL$HomeTeam), FUN = sum)
ucl_home_gs_avg <- aggregate(UCL$FTHG, by = list(UCL$HomeTeam),mean)
ucl_home_scoring <- merge(ucl_home_gs,ucl_home_gs_avg, by='Group.1',all = T)
names(ucl_home_scoring)[names(ucl_home_scoring) == "x.x"] <- "TFthg"
names(ucl_home_scoring)[names(ucl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
ucl_away_gs <- aggregate(UCL$FTAG, by = list(UCL$AwayTeam), FUN = sum)
ucl_away_gs_avg <- aggregate(UCL$FTAG, by = list(UCL$AwayTeam),mean)
ucl_away_scoring <- merge(ucl_away_gs,ucl_away_gs_avg, by='Group.1',all = T)
names(ucl_away_scoring)[names(ucl_away_scoring) == "x.x"] <- "TFtag"
names(ucl_away_scoring)[names(ucl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
ucl_scoring <- merge(ucl_home_scoring,ucl_away_scoring,by='Group.1',all = T)
ucl_scoring$TGS <- ucl_scoring$TFthg + ucl_scoring$TFtag

#Home shots on target
ucl_home_hst <- aggregate(UCL$HST, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_ast <- aggregate(UCL$AST, by = list(UCL$AwayTeam), FUN = sum)
ucl_tst <- merge(ucl_home_hst,ucl_away_ast, by='Group.1',all = T)
names(ucl_tst)[names(ucl_tst) == "x.x"] <- "hst"
names(ucl_tst)[names(ucl_tst) == "x.y"] <- "ast"
ucl_tst$TST <- ucl_tst$hst + ucl_tst$ast
#merge goals scored and shots on target
ucl_scoring_conversion <- merge(ucl_tst,ucl_scoring,by='Group.1',all = T)
#add HSC ASC TSC
ucl_scoring_conversion$HSTC <- percent(ucl_scoring_conversion$TFthg/ucl_scoring_conversion$hst, accuracy = 0.01)
ucl_scoring_conversion$ASTC <- percent(ucl_scoring_conversion$TFtag/ucl_scoring_conversion$ast, accuracy = 0.01)
ucl_scoring_conversion$TSTC <- percent(ucl_scoring_conversion$TGS/ucl_scoring_conversion$TST, accuracy = 0.01)
#merge games played
ucl_scoring_conversion <- cbind(ucl_scoring_conversion,ucl_games_played)
#create the second part
#home goals conceded
ucl_home_gc <- aggregate(UCL$FTAG, by = list(UCL$HomeTeam), FUN = sum)
ucl_home_gc_avg <- aggregate(UCL$FTAG, by = list(UCL$HomeTeam),mean)
ucl_home_conceding <- merge(ucl_home_gc,ucl_home_gc_avg, by='Group.1',all = T)
names(ucl_home_conceding)[names(ucl_home_conceding) == "x.x"] <- "TFthc"
names(ucl_home_conceding)[names(ucl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
ucl_away_gc <- aggregate(UCL$FTHG, by = list(UCL$AwayTeam), FUN = sum)
ucl_away_gc_avg <- aggregate(UCL$FTHG, by = list(UCL$AwayTeam),mean)
ucl_away_conceding <- merge(ucl_away_gc,ucl_away_gc_avg, by='Group.1',all = T)
names(ucl_away_conceding)[names(ucl_away_conceding) == "x.x"] <- "TFtac"
names(ucl_away_conceding)[names(ucl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
ucl_conceding <- merge(ucl_home_conceding,ucl_away_conceding,by='Group.1',all = T)
ucl_conceding$TGC <- ucl_conceding$TFthc + ucl_conceding$TFtac
ucl_home_hst
#Home shots conceded
ucl_home_hsc <- aggregate(UCL$AST, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_asc <- aggregate(UCL$HST, by = list(UCL$AwayTeam), FUN = sum)
ucl_tsc <- merge(ucl_home_hsc,ucl_away_asc, by='Group.1',all = T)
names(ucl_tsc)[names(ucl_tsc) == "x.x"] <- "hsc"
names(ucl_tsc)[names(ucl_tsc) == "x.y"] <- "asc"
ucl_tsc$TSC <- ucl_tsc$hsc + ucl_tsc$asc
#merge goals conceded and shots conceded
ucl_conceding_conversion <- merge(ucl_tsc,ucl_conceding,by='Group.1',all = T)

#add HSC ASC TSC
ucl_conceding_conversion$HSCC <- percent(ucl_conceding_conversion$TFthc/ucl_conceding_conversion$hsc, accuracy = 0.01)
ucl_conceding_conversion$ASCC <- percent(ucl_conceding_conversion$TFtac/ucl_conceding_conversion$asc, accuracy = 0.01)
ucl_conceding_conversion$TSCC <- percent(ucl_conceding_conversion$TGC/ucl_conceding_conversion$TSC, accuracy = 0.01)
ucl_conceding_conversion$XSTC <- round(ucl_scoring$TGS/(ucl_tst$TST - ucl_scoring$TGS), digits = 2)

#merge the two parts
ucl_shots_analysis <- merge(ucl_scoring_conversion,ucl_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#UCL
#home fouls for
ucl_home_fouls <- aggregate(UCL$HF, by = list(UCL$HomeTeam), FUN = sum)
ucl_home_fouls_avg <- aggregate(UCL$HF, by = list(UCL$HomeTeam),mean)
ucl_home_foulsdata <- merge(ucl_home_fouls,ucl_home_fouls_avg, by='Group.1',all = T)
names(ucl_home_foulsdata)[names(ucl_home_foulsdata) == "x.x"] <- "THfouls"
names(ucl_home_foulsdata)[names(ucl_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
ucl_away_fouls <- aggregate(UCL$HF, by = list(UCL$AwayTeam), FUN = sum)
ucl_away_fouls_avg <- aggregate(UCL$HF, by = list(UCL$AwayTeam),mean)
ucl_away_foulsdata <- merge(ucl_away_fouls,ucl_away_fouls_avg, by='Group.1',all = T)
names(ucl_away_foulsdata)[names(ucl_away_foulsdata) == "x.x"] <- "TAfouls"
names(ucl_away_foulsdata)[names(ucl_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
ucl_fouls <- merge(ucl_home_foulsdata,ucl_away_foulsdata,by='Group.1',all = T)
ucl_fouls$TotalFouls <- ucl_fouls$THfouls + ucl_fouls$TAfouls

#yellow cards
ucl_home_hyc <- aggregate(UCL$HY, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_ayc <- aggregate(UCL$AY, by = list(UCL$AwayTeam), FUN = sum)
ucl_tyc <- merge(ucl_home_hyc,ucl_away_ayc, by='Group.1',all = T)
names(ucl_tyc)[names(ucl_tyc) == "x.x"] <- "hyc"
names(ucl_tyc)[names(ucl_tyc) == "x.y"] <- "ayc"
ucl_tyc$TotalYellows <- ucl_tyc$hyc + ucl_tyc$ayc

#merge fouls for and yellow cards
ucl_fouls_conversion <- merge(ucl_tyc,ucl_fouls,by='Group.1',all = T)
ucl_fouls_conversion$YcPerfoul <- round((ucl_fouls_conversion$TotalYellows/ucl_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
UCL$Div <- "UCL"
##
###################################################################################################################################################
#poisson cards
ucl_GP <- nrow(UCL)
#Calculate total home goals for each division
ucl_T_HY <- sum(ucl_home_hyc$x)
#calculate average home goal
ucl_avg_HY <- round(ucl_T_HY /ucl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ucl_T_AY <- sum(ucl_away_ayc$x)
#calculate average away goal
ucl_avg_AY <- round(ucl_T_AY /ucl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ucl_home_yas <- round(((ucl_home_hyc$x/ucl_home_games))/ucl_avg_HY, digits = 4)
#calculate away attack strength
ucl_away_yas <- round(((ucl_away_ayc$x/ucl_away_games))/ucl_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
ucl_avg_HYC <- round(ucl_T_AY /ucl_GP, digits = 4)
#avg away concede
ucl_avg_AYC <- round(ucl_T_HY /ucl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
ucl_home_ycc <- aggregate(UCL$AY, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_ycc <- aggregate(UCL$HY, by = list(UCL$AwayTeam), FUN = sum)
#home defense strength
ucl_home_yds <- round(((ucl_home_ycc$x/ucl_home_games))/ucl_avg_HYC, digits = 4)
#away defense strength
ucl_away_yds <- round(((ucl_away_ycc$x/ucl_away_games))/ucl_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_home_poisson_yc <- cbind(ucl_division,ucl_teams,ucl_avg_HY,ucl_home_yas,ucl_home_yds)
#away poisson data
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_away_poisson_yc <- cbind(ucl_division,ucl_teams,ucl_avg_AY,ucl_away_yas,ucl_away_yds)
###
HomeTeam_ucl_yc <- rep(ucl_teams, each = length(ucl_teams))
AwayTeam_ucl_yc <- rep(ucl_teams, length(ucl_teams))
UCL_fixtures_yc <- cbind(HomeTeam_ucl_yc,AwayTeam_ucl_yc)
UCL_fixtures_yc <- as.data.frame(UCL_fixtures_yc)
UCL_fixtures_yc <- UCL_fixtures_yc[!UCL_fixtures_yc$HomeTeam_ucl_yc == UCL_fixtures_yc$AwayTeam_ucl_yc,]
rownames(UCL_fixtures_yc) <- NULL
UCL_fixtures_yc$Div <- "UCL"
UCL_fixtures_yc <- UCL_fixtures_yc[,c(3,1,2)]

UCL_fixtures_yc$avg_HY_ucl <- ucl_avg_HY

UCL_fixtures_yc$ucl_homeyas <- rep(ucl_home_yas,each = length(ucl_teams)-1)

ucl_awayyds_lookup <- cbind(ucl_teams,ucl_away_yds)

ucl_awayyds_lookup <- as.data.frame(ucl_awayyds_lookup)

colnames(ucl_awayyds_lookup) <- c("AwayTeam_ucl_yc","ucl_awayyds")


require('RH2')
UCL_fixtures_yc$ucl_awayyds <- sqldf("SELECT ucl_awayyds_lookup.ucl_awayyds FROM ucl_awayyds_lookup INNER JOIN UCL_fixtures_yc ON ucl_awayyds_lookup.AwayTeam_ucl_yc = UCL_fixtures_yc.AwayTeam_ucl_yc")

UCL_fixtures_yc$avg_AY_ucl <- ucl_avg_AY

ucl_awayyas_lookup <- cbind(ucl_teams,ucl_away_yas)

ucl_awayyas_lookup <- as.data.frame(ucl_awayyas_lookup)

colnames(ucl_awayyas_lookup) <- c("AwayTeam_ucl_yc","ucl_awayyas")

UCL_fixtures_yc$ucl_awayyas <- sqldf("SELECT ucl_awayyas_lookup.ucl_awayyas FROM ucl_awayyas_lookup INNER JOIN UCL_fixtures_yc ON ucl_awayyas_lookup.AwayTeam_ucl_yc = UCL_fixtures_yc.AwayTeam_ucl_yc")

UCL_fixtures_yc$ucl_homeyds <- rep(ucl_home_yds,each = length(ucl_teams)-1)

UCL_fixtures_yc$ucl_awayyds <- as.numeric(unlist(UCL_fixtures_yc$ucl_awayyds))
#xGH
UCL_fixtures_yc$ucl_xHYC <- UCL_fixtures_yc$avg_HY_ucl * UCL_fixtures_yc$ucl_homeyas * UCL_fixtures_yc$ucl_awayyds
#xGA

UCL_fixtures_yc$ucl_awayyas <- as.numeric(unlist(UCL_fixtures_yc$ucl_awayyas))

UCL_fixtures_yc$ucl_xAYC <- UCL_fixtures_yc$avg_AY_ucl * UCL_fixtures_yc$ucl_awayyas * UCL_fixtures_yc$ucl_homeyds

UCL_fixtures_yc$ucl_0_0 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_0 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_0_1 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_1 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_0 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_0_2 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_2 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_1 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_2 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_3 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_0 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_1 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_2 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_0_3 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_3 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_3 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_4 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_0 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_1 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_2 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_3 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_0_4 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_4 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_4 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_4 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_5 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_0 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_1 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_2 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_3 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_4 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_0_5 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_5 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_5 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_5 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_5 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_6 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_0 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(0,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_1 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(1,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_2 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(2,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_3 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(3,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_4 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(4,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_6_5 <- round(stats::dpois(6,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(5,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_0_6 <- round(stats::dpois(0,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_1_6 <- round(stats::dpois(1,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_2_6 <- round(stats::dpois(2,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_3_6 <- round(stats::dpois(3,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_4_6 <- round(stats::dpois(4,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
UCL_fixtures_yc$ucl_5_6 <- round(stats::dpois(5,UCL_fixtures_yc$ucl_xHYC) * stats::dpois(6,UCL_fixtures_yc$ucl_xAYC), digits = 4)
#Home win
UCL_fixtures_yc$ucl_H <- (
  UCL_fixtures_yc$ucl_1_0 + UCL_fixtures_yc$ucl_2_0 + UCL_fixtures_yc$ucl_2_1 + UCL_fixtures_yc$ucl_3_0 + UCL_fixtures_yc$ucl_3_1 +
    UCL_fixtures_yc$ucl_3_2 + UCL_fixtures_yc$ucl_4_0 + UCL_fixtures_yc$ucl_4_1 + UCL_fixtures_yc$ucl_4_2 + UCL_fixtures_yc$ucl_4_3 +
    UCL_fixtures_yc$ucl_5_0 + UCL_fixtures_yc$ucl_5_1 + UCL_fixtures_yc$ucl_5_2 + UCL_fixtures_yc$ucl_5_3 + UCL_fixtures_yc$ucl_5_4 +
    UCL_fixtures_yc$ucl_6_0 + UCL_fixtures_yc$ucl_6_1 + UCL_fixtures_yc$ucl_6_2 + UCL_fixtures_yc$ucl_6_3 + UCL_fixtures_yc$ucl_6_4 +
    UCL_fixtures_yc$ucl_6_5
)

UCL_fixtures_yc$ucl_H <- percent(UCL_fixtures_yc$ucl_H, accuracy = 0.1)

#Draw
UCL_fixtures_yc$ucl_D <- (

  UCL_fixtures_yc$ucl_0_0 + UCL_fixtures_yc$ucl_1_1 + UCL_fixtures_yc$ucl_2_2 + UCL_fixtures_yc$ucl_3_3 + UCL_fixtures_yc$ucl_4_4 +
    UCL_fixtures_yc$ucl_5_5 + UCL_fixtures_yc$ucl_6_6
)

UCL_fixtures_yc$ucl_D <- percent(UCL_fixtures_yc$ucl_D, accuracy = 0.1)

#Away

UCL_fixtures_yc$ucl_A <- (
  UCL_fixtures_yc$ucl_0_1 + UCL_fixtures_yc$ucl_0_2 + UCL_fixtures_yc$ucl_1_2 + UCL_fixtures_yc$ucl_0_3 + UCL_fixtures_yc$ucl_1_3 +
    UCL_fixtures_yc$ucl_2_3 + UCL_fixtures_yc$ucl_0_4 + UCL_fixtures_yc$ucl_1_4 + UCL_fixtures_yc$ucl_2_4 + UCL_fixtures_yc$ucl_3_4 +
    UCL_fixtures_yc$ucl_0_5 + UCL_fixtures_yc$ucl_1_5 + UCL_fixtures_yc$ucl_2_5 + UCL_fixtures_yc$ucl_3_5 + UCL_fixtures_yc$ucl_4_5 +
    UCL_fixtures_yc$ucl_0_6 + UCL_fixtures_yc$ucl_1_6 + UCL_fixtures_yc$ucl_2_6 + UCL_fixtures_yc$ucl_3_6 + UCL_fixtures_yc$ucl_4_6 +
    UCL_fixtures_yc$ucl_5_6
)

UCL_fixtures_yc$ucl_A <- percent(UCL_fixtures_yc$ucl_A, accuracy = 0.1)

#ov25
UCL_fixtures_yc$ucl_ov25 <- (
  UCL_fixtures_yc$ucl_2_1 + UCL_fixtures_yc$ucl_1_2 + UCL_fixtures_yc$ucl_2_2 + UCL_fixtures_yc$ucl_3_0 + UCL_fixtures_yc$ucl_3_1 +
    UCL_fixtures_yc$ucl_3_2 + UCL_fixtures_yc$ucl_0_3 + UCL_fixtures_yc$ucl_1_3 + UCL_fixtures_yc$ucl_2_3 + UCL_fixtures_yc$ucl_3_3 +
    UCL_fixtures_yc$ucl_4_0 + UCL_fixtures_yc$ucl_4_1 + UCL_fixtures_yc$ucl_4_2 + UCL_fixtures_yc$ucl_4_3 + UCL_fixtures_yc$ucl_0_4 +
    UCL_fixtures_yc$ucl_1_4 + UCL_fixtures_yc$ucl_2_4 + UCL_fixtures_yc$ucl_3_4 + UCL_fixtures_yc$ucl_4_4 + UCL_fixtures_yc$ucl_5_0 +
    UCL_fixtures_yc$ucl_5_1 + UCL_fixtures_yc$ucl_5_2 + UCL_fixtures_yc$ucl_5_3 + UCL_fixtures_yc$ucl_5_4 + UCL_fixtures_yc$ucl_0_5 +
    UCL_fixtures_yc$ucl_1_5 + UCL_fixtures_yc$ucl_2_5 + UCL_fixtures_yc$ucl_3_5 + UCL_fixtures_yc$ucl_4_5 + UCL_fixtures_yc$ucl_5_5 +
    UCL_fixtures_yc$ucl_6_0 + UCL_fixtures_yc$ucl_6_1 + UCL_fixtures_yc$ucl_6_2 + UCL_fixtures_yc$ucl_6_3 + UCL_fixtures_yc$ucl_6_4 +
    UCL_fixtures_yc$ucl_6_5 + UCL_fixtures_yc$ucl_0_6 + UCL_fixtures_yc$ucl_1_6 + UCL_fixtures_yc$ucl_2_6 + UCL_fixtures_yc$ucl_3_6 +
    UCL_fixtures_yc$ucl_4_6 + UCL_fixtures_yc$ucl_5_6 + UCL_fixtures_yc$ucl_6_6
)
#un25
UCL_fixtures_yc$ucl_un25 <- (
  UCL_fixtures_yc$ucl_0_0 + UCL_fixtures_yc$ucl_1_0 + UCL_fixtures_yc$ucl_0_1 + UCL_fixtures_yc$ucl_1_1 + UCL_fixtures_yc$ucl_2_0 + UCL_fixtures_yc$ucl_0_2
)
#odds
UCL_fixtures_yc$ucl_ov25_odds <- round((1/UCL_fixtures_yc$ucl_ov25),digits = 2)
UCL_fixtures_yc$ucl_un25_odds <- round((1/UCL_fixtures_yc$ucl_un25),digits = 2)

UCL_fixtures_yc$ucl_ov25_odds
UCL_fixtures_yc$ucl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UCL_fixtures_yc$ucl_ov25 <- percent(UCL_fixtures_yc$ucl_ov25, accuracy = 0.1)

UCL_fixtures_yc$ucl_un25 <- percent(UCL_fixtures_yc$ucl_un25, accuracy = 0.1)
UCL_fixtures_yc$ucl_pscore <- paste(round(UCL_fixtures_yc$ucl_xHYC,digits = 0),round(UCL_fixtures_yc$ucl_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
ucl_GP <- nrow(UCL)
#Calculate total home corners for each division
ucl_home_corners <- aggregate(UCL$HCO, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_corners <- aggregate(UCL$ACO, by = list(UCL$AwayTeam), FUN = sum)
###############################################################################
ucl_T_HCO <- sum(ucl_home_corners$x)
#calculate average home corners
ucl_avg_HCO <- round(ucl_T_HCO /ucl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ucl_T_ACO <- sum(ucl_away_corners$x)
#calculate average away goal
ucl_avg_ACO <- round(ucl_T_ACO /ucl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ucl_home_coas <- round(((ucl_home_corners$x/ucl_home_games))/ucl_avg_HCO, digits = 4)
#calculate away attack strength
ucl_away_coas <- round(((ucl_away_corners$x/ucl_away_games))/ucl_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
ucl_avg_HCOC <- round(ucl_T_ACO /ucl_GP, digits = 4)
#avg away concede
ucl_avg_ACOC <- round(ucl_T_HCO /ucl_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
ucl_home_coc <- aggregate(UCL$ACO, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_coc <- aggregate(UCL$HCO, by = list(UCL$AwayTeam), FUN = sum)
#home defense strength
ucl_home_cods <- round(((ucl_home_coc$x/ucl_home_games))/ucl_avg_HCOC, digits = 4)
#away defense strength
ucl_away_cods <- round(((ucl_away_coc$x/ucl_away_games))/ucl_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_home_poisson_corners <- cbind(ucl_division,ucl_teams,ucl_avg_HCO,ucl_home_coas,ucl_home_cods)
#################################################################################
#away poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_away_poisson_corners <- cbind(ucl_division,ucl_teams,ucl_avg_ACO,ucl_away_coas,ucl_away_cods)

#UCL
HomeTeam_ucl_co <- rep(ucl_teams, each = length(ucl_teams))
AwayTeam_ucl_co <- rep(ucl_teams, length(ucl_teams))
UCL_fixtures_co <- cbind(HomeTeam_ucl_co,AwayTeam_ucl_co)
UCL_fixtures_co <- as.data.frame(UCL_fixtures_co)
UCL_fixtures_co <- UCL_fixtures_co[!UCL_fixtures_co$HomeTeam_ucl_co == UCL_fixtures_co$AwayTeam_ucl_co,]
rownames(UCL_fixtures_co) <- NULL
UCL_fixtures_co$Div <- "UCL"
UCL_fixtures_co <- UCL_fixtures_co[,c(3,1,2)]

UCL_fixtures_co$avg_HCO_ucl <- ucl_avg_HCO

UCL_fixtures_co$ucl_homecoas <- rep(ucl_home_coas,each = length(ucl_teams)-1)

ucl_awaycods_lookup <- cbind(ucl_teams,ucl_away_cods)

ucl_awaycods_lookup <- as.data.frame(ucl_awaycods_lookup)

colnames(ucl_awaycods_lookup) <- c("AwayTeam_ucl_co","ucl_awaycods")


require('RH2')
UCL_fixtures_co$ucl_awaycods <- sqldf("SELECT ucl_awaycods_lookup.ucl_awaycods FROM ucl_awaycods_lookup INNER JOIN UCL_fixtures_co ON ucl_awaycods_lookup.AwayTeam_ucl_co = UCL_fixtures_co.AwayTeam_ucl_co")

UCL_fixtures_co$avg_ACO_ucl <- ucl_avg_ACO

ucl_awaycoas_lookup <- cbind(ucl_teams,ucl_away_coas)

ucl_awaycoas_lookup <- as.data.frame(ucl_awaycoas_lookup)

colnames(ucl_awaycoas_lookup) <- c("AwayTeam_ucl_co","ucl_awaycoas")

UCL_fixtures_co$ucl_awaycoas <- sqldf("SELECT ucl_awaycoas_lookup.ucl_awaycoas FROM ucl_awaycoas_lookup INNER JOIN UCL_fixtures_co ON ucl_awaycoas_lookup.AwayTeam_ucl_co = UCL_fixtures_co.AwayTeam_ucl_co")

UCL_fixtures_co$ucl_homecods <- rep(ucl_home_cods,each = length(ucl_teams)-1)

UCL_fixtures_co$ucl_awaycods <- as.numeric(unlist(UCL_fixtures_co$ucl_awaycods))
#xGH
UCL_fixtures_co$ucl_xHCOC <- UCL_fixtures_co$avg_HCO_ucl * UCL_fixtures_co$ucl_homecoas * UCL_fixtures_co$ucl_awaycods
#xGA

UCL_fixtures_co$ucl_awaycoas <- as.numeric(unlist(UCL_fixtures_co$ucl_awaycoas))

UCL_fixtures_co$ucl_xACOC <- UCL_fixtures_co$avg_ACO_ucl * UCL_fixtures_co$ucl_awaycoas * UCL_fixtures_co$ucl_homecods

UCL_fixtures_co$ucl_0_0 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_0 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_0_1 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_1 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_0 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_0_2 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_2 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_1 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_2 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_3 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_0 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_1 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_2 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_0_3 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_3 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_3 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_4 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_0 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_1 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_2 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_3 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_0_4 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_4 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_4 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_4 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_5 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_0 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_1 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_2 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_3 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_4 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_0_5 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_5 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_5 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_5 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_5 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_6 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_0 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(0,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_1 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(1,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_2 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(2,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_3 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(3,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_4 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(4,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_6_5 <- round(stats::dpois(6,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(5,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_0_6 <- round(stats::dpois(0,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_1_6 <- round(stats::dpois(1,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_2_6 <- round(stats::dpois(2,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_3_6 <- round(stats::dpois(3,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_4_6 <- round(stats::dpois(4,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
UCL_fixtures_co$ucl_5_6 <- round(stats::dpois(5,UCL_fixtures_co$ucl_xHCOC) * stats::dpois(6,UCL_fixtures_co$ucl_xACOC), digits = 4)
#Home win
UCL_fixtures_co$ucl_H <- (
  UCL_fixtures_co$ucl_1_0 + UCL_fixtures_co$ucl_2_0 + UCL_fixtures_co$ucl_2_1 + UCL_fixtures_co$ucl_3_0 + UCL_fixtures_co$ucl_3_1 +
    UCL_fixtures_co$ucl_3_2 + UCL_fixtures_co$ucl_4_0 + UCL_fixtures_co$ucl_4_1 + UCL_fixtures_co$ucl_4_2 + UCL_fixtures_co$ucl_4_3 +
    UCL_fixtures_co$ucl_5_0 + UCL_fixtures_co$ucl_5_1 + UCL_fixtures_co$ucl_5_2 + UCL_fixtures_co$ucl_5_3 + UCL_fixtures_co$ucl_5_4 +
    UCL_fixtures_co$ucl_6_0 + UCL_fixtures_co$ucl_6_1 + UCL_fixtures_co$ucl_6_2 + UCL_fixtures_co$ucl_6_3 + UCL_fixtures_co$ucl_6_4 +
    UCL_fixtures_co$ucl_6_5
)

UCL_fixtures_co$ucl_H <- percent(UCL_fixtures_co$ucl_H, accuracy = 0.1)

#Draw
UCL_fixtures_co$ucl_D <- (

  UCL_fixtures_co$ucl_0_0 + UCL_fixtures_co$ucl_1_1 + UCL_fixtures_co$ucl_2_2 + UCL_fixtures_co$ucl_3_3 + UCL_fixtures_co$ucl_4_4 +
    UCL_fixtures_co$ucl_5_5 + UCL_fixtures_co$ucl_6_6
)

UCL_fixtures_co$ucl_D <- percent(UCL_fixtures_co$ucl_D, accuracy = 0.1)

#Away

UCL_fixtures_co$ucl_A <- (
  UCL_fixtures_co$ucl_0_1 + UCL_fixtures_co$ucl_0_2 + UCL_fixtures_co$ucl_1_2 + UCL_fixtures_co$ucl_0_3 + UCL_fixtures_co$ucl_1_3 +
    UCL_fixtures_co$ucl_2_3 + UCL_fixtures_co$ucl_0_4 + UCL_fixtures_co$ucl_1_4 + UCL_fixtures_co$ucl_2_4 + UCL_fixtures_co$ucl_3_4 +
    UCL_fixtures_co$ucl_0_5 + UCL_fixtures_co$ucl_1_5 + UCL_fixtures_co$ucl_2_5 + UCL_fixtures_co$ucl_3_5 + UCL_fixtures_co$ucl_4_5 +
    UCL_fixtures_co$ucl_0_6 + UCL_fixtures_co$ucl_1_6 + UCL_fixtures_co$ucl_2_6 + UCL_fixtures_co$ucl_3_6 + UCL_fixtures_co$ucl_4_6 +
    UCL_fixtures_co$ucl_5_6
)

UCL_fixtures_co$ucl_A <- percent(UCL_fixtures_co$ucl_A, accuracy = 0.1)

#ov25
UCL_fixtures_co$ucl_ov25 <- (
  UCL_fixtures_co$ucl_2_1 + UCL_fixtures_co$ucl_1_2 + UCL_fixtures_co$ucl_2_2 + UCL_fixtures_co$ucl_3_0 + UCL_fixtures_co$ucl_3_1 +
    UCL_fixtures_co$ucl_3_2 + UCL_fixtures_co$ucl_0_3 + UCL_fixtures_co$ucl_1_3 + UCL_fixtures_co$ucl_2_3 + UCL_fixtures_co$ucl_3_3 +
    UCL_fixtures_co$ucl_4_0 + UCL_fixtures_co$ucl_4_1 + UCL_fixtures_co$ucl_4_2 + UCL_fixtures_co$ucl_4_3 + UCL_fixtures_co$ucl_0_4 +
    UCL_fixtures_co$ucl_1_4 + UCL_fixtures_co$ucl_2_4 + UCL_fixtures_co$ucl_3_4 + UCL_fixtures_co$ucl_4_4 + UCL_fixtures_co$ucl_5_0 +
    UCL_fixtures_co$ucl_5_1 + UCL_fixtures_co$ucl_5_2 + UCL_fixtures_co$ucl_5_3 + UCL_fixtures_co$ucl_5_4 + UCL_fixtures_co$ucl_0_5 +
    UCL_fixtures_co$ucl_1_5 + UCL_fixtures_co$ucl_2_5 + UCL_fixtures_co$ucl_3_5 + UCL_fixtures_co$ucl_4_5 + UCL_fixtures_co$ucl_5_5 +
    UCL_fixtures_co$ucl_6_0 + UCL_fixtures_co$ucl_6_1 + UCL_fixtures_co$ucl_6_2 + UCL_fixtures_co$ucl_6_3 + UCL_fixtures_co$ucl_6_4 +
    UCL_fixtures_co$ucl_6_5 + UCL_fixtures_co$ucl_0_6 + UCL_fixtures_co$ucl_1_6 + UCL_fixtures_co$ucl_2_6 + UCL_fixtures_co$ucl_3_6 +
    UCL_fixtures_co$ucl_4_6 + UCL_fixtures_co$ucl_5_6 + UCL_fixtures_co$ucl_6_6
)
#un25
UCL_fixtures_co$ucl_un25 <- (
  UCL_fixtures_co$ucl_0_0 + UCL_fixtures_co$ucl_1_0 + UCL_fixtures_co$ucl_0_1 + UCL_fixtures_co$ucl_1_1 + UCL_fixtures_co$ucl_2_0 + UCL_fixtures_co$ucl_0_2
)
#odds
UCL_fixtures_co$ucl_ov25_odds <- round((1/UCL_fixtures_co$ucl_ov25),digits = 2)
UCL_fixtures_co$ucl_un25_odds <- round((1/UCL_fixtures_co$ucl_un25),digits = 2)

UCL_fixtures_co$ucl_ov25_odds
UCL_fixtures_co$ucl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UCL_fixtures_co$ucl_ov25 <- percent(UCL_fixtures_co$ucl_ov25, accuracy = 0.1)

UCL_fixtures_co$ucl_un25 <- percent(UCL_fixtures_co$ucl_un25, accuracy = 0.1)
UCL_fixtures_co$ucl_pscore <- paste(round(UCL_fixtures_co$ucl_xHCOC,digits = 0),round(UCL_fixtures_co$ucl_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
ucl_GP <- nrow(UCL)
#Calculate total home goals for each division
ucl_T_HF <- sum(ucl_home_fouls$x)
#calculate average home goal
ucl_avg_HF <- round(ucl_T_HF /ucl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ucl_T_AF <- sum(ucl_away_fouls$x)
#calculate average away goal
ucl_avg_AF <- round(ucl_T_AF /ucl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ucl_home_fas <- round(((ucl_home_fouls$x/ucl_home_games))/ucl_avg_HF, digits = 4)
#calculate away attack strength
ucl_away_fas <- round(((ucl_away_fouls$x/ucl_away_games))/ucl_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
ucl_avg_HFC <- round(ucl_T_AF /ucl_GP, digits = 4)
#avg away concede
ucl_avg_AFC <- round(ucl_T_HF /ucl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
ucl_home_fcc <- aggregate(UCL$AF, by = list(UCL$HomeTeam), FUN = sum)
ucl_away_fcc <- aggregate(UCL$HF, by = list(UCL$AwayTeam), FUN = sum)

#home defense strength
ucl_home_fds <- round(((ucl_home_fcc$x/ucl_home_games))/ucl_avg_HFC, digits = 4)

#away defense strength
ucl_away_fds <- round(((ucl_away_fcc$x/ucl_away_games))/ucl_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_home_poisson_fo <- cbind(ucl_division,ucl_teams,ucl_avg_HF,ucl_home_fas,ucl_home_fds)

#################################################################################
#away poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_away_poisson_fo <- cbind(ucl_division,ucl_teams,ucl_avg_AF,ucl_away_fas,ucl_away_fds)

#UCL
HomeTeam_ucl_fo <- rep(ucl_teams, each = length(ucl_teams))
AwayTeam_ucl_fo <- rep(ucl_teams, length(ucl_teams))
UCL_fixtures_fo <- cbind(HomeTeam_ucl_fo,AwayTeam_ucl_fo)
UCL_fixtures_fo <- as.data.frame(UCL_fixtures_fo)
UCL_fixtures_fo <- UCL_fixtures_fo[!UCL_fixtures_fo$HomeTeam_ucl_fo == UCL_fixtures_fo$AwayTeam_ucl_fo,]
rownames(UCL_fixtures_fo) <- NULL
UCL_fixtures_fo$Div <- "UCL"
UCL_fixtures_fo <- UCL_fixtures_fo[,c(3,1,2)]

UCL_fixtures_fo$avg_HF_ucl <- ucl_avg_HF

UCL_fixtures_fo$ucl_homefas <- rep(ucl_home_fas,each = length(ucl_teams)-1)

ucl_awayfds_lookup <- cbind(ucl_teams,ucl_away_fds)

ucl_awayfds_lookup <- as.data.frame(ucl_awayfds_lookup)

colnames(ucl_awayfds_lookup) <- c("AwayTeam_ucl_fo","ucl_awayfds")


require('RH2')
UCL_fixtures_fo$ucl_awayfds <- sqldf("SELECT ucl_awayfds_lookup.ucl_awayfds FROM ucl_awayfds_lookup INNER JOIN UCL_fixtures_fo ON ucl_awayfds_lookup.AwayTeam_ucl_fo = UCL_fixtures_fo.AwayTeam_ucl_fo")

UCL_fixtures_fo$avg_AF_ucl <- ucl_avg_AF

ucl_awayfas_lookup <- cbind(ucl_teams,ucl_away_fas)

ucl_awayfas_lookup <- as.data.frame(ucl_awayfas_lookup)

colnames(ucl_awayfas_lookup) <- c("AwayTeam_ucl_fo","ucl_awayfas")

UCL_fixtures_fo$ucl_awayfas <- sqldf("SELECT ucl_awayfas_lookup.ucl_awayfas FROM ucl_awayfas_lookup INNER JOIN UCL_fixtures_fo ON ucl_awayfas_lookup.AwayTeam_ucl_fo = UCL_fixtures_fo.AwayTeam_ucl_fo")

UCL_fixtures_fo$ucl_homefds <- rep(ucl_home_fds,each = length(ucl_teams)-1)

UCL_fixtures_fo$ucl_awayfds <- as.numeric(unlist(UCL_fixtures_fo$ucl_awayfds))
#xGH
UCL_fixtures_fo$ucl_xHF <- UCL_fixtures_fo$avg_HF_ucl * UCL_fixtures_fo$ucl_homefas * UCL_fixtures_fo$ucl_awayfds
#xGA

UCL_fixtures_fo$ucl_awayfas <- as.numeric(unlist(UCL_fixtures_fo$ucl_awayfas))

UCL_fixtures_fo$ucl_xAF <- UCL_fixtures_fo$avg_AF_ucl * UCL_fixtures_fo$ucl_awayfas * UCL_fixtures_fo$ucl_homefds

UCL_fixtures_fo$ucl_0_0 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_0 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_0_1 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_1 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_0 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_0_2 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_2 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_1 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_2 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_3 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_0 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_1 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_2 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_0_3 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_3 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_3 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_4 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_0 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_1 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_2 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_3 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_0_4 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_4 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_4 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_4 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_5 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_0 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_1 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_2 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_3 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_4 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_0_5 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_5 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_5 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_5 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_5 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_6 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_0 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(0,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_1 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(1,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_2 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(2,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_3 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(3,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_4 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(4,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_6_5 <- round(stats::dpois(6,UCL_fixtures_fo$ucl_xHF) * stats::dpois(5,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_0_6 <- round(stats::dpois(0,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_1_6 <- round(stats::dpois(1,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_2_6 <- round(stats::dpois(2,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_3_6 <- round(stats::dpois(3,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_4_6 <- round(stats::dpois(4,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
UCL_fixtures_fo$ucl_5_6 <- round(stats::dpois(5,UCL_fixtures_fo$ucl_xHF) * stats::dpois(6,UCL_fixtures_fo$ucl_xAF), digits = 4)
#Home win
UCL_fixtures_fo$ucl_H <- (
  UCL_fixtures_fo$ucl_1_0 + UCL_fixtures_fo$ucl_2_0 + UCL_fixtures_fo$ucl_2_1 + UCL_fixtures_fo$ucl_3_0 + UCL_fixtures_fo$ucl_3_1 +
    UCL_fixtures_fo$ucl_3_2 + UCL_fixtures_fo$ucl_4_0 + UCL_fixtures_fo$ucl_4_1 + UCL_fixtures_fo$ucl_4_2 + UCL_fixtures_fo$ucl_4_3 +
    UCL_fixtures_fo$ucl_5_0 + UCL_fixtures_fo$ucl_5_1 + UCL_fixtures_fo$ucl_5_2 + UCL_fixtures_fo$ucl_5_3 + UCL_fixtures_fo$ucl_5_4 +
    UCL_fixtures_fo$ucl_6_0 + UCL_fixtures_fo$ucl_6_1 + UCL_fixtures_fo$ucl_6_2 + UCL_fixtures_fo$ucl_6_3 + UCL_fixtures_fo$ucl_6_4 +
    UCL_fixtures_fo$ucl_6_5
)

UCL_fixtures_fo$ucl_H <- percent(UCL_fixtures_fo$ucl_H, accuracy = 0.1)

#Draw
UCL_fixtures_fo$ucl_D <- (

  UCL_fixtures_fo$ucl_0_0 + UCL_fixtures_fo$ucl_1_1 + UCL_fixtures_fo$ucl_2_2 + UCL_fixtures_fo$ucl_3_3 + UCL_fixtures_fo$ucl_4_4 +
    UCL_fixtures_fo$ucl_5_5 + UCL_fixtures_fo$ucl_6_6
)

UCL_fixtures_fo$ucl_D <- percent(UCL_fixtures_fo$ucl_D, accuracy = 0.1)

#Away

UCL_fixtures_fo$ucl_A <- (
  UCL_fixtures_fo$ucl_0_1 + UCL_fixtures_fo$ucl_0_2 + UCL_fixtures_fo$ucl_1_2 + UCL_fixtures_fo$ucl_0_3 + UCL_fixtures_fo$ucl_1_3 +
    UCL_fixtures_fo$ucl_2_3 + UCL_fixtures_fo$ucl_0_4 + UCL_fixtures_fo$ucl_1_4 + UCL_fixtures_fo$ucl_2_4 + UCL_fixtures_fo$ucl_3_4 +
    UCL_fixtures_fo$ucl_0_5 + UCL_fixtures_fo$ucl_1_5 + UCL_fixtures_fo$ucl_2_5 + UCL_fixtures_fo$ucl_3_5 + UCL_fixtures_fo$ucl_4_5 +
    UCL_fixtures_fo$ucl_0_6 + UCL_fixtures_fo$ucl_1_6 + UCL_fixtures_fo$ucl_2_6 + UCL_fixtures_fo$ucl_3_6 + UCL_fixtures_fo$ucl_4_6 +
    UCL_fixtures_fo$ucl_5_6
)

UCL_fixtures_fo$ucl_A <- percent(UCL_fixtures_fo$ucl_A, accuracy = 0.1)

#ov25
UCL_fixtures_fo$ucl_ov25 <- (
  UCL_fixtures_fo$ucl_2_1 + UCL_fixtures_fo$ucl_1_2 + UCL_fixtures_fo$ucl_2_2 + UCL_fixtures_fo$ucl_3_0 + UCL_fixtures_fo$ucl_3_1 +
    UCL_fixtures_fo$ucl_3_2 + UCL_fixtures_fo$ucl_0_3 + UCL_fixtures_fo$ucl_1_3 + UCL_fixtures_fo$ucl_2_3 + UCL_fixtures_fo$ucl_3_3 +
    UCL_fixtures_fo$ucl_4_0 + UCL_fixtures_fo$ucl_4_1 + UCL_fixtures_fo$ucl_4_2 + UCL_fixtures_fo$ucl_4_3 + UCL_fixtures_fo$ucl_0_4 +
    UCL_fixtures_fo$ucl_1_4 + UCL_fixtures_fo$ucl_2_4 + UCL_fixtures_fo$ucl_3_4 + UCL_fixtures_fo$ucl_4_4 + UCL_fixtures_fo$ucl_5_0 +
    UCL_fixtures_fo$ucl_5_1 + UCL_fixtures_fo$ucl_5_2 + UCL_fixtures_fo$ucl_5_3 + UCL_fixtures_fo$ucl_5_4 + UCL_fixtures_fo$ucl_0_5 +
    UCL_fixtures_fo$ucl_1_5 + UCL_fixtures_fo$ucl_2_5 + UCL_fixtures_fo$ucl_3_5 + UCL_fixtures_fo$ucl_4_5 + UCL_fixtures_fo$ucl_5_5 +
    UCL_fixtures_fo$ucl_6_0 + UCL_fixtures_fo$ucl_6_1 + UCL_fixtures_fo$ucl_6_2 + UCL_fixtures_fo$ucl_6_3 + UCL_fixtures_fo$ucl_6_4 +
    UCL_fixtures_fo$ucl_6_5 + UCL_fixtures_fo$ucl_0_6 + UCL_fixtures_fo$ucl_1_6 + UCL_fixtures_fo$ucl_2_6 + UCL_fixtures_fo$ucl_3_6 +
    UCL_fixtures_fo$ucl_4_6 + UCL_fixtures_fo$ucl_5_6 + UCL_fixtures_fo$ucl_6_6
)
#un25
UCL_fixtures_fo$ucl_un25 <- (
  UCL_fixtures_fo$ucl_0_0 + UCL_fixtures_fo$ucl_1_0 + UCL_fixtures_fo$ucl_0_1 + UCL_fixtures_fo$ucl_1_1 + UCL_fixtures_fo$ucl_2_0 + UCL_fixtures_fo$ucl_0_2
)
#odds
UCL_fixtures_fo$ucl_ov25_odds <- round((1/UCL_fixtures_fo$ucl_ov25),digits = 2)
UCL_fixtures_fo$ucl_un25_odds <- round((1/UCL_fixtures_fo$ucl_un25),digits = 2)

UCL_fixtures_fo$ucl_ov25_odds
UCL_fixtures_fo$ucl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UCL_fixtures_fo$ucl_ov25 <- percent(UCL_fixtures_fo$ucl_ov25, accuracy = 0.1)

UCL_fixtures_fo$ucl_un25 <- percent(UCL_fixtures_fo$ucl_un25, accuracy = 0.1)
UCL_fixtures_fo$ucl_psfore <- paste(round(UCL_fixtures_fo$ucl_xHF,digits = 0),round(UCL_fixtures_fo$ucl_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
ucl_GP <- nrow(UCL)

#Calculate total home goals for each division
ucl_T_HST <- sum(ucl_home_hst$x)
#calculate average home goal

ucl_avg_HST <- round(ucl_T_HST /ucl_GP, digits = 4)

############################################################
#Calculate total away goals for each division
ucl_T_AST <- sum(ucl_away_ast$x)
#calculate average away goal
ucl_avg_AST <- round(ucl_T_AST /ucl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ucl_home_sotas <- round(((ucl_home_hst$x/ucl_home_games))/ucl_avg_HST, digits = 4)
#calculate away attack strength
ucl_away_sotas <- round(((ucl_away_ast$x/ucl_away_games))/ucl_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
ucl_avg_HSC <- round(ucl_T_AST /ucl_GP, digits = 4)

#avg away concede
ucl_avg_ASC <- round(ucl_T_HST /ucl_GP, digits = 4)
#home defense strength
ucl_home_sods <- round(((ucl_home_hsc$x/ucl_home_games))/ucl_avg_HSC, digits = 4)

#away defense strength
ucl_away_sods <- round(((ucl_away_ast$x/ucl_away_games))/ucl_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_home_poisson_sot <- cbind(ucl_division,ucl_teams,ucl_avg_HST,ucl_home_sotas,ucl_home_sods)

#################################################################################
#away poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_away_poisson_sot <- cbind(ucl_division,ucl_teams,ucl_avg_AST,ucl_away_sotas,ucl_away_sods)

#UCL
HomeTeam_ucl_sot <- rep(ucl_teams, each = length(ucl_teams))
AwayTeam_ucl_sot <- rep(ucl_teams, length(ucl_teams))
UCL_fixtures_sot <- cbind(HomeTeam_ucl_sot,AwayTeam_ucl_sot)
UCL_fixtures_sot <- as.data.frame(UCL_fixtures_sot)
UCL_fixtures_sot <- UCL_fixtures_sot[!UCL_fixtures_sot$HomeTeam_ucl_sot == UCL_fixtures_sot$AwayTeam_ucl_sot,]
rownames(UCL_fixtures_sot) <- NULL
UCL_fixtures_sot$Div <- "UCL"
UCL_fixtures_sot <- UCL_fixtures_sot[,c(3,1,2)]

UCL_fixtures_sot$avg_HST_ucl <- ucl_avg_HST

UCL_fixtures_sot$ucl_homesotas <- rep(ucl_home_sotas,each = length(ucl_teams)-1)

ucl_awaysods_lookup <- cbind(ucl_teams,ucl_away_sods)

ucl_awaysods_lookup <- as.data.frame(ucl_awaysods_lookup)

colnames(ucl_awaysods_lookup) <- c("AwayTeam_ucl_sot","ucl_awaysods")


require('RH2')
UCL_fixtures_sot$ucl_awaysods <- sqldf("SELECT ucl_awaysods_lookup.ucl_awaysods FROM ucl_awaysods_lookup INNER JOIN UCL_fixtures_sot ON ucl_awaysods_lookup.AwayTeam_ucl_sot = UCL_fixtures_sot.AwayTeam_ucl_sot")

UCL_fixtures_sot$avg_AST_ucl <- ucl_avg_AST

ucl_awaysotas_lookup <- cbind(ucl_teams,ucl_away_sotas)

ucl_awaysotas_lookup <- as.data.frame(ucl_awaysotas_lookup)

colnames(ucl_awaysotas_lookup) <- c("AwayTeam_ucl_sot","ucl_awaysotas")

UCL_fixtures_sot$ucl_awaysotas <- sqldf("SELECT ucl_awaysotas_lookup.ucl_awaysotas FROM ucl_awaysotas_lookup INNER JOIN UCL_fixtures_sot ON ucl_awaysotas_lookup.AwayTeam_ucl_sot = UCL_fixtures_sot.AwayTeam_ucl_sot")

UCL_fixtures_sot$ucl_homesods <- rep(ucl_home_sods,each = length(ucl_teams)-1)

UCL_fixtures_sot$ucl_awaysods <- as.numeric(unlist(UCL_fixtures_sot$ucl_awaysods))
#xGH
UCL_fixtures_sot$ucl_xHST <- UCL_fixtures_sot$avg_HST_ucl * UCL_fixtures_sot$ucl_homesotas * UCL_fixtures_sot$ucl_awaysods
#xGA

UCL_fixtures_sot$ucl_awaysotas <- as.numeric(unlist(UCL_fixtures_sot$ucl_awaysotas))

UCL_fixtures_sot$ucl_xAST <- UCL_fixtures_sot$avg_AST_ucl * UCL_fixtures_sot$ucl_awaysotas * UCL_fixtures_sot$ucl_homesods

UCL_fixtures_sot$ucl_0_0 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_0 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_0_1 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_1 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_0 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_0_2 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_2 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_1 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_2 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_3 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_0 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_1 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_2 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_0_3 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_3 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_3 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_4 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_0 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_1 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_2 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_3 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_0_4 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_4 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_4 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_4 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_5 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_0 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_1 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_2 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_3 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_4 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_0_5 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_5 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_5 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_5 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_5 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_6 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_0 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(0,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_1 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(1,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_2 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(2,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_3 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(3,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_4 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(4,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_6_5 <- round(stats::dpois(6,UCL_fixtures_sot$ucl_xHST) * stats::dpois(5,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_0_6 <- round(stats::dpois(0,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_1_6 <- round(stats::dpois(1,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_2_6 <- round(stats::dpois(2,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_3_6 <- round(stats::dpois(3,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_4_6 <- round(stats::dpois(4,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
UCL_fixtures_sot$ucl_5_6 <- round(stats::dpois(5,UCL_fixtures_sot$ucl_xHST) * stats::dpois(6,UCL_fixtures_sot$ucl_xAST), digits = 4)
#Home win
UCL_fixtures_sot$ucl_H <- (
  UCL_fixtures_sot$ucl_1_0 + UCL_fixtures_sot$ucl_2_0 + UCL_fixtures_sot$ucl_2_1 + UCL_fixtures_sot$ucl_3_0 + UCL_fixtures_sot$ucl_3_1 +
    UCL_fixtures_sot$ucl_3_2 + UCL_fixtures_sot$ucl_4_0 + UCL_fixtures_sot$ucl_4_1 + UCL_fixtures_sot$ucl_4_2 + UCL_fixtures_sot$ucl_4_3 +
    UCL_fixtures_sot$ucl_5_0 + UCL_fixtures_sot$ucl_5_1 + UCL_fixtures_sot$ucl_5_2 + UCL_fixtures_sot$ucl_5_3 + UCL_fixtures_sot$ucl_5_4 +
    UCL_fixtures_sot$ucl_6_0 + UCL_fixtures_sot$ucl_6_1 + UCL_fixtures_sot$ucl_6_2 + UCL_fixtures_sot$ucl_6_3 + UCL_fixtures_sot$ucl_6_4 +
    UCL_fixtures_sot$ucl_6_5
)

UCL_fixtures_sot$ucl_H <- percent(UCL_fixtures_sot$ucl_H, accuracy = 0.1)

#Draw
UCL_fixtures_sot$ucl_D <- (

  UCL_fixtures_sot$ucl_0_0 + UCL_fixtures_sot$ucl_1_1 + UCL_fixtures_sot$ucl_2_2 + UCL_fixtures_sot$ucl_3_3 + UCL_fixtures_sot$ucl_4_4 +
    UCL_fixtures_sot$ucl_5_5 + UCL_fixtures_sot$ucl_6_6
)

UCL_fixtures_sot$ucl_D <- percent(UCL_fixtures_sot$ucl_D, accuracy = 0.1)

#Away

UCL_fixtures_sot$ucl_A <- (
  UCL_fixtures_sot$ucl_0_1 + UCL_fixtures_sot$ucl_0_2 + UCL_fixtures_sot$ucl_1_2 + UCL_fixtures_sot$ucl_0_3 + UCL_fixtures_sot$ucl_1_3 +
    UCL_fixtures_sot$ucl_2_3 + UCL_fixtures_sot$ucl_0_4 + UCL_fixtures_sot$ucl_1_4 + UCL_fixtures_sot$ucl_2_4 + UCL_fixtures_sot$ucl_3_4 +
    UCL_fixtures_sot$ucl_0_5 + UCL_fixtures_sot$ucl_1_5 + UCL_fixtures_sot$ucl_2_5 + UCL_fixtures_sot$ucl_3_5 + UCL_fixtures_sot$ucl_4_5 +
    UCL_fixtures_sot$ucl_0_6 + UCL_fixtures_sot$ucl_1_6 + UCL_fixtures_sot$ucl_2_6 + UCL_fixtures_sot$ucl_3_6 + UCL_fixtures_sot$ucl_4_6 +
    UCL_fixtures_sot$ucl_5_6
)

UCL_fixtures_sot$ucl_A <- percent(UCL_fixtures_sot$ucl_A, accuracy = 0.1)

#ov25
UCL_fixtures_sot$ucl_ov25 <- (
  UCL_fixtures_sot$ucl_2_1 + UCL_fixtures_sot$ucl_1_2 + UCL_fixtures_sot$ucl_2_2 + UCL_fixtures_sot$ucl_3_0 + UCL_fixtures_sot$ucl_3_1 +
    UCL_fixtures_sot$ucl_3_2 + UCL_fixtures_sot$ucl_0_3 + UCL_fixtures_sot$ucl_1_3 + UCL_fixtures_sot$ucl_2_3 + UCL_fixtures_sot$ucl_3_3 +
    UCL_fixtures_sot$ucl_4_0 + UCL_fixtures_sot$ucl_4_1 + UCL_fixtures_sot$ucl_4_2 + UCL_fixtures_sot$ucl_4_3 + UCL_fixtures_sot$ucl_0_4 +
    UCL_fixtures_sot$ucl_1_4 + UCL_fixtures_sot$ucl_2_4 + UCL_fixtures_sot$ucl_3_4 + UCL_fixtures_sot$ucl_4_4 + UCL_fixtures_sot$ucl_5_0 +
    UCL_fixtures_sot$ucl_5_1 + UCL_fixtures_sot$ucl_5_2 + UCL_fixtures_sot$ucl_5_3 + UCL_fixtures_sot$ucl_5_4 + UCL_fixtures_sot$ucl_0_5 +
    UCL_fixtures_sot$ucl_1_5 + UCL_fixtures_sot$ucl_2_5 + UCL_fixtures_sot$ucl_3_5 + UCL_fixtures_sot$ucl_4_5 + UCL_fixtures_sot$ucl_5_5 +
    UCL_fixtures_sot$ucl_6_0 + UCL_fixtures_sot$ucl_6_1 + UCL_fixtures_sot$ucl_6_2 + UCL_fixtures_sot$ucl_6_3 + UCL_fixtures_sot$ucl_6_4 +
    UCL_fixtures_sot$ucl_6_5 + UCL_fixtures_sot$ucl_0_6 + UCL_fixtures_sot$ucl_1_6 + UCL_fixtures_sot$ucl_2_6 + UCL_fixtures_sot$ucl_3_6 +
    UCL_fixtures_sot$ucl_4_6 + UCL_fixtures_sot$ucl_5_6 + UCL_fixtures_sot$ucl_6_6
)
#un25
UCL_fixtures_sot$ucl_un25 <- (
  UCL_fixtures_sot$ucl_0_0 + UCL_fixtures_sot$ucl_1_0 + UCL_fixtures_sot$ucl_0_1 + UCL_fixtures_sot$ucl_1_1 + UCL_fixtures_sot$ucl_2_0 + UCL_fixtures_sot$ucl_0_2
)
#odds
UCL_fixtures_sot$ucl_ov25_odds <- round((1/UCL_fixtures_sot$ucl_ov25),digits = 2)
UCL_fixtures_sot$ucl_un25_odds <- round((1/UCL_fixtures_sot$ucl_un25),digits = 2)

UCL_fixtures_sot$ucl_ov25_odds
UCL_fixtures_sot$ucl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UCL_fixtures_sot$ucl_ov25 <- percent(UCL_fixtures_sot$ucl_ov25, accuracy = 0.1)

UCL_fixtures_sot$ucl_un25 <- percent(UCL_fixtures_sot$ucl_un25, accuracy = 0.1)
UCL_fixtures_sot$ucl_pssotre <- paste(round(UCL_fixtures_sot$ucl_xHST,digits = 0),round(UCL_fixtures_sot$ucl_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
ucl_home_wins <- c()
ucl_away_wins <- c()
ucl_home_draws <- c()
ucl_away_draws <- c()
ucl_home_loss <- c()
ucl_away_loss <- c()



for (i_ucl_wins in 1:length(ucl_teams))
{

  ucl_home_wins[i_ucl_wins] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_wins] & UCL$FTR == "H",])
  ucl_away_wins[i_ucl_wins] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_wins] & UCL$FTR == "A",])
  ucl_home_draws[i_ucl_wins] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_wins] & UCL$FTR == "D",])
  ucl_away_draws[i_ucl_wins] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_wins] & UCL$FTR == "D",])
  ucl_home_loss[i_ucl_wins] <- nrow(UCL[UCL$HomeTeam == ucl_teams[i_ucl_wins] & UCL$FTR == "A",])
  ucl_away_loss[i_ucl_wins] <- nrow(UCL[UCL$AwayTeam == ucl_teams[i_ucl_wins] & UCL$FTR == "H",])

}

ucl_total_wins <- ucl_home_wins + ucl_away_wins
ucl_total_draws <- ucl_home_draws + ucl_away_draws
ucl_total_loss <- ucl_home_loss + ucl_away_loss

ucl_league_table <- cbind(ucl_teams,ucl_games_played,ucl_total_wins,ucl_total_draws,ucl_total_loss)
ucl_GS <- ucl_scoring$TGS
ucl_GC <-ucl_conceding$TGC
ucl_GD <- ucl_scoring$TGS - ucl_conceding$TGC
ucl_PTS <- (ucl_total_wins*3) + (ucl_total_draws*1)
ucl_league_table <- cbind(ucl_league_table,ucl_GS,ucl_GC,ucl_GD,ucl_PTS)
ucl_league_table <- as.data.frame(ucl_league_table)
#rename the columns
names(ucl_league_table)[names(ucl_league_table) == "ucl_teams"] <- "Team"
names(ucl_league_table)[names(ucl_league_table) == "ucl_games_played"] <- "P"
names(ucl_league_table)[names(ucl_league_table) == "ucl_total_wins"] <- "W"
names(ucl_league_table)[names(ucl_league_table) == "ucl_total_draws"] <- "D"
names(ucl_league_table)[names(ucl_league_table) == "ucl_total_loss"] <- "L"
names(ucl_league_table)[names(ucl_league_table) == "ucl_GS"] <- "F"
names(ucl_league_table)[names(ucl_league_table) == "ucl_GC"] <- "A"
points_ucl <- ucl_league_table[order(as.numeric(ucl_league_table$ucl_PTS), decreasing = TRUE),]
points_ucl$ucl_rank <- 1:length(ucl_teams)
row.names(points_ucl) <- points_ucl$ucl_rank
#create final_ucl_hf_against with team ranks in brackets
for(ucl_rowhrank in 1:nrow(ucl_form_team_against_h)) {
  for(ucl_colhrank in 1:ncol(ucl_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!ucl_form_team_against_h[ucl_rowhrank,ucl_colhrank]=="",ucl_form_team_against_h[ucl_rowhrank,ucl_colhrank] <- paste(ucl_form_team_against_h[ucl_rowhrank,ucl_colhrank],"(",points_ucl$ucl_rank[points_ucl$Team ==ucl_form_team_against_h[ucl_rowhrank,ucl_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
ucl_GP <- nrow(UCL)

#Calculate total home goals for each division
ucl_T_HG <- sum(ucl_home_gs$x)

#calculate average home goal
ucl_avg_HG <- round(ucl_T_HG /ucl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ucl_T_AG <- sum(ucl_away_gs$x)
#calculate average away goal
ucl_avg_AG <- round(ucl_T_AG /ucl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ucl_home_as <- round(((ucl_home_gs$x/ucl_home_games))/ucl_avg_HG, digits = 4)
#calculate away attack strength
ucl_away_as <- round(((ucl_away_gs$x/ucl_away_games))/ucl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
ucl_avg_HC <- round(ucl_T_AG /ucl_GP, digits = 4)
#avg away concede
ucl_avg_AC <- round(ucl_T_HG /ucl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
ucl_home_ds <- round(((ucl_home_gc$x/ucl_home_games))/ucl_avg_HC, digits = 4)
#away defense strength
ucl_away_ds <- round(((ucl_away_gc$x/ucl_away_games))/ucl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_home_poisson <- cbind(ucl_division,ucl_teams,ucl_avg_HG,ucl_home_as,ucl_home_ds)
#################################################################################
#away poisson data
#ucl
ucl_division <- c()
ucl_division[1:length(ucl_teams)] <- "UCL"
ucl_away_poisson <- cbind(ucl_division,ucl_teams,ucl_avg_AG,ucl_away_as,ucl_away_ds)

#UCL
HomeTeam_ucl <- rep(ucl_teams, each = length(ucl_teams))
AwayTeam_ucl <- rep(ucl_teams, length(ucl_teams))
UCL_fixtures <- cbind(HomeTeam_ucl,AwayTeam_ucl)
UCL_fixtures <- as.data.frame(UCL_fixtures)
UCL_fixtures <- UCL_fixtures[!UCL_fixtures$HomeTeam_ucl == UCL_fixtures$AwayTeam_ucl,]
rownames(UCL_fixtures) <- NULL
UCL_fixtures$Div <- "UCL"
UCL_fixtures <- UCL_fixtures[,c(3,1,2)]

UCL_fixtures$avg_HG_ucl <- ucl_avg_HG

UCL_fixtures$ucl_homeas <- rep(ucl_home_as,each = length(ucl_teams)-1)

ucl_awayds_lookup <- cbind(ucl_teams,ucl_away_ds)

ucl_awayds_lookup <- as.data.frame(ucl_awayds_lookup)

colnames(ucl_awayds_lookup) <- c("AwayTeam_ucl","ucl_awayds")


require('RH2')
UCL_fixtures$ucl_awayds <- sqldf("SELECT ucl_awayds_lookup.ucl_awayds FROM ucl_awayds_lookup INNER JOIN UCL_fixtures ON ucl_awayds_lookup.AwayTeam_ucl = UCL_fixtures.AwayTeam_ucl")

UCL_fixtures$avg_AG_ucl <- ucl_avg_AG

ucl_awayas_lookup <- cbind(ucl_teams,ucl_away_as)

ucl_awayas_lookup <- as.data.frame(ucl_awayas_lookup)

colnames(ucl_awayas_lookup) <- c("AwayTeam_ucl","ucl_awayas")


UCL_fixtures$ucl_awayas <- sqldf("SELECT ucl_awayas_lookup.ucl_awayas FROM ucl_awayas_lookup INNER JOIN UCL_fixtures ON ucl_awayas_lookup.AwayTeam_ucl = UCL_fixtures.AwayTeam_ucl")

UCL_fixtures$ucl_homeds <- rep(ucl_home_ds,each = length(ucl_teams)-1)

UCL_fixtures$ucl_awayds <- as.numeric(unlist(UCL_fixtures$ucl_awayds))
#xGH
UCL_fixtures$ucl_xGH <- UCL_fixtures$avg_HG_ucl * UCL_fixtures$ucl_homeas * UCL_fixtures$ucl_awayds

#xGA

UCL_fixtures$ucl_awayas <- as.numeric(unlist(UCL_fixtures$ucl_awayas))

UCL_fixtures$ucl_xGA <- UCL_fixtures$avg_AG_ucl * UCL_fixtures$ucl_awayas * UCL_fixtures$ucl_homeds

UCL_fixtures$ucl_0_0 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_0 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_0_1 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_1 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_0 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_0_2 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_2 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_1 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_2 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_3 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_0 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_1 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_2 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_0_3 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_3 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_3 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_4 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_0 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_1 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_2 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_3 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_0_4 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_4 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_4 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_4 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_5 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_0 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_1 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_2 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_3 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_4 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_0_5 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_5 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_5 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_5 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_5 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_6 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_0 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(0,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_1 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(1,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_2 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(2,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_3 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(3,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_4 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(4,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_6_5 <- round(stats::dpois(6,UCL_fixtures$ucl_xGH) * stats::dpois(5,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_0_6 <- round(stats::dpois(0,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_1_6 <- round(stats::dpois(1,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_2_6 <- round(stats::dpois(2,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_3_6 <- round(stats::dpois(3,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_4_6 <- round(stats::dpois(4,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
UCL_fixtures$ucl_5_6 <- round(stats::dpois(5,UCL_fixtures$ucl_xGH) * stats::dpois(6,UCL_fixtures$ucl_xGA), digits = 4)
#Home win
UCL_fixtures$ucl_H <- (
  UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
    UCL_fixtures$ucl_5_0 + UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5
)

UCL_fixtures$ucl_H <- percent(UCL_fixtures$ucl_H, accuracy = 0.1)

#Draw
UCL_fixtures$ucl_D <- (

  UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 + UCL_fixtures$ucl_4_4 +
    UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6
)

UCL_fixtures$ucl_D <- percent(UCL_fixtures$ucl_D, accuracy = 0.1)

#Away

UCL_fixtures$ucl_A <- (
  UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
    UCL_fixtures$ucl_0_5 + UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6
)

UCL_fixtures$ucl_A <- percent(UCL_fixtures$ucl_A, accuracy = 0.1)

#ov25
UCL_fixtures$ucl_ov25 <- (
  UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 + UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 + UCL_fixtures$ucl_0_4 +
    UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 + UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_0 +
    UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 + UCL_fixtures$ucl_0_5 +
    UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 + UCL_fixtures$ucl_5_5 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5 + UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 +
    UCL_fixtures$ucl_4_6 + UCL_fixtures$ucl_5_6 + UCL_fixtures$ucl_6_6
)
#un25
UCL_fixtures$ucl_un25 <- (
  UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_0_2
)
#odds
UCL_fixtures$ucl_ov25_odds <- round((1/UCL_fixtures$ucl_ov25),digits = 2)
UCL_fixtures$ucl_un25_odds <- round((1/UCL_fixtures$ucl_un25),digits = 2)

UCL_fixtures$ucl_ov25_odds
UCL_fixtures$ucl_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
UCL_fixtures$ucl_BTTSY <- (
  UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_3_1 + UCL_fixtures$ucl_3_2 +
    UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_1_3 + UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_3_3 + UCL_fixtures$ucl_4_4 +
    UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_3 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 +
    UCL_fixtures$ucl_3_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 +
    UCL_fixtures$ucl_5_4 + UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_6_6 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6
)
#BTTSN
UCL_fixtures$ucl_BTTSN <- (
  UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_0_2 +
    UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_5_0 +
    UCL_fixtures$ucl_0_5 + UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_0_6
)

UCL_fixtures$ucl_BTTSY_odds <- round((1/UCL_fixtures$ucl_BTTSY),digits = 2)
UCL_fixtures$ucl_BTTSN_odds <- round((1/UCL_fixtures$ucl_BTTSN),digits = 2)

UCL_fixtures$ucl_BTTSY <- percent(UCL_fixtures$ucl_BTTSY, accuracy = 0.1)
UCL_fixtures$ucl_BTTSN <- percent(UCL_fixtures$ucl_BTTSN, accuracy = 0.1)
#odds
UCL_fixtures$ucl_BTTSY_odds
UCL_fixtures$ucl_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
UCL_fixtures$ucl_AH_0_H <- (
  UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
    UCL_fixtures$ucl_5_0 +UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5 + UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6
)
#AH_0_A
UCL_fixtures$ucl_AH_0_A <- (
  UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
    UCL_fixtures$ucl_0_5 +UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6 + UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6
)

#odds
UCL_fixtures$ucl_AH_0_H_odds <- round((1/UCL_fixtures$ucl_AH_0_H),digits = 2)
UCL_fixtures$ucl_AH_0_A_odds <- round((1/UCL_fixtures$ucl_AH_0_A),digits = 2)

UCL_fixtures$ucl_AH_0_H_odds
UCL_fixtures$ucl_AH_0_A_odds
#percentages
UCL_fixtures$ucl_AH_0_H <- percent(UCL_fixtures$ucl_AH_0_H, accuracy = 0.1)
UCL_fixtures$ucl_AH_0_A <- percent(UCL_fixtures$ucl_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
UCL_fixtures$ucl_AH_n075_H <- (
  UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
    UCL_fixtures$ucl_5_0 +UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5
)
#AH_n075_A
UCL_fixtures$ucl_AH_n075_A <- (
  UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
    UCL_fixtures$ucl_0_5 +UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6
)

#odds
UCL_fixtures$ucl_AH_n075_H_odds <- round((1/UCL_fixtures$ucl_AH_n075_H),digits = 2)
UCL_fixtures$ucl_AH_n075_A_odds <- round((1/UCL_fixtures$ucl_AH_n075_A),digits = 2)

UCL_fixtures$ucl_AH_n075_H_odds
UCL_fixtures$ucl_AH_n075_A_odds
#percentages
UCL_fixtures$ucl_AH_n075_H <- percent(UCL_fixtures$ucl_AH_n075_H, accuracy = 0.1)
UCL_fixtures$ucl_AH_n075_A <- percent(UCL_fixtures$ucl_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
UCL_fixtures$ucl_AH_075_H <- (
  UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
    UCL_fixtures$ucl_5_0 +UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5 + UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6 + UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_1_2 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_3_4 + UCL_fixtures$ucl_4_5 + UCL_fixtures$ucl_5_6
)
#AH_075_A
UCL_fixtures$ucl_AH_075_A <- (
  UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
    UCL_fixtures$ucl_0_5 +UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6 + UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6 + UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_3 + UCL_fixtures$ucl_5_4 + UCL_fixtures$ucl_6_5
)

#odds
UCL_fixtures$ucl_AH_075_H_odds <- round((1/UCL_fixtures$ucl_AH_075_H),digits = 2)
UCL_fixtures$ucl_AH_075_A_odds <- round((1/UCL_fixtures$ucl_AH_075_A),digits = 2)

UCL_fixtures$ucl_AH_075_H_odds
UCL_fixtures$ucl_AH_075_A_odds
#percentages
UCL_fixtures$ucl_AH_075_H <- percent(UCL_fixtures$ucl_AH_075_H, accuracy = 0.1)
UCL_fixtures$ucl_AH_075_A <- percent(UCL_fixtures$ucl_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
UCL_fixtures$ucl_AH_n125_H <- (
  UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
    UCL_fixtures$ucl_5_0 +UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5
)
#AH_n125_A
UCL_fixtures$ucl_AH_n125_A <- (
  UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
    UCL_fixtures$ucl_0_5 +UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6
)

#odds
UCL_fixtures$ucl_AH_n125_H_odds <- round((1/UCL_fixtures$ucl_AH_n125_H),digits = 2)
UCL_fixtures$ucl_AH_n125_A_odds <- round((1/UCL_fixtures$ucl_AH_n125_A),digits = 2)

UCL_fixtures$ucl_AH_n125_H_odds
UCL_fixtures$ucl_AH_n125_A_odds
#percentages
UCL_fixtures$ucl_AH_n125_H <- percent(UCL_fixtures$ucl_AH_n125_H, accuracy = 0.1)
UCL_fixtures$ucl_AH_n125_A <- percent(UCL_fixtures$ucl_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
UCL_fixtures$ucl_AH_125_H <- (
  UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
    UCL_fixtures$ucl_5_0 +UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
    UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
    UCL_fixtures$ucl_6_5 + UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6 + UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_1_2 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_3_4 + UCL_fixtures$ucl_4_5 + UCL_fixtures$ucl_5_6
)
#AH_125_A
UCL_fixtures$ucl_AH_125_A <- (
  UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
    UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
    UCL_fixtures$ucl_0_5 +UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
    UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
    UCL_fixtures$ucl_5_6 + UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 +
    UCL_fixtures$ucl_4_4 + UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6 + UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_1 +
    UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_3 + UCL_fixtures$ucl_5_4 + UCL_fixtures$ucl_6_5
)

#odds
UCL_fixtures$ucl_AH_125_H_odds <- round((1/UCL_fixtures$ucl_AH_125_H),digits = 2)
UCL_fixtures$ucl_AH_125_A_odds <- round((1/UCL_fixtures$ucl_AH_125_A),digits = 2)

UCL_fixtures$ucl_AH_125_H_odds
UCL_fixtures$ucl_AH_125_A_odds
#percentages
UCL_fixtures$ucl_AH_125_H <- percent(UCL_fixtures$ucl_AH_125_H, accuracy = 0.1)
UCL_fixtures$ucl_AH_125_A <- percent(UCL_fixtures$ucl_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
UCL_fixtures$ucl_ov25 <- percent(UCL_fixtures$ucl_ov25, accuracy = 0.1)

UCL_fixtures$ucl_un25 <- percent(UCL_fixtures$ucl_un25, accuracy = 0.1)
UCL_fixtures$ucl_pscore <- paste(round(UCL_fixtures$ucl_xGH,digits = 0),round(UCL_fixtures$ucl_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
ucl_last_n_games <- 6

#create final_ucl_hf object
final_ucl_hf <- c()
for(index_ucl_hf in 1:length(ucl_teams))
{
  index_ucl_hf <- row.names(ucl_form_h) == ucl_teams[index_ucl_hf]
  form_ucl_hf <- ucl_form_h[index_ucl_hf]
  deleted_form_ucl_hf <- form_ucl_hf[!form_ucl_hf[] == ""]
  l6_form_ucl_hf <- tail(deleted_form_ucl_hf,ucl_last_n_games)
  l6_form_ucl_hf <- paste(l6_form_ucl_hf,collapse = " ")
  final_ucl_hf[index_ucl_hf] <- rbind(paste(ucl_teams[index_ucl_hf],l6_form_ucl_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}

#change column nam
final_ucl_hf <- as.data.frame(final_ucl_hf)
colnames(final_ucl_hf) <- "Form"
#goals scored
#create final_ucl_gs object
final_ucl_gs <- c()
suml6_ucl_gs <- c()
for(index_ucl_gs in 1:length(ucl_teams))
{
  index_ucl_gs <- row.names(ucl_goalscored_h) == ucl_teams[index_ucl_gs]
  form_ucl_gs <- ucl_goalscored_h[index_ucl_gs]
  deleted_form_ucl_gs <- form_ucl_gs[!form_ucl_gs[] == ""]
  l6_form_ucl_gs <- tail(deleted_form_ucl_gs,ucl_last_n_games)
  l6_form_ucl_gs <- as.numeric(l6_form_ucl_gs)
  suml6_ucl_gs[index_ucl_gs] <- sum(l6_form_ucl_gs)
  suml6_ucl_gs[index_ucl_gs] <- paste("(",suml6_ucl_gs[index_ucl_gs],")",sep = "")
  l6_form_ucl_gs <- paste(l6_form_ucl_gs,collapse = " ")
  final_ucl_gs[index_ucl_gs] <- rbind(paste(ucl_teams[index_ucl_gs],l6_form_ucl_gs,suml6_ucl_gs[index_ucl_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
final_ucl_gs
#change column names
final_ucl_gs <- as.data.frame(final_ucl_gs)
colnames(final_ucl_gs) <- "Goals scored"
#goal conceded
#create final_ucl_gc object
final_ucl_gc <- c()
suml6_ucl_gc <- c()
for(index_ucl_gc in 1:length(ucl_teams))
{
  index_ucl_gc <- row.names(ucl_goalconceded_h) == ucl_teams[index_ucl_gc]
  form_ucl_gc <- ucl_goalconceded_h[index_ucl_gc]
  deleted_form_ucl_gc <- form_ucl_gc[!form_ucl_gc[] == ""]
  l6_form_ucl_gc <- tail(deleted_form_ucl_gc,ucl_last_n_games)
  l6_form_ucl_gc <- as.numeric(l6_form_ucl_gc)
  suml6_ucl_gc[index_ucl_gc] <- sum(l6_form_ucl_gc)
  suml6_ucl_gc[index_ucl_gc] <- paste("(",suml6_ucl_gc[index_ucl_gc],")",sep = "")
  l6_form_ucl_gc <- paste(l6_form_ucl_gc,collapse = " ")
  final_ucl_gc[index_ucl_gc] <- rbind(paste(ucl_teams[index_ucl_gc],l6_form_ucl_gc,suml6_ucl_gc[index_ucl_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
#change column names
final_ucl_gc <- as.data.frame(final_ucl_gc)
colnames(final_ucl_gc) <- "Goals conceded"


toString(l6_form_ucl_gc)
#total goals
#create final_ucl_tg object
final_ucl_tg <- c()
suml6_ucl_tg <- c()
for(index_ucl_tg in 1:length(ucl_teams))
{
  index_ucl_tg <- row.names(ucl_totalgoals_h) == ucl_teams[index_ucl_tg]
  form_ucl_tg <- ucl_totalgoals_h[index_ucl_tg]
  deleted_form_ucl_tg <- form_ucl_tg[!form_ucl_tg[] == ""]
  l6_form_ucl_tg <- tail(deleted_form_ucl_tg,ucl_last_n_games)
  l6_form_ucl_tg <- as.numeric(l6_form_ucl_tg)
  suml6_ucl_tg[index_ucl_tg] <- sum(l6_form_ucl_tg)
  suml6_ucl_tg[index_ucl_tg] <- paste("(",suml6_ucl_tg[index_ucl_tg],")",sep = "")
  l6_form_ucl_tg <- paste(l6_form_ucl_tg,collapse = " ")
  final_ucl_tg[index_ucl_tg] <- rbind(paste(ucl_teams[index_ucl_tg],l6_form_ucl_tg,suml6_ucl_tg[index_ucl_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
#change column names
final_ucl_tg <- as.data.frame(final_ucl_tg)
colnames(final_ucl_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_ucl_hf object
final_ucl_cs <- c()
for(index_ucl_cs in 1:length(ucl_teams))
{
  index_ucl_cs <- row.names(ucl_csform_h) == ucl_teams[index_ucl_cs]
  csform_ucl_cs <- ucl_csform_h[index_ucl_cs]
  deleted_csform_ucl_cs <- csform_ucl_cs[!csform_ucl_cs[] == ""]
  l6_csform_ucl_cs <- tail(deleted_csform_ucl_cs,ucl_last_n_games)
  l6_csform_ucl_cs <- paste(l6_csform_ucl_cs,collapse = " ")
  final_ucl_cs[index_ucl_cs] <- rbind(paste(ucl_teams[index_ucl_cs],l6_csform_ucl_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",ucl_teams[index],l6_csform)

}

#change column names
final_ucl_cs <- as.data.frame(final_ucl_cs)
colnames(final_ucl_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_ucl_wm object
final_ucl_wm <- c()
suml6_ucl_wm <- c()
for(index_ucl_wm in 1:length(ucl_teams))
{
  index_ucl_wm <- row.names(ucl_winmargin_h) == ucl_teams[index_ucl_wm]
  form_ucl_wm <- ucl_winmargin_h[index_ucl_wm]
  deleted_form_ucl_wm <- form_ucl_wm[!form_ucl_wm[] == ""]
  l6_form_ucl_wm <- tail(deleted_form_ucl_wm,ucl_last_n_games)
  l6_form_ucl_wm <- as.numeric(l6_form_ucl_wm)
  suml6_ucl_wm[index_ucl_wm] <- sum(l6_form_ucl_wm)
  suml6_ucl_wm[index_ucl_wm] <- paste("(",suml6_ucl_wm[index_ucl_wm],")",sep = "")
  l6_form_ucl_wm <- paste(l6_form_ucl_wm,collapse = " ")
  final_ucl_wm[index_ucl_wm] <- rbind(paste(ucl_teams[index_ucl_wm],l6_form_ucl_wm,suml6_ucl_wm[index_ucl_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
final_ucl_wm
#change column names
final_ucl_wm <- as.data.frame(final_ucl_wm)
colnames(final_ucl_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_ucl_ca object
final_ucl_ca <- c()
suml6_ucl_ca <- c()
for(index_ucl_ca in 1:length(ucl_teams))
{
  index_ucl_ca <- row.names(ucl_coawarded_h) == ucl_teams[index_ucl_ca]
  form_ucl_ca <- ucl_coawarded_h[index_ucl_ca]
  deleted_form_ucl_ca <- form_ucl_ca[!form_ucl_ca[] == ""]
  l6_form_ucl_ca <- tail(deleted_form_ucl_ca,ucl_last_n_games)
  l6_form_ucl_ca <- as.numeric(l6_form_ucl_ca)
  suml6_ucl_ca[index_ucl_ca] <- sum(l6_form_ucl_ca)
  suml6_ucl_ca[index_ucl_ca] <- paste("(",suml6_ucl_ca[index_ucl_ca],")",sep = "")
  l6_form_ucl_ca <- paste(l6_form_ucl_ca,collapse = " ")
  final_ucl_ca[index_ucl_ca] <- rbind(paste(ucl_teams[index_ucl_ca],l6_form_ucl_ca,suml6_ucl_ca[index_ucl_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
final_ucl_ca
#change column names
final_ucl_ca <- as.data.frame(final_ucl_ca)
colnames(final_ucl_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_ucl_ca object
final_ucl_cc <- c()
suml6_ucl_cc <- c()
for(index_ucl_cc in 1:length(ucl_teams))
{
  index_ucl_cc <- row.names(ucl_cornersconceded_h) == ucl_teams[index_ucl_cc]
  form_ucl_cc <- ucl_cornersconceded_h[index_ucl_cc]
  deleted_form_ucl_cc <- form_ucl_cc[!form_ucl_cc[] == ""]
  l6_form_ucl_cc <- tail(deleted_form_ucl_cc,ucl_last_n_games)
  l6_form_ucl_cc <- as.numeric(l6_form_ucl_cc)
  suml6_ucl_cc[index_ucl_cc] <- sum(l6_form_ucl_cc)
  suml6_ucl_cc[index_ucl_cc] <- paste("(",suml6_ucl_cc[index_ucl_cc],")",sep = "")
  l6_form_ucl_cc <- paste(l6_form_ucl_cc,collapse = " ")
  final_ucl_cc[index_ucl_cc] <- rbind(paste(ucl_teams[index_ucl_cc],l6_form_ucl_cc,suml6_ucl_cc[index_ucl_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
final_ucl_cc
#change column names
final_ucl_cc <- as.data.frame(final_ucl_cc)
colnames(final_ucl_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_ucl_cosc <- c()
for(index_ucl_cosc in 1:length(ucl_teams))
{
  index_ucl_cosc <- row.names(ucl_coscform_h) == ucl_teams[index_ucl_cosc]
  coscform_ucl_cosc <- ucl_coscform_h[index_ucl_cosc]
  deleted_coscform_ucl_cosc <- coscform_ucl_cosc[!coscform_ucl_cosc[] == ""]
  l6_coscform_ucl_cosc <- tail(deleted_coscform_ucl_cosc,ucl_last_n_games)
  l6_coscform_ucl_cosc <- paste(l6_coscform_ucl_cosc,collapse = " ")
  final_ucl_cosc[index_ucl_cosc] <- rbind(paste(ucl_teams[index_ucl_cosc],l6_coscform_ucl_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",ucl_teams[index],l6_coscform)

}
final_ucl_cosc
#change column names
final_ucl_cosc <- as.data.frame(final_ucl_cosc)
colnames(final_ucl_cosc) <- "CornersForm"
##################################################
#total corners
#create final_ucl_tcorners object
final_ucl_tcorners <- c()
suml6_ucl_tcorners <- c()
for(index_ucl_tcorners in 1:length(ucl_teams))
{
  index_ucl_tcorners <- row.names(ucl_totalcorners_h) == ucl_teams[index_ucl_tcorners]
  form_ucl_tcorners <- ucl_totalcorners_h[index_ucl_tcorners]
  deleted_form_ucl_tcorners <- form_ucl_tcorners[!form_ucl_tcorners[] == ""]
  l6_form_ucl_tcorners <- tail(deleted_form_ucl_tcorners,ucl_last_n_games)
  l6_form_ucl_tcorners <- as.numeric(l6_form_ucl_tcorners)
  suml6_ucl_tcorners[index_ucl_tcorners] <- sum(l6_form_ucl_tcorners)
  suml6_ucl_tcorners[index_ucl_tcorners] <- paste("(",suml6_ucl_tcorners[index_ucl_tcorners],")",sep = "")
  l6_form_ucl_tcorners <- paste(l6_form_ucl_tcorners,collapse = " ")
  final_ucl_tcorners[index_ucl_tcorners] <- rbind(paste(ucl_teams[index_ucl_tcorners],l6_form_ucl_tcorners,suml6_ucl_tcorners[index_ucl_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
#change column names
final_ucl_tcorners <- as.data.frame(final_ucl_tcorners)
colnames(final_ucl_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_ucl_hf_against
final_ucl_hf_against <- c()
for(index_ucl_hf_against in 1:length(ucl_teams))
{
  index_ucl_hf_against <- row.names(ucl_form_team_against_h) == ucl_teams[index_ucl_hf_against]
  form_ucl_hf_against <- ucl_form_team_against_h[index_ucl_hf_against]
  deleted_form_ucl_hf_against <- form_ucl_hf_against[!form_ucl_hf_against[] == ""]
  l6_form_ucl_hf_against <- tail(deleted_form_ucl_hf_against,ucl_last_n_games)
  l6_form_ucl_hf_against <- paste(l6_form_ucl_hf_against,collapse = " ")
  final_ucl_hf_against[index_ucl_hf_against] <- rbind(paste(ucl_teams[index_ucl_hf_against],l6_form_ucl_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ucl_teams[index],l6_form)

}
final_ucl_hf_against <- as.data.frame(final_ucl_hf_against)
colnames(final_ucl_hf_against) <- "Team against"
#combine the columns
final_ucl_all <- cbind(final_ucl_hf,final_ucl_gs,final_ucl_gc,final_ucl_tg,final_ucl_ca,final_ucl_cc,final_ucl_tcorners,final_ucl_cosc,final_ucl_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#UCL
UCL_sim <- UCL
UCL_sim$matchid <- paste(UCL_sim$HomeTeam,UCL_sim$AwayTeam,sep = "-")
UCL_fixtures$matchid <- paste(UCL_fixtures$HomeTeam_ucl,UCL_fixtures$AwayTeam_ucl,sep = "-")
UCL_fixtures$ucl_FTR <- sapply(UCL_fixtures$ucl_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

UCL_fixtures$ucl_gamestatus <- ifelse(UCL_fixtures$matchid %in% UCL_sim$matchid,"played","notplayed")

ucl_home_wins_sim <- c()
ucl_away_wins_sim <- c()
ucl_home_draws_sim <- c()
ucl_away_draws_sim <- c()
ucl_home_loss_sim <- c()
ucl_away_loss_sim <- c()



for (i_ucl_wins_sim in 1:length(ucl_teams))
{

  ucl_home_wins_sim[i_ucl_wins_sim] <- nrow(UCL_fixtures[UCL_fixtures$HomeTeam_ucl == ucl_teams[i_ucl_wins_sim] & UCL_fixtures$ucl_FTR == "H" & UCL_fixtures$ucl_gamestatus =="notplayed",])
  ucl_away_wins_sim[i_ucl_wins_sim] <- nrow(UCL_fixtures[UCL_fixtures$AwayTeam_ucl == ucl_teams[i_ucl_wins_sim] & UCL_fixtures$ucl_FTR == "A" & UCL_fixtures$ucl_gamestatus == "notplayed",])
  ucl_home_draws_sim[i_ucl_wins_sim] <- nrow(UCL_fixtures[UCL_fixtures$HomeTeam_ucl == ucl_teams[i_ucl_wins_sim] & UCL_fixtures$ucl_FTR == "D" & UCL_fixtures$ucl_gamestatus == "notplayed",])
  ucl_away_draws_sim[i_ucl_wins_sim] <- nrow(UCL_fixtures[UCL_fixtures$AwayTeam_ucl == ucl_teams[i_ucl_wins_sim] & UCL_fixtures$ucl_FTR == "D" & UCL_fixtures$ucl_gamestatus == "notplayed",])
  ucl_home_loss_sim[i_ucl_wins_sim] <- nrow(UCL_fixtures[UCL_fixtures$HomeTeam_ucl == ucl_teams[i_ucl_wins_sim] & UCL_fixtures$ucl_FTR == "A" & UCL_fixtures$ucl_gamestatus == "notplayed",])
  ucl_away_loss_sim[i_ucl_wins_sim] <- nrow(UCL_fixtures[UCL_fixtures$AwayTeam_ucl == ucl_teams[i_ucl_wins_sim] & UCL_fixtures$ucl_FTR == "H" & UCL_fixtures$ucl_gamestatus == "notplayed", ])

}

ucl_total_wins_sim <- ucl_home_wins_sim + ucl_away_wins_sim
ucl_total_draws_sim <- ucl_home_draws_sim + ucl_away_draws_sim
ucl_total_loss_sim <- ucl_home_loss_sim + ucl_away_loss_sim

ucl_home_games_sim <- c()
ucl_away_games_sim <-c()

for (i_ucl_sim in 1:length(ucl_teams))
{

  ucl_home_games_sim[i_ucl_sim] <- nrow(UCL_fixtures[UCL_fixtures$HomeTeam_ucl == ucl_teams[i_ucl_sim] & UCL_fixtures$ucl_gamestatus == "notplayed",])
  ucl_away_games_sim[i_ucl_sim]  <- nrow(UCL_fixtures[UCL_fixtures$AwayTeam_ucl == ucl_teams[i_ucl_sim] & UCL_fixtures$ucl_gamestatus == "notplayed",])

}

ucl_games_played_sim <- ucl_home_games_sim + ucl_away_games_sim

ucl_league_table_sim <- cbind(ucl_teams,ucl_games_played_sim,ucl_total_wins_sim,ucl_total_draws_sim,ucl_total_loss_sim)
ucl_PTS_sim <- (ucl_total_wins_sim*3) + (ucl_total_draws_sim*1)
ucl_league_table_sim <- cbind(ucl_league_table_sim,ucl_PTS_sim)

ucl_games_played_simfinal <- ucl_games_played + ucl_games_played_sim
ucl_total_wins_simfinal <- ucl_total_wins + ucl_total_wins_sim
ucl_total_draws_simfinal <- ucl_total_draws + ucl_total_draws_sim
ucl_total_loss_simfinal <- ucl_total_loss + ucl_total_loss_sim
ucl_PTS_simfinal <- ucl_PTS + ucl_PTS_sim

ucl_league_table_simfinal <- cbind(ucl_teams,ucl_games_played_simfinal,ucl_total_wins_simfinal,ucl_total_draws_simfinal,ucl_total_loss_simfinal,ucl_PTS_simfinal)
ucl_league_table_simfinal <- as.data.frame(ucl_league_table_simfinal)
names(ucl_league_table_simfinal)[names(ucl_league_table_simfinal) == "ucl_teams"] <- "Team_f"
names(ucl_league_table_simfinal)[names(ucl_league_table_simfinal) == "ucl_games_played_simfinal"] <- "P_f"
names(ucl_league_table_simfinal)[names(ucl_league_table_simfinal) == "ucl_total_wins_simfinal"] <- "W_f"
names(ucl_league_table_simfinal)[names(ucl_league_table_simfinal) == "ucl_total_draws_simfinal"] <- "D_f"
names(ucl_league_table_simfinal)[names(ucl_league_table_simfinal) == "ucl_total_loss_simfinal"] <- "L_f"
names(ucl_league_table_simfinal)[names(ucl_league_table_simfinal) == "ucl_PTS_simfinal"] <- "PTS_f"
points_ucl_sim <-  ucl_league_table_simfinal[order(as.numeric(ucl_league_table_simfinal$PTS_f), decreasing = TRUE),]

UCL_notplayed <- UCL_fixtures[UCL_fixtures$ucl_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#UCL
UCL_fixtures$Hometeam_ucl_index <- match(UCL_fixtures$HomeTeam_ucl,ucl_teams)
UCL_fixtures$Awayteam_ucl_index <- match(UCL_fixtures$AwayTeam_ucl,ucl_teams)
ucl_prediction <- c()
ucl_HWM <- c()
ucl_AWM <- c()
ucl_HWMLM <- c()
ucl_AWMLM <- c()
ucl_HY <- c()
ucl_AY <- c()
ucl_HCO <- c()
ucl_ACO <- c()
ucl_HXSC <- c()
ucl_AXSC <- c()
ucl_HYCPF <- c()
ucl_AYCPF <- c()
for(ucl_row in 1:nrow(UCL_fixtures))
{

  ucl_hometeamindex <- UCL_fixtures[ucl_row,"Hometeam_ucl_index"]
  ucl_awayteamindex <- UCL_fixtures[ucl_row,"Awayteam_ucl_index"]
  #analyse team form
  #home team
  ucl_form_vec_ht <- as.vector(ucl_form_h[ucl_hometeamindex,])
  ucl_form_vec_ht[is.na(ucl_form_vec_ht)] <- ""
  ucl_form_vec_ht <- ucl_form_vec_ht[ucl_form_vec_ht != ""]
  ucl_form_vec_ht  <-tail(ucl_form_vec_ht,6)
  ucl_ht_numberof_wins <- length(which(ucl_form_vec_ht == "W"))
  ucl_ht_numberof_draws <- length(which(ucl_form_vec_ht == "D"))
  ucl_ht_numberof_loss <- length(which(ucl_form_vec_ht == "L"))
  #awayteam
  ucl_form_vec_at <- as.vector(ucl_form_h[ucl_awayteamindex,])
  ucl_form_vec_at[is.na(ucl_form_vec_at)] <- ""
  ucl_form_vec_at <- ucl_form_vec_at[ucl_form_vec_at != ""]
  ucl_form_vec_at  <-tail(ucl_form_vec_at,6)
  ucl_at_numberof_wins <- length(which(ucl_form_vec_at == "W"))
  ucl_at_numberof_draws <- length(which(ucl_form_vec_at == "D"))
  ucl_at_numberof_loss <- length(which(ucl_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  ucl_goalscored_vec_ht <- as.vector(ucl_goalscored_h[ucl_hometeamindex,])
  ucl_goalscored_vec_ht[is.na(ucl_goalscored_vec_ht)] <- ""
  ucl_goalscored_vec_ht <- ucl_goalscored_vec_ht[ucl_goalscored_vec_ht != ""]
  ucl_goalscored_vec_ht  <-tail(ucl_goalscored_vec_ht,6)
  ucl_goalscored_vec_ht  <- as.numeric(ucl_goalscored_vec_ht)
  ucl_ht_totalgoalscored <- sum(ucl_goalscored_vec_ht)
  ucl_ht_matches_scoring <- length(which(ucl_goalscored_vec_ht > 0))
  ucl_ht_matches_without_scoring <- length(which(ucl_goalscored_vec_ht == "0"))
  #awayteam
  ucl_goalscored_vec_at <- as.vector(ucl_goalscored_h[ucl_awayteamindex,])
  ucl_goalscored_vec_at[is.na(ucl_goalscored_vec_at)] <- ""
  ucl_goalscored_vec_at <- ucl_goalscored_vec_at[ucl_goalscored_vec_at != ""]
  ucl_goalscored_vec_at  <-tail(ucl_goalscored_vec_at,6)
  ucl_goalscored_vec_at  <- as.numeric(ucl_goalscored_vec_at)
  ucl_at_totalgoalscored <- sum(ucl_goalscored_vec_at)
  ucl_at_matches_scoring <- length(which(ucl_goalscored_vec_at > 0))
  ucl_at_matches_without_scoring <- length(which(ucl_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  ucl_goalconceded_vec_ht <- as.vector(ucl_goalconceded_h[ucl_hometeamindex,])
  ucl_goalconceded_vec_ht[is.na(ucl_goalconceded_vec_ht)] <- ""
  ucl_goalconceded_vec_ht <- ucl_goalconceded_vec_ht[ucl_goalconceded_vec_ht != ""]
  ucl_goalconceded_vec_ht  <-tail(ucl_goalconceded_vec_ht,6)
  ucl_goalconceded_vec_ht  <- as.numeric(ucl_goalconceded_vec_ht)
  ucl_goalconceded_vec_ht
  ucl_ht_totalgoalconceded <- sum(ucl_goalconceded_vec_ht)
  ucl_ht_matches_concede <- length(which(ucl_goalconceded_vec_ht > 0))
  ucl_ht_matches_without_concede <- length(which(ucl_goalconceded_vec_ht == "0"))
  #awayteam
  ucl_goalconceded_vec_at <- as.vector(ucl_goalconceded_h[ucl_awayteamindex,])
  ucl_goalconceded_vec_at[is.na(ucl_goalconceded_vec_at)] <- ""
  ucl_goalconceded_vec_at <- ucl_goalconceded_vec_at[ucl_goalconceded_vec_at != ""]
  ucl_goalconceded_vec_at  <-tail(ucl_goalconceded_vec_at,6)
  ucl_goalconceded_vec_at  <- as.numeric(ucl_goalconceded_vec_at)
  ucl_at_totalgoalconceded <- sum(ucl_goalconceded_vec_at)
  ucl_at_matches_concede <- length(which(ucl_goalconceded_vec_at > 0))
  ucl_at_matches_without_concede <- length(which(ucl_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  ucl_totalgoals_vec_ht <- as.vector(ucl_totalgoals_h[ucl_hometeamindex,])
  ucl_totalgoals_vec_ht[is.na(ucl_totalgoals_vec_ht)] <- ""
  ucl_totalgoals_vec_ht <- ucl_totalgoals_vec_ht[ucl_totalgoals_vec_ht != ""]
  ucl_totalgoals_vec_ht  <-tail(ucl_totalgoals_vec_ht,6)
  ucl_totalgoals_vec_ht  <- as.numeric(ucl_totalgoals_vec_ht)
  ucl_totalgoals_vec_ht
  ucl_ht_totalgoals <- sum(ucl_totalgoals_vec_ht)
  ucl_ht_avgtotalgoals <- (ucl_ht_totalgoals/6)
  ucl_ht_no_of_ov25 <- length(which(ucl_totalgoals_vec_ht >= 3))
  ucl_ht_no_of_un25 <- length(which(ucl_totalgoals_vec_ht <= 2))
  #awayteam
  ucl_totalgoals_vec_at <- as.vector(ucl_totalgoals_h[ucl_awayteamindex,])
  ucl_totalgoals_vec_at[is.na(ucl_totalgoals_vec_at)] <- ""
  ucl_totalgoals_vec_at <- ucl_totalgoals_vec_at[ucl_totalgoals_vec_at != ""]
  ucl_totalgoals_vec_at  <-tail(ucl_totalgoals_vec_at,6)
  ucl_totalgoals_vec_at  <- as.numeric(ucl_totalgoals_vec_at)
  ucl_totalgoals_vec_at
  ucl_at_totalgoals <- sum(ucl_totalgoals_vec_at)
  ucl_at_avgtotalgoals <- (ucl_at_totalgoals/6)
  ucl_at_no_of_ov25 <- length(which(ucl_totalgoals_vec_at >= 3))
  ucl_at_no_of_un25 <- length(which(ucl_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  ucl_winmargin_vec_ht <- as.vector(ucl_winmargin_h[ucl_hometeamindex,])
  ucl_winmargin_vec_ht[is.na(ucl_winmargin_vec_ht)] <- ""
  ucl_winmargin_vec_ht <- ucl_winmargin_vec_ht[ucl_winmargin_vec_ht != ""]
  ucl_winmargin_vec_ht  <-tail(ucl_winmargin_vec_ht,6)
  ucl_winmargin_vec_ht  <- as.numeric(ucl_winmargin_vec_ht)

  ucl_ht_totalwinmargin <- sum(ucl_winmargin_vec_ht)
  ucl_ht_no_of_winmargin_ov0 <- length(which(ucl_winmargin_vec_ht >= 0))
  ucl_ht_no_of_winmargin_ov1 <- length(which(ucl_winmargin_vec_ht >= 1))
  ucl_ht_no_of_winmargin_un0 <- length(which(ucl_winmargin_vec_ht <= 0))
  ucl_ht_no_of_winmargin_un1 <- length(which(ucl_winmargin_vec_ht <= 1))
  #awayteam
  ucl_winmargin_vec_at <- as.vector(ucl_winmargin_h[ucl_awayteamindex,])
  ucl_winmargin_vec_at[is.na(ucl_winmargin_vec_at)] <- ""
  ucl_winmargin_vec_at <- ucl_winmargin_vec_at[ucl_winmargin_vec_at != ""]
  ucl_winmargin_vec_at  <-tail(ucl_winmargin_vec_at,6)
  ucl_winmargin_vec_at  <- as.numeric(ucl_winmargin_vec_at)

  ucl_at_totalwinmargin <- sum(ucl_winmargin_vec_at)
  ucl_at_no_of_winmargin_ov0 <- length(which(ucl_winmargin_vec_at >= 0))
  ucl_at_no_of_winmargin_ov1 <- length(which(ucl_winmargin_vec_at >= 1))
  ucl_at_no_of_winmargin_un0 <- length(which(ucl_winmargin_vec_at <= 0))
  ucl_at_no_of_winmargin_un1 <- length(which(ucl_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  ucl_winmargin_vec_ht_lm <- as.vector(ucl_winmargin_h[ucl_hometeamindex,])
  ucl_winmargin_vec_ht_lm[is.na(ucl_winmargin_vec_ht_lm)] <- ""
  ucl_winmargin_vec_ht_lm <- ucl_winmargin_vec_ht_lm[ucl_winmargin_vec_ht_lm != ""]
  ucl_winmargin_vec_ht_lm  <-tail(ucl_winmargin_vec_ht_lm,1)
  #awayteam
  ucl_winmargin_vec_at_lm <- as.vector(ucl_winmargin_h[ucl_awayteamindex,])
  ucl_winmargin_vec_at_lm[is.na(ucl_winmargin_vec_at_lm)] <- ""
  ucl_winmargin_vec_at_lm <- ucl_winmargin_vec_at_lm[ucl_winmargin_vec_at_lm != ""]
  ucl_winmargin_vec_at_lm  <-tail(ucl_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  ucl_yellowtotals_vec_ht <- as.vector(ucl_yellowtotalsv2[ucl_hometeamindex,])
  ucl_yellowtotals_vec_ht[is.na(ucl_yellowtotals_vec_ht)] <- ""
  ucl_yellowtotals_vec_ht <- ucl_yellowtotals_vec_ht[ucl_yellowtotals_vec_ht != ""]
  ucl_yellowtotals_vec_ht  <-tail(ucl_yellowtotals_vec_ht,1)
  #awayteam
  ucl_yellowtotals_vec_at <- as.vector(ucl_yellowtotalsv2[ucl_awayteamindex,])
  ucl_yellowtotals_vec_at[is.na(ucl_yellowtotals_vec_at)] <- ""
  ucl_yellowtotals_vec_at <- ucl_yellowtotals_vec_at[ucl_yellowtotals_vec_at != ""]
  ucl_yellowtotals_vec_at  <-tail(ucl_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  ucl_cornertotals_vec_ht <- as.vector(ucl_cornertotalsv2[ucl_hometeamindex,])
  ucl_cornertotals_vec_ht[is.na(ucl_cornertotals_vec_ht)] <- ""
  ucl_cornertotals_vec_ht <- ucl_cornertotals_vec_ht[ucl_cornertotals_vec_ht != ""]
  ucl_cornertotals_vec_ht  <-tail(ucl_cornertotals_vec_ht,1)
  #awayteam
  ucl_cornertotals_vec_at <- as.vector(ucl_cornertotalsv2[ucl_awayteamindex,])
  ucl_cornertotals_vec_at[is.na(ucl_cornertotals_vec_at)] <- ""
  ucl_cornertotals_vec_at <- ucl_cornertotals_vec_at[ucl_cornertotals_vec_at != ""]
  ucl_cornertotals_vec_at  <-tail(ucl_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  ucl_xshotsconversion_vec_ht <- as.vector(ucl_shots_analysis[ucl_hometeamindex,])
  ucl_xshotsconversion_vec_ht[is.na(ucl_xshotsconversion_vec_ht)] <- ""
  ucl_xshotsconversion_vec_ht <- ucl_xshotsconversion_vec_ht[ucl_xshotsconversion_vec_ht != ""]
  ucl_xshotsconversion_vec_ht  <-tail(ucl_xshotsconversion_vec_ht,1)
  #awayteam
  ucl_xshotsconversion_vec_at <- as.vector(ucl_shots_analysis[ucl_awayteamindex,])
  ucl_xshotsconversion_vec_at[is.na(ucl_xshotsconversion_vec_at)] <- ""
  ucl_xshotsconversion_vec_at <- ucl_xshotsconversion_vec_at[ucl_xshotsconversion_vec_at != ""]
  ucl_xshotsconversion_vec_at  <-tail(ucl_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  ucl_fouls_conversion_vec_ht <- as.vector(ucl_fouls_conversion[ucl_hometeamindex,])
  ucl_fouls_conversion_vec_ht[is.na(ucl_fouls_conversion_vec_ht)] <- ""
  ucl_fouls_conversion_vec_ht <- ucl_fouls_conversion_vec_ht[ucl_fouls_conversion_vec_ht != ""]
  ucl_fouls_conversion_vec_ht  <-tail(ucl_fouls_conversion_vec_ht,1)
  #awayteam
  ucl_fouls_conversion_vec_at <- as.vector(ucl_fouls_conversion[ucl_awayteamindex,])
  ucl_fouls_conversion_vec_at[is.na(ucl_fouls_conversion_vec_at)] <- ""
  ucl_fouls_conversion_vec_at <- ucl_fouls_conversion_vec_at[ucl_fouls_conversion_vec_at != ""]
  ucl_fouls_conversion_vec_at  <-tail(ucl_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  ucl_ht_last6points <- ucl_ht_numberof_wins*3 + ucl_ht_numberof_draws*1
  ucl_at_last6points <- ucl_at_numberof_wins*3 + ucl_at_numberof_draws*1

  if(ucl_ht_last6points > ucl_at_last6points) {ucl_3waypick <- "1"}  else {ucl_3waypick <- "X2"}

  if(ucl_at_last6points > ucl_ht_last6points ) {ucl_3waypick <- "2"} else {ucl_3waypick <- "1X"}

  if(ucl_ht_no_of_ov25 + ucl_at_no_of_ov25 >= 6) {ucl_goalspick <- "ov25"} else {ucl_goalspick <- "un25"}

  if(ucl_ht_no_of_un25 + ucl_at_no_of_un25 >= 6) {ucl_goalspick <- "un25"} else {ucl_goalspick <- "ov25"}

  if(ucl_ht_matches_scoring >= 4 && ucl_at_matches_scoring >=4) {ucl_btts <- "BTTS-Y"} else {ucl_btts <- "BTTS-N"}


  ucl_prediction[ucl_row] <- rbind(paste(ucl_3waypick,ucl_goalspick,ucl_btts,sep = ","))
  ucl_HWM[ucl_row] <- ucl_ht_totalwinmargin
  ucl_AWM[ucl_row] <- ucl_at_totalwinmargin

  ucl_HWMLM[ucl_row] <- ucl_winmargin_vec_ht_lm
  ucl_AWMLM[ucl_row] <- ucl_winmargin_vec_at_lm

  ucl_HY[ucl_row] <- ucl_yellowtotals_vec_ht
  ucl_AY[ucl_row] <- ucl_yellowtotals_vec_at

  ucl_HCO[ucl_row] <- ucl_cornertotals_vec_ht
  ucl_ACO[ucl_row] <- ucl_cornertotals_vec_at

  ucl_HXSC[ucl_row] <- ucl_xshotsconversion_vec_ht
  ucl_AXSC[ucl_row] <- ucl_xshotsconversion_vec_at

  ucl_HYCPF[ucl_row] <- ucl_fouls_conversion_vec_ht
  ucl_AYCPF[ucl_row] <- ucl_fouls_conversion_vec_at
}

ucl_prediction <- as.data.frame(ucl_prediction)
colnames(ucl_prediction) <- "prediction"

ucl_HWM <- as.data.frame(ucl_HWM)
colnames(ucl_HWM) <- "HWM"

ucl_AWM <- as.data.frame(ucl_AWM)
colnames(ucl_AWM) <- "AWM"

ucl_HWMLM <- as.data.frame(ucl_HWMLM)
colnames(ucl_HWMLM) <- "HWMLM"

ucl_AWMLM <- as.data.frame(ucl_AWMLM)
colnames(ucl_AWMLM) <- "AWMLM"

ucl_HY <- as.data.frame(ucl_HY)
colnames(ucl_HY) <- "AVGHY"

ucl_AY <- as.data.frame(ucl_AY)
colnames(ucl_AY) <- "AVGAY"

ucl_HCO <- as.data.frame(ucl_HCO)
colnames(ucl_HCO) <- "AVGHCO"

ucl_ACO <- as.data.frame(ucl_ACO)
colnames(ucl_ACO) <- "AVGACO"

ucl_HXSC <- as.data.frame(ucl_HXSC)
colnames(ucl_HXSC) <- "HXSC"

ucl_AXSC <- as.data.frame(ucl_AXSC)
colnames(ucl_AXSC) <- "AXSC"

ucl_HYCPF <- as.data.frame(ucl_HYCPF)
colnames(ucl_HYCPF) <- "HYCPF"

ucl_AYCPF <- as.data.frame(ucl_AYCPF)
colnames(ucl_AYCPF) <- "AYCPF"

ucl_picks <- cbind(UCL_fixtures$Div,UCL_fixtures$HomeTeam_ucl,UCL_fixtures$AwayTeam_ucl,ucl_prediction,ucl_HWM,ucl_AWM,ucl_HWMLM,ucl_AWMLM,ucl_HY,ucl_AY,ucl_HCO,ucl_ACO,ucl_HXSC,ucl_AXSC,ucl_HYCPF,ucl_AYCPF)

colnames(ucl_picks)[1] <- "picks_Div"
colnames(ucl_picks)[2] <- "picks_HomeTeam"
colnames(ucl_picks)[3] <- "picks_AwayTeam"
ucl_picks$matchid <- paste(ucl_picks$picks_HomeTeam,ucl_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of UCL
ucl_picks
#############################################################################################################################################################################
#clone fixtures
UCL_fixtures_clone <- UCL_fixtures
colnames(UCL_fixtures_clone)[61] <- "Hwin"
colnames(UCL_fixtures_clone)[62] <- "Draw"
colnames(UCL_fixtures_clone)[63] <- "Awin"

UCL_fixtures_clone$Hwinodds <-   UCL_fixtures$ucl_1_0 + UCL_fixtures$ucl_2_0 + UCL_fixtures$ucl_2_1 + UCL_fixtures$ucl_3_0 + UCL_fixtures$ucl_3_1 +
  UCL_fixtures$ucl_3_2 + UCL_fixtures$ucl_4_0 + UCL_fixtures$ucl_4_1 + UCL_fixtures$ucl_4_2 + UCL_fixtures$ucl_4_3 +
  UCL_fixtures$ucl_5_0 + UCL_fixtures$ucl_5_1 + UCL_fixtures$ucl_5_2 + UCL_fixtures$ucl_5_3 + UCL_fixtures$ucl_5_4 +
  UCL_fixtures$ucl_6_0 + UCL_fixtures$ucl_6_1 + UCL_fixtures$ucl_6_2 + UCL_fixtures$ucl_6_3 + UCL_fixtures$ucl_6_4 +
  UCL_fixtures$ucl_6_5
UCL_fixtures_clone$Hwinodds <- round(1/UCL_fixtures_clone$Hwinodds, digits = 3)

UCL_fixtures_clone$Drawodds <-  UCL_fixtures$ucl_0_0 + UCL_fixtures$ucl_1_1 + UCL_fixtures$ucl_2_2 + UCL_fixtures$ucl_3_3 + UCL_fixtures$ucl_4_4 +
  UCL_fixtures$ucl_5_5 + UCL_fixtures$ucl_6_6

UCL_fixtures_clone$Drawodds <- round(1/UCL_fixtures_clone$Drawodds, digits = 3)

UCL_fixtures_clone$Awinodds <-   UCL_fixtures$ucl_0_1 + UCL_fixtures$ucl_0_2 + UCL_fixtures$ucl_1_2 + UCL_fixtures$ucl_0_3 + UCL_fixtures$ucl_1_3 +
  UCL_fixtures$ucl_2_3 + UCL_fixtures$ucl_0_4 + UCL_fixtures$ucl_1_4 + UCL_fixtures$ucl_2_4 + UCL_fixtures$ucl_3_4 +
  UCL_fixtures$ucl_0_5 + UCL_fixtures$ucl_1_5 + UCL_fixtures$ucl_2_5 + UCL_fixtures$ucl_3_5 + UCL_fixtures$ucl_4_5 +
  UCL_fixtures$ucl_0_6 + UCL_fixtures$ucl_1_6 + UCL_fixtures$ucl_2_6 + UCL_fixtures$ucl_3_6 + UCL_fixtures$ucl_4_6 +
  UCL_fixtures$ucl_5_6

UCL_fixtures_clone$Awinodds <- round(1/UCL_fixtures_clone$Awinodds, digits = 3)

colnames(UCL_fixtures_clone)[15] <- "CS_1-1"
colnames(UCL_fixtures_clone)[13] <- "CS_1-0"
colnames(UCL_fixtures_clone)[14] <- "CS_0-1"
colnames(UCL_fixtures_clone)[16] <- "CS_2-0"
colnames(UCL_fixtures_clone)[17] <- "CS_0-2"
colnames(UCL_fixtures_clone)[19] <- "CS_2-1"
colnames(UCL_fixtures_clone)[20] <- "CS_1-2"

UCL_fixtures_clone$`CS_1-1` <- round(1/UCL_fixtures_clone$`CS_1-1`, digits = 3)
UCL_fixtures_clone$`CS_1-0` <- round(1/UCL_fixtures_clone$`CS_1-0`, digits = 3)
UCL_fixtures_clone$`CS_0-1` <- round(1/UCL_fixtures_clone$`CS_0-1`, digits = 3)
UCL_fixtures_clone$`CS_2-0` <- round(1/UCL_fixtures_clone$`CS_2-0`, digits = 3)
UCL_fixtures_clone$`CS_0-2` <- round(1/UCL_fixtures_clone$`CS_0-2`, digits = 3)
UCL_fixtures_clone$`CS_2-1` <- round(1/UCL_fixtures_clone$`CS_2-1`, digits = 3)
UCL_fixtures_clone$`CS_1-2` <- round(1/UCL_fixtures_clone$`CS_1-2`, digits = 3)

colnames(UCL_fixtures_clone)[1] <- "league"
colnames(UCL_fixtures_clone)[2] <- "Hometeam"
colnames(UCL_fixtures_clone)[3] <- "Awayteam"
colnames(UCL_fixtures_clone)[92] <- "predscore"
colnames(UCL_fixtures_clone)[64] <- "ov25"
colnames(UCL_fixtures_clone)[66] <- "ov25odds"
colnames(UCL_fixtures_clone)[65] <- "un25"
colnames(UCL_fixtures_clone)[67] <- "un25odds"
colnames(UCL_fixtures_clone)[68] <- "BTTSY"
colnames(UCL_fixtures_clone)[69] <- "BTTSN"
colnames(UCL_fixtures_clone)[70] <- "BTTSYodds"
colnames(UCL_fixtures_clone)[71] <- "BTTSNodds"

UCL_fixtures_clone <- UCL_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
UCL_fixtures_clone$matchid <- paste(UCL_fixtures_clone$Hometeam,UCL_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
UCL_fixtures_clone_final <- UCL_fixtures_clone[,-c(8,9,10,27)]
UCL_fixtures_clone_final[,'sep'] <- ''

ucl_dmprediction <-  ucl_picks[,c(4,5,6,7,8)]
ucl_dmprediction[,'sep2'] <- ''

ucl_avgyellow <- ucl_picks[,c(9,10)]
ucl_avgyellow[,'sep3'] <- ''

ucl_avgcorners <- ucl_picks[,c(11,12)]
ucl_avgcorners[,'sep4'] <- ''

ucl_goals <- UCL_fixtures[,c(10,11)]
ucl_goals$ucl_xGH <- round(ucl_goals$ucl_xGH, digits = 2)
ucl_goals$ucl_xGA <- round(ucl_goals$ucl_xGA, digits = 2)
ucl_goals$ucl_TxG <- ucl_goals$ucl_xGH + ucl_goals$ucl_xGA
ucl_goals[,'sep5'] <- ''

ucl_shots <- UCL_fixtures_sot[,c(10,11)]
ucl_shots$ucl_xHST <- round(ucl_shots$ucl_xHST, digits = 2)
ucl_shots$ucl_xAST <- round(ucl_shots$ucl_xAST, digits = 2)
ucl_shots$TxSOT <- ucl_shots$ucl_xHST + ucl_shots$ucl_xAST
ucl_shots[,'sep6'] <- ''

ucl_fouls <- UCL_fixtures_fo[,c(10,11)]
ucl_fouls$ucl_xHF <- round(ucl_fouls$ucl_xHF, digits = 2)
ucl_fouls$ucl_xAF <- round(ucl_fouls$ucl_xAF, digits = 2)
ucl_fouls$ucl_TxF <- ucl_fouls$ucl_xHF + ucl_fouls$ucl_xAF

ucl_ycpf <- ucl_picks[,c(15,16)]
ucl_fouls <- cbind(ucl_fouls,ucl_ycpf)
ucl_fouls$HYCPF <- as.numeric(ucl_fouls$HYCPF)
ucl_fouls$AYCPF <- as.numeric(ucl_fouls$AYCPF)
ucl_fouls$x_hyc <- (ucl_fouls$ucl_xHF) * (ucl_fouls$HYCPF)
ucl_fouls$x_ayc <- (ucl_fouls$ucl_xAF) * (ucl_fouls$AYCPF)
ucl_fouls$x_TYC <- round((ucl_fouls$x_hyc + ucl_fouls$x_ayc),digits = 2)
ucl_fouls[,'sep7'] <- ''

ucl_bookings <- UCL_fixtures_yc[,c(10,11)]
ucl_bookings$ucl_xHYC <- round(ucl_bookings$ucl_xHYC, digits = 2)
ucl_bookings$ucl_xAYC <- round(ucl_bookings$ucl_xAYC, digits = 2)
ucl_bookings$ucl_TYcards <- ucl_bookings$ucl_xHYC + ucl_bookings$ucl_xAYC
ucl_bookings[,'sep8'] <- ''

ucl_corners <- UCL_fixtures_co[,c(10,11)]
ucl_corners$ucl_xHCOC <- round(ucl_corners$ucl_xHCOC, digits = 2)
ucl_corners$ucl_xACOC <- round(ucl_corners$ucl_xACOC, digits = 2)
ucl_corners$ucl_TCOs <- ucl_corners$ucl_xHCOC + ucl_corners$ucl_xACOC
ucl_corners[,'sep9'] <- ''

ucl_shotsconversion <- ucl_picks[,c(13,14)]
ucl_shotsconversion <- cbind(ucl_shotsconversion,ucl_shots)
ucl_shotsconversion$HXSC <- as.numeric(ucl_shotsconversion$HXSC)
ucl_shotsconversion$AXSC <- as.numeric(ucl_shotsconversion$AXSC)
ucl_shotsconversion$ucl_hXgoals <- round((ucl_shotsconversion$HXSC * ucl_shotsconversion$ucl_xHST), digits = 2)
ucl_shotsconversion$ucl_aXgoals <- round((ucl_shotsconversion$AXSC * ucl_shotsconversion$ucl_xAST), digits = 2)
ucl_shotsconversion$Xgoals <- ucl_shotsconversion$ucl_hXgoals + ucl_shotsconversion$ucl_aXgoals
options(java.parameters = "-Xmx4g")
UCL_all <- cbind(UCL_fixtures_clone_final,ucl_dmprediction,ucl_avgyellow,ucl_avgcorners,ucl_goals,ucl_shots,ucl_fouls,ucl_bookings,ucl_corners,ucl_shotsconversion)
unlink('Divisions/UCL.xlsx')
write.xlsx(UCL_all,'Divisions/UCL.xlsx', sheetName = "UCL_all", append = TRUE)
write.xlsx(points_ucl,'Divisions/UCL.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(ucl_cornertotalsv2,'Divisions/UCL.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(ucl_goaltotalsv2,'Divisions/UCL.xlsx', sheetName = "Goaltotals", append = TRUE)
write.xlsx(ucl_yellowtotalsv2,'Divisions/UCL.xlsx', sheetName = "Yellowtotals", append = TRUE)


##write.csv(UCL_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
