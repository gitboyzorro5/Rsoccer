#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
e0_currentround
#first_df <- I1_rounds[I1_rounds$i1_matchday > 7,]
#second_df <- SC0_rounds[SC0_rounds$sc0_matchday > 7,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#EPL <- rbind(first_df,second_df)
EPL <- E0_rounds[E0_rounds$e0_matchday >= 13,]
#EPL <- na.omit(EPL)
#goaltotals v2
epl_goaltotalsv2 <- tapply(EPL$TG, EPL[c("HomeTeam", "AwayTeam")],mean)
epl_hgtotals <- rowSums(epl_goaltotalsv2, na.rm = T)
epl_agtotals <- colSums(epl_goaltotalsv2, na.rm = T)
epl_goaltotalsv2 <- cbind(epl_goaltotalsv2,epl_hgtotals,epl_agtotals)
epl_totalgoals <- epl_hgtotals + epl_agtotals
epl_goaltotalsv2 <- cbind(epl_goaltotalsv2,epl_totalgoals)
epl_teams <- sort(unique(EPL$HomeTeam))
epl_home_games <- c()
epl_away_games <-c()
for (i_epl in 1:length(epl_teams))
{

  epl_home_games[i_epl] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl],])
  epl_away_games[i_epl]  <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl],])

}
epl_games_played <- epl_home_games + epl_away_games
epl_goaltotalsv2 <- cbind(epl_goaltotalsv2,epl_games_played)
epl_avg_totalgoals <- round((epl_totalgoals/ epl_games_played), digits = 4)
epl_goaltotalsv2[is.na(epl_goaltotalsv2)] <- ""
epl_goaltotalsv2 <- cbind(epl_goaltotalsv2,epl_avg_totalgoals)

############################################################################################################
#Cornertotals v2
epl_cornertotalsv2 <- tapply(EPL$TC, EPL[c("HomeTeam", "AwayTeam")],mean)
epl_hcototals <- rowSums(epl_cornertotalsv2, na.rm = T)
epl_acototals <- colSums(epl_cornertotalsv2, na.rm = T)
epl_cornertotalsv2 <- cbind(epl_cornertotalsv2,epl_hcototals,epl_acototals)
epl_totalcorners <- epl_hcototals + epl_acototals
epl_cornertotalsv2 <- cbind(epl_cornertotalsv2,epl_totalcorners)
epl_cornertotalsv2 <- cbind(epl_cornertotalsv2,epl_games_played)
epl_avg_totalcorners <- round((epl_totalcorners/ epl_games_played), digits = 4)
epl_cornertotalsv2[is.na(epl_cornertotalsv2)] <- ""
epl_cornertotalsv2 <- cbind(epl_cornertotalsv2,epl_avg_totalcorners)
############################################################################################################
#GS matrix
epl_goalscored_h <- tapply(EPL$FTHG, EPL[c("HomeTeam", "Date")],mean)
epl_goalscored_a <- tapply(EPL$FTAG, EPL[c("AwayTeam", "Date")],mean)
epl_goalscored_h[is.na(epl_goalscored_h)] <- ""
epl_goalscored_a[is.na(epl_goalscored_a)] <- ""
for(epl_rowhgs in 1:nrow(epl_goalscored_h)) {
  for(epl_colhgs in 1:ncol(epl_goalscored_h)) {

    # print(my_matrix[row, col])
    for(epl_rowags in 1:nrow(epl_goalscored_a)) {
      for(epl_colags in 1:ncol(epl_goalscored_a)) {
        ifelse(!epl_goalscored_a[epl_rowags,epl_colags]=="",epl_goalscored_h[epl_rowags,epl_colags] <- epl_goalscored_a[epl_rowags,epl_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
epl_goalconceded_h <- tapply(EPL$FTAG, EPL[c("HomeTeam", "Date")],mean)
epl_goalconceded_a <- tapply(EPL$FTHG, EPL[c("AwayTeam", "Date")],mean)
epl_goalconceded_h[is.na(epl_goalconceded_h)] <- ""
epl_goalconceded_a[is.na(epl_goalconceded_a)] <- ""
for(epl_rowhgc in 1:nrow(epl_goalconceded_h)) {
  for(epl_colhgc in 1:ncol(epl_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(epl_rowagc in 1:nrow(epl_goalconceded_a)) {
      for(epl_colagc in 1:ncol(epl_goalconceded_a)) {
        ifelse(!epl_goalconceded_a[epl_rowagc,epl_colagc]=="",epl_goalconceded_h[epl_rowagc,epl_colagc] <- epl_goalconceded_a[epl_rowagc,epl_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
epl_totalcorners_h <- tapply(EPL$TC, EPL[c("HomeTeam", "Date")],mean)
epl_totalcorners_a <- tapply(EPL$TC, EPL[c("AwayTeam", "Date")],mean)
epl_totalcorners_h[is.na(epl_totalcorners_h)] <- ""
epl_totalcorners_a[is.na(epl_totalcorners_a)] <- ""
#EPL
for(epl_rowTC in 1:nrow(epl_totalcorners_h)) {
  for(epl_colTC in 1:ncol(epl_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(epl_rowTC in 1:nrow(epl_totalcorners_a)) {
      for(epl_colTC in 1:ncol(epl_totalcorners_a)) {
        ifelse(!epl_totalcorners_a[epl_rowTC,epl_colTC]=="",epl_totalcorners_h[epl_rowTC,epl_colTC] <- epl_totalcorners_a[epl_rowTC,epl_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
epl_coawarded_h <- tapply(EPL$HCO, EPL[c("HomeTeam", "Date")],mean)
epl_coawarded_a <- tapply(EPL$ACO, EPL[c("AwayTeam", "Date")],mean)
epl_coawarded_h[is.na(epl_coawarded_h)] <- ""
epl_coawarded_a[is.na(epl_coawarded_a)] <- ""
#EPL
for(epl_rowhco in 1:nrow(epl_coawarded_h)) {
  for(epl_colhco in 1:ncol(epl_coawarded_h)) {

    # print(my_matrix[row, col])
    for(epl_rowaco in 1:nrow(epl_coawarded_a)) {
      for(epl_colaco in 1:ncol(epl_coawarded_a)) {
        ifelse(!epl_coawarded_a[epl_rowaco,epl_colaco]=="",epl_coawarded_h[epl_rowaco,epl_colaco] <- epl_coawarded_a[epl_rowaco,epl_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
epl_cornersconceded_h <- tapply(EPL$ACO, EPL[c("HomeTeam", "Date")],mean)
epl_cornersconceded_a <- tapply(EPL$HCO, EPL[c("AwayTeam", "Date")],mean)
epl_cornersconceded_h[is.na(epl_cornersconceded_h)] <- ""
epl_cornersconceded_a[is.na(epl_cornersconceded_a)] <- ""
#EPL
for(epl_rowhcc in 1:nrow(epl_cornersconceded_h)) {
  for(epl_colhcc in 1:ncol(epl_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(epl_rowacc in 1:nrow(epl_cornersconceded_a)) {
      for(epl_colacc in 1:ncol(epl_cornersconceded_a)) {
        ifelse(!epl_cornersconceded_a[epl_rowacc,epl_colacc]=="",epl_cornersconceded_h[epl_rowacc,epl_colacc] <- epl_cornersconceded_a[epl_rowacc,epl_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
epl_coscform_h <- tapply(EPL$COSC, EPL[c("HomeTeam", "Date")],median)
epl_coscform_a <- tapply(EPL$COSC, EPL[c("AwayTeam", "Date")],median)
epl_coscform_h[is.na(epl_coscform_h)] <- ""
epl_coscform_a[is.na(epl_coscform_a)] <- ""
#EPL
for(epl_rowh_f_cosc in 1:nrow(epl_coscform_h)) {
  for(epl_colh_f_cosc in 1:ncol(epl_coscform_h)) {

    # print(my_matrix[row, col])
    for(epl_rowa_f_cosc in 1:nrow(epl_coscform_a)) {
      for(epl_cola_f_cosc in 1:ncol(epl_coscform_a)) {
        ifelse(!epl_coscform_a[epl_rowa_f_cosc,epl_cola_f_cosc]=="",epl_coscform_h[epl_rowa_f_cosc,epl_cola_f_cosc] <- epl_coscform_a[epl_rowa_f_cosc,epl_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
epl_winmargin_h <- tapply(EPL$FTHG - EPL$FTAG, EPL[c("HomeTeam", "Date")],mean)
epl_winmargin_a <- tapply(EPL$FTAG - EPL$FTHG, EPL[c("AwayTeam", "Date")],mean)
epl_winmargin_h[is.na(epl_winmargin_h)] <- ""
epl_winmargin_a[is.na(epl_winmargin_a)] <- ""
#EPL
for(epl_rowhwm in 1:nrow(epl_winmargin_h)) {
  for(epl_colhwm in 1:ncol(epl_winmargin_h)) {

    # print(my_matrix[row, col])
    for(epl_rowawm in 1:nrow(epl_winmargin_a)) {
      for(epl_colawm in 1:ncol(epl_winmargin_a)) {
        ifelse(!epl_winmargin_a[epl_rowawm,epl_colawm]=="",epl_winmargin_h[epl_rowawm,epl_colawm] <- epl_winmargin_a[epl_rowawm,epl_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
epl_yellowscored_h <- tapply(EPL$HY, EPL[c("HomeTeam", "Date")],mean)
epl_yellowscored_a <- tapply(EPL$AY, EPL[c("AwayTeam", "Date")],mean)
epl_yellowscored_h[is.na(epl_yellowscored_h)] <- ""
epl_yellowscored_a[is.na(epl_yellowscored_a)] <- ""
#EPL
for(epl_rowhys in 1:nrow(epl_yellowscored_h)) {
  for(epl_colhys in 1:ncol(epl_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(epl_roways in 1:nrow(epl_yellowscored_a)) {
      for(epl_colays in 1:ncol(epl_yellowscored_a)) {
        ifelse(!epl_yellowscored_a[epl_roways,epl_colays]=="",epl_yellowscored_h[epl_roways,epl_colays] <- epl_yellowscored_a[epl_roways,epl_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
epl_redscored_h <- tapply(EPL$HR, EPL[c("HomeTeam", "Date")],mean)
epl_redscored_a <- tapply(EPL$AR, EPL[c("AwayTeam", "Date")],mean)
epl_redscored_h[is.na(epl_redscored_h)] <- ""
epl_redscored_a[is.na(epl_redscored_a)] <- ""
for(epl_rowhrs in 1:nrow(epl_redscored_h)) {
  for(epl_colhrs in 1:ncol(epl_redscored_h)) {

    # print(my_matrix[row, col])
    for(epl_rowars in 1:nrow(epl_redscored_a)) {
      for(epl_colars in 1:ncol(epl_redscored_a)) {
        ifelse(!epl_redscored_a[epl_rowars,epl_colars]=="",epl_redscored_h[epl_rowars,epl_colars] <- epl_redscored_a[epl_rowars,epl_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
epl_redtotalsv2 <- tapply(EPL$TR, EPL[c("HomeTeam", "AwayTeam")],mean)
epl_hrtotals <- rowSums(epl_redtotalsv2, na.rm = T)
epl_artotals <- colSums(epl_redtotalsv2, na.rm = T)
epl_redtotalsv2 <- cbind(epl_redtotalsv2,epl_hrtotals,epl_artotals)
epl_totalreds <- epl_hrtotals + epl_artotals
epl_redtotalsv2 <- cbind(epl_redtotalsv2,epl_totalreds)
epl_redtotalsv2 <- cbind(epl_redtotalsv2,epl_games_played)
epl_avg_totalreds <- round((epl_totalreds/ epl_games_played), digits = 4)
epl_redtotalsv2[is.na(epl_redtotalsv2)] <- ""
epl_redtotalsv2 <- cbind(epl_redtotalsv2,epl_avg_totalreds)
############################################################################################################################################################
#yellowtotals
epl_yellowtotalsv2 <- tapply(EPL$TY, EPL[c("HomeTeam", "AwayTeam")],mean)
epl_hytotals <- rowSums(epl_yellowtotalsv2, na.rm = T)
epl_aytotals <- colSums(epl_yellowtotalsv2, na.rm = T)
epl_yellowtotalsv2 <- cbind(epl_yellowtotalsv2,epl_hytotals,epl_aytotals)
epl_totalyellows <- epl_hytotals + epl_aytotals
epl_yellowtotalsv2 <- cbind(epl_yellowtotalsv2,epl_totalyellows)
epl_yellowtotalsv2 <- cbind(epl_yellowtotalsv2,epl_games_played)
epl_avg_totalyellows <- round((epl_totalyellows/ epl_games_played), digits = 4)
epl_yellowtotalsv2[is.na(epl_yellowtotalsv2)] <- ""
epl_yellowtotalsv2 <- cbind(epl_yellowtotalsv2,epl_avg_totalyellows)
##################################################################################################################################################
#team form
epl_form_h <- tapply(EPL$FTR, EPL[c("HomeTeam", "Date")],median)
epl_form_a <- tapply(EPL$FTR, EPL[c("AwayTeam", "Date")],median)
epl_form_h[is.na(epl_form_h)] <- ""
epl_form_a[is.na(epl_form_a)] <- ""
epl_form_h <- sub("A","L",epl_form_h)
epl_form_h <- sub("H","W",epl_form_h)
epl_form_a <- sub("A","W",epl_form_a)
epl_form_a <- sub("H","L",epl_form_a)
for(epl_rowh_f in 1:nrow(epl_form_h)) {
  for(epl_colh_f in 1:ncol(epl_form_h)) {

    # print(my_matrix[row, col])
    for(epl_rowa_f in 1:nrow(epl_form_a)) {
      for(epl_cola_f in 1:ncol(epl_form_a)) {
        ifelse(!epl_form_a[epl_rowa_f,epl_cola_f]=="",epl_form_h[epl_rowa_f,epl_cola_f] <- epl_form_a[epl_rowa_f,epl_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
epl_csform_h <- tapply(EPL$CS, EPL[c("HomeTeam", "Date")],median)
epl_csform_a <- tapply(EPL$CS, EPL[c("AwayTeam", "Date")],median)
epl_csform_h[is.na(epl_csform_h)] <- ""
epl_csform_a[is.na(epl_csform_a)] <- ""
#EPL
for(epl_rowh_f_cs in 1:nrow(epl_csform_h)) {
  for(epl_colh_f_cs in 1:ncol(epl_csform_h)) {

    # print(my_matrix[row, col])
    for(epl_rowa_f_cs in 1:nrow(epl_csform_a)) {
      for(epl_cola_f_cs in 1:ncol(epl_csform_a)) {
        ifelse(!epl_csform_a[epl_rowa_f_cs,epl_cola_f_cs]=="",epl_csform_h[epl_rowa_f_cs,epl_cola_f_cs] <- epl_csform_a[epl_rowa_f_cs,epl_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
epl_totalgoals_h <- tapply(EPL$TG, EPL[c("HomeTeam", "Date")],mean)
epl_totalgoals_a <- tapply(EPL$TG, EPL[c("AwayTeam", "Date")],mean)
epl_totalgoals_h[is.na(epl_totalgoals_h)] <- ""
epl_totalgoals_a[is.na(epl_totalgoals_a)] <- ""
for(epl_rowh in 1:nrow(epl_totalgoals_h)) {
  for(epl_colh in 1:ncol(epl_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(epl_rowa in 1:nrow(epl_totalgoals_a)) {
      for(epl_cola in 1:ncol(epl_totalgoals_a)) {
        ifelse(!epl_totalgoals_a[epl_rowa,epl_cola]=="",epl_totalgoals_h[epl_rowa,epl_cola] <- epl_totalgoals_a[epl_rowa,epl_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#EPL
epl_un05_home <- c()
epl_un05_away <- c()
epl_ov05_home <- c()
epl_ov05_away <- c()

epl_un15_home <- c()
epl_un15_away <- c()
epl_ov15_home <- c()
epl_ov15_away <- c()

epl_un25_home <- c()
epl_un25_away <- c()
epl_ov25_home <- c()
epl_ov25_away <- c()

epl_un35_home <- c()
epl_un35_away <- c()
epl_ov35_home <- c()
epl_ov35_away <- c()

epl_un45_home <- c()
epl_un45_away <- c()
epl_ov45_home <- c()
epl_ov45_away <- c()

epl_un55_home <- c()
epl_un55_away <- c()
epl_ov55_home <- c()
epl_ov55_away <- c()

for (i_epl_tg in 1:length(epl_teams))
{

  epl_un05_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG == 0,])
  epl_un05_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG == 0,])

  epl_ov05_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG > 0,])
  epl_ov05_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG > 0,])

  epl_un15_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG <= 1,])
  epl_un15_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG <= 1,])

  epl_ov15_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG >= 2,])
  epl_ov15_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG >= 2,])

  epl_un25_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG <= 2,])
  epl_un25_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG <= 2,])

  epl_ov25_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG >=3,])
  epl_ov25_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG >=3,])

  epl_un35_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG <= 3,])
  epl_un35_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG <= 3,])

  epl_ov35_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG >= 4,])
  epl_ov35_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG >= 4,])

  epl_un45_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG <= 4,])
  epl_un45_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG <= 4,])

  epl_ov45_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG >= 5,])
  epl_ov45_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG >= 5,])

  epl_un55_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG <= 5,])
  epl_un55_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG <= 5,])

  epl_ov55_home[i_epl_tg] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_tg] & EPL$TG >= 6,])
  epl_ov55_away[i_epl_tg] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_tg] & EPL$TG >= 6,])


}

epl_un05 <- epl_un05_home + epl_un05_away
epl_ov05 <- epl_ov05_home + epl_ov05_away

epl_un15 <- epl_un15_home + epl_un15_away
epl_ov15 <- epl_ov15_home + epl_ov15_away

epl_un25 <- epl_un25_home + epl_un25_away
epl_ov25 <- epl_ov25_home + epl_ov25_away

epl_un35 <- epl_un35_home + epl_un35_away
epl_ov35 <- epl_ov35_home + epl_ov35_away

epl_un45 <- epl_un45_home + epl_un45_away
epl_ov45 <- epl_ov45_home + epl_ov45_away

epl_un55 <- epl_un55_home + epl_un55_away
epl_ov55 <- epl_ov55_home + epl_ov55_away

epl_ovundata <- cbind(epl_teams,epl_un05,epl_ov05,epl_un15,epl_ov15,epl_un25,epl_ov25,epl_un35,epl_ov35,epl_un45,epl_ov45,epl_un55,epl_ov55)
#################################################################################################################################################################
#team against
epl_form_team_against_h <- tapply(EPL$AwayTeam, EPL[c("HomeTeam", "Date")],median)
epl_form_team_against_a <- tapply(EPL$HomeTeam, EPL[c("AwayTeam", "Date")],median)
epl_form_team_against_h[is.na(epl_form_team_against_h)] <- ""
epl_form_team_against_a[is.na(epl_form_team_against_a)] <- ""
#EPL
for(epl_rowh_f_against in 1:nrow(epl_form_team_against_h)) {
  for(epl_colh_f_against in 1:ncol(epl_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(epl_rowa_f_against in 1:nrow(epl_form_team_against_a)) {
      for(epl_cola_f_against in 1:ncol(epl_form_team_against_a)) {
        ifelse(!epl_form_team_against_a[epl_rowa_f_against,epl_cola_f_against]=="",epl_form_team_against_h[epl_rowa_f_against,epl_cola_f_against] <- epl_form_team_against_a[epl_rowa_f_against,epl_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#EPL
#home goals scored
epl_home_gs <- aggregate(EPL$FTHG, by = list(EPL$HomeTeam), FUN = sum)
epl_home_gs_avg <- aggregate(EPL$FTHG, by = list(EPL$HomeTeam),mean)
epl_home_scoring <- merge(epl_home_gs,epl_home_gs_avg, by='Group.1',all = T)
names(epl_home_scoring)[names(epl_home_scoring) == "x.x"] <- "TFthg"
names(epl_home_scoring)[names(epl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
epl_away_gs <- aggregate(EPL$FTAG, by = list(EPL$AwayTeam), FUN = sum)
epl_away_gs_avg <- aggregate(EPL$FTAG, by = list(EPL$AwayTeam),mean)
epl_away_scoring <- merge(epl_away_gs,epl_away_gs_avg, by='Group.1',all = T)
names(epl_away_scoring)[names(epl_away_scoring) == "x.x"] <- "TFtag"
names(epl_away_scoring)[names(epl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
epl_scoring <- merge(epl_home_scoring,epl_away_scoring,by='Group.1',all = T)
epl_scoring$TGS <- epl_scoring$TFthg + epl_scoring$TFtag

#Home shots on target
epl_home_hst <- aggregate(EPL$HST, by = list(EPL$HomeTeam), FUN = sum)
epl_away_ast <- aggregate(EPL$AST, by = list(EPL$AwayTeam), FUN = sum)
epl_tst <- merge(epl_home_hst,epl_away_ast, by='Group.1',all = T)
names(epl_tst)[names(epl_tst) == "x.x"] <- "hst"
names(epl_tst)[names(epl_tst) == "x.y"] <- "ast"
epl_tst$TST <- epl_tst$hst + epl_tst$ast
#merge goals scored and shots on target
epl_scoring_conversion <- merge(epl_tst,epl_scoring,by='Group.1',all = T)
#add HSC ASC TSC
epl_scoring_conversion$HSTC <- percent(epl_scoring_conversion$TFthg/epl_scoring_conversion$hst, accuracy = 0.01)
epl_scoring_conversion$ASTC <- percent(epl_scoring_conversion$TFtag/epl_scoring_conversion$ast, accuracy = 0.01)
epl_scoring_conversion$TSTC <- percent(epl_scoring_conversion$TGS/epl_scoring_conversion$TST, accuracy = 0.01)
#merge games played
epl_scoring_conversion <- cbind(epl_scoring_conversion,epl_games_played)
#create the second part
#home goals conceded
epl_home_gc <- aggregate(EPL$FTAG, by = list(EPL$HomeTeam), FUN = sum)
epl_home_gc_avg <- aggregate(EPL$FTAG, by = list(EPL$HomeTeam),mean)
epl_home_conceding <- merge(epl_home_gc,epl_home_gc_avg, by='Group.1',all = T)
names(epl_home_conceding)[names(epl_home_conceding) == "x.x"] <- "TFthc"
names(epl_home_conceding)[names(epl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
epl_away_gc <- aggregate(EPL$FTHG, by = list(EPL$AwayTeam), FUN = sum)
epl_away_gc_avg <- aggregate(EPL$FTHG, by = list(EPL$AwayTeam),mean)
epl_away_conceding <- merge(epl_away_gc,epl_away_gc_avg, by='Group.1',all = T)
names(epl_away_conceding)[names(epl_away_conceding) == "x.x"] <- "TFtac"
names(epl_away_conceding)[names(epl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
epl_conceding <- merge(epl_home_conceding,epl_away_conceding,by='Group.1',all = T)
epl_conceding$TGC <- epl_conceding$TFthc + epl_conceding$TFtac
epl_home_hst
#Home shots conceded
epl_home_hsc <- aggregate(EPL$AST, by = list(EPL$HomeTeam), FUN = sum)
epl_away_asc <- aggregate(EPL$HST, by = list(EPL$AwayTeam), FUN = sum)
epl_tsc <- merge(epl_home_hsc,epl_away_asc, by='Group.1',all = T)
names(epl_tsc)[names(epl_tsc) == "x.x"] <- "hsc"
names(epl_tsc)[names(epl_tsc) == "x.y"] <- "asc"
epl_tsc$TSC <- epl_tsc$hsc + epl_tsc$asc
#merge goals conceded and shots conceded
epl_conceding_conversion <- merge(epl_tsc,epl_conceding,by='Group.1',all = T)

#add HSC ASC TSC
epl_conceding_conversion$HSCC <- percent(epl_conceding_conversion$TFthc/epl_conceding_conversion$hsc, accuracy = 0.01)
epl_conceding_conversion$ASCC <- percent(epl_conceding_conversion$TFtac/epl_conceding_conversion$asc, accuracy = 0.01)
epl_conceding_conversion$TSCC <- percent(epl_conceding_conversion$TGC/epl_conceding_conversion$TSC, accuracy = 0.01)
epl_conceding_conversion$XSTC <- round(epl_scoring$TGS/(epl_tst$TST - epl_scoring$TGS), digits = 2)

#merge the two parts
epl_shots_analysis <- merge(epl_scoring_conversion,epl_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#EPL
#home fouls for
epl_home_fouls <- aggregate(EPL$HF, by = list(EPL$HomeTeam), FUN = sum)
epl_home_fouls_avg <- aggregate(EPL$HF, by = list(EPL$HomeTeam),mean)
epl_home_foulsdata <- merge(epl_home_fouls,epl_home_fouls_avg, by='Group.1',all = T)
names(epl_home_foulsdata)[names(epl_home_foulsdata) == "x.x"] <- "THfouls"
names(epl_home_foulsdata)[names(epl_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
epl_away_fouls <- aggregate(EPL$HF, by = list(EPL$AwayTeam), FUN = sum)
epl_away_fouls_avg <- aggregate(EPL$HF, by = list(EPL$AwayTeam),mean)
epl_away_foulsdata <- merge(epl_away_fouls,epl_away_fouls_avg, by='Group.1',all = T)
names(epl_away_foulsdata)[names(epl_away_foulsdata) == "x.x"] <- "TAfouls"
names(epl_away_foulsdata)[names(epl_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
epl_fouls <- merge(epl_home_foulsdata,epl_away_foulsdata,by='Group.1',all = T)
epl_fouls$TotalFouls <- epl_fouls$THfouls + epl_fouls$TAfouls

#yellow cards
epl_home_hyc <- aggregate(EPL$HY, by = list(EPL$HomeTeam), FUN = sum)
epl_away_ayc <- aggregate(EPL$AY, by = list(EPL$AwayTeam), FUN = sum)
epl_tyc <- merge(epl_home_hyc,epl_away_ayc, by='Group.1',all = T)
names(epl_tyc)[names(epl_tyc) == "x.x"] <- "hyc"
names(epl_tyc)[names(epl_tyc) == "x.y"] <- "ayc"
epl_tyc$TotalYellows <- epl_tyc$hyc + epl_tyc$ayc

#merge fouls for and yellow cards
epl_fouls_conversion <- merge(epl_tyc,epl_fouls,by='Group.1',all = T)
epl_fouls_conversion$YcPerfoul <- round((epl_fouls_conversion$TotalYellows/epl_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
EPL$Div <- "EPL"
##
###################################################################################################################################################
#poisson cards
epl_GP <- nrow(EPL)
#Calculate total home goals for each division
epl_T_HY <- sum(epl_home_hyc$x)
#calculate average home goal
epl_avg_HY <- round(epl_T_HY /epl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
epl_T_AY <- sum(epl_away_ayc$x)
#calculate average away goal
epl_avg_AY <- round(epl_T_AY /epl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
epl_home_yas <- round(((epl_home_hyc$x/epl_home_games))/epl_avg_HY, digits = 4)
#calculate away attack strength
epl_away_yas <- round(((epl_away_ayc$x/epl_away_games))/epl_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
epl_avg_HYC <- round(epl_T_AY /epl_GP, digits = 4)
#avg away concede
epl_avg_AYC <- round(epl_T_HY /epl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
epl_home_ycc <- aggregate(EPL$AY, by = list(EPL$HomeTeam), FUN = sum)
epl_away_ycc <- aggregate(EPL$HY, by = list(EPL$AwayTeam), FUN = sum)
#home defense strength
epl_home_yds <- round(((epl_home_ycc$x/epl_home_games))/epl_avg_HYC, digits = 4)
#away defense strength
epl_away_yds <- round(((epl_away_ycc$x/epl_away_games))/epl_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_home_poisson_yc <- cbind(epl_division,epl_teams,epl_avg_HY,epl_home_yas,epl_home_yds)
#away poisson data
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_away_poisson_yc <- cbind(epl_division,epl_teams,epl_avg_AY,epl_away_yas,epl_away_yds)
###
HomeTeam_epl_yc <- rep(epl_teams, each = length(epl_teams))
AwayTeam_epl_yc <- rep(epl_teams, length(epl_teams))
EPL_fixtures_yc <- cbind(HomeTeam_epl_yc,AwayTeam_epl_yc)
EPL_fixtures_yc <- as.data.frame(EPL_fixtures_yc)
EPL_fixtures_yc <- EPL_fixtures_yc[!EPL_fixtures_yc$HomeTeam_epl_yc == EPL_fixtures_yc$AwayTeam_epl_yc,]
rownames(EPL_fixtures_yc) <- NULL
EPL_fixtures_yc$Div <- "EPL"
EPL_fixtures_yc <- EPL_fixtures_yc[,c(3,1,2)]

EPL_fixtures_yc$avg_HY_epl <- epl_avg_HY

EPL_fixtures_yc$epl_homeyas <- rep(epl_home_yas,each = length(epl_teams)-1)

epl_awayyds_lookup <- cbind(epl_teams,epl_away_yds)

epl_awayyds_lookup <- as.data.frame(epl_awayyds_lookup)

colnames(epl_awayyds_lookup) <- c("AwayTeam_epl_yc","epl_awayyds")


require('RH2')
EPL_fixtures_yc$epl_awayyds <- sqldf("SELECT epl_awayyds_lookup.epl_awayyds FROM epl_awayyds_lookup INNER JOIN EPL_fixtures_yc ON epl_awayyds_lookup.AwayTeam_epl_yc = EPL_fixtures_yc.AwayTeam_epl_yc")

EPL_fixtures_yc$avg_AY_epl <- epl_avg_AY

epl_awayyas_lookup <- cbind(epl_teams,epl_away_yas)

epl_awayyas_lookup <- as.data.frame(epl_awayyas_lookup)

colnames(epl_awayyas_lookup) <- c("AwayTeam_epl_yc","epl_awayyas")

EPL_fixtures_yc$epl_awayyas <- sqldf("SELECT epl_awayyas_lookup.epl_awayyas FROM epl_awayyas_lookup INNER JOIN EPL_fixtures_yc ON epl_awayyas_lookup.AwayTeam_epl_yc = EPL_fixtures_yc.AwayTeam_epl_yc")

EPL_fixtures_yc$epl_homeyds <- rep(epl_home_yds,each = length(epl_teams)-1)

EPL_fixtures_yc$epl_awayyds <- as.numeric(unlist(EPL_fixtures_yc$epl_awayyds))
#xGH
EPL_fixtures_yc$epl_xHYC <- EPL_fixtures_yc$avg_HY_epl * EPL_fixtures_yc$epl_homeyas * EPL_fixtures_yc$epl_awayyds
#xGA

EPL_fixtures_yc$epl_awayyas <- as.numeric(unlist(EPL_fixtures_yc$epl_awayyas))

EPL_fixtures_yc$epl_xAYC <- EPL_fixtures_yc$avg_AY_epl * EPL_fixtures_yc$epl_awayyas * EPL_fixtures_yc$epl_homeyds

EPL_fixtures_yc$epl_0_0 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_0 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_0_1 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_1 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_0 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_0_2 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_2 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_1 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_2 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_3 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_0 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_1 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_2 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_0_3 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_3 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_3 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_4 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_0 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_1 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_2 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_3 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_0_4 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_4 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_4 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_4 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_5 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_0 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_1 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_2 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_3 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_4 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_0_5 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_5 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_5 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_5 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_5 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_6 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_0 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(0,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_1 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(1,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_2 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(2,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_3 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(3,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_4 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(4,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_6_5 <- round(stats::dpois(6,EPL_fixtures_yc$epl_xHYC) * stats::dpois(5,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_0_6 <- round(stats::dpois(0,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_1_6 <- round(stats::dpois(1,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_2_6 <- round(stats::dpois(2,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_3_6 <- round(stats::dpois(3,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_4_6 <- round(stats::dpois(4,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
EPL_fixtures_yc$epl_5_6 <- round(stats::dpois(5,EPL_fixtures_yc$epl_xHYC) * stats::dpois(6,EPL_fixtures_yc$epl_xAYC), digits = 4)
#Home win
EPL_fixtures_yc$epl_H <- (
  EPL_fixtures_yc$epl_1_0 + EPL_fixtures_yc$epl_2_0 + EPL_fixtures_yc$epl_2_1 + EPL_fixtures_yc$epl_3_0 + EPL_fixtures_yc$epl_3_1 +
    EPL_fixtures_yc$epl_3_2 + EPL_fixtures_yc$epl_4_0 + EPL_fixtures_yc$epl_4_1 + EPL_fixtures_yc$epl_4_2 + EPL_fixtures_yc$epl_4_3 +
    EPL_fixtures_yc$epl_5_0 + EPL_fixtures_yc$epl_5_1 + EPL_fixtures_yc$epl_5_2 + EPL_fixtures_yc$epl_5_3 + EPL_fixtures_yc$epl_5_4 +
    EPL_fixtures_yc$epl_6_0 + EPL_fixtures_yc$epl_6_1 + EPL_fixtures_yc$epl_6_2 + EPL_fixtures_yc$epl_6_3 + EPL_fixtures_yc$epl_6_4 +
    EPL_fixtures_yc$epl_6_5
)

EPL_fixtures_yc$epl_H <- percent(EPL_fixtures_yc$epl_H, accuracy = 0.1)

#Draw
EPL_fixtures_yc$epl_D <- (

  EPL_fixtures_yc$epl_0_0 + EPL_fixtures_yc$epl_1_1 + EPL_fixtures_yc$epl_2_2 + EPL_fixtures_yc$epl_3_3 + EPL_fixtures_yc$epl_4_4 +
    EPL_fixtures_yc$epl_5_5 + EPL_fixtures_yc$epl_6_6
)

EPL_fixtures_yc$epl_D <- percent(EPL_fixtures_yc$epl_D, accuracy = 0.1)

#Away

EPL_fixtures_yc$epl_A <- (
  EPL_fixtures_yc$epl_0_1 + EPL_fixtures_yc$epl_0_2 + EPL_fixtures_yc$epl_1_2 + EPL_fixtures_yc$epl_0_3 + EPL_fixtures_yc$epl_1_3 +
    EPL_fixtures_yc$epl_2_3 + EPL_fixtures_yc$epl_0_4 + EPL_fixtures_yc$epl_1_4 + EPL_fixtures_yc$epl_2_4 + EPL_fixtures_yc$epl_3_4 +
    EPL_fixtures_yc$epl_0_5 + EPL_fixtures_yc$epl_1_5 + EPL_fixtures_yc$epl_2_5 + EPL_fixtures_yc$epl_3_5 + EPL_fixtures_yc$epl_4_5 +
    EPL_fixtures_yc$epl_0_6 + EPL_fixtures_yc$epl_1_6 + EPL_fixtures_yc$epl_2_6 + EPL_fixtures_yc$epl_3_6 + EPL_fixtures_yc$epl_4_6 +
    EPL_fixtures_yc$epl_5_6
)

EPL_fixtures_yc$epl_A <- percent(EPL_fixtures_yc$epl_A, accuracy = 0.1)

#ov25
EPL_fixtures_yc$epl_ov25 <- (
  EPL_fixtures_yc$epl_2_1 + EPL_fixtures_yc$epl_1_2 + EPL_fixtures_yc$epl_2_2 + EPL_fixtures_yc$epl_3_0 + EPL_fixtures_yc$epl_3_1 +
    EPL_fixtures_yc$epl_3_2 + EPL_fixtures_yc$epl_0_3 + EPL_fixtures_yc$epl_1_3 + EPL_fixtures_yc$epl_2_3 + EPL_fixtures_yc$epl_3_3 +
    EPL_fixtures_yc$epl_4_0 + EPL_fixtures_yc$epl_4_1 + EPL_fixtures_yc$epl_4_2 + EPL_fixtures_yc$epl_4_3 + EPL_fixtures_yc$epl_0_4 +
    EPL_fixtures_yc$epl_1_4 + EPL_fixtures_yc$epl_2_4 + EPL_fixtures_yc$epl_3_4 + EPL_fixtures_yc$epl_4_4 + EPL_fixtures_yc$epl_5_0 +
    EPL_fixtures_yc$epl_5_1 + EPL_fixtures_yc$epl_5_2 + EPL_fixtures_yc$epl_5_3 + EPL_fixtures_yc$epl_5_4 + EPL_fixtures_yc$epl_0_5 +
    EPL_fixtures_yc$epl_1_5 + EPL_fixtures_yc$epl_2_5 + EPL_fixtures_yc$epl_3_5 + EPL_fixtures_yc$epl_4_5 + EPL_fixtures_yc$epl_5_5 +
    EPL_fixtures_yc$epl_6_0 + EPL_fixtures_yc$epl_6_1 + EPL_fixtures_yc$epl_6_2 + EPL_fixtures_yc$epl_6_3 + EPL_fixtures_yc$epl_6_4 +
    EPL_fixtures_yc$epl_6_5 + EPL_fixtures_yc$epl_0_6 + EPL_fixtures_yc$epl_1_6 + EPL_fixtures_yc$epl_2_6 + EPL_fixtures_yc$epl_3_6 +
    EPL_fixtures_yc$epl_4_6 + EPL_fixtures_yc$epl_5_6 + EPL_fixtures_yc$epl_6_6
)
#un25
EPL_fixtures_yc$epl_un25 <- (
  EPL_fixtures_yc$epl_0_0 + EPL_fixtures_yc$epl_1_0 + EPL_fixtures_yc$epl_0_1 + EPL_fixtures_yc$epl_1_1 + EPL_fixtures_yc$epl_2_0 + EPL_fixtures_yc$epl_0_2
)
#odds
EPL_fixtures_yc$epl_ov25_odds <- round((1/EPL_fixtures_yc$epl_ov25),digits = 2)
EPL_fixtures_yc$epl_un25_odds <- round((1/EPL_fixtures_yc$epl_un25),digits = 2)

EPL_fixtures_yc$epl_ov25_odds
EPL_fixtures_yc$epl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EPL_fixtures_yc$epl_ov25 <- percent(EPL_fixtures_yc$epl_ov25, accuracy = 0.1)

EPL_fixtures_yc$epl_un25 <- percent(EPL_fixtures_yc$epl_un25, accuracy = 0.1)
EPL_fixtures_yc$epl_pscore <- paste(round(EPL_fixtures_yc$epl_xHYC,digits = 0),round(EPL_fixtures_yc$epl_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
epl_GP <- nrow(EPL)
#Calculate total home corners for each division
epl_home_corners <- aggregate(EPL$HCO, by = list(EPL$HomeTeam), FUN = sum)
epl_away_corners <- aggregate(EPL$ACO, by = list(EPL$AwayTeam), FUN = sum)
###############################################################################
epl_T_HCO <- sum(epl_home_corners$x)
#calculate average home corners
epl_avg_HCO <- round(epl_T_HCO /epl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
epl_T_ACO <- sum(epl_away_corners$x)
#calculate average away goal
epl_avg_ACO <- round(epl_T_ACO /epl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
epl_home_coas <- round(((epl_home_corners$x/epl_home_games))/epl_avg_HCO, digits = 4)
#calculate away attack strength
epl_away_coas <- round(((epl_away_corners$x/epl_away_games))/epl_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
epl_avg_HCOC <- round(epl_T_ACO /epl_GP, digits = 4)
#avg away concede
epl_avg_ACOC <- round(epl_T_HCO /epl_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
epl_home_coc <- aggregate(EPL$ACO, by = list(EPL$HomeTeam), FUN = sum)
epl_away_coc <- aggregate(EPL$HCO, by = list(EPL$AwayTeam), FUN = sum)
#home defense strength
epl_home_cods <- round(((epl_home_coc$x/epl_home_games))/epl_avg_HCOC, digits = 4)
#away defense strength
epl_away_cods <- round(((epl_away_coc$x/epl_away_games))/epl_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_home_poisson_corners <- cbind(epl_division,epl_teams,epl_avg_HCO,epl_home_coas,epl_home_cods)
#################################################################################
#away poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_away_poisson_corners <- cbind(epl_division,epl_teams,epl_avg_ACO,epl_away_coas,epl_away_cods)

#EPL
HomeTeam_epl_co <- rep(epl_teams, each = length(epl_teams))
AwayTeam_epl_co <- rep(epl_teams, length(epl_teams))
EPL_fixtures_co <- cbind(HomeTeam_epl_co,AwayTeam_epl_co)
EPL_fixtures_co <- as.data.frame(EPL_fixtures_co)
EPL_fixtures_co <- EPL_fixtures_co[!EPL_fixtures_co$HomeTeam_epl_co == EPL_fixtures_co$AwayTeam_epl_co,]
rownames(EPL_fixtures_co) <- NULL
EPL_fixtures_co$Div <- "EPL"
EPL_fixtures_co <- EPL_fixtures_co[,c(3,1,2)]

EPL_fixtures_co$avg_HCO_epl <- epl_avg_HCO

EPL_fixtures_co$epl_homecoas <- rep(epl_home_coas,each = length(epl_teams)-1)

epl_awaycods_lookup <- cbind(epl_teams,epl_away_cods)

epl_awaycods_lookup <- as.data.frame(epl_awaycods_lookup)

colnames(epl_awaycods_lookup) <- c("AwayTeam_epl_co","epl_awaycods")


require('RH2')
EPL_fixtures_co$epl_awaycods <- sqldf("SELECT epl_awaycods_lookup.epl_awaycods FROM epl_awaycods_lookup INNER JOIN EPL_fixtures_co ON epl_awaycods_lookup.AwayTeam_epl_co = EPL_fixtures_co.AwayTeam_epl_co")

EPL_fixtures_co$avg_ACO_epl <- epl_avg_ACO

epl_awaycoas_lookup <- cbind(epl_teams,epl_away_coas)

epl_awaycoas_lookup <- as.data.frame(epl_awaycoas_lookup)

colnames(epl_awaycoas_lookup) <- c("AwayTeam_epl_co","epl_awaycoas")

EPL_fixtures_co$epl_awaycoas <- sqldf("SELECT epl_awaycoas_lookup.epl_awaycoas FROM epl_awaycoas_lookup INNER JOIN EPL_fixtures_co ON epl_awaycoas_lookup.AwayTeam_epl_co = EPL_fixtures_co.AwayTeam_epl_co")

EPL_fixtures_co$epl_homecods <- rep(epl_home_cods,each = length(epl_teams)-1)

EPL_fixtures_co$epl_awaycods <- as.numeric(unlist(EPL_fixtures_co$epl_awaycods))
#xGH
EPL_fixtures_co$epl_xHCOC <- EPL_fixtures_co$avg_HCO_epl * EPL_fixtures_co$epl_homecoas * EPL_fixtures_co$epl_awaycods
#xGA

EPL_fixtures_co$epl_awaycoas <- as.numeric(unlist(EPL_fixtures_co$epl_awaycoas))

EPL_fixtures_co$epl_xACOC <- EPL_fixtures_co$avg_ACO_epl * EPL_fixtures_co$epl_awaycoas * EPL_fixtures_co$epl_homecods

EPL_fixtures_co$epl_0_0 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_0 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_0_1 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_1 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_0 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_0_2 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_2 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_1 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_2 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_3 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_0 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_1 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_2 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_0_3 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_3 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_3 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_4 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_0 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_1 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_2 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_3 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_0_4 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_4 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_4 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_4 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_5 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_0 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_1 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_2 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_3 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_4 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_0_5 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_5 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_5 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_5 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_5 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_6 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_0 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(0,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_1 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(1,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_2 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(2,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_3 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(3,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_4 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(4,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_6_5 <- round(stats::dpois(6,EPL_fixtures_co$epl_xHCOC) * stats::dpois(5,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_0_6 <- round(stats::dpois(0,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_1_6 <- round(stats::dpois(1,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_2_6 <- round(stats::dpois(2,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_3_6 <- round(stats::dpois(3,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_4_6 <- round(stats::dpois(4,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
EPL_fixtures_co$epl_5_6 <- round(stats::dpois(5,EPL_fixtures_co$epl_xHCOC) * stats::dpois(6,EPL_fixtures_co$epl_xACOC), digits = 4)
#Home win
EPL_fixtures_co$epl_H <- (
  EPL_fixtures_co$epl_1_0 + EPL_fixtures_co$epl_2_0 + EPL_fixtures_co$epl_2_1 + EPL_fixtures_co$epl_3_0 + EPL_fixtures_co$epl_3_1 +
    EPL_fixtures_co$epl_3_2 + EPL_fixtures_co$epl_4_0 + EPL_fixtures_co$epl_4_1 + EPL_fixtures_co$epl_4_2 + EPL_fixtures_co$epl_4_3 +
    EPL_fixtures_co$epl_5_0 + EPL_fixtures_co$epl_5_1 + EPL_fixtures_co$epl_5_2 + EPL_fixtures_co$epl_5_3 + EPL_fixtures_co$epl_5_4 +
    EPL_fixtures_co$epl_6_0 + EPL_fixtures_co$epl_6_1 + EPL_fixtures_co$epl_6_2 + EPL_fixtures_co$epl_6_3 + EPL_fixtures_co$epl_6_4 +
    EPL_fixtures_co$epl_6_5
)

EPL_fixtures_co$epl_H <- percent(EPL_fixtures_co$epl_H, accuracy = 0.1)

#Draw
EPL_fixtures_co$epl_D <- (

  EPL_fixtures_co$epl_0_0 + EPL_fixtures_co$epl_1_1 + EPL_fixtures_co$epl_2_2 + EPL_fixtures_co$epl_3_3 + EPL_fixtures_co$epl_4_4 +
    EPL_fixtures_co$epl_5_5 + EPL_fixtures_co$epl_6_6
)

EPL_fixtures_co$epl_D <- percent(EPL_fixtures_co$epl_D, accuracy = 0.1)

#Away

EPL_fixtures_co$epl_A <- (
  EPL_fixtures_co$epl_0_1 + EPL_fixtures_co$epl_0_2 + EPL_fixtures_co$epl_1_2 + EPL_fixtures_co$epl_0_3 + EPL_fixtures_co$epl_1_3 +
    EPL_fixtures_co$epl_2_3 + EPL_fixtures_co$epl_0_4 + EPL_fixtures_co$epl_1_4 + EPL_fixtures_co$epl_2_4 + EPL_fixtures_co$epl_3_4 +
    EPL_fixtures_co$epl_0_5 + EPL_fixtures_co$epl_1_5 + EPL_fixtures_co$epl_2_5 + EPL_fixtures_co$epl_3_5 + EPL_fixtures_co$epl_4_5 +
    EPL_fixtures_co$epl_0_6 + EPL_fixtures_co$epl_1_6 + EPL_fixtures_co$epl_2_6 + EPL_fixtures_co$epl_3_6 + EPL_fixtures_co$epl_4_6 +
    EPL_fixtures_co$epl_5_6
)

EPL_fixtures_co$epl_A <- percent(EPL_fixtures_co$epl_A, accuracy = 0.1)

#ov25
EPL_fixtures_co$epl_ov25 <- (
  EPL_fixtures_co$epl_2_1 + EPL_fixtures_co$epl_1_2 + EPL_fixtures_co$epl_2_2 + EPL_fixtures_co$epl_3_0 + EPL_fixtures_co$epl_3_1 +
    EPL_fixtures_co$epl_3_2 + EPL_fixtures_co$epl_0_3 + EPL_fixtures_co$epl_1_3 + EPL_fixtures_co$epl_2_3 + EPL_fixtures_co$epl_3_3 +
    EPL_fixtures_co$epl_4_0 + EPL_fixtures_co$epl_4_1 + EPL_fixtures_co$epl_4_2 + EPL_fixtures_co$epl_4_3 + EPL_fixtures_co$epl_0_4 +
    EPL_fixtures_co$epl_1_4 + EPL_fixtures_co$epl_2_4 + EPL_fixtures_co$epl_3_4 + EPL_fixtures_co$epl_4_4 + EPL_fixtures_co$epl_5_0 +
    EPL_fixtures_co$epl_5_1 + EPL_fixtures_co$epl_5_2 + EPL_fixtures_co$epl_5_3 + EPL_fixtures_co$epl_5_4 + EPL_fixtures_co$epl_0_5 +
    EPL_fixtures_co$epl_1_5 + EPL_fixtures_co$epl_2_5 + EPL_fixtures_co$epl_3_5 + EPL_fixtures_co$epl_4_5 + EPL_fixtures_co$epl_5_5 +
    EPL_fixtures_co$epl_6_0 + EPL_fixtures_co$epl_6_1 + EPL_fixtures_co$epl_6_2 + EPL_fixtures_co$epl_6_3 + EPL_fixtures_co$epl_6_4 +
    EPL_fixtures_co$epl_6_5 + EPL_fixtures_co$epl_0_6 + EPL_fixtures_co$epl_1_6 + EPL_fixtures_co$epl_2_6 + EPL_fixtures_co$epl_3_6 +
    EPL_fixtures_co$epl_4_6 + EPL_fixtures_co$epl_5_6 + EPL_fixtures_co$epl_6_6
)
#un25
EPL_fixtures_co$epl_un25 <- (
  EPL_fixtures_co$epl_0_0 + EPL_fixtures_co$epl_1_0 + EPL_fixtures_co$epl_0_1 + EPL_fixtures_co$epl_1_1 + EPL_fixtures_co$epl_2_0 + EPL_fixtures_co$epl_0_2
)
#odds
EPL_fixtures_co$epl_ov25_odds <- round((1/EPL_fixtures_co$epl_ov25),digits = 2)
EPL_fixtures_co$epl_un25_odds <- round((1/EPL_fixtures_co$epl_un25),digits = 2)

EPL_fixtures_co$epl_ov25_odds
EPL_fixtures_co$epl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EPL_fixtures_co$epl_ov25 <- percent(EPL_fixtures_co$epl_ov25, accuracy = 0.1)

EPL_fixtures_co$epl_un25 <- percent(EPL_fixtures_co$epl_un25, accuracy = 0.1)
EPL_fixtures_co$epl_pscore <- paste(round(EPL_fixtures_co$epl_xHCOC,digits = 0),round(EPL_fixtures_co$epl_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
epl_GP <- nrow(EPL)
#Calculate total home goals for each division
epl_T_HF <- sum(epl_home_fouls$x)
#calculate average home goal
epl_avg_HF <- round(epl_T_HF /epl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
epl_T_AF <- sum(epl_away_fouls$x)
#calculate average away goal
epl_avg_AF <- round(epl_T_AF /epl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
epl_home_fas <- round(((epl_home_fouls$x/epl_home_games))/epl_avg_HF, digits = 4)
#calculate away attack strength
epl_away_fas <- round(((epl_away_fouls$x/epl_away_games))/epl_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
epl_avg_HFC <- round(epl_T_AF /epl_GP, digits = 4)
#avg away concede
epl_avg_AFC <- round(epl_T_HF /epl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
epl_home_fcc <- aggregate(EPL$AF, by = list(EPL$HomeTeam), FUN = sum)
epl_away_fcc <- aggregate(EPL$HF, by = list(EPL$AwayTeam), FUN = sum)

#home defense strength
epl_home_fds <- round(((epl_home_fcc$x/epl_home_games))/epl_avg_HFC, digits = 4)

#away defense strength
epl_away_fds <- round(((epl_away_fcc$x/epl_away_games))/epl_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_home_poisson_fo <- cbind(epl_division,epl_teams,epl_avg_HF,epl_home_fas,epl_home_fds)

#################################################################################
#away poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_away_poisson_fo <- cbind(epl_division,epl_teams,epl_avg_AF,epl_away_fas,epl_away_fds)

#EPL
HomeTeam_epl_fo <- rep(epl_teams, each = length(epl_teams))
AwayTeam_epl_fo <- rep(epl_teams, length(epl_teams))
EPL_fixtures_fo <- cbind(HomeTeam_epl_fo,AwayTeam_epl_fo)
EPL_fixtures_fo <- as.data.frame(EPL_fixtures_fo)
EPL_fixtures_fo <- EPL_fixtures_fo[!EPL_fixtures_fo$HomeTeam_epl_fo == EPL_fixtures_fo$AwayTeam_epl_fo,]
rownames(EPL_fixtures_fo) <- NULL
EPL_fixtures_fo$Div <- "EPL"
EPL_fixtures_fo <- EPL_fixtures_fo[,c(3,1,2)]

EPL_fixtures_fo$avg_HF_epl <- epl_avg_HF

EPL_fixtures_fo$epl_homefas <- rep(epl_home_fas,each = length(epl_teams)-1)

epl_awayfds_lookup <- cbind(epl_teams,epl_away_fds)

epl_awayfds_lookup <- as.data.frame(epl_awayfds_lookup)

colnames(epl_awayfds_lookup) <- c("AwayTeam_epl_fo","epl_awayfds")


require('RH2')
EPL_fixtures_fo$epl_awayfds <- sqldf("SELECT epl_awayfds_lookup.epl_awayfds FROM epl_awayfds_lookup INNER JOIN EPL_fixtures_fo ON epl_awayfds_lookup.AwayTeam_epl_fo = EPL_fixtures_fo.AwayTeam_epl_fo")

EPL_fixtures_fo$avg_AF_epl <- epl_avg_AF

epl_awayfas_lookup <- cbind(epl_teams,epl_away_fas)

epl_awayfas_lookup <- as.data.frame(epl_awayfas_lookup)

colnames(epl_awayfas_lookup) <- c("AwayTeam_epl_fo","epl_awayfas")

EPL_fixtures_fo$epl_awayfas <- sqldf("SELECT epl_awayfas_lookup.epl_awayfas FROM epl_awayfas_lookup INNER JOIN EPL_fixtures_fo ON epl_awayfas_lookup.AwayTeam_epl_fo = EPL_fixtures_fo.AwayTeam_epl_fo")

EPL_fixtures_fo$epl_homefds <- rep(epl_home_fds,each = length(epl_teams)-1)

EPL_fixtures_fo$epl_awayfds <- as.numeric(unlist(EPL_fixtures_fo$epl_awayfds))
#xGH
EPL_fixtures_fo$epl_xHF <- EPL_fixtures_fo$avg_HF_epl * EPL_fixtures_fo$epl_homefas * EPL_fixtures_fo$epl_awayfds
#xGA

EPL_fixtures_fo$epl_awayfas <- as.numeric(unlist(EPL_fixtures_fo$epl_awayfas))

EPL_fixtures_fo$epl_xAF <- EPL_fixtures_fo$avg_AF_epl * EPL_fixtures_fo$epl_awayfas * EPL_fixtures_fo$epl_homefds

EPL_fixtures_fo$epl_0_0 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_0 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_0_1 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_1 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_0 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_0_2 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_2 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_1 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_2 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_3 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_0 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_1 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_2 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_0_3 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_3 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_3 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_4 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_0 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_1 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_2 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_3 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_0_4 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_4 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_4 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_4 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_5 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_0 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_1 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_2 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_3 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_4 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_0_5 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_5 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_5 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_5 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_5 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_6 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_0 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(0,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_1 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(1,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_2 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(2,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_3 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(3,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_4 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(4,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_6_5 <- round(stats::dpois(6,EPL_fixtures_fo$epl_xHF) * stats::dpois(5,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_0_6 <- round(stats::dpois(0,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_1_6 <- round(stats::dpois(1,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_2_6 <- round(stats::dpois(2,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_3_6 <- round(stats::dpois(3,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_4_6 <- round(stats::dpois(4,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
EPL_fixtures_fo$epl_5_6 <- round(stats::dpois(5,EPL_fixtures_fo$epl_xHF) * stats::dpois(6,EPL_fixtures_fo$epl_xAF), digits = 4)
#Home win
EPL_fixtures_fo$epl_H <- (
  EPL_fixtures_fo$epl_1_0 + EPL_fixtures_fo$epl_2_0 + EPL_fixtures_fo$epl_2_1 + EPL_fixtures_fo$epl_3_0 + EPL_fixtures_fo$epl_3_1 +
    EPL_fixtures_fo$epl_3_2 + EPL_fixtures_fo$epl_4_0 + EPL_fixtures_fo$epl_4_1 + EPL_fixtures_fo$epl_4_2 + EPL_fixtures_fo$epl_4_3 +
    EPL_fixtures_fo$epl_5_0 + EPL_fixtures_fo$epl_5_1 + EPL_fixtures_fo$epl_5_2 + EPL_fixtures_fo$epl_5_3 + EPL_fixtures_fo$epl_5_4 +
    EPL_fixtures_fo$epl_6_0 + EPL_fixtures_fo$epl_6_1 + EPL_fixtures_fo$epl_6_2 + EPL_fixtures_fo$epl_6_3 + EPL_fixtures_fo$epl_6_4 +
    EPL_fixtures_fo$epl_6_5
)

EPL_fixtures_fo$epl_H <- percent(EPL_fixtures_fo$epl_H, accuracy = 0.1)

#Draw
EPL_fixtures_fo$epl_D <- (

  EPL_fixtures_fo$epl_0_0 + EPL_fixtures_fo$epl_1_1 + EPL_fixtures_fo$epl_2_2 + EPL_fixtures_fo$epl_3_3 + EPL_fixtures_fo$epl_4_4 +
    EPL_fixtures_fo$epl_5_5 + EPL_fixtures_fo$epl_6_6
)

EPL_fixtures_fo$epl_D <- percent(EPL_fixtures_fo$epl_D, accuracy = 0.1)

#Away

EPL_fixtures_fo$epl_A <- (
  EPL_fixtures_fo$epl_0_1 + EPL_fixtures_fo$epl_0_2 + EPL_fixtures_fo$epl_1_2 + EPL_fixtures_fo$epl_0_3 + EPL_fixtures_fo$epl_1_3 +
    EPL_fixtures_fo$epl_2_3 + EPL_fixtures_fo$epl_0_4 + EPL_fixtures_fo$epl_1_4 + EPL_fixtures_fo$epl_2_4 + EPL_fixtures_fo$epl_3_4 +
    EPL_fixtures_fo$epl_0_5 + EPL_fixtures_fo$epl_1_5 + EPL_fixtures_fo$epl_2_5 + EPL_fixtures_fo$epl_3_5 + EPL_fixtures_fo$epl_4_5 +
    EPL_fixtures_fo$epl_0_6 + EPL_fixtures_fo$epl_1_6 + EPL_fixtures_fo$epl_2_6 + EPL_fixtures_fo$epl_3_6 + EPL_fixtures_fo$epl_4_6 +
    EPL_fixtures_fo$epl_5_6
)

EPL_fixtures_fo$epl_A <- percent(EPL_fixtures_fo$epl_A, accuracy = 0.1)

#ov25
EPL_fixtures_fo$epl_ov25 <- (
  EPL_fixtures_fo$epl_2_1 + EPL_fixtures_fo$epl_1_2 + EPL_fixtures_fo$epl_2_2 + EPL_fixtures_fo$epl_3_0 + EPL_fixtures_fo$epl_3_1 +
    EPL_fixtures_fo$epl_3_2 + EPL_fixtures_fo$epl_0_3 + EPL_fixtures_fo$epl_1_3 + EPL_fixtures_fo$epl_2_3 + EPL_fixtures_fo$epl_3_3 +
    EPL_fixtures_fo$epl_4_0 + EPL_fixtures_fo$epl_4_1 + EPL_fixtures_fo$epl_4_2 + EPL_fixtures_fo$epl_4_3 + EPL_fixtures_fo$epl_0_4 +
    EPL_fixtures_fo$epl_1_4 + EPL_fixtures_fo$epl_2_4 + EPL_fixtures_fo$epl_3_4 + EPL_fixtures_fo$epl_4_4 + EPL_fixtures_fo$epl_5_0 +
    EPL_fixtures_fo$epl_5_1 + EPL_fixtures_fo$epl_5_2 + EPL_fixtures_fo$epl_5_3 + EPL_fixtures_fo$epl_5_4 + EPL_fixtures_fo$epl_0_5 +
    EPL_fixtures_fo$epl_1_5 + EPL_fixtures_fo$epl_2_5 + EPL_fixtures_fo$epl_3_5 + EPL_fixtures_fo$epl_4_5 + EPL_fixtures_fo$epl_5_5 +
    EPL_fixtures_fo$epl_6_0 + EPL_fixtures_fo$epl_6_1 + EPL_fixtures_fo$epl_6_2 + EPL_fixtures_fo$epl_6_3 + EPL_fixtures_fo$epl_6_4 +
    EPL_fixtures_fo$epl_6_5 + EPL_fixtures_fo$epl_0_6 + EPL_fixtures_fo$epl_1_6 + EPL_fixtures_fo$epl_2_6 + EPL_fixtures_fo$epl_3_6 +
    EPL_fixtures_fo$epl_4_6 + EPL_fixtures_fo$epl_5_6 + EPL_fixtures_fo$epl_6_6
)
#un25
EPL_fixtures_fo$epl_un25 <- (
  EPL_fixtures_fo$epl_0_0 + EPL_fixtures_fo$epl_1_0 + EPL_fixtures_fo$epl_0_1 + EPL_fixtures_fo$epl_1_1 + EPL_fixtures_fo$epl_2_0 + EPL_fixtures_fo$epl_0_2
)
#odds
EPL_fixtures_fo$epl_ov25_odds <- round((1/EPL_fixtures_fo$epl_ov25),digits = 2)
EPL_fixtures_fo$epl_un25_odds <- round((1/EPL_fixtures_fo$epl_un25),digits = 2)

EPL_fixtures_fo$epl_ov25_odds
EPL_fixtures_fo$epl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EPL_fixtures_fo$epl_ov25 <- percent(EPL_fixtures_fo$epl_ov25, accuracy = 0.1)

EPL_fixtures_fo$epl_un25 <- percent(EPL_fixtures_fo$epl_un25, accuracy = 0.1)
EPL_fixtures_fo$epl_psfore <- paste(round(EPL_fixtures_fo$epl_xHF,digits = 0),round(EPL_fixtures_fo$epl_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
epl_GP <- nrow(EPL)

#Calculate total home goals for each division
epl_T_HST <- sum(epl_home_hst$x)
#calculate average home goal

epl_avg_HST <- round(epl_T_HST /epl_GP, digits = 4)

############################################################
#Calculate total away goals for each division
epl_T_AST <- sum(epl_away_ast$x)
#calculate average away goal
epl_avg_AST <- round(epl_T_AST /epl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
epl_home_sotas <- round(((epl_home_hst$x/epl_home_games))/epl_avg_HST, digits = 4)
#calculate away attack strength
epl_away_sotas <- round(((epl_away_ast$x/epl_away_games))/epl_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
epl_avg_HSC <- round(epl_T_AST /epl_GP, digits = 4)

#avg away concede
epl_avg_ASC <- round(epl_T_HST /epl_GP, digits = 4)
#home defense strength
epl_home_sods <- round(((epl_home_hsc$x/epl_home_games))/epl_avg_HSC, digits = 4)

#away defense strength
epl_away_sods <- round(((epl_away_ast$x/epl_away_games))/epl_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_home_poisson_sot <- cbind(epl_division,epl_teams,epl_avg_HST,epl_home_sotas,epl_home_sods)

#################################################################################
#away poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_away_poisson_sot <- cbind(epl_division,epl_teams,epl_avg_AST,epl_away_sotas,epl_away_sods)

#EPL
HomeTeam_epl_sot <- rep(epl_teams, each = length(epl_teams))
AwayTeam_epl_sot <- rep(epl_teams, length(epl_teams))
EPL_fixtures_sot <- cbind(HomeTeam_epl_sot,AwayTeam_epl_sot)
EPL_fixtures_sot <- as.data.frame(EPL_fixtures_sot)
EPL_fixtures_sot <- EPL_fixtures_sot[!EPL_fixtures_sot$HomeTeam_epl_sot == EPL_fixtures_sot$AwayTeam_epl_sot,]
rownames(EPL_fixtures_sot) <- NULL
EPL_fixtures_sot$Div <- "EPL"
EPL_fixtures_sot <- EPL_fixtures_sot[,c(3,1,2)]

EPL_fixtures_sot$avg_HST_epl <- epl_avg_HST

EPL_fixtures_sot$epl_homesotas <- rep(epl_home_sotas,each = length(epl_teams)-1)

epl_awaysods_lookup <- cbind(epl_teams,epl_away_sods)

epl_awaysods_lookup <- as.data.frame(epl_awaysods_lookup)

colnames(epl_awaysods_lookup) <- c("AwayTeam_epl_sot","epl_awaysods")


require('RH2')
EPL_fixtures_sot$epl_awaysods <- sqldf("SELECT epl_awaysods_lookup.epl_awaysods FROM epl_awaysods_lookup INNER JOIN EPL_fixtures_sot ON epl_awaysods_lookup.AwayTeam_epl_sot = EPL_fixtures_sot.AwayTeam_epl_sot")

EPL_fixtures_sot$avg_AST_epl <- epl_avg_AST

epl_awaysotas_lookup <- cbind(epl_teams,epl_away_sotas)

epl_awaysotas_lookup <- as.data.frame(epl_awaysotas_lookup)

colnames(epl_awaysotas_lookup) <- c("AwayTeam_epl_sot","epl_awaysotas")

EPL_fixtures_sot$epl_awaysotas <- sqldf("SELECT epl_awaysotas_lookup.epl_awaysotas FROM epl_awaysotas_lookup INNER JOIN EPL_fixtures_sot ON epl_awaysotas_lookup.AwayTeam_epl_sot = EPL_fixtures_sot.AwayTeam_epl_sot")

EPL_fixtures_sot$epl_homesods <- rep(epl_home_sods,each = length(epl_teams)-1)

EPL_fixtures_sot$epl_awaysods <- as.numeric(unlist(EPL_fixtures_sot$epl_awaysods))
#xGH
EPL_fixtures_sot$epl_xHST <- EPL_fixtures_sot$avg_HST_epl * EPL_fixtures_sot$epl_homesotas * EPL_fixtures_sot$epl_awaysods
#xGA

EPL_fixtures_sot$epl_awaysotas <- as.numeric(unlist(EPL_fixtures_sot$epl_awaysotas))

EPL_fixtures_sot$epl_xAST <- EPL_fixtures_sot$avg_AST_epl * EPL_fixtures_sot$epl_awaysotas * EPL_fixtures_sot$epl_homesods

EPL_fixtures_sot$epl_0_0 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_0 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_0_1 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_1 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_0 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_0_2 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_2 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_1 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_2 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_3 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_0 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_1 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_2 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_0_3 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_3 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_3 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_4 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_0 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_1 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_2 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_3 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_0_4 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_4 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_4 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_4 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_5 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_0 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_1 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_2 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_3 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_4 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_0_5 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_5 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_5 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_5 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_5 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_6 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_0 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(0,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_1 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(1,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_2 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(2,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_3 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(3,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_4 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(4,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_6_5 <- round(stats::dpois(6,EPL_fixtures_sot$epl_xHST) * stats::dpois(5,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_0_6 <- round(stats::dpois(0,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_1_6 <- round(stats::dpois(1,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_2_6 <- round(stats::dpois(2,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_3_6 <- round(stats::dpois(3,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_4_6 <- round(stats::dpois(4,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
EPL_fixtures_sot$epl_5_6 <- round(stats::dpois(5,EPL_fixtures_sot$epl_xHST) * stats::dpois(6,EPL_fixtures_sot$epl_xAST), digits = 4)
#Home win
EPL_fixtures_sot$epl_H <- (
  EPL_fixtures_sot$epl_1_0 + EPL_fixtures_sot$epl_2_0 + EPL_fixtures_sot$epl_2_1 + EPL_fixtures_sot$epl_3_0 + EPL_fixtures_sot$epl_3_1 +
    EPL_fixtures_sot$epl_3_2 + EPL_fixtures_sot$epl_4_0 + EPL_fixtures_sot$epl_4_1 + EPL_fixtures_sot$epl_4_2 + EPL_fixtures_sot$epl_4_3 +
    EPL_fixtures_sot$epl_5_0 + EPL_fixtures_sot$epl_5_1 + EPL_fixtures_sot$epl_5_2 + EPL_fixtures_sot$epl_5_3 + EPL_fixtures_sot$epl_5_4 +
    EPL_fixtures_sot$epl_6_0 + EPL_fixtures_sot$epl_6_1 + EPL_fixtures_sot$epl_6_2 + EPL_fixtures_sot$epl_6_3 + EPL_fixtures_sot$epl_6_4 +
    EPL_fixtures_sot$epl_6_5
)

EPL_fixtures_sot$epl_H <- percent(EPL_fixtures_sot$epl_H, accuracy = 0.1)

#Draw
EPL_fixtures_sot$epl_D <- (

  EPL_fixtures_sot$epl_0_0 + EPL_fixtures_sot$epl_1_1 + EPL_fixtures_sot$epl_2_2 + EPL_fixtures_sot$epl_3_3 + EPL_fixtures_sot$epl_4_4 +
    EPL_fixtures_sot$epl_5_5 + EPL_fixtures_sot$epl_6_6
)

EPL_fixtures_sot$epl_D <- percent(EPL_fixtures_sot$epl_D, accuracy = 0.1)

#Away

EPL_fixtures_sot$epl_A <- (
  EPL_fixtures_sot$epl_0_1 + EPL_fixtures_sot$epl_0_2 + EPL_fixtures_sot$epl_1_2 + EPL_fixtures_sot$epl_0_3 + EPL_fixtures_sot$epl_1_3 +
    EPL_fixtures_sot$epl_2_3 + EPL_fixtures_sot$epl_0_4 + EPL_fixtures_sot$epl_1_4 + EPL_fixtures_sot$epl_2_4 + EPL_fixtures_sot$epl_3_4 +
    EPL_fixtures_sot$epl_0_5 + EPL_fixtures_sot$epl_1_5 + EPL_fixtures_sot$epl_2_5 + EPL_fixtures_sot$epl_3_5 + EPL_fixtures_sot$epl_4_5 +
    EPL_fixtures_sot$epl_0_6 + EPL_fixtures_sot$epl_1_6 + EPL_fixtures_sot$epl_2_6 + EPL_fixtures_sot$epl_3_6 + EPL_fixtures_sot$epl_4_6 +
    EPL_fixtures_sot$epl_5_6
)

EPL_fixtures_sot$epl_A <- percent(EPL_fixtures_sot$epl_A, accuracy = 0.1)

#ov25
EPL_fixtures_sot$epl_ov25 <- (
  EPL_fixtures_sot$epl_2_1 + EPL_fixtures_sot$epl_1_2 + EPL_fixtures_sot$epl_2_2 + EPL_fixtures_sot$epl_3_0 + EPL_fixtures_sot$epl_3_1 +
    EPL_fixtures_sot$epl_3_2 + EPL_fixtures_sot$epl_0_3 + EPL_fixtures_sot$epl_1_3 + EPL_fixtures_sot$epl_2_3 + EPL_fixtures_sot$epl_3_3 +
    EPL_fixtures_sot$epl_4_0 + EPL_fixtures_sot$epl_4_1 + EPL_fixtures_sot$epl_4_2 + EPL_fixtures_sot$epl_4_3 + EPL_fixtures_sot$epl_0_4 +
    EPL_fixtures_sot$epl_1_4 + EPL_fixtures_sot$epl_2_4 + EPL_fixtures_sot$epl_3_4 + EPL_fixtures_sot$epl_4_4 + EPL_fixtures_sot$epl_5_0 +
    EPL_fixtures_sot$epl_5_1 + EPL_fixtures_sot$epl_5_2 + EPL_fixtures_sot$epl_5_3 + EPL_fixtures_sot$epl_5_4 + EPL_fixtures_sot$epl_0_5 +
    EPL_fixtures_sot$epl_1_5 + EPL_fixtures_sot$epl_2_5 + EPL_fixtures_sot$epl_3_5 + EPL_fixtures_sot$epl_4_5 + EPL_fixtures_sot$epl_5_5 +
    EPL_fixtures_sot$epl_6_0 + EPL_fixtures_sot$epl_6_1 + EPL_fixtures_sot$epl_6_2 + EPL_fixtures_sot$epl_6_3 + EPL_fixtures_sot$epl_6_4 +
    EPL_fixtures_sot$epl_6_5 + EPL_fixtures_sot$epl_0_6 + EPL_fixtures_sot$epl_1_6 + EPL_fixtures_sot$epl_2_6 + EPL_fixtures_sot$epl_3_6 +
    EPL_fixtures_sot$epl_4_6 + EPL_fixtures_sot$epl_5_6 + EPL_fixtures_sot$epl_6_6
)
#un25
EPL_fixtures_sot$epl_un25 <- (
  EPL_fixtures_sot$epl_0_0 + EPL_fixtures_sot$epl_1_0 + EPL_fixtures_sot$epl_0_1 + EPL_fixtures_sot$epl_1_1 + EPL_fixtures_sot$epl_2_0 + EPL_fixtures_sot$epl_0_2
)
#odds
EPL_fixtures_sot$epl_ov25_odds <- round((1/EPL_fixtures_sot$epl_ov25),digits = 2)
EPL_fixtures_sot$epl_un25_odds <- round((1/EPL_fixtures_sot$epl_un25),digits = 2)

EPL_fixtures_sot$epl_ov25_odds
EPL_fixtures_sot$epl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EPL_fixtures_sot$epl_ov25 <- percent(EPL_fixtures_sot$epl_ov25, accuracy = 0.1)

EPL_fixtures_sot$epl_un25 <- percent(EPL_fixtures_sot$epl_un25, accuracy = 0.1)
EPL_fixtures_sot$epl_pssotre <- paste(round(EPL_fixtures_sot$epl_xHST,digits = 0),round(EPL_fixtures_sot$epl_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
epl_home_wins <- c()
epl_away_wins <- c()
epl_home_draws <- c()
epl_away_draws <- c()
epl_home_loss <- c()
epl_away_loss <- c()



for (i_epl_wins in 1:length(epl_teams))
{

  epl_home_wins[i_epl_wins] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_wins] & EPL$FTR == "H",])
  epl_away_wins[i_epl_wins] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_wins] & EPL$FTR == "A",])
  epl_home_draws[i_epl_wins] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_wins] & EPL$FTR == "D",])
  epl_away_draws[i_epl_wins] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_wins] & EPL$FTR == "D",])
  epl_home_loss[i_epl_wins] <- nrow(EPL[EPL$HomeTeam == epl_teams[i_epl_wins] & EPL$FTR == "A",])
  epl_away_loss[i_epl_wins] <- nrow(EPL[EPL$AwayTeam == epl_teams[i_epl_wins] & EPL$FTR == "H",])

}

epl_total_wins <- epl_home_wins + epl_away_wins
epl_total_draws <- epl_home_draws + epl_away_draws
epl_total_loss <- epl_home_loss + epl_away_loss

epl_league_table <- cbind(epl_teams,epl_games_played,epl_total_wins,epl_total_draws,epl_total_loss)
epl_GS <- epl_scoring$TGS
epl_GC <-epl_conceding$TGC
epl_GD <- epl_scoring$TGS - epl_conceding$TGC
epl_PTS <- (epl_total_wins*3) + (epl_total_draws*1)
epl_league_table <- cbind(epl_league_table,epl_GS,epl_GC,epl_GD,epl_PTS)
epl_league_table <- as.data.frame(epl_league_table)
#rename the columns
names(epl_league_table)[names(epl_league_table) == "epl_teams"] <- "Team"
names(epl_league_table)[names(epl_league_table) == "epl_games_played"] <- "P"
names(epl_league_table)[names(epl_league_table) == "epl_total_wins"] <- "W"
names(epl_league_table)[names(epl_league_table) == "epl_total_draws"] <- "D"
names(epl_league_table)[names(epl_league_table) == "epl_total_loss"] <- "L"
names(epl_league_table)[names(epl_league_table) == "epl_GS"] <- "F"
names(epl_league_table)[names(epl_league_table) == "epl_GC"] <- "A"
points_epl <- epl_league_table[order(as.numeric(epl_league_table$epl_PTS), decreasing = TRUE),]
points_epl$epl_rank <- 1:length(epl_teams)
row.names(points_epl) <- points_epl$epl_rank
#create final_epl_hf_against with team ranks in brackets
for(epl_rowhrank in 1:nrow(epl_form_team_against_h)) {
  for(epl_colhrank in 1:ncol(epl_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!epl_form_team_against_h[epl_rowhrank,epl_colhrank]=="",epl_form_team_against_h[epl_rowhrank,epl_colhrank] <- paste(epl_form_team_against_h[epl_rowhrank,epl_colhrank],"(",points_epl$epl_rank[points_epl$Team ==epl_form_team_against_h[epl_rowhrank,epl_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
epl_GP <- nrow(EPL)

#Calculate total home goals for each division
epl_T_HG <- sum(epl_home_gs$x)

#calculate average home goal
epl_avg_HG <- round(epl_T_HG /epl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
epl_T_AG <- sum(epl_away_gs$x)
#calculate average away goal
epl_avg_AG <- round(epl_T_AG /epl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
epl_home_as <- round(((epl_home_gs$x/epl_home_games))/epl_avg_HG, digits = 4)
#calculate away attack strength
epl_away_as <- round(((epl_away_gs$x/epl_away_games))/epl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
epl_avg_HC <- round(epl_T_AG /epl_GP, digits = 4)
#avg away concede
epl_avg_AC <- round(epl_T_HG /epl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
epl_home_ds <- round(((epl_home_gc$x/epl_home_games))/epl_avg_HC, digits = 4)
#away defense strength
epl_away_ds <- round(((epl_away_gc$x/epl_away_games))/epl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_home_poisson <- cbind(epl_division,epl_teams,epl_avg_HG,epl_home_as,epl_home_ds)
#################################################################################
#away poisson data
#epl
epl_division <- c()
epl_division[1:length(epl_teams)] <- "EPL"
epl_away_poisson <- cbind(epl_division,epl_teams,epl_avg_AG,epl_away_as,epl_away_ds)

#EPL
HomeTeam_epl <- rep(epl_teams, each = length(epl_teams))
AwayTeam_epl <- rep(epl_teams, length(epl_teams))
EPL_fixtures <- cbind(HomeTeam_epl,AwayTeam_epl)
EPL_fixtures <- as.data.frame(EPL_fixtures)
EPL_fixtures <- EPL_fixtures[!EPL_fixtures$HomeTeam_epl == EPL_fixtures$AwayTeam_epl,]
rownames(EPL_fixtures) <- NULL
EPL_fixtures$Div <- "EPL"
EPL_fixtures <- EPL_fixtures[,c(3,1,2)]

EPL_fixtures$avg_HG_epl <- epl_avg_HG

EPL_fixtures$epl_homeas <- rep(epl_home_as,each = length(epl_teams)-1)

epl_awayds_lookup <- cbind(epl_teams,epl_away_ds)

epl_awayds_lookup <- as.data.frame(epl_awayds_lookup)

colnames(epl_awayds_lookup) <- c("AwayTeam_epl","epl_awayds")


require('RH2')
EPL_fixtures$epl_awayds <- sqldf("SELECT epl_awayds_lookup.epl_awayds FROM epl_awayds_lookup INNER JOIN EPL_fixtures ON epl_awayds_lookup.AwayTeam_epl = EPL_fixtures.AwayTeam_epl")

EPL_fixtures$avg_AG_epl <- epl_avg_AG

epl_awayas_lookup <- cbind(epl_teams,epl_away_as)

epl_awayas_lookup <- as.data.frame(epl_awayas_lookup)

colnames(epl_awayas_lookup) <- c("AwayTeam_epl","epl_awayas")


EPL_fixtures$epl_awayas <- sqldf("SELECT epl_awayas_lookup.epl_awayas FROM epl_awayas_lookup INNER JOIN EPL_fixtures ON epl_awayas_lookup.AwayTeam_epl = EPL_fixtures.AwayTeam_epl")

EPL_fixtures$epl_homeds <- rep(epl_home_ds,each = length(epl_teams)-1)

EPL_fixtures$epl_awayds <- as.numeric(unlist(EPL_fixtures$epl_awayds))
#xGH
EPL_fixtures$epl_xGH <- EPL_fixtures$avg_HG_epl * EPL_fixtures$epl_homeas * EPL_fixtures$epl_awayds

#xGA

EPL_fixtures$epl_awayas <- as.numeric(unlist(EPL_fixtures$epl_awayas))

EPL_fixtures$epl_xGA <- EPL_fixtures$avg_AG_epl * EPL_fixtures$epl_awayas * EPL_fixtures$epl_homeds

EPL_fixtures$epl_0_0 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_0 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_0_1 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_1 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_0 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_0_2 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_2 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_1 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_2 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_3 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_0 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_1 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_2 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_0_3 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_3 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_3 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_4 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_0 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_1 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_2 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_3 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_0_4 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_4 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_4 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_4 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_5 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_0 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_1 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_2 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_3 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_4 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_0_5 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_5 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_5 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_5 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_5 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_6 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_0 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(0,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_1 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(1,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_2 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(2,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_3 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(3,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_4 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(4,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_6_5 <- round(stats::dpois(6,EPL_fixtures$epl_xGH) * stats::dpois(5,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_0_6 <- round(stats::dpois(0,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_1_6 <- round(stats::dpois(1,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_2_6 <- round(stats::dpois(2,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_3_6 <- round(stats::dpois(3,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_4_6 <- round(stats::dpois(4,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
EPL_fixtures$epl_5_6 <- round(stats::dpois(5,EPL_fixtures$epl_xGH) * stats::dpois(6,EPL_fixtures$epl_xGA), digits = 4)
#Home win
EPL_fixtures$epl_H <- (
  EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
    EPL_fixtures$epl_5_0 + EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5
)

EPL_fixtures$epl_H <- percent(EPL_fixtures$epl_H, accuracy = 0.1)

#Draw
EPL_fixtures$epl_D <- (

  EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 + EPL_fixtures$epl_4_4 +
    EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6
)

EPL_fixtures$epl_D <- percent(EPL_fixtures$epl_D, accuracy = 0.1)

#Away

EPL_fixtures$epl_A <- (
  EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
    EPL_fixtures$epl_0_5 + EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6
)

EPL_fixtures$epl_A <- percent(EPL_fixtures$epl_A, accuracy = 0.1)

#ov25
EPL_fixtures$epl_ov25 <- (
  EPL_fixtures$epl_2_1 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 + EPL_fixtures$epl_2_3 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 + EPL_fixtures$epl_0_4 +
    EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 + EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_0 +
    EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 + EPL_fixtures$epl_0_5 +
    EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 + EPL_fixtures$epl_5_5 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5 + EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 +
    EPL_fixtures$epl_4_6 + EPL_fixtures$epl_5_6 + EPL_fixtures$epl_6_6
)
#un25
EPL_fixtures$epl_un25 <- (
  EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_0 + EPL_fixtures$epl_0_1 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_0_2
)
#odds
EPL_fixtures$epl_ov25_odds <- round((1/EPL_fixtures$epl_ov25),digits = 2)
EPL_fixtures$epl_un25_odds <- round((1/EPL_fixtures$epl_un25),digits = 2)

EPL_fixtures$epl_ov25_odds
EPL_fixtures$epl_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
EPL_fixtures$epl_BTTSY <- (
  EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_3_1 + EPL_fixtures$epl_3_2 +
    EPL_fixtures$epl_2_2 + EPL_fixtures$epl_1_3 + EPL_fixtures$epl_2_3 + EPL_fixtures$epl_3_3 + EPL_fixtures$epl_4_4 +
    EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_3 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 +
    EPL_fixtures$epl_3_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 +
    EPL_fixtures$epl_5_4 + EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_6_6 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6
)
#BTTSN
EPL_fixtures$epl_BTTSN <- (
  EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_0 + EPL_fixtures$epl_0_1 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_0_2 +
    EPL_fixtures$epl_3_0 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_5_0 +
    EPL_fixtures$epl_0_5 + EPL_fixtures$epl_6_0 + EPL_fixtures$epl_0_6
)

EPL_fixtures$epl_BTTSY_odds <- round((1/EPL_fixtures$epl_BTTSY),digits = 2)
EPL_fixtures$epl_BTTSN_odds <- round((1/EPL_fixtures$epl_BTTSN),digits = 2)

EPL_fixtures$epl_BTTSY <- percent(EPL_fixtures$epl_BTTSY, accuracy = 0.1)
EPL_fixtures$epl_BTTSN <- percent(EPL_fixtures$epl_BTTSN, accuracy = 0.1)
#odds
EPL_fixtures$epl_BTTSY_odds
EPL_fixtures$epl_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
EPL_fixtures$epl_AH_0_H <- (
  EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
    EPL_fixtures$epl_5_0 +EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5 + EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6
)
#AH_0_A
EPL_fixtures$epl_AH_0_A <- (
  EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
    EPL_fixtures$epl_0_5 +EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6 + EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6
)

#odds
EPL_fixtures$epl_AH_0_H_odds <- round((1/EPL_fixtures$epl_AH_0_H),digits = 2)
EPL_fixtures$epl_AH_0_A_odds <- round((1/EPL_fixtures$epl_AH_0_A),digits = 2)

EPL_fixtures$epl_AH_0_H_odds
EPL_fixtures$epl_AH_0_A_odds
#percentages
EPL_fixtures$epl_AH_0_H <- percent(EPL_fixtures$epl_AH_0_H, accuracy = 0.1)
EPL_fixtures$epl_AH_0_A <- percent(EPL_fixtures$epl_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
EPL_fixtures$epl_AH_n075_H <- (
  EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
    EPL_fixtures$epl_5_0 +EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5
)
#AH_n075_A
EPL_fixtures$epl_AH_n075_A <- (
  EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
    EPL_fixtures$epl_0_5 +EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6
)

#odds
EPL_fixtures$epl_AH_n075_H_odds <- round((1/EPL_fixtures$epl_AH_n075_H),digits = 2)
EPL_fixtures$epl_AH_n075_A_odds <- round((1/EPL_fixtures$epl_AH_n075_A),digits = 2)

EPL_fixtures$epl_AH_n075_H_odds
EPL_fixtures$epl_AH_n075_A_odds
#percentages
EPL_fixtures$epl_AH_n075_H <- percent(EPL_fixtures$epl_AH_n075_H, accuracy = 0.1)
EPL_fixtures$epl_AH_n075_A <- percent(EPL_fixtures$epl_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
EPL_fixtures$epl_AH_075_H <- (
  EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
    EPL_fixtures$epl_5_0 +EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5 + EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6 + EPL_fixtures$epl_0_1 + EPL_fixtures$epl_1_2 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_3_4 + EPL_fixtures$epl_4_5 + EPL_fixtures$epl_5_6
)
#AH_075_A
EPL_fixtures$epl_AH_075_A <- (
  EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
    EPL_fixtures$epl_0_5 +EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6 + EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6 + EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_3 + EPL_fixtures$epl_5_4 + EPL_fixtures$epl_6_5
)

#odds
EPL_fixtures$epl_AH_075_H_odds <- round((1/EPL_fixtures$epl_AH_075_H),digits = 2)
EPL_fixtures$epl_AH_075_A_odds <- round((1/EPL_fixtures$epl_AH_075_A),digits = 2)

EPL_fixtures$epl_AH_075_H_odds
EPL_fixtures$epl_AH_075_A_odds
#percentages
EPL_fixtures$epl_AH_075_H <- percent(EPL_fixtures$epl_AH_075_H, accuracy = 0.1)
EPL_fixtures$epl_AH_075_A <- percent(EPL_fixtures$epl_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
EPL_fixtures$epl_AH_n125_H <- (
  EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
    EPL_fixtures$epl_5_0 +EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5
)
#AH_n125_A
EPL_fixtures$epl_AH_n125_A <- (
  EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
    EPL_fixtures$epl_0_5 +EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6
)

#odds
EPL_fixtures$epl_AH_n125_H_odds <- round((1/EPL_fixtures$epl_AH_n125_H),digits = 2)
EPL_fixtures$epl_AH_n125_A_odds <- round((1/EPL_fixtures$epl_AH_n125_A),digits = 2)

EPL_fixtures$epl_AH_n125_H_odds
EPL_fixtures$epl_AH_n125_A_odds
#percentages
EPL_fixtures$epl_AH_n125_H <- percent(EPL_fixtures$epl_AH_n125_H, accuracy = 0.1)
EPL_fixtures$epl_AH_n125_A <- percent(EPL_fixtures$epl_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
EPL_fixtures$epl_AH_125_H <- (
  EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
    EPL_fixtures$epl_5_0 +EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
    EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
    EPL_fixtures$epl_6_5 + EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6 + EPL_fixtures$epl_0_1 + EPL_fixtures$epl_1_2 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_3_4 + EPL_fixtures$epl_4_5 + EPL_fixtures$epl_5_6
)
#AH_125_A
EPL_fixtures$epl_AH_125_A <- (
  EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
    EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
    EPL_fixtures$epl_0_5 +EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
    EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
    EPL_fixtures$epl_5_6 + EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 +
    EPL_fixtures$epl_4_4 + EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6 + EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_1 +
    EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_3 + EPL_fixtures$epl_5_4 + EPL_fixtures$epl_6_5
)

#odds
EPL_fixtures$epl_AH_125_H_odds <- round((1/EPL_fixtures$epl_AH_125_H),digits = 2)
EPL_fixtures$epl_AH_125_A_odds <- round((1/EPL_fixtures$epl_AH_125_A),digits = 2)

EPL_fixtures$epl_AH_125_H_odds
EPL_fixtures$epl_AH_125_A_odds
#percentages
EPL_fixtures$epl_AH_125_H <- percent(EPL_fixtures$epl_AH_125_H, accuracy = 0.1)
EPL_fixtures$epl_AH_125_A <- percent(EPL_fixtures$epl_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
EPL_fixtures$epl_ov25 <- percent(EPL_fixtures$epl_ov25, accuracy = 0.1)

EPL_fixtures$epl_un25 <- percent(EPL_fixtures$epl_un25, accuracy = 0.1)
EPL_fixtures$epl_pscore <- paste(round(EPL_fixtures$epl_xGH,digits = 0),round(EPL_fixtures$epl_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
epl_last_n_games <- 6

#create final_epl_hf object
final_epl_hf <- c()
for(index_epl_hf in 1:length(epl_teams))
{
  index_epl_hf <- row.names(epl_form_h) == epl_teams[index_epl_hf]
  form_epl_hf <- epl_form_h[index_epl_hf]
  deleted_form_epl_hf <- form_epl_hf[!form_epl_hf[] == ""]
  l6_form_epl_hf <- tail(deleted_form_epl_hf,epl_last_n_games)
  l6_form_epl_hf <- paste(l6_form_epl_hf,collapse = " ")
  final_epl_hf[index_epl_hf] <- rbind(paste(epl_teams[index_epl_hf],l6_form_epl_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}

#change column nam
final_epl_hf <- as.data.frame(final_epl_hf)
colnames(final_epl_hf) <- "Form"
#goals scored
#create final_epl_gs object
final_epl_gs <- c()
suml6_epl_gs <- c()
for(index_epl_gs in 1:length(epl_teams))
{
  index_epl_gs <- row.names(epl_goalscored_h) == epl_teams[index_epl_gs]
  form_epl_gs <- epl_goalscored_h[index_epl_gs]
  deleted_form_epl_gs <- form_epl_gs[!form_epl_gs[] == ""]
  l6_form_epl_gs <- tail(deleted_form_epl_gs,epl_last_n_games)
  l6_form_epl_gs <- as.numeric(l6_form_epl_gs)
  suml6_epl_gs[index_epl_gs] <- sum(l6_form_epl_gs)
  suml6_epl_gs[index_epl_gs] <- paste("(",suml6_epl_gs[index_epl_gs],")",sep = "")
  l6_form_epl_gs <- paste(l6_form_epl_gs,collapse = " ")
  final_epl_gs[index_epl_gs] <- rbind(paste(epl_teams[index_epl_gs],l6_form_epl_gs,suml6_epl_gs[index_epl_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
final_epl_gs
#change column names
final_epl_gs <- as.data.frame(final_epl_gs)
colnames(final_epl_gs) <- "Goals scored"
#goal conceded
#create final_epl_gc object
final_epl_gc <- c()
suml6_epl_gc <- c()
for(index_epl_gc in 1:length(epl_teams))
{
  index_epl_gc <- row.names(epl_goalconceded_h) == epl_teams[index_epl_gc]
  form_epl_gc <- epl_goalconceded_h[index_epl_gc]
  deleted_form_epl_gc <- form_epl_gc[!form_epl_gc[] == ""]
  l6_form_epl_gc <- tail(deleted_form_epl_gc,epl_last_n_games)
  l6_form_epl_gc <- as.numeric(l6_form_epl_gc)
  suml6_epl_gc[index_epl_gc] <- sum(l6_form_epl_gc)
  suml6_epl_gc[index_epl_gc] <- paste("(",suml6_epl_gc[index_epl_gc],")",sep = "")
  l6_form_epl_gc <- paste(l6_form_epl_gc,collapse = " ")
  final_epl_gc[index_epl_gc] <- rbind(paste(epl_teams[index_epl_gc],l6_form_epl_gc,suml6_epl_gc[index_epl_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
#change column names
final_epl_gc <- as.data.frame(final_epl_gc)
colnames(final_epl_gc) <- "Goals conceded"


toString(l6_form_epl_gc)
#total goals
#create final_epl_tg object
final_epl_tg <- c()
suml6_epl_tg <- c()
for(index_epl_tg in 1:length(epl_teams))
{
  index_epl_tg <- row.names(epl_totalgoals_h) == epl_teams[index_epl_tg]
  form_epl_tg <- epl_totalgoals_h[index_epl_tg]
  deleted_form_epl_tg <- form_epl_tg[!form_epl_tg[] == ""]
  l6_form_epl_tg <- tail(deleted_form_epl_tg,epl_last_n_games)
  l6_form_epl_tg <- as.numeric(l6_form_epl_tg)
  suml6_epl_tg[index_epl_tg] <- sum(l6_form_epl_tg)
  suml6_epl_tg[index_epl_tg] <- paste("(",suml6_epl_tg[index_epl_tg],")",sep = "")
  l6_form_epl_tg <- paste(l6_form_epl_tg,collapse = " ")
  final_epl_tg[index_epl_tg] <- rbind(paste(epl_teams[index_epl_tg],l6_form_epl_tg,suml6_epl_tg[index_epl_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
#change column names
final_epl_tg <- as.data.frame(final_epl_tg)
colnames(final_epl_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_epl_hf object
final_epl_cs <- c()
for(index_epl_cs in 1:length(epl_teams))
{
  index_epl_cs <- row.names(epl_csform_h) == epl_teams[index_epl_cs]
  csform_epl_cs <- epl_csform_h[index_epl_cs]
  deleted_csform_epl_cs <- csform_epl_cs[!csform_epl_cs[] == ""]
  l6_csform_epl_cs <- tail(deleted_csform_epl_cs,epl_last_n_games)
  l6_csform_epl_cs <- paste(l6_csform_epl_cs,collapse = " ")
  final_epl_cs[index_epl_cs] <- rbind(paste(epl_teams[index_epl_cs],l6_csform_epl_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",epl_teams[index],l6_csform)

}

#change column names
final_epl_cs <- as.data.frame(final_epl_cs)
colnames(final_epl_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_epl_wm object
final_epl_wm <- c()
suml6_epl_wm <- c()
for(index_epl_wm in 1:length(epl_teams))
{
  index_epl_wm <- row.names(epl_winmargin_h) == epl_teams[index_epl_wm]
  form_epl_wm <- epl_winmargin_h[index_epl_wm]
  deleted_form_epl_wm <- form_epl_wm[!form_epl_wm[] == ""]
  l6_form_epl_wm <- tail(deleted_form_epl_wm,epl_last_n_games)
  l6_form_epl_wm <- as.numeric(l6_form_epl_wm)
  suml6_epl_wm[index_epl_wm] <- sum(l6_form_epl_wm)
  suml6_epl_wm[index_epl_wm] <- paste("(",suml6_epl_wm[index_epl_wm],")",sep = "")
  l6_form_epl_wm <- paste(l6_form_epl_wm,collapse = " ")
  final_epl_wm[index_epl_wm] <- rbind(paste(epl_teams[index_epl_wm],l6_form_epl_wm,suml6_epl_wm[index_epl_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
final_epl_wm
#change column names
final_epl_wm <- as.data.frame(final_epl_wm)
colnames(final_epl_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_epl_ca object
final_epl_ca <- c()
suml6_epl_ca <- c()
for(index_epl_ca in 1:length(epl_teams))
{
  index_epl_ca <- row.names(epl_coawarded_h) == epl_teams[index_epl_ca]
  form_epl_ca <- epl_coawarded_h[index_epl_ca]
  deleted_form_epl_ca <- form_epl_ca[!form_epl_ca[] == ""]
  l6_form_epl_ca <- tail(deleted_form_epl_ca,epl_last_n_games)
  l6_form_epl_ca <- as.numeric(l6_form_epl_ca)
  suml6_epl_ca[index_epl_ca] <- sum(l6_form_epl_ca)
  suml6_epl_ca[index_epl_ca] <- paste("(",suml6_epl_ca[index_epl_ca],")",sep = "")
  l6_form_epl_ca <- paste(l6_form_epl_ca,collapse = " ")
  final_epl_ca[index_epl_ca] <- rbind(paste(epl_teams[index_epl_ca],l6_form_epl_ca,suml6_epl_ca[index_epl_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
final_epl_ca
#change column names
final_epl_ca <- as.data.frame(final_epl_ca)
colnames(final_epl_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_epl_ca object
final_epl_cc <- c()
suml6_epl_cc <- c()
for(index_epl_cc in 1:length(epl_teams))
{
  index_epl_cc <- row.names(epl_cornersconceded_h) == epl_teams[index_epl_cc]
  form_epl_cc <- epl_cornersconceded_h[index_epl_cc]
  deleted_form_epl_cc <- form_epl_cc[!form_epl_cc[] == ""]
  l6_form_epl_cc <- tail(deleted_form_epl_cc,epl_last_n_games)
  l6_form_epl_cc <- as.numeric(l6_form_epl_cc)
  suml6_epl_cc[index_epl_cc] <- sum(l6_form_epl_cc)
  suml6_epl_cc[index_epl_cc] <- paste("(",suml6_epl_cc[index_epl_cc],")",sep = "")
  l6_form_epl_cc <- paste(l6_form_epl_cc,collapse = " ")
  final_epl_cc[index_epl_cc] <- rbind(paste(epl_teams[index_epl_cc],l6_form_epl_cc,suml6_epl_cc[index_epl_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
final_epl_cc
#change column names
final_epl_cc <- as.data.frame(final_epl_cc)
colnames(final_epl_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_epl_cosc <- c()
for(index_epl_cosc in 1:length(epl_teams))
{
  index_epl_cosc <- row.names(epl_coscform_h) == epl_teams[index_epl_cosc]
  coscform_epl_cosc <- epl_coscform_h[index_epl_cosc]
  deleted_coscform_epl_cosc <- coscform_epl_cosc[!coscform_epl_cosc[] == ""]
  l6_coscform_epl_cosc <- tail(deleted_coscform_epl_cosc,epl_last_n_games)
  l6_coscform_epl_cosc <- paste(l6_coscform_epl_cosc,collapse = " ")
  final_epl_cosc[index_epl_cosc] <- rbind(paste(epl_teams[index_epl_cosc],l6_coscform_epl_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",epl_teams[index],l6_coscform)

}
final_epl_cosc
#change column names
final_epl_cosc <- as.data.frame(final_epl_cosc)
colnames(final_epl_cosc) <- "CornersForm"
##################################################
#total corners
#create final_epl_tcorners object
final_epl_tcorners <- c()
suml6_epl_tcorners <- c()
for(index_epl_tcorners in 1:length(epl_teams))
{
  index_epl_tcorners <- row.names(epl_totalcorners_h) == epl_teams[index_epl_tcorners]
  form_epl_tcorners <- epl_totalcorners_h[index_epl_tcorners]
  deleted_form_epl_tcorners <- form_epl_tcorners[!form_epl_tcorners[] == ""]
  l6_form_epl_tcorners <- tail(deleted_form_epl_tcorners,epl_last_n_games)
  l6_form_epl_tcorners <- as.numeric(l6_form_epl_tcorners)
  suml6_epl_tcorners[index_epl_tcorners] <- sum(l6_form_epl_tcorners)
  suml6_epl_tcorners[index_epl_tcorners] <- paste("(",suml6_epl_tcorners[index_epl_tcorners],")",sep = "")
  l6_form_epl_tcorners <- paste(l6_form_epl_tcorners,collapse = " ")
  final_epl_tcorners[index_epl_tcorners] <- rbind(paste(epl_teams[index_epl_tcorners],l6_form_epl_tcorners,suml6_epl_tcorners[index_epl_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
#change column names
final_epl_tcorners <- as.data.frame(final_epl_tcorners)
colnames(final_epl_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_epl_hf_against
final_epl_hf_against <- c()
for(index_epl_hf_against in 1:length(epl_teams))
{
  index_epl_hf_against <- row.names(epl_form_team_against_h) == epl_teams[index_epl_hf_against]
  form_epl_hf_against <- epl_form_team_against_h[index_epl_hf_against]
  deleted_form_epl_hf_against <- form_epl_hf_against[!form_epl_hf_against[] == ""]
  l6_form_epl_hf_against <- tail(deleted_form_epl_hf_against,epl_last_n_games)
  l6_form_epl_hf_against <- paste(l6_form_epl_hf_against,collapse = " ")
  final_epl_hf_against[index_epl_hf_against] <- rbind(paste(epl_teams[index_epl_hf_against],l6_form_epl_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",epl_teams[index],l6_form)

}
final_epl_hf_against <- as.data.frame(final_epl_hf_against)
colnames(final_epl_hf_against) <- "Team against"
#combine the columns
final_epl_all <- cbind(final_epl_hf,final_epl_gs,final_epl_gc,final_epl_tg,final_epl_ca,final_epl_cc,final_epl_tcorners,final_epl_cosc,final_epl_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#EPL
EPL_sim <- EPL
EPL_sim$matchid <- paste(EPL_sim$HomeTeam,EPL_sim$AwayTeam,sep = "-")
EPL_fixtures$matchid <- paste(EPL_fixtures$HomeTeam_epl,EPL_fixtures$AwayTeam_epl,sep = "-")
EPL_fixtures$epl_FTR <- sapply(EPL_fixtures$epl_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

EPL_fixtures$epl_gamestatus <- ifelse(EPL_fixtures$matchid %in% EPL_sim$matchid,"played","notplayed")

epl_home_wins_sim <- c()
epl_away_wins_sim <- c()
epl_home_draws_sim <- c()
epl_away_draws_sim <- c()
epl_home_loss_sim <- c()
epl_away_loss_sim <- c()



for (i_epl_wins_sim in 1:length(epl_teams))
{

  epl_home_wins_sim[i_epl_wins_sim] <- nrow(EPL_fixtures[EPL_fixtures$HomeTeam_epl == epl_teams[i_epl_wins_sim] & EPL_fixtures$epl_FTR == "H" & EPL_fixtures$epl_gamestatus =="notplayed",])
  epl_away_wins_sim[i_epl_wins_sim] <- nrow(EPL_fixtures[EPL_fixtures$AwayTeam_epl == epl_teams[i_epl_wins_sim] & EPL_fixtures$epl_FTR == "A" & EPL_fixtures$epl_gamestatus == "notplayed",])
  epl_home_draws_sim[i_epl_wins_sim] <- nrow(EPL_fixtures[EPL_fixtures$HomeTeam_epl == epl_teams[i_epl_wins_sim] & EPL_fixtures$epl_FTR == "D" & EPL_fixtures$epl_gamestatus == "notplayed",])
  epl_away_draws_sim[i_epl_wins_sim] <- nrow(EPL_fixtures[EPL_fixtures$AwayTeam_epl == epl_teams[i_epl_wins_sim] & EPL_fixtures$epl_FTR == "D" & EPL_fixtures$epl_gamestatus == "notplayed",])
  epl_home_loss_sim[i_epl_wins_sim] <- nrow(EPL_fixtures[EPL_fixtures$HomeTeam_epl == epl_teams[i_epl_wins_sim] & EPL_fixtures$epl_FTR == "A" & EPL_fixtures$epl_gamestatus == "notplayed",])
  epl_away_loss_sim[i_epl_wins_sim] <- nrow(EPL_fixtures[EPL_fixtures$AwayTeam_epl == epl_teams[i_epl_wins_sim] & EPL_fixtures$epl_FTR == "H" & EPL_fixtures$epl_gamestatus == "notplayed", ])

}

epl_total_wins_sim <- epl_home_wins_sim + epl_away_wins_sim
epl_total_draws_sim <- epl_home_draws_sim + epl_away_draws_sim
epl_total_loss_sim <- epl_home_loss_sim + epl_away_loss_sim

epl_home_games_sim <- c()
epl_away_games_sim <-c()

for (i_epl_sim in 1:length(epl_teams))
{

  epl_home_games_sim[i_epl_sim] <- nrow(EPL_fixtures[EPL_fixtures$HomeTeam_epl == epl_teams[i_epl_sim] & EPL_fixtures$epl_gamestatus == "notplayed",])
  epl_away_games_sim[i_epl_sim]  <- nrow(EPL_fixtures[EPL_fixtures$AwayTeam_epl == epl_teams[i_epl_sim] & EPL_fixtures$epl_gamestatus == "notplayed",])

}

epl_games_played_sim <- epl_home_games_sim + epl_away_games_sim

epl_league_table_sim <- cbind(epl_teams,epl_games_played_sim,epl_total_wins_sim,epl_total_draws_sim,epl_total_loss_sim)
epl_PTS_sim <- (epl_total_wins_sim*3) + (epl_total_draws_sim*1)
epl_league_table_sim <- cbind(epl_league_table_sim,epl_PTS_sim)

epl_games_played_simfinal <- epl_games_played + epl_games_played_sim
epl_total_wins_simfinal <- epl_total_wins + epl_total_wins_sim
epl_total_draws_simfinal <- epl_total_draws + epl_total_draws_sim
epl_total_loss_simfinal <- epl_total_loss + epl_total_loss_sim
epl_PTS_simfinal <- epl_PTS + epl_PTS_sim

epl_league_table_simfinal <- cbind(epl_teams,epl_games_played_simfinal,epl_total_wins_simfinal,epl_total_draws_simfinal,epl_total_loss_simfinal,epl_PTS_simfinal)
epl_league_table_simfinal <- as.data.frame(epl_league_table_simfinal)
names(epl_league_table_simfinal)[names(epl_league_table_simfinal) == "epl_teams"] <- "Team_f"
names(epl_league_table_simfinal)[names(epl_league_table_simfinal) == "epl_games_played_simfinal"] <- "P_f"
names(epl_league_table_simfinal)[names(epl_league_table_simfinal) == "epl_total_wins_simfinal"] <- "W_f"
names(epl_league_table_simfinal)[names(epl_league_table_simfinal) == "epl_total_draws_simfinal"] <- "D_f"
names(epl_league_table_simfinal)[names(epl_league_table_simfinal) == "epl_total_loss_simfinal"] <- "L_f"
names(epl_league_table_simfinal)[names(epl_league_table_simfinal) == "epl_PTS_simfinal"] <- "PTS_f"
points_epl_sim <-  epl_league_table_simfinal[order(as.numeric(epl_league_table_simfinal$PTS_f), decreasing = TRUE),]

EPL_notplayed <- EPL_fixtures[EPL_fixtures$epl_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#EPL
EPL_fixtures$Hometeam_epl_index <- match(EPL_fixtures$HomeTeam_epl,epl_teams)
EPL_fixtures$Awayteam_epl_index <- match(EPL_fixtures$AwayTeam_epl,epl_teams)
epl_prediction <- c()
epl_HWM <- c()
epl_AWM <- c()
epl_HWMLM <- c()
epl_AWMLM <- c()
epl_HY <- c()
epl_AY <- c()
epl_HCO <- c()
epl_ACO <- c()
epl_HXSC <- c()
epl_AXSC <- c()
epl_HYCPF <- c()
epl_AYCPF <- c()
for(epl_row in 1:nrow(EPL_fixtures))
{

  epl_hometeamindex <- EPL_fixtures[epl_row,"Hometeam_epl_index"]
  epl_awayteamindex <- EPL_fixtures[epl_row,"Awayteam_epl_index"]
  #analyse team form
  #home team
  epl_form_vec_ht <- as.vector(epl_form_h[epl_hometeamindex,])
  epl_form_vec_ht[is.na(epl_form_vec_ht)] <- ""
  epl_form_vec_ht <- epl_form_vec_ht[epl_form_vec_ht != ""]
  epl_form_vec_ht  <-tail(epl_form_vec_ht,6)
  epl_ht_numberof_wins <- length(which(epl_form_vec_ht == "W"))
  epl_ht_numberof_draws <- length(which(epl_form_vec_ht == "D"))
  epl_ht_numberof_loss <- length(which(epl_form_vec_ht == "L"))
  #awayteam
  epl_form_vec_at <- as.vector(epl_form_h[epl_awayteamindex,])
  epl_form_vec_at[is.na(epl_form_vec_at)] <- ""
  epl_form_vec_at <- epl_form_vec_at[epl_form_vec_at != ""]
  epl_form_vec_at  <-tail(epl_form_vec_at,6)
  epl_at_numberof_wins <- length(which(epl_form_vec_at == "W"))
  epl_at_numberof_draws <- length(which(epl_form_vec_at == "D"))
  epl_at_numberof_loss <- length(which(epl_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  epl_goalscored_vec_ht <- as.vector(epl_goalscored_h[epl_hometeamindex,])
  epl_goalscored_vec_ht[is.na(epl_goalscored_vec_ht)] <- ""
  epl_goalscored_vec_ht <- epl_goalscored_vec_ht[epl_goalscored_vec_ht != ""]
  epl_goalscored_vec_ht  <-tail(epl_goalscored_vec_ht,6)
  epl_goalscored_vec_ht  <- as.numeric(epl_goalscored_vec_ht)
  epl_ht_totalgoalscored <- sum(epl_goalscored_vec_ht)
  epl_ht_matches_scoring <- length(which(epl_goalscored_vec_ht > 0))
  epl_ht_matches_without_scoring <- length(which(epl_goalscored_vec_ht == "0"))
  #awayteam
  epl_goalscored_vec_at <- as.vector(epl_goalscored_h[epl_awayteamindex,])
  epl_goalscored_vec_at[is.na(epl_goalscored_vec_at)] <- ""
  epl_goalscored_vec_at <- epl_goalscored_vec_at[epl_goalscored_vec_at != ""]
  epl_goalscored_vec_at  <-tail(epl_goalscored_vec_at,6)
  epl_goalscored_vec_at  <- as.numeric(epl_goalscored_vec_at)
  epl_at_totalgoalscored <- sum(epl_goalscored_vec_at)
  epl_at_matches_scoring <- length(which(epl_goalscored_vec_at > 0))
  epl_at_matches_without_scoring <- length(which(epl_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  epl_goalconceded_vec_ht <- as.vector(epl_goalconceded_h[epl_hometeamindex,])
  epl_goalconceded_vec_ht[is.na(epl_goalconceded_vec_ht)] <- ""
  epl_goalconceded_vec_ht <- epl_goalconceded_vec_ht[epl_goalconceded_vec_ht != ""]
  epl_goalconceded_vec_ht  <-tail(epl_goalconceded_vec_ht,6)
  epl_goalconceded_vec_ht  <- as.numeric(epl_goalconceded_vec_ht)
  epl_goalconceded_vec_ht
  epl_ht_totalgoalconceded <- sum(epl_goalconceded_vec_ht)
  epl_ht_matches_concede <- length(which(epl_goalconceded_vec_ht > 0))
  epl_ht_matches_without_concede <- length(which(epl_goalconceded_vec_ht == "0"))
  #awayteam
  epl_goalconceded_vec_at <- as.vector(epl_goalconceded_h[epl_awayteamindex,])
  epl_goalconceded_vec_at[is.na(epl_goalconceded_vec_at)] <- ""
  epl_goalconceded_vec_at <- epl_goalconceded_vec_at[epl_goalconceded_vec_at != ""]
  epl_goalconceded_vec_at  <-tail(epl_goalconceded_vec_at,6)
  epl_goalconceded_vec_at  <- as.numeric(epl_goalconceded_vec_at)
  epl_at_totalgoalconceded <- sum(epl_goalconceded_vec_at)
  epl_at_matches_concede <- length(which(epl_goalconceded_vec_at > 0))
  epl_at_matches_without_concede <- length(which(epl_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  epl_totalgoals_vec_ht <- as.vector(epl_totalgoals_h[epl_hometeamindex,])
  epl_totalgoals_vec_ht[is.na(epl_totalgoals_vec_ht)] <- ""
  epl_totalgoals_vec_ht <- epl_totalgoals_vec_ht[epl_totalgoals_vec_ht != ""]
  epl_totalgoals_vec_ht  <-tail(epl_totalgoals_vec_ht,6)
  epl_totalgoals_vec_ht  <- as.numeric(epl_totalgoals_vec_ht)
  epl_totalgoals_vec_ht
  epl_ht_totalgoals <- sum(epl_totalgoals_vec_ht)
  epl_ht_avgtotalgoals <- (epl_ht_totalgoals/6)
  epl_ht_no_of_ov25 <- length(which(epl_totalgoals_vec_ht >= 3))
  epl_ht_no_of_un25 <- length(which(epl_totalgoals_vec_ht <= 2))
  #awayteam
  epl_totalgoals_vec_at <- as.vector(epl_totalgoals_h[epl_awayteamindex,])
  epl_totalgoals_vec_at[is.na(epl_totalgoals_vec_at)] <- ""
  epl_totalgoals_vec_at <- epl_totalgoals_vec_at[epl_totalgoals_vec_at != ""]
  epl_totalgoals_vec_at  <-tail(epl_totalgoals_vec_at,6)
  epl_totalgoals_vec_at  <- as.numeric(epl_totalgoals_vec_at)
  epl_totalgoals_vec_at
  epl_at_totalgoals <- sum(epl_totalgoals_vec_at)
  epl_at_avgtotalgoals <- (epl_at_totalgoals/6)
  epl_at_no_of_ov25 <- length(which(epl_totalgoals_vec_at >= 3))
  epl_at_no_of_un25 <- length(which(epl_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  epl_winmargin_vec_ht <- as.vector(epl_winmargin_h[epl_hometeamindex,])
  epl_winmargin_vec_ht[is.na(epl_winmargin_vec_ht)] <- ""
  epl_winmargin_vec_ht <- epl_winmargin_vec_ht[epl_winmargin_vec_ht != ""]
  epl_winmargin_vec_ht  <-tail(epl_winmargin_vec_ht,6)
  epl_winmargin_vec_ht  <- as.numeric(epl_winmargin_vec_ht)

  epl_ht_totalwinmargin <- sum(epl_winmargin_vec_ht)
  epl_ht_no_of_winmargin_ov0 <- length(which(epl_winmargin_vec_ht >= 0))
  epl_ht_no_of_winmargin_ov1 <- length(which(epl_winmargin_vec_ht >= 1))
  epl_ht_no_of_winmargin_un0 <- length(which(epl_winmargin_vec_ht <= 0))
  epl_ht_no_of_winmargin_un1 <- length(which(epl_winmargin_vec_ht <= 1))
  #awayteam
  epl_winmargin_vec_at <- as.vector(epl_winmargin_h[epl_awayteamindex,])
  epl_winmargin_vec_at[is.na(epl_winmargin_vec_at)] <- ""
  epl_winmargin_vec_at <- epl_winmargin_vec_at[epl_winmargin_vec_at != ""]
  epl_winmargin_vec_at  <-tail(epl_winmargin_vec_at,6)
  epl_winmargin_vec_at  <- as.numeric(epl_winmargin_vec_at)

  epl_at_totalwinmargin <- sum(epl_winmargin_vec_at)
  epl_at_no_of_winmargin_ov0 <- length(which(epl_winmargin_vec_at >= 0))
  epl_at_no_of_winmargin_ov1 <- length(which(epl_winmargin_vec_at >= 1))
  epl_at_no_of_winmargin_un0 <- length(which(epl_winmargin_vec_at <= 0))
  epl_at_no_of_winmargin_un1 <- length(which(epl_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  epl_winmargin_vec_ht_lm <- as.vector(epl_winmargin_h[epl_hometeamindex,])
  epl_winmargin_vec_ht_lm[is.na(epl_winmargin_vec_ht_lm)] <- ""
  epl_winmargin_vec_ht_lm <- epl_winmargin_vec_ht_lm[epl_winmargin_vec_ht_lm != ""]
  epl_winmargin_vec_ht_lm  <-tail(epl_winmargin_vec_ht_lm,1)
  #awayteam
  epl_winmargin_vec_at_lm <- as.vector(epl_winmargin_h[epl_awayteamindex,])
  epl_winmargin_vec_at_lm[is.na(epl_winmargin_vec_at_lm)] <- ""
  epl_winmargin_vec_at_lm <- epl_winmargin_vec_at_lm[epl_winmargin_vec_at_lm != ""]
  epl_winmargin_vec_at_lm  <-tail(epl_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  epl_yellowtotals_vec_ht <- as.vector(epl_yellowtotalsv2[epl_hometeamindex,])
  epl_yellowtotals_vec_ht[is.na(epl_yellowtotals_vec_ht)] <- ""
  epl_yellowtotals_vec_ht <- epl_yellowtotals_vec_ht[epl_yellowtotals_vec_ht != ""]
  epl_yellowtotals_vec_ht  <-tail(epl_yellowtotals_vec_ht,1)
  #awayteam
  epl_yellowtotals_vec_at <- as.vector(epl_yellowtotalsv2[epl_awayteamindex,])
  epl_yellowtotals_vec_at[is.na(epl_yellowtotals_vec_at)] <- ""
  epl_yellowtotals_vec_at <- epl_yellowtotals_vec_at[epl_yellowtotals_vec_at != ""]
  epl_yellowtotals_vec_at  <-tail(epl_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  epl_cornertotals_vec_ht <- as.vector(epl_cornertotalsv2[epl_hometeamindex,])
  epl_cornertotals_vec_ht[is.na(epl_cornertotals_vec_ht)] <- ""
  epl_cornertotals_vec_ht <- epl_cornertotals_vec_ht[epl_cornertotals_vec_ht != ""]
  epl_cornertotals_vec_ht  <-tail(epl_cornertotals_vec_ht,1)
  #awayteam
  epl_cornertotals_vec_at <- as.vector(epl_cornertotalsv2[epl_awayteamindex,])
  epl_cornertotals_vec_at[is.na(epl_cornertotals_vec_at)] <- ""
  epl_cornertotals_vec_at <- epl_cornertotals_vec_at[epl_cornertotals_vec_at != ""]
  epl_cornertotals_vec_at  <-tail(epl_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  epl_xshotsconversion_vec_ht <- as.vector(epl_shots_analysis[epl_hometeamindex,])
  epl_xshotsconversion_vec_ht[is.na(epl_xshotsconversion_vec_ht)] <- ""
  epl_xshotsconversion_vec_ht <- epl_xshotsconversion_vec_ht[epl_xshotsconversion_vec_ht != ""]
  epl_xshotsconversion_vec_ht  <-tail(epl_xshotsconversion_vec_ht,1)
  #awayteam
  epl_xshotsconversion_vec_at <- as.vector(epl_shots_analysis[epl_awayteamindex,])
  epl_xshotsconversion_vec_at[is.na(epl_xshotsconversion_vec_at)] <- ""
  epl_xshotsconversion_vec_at <- epl_xshotsconversion_vec_at[epl_xshotsconversion_vec_at != ""]
  epl_xshotsconversion_vec_at  <-tail(epl_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  epl_fouls_conversion_vec_ht <- as.vector(epl_fouls_conversion[epl_hometeamindex,])
  epl_fouls_conversion_vec_ht[is.na(epl_fouls_conversion_vec_ht)] <- ""
  epl_fouls_conversion_vec_ht <- epl_fouls_conversion_vec_ht[epl_fouls_conversion_vec_ht != ""]
  epl_fouls_conversion_vec_ht  <-tail(epl_fouls_conversion_vec_ht,1)
  #awayteam
  epl_fouls_conversion_vec_at <- as.vector(epl_fouls_conversion[epl_awayteamindex,])
  epl_fouls_conversion_vec_at[is.na(epl_fouls_conversion_vec_at)] <- ""
  epl_fouls_conversion_vec_at <- epl_fouls_conversion_vec_at[epl_fouls_conversion_vec_at != ""]
  epl_fouls_conversion_vec_at  <-tail(epl_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  epl_ht_last6points <- epl_ht_numberof_wins*3 + epl_ht_numberof_draws*1
  epl_at_last6points <- epl_at_numberof_wins*3 + epl_at_numberof_draws*1

  if(epl_ht_last6points > epl_at_last6points) {epl_3waypick <- "1"}  else {epl_3waypick <- "X2"}

  if(epl_at_last6points > epl_ht_last6points ) {epl_3waypick <- "2"} else {epl_3waypick <- "1X"}

  if(epl_ht_no_of_ov25 + epl_at_no_of_ov25 >= 6) {epl_goalspick <- "ov25"} else {epl_goalspick <- "un25"}

  if(epl_ht_no_of_un25 + epl_at_no_of_un25 >= 6) {epl_goalspick <- "un25"} else {epl_goalspick <- "ov25"}

  if(epl_ht_matches_scoring >= 4 && epl_at_matches_scoring >=4) {epl_btts <- "BTTS-Y"} else {epl_btts <- "BTTS-N"}


  epl_prediction[epl_row] <- rbind(paste(epl_3waypick,epl_goalspick,epl_btts,sep = ","))
  epl_HWM[epl_row] <- epl_ht_totalwinmargin
  epl_AWM[epl_row] <- epl_at_totalwinmargin

  epl_HWMLM[epl_row] <- epl_winmargin_vec_ht_lm
  epl_AWMLM[epl_row] <- epl_winmargin_vec_at_lm

  epl_HY[epl_row] <- epl_yellowtotals_vec_ht
  epl_AY[epl_row] <- epl_yellowtotals_vec_at

  epl_HCO[epl_row] <- epl_cornertotals_vec_ht
  epl_ACO[epl_row] <- epl_cornertotals_vec_at

  epl_HXSC[epl_row] <- epl_xshotsconversion_vec_ht
  epl_AXSC[epl_row] <- epl_xshotsconversion_vec_at

  epl_HYCPF[epl_row] <- epl_fouls_conversion_vec_ht
  epl_AYCPF[epl_row] <- epl_fouls_conversion_vec_at
}

epl_prediction <- as.data.frame(epl_prediction)
colnames(epl_prediction) <- "prediction"

epl_HWM <- as.data.frame(epl_HWM)
colnames(epl_HWM) <- "HWM"

epl_AWM <- as.data.frame(epl_AWM)
colnames(epl_AWM) <- "AWM"

epl_HWMLM <- as.data.frame(epl_HWMLM)
colnames(epl_HWMLM) <- "HWMLM"

epl_AWMLM <- as.data.frame(epl_AWMLM)
colnames(epl_AWMLM) <- "AWMLM"

epl_HY <- as.data.frame(epl_HY)
colnames(epl_HY) <- "AVGHY"

epl_AY <- as.data.frame(epl_AY)
colnames(epl_AY) <- "AVGAY"

epl_HCO <- as.data.frame(epl_HCO)
colnames(epl_HCO) <- "AVGHCO"

epl_ACO <- as.data.frame(epl_ACO)
colnames(epl_ACO) <- "AVGACO"

epl_HXSC <- as.data.frame(epl_HXSC)
colnames(epl_HXSC) <- "HXSC"

epl_AXSC <- as.data.frame(epl_AXSC)
colnames(epl_AXSC) <- "AXSC"

epl_HYCPF <- as.data.frame(epl_HYCPF)
colnames(epl_HYCPF) <- "HYCPF"

epl_AYCPF <- as.data.frame(epl_AYCPF)
colnames(epl_AYCPF) <- "AYCPF"

epl_picks <- cbind(EPL_fixtures$Div,EPL_fixtures$HomeTeam_epl,EPL_fixtures$AwayTeam_epl,epl_prediction,epl_HWM,epl_AWM,epl_HWMLM,epl_AWMLM,epl_HY,epl_AY,epl_HCO,epl_ACO,epl_HXSC,epl_AXSC,epl_HYCPF,epl_AYCPF)

colnames(epl_picks)[1] <- "picks_Div"
colnames(epl_picks)[2] <- "picks_HomeTeam"
colnames(epl_picks)[3] <- "picks_AwayTeam"
epl_picks$matchid <- paste(epl_picks$picks_HomeTeam,epl_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of EPL
epl_picks
#############################################################################################################################################################################
#clone fixtures
EPL_fixtures_clone <- EPL_fixtures
colnames(EPL_fixtures_clone)[61] <- "Hwin"
colnames(EPL_fixtures_clone)[62] <- "Draw"
colnames(EPL_fixtures_clone)[63] <- "Awin"

EPL_fixtures_clone$Hwinodds <-   EPL_fixtures$epl_1_0 + EPL_fixtures$epl_2_0 + EPL_fixtures$epl_2_1 + EPL_fixtures$epl_3_0 + EPL_fixtures$epl_3_1 +
  EPL_fixtures$epl_3_2 + EPL_fixtures$epl_4_0 + EPL_fixtures$epl_4_1 + EPL_fixtures$epl_4_2 + EPL_fixtures$epl_4_3 +
  EPL_fixtures$epl_5_0 + EPL_fixtures$epl_5_1 + EPL_fixtures$epl_5_2 + EPL_fixtures$epl_5_3 + EPL_fixtures$epl_5_4 +
  EPL_fixtures$epl_6_0 + EPL_fixtures$epl_6_1 + EPL_fixtures$epl_6_2 + EPL_fixtures$epl_6_3 + EPL_fixtures$epl_6_4 +
  EPL_fixtures$epl_6_5
EPL_fixtures_clone$Hwinodds <- round(1/EPL_fixtures_clone$Hwinodds, digits = 3)

EPL_fixtures_clone$Drawodds <-  EPL_fixtures$epl_0_0 + EPL_fixtures$epl_1_1 + EPL_fixtures$epl_2_2 + EPL_fixtures$epl_3_3 + EPL_fixtures$epl_4_4 +
  EPL_fixtures$epl_5_5 + EPL_fixtures$epl_6_6

EPL_fixtures_clone$Drawodds <- round(1/EPL_fixtures_clone$Drawodds, digits = 3)

EPL_fixtures_clone$Awinodds <-   EPL_fixtures$epl_0_1 + EPL_fixtures$epl_0_2 + EPL_fixtures$epl_1_2 + EPL_fixtures$epl_0_3 + EPL_fixtures$epl_1_3 +
  EPL_fixtures$epl_2_3 + EPL_fixtures$epl_0_4 + EPL_fixtures$epl_1_4 + EPL_fixtures$epl_2_4 + EPL_fixtures$epl_3_4 +
  EPL_fixtures$epl_0_5 + EPL_fixtures$epl_1_5 + EPL_fixtures$epl_2_5 + EPL_fixtures$epl_3_5 + EPL_fixtures$epl_4_5 +
  EPL_fixtures$epl_0_6 + EPL_fixtures$epl_1_6 + EPL_fixtures$epl_2_6 + EPL_fixtures$epl_3_6 + EPL_fixtures$epl_4_6 +
  EPL_fixtures$epl_5_6

EPL_fixtures_clone$Awinodds <- round(1/EPL_fixtures_clone$Awinodds, digits = 3)

colnames(EPL_fixtures_clone)[15] <- "CS_1-1"
colnames(EPL_fixtures_clone)[13] <- "CS_1-0"
colnames(EPL_fixtures_clone)[14] <- "CS_0-1"
colnames(EPL_fixtures_clone)[16] <- "CS_2-0"
colnames(EPL_fixtures_clone)[17] <- "CS_0-2"
colnames(EPL_fixtures_clone)[19] <- "CS_2-1"
colnames(EPL_fixtures_clone)[20] <- "CS_1-2"

EPL_fixtures_clone$`CS_1-1` <- round(1/EPL_fixtures_clone$`CS_1-1`, digits = 3)
EPL_fixtures_clone$`CS_1-0` <- round(1/EPL_fixtures_clone$`CS_1-0`, digits = 3)
EPL_fixtures_clone$`CS_0-1` <- round(1/EPL_fixtures_clone$`CS_0-1`, digits = 3)
EPL_fixtures_clone$`CS_2-0` <- round(1/EPL_fixtures_clone$`CS_2-0`, digits = 3)
EPL_fixtures_clone$`CS_0-2` <- round(1/EPL_fixtures_clone$`CS_0-2`, digits = 3)
EPL_fixtures_clone$`CS_2-1` <- round(1/EPL_fixtures_clone$`CS_2-1`, digits = 3)
EPL_fixtures_clone$`CS_1-2` <- round(1/EPL_fixtures_clone$`CS_1-2`, digits = 3)

colnames(EPL_fixtures_clone)[1] <- "league"
colnames(EPL_fixtures_clone)[2] <- "Hometeam"
colnames(EPL_fixtures_clone)[3] <- "Awayteam"
colnames(EPL_fixtures_clone)[92] <- "predscore"
colnames(EPL_fixtures_clone)[64] <- "ov25"
colnames(EPL_fixtures_clone)[66] <- "ov25odds"
colnames(EPL_fixtures_clone)[65] <- "un25"
colnames(EPL_fixtures_clone)[67] <- "un25odds"
colnames(EPL_fixtures_clone)[68] <- "BTTSY"
colnames(EPL_fixtures_clone)[69] <- "BTTSN"
colnames(EPL_fixtures_clone)[70] <- "BTTSYodds"
colnames(EPL_fixtures_clone)[71] <- "BTTSNodds"

EPL_fixtures_clone <- EPL_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
EPL_fixtures_clone$matchid <- paste(EPL_fixtures_clone$Hometeam,EPL_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
EPL_fixtures_clone_final <- EPL_fixtures_clone[,-c(8,9,10,27)]
EPL_fixtures_clone_final[,'sep'] <- ''

epl_dmprediction <-  epl_picks[,c(4,5,6,7,8)]
epl_dmprediction[,'sep2'] <- ''

epl_avgyellow <- epl_picks[,c(9,10)]
epl_avgyellow[,'sep3'] <- ''

epl_avgcorners <- epl_picks[,c(11,12)]
epl_avgcorners[,'sep4'] <- ''

epl_goals <- EPL_fixtures[,c(10,11)]
epl_goals$epl_xGH <- round(epl_goals$epl_xGH, digits = 2)
epl_goals$epl_xGA <- round(epl_goals$epl_xGA, digits = 2)
epl_goals$epl_TxG <- epl_goals$epl_xGH + epl_goals$epl_xGA
epl_goals[,'sep5'] <- ''

epl_shots <- EPL_fixtures_sot[,c(10,11)]
epl_shots$epl_xHST <- round(epl_shots$epl_xHST, digits = 2)
epl_shots$epl_xAST <- round(epl_shots$epl_xAST, digits = 2)
epl_shots$TxSOT <- epl_shots$epl_xHST + epl_shots$epl_xAST
epl_shots[,'sep6'] <- ''

epl_fouls <- EPL_fixtures_fo[,c(10,11)]
epl_fouls$epl_xHF <- round(epl_fouls$epl_xHF, digits = 2)
epl_fouls$epl_xAF <- round(epl_fouls$epl_xAF, digits = 2)
epl_fouls$epl_TxF <- epl_fouls$epl_xHF + epl_fouls$epl_xAF

epl_ycpf <- epl_picks[,c(15,16)]
epl_fouls <- cbind(epl_fouls,epl_ycpf)
epl_fouls$HYCPF <- as.numeric(epl_fouls$HYCPF)
epl_fouls$AYCPF <- as.numeric(epl_fouls$AYCPF)
epl_fouls$x_hyc <- (epl_fouls$epl_xHF) * (epl_fouls$HYCPF)
epl_fouls$x_ayc <- (epl_fouls$epl_xAF) * (epl_fouls$AYCPF)
epl_fouls$x_TYC <- round((epl_fouls$x_hyc + epl_fouls$x_ayc),digits = 2)
epl_fouls[,'sep7'] <- ''

epl_bookings <- EPL_fixtures_yc[,c(10,11)]
epl_bookings$epl_xHYC <- round(epl_bookings$epl_xHYC, digits = 2)
epl_bookings$epl_xAYC <- round(epl_bookings$epl_xAYC, digits = 2)
epl_bookings$epl_TYcards <- epl_bookings$epl_xHYC + epl_bookings$epl_xAYC
epl_bookings[,'sep8'] <- ''

epl_corners <- EPL_fixtures_co[,c(10,11)]
epl_corners$epl_xHCOC <- round(epl_corners$epl_xHCOC, digits = 2)
epl_corners$epl_xACOC <- round(epl_corners$epl_xACOC, digits = 2)
epl_corners$epl_TCOs <- epl_corners$epl_xHCOC + epl_corners$epl_xACOC
epl_corners[,'sep9'] <- ''

epl_shotsconversion <- epl_picks[,c(13,14)]
epl_shotsconversion <- cbind(epl_shotsconversion,epl_shots)
epl_shotsconversion$HXSC <- as.numeric(epl_shotsconversion$HXSC)
epl_shotsconversion$AXSC <- as.numeric(epl_shotsconversion$AXSC)
epl_shotsconversion$epl_hXgoals <- round((epl_shotsconversion$HXSC * epl_shotsconversion$epl_xHST), digits = 2)
epl_shotsconversion$epl_aXgoals <- round((epl_shotsconversion$AXSC * epl_shotsconversion$epl_xAST), digits = 2)
epl_shotsconversion$Xgoals <- epl_shotsconversion$epl_hXgoals + epl_shotsconversion$epl_aXgoals
options(java.parameters = "-Xmx4g")
EPL_all <- cbind(EPL_fixtures_clone_final,epl_dmprediction,epl_avgyellow,epl_avgcorners,epl_goals,epl_shots,epl_fouls,epl_bookings,epl_corners,epl_shotsconversion)
unlink('Divisions/EPL.xlsx')
write.xlsx(EPL_all,'Divisions/EPL.xlsx', sheetName = "EPL_all", append = TRUE)
write.xlsx(points_epl,'Divisions/EPL.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(epl_cornertotalsv2,'Divisions/EPL.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(epl_goaltotalsv2,'Divisions/EPL.xlsx', sheetName = "Goaltotals", append = TRUE)


##write.csv(EPL_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
