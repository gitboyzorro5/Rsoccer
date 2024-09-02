#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
sp1_currentround

first_df <- E1_rounds[E1_rounds$e1_matchday > 40,]
second_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
third_df <- E3_rounds[E3_rounds$e3_matchday > 40,]
first_df <- first_df[,-37]
second_df <- second_df[,-37]
third_df <- third_df[,-37]
EFL <- rbind(first_df,second_df,third_df)
#EFL <- SP1_rounds[SP1_rounds$sp1_matchday > 27,]
#EFL <- na.omit(EFL)
#goaltotals v2
efl_goaltotalsv2 <- tapply(EFL$TG, EFL[c("HomeTeam", "AwayTeam")],mean)
efl_hgtotals <- rowSums(efl_goaltotalsv2, na.rm = T)
efl_agtotals <- colSums(efl_goaltotalsv2, na.rm = T)
efl_goaltotalsv2 <- cbind(efl_goaltotalsv2,efl_hgtotals,efl_agtotals)
efl_totalgoals <- efl_hgtotals + efl_agtotals
efl_goaltotalsv2 <- cbind(efl_goaltotalsv2,efl_totalgoals)
efl_teams <- sort(unique(EFL$HomeTeam))
efl_home_games <- c()
efl_away_games <-c()
for (i_efl in 1:length(efl_teams))
{

  efl_home_games[i_efl] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl],])
  efl_away_games[i_efl]  <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl],])

}
efl_games_played <- efl_home_games + efl_away_games
efl_goaltotalsv2 <- cbind(efl_goaltotalsv2,efl_games_played)
efl_avg_totalgoals <- round((efl_totalgoals/ efl_games_played), digits = 4)
efl_goaltotalsv2[is.na(efl_goaltotalsv2)] <- ""
efl_goaltotalsv2 <- cbind(efl_goaltotalsv2,efl_avg_totalgoals)

############################################################################################################
#Cornertotals v2
efl_cornertotalsv2 <- tapply(EFL$TC, EFL[c("HomeTeam", "AwayTeam")],mean)
efl_hcototals <- rowSums(efl_cornertotalsv2, na.rm = T)
efl_acototals <- colSums(efl_cornertotalsv2, na.rm = T)
efl_cornertotalsv2 <- cbind(efl_cornertotalsv2,efl_hcototals,efl_acototals)
efl_totalcorners <- efl_hcototals + efl_acototals
efl_cornertotalsv2 <- cbind(efl_cornertotalsv2,efl_totalcorners)
efl_cornertotalsv2 <- cbind(efl_cornertotalsv2,efl_games_played)
efl_avg_totalcorners <- round((efl_totalcorners/ efl_games_played), digits = 4)
efl_cornertotalsv2[is.na(efl_cornertotalsv2)] <- ""
efl_cornertotalsv2 <- cbind(efl_cornertotalsv2,efl_avg_totalcorners)
############################################################################################################
#GS matrix
efl_goalscored_h <- tapply(EFL$FTHG, EFL[c("HomeTeam", "Date")],mean)
efl_goalscored_a <- tapply(EFL$FTAG, EFL[c("AwayTeam", "Date")],mean)
efl_goalscored_h[is.na(efl_goalscored_h)] <- ""
efl_goalscored_a[is.na(efl_goalscored_a)] <- ""
for(efl_rowhgs in 1:nrow(efl_goalscored_h)) {
  for(efl_colhgs in 1:ncol(efl_goalscored_h)) {

    # print(my_matrix[row, col])
    for(efl_rowags in 1:nrow(efl_goalscored_a)) {
      for(efl_colags in 1:ncol(efl_goalscored_a)) {
        ifelse(!efl_goalscored_a[efl_rowags,efl_colags]=="",efl_goalscored_h[efl_rowags,efl_colags] <- efl_goalscored_a[efl_rowags,efl_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
efl_goalconceded_h <- tapply(EFL$FTAG, EFL[c("HomeTeam", "Date")],mean)
efl_goalconceded_a <- tapply(EFL$FTHG, EFL[c("AwayTeam", "Date")],mean)
efl_goalconceded_h[is.na(efl_goalconceded_h)] <- ""
efl_goalconceded_a[is.na(efl_goalconceded_a)] <- ""
for(efl_rowhgc in 1:nrow(efl_goalconceded_h)) {
  for(efl_colhgc in 1:ncol(efl_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(efl_rowagc in 1:nrow(efl_goalconceded_a)) {
      for(efl_colagc in 1:ncol(efl_goalconceded_a)) {
        ifelse(!efl_goalconceded_a[efl_rowagc,efl_colagc]=="",efl_goalconceded_h[efl_rowagc,efl_colagc] <- efl_goalconceded_a[efl_rowagc,efl_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
efl_totalcorners_h <- tapply(EFL$TC, EFL[c("HomeTeam", "Date")],mean)
efl_totalcorners_a <- tapply(EFL$TC, EFL[c("AwayTeam", "Date")],mean)
efl_totalcorners_h[is.na(efl_totalcorners_h)] <- ""
efl_totalcorners_a[is.na(efl_totalcorners_a)] <- ""
#EFL
for(efl_rowTC in 1:nrow(efl_totalcorners_h)) {
  for(efl_colTC in 1:ncol(efl_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(efl_rowTC in 1:nrow(efl_totalcorners_a)) {
      for(efl_colTC in 1:ncol(efl_totalcorners_a)) {
        ifelse(!efl_totalcorners_a[efl_rowTC,efl_colTC]=="",efl_totalcorners_h[efl_rowTC,efl_colTC] <- efl_totalcorners_a[efl_rowTC,efl_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
efl_coawarded_h <- tapply(EFL$HCO, EFL[c("HomeTeam", "Date")],mean)
efl_coawarded_a <- tapply(EFL$ACO, EFL[c("AwayTeam", "Date")],mean)
efl_coawarded_h[is.na(efl_coawarded_h)] <- ""
efl_coawarded_a[is.na(efl_coawarded_a)] <- ""
#EFL
for(efl_rowhco in 1:nrow(efl_coawarded_h)) {
  for(efl_colhco in 1:ncol(efl_coawarded_h)) {

    # print(my_matrix[row, col])
    for(efl_rowaco in 1:nrow(efl_coawarded_a)) {
      for(efl_colaco in 1:ncol(efl_coawarded_a)) {
        ifelse(!efl_coawarded_a[efl_rowaco,efl_colaco]=="",efl_coawarded_h[efl_rowaco,efl_colaco] <- efl_coawarded_a[efl_rowaco,efl_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
efl_cornersconceded_h <- tapply(EFL$ACO, EFL[c("HomeTeam", "Date")],mean)
efl_cornersconceded_a <- tapply(EFL$HCO, EFL[c("AwayTeam", "Date")],mean)
efl_cornersconceded_h[is.na(efl_cornersconceded_h)] <- ""
efl_cornersconceded_a[is.na(efl_cornersconceded_a)] <- ""
#EFL
for(efl_rowhcc in 1:nrow(efl_cornersconceded_h)) {
  for(efl_colhcc in 1:ncol(efl_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(efl_rowacc in 1:nrow(efl_cornersconceded_a)) {
      for(efl_colacc in 1:ncol(efl_cornersconceded_a)) {
        ifelse(!efl_cornersconceded_a[efl_rowacc,efl_colacc]=="",efl_cornersconceded_h[efl_rowacc,efl_colacc] <- efl_cornersconceded_a[efl_rowacc,efl_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
efl_coscform_h <- tapply(EFL$COSC, EFL[c("HomeTeam", "Date")],median)
efl_coscform_a <- tapply(EFL$COSC, EFL[c("AwayTeam", "Date")],median)
efl_coscform_h[is.na(efl_coscform_h)] <- ""
efl_coscform_a[is.na(efl_coscform_a)] <- ""
#EFL
for(efl_rowh_f_cosc in 1:nrow(efl_coscform_h)) {
  for(efl_colh_f_cosc in 1:ncol(efl_coscform_h)) {

    # print(my_matrix[row, col])
    for(efl_rowa_f_cosc in 1:nrow(efl_coscform_a)) {
      for(efl_cola_f_cosc in 1:ncol(efl_coscform_a)) {
        ifelse(!efl_coscform_a[efl_rowa_f_cosc,efl_cola_f_cosc]=="",efl_coscform_h[efl_rowa_f_cosc,efl_cola_f_cosc] <- efl_coscform_a[efl_rowa_f_cosc,efl_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
efl_winmargin_h <- tapply(EFL$FTHG - EFL$FTAG, EFL[c("HomeTeam", "Date")],mean)
efl_winmargin_a <- tapply(EFL$FTAG - EFL$FTHG, EFL[c("AwayTeam", "Date")],mean)
efl_winmargin_h[is.na(efl_winmargin_h)] <- ""
efl_winmargin_a[is.na(efl_winmargin_a)] <- ""
#EFL
for(efl_rowhwm in 1:nrow(efl_winmargin_h)) {
  for(efl_colhwm in 1:ncol(efl_winmargin_h)) {

    # print(my_matrix[row, col])
    for(efl_rowawm in 1:nrow(efl_winmargin_a)) {
      for(efl_colawm in 1:ncol(efl_winmargin_a)) {
        ifelse(!efl_winmargin_a[efl_rowawm,efl_colawm]=="",efl_winmargin_h[efl_rowawm,efl_colawm] <- efl_winmargin_a[efl_rowawm,efl_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
efl_yellowscored_h <- tapply(EFL$HY, EFL[c("HomeTeam", "Date")],mean)
efl_yellowscored_a <- tapply(EFL$AY, EFL[c("AwayTeam", "Date")],mean)
efl_yellowscored_h[is.na(efl_yellowscored_h)] <- ""
efl_yellowscored_a[is.na(efl_yellowscored_a)] <- ""
#EFL
for(efl_rowhys in 1:nrow(efl_yellowscored_h)) {
  for(efl_colhys in 1:ncol(efl_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(efl_roways in 1:nrow(efl_yellowscored_a)) {
      for(efl_colays in 1:ncol(efl_yellowscored_a)) {
        ifelse(!efl_yellowscored_a[efl_roways,efl_colays]=="",efl_yellowscored_h[efl_roways,efl_colays] <- efl_yellowscored_a[efl_roways,efl_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
efl_redscored_h <- tapply(EFL$HR, EFL[c("HomeTeam", "Date")],mean)
efl_redscored_a <- tapply(EFL$AR, EFL[c("AwayTeam", "Date")],mean)
efl_redscored_h[is.na(efl_redscored_h)] <- ""
efl_redscored_a[is.na(efl_redscored_a)] <- ""
for(efl_rowhrs in 1:nrow(efl_redscored_h)) {
  for(efl_colhrs in 1:ncol(efl_redscored_h)) {

    # print(my_matrix[row, col])
    for(efl_rowars in 1:nrow(efl_redscored_a)) {
      for(efl_colars in 1:ncol(efl_redscored_a)) {
        ifelse(!efl_redscored_a[efl_rowars,efl_colars]=="",efl_redscored_h[efl_rowars,efl_colars] <- efl_redscored_a[efl_rowars,efl_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
efl_redtotalsv2 <- tapply(EFL$TR, EFL[c("HomeTeam", "AwayTeam")],mean)
efl_hrtotals <- rowSums(efl_redtotalsv2, na.rm = T)
efl_artotals <- colSums(efl_redtotalsv2, na.rm = T)
efl_redtotalsv2 <- cbind(efl_redtotalsv2,efl_hrtotals,efl_artotals)
efl_totalreds <- efl_hrtotals + efl_artotals
efl_redtotalsv2 <- cbind(efl_redtotalsv2,efl_totalreds)
efl_redtotalsv2 <- cbind(efl_redtotalsv2,efl_games_played)
efl_avg_totalreds <- round((efl_totalreds/ efl_games_played), digits = 4)
efl_redtotalsv2[is.na(efl_redtotalsv2)] <- ""
efl_redtotalsv2 <- cbind(efl_redtotalsv2,efl_avg_totalreds)
############################################################################################################################################################
#yellowtotals
efl_yellowtotalsv2 <- tapply(EFL$TY, EFL[c("HomeTeam", "AwayTeam")],mean)
efl_hytotals <- rowSums(efl_yellowtotalsv2, na.rm = T)
efl_aytotals <- colSums(efl_yellowtotalsv2, na.rm = T)
efl_yellowtotalsv2 <- cbind(efl_yellowtotalsv2,efl_hytotals,efl_aytotals)
efl_totalyellows <- efl_hytotals + efl_aytotals
efl_yellowtotalsv2 <- cbind(efl_yellowtotalsv2,efl_totalyellows)
efl_yellowtotalsv2 <- cbind(efl_yellowtotalsv2,efl_games_played)
efl_avg_totalyellows <- round((efl_totalyellows/ efl_games_played), digits = 4)
efl_yellowtotalsv2[is.na(efl_yellowtotalsv2)] <- ""
efl_yellowtotalsv2 <- cbind(efl_yellowtotalsv2,efl_avg_totalyellows)
##################################################################################################################################################
#team form
efl_form_h <- tapply(EFL$FTR, EFL[c("HomeTeam", "Date")],median)
efl_form_a <- tapply(EFL$FTR, EFL[c("AwayTeam", "Date")],median)
efl_form_h[is.na(efl_form_h)] <- ""
efl_form_a[is.na(efl_form_a)] <- ""
efl_form_h <- sub("A","L",efl_form_h)
efl_form_h <- sub("H","W",efl_form_h)
efl_form_a <- sub("A","W",efl_form_a)
efl_form_a <- sub("H","L",efl_form_a)
for(efl_rowh_f in 1:nrow(efl_form_h)) {
  for(efl_colh_f in 1:ncol(efl_form_h)) {

    # print(my_matrix[row, col])
    for(efl_rowa_f in 1:nrow(efl_form_a)) {
      for(efl_cola_f in 1:ncol(efl_form_a)) {
        ifelse(!efl_form_a[efl_rowa_f,efl_cola_f]=="",efl_form_h[efl_rowa_f,efl_cola_f] <- efl_form_a[efl_rowa_f,efl_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
efl_csform_h <- tapply(EFL$CS, EFL[c("HomeTeam", "Date")],median)
efl_csform_a <- tapply(EFL$CS, EFL[c("AwayTeam", "Date")],median)
efl_csform_h[is.na(efl_csform_h)] <- ""
efl_csform_a[is.na(efl_csform_a)] <- ""
#EFL
for(efl_rowh_f_cs in 1:nrow(efl_csform_h)) {
  for(efl_colh_f_cs in 1:ncol(efl_csform_h)) {

    # print(my_matrix[row, col])
    for(efl_rowa_f_cs in 1:nrow(efl_csform_a)) {
      for(efl_cola_f_cs in 1:ncol(efl_csform_a)) {
        ifelse(!efl_csform_a[efl_rowa_f_cs,efl_cola_f_cs]=="",efl_csform_h[efl_rowa_f_cs,efl_cola_f_cs] <- efl_csform_a[efl_rowa_f_cs,efl_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
efl_totalgoals_h <- tapply(EFL$TG, EFL[c("HomeTeam", "Date")],mean)
efl_totalgoals_a <- tapply(EFL$TG, EFL[c("AwayTeam", "Date")],mean)
efl_totalgoals_h[is.na(efl_totalgoals_h)] <- ""
efl_totalgoals_a[is.na(efl_totalgoals_a)] <- ""
for(efl_rowh in 1:nrow(efl_totalgoals_h)) {
  for(efl_colh in 1:ncol(efl_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(efl_rowa in 1:nrow(efl_totalgoals_a)) {
      for(efl_cola in 1:ncol(efl_totalgoals_a)) {
        ifelse(!efl_totalgoals_a[efl_rowa,efl_cola]=="",efl_totalgoals_h[efl_rowa,efl_cola] <- efl_totalgoals_a[efl_rowa,efl_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#EFL
efl_un05_home <- c()
efl_un05_away <- c()
efl_ov05_home <- c()
efl_ov05_away <- c()

efl_un15_home <- c()
efl_un15_away <- c()
efl_ov15_home <- c()
efl_ov15_away <- c()

efl_un25_home <- c()
efl_un25_away <- c()
efl_ov25_home <- c()
efl_ov25_away <- c()

efl_un35_home <- c()
efl_un35_away <- c()
efl_ov35_home <- c()
efl_ov35_away <- c()

efl_un45_home <- c()
efl_un45_away <- c()
efl_ov45_home <- c()
efl_ov45_away <- c()

efl_un55_home <- c()
efl_un55_away <- c()
efl_ov55_home <- c()
efl_ov55_away <- c()

for (i_efl_tg in 1:length(efl_teams))
{

  efl_un05_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG == 0,])
  efl_un05_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG == 0,])

  efl_ov05_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG > 0,])
  efl_ov05_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG > 0,])

  efl_un15_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG <= 1,])
  efl_un15_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG <= 1,])

  efl_ov15_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG >= 2,])
  efl_ov15_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG >= 2,])

  efl_un25_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG <= 2,])
  efl_un25_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG <= 2,])

  efl_ov25_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG >=3,])
  efl_ov25_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG >=3,])

  efl_un35_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG <= 3,])
  efl_un35_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG <= 3,])

  efl_ov35_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG >= 4,])
  efl_ov35_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG >= 4,])

  efl_un45_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG <= 4,])
  efl_un45_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG <= 4,])

  efl_ov45_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG >= 5,])
  efl_ov45_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG >= 5,])

  efl_un55_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG <= 5,])
  efl_un55_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG <= 5,])

  efl_ov55_home[i_efl_tg] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_tg] & EFL$TG >= 6,])
  efl_ov55_away[i_efl_tg] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_tg] & EFL$TG >= 6,])


}

efl_un05 <- efl_un05_home + efl_un05_away
efl_ov05 <- efl_ov05_home + efl_ov05_away

efl_un15 <- efl_un15_home + efl_un15_away
efl_ov15 <- efl_ov15_home + efl_ov15_away

efl_un25 <- efl_un25_home + efl_un25_away
efl_ov25 <- efl_ov25_home + efl_ov25_away

efl_un35 <- efl_un35_home + efl_un35_away
efl_ov35 <- efl_ov35_home + efl_ov35_away

efl_un45 <- efl_un45_home + efl_un45_away
efl_ov45 <- efl_ov45_home + efl_ov45_away

efl_un55 <- efl_un55_home + efl_un55_away
efl_ov55 <- efl_ov55_home + efl_ov55_away

efl_ovundata <- cbind(efl_teams,efl_un05,efl_ov05,efl_un15,efl_ov15,efl_un25,efl_ov25,efl_un35,efl_ov35,efl_un45,efl_ov45,efl_un55,efl_ov55)
#################################################################################################################################################################
#team against
efl_form_team_against_h <- tapply(EFL$AwayTeam, EFL[c("HomeTeam", "Date")],median)
efl_form_team_against_a <- tapply(EFL$HomeTeam, EFL[c("AwayTeam", "Date")],median)
efl_form_team_against_h[is.na(efl_form_team_against_h)] <- ""
efl_form_team_against_a[is.na(efl_form_team_against_a)] <- ""
#EFL
for(efl_rowh_f_against in 1:nrow(efl_form_team_against_h)) {
  for(efl_colh_f_against in 1:ncol(efl_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(efl_rowa_f_against in 1:nrow(efl_form_team_against_a)) {
      for(efl_cola_f_against in 1:ncol(efl_form_team_against_a)) {
        ifelse(!efl_form_team_against_a[efl_rowa_f_against,efl_cola_f_against]=="",efl_form_team_against_h[efl_rowa_f_against,efl_cola_f_against] <- efl_form_team_against_a[efl_rowa_f_against,efl_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#EFL
#home goals scored
efl_home_gs <- aggregate(EFL$FTHG, by = list(EFL$HomeTeam), FUN = sum)
efl_home_gs_avg <- aggregate(EFL$FTHG, by = list(EFL$HomeTeam),mean)
efl_home_scoring <- merge(efl_home_gs,efl_home_gs_avg, by='Group.1',all = T)
names(efl_home_scoring)[names(efl_home_scoring) == "x.x"] <- "TFthg"
names(efl_home_scoring)[names(efl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
efl_away_gs <- aggregate(EFL$FTAG, by = list(EFL$AwayTeam), FUN = sum)
efl_away_gs_avg <- aggregate(EFL$FTAG, by = list(EFL$AwayTeam),mean)
efl_away_scoring <- merge(efl_away_gs,efl_away_gs_avg, by='Group.1',all = T)
names(efl_away_scoring)[names(efl_away_scoring) == "x.x"] <- "TFtag"
names(efl_away_scoring)[names(efl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
efl_scoring <- merge(efl_home_scoring,efl_away_scoring,by='Group.1',all = T)
efl_scoring$TGS <- efl_scoring$TFthg + efl_scoring$TFtag

#Home shots on target
efl_home_hst <- aggregate(EFL$HST, by = list(EFL$HomeTeam), FUN = sum)
efl_away_ast <- aggregate(EFL$AST, by = list(EFL$AwayTeam), FUN = sum)
efl_tst <- merge(efl_home_hst,efl_away_ast, by='Group.1',all = T)
names(efl_tst)[names(efl_tst) == "x.x"] <- "hst"
names(efl_tst)[names(efl_tst) == "x.y"] <- "ast"
efl_tst$TST <- efl_tst$hst + efl_tst$ast
#merge goals scored and shots on target
efl_scoring_conversion <- merge(efl_tst,efl_scoring,by='Group.1',all = T)
#add HSC ASC TSC
efl_scoring_conversion$HSTC <- percent(efl_scoring_conversion$TFthg/efl_scoring_conversion$hst, accuracy = 0.01)
efl_scoring_conversion$ASTC <- percent(efl_scoring_conversion$TFtag/efl_scoring_conversion$ast, accuracy = 0.01)
efl_scoring_conversion$TSTC <- percent(efl_scoring_conversion$TGS/efl_scoring_conversion$TST, accuracy = 0.01)
#merge games played
efl_scoring_conversion <- cbind(efl_scoring_conversion,efl_games_played)
#create the second part
#home goals conceded
efl_home_gc <- aggregate(EFL$FTAG, by = list(EFL$HomeTeam), FUN = sum)
efl_home_gc_avg <- aggregate(EFL$FTAG, by = list(EFL$HomeTeam),mean)
efl_home_conceding <- merge(efl_home_gc,efl_home_gc_avg, by='Group.1',all = T)
names(efl_home_conceding)[names(efl_home_conceding) == "x.x"] <- "TFthc"
names(efl_home_conceding)[names(efl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
efl_away_gc <- aggregate(EFL$FTHG, by = list(EFL$AwayTeam), FUN = sum)
efl_away_gc_avg <- aggregate(EFL$FTHG, by = list(EFL$AwayTeam),mean)
efl_away_conceding <- merge(efl_away_gc,efl_away_gc_avg, by='Group.1',all = T)
names(efl_away_conceding)[names(efl_away_conceding) == "x.x"] <- "TFtac"
names(efl_away_conceding)[names(efl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
efl_conceding <- merge(efl_home_conceding,efl_away_conceding,by='Group.1',all = T)
efl_conceding$TGC <- efl_conceding$TFthc + efl_conceding$TFtac
efl_home_hst
#Home shots conceded
efl_home_hsc <- aggregate(EFL$AST, by = list(EFL$HomeTeam), FUN = sum)
efl_away_asc <- aggregate(EFL$HST, by = list(EFL$AwayTeam), FUN = sum)
efl_tsc <- merge(efl_home_hsc,efl_away_asc, by='Group.1',all = T)
names(efl_tsc)[names(efl_tsc) == "x.x"] <- "hsc"
names(efl_tsc)[names(efl_tsc) == "x.y"] <- "asc"
efl_tsc$TSC <- efl_tsc$hsc + efl_tsc$asc
#merge goals conceded and shots conceded
efl_conceding_conversion <- merge(efl_tsc,efl_conceding,by='Group.1',all = T)

#add HSC ASC TSC
efl_conceding_conversion$HSCC <- percent(efl_conceding_conversion$TFthc/efl_conceding_conversion$hsc, accuracy = 0.01)
efl_conceding_conversion$ASCC <- percent(efl_conceding_conversion$TFtac/efl_conceding_conversion$asc, accuracy = 0.01)
efl_conceding_conversion$TSCC <- percent(efl_conceding_conversion$TGC/efl_conceding_conversion$TSC, accuracy = 0.01)
efl_conceding_conversion$XSTC <- round(efl_scoring$TGS/(efl_tst$TST - efl_scoring$TGS), digits = 2)

#merge the two parts
efl_shots_analysis <- merge(efl_scoring_conversion,efl_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#EFL
#home fouls for
efl_home_fouls <- aggregate(EFL$HF, by = list(EFL$HomeTeam), FUN = sum)
efl_home_fouls_avg <- aggregate(EFL$HF, by = list(EFL$HomeTeam),mean)
efl_home_foulsdata <- merge(efl_home_fouls,efl_home_fouls_avg, by='Group.1',all = T)
names(efl_home_foulsdata)[names(efl_home_foulsdata) == "x.x"] <- "THfouls"
names(efl_home_foulsdata)[names(efl_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
efl_away_fouls <- aggregate(EFL$HF, by = list(EFL$AwayTeam), FUN = sum)
efl_away_fouls_avg <- aggregate(EFL$HF, by = list(EFL$AwayTeam),mean)
efl_away_foulsdata <- merge(efl_away_fouls,efl_away_fouls_avg, by='Group.1',all = T)
names(efl_away_foulsdata)[names(efl_away_foulsdata) == "x.x"] <- "TAfouls"
names(efl_away_foulsdata)[names(efl_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
efl_fouls <- merge(efl_home_foulsdata,efl_away_foulsdata,by='Group.1',all = T)
efl_fouls$TotalFouls <- efl_fouls$THfouls + efl_fouls$TAfouls

#yellow cards
efl_home_hyc <- aggregate(EFL$HY, by = list(EFL$HomeTeam), FUN = sum)
efl_away_ayc <- aggregate(EFL$AY, by = list(EFL$AwayTeam), FUN = sum)
efl_tyc <- merge(efl_home_hyc,efl_away_ayc, by='Group.1',all = T)
names(efl_tyc)[names(efl_tyc) == "x.x"] <- "hyc"
names(efl_tyc)[names(efl_tyc) == "x.y"] <- "ayc"
efl_tyc$TotalYellows <- efl_tyc$hyc + efl_tyc$ayc

#merge fouls for and yellow cards
efl_fouls_conversion <- merge(efl_tyc,efl_fouls,by='Group.1',all = T)
efl_fouls_conversion$YcPerfoul <- round((efl_fouls_conversion$TotalYellows/efl_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
EFL$Div <- "EFL"
##
###################################################################################################################################################
#poisson cards
efl_GP <- nrow(EFL)
#Calculate total home goals for each division
efl_T_HY <- sum(efl_home_hyc$x)
#calculate average home goal
efl_avg_HY <- round(efl_T_HY /efl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
efl_T_AY <- sum(efl_away_ayc$x)
#calculate average away goal
efl_avg_AY <- round(efl_T_AY /efl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
efl_home_yas <- round(((efl_home_hyc$x/efl_home_games))/efl_avg_HY, digits = 4)
#calculate away attack strength
efl_away_yas <- round(((efl_away_ayc$x/efl_away_games))/efl_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
efl_avg_HYC <- round(efl_T_AY /efl_GP, digits = 4)
#avg away concede
efl_avg_AYC <- round(efl_T_HY /efl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
efl_home_ycc <- aggregate(EFL$AY, by = list(EFL$HomeTeam), FUN = sum)
efl_away_ycc <- aggregate(EFL$HY, by = list(EFL$AwayTeam), FUN = sum)
#home defense strength
efl_home_yds <- round(((efl_home_ycc$x/efl_home_games))/efl_avg_HYC, digits = 4)
#away defense strength
efl_away_yds <- round(((efl_away_ycc$x/efl_away_games))/efl_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_home_poisson_yc <- cbind(efl_division,efl_teams,efl_avg_HY,efl_home_yas,efl_home_yds)
#away poisson data
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_away_poisson_yc <- cbind(efl_division,efl_teams,efl_avg_AY,efl_away_yas,efl_away_yds)
###
HomeTeam_efl_yc <- rep(efl_teams, each = length(efl_teams))
AwayTeam_efl_yc <- rep(efl_teams, length(efl_teams))
EFL_fixtures_yc <- cbind(HomeTeam_efl_yc,AwayTeam_efl_yc)
EFL_fixtures_yc <- as.data.frame(EFL_fixtures_yc)
EFL_fixtures_yc <- EFL_fixtures_yc[!EFL_fixtures_yc$HomeTeam_efl_yc == EFL_fixtures_yc$AwayTeam_efl_yc,]
rownames(EFL_fixtures_yc) <- NULL
EFL_fixtures_yc$Div <- "EFL"
EFL_fixtures_yc <- EFL_fixtures_yc[,c(3,1,2)]

EFL_fixtures_yc$avg_HY_efl <- efl_avg_HY

EFL_fixtures_yc$efl_homeyas <- rep(efl_home_yas,each = length(efl_teams)-1)

efl_awayyds_lookup <- cbind(efl_teams,efl_away_yds)

efl_awayyds_lookup <- as.data.frame(efl_awayyds_lookup)

colnames(efl_awayyds_lookup) <- c("AwayTeam_efl_yc","efl_awayyds")


require('RH2')
EFL_fixtures_yc$efl_awayyds <- sqldf("SELECT efl_awayyds_lookup.efl_awayyds FROM efl_awayyds_lookup INNER JOIN EFL_fixtures_yc ON efl_awayyds_lookup.AwayTeam_efl_yc = EFL_fixtures_yc.AwayTeam_efl_yc")

EFL_fixtures_yc$avg_AY_efl <- efl_avg_AY

efl_awayyas_lookup <- cbind(efl_teams,efl_away_yas)

efl_awayyas_lookup <- as.data.frame(efl_awayyas_lookup)

colnames(efl_awayyas_lookup) <- c("AwayTeam_efl_yc","efl_awayyas")

EFL_fixtures_yc$efl_awayyas <- sqldf("SELECT efl_awayyas_lookup.efl_awayyas FROM efl_awayyas_lookup INNER JOIN EFL_fixtures_yc ON efl_awayyas_lookup.AwayTeam_efl_yc = EFL_fixtures_yc.AwayTeam_efl_yc")

EFL_fixtures_yc$efl_homeyds <- rep(efl_home_yds,each = length(efl_teams)-1)

EFL_fixtures_yc$efl_awayyds <- as.numeric(unlist(EFL_fixtures_yc$efl_awayyds))
#xGH
EFL_fixtures_yc$efl_xHYC <- EFL_fixtures_yc$avg_HY_efl * EFL_fixtures_yc$efl_homeyas * EFL_fixtures_yc$efl_awayyds
#xGA

EFL_fixtures_yc$efl_awayyas <- as.numeric(unlist(EFL_fixtures_yc$efl_awayyas))

EFL_fixtures_yc$efl_xAYC <- EFL_fixtures_yc$avg_AY_efl * EFL_fixtures_yc$efl_awayyas * EFL_fixtures_yc$efl_homeyds

EFL_fixtures_yc$efl_0_0 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_0 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_0_1 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_1 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_0 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_0_2 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_2 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_1 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_2 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_3 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_0 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_1 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_2 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_0_3 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_3 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_3 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_4 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_0 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_1 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_2 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_3 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_0_4 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_4 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_4 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_4 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_5 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_0 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_1 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_2 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_3 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_4 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_0_5 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_5 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_5 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_5 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_5 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_6 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_0 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(0,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_1 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(1,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_2 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(2,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_3 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(3,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_4 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(4,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_6_5 <- round(stats::dpois(6,EFL_fixtures_yc$efl_xHYC) * stats::dpois(5,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_0_6 <- round(stats::dpois(0,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_1_6 <- round(stats::dpois(1,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_2_6 <- round(stats::dpois(2,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_3_6 <- round(stats::dpois(3,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_4_6 <- round(stats::dpois(4,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
EFL_fixtures_yc$efl_5_6 <- round(stats::dpois(5,EFL_fixtures_yc$efl_xHYC) * stats::dpois(6,EFL_fixtures_yc$efl_xAYC), digits = 4)
#Home win
EFL_fixtures_yc$efl_H <- (
  EFL_fixtures_yc$efl_1_0 + EFL_fixtures_yc$efl_2_0 + EFL_fixtures_yc$efl_2_1 + EFL_fixtures_yc$efl_3_0 + EFL_fixtures_yc$efl_3_1 +
    EFL_fixtures_yc$efl_3_2 + EFL_fixtures_yc$efl_4_0 + EFL_fixtures_yc$efl_4_1 + EFL_fixtures_yc$efl_4_2 + EFL_fixtures_yc$efl_4_3 +
    EFL_fixtures_yc$efl_5_0 + EFL_fixtures_yc$efl_5_1 + EFL_fixtures_yc$efl_5_2 + EFL_fixtures_yc$efl_5_3 + EFL_fixtures_yc$efl_5_4 +
    EFL_fixtures_yc$efl_6_0 + EFL_fixtures_yc$efl_6_1 + EFL_fixtures_yc$efl_6_2 + EFL_fixtures_yc$efl_6_3 + EFL_fixtures_yc$efl_6_4 +
    EFL_fixtures_yc$efl_6_5
)

EFL_fixtures_yc$efl_H <- percent(EFL_fixtures_yc$efl_H, accuracy = 0.1)

#Draw
EFL_fixtures_yc$efl_D <- (

  EFL_fixtures_yc$efl_0_0 + EFL_fixtures_yc$efl_1_1 + EFL_fixtures_yc$efl_2_2 + EFL_fixtures_yc$efl_3_3 + EFL_fixtures_yc$efl_4_4 +
    EFL_fixtures_yc$efl_5_5 + EFL_fixtures_yc$efl_6_6
)

EFL_fixtures_yc$efl_D <- percent(EFL_fixtures_yc$efl_D, accuracy = 0.1)

#Away

EFL_fixtures_yc$efl_A <- (
  EFL_fixtures_yc$efl_0_1 + EFL_fixtures_yc$efl_0_2 + EFL_fixtures_yc$efl_1_2 + EFL_fixtures_yc$efl_0_3 + EFL_fixtures_yc$efl_1_3 +
    EFL_fixtures_yc$efl_2_3 + EFL_fixtures_yc$efl_0_4 + EFL_fixtures_yc$efl_1_4 + EFL_fixtures_yc$efl_2_4 + EFL_fixtures_yc$efl_3_4 +
    EFL_fixtures_yc$efl_0_5 + EFL_fixtures_yc$efl_1_5 + EFL_fixtures_yc$efl_2_5 + EFL_fixtures_yc$efl_3_5 + EFL_fixtures_yc$efl_4_5 +
    EFL_fixtures_yc$efl_0_6 + EFL_fixtures_yc$efl_1_6 + EFL_fixtures_yc$efl_2_6 + EFL_fixtures_yc$efl_3_6 + EFL_fixtures_yc$efl_4_6 +
    EFL_fixtures_yc$efl_5_6
)

EFL_fixtures_yc$efl_A <- percent(EFL_fixtures_yc$efl_A, accuracy = 0.1)

#ov25
EFL_fixtures_yc$efl_ov25 <- (
  EFL_fixtures_yc$efl_2_1 + EFL_fixtures_yc$efl_1_2 + EFL_fixtures_yc$efl_2_2 + EFL_fixtures_yc$efl_3_0 + EFL_fixtures_yc$efl_3_1 +
    EFL_fixtures_yc$efl_3_2 + EFL_fixtures_yc$efl_0_3 + EFL_fixtures_yc$efl_1_3 + EFL_fixtures_yc$efl_2_3 + EFL_fixtures_yc$efl_3_3 +
    EFL_fixtures_yc$efl_4_0 + EFL_fixtures_yc$efl_4_1 + EFL_fixtures_yc$efl_4_2 + EFL_fixtures_yc$efl_4_3 + EFL_fixtures_yc$efl_0_4 +
    EFL_fixtures_yc$efl_1_4 + EFL_fixtures_yc$efl_2_4 + EFL_fixtures_yc$efl_3_4 + EFL_fixtures_yc$efl_4_4 + EFL_fixtures_yc$efl_5_0 +
    EFL_fixtures_yc$efl_5_1 + EFL_fixtures_yc$efl_5_2 + EFL_fixtures_yc$efl_5_3 + EFL_fixtures_yc$efl_5_4 + EFL_fixtures_yc$efl_0_5 +
    EFL_fixtures_yc$efl_1_5 + EFL_fixtures_yc$efl_2_5 + EFL_fixtures_yc$efl_3_5 + EFL_fixtures_yc$efl_4_5 + EFL_fixtures_yc$efl_5_5 +
    EFL_fixtures_yc$efl_6_0 + EFL_fixtures_yc$efl_6_1 + EFL_fixtures_yc$efl_6_2 + EFL_fixtures_yc$efl_6_3 + EFL_fixtures_yc$efl_6_4 +
    EFL_fixtures_yc$efl_6_5 + EFL_fixtures_yc$efl_0_6 + EFL_fixtures_yc$efl_1_6 + EFL_fixtures_yc$efl_2_6 + EFL_fixtures_yc$efl_3_6 +
    EFL_fixtures_yc$efl_4_6 + EFL_fixtures_yc$efl_5_6 + EFL_fixtures_yc$efl_6_6
)
#un25
EFL_fixtures_yc$efl_un25 <- (
  EFL_fixtures_yc$efl_0_0 + EFL_fixtures_yc$efl_1_0 + EFL_fixtures_yc$efl_0_1 + EFL_fixtures_yc$efl_1_1 + EFL_fixtures_yc$efl_2_0 + EFL_fixtures_yc$efl_0_2
)
#odds
EFL_fixtures_yc$efl_ov25_odds <- round((1/EFL_fixtures_yc$efl_ov25),digits = 2)
EFL_fixtures_yc$efl_un25_odds <- round((1/EFL_fixtures_yc$efl_un25),digits = 2)

EFL_fixtures_yc$efl_ov25_odds
EFL_fixtures_yc$efl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EFL_fixtures_yc$efl_ov25 <- percent(EFL_fixtures_yc$efl_ov25, accuracy = 0.1)

EFL_fixtures_yc$efl_un25 <- percent(EFL_fixtures_yc$efl_un25, accuracy = 0.1)
EFL_fixtures_yc$efl_pscore <- paste(round(EFL_fixtures_yc$efl_xHYC,digits = 0),round(EFL_fixtures_yc$efl_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
efl_GP <- nrow(EFL)
#Calculate total home corners for each division
efl_home_corners <- aggregate(EFL$HCO, by = list(EFL$HomeTeam), FUN = sum)
efl_away_corners <- aggregate(EFL$ACO, by = list(EFL$AwayTeam), FUN = sum)
###############################################################################
efl_T_HCO <- sum(efl_home_corners$x)
#calculate average home corners
efl_avg_HCO <- round(efl_T_HCO /efl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
efl_T_ACO <- sum(efl_away_corners$x)
#calculate average away goal
efl_avg_ACO <- round(efl_T_ACO /efl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
efl_home_coas <- round(((efl_home_corners$x/efl_home_games))/efl_avg_HCO, digits = 4)
#calculate away attack strength
efl_away_coas <- round(((efl_away_corners$x/efl_away_games))/efl_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
efl_avg_HCOC <- round(efl_T_ACO /efl_GP, digits = 4)
#avg away concede
efl_avg_ACOC <- round(efl_T_HCO /efl_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
efl_home_coc <- aggregate(EFL$ACO, by = list(EFL$HomeTeam), FUN = sum)
efl_away_coc <- aggregate(EFL$HCO, by = list(EFL$AwayTeam), FUN = sum)
#home defense strength
efl_home_cods <- round(((efl_home_coc$x/efl_home_games))/efl_avg_HCOC, digits = 4)
#away defense strength
efl_away_cods <- round(((efl_away_coc$x/efl_away_games))/efl_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_home_poisson_corners <- cbind(efl_division,efl_teams,efl_avg_HCO,efl_home_coas,efl_home_cods)
#################################################################################
#away poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_away_poisson_corners <- cbind(efl_division,efl_teams,efl_avg_ACO,efl_away_coas,efl_away_cods)

#EFL
HomeTeam_efl_co <- rep(efl_teams, each = length(efl_teams))
AwayTeam_efl_co <- rep(efl_teams, length(efl_teams))
EFL_fixtures_co <- cbind(HomeTeam_efl_co,AwayTeam_efl_co)
EFL_fixtures_co <- as.data.frame(EFL_fixtures_co)
EFL_fixtures_co <- EFL_fixtures_co[!EFL_fixtures_co$HomeTeam_efl_co == EFL_fixtures_co$AwayTeam_efl_co,]
rownames(EFL_fixtures_co) <- NULL
EFL_fixtures_co$Div <- "EFL"
EFL_fixtures_co <- EFL_fixtures_co[,c(3,1,2)]

EFL_fixtures_co$avg_HCO_efl <- efl_avg_HCO

EFL_fixtures_co$efl_homecoas <- rep(efl_home_coas,each = length(efl_teams)-1)

efl_awaycods_lookup <- cbind(efl_teams,efl_away_cods)

efl_awaycods_lookup <- as.data.frame(efl_awaycods_lookup)

colnames(efl_awaycods_lookup) <- c("AwayTeam_efl_co","efl_awaycods")


require('RH2')
EFL_fixtures_co$efl_awaycods <- sqldf("SELECT efl_awaycods_lookup.efl_awaycods FROM efl_awaycods_lookup INNER JOIN EFL_fixtures_co ON efl_awaycods_lookup.AwayTeam_efl_co = EFL_fixtures_co.AwayTeam_efl_co")

EFL_fixtures_co$avg_ACO_efl <- efl_avg_ACO

efl_awaycoas_lookup <- cbind(efl_teams,efl_away_coas)

efl_awaycoas_lookup <- as.data.frame(efl_awaycoas_lookup)

colnames(efl_awaycoas_lookup) <- c("AwayTeam_efl_co","efl_awaycoas")

EFL_fixtures_co$efl_awaycoas <- sqldf("SELECT efl_awaycoas_lookup.efl_awaycoas FROM efl_awaycoas_lookup INNER JOIN EFL_fixtures_co ON efl_awaycoas_lookup.AwayTeam_efl_co = EFL_fixtures_co.AwayTeam_efl_co")

EFL_fixtures_co$efl_homecods <- rep(efl_home_cods,each = length(efl_teams)-1)

EFL_fixtures_co$efl_awaycods <- as.numeric(unlist(EFL_fixtures_co$efl_awaycods))
#xGH
EFL_fixtures_co$efl_xHCOC <- EFL_fixtures_co$avg_HCO_efl * EFL_fixtures_co$efl_homecoas * EFL_fixtures_co$efl_awaycods
#xGA

EFL_fixtures_co$efl_awaycoas <- as.numeric(unlist(EFL_fixtures_co$efl_awaycoas))

EFL_fixtures_co$efl_xACOC <- EFL_fixtures_co$avg_ACO_efl * EFL_fixtures_co$efl_awaycoas * EFL_fixtures_co$efl_homecods

EFL_fixtures_co$efl_0_0 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_0 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_0_1 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_1 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_0 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_0_2 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_2 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_1 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_2 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_3 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_0 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_1 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_2 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_0_3 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_3 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_3 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_4 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_0 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_1 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_2 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_3 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_0_4 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_4 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_4 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_4 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_5 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_0 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_1 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_2 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_3 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_4 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_0_5 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_5 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_5 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_5 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_5 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_6 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_0 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(0,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_1 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(1,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_2 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(2,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_3 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(3,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_4 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(4,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_6_5 <- round(stats::dpois(6,EFL_fixtures_co$efl_xHCOC) * stats::dpois(5,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_0_6 <- round(stats::dpois(0,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_1_6 <- round(stats::dpois(1,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_2_6 <- round(stats::dpois(2,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_3_6 <- round(stats::dpois(3,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_4_6 <- round(stats::dpois(4,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
EFL_fixtures_co$efl_5_6 <- round(stats::dpois(5,EFL_fixtures_co$efl_xHCOC) * stats::dpois(6,EFL_fixtures_co$efl_xACOC), digits = 4)
#Home win
EFL_fixtures_co$efl_H <- (
  EFL_fixtures_co$efl_1_0 + EFL_fixtures_co$efl_2_0 + EFL_fixtures_co$efl_2_1 + EFL_fixtures_co$efl_3_0 + EFL_fixtures_co$efl_3_1 +
    EFL_fixtures_co$efl_3_2 + EFL_fixtures_co$efl_4_0 + EFL_fixtures_co$efl_4_1 + EFL_fixtures_co$efl_4_2 + EFL_fixtures_co$efl_4_3 +
    EFL_fixtures_co$efl_5_0 + EFL_fixtures_co$efl_5_1 + EFL_fixtures_co$efl_5_2 + EFL_fixtures_co$efl_5_3 + EFL_fixtures_co$efl_5_4 +
    EFL_fixtures_co$efl_6_0 + EFL_fixtures_co$efl_6_1 + EFL_fixtures_co$efl_6_2 + EFL_fixtures_co$efl_6_3 + EFL_fixtures_co$efl_6_4 +
    EFL_fixtures_co$efl_6_5
)

EFL_fixtures_co$efl_H <- percent(EFL_fixtures_co$efl_H, accuracy = 0.1)

#Draw
EFL_fixtures_co$efl_D <- (

  EFL_fixtures_co$efl_0_0 + EFL_fixtures_co$efl_1_1 + EFL_fixtures_co$efl_2_2 + EFL_fixtures_co$efl_3_3 + EFL_fixtures_co$efl_4_4 +
    EFL_fixtures_co$efl_5_5 + EFL_fixtures_co$efl_6_6
)

EFL_fixtures_co$efl_D <- percent(EFL_fixtures_co$efl_D, accuracy = 0.1)

#Away

EFL_fixtures_co$efl_A <- (
  EFL_fixtures_co$efl_0_1 + EFL_fixtures_co$efl_0_2 + EFL_fixtures_co$efl_1_2 + EFL_fixtures_co$efl_0_3 + EFL_fixtures_co$efl_1_3 +
    EFL_fixtures_co$efl_2_3 + EFL_fixtures_co$efl_0_4 + EFL_fixtures_co$efl_1_4 + EFL_fixtures_co$efl_2_4 + EFL_fixtures_co$efl_3_4 +
    EFL_fixtures_co$efl_0_5 + EFL_fixtures_co$efl_1_5 + EFL_fixtures_co$efl_2_5 + EFL_fixtures_co$efl_3_5 + EFL_fixtures_co$efl_4_5 +
    EFL_fixtures_co$efl_0_6 + EFL_fixtures_co$efl_1_6 + EFL_fixtures_co$efl_2_6 + EFL_fixtures_co$efl_3_6 + EFL_fixtures_co$efl_4_6 +
    EFL_fixtures_co$efl_5_6
)

EFL_fixtures_co$efl_A <- percent(EFL_fixtures_co$efl_A, accuracy = 0.1)

#ov25
EFL_fixtures_co$efl_ov25 <- (
  EFL_fixtures_co$efl_2_1 + EFL_fixtures_co$efl_1_2 + EFL_fixtures_co$efl_2_2 + EFL_fixtures_co$efl_3_0 + EFL_fixtures_co$efl_3_1 +
    EFL_fixtures_co$efl_3_2 + EFL_fixtures_co$efl_0_3 + EFL_fixtures_co$efl_1_3 + EFL_fixtures_co$efl_2_3 + EFL_fixtures_co$efl_3_3 +
    EFL_fixtures_co$efl_4_0 + EFL_fixtures_co$efl_4_1 + EFL_fixtures_co$efl_4_2 + EFL_fixtures_co$efl_4_3 + EFL_fixtures_co$efl_0_4 +
    EFL_fixtures_co$efl_1_4 + EFL_fixtures_co$efl_2_4 + EFL_fixtures_co$efl_3_4 + EFL_fixtures_co$efl_4_4 + EFL_fixtures_co$efl_5_0 +
    EFL_fixtures_co$efl_5_1 + EFL_fixtures_co$efl_5_2 + EFL_fixtures_co$efl_5_3 + EFL_fixtures_co$efl_5_4 + EFL_fixtures_co$efl_0_5 +
    EFL_fixtures_co$efl_1_5 + EFL_fixtures_co$efl_2_5 + EFL_fixtures_co$efl_3_5 + EFL_fixtures_co$efl_4_5 + EFL_fixtures_co$efl_5_5 +
    EFL_fixtures_co$efl_6_0 + EFL_fixtures_co$efl_6_1 + EFL_fixtures_co$efl_6_2 + EFL_fixtures_co$efl_6_3 + EFL_fixtures_co$efl_6_4 +
    EFL_fixtures_co$efl_6_5 + EFL_fixtures_co$efl_0_6 + EFL_fixtures_co$efl_1_6 + EFL_fixtures_co$efl_2_6 + EFL_fixtures_co$efl_3_6 +
    EFL_fixtures_co$efl_4_6 + EFL_fixtures_co$efl_5_6 + EFL_fixtures_co$efl_6_6
)
#un25
EFL_fixtures_co$efl_un25 <- (
  EFL_fixtures_co$efl_0_0 + EFL_fixtures_co$efl_1_0 + EFL_fixtures_co$efl_0_1 + EFL_fixtures_co$efl_1_1 + EFL_fixtures_co$efl_2_0 + EFL_fixtures_co$efl_0_2
)
#odds
EFL_fixtures_co$efl_ov25_odds <- round((1/EFL_fixtures_co$efl_ov25),digits = 2)
EFL_fixtures_co$efl_un25_odds <- round((1/EFL_fixtures_co$efl_un25),digits = 2)

EFL_fixtures_co$efl_ov25_odds
EFL_fixtures_co$efl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EFL_fixtures_co$efl_ov25 <- percent(EFL_fixtures_co$efl_ov25, accuracy = 0.1)

EFL_fixtures_co$efl_un25 <- percent(EFL_fixtures_co$efl_un25, accuracy = 0.1)
EFL_fixtures_co$efl_pscore <- paste(round(EFL_fixtures_co$efl_xHCOC,digits = 0),round(EFL_fixtures_co$efl_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
efl_GP <- nrow(EFL)
#Calculate total home goals for each division
efl_T_HF <- sum(efl_home_fouls$x)
#calculate average home goal
efl_avg_HF <- round(efl_T_HF /efl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
efl_T_AF <- sum(efl_away_fouls$x)
#calculate average away goal
efl_avg_AF <- round(efl_T_AF /efl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
efl_home_fas <- round(((efl_home_fouls$x/efl_home_games))/efl_avg_HF, digits = 4)
#calculate away attack strength
efl_away_fas <- round(((efl_away_fouls$x/efl_away_games))/efl_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
efl_avg_HFC <- round(efl_T_AF /efl_GP, digits = 4)
#avg away concede
efl_avg_AFC <- round(efl_T_HF /efl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
efl_home_fcc <- aggregate(EFL$AF, by = list(EFL$HomeTeam), FUN = sum)
efl_away_fcc <- aggregate(EFL$HF, by = list(EFL$AwayTeam), FUN = sum)

#home defense strength
efl_home_fds <- round(((efl_home_fcc$x/efl_home_games))/efl_avg_HFC, digits = 4)

#away defense strength
efl_away_fds <- round(((efl_away_fcc$x/efl_away_games))/efl_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_home_poisson_fo <- cbind(efl_division,efl_teams,efl_avg_HF,efl_home_fas,efl_home_fds)

#################################################################################
#away poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_away_poisson_fo <- cbind(efl_division,efl_teams,efl_avg_AF,efl_away_fas,efl_away_fds)

#EFL
HomeTeam_efl_fo <- rep(efl_teams, each = length(efl_teams))
AwayTeam_efl_fo <- rep(efl_teams, length(efl_teams))
EFL_fixtures_fo <- cbind(HomeTeam_efl_fo,AwayTeam_efl_fo)
EFL_fixtures_fo <- as.data.frame(EFL_fixtures_fo)
EFL_fixtures_fo <- EFL_fixtures_fo[!EFL_fixtures_fo$HomeTeam_efl_fo == EFL_fixtures_fo$AwayTeam_efl_fo,]
rownames(EFL_fixtures_fo) <- NULL
EFL_fixtures_fo$Div <- "EFL"
EFL_fixtures_fo <- EFL_fixtures_fo[,c(3,1,2)]

EFL_fixtures_fo$avg_HF_efl <- efl_avg_HF

EFL_fixtures_fo$efl_homefas <- rep(efl_home_fas,each = length(efl_teams)-1)

efl_awayfds_lookup <- cbind(efl_teams,efl_away_fds)

efl_awayfds_lookup <- as.data.frame(efl_awayfds_lookup)

colnames(efl_awayfds_lookup) <- c("AwayTeam_efl_fo","efl_awayfds")


require('RH2')
EFL_fixtures_fo$efl_awayfds <- sqldf("SELECT efl_awayfds_lookup.efl_awayfds FROM efl_awayfds_lookup INNER JOIN EFL_fixtures_fo ON efl_awayfds_lookup.AwayTeam_efl_fo = EFL_fixtures_fo.AwayTeam_efl_fo")

EFL_fixtures_fo$avg_AF_efl <- efl_avg_AF

efl_awayfas_lookup <- cbind(efl_teams,efl_away_fas)

efl_awayfas_lookup <- as.data.frame(efl_awayfas_lookup)

colnames(efl_awayfas_lookup) <- c("AwayTeam_efl_fo","efl_awayfas")

EFL_fixtures_fo$efl_awayfas <- sqldf("SELECT efl_awayfas_lookup.efl_awayfas FROM efl_awayfas_lookup INNER JOIN EFL_fixtures_fo ON efl_awayfas_lookup.AwayTeam_efl_fo = EFL_fixtures_fo.AwayTeam_efl_fo")

EFL_fixtures_fo$efl_homefds <- rep(efl_home_fds,each = length(efl_teams)-1)

EFL_fixtures_fo$efl_awayfds <- as.numeric(unlist(EFL_fixtures_fo$efl_awayfds))
#xGH
EFL_fixtures_fo$efl_xHF <- EFL_fixtures_fo$avg_HF_efl * EFL_fixtures_fo$efl_homefas * EFL_fixtures_fo$efl_awayfds
#xGA

EFL_fixtures_fo$efl_awayfas <- as.numeric(unlist(EFL_fixtures_fo$efl_awayfas))

EFL_fixtures_fo$efl_xAF <- EFL_fixtures_fo$avg_AF_efl * EFL_fixtures_fo$efl_awayfas * EFL_fixtures_fo$efl_homefds

EFL_fixtures_fo$efl_0_0 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_0 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_0_1 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_1 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_0 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_0_2 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_2 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_1 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_2 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_3 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_0 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_1 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_2 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_0_3 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_3 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_3 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_4 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_0 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_1 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_2 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_3 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_0_4 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_4 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_4 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_4 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_5 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_0 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_1 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_2 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_3 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_4 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_0_5 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_5 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_5 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_5 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_5 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_6 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_0 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(0,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_1 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(1,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_2 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(2,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_3 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(3,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_4 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(4,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_6_5 <- round(stats::dpois(6,EFL_fixtures_fo$efl_xHF) * stats::dpois(5,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_0_6 <- round(stats::dpois(0,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_1_6 <- round(stats::dpois(1,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_2_6 <- round(stats::dpois(2,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_3_6 <- round(stats::dpois(3,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_4_6 <- round(stats::dpois(4,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
EFL_fixtures_fo$efl_5_6 <- round(stats::dpois(5,EFL_fixtures_fo$efl_xHF) * stats::dpois(6,EFL_fixtures_fo$efl_xAF), digits = 4)
#Home win
EFL_fixtures_fo$efl_H <- (
  EFL_fixtures_fo$efl_1_0 + EFL_fixtures_fo$efl_2_0 + EFL_fixtures_fo$efl_2_1 + EFL_fixtures_fo$efl_3_0 + EFL_fixtures_fo$efl_3_1 +
    EFL_fixtures_fo$efl_3_2 + EFL_fixtures_fo$efl_4_0 + EFL_fixtures_fo$efl_4_1 + EFL_fixtures_fo$efl_4_2 + EFL_fixtures_fo$efl_4_3 +
    EFL_fixtures_fo$efl_5_0 + EFL_fixtures_fo$efl_5_1 + EFL_fixtures_fo$efl_5_2 + EFL_fixtures_fo$efl_5_3 + EFL_fixtures_fo$efl_5_4 +
    EFL_fixtures_fo$efl_6_0 + EFL_fixtures_fo$efl_6_1 + EFL_fixtures_fo$efl_6_2 + EFL_fixtures_fo$efl_6_3 + EFL_fixtures_fo$efl_6_4 +
    EFL_fixtures_fo$efl_6_5
)

EFL_fixtures_fo$efl_H <- percent(EFL_fixtures_fo$efl_H, accuracy = 0.1)

#Draw
EFL_fixtures_fo$efl_D <- (

  EFL_fixtures_fo$efl_0_0 + EFL_fixtures_fo$efl_1_1 + EFL_fixtures_fo$efl_2_2 + EFL_fixtures_fo$efl_3_3 + EFL_fixtures_fo$efl_4_4 +
    EFL_fixtures_fo$efl_5_5 + EFL_fixtures_fo$efl_6_6
)

EFL_fixtures_fo$efl_D <- percent(EFL_fixtures_fo$efl_D, accuracy = 0.1)

#Away

EFL_fixtures_fo$efl_A <- (
  EFL_fixtures_fo$efl_0_1 + EFL_fixtures_fo$efl_0_2 + EFL_fixtures_fo$efl_1_2 + EFL_fixtures_fo$efl_0_3 + EFL_fixtures_fo$efl_1_3 +
    EFL_fixtures_fo$efl_2_3 + EFL_fixtures_fo$efl_0_4 + EFL_fixtures_fo$efl_1_4 + EFL_fixtures_fo$efl_2_4 + EFL_fixtures_fo$efl_3_4 +
    EFL_fixtures_fo$efl_0_5 + EFL_fixtures_fo$efl_1_5 + EFL_fixtures_fo$efl_2_5 + EFL_fixtures_fo$efl_3_5 + EFL_fixtures_fo$efl_4_5 +
    EFL_fixtures_fo$efl_0_6 + EFL_fixtures_fo$efl_1_6 + EFL_fixtures_fo$efl_2_6 + EFL_fixtures_fo$efl_3_6 + EFL_fixtures_fo$efl_4_6 +
    EFL_fixtures_fo$efl_5_6
)

EFL_fixtures_fo$efl_A <- percent(EFL_fixtures_fo$efl_A, accuracy = 0.1)

#ov25
EFL_fixtures_fo$efl_ov25 <- (
  EFL_fixtures_fo$efl_2_1 + EFL_fixtures_fo$efl_1_2 + EFL_fixtures_fo$efl_2_2 + EFL_fixtures_fo$efl_3_0 + EFL_fixtures_fo$efl_3_1 +
    EFL_fixtures_fo$efl_3_2 + EFL_fixtures_fo$efl_0_3 + EFL_fixtures_fo$efl_1_3 + EFL_fixtures_fo$efl_2_3 + EFL_fixtures_fo$efl_3_3 +
    EFL_fixtures_fo$efl_4_0 + EFL_fixtures_fo$efl_4_1 + EFL_fixtures_fo$efl_4_2 + EFL_fixtures_fo$efl_4_3 + EFL_fixtures_fo$efl_0_4 +
    EFL_fixtures_fo$efl_1_4 + EFL_fixtures_fo$efl_2_4 + EFL_fixtures_fo$efl_3_4 + EFL_fixtures_fo$efl_4_4 + EFL_fixtures_fo$efl_5_0 +
    EFL_fixtures_fo$efl_5_1 + EFL_fixtures_fo$efl_5_2 + EFL_fixtures_fo$efl_5_3 + EFL_fixtures_fo$efl_5_4 + EFL_fixtures_fo$efl_0_5 +
    EFL_fixtures_fo$efl_1_5 + EFL_fixtures_fo$efl_2_5 + EFL_fixtures_fo$efl_3_5 + EFL_fixtures_fo$efl_4_5 + EFL_fixtures_fo$efl_5_5 +
    EFL_fixtures_fo$efl_6_0 + EFL_fixtures_fo$efl_6_1 + EFL_fixtures_fo$efl_6_2 + EFL_fixtures_fo$efl_6_3 + EFL_fixtures_fo$efl_6_4 +
    EFL_fixtures_fo$efl_6_5 + EFL_fixtures_fo$efl_0_6 + EFL_fixtures_fo$efl_1_6 + EFL_fixtures_fo$efl_2_6 + EFL_fixtures_fo$efl_3_6 +
    EFL_fixtures_fo$efl_4_6 + EFL_fixtures_fo$efl_5_6 + EFL_fixtures_fo$efl_6_6
)
#un25
EFL_fixtures_fo$efl_un25 <- (
  EFL_fixtures_fo$efl_0_0 + EFL_fixtures_fo$efl_1_0 + EFL_fixtures_fo$efl_0_1 + EFL_fixtures_fo$efl_1_1 + EFL_fixtures_fo$efl_2_0 + EFL_fixtures_fo$efl_0_2
)
#odds
EFL_fixtures_fo$efl_ov25_odds <- round((1/EFL_fixtures_fo$efl_ov25),digits = 2)
EFL_fixtures_fo$efl_un25_odds <- round((1/EFL_fixtures_fo$efl_un25),digits = 2)

EFL_fixtures_fo$efl_ov25_odds
EFL_fixtures_fo$efl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EFL_fixtures_fo$efl_ov25 <- percent(EFL_fixtures_fo$efl_ov25, accuracy = 0.1)

EFL_fixtures_fo$efl_un25 <- percent(EFL_fixtures_fo$efl_un25, accuracy = 0.1)
EFL_fixtures_fo$efl_psfore <- paste(round(EFL_fixtures_fo$efl_xHF,digits = 0),round(EFL_fixtures_fo$efl_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
efl_GP <- nrow(EFL)

#Calculate total home goals for each division
efl_T_HST <- sum(efl_home_hst$x)
#calculate average home goal

efl_avg_HST <- round(efl_T_HST /efl_GP, digits = 4)

############################################################
#Calculate total away goals for each division
efl_T_AST <- sum(efl_away_ast$x)
#calculate average away goal
efl_avg_AST <- round(efl_T_AST /efl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
efl_home_sotas <- round(((efl_home_hst$x/efl_home_games))/efl_avg_HST, digits = 4)
#calculate away attack strength
efl_away_sotas <- round(((efl_away_ast$x/efl_away_games))/efl_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
efl_avg_HSC <- round(efl_T_AST /efl_GP, digits = 4)

#avg away concede
efl_avg_ASC <- round(efl_T_HST /efl_GP, digits = 4)
#home defense strength
efl_home_sods <- round(((efl_home_hsc$x/efl_home_games))/efl_avg_HSC, digits = 4)

#away defense strength
efl_away_sods <- round(((efl_away_ast$x/efl_away_games))/efl_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_home_poisson_sot <- cbind(efl_division,efl_teams,efl_avg_HST,efl_home_sotas,efl_home_sods)

#################################################################################
#away poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_away_poisson_sot <- cbind(efl_division,efl_teams,efl_avg_AST,efl_away_sotas,efl_away_sods)

#EFL
HomeTeam_efl_sot <- rep(efl_teams, each = length(efl_teams))
AwayTeam_efl_sot <- rep(efl_teams, length(efl_teams))
EFL_fixtures_sot <- cbind(HomeTeam_efl_sot,AwayTeam_efl_sot)
EFL_fixtures_sot <- as.data.frame(EFL_fixtures_sot)
EFL_fixtures_sot <- EFL_fixtures_sot[!EFL_fixtures_sot$HomeTeam_efl_sot == EFL_fixtures_sot$AwayTeam_efl_sot,]
rownames(EFL_fixtures_sot) <- NULL
EFL_fixtures_sot$Div <- "EFL"
EFL_fixtures_sot <- EFL_fixtures_sot[,c(3,1,2)]

EFL_fixtures_sot$avg_HST_efl <- efl_avg_HST

EFL_fixtures_sot$efl_homesotas <- rep(efl_home_sotas,each = length(efl_teams)-1)

efl_awaysods_lookup <- cbind(efl_teams,efl_away_sods)

efl_awaysods_lookup <- as.data.frame(efl_awaysods_lookup)

colnames(efl_awaysods_lookup) <- c("AwayTeam_efl_sot","efl_awaysods")


require('RH2')
EFL_fixtures_sot$efl_awaysods <- sqldf("SELECT efl_awaysods_lookup.efl_awaysods FROM efl_awaysods_lookup INNER JOIN EFL_fixtures_sot ON efl_awaysods_lookup.AwayTeam_efl_sot = EFL_fixtures_sot.AwayTeam_efl_sot")

EFL_fixtures_sot$avg_AST_efl <- efl_avg_AST

efl_awaysotas_lookup <- cbind(efl_teams,efl_away_sotas)

efl_awaysotas_lookup <- as.data.frame(efl_awaysotas_lookup)

colnames(efl_awaysotas_lookup) <- c("AwayTeam_efl_sot","efl_awaysotas")

EFL_fixtures_sot$efl_awaysotas <- sqldf("SELECT efl_awaysotas_lookup.efl_awaysotas FROM efl_awaysotas_lookup INNER JOIN EFL_fixtures_sot ON efl_awaysotas_lookup.AwayTeam_efl_sot = EFL_fixtures_sot.AwayTeam_efl_sot")

EFL_fixtures_sot$efl_homesods <- rep(efl_home_sods,each = length(efl_teams)-1)

EFL_fixtures_sot$efl_awaysods <- as.numeric(unlist(EFL_fixtures_sot$efl_awaysods))
#xGH
EFL_fixtures_sot$efl_xHST <- EFL_fixtures_sot$avg_HST_efl * EFL_fixtures_sot$efl_homesotas * EFL_fixtures_sot$efl_awaysods
#xGA

EFL_fixtures_sot$efl_awaysotas <- as.numeric(unlist(EFL_fixtures_sot$efl_awaysotas))

EFL_fixtures_sot$efl_xAST <- EFL_fixtures_sot$avg_AST_efl * EFL_fixtures_sot$efl_awaysotas * EFL_fixtures_sot$efl_homesods

EFL_fixtures_sot$efl_0_0 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_0 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_0_1 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_1 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_0 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_0_2 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_2 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_1 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_2 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_3 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_0 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_1 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_2 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_0_3 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_3 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_3 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_4 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_0 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_1 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_2 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_3 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_0_4 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_4 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_4 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_4 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_5 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_0 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_1 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_2 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_3 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_4 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_0_5 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_5 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_5 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_5 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_5 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_6 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_0 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(0,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_1 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(1,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_2 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(2,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_3 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(3,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_4 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(4,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_6_5 <- round(stats::dpois(6,EFL_fixtures_sot$efl_xHST) * stats::dpois(5,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_0_6 <- round(stats::dpois(0,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_1_6 <- round(stats::dpois(1,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_2_6 <- round(stats::dpois(2,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_3_6 <- round(stats::dpois(3,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_4_6 <- round(stats::dpois(4,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
EFL_fixtures_sot$efl_5_6 <- round(stats::dpois(5,EFL_fixtures_sot$efl_xHST) * stats::dpois(6,EFL_fixtures_sot$efl_xAST), digits = 4)
#Home win
EFL_fixtures_sot$efl_H <- (
  EFL_fixtures_sot$efl_1_0 + EFL_fixtures_sot$efl_2_0 + EFL_fixtures_sot$efl_2_1 + EFL_fixtures_sot$efl_3_0 + EFL_fixtures_sot$efl_3_1 +
    EFL_fixtures_sot$efl_3_2 + EFL_fixtures_sot$efl_4_0 + EFL_fixtures_sot$efl_4_1 + EFL_fixtures_sot$efl_4_2 + EFL_fixtures_sot$efl_4_3 +
    EFL_fixtures_sot$efl_5_0 + EFL_fixtures_sot$efl_5_1 + EFL_fixtures_sot$efl_5_2 + EFL_fixtures_sot$efl_5_3 + EFL_fixtures_sot$efl_5_4 +
    EFL_fixtures_sot$efl_6_0 + EFL_fixtures_sot$efl_6_1 + EFL_fixtures_sot$efl_6_2 + EFL_fixtures_sot$efl_6_3 + EFL_fixtures_sot$efl_6_4 +
    EFL_fixtures_sot$efl_6_5
)

EFL_fixtures_sot$efl_H <- percent(EFL_fixtures_sot$efl_H, accuracy = 0.1)

#Draw
EFL_fixtures_sot$efl_D <- (

  EFL_fixtures_sot$efl_0_0 + EFL_fixtures_sot$efl_1_1 + EFL_fixtures_sot$efl_2_2 + EFL_fixtures_sot$efl_3_3 + EFL_fixtures_sot$efl_4_4 +
    EFL_fixtures_sot$efl_5_5 + EFL_fixtures_sot$efl_6_6
)

EFL_fixtures_sot$efl_D <- percent(EFL_fixtures_sot$efl_D, accuracy = 0.1)

#Away

EFL_fixtures_sot$efl_A <- (
  EFL_fixtures_sot$efl_0_1 + EFL_fixtures_sot$efl_0_2 + EFL_fixtures_sot$efl_1_2 + EFL_fixtures_sot$efl_0_3 + EFL_fixtures_sot$efl_1_3 +
    EFL_fixtures_sot$efl_2_3 + EFL_fixtures_sot$efl_0_4 + EFL_fixtures_sot$efl_1_4 + EFL_fixtures_sot$efl_2_4 + EFL_fixtures_sot$efl_3_4 +
    EFL_fixtures_sot$efl_0_5 + EFL_fixtures_sot$efl_1_5 + EFL_fixtures_sot$efl_2_5 + EFL_fixtures_sot$efl_3_5 + EFL_fixtures_sot$efl_4_5 +
    EFL_fixtures_sot$efl_0_6 + EFL_fixtures_sot$efl_1_6 + EFL_fixtures_sot$efl_2_6 + EFL_fixtures_sot$efl_3_6 + EFL_fixtures_sot$efl_4_6 +
    EFL_fixtures_sot$efl_5_6
)

EFL_fixtures_sot$efl_A <- percent(EFL_fixtures_sot$efl_A, accuracy = 0.1)

#ov25
EFL_fixtures_sot$efl_ov25 <- (
  EFL_fixtures_sot$efl_2_1 + EFL_fixtures_sot$efl_1_2 + EFL_fixtures_sot$efl_2_2 + EFL_fixtures_sot$efl_3_0 + EFL_fixtures_sot$efl_3_1 +
    EFL_fixtures_sot$efl_3_2 + EFL_fixtures_sot$efl_0_3 + EFL_fixtures_sot$efl_1_3 + EFL_fixtures_sot$efl_2_3 + EFL_fixtures_sot$efl_3_3 +
    EFL_fixtures_sot$efl_4_0 + EFL_fixtures_sot$efl_4_1 + EFL_fixtures_sot$efl_4_2 + EFL_fixtures_sot$efl_4_3 + EFL_fixtures_sot$efl_0_4 +
    EFL_fixtures_sot$efl_1_4 + EFL_fixtures_sot$efl_2_4 + EFL_fixtures_sot$efl_3_4 + EFL_fixtures_sot$efl_4_4 + EFL_fixtures_sot$efl_5_0 +
    EFL_fixtures_sot$efl_5_1 + EFL_fixtures_sot$efl_5_2 + EFL_fixtures_sot$efl_5_3 + EFL_fixtures_sot$efl_5_4 + EFL_fixtures_sot$efl_0_5 +
    EFL_fixtures_sot$efl_1_5 + EFL_fixtures_sot$efl_2_5 + EFL_fixtures_sot$efl_3_5 + EFL_fixtures_sot$efl_4_5 + EFL_fixtures_sot$efl_5_5 +
    EFL_fixtures_sot$efl_6_0 + EFL_fixtures_sot$efl_6_1 + EFL_fixtures_sot$efl_6_2 + EFL_fixtures_sot$efl_6_3 + EFL_fixtures_sot$efl_6_4 +
    EFL_fixtures_sot$efl_6_5 + EFL_fixtures_sot$efl_0_6 + EFL_fixtures_sot$efl_1_6 + EFL_fixtures_sot$efl_2_6 + EFL_fixtures_sot$efl_3_6 +
    EFL_fixtures_sot$efl_4_6 + EFL_fixtures_sot$efl_5_6 + EFL_fixtures_sot$efl_6_6
)
#un25
EFL_fixtures_sot$efl_un25 <- (
  EFL_fixtures_sot$efl_0_0 + EFL_fixtures_sot$efl_1_0 + EFL_fixtures_sot$efl_0_1 + EFL_fixtures_sot$efl_1_1 + EFL_fixtures_sot$efl_2_0 + EFL_fixtures_sot$efl_0_2
)
#odds
EFL_fixtures_sot$efl_ov25_odds <- round((1/EFL_fixtures_sot$efl_ov25),digits = 2)
EFL_fixtures_sot$efl_un25_odds <- round((1/EFL_fixtures_sot$efl_un25),digits = 2)

EFL_fixtures_sot$efl_ov25_odds
EFL_fixtures_sot$efl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
EFL_fixtures_sot$efl_ov25 <- percent(EFL_fixtures_sot$efl_ov25, accuracy = 0.1)

EFL_fixtures_sot$efl_un25 <- percent(EFL_fixtures_sot$efl_un25, accuracy = 0.1)
EFL_fixtures_sot$efl_pssotre <- paste(round(EFL_fixtures_sot$efl_xHST,digits = 0),round(EFL_fixtures_sot$efl_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
efl_home_wins <- c()
efl_away_wins <- c()
efl_home_draws <- c()
efl_away_draws <- c()
efl_home_loss <- c()
efl_away_loss <- c()



for (i_efl_wins in 1:length(efl_teams))
{

  efl_home_wins[i_efl_wins] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_wins] & EFL$FTR == "H",])
  efl_away_wins[i_efl_wins] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_wins] & EFL$FTR == "A",])
  efl_home_draws[i_efl_wins] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_wins] & EFL$FTR == "D",])
  efl_away_draws[i_efl_wins] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_wins] & EFL$FTR == "D",])
  efl_home_loss[i_efl_wins] <- nrow(EFL[EFL$HomeTeam == efl_teams[i_efl_wins] & EFL$FTR == "A",])
  efl_away_loss[i_efl_wins] <- nrow(EFL[EFL$AwayTeam == efl_teams[i_efl_wins] & EFL$FTR == "H",])

}

efl_total_wins <- efl_home_wins + efl_away_wins
efl_total_draws <- efl_home_draws + efl_away_draws
efl_total_loss <- efl_home_loss + efl_away_loss

efl_league_table <- cbind(efl_teams,efl_games_played,efl_total_wins,efl_total_draws,efl_total_loss)
efl_GS <- efl_scoring$TGS
efl_GC <-efl_conceding$TGC
efl_GD <- efl_scoring$TGS - efl_conceding$TGC
efl_PTS <- (efl_total_wins*3) + (efl_total_draws*1)
efl_league_table <- cbind(efl_league_table,efl_GS,efl_GC,efl_GD,efl_PTS)
efl_league_table <- as.data.frame(efl_league_table)
#rename the columns
names(efl_league_table)[names(efl_league_table) == "efl_teams"] <- "Team"
names(efl_league_table)[names(efl_league_table) == "efl_games_played"] <- "P"
names(efl_league_table)[names(efl_league_table) == "efl_total_wins"] <- "W"
names(efl_league_table)[names(efl_league_table) == "efl_total_draws"] <- "D"
names(efl_league_table)[names(efl_league_table) == "efl_total_loss"] <- "L"
names(efl_league_table)[names(efl_league_table) == "efl_GS"] <- "F"
names(efl_league_table)[names(efl_league_table) == "efl_GC"] <- "A"
points_efl <- efl_league_table[order(as.numeric(efl_league_table$efl_PTS), decreasing = TRUE),]
points_efl$efl_rank <- 1:length(efl_teams)
row.names(points_efl) <- points_efl$efl_rank
#create final_efl_hf_against with team ranks in brackets
for(efl_rowhrank in 1:nrow(efl_form_team_against_h)) {
  for(efl_colhrank in 1:ncol(efl_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!efl_form_team_against_h[efl_rowhrank,efl_colhrank]=="",efl_form_team_against_h[efl_rowhrank,efl_colhrank] <- paste(efl_form_team_against_h[efl_rowhrank,efl_colhrank],"(",points_efl$efl_rank[points_efl$Team ==efl_form_team_against_h[efl_rowhrank,efl_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
efl_GP <- nrow(EFL)

#Calculate total home goals for each division
efl_T_HG <- sum(efl_home_gs$x)

#calculate average home goal
efl_avg_HG <- round(efl_T_HG /efl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
efl_T_AG <- sum(efl_away_gs$x)
#calculate average away goal
efl_avg_AG <- round(efl_T_AG /efl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
efl_home_as <- round(((efl_home_gs$x/efl_home_games))/efl_avg_HG, digits = 4)
#calculate away attack strength
efl_away_as <- round(((efl_away_gs$x/efl_away_games))/efl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
efl_avg_HC <- round(efl_T_AG /efl_GP, digits = 4)
#avg away concede
efl_avg_AC <- round(efl_T_HG /efl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
efl_home_ds <- round(((efl_home_gc$x/efl_home_games))/efl_avg_HC, digits = 4)
#away defense strength
efl_away_ds <- round(((efl_away_gc$x/efl_away_games))/efl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_home_poisson <- cbind(efl_division,efl_teams,efl_avg_HG,efl_home_as,efl_home_ds)
#################################################################################
#away poisson data
#efl
efl_division <- c()
efl_division[1:length(efl_teams)] <- "EFL"
efl_away_poisson <- cbind(efl_division,efl_teams,efl_avg_AG,efl_away_as,efl_away_ds)

#EFL
HomeTeam_efl <- rep(efl_teams, each = length(efl_teams))
AwayTeam_efl <- rep(efl_teams, length(efl_teams))
EFL_fixtures <- cbind(HomeTeam_efl,AwayTeam_efl)
EFL_fixtures <- as.data.frame(EFL_fixtures)
EFL_fixtures <- EFL_fixtures[!EFL_fixtures$HomeTeam_efl == EFL_fixtures$AwayTeam_efl,]
rownames(EFL_fixtures) <- NULL
EFL_fixtures$Div <- "EFL"
EFL_fixtures <- EFL_fixtures[,c(3,1,2)]

EFL_fixtures$avg_HG_efl <- efl_avg_HG

EFL_fixtures$efl_homeas <- rep(efl_home_as,each = length(efl_teams)-1)

efl_awayds_lookup <- cbind(efl_teams,efl_away_ds)

efl_awayds_lookup <- as.data.frame(efl_awayds_lookup)

colnames(efl_awayds_lookup) <- c("AwayTeam_efl","efl_awayds")


require('RH2')
EFL_fixtures$efl_awayds <- sqldf("SELECT efl_awayds_lookup.efl_awayds FROM efl_awayds_lookup INNER JOIN EFL_fixtures ON efl_awayds_lookup.AwayTeam_efl = EFL_fixtures.AwayTeam_efl")

EFL_fixtures$avg_AG_efl <- efl_avg_AG

efl_awayas_lookup <- cbind(efl_teams,efl_away_as)

efl_awayas_lookup <- as.data.frame(efl_awayas_lookup)

colnames(efl_awayas_lookup) <- c("AwayTeam_efl","efl_awayas")


EFL_fixtures$efl_awayas <- sqldf("SELECT efl_awayas_lookup.efl_awayas FROM efl_awayas_lookup INNER JOIN EFL_fixtures ON efl_awayas_lookup.AwayTeam_efl = EFL_fixtures.AwayTeam_efl")

EFL_fixtures$efl_homeds <- rep(efl_home_ds,each = length(efl_teams)-1)

EFL_fixtures$efl_awayds <- as.numeric(unlist(EFL_fixtures$efl_awayds))
#xGH
EFL_fixtures$efl_xGH <- EFL_fixtures$avg_HG_efl * EFL_fixtures$efl_homeas * EFL_fixtures$efl_awayds

#xGA

EFL_fixtures$efl_awayas <- as.numeric(unlist(EFL_fixtures$efl_awayas))

EFL_fixtures$efl_xGA <- EFL_fixtures$avg_AG_efl * EFL_fixtures$efl_awayas * EFL_fixtures$efl_homeds

EFL_fixtures$efl_0_0 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_0 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_0_1 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_1 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_0 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_0_2 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_2 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_1 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_2 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_3 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_0 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_1 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_2 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_0_3 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_3 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_3 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_4 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_0 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_1 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_2 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_3 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_0_4 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_4 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_4 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_4 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_5 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_0 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_1 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_2 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_3 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_4 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_0_5 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_5 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_5 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_5 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_5 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_6 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_0 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(0,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_1 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(1,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_2 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(2,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_3 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(3,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_4 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(4,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_6_5 <- round(stats::dpois(6,EFL_fixtures$efl_xGH) * stats::dpois(5,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_0_6 <- round(stats::dpois(0,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_1_6 <- round(stats::dpois(1,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_2_6 <- round(stats::dpois(2,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_3_6 <- round(stats::dpois(3,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_4_6 <- round(stats::dpois(4,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
EFL_fixtures$efl_5_6 <- round(stats::dpois(5,EFL_fixtures$efl_xGH) * stats::dpois(6,EFL_fixtures$efl_xGA), digits = 4)
#Home win
EFL_fixtures$efl_H <- (
  EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
    EFL_fixtures$efl_5_0 + EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5
)

EFL_fixtures$efl_H <- percent(EFL_fixtures$efl_H, accuracy = 0.1)

#Draw
EFL_fixtures$efl_D <- (

  EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 + EFL_fixtures$efl_4_4 +
    EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6
)

EFL_fixtures$efl_D <- percent(EFL_fixtures$efl_D, accuracy = 0.1)

#Away

EFL_fixtures$efl_A <- (
  EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
    EFL_fixtures$efl_0_5 + EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6
)

EFL_fixtures$efl_A <- percent(EFL_fixtures$efl_A, accuracy = 0.1)

#ov25
EFL_fixtures$efl_ov25 <- (
  EFL_fixtures$efl_2_1 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 + EFL_fixtures$efl_2_3 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 + EFL_fixtures$efl_0_4 +
    EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 + EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_0 +
    EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 + EFL_fixtures$efl_0_5 +
    EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 + EFL_fixtures$efl_5_5 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5 + EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 +
    EFL_fixtures$efl_4_6 + EFL_fixtures$efl_5_6 + EFL_fixtures$efl_6_6
)
#un25
EFL_fixtures$efl_un25 <- (
  EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_0 + EFL_fixtures$efl_0_1 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_0_2
)
#odds
EFL_fixtures$efl_ov25_odds <- round((1/EFL_fixtures$efl_ov25),digits = 2)
EFL_fixtures$efl_un25_odds <- round((1/EFL_fixtures$efl_un25),digits = 2)

EFL_fixtures$efl_ov25_odds
EFL_fixtures$efl_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
EFL_fixtures$efl_BTTSY <- (
  EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_3_1 + EFL_fixtures$efl_3_2 +
    EFL_fixtures$efl_2_2 + EFL_fixtures$efl_1_3 + EFL_fixtures$efl_2_3 + EFL_fixtures$efl_3_3 + EFL_fixtures$efl_4_4 +
    EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_3 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 +
    EFL_fixtures$efl_3_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 +
    EFL_fixtures$efl_5_4 + EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_6_6 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6
)
#BTTSN
EFL_fixtures$efl_BTTSN <- (
  EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_0 + EFL_fixtures$efl_0_1 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_0_2 +
    EFL_fixtures$efl_3_0 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_5_0 +
    EFL_fixtures$efl_0_5 + EFL_fixtures$efl_6_0 + EFL_fixtures$efl_0_6
)

EFL_fixtures$efl_BTTSY_odds <- round((1/EFL_fixtures$efl_BTTSY),digits = 2)
EFL_fixtures$efl_BTTSN_odds <- round((1/EFL_fixtures$efl_BTTSN),digits = 2)

EFL_fixtures$efl_BTTSY <- percent(EFL_fixtures$efl_BTTSY, accuracy = 0.1)
EFL_fixtures$efl_BTTSN <- percent(EFL_fixtures$efl_BTTSN, accuracy = 0.1)
#odds
EFL_fixtures$efl_BTTSY_odds
EFL_fixtures$efl_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
EFL_fixtures$efl_AH_0_H <- (
  EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
    EFL_fixtures$efl_5_0 +EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5 + EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6
)
#AH_0_A
EFL_fixtures$efl_AH_0_A <- (
  EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
    EFL_fixtures$efl_0_5 +EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6 + EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6
)

#odds
EFL_fixtures$efl_AH_0_H_odds <- round((1/EFL_fixtures$efl_AH_0_H),digits = 2)
EFL_fixtures$efl_AH_0_A_odds <- round((1/EFL_fixtures$efl_AH_0_A),digits = 2)

EFL_fixtures$efl_AH_0_H_odds
EFL_fixtures$efl_AH_0_A_odds
#percentages
EFL_fixtures$efl_AH_0_H <- percent(EFL_fixtures$efl_AH_0_H, accuracy = 0.1)
EFL_fixtures$efl_AH_0_A <- percent(EFL_fixtures$efl_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
EFL_fixtures$efl_AH_n075_H <- (
  EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
    EFL_fixtures$efl_5_0 +EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5
)
#AH_n075_A
EFL_fixtures$efl_AH_n075_A <- (
  EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
    EFL_fixtures$efl_0_5 +EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6
)

#odds
EFL_fixtures$efl_AH_n075_H_odds <- round((1/EFL_fixtures$efl_AH_n075_H),digits = 2)
EFL_fixtures$efl_AH_n075_A_odds <- round((1/EFL_fixtures$efl_AH_n075_A),digits = 2)

EFL_fixtures$efl_AH_n075_H_odds
EFL_fixtures$efl_AH_n075_A_odds
#percentages
EFL_fixtures$efl_AH_n075_H <- percent(EFL_fixtures$efl_AH_n075_H, accuracy = 0.1)
EFL_fixtures$efl_AH_n075_A <- percent(EFL_fixtures$efl_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
EFL_fixtures$efl_AH_075_H <- (
  EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
    EFL_fixtures$efl_5_0 +EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5 + EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6 + EFL_fixtures$efl_0_1 + EFL_fixtures$efl_1_2 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_3_4 + EFL_fixtures$efl_4_5 + EFL_fixtures$efl_5_6
)
#AH_075_A
EFL_fixtures$efl_AH_075_A <- (
  EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
    EFL_fixtures$efl_0_5 +EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6 + EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6 + EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_3 + EFL_fixtures$efl_5_4 + EFL_fixtures$efl_6_5
)

#odds
EFL_fixtures$efl_AH_075_H_odds <- round((1/EFL_fixtures$efl_AH_075_H),digits = 2)
EFL_fixtures$efl_AH_075_A_odds <- round((1/EFL_fixtures$efl_AH_075_A),digits = 2)

EFL_fixtures$efl_AH_075_H_odds
EFL_fixtures$efl_AH_075_A_odds
#percentages
EFL_fixtures$efl_AH_075_H <- percent(EFL_fixtures$efl_AH_075_H, accuracy = 0.1)
EFL_fixtures$efl_AH_075_A <- percent(EFL_fixtures$efl_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
EFL_fixtures$efl_AH_n125_H <- (
  EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
    EFL_fixtures$efl_5_0 +EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5
)
#AH_n125_A
EFL_fixtures$efl_AH_n125_A <- (
  EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
    EFL_fixtures$efl_0_5 +EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6
)

#odds
EFL_fixtures$efl_AH_n125_H_odds <- round((1/EFL_fixtures$efl_AH_n125_H),digits = 2)
EFL_fixtures$efl_AH_n125_A_odds <- round((1/EFL_fixtures$efl_AH_n125_A),digits = 2)

EFL_fixtures$efl_AH_n125_H_odds
EFL_fixtures$efl_AH_n125_A_odds
#percentages
EFL_fixtures$efl_AH_n125_H <- percent(EFL_fixtures$efl_AH_n125_H, accuracy = 0.1)
EFL_fixtures$efl_AH_n125_A <- percent(EFL_fixtures$efl_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
EFL_fixtures$efl_AH_125_H <- (
  EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
    EFL_fixtures$efl_5_0 +EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
    EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
    EFL_fixtures$efl_6_5 + EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6 + EFL_fixtures$efl_0_1 + EFL_fixtures$efl_1_2 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_3_4 + EFL_fixtures$efl_4_5 + EFL_fixtures$efl_5_6
)
#AH_125_A
EFL_fixtures$efl_AH_125_A <- (
  EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
    EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
    EFL_fixtures$efl_0_5 +EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
    EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
    EFL_fixtures$efl_5_6 + EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 +
    EFL_fixtures$efl_4_4 + EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6 + EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_1 +
    EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_3 + EFL_fixtures$efl_5_4 + EFL_fixtures$efl_6_5
)

#odds
EFL_fixtures$efl_AH_125_H_odds <- round((1/EFL_fixtures$efl_AH_125_H),digits = 2)
EFL_fixtures$efl_AH_125_A_odds <- round((1/EFL_fixtures$efl_AH_125_A),digits = 2)

EFL_fixtures$efl_AH_125_H_odds
EFL_fixtures$efl_AH_125_A_odds
#percentages
EFL_fixtures$efl_AH_125_H <- percent(EFL_fixtures$efl_AH_125_H, accuracy = 0.1)
EFL_fixtures$efl_AH_125_A <- percent(EFL_fixtures$efl_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
EFL_fixtures$efl_ov25 <- percent(EFL_fixtures$efl_ov25, accuracy = 0.1)

EFL_fixtures$efl_un25 <- percent(EFL_fixtures$efl_un25, accuracy = 0.1)
EFL_fixtures$efl_pscore <- paste(round(EFL_fixtures$efl_xGH,digits = 0),round(EFL_fixtures$efl_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
efl_last_n_games <- 6

#create final_efl_hf object
final_efl_hf <- c()
for(index_efl_hf in 1:length(efl_teams))
{
  index_efl_hf <- row.names(efl_form_h) == efl_teams[index_efl_hf]
  form_efl_hf <- efl_form_h[index_efl_hf]
  deleted_form_efl_hf <- form_efl_hf[!form_efl_hf[] == ""]
  l6_form_efl_hf <- tail(deleted_form_efl_hf,efl_last_n_games)
  l6_form_efl_hf <- paste(l6_form_efl_hf,collapse = " ")
  final_efl_hf[index_efl_hf] <- rbind(paste(efl_teams[index_efl_hf],l6_form_efl_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}

#change column nam
final_efl_hf <- as.data.frame(final_efl_hf)
colnames(final_efl_hf) <- "Form"
#goals scored
#create final_efl_gs object
final_efl_gs <- c()
suml6_efl_gs <- c()
for(index_efl_gs in 1:length(efl_teams))
{
  index_efl_gs <- row.names(efl_goalscored_h) == efl_teams[index_efl_gs]
  form_efl_gs <- efl_goalscored_h[index_efl_gs]
  deleted_form_efl_gs <- form_efl_gs[!form_efl_gs[] == ""]
  l6_form_efl_gs <- tail(deleted_form_efl_gs,efl_last_n_games)
  l6_form_efl_gs <- as.numeric(l6_form_efl_gs)
  suml6_efl_gs[index_efl_gs] <- sum(l6_form_efl_gs)
  suml6_efl_gs[index_efl_gs] <- paste("(",suml6_efl_gs[index_efl_gs],")",sep = "")
  l6_form_efl_gs <- paste(l6_form_efl_gs,collapse = " ")
  final_efl_gs[index_efl_gs] <- rbind(paste(efl_teams[index_efl_gs],l6_form_efl_gs,suml6_efl_gs[index_efl_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
final_efl_gs
#change column names
final_efl_gs <- as.data.frame(final_efl_gs)
colnames(final_efl_gs) <- "Goals scored"
#goal conceded
#create final_efl_gc object
final_efl_gc <- c()
suml6_efl_gc <- c()
for(index_efl_gc in 1:length(efl_teams))
{
  index_efl_gc <- row.names(efl_goalconceded_h) == efl_teams[index_efl_gc]
  form_efl_gc <- efl_goalconceded_h[index_efl_gc]
  deleted_form_efl_gc <- form_efl_gc[!form_efl_gc[] == ""]
  l6_form_efl_gc <- tail(deleted_form_efl_gc,efl_last_n_games)
  l6_form_efl_gc <- as.numeric(l6_form_efl_gc)
  suml6_efl_gc[index_efl_gc] <- sum(l6_form_efl_gc)
  suml6_efl_gc[index_efl_gc] <- paste("(",suml6_efl_gc[index_efl_gc],")",sep = "")
  l6_form_efl_gc <- paste(l6_form_efl_gc,collapse = " ")
  final_efl_gc[index_efl_gc] <- rbind(paste(efl_teams[index_efl_gc],l6_form_efl_gc,suml6_efl_gc[index_efl_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
#change column names
final_efl_gc <- as.data.frame(final_efl_gc)
colnames(final_efl_gc) <- "Goals conceded"


toString(l6_form_efl_gc)
#total goals
#create final_efl_tg object
final_efl_tg <- c()
suml6_efl_tg <- c()
for(index_efl_tg in 1:length(efl_teams))
{
  index_efl_tg <- row.names(efl_totalgoals_h) == efl_teams[index_efl_tg]
  form_efl_tg <- efl_totalgoals_h[index_efl_tg]
  deleted_form_efl_tg <- form_efl_tg[!form_efl_tg[] == ""]
  l6_form_efl_tg <- tail(deleted_form_efl_tg,efl_last_n_games)
  l6_form_efl_tg <- as.numeric(l6_form_efl_tg)
  suml6_efl_tg[index_efl_tg] <- sum(l6_form_efl_tg)
  suml6_efl_tg[index_efl_tg] <- paste("(",suml6_efl_tg[index_efl_tg],")",sep = "")
  l6_form_efl_tg <- paste(l6_form_efl_tg,collapse = " ")
  final_efl_tg[index_efl_tg] <- rbind(paste(efl_teams[index_efl_tg],l6_form_efl_tg,suml6_efl_tg[index_efl_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
#change column names
final_efl_tg <- as.data.frame(final_efl_tg)
colnames(final_efl_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_efl_hf object
final_efl_cs <- c()
for(index_efl_cs in 1:length(efl_teams))
{
  index_efl_cs <- row.names(efl_csform_h) == efl_teams[index_efl_cs]
  csform_efl_cs <- efl_csform_h[index_efl_cs]
  deleted_csform_efl_cs <- csform_efl_cs[!csform_efl_cs[] == ""]
  l6_csform_efl_cs <- tail(deleted_csform_efl_cs,efl_last_n_games)
  l6_csform_efl_cs <- paste(l6_csform_efl_cs,collapse = " ")
  final_efl_cs[index_efl_cs] <- rbind(paste(efl_teams[index_efl_cs],l6_csform_efl_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",efl_teams[index],l6_csform)

}

#change column names
final_efl_cs <- as.data.frame(final_efl_cs)
colnames(final_efl_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_efl_wm object
final_efl_wm <- c()
suml6_efl_wm <- c()
for(index_efl_wm in 1:length(efl_teams))
{
  index_efl_wm <- row.names(efl_winmargin_h) == efl_teams[index_efl_wm]
  form_efl_wm <- efl_winmargin_h[index_efl_wm]
  deleted_form_efl_wm <- form_efl_wm[!form_efl_wm[] == ""]
  l6_form_efl_wm <- tail(deleted_form_efl_wm,efl_last_n_games)
  l6_form_efl_wm <- as.numeric(l6_form_efl_wm)
  suml6_efl_wm[index_efl_wm] <- sum(l6_form_efl_wm)
  suml6_efl_wm[index_efl_wm] <- paste("(",suml6_efl_wm[index_efl_wm],")",sep = "")
  l6_form_efl_wm <- paste(l6_form_efl_wm,collapse = " ")
  final_efl_wm[index_efl_wm] <- rbind(paste(efl_teams[index_efl_wm],l6_form_efl_wm,suml6_efl_wm[index_efl_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
final_efl_wm
#change column names
final_efl_wm <- as.data.frame(final_efl_wm)
colnames(final_efl_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_efl_ca object
final_efl_ca <- c()
suml6_efl_ca <- c()
for(index_efl_ca in 1:length(efl_teams))
{
  index_efl_ca <- row.names(efl_coawarded_h) == efl_teams[index_efl_ca]
  form_efl_ca <- efl_coawarded_h[index_efl_ca]
  deleted_form_efl_ca <- form_efl_ca[!form_efl_ca[] == ""]
  l6_form_efl_ca <- tail(deleted_form_efl_ca,efl_last_n_games)
  l6_form_efl_ca <- as.numeric(l6_form_efl_ca)
  suml6_efl_ca[index_efl_ca] <- sum(l6_form_efl_ca)
  suml6_efl_ca[index_efl_ca] <- paste("(",suml6_efl_ca[index_efl_ca],")",sep = "")
  l6_form_efl_ca <- paste(l6_form_efl_ca,collapse = " ")
  final_efl_ca[index_efl_ca] <- rbind(paste(efl_teams[index_efl_ca],l6_form_efl_ca,suml6_efl_ca[index_efl_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
final_efl_ca
#change column names
final_efl_ca <- as.data.frame(final_efl_ca)
colnames(final_efl_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_efl_ca object
final_efl_cc <- c()
suml6_efl_cc <- c()
for(index_efl_cc in 1:length(efl_teams))
{
  index_efl_cc <- row.names(efl_cornersconceded_h) == efl_teams[index_efl_cc]
  form_efl_cc <- efl_cornersconceded_h[index_efl_cc]
  deleted_form_efl_cc <- form_efl_cc[!form_efl_cc[] == ""]
  l6_form_efl_cc <- tail(deleted_form_efl_cc,efl_last_n_games)
  l6_form_efl_cc <- as.numeric(l6_form_efl_cc)
  suml6_efl_cc[index_efl_cc] <- sum(l6_form_efl_cc)
  suml6_efl_cc[index_efl_cc] <- paste("(",suml6_efl_cc[index_efl_cc],")",sep = "")
  l6_form_efl_cc <- paste(l6_form_efl_cc,collapse = " ")
  final_efl_cc[index_efl_cc] <- rbind(paste(efl_teams[index_efl_cc],l6_form_efl_cc,suml6_efl_cc[index_efl_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
final_efl_cc
#change column names
final_efl_cc <- as.data.frame(final_efl_cc)
colnames(final_efl_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_efl_cosc <- c()
for(index_efl_cosc in 1:length(efl_teams))
{
  index_efl_cosc <- row.names(efl_coscform_h) == efl_teams[index_efl_cosc]
  coscform_efl_cosc <- efl_coscform_h[index_efl_cosc]
  deleted_coscform_efl_cosc <- coscform_efl_cosc[!coscform_efl_cosc[] == ""]
  l6_coscform_efl_cosc <- tail(deleted_coscform_efl_cosc,efl_last_n_games)
  l6_coscform_efl_cosc <- paste(l6_coscform_efl_cosc,collapse = " ")
  final_efl_cosc[index_efl_cosc] <- rbind(paste(efl_teams[index_efl_cosc],l6_coscform_efl_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",efl_teams[index],l6_coscform)

}
final_efl_cosc
#change column names
final_efl_cosc <- as.data.frame(final_efl_cosc)
colnames(final_efl_cosc) <- "CornersForm"
##################################################
#total corners
#create final_efl_tcorners object
final_efl_tcorners <- c()
suml6_efl_tcorners <- c()
for(index_efl_tcorners in 1:length(efl_teams))
{
  index_efl_tcorners <- row.names(efl_totalcorners_h) == efl_teams[index_efl_tcorners]
  form_efl_tcorners <- efl_totalcorners_h[index_efl_tcorners]
  deleted_form_efl_tcorners <- form_efl_tcorners[!form_efl_tcorners[] == ""]
  l6_form_efl_tcorners <- tail(deleted_form_efl_tcorners,efl_last_n_games)
  l6_form_efl_tcorners <- as.numeric(l6_form_efl_tcorners)
  suml6_efl_tcorners[index_efl_tcorners] <- sum(l6_form_efl_tcorners)
  suml6_efl_tcorners[index_efl_tcorners] <- paste("(",suml6_efl_tcorners[index_efl_tcorners],")",sep = "")
  l6_form_efl_tcorners <- paste(l6_form_efl_tcorners,collapse = " ")
  final_efl_tcorners[index_efl_tcorners] <- rbind(paste(efl_teams[index_efl_tcorners],l6_form_efl_tcorners,suml6_efl_tcorners[index_efl_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
#change column names
final_efl_tcorners <- as.data.frame(final_efl_tcorners)
colnames(final_efl_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_efl_hf_against
final_efl_hf_against <- c()
for(index_efl_hf_against in 1:length(efl_teams))
{
  index_efl_hf_against <- row.names(efl_form_team_against_h) == efl_teams[index_efl_hf_against]
  form_efl_hf_against <- efl_form_team_against_h[index_efl_hf_against]
  deleted_form_efl_hf_against <- form_efl_hf_against[!form_efl_hf_against[] == ""]
  l6_form_efl_hf_against <- tail(deleted_form_efl_hf_against,efl_last_n_games)
  l6_form_efl_hf_against <- paste(l6_form_efl_hf_against,collapse = " ")
  final_efl_hf_against[index_efl_hf_against] <- rbind(paste(efl_teams[index_efl_hf_against],l6_form_efl_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",efl_teams[index],l6_form)

}
final_efl_hf_against <- as.data.frame(final_efl_hf_against)
colnames(final_efl_hf_against) <- "Team against"
#combine the columns
final_efl_all <- cbind(final_efl_hf,final_efl_gs,final_efl_gc,final_efl_tg,final_efl_ca,final_efl_cc,final_efl_tcorners,final_efl_cosc,final_efl_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#EFL
EFL_sim <- EFL
EFL_sim$matchid <- paste(EFL_sim$HomeTeam,EFL_sim$AwayTeam,sep = "-")
EFL_fixtures$matchid <- paste(EFL_fixtures$HomeTeam_efl,EFL_fixtures$AwayTeam_efl,sep = "-")
EFL_fixtures$efl_FTR <- sapply(EFL_fixtures$efl_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

EFL_fixtures$efl_gamestatus <- ifelse(EFL_fixtures$matchid %in% EFL_sim$matchid,"played","notplayed")

efl_home_wins_sim <- c()
efl_away_wins_sim <- c()
efl_home_draws_sim <- c()
efl_away_draws_sim <- c()
efl_home_loss_sim <- c()
efl_away_loss_sim <- c()



for (i_efl_wins_sim in 1:length(efl_teams))
{

  efl_home_wins_sim[i_efl_wins_sim] <- nrow(EFL_fixtures[EFL_fixtures$HomeTeam_efl == efl_teams[i_efl_wins_sim] & EFL_fixtures$efl_FTR == "H" & EFL_fixtures$efl_gamestatus =="notplayed",])
  efl_away_wins_sim[i_efl_wins_sim] <- nrow(EFL_fixtures[EFL_fixtures$AwayTeam_efl == efl_teams[i_efl_wins_sim] & EFL_fixtures$efl_FTR == "A" & EFL_fixtures$efl_gamestatus == "notplayed",])
  efl_home_draws_sim[i_efl_wins_sim] <- nrow(EFL_fixtures[EFL_fixtures$HomeTeam_efl == efl_teams[i_efl_wins_sim] & EFL_fixtures$efl_FTR == "D" & EFL_fixtures$efl_gamestatus == "notplayed",])
  efl_away_draws_sim[i_efl_wins_sim] <- nrow(EFL_fixtures[EFL_fixtures$AwayTeam_efl == efl_teams[i_efl_wins_sim] & EFL_fixtures$efl_FTR == "D" & EFL_fixtures$efl_gamestatus == "notplayed",])
  efl_home_loss_sim[i_efl_wins_sim] <- nrow(EFL_fixtures[EFL_fixtures$HomeTeam_efl == efl_teams[i_efl_wins_sim] & EFL_fixtures$efl_FTR == "A" & EFL_fixtures$efl_gamestatus == "notplayed",])
  efl_away_loss_sim[i_efl_wins_sim] <- nrow(EFL_fixtures[EFL_fixtures$AwayTeam_efl == efl_teams[i_efl_wins_sim] & EFL_fixtures$efl_FTR == "H" & EFL_fixtures$efl_gamestatus == "notplayed", ])

}

efl_total_wins_sim <- efl_home_wins_sim + efl_away_wins_sim
efl_total_draws_sim <- efl_home_draws_sim + efl_away_draws_sim
efl_total_loss_sim <- efl_home_loss_sim + efl_away_loss_sim

efl_home_games_sim <- c()
efl_away_games_sim <-c()

for (i_efl_sim in 1:length(efl_teams))
{

  efl_home_games_sim[i_efl_sim] <- nrow(EFL_fixtures[EFL_fixtures$HomeTeam_efl == efl_teams[i_efl_sim] & EFL_fixtures$efl_gamestatus == "notplayed",])
  efl_away_games_sim[i_efl_sim]  <- nrow(EFL_fixtures[EFL_fixtures$AwayTeam_efl == efl_teams[i_efl_sim] & EFL_fixtures$efl_gamestatus == "notplayed",])

}

efl_games_played_sim <- efl_home_games_sim + efl_away_games_sim

efl_league_table_sim <- cbind(efl_teams,efl_games_played_sim,efl_total_wins_sim,efl_total_draws_sim,efl_total_loss_sim)
efl_PTS_sim <- (efl_total_wins_sim*3) + (efl_total_draws_sim*1)
efl_league_table_sim <- cbind(efl_league_table_sim,efl_PTS_sim)

efl_games_played_simfinal <- efl_games_played + efl_games_played_sim
efl_total_wins_simfinal <- efl_total_wins + efl_total_wins_sim
efl_total_draws_simfinal <- efl_total_draws + efl_total_draws_sim
efl_total_loss_simfinal <- efl_total_loss + efl_total_loss_sim
efl_PTS_simfinal <- efl_PTS + efl_PTS_sim

efl_league_table_simfinal <- cbind(efl_teams,efl_games_played_simfinal,efl_total_wins_simfinal,efl_total_draws_simfinal,efl_total_loss_simfinal,efl_PTS_simfinal)
efl_league_table_simfinal <- as.data.frame(efl_league_table_simfinal)
names(efl_league_table_simfinal)[names(efl_league_table_simfinal) == "efl_teams"] <- "Team_f"
names(efl_league_table_simfinal)[names(efl_league_table_simfinal) == "efl_games_played_simfinal"] <- "P_f"
names(efl_league_table_simfinal)[names(efl_league_table_simfinal) == "efl_total_wins_simfinal"] <- "W_f"
names(efl_league_table_simfinal)[names(efl_league_table_simfinal) == "efl_total_draws_simfinal"] <- "D_f"
names(efl_league_table_simfinal)[names(efl_league_table_simfinal) == "efl_total_loss_simfinal"] <- "L_f"
names(efl_league_table_simfinal)[names(efl_league_table_simfinal) == "efl_PTS_simfinal"] <- "PTS_f"
points_efl_sim <-  efl_league_table_simfinal[order(as.numeric(efl_league_table_simfinal$PTS_f), decreasing = TRUE),]

EFL_notplayed <- EFL_fixtures[EFL_fixtures$efl_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#EFL
EFL_fixtures$Hometeam_efl_index <- match(EFL_fixtures$HomeTeam_efl,efl_teams)
EFL_fixtures$Awayteam_efl_index <- match(EFL_fixtures$AwayTeam_efl,efl_teams)
efl_prediction <- c()
efl_HWM <- c()
efl_AWM <- c()
efl_HWMLM <- c()
efl_AWMLM <- c()
efl_HY <- c()
efl_AY <- c()
efl_HCO <- c()
efl_ACO <- c()
efl_HXSC <- c()
efl_AXSC <- c()
efl_HYCPF <- c()
efl_AYCPF <- c()
for(efl_row in 1:nrow(EFL_fixtures))
{

  efl_hometeamindex <- EFL_fixtures[efl_row,"Hometeam_efl_index"]
  efl_awayteamindex <- EFL_fixtures[efl_row,"Awayteam_efl_index"]
  #analyse team form
  #home team
  efl_form_vec_ht <- as.vector(efl_form_h[efl_hometeamindex,])
  efl_form_vec_ht[is.na(efl_form_vec_ht)] <- ""
  efl_form_vec_ht <- efl_form_vec_ht[efl_form_vec_ht != ""]
  efl_form_vec_ht  <-tail(efl_form_vec_ht,6)
  efl_ht_numberof_wins <- length(which(efl_form_vec_ht == "W"))
  efl_ht_numberof_draws <- length(which(efl_form_vec_ht == "D"))
  efl_ht_numberof_loss <- length(which(efl_form_vec_ht == "L"))
  #awayteam
  efl_form_vec_at <- as.vector(efl_form_h[efl_awayteamindex,])
  efl_form_vec_at[is.na(efl_form_vec_at)] <- ""
  efl_form_vec_at <- efl_form_vec_at[efl_form_vec_at != ""]
  efl_form_vec_at  <-tail(efl_form_vec_at,6)
  efl_at_numberof_wins <- length(which(efl_form_vec_at == "W"))
  efl_at_numberof_draws <- length(which(efl_form_vec_at == "D"))
  efl_at_numberof_loss <- length(which(efl_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  efl_goalscored_vec_ht <- as.vector(efl_goalscored_h[efl_hometeamindex,])
  efl_goalscored_vec_ht[is.na(efl_goalscored_vec_ht)] <- ""
  efl_goalscored_vec_ht <- efl_goalscored_vec_ht[efl_goalscored_vec_ht != ""]
  efl_goalscored_vec_ht  <-tail(efl_goalscored_vec_ht,6)
  efl_goalscored_vec_ht  <- as.numeric(efl_goalscored_vec_ht)
  efl_ht_totalgoalscored <- sum(efl_goalscored_vec_ht)
  efl_ht_matches_scoring <- length(which(efl_goalscored_vec_ht > 0))
  efl_ht_matches_without_scoring <- length(which(efl_goalscored_vec_ht == "0"))
  #awayteam
  efl_goalscored_vec_at <- as.vector(efl_goalscored_h[efl_awayteamindex,])
  efl_goalscored_vec_at[is.na(efl_goalscored_vec_at)] <- ""
  efl_goalscored_vec_at <- efl_goalscored_vec_at[efl_goalscored_vec_at != ""]
  efl_goalscored_vec_at  <-tail(efl_goalscored_vec_at,6)
  efl_goalscored_vec_at  <- as.numeric(efl_goalscored_vec_at)
  efl_at_totalgoalscored <- sum(efl_goalscored_vec_at)
  efl_at_matches_scoring <- length(which(efl_goalscored_vec_at > 0))
  efl_at_matches_without_scoring <- length(which(efl_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  efl_goalconceded_vec_ht <- as.vector(efl_goalconceded_h[efl_hometeamindex,])
  efl_goalconceded_vec_ht[is.na(efl_goalconceded_vec_ht)] <- ""
  efl_goalconceded_vec_ht <- efl_goalconceded_vec_ht[efl_goalconceded_vec_ht != ""]
  efl_goalconceded_vec_ht  <-tail(efl_goalconceded_vec_ht,6)
  efl_goalconceded_vec_ht  <- as.numeric(efl_goalconceded_vec_ht)
  efl_goalconceded_vec_ht
  efl_ht_totalgoalconceded <- sum(efl_goalconceded_vec_ht)
  efl_ht_matches_concede <- length(which(efl_goalconceded_vec_ht > 0))
  efl_ht_matches_without_concede <- length(which(efl_goalconceded_vec_ht == "0"))
  #awayteam
  efl_goalconceded_vec_at <- as.vector(efl_goalconceded_h[efl_awayteamindex,])
  efl_goalconceded_vec_at[is.na(efl_goalconceded_vec_at)] <- ""
  efl_goalconceded_vec_at <- efl_goalconceded_vec_at[efl_goalconceded_vec_at != ""]
  efl_goalconceded_vec_at  <-tail(efl_goalconceded_vec_at,6)
  efl_goalconceded_vec_at  <- as.numeric(efl_goalconceded_vec_at)
  efl_at_totalgoalconceded <- sum(efl_goalconceded_vec_at)
  efl_at_matches_concede <- length(which(efl_goalconceded_vec_at > 0))
  efl_at_matches_without_concede <- length(which(efl_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  efl_totalgoals_vec_ht <- as.vector(efl_totalgoals_h[efl_hometeamindex,])
  efl_totalgoals_vec_ht[is.na(efl_totalgoals_vec_ht)] <- ""
  efl_totalgoals_vec_ht <- efl_totalgoals_vec_ht[efl_totalgoals_vec_ht != ""]
  efl_totalgoals_vec_ht  <-tail(efl_totalgoals_vec_ht,6)
  efl_totalgoals_vec_ht  <- as.numeric(efl_totalgoals_vec_ht)
  efl_totalgoals_vec_ht
  efl_ht_totalgoals <- sum(efl_totalgoals_vec_ht)
  efl_ht_avgtotalgoals <- (efl_ht_totalgoals/6)
  efl_ht_no_of_ov25 <- length(which(efl_totalgoals_vec_ht >= 3))
  efl_ht_no_of_un25 <- length(which(efl_totalgoals_vec_ht <= 2))
  #awayteam
  efl_totalgoals_vec_at <- as.vector(efl_totalgoals_h[efl_awayteamindex,])
  efl_totalgoals_vec_at[is.na(efl_totalgoals_vec_at)] <- ""
  efl_totalgoals_vec_at <- efl_totalgoals_vec_at[efl_totalgoals_vec_at != ""]
  efl_totalgoals_vec_at  <-tail(efl_totalgoals_vec_at,6)
  efl_totalgoals_vec_at  <- as.numeric(efl_totalgoals_vec_at)
  efl_totalgoals_vec_at
  efl_at_totalgoals <- sum(efl_totalgoals_vec_at)
  efl_at_avgtotalgoals <- (efl_at_totalgoals/6)
  efl_at_no_of_ov25 <- length(which(efl_totalgoals_vec_at >= 3))
  efl_at_no_of_un25 <- length(which(efl_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  efl_winmargin_vec_ht <- as.vector(efl_winmargin_h[efl_hometeamindex,])
  efl_winmargin_vec_ht[is.na(efl_winmargin_vec_ht)] <- ""
  efl_winmargin_vec_ht <- efl_winmargin_vec_ht[efl_winmargin_vec_ht != ""]
  efl_winmargin_vec_ht  <-tail(efl_winmargin_vec_ht,6)
  efl_winmargin_vec_ht  <- as.numeric(efl_winmargin_vec_ht)

  efl_ht_totalwinmargin <- sum(efl_winmargin_vec_ht)
  efl_ht_no_of_winmargin_ov0 <- length(which(efl_winmargin_vec_ht >= 0))
  efl_ht_no_of_winmargin_ov1 <- length(which(efl_winmargin_vec_ht >= 1))
  efl_ht_no_of_winmargin_un0 <- length(which(efl_winmargin_vec_ht <= 0))
  efl_ht_no_of_winmargin_un1 <- length(which(efl_winmargin_vec_ht <= 1))
  #awayteam
  efl_winmargin_vec_at <- as.vector(efl_winmargin_h[efl_awayteamindex,])
  efl_winmargin_vec_at[is.na(efl_winmargin_vec_at)] <- ""
  efl_winmargin_vec_at <- efl_winmargin_vec_at[efl_winmargin_vec_at != ""]
  efl_winmargin_vec_at  <-tail(efl_winmargin_vec_at,6)
  efl_winmargin_vec_at  <- as.numeric(efl_winmargin_vec_at)

  efl_at_totalwinmargin <- sum(efl_winmargin_vec_at)
  efl_at_no_of_winmargin_ov0 <- length(which(efl_winmargin_vec_at >= 0))
  efl_at_no_of_winmargin_ov1 <- length(which(efl_winmargin_vec_at >= 1))
  efl_at_no_of_winmargin_un0 <- length(which(efl_winmargin_vec_at <= 0))
  efl_at_no_of_winmargin_un1 <- length(which(efl_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  efl_winmargin_vec_ht_lm <- as.vector(efl_winmargin_h[efl_hometeamindex,])
  efl_winmargin_vec_ht_lm[is.na(efl_winmargin_vec_ht_lm)] <- ""
  efl_winmargin_vec_ht_lm <- efl_winmargin_vec_ht_lm[efl_winmargin_vec_ht_lm != ""]
  efl_winmargin_vec_ht_lm  <-tail(efl_winmargin_vec_ht_lm,1)
  #awayteam
  efl_winmargin_vec_at_lm <- as.vector(efl_winmargin_h[efl_awayteamindex,])
  efl_winmargin_vec_at_lm[is.na(efl_winmargin_vec_at_lm)] <- ""
  efl_winmargin_vec_at_lm <- efl_winmargin_vec_at_lm[efl_winmargin_vec_at_lm != ""]
  efl_winmargin_vec_at_lm  <-tail(efl_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  efl_yellowtotals_vec_ht <- as.vector(efl_yellowtotalsv2[efl_hometeamindex,])
  efl_yellowtotals_vec_ht[is.na(efl_yellowtotals_vec_ht)] <- ""
  efl_yellowtotals_vec_ht <- efl_yellowtotals_vec_ht[efl_yellowtotals_vec_ht != ""]
  efl_yellowtotals_vec_ht  <-tail(efl_yellowtotals_vec_ht,1)
  #awayteam
  efl_yellowtotals_vec_at <- as.vector(efl_yellowtotalsv2[efl_awayteamindex,])
  efl_yellowtotals_vec_at[is.na(efl_yellowtotals_vec_at)] <- ""
  efl_yellowtotals_vec_at <- efl_yellowtotals_vec_at[efl_yellowtotals_vec_at != ""]
  efl_yellowtotals_vec_at  <-tail(efl_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  efl_cornertotals_vec_ht <- as.vector(efl_cornertotalsv2[efl_hometeamindex,])
  efl_cornertotals_vec_ht[is.na(efl_cornertotals_vec_ht)] <- ""
  efl_cornertotals_vec_ht <- efl_cornertotals_vec_ht[efl_cornertotals_vec_ht != ""]
  efl_cornertotals_vec_ht  <-tail(efl_cornertotals_vec_ht,1)
  #awayteam
  efl_cornertotals_vec_at <- as.vector(efl_cornertotalsv2[efl_awayteamindex,])
  efl_cornertotals_vec_at[is.na(efl_cornertotals_vec_at)] <- ""
  efl_cornertotals_vec_at <- efl_cornertotals_vec_at[efl_cornertotals_vec_at != ""]
  efl_cornertotals_vec_at  <-tail(efl_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  efl_xshotsconversion_vec_ht <- as.vector(efl_shots_analysis[efl_hometeamindex,])
  efl_xshotsconversion_vec_ht[is.na(efl_xshotsconversion_vec_ht)] <- ""
  efl_xshotsconversion_vec_ht <- efl_xshotsconversion_vec_ht[efl_xshotsconversion_vec_ht != ""]
  efl_xshotsconversion_vec_ht  <-tail(efl_xshotsconversion_vec_ht,1)
  #awayteam
  efl_xshotsconversion_vec_at <- as.vector(efl_shots_analysis[efl_awayteamindex,])
  efl_xshotsconversion_vec_at[is.na(efl_xshotsconversion_vec_at)] <- ""
  efl_xshotsconversion_vec_at <- efl_xshotsconversion_vec_at[efl_xshotsconversion_vec_at != ""]
  efl_xshotsconversion_vec_at  <-tail(efl_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  efl_fouls_conversion_vec_ht <- as.vector(efl_fouls_conversion[efl_hometeamindex,])
  efl_fouls_conversion_vec_ht[is.na(efl_fouls_conversion_vec_ht)] <- ""
  efl_fouls_conversion_vec_ht <- efl_fouls_conversion_vec_ht[efl_fouls_conversion_vec_ht != ""]
  efl_fouls_conversion_vec_ht  <-tail(efl_fouls_conversion_vec_ht,1)
  #awayteam
  efl_fouls_conversion_vec_at <- as.vector(efl_fouls_conversion[efl_awayteamindex,])
  efl_fouls_conversion_vec_at[is.na(efl_fouls_conversion_vec_at)] <- ""
  efl_fouls_conversion_vec_at <- efl_fouls_conversion_vec_at[efl_fouls_conversion_vec_at != ""]
  efl_fouls_conversion_vec_at  <-tail(efl_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  efl_ht_last6points <- efl_ht_numberof_wins*3 + efl_ht_numberof_draws*1
  efl_at_last6points <- efl_at_numberof_wins*3 + efl_at_numberof_draws*1

  if(efl_ht_last6points > efl_at_last6points) {efl_3waypick <- "1"}  else {efl_3waypick <- "X2"}

  if(efl_at_last6points > efl_ht_last6points ) {efl_3waypick <- "2"} else {efl_3waypick <- "1X"}

  if(efl_ht_no_of_ov25 + efl_at_no_of_ov25 >= 6) {efl_goalspick <- "ov25"} else {efl_goalspick <- "un25"}

  if(efl_ht_no_of_un25 + efl_at_no_of_un25 >= 6) {efl_goalspick <- "un25"} else {efl_goalspick <- "ov25"}

  if(efl_ht_matches_scoring >= 4 && efl_at_matches_scoring >=4) {efl_btts <- "BTTS-Y"} else {efl_btts <- "BTTS-N"}


  efl_prediction[efl_row] <- rbind(paste(efl_3waypick,efl_goalspick,efl_btts,sep = ","))
  efl_HWM[efl_row] <- efl_ht_totalwinmargin
  efl_AWM[efl_row] <- efl_at_totalwinmargin

  efl_HWMLM[efl_row] <- efl_winmargin_vec_ht_lm
  efl_AWMLM[efl_row] <- efl_winmargin_vec_at_lm

  efl_HY[efl_row] <- efl_yellowtotals_vec_ht
  efl_AY[efl_row] <- efl_yellowtotals_vec_at

  efl_HCO[efl_row] <- efl_cornertotals_vec_ht
  efl_ACO[efl_row] <- efl_cornertotals_vec_at

  efl_HXSC[efl_row] <- efl_xshotsconversion_vec_ht
  efl_AXSC[efl_row] <- efl_xshotsconversion_vec_at

  efl_HYCPF[efl_row] <- efl_fouls_conversion_vec_ht
  efl_AYCPF[efl_row] <- efl_fouls_conversion_vec_at
}

efl_prediction <- as.data.frame(efl_prediction)
colnames(efl_prediction) <- "prediction"

efl_HWM <- as.data.frame(efl_HWM)
colnames(efl_HWM) <- "HWM"

efl_AWM <- as.data.frame(efl_AWM)
colnames(efl_AWM) <- "AWM"

efl_HWMLM <- as.data.frame(efl_HWMLM)
colnames(efl_HWMLM) <- "HWMLM"

efl_AWMLM <- as.data.frame(efl_AWMLM)
colnames(efl_AWMLM) <- "AWMLM"

efl_HY <- as.data.frame(efl_HY)
colnames(efl_HY) <- "AVGHY"

efl_AY <- as.data.frame(efl_AY)
colnames(efl_AY) <- "AVGAY"

efl_HCO <- as.data.frame(efl_HCO)
colnames(efl_HCO) <- "AVGHCO"

efl_ACO <- as.data.frame(efl_ACO)
colnames(efl_ACO) <- "AVGACO"

efl_HXSC <- as.data.frame(efl_HXSC)
colnames(efl_HXSC) <- "HXSC"

efl_AXSC <- as.data.frame(efl_AXSC)
colnames(efl_AXSC) <- "AXSC"

efl_HYCPF <- as.data.frame(efl_HYCPF)
colnames(efl_HYCPF) <- "HYCPF"

efl_AYCPF <- as.data.frame(efl_AYCPF)
colnames(efl_AYCPF) <- "AYCPF"

efl_picks <- cbind(EFL_fixtures$Div,EFL_fixtures$HomeTeam_efl,EFL_fixtures$AwayTeam_efl,efl_prediction,efl_HWM,efl_AWM,efl_HWMLM,efl_AWMLM,efl_HY,efl_AY,efl_HCO,efl_ACO,efl_HXSC,efl_AXSC,efl_HYCPF,efl_AYCPF)

colnames(efl_picks)[1] <- "picks_Div"
colnames(efl_picks)[2] <- "picks_HomeTeam"
colnames(efl_picks)[3] <- "picks_AwayTeam"
efl_picks$matchid <- paste(efl_picks$picks_HomeTeam,efl_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of EFL
efl_picks
#############################################################################################################################################################################
#clone fixtures
EFL_fixtures_clone <- EFL_fixtures
colnames(EFL_fixtures_clone)[61] <- "Hwin"
colnames(EFL_fixtures_clone)[62] <- "Draw"
colnames(EFL_fixtures_clone)[63] <- "Awin"

EFL_fixtures_clone$Hwinodds <-   EFL_fixtures$efl_1_0 + EFL_fixtures$efl_2_0 + EFL_fixtures$efl_2_1 + EFL_fixtures$efl_3_0 + EFL_fixtures$efl_3_1 +
  EFL_fixtures$efl_3_2 + EFL_fixtures$efl_4_0 + EFL_fixtures$efl_4_1 + EFL_fixtures$efl_4_2 + EFL_fixtures$efl_4_3 +
  EFL_fixtures$efl_5_0 + EFL_fixtures$efl_5_1 + EFL_fixtures$efl_5_2 + EFL_fixtures$efl_5_3 + EFL_fixtures$efl_5_4 +
  EFL_fixtures$efl_6_0 + EFL_fixtures$efl_6_1 + EFL_fixtures$efl_6_2 + EFL_fixtures$efl_6_3 + EFL_fixtures$efl_6_4 +
  EFL_fixtures$efl_6_5
EFL_fixtures_clone$Hwinodds <- round(1/EFL_fixtures_clone$Hwinodds, digits = 3)

EFL_fixtures_clone$Drawodds <-  EFL_fixtures$efl_0_0 + EFL_fixtures$efl_1_1 + EFL_fixtures$efl_2_2 + EFL_fixtures$efl_3_3 + EFL_fixtures$efl_4_4 +
  EFL_fixtures$efl_5_5 + EFL_fixtures$efl_6_6

EFL_fixtures_clone$Drawodds <- round(1/EFL_fixtures_clone$Drawodds, digits = 3)

EFL_fixtures_clone$Awinodds <-   EFL_fixtures$efl_0_1 + EFL_fixtures$efl_0_2 + EFL_fixtures$efl_1_2 + EFL_fixtures$efl_0_3 + EFL_fixtures$efl_1_3 +
  EFL_fixtures$efl_2_3 + EFL_fixtures$efl_0_4 + EFL_fixtures$efl_1_4 + EFL_fixtures$efl_2_4 + EFL_fixtures$efl_3_4 +
  EFL_fixtures$efl_0_5 + EFL_fixtures$efl_1_5 + EFL_fixtures$efl_2_5 + EFL_fixtures$efl_3_5 + EFL_fixtures$efl_4_5 +
  EFL_fixtures$efl_0_6 + EFL_fixtures$efl_1_6 + EFL_fixtures$efl_2_6 + EFL_fixtures$efl_3_6 + EFL_fixtures$efl_4_6 +
  EFL_fixtures$efl_5_6

EFL_fixtures_clone$Awinodds <- round(1/EFL_fixtures_clone$Awinodds, digits = 3)

colnames(EFL_fixtures_clone)[15] <- "CS_1-1"
colnames(EFL_fixtures_clone)[13] <- "CS_1-0"
colnames(EFL_fixtures_clone)[14] <- "CS_0-1"
colnames(EFL_fixtures_clone)[16] <- "CS_2-0"
colnames(EFL_fixtures_clone)[17] <- "CS_0-2"
colnames(EFL_fixtures_clone)[19] <- "CS_2-1"
colnames(EFL_fixtures_clone)[20] <- "CS_1-2"

EFL_fixtures_clone$`CS_1-1` <- round(1/EFL_fixtures_clone$`CS_1-1`, digits = 3)
EFL_fixtures_clone$`CS_1-0` <- round(1/EFL_fixtures_clone$`CS_1-0`, digits = 3)
EFL_fixtures_clone$`CS_0-1` <- round(1/EFL_fixtures_clone$`CS_0-1`, digits = 3)
EFL_fixtures_clone$`CS_2-0` <- round(1/EFL_fixtures_clone$`CS_2-0`, digits = 3)
EFL_fixtures_clone$`CS_0-2` <- round(1/EFL_fixtures_clone$`CS_0-2`, digits = 3)
EFL_fixtures_clone$`CS_2-1` <- round(1/EFL_fixtures_clone$`CS_2-1`, digits = 3)
EFL_fixtures_clone$`CS_1-2` <- round(1/EFL_fixtures_clone$`CS_1-2`, digits = 3)

colnames(EFL_fixtures_clone)[1] <- "league"
colnames(EFL_fixtures_clone)[2] <- "Hometeam"
colnames(EFL_fixtures_clone)[3] <- "Awayteam"
colnames(EFL_fixtures_clone)[92] <- "predscore"
colnames(EFL_fixtures_clone)[64] <- "ov25"
colnames(EFL_fixtures_clone)[66] <- "ov25odds"
colnames(EFL_fixtures_clone)[65] <- "un25"
colnames(EFL_fixtures_clone)[67] <- "un25odds"
colnames(EFL_fixtures_clone)[68] <- "BTTSY"
colnames(EFL_fixtures_clone)[69] <- "BTTSN"
colnames(EFL_fixtures_clone)[70] <- "BTTSYodds"
colnames(EFL_fixtures_clone)[71] <- "BTTSNodds"

EFL_fixtures_clone <- EFL_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
EFL_fixtures_clone$matchid <- paste(EFL_fixtures_clone$Hometeam,EFL_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
EFL_fixtures_clone_final <- EFL_fixtures_clone[,-c(8,9,10,27)]
EFL_fixtures_clone_final[,'sep'] <- ''

efl_dmprediction <-  efl_picks[,c(4,5,6,7,8)]
efl_dmprediction[,'sep2'] <- ''

efl_avgyellow <- efl_picks[,c(9,10)]
efl_avgyellow[,'sep3'] <- ''

efl_avgcorners <- efl_picks[,c(11,12)]
efl_avgcorners[,'sep4'] <- ''

efl_goals <- EFL_fixtures[,c(10,11)]
efl_goals$efl_xGH <- round(efl_goals$efl_xGH, digits = 2)
efl_goals$efl_xGA <- round(efl_goals$efl_xGA, digits = 2)
efl_goals$efl_TxG <- efl_goals$efl_xGH + efl_goals$efl_xGA
efl_goals[,'sep5'] <- ''

efl_shots <- EFL_fixtures_sot[,c(10,11)]
efl_shots$efl_xHST <- round(efl_shots$efl_xHST, digits = 2)
efl_shots$efl_xAST <- round(efl_shots$efl_xAST, digits = 2)
efl_shots$TxSOT <- efl_shots$efl_xHST + efl_shots$efl_xAST
efl_shots[,'sep6'] <- ''

efl_fouls <- EFL_fixtures_fo[,c(10,11)]
efl_fouls$efl_xHF <- round(efl_fouls$efl_xHF, digits = 2)
efl_fouls$efl_xAF <- round(efl_fouls$efl_xAF, digits = 2)
efl_fouls$efl_TxF <- efl_fouls$efl_xHF + efl_fouls$efl_xAF

efl_ycpf <- efl_picks[,c(15,16)]
efl_fouls <- cbind(efl_fouls,efl_ycpf)
efl_fouls$HYCPF <- as.numeric(efl_fouls$HYCPF)
efl_fouls$AYCPF <- as.numeric(efl_fouls$AYCPF)
efl_fouls$x_hyc <- (efl_fouls$efl_xHF) * (efl_fouls$HYCPF)
efl_fouls$x_ayc <- (efl_fouls$efl_xAF) * (efl_fouls$AYCPF)
efl_fouls$x_TYC <- round((efl_fouls$x_hyc + efl_fouls$x_ayc),digits = 2)
efl_fouls[,'sep7'] <- ''

efl_bookings <- EFL_fixtures_yc[,c(10,11)]
efl_bookings$efl_xHYC <- round(efl_bookings$efl_xHYC, digits = 2)
efl_bookings$efl_xAYC <- round(efl_bookings$efl_xAYC, digits = 2)
efl_bookings$efl_TYcards <- efl_bookings$efl_xHYC + efl_bookings$efl_xAYC
efl_bookings[,'sep8'] <- ''

efl_corners <- EFL_fixtures_co[,c(10,11)]
efl_corners$efl_xHCOC <- round(efl_corners$efl_xHCOC, digits = 2)
efl_corners$efl_xACOC <- round(efl_corners$efl_xACOC, digits = 2)
efl_corners$efl_TCOs <- efl_corners$efl_xHCOC + efl_corners$efl_xACOC
efl_corners[,'sep9'] <- ''

efl_shotsconversion <- efl_picks[,c(13,14)]
efl_shotsconversion <- cbind(efl_shotsconversion,efl_shots)
efl_shotsconversion$HXSC <- as.numeric(efl_shotsconversion$HXSC)
efl_shotsconversion$AXSC <- as.numeric(efl_shotsconversion$AXSC)
efl_shotsconversion$efl_hXgoals <- round((efl_shotsconversion$HXSC * efl_shotsconversion$efl_xHST), digits = 2)
efl_shotsconversion$efl_aXgoals <- round((efl_shotsconversion$AXSC * efl_shotsconversion$efl_xAST), digits = 2)
efl_shotsconversion$Xgoals <- efl_shotsconversion$efl_hXgoals + efl_shotsconversion$efl_aXgoals
options(java.parameters = "-Xmx4g")
EFL_all <- cbind(EFL_fixtures_clone_final,efl_dmprediction,efl_avgyellow,efl_avgcorners,efl_goals,efl_shots,efl_fouls,efl_bookings,efl_corners,efl_shotsconversion)
unlink('Divisions/EFL.xlsx')
write.xlsx(EFL_all,'Divisions/EFL.xlsx', sheetName = "EFL_all", append = TRUE)
write.xlsx(points_efl,'Divisions/EFL.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(efl_cornertotalsv2,'Divisions/EFL.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(efl_goaltotalsv2,'Divisions/EFL.xlsx', sheetName = "Goaltotals", append = TRUE)
write.xlsx(efl_yellowtotalsv2,'Divisions/EFL.xlsx', sheetName = "Yellowtotals", append = TRUE)


##write.csv(EFL_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
