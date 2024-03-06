#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
d2_currentround
#first_df <- I1_rounds[I1_rounds$i1_matchday > 18,]
#second_df <- N1_rounds[N1_rounds$n1_matchday > 16,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#BUNDESLIGATWO <- rbind(first_df,second_df)
BUNDESLIGATWO <- D2_rounds[D2_rounds$d2_matchday > 17,]
#BUNDESLIGATWO <- na.omit(BUNDESLIGATWO)
#goaltotals v2
bundesligatwo_goaltotalsv2 <- tapply(BUNDESLIGATWO$TG, BUNDESLIGATWO[c("HomeTeam", "AwayTeam")],mean)
bundesligatwo_hgtotals <- rowSums(bundesligatwo_goaltotalsv2, na.rm = T)
bundesligatwo_agtotals <- colSums(bundesligatwo_goaltotalsv2, na.rm = T)
bundesligatwo_goaltotalsv2 <- cbind(bundesligatwo_goaltotalsv2,bundesligatwo_hgtotals,bundesligatwo_agtotals)
bundesligatwo_totalgoals <- bundesligatwo_hgtotals + bundesligatwo_agtotals
bundesligatwo_goaltotalsv2 <- cbind(bundesligatwo_goaltotalsv2,bundesligatwo_totalgoals)
bundesligatwo_teams <- sort(unique(BUNDESLIGATWO$HomeTeam))
bundesligatwo_home_games <- c()
bundesligatwo_away_games <-c()
for (i_bundesligatwo in 1:length(bundesligatwo_teams))
{

  bundesligatwo_home_games[i_bundesligatwo] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo],])
  bundesligatwo_away_games[i_bundesligatwo]  <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo],])

}
bundesligatwo_games_played <- bundesligatwo_home_games + bundesligatwo_away_games
bundesligatwo_goaltotalsv2 <- cbind(bundesligatwo_goaltotalsv2,bundesligatwo_games_played)
bundesligatwo_avg_totalgoals <- round((bundesligatwo_totalgoals/ bundesligatwo_games_played), digits = 4)
bundesligatwo_goaltotalsv2[is.na(bundesligatwo_goaltotalsv2)] <- ""
bundesligatwo_goaltotalsv2 <- cbind(bundesligatwo_goaltotalsv2,bundesligatwo_avg_totalgoals)

############################################################################################################
#Cornertotals v2
bundesligatwo_cornertotalsv2 <- tapply(BUNDESLIGATWO$TC, BUNDESLIGATWO[c("HomeTeam", "AwayTeam")],mean)
bundesligatwo_hcototals <- rowSums(bundesligatwo_cornertotalsv2, na.rm = T)
bundesligatwo_acototals <- colSums(bundesligatwo_cornertotalsv2, na.rm = T)
bundesligatwo_cornertotalsv2 <- cbind(bundesligatwo_cornertotalsv2,bundesligatwo_hcototals,bundesligatwo_acototals)
bundesligatwo_totalcorners <- bundesligatwo_hcototals + bundesligatwo_acototals
bundesligatwo_cornertotalsv2 <- cbind(bundesligatwo_cornertotalsv2,bundesligatwo_totalcorners)
bundesligatwo_cornertotalsv2 <- cbind(bundesligatwo_cornertotalsv2,bundesligatwo_games_played)
bundesligatwo_avg_totalcorners <- round((bundesligatwo_totalcorners/ bundesligatwo_games_played), digits = 4)
bundesligatwo_cornertotalsv2[is.na(bundesligatwo_cornertotalsv2)] <- ""
bundesligatwo_cornertotalsv2 <- cbind(bundesligatwo_cornertotalsv2,bundesligatwo_avg_totalcorners)
############################################################################################################
#GS matrix
bundesligatwo_goalscored_h <- tapply(BUNDESLIGATWO$FTHG, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_goalscored_a <- tapply(BUNDESLIGATWO$FTAG, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_goalscored_h[is.na(bundesligatwo_goalscored_h)] <- ""
bundesligatwo_goalscored_a[is.na(bundesligatwo_goalscored_a)] <- ""
for(bundesligatwo_rowhgs in 1:nrow(bundesligatwo_goalscored_h)) {
  for(bundesligatwo_colhgs in 1:ncol(bundesligatwo_goalscored_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowags in 1:nrow(bundesligatwo_goalscored_a)) {
      for(bundesligatwo_colags in 1:ncol(bundesligatwo_goalscored_a)) {
        ifelse(!bundesligatwo_goalscored_a[bundesligatwo_rowags,bundesligatwo_colags]=="",bundesligatwo_goalscored_h[bundesligatwo_rowags,bundesligatwo_colags] <- bundesligatwo_goalscored_a[bundesligatwo_rowags,bundesligatwo_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
bundesligatwo_goalconceded_h <- tapply(BUNDESLIGATWO$FTAG, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_goalconceded_a <- tapply(BUNDESLIGATWO$FTHG, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_goalconceded_h[is.na(bundesligatwo_goalconceded_h)] <- ""
bundesligatwo_goalconceded_a[is.na(bundesligatwo_goalconceded_a)] <- ""
for(bundesligatwo_rowhgc in 1:nrow(bundesligatwo_goalconceded_h)) {
  for(bundesligatwo_colhgc in 1:ncol(bundesligatwo_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowagc in 1:nrow(bundesligatwo_goalconceded_a)) {
      for(bundesligatwo_colagc in 1:ncol(bundesligatwo_goalconceded_a)) {
        ifelse(!bundesligatwo_goalconceded_a[bundesligatwo_rowagc,bundesligatwo_colagc]=="",bundesligatwo_goalconceded_h[bundesligatwo_rowagc,bundesligatwo_colagc] <- bundesligatwo_goalconceded_a[bundesligatwo_rowagc,bundesligatwo_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
bundesligatwo_totalcorners_h <- tapply(BUNDESLIGATWO$TC, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_totalcorners_a <- tapply(BUNDESLIGATWO$TC, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_totalcorners_h[is.na(bundesligatwo_totalcorners_h)] <- ""
bundesligatwo_totalcorners_a[is.na(bundesligatwo_totalcorners_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowTC in 1:nrow(bundesligatwo_totalcorners_h)) {
  for(bundesligatwo_colTC in 1:ncol(bundesligatwo_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowTC in 1:nrow(bundesligatwo_totalcorners_a)) {
      for(bundesligatwo_colTC in 1:ncol(bundesligatwo_totalcorners_a)) {
        ifelse(!bundesligatwo_totalcorners_a[bundesligatwo_rowTC,bundesligatwo_colTC]=="",bundesligatwo_totalcorners_h[bundesligatwo_rowTC,bundesligatwo_colTC] <- bundesligatwo_totalcorners_a[bundesligatwo_rowTC,bundesligatwo_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
bundesligatwo_coawarded_h <- tapply(BUNDESLIGATWO$HCO, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_coawarded_a <- tapply(BUNDESLIGATWO$ACO, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_coawarded_h[is.na(bundesligatwo_coawarded_h)] <- ""
bundesligatwo_coawarded_a[is.na(bundesligatwo_coawarded_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowhco in 1:nrow(bundesligatwo_coawarded_h)) {
  for(bundesligatwo_colhco in 1:ncol(bundesligatwo_coawarded_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowaco in 1:nrow(bundesligatwo_coawarded_a)) {
      for(bundesligatwo_colaco in 1:ncol(bundesligatwo_coawarded_a)) {
        ifelse(!bundesligatwo_coawarded_a[bundesligatwo_rowaco,bundesligatwo_colaco]=="",bundesligatwo_coawarded_h[bundesligatwo_rowaco,bundesligatwo_colaco] <- bundesligatwo_coawarded_a[bundesligatwo_rowaco,bundesligatwo_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
bundesligatwo_cornersconceded_h <- tapply(BUNDESLIGATWO$ACO, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_cornersconceded_a <- tapply(BUNDESLIGATWO$HCO, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_cornersconceded_h[is.na(bundesligatwo_cornersconceded_h)] <- ""
bundesligatwo_cornersconceded_a[is.na(bundesligatwo_cornersconceded_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowhcc in 1:nrow(bundesligatwo_cornersconceded_h)) {
  for(bundesligatwo_colhcc in 1:ncol(bundesligatwo_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowacc in 1:nrow(bundesligatwo_cornersconceded_a)) {
      for(bundesligatwo_colacc in 1:ncol(bundesligatwo_cornersconceded_a)) {
        ifelse(!bundesligatwo_cornersconceded_a[bundesligatwo_rowacc,bundesligatwo_colacc]=="",bundesligatwo_cornersconceded_h[bundesligatwo_rowacc,bundesligatwo_colacc] <- bundesligatwo_cornersconceded_a[bundesligatwo_rowacc,bundesligatwo_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
bundesligatwo_coscform_h <- tapply(BUNDESLIGATWO$COSC, BUNDESLIGATWO[c("HomeTeam", "Date")],median)
bundesligatwo_coscform_a <- tapply(BUNDESLIGATWO$COSC, BUNDESLIGATWO[c("AwayTeam", "Date")],median)
bundesligatwo_coscform_h[is.na(bundesligatwo_coscform_h)] <- ""
bundesligatwo_coscform_a[is.na(bundesligatwo_coscform_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowh_f_cosc in 1:nrow(bundesligatwo_coscform_h)) {
  for(bundesligatwo_colh_f_cosc in 1:ncol(bundesligatwo_coscform_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowa_f_cosc in 1:nrow(bundesligatwo_coscform_a)) {
      for(bundesligatwo_cola_f_cosc in 1:ncol(bundesligatwo_coscform_a)) {
        ifelse(!bundesligatwo_coscform_a[bundesligatwo_rowa_f_cosc,bundesligatwo_cola_f_cosc]=="",bundesligatwo_coscform_h[bundesligatwo_rowa_f_cosc,bundesligatwo_cola_f_cosc] <- bundesligatwo_coscform_a[bundesligatwo_rowa_f_cosc,bundesligatwo_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
bundesligatwo_winmargin_h <- tapply(BUNDESLIGATWO$FTHG - BUNDESLIGATWO$FTAG, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_winmargin_a <- tapply(BUNDESLIGATWO$FTAG - BUNDESLIGATWO$FTHG, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_winmargin_h[is.na(bundesligatwo_winmargin_h)] <- ""
bundesligatwo_winmargin_a[is.na(bundesligatwo_winmargin_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowhwm in 1:nrow(bundesligatwo_winmargin_h)) {
  for(bundesligatwo_colhwm in 1:ncol(bundesligatwo_winmargin_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowawm in 1:nrow(bundesligatwo_winmargin_a)) {
      for(bundesligatwo_colawm in 1:ncol(bundesligatwo_winmargin_a)) {
        ifelse(!bundesligatwo_winmargin_a[bundesligatwo_rowawm,bundesligatwo_colawm]=="",bundesligatwo_winmargin_h[bundesligatwo_rowawm,bundesligatwo_colawm] <- bundesligatwo_winmargin_a[bundesligatwo_rowawm,bundesligatwo_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
bundesligatwo_yellowscored_h <- tapply(BUNDESLIGATWO$HY, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_yellowscored_a <- tapply(BUNDESLIGATWO$AY, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_yellowscored_h[is.na(bundesligatwo_yellowscored_h)] <- ""
bundesligatwo_yellowscored_a[is.na(bundesligatwo_yellowscored_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowhys in 1:nrow(bundesligatwo_yellowscored_h)) {
  for(bundesligatwo_colhys in 1:ncol(bundesligatwo_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_roways in 1:nrow(bundesligatwo_yellowscored_a)) {
      for(bundesligatwo_colays in 1:ncol(bundesligatwo_yellowscored_a)) {
        ifelse(!bundesligatwo_yellowscored_a[bundesligatwo_roways,bundesligatwo_colays]=="",bundesligatwo_yellowscored_h[bundesligatwo_roways,bundesligatwo_colays] <- bundesligatwo_yellowscored_a[bundesligatwo_roways,bundesligatwo_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
bundesligatwo_redscored_h <- tapply(BUNDESLIGATWO$HR, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_redscored_a <- tapply(BUNDESLIGATWO$AR, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_redscored_h[is.na(bundesligatwo_redscored_h)] <- ""
bundesligatwo_redscored_a[is.na(bundesligatwo_redscored_a)] <- ""
for(bundesligatwo_rowhrs in 1:nrow(bundesligatwo_redscored_h)) {
  for(bundesligatwo_colhrs in 1:ncol(bundesligatwo_redscored_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowars in 1:nrow(bundesligatwo_redscored_a)) {
      for(bundesligatwo_colars in 1:ncol(bundesligatwo_redscored_a)) {
        ifelse(!bundesligatwo_redscored_a[bundesligatwo_rowars,bundesligatwo_colars]=="",bundesligatwo_redscored_h[bundesligatwo_rowars,bundesligatwo_colars] <- bundesligatwo_redscored_a[bundesligatwo_rowars,bundesligatwo_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
bundesligatwo_redtotalsv2 <- tapply(BUNDESLIGATWO$TR, BUNDESLIGATWO[c("HomeTeam", "AwayTeam")],mean)
bundesligatwo_hrtotals <- rowSums(bundesligatwo_redtotalsv2, na.rm = T)
bundesligatwo_artotals <- colSums(bundesligatwo_redtotalsv2, na.rm = T)
bundesligatwo_redtotalsv2 <- cbind(bundesligatwo_redtotalsv2,bundesligatwo_hrtotals,bundesligatwo_artotals)
bundesligatwo_totalreds <- bundesligatwo_hrtotals + bundesligatwo_artotals
bundesligatwo_redtotalsv2 <- cbind(bundesligatwo_redtotalsv2,bundesligatwo_totalreds)
bundesligatwo_redtotalsv2 <- cbind(bundesligatwo_redtotalsv2,bundesligatwo_games_played)
bundesligatwo_avg_totalreds <- round((bundesligatwo_totalreds/ bundesligatwo_games_played), digits = 4)
bundesligatwo_redtotalsv2[is.na(bundesligatwo_redtotalsv2)] <- ""
bundesligatwo_redtotalsv2 <- cbind(bundesligatwo_redtotalsv2,bundesligatwo_avg_totalreds)
############################################################################################################################################################
#yellowtotals
bundesligatwo_yellowtotalsv2 <- tapply(BUNDESLIGATWO$TY, BUNDESLIGATWO[c("HomeTeam", "AwayTeam")],mean)
bundesligatwo_hytotals <- rowSums(bundesligatwo_yellowtotalsv2, na.rm = T)
bundesligatwo_aytotals <- colSums(bundesligatwo_yellowtotalsv2, na.rm = T)
bundesligatwo_yellowtotalsv2 <- cbind(bundesligatwo_yellowtotalsv2,bundesligatwo_hytotals,bundesligatwo_aytotals)
bundesligatwo_totalyellows <- bundesligatwo_hytotals + bundesligatwo_aytotals
bundesligatwo_yellowtotalsv2 <- cbind(bundesligatwo_yellowtotalsv2,bundesligatwo_totalyellows)
bundesligatwo_yellowtotalsv2 <- cbind(bundesligatwo_yellowtotalsv2,bundesligatwo_games_played)
bundesligatwo_avg_totalyellows <- round((bundesligatwo_totalyellows/ bundesligatwo_games_played), digits = 4)
bundesligatwo_yellowtotalsv2[is.na(bundesligatwo_yellowtotalsv2)] <- ""
bundesligatwo_yellowtotalsv2 <- cbind(bundesligatwo_yellowtotalsv2,bundesligatwo_avg_totalyellows)
##################################################################################################################################################
#team form
bundesligatwo_form_h <- tapply(BUNDESLIGATWO$FTR, BUNDESLIGATWO[c("HomeTeam", "Date")],median)
bundesligatwo_form_a <- tapply(BUNDESLIGATWO$FTR, BUNDESLIGATWO[c("AwayTeam", "Date")],median)
bundesligatwo_form_h[is.na(bundesligatwo_form_h)] <- ""
bundesligatwo_form_a[is.na(bundesligatwo_form_a)] <- ""
bundesligatwo_form_h <- sub("A","L",bundesligatwo_form_h)
bundesligatwo_form_h <- sub("H","W",bundesligatwo_form_h)
bundesligatwo_form_a <- sub("A","W",bundesligatwo_form_a)
bundesligatwo_form_a <- sub("H","L",bundesligatwo_form_a)
for(bundesligatwo_rowh_f in 1:nrow(bundesligatwo_form_h)) {
  for(bundesligatwo_colh_f in 1:ncol(bundesligatwo_form_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowa_f in 1:nrow(bundesligatwo_form_a)) {
      for(bundesligatwo_cola_f in 1:ncol(bundesligatwo_form_a)) {
        ifelse(!bundesligatwo_form_a[bundesligatwo_rowa_f,bundesligatwo_cola_f]=="",bundesligatwo_form_h[bundesligatwo_rowa_f,bundesligatwo_cola_f] <- bundesligatwo_form_a[bundesligatwo_rowa_f,bundesligatwo_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
bundesligatwo_csform_h <- tapply(BUNDESLIGATWO$CS, BUNDESLIGATWO[c("HomeTeam", "Date")],median)
bundesligatwo_csform_a <- tapply(BUNDESLIGATWO$CS, BUNDESLIGATWO[c("AwayTeam", "Date")],median)
bundesligatwo_csform_h[is.na(bundesligatwo_csform_h)] <- ""
bundesligatwo_csform_a[is.na(bundesligatwo_csform_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowh_f_cs in 1:nrow(bundesligatwo_csform_h)) {
  for(bundesligatwo_colh_f_cs in 1:ncol(bundesligatwo_csform_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowa_f_cs in 1:nrow(bundesligatwo_csform_a)) {
      for(bundesligatwo_cola_f_cs in 1:ncol(bundesligatwo_csform_a)) {
        ifelse(!bundesligatwo_csform_a[bundesligatwo_rowa_f_cs,bundesligatwo_cola_f_cs]=="",bundesligatwo_csform_h[bundesligatwo_rowa_f_cs,bundesligatwo_cola_f_cs] <- bundesligatwo_csform_a[bundesligatwo_rowa_f_cs,bundesligatwo_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
bundesligatwo_totalgoals_h <- tapply(BUNDESLIGATWO$TG, BUNDESLIGATWO[c("HomeTeam", "Date")],mean)
bundesligatwo_totalgoals_a <- tapply(BUNDESLIGATWO$TG, BUNDESLIGATWO[c("AwayTeam", "Date")],mean)
bundesligatwo_totalgoals_h[is.na(bundesligatwo_totalgoals_h)] <- ""
bundesligatwo_totalgoals_a[is.na(bundesligatwo_totalgoals_a)] <- ""
for(bundesligatwo_rowh in 1:nrow(bundesligatwo_totalgoals_h)) {
  for(bundesligatwo_colh in 1:ncol(bundesligatwo_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowa in 1:nrow(bundesligatwo_totalgoals_a)) {
      for(bundesligatwo_cola in 1:ncol(bundesligatwo_totalgoals_a)) {
        ifelse(!bundesligatwo_totalgoals_a[bundesligatwo_rowa,bundesligatwo_cola]=="",bundesligatwo_totalgoals_h[bundesligatwo_rowa,bundesligatwo_cola] <- bundesligatwo_totalgoals_a[bundesligatwo_rowa,bundesligatwo_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#BUNDESLIGATWO
bundesligatwo_un05_home <- c()
bundesligatwo_un05_away <- c()
bundesligatwo_ov05_home <- c()
bundesligatwo_ov05_away <- c()

bundesligatwo_un15_home <- c()
bundesligatwo_un15_away <- c()
bundesligatwo_ov15_home <- c()
bundesligatwo_ov15_away <- c()

bundesligatwo_un25_home <- c()
bundesligatwo_un25_away <- c()
bundesligatwo_ov25_home <- c()
bundesligatwo_ov25_away <- c()

bundesligatwo_un35_home <- c()
bundesligatwo_un35_away <- c()
bundesligatwo_ov35_home <- c()
bundesligatwo_ov35_away <- c()

bundesligatwo_un45_home <- c()
bundesligatwo_un45_away <- c()
bundesligatwo_ov45_home <- c()
bundesligatwo_ov45_away <- c()

bundesligatwo_un55_home <- c()
bundesligatwo_un55_away <- c()
bundesligatwo_ov55_home <- c()
bundesligatwo_ov55_away <- c()

for (i_bundesligatwo_tg in 1:length(bundesligatwo_teams))
{

  bundesligatwo_un05_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG == 0,])
  bundesligatwo_un05_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG == 0,])

  bundesligatwo_ov05_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG > 0,])
  bundesligatwo_ov05_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG > 0,])

  bundesligatwo_un15_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 1,])
  bundesligatwo_un15_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 1,])

  bundesligatwo_ov15_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 2,])
  bundesligatwo_ov15_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 2,])

  bundesligatwo_un25_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 2,])
  bundesligatwo_un25_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 2,])

  bundesligatwo_ov25_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >=3,])
  bundesligatwo_ov25_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >=3,])

  bundesligatwo_un35_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 3,])
  bundesligatwo_un35_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 3,])

  bundesligatwo_ov35_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 4,])
  bundesligatwo_ov35_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 4,])

  bundesligatwo_un45_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 4,])
  bundesligatwo_un45_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 4,])

  bundesligatwo_ov45_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 5,])
  bundesligatwo_ov45_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 5,])

  bundesligatwo_un55_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 5,])
  bundesligatwo_un55_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG <= 5,])

  bundesligatwo_ov55_home[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 6,])
  bundesligatwo_ov55_away[i_bundesligatwo_tg] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_tg] & BUNDESLIGATWO$TG >= 6,])


}

bundesligatwo_un05 <- bundesligatwo_un05_home + bundesligatwo_un05_away
bundesligatwo_ov05 <- bundesligatwo_ov05_home + bundesligatwo_ov05_away

bundesligatwo_un15 <- bundesligatwo_un15_home + bundesligatwo_un15_away
bundesligatwo_ov15 <- bundesligatwo_ov15_home + bundesligatwo_ov15_away

bundesligatwo_un25 <- bundesligatwo_un25_home + bundesligatwo_un25_away
bundesligatwo_ov25 <- bundesligatwo_ov25_home + bundesligatwo_ov25_away

bundesligatwo_un35 <- bundesligatwo_un35_home + bundesligatwo_un35_away
bundesligatwo_ov35 <- bundesligatwo_ov35_home + bundesligatwo_ov35_away

bundesligatwo_un45 <- bundesligatwo_un45_home + bundesligatwo_un45_away
bundesligatwo_ov45 <- bundesligatwo_ov45_home + bundesligatwo_ov45_away

bundesligatwo_un55 <- bundesligatwo_un55_home + bundesligatwo_un55_away
bundesligatwo_ov55 <- bundesligatwo_ov55_home + bundesligatwo_ov55_away

bundesligatwo_ovundata <- cbind(bundesligatwo_teams,bundesligatwo_un05,bundesligatwo_ov05,bundesligatwo_un15,bundesligatwo_ov15,bundesligatwo_un25,bundesligatwo_ov25,bundesligatwo_un35,bundesligatwo_ov35,bundesligatwo_un45,bundesligatwo_ov45,bundesligatwo_un55,bundesligatwo_ov55)
#################################################################################################################################################################
#team against
bundesligatwo_form_team_against_h <- tapply(BUNDESLIGATWO$AwayTeam, BUNDESLIGATWO[c("HomeTeam", "Date")],median)
bundesligatwo_form_team_against_a <- tapply(BUNDESLIGATWO$HomeTeam, BUNDESLIGATWO[c("AwayTeam", "Date")],median)
bundesligatwo_form_team_against_h[is.na(bundesligatwo_form_team_against_h)] <- ""
bundesligatwo_form_team_against_a[is.na(bundesligatwo_form_team_against_a)] <- ""
#BUNDESLIGATWO
for(bundesligatwo_rowh_f_against in 1:nrow(bundesligatwo_form_team_against_h)) {
  for(bundesligatwo_colh_f_against in 1:ncol(bundesligatwo_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(bundesligatwo_rowa_f_against in 1:nrow(bundesligatwo_form_team_against_a)) {
      for(bundesligatwo_cola_f_against in 1:ncol(bundesligatwo_form_team_against_a)) {
        ifelse(!bundesligatwo_form_team_against_a[bundesligatwo_rowa_f_against,bundesligatwo_cola_f_against]=="",bundesligatwo_form_team_against_h[bundesligatwo_rowa_f_against,bundesligatwo_cola_f_against] <- bundesligatwo_form_team_against_a[bundesligatwo_rowa_f_against,bundesligatwo_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#BUNDESLIGATWO
#home goals scored
bundesligatwo_home_gs <- aggregate(BUNDESLIGATWO$FTHG, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_home_gs_avg <- aggregate(BUNDESLIGATWO$FTHG, by = list(BUNDESLIGATWO$HomeTeam),mean)
bundesligatwo_home_scoring <- merge(bundesligatwo_home_gs,bundesligatwo_home_gs_avg, by='Group.1',all = T)
names(bundesligatwo_home_scoring)[names(bundesligatwo_home_scoring) == "x.x"] <- "TFthg"
names(bundesligatwo_home_scoring)[names(bundesligatwo_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
bundesligatwo_away_gs <- aggregate(BUNDESLIGATWO$FTAG, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
bundesligatwo_away_gs_avg <- aggregate(BUNDESLIGATWO$FTAG, by = list(BUNDESLIGATWO$AwayTeam),mean)
bundesligatwo_away_scoring <- merge(bundesligatwo_away_gs,bundesligatwo_away_gs_avg, by='Group.1',all = T)
names(bundesligatwo_away_scoring)[names(bundesligatwo_away_scoring) == "x.x"] <- "TFtag"
names(bundesligatwo_away_scoring)[names(bundesligatwo_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
bundesligatwo_scoring <- merge(bundesligatwo_home_scoring,bundesligatwo_away_scoring,by='Group.1',all = T)
bundesligatwo_scoring$TGS <- bundesligatwo_scoring$TFthg + bundesligatwo_scoring$TFtag

#Home shots on target
bundesligatwo_home_hst <- aggregate(BUNDESLIGATWO$HST, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_ast <- aggregate(BUNDESLIGATWO$AST, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
bundesligatwo_tst <- merge(bundesligatwo_home_hst,bundesligatwo_away_ast, by='Group.1',all = T)
names(bundesligatwo_tst)[names(bundesligatwo_tst) == "x.x"] <- "hst"
names(bundesligatwo_tst)[names(bundesligatwo_tst) == "x.y"] <- "ast"
bundesligatwo_tst$TST <- bundesligatwo_tst$hst + bundesligatwo_tst$ast
#merge goals scored and shots on target
bundesligatwo_scoring_conversion <- merge(bundesligatwo_tst,bundesligatwo_scoring,by='Group.1',all = T)
#add HSC ASC TSC
bundesligatwo_scoring_conversion$HSTC <- percent(bundesligatwo_scoring_conversion$TFthg/bundesligatwo_scoring_conversion$hst, accuracy = 0.01)
bundesligatwo_scoring_conversion$ASTC <- percent(bundesligatwo_scoring_conversion$TFtag/bundesligatwo_scoring_conversion$ast, accuracy = 0.01)
bundesligatwo_scoring_conversion$TSTC <- percent(bundesligatwo_scoring_conversion$TGS/bundesligatwo_scoring_conversion$TST, accuracy = 0.01)
#merge games played
bundesligatwo_scoring_conversion <- cbind(bundesligatwo_scoring_conversion,bundesligatwo_games_played)
#create the second part
#home goals conceded
bundesligatwo_home_gc <- aggregate(BUNDESLIGATWO$FTAG, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_home_gc_avg <- aggregate(BUNDESLIGATWO$FTAG, by = list(BUNDESLIGATWO$HomeTeam),mean)
bundesligatwo_home_conceding <- merge(bundesligatwo_home_gc,bundesligatwo_home_gc_avg, by='Group.1',all = T)
names(bundesligatwo_home_conceding)[names(bundesligatwo_home_conceding) == "x.x"] <- "TFthc"
names(bundesligatwo_home_conceding)[names(bundesligatwo_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
bundesligatwo_away_gc <- aggregate(BUNDESLIGATWO$FTHG, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
bundesligatwo_away_gc_avg <- aggregate(BUNDESLIGATWO$FTHG, by = list(BUNDESLIGATWO$AwayTeam),mean)
bundesligatwo_away_conceding <- merge(bundesligatwo_away_gc,bundesligatwo_away_gc_avg, by='Group.1',all = T)
names(bundesligatwo_away_conceding)[names(bundesligatwo_away_conceding) == "x.x"] <- "TFtac"
names(bundesligatwo_away_conceding)[names(bundesligatwo_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
bundesligatwo_conceding <- merge(bundesligatwo_home_conceding,bundesligatwo_away_conceding,by='Group.1',all = T)
bundesligatwo_conceding$TGC <- bundesligatwo_conceding$TFthc + bundesligatwo_conceding$TFtac
bundesligatwo_home_hst
#Home shots conceded
bundesligatwo_home_hsc <- aggregate(BUNDESLIGATWO$AST, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_asc <- aggregate(BUNDESLIGATWO$HST, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
bundesligatwo_tsc <- merge(bundesligatwo_home_hsc,bundesligatwo_away_asc, by='Group.1',all = T)
names(bundesligatwo_tsc)[names(bundesligatwo_tsc) == "x.x"] <- "hsc"
names(bundesligatwo_tsc)[names(bundesligatwo_tsc) == "x.y"] <- "asc"
bundesligatwo_tsc$TSC <- bundesligatwo_tsc$hsc + bundesligatwo_tsc$asc
#merge goals conceded and shots conceded
bundesligatwo_conceding_conversion <- merge(bundesligatwo_tsc,bundesligatwo_conceding,by='Group.1',all = T)

#add HSC ASC TSC
bundesligatwo_conceding_conversion$HSCC <- percent(bundesligatwo_conceding_conversion$TFthc/bundesligatwo_conceding_conversion$hsc, accuracy = 0.01)
bundesligatwo_conceding_conversion$ASCC <- percent(bundesligatwo_conceding_conversion$TFtac/bundesligatwo_conceding_conversion$asc, accuracy = 0.01)
bundesligatwo_conceding_conversion$TSCC <- percent(bundesligatwo_conceding_conversion$TGC/bundesligatwo_conceding_conversion$TSC, accuracy = 0.01)
bundesligatwo_conceding_conversion$XSTC <- round(bundesligatwo_scoring$TGS/(bundesligatwo_tst$TST - bundesligatwo_scoring$TGS), digits = 2)

#merge the two parts
bundesligatwo_shots_analysis <- merge(bundesligatwo_scoring_conversion,bundesligatwo_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#BUNDESLIGATWO
#home fouls for
bundesligatwo_home_fouls <- aggregate(BUNDESLIGATWO$HF, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_home_fouls_avg <- aggregate(BUNDESLIGATWO$HF, by = list(BUNDESLIGATWO$HomeTeam),mean)
bundesligatwo_home_foulsdata <- merge(bundesligatwo_home_fouls,bundesligatwo_home_fouls_avg, by='Group.1',all = T)
names(bundesligatwo_home_foulsdata)[names(bundesligatwo_home_foulsdata) == "x.x"] <- "THfouls"
names(bundesligatwo_home_foulsdata)[names(bundesligatwo_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
bundesligatwo_away_fouls <- aggregate(BUNDESLIGATWO$HF, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
bundesligatwo_away_fouls_avg <- aggregate(BUNDESLIGATWO$HF, by = list(BUNDESLIGATWO$AwayTeam),mean)
bundesligatwo_away_foulsdata <- merge(bundesligatwo_away_fouls,bundesligatwo_away_fouls_avg, by='Group.1',all = T)
names(bundesligatwo_away_foulsdata)[names(bundesligatwo_away_foulsdata) == "x.x"] <- "TAfouls"
names(bundesligatwo_away_foulsdata)[names(bundesligatwo_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
bundesligatwo_fouls <- merge(bundesligatwo_home_foulsdata,bundesligatwo_away_foulsdata,by='Group.1',all = T)
bundesligatwo_fouls$TotalFouls <- bundesligatwo_fouls$THfouls + bundesligatwo_fouls$TAfouls

#yellow cards
bundesligatwo_home_hyc <- aggregate(BUNDESLIGATWO$HY, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_ayc <- aggregate(BUNDESLIGATWO$AY, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
bundesligatwo_tyc <- merge(bundesligatwo_home_hyc,bundesligatwo_away_ayc, by='Group.1',all = T)
names(bundesligatwo_tyc)[names(bundesligatwo_tyc) == "x.x"] <- "hyc"
names(bundesligatwo_tyc)[names(bundesligatwo_tyc) == "x.y"] <- "ayc"
bundesligatwo_tyc$TotalYellows <- bundesligatwo_tyc$hyc + bundesligatwo_tyc$ayc

#merge fouls for and yellow cards
bundesligatwo_fouls_conversion <- merge(bundesligatwo_tyc,bundesligatwo_fouls,by='Group.1',all = T)
bundesligatwo_fouls_conversion$YcPerfoul <- round((bundesligatwo_fouls_conversion$TotalYellows/bundesligatwo_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
BUNDESLIGATWO$Div <- "BUNDESLIGATWO"
##
###################################################################################################################################################
#poisson cards
bundesligatwo_GP <- nrow(BUNDESLIGATWO)
#Calculate total home goals for each division
bundesligatwo_T_HY <- sum(bundesligatwo_home_hyc$x)
#calculate average home goal
bundesligatwo_avg_HY <- round(bundesligatwo_T_HY /bundesligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
bundesligatwo_T_AY <- sum(bundesligatwo_away_ayc$x)
#calculate average away goal
bundesligatwo_avg_AY <- round(bundesligatwo_T_AY /bundesligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bundesligatwo_home_yas <- round(((bundesligatwo_home_hyc$x/bundesligatwo_home_games))/bundesligatwo_avg_HY, digits = 4)
#calculate away attack strength
bundesligatwo_away_yas <- round(((bundesligatwo_away_ayc$x/bundesligatwo_away_games))/bundesligatwo_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
bundesligatwo_avg_HYC <- round(bundesligatwo_T_AY /bundesligatwo_GP, digits = 4)
#avg away concede
bundesligatwo_avg_AYC <- round(bundesligatwo_T_HY /bundesligatwo_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
bundesligatwo_home_ycc <- aggregate(BUNDESLIGATWO$AY, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_ycc <- aggregate(BUNDESLIGATWO$HY, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
#home defense strength
bundesligatwo_home_yds <- round(((bundesligatwo_home_ycc$x/bundesligatwo_home_games))/bundesligatwo_avg_HYC, digits = 4)
#away defense strength
bundesligatwo_away_yds <- round(((bundesligatwo_away_ycc$x/bundesligatwo_away_games))/bundesligatwo_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_home_poisson_yc <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_HY,bundesligatwo_home_yas,bundesligatwo_home_yds)
#away poisson data
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_away_poisson_yc <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_AY,bundesligatwo_away_yas,bundesligatwo_away_yds)
###
HomeTeam_bundesligatwo_yc <- rep(bundesligatwo_teams, each = length(bundesligatwo_teams))
AwayTeam_bundesligatwo_yc <- rep(bundesligatwo_teams, length(bundesligatwo_teams))
BUNDESLIGATWO_fixtures_yc <- cbind(HomeTeam_bundesligatwo_yc,AwayTeam_bundesligatwo_yc)
BUNDESLIGATWO_fixtures_yc <- as.data.frame(BUNDESLIGATWO_fixtures_yc)
BUNDESLIGATWO_fixtures_yc <- BUNDESLIGATWO_fixtures_yc[!BUNDESLIGATWO_fixtures_yc$HomeTeam_bundesligatwo_yc == BUNDESLIGATWO_fixtures_yc$AwayTeam_bundesligatwo_yc,]
rownames(BUNDESLIGATWO_fixtures_yc) <- NULL
BUNDESLIGATWO_fixtures_yc$Div <- "BUNDESLIGATWO"
BUNDESLIGATWO_fixtures_yc <- BUNDESLIGATWO_fixtures_yc[,c(3,1,2)]

BUNDESLIGATWO_fixtures_yc$avg_HY_bundesligatwo <- bundesligatwo_avg_HY

BUNDESLIGATWO_fixtures_yc$bundesligatwo_homeyas <- rep(bundesligatwo_home_yas,each = length(bundesligatwo_teams)-1)

bundesligatwo_awayyds_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_yds)

bundesligatwo_awayyds_lookup <- as.data.frame(bundesligatwo_awayyds_lookup)

colnames(bundesligatwo_awayyds_lookup) <- c("AwayTeam_bundesligatwo_yc","bundesligatwo_awayyds")


require('RH2')
BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyds <- sqldf("SELECT bundesligatwo_awayyds_lookup.bundesligatwo_awayyds FROM bundesligatwo_awayyds_lookup INNER JOIN BUNDESLIGATWO_fixtures_yc ON bundesligatwo_awayyds_lookup.AwayTeam_bundesligatwo_yc = BUNDESLIGATWO_fixtures_yc.AwayTeam_bundesligatwo_yc")

BUNDESLIGATWO_fixtures_yc$avg_AY_bundesligatwo <- bundesligatwo_avg_AY

bundesligatwo_awayyas_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_yas)

bundesligatwo_awayyas_lookup <- as.data.frame(bundesligatwo_awayyas_lookup)

colnames(bundesligatwo_awayyas_lookup) <- c("AwayTeam_bundesligatwo_yc","bundesligatwo_awayyas")

BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyas <- sqldf("SELECT bundesligatwo_awayyas_lookup.bundesligatwo_awayyas FROM bundesligatwo_awayyas_lookup INNER JOIN BUNDESLIGATWO_fixtures_yc ON bundesligatwo_awayyas_lookup.AwayTeam_bundesligatwo_yc = BUNDESLIGATWO_fixtures_yc.AwayTeam_bundesligatwo_yc")

BUNDESLIGATWO_fixtures_yc$bundesligatwo_homeyds <- rep(bundesligatwo_home_yds,each = length(bundesligatwo_teams)-1)

BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyds <- as.numeric(unlist(BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyds))
#xGH
BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC <- BUNDESLIGATWO_fixtures_yc$avg_HY_bundesligatwo * BUNDESLIGATWO_fixtures_yc$bundesligatwo_homeyas * BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyds
#xGA

BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyas <- as.numeric(unlist(BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyas))

BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC <- BUNDESLIGATWO_fixtures_yc$avg_AY_bundesligatwo * BUNDESLIGATWO_fixtures_yc$bundesligatwo_awayyas * BUNDESLIGATWO_fixtures_yc$bundesligatwo_homeyds

BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_0 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_0 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_1 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_1 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_0 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_2 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_2 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_1 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_2 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_3 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_0 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_1 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_2 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_3 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_3 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_3 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_4 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_0 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_1 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_2 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_3 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_4 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_4 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_4 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_4 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_5 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_0 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_1 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_2 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_3 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_4 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_5 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_5 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_5 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_5 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_5 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_6 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_0 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_1 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_2 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_3 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_4 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_5 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_6 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_6 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_6 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_6 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_6 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_6 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC) * stats::dpois(6,BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC), digits = 4)
#Home win
BUNDESLIGATWO_fixtures_yc$bundesligatwo_H <- (
  BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_5
)

BUNDESLIGATWO_fixtures_yc$bundesligatwo_H <- percent(BUNDESLIGATWO_fixtures_yc$bundesligatwo_H, accuracy = 0.1)

#Draw
BUNDESLIGATWO_fixtures_yc$bundesligatwo_D <- (

  BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_4 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_6
)

BUNDESLIGATWO_fixtures_yc$bundesligatwo_D <- percent(BUNDESLIGATWO_fixtures_yc$bundesligatwo_D, accuracy = 0.1)

#Away

BUNDESLIGATWO_fixtures_yc$bundesligatwo_A <- (
  BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_6
)

BUNDESLIGATWO_fixtures_yc$bundesligatwo_A <- percent(BUNDESLIGATWO_fixtures_yc$bundesligatwo_A, accuracy = 0.1)

#ov25
BUNDESLIGATWO_fixtures_yc$bundesligatwo_ov25 <- (
  BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_4 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_0 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_5 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_5 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_3_6 +
    BUNDESLIGATWO_fixtures_yc$bundesligatwo_4_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_6_6
)
#un25
BUNDESLIGATWO_fixtures_yc$bundesligatwo_un25 <- (
  BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_yc$bundesligatwo_0_2
)
#odds
BUNDESLIGATWO_fixtures_yc$bundesligatwo_ov25_odds <- round((1/BUNDESLIGATWO_fixtures_yc$bundesligatwo_ov25),digits = 2)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_un25_odds <- round((1/BUNDESLIGATWO_fixtures_yc$bundesligatwo_un25),digits = 2)

BUNDESLIGATWO_fixtures_yc$bundesligatwo_ov25_odds
BUNDESLIGATWO_fixtures_yc$bundesligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
BUNDESLIGATWO_fixtures_yc$bundesligatwo_ov25 <- percent(BUNDESLIGATWO_fixtures_yc$bundesligatwo_ov25, accuracy = 0.1)

BUNDESLIGATWO_fixtures_yc$bundesligatwo_un25 <- percent(BUNDESLIGATWO_fixtures_yc$bundesligatwo_un25, accuracy = 0.1)
BUNDESLIGATWO_fixtures_yc$bundesligatwo_pscore <- paste(round(BUNDESLIGATWO_fixtures_yc$bundesligatwo_xHYC,digits = 0),round(BUNDESLIGATWO_fixtures_yc$bundesligatwo_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
bundesligatwo_GP <- nrow(BUNDESLIGATWO)
#Calculate total home corners for each division
bundesligatwo_home_corners <- aggregate(BUNDESLIGATWO$HCO, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_corners <- aggregate(BUNDESLIGATWO$ACO, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
###############################################################################
bundesligatwo_T_HCO <- sum(bundesligatwo_home_corners$x)
#calculate average home corners
bundesligatwo_avg_HCO <- round(bundesligatwo_T_HCO /bundesligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
bundesligatwo_T_ACO <- sum(bundesligatwo_away_corners$x)
#calculate average away goal
bundesligatwo_avg_ACO <- round(bundesligatwo_T_ACO /bundesligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bundesligatwo_home_coas <- round(((bundesligatwo_home_corners$x/bundesligatwo_home_games))/bundesligatwo_avg_HCO, digits = 4)
#calculate away attack strength
bundesligatwo_away_coas <- round(((bundesligatwo_away_corners$x/bundesligatwo_away_games))/bundesligatwo_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
bundesligatwo_avg_HCOC <- round(bundesligatwo_T_ACO /bundesligatwo_GP, digits = 4)
#avg away concede
bundesligatwo_avg_ACOC <- round(bundesligatwo_T_HCO /bundesligatwo_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
bundesligatwo_home_coc <- aggregate(BUNDESLIGATWO$ACO, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_coc <- aggregate(BUNDESLIGATWO$HCO, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)
#home defense strength
bundesligatwo_home_cods <- round(((bundesligatwo_home_coc$x/bundesligatwo_home_games))/bundesligatwo_avg_HCOC, digits = 4)
#away defense strength
bundesligatwo_away_cods <- round(((bundesligatwo_away_coc$x/bundesligatwo_away_games))/bundesligatwo_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_home_poisson_corners <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_HCO,bundesligatwo_home_coas,bundesligatwo_home_cods)
#################################################################################
#away poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_away_poisson_corners <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_ACO,bundesligatwo_away_coas,bundesligatwo_away_cods)

#BUNDESLIGATWO
HomeTeam_bundesligatwo_co <- rep(bundesligatwo_teams, each = length(bundesligatwo_teams))
AwayTeam_bundesligatwo_co <- rep(bundesligatwo_teams, length(bundesligatwo_teams))
BUNDESLIGATWO_fixtures_co <- cbind(HomeTeam_bundesligatwo_co,AwayTeam_bundesligatwo_co)
BUNDESLIGATWO_fixtures_co <- as.data.frame(BUNDESLIGATWO_fixtures_co)
BUNDESLIGATWO_fixtures_co <- BUNDESLIGATWO_fixtures_co[!BUNDESLIGATWO_fixtures_co$HomeTeam_bundesligatwo_co == BUNDESLIGATWO_fixtures_co$AwayTeam_bundesligatwo_co,]
rownames(BUNDESLIGATWO_fixtures_co) <- NULL
BUNDESLIGATWO_fixtures_co$Div <- "BUNDESLIGATWO"
BUNDESLIGATWO_fixtures_co <- BUNDESLIGATWO_fixtures_co[,c(3,1,2)]

BUNDESLIGATWO_fixtures_co$avg_HCO_bundesligatwo <- bundesligatwo_avg_HCO

BUNDESLIGATWO_fixtures_co$bundesligatwo_homecoas <- rep(bundesligatwo_home_coas,each = length(bundesligatwo_teams)-1)

bundesligatwo_awaycods_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_cods)

bundesligatwo_awaycods_lookup <- as.data.frame(bundesligatwo_awaycods_lookup)

colnames(bundesligatwo_awaycods_lookup) <- c("AwayTeam_bundesligatwo_co","bundesligatwo_awaycods")


require('RH2')
BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycods <- sqldf("SELECT bundesligatwo_awaycods_lookup.bundesligatwo_awaycods FROM bundesligatwo_awaycods_lookup INNER JOIN BUNDESLIGATWO_fixtures_co ON bundesligatwo_awaycods_lookup.AwayTeam_bundesligatwo_co = BUNDESLIGATWO_fixtures_co.AwayTeam_bundesligatwo_co")

BUNDESLIGATWO_fixtures_co$avg_ACO_bundesligatwo <- bundesligatwo_avg_ACO

bundesligatwo_awaycoas_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_coas)

bundesligatwo_awaycoas_lookup <- as.data.frame(bundesligatwo_awaycoas_lookup)

colnames(bundesligatwo_awaycoas_lookup) <- c("AwayTeam_bundesligatwo_co","bundesligatwo_awaycoas")

BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycoas <- sqldf("SELECT bundesligatwo_awaycoas_lookup.bundesligatwo_awaycoas FROM bundesligatwo_awaycoas_lookup INNER JOIN BUNDESLIGATWO_fixtures_co ON bundesligatwo_awaycoas_lookup.AwayTeam_bundesligatwo_co = BUNDESLIGATWO_fixtures_co.AwayTeam_bundesligatwo_co")

BUNDESLIGATWO_fixtures_co$bundesligatwo_homecods <- rep(bundesligatwo_home_cods,each = length(bundesligatwo_teams)-1)

BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycods <- as.numeric(unlist(BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycods))
#xGH
BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC <- BUNDESLIGATWO_fixtures_co$avg_HCO_bundesligatwo * BUNDESLIGATWO_fixtures_co$bundesligatwo_homecoas * BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycods
#xGA

BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycoas <- as.numeric(unlist(BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycoas))

BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC <- BUNDESLIGATWO_fixtures_co$avg_ACO_bundesligatwo * BUNDESLIGATWO_fixtures_co$bundesligatwo_awaycoas * BUNDESLIGATWO_fixtures_co$bundesligatwo_homecods

BUNDESLIGATWO_fixtures_co$bundesligatwo_0_0 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_0 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_0_1 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_1 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_0 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_0_2 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_2 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_1 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_2 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_3 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_0 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_1 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_2 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_0_3 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_3 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_3 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_4 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_0 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_1 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_2 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_3 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_0_4 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_4 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_4 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_4 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_5 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_0 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_1 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_2 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_3 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_4 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_0_5 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_5 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_5 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_5 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_5 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_6 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_0 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_1 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_2 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_3 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_4 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_6_5 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_0_6 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_1_6 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_2_6 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_3_6 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_4_6 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
BUNDESLIGATWO_fixtures_co$bundesligatwo_5_6 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC) * stats::dpois(6,BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC), digits = 4)
#Home win
BUNDESLIGATWO_fixtures_co$bundesligatwo_H <- (
  BUNDESLIGATWO_fixtures_co$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_5_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_6_5
)

BUNDESLIGATWO_fixtures_co$bundesligatwo_H <- percent(BUNDESLIGATWO_fixtures_co$bundesligatwo_H, accuracy = 0.1)

#Draw
BUNDESLIGATWO_fixtures_co$bundesligatwo_D <- (

  BUNDESLIGATWO_fixtures_co$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_4 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_6
)

BUNDESLIGATWO_fixtures_co$bundesligatwo_D <- percent(BUNDESLIGATWO_fixtures_co$bundesligatwo_D, accuracy = 0.1)

#Away

BUNDESLIGATWO_fixtures_co$bundesligatwo_A <- (
  BUNDESLIGATWO_fixtures_co$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_5_6
)

BUNDESLIGATWO_fixtures_co$bundesligatwo_A <- percent(BUNDESLIGATWO_fixtures_co$bundesligatwo_A, accuracy = 0.1)

#ov25
BUNDESLIGATWO_fixtures_co$bundesligatwo_ov25 <- (
  BUNDESLIGATWO_fixtures_co$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_4 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_0 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_5 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_5 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_3_6 +
    BUNDESLIGATWO_fixtures_co$bundesligatwo_4_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures_co$bundesligatwo_6_6
)
#un25
BUNDESLIGATWO_fixtures_co$bundesligatwo_un25 <- (
  BUNDESLIGATWO_fixtures_co$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_co$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_co$bundesligatwo_0_2
)
#odds
BUNDESLIGATWO_fixtures_co$bundesligatwo_ov25_odds <- round((1/BUNDESLIGATWO_fixtures_co$bundesligatwo_ov25),digits = 2)
BUNDESLIGATWO_fixtures_co$bundesligatwo_un25_odds <- round((1/BUNDESLIGATWO_fixtures_co$bundesligatwo_un25),digits = 2)

BUNDESLIGATWO_fixtures_co$bundesligatwo_ov25_odds
BUNDESLIGATWO_fixtures_co$bundesligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
BUNDESLIGATWO_fixtures_co$bundesligatwo_ov25 <- percent(BUNDESLIGATWO_fixtures_co$bundesligatwo_ov25, accuracy = 0.1)

BUNDESLIGATWO_fixtures_co$bundesligatwo_un25 <- percent(BUNDESLIGATWO_fixtures_co$bundesligatwo_un25, accuracy = 0.1)
BUNDESLIGATWO_fixtures_co$bundesligatwo_pscore <- paste(round(BUNDESLIGATWO_fixtures_co$bundesligatwo_xHCOC,digits = 0),round(BUNDESLIGATWO_fixtures_co$bundesligatwo_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
bundesligatwo_GP <- nrow(BUNDESLIGATWO)
#Calculate total home goals for each division
bundesligatwo_T_HF <- sum(bundesligatwo_home_fouls$x)
#calculate average home goal
bundesligatwo_avg_HF <- round(bundesligatwo_T_HF /bundesligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
bundesligatwo_T_AF <- sum(bundesligatwo_away_fouls$x)
#calculate average away goal
bundesligatwo_avg_AF <- round(bundesligatwo_T_AF /bundesligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bundesligatwo_home_fas <- round(((bundesligatwo_home_fouls$x/bundesligatwo_home_games))/bundesligatwo_avg_HF, digits = 4)
#calculate away attack strength
bundesligatwo_away_fas <- round(((bundesligatwo_away_fouls$x/bundesligatwo_away_games))/bundesligatwo_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
bundesligatwo_avg_HFC <- round(bundesligatwo_T_AF /bundesligatwo_GP, digits = 4)
#avg away concede
bundesligatwo_avg_AFC <- round(bundesligatwo_T_HF /bundesligatwo_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
bundesligatwo_home_fcc <- aggregate(BUNDESLIGATWO$AF, by = list(BUNDESLIGATWO$HomeTeam), FUN = sum)
bundesligatwo_away_fcc <- aggregate(BUNDESLIGATWO$HF, by = list(BUNDESLIGATWO$AwayTeam), FUN = sum)

#home defense strength
bundesligatwo_home_fds <- round(((bundesligatwo_home_fcc$x/bundesligatwo_home_games))/bundesligatwo_avg_HFC, digits = 4)

#away defense strength
bundesligatwo_away_fds <- round(((bundesligatwo_away_fcc$x/bundesligatwo_away_games))/bundesligatwo_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_home_poisson_fo <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_HF,bundesligatwo_home_fas,bundesligatwo_home_fds)

#################################################################################
#away poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_away_poisson_fo <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_AF,bundesligatwo_away_fas,bundesligatwo_away_fds)

#BUNDESLIGATWO
HomeTeam_bundesligatwo_fo <- rep(bundesligatwo_teams, each = length(bundesligatwo_teams))
AwayTeam_bundesligatwo_fo <- rep(bundesligatwo_teams, length(bundesligatwo_teams))
BUNDESLIGATWO_fixtures_fo <- cbind(HomeTeam_bundesligatwo_fo,AwayTeam_bundesligatwo_fo)
BUNDESLIGATWO_fixtures_fo <- as.data.frame(BUNDESLIGATWO_fixtures_fo)
BUNDESLIGATWO_fixtures_fo <- BUNDESLIGATWO_fixtures_fo[!BUNDESLIGATWO_fixtures_fo$HomeTeam_bundesligatwo_fo == BUNDESLIGATWO_fixtures_fo$AwayTeam_bundesligatwo_fo,]
rownames(BUNDESLIGATWO_fixtures_fo) <- NULL
BUNDESLIGATWO_fixtures_fo$Div <- "BUNDESLIGATWO"
BUNDESLIGATWO_fixtures_fo <- BUNDESLIGATWO_fixtures_fo[,c(3,1,2)]

BUNDESLIGATWO_fixtures_fo$avg_HF_bundesligatwo <- bundesligatwo_avg_HF

BUNDESLIGATWO_fixtures_fo$bundesligatwo_homefas <- rep(bundesligatwo_home_fas,each = length(bundesligatwo_teams)-1)

bundesligatwo_awayfds_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_fds)

bundesligatwo_awayfds_lookup <- as.data.frame(bundesligatwo_awayfds_lookup)

colnames(bundesligatwo_awayfds_lookup) <- c("AwayTeam_bundesligatwo_fo","bundesligatwo_awayfds")


require('RH2')
BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfds <- sqldf("SELECT bundesligatwo_awayfds_lookup.bundesligatwo_awayfds FROM bundesligatwo_awayfds_lookup INNER JOIN BUNDESLIGATWO_fixtures_fo ON bundesligatwo_awayfds_lookup.AwayTeam_bundesligatwo_fo = BUNDESLIGATWO_fixtures_fo.AwayTeam_bundesligatwo_fo")

BUNDESLIGATWO_fixtures_fo$avg_AF_bundesligatwo <- bundesligatwo_avg_AF

bundesligatwo_awayfas_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_fas)

bundesligatwo_awayfas_lookup <- as.data.frame(bundesligatwo_awayfas_lookup)

colnames(bundesligatwo_awayfas_lookup) <- c("AwayTeam_bundesligatwo_fo","bundesligatwo_awayfas")

BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfas <- sqldf("SELECT bundesligatwo_awayfas_lookup.bundesligatwo_awayfas FROM bundesligatwo_awayfas_lookup INNER JOIN BUNDESLIGATWO_fixtures_fo ON bundesligatwo_awayfas_lookup.AwayTeam_bundesligatwo_fo = BUNDESLIGATWO_fixtures_fo.AwayTeam_bundesligatwo_fo")

BUNDESLIGATWO_fixtures_fo$bundesligatwo_homefds <- rep(bundesligatwo_home_fds,each = length(bundesligatwo_teams)-1)

BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfds <- as.numeric(unlist(BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfds))
#xGH
BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF <- BUNDESLIGATWO_fixtures_fo$avg_HF_bundesligatwo * BUNDESLIGATWO_fixtures_fo$bundesligatwo_homefas * BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfds
#xGA

BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfas <- as.numeric(unlist(BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfas))

BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF <- BUNDESLIGATWO_fixtures_fo$avg_AF_bundesligatwo * BUNDESLIGATWO_fixtures_fo$bundesligatwo_awayfas * BUNDESLIGATWO_fixtures_fo$bundesligatwo_homefds

BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_0 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_0 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_1 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_1 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_0 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_2 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_2 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_1 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_2 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_3 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_0 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_1 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_2 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_3 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_3 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_3 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_4 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_0 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_1 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_2 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_3 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_4 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_4 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_4 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_4 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_5 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_0 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_1 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_2 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_3 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_4 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_5 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_5 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_5 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_5 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_5 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_6 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_0 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_1 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_2 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_3 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_4 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_5 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_6 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_6 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_6 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_6 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_6 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_6 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF) * stats::dpois(6,BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF), digits = 4)
#Home win
BUNDESLIGATWO_fixtures_fo$bundesligatwo_H <- (
  BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_5
)

BUNDESLIGATWO_fixtures_fo$bundesligatwo_H <- percent(BUNDESLIGATWO_fixtures_fo$bundesligatwo_H, accuracy = 0.1)

#Draw
BUNDESLIGATWO_fixtures_fo$bundesligatwo_D <- (

  BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_4 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_6
)

BUNDESLIGATWO_fixtures_fo$bundesligatwo_D <- percent(BUNDESLIGATWO_fixtures_fo$bundesligatwo_D, accuracy = 0.1)

#Away

BUNDESLIGATWO_fixtures_fo$bundesligatwo_A <- (
  BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_6
)

BUNDESLIGATWO_fixtures_fo$bundesligatwo_A <- percent(BUNDESLIGATWO_fixtures_fo$bundesligatwo_A, accuracy = 0.1)

#ov25
BUNDESLIGATWO_fixtures_fo$bundesligatwo_ov25 <- (
  BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_4 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_0 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_5 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_5 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_3_6 +
    BUNDESLIGATWO_fixtures_fo$bundesligatwo_4_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_6_6
)
#un25
BUNDESLIGATWO_fixtures_fo$bundesligatwo_un25 <- (
  BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_fo$bundesligatwo_0_2
)
#odds
BUNDESLIGATWO_fixtures_fo$bundesligatwo_ov25_odds <- round((1/BUNDESLIGATWO_fixtures_fo$bundesligatwo_ov25),digits = 2)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_un25_odds <- round((1/BUNDESLIGATWO_fixtures_fo$bundesligatwo_un25),digits = 2)

BUNDESLIGATWO_fixtures_fo$bundesligatwo_ov25_odds
BUNDESLIGATWO_fixtures_fo$bundesligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
BUNDESLIGATWO_fixtures_fo$bundesligatwo_ov25 <- percent(BUNDESLIGATWO_fixtures_fo$bundesligatwo_ov25, accuracy = 0.1)

BUNDESLIGATWO_fixtures_fo$bundesligatwo_un25 <- percent(BUNDESLIGATWO_fixtures_fo$bundesligatwo_un25, accuracy = 0.1)
BUNDESLIGATWO_fixtures_fo$bundesligatwo_psfore <- paste(round(BUNDESLIGATWO_fixtures_fo$bundesligatwo_xHF,digits = 0),round(BUNDESLIGATWO_fixtures_fo$bundesligatwo_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
bundesligatwo_GP <- nrow(BUNDESLIGATWO)

#Calculate total home goals for each division
bundesligatwo_T_HST <- sum(bundesligatwo_home_hst$x)
#calculate average home goal

bundesligatwo_avg_HST <- round(bundesligatwo_T_HST /bundesligatwo_GP, digits = 4)

############################################################
#Calculate total away goals for each division
bundesligatwo_T_AST <- sum(bundesligatwo_away_ast$x)
#calculate average away goal
bundesligatwo_avg_AST <- round(bundesligatwo_T_AST /bundesligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bundesligatwo_home_sotas <- round(((bundesligatwo_home_hst$x/bundesligatwo_home_games))/bundesligatwo_avg_HST, digits = 4)
#calculate away attack strength
bundesligatwo_away_sotas <- round(((bundesligatwo_away_ast$x/bundesligatwo_away_games))/bundesligatwo_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
bundesligatwo_avg_HSC <- round(bundesligatwo_T_AST /bundesligatwo_GP, digits = 4)

#avg away concede
bundesligatwo_avg_ASC <- round(bundesligatwo_T_HST /bundesligatwo_GP, digits = 4)
#home defense strength
bundesligatwo_home_sods <- round(((bundesligatwo_home_hsc$x/bundesligatwo_home_games))/bundesligatwo_avg_HSC, digits = 4)

#away defense strength
bundesligatwo_away_sods <- round(((bundesligatwo_away_ast$x/bundesligatwo_away_games))/bundesligatwo_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_home_poisson_sot <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_HST,bundesligatwo_home_sotas,bundesligatwo_home_sods)

#################################################################################
#away poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_away_poisson_sot <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_AST,bundesligatwo_away_sotas,bundesligatwo_away_sods)

#BUNDESLIGATWO
HomeTeam_bundesligatwo_sot <- rep(bundesligatwo_teams, each = length(bundesligatwo_teams))
AwayTeam_bundesligatwo_sot <- rep(bundesligatwo_teams, length(bundesligatwo_teams))
BUNDESLIGATWO_fixtures_sot <- cbind(HomeTeam_bundesligatwo_sot,AwayTeam_bundesligatwo_sot)
BUNDESLIGATWO_fixtures_sot <- as.data.frame(BUNDESLIGATWO_fixtures_sot)
BUNDESLIGATWO_fixtures_sot <- BUNDESLIGATWO_fixtures_sot[!BUNDESLIGATWO_fixtures_sot$HomeTeam_bundesligatwo_sot == BUNDESLIGATWO_fixtures_sot$AwayTeam_bundesligatwo_sot,]
rownames(BUNDESLIGATWO_fixtures_sot) <- NULL
BUNDESLIGATWO_fixtures_sot$Div <- "BUNDESLIGATWO"
BUNDESLIGATWO_fixtures_sot <- BUNDESLIGATWO_fixtures_sot[,c(3,1,2)]

BUNDESLIGATWO_fixtures_sot$avg_HST_bundesligatwo <- bundesligatwo_avg_HST

BUNDESLIGATWO_fixtures_sot$bundesligatwo_homesotas <- rep(bundesligatwo_home_sotas,each = length(bundesligatwo_teams)-1)

bundesligatwo_awaysods_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_sods)

bundesligatwo_awaysods_lookup <- as.data.frame(bundesligatwo_awaysods_lookup)

colnames(bundesligatwo_awaysods_lookup) <- c("AwayTeam_bundesligatwo_sot","bundesligatwo_awaysods")


require('RH2')
BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysods <- sqldf("SELECT bundesligatwo_awaysods_lookup.bundesligatwo_awaysods FROM bundesligatwo_awaysods_lookup INNER JOIN BUNDESLIGATWO_fixtures_sot ON bundesligatwo_awaysods_lookup.AwayTeam_bundesligatwo_sot = BUNDESLIGATWO_fixtures_sot.AwayTeam_bundesligatwo_sot")

BUNDESLIGATWO_fixtures_sot$avg_AST_bundesligatwo <- bundesligatwo_avg_AST

bundesligatwo_awaysotas_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_sotas)

bundesligatwo_awaysotas_lookup <- as.data.frame(bundesligatwo_awaysotas_lookup)

colnames(bundesligatwo_awaysotas_lookup) <- c("AwayTeam_bundesligatwo_sot","bundesligatwo_awaysotas")

BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysotas <- sqldf("SELECT bundesligatwo_awaysotas_lookup.bundesligatwo_awaysotas FROM bundesligatwo_awaysotas_lookup INNER JOIN BUNDESLIGATWO_fixtures_sot ON bundesligatwo_awaysotas_lookup.AwayTeam_bundesligatwo_sot = BUNDESLIGATWO_fixtures_sot.AwayTeam_bundesligatwo_sot")

BUNDESLIGATWO_fixtures_sot$bundesligatwo_homesods <- rep(bundesligatwo_home_sods,each = length(bundesligatwo_teams)-1)

BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysods <- as.numeric(unlist(BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysods))
#xGH
BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST <- BUNDESLIGATWO_fixtures_sot$avg_HST_bundesligatwo * BUNDESLIGATWO_fixtures_sot$bundesligatwo_homesotas * BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysods
#xGA

BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysotas <- as.numeric(unlist(BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysotas))

BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST <- BUNDESLIGATWO_fixtures_sot$avg_AST_bundesligatwo * BUNDESLIGATWO_fixtures_sot$bundesligatwo_awaysotas * BUNDESLIGATWO_fixtures_sot$bundesligatwo_homesods

BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_0 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_0 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_1 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_1 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_0 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_2 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_2 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_1 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_2 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_3 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_0 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_1 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_2 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_3 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_3 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_3 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_4 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_0 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_1 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_2 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_3 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_4 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_4 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_4 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_4 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_5 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_0 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_1 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_2 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_3 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_4 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_5 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_5 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_5 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_5 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_5 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_6 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_0 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_1 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_2 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_3 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_4 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_5 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_6 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_6 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_6 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_6 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_6 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_6 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST) * stats::dpois(6,BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST), digits = 4)
#Home win
BUNDESLIGATWO_fixtures_sot$bundesligatwo_H <- (
  BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_5
)

BUNDESLIGATWO_fixtures_sot$bundesligatwo_H <- percent(BUNDESLIGATWO_fixtures_sot$bundesligatwo_H, accuracy = 0.1)

#Draw
BUNDESLIGATWO_fixtures_sot$bundesligatwo_D <- (

  BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_4 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_6
)

BUNDESLIGATWO_fixtures_sot$bundesligatwo_D <- percent(BUNDESLIGATWO_fixtures_sot$bundesligatwo_D, accuracy = 0.1)

#Away

BUNDESLIGATWO_fixtures_sot$bundesligatwo_A <- (
  BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_6
)

BUNDESLIGATWO_fixtures_sot$bundesligatwo_A <- percent(BUNDESLIGATWO_fixtures_sot$bundesligatwo_A, accuracy = 0.1)

#ov25
BUNDESLIGATWO_fixtures_sot$bundesligatwo_ov25 <- (
  BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_4 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_0 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_5 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_5 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_3_6 +
    BUNDESLIGATWO_fixtures_sot$bundesligatwo_4_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_6_6
)
#un25
BUNDESLIGATWO_fixtures_sot$bundesligatwo_un25 <- (
  BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures_sot$bundesligatwo_0_2
)
#odds
BUNDESLIGATWO_fixtures_sot$bundesligatwo_ov25_odds <- round((1/BUNDESLIGATWO_fixtures_sot$bundesligatwo_ov25),digits = 2)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_un25_odds <- round((1/BUNDESLIGATWO_fixtures_sot$bundesligatwo_un25),digits = 2)

BUNDESLIGATWO_fixtures_sot$bundesligatwo_ov25_odds
BUNDESLIGATWO_fixtures_sot$bundesligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
BUNDESLIGATWO_fixtures_sot$bundesligatwo_ov25 <- percent(BUNDESLIGATWO_fixtures_sot$bundesligatwo_ov25, accuracy = 0.1)

BUNDESLIGATWO_fixtures_sot$bundesligatwo_un25 <- percent(BUNDESLIGATWO_fixtures_sot$bundesligatwo_un25, accuracy = 0.1)
BUNDESLIGATWO_fixtures_sot$bundesligatwo_pssotre <- paste(round(BUNDESLIGATWO_fixtures_sot$bundesligatwo_xHST,digits = 0),round(BUNDESLIGATWO_fixtures_sot$bundesligatwo_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
bundesligatwo_home_wins <- c()
bundesligatwo_away_wins <- c()
bundesligatwo_home_draws <- c()
bundesligatwo_away_draws <- c()
bundesligatwo_home_loss <- c()
bundesligatwo_away_loss <- c()



for (i_bundesligatwo_wins in 1:length(bundesligatwo_teams))
{

  bundesligatwo_home_wins[i_bundesligatwo_wins] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_wins] & BUNDESLIGATWO$FTR == "H",])
  bundesligatwo_away_wins[i_bundesligatwo_wins] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_wins] & BUNDESLIGATWO$FTR == "A",])
  bundesligatwo_home_draws[i_bundesligatwo_wins] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_wins] & BUNDESLIGATWO$FTR == "D",])
  bundesligatwo_away_draws[i_bundesligatwo_wins] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_wins] & BUNDESLIGATWO$FTR == "D",])
  bundesligatwo_home_loss[i_bundesligatwo_wins] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$HomeTeam == bundesligatwo_teams[i_bundesligatwo_wins] & BUNDESLIGATWO$FTR == "A",])
  bundesligatwo_away_loss[i_bundesligatwo_wins] <- nrow(BUNDESLIGATWO[BUNDESLIGATWO$AwayTeam == bundesligatwo_teams[i_bundesligatwo_wins] & BUNDESLIGATWO$FTR == "H",])

}

bundesligatwo_total_wins <- bundesligatwo_home_wins + bundesligatwo_away_wins
bundesligatwo_total_draws <- bundesligatwo_home_draws + bundesligatwo_away_draws
bundesligatwo_total_loss <- bundesligatwo_home_loss + bundesligatwo_away_loss

bundesligatwo_league_table <- cbind(bundesligatwo_teams,bundesligatwo_games_played,bundesligatwo_total_wins,bundesligatwo_total_draws,bundesligatwo_total_loss)
bundesligatwo_GS <- bundesligatwo_scoring$TGS
bundesligatwo_GC <-bundesligatwo_conceding$TGC
bundesligatwo_GD <- bundesligatwo_scoring$TGS - bundesligatwo_conceding$TGC
bundesligatwo_PTS <- (bundesligatwo_total_wins*3) + (bundesligatwo_total_draws*1)
bundesligatwo_league_table <- cbind(bundesligatwo_league_table,bundesligatwo_GS,bundesligatwo_GC,bundesligatwo_GD,bundesligatwo_PTS)
bundesligatwo_league_table <- as.data.frame(bundesligatwo_league_table)
#rename the columns
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_teams"] <- "Team"
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_games_played"] <- "P"
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_total_wins"] <- "W"
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_total_draws"] <- "D"
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_total_loss"] <- "L"
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_GS"] <- "F"
names(bundesligatwo_league_table)[names(bundesligatwo_league_table) == "bundesligatwo_GC"] <- "A"
points_bundesligatwo <- bundesligatwo_league_table[order(as.numeric(bundesligatwo_league_table$bundesligatwo_PTS), decreasing = TRUE),]
points_bundesligatwo$bundesligatwo_rank <- 1:length(bundesligatwo_teams)
row.names(points_bundesligatwo) <- points_bundesligatwo$bundesligatwo_rank
#create final_bundesligatwo_hf_against with team ranks in brackets
for(bundesligatwo_rowhrank in 1:nrow(bundesligatwo_form_team_against_h)) {
  for(bundesligatwo_colhrank in 1:ncol(bundesligatwo_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!bundesligatwo_form_team_against_h[bundesligatwo_rowhrank,bundesligatwo_colhrank]=="",bundesligatwo_form_team_against_h[bundesligatwo_rowhrank,bundesligatwo_colhrank] <- paste(bundesligatwo_form_team_against_h[bundesligatwo_rowhrank,bundesligatwo_colhrank],"(",points_bundesligatwo$bundesligatwo_rank[points_bundesligatwo$Team ==bundesligatwo_form_team_against_h[bundesligatwo_rowhrank,bundesligatwo_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
bundesligatwo_GP <- nrow(BUNDESLIGATWO)

#Calculate total home goals for each division
bundesligatwo_T_HG <- sum(bundesligatwo_home_gs$x)

#calculate average home goal
bundesligatwo_avg_HG <- round(bundesligatwo_T_HG /bundesligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
bundesligatwo_T_AG <- sum(bundesligatwo_away_gs$x)
#calculate average away goal
bundesligatwo_avg_AG <- round(bundesligatwo_T_AG /bundesligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bundesligatwo_home_as <- round(((bundesligatwo_home_gs$x/bundesligatwo_home_games))/bundesligatwo_avg_HG, digits = 4)
#calculate away attack strength
bundesligatwo_away_as <- round(((bundesligatwo_away_gs$x/bundesligatwo_away_games))/bundesligatwo_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
bundesligatwo_avg_HC <- round(bundesligatwo_T_AG /bundesligatwo_GP, digits = 4)
#avg away concede
bundesligatwo_avg_AC <- round(bundesligatwo_T_HG /bundesligatwo_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
bundesligatwo_home_ds <- round(((bundesligatwo_home_gc$x/bundesligatwo_home_games))/bundesligatwo_avg_HC, digits = 4)
#away defense strength
bundesligatwo_away_ds <- round(((bundesligatwo_away_gc$x/bundesligatwo_away_games))/bundesligatwo_avg_AC, digits = 4)
#############################################################################
#home poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_home_poisson <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_HG,bundesligatwo_home_as,bundesligatwo_home_ds)
#################################################################################
#away poisson data
#bundesligatwo
bundesligatwo_division <- c()
bundesligatwo_division[1:length(bundesligatwo_teams)] <- "BUNDESLIGATWO"
bundesligatwo_away_poisson <- cbind(bundesligatwo_division,bundesligatwo_teams,bundesligatwo_avg_AG,bundesligatwo_away_as,bundesligatwo_away_ds)

#BUNDESLIGATWO
HomeTeam_bundesligatwo <- rep(bundesligatwo_teams, each = length(bundesligatwo_teams))
AwayTeam_bundesligatwo <- rep(bundesligatwo_teams, length(bundesligatwo_teams))
BUNDESLIGATWO_fixtures <- cbind(HomeTeam_bundesligatwo,AwayTeam_bundesligatwo)
BUNDESLIGATWO_fixtures <- as.data.frame(BUNDESLIGATWO_fixtures)
BUNDESLIGATWO_fixtures <- BUNDESLIGATWO_fixtures[!BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo == BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo,]
rownames(BUNDESLIGATWO_fixtures) <- NULL
BUNDESLIGATWO_fixtures$Div <- "BUNDESLIGATWO"
BUNDESLIGATWO_fixtures <- BUNDESLIGATWO_fixtures[,c(3,1,2)]

BUNDESLIGATWO_fixtures$avg_HG_bundesligatwo <- bundesligatwo_avg_HG

BUNDESLIGATWO_fixtures$bundesligatwo_homeas <- rep(bundesligatwo_home_as,each = length(bundesligatwo_teams)-1)

bundesligatwo_awayds_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_ds)

bundesligatwo_awayds_lookup <- as.data.frame(bundesligatwo_awayds_lookup)

colnames(bundesligatwo_awayds_lookup) <- c("AwayTeam_bundesligatwo","bundesligatwo_awayds")


require('RH2')
BUNDESLIGATWO_fixtures$bundesligatwo_awayds <- sqldf("SELECT bundesligatwo_awayds_lookup.bundesligatwo_awayds FROM bundesligatwo_awayds_lookup INNER JOIN BUNDESLIGATWO_fixtures ON bundesligatwo_awayds_lookup.AwayTeam_bundesligatwo = BUNDESLIGATWO_fixtures.AwayTeam_bundesligatwo")

BUNDESLIGATWO_fixtures$avg_AG_bundesligatwo <- bundesligatwo_avg_AG

bundesligatwo_awayas_lookup <- cbind(bundesligatwo_teams,bundesligatwo_away_as)

bundesligatwo_awayas_lookup <- as.data.frame(bundesligatwo_awayas_lookup)

colnames(bundesligatwo_awayas_lookup) <- c("AwayTeam_bundesligatwo","bundesligatwo_awayas")


BUNDESLIGATWO_fixtures$bundesligatwo_awayas <- sqldf("SELECT bundesligatwo_awayas_lookup.bundesligatwo_awayas FROM bundesligatwo_awayas_lookup INNER JOIN BUNDESLIGATWO_fixtures ON bundesligatwo_awayas_lookup.AwayTeam_bundesligatwo = BUNDESLIGATWO_fixtures.AwayTeam_bundesligatwo")

BUNDESLIGATWO_fixtures$bundesligatwo_homeds <- rep(bundesligatwo_home_ds,each = length(bundesligatwo_teams)-1)

BUNDESLIGATWO_fixtures$bundesligatwo_awayds <- as.numeric(unlist(BUNDESLIGATWO_fixtures$bundesligatwo_awayds))
#xGH
BUNDESLIGATWO_fixtures$bundesligatwo_xGH <- BUNDESLIGATWO_fixtures$avg_HG_bundesligatwo * BUNDESLIGATWO_fixtures$bundesligatwo_homeas * BUNDESLIGATWO_fixtures$bundesligatwo_awayds

#xGA

BUNDESLIGATWO_fixtures$bundesligatwo_awayas <- as.numeric(unlist(BUNDESLIGATWO_fixtures$bundesligatwo_awayas))

BUNDESLIGATWO_fixtures$bundesligatwo_xGA <- BUNDESLIGATWO_fixtures$avg_AG_bundesligatwo * BUNDESLIGATWO_fixtures$bundesligatwo_awayas * BUNDESLIGATWO_fixtures$bundesligatwo_homeds

BUNDESLIGATWO_fixtures$bundesligatwo_0_0 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_0 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_0_1 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_1 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_0 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_0_2 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_2 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_1 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_2 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_3 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_0 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_1 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_2 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_0_3 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_3 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_3 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_4 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_0 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_1 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_2 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_3 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_0_4 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_4 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_4 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_4 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_5 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_0 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_1 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_2 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_3 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_4 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_0_5 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_5 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_5 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_5 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_5 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_6 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_0 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_1 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_2 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_3 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_4 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_6_5 <- round(stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_0_6 <- round(stats::dpois(0,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_1_6 <- round(stats::dpois(1,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_2_6 <- round(stats::dpois(2,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_3_6 <- round(stats::dpois(3,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_4_6 <- round(stats::dpois(4,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
BUNDESLIGATWO_fixtures$bundesligatwo_5_6 <- round(stats::dpois(5,BUNDESLIGATWO_fixtures$bundesligatwo_xGH) * stats::dpois(6,BUNDESLIGATWO_fixtures$bundesligatwo_xGA), digits = 4)
#Home win
BUNDESLIGATWO_fixtures$bundesligatwo_H <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_0 + BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5
)

BUNDESLIGATWO_fixtures$bundesligatwo_H <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_H, accuracy = 0.1)

#Draw
BUNDESLIGATWO_fixtures$bundesligatwo_D <- (

  BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures$bundesligatwo_4_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6
)

BUNDESLIGATWO_fixtures$bundesligatwo_D <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_D, accuracy = 0.1)

#Away

BUNDESLIGATWO_fixtures$bundesligatwo_A <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6
)

BUNDESLIGATWO_fixtures$bundesligatwo_A <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_A, accuracy = 0.1)

#ov25
BUNDESLIGATWO_fixtures$bundesligatwo_ov25 <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 + BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures$bundesligatwo_0_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_6 + BUNDESLIGATWO_fixtures$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6
)
#un25
BUNDESLIGATWO_fixtures$bundesligatwo_un25 <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2
)
#odds
BUNDESLIGATWO_fixtures$bundesligatwo_ov25_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_ov25),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_un25_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_un25),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_ov25_odds
BUNDESLIGATWO_fixtures$bundesligatwo_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
BUNDESLIGATWO_fixtures$bundesligatwo_BTTSY <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_2 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 + BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures$bundesligatwo_4_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_6 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6
)
#BTTSN
BUNDESLIGATWO_fixtures$bundesligatwo_BTTSN <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_0_6
)

BUNDESLIGATWO_fixtures$bundesligatwo_BTTSY_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_BTTSY),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_BTTSN_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_BTTSN),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_BTTSY <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_BTTSY, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_BTTSN <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_BTTSN, accuracy = 0.1)
#odds
BUNDESLIGATWO_fixtures$bundesligatwo_BTTSY_odds
BUNDESLIGATWO_fixtures$bundesligatwo_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_H <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6
)
#AH_0_A
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_A <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 +BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6
)

#odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_H_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_H),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_A_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_A),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_H_odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_A_odds
#percentages
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_H <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_H, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_A <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_H <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5
)
#AH_n075_A
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_A <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 +BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6
)

#odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_H_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_H),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_A_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_A),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_H_odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_A_odds
#percentages
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_H <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_H, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_A <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_H <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6 + BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures$bundesligatwo_5_6
)
#AH_075_A
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_A <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 +BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures$bundesligatwo_6_5
)

#odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_H_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_H),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_A_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_A),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_H_odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_A_odds
#percentages
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_H <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_H, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_A <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_H <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5
)
#AH_n125_A
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_A <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 +BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6
)

#odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_H_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_H),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_A_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_A),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_H_odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_A_odds
#percentages
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_H <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_H, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_A <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_H <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_0 +BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_6_5 + BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6 + BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 + BUNDESLIGATWO_fixtures$bundesligatwo_5_6
)
#AH_125_A
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_A <- (
  BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_5 +BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
    BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
    BUNDESLIGATWO_fixtures$bundesligatwo_5_6 + BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 +
    BUNDESLIGATWO_fixtures$bundesligatwo_4_4 + BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 +
    BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 + BUNDESLIGATWO_fixtures$bundesligatwo_6_5
)

#odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_H_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_H),digits = 2)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_A_odds <- round((1/BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_A),digits = 2)

BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_H_odds
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_A_odds
#percentages
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_H <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_H, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_A <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
BUNDESLIGATWO_fixtures$bundesligatwo_ov25 <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_ov25, accuracy = 0.1)

BUNDESLIGATWO_fixtures$bundesligatwo_un25 <- percent(BUNDESLIGATWO_fixtures$bundesligatwo_un25, accuracy = 0.1)
BUNDESLIGATWO_fixtures$bundesligatwo_pscore <- paste(round(BUNDESLIGATWO_fixtures$bundesligatwo_xGH,digits = 0),round(BUNDESLIGATWO_fixtures$bundesligatwo_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
bundesligatwo_last_n_games <- 6

#create final_bundesligatwo_hf object
final_bundesligatwo_hf <- c()
for(index_bundesligatwo_hf in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_hf <- row.names(bundesligatwo_form_h) == bundesligatwo_teams[index_bundesligatwo_hf]
  form_bundesligatwo_hf <- bundesligatwo_form_h[index_bundesligatwo_hf]
  deleted_form_bundesligatwo_hf <- form_bundesligatwo_hf[!form_bundesligatwo_hf[] == ""]
  l6_form_bundesligatwo_hf <- tail(deleted_form_bundesligatwo_hf,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_hf <- paste(l6_form_bundesligatwo_hf,collapse = " ")
  final_bundesligatwo_hf[index_bundesligatwo_hf] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_hf],l6_form_bundesligatwo_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}

#change column nam
final_bundesligatwo_hf <- as.data.frame(final_bundesligatwo_hf)
colnames(final_bundesligatwo_hf) <- "Form"
#goals scored
#create final_bundesligatwo_gs object
final_bundesligatwo_gs <- c()
suml6_bundesligatwo_gs <- c()
for(index_bundesligatwo_gs in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_gs <- row.names(bundesligatwo_goalscored_h) == bundesligatwo_teams[index_bundesligatwo_gs]
  form_bundesligatwo_gs <- bundesligatwo_goalscored_h[index_bundesligatwo_gs]
  deleted_form_bundesligatwo_gs <- form_bundesligatwo_gs[!form_bundesligatwo_gs[] == ""]
  l6_form_bundesligatwo_gs <- tail(deleted_form_bundesligatwo_gs,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_gs <- as.numeric(l6_form_bundesligatwo_gs)
  suml6_bundesligatwo_gs[index_bundesligatwo_gs] <- sum(l6_form_bundesligatwo_gs)
  suml6_bundesligatwo_gs[index_bundesligatwo_gs] <- paste("(",suml6_bundesligatwo_gs[index_bundesligatwo_gs],")",sep = "")
  l6_form_bundesligatwo_gs <- paste(l6_form_bundesligatwo_gs,collapse = " ")
  final_bundesligatwo_gs[index_bundesligatwo_gs] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_gs],l6_form_bundesligatwo_gs,suml6_bundesligatwo_gs[index_bundesligatwo_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
final_bundesligatwo_gs
#change column names
final_bundesligatwo_gs <- as.data.frame(final_bundesligatwo_gs)
colnames(final_bundesligatwo_gs) <- "Goals scored"
#goal conceded
#create final_bundesligatwo_gc object
final_bundesligatwo_gc <- c()
suml6_bundesligatwo_gc <- c()
for(index_bundesligatwo_gc in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_gc <- row.names(bundesligatwo_goalconceded_h) == bundesligatwo_teams[index_bundesligatwo_gc]
  form_bundesligatwo_gc <- bundesligatwo_goalconceded_h[index_bundesligatwo_gc]
  deleted_form_bundesligatwo_gc <- form_bundesligatwo_gc[!form_bundesligatwo_gc[] == ""]
  l6_form_bundesligatwo_gc <- tail(deleted_form_bundesligatwo_gc,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_gc <- as.numeric(l6_form_bundesligatwo_gc)
  suml6_bundesligatwo_gc[index_bundesligatwo_gc] <- sum(l6_form_bundesligatwo_gc)
  suml6_bundesligatwo_gc[index_bundesligatwo_gc] <- paste("(",suml6_bundesligatwo_gc[index_bundesligatwo_gc],")",sep = "")
  l6_form_bundesligatwo_gc <- paste(l6_form_bundesligatwo_gc,collapse = " ")
  final_bundesligatwo_gc[index_bundesligatwo_gc] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_gc],l6_form_bundesligatwo_gc,suml6_bundesligatwo_gc[index_bundesligatwo_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
#change column names
final_bundesligatwo_gc <- as.data.frame(final_bundesligatwo_gc)
colnames(final_bundesligatwo_gc) <- "Goals conceded"


toString(l6_form_bundesligatwo_gc)
#total goals
#create final_bundesligatwo_tg object
final_bundesligatwo_tg <- c()
suml6_bundesligatwo_tg <- c()
for(index_bundesligatwo_tg in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_tg <- row.names(bundesligatwo_totalgoals_h) == bundesligatwo_teams[index_bundesligatwo_tg]
  form_bundesligatwo_tg <- bundesligatwo_totalgoals_h[index_bundesligatwo_tg]
  deleted_form_bundesligatwo_tg <- form_bundesligatwo_tg[!form_bundesligatwo_tg[] == ""]
  l6_form_bundesligatwo_tg <- tail(deleted_form_bundesligatwo_tg,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_tg <- as.numeric(l6_form_bundesligatwo_tg)
  suml6_bundesligatwo_tg[index_bundesligatwo_tg] <- sum(l6_form_bundesligatwo_tg)
  suml6_bundesligatwo_tg[index_bundesligatwo_tg] <- paste("(",suml6_bundesligatwo_tg[index_bundesligatwo_tg],")",sep = "")
  l6_form_bundesligatwo_tg <- paste(l6_form_bundesligatwo_tg,collapse = " ")
  final_bundesligatwo_tg[index_bundesligatwo_tg] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_tg],l6_form_bundesligatwo_tg,suml6_bundesligatwo_tg[index_bundesligatwo_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
#change column names
final_bundesligatwo_tg <- as.data.frame(final_bundesligatwo_tg)
colnames(final_bundesligatwo_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_bundesligatwo_hf object
final_bundesligatwo_cs <- c()
for(index_bundesligatwo_cs in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_cs <- row.names(bundesligatwo_csform_h) == bundesligatwo_teams[index_bundesligatwo_cs]
  csform_bundesligatwo_cs <- bundesligatwo_csform_h[index_bundesligatwo_cs]
  deleted_csform_bundesligatwo_cs <- csform_bundesligatwo_cs[!csform_bundesligatwo_cs[] == ""]
  l6_csform_bundesligatwo_cs <- tail(deleted_csform_bundesligatwo_cs,bundesligatwo_last_n_games)
  l6_csform_bundesligatwo_cs <- paste(l6_csform_bundesligatwo_cs,collapse = " ")
  final_bundesligatwo_cs[index_bundesligatwo_cs] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_cs],l6_csform_bundesligatwo_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_csform)

}

#change column names
final_bundesligatwo_cs <- as.data.frame(final_bundesligatwo_cs)
colnames(final_bundesligatwo_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_bundesligatwo_wm object
final_bundesligatwo_wm <- c()
suml6_bundesligatwo_wm <- c()
for(index_bundesligatwo_wm in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_wm <- row.names(bundesligatwo_winmargin_h) == bundesligatwo_teams[index_bundesligatwo_wm]
  form_bundesligatwo_wm <- bundesligatwo_winmargin_h[index_bundesligatwo_wm]
  deleted_form_bundesligatwo_wm <- form_bundesligatwo_wm[!form_bundesligatwo_wm[] == ""]
  l6_form_bundesligatwo_wm <- tail(deleted_form_bundesligatwo_wm,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_wm <- as.numeric(l6_form_bundesligatwo_wm)
  suml6_bundesligatwo_wm[index_bundesligatwo_wm] <- sum(l6_form_bundesligatwo_wm)
  suml6_bundesligatwo_wm[index_bundesligatwo_wm] <- paste("(",suml6_bundesligatwo_wm[index_bundesligatwo_wm],")",sep = "")
  l6_form_bundesligatwo_wm <- paste(l6_form_bundesligatwo_wm,collapse = " ")
  final_bundesligatwo_wm[index_bundesligatwo_wm] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_wm],l6_form_bundesligatwo_wm,suml6_bundesligatwo_wm[index_bundesligatwo_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
final_bundesligatwo_wm
#change column names
final_bundesligatwo_wm <- as.data.frame(final_bundesligatwo_wm)
colnames(final_bundesligatwo_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_bundesligatwo_ca object
final_bundesligatwo_ca <- c()
suml6_bundesligatwo_ca <- c()
for(index_bundesligatwo_ca in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_ca <- row.names(bundesligatwo_coawarded_h) == bundesligatwo_teams[index_bundesligatwo_ca]
  form_bundesligatwo_ca <- bundesligatwo_coawarded_h[index_bundesligatwo_ca]
  deleted_form_bundesligatwo_ca <- form_bundesligatwo_ca[!form_bundesligatwo_ca[] == ""]
  l6_form_bundesligatwo_ca <- tail(deleted_form_bundesligatwo_ca,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_ca <- as.numeric(l6_form_bundesligatwo_ca)
  suml6_bundesligatwo_ca[index_bundesligatwo_ca] <- sum(l6_form_bundesligatwo_ca)
  suml6_bundesligatwo_ca[index_bundesligatwo_ca] <- paste("(",suml6_bundesligatwo_ca[index_bundesligatwo_ca],")",sep = "")
  l6_form_bundesligatwo_ca <- paste(l6_form_bundesligatwo_ca,collapse = " ")
  final_bundesligatwo_ca[index_bundesligatwo_ca] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_ca],l6_form_bundesligatwo_ca,suml6_bundesligatwo_ca[index_bundesligatwo_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
final_bundesligatwo_ca
#change column names
final_bundesligatwo_ca <- as.data.frame(final_bundesligatwo_ca)
colnames(final_bundesligatwo_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_bundesligatwo_ca object
final_bundesligatwo_cc <- c()
suml6_bundesligatwo_cc <- c()
for(index_bundesligatwo_cc in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_cc <- row.names(bundesligatwo_cornersconceded_h) == bundesligatwo_teams[index_bundesligatwo_cc]
  form_bundesligatwo_cc <- bundesligatwo_cornersconceded_h[index_bundesligatwo_cc]
  deleted_form_bundesligatwo_cc <- form_bundesligatwo_cc[!form_bundesligatwo_cc[] == ""]
  l6_form_bundesligatwo_cc <- tail(deleted_form_bundesligatwo_cc,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_cc <- as.numeric(l6_form_bundesligatwo_cc)
  suml6_bundesligatwo_cc[index_bundesligatwo_cc] <- sum(l6_form_bundesligatwo_cc)
  suml6_bundesligatwo_cc[index_bundesligatwo_cc] <- paste("(",suml6_bundesligatwo_cc[index_bundesligatwo_cc],")",sep = "")
  l6_form_bundesligatwo_cc <- paste(l6_form_bundesligatwo_cc,collapse = " ")
  final_bundesligatwo_cc[index_bundesligatwo_cc] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_cc],l6_form_bundesligatwo_cc,suml6_bundesligatwo_cc[index_bundesligatwo_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
final_bundesligatwo_cc
#change column names
final_bundesligatwo_cc <- as.data.frame(final_bundesligatwo_cc)
colnames(final_bundesligatwo_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_bundesligatwo_cosc <- c()
for(index_bundesligatwo_cosc in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_cosc <- row.names(bundesligatwo_coscform_h) == bundesligatwo_teams[index_bundesligatwo_cosc]
  coscform_bundesligatwo_cosc <- bundesligatwo_coscform_h[index_bundesligatwo_cosc]
  deleted_coscform_bundesligatwo_cosc <- coscform_bundesligatwo_cosc[!coscform_bundesligatwo_cosc[] == ""]
  l6_coscform_bundesligatwo_cosc <- tail(deleted_coscform_bundesligatwo_cosc,bundesligatwo_last_n_games)
  l6_coscform_bundesligatwo_cosc <- paste(l6_coscform_bundesligatwo_cosc,collapse = " ")
  final_bundesligatwo_cosc[index_bundesligatwo_cosc] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_cosc],l6_coscform_bundesligatwo_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_coscform)

}
final_bundesligatwo_cosc
#change column names
final_bundesligatwo_cosc <- as.data.frame(final_bundesligatwo_cosc)
colnames(final_bundesligatwo_cosc) <- "CornersForm"
##################################################
#total corners
#create final_bundesligatwo_tcorners object
final_bundesligatwo_tcorners <- c()
suml6_bundesligatwo_tcorners <- c()
for(index_bundesligatwo_tcorners in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_tcorners <- row.names(bundesligatwo_totalcorners_h) == bundesligatwo_teams[index_bundesligatwo_tcorners]
  form_bundesligatwo_tcorners <- bundesligatwo_totalcorners_h[index_bundesligatwo_tcorners]
  deleted_form_bundesligatwo_tcorners <- form_bundesligatwo_tcorners[!form_bundesligatwo_tcorners[] == ""]
  l6_form_bundesligatwo_tcorners <- tail(deleted_form_bundesligatwo_tcorners,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_tcorners <- as.numeric(l6_form_bundesligatwo_tcorners)
  suml6_bundesligatwo_tcorners[index_bundesligatwo_tcorners] <- sum(l6_form_bundesligatwo_tcorners)
  suml6_bundesligatwo_tcorners[index_bundesligatwo_tcorners] <- paste("(",suml6_bundesligatwo_tcorners[index_bundesligatwo_tcorners],")",sep = "")
  l6_form_bundesligatwo_tcorners <- paste(l6_form_bundesligatwo_tcorners,collapse = " ")
  final_bundesligatwo_tcorners[index_bundesligatwo_tcorners] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_tcorners],l6_form_bundesligatwo_tcorners,suml6_bundesligatwo_tcorners[index_bundesligatwo_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
#change column names
final_bundesligatwo_tcorners <- as.data.frame(final_bundesligatwo_tcorners)
colnames(final_bundesligatwo_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_bundesligatwo_hf_against
final_bundesligatwo_hf_against <- c()
for(index_bundesligatwo_hf_against in 1:length(bundesligatwo_teams))
{
  index_bundesligatwo_hf_against <- row.names(bundesligatwo_form_team_against_h) == bundesligatwo_teams[index_bundesligatwo_hf_against]
  form_bundesligatwo_hf_against <- bundesligatwo_form_team_against_h[index_bundesligatwo_hf_against]
  deleted_form_bundesligatwo_hf_against <- form_bundesligatwo_hf_against[!form_bundesligatwo_hf_against[] == ""]
  l6_form_bundesligatwo_hf_against <- tail(deleted_form_bundesligatwo_hf_against,bundesligatwo_last_n_games)
  l6_form_bundesligatwo_hf_against <- paste(l6_form_bundesligatwo_hf_against,collapse = " ")
  final_bundesligatwo_hf_against[index_bundesligatwo_hf_against] <- rbind(paste(bundesligatwo_teams[index_bundesligatwo_hf_against],l6_form_bundesligatwo_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bundesligatwo_teams[index],l6_form)

}
final_bundesligatwo_hf_against <- as.data.frame(final_bundesligatwo_hf_against)
colnames(final_bundesligatwo_hf_against) <- "Team against"
#combine the columns
final_bundesligatwo_all <- cbind(final_bundesligatwo_hf,final_bundesligatwo_gs,final_bundesligatwo_gc,final_bundesligatwo_tg,final_bundesligatwo_ca,final_bundesligatwo_cc,final_bundesligatwo_tcorners,final_bundesligatwo_cosc,final_bundesligatwo_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#BUNDESLIGATWO
BUNDESLIGATWO_sim <- BUNDESLIGATWO
BUNDESLIGATWO_sim$matchid <- paste(BUNDESLIGATWO_sim$HomeTeam,BUNDESLIGATWO_sim$AwayTeam,sep = "-")
BUNDESLIGATWO_fixtures$matchid <- paste(BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo,BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo,sep = "-")
BUNDESLIGATWO_fixtures$bundesligatwo_FTR <- sapply(BUNDESLIGATWO_fixtures$bundesligatwo_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus <- ifelse(BUNDESLIGATWO_fixtures$matchid %in% BUNDESLIGATWO_sim$matchid,"played","notplayed")

bundesligatwo_home_wins_sim <- c()
bundesligatwo_away_wins_sim <- c()
bundesligatwo_home_draws_sim <- c()
bundesligatwo_away_draws_sim <- c()
bundesligatwo_home_loss_sim <- c()
bundesligatwo_away_loss_sim <- c()



for (i_bundesligatwo_wins_sim in 1:length(bundesligatwo_teams))
{

  bundesligatwo_home_wins_sim[i_bundesligatwo_wins_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_wins_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_FTR == "H" & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus =="notplayed",])
  bundesligatwo_away_wins_sim[i_bundesligatwo_wins_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_wins_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_FTR == "A" & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",])
  bundesligatwo_home_draws_sim[i_bundesligatwo_wins_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_wins_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_FTR == "D" & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",])
  bundesligatwo_away_draws_sim[i_bundesligatwo_wins_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_wins_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_FTR == "D" & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",])
  bundesligatwo_home_loss_sim[i_bundesligatwo_wins_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_wins_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_FTR == "A" & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",])
  bundesligatwo_away_loss_sim[i_bundesligatwo_wins_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_wins_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_FTR == "H" & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed", ])

}

bundesligatwo_total_wins_sim <- bundesligatwo_home_wins_sim + bundesligatwo_away_wins_sim
bundesligatwo_total_draws_sim <- bundesligatwo_home_draws_sim + bundesligatwo_away_draws_sim
bundesligatwo_total_loss_sim <- bundesligatwo_home_loss_sim + bundesligatwo_away_loss_sim

bundesligatwo_home_games_sim <- c()
bundesligatwo_away_games_sim <-c()

for (i_bundesligatwo_sim in 1:length(bundesligatwo_teams))
{

  bundesligatwo_home_games_sim[i_bundesligatwo_sim] <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",])
  bundesligatwo_away_games_sim[i_bundesligatwo_sim]  <- nrow(BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo == bundesligatwo_teams[i_bundesligatwo_sim] & BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",])

}

bundesligatwo_games_played_sim <- bundesligatwo_home_games_sim + bundesligatwo_away_games_sim

bundesligatwo_league_table_sim <- cbind(bundesligatwo_teams,bundesligatwo_games_played_sim,bundesligatwo_total_wins_sim,bundesligatwo_total_draws_sim,bundesligatwo_total_loss_sim)
bundesligatwo_PTS_sim <- (bundesligatwo_total_wins_sim*3) + (bundesligatwo_total_draws_sim*1)
bundesligatwo_league_table_sim <- cbind(bundesligatwo_league_table_sim,bundesligatwo_PTS_sim)

bundesligatwo_games_played_simfinal <- bundesligatwo_games_played + bundesligatwo_games_played_sim
bundesligatwo_total_wins_simfinal <- bundesligatwo_total_wins + bundesligatwo_total_wins_sim
bundesligatwo_total_draws_simfinal <- bundesligatwo_total_draws + bundesligatwo_total_draws_sim
bundesligatwo_total_loss_simfinal <- bundesligatwo_total_loss + bundesligatwo_total_loss_sim
bundesligatwo_PTS_simfinal <- bundesligatwo_PTS + bundesligatwo_PTS_sim

bundesligatwo_league_table_simfinal <- cbind(bundesligatwo_teams,bundesligatwo_games_played_simfinal,bundesligatwo_total_wins_simfinal,bundesligatwo_total_draws_simfinal,bundesligatwo_total_loss_simfinal,bundesligatwo_PTS_simfinal)
bundesligatwo_league_table_simfinal <- as.data.frame(bundesligatwo_league_table_simfinal)
names(bundesligatwo_league_table_simfinal)[names(bundesligatwo_league_table_simfinal) == "bundesligatwo_teams"] <- "Team_f"
names(bundesligatwo_league_table_simfinal)[names(bundesligatwo_league_table_simfinal) == "bundesligatwo_games_played_simfinal"] <- "P_f"
names(bundesligatwo_league_table_simfinal)[names(bundesligatwo_league_table_simfinal) == "bundesligatwo_total_wins_simfinal"] <- "W_f"
names(bundesligatwo_league_table_simfinal)[names(bundesligatwo_league_table_simfinal) == "bundesligatwo_total_draws_simfinal"] <- "D_f"
names(bundesligatwo_league_table_simfinal)[names(bundesligatwo_league_table_simfinal) == "bundesligatwo_total_loss_simfinal"] <- "L_f"
names(bundesligatwo_league_table_simfinal)[names(bundesligatwo_league_table_simfinal) == "bundesligatwo_PTS_simfinal"] <- "PTS_f"
points_bundesligatwo_sim <-  bundesligatwo_league_table_simfinal[order(as.numeric(bundesligatwo_league_table_simfinal$PTS_f), decreasing = TRUE),]

BUNDESLIGATWO_notplayed <- BUNDESLIGATWO_fixtures[BUNDESLIGATWO_fixtures$bundesligatwo_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#BUNDESLIGATWO
BUNDESLIGATWO_fixtures$Hometeam_bundesligatwo_index <- match(BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo,bundesligatwo_teams)
BUNDESLIGATWO_fixtures$Awayteam_bundesligatwo_index <- match(BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo,bundesligatwo_teams)
bundesligatwo_prediction <- c()
bundesligatwo_HWM <- c()
bundesligatwo_AWM <- c()
bundesligatwo_HWMLM <- c()
bundesligatwo_AWMLM <- c()
bundesligatwo_HY <- c()
bundesligatwo_AY <- c()
bundesligatwo_HCO <- c()
bundesligatwo_ACO <- c()
bundesligatwo_HXSC <- c()
bundesligatwo_AXSC <- c()
bundesligatwo_HYCPF <- c()
bundesligatwo_AYCPF <- c()
for(bundesligatwo_row in 1:nrow(BUNDESLIGATWO_fixtures))
{

  bundesligatwo_hometeamindex <- BUNDESLIGATWO_fixtures[bundesligatwo_row,"Hometeam_bundesligatwo_index"]
  bundesligatwo_awayteamindex <- BUNDESLIGATWO_fixtures[bundesligatwo_row,"Awayteam_bundesligatwo_index"]
  #analyse team form
  #home team
  bundesligatwo_form_vec_ht <- as.vector(bundesligatwo_form_h[bundesligatwo_hometeamindex,])
  bundesligatwo_form_vec_ht[is.na(bundesligatwo_form_vec_ht)] <- ""
  bundesligatwo_form_vec_ht <- bundesligatwo_form_vec_ht[bundesligatwo_form_vec_ht != ""]
  bundesligatwo_form_vec_ht  <-tail(bundesligatwo_form_vec_ht,6)
  bundesligatwo_ht_numberof_wins <- length(which(bundesligatwo_form_vec_ht == "W"))
  bundesligatwo_ht_numberof_draws <- length(which(bundesligatwo_form_vec_ht == "D"))
  bundesligatwo_ht_numberof_loss <- length(which(bundesligatwo_form_vec_ht == "L"))
  #awayteam
  bundesligatwo_form_vec_at <- as.vector(bundesligatwo_form_h[bundesligatwo_awayteamindex,])
  bundesligatwo_form_vec_at[is.na(bundesligatwo_form_vec_at)] <- ""
  bundesligatwo_form_vec_at <- bundesligatwo_form_vec_at[bundesligatwo_form_vec_at != ""]
  bundesligatwo_form_vec_at  <-tail(bundesligatwo_form_vec_at,6)
  bundesligatwo_at_numberof_wins <- length(which(bundesligatwo_form_vec_at == "W"))
  bundesligatwo_at_numberof_draws <- length(which(bundesligatwo_form_vec_at == "D"))
  bundesligatwo_at_numberof_loss <- length(which(bundesligatwo_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  bundesligatwo_goalscored_vec_ht <- as.vector(bundesligatwo_goalscored_h[bundesligatwo_hometeamindex,])
  bundesligatwo_goalscored_vec_ht[is.na(bundesligatwo_goalscored_vec_ht)] <- ""
  bundesligatwo_goalscored_vec_ht <- bundesligatwo_goalscored_vec_ht[bundesligatwo_goalscored_vec_ht != ""]
  bundesligatwo_goalscored_vec_ht  <-tail(bundesligatwo_goalscored_vec_ht,6)
  bundesligatwo_goalscored_vec_ht  <- as.numeric(bundesligatwo_goalscored_vec_ht)
  bundesligatwo_ht_totalgoalscored <- sum(bundesligatwo_goalscored_vec_ht)
  bundesligatwo_ht_matches_scoring <- length(which(bundesligatwo_goalscored_vec_ht > 0))
  bundesligatwo_ht_matches_without_scoring <- length(which(bundesligatwo_goalscored_vec_ht == "0"))
  #awayteam
  bundesligatwo_goalscored_vec_at <- as.vector(bundesligatwo_goalscored_h[bundesligatwo_awayteamindex,])
  bundesligatwo_goalscored_vec_at[is.na(bundesligatwo_goalscored_vec_at)] <- ""
  bundesligatwo_goalscored_vec_at <- bundesligatwo_goalscored_vec_at[bundesligatwo_goalscored_vec_at != ""]
  bundesligatwo_goalscored_vec_at  <-tail(bundesligatwo_goalscored_vec_at,6)
  bundesligatwo_goalscored_vec_at  <- as.numeric(bundesligatwo_goalscored_vec_at)
  bundesligatwo_at_totalgoalscored <- sum(bundesligatwo_goalscored_vec_at)
  bundesligatwo_at_matches_scoring <- length(which(bundesligatwo_goalscored_vec_at > 0))
  bundesligatwo_at_matches_without_scoring <- length(which(bundesligatwo_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  bundesligatwo_goalconceded_vec_ht <- as.vector(bundesligatwo_goalconceded_h[bundesligatwo_hometeamindex,])
  bundesligatwo_goalconceded_vec_ht[is.na(bundesligatwo_goalconceded_vec_ht)] <- ""
  bundesligatwo_goalconceded_vec_ht <- bundesligatwo_goalconceded_vec_ht[bundesligatwo_goalconceded_vec_ht != ""]
  bundesligatwo_goalconceded_vec_ht  <-tail(bundesligatwo_goalconceded_vec_ht,6)
  bundesligatwo_goalconceded_vec_ht  <- as.numeric(bundesligatwo_goalconceded_vec_ht)
  bundesligatwo_goalconceded_vec_ht
  bundesligatwo_ht_totalgoalconceded <- sum(bundesligatwo_goalconceded_vec_ht)
  bundesligatwo_ht_matches_concede <- length(which(bundesligatwo_goalconceded_vec_ht > 0))
  bundesligatwo_ht_matches_without_concede <- length(which(bundesligatwo_goalconceded_vec_ht == "0"))
  #awayteam
  bundesligatwo_goalconceded_vec_at <- as.vector(bundesligatwo_goalconceded_h[bundesligatwo_awayteamindex,])
  bundesligatwo_goalconceded_vec_at[is.na(bundesligatwo_goalconceded_vec_at)] <- ""
  bundesligatwo_goalconceded_vec_at <- bundesligatwo_goalconceded_vec_at[bundesligatwo_goalconceded_vec_at != ""]
  bundesligatwo_goalconceded_vec_at  <-tail(bundesligatwo_goalconceded_vec_at,6)
  bundesligatwo_goalconceded_vec_at  <- as.numeric(bundesligatwo_goalconceded_vec_at)
  bundesligatwo_at_totalgoalconceded <- sum(bundesligatwo_goalconceded_vec_at)
  bundesligatwo_at_matches_concede <- length(which(bundesligatwo_goalconceded_vec_at > 0))
  bundesligatwo_at_matches_without_concede <- length(which(bundesligatwo_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  bundesligatwo_totalgoals_vec_ht <- as.vector(bundesligatwo_totalgoals_h[bundesligatwo_hometeamindex,])
  bundesligatwo_totalgoals_vec_ht[is.na(bundesligatwo_totalgoals_vec_ht)] <- ""
  bundesligatwo_totalgoals_vec_ht <- bundesligatwo_totalgoals_vec_ht[bundesligatwo_totalgoals_vec_ht != ""]
  bundesligatwo_totalgoals_vec_ht  <-tail(bundesligatwo_totalgoals_vec_ht,6)
  bundesligatwo_totalgoals_vec_ht  <- as.numeric(bundesligatwo_totalgoals_vec_ht)
  bundesligatwo_totalgoals_vec_ht
  bundesligatwo_ht_totalgoals <- sum(bundesligatwo_totalgoals_vec_ht)
  bundesligatwo_ht_avgtotalgoals <- (bundesligatwo_ht_totalgoals/6)
  bundesligatwo_ht_no_of_ov25 <- length(which(bundesligatwo_totalgoals_vec_ht >= 3))
  bundesligatwo_ht_no_of_un25 <- length(which(bundesligatwo_totalgoals_vec_ht <= 2))
  #awayteam
  bundesligatwo_totalgoals_vec_at <- as.vector(bundesligatwo_totalgoals_h[bundesligatwo_awayteamindex,])
  bundesligatwo_totalgoals_vec_at[is.na(bundesligatwo_totalgoals_vec_at)] <- ""
  bundesligatwo_totalgoals_vec_at <- bundesligatwo_totalgoals_vec_at[bundesligatwo_totalgoals_vec_at != ""]
  bundesligatwo_totalgoals_vec_at  <-tail(bundesligatwo_totalgoals_vec_at,6)
  bundesligatwo_totalgoals_vec_at  <- as.numeric(bundesligatwo_totalgoals_vec_at)
  bundesligatwo_totalgoals_vec_at
  bundesligatwo_at_totalgoals <- sum(bundesligatwo_totalgoals_vec_at)
  bundesligatwo_at_avgtotalgoals <- (bundesligatwo_at_totalgoals/6)
  bundesligatwo_at_no_of_ov25 <- length(which(bundesligatwo_totalgoals_vec_at >= 3))
  bundesligatwo_at_no_of_un25 <- length(which(bundesligatwo_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  bundesligatwo_winmargin_vec_ht <- as.vector(bundesligatwo_winmargin_h[bundesligatwo_hometeamindex,])
  bundesligatwo_winmargin_vec_ht[is.na(bundesligatwo_winmargin_vec_ht)] <- ""
  bundesligatwo_winmargin_vec_ht <- bundesligatwo_winmargin_vec_ht[bundesligatwo_winmargin_vec_ht != ""]
  bundesligatwo_winmargin_vec_ht  <-tail(bundesligatwo_winmargin_vec_ht,6)
  bundesligatwo_winmargin_vec_ht  <- as.numeric(bundesligatwo_winmargin_vec_ht)

  bundesligatwo_ht_totalwinmargin <- sum(bundesligatwo_winmargin_vec_ht)
  bundesligatwo_ht_no_of_winmargin_ov0 <- length(which(bundesligatwo_winmargin_vec_ht >= 0))
  bundesligatwo_ht_no_of_winmargin_ov1 <- length(which(bundesligatwo_winmargin_vec_ht >= 1))
  bundesligatwo_ht_no_of_winmargin_un0 <- length(which(bundesligatwo_winmargin_vec_ht <= 0))
  bundesligatwo_ht_no_of_winmargin_un1 <- length(which(bundesligatwo_winmargin_vec_ht <= 1))
  #awayteam
  bundesligatwo_winmargin_vec_at <- as.vector(bundesligatwo_winmargin_h[bundesligatwo_awayteamindex,])
  bundesligatwo_winmargin_vec_at[is.na(bundesligatwo_winmargin_vec_at)] <- ""
  bundesligatwo_winmargin_vec_at <- bundesligatwo_winmargin_vec_at[bundesligatwo_winmargin_vec_at != ""]
  bundesligatwo_winmargin_vec_at  <-tail(bundesligatwo_winmargin_vec_at,6)
  bundesligatwo_winmargin_vec_at  <- as.numeric(bundesligatwo_winmargin_vec_at)

  bundesligatwo_at_totalwinmargin <- sum(bundesligatwo_winmargin_vec_at)
  bundesligatwo_at_no_of_winmargin_ov0 <- length(which(bundesligatwo_winmargin_vec_at >= 0))
  bundesligatwo_at_no_of_winmargin_ov1 <- length(which(bundesligatwo_winmargin_vec_at >= 1))
  bundesligatwo_at_no_of_winmargin_un0 <- length(which(bundesligatwo_winmargin_vec_at <= 0))
  bundesligatwo_at_no_of_winmargin_un1 <- length(which(bundesligatwo_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  bundesligatwo_winmargin_vec_ht_lm <- as.vector(bundesligatwo_winmargin_h[bundesligatwo_hometeamindex,])
  bundesligatwo_winmargin_vec_ht_lm[is.na(bundesligatwo_winmargin_vec_ht_lm)] <- ""
  bundesligatwo_winmargin_vec_ht_lm <- bundesligatwo_winmargin_vec_ht_lm[bundesligatwo_winmargin_vec_ht_lm != ""]
  bundesligatwo_winmargin_vec_ht_lm  <-tail(bundesligatwo_winmargin_vec_ht_lm,1)
  #awayteam
  bundesligatwo_winmargin_vec_at_lm <- as.vector(bundesligatwo_winmargin_h[bundesligatwo_awayteamindex,])
  bundesligatwo_winmargin_vec_at_lm[is.na(bundesligatwo_winmargin_vec_at_lm)] <- ""
  bundesligatwo_winmargin_vec_at_lm <- bundesligatwo_winmargin_vec_at_lm[bundesligatwo_winmargin_vec_at_lm != ""]
  bundesligatwo_winmargin_vec_at_lm  <-tail(bundesligatwo_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  bundesligatwo_yellowtotals_vec_ht <- as.vector(bundesligatwo_yellowtotalsv2[bundesligatwo_hometeamindex,])
  bundesligatwo_yellowtotals_vec_ht[is.na(bundesligatwo_yellowtotals_vec_ht)] <- ""
  bundesligatwo_yellowtotals_vec_ht <- bundesligatwo_yellowtotals_vec_ht[bundesligatwo_yellowtotals_vec_ht != ""]
  bundesligatwo_yellowtotals_vec_ht  <-tail(bundesligatwo_yellowtotals_vec_ht,1)
  #awayteam
  bundesligatwo_yellowtotals_vec_at <- as.vector(bundesligatwo_yellowtotalsv2[bundesligatwo_awayteamindex,])
  bundesligatwo_yellowtotals_vec_at[is.na(bundesligatwo_yellowtotals_vec_at)] <- ""
  bundesligatwo_yellowtotals_vec_at <- bundesligatwo_yellowtotals_vec_at[bundesligatwo_yellowtotals_vec_at != ""]
  bundesligatwo_yellowtotals_vec_at  <-tail(bundesligatwo_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  bundesligatwo_cornertotals_vec_ht <- as.vector(bundesligatwo_cornertotalsv2[bundesligatwo_hometeamindex,])
  bundesligatwo_cornertotals_vec_ht[is.na(bundesligatwo_cornertotals_vec_ht)] <- ""
  bundesligatwo_cornertotals_vec_ht <- bundesligatwo_cornertotals_vec_ht[bundesligatwo_cornertotals_vec_ht != ""]
  bundesligatwo_cornertotals_vec_ht  <-tail(bundesligatwo_cornertotals_vec_ht,1)
  #awayteam
  bundesligatwo_cornertotals_vec_at <- as.vector(bundesligatwo_cornertotalsv2[bundesligatwo_awayteamindex,])
  bundesligatwo_cornertotals_vec_at[is.na(bundesligatwo_cornertotals_vec_at)] <- ""
  bundesligatwo_cornertotals_vec_at <- bundesligatwo_cornertotals_vec_at[bundesligatwo_cornertotals_vec_at != ""]
  bundesligatwo_cornertotals_vec_at  <-tail(bundesligatwo_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  bundesligatwo_xshotsconversion_vec_ht <- as.vector(bundesligatwo_shots_analysis[bundesligatwo_hometeamindex,])
  bundesligatwo_xshotsconversion_vec_ht[is.na(bundesligatwo_xshotsconversion_vec_ht)] <- ""
  bundesligatwo_xshotsconversion_vec_ht <- bundesligatwo_xshotsconversion_vec_ht[bundesligatwo_xshotsconversion_vec_ht != ""]
  bundesligatwo_xshotsconversion_vec_ht  <-tail(bundesligatwo_xshotsconversion_vec_ht,1)
  #awayteam
  bundesligatwo_xshotsconversion_vec_at <- as.vector(bundesligatwo_shots_analysis[bundesligatwo_awayteamindex,])
  bundesligatwo_xshotsconversion_vec_at[is.na(bundesligatwo_xshotsconversion_vec_at)] <- ""
  bundesligatwo_xshotsconversion_vec_at <- bundesligatwo_xshotsconversion_vec_at[bundesligatwo_xshotsconversion_vec_at != ""]
  bundesligatwo_xshotsconversion_vec_at  <-tail(bundesligatwo_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  bundesligatwo_fouls_conversion_vec_ht <- as.vector(bundesligatwo_fouls_conversion[bundesligatwo_hometeamindex,])
  bundesligatwo_fouls_conversion_vec_ht[is.na(bundesligatwo_fouls_conversion_vec_ht)] <- ""
  bundesligatwo_fouls_conversion_vec_ht <- bundesligatwo_fouls_conversion_vec_ht[bundesligatwo_fouls_conversion_vec_ht != ""]
  bundesligatwo_fouls_conversion_vec_ht  <-tail(bundesligatwo_fouls_conversion_vec_ht,1)
  #awayteam
  bundesligatwo_fouls_conversion_vec_at <- as.vector(bundesligatwo_fouls_conversion[bundesligatwo_awayteamindex,])
  bundesligatwo_fouls_conversion_vec_at[is.na(bundesligatwo_fouls_conversion_vec_at)] <- ""
  bundesligatwo_fouls_conversion_vec_at <- bundesligatwo_fouls_conversion_vec_at[bundesligatwo_fouls_conversion_vec_at != ""]
  bundesligatwo_fouls_conversion_vec_at  <-tail(bundesligatwo_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  bundesligatwo_ht_last6points <- bundesligatwo_ht_numberof_wins*3 + bundesligatwo_ht_numberof_draws*1
  bundesligatwo_at_last6points <- bundesligatwo_at_numberof_wins*3 + bundesligatwo_at_numberof_draws*1

  if(bundesligatwo_ht_last6points > bundesligatwo_at_last6points) {bundesligatwo_3waypick <- "1"}  else {bundesligatwo_3waypick <- "X2"}

  if(bundesligatwo_at_last6points > bundesligatwo_ht_last6points ) {bundesligatwo_3waypick <- "2"} else {bundesligatwo_3waypick <- "1X"}

  if(bundesligatwo_ht_no_of_ov25 + bundesligatwo_at_no_of_ov25 >= 6) {bundesligatwo_goalspick <- "ov25"} else {bundesligatwo_goalspick <- "un25"}

  if(bundesligatwo_ht_no_of_un25 + bundesligatwo_at_no_of_un25 >= 6) {bundesligatwo_goalspick <- "un25"} else {bundesligatwo_goalspick <- "ov25"}

  if(bundesligatwo_ht_matches_scoring >= 4 && bundesligatwo_at_matches_scoring >=4) {bundesligatwo_btts <- "BTTS-Y"} else {bundesligatwo_btts <- "BTTS-N"}


  bundesligatwo_prediction[bundesligatwo_row] <- rbind(paste(bundesligatwo_3waypick,bundesligatwo_goalspick,bundesligatwo_btts,sep = ","))
  bundesligatwo_HWM[bundesligatwo_row] <- bundesligatwo_ht_totalwinmargin
  bundesligatwo_AWM[bundesligatwo_row] <- bundesligatwo_at_totalwinmargin

  bundesligatwo_HWMLM[bundesligatwo_row] <- bundesligatwo_winmargin_vec_ht_lm
  bundesligatwo_AWMLM[bundesligatwo_row] <- bundesligatwo_winmargin_vec_at_lm

  bundesligatwo_HY[bundesligatwo_row] <- bundesligatwo_yellowtotals_vec_ht
  bundesligatwo_AY[bundesligatwo_row] <- bundesligatwo_yellowtotals_vec_at

  bundesligatwo_HCO[bundesligatwo_row] <- bundesligatwo_cornertotals_vec_ht
  bundesligatwo_ACO[bundesligatwo_row] <- bundesligatwo_cornertotals_vec_at

  bundesligatwo_HXSC[bundesligatwo_row] <- bundesligatwo_xshotsconversion_vec_ht
  bundesligatwo_AXSC[bundesligatwo_row] <- bundesligatwo_xshotsconversion_vec_at

  bundesligatwo_HYCPF[bundesligatwo_row] <- bundesligatwo_fouls_conversion_vec_ht
  bundesligatwo_AYCPF[bundesligatwo_row] <- bundesligatwo_fouls_conversion_vec_at
}

bundesligatwo_prediction <- as.data.frame(bundesligatwo_prediction)
colnames(bundesligatwo_prediction) <- "prediction"

bundesligatwo_HWM <- as.data.frame(bundesligatwo_HWM)
colnames(bundesligatwo_HWM) <- "HWM"

bundesligatwo_AWM <- as.data.frame(bundesligatwo_AWM)
colnames(bundesligatwo_AWM) <- "AWM"

bundesligatwo_HWMLM <- as.data.frame(bundesligatwo_HWMLM)
colnames(bundesligatwo_HWMLM) <- "HWMLM"

bundesligatwo_AWMLM <- as.data.frame(bundesligatwo_AWMLM)
colnames(bundesligatwo_AWMLM) <- "AWMLM"

bundesligatwo_HY <- as.data.frame(bundesligatwo_HY)
colnames(bundesligatwo_HY) <- "AVGHY"

bundesligatwo_AY <- as.data.frame(bundesligatwo_AY)
colnames(bundesligatwo_AY) <- "AVGAY"

bundesligatwo_HCO <- as.data.frame(bundesligatwo_HCO)
colnames(bundesligatwo_HCO) <- "AVGHCO"

bundesligatwo_ACO <- as.data.frame(bundesligatwo_ACO)
colnames(bundesligatwo_ACO) <- "AVGACO"

bundesligatwo_HXSC <- as.data.frame(bundesligatwo_HXSC)
colnames(bundesligatwo_HXSC) <- "HXSC"

bundesligatwo_AXSC <- as.data.frame(bundesligatwo_AXSC)
colnames(bundesligatwo_AXSC) <- "AXSC"

bundesligatwo_HYCPF <- as.data.frame(bundesligatwo_HYCPF)
colnames(bundesligatwo_HYCPF) <- "HYCPF"

bundesligatwo_AYCPF <- as.data.frame(bundesligatwo_AYCPF)
colnames(bundesligatwo_AYCPF) <- "AYCPF"

bundesligatwo_picks <- cbind(BUNDESLIGATWO_fixtures$Div,BUNDESLIGATWO_fixtures$HomeTeam_bundesligatwo,BUNDESLIGATWO_fixtures$AwayTeam_bundesligatwo,bundesligatwo_prediction,bundesligatwo_HWM,bundesligatwo_AWM,bundesligatwo_HWMLM,bundesligatwo_AWMLM,bundesligatwo_HY,bundesligatwo_AY,bundesligatwo_HCO,bundesligatwo_ACO,bundesligatwo_HXSC,bundesligatwo_AXSC,bundesligatwo_HYCPF,bundesligatwo_AYCPF)

colnames(bundesligatwo_picks)[1] <- "picks_Div"
colnames(bundesligatwo_picks)[2] <- "picks_HomeTeam"
colnames(bundesligatwo_picks)[3] <- "picks_AwayTeam"
bundesligatwo_picks$matchid <- paste(bundesligatwo_picks$picks_HomeTeam,bundesligatwo_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of BUNDESLIGATWO
bundesligatwo_picks
#############################################################################################################################################################################
#clone fixtures
BUNDESLIGATWO_fixtures_clone <- BUNDESLIGATWO_fixtures
colnames(BUNDESLIGATWO_fixtures_clone)[61] <- "Hwin"
colnames(BUNDESLIGATWO_fixtures_clone)[62] <- "Draw"
colnames(BUNDESLIGATWO_fixtures_clone)[63] <- "Awin"

BUNDESLIGATWO_fixtures_clone$Hwinodds <-   BUNDESLIGATWO_fixtures$bundesligatwo_1_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_0 + BUNDESLIGATWO_fixtures$bundesligatwo_2_1 + BUNDESLIGATWO_fixtures$bundesligatwo_3_0 + BUNDESLIGATWO_fixtures$bundesligatwo_3_1 +
  BUNDESLIGATWO_fixtures$bundesligatwo_3_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_0 + BUNDESLIGATWO_fixtures$bundesligatwo_4_1 + BUNDESLIGATWO_fixtures$bundesligatwo_4_2 + BUNDESLIGATWO_fixtures$bundesligatwo_4_3 +
  BUNDESLIGATWO_fixtures$bundesligatwo_5_0 + BUNDESLIGATWO_fixtures$bundesligatwo_5_1 + BUNDESLIGATWO_fixtures$bundesligatwo_5_2 + BUNDESLIGATWO_fixtures$bundesligatwo_5_3 + BUNDESLIGATWO_fixtures$bundesligatwo_5_4 +
  BUNDESLIGATWO_fixtures$bundesligatwo_6_0 + BUNDESLIGATWO_fixtures$bundesligatwo_6_1 + BUNDESLIGATWO_fixtures$bundesligatwo_6_2 + BUNDESLIGATWO_fixtures$bundesligatwo_6_3 + BUNDESLIGATWO_fixtures$bundesligatwo_6_4 +
  BUNDESLIGATWO_fixtures$bundesligatwo_6_5
BUNDESLIGATWO_fixtures_clone$Hwinodds <- round(1/BUNDESLIGATWO_fixtures_clone$Hwinodds, digits = 3)

BUNDESLIGATWO_fixtures_clone$Drawodds <-  BUNDESLIGATWO_fixtures$bundesligatwo_0_0 + BUNDESLIGATWO_fixtures$bundesligatwo_1_1 + BUNDESLIGATWO_fixtures$bundesligatwo_2_2 + BUNDESLIGATWO_fixtures$bundesligatwo_3_3 + BUNDESLIGATWO_fixtures$bundesligatwo_4_4 +
  BUNDESLIGATWO_fixtures$bundesligatwo_5_5 + BUNDESLIGATWO_fixtures$bundesligatwo_6_6

BUNDESLIGATWO_fixtures_clone$Drawodds <- round(1/BUNDESLIGATWO_fixtures_clone$Drawodds, digits = 3)

BUNDESLIGATWO_fixtures_clone$Awinodds <-   BUNDESLIGATWO_fixtures$bundesligatwo_0_1 + BUNDESLIGATWO_fixtures$bundesligatwo_0_2 + BUNDESLIGATWO_fixtures$bundesligatwo_1_2 + BUNDESLIGATWO_fixtures$bundesligatwo_0_3 + BUNDESLIGATWO_fixtures$bundesligatwo_1_3 +
  BUNDESLIGATWO_fixtures$bundesligatwo_2_3 + BUNDESLIGATWO_fixtures$bundesligatwo_0_4 + BUNDESLIGATWO_fixtures$bundesligatwo_1_4 + BUNDESLIGATWO_fixtures$bundesligatwo_2_4 + BUNDESLIGATWO_fixtures$bundesligatwo_3_4 +
  BUNDESLIGATWO_fixtures$bundesligatwo_0_5 + BUNDESLIGATWO_fixtures$bundesligatwo_1_5 + BUNDESLIGATWO_fixtures$bundesligatwo_2_5 + BUNDESLIGATWO_fixtures$bundesligatwo_3_5 + BUNDESLIGATWO_fixtures$bundesligatwo_4_5 +
  BUNDESLIGATWO_fixtures$bundesligatwo_0_6 + BUNDESLIGATWO_fixtures$bundesligatwo_1_6 + BUNDESLIGATWO_fixtures$bundesligatwo_2_6 + BUNDESLIGATWO_fixtures$bundesligatwo_3_6 + BUNDESLIGATWO_fixtures$bundesligatwo_4_6 +
  BUNDESLIGATWO_fixtures$bundesligatwo_5_6

BUNDESLIGATWO_fixtures_clone$Awinodds <- round(1/BUNDESLIGATWO_fixtures_clone$Awinodds, digits = 3)

colnames(BUNDESLIGATWO_fixtures_clone)[15] <- "CS_1-1"
colnames(BUNDESLIGATWO_fixtures_clone)[13] <- "CS_1-0"
colnames(BUNDESLIGATWO_fixtures_clone)[14] <- "CS_0-1"
colnames(BUNDESLIGATWO_fixtures_clone)[16] <- "CS_2-0"
colnames(BUNDESLIGATWO_fixtures_clone)[17] <- "CS_0-2"
colnames(BUNDESLIGATWO_fixtures_clone)[19] <- "CS_2-1"
colnames(BUNDESLIGATWO_fixtures_clone)[20] <- "CS_1-2"

BUNDESLIGATWO_fixtures_clone$`CS_1-1` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_1-1`, digits = 3)
BUNDESLIGATWO_fixtures_clone$`CS_1-0` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_1-0`, digits = 3)
BUNDESLIGATWO_fixtures_clone$`CS_0-1` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_0-1`, digits = 3)
BUNDESLIGATWO_fixtures_clone$`CS_2-0` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_2-0`, digits = 3)
BUNDESLIGATWO_fixtures_clone$`CS_0-2` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_0-2`, digits = 3)
BUNDESLIGATWO_fixtures_clone$`CS_2-1` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_2-1`, digits = 3)
BUNDESLIGATWO_fixtures_clone$`CS_1-2` <- round(1/BUNDESLIGATWO_fixtures_clone$`CS_1-2`, digits = 3)

colnames(BUNDESLIGATWO_fixtures_clone)[1] <- "league"
colnames(BUNDESLIGATWO_fixtures_clone)[2] <- "Hometeam"
colnames(BUNDESLIGATWO_fixtures_clone)[3] <- "Awayteam"
colnames(BUNDESLIGATWO_fixtures_clone)[92] <- "predscore"
colnames(BUNDESLIGATWO_fixtures_clone)[64] <- "ov25"
colnames(BUNDESLIGATWO_fixtures_clone)[66] <- "ov25odds"
colnames(BUNDESLIGATWO_fixtures_clone)[65] <- "un25"
colnames(BUNDESLIGATWO_fixtures_clone)[67] <- "un25odds"
colnames(BUNDESLIGATWO_fixtures_clone)[68] <- "BTTSY"
colnames(BUNDESLIGATWO_fixtures_clone)[69] <- "BTTSN"
colnames(BUNDESLIGATWO_fixtures_clone)[70] <- "BTTSYodds"
colnames(BUNDESLIGATWO_fixtures_clone)[71] <- "BTTSNodds"

BUNDESLIGATWO_fixtures_clone <- BUNDESLIGATWO_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
BUNDESLIGATWO_fixtures_clone$matchid <- paste(BUNDESLIGATWO_fixtures_clone$Hometeam,BUNDESLIGATWO_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
BUNDESLIGATWO_fixtures_clone_final <- BUNDESLIGATWO_fixtures_clone[,-c(8,9,10,27)]
BUNDESLIGATWO_fixtures_clone_final[,'sep'] <- ''

bundesligatwo_dmprediction <-  bundesligatwo_picks[,c(4,5,6,7,8)]
bundesligatwo_dmprediction[,'sep2'] <- ''

bundesligatwo_avgyellow <- bundesligatwo_picks[,c(9,10)]
bundesligatwo_avgyellow[,'sep3'] <- ''

bundesligatwo_avgcorners <- bundesligatwo_picks[,c(11,12)]
bundesligatwo_avgcorners[,'sep4'] <- ''

bundesligatwo_goals <- BUNDESLIGATWO_fixtures[,c(10,11)]
bundesligatwo_goals$bundesligatwo_xGH <- round(bundesligatwo_goals$bundesligatwo_xGH, digits = 2)
bundesligatwo_goals$bundesligatwo_xGA <- round(bundesligatwo_goals$bundesligatwo_xGA, digits = 2)
bundesligatwo_goals$bundesligatwo_TxG <- bundesligatwo_goals$bundesligatwo_xGH + bundesligatwo_goals$bundesligatwo_xGA
bundesligatwo_goals[,'sep5'] <- ''

bundesligatwo_shots <- BUNDESLIGATWO_fixtures_sot[,c(10,11)]
bundesligatwo_shots$bundesligatwo_xHST <- round(bundesligatwo_shots$bundesligatwo_xHST, digits = 2)
bundesligatwo_shots$bundesligatwo_xAST <- round(bundesligatwo_shots$bundesligatwo_xAST, digits = 2)
bundesligatwo_shots$TxSOT <- bundesligatwo_shots$bundesligatwo_xHST + bundesligatwo_shots$bundesligatwo_xAST
bundesligatwo_shots[,'sep6'] <- ''

bundesligatwo_fouls <- BUNDESLIGATWO_fixtures_fo[,c(10,11)]
bundesligatwo_fouls$bundesligatwo_xHF <- round(bundesligatwo_fouls$bundesligatwo_xHF, digits = 2)
bundesligatwo_fouls$bundesligatwo_xAF <- round(bundesligatwo_fouls$bundesligatwo_xAF, digits = 2)
bundesligatwo_fouls$bundesligatwo_TxF <- bundesligatwo_fouls$bundesligatwo_xHF + bundesligatwo_fouls$bundesligatwo_xAF

bundesligatwo_ycpf <- bundesligatwo_picks[,c(15,16)]
bundesligatwo_fouls <- cbind(bundesligatwo_fouls,bundesligatwo_ycpf)
bundesligatwo_fouls$HYCPF <- as.numeric(bundesligatwo_fouls$HYCPF)
bundesligatwo_fouls$AYCPF <- as.numeric(bundesligatwo_fouls$AYCPF)
bundesligatwo_fouls$x_hyc <- (bundesligatwo_fouls$bundesligatwo_xHF) * (bundesligatwo_fouls$HYCPF)
bundesligatwo_fouls$x_ayc <- (bundesligatwo_fouls$bundesligatwo_xAF) * (bundesligatwo_fouls$AYCPF)
bundesligatwo_fouls$x_TYC <- round((bundesligatwo_fouls$x_hyc + bundesligatwo_fouls$x_ayc),digits = 2)
bundesligatwo_fouls[,'sep7'] <- ''

bundesligatwo_bookings <- BUNDESLIGATWO_fixtures_yc[,c(10,11)]
bundesligatwo_bookings$bundesligatwo_xHYC <- round(bundesligatwo_bookings$bundesligatwo_xHYC, digits = 2)
bundesligatwo_bookings$bundesligatwo_xAYC <- round(bundesligatwo_bookings$bundesligatwo_xAYC, digits = 2)
bundesligatwo_bookings$bundesligatwo_TYcards <- bundesligatwo_bookings$bundesligatwo_xHYC + bundesligatwo_bookings$bundesligatwo_xAYC
bundesligatwo_bookings[,'sep8'] <- ''

bundesligatwo_corners <- BUNDESLIGATWO_fixtures_co[,c(10,11)]
bundesligatwo_corners$bundesligatwo_xHCOC <- round(bundesligatwo_corners$bundesligatwo_xHCOC, digits = 2)
bundesligatwo_corners$bundesligatwo_xACOC <- round(bundesligatwo_corners$bundesligatwo_xACOC, digits = 2)
bundesligatwo_corners$bundesligatwo_TCOs <- bundesligatwo_corners$bundesligatwo_xHCOC + bundesligatwo_corners$bundesligatwo_xACOC
bundesligatwo_corners[,'sep9'] <- ''

bundesligatwo_shotsconversion <- bundesligatwo_picks[,c(13,14)]
bundesligatwo_shotsconversion <- cbind(bundesligatwo_shotsconversion,bundesligatwo_shots)
bundesligatwo_shotsconversion$HXSC <- as.numeric(bundesligatwo_shotsconversion$HXSC)
bundesligatwo_shotsconversion$AXSC <- as.numeric(bundesligatwo_shotsconversion$AXSC)
bundesligatwo_shotsconversion$bundesligatwo_hXgoals <- round((bundesligatwo_shotsconversion$HXSC * bundesligatwo_shotsconversion$bundesligatwo_xHST), digits = 2)
bundesligatwo_shotsconversion$bundesligatwo_aXgoals <- round((bundesligatwo_shotsconversion$AXSC * bundesligatwo_shotsconversion$bundesligatwo_xAST), digits = 2)
bundesligatwo_shotsconversion$Xgoals <- bundesligatwo_shotsconversion$bundesligatwo_hXgoals + bundesligatwo_shotsconversion$bundesligatwo_aXgoals
options(java.parameters = "-Xmx4g")
BUNDESLIGATWO_all <- cbind(BUNDESLIGATWO_fixtures_clone_final,bundesligatwo_dmprediction,bundesligatwo_avgyellow,bundesligatwo_avgcorners,bundesligatwo_goals,bundesligatwo_shots,bundesligatwo_fouls,bundesligatwo_bookings,bundesligatwo_corners,bundesligatwo_shotsconversion)
unlink('Divisions/BUNDESLIGATWO.xlsx')
write.xlsx(BUNDESLIGATWO_all,'Divisions/BUNDESLIGATWO.xlsx', sheetName = "BUNDESLIGATWO_all", append = TRUE)
write.xlsx(points_bundesligatwo,'Divisions/BUNDESLIGATWO.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(bundesligatwo_cornertotalsv2,'Divisions/BUNDESLIGATWO.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(bundesligatwo_goaltotalsv2,'Divisions/BUNDESLIGATWO.xlsx', sheetName = "Goaltotals", append = TRUE)
write.xlsx(bundesligatwo_yellowtotalsv2,'Divisions/BUNDESLIGATWO.xlsx', sheetName = "Yellowtotals", append = TRUE)


##write.csv(BUNDESLIGATWO_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
