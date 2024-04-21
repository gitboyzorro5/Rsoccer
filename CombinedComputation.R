#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
f1_currentround

#first_df <- SP1_rounds[SP1_rounds$sp1_matchday > 25,]
#second_df <- E0_rounds[E0_rounds$e0_matchday > 26,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#LIGUEONE <- rbind(first_df,second_df)
LIGUEONE <- F1_rounds[F1_rounds$f1_matchday > 22,]
#LIGUEONE <- na.omit(LIGUEONE)
#goaltotals v2
ligueone_goaltotalsv2 <- tapply(LIGUEONE$TG, LIGUEONE[c("HomeTeam", "AwayTeam")],mean)
ligueone_hgtotals <- rowSums(ligueone_goaltotalsv2, na.rm = T)
ligueone_agtotals <- colSums(ligueone_goaltotalsv2, na.rm = T)
ligueone_goaltotalsv2 <- cbind(ligueone_goaltotalsv2,ligueone_hgtotals,ligueone_agtotals)
ligueone_totalgoals <- ligueone_hgtotals + ligueone_agtotals
ligueone_goaltotalsv2 <- cbind(ligueone_goaltotalsv2,ligueone_totalgoals)
ligueone_teams <- sort(unique(LIGUEONE$HomeTeam))
ligueone_home_games <- c()
ligueone_away_games <-c()
for (i_ligueone in 1:length(ligueone_teams))
{

  ligueone_home_games[i_ligueone] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone],])
  ligueone_away_games[i_ligueone]  <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone],])

}
ligueone_games_played <- ligueone_home_games + ligueone_away_games
ligueone_goaltotalsv2 <- cbind(ligueone_goaltotalsv2,ligueone_games_played)
ligueone_avg_totalgoals <- round((ligueone_totalgoals/ ligueone_games_played), digits = 4)
ligueone_goaltotalsv2[is.na(ligueone_goaltotalsv2)] <- ""
ligueone_goaltotalsv2 <- cbind(ligueone_goaltotalsv2,ligueone_avg_totalgoals)

############################################################################################################
#Cornertotals v2
ligueone_cornertotalsv2 <- tapply(LIGUEONE$TC, LIGUEONE[c("HomeTeam", "AwayTeam")],mean)
ligueone_hcototals <- rowSums(ligueone_cornertotalsv2, na.rm = T)
ligueone_acototals <- colSums(ligueone_cornertotalsv2, na.rm = T)
ligueone_cornertotalsv2 <- cbind(ligueone_cornertotalsv2,ligueone_hcototals,ligueone_acototals)
ligueone_totalcorners <- ligueone_hcototals + ligueone_acototals
ligueone_cornertotalsv2 <- cbind(ligueone_cornertotalsv2,ligueone_totalcorners)
ligueone_cornertotalsv2 <- cbind(ligueone_cornertotalsv2,ligueone_games_played)
ligueone_avg_totalcorners <- round((ligueone_totalcorners/ ligueone_games_played), digits = 4)
ligueone_cornertotalsv2[is.na(ligueone_cornertotalsv2)] <- ""
ligueone_cornertotalsv2 <- cbind(ligueone_cornertotalsv2,ligueone_avg_totalcorners)
############################################################################################################
#GS matrix
ligueone_goalscored_h <- tapply(LIGUEONE$FTHG, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_goalscored_a <- tapply(LIGUEONE$FTAG, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_goalscored_h[is.na(ligueone_goalscored_h)] <- ""
ligueone_goalscored_a[is.na(ligueone_goalscored_a)] <- ""
for(ligueone_rowhgs in 1:nrow(ligueone_goalscored_h)) {
  for(ligueone_colhgs in 1:ncol(ligueone_goalscored_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowags in 1:nrow(ligueone_goalscored_a)) {
      for(ligueone_colags in 1:ncol(ligueone_goalscored_a)) {
        ifelse(!ligueone_goalscored_a[ligueone_rowags,ligueone_colags]=="",ligueone_goalscored_h[ligueone_rowags,ligueone_colags] <- ligueone_goalscored_a[ligueone_rowags,ligueone_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
ligueone_goalconceded_h <- tapply(LIGUEONE$FTAG, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_goalconceded_a <- tapply(LIGUEONE$FTHG, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_goalconceded_h[is.na(ligueone_goalconceded_h)] <- ""
ligueone_goalconceded_a[is.na(ligueone_goalconceded_a)] <- ""
for(ligueone_rowhgc in 1:nrow(ligueone_goalconceded_h)) {
  for(ligueone_colhgc in 1:ncol(ligueone_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowagc in 1:nrow(ligueone_goalconceded_a)) {
      for(ligueone_colagc in 1:ncol(ligueone_goalconceded_a)) {
        ifelse(!ligueone_goalconceded_a[ligueone_rowagc,ligueone_colagc]=="",ligueone_goalconceded_h[ligueone_rowagc,ligueone_colagc] <- ligueone_goalconceded_a[ligueone_rowagc,ligueone_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
ligueone_totalcorners_h <- tapply(LIGUEONE$TC, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_totalcorners_a <- tapply(LIGUEONE$TC, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_totalcorners_h[is.na(ligueone_totalcorners_h)] <- ""
ligueone_totalcorners_a[is.na(ligueone_totalcorners_a)] <- ""
#LIGUEONE
for(ligueone_rowTC in 1:nrow(ligueone_totalcorners_h)) {
  for(ligueone_colTC in 1:ncol(ligueone_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowTC in 1:nrow(ligueone_totalcorners_a)) {
      for(ligueone_colTC in 1:ncol(ligueone_totalcorners_a)) {
        ifelse(!ligueone_totalcorners_a[ligueone_rowTC,ligueone_colTC]=="",ligueone_totalcorners_h[ligueone_rowTC,ligueone_colTC] <- ligueone_totalcorners_a[ligueone_rowTC,ligueone_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
ligueone_coawarded_h <- tapply(LIGUEONE$HCO, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_coawarded_a <- tapply(LIGUEONE$ACO, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_coawarded_h[is.na(ligueone_coawarded_h)] <- ""
ligueone_coawarded_a[is.na(ligueone_coawarded_a)] <- ""
#LIGUEONE
for(ligueone_rowhco in 1:nrow(ligueone_coawarded_h)) {
  for(ligueone_colhco in 1:ncol(ligueone_coawarded_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowaco in 1:nrow(ligueone_coawarded_a)) {
      for(ligueone_colaco in 1:ncol(ligueone_coawarded_a)) {
        ifelse(!ligueone_coawarded_a[ligueone_rowaco,ligueone_colaco]=="",ligueone_coawarded_h[ligueone_rowaco,ligueone_colaco] <- ligueone_coawarded_a[ligueone_rowaco,ligueone_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
ligueone_cornersconceded_h <- tapply(LIGUEONE$ACO, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_cornersconceded_a <- tapply(LIGUEONE$HCO, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_cornersconceded_h[is.na(ligueone_cornersconceded_h)] <- ""
ligueone_cornersconceded_a[is.na(ligueone_cornersconceded_a)] <- ""
#LIGUEONE
for(ligueone_rowhcc in 1:nrow(ligueone_cornersconceded_h)) {
  for(ligueone_colhcc in 1:ncol(ligueone_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowacc in 1:nrow(ligueone_cornersconceded_a)) {
      for(ligueone_colacc in 1:ncol(ligueone_cornersconceded_a)) {
        ifelse(!ligueone_cornersconceded_a[ligueone_rowacc,ligueone_colacc]=="",ligueone_cornersconceded_h[ligueone_rowacc,ligueone_colacc] <- ligueone_cornersconceded_a[ligueone_rowacc,ligueone_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
ligueone_coscform_h <- tapply(LIGUEONE$COSC, LIGUEONE[c("HomeTeam", "Date")],median)
ligueone_coscform_a <- tapply(LIGUEONE$COSC, LIGUEONE[c("AwayTeam", "Date")],median)
ligueone_coscform_h[is.na(ligueone_coscform_h)] <- ""
ligueone_coscform_a[is.na(ligueone_coscform_a)] <- ""
#LIGUEONE
for(ligueone_rowh_f_cosc in 1:nrow(ligueone_coscform_h)) {
  for(ligueone_colh_f_cosc in 1:ncol(ligueone_coscform_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowa_f_cosc in 1:nrow(ligueone_coscform_a)) {
      for(ligueone_cola_f_cosc in 1:ncol(ligueone_coscform_a)) {
        ifelse(!ligueone_coscform_a[ligueone_rowa_f_cosc,ligueone_cola_f_cosc]=="",ligueone_coscform_h[ligueone_rowa_f_cosc,ligueone_cola_f_cosc] <- ligueone_coscform_a[ligueone_rowa_f_cosc,ligueone_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
ligueone_winmargin_h <- tapply(LIGUEONE$FTHG - LIGUEONE$FTAG, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_winmargin_a <- tapply(LIGUEONE$FTAG - LIGUEONE$FTHG, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_winmargin_h[is.na(ligueone_winmargin_h)] <- ""
ligueone_winmargin_a[is.na(ligueone_winmargin_a)] <- ""
#LIGUEONE
for(ligueone_rowhwm in 1:nrow(ligueone_winmargin_h)) {
  for(ligueone_colhwm in 1:ncol(ligueone_winmargin_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowawm in 1:nrow(ligueone_winmargin_a)) {
      for(ligueone_colawm in 1:ncol(ligueone_winmargin_a)) {
        ifelse(!ligueone_winmargin_a[ligueone_rowawm,ligueone_colawm]=="",ligueone_winmargin_h[ligueone_rowawm,ligueone_colawm] <- ligueone_winmargin_a[ligueone_rowawm,ligueone_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
ligueone_yellowscored_h <- tapply(LIGUEONE$HY, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_yellowscored_a <- tapply(LIGUEONE$AY, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_yellowscored_h[is.na(ligueone_yellowscored_h)] <- ""
ligueone_yellowscored_a[is.na(ligueone_yellowscored_a)] <- ""
#LIGUEONE
for(ligueone_rowhys in 1:nrow(ligueone_yellowscored_h)) {
  for(ligueone_colhys in 1:ncol(ligueone_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(ligueone_roways in 1:nrow(ligueone_yellowscored_a)) {
      for(ligueone_colays in 1:ncol(ligueone_yellowscored_a)) {
        ifelse(!ligueone_yellowscored_a[ligueone_roways,ligueone_colays]=="",ligueone_yellowscored_h[ligueone_roways,ligueone_colays] <- ligueone_yellowscored_a[ligueone_roways,ligueone_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
ligueone_redscored_h <- tapply(LIGUEONE$HR, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_redscored_a <- tapply(LIGUEONE$AR, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_redscored_h[is.na(ligueone_redscored_h)] <- ""
ligueone_redscored_a[is.na(ligueone_redscored_a)] <- ""
for(ligueone_rowhrs in 1:nrow(ligueone_redscored_h)) {
  for(ligueone_colhrs in 1:ncol(ligueone_redscored_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowars in 1:nrow(ligueone_redscored_a)) {
      for(ligueone_colars in 1:ncol(ligueone_redscored_a)) {
        ifelse(!ligueone_redscored_a[ligueone_rowars,ligueone_colars]=="",ligueone_redscored_h[ligueone_rowars,ligueone_colars] <- ligueone_redscored_a[ligueone_rowars,ligueone_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
ligueone_redtotalsv2 <- tapply(LIGUEONE$TR, LIGUEONE[c("HomeTeam", "AwayTeam")],mean)
ligueone_hrtotals <- rowSums(ligueone_redtotalsv2, na.rm = T)
ligueone_artotals <- colSums(ligueone_redtotalsv2, na.rm = T)
ligueone_redtotalsv2 <- cbind(ligueone_redtotalsv2,ligueone_hrtotals,ligueone_artotals)
ligueone_totalreds <- ligueone_hrtotals + ligueone_artotals
ligueone_redtotalsv2 <- cbind(ligueone_redtotalsv2,ligueone_totalreds)
ligueone_redtotalsv2 <- cbind(ligueone_redtotalsv2,ligueone_games_played)
ligueone_avg_totalreds <- round((ligueone_totalreds/ ligueone_games_played), digits = 4)
ligueone_redtotalsv2[is.na(ligueone_redtotalsv2)] <- ""
ligueone_redtotalsv2 <- cbind(ligueone_redtotalsv2,ligueone_avg_totalreds)
############################################################################################################################################################
#yellowtotals
ligueone_yellowtotalsv2 <- tapply(LIGUEONE$TY, LIGUEONE[c("HomeTeam", "AwayTeam")],mean)
ligueone_hytotals <- rowSums(ligueone_yellowtotalsv2, na.rm = T)
ligueone_aytotals <- colSums(ligueone_yellowtotalsv2, na.rm = T)
ligueone_yellowtotalsv2 <- cbind(ligueone_yellowtotalsv2,ligueone_hytotals,ligueone_aytotals)
ligueone_totalyellows <- ligueone_hytotals + ligueone_aytotals
ligueone_yellowtotalsv2 <- cbind(ligueone_yellowtotalsv2,ligueone_totalyellows)
ligueone_yellowtotalsv2 <- cbind(ligueone_yellowtotalsv2,ligueone_games_played)
ligueone_avg_totalyellows <- round((ligueone_totalyellows/ ligueone_games_played), digits = 4)
ligueone_yellowtotalsv2[is.na(ligueone_yellowtotalsv2)] <- ""
ligueone_yellowtotalsv2 <- cbind(ligueone_yellowtotalsv2,ligueone_avg_totalyellows)
##################################################################################################################################################
#team form
ligueone_form_h <- tapply(LIGUEONE$FTR, LIGUEONE[c("HomeTeam", "Date")],median)
ligueone_form_a <- tapply(LIGUEONE$FTR, LIGUEONE[c("AwayTeam", "Date")],median)
ligueone_form_h[is.na(ligueone_form_h)] <- ""
ligueone_form_a[is.na(ligueone_form_a)] <- ""
ligueone_form_h <- sub("A","L",ligueone_form_h)
ligueone_form_h <- sub("H","W",ligueone_form_h)
ligueone_form_a <- sub("A","W",ligueone_form_a)
ligueone_form_a <- sub("H","L",ligueone_form_a)
for(ligueone_rowh_f in 1:nrow(ligueone_form_h)) {
  for(ligueone_colh_f in 1:ncol(ligueone_form_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowa_f in 1:nrow(ligueone_form_a)) {
      for(ligueone_cola_f in 1:ncol(ligueone_form_a)) {
        ifelse(!ligueone_form_a[ligueone_rowa_f,ligueone_cola_f]=="",ligueone_form_h[ligueone_rowa_f,ligueone_cola_f] <- ligueone_form_a[ligueone_rowa_f,ligueone_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
ligueone_csform_h <- tapply(LIGUEONE$CS, LIGUEONE[c("HomeTeam", "Date")],median)
ligueone_csform_a <- tapply(LIGUEONE$CS, LIGUEONE[c("AwayTeam", "Date")],median)
ligueone_csform_h[is.na(ligueone_csform_h)] <- ""
ligueone_csform_a[is.na(ligueone_csform_a)] <- ""
#LIGUEONE
for(ligueone_rowh_f_cs in 1:nrow(ligueone_csform_h)) {
  for(ligueone_colh_f_cs in 1:ncol(ligueone_csform_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowa_f_cs in 1:nrow(ligueone_csform_a)) {
      for(ligueone_cola_f_cs in 1:ncol(ligueone_csform_a)) {
        ifelse(!ligueone_csform_a[ligueone_rowa_f_cs,ligueone_cola_f_cs]=="",ligueone_csform_h[ligueone_rowa_f_cs,ligueone_cola_f_cs] <- ligueone_csform_a[ligueone_rowa_f_cs,ligueone_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
ligueone_totalgoals_h <- tapply(LIGUEONE$TG, LIGUEONE[c("HomeTeam", "Date")],mean)
ligueone_totalgoals_a <- tapply(LIGUEONE$TG, LIGUEONE[c("AwayTeam", "Date")],mean)
ligueone_totalgoals_h[is.na(ligueone_totalgoals_h)] <- ""
ligueone_totalgoals_a[is.na(ligueone_totalgoals_a)] <- ""
for(ligueone_rowh in 1:nrow(ligueone_totalgoals_h)) {
  for(ligueone_colh in 1:ncol(ligueone_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowa in 1:nrow(ligueone_totalgoals_a)) {
      for(ligueone_cola in 1:ncol(ligueone_totalgoals_a)) {
        ifelse(!ligueone_totalgoals_a[ligueone_rowa,ligueone_cola]=="",ligueone_totalgoals_h[ligueone_rowa,ligueone_cola] <- ligueone_totalgoals_a[ligueone_rowa,ligueone_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#LIGUEONE
ligueone_un05_home <- c()
ligueone_un05_away <- c()
ligueone_ov05_home <- c()
ligueone_ov05_away <- c()

ligueone_un15_home <- c()
ligueone_un15_away <- c()
ligueone_ov15_home <- c()
ligueone_ov15_away <- c()

ligueone_un25_home <- c()
ligueone_un25_away <- c()
ligueone_ov25_home <- c()
ligueone_ov25_away <- c()

ligueone_un35_home <- c()
ligueone_un35_away <- c()
ligueone_ov35_home <- c()
ligueone_ov35_away <- c()

ligueone_un45_home <- c()
ligueone_un45_away <- c()
ligueone_ov45_home <- c()
ligueone_ov45_away <- c()

ligueone_un55_home <- c()
ligueone_un55_away <- c()
ligueone_ov55_home <- c()
ligueone_ov55_away <- c()

for (i_ligueone_tg in 1:length(ligueone_teams))
{

  ligueone_un05_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG == 0,])
  ligueone_un05_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG == 0,])

  ligueone_ov05_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG > 0,])
  ligueone_ov05_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG > 0,])

  ligueone_un15_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 1,])
  ligueone_un15_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 1,])

  ligueone_ov15_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 2,])
  ligueone_ov15_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 2,])

  ligueone_un25_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 2,])
  ligueone_un25_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 2,])

  ligueone_ov25_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >=3,])
  ligueone_ov25_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >=3,])

  ligueone_un35_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 3,])
  ligueone_un35_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 3,])

  ligueone_ov35_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 4,])
  ligueone_ov35_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 4,])

  ligueone_un45_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 4,])
  ligueone_un45_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 4,])

  ligueone_ov45_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 5,])
  ligueone_ov45_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 5,])

  ligueone_un55_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 5,])
  ligueone_un55_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG <= 5,])

  ligueone_ov55_home[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 6,])
  ligueone_ov55_away[i_ligueone_tg] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_tg] & LIGUEONE$TG >= 6,])


}

ligueone_un05 <- ligueone_un05_home + ligueone_un05_away
ligueone_ov05 <- ligueone_ov05_home + ligueone_ov05_away

ligueone_un15 <- ligueone_un15_home + ligueone_un15_away
ligueone_ov15 <- ligueone_ov15_home + ligueone_ov15_away

ligueone_un25 <- ligueone_un25_home + ligueone_un25_away
ligueone_ov25 <- ligueone_ov25_home + ligueone_ov25_away

ligueone_un35 <- ligueone_un35_home + ligueone_un35_away
ligueone_ov35 <- ligueone_ov35_home + ligueone_ov35_away

ligueone_un45 <- ligueone_un45_home + ligueone_un45_away
ligueone_ov45 <- ligueone_ov45_home + ligueone_ov45_away

ligueone_un55 <- ligueone_un55_home + ligueone_un55_away
ligueone_ov55 <- ligueone_ov55_home + ligueone_ov55_away

ligueone_ovundata <- cbind(ligueone_teams,ligueone_un05,ligueone_ov05,ligueone_un15,ligueone_ov15,ligueone_un25,ligueone_ov25,ligueone_un35,ligueone_ov35,ligueone_un45,ligueone_ov45,ligueone_un55,ligueone_ov55)
#################################################################################################################################################################
#team against
ligueone_form_team_against_h <- tapply(LIGUEONE$AwayTeam, LIGUEONE[c("HomeTeam", "Date")],median)
ligueone_form_team_against_a <- tapply(LIGUEONE$HomeTeam, LIGUEONE[c("AwayTeam", "Date")],median)
ligueone_form_team_against_h[is.na(ligueone_form_team_against_h)] <- ""
ligueone_form_team_against_a[is.na(ligueone_form_team_against_a)] <- ""
#LIGUEONE
for(ligueone_rowh_f_against in 1:nrow(ligueone_form_team_against_h)) {
  for(ligueone_colh_f_against in 1:ncol(ligueone_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(ligueone_rowa_f_against in 1:nrow(ligueone_form_team_against_a)) {
      for(ligueone_cola_f_against in 1:ncol(ligueone_form_team_against_a)) {
        ifelse(!ligueone_form_team_against_a[ligueone_rowa_f_against,ligueone_cola_f_against]=="",ligueone_form_team_against_h[ligueone_rowa_f_against,ligueone_cola_f_against] <- ligueone_form_team_against_a[ligueone_rowa_f_against,ligueone_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#LIGUEONE
#home goals scored
ligueone_home_gs <- aggregate(LIGUEONE$FTHG, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_home_gs_avg <- aggregate(LIGUEONE$FTHG, by = list(LIGUEONE$HomeTeam),mean)
ligueone_home_scoring <- merge(ligueone_home_gs,ligueone_home_gs_avg, by='Group.1',all = T)
names(ligueone_home_scoring)[names(ligueone_home_scoring) == "x.x"] <- "TFthg"
names(ligueone_home_scoring)[names(ligueone_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
ligueone_away_gs <- aggregate(LIGUEONE$FTAG, by = list(LIGUEONE$AwayTeam), FUN = sum)
ligueone_away_gs_avg <- aggregate(LIGUEONE$FTAG, by = list(LIGUEONE$AwayTeam),mean)
ligueone_away_scoring <- merge(ligueone_away_gs,ligueone_away_gs_avg, by='Group.1',all = T)
names(ligueone_away_scoring)[names(ligueone_away_scoring) == "x.x"] <- "TFtag"
names(ligueone_away_scoring)[names(ligueone_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
ligueone_scoring <- merge(ligueone_home_scoring,ligueone_away_scoring,by='Group.1',all = T)
ligueone_scoring$TGS <- ligueone_scoring$TFthg + ligueone_scoring$TFtag

#Home shots on target
ligueone_home_hst <- aggregate(LIGUEONE$HST, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_ast <- aggregate(LIGUEONE$AST, by = list(LIGUEONE$AwayTeam), FUN = sum)
ligueone_tst <- merge(ligueone_home_hst,ligueone_away_ast, by='Group.1',all = T)
names(ligueone_tst)[names(ligueone_tst) == "x.x"] <- "hst"
names(ligueone_tst)[names(ligueone_tst) == "x.y"] <- "ast"
ligueone_tst$TST <- ligueone_tst$hst + ligueone_tst$ast
#merge goals scored and shots on target
ligueone_scoring_conversion <- merge(ligueone_tst,ligueone_scoring,by='Group.1',all = T)
#add HSC ASC TSC
ligueone_scoring_conversion$HSTC <- percent(ligueone_scoring_conversion$TFthg/ligueone_scoring_conversion$hst, accuracy = 0.01)
ligueone_scoring_conversion$ASTC <- percent(ligueone_scoring_conversion$TFtag/ligueone_scoring_conversion$ast, accuracy = 0.01)
ligueone_scoring_conversion$TSTC <- percent(ligueone_scoring_conversion$TGS/ligueone_scoring_conversion$TST, accuracy = 0.01)
#merge games played
ligueone_scoring_conversion <- cbind(ligueone_scoring_conversion,ligueone_games_played)
#create the second part
#home goals conceded
ligueone_home_gc <- aggregate(LIGUEONE$FTAG, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_home_gc_avg <- aggregate(LIGUEONE$FTAG, by = list(LIGUEONE$HomeTeam),mean)
ligueone_home_conceding <- merge(ligueone_home_gc,ligueone_home_gc_avg, by='Group.1',all = T)
names(ligueone_home_conceding)[names(ligueone_home_conceding) == "x.x"] <- "TFthc"
names(ligueone_home_conceding)[names(ligueone_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
ligueone_away_gc <- aggregate(LIGUEONE$FTHG, by = list(LIGUEONE$AwayTeam), FUN = sum)
ligueone_away_gc_avg <- aggregate(LIGUEONE$FTHG, by = list(LIGUEONE$AwayTeam),mean)
ligueone_away_conceding <- merge(ligueone_away_gc,ligueone_away_gc_avg, by='Group.1',all = T)
names(ligueone_away_conceding)[names(ligueone_away_conceding) == "x.x"] <- "TFtac"
names(ligueone_away_conceding)[names(ligueone_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
ligueone_conceding <- merge(ligueone_home_conceding,ligueone_away_conceding,by='Group.1',all = T)
ligueone_conceding$TGC <- ligueone_conceding$TFthc + ligueone_conceding$TFtac
ligueone_home_hst
#Home shots conceded
ligueone_home_hsc <- aggregate(LIGUEONE$AST, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_asc <- aggregate(LIGUEONE$HST, by = list(LIGUEONE$AwayTeam), FUN = sum)
ligueone_tsc <- merge(ligueone_home_hsc,ligueone_away_asc, by='Group.1',all = T)
names(ligueone_tsc)[names(ligueone_tsc) == "x.x"] <- "hsc"
names(ligueone_tsc)[names(ligueone_tsc) == "x.y"] <- "asc"
ligueone_tsc$TSC <- ligueone_tsc$hsc + ligueone_tsc$asc
#merge goals conceded and shots conceded
ligueone_conceding_conversion <- merge(ligueone_tsc,ligueone_conceding,by='Group.1',all = T)

#add HSC ASC TSC
ligueone_conceding_conversion$HSCC <- percent(ligueone_conceding_conversion$TFthc/ligueone_conceding_conversion$hsc, accuracy = 0.01)
ligueone_conceding_conversion$ASCC <- percent(ligueone_conceding_conversion$TFtac/ligueone_conceding_conversion$asc, accuracy = 0.01)
ligueone_conceding_conversion$TSCC <- percent(ligueone_conceding_conversion$TGC/ligueone_conceding_conversion$TSC, accuracy = 0.01)
ligueone_conceding_conversion$XSTC <- round(ligueone_scoring$TGS/(ligueone_tst$TST - ligueone_scoring$TGS), digits = 2)

#merge the two parts
ligueone_shots_analysis <- merge(ligueone_scoring_conversion,ligueone_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#LIGUEONE
#home fouls for
ligueone_home_fouls <- aggregate(LIGUEONE$HF, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_home_fouls_avg <- aggregate(LIGUEONE$HF, by = list(LIGUEONE$HomeTeam),mean)
ligueone_home_foulsdata <- merge(ligueone_home_fouls,ligueone_home_fouls_avg, by='Group.1',all = T)
names(ligueone_home_foulsdata)[names(ligueone_home_foulsdata) == "x.x"] <- "THfouls"
names(ligueone_home_foulsdata)[names(ligueone_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
ligueone_away_fouls <- aggregate(LIGUEONE$HF, by = list(LIGUEONE$AwayTeam), FUN = sum)
ligueone_away_fouls_avg <- aggregate(LIGUEONE$HF, by = list(LIGUEONE$AwayTeam),mean)
ligueone_away_foulsdata <- merge(ligueone_away_fouls,ligueone_away_fouls_avg, by='Group.1',all = T)
names(ligueone_away_foulsdata)[names(ligueone_away_foulsdata) == "x.x"] <- "TAfouls"
names(ligueone_away_foulsdata)[names(ligueone_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
ligueone_fouls <- merge(ligueone_home_foulsdata,ligueone_away_foulsdata,by='Group.1',all = T)
ligueone_fouls$TotalFouls <- ligueone_fouls$THfouls + ligueone_fouls$TAfouls

#yellow cards
ligueone_home_hyc <- aggregate(LIGUEONE$HY, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_ayc <- aggregate(LIGUEONE$AY, by = list(LIGUEONE$AwayTeam), FUN = sum)
ligueone_tyc <- merge(ligueone_home_hyc,ligueone_away_ayc, by='Group.1',all = T)
names(ligueone_tyc)[names(ligueone_tyc) == "x.x"] <- "hyc"
names(ligueone_tyc)[names(ligueone_tyc) == "x.y"] <- "ayc"
ligueone_tyc$TotalYellows <- ligueone_tyc$hyc + ligueone_tyc$ayc

#merge fouls for and yellow cards
ligueone_fouls_conversion <- merge(ligueone_tyc,ligueone_fouls,by='Group.1',all = T)
ligueone_fouls_conversion$YcPerfoul <- round((ligueone_fouls_conversion$TotalYellows/ligueone_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
LIGUEONE$Div <- "LIGUEONE"
##
###################################################################################################################################################
#poisson cards
ligueone_GP <- nrow(LIGUEONE)
#Calculate total home goals for each division
ligueone_T_HY <- sum(ligueone_home_hyc$x)
#calculate average home goal
ligueone_avg_HY <- round(ligueone_T_HY /ligueone_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ligueone_T_AY <- sum(ligueone_away_ayc$x)
#calculate average away goal
ligueone_avg_AY <- round(ligueone_T_AY /ligueone_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ligueone_home_yas <- round(((ligueone_home_hyc$x/ligueone_home_games))/ligueone_avg_HY, digits = 4)
#calculate away attack strength
ligueone_away_yas <- round(((ligueone_away_ayc$x/ligueone_away_games))/ligueone_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
ligueone_avg_HYC <- round(ligueone_T_AY /ligueone_GP, digits = 4)
#avg away concede
ligueone_avg_AYC <- round(ligueone_T_HY /ligueone_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
ligueone_home_ycc <- aggregate(LIGUEONE$AY, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_ycc <- aggregate(LIGUEONE$HY, by = list(LIGUEONE$AwayTeam), FUN = sum)
#home defense strength
ligueone_home_yds <- round(((ligueone_home_ycc$x/ligueone_home_games))/ligueone_avg_HYC, digits = 4)
#away defense strength
ligueone_away_yds <- round(((ligueone_away_ycc$x/ligueone_away_games))/ligueone_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_home_poisson_yc <- cbind(ligueone_division,ligueone_teams,ligueone_avg_HY,ligueone_home_yas,ligueone_home_yds)
#away poisson data
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_away_poisson_yc <- cbind(ligueone_division,ligueone_teams,ligueone_avg_AY,ligueone_away_yas,ligueone_away_yds)
###
HomeTeam_ligueone_yc <- rep(ligueone_teams, each = length(ligueone_teams))
AwayTeam_ligueone_yc <- rep(ligueone_teams, length(ligueone_teams))
LIGUEONE_fixtures_yc <- cbind(HomeTeam_ligueone_yc,AwayTeam_ligueone_yc)
LIGUEONE_fixtures_yc <- as.data.frame(LIGUEONE_fixtures_yc)
LIGUEONE_fixtures_yc <- LIGUEONE_fixtures_yc[!LIGUEONE_fixtures_yc$HomeTeam_ligueone_yc == LIGUEONE_fixtures_yc$AwayTeam_ligueone_yc,]
rownames(LIGUEONE_fixtures_yc) <- NULL
LIGUEONE_fixtures_yc$Div <- "LIGUEONE"
LIGUEONE_fixtures_yc <- LIGUEONE_fixtures_yc[,c(3,1,2)]

LIGUEONE_fixtures_yc$avg_HY_ligueone <- ligueone_avg_HY

LIGUEONE_fixtures_yc$ligueone_homeyas <- rep(ligueone_home_yas,each = length(ligueone_teams)-1)

ligueone_awayyds_lookup <- cbind(ligueone_teams,ligueone_away_yds)

ligueone_awayyds_lookup <- as.data.frame(ligueone_awayyds_lookup)

colnames(ligueone_awayyds_lookup) <- c("AwayTeam_ligueone_yc","ligueone_awayyds")


require('RH2')
LIGUEONE_fixtures_yc$ligueone_awayyds <- sqldf("SELECT ligueone_awayyds_lookup.ligueone_awayyds FROM ligueone_awayyds_lookup INNER JOIN LIGUEONE_fixtures_yc ON ligueone_awayyds_lookup.AwayTeam_ligueone_yc = LIGUEONE_fixtures_yc.AwayTeam_ligueone_yc")

LIGUEONE_fixtures_yc$avg_AY_ligueone <- ligueone_avg_AY

ligueone_awayyas_lookup <- cbind(ligueone_teams,ligueone_away_yas)

ligueone_awayyas_lookup <- as.data.frame(ligueone_awayyas_lookup)

colnames(ligueone_awayyas_lookup) <- c("AwayTeam_ligueone_yc","ligueone_awayyas")

LIGUEONE_fixtures_yc$ligueone_awayyas <- sqldf("SELECT ligueone_awayyas_lookup.ligueone_awayyas FROM ligueone_awayyas_lookup INNER JOIN LIGUEONE_fixtures_yc ON ligueone_awayyas_lookup.AwayTeam_ligueone_yc = LIGUEONE_fixtures_yc.AwayTeam_ligueone_yc")

LIGUEONE_fixtures_yc$ligueone_homeyds <- rep(ligueone_home_yds,each = length(ligueone_teams)-1)

LIGUEONE_fixtures_yc$ligueone_awayyds <- as.numeric(unlist(LIGUEONE_fixtures_yc$ligueone_awayyds))
#xGH
LIGUEONE_fixtures_yc$ligueone_xHYC <- LIGUEONE_fixtures_yc$avg_HY_ligueone * LIGUEONE_fixtures_yc$ligueone_homeyas * LIGUEONE_fixtures_yc$ligueone_awayyds
#xGA

LIGUEONE_fixtures_yc$ligueone_awayyas <- as.numeric(unlist(LIGUEONE_fixtures_yc$ligueone_awayyas))

LIGUEONE_fixtures_yc$ligueone_xAYC <- LIGUEONE_fixtures_yc$avg_AY_ligueone * LIGUEONE_fixtures_yc$ligueone_awayyas * LIGUEONE_fixtures_yc$ligueone_homeyds

LIGUEONE_fixtures_yc$ligueone_0_0 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_0 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_0_1 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_1 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_0 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_0_2 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_2 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_1 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_2 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_3 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_0 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_1 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_2 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_0_3 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_3 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_3 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_4 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_0 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_1 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_2 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_3 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_0_4 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_4 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_4 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_4 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_5 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_0 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_1 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_2 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_3 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_4 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_0_5 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_5 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_5 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_5 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_5 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_6 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_0 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_1 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_2 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_3 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_4 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_6_5 <- round(stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_0_6 <- round(stats::dpois(0,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_1_6 <- round(stats::dpois(1,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_2_6 <- round(stats::dpois(2,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_3_6 <- round(stats::dpois(3,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_4_6 <- round(stats::dpois(4,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
LIGUEONE_fixtures_yc$ligueone_5_6 <- round(stats::dpois(5,LIGUEONE_fixtures_yc$ligueone_xHYC) * stats::dpois(6,LIGUEONE_fixtures_yc$ligueone_xAYC), digits = 4)
#Home win
LIGUEONE_fixtures_yc$ligueone_H <- (
  LIGUEONE_fixtures_yc$ligueone_1_0 + LIGUEONE_fixtures_yc$ligueone_2_0 + LIGUEONE_fixtures_yc$ligueone_2_1 + LIGUEONE_fixtures_yc$ligueone_3_0 + LIGUEONE_fixtures_yc$ligueone_3_1 +
    LIGUEONE_fixtures_yc$ligueone_3_2 + LIGUEONE_fixtures_yc$ligueone_4_0 + LIGUEONE_fixtures_yc$ligueone_4_1 + LIGUEONE_fixtures_yc$ligueone_4_2 + LIGUEONE_fixtures_yc$ligueone_4_3 +
    LIGUEONE_fixtures_yc$ligueone_5_0 + LIGUEONE_fixtures_yc$ligueone_5_1 + LIGUEONE_fixtures_yc$ligueone_5_2 + LIGUEONE_fixtures_yc$ligueone_5_3 + LIGUEONE_fixtures_yc$ligueone_5_4 +
    LIGUEONE_fixtures_yc$ligueone_6_0 + LIGUEONE_fixtures_yc$ligueone_6_1 + LIGUEONE_fixtures_yc$ligueone_6_2 + LIGUEONE_fixtures_yc$ligueone_6_3 + LIGUEONE_fixtures_yc$ligueone_6_4 +
    LIGUEONE_fixtures_yc$ligueone_6_5
)

LIGUEONE_fixtures_yc$ligueone_H <- percent(LIGUEONE_fixtures_yc$ligueone_H, accuracy = 0.1)

#Draw
LIGUEONE_fixtures_yc$ligueone_D <- (

  LIGUEONE_fixtures_yc$ligueone_0_0 + LIGUEONE_fixtures_yc$ligueone_1_1 + LIGUEONE_fixtures_yc$ligueone_2_2 + LIGUEONE_fixtures_yc$ligueone_3_3 + LIGUEONE_fixtures_yc$ligueone_4_4 +
    LIGUEONE_fixtures_yc$ligueone_5_5 + LIGUEONE_fixtures_yc$ligueone_6_6
)

LIGUEONE_fixtures_yc$ligueone_D <- percent(LIGUEONE_fixtures_yc$ligueone_D, accuracy = 0.1)

#Away

LIGUEONE_fixtures_yc$ligueone_A <- (
  LIGUEONE_fixtures_yc$ligueone_0_1 + LIGUEONE_fixtures_yc$ligueone_0_2 + LIGUEONE_fixtures_yc$ligueone_1_2 + LIGUEONE_fixtures_yc$ligueone_0_3 + LIGUEONE_fixtures_yc$ligueone_1_3 +
    LIGUEONE_fixtures_yc$ligueone_2_3 + LIGUEONE_fixtures_yc$ligueone_0_4 + LIGUEONE_fixtures_yc$ligueone_1_4 + LIGUEONE_fixtures_yc$ligueone_2_4 + LIGUEONE_fixtures_yc$ligueone_3_4 +
    LIGUEONE_fixtures_yc$ligueone_0_5 + LIGUEONE_fixtures_yc$ligueone_1_5 + LIGUEONE_fixtures_yc$ligueone_2_5 + LIGUEONE_fixtures_yc$ligueone_3_5 + LIGUEONE_fixtures_yc$ligueone_4_5 +
    LIGUEONE_fixtures_yc$ligueone_0_6 + LIGUEONE_fixtures_yc$ligueone_1_6 + LIGUEONE_fixtures_yc$ligueone_2_6 + LIGUEONE_fixtures_yc$ligueone_3_6 + LIGUEONE_fixtures_yc$ligueone_4_6 +
    LIGUEONE_fixtures_yc$ligueone_5_6
)

LIGUEONE_fixtures_yc$ligueone_A <- percent(LIGUEONE_fixtures_yc$ligueone_A, accuracy = 0.1)

#ov25
LIGUEONE_fixtures_yc$ligueone_ov25 <- (
  LIGUEONE_fixtures_yc$ligueone_2_1 + LIGUEONE_fixtures_yc$ligueone_1_2 + LIGUEONE_fixtures_yc$ligueone_2_2 + LIGUEONE_fixtures_yc$ligueone_3_0 + LIGUEONE_fixtures_yc$ligueone_3_1 +
    LIGUEONE_fixtures_yc$ligueone_3_2 + LIGUEONE_fixtures_yc$ligueone_0_3 + LIGUEONE_fixtures_yc$ligueone_1_3 + LIGUEONE_fixtures_yc$ligueone_2_3 + LIGUEONE_fixtures_yc$ligueone_3_3 +
    LIGUEONE_fixtures_yc$ligueone_4_0 + LIGUEONE_fixtures_yc$ligueone_4_1 + LIGUEONE_fixtures_yc$ligueone_4_2 + LIGUEONE_fixtures_yc$ligueone_4_3 + LIGUEONE_fixtures_yc$ligueone_0_4 +
    LIGUEONE_fixtures_yc$ligueone_1_4 + LIGUEONE_fixtures_yc$ligueone_2_4 + LIGUEONE_fixtures_yc$ligueone_3_4 + LIGUEONE_fixtures_yc$ligueone_4_4 + LIGUEONE_fixtures_yc$ligueone_5_0 +
    LIGUEONE_fixtures_yc$ligueone_5_1 + LIGUEONE_fixtures_yc$ligueone_5_2 + LIGUEONE_fixtures_yc$ligueone_5_3 + LIGUEONE_fixtures_yc$ligueone_5_4 + LIGUEONE_fixtures_yc$ligueone_0_5 +
    LIGUEONE_fixtures_yc$ligueone_1_5 + LIGUEONE_fixtures_yc$ligueone_2_5 + LIGUEONE_fixtures_yc$ligueone_3_5 + LIGUEONE_fixtures_yc$ligueone_4_5 + LIGUEONE_fixtures_yc$ligueone_5_5 +
    LIGUEONE_fixtures_yc$ligueone_6_0 + LIGUEONE_fixtures_yc$ligueone_6_1 + LIGUEONE_fixtures_yc$ligueone_6_2 + LIGUEONE_fixtures_yc$ligueone_6_3 + LIGUEONE_fixtures_yc$ligueone_6_4 +
    LIGUEONE_fixtures_yc$ligueone_6_5 + LIGUEONE_fixtures_yc$ligueone_0_6 + LIGUEONE_fixtures_yc$ligueone_1_6 + LIGUEONE_fixtures_yc$ligueone_2_6 + LIGUEONE_fixtures_yc$ligueone_3_6 +
    LIGUEONE_fixtures_yc$ligueone_4_6 + LIGUEONE_fixtures_yc$ligueone_5_6 + LIGUEONE_fixtures_yc$ligueone_6_6
)
#un25
LIGUEONE_fixtures_yc$ligueone_un25 <- (
  LIGUEONE_fixtures_yc$ligueone_0_0 + LIGUEONE_fixtures_yc$ligueone_1_0 + LIGUEONE_fixtures_yc$ligueone_0_1 + LIGUEONE_fixtures_yc$ligueone_1_1 + LIGUEONE_fixtures_yc$ligueone_2_0 + LIGUEONE_fixtures_yc$ligueone_0_2
)
#odds
LIGUEONE_fixtures_yc$ligueone_ov25_odds <- round((1/LIGUEONE_fixtures_yc$ligueone_ov25),digits = 2)
LIGUEONE_fixtures_yc$ligueone_un25_odds <- round((1/LIGUEONE_fixtures_yc$ligueone_un25),digits = 2)

LIGUEONE_fixtures_yc$ligueone_ov25_odds
LIGUEONE_fixtures_yc$ligueone_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUEONE_fixtures_yc$ligueone_ov25 <- percent(LIGUEONE_fixtures_yc$ligueone_ov25, accuracy = 0.1)

LIGUEONE_fixtures_yc$ligueone_un25 <- percent(LIGUEONE_fixtures_yc$ligueone_un25, accuracy = 0.1)
LIGUEONE_fixtures_yc$ligueone_pscore <- paste(round(LIGUEONE_fixtures_yc$ligueone_xHYC,digits = 0),round(LIGUEONE_fixtures_yc$ligueone_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
ligueone_GP <- nrow(LIGUEONE)
#Calculate total home corners for each division
ligueone_home_corners <- aggregate(LIGUEONE$HCO, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_corners <- aggregate(LIGUEONE$ACO, by = list(LIGUEONE$AwayTeam), FUN = sum)
###############################################################################
ligueone_T_HCO <- sum(ligueone_home_corners$x)
#calculate average home corners
ligueone_avg_HCO <- round(ligueone_T_HCO /ligueone_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ligueone_T_ACO <- sum(ligueone_away_corners$x)
#calculate average away goal
ligueone_avg_ACO <- round(ligueone_T_ACO /ligueone_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ligueone_home_coas <- round(((ligueone_home_corners$x/ligueone_home_games))/ligueone_avg_HCO, digits = 4)
#calculate away attack strength
ligueone_away_coas <- round(((ligueone_away_corners$x/ligueone_away_games))/ligueone_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
ligueone_avg_HCOC <- round(ligueone_T_ACO /ligueone_GP, digits = 4)
#avg away concede
ligueone_avg_ACOC <- round(ligueone_T_HCO /ligueone_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
ligueone_home_coc <- aggregate(LIGUEONE$ACO, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_coc <- aggregate(LIGUEONE$HCO, by = list(LIGUEONE$AwayTeam), FUN = sum)
#home defense strength
ligueone_home_cods <- round(((ligueone_home_coc$x/ligueone_home_games))/ligueone_avg_HCOC, digits = 4)
#away defense strength
ligueone_away_cods <- round(((ligueone_away_coc$x/ligueone_away_games))/ligueone_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_home_poisson_corners <- cbind(ligueone_division,ligueone_teams,ligueone_avg_HCO,ligueone_home_coas,ligueone_home_cods)
#################################################################################
#away poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_away_poisson_corners <- cbind(ligueone_division,ligueone_teams,ligueone_avg_ACO,ligueone_away_coas,ligueone_away_cods)

#LIGUEONE
HomeTeam_ligueone_co <- rep(ligueone_teams, each = length(ligueone_teams))
AwayTeam_ligueone_co <- rep(ligueone_teams, length(ligueone_teams))
LIGUEONE_fixtures_co <- cbind(HomeTeam_ligueone_co,AwayTeam_ligueone_co)
LIGUEONE_fixtures_co <- as.data.frame(LIGUEONE_fixtures_co)
LIGUEONE_fixtures_co <- LIGUEONE_fixtures_co[!LIGUEONE_fixtures_co$HomeTeam_ligueone_co == LIGUEONE_fixtures_co$AwayTeam_ligueone_co,]
rownames(LIGUEONE_fixtures_co) <- NULL
LIGUEONE_fixtures_co$Div <- "LIGUEONE"
LIGUEONE_fixtures_co <- LIGUEONE_fixtures_co[,c(3,1,2)]

LIGUEONE_fixtures_co$avg_HCO_ligueone <- ligueone_avg_HCO

LIGUEONE_fixtures_co$ligueone_homecoas <- rep(ligueone_home_coas,each = length(ligueone_teams)-1)

ligueone_awaycods_lookup <- cbind(ligueone_teams,ligueone_away_cods)

ligueone_awaycods_lookup <- as.data.frame(ligueone_awaycods_lookup)

colnames(ligueone_awaycods_lookup) <- c("AwayTeam_ligueone_co","ligueone_awaycods")


require('RH2')
LIGUEONE_fixtures_co$ligueone_awaycods <- sqldf("SELECT ligueone_awaycods_lookup.ligueone_awaycods FROM ligueone_awaycods_lookup INNER JOIN LIGUEONE_fixtures_co ON ligueone_awaycods_lookup.AwayTeam_ligueone_co = LIGUEONE_fixtures_co.AwayTeam_ligueone_co")

LIGUEONE_fixtures_co$avg_ACO_ligueone <- ligueone_avg_ACO

ligueone_awaycoas_lookup <- cbind(ligueone_teams,ligueone_away_coas)

ligueone_awaycoas_lookup <- as.data.frame(ligueone_awaycoas_lookup)

colnames(ligueone_awaycoas_lookup) <- c("AwayTeam_ligueone_co","ligueone_awaycoas")

LIGUEONE_fixtures_co$ligueone_awaycoas <- sqldf("SELECT ligueone_awaycoas_lookup.ligueone_awaycoas FROM ligueone_awaycoas_lookup INNER JOIN LIGUEONE_fixtures_co ON ligueone_awaycoas_lookup.AwayTeam_ligueone_co = LIGUEONE_fixtures_co.AwayTeam_ligueone_co")

LIGUEONE_fixtures_co$ligueone_homecods <- rep(ligueone_home_cods,each = length(ligueone_teams)-1)

LIGUEONE_fixtures_co$ligueone_awaycods <- as.numeric(unlist(LIGUEONE_fixtures_co$ligueone_awaycods))
#xGH
LIGUEONE_fixtures_co$ligueone_xHCOC <- LIGUEONE_fixtures_co$avg_HCO_ligueone * LIGUEONE_fixtures_co$ligueone_homecoas * LIGUEONE_fixtures_co$ligueone_awaycods
#xGA

LIGUEONE_fixtures_co$ligueone_awaycoas <- as.numeric(unlist(LIGUEONE_fixtures_co$ligueone_awaycoas))

LIGUEONE_fixtures_co$ligueone_xACOC <- LIGUEONE_fixtures_co$avg_ACO_ligueone * LIGUEONE_fixtures_co$ligueone_awaycoas * LIGUEONE_fixtures_co$ligueone_homecods

LIGUEONE_fixtures_co$ligueone_0_0 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_0 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_0_1 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_1 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_0 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_0_2 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_2 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_1 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_2 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_3 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_0 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_1 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_2 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_0_3 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_3 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_3 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_4 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_0 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_1 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_2 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_3 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_0_4 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_4 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_4 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_4 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_5 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_0 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_1 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_2 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_3 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_4 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_0_5 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_5 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_5 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_5 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_5 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_6 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_0 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_1 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_2 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_3 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_4 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_6_5 <- round(stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_0_6 <- round(stats::dpois(0,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_1_6 <- round(stats::dpois(1,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_2_6 <- round(stats::dpois(2,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_3_6 <- round(stats::dpois(3,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_4_6 <- round(stats::dpois(4,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
LIGUEONE_fixtures_co$ligueone_5_6 <- round(stats::dpois(5,LIGUEONE_fixtures_co$ligueone_xHCOC) * stats::dpois(6,LIGUEONE_fixtures_co$ligueone_xACOC), digits = 4)
#Home win
LIGUEONE_fixtures_co$ligueone_H <- (
  LIGUEONE_fixtures_co$ligueone_1_0 + LIGUEONE_fixtures_co$ligueone_2_0 + LIGUEONE_fixtures_co$ligueone_2_1 + LIGUEONE_fixtures_co$ligueone_3_0 + LIGUEONE_fixtures_co$ligueone_3_1 +
    LIGUEONE_fixtures_co$ligueone_3_2 + LIGUEONE_fixtures_co$ligueone_4_0 + LIGUEONE_fixtures_co$ligueone_4_1 + LIGUEONE_fixtures_co$ligueone_4_2 + LIGUEONE_fixtures_co$ligueone_4_3 +
    LIGUEONE_fixtures_co$ligueone_5_0 + LIGUEONE_fixtures_co$ligueone_5_1 + LIGUEONE_fixtures_co$ligueone_5_2 + LIGUEONE_fixtures_co$ligueone_5_3 + LIGUEONE_fixtures_co$ligueone_5_4 +
    LIGUEONE_fixtures_co$ligueone_6_0 + LIGUEONE_fixtures_co$ligueone_6_1 + LIGUEONE_fixtures_co$ligueone_6_2 + LIGUEONE_fixtures_co$ligueone_6_3 + LIGUEONE_fixtures_co$ligueone_6_4 +
    LIGUEONE_fixtures_co$ligueone_6_5
)

LIGUEONE_fixtures_co$ligueone_H <- percent(LIGUEONE_fixtures_co$ligueone_H, accuracy = 0.1)

#Draw
LIGUEONE_fixtures_co$ligueone_D <- (

  LIGUEONE_fixtures_co$ligueone_0_0 + LIGUEONE_fixtures_co$ligueone_1_1 + LIGUEONE_fixtures_co$ligueone_2_2 + LIGUEONE_fixtures_co$ligueone_3_3 + LIGUEONE_fixtures_co$ligueone_4_4 +
    LIGUEONE_fixtures_co$ligueone_5_5 + LIGUEONE_fixtures_co$ligueone_6_6
)

LIGUEONE_fixtures_co$ligueone_D <- percent(LIGUEONE_fixtures_co$ligueone_D, accuracy = 0.1)

#Away

LIGUEONE_fixtures_co$ligueone_A <- (
  LIGUEONE_fixtures_co$ligueone_0_1 + LIGUEONE_fixtures_co$ligueone_0_2 + LIGUEONE_fixtures_co$ligueone_1_2 + LIGUEONE_fixtures_co$ligueone_0_3 + LIGUEONE_fixtures_co$ligueone_1_3 +
    LIGUEONE_fixtures_co$ligueone_2_3 + LIGUEONE_fixtures_co$ligueone_0_4 + LIGUEONE_fixtures_co$ligueone_1_4 + LIGUEONE_fixtures_co$ligueone_2_4 + LIGUEONE_fixtures_co$ligueone_3_4 +
    LIGUEONE_fixtures_co$ligueone_0_5 + LIGUEONE_fixtures_co$ligueone_1_5 + LIGUEONE_fixtures_co$ligueone_2_5 + LIGUEONE_fixtures_co$ligueone_3_5 + LIGUEONE_fixtures_co$ligueone_4_5 +
    LIGUEONE_fixtures_co$ligueone_0_6 + LIGUEONE_fixtures_co$ligueone_1_6 + LIGUEONE_fixtures_co$ligueone_2_6 + LIGUEONE_fixtures_co$ligueone_3_6 + LIGUEONE_fixtures_co$ligueone_4_6 +
    LIGUEONE_fixtures_co$ligueone_5_6
)

LIGUEONE_fixtures_co$ligueone_A <- percent(LIGUEONE_fixtures_co$ligueone_A, accuracy = 0.1)

#ov25
LIGUEONE_fixtures_co$ligueone_ov25 <- (
  LIGUEONE_fixtures_co$ligueone_2_1 + LIGUEONE_fixtures_co$ligueone_1_2 + LIGUEONE_fixtures_co$ligueone_2_2 + LIGUEONE_fixtures_co$ligueone_3_0 + LIGUEONE_fixtures_co$ligueone_3_1 +
    LIGUEONE_fixtures_co$ligueone_3_2 + LIGUEONE_fixtures_co$ligueone_0_3 + LIGUEONE_fixtures_co$ligueone_1_3 + LIGUEONE_fixtures_co$ligueone_2_3 + LIGUEONE_fixtures_co$ligueone_3_3 +
    LIGUEONE_fixtures_co$ligueone_4_0 + LIGUEONE_fixtures_co$ligueone_4_1 + LIGUEONE_fixtures_co$ligueone_4_2 + LIGUEONE_fixtures_co$ligueone_4_3 + LIGUEONE_fixtures_co$ligueone_0_4 +
    LIGUEONE_fixtures_co$ligueone_1_4 + LIGUEONE_fixtures_co$ligueone_2_4 + LIGUEONE_fixtures_co$ligueone_3_4 + LIGUEONE_fixtures_co$ligueone_4_4 + LIGUEONE_fixtures_co$ligueone_5_0 +
    LIGUEONE_fixtures_co$ligueone_5_1 + LIGUEONE_fixtures_co$ligueone_5_2 + LIGUEONE_fixtures_co$ligueone_5_3 + LIGUEONE_fixtures_co$ligueone_5_4 + LIGUEONE_fixtures_co$ligueone_0_5 +
    LIGUEONE_fixtures_co$ligueone_1_5 + LIGUEONE_fixtures_co$ligueone_2_5 + LIGUEONE_fixtures_co$ligueone_3_5 + LIGUEONE_fixtures_co$ligueone_4_5 + LIGUEONE_fixtures_co$ligueone_5_5 +
    LIGUEONE_fixtures_co$ligueone_6_0 + LIGUEONE_fixtures_co$ligueone_6_1 + LIGUEONE_fixtures_co$ligueone_6_2 + LIGUEONE_fixtures_co$ligueone_6_3 + LIGUEONE_fixtures_co$ligueone_6_4 +
    LIGUEONE_fixtures_co$ligueone_6_5 + LIGUEONE_fixtures_co$ligueone_0_6 + LIGUEONE_fixtures_co$ligueone_1_6 + LIGUEONE_fixtures_co$ligueone_2_6 + LIGUEONE_fixtures_co$ligueone_3_6 +
    LIGUEONE_fixtures_co$ligueone_4_6 + LIGUEONE_fixtures_co$ligueone_5_6 + LIGUEONE_fixtures_co$ligueone_6_6
)
#un25
LIGUEONE_fixtures_co$ligueone_un25 <- (
  LIGUEONE_fixtures_co$ligueone_0_0 + LIGUEONE_fixtures_co$ligueone_1_0 + LIGUEONE_fixtures_co$ligueone_0_1 + LIGUEONE_fixtures_co$ligueone_1_1 + LIGUEONE_fixtures_co$ligueone_2_0 + LIGUEONE_fixtures_co$ligueone_0_2
)
#odds
LIGUEONE_fixtures_co$ligueone_ov25_odds <- round((1/LIGUEONE_fixtures_co$ligueone_ov25),digits = 2)
LIGUEONE_fixtures_co$ligueone_un25_odds <- round((1/LIGUEONE_fixtures_co$ligueone_un25),digits = 2)

LIGUEONE_fixtures_co$ligueone_ov25_odds
LIGUEONE_fixtures_co$ligueone_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUEONE_fixtures_co$ligueone_ov25 <- percent(LIGUEONE_fixtures_co$ligueone_ov25, accuracy = 0.1)

LIGUEONE_fixtures_co$ligueone_un25 <- percent(LIGUEONE_fixtures_co$ligueone_un25, accuracy = 0.1)
LIGUEONE_fixtures_co$ligueone_pscore <- paste(round(LIGUEONE_fixtures_co$ligueone_xHCOC,digits = 0),round(LIGUEONE_fixtures_co$ligueone_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
ligueone_GP <- nrow(LIGUEONE)
#Calculate total home goals for each division
ligueone_T_HF <- sum(ligueone_home_fouls$x)
#calculate average home goal
ligueone_avg_HF <- round(ligueone_T_HF /ligueone_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ligueone_T_AF <- sum(ligueone_away_fouls$x)
#calculate average away goal
ligueone_avg_AF <- round(ligueone_T_AF /ligueone_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ligueone_home_fas <- round(((ligueone_home_fouls$x/ligueone_home_games))/ligueone_avg_HF, digits = 4)
#calculate away attack strength
ligueone_away_fas <- round(((ligueone_away_fouls$x/ligueone_away_games))/ligueone_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
ligueone_avg_HFC <- round(ligueone_T_AF /ligueone_GP, digits = 4)
#avg away concede
ligueone_avg_AFC <- round(ligueone_T_HF /ligueone_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
ligueone_home_fcc <- aggregate(LIGUEONE$AF, by = list(LIGUEONE$HomeTeam), FUN = sum)
ligueone_away_fcc <- aggregate(LIGUEONE$HF, by = list(LIGUEONE$AwayTeam), FUN = sum)

#home defense strength
ligueone_home_fds <- round(((ligueone_home_fcc$x/ligueone_home_games))/ligueone_avg_HFC, digits = 4)

#away defense strength
ligueone_away_fds <- round(((ligueone_away_fcc$x/ligueone_away_games))/ligueone_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_home_poisson_fo <- cbind(ligueone_division,ligueone_teams,ligueone_avg_HF,ligueone_home_fas,ligueone_home_fds)

#################################################################################
#away poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_away_poisson_fo <- cbind(ligueone_division,ligueone_teams,ligueone_avg_AF,ligueone_away_fas,ligueone_away_fds)

#LIGUEONE
HomeTeam_ligueone_fo <- rep(ligueone_teams, each = length(ligueone_teams))
AwayTeam_ligueone_fo <- rep(ligueone_teams, length(ligueone_teams))
LIGUEONE_fixtures_fo <- cbind(HomeTeam_ligueone_fo,AwayTeam_ligueone_fo)
LIGUEONE_fixtures_fo <- as.data.frame(LIGUEONE_fixtures_fo)
LIGUEONE_fixtures_fo <- LIGUEONE_fixtures_fo[!LIGUEONE_fixtures_fo$HomeTeam_ligueone_fo == LIGUEONE_fixtures_fo$AwayTeam_ligueone_fo,]
rownames(LIGUEONE_fixtures_fo) <- NULL
LIGUEONE_fixtures_fo$Div <- "LIGUEONE"
LIGUEONE_fixtures_fo <- LIGUEONE_fixtures_fo[,c(3,1,2)]

LIGUEONE_fixtures_fo$avg_HF_ligueone <- ligueone_avg_HF

LIGUEONE_fixtures_fo$ligueone_homefas <- rep(ligueone_home_fas,each = length(ligueone_teams)-1)

ligueone_awayfds_lookup <- cbind(ligueone_teams,ligueone_away_fds)

ligueone_awayfds_lookup <- as.data.frame(ligueone_awayfds_lookup)

colnames(ligueone_awayfds_lookup) <- c("AwayTeam_ligueone_fo","ligueone_awayfds")


require('RH2')
LIGUEONE_fixtures_fo$ligueone_awayfds <- sqldf("SELECT ligueone_awayfds_lookup.ligueone_awayfds FROM ligueone_awayfds_lookup INNER JOIN LIGUEONE_fixtures_fo ON ligueone_awayfds_lookup.AwayTeam_ligueone_fo = LIGUEONE_fixtures_fo.AwayTeam_ligueone_fo")

LIGUEONE_fixtures_fo$avg_AF_ligueone <- ligueone_avg_AF

ligueone_awayfas_lookup <- cbind(ligueone_teams,ligueone_away_fas)

ligueone_awayfas_lookup <- as.data.frame(ligueone_awayfas_lookup)

colnames(ligueone_awayfas_lookup) <- c("AwayTeam_ligueone_fo","ligueone_awayfas")

LIGUEONE_fixtures_fo$ligueone_awayfas <- sqldf("SELECT ligueone_awayfas_lookup.ligueone_awayfas FROM ligueone_awayfas_lookup INNER JOIN LIGUEONE_fixtures_fo ON ligueone_awayfas_lookup.AwayTeam_ligueone_fo = LIGUEONE_fixtures_fo.AwayTeam_ligueone_fo")

LIGUEONE_fixtures_fo$ligueone_homefds <- rep(ligueone_home_fds,each = length(ligueone_teams)-1)

LIGUEONE_fixtures_fo$ligueone_awayfds <- as.numeric(unlist(LIGUEONE_fixtures_fo$ligueone_awayfds))
#xGH
LIGUEONE_fixtures_fo$ligueone_xHF <- LIGUEONE_fixtures_fo$avg_HF_ligueone * LIGUEONE_fixtures_fo$ligueone_homefas * LIGUEONE_fixtures_fo$ligueone_awayfds
#xGA

LIGUEONE_fixtures_fo$ligueone_awayfas <- as.numeric(unlist(LIGUEONE_fixtures_fo$ligueone_awayfas))

LIGUEONE_fixtures_fo$ligueone_xAF <- LIGUEONE_fixtures_fo$avg_AF_ligueone * LIGUEONE_fixtures_fo$ligueone_awayfas * LIGUEONE_fixtures_fo$ligueone_homefds

LIGUEONE_fixtures_fo$ligueone_0_0 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_0 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_0_1 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_1 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_0 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_0_2 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_2 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_1 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_2 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_3 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_0 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_1 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_2 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_0_3 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_3 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_3 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_4 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_0 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_1 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_2 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_3 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_0_4 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_4 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_4 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_4 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_5 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_0 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_1 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_2 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_3 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_4 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_0_5 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_5 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_5 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_5 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_5 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_6 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_0 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_1 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_2 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_3 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_4 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_6_5 <- round(stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_0_6 <- round(stats::dpois(0,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_1_6 <- round(stats::dpois(1,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_2_6 <- round(stats::dpois(2,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_3_6 <- round(stats::dpois(3,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_4_6 <- round(stats::dpois(4,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
LIGUEONE_fixtures_fo$ligueone_5_6 <- round(stats::dpois(5,LIGUEONE_fixtures_fo$ligueone_xHF) * stats::dpois(6,LIGUEONE_fixtures_fo$ligueone_xAF), digits = 4)
#Home win
LIGUEONE_fixtures_fo$ligueone_H <- (
  LIGUEONE_fixtures_fo$ligueone_1_0 + LIGUEONE_fixtures_fo$ligueone_2_0 + LIGUEONE_fixtures_fo$ligueone_2_1 + LIGUEONE_fixtures_fo$ligueone_3_0 + LIGUEONE_fixtures_fo$ligueone_3_1 +
    LIGUEONE_fixtures_fo$ligueone_3_2 + LIGUEONE_fixtures_fo$ligueone_4_0 + LIGUEONE_fixtures_fo$ligueone_4_1 + LIGUEONE_fixtures_fo$ligueone_4_2 + LIGUEONE_fixtures_fo$ligueone_4_3 +
    LIGUEONE_fixtures_fo$ligueone_5_0 + LIGUEONE_fixtures_fo$ligueone_5_1 + LIGUEONE_fixtures_fo$ligueone_5_2 + LIGUEONE_fixtures_fo$ligueone_5_3 + LIGUEONE_fixtures_fo$ligueone_5_4 +
    LIGUEONE_fixtures_fo$ligueone_6_0 + LIGUEONE_fixtures_fo$ligueone_6_1 + LIGUEONE_fixtures_fo$ligueone_6_2 + LIGUEONE_fixtures_fo$ligueone_6_3 + LIGUEONE_fixtures_fo$ligueone_6_4 +
    LIGUEONE_fixtures_fo$ligueone_6_5
)

LIGUEONE_fixtures_fo$ligueone_H <- percent(LIGUEONE_fixtures_fo$ligueone_H, accuracy = 0.1)

#Draw
LIGUEONE_fixtures_fo$ligueone_D <- (

  LIGUEONE_fixtures_fo$ligueone_0_0 + LIGUEONE_fixtures_fo$ligueone_1_1 + LIGUEONE_fixtures_fo$ligueone_2_2 + LIGUEONE_fixtures_fo$ligueone_3_3 + LIGUEONE_fixtures_fo$ligueone_4_4 +
    LIGUEONE_fixtures_fo$ligueone_5_5 + LIGUEONE_fixtures_fo$ligueone_6_6
)

LIGUEONE_fixtures_fo$ligueone_D <- percent(LIGUEONE_fixtures_fo$ligueone_D, accuracy = 0.1)

#Away

LIGUEONE_fixtures_fo$ligueone_A <- (
  LIGUEONE_fixtures_fo$ligueone_0_1 + LIGUEONE_fixtures_fo$ligueone_0_2 + LIGUEONE_fixtures_fo$ligueone_1_2 + LIGUEONE_fixtures_fo$ligueone_0_3 + LIGUEONE_fixtures_fo$ligueone_1_3 +
    LIGUEONE_fixtures_fo$ligueone_2_3 + LIGUEONE_fixtures_fo$ligueone_0_4 + LIGUEONE_fixtures_fo$ligueone_1_4 + LIGUEONE_fixtures_fo$ligueone_2_4 + LIGUEONE_fixtures_fo$ligueone_3_4 +
    LIGUEONE_fixtures_fo$ligueone_0_5 + LIGUEONE_fixtures_fo$ligueone_1_5 + LIGUEONE_fixtures_fo$ligueone_2_5 + LIGUEONE_fixtures_fo$ligueone_3_5 + LIGUEONE_fixtures_fo$ligueone_4_5 +
    LIGUEONE_fixtures_fo$ligueone_0_6 + LIGUEONE_fixtures_fo$ligueone_1_6 + LIGUEONE_fixtures_fo$ligueone_2_6 + LIGUEONE_fixtures_fo$ligueone_3_6 + LIGUEONE_fixtures_fo$ligueone_4_6 +
    LIGUEONE_fixtures_fo$ligueone_5_6
)

LIGUEONE_fixtures_fo$ligueone_A <- percent(LIGUEONE_fixtures_fo$ligueone_A, accuracy = 0.1)

#ov25
LIGUEONE_fixtures_fo$ligueone_ov25 <- (
  LIGUEONE_fixtures_fo$ligueone_2_1 + LIGUEONE_fixtures_fo$ligueone_1_2 + LIGUEONE_fixtures_fo$ligueone_2_2 + LIGUEONE_fixtures_fo$ligueone_3_0 + LIGUEONE_fixtures_fo$ligueone_3_1 +
    LIGUEONE_fixtures_fo$ligueone_3_2 + LIGUEONE_fixtures_fo$ligueone_0_3 + LIGUEONE_fixtures_fo$ligueone_1_3 + LIGUEONE_fixtures_fo$ligueone_2_3 + LIGUEONE_fixtures_fo$ligueone_3_3 +
    LIGUEONE_fixtures_fo$ligueone_4_0 + LIGUEONE_fixtures_fo$ligueone_4_1 + LIGUEONE_fixtures_fo$ligueone_4_2 + LIGUEONE_fixtures_fo$ligueone_4_3 + LIGUEONE_fixtures_fo$ligueone_0_4 +
    LIGUEONE_fixtures_fo$ligueone_1_4 + LIGUEONE_fixtures_fo$ligueone_2_4 + LIGUEONE_fixtures_fo$ligueone_3_4 + LIGUEONE_fixtures_fo$ligueone_4_4 + LIGUEONE_fixtures_fo$ligueone_5_0 +
    LIGUEONE_fixtures_fo$ligueone_5_1 + LIGUEONE_fixtures_fo$ligueone_5_2 + LIGUEONE_fixtures_fo$ligueone_5_3 + LIGUEONE_fixtures_fo$ligueone_5_4 + LIGUEONE_fixtures_fo$ligueone_0_5 +
    LIGUEONE_fixtures_fo$ligueone_1_5 + LIGUEONE_fixtures_fo$ligueone_2_5 + LIGUEONE_fixtures_fo$ligueone_3_5 + LIGUEONE_fixtures_fo$ligueone_4_5 + LIGUEONE_fixtures_fo$ligueone_5_5 +
    LIGUEONE_fixtures_fo$ligueone_6_0 + LIGUEONE_fixtures_fo$ligueone_6_1 + LIGUEONE_fixtures_fo$ligueone_6_2 + LIGUEONE_fixtures_fo$ligueone_6_3 + LIGUEONE_fixtures_fo$ligueone_6_4 +
    LIGUEONE_fixtures_fo$ligueone_6_5 + LIGUEONE_fixtures_fo$ligueone_0_6 + LIGUEONE_fixtures_fo$ligueone_1_6 + LIGUEONE_fixtures_fo$ligueone_2_6 + LIGUEONE_fixtures_fo$ligueone_3_6 +
    LIGUEONE_fixtures_fo$ligueone_4_6 + LIGUEONE_fixtures_fo$ligueone_5_6 + LIGUEONE_fixtures_fo$ligueone_6_6
)
#un25
LIGUEONE_fixtures_fo$ligueone_un25 <- (
  LIGUEONE_fixtures_fo$ligueone_0_0 + LIGUEONE_fixtures_fo$ligueone_1_0 + LIGUEONE_fixtures_fo$ligueone_0_1 + LIGUEONE_fixtures_fo$ligueone_1_1 + LIGUEONE_fixtures_fo$ligueone_2_0 + LIGUEONE_fixtures_fo$ligueone_0_2
)
#odds
LIGUEONE_fixtures_fo$ligueone_ov25_odds <- round((1/LIGUEONE_fixtures_fo$ligueone_ov25),digits = 2)
LIGUEONE_fixtures_fo$ligueone_un25_odds <- round((1/LIGUEONE_fixtures_fo$ligueone_un25),digits = 2)

LIGUEONE_fixtures_fo$ligueone_ov25_odds
LIGUEONE_fixtures_fo$ligueone_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUEONE_fixtures_fo$ligueone_ov25 <- percent(LIGUEONE_fixtures_fo$ligueone_ov25, accuracy = 0.1)

LIGUEONE_fixtures_fo$ligueone_un25 <- percent(LIGUEONE_fixtures_fo$ligueone_un25, accuracy = 0.1)
LIGUEONE_fixtures_fo$ligueone_psfore <- paste(round(LIGUEONE_fixtures_fo$ligueone_xHF,digits = 0),round(LIGUEONE_fixtures_fo$ligueone_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
ligueone_GP <- nrow(LIGUEONE)

#Calculate total home goals for each division
ligueone_T_HST <- sum(ligueone_home_hst$x)
#calculate average home goal

ligueone_avg_HST <- round(ligueone_T_HST /ligueone_GP, digits = 4)

############################################################
#Calculate total away goals for each division
ligueone_T_AST <- sum(ligueone_away_ast$x)
#calculate average away goal
ligueone_avg_AST <- round(ligueone_T_AST /ligueone_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ligueone_home_sotas <- round(((ligueone_home_hst$x/ligueone_home_games))/ligueone_avg_HST, digits = 4)
#calculate away attack strength
ligueone_away_sotas <- round(((ligueone_away_ast$x/ligueone_away_games))/ligueone_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
ligueone_avg_HSC <- round(ligueone_T_AST /ligueone_GP, digits = 4)

#avg away concede
ligueone_avg_ASC <- round(ligueone_T_HST /ligueone_GP, digits = 4)
#home defense strength
ligueone_home_sods <- round(((ligueone_home_hsc$x/ligueone_home_games))/ligueone_avg_HSC, digits = 4)

#away defense strength
ligueone_away_sods <- round(((ligueone_away_ast$x/ligueone_away_games))/ligueone_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_home_poisson_sot <- cbind(ligueone_division,ligueone_teams,ligueone_avg_HST,ligueone_home_sotas,ligueone_home_sods)

#################################################################################
#away poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_away_poisson_sot <- cbind(ligueone_division,ligueone_teams,ligueone_avg_AST,ligueone_away_sotas,ligueone_away_sods)

#LIGUEONE
HomeTeam_ligueone_sot <- rep(ligueone_teams, each = length(ligueone_teams))
AwayTeam_ligueone_sot <- rep(ligueone_teams, length(ligueone_teams))
LIGUEONE_fixtures_sot <- cbind(HomeTeam_ligueone_sot,AwayTeam_ligueone_sot)
LIGUEONE_fixtures_sot <- as.data.frame(LIGUEONE_fixtures_sot)
LIGUEONE_fixtures_sot <- LIGUEONE_fixtures_sot[!LIGUEONE_fixtures_sot$HomeTeam_ligueone_sot == LIGUEONE_fixtures_sot$AwayTeam_ligueone_sot,]
rownames(LIGUEONE_fixtures_sot) <- NULL
LIGUEONE_fixtures_sot$Div <- "LIGUEONE"
LIGUEONE_fixtures_sot <- LIGUEONE_fixtures_sot[,c(3,1,2)]

LIGUEONE_fixtures_sot$avg_HST_ligueone <- ligueone_avg_HST

LIGUEONE_fixtures_sot$ligueone_homesotas <- rep(ligueone_home_sotas,each = length(ligueone_teams)-1)

ligueone_awaysods_lookup <- cbind(ligueone_teams,ligueone_away_sods)

ligueone_awaysods_lookup <- as.data.frame(ligueone_awaysods_lookup)

colnames(ligueone_awaysods_lookup) <- c("AwayTeam_ligueone_sot","ligueone_awaysods")


require('RH2')
LIGUEONE_fixtures_sot$ligueone_awaysods <- sqldf("SELECT ligueone_awaysods_lookup.ligueone_awaysods FROM ligueone_awaysods_lookup INNER JOIN LIGUEONE_fixtures_sot ON ligueone_awaysods_lookup.AwayTeam_ligueone_sot = LIGUEONE_fixtures_sot.AwayTeam_ligueone_sot")

LIGUEONE_fixtures_sot$avg_AST_ligueone <- ligueone_avg_AST

ligueone_awaysotas_lookup <- cbind(ligueone_teams,ligueone_away_sotas)

ligueone_awaysotas_lookup <- as.data.frame(ligueone_awaysotas_lookup)

colnames(ligueone_awaysotas_lookup) <- c("AwayTeam_ligueone_sot","ligueone_awaysotas")

LIGUEONE_fixtures_sot$ligueone_awaysotas <- sqldf("SELECT ligueone_awaysotas_lookup.ligueone_awaysotas FROM ligueone_awaysotas_lookup INNER JOIN LIGUEONE_fixtures_sot ON ligueone_awaysotas_lookup.AwayTeam_ligueone_sot = LIGUEONE_fixtures_sot.AwayTeam_ligueone_sot")

LIGUEONE_fixtures_sot$ligueone_homesods <- rep(ligueone_home_sods,each = length(ligueone_teams)-1)

LIGUEONE_fixtures_sot$ligueone_awaysods <- as.numeric(unlist(LIGUEONE_fixtures_sot$ligueone_awaysods))
#xGH
LIGUEONE_fixtures_sot$ligueone_xHST <- LIGUEONE_fixtures_sot$avg_HST_ligueone * LIGUEONE_fixtures_sot$ligueone_homesotas * LIGUEONE_fixtures_sot$ligueone_awaysods
#xGA

LIGUEONE_fixtures_sot$ligueone_awaysotas <- as.numeric(unlist(LIGUEONE_fixtures_sot$ligueone_awaysotas))

LIGUEONE_fixtures_sot$ligueone_xAST <- LIGUEONE_fixtures_sot$avg_AST_ligueone * LIGUEONE_fixtures_sot$ligueone_awaysotas * LIGUEONE_fixtures_sot$ligueone_homesods

LIGUEONE_fixtures_sot$ligueone_0_0 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_0 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_0_1 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_1 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_0 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_0_2 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_2 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_1 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_2 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_3 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_0 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_1 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_2 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_0_3 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_3 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_3 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_4 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_0 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_1 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_2 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_3 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_0_4 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_4 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_4 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_4 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_5 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_0 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_1 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_2 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_3 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_4 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_0_5 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_5 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_5 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_5 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_5 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_6 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_0 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_1 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_2 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_3 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_4 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_6_5 <- round(stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_0_6 <- round(stats::dpois(0,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_1_6 <- round(stats::dpois(1,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_2_6 <- round(stats::dpois(2,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_3_6 <- round(stats::dpois(3,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_4_6 <- round(stats::dpois(4,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
LIGUEONE_fixtures_sot$ligueone_5_6 <- round(stats::dpois(5,LIGUEONE_fixtures_sot$ligueone_xHST) * stats::dpois(6,LIGUEONE_fixtures_sot$ligueone_xAST), digits = 4)
#Home win
LIGUEONE_fixtures_sot$ligueone_H <- (
  LIGUEONE_fixtures_sot$ligueone_1_0 + LIGUEONE_fixtures_sot$ligueone_2_0 + LIGUEONE_fixtures_sot$ligueone_2_1 + LIGUEONE_fixtures_sot$ligueone_3_0 + LIGUEONE_fixtures_sot$ligueone_3_1 +
    LIGUEONE_fixtures_sot$ligueone_3_2 + LIGUEONE_fixtures_sot$ligueone_4_0 + LIGUEONE_fixtures_sot$ligueone_4_1 + LIGUEONE_fixtures_sot$ligueone_4_2 + LIGUEONE_fixtures_sot$ligueone_4_3 +
    LIGUEONE_fixtures_sot$ligueone_5_0 + LIGUEONE_fixtures_sot$ligueone_5_1 + LIGUEONE_fixtures_sot$ligueone_5_2 + LIGUEONE_fixtures_sot$ligueone_5_3 + LIGUEONE_fixtures_sot$ligueone_5_4 +
    LIGUEONE_fixtures_sot$ligueone_6_0 + LIGUEONE_fixtures_sot$ligueone_6_1 + LIGUEONE_fixtures_sot$ligueone_6_2 + LIGUEONE_fixtures_sot$ligueone_6_3 + LIGUEONE_fixtures_sot$ligueone_6_4 +
    LIGUEONE_fixtures_sot$ligueone_6_5
)

LIGUEONE_fixtures_sot$ligueone_H <- percent(LIGUEONE_fixtures_sot$ligueone_H, accuracy = 0.1)

#Draw
LIGUEONE_fixtures_sot$ligueone_D <- (

  LIGUEONE_fixtures_sot$ligueone_0_0 + LIGUEONE_fixtures_sot$ligueone_1_1 + LIGUEONE_fixtures_sot$ligueone_2_2 + LIGUEONE_fixtures_sot$ligueone_3_3 + LIGUEONE_fixtures_sot$ligueone_4_4 +
    LIGUEONE_fixtures_sot$ligueone_5_5 + LIGUEONE_fixtures_sot$ligueone_6_6
)

LIGUEONE_fixtures_sot$ligueone_D <- percent(LIGUEONE_fixtures_sot$ligueone_D, accuracy = 0.1)

#Away

LIGUEONE_fixtures_sot$ligueone_A <- (
  LIGUEONE_fixtures_sot$ligueone_0_1 + LIGUEONE_fixtures_sot$ligueone_0_2 + LIGUEONE_fixtures_sot$ligueone_1_2 + LIGUEONE_fixtures_sot$ligueone_0_3 + LIGUEONE_fixtures_sot$ligueone_1_3 +
    LIGUEONE_fixtures_sot$ligueone_2_3 + LIGUEONE_fixtures_sot$ligueone_0_4 + LIGUEONE_fixtures_sot$ligueone_1_4 + LIGUEONE_fixtures_sot$ligueone_2_4 + LIGUEONE_fixtures_sot$ligueone_3_4 +
    LIGUEONE_fixtures_sot$ligueone_0_5 + LIGUEONE_fixtures_sot$ligueone_1_5 + LIGUEONE_fixtures_sot$ligueone_2_5 + LIGUEONE_fixtures_sot$ligueone_3_5 + LIGUEONE_fixtures_sot$ligueone_4_5 +
    LIGUEONE_fixtures_sot$ligueone_0_6 + LIGUEONE_fixtures_sot$ligueone_1_6 + LIGUEONE_fixtures_sot$ligueone_2_6 + LIGUEONE_fixtures_sot$ligueone_3_6 + LIGUEONE_fixtures_sot$ligueone_4_6 +
    LIGUEONE_fixtures_sot$ligueone_5_6
)

LIGUEONE_fixtures_sot$ligueone_A <- percent(LIGUEONE_fixtures_sot$ligueone_A, accuracy = 0.1)

#ov25
LIGUEONE_fixtures_sot$ligueone_ov25 <- (
  LIGUEONE_fixtures_sot$ligueone_2_1 + LIGUEONE_fixtures_sot$ligueone_1_2 + LIGUEONE_fixtures_sot$ligueone_2_2 + LIGUEONE_fixtures_sot$ligueone_3_0 + LIGUEONE_fixtures_sot$ligueone_3_1 +
    LIGUEONE_fixtures_sot$ligueone_3_2 + LIGUEONE_fixtures_sot$ligueone_0_3 + LIGUEONE_fixtures_sot$ligueone_1_3 + LIGUEONE_fixtures_sot$ligueone_2_3 + LIGUEONE_fixtures_sot$ligueone_3_3 +
    LIGUEONE_fixtures_sot$ligueone_4_0 + LIGUEONE_fixtures_sot$ligueone_4_1 + LIGUEONE_fixtures_sot$ligueone_4_2 + LIGUEONE_fixtures_sot$ligueone_4_3 + LIGUEONE_fixtures_sot$ligueone_0_4 +
    LIGUEONE_fixtures_sot$ligueone_1_4 + LIGUEONE_fixtures_sot$ligueone_2_4 + LIGUEONE_fixtures_sot$ligueone_3_4 + LIGUEONE_fixtures_sot$ligueone_4_4 + LIGUEONE_fixtures_sot$ligueone_5_0 +
    LIGUEONE_fixtures_sot$ligueone_5_1 + LIGUEONE_fixtures_sot$ligueone_5_2 + LIGUEONE_fixtures_sot$ligueone_5_3 + LIGUEONE_fixtures_sot$ligueone_5_4 + LIGUEONE_fixtures_sot$ligueone_0_5 +
    LIGUEONE_fixtures_sot$ligueone_1_5 + LIGUEONE_fixtures_sot$ligueone_2_5 + LIGUEONE_fixtures_sot$ligueone_3_5 + LIGUEONE_fixtures_sot$ligueone_4_5 + LIGUEONE_fixtures_sot$ligueone_5_5 +
    LIGUEONE_fixtures_sot$ligueone_6_0 + LIGUEONE_fixtures_sot$ligueone_6_1 + LIGUEONE_fixtures_sot$ligueone_6_2 + LIGUEONE_fixtures_sot$ligueone_6_3 + LIGUEONE_fixtures_sot$ligueone_6_4 +
    LIGUEONE_fixtures_sot$ligueone_6_5 + LIGUEONE_fixtures_sot$ligueone_0_6 + LIGUEONE_fixtures_sot$ligueone_1_6 + LIGUEONE_fixtures_sot$ligueone_2_6 + LIGUEONE_fixtures_sot$ligueone_3_6 +
    LIGUEONE_fixtures_sot$ligueone_4_6 + LIGUEONE_fixtures_sot$ligueone_5_6 + LIGUEONE_fixtures_sot$ligueone_6_6
)
#un25
LIGUEONE_fixtures_sot$ligueone_un25 <- (
  LIGUEONE_fixtures_sot$ligueone_0_0 + LIGUEONE_fixtures_sot$ligueone_1_0 + LIGUEONE_fixtures_sot$ligueone_0_1 + LIGUEONE_fixtures_sot$ligueone_1_1 + LIGUEONE_fixtures_sot$ligueone_2_0 + LIGUEONE_fixtures_sot$ligueone_0_2
)
#odds
LIGUEONE_fixtures_sot$ligueone_ov25_odds <- round((1/LIGUEONE_fixtures_sot$ligueone_ov25),digits = 2)
LIGUEONE_fixtures_sot$ligueone_un25_odds <- round((1/LIGUEONE_fixtures_sot$ligueone_un25),digits = 2)

LIGUEONE_fixtures_sot$ligueone_ov25_odds
LIGUEONE_fixtures_sot$ligueone_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUEONE_fixtures_sot$ligueone_ov25 <- percent(LIGUEONE_fixtures_sot$ligueone_ov25, accuracy = 0.1)

LIGUEONE_fixtures_sot$ligueone_un25 <- percent(LIGUEONE_fixtures_sot$ligueone_un25, accuracy = 0.1)
LIGUEONE_fixtures_sot$ligueone_pssotre <- paste(round(LIGUEONE_fixtures_sot$ligueone_xHST,digits = 0),round(LIGUEONE_fixtures_sot$ligueone_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
ligueone_home_wins <- c()
ligueone_away_wins <- c()
ligueone_home_draws <- c()
ligueone_away_draws <- c()
ligueone_home_loss <- c()
ligueone_away_loss <- c()



for (i_ligueone_wins in 1:length(ligueone_teams))
{

  ligueone_home_wins[i_ligueone_wins] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_wins] & LIGUEONE$FTR == "H",])
  ligueone_away_wins[i_ligueone_wins] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_wins] & LIGUEONE$FTR == "A",])
  ligueone_home_draws[i_ligueone_wins] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_wins] & LIGUEONE$FTR == "D",])
  ligueone_away_draws[i_ligueone_wins] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_wins] & LIGUEONE$FTR == "D",])
  ligueone_home_loss[i_ligueone_wins] <- nrow(LIGUEONE[LIGUEONE$HomeTeam == ligueone_teams[i_ligueone_wins] & LIGUEONE$FTR == "A",])
  ligueone_away_loss[i_ligueone_wins] <- nrow(LIGUEONE[LIGUEONE$AwayTeam == ligueone_teams[i_ligueone_wins] & LIGUEONE$FTR == "H",])

}

ligueone_total_wins <- ligueone_home_wins + ligueone_away_wins
ligueone_total_draws <- ligueone_home_draws + ligueone_away_draws
ligueone_total_loss <- ligueone_home_loss + ligueone_away_loss

ligueone_league_table <- cbind(ligueone_teams,ligueone_games_played,ligueone_total_wins,ligueone_total_draws,ligueone_total_loss)
ligueone_GS <- ligueone_scoring$TGS
ligueone_GC <-ligueone_conceding$TGC
ligueone_GD <- ligueone_scoring$TGS - ligueone_conceding$TGC
ligueone_PTS <- (ligueone_total_wins*3) + (ligueone_total_draws*1)
ligueone_league_table <- cbind(ligueone_league_table,ligueone_GS,ligueone_GC,ligueone_GD,ligueone_PTS)
ligueone_league_table <- as.data.frame(ligueone_league_table)
#rename the columns
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_teams"] <- "Team"
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_games_played"] <- "P"
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_total_wins"] <- "W"
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_total_draws"] <- "D"
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_total_loss"] <- "L"
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_GS"] <- "F"
names(ligueone_league_table)[names(ligueone_league_table) == "ligueone_GC"] <- "A"
points_ligueone <- ligueone_league_table[order(as.numeric(ligueone_league_table$ligueone_PTS), decreasing = TRUE),]
points_ligueone$ligueone_rank <- 1:length(ligueone_teams)
row.names(points_ligueone) <- points_ligueone$ligueone_rank
#create final_ligueone_hf_against with team ranks in brackets
for(ligueone_rowhrank in 1:nrow(ligueone_form_team_against_h)) {
  for(ligueone_colhrank in 1:ncol(ligueone_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!ligueone_form_team_against_h[ligueone_rowhrank,ligueone_colhrank]=="",ligueone_form_team_against_h[ligueone_rowhrank,ligueone_colhrank] <- paste(ligueone_form_team_against_h[ligueone_rowhrank,ligueone_colhrank],"(",points_ligueone$ligueone_rank[points_ligueone$Team ==ligueone_form_team_against_h[ligueone_rowhrank,ligueone_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
ligueone_GP <- nrow(LIGUEONE)

#Calculate total home goals for each division
ligueone_T_HG <- sum(ligueone_home_gs$x)

#calculate average home goal
ligueone_avg_HG <- round(ligueone_T_HG /ligueone_GP, digits = 4)
############################################################
#Calculate total away goals for each division
ligueone_T_AG <- sum(ligueone_away_gs$x)
#calculate average away goal
ligueone_avg_AG <- round(ligueone_T_AG /ligueone_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
ligueone_home_as <- round(((ligueone_home_gs$x/ligueone_home_games))/ligueone_avg_HG, digits = 4)
#calculate away attack strength
ligueone_away_as <- round(((ligueone_away_gs$x/ligueone_away_games))/ligueone_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
ligueone_avg_HC <- round(ligueone_T_AG /ligueone_GP, digits = 4)
#avg away concede
ligueone_avg_AC <- round(ligueone_T_HG /ligueone_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
ligueone_home_ds <- round(((ligueone_home_gc$x/ligueone_home_games))/ligueone_avg_HC, digits = 4)
#away defense strength
ligueone_away_ds <- round(((ligueone_away_gc$x/ligueone_away_games))/ligueone_avg_AC, digits = 4)
#############################################################################
#home poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_home_poisson <- cbind(ligueone_division,ligueone_teams,ligueone_avg_HG,ligueone_home_as,ligueone_home_ds)
#################################################################################
#away poisson data
#ligueone
ligueone_division <- c()
ligueone_division[1:length(ligueone_teams)] <- "LIGUEONE"
ligueone_away_poisson <- cbind(ligueone_division,ligueone_teams,ligueone_avg_AG,ligueone_away_as,ligueone_away_ds)

#LIGUEONE
HomeTeam_ligueone <- rep(ligueone_teams, each = length(ligueone_teams))
AwayTeam_ligueone <- rep(ligueone_teams, length(ligueone_teams))
LIGUEONE_fixtures <- cbind(HomeTeam_ligueone,AwayTeam_ligueone)
LIGUEONE_fixtures <- as.data.frame(LIGUEONE_fixtures)
LIGUEONE_fixtures <- LIGUEONE_fixtures[!LIGUEONE_fixtures$HomeTeam_ligueone == LIGUEONE_fixtures$AwayTeam_ligueone,]
rownames(LIGUEONE_fixtures) <- NULL
LIGUEONE_fixtures$Div <- "LIGUEONE"
LIGUEONE_fixtures <- LIGUEONE_fixtures[,c(3,1,2)]

LIGUEONE_fixtures$avg_HG_ligueone <- ligueone_avg_HG

LIGUEONE_fixtures$ligueone_homeas <- rep(ligueone_home_as,each = length(ligueone_teams)-1)

ligueone_awayds_lookup <- cbind(ligueone_teams,ligueone_away_ds)

ligueone_awayds_lookup <- as.data.frame(ligueone_awayds_lookup)

colnames(ligueone_awayds_lookup) <- c("AwayTeam_ligueone","ligueone_awayds")


require('RH2')
LIGUEONE_fixtures$ligueone_awayds <- sqldf("SELECT ligueone_awayds_lookup.ligueone_awayds FROM ligueone_awayds_lookup INNER JOIN LIGUEONE_fixtures ON ligueone_awayds_lookup.AwayTeam_ligueone = LIGUEONE_fixtures.AwayTeam_ligueone")

LIGUEONE_fixtures$avg_AG_ligueone <- ligueone_avg_AG

ligueone_awayas_lookup <- cbind(ligueone_teams,ligueone_away_as)

ligueone_awayas_lookup <- as.data.frame(ligueone_awayas_lookup)

colnames(ligueone_awayas_lookup) <- c("AwayTeam_ligueone","ligueone_awayas")


LIGUEONE_fixtures$ligueone_awayas <- sqldf("SELECT ligueone_awayas_lookup.ligueone_awayas FROM ligueone_awayas_lookup INNER JOIN LIGUEONE_fixtures ON ligueone_awayas_lookup.AwayTeam_ligueone = LIGUEONE_fixtures.AwayTeam_ligueone")

LIGUEONE_fixtures$ligueone_homeds <- rep(ligueone_home_ds,each = length(ligueone_teams)-1)

LIGUEONE_fixtures$ligueone_awayds <- as.numeric(unlist(LIGUEONE_fixtures$ligueone_awayds))
#xGH
LIGUEONE_fixtures$ligueone_xGH <- LIGUEONE_fixtures$avg_HG_ligueone * LIGUEONE_fixtures$ligueone_homeas * LIGUEONE_fixtures$ligueone_awayds

#xGA

LIGUEONE_fixtures$ligueone_awayas <- as.numeric(unlist(LIGUEONE_fixtures$ligueone_awayas))

LIGUEONE_fixtures$ligueone_xGA <- LIGUEONE_fixtures$avg_AG_ligueone * LIGUEONE_fixtures$ligueone_awayas * LIGUEONE_fixtures$ligueone_homeds

LIGUEONE_fixtures$ligueone_0_0 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_0 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_0_1 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_1 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_0 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_0_2 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_2 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_1 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_2 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_3 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_0 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_1 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_2 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_0_3 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_3 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_3 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_4 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_0 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_1 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_2 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_3 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_0_4 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_4 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_4 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_4 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_5 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_0 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_1 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_2 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_3 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_4 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_0_5 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_5 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_5 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_5 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_5 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_6 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_0 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(0,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_1 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(1,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_2 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(2,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_3 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(3,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_4 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(4,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_6_5 <- round(stats::dpois(6,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(5,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_0_6 <- round(stats::dpois(0,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_1_6 <- round(stats::dpois(1,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_2_6 <- round(stats::dpois(2,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_3_6 <- round(stats::dpois(3,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_4_6 <- round(stats::dpois(4,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
LIGUEONE_fixtures$ligueone_5_6 <- round(stats::dpois(5,LIGUEONE_fixtures$ligueone_xGH) * stats::dpois(6,LIGUEONE_fixtures$ligueone_xGA), digits = 4)
#Home win
LIGUEONE_fixtures$ligueone_H <- (
  LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
    LIGUEONE_fixtures$ligueone_5_0 + LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5
)

LIGUEONE_fixtures$ligueone_H <- percent(LIGUEONE_fixtures$ligueone_H, accuracy = 0.1)

#Draw
LIGUEONE_fixtures$ligueone_D <- (

  LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 + LIGUEONE_fixtures$ligueone_4_4 +
    LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6
)

LIGUEONE_fixtures$ligueone_D <- percent(LIGUEONE_fixtures$ligueone_D, accuracy = 0.1)

#Away

LIGUEONE_fixtures$ligueone_A <- (
  LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
    LIGUEONE_fixtures$ligueone_0_5 + LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6
)

LIGUEONE_fixtures$ligueone_A <- percent(LIGUEONE_fixtures$ligueone_A, accuracy = 0.1)

#ov25
LIGUEONE_fixtures$ligueone_ov25 <- (
  LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 + LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 + LIGUEONE_fixtures$ligueone_0_4 +
    LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 + LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_0 +
    LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 + LIGUEONE_fixtures$ligueone_0_5 +
    LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 + LIGUEONE_fixtures$ligueone_5_5 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5 + LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 +
    LIGUEONE_fixtures$ligueone_4_6 + LIGUEONE_fixtures$ligueone_5_6 + LIGUEONE_fixtures$ligueone_6_6
)
#un25
LIGUEONE_fixtures$ligueone_un25 <- (
  LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_0_2
)
#odds
LIGUEONE_fixtures$ligueone_ov25_odds <- round((1/LIGUEONE_fixtures$ligueone_ov25),digits = 2)
LIGUEONE_fixtures$ligueone_un25_odds <- round((1/LIGUEONE_fixtures$ligueone_un25),digits = 2)

LIGUEONE_fixtures$ligueone_ov25_odds
LIGUEONE_fixtures$ligueone_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
LIGUEONE_fixtures$ligueone_BTTSY <- (
  LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_3_1 + LIGUEONE_fixtures$ligueone_3_2 +
    LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_1_3 + LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_3_3 + LIGUEONE_fixtures$ligueone_4_4 +
    LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_3 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 +
    LIGUEONE_fixtures$ligueone_3_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 +
    LIGUEONE_fixtures$ligueone_5_4 + LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_6_6 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6
)
#BTTSN
LIGUEONE_fixtures$ligueone_BTTSN <- (
  LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_0_2 +
    LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_5_0 +
    LIGUEONE_fixtures$ligueone_0_5 + LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_0_6
)

LIGUEONE_fixtures$ligueone_BTTSY_odds <- round((1/LIGUEONE_fixtures$ligueone_BTTSY),digits = 2)
LIGUEONE_fixtures$ligueone_BTTSN_odds <- round((1/LIGUEONE_fixtures$ligueone_BTTSN),digits = 2)

LIGUEONE_fixtures$ligueone_BTTSY <- percent(LIGUEONE_fixtures$ligueone_BTTSY, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_BTTSN <- percent(LIGUEONE_fixtures$ligueone_BTTSN, accuracy = 0.1)
#odds
LIGUEONE_fixtures$ligueone_BTTSY_odds
LIGUEONE_fixtures$ligueone_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
LIGUEONE_fixtures$ligueone_AH_0_H <- (
  LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
    LIGUEONE_fixtures$ligueone_5_0 +LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5 + LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6
)
#AH_0_A
LIGUEONE_fixtures$ligueone_AH_0_A <- (
  LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
    LIGUEONE_fixtures$ligueone_0_5 +LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6 + LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6
)

#odds
LIGUEONE_fixtures$ligueone_AH_0_H_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_0_H),digits = 2)
LIGUEONE_fixtures$ligueone_AH_0_A_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_0_A),digits = 2)

LIGUEONE_fixtures$ligueone_AH_0_H_odds
LIGUEONE_fixtures$ligueone_AH_0_A_odds
#percentages
LIGUEONE_fixtures$ligueone_AH_0_H <- percent(LIGUEONE_fixtures$ligueone_AH_0_H, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_AH_0_A <- percent(LIGUEONE_fixtures$ligueone_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
LIGUEONE_fixtures$ligueone_AH_n075_H <- (
  LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
    LIGUEONE_fixtures$ligueone_5_0 +LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5
)
#AH_n075_A
LIGUEONE_fixtures$ligueone_AH_n075_A <- (
  LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
    LIGUEONE_fixtures$ligueone_0_5 +LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6
)

#odds
LIGUEONE_fixtures$ligueone_AH_n075_H_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_n075_H),digits = 2)
LIGUEONE_fixtures$ligueone_AH_n075_A_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_n075_A),digits = 2)

LIGUEONE_fixtures$ligueone_AH_n075_H_odds
LIGUEONE_fixtures$ligueone_AH_n075_A_odds
#percentages
LIGUEONE_fixtures$ligueone_AH_n075_H <- percent(LIGUEONE_fixtures$ligueone_AH_n075_H, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_AH_n075_A <- percent(LIGUEONE_fixtures$ligueone_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
LIGUEONE_fixtures$ligueone_AH_075_H <- (
  LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
    LIGUEONE_fixtures$ligueone_5_0 +LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5 + LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6 + LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_1_2 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_3_4 + LIGUEONE_fixtures$ligueone_4_5 + LIGUEONE_fixtures$ligueone_5_6
)
#AH_075_A
LIGUEONE_fixtures$ligueone_AH_075_A <- (
  LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
    LIGUEONE_fixtures$ligueone_0_5 +LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6 + LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6 + LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_3 + LIGUEONE_fixtures$ligueone_5_4 + LIGUEONE_fixtures$ligueone_6_5
)

#odds
LIGUEONE_fixtures$ligueone_AH_075_H_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_075_H),digits = 2)
LIGUEONE_fixtures$ligueone_AH_075_A_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_075_A),digits = 2)

LIGUEONE_fixtures$ligueone_AH_075_H_odds
LIGUEONE_fixtures$ligueone_AH_075_A_odds
#percentages
LIGUEONE_fixtures$ligueone_AH_075_H <- percent(LIGUEONE_fixtures$ligueone_AH_075_H, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_AH_075_A <- percent(LIGUEONE_fixtures$ligueone_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
LIGUEONE_fixtures$ligueone_AH_n125_H <- (
  LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
    LIGUEONE_fixtures$ligueone_5_0 +LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5
)
#AH_n125_A
LIGUEONE_fixtures$ligueone_AH_n125_A <- (
  LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
    LIGUEONE_fixtures$ligueone_0_5 +LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6
)

#odds
LIGUEONE_fixtures$ligueone_AH_n125_H_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_n125_H),digits = 2)
LIGUEONE_fixtures$ligueone_AH_n125_A_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_n125_A),digits = 2)

LIGUEONE_fixtures$ligueone_AH_n125_H_odds
LIGUEONE_fixtures$ligueone_AH_n125_A_odds
#percentages
LIGUEONE_fixtures$ligueone_AH_n125_H <- percent(LIGUEONE_fixtures$ligueone_AH_n125_H, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_AH_n125_A <- percent(LIGUEONE_fixtures$ligueone_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
LIGUEONE_fixtures$ligueone_AH_125_H <- (
  LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
    LIGUEONE_fixtures$ligueone_5_0 +LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
    LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
    LIGUEONE_fixtures$ligueone_6_5 + LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6 + LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_1_2 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_3_4 + LIGUEONE_fixtures$ligueone_4_5 + LIGUEONE_fixtures$ligueone_5_6
)
#AH_125_A
LIGUEONE_fixtures$ligueone_AH_125_A <- (
  LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
    LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
    LIGUEONE_fixtures$ligueone_0_5 +LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
    LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
    LIGUEONE_fixtures$ligueone_5_6 + LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 +
    LIGUEONE_fixtures$ligueone_4_4 + LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6 + LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_1 +
    LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_3 + LIGUEONE_fixtures$ligueone_5_4 + LIGUEONE_fixtures$ligueone_6_5
)

#odds
LIGUEONE_fixtures$ligueone_AH_125_H_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_125_H),digits = 2)
LIGUEONE_fixtures$ligueone_AH_125_A_odds <- round((1/LIGUEONE_fixtures$ligueone_AH_125_A),digits = 2)

LIGUEONE_fixtures$ligueone_AH_125_H_odds
LIGUEONE_fixtures$ligueone_AH_125_A_odds
#percentages
LIGUEONE_fixtures$ligueone_AH_125_H <- percent(LIGUEONE_fixtures$ligueone_AH_125_H, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_AH_125_A <- percent(LIGUEONE_fixtures$ligueone_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
LIGUEONE_fixtures$ligueone_ov25 <- percent(LIGUEONE_fixtures$ligueone_ov25, accuracy = 0.1)

LIGUEONE_fixtures$ligueone_un25 <- percent(LIGUEONE_fixtures$ligueone_un25, accuracy = 0.1)
LIGUEONE_fixtures$ligueone_pscore <- paste(round(LIGUEONE_fixtures$ligueone_xGH,digits = 0),round(LIGUEONE_fixtures$ligueone_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
ligueone_last_n_games <- 6

#create final_ligueone_hf object
final_ligueone_hf <- c()
for(index_ligueone_hf in 1:length(ligueone_teams))
{
  index_ligueone_hf <- row.names(ligueone_form_h) == ligueone_teams[index_ligueone_hf]
  form_ligueone_hf <- ligueone_form_h[index_ligueone_hf]
  deleted_form_ligueone_hf <- form_ligueone_hf[!form_ligueone_hf[] == ""]
  l6_form_ligueone_hf <- tail(deleted_form_ligueone_hf,ligueone_last_n_games)
  l6_form_ligueone_hf <- paste(l6_form_ligueone_hf,collapse = " ")
  final_ligueone_hf[index_ligueone_hf] <- rbind(paste(ligueone_teams[index_ligueone_hf],l6_form_ligueone_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}

#change column nam
final_ligueone_hf <- as.data.frame(final_ligueone_hf)
colnames(final_ligueone_hf) <- "Form"
#goals scored
#create final_ligueone_gs object
final_ligueone_gs <- c()
suml6_ligueone_gs <- c()
for(index_ligueone_gs in 1:length(ligueone_teams))
{
  index_ligueone_gs <- row.names(ligueone_goalscored_h) == ligueone_teams[index_ligueone_gs]
  form_ligueone_gs <- ligueone_goalscored_h[index_ligueone_gs]
  deleted_form_ligueone_gs <- form_ligueone_gs[!form_ligueone_gs[] == ""]
  l6_form_ligueone_gs <- tail(deleted_form_ligueone_gs,ligueone_last_n_games)
  l6_form_ligueone_gs <- as.numeric(l6_form_ligueone_gs)
  suml6_ligueone_gs[index_ligueone_gs] <- sum(l6_form_ligueone_gs)
  suml6_ligueone_gs[index_ligueone_gs] <- paste("(",suml6_ligueone_gs[index_ligueone_gs],")",sep = "")
  l6_form_ligueone_gs <- paste(l6_form_ligueone_gs,collapse = " ")
  final_ligueone_gs[index_ligueone_gs] <- rbind(paste(ligueone_teams[index_ligueone_gs],l6_form_ligueone_gs,suml6_ligueone_gs[index_ligueone_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
final_ligueone_gs
#change column names
final_ligueone_gs <- as.data.frame(final_ligueone_gs)
colnames(final_ligueone_gs) <- "Goals scored"
#goal conceded
#create final_ligueone_gc object
final_ligueone_gc <- c()
suml6_ligueone_gc <- c()
for(index_ligueone_gc in 1:length(ligueone_teams))
{
  index_ligueone_gc <- row.names(ligueone_goalconceded_h) == ligueone_teams[index_ligueone_gc]
  form_ligueone_gc <- ligueone_goalconceded_h[index_ligueone_gc]
  deleted_form_ligueone_gc <- form_ligueone_gc[!form_ligueone_gc[] == ""]
  l6_form_ligueone_gc <- tail(deleted_form_ligueone_gc,ligueone_last_n_games)
  l6_form_ligueone_gc <- as.numeric(l6_form_ligueone_gc)
  suml6_ligueone_gc[index_ligueone_gc] <- sum(l6_form_ligueone_gc)
  suml6_ligueone_gc[index_ligueone_gc] <- paste("(",suml6_ligueone_gc[index_ligueone_gc],")",sep = "")
  l6_form_ligueone_gc <- paste(l6_form_ligueone_gc,collapse = " ")
  final_ligueone_gc[index_ligueone_gc] <- rbind(paste(ligueone_teams[index_ligueone_gc],l6_form_ligueone_gc,suml6_ligueone_gc[index_ligueone_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
#change column names
final_ligueone_gc <- as.data.frame(final_ligueone_gc)
colnames(final_ligueone_gc) <- "Goals conceded"


toString(l6_form_ligueone_gc)
#total goals
#create final_ligueone_tg object
final_ligueone_tg <- c()
suml6_ligueone_tg <- c()
for(index_ligueone_tg in 1:length(ligueone_teams))
{
  index_ligueone_tg <- row.names(ligueone_totalgoals_h) == ligueone_teams[index_ligueone_tg]
  form_ligueone_tg <- ligueone_totalgoals_h[index_ligueone_tg]
  deleted_form_ligueone_tg <- form_ligueone_tg[!form_ligueone_tg[] == ""]
  l6_form_ligueone_tg <- tail(deleted_form_ligueone_tg,ligueone_last_n_games)
  l6_form_ligueone_tg <- as.numeric(l6_form_ligueone_tg)
  suml6_ligueone_tg[index_ligueone_tg] <- sum(l6_form_ligueone_tg)
  suml6_ligueone_tg[index_ligueone_tg] <- paste("(",suml6_ligueone_tg[index_ligueone_tg],")",sep = "")
  l6_form_ligueone_tg <- paste(l6_form_ligueone_tg,collapse = " ")
  final_ligueone_tg[index_ligueone_tg] <- rbind(paste(ligueone_teams[index_ligueone_tg],l6_form_ligueone_tg,suml6_ligueone_tg[index_ligueone_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
#change column names
final_ligueone_tg <- as.data.frame(final_ligueone_tg)
colnames(final_ligueone_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_ligueone_hf object
final_ligueone_cs <- c()
for(index_ligueone_cs in 1:length(ligueone_teams))
{
  index_ligueone_cs <- row.names(ligueone_csform_h) == ligueone_teams[index_ligueone_cs]
  csform_ligueone_cs <- ligueone_csform_h[index_ligueone_cs]
  deleted_csform_ligueone_cs <- csform_ligueone_cs[!csform_ligueone_cs[] == ""]
  l6_csform_ligueone_cs <- tail(deleted_csform_ligueone_cs,ligueone_last_n_games)
  l6_csform_ligueone_cs <- paste(l6_csform_ligueone_cs,collapse = " ")
  final_ligueone_cs[index_ligueone_cs] <- rbind(paste(ligueone_teams[index_ligueone_cs],l6_csform_ligueone_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_csform)

}

#change column names
final_ligueone_cs <- as.data.frame(final_ligueone_cs)
colnames(final_ligueone_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_ligueone_wm object
final_ligueone_wm <- c()
suml6_ligueone_wm <- c()
for(index_ligueone_wm in 1:length(ligueone_teams))
{
  index_ligueone_wm <- row.names(ligueone_winmargin_h) == ligueone_teams[index_ligueone_wm]
  form_ligueone_wm <- ligueone_winmargin_h[index_ligueone_wm]
  deleted_form_ligueone_wm <- form_ligueone_wm[!form_ligueone_wm[] == ""]
  l6_form_ligueone_wm <- tail(deleted_form_ligueone_wm,ligueone_last_n_games)
  l6_form_ligueone_wm <- as.numeric(l6_form_ligueone_wm)
  suml6_ligueone_wm[index_ligueone_wm] <- sum(l6_form_ligueone_wm)
  suml6_ligueone_wm[index_ligueone_wm] <- paste("(",suml6_ligueone_wm[index_ligueone_wm],")",sep = "")
  l6_form_ligueone_wm <- paste(l6_form_ligueone_wm,collapse = " ")
  final_ligueone_wm[index_ligueone_wm] <- rbind(paste(ligueone_teams[index_ligueone_wm],l6_form_ligueone_wm,suml6_ligueone_wm[index_ligueone_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
final_ligueone_wm
#change column names
final_ligueone_wm <- as.data.frame(final_ligueone_wm)
colnames(final_ligueone_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_ligueone_ca object
final_ligueone_ca <- c()
suml6_ligueone_ca <- c()
for(index_ligueone_ca in 1:length(ligueone_teams))
{
  index_ligueone_ca <- row.names(ligueone_coawarded_h) == ligueone_teams[index_ligueone_ca]
  form_ligueone_ca <- ligueone_coawarded_h[index_ligueone_ca]
  deleted_form_ligueone_ca <- form_ligueone_ca[!form_ligueone_ca[] == ""]
  l6_form_ligueone_ca <- tail(deleted_form_ligueone_ca,ligueone_last_n_games)
  l6_form_ligueone_ca <- as.numeric(l6_form_ligueone_ca)
  suml6_ligueone_ca[index_ligueone_ca] <- sum(l6_form_ligueone_ca)
  suml6_ligueone_ca[index_ligueone_ca] <- paste("(",suml6_ligueone_ca[index_ligueone_ca],")",sep = "")
  l6_form_ligueone_ca <- paste(l6_form_ligueone_ca,collapse = " ")
  final_ligueone_ca[index_ligueone_ca] <- rbind(paste(ligueone_teams[index_ligueone_ca],l6_form_ligueone_ca,suml6_ligueone_ca[index_ligueone_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
final_ligueone_ca
#change column names
final_ligueone_ca <- as.data.frame(final_ligueone_ca)
colnames(final_ligueone_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_ligueone_ca object
final_ligueone_cc <- c()
suml6_ligueone_cc <- c()
for(index_ligueone_cc in 1:length(ligueone_teams))
{
  index_ligueone_cc <- row.names(ligueone_cornersconceded_h) == ligueone_teams[index_ligueone_cc]
  form_ligueone_cc <- ligueone_cornersconceded_h[index_ligueone_cc]
  deleted_form_ligueone_cc <- form_ligueone_cc[!form_ligueone_cc[] == ""]
  l6_form_ligueone_cc <- tail(deleted_form_ligueone_cc,ligueone_last_n_games)
  l6_form_ligueone_cc <- as.numeric(l6_form_ligueone_cc)
  suml6_ligueone_cc[index_ligueone_cc] <- sum(l6_form_ligueone_cc)
  suml6_ligueone_cc[index_ligueone_cc] <- paste("(",suml6_ligueone_cc[index_ligueone_cc],")",sep = "")
  l6_form_ligueone_cc <- paste(l6_form_ligueone_cc,collapse = " ")
  final_ligueone_cc[index_ligueone_cc] <- rbind(paste(ligueone_teams[index_ligueone_cc],l6_form_ligueone_cc,suml6_ligueone_cc[index_ligueone_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
final_ligueone_cc
#change column names
final_ligueone_cc <- as.data.frame(final_ligueone_cc)
colnames(final_ligueone_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_ligueone_cosc <- c()
for(index_ligueone_cosc in 1:length(ligueone_teams))
{
  index_ligueone_cosc <- row.names(ligueone_coscform_h) == ligueone_teams[index_ligueone_cosc]
  coscform_ligueone_cosc <- ligueone_coscform_h[index_ligueone_cosc]
  deleted_coscform_ligueone_cosc <- coscform_ligueone_cosc[!coscform_ligueone_cosc[] == ""]
  l6_coscform_ligueone_cosc <- tail(deleted_coscform_ligueone_cosc,ligueone_last_n_games)
  l6_coscform_ligueone_cosc <- paste(l6_coscform_ligueone_cosc,collapse = " ")
  final_ligueone_cosc[index_ligueone_cosc] <- rbind(paste(ligueone_teams[index_ligueone_cosc],l6_coscform_ligueone_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_coscform)

}
final_ligueone_cosc
#change column names
final_ligueone_cosc <- as.data.frame(final_ligueone_cosc)
colnames(final_ligueone_cosc) <- "CornersForm"
##################################################
#total corners
#create final_ligueone_tcorners object
final_ligueone_tcorners <- c()
suml6_ligueone_tcorners <- c()
for(index_ligueone_tcorners in 1:length(ligueone_teams))
{
  index_ligueone_tcorners <- row.names(ligueone_totalcorners_h) == ligueone_teams[index_ligueone_tcorners]
  form_ligueone_tcorners <- ligueone_totalcorners_h[index_ligueone_tcorners]
  deleted_form_ligueone_tcorners <- form_ligueone_tcorners[!form_ligueone_tcorners[] == ""]
  l6_form_ligueone_tcorners <- tail(deleted_form_ligueone_tcorners,ligueone_last_n_games)
  l6_form_ligueone_tcorners <- as.numeric(l6_form_ligueone_tcorners)
  suml6_ligueone_tcorners[index_ligueone_tcorners] <- sum(l6_form_ligueone_tcorners)
  suml6_ligueone_tcorners[index_ligueone_tcorners] <- paste("(",suml6_ligueone_tcorners[index_ligueone_tcorners],")",sep = "")
  l6_form_ligueone_tcorners <- paste(l6_form_ligueone_tcorners,collapse = " ")
  final_ligueone_tcorners[index_ligueone_tcorners] <- rbind(paste(ligueone_teams[index_ligueone_tcorners],l6_form_ligueone_tcorners,suml6_ligueone_tcorners[index_ligueone_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
#change column names
final_ligueone_tcorners <- as.data.frame(final_ligueone_tcorners)
colnames(final_ligueone_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_ligueone_hf_against
final_ligueone_hf_against <- c()
for(index_ligueone_hf_against in 1:length(ligueone_teams))
{
  index_ligueone_hf_against <- row.names(ligueone_form_team_against_h) == ligueone_teams[index_ligueone_hf_against]
  form_ligueone_hf_against <- ligueone_form_team_against_h[index_ligueone_hf_against]
  deleted_form_ligueone_hf_against <- form_ligueone_hf_against[!form_ligueone_hf_against[] == ""]
  l6_form_ligueone_hf_against <- tail(deleted_form_ligueone_hf_against,ligueone_last_n_games)
  l6_form_ligueone_hf_against <- paste(l6_form_ligueone_hf_against,collapse = " ")
  final_ligueone_hf_against[index_ligueone_hf_against] <- rbind(paste(ligueone_teams[index_ligueone_hf_against],l6_form_ligueone_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ligueone_teams[index],l6_form)

}
final_ligueone_hf_against <- as.data.frame(final_ligueone_hf_against)
colnames(final_ligueone_hf_against) <- "Team against"
#combine the columns
final_ligueone_all <- cbind(final_ligueone_hf,final_ligueone_gs,final_ligueone_gc,final_ligueone_tg,final_ligueone_ca,final_ligueone_cc,final_ligueone_tcorners,final_ligueone_cosc,final_ligueone_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#LIGUEONE
LIGUEONE_sim <- LIGUEONE
LIGUEONE_sim$matchid <- paste(LIGUEONE_sim$HomeTeam,LIGUEONE_sim$AwayTeam,sep = "-")
LIGUEONE_fixtures$matchid <- paste(LIGUEONE_fixtures$HomeTeam_ligueone,LIGUEONE_fixtures$AwayTeam_ligueone,sep = "-")
LIGUEONE_fixtures$ligueone_FTR <- sapply(LIGUEONE_fixtures$ligueone_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

LIGUEONE_fixtures$ligueone_gamestatus <- ifelse(LIGUEONE_fixtures$matchid %in% LIGUEONE_sim$matchid,"played","notplayed")

ligueone_home_wins_sim <- c()
ligueone_away_wins_sim <- c()
ligueone_home_draws_sim <- c()
ligueone_away_draws_sim <- c()
ligueone_home_loss_sim <- c()
ligueone_away_loss_sim <- c()



for (i_ligueone_wins_sim in 1:length(ligueone_teams))
{

  ligueone_home_wins_sim[i_ligueone_wins_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$HomeTeam_ligueone == ligueone_teams[i_ligueone_wins_sim] & LIGUEONE_fixtures$ligueone_FTR == "H" & LIGUEONE_fixtures$ligueone_gamestatus =="notplayed",])
  ligueone_away_wins_sim[i_ligueone_wins_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$AwayTeam_ligueone == ligueone_teams[i_ligueone_wins_sim] & LIGUEONE_fixtures$ligueone_FTR == "A" & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",])
  ligueone_home_draws_sim[i_ligueone_wins_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$HomeTeam_ligueone == ligueone_teams[i_ligueone_wins_sim] & LIGUEONE_fixtures$ligueone_FTR == "D" & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",])
  ligueone_away_draws_sim[i_ligueone_wins_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$AwayTeam_ligueone == ligueone_teams[i_ligueone_wins_sim] & LIGUEONE_fixtures$ligueone_FTR == "D" & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",])
  ligueone_home_loss_sim[i_ligueone_wins_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$HomeTeam_ligueone == ligueone_teams[i_ligueone_wins_sim] & LIGUEONE_fixtures$ligueone_FTR == "A" & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",])
  ligueone_away_loss_sim[i_ligueone_wins_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$AwayTeam_ligueone == ligueone_teams[i_ligueone_wins_sim] & LIGUEONE_fixtures$ligueone_FTR == "H" & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed", ])

}

ligueone_total_wins_sim <- ligueone_home_wins_sim + ligueone_away_wins_sim
ligueone_total_draws_sim <- ligueone_home_draws_sim + ligueone_away_draws_sim
ligueone_total_loss_sim <- ligueone_home_loss_sim + ligueone_away_loss_sim

ligueone_home_games_sim <- c()
ligueone_away_games_sim <-c()

for (i_ligueone_sim in 1:length(ligueone_teams))
{

  ligueone_home_games_sim[i_ligueone_sim] <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$HomeTeam_ligueone == ligueone_teams[i_ligueone_sim] & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",])
  ligueone_away_games_sim[i_ligueone_sim]  <- nrow(LIGUEONE_fixtures[LIGUEONE_fixtures$AwayTeam_ligueone == ligueone_teams[i_ligueone_sim] & LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",])

}

ligueone_games_played_sim <- ligueone_home_games_sim + ligueone_away_games_sim

ligueone_league_table_sim <- cbind(ligueone_teams,ligueone_games_played_sim,ligueone_total_wins_sim,ligueone_total_draws_sim,ligueone_total_loss_sim)
ligueone_PTS_sim <- (ligueone_total_wins_sim*3) + (ligueone_total_draws_sim*1)
ligueone_league_table_sim <- cbind(ligueone_league_table_sim,ligueone_PTS_sim)

ligueone_games_played_simfinal <- ligueone_games_played + ligueone_games_played_sim
ligueone_total_wins_simfinal <- ligueone_total_wins + ligueone_total_wins_sim
ligueone_total_draws_simfinal <- ligueone_total_draws + ligueone_total_draws_sim
ligueone_total_loss_simfinal <- ligueone_total_loss + ligueone_total_loss_sim
ligueone_PTS_simfinal <- ligueone_PTS + ligueone_PTS_sim

ligueone_league_table_simfinal <- cbind(ligueone_teams,ligueone_games_played_simfinal,ligueone_total_wins_simfinal,ligueone_total_draws_simfinal,ligueone_total_loss_simfinal,ligueone_PTS_simfinal)
ligueone_league_table_simfinal <- as.data.frame(ligueone_league_table_simfinal)
names(ligueone_league_table_simfinal)[names(ligueone_league_table_simfinal) == "ligueone_teams"] <- "Team_f"
names(ligueone_league_table_simfinal)[names(ligueone_league_table_simfinal) == "ligueone_games_played_simfinal"] <- "P_f"
names(ligueone_league_table_simfinal)[names(ligueone_league_table_simfinal) == "ligueone_total_wins_simfinal"] <- "W_f"
names(ligueone_league_table_simfinal)[names(ligueone_league_table_simfinal) == "ligueone_total_draws_simfinal"] <- "D_f"
names(ligueone_league_table_simfinal)[names(ligueone_league_table_simfinal) == "ligueone_total_loss_simfinal"] <- "L_f"
names(ligueone_league_table_simfinal)[names(ligueone_league_table_simfinal) == "ligueone_PTS_simfinal"] <- "PTS_f"
points_ligueone_sim <-  ligueone_league_table_simfinal[order(as.numeric(ligueone_league_table_simfinal$PTS_f), decreasing = TRUE),]

LIGUEONE_notplayed <- LIGUEONE_fixtures[LIGUEONE_fixtures$ligueone_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#LIGUEONE
LIGUEONE_fixtures$Hometeam_ligueone_index <- match(LIGUEONE_fixtures$HomeTeam_ligueone,ligueone_teams)
LIGUEONE_fixtures$Awayteam_ligueone_index <- match(LIGUEONE_fixtures$AwayTeam_ligueone,ligueone_teams)
ligueone_prediction <- c()
ligueone_HWM <- c()
ligueone_AWM <- c()
ligueone_HWMLM <- c()
ligueone_AWMLM <- c()
ligueone_HY <- c()
ligueone_AY <- c()
ligueone_HCO <- c()
ligueone_ACO <- c()
ligueone_HXSC <- c()
ligueone_AXSC <- c()
ligueone_HYCPF <- c()
ligueone_AYCPF <- c()
for(ligueone_row in 1:nrow(LIGUEONE_fixtures))
{

  ligueone_hometeamindex <- LIGUEONE_fixtures[ligueone_row,"Hometeam_ligueone_index"]
  ligueone_awayteamindex <- LIGUEONE_fixtures[ligueone_row,"Awayteam_ligueone_index"]
  #analyse team form
  #home team
  ligueone_form_vec_ht <- as.vector(ligueone_form_h[ligueone_hometeamindex,])
  ligueone_form_vec_ht[is.na(ligueone_form_vec_ht)] <- ""
  ligueone_form_vec_ht <- ligueone_form_vec_ht[ligueone_form_vec_ht != ""]
  ligueone_form_vec_ht  <-tail(ligueone_form_vec_ht,6)
  ligueone_ht_numberof_wins <- length(which(ligueone_form_vec_ht == "W"))
  ligueone_ht_numberof_draws <- length(which(ligueone_form_vec_ht == "D"))
  ligueone_ht_numberof_loss <- length(which(ligueone_form_vec_ht == "L"))
  #awayteam
  ligueone_form_vec_at <- as.vector(ligueone_form_h[ligueone_awayteamindex,])
  ligueone_form_vec_at[is.na(ligueone_form_vec_at)] <- ""
  ligueone_form_vec_at <- ligueone_form_vec_at[ligueone_form_vec_at != ""]
  ligueone_form_vec_at  <-tail(ligueone_form_vec_at,6)
  ligueone_at_numberof_wins <- length(which(ligueone_form_vec_at == "W"))
  ligueone_at_numberof_draws <- length(which(ligueone_form_vec_at == "D"))
  ligueone_at_numberof_loss <- length(which(ligueone_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  ligueone_goalscored_vec_ht <- as.vector(ligueone_goalscored_h[ligueone_hometeamindex,])
  ligueone_goalscored_vec_ht[is.na(ligueone_goalscored_vec_ht)] <- ""
  ligueone_goalscored_vec_ht <- ligueone_goalscored_vec_ht[ligueone_goalscored_vec_ht != ""]
  ligueone_goalscored_vec_ht  <-tail(ligueone_goalscored_vec_ht,6)
  ligueone_goalscored_vec_ht  <- as.numeric(ligueone_goalscored_vec_ht)
  ligueone_ht_totalgoalscored <- sum(ligueone_goalscored_vec_ht)
  ligueone_ht_matches_scoring <- length(which(ligueone_goalscored_vec_ht > 0))
  ligueone_ht_matches_without_scoring <- length(which(ligueone_goalscored_vec_ht == "0"))
  #awayteam
  ligueone_goalscored_vec_at <- as.vector(ligueone_goalscored_h[ligueone_awayteamindex,])
  ligueone_goalscored_vec_at[is.na(ligueone_goalscored_vec_at)] <- ""
  ligueone_goalscored_vec_at <- ligueone_goalscored_vec_at[ligueone_goalscored_vec_at != ""]
  ligueone_goalscored_vec_at  <-tail(ligueone_goalscored_vec_at,6)
  ligueone_goalscored_vec_at  <- as.numeric(ligueone_goalscored_vec_at)
  ligueone_at_totalgoalscored <- sum(ligueone_goalscored_vec_at)
  ligueone_at_matches_scoring <- length(which(ligueone_goalscored_vec_at > 0))
  ligueone_at_matches_without_scoring <- length(which(ligueone_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  ligueone_goalconceded_vec_ht <- as.vector(ligueone_goalconceded_h[ligueone_hometeamindex,])
  ligueone_goalconceded_vec_ht[is.na(ligueone_goalconceded_vec_ht)] <- ""
  ligueone_goalconceded_vec_ht <- ligueone_goalconceded_vec_ht[ligueone_goalconceded_vec_ht != ""]
  ligueone_goalconceded_vec_ht  <-tail(ligueone_goalconceded_vec_ht,6)
  ligueone_goalconceded_vec_ht  <- as.numeric(ligueone_goalconceded_vec_ht)
  ligueone_goalconceded_vec_ht
  ligueone_ht_totalgoalconceded <- sum(ligueone_goalconceded_vec_ht)
  ligueone_ht_matches_concede <- length(which(ligueone_goalconceded_vec_ht > 0))
  ligueone_ht_matches_without_concede <- length(which(ligueone_goalconceded_vec_ht == "0"))
  #awayteam
  ligueone_goalconceded_vec_at <- as.vector(ligueone_goalconceded_h[ligueone_awayteamindex,])
  ligueone_goalconceded_vec_at[is.na(ligueone_goalconceded_vec_at)] <- ""
  ligueone_goalconceded_vec_at <- ligueone_goalconceded_vec_at[ligueone_goalconceded_vec_at != ""]
  ligueone_goalconceded_vec_at  <-tail(ligueone_goalconceded_vec_at,6)
  ligueone_goalconceded_vec_at  <- as.numeric(ligueone_goalconceded_vec_at)
  ligueone_at_totalgoalconceded <- sum(ligueone_goalconceded_vec_at)
  ligueone_at_matches_concede <- length(which(ligueone_goalconceded_vec_at > 0))
  ligueone_at_matches_without_concede <- length(which(ligueone_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  ligueone_totalgoals_vec_ht <- as.vector(ligueone_totalgoals_h[ligueone_hometeamindex,])
  ligueone_totalgoals_vec_ht[is.na(ligueone_totalgoals_vec_ht)] <- ""
  ligueone_totalgoals_vec_ht <- ligueone_totalgoals_vec_ht[ligueone_totalgoals_vec_ht != ""]
  ligueone_totalgoals_vec_ht  <-tail(ligueone_totalgoals_vec_ht,6)
  ligueone_totalgoals_vec_ht  <- as.numeric(ligueone_totalgoals_vec_ht)
  ligueone_totalgoals_vec_ht
  ligueone_ht_totalgoals <- sum(ligueone_totalgoals_vec_ht)
  ligueone_ht_avgtotalgoals <- (ligueone_ht_totalgoals/6)
  ligueone_ht_no_of_ov25 <- length(which(ligueone_totalgoals_vec_ht >= 3))
  ligueone_ht_no_of_un25 <- length(which(ligueone_totalgoals_vec_ht <= 2))
  #awayteam
  ligueone_totalgoals_vec_at <- as.vector(ligueone_totalgoals_h[ligueone_awayteamindex,])
  ligueone_totalgoals_vec_at[is.na(ligueone_totalgoals_vec_at)] <- ""
  ligueone_totalgoals_vec_at <- ligueone_totalgoals_vec_at[ligueone_totalgoals_vec_at != ""]
  ligueone_totalgoals_vec_at  <-tail(ligueone_totalgoals_vec_at,6)
  ligueone_totalgoals_vec_at  <- as.numeric(ligueone_totalgoals_vec_at)
  ligueone_totalgoals_vec_at
  ligueone_at_totalgoals <- sum(ligueone_totalgoals_vec_at)
  ligueone_at_avgtotalgoals <- (ligueone_at_totalgoals/6)
  ligueone_at_no_of_ov25 <- length(which(ligueone_totalgoals_vec_at >= 3))
  ligueone_at_no_of_un25 <- length(which(ligueone_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  ligueone_winmargin_vec_ht <- as.vector(ligueone_winmargin_h[ligueone_hometeamindex,])
  ligueone_winmargin_vec_ht[is.na(ligueone_winmargin_vec_ht)] <- ""
  ligueone_winmargin_vec_ht <- ligueone_winmargin_vec_ht[ligueone_winmargin_vec_ht != ""]
  ligueone_winmargin_vec_ht  <-tail(ligueone_winmargin_vec_ht,6)
  ligueone_winmargin_vec_ht  <- as.numeric(ligueone_winmargin_vec_ht)

  ligueone_ht_totalwinmargin <- sum(ligueone_winmargin_vec_ht)
  ligueone_ht_no_of_winmargin_ov0 <- length(which(ligueone_winmargin_vec_ht >= 0))
  ligueone_ht_no_of_winmargin_ov1 <- length(which(ligueone_winmargin_vec_ht >= 1))
  ligueone_ht_no_of_winmargin_un0 <- length(which(ligueone_winmargin_vec_ht <= 0))
  ligueone_ht_no_of_winmargin_un1 <- length(which(ligueone_winmargin_vec_ht <= 1))
  #awayteam
  ligueone_winmargin_vec_at <- as.vector(ligueone_winmargin_h[ligueone_awayteamindex,])
  ligueone_winmargin_vec_at[is.na(ligueone_winmargin_vec_at)] <- ""
  ligueone_winmargin_vec_at <- ligueone_winmargin_vec_at[ligueone_winmargin_vec_at != ""]
  ligueone_winmargin_vec_at  <-tail(ligueone_winmargin_vec_at,6)
  ligueone_winmargin_vec_at  <- as.numeric(ligueone_winmargin_vec_at)

  ligueone_at_totalwinmargin <- sum(ligueone_winmargin_vec_at)
  ligueone_at_no_of_winmargin_ov0 <- length(which(ligueone_winmargin_vec_at >= 0))
  ligueone_at_no_of_winmargin_ov1 <- length(which(ligueone_winmargin_vec_at >= 1))
  ligueone_at_no_of_winmargin_un0 <- length(which(ligueone_winmargin_vec_at <= 0))
  ligueone_at_no_of_winmargin_un1 <- length(which(ligueone_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  ligueone_winmargin_vec_ht_lm <- as.vector(ligueone_winmargin_h[ligueone_hometeamindex,])
  ligueone_winmargin_vec_ht_lm[is.na(ligueone_winmargin_vec_ht_lm)] <- ""
  ligueone_winmargin_vec_ht_lm <- ligueone_winmargin_vec_ht_lm[ligueone_winmargin_vec_ht_lm != ""]
  ligueone_winmargin_vec_ht_lm  <-tail(ligueone_winmargin_vec_ht_lm,1)
  #awayteam
  ligueone_winmargin_vec_at_lm <- as.vector(ligueone_winmargin_h[ligueone_awayteamindex,])
  ligueone_winmargin_vec_at_lm[is.na(ligueone_winmargin_vec_at_lm)] <- ""
  ligueone_winmargin_vec_at_lm <- ligueone_winmargin_vec_at_lm[ligueone_winmargin_vec_at_lm != ""]
  ligueone_winmargin_vec_at_lm  <-tail(ligueone_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  ligueone_yellowtotals_vec_ht <- as.vector(ligueone_yellowtotalsv2[ligueone_hometeamindex,])
  ligueone_yellowtotals_vec_ht[is.na(ligueone_yellowtotals_vec_ht)] <- ""
  ligueone_yellowtotals_vec_ht <- ligueone_yellowtotals_vec_ht[ligueone_yellowtotals_vec_ht != ""]
  ligueone_yellowtotals_vec_ht  <-tail(ligueone_yellowtotals_vec_ht,1)
  #awayteam
  ligueone_yellowtotals_vec_at <- as.vector(ligueone_yellowtotalsv2[ligueone_awayteamindex,])
  ligueone_yellowtotals_vec_at[is.na(ligueone_yellowtotals_vec_at)] <- ""
  ligueone_yellowtotals_vec_at <- ligueone_yellowtotals_vec_at[ligueone_yellowtotals_vec_at != ""]
  ligueone_yellowtotals_vec_at  <-tail(ligueone_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  ligueone_cornertotals_vec_ht <- as.vector(ligueone_cornertotalsv2[ligueone_hometeamindex,])
  ligueone_cornertotals_vec_ht[is.na(ligueone_cornertotals_vec_ht)] <- ""
  ligueone_cornertotals_vec_ht <- ligueone_cornertotals_vec_ht[ligueone_cornertotals_vec_ht != ""]
  ligueone_cornertotals_vec_ht  <-tail(ligueone_cornertotals_vec_ht,1)
  #awayteam
  ligueone_cornertotals_vec_at <- as.vector(ligueone_cornertotalsv2[ligueone_awayteamindex,])
  ligueone_cornertotals_vec_at[is.na(ligueone_cornertotals_vec_at)] <- ""
  ligueone_cornertotals_vec_at <- ligueone_cornertotals_vec_at[ligueone_cornertotals_vec_at != ""]
  ligueone_cornertotals_vec_at  <-tail(ligueone_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  ligueone_xshotsconversion_vec_ht <- as.vector(ligueone_shots_analysis[ligueone_hometeamindex,])
  ligueone_xshotsconversion_vec_ht[is.na(ligueone_xshotsconversion_vec_ht)] <- ""
  ligueone_xshotsconversion_vec_ht <- ligueone_xshotsconversion_vec_ht[ligueone_xshotsconversion_vec_ht != ""]
  ligueone_xshotsconversion_vec_ht  <-tail(ligueone_xshotsconversion_vec_ht,1)
  #awayteam
  ligueone_xshotsconversion_vec_at <- as.vector(ligueone_shots_analysis[ligueone_awayteamindex,])
  ligueone_xshotsconversion_vec_at[is.na(ligueone_xshotsconversion_vec_at)] <- ""
  ligueone_xshotsconversion_vec_at <- ligueone_xshotsconversion_vec_at[ligueone_xshotsconversion_vec_at != ""]
  ligueone_xshotsconversion_vec_at  <-tail(ligueone_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  ligueone_fouls_conversion_vec_ht <- as.vector(ligueone_fouls_conversion[ligueone_hometeamindex,])
  ligueone_fouls_conversion_vec_ht[is.na(ligueone_fouls_conversion_vec_ht)] <- ""
  ligueone_fouls_conversion_vec_ht <- ligueone_fouls_conversion_vec_ht[ligueone_fouls_conversion_vec_ht != ""]
  ligueone_fouls_conversion_vec_ht  <-tail(ligueone_fouls_conversion_vec_ht,1)
  #awayteam
  ligueone_fouls_conversion_vec_at <- as.vector(ligueone_fouls_conversion[ligueone_awayteamindex,])
  ligueone_fouls_conversion_vec_at[is.na(ligueone_fouls_conversion_vec_at)] <- ""
  ligueone_fouls_conversion_vec_at <- ligueone_fouls_conversion_vec_at[ligueone_fouls_conversion_vec_at != ""]
  ligueone_fouls_conversion_vec_at  <-tail(ligueone_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  ligueone_ht_last6points <- ligueone_ht_numberof_wins*3 + ligueone_ht_numberof_draws*1
  ligueone_at_last6points <- ligueone_at_numberof_wins*3 + ligueone_at_numberof_draws*1

  if(ligueone_ht_last6points > ligueone_at_last6points) {ligueone_3waypick <- "1"}  else {ligueone_3waypick <- "X2"}

  if(ligueone_at_last6points > ligueone_ht_last6points ) {ligueone_3waypick <- "2"} else {ligueone_3waypick <- "1X"}

  if(ligueone_ht_no_of_ov25 + ligueone_at_no_of_ov25 >= 6) {ligueone_goalspick <- "ov25"} else {ligueone_goalspick <- "un25"}

  if(ligueone_ht_no_of_un25 + ligueone_at_no_of_un25 >= 6) {ligueone_goalspick <- "un25"} else {ligueone_goalspick <- "ov25"}

  if(ligueone_ht_matches_scoring >= 4 && ligueone_at_matches_scoring >=4) {ligueone_btts <- "BTTS-Y"} else {ligueone_btts <- "BTTS-N"}


  ligueone_prediction[ligueone_row] <- rbind(paste(ligueone_3waypick,ligueone_goalspick,ligueone_btts,sep = ","))
  ligueone_HWM[ligueone_row] <- ligueone_ht_totalwinmargin
  ligueone_AWM[ligueone_row] <- ligueone_at_totalwinmargin

  ligueone_HWMLM[ligueone_row] <- ligueone_winmargin_vec_ht_lm
  ligueone_AWMLM[ligueone_row] <- ligueone_winmargin_vec_at_lm

  ligueone_HY[ligueone_row] <- ligueone_yellowtotals_vec_ht
  ligueone_AY[ligueone_row] <- ligueone_yellowtotals_vec_at

  ligueone_HCO[ligueone_row] <- ligueone_cornertotals_vec_ht
  ligueone_ACO[ligueone_row] <- ligueone_cornertotals_vec_at

  ligueone_HXSC[ligueone_row] <- ligueone_xshotsconversion_vec_ht
  ligueone_AXSC[ligueone_row] <- ligueone_xshotsconversion_vec_at

  ligueone_HYCPF[ligueone_row] <- ligueone_fouls_conversion_vec_ht
  ligueone_AYCPF[ligueone_row] <- ligueone_fouls_conversion_vec_at
}

ligueone_prediction <- as.data.frame(ligueone_prediction)
colnames(ligueone_prediction) <- "prediction"

ligueone_HWM <- as.data.frame(ligueone_HWM)
colnames(ligueone_HWM) <- "HWM"

ligueone_AWM <- as.data.frame(ligueone_AWM)
colnames(ligueone_AWM) <- "AWM"

ligueone_HWMLM <- as.data.frame(ligueone_HWMLM)
colnames(ligueone_HWMLM) <- "HWMLM"

ligueone_AWMLM <- as.data.frame(ligueone_AWMLM)
colnames(ligueone_AWMLM) <- "AWMLM"

ligueone_HY <- as.data.frame(ligueone_HY)
colnames(ligueone_HY) <- "AVGHY"

ligueone_AY <- as.data.frame(ligueone_AY)
colnames(ligueone_AY) <- "AVGAY"

ligueone_HCO <- as.data.frame(ligueone_HCO)
colnames(ligueone_HCO) <- "AVGHCO"

ligueone_ACO <- as.data.frame(ligueone_ACO)
colnames(ligueone_ACO) <- "AVGACO"

ligueone_HXSC <- as.data.frame(ligueone_HXSC)
colnames(ligueone_HXSC) <- "HXSC"

ligueone_AXSC <- as.data.frame(ligueone_AXSC)
colnames(ligueone_AXSC) <- "AXSC"

ligueone_HYCPF <- as.data.frame(ligueone_HYCPF)
colnames(ligueone_HYCPF) <- "HYCPF"

ligueone_AYCPF <- as.data.frame(ligueone_AYCPF)
colnames(ligueone_AYCPF) <- "AYCPF"

ligueone_picks <- cbind(LIGUEONE_fixtures$Div,LIGUEONE_fixtures$HomeTeam_ligueone,LIGUEONE_fixtures$AwayTeam_ligueone,ligueone_prediction,ligueone_HWM,ligueone_AWM,ligueone_HWMLM,ligueone_AWMLM,ligueone_HY,ligueone_AY,ligueone_HCO,ligueone_ACO,ligueone_HXSC,ligueone_AXSC,ligueone_HYCPF,ligueone_AYCPF)

colnames(ligueone_picks)[1] <- "picks_Div"
colnames(ligueone_picks)[2] <- "picks_HomeTeam"
colnames(ligueone_picks)[3] <- "picks_AwayTeam"
ligueone_picks$matchid <- paste(ligueone_picks$picks_HomeTeam,ligueone_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of LIGUEONE
ligueone_picks
#############################################################################################################################################################################
#clone fixtures
LIGUEONE_fixtures_clone <- LIGUEONE_fixtures
colnames(LIGUEONE_fixtures_clone)[61] <- "Hwin"
colnames(LIGUEONE_fixtures_clone)[62] <- "Draw"
colnames(LIGUEONE_fixtures_clone)[63] <- "Awin"

LIGUEONE_fixtures_clone$Hwinodds <-   LIGUEONE_fixtures$ligueone_1_0 + LIGUEONE_fixtures$ligueone_2_0 + LIGUEONE_fixtures$ligueone_2_1 + LIGUEONE_fixtures$ligueone_3_0 + LIGUEONE_fixtures$ligueone_3_1 +
  LIGUEONE_fixtures$ligueone_3_2 + LIGUEONE_fixtures$ligueone_4_0 + LIGUEONE_fixtures$ligueone_4_1 + LIGUEONE_fixtures$ligueone_4_2 + LIGUEONE_fixtures$ligueone_4_3 +
  LIGUEONE_fixtures$ligueone_5_0 + LIGUEONE_fixtures$ligueone_5_1 + LIGUEONE_fixtures$ligueone_5_2 + LIGUEONE_fixtures$ligueone_5_3 + LIGUEONE_fixtures$ligueone_5_4 +
  LIGUEONE_fixtures$ligueone_6_0 + LIGUEONE_fixtures$ligueone_6_1 + LIGUEONE_fixtures$ligueone_6_2 + LIGUEONE_fixtures$ligueone_6_3 + LIGUEONE_fixtures$ligueone_6_4 +
  LIGUEONE_fixtures$ligueone_6_5
LIGUEONE_fixtures_clone$Hwinodds <- round(1/LIGUEONE_fixtures_clone$Hwinodds, digits = 3)

LIGUEONE_fixtures_clone$Drawodds <-  LIGUEONE_fixtures$ligueone_0_0 + LIGUEONE_fixtures$ligueone_1_1 + LIGUEONE_fixtures$ligueone_2_2 + LIGUEONE_fixtures$ligueone_3_3 + LIGUEONE_fixtures$ligueone_4_4 +
  LIGUEONE_fixtures$ligueone_5_5 + LIGUEONE_fixtures$ligueone_6_6

LIGUEONE_fixtures_clone$Drawodds <- round(1/LIGUEONE_fixtures_clone$Drawodds, digits = 3)

LIGUEONE_fixtures_clone$Awinodds <-   LIGUEONE_fixtures$ligueone_0_1 + LIGUEONE_fixtures$ligueone_0_2 + LIGUEONE_fixtures$ligueone_1_2 + LIGUEONE_fixtures$ligueone_0_3 + LIGUEONE_fixtures$ligueone_1_3 +
  LIGUEONE_fixtures$ligueone_2_3 + LIGUEONE_fixtures$ligueone_0_4 + LIGUEONE_fixtures$ligueone_1_4 + LIGUEONE_fixtures$ligueone_2_4 + LIGUEONE_fixtures$ligueone_3_4 +
  LIGUEONE_fixtures$ligueone_0_5 + LIGUEONE_fixtures$ligueone_1_5 + LIGUEONE_fixtures$ligueone_2_5 + LIGUEONE_fixtures$ligueone_3_5 + LIGUEONE_fixtures$ligueone_4_5 +
  LIGUEONE_fixtures$ligueone_0_6 + LIGUEONE_fixtures$ligueone_1_6 + LIGUEONE_fixtures$ligueone_2_6 + LIGUEONE_fixtures$ligueone_3_6 + LIGUEONE_fixtures$ligueone_4_6 +
  LIGUEONE_fixtures$ligueone_5_6

LIGUEONE_fixtures_clone$Awinodds <- round(1/LIGUEONE_fixtures_clone$Awinodds, digits = 3)

colnames(LIGUEONE_fixtures_clone)[15] <- "CS_1-1"
colnames(LIGUEONE_fixtures_clone)[13] <- "CS_1-0"
colnames(LIGUEONE_fixtures_clone)[14] <- "CS_0-1"
colnames(LIGUEONE_fixtures_clone)[16] <- "CS_2-0"
colnames(LIGUEONE_fixtures_clone)[17] <- "CS_0-2"
colnames(LIGUEONE_fixtures_clone)[19] <- "CS_2-1"
colnames(LIGUEONE_fixtures_clone)[20] <- "CS_1-2"

LIGUEONE_fixtures_clone$`CS_1-1` <- round(1/LIGUEONE_fixtures_clone$`CS_1-1`, digits = 3)
LIGUEONE_fixtures_clone$`CS_1-0` <- round(1/LIGUEONE_fixtures_clone$`CS_1-0`, digits = 3)
LIGUEONE_fixtures_clone$`CS_0-1` <- round(1/LIGUEONE_fixtures_clone$`CS_0-1`, digits = 3)
LIGUEONE_fixtures_clone$`CS_2-0` <- round(1/LIGUEONE_fixtures_clone$`CS_2-0`, digits = 3)
LIGUEONE_fixtures_clone$`CS_0-2` <- round(1/LIGUEONE_fixtures_clone$`CS_0-2`, digits = 3)
LIGUEONE_fixtures_clone$`CS_2-1` <- round(1/LIGUEONE_fixtures_clone$`CS_2-1`, digits = 3)
LIGUEONE_fixtures_clone$`CS_1-2` <- round(1/LIGUEONE_fixtures_clone$`CS_1-2`, digits = 3)

colnames(LIGUEONE_fixtures_clone)[1] <- "league"
colnames(LIGUEONE_fixtures_clone)[2] <- "Hometeam"
colnames(LIGUEONE_fixtures_clone)[3] <- "Awayteam"
colnames(LIGUEONE_fixtures_clone)[92] <- "predscore"
colnames(LIGUEONE_fixtures_clone)[64] <- "ov25"
colnames(LIGUEONE_fixtures_clone)[66] <- "ov25odds"
colnames(LIGUEONE_fixtures_clone)[65] <- "un25"
colnames(LIGUEONE_fixtures_clone)[67] <- "un25odds"
colnames(LIGUEONE_fixtures_clone)[68] <- "BTTSY"
colnames(LIGUEONE_fixtures_clone)[69] <- "BTTSN"
colnames(LIGUEONE_fixtures_clone)[70] <- "BTTSYodds"
colnames(LIGUEONE_fixtures_clone)[71] <- "BTTSNodds"

LIGUEONE_fixtures_clone <- LIGUEONE_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
LIGUEONE_fixtures_clone$matchid <- paste(LIGUEONE_fixtures_clone$Hometeam,LIGUEONE_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
LIGUEONE_fixtures_clone_final <- LIGUEONE_fixtures_clone[,-c(8,9,10,27)]
LIGUEONE_fixtures_clone_final[,'sep'] <- ''

ligueone_dmprediction <-  ligueone_picks[,c(4,5,6,7,8)]
ligueone_dmprediction[,'sep2'] <- ''

ligueone_avgyellow <- ligueone_picks[,c(9,10)]
ligueone_avgyellow[,'sep3'] <- ''

ligueone_avgcorners <- ligueone_picks[,c(11,12)]
ligueone_avgcorners[,'sep4'] <- ''

ligueone_goals <- LIGUEONE_fixtures[,c(10,11)]
ligueone_goals$ligueone_xGH <- round(ligueone_goals$ligueone_xGH, digits = 2)
ligueone_goals$ligueone_xGA <- round(ligueone_goals$ligueone_xGA, digits = 2)
ligueone_goals$ligueone_TxG <- ligueone_goals$ligueone_xGH + ligueone_goals$ligueone_xGA
ligueone_goals[,'sep5'] <- ''

ligueone_shots <- LIGUEONE_fixtures_sot[,c(10,11)]
ligueone_shots$ligueone_xHST <- round(ligueone_shots$ligueone_xHST, digits = 2)
ligueone_shots$ligueone_xAST <- round(ligueone_shots$ligueone_xAST, digits = 2)
ligueone_shots$TxSOT <- ligueone_shots$ligueone_xHST + ligueone_shots$ligueone_xAST
ligueone_shots[,'sep6'] <- ''

ligueone_fouls <- LIGUEONE_fixtures_fo[,c(10,11)]
ligueone_fouls$ligueone_xHF <- round(ligueone_fouls$ligueone_xHF, digits = 2)
ligueone_fouls$ligueone_xAF <- round(ligueone_fouls$ligueone_xAF, digits = 2)
ligueone_fouls$ligueone_TxF <- ligueone_fouls$ligueone_xHF + ligueone_fouls$ligueone_xAF

ligueone_ycpf <- ligueone_picks[,c(15,16)]
ligueone_fouls <- cbind(ligueone_fouls,ligueone_ycpf)
ligueone_fouls$HYCPF <- as.numeric(ligueone_fouls$HYCPF)
ligueone_fouls$AYCPF <- as.numeric(ligueone_fouls$AYCPF)
ligueone_fouls$x_hyc <- (ligueone_fouls$ligueone_xHF) * (ligueone_fouls$HYCPF)
ligueone_fouls$x_ayc <- (ligueone_fouls$ligueone_xAF) * (ligueone_fouls$AYCPF)
ligueone_fouls$x_TYC <- round((ligueone_fouls$x_hyc + ligueone_fouls$x_ayc),digits = 2)
ligueone_fouls[,'sep7'] <- ''

ligueone_bookings <- LIGUEONE_fixtures_yc[,c(10,11)]
ligueone_bookings$ligueone_xHYC <- round(ligueone_bookings$ligueone_xHYC, digits = 2)
ligueone_bookings$ligueone_xAYC <- round(ligueone_bookings$ligueone_xAYC, digits = 2)
ligueone_bookings$ligueone_TYcards <- ligueone_bookings$ligueone_xHYC + ligueone_bookings$ligueone_xAYC
ligueone_bookings[,'sep8'] <- ''

ligueone_corners <- LIGUEONE_fixtures_co[,c(10,11)]
ligueone_corners$ligueone_xHCOC <- round(ligueone_corners$ligueone_xHCOC, digits = 2)
ligueone_corners$ligueone_xACOC <- round(ligueone_corners$ligueone_xACOC, digits = 2)
ligueone_corners$ligueone_TCOs <- ligueone_corners$ligueone_xHCOC + ligueone_corners$ligueone_xACOC
ligueone_corners[,'sep9'] <- ''

ligueone_shotsconversion <- ligueone_picks[,c(13,14)]
ligueone_shotsconversion <- cbind(ligueone_shotsconversion,ligueone_shots)
ligueone_shotsconversion$HXSC <- as.numeric(ligueone_shotsconversion$HXSC)
ligueone_shotsconversion$AXSC <- as.numeric(ligueone_shotsconversion$AXSC)
ligueone_shotsconversion$ligueone_hXgoals <- round((ligueone_shotsconversion$HXSC * ligueone_shotsconversion$ligueone_xHST), digits = 2)
ligueone_shotsconversion$ligueone_aXgoals <- round((ligueone_shotsconversion$AXSC * ligueone_shotsconversion$ligueone_xAST), digits = 2)
ligueone_shotsconversion$Xgoals <- ligueone_shotsconversion$ligueone_hXgoals + ligueone_shotsconversion$ligueone_aXgoals
options(java.parameters = "-Xmx4g")
LIGUEONE_all <- cbind(LIGUEONE_fixtures_clone_final,ligueone_dmprediction,ligueone_avgyellow,ligueone_avgcorners,ligueone_goals,ligueone_shots,ligueone_fouls,ligueone_bookings,ligueone_corners,ligueone_shotsconversion)
unlink('Divisions/LIGUEONE.xlsx')
write.xlsx(LIGUEONE_all,'Divisions/LIGUEONE.xlsx', sheetName = "LIGUEONE_all", append = TRUE)
write.xlsx(points_ligueone,'Divisions/LIGUEONE.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(ligueone_cornertotalsv2,'Divisions/LIGUEONE.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(ligueone_goaltotalsv2,'Divisions/LIGUEONE.xlsx', sheetName = "Goaltotals", append = TRUE)
write.xlsx(ligueone_yellowtotalsv2,'Divisions/LIGUEONE.xlsx', sheetName = "Yellowtotals", append = TRUE)


##write.csv(LIGUEONE_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
