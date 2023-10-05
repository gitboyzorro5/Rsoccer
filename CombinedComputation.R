#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
#source('divisions.R')
#source('Matchday.R')
d1_currentround
#first_df <- E0_rounds[E0_rounds$e0_matchday > 32,]
#second_df <- E1_rounds[E1_rounds$e1_matchday > 40,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#LIGUETWONOW <- rbind(first_df,second_df,third_df)
LIGUETWONOW <- F2_rounds[F2_rounds$f2_matchday >= 1,]
#LIGUETWONOW <- na.omit(LIGUETWONOW)
#goaltotals v2
liguetwonow_goaltotalsv2 <- tapply(LIGUETWONOW$TG, LIGUETWONOW[c("HomeTeam", "AwayTeam")],mean)
liguetwonow_hgtotals <- rowSums(liguetwonow_goaltotalsv2, na.rm = T)
liguetwonow_agtotals <- colSums(liguetwonow_goaltotalsv2, na.rm = T)
liguetwonow_goaltotalsv2 <- cbind(liguetwonow_goaltotalsv2,liguetwonow_hgtotals,liguetwonow_agtotals)
liguetwonow_totalgoals <- liguetwonow_hgtotals + liguetwonow_agtotals
liguetwonow_goaltotalsv2 <- cbind(liguetwonow_goaltotalsv2,liguetwonow_totalgoals)
liguetwonow_teams <- sort(unique(LIGUETWONOW$HomeTeam))
liguetwonow_home_games <- c()
liguetwonow_away_games <-c()
for (i_liguetwonow in 1:length(liguetwonow_teams))
{

  liguetwonow_home_games[i_liguetwonow] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow],])
  liguetwonow_away_games[i_liguetwonow]  <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow],])

}
liguetwonow_games_played <- liguetwonow_home_games + liguetwonow_away_games
liguetwonow_goaltotalsv2 <- cbind(liguetwonow_goaltotalsv2,liguetwonow_games_played)
liguetwonow_avg_totalgoals <- round((liguetwonow_totalgoals/ liguetwonow_games_played), digits = 4)
liguetwonow_goaltotalsv2[is.na(liguetwonow_goaltotalsv2)] <- ""
liguetwonow_goaltotalsv2 <- cbind(liguetwonow_goaltotalsv2,liguetwonow_avg_totalgoals)

############################################################################################################
#Cornertotals v2
liguetwonow_cornertotalsv2 <- tapply(LIGUETWONOW$TC, LIGUETWONOW[c("HomeTeam", "AwayTeam")],mean)
liguetwonow_hcototals <- rowSums(liguetwonow_cornertotalsv2, na.rm = T)
liguetwonow_acototals <- colSums(liguetwonow_cornertotalsv2, na.rm = T)
liguetwonow_cornertotalsv2 <- cbind(liguetwonow_cornertotalsv2,liguetwonow_hcototals,liguetwonow_acototals)
liguetwonow_totalcorners <- liguetwonow_hcototals + liguetwonow_acototals
liguetwonow_cornertotalsv2 <- cbind(liguetwonow_cornertotalsv2,liguetwonow_totalcorners)
liguetwonow_cornertotalsv2 <- cbind(liguetwonow_cornertotalsv2,liguetwonow_games_played)
liguetwonow_avg_totalcorners <- round((liguetwonow_totalcorners/ liguetwonow_games_played), digits = 4)
liguetwonow_cornertotalsv2[is.na(liguetwonow_cornertotalsv2)] <- ""
liguetwonow_cornertotalsv2 <- cbind(liguetwonow_cornertotalsv2,liguetwonow_avg_totalcorners)
############################################################################################################
#GS matrix
liguetwonow_goalscored_h <- tapply(LIGUETWONOW$FTHG, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_goalscored_a <- tapply(LIGUETWONOW$FTAG, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_goalscored_h[is.na(liguetwonow_goalscored_h)] <- ""
liguetwonow_goalscored_a[is.na(liguetwonow_goalscored_a)] <- ""
for(liguetwonow_rowhgs in 1:nrow(liguetwonow_goalscored_h)) {
  for(liguetwonow_colhgs in 1:ncol(liguetwonow_goalscored_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowags in 1:nrow(liguetwonow_goalscored_a)) {
      for(liguetwonow_colags in 1:ncol(liguetwonow_goalscored_a)) {
        ifelse(!liguetwonow_goalscored_a[liguetwonow_rowags,liguetwonow_colags]=="",liguetwonow_goalscored_h[liguetwonow_rowags,liguetwonow_colags] <- liguetwonow_goalscored_a[liguetwonow_rowags,liguetwonow_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
liguetwonow_goalconceded_h <- tapply(LIGUETWONOW$FTAG, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_goalconceded_a <- tapply(LIGUETWONOW$FTHG, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_goalconceded_h[is.na(liguetwonow_goalconceded_h)] <- ""
liguetwonow_goalconceded_a[is.na(liguetwonow_goalconceded_a)] <- ""
for(liguetwonow_rowhgc in 1:nrow(liguetwonow_goalconceded_h)) {
  for(liguetwonow_colhgc in 1:ncol(liguetwonow_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowagc in 1:nrow(liguetwonow_goalconceded_a)) {
      for(liguetwonow_colagc in 1:ncol(liguetwonow_goalconceded_a)) {
        ifelse(!liguetwonow_goalconceded_a[liguetwonow_rowagc,liguetwonow_colagc]=="",liguetwonow_goalconceded_h[liguetwonow_rowagc,liguetwonow_colagc] <- liguetwonow_goalconceded_a[liguetwonow_rowagc,liguetwonow_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
liguetwonow_totalcorners_h <- tapply(LIGUETWONOW$TC, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_totalcorners_a <- tapply(LIGUETWONOW$TC, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_totalcorners_h[is.na(liguetwonow_totalcorners_h)] <- ""
liguetwonow_totalcorners_a[is.na(liguetwonow_totalcorners_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowTC in 1:nrow(liguetwonow_totalcorners_h)) {
  for(liguetwonow_colTC in 1:ncol(liguetwonow_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowTC in 1:nrow(liguetwonow_totalcorners_a)) {
      for(liguetwonow_colTC in 1:ncol(liguetwonow_totalcorners_a)) {
        ifelse(!liguetwonow_totalcorners_a[liguetwonow_rowTC,liguetwonow_colTC]=="",liguetwonow_totalcorners_h[liguetwonow_rowTC,liguetwonow_colTC] <- liguetwonow_totalcorners_a[liguetwonow_rowTC,liguetwonow_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
liguetwonow_coawarded_h <- tapply(LIGUETWONOW$HCO, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_coawarded_a <- tapply(LIGUETWONOW$ACO, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_coawarded_h[is.na(liguetwonow_coawarded_h)] <- ""
liguetwonow_coawarded_a[is.na(liguetwonow_coawarded_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowhco in 1:nrow(liguetwonow_coawarded_h)) {
  for(liguetwonow_colhco in 1:ncol(liguetwonow_coawarded_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowaco in 1:nrow(liguetwonow_coawarded_a)) {
      for(liguetwonow_colaco in 1:ncol(liguetwonow_coawarded_a)) {
        ifelse(!liguetwonow_coawarded_a[liguetwonow_rowaco,liguetwonow_colaco]=="",liguetwonow_coawarded_h[liguetwonow_rowaco,liguetwonow_colaco] <- liguetwonow_coawarded_a[liguetwonow_rowaco,liguetwonow_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
liguetwonow_cornersconceded_h <- tapply(LIGUETWONOW$ACO, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_cornersconceded_a <- tapply(LIGUETWONOW$HCO, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_cornersconceded_h[is.na(liguetwonow_cornersconceded_h)] <- ""
liguetwonow_cornersconceded_a[is.na(liguetwonow_cornersconceded_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowhcc in 1:nrow(liguetwonow_cornersconceded_h)) {
  for(liguetwonow_colhcc in 1:ncol(liguetwonow_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowacc in 1:nrow(liguetwonow_cornersconceded_a)) {
      for(liguetwonow_colacc in 1:ncol(liguetwonow_cornersconceded_a)) {
        ifelse(!liguetwonow_cornersconceded_a[liguetwonow_rowacc,liguetwonow_colacc]=="",liguetwonow_cornersconceded_h[liguetwonow_rowacc,liguetwonow_colacc] <- liguetwonow_cornersconceded_a[liguetwonow_rowacc,liguetwonow_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
liguetwonow_coscform_h <- tapply(LIGUETWONOW$COSC, LIGUETWONOW[c("HomeTeam", "Date")],median)
liguetwonow_coscform_a <- tapply(LIGUETWONOW$COSC, LIGUETWONOW[c("AwayTeam", "Date")],median)
liguetwonow_coscform_h[is.na(liguetwonow_coscform_h)] <- ""
liguetwonow_coscform_a[is.na(liguetwonow_coscform_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowh_f_cosc in 1:nrow(liguetwonow_coscform_h)) {
  for(liguetwonow_colh_f_cosc in 1:ncol(liguetwonow_coscform_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowa_f_cosc in 1:nrow(liguetwonow_coscform_a)) {
      for(liguetwonow_cola_f_cosc in 1:ncol(liguetwonow_coscform_a)) {
        ifelse(!liguetwonow_coscform_a[liguetwonow_rowa_f_cosc,liguetwonow_cola_f_cosc]=="",liguetwonow_coscform_h[liguetwonow_rowa_f_cosc,liguetwonow_cola_f_cosc] <- liguetwonow_coscform_a[liguetwonow_rowa_f_cosc,liguetwonow_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
liguetwonow_winmargin_h <- tapply(LIGUETWONOW$FTHG - LIGUETWONOW$FTAG, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_winmargin_a <- tapply(LIGUETWONOW$FTAG - LIGUETWONOW$FTHG, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_winmargin_h[is.na(liguetwonow_winmargin_h)] <- ""
liguetwonow_winmargin_a[is.na(liguetwonow_winmargin_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowhwm in 1:nrow(liguetwonow_winmargin_h)) {
  for(liguetwonow_colhwm in 1:ncol(liguetwonow_winmargin_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowawm in 1:nrow(liguetwonow_winmargin_a)) {
      for(liguetwonow_colawm in 1:ncol(liguetwonow_winmargin_a)) {
        ifelse(!liguetwonow_winmargin_a[liguetwonow_rowawm,liguetwonow_colawm]=="",liguetwonow_winmargin_h[liguetwonow_rowawm,liguetwonow_colawm] <- liguetwonow_winmargin_a[liguetwonow_rowawm,liguetwonow_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
liguetwonow_yellowscored_h <- tapply(LIGUETWONOW$HY, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_yellowscored_a <- tapply(LIGUETWONOW$AY, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_yellowscored_h[is.na(liguetwonow_yellowscored_h)] <- ""
liguetwonow_yellowscored_a[is.na(liguetwonow_yellowscored_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowhys in 1:nrow(liguetwonow_yellowscored_h)) {
  for(liguetwonow_colhys in 1:ncol(liguetwonow_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_roways in 1:nrow(liguetwonow_yellowscored_a)) {
      for(liguetwonow_colays in 1:ncol(liguetwonow_yellowscored_a)) {
        ifelse(!liguetwonow_yellowscored_a[liguetwonow_roways,liguetwonow_colays]=="",liguetwonow_yellowscored_h[liguetwonow_roways,liguetwonow_colays] <- liguetwonow_yellowscored_a[liguetwonow_roways,liguetwonow_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
liguetwonow_redscored_h <- tapply(LIGUETWONOW$HR, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_redscored_a <- tapply(LIGUETWONOW$AR, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_redscored_h[is.na(liguetwonow_redscored_h)] <- ""
liguetwonow_redscored_a[is.na(liguetwonow_redscored_a)] <- ""
for(liguetwonow_rowhrs in 1:nrow(liguetwonow_redscored_h)) {
  for(liguetwonow_colhrs in 1:ncol(liguetwonow_redscored_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowars in 1:nrow(liguetwonow_redscored_a)) {
      for(liguetwonow_colars in 1:ncol(liguetwonow_redscored_a)) {
        ifelse(!liguetwonow_redscored_a[liguetwonow_rowars,liguetwonow_colars]=="",liguetwonow_redscored_h[liguetwonow_rowars,liguetwonow_colars] <- liguetwonow_redscored_a[liguetwonow_rowars,liguetwonow_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
liguetwonow_redtotalsv2 <- tapply(LIGUETWONOW$TR, LIGUETWONOW[c("HomeTeam", "AwayTeam")],mean)
liguetwonow_hrtotals <- rowSums(liguetwonow_redtotalsv2, na.rm = T)
liguetwonow_artotals <- colSums(liguetwonow_redtotalsv2, na.rm = T)
liguetwonow_redtotalsv2 <- cbind(liguetwonow_redtotalsv2,liguetwonow_hrtotals,liguetwonow_artotals)
liguetwonow_totalreds <- liguetwonow_hrtotals + liguetwonow_artotals
liguetwonow_redtotalsv2 <- cbind(liguetwonow_redtotalsv2,liguetwonow_totalreds)
liguetwonow_redtotalsv2 <- cbind(liguetwonow_redtotalsv2,liguetwonow_games_played)
liguetwonow_avg_totalreds <- round((liguetwonow_totalreds/ liguetwonow_games_played), digits = 4)
liguetwonow_redtotalsv2[is.na(liguetwonow_redtotalsv2)] <- ""
liguetwonow_redtotalsv2 <- cbind(liguetwonow_redtotalsv2,liguetwonow_avg_totalreds)
############################################################################################################################################################
#yellowtotals
liguetwonow_yellowtotalsv2 <- tapply(LIGUETWONOW$TY, LIGUETWONOW[c("HomeTeam", "AwayTeam")],mean)
liguetwonow_hytotals <- rowSums(liguetwonow_yellowtotalsv2, na.rm = T)
liguetwonow_aytotals <- colSums(liguetwonow_yellowtotalsv2, na.rm = T)
liguetwonow_yellowtotalsv2 <- cbind(liguetwonow_yellowtotalsv2,liguetwonow_hytotals,liguetwonow_aytotals)
liguetwonow_totalyellows <- liguetwonow_hytotals + liguetwonow_aytotals
liguetwonow_yellowtotalsv2 <- cbind(liguetwonow_yellowtotalsv2,liguetwonow_totalyellows)
liguetwonow_yellowtotalsv2 <- cbind(liguetwonow_yellowtotalsv2,liguetwonow_games_played)
liguetwonow_avg_totalyellows <- round((liguetwonow_totalyellows/ liguetwonow_games_played), digits = 4)
liguetwonow_yellowtotalsv2[is.na(liguetwonow_yellowtotalsv2)] <- ""
liguetwonow_yellowtotalsv2 <- cbind(liguetwonow_yellowtotalsv2,liguetwonow_avg_totalyellows)
##################################################################################################################################################
#team form
liguetwonow_form_h <- tapply(LIGUETWONOW$FTR, LIGUETWONOW[c("HomeTeam", "Date")],median)
liguetwonow_form_a <- tapply(LIGUETWONOW$FTR, LIGUETWONOW[c("AwayTeam", "Date")],median)
liguetwonow_form_h[is.na(liguetwonow_form_h)] <- ""
liguetwonow_form_a[is.na(liguetwonow_form_a)] <- ""
liguetwonow_form_h <- sub("A","L",liguetwonow_form_h)
liguetwonow_form_h <- sub("H","W",liguetwonow_form_h)
liguetwonow_form_a <- sub("A","W",liguetwonow_form_a)
liguetwonow_form_a <- sub("H","L",liguetwonow_form_a)
for(liguetwonow_rowh_f in 1:nrow(liguetwonow_form_h)) {
  for(liguetwonow_colh_f in 1:ncol(liguetwonow_form_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowa_f in 1:nrow(liguetwonow_form_a)) {
      for(liguetwonow_cola_f in 1:ncol(liguetwonow_form_a)) {
        ifelse(!liguetwonow_form_a[liguetwonow_rowa_f,liguetwonow_cola_f]=="",liguetwonow_form_h[liguetwonow_rowa_f,liguetwonow_cola_f] <- liguetwonow_form_a[liguetwonow_rowa_f,liguetwonow_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
liguetwonow_csform_h <- tapply(LIGUETWONOW$CS, LIGUETWONOW[c("HomeTeam", "Date")],median)
liguetwonow_csform_a <- tapply(LIGUETWONOW$CS, LIGUETWONOW[c("AwayTeam", "Date")],median)
liguetwonow_csform_h[is.na(liguetwonow_csform_h)] <- ""
liguetwonow_csform_a[is.na(liguetwonow_csform_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowh_f_cs in 1:nrow(liguetwonow_csform_h)) {
  for(liguetwonow_colh_f_cs in 1:ncol(liguetwonow_csform_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowa_f_cs in 1:nrow(liguetwonow_csform_a)) {
      for(liguetwonow_cola_f_cs in 1:ncol(liguetwonow_csform_a)) {
        ifelse(!liguetwonow_csform_a[liguetwonow_rowa_f_cs,liguetwonow_cola_f_cs]=="",liguetwonow_csform_h[liguetwonow_rowa_f_cs,liguetwonow_cola_f_cs] <- liguetwonow_csform_a[liguetwonow_rowa_f_cs,liguetwonow_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
liguetwonow_totalgoals_h <- tapply(LIGUETWONOW$TG, LIGUETWONOW[c("HomeTeam", "Date")],mean)
liguetwonow_totalgoals_a <- tapply(LIGUETWONOW$TG, LIGUETWONOW[c("AwayTeam", "Date")],mean)
liguetwonow_totalgoals_h[is.na(liguetwonow_totalgoals_h)] <- ""
liguetwonow_totalgoals_a[is.na(liguetwonow_totalgoals_a)] <- ""
for(liguetwonow_rowh in 1:nrow(liguetwonow_totalgoals_h)) {
  for(liguetwonow_colh in 1:ncol(liguetwonow_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowa in 1:nrow(liguetwonow_totalgoals_a)) {
      for(liguetwonow_cola in 1:ncol(liguetwonow_totalgoals_a)) {
        ifelse(!liguetwonow_totalgoals_a[liguetwonow_rowa,liguetwonow_cola]=="",liguetwonow_totalgoals_h[liguetwonow_rowa,liguetwonow_cola] <- liguetwonow_totalgoals_a[liguetwonow_rowa,liguetwonow_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#LIGUETWONOW
liguetwonow_un05_home <- c()
liguetwonow_un05_away <- c()
liguetwonow_ov05_home <- c()
liguetwonow_ov05_away <- c()

liguetwonow_un15_home <- c()
liguetwonow_un15_away <- c()
liguetwonow_ov15_home <- c()
liguetwonow_ov15_away <- c()

liguetwonow_un25_home <- c()
liguetwonow_un25_away <- c()
liguetwonow_ov25_home <- c()
liguetwonow_ov25_away <- c()

liguetwonow_un35_home <- c()
liguetwonow_un35_away <- c()
liguetwonow_ov35_home <- c()
liguetwonow_ov35_away <- c()

liguetwonow_un45_home <- c()
liguetwonow_un45_away <- c()
liguetwonow_ov45_home <- c()
liguetwonow_ov45_away <- c()

liguetwonow_un55_home <- c()
liguetwonow_un55_away <- c()
liguetwonow_ov55_home <- c()
liguetwonow_ov55_away <- c()

for (i_liguetwonow_tg in 1:length(liguetwonow_teams))
{

  liguetwonow_un05_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG == 0,])
  liguetwonow_un05_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG == 0,])

  liguetwonow_ov05_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG > 0,])
  liguetwonow_ov05_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG > 0,])

  liguetwonow_un15_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 1,])
  liguetwonow_un15_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 1,])

  liguetwonow_ov15_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 2,])
  liguetwonow_ov15_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 2,])

  liguetwonow_un25_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 2,])
  liguetwonow_un25_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 2,])

  liguetwonow_ov25_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >=3,])
  liguetwonow_ov25_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >=3,])

  liguetwonow_un35_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 3,])
  liguetwonow_un35_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 3,])

  liguetwonow_ov35_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 4,])
  liguetwonow_ov35_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 4,])

  liguetwonow_un45_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 4,])
  liguetwonow_un45_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 4,])

  liguetwonow_ov45_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 5,])
  liguetwonow_ov45_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 5,])

  liguetwonow_un55_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 5,])
  liguetwonow_un55_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG <= 5,])

  liguetwonow_ov55_home[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 6,])
  liguetwonow_ov55_away[i_liguetwonow_tg] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_tg] & LIGUETWONOW$TG >= 6,])


}

liguetwonow_un05 <- liguetwonow_un05_home + liguetwonow_un05_away
liguetwonow_ov05 <- liguetwonow_ov05_home + liguetwonow_ov05_away

liguetwonow_un15 <- liguetwonow_un15_home + liguetwonow_un15_away
liguetwonow_ov15 <- liguetwonow_ov15_home + liguetwonow_ov15_away

liguetwonow_un25 <- liguetwonow_un25_home + liguetwonow_un25_away
liguetwonow_ov25 <- liguetwonow_ov25_home + liguetwonow_ov25_away

liguetwonow_un35 <- liguetwonow_un35_home + liguetwonow_un35_away
liguetwonow_ov35 <- liguetwonow_ov35_home + liguetwonow_ov35_away

liguetwonow_un45 <- liguetwonow_un45_home + liguetwonow_un45_away
liguetwonow_ov45 <- liguetwonow_ov45_home + liguetwonow_ov45_away

liguetwonow_un55 <- liguetwonow_un55_home + liguetwonow_un55_away
liguetwonow_ov55 <- liguetwonow_ov55_home + liguetwonow_ov55_away

liguetwonow_ovundata <- cbind(liguetwonow_teams,liguetwonow_un05,liguetwonow_ov05,liguetwonow_un15,liguetwonow_ov15,liguetwonow_un25,liguetwonow_ov25,liguetwonow_un35,liguetwonow_ov35,liguetwonow_un45,liguetwonow_ov45,liguetwonow_un55,liguetwonow_ov55)
#################################################################################################################################################################
#team against
liguetwonow_form_team_against_h <- tapply(LIGUETWONOW$AwayTeam, LIGUETWONOW[c("HomeTeam", "Date")],median)
liguetwonow_form_team_against_a <- tapply(LIGUETWONOW$HomeTeam, LIGUETWONOW[c("AwayTeam", "Date")],median)
liguetwonow_form_team_against_h[is.na(liguetwonow_form_team_against_h)] <- ""
liguetwonow_form_team_against_a[is.na(liguetwonow_form_team_against_a)] <- ""
#LIGUETWONOW
for(liguetwonow_rowh_f_against in 1:nrow(liguetwonow_form_team_against_h)) {
  for(liguetwonow_colh_f_against in 1:ncol(liguetwonow_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(liguetwonow_rowa_f_against in 1:nrow(liguetwonow_form_team_against_a)) {
      for(liguetwonow_cola_f_against in 1:ncol(liguetwonow_form_team_against_a)) {
        ifelse(!liguetwonow_form_team_against_a[liguetwonow_rowa_f_against,liguetwonow_cola_f_against]=="",liguetwonow_form_team_against_h[liguetwonow_rowa_f_against,liguetwonow_cola_f_against] <- liguetwonow_form_team_against_a[liguetwonow_rowa_f_against,liguetwonow_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#LIGUETWONOW
#home goals scored
liguetwonow_home_gs <- aggregate(LIGUETWONOW$FTHG, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_home_gs_avg <- aggregate(LIGUETWONOW$FTHG, by = list(LIGUETWONOW$HomeTeam),mean)
liguetwonow_home_scoring <- merge(liguetwonow_home_gs,liguetwonow_home_gs_avg, by='Group.1',all = T)
names(liguetwonow_home_scoring)[names(liguetwonow_home_scoring) == "x.x"] <- "TFthg"
names(liguetwonow_home_scoring)[names(liguetwonow_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
liguetwonow_away_gs <- aggregate(LIGUETWONOW$FTAG, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
liguetwonow_away_gs_avg <- aggregate(LIGUETWONOW$FTAG, by = list(LIGUETWONOW$AwayTeam),mean)
liguetwonow_away_scoring <- merge(liguetwonow_away_gs,liguetwonow_away_gs_avg, by='Group.1',all = T)
names(liguetwonow_away_scoring)[names(liguetwonow_away_scoring) == "x.x"] <- "TFtag"
names(liguetwonow_away_scoring)[names(liguetwonow_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
liguetwonow_scoring <- merge(liguetwonow_home_scoring,liguetwonow_away_scoring,by='Group.1',all = T)
liguetwonow_scoring$TGS <- liguetwonow_scoring$TFthg + liguetwonow_scoring$TFtag

#Home shots on target
liguetwonow_home_hst <- aggregate(LIGUETWONOW$HST, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_ast <- aggregate(LIGUETWONOW$AST, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
liguetwonow_tst <- merge(liguetwonow_home_hst,liguetwonow_away_ast, by='Group.1',all = T)
names(liguetwonow_tst)[names(liguetwonow_tst) == "x.x"] <- "hst"
names(liguetwonow_tst)[names(liguetwonow_tst) == "x.y"] <- "ast"
liguetwonow_tst$TST <- liguetwonow_tst$hst + liguetwonow_tst$ast
#merge goals scored and shots on target
liguetwonow_scoring_conversion <- merge(liguetwonow_tst,liguetwonow_scoring,by='Group.1',all = T)
#add HSC ASC TSC
liguetwonow_scoring_conversion$HSTC <- percent(liguetwonow_scoring_conversion$TFthg/liguetwonow_scoring_conversion$hst, accuracy = 0.01)
liguetwonow_scoring_conversion$ASTC <- percent(liguetwonow_scoring_conversion$TFtag/liguetwonow_scoring_conversion$ast, accuracy = 0.01)
liguetwonow_scoring_conversion$TSTC <- percent(liguetwonow_scoring_conversion$TGS/liguetwonow_scoring_conversion$TST, accuracy = 0.01)
#merge games played
liguetwonow_scoring_conversion <- cbind(liguetwonow_scoring_conversion,liguetwonow_games_played)
#create the second part
#home goals conceded
liguetwonow_home_gc <- aggregate(LIGUETWONOW$FTAG, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_home_gc_avg <- aggregate(LIGUETWONOW$FTAG, by = list(LIGUETWONOW$HomeTeam),mean)
liguetwonow_home_conceding <- merge(liguetwonow_home_gc,liguetwonow_home_gc_avg, by='Group.1',all = T)
names(liguetwonow_home_conceding)[names(liguetwonow_home_conceding) == "x.x"] <- "TFthc"
names(liguetwonow_home_conceding)[names(liguetwonow_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
liguetwonow_away_gc <- aggregate(LIGUETWONOW$FTHG, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
liguetwonow_away_gc_avg <- aggregate(LIGUETWONOW$FTHG, by = list(LIGUETWONOW$AwayTeam),mean)
liguetwonow_away_conceding <- merge(liguetwonow_away_gc,liguetwonow_away_gc_avg, by='Group.1',all = T)
names(liguetwonow_away_conceding)[names(liguetwonow_away_conceding) == "x.x"] <- "TFtac"
names(liguetwonow_away_conceding)[names(liguetwonow_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
liguetwonow_conceding <- merge(liguetwonow_home_conceding,liguetwonow_away_conceding,by='Group.1',all = T)
liguetwonow_conceding$TGC <- liguetwonow_conceding$TFthc + liguetwonow_conceding$TFtac
liguetwonow_home_hst
#Home shots conceded
liguetwonow_home_hsc <- aggregate(LIGUETWONOW$AST, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_asc <- aggregate(LIGUETWONOW$HST, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
liguetwonow_tsc <- merge(liguetwonow_home_hsc,liguetwonow_away_asc, by='Group.1',all = T)
names(liguetwonow_tsc)[names(liguetwonow_tsc) == "x.x"] <- "hsc"
names(liguetwonow_tsc)[names(liguetwonow_tsc) == "x.y"] <- "asc"
liguetwonow_tsc$TSC <- liguetwonow_tsc$hsc + liguetwonow_tsc$asc
#merge goals conceded and shots conceded
liguetwonow_conceding_conversion <- merge(liguetwonow_tsc,liguetwonow_conceding,by='Group.1',all = T)

#add HSC ASC TSC
liguetwonow_conceding_conversion$HSCC <- percent(liguetwonow_conceding_conversion$TFthc/liguetwonow_conceding_conversion$hsc, accuracy = 0.01)
liguetwonow_conceding_conversion$ASCC <- percent(liguetwonow_conceding_conversion$TFtac/liguetwonow_conceding_conversion$asc, accuracy = 0.01)
liguetwonow_conceding_conversion$TSCC <- percent(liguetwonow_conceding_conversion$TGC/liguetwonow_conceding_conversion$TSC, accuracy = 0.01)
liguetwonow_conceding_conversion$XSTC <- round(liguetwonow_scoring$TGS/(liguetwonow_tst$TST - liguetwonow_scoring$TGS), digits = 2)

#merge the two parts
liguetwonow_shots_analysis <- merge(liguetwonow_scoring_conversion,liguetwonow_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#LIGUETWONOW
#home fouls for
liguetwonow_home_fouls <- aggregate(LIGUETWONOW$HF, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_home_fouls_avg <- aggregate(LIGUETWONOW$HF, by = list(LIGUETWONOW$HomeTeam),mean)
liguetwonow_home_foulsdata <- merge(liguetwonow_home_fouls,liguetwonow_home_fouls_avg, by='Group.1',all = T)
names(liguetwonow_home_foulsdata)[names(liguetwonow_home_foulsdata) == "x.x"] <- "THfouls"
names(liguetwonow_home_foulsdata)[names(liguetwonow_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
liguetwonow_away_fouls <- aggregate(LIGUETWONOW$HF, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
liguetwonow_away_fouls_avg <- aggregate(LIGUETWONOW$HF, by = list(LIGUETWONOW$AwayTeam),mean)
liguetwonow_away_foulsdata <- merge(liguetwonow_away_fouls,liguetwonow_away_fouls_avg, by='Group.1',all = T)
names(liguetwonow_away_foulsdata)[names(liguetwonow_away_foulsdata) == "x.x"] <- "TAfouls"
names(liguetwonow_away_foulsdata)[names(liguetwonow_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
liguetwonow_fouls <- merge(liguetwonow_home_foulsdata,liguetwonow_away_foulsdata,by='Group.1',all = T)
liguetwonow_fouls$TotalFouls <- liguetwonow_fouls$THfouls + liguetwonow_fouls$TAfouls

#yellow cards
liguetwonow_home_hyc <- aggregate(LIGUETWONOW$HY, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_ayc <- aggregate(LIGUETWONOW$AY, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
liguetwonow_tyc <- merge(liguetwonow_home_hyc,liguetwonow_away_ayc, by='Group.1',all = T)
names(liguetwonow_tyc)[names(liguetwonow_tyc) == "x.x"] <- "hyc"
names(liguetwonow_tyc)[names(liguetwonow_tyc) == "x.y"] <- "ayc"
liguetwonow_tyc$TotalYellows <- liguetwonow_tyc$hyc + liguetwonow_tyc$ayc

#merge fouls for and yellow cards
liguetwonow_fouls_conversion <- merge(liguetwonow_tyc,liguetwonow_fouls,by='Group.1',all = T)
liguetwonow_fouls_conversion$YcPerfoul <- round((liguetwonow_fouls_conversion$TotalYellows/liguetwonow_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
LIGUETWONOW$Div <- "LIGUETWONOW"
##
###################################################################################################################################################
#poisson cards
liguetwonow_GP <- nrow(LIGUETWONOW)
#Calculate total home goals for each division
liguetwonow_T_HY <- sum(liguetwonow_home_hyc$x)
#calculate average home goal
liguetwonow_avg_HY <- round(liguetwonow_T_HY /liguetwonow_GP, digits = 4)
############################################################
#Calculate total away goals for each division
liguetwonow_T_AY <- sum(liguetwonow_away_ayc$x)
#calculate average away goal
liguetwonow_avg_AY <- round(liguetwonow_T_AY /liguetwonow_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
liguetwonow_home_yas <- round(((liguetwonow_home_hyc$x/liguetwonow_home_games))/liguetwonow_avg_HY, digits = 4)
#calculate away attack strength
liguetwonow_away_yas <- round(((liguetwonow_away_ayc$x/liguetwonow_away_games))/liguetwonow_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
liguetwonow_avg_HYC <- round(liguetwonow_T_AY /liguetwonow_GP, digits = 4)
#avg away concede
liguetwonow_avg_AYC <- round(liguetwonow_T_HY /liguetwonow_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
liguetwonow_home_ycc <- aggregate(LIGUETWONOW$AY, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_ycc <- aggregate(LIGUETWONOW$HY, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
#home defense strength
liguetwonow_home_yds <- round(((liguetwonow_home_ycc$x/liguetwonow_home_games))/liguetwonow_avg_HYC, digits = 4)
#away defense strength
liguetwonow_away_yds <- round(((liguetwonow_away_ycc$x/liguetwonow_away_games))/liguetwonow_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_home_poisson_yc <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_HY,liguetwonow_home_yas,liguetwonow_home_yds)
#away poisson data
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_away_poisson_yc <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_AY,liguetwonow_away_yas,liguetwonow_away_yds)
###
HomeTeam_liguetwonow_yc <- rep(liguetwonow_teams, each = length(liguetwonow_teams))
AwayTeam_liguetwonow_yc <- rep(liguetwonow_teams, length(liguetwonow_teams))
LIGUETWONOW_fixtures_yc <- cbind(HomeTeam_liguetwonow_yc,AwayTeam_liguetwonow_yc)
LIGUETWONOW_fixtures_yc <- as.data.frame(LIGUETWONOW_fixtures_yc)
LIGUETWONOW_fixtures_yc <- LIGUETWONOW_fixtures_yc[!LIGUETWONOW_fixtures_yc$HomeTeam_liguetwonow_yc == LIGUETWONOW_fixtures_yc$AwayTeam_liguetwonow_yc,]
rownames(LIGUETWONOW_fixtures_yc) <- NULL
LIGUETWONOW_fixtures_yc$Div <- "LIGUETWONOW"
LIGUETWONOW_fixtures_yc <- LIGUETWONOW_fixtures_yc[,c(3,1,2)]

LIGUETWONOW_fixtures_yc$avg_HY_liguetwonow <- liguetwonow_avg_HY

LIGUETWONOW_fixtures_yc$liguetwonow_homeyas <- rep(liguetwonow_home_yas,each = length(liguetwonow_teams)-1)

liguetwonow_awayyds_lookup <- cbind(liguetwonow_teams,liguetwonow_away_yds)

liguetwonow_awayyds_lookup <- as.data.frame(liguetwonow_awayyds_lookup)

colnames(liguetwonow_awayyds_lookup) <- c("AwayTeam_liguetwonow_yc","liguetwonow_awayyds")


require('RH2')
LIGUETWONOW_fixtures_yc$liguetwonow_awayyds <- sqldf("SELECT liguetwonow_awayyds_lookup.liguetwonow_awayyds FROM liguetwonow_awayyds_lookup INNER JOIN LIGUETWONOW_fixtures_yc ON liguetwonow_awayyds_lookup.AwayTeam_liguetwonow_yc = LIGUETWONOW_fixtures_yc.AwayTeam_liguetwonow_yc")

LIGUETWONOW_fixtures_yc$avg_AY_liguetwonow <- liguetwonow_avg_AY

liguetwonow_awayyas_lookup <- cbind(liguetwonow_teams,liguetwonow_away_yas)

liguetwonow_awayyas_lookup <- as.data.frame(liguetwonow_awayyas_lookup)

colnames(liguetwonow_awayyas_lookup) <- c("AwayTeam_liguetwonow_yc","liguetwonow_awayyas")

LIGUETWONOW_fixtures_yc$liguetwonow_awayyas <- sqldf("SELECT liguetwonow_awayyas_lookup.liguetwonow_awayyas FROM liguetwonow_awayyas_lookup INNER JOIN LIGUETWONOW_fixtures_yc ON liguetwonow_awayyas_lookup.AwayTeam_liguetwonow_yc = LIGUETWONOW_fixtures_yc.AwayTeam_liguetwonow_yc")

LIGUETWONOW_fixtures_yc$liguetwonow_homeyds <- rep(liguetwonow_home_yds,each = length(liguetwonow_teams)-1)

LIGUETWONOW_fixtures_yc$liguetwonow_awayyds <- as.numeric(unlist(LIGUETWONOW_fixtures_yc$liguetwonow_awayyds))
#xGH
LIGUETWONOW_fixtures_yc$liguetwonow_xHYC <- LIGUETWONOW_fixtures_yc$avg_HY_liguetwonow * LIGUETWONOW_fixtures_yc$liguetwonow_homeyas * LIGUETWONOW_fixtures_yc$liguetwonow_awayyds
#xGA

LIGUETWONOW_fixtures_yc$liguetwonow_awayyas <- as.numeric(unlist(LIGUETWONOW_fixtures_yc$liguetwonow_awayyas))

LIGUETWONOW_fixtures_yc$liguetwonow_xAYC <- LIGUETWONOW_fixtures_yc$avg_AY_liguetwonow * LIGUETWONOW_fixtures_yc$liguetwonow_awayyas * LIGUETWONOW_fixtures_yc$liguetwonow_homeyds

LIGUETWONOW_fixtures_yc$liguetwonow_0_0 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_0 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_0_1 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_1 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_0 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_0_2 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_2 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_1 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_2 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_3 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_0 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_1 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_2 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_0_3 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_3 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_3 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_4 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_0 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_1 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_2 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_3 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_0_4 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_4 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_4 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_4 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_5 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_0 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_1 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_2 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_3 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_4 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_0_5 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_5 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_5 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_5 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_5 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_6 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_0 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_1 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_2 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_3 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_4 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_6_5 <- round(stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_0_6 <- round(stats::dpois(0,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_1_6 <- round(stats::dpois(1,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_2_6 <- round(stats::dpois(2,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_3_6 <- round(stats::dpois(3,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_4_6 <- round(stats::dpois(4,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
LIGUETWONOW_fixtures_yc$liguetwonow_5_6 <- round(stats::dpois(5,LIGUETWONOW_fixtures_yc$liguetwonow_xHYC) * stats::dpois(6,LIGUETWONOW_fixtures_yc$liguetwonow_xAYC), digits = 4)
#Home win
LIGUETWONOW_fixtures_yc$liguetwonow_H <- (
  LIGUETWONOW_fixtures_yc$liguetwonow_1_0 + LIGUETWONOW_fixtures_yc$liguetwonow_2_0 + LIGUETWONOW_fixtures_yc$liguetwonow_2_1 + LIGUETWONOW_fixtures_yc$liguetwonow_3_0 + LIGUETWONOW_fixtures_yc$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_yc$liguetwonow_3_2 + LIGUETWONOW_fixtures_yc$liguetwonow_4_0 + LIGUETWONOW_fixtures_yc$liguetwonow_4_1 + LIGUETWONOW_fixtures_yc$liguetwonow_4_2 + LIGUETWONOW_fixtures_yc$liguetwonow_4_3 +
    LIGUETWONOW_fixtures_yc$liguetwonow_5_0 + LIGUETWONOW_fixtures_yc$liguetwonow_5_1 + LIGUETWONOW_fixtures_yc$liguetwonow_5_2 + LIGUETWONOW_fixtures_yc$liguetwonow_5_3 + LIGUETWONOW_fixtures_yc$liguetwonow_5_4 +
    LIGUETWONOW_fixtures_yc$liguetwonow_6_0 + LIGUETWONOW_fixtures_yc$liguetwonow_6_1 + LIGUETWONOW_fixtures_yc$liguetwonow_6_2 + LIGUETWONOW_fixtures_yc$liguetwonow_6_3 + LIGUETWONOW_fixtures_yc$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_yc$liguetwonow_6_5
)

LIGUETWONOW_fixtures_yc$liguetwonow_H <- percent(LIGUETWONOW_fixtures_yc$liguetwonow_H, accuracy = 0.1)

#Draw
LIGUETWONOW_fixtures_yc$liguetwonow_D <- (

  LIGUETWONOW_fixtures_yc$liguetwonow_0_0 + LIGUETWONOW_fixtures_yc$liguetwonow_1_1 + LIGUETWONOW_fixtures_yc$liguetwonow_2_2 + LIGUETWONOW_fixtures_yc$liguetwonow_3_3 + LIGUETWONOW_fixtures_yc$liguetwonow_4_4 +
    LIGUETWONOW_fixtures_yc$liguetwonow_5_5 + LIGUETWONOW_fixtures_yc$liguetwonow_6_6
)

LIGUETWONOW_fixtures_yc$liguetwonow_D <- percent(LIGUETWONOW_fixtures_yc$liguetwonow_D, accuracy = 0.1)

#Away

LIGUETWONOW_fixtures_yc$liguetwonow_A <- (
  LIGUETWONOW_fixtures_yc$liguetwonow_0_1 + LIGUETWONOW_fixtures_yc$liguetwonow_0_2 + LIGUETWONOW_fixtures_yc$liguetwonow_1_2 + LIGUETWONOW_fixtures_yc$liguetwonow_0_3 + LIGUETWONOW_fixtures_yc$liguetwonow_1_3 +
    LIGUETWONOW_fixtures_yc$liguetwonow_2_3 + LIGUETWONOW_fixtures_yc$liguetwonow_0_4 + LIGUETWONOW_fixtures_yc$liguetwonow_1_4 + LIGUETWONOW_fixtures_yc$liguetwonow_2_4 + LIGUETWONOW_fixtures_yc$liguetwonow_3_4 +
    LIGUETWONOW_fixtures_yc$liguetwonow_0_5 + LIGUETWONOW_fixtures_yc$liguetwonow_1_5 + LIGUETWONOW_fixtures_yc$liguetwonow_2_5 + LIGUETWONOW_fixtures_yc$liguetwonow_3_5 + LIGUETWONOW_fixtures_yc$liguetwonow_4_5 +
    LIGUETWONOW_fixtures_yc$liguetwonow_0_6 + LIGUETWONOW_fixtures_yc$liguetwonow_1_6 + LIGUETWONOW_fixtures_yc$liguetwonow_2_6 + LIGUETWONOW_fixtures_yc$liguetwonow_3_6 + LIGUETWONOW_fixtures_yc$liguetwonow_4_6 +
    LIGUETWONOW_fixtures_yc$liguetwonow_5_6
)

LIGUETWONOW_fixtures_yc$liguetwonow_A <- percent(LIGUETWONOW_fixtures_yc$liguetwonow_A, accuracy = 0.1)

#ov25
LIGUETWONOW_fixtures_yc$liguetwonow_ov25 <- (
  LIGUETWONOW_fixtures_yc$liguetwonow_2_1 + LIGUETWONOW_fixtures_yc$liguetwonow_1_2 + LIGUETWONOW_fixtures_yc$liguetwonow_2_2 + LIGUETWONOW_fixtures_yc$liguetwonow_3_0 + LIGUETWONOW_fixtures_yc$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_yc$liguetwonow_3_2 + LIGUETWONOW_fixtures_yc$liguetwonow_0_3 + LIGUETWONOW_fixtures_yc$liguetwonow_1_3 + LIGUETWONOW_fixtures_yc$liguetwonow_2_3 + LIGUETWONOW_fixtures_yc$liguetwonow_3_3 +
    LIGUETWONOW_fixtures_yc$liguetwonow_4_0 + LIGUETWONOW_fixtures_yc$liguetwonow_4_1 + LIGUETWONOW_fixtures_yc$liguetwonow_4_2 + LIGUETWONOW_fixtures_yc$liguetwonow_4_3 + LIGUETWONOW_fixtures_yc$liguetwonow_0_4 +
    LIGUETWONOW_fixtures_yc$liguetwonow_1_4 + LIGUETWONOW_fixtures_yc$liguetwonow_2_4 + LIGUETWONOW_fixtures_yc$liguetwonow_3_4 + LIGUETWONOW_fixtures_yc$liguetwonow_4_4 + LIGUETWONOW_fixtures_yc$liguetwonow_5_0 +
    LIGUETWONOW_fixtures_yc$liguetwonow_5_1 + LIGUETWONOW_fixtures_yc$liguetwonow_5_2 + LIGUETWONOW_fixtures_yc$liguetwonow_5_3 + LIGUETWONOW_fixtures_yc$liguetwonow_5_4 + LIGUETWONOW_fixtures_yc$liguetwonow_0_5 +
    LIGUETWONOW_fixtures_yc$liguetwonow_1_5 + LIGUETWONOW_fixtures_yc$liguetwonow_2_5 + LIGUETWONOW_fixtures_yc$liguetwonow_3_5 + LIGUETWONOW_fixtures_yc$liguetwonow_4_5 + LIGUETWONOW_fixtures_yc$liguetwonow_5_5 +
    LIGUETWONOW_fixtures_yc$liguetwonow_6_0 + LIGUETWONOW_fixtures_yc$liguetwonow_6_1 + LIGUETWONOW_fixtures_yc$liguetwonow_6_2 + LIGUETWONOW_fixtures_yc$liguetwonow_6_3 + LIGUETWONOW_fixtures_yc$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_yc$liguetwonow_6_5 + LIGUETWONOW_fixtures_yc$liguetwonow_0_6 + LIGUETWONOW_fixtures_yc$liguetwonow_1_6 + LIGUETWONOW_fixtures_yc$liguetwonow_2_6 + LIGUETWONOW_fixtures_yc$liguetwonow_3_6 +
    LIGUETWONOW_fixtures_yc$liguetwonow_4_6 + LIGUETWONOW_fixtures_yc$liguetwonow_5_6 + LIGUETWONOW_fixtures_yc$liguetwonow_6_6
)
#un25
LIGUETWONOW_fixtures_yc$liguetwonow_un25 <- (
  LIGUETWONOW_fixtures_yc$liguetwonow_0_0 + LIGUETWONOW_fixtures_yc$liguetwonow_1_0 + LIGUETWONOW_fixtures_yc$liguetwonow_0_1 + LIGUETWONOW_fixtures_yc$liguetwonow_1_1 + LIGUETWONOW_fixtures_yc$liguetwonow_2_0 + LIGUETWONOW_fixtures_yc$liguetwonow_0_2
)
#odds
LIGUETWONOW_fixtures_yc$liguetwonow_ov25_odds <- round((1/LIGUETWONOW_fixtures_yc$liguetwonow_ov25),digits = 2)
LIGUETWONOW_fixtures_yc$liguetwonow_un25_odds <- round((1/LIGUETWONOW_fixtures_yc$liguetwonow_un25),digits = 2)

LIGUETWONOW_fixtures_yc$liguetwonow_ov25_odds
LIGUETWONOW_fixtures_yc$liguetwonow_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUETWONOW_fixtures_yc$liguetwonow_ov25 <- percent(LIGUETWONOW_fixtures_yc$liguetwonow_ov25, accuracy = 0.1)

LIGUETWONOW_fixtures_yc$liguetwonow_un25 <- percent(LIGUETWONOW_fixtures_yc$liguetwonow_un25, accuracy = 0.1)
LIGUETWONOW_fixtures_yc$liguetwonow_pscore <- paste(round(LIGUETWONOW_fixtures_yc$liguetwonow_xHYC,digits = 0),round(LIGUETWONOW_fixtures_yc$liguetwonow_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
liguetwonow_GP <- nrow(LIGUETWONOW)
#Calculate total home corners for each division
liguetwonow_home_corners <- aggregate(LIGUETWONOW$HCO, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_corners <- aggregate(LIGUETWONOW$ACO, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
###############################################################################
liguetwonow_T_HCO <- sum(liguetwonow_home_corners$x)
#calculate average home corners
liguetwonow_avg_HCO <- round(liguetwonow_T_HCO /liguetwonow_GP, digits = 4)
############################################################
#Calculate total away goals for each division
liguetwonow_T_ACO <- sum(liguetwonow_away_corners$x)
#calculate average away goal
liguetwonow_avg_ACO <- round(liguetwonow_T_ACO /liguetwonow_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
liguetwonow_home_coas <- round(((liguetwonow_home_corners$x/liguetwonow_home_games))/liguetwonow_avg_HCO, digits = 4)
#calculate away attack strength
liguetwonow_away_coas <- round(((liguetwonow_away_corners$x/liguetwonow_away_games))/liguetwonow_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
liguetwonow_avg_HCOC <- round(liguetwonow_T_ACO /liguetwonow_GP, digits = 4)
#avg away concede
liguetwonow_avg_ACOC <- round(liguetwonow_T_HCO /liguetwonow_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
liguetwonow_home_coc <- aggregate(LIGUETWONOW$ACO, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_coc <- aggregate(LIGUETWONOW$HCO, by = list(LIGUETWONOW$AwayTeam), FUN = sum)
#home defense strength
liguetwonow_home_cods <- round(((liguetwonow_home_coc$x/liguetwonow_home_games))/liguetwonow_avg_HCOC, digits = 4)
#away defense strength
liguetwonow_away_cods <- round(((liguetwonow_away_coc$x/liguetwonow_away_games))/liguetwonow_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_home_poisson_corners <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_HCO,liguetwonow_home_coas,liguetwonow_home_cods)
#################################################################################
#away poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_away_poisson_corners <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_ACO,liguetwonow_away_coas,liguetwonow_away_cods)

#LIGUETWONOW
HomeTeam_liguetwonow_co <- rep(liguetwonow_teams, each = length(liguetwonow_teams))
AwayTeam_liguetwonow_co <- rep(liguetwonow_teams, length(liguetwonow_teams))
LIGUETWONOW_fixtures_co <- cbind(HomeTeam_liguetwonow_co,AwayTeam_liguetwonow_co)
LIGUETWONOW_fixtures_co <- as.data.frame(LIGUETWONOW_fixtures_co)
LIGUETWONOW_fixtures_co <- LIGUETWONOW_fixtures_co[!LIGUETWONOW_fixtures_co$HomeTeam_liguetwonow_co == LIGUETWONOW_fixtures_co$AwayTeam_liguetwonow_co,]
rownames(LIGUETWONOW_fixtures_co) <- NULL
LIGUETWONOW_fixtures_co$Div <- "LIGUETWONOW"
LIGUETWONOW_fixtures_co <- LIGUETWONOW_fixtures_co[,c(3,1,2)]

LIGUETWONOW_fixtures_co$avg_HCO_liguetwonow <- liguetwonow_avg_HCO

LIGUETWONOW_fixtures_co$liguetwonow_homecoas <- rep(liguetwonow_home_coas,each = length(liguetwonow_teams)-1)

liguetwonow_awaycods_lookup <- cbind(liguetwonow_teams,liguetwonow_away_cods)

liguetwonow_awaycods_lookup <- as.data.frame(liguetwonow_awaycods_lookup)

colnames(liguetwonow_awaycods_lookup) <- c("AwayTeam_liguetwonow_co","liguetwonow_awaycods")


require('RH2')
LIGUETWONOW_fixtures_co$liguetwonow_awaycods <- sqldf("SELECT liguetwonow_awaycods_lookup.liguetwonow_awaycods FROM liguetwonow_awaycods_lookup INNER JOIN LIGUETWONOW_fixtures_co ON liguetwonow_awaycods_lookup.AwayTeam_liguetwonow_co = LIGUETWONOW_fixtures_co.AwayTeam_liguetwonow_co")

LIGUETWONOW_fixtures_co$avg_ACO_liguetwonow <- liguetwonow_avg_ACO

liguetwonow_awaycoas_lookup <- cbind(liguetwonow_teams,liguetwonow_away_coas)

liguetwonow_awaycoas_lookup <- as.data.frame(liguetwonow_awaycoas_lookup)

colnames(liguetwonow_awaycoas_lookup) <- c("AwayTeam_liguetwonow_co","liguetwonow_awaycoas")

LIGUETWONOW_fixtures_co$liguetwonow_awaycoas <- sqldf("SELECT liguetwonow_awaycoas_lookup.liguetwonow_awaycoas FROM liguetwonow_awaycoas_lookup INNER JOIN LIGUETWONOW_fixtures_co ON liguetwonow_awaycoas_lookup.AwayTeam_liguetwonow_co = LIGUETWONOW_fixtures_co.AwayTeam_liguetwonow_co")

LIGUETWONOW_fixtures_co$liguetwonow_homecods <- rep(liguetwonow_home_cods,each = length(liguetwonow_teams)-1)

LIGUETWONOW_fixtures_co$liguetwonow_awaycods <- as.numeric(unlist(LIGUETWONOW_fixtures_co$liguetwonow_awaycods))
#xGH
LIGUETWONOW_fixtures_co$liguetwonow_xHCOC <- LIGUETWONOW_fixtures_co$avg_HCO_liguetwonow * LIGUETWONOW_fixtures_co$liguetwonow_homecoas * LIGUETWONOW_fixtures_co$liguetwonow_awaycods
#xGA

LIGUETWONOW_fixtures_co$liguetwonow_awaycoas <- as.numeric(unlist(LIGUETWONOW_fixtures_co$liguetwonow_awaycoas))

LIGUETWONOW_fixtures_co$liguetwonow_xACOC <- LIGUETWONOW_fixtures_co$avg_ACO_liguetwonow * LIGUETWONOW_fixtures_co$liguetwonow_awaycoas * LIGUETWONOW_fixtures_co$liguetwonow_homecods

LIGUETWONOW_fixtures_co$liguetwonow_0_0 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_0 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_0_1 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_1 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_0 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_0_2 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_2 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_1 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_2 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_3 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_0 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_1 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_2 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_0_3 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_3 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_3 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_4 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_0 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_1 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_2 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_3 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_0_4 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_4 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_4 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_4 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_5 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_0 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_1 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_2 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_3 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_4 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_0_5 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_5 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_5 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_5 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_5 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_6 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_0 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_1 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_2 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_3 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_4 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_6_5 <- round(stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_0_6 <- round(stats::dpois(0,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_1_6 <- round(stats::dpois(1,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_2_6 <- round(stats::dpois(2,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_3_6 <- round(stats::dpois(3,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_4_6 <- round(stats::dpois(4,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
LIGUETWONOW_fixtures_co$liguetwonow_5_6 <- round(stats::dpois(5,LIGUETWONOW_fixtures_co$liguetwonow_xHCOC) * stats::dpois(6,LIGUETWONOW_fixtures_co$liguetwonow_xACOC), digits = 4)
#Home win
LIGUETWONOW_fixtures_co$liguetwonow_H <- (
  LIGUETWONOW_fixtures_co$liguetwonow_1_0 + LIGUETWONOW_fixtures_co$liguetwonow_2_0 + LIGUETWONOW_fixtures_co$liguetwonow_2_1 + LIGUETWONOW_fixtures_co$liguetwonow_3_0 + LIGUETWONOW_fixtures_co$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_co$liguetwonow_3_2 + LIGUETWONOW_fixtures_co$liguetwonow_4_0 + LIGUETWONOW_fixtures_co$liguetwonow_4_1 + LIGUETWONOW_fixtures_co$liguetwonow_4_2 + LIGUETWONOW_fixtures_co$liguetwonow_4_3 +
    LIGUETWONOW_fixtures_co$liguetwonow_5_0 + LIGUETWONOW_fixtures_co$liguetwonow_5_1 + LIGUETWONOW_fixtures_co$liguetwonow_5_2 + LIGUETWONOW_fixtures_co$liguetwonow_5_3 + LIGUETWONOW_fixtures_co$liguetwonow_5_4 +
    LIGUETWONOW_fixtures_co$liguetwonow_6_0 + LIGUETWONOW_fixtures_co$liguetwonow_6_1 + LIGUETWONOW_fixtures_co$liguetwonow_6_2 + LIGUETWONOW_fixtures_co$liguetwonow_6_3 + LIGUETWONOW_fixtures_co$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_co$liguetwonow_6_5
)

LIGUETWONOW_fixtures_co$liguetwonow_H <- percent(LIGUETWONOW_fixtures_co$liguetwonow_H, accuracy = 0.1)

#Draw
LIGUETWONOW_fixtures_co$liguetwonow_D <- (

  LIGUETWONOW_fixtures_co$liguetwonow_0_0 + LIGUETWONOW_fixtures_co$liguetwonow_1_1 + LIGUETWONOW_fixtures_co$liguetwonow_2_2 + LIGUETWONOW_fixtures_co$liguetwonow_3_3 + LIGUETWONOW_fixtures_co$liguetwonow_4_4 +
    LIGUETWONOW_fixtures_co$liguetwonow_5_5 + LIGUETWONOW_fixtures_co$liguetwonow_6_6
)

LIGUETWONOW_fixtures_co$liguetwonow_D <- percent(LIGUETWONOW_fixtures_co$liguetwonow_D, accuracy = 0.1)

#Away

LIGUETWONOW_fixtures_co$liguetwonow_A <- (
  LIGUETWONOW_fixtures_co$liguetwonow_0_1 + LIGUETWONOW_fixtures_co$liguetwonow_0_2 + LIGUETWONOW_fixtures_co$liguetwonow_1_2 + LIGUETWONOW_fixtures_co$liguetwonow_0_3 + LIGUETWONOW_fixtures_co$liguetwonow_1_3 +
    LIGUETWONOW_fixtures_co$liguetwonow_2_3 + LIGUETWONOW_fixtures_co$liguetwonow_0_4 + LIGUETWONOW_fixtures_co$liguetwonow_1_4 + LIGUETWONOW_fixtures_co$liguetwonow_2_4 + LIGUETWONOW_fixtures_co$liguetwonow_3_4 +
    LIGUETWONOW_fixtures_co$liguetwonow_0_5 + LIGUETWONOW_fixtures_co$liguetwonow_1_5 + LIGUETWONOW_fixtures_co$liguetwonow_2_5 + LIGUETWONOW_fixtures_co$liguetwonow_3_5 + LIGUETWONOW_fixtures_co$liguetwonow_4_5 +
    LIGUETWONOW_fixtures_co$liguetwonow_0_6 + LIGUETWONOW_fixtures_co$liguetwonow_1_6 + LIGUETWONOW_fixtures_co$liguetwonow_2_6 + LIGUETWONOW_fixtures_co$liguetwonow_3_6 + LIGUETWONOW_fixtures_co$liguetwonow_4_6 +
    LIGUETWONOW_fixtures_co$liguetwonow_5_6
)

LIGUETWONOW_fixtures_co$liguetwonow_A <- percent(LIGUETWONOW_fixtures_co$liguetwonow_A, accuracy = 0.1)

#ov25
LIGUETWONOW_fixtures_co$liguetwonow_ov25 <- (
  LIGUETWONOW_fixtures_co$liguetwonow_2_1 + LIGUETWONOW_fixtures_co$liguetwonow_1_2 + LIGUETWONOW_fixtures_co$liguetwonow_2_2 + LIGUETWONOW_fixtures_co$liguetwonow_3_0 + LIGUETWONOW_fixtures_co$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_co$liguetwonow_3_2 + LIGUETWONOW_fixtures_co$liguetwonow_0_3 + LIGUETWONOW_fixtures_co$liguetwonow_1_3 + LIGUETWONOW_fixtures_co$liguetwonow_2_3 + LIGUETWONOW_fixtures_co$liguetwonow_3_3 +
    LIGUETWONOW_fixtures_co$liguetwonow_4_0 + LIGUETWONOW_fixtures_co$liguetwonow_4_1 + LIGUETWONOW_fixtures_co$liguetwonow_4_2 + LIGUETWONOW_fixtures_co$liguetwonow_4_3 + LIGUETWONOW_fixtures_co$liguetwonow_0_4 +
    LIGUETWONOW_fixtures_co$liguetwonow_1_4 + LIGUETWONOW_fixtures_co$liguetwonow_2_4 + LIGUETWONOW_fixtures_co$liguetwonow_3_4 + LIGUETWONOW_fixtures_co$liguetwonow_4_4 + LIGUETWONOW_fixtures_co$liguetwonow_5_0 +
    LIGUETWONOW_fixtures_co$liguetwonow_5_1 + LIGUETWONOW_fixtures_co$liguetwonow_5_2 + LIGUETWONOW_fixtures_co$liguetwonow_5_3 + LIGUETWONOW_fixtures_co$liguetwonow_5_4 + LIGUETWONOW_fixtures_co$liguetwonow_0_5 +
    LIGUETWONOW_fixtures_co$liguetwonow_1_5 + LIGUETWONOW_fixtures_co$liguetwonow_2_5 + LIGUETWONOW_fixtures_co$liguetwonow_3_5 + LIGUETWONOW_fixtures_co$liguetwonow_4_5 + LIGUETWONOW_fixtures_co$liguetwonow_5_5 +
    LIGUETWONOW_fixtures_co$liguetwonow_6_0 + LIGUETWONOW_fixtures_co$liguetwonow_6_1 + LIGUETWONOW_fixtures_co$liguetwonow_6_2 + LIGUETWONOW_fixtures_co$liguetwonow_6_3 + LIGUETWONOW_fixtures_co$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_co$liguetwonow_6_5 + LIGUETWONOW_fixtures_co$liguetwonow_0_6 + LIGUETWONOW_fixtures_co$liguetwonow_1_6 + LIGUETWONOW_fixtures_co$liguetwonow_2_6 + LIGUETWONOW_fixtures_co$liguetwonow_3_6 +
    LIGUETWONOW_fixtures_co$liguetwonow_4_6 + LIGUETWONOW_fixtures_co$liguetwonow_5_6 + LIGUETWONOW_fixtures_co$liguetwonow_6_6
)
#un25
LIGUETWONOW_fixtures_co$liguetwonow_un25 <- (
  LIGUETWONOW_fixtures_co$liguetwonow_0_0 + LIGUETWONOW_fixtures_co$liguetwonow_1_0 + LIGUETWONOW_fixtures_co$liguetwonow_0_1 + LIGUETWONOW_fixtures_co$liguetwonow_1_1 + LIGUETWONOW_fixtures_co$liguetwonow_2_0 + LIGUETWONOW_fixtures_co$liguetwonow_0_2
)
#odds
LIGUETWONOW_fixtures_co$liguetwonow_ov25_odds <- round((1/LIGUETWONOW_fixtures_co$liguetwonow_ov25),digits = 2)
LIGUETWONOW_fixtures_co$liguetwonow_un25_odds <- round((1/LIGUETWONOW_fixtures_co$liguetwonow_un25),digits = 2)

LIGUETWONOW_fixtures_co$liguetwonow_ov25_odds
LIGUETWONOW_fixtures_co$liguetwonow_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUETWONOW_fixtures_co$liguetwonow_ov25 <- percent(LIGUETWONOW_fixtures_co$liguetwonow_ov25, accuracy = 0.1)

LIGUETWONOW_fixtures_co$liguetwonow_un25 <- percent(LIGUETWONOW_fixtures_co$liguetwonow_un25, accuracy = 0.1)
LIGUETWONOW_fixtures_co$liguetwonow_pscore <- paste(round(LIGUETWONOW_fixtures_co$liguetwonow_xHCOC,digits = 0),round(LIGUETWONOW_fixtures_co$liguetwonow_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
liguetwonow_GP <- nrow(LIGUETWONOW)
#Calculate total home goals for each division
liguetwonow_T_HF <- sum(liguetwonow_home_fouls$x)
#calculate average home goal
liguetwonow_avg_HF <- round(liguetwonow_T_HF /liguetwonow_GP, digits = 4)
############################################################
#Calculate total away goals for each division
liguetwonow_T_AF <- sum(liguetwonow_away_fouls$x)
#calculate average away goal
liguetwonow_avg_AF <- round(liguetwonow_T_AF /liguetwonow_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
liguetwonow_home_fas <- round(((liguetwonow_home_fouls$x/liguetwonow_home_games))/liguetwonow_avg_HF, digits = 4)
#calculate away attack strength
liguetwonow_away_fas <- round(((liguetwonow_away_fouls$x/liguetwonow_away_games))/liguetwonow_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
liguetwonow_avg_HFC <- round(liguetwonow_T_AF /liguetwonow_GP, digits = 4)
#avg away concede
liguetwonow_avg_AFC <- round(liguetwonow_T_HF /liguetwonow_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
liguetwonow_home_fcc <- aggregate(LIGUETWONOW$AF, by = list(LIGUETWONOW$HomeTeam), FUN = sum)
liguetwonow_away_fcc <- aggregate(LIGUETWONOW$HF, by = list(LIGUETWONOW$AwayTeam), FUN = sum)

#home defense strength
liguetwonow_home_fds <- round(((liguetwonow_home_fcc$x/liguetwonow_home_games))/liguetwonow_avg_HFC, digits = 4)

#away defense strength
liguetwonow_away_fds <- round(((liguetwonow_away_fcc$x/liguetwonow_away_games))/liguetwonow_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_home_poisson_fo <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_HF,liguetwonow_home_fas,liguetwonow_home_fds)

#################################################################################
#away poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_away_poisson_fo <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_AF,liguetwonow_away_fas,liguetwonow_away_fds)

#LIGUETWONOW
HomeTeam_liguetwonow_fo <- rep(liguetwonow_teams, each = length(liguetwonow_teams))
AwayTeam_liguetwonow_fo <- rep(liguetwonow_teams, length(liguetwonow_teams))
LIGUETWONOW_fixtures_fo <- cbind(HomeTeam_liguetwonow_fo,AwayTeam_liguetwonow_fo)
LIGUETWONOW_fixtures_fo <- as.data.frame(LIGUETWONOW_fixtures_fo)
LIGUETWONOW_fixtures_fo <- LIGUETWONOW_fixtures_fo[!LIGUETWONOW_fixtures_fo$HomeTeam_liguetwonow_fo == LIGUETWONOW_fixtures_fo$AwayTeam_liguetwonow_fo,]
rownames(LIGUETWONOW_fixtures_fo) <- NULL
LIGUETWONOW_fixtures_fo$Div <- "LIGUETWONOW"
LIGUETWONOW_fixtures_fo <- LIGUETWONOW_fixtures_fo[,c(3,1,2)]

LIGUETWONOW_fixtures_fo$avg_HF_liguetwonow <- liguetwonow_avg_HF

LIGUETWONOW_fixtures_fo$liguetwonow_homefas <- rep(liguetwonow_home_fas,each = length(liguetwonow_teams)-1)

liguetwonow_awayfds_lookup <- cbind(liguetwonow_teams,liguetwonow_away_fds)

liguetwonow_awayfds_lookup <- as.data.frame(liguetwonow_awayfds_lookup)

colnames(liguetwonow_awayfds_lookup) <- c("AwayTeam_liguetwonow_fo","liguetwonow_awayfds")


require('RH2')
LIGUETWONOW_fixtures_fo$liguetwonow_awayfds <- sqldf("SELECT liguetwonow_awayfds_lookup.liguetwonow_awayfds FROM liguetwonow_awayfds_lookup INNER JOIN LIGUETWONOW_fixtures_fo ON liguetwonow_awayfds_lookup.AwayTeam_liguetwonow_fo = LIGUETWONOW_fixtures_fo.AwayTeam_liguetwonow_fo")

LIGUETWONOW_fixtures_fo$avg_AF_liguetwonow <- liguetwonow_avg_AF

liguetwonow_awayfas_lookup <- cbind(liguetwonow_teams,liguetwonow_away_fas)

liguetwonow_awayfas_lookup <- as.data.frame(liguetwonow_awayfas_lookup)

colnames(liguetwonow_awayfas_lookup) <- c("AwayTeam_liguetwonow_fo","liguetwonow_awayfas")

LIGUETWONOW_fixtures_fo$liguetwonow_awayfas <- sqldf("SELECT liguetwonow_awayfas_lookup.liguetwonow_awayfas FROM liguetwonow_awayfas_lookup INNER JOIN LIGUETWONOW_fixtures_fo ON liguetwonow_awayfas_lookup.AwayTeam_liguetwonow_fo = LIGUETWONOW_fixtures_fo.AwayTeam_liguetwonow_fo")

LIGUETWONOW_fixtures_fo$liguetwonow_homefds <- rep(liguetwonow_home_fds,each = length(liguetwonow_teams)-1)

LIGUETWONOW_fixtures_fo$liguetwonow_awayfds <- as.numeric(unlist(LIGUETWONOW_fixtures_fo$liguetwonow_awayfds))
#xGH
LIGUETWONOW_fixtures_fo$liguetwonow_xHF <- LIGUETWONOW_fixtures_fo$avg_HF_liguetwonow * LIGUETWONOW_fixtures_fo$liguetwonow_homefas * LIGUETWONOW_fixtures_fo$liguetwonow_awayfds
#xGA

LIGUETWONOW_fixtures_fo$liguetwonow_awayfas <- as.numeric(unlist(LIGUETWONOW_fixtures_fo$liguetwonow_awayfas))

LIGUETWONOW_fixtures_fo$liguetwonow_xAF <- LIGUETWONOW_fixtures_fo$avg_AF_liguetwonow * LIGUETWONOW_fixtures_fo$liguetwonow_awayfas * LIGUETWONOW_fixtures_fo$liguetwonow_homefds

LIGUETWONOW_fixtures_fo$liguetwonow_0_0 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_0 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_0_1 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_1 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_0 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_0_2 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_2 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_1 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_2 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_3 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_0 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_1 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_2 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_0_3 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_3 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_3 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_4 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_0 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_1 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_2 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_3 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_0_4 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_4 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_4 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_4 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_5 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_0 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_1 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_2 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_3 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_4 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_0_5 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_5 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_5 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_5 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_5 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_6 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_0 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_1 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_2 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_3 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_4 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_6_5 <- round(stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_0_6 <- round(stats::dpois(0,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_1_6 <- round(stats::dpois(1,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_2_6 <- round(stats::dpois(2,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_3_6 <- round(stats::dpois(3,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_4_6 <- round(stats::dpois(4,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
LIGUETWONOW_fixtures_fo$liguetwonow_5_6 <- round(stats::dpois(5,LIGUETWONOW_fixtures_fo$liguetwonow_xHF) * stats::dpois(6,LIGUETWONOW_fixtures_fo$liguetwonow_xAF), digits = 4)
#Home win
LIGUETWONOW_fixtures_fo$liguetwonow_H <- (
  LIGUETWONOW_fixtures_fo$liguetwonow_1_0 + LIGUETWONOW_fixtures_fo$liguetwonow_2_0 + LIGUETWONOW_fixtures_fo$liguetwonow_2_1 + LIGUETWONOW_fixtures_fo$liguetwonow_3_0 + LIGUETWONOW_fixtures_fo$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_fo$liguetwonow_3_2 + LIGUETWONOW_fixtures_fo$liguetwonow_4_0 + LIGUETWONOW_fixtures_fo$liguetwonow_4_1 + LIGUETWONOW_fixtures_fo$liguetwonow_4_2 + LIGUETWONOW_fixtures_fo$liguetwonow_4_3 +
    LIGUETWONOW_fixtures_fo$liguetwonow_5_0 + LIGUETWONOW_fixtures_fo$liguetwonow_5_1 + LIGUETWONOW_fixtures_fo$liguetwonow_5_2 + LIGUETWONOW_fixtures_fo$liguetwonow_5_3 + LIGUETWONOW_fixtures_fo$liguetwonow_5_4 +
    LIGUETWONOW_fixtures_fo$liguetwonow_6_0 + LIGUETWONOW_fixtures_fo$liguetwonow_6_1 + LIGUETWONOW_fixtures_fo$liguetwonow_6_2 + LIGUETWONOW_fixtures_fo$liguetwonow_6_3 + LIGUETWONOW_fixtures_fo$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_fo$liguetwonow_6_5
)

LIGUETWONOW_fixtures_fo$liguetwonow_H <- percent(LIGUETWONOW_fixtures_fo$liguetwonow_H, accuracy = 0.1)

#Draw
LIGUETWONOW_fixtures_fo$liguetwonow_D <- (

  LIGUETWONOW_fixtures_fo$liguetwonow_0_0 + LIGUETWONOW_fixtures_fo$liguetwonow_1_1 + LIGUETWONOW_fixtures_fo$liguetwonow_2_2 + LIGUETWONOW_fixtures_fo$liguetwonow_3_3 + LIGUETWONOW_fixtures_fo$liguetwonow_4_4 +
    LIGUETWONOW_fixtures_fo$liguetwonow_5_5 + LIGUETWONOW_fixtures_fo$liguetwonow_6_6
)

LIGUETWONOW_fixtures_fo$liguetwonow_D <- percent(LIGUETWONOW_fixtures_fo$liguetwonow_D, accuracy = 0.1)

#Away

LIGUETWONOW_fixtures_fo$liguetwonow_A <- (
  LIGUETWONOW_fixtures_fo$liguetwonow_0_1 + LIGUETWONOW_fixtures_fo$liguetwonow_0_2 + LIGUETWONOW_fixtures_fo$liguetwonow_1_2 + LIGUETWONOW_fixtures_fo$liguetwonow_0_3 + LIGUETWONOW_fixtures_fo$liguetwonow_1_3 +
    LIGUETWONOW_fixtures_fo$liguetwonow_2_3 + LIGUETWONOW_fixtures_fo$liguetwonow_0_4 + LIGUETWONOW_fixtures_fo$liguetwonow_1_4 + LIGUETWONOW_fixtures_fo$liguetwonow_2_4 + LIGUETWONOW_fixtures_fo$liguetwonow_3_4 +
    LIGUETWONOW_fixtures_fo$liguetwonow_0_5 + LIGUETWONOW_fixtures_fo$liguetwonow_1_5 + LIGUETWONOW_fixtures_fo$liguetwonow_2_5 + LIGUETWONOW_fixtures_fo$liguetwonow_3_5 + LIGUETWONOW_fixtures_fo$liguetwonow_4_5 +
    LIGUETWONOW_fixtures_fo$liguetwonow_0_6 + LIGUETWONOW_fixtures_fo$liguetwonow_1_6 + LIGUETWONOW_fixtures_fo$liguetwonow_2_6 + LIGUETWONOW_fixtures_fo$liguetwonow_3_6 + LIGUETWONOW_fixtures_fo$liguetwonow_4_6 +
    LIGUETWONOW_fixtures_fo$liguetwonow_5_6
)

LIGUETWONOW_fixtures_fo$liguetwonow_A <- percent(LIGUETWONOW_fixtures_fo$liguetwonow_A, accuracy = 0.1)

#ov25
LIGUETWONOW_fixtures_fo$liguetwonow_ov25 <- (
  LIGUETWONOW_fixtures_fo$liguetwonow_2_1 + LIGUETWONOW_fixtures_fo$liguetwonow_1_2 + LIGUETWONOW_fixtures_fo$liguetwonow_2_2 + LIGUETWONOW_fixtures_fo$liguetwonow_3_0 + LIGUETWONOW_fixtures_fo$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_fo$liguetwonow_3_2 + LIGUETWONOW_fixtures_fo$liguetwonow_0_3 + LIGUETWONOW_fixtures_fo$liguetwonow_1_3 + LIGUETWONOW_fixtures_fo$liguetwonow_2_3 + LIGUETWONOW_fixtures_fo$liguetwonow_3_3 +
    LIGUETWONOW_fixtures_fo$liguetwonow_4_0 + LIGUETWONOW_fixtures_fo$liguetwonow_4_1 + LIGUETWONOW_fixtures_fo$liguetwonow_4_2 + LIGUETWONOW_fixtures_fo$liguetwonow_4_3 + LIGUETWONOW_fixtures_fo$liguetwonow_0_4 +
    LIGUETWONOW_fixtures_fo$liguetwonow_1_4 + LIGUETWONOW_fixtures_fo$liguetwonow_2_4 + LIGUETWONOW_fixtures_fo$liguetwonow_3_4 + LIGUETWONOW_fixtures_fo$liguetwonow_4_4 + LIGUETWONOW_fixtures_fo$liguetwonow_5_0 +
    LIGUETWONOW_fixtures_fo$liguetwonow_5_1 + LIGUETWONOW_fixtures_fo$liguetwonow_5_2 + LIGUETWONOW_fixtures_fo$liguetwonow_5_3 + LIGUETWONOW_fixtures_fo$liguetwonow_5_4 + LIGUETWONOW_fixtures_fo$liguetwonow_0_5 +
    LIGUETWONOW_fixtures_fo$liguetwonow_1_5 + LIGUETWONOW_fixtures_fo$liguetwonow_2_5 + LIGUETWONOW_fixtures_fo$liguetwonow_3_5 + LIGUETWONOW_fixtures_fo$liguetwonow_4_5 + LIGUETWONOW_fixtures_fo$liguetwonow_5_5 +
    LIGUETWONOW_fixtures_fo$liguetwonow_6_0 + LIGUETWONOW_fixtures_fo$liguetwonow_6_1 + LIGUETWONOW_fixtures_fo$liguetwonow_6_2 + LIGUETWONOW_fixtures_fo$liguetwonow_6_3 + LIGUETWONOW_fixtures_fo$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_fo$liguetwonow_6_5 + LIGUETWONOW_fixtures_fo$liguetwonow_0_6 + LIGUETWONOW_fixtures_fo$liguetwonow_1_6 + LIGUETWONOW_fixtures_fo$liguetwonow_2_6 + LIGUETWONOW_fixtures_fo$liguetwonow_3_6 +
    LIGUETWONOW_fixtures_fo$liguetwonow_4_6 + LIGUETWONOW_fixtures_fo$liguetwonow_5_6 + LIGUETWONOW_fixtures_fo$liguetwonow_6_6
)
#un25
LIGUETWONOW_fixtures_fo$liguetwonow_un25 <- (
  LIGUETWONOW_fixtures_fo$liguetwonow_0_0 + LIGUETWONOW_fixtures_fo$liguetwonow_1_0 + LIGUETWONOW_fixtures_fo$liguetwonow_0_1 + LIGUETWONOW_fixtures_fo$liguetwonow_1_1 + LIGUETWONOW_fixtures_fo$liguetwonow_2_0 + LIGUETWONOW_fixtures_fo$liguetwonow_0_2
)
#odds
LIGUETWONOW_fixtures_fo$liguetwonow_ov25_odds <- round((1/LIGUETWONOW_fixtures_fo$liguetwonow_ov25),digits = 2)
LIGUETWONOW_fixtures_fo$liguetwonow_un25_odds <- round((1/LIGUETWONOW_fixtures_fo$liguetwonow_un25),digits = 2)

LIGUETWONOW_fixtures_fo$liguetwonow_ov25_odds
LIGUETWONOW_fixtures_fo$liguetwonow_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUETWONOW_fixtures_fo$liguetwonow_ov25 <- percent(LIGUETWONOW_fixtures_fo$liguetwonow_ov25, accuracy = 0.1)

LIGUETWONOW_fixtures_fo$liguetwonow_un25 <- percent(LIGUETWONOW_fixtures_fo$liguetwonow_un25, accuracy = 0.1)
LIGUETWONOW_fixtures_fo$liguetwonow_psfore <- paste(round(LIGUETWONOW_fixtures_fo$liguetwonow_xHF,digits = 0),round(LIGUETWONOW_fixtures_fo$liguetwonow_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
liguetwonow_GP <- nrow(LIGUETWONOW)

#Calculate total home goals for each division
liguetwonow_T_HST <- sum(liguetwonow_home_hst$x)
#calculate average home goal

liguetwonow_avg_HST <- round(liguetwonow_T_HST /liguetwonow_GP, digits = 4)

############################################################
#Calculate total away goals for each division
liguetwonow_T_AST <- sum(liguetwonow_away_ast$x)
#calculate average away goal
liguetwonow_avg_AST <- round(liguetwonow_T_AST /liguetwonow_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
liguetwonow_home_sotas <- round(((liguetwonow_home_hst$x/liguetwonow_home_games))/liguetwonow_avg_HST, digits = 4)
#calculate away attack strength
liguetwonow_away_sotas <- round(((liguetwonow_away_ast$x/liguetwonow_away_games))/liguetwonow_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
liguetwonow_avg_HSC <- round(liguetwonow_T_AST /liguetwonow_GP, digits = 4)

#avg away concede
liguetwonow_avg_ASC <- round(liguetwonow_T_HST /liguetwonow_GP, digits = 4)
#home defense strength
liguetwonow_home_sods <- round(((liguetwonow_home_hsc$x/liguetwonow_home_games))/liguetwonow_avg_HSC, digits = 4)

#away defense strength
liguetwonow_away_sods <- round(((liguetwonow_away_ast$x/liguetwonow_away_games))/liguetwonow_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_home_poisson_sot <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_HST,liguetwonow_home_sotas,liguetwonow_home_sods)

#################################################################################
#away poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_away_poisson_sot <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_AST,liguetwonow_away_sotas,liguetwonow_away_sods)

#LIGUETWONOW
HomeTeam_liguetwonow_sot <- rep(liguetwonow_teams, each = length(liguetwonow_teams))
AwayTeam_liguetwonow_sot <- rep(liguetwonow_teams, length(liguetwonow_teams))
LIGUETWONOW_fixtures_sot <- cbind(HomeTeam_liguetwonow_sot,AwayTeam_liguetwonow_sot)
LIGUETWONOW_fixtures_sot <- as.data.frame(LIGUETWONOW_fixtures_sot)
LIGUETWONOW_fixtures_sot <- LIGUETWONOW_fixtures_sot[!LIGUETWONOW_fixtures_sot$HomeTeam_liguetwonow_sot == LIGUETWONOW_fixtures_sot$AwayTeam_liguetwonow_sot,]
rownames(LIGUETWONOW_fixtures_sot) <- NULL
LIGUETWONOW_fixtures_sot$Div <- "LIGUETWONOW"
LIGUETWONOW_fixtures_sot <- LIGUETWONOW_fixtures_sot[,c(3,1,2)]

LIGUETWONOW_fixtures_sot$avg_HST_liguetwonow <- liguetwonow_avg_HST

LIGUETWONOW_fixtures_sot$liguetwonow_homesotas <- rep(liguetwonow_home_sotas,each = length(liguetwonow_teams)-1)

liguetwonow_awaysods_lookup <- cbind(liguetwonow_teams,liguetwonow_away_sods)

liguetwonow_awaysods_lookup <- as.data.frame(liguetwonow_awaysods_lookup)

colnames(liguetwonow_awaysods_lookup) <- c("AwayTeam_liguetwonow_sot","liguetwonow_awaysods")


require('RH2')
LIGUETWONOW_fixtures_sot$liguetwonow_awaysods <- sqldf("SELECT liguetwonow_awaysods_lookup.liguetwonow_awaysods FROM liguetwonow_awaysods_lookup INNER JOIN LIGUETWONOW_fixtures_sot ON liguetwonow_awaysods_lookup.AwayTeam_liguetwonow_sot = LIGUETWONOW_fixtures_sot.AwayTeam_liguetwonow_sot")

LIGUETWONOW_fixtures_sot$avg_AST_liguetwonow <- liguetwonow_avg_AST

liguetwonow_awaysotas_lookup <- cbind(liguetwonow_teams,liguetwonow_away_sotas)

liguetwonow_awaysotas_lookup <- as.data.frame(liguetwonow_awaysotas_lookup)

colnames(liguetwonow_awaysotas_lookup) <- c("AwayTeam_liguetwonow_sot","liguetwonow_awaysotas")

LIGUETWONOW_fixtures_sot$liguetwonow_awaysotas <- sqldf("SELECT liguetwonow_awaysotas_lookup.liguetwonow_awaysotas FROM liguetwonow_awaysotas_lookup INNER JOIN LIGUETWONOW_fixtures_sot ON liguetwonow_awaysotas_lookup.AwayTeam_liguetwonow_sot = LIGUETWONOW_fixtures_sot.AwayTeam_liguetwonow_sot")

LIGUETWONOW_fixtures_sot$liguetwonow_homesods <- rep(liguetwonow_home_sods,each = length(liguetwonow_teams)-1)

LIGUETWONOW_fixtures_sot$liguetwonow_awaysods <- as.numeric(unlist(LIGUETWONOW_fixtures_sot$liguetwonow_awaysods))
#xGH
LIGUETWONOW_fixtures_sot$liguetwonow_xHST <- LIGUETWONOW_fixtures_sot$avg_HST_liguetwonow * LIGUETWONOW_fixtures_sot$liguetwonow_homesotas * LIGUETWONOW_fixtures_sot$liguetwonow_awaysods
#xGA

LIGUETWONOW_fixtures_sot$liguetwonow_awaysotas <- as.numeric(unlist(LIGUETWONOW_fixtures_sot$liguetwonow_awaysotas))

LIGUETWONOW_fixtures_sot$liguetwonow_xAST <- LIGUETWONOW_fixtures_sot$avg_AST_liguetwonow * LIGUETWONOW_fixtures_sot$liguetwonow_awaysotas * LIGUETWONOW_fixtures_sot$liguetwonow_homesods

LIGUETWONOW_fixtures_sot$liguetwonow_0_0 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_0 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_0_1 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_1 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_0 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_0_2 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_2 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_1 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_2 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_3 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_0 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_1 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_2 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_0_3 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_3 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_3 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_4 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_0 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_1 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_2 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_3 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_0_4 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_4 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_4 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_4 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_5 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_0 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_1 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_2 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_3 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_4 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_0_5 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_5 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_5 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_5 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_5 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_6 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_0 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_1 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_2 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_3 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_4 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_6_5 <- round(stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_0_6 <- round(stats::dpois(0,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_1_6 <- round(stats::dpois(1,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_2_6 <- round(stats::dpois(2,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_3_6 <- round(stats::dpois(3,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_4_6 <- round(stats::dpois(4,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
LIGUETWONOW_fixtures_sot$liguetwonow_5_6 <- round(stats::dpois(5,LIGUETWONOW_fixtures_sot$liguetwonow_xHST) * stats::dpois(6,LIGUETWONOW_fixtures_sot$liguetwonow_xAST), digits = 4)
#Home win
LIGUETWONOW_fixtures_sot$liguetwonow_H <- (
  LIGUETWONOW_fixtures_sot$liguetwonow_1_0 + LIGUETWONOW_fixtures_sot$liguetwonow_2_0 + LIGUETWONOW_fixtures_sot$liguetwonow_2_1 + LIGUETWONOW_fixtures_sot$liguetwonow_3_0 + LIGUETWONOW_fixtures_sot$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_sot$liguetwonow_3_2 + LIGUETWONOW_fixtures_sot$liguetwonow_4_0 + LIGUETWONOW_fixtures_sot$liguetwonow_4_1 + LIGUETWONOW_fixtures_sot$liguetwonow_4_2 + LIGUETWONOW_fixtures_sot$liguetwonow_4_3 +
    LIGUETWONOW_fixtures_sot$liguetwonow_5_0 + LIGUETWONOW_fixtures_sot$liguetwonow_5_1 + LIGUETWONOW_fixtures_sot$liguetwonow_5_2 + LIGUETWONOW_fixtures_sot$liguetwonow_5_3 + LIGUETWONOW_fixtures_sot$liguetwonow_5_4 +
    LIGUETWONOW_fixtures_sot$liguetwonow_6_0 + LIGUETWONOW_fixtures_sot$liguetwonow_6_1 + LIGUETWONOW_fixtures_sot$liguetwonow_6_2 + LIGUETWONOW_fixtures_sot$liguetwonow_6_3 + LIGUETWONOW_fixtures_sot$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_sot$liguetwonow_6_5
)

LIGUETWONOW_fixtures_sot$liguetwonow_H <- percent(LIGUETWONOW_fixtures_sot$liguetwonow_H, accuracy = 0.1)

#Draw
LIGUETWONOW_fixtures_sot$liguetwonow_D <- (

  LIGUETWONOW_fixtures_sot$liguetwonow_0_0 + LIGUETWONOW_fixtures_sot$liguetwonow_1_1 + LIGUETWONOW_fixtures_sot$liguetwonow_2_2 + LIGUETWONOW_fixtures_sot$liguetwonow_3_3 + LIGUETWONOW_fixtures_sot$liguetwonow_4_4 +
    LIGUETWONOW_fixtures_sot$liguetwonow_5_5 + LIGUETWONOW_fixtures_sot$liguetwonow_6_6
)

LIGUETWONOW_fixtures_sot$liguetwonow_D <- percent(LIGUETWONOW_fixtures_sot$liguetwonow_D, accuracy = 0.1)

#Away

LIGUETWONOW_fixtures_sot$liguetwonow_A <- (
  LIGUETWONOW_fixtures_sot$liguetwonow_0_1 + LIGUETWONOW_fixtures_sot$liguetwonow_0_2 + LIGUETWONOW_fixtures_sot$liguetwonow_1_2 + LIGUETWONOW_fixtures_sot$liguetwonow_0_3 + LIGUETWONOW_fixtures_sot$liguetwonow_1_3 +
    LIGUETWONOW_fixtures_sot$liguetwonow_2_3 + LIGUETWONOW_fixtures_sot$liguetwonow_0_4 + LIGUETWONOW_fixtures_sot$liguetwonow_1_4 + LIGUETWONOW_fixtures_sot$liguetwonow_2_4 + LIGUETWONOW_fixtures_sot$liguetwonow_3_4 +
    LIGUETWONOW_fixtures_sot$liguetwonow_0_5 + LIGUETWONOW_fixtures_sot$liguetwonow_1_5 + LIGUETWONOW_fixtures_sot$liguetwonow_2_5 + LIGUETWONOW_fixtures_sot$liguetwonow_3_5 + LIGUETWONOW_fixtures_sot$liguetwonow_4_5 +
    LIGUETWONOW_fixtures_sot$liguetwonow_0_6 + LIGUETWONOW_fixtures_sot$liguetwonow_1_6 + LIGUETWONOW_fixtures_sot$liguetwonow_2_6 + LIGUETWONOW_fixtures_sot$liguetwonow_3_6 + LIGUETWONOW_fixtures_sot$liguetwonow_4_6 +
    LIGUETWONOW_fixtures_sot$liguetwonow_5_6
)

LIGUETWONOW_fixtures_sot$liguetwonow_A <- percent(LIGUETWONOW_fixtures_sot$liguetwonow_A, accuracy = 0.1)

#ov25
LIGUETWONOW_fixtures_sot$liguetwonow_ov25 <- (
  LIGUETWONOW_fixtures_sot$liguetwonow_2_1 + LIGUETWONOW_fixtures_sot$liguetwonow_1_2 + LIGUETWONOW_fixtures_sot$liguetwonow_2_2 + LIGUETWONOW_fixtures_sot$liguetwonow_3_0 + LIGUETWONOW_fixtures_sot$liguetwonow_3_1 +
    LIGUETWONOW_fixtures_sot$liguetwonow_3_2 + LIGUETWONOW_fixtures_sot$liguetwonow_0_3 + LIGUETWONOW_fixtures_sot$liguetwonow_1_3 + LIGUETWONOW_fixtures_sot$liguetwonow_2_3 + LIGUETWONOW_fixtures_sot$liguetwonow_3_3 +
    LIGUETWONOW_fixtures_sot$liguetwonow_4_0 + LIGUETWONOW_fixtures_sot$liguetwonow_4_1 + LIGUETWONOW_fixtures_sot$liguetwonow_4_2 + LIGUETWONOW_fixtures_sot$liguetwonow_4_3 + LIGUETWONOW_fixtures_sot$liguetwonow_0_4 +
    LIGUETWONOW_fixtures_sot$liguetwonow_1_4 + LIGUETWONOW_fixtures_sot$liguetwonow_2_4 + LIGUETWONOW_fixtures_sot$liguetwonow_3_4 + LIGUETWONOW_fixtures_sot$liguetwonow_4_4 + LIGUETWONOW_fixtures_sot$liguetwonow_5_0 +
    LIGUETWONOW_fixtures_sot$liguetwonow_5_1 + LIGUETWONOW_fixtures_sot$liguetwonow_5_2 + LIGUETWONOW_fixtures_sot$liguetwonow_5_3 + LIGUETWONOW_fixtures_sot$liguetwonow_5_4 + LIGUETWONOW_fixtures_sot$liguetwonow_0_5 +
    LIGUETWONOW_fixtures_sot$liguetwonow_1_5 + LIGUETWONOW_fixtures_sot$liguetwonow_2_5 + LIGUETWONOW_fixtures_sot$liguetwonow_3_5 + LIGUETWONOW_fixtures_sot$liguetwonow_4_5 + LIGUETWONOW_fixtures_sot$liguetwonow_5_5 +
    LIGUETWONOW_fixtures_sot$liguetwonow_6_0 + LIGUETWONOW_fixtures_sot$liguetwonow_6_1 + LIGUETWONOW_fixtures_sot$liguetwonow_6_2 + LIGUETWONOW_fixtures_sot$liguetwonow_6_3 + LIGUETWONOW_fixtures_sot$liguetwonow_6_4 +
    LIGUETWONOW_fixtures_sot$liguetwonow_6_5 + LIGUETWONOW_fixtures_sot$liguetwonow_0_6 + LIGUETWONOW_fixtures_sot$liguetwonow_1_6 + LIGUETWONOW_fixtures_sot$liguetwonow_2_6 + LIGUETWONOW_fixtures_sot$liguetwonow_3_6 +
    LIGUETWONOW_fixtures_sot$liguetwonow_4_6 + LIGUETWONOW_fixtures_sot$liguetwonow_5_6 + LIGUETWONOW_fixtures_sot$liguetwonow_6_6
)
#un25
LIGUETWONOW_fixtures_sot$liguetwonow_un25 <- (
  LIGUETWONOW_fixtures_sot$liguetwonow_0_0 + LIGUETWONOW_fixtures_sot$liguetwonow_1_0 + LIGUETWONOW_fixtures_sot$liguetwonow_0_1 + LIGUETWONOW_fixtures_sot$liguetwonow_1_1 + LIGUETWONOW_fixtures_sot$liguetwonow_2_0 + LIGUETWONOW_fixtures_sot$liguetwonow_0_2
)
#odds
LIGUETWONOW_fixtures_sot$liguetwonow_ov25_odds <- round((1/LIGUETWONOW_fixtures_sot$liguetwonow_ov25),digits = 2)
LIGUETWONOW_fixtures_sot$liguetwonow_un25_odds <- round((1/LIGUETWONOW_fixtures_sot$liguetwonow_un25),digits = 2)

LIGUETWONOW_fixtures_sot$liguetwonow_ov25_odds
LIGUETWONOW_fixtures_sot$liguetwonow_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LIGUETWONOW_fixtures_sot$liguetwonow_ov25 <- percent(LIGUETWONOW_fixtures_sot$liguetwonow_ov25, accuracy = 0.1)

LIGUETWONOW_fixtures_sot$liguetwonow_un25 <- percent(LIGUETWONOW_fixtures_sot$liguetwonow_un25, accuracy = 0.1)
LIGUETWONOW_fixtures_sot$liguetwonow_pssotre <- paste(round(LIGUETWONOW_fixtures_sot$liguetwonow_xHST,digits = 0),round(LIGUETWONOW_fixtures_sot$liguetwonow_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
liguetwonow_home_wins <- c()
liguetwonow_away_wins <- c()
liguetwonow_home_draws <- c()
liguetwonow_away_draws <- c()
liguetwonow_home_loss <- c()
liguetwonow_away_loss <- c()



for (i_liguetwonow_wins in 1:length(liguetwonow_teams))
{

  liguetwonow_home_wins[i_liguetwonow_wins] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_wins] & LIGUETWONOW$FTR == "H",])
  liguetwonow_away_wins[i_liguetwonow_wins] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_wins] & LIGUETWONOW$FTR == "A",])
  liguetwonow_home_draws[i_liguetwonow_wins] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_wins] & LIGUETWONOW$FTR == "D",])
  liguetwonow_away_draws[i_liguetwonow_wins] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_wins] & LIGUETWONOW$FTR == "D",])
  liguetwonow_home_loss[i_liguetwonow_wins] <- nrow(LIGUETWONOW[LIGUETWONOW$HomeTeam == liguetwonow_teams[i_liguetwonow_wins] & LIGUETWONOW$FTR == "A",])
  liguetwonow_away_loss[i_liguetwonow_wins] <- nrow(LIGUETWONOW[LIGUETWONOW$AwayTeam == liguetwonow_teams[i_liguetwonow_wins] & LIGUETWONOW$FTR == "H",])

}

liguetwonow_total_wins <- liguetwonow_home_wins + liguetwonow_away_wins
liguetwonow_total_draws <- liguetwonow_home_draws + liguetwonow_away_draws
liguetwonow_total_loss <- liguetwonow_home_loss + liguetwonow_away_loss

liguetwonow_league_table <- cbind(liguetwonow_teams,liguetwonow_games_played,liguetwonow_total_wins,liguetwonow_total_draws,liguetwonow_total_loss)
liguetwonow_GS <- liguetwonow_scoring$TGS
liguetwonow_GC <-liguetwonow_conceding$TGC
liguetwonow_GD <- liguetwonow_scoring$TGS - liguetwonow_conceding$TGC
liguetwonow_PTS <- (liguetwonow_total_wins*3) + (liguetwonow_total_draws*1)
liguetwonow_league_table <- cbind(liguetwonow_league_table,liguetwonow_GS,liguetwonow_GC,liguetwonow_GD,liguetwonow_PTS)
liguetwonow_league_table <- as.data.frame(liguetwonow_league_table)
#rename the columns
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_teams"] <- "Team"
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_games_played"] <- "P"
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_total_wins"] <- "W"
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_total_draws"] <- "D"
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_total_loss"] <- "L"
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_GS"] <- "F"
names(liguetwonow_league_table)[names(liguetwonow_league_table) == "liguetwonow_GC"] <- "A"
points_liguetwonow <- liguetwonow_league_table[order(as.numeric(liguetwonow_league_table$liguetwonow_PTS), decreasing = TRUE),]
points_liguetwonow$liguetwonow_rank <- 1:length(liguetwonow_teams)
row.names(points_liguetwonow) <- points_liguetwonow$liguetwonow_rank
#create final_liguetwonow_hf_against with team ranks in brackets
for(liguetwonow_rowhrank in 1:nrow(liguetwonow_form_team_against_h)) {
  for(liguetwonow_colhrank in 1:ncol(liguetwonow_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!liguetwonow_form_team_against_h[liguetwonow_rowhrank,liguetwonow_colhrank]=="",liguetwonow_form_team_against_h[liguetwonow_rowhrank,liguetwonow_colhrank] <- paste(liguetwonow_form_team_against_h[liguetwonow_rowhrank,liguetwonow_colhrank],"(",points_liguetwonow$liguetwonow_rank[points_liguetwonow$Team ==liguetwonow_form_team_against_h[liguetwonow_rowhrank,liguetwonow_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
liguetwonow_GP <- nrow(LIGUETWONOW)

#Calculate total home goals for each division
liguetwonow_T_HG <- sum(liguetwonow_home_gs$x)

#calculate average home goal
liguetwonow_avg_HG <- round(liguetwonow_T_HG /liguetwonow_GP, digits = 4)
############################################################
#Calculate total away goals for each division
liguetwonow_T_AG <- sum(liguetwonow_away_gs$x)
#calculate average away goal
liguetwonow_avg_AG <- round(liguetwonow_T_AG /liguetwonow_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
liguetwonow_home_as <- round(((liguetwonow_home_gs$x/liguetwonow_home_games))/liguetwonow_avg_HG, digits = 4)
#calculate away attack strength
liguetwonow_away_as <- round(((liguetwonow_away_gs$x/liguetwonow_away_games))/liguetwonow_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
liguetwonow_avg_HC <- round(liguetwonow_T_AG /liguetwonow_GP, digits = 4)
#avg away concede
liguetwonow_avg_AC <- round(liguetwonow_T_HG /liguetwonow_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
liguetwonow_home_ds <- round(((liguetwonow_home_gc$x/liguetwonow_home_games))/liguetwonow_avg_HC, digits = 4)
#away defense strength
liguetwonow_away_ds <- round(((liguetwonow_away_gc$x/liguetwonow_away_games))/liguetwonow_avg_AC, digits = 4)
#############################################################################
#home poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_home_poisson <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_HG,liguetwonow_home_as,liguetwonow_home_ds)
#################################################################################
#away poisson data
#liguetwonow
liguetwonow_division <- c()
liguetwonow_division[1:length(liguetwonow_teams)] <- "LIGUETWONOW"
liguetwonow_away_poisson <- cbind(liguetwonow_division,liguetwonow_teams,liguetwonow_avg_AG,liguetwonow_away_as,liguetwonow_away_ds)

#LIGUETWONOW
HomeTeam_liguetwonow <- rep(liguetwonow_teams, each = length(liguetwonow_teams))
AwayTeam_liguetwonow <- rep(liguetwonow_teams, length(liguetwonow_teams))
LIGUETWONOW_fixtures <- cbind(HomeTeam_liguetwonow,AwayTeam_liguetwonow)
LIGUETWONOW_fixtures <- as.data.frame(LIGUETWONOW_fixtures)
LIGUETWONOW_fixtures <- LIGUETWONOW_fixtures[!LIGUETWONOW_fixtures$HomeTeam_liguetwonow == LIGUETWONOW_fixtures$AwayTeam_liguetwonow,]
rownames(LIGUETWONOW_fixtures) <- NULL
LIGUETWONOW_fixtures$Div <- "LIGUETWONOW"
LIGUETWONOW_fixtures <- LIGUETWONOW_fixtures[,c(3,1,2)]

LIGUETWONOW_fixtures$avg_HG_liguetwonow <- liguetwonow_avg_HG

LIGUETWONOW_fixtures$liguetwonow_homeas <- rep(liguetwonow_home_as,each = length(liguetwonow_teams)-1)

liguetwonow_awayds_lookup <- cbind(liguetwonow_teams,liguetwonow_away_ds)

liguetwonow_awayds_lookup <- as.data.frame(liguetwonow_awayds_lookup)

colnames(liguetwonow_awayds_lookup) <- c("AwayTeam_liguetwonow","liguetwonow_awayds")


require('RH2')
LIGUETWONOW_fixtures$liguetwonow_awayds <- sqldf("SELECT liguetwonow_awayds_lookup.liguetwonow_awayds FROM liguetwonow_awayds_lookup INNER JOIN LIGUETWONOW_fixtures ON liguetwonow_awayds_lookup.AwayTeam_liguetwonow = LIGUETWONOW_fixtures.AwayTeam_liguetwonow")

LIGUETWONOW_fixtures$avg_AG_liguetwonow <- liguetwonow_avg_AG

liguetwonow_awayas_lookup <- cbind(liguetwonow_teams,liguetwonow_away_as)

liguetwonow_awayas_lookup <- as.data.frame(liguetwonow_awayas_lookup)

colnames(liguetwonow_awayas_lookup) <- c("AwayTeam_liguetwonow","liguetwonow_awayas")


LIGUETWONOW_fixtures$liguetwonow_awayas <- sqldf("SELECT liguetwonow_awayas_lookup.liguetwonow_awayas FROM liguetwonow_awayas_lookup INNER JOIN LIGUETWONOW_fixtures ON liguetwonow_awayas_lookup.AwayTeam_liguetwonow = LIGUETWONOW_fixtures.AwayTeam_liguetwonow")

LIGUETWONOW_fixtures$liguetwonow_homeds <- rep(liguetwonow_home_ds,each = length(liguetwonow_teams)-1)

LIGUETWONOW_fixtures$liguetwonow_awayds <- as.numeric(unlist(LIGUETWONOW_fixtures$liguetwonow_awayds))
#xGH
LIGUETWONOW_fixtures$liguetwonow_xGH <- LIGUETWONOW_fixtures$avg_HG_liguetwonow * LIGUETWONOW_fixtures$liguetwonow_homeas * LIGUETWONOW_fixtures$liguetwonow_awayds

#xGA

LIGUETWONOW_fixtures$liguetwonow_awayas <- as.numeric(unlist(LIGUETWONOW_fixtures$liguetwonow_awayas))

LIGUETWONOW_fixtures$liguetwonow_xGA <- LIGUETWONOW_fixtures$avg_AG_liguetwonow * LIGUETWONOW_fixtures$liguetwonow_awayas * LIGUETWONOW_fixtures$liguetwonow_homeds

LIGUETWONOW_fixtures$liguetwonow_0_0 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_0 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_0_1 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_1 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_0 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_0_2 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_2 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_1 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_2 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_3 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_0 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_1 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_2 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_0_3 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_3 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_3 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_4 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_0 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_1 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_2 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_3 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_0_4 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_4 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_4 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_4 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_5 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_0 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_1 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_2 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_3 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_4 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_0_5 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_5 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_5 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_5 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_5 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_6 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_0 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_1 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_2 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_3 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_4 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_6_5 <- round(stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_0_6 <- round(stats::dpois(0,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_1_6 <- round(stats::dpois(1,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_2_6 <- round(stats::dpois(2,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_3_6 <- round(stats::dpois(3,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_4_6 <- round(stats::dpois(4,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
LIGUETWONOW_fixtures$liguetwonow_5_6 <- round(stats::dpois(5,LIGUETWONOW_fixtures$liguetwonow_xGH) * stats::dpois(6,LIGUETWONOW_fixtures$liguetwonow_xGA), digits = 4)
#Home win
LIGUETWONOW_fixtures$liguetwonow_H <- (
  LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_0 + LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5
)

LIGUETWONOW_fixtures$liguetwonow_H <- percent(LIGUETWONOW_fixtures$liguetwonow_H, accuracy = 0.1)

#Draw
LIGUETWONOW_fixtures$liguetwonow_D <- (

  LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 + LIGUETWONOW_fixtures$liguetwonow_4_4 +
    LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6
)

LIGUETWONOW_fixtures$liguetwonow_D <- percent(LIGUETWONOW_fixtures$liguetwonow_D, accuracy = 0.1)

#Away

LIGUETWONOW_fixtures$liguetwonow_A <- (
  LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 + LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6
)

LIGUETWONOW_fixtures$liguetwonow_A <- percent(LIGUETWONOW_fixtures$liguetwonow_A, accuracy = 0.1)

#ov25
LIGUETWONOW_fixtures$liguetwonow_ov25 <- (
  LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 + LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 +
    LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 + LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_0 +
    LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 + LIGUETWONOW_fixtures$liguetwonow_0_5 +
    LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 + LIGUETWONOW_fixtures$liguetwonow_5_5 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5 + LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 +
    LIGUETWONOW_fixtures$liguetwonow_4_6 + LIGUETWONOW_fixtures$liguetwonow_5_6 + LIGUETWONOW_fixtures$liguetwonow_6_6
)
#un25
LIGUETWONOW_fixtures$liguetwonow_un25 <- (
  LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_0_2
)
#odds
LIGUETWONOW_fixtures$liguetwonow_ov25_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_ov25),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_un25_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_un25),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_ov25_odds
LIGUETWONOW_fixtures$liguetwonow_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
LIGUETWONOW_fixtures$liguetwonow_BTTSY <- (
  LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_3_1 + LIGUETWONOW_fixtures$liguetwonow_3_2 +
    LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_1_3 + LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_3_3 + LIGUETWONOW_fixtures$liguetwonow_4_4 +
    LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_3 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 +
    LIGUETWONOW_fixtures$liguetwonow_3_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_4 + LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_6_6 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6
)
#BTTSN
LIGUETWONOW_fixtures$liguetwonow_BTTSN <- (
  LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_0_2 +
    LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_5_0 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 + LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_0_6
)

LIGUETWONOW_fixtures$liguetwonow_BTTSY_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_BTTSY),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_BTTSN_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_BTTSN),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_BTTSY <- percent(LIGUETWONOW_fixtures$liguetwonow_BTTSY, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_BTTSN <- percent(LIGUETWONOW_fixtures$liguetwonow_BTTSN, accuracy = 0.1)
#odds
LIGUETWONOW_fixtures$liguetwonow_BTTSY_odds
LIGUETWONOW_fixtures$liguetwonow_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
LIGUETWONOW_fixtures$liguetwonow_AH_0_H <- (
  LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_0 +LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5 + LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6
)
#AH_0_A
LIGUETWONOW_fixtures$liguetwonow_AH_0_A <- (
  LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 +LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6 + LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6
)

#odds
LIGUETWONOW_fixtures$liguetwonow_AH_0_H_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_0_H),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_AH_0_A_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_0_A),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_AH_0_H_odds
LIGUETWONOW_fixtures$liguetwonow_AH_0_A_odds
#percentages
LIGUETWONOW_fixtures$liguetwonow_AH_0_H <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_0_H, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_AH_0_A <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
LIGUETWONOW_fixtures$liguetwonow_AH_n075_H <- (
  LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_0 +LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5
)
#AH_n075_A
LIGUETWONOW_fixtures$liguetwonow_AH_n075_A <- (
  LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 +LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6
)

#odds
LIGUETWONOW_fixtures$liguetwonow_AH_n075_H_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_n075_H),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_AH_n075_A_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_n075_A),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_AH_n075_H_odds
LIGUETWONOW_fixtures$liguetwonow_AH_n075_A_odds
#percentages
LIGUETWONOW_fixtures$liguetwonow_AH_n075_H <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_n075_H, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_AH_n075_A <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
LIGUETWONOW_fixtures$liguetwonow_AH_075_H <- (
  LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_0 +LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5 + LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6 + LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_1_2 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_3_4 + LIGUETWONOW_fixtures$liguetwonow_4_5 + LIGUETWONOW_fixtures$liguetwonow_5_6
)
#AH_075_A
LIGUETWONOW_fixtures$liguetwonow_AH_075_A <- (
  LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 +LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6 + LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6 + LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 + LIGUETWONOW_fixtures$liguetwonow_6_5
)

#odds
LIGUETWONOW_fixtures$liguetwonow_AH_075_H_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_075_H),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_AH_075_A_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_075_A),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_AH_075_H_odds
LIGUETWONOW_fixtures$liguetwonow_AH_075_A_odds
#percentages
LIGUETWONOW_fixtures$liguetwonow_AH_075_H <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_075_H, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_AH_075_A <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
LIGUETWONOW_fixtures$liguetwonow_AH_n125_H <- (
  LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_0 +LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5
)
#AH_n125_A
LIGUETWONOW_fixtures$liguetwonow_AH_n125_A <- (
  LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 +LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6
)

#odds
LIGUETWONOW_fixtures$liguetwonow_AH_n125_H_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_n125_H),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_AH_n125_A_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_n125_A),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_AH_n125_H_odds
LIGUETWONOW_fixtures$liguetwonow_AH_n125_A_odds
#percentages
LIGUETWONOW_fixtures$liguetwonow_AH_n125_H <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_n125_H, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_AH_n125_A <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
LIGUETWONOW_fixtures$liguetwonow_AH_125_H <- (
  LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
    LIGUETWONOW_fixtures$liguetwonow_5_0 +LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
    LIGUETWONOW_fixtures$liguetwonow_6_5 + LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6 + LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_1_2 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_3_4 + LIGUETWONOW_fixtures$liguetwonow_4_5 + LIGUETWONOW_fixtures$liguetwonow_5_6
)
#AH_125_A
LIGUETWONOW_fixtures$liguetwonow_AH_125_A <- (
  LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
    LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
    LIGUETWONOW_fixtures$liguetwonow_0_5 +LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
    LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
    LIGUETWONOW_fixtures$liguetwonow_5_6 + LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 +
    LIGUETWONOW_fixtures$liguetwonow_4_4 + LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6 + LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 +
    LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 + LIGUETWONOW_fixtures$liguetwonow_6_5
)

#odds
LIGUETWONOW_fixtures$liguetwonow_AH_125_H_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_125_H),digits = 2)
LIGUETWONOW_fixtures$liguetwonow_AH_125_A_odds <- round((1/LIGUETWONOW_fixtures$liguetwonow_AH_125_A),digits = 2)

LIGUETWONOW_fixtures$liguetwonow_AH_125_H_odds
LIGUETWONOW_fixtures$liguetwonow_AH_125_A_odds
#percentages
LIGUETWONOW_fixtures$liguetwonow_AH_125_H <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_125_H, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_AH_125_A <- percent(LIGUETWONOW_fixtures$liguetwonow_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
LIGUETWONOW_fixtures$liguetwonow_ov25 <- percent(LIGUETWONOW_fixtures$liguetwonow_ov25, accuracy = 0.1)

LIGUETWONOW_fixtures$liguetwonow_un25 <- percent(LIGUETWONOW_fixtures$liguetwonow_un25, accuracy = 0.1)
LIGUETWONOW_fixtures$liguetwonow_pscore <- paste(round(LIGUETWONOW_fixtures$liguetwonow_xGH,digits = 0),round(LIGUETWONOW_fixtures$liguetwonow_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
liguetwonow_last_n_games <- 6

#create final_liguetwonow_hf object
final_liguetwonow_hf <- c()
for(index_liguetwonow_hf in 1:length(liguetwonow_teams))
{
  index_liguetwonow_hf <- row.names(liguetwonow_form_h) == liguetwonow_teams[index_liguetwonow_hf]
  form_liguetwonow_hf <- liguetwonow_form_h[index_liguetwonow_hf]
  deleted_form_liguetwonow_hf <- form_liguetwonow_hf[!form_liguetwonow_hf[] == ""]
  l6_form_liguetwonow_hf <- tail(deleted_form_liguetwonow_hf,liguetwonow_last_n_games)
  l6_form_liguetwonow_hf <- paste(l6_form_liguetwonow_hf,collapse = " ")
  final_liguetwonow_hf[index_liguetwonow_hf] <- rbind(paste(liguetwonow_teams[index_liguetwonow_hf],l6_form_liguetwonow_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}

#change column nam
final_liguetwonow_hf <- as.data.frame(final_liguetwonow_hf)
colnames(final_liguetwonow_hf) <- "Form"
#goals scored
#create final_liguetwonow_gs object
final_liguetwonow_gs <- c()
suml6_liguetwonow_gs <- c()
for(index_liguetwonow_gs in 1:length(liguetwonow_teams))
{
  index_liguetwonow_gs <- row.names(liguetwonow_goalscored_h) == liguetwonow_teams[index_liguetwonow_gs]
  form_liguetwonow_gs <- liguetwonow_goalscored_h[index_liguetwonow_gs]
  deleted_form_liguetwonow_gs <- form_liguetwonow_gs[!form_liguetwonow_gs[] == ""]
  l6_form_liguetwonow_gs <- tail(deleted_form_liguetwonow_gs,liguetwonow_last_n_games)
  l6_form_liguetwonow_gs <- as.numeric(l6_form_liguetwonow_gs)
  suml6_liguetwonow_gs[index_liguetwonow_gs] <- sum(l6_form_liguetwonow_gs)
  suml6_liguetwonow_gs[index_liguetwonow_gs] <- paste("(",suml6_liguetwonow_gs[index_liguetwonow_gs],")",sep = "")
  l6_form_liguetwonow_gs <- paste(l6_form_liguetwonow_gs,collapse = " ")
  final_liguetwonow_gs[index_liguetwonow_gs] <- rbind(paste(liguetwonow_teams[index_liguetwonow_gs],l6_form_liguetwonow_gs,suml6_liguetwonow_gs[index_liguetwonow_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
final_liguetwonow_gs
#change column names
final_liguetwonow_gs <- as.data.frame(final_liguetwonow_gs)
colnames(final_liguetwonow_gs) <- "Goals scored"
#goal conceded
#create final_liguetwonow_gc object
final_liguetwonow_gc <- c()
suml6_liguetwonow_gc <- c()
for(index_liguetwonow_gc in 1:length(liguetwonow_teams))
{
  index_liguetwonow_gc <- row.names(liguetwonow_goalconceded_h) == liguetwonow_teams[index_liguetwonow_gc]
  form_liguetwonow_gc <- liguetwonow_goalconceded_h[index_liguetwonow_gc]
  deleted_form_liguetwonow_gc <- form_liguetwonow_gc[!form_liguetwonow_gc[] == ""]
  l6_form_liguetwonow_gc <- tail(deleted_form_liguetwonow_gc,liguetwonow_last_n_games)
  l6_form_liguetwonow_gc <- as.numeric(l6_form_liguetwonow_gc)
  suml6_liguetwonow_gc[index_liguetwonow_gc] <- sum(l6_form_liguetwonow_gc)
  suml6_liguetwonow_gc[index_liguetwonow_gc] <- paste("(",suml6_liguetwonow_gc[index_liguetwonow_gc],")",sep = "")
  l6_form_liguetwonow_gc <- paste(l6_form_liguetwonow_gc,collapse = " ")
  final_liguetwonow_gc[index_liguetwonow_gc] <- rbind(paste(liguetwonow_teams[index_liguetwonow_gc],l6_form_liguetwonow_gc,suml6_liguetwonow_gc[index_liguetwonow_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
#change column names
final_liguetwonow_gc <- as.data.frame(final_liguetwonow_gc)
colnames(final_liguetwonow_gc) <- "Goals conceded"


toString(l6_form_liguetwonow_gc)
#total goals
#create final_liguetwonow_tg object
final_liguetwonow_tg <- c()
suml6_liguetwonow_tg <- c()
for(index_liguetwonow_tg in 1:length(liguetwonow_teams))
{
  index_liguetwonow_tg <- row.names(liguetwonow_totalgoals_h) == liguetwonow_teams[index_liguetwonow_tg]
  form_liguetwonow_tg <- liguetwonow_totalgoals_h[index_liguetwonow_tg]
  deleted_form_liguetwonow_tg <- form_liguetwonow_tg[!form_liguetwonow_tg[] == ""]
  l6_form_liguetwonow_tg <- tail(deleted_form_liguetwonow_tg,liguetwonow_last_n_games)
  l6_form_liguetwonow_tg <- as.numeric(l6_form_liguetwonow_tg)
  suml6_liguetwonow_tg[index_liguetwonow_tg] <- sum(l6_form_liguetwonow_tg)
  suml6_liguetwonow_tg[index_liguetwonow_tg] <- paste("(",suml6_liguetwonow_tg[index_liguetwonow_tg],")",sep = "")
  l6_form_liguetwonow_tg <- paste(l6_form_liguetwonow_tg,collapse = " ")
  final_liguetwonow_tg[index_liguetwonow_tg] <- rbind(paste(liguetwonow_teams[index_liguetwonow_tg],l6_form_liguetwonow_tg,suml6_liguetwonow_tg[index_liguetwonow_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
#change column names
final_liguetwonow_tg <- as.data.frame(final_liguetwonow_tg)
colnames(final_liguetwonow_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_liguetwonow_hf object
final_liguetwonow_cs <- c()
for(index_liguetwonow_cs in 1:length(liguetwonow_teams))
{
  index_liguetwonow_cs <- row.names(liguetwonow_csform_h) == liguetwonow_teams[index_liguetwonow_cs]
  csform_liguetwonow_cs <- liguetwonow_csform_h[index_liguetwonow_cs]
  deleted_csform_liguetwonow_cs <- csform_liguetwonow_cs[!csform_liguetwonow_cs[] == ""]
  l6_csform_liguetwonow_cs <- tail(deleted_csform_liguetwonow_cs,liguetwonow_last_n_games)
  l6_csform_liguetwonow_cs <- paste(l6_csform_liguetwonow_cs,collapse = " ")
  final_liguetwonow_cs[index_liguetwonow_cs] <- rbind(paste(liguetwonow_teams[index_liguetwonow_cs],l6_csform_liguetwonow_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_csform)

}

#change column names
final_liguetwonow_cs <- as.data.frame(final_liguetwonow_cs)
colnames(final_liguetwonow_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_liguetwonow_wm object
final_liguetwonow_wm <- c()
suml6_liguetwonow_wm <- c()
for(index_liguetwonow_wm in 1:length(liguetwonow_teams))
{
  index_liguetwonow_wm <- row.names(liguetwonow_winmargin_h) == liguetwonow_teams[index_liguetwonow_wm]
  form_liguetwonow_wm <- liguetwonow_winmargin_h[index_liguetwonow_wm]
  deleted_form_liguetwonow_wm <- form_liguetwonow_wm[!form_liguetwonow_wm[] == ""]
  l6_form_liguetwonow_wm <- tail(deleted_form_liguetwonow_wm,liguetwonow_last_n_games)
  l6_form_liguetwonow_wm <- as.numeric(l6_form_liguetwonow_wm)
  suml6_liguetwonow_wm[index_liguetwonow_wm] <- sum(l6_form_liguetwonow_wm)
  suml6_liguetwonow_wm[index_liguetwonow_wm] <- paste("(",suml6_liguetwonow_wm[index_liguetwonow_wm],")",sep = "")
  l6_form_liguetwonow_wm <- paste(l6_form_liguetwonow_wm,collapse = " ")
  final_liguetwonow_wm[index_liguetwonow_wm] <- rbind(paste(liguetwonow_teams[index_liguetwonow_wm],l6_form_liguetwonow_wm,suml6_liguetwonow_wm[index_liguetwonow_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
final_liguetwonow_wm
#change column names
final_liguetwonow_wm <- as.data.frame(final_liguetwonow_wm)
colnames(final_liguetwonow_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_liguetwonow_ca object
final_liguetwonow_ca <- c()
suml6_liguetwonow_ca <- c()
for(index_liguetwonow_ca in 1:length(liguetwonow_teams))
{
  index_liguetwonow_ca <- row.names(liguetwonow_coawarded_h) == liguetwonow_teams[index_liguetwonow_ca]
  form_liguetwonow_ca <- liguetwonow_coawarded_h[index_liguetwonow_ca]
  deleted_form_liguetwonow_ca <- form_liguetwonow_ca[!form_liguetwonow_ca[] == ""]
  l6_form_liguetwonow_ca <- tail(deleted_form_liguetwonow_ca,liguetwonow_last_n_games)
  l6_form_liguetwonow_ca <- as.numeric(l6_form_liguetwonow_ca)
  suml6_liguetwonow_ca[index_liguetwonow_ca] <- sum(l6_form_liguetwonow_ca)
  suml6_liguetwonow_ca[index_liguetwonow_ca] <- paste("(",suml6_liguetwonow_ca[index_liguetwonow_ca],")",sep = "")
  l6_form_liguetwonow_ca <- paste(l6_form_liguetwonow_ca,collapse = " ")
  final_liguetwonow_ca[index_liguetwonow_ca] <- rbind(paste(liguetwonow_teams[index_liguetwonow_ca],l6_form_liguetwonow_ca,suml6_liguetwonow_ca[index_liguetwonow_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
final_liguetwonow_ca
#change column names
final_liguetwonow_ca <- as.data.frame(final_liguetwonow_ca)
colnames(final_liguetwonow_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_liguetwonow_ca object
final_liguetwonow_cc <- c()
suml6_liguetwonow_cc <- c()
for(index_liguetwonow_cc in 1:length(liguetwonow_teams))
{
  index_liguetwonow_cc <- row.names(liguetwonow_cornersconceded_h) == liguetwonow_teams[index_liguetwonow_cc]
  form_liguetwonow_cc <- liguetwonow_cornersconceded_h[index_liguetwonow_cc]
  deleted_form_liguetwonow_cc <- form_liguetwonow_cc[!form_liguetwonow_cc[] == ""]
  l6_form_liguetwonow_cc <- tail(deleted_form_liguetwonow_cc,liguetwonow_last_n_games)
  l6_form_liguetwonow_cc <- as.numeric(l6_form_liguetwonow_cc)
  suml6_liguetwonow_cc[index_liguetwonow_cc] <- sum(l6_form_liguetwonow_cc)
  suml6_liguetwonow_cc[index_liguetwonow_cc] <- paste("(",suml6_liguetwonow_cc[index_liguetwonow_cc],")",sep = "")
  l6_form_liguetwonow_cc <- paste(l6_form_liguetwonow_cc,collapse = " ")
  final_liguetwonow_cc[index_liguetwonow_cc] <- rbind(paste(liguetwonow_teams[index_liguetwonow_cc],l6_form_liguetwonow_cc,suml6_liguetwonow_cc[index_liguetwonow_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
final_liguetwonow_cc
#change column names
final_liguetwonow_cc <- as.data.frame(final_liguetwonow_cc)
colnames(final_liguetwonow_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_liguetwonow_cosc <- c()
for(index_liguetwonow_cosc in 1:length(liguetwonow_teams))
{
  index_liguetwonow_cosc <- row.names(liguetwonow_coscform_h) == liguetwonow_teams[index_liguetwonow_cosc]
  coscform_liguetwonow_cosc <- liguetwonow_coscform_h[index_liguetwonow_cosc]
  deleted_coscform_liguetwonow_cosc <- coscform_liguetwonow_cosc[!coscform_liguetwonow_cosc[] == ""]
  l6_coscform_liguetwonow_cosc <- tail(deleted_coscform_liguetwonow_cosc,liguetwonow_last_n_games)
  l6_coscform_liguetwonow_cosc <- paste(l6_coscform_liguetwonow_cosc,collapse = " ")
  final_liguetwonow_cosc[index_liguetwonow_cosc] <- rbind(paste(liguetwonow_teams[index_liguetwonow_cosc],l6_coscform_liguetwonow_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_coscform)

}
final_liguetwonow_cosc
#change column names
final_liguetwonow_cosc <- as.data.frame(final_liguetwonow_cosc)
colnames(final_liguetwonow_cosc) <- "CornersForm"
##################################################
#total corners
#create final_liguetwonow_tcorners object
final_liguetwonow_tcorners <- c()
suml6_liguetwonow_tcorners <- c()
for(index_liguetwonow_tcorners in 1:length(liguetwonow_teams))
{
  index_liguetwonow_tcorners <- row.names(liguetwonow_totalcorners_h) == liguetwonow_teams[index_liguetwonow_tcorners]
  form_liguetwonow_tcorners <- liguetwonow_totalcorners_h[index_liguetwonow_tcorners]
  deleted_form_liguetwonow_tcorners <- form_liguetwonow_tcorners[!form_liguetwonow_tcorners[] == ""]
  l6_form_liguetwonow_tcorners <- tail(deleted_form_liguetwonow_tcorners,liguetwonow_last_n_games)
  l6_form_liguetwonow_tcorners <- as.numeric(l6_form_liguetwonow_tcorners)
  suml6_liguetwonow_tcorners[index_liguetwonow_tcorners] <- sum(l6_form_liguetwonow_tcorners)
  suml6_liguetwonow_tcorners[index_liguetwonow_tcorners] <- paste("(",suml6_liguetwonow_tcorners[index_liguetwonow_tcorners],")",sep = "")
  l6_form_liguetwonow_tcorners <- paste(l6_form_liguetwonow_tcorners,collapse = " ")
  final_liguetwonow_tcorners[index_liguetwonow_tcorners] <- rbind(paste(liguetwonow_teams[index_liguetwonow_tcorners],l6_form_liguetwonow_tcorners,suml6_liguetwonow_tcorners[index_liguetwonow_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
#change column names
final_liguetwonow_tcorners <- as.data.frame(final_liguetwonow_tcorners)
colnames(final_liguetwonow_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_liguetwonow_hf_against
final_liguetwonow_hf_against <- c()
for(index_liguetwonow_hf_against in 1:length(liguetwonow_teams))
{
  index_liguetwonow_hf_against <- row.names(liguetwonow_form_team_against_h) == liguetwonow_teams[index_liguetwonow_hf_against]
  form_liguetwonow_hf_against <- liguetwonow_form_team_against_h[index_liguetwonow_hf_against]
  deleted_form_liguetwonow_hf_against <- form_liguetwonow_hf_against[!form_liguetwonow_hf_against[] == ""]
  l6_form_liguetwonow_hf_against <- tail(deleted_form_liguetwonow_hf_against,liguetwonow_last_n_games)
  l6_form_liguetwonow_hf_against <- paste(l6_form_liguetwonow_hf_against,collapse = " ")
  final_liguetwonow_hf_against[index_liguetwonow_hf_against] <- rbind(paste(liguetwonow_teams[index_liguetwonow_hf_against],l6_form_liguetwonow_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",liguetwonow_teams[index],l6_form)

}
final_liguetwonow_hf_against <- as.data.frame(final_liguetwonow_hf_against)
colnames(final_liguetwonow_hf_against) <- "Team against"
#combine the columns
final_liguetwonow_all <- cbind(final_liguetwonow_hf,final_liguetwonow_gs,final_liguetwonow_gc,final_liguetwonow_tg,final_liguetwonow_ca,final_liguetwonow_cc,final_liguetwonow_tcorners,final_liguetwonow_cosc,final_liguetwonow_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#LIGUETWONOW
LIGUETWONOW_sim <- LIGUETWONOW
LIGUETWONOW_sim$matchid <- paste(LIGUETWONOW_sim$HomeTeam,LIGUETWONOW_sim$AwayTeam,sep = "-")
LIGUETWONOW_fixtures$matchid <- paste(LIGUETWONOW_fixtures$HomeTeam_liguetwonow,LIGUETWONOW_fixtures$AwayTeam_liguetwonow,sep = "-")
LIGUETWONOW_fixtures$liguetwonow_FTR <- sapply(LIGUETWONOW_fixtures$liguetwonow_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

LIGUETWONOW_fixtures$liguetwonow_gamestatus <- ifelse(LIGUETWONOW_fixtures$matchid %in% LIGUETWONOW_sim$matchid,"played","notplayed")

liguetwonow_home_wins_sim <- c()
liguetwonow_away_wins_sim <- c()
liguetwonow_home_draws_sim <- c()
liguetwonow_away_draws_sim <- c()
liguetwonow_home_loss_sim <- c()
liguetwonow_away_loss_sim <- c()



for (i_liguetwonow_wins_sim in 1:length(liguetwonow_teams))
{

  liguetwonow_home_wins_sim[i_liguetwonow_wins_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$HomeTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_wins_sim] & LIGUETWONOW_fixtures$liguetwonow_FTR == "H" & LIGUETWONOW_fixtures$liguetwonow_gamestatus =="notplayed",])
  liguetwonow_away_wins_sim[i_liguetwonow_wins_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$AwayTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_wins_sim] & LIGUETWONOW_fixtures$liguetwonow_FTR == "A" & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",])
  liguetwonow_home_draws_sim[i_liguetwonow_wins_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$HomeTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_wins_sim] & LIGUETWONOW_fixtures$liguetwonow_FTR == "D" & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",])
  liguetwonow_away_draws_sim[i_liguetwonow_wins_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$AwayTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_wins_sim] & LIGUETWONOW_fixtures$liguetwonow_FTR == "D" & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",])
  liguetwonow_home_loss_sim[i_liguetwonow_wins_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$HomeTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_wins_sim] & LIGUETWONOW_fixtures$liguetwonow_FTR == "A" & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",])
  liguetwonow_away_loss_sim[i_liguetwonow_wins_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$AwayTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_wins_sim] & LIGUETWONOW_fixtures$liguetwonow_FTR == "H" & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed", ])

}

liguetwonow_total_wins_sim <- liguetwonow_home_wins_sim + liguetwonow_away_wins_sim
liguetwonow_total_draws_sim <- liguetwonow_home_draws_sim + liguetwonow_away_draws_sim
liguetwonow_total_loss_sim <- liguetwonow_home_loss_sim + liguetwonow_away_loss_sim

liguetwonow_home_games_sim <- c()
liguetwonow_away_games_sim <-c()

for (i_liguetwonow_sim in 1:length(liguetwonow_teams))
{

  liguetwonow_home_games_sim[i_liguetwonow_sim] <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$HomeTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_sim] & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",])
  liguetwonow_away_games_sim[i_liguetwonow_sim]  <- nrow(LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$AwayTeam_liguetwonow == liguetwonow_teams[i_liguetwonow_sim] & LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",])

}

liguetwonow_games_played_sim <- liguetwonow_home_games_sim + liguetwonow_away_games_sim

liguetwonow_league_table_sim <- cbind(liguetwonow_teams,liguetwonow_games_played_sim,liguetwonow_total_wins_sim,liguetwonow_total_draws_sim,liguetwonow_total_loss_sim)
liguetwonow_PTS_sim <- (liguetwonow_total_wins_sim*3) + (liguetwonow_total_draws_sim*1)
liguetwonow_league_table_sim <- cbind(liguetwonow_league_table_sim,liguetwonow_PTS_sim)

liguetwonow_games_played_simfinal <- liguetwonow_games_played + liguetwonow_games_played_sim
liguetwonow_total_wins_simfinal <- liguetwonow_total_wins + liguetwonow_total_wins_sim
liguetwonow_total_draws_simfinal <- liguetwonow_total_draws + liguetwonow_total_draws_sim
liguetwonow_total_loss_simfinal <- liguetwonow_total_loss + liguetwonow_total_loss_sim
liguetwonow_PTS_simfinal <- liguetwonow_PTS + liguetwonow_PTS_sim

liguetwonow_league_table_simfinal <- cbind(liguetwonow_teams,liguetwonow_games_played_simfinal,liguetwonow_total_wins_simfinal,liguetwonow_total_draws_simfinal,liguetwonow_total_loss_simfinal,liguetwonow_PTS_simfinal)
liguetwonow_league_table_simfinal <- as.data.frame(liguetwonow_league_table_simfinal)
names(liguetwonow_league_table_simfinal)[names(liguetwonow_league_table_simfinal) == "liguetwonow_teams"] <- "Team_f"
names(liguetwonow_league_table_simfinal)[names(liguetwonow_league_table_simfinal) == "liguetwonow_games_played_simfinal"] <- "P_f"
names(liguetwonow_league_table_simfinal)[names(liguetwonow_league_table_simfinal) == "liguetwonow_total_wins_simfinal"] <- "W_f"
names(liguetwonow_league_table_simfinal)[names(liguetwonow_league_table_simfinal) == "liguetwonow_total_draws_simfinal"] <- "D_f"
names(liguetwonow_league_table_simfinal)[names(liguetwonow_league_table_simfinal) == "liguetwonow_total_loss_simfinal"] <- "L_f"
names(liguetwonow_league_table_simfinal)[names(liguetwonow_league_table_simfinal) == "liguetwonow_PTS_simfinal"] <- "PTS_f"
points_liguetwonow_sim <-  liguetwonow_league_table_simfinal[order(as.numeric(liguetwonow_league_table_simfinal$PTS_f), decreasing = TRUE),]

LIGUETWONOW_notplayed <- LIGUETWONOW_fixtures[LIGUETWONOW_fixtures$liguetwonow_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#LIGUETWONOW
LIGUETWONOW_fixtures$Hometeam_liguetwonow_index <- match(LIGUETWONOW_fixtures$HomeTeam_liguetwonow,liguetwonow_teams)
LIGUETWONOW_fixtures$Awayteam_liguetwonow_index <- match(LIGUETWONOW_fixtures$AwayTeam_liguetwonow,liguetwonow_teams)
liguetwonow_prediction <- c()
liguetwonow_HWM <- c()
liguetwonow_AWM <- c()
liguetwonow_HWMLM <- c()
liguetwonow_AWMLM <- c()
liguetwonow_HY <- c()
liguetwonow_AY <- c()
liguetwonow_HCO <- c()
liguetwonow_ACO <- c()
liguetwonow_HXSC <- c()
liguetwonow_AXSC <- c()
liguetwonow_HYCPF <- c()
liguetwonow_AYCPF <- c()
for(liguetwonow_row in 1:nrow(LIGUETWONOW_fixtures))
{

  liguetwonow_hometeamindex <- LIGUETWONOW_fixtures[liguetwonow_row,"Hometeam_liguetwonow_index"]
  liguetwonow_awayteamindex <- LIGUETWONOW_fixtures[liguetwonow_row,"Awayteam_liguetwonow_index"]
  #analyse team form
  #home team
  liguetwonow_form_vec_ht <- as.vector(liguetwonow_form_h[liguetwonow_hometeamindex,])
  liguetwonow_form_vec_ht[is.na(liguetwonow_form_vec_ht)] <- ""
  liguetwonow_form_vec_ht <- liguetwonow_form_vec_ht[liguetwonow_form_vec_ht != ""]
  liguetwonow_form_vec_ht  <-tail(liguetwonow_form_vec_ht,6)
  liguetwonow_ht_numberof_wins <- length(which(liguetwonow_form_vec_ht == "W"))
  liguetwonow_ht_numberof_draws <- length(which(liguetwonow_form_vec_ht == "D"))
  liguetwonow_ht_numberof_loss <- length(which(liguetwonow_form_vec_ht == "L"))
  #awayteam
  liguetwonow_form_vec_at <- as.vector(liguetwonow_form_h[liguetwonow_awayteamindex,])
  liguetwonow_form_vec_at[is.na(liguetwonow_form_vec_at)] <- ""
  liguetwonow_form_vec_at <- liguetwonow_form_vec_at[liguetwonow_form_vec_at != ""]
  liguetwonow_form_vec_at  <-tail(liguetwonow_form_vec_at,6)
  liguetwonow_at_numberof_wins <- length(which(liguetwonow_form_vec_at == "W"))
  liguetwonow_at_numberof_draws <- length(which(liguetwonow_form_vec_at == "D"))
  liguetwonow_at_numberof_loss <- length(which(liguetwonow_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  liguetwonow_goalscored_vec_ht <- as.vector(liguetwonow_goalscored_h[liguetwonow_hometeamindex,])
  liguetwonow_goalscored_vec_ht[is.na(liguetwonow_goalscored_vec_ht)] <- ""
  liguetwonow_goalscored_vec_ht <- liguetwonow_goalscored_vec_ht[liguetwonow_goalscored_vec_ht != ""]
  liguetwonow_goalscored_vec_ht  <-tail(liguetwonow_goalscored_vec_ht,6)
  liguetwonow_goalscored_vec_ht  <- as.numeric(liguetwonow_goalscored_vec_ht)
  liguetwonow_ht_totalgoalscored <- sum(liguetwonow_goalscored_vec_ht)
  liguetwonow_ht_matches_scoring <- length(which(liguetwonow_goalscored_vec_ht > 0))
  liguetwonow_ht_matches_without_scoring <- length(which(liguetwonow_goalscored_vec_ht == "0"))
  #awayteam
  liguetwonow_goalscored_vec_at <- as.vector(liguetwonow_goalscored_h[liguetwonow_awayteamindex,])
  liguetwonow_goalscored_vec_at[is.na(liguetwonow_goalscored_vec_at)] <- ""
  liguetwonow_goalscored_vec_at <- liguetwonow_goalscored_vec_at[liguetwonow_goalscored_vec_at != ""]
  liguetwonow_goalscored_vec_at  <-tail(liguetwonow_goalscored_vec_at,6)
  liguetwonow_goalscored_vec_at  <- as.numeric(liguetwonow_goalscored_vec_at)
  liguetwonow_at_totalgoalscored <- sum(liguetwonow_goalscored_vec_at)
  liguetwonow_at_matches_scoring <- length(which(liguetwonow_goalscored_vec_at > 0))
  liguetwonow_at_matches_without_scoring <- length(which(liguetwonow_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  liguetwonow_goalconceded_vec_ht <- as.vector(liguetwonow_goalconceded_h[liguetwonow_hometeamindex,])
  liguetwonow_goalconceded_vec_ht[is.na(liguetwonow_goalconceded_vec_ht)] <- ""
  liguetwonow_goalconceded_vec_ht <- liguetwonow_goalconceded_vec_ht[liguetwonow_goalconceded_vec_ht != ""]
  liguetwonow_goalconceded_vec_ht  <-tail(liguetwonow_goalconceded_vec_ht,6)
  liguetwonow_goalconceded_vec_ht  <- as.numeric(liguetwonow_goalconceded_vec_ht)
  liguetwonow_goalconceded_vec_ht
  liguetwonow_ht_totalgoalconceded <- sum(liguetwonow_goalconceded_vec_ht)
  liguetwonow_ht_matches_concede <- length(which(liguetwonow_goalconceded_vec_ht > 0))
  liguetwonow_ht_matches_without_concede <- length(which(liguetwonow_goalconceded_vec_ht == "0"))
  #awayteam
  liguetwonow_goalconceded_vec_at <- as.vector(liguetwonow_goalconceded_h[liguetwonow_awayteamindex,])
  liguetwonow_goalconceded_vec_at[is.na(liguetwonow_goalconceded_vec_at)] <- ""
  liguetwonow_goalconceded_vec_at <- liguetwonow_goalconceded_vec_at[liguetwonow_goalconceded_vec_at != ""]
  liguetwonow_goalconceded_vec_at  <-tail(liguetwonow_goalconceded_vec_at,6)
  liguetwonow_goalconceded_vec_at  <- as.numeric(liguetwonow_goalconceded_vec_at)
  liguetwonow_at_totalgoalconceded <- sum(liguetwonow_goalconceded_vec_at)
  liguetwonow_at_matches_concede <- length(which(liguetwonow_goalconceded_vec_at > 0))
  liguetwonow_at_matches_without_concede <- length(which(liguetwonow_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  liguetwonow_totalgoals_vec_ht <- as.vector(liguetwonow_totalgoals_h[liguetwonow_hometeamindex,])
  liguetwonow_totalgoals_vec_ht[is.na(liguetwonow_totalgoals_vec_ht)] <- ""
  liguetwonow_totalgoals_vec_ht <- liguetwonow_totalgoals_vec_ht[liguetwonow_totalgoals_vec_ht != ""]
  liguetwonow_totalgoals_vec_ht  <-tail(liguetwonow_totalgoals_vec_ht,6)
  liguetwonow_totalgoals_vec_ht  <- as.numeric(liguetwonow_totalgoals_vec_ht)
  liguetwonow_totalgoals_vec_ht
  liguetwonow_ht_totalgoals <- sum(liguetwonow_totalgoals_vec_ht)
  liguetwonow_ht_avgtotalgoals <- (liguetwonow_ht_totalgoals/6)
  liguetwonow_ht_no_of_ov25 <- length(which(liguetwonow_totalgoals_vec_ht >= 3))
  liguetwonow_ht_no_of_un25 <- length(which(liguetwonow_totalgoals_vec_ht <= 2))
  #awayteam
  liguetwonow_totalgoals_vec_at <- as.vector(liguetwonow_totalgoals_h[liguetwonow_awayteamindex,])
  liguetwonow_totalgoals_vec_at[is.na(liguetwonow_totalgoals_vec_at)] <- ""
  liguetwonow_totalgoals_vec_at <- liguetwonow_totalgoals_vec_at[liguetwonow_totalgoals_vec_at != ""]
  liguetwonow_totalgoals_vec_at  <-tail(liguetwonow_totalgoals_vec_at,6)
  liguetwonow_totalgoals_vec_at  <- as.numeric(liguetwonow_totalgoals_vec_at)
  liguetwonow_totalgoals_vec_at
  liguetwonow_at_totalgoals <- sum(liguetwonow_totalgoals_vec_at)
  liguetwonow_at_avgtotalgoals <- (liguetwonow_at_totalgoals/6)
  liguetwonow_at_no_of_ov25 <- length(which(liguetwonow_totalgoals_vec_at >= 3))
  liguetwonow_at_no_of_un25 <- length(which(liguetwonow_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  liguetwonow_winmargin_vec_ht <- as.vector(liguetwonow_winmargin_h[liguetwonow_hometeamindex,])
  liguetwonow_winmargin_vec_ht[is.na(liguetwonow_winmargin_vec_ht)] <- ""
  liguetwonow_winmargin_vec_ht <- liguetwonow_winmargin_vec_ht[liguetwonow_winmargin_vec_ht != ""]
  liguetwonow_winmargin_vec_ht  <-tail(liguetwonow_winmargin_vec_ht,6)
  liguetwonow_winmargin_vec_ht  <- as.numeric(liguetwonow_winmargin_vec_ht)

  liguetwonow_ht_totalwinmargin <- sum(liguetwonow_winmargin_vec_ht)
  liguetwonow_ht_no_of_winmargin_ov0 <- length(which(liguetwonow_winmargin_vec_ht >= 0))
  liguetwonow_ht_no_of_winmargin_ov1 <- length(which(liguetwonow_winmargin_vec_ht >= 1))
  liguetwonow_ht_no_of_winmargin_un0 <- length(which(liguetwonow_winmargin_vec_ht <= 0))
  liguetwonow_ht_no_of_winmargin_un1 <- length(which(liguetwonow_winmargin_vec_ht <= 1))
  #awayteam
  liguetwonow_winmargin_vec_at <- as.vector(liguetwonow_winmargin_h[liguetwonow_awayteamindex,])
  liguetwonow_winmargin_vec_at[is.na(liguetwonow_winmargin_vec_at)] <- ""
  liguetwonow_winmargin_vec_at <- liguetwonow_winmargin_vec_at[liguetwonow_winmargin_vec_at != ""]
  liguetwonow_winmargin_vec_at  <-tail(liguetwonow_winmargin_vec_at,6)
  liguetwonow_winmargin_vec_at  <- as.numeric(liguetwonow_winmargin_vec_at)

  liguetwonow_at_totalwinmargin <- sum(liguetwonow_winmargin_vec_at)
  liguetwonow_at_no_of_winmargin_ov0 <- length(which(liguetwonow_winmargin_vec_at >= 0))
  liguetwonow_at_no_of_winmargin_ov1 <- length(which(liguetwonow_winmargin_vec_at >= 1))
  liguetwonow_at_no_of_winmargin_un0 <- length(which(liguetwonow_winmargin_vec_at <= 0))
  liguetwonow_at_no_of_winmargin_un1 <- length(which(liguetwonow_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  liguetwonow_winmargin_vec_ht_lm <- as.vector(liguetwonow_winmargin_h[liguetwonow_hometeamindex,])
  liguetwonow_winmargin_vec_ht_lm[is.na(liguetwonow_winmargin_vec_ht_lm)] <- ""
  liguetwonow_winmargin_vec_ht_lm <- liguetwonow_winmargin_vec_ht_lm[liguetwonow_winmargin_vec_ht_lm != ""]
  liguetwonow_winmargin_vec_ht_lm  <-tail(liguetwonow_winmargin_vec_ht_lm,1)
  #awayteam
  liguetwonow_winmargin_vec_at_lm <- as.vector(liguetwonow_winmargin_h[liguetwonow_awayteamindex,])
  liguetwonow_winmargin_vec_at_lm[is.na(liguetwonow_winmargin_vec_at_lm)] <- ""
  liguetwonow_winmargin_vec_at_lm <- liguetwonow_winmargin_vec_at_lm[liguetwonow_winmargin_vec_at_lm != ""]
  liguetwonow_winmargin_vec_at_lm  <-tail(liguetwonow_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  liguetwonow_yellowtotals_vec_ht <- as.vector(liguetwonow_yellowtotalsv2[liguetwonow_hometeamindex,])
  liguetwonow_yellowtotals_vec_ht[is.na(liguetwonow_yellowtotals_vec_ht)] <- ""
  liguetwonow_yellowtotals_vec_ht <- liguetwonow_yellowtotals_vec_ht[liguetwonow_yellowtotals_vec_ht != ""]
  liguetwonow_yellowtotals_vec_ht  <-tail(liguetwonow_yellowtotals_vec_ht,1)
  #awayteam
  liguetwonow_yellowtotals_vec_at <- as.vector(liguetwonow_yellowtotalsv2[liguetwonow_awayteamindex,])
  liguetwonow_yellowtotals_vec_at[is.na(liguetwonow_yellowtotals_vec_at)] <- ""
  liguetwonow_yellowtotals_vec_at <- liguetwonow_yellowtotals_vec_at[liguetwonow_yellowtotals_vec_at != ""]
  liguetwonow_yellowtotals_vec_at  <-tail(liguetwonow_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  liguetwonow_cornertotals_vec_ht <- as.vector(liguetwonow_cornertotalsv2[liguetwonow_hometeamindex,])
  liguetwonow_cornertotals_vec_ht[is.na(liguetwonow_cornertotals_vec_ht)] <- ""
  liguetwonow_cornertotals_vec_ht <- liguetwonow_cornertotals_vec_ht[liguetwonow_cornertotals_vec_ht != ""]
  liguetwonow_cornertotals_vec_ht  <-tail(liguetwonow_cornertotals_vec_ht,1)
  #awayteam
  liguetwonow_cornertotals_vec_at <- as.vector(liguetwonow_cornertotalsv2[liguetwonow_awayteamindex,])
  liguetwonow_cornertotals_vec_at[is.na(liguetwonow_cornertotals_vec_at)] <- ""
  liguetwonow_cornertotals_vec_at <- liguetwonow_cornertotals_vec_at[liguetwonow_cornertotals_vec_at != ""]
  liguetwonow_cornertotals_vec_at  <-tail(liguetwonow_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  liguetwonow_xshotsconversion_vec_ht <- as.vector(liguetwonow_shots_analysis[liguetwonow_hometeamindex,])
  liguetwonow_xshotsconversion_vec_ht[is.na(liguetwonow_xshotsconversion_vec_ht)] <- ""
  liguetwonow_xshotsconversion_vec_ht <- liguetwonow_xshotsconversion_vec_ht[liguetwonow_xshotsconversion_vec_ht != ""]
  liguetwonow_xshotsconversion_vec_ht  <-tail(liguetwonow_xshotsconversion_vec_ht,1)
  #awayteam
  liguetwonow_xshotsconversion_vec_at <- as.vector(liguetwonow_shots_analysis[liguetwonow_awayteamindex,])
  liguetwonow_xshotsconversion_vec_at[is.na(liguetwonow_xshotsconversion_vec_at)] <- ""
  liguetwonow_xshotsconversion_vec_at <- liguetwonow_xshotsconversion_vec_at[liguetwonow_xshotsconversion_vec_at != ""]
  liguetwonow_xshotsconversion_vec_at  <-tail(liguetwonow_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  liguetwonow_fouls_conversion_vec_ht <- as.vector(liguetwonow_fouls_conversion[liguetwonow_hometeamindex,])
  liguetwonow_fouls_conversion_vec_ht[is.na(liguetwonow_fouls_conversion_vec_ht)] <- ""
  liguetwonow_fouls_conversion_vec_ht <- liguetwonow_fouls_conversion_vec_ht[liguetwonow_fouls_conversion_vec_ht != ""]
  liguetwonow_fouls_conversion_vec_ht  <-tail(liguetwonow_fouls_conversion_vec_ht,1)
  #awayteam
  liguetwonow_fouls_conversion_vec_at <- as.vector(liguetwonow_fouls_conversion[liguetwonow_awayteamindex,])
  liguetwonow_fouls_conversion_vec_at[is.na(liguetwonow_fouls_conversion_vec_at)] <- ""
  liguetwonow_fouls_conversion_vec_at <- liguetwonow_fouls_conversion_vec_at[liguetwonow_fouls_conversion_vec_at != ""]
  liguetwonow_fouls_conversion_vec_at  <-tail(liguetwonow_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  liguetwonow_ht_last6points <- liguetwonow_ht_numberof_wins*3 + liguetwonow_ht_numberof_draws*1
  liguetwonow_at_last6points <- liguetwonow_at_numberof_wins*3 + liguetwonow_at_numberof_draws*1

  if(liguetwonow_ht_last6points > liguetwonow_at_last6points) {liguetwonow_3waypick <- "1"}  else {liguetwonow_3waypick <- "X2"}

  if(liguetwonow_at_last6points > liguetwonow_ht_last6points ) {liguetwonow_3waypick <- "2"} else {liguetwonow_3waypick <- "1X"}

  if(liguetwonow_ht_no_of_ov25 + liguetwonow_at_no_of_ov25 >= 6) {liguetwonow_goalspick <- "ov25"} else {liguetwonow_goalspick <- "un25"}

  if(liguetwonow_ht_no_of_un25 + liguetwonow_at_no_of_un25 >= 6) {liguetwonow_goalspick <- "un25"} else {liguetwonow_goalspick <- "ov25"}

  if(liguetwonow_ht_matches_scoring >= 4 && liguetwonow_at_matches_scoring >=4) {liguetwonow_btts <- "BTTS-Y"} else {liguetwonow_btts <- "BTTS-N"}


  liguetwonow_prediction[liguetwonow_row] <- rbind(paste(liguetwonow_3waypick,liguetwonow_goalspick,liguetwonow_btts,sep = ","))
  liguetwonow_HWM[liguetwonow_row] <- liguetwonow_ht_totalwinmargin
  liguetwonow_AWM[liguetwonow_row] <- liguetwonow_at_totalwinmargin

  liguetwonow_HWMLM[liguetwonow_row] <- liguetwonow_winmargin_vec_ht_lm
  liguetwonow_AWMLM[liguetwonow_row] <- liguetwonow_winmargin_vec_at_lm

  liguetwonow_HY[liguetwonow_row] <- liguetwonow_yellowtotals_vec_ht
  liguetwonow_AY[liguetwonow_row] <- liguetwonow_yellowtotals_vec_at

  liguetwonow_HCO[liguetwonow_row] <- liguetwonow_cornertotals_vec_ht
  liguetwonow_ACO[liguetwonow_row] <- liguetwonow_cornertotals_vec_at

  liguetwonow_HXSC[liguetwonow_row] <- liguetwonow_xshotsconversion_vec_ht
  liguetwonow_AXSC[liguetwonow_row] <- liguetwonow_xshotsconversion_vec_at

  liguetwonow_HYCPF[liguetwonow_row] <- liguetwonow_fouls_conversion_vec_ht
  liguetwonow_AYCPF[liguetwonow_row] <- liguetwonow_fouls_conversion_vec_at
}

liguetwonow_prediction <- as.data.frame(liguetwonow_prediction)
colnames(liguetwonow_prediction) <- "prediction"

liguetwonow_HWM <- as.data.frame(liguetwonow_HWM)
colnames(liguetwonow_HWM) <- "HWM"

liguetwonow_AWM <- as.data.frame(liguetwonow_AWM)
colnames(liguetwonow_AWM) <- "AWM"

liguetwonow_HWMLM <- as.data.frame(liguetwonow_HWMLM)
colnames(liguetwonow_HWMLM) <- "HWMLM"

liguetwonow_AWMLM <- as.data.frame(liguetwonow_AWMLM)
colnames(liguetwonow_AWMLM) <- "AWMLM"

liguetwonow_HY <- as.data.frame(liguetwonow_HY)
colnames(liguetwonow_HY) <- "AVGHY"

liguetwonow_AY <- as.data.frame(liguetwonow_AY)
colnames(liguetwonow_AY) <- "AVGAY"

liguetwonow_HCO <- as.data.frame(liguetwonow_HCO)
colnames(liguetwonow_HCO) <- "AVGHCO"

liguetwonow_ACO <- as.data.frame(liguetwonow_ACO)
colnames(liguetwonow_ACO) <- "AVGACO"

liguetwonow_HXSC <- as.data.frame(liguetwonow_HXSC)
colnames(liguetwonow_HXSC) <- "HXSC"

liguetwonow_AXSC <- as.data.frame(liguetwonow_AXSC)
colnames(liguetwonow_AXSC) <- "AXSC"

liguetwonow_HYCPF <- as.data.frame(liguetwonow_HYCPF)
colnames(liguetwonow_HYCPF) <- "HYCPF"

liguetwonow_AYCPF <- as.data.frame(liguetwonow_AYCPF)
colnames(liguetwonow_AYCPF) <- "AYCPF"

liguetwonow_picks <- cbind(LIGUETWONOW_fixtures$Div,LIGUETWONOW_fixtures$HomeTeam_liguetwonow,LIGUETWONOW_fixtures$AwayTeam_liguetwonow,liguetwonow_prediction,liguetwonow_HWM,liguetwonow_AWM,liguetwonow_HWMLM,liguetwonow_AWMLM,liguetwonow_HY,liguetwonow_AY,liguetwonow_HCO,liguetwonow_ACO,liguetwonow_HXSC,liguetwonow_AXSC,liguetwonow_HYCPF,liguetwonow_AYCPF)

colnames(liguetwonow_picks)[1] <- "picks_Div"
colnames(liguetwonow_picks)[2] <- "picks_HomeTeam"
colnames(liguetwonow_picks)[3] <- "picks_AwayTeam"
liguetwonow_picks$matchid <- paste(liguetwonow_picks$picks_HomeTeam,liguetwonow_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of LIGUETWONOW
liguetwonow_picks
#############################################################################################################################################################################
#clone fixtures
LIGUETWONOW_fixtures_clone <- LIGUETWONOW_fixtures
colnames(LIGUETWONOW_fixtures_clone)[61] <- "Hwin"
colnames(LIGUETWONOW_fixtures_clone)[62] <- "Draw"
colnames(LIGUETWONOW_fixtures_clone)[63] <- "Awin"

LIGUETWONOW_fixtures_clone$Hwinodds <-   LIGUETWONOW_fixtures$liguetwonow_1_0 + LIGUETWONOW_fixtures$liguetwonow_2_0 + LIGUETWONOW_fixtures$liguetwonow_2_1 + LIGUETWONOW_fixtures$liguetwonow_3_0 + LIGUETWONOW_fixtures$liguetwonow_3_1 +
  LIGUETWONOW_fixtures$liguetwonow_3_2 + LIGUETWONOW_fixtures$liguetwonow_4_0 + LIGUETWONOW_fixtures$liguetwonow_4_1 + LIGUETWONOW_fixtures$liguetwonow_4_2 + LIGUETWONOW_fixtures$liguetwonow_4_3 +
  LIGUETWONOW_fixtures$liguetwonow_5_0 + LIGUETWONOW_fixtures$liguetwonow_5_1 + LIGUETWONOW_fixtures$liguetwonow_5_2 + LIGUETWONOW_fixtures$liguetwonow_5_3 + LIGUETWONOW_fixtures$liguetwonow_5_4 +
  LIGUETWONOW_fixtures$liguetwonow_6_0 + LIGUETWONOW_fixtures$liguetwonow_6_1 + LIGUETWONOW_fixtures$liguetwonow_6_2 + LIGUETWONOW_fixtures$liguetwonow_6_3 + LIGUETWONOW_fixtures$liguetwonow_6_4 +
  LIGUETWONOW_fixtures$liguetwonow_6_5
LIGUETWONOW_fixtures_clone$Hwinodds <- round(1/LIGUETWONOW_fixtures_clone$Hwinodds, digits = 3)

LIGUETWONOW_fixtures_clone$Drawodds <-  LIGUETWONOW_fixtures$liguetwonow_0_0 + LIGUETWONOW_fixtures$liguetwonow_1_1 + LIGUETWONOW_fixtures$liguetwonow_2_2 + LIGUETWONOW_fixtures$liguetwonow_3_3 + LIGUETWONOW_fixtures$liguetwonow_4_4 +
  LIGUETWONOW_fixtures$liguetwonow_5_5 + LIGUETWONOW_fixtures$liguetwonow_6_6

LIGUETWONOW_fixtures_clone$Drawodds <- round(1/LIGUETWONOW_fixtures_clone$Drawodds, digits = 3)

LIGUETWONOW_fixtures_clone$Awinodds <-   LIGUETWONOW_fixtures$liguetwonow_0_1 + LIGUETWONOW_fixtures$liguetwonow_0_2 + LIGUETWONOW_fixtures$liguetwonow_1_2 + LIGUETWONOW_fixtures$liguetwonow_0_3 + LIGUETWONOW_fixtures$liguetwonow_1_3 +
  LIGUETWONOW_fixtures$liguetwonow_2_3 + LIGUETWONOW_fixtures$liguetwonow_0_4 + LIGUETWONOW_fixtures$liguetwonow_1_4 + LIGUETWONOW_fixtures$liguetwonow_2_4 + LIGUETWONOW_fixtures$liguetwonow_3_4 +
  LIGUETWONOW_fixtures$liguetwonow_0_5 + LIGUETWONOW_fixtures$liguetwonow_1_5 + LIGUETWONOW_fixtures$liguetwonow_2_5 + LIGUETWONOW_fixtures$liguetwonow_3_5 + LIGUETWONOW_fixtures$liguetwonow_4_5 +
  LIGUETWONOW_fixtures$liguetwonow_0_6 + LIGUETWONOW_fixtures$liguetwonow_1_6 + LIGUETWONOW_fixtures$liguetwonow_2_6 + LIGUETWONOW_fixtures$liguetwonow_3_6 + LIGUETWONOW_fixtures$liguetwonow_4_6 +
  LIGUETWONOW_fixtures$liguetwonow_5_6

LIGUETWONOW_fixtures_clone$Awinodds <- round(1/LIGUETWONOW_fixtures_clone$Awinodds, digits = 3)

colnames(LIGUETWONOW_fixtures_clone)[15] <- "CS_1-1"
colnames(LIGUETWONOW_fixtures_clone)[13] <- "CS_1-0"
colnames(LIGUETWONOW_fixtures_clone)[14] <- "CS_0-1"
colnames(LIGUETWONOW_fixtures_clone)[16] <- "CS_2-0"
colnames(LIGUETWONOW_fixtures_clone)[17] <- "CS_0-2"
colnames(LIGUETWONOW_fixtures_clone)[19] <- "CS_2-1"
colnames(LIGUETWONOW_fixtures_clone)[20] <- "CS_1-2"

LIGUETWONOW_fixtures_clone$`CS_1-1` <- round(1/LIGUETWONOW_fixtures_clone$`CS_1-1`, digits = 3)
LIGUETWONOW_fixtures_clone$`CS_1-0` <- round(1/LIGUETWONOW_fixtures_clone$`CS_1-0`, digits = 3)
LIGUETWONOW_fixtures_clone$`CS_0-1` <- round(1/LIGUETWONOW_fixtures_clone$`CS_0-1`, digits = 3)
LIGUETWONOW_fixtures_clone$`CS_2-0` <- round(1/LIGUETWONOW_fixtures_clone$`CS_2-0`, digits = 3)
LIGUETWONOW_fixtures_clone$`CS_0-2` <- round(1/LIGUETWONOW_fixtures_clone$`CS_0-2`, digits = 3)
LIGUETWONOW_fixtures_clone$`CS_2-1` <- round(1/LIGUETWONOW_fixtures_clone$`CS_2-1`, digits = 3)
LIGUETWONOW_fixtures_clone$`CS_1-2` <- round(1/LIGUETWONOW_fixtures_clone$`CS_1-2`, digits = 3)

colnames(LIGUETWONOW_fixtures_clone)[1] <- "league"
colnames(LIGUETWONOW_fixtures_clone)[2] <- "Hometeam"
colnames(LIGUETWONOW_fixtures_clone)[3] <- "Awayteam"
colnames(LIGUETWONOW_fixtures_clone)[92] <- "predscore"
colnames(LIGUETWONOW_fixtures_clone)[64] <- "ov25"
colnames(LIGUETWONOW_fixtures_clone)[66] <- "ov25odds"
colnames(LIGUETWONOW_fixtures_clone)[65] <- "un25"
colnames(LIGUETWONOW_fixtures_clone)[67] <- "un25odds"
colnames(LIGUETWONOW_fixtures_clone)[68] <- "BTTSY"
colnames(LIGUETWONOW_fixtures_clone)[69] <- "BTTSN"
colnames(LIGUETWONOW_fixtures_clone)[70] <- "BTTSYodds"
colnames(LIGUETWONOW_fixtures_clone)[71] <- "BTTSNodds"

LIGUETWONOW_fixtures_clone <- LIGUETWONOW_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
LIGUETWONOW_fixtures_clone$matchid <- paste(LIGUETWONOW_fixtures_clone$Hometeam,LIGUETWONOW_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
LIGUETWONOW_fixtures_clone_final <- LIGUETWONOW_fixtures_clone[,-c(8,9,10,27)]
LIGUETWONOW_fixtures_clone_final[,'sep'] <- ''

liguetwonow_dmprediction <-  liguetwonow_picks[,c(4,5,6,7,8)]
liguetwonow_dmprediction[,'sep2'] <- ''

liguetwonow_avgyellow <- liguetwonow_picks[,c(9,10)]
liguetwonow_avgyellow[,'sep3'] <- ''

liguetwonow_avgcorners <- liguetwonow_picks[,c(11,12)]
liguetwonow_avgcorners[,'sep4'] <- ''

liguetwonow_goals <- LIGUETWONOW_fixtures[,c(10,11)]
liguetwonow_goals$liguetwonow_xGH <- round(liguetwonow_goals$liguetwonow_xGH, digits = 2)
liguetwonow_goals$liguetwonow_xGA <- round(liguetwonow_goals$liguetwonow_xGA, digits = 2)
liguetwonow_goals$liguetwonow_TxG <- liguetwonow_goals$liguetwonow_xGH + liguetwonow_goals$liguetwonow_xGA
liguetwonow_goals[,'sep5'] <- ''

liguetwonow_shots <- LIGUETWONOW_fixtures_sot[,c(10,11)]
liguetwonow_shots$liguetwonow_xHST <- round(liguetwonow_shots$liguetwonow_xHST, digits = 2)
liguetwonow_shots$liguetwonow_xAST <- round(liguetwonow_shots$liguetwonow_xAST, digits = 2)
liguetwonow_shots$TxSOT <- liguetwonow_shots$liguetwonow_xHST + liguetwonow_shots$liguetwonow_xAST
liguetwonow_shots[,'sep6'] <- ''

liguetwonow_fouls <- LIGUETWONOW_fixtures_fo[,c(10,11)]
liguetwonow_fouls$liguetwonow_xHF <- round(liguetwonow_fouls$liguetwonow_xHF, digits = 2)
liguetwonow_fouls$liguetwonow_xAF <- round(liguetwonow_fouls$liguetwonow_xAF, digits = 2)
liguetwonow_fouls$liguetwonow_TxF <- liguetwonow_fouls$liguetwonow_xHF + liguetwonow_fouls$liguetwonow_xAF

liguetwonow_ycpf <- liguetwonow_picks[,c(15,16)]
liguetwonow_fouls <- cbind(liguetwonow_fouls,liguetwonow_ycpf)
liguetwonow_fouls$HYCPF <- as.numeric(liguetwonow_fouls$HYCPF)
liguetwonow_fouls$AYCPF <- as.numeric(liguetwonow_fouls$AYCPF)
liguetwonow_fouls$x_hyc <- (liguetwonow_fouls$liguetwonow_xHF) * (liguetwonow_fouls$HYCPF)
liguetwonow_fouls$x_ayc <- (liguetwonow_fouls$liguetwonow_xAF) * (liguetwonow_fouls$AYCPF)
liguetwonow_fouls$x_TYC <- round((liguetwonow_fouls$x_hyc + liguetwonow_fouls$x_ayc),digits = 2)
liguetwonow_fouls[,'sep7'] <- ''

liguetwonow_bookings <- LIGUETWONOW_fixtures_yc[,c(10,11)]
liguetwonow_bookings$liguetwonow_xHYC <- round(liguetwonow_bookings$liguetwonow_xHYC, digits = 2)
liguetwonow_bookings$liguetwonow_xAYC <- round(liguetwonow_bookings$liguetwonow_xAYC, digits = 2)
liguetwonow_bookings$liguetwonow_TYcards <- liguetwonow_bookings$liguetwonow_xHYC + liguetwonow_bookings$liguetwonow_xAYC
liguetwonow_bookings[,'sep8'] <- ''

liguetwonow_corners <- LIGUETWONOW_fixtures_co[,c(10,11)]
liguetwonow_corners$liguetwonow_xHCOC <- round(liguetwonow_corners$liguetwonow_xHCOC, digits = 2)
liguetwonow_corners$liguetwonow_xACOC <- round(liguetwonow_corners$liguetwonow_xACOC, digits = 2)
liguetwonow_corners$liguetwonow_TCOs <- liguetwonow_corners$liguetwonow_xHCOC + liguetwonow_corners$liguetwonow_xACOC
liguetwonow_corners[,'sep9'] <- ''

liguetwonow_shotsconversion <- liguetwonow_picks[,c(13,14)]
liguetwonow_shotsconversion <- cbind(liguetwonow_shotsconversion,liguetwonow_shots)
liguetwonow_shotsconversion$HXSC <- as.numeric(liguetwonow_shotsconversion$HXSC)
liguetwonow_shotsconversion$AXSC <- as.numeric(liguetwonow_shotsconversion$AXSC)
liguetwonow_shotsconversion$liguetwonow_hXgoals <- round((liguetwonow_shotsconversion$HXSC * liguetwonow_shotsconversion$liguetwonow_xHST), digits = 2)
liguetwonow_shotsconversion$liguetwonow_aXgoals <- round((liguetwonow_shotsconversion$AXSC * liguetwonow_shotsconversion$liguetwonow_xAST), digits = 2)
liguetwonow_shotsconversion$Xgoals <- liguetwonow_shotsconversion$liguetwonow_hXgoals + liguetwonow_shotsconversion$liguetwonow_aXgoals
options(java.parameters = "-Xmx4g")
LIGUETWONOW_all <- cbind(LIGUETWONOW_fixtures_clone_final,liguetwonow_dmprediction,liguetwonow_avgyellow,liguetwonow_avgcorners,liguetwonow_goals,liguetwonow_shots,liguetwonow_fouls,liguetwonow_bookings,liguetwonow_corners,liguetwonow_shotsconversion)
unlink('Divisions/LIGUETWONOW.xlsx')
write.xlsx(LIGUETWONOW_all,'Divisions/LIGUETWONOW.xlsx', sheetName = "LIGUETWONOW_all", append = TRUE)
write.xlsx(points_liguetwonow,'Divisions/LIGUETWONOW.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(liguetwonow_cornertotalsv2,'Divisions/LIGUETWONOW.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(liguetwonow_goaltotalsv2,'Divisions/LIGUETWONOW.xlsx', sheetName = "Goaltotals", append = TRUE)


write.csv(LIGUETWONOW_fixtures[,c(1,2,3,4,5,6)],'F2_schedule20232024.csv')
