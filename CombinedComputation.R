#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
#source('divisions.R')
#source('Matchday.R')
sp2_currentround
#first_df <- E0_rounds[E0_rounds$e0_matchday > 32,]
#second_df <- E1_rounds[E1_rounds$e1_matchday > 40,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#LALIGATWO <- rbind(first_df,second_df,third_df)
LALIGATWO <- SP2_rounds[SP2_rounds$sp2_matchday >= 1,]
#LALIGATWO <- na.omit(LALIGATWO)
#goaltotals v2
laligatwo_goaltotalsv2 <- tapply(LALIGATWO$TG, LALIGATWO[c("HomeTeam", "AwayTeam")],mean)
laligatwo_hgtotals <- rowSums(laligatwo_goaltotalsv2, na.rm = T)
laligatwo_agtotals <- colSums(laligatwo_goaltotalsv2, na.rm = T)
laligatwo_goaltotalsv2 <- cbind(laligatwo_goaltotalsv2,laligatwo_hgtotals,laligatwo_agtotals)
laligatwo_totalgoals <- laligatwo_hgtotals + laligatwo_agtotals
laligatwo_goaltotalsv2 <- cbind(laligatwo_goaltotalsv2,laligatwo_totalgoals)
laligatwo_teams <- sort(unique(LALIGATWO$HomeTeam))
laligatwo_home_games <- c()
laligatwo_away_games <-c()
for (i_laligatwo in 1:length(laligatwo_teams))
{

  laligatwo_home_games[i_laligatwo] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo],])
  laligatwo_away_games[i_laligatwo]  <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo],])

}
laligatwo_games_played <- laligatwo_home_games + laligatwo_away_games
laligatwo_goaltotalsv2 <- cbind(laligatwo_goaltotalsv2,laligatwo_games_played)
laligatwo_avg_totalgoals <- round((laligatwo_totalgoals/ laligatwo_games_played), digits = 4)
laligatwo_goaltotalsv2[is.na(laligatwo_goaltotalsv2)] <- ""
laligatwo_goaltotalsv2 <- cbind(laligatwo_goaltotalsv2,laligatwo_avg_totalgoals)

############################################################################################################
#Cornertotals v2
laligatwo_cornertotalsv2 <- tapply(LALIGATWO$TC, LALIGATWO[c("HomeTeam", "AwayTeam")],mean)
laligatwo_hcototals <- rowSums(laligatwo_cornertotalsv2, na.rm = T)
laligatwo_acototals <- colSums(laligatwo_cornertotalsv2, na.rm = T)
laligatwo_cornertotalsv2 <- cbind(laligatwo_cornertotalsv2,laligatwo_hcototals,laligatwo_acototals)
laligatwo_totalcorners <- laligatwo_hcototals + laligatwo_acototals
laligatwo_cornertotalsv2 <- cbind(laligatwo_cornertotalsv2,laligatwo_totalcorners)
laligatwo_cornertotalsv2 <- cbind(laligatwo_cornertotalsv2,laligatwo_games_played)
laligatwo_avg_totalcorners <- round((laligatwo_totalcorners/ laligatwo_games_played), digits = 4)
laligatwo_cornertotalsv2[is.na(laligatwo_cornertotalsv2)] <- ""
laligatwo_cornertotalsv2 <- cbind(laligatwo_cornertotalsv2,laligatwo_avg_totalcorners)
############################################################################################################
#GS matrix
laligatwo_goalscored_h <- tapply(LALIGATWO$FTHG, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_goalscored_a <- tapply(LALIGATWO$FTAG, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_goalscored_h[is.na(laligatwo_goalscored_h)] <- ""
laligatwo_goalscored_a[is.na(laligatwo_goalscored_a)] <- ""
for(laligatwo_rowhgs in 1:nrow(laligatwo_goalscored_h)) {
  for(laligatwo_colhgs in 1:ncol(laligatwo_goalscored_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowags in 1:nrow(laligatwo_goalscored_a)) {
      for(laligatwo_colags in 1:ncol(laligatwo_goalscored_a)) {
        ifelse(!laligatwo_goalscored_a[laligatwo_rowags,laligatwo_colags]=="",laligatwo_goalscored_h[laligatwo_rowags,laligatwo_colags] <- laligatwo_goalscored_a[laligatwo_rowags,laligatwo_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
laligatwo_goalconceded_h <- tapply(LALIGATWO$FTAG, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_goalconceded_a <- tapply(LALIGATWO$FTHG, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_goalconceded_h[is.na(laligatwo_goalconceded_h)] <- ""
laligatwo_goalconceded_a[is.na(laligatwo_goalconceded_a)] <- ""
for(laligatwo_rowhgc in 1:nrow(laligatwo_goalconceded_h)) {
  for(laligatwo_colhgc in 1:ncol(laligatwo_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowagc in 1:nrow(laligatwo_goalconceded_a)) {
      for(laligatwo_colagc in 1:ncol(laligatwo_goalconceded_a)) {
        ifelse(!laligatwo_goalconceded_a[laligatwo_rowagc,laligatwo_colagc]=="",laligatwo_goalconceded_h[laligatwo_rowagc,laligatwo_colagc] <- laligatwo_goalconceded_a[laligatwo_rowagc,laligatwo_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
laligatwo_totalcorners_h <- tapply(LALIGATWO$TC, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_totalcorners_a <- tapply(LALIGATWO$TC, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_totalcorners_h[is.na(laligatwo_totalcorners_h)] <- ""
laligatwo_totalcorners_a[is.na(laligatwo_totalcorners_a)] <- ""
#LALIGATWO
for(laligatwo_rowTC in 1:nrow(laligatwo_totalcorners_h)) {
  for(laligatwo_colTC in 1:ncol(laligatwo_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowTC in 1:nrow(laligatwo_totalcorners_a)) {
      for(laligatwo_colTC in 1:ncol(laligatwo_totalcorners_a)) {
        ifelse(!laligatwo_totalcorners_a[laligatwo_rowTC,laligatwo_colTC]=="",laligatwo_totalcorners_h[laligatwo_rowTC,laligatwo_colTC] <- laligatwo_totalcorners_a[laligatwo_rowTC,laligatwo_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
laligatwo_coawarded_h <- tapply(LALIGATWO$HCO, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_coawarded_a <- tapply(LALIGATWO$ACO, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_coawarded_h[is.na(laligatwo_coawarded_h)] <- ""
laligatwo_coawarded_a[is.na(laligatwo_coawarded_a)] <- ""
#LALIGATWO
for(laligatwo_rowhco in 1:nrow(laligatwo_coawarded_h)) {
  for(laligatwo_colhco in 1:ncol(laligatwo_coawarded_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowaco in 1:nrow(laligatwo_coawarded_a)) {
      for(laligatwo_colaco in 1:ncol(laligatwo_coawarded_a)) {
        ifelse(!laligatwo_coawarded_a[laligatwo_rowaco,laligatwo_colaco]=="",laligatwo_coawarded_h[laligatwo_rowaco,laligatwo_colaco] <- laligatwo_coawarded_a[laligatwo_rowaco,laligatwo_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
laligatwo_cornersconceded_h <- tapply(LALIGATWO$ACO, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_cornersconceded_a <- tapply(LALIGATWO$HCO, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_cornersconceded_h[is.na(laligatwo_cornersconceded_h)] <- ""
laligatwo_cornersconceded_a[is.na(laligatwo_cornersconceded_a)] <- ""
#LALIGATWO
for(laligatwo_rowhcc in 1:nrow(laligatwo_cornersconceded_h)) {
  for(laligatwo_colhcc in 1:ncol(laligatwo_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowacc in 1:nrow(laligatwo_cornersconceded_a)) {
      for(laligatwo_colacc in 1:ncol(laligatwo_cornersconceded_a)) {
        ifelse(!laligatwo_cornersconceded_a[laligatwo_rowacc,laligatwo_colacc]=="",laligatwo_cornersconceded_h[laligatwo_rowacc,laligatwo_colacc] <- laligatwo_cornersconceded_a[laligatwo_rowacc,laligatwo_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
laligatwo_coscform_h <- tapply(LALIGATWO$COSC, LALIGATWO[c("HomeTeam", "Date")],median)
laligatwo_coscform_a <- tapply(LALIGATWO$COSC, LALIGATWO[c("AwayTeam", "Date")],median)
laligatwo_coscform_h[is.na(laligatwo_coscform_h)] <- ""
laligatwo_coscform_a[is.na(laligatwo_coscform_a)] <- ""
#LALIGATWO
for(laligatwo_rowh_f_cosc in 1:nrow(laligatwo_coscform_h)) {
  for(laligatwo_colh_f_cosc in 1:ncol(laligatwo_coscform_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowa_f_cosc in 1:nrow(laligatwo_coscform_a)) {
      for(laligatwo_cola_f_cosc in 1:ncol(laligatwo_coscform_a)) {
        ifelse(!laligatwo_coscform_a[laligatwo_rowa_f_cosc,laligatwo_cola_f_cosc]=="",laligatwo_coscform_h[laligatwo_rowa_f_cosc,laligatwo_cola_f_cosc] <- laligatwo_coscform_a[laligatwo_rowa_f_cosc,laligatwo_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
laligatwo_winmargin_h <- tapply(LALIGATWO$FTHG - LALIGATWO$FTAG, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_winmargin_a <- tapply(LALIGATWO$FTAG - LALIGATWO$FTHG, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_winmargin_h[is.na(laligatwo_winmargin_h)] <- ""
laligatwo_winmargin_a[is.na(laligatwo_winmargin_a)] <- ""
#LALIGATWO
for(laligatwo_rowhwm in 1:nrow(laligatwo_winmargin_h)) {
  for(laligatwo_colhwm in 1:ncol(laligatwo_winmargin_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowawm in 1:nrow(laligatwo_winmargin_a)) {
      for(laligatwo_colawm in 1:ncol(laligatwo_winmargin_a)) {
        ifelse(!laligatwo_winmargin_a[laligatwo_rowawm,laligatwo_colawm]=="",laligatwo_winmargin_h[laligatwo_rowawm,laligatwo_colawm] <- laligatwo_winmargin_a[laligatwo_rowawm,laligatwo_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
laligatwo_yellowscored_h <- tapply(LALIGATWO$HY, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_yellowscored_a <- tapply(LALIGATWO$AY, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_yellowscored_h[is.na(laligatwo_yellowscored_h)] <- ""
laligatwo_yellowscored_a[is.na(laligatwo_yellowscored_a)] <- ""
#LALIGATWO
for(laligatwo_rowhys in 1:nrow(laligatwo_yellowscored_h)) {
  for(laligatwo_colhys in 1:ncol(laligatwo_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_roways in 1:nrow(laligatwo_yellowscored_a)) {
      for(laligatwo_colays in 1:ncol(laligatwo_yellowscored_a)) {
        ifelse(!laligatwo_yellowscored_a[laligatwo_roways,laligatwo_colays]=="",laligatwo_yellowscored_h[laligatwo_roways,laligatwo_colays] <- laligatwo_yellowscored_a[laligatwo_roways,laligatwo_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
laligatwo_redscored_h <- tapply(LALIGATWO$HR, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_redscored_a <- tapply(LALIGATWO$AR, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_redscored_h[is.na(laligatwo_redscored_h)] <- ""
laligatwo_redscored_a[is.na(laligatwo_redscored_a)] <- ""
for(laligatwo_rowhrs in 1:nrow(laligatwo_redscored_h)) {
  for(laligatwo_colhrs in 1:ncol(laligatwo_redscored_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowars in 1:nrow(laligatwo_redscored_a)) {
      for(laligatwo_colars in 1:ncol(laligatwo_redscored_a)) {
        ifelse(!laligatwo_redscored_a[laligatwo_rowars,laligatwo_colars]=="",laligatwo_redscored_h[laligatwo_rowars,laligatwo_colars] <- laligatwo_redscored_a[laligatwo_rowars,laligatwo_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
laligatwo_redtotalsv2 <- tapply(LALIGATWO$TR, LALIGATWO[c("HomeTeam", "AwayTeam")],mean)
laligatwo_hrtotals <- rowSums(laligatwo_redtotalsv2, na.rm = T)
laligatwo_artotals <- colSums(laligatwo_redtotalsv2, na.rm = T)
laligatwo_redtotalsv2 <- cbind(laligatwo_redtotalsv2,laligatwo_hrtotals,laligatwo_artotals)
laligatwo_totalreds <- laligatwo_hrtotals + laligatwo_artotals
laligatwo_redtotalsv2 <- cbind(laligatwo_redtotalsv2,laligatwo_totalreds)
laligatwo_redtotalsv2 <- cbind(laligatwo_redtotalsv2,laligatwo_games_played)
laligatwo_avg_totalreds <- round((laligatwo_totalreds/ laligatwo_games_played), digits = 4)
laligatwo_redtotalsv2[is.na(laligatwo_redtotalsv2)] <- ""
laligatwo_redtotalsv2 <- cbind(laligatwo_redtotalsv2,laligatwo_avg_totalreds)
############################################################################################################################################################
#yellowtotals
laligatwo_yellowtotalsv2 <- tapply(LALIGATWO$TY, LALIGATWO[c("HomeTeam", "AwayTeam")],mean)
laligatwo_hytotals <- rowSums(laligatwo_yellowtotalsv2, na.rm = T)
laligatwo_aytotals <- colSums(laligatwo_yellowtotalsv2, na.rm = T)
laligatwo_yellowtotalsv2 <- cbind(laligatwo_yellowtotalsv2,laligatwo_hytotals,laligatwo_aytotals)
laligatwo_totalyellows <- laligatwo_hytotals + laligatwo_aytotals
laligatwo_yellowtotalsv2 <- cbind(laligatwo_yellowtotalsv2,laligatwo_totalyellows)
laligatwo_yellowtotalsv2 <- cbind(laligatwo_yellowtotalsv2,laligatwo_games_played)
laligatwo_avg_totalyellows <- round((laligatwo_totalyellows/ laligatwo_games_played), digits = 4)
laligatwo_yellowtotalsv2[is.na(laligatwo_yellowtotalsv2)] <- ""
laligatwo_yellowtotalsv2 <- cbind(laligatwo_yellowtotalsv2,laligatwo_avg_totalyellows)
##################################################################################################################################################
#team form
laligatwo_form_h <- tapply(LALIGATWO$FTR, LALIGATWO[c("HomeTeam", "Date")],median)
laligatwo_form_a <- tapply(LALIGATWO$FTR, LALIGATWO[c("AwayTeam", "Date")],median)
laligatwo_form_h[is.na(laligatwo_form_h)] <- ""
laligatwo_form_a[is.na(laligatwo_form_a)] <- ""
laligatwo_form_h <- sub("A","L",laligatwo_form_h)
laligatwo_form_h <- sub("H","W",laligatwo_form_h)
laligatwo_form_a <- sub("A","W",laligatwo_form_a)
laligatwo_form_a <- sub("H","L",laligatwo_form_a)
for(laligatwo_rowh_f in 1:nrow(laligatwo_form_h)) {
  for(laligatwo_colh_f in 1:ncol(laligatwo_form_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowa_f in 1:nrow(laligatwo_form_a)) {
      for(laligatwo_cola_f in 1:ncol(laligatwo_form_a)) {
        ifelse(!laligatwo_form_a[laligatwo_rowa_f,laligatwo_cola_f]=="",laligatwo_form_h[laligatwo_rowa_f,laligatwo_cola_f] <- laligatwo_form_a[laligatwo_rowa_f,laligatwo_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
laligatwo_csform_h <- tapply(LALIGATWO$CS, LALIGATWO[c("HomeTeam", "Date")],median)
laligatwo_csform_a <- tapply(LALIGATWO$CS, LALIGATWO[c("AwayTeam", "Date")],median)
laligatwo_csform_h[is.na(laligatwo_csform_h)] <- ""
laligatwo_csform_a[is.na(laligatwo_csform_a)] <- ""
#LALIGATWO
for(laligatwo_rowh_f_cs in 1:nrow(laligatwo_csform_h)) {
  for(laligatwo_colh_f_cs in 1:ncol(laligatwo_csform_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowa_f_cs in 1:nrow(laligatwo_csform_a)) {
      for(laligatwo_cola_f_cs in 1:ncol(laligatwo_csform_a)) {
        ifelse(!laligatwo_csform_a[laligatwo_rowa_f_cs,laligatwo_cola_f_cs]=="",laligatwo_csform_h[laligatwo_rowa_f_cs,laligatwo_cola_f_cs] <- laligatwo_csform_a[laligatwo_rowa_f_cs,laligatwo_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
laligatwo_totalgoals_h <- tapply(LALIGATWO$TG, LALIGATWO[c("HomeTeam", "Date")],mean)
laligatwo_totalgoals_a <- tapply(LALIGATWO$TG, LALIGATWO[c("AwayTeam", "Date")],mean)
laligatwo_totalgoals_h[is.na(laligatwo_totalgoals_h)] <- ""
laligatwo_totalgoals_a[is.na(laligatwo_totalgoals_a)] <- ""
for(laligatwo_rowh in 1:nrow(laligatwo_totalgoals_h)) {
  for(laligatwo_colh in 1:ncol(laligatwo_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowa in 1:nrow(laligatwo_totalgoals_a)) {
      for(laligatwo_cola in 1:ncol(laligatwo_totalgoals_a)) {
        ifelse(!laligatwo_totalgoals_a[laligatwo_rowa,laligatwo_cola]=="",laligatwo_totalgoals_h[laligatwo_rowa,laligatwo_cola] <- laligatwo_totalgoals_a[laligatwo_rowa,laligatwo_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#LALIGATWO
laligatwo_un05_home <- c()
laligatwo_un05_away <- c()
laligatwo_ov05_home <- c()
laligatwo_ov05_away <- c()

laligatwo_un15_home <- c()
laligatwo_un15_away <- c()
laligatwo_ov15_home <- c()
laligatwo_ov15_away <- c()

laligatwo_un25_home <- c()
laligatwo_un25_away <- c()
laligatwo_ov25_home <- c()
laligatwo_ov25_away <- c()

laligatwo_un35_home <- c()
laligatwo_un35_away <- c()
laligatwo_ov35_home <- c()
laligatwo_ov35_away <- c()

laligatwo_un45_home <- c()
laligatwo_un45_away <- c()
laligatwo_ov45_home <- c()
laligatwo_ov45_away <- c()

laligatwo_un55_home <- c()
laligatwo_un55_away <- c()
laligatwo_ov55_home <- c()
laligatwo_ov55_away <- c()

for (i_laligatwo_tg in 1:length(laligatwo_teams))
{

  laligatwo_un05_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG == 0,])
  laligatwo_un05_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG == 0,])

  laligatwo_ov05_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG > 0,])
  laligatwo_ov05_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG > 0,])

  laligatwo_un15_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 1,])
  laligatwo_un15_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 1,])

  laligatwo_ov15_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 2,])
  laligatwo_ov15_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 2,])

  laligatwo_un25_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 2,])
  laligatwo_un25_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 2,])

  laligatwo_ov25_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >=3,])
  laligatwo_ov25_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >=3,])

  laligatwo_un35_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 3,])
  laligatwo_un35_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 3,])

  laligatwo_ov35_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 4,])
  laligatwo_ov35_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 4,])

  laligatwo_un45_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 4,])
  laligatwo_un45_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 4,])

  laligatwo_ov45_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 5,])
  laligatwo_ov45_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 5,])

  laligatwo_un55_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 5,])
  laligatwo_un55_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG <= 5,])

  laligatwo_ov55_home[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 6,])
  laligatwo_ov55_away[i_laligatwo_tg] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_tg] & LALIGATWO$TG >= 6,])


}

laligatwo_un05 <- laligatwo_un05_home + laligatwo_un05_away
laligatwo_ov05 <- laligatwo_ov05_home + laligatwo_ov05_away

laligatwo_un15 <- laligatwo_un15_home + laligatwo_un15_away
laligatwo_ov15 <- laligatwo_ov15_home + laligatwo_ov15_away

laligatwo_un25 <- laligatwo_un25_home + laligatwo_un25_away
laligatwo_ov25 <- laligatwo_ov25_home + laligatwo_ov25_away

laligatwo_un35 <- laligatwo_un35_home + laligatwo_un35_away
laligatwo_ov35 <- laligatwo_ov35_home + laligatwo_ov35_away

laligatwo_un45 <- laligatwo_un45_home + laligatwo_un45_away
laligatwo_ov45 <- laligatwo_ov45_home + laligatwo_ov45_away

laligatwo_un55 <- laligatwo_un55_home + laligatwo_un55_away
laligatwo_ov55 <- laligatwo_ov55_home + laligatwo_ov55_away

laligatwo_ovundata <- cbind(laligatwo_teams,laligatwo_un05,laligatwo_ov05,laligatwo_un15,laligatwo_ov15,laligatwo_un25,laligatwo_ov25,laligatwo_un35,laligatwo_ov35,laligatwo_un45,laligatwo_ov45,laligatwo_un55,laligatwo_ov55)
#################################################################################################################################################################
#team against
laligatwo_form_team_against_h <- tapply(LALIGATWO$AwayTeam, LALIGATWO[c("HomeTeam", "Date")],median)
laligatwo_form_team_against_a <- tapply(LALIGATWO$HomeTeam, LALIGATWO[c("AwayTeam", "Date")],median)
laligatwo_form_team_against_h[is.na(laligatwo_form_team_against_h)] <- ""
laligatwo_form_team_against_a[is.na(laligatwo_form_team_against_a)] <- ""
#LALIGATWO
for(laligatwo_rowh_f_against in 1:nrow(laligatwo_form_team_against_h)) {
  for(laligatwo_colh_f_against in 1:ncol(laligatwo_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(laligatwo_rowa_f_against in 1:nrow(laligatwo_form_team_against_a)) {
      for(laligatwo_cola_f_against in 1:ncol(laligatwo_form_team_against_a)) {
        ifelse(!laligatwo_form_team_against_a[laligatwo_rowa_f_against,laligatwo_cola_f_against]=="",laligatwo_form_team_against_h[laligatwo_rowa_f_against,laligatwo_cola_f_against] <- laligatwo_form_team_against_a[laligatwo_rowa_f_against,laligatwo_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#LALIGATWO
#home goals scored
laligatwo_home_gs <- aggregate(LALIGATWO$FTHG, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_home_gs_avg <- aggregate(LALIGATWO$FTHG, by = list(LALIGATWO$HomeTeam),mean)
laligatwo_home_scoring <- merge(laligatwo_home_gs,laligatwo_home_gs_avg, by='Group.1',all = T)
names(laligatwo_home_scoring)[names(laligatwo_home_scoring) == "x.x"] <- "TFthg"
names(laligatwo_home_scoring)[names(laligatwo_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
laligatwo_away_gs <- aggregate(LALIGATWO$FTAG, by = list(LALIGATWO$AwayTeam), FUN = sum)
laligatwo_away_gs_avg <- aggregate(LALIGATWO$FTAG, by = list(LALIGATWO$AwayTeam),mean)
laligatwo_away_scoring <- merge(laligatwo_away_gs,laligatwo_away_gs_avg, by='Group.1',all = T)
names(laligatwo_away_scoring)[names(laligatwo_away_scoring) == "x.x"] <- "TFtag"
names(laligatwo_away_scoring)[names(laligatwo_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
laligatwo_scoring <- merge(laligatwo_home_scoring,laligatwo_away_scoring,by='Group.1',all = T)
laligatwo_scoring$TGS <- laligatwo_scoring$TFthg + laligatwo_scoring$TFtag

#Home shots on target
laligatwo_home_hst <- aggregate(LALIGATWO$HST, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_ast <- aggregate(LALIGATWO$AST, by = list(LALIGATWO$AwayTeam), FUN = sum)
laligatwo_tst <- merge(laligatwo_home_hst,laligatwo_away_ast, by='Group.1',all = T)
names(laligatwo_tst)[names(laligatwo_tst) == "x.x"] <- "hst"
names(laligatwo_tst)[names(laligatwo_tst) == "x.y"] <- "ast"
laligatwo_tst$TST <- laligatwo_tst$hst + laligatwo_tst$ast
#merge goals scored and shots on target
laligatwo_scoring_conversion <- merge(laligatwo_tst,laligatwo_scoring,by='Group.1',all = T)
#add HSC ASC TSC
laligatwo_scoring_conversion$HSTC <- percent(laligatwo_scoring_conversion$TFthg/laligatwo_scoring_conversion$hst, accuracy = 0.01)
laligatwo_scoring_conversion$ASTC <- percent(laligatwo_scoring_conversion$TFtag/laligatwo_scoring_conversion$ast, accuracy = 0.01)
laligatwo_scoring_conversion$TSTC <- percent(laligatwo_scoring_conversion$TGS/laligatwo_scoring_conversion$TST, accuracy = 0.01)
#merge games played
laligatwo_scoring_conversion <- cbind(laligatwo_scoring_conversion,laligatwo_games_played)
#create the second part
#home goals conceded
laligatwo_home_gc <- aggregate(LALIGATWO$FTAG, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_home_gc_avg <- aggregate(LALIGATWO$FTAG, by = list(LALIGATWO$HomeTeam),mean)
laligatwo_home_conceding <- merge(laligatwo_home_gc,laligatwo_home_gc_avg, by='Group.1',all = T)
names(laligatwo_home_conceding)[names(laligatwo_home_conceding) == "x.x"] <- "TFthc"
names(laligatwo_home_conceding)[names(laligatwo_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
laligatwo_away_gc <- aggregate(LALIGATWO$FTHG, by = list(LALIGATWO$AwayTeam), FUN = sum)
laligatwo_away_gc_avg <- aggregate(LALIGATWO$FTHG, by = list(LALIGATWO$AwayTeam),mean)
laligatwo_away_conceding <- merge(laligatwo_away_gc,laligatwo_away_gc_avg, by='Group.1',all = T)
names(laligatwo_away_conceding)[names(laligatwo_away_conceding) == "x.x"] <- "TFtac"
names(laligatwo_away_conceding)[names(laligatwo_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
laligatwo_conceding <- merge(laligatwo_home_conceding,laligatwo_away_conceding,by='Group.1',all = T)
laligatwo_conceding$TGC <- laligatwo_conceding$TFthc + laligatwo_conceding$TFtac
laligatwo_home_hst
#Home shots conceded
laligatwo_home_hsc <- aggregate(LALIGATWO$AST, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_asc <- aggregate(LALIGATWO$HST, by = list(LALIGATWO$AwayTeam), FUN = sum)
laligatwo_tsc <- merge(laligatwo_home_hsc,laligatwo_away_asc, by='Group.1',all = T)
names(laligatwo_tsc)[names(laligatwo_tsc) == "x.x"] <- "hsc"
names(laligatwo_tsc)[names(laligatwo_tsc) == "x.y"] <- "asc"
laligatwo_tsc$TSC <- laligatwo_tsc$hsc + laligatwo_tsc$asc
#merge goals conceded and shots conceded
laligatwo_conceding_conversion <- merge(laligatwo_tsc,laligatwo_conceding,by='Group.1',all = T)

#add HSC ASC TSC
laligatwo_conceding_conversion$HSCC <- percent(laligatwo_conceding_conversion$TFthc/laligatwo_conceding_conversion$hsc, accuracy = 0.01)
laligatwo_conceding_conversion$ASCC <- percent(laligatwo_conceding_conversion$TFtac/laligatwo_conceding_conversion$asc, accuracy = 0.01)
laligatwo_conceding_conversion$TSCC <- percent(laligatwo_conceding_conversion$TGC/laligatwo_conceding_conversion$TSC, accuracy = 0.01)
laligatwo_conceding_conversion$XSTC <- round(laligatwo_scoring$TGS/(laligatwo_tst$TST - laligatwo_scoring$TGS), digits = 2)

#merge the two parts
laligatwo_shots_analysis <- merge(laligatwo_scoring_conversion,laligatwo_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#LALIGATWO
#home fouls for
laligatwo_home_fouls <- aggregate(LALIGATWO$HF, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_home_fouls_avg <- aggregate(LALIGATWO$HF, by = list(LALIGATWO$HomeTeam),mean)
laligatwo_home_foulsdata <- merge(laligatwo_home_fouls,laligatwo_home_fouls_avg, by='Group.1',all = T)
names(laligatwo_home_foulsdata)[names(laligatwo_home_foulsdata) == "x.x"] <- "THfouls"
names(laligatwo_home_foulsdata)[names(laligatwo_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
laligatwo_away_fouls <- aggregate(LALIGATWO$HF, by = list(LALIGATWO$AwayTeam), FUN = sum)
laligatwo_away_fouls_avg <- aggregate(LALIGATWO$HF, by = list(LALIGATWO$AwayTeam),mean)
laligatwo_away_foulsdata <- merge(laligatwo_away_fouls,laligatwo_away_fouls_avg, by='Group.1',all = T)
names(laligatwo_away_foulsdata)[names(laligatwo_away_foulsdata) == "x.x"] <- "TAfouls"
names(laligatwo_away_foulsdata)[names(laligatwo_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
laligatwo_fouls <- merge(laligatwo_home_foulsdata,laligatwo_away_foulsdata,by='Group.1',all = T)
laligatwo_fouls$TotalFouls <- laligatwo_fouls$THfouls + laligatwo_fouls$TAfouls

#yellow cards
laligatwo_home_hyc <- aggregate(LALIGATWO$HY, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_ayc <- aggregate(LALIGATWO$AY, by = list(LALIGATWO$AwayTeam), FUN = sum)
laligatwo_tyc <- merge(laligatwo_home_hyc,laligatwo_away_ayc, by='Group.1',all = T)
names(laligatwo_tyc)[names(laligatwo_tyc) == "x.x"] <- "hyc"
names(laligatwo_tyc)[names(laligatwo_tyc) == "x.y"] <- "ayc"
laligatwo_tyc$TotalYellows <- laligatwo_tyc$hyc + laligatwo_tyc$ayc

#merge fouls for and yellow cards
laligatwo_fouls_conversion <- merge(laligatwo_tyc,laligatwo_fouls,by='Group.1',all = T)
laligatwo_fouls_conversion$YcPerfoul <- round((laligatwo_fouls_conversion$TotalYellows/laligatwo_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
LALIGATWO$Div <- "LALIGATWO"
##
###################################################################################################################################################
#poisson cards
laligatwo_GP <- nrow(LALIGATWO)
#Calculate total home goals for each division
laligatwo_T_HY <- sum(laligatwo_home_hyc$x)
#calculate average home goal
laligatwo_avg_HY <- round(laligatwo_T_HY /laligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laligatwo_T_AY <- sum(laligatwo_away_ayc$x)
#calculate average away goal
laligatwo_avg_AY <- round(laligatwo_T_AY /laligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laligatwo_home_yas <- round(((laligatwo_home_hyc$x/laligatwo_home_games))/laligatwo_avg_HY, digits = 4)
#calculate away attack strength
laligatwo_away_yas <- round(((laligatwo_away_ayc$x/laligatwo_away_games))/laligatwo_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
laligatwo_avg_HYC <- round(laligatwo_T_AY /laligatwo_GP, digits = 4)
#avg away concede
laligatwo_avg_AYC <- round(laligatwo_T_HY /laligatwo_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
laligatwo_home_ycc <- aggregate(LALIGATWO$AY, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_ycc <- aggregate(LALIGATWO$HY, by = list(LALIGATWO$AwayTeam), FUN = sum)
#home defense strength
laligatwo_home_yds <- round(((laligatwo_home_ycc$x/laligatwo_home_games))/laligatwo_avg_HYC, digits = 4)
#away defense strength
laligatwo_away_yds <- round(((laligatwo_away_ycc$x/laligatwo_away_games))/laligatwo_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_home_poisson_yc <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_HY,laligatwo_home_yas,laligatwo_home_yds)
#away poisson data
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_away_poisson_yc <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_AY,laligatwo_away_yas,laligatwo_away_yds)
###
HomeTeam_laligatwo_yc <- rep(laligatwo_teams, each = length(laligatwo_teams))
AwayTeam_laligatwo_yc <- rep(laligatwo_teams, length(laligatwo_teams))
LALIGATWO_fixtures_yc <- cbind(HomeTeam_laligatwo_yc,AwayTeam_laligatwo_yc)
LALIGATWO_fixtures_yc <- as.data.frame(LALIGATWO_fixtures_yc)
LALIGATWO_fixtures_yc <- LALIGATWO_fixtures_yc[!LALIGATWO_fixtures_yc$HomeTeam_laligatwo_yc == LALIGATWO_fixtures_yc$AwayTeam_laligatwo_yc,]
rownames(LALIGATWO_fixtures_yc) <- NULL
LALIGATWO_fixtures_yc$Div <- "LALIGATWO"
LALIGATWO_fixtures_yc <- LALIGATWO_fixtures_yc[,c(3,1,2)]

LALIGATWO_fixtures_yc$avg_HY_laligatwo <- laligatwo_avg_HY

LALIGATWO_fixtures_yc$laligatwo_homeyas <- rep(laligatwo_home_yas,each = length(laligatwo_teams)-1)

laligatwo_awayyds_lookup <- cbind(laligatwo_teams,laligatwo_away_yds)

laligatwo_awayyds_lookup <- as.data.frame(laligatwo_awayyds_lookup)

colnames(laligatwo_awayyds_lookup) <- c("AwayTeam_laligatwo_yc","laligatwo_awayyds")


require('RH2')
LALIGATWO_fixtures_yc$laligatwo_awayyds <- sqldf("SELECT laligatwo_awayyds_lookup.laligatwo_awayyds FROM laligatwo_awayyds_lookup INNER JOIN LALIGATWO_fixtures_yc ON laligatwo_awayyds_lookup.AwayTeam_laligatwo_yc = LALIGATWO_fixtures_yc.AwayTeam_laligatwo_yc")

LALIGATWO_fixtures_yc$avg_AY_laligatwo <- laligatwo_avg_AY

laligatwo_awayyas_lookup <- cbind(laligatwo_teams,laligatwo_away_yas)

laligatwo_awayyas_lookup <- as.data.frame(laligatwo_awayyas_lookup)

colnames(laligatwo_awayyas_lookup) <- c("AwayTeam_laligatwo_yc","laligatwo_awayyas")

LALIGATWO_fixtures_yc$laligatwo_awayyas <- sqldf("SELECT laligatwo_awayyas_lookup.laligatwo_awayyas FROM laligatwo_awayyas_lookup INNER JOIN LALIGATWO_fixtures_yc ON laligatwo_awayyas_lookup.AwayTeam_laligatwo_yc = LALIGATWO_fixtures_yc.AwayTeam_laligatwo_yc")

LALIGATWO_fixtures_yc$laligatwo_homeyds <- rep(laligatwo_home_yds,each = length(laligatwo_teams)-1)

LALIGATWO_fixtures_yc$laligatwo_awayyds <- as.numeric(unlist(LALIGATWO_fixtures_yc$laligatwo_awayyds))
#xGH
LALIGATWO_fixtures_yc$laligatwo_xHYC <- LALIGATWO_fixtures_yc$avg_HY_laligatwo * LALIGATWO_fixtures_yc$laligatwo_homeyas * LALIGATWO_fixtures_yc$laligatwo_awayyds
#xGA

LALIGATWO_fixtures_yc$laligatwo_awayyas <- as.numeric(unlist(LALIGATWO_fixtures_yc$laligatwo_awayyas))

LALIGATWO_fixtures_yc$laligatwo_xAYC <- LALIGATWO_fixtures_yc$avg_AY_laligatwo * LALIGATWO_fixtures_yc$laligatwo_awayyas * LALIGATWO_fixtures_yc$laligatwo_homeyds

LALIGATWO_fixtures_yc$laligatwo_0_0 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_0 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_0_1 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_1 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_0 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_0_2 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_2 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_1 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_2 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_3 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_0 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_1 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_2 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_0_3 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_3 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_3 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_4 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_0 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_1 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_2 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_3 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_0_4 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_4 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_4 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_4 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_5 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_0 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_1 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_2 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_3 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_4 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_0_5 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_5 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_5 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_5 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_5 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_6 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_0 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_1 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_2 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_3 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_4 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_6_5 <- round(stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_0_6 <- round(stats::dpois(0,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_1_6 <- round(stats::dpois(1,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_2_6 <- round(stats::dpois(2,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_3_6 <- round(stats::dpois(3,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_4_6 <- round(stats::dpois(4,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
LALIGATWO_fixtures_yc$laligatwo_5_6 <- round(stats::dpois(5,LALIGATWO_fixtures_yc$laligatwo_xHYC) * stats::dpois(6,LALIGATWO_fixtures_yc$laligatwo_xAYC), digits = 4)
#Home win
LALIGATWO_fixtures_yc$laligatwo_H <- (
  LALIGATWO_fixtures_yc$laligatwo_1_0 + LALIGATWO_fixtures_yc$laligatwo_2_0 + LALIGATWO_fixtures_yc$laligatwo_2_1 + LALIGATWO_fixtures_yc$laligatwo_3_0 + LALIGATWO_fixtures_yc$laligatwo_3_1 +
    LALIGATWO_fixtures_yc$laligatwo_3_2 + LALIGATWO_fixtures_yc$laligatwo_4_0 + LALIGATWO_fixtures_yc$laligatwo_4_1 + LALIGATWO_fixtures_yc$laligatwo_4_2 + LALIGATWO_fixtures_yc$laligatwo_4_3 +
    LALIGATWO_fixtures_yc$laligatwo_5_0 + LALIGATWO_fixtures_yc$laligatwo_5_1 + LALIGATWO_fixtures_yc$laligatwo_5_2 + LALIGATWO_fixtures_yc$laligatwo_5_3 + LALIGATWO_fixtures_yc$laligatwo_5_4 +
    LALIGATWO_fixtures_yc$laligatwo_6_0 + LALIGATWO_fixtures_yc$laligatwo_6_1 + LALIGATWO_fixtures_yc$laligatwo_6_2 + LALIGATWO_fixtures_yc$laligatwo_6_3 + LALIGATWO_fixtures_yc$laligatwo_6_4 +
    LALIGATWO_fixtures_yc$laligatwo_6_5
)

LALIGATWO_fixtures_yc$laligatwo_H <- percent(LALIGATWO_fixtures_yc$laligatwo_H, accuracy = 0.1)

#Draw
LALIGATWO_fixtures_yc$laligatwo_D <- (

  LALIGATWO_fixtures_yc$laligatwo_0_0 + LALIGATWO_fixtures_yc$laligatwo_1_1 + LALIGATWO_fixtures_yc$laligatwo_2_2 + LALIGATWO_fixtures_yc$laligatwo_3_3 + LALIGATWO_fixtures_yc$laligatwo_4_4 +
    LALIGATWO_fixtures_yc$laligatwo_5_5 + LALIGATWO_fixtures_yc$laligatwo_6_6
)

LALIGATWO_fixtures_yc$laligatwo_D <- percent(LALIGATWO_fixtures_yc$laligatwo_D, accuracy = 0.1)

#Away

LALIGATWO_fixtures_yc$laligatwo_A <- (
  LALIGATWO_fixtures_yc$laligatwo_0_1 + LALIGATWO_fixtures_yc$laligatwo_0_2 + LALIGATWO_fixtures_yc$laligatwo_1_2 + LALIGATWO_fixtures_yc$laligatwo_0_3 + LALIGATWO_fixtures_yc$laligatwo_1_3 +
    LALIGATWO_fixtures_yc$laligatwo_2_3 + LALIGATWO_fixtures_yc$laligatwo_0_4 + LALIGATWO_fixtures_yc$laligatwo_1_4 + LALIGATWO_fixtures_yc$laligatwo_2_4 + LALIGATWO_fixtures_yc$laligatwo_3_4 +
    LALIGATWO_fixtures_yc$laligatwo_0_5 + LALIGATWO_fixtures_yc$laligatwo_1_5 + LALIGATWO_fixtures_yc$laligatwo_2_5 + LALIGATWO_fixtures_yc$laligatwo_3_5 + LALIGATWO_fixtures_yc$laligatwo_4_5 +
    LALIGATWO_fixtures_yc$laligatwo_0_6 + LALIGATWO_fixtures_yc$laligatwo_1_6 + LALIGATWO_fixtures_yc$laligatwo_2_6 + LALIGATWO_fixtures_yc$laligatwo_3_6 + LALIGATWO_fixtures_yc$laligatwo_4_6 +
    LALIGATWO_fixtures_yc$laligatwo_5_6
)

LALIGATWO_fixtures_yc$laligatwo_A <- percent(LALIGATWO_fixtures_yc$laligatwo_A, accuracy = 0.1)

#ov25
LALIGATWO_fixtures_yc$laligatwo_ov25 <- (
  LALIGATWO_fixtures_yc$laligatwo_2_1 + LALIGATWO_fixtures_yc$laligatwo_1_2 + LALIGATWO_fixtures_yc$laligatwo_2_2 + LALIGATWO_fixtures_yc$laligatwo_3_0 + LALIGATWO_fixtures_yc$laligatwo_3_1 +
    LALIGATWO_fixtures_yc$laligatwo_3_2 + LALIGATWO_fixtures_yc$laligatwo_0_3 + LALIGATWO_fixtures_yc$laligatwo_1_3 + LALIGATWO_fixtures_yc$laligatwo_2_3 + LALIGATWO_fixtures_yc$laligatwo_3_3 +
    LALIGATWO_fixtures_yc$laligatwo_4_0 + LALIGATWO_fixtures_yc$laligatwo_4_1 + LALIGATWO_fixtures_yc$laligatwo_4_2 + LALIGATWO_fixtures_yc$laligatwo_4_3 + LALIGATWO_fixtures_yc$laligatwo_0_4 +
    LALIGATWO_fixtures_yc$laligatwo_1_4 + LALIGATWO_fixtures_yc$laligatwo_2_4 + LALIGATWO_fixtures_yc$laligatwo_3_4 + LALIGATWO_fixtures_yc$laligatwo_4_4 + LALIGATWO_fixtures_yc$laligatwo_5_0 +
    LALIGATWO_fixtures_yc$laligatwo_5_1 + LALIGATWO_fixtures_yc$laligatwo_5_2 + LALIGATWO_fixtures_yc$laligatwo_5_3 + LALIGATWO_fixtures_yc$laligatwo_5_4 + LALIGATWO_fixtures_yc$laligatwo_0_5 +
    LALIGATWO_fixtures_yc$laligatwo_1_5 + LALIGATWO_fixtures_yc$laligatwo_2_5 + LALIGATWO_fixtures_yc$laligatwo_3_5 + LALIGATWO_fixtures_yc$laligatwo_4_5 + LALIGATWO_fixtures_yc$laligatwo_5_5 +
    LALIGATWO_fixtures_yc$laligatwo_6_0 + LALIGATWO_fixtures_yc$laligatwo_6_1 + LALIGATWO_fixtures_yc$laligatwo_6_2 + LALIGATWO_fixtures_yc$laligatwo_6_3 + LALIGATWO_fixtures_yc$laligatwo_6_4 +
    LALIGATWO_fixtures_yc$laligatwo_6_5 + LALIGATWO_fixtures_yc$laligatwo_0_6 + LALIGATWO_fixtures_yc$laligatwo_1_6 + LALIGATWO_fixtures_yc$laligatwo_2_6 + LALIGATWO_fixtures_yc$laligatwo_3_6 +
    LALIGATWO_fixtures_yc$laligatwo_4_6 + LALIGATWO_fixtures_yc$laligatwo_5_6 + LALIGATWO_fixtures_yc$laligatwo_6_6
)
#un25
LALIGATWO_fixtures_yc$laligatwo_un25 <- (
  LALIGATWO_fixtures_yc$laligatwo_0_0 + LALIGATWO_fixtures_yc$laligatwo_1_0 + LALIGATWO_fixtures_yc$laligatwo_0_1 + LALIGATWO_fixtures_yc$laligatwo_1_1 + LALIGATWO_fixtures_yc$laligatwo_2_0 + LALIGATWO_fixtures_yc$laligatwo_0_2
)
#odds
LALIGATWO_fixtures_yc$laligatwo_ov25_odds <- round((1/LALIGATWO_fixtures_yc$laligatwo_ov25),digits = 2)
LALIGATWO_fixtures_yc$laligatwo_un25_odds <- round((1/LALIGATWO_fixtures_yc$laligatwo_un25),digits = 2)

LALIGATWO_fixtures_yc$laligatwo_ov25_odds
LALIGATWO_fixtures_yc$laligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGATWO_fixtures_yc$laligatwo_ov25 <- percent(LALIGATWO_fixtures_yc$laligatwo_ov25, accuracy = 0.1)

LALIGATWO_fixtures_yc$laligatwo_un25 <- percent(LALIGATWO_fixtures_yc$laligatwo_un25, accuracy = 0.1)
LALIGATWO_fixtures_yc$laligatwo_pscore <- paste(round(LALIGATWO_fixtures_yc$laligatwo_xHYC,digits = 0),round(LALIGATWO_fixtures_yc$laligatwo_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
laligatwo_GP <- nrow(LALIGATWO)
#Calculate total home corners for each division
laligatwo_home_corners <- aggregate(LALIGATWO$HCO, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_corners <- aggregate(LALIGATWO$ACO, by = list(LALIGATWO$AwayTeam), FUN = sum)
###############################################################################
laligatwo_T_HCO <- sum(laligatwo_home_corners$x)
#calculate average home corners
laligatwo_avg_HCO <- round(laligatwo_T_HCO /laligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laligatwo_T_ACO <- sum(laligatwo_away_corners$x)
#calculate average away goal
laligatwo_avg_ACO <- round(laligatwo_T_ACO /laligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laligatwo_home_coas <- round(((laligatwo_home_corners$x/laligatwo_home_games))/laligatwo_avg_HCO, digits = 4)
#calculate away attack strength
laligatwo_away_coas <- round(((laligatwo_away_corners$x/laligatwo_away_games))/laligatwo_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
laligatwo_avg_HCOC <- round(laligatwo_T_ACO /laligatwo_GP, digits = 4)
#avg away concede
laligatwo_avg_ACOC <- round(laligatwo_T_HCO /laligatwo_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
laligatwo_home_coc <- aggregate(LALIGATWO$ACO, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_coc <- aggregate(LALIGATWO$HCO, by = list(LALIGATWO$AwayTeam), FUN = sum)
#home defense strength
laligatwo_home_cods <- round(((laligatwo_home_coc$x/laligatwo_home_games))/laligatwo_avg_HCOC, digits = 4)
#away defense strength
laligatwo_away_cods <- round(((laligatwo_away_coc$x/laligatwo_away_games))/laligatwo_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_home_poisson_corners <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_HCO,laligatwo_home_coas,laligatwo_home_cods)
#################################################################################
#away poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_away_poisson_corners <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_ACO,laligatwo_away_coas,laligatwo_away_cods)

#LALIGATWO
HomeTeam_laligatwo_co <- rep(laligatwo_teams, each = length(laligatwo_teams))
AwayTeam_laligatwo_co <- rep(laligatwo_teams, length(laligatwo_teams))
LALIGATWO_fixtures_co <- cbind(HomeTeam_laligatwo_co,AwayTeam_laligatwo_co)
LALIGATWO_fixtures_co <- as.data.frame(LALIGATWO_fixtures_co)
LALIGATWO_fixtures_co <- LALIGATWO_fixtures_co[!LALIGATWO_fixtures_co$HomeTeam_laligatwo_co == LALIGATWO_fixtures_co$AwayTeam_laligatwo_co,]
rownames(LALIGATWO_fixtures_co) <- NULL
LALIGATWO_fixtures_co$Div <- "LALIGATWO"
LALIGATWO_fixtures_co <- LALIGATWO_fixtures_co[,c(3,1,2)]

LALIGATWO_fixtures_co$avg_HCO_laligatwo <- laligatwo_avg_HCO

LALIGATWO_fixtures_co$laligatwo_homecoas <- rep(laligatwo_home_coas,each = length(laligatwo_teams)-1)

laligatwo_awaycods_lookup <- cbind(laligatwo_teams,laligatwo_away_cods)

laligatwo_awaycods_lookup <- as.data.frame(laligatwo_awaycods_lookup)

colnames(laligatwo_awaycods_lookup) <- c("AwayTeam_laligatwo_co","laligatwo_awaycods")


require('RH2')
LALIGATWO_fixtures_co$laligatwo_awaycods <- sqldf("SELECT laligatwo_awaycods_lookup.laligatwo_awaycods FROM laligatwo_awaycods_lookup INNER JOIN LALIGATWO_fixtures_co ON laligatwo_awaycods_lookup.AwayTeam_laligatwo_co = LALIGATWO_fixtures_co.AwayTeam_laligatwo_co")

LALIGATWO_fixtures_co$avg_ACO_laligatwo <- laligatwo_avg_ACO

laligatwo_awaycoas_lookup <- cbind(laligatwo_teams,laligatwo_away_coas)

laligatwo_awaycoas_lookup <- as.data.frame(laligatwo_awaycoas_lookup)

colnames(laligatwo_awaycoas_lookup) <- c("AwayTeam_laligatwo_co","laligatwo_awaycoas")

LALIGATWO_fixtures_co$laligatwo_awaycoas <- sqldf("SELECT laligatwo_awaycoas_lookup.laligatwo_awaycoas FROM laligatwo_awaycoas_lookup INNER JOIN LALIGATWO_fixtures_co ON laligatwo_awaycoas_lookup.AwayTeam_laligatwo_co = LALIGATWO_fixtures_co.AwayTeam_laligatwo_co")

LALIGATWO_fixtures_co$laligatwo_homecods <- rep(laligatwo_home_cods,each = length(laligatwo_teams)-1)

LALIGATWO_fixtures_co$laligatwo_awaycods <- as.numeric(unlist(LALIGATWO_fixtures_co$laligatwo_awaycods))
#xGH
LALIGATWO_fixtures_co$laligatwo_xHCOC <- LALIGATWO_fixtures_co$avg_HCO_laligatwo * LALIGATWO_fixtures_co$laligatwo_homecoas * LALIGATWO_fixtures_co$laligatwo_awaycods
#xGA

LALIGATWO_fixtures_co$laligatwo_awaycoas <- as.numeric(unlist(LALIGATWO_fixtures_co$laligatwo_awaycoas))

LALIGATWO_fixtures_co$laligatwo_xACOC <- LALIGATWO_fixtures_co$avg_ACO_laligatwo * LALIGATWO_fixtures_co$laligatwo_awaycoas * LALIGATWO_fixtures_co$laligatwo_homecods

LALIGATWO_fixtures_co$laligatwo_0_0 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_0 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_0_1 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_1 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_0 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_0_2 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_2 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_1 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_2 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_3 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_0 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_1 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_2 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_0_3 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_3 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_3 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_4 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_0 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_1 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_2 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_3 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_0_4 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_4 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_4 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_4 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_5 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_0 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_1 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_2 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_3 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_4 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_0_5 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_5 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_5 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_5 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_5 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_6 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_0 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_1 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_2 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_3 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_4 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_6_5 <- round(stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_0_6 <- round(stats::dpois(0,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_1_6 <- round(stats::dpois(1,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_2_6 <- round(stats::dpois(2,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_3_6 <- round(stats::dpois(3,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_4_6 <- round(stats::dpois(4,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
LALIGATWO_fixtures_co$laligatwo_5_6 <- round(stats::dpois(5,LALIGATWO_fixtures_co$laligatwo_xHCOC) * stats::dpois(6,LALIGATWO_fixtures_co$laligatwo_xACOC), digits = 4)
#Home win
LALIGATWO_fixtures_co$laligatwo_H <- (
  LALIGATWO_fixtures_co$laligatwo_1_0 + LALIGATWO_fixtures_co$laligatwo_2_0 + LALIGATWO_fixtures_co$laligatwo_2_1 + LALIGATWO_fixtures_co$laligatwo_3_0 + LALIGATWO_fixtures_co$laligatwo_3_1 +
    LALIGATWO_fixtures_co$laligatwo_3_2 + LALIGATWO_fixtures_co$laligatwo_4_0 + LALIGATWO_fixtures_co$laligatwo_4_1 + LALIGATWO_fixtures_co$laligatwo_4_2 + LALIGATWO_fixtures_co$laligatwo_4_3 +
    LALIGATWO_fixtures_co$laligatwo_5_0 + LALIGATWO_fixtures_co$laligatwo_5_1 + LALIGATWO_fixtures_co$laligatwo_5_2 + LALIGATWO_fixtures_co$laligatwo_5_3 + LALIGATWO_fixtures_co$laligatwo_5_4 +
    LALIGATWO_fixtures_co$laligatwo_6_0 + LALIGATWO_fixtures_co$laligatwo_6_1 + LALIGATWO_fixtures_co$laligatwo_6_2 + LALIGATWO_fixtures_co$laligatwo_6_3 + LALIGATWO_fixtures_co$laligatwo_6_4 +
    LALIGATWO_fixtures_co$laligatwo_6_5
)

LALIGATWO_fixtures_co$laligatwo_H <- percent(LALIGATWO_fixtures_co$laligatwo_H, accuracy = 0.1)

#Draw
LALIGATWO_fixtures_co$laligatwo_D <- (

  LALIGATWO_fixtures_co$laligatwo_0_0 + LALIGATWO_fixtures_co$laligatwo_1_1 + LALIGATWO_fixtures_co$laligatwo_2_2 + LALIGATWO_fixtures_co$laligatwo_3_3 + LALIGATWO_fixtures_co$laligatwo_4_4 +
    LALIGATWO_fixtures_co$laligatwo_5_5 + LALIGATWO_fixtures_co$laligatwo_6_6
)

LALIGATWO_fixtures_co$laligatwo_D <- percent(LALIGATWO_fixtures_co$laligatwo_D, accuracy = 0.1)

#Away

LALIGATWO_fixtures_co$laligatwo_A <- (
  LALIGATWO_fixtures_co$laligatwo_0_1 + LALIGATWO_fixtures_co$laligatwo_0_2 + LALIGATWO_fixtures_co$laligatwo_1_2 + LALIGATWO_fixtures_co$laligatwo_0_3 + LALIGATWO_fixtures_co$laligatwo_1_3 +
    LALIGATWO_fixtures_co$laligatwo_2_3 + LALIGATWO_fixtures_co$laligatwo_0_4 + LALIGATWO_fixtures_co$laligatwo_1_4 + LALIGATWO_fixtures_co$laligatwo_2_4 + LALIGATWO_fixtures_co$laligatwo_3_4 +
    LALIGATWO_fixtures_co$laligatwo_0_5 + LALIGATWO_fixtures_co$laligatwo_1_5 + LALIGATWO_fixtures_co$laligatwo_2_5 + LALIGATWO_fixtures_co$laligatwo_3_5 + LALIGATWO_fixtures_co$laligatwo_4_5 +
    LALIGATWO_fixtures_co$laligatwo_0_6 + LALIGATWO_fixtures_co$laligatwo_1_6 + LALIGATWO_fixtures_co$laligatwo_2_6 + LALIGATWO_fixtures_co$laligatwo_3_6 + LALIGATWO_fixtures_co$laligatwo_4_6 +
    LALIGATWO_fixtures_co$laligatwo_5_6
)

LALIGATWO_fixtures_co$laligatwo_A <- percent(LALIGATWO_fixtures_co$laligatwo_A, accuracy = 0.1)

#ov25
LALIGATWO_fixtures_co$laligatwo_ov25 <- (
  LALIGATWO_fixtures_co$laligatwo_2_1 + LALIGATWO_fixtures_co$laligatwo_1_2 + LALIGATWO_fixtures_co$laligatwo_2_2 + LALIGATWO_fixtures_co$laligatwo_3_0 + LALIGATWO_fixtures_co$laligatwo_3_1 +
    LALIGATWO_fixtures_co$laligatwo_3_2 + LALIGATWO_fixtures_co$laligatwo_0_3 + LALIGATWO_fixtures_co$laligatwo_1_3 + LALIGATWO_fixtures_co$laligatwo_2_3 + LALIGATWO_fixtures_co$laligatwo_3_3 +
    LALIGATWO_fixtures_co$laligatwo_4_0 + LALIGATWO_fixtures_co$laligatwo_4_1 + LALIGATWO_fixtures_co$laligatwo_4_2 + LALIGATWO_fixtures_co$laligatwo_4_3 + LALIGATWO_fixtures_co$laligatwo_0_4 +
    LALIGATWO_fixtures_co$laligatwo_1_4 + LALIGATWO_fixtures_co$laligatwo_2_4 + LALIGATWO_fixtures_co$laligatwo_3_4 + LALIGATWO_fixtures_co$laligatwo_4_4 + LALIGATWO_fixtures_co$laligatwo_5_0 +
    LALIGATWO_fixtures_co$laligatwo_5_1 + LALIGATWO_fixtures_co$laligatwo_5_2 + LALIGATWO_fixtures_co$laligatwo_5_3 + LALIGATWO_fixtures_co$laligatwo_5_4 + LALIGATWO_fixtures_co$laligatwo_0_5 +
    LALIGATWO_fixtures_co$laligatwo_1_5 + LALIGATWO_fixtures_co$laligatwo_2_5 + LALIGATWO_fixtures_co$laligatwo_3_5 + LALIGATWO_fixtures_co$laligatwo_4_5 + LALIGATWO_fixtures_co$laligatwo_5_5 +
    LALIGATWO_fixtures_co$laligatwo_6_0 + LALIGATWO_fixtures_co$laligatwo_6_1 + LALIGATWO_fixtures_co$laligatwo_6_2 + LALIGATWO_fixtures_co$laligatwo_6_3 + LALIGATWO_fixtures_co$laligatwo_6_4 +
    LALIGATWO_fixtures_co$laligatwo_6_5 + LALIGATWO_fixtures_co$laligatwo_0_6 + LALIGATWO_fixtures_co$laligatwo_1_6 + LALIGATWO_fixtures_co$laligatwo_2_6 + LALIGATWO_fixtures_co$laligatwo_3_6 +
    LALIGATWO_fixtures_co$laligatwo_4_6 + LALIGATWO_fixtures_co$laligatwo_5_6 + LALIGATWO_fixtures_co$laligatwo_6_6
)
#un25
LALIGATWO_fixtures_co$laligatwo_un25 <- (
  LALIGATWO_fixtures_co$laligatwo_0_0 + LALIGATWO_fixtures_co$laligatwo_1_0 + LALIGATWO_fixtures_co$laligatwo_0_1 + LALIGATWO_fixtures_co$laligatwo_1_1 + LALIGATWO_fixtures_co$laligatwo_2_0 + LALIGATWO_fixtures_co$laligatwo_0_2
)
#odds
LALIGATWO_fixtures_co$laligatwo_ov25_odds <- round((1/LALIGATWO_fixtures_co$laligatwo_ov25),digits = 2)
LALIGATWO_fixtures_co$laligatwo_un25_odds <- round((1/LALIGATWO_fixtures_co$laligatwo_un25),digits = 2)

LALIGATWO_fixtures_co$laligatwo_ov25_odds
LALIGATWO_fixtures_co$laligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGATWO_fixtures_co$laligatwo_ov25 <- percent(LALIGATWO_fixtures_co$laligatwo_ov25, accuracy = 0.1)

LALIGATWO_fixtures_co$laligatwo_un25 <- percent(LALIGATWO_fixtures_co$laligatwo_un25, accuracy = 0.1)
LALIGATWO_fixtures_co$laligatwo_pscore <- paste(round(LALIGATWO_fixtures_co$laligatwo_xHCOC,digits = 0),round(LALIGATWO_fixtures_co$laligatwo_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
laligatwo_GP <- nrow(LALIGATWO)
#Calculate total home goals for each division
laligatwo_T_HF <- sum(laligatwo_home_fouls$x)
#calculate average home goal
laligatwo_avg_HF <- round(laligatwo_T_HF /laligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laligatwo_T_AF <- sum(laligatwo_away_fouls$x)
#calculate average away goal
laligatwo_avg_AF <- round(laligatwo_T_AF /laligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laligatwo_home_fas <- round(((laligatwo_home_fouls$x/laligatwo_home_games))/laligatwo_avg_HF, digits = 4)
#calculate away attack strength
laligatwo_away_fas <- round(((laligatwo_away_fouls$x/laligatwo_away_games))/laligatwo_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
laligatwo_avg_HFC <- round(laligatwo_T_AF /laligatwo_GP, digits = 4)
#avg away concede
laligatwo_avg_AFC <- round(laligatwo_T_HF /laligatwo_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
laligatwo_home_fcc <- aggregate(LALIGATWO$AF, by = list(LALIGATWO$HomeTeam), FUN = sum)
laligatwo_away_fcc <- aggregate(LALIGATWO$HF, by = list(LALIGATWO$AwayTeam), FUN = sum)

#home defense strength
laligatwo_home_fds <- round(((laligatwo_home_fcc$x/laligatwo_home_games))/laligatwo_avg_HFC, digits = 4)

#away defense strength
laligatwo_away_fds <- round(((laligatwo_away_fcc$x/laligatwo_away_games))/laligatwo_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_home_poisson_fo <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_HF,laligatwo_home_fas,laligatwo_home_fds)

#################################################################################
#away poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_away_poisson_fo <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_AF,laligatwo_away_fas,laligatwo_away_fds)

#LALIGATWO
HomeTeam_laligatwo_fo <- rep(laligatwo_teams, each = length(laligatwo_teams))
AwayTeam_laligatwo_fo <- rep(laligatwo_teams, length(laligatwo_teams))
LALIGATWO_fixtures_fo <- cbind(HomeTeam_laligatwo_fo,AwayTeam_laligatwo_fo)
LALIGATWO_fixtures_fo <- as.data.frame(LALIGATWO_fixtures_fo)
LALIGATWO_fixtures_fo <- LALIGATWO_fixtures_fo[!LALIGATWO_fixtures_fo$HomeTeam_laligatwo_fo == LALIGATWO_fixtures_fo$AwayTeam_laligatwo_fo,]
rownames(LALIGATWO_fixtures_fo) <- NULL
LALIGATWO_fixtures_fo$Div <- "LALIGATWO"
LALIGATWO_fixtures_fo <- LALIGATWO_fixtures_fo[,c(3,1,2)]

LALIGATWO_fixtures_fo$avg_HF_laligatwo <- laligatwo_avg_HF

LALIGATWO_fixtures_fo$laligatwo_homefas <- rep(laligatwo_home_fas,each = length(laligatwo_teams)-1)

laligatwo_awayfds_lookup <- cbind(laligatwo_teams,laligatwo_away_fds)

laligatwo_awayfds_lookup <- as.data.frame(laligatwo_awayfds_lookup)

colnames(laligatwo_awayfds_lookup) <- c("AwayTeam_laligatwo_fo","laligatwo_awayfds")


require('RH2')
LALIGATWO_fixtures_fo$laligatwo_awayfds <- sqldf("SELECT laligatwo_awayfds_lookup.laligatwo_awayfds FROM laligatwo_awayfds_lookup INNER JOIN LALIGATWO_fixtures_fo ON laligatwo_awayfds_lookup.AwayTeam_laligatwo_fo = LALIGATWO_fixtures_fo.AwayTeam_laligatwo_fo")

LALIGATWO_fixtures_fo$avg_AF_laligatwo <- laligatwo_avg_AF

laligatwo_awayfas_lookup <- cbind(laligatwo_teams,laligatwo_away_fas)

laligatwo_awayfas_lookup <- as.data.frame(laligatwo_awayfas_lookup)

colnames(laligatwo_awayfas_lookup) <- c("AwayTeam_laligatwo_fo","laligatwo_awayfas")

LALIGATWO_fixtures_fo$laligatwo_awayfas <- sqldf("SELECT laligatwo_awayfas_lookup.laligatwo_awayfas FROM laligatwo_awayfas_lookup INNER JOIN LALIGATWO_fixtures_fo ON laligatwo_awayfas_lookup.AwayTeam_laligatwo_fo = LALIGATWO_fixtures_fo.AwayTeam_laligatwo_fo")

LALIGATWO_fixtures_fo$laligatwo_homefds <- rep(laligatwo_home_fds,each = length(laligatwo_teams)-1)

LALIGATWO_fixtures_fo$laligatwo_awayfds <- as.numeric(unlist(LALIGATWO_fixtures_fo$laligatwo_awayfds))
#xGH
LALIGATWO_fixtures_fo$laligatwo_xHF <- LALIGATWO_fixtures_fo$avg_HF_laligatwo * LALIGATWO_fixtures_fo$laligatwo_homefas * LALIGATWO_fixtures_fo$laligatwo_awayfds
#xGA

LALIGATWO_fixtures_fo$laligatwo_awayfas <- as.numeric(unlist(LALIGATWO_fixtures_fo$laligatwo_awayfas))

LALIGATWO_fixtures_fo$laligatwo_xAF <- LALIGATWO_fixtures_fo$avg_AF_laligatwo * LALIGATWO_fixtures_fo$laligatwo_awayfas * LALIGATWO_fixtures_fo$laligatwo_homefds

LALIGATWO_fixtures_fo$laligatwo_0_0 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_0 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_0_1 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_1 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_0 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_0_2 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_2 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_1 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_2 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_3 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_0 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_1 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_2 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_0_3 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_3 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_3 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_4 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_0 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_1 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_2 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_3 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_0_4 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_4 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_4 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_4 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_5 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_0 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_1 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_2 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_3 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_4 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_0_5 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_5 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_5 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_5 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_5 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_6 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_0 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_1 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_2 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_3 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_4 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_6_5 <- round(stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_0_6 <- round(stats::dpois(0,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_1_6 <- round(stats::dpois(1,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_2_6 <- round(stats::dpois(2,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_3_6 <- round(stats::dpois(3,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_4_6 <- round(stats::dpois(4,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
LALIGATWO_fixtures_fo$laligatwo_5_6 <- round(stats::dpois(5,LALIGATWO_fixtures_fo$laligatwo_xHF) * stats::dpois(6,LALIGATWO_fixtures_fo$laligatwo_xAF), digits = 4)
#Home win
LALIGATWO_fixtures_fo$laligatwo_H <- (
  LALIGATWO_fixtures_fo$laligatwo_1_0 + LALIGATWO_fixtures_fo$laligatwo_2_0 + LALIGATWO_fixtures_fo$laligatwo_2_1 + LALIGATWO_fixtures_fo$laligatwo_3_0 + LALIGATWO_fixtures_fo$laligatwo_3_1 +
    LALIGATWO_fixtures_fo$laligatwo_3_2 + LALIGATWO_fixtures_fo$laligatwo_4_0 + LALIGATWO_fixtures_fo$laligatwo_4_1 + LALIGATWO_fixtures_fo$laligatwo_4_2 + LALIGATWO_fixtures_fo$laligatwo_4_3 +
    LALIGATWO_fixtures_fo$laligatwo_5_0 + LALIGATWO_fixtures_fo$laligatwo_5_1 + LALIGATWO_fixtures_fo$laligatwo_5_2 + LALIGATWO_fixtures_fo$laligatwo_5_3 + LALIGATWO_fixtures_fo$laligatwo_5_4 +
    LALIGATWO_fixtures_fo$laligatwo_6_0 + LALIGATWO_fixtures_fo$laligatwo_6_1 + LALIGATWO_fixtures_fo$laligatwo_6_2 + LALIGATWO_fixtures_fo$laligatwo_6_3 + LALIGATWO_fixtures_fo$laligatwo_6_4 +
    LALIGATWO_fixtures_fo$laligatwo_6_5
)

LALIGATWO_fixtures_fo$laligatwo_H <- percent(LALIGATWO_fixtures_fo$laligatwo_H, accuracy = 0.1)

#Draw
LALIGATWO_fixtures_fo$laligatwo_D <- (

  LALIGATWO_fixtures_fo$laligatwo_0_0 + LALIGATWO_fixtures_fo$laligatwo_1_1 + LALIGATWO_fixtures_fo$laligatwo_2_2 + LALIGATWO_fixtures_fo$laligatwo_3_3 + LALIGATWO_fixtures_fo$laligatwo_4_4 +
    LALIGATWO_fixtures_fo$laligatwo_5_5 + LALIGATWO_fixtures_fo$laligatwo_6_6
)

LALIGATWO_fixtures_fo$laligatwo_D <- percent(LALIGATWO_fixtures_fo$laligatwo_D, accuracy = 0.1)

#Away

LALIGATWO_fixtures_fo$laligatwo_A <- (
  LALIGATWO_fixtures_fo$laligatwo_0_1 + LALIGATWO_fixtures_fo$laligatwo_0_2 + LALIGATWO_fixtures_fo$laligatwo_1_2 + LALIGATWO_fixtures_fo$laligatwo_0_3 + LALIGATWO_fixtures_fo$laligatwo_1_3 +
    LALIGATWO_fixtures_fo$laligatwo_2_3 + LALIGATWO_fixtures_fo$laligatwo_0_4 + LALIGATWO_fixtures_fo$laligatwo_1_4 + LALIGATWO_fixtures_fo$laligatwo_2_4 + LALIGATWO_fixtures_fo$laligatwo_3_4 +
    LALIGATWO_fixtures_fo$laligatwo_0_5 + LALIGATWO_fixtures_fo$laligatwo_1_5 + LALIGATWO_fixtures_fo$laligatwo_2_5 + LALIGATWO_fixtures_fo$laligatwo_3_5 + LALIGATWO_fixtures_fo$laligatwo_4_5 +
    LALIGATWO_fixtures_fo$laligatwo_0_6 + LALIGATWO_fixtures_fo$laligatwo_1_6 + LALIGATWO_fixtures_fo$laligatwo_2_6 + LALIGATWO_fixtures_fo$laligatwo_3_6 + LALIGATWO_fixtures_fo$laligatwo_4_6 +
    LALIGATWO_fixtures_fo$laligatwo_5_6
)

LALIGATWO_fixtures_fo$laligatwo_A <- percent(LALIGATWO_fixtures_fo$laligatwo_A, accuracy = 0.1)

#ov25
LALIGATWO_fixtures_fo$laligatwo_ov25 <- (
  LALIGATWO_fixtures_fo$laligatwo_2_1 + LALIGATWO_fixtures_fo$laligatwo_1_2 + LALIGATWO_fixtures_fo$laligatwo_2_2 + LALIGATWO_fixtures_fo$laligatwo_3_0 + LALIGATWO_fixtures_fo$laligatwo_3_1 +
    LALIGATWO_fixtures_fo$laligatwo_3_2 + LALIGATWO_fixtures_fo$laligatwo_0_3 + LALIGATWO_fixtures_fo$laligatwo_1_3 + LALIGATWO_fixtures_fo$laligatwo_2_3 + LALIGATWO_fixtures_fo$laligatwo_3_3 +
    LALIGATWO_fixtures_fo$laligatwo_4_0 + LALIGATWO_fixtures_fo$laligatwo_4_1 + LALIGATWO_fixtures_fo$laligatwo_4_2 + LALIGATWO_fixtures_fo$laligatwo_4_3 + LALIGATWO_fixtures_fo$laligatwo_0_4 +
    LALIGATWO_fixtures_fo$laligatwo_1_4 + LALIGATWO_fixtures_fo$laligatwo_2_4 + LALIGATWO_fixtures_fo$laligatwo_3_4 + LALIGATWO_fixtures_fo$laligatwo_4_4 + LALIGATWO_fixtures_fo$laligatwo_5_0 +
    LALIGATWO_fixtures_fo$laligatwo_5_1 + LALIGATWO_fixtures_fo$laligatwo_5_2 + LALIGATWO_fixtures_fo$laligatwo_5_3 + LALIGATWO_fixtures_fo$laligatwo_5_4 + LALIGATWO_fixtures_fo$laligatwo_0_5 +
    LALIGATWO_fixtures_fo$laligatwo_1_5 + LALIGATWO_fixtures_fo$laligatwo_2_5 + LALIGATWO_fixtures_fo$laligatwo_3_5 + LALIGATWO_fixtures_fo$laligatwo_4_5 + LALIGATWO_fixtures_fo$laligatwo_5_5 +
    LALIGATWO_fixtures_fo$laligatwo_6_0 + LALIGATWO_fixtures_fo$laligatwo_6_1 + LALIGATWO_fixtures_fo$laligatwo_6_2 + LALIGATWO_fixtures_fo$laligatwo_6_3 + LALIGATWO_fixtures_fo$laligatwo_6_4 +
    LALIGATWO_fixtures_fo$laligatwo_6_5 + LALIGATWO_fixtures_fo$laligatwo_0_6 + LALIGATWO_fixtures_fo$laligatwo_1_6 + LALIGATWO_fixtures_fo$laligatwo_2_6 + LALIGATWO_fixtures_fo$laligatwo_3_6 +
    LALIGATWO_fixtures_fo$laligatwo_4_6 + LALIGATWO_fixtures_fo$laligatwo_5_6 + LALIGATWO_fixtures_fo$laligatwo_6_6
)
#un25
LALIGATWO_fixtures_fo$laligatwo_un25 <- (
  LALIGATWO_fixtures_fo$laligatwo_0_0 + LALIGATWO_fixtures_fo$laligatwo_1_0 + LALIGATWO_fixtures_fo$laligatwo_0_1 + LALIGATWO_fixtures_fo$laligatwo_1_1 + LALIGATWO_fixtures_fo$laligatwo_2_0 + LALIGATWO_fixtures_fo$laligatwo_0_2
)
#odds
LALIGATWO_fixtures_fo$laligatwo_ov25_odds <- round((1/LALIGATWO_fixtures_fo$laligatwo_ov25),digits = 2)
LALIGATWO_fixtures_fo$laligatwo_un25_odds <- round((1/LALIGATWO_fixtures_fo$laligatwo_un25),digits = 2)

LALIGATWO_fixtures_fo$laligatwo_ov25_odds
LALIGATWO_fixtures_fo$laligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGATWO_fixtures_fo$laligatwo_ov25 <- percent(LALIGATWO_fixtures_fo$laligatwo_ov25, accuracy = 0.1)

LALIGATWO_fixtures_fo$laligatwo_un25 <- percent(LALIGATWO_fixtures_fo$laligatwo_un25, accuracy = 0.1)
LALIGATWO_fixtures_fo$laligatwo_psfore <- paste(round(LALIGATWO_fixtures_fo$laligatwo_xHF,digits = 0),round(LALIGATWO_fixtures_fo$laligatwo_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
laligatwo_GP <- nrow(LALIGATWO)

#Calculate total home goals for each division
laligatwo_T_HST <- sum(laligatwo_home_hst$x)
#calculate average home goal

laligatwo_avg_HST <- round(laligatwo_T_HST /laligatwo_GP, digits = 4)

############################################################
#Calculate total away goals for each division
laligatwo_T_AST <- sum(laligatwo_away_ast$x)
#calculate average away goal
laligatwo_avg_AST <- round(laligatwo_T_AST /laligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laligatwo_home_sotas <- round(((laligatwo_home_hst$x/laligatwo_home_games))/laligatwo_avg_HST, digits = 4)
#calculate away attack strength
laligatwo_away_sotas <- round(((laligatwo_away_ast$x/laligatwo_away_games))/laligatwo_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
laligatwo_avg_HSC <- round(laligatwo_T_AST /laligatwo_GP, digits = 4)

#avg away concede
laligatwo_avg_ASC <- round(laligatwo_T_HST /laligatwo_GP, digits = 4)
#home defense strength
laligatwo_home_sods <- round(((laligatwo_home_hsc$x/laligatwo_home_games))/laligatwo_avg_HSC, digits = 4)

#away defense strength
laligatwo_away_sods <- round(((laligatwo_away_ast$x/laligatwo_away_games))/laligatwo_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_home_poisson_sot <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_HST,laligatwo_home_sotas,laligatwo_home_sods)

#################################################################################
#away poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_away_poisson_sot <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_AST,laligatwo_away_sotas,laligatwo_away_sods)

#LALIGATWO
HomeTeam_laligatwo_sot <- rep(laligatwo_teams, each = length(laligatwo_teams))
AwayTeam_laligatwo_sot <- rep(laligatwo_teams, length(laligatwo_teams))
LALIGATWO_fixtures_sot <- cbind(HomeTeam_laligatwo_sot,AwayTeam_laligatwo_sot)
LALIGATWO_fixtures_sot <- as.data.frame(LALIGATWO_fixtures_sot)
LALIGATWO_fixtures_sot <- LALIGATWO_fixtures_sot[!LALIGATWO_fixtures_sot$HomeTeam_laligatwo_sot == LALIGATWO_fixtures_sot$AwayTeam_laligatwo_sot,]
rownames(LALIGATWO_fixtures_sot) <- NULL
LALIGATWO_fixtures_sot$Div <- "LALIGATWO"
LALIGATWO_fixtures_sot <- LALIGATWO_fixtures_sot[,c(3,1,2)]

LALIGATWO_fixtures_sot$avg_HST_laligatwo <- laligatwo_avg_HST

LALIGATWO_fixtures_sot$laligatwo_homesotas <- rep(laligatwo_home_sotas,each = length(laligatwo_teams)-1)

laligatwo_awaysods_lookup <- cbind(laligatwo_teams,laligatwo_away_sods)

laligatwo_awaysods_lookup <- as.data.frame(laligatwo_awaysods_lookup)

colnames(laligatwo_awaysods_lookup) <- c("AwayTeam_laligatwo_sot","laligatwo_awaysods")


require('RH2')
LALIGATWO_fixtures_sot$laligatwo_awaysods <- sqldf("SELECT laligatwo_awaysods_lookup.laligatwo_awaysods FROM laligatwo_awaysods_lookup INNER JOIN LALIGATWO_fixtures_sot ON laligatwo_awaysods_lookup.AwayTeam_laligatwo_sot = LALIGATWO_fixtures_sot.AwayTeam_laligatwo_sot")

LALIGATWO_fixtures_sot$avg_AST_laligatwo <- laligatwo_avg_AST

laligatwo_awaysotas_lookup <- cbind(laligatwo_teams,laligatwo_away_sotas)

laligatwo_awaysotas_lookup <- as.data.frame(laligatwo_awaysotas_lookup)

colnames(laligatwo_awaysotas_lookup) <- c("AwayTeam_laligatwo_sot","laligatwo_awaysotas")

LALIGATWO_fixtures_sot$laligatwo_awaysotas <- sqldf("SELECT laligatwo_awaysotas_lookup.laligatwo_awaysotas FROM laligatwo_awaysotas_lookup INNER JOIN LALIGATWO_fixtures_sot ON laligatwo_awaysotas_lookup.AwayTeam_laligatwo_sot = LALIGATWO_fixtures_sot.AwayTeam_laligatwo_sot")

LALIGATWO_fixtures_sot$laligatwo_homesods <- rep(laligatwo_home_sods,each = length(laligatwo_teams)-1)

LALIGATWO_fixtures_sot$laligatwo_awaysods <- as.numeric(unlist(LALIGATWO_fixtures_sot$laligatwo_awaysods))
#xGH
LALIGATWO_fixtures_sot$laligatwo_xHST <- LALIGATWO_fixtures_sot$avg_HST_laligatwo * LALIGATWO_fixtures_sot$laligatwo_homesotas * LALIGATWO_fixtures_sot$laligatwo_awaysods
#xGA

LALIGATWO_fixtures_sot$laligatwo_awaysotas <- as.numeric(unlist(LALIGATWO_fixtures_sot$laligatwo_awaysotas))

LALIGATWO_fixtures_sot$laligatwo_xAST <- LALIGATWO_fixtures_sot$avg_AST_laligatwo * LALIGATWO_fixtures_sot$laligatwo_awaysotas * LALIGATWO_fixtures_sot$laligatwo_homesods

LALIGATWO_fixtures_sot$laligatwo_0_0 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_0 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_0_1 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_1 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_0 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_0_2 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_2 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_1 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_2 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_3 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_0 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_1 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_2 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_0_3 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_3 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_3 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_4 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_0 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_1 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_2 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_3 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_0_4 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_4 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_4 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_4 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_5 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_0 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_1 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_2 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_3 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_4 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_0_5 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_5 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_5 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_5 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_5 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_6 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_0 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_1 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_2 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_3 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_4 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_6_5 <- round(stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_0_6 <- round(stats::dpois(0,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_1_6 <- round(stats::dpois(1,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_2_6 <- round(stats::dpois(2,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_3_6 <- round(stats::dpois(3,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_4_6 <- round(stats::dpois(4,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
LALIGATWO_fixtures_sot$laligatwo_5_6 <- round(stats::dpois(5,LALIGATWO_fixtures_sot$laligatwo_xHST) * stats::dpois(6,LALIGATWO_fixtures_sot$laligatwo_xAST), digits = 4)
#Home win
LALIGATWO_fixtures_sot$laligatwo_H <- (
  LALIGATWO_fixtures_sot$laligatwo_1_0 + LALIGATWO_fixtures_sot$laligatwo_2_0 + LALIGATWO_fixtures_sot$laligatwo_2_1 + LALIGATWO_fixtures_sot$laligatwo_3_0 + LALIGATWO_fixtures_sot$laligatwo_3_1 +
    LALIGATWO_fixtures_sot$laligatwo_3_2 + LALIGATWO_fixtures_sot$laligatwo_4_0 + LALIGATWO_fixtures_sot$laligatwo_4_1 + LALIGATWO_fixtures_sot$laligatwo_4_2 + LALIGATWO_fixtures_sot$laligatwo_4_3 +
    LALIGATWO_fixtures_sot$laligatwo_5_0 + LALIGATWO_fixtures_sot$laligatwo_5_1 + LALIGATWO_fixtures_sot$laligatwo_5_2 + LALIGATWO_fixtures_sot$laligatwo_5_3 + LALIGATWO_fixtures_sot$laligatwo_5_4 +
    LALIGATWO_fixtures_sot$laligatwo_6_0 + LALIGATWO_fixtures_sot$laligatwo_6_1 + LALIGATWO_fixtures_sot$laligatwo_6_2 + LALIGATWO_fixtures_sot$laligatwo_6_3 + LALIGATWO_fixtures_sot$laligatwo_6_4 +
    LALIGATWO_fixtures_sot$laligatwo_6_5
)

LALIGATWO_fixtures_sot$laligatwo_H <- percent(LALIGATWO_fixtures_sot$laligatwo_H, accuracy = 0.1)

#Draw
LALIGATWO_fixtures_sot$laligatwo_D <- (

  LALIGATWO_fixtures_sot$laligatwo_0_0 + LALIGATWO_fixtures_sot$laligatwo_1_1 + LALIGATWO_fixtures_sot$laligatwo_2_2 + LALIGATWO_fixtures_sot$laligatwo_3_3 + LALIGATWO_fixtures_sot$laligatwo_4_4 +
    LALIGATWO_fixtures_sot$laligatwo_5_5 + LALIGATWO_fixtures_sot$laligatwo_6_6
)

LALIGATWO_fixtures_sot$laligatwo_D <- percent(LALIGATWO_fixtures_sot$laligatwo_D, accuracy = 0.1)

#Away

LALIGATWO_fixtures_sot$laligatwo_A <- (
  LALIGATWO_fixtures_sot$laligatwo_0_1 + LALIGATWO_fixtures_sot$laligatwo_0_2 + LALIGATWO_fixtures_sot$laligatwo_1_2 + LALIGATWO_fixtures_sot$laligatwo_0_3 + LALIGATWO_fixtures_sot$laligatwo_1_3 +
    LALIGATWO_fixtures_sot$laligatwo_2_3 + LALIGATWO_fixtures_sot$laligatwo_0_4 + LALIGATWO_fixtures_sot$laligatwo_1_4 + LALIGATWO_fixtures_sot$laligatwo_2_4 + LALIGATWO_fixtures_sot$laligatwo_3_4 +
    LALIGATWO_fixtures_sot$laligatwo_0_5 + LALIGATWO_fixtures_sot$laligatwo_1_5 + LALIGATWO_fixtures_sot$laligatwo_2_5 + LALIGATWO_fixtures_sot$laligatwo_3_5 + LALIGATWO_fixtures_sot$laligatwo_4_5 +
    LALIGATWO_fixtures_sot$laligatwo_0_6 + LALIGATWO_fixtures_sot$laligatwo_1_6 + LALIGATWO_fixtures_sot$laligatwo_2_6 + LALIGATWO_fixtures_sot$laligatwo_3_6 + LALIGATWO_fixtures_sot$laligatwo_4_6 +
    LALIGATWO_fixtures_sot$laligatwo_5_6
)

LALIGATWO_fixtures_sot$laligatwo_A <- percent(LALIGATWO_fixtures_sot$laligatwo_A, accuracy = 0.1)

#ov25
LALIGATWO_fixtures_sot$laligatwo_ov25 <- (
  LALIGATWO_fixtures_sot$laligatwo_2_1 + LALIGATWO_fixtures_sot$laligatwo_1_2 + LALIGATWO_fixtures_sot$laligatwo_2_2 + LALIGATWO_fixtures_sot$laligatwo_3_0 + LALIGATWO_fixtures_sot$laligatwo_3_1 +
    LALIGATWO_fixtures_sot$laligatwo_3_2 + LALIGATWO_fixtures_sot$laligatwo_0_3 + LALIGATWO_fixtures_sot$laligatwo_1_3 + LALIGATWO_fixtures_sot$laligatwo_2_3 + LALIGATWO_fixtures_sot$laligatwo_3_3 +
    LALIGATWO_fixtures_sot$laligatwo_4_0 + LALIGATWO_fixtures_sot$laligatwo_4_1 + LALIGATWO_fixtures_sot$laligatwo_4_2 + LALIGATWO_fixtures_sot$laligatwo_4_3 + LALIGATWO_fixtures_sot$laligatwo_0_4 +
    LALIGATWO_fixtures_sot$laligatwo_1_4 + LALIGATWO_fixtures_sot$laligatwo_2_4 + LALIGATWO_fixtures_sot$laligatwo_3_4 + LALIGATWO_fixtures_sot$laligatwo_4_4 + LALIGATWO_fixtures_sot$laligatwo_5_0 +
    LALIGATWO_fixtures_sot$laligatwo_5_1 + LALIGATWO_fixtures_sot$laligatwo_5_2 + LALIGATWO_fixtures_sot$laligatwo_5_3 + LALIGATWO_fixtures_sot$laligatwo_5_4 + LALIGATWO_fixtures_sot$laligatwo_0_5 +
    LALIGATWO_fixtures_sot$laligatwo_1_5 + LALIGATWO_fixtures_sot$laligatwo_2_5 + LALIGATWO_fixtures_sot$laligatwo_3_5 + LALIGATWO_fixtures_sot$laligatwo_4_5 + LALIGATWO_fixtures_sot$laligatwo_5_5 +
    LALIGATWO_fixtures_sot$laligatwo_6_0 + LALIGATWO_fixtures_sot$laligatwo_6_1 + LALIGATWO_fixtures_sot$laligatwo_6_2 + LALIGATWO_fixtures_sot$laligatwo_6_3 + LALIGATWO_fixtures_sot$laligatwo_6_4 +
    LALIGATWO_fixtures_sot$laligatwo_6_5 + LALIGATWO_fixtures_sot$laligatwo_0_6 + LALIGATWO_fixtures_sot$laligatwo_1_6 + LALIGATWO_fixtures_sot$laligatwo_2_6 + LALIGATWO_fixtures_sot$laligatwo_3_6 +
    LALIGATWO_fixtures_sot$laligatwo_4_6 + LALIGATWO_fixtures_sot$laligatwo_5_6 + LALIGATWO_fixtures_sot$laligatwo_6_6
)
#un25
LALIGATWO_fixtures_sot$laligatwo_un25 <- (
  LALIGATWO_fixtures_sot$laligatwo_0_0 + LALIGATWO_fixtures_sot$laligatwo_1_0 + LALIGATWO_fixtures_sot$laligatwo_0_1 + LALIGATWO_fixtures_sot$laligatwo_1_1 + LALIGATWO_fixtures_sot$laligatwo_2_0 + LALIGATWO_fixtures_sot$laligatwo_0_2
)
#odds
LALIGATWO_fixtures_sot$laligatwo_ov25_odds <- round((1/LALIGATWO_fixtures_sot$laligatwo_ov25),digits = 2)
LALIGATWO_fixtures_sot$laligatwo_un25_odds <- round((1/LALIGATWO_fixtures_sot$laligatwo_un25),digits = 2)

LALIGATWO_fixtures_sot$laligatwo_ov25_odds
LALIGATWO_fixtures_sot$laligatwo_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
LALIGATWO_fixtures_sot$laligatwo_ov25 <- percent(LALIGATWO_fixtures_sot$laligatwo_ov25, accuracy = 0.1)

LALIGATWO_fixtures_sot$laligatwo_un25 <- percent(LALIGATWO_fixtures_sot$laligatwo_un25, accuracy = 0.1)
LALIGATWO_fixtures_sot$laligatwo_pssotre <- paste(round(LALIGATWO_fixtures_sot$laligatwo_xHST,digits = 0),round(LALIGATWO_fixtures_sot$laligatwo_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
laligatwo_home_wins <- c()
laligatwo_away_wins <- c()
laligatwo_home_draws <- c()
laligatwo_away_draws <- c()
laligatwo_home_loss <- c()
laligatwo_away_loss <- c()



for (i_laligatwo_wins in 1:length(laligatwo_teams))
{

  laligatwo_home_wins[i_laligatwo_wins] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_wins] & LALIGATWO$FTR == "H",])
  laligatwo_away_wins[i_laligatwo_wins] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_wins] & LALIGATWO$FTR == "A",])
  laligatwo_home_draws[i_laligatwo_wins] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_wins] & LALIGATWO$FTR == "D",])
  laligatwo_away_draws[i_laligatwo_wins] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_wins] & LALIGATWO$FTR == "D",])
  laligatwo_home_loss[i_laligatwo_wins] <- nrow(LALIGATWO[LALIGATWO$HomeTeam == laligatwo_teams[i_laligatwo_wins] & LALIGATWO$FTR == "A",])
  laligatwo_away_loss[i_laligatwo_wins] <- nrow(LALIGATWO[LALIGATWO$AwayTeam == laligatwo_teams[i_laligatwo_wins] & LALIGATWO$FTR == "H",])

}

laligatwo_total_wins <- laligatwo_home_wins + laligatwo_away_wins
laligatwo_total_draws <- laligatwo_home_draws + laligatwo_away_draws
laligatwo_total_loss <- laligatwo_home_loss + laligatwo_away_loss

laligatwo_league_table <- cbind(laligatwo_teams,laligatwo_games_played,laligatwo_total_wins,laligatwo_total_draws,laligatwo_total_loss)
laligatwo_GS <- laligatwo_scoring$TGS
laligatwo_GC <-laligatwo_conceding$TGC
laligatwo_GD <- laligatwo_scoring$TGS - laligatwo_conceding$TGC
laligatwo_PTS <- (laligatwo_total_wins*3) + (laligatwo_total_draws*1)
laligatwo_league_table <- cbind(laligatwo_league_table,laligatwo_GS,laligatwo_GC,laligatwo_GD,laligatwo_PTS)
laligatwo_league_table <- as.data.frame(laligatwo_league_table)
#rename the columns
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_teams"] <- "Team"
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_games_played"] <- "P"
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_total_wins"] <- "W"
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_total_draws"] <- "D"
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_total_loss"] <- "L"
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_GS"] <- "F"
names(laligatwo_league_table)[names(laligatwo_league_table) == "laligatwo_GC"] <- "A"
points_laligatwo <- laligatwo_league_table[order(as.numeric(laligatwo_league_table$laligatwo_PTS), decreasing = TRUE),]
points_laligatwo$laligatwo_rank <- 1:length(laligatwo_teams)
row.names(points_laligatwo) <- points_laligatwo$laligatwo_rank
#create final_laligatwo_hf_against with team ranks in brackets
for(laligatwo_rowhrank in 1:nrow(laligatwo_form_team_against_h)) {
  for(laligatwo_colhrank in 1:ncol(laligatwo_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!laligatwo_form_team_against_h[laligatwo_rowhrank,laligatwo_colhrank]=="",laligatwo_form_team_against_h[laligatwo_rowhrank,laligatwo_colhrank] <- paste(laligatwo_form_team_against_h[laligatwo_rowhrank,laligatwo_colhrank],"(",points_laligatwo$laligatwo_rank[points_laligatwo$Team ==laligatwo_form_team_against_h[laligatwo_rowhrank,laligatwo_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
laligatwo_GP <- nrow(LALIGATWO)

#Calculate total home goals for each division
laligatwo_T_HG <- sum(laligatwo_home_gs$x)

#calculate average home goal
laligatwo_avg_HG <- round(laligatwo_T_HG /laligatwo_GP, digits = 4)
############################################################
#Calculate total away goals for each division
laligatwo_T_AG <- sum(laligatwo_away_gs$x)
#calculate average away goal
laligatwo_avg_AG <- round(laligatwo_T_AG /laligatwo_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
laligatwo_home_as <- round(((laligatwo_home_gs$x/laligatwo_home_games))/laligatwo_avg_HG, digits = 4)
#calculate away attack strength
laligatwo_away_as <- round(((laligatwo_away_gs$x/laligatwo_away_games))/laligatwo_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
laligatwo_avg_HC <- round(laligatwo_T_AG /laligatwo_GP, digits = 4)
#avg away concede
laligatwo_avg_AC <- round(laligatwo_T_HG /laligatwo_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
laligatwo_home_ds <- round(((laligatwo_home_gc$x/laligatwo_home_games))/laligatwo_avg_HC, digits = 4)
#away defense strength
laligatwo_away_ds <- round(((laligatwo_away_gc$x/laligatwo_away_games))/laligatwo_avg_AC, digits = 4)
#############################################################################
#home poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_home_poisson <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_HG,laligatwo_home_as,laligatwo_home_ds)
#################################################################################
#away poisson data
#laligatwo
laligatwo_division <- c()
laligatwo_division[1:length(laligatwo_teams)] <- "LALIGATWO"
laligatwo_away_poisson <- cbind(laligatwo_division,laligatwo_teams,laligatwo_avg_AG,laligatwo_away_as,laligatwo_away_ds)

#LALIGATWO
HomeTeam_laligatwo <- rep(laligatwo_teams, each = length(laligatwo_teams))
AwayTeam_laligatwo <- rep(laligatwo_teams, length(laligatwo_teams))
LALIGATWO_fixtures <- cbind(HomeTeam_laligatwo,AwayTeam_laligatwo)
LALIGATWO_fixtures <- as.data.frame(LALIGATWO_fixtures)
LALIGATWO_fixtures <- LALIGATWO_fixtures[!LALIGATWO_fixtures$HomeTeam_laligatwo == LALIGATWO_fixtures$AwayTeam_laligatwo,]
rownames(LALIGATWO_fixtures) <- NULL
LALIGATWO_fixtures$Div <- "LALIGATWO"
LALIGATWO_fixtures <- LALIGATWO_fixtures[,c(3,1,2)]

LALIGATWO_fixtures$avg_HG_laligatwo <- laligatwo_avg_HG

LALIGATWO_fixtures$laligatwo_homeas <- rep(laligatwo_home_as,each = length(laligatwo_teams)-1)

laligatwo_awayds_lookup <- cbind(laligatwo_teams,laligatwo_away_ds)

laligatwo_awayds_lookup <- as.data.frame(laligatwo_awayds_lookup)

colnames(laligatwo_awayds_lookup) <- c("AwayTeam_laligatwo","laligatwo_awayds")


require('RH2')
LALIGATWO_fixtures$laligatwo_awayds <- sqldf("SELECT laligatwo_awayds_lookup.laligatwo_awayds FROM laligatwo_awayds_lookup INNER JOIN LALIGATWO_fixtures ON laligatwo_awayds_lookup.AwayTeam_laligatwo = LALIGATWO_fixtures.AwayTeam_laligatwo")

LALIGATWO_fixtures$avg_AG_laligatwo <- laligatwo_avg_AG

laligatwo_awayas_lookup <- cbind(laligatwo_teams,laligatwo_away_as)

laligatwo_awayas_lookup <- as.data.frame(laligatwo_awayas_lookup)

colnames(laligatwo_awayas_lookup) <- c("AwayTeam_laligatwo","laligatwo_awayas")


LALIGATWO_fixtures$laligatwo_awayas <- sqldf("SELECT laligatwo_awayas_lookup.laligatwo_awayas FROM laligatwo_awayas_lookup INNER JOIN LALIGATWO_fixtures ON laligatwo_awayas_lookup.AwayTeam_laligatwo = LALIGATWO_fixtures.AwayTeam_laligatwo")

LALIGATWO_fixtures$laligatwo_homeds <- rep(laligatwo_home_ds,each = length(laligatwo_teams)-1)

LALIGATWO_fixtures$laligatwo_awayds <- as.numeric(unlist(LALIGATWO_fixtures$laligatwo_awayds))
#xGH
LALIGATWO_fixtures$laligatwo_xGH <- LALIGATWO_fixtures$avg_HG_laligatwo * LALIGATWO_fixtures$laligatwo_homeas * LALIGATWO_fixtures$laligatwo_awayds

#xGA

LALIGATWO_fixtures$laligatwo_awayas <- as.numeric(unlist(LALIGATWO_fixtures$laligatwo_awayas))

LALIGATWO_fixtures$laligatwo_xGA <- LALIGATWO_fixtures$avg_AG_laligatwo * LALIGATWO_fixtures$laligatwo_awayas * LALIGATWO_fixtures$laligatwo_homeds

LALIGATWO_fixtures$laligatwo_0_0 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_0 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_0_1 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_1 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_0 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_0_2 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_2 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_1 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_2 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_3 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_0 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_1 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_2 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_0_3 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_3 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_3 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_4 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_0 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_1 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_2 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_3 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_0_4 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_4 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_4 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_4 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_5 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_0 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_1 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_2 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_3 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_4 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_0_5 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_5 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_5 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_5 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_5 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_6 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_0 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_1 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_2 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_3 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_4 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_6_5 <- round(stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_0_6 <- round(stats::dpois(0,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_1_6 <- round(stats::dpois(1,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_2_6 <- round(stats::dpois(2,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_3_6 <- round(stats::dpois(3,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_4_6 <- round(stats::dpois(4,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
LALIGATWO_fixtures$laligatwo_5_6 <- round(stats::dpois(5,LALIGATWO_fixtures$laligatwo_xGH) * stats::dpois(6,LALIGATWO_fixtures$laligatwo_xGA), digits = 4)
#Home win
LALIGATWO_fixtures$laligatwo_H <- (
  LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
    LALIGATWO_fixtures$laligatwo_5_0 + LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5
)

LALIGATWO_fixtures$laligatwo_H <- percent(LALIGATWO_fixtures$laligatwo_H, accuracy = 0.1)

#Draw
LALIGATWO_fixtures$laligatwo_D <- (

  LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 + LALIGATWO_fixtures$laligatwo_4_4 +
    LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6
)

LALIGATWO_fixtures$laligatwo_D <- percent(LALIGATWO_fixtures$laligatwo_D, accuracy = 0.1)

#Away

LALIGATWO_fixtures$laligatwo_A <- (
  LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
    LALIGATWO_fixtures$laligatwo_0_5 + LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6
)

LALIGATWO_fixtures$laligatwo_A <- percent(LALIGATWO_fixtures$laligatwo_A, accuracy = 0.1)

#ov25
LALIGATWO_fixtures$laligatwo_ov25 <- (
  LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 + LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 + LALIGATWO_fixtures$laligatwo_0_4 +
    LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 + LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_0 +
    LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 + LALIGATWO_fixtures$laligatwo_0_5 +
    LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 + LALIGATWO_fixtures$laligatwo_5_5 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5 + LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 +
    LALIGATWO_fixtures$laligatwo_4_6 + LALIGATWO_fixtures$laligatwo_5_6 + LALIGATWO_fixtures$laligatwo_6_6
)
#un25
LALIGATWO_fixtures$laligatwo_un25 <- (
  LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_0_2
)
#odds
LALIGATWO_fixtures$laligatwo_ov25_odds <- round((1/LALIGATWO_fixtures$laligatwo_ov25),digits = 2)
LALIGATWO_fixtures$laligatwo_un25_odds <- round((1/LALIGATWO_fixtures$laligatwo_un25),digits = 2)

LALIGATWO_fixtures$laligatwo_ov25_odds
LALIGATWO_fixtures$laligatwo_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
LALIGATWO_fixtures$laligatwo_BTTSY <- (
  LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_3_1 + LALIGATWO_fixtures$laligatwo_3_2 +
    LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_1_3 + LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_3_3 + LALIGATWO_fixtures$laligatwo_4_4 +
    LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_3 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 +
    LALIGATWO_fixtures$laligatwo_3_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 +
    LALIGATWO_fixtures$laligatwo_5_4 + LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_6_6 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6
)
#BTTSN
LALIGATWO_fixtures$laligatwo_BTTSN <- (
  LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_0_2 +
    LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_5_0 +
    LALIGATWO_fixtures$laligatwo_0_5 + LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_0_6
)

LALIGATWO_fixtures$laligatwo_BTTSY_odds <- round((1/LALIGATWO_fixtures$laligatwo_BTTSY),digits = 2)
LALIGATWO_fixtures$laligatwo_BTTSN_odds <- round((1/LALIGATWO_fixtures$laligatwo_BTTSN),digits = 2)

LALIGATWO_fixtures$laligatwo_BTTSY <- percent(LALIGATWO_fixtures$laligatwo_BTTSY, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_BTTSN <- percent(LALIGATWO_fixtures$laligatwo_BTTSN, accuracy = 0.1)
#odds
LALIGATWO_fixtures$laligatwo_BTTSY_odds
LALIGATWO_fixtures$laligatwo_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
LALIGATWO_fixtures$laligatwo_AH_0_H <- (
  LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
    LALIGATWO_fixtures$laligatwo_5_0 +LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5 + LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6
)
#AH_0_A
LALIGATWO_fixtures$laligatwo_AH_0_A <- (
  LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
    LALIGATWO_fixtures$laligatwo_0_5 +LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6 + LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6
)

#odds
LALIGATWO_fixtures$laligatwo_AH_0_H_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_0_H),digits = 2)
LALIGATWO_fixtures$laligatwo_AH_0_A_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_0_A),digits = 2)

LALIGATWO_fixtures$laligatwo_AH_0_H_odds
LALIGATWO_fixtures$laligatwo_AH_0_A_odds
#percentages
LALIGATWO_fixtures$laligatwo_AH_0_H <- percent(LALIGATWO_fixtures$laligatwo_AH_0_H, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_AH_0_A <- percent(LALIGATWO_fixtures$laligatwo_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
LALIGATWO_fixtures$laligatwo_AH_n075_H <- (
  LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
    LALIGATWO_fixtures$laligatwo_5_0 +LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5
)
#AH_n075_A
LALIGATWO_fixtures$laligatwo_AH_n075_A <- (
  LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
    LALIGATWO_fixtures$laligatwo_0_5 +LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6
)

#odds
LALIGATWO_fixtures$laligatwo_AH_n075_H_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_n075_H),digits = 2)
LALIGATWO_fixtures$laligatwo_AH_n075_A_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_n075_A),digits = 2)

LALIGATWO_fixtures$laligatwo_AH_n075_H_odds
LALIGATWO_fixtures$laligatwo_AH_n075_A_odds
#percentages
LALIGATWO_fixtures$laligatwo_AH_n075_H <- percent(LALIGATWO_fixtures$laligatwo_AH_n075_H, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_AH_n075_A <- percent(LALIGATWO_fixtures$laligatwo_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
LALIGATWO_fixtures$laligatwo_AH_075_H <- (
  LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
    LALIGATWO_fixtures$laligatwo_5_0 +LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5 + LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6 + LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_1_2 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_3_4 + LALIGATWO_fixtures$laligatwo_4_5 + LALIGATWO_fixtures$laligatwo_5_6
)
#AH_075_A
LALIGATWO_fixtures$laligatwo_AH_075_A <- (
  LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
    LALIGATWO_fixtures$laligatwo_0_5 +LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6 + LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6 + LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_3 + LALIGATWO_fixtures$laligatwo_5_4 + LALIGATWO_fixtures$laligatwo_6_5
)

#odds
LALIGATWO_fixtures$laligatwo_AH_075_H_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_075_H),digits = 2)
LALIGATWO_fixtures$laligatwo_AH_075_A_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_075_A),digits = 2)

LALIGATWO_fixtures$laligatwo_AH_075_H_odds
LALIGATWO_fixtures$laligatwo_AH_075_A_odds
#percentages
LALIGATWO_fixtures$laligatwo_AH_075_H <- percent(LALIGATWO_fixtures$laligatwo_AH_075_H, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_AH_075_A <- percent(LALIGATWO_fixtures$laligatwo_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
LALIGATWO_fixtures$laligatwo_AH_n125_H <- (
  LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
    LALIGATWO_fixtures$laligatwo_5_0 +LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5
)
#AH_n125_A
LALIGATWO_fixtures$laligatwo_AH_n125_A <- (
  LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
    LALIGATWO_fixtures$laligatwo_0_5 +LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6
)

#odds
LALIGATWO_fixtures$laligatwo_AH_n125_H_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_n125_H),digits = 2)
LALIGATWO_fixtures$laligatwo_AH_n125_A_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_n125_A),digits = 2)

LALIGATWO_fixtures$laligatwo_AH_n125_H_odds
LALIGATWO_fixtures$laligatwo_AH_n125_A_odds
#percentages
LALIGATWO_fixtures$laligatwo_AH_n125_H <- percent(LALIGATWO_fixtures$laligatwo_AH_n125_H, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_AH_n125_A <- percent(LALIGATWO_fixtures$laligatwo_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
LALIGATWO_fixtures$laligatwo_AH_125_H <- (
  LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
    LALIGATWO_fixtures$laligatwo_5_0 +LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
    LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
    LALIGATWO_fixtures$laligatwo_6_5 + LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6 + LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_1_2 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_3_4 + LALIGATWO_fixtures$laligatwo_4_5 + LALIGATWO_fixtures$laligatwo_5_6
)
#AH_125_A
LALIGATWO_fixtures$laligatwo_AH_125_A <- (
  LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
    LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
    LALIGATWO_fixtures$laligatwo_0_5 +LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
    LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
    LALIGATWO_fixtures$laligatwo_5_6 + LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 +
    LALIGATWO_fixtures$laligatwo_4_4 + LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6 + LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_1 +
    LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_3 + LALIGATWO_fixtures$laligatwo_5_4 + LALIGATWO_fixtures$laligatwo_6_5
)

#odds
LALIGATWO_fixtures$laligatwo_AH_125_H_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_125_H),digits = 2)
LALIGATWO_fixtures$laligatwo_AH_125_A_odds <- round((1/LALIGATWO_fixtures$laligatwo_AH_125_A),digits = 2)

LALIGATWO_fixtures$laligatwo_AH_125_H_odds
LALIGATWO_fixtures$laligatwo_AH_125_A_odds
#percentages
LALIGATWO_fixtures$laligatwo_AH_125_H <- percent(LALIGATWO_fixtures$laligatwo_AH_125_H, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_AH_125_A <- percent(LALIGATWO_fixtures$laligatwo_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
LALIGATWO_fixtures$laligatwo_ov25 <- percent(LALIGATWO_fixtures$laligatwo_ov25, accuracy = 0.1)

LALIGATWO_fixtures$laligatwo_un25 <- percent(LALIGATWO_fixtures$laligatwo_un25, accuracy = 0.1)
LALIGATWO_fixtures$laligatwo_pscore <- paste(round(LALIGATWO_fixtures$laligatwo_xGH,digits = 0),round(LALIGATWO_fixtures$laligatwo_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
laligatwo_last_n_games <- 6

#create final_laligatwo_hf object
final_laligatwo_hf <- c()
for(index_laligatwo_hf in 1:length(laligatwo_teams))
{
  index_laligatwo_hf <- row.names(laligatwo_form_h) == laligatwo_teams[index_laligatwo_hf]
  form_laligatwo_hf <- laligatwo_form_h[index_laligatwo_hf]
  deleted_form_laligatwo_hf <- form_laligatwo_hf[!form_laligatwo_hf[] == ""]
  l6_form_laligatwo_hf <- tail(deleted_form_laligatwo_hf,laligatwo_last_n_games)
  l6_form_laligatwo_hf <- paste(l6_form_laligatwo_hf,collapse = " ")
  final_laligatwo_hf[index_laligatwo_hf] <- rbind(paste(laligatwo_teams[index_laligatwo_hf],l6_form_laligatwo_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}

#change column nam
final_laligatwo_hf <- as.data.frame(final_laligatwo_hf)
colnames(final_laligatwo_hf) <- "Form"
#goals scored
#create final_laligatwo_gs object
final_laligatwo_gs <- c()
suml6_laligatwo_gs <- c()
for(index_laligatwo_gs in 1:length(laligatwo_teams))
{
  index_laligatwo_gs <- row.names(laligatwo_goalscored_h) == laligatwo_teams[index_laligatwo_gs]
  form_laligatwo_gs <- laligatwo_goalscored_h[index_laligatwo_gs]
  deleted_form_laligatwo_gs <- form_laligatwo_gs[!form_laligatwo_gs[] == ""]
  l6_form_laligatwo_gs <- tail(deleted_form_laligatwo_gs,laligatwo_last_n_games)
  l6_form_laligatwo_gs <- as.numeric(l6_form_laligatwo_gs)
  suml6_laligatwo_gs[index_laligatwo_gs] <- sum(l6_form_laligatwo_gs)
  suml6_laligatwo_gs[index_laligatwo_gs] <- paste("(",suml6_laligatwo_gs[index_laligatwo_gs],")",sep = "")
  l6_form_laligatwo_gs <- paste(l6_form_laligatwo_gs,collapse = " ")
  final_laligatwo_gs[index_laligatwo_gs] <- rbind(paste(laligatwo_teams[index_laligatwo_gs],l6_form_laligatwo_gs,suml6_laligatwo_gs[index_laligatwo_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
final_laligatwo_gs
#change column names
final_laligatwo_gs <- as.data.frame(final_laligatwo_gs)
colnames(final_laligatwo_gs) <- "Goals scored"
#goal conceded
#create final_laligatwo_gc object
final_laligatwo_gc <- c()
suml6_laligatwo_gc <- c()
for(index_laligatwo_gc in 1:length(laligatwo_teams))
{
  index_laligatwo_gc <- row.names(laligatwo_goalconceded_h) == laligatwo_teams[index_laligatwo_gc]
  form_laligatwo_gc <- laligatwo_goalconceded_h[index_laligatwo_gc]
  deleted_form_laligatwo_gc <- form_laligatwo_gc[!form_laligatwo_gc[] == ""]
  l6_form_laligatwo_gc <- tail(deleted_form_laligatwo_gc,laligatwo_last_n_games)
  l6_form_laligatwo_gc <- as.numeric(l6_form_laligatwo_gc)
  suml6_laligatwo_gc[index_laligatwo_gc] <- sum(l6_form_laligatwo_gc)
  suml6_laligatwo_gc[index_laligatwo_gc] <- paste("(",suml6_laligatwo_gc[index_laligatwo_gc],")",sep = "")
  l6_form_laligatwo_gc <- paste(l6_form_laligatwo_gc,collapse = " ")
  final_laligatwo_gc[index_laligatwo_gc] <- rbind(paste(laligatwo_teams[index_laligatwo_gc],l6_form_laligatwo_gc,suml6_laligatwo_gc[index_laligatwo_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
#change column names
final_laligatwo_gc <- as.data.frame(final_laligatwo_gc)
colnames(final_laligatwo_gc) <- "Goals conceded"


toString(l6_form_laligatwo_gc)
#total goals
#create final_laligatwo_tg object
final_laligatwo_tg <- c()
suml6_laligatwo_tg <- c()
for(index_laligatwo_tg in 1:length(laligatwo_teams))
{
  index_laligatwo_tg <- row.names(laligatwo_totalgoals_h) == laligatwo_teams[index_laligatwo_tg]
  form_laligatwo_tg <- laligatwo_totalgoals_h[index_laligatwo_tg]
  deleted_form_laligatwo_tg <- form_laligatwo_tg[!form_laligatwo_tg[] == ""]
  l6_form_laligatwo_tg <- tail(deleted_form_laligatwo_tg,laligatwo_last_n_games)
  l6_form_laligatwo_tg <- as.numeric(l6_form_laligatwo_tg)
  suml6_laligatwo_tg[index_laligatwo_tg] <- sum(l6_form_laligatwo_tg)
  suml6_laligatwo_tg[index_laligatwo_tg] <- paste("(",suml6_laligatwo_tg[index_laligatwo_tg],")",sep = "")
  l6_form_laligatwo_tg <- paste(l6_form_laligatwo_tg,collapse = " ")
  final_laligatwo_tg[index_laligatwo_tg] <- rbind(paste(laligatwo_teams[index_laligatwo_tg],l6_form_laligatwo_tg,suml6_laligatwo_tg[index_laligatwo_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
#change column names
final_laligatwo_tg <- as.data.frame(final_laligatwo_tg)
colnames(final_laligatwo_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_laligatwo_hf object
final_laligatwo_cs <- c()
for(index_laligatwo_cs in 1:length(laligatwo_teams))
{
  index_laligatwo_cs <- row.names(laligatwo_csform_h) == laligatwo_teams[index_laligatwo_cs]
  csform_laligatwo_cs <- laligatwo_csform_h[index_laligatwo_cs]
  deleted_csform_laligatwo_cs <- csform_laligatwo_cs[!csform_laligatwo_cs[] == ""]
  l6_csform_laligatwo_cs <- tail(deleted_csform_laligatwo_cs,laligatwo_last_n_games)
  l6_csform_laligatwo_cs <- paste(l6_csform_laligatwo_cs,collapse = " ")
  final_laligatwo_cs[index_laligatwo_cs] <- rbind(paste(laligatwo_teams[index_laligatwo_cs],l6_csform_laligatwo_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_csform)

}

#change column names
final_laligatwo_cs <- as.data.frame(final_laligatwo_cs)
colnames(final_laligatwo_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_laligatwo_wm object
final_laligatwo_wm <- c()
suml6_laligatwo_wm <- c()
for(index_laligatwo_wm in 1:length(laligatwo_teams))
{
  index_laligatwo_wm <- row.names(laligatwo_winmargin_h) == laligatwo_teams[index_laligatwo_wm]
  form_laligatwo_wm <- laligatwo_winmargin_h[index_laligatwo_wm]
  deleted_form_laligatwo_wm <- form_laligatwo_wm[!form_laligatwo_wm[] == ""]
  l6_form_laligatwo_wm <- tail(deleted_form_laligatwo_wm,laligatwo_last_n_games)
  l6_form_laligatwo_wm <- as.numeric(l6_form_laligatwo_wm)
  suml6_laligatwo_wm[index_laligatwo_wm] <- sum(l6_form_laligatwo_wm)
  suml6_laligatwo_wm[index_laligatwo_wm] <- paste("(",suml6_laligatwo_wm[index_laligatwo_wm],")",sep = "")
  l6_form_laligatwo_wm <- paste(l6_form_laligatwo_wm,collapse = " ")
  final_laligatwo_wm[index_laligatwo_wm] <- rbind(paste(laligatwo_teams[index_laligatwo_wm],l6_form_laligatwo_wm,suml6_laligatwo_wm[index_laligatwo_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
final_laligatwo_wm
#change column names
final_laligatwo_wm <- as.data.frame(final_laligatwo_wm)
colnames(final_laligatwo_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_laligatwo_ca object
final_laligatwo_ca <- c()
suml6_laligatwo_ca <- c()
for(index_laligatwo_ca in 1:length(laligatwo_teams))
{
  index_laligatwo_ca <- row.names(laligatwo_coawarded_h) == laligatwo_teams[index_laligatwo_ca]
  form_laligatwo_ca <- laligatwo_coawarded_h[index_laligatwo_ca]
  deleted_form_laligatwo_ca <- form_laligatwo_ca[!form_laligatwo_ca[] == ""]
  l6_form_laligatwo_ca <- tail(deleted_form_laligatwo_ca,laligatwo_last_n_games)
  l6_form_laligatwo_ca <- as.numeric(l6_form_laligatwo_ca)
  suml6_laligatwo_ca[index_laligatwo_ca] <- sum(l6_form_laligatwo_ca)
  suml6_laligatwo_ca[index_laligatwo_ca] <- paste("(",suml6_laligatwo_ca[index_laligatwo_ca],")",sep = "")
  l6_form_laligatwo_ca <- paste(l6_form_laligatwo_ca,collapse = " ")
  final_laligatwo_ca[index_laligatwo_ca] <- rbind(paste(laligatwo_teams[index_laligatwo_ca],l6_form_laligatwo_ca,suml6_laligatwo_ca[index_laligatwo_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
final_laligatwo_ca
#change column names
final_laligatwo_ca <- as.data.frame(final_laligatwo_ca)
colnames(final_laligatwo_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_laligatwo_ca object
final_laligatwo_cc <- c()
suml6_laligatwo_cc <- c()
for(index_laligatwo_cc in 1:length(laligatwo_teams))
{
  index_laligatwo_cc <- row.names(laligatwo_cornersconceded_h) == laligatwo_teams[index_laligatwo_cc]
  form_laligatwo_cc <- laligatwo_cornersconceded_h[index_laligatwo_cc]
  deleted_form_laligatwo_cc <- form_laligatwo_cc[!form_laligatwo_cc[] == ""]
  l6_form_laligatwo_cc <- tail(deleted_form_laligatwo_cc,laligatwo_last_n_games)
  l6_form_laligatwo_cc <- as.numeric(l6_form_laligatwo_cc)
  suml6_laligatwo_cc[index_laligatwo_cc] <- sum(l6_form_laligatwo_cc)
  suml6_laligatwo_cc[index_laligatwo_cc] <- paste("(",suml6_laligatwo_cc[index_laligatwo_cc],")",sep = "")
  l6_form_laligatwo_cc <- paste(l6_form_laligatwo_cc,collapse = " ")
  final_laligatwo_cc[index_laligatwo_cc] <- rbind(paste(laligatwo_teams[index_laligatwo_cc],l6_form_laligatwo_cc,suml6_laligatwo_cc[index_laligatwo_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
final_laligatwo_cc
#change column names
final_laligatwo_cc <- as.data.frame(final_laligatwo_cc)
colnames(final_laligatwo_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_laligatwo_cosc <- c()
for(index_laligatwo_cosc in 1:length(laligatwo_teams))
{
  index_laligatwo_cosc <- row.names(laligatwo_coscform_h) == laligatwo_teams[index_laligatwo_cosc]
  coscform_laligatwo_cosc <- laligatwo_coscform_h[index_laligatwo_cosc]
  deleted_coscform_laligatwo_cosc <- coscform_laligatwo_cosc[!coscform_laligatwo_cosc[] == ""]
  l6_coscform_laligatwo_cosc <- tail(deleted_coscform_laligatwo_cosc,laligatwo_last_n_games)
  l6_coscform_laligatwo_cosc <- paste(l6_coscform_laligatwo_cosc,collapse = " ")
  final_laligatwo_cosc[index_laligatwo_cosc] <- rbind(paste(laligatwo_teams[index_laligatwo_cosc],l6_coscform_laligatwo_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_coscform)

}
final_laligatwo_cosc
#change column names
final_laligatwo_cosc <- as.data.frame(final_laligatwo_cosc)
colnames(final_laligatwo_cosc) <- "CornersForm"
##################################################
#total corners
#create final_laligatwo_tcorners object
final_laligatwo_tcorners <- c()
suml6_laligatwo_tcorners <- c()
for(index_laligatwo_tcorners in 1:length(laligatwo_teams))
{
  index_laligatwo_tcorners <- row.names(laligatwo_totalcorners_h) == laligatwo_teams[index_laligatwo_tcorners]
  form_laligatwo_tcorners <- laligatwo_totalcorners_h[index_laligatwo_tcorners]
  deleted_form_laligatwo_tcorners <- form_laligatwo_tcorners[!form_laligatwo_tcorners[] == ""]
  l6_form_laligatwo_tcorners <- tail(deleted_form_laligatwo_tcorners,laligatwo_last_n_games)
  l6_form_laligatwo_tcorners <- as.numeric(l6_form_laligatwo_tcorners)
  suml6_laligatwo_tcorners[index_laligatwo_tcorners] <- sum(l6_form_laligatwo_tcorners)
  suml6_laligatwo_tcorners[index_laligatwo_tcorners] <- paste("(",suml6_laligatwo_tcorners[index_laligatwo_tcorners],")",sep = "")
  l6_form_laligatwo_tcorners <- paste(l6_form_laligatwo_tcorners,collapse = " ")
  final_laligatwo_tcorners[index_laligatwo_tcorners] <- rbind(paste(laligatwo_teams[index_laligatwo_tcorners],l6_form_laligatwo_tcorners,suml6_laligatwo_tcorners[index_laligatwo_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
#change column names
final_laligatwo_tcorners <- as.data.frame(final_laligatwo_tcorners)
colnames(final_laligatwo_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_laligatwo_hf_against
final_laligatwo_hf_against <- c()
for(index_laligatwo_hf_against in 1:length(laligatwo_teams))
{
  index_laligatwo_hf_against <- row.names(laligatwo_form_team_against_h) == laligatwo_teams[index_laligatwo_hf_against]
  form_laligatwo_hf_against <- laligatwo_form_team_against_h[index_laligatwo_hf_against]
  deleted_form_laligatwo_hf_against <- form_laligatwo_hf_against[!form_laligatwo_hf_against[] == ""]
  l6_form_laligatwo_hf_against <- tail(deleted_form_laligatwo_hf_against,laligatwo_last_n_games)
  l6_form_laligatwo_hf_against <- paste(l6_form_laligatwo_hf_against,collapse = " ")
  final_laligatwo_hf_against[index_laligatwo_hf_against] <- rbind(paste(laligatwo_teams[index_laligatwo_hf_against],l6_form_laligatwo_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",laligatwo_teams[index],l6_form)

}
final_laligatwo_hf_against <- as.data.frame(final_laligatwo_hf_against)
colnames(final_laligatwo_hf_against) <- "Team against"
#combine the columns
final_laligatwo_all <- cbind(final_laligatwo_hf,final_laligatwo_gs,final_laligatwo_gc,final_laligatwo_tg,final_laligatwo_ca,final_laligatwo_cc,final_laligatwo_tcorners,final_laligatwo_cosc,final_laligatwo_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#LALIGATWO
LALIGATWO_sim <- LALIGATWO
LALIGATWO_sim$matchid <- paste(LALIGATWO_sim$HomeTeam,LALIGATWO_sim$AwayTeam,sep = "-")
LALIGATWO_fixtures$matchid <- paste(LALIGATWO_fixtures$HomeTeam_laligatwo,LALIGATWO_fixtures$AwayTeam_laligatwo,sep = "-")
LALIGATWO_fixtures$laligatwo_FTR <- sapply(LALIGATWO_fixtures$laligatwo_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

LALIGATWO_fixtures$laligatwo_gamestatus <- ifelse(LALIGATWO_fixtures$matchid %in% LALIGATWO_sim$matchid,"played","notplayed")

laligatwo_home_wins_sim <- c()
laligatwo_away_wins_sim <- c()
laligatwo_home_draws_sim <- c()
laligatwo_away_draws_sim <- c()
laligatwo_home_loss_sim <- c()
laligatwo_away_loss_sim <- c()



for (i_laligatwo_wins_sim in 1:length(laligatwo_teams))
{

  laligatwo_home_wins_sim[i_laligatwo_wins_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$HomeTeam_laligatwo == laligatwo_teams[i_laligatwo_wins_sim] & LALIGATWO_fixtures$laligatwo_FTR == "H" & LALIGATWO_fixtures$laligatwo_gamestatus =="notplayed",])
  laligatwo_away_wins_sim[i_laligatwo_wins_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$AwayTeam_laligatwo == laligatwo_teams[i_laligatwo_wins_sim] & LALIGATWO_fixtures$laligatwo_FTR == "A" & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",])
  laligatwo_home_draws_sim[i_laligatwo_wins_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$HomeTeam_laligatwo == laligatwo_teams[i_laligatwo_wins_sim] & LALIGATWO_fixtures$laligatwo_FTR == "D" & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",])
  laligatwo_away_draws_sim[i_laligatwo_wins_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$AwayTeam_laligatwo == laligatwo_teams[i_laligatwo_wins_sim] & LALIGATWO_fixtures$laligatwo_FTR == "D" & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",])
  laligatwo_home_loss_sim[i_laligatwo_wins_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$HomeTeam_laligatwo == laligatwo_teams[i_laligatwo_wins_sim] & LALIGATWO_fixtures$laligatwo_FTR == "A" & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",])
  laligatwo_away_loss_sim[i_laligatwo_wins_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$AwayTeam_laligatwo == laligatwo_teams[i_laligatwo_wins_sim] & LALIGATWO_fixtures$laligatwo_FTR == "H" & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed", ])

}

laligatwo_total_wins_sim <- laligatwo_home_wins_sim + laligatwo_away_wins_sim
laligatwo_total_draws_sim <- laligatwo_home_draws_sim + laligatwo_away_draws_sim
laligatwo_total_loss_sim <- laligatwo_home_loss_sim + laligatwo_away_loss_sim

laligatwo_home_games_sim <- c()
laligatwo_away_games_sim <-c()

for (i_laligatwo_sim in 1:length(laligatwo_teams))
{

  laligatwo_home_games_sim[i_laligatwo_sim] <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$HomeTeam_laligatwo == laligatwo_teams[i_laligatwo_sim] & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",])
  laligatwo_away_games_sim[i_laligatwo_sim]  <- nrow(LALIGATWO_fixtures[LALIGATWO_fixtures$AwayTeam_laligatwo == laligatwo_teams[i_laligatwo_sim] & LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",])

}

laligatwo_games_played_sim <- laligatwo_home_games_sim + laligatwo_away_games_sim

laligatwo_league_table_sim <- cbind(laligatwo_teams,laligatwo_games_played_sim,laligatwo_total_wins_sim,laligatwo_total_draws_sim,laligatwo_total_loss_sim)
laligatwo_PTS_sim <- (laligatwo_total_wins_sim*3) + (laligatwo_total_draws_sim*1)
laligatwo_league_table_sim <- cbind(laligatwo_league_table_sim,laligatwo_PTS_sim)

laligatwo_games_played_simfinal <- laligatwo_games_played + laligatwo_games_played_sim
laligatwo_total_wins_simfinal <- laligatwo_total_wins + laligatwo_total_wins_sim
laligatwo_total_draws_simfinal <- laligatwo_total_draws + laligatwo_total_draws_sim
laligatwo_total_loss_simfinal <- laligatwo_total_loss + laligatwo_total_loss_sim
laligatwo_PTS_simfinal <- laligatwo_PTS + laligatwo_PTS_sim

laligatwo_league_table_simfinal <- cbind(laligatwo_teams,laligatwo_games_played_simfinal,laligatwo_total_wins_simfinal,laligatwo_total_draws_simfinal,laligatwo_total_loss_simfinal,laligatwo_PTS_simfinal)
laligatwo_league_table_simfinal <- as.data.frame(laligatwo_league_table_simfinal)
names(laligatwo_league_table_simfinal)[names(laligatwo_league_table_simfinal) == "laligatwo_teams"] <- "Team_f"
names(laligatwo_league_table_simfinal)[names(laligatwo_league_table_simfinal) == "laligatwo_games_played_simfinal"] <- "P_f"
names(laligatwo_league_table_simfinal)[names(laligatwo_league_table_simfinal) == "laligatwo_total_wins_simfinal"] <- "W_f"
names(laligatwo_league_table_simfinal)[names(laligatwo_league_table_simfinal) == "laligatwo_total_draws_simfinal"] <- "D_f"
names(laligatwo_league_table_simfinal)[names(laligatwo_league_table_simfinal) == "laligatwo_total_loss_simfinal"] <- "L_f"
names(laligatwo_league_table_simfinal)[names(laligatwo_league_table_simfinal) == "laligatwo_PTS_simfinal"] <- "PTS_f"
points_laligatwo_sim <-  laligatwo_league_table_simfinal[order(as.numeric(laligatwo_league_table_simfinal$PTS_f), decreasing = TRUE),]

LALIGATWO_notplayed <- LALIGATWO_fixtures[LALIGATWO_fixtures$laligatwo_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#LALIGATWO
LALIGATWO_fixtures$Hometeam_laligatwo_index <- match(LALIGATWO_fixtures$HomeTeam_laligatwo,laligatwo_teams)
LALIGATWO_fixtures$Awayteam_laligatwo_index <- match(LALIGATWO_fixtures$AwayTeam_laligatwo,laligatwo_teams)
laligatwo_prediction <- c()
laligatwo_HWM <- c()
laligatwo_AWM <- c()
laligatwo_HWMLM <- c()
laligatwo_AWMLM <- c()
laligatwo_HY <- c()
laligatwo_AY <- c()
laligatwo_HCO <- c()
laligatwo_ACO <- c()
laligatwo_HXSC <- c()
laligatwo_AXSC <- c()
laligatwo_HYCPF <- c()
laligatwo_AYCPF <- c()
for(laligatwo_row in 1:nrow(LALIGATWO_fixtures))
{

  laligatwo_hometeamindex <- LALIGATWO_fixtures[laligatwo_row,"Hometeam_laligatwo_index"]
  laligatwo_awayteamindex <- LALIGATWO_fixtures[laligatwo_row,"Awayteam_laligatwo_index"]
  #analyse team form
  #home team
  laligatwo_form_vec_ht <- as.vector(laligatwo_form_h[laligatwo_hometeamindex,])
  laligatwo_form_vec_ht[is.na(laligatwo_form_vec_ht)] <- ""
  laligatwo_form_vec_ht <- laligatwo_form_vec_ht[laligatwo_form_vec_ht != ""]
  laligatwo_form_vec_ht  <-tail(laligatwo_form_vec_ht,6)
  laligatwo_ht_numberof_wins <- length(which(laligatwo_form_vec_ht == "W"))
  laligatwo_ht_numberof_draws <- length(which(laligatwo_form_vec_ht == "D"))
  laligatwo_ht_numberof_loss <- length(which(laligatwo_form_vec_ht == "L"))
  #awayteam
  laligatwo_form_vec_at <- as.vector(laligatwo_form_h[laligatwo_awayteamindex,])
  laligatwo_form_vec_at[is.na(laligatwo_form_vec_at)] <- ""
  laligatwo_form_vec_at <- laligatwo_form_vec_at[laligatwo_form_vec_at != ""]
  laligatwo_form_vec_at  <-tail(laligatwo_form_vec_at,6)
  laligatwo_at_numberof_wins <- length(which(laligatwo_form_vec_at == "W"))
  laligatwo_at_numberof_draws <- length(which(laligatwo_form_vec_at == "D"))
  laligatwo_at_numberof_loss <- length(which(laligatwo_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  laligatwo_goalscored_vec_ht <- as.vector(laligatwo_goalscored_h[laligatwo_hometeamindex,])
  laligatwo_goalscored_vec_ht[is.na(laligatwo_goalscored_vec_ht)] <- ""
  laligatwo_goalscored_vec_ht <- laligatwo_goalscored_vec_ht[laligatwo_goalscored_vec_ht != ""]
  laligatwo_goalscored_vec_ht  <-tail(laligatwo_goalscored_vec_ht,6)
  laligatwo_goalscored_vec_ht  <- as.numeric(laligatwo_goalscored_vec_ht)
  laligatwo_ht_totalgoalscored <- sum(laligatwo_goalscored_vec_ht)
  laligatwo_ht_matches_scoring <- length(which(laligatwo_goalscored_vec_ht > 0))
  laligatwo_ht_matches_without_scoring <- length(which(laligatwo_goalscored_vec_ht == "0"))
  #awayteam
  laligatwo_goalscored_vec_at <- as.vector(laligatwo_goalscored_h[laligatwo_awayteamindex,])
  laligatwo_goalscored_vec_at[is.na(laligatwo_goalscored_vec_at)] <- ""
  laligatwo_goalscored_vec_at <- laligatwo_goalscored_vec_at[laligatwo_goalscored_vec_at != ""]
  laligatwo_goalscored_vec_at  <-tail(laligatwo_goalscored_vec_at,6)
  laligatwo_goalscored_vec_at  <- as.numeric(laligatwo_goalscored_vec_at)
  laligatwo_at_totalgoalscored <- sum(laligatwo_goalscored_vec_at)
  laligatwo_at_matches_scoring <- length(which(laligatwo_goalscored_vec_at > 0))
  laligatwo_at_matches_without_scoring <- length(which(laligatwo_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  laligatwo_goalconceded_vec_ht <- as.vector(laligatwo_goalconceded_h[laligatwo_hometeamindex,])
  laligatwo_goalconceded_vec_ht[is.na(laligatwo_goalconceded_vec_ht)] <- ""
  laligatwo_goalconceded_vec_ht <- laligatwo_goalconceded_vec_ht[laligatwo_goalconceded_vec_ht != ""]
  laligatwo_goalconceded_vec_ht  <-tail(laligatwo_goalconceded_vec_ht,6)
  laligatwo_goalconceded_vec_ht  <- as.numeric(laligatwo_goalconceded_vec_ht)
  laligatwo_goalconceded_vec_ht
  laligatwo_ht_totalgoalconceded <- sum(laligatwo_goalconceded_vec_ht)
  laligatwo_ht_matches_concede <- length(which(laligatwo_goalconceded_vec_ht > 0))
  laligatwo_ht_matches_without_concede <- length(which(laligatwo_goalconceded_vec_ht == "0"))
  #awayteam
  laligatwo_goalconceded_vec_at <- as.vector(laligatwo_goalconceded_h[laligatwo_awayteamindex,])
  laligatwo_goalconceded_vec_at[is.na(laligatwo_goalconceded_vec_at)] <- ""
  laligatwo_goalconceded_vec_at <- laligatwo_goalconceded_vec_at[laligatwo_goalconceded_vec_at != ""]
  laligatwo_goalconceded_vec_at  <-tail(laligatwo_goalconceded_vec_at,6)
  laligatwo_goalconceded_vec_at  <- as.numeric(laligatwo_goalconceded_vec_at)
  laligatwo_at_totalgoalconceded <- sum(laligatwo_goalconceded_vec_at)
  laligatwo_at_matches_concede <- length(which(laligatwo_goalconceded_vec_at > 0))
  laligatwo_at_matches_without_concede <- length(which(laligatwo_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  laligatwo_totalgoals_vec_ht <- as.vector(laligatwo_totalgoals_h[laligatwo_hometeamindex,])
  laligatwo_totalgoals_vec_ht[is.na(laligatwo_totalgoals_vec_ht)] <- ""
  laligatwo_totalgoals_vec_ht <- laligatwo_totalgoals_vec_ht[laligatwo_totalgoals_vec_ht != ""]
  laligatwo_totalgoals_vec_ht  <-tail(laligatwo_totalgoals_vec_ht,6)
  laligatwo_totalgoals_vec_ht  <- as.numeric(laligatwo_totalgoals_vec_ht)
  laligatwo_totalgoals_vec_ht
  laligatwo_ht_totalgoals <- sum(laligatwo_totalgoals_vec_ht)
  laligatwo_ht_avgtotalgoals <- (laligatwo_ht_totalgoals/6)
  laligatwo_ht_no_of_ov25 <- length(which(laligatwo_totalgoals_vec_ht >= 3))
  laligatwo_ht_no_of_un25 <- length(which(laligatwo_totalgoals_vec_ht <= 2))
  #awayteam
  laligatwo_totalgoals_vec_at <- as.vector(laligatwo_totalgoals_h[laligatwo_awayteamindex,])
  laligatwo_totalgoals_vec_at[is.na(laligatwo_totalgoals_vec_at)] <- ""
  laligatwo_totalgoals_vec_at <- laligatwo_totalgoals_vec_at[laligatwo_totalgoals_vec_at != ""]
  laligatwo_totalgoals_vec_at  <-tail(laligatwo_totalgoals_vec_at,6)
  laligatwo_totalgoals_vec_at  <- as.numeric(laligatwo_totalgoals_vec_at)
  laligatwo_totalgoals_vec_at
  laligatwo_at_totalgoals <- sum(laligatwo_totalgoals_vec_at)
  laligatwo_at_avgtotalgoals <- (laligatwo_at_totalgoals/6)
  laligatwo_at_no_of_ov25 <- length(which(laligatwo_totalgoals_vec_at >= 3))
  laligatwo_at_no_of_un25 <- length(which(laligatwo_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  laligatwo_winmargin_vec_ht <- as.vector(laligatwo_winmargin_h[laligatwo_hometeamindex,])
  laligatwo_winmargin_vec_ht[is.na(laligatwo_winmargin_vec_ht)] <- ""
  laligatwo_winmargin_vec_ht <- laligatwo_winmargin_vec_ht[laligatwo_winmargin_vec_ht != ""]
  laligatwo_winmargin_vec_ht  <-tail(laligatwo_winmargin_vec_ht,6)
  laligatwo_winmargin_vec_ht  <- as.numeric(laligatwo_winmargin_vec_ht)

  laligatwo_ht_totalwinmargin <- sum(laligatwo_winmargin_vec_ht)
  laligatwo_ht_no_of_winmargin_ov0 <- length(which(laligatwo_winmargin_vec_ht >= 0))
  laligatwo_ht_no_of_winmargin_ov1 <- length(which(laligatwo_winmargin_vec_ht >= 1))
  laligatwo_ht_no_of_winmargin_un0 <- length(which(laligatwo_winmargin_vec_ht <= 0))
  laligatwo_ht_no_of_winmargin_un1 <- length(which(laligatwo_winmargin_vec_ht <= 1))
  #awayteam
  laligatwo_winmargin_vec_at <- as.vector(laligatwo_winmargin_h[laligatwo_awayteamindex,])
  laligatwo_winmargin_vec_at[is.na(laligatwo_winmargin_vec_at)] <- ""
  laligatwo_winmargin_vec_at <- laligatwo_winmargin_vec_at[laligatwo_winmargin_vec_at != ""]
  laligatwo_winmargin_vec_at  <-tail(laligatwo_winmargin_vec_at,6)
  laligatwo_winmargin_vec_at  <- as.numeric(laligatwo_winmargin_vec_at)

  laligatwo_at_totalwinmargin <- sum(laligatwo_winmargin_vec_at)
  laligatwo_at_no_of_winmargin_ov0 <- length(which(laligatwo_winmargin_vec_at >= 0))
  laligatwo_at_no_of_winmargin_ov1 <- length(which(laligatwo_winmargin_vec_at >= 1))
  laligatwo_at_no_of_winmargin_un0 <- length(which(laligatwo_winmargin_vec_at <= 0))
  laligatwo_at_no_of_winmargin_un1 <- length(which(laligatwo_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  laligatwo_winmargin_vec_ht_lm <- as.vector(laligatwo_winmargin_h[laligatwo_hometeamindex,])
  laligatwo_winmargin_vec_ht_lm[is.na(laligatwo_winmargin_vec_ht_lm)] <- ""
  laligatwo_winmargin_vec_ht_lm <- laligatwo_winmargin_vec_ht_lm[laligatwo_winmargin_vec_ht_lm != ""]
  laligatwo_winmargin_vec_ht_lm  <-tail(laligatwo_winmargin_vec_ht_lm,1)
  #awayteam
  laligatwo_winmargin_vec_at_lm <- as.vector(laligatwo_winmargin_h[laligatwo_awayteamindex,])
  laligatwo_winmargin_vec_at_lm[is.na(laligatwo_winmargin_vec_at_lm)] <- ""
  laligatwo_winmargin_vec_at_lm <- laligatwo_winmargin_vec_at_lm[laligatwo_winmargin_vec_at_lm != ""]
  laligatwo_winmargin_vec_at_lm  <-tail(laligatwo_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  laligatwo_yellowtotals_vec_ht <- as.vector(laligatwo_yellowtotalsv2[laligatwo_hometeamindex,])
  laligatwo_yellowtotals_vec_ht[is.na(laligatwo_yellowtotals_vec_ht)] <- ""
  laligatwo_yellowtotals_vec_ht <- laligatwo_yellowtotals_vec_ht[laligatwo_yellowtotals_vec_ht != ""]
  laligatwo_yellowtotals_vec_ht  <-tail(laligatwo_yellowtotals_vec_ht,1)
  #awayteam
  laligatwo_yellowtotals_vec_at <- as.vector(laligatwo_yellowtotalsv2[laligatwo_awayteamindex,])
  laligatwo_yellowtotals_vec_at[is.na(laligatwo_yellowtotals_vec_at)] <- ""
  laligatwo_yellowtotals_vec_at <- laligatwo_yellowtotals_vec_at[laligatwo_yellowtotals_vec_at != ""]
  laligatwo_yellowtotals_vec_at  <-tail(laligatwo_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  laligatwo_cornertotals_vec_ht <- as.vector(laligatwo_cornertotalsv2[laligatwo_hometeamindex,])
  laligatwo_cornertotals_vec_ht[is.na(laligatwo_cornertotals_vec_ht)] <- ""
  laligatwo_cornertotals_vec_ht <- laligatwo_cornertotals_vec_ht[laligatwo_cornertotals_vec_ht != ""]
  laligatwo_cornertotals_vec_ht  <-tail(laligatwo_cornertotals_vec_ht,1)
  #awayteam
  laligatwo_cornertotals_vec_at <- as.vector(laligatwo_cornertotalsv2[laligatwo_awayteamindex,])
  laligatwo_cornertotals_vec_at[is.na(laligatwo_cornertotals_vec_at)] <- ""
  laligatwo_cornertotals_vec_at <- laligatwo_cornertotals_vec_at[laligatwo_cornertotals_vec_at != ""]
  laligatwo_cornertotals_vec_at  <-tail(laligatwo_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  laligatwo_xshotsconversion_vec_ht <- as.vector(laligatwo_shots_analysis[laligatwo_hometeamindex,])
  laligatwo_xshotsconversion_vec_ht[is.na(laligatwo_xshotsconversion_vec_ht)] <- ""
  laligatwo_xshotsconversion_vec_ht <- laligatwo_xshotsconversion_vec_ht[laligatwo_xshotsconversion_vec_ht != ""]
  laligatwo_xshotsconversion_vec_ht  <-tail(laligatwo_xshotsconversion_vec_ht,1)
  #awayteam
  laligatwo_xshotsconversion_vec_at <- as.vector(laligatwo_shots_analysis[laligatwo_awayteamindex,])
  laligatwo_xshotsconversion_vec_at[is.na(laligatwo_xshotsconversion_vec_at)] <- ""
  laligatwo_xshotsconversion_vec_at <- laligatwo_xshotsconversion_vec_at[laligatwo_xshotsconversion_vec_at != ""]
  laligatwo_xshotsconversion_vec_at  <-tail(laligatwo_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  laligatwo_fouls_conversion_vec_ht <- as.vector(laligatwo_fouls_conversion[laligatwo_hometeamindex,])
  laligatwo_fouls_conversion_vec_ht[is.na(laligatwo_fouls_conversion_vec_ht)] <- ""
  laligatwo_fouls_conversion_vec_ht <- laligatwo_fouls_conversion_vec_ht[laligatwo_fouls_conversion_vec_ht != ""]
  laligatwo_fouls_conversion_vec_ht  <-tail(laligatwo_fouls_conversion_vec_ht,1)
  #awayteam
  laligatwo_fouls_conversion_vec_at <- as.vector(laligatwo_fouls_conversion[laligatwo_awayteamindex,])
  laligatwo_fouls_conversion_vec_at[is.na(laligatwo_fouls_conversion_vec_at)] <- ""
  laligatwo_fouls_conversion_vec_at <- laligatwo_fouls_conversion_vec_at[laligatwo_fouls_conversion_vec_at != ""]
  laligatwo_fouls_conversion_vec_at  <-tail(laligatwo_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  laligatwo_ht_last6points <- laligatwo_ht_numberof_wins*3 + laligatwo_ht_numberof_draws*1
  laligatwo_at_last6points <- laligatwo_at_numberof_wins*3 + laligatwo_at_numberof_draws*1

  if(laligatwo_ht_last6points > laligatwo_at_last6points) {laligatwo_3waypick <- "1"}  else {laligatwo_3waypick <- "X2"}

  if(laligatwo_at_last6points > laligatwo_ht_last6points ) {laligatwo_3waypick <- "2"} else {laligatwo_3waypick <- "1X"}

  if(laligatwo_ht_no_of_ov25 + laligatwo_at_no_of_ov25 >= 6) {laligatwo_goalspick <- "ov25"} else {laligatwo_goalspick <- "un25"}

  if(laligatwo_ht_no_of_un25 + laligatwo_at_no_of_un25 >= 6) {laligatwo_goalspick <- "un25"} else {laligatwo_goalspick <- "ov25"}

  if(laligatwo_ht_matches_scoring >= 4 && laligatwo_at_matches_scoring >=4) {laligatwo_btts <- "BTTS-Y"} else {laligatwo_btts <- "BTTS-N"}


  laligatwo_prediction[laligatwo_row] <- rbind(paste(laligatwo_3waypick,laligatwo_goalspick,laligatwo_btts,sep = ","))
  laligatwo_HWM[laligatwo_row] <- laligatwo_ht_totalwinmargin
  laligatwo_AWM[laligatwo_row] <- laligatwo_at_totalwinmargin

  laligatwo_HWMLM[laligatwo_row] <- laligatwo_winmargin_vec_ht_lm
  laligatwo_AWMLM[laligatwo_row] <- laligatwo_winmargin_vec_at_lm

  laligatwo_HY[laligatwo_row] <- laligatwo_yellowtotals_vec_ht
  laligatwo_AY[laligatwo_row] <- laligatwo_yellowtotals_vec_at

  laligatwo_HCO[laligatwo_row] <- laligatwo_cornertotals_vec_ht
  laligatwo_ACO[laligatwo_row] <- laligatwo_cornertotals_vec_at

  laligatwo_HXSC[laligatwo_row] <- laligatwo_xshotsconversion_vec_ht
  laligatwo_AXSC[laligatwo_row] <- laligatwo_xshotsconversion_vec_at

  laligatwo_HYCPF[laligatwo_row] <- laligatwo_fouls_conversion_vec_ht
  laligatwo_AYCPF[laligatwo_row] <- laligatwo_fouls_conversion_vec_at
}

laligatwo_prediction <- as.data.frame(laligatwo_prediction)
colnames(laligatwo_prediction) <- "prediction"

laligatwo_HWM <- as.data.frame(laligatwo_HWM)
colnames(laligatwo_HWM) <- "HWM"

laligatwo_AWM <- as.data.frame(laligatwo_AWM)
colnames(laligatwo_AWM) <- "AWM"

laligatwo_HWMLM <- as.data.frame(laligatwo_HWMLM)
colnames(laligatwo_HWMLM) <- "HWMLM"

laligatwo_AWMLM <- as.data.frame(laligatwo_AWMLM)
colnames(laligatwo_AWMLM) <- "AWMLM"

laligatwo_HY <- as.data.frame(laligatwo_HY)
colnames(laligatwo_HY) <- "AVGHY"

laligatwo_AY <- as.data.frame(laligatwo_AY)
colnames(laligatwo_AY) <- "AVGAY"

laligatwo_HCO <- as.data.frame(laligatwo_HCO)
colnames(laligatwo_HCO) <- "AVGHCO"

laligatwo_ACO <- as.data.frame(laligatwo_ACO)
colnames(laligatwo_ACO) <- "AVGACO"

laligatwo_HXSC <- as.data.frame(laligatwo_HXSC)
colnames(laligatwo_HXSC) <- "HXSC"

laligatwo_AXSC <- as.data.frame(laligatwo_AXSC)
colnames(laligatwo_AXSC) <- "AXSC"

laligatwo_HYCPF <- as.data.frame(laligatwo_HYCPF)
colnames(laligatwo_HYCPF) <- "HYCPF"

laligatwo_AYCPF <- as.data.frame(laligatwo_AYCPF)
colnames(laligatwo_AYCPF) <- "AYCPF"

laligatwo_picks <- cbind(LALIGATWO_fixtures$Div,LALIGATWO_fixtures$HomeTeam_laligatwo,LALIGATWO_fixtures$AwayTeam_laligatwo,laligatwo_prediction,laligatwo_HWM,laligatwo_AWM,laligatwo_HWMLM,laligatwo_AWMLM,laligatwo_HY,laligatwo_AY,laligatwo_HCO,laligatwo_ACO,laligatwo_HXSC,laligatwo_AXSC,laligatwo_HYCPF,laligatwo_AYCPF)

colnames(laligatwo_picks)[1] <- "picks_Div"
colnames(laligatwo_picks)[2] <- "picks_HomeTeam"
colnames(laligatwo_picks)[3] <- "picks_AwayTeam"
laligatwo_picks$matchid <- paste(laligatwo_picks$picks_HomeTeam,laligatwo_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of LALIGATWO
laligatwo_picks
#############################################################################################################################################################################
#clone fixtures
LALIGATWO_fixtures_clone <- LALIGATWO_fixtures
colnames(LALIGATWO_fixtures_clone)[61] <- "Hwin"
colnames(LALIGATWO_fixtures_clone)[62] <- "Draw"
colnames(LALIGATWO_fixtures_clone)[63] <- "Awin"

LALIGATWO_fixtures_clone$Hwinodds <-   LALIGATWO_fixtures$laligatwo_1_0 + LALIGATWO_fixtures$laligatwo_2_0 + LALIGATWO_fixtures$laligatwo_2_1 + LALIGATWO_fixtures$laligatwo_3_0 + LALIGATWO_fixtures$laligatwo_3_1 +
  LALIGATWO_fixtures$laligatwo_3_2 + LALIGATWO_fixtures$laligatwo_4_0 + LALIGATWO_fixtures$laligatwo_4_1 + LALIGATWO_fixtures$laligatwo_4_2 + LALIGATWO_fixtures$laligatwo_4_3 +
  LALIGATWO_fixtures$laligatwo_5_0 + LALIGATWO_fixtures$laligatwo_5_1 + LALIGATWO_fixtures$laligatwo_5_2 + LALIGATWO_fixtures$laligatwo_5_3 + LALIGATWO_fixtures$laligatwo_5_4 +
  LALIGATWO_fixtures$laligatwo_6_0 + LALIGATWO_fixtures$laligatwo_6_1 + LALIGATWO_fixtures$laligatwo_6_2 + LALIGATWO_fixtures$laligatwo_6_3 + LALIGATWO_fixtures$laligatwo_6_4 +
  LALIGATWO_fixtures$laligatwo_6_5
LALIGATWO_fixtures_clone$Hwinodds <- round(1/LALIGATWO_fixtures_clone$Hwinodds, digits = 3)

LALIGATWO_fixtures_clone$Drawodds <-  LALIGATWO_fixtures$laligatwo_0_0 + LALIGATWO_fixtures$laligatwo_1_1 + LALIGATWO_fixtures$laligatwo_2_2 + LALIGATWO_fixtures$laligatwo_3_3 + LALIGATWO_fixtures$laligatwo_4_4 +
  LALIGATWO_fixtures$laligatwo_5_5 + LALIGATWO_fixtures$laligatwo_6_6

LALIGATWO_fixtures_clone$Drawodds <- round(1/LALIGATWO_fixtures_clone$Drawodds, digits = 3)

LALIGATWO_fixtures_clone$Awinodds <-   LALIGATWO_fixtures$laligatwo_0_1 + LALIGATWO_fixtures$laligatwo_0_2 + LALIGATWO_fixtures$laligatwo_1_2 + LALIGATWO_fixtures$laligatwo_0_3 + LALIGATWO_fixtures$laligatwo_1_3 +
  LALIGATWO_fixtures$laligatwo_2_3 + LALIGATWO_fixtures$laligatwo_0_4 + LALIGATWO_fixtures$laligatwo_1_4 + LALIGATWO_fixtures$laligatwo_2_4 + LALIGATWO_fixtures$laligatwo_3_4 +
  LALIGATWO_fixtures$laligatwo_0_5 + LALIGATWO_fixtures$laligatwo_1_5 + LALIGATWO_fixtures$laligatwo_2_5 + LALIGATWO_fixtures$laligatwo_3_5 + LALIGATWO_fixtures$laligatwo_4_5 +
  LALIGATWO_fixtures$laligatwo_0_6 + LALIGATWO_fixtures$laligatwo_1_6 + LALIGATWO_fixtures$laligatwo_2_6 + LALIGATWO_fixtures$laligatwo_3_6 + LALIGATWO_fixtures$laligatwo_4_6 +
  LALIGATWO_fixtures$laligatwo_5_6

LALIGATWO_fixtures_clone$Awinodds <- round(1/LALIGATWO_fixtures_clone$Awinodds, digits = 3)

colnames(LALIGATWO_fixtures_clone)[15] <- "CS_1-1"
colnames(LALIGATWO_fixtures_clone)[13] <- "CS_1-0"
colnames(LALIGATWO_fixtures_clone)[14] <- "CS_0-1"
colnames(LALIGATWO_fixtures_clone)[16] <- "CS_2-0"
colnames(LALIGATWO_fixtures_clone)[17] <- "CS_0-2"
colnames(LALIGATWO_fixtures_clone)[19] <- "CS_2-1"
colnames(LALIGATWO_fixtures_clone)[20] <- "CS_1-2"

LALIGATWO_fixtures_clone$`CS_1-1` <- round(1/LALIGATWO_fixtures_clone$`CS_1-1`, digits = 3)
LALIGATWO_fixtures_clone$`CS_1-0` <- round(1/LALIGATWO_fixtures_clone$`CS_1-0`, digits = 3)
LALIGATWO_fixtures_clone$`CS_0-1` <- round(1/LALIGATWO_fixtures_clone$`CS_0-1`, digits = 3)
LALIGATWO_fixtures_clone$`CS_2-0` <- round(1/LALIGATWO_fixtures_clone$`CS_2-0`, digits = 3)
LALIGATWO_fixtures_clone$`CS_0-2` <- round(1/LALIGATWO_fixtures_clone$`CS_0-2`, digits = 3)
LALIGATWO_fixtures_clone$`CS_2-1` <- round(1/LALIGATWO_fixtures_clone$`CS_2-1`, digits = 3)
LALIGATWO_fixtures_clone$`CS_1-2` <- round(1/LALIGATWO_fixtures_clone$`CS_1-2`, digits = 3)

colnames(LALIGATWO_fixtures_clone)[1] <- "league"
colnames(LALIGATWO_fixtures_clone)[2] <- "Hometeam"
colnames(LALIGATWO_fixtures_clone)[3] <- "Awayteam"
colnames(LALIGATWO_fixtures_clone)[92] <- "predscore"
colnames(LALIGATWO_fixtures_clone)[64] <- "ov25"
colnames(LALIGATWO_fixtures_clone)[66] <- "ov25odds"
colnames(LALIGATWO_fixtures_clone)[65] <- "un25"
colnames(LALIGATWO_fixtures_clone)[67] <- "un25odds"
colnames(LALIGATWO_fixtures_clone)[68] <- "BTTSY"
colnames(LALIGATWO_fixtures_clone)[69] <- "BTTSN"
colnames(LALIGATWO_fixtures_clone)[70] <- "BTTSYodds"
colnames(LALIGATWO_fixtures_clone)[71] <- "BTTSNodds"

LALIGATWO_fixtures_clone <- LALIGATWO_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
LALIGATWO_fixtures_clone$matchid <- paste(LALIGATWO_fixtures_clone$Hometeam,LALIGATWO_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
LALIGATWO_fixtures_clone_final <- LALIGATWO_fixtures_clone[,-c(8,9,10,27)]
LALIGATWO_fixtures_clone_final[,'sep'] <- ''

laligatwo_dmprediction <-  laligatwo_picks[,c(4,5,6,7,8)]
laligatwo_dmprediction[,'sep2'] <- ''

laligatwo_avgyellow <- laligatwo_picks[,c(9,10)]
laligatwo_avgyellow[,'sep3'] <- ''

laligatwo_avgcorners <- laligatwo_picks[,c(11,12)]
laligatwo_avgcorners[,'sep4'] <- ''

laligatwo_goals <- LALIGATWO_fixtures[,c(10,11)]
laligatwo_goals$laligatwo_xGH <- round(laligatwo_goals$laligatwo_xGH, digits = 2)
laligatwo_goals$laligatwo_xGA <- round(laligatwo_goals$laligatwo_xGA, digits = 2)
laligatwo_goals$laligatwo_TxG <- laligatwo_goals$laligatwo_xGH + laligatwo_goals$laligatwo_xGA
laligatwo_goals[,'sep5'] <- ''

laligatwo_shots <- LALIGATWO_fixtures_sot[,c(10,11)]
laligatwo_shots$laligatwo_xHST <- round(laligatwo_shots$laligatwo_xHST, digits = 2)
laligatwo_shots$laligatwo_xAST <- round(laligatwo_shots$laligatwo_xAST, digits = 2)
laligatwo_shots$TxSOT <- laligatwo_shots$laligatwo_xHST + laligatwo_shots$laligatwo_xAST
laligatwo_shots[,'sep6'] <- ''

laligatwo_fouls <- LALIGATWO_fixtures_fo[,c(10,11)]
laligatwo_fouls$laligatwo_xHF <- round(laligatwo_fouls$laligatwo_xHF, digits = 2)
laligatwo_fouls$laligatwo_xAF <- round(laligatwo_fouls$laligatwo_xAF, digits = 2)
laligatwo_fouls$laligatwo_TxF <- laligatwo_fouls$laligatwo_xHF + laligatwo_fouls$laligatwo_xAF

laligatwo_ycpf <- laligatwo_picks[,c(15,16)]
laligatwo_fouls <- cbind(laligatwo_fouls,laligatwo_ycpf)
laligatwo_fouls$HYCPF <- as.numeric(laligatwo_fouls$HYCPF)
laligatwo_fouls$AYCPF <- as.numeric(laligatwo_fouls$AYCPF)
laligatwo_fouls$x_hyc <- (laligatwo_fouls$laligatwo_xHF) * (laligatwo_fouls$HYCPF)
laligatwo_fouls$x_ayc <- (laligatwo_fouls$laligatwo_xAF) * (laligatwo_fouls$AYCPF)
laligatwo_fouls$x_TYC <- round((laligatwo_fouls$x_hyc + laligatwo_fouls$x_ayc),digits = 2)
laligatwo_fouls[,'sep7'] <- ''

laligatwo_bookings <- LALIGATWO_fixtures_yc[,c(10,11)]
laligatwo_bookings$laligatwo_xHYC <- round(laligatwo_bookings$laligatwo_xHYC, digits = 2)
laligatwo_bookings$laligatwo_xAYC <- round(laligatwo_bookings$laligatwo_xAYC, digits = 2)
laligatwo_bookings$laligatwo_TYcards <- laligatwo_bookings$laligatwo_xHYC + laligatwo_bookings$laligatwo_xAYC
laligatwo_bookings[,'sep8'] <- ''

laligatwo_corners <- LALIGATWO_fixtures_co[,c(10,11)]
laligatwo_corners$laligatwo_xHCOC <- round(laligatwo_corners$laligatwo_xHCOC, digits = 2)
laligatwo_corners$laligatwo_xACOC <- round(laligatwo_corners$laligatwo_xACOC, digits = 2)
laligatwo_corners$laligatwo_TCOs <- laligatwo_corners$laligatwo_xHCOC + laligatwo_corners$laligatwo_xACOC
laligatwo_corners[,'sep9'] <- ''

laligatwo_shotsconversion <- laligatwo_picks[,c(13,14)]
laligatwo_shotsconversion <- cbind(laligatwo_shotsconversion,laligatwo_shots)
laligatwo_shotsconversion$HXSC <- as.numeric(laligatwo_shotsconversion$HXSC)
laligatwo_shotsconversion$AXSC <- as.numeric(laligatwo_shotsconversion$AXSC)
laligatwo_shotsconversion$laligatwo_hXgoals <- round((laligatwo_shotsconversion$HXSC * laligatwo_shotsconversion$laligatwo_xHST), digits = 2)
laligatwo_shotsconversion$laligatwo_aXgoals <- round((laligatwo_shotsconversion$AXSC * laligatwo_shotsconversion$laligatwo_xAST), digits = 2)
laligatwo_shotsconversion$Xgoals <- laligatwo_shotsconversion$laligatwo_hXgoals + laligatwo_shotsconversion$laligatwo_aXgoals
options(java.parameters = "-Xmx4g")
LALIGATWO_all <- cbind(LALIGATWO_fixtures_clone_final,laligatwo_dmprediction,laligatwo_avgyellow,laligatwo_avgcorners,laligatwo_goals,laligatwo_shots,laligatwo_fouls,laligatwo_bookings,laligatwo_corners,laligatwo_shotsconversion)
unlink('Divisions/LALIGATWO.xlsx')
write.xlsx(LALIGATWO_all,'Divisions/LALIGATWO.xlsx', sheetName = "LALIGATWO_all", append = TRUE)
write.xlsx(points_laligatwo,'Divisions/LALIGATWO.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(laligatwo_cornertotalsv2,'Divisions/LALIGATWO.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(laligatwo_goaltotalsv2,'Divisions/LALIGATWO.xlsx', sheetName = "Goaltotals", append = TRUE)


write.csv(LALIGATWO_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
