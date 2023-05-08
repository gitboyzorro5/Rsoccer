#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')

first_df <- E0_rounds[E0_rounds$e0_matchday >= 27,]
second_df <- SP1_rounds[SP1_rounds$sp1_matchday >=25,]
first_df <- first_df[,-37]
second_df <- second_df[,-37]

UEFACL <- rbind(first_df,second_df)

#UEFACL <- F1_rounds[F1_rounds$f1_matchday >= 27,]
#goaltotals v2
uefacl_goaltotalsv2 <- tapply(UEFACL$TG, UEFACL[c("HomeTeam", "AwayTeam")],mean)
uefacl_hgtotals <- rowSums(uefacl_goaltotalsv2, na.rm = T)
uefacl_agtotals <- colSums(uefacl_goaltotalsv2, na.rm = T)
uefacl_goaltotalsv2 <- cbind(uefacl_goaltotalsv2,uefacl_hgtotals,uefacl_agtotals)
uefacl_totalgoals <- uefacl_hgtotals + uefacl_agtotals
uefacl_goaltotalsv2 <- cbind(uefacl_goaltotalsv2,uefacl_totalgoals)
uefacl_teams <- sort(unique(UEFACL$HomeTeam))
uefacl_home_games <- c()
uefacl_away_games <-c()
for (i_uefacl in 1:length(uefacl_teams))
{

  uefacl_home_games[i_uefacl] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl],])
  uefacl_away_games[i_uefacl]  <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl],])

}
uefacl_games_played <- uefacl_home_games + uefacl_away_games
uefacl_goaltotalsv2 <- cbind(uefacl_goaltotalsv2,uefacl_games_played)
uefacl_avg_totalgoals <- round((uefacl_totalgoals/ uefacl_games_played), digits = 4)
uefacl_goaltotalsv2[is.na(uefacl_goaltotalsv2)] <- ""
uefacl_goaltotalsv2 <- cbind(uefacl_goaltotalsv2,uefacl_avg_totalgoals)

############################################################################################################
#Cornertotals v2
uefacl_cornertotalsv2 <- tapply(UEFACL$TC, UEFACL[c("HomeTeam", "AwayTeam")],mean)
uefacl_hcototals <- rowSums(uefacl_cornertotalsv2, na.rm = T)
uefacl_acototals <- colSums(uefacl_cornertotalsv2, na.rm = T)
uefacl_cornertotalsv2 <- cbind(uefacl_cornertotalsv2,uefacl_hcototals,uefacl_acototals)
uefacl_totalcorners <- uefacl_hcototals + uefacl_acototals
uefacl_cornertotalsv2 <- cbind(uefacl_cornertotalsv2,uefacl_totalcorners)
uefacl_cornertotalsv2 <- cbind(uefacl_cornertotalsv2,uefacl_games_played)
uefacl_avg_totalcorners <- round((uefacl_totalcorners/ uefacl_games_played), digits = 4)
uefacl_cornertotalsv2[is.na(uefacl_cornertotalsv2)] <- ""
uefacl_cornertotalsv2 <- cbind(uefacl_cornertotalsv2,uefacl_avg_totalcorners)
############################################################################################################
#GS matrix
uefacl_goalscored_h <- tapply(UEFACL$FTHG, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_goalscored_a <- tapply(UEFACL$FTAG, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_goalscored_h[is.na(uefacl_goalscored_h)] <- ""
uefacl_goalscored_a[is.na(uefacl_goalscored_a)] <- ""
for(uefacl_rowhgs in 1:nrow(uefacl_goalscored_h)) {
  for(uefacl_colhgs in 1:ncol(uefacl_goalscored_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowags in 1:nrow(uefacl_goalscored_a)) {
      for(uefacl_colags in 1:ncol(uefacl_goalscored_a)) {
        ifelse(!uefacl_goalscored_a[uefacl_rowags,uefacl_colags]=="",uefacl_goalscored_h[uefacl_rowags,uefacl_colags] <- uefacl_goalscored_a[uefacl_rowags,uefacl_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
uefacl_goalconceded_h <- tapply(UEFACL$FTAG, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_goalconceded_a <- tapply(UEFACL$FTHG, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_goalconceded_h[is.na(uefacl_goalconceded_h)] <- ""
uefacl_goalconceded_a[is.na(uefacl_goalconceded_a)] <- ""
for(uefacl_rowhgc in 1:nrow(uefacl_goalconceded_h)) {
  for(uefacl_colhgc in 1:ncol(uefacl_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowagc in 1:nrow(uefacl_goalconceded_a)) {
      for(uefacl_colagc in 1:ncol(uefacl_goalconceded_a)) {
        ifelse(!uefacl_goalconceded_a[uefacl_rowagc,uefacl_colagc]=="",uefacl_goalconceded_h[uefacl_rowagc,uefacl_colagc] <- uefacl_goalconceded_a[uefacl_rowagc,uefacl_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
uefacl_totalcorners_h <- tapply(UEFACL$TC, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_totalcorners_a <- tapply(UEFACL$TC, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_totalcorners_h[is.na(uefacl_totalcorners_h)] <- ""
uefacl_totalcorners_a[is.na(uefacl_totalcorners_a)] <- ""
#UEFACL
for(uefacl_rowTC in 1:nrow(uefacl_totalcorners_h)) {
  for(uefacl_colTC in 1:ncol(uefacl_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowTC in 1:nrow(uefacl_totalcorners_a)) {
      for(uefacl_colTC in 1:ncol(uefacl_totalcorners_a)) {
        ifelse(!uefacl_totalcorners_a[uefacl_rowTC,uefacl_colTC]=="",uefacl_totalcorners_h[uefacl_rowTC,uefacl_colTC] <- uefacl_totalcorners_a[uefacl_rowTC,uefacl_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
uefacl_coawarded_h <- tapply(UEFACL$HCO, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_coawarded_a <- tapply(UEFACL$ACO, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_coawarded_h[is.na(uefacl_coawarded_h)] <- ""
uefacl_coawarded_a[is.na(uefacl_coawarded_a)] <- ""
#UEFACL
for(uefacl_rowhco in 1:nrow(uefacl_coawarded_h)) {
  for(uefacl_colhco in 1:ncol(uefacl_coawarded_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowaco in 1:nrow(uefacl_coawarded_a)) {
      for(uefacl_colaco in 1:ncol(uefacl_coawarded_a)) {
        ifelse(!uefacl_coawarded_a[uefacl_rowaco,uefacl_colaco]=="",uefacl_coawarded_h[uefacl_rowaco,uefacl_colaco] <- uefacl_coawarded_a[uefacl_rowaco,uefacl_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
uefacl_cornersconceded_h <- tapply(UEFACL$ACO, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_cornersconceded_a <- tapply(UEFACL$HCO, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_cornersconceded_h[is.na(uefacl_cornersconceded_h)] <- ""
uefacl_cornersconceded_a[is.na(uefacl_cornersconceded_a)] <- ""
#UEFACL
for(uefacl_rowhcc in 1:nrow(uefacl_cornersconceded_h)) {
  for(uefacl_colhcc in 1:ncol(uefacl_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowacc in 1:nrow(uefacl_cornersconceded_a)) {
      for(uefacl_colacc in 1:ncol(uefacl_cornersconceded_a)) {
        ifelse(!uefacl_cornersconceded_a[uefacl_rowacc,uefacl_colacc]=="",uefacl_cornersconceded_h[uefacl_rowacc,uefacl_colacc] <- uefacl_cornersconceded_a[uefacl_rowacc,uefacl_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
uefacl_coscform_h <- tapply(UEFACL$COSC, UEFACL[c("HomeTeam", "Date")],median)
uefacl_coscform_a <- tapply(UEFACL$COSC, UEFACL[c("AwayTeam", "Date")],median)
uefacl_coscform_h[is.na(uefacl_coscform_h)] <- ""
uefacl_coscform_a[is.na(uefacl_coscform_a)] <- ""
#UEFACL
for(uefacl_rowh_f_cosc in 1:nrow(uefacl_coscform_h)) {
  for(uefacl_colh_f_cosc in 1:ncol(uefacl_coscform_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowa_f_cosc in 1:nrow(uefacl_coscform_a)) {
      for(uefacl_cola_f_cosc in 1:ncol(uefacl_coscform_a)) {
        ifelse(!uefacl_coscform_a[uefacl_rowa_f_cosc,uefacl_cola_f_cosc]=="",uefacl_coscform_h[uefacl_rowa_f_cosc,uefacl_cola_f_cosc] <- uefacl_coscform_a[uefacl_rowa_f_cosc,uefacl_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
uefacl_winmargin_h <- tapply(UEFACL$FTHG - UEFACL$FTAG, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_winmargin_a <- tapply(UEFACL$FTAG - UEFACL$FTHG, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_winmargin_h[is.na(uefacl_winmargin_h)] <- ""
uefacl_winmargin_a[is.na(uefacl_winmargin_a)] <- ""
#UEFACL
for(uefacl_rowhwm in 1:nrow(uefacl_winmargin_h)) {
  for(uefacl_colhwm in 1:ncol(uefacl_winmargin_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowawm in 1:nrow(uefacl_winmargin_a)) {
      for(uefacl_colawm in 1:ncol(uefacl_winmargin_a)) {
        ifelse(!uefacl_winmargin_a[uefacl_rowawm,uefacl_colawm]=="",uefacl_winmargin_h[uefacl_rowawm,uefacl_colawm] <- uefacl_winmargin_a[uefacl_rowawm,uefacl_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
uefacl_yellowscored_h <- tapply(UEFACL$HY, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_yellowscored_a <- tapply(UEFACL$AY, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_yellowscored_h[is.na(uefacl_yellowscored_h)] <- ""
uefacl_yellowscored_a[is.na(uefacl_yellowscored_a)] <- ""
#UEFACL
for(uefacl_rowhys in 1:nrow(uefacl_yellowscored_h)) {
  for(uefacl_colhys in 1:ncol(uefacl_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(uefacl_roways in 1:nrow(uefacl_yellowscored_a)) {
      for(uefacl_colays in 1:ncol(uefacl_yellowscored_a)) {
        ifelse(!uefacl_yellowscored_a[uefacl_roways,uefacl_colays]=="",uefacl_yellowscored_h[uefacl_roways,uefacl_colays] <- uefacl_yellowscored_a[uefacl_roways,uefacl_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
uefacl_redscored_h <- tapply(UEFACL$HR, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_redscored_a <- tapply(UEFACL$AR, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_redscored_h[is.na(uefacl_redscored_h)] <- ""
uefacl_redscored_a[is.na(uefacl_redscored_a)] <- ""
for(uefacl_rowhrs in 1:nrow(uefacl_redscored_h)) {
  for(uefacl_colhrs in 1:ncol(uefacl_redscored_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowars in 1:nrow(uefacl_redscored_a)) {
      for(uefacl_colars in 1:ncol(uefacl_redscored_a)) {
        ifelse(!uefacl_redscored_a[uefacl_rowars,uefacl_colars]=="",uefacl_redscored_h[uefacl_rowars,uefacl_colars] <- uefacl_redscored_a[uefacl_rowars,uefacl_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
uefacl_redtotalsv2 <- tapply(UEFACL$TR, UEFACL[c("HomeTeam", "AwayTeam")],mean)
uefacl_hrtotals <- rowSums(uefacl_redtotalsv2, na.rm = T)
uefacl_artotals <- colSums(uefacl_redtotalsv2, na.rm = T)
uefacl_redtotalsv2 <- cbind(uefacl_redtotalsv2,uefacl_hrtotals,uefacl_artotals)
uefacl_totalreds <- uefacl_hrtotals + uefacl_artotals
uefacl_redtotalsv2 <- cbind(uefacl_redtotalsv2,uefacl_totalreds)
uefacl_redtotalsv2 <- cbind(uefacl_redtotalsv2,uefacl_games_played)
uefacl_avg_totalreds <- round((uefacl_totalreds/ uefacl_games_played), digits = 4)
uefacl_redtotalsv2[is.na(uefacl_redtotalsv2)] <- ""
uefacl_redtotalsv2 <- cbind(uefacl_redtotalsv2,uefacl_avg_totalreds)
############################################################################################################################################################
#yellowtotals
uefacl_yellowtotalsv2 <- tapply(UEFACL$TY, UEFACL[c("HomeTeam", "AwayTeam")],mean)
uefacl_hytotals <- rowSums(uefacl_yellowtotalsv2, na.rm = T)
uefacl_aytotals <- colSums(uefacl_yellowtotalsv2, na.rm = T)
uefacl_yellowtotalsv2 <- cbind(uefacl_yellowtotalsv2,uefacl_hytotals,uefacl_aytotals)
uefacl_totalyellows <- uefacl_hytotals + uefacl_aytotals
uefacl_yellowtotalsv2 <- cbind(uefacl_yellowtotalsv2,uefacl_totalyellows)
uefacl_yellowtotalsv2 <- cbind(uefacl_yellowtotalsv2,uefacl_games_played)
uefacl_avg_totalyellows <- round((uefacl_totalyellows/ uefacl_games_played), digits = 4)
uefacl_yellowtotalsv2[is.na(uefacl_yellowtotalsv2)] <- ""
uefacl_yellowtotalsv2 <- cbind(uefacl_yellowtotalsv2,uefacl_avg_totalyellows)
##################################################################################################################################################
#team form
uefacl_form_h <- tapply(UEFACL$FTR, UEFACL[c("HomeTeam", "Date")],median)
uefacl_form_a <- tapply(UEFACL$FTR, UEFACL[c("AwayTeam", "Date")],median)
uefacl_form_h[is.na(uefacl_form_h)] <- ""
uefacl_form_a[is.na(uefacl_form_a)] <- ""
uefacl_form_h <- sub("A","L",uefacl_form_h)
uefacl_form_h <- sub("H","W",uefacl_form_h)
uefacl_form_a <- sub("A","W",uefacl_form_a)
uefacl_form_a <- sub("H","L",uefacl_form_a)
for(uefacl_rowh_f in 1:nrow(uefacl_form_h)) {
  for(uefacl_colh_f in 1:ncol(uefacl_form_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowa_f in 1:nrow(uefacl_form_a)) {
      for(uefacl_cola_f in 1:ncol(uefacl_form_a)) {
        ifelse(!uefacl_form_a[uefacl_rowa_f,uefacl_cola_f]=="",uefacl_form_h[uefacl_rowa_f,uefacl_cola_f] <- uefacl_form_a[uefacl_rowa_f,uefacl_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
uefacl_csform_h <- tapply(UEFACL$CS, UEFACL[c("HomeTeam", "Date")],median)
uefacl_csform_a <- tapply(UEFACL$CS, UEFACL[c("AwayTeam", "Date")],median)
uefacl_csform_h[is.na(uefacl_csform_h)] <- ""
uefacl_csform_a[is.na(uefacl_csform_a)] <- ""
#UEFACL
for(uefacl_rowh_f_cs in 1:nrow(uefacl_csform_h)) {
  for(uefacl_colh_f_cs in 1:ncol(uefacl_csform_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowa_f_cs in 1:nrow(uefacl_csform_a)) {
      for(uefacl_cola_f_cs in 1:ncol(uefacl_csform_a)) {
        ifelse(!uefacl_csform_a[uefacl_rowa_f_cs,uefacl_cola_f_cs]=="",uefacl_csform_h[uefacl_rowa_f_cs,uefacl_cola_f_cs] <- uefacl_csform_a[uefacl_rowa_f_cs,uefacl_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
uefacl_totalgoals_h <- tapply(UEFACL$TG, UEFACL[c("HomeTeam", "Date")],mean)
uefacl_totalgoals_a <- tapply(UEFACL$TG, UEFACL[c("AwayTeam", "Date")],mean)
uefacl_totalgoals_h[is.na(uefacl_totalgoals_h)] <- ""
uefacl_totalgoals_a[is.na(uefacl_totalgoals_a)] <- ""
for(uefacl_rowh in 1:nrow(uefacl_totalgoals_h)) {
  for(uefacl_colh in 1:ncol(uefacl_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowa in 1:nrow(uefacl_totalgoals_a)) {
      for(uefacl_cola in 1:ncol(uefacl_totalgoals_a)) {
        ifelse(!uefacl_totalgoals_a[uefacl_rowa,uefacl_cola]=="",uefacl_totalgoals_h[uefacl_rowa,uefacl_cola] <- uefacl_totalgoals_a[uefacl_rowa,uefacl_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
#UEFACL
uefacl_un05_home <- c()
uefacl_un05_away <- c()
uefacl_ov05_home <- c()
uefacl_ov05_away <- c()

uefacl_un15_home <- c()
uefacl_un15_away <- c()
uefacl_ov15_home <- c()
uefacl_ov15_away <- c()

uefacl_un25_home <- c()
uefacl_un25_away <- c()
uefacl_ov25_home <- c()
uefacl_ov25_away <- c()

uefacl_un35_home <- c()
uefacl_un35_away <- c()
uefacl_ov35_home <- c()
uefacl_ov35_away <- c()

uefacl_un45_home <- c()
uefacl_un45_away <- c()
uefacl_ov45_home <- c()
uefacl_ov45_away <- c()

uefacl_un55_home <- c()
uefacl_un55_away <- c()
uefacl_ov55_home <- c()
uefacl_ov55_away <- c()

for (i_uefacl_tg in 1:length(uefacl_teams))
{

  uefacl_un05_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG == 0,])
  uefacl_un05_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG == 0,])

  uefacl_ov05_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG > 0,])
  uefacl_ov05_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG > 0,])

  uefacl_un15_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 1,])
  uefacl_un15_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 1,])

  uefacl_ov15_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 2,])
  uefacl_ov15_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 2,])

  uefacl_un25_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 2,])
  uefacl_un25_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 2,])

  uefacl_ov25_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >=3,])
  uefacl_ov25_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >=3,])

  uefacl_un35_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 3,])
  uefacl_un35_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 3,])

  uefacl_ov35_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 4,])
  uefacl_ov35_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 4,])

  uefacl_un45_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 4,])
  uefacl_un45_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 4,])

  uefacl_ov45_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 5,])
  uefacl_ov45_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 5,])

  uefacl_un55_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 5,])
  uefacl_un55_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG <= 5,])

  uefacl_ov55_home[i_uefacl_tg] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 6,])
  uefacl_ov55_away[i_uefacl_tg] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_tg] & UEFACL$TG >= 6,])


}

uefacl_un05 <- uefacl_un05_home + uefacl_un05_away
uefacl_ov05 <- uefacl_ov05_home + uefacl_ov05_away

uefacl_un15 <- uefacl_un15_home + uefacl_un15_away
uefacl_ov15 <- uefacl_ov15_home + uefacl_ov15_away

uefacl_un25 <- uefacl_un25_home + uefacl_un25_away
uefacl_ov25 <- uefacl_ov25_home + uefacl_ov25_away

uefacl_un35 <- uefacl_un35_home + uefacl_un35_away
uefacl_ov35 <- uefacl_ov35_home + uefacl_ov35_away

uefacl_un45 <- uefacl_un45_home + uefacl_un45_away
uefacl_ov45 <- uefacl_ov45_home + uefacl_ov45_away

uefacl_un55 <- uefacl_un55_home + uefacl_un55_away
uefacl_ov55 <- uefacl_ov55_home + uefacl_ov55_away

uefacl_ovundata <- cbind(uefacl_teams,uefacl_un05,uefacl_ov05,uefacl_un15,uefacl_ov15,uefacl_un25,uefacl_ov25,uefacl_un35,uefacl_ov35,uefacl_un45,uefacl_ov45,uefacl_un55,uefacl_ov55)
#################################################################################################################################################################
#team against
uefacl_form_team_against_h <- tapply(UEFACL$AwayTeam, UEFACL[c("HomeTeam", "Date")],median)
uefacl_form_team_against_a <- tapply(UEFACL$HomeTeam, UEFACL[c("AwayTeam", "Date")],median)
uefacl_form_team_against_h[is.na(uefacl_form_team_against_h)] <- ""
uefacl_form_team_against_a[is.na(uefacl_form_team_against_a)] <- ""
#UEFACL
for(uefacl_rowh_f_against in 1:nrow(uefacl_form_team_against_h)) {
  for(uefacl_colh_f_against in 1:ncol(uefacl_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(uefacl_rowa_f_against in 1:nrow(uefacl_form_team_against_a)) {
      for(uefacl_cola_f_against in 1:ncol(uefacl_form_team_against_a)) {
        ifelse(!uefacl_form_team_against_a[uefacl_rowa_f_against,uefacl_cola_f_against]=="",uefacl_form_team_against_h[uefacl_rowa_f_against,uefacl_cola_f_against] <- uefacl_form_team_against_a[uefacl_rowa_f_against,uefacl_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#UEFACL
#home goals scored
uefacl_home_gs <- aggregate(UEFACL$FTHG, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_home_gs_avg <- aggregate(UEFACL$FTHG, by = list(UEFACL$HomeTeam),mean)
uefacl_home_scoring <- merge(uefacl_home_gs,uefacl_home_gs_avg, by='Group.1',all = T)
names(uefacl_home_scoring)[names(uefacl_home_scoring) == "x.x"] <- "TFthg"
names(uefacl_home_scoring)[names(uefacl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
uefacl_away_gs <- aggregate(UEFACL$FTAG, by = list(UEFACL$AwayTeam), FUN = sum)
uefacl_away_gs_avg <- aggregate(UEFACL$FTAG, by = list(UEFACL$AwayTeam),mean)
uefacl_away_scoring <- merge(uefacl_away_gs,uefacl_away_gs_avg, by='Group.1',all = T)
names(uefacl_away_scoring)[names(uefacl_away_scoring) == "x.x"] <- "TFtag"
names(uefacl_away_scoring)[names(uefacl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
uefacl_scoring <- merge(uefacl_home_scoring,uefacl_away_scoring,by='Group.1',all = T)
uefacl_scoring$TGS <- uefacl_scoring$TFthg + uefacl_scoring$TFtag

#Home shots on target
uefacl_home_hst <- aggregate(UEFACL$HST, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_ast <- aggregate(UEFACL$AST, by = list(UEFACL$AwayTeam), FUN = sum)
uefacl_tst <- merge(uefacl_home_hst,uefacl_away_ast, by='Group.1',all = T)
names(uefacl_tst)[names(uefacl_tst) == "x.x"] <- "hst"
names(uefacl_tst)[names(uefacl_tst) == "x.y"] <- "ast"
uefacl_tst$TST <- uefacl_tst$hst + uefacl_tst$ast
#merge goals scored and shots on target
uefacl_scoring_conversion <- merge(uefacl_tst,uefacl_scoring,by='Group.1',all = T)
#add HSC ASC TSC
uefacl_scoring_conversion$HSTC <- percent(uefacl_scoring_conversion$TFthg/uefacl_scoring_conversion$hst, accuracy = 0.01)
uefacl_scoring_conversion$ASTC <- percent(uefacl_scoring_conversion$TFtag/uefacl_scoring_conversion$ast, accuracy = 0.01)
uefacl_scoring_conversion$TSTC <- percent(uefacl_scoring_conversion$TGS/uefacl_scoring_conversion$TST, accuracy = 0.01)
#merge games played
uefacl_scoring_conversion <- cbind(uefacl_scoring_conversion,uefacl_games_played)
#create the second part
#home goals conceded
uefacl_home_gc <- aggregate(UEFACL$FTAG, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_home_gc_avg <- aggregate(UEFACL$FTAG, by = list(UEFACL$HomeTeam),mean)
uefacl_home_conceding <- merge(uefacl_home_gc,uefacl_home_gc_avg, by='Group.1',all = T)
names(uefacl_home_conceding)[names(uefacl_home_conceding) == "x.x"] <- "TFthc"
names(uefacl_home_conceding)[names(uefacl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
uefacl_away_gc <- aggregate(UEFACL$FTHG, by = list(UEFACL$AwayTeam), FUN = sum)
uefacl_away_gc_avg <- aggregate(UEFACL$FTHG, by = list(UEFACL$AwayTeam),mean)
uefacl_away_conceding <- merge(uefacl_away_gc,uefacl_away_gc_avg, by='Group.1',all = T)
names(uefacl_away_conceding)[names(uefacl_away_conceding) == "x.x"] <- "TFtac"
names(uefacl_away_conceding)[names(uefacl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
uefacl_conceding <- merge(uefacl_home_conceding,uefacl_away_conceding,by='Group.1',all = T)
uefacl_conceding$TGC <- uefacl_conceding$TFthc + uefacl_conceding$TFtac
uefacl_home_hst
#Home shots conceded
uefacl_home_hsc <- aggregate(UEFACL$AST, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_asc <- aggregate(UEFACL$HST, by = list(UEFACL$AwayTeam), FUN = sum)
uefacl_tsc <- merge(uefacl_home_hsc,uefacl_away_asc, by='Group.1',all = T)
names(uefacl_tsc)[names(uefacl_tsc) == "x.x"] <- "hsc"
names(uefacl_tsc)[names(uefacl_tsc) == "x.y"] <- "asc"
uefacl_tsc$TSC <- uefacl_tsc$hsc + uefacl_tsc$asc
#merge goals conceded and shots conceded
uefacl_conceding_conversion <- merge(uefacl_tsc,uefacl_conceding,by='Group.1',all = T)

#add HSC ASC TSC
uefacl_conceding_conversion$HSCC <- percent(uefacl_conceding_conversion$TFthc/uefacl_conceding_conversion$hsc, accuracy = 0.01)
uefacl_conceding_conversion$ASCC <- percent(uefacl_conceding_conversion$TFtac/uefacl_conceding_conversion$asc, accuracy = 0.01)
uefacl_conceding_conversion$TSCC <- percent(uefacl_conceding_conversion$TGC/uefacl_conceding_conversion$TSC, accuracy = 0.01)
uefacl_conceding_conversion$XSTC <- round(uefacl_scoring$TGS/(uefacl_tst$TST - uefacl_scoring$TGS), digits = 2)

#merge the two parts
uefacl_shots_analysis <- merge(uefacl_scoring_conversion,uefacl_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#UEFACL
#home fouls for
uefacl_home_fouls <- aggregate(UEFACL$HF, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_home_fouls_avg <- aggregate(UEFACL$HF, by = list(UEFACL$HomeTeam),mean)
uefacl_home_foulsdata <- merge(uefacl_home_fouls,uefacl_home_fouls_avg, by='Group.1',all = T)
names(uefacl_home_foulsdata)[names(uefacl_home_foulsdata) == "x.x"] <- "THfouls"
names(uefacl_home_foulsdata)[names(uefacl_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
uefacl_away_fouls <- aggregate(UEFACL$HF, by = list(UEFACL$AwayTeam), FUN = sum)
uefacl_away_fouls_avg <- aggregate(UEFACL$HF, by = list(UEFACL$AwayTeam),mean)
uefacl_away_foulsdata <- merge(uefacl_away_fouls,uefacl_away_fouls_avg, by='Group.1',all = T)
names(uefacl_away_foulsdata)[names(uefacl_away_foulsdata) == "x.x"] <- "TAfouls"
names(uefacl_away_foulsdata)[names(uefacl_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
uefacl_fouls <- merge(uefacl_home_foulsdata,uefacl_away_foulsdata,by='Group.1',all = T)
uefacl_fouls$TotalFouls <- uefacl_fouls$THfouls + uefacl_fouls$TAfouls

#yellow cards
uefacl_home_hyc <- aggregate(UEFACL$HY, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_ayc <- aggregate(UEFACL$AY, by = list(UEFACL$AwayTeam), FUN = sum)
uefacl_tyc <- merge(uefacl_home_hyc,uefacl_away_ayc, by='Group.1',all = T)
names(uefacl_tyc)[names(uefacl_tyc) == "x.x"] <- "hyc"
names(uefacl_tyc)[names(uefacl_tyc) == "x.y"] <- "ayc"
uefacl_tyc$TotalYellows <- uefacl_tyc$hyc + uefacl_tyc$ayc

#merge fouls for and yellow cards
uefacl_fouls_conversion <- merge(uefacl_tyc,uefacl_fouls,by='Group.1',all = T)
uefacl_fouls_conversion$YcPerfoul <- round((uefacl_fouls_conversion$TotalYellows/uefacl_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
UEFACL$Div <- "UEFACL"
##
###################################################################################################################################################
#poisson cards
uefacl_GP <- nrow(UEFACL)
#Calculate total home goals for each division
uefacl_T_HY <- sum(uefacl_home_hyc$x)
#calculate average home goal
uefacl_avg_HY <- round(uefacl_T_HY /uefacl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
uefacl_T_AY <- sum(uefacl_away_ayc$x)
#calculate average away goal
uefacl_avg_AY <- round(uefacl_T_AY /uefacl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
uefacl_home_yas <- round(((uefacl_home_hyc$x/uefacl_home_games))/uefacl_avg_HY, digits = 4)
#calculate away attack strength
uefacl_away_yas <- round(((uefacl_away_ayc$x/uefacl_away_games))/uefacl_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
uefacl_avg_HYC <- round(uefacl_T_AY /uefacl_GP, digits = 4)
#avg away concede
uefacl_avg_AYC <- round(uefacl_T_HY /uefacl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
uefacl_home_ycc <- aggregate(UEFACL$AY, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_ycc <- aggregate(UEFACL$HY, by = list(UEFACL$AwayTeam), FUN = sum)
#home defense strength
uefacl_home_yds <- round(((uefacl_home_ycc$x/uefacl_home_games))/uefacl_avg_HYC, digits = 4)
#away defense strength
uefacl_away_yds <- round(((uefacl_away_ycc$x/uefacl_away_games))/uefacl_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_home_poisson_yc <- cbind(uefacl_division,uefacl_teams,uefacl_avg_HY,uefacl_home_yas,uefacl_home_yds)
#away poisson data
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_away_poisson_yc <- cbind(uefacl_division,uefacl_teams,uefacl_avg_AY,uefacl_away_yas,uefacl_away_yds)
###
HomeTeam_uefacl_yc <- rep(uefacl_teams, each = length(uefacl_teams))
AwayTeam_uefacl_yc <- rep(uefacl_teams, length(uefacl_teams))
UEFACL_fixtures_yc <- cbind(HomeTeam_uefacl_yc,AwayTeam_uefacl_yc)
UEFACL_fixtures_yc <- as.data.frame(UEFACL_fixtures_yc)
UEFACL_fixtures_yc <- UEFACL_fixtures_yc[!UEFACL_fixtures_yc$HomeTeam_uefacl_yc == UEFACL_fixtures_yc$AwayTeam_uefacl_yc,]
rownames(UEFACL_fixtures_yc) <- NULL
UEFACL_fixtures_yc$Div <- "UEFACL"
UEFACL_fixtures_yc <- UEFACL_fixtures_yc[,c(3,1,2)]

UEFACL_fixtures_yc$avg_HY_uefacl <- uefacl_avg_HY

UEFACL_fixtures_yc$uefacl_homeyas <- rep(uefacl_home_yas,each = length(uefacl_teams)-1)

uefacl_awayyds_lookup <- cbind(uefacl_teams,uefacl_away_yds)

uefacl_awayyds_lookup <- as.data.frame(uefacl_awayyds_lookup)

colnames(uefacl_awayyds_lookup) <- c("AwayTeam_uefacl_yc","uefacl_awayyds")


require('RH2')
UEFACL_fixtures_yc$uefacl_awayyds <- sqldf("SELECT uefacl_awayyds_lookup.uefacl_awayyds FROM uefacl_awayyds_lookup INNER JOIN UEFACL_fixtures_yc ON uefacl_awayyds_lookup.AwayTeam_uefacl_yc = UEFACL_fixtures_yc.AwayTeam_uefacl_yc")

UEFACL_fixtures_yc$avg_AY_uefacl <- uefacl_avg_AY

uefacl_awayyas_lookup <- cbind(uefacl_teams,uefacl_away_yas)

uefacl_awayyas_lookup <- as.data.frame(uefacl_awayyas_lookup)

colnames(uefacl_awayyas_lookup) <- c("AwayTeam_uefacl_yc","uefacl_awayyas")

UEFACL_fixtures_yc$uefacl_awayyas <- sqldf("SELECT uefacl_awayyas_lookup.uefacl_awayyas FROM uefacl_awayyas_lookup INNER JOIN UEFACL_fixtures_yc ON uefacl_awayyas_lookup.AwayTeam_uefacl_yc = UEFACL_fixtures_yc.AwayTeam_uefacl_yc")

UEFACL_fixtures_yc$uefacl_homeyds <- rep(uefacl_home_yds,each = length(uefacl_teams)-1)

UEFACL_fixtures_yc$uefacl_awayyds <- as.numeric(unlist(UEFACL_fixtures_yc$uefacl_awayyds))
#xGH
UEFACL_fixtures_yc$uefacl_xHYC <- UEFACL_fixtures_yc$avg_HY_uefacl * UEFACL_fixtures_yc$uefacl_homeyas * UEFACL_fixtures_yc$uefacl_awayyds
#xGA

UEFACL_fixtures_yc$uefacl_awayyas <- as.numeric(unlist(UEFACL_fixtures_yc$uefacl_awayyas))

UEFACL_fixtures_yc$uefacl_xAYC <- UEFACL_fixtures_yc$avg_AY_uefacl * UEFACL_fixtures_yc$uefacl_awayyas * UEFACL_fixtures_yc$uefacl_homeyds

UEFACL_fixtures_yc$uefacl_0_0 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_0 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_0_1 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_1 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_0 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_0_2 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_2 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_1 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_2 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_3 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_0 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_1 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_2 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_0_3 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_3 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_3 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_4 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_0 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_1 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_2 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_3 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_0_4 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_4 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_4 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_4 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_5 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_0 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_1 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_2 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_3 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_4 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_0_5 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_5 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_5 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_5 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_5 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_6 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_0 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(0,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_1 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(1,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_2 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(2,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_3 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(3,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_4 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(4,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_6_5 <- round(stats::dpois(6,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(5,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_0_6 <- round(stats::dpois(0,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_1_6 <- round(stats::dpois(1,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_2_6 <- round(stats::dpois(2,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_3_6 <- round(stats::dpois(3,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_4_6 <- round(stats::dpois(4,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
UEFACL_fixtures_yc$uefacl_5_6 <- round(stats::dpois(5,UEFACL_fixtures_yc$uefacl_xHYC) * stats::dpois(6,UEFACL_fixtures_yc$uefacl_xAYC), digits = 4)
#Home win
UEFACL_fixtures_yc$uefacl_H <- (
  UEFACL_fixtures_yc$uefacl_1_0 + UEFACL_fixtures_yc$uefacl_2_0 + UEFACL_fixtures_yc$uefacl_2_1 + UEFACL_fixtures_yc$uefacl_3_0 + UEFACL_fixtures_yc$uefacl_3_1 +
    UEFACL_fixtures_yc$uefacl_3_2 + UEFACL_fixtures_yc$uefacl_4_0 + UEFACL_fixtures_yc$uefacl_4_1 + UEFACL_fixtures_yc$uefacl_4_2 + UEFACL_fixtures_yc$uefacl_4_3 +
    UEFACL_fixtures_yc$uefacl_5_0 + UEFACL_fixtures_yc$uefacl_5_1 + UEFACL_fixtures_yc$uefacl_5_2 + UEFACL_fixtures_yc$uefacl_5_3 + UEFACL_fixtures_yc$uefacl_5_4 +
    UEFACL_fixtures_yc$uefacl_6_0 + UEFACL_fixtures_yc$uefacl_6_1 + UEFACL_fixtures_yc$uefacl_6_2 + UEFACL_fixtures_yc$uefacl_6_3 + UEFACL_fixtures_yc$uefacl_6_4 +
    UEFACL_fixtures_yc$uefacl_6_5
)

UEFACL_fixtures_yc$uefacl_H <- percent(UEFACL_fixtures_yc$uefacl_H, accuracy = 0.1)

#Draw
UEFACL_fixtures_yc$uefacl_D <- (

  UEFACL_fixtures_yc$uefacl_0_0 + UEFACL_fixtures_yc$uefacl_1_1 + UEFACL_fixtures_yc$uefacl_2_2 + UEFACL_fixtures_yc$uefacl_3_3 + UEFACL_fixtures_yc$uefacl_4_4 +
    UEFACL_fixtures_yc$uefacl_5_5 + UEFACL_fixtures_yc$uefacl_6_6
)

UEFACL_fixtures_yc$uefacl_D <- percent(UEFACL_fixtures_yc$uefacl_D, accuracy = 0.1)

#Away

UEFACL_fixtures_yc$uefacl_A <- (
  UEFACL_fixtures_yc$uefacl_0_1 + UEFACL_fixtures_yc$uefacl_0_2 + UEFACL_fixtures_yc$uefacl_1_2 + UEFACL_fixtures_yc$uefacl_0_3 + UEFACL_fixtures_yc$uefacl_1_3 +
    UEFACL_fixtures_yc$uefacl_2_3 + UEFACL_fixtures_yc$uefacl_0_4 + UEFACL_fixtures_yc$uefacl_1_4 + UEFACL_fixtures_yc$uefacl_2_4 + UEFACL_fixtures_yc$uefacl_3_4 +
    UEFACL_fixtures_yc$uefacl_0_5 + UEFACL_fixtures_yc$uefacl_1_5 + UEFACL_fixtures_yc$uefacl_2_5 + UEFACL_fixtures_yc$uefacl_3_5 + UEFACL_fixtures_yc$uefacl_4_5 +
    UEFACL_fixtures_yc$uefacl_0_6 + UEFACL_fixtures_yc$uefacl_1_6 + UEFACL_fixtures_yc$uefacl_2_6 + UEFACL_fixtures_yc$uefacl_3_6 + UEFACL_fixtures_yc$uefacl_4_6 +
    UEFACL_fixtures_yc$uefacl_5_6
)

UEFACL_fixtures_yc$uefacl_A <- percent(UEFACL_fixtures_yc$uefacl_A, accuracy = 0.1)

#ov25
UEFACL_fixtures_yc$uefacl_ov25 <- (
  UEFACL_fixtures_yc$uefacl_2_1 + UEFACL_fixtures_yc$uefacl_1_2 + UEFACL_fixtures_yc$uefacl_2_2 + UEFACL_fixtures_yc$uefacl_3_0 + UEFACL_fixtures_yc$uefacl_3_1 +
    UEFACL_fixtures_yc$uefacl_3_2 + UEFACL_fixtures_yc$uefacl_0_3 + UEFACL_fixtures_yc$uefacl_1_3 + UEFACL_fixtures_yc$uefacl_2_3 + UEFACL_fixtures_yc$uefacl_3_3 +
    UEFACL_fixtures_yc$uefacl_4_0 + UEFACL_fixtures_yc$uefacl_4_1 + UEFACL_fixtures_yc$uefacl_4_2 + UEFACL_fixtures_yc$uefacl_4_3 + UEFACL_fixtures_yc$uefacl_0_4 +
    UEFACL_fixtures_yc$uefacl_1_4 + UEFACL_fixtures_yc$uefacl_2_4 + UEFACL_fixtures_yc$uefacl_3_4 + UEFACL_fixtures_yc$uefacl_4_4 + UEFACL_fixtures_yc$uefacl_5_0 +
    UEFACL_fixtures_yc$uefacl_5_1 + UEFACL_fixtures_yc$uefacl_5_2 + UEFACL_fixtures_yc$uefacl_5_3 + UEFACL_fixtures_yc$uefacl_5_4 + UEFACL_fixtures_yc$uefacl_0_5 +
    UEFACL_fixtures_yc$uefacl_1_5 + UEFACL_fixtures_yc$uefacl_2_5 + UEFACL_fixtures_yc$uefacl_3_5 + UEFACL_fixtures_yc$uefacl_4_5 + UEFACL_fixtures_yc$uefacl_5_5 +
    UEFACL_fixtures_yc$uefacl_6_0 + UEFACL_fixtures_yc$uefacl_6_1 + UEFACL_fixtures_yc$uefacl_6_2 + UEFACL_fixtures_yc$uefacl_6_3 + UEFACL_fixtures_yc$uefacl_6_4 +
    UEFACL_fixtures_yc$uefacl_6_5 + UEFACL_fixtures_yc$uefacl_0_6 + UEFACL_fixtures_yc$uefacl_1_6 + UEFACL_fixtures_yc$uefacl_2_6 + UEFACL_fixtures_yc$uefacl_3_6 +
    UEFACL_fixtures_yc$uefacl_4_6 + UEFACL_fixtures_yc$uefacl_5_6 + UEFACL_fixtures_yc$uefacl_6_6
)
#un25
UEFACL_fixtures_yc$uefacl_un25 <- (
  UEFACL_fixtures_yc$uefacl_0_0 + UEFACL_fixtures_yc$uefacl_1_0 + UEFACL_fixtures_yc$uefacl_0_1 + UEFACL_fixtures_yc$uefacl_1_1 + UEFACL_fixtures_yc$uefacl_2_0 + UEFACL_fixtures_yc$uefacl_0_2
)
#odds
UEFACL_fixtures_yc$uefacl_ov25_odds <- round((1/UEFACL_fixtures_yc$uefacl_ov25),digits = 2)
UEFACL_fixtures_yc$uefacl_un25_odds <- round((1/UEFACL_fixtures_yc$uefacl_un25),digits = 2)

UEFACL_fixtures_yc$uefacl_ov25_odds
UEFACL_fixtures_yc$uefacl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UEFACL_fixtures_yc$uefacl_ov25 <- percent(UEFACL_fixtures_yc$uefacl_ov25, accuracy = 0.1)

UEFACL_fixtures_yc$uefacl_un25 <- percent(UEFACL_fixtures_yc$uefacl_un25, accuracy = 0.1)
UEFACL_fixtures_yc$uefacl_pscore <- paste(round(UEFACL_fixtures_yc$uefacl_xHYC,digits = 0),round(UEFACL_fixtures_yc$uefacl_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
uefacl_GP <- nrow(UEFACL)
#Calculate total home corners for each division
uefacl_home_corners <- aggregate(UEFACL$HCO, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_corners <- aggregate(UEFACL$ACO, by = list(UEFACL$AwayTeam), FUN = sum)
###############################################################################
uefacl_T_HCO <- sum(uefacl_home_corners$x)
#calculate average home corners
uefacl_avg_HCO <- round(uefacl_T_HCO /uefacl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
uefacl_T_ACO <- sum(uefacl_away_corners$x)
#calculate average away goal
uefacl_avg_ACO <- round(uefacl_T_ACO /uefacl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
uefacl_home_coas <- round(((uefacl_home_corners$x/uefacl_home_games))/uefacl_avg_HCO, digits = 4)
#calculate away attack strength
uefacl_away_coas <- round(((uefacl_away_corners$x/uefacl_away_games))/uefacl_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
uefacl_avg_HCOC <- round(uefacl_T_ACO /uefacl_GP, digits = 4)
#avg away concede
uefacl_avg_ACOC <- round(uefacl_T_HCO /uefacl_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
uefacl_home_coc <- aggregate(UEFACL$ACO, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_coc <- aggregate(UEFACL$HCO, by = list(UEFACL$AwayTeam), FUN = sum)
#home defense strength
uefacl_home_cods <- round(((uefacl_home_coc$x/uefacl_home_games))/uefacl_avg_HCOC, digits = 4)
#away defense strength
uefacl_away_cods <- round(((uefacl_away_coc$x/uefacl_away_games))/uefacl_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_home_poisson_corners <- cbind(uefacl_division,uefacl_teams,uefacl_avg_HCO,uefacl_home_coas,uefacl_home_cods)
#################################################################################
#away poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_away_poisson_corners <- cbind(uefacl_division,uefacl_teams,uefacl_avg_ACO,uefacl_away_coas,uefacl_away_cods)

#UEFACL
HomeTeam_uefacl_co <- rep(uefacl_teams, each = length(uefacl_teams))
AwayTeam_uefacl_co <- rep(uefacl_teams, length(uefacl_teams))
UEFACL_fixtures_co <- cbind(HomeTeam_uefacl_co,AwayTeam_uefacl_co)
UEFACL_fixtures_co <- as.data.frame(UEFACL_fixtures_co)
UEFACL_fixtures_co <- UEFACL_fixtures_co[!UEFACL_fixtures_co$HomeTeam_uefacl_co == UEFACL_fixtures_co$AwayTeam_uefacl_co,]
rownames(UEFACL_fixtures_co) <- NULL
UEFACL_fixtures_co$Div <- "UEFACL"
UEFACL_fixtures_co <- UEFACL_fixtures_co[,c(3,1,2)]

UEFACL_fixtures_co$avg_HCO_uefacl <- uefacl_avg_HCO

UEFACL_fixtures_co$uefacl_homecoas <- rep(uefacl_home_coas,each = length(uefacl_teams)-1)

uefacl_awaycods_lookup <- cbind(uefacl_teams,uefacl_away_cods)

uefacl_awaycods_lookup <- as.data.frame(uefacl_awaycods_lookup)

colnames(uefacl_awaycods_lookup) <- c("AwayTeam_uefacl_co","uefacl_awaycods")


require('RH2')
UEFACL_fixtures_co$uefacl_awaycods <- sqldf("SELECT uefacl_awaycods_lookup.uefacl_awaycods FROM uefacl_awaycods_lookup INNER JOIN UEFACL_fixtures_co ON uefacl_awaycods_lookup.AwayTeam_uefacl_co = UEFACL_fixtures_co.AwayTeam_uefacl_co")

UEFACL_fixtures_co$avg_ACO_uefacl <- uefacl_avg_ACO

uefacl_awaycoas_lookup <- cbind(uefacl_teams,uefacl_away_coas)

uefacl_awaycoas_lookup <- as.data.frame(uefacl_awaycoas_lookup)

colnames(uefacl_awaycoas_lookup) <- c("AwayTeam_uefacl_co","uefacl_awaycoas")

UEFACL_fixtures_co$uefacl_awaycoas <- sqldf("SELECT uefacl_awaycoas_lookup.uefacl_awaycoas FROM uefacl_awaycoas_lookup INNER JOIN UEFACL_fixtures_co ON uefacl_awaycoas_lookup.AwayTeam_uefacl_co = UEFACL_fixtures_co.AwayTeam_uefacl_co")

UEFACL_fixtures_co$uefacl_homecods <- rep(uefacl_home_cods,each = length(uefacl_teams)-1)

UEFACL_fixtures_co$uefacl_awaycods <- as.numeric(unlist(UEFACL_fixtures_co$uefacl_awaycods))
#xGH
UEFACL_fixtures_co$uefacl_xHCOC <- UEFACL_fixtures_co$avg_HCO_uefacl * UEFACL_fixtures_co$uefacl_homecoas * UEFACL_fixtures_co$uefacl_awaycods
#xGA

UEFACL_fixtures_co$uefacl_awaycoas <- as.numeric(unlist(UEFACL_fixtures_co$uefacl_awaycoas))

UEFACL_fixtures_co$uefacl_xACOC <- UEFACL_fixtures_co$avg_ACO_uefacl * UEFACL_fixtures_co$uefacl_awaycoas * UEFACL_fixtures_co$uefacl_homecods

UEFACL_fixtures_co$uefacl_0_0 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_0 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_0_1 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_1 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_0 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_0_2 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_2 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_1 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_2 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_3 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_0 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_1 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_2 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_0_3 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_3 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_3 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_4 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_0 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_1 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_2 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_3 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_0_4 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_4 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_4 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_4 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_5 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_0 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_1 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_2 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_3 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_4 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_0_5 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_5 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_5 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_5 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_5 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_6 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_0 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(0,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_1 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(1,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_2 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(2,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_3 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(3,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_4 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(4,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_6_5 <- round(stats::dpois(6,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(5,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_0_6 <- round(stats::dpois(0,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_1_6 <- round(stats::dpois(1,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_2_6 <- round(stats::dpois(2,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_3_6 <- round(stats::dpois(3,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_4_6 <- round(stats::dpois(4,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
UEFACL_fixtures_co$uefacl_5_6 <- round(stats::dpois(5,UEFACL_fixtures_co$uefacl_xHCOC) * stats::dpois(6,UEFACL_fixtures_co$uefacl_xACOC), digits = 4)
#Home win
UEFACL_fixtures_co$uefacl_H <- (
  UEFACL_fixtures_co$uefacl_1_0 + UEFACL_fixtures_co$uefacl_2_0 + UEFACL_fixtures_co$uefacl_2_1 + UEFACL_fixtures_co$uefacl_3_0 + UEFACL_fixtures_co$uefacl_3_1 +
    UEFACL_fixtures_co$uefacl_3_2 + UEFACL_fixtures_co$uefacl_4_0 + UEFACL_fixtures_co$uefacl_4_1 + UEFACL_fixtures_co$uefacl_4_2 + UEFACL_fixtures_co$uefacl_4_3 +
    UEFACL_fixtures_co$uefacl_5_0 + UEFACL_fixtures_co$uefacl_5_1 + UEFACL_fixtures_co$uefacl_5_2 + UEFACL_fixtures_co$uefacl_5_3 + UEFACL_fixtures_co$uefacl_5_4 +
    UEFACL_fixtures_co$uefacl_6_0 + UEFACL_fixtures_co$uefacl_6_1 + UEFACL_fixtures_co$uefacl_6_2 + UEFACL_fixtures_co$uefacl_6_3 + UEFACL_fixtures_co$uefacl_6_4 +
    UEFACL_fixtures_co$uefacl_6_5
)

UEFACL_fixtures_co$uefacl_H <- percent(UEFACL_fixtures_co$uefacl_H, accuracy = 0.1)

#Draw
UEFACL_fixtures_co$uefacl_D <- (

  UEFACL_fixtures_co$uefacl_0_0 + UEFACL_fixtures_co$uefacl_1_1 + UEFACL_fixtures_co$uefacl_2_2 + UEFACL_fixtures_co$uefacl_3_3 + UEFACL_fixtures_co$uefacl_4_4 +
    UEFACL_fixtures_co$uefacl_5_5 + UEFACL_fixtures_co$uefacl_6_6
)

UEFACL_fixtures_co$uefacl_D <- percent(UEFACL_fixtures_co$uefacl_D, accuracy = 0.1)

#Away

UEFACL_fixtures_co$uefacl_A <- (
  UEFACL_fixtures_co$uefacl_0_1 + UEFACL_fixtures_co$uefacl_0_2 + UEFACL_fixtures_co$uefacl_1_2 + UEFACL_fixtures_co$uefacl_0_3 + UEFACL_fixtures_co$uefacl_1_3 +
    UEFACL_fixtures_co$uefacl_2_3 + UEFACL_fixtures_co$uefacl_0_4 + UEFACL_fixtures_co$uefacl_1_4 + UEFACL_fixtures_co$uefacl_2_4 + UEFACL_fixtures_co$uefacl_3_4 +
    UEFACL_fixtures_co$uefacl_0_5 + UEFACL_fixtures_co$uefacl_1_5 + UEFACL_fixtures_co$uefacl_2_5 + UEFACL_fixtures_co$uefacl_3_5 + UEFACL_fixtures_co$uefacl_4_5 +
    UEFACL_fixtures_co$uefacl_0_6 + UEFACL_fixtures_co$uefacl_1_6 + UEFACL_fixtures_co$uefacl_2_6 + UEFACL_fixtures_co$uefacl_3_6 + UEFACL_fixtures_co$uefacl_4_6 +
    UEFACL_fixtures_co$uefacl_5_6
)

UEFACL_fixtures_co$uefacl_A <- percent(UEFACL_fixtures_co$uefacl_A, accuracy = 0.1)

#ov25
UEFACL_fixtures_co$uefacl_ov25 <- (
  UEFACL_fixtures_co$uefacl_2_1 + UEFACL_fixtures_co$uefacl_1_2 + UEFACL_fixtures_co$uefacl_2_2 + UEFACL_fixtures_co$uefacl_3_0 + UEFACL_fixtures_co$uefacl_3_1 +
    UEFACL_fixtures_co$uefacl_3_2 + UEFACL_fixtures_co$uefacl_0_3 + UEFACL_fixtures_co$uefacl_1_3 + UEFACL_fixtures_co$uefacl_2_3 + UEFACL_fixtures_co$uefacl_3_3 +
    UEFACL_fixtures_co$uefacl_4_0 + UEFACL_fixtures_co$uefacl_4_1 + UEFACL_fixtures_co$uefacl_4_2 + UEFACL_fixtures_co$uefacl_4_3 + UEFACL_fixtures_co$uefacl_0_4 +
    UEFACL_fixtures_co$uefacl_1_4 + UEFACL_fixtures_co$uefacl_2_4 + UEFACL_fixtures_co$uefacl_3_4 + UEFACL_fixtures_co$uefacl_4_4 + UEFACL_fixtures_co$uefacl_5_0 +
    UEFACL_fixtures_co$uefacl_5_1 + UEFACL_fixtures_co$uefacl_5_2 + UEFACL_fixtures_co$uefacl_5_3 + UEFACL_fixtures_co$uefacl_5_4 + UEFACL_fixtures_co$uefacl_0_5 +
    UEFACL_fixtures_co$uefacl_1_5 + UEFACL_fixtures_co$uefacl_2_5 + UEFACL_fixtures_co$uefacl_3_5 + UEFACL_fixtures_co$uefacl_4_5 + UEFACL_fixtures_co$uefacl_5_5 +
    UEFACL_fixtures_co$uefacl_6_0 + UEFACL_fixtures_co$uefacl_6_1 + UEFACL_fixtures_co$uefacl_6_2 + UEFACL_fixtures_co$uefacl_6_3 + UEFACL_fixtures_co$uefacl_6_4 +
    UEFACL_fixtures_co$uefacl_6_5 + UEFACL_fixtures_co$uefacl_0_6 + UEFACL_fixtures_co$uefacl_1_6 + UEFACL_fixtures_co$uefacl_2_6 + UEFACL_fixtures_co$uefacl_3_6 +
    UEFACL_fixtures_co$uefacl_4_6 + UEFACL_fixtures_co$uefacl_5_6 + UEFACL_fixtures_co$uefacl_6_6
)
#un25
UEFACL_fixtures_co$uefacl_un25 <- (
  UEFACL_fixtures_co$uefacl_0_0 + UEFACL_fixtures_co$uefacl_1_0 + UEFACL_fixtures_co$uefacl_0_1 + UEFACL_fixtures_co$uefacl_1_1 + UEFACL_fixtures_co$uefacl_2_0 + UEFACL_fixtures_co$uefacl_0_2
)
#odds
UEFACL_fixtures_co$uefacl_ov25_odds <- round((1/UEFACL_fixtures_co$uefacl_ov25),digits = 2)
UEFACL_fixtures_co$uefacl_un25_odds <- round((1/UEFACL_fixtures_co$uefacl_un25),digits = 2)

UEFACL_fixtures_co$uefacl_ov25_odds
UEFACL_fixtures_co$uefacl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UEFACL_fixtures_co$uefacl_ov25 <- percent(UEFACL_fixtures_co$uefacl_ov25, accuracy = 0.1)

UEFACL_fixtures_co$uefacl_un25 <- percent(UEFACL_fixtures_co$uefacl_un25, accuracy = 0.1)
UEFACL_fixtures_co$uefacl_pscore <- paste(round(UEFACL_fixtures_co$uefacl_xHCOC,digits = 0),round(UEFACL_fixtures_co$uefacl_xACOC,digits = 0),sep = "-")
######################################################################################################################################################################
#poisson fouls
uefacl_GP <- nrow(UEFACL)
#Calculate total home goals for each division
uefacl_T_HF <- sum(uefacl_home_fouls$x)
#calculate average home goal
uefacl_avg_HF <- round(uefacl_T_HF /uefacl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
uefacl_T_AF <- sum(uefacl_away_fouls$x)
#calculate average away goal
uefacl_avg_AF <- round(uefacl_T_AF /uefacl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
uefacl_home_fas <- round(((uefacl_home_fouls$x/uefacl_home_games))/uefacl_avg_HF, digits = 4)
#calculate away attack strength
uefacl_away_fas <- round(((uefacl_away_fouls$x/uefacl_away_games))/uefacl_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
uefacl_avg_HFC <- round(uefacl_T_AF /uefacl_GP, digits = 4)
#avg away concede
uefacl_avg_AFC <- round(uefacl_T_HF /uefacl_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
uefacl_home_fcc <- aggregate(UEFACL$AF, by = list(UEFACL$HomeTeam), FUN = sum)
uefacl_away_fcc <- aggregate(UEFACL$HF, by = list(UEFACL$AwayTeam), FUN = sum)

#home defense strength
uefacl_home_fds <- round(((uefacl_home_fcc$x/uefacl_home_games))/uefacl_avg_HFC, digits = 4)

#away defense strength
uefacl_away_fds <- round(((uefacl_away_fcc$x/uefacl_away_games))/uefacl_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_home_poisson_fo <- cbind(uefacl_division,uefacl_teams,uefacl_avg_HF,uefacl_home_fas,uefacl_home_fds)

#################################################################################
#away poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_away_poisson_fo <- cbind(uefacl_division,uefacl_teams,uefacl_avg_AF,uefacl_away_fas,uefacl_away_fds)

#UEFACL
HomeTeam_uefacl_fo <- rep(uefacl_teams, each = length(uefacl_teams))
AwayTeam_uefacl_fo <- rep(uefacl_teams, length(uefacl_teams))
UEFACL_fixtures_fo <- cbind(HomeTeam_uefacl_fo,AwayTeam_uefacl_fo)
UEFACL_fixtures_fo <- as.data.frame(UEFACL_fixtures_fo)
UEFACL_fixtures_fo <- UEFACL_fixtures_fo[!UEFACL_fixtures_fo$HomeTeam_uefacl_fo == UEFACL_fixtures_fo$AwayTeam_uefacl_fo,]
rownames(UEFACL_fixtures_fo) <- NULL
UEFACL_fixtures_fo$Div <- "UEFACL"
UEFACL_fixtures_fo <- UEFACL_fixtures_fo[,c(3,1,2)]

UEFACL_fixtures_fo$avg_HF_uefacl <- uefacl_avg_HF

UEFACL_fixtures_fo$uefacl_homefas <- rep(uefacl_home_fas,each = length(uefacl_teams)-1)

uefacl_awayfds_lookup <- cbind(uefacl_teams,uefacl_away_fds)

uefacl_awayfds_lookup <- as.data.frame(uefacl_awayfds_lookup)

colnames(uefacl_awayfds_lookup) <- c("AwayTeam_uefacl_fo","uefacl_awayfds")


require('RH2')
UEFACL_fixtures_fo$uefacl_awayfds <- sqldf("SELECT uefacl_awayfds_lookup.uefacl_awayfds FROM uefacl_awayfds_lookup INNER JOIN UEFACL_fixtures_fo ON uefacl_awayfds_lookup.AwayTeam_uefacl_fo = UEFACL_fixtures_fo.AwayTeam_uefacl_fo")

UEFACL_fixtures_fo$avg_AF_uefacl <- uefacl_avg_AF

uefacl_awayfas_lookup <- cbind(uefacl_teams,uefacl_away_fas)

uefacl_awayfas_lookup <- as.data.frame(uefacl_awayfas_lookup)

colnames(uefacl_awayfas_lookup) <- c("AwayTeam_uefacl_fo","uefacl_awayfas")

UEFACL_fixtures_fo$uefacl_awayfas <- sqldf("SELECT uefacl_awayfas_lookup.uefacl_awayfas FROM uefacl_awayfas_lookup INNER JOIN UEFACL_fixtures_fo ON uefacl_awayfas_lookup.AwayTeam_uefacl_fo = UEFACL_fixtures_fo.AwayTeam_uefacl_fo")

UEFACL_fixtures_fo$uefacl_homefds <- rep(uefacl_home_fds,each = length(uefacl_teams)-1)

UEFACL_fixtures_fo$uefacl_awayfds <- as.numeric(unlist(UEFACL_fixtures_fo$uefacl_awayfds))
#xGH
UEFACL_fixtures_fo$uefacl_xHF <- UEFACL_fixtures_fo$avg_HF_uefacl * UEFACL_fixtures_fo$uefacl_homefas * UEFACL_fixtures_fo$uefacl_awayfds
#xGA

UEFACL_fixtures_fo$uefacl_awayfas <- as.numeric(unlist(UEFACL_fixtures_fo$uefacl_awayfas))

UEFACL_fixtures_fo$uefacl_xAF <- UEFACL_fixtures_fo$avg_AF_uefacl * UEFACL_fixtures_fo$uefacl_awayfas * UEFACL_fixtures_fo$uefacl_homefds

UEFACL_fixtures_fo$uefacl_0_0 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_0 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_0_1 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_1 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_0 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_0_2 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_2 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_1 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_2 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_3 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_0 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_1 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_2 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_0_3 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_3 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_3 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_4 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_0 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_1 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_2 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_3 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_0_4 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_4 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_4 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_4 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_5 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_0 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_1 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_2 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_3 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_4 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_0_5 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_5 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_5 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_5 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_5 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_6 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_0 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(0,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_1 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(1,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_2 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(2,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_3 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(3,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_4 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(4,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_6_5 <- round(stats::dpois(6,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(5,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_0_6 <- round(stats::dpois(0,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_1_6 <- round(stats::dpois(1,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_2_6 <- round(stats::dpois(2,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_3_6 <- round(stats::dpois(3,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_4_6 <- round(stats::dpois(4,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
UEFACL_fixtures_fo$uefacl_5_6 <- round(stats::dpois(5,UEFACL_fixtures_fo$uefacl_xHF) * stats::dpois(6,UEFACL_fixtures_fo$uefacl_xAF), digits = 4)
#Home win
UEFACL_fixtures_fo$uefacl_H <- (
  UEFACL_fixtures_fo$uefacl_1_0 + UEFACL_fixtures_fo$uefacl_2_0 + UEFACL_fixtures_fo$uefacl_2_1 + UEFACL_fixtures_fo$uefacl_3_0 + UEFACL_fixtures_fo$uefacl_3_1 +
    UEFACL_fixtures_fo$uefacl_3_2 + UEFACL_fixtures_fo$uefacl_4_0 + UEFACL_fixtures_fo$uefacl_4_1 + UEFACL_fixtures_fo$uefacl_4_2 + UEFACL_fixtures_fo$uefacl_4_3 +
    UEFACL_fixtures_fo$uefacl_5_0 + UEFACL_fixtures_fo$uefacl_5_1 + UEFACL_fixtures_fo$uefacl_5_2 + UEFACL_fixtures_fo$uefacl_5_3 + UEFACL_fixtures_fo$uefacl_5_4 +
    UEFACL_fixtures_fo$uefacl_6_0 + UEFACL_fixtures_fo$uefacl_6_1 + UEFACL_fixtures_fo$uefacl_6_2 + UEFACL_fixtures_fo$uefacl_6_3 + UEFACL_fixtures_fo$uefacl_6_4 +
    UEFACL_fixtures_fo$uefacl_6_5
)

UEFACL_fixtures_fo$uefacl_H <- percent(UEFACL_fixtures_fo$uefacl_H, accuracy = 0.1)

#Draw
UEFACL_fixtures_fo$uefacl_D <- (

  UEFACL_fixtures_fo$uefacl_0_0 + UEFACL_fixtures_fo$uefacl_1_1 + UEFACL_fixtures_fo$uefacl_2_2 + UEFACL_fixtures_fo$uefacl_3_3 + UEFACL_fixtures_fo$uefacl_4_4 +
    UEFACL_fixtures_fo$uefacl_5_5 + UEFACL_fixtures_fo$uefacl_6_6
)

UEFACL_fixtures_fo$uefacl_D <- percent(UEFACL_fixtures_fo$uefacl_D, accuracy = 0.1)

#Away

UEFACL_fixtures_fo$uefacl_A <- (
  UEFACL_fixtures_fo$uefacl_0_1 + UEFACL_fixtures_fo$uefacl_0_2 + UEFACL_fixtures_fo$uefacl_1_2 + UEFACL_fixtures_fo$uefacl_0_3 + UEFACL_fixtures_fo$uefacl_1_3 +
    UEFACL_fixtures_fo$uefacl_2_3 + UEFACL_fixtures_fo$uefacl_0_4 + UEFACL_fixtures_fo$uefacl_1_4 + UEFACL_fixtures_fo$uefacl_2_4 + UEFACL_fixtures_fo$uefacl_3_4 +
    UEFACL_fixtures_fo$uefacl_0_5 + UEFACL_fixtures_fo$uefacl_1_5 + UEFACL_fixtures_fo$uefacl_2_5 + UEFACL_fixtures_fo$uefacl_3_5 + UEFACL_fixtures_fo$uefacl_4_5 +
    UEFACL_fixtures_fo$uefacl_0_6 + UEFACL_fixtures_fo$uefacl_1_6 + UEFACL_fixtures_fo$uefacl_2_6 + UEFACL_fixtures_fo$uefacl_3_6 + UEFACL_fixtures_fo$uefacl_4_6 +
    UEFACL_fixtures_fo$uefacl_5_6
)

UEFACL_fixtures_fo$uefacl_A <- percent(UEFACL_fixtures_fo$uefacl_A, accuracy = 0.1)

#ov25
UEFACL_fixtures_fo$uefacl_ov25 <- (
  UEFACL_fixtures_fo$uefacl_2_1 + UEFACL_fixtures_fo$uefacl_1_2 + UEFACL_fixtures_fo$uefacl_2_2 + UEFACL_fixtures_fo$uefacl_3_0 + UEFACL_fixtures_fo$uefacl_3_1 +
    UEFACL_fixtures_fo$uefacl_3_2 + UEFACL_fixtures_fo$uefacl_0_3 + UEFACL_fixtures_fo$uefacl_1_3 + UEFACL_fixtures_fo$uefacl_2_3 + UEFACL_fixtures_fo$uefacl_3_3 +
    UEFACL_fixtures_fo$uefacl_4_0 + UEFACL_fixtures_fo$uefacl_4_1 + UEFACL_fixtures_fo$uefacl_4_2 + UEFACL_fixtures_fo$uefacl_4_3 + UEFACL_fixtures_fo$uefacl_0_4 +
    UEFACL_fixtures_fo$uefacl_1_4 + UEFACL_fixtures_fo$uefacl_2_4 + UEFACL_fixtures_fo$uefacl_3_4 + UEFACL_fixtures_fo$uefacl_4_4 + UEFACL_fixtures_fo$uefacl_5_0 +
    UEFACL_fixtures_fo$uefacl_5_1 + UEFACL_fixtures_fo$uefacl_5_2 + UEFACL_fixtures_fo$uefacl_5_3 + UEFACL_fixtures_fo$uefacl_5_4 + UEFACL_fixtures_fo$uefacl_0_5 +
    UEFACL_fixtures_fo$uefacl_1_5 + UEFACL_fixtures_fo$uefacl_2_5 + UEFACL_fixtures_fo$uefacl_3_5 + UEFACL_fixtures_fo$uefacl_4_5 + UEFACL_fixtures_fo$uefacl_5_5 +
    UEFACL_fixtures_fo$uefacl_6_0 + UEFACL_fixtures_fo$uefacl_6_1 + UEFACL_fixtures_fo$uefacl_6_2 + UEFACL_fixtures_fo$uefacl_6_3 + UEFACL_fixtures_fo$uefacl_6_4 +
    UEFACL_fixtures_fo$uefacl_6_5 + UEFACL_fixtures_fo$uefacl_0_6 + UEFACL_fixtures_fo$uefacl_1_6 + UEFACL_fixtures_fo$uefacl_2_6 + UEFACL_fixtures_fo$uefacl_3_6 +
    UEFACL_fixtures_fo$uefacl_4_6 + UEFACL_fixtures_fo$uefacl_5_6 + UEFACL_fixtures_fo$uefacl_6_6
)
#un25
UEFACL_fixtures_fo$uefacl_un25 <- (
  UEFACL_fixtures_fo$uefacl_0_0 + UEFACL_fixtures_fo$uefacl_1_0 + UEFACL_fixtures_fo$uefacl_0_1 + UEFACL_fixtures_fo$uefacl_1_1 + UEFACL_fixtures_fo$uefacl_2_0 + UEFACL_fixtures_fo$uefacl_0_2
)
#odds
UEFACL_fixtures_fo$uefacl_ov25_odds <- round((1/UEFACL_fixtures_fo$uefacl_ov25),digits = 2)
UEFACL_fixtures_fo$uefacl_un25_odds <- round((1/UEFACL_fixtures_fo$uefacl_un25),digits = 2)

UEFACL_fixtures_fo$uefacl_ov25_odds
UEFACL_fixtures_fo$uefacl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UEFACL_fixtures_fo$uefacl_ov25 <- percent(UEFACL_fixtures_fo$uefacl_ov25, accuracy = 0.1)

UEFACL_fixtures_fo$uefacl_un25 <- percent(UEFACL_fixtures_fo$uefacl_un25, accuracy = 0.1)
UEFACL_fixtures_fo$uefacl_psfore <- paste(round(UEFACL_fixtures_fo$uefacl_xHF,digits = 0),round(UEFACL_fixtures_fo$uefacl_xAF,digits = 0),sep = "-")
####################################################################################################################################################################
#poisson shots
uefacl_GP <- nrow(UEFACL)

#Calculate total home goals for each division
uefacl_T_HST <- sum(uefacl_home_hst$x)
#calculate average home goal

uefacl_avg_HST <- round(uefacl_T_HST /uefacl_GP, digits = 4)

############################################################
#Calculate total away goals for each division
uefacl_T_AST <- sum(uefacl_away_ast$x)
#calculate average away goal
uefacl_avg_AST <- round(uefacl_T_AST /uefacl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
uefacl_home_sotas <- round(((uefacl_home_hst$x/uefacl_home_games))/uefacl_avg_HST, digits = 4)
#calculate away attack strength
uefacl_away_sotas <- round(((uefacl_away_ast$x/uefacl_away_games))/uefacl_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
uefacl_avg_HSC <- round(uefacl_T_AST /uefacl_GP, digits = 4)

#avg away concede
uefacl_avg_ASC <- round(uefacl_T_HST /uefacl_GP, digits = 4)
#home defense strength
uefacl_home_sods <- round(((uefacl_home_hsc$x/uefacl_home_games))/uefacl_avg_HSC, digits = 4)

#away defense strength
uefacl_away_sods <- round(((uefacl_away_ast$x/uefacl_away_games))/uefacl_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_home_poisson_sot <- cbind(uefacl_division,uefacl_teams,uefacl_avg_HST,uefacl_home_sotas,uefacl_home_sods)

#################################################################################
#away poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_away_poisson_sot <- cbind(uefacl_division,uefacl_teams,uefacl_avg_AST,uefacl_away_sotas,uefacl_away_sods)

#UEFACL
HomeTeam_uefacl_sot <- rep(uefacl_teams, each = length(uefacl_teams))
AwayTeam_uefacl_sot <- rep(uefacl_teams, length(uefacl_teams))
UEFACL_fixtures_sot <- cbind(HomeTeam_uefacl_sot,AwayTeam_uefacl_sot)
UEFACL_fixtures_sot <- as.data.frame(UEFACL_fixtures_sot)
UEFACL_fixtures_sot <- UEFACL_fixtures_sot[!UEFACL_fixtures_sot$HomeTeam_uefacl_sot == UEFACL_fixtures_sot$AwayTeam_uefacl_sot,]
rownames(UEFACL_fixtures_sot) <- NULL
UEFACL_fixtures_sot$Div <- "UEFACL"
UEFACL_fixtures_sot <- UEFACL_fixtures_sot[,c(3,1,2)]

UEFACL_fixtures_sot$avg_HST_uefacl <- uefacl_avg_HST

UEFACL_fixtures_sot$uefacl_homesotas <- rep(uefacl_home_sotas,each = length(uefacl_teams)-1)

uefacl_awaysods_lookup <- cbind(uefacl_teams,uefacl_away_sods)

uefacl_awaysods_lookup <- as.data.frame(uefacl_awaysods_lookup)

colnames(uefacl_awaysods_lookup) <- c("AwayTeam_uefacl_sot","uefacl_awaysods")


require('RH2')
UEFACL_fixtures_sot$uefacl_awaysods <- sqldf("SELECT uefacl_awaysods_lookup.uefacl_awaysods FROM uefacl_awaysods_lookup INNER JOIN UEFACL_fixtures_sot ON uefacl_awaysods_lookup.AwayTeam_uefacl_sot = UEFACL_fixtures_sot.AwayTeam_uefacl_sot")

UEFACL_fixtures_sot$avg_AST_uefacl <- uefacl_avg_AST

uefacl_awaysotas_lookup <- cbind(uefacl_teams,uefacl_away_sotas)

uefacl_awaysotas_lookup <- as.data.frame(uefacl_awaysotas_lookup)

colnames(uefacl_awaysotas_lookup) <- c("AwayTeam_uefacl_sot","uefacl_awaysotas")

UEFACL_fixtures_sot$uefacl_awaysotas <- sqldf("SELECT uefacl_awaysotas_lookup.uefacl_awaysotas FROM uefacl_awaysotas_lookup INNER JOIN UEFACL_fixtures_sot ON uefacl_awaysotas_lookup.AwayTeam_uefacl_sot = UEFACL_fixtures_sot.AwayTeam_uefacl_sot")

UEFACL_fixtures_sot$uefacl_homesods <- rep(uefacl_home_sods,each = length(uefacl_teams)-1)

UEFACL_fixtures_sot$uefacl_awaysods <- as.numeric(unlist(UEFACL_fixtures_sot$uefacl_awaysods))
#xGH
UEFACL_fixtures_sot$uefacl_xHST <- UEFACL_fixtures_sot$avg_HST_uefacl * UEFACL_fixtures_sot$uefacl_homesotas * UEFACL_fixtures_sot$uefacl_awaysods
#xGA

UEFACL_fixtures_sot$uefacl_awaysotas <- as.numeric(unlist(UEFACL_fixtures_sot$uefacl_awaysotas))

UEFACL_fixtures_sot$uefacl_xAST <- UEFACL_fixtures_sot$avg_AST_uefacl * UEFACL_fixtures_sot$uefacl_awaysotas * UEFACL_fixtures_sot$uefacl_homesods

UEFACL_fixtures_sot$uefacl_0_0 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_0 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_0_1 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_1 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_0 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_0_2 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_2 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_1 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_2 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_3 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_0 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_1 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_2 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_0_3 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_3 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_3 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_4 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_0 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_1 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_2 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_3 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_0_4 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_4 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_4 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_4 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_5 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_0 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_1 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_2 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_3 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_4 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_0_5 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_5 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_5 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_5 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_5 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_6 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_0 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(0,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_1 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(1,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_2 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(2,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_3 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(3,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_4 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(4,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_6_5 <- round(stats::dpois(6,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(5,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_0_6 <- round(stats::dpois(0,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_1_6 <- round(stats::dpois(1,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_2_6 <- round(stats::dpois(2,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_3_6 <- round(stats::dpois(3,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_4_6 <- round(stats::dpois(4,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
UEFACL_fixtures_sot$uefacl_5_6 <- round(stats::dpois(5,UEFACL_fixtures_sot$uefacl_xHST) * stats::dpois(6,UEFACL_fixtures_sot$uefacl_xAST), digits = 4)
#Home win
UEFACL_fixtures_sot$uefacl_H <- (
  UEFACL_fixtures_sot$uefacl_1_0 + UEFACL_fixtures_sot$uefacl_2_0 + UEFACL_fixtures_sot$uefacl_2_1 + UEFACL_fixtures_sot$uefacl_3_0 + UEFACL_fixtures_sot$uefacl_3_1 +
    UEFACL_fixtures_sot$uefacl_3_2 + UEFACL_fixtures_sot$uefacl_4_0 + UEFACL_fixtures_sot$uefacl_4_1 + UEFACL_fixtures_sot$uefacl_4_2 + UEFACL_fixtures_sot$uefacl_4_3 +
    UEFACL_fixtures_sot$uefacl_5_0 + UEFACL_fixtures_sot$uefacl_5_1 + UEFACL_fixtures_sot$uefacl_5_2 + UEFACL_fixtures_sot$uefacl_5_3 + UEFACL_fixtures_sot$uefacl_5_4 +
    UEFACL_fixtures_sot$uefacl_6_0 + UEFACL_fixtures_sot$uefacl_6_1 + UEFACL_fixtures_sot$uefacl_6_2 + UEFACL_fixtures_sot$uefacl_6_3 + UEFACL_fixtures_sot$uefacl_6_4 +
    UEFACL_fixtures_sot$uefacl_6_5
)

UEFACL_fixtures_sot$uefacl_H <- percent(UEFACL_fixtures_sot$uefacl_H, accuracy = 0.1)

#Draw
UEFACL_fixtures_sot$uefacl_D <- (

  UEFACL_fixtures_sot$uefacl_0_0 + UEFACL_fixtures_sot$uefacl_1_1 + UEFACL_fixtures_sot$uefacl_2_2 + UEFACL_fixtures_sot$uefacl_3_3 + UEFACL_fixtures_sot$uefacl_4_4 +
    UEFACL_fixtures_sot$uefacl_5_5 + UEFACL_fixtures_sot$uefacl_6_6
)

UEFACL_fixtures_sot$uefacl_D <- percent(UEFACL_fixtures_sot$uefacl_D, accuracy = 0.1)

#Away

UEFACL_fixtures_sot$uefacl_A <- (
  UEFACL_fixtures_sot$uefacl_0_1 + UEFACL_fixtures_sot$uefacl_0_2 + UEFACL_fixtures_sot$uefacl_1_2 + UEFACL_fixtures_sot$uefacl_0_3 + UEFACL_fixtures_sot$uefacl_1_3 +
    UEFACL_fixtures_sot$uefacl_2_3 + UEFACL_fixtures_sot$uefacl_0_4 + UEFACL_fixtures_sot$uefacl_1_4 + UEFACL_fixtures_sot$uefacl_2_4 + UEFACL_fixtures_sot$uefacl_3_4 +
    UEFACL_fixtures_sot$uefacl_0_5 + UEFACL_fixtures_sot$uefacl_1_5 + UEFACL_fixtures_sot$uefacl_2_5 + UEFACL_fixtures_sot$uefacl_3_5 + UEFACL_fixtures_sot$uefacl_4_5 +
    UEFACL_fixtures_sot$uefacl_0_6 + UEFACL_fixtures_sot$uefacl_1_6 + UEFACL_fixtures_sot$uefacl_2_6 + UEFACL_fixtures_sot$uefacl_3_6 + UEFACL_fixtures_sot$uefacl_4_6 +
    UEFACL_fixtures_sot$uefacl_5_6
)

UEFACL_fixtures_sot$uefacl_A <- percent(UEFACL_fixtures_sot$uefacl_A, accuracy = 0.1)

#ov25
UEFACL_fixtures_sot$uefacl_ov25 <- (
  UEFACL_fixtures_sot$uefacl_2_1 + UEFACL_fixtures_sot$uefacl_1_2 + UEFACL_fixtures_sot$uefacl_2_2 + UEFACL_fixtures_sot$uefacl_3_0 + UEFACL_fixtures_sot$uefacl_3_1 +
    UEFACL_fixtures_sot$uefacl_3_2 + UEFACL_fixtures_sot$uefacl_0_3 + UEFACL_fixtures_sot$uefacl_1_3 + UEFACL_fixtures_sot$uefacl_2_3 + UEFACL_fixtures_sot$uefacl_3_3 +
    UEFACL_fixtures_sot$uefacl_4_0 + UEFACL_fixtures_sot$uefacl_4_1 + UEFACL_fixtures_sot$uefacl_4_2 + UEFACL_fixtures_sot$uefacl_4_3 + UEFACL_fixtures_sot$uefacl_0_4 +
    UEFACL_fixtures_sot$uefacl_1_4 + UEFACL_fixtures_sot$uefacl_2_4 + UEFACL_fixtures_sot$uefacl_3_4 + UEFACL_fixtures_sot$uefacl_4_4 + UEFACL_fixtures_sot$uefacl_5_0 +
    UEFACL_fixtures_sot$uefacl_5_1 + UEFACL_fixtures_sot$uefacl_5_2 + UEFACL_fixtures_sot$uefacl_5_3 + UEFACL_fixtures_sot$uefacl_5_4 + UEFACL_fixtures_sot$uefacl_0_5 +
    UEFACL_fixtures_sot$uefacl_1_5 + UEFACL_fixtures_sot$uefacl_2_5 + UEFACL_fixtures_sot$uefacl_3_5 + UEFACL_fixtures_sot$uefacl_4_5 + UEFACL_fixtures_sot$uefacl_5_5 +
    UEFACL_fixtures_sot$uefacl_6_0 + UEFACL_fixtures_sot$uefacl_6_1 + UEFACL_fixtures_sot$uefacl_6_2 + UEFACL_fixtures_sot$uefacl_6_3 + UEFACL_fixtures_sot$uefacl_6_4 +
    UEFACL_fixtures_sot$uefacl_6_5 + UEFACL_fixtures_sot$uefacl_0_6 + UEFACL_fixtures_sot$uefacl_1_6 + UEFACL_fixtures_sot$uefacl_2_6 + UEFACL_fixtures_sot$uefacl_3_6 +
    UEFACL_fixtures_sot$uefacl_4_6 + UEFACL_fixtures_sot$uefacl_5_6 + UEFACL_fixtures_sot$uefacl_6_6
)
#un25
UEFACL_fixtures_sot$uefacl_un25 <- (
  UEFACL_fixtures_sot$uefacl_0_0 + UEFACL_fixtures_sot$uefacl_1_0 + UEFACL_fixtures_sot$uefacl_0_1 + UEFACL_fixtures_sot$uefacl_1_1 + UEFACL_fixtures_sot$uefacl_2_0 + UEFACL_fixtures_sot$uefacl_0_2
)
#odds
UEFACL_fixtures_sot$uefacl_ov25_odds <- round((1/UEFACL_fixtures_sot$uefacl_ov25),digits = 2)
UEFACL_fixtures_sot$uefacl_un25_odds <- round((1/UEFACL_fixtures_sot$uefacl_un25),digits = 2)

UEFACL_fixtures_sot$uefacl_ov25_odds
UEFACL_fixtures_sot$uefacl_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
UEFACL_fixtures_sot$uefacl_ov25 <- percent(UEFACL_fixtures_sot$uefacl_ov25, accuracy = 0.1)

UEFACL_fixtures_sot$uefacl_un25 <- percent(UEFACL_fixtures_sot$uefacl_un25, accuracy = 0.1)
UEFACL_fixtures_sot$uefacl_pssotre <- paste(round(UEFACL_fixtures_sot$uefacl_xHST,digits = 0),round(UEFACL_fixtures_sot$uefacl_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
#hwins and away wins
uefacl_home_wins <- c()
uefacl_away_wins <- c()
uefacl_home_draws <- c()
uefacl_away_draws <- c()
uefacl_home_loss <- c()
uefacl_away_loss <- c()



for (i_uefacl_wins in 1:length(uefacl_teams))
{

  uefacl_home_wins[i_uefacl_wins] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_wins] & UEFACL$FTR == "H",])
  uefacl_away_wins[i_uefacl_wins] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_wins] & UEFACL$FTR == "A",])
  uefacl_home_draws[i_uefacl_wins] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_wins] & UEFACL$FTR == "D",])
  uefacl_away_draws[i_uefacl_wins] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_wins] & UEFACL$FTR == "D",])
  uefacl_home_loss[i_uefacl_wins] <- nrow(UEFACL[UEFACL$HomeTeam == uefacl_teams[i_uefacl_wins] & UEFACL$FTR == "A",])
  uefacl_away_loss[i_uefacl_wins] <- nrow(UEFACL[UEFACL$AwayTeam == uefacl_teams[i_uefacl_wins] & UEFACL$FTR == "H",])

}

uefacl_total_wins <- uefacl_home_wins + uefacl_away_wins
uefacl_total_draws <- uefacl_home_draws + uefacl_away_draws
uefacl_total_loss <- uefacl_home_loss + uefacl_away_loss

uefacl_league_table <- cbind(uefacl_teams,uefacl_games_played,uefacl_total_wins,uefacl_total_draws,uefacl_total_loss)
uefacl_GS <- uefacl_scoring$TGS
uefacl_GC <-uefacl_conceding$TGC
uefacl_GD <- uefacl_scoring$TGS - uefacl_conceding$TGC
uefacl_PTS <- (uefacl_total_wins*3) + (uefacl_total_draws*1)
uefacl_league_table <- cbind(uefacl_league_table,uefacl_GS,uefacl_GC,uefacl_GD,uefacl_PTS)
uefacl_league_table <- as.data.frame(uefacl_league_table)
#rename the columns
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_teams"] <- "Team"
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_games_played"] <- "P"
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_total_wins"] <- "W"
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_total_draws"] <- "D"
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_total_loss"] <- "L"
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_GS"] <- "F"
names(uefacl_league_table)[names(uefacl_league_table) == "uefacl_GC"] <- "A"
points_uefacl <- uefacl_league_table[order(as.numeric(uefacl_league_table$uefacl_PTS), decreasing = TRUE),]
points_uefacl$uefacl_rank <- 1:length(uefacl_teams)
row.names(points_uefacl) <- points_uefacl$uefacl_rank
#create final_uefacl_hf_against with team ranks in brackets
for(uefacl_rowhrank in 1:nrow(uefacl_form_team_against_h)) {
  for(uefacl_colhrank in 1:ncol(uefacl_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!uefacl_form_team_against_h[uefacl_rowhrank,uefacl_colhrank]=="",uefacl_form_team_against_h[uefacl_rowhrank,uefacl_colhrank] <- paste(uefacl_form_team_against_h[uefacl_rowhrank,uefacl_colhrank],"(",points_uefacl$uefacl_rank[points_uefacl$Team ==uefacl_form_team_against_h[uefacl_rowhrank,uefacl_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#################################################################################################################################################
#################################################################################################################################################
#poisson model
uefacl_GP <- nrow(UEFACL)

#Calculate total home goals for each division
uefacl_T_HG <- sum(uefacl_home_gs$x)

#calculate average home goal
uefacl_avg_HG <- round(uefacl_T_HG /uefacl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
uefacl_T_AG <- sum(uefacl_away_gs$x)
#calculate average away goal
uefacl_avg_AG <- round(uefacl_T_AG /uefacl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
uefacl_home_as <- round(((uefacl_home_gs$x/uefacl_home_games))/uefacl_avg_HG, digits = 4)
#calculate away attack strength
uefacl_away_as <- round(((uefacl_away_gs$x/uefacl_away_games))/uefacl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
uefacl_avg_HC <- round(uefacl_T_AG /uefacl_GP, digits = 4)
#avg away concede
uefacl_avg_AC <- round(uefacl_T_HG /uefacl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
uefacl_home_ds <- round(((uefacl_home_gc$x/uefacl_home_games))/uefacl_avg_HC, digits = 4)
#away defense strength
uefacl_away_ds <- round(((uefacl_away_gc$x/uefacl_away_games))/uefacl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_home_poisson <- cbind(uefacl_division,uefacl_teams,uefacl_avg_HG,uefacl_home_as,uefacl_home_ds)
#################################################################################
#away poisson data
#uefacl
uefacl_division <- c()
uefacl_division[1:length(uefacl_teams)] <- "UEFACL"
uefacl_away_poisson <- cbind(uefacl_division,uefacl_teams,uefacl_avg_AG,uefacl_away_as,uefacl_away_ds)

#UEFACL
HomeTeam_uefacl <- rep(uefacl_teams, each = length(uefacl_teams))
AwayTeam_uefacl <- rep(uefacl_teams, length(uefacl_teams))
UEFACL_fixtures <- cbind(HomeTeam_uefacl,AwayTeam_uefacl)
UEFACL_fixtures <- as.data.frame(UEFACL_fixtures)
UEFACL_fixtures <- UEFACL_fixtures[!UEFACL_fixtures$HomeTeam_uefacl == UEFACL_fixtures$AwayTeam_uefacl,]
rownames(UEFACL_fixtures) <- NULL
UEFACL_fixtures$Div <- "UEFACL"
UEFACL_fixtures <- UEFACL_fixtures[,c(3,1,2)]

UEFACL_fixtures$avg_HG_uefacl <- uefacl_avg_HG

UEFACL_fixtures$uefacl_homeas <- rep(uefacl_home_as,each = length(uefacl_teams)-1)

uefacl_awayds_lookup <- cbind(uefacl_teams,uefacl_away_ds)

uefacl_awayds_lookup <- as.data.frame(uefacl_awayds_lookup)

colnames(uefacl_awayds_lookup) <- c("AwayTeam_uefacl","uefacl_awayds")


require('RH2')
UEFACL_fixtures$uefacl_awayds <- sqldf("SELECT uefacl_awayds_lookup.uefacl_awayds FROM uefacl_awayds_lookup INNER JOIN UEFACL_fixtures ON uefacl_awayds_lookup.AwayTeam_uefacl = UEFACL_fixtures.AwayTeam_uefacl")

UEFACL_fixtures$avg_AG_uefacl <- uefacl_avg_AG

uefacl_awayas_lookup <- cbind(uefacl_teams,uefacl_away_as)

uefacl_awayas_lookup <- as.data.frame(uefacl_awayas_lookup)

colnames(uefacl_awayas_lookup) <- c("AwayTeam_uefacl","uefacl_awayas")


UEFACL_fixtures$uefacl_awayas <- sqldf("SELECT uefacl_awayas_lookup.uefacl_awayas FROM uefacl_awayas_lookup INNER JOIN UEFACL_fixtures ON uefacl_awayas_lookup.AwayTeam_uefacl = UEFACL_fixtures.AwayTeam_uefacl")

UEFACL_fixtures$uefacl_homeds <- rep(uefacl_home_ds,each = length(uefacl_teams)-1)

UEFACL_fixtures$uefacl_awayds <- as.numeric(unlist(UEFACL_fixtures$uefacl_awayds))
#xGH
UEFACL_fixtures$uefacl_xGH <- UEFACL_fixtures$avg_HG_uefacl * UEFACL_fixtures$uefacl_homeas * UEFACL_fixtures$uefacl_awayds

#xGA

UEFACL_fixtures$uefacl_awayas <- as.numeric(unlist(UEFACL_fixtures$uefacl_awayas))

UEFACL_fixtures$uefacl_xGA <- UEFACL_fixtures$avg_AG_uefacl * UEFACL_fixtures$uefacl_awayas * UEFACL_fixtures$uefacl_homeds

UEFACL_fixtures$uefacl_0_0 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_0 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_0_1 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_1 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_0 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_0_2 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_2 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_1 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_2 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_3 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_0 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_1 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_2 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_0_3 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_3 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_3 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_4 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_0 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_1 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_2 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_3 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_0_4 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_4 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_4 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_4 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_5 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_0 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_1 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_2 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_3 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_4 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_0_5 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_5 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_5 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_5 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_5 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_6 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_0 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(0,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_1 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(1,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_2 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(2,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_3 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(3,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_4 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(4,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_6_5 <- round(stats::dpois(6,UEFACL_fixtures$uefacl_xGH) * stats::dpois(5,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_0_6 <- round(stats::dpois(0,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_1_6 <- round(stats::dpois(1,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_2_6 <- round(stats::dpois(2,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_3_6 <- round(stats::dpois(3,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_4_6 <- round(stats::dpois(4,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
UEFACL_fixtures$uefacl_5_6 <- round(stats::dpois(5,UEFACL_fixtures$uefacl_xGH) * stats::dpois(6,UEFACL_fixtures$uefacl_xGA), digits = 4)
#Home win
UEFACL_fixtures$uefacl_H <- (
  UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
    UEFACL_fixtures$uefacl_5_0 + UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5
)

UEFACL_fixtures$uefacl_H <- percent(UEFACL_fixtures$uefacl_H, accuracy = 0.1)

#Draw
UEFACL_fixtures$uefacl_D <- (

  UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 + UEFACL_fixtures$uefacl_4_4 +
    UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6
)

UEFACL_fixtures$uefacl_D <- percent(UEFACL_fixtures$uefacl_D, accuracy = 0.1)

#Away

UEFACL_fixtures$uefacl_A <- (
  UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
    UEFACL_fixtures$uefacl_0_5 + UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6
)

UEFACL_fixtures$uefacl_A <- percent(UEFACL_fixtures$uefacl_A, accuracy = 0.1)

#ov25
UEFACL_fixtures$uefacl_ov25 <- (
  UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 + UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 + UEFACL_fixtures$uefacl_0_4 +
    UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 + UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_0 +
    UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 + UEFACL_fixtures$uefacl_0_5 +
    UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 + UEFACL_fixtures$uefacl_5_5 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5 + UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 +
    UEFACL_fixtures$uefacl_4_6 + UEFACL_fixtures$uefacl_5_6 + UEFACL_fixtures$uefacl_6_6
)
#un25
UEFACL_fixtures$uefacl_un25 <- (
  UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_0_2
)
#odds
UEFACL_fixtures$uefacl_ov25_odds <- round((1/UEFACL_fixtures$uefacl_ov25),digits = 2)
UEFACL_fixtures$uefacl_un25_odds <- round((1/UEFACL_fixtures$uefacl_un25),digits = 2)

UEFACL_fixtures$uefacl_ov25_odds
UEFACL_fixtures$uefacl_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
UEFACL_fixtures$uefacl_BTTSY <- (
  UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_3_1 + UEFACL_fixtures$uefacl_3_2 +
    UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_1_3 + UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_3_3 + UEFACL_fixtures$uefacl_4_4 +
    UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_3 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 +
    UEFACL_fixtures$uefacl_3_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 +
    UEFACL_fixtures$uefacl_5_4 + UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_6_6 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6
)
#BTTSN
UEFACL_fixtures$uefacl_BTTSN <- (
  UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_0_2 +
    UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_5_0 +
    UEFACL_fixtures$uefacl_0_5 + UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_0_6
)

UEFACL_fixtures$uefacl_BTTSY_odds <- round((1/UEFACL_fixtures$uefacl_BTTSY),digits = 2)
UEFACL_fixtures$uefacl_BTTSN_odds <- round((1/UEFACL_fixtures$uefacl_BTTSN),digits = 2)

UEFACL_fixtures$uefacl_BTTSY <- percent(UEFACL_fixtures$uefacl_BTTSY, accuracy = 0.1)
UEFACL_fixtures$uefacl_BTTSN <- percent(UEFACL_fixtures$uefacl_BTTSN, accuracy = 0.1)
#odds
UEFACL_fixtures$uefacl_BTTSY_odds
UEFACL_fixtures$uefacl_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
UEFACL_fixtures$uefacl_AH_0_H <- (
  UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
    UEFACL_fixtures$uefacl_5_0 +UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5 + UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6
)
#AH_0_A
UEFACL_fixtures$uefacl_AH_0_A <- (
  UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
    UEFACL_fixtures$uefacl_0_5 +UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6 + UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6
)

#odds
UEFACL_fixtures$uefacl_AH_0_H_odds <- round((1/UEFACL_fixtures$uefacl_AH_0_H),digits = 2)
UEFACL_fixtures$uefacl_AH_0_A_odds <- round((1/UEFACL_fixtures$uefacl_AH_0_A),digits = 2)

UEFACL_fixtures$uefacl_AH_0_H_odds
UEFACL_fixtures$uefacl_AH_0_A_odds
#percentages
UEFACL_fixtures$uefacl_AH_0_H <- percent(UEFACL_fixtures$uefacl_AH_0_H, accuracy = 0.1)
UEFACL_fixtures$uefacl_AH_0_A <- percent(UEFACL_fixtures$uefacl_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
UEFACL_fixtures$uefacl_AH_n075_H <- (
  UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
    UEFACL_fixtures$uefacl_5_0 +UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5
)
#AH_n075_A
UEFACL_fixtures$uefacl_AH_n075_A <- (
  UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
    UEFACL_fixtures$uefacl_0_5 +UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6
)

#odds
UEFACL_fixtures$uefacl_AH_n075_H_odds <- round((1/UEFACL_fixtures$uefacl_AH_n075_H),digits = 2)
UEFACL_fixtures$uefacl_AH_n075_A_odds <- round((1/UEFACL_fixtures$uefacl_AH_n075_A),digits = 2)

UEFACL_fixtures$uefacl_AH_n075_H_odds
UEFACL_fixtures$uefacl_AH_n075_A_odds
#percentages
UEFACL_fixtures$uefacl_AH_n075_H <- percent(UEFACL_fixtures$uefacl_AH_n075_H, accuracy = 0.1)
UEFACL_fixtures$uefacl_AH_n075_A <- percent(UEFACL_fixtures$uefacl_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
UEFACL_fixtures$uefacl_AH_075_H <- (
  UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
    UEFACL_fixtures$uefacl_5_0 +UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5 + UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6 + UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_1_2 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_3_4 + UEFACL_fixtures$uefacl_4_5 + UEFACL_fixtures$uefacl_5_6
)
#AH_075_A
UEFACL_fixtures$uefacl_AH_075_A <- (
  UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
    UEFACL_fixtures$uefacl_0_5 +UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6 + UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6 + UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_3 + UEFACL_fixtures$uefacl_5_4 + UEFACL_fixtures$uefacl_6_5
)

#odds
UEFACL_fixtures$uefacl_AH_075_H_odds <- round((1/UEFACL_fixtures$uefacl_AH_075_H),digits = 2)
UEFACL_fixtures$uefacl_AH_075_A_odds <- round((1/UEFACL_fixtures$uefacl_AH_075_A),digits = 2)

UEFACL_fixtures$uefacl_AH_075_H_odds
UEFACL_fixtures$uefacl_AH_075_A_odds
#percentages
UEFACL_fixtures$uefacl_AH_075_H <- percent(UEFACL_fixtures$uefacl_AH_075_H, accuracy = 0.1)
UEFACL_fixtures$uefacl_AH_075_A <- percent(UEFACL_fixtures$uefacl_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
UEFACL_fixtures$uefacl_AH_n125_H <- (
  UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
    UEFACL_fixtures$uefacl_5_0 +UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5
)
#AH_n125_A
UEFACL_fixtures$uefacl_AH_n125_A <- (
  UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
    UEFACL_fixtures$uefacl_0_5 +UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6
)

#odds
UEFACL_fixtures$uefacl_AH_n125_H_odds <- round((1/UEFACL_fixtures$uefacl_AH_n125_H),digits = 2)
UEFACL_fixtures$uefacl_AH_n125_A_odds <- round((1/UEFACL_fixtures$uefacl_AH_n125_A),digits = 2)

UEFACL_fixtures$uefacl_AH_n125_H_odds
UEFACL_fixtures$uefacl_AH_n125_A_odds
#percentages
UEFACL_fixtures$uefacl_AH_n125_H <- percent(UEFACL_fixtures$uefacl_AH_n125_H, accuracy = 0.1)
UEFACL_fixtures$uefacl_AH_n125_A <- percent(UEFACL_fixtures$uefacl_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
UEFACL_fixtures$uefacl_AH_125_H <- (
  UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
    UEFACL_fixtures$uefacl_5_0 +UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
    UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
    UEFACL_fixtures$uefacl_6_5 + UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6 + UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_1_2 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_3_4 + UEFACL_fixtures$uefacl_4_5 + UEFACL_fixtures$uefacl_5_6
)
#AH_125_A
UEFACL_fixtures$uefacl_AH_125_A <- (
  UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
    UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
    UEFACL_fixtures$uefacl_0_5 +UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
    UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
    UEFACL_fixtures$uefacl_5_6 + UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 +
    UEFACL_fixtures$uefacl_4_4 + UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6 + UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_1 +
    UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_3 + UEFACL_fixtures$uefacl_5_4 + UEFACL_fixtures$uefacl_6_5
)

#odds
UEFACL_fixtures$uefacl_AH_125_H_odds <- round((1/UEFACL_fixtures$uefacl_AH_125_H),digits = 2)
UEFACL_fixtures$uefacl_AH_125_A_odds <- round((1/UEFACL_fixtures$uefacl_AH_125_A),digits = 2)

UEFACL_fixtures$uefacl_AH_125_H_odds
UEFACL_fixtures$uefacl_AH_125_A_odds
#percentages
UEFACL_fixtures$uefacl_AH_125_H <- percent(UEFACL_fixtures$uefacl_AH_125_H, accuracy = 0.1)
UEFACL_fixtures$uefacl_AH_125_A <- percent(UEFACL_fixtures$uefacl_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
UEFACL_fixtures$uefacl_ov25 <- percent(UEFACL_fixtures$uefacl_ov25, accuracy = 0.1)

UEFACL_fixtures$uefacl_un25 <- percent(UEFACL_fixtures$uefacl_un25, accuracy = 0.1)
UEFACL_fixtures$uefacl_pscore <- paste(round(UEFACL_fixtures$uefacl_xGH,digits = 0),round(UEFACL_fixtures$uefacl_xGA,digits = 0),sep = "-")
############################################################################################################################################################
#Last six
uefacl_last_n_games <- 6

#create final_uefacl_hf object
final_uefacl_hf <- c()
for(index_uefacl_hf in 1:length(uefacl_teams))
{
  index_uefacl_hf <- row.names(uefacl_form_h) == uefacl_teams[index_uefacl_hf]
  form_uefacl_hf <- uefacl_form_h[index_uefacl_hf]
  deleted_form_uefacl_hf <- form_uefacl_hf[!form_uefacl_hf[] == ""]
  l6_form_uefacl_hf <- tail(deleted_form_uefacl_hf,uefacl_last_n_games)
  l6_form_uefacl_hf <- paste(l6_form_uefacl_hf,collapse = " ")
  final_uefacl_hf[index_uefacl_hf] <- rbind(paste(uefacl_teams[index_uefacl_hf],l6_form_uefacl_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}

#change column nam
final_uefacl_hf <- as.data.frame(final_uefacl_hf)
colnames(final_uefacl_hf) <- "Form"
#goals scored
#create final_uefacl_gs object
final_uefacl_gs <- c()
suml6_uefacl_gs <- c()
for(index_uefacl_gs in 1:length(uefacl_teams))
{
  index_uefacl_gs <- row.names(uefacl_goalscored_h) == uefacl_teams[index_uefacl_gs]
  form_uefacl_gs <- uefacl_goalscored_h[index_uefacl_gs]
  deleted_form_uefacl_gs <- form_uefacl_gs[!form_uefacl_gs[] == ""]
  l6_form_uefacl_gs <- tail(deleted_form_uefacl_gs,uefacl_last_n_games)
  l6_form_uefacl_gs <- as.numeric(l6_form_uefacl_gs)
  suml6_uefacl_gs[index_uefacl_gs] <- sum(l6_form_uefacl_gs)
  suml6_uefacl_gs[index_uefacl_gs] <- paste("(",suml6_uefacl_gs[index_uefacl_gs],")",sep = "")
  l6_form_uefacl_gs <- paste(l6_form_uefacl_gs,collapse = " ")
  final_uefacl_gs[index_uefacl_gs] <- rbind(paste(uefacl_teams[index_uefacl_gs],l6_form_uefacl_gs,suml6_uefacl_gs[index_uefacl_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
final_uefacl_gs
#change column names
final_uefacl_gs <- as.data.frame(final_uefacl_gs)
colnames(final_uefacl_gs) <- "Goals scored"
#goal conceded
#create final_uefacl_gc object
final_uefacl_gc <- c()
suml6_uefacl_gc <- c()
for(index_uefacl_gc in 1:length(uefacl_teams))
{
  index_uefacl_gc <- row.names(uefacl_goalconceded_h) == uefacl_teams[index_uefacl_gc]
  form_uefacl_gc <- uefacl_goalconceded_h[index_uefacl_gc]
  deleted_form_uefacl_gc <- form_uefacl_gc[!form_uefacl_gc[] == ""]
  l6_form_uefacl_gc <- tail(deleted_form_uefacl_gc,uefacl_last_n_games)
  l6_form_uefacl_gc <- as.numeric(l6_form_uefacl_gc)
  suml6_uefacl_gc[index_uefacl_gc] <- sum(l6_form_uefacl_gc)
  suml6_uefacl_gc[index_uefacl_gc] <- paste("(",suml6_uefacl_gc[index_uefacl_gc],")",sep = "")
  l6_form_uefacl_gc <- paste(l6_form_uefacl_gc,collapse = " ")
  final_uefacl_gc[index_uefacl_gc] <- rbind(paste(uefacl_teams[index_uefacl_gc],l6_form_uefacl_gc,suml6_uefacl_gc[index_uefacl_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
#change column names
final_uefacl_gc <- as.data.frame(final_uefacl_gc)
colnames(final_uefacl_gc) <- "Goals conceded"


toString(l6_form_uefacl_gc)
#total goals
#create final_uefacl_tg object
final_uefacl_tg <- c()
suml6_uefacl_tg <- c()
for(index_uefacl_tg in 1:length(uefacl_teams))
{
  index_uefacl_tg <- row.names(uefacl_totalgoals_h) == uefacl_teams[index_uefacl_tg]
  form_uefacl_tg <- uefacl_totalgoals_h[index_uefacl_tg]
  deleted_form_uefacl_tg <- form_uefacl_tg[!form_uefacl_tg[] == ""]
  l6_form_uefacl_tg <- tail(deleted_form_uefacl_tg,uefacl_last_n_games)
  l6_form_uefacl_tg <- as.numeric(l6_form_uefacl_tg)
  suml6_uefacl_tg[index_uefacl_tg] <- sum(l6_form_uefacl_tg)
  suml6_uefacl_tg[index_uefacl_tg] <- paste("(",suml6_uefacl_tg[index_uefacl_tg],")",sep = "")
  l6_form_uefacl_tg <- paste(l6_form_uefacl_tg,collapse = " ")
  final_uefacl_tg[index_uefacl_tg] <- rbind(paste(uefacl_teams[index_uefacl_tg],l6_form_uefacl_tg,suml6_uefacl_tg[index_uefacl_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
#change column names
final_uefacl_tg <- as.data.frame(final_uefacl_tg)
colnames(final_uefacl_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_uefacl_hf object
final_uefacl_cs <- c()
for(index_uefacl_cs in 1:length(uefacl_teams))
{
  index_uefacl_cs <- row.names(uefacl_csform_h) == uefacl_teams[index_uefacl_cs]
  csform_uefacl_cs <- uefacl_csform_h[index_uefacl_cs]
  deleted_csform_uefacl_cs <- csform_uefacl_cs[!csform_uefacl_cs[] == ""]
  l6_csform_uefacl_cs <- tail(deleted_csform_uefacl_cs,uefacl_last_n_games)
  l6_csform_uefacl_cs <- paste(l6_csform_uefacl_cs,collapse = " ")
  final_uefacl_cs[index_uefacl_cs] <- rbind(paste(uefacl_teams[index_uefacl_cs],l6_csform_uefacl_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_csform)

}

#change column names
final_uefacl_cs <- as.data.frame(final_uefacl_cs)
colnames(final_uefacl_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_uefacl_wm object
final_uefacl_wm <- c()
suml6_uefacl_wm <- c()
for(index_uefacl_wm in 1:length(uefacl_teams))
{
  index_uefacl_wm <- row.names(uefacl_winmargin_h) == uefacl_teams[index_uefacl_wm]
  form_uefacl_wm <- uefacl_winmargin_h[index_uefacl_wm]
  deleted_form_uefacl_wm <- form_uefacl_wm[!form_uefacl_wm[] == ""]
  l6_form_uefacl_wm <- tail(deleted_form_uefacl_wm,uefacl_last_n_games)
  l6_form_uefacl_wm <- as.numeric(l6_form_uefacl_wm)
  suml6_uefacl_wm[index_uefacl_wm] <- sum(l6_form_uefacl_wm)
  suml6_uefacl_wm[index_uefacl_wm] <- paste("(",suml6_uefacl_wm[index_uefacl_wm],")",sep = "")
  l6_form_uefacl_wm <- paste(l6_form_uefacl_wm,collapse = " ")
  final_uefacl_wm[index_uefacl_wm] <- rbind(paste(uefacl_teams[index_uefacl_wm],l6_form_uefacl_wm,suml6_uefacl_wm[index_uefacl_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
final_uefacl_wm
#change column names
final_uefacl_wm <- as.data.frame(final_uefacl_wm)
colnames(final_uefacl_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_uefacl_ca object
final_uefacl_ca <- c()
suml6_uefacl_ca <- c()
for(index_uefacl_ca in 1:length(uefacl_teams))
{
  index_uefacl_ca <- row.names(uefacl_coawarded_h) == uefacl_teams[index_uefacl_ca]
  form_uefacl_ca <- uefacl_coawarded_h[index_uefacl_ca]
  deleted_form_uefacl_ca <- form_uefacl_ca[!form_uefacl_ca[] == ""]
  l6_form_uefacl_ca <- tail(deleted_form_uefacl_ca,uefacl_last_n_games)
  l6_form_uefacl_ca <- as.numeric(l6_form_uefacl_ca)
  suml6_uefacl_ca[index_uefacl_ca] <- sum(l6_form_uefacl_ca)
  suml6_uefacl_ca[index_uefacl_ca] <- paste("(",suml6_uefacl_ca[index_uefacl_ca],")",sep = "")
  l6_form_uefacl_ca <- paste(l6_form_uefacl_ca,collapse = " ")
  final_uefacl_ca[index_uefacl_ca] <- rbind(paste(uefacl_teams[index_uefacl_ca],l6_form_uefacl_ca,suml6_uefacl_ca[index_uefacl_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
final_uefacl_ca
#change column names
final_uefacl_ca <- as.data.frame(final_uefacl_ca)
colnames(final_uefacl_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_uefacl_ca object
final_uefacl_cc <- c()
suml6_uefacl_cc <- c()
for(index_uefacl_cc in 1:length(uefacl_teams))
{
  index_uefacl_cc <- row.names(uefacl_cornersconceded_h) == uefacl_teams[index_uefacl_cc]
  form_uefacl_cc <- uefacl_cornersconceded_h[index_uefacl_cc]
  deleted_form_uefacl_cc <- form_uefacl_cc[!form_uefacl_cc[] == ""]
  l6_form_uefacl_cc <- tail(deleted_form_uefacl_cc,uefacl_last_n_games)
  l6_form_uefacl_cc <- as.numeric(l6_form_uefacl_cc)
  suml6_uefacl_cc[index_uefacl_cc] <- sum(l6_form_uefacl_cc)
  suml6_uefacl_cc[index_uefacl_cc] <- paste("(",suml6_uefacl_cc[index_uefacl_cc],")",sep = "")
  l6_form_uefacl_cc <- paste(l6_form_uefacl_cc,collapse = " ")
  final_uefacl_cc[index_uefacl_cc] <- rbind(paste(uefacl_teams[index_uefacl_cc],l6_form_uefacl_cc,suml6_uefacl_cc[index_uefacl_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
final_uefacl_cc
#change column names
final_uefacl_cc <- as.data.frame(final_uefacl_cc)
colnames(final_uefacl_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_uefacl_cosc <- c()
for(index_uefacl_cosc in 1:length(uefacl_teams))
{
  index_uefacl_cosc <- row.names(uefacl_coscform_h) == uefacl_teams[index_uefacl_cosc]
  coscform_uefacl_cosc <- uefacl_coscform_h[index_uefacl_cosc]
  deleted_coscform_uefacl_cosc <- coscform_uefacl_cosc[!coscform_uefacl_cosc[] == ""]
  l6_coscform_uefacl_cosc <- tail(deleted_coscform_uefacl_cosc,uefacl_last_n_games)
  l6_coscform_uefacl_cosc <- paste(l6_coscform_uefacl_cosc,collapse = " ")
  final_uefacl_cosc[index_uefacl_cosc] <- rbind(paste(uefacl_teams[index_uefacl_cosc],l6_coscform_uefacl_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_coscform)

}
final_uefacl_cosc
#change column names
final_uefacl_cosc <- as.data.frame(final_uefacl_cosc)
colnames(final_uefacl_cosc) <- "CornersForm"
##################################################
#total corners
#create final_uefacl_tcorners object
final_uefacl_tcorners <- c()
suml6_uefacl_tcorners <- c()
for(index_uefacl_tcorners in 1:length(uefacl_teams))
{
  index_uefacl_tcorners <- row.names(uefacl_totalcorners_h) == uefacl_teams[index_uefacl_tcorners]
  form_uefacl_tcorners <- uefacl_totalcorners_h[index_uefacl_tcorners]
  deleted_form_uefacl_tcorners <- form_uefacl_tcorners[!form_uefacl_tcorners[] == ""]
  l6_form_uefacl_tcorners <- tail(deleted_form_uefacl_tcorners,uefacl_last_n_games)
  l6_form_uefacl_tcorners <- as.numeric(l6_form_uefacl_tcorners)
  suml6_uefacl_tcorners[index_uefacl_tcorners] <- sum(l6_form_uefacl_tcorners)
  suml6_uefacl_tcorners[index_uefacl_tcorners] <- paste("(",suml6_uefacl_tcorners[index_uefacl_tcorners],")",sep = "")
  l6_form_uefacl_tcorners <- paste(l6_form_uefacl_tcorners,collapse = " ")
  final_uefacl_tcorners[index_uefacl_tcorners] <- rbind(paste(uefacl_teams[index_uefacl_tcorners],l6_form_uefacl_tcorners,suml6_uefacl_tcorners[index_uefacl_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
#change column names
final_uefacl_tcorners <- as.data.frame(final_uefacl_tcorners)
colnames(final_uefacl_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_uefacl_hf_against
final_uefacl_hf_against <- c()
for(index_uefacl_hf_against in 1:length(uefacl_teams))
{
  index_uefacl_hf_against <- row.names(uefacl_form_team_against_h) == uefacl_teams[index_uefacl_hf_against]
  form_uefacl_hf_against <- uefacl_form_team_against_h[index_uefacl_hf_against]
  deleted_form_uefacl_hf_against <- form_uefacl_hf_against[!form_uefacl_hf_against[] == ""]
  l6_form_uefacl_hf_against <- tail(deleted_form_uefacl_hf_against,uefacl_last_n_games)
  l6_form_uefacl_hf_against <- paste(l6_form_uefacl_hf_against,collapse = " ")
  final_uefacl_hf_against[index_uefacl_hf_against] <- rbind(paste(uefacl_teams[index_uefacl_hf_against],l6_form_uefacl_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefacl_teams[index],l6_form)

}
final_uefacl_hf_against <- as.data.frame(final_uefacl_hf_against)
colnames(final_uefacl_hf_against) <- "Team against"
#combine the columns
final_uefacl_all <- cbind(final_uefacl_hf,final_uefacl_gs,final_uefacl_gc,final_uefacl_tg,final_uefacl_ca,final_uefacl_cc,final_uefacl_tcorners,final_uefacl_cosc,final_uefacl_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
#UEFACL
UEFACL_sim <- UEFACL
UEFACL_sim$matchid <- paste(UEFACL_sim$HomeTeam,UEFACL_sim$AwayTeam,sep = "-")
UEFACL_fixtures$matchid <- paste(UEFACL_fixtures$HomeTeam_uefacl,UEFACL_fixtures$AwayTeam_uefacl,sep = "-")
UEFACL_fixtures$uefacl_FTR <- sapply(UEFACL_fixtures$uefacl_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

UEFACL_fixtures$uefacl_gamestatus <- ifelse(UEFACL_fixtures$matchid %in% UEFACL_sim$matchid,"played","notplayed")

uefacl_home_wins_sim <- c()
uefacl_away_wins_sim <- c()
uefacl_home_draws_sim <- c()
uefacl_away_draws_sim <- c()
uefacl_home_loss_sim <- c()
uefacl_away_loss_sim <- c()



for (i_uefacl_wins_sim in 1:length(uefacl_teams))
{

  uefacl_home_wins_sim[i_uefacl_wins_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$HomeTeam_uefacl == uefacl_teams[i_uefacl_wins_sim] & UEFACL_fixtures$uefacl_FTR == "H" & UEFACL_fixtures$uefacl_gamestatus =="notplayed",])
  uefacl_away_wins_sim[i_uefacl_wins_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$AwayTeam_uefacl == uefacl_teams[i_uefacl_wins_sim] & UEFACL_fixtures$uefacl_FTR == "A" & UEFACL_fixtures$uefacl_gamestatus == "notplayed",])
  uefacl_home_draws_sim[i_uefacl_wins_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$HomeTeam_uefacl == uefacl_teams[i_uefacl_wins_sim] & UEFACL_fixtures$uefacl_FTR == "D" & UEFACL_fixtures$uefacl_gamestatus == "notplayed",])
  uefacl_away_draws_sim[i_uefacl_wins_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$AwayTeam_uefacl == uefacl_teams[i_uefacl_wins_sim] & UEFACL_fixtures$uefacl_FTR == "D" & UEFACL_fixtures$uefacl_gamestatus == "notplayed",])
  uefacl_home_loss_sim[i_uefacl_wins_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$HomeTeam_uefacl == uefacl_teams[i_uefacl_wins_sim] & UEFACL_fixtures$uefacl_FTR == "A" & UEFACL_fixtures$uefacl_gamestatus == "notplayed",])
  uefacl_away_loss_sim[i_uefacl_wins_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$AwayTeam_uefacl == uefacl_teams[i_uefacl_wins_sim] & UEFACL_fixtures$uefacl_FTR == "H" & UEFACL_fixtures$uefacl_gamestatus == "notplayed", ])

}

uefacl_total_wins_sim <- uefacl_home_wins_sim + uefacl_away_wins_sim
uefacl_total_draws_sim <- uefacl_home_draws_sim + uefacl_away_draws_sim
uefacl_total_loss_sim <- uefacl_home_loss_sim + uefacl_away_loss_sim

uefacl_home_games_sim <- c()
uefacl_away_games_sim <-c()

for (i_uefacl_sim in 1:length(uefacl_teams))
{

  uefacl_home_games_sim[i_uefacl_sim] <- nrow(UEFACL_fixtures[UEFACL_fixtures$HomeTeam_uefacl == uefacl_teams[i_uefacl_sim] & UEFACL_fixtures$uefacl_gamestatus == "notplayed",])
  uefacl_away_games_sim[i_uefacl_sim]  <- nrow(UEFACL_fixtures[UEFACL_fixtures$AwayTeam_uefacl == uefacl_teams[i_uefacl_sim] & UEFACL_fixtures$uefacl_gamestatus == "notplayed",])

}

uefacl_games_played_sim <- uefacl_home_games_sim + uefacl_away_games_sim

uefacl_league_table_sim <- cbind(uefacl_teams,uefacl_games_played_sim,uefacl_total_wins_sim,uefacl_total_draws_sim,uefacl_total_loss_sim)
uefacl_PTS_sim <- (uefacl_total_wins_sim*3) + (uefacl_total_draws_sim*1)
uefacl_league_table_sim <- cbind(uefacl_league_table_sim,uefacl_PTS_sim)

uefacl_games_played_simfinal <- uefacl_games_played + uefacl_games_played_sim
uefacl_total_wins_simfinal <- uefacl_total_wins + uefacl_total_wins_sim
uefacl_total_draws_simfinal <- uefacl_total_draws + uefacl_total_draws_sim
uefacl_total_loss_simfinal <- uefacl_total_loss + uefacl_total_loss_sim
uefacl_PTS_simfinal <- uefacl_PTS + uefacl_PTS_sim

uefacl_league_table_simfinal <- cbind(uefacl_teams,uefacl_games_played_simfinal,uefacl_total_wins_simfinal,uefacl_total_draws_simfinal,uefacl_total_loss_simfinal,uefacl_PTS_simfinal)
uefacl_league_table_simfinal <- as.data.frame(uefacl_league_table_simfinal)
names(uefacl_league_table_simfinal)[names(uefacl_league_table_simfinal) == "uefacl_teams"] <- "Team_f"
names(uefacl_league_table_simfinal)[names(uefacl_league_table_simfinal) == "uefacl_games_played_simfinal"] <- "P_f"
names(uefacl_league_table_simfinal)[names(uefacl_league_table_simfinal) == "uefacl_total_wins_simfinal"] <- "W_f"
names(uefacl_league_table_simfinal)[names(uefacl_league_table_simfinal) == "uefacl_total_draws_simfinal"] <- "D_f"
names(uefacl_league_table_simfinal)[names(uefacl_league_table_simfinal) == "uefacl_total_loss_simfinal"] <- "L_f"
names(uefacl_league_table_simfinal)[names(uefacl_league_table_simfinal) == "uefacl_PTS_simfinal"] <- "PTS_f"
points_uefacl_sim <-  uefacl_league_table_simfinal[order(as.numeric(uefacl_league_table_simfinal$PTS_f), decreasing = TRUE),]

UEFACL_notplayed <- UEFACL_fixtures[UEFACL_fixtures$uefacl_gamestatus == "notplayed",]
###########################################################################################################################################################
#decision model
#UEFACL
UEFACL_fixtures$Hometeam_uefacl_index <- match(UEFACL_fixtures$HomeTeam_uefacl,uefacl_teams)
UEFACL_fixtures$Awayteam_uefacl_index <- match(UEFACL_fixtures$AwayTeam_uefacl,uefacl_teams)
uefacl_prediction <- c()
uefacl_HWM <- c()
uefacl_AWM <- c()
uefacl_HWMLM <- c()
uefacl_AWMLM <- c()
uefacl_HY <- c()
uefacl_AY <- c()
uefacl_HCO <- c()
uefacl_ACO <- c()
uefacl_HXSC <- c()
uefacl_AXSC <- c()
uefacl_HYCPF <- c()
uefacl_AYCPF <- c()
for(uefacl_row in 1:nrow(UEFACL_fixtures))
{

  uefacl_hometeamindex <- UEFACL_fixtures[uefacl_row,"Hometeam_uefacl_index"]
  uefacl_awayteamindex <- UEFACL_fixtures[uefacl_row,"Awayteam_uefacl_index"]
  #analyse team form
  #home team
  uefacl_form_vec_ht <- as.vector(uefacl_form_h[uefacl_hometeamindex,])
  uefacl_form_vec_ht[is.na(uefacl_form_vec_ht)] <- ""
  uefacl_form_vec_ht <- uefacl_form_vec_ht[uefacl_form_vec_ht != ""]
  uefacl_form_vec_ht  <-tail(uefacl_form_vec_ht,6)
  uefacl_ht_numberof_wins <- length(which(uefacl_form_vec_ht == "W"))
  uefacl_ht_numberof_draws <- length(which(uefacl_form_vec_ht == "D"))
  uefacl_ht_numberof_loss <- length(which(uefacl_form_vec_ht == "L"))
  #awayteam
  uefacl_form_vec_at <- as.vector(uefacl_form_h[uefacl_awayteamindex,])
  uefacl_form_vec_at[is.na(uefacl_form_vec_at)] <- ""
  uefacl_form_vec_at <- uefacl_form_vec_at[uefacl_form_vec_at != ""]
  uefacl_form_vec_at  <-tail(uefacl_form_vec_at,6)
  uefacl_at_numberof_wins <- length(which(uefacl_form_vec_at == "W"))
  uefacl_at_numberof_draws <- length(which(uefacl_form_vec_at == "D"))
  uefacl_at_numberof_loss <- length(which(uefacl_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  uefacl_goalscored_vec_ht <- as.vector(uefacl_goalscored_h[uefacl_hometeamindex,])
  uefacl_goalscored_vec_ht[is.na(uefacl_goalscored_vec_ht)] <- ""
  uefacl_goalscored_vec_ht <- uefacl_goalscored_vec_ht[uefacl_goalscored_vec_ht != ""]
  uefacl_goalscored_vec_ht  <-tail(uefacl_goalscored_vec_ht,6)
  uefacl_goalscored_vec_ht  <- as.numeric(uefacl_goalscored_vec_ht)
  uefacl_ht_totalgoalscored <- sum(uefacl_goalscored_vec_ht)
  uefacl_ht_matches_scoring <- length(which(uefacl_goalscored_vec_ht > 0))
  uefacl_ht_matches_without_scoring <- length(which(uefacl_goalscored_vec_ht == "0"))
  #awayteam
  uefacl_goalscored_vec_at <- as.vector(uefacl_goalscored_h[uefacl_awayteamindex,])
  uefacl_goalscored_vec_at[is.na(uefacl_goalscored_vec_at)] <- ""
  uefacl_goalscored_vec_at <- uefacl_goalscored_vec_at[uefacl_goalscored_vec_at != ""]
  uefacl_goalscored_vec_at  <-tail(uefacl_goalscored_vec_at,6)
  uefacl_goalscored_vec_at  <- as.numeric(uefacl_goalscored_vec_at)
  uefacl_at_totalgoalscored <- sum(uefacl_goalscored_vec_at)
  uefacl_at_matches_scoring <- length(which(uefacl_goalscored_vec_at > 0))
  uefacl_at_matches_without_scoring <- length(which(uefacl_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  uefacl_goalconceded_vec_ht <- as.vector(uefacl_goalconceded_h[uefacl_hometeamindex,])
  uefacl_goalconceded_vec_ht[is.na(uefacl_goalconceded_vec_ht)] <- ""
  uefacl_goalconceded_vec_ht <- uefacl_goalconceded_vec_ht[uefacl_goalconceded_vec_ht != ""]
  uefacl_goalconceded_vec_ht  <-tail(uefacl_goalconceded_vec_ht,6)
  uefacl_goalconceded_vec_ht  <- as.numeric(uefacl_goalconceded_vec_ht)
  uefacl_goalconceded_vec_ht
  uefacl_ht_totalgoalconceded <- sum(uefacl_goalconceded_vec_ht)
  uefacl_ht_matches_concede <- length(which(uefacl_goalconceded_vec_ht > 0))
  uefacl_ht_matches_without_concede <- length(which(uefacl_goalconceded_vec_ht == "0"))
  #awayteam
  uefacl_goalconceded_vec_at <- as.vector(uefacl_goalconceded_h[uefacl_awayteamindex,])
  uefacl_goalconceded_vec_at[is.na(uefacl_goalconceded_vec_at)] <- ""
  uefacl_goalconceded_vec_at <- uefacl_goalconceded_vec_at[uefacl_goalconceded_vec_at != ""]
  uefacl_goalconceded_vec_at  <-tail(uefacl_goalconceded_vec_at,6)
  uefacl_goalconceded_vec_at  <- as.numeric(uefacl_goalconceded_vec_at)
  uefacl_at_totalgoalconceded <- sum(uefacl_goalconceded_vec_at)
  uefacl_at_matches_concede <- length(which(uefacl_goalconceded_vec_at > 0))
  uefacl_at_matches_without_concede <- length(which(uefacl_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  uefacl_totalgoals_vec_ht <- as.vector(uefacl_totalgoals_h[uefacl_hometeamindex,])
  uefacl_totalgoals_vec_ht[is.na(uefacl_totalgoals_vec_ht)] <- ""
  uefacl_totalgoals_vec_ht <- uefacl_totalgoals_vec_ht[uefacl_totalgoals_vec_ht != ""]
  uefacl_totalgoals_vec_ht  <-tail(uefacl_totalgoals_vec_ht,6)
  uefacl_totalgoals_vec_ht  <- as.numeric(uefacl_totalgoals_vec_ht)
  uefacl_totalgoals_vec_ht
  uefacl_ht_totalgoals <- sum(uefacl_totalgoals_vec_ht)
  uefacl_ht_avgtotalgoals <- (uefacl_ht_totalgoals/6)
  uefacl_ht_no_of_ov25 <- length(which(uefacl_totalgoals_vec_ht >= 3))
  uefacl_ht_no_of_un25 <- length(which(uefacl_totalgoals_vec_ht <= 2))
  #awayteam
  uefacl_totalgoals_vec_at <- as.vector(uefacl_totalgoals_h[uefacl_awayteamindex,])
  uefacl_totalgoals_vec_at[is.na(uefacl_totalgoals_vec_at)] <- ""
  uefacl_totalgoals_vec_at <- uefacl_totalgoals_vec_at[uefacl_totalgoals_vec_at != ""]
  uefacl_totalgoals_vec_at  <-tail(uefacl_totalgoals_vec_at,6)
  uefacl_totalgoals_vec_at  <- as.numeric(uefacl_totalgoals_vec_at)
  uefacl_totalgoals_vec_at
  uefacl_at_totalgoals <- sum(uefacl_totalgoals_vec_at)
  uefacl_at_avgtotalgoals <- (uefacl_at_totalgoals/6)
  uefacl_at_no_of_ov25 <- length(which(uefacl_totalgoals_vec_at >= 3))
  uefacl_at_no_of_un25 <- length(which(uefacl_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  uefacl_winmargin_vec_ht <- as.vector(uefacl_winmargin_h[uefacl_hometeamindex,])
  uefacl_winmargin_vec_ht[is.na(uefacl_winmargin_vec_ht)] <- ""
  uefacl_winmargin_vec_ht <- uefacl_winmargin_vec_ht[uefacl_winmargin_vec_ht != ""]
  uefacl_winmargin_vec_ht  <-tail(uefacl_winmargin_vec_ht,6)
  uefacl_winmargin_vec_ht  <- as.numeric(uefacl_winmargin_vec_ht)

  uefacl_ht_totalwinmargin <- sum(uefacl_winmargin_vec_ht)
  uefacl_ht_no_of_winmargin_ov0 <- length(which(uefacl_winmargin_vec_ht >= 0))
  uefacl_ht_no_of_winmargin_ov1 <- length(which(uefacl_winmargin_vec_ht >= 1))
  uefacl_ht_no_of_winmargin_un0 <- length(which(uefacl_winmargin_vec_ht <= 0))
  uefacl_ht_no_of_winmargin_un1 <- length(which(uefacl_winmargin_vec_ht <= 1))
  #awayteam
  uefacl_winmargin_vec_at <- as.vector(uefacl_winmargin_h[uefacl_awayteamindex,])
  uefacl_winmargin_vec_at[is.na(uefacl_winmargin_vec_at)] <- ""
  uefacl_winmargin_vec_at <- uefacl_winmargin_vec_at[uefacl_winmargin_vec_at != ""]
  uefacl_winmargin_vec_at  <-tail(uefacl_winmargin_vec_at,6)
  uefacl_winmargin_vec_at  <- as.numeric(uefacl_winmargin_vec_at)

  uefacl_at_totalwinmargin <- sum(uefacl_winmargin_vec_at)
  uefacl_at_no_of_winmargin_ov0 <- length(which(uefacl_winmargin_vec_at >= 0))
  uefacl_at_no_of_winmargin_ov1 <- length(which(uefacl_winmargin_vec_at >= 1))
  uefacl_at_no_of_winmargin_un0 <- length(which(uefacl_winmargin_vec_at <= 0))
  uefacl_at_no_of_winmargin_un1 <- length(which(uefacl_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  uefacl_winmargin_vec_ht_lm <- as.vector(uefacl_winmargin_h[uefacl_hometeamindex,])
  uefacl_winmargin_vec_ht_lm[is.na(uefacl_winmargin_vec_ht_lm)] <- ""
  uefacl_winmargin_vec_ht_lm <- uefacl_winmargin_vec_ht_lm[uefacl_winmargin_vec_ht_lm != ""]
  uefacl_winmargin_vec_ht_lm  <-tail(uefacl_winmargin_vec_ht_lm,1)
  #awayteam
  uefacl_winmargin_vec_at_lm <- as.vector(uefacl_winmargin_h[uefacl_awayteamindex,])
  uefacl_winmargin_vec_at_lm[is.na(uefacl_winmargin_vec_at_lm)] <- ""
  uefacl_winmargin_vec_at_lm <- uefacl_winmargin_vec_at_lm[uefacl_winmargin_vec_at_lm != ""]
  uefacl_winmargin_vec_at_lm  <-tail(uefacl_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  uefacl_yellowtotals_vec_ht <- as.vector(uefacl_yellowtotalsv2[uefacl_hometeamindex,])
  uefacl_yellowtotals_vec_ht[is.na(uefacl_yellowtotals_vec_ht)] <- ""
  uefacl_yellowtotals_vec_ht <- uefacl_yellowtotals_vec_ht[uefacl_yellowtotals_vec_ht != ""]
  uefacl_yellowtotals_vec_ht  <-tail(uefacl_yellowtotals_vec_ht,1)
  #awayteam
  uefacl_yellowtotals_vec_at <- as.vector(uefacl_yellowtotalsv2[uefacl_awayteamindex,])
  uefacl_yellowtotals_vec_at[is.na(uefacl_yellowtotals_vec_at)] <- ""
  uefacl_yellowtotals_vec_at <- uefacl_yellowtotals_vec_at[uefacl_yellowtotals_vec_at != ""]
  uefacl_yellowtotals_vec_at  <-tail(uefacl_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  uefacl_cornertotals_vec_ht <- as.vector(uefacl_cornertotalsv2[uefacl_hometeamindex,])
  uefacl_cornertotals_vec_ht[is.na(uefacl_cornertotals_vec_ht)] <- ""
  uefacl_cornertotals_vec_ht <- uefacl_cornertotals_vec_ht[uefacl_cornertotals_vec_ht != ""]
  uefacl_cornertotals_vec_ht  <-tail(uefacl_cornertotals_vec_ht,1)
  #awayteam
  uefacl_cornertotals_vec_at <- as.vector(uefacl_cornertotalsv2[uefacl_awayteamindex,])
  uefacl_cornertotals_vec_at[is.na(uefacl_cornertotals_vec_at)] <- ""
  uefacl_cornertotals_vec_at <- uefacl_cornertotals_vec_at[uefacl_cornertotals_vec_at != ""]
  uefacl_cornertotals_vec_at  <-tail(uefacl_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  uefacl_xshotsconversion_vec_ht <- as.vector(uefacl_shots_analysis[uefacl_hometeamindex,])
  uefacl_xshotsconversion_vec_ht[is.na(uefacl_xshotsconversion_vec_ht)] <- ""
  uefacl_xshotsconversion_vec_ht <- uefacl_xshotsconversion_vec_ht[uefacl_xshotsconversion_vec_ht != ""]
  uefacl_xshotsconversion_vec_ht  <-tail(uefacl_xshotsconversion_vec_ht,1)
  #awayteam
  uefacl_xshotsconversion_vec_at <- as.vector(uefacl_shots_analysis[uefacl_awayteamindex,])
  uefacl_xshotsconversion_vec_at[is.na(uefacl_xshotsconversion_vec_at)] <- ""
  uefacl_xshotsconversion_vec_at <- uefacl_xshotsconversion_vec_at[uefacl_xshotsconversion_vec_at != ""]
  uefacl_xshotsconversion_vec_at  <-tail(uefacl_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  uefacl_fouls_conversion_vec_ht <- as.vector(uefacl_fouls_conversion[uefacl_hometeamindex,])
  uefacl_fouls_conversion_vec_ht[is.na(uefacl_fouls_conversion_vec_ht)] <- ""
  uefacl_fouls_conversion_vec_ht <- uefacl_fouls_conversion_vec_ht[uefacl_fouls_conversion_vec_ht != ""]
  uefacl_fouls_conversion_vec_ht  <-tail(uefacl_fouls_conversion_vec_ht,1)
  #awayteam
  uefacl_fouls_conversion_vec_at <- as.vector(uefacl_fouls_conversion[uefacl_awayteamindex,])
  uefacl_fouls_conversion_vec_at[is.na(uefacl_fouls_conversion_vec_at)] <- ""
  uefacl_fouls_conversion_vec_at <- uefacl_fouls_conversion_vec_at[uefacl_fouls_conversion_vec_at != ""]
  uefacl_fouls_conversion_vec_at  <-tail(uefacl_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  uefacl_ht_last6points <- uefacl_ht_numberof_wins*3 + uefacl_ht_numberof_draws*1
  uefacl_at_last6points <- uefacl_at_numberof_wins*3 + uefacl_at_numberof_draws*1

  if(uefacl_ht_last6points > uefacl_at_last6points) {uefacl_3waypick <- "1"}  else {uefacl_3waypick <- "X2"}

  if(uefacl_at_last6points > uefacl_ht_last6points ) {uefacl_3waypick <- "2"} else {uefacl_3waypick <- "1X"}

  if(uefacl_ht_no_of_ov25 + uefacl_at_no_of_ov25 >= 6) {uefacl_goalspick <- "ov25"} else {uefacl_goalspick <- "un25"}

  if(uefacl_ht_no_of_un25 + uefacl_at_no_of_un25 >= 6) {uefacl_goalspick <- "un25"} else {uefacl_goalspick <- "ov25"}

  if(uefacl_ht_matches_scoring >= 4 && uefacl_at_matches_scoring >=4) {uefacl_btts <- "BTTS-Y"} else {uefacl_btts <- "BTTS-N"}


  uefacl_prediction[uefacl_row] <- rbind(paste(uefacl_3waypick,uefacl_goalspick,uefacl_btts,sep = ","))
  uefacl_HWM[uefacl_row] <- uefacl_ht_totalwinmargin
  uefacl_AWM[uefacl_row] <- uefacl_at_totalwinmargin

  uefacl_HWMLM[uefacl_row] <- uefacl_winmargin_vec_ht_lm
  uefacl_AWMLM[uefacl_row] <- uefacl_winmargin_vec_at_lm

  uefacl_HY[uefacl_row] <- uefacl_yellowtotals_vec_ht
  uefacl_AY[uefacl_row] <- uefacl_yellowtotals_vec_at

  uefacl_HCO[uefacl_row] <- uefacl_cornertotals_vec_ht
  uefacl_ACO[uefacl_row] <- uefacl_cornertotals_vec_at

  uefacl_HXSC[uefacl_row] <- uefacl_xshotsconversion_vec_ht
  uefacl_AXSC[uefacl_row] <- uefacl_xshotsconversion_vec_at

  uefacl_HYCPF[uefacl_row] <- uefacl_fouls_conversion_vec_ht
  uefacl_AYCPF[uefacl_row] <- uefacl_fouls_conversion_vec_at
}

uefacl_prediction <- as.data.frame(uefacl_prediction)
colnames(uefacl_prediction) <- "prediction"

uefacl_HWM <- as.data.frame(uefacl_HWM)
colnames(uefacl_HWM) <- "HWM"

uefacl_AWM <- as.data.frame(uefacl_AWM)
colnames(uefacl_AWM) <- "AWM"

uefacl_HWMLM <- as.data.frame(uefacl_HWMLM)
colnames(uefacl_HWMLM) <- "HWMLM"

uefacl_AWMLM <- as.data.frame(uefacl_AWMLM)
colnames(uefacl_AWMLM) <- "AWMLM"

uefacl_HY <- as.data.frame(uefacl_HY)
colnames(uefacl_HY) <- "AVGHY"

uefacl_AY <- as.data.frame(uefacl_AY)
colnames(uefacl_AY) <- "AVGAY"

uefacl_HCO <- as.data.frame(uefacl_HCO)
colnames(uefacl_HCO) <- "AVGHCO"

uefacl_ACO <- as.data.frame(uefacl_ACO)
colnames(uefacl_ACO) <- "AVGACO"

uefacl_HXSC <- as.data.frame(uefacl_HXSC)
colnames(uefacl_HXSC) <- "HXSC"

uefacl_AXSC <- as.data.frame(uefacl_AXSC)
colnames(uefacl_AXSC) <- "AXSC"

uefacl_HYCPF <- as.data.frame(uefacl_HYCPF)
colnames(uefacl_HYCPF) <- "HYCPF"

uefacl_AYCPF <- as.data.frame(uefacl_AYCPF)
colnames(uefacl_AYCPF) <- "AYCPF"

uefacl_picks <- cbind(UEFACL_fixtures$Div,UEFACL_fixtures$HomeTeam_uefacl,UEFACL_fixtures$AwayTeam_uefacl,uefacl_prediction,uefacl_HWM,uefacl_AWM,uefacl_HWMLM,uefacl_AWMLM,uefacl_HY,uefacl_AY,uefacl_HCO,uefacl_ACO,uefacl_HXSC,uefacl_AXSC,uefacl_HYCPF,uefacl_AYCPF)

colnames(uefacl_picks)[1] <- "picks_Div"
colnames(uefacl_picks)[2] <- "picks_HomeTeam"
colnames(uefacl_picks)[3] <- "picks_AwayTeam"
uefacl_picks$matchid <- paste(uefacl_picks$picks_HomeTeam,uefacl_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of UEFACL
uefacl_picks
#############################################################################################################################################################################
#clone fixtures
UEFACL_fixtures_clone <- UEFACL_fixtures
colnames(UEFACL_fixtures_clone)[61] <- "Hwin"
colnames(UEFACL_fixtures_clone)[62] <- "Draw"
colnames(UEFACL_fixtures_clone)[63] <- "Awin"

UEFACL_fixtures_clone$Hwinodds <-   UEFACL_fixtures$uefacl_1_0 + UEFACL_fixtures$uefacl_2_0 + UEFACL_fixtures$uefacl_2_1 + UEFACL_fixtures$uefacl_3_0 + UEFACL_fixtures$uefacl_3_1 +
  UEFACL_fixtures$uefacl_3_2 + UEFACL_fixtures$uefacl_4_0 + UEFACL_fixtures$uefacl_4_1 + UEFACL_fixtures$uefacl_4_2 + UEFACL_fixtures$uefacl_4_3 +
  UEFACL_fixtures$uefacl_5_0 + UEFACL_fixtures$uefacl_5_1 + UEFACL_fixtures$uefacl_5_2 + UEFACL_fixtures$uefacl_5_3 + UEFACL_fixtures$uefacl_5_4 +
  UEFACL_fixtures$uefacl_6_0 + UEFACL_fixtures$uefacl_6_1 + UEFACL_fixtures$uefacl_6_2 + UEFACL_fixtures$uefacl_6_3 + UEFACL_fixtures$uefacl_6_4 +
  UEFACL_fixtures$uefacl_6_5
UEFACL_fixtures_clone$Hwinodds <- round(1/UEFACL_fixtures_clone$Hwinodds, digits = 3)

UEFACL_fixtures_clone$Drawodds <-  UEFACL_fixtures$uefacl_0_0 + UEFACL_fixtures$uefacl_1_1 + UEFACL_fixtures$uefacl_2_2 + UEFACL_fixtures$uefacl_3_3 + UEFACL_fixtures$uefacl_4_4 +
  UEFACL_fixtures$uefacl_5_5 + UEFACL_fixtures$uefacl_6_6

UEFACL_fixtures_clone$Drawodds <- round(1/UEFACL_fixtures_clone$Drawodds, digits = 3)

UEFACL_fixtures_clone$Awinodds <-   UEFACL_fixtures$uefacl_0_1 + UEFACL_fixtures$uefacl_0_2 + UEFACL_fixtures$uefacl_1_2 + UEFACL_fixtures$uefacl_0_3 + UEFACL_fixtures$uefacl_1_3 +
  UEFACL_fixtures$uefacl_2_3 + UEFACL_fixtures$uefacl_0_4 + UEFACL_fixtures$uefacl_1_4 + UEFACL_fixtures$uefacl_2_4 + UEFACL_fixtures$uefacl_3_4 +
  UEFACL_fixtures$uefacl_0_5 + UEFACL_fixtures$uefacl_1_5 + UEFACL_fixtures$uefacl_2_5 + UEFACL_fixtures$uefacl_3_5 + UEFACL_fixtures$uefacl_4_5 +
  UEFACL_fixtures$uefacl_0_6 + UEFACL_fixtures$uefacl_1_6 + UEFACL_fixtures$uefacl_2_6 + UEFACL_fixtures$uefacl_3_6 + UEFACL_fixtures$uefacl_4_6 +
  UEFACL_fixtures$uefacl_5_6

UEFACL_fixtures_clone$Awinodds <- round(1/UEFACL_fixtures_clone$Awinodds, digits = 3)

colnames(UEFACL_fixtures_clone)[15] <- "CS_1-1"
colnames(UEFACL_fixtures_clone)[13] <- "CS_1-0"
colnames(UEFACL_fixtures_clone)[14] <- "CS_0-1"
colnames(UEFACL_fixtures_clone)[16] <- "CS_2-0"
colnames(UEFACL_fixtures_clone)[17] <- "CS_0-2"
colnames(UEFACL_fixtures_clone)[19] <- "CS_2-1"
colnames(UEFACL_fixtures_clone)[20] <- "CS_1-2"

UEFACL_fixtures_clone$`CS_1-1` <- round(1/UEFACL_fixtures_clone$`CS_1-1`, digits = 3)
UEFACL_fixtures_clone$`CS_1-0` <- round(1/UEFACL_fixtures_clone$`CS_1-0`, digits = 3)
UEFACL_fixtures_clone$`CS_0-1` <- round(1/UEFACL_fixtures_clone$`CS_0-1`, digits = 3)
UEFACL_fixtures_clone$`CS_2-0` <- round(1/UEFACL_fixtures_clone$`CS_2-0`, digits = 3)
UEFACL_fixtures_clone$`CS_0-2` <- round(1/UEFACL_fixtures_clone$`CS_0-2`, digits = 3)
UEFACL_fixtures_clone$`CS_2-1` <- round(1/UEFACL_fixtures_clone$`CS_2-1`, digits = 3)
UEFACL_fixtures_clone$`CS_1-2` <- round(1/UEFACL_fixtures_clone$`CS_1-2`, digits = 3)

colnames(UEFACL_fixtures_clone)[1] <- "league"
colnames(UEFACL_fixtures_clone)[2] <- "Hometeam"
colnames(UEFACL_fixtures_clone)[3] <- "Awayteam"
colnames(UEFACL_fixtures_clone)[92] <- "predscore"
colnames(UEFACL_fixtures_clone)[64] <- "ov25"
colnames(UEFACL_fixtures_clone)[66] <- "ov25odds"
colnames(UEFACL_fixtures_clone)[65] <- "un25"
colnames(UEFACL_fixtures_clone)[67] <- "un25odds"
colnames(UEFACL_fixtures_clone)[68] <- "BTTSY"
colnames(UEFACL_fixtures_clone)[69] <- "BTTSN"
colnames(UEFACL_fixtures_clone)[70] <- "BTTSYodds"
colnames(UEFACL_fixtures_clone)[71] <- "BTTSNodds"

UEFACL_fixtures_clone <- UEFACL_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
UEFACL_fixtures_clone$matchid <- paste(UEFACL_fixtures_clone$Hometeam,UEFACL_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
UEFACL_fixtures_clone_final <- UEFACL_fixtures_clone[,-c(8,9,10,27)]
UEFACL_fixtures_clone_final[,'sep'] <- ''

uefacl_dmprediction <-  uefacl_picks[,c(4,5,6,7,8)]
uefacl_dmprediction[,'sep2'] <- ''

uefacl_avgyellow <- uefacl_picks[,c(9,10)]
uefacl_avgyellow[,'sep3'] <- ''

uefacl_avgcorners <- uefacl_picks[,c(11,12)]
uefacl_avgcorners[,'sep4'] <- ''

uefacl_goals <- UEFACL_fixtures[,c(10,11)]
uefacl_goals$uefacl_xGH <- round(uefacl_goals$uefacl_xGH, digits = 2)
uefacl_goals$uefacl_xGA <- round(uefacl_goals$uefacl_xGA, digits = 2)
uefacl_goals$uefacl_TxG <- uefacl_goals$uefacl_xGH + uefacl_goals$uefacl_xGA
uefacl_goals[,'sep5'] <- ''

uefacl_shots <- UEFACL_fixtures_sot[,c(10,11)]
uefacl_shots$uefacl_xHST <- round(uefacl_shots$uefacl_xHST, digits = 2)
uefacl_shots$uefacl_xAST <- round(uefacl_shots$uefacl_xAST, digits = 2)
uefacl_shots$TxSOT <- uefacl_shots$uefacl_xHST + uefacl_shots$uefacl_xAST
uefacl_shots[,'sep6'] <- ''

uefacl_fouls <- UEFACL_fixtures_fo[,c(10,11)]
uefacl_fouls$uefacl_xHF <- round(uefacl_fouls$uefacl_xHF, digits = 2)
uefacl_fouls$uefacl_xAF <- round(uefacl_fouls$uefacl_xAF, digits = 2)
uefacl_fouls$uefacl_TxF <- uefacl_fouls$uefacl_xHF + uefacl_fouls$uefacl_xAF

uefacl_ycpf <- uefacl_picks[,c(15,16)]
uefacl_fouls <- cbind(uefacl_fouls,uefacl_ycpf)
uefacl_fouls$HYCPF <- as.numeric(uefacl_fouls$HYCPF)
uefacl_fouls$AYCPF <- as.numeric(uefacl_fouls$AYCPF)
uefacl_fouls$x_hyc <- (uefacl_fouls$uefacl_xHF) * (uefacl_fouls$HYCPF)
uefacl_fouls$x_ayc <- (uefacl_fouls$uefacl_xAF) * (uefacl_fouls$AYCPF)
uefacl_fouls$x_TYC <- round((uefacl_fouls$x_hyc + uefacl_fouls$x_ayc),digits = 2)
uefacl_fouls[,'sep7'] <- ''

uefacl_bookings <- UEFACL_fixtures_yc[,c(10,11)]
uefacl_bookings$uefacl_xHYC <- round(uefacl_bookings$uefacl_xHYC, digits = 2)
uefacl_bookings$uefacl_xAYC <- round(uefacl_bookings$uefacl_xAYC, digits = 2)
uefacl_bookings$uefacl_TYcards <- uefacl_bookings$uefacl_xHYC + uefacl_bookings$uefacl_xAYC
uefacl_bookings[,'sep8'] <- ''

uefacl_corners <- UEFACL_fixtures_co[,c(10,11)]
uefacl_corners$uefacl_xHCOC <- round(uefacl_corners$uefacl_xHCOC, digits = 2)
uefacl_corners$uefacl_xACOC <- round(uefacl_corners$uefacl_xACOC, digits = 2)
uefacl_corners$uefacl_TCOs <- uefacl_corners$uefacl_xHCOC + uefacl_corners$uefacl_xACOC
uefacl_corners[,'sep9'] <- ''

uefacl_shotsconversion <- uefacl_picks[,c(13,14)]
uefacl_shotsconversion <- cbind(uefacl_shotsconversion,uefacl_shots)
uefacl_shotsconversion$HXSC <- as.numeric(uefacl_shotsconversion$HXSC)
uefacl_shotsconversion$AXSC <- as.numeric(uefacl_shotsconversion$AXSC)
uefacl_shotsconversion$uefacl_hXgoals <- round((uefacl_shotsconversion$HXSC * uefacl_shotsconversion$uefacl_xHST), digits = 2)
uefacl_shotsconversion$uefacl_aXgoals <- round((uefacl_shotsconversion$AXSC * uefacl_shotsconversion$uefacl_xAST), digits = 2)
uefacl_shotsconversion$Xgoals <- uefacl_shotsconversion$uefacl_hXgoals + uefacl_shotsconversion$uefacl_aXgoals

UEFACL_all <- cbind(UEFACL_fixtures_clone_final,uefacl_dmprediction,uefacl_avgyellow,uefacl_avgcorners,uefacl_goals,uefacl_shots,uefacl_fouls,uefacl_bookings,uefacl_corners,uefacl_shotsconversion)
unlink('Divisions/UEFACL.xlsx')
write.xlsx(UEFACL_all,'Divisions/UEFACL.xlsx', sheetName = "UEFACL_all", append = TRUE)
write.xlsx(points_uefacl,'Divisions/UEFACL.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(uefacl_cornertotalsv2,'Divisions/UEFACL.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(uefacl_goaltotalsv2,'Divisions/UEFACL.xlsx', sheetName = "Goaltotals", append = TRUE)


