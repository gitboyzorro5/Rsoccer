library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('NL/BRA.xlsx')

######################BRA START#######################################
#####################################################################
BRA <- read.csv('../FDAS/BRA.csv')
BRA <- within(BRA,rm(Res))
BRA$Date <- dmy(BRA$Date)
BRA <- BRA[order(as.Date(BRA$Date, format = "%d/%m%Y"), decreasing = FALSE),]
BRA$CS <- paste(BRA$HG,BRA$AG, sep = "-")
#BRA_qualificaton <- subset(BRA,tournament == "UEFA Euro qualification")
BRA <- subset(BRA,Season == "2021")
BRA <- BRA[!BRA$Home =="Coritiba",]
BRA <- BRA[!BRA$Away == "Coritiba",]
BRA <- BRA[!BRA$Home =="Vasco",]
BRA <- BRA[!BRA$Away == "Vasco",]
BRA <- BRA[!BRA$Home =="Goias",]
BRA <- BRA[!BRA$Away == "Goias",]
BRA <- BRA[!BRA$Home =="Cuiaba Esporte",]
BRA <- BRA[!BRA$Away == "Cuiaba Esporte",]
#BRA <- BRA[BRA$Date > '2008-01-01',])
BRA$TG <- BRA$HG + BRA$AG
BRA$OV25 <- ifelse(BRA$TG >= 3,"Y","N")
BRA$FTR <- with(BRA,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)

###################################################
####GoalTotalsv2##################################
####GoalTotalsv2##################################
bra_totalgoalsv2 <- tapply(BRA$TG, BRA[c("Home", "Away")],mean)
bra_totalgoalsv2
bra_hgtotals <- rowSums(bra_totalgoalsv2,na.rm = T)
bra_agtotals <- colSums(bra_totalgoalsv2,na.rm = T)

bra_totalgoals <- bra_hgtotals + bra_agtotals
bra_totalgoalsv2 <- cbind(bra_totalgoalsv2,bra_totalgoals)
bra_teams <- sort(unique(BRA$Home))
bra_home_games <- c()
bra_away_games <-c()
for (i_bra in 1:length(bra_teams))
{

  bra_home_games[i_bra] <- nrow(BRA[BRA$Home == bra_teams[i_bra],])
  bra_away_games[i_bra]  <- nrow(BRA[BRA$Away == bra_teams[i_bra],])

}
bra_games_played <- bra_home_games + bra_away_games
bra_goaltotalsv2 <- cbind(bra_totalgoalsv2,bra_games_played)
bra_avg_totalgoals <- round((bra_totalgoals/ bra_games_played), digits = 4)
bra_goaltotalsv2[is.na(bra_goaltotalsv2)] <- ""
bra_goaltotalsv2 <- cbind(bra_goaltotalsv2,bra_avg_totalgoals)
write.xlsx(bra_goaltotalsv2,'NL/BRA.xlsx',sheetName = "totalgoalsv2")
#####################################################################
#bra goal scored rounds
#####################################################################
bra_krounds <- tail(unique(BRA_rounds$bra_matchday),1)
nrow(BRA)
bra_goalscoredmatrix <- data.frame(matrix(nrow = length(bra_teams),ncol = bra_krounds))
bra_goalscoredround <- c()
for(i_bra_krounds in 1:bra_krounds)
{
  bra_homegoalscored <- BRA_rounds$HG[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awaygoalscored <- BRA_rounds$AG[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_hometeamstemp_gs <- BRA_rounds$Home[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awayteamstemp_gs <- BRA_rounds$Away[BRA_rounds$bra_matchday== i_bra_krounds]

  bra_goalscombined <- c(bra_homegoalscored,bra_awaygoalscored)
  bra_teamscombined <- c(bra_hometeamstemp_gs,bra_awayteamstemp_gs)

  bra_goalscoredround <- data.frame(bra_teamscombined,bra_goalscombined)

  bra_goalscoredround <- bra_goalscoredround[order(bra_goalscoredround$bra_teamscombined),]
  bra_goalscoredround$bra_teamscombined <- NULL
  bra_goalscoredmatrix[,i_bra_krounds] <- bra_goalscoredround

}
bra_goalscoredmatrix
bra_goalscoredmatrix <- cbind(bra_teams,bra_goalscoredmatrix)
####GSmatrix################################
#create home and away matrices
bra_goalscored_h <- tapply(BRA$HG, BRA[c("Home", "Date")],mean)
bra_goalscored_a <- tapply(BRA$AG, BRA[c("Away", "Date")],mean)
bra_goalscored_h[is.na(bra_goalscored_h)] <- ""
bra_goalscored_a[is.na(bra_goalscored_a)] <- ""

for(bra_rowhgs in 1:nrow(bra_goalscored_h)) {
  for(bra_colhgs in 1:ncol(bra_goalscored_h)) {

    # print(my_matrix[row, col])
    for(bra_rowags in 1:nrow(bra_goalscored_a)) {
      for(bra_colags in 1:ncol(bra_goalscored_a)) {
        ifelse(!bra_goalscored_a[bra_rowags,bra_colags]=="",bra_goalscored_h[bra_rowags,bra_colags] <- bra_goalscored_a[bra_rowags,bra_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

write.xlsx(bra_goalscoredmatrix,'NL/BRA.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#bra goal conceded rounds
#bra
bra_krounds <- tail(unique(BRA_rounds$bra_matchday),1)
bra_goalconcededmatrix <- data.frame(matrix(nrow = length(bra_teams),ncol = bra_krounds))
bra_goalconcededround <- c()
for(i_bra_krounds in 1:bra_krounds)
{
  bra_homegoalconceded <- BRA_rounds$AG[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awaygoalconceded <- BRA_rounds$HG[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_hometeamstemp_gc <- BRA_rounds$Home[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awayteamstemp_gc <- BRA_rounds$Away[BRA_rounds$bra_matchday== i_bra_krounds]

  bra_goalsconcededcombined <- c(bra_homegoalconceded,bra_awaygoalconceded)
  bra_teamscombined_gc <- c(bra_hometeamstemp_gc,bra_awayteamstemp_gc)

  bra_goalconcededround <- data.frame(bra_teamscombined_gc,bra_goalsconcededcombined)

  bra_goalconcededround <- bra_goalconcededround[order(bra_goalconcededround$bra_teamscombined_gc),]
  bra_goalconcededround$bra_teamscombined_gc <- NULL
  bra_goalconcededmatrix[,i_bra_krounds] <- bra_goalconcededround

}

bra_goalconcededmatrix <- cbind(bra_teams,bra_goalconcededmatrix)

####GCmatrix#############################################################################
#create home and away matrices
bra_goalconceded_h <- tapply(BRA$AG, BRA[c("Home", "Date")],mean)
bra_goalconceded_a <- tapply(BRA$HG, BRA[c("Away", "Date")],mean)
bra_goalconceded_h[is.na(bra_goalconceded_h)] <- ""
bra_goalconceded_a[is.na(bra_goalconceded_a)] <- ""

for(bra_rowhgc in 1:nrow(bra_goalconceded_h)) {
  for(bra_colhgc in 1:ncol(bra_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(bra_rowagc in 1:nrow(bra_goalconceded_a)) {
      for(bra_colagc in 1:ncol(bra_goalconceded_a)) {
        ifelse(!bra_goalconceded_a[bra_rowagc,bra_colagc]=="",bra_goalconceded_h[bra_rowagc,bra_colagc] <- bra_goalconceded_a[bra_rowagc,bra_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(bra_goalconcededmatrix,'NL/BRA.xlsx',sheetName = "gcmatrix", append = TRUE)
########################################################################################
#bra team form
bra_krounds <- tail(unique(BRA_rounds$bra_matchday),1)
bra_formmatrix <- data.frame(matrix(nrow = length(bra_teams),ncol = bra_krounds))
bra_formround <- c()
for(i_bra_krounds in 1:bra_krounds)
{
  bra_homeform <- BRA_rounds$FTR[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_homeform <- sub("H","W",bra_homeform)
  bra_homeform <- sub("A","L",bra_homeform)

  bra_awayform <- BRA_rounds$FTR[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awayform <- sub("A","W",bra_awayform)
  bra_awayform <- sub("H","L",bra_awayform)

  bra_hometeamstemp_form <- BRA_rounds$Home[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awayteamstemp_form <- BRA_rounds$Away[BRA_rounds$bra_matchday== i_bra_krounds]

  bra_formcombined <- c(bra_homeform,bra_awayform)
  bra_teamscombined_form <- c(bra_hometeamstemp_form,bra_awayteamstemp_form)

  bra_formround <- data.frame(bra_teamscombined_form,bra_formcombined)

  bra_formround <- bra_formround[order(bra_formround$bra_teamscombined_form),]
  bra_formround$bra_teamscombined_form <- NULL
  bra_formmatrix[,i_bra_krounds] <- bra_formround

}

bra_formmatrix <- cbind(bra_teams,bra_formmatrix)
########################################################################################
########################################################################################
#########################################################################################
####Teamform#############################################################################

bra_form_h <- tapply(BRA$FTR, BRA[c("Home", "Date")],median)
bra_form_a <- tapply(BRA$FTR, BRA[c("Away", "Date")],median)
bra_form_h[is.na(bra_form_h)] <- ""
bra_form_a[is.na(bra_form_a)] <- ""
bra_form_h <- sub("A","L",bra_form_h)
bra_form_h <- sub("H","W",bra_form_h)
bra_form_a <- sub("A","W",bra_form_a)
bra_form_a <- sub("H","L",bra_form_a)
for(bra_rowh_f in 1:nrow(bra_form_h)) {
  for(bra_colh_f in 1:ncol(bra_form_h)) {

    # print(my_matrix[row, col])
    for(bra_rowa_f in 1:nrow(bra_form_a)) {
      for(bra_cola_f in 1:ncol(bra_form_a)) {
        ifelse(!bra_form_a[bra_rowa_f,bra_cola_f]=="",bra_form_h[bra_rowa_f,bra_cola_f] <- bra_form_a[bra_rowa_f,bra_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(bra_formmatrix,'NL/BRA.xlsx',sheetName = "form", append = TRUE)
##################################################################################
##################################################################################
#bra total goals rounds
bra_krounds <- tail(unique(BRA_rounds$bra_matchday),1)
bra_goaltotalmatrix <- data.frame(matrix(nrow = length(bra_teams),ncol = bra_krounds))
bra_goaltotalround <- c()
for(i_bra_krounds in 1:bra_krounds)
{
  bra_homegoaltotal <- BRA_rounds$TG[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awaygoaltotal <- BRA_rounds$TG[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_hometeamstemp_tg <- BRA_rounds$Home[BRA_rounds$bra_matchday == i_bra_krounds]

  bra_awayteamstemp_tg <- BRA_rounds$Away[BRA_rounds$bra_matchday== i_bra_krounds]

  bra_goalscombined_tg <- c(bra_homegoaltotal,bra_awaygoaltotal)
  bra_teamscombined_tg <- c(bra_hometeamstemp_tg,bra_awayteamstemp_tg)

  bra_goaltotalround <- data.frame(bra_teamscombined_tg,bra_goalscombined_tg)

  bra_goaltotalround <- bra_goaltotalround[order(bra_goaltotalround$bra_teamscombined_tg),]
  bra_goaltotalround$bra_teamscombined_tg <- NULL
  bra_goaltotalmatrix[,i_bra_krounds] <- bra_goaltotalround

}

bra_goaltotalmatrix <- cbind(bra_teams,bra_goaltotalmatrix)
##############################################################################################
#d1
#######TGMatrix##################################################################
bra_totalgoals_h <- tapply(BRA$TG, BRA[c("Home", "Date")],mean)
bra_totalgoals_a <- tapply(BRA$TG, BRA[c("Away", "Date")],mean)
bra_totalgoals_h[is.na(bra_totalgoals_h)] <- ""
bra_totalgoals_a[is.na(bra_totalgoals_a)] <- ""
for(bra_rowh in 1:nrow(bra_totalgoals_h)) {
  for(bra_colh in 1:ncol(bra_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(bra_rowa in 1:nrow(bra_totalgoals_a)) {
      for(bra_cola in 1:ncol(bra_totalgoals_a)) {
        ifelse(!bra_totalgoals_a[bra_rowa,bra_cola]=="",bra_totalgoals_h[bra_rowa,bra_cola] <- bra_totalgoals_a[bra_rowa,bra_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(bra_goaltotalmatrix,'NL/BRA.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
bra_form_team_against_h <- tapply(BRA$Away, BRA[c("Home", "Date")],median)
bra_form_team_against_a <- tapply(BRA$Home, BRA[c("Away", "Date")],median)
bra_form_team_against_h[is.na(bra_form_team_against_h)] <- ""
bra_form_team_against_a[is.na(bra_form_team_against_a)] <- ""
for(bra_rowh_f_against in 1:nrow(bra_form_team_against_h)) {
  for(bra_colh_f_against in 1:ncol(bra_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(bra_rowa_f_against in 1:nrow(bra_form_team_against_a)) {
      for(bra_cola_f_against in 1:ncol(bra_form_team_against_a)) {
        ifelse(!bra_form_team_against_a[bra_rowa_f_against,bra_cola_f_against]=="",bra_form_team_against_h[bra_rowa_f_against,bra_cola_f_against] <- bra_form_team_against_a[bra_rowa_f_against,bra_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
#win margin
bra_winmargin_h <- tapply(BRA$HG - BRA$AG, BRA[c("Home", "Date")],mean)
bra_winmargin_a <- tapply(BRA$AG - BRA$HG, BRA[c("Away", "Date")],mean)
bra_winmargin_h[is.na(bra_winmargin_h)] <- ""
#
for(bra_rowhwm in 1:nrow(bra_winmargin_h)) {
  for(bra_colhwm in 1:ncol(bra_winmargin_h)) {

    # print(my_matrix[row, col])
    for(bra_rowawm in 1:nrow(bra_winmargin_a)) {
      for(bra_colawm in 1:ncol(bra_winmargin_a)) {
        ifelse(!bra_winmargin_a[bra_rowawm,bra_colawm]=="",bra_winmargin_h[bra_rowawm,bra_colawm] <- bra_winmargin_a[bra_rowawm,bra_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
####################################################################################################################
##########Goals over under############
#BRA
bra_un05_home <- c()
bra_un05_away <- c()
bra_ov05_home <- c()
bra_ov05_away <- c()

bra_un15_home <- c()
bra_un15_away <- c()
bra_ov15_home <- c()
bra_ov15_away <- c()

bra_un25_home <- c()
bra_un25_away <- c()
bra_ov25_home <- c()
bra_ov25_away <- c()

bra_un35_home <- c()
bra_un35_away <- c()
bra_ov35_home <- c()
bra_ov35_away <- c()

bra_un45_home <- c()
bra_un45_away <- c()
bra_ov45_home <- c()
bra_ov45_away <- c()

bra_un55_home <- c()
bra_un55_away <- c()
bra_ov55_home <- c()
bra_ov55_away <- c()

for (i_bra_tg in 1:length(bra_teams))
{

  bra_un05_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG == 0,])
  bra_un05_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG == 0,])

  bra_ov05_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG > 0,])
  bra_ov05_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG > 0,])

  bra_un15_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG <= 1,])
  bra_un15_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG <= 1,])

  bra_ov15_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG >= 2,])
  bra_ov15_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG >= 2,])

  bra_un25_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG <= 2,])
  bra_un25_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG <= 2,])

  bra_ov25_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG >=3,])
  bra_ov25_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG >=3,])

  bra_un35_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG <= 3,])
  bra_un35_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG <= 3,])

  bra_ov35_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG >= 4,])
  bra_ov35_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG >= 4,])

  bra_un45_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG <= 4,])
  bra_un45_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG <= 4,])

  bra_ov45_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG >= 5,])
  bra_ov45_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG >= 5,])

  bra_un55_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG <= 5,])
  bra_un55_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG <= 5,])

  bra_ov55_home[i_bra_tg] <- nrow(BRA[BRA$Home == bra_teams[i_bra_tg] & BRA$TG >= 6,])
  bra_ov55_away[i_bra_tg] <- nrow(BRA[BRA$Away == bra_teams[i_bra_tg] & BRA$TG >= 6,])


}

bra_un05 <- bra_un05_home + bra_un05_away
bra_ov05 <- bra_ov05_home + bra_ov05_away

bra_un15 <- bra_un15_home + bra_un15_away
bra_ov15 <- bra_ov15_home + bra_ov15_away

bra_un25 <- bra_un25_home + bra_un25_away
bra_ov25 <- bra_ov25_home + bra_ov25_away

bra_un35 <- bra_un35_home + bra_un35_away
bra_ov35 <- bra_ov35_home + bra_ov35_away

bra_un45 <- bra_un45_home + bra_un45_away
bra_ov45 <- bra_ov45_home + bra_ov45_away

bra_un55 <- bra_un55_home + bra_un55_away
bra_ov55 <- bra_ov55_home + bra_ov55_away

bra_ovundata <- cbind(bra_teams,bra_un05,bra_ov05,bra_un15,bra_ov15,bra_un25,bra_ov25,bra_un35,bra_ov35,bra_un45,bra_ov45,bra_un55,bra_ov55)
write.xlsx(bra_ovundata,'NL/BRA.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
bra_csform_h <- tapply(BRA$CS, BRA[c("Home", "Date")],median)
bra_csform_a <- tapply(BRA$CS, BRA[c("Away", "Date")],median)

bra_csform_h[is.na(bra_csform_h)] <- ""
bra_csform_a[is.na(bra_csform_a)] <- ""

for(bra_rowh_f_cs in 1:nrow(bra_csform_h)) {
  for(bra_colh_f_cs in 1:ncol(bra_csform_h)) {

    # print(my_matrix[row, col])
    for(bra_rowa_f_cs in 1:nrow(bra_csform_a)) {
      for(bra_cola_f_cs in 1:ncol(bra_csform_a)) {
        ifelse(!bra_csform_a[bra_rowa_f_cs,bra_cola_f_cs]=="",bra_csform_h[bra_rowa_f_cs,bra_cola_f_cs] <- bra_csform_a[bra_rowa_f_cs,bra_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
bra_home_gs <- aggregate(BRA$HG, by = list(BRA$Home), FUN = sum)
bra_home_gs_avg <- aggregate(BRA$HG, by = list(BRA$Home),mean)
bra_home_scoring <- merge(bra_home_gs,bra_home_gs_avg, by='Group.1',all = T)
names(bra_home_scoring)[names(bra_home_scoring) == "x.x"] <- "TFthg"
names(bra_home_scoring)[names(bra_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
bra_away_gs <- aggregate(BRA$AG, by = list(BRA$Away), FUN = sum)
bra_away_gs_avg <- aggregate(BRA$AG, by = list(BRA$Away),mean)
bra_away_scoring <- merge(bra_away_gs,bra_away_gs_avg, by='Group.1',all = T)
names(bra_away_scoring)[names(bra_away_scoring) == "x.x"] <- "TFtag"
names(bra_away_scoring)[names(bra_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
bra_scoring <- merge(bra_home_scoring,bra_away_scoring,by='Group.1',all = T)
bra_scoring$TGS <- bra_scoring$TFthg + bra_scoring$TFtag

#home goals conceded
bra_home_gc <- aggregate(BRA$AG, by = list(BRA$Home), FUN = sum)
bra_home_gc_avg <- aggregate(BRA$AG, by = list(BRA$Home),mean)
bra_home_conceding <- merge(bra_home_gc,bra_home_gc_avg, by='Group.1',all = T)
names(bra_home_conceding)[names(bra_home_conceding) == "x.x"] <- "TFthc"
names(bra_home_conceding)[names(bra_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
bra_away_gc <- aggregate(BRA$HG, by = list(BRA$Away), FUN = sum)
bra_away_gc_avg <- aggregate(BRA$HG, by = list(BRA$Away),mean)
bra_away_conceding <- merge(bra_away_gc,bra_away_gc_avg, by='Group.1',all = T)
names(bra_away_conceding)[names(bra_away_conceding) == "x.x"] <- "TFtac"
names(bra_away_conceding)[names(bra_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
bra_conceding <- merge(bra_home_conceding,bra_away_conceding,by='Group.1',all = T)
bra_conceding$TGC <- bra_conceding$TFthc + bra_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
bra_home_wins <- c()
bra_away_wins <- c()
bra_home_draws <- c()
bra_away_draws <- c()
bra_home_loss <- c()
bra_away_loss <- c()



for (i_bra_wins in 1:length(bra_teams))
{

  bra_home_wins[i_bra_wins] <- nrow(BRA[BRA$Home == bra_teams[i_bra_wins] & BRA$FTR == "H",])
  bra_away_wins[i_bra_wins] <- nrow(BRA[BRA$Away == bra_teams[i_bra_wins] & BRA$FTR == "A",])
  bra_home_draws[i_bra_wins] <- nrow(BRA[BRA$Home == bra_teams[i_bra_wins] & BRA$FTR == "D",])
  bra_away_draws[i_bra_wins] <- nrow(BRA[BRA$Away == bra_teams[i_bra_wins] & BRA$FTR == "D",])
  bra_home_loss[i_bra_wins] <- nrow(BRA[BRA$Home == bra_teams[i_bra_wins] & BRA$FTR == "A",])
  bra_away_loss[i_bra_wins] <- nrow(BRA[BRA$Away == bra_teams[i_bra_wins] & BRA$FTR == "H",])

}

bra_total_wins <- bra_home_wins + bra_away_wins
bra_total_draws <- bra_home_draws + bra_away_draws
bra_total_loss <- bra_home_loss + bra_away_loss

bra_league_table <- cbind(bra_teams,bra_games_played,bra_total_wins,bra_total_draws,bra_total_loss)
bra_GS <- bra_scoring$TGS
bra_GC <-bra_conceding$TGC
bra_GD <- bra_scoring$TGS - bra_conceding$TGC
bra_PTS <- (bra_total_wins*3) + (bra_total_draws*1)
bra_league_table <- cbind(bra_league_table,bra_GS,bra_GC,bra_GD,bra_PTS)
bra_league_table <- as.data.frame(bra_league_table)
#rename the columns
names(bra_league_table)[names(bra_league_table) == "bra_teams"] <- "Team"
names(bra_league_table)[names(bra_league_table) == "bra_games_played"] <- "P"
names(bra_league_table)[names(bra_league_table) == "bra_total_wins"] <- "W"
names(bra_league_table)[names(bra_league_table) == "bra_total_draws"] <- "D"
names(bra_league_table)[names(bra_league_table) == "bra_total_loss"] <- "L"
names(bra_league_table)[names(bra_league_table) == "bra_GS"] <- "F"
names(bra_league_table)[names(bra_league_table) == "bra_GC"] <- "A"
points_bra <- bra_league_table[order(as.numeric(bra_league_table$bra_PTS), decreasing = TRUE),]
points_bra$bra_rank <- 1:length(bra_teams)
row.names(points_bra) <- points_bra$bra_rank
#create final_bra_hf_against with team ranks in brackets
for(bra_rowhrank in 1:nrow(bra_form_team_against_h)) {
  for(bra_colhrank in 1:ncol(bra_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!bra_form_team_against_h[bra_rowhrank,bra_colhrank]=="",bra_form_team_against_h[bra_rowhrank,bra_colhrank] <- paste(bra_form_team_against_h[bra_rowhrank,bra_colhrank],"(",points_bra$bra_rank[points_bra$Team ==bra_form_team_against_h[bra_rowhrank,bra_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_bra,'NL/BRA.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six bra###################################################
#BRA
#form
#create final_bra_hf object
#bra_last_n_games <- 6
final_bra_hf <- c()
for(index_bra_hf in 1:length(bra_teams))
{
  index_bra_hf <- row.names(bra_form_h) == bra_teams[index_bra_hf]
  form_bra_hf <- bra_form_h[index_bra_hf]
  deleted_form_bra_hf <- form_bra_hf[!form_bra_hf[] == ""]
  l6_form_bra_hf <- tail(deleted_form_bra_hf,bra_last_n_games)
  l6_form_bra_hf <- paste(l6_form_bra_hf,collapse = " ")
  final_bra_hf[index_bra_hf] <- rbind(paste(bra_teams[index_bra_hf],l6_form_bra_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bra_teams[index],l6_form)

}

#change column names
final_bra_hf <- as.data.frame(final_bra_hf)
colnames(final_bra_hf) <- "Form"
#goals scored
#create final_bra_gs object
final_bra_gs <- c()
suml6_bra_gs <- c()
for(index_bra_gs in 1:length(bra_teams))
{
  index_bra_gs <- row.names(bra_goalscored_h) == bra_teams[index_bra_gs]
  form_bra_gs <- bra_goalscored_h[index_bra_gs]
  deleted_form_bra_gs <- form_bra_gs[!form_bra_gs[] == ""]
  l6_form_bra_gs <- tail(deleted_form_bra_gs,bra_last_n_games)
  l6_form_bra_gs <- as.numeric(l6_form_bra_gs)
  suml6_bra_gs[index_bra_gs] <- sum(l6_form_bra_gs)
  suml6_bra_gs[index_bra_gs] <- paste("(",suml6_bra_gs[index_bra_gs],")",sep = "")
  l6_form_bra_gs <- paste(l6_form_bra_gs,collapse = " ")
  final_bra_gs[index_bra_gs] <- rbind(paste(bra_teams[index_bra_gs],l6_form_bra_gs,suml6_bra_gs[index_bra_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bra_teams[index],l6_form)

}
final_bra_gs
#change column names
final_bra_gs <- as.data.frame(final_bra_gs)
colnames(final_bra_gs) <- "Goals scored"
#goal conceded
#create final_bra_gc object
final_bra_gc <- c()
suml6_bra_gc <- c()
for(index_bra_gc in 1:length(bra_teams))
{
  index_bra_gc <- row.names(bra_goalconceded_h) == bra_teams[index_bra_gc]
  form_bra_gc <- bra_goalconceded_h[index_bra_gc]
  deleted_form_bra_gc <- form_bra_gc[!form_bra_gc[] == ""]
  l6_form_bra_gc <- tail(deleted_form_bra_gc,bra_last_n_games)
  l6_form_bra_gc <- as.numeric(l6_form_bra_gc)
  suml6_bra_gc[index_bra_gc] <- sum(l6_form_bra_gc)
  suml6_bra_gc[index_bra_gc] <- paste("(",suml6_bra_gc[index_bra_gc],")",sep = "")
  l6_form_bra_gc <- paste(l6_form_bra_gc,collapse = " ")
  final_bra_gc[index_bra_gc] <- rbind(paste(bra_teams[index_bra_gc],l6_form_bra_gc,suml6_bra_gc[index_bra_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bra_teams[index],l6_form)

}

#change column names
final_bra_gc <- as.data.frame(final_bra_gc)
colnames(final_bra_gc) <- "Goals conceded"
#total goals
#create final_bra_tg object
final_bra_tg <- c()
suml6_bra_tg <- c()
for(index_bra_tg in 1:length(bra_teams))
{
  index_bra_tg <- row.names(bra_totalgoals_h) == bra_teams[index_bra_tg]
  form_bra_tg <- bra_totalgoals_h[index_bra_tg]
  deleted_form_bra_tg <- form_bra_tg[!form_bra_tg[] == ""]
  l6_form_bra_tg <- tail(deleted_form_bra_tg,bra_last_n_games)
  l6_form_bra_tg <- as.numeric(l6_form_bra_tg)
  suml6_bra_tg[index_bra_tg] <- sum(l6_form_bra_tg)
  suml6_bra_tg[index_bra_tg] <- paste("(",suml6_bra_tg[index_bra_tg],")",sep = "")
  l6_form_bra_tg <- paste(l6_form_bra_tg,collapse = " ")
  final_bra_tg[index_bra_tg] <- rbind(paste(bra_teams[index_bra_tg],l6_form_bra_tg,suml6_bra_tg[index_bra_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bra_teams[index],l6_form)

}
#change column names
final_bra_tg <- as.data.frame(final_bra_tg)
colnames(final_bra_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_bra_hf object
final_bra_cs <- c()
for(index_bra_cs in 1:length(bra_teams))
{
  index_bra_cs <- row.names(bra_csform_h) == bra_teams[index_bra_cs]
  csform_bra_cs <- bra_csform_h[index_bra_cs]
  deleted_csform_bra_cs <- csform_bra_cs[!csform_bra_cs[] == ""]
  l6_csform_bra_cs <- tail(deleted_csform_bra_cs,bra_last_n_games)
  l6_csform_bra_cs <- paste(l6_csform_bra_cs,collapse = " ")
  final_bra_cs[index_bra_cs] <- rbind(paste(bra_teams[index_bra_cs],l6_csform_bra_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",bra_teams[index],l6_csform)

}

#change column names
final_bra_cs <- as.data.frame(final_bra_cs)
colnames(final_bra_cs) <- "CSForm"
#################################################
#Team against
#create final_bra_hf_against
final_bra_hf_against <- c()
for(index_bra_hf_against in 1:length(bra_teams))
{
  index_bra_hf_against <- row.names(bra_form_team_against_h) == bra_teams[index_bra_hf_against]
  form_bra_hf_against <- bra_form_team_against_h[index_bra_hf_against]
  deleted_form_bra_hf_against <- form_bra_hf_against[!form_bra_hf_against[] == ""]
  l6_form_bra_hf_against <- tail(deleted_form_bra_hf_against,bra_last_n_games)
  l6_form_bra_hf_against <- paste(l6_form_bra_hf_against,collapse = " ")
  final_bra_hf_against[index_bra_hf_against] <- rbind(paste(bra_teams[index_bra_hf_against],l6_form_bra_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bra_teams[index],l6_form)

}
#################################################
#Win Margin
#goals scored
#create final_bra_wm object
final_bra_wm <- c()
suml6_bra_wm <- c()
for(index_bra_wm in 1:length(bra_teams))
{
  index_bra_wm <- row.names(bra_winmargin_h) == bra_teams[index_bra_wm]
  form_bra_wm <- bra_winmargin_h[index_bra_wm]
  deleted_form_bra_wm <- form_bra_wm[!form_bra_wm[] == ""]
  l6_form_bra_wm <- tail(deleted_form_bra_wm,bra_last_n_games)
  l6_form_bra_wm <- as.numeric(l6_form_bra_wm)
  suml6_bra_wm[index_bra_wm] <- sum(l6_form_bra_wm)
  suml6_bra_wm[index_bra_wm] <- paste("(",suml6_bra_wm[index_bra_wm],")",sep = "")
  l6_form_bra_wm <- paste(l6_form_bra_wm,collapse = " ")
  final_bra_wm[index_bra_wm] <- rbind(paste(bra_teams[index_bra_wm],l6_form_bra_wm,suml6_bra_wm[index_bra_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",bra_teams[index],l6_form)

}
final_bra_wm
#change column names
final_bra_wm <- as.data.frame(final_bra_wm)
colnames(final_bra_wm) <- "Win Margin"
###########################################################################
final_bra_hf_against <- as.data.frame(final_bra_hf_against)
colnames(final_bra_hf_against) <- "Team against"
#combine the columns
final_bra_all <- cbind(final_bra_hf,final_bra_gs,final_bra_gc,final_bra_tg,final_bra_cs,final_bra_wm,final_bra_hf_against)
write.xlsx(final_bra_all,'NL/BRA.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
bra_GP <- nrow(BRA)
#Calculate total home goals for each division
bra_T_HG <- sum(bra_home_gs$x)
#calculate average home goal
bra_avg_HG <- round(bra_T_HG /bra_GP, digits = 4)
############################################################
#Calculate total away goals for each division
bra_T_AG <- sum(bra_away_gs$x)
#calculate average away goal
bra_avg_AG <- round(bra_T_AG /bra_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bra_home_as <- round(((bra_home_gs$x/bra_home_games))/bra_avg_HG, digits = 4)
#calculate away attack strength
bra_away_as <- round(((bra_away_gs$x/bra_away_games))/bra_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
bra_avg_HC <- round(bra_T_AG /bra_GP, digits = 4)
#avg away concede
bra_avg_AC <- round(bra_T_HG /bra_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
bra_home_ds <- round(((bra_home_gc$x/bra_home_games))/bra_avg_HC, digits = 4)
#away defense strength
bra_away_ds <- round(((bra_away_gc$x/bra_away_games))/bra_avg_AC, digits = 4)
#############################################################################
#home poisson data
#bra
bra_division <- c()
bra_division[1:length(bra_teams)] <- "BRA"
bra_home_poisson <- cbind(bra_division,bra_teams,bra_avg_HG,bra_home_as,bra_home_ds)
#################################################################################
#away poisson data
#bra
bra_division <- c()
bra_division[1:length(bra_teams)] <- "BRA"
bra_away_poisson <- cbind(bra_division,bra_teams,bra_avg_AG,bra_away_as,bra_away_ds)

#create home and away csv
#bra_home_poisson <- rbind(bra_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#bra_away_poisson <- rbind(bra_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(bra_home_poisson,'NL/BRA.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(bra_away_poisson,'NL/BRA.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################BRA FIXTURES##########################################################################
#BRA
HomeTeam_bra <- rep(bra_teams, each = length(bra_teams))
AwayTeam_bra <- rep(bra_teams, length(bra_teams))
BRA_fixtures <- cbind(HomeTeam_bra,AwayTeam_bra)
BRA_fixtures <- as.data.frame(BRA_fixtures)
BRA_fixtures <- BRA_fixtures[!BRA_fixtures$HomeTeam_bra == BRA_fixtures$AwayTeam_bra,]
rownames(BRA_fixtures) <- NULL
BRA_fixtures$Div <- "BRA"
BRA_fixtures <- BRA_fixtures[,c(3,1,2)]

BRA_fixtures$avg_HG_bra <- bra_avg_HG

BRA_fixtures$bra_homeas <- rep(bra_home_as,each = length(bra_teams)-1)

bra_awayds_lookup <- cbind(bra_teams,bra_away_ds)

bra_awayds_lookup <- as.data.frame(bra_awayds_lookup)

colnames(bra_awayds_lookup) <- c("AwayTeam_bra","bra_awayds")


require('RH2')
BRA_fixtures$bra_awayds <- sqldf("SELECT bra_awayds_lookup.bra_awayds FROM bra_awayds_lookup INNER JOIN BRA_fixtures ON bra_awayds_lookup.AwayTeam_bra = BRA_fixtures.AwayTeam_bra")

BRA_fixtures$avg_AG_bra <- bra_avg_AG

bra_awayas_lookup <- cbind(bra_teams,bra_away_as)

bra_awayas_lookup <- as.data.frame(bra_awayas_lookup)

colnames(bra_awayas_lookup) <- c("AwayTeam_bra","bra_awayas")


BRA_fixtures$bra_awayas <- sqldf("SELECT bra_awayas_lookup.bra_awayas FROM bra_awayas_lookup INNER JOIN BRA_fixtures ON bra_awayas_lookup.AwayTeam_bra = BRA_fixtures.AwayTeam_bra")

BRA_fixtures$bra_homeds <- rep(bra_home_ds,each = length(bra_teams)-1)

BRA_fixtures$bra_awayds <- as.numeric(unlist(BRA_fixtures$bra_awayds))
#xGH
BRA_fixtures$bra_xGH <- BRA_fixtures$avg_HG_bra * BRA_fixtures$bra_homeas * BRA_fixtures$bra_awayds

#xGA

BRA_fixtures$bra_awayas <- as.numeric(unlist(BRA_fixtures$bra_awayas))

BRA_fixtures$bra_xGA <- BRA_fixtures$avg_AG_bra * BRA_fixtures$bra_awayas * BRA_fixtures$bra_homeds

BRA_fixtures$bra_0_0 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_0 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_0_1 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_1 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_0 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_0_2 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_2 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_1 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_2 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_3 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_0 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_1 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_2 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_0_3 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_3 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_3 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_4 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_0 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_1 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_2 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_3 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_0_4 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_4 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_4 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_4 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_5 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_0 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_1 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_2 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_3 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_4 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_0_5 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_5 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_5 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_5 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_5 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_6 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_0 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(0,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_1 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(1,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_2 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(2,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_3 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(3,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_4 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(4,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_6_5 <- round(stats::dpois(6,BRA_fixtures$bra_xGH) * stats::dpois(5,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_0_6 <- round(stats::dpois(0,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_1_6 <- round(stats::dpois(1,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_2_6 <- round(stats::dpois(2,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_3_6 <- round(stats::dpois(3,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_4_6 <- round(stats::dpois(4,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
BRA_fixtures$bra_5_6 <- round(stats::dpois(5,BRA_fixtures$bra_xGH) * stats::dpois(6,BRA_fixtures$bra_xGA), digits = 4)
#Home win
BRA_fixtures$bra_H <- (
  BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
    BRA_fixtures$bra_5_0 + BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5
)

BRA_fixtures$bra_H <- percent(BRA_fixtures$bra_H, accuracy = 0.1)

#Draw
BRA_fixtures$bra_D <- (

  BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 + BRA_fixtures$bra_4_4 +
    BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6
)

BRA_fixtures$bra_D <- percent(BRA_fixtures$bra_D, accuracy = 0.1)

#Away

BRA_fixtures$bra_A <- (
  BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
    BRA_fixtures$bra_0_5 + BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6
)

BRA_fixtures$bra_A <- percent(BRA_fixtures$bra_A, accuracy = 0.1)

#ov25
BRA_fixtures$bra_ov25 <- (
  BRA_fixtures$bra_2_1 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 + BRA_fixtures$bra_2_3 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 + BRA_fixtures$bra_0_4 +
    BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 + BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_0 +
    BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 + BRA_fixtures$bra_0_5 +
    BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 + BRA_fixtures$bra_5_5 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5 + BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 +
    BRA_fixtures$bra_4_6 + BRA_fixtures$bra_5_6 + BRA_fixtures$bra_6_6
)
#un25
BRA_fixtures$bra_un25 <- (
  BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_0 + BRA_fixtures$bra_0_1 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_0_2
)
#odds
BRA_fixtures$bra_ov25_odds <- round((1/BRA_fixtures$bra_ov25),digits = 2)
BRA_fixtures$bra_un25_odds <- round((1/BRA_fixtures$bra_un25),digits = 2)

BRA_fixtures$bra_ov25_odds
BRA_fixtures$bra_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
BRA_fixtures$bra_BTTSY <- (
  BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_3_1 + BRA_fixtures$bra_3_2 +
    BRA_fixtures$bra_2_2 + BRA_fixtures$bra_1_3 + BRA_fixtures$bra_2_3 + BRA_fixtures$bra_3_3 + BRA_fixtures$bra_4_4 +
    BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_3 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 +
    BRA_fixtures$bra_3_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 +
    BRA_fixtures$bra_5_4 + BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_6_6 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6
)
#BTTSN
BRA_fixtures$bra_BTTSN <- (
  BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_0 + BRA_fixtures$bra_0_1 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_0_2 +
    BRA_fixtures$bra_3_0 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_5_0 +
    BRA_fixtures$bra_0_5 + BRA_fixtures$bra_6_0 + BRA_fixtures$bra_0_6
)

BRA_fixtures$bra_BTTSY_odds <- round((1/BRA_fixtures$bra_BTTSY),digits = 2)
BRA_fixtures$bra_BTTSN_odds <- round((1/BRA_fixtures$bra_BTTSN),digits = 2)

BRA_fixtures$bra_BTTSY <- percent(BRA_fixtures$bra_BTTSY, accuracy = 0.1)
BRA_fixtures$bra_BTTSN <- percent(BRA_fixtures$bra_BTTSN, accuracy = 0.1)
#odds
BRA_fixtures$bra_BTTSY_odds
BRA_fixtures$bra_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
BRA_fixtures$bra_AH_0_H <- (
  BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
    BRA_fixtures$bra_5_0 +BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5 + BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6
)
#AH_0_A
BRA_fixtures$bra_AH_0_A <- (
  BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
    BRA_fixtures$bra_0_5 +BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6 + BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6
)

#odds
BRA_fixtures$bra_AH_0_H_odds <- round((1/BRA_fixtures$bra_AH_0_H),digits = 2)
BRA_fixtures$bra_AH_0_A_odds <- round((1/BRA_fixtures$bra_AH_0_A),digits = 2)

BRA_fixtures$bra_AH_0_H_odds
BRA_fixtures$bra_AH_0_A_odds
#percentages
BRA_fixtures$bra_AH_0_H <- percent(BRA_fixtures$bra_AH_0_H, accuracy = 0.1)
BRA_fixtures$bra_AH_0_A <- percent(BRA_fixtures$bra_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
BRA_fixtures$bra_AH_n075_H <- (
  BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
    BRA_fixtures$bra_5_0 +BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5
)
#AH_n075_A
BRA_fixtures$bra_AH_n075_A <- (
  BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
    BRA_fixtures$bra_0_5 +BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6
)

#odds
BRA_fixtures$bra_AH_n075_H_odds <- round((1/BRA_fixtures$bra_AH_n075_H),digits = 2)
BRA_fixtures$bra_AH_n075_A_odds <- round((1/BRA_fixtures$bra_AH_n075_A),digits = 2)

BRA_fixtures$bra_AH_n075_H_odds
BRA_fixtures$bra_AH_n075_A_odds
#percentages
BRA_fixtures$bra_AH_n075_H <- percent(BRA_fixtures$bra_AH_n075_H, accuracy = 0.1)
BRA_fixtures$bra_AH_n075_A <- percent(BRA_fixtures$bra_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
BRA_fixtures$bra_AH_075_H <- (
  BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
    BRA_fixtures$bra_5_0 +BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5 + BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6 + BRA_fixtures$bra_0_1 + BRA_fixtures$bra_1_2 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_3_4 + BRA_fixtures$bra_4_5 + BRA_fixtures$bra_5_6
)
#AH_075_A
BRA_fixtures$bra_AH_075_A <- (
  BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
    BRA_fixtures$bra_0_5 +BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6 + BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6 + BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_3 + BRA_fixtures$bra_5_4 + BRA_fixtures$bra_6_5
)

#odds
BRA_fixtures$bra_AH_075_H_odds <- round((1/BRA_fixtures$bra_AH_075_H),digits = 2)
BRA_fixtures$bra_AH_075_A_odds <- round((1/BRA_fixtures$bra_AH_075_A),digits = 2)

BRA_fixtures$bra_AH_075_H_odds
BRA_fixtures$bra_AH_075_A_odds
#percentages
BRA_fixtures$bra_AH_075_H <- percent(BRA_fixtures$bra_AH_075_H, accuracy = 0.1)
BRA_fixtures$bra_AH_075_A <- percent(BRA_fixtures$bra_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
BRA_fixtures$bra_AH_n125_H <- (
  BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
    BRA_fixtures$bra_5_0 +BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5
)
#AH_n125_A
BRA_fixtures$bra_AH_n125_A <- (
  BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
    BRA_fixtures$bra_0_5 +BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6
)

#odds
BRA_fixtures$bra_AH_n125_H_odds <- round((1/BRA_fixtures$bra_AH_n125_H),digits = 2)
BRA_fixtures$bra_AH_n125_A_odds <- round((1/BRA_fixtures$bra_AH_n125_A),digits = 2)

BRA_fixtures$bra_AH_n125_H_odds
BRA_fixtures$bra_AH_n125_A_odds
#percentages
BRA_fixtures$bra_AH_n125_H <- percent(BRA_fixtures$bra_AH_n125_H, accuracy = 0.1)
BRA_fixtures$bra_AH_n125_A <- percent(BRA_fixtures$bra_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
BRA_fixtures$bra_AH_125_H <- (
  BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_0 + BRA_fixtures$bra_2_1 + BRA_fixtures$bra_3_0 + BRA_fixtures$bra_3_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_0 + BRA_fixtures$bra_4_1 + BRA_fixtures$bra_4_2 + BRA_fixtures$bra_4_3 +
    BRA_fixtures$bra_5_0 +BRA_fixtures$bra_5_1 + BRA_fixtures$bra_5_2 + BRA_fixtures$bra_5_3 + BRA_fixtures$bra_5_4 +
    BRA_fixtures$bra_6_0 + BRA_fixtures$bra_6_1 + BRA_fixtures$bra_6_2 + BRA_fixtures$bra_6_3 + BRA_fixtures$bra_6_4 +
    BRA_fixtures$bra_6_5 + BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6 + BRA_fixtures$bra_0_1 + BRA_fixtures$bra_1_2 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_3_4 + BRA_fixtures$bra_4_5 + BRA_fixtures$bra_5_6
)
#AH_125_A
BRA_fixtures$bra_AH_125_A <- (
  BRA_fixtures$bra_0_1 + BRA_fixtures$bra_0_2 + BRA_fixtures$bra_1_2 + BRA_fixtures$bra_0_3 + BRA_fixtures$bra_1_3 +
    BRA_fixtures$bra_2_3 + BRA_fixtures$bra_0_4 + BRA_fixtures$bra_1_4 + BRA_fixtures$bra_2_4 + BRA_fixtures$bra_3_4 +
    BRA_fixtures$bra_0_5 +BRA_fixtures$bra_1_5 + BRA_fixtures$bra_2_5 + BRA_fixtures$bra_3_5 + BRA_fixtures$bra_4_5 +
    BRA_fixtures$bra_0_6 + BRA_fixtures$bra_1_6 + BRA_fixtures$bra_2_6 + BRA_fixtures$bra_3_6 + BRA_fixtures$bra_4_6 +
    BRA_fixtures$bra_5_6 + BRA_fixtures$bra_0_0 + BRA_fixtures$bra_1_1 + BRA_fixtures$bra_2_2 + BRA_fixtures$bra_3_3 +
    BRA_fixtures$bra_4_4 + BRA_fixtures$bra_5_5 + BRA_fixtures$bra_6_6 + BRA_fixtures$bra_1_0 + BRA_fixtures$bra_2_1 +
    BRA_fixtures$bra_3_2 + BRA_fixtures$bra_4_3 + BRA_fixtures$bra_5_4 + BRA_fixtures$bra_6_5
)

#odds
BRA_fixtures$bra_AH_125_H_odds <- round((1/BRA_fixtures$bra_AH_125_H),digits = 2)
BRA_fixtures$bra_AH_125_A_odds <- round((1/BRA_fixtures$bra_AH_125_A),digits = 2)

BRA_fixtures$bra_AH_125_H_odds
BRA_fixtures$bra_AH_125_A_odds
#percentages
BRA_fixtures$bra_AH_125_H <- percent(BRA_fixtures$bra_AH_125_H, accuracy = 0.1)
BRA_fixtures$bra_AH_125_A <- percent(BRA_fixtures$bra_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
BRA_fixtures$bra_ov25 <- percent(BRA_fixtures$bra_ov25, accuracy = 0.1)

BRA_fixtures$bra_un25 <- percent(BRA_fixtures$bra_un25, accuracy = 0.1)
BRA_fixtures$bra_pscore <- paste(round(BRA_fixtures$bra_xGH,digits = 0),round(BRA_fixtures$bra_xGA,digits = 0),sep = "-")
#write out
write.xlsx(BRA_fixtures,'NL/BRA.xlsx',sheetName = "BRA", append = TRUE)
###########################################################################################################
########################BRA END###########################################################################
BRA <- read.csv('../FDAS/BRA.csv')
BRA$TG <- BRA$HG + BRA$AG
BRA$OV25 <- ifelse(BRA$TG >= 3,"Y","N")
bra_ftr_summary <- tabyl(BRA,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
bra_ov25_summary <- tabyl(BRA,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(bra_ftr_summary,'NL/BRA.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(bra_ov25_summary,'NL/BRA.xlsx',sheetName = "OVUN25", append = TRUE)



