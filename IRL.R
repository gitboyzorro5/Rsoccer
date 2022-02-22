library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
library('mgsub')
#delete current file
unlink('NL/IRL.xlsx')
######################IRL START#######################################
#####################################################################
IRL <- read.csv('../FDAS/IRL.csv')
IRL <- within(IRL,rm(Res))
IRL$Date <- dmy(IRL$Date)
IRL <- IRL[order(as.Date(IRL$Date, format = "%d/%m%Y"), decreasing = FALSE),]
IRL$CS <- paste(IRL$HG,IRL$AG, sep = "-")

#IRL_qualificaton <- subset(IRL,tournament == "UEFA Euro qualification")
IRL <- subset(IRL,Season == "2021")
#IRL <- IRL[IRL$Date > '2008-01-01',])
IRL$TG <- IRL$HG + IRL$AG
IRL$OV25 <- ifelse(IRL$TG >= 3,"Y","N")
IRL$FTR <- with(IRL,
               ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)

###################################################
# IRL <- mgsub(IRL,c("Wolfsberger"),c("Wolfsberger AC"))
# IRL <- mgsub(IRL,c("Wolfsberger AC AC"),c("Wolfsberger AC"))
####GoalTotalsv2##################################
irl_totalgoalsv2 <- tapply(IRL$TG, IRL[c("Home", "Away")],mean)
irl_totalgoalsv2
irl_hgtotals <- rowSums(irl_totalgoalsv2,na.rm = T)
irl_agtotals <- colSums(irl_totalgoalsv2,na.rm = T)

irl_totalgoals <- irl_hgtotals + irl_agtotals
irl_totalgoalsv2 <- cbind(irl_totalgoalsv2,irl_totalgoals)
irl_teams <- sort(unique(IRL$Home))
irl_home_games <- c()
irl_away_games <-c()
for (i_irl in 1:length(irl_teams))
{

  irl_home_games[i_irl] <- nrow(IRL[IRL$Home == irl_teams[i_irl],])
  irl_away_games[i_irl]  <- nrow(IRL[IRL$Away == irl_teams[i_irl],])

}
irl_games_played <- irl_home_games + irl_away_games
irl_goaltotalsv2 <- cbind(irl_totalgoalsv2,irl_games_played)
irl_avg_totalgoals <- round((irl_totalgoals/ irl_games_played), digits = 4)
irl_goaltotalsv2[is.na(irl_goaltotalsv2)] <- ""
irl_goaltotalsv2 <- cbind(irl_goaltotalsv2,irl_avg_totalgoals)
write.xlsx(irl_goaltotalsv2,'NL/IRL.xlsx',sheetName = "totalgoalsv2")
#####################################################################
#irl goal scored rounds
#####################################################################
irl_krounds <- tail(unique(IRL_rounds$irl_matchday),1)
nrow(IRL)
irl_goalscoredmatrix <- data.frame(matrix(nrow = length(irl_teams),ncol = irl_krounds))
irl_goalscoredround <- c()
for(i_irl_krounds in 1:irl_krounds)
{
  irl_homegoalscored <- IRL_rounds$HG[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awaygoalscored <- IRL_rounds$AG[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_hometeamstemp_gs <- IRL_rounds$Home[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awayteamstemp_gs <- IRL_rounds$Away[IRL_rounds$irl_matchday== i_irl_krounds]

  irl_goalscombined <- c(irl_homegoalscored,irl_awaygoalscored)
  irl_teamscombined <- c(irl_hometeamstemp_gs,irl_awayteamstemp_gs)

  irl_goalscoredround <- data.frame(irl_teamscombined,irl_goalscombined)

  irl_goalscoredround <- irl_goalscoredround[order(irl_goalscoredround$irl_teamscombined),]
  irl_goalscoredround$irl_teamscombined <- NULL
  irl_goalscoredmatrix[,i_irl_krounds] <- irl_goalscoredround

}

irl_goalscoredmatrix <- cbind(irl_teams,irl_goalscoredmatrix)
####GSmatrix################################
#create home and away matrices
irl_goalscored_h <- tapply(IRL$HG, IRL[c("Home", "Date")],mean)
irl_goalscored_a <- tapply(IRL$AG, IRL[c("Away", "Date")],mean)
irl_goalscored_h[is.na(irl_goalscored_h)] <- ""
irl_goalscored_a[is.na(irl_goalscored_a)] <- ""

for(irl_rowhgs in 1:nrow(irl_goalscored_h)) {
  for(irl_colhgs in 1:ncol(irl_goalscored_h)) {

    # print(my_matrix[row, col])
    for(irl_rowags in 1:nrow(irl_goalscored_a)) {
      for(irl_colags in 1:ncol(irl_goalscored_a)) {
        ifelse(!irl_goalscored_a[irl_rowags,irl_colags]=="",irl_goalscored_h[irl_rowags,irl_colags] <- irl_goalscored_a[irl_rowags,irl_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(irl_goalscoredmatrix,'NL/IRL.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#irl goal conceded rounds
#irl
irl_krounds <- tail(unique(IRL_rounds$irl_matchday),1)
irl_goalconcededmatrix <- data.frame(matrix(nrow = length(irl_teams),ncol = irl_krounds))
irl_goalconcededround <- c()
for(i_irl_krounds in 1:irl_krounds)
{
  irl_homegoalconceded <- IRL_rounds$AG[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awaygoalconceded <- IRL_rounds$HG[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_hometeamstemp_gc <- IRL_rounds$Home[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awayteamstemp_gc <- IRL_rounds$Away[IRL_rounds$irl_matchday== i_irl_krounds]

  irl_goalsconcededcombined <- c(irl_homegoalconceded,irl_awaygoalconceded)
  irl_teamscombined_gc <- c(irl_hometeamstemp_gc,irl_awayteamstemp_gc)

  irl_goalconcededround <- data.frame(irl_teamscombined_gc,irl_goalsconcededcombined)

  irl_goalconcededround <- irl_goalconcededround[order(irl_goalconcededround$irl_teamscombined_gc),]
  irl_goalconcededround$irl_teamscombined_gc <- NULL
  irl_goalconcededmatrix[,i_irl_krounds] <- irl_goalconcededround

}

irl_goalconcededmatrix <- cbind(irl_teams,irl_goalconcededmatrix)

####GCmatrix#############################################################################
#create home and away matrices
irl_goalconceded_h <- tapply(IRL$AG, IRL[c("Home", "Date")],mean)
irl_goalconceded_a <- tapply(IRL$HG, IRL[c("Away", "Date")],mean)
irl_goalconceded_h[is.na(irl_goalconceded_h)] <- ""
irl_goalconceded_a[is.na(irl_goalconceded_a)] <- ""

for(irl_rowhgc in 1:nrow(irl_goalconceded_h)) {
  for(irl_colhgc in 1:ncol(irl_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(irl_rowagc in 1:nrow(irl_goalconceded_a)) {
      for(irl_colagc in 1:ncol(irl_goalconceded_a)) {
        ifelse(!irl_goalconceded_a[irl_rowagc,irl_colagc]=="",irl_goalconceded_h[irl_rowagc,irl_colagc] <- irl_goalconceded_a[irl_rowagc,irl_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(irl_goalconcededmatrix,'NL/IRL.xlsx',sheetName = "gcmatrix", append = TRUE)
########################################################################################
#irl team form
irl_krounds <- tail(unique(IRL_rounds$irl_matchday),1)
irl_formmatrix <- data.frame(matrix(nrow = length(irl_teams),ncol = irl_krounds))
irl_formround <- c()
for(i_irl_krounds in 1:irl_krounds)
{
  irl_homeform <- IRL_rounds$FTR[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_homeform <- sub("H","W",irl_homeform)
  irl_homeform <- sub("A","L",irl_homeform)

  irl_awayform <- IRL_rounds$FTR[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awayform <- sub("A","W",irl_awayform)
  irl_awayform <- sub("H","L",irl_awayform)

  irl_hometeamstemp_form <- IRL_rounds$Home[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awayteamstemp_form <- IRL_rounds$Away[IRL_rounds$irl_matchday== i_irl_krounds]

  irl_formcombined <- c(irl_homeform,irl_awayform)
  irl_teamscombined_form <- c(irl_hometeamstemp_form,irl_awayteamstemp_form)

  irl_formround <- data.frame(irl_teamscombined_form,irl_formcombined)

  irl_formround <- irl_formround[order(irl_formround$irl_teamscombined_form),]
  irl_formround$irl_teamscombined_form <- NULL
  irl_formmatrix[,i_irl_krounds] <- irl_formround

}

irl_formmatrix <- cbind(irl_teams,irl_formmatrix)
########################################################################################
########################################################################################
#########################################################################################
####Teamform#############################################################################

irl_form_h <- tapply(IRL$FTR, IRL[c("Home", "Date")],median)
irl_form_a <- tapply(IRL$FTR, IRL[c("Away", "Date")],median)
irl_form_h[is.na(irl_form_h)] <- ""
irl_form_a[is.na(irl_form_a)] <- ""
irl_form_h <- sub("A","L",irl_form_h)
irl_form_h <- sub("H","W",irl_form_h)
irl_form_a <- sub("A","W",irl_form_a)
irl_form_a <- sub("H","L",irl_form_a)
for(irl_rowh_f in 1:nrow(irl_form_h)) {
  for(irl_colh_f in 1:ncol(irl_form_h)) {

    # print(my_matrix[row, col])
    for(irl_rowa_f in 1:nrow(irl_form_a)) {
      for(irl_cola_f in 1:ncol(irl_form_a)) {
        ifelse(!irl_form_a[irl_rowa_f,irl_cola_f]=="",irl_form_h[irl_rowa_f,irl_cola_f] <- irl_form_a[irl_rowa_f,irl_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(irl_formmatrix,'NL/IRL.xlsx',sheetName = "form", append = TRUE)
##################################################################################
##################################################################################
#irl total goals rounds
irl_krounds <- tail(unique(IRL_rounds$irl_matchday),1)
irl_goaltotalmatrix <- data.frame(matrix(nrow = length(irl_teams),ncol = irl_krounds))
irl_goaltotalround <- c()
for(i_irl_krounds in 1:irl_krounds)
{
  irl_homegoaltotal <- IRL_rounds$TG[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awaygoaltotal <- IRL_rounds$TG[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_hometeamstemp_tg <- IRL_rounds$Home[IRL_rounds$irl_matchday == i_irl_krounds]

  irl_awayteamstemp_tg <- IRL_rounds$Away[IRL_rounds$irl_matchday== i_irl_krounds]

  irl_goalscombined_tg <- c(irl_homegoaltotal,irl_awaygoaltotal)
  irl_teamscombined_tg <- c(irl_hometeamstemp_tg,irl_awayteamstemp_tg)

  irl_goaltotalround <- data.frame(irl_teamscombined_tg,irl_goalscombined_tg)

  irl_goaltotalround <- irl_goaltotalround[order(irl_goaltotalround$irl_teamscombined_tg),]
  irl_goaltotalround$irl_teamscombined_tg <- NULL
  irl_goaltotalmatrix[,i_irl_krounds] <- irl_goaltotalround

}

irl_goaltotalmatrix <- cbind(irl_teams,irl_goaltotalmatrix)
##############################################################################################
#d1
#######TGMatrix##################################################################
irl_totalgoals_h <- tapply(IRL$TG, IRL[c("Home", "Date")],mean)
irl_totalgoals_a <- tapply(IRL$TG, IRL[c("Away", "Date")],mean)
irl_totalgoals_h[is.na(irl_totalgoals_h)] <- ""
irl_totalgoals_a[is.na(irl_totalgoals_a)] <- ""
for(irl_rowh in 1:nrow(irl_totalgoals_h)) {
  for(irl_colh in 1:ncol(irl_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(irl_rowa in 1:nrow(irl_totalgoals_a)) {
      for(irl_cola in 1:ncol(irl_totalgoals_a)) {
        ifelse(!irl_totalgoals_a[irl_rowa,irl_cola]=="",irl_totalgoals_h[irl_rowa,irl_cola] <- irl_totalgoals_a[irl_rowa,irl_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(irl_goaltotalmatrix,'NL/IRL.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
irl_form_team_against_h <- tapply(IRL$Away, IRL[c("Home", "Date")],median)
irl_form_team_against_a <- tapply(IRL$Home, IRL[c("Away", "Date")],median)
irl_form_team_against_h[is.na(irl_form_team_against_h)] <- ""
irl_form_team_against_a[is.na(irl_form_team_against_a)] <- ""
for(irl_rowh_f_against in 1:nrow(irl_form_team_against_h)) {
  for(irl_colh_f_against in 1:ncol(irl_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(irl_rowa_f_against in 1:nrow(irl_form_team_against_a)) {
      for(irl_cola_f_against in 1:ncol(irl_form_team_against_a)) {
        ifelse(!irl_form_team_against_a[irl_rowa_f_against,irl_cola_f_against]=="",irl_form_team_against_h[irl_rowa_f_against,irl_cola_f_against] <- irl_form_team_against_a[irl_rowa_f_against,irl_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
#win margin
irl_winmargin_h <- tapply(IRL$HG - IRL$AG, IRL[c("Home", "Date")],mean)
irl_winmargin_a <- tapply(IRL$AG - IRL$HG, IRL[c("Away", "Date")],mean)
irl_winmargin_h[is.na(irl_winmargin_h)] <- ""
#
for(irl_rowhwm in 1:nrow(irl_winmargin_h)) {
  for(irl_colhwm in 1:ncol(irl_winmargin_h)) {

    # print(my_matrix[row, col])
    for(irl_rowawm in 1:nrow(irl_winmargin_a)) {
      for(irl_colawm in 1:ncol(irl_winmargin_a)) {
        ifelse(!irl_winmargin_a[irl_rowawm,irl_colawm]=="",irl_winmargin_h[irl_rowawm,irl_colawm] <- irl_winmargin_a[irl_rowawm,irl_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
####################################################################################################################
##########Goals over under############
#IRL
irl_un05_home <- c()
irl_un05_away <- c()
irl_ov05_home <- c()
irl_ov05_away <- c()

irl_un15_home <- c()
irl_un15_away <- c()
irl_ov15_home <- c()
irl_ov15_away <- c()

irl_un25_home <- c()
irl_un25_away <- c()
irl_ov25_home <- c()
irl_ov25_away <- c()

irl_un35_home <- c()
irl_un35_away <- c()
irl_ov35_home <- c()
irl_ov35_away <- c()

irl_un45_home <- c()
irl_un45_away <- c()
irl_ov45_home <- c()
irl_ov45_away <- c()

irl_un55_home <- c()
irl_un55_away <- c()
irl_ov55_home <- c()
irl_ov55_away <- c()

for (i_irl_tg in 1:length(irl_teams))
{

  irl_un05_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG == 0,])
  irl_un05_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG == 0,])

  irl_ov05_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG > 0,])
  irl_ov05_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG > 0,])

  irl_un15_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG <= 1,])
  irl_un15_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG <= 1,])

  irl_ov15_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG >= 2,])
  irl_ov15_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG >= 2,])

  irl_un25_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG <= 2,])
  irl_un25_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG <= 2,])

  irl_ov25_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG >=3,])
  irl_ov25_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG >=3,])

  irl_un35_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG <= 3,])
  irl_un35_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG <= 3,])

  irl_ov35_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG >= 4,])
  irl_ov35_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG >= 4,])

  irl_un45_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG <= 4,])
  irl_un45_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG <= 4,])

  irl_ov45_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG >= 5,])
  irl_ov45_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG >= 5,])

  irl_un55_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG <= 5,])
  irl_un55_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG <= 5,])

  irl_ov55_home[i_irl_tg] <- nrow(IRL[IRL$Home == irl_teams[i_irl_tg] & IRL$TG >= 6,])
  irl_ov55_away[i_irl_tg] <- nrow(IRL[IRL$Away == irl_teams[i_irl_tg] & IRL$TG >= 6,])


}

irl_un05 <- irl_un05_home + irl_un05_away
irl_ov05 <- irl_ov05_home + irl_ov05_away

irl_un15 <- irl_un15_home + irl_un15_away
irl_ov15 <- irl_ov15_home + irl_ov15_away

irl_un25 <- irl_un25_home + irl_un25_away
irl_ov25 <- irl_ov25_home + irl_ov25_away

irl_un35 <- irl_un35_home + irl_un35_away
irl_ov35 <- irl_ov35_home + irl_ov35_away

irl_un45 <- irl_un45_home + irl_un45_away
irl_ov45 <- irl_ov45_home + irl_ov45_away

irl_un55 <- irl_un55_home + irl_un55_away
irl_ov55 <- irl_ov55_home + irl_ov55_away

irl_ovundata <- cbind(irl_teams,irl_un05,irl_ov05,irl_un15,irl_ov15,irl_un25,irl_ov25,irl_un35,irl_ov35,irl_un45,irl_ov45,irl_un55,irl_ov55)
write.xlsx(irl_ovundata,'NL/IRL.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
irl_csform_h <- tapply(IRL$CS, IRL[c("Home", "Date")],median)
irl_csform_a <- tapply(IRL$CS, IRL[c("Away", "Date")],median)

irl_csform_h[is.na(irl_csform_h)] <- ""
irl_csform_a[is.na(irl_csform_a)] <- ""

for(irl_rowh_f_cs in 1:nrow(irl_csform_h)) {
  for(irl_colh_f_cs in 1:ncol(irl_csform_h)) {

    # print(my_matrix[row, col])
    for(irl_rowa_f_cs in 1:nrow(irl_csform_a)) {
      for(irl_cola_f_cs in 1:ncol(irl_csform_a)) {
        ifelse(!irl_csform_a[irl_rowa_f_cs,irl_cola_f_cs]=="",irl_csform_h[irl_rowa_f_cs,irl_cola_f_cs] <- irl_csform_a[irl_rowa_f_cs,irl_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
irl_home_gs <- aggregate(IRL$HG, by = list(IRL$Home), FUN = sum)
irl_home_gs_avg <- aggregate(IRL$HG, by = list(IRL$Home),mean)
irl_home_scoring <- merge(irl_home_gs,irl_home_gs_avg, by='Group.1',all = T)
names(irl_home_scoring)[names(irl_home_scoring) == "x.x"] <- "TFthg"
names(irl_home_scoring)[names(irl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
irl_away_gs <- aggregate(IRL$AG, by = list(IRL$Away), FUN = sum)
irl_away_gs_avg <- aggregate(IRL$AG, by = list(IRL$Away),mean)
irl_away_scoring <- merge(irl_away_gs,irl_away_gs_avg, by='Group.1',all = T)
names(irl_away_scoring)[names(irl_away_scoring) == "x.x"] <- "TFtag"
names(irl_away_scoring)[names(irl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
irl_scoring <- merge(irl_home_scoring,irl_away_scoring,by='Group.1',all = T)
irl_scoring$TGS <- irl_scoring$TFthg + irl_scoring$TFtag

#home goals conceded
irl_home_gc <- aggregate(IRL$AG, by = list(IRL$Home), FUN = sum)
irl_home_gc_avg <- aggregate(IRL$AG, by = list(IRL$Home),mean)
irl_home_conceding <- merge(irl_home_gc,irl_home_gc_avg, by='Group.1',all = T)
names(irl_home_conceding)[names(irl_home_conceding) == "x.x"] <- "TFthc"
names(irl_home_conceding)[names(irl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
irl_away_gc <- aggregate(IRL$HG, by = list(IRL$Away), FUN = sum)
irl_away_gc_avg <- aggregate(IRL$HG, by = list(IRL$Away),mean)
irl_away_conceding <- merge(irl_away_gc,irl_away_gc_avg, by='Group.1',all = T)
names(irl_away_conceding)[names(irl_away_conceding) == "x.x"] <- "TFtac"
names(irl_away_conceding)[names(irl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
irl_conceding <- merge(irl_home_conceding,irl_away_conceding,by='Group.1',all = T)
irl_conceding$TGC <- irl_conceding$TFthc + irl_conceding$TFtac

######################################################################################
###########League Table###############################################################

#hwins and away wins
irl_home_wins <- c()
irl_away_wins <- c()
irl_home_draws <- c()
irl_away_draws <- c()
irl_home_loss <- c()
irl_away_loss <- c()



for (i_irl_wins in 1:length(irl_teams))
{

  irl_home_wins[i_irl_wins] <- nrow(IRL[IRL$Home == irl_teams[i_irl_wins] & IRL$FTR == "H",])
  irl_away_wins[i_irl_wins] <- nrow(IRL[IRL$Away == irl_teams[i_irl_wins] & IRL$FTR == "A",])
  irl_home_draws[i_irl_wins] <- nrow(IRL[IRL$Home == irl_teams[i_irl_wins] & IRL$FTR == "D",])
  irl_away_draws[i_irl_wins] <- nrow(IRL[IRL$Away == irl_teams[i_irl_wins] & IRL$FTR == "D",])
  irl_home_loss[i_irl_wins] <- nrow(IRL[IRL$Home == irl_teams[i_irl_wins] & IRL$FTR == "A",])
  irl_away_loss[i_irl_wins] <- nrow(IRL[IRL$Away == irl_teams[i_irl_wins] & IRL$FTR == "H",])

}

irl_total_wins <- irl_home_wins + irl_away_wins
irl_total_draws <- irl_home_draws + irl_away_draws
irl_total_loss <- irl_home_loss + irl_away_loss

irl_league_table <- cbind(irl_teams,irl_games_played,irl_total_wins,irl_total_draws,irl_total_loss)
irl_GS <- irl_scoring$TGS
irl_GC <-irl_conceding$TGC
irl_GD <- irl_scoring$TGS - irl_conceding$TGC
irl_PTS <- (irl_total_wins*3) + (irl_total_draws*1)
irl_league_table <- cbind(irl_league_table,irl_GS,irl_GC,irl_GD,irl_PTS)
irl_league_table <- as.data.frame(irl_league_table)
#rename the columns
names(irl_league_table)[names(irl_league_table) == "irl_teams"] <- "Team"
names(irl_league_table)[names(irl_league_table) == "irl_games_played"] <- "P"
names(irl_league_table)[names(irl_league_table) == "irl_total_wins"] <- "W"
names(irl_league_table)[names(irl_league_table) == "irl_total_draws"] <- "D"
names(irl_league_table)[names(irl_league_table) == "irl_total_loss"] <- "L"
names(irl_league_table)[names(irl_league_table) == "irl_GS"] <- "F"
names(irl_league_table)[names(irl_league_table) == "irl_GC"] <- "A"
points_irl <- irl_league_table[order(as.numeric(irl_league_table$irl_PTS), decreasing = TRUE),]
points_irl$irl_rank <- 1:length(irl_teams)
row.names(points_irl) <- points_irl$irl_rank
#create final_irl_hf_against with team ranks in brackets
for(irl_rowhrank in 1:nrow(irl_form_team_against_h)) {
  for(irl_colhrank in 1:ncol(irl_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!irl_form_team_against_h[irl_rowhrank,irl_colhrank]=="",irl_form_team_against_h[irl_rowhrank,irl_colhrank] <- paste(irl_form_team_against_h[irl_rowhrank,irl_colhrank],"(",points_irl$irl_rank[points_irl$Team ==irl_form_team_against_h[irl_rowhrank,irl_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_irl,'NL/IRL.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six irl###################################################
#IRL
#form
#create final_irl_hf object
#irl_last_n_games <- 6
final_irl_hf <- c()
for(index_irl_hf in 1:length(irl_teams))
{
  index_irl_hf <- row.names(irl_form_h) == irl_teams[index_irl_hf]
  form_irl_hf <- irl_form_h[index_irl_hf]
  deleted_form_irl_hf <- form_irl_hf[!form_irl_hf[] == ""]
  l6_form_irl_hf <- tail(deleted_form_irl_hf,irl_last_n_games)
  l6_form_irl_hf <- paste(l6_form_irl_hf,collapse = " ")
  final_irl_hf[index_irl_hf] <- rbind(paste(irl_teams[index_irl_hf],l6_form_irl_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",irl_teams[index],l6_form)

}

#change column names
final_irl_hf <- as.data.frame(final_irl_hf)
colnames(final_irl_hf) <- "Form"
#goals scored
#create final_irl_gs object
final_irl_gs <- c()
suml6_irl_gs <- c()
for(index_irl_gs in 1:length(irl_teams))
{
  index_irl_gs <- row.names(irl_goalscored_h) == irl_teams[index_irl_gs]
  form_irl_gs <- irl_goalscored_h[index_irl_gs]
  deleted_form_irl_gs <- form_irl_gs[!form_irl_gs[] == ""]
  l6_form_irl_gs <- tail(deleted_form_irl_gs,irl_last_n_games)
  l6_form_irl_gs <- as.numeric(l6_form_irl_gs)
  suml6_irl_gs[index_irl_gs] <- sum(l6_form_irl_gs)
  suml6_irl_gs[index_irl_gs] <- paste("(",suml6_irl_gs[index_irl_gs],")",sep = "")
  l6_form_irl_gs <- paste(l6_form_irl_gs,collapse = " ")
  final_irl_gs[index_irl_gs] <- rbind(paste(irl_teams[index_irl_gs],l6_form_irl_gs,suml6_irl_gs[index_irl_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",irl_teams[index],l6_form)

}
final_irl_gs
#change column names
final_irl_gs <- as.data.frame(final_irl_gs)
colnames(final_irl_gs) <- "Goals scored"
#goal conceded
#create final_irl_gc object
final_irl_gc <- c()
suml6_irl_gc <- c()
for(index_irl_gc in 1:length(irl_teams))
{
  index_irl_gc <- row.names(irl_goalconceded_h) == irl_teams[index_irl_gc]
  form_irl_gc <- irl_goalconceded_h[index_irl_gc]
  deleted_form_irl_gc <- form_irl_gc[!form_irl_gc[] == ""]
  l6_form_irl_gc <- tail(deleted_form_irl_gc,irl_last_n_games)
  l6_form_irl_gc <- as.numeric(l6_form_irl_gc)
  suml6_irl_gc[index_irl_gc] <- sum(l6_form_irl_gc)
  suml6_irl_gc[index_irl_gc] <- paste("(",suml6_irl_gc[index_irl_gc],")",sep = "")
  l6_form_irl_gc <- paste(l6_form_irl_gc,collapse = " ")
  final_irl_gc[index_irl_gc] <- rbind(paste(irl_teams[index_irl_gc],l6_form_irl_gc,suml6_irl_gc[index_irl_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",irl_teams[index],l6_form)

}

#change column names
final_irl_gc <- as.data.frame(final_irl_gc)
colnames(final_irl_gc) <- "Goals conceded"
#total goals
#create final_irl_tg object
final_irl_tg <- c()
suml6_irl_tg <- c()
for(index_irl_tg in 1:length(irl_teams))
{
  index_irl_tg <- row.names(irl_totalgoals_h) == irl_teams[index_irl_tg]
  form_irl_tg <- irl_totalgoals_h[index_irl_tg]
  deleted_form_irl_tg <- form_irl_tg[!form_irl_tg[] == ""]
  l6_form_irl_tg <- tail(deleted_form_irl_tg,irl_last_n_games)
  l6_form_irl_tg <- as.numeric(l6_form_irl_tg)
  suml6_irl_tg[index_irl_tg] <- sum(l6_form_irl_tg)
  suml6_irl_tg[index_irl_tg] <- paste("(",suml6_irl_tg[index_irl_tg],")",sep = "")
  l6_form_irl_tg <- paste(l6_form_irl_tg,collapse = " ")
  final_irl_tg[index_irl_tg] <- rbind(paste(irl_teams[index_irl_tg],l6_form_irl_tg,suml6_irl_tg[index_irl_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",irl_teams[index],l6_form)

}
#change column names
final_irl_tg <- as.data.frame(final_irl_tg)
colnames(final_irl_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_irl_hf object
final_irl_cs <- c()
for(index_irl_cs in 1:length(irl_teams))
{
  index_irl_cs <- row.names(irl_csform_h) == irl_teams[index_irl_cs]
  csform_irl_cs <- irl_csform_h[index_irl_cs]
  deleted_csform_irl_cs <- csform_irl_cs[!csform_irl_cs[] == ""]
  l6_csform_irl_cs <- tail(deleted_csform_irl_cs,irl_last_n_games)
  l6_csform_irl_cs <- paste(l6_csform_irl_cs,collapse = " ")
  final_irl_cs[index_irl_cs] <- rbind(paste(irl_teams[index_irl_cs],l6_csform_irl_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",irl_teams[index],l6_csform)

}

#change column names
final_irl_cs <- as.data.frame(final_irl_cs)
colnames(final_irl_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_irl_wm object
final_irl_wm <- c()
suml6_irl_wm <- c()
for(index_irl_wm in 1:length(irl_teams))
{
  index_irl_wm <- row.names(irl_winmargin_h) == irl_teams[index_irl_wm]
  form_irl_wm <- irl_winmargin_h[index_irl_wm]
  deleted_form_irl_wm <- form_irl_wm[!form_irl_wm[] == ""]
  l6_form_irl_wm <- tail(deleted_form_irl_wm,irl_last_n_games)
  l6_form_irl_wm <- as.numeric(l6_form_irl_wm)
  suml6_irl_wm[index_irl_wm] <- sum(l6_form_irl_wm)
  suml6_irl_wm[index_irl_wm] <- paste("(",suml6_irl_wm[index_irl_wm],")",sep = "")
  l6_form_irl_wm <- paste(l6_form_irl_wm,collapse = " ")
  final_irl_wm[index_irl_wm] <- rbind(paste(irl_teams[index_irl_wm],l6_form_irl_wm,suml6_irl_wm[index_irl_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",irl_teams[index],l6_form)

}
final_irl_wm
#change column names
final_irl_wm <- as.data.frame(final_irl_wm)
colnames(final_irl_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_irl_hf_against
final_irl_hf_against <- c()
for(index_irl_hf_against in 1:length(irl_teams))
{
  index_irl_hf_against <- row.names(irl_form_team_against_h) == irl_teams[index_irl_hf_against]
  form_irl_hf_against <- irl_form_team_against_h[index_irl_hf_against]
  deleted_form_irl_hf_against <- form_irl_hf_against[!form_irl_hf_against[] == ""]
  l6_form_irl_hf_against <- tail(deleted_form_irl_hf_against,irl_last_n_games)
  l6_form_irl_hf_against <- paste(l6_form_irl_hf_against,collapse = " ")
  final_irl_hf_against[index_irl_hf_against] <- rbind(paste(irl_teams[index_irl_hf_against],l6_form_irl_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",irl_teams[index],l6_form)

}
final_irl_hf_against <- as.data.frame(final_irl_hf_against)
colnames(final_irl_hf_against) <- "Team against"
#combine the columns
final_irl_all <- cbind(final_irl_hf,final_irl_gs,final_irl_gc,final_irl_tg,final_irl_cs,final_irl_wm,final_irl_hf_against)
write.xlsx(final_irl_all,'NL/IRL.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
irl_GP <- nrow(IRL)
#Calculate total home goals for each division
irl_T_HG <- sum(irl_home_gs$x)
#calculate average home goal
irl_avg_HG <- round(irl_T_HG /irl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
irl_T_AG <- sum(irl_away_gs$x)
#calculate average away goal
irl_avg_AG <- round(irl_T_AG /irl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
irl_home_as <- round(((irl_home_gs$x/irl_home_games))/irl_avg_HG, digits = 4)
#calculate away attack strength
irl_away_as <- round(((irl_away_gs$x/irl_away_games))/irl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
irl_avg_HC <- round(irl_T_AG /irl_GP, digits = 4)
#avg away concede
irl_avg_AC <- round(irl_T_HG /irl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
irl_home_ds <- round(((irl_home_gc$x/irl_home_games))/irl_avg_HC, digits = 4)
#away defense strength
irl_away_ds <- round(((irl_away_gc$x/irl_away_games))/irl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#irl
irl_division <- c()
irl_division[1:length(irl_teams)] <- "IRL"
irl_home_poisson <- cbind(irl_division,irl_teams,irl_avg_HG,irl_home_as,irl_home_ds)
#################################################################################
#away poisson data
#irl
irl_division <- c()
irl_division[1:length(irl_teams)] <- "IRL"
irl_away_poisson <- cbind(irl_division,irl_teams,irl_avg_AG,irl_away_as,irl_away_ds)

#create home and away csv
#irl_home_poisson <- rbind(irl_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#irl_away_poisson <- rbind(irl_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(irl_home_poisson,'NL/IRL.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(irl_away_poisson,'NL/IRL.xlsx',sheetName = "awaypoisson", append = TRUE)
irl_home_poisson
irl_away_poisson
##########################################################################################################
###################IRL FIXTURES##########################################################################
#IRL
HomeTeam_irl <- rep(irl_teams, each = length(irl_teams))
AwayTeam_irl <- rep(irl_teams, length(irl_teams))
IRL_fixtures <- cbind(HomeTeam_irl,AwayTeam_irl)
IRL_fixtures <- as.data.frame(IRL_fixtures)
IRL_fixtures <- IRL_fixtures[!IRL_fixtures$HomeTeam_irl == IRL_fixtures$AwayTeam_irl,]
rownames(IRL_fixtures) <- NULL
IRL_fixtures$Div <- "IRL"
IRL_fixtures <- IRL_fixtures[,c(3,1,2)]

IRL_fixtures$avg_HG_irl <- irl_avg_HG

IRL_fixtures$irl_homeas <- rep(irl_home_as,each = length(irl_teams)-1)

irl_awayds_lookup <- cbind(irl_teams,irl_away_ds)

irl_awayds_lookup <- as.data.frame(irl_awayds_lookup)

colnames(irl_awayds_lookup) <- c("AwayTeam_irl","irl_awayds")


require('RH2')
IRL_fixtures$irl_awayds <- sqldf("SELECT irl_awayds_lookup.irl_awayds FROM irl_awayds_lookup INNER JOIN IRL_fixtures ON irl_awayds_lookup.AwayTeam_irl = IRL_fixtures.AwayTeam_irl")

IRL_fixtures$avg_AG_irl <- irl_avg_AG

irl_awayas_lookup <- cbind(irl_teams,irl_away_as)

irl_awayas_lookup <- as.data.frame(irl_awayas_lookup)

colnames(irl_awayas_lookup) <- c("AwayTeam_irl","irl_awayas")


IRL_fixtures$irl_awayas <- sqldf("SELECT irl_awayas_lookup.irl_awayas FROM irl_awayas_lookup INNER JOIN IRL_fixtures ON irl_awayas_lookup.AwayTeam_irl = IRL_fixtures.AwayTeam_irl")

IRL_fixtures$irl_homeds <- rep(irl_home_ds,each = length(irl_teams)-1)

IRL_fixtures$irl_awayds <- as.numeric(unlist(IRL_fixtures$irl_awayds))
#xGH
IRL_fixtures$irl_xGH <- IRL_fixtures$avg_HG_irl * IRL_fixtures$irl_homeas * IRL_fixtures$irl_awayds

#xGA

IRL_fixtures$irl_awayas <- as.numeric(unlist(IRL_fixtures$irl_awayas))

IRL_fixtures$irl_xGA <- IRL_fixtures$avg_AG_irl * IRL_fixtures$irl_awayas * IRL_fixtures$irl_homeds

IRL_fixtures$irl_0_0 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_0 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_0_1 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_1 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_0 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_0_2 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_2 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_1 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_2 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_3 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_0 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_1 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_2 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_0_3 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_3 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_3 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_4 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_0 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_1 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_2 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_3 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_0_4 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_4 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_4 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_4 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_5 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_0 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_1 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_2 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_3 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_4 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_0_5 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_5 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_5 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_5 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_5 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_6 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_0 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(0,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_1 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(1,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_2 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(2,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_3 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(3,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_4 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(4,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_6_5 <- round(stats::dpois(6,IRL_fixtures$irl_xGH) * stats::dpois(5,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_0_6 <- round(stats::dpois(0,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_1_6 <- round(stats::dpois(1,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_2_6 <- round(stats::dpois(2,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_3_6 <- round(stats::dpois(3,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_4_6 <- round(stats::dpois(4,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
IRL_fixtures$irl_5_6 <- round(stats::dpois(5,IRL_fixtures$irl_xGH) * stats::dpois(6,IRL_fixtures$irl_xGA), digits = 4)
#Home win
IRL_fixtures$irl_H <- (
  IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
    IRL_fixtures$irl_5_0 + IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5
)

IRL_fixtures$irl_H <- percent(IRL_fixtures$irl_H, accuracy = 0.1)

#Draw
IRL_fixtures$irl_D <- (

  IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 + IRL_fixtures$irl_4_4 +
    IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6
)

IRL_fixtures$irl_D <- percent(IRL_fixtures$irl_D, accuracy = 0.1)

#Away

IRL_fixtures$irl_A <- (
  IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
    IRL_fixtures$irl_0_5 + IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6
)

IRL_fixtures$irl_A <- percent(IRL_fixtures$irl_A, accuracy = 0.1)

#ov25
IRL_fixtures$irl_ov25 <- (
  IRL_fixtures$irl_2_1 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 + IRL_fixtures$irl_2_3 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 + IRL_fixtures$irl_0_4 +
    IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 + IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_0 +
    IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 + IRL_fixtures$irl_0_5 +
    IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 + IRL_fixtures$irl_5_5 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5 + IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 +
    IRL_fixtures$irl_4_6 + IRL_fixtures$irl_5_6 + IRL_fixtures$irl_6_6
)
#un25
IRL_fixtures$irl_un25 <- (
  IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_0 + IRL_fixtures$irl_0_1 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_0_2
)
#odds
IRL_fixtures$irl_ov25_odds <- round((1/IRL_fixtures$irl_ov25),digits = 2)
IRL_fixtures$irl_un25_odds <- round((1/IRL_fixtures$irl_un25),digits = 2)

IRL_fixtures$irl_ov25_odds
IRL_fixtures$irl_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
IRL_fixtures$irl_BTTSY <- (
  IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_3_1 + IRL_fixtures$irl_3_2 +
    IRL_fixtures$irl_2_2 + IRL_fixtures$irl_1_3 + IRL_fixtures$irl_2_3 + IRL_fixtures$irl_3_3 + IRL_fixtures$irl_4_4 +
    IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_3 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 +
    IRL_fixtures$irl_3_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 +
    IRL_fixtures$irl_5_4 + IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_6_6 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6
)
#BTTSN
IRL_fixtures$irl_BTTSN <- (
  IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_0 + IRL_fixtures$irl_0_1 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_0_2 +
    IRL_fixtures$irl_3_0 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_5_0 +
    IRL_fixtures$irl_0_5 + IRL_fixtures$irl_6_0 + IRL_fixtures$irl_0_6
)

IRL_fixtures$irl_BTTSY_odds <- round((1/IRL_fixtures$irl_BTTSY),digits = 2)
IRL_fixtures$irl_BTTSN_odds <- round((1/IRL_fixtures$irl_BTTSN),digits = 2)

IRL_fixtures$irl_BTTSY <- percent(IRL_fixtures$irl_BTTSY, accuracy = 0.1)
IRL_fixtures$irl_BTTSN <- percent(IRL_fixtures$irl_BTTSN, accuracy = 0.1)
#odds
IRL_fixtures$irl_BTTSY_odds
IRL_fixtures$irl_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
IRL_fixtures$irl_AH_0_H <- (
  IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
    IRL_fixtures$irl_5_0 +IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5 + IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6
)
#AH_0_A
IRL_fixtures$irl_AH_0_A <- (
  IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
    IRL_fixtures$irl_0_5 +IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6 + IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6
)

#odds
IRL_fixtures$irl_AH_0_H_odds <- round((1/IRL_fixtures$irl_AH_0_H),digits = 2)
IRL_fixtures$irl_AH_0_A_odds <- round((1/IRL_fixtures$irl_AH_0_A),digits = 2)

IRL_fixtures$irl_AH_0_H_odds
IRL_fixtures$irl_AH_0_A_odds
#percentages
IRL_fixtures$irl_AH_0_H <- percent(IRL_fixtures$irl_AH_0_H, accuracy = 0.1)
IRL_fixtures$irl_AH_0_A <- percent(IRL_fixtures$irl_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
IRL_fixtures$irl_AH_n075_H <- (
  IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
    IRL_fixtures$irl_5_0 +IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5
)
#AH_n075_A
IRL_fixtures$irl_AH_n075_A <- (
  IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
    IRL_fixtures$irl_0_5 +IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6
)

#odds
IRL_fixtures$irl_AH_n075_H_odds <- round((1/IRL_fixtures$irl_AH_n075_H),digits = 2)
IRL_fixtures$irl_AH_n075_A_odds <- round((1/IRL_fixtures$irl_AH_n075_A),digits = 2)

IRL_fixtures$irl_AH_n075_H_odds
IRL_fixtures$irl_AH_n075_A_odds
#percentages
IRL_fixtures$irl_AH_n075_H <- percent(IRL_fixtures$irl_AH_n075_H, accuracy = 0.1)
IRL_fixtures$irl_AH_n075_A <- percent(IRL_fixtures$irl_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
IRL_fixtures$irl_AH_075_H <- (
  IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
    IRL_fixtures$irl_5_0 +IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5 + IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6 + IRL_fixtures$irl_0_1 + IRL_fixtures$irl_1_2 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_3_4 + IRL_fixtures$irl_4_5 + IRL_fixtures$irl_5_6
)
#AH_075_A
IRL_fixtures$irl_AH_075_A <- (
  IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
    IRL_fixtures$irl_0_5 +IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6 + IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6 + IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_3 + IRL_fixtures$irl_5_4 + IRL_fixtures$irl_6_5
)

#odds
IRL_fixtures$irl_AH_075_H_odds <- round((1/IRL_fixtures$irl_AH_075_H),digits = 2)
IRL_fixtures$irl_AH_075_A_odds <- round((1/IRL_fixtures$irl_AH_075_A),digits = 2)

IRL_fixtures$irl_AH_075_H_odds
IRL_fixtures$irl_AH_075_A_odds
#percentages
IRL_fixtures$irl_AH_075_H <- percent(IRL_fixtures$irl_AH_075_H, accuracy = 0.1)
IRL_fixtures$irl_AH_075_A <- percent(IRL_fixtures$irl_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
IRL_fixtures$irl_AH_n125_H <- (
  IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
    IRL_fixtures$irl_5_0 +IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5
)
#AH_n125_A
IRL_fixtures$irl_AH_n125_A <- (
  IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
    IRL_fixtures$irl_0_5 +IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6
)

#odds
IRL_fixtures$irl_AH_n125_H_odds <- round((1/IRL_fixtures$irl_AH_n125_H),digits = 2)
IRL_fixtures$irl_AH_n125_A_odds <- round((1/IRL_fixtures$irl_AH_n125_A),digits = 2)

IRL_fixtures$irl_AH_n125_H_odds
IRL_fixtures$irl_AH_n125_A_odds
#percentages
IRL_fixtures$irl_AH_n125_H <- percent(IRL_fixtures$irl_AH_n125_H, accuracy = 0.1)
IRL_fixtures$irl_AH_n125_A <- percent(IRL_fixtures$irl_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
IRL_fixtures$irl_AH_125_H <- (
  IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_0 + IRL_fixtures$irl_2_1 + IRL_fixtures$irl_3_0 + IRL_fixtures$irl_3_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_0 + IRL_fixtures$irl_4_1 + IRL_fixtures$irl_4_2 + IRL_fixtures$irl_4_3 +
    IRL_fixtures$irl_5_0 +IRL_fixtures$irl_5_1 + IRL_fixtures$irl_5_2 + IRL_fixtures$irl_5_3 + IRL_fixtures$irl_5_4 +
    IRL_fixtures$irl_6_0 + IRL_fixtures$irl_6_1 + IRL_fixtures$irl_6_2 + IRL_fixtures$irl_6_3 + IRL_fixtures$irl_6_4 +
    IRL_fixtures$irl_6_5 + IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6 + IRL_fixtures$irl_0_1 + IRL_fixtures$irl_1_2 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_3_4 + IRL_fixtures$irl_4_5 + IRL_fixtures$irl_5_6
)
#AH_125_A
IRL_fixtures$irl_AH_125_A <- (
  IRL_fixtures$irl_0_1 + IRL_fixtures$irl_0_2 + IRL_fixtures$irl_1_2 + IRL_fixtures$irl_0_3 + IRL_fixtures$irl_1_3 +
    IRL_fixtures$irl_2_3 + IRL_fixtures$irl_0_4 + IRL_fixtures$irl_1_4 + IRL_fixtures$irl_2_4 + IRL_fixtures$irl_3_4 +
    IRL_fixtures$irl_0_5 +IRL_fixtures$irl_1_5 + IRL_fixtures$irl_2_5 + IRL_fixtures$irl_3_5 + IRL_fixtures$irl_4_5 +
    IRL_fixtures$irl_0_6 + IRL_fixtures$irl_1_6 + IRL_fixtures$irl_2_6 + IRL_fixtures$irl_3_6 + IRL_fixtures$irl_4_6 +
    IRL_fixtures$irl_5_6 + IRL_fixtures$irl_0_0 + IRL_fixtures$irl_1_1 + IRL_fixtures$irl_2_2 + IRL_fixtures$irl_3_3 +
    IRL_fixtures$irl_4_4 + IRL_fixtures$irl_5_5 + IRL_fixtures$irl_6_6 + IRL_fixtures$irl_1_0 + IRL_fixtures$irl_2_1 +
    IRL_fixtures$irl_3_2 + IRL_fixtures$irl_4_3 + IRL_fixtures$irl_5_4 + IRL_fixtures$irl_6_5
)

#odds
IRL_fixtures$irl_AH_125_H_odds <- round((1/IRL_fixtures$irl_AH_125_H),digits = 2)
IRL_fixtures$irl_AH_125_A_odds <- round((1/IRL_fixtures$irl_AH_125_A),digits = 2)

IRL_fixtures$irl_AH_125_H_odds
IRL_fixtures$irl_AH_125_A_odds
#percentages
IRL_fixtures$irl_AH_125_H <- percent(IRL_fixtures$irl_AH_125_H, accuracy = 0.1)
IRL_fixtures$irl_AH_125_A <- percent(IRL_fixtures$irl_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
IRL_fixtures$irl_ov25 <- percent(IRL_fixtures$irl_ov25, accuracy = 0.1)

IRL_fixtures$irl_un25 <- percent(IRL_fixtures$irl_un25, accuracy = 0.1)
IRL_fixtures$irl_pscore <- paste(round(IRL_fixtures$irl_xGH,digits = 0),round(IRL_fixtures$irl_xGA,digits = 0),sep = "-")
#write out
write.xlsx(IRL_fixtures,'NL/IRL.xlsx',sheetName = "IRL", append = TRUE)
###########################################################################################################
########################IRL END###########################################################################
IRL <- read.csv('../FDAS/IRL.csv')
IRL$TG <- IRL$HG + IRL$AG
IRL$OV25 <- ifelse(IRL$TG >= 3,"Y","N")
irl_ftr_summary <- tabyl(IRL,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
irl_ov25_summary <- tabyl(IRL,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(irl_ftr_summary,'NL/IRL.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(irl_ov25_summary,'NL/IRL.xlsx',sheetName = "OVUN25", append = TRUE)



