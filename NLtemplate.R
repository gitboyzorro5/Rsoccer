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
unlink('NL/B1.xlsx')
######################B1 START#######################################
#####################################################################
B1 <- read.csv('../FDAS/B1.csv')
B1 <- within(B1,rm(Res))
B1$Date <- dmy(B1$Date)
B1 <- B1[order(as.Date(B1$Date, format = "%d/%m%Y"), decreasing = FALSE),]
B1$CS <- paste(B1$HG,B1$AG, sep = "-")

#B1_qualificaton <- subset(B1,tournament == "UEFA Euro qualification")
B1 <- subset(B1,Season == "2021/2022")
#B1 <- B1[B1$Date > '2008-01-01',])
B1$TG <- B1$HG + B1$AG
B1$OV25 <- ifelse(B1$TG >= 3,"Y","N")
B1$FTR <- with(B1,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
# B1 <- mgsub(B1,c("Wolfsberger"),c("Wolfsberger AC"))
# B1 <- mgsub(B1,c("Wolfsberger AC AC"),c("Wolfsberger AC"))
####GoalTotalsv2##################################
b1_totalgoalsv2 <- tapply(B1$TG, B1[c("Home", "Away")],mean)
b1_totalgoalsv2
b1_hgtotals <- rowSums(b1_totalgoalsv2,na.rm = T)
b1_agtotals <- colSums(b1_totalgoalsv2,na.rm = T)

b1_totalgoals <- b1_hgtotals + b1_agtotals
b1_totalgoalsv2 <- cbind(b1_totalgoalsv2,b1_totalgoals)
b1_teams <- sort(unique(B1$Home))
b1_home_games <- c()
b1_away_games <-c()
for (i_b1 in 1:length(b1_teams))
{

  b1_home_games[i_b1] <- nrow(B1[B1$Home == b1_teams[i_b1],])
  b1_away_games[i_b1]  <- nrow(B1[B1$Away == b1_teams[i_b1],])

}
b1_games_played <- b1_home_games + b1_away_games
b1_goaltotalsv2 <- cbind(b1_totalgoalsv2,b1_games_played)
b1_avg_totalgoals <- round((b1_totalgoals/ b1_games_played), digits = 4)
b1_goaltotalsv2[is.na(b1_goaltotalsv2)] <- ""
b1_goaltotalsv2 <- cbind(b1_goaltotalsv2,b1_avg_totalgoals)
write.xlsx(b1_goaltotalsv2,'NL/B1.xlsx',sheetName = "totalgoalsv2")
#####################################################################
#b1 goal scored rounds
#####################################################################
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
nrow(B1)
b1_goalscoredmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goalscoredround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoalscored <- B1_rounds$HG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoalscored <- B1_rounds$AG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_gs <- B1_rounds$Home[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_gs <- B1_rounds$Away[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalscombined <- c(b1_homegoalscored,b1_awaygoalscored)
  b1_teamscombined <- c(b1_hometeamstemp_gs,b1_awayteamstemp_gs)

  b1_goalscoredround <- data.frame(b1_teamscombined,b1_goalscombined)

  b1_goalscoredround <- b1_goalscoredround[order(b1_goalscoredround$b1_teamscombined),]
  b1_goalscoredround$b1_teamscombined <- NULL
  b1_goalscoredmatrix[,i_b1_krounds] <- b1_goalscoredround

}

b1_goalscoredmatrix <- cbind(b1_teams,b1_goalscoredmatrix)
####GSmatrix################################
#create home and away matrices
b1_goalscored_h <- tapply(B1$HG, B1[c("Home", "Date")],mean)
b1_goalscored_a <- tapply(B1$AG, B1[c("Away", "Date")],mean)
b1_goalscored_h[is.na(b1_goalscored_h)] <- ""
b1_goalscored_a[is.na(b1_goalscored_a)] <- ""

for(b1_rowhgs in 1:nrow(b1_goalscored_h)) {
  for(b1_colhgs in 1:ncol(b1_goalscored_h)) {

    # print(my_matrix[row, col])
    for(b1_rowags in 1:nrow(b1_goalscored_a)) {
      for(b1_colags in 1:ncol(b1_goalscored_a)) {
        ifelse(!b1_goalscored_a[b1_rowags,b1_colags]=="",b1_goalscored_h[b1_rowags,b1_colags] <- b1_goalscored_a[b1_rowags,b1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(b1_goalscoredmatrix,'NL/B1.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#b1 goal conceded rounds
#b1
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goalconcededmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goalconcededround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoalconceded <- B1_rounds$AG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoalconceded <- B1_rounds$HG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_gc <- B1_rounds$Home[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_gc <- B1_rounds$Away[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalsconcededcombined <- c(b1_homegoalconceded,b1_awaygoalconceded)
  b1_teamscombined_gc <- c(b1_hometeamstemp_gc,b1_awayteamstemp_gc)

  b1_goalconcededround <- data.frame(b1_teamscombined_gc,b1_goalsconcededcombined)

  b1_goalconcededround <- b1_goalconcededround[order(b1_goalconcededround$b1_teamscombined_gc),]
  b1_goalconcededround$b1_teamscombined_gc <- NULL
  b1_goalconcededmatrix[,i_b1_krounds] <- b1_goalconcededround

}

b1_goalconcededmatrix <- cbind(b1_teams,b1_goalconcededmatrix)

####GCmatrix#############################################################################
#create home and away matrices
b1_goalconceded_h <- tapply(B1$AG, B1[c("Home", "Date")],mean)
b1_goalconceded_a <- tapply(B1$HG, B1[c("Away", "Date")],mean)
b1_goalconceded_h[is.na(b1_goalconceded_h)] <- ""
b1_goalconceded_a[is.na(b1_goalconceded_a)] <- ""

for(b1_rowhgc in 1:nrow(b1_goalconceded_h)) {
  for(b1_colhgc in 1:ncol(b1_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(b1_rowagc in 1:nrow(b1_goalconceded_a)) {
      for(b1_colagc in 1:ncol(b1_goalconceded_a)) {
        ifelse(!b1_goalconceded_a[b1_rowagc,b1_colagc]=="",b1_goalconceded_h[b1_rowagc,b1_colagc] <- b1_goalconceded_a[b1_rowagc,b1_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(b1_goalconcededmatrix,'NL/B1.xlsx',sheetName = "gcmatrix", append = TRUE)
########################################################################################
#b1 team form
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_formmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_formround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homeform <- B1_rounds$Res[B1_rounds$b1_matchday == i_b1_krounds]

  b1_homeform <- sub("H","W",b1_homeform)
  b1_homeform <- sub("A","L",b1_homeform)

  b1_awayform <- B1_rounds$Res[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayform <- sub("A","W",b1_awayform)
  b1_awayform <- sub("H","L",b1_awayform)

  b1_hometeamstemp_form <- B1_rounds$Home[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_form <- B1_rounds$Away[B1_rounds$b1_matchday== i_b1_krounds]

  b1_formcombined <- c(b1_homeform,b1_awayform)
  b1_teamscombined_form <- c(b1_hometeamstemp_form,b1_awayteamstemp_form)

  b1_formround <- data.frame(b1_teamscombined_form,b1_formcombined)

  b1_formround <- b1_formround[order(b1_formround$b1_teamscombined_form),]
  b1_formround$b1_teamscombined_form <- NULL
  b1_formmatrix[,i_b1_krounds] <- b1_formround

}

b1_formmatrix <- cbind(b1_teams,b1_formmatrix)
########################################################################################
########################################################################################
#########################################################################################
####Teamform#############################################################################

b1_form_h <- tapply(B1$FTR, B1[c("Home", "Date")],median)
b1_form_a <- tapply(B1$FTR, B1[c("Away", "Date")],median)
b1_form_h[is.na(b1_form_h)] <- ""
b1_form_a[is.na(b1_form_a)] <- ""
b1_form_h <- sub("A","L",b1_form_h)
b1_form_h <- sub("H","W",b1_form_h)
b1_form_a <- sub("A","W",b1_form_a)
b1_form_a <- sub("H","L",b1_form_a)
for(b1_rowh_f in 1:nrow(b1_form_h)) {
  for(b1_colh_f in 1:ncol(b1_form_h)) {

    # print(my_matrix[row, col])
    for(b1_rowa_f in 1:nrow(b1_form_a)) {
      for(b1_cola_f in 1:ncol(b1_form_a)) {
        ifelse(!b1_form_a[b1_rowa_f,b1_cola_f]=="",b1_form_h[b1_rowa_f,b1_cola_f] <- b1_form_a[b1_rowa_f,b1_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(b1_formmatrix,'NL/B1.xlsx',sheetName = "form", append = TRUE)
##################################################################################
##################################################################################
#b1 total goals rounds
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goaltotalmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goaltotalround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoaltotal <- B1_rounds$TG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoaltotal <- B1_rounds$TG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_tg <- B1_rounds$Home[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_tg <- B1_rounds$Away[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalscombined_tg <- c(b1_homegoaltotal,b1_awaygoaltotal)
  b1_teamscombined_tg <- c(b1_hometeamstemp_tg,b1_awayteamstemp_tg)

  b1_goaltotalround <- data.frame(b1_teamscombined_tg,b1_goalscombined_tg)

  b1_goaltotalround <- b1_goaltotalround[order(b1_goaltotalround$b1_teamscombined_tg),]
  b1_goaltotalround$b1_teamscombined_tg <- NULL
  b1_goaltotalmatrix[,i_b1_krounds] <- b1_goaltotalround

}

b1_goaltotalmatrix <- cbind(b1_teams,b1_goaltotalmatrix)
##############################################################################################
#d1
#######TGMatrix##################################################################
b1_totalgoals_h <- tapply(B1$TG, B1[c("Home", "Date")],mean)
b1_totalgoals_a <- tapply(B1$TG, B1[c("Away", "Date")],mean)
b1_totalgoals_h[is.na(b1_totalgoals_h)] <- ""
b1_totalgoals_a[is.na(b1_totalgoals_a)] <- ""
for(b1_rowh in 1:nrow(b1_totalgoals_h)) {
  for(b1_colh in 1:ncol(b1_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(b1_rowa in 1:nrow(b1_totalgoals_a)) {
      for(b1_cola in 1:ncol(b1_totalgoals_a)) {
        ifelse(!b1_totalgoals_a[b1_rowa,b1_cola]=="",b1_totalgoals_h[b1_rowa,b1_cola] <- b1_totalgoals_a[b1_rowa,b1_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(b1_goaltotalmatrix,'NL/B1.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
b1_form_team_against_h <- tapply(B1$Away, B1[c("Home", "Date")],median)
b1_form_team_against_a <- tapply(B1$Home, B1[c("Away", "Date")],median)
b1_form_team_against_h[is.na(b1_form_team_against_h)] <- ""
b1_form_team_against_a[is.na(b1_form_team_against_a)] <- ""
for(b1_rowh_f_against in 1:nrow(b1_form_team_against_h)) {
  for(b1_colh_f_against in 1:ncol(b1_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(b1_rowa_f_against in 1:nrow(b1_form_team_against_a)) {
      for(b1_cola_f_against in 1:ncol(b1_form_team_against_a)) {
        ifelse(!b1_form_team_against_a[b1_rowa_f_against,b1_cola_f_against]=="",b1_form_team_against_h[b1_rowa_f_against,b1_cola_f_against] <- b1_form_team_against_a[b1_rowa_f_against,b1_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
#win margin
b1_winmargin_h <- tapply(B1$HG - B1$AG, B1[c("Home", "Date")],mean)
b1_winmargin_a <- tapply(B1$AG - B1$HG, B1[c("Away", "Date")],mean)
b1_winmargin_h[is.na(b1_winmargin_h)] <- ""
#
for(b1_rowhwm in 1:nrow(b1_winmargin_h)) {
  for(b1_colhwm in 1:ncol(b1_winmargin_h)) {

    # print(my_matrix[row, col])
    for(b1_rowawm in 1:nrow(b1_winmargin_a)) {
      for(b1_colawm in 1:ncol(b1_winmargin_a)) {
        ifelse(!b1_winmargin_a[b1_rowawm,b1_colawm]=="",b1_winmargin_h[b1_rowawm,b1_colawm] <- b1_winmargin_a[b1_rowawm,b1_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
####################################################################################################################
##########Goals over under############
#B1
b1_un05_home <- c()
b1_un05_away <- c()
b1_ov05_home <- c()
b1_ov05_away <- c()

b1_un15_home <- c()
b1_un15_away <- c()
b1_ov15_home <- c()
b1_ov15_away <- c()

b1_un25_home <- c()
b1_un25_away <- c()
b1_ov25_home <- c()
b1_ov25_away <- c()

b1_un35_home <- c()
b1_un35_away <- c()
b1_ov35_home <- c()
b1_ov35_away <- c()

b1_un45_home <- c()
b1_un45_away <- c()
b1_ov45_home <- c()
b1_ov45_away <- c()

b1_un55_home <- c()
b1_un55_away <- c()
b1_ov55_home <- c()
b1_ov55_away <- c()

for (i_b1_tg in 1:length(b1_teams))
{

  b1_un05_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG == 0,])
  b1_un05_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG == 0,])

  b1_ov05_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG > 0,])
  b1_ov05_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG > 0,])

  b1_un15_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG <= 1,])
  b1_un15_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG <= 1,])

  b1_ov15_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG >= 2,])
  b1_ov15_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG >= 2,])

  b1_un25_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG <= 2,])
  b1_un25_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG <= 2,])

  b1_ov25_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG >=3,])
  b1_ov25_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG >=3,])

  b1_un35_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG <= 3,])
  b1_un35_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG <= 3,])

  b1_ov35_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG >= 4,])
  b1_ov35_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG >= 4,])

  b1_un45_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG <= 4,])
  b1_un45_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG <= 4,])

  b1_ov45_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG >= 5,])
  b1_ov45_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG >= 5,])

  b1_un55_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG <= 5,])
  b1_un55_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG <= 5,])

  b1_ov55_home[i_b1_tg] <- nrow(B1[B1$Home == b1_teams[i_b1_tg] & B1$TG >= 6,])
  b1_ov55_away[i_b1_tg] <- nrow(B1[B1$Away == b1_teams[i_b1_tg] & B1$TG >= 6,])


}

b1_un05 <- b1_un05_home + b1_un05_away
b1_ov05 <- b1_ov05_home + b1_ov05_away

b1_un15 <- b1_un15_home + b1_un15_away
b1_ov15 <- b1_ov15_home + b1_ov15_away

b1_un25 <- b1_un25_home + b1_un25_away
b1_ov25 <- b1_ov25_home + b1_ov25_away

b1_un35 <- b1_un35_home + b1_un35_away
b1_ov35 <- b1_ov35_home + b1_ov35_away

b1_un45 <- b1_un45_home + b1_un45_away
b1_ov45 <- b1_ov45_home + b1_ov45_away

b1_un55 <- b1_un55_home + b1_un55_away
b1_ov55 <- b1_ov55_home + b1_ov55_away

b1_ovundata <- cbind(b1_teams,b1_un05,b1_ov05,b1_un15,b1_ov15,b1_un25,b1_ov25,b1_un35,b1_ov35,b1_un45,b1_ov45,b1_un55,b1_ov55)
write.xlsx(b1_ovundata,'NL/B1.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
b1_csform_h <- tapply(B1$CS, B1[c("Home", "Date")],median)
b1_csform_a <- tapply(B1$CS, B1[c("Away", "Date")],median)

b1_csform_h[is.na(b1_csform_h)] <- ""
b1_csform_a[is.na(b1_csform_a)] <- ""

for(b1_rowh_f_cs in 1:nrow(b1_csform_h)) {
  for(b1_colh_f_cs in 1:ncol(b1_csform_h)) {

    # print(my_matrix[row, col])
    for(b1_rowa_f_cs in 1:nrow(b1_csform_a)) {
      for(b1_cola_f_cs in 1:ncol(b1_csform_a)) {
        ifelse(!b1_csform_a[b1_rowa_f_cs,b1_cola_f_cs]=="",b1_csform_h[b1_rowa_f_cs,b1_cola_f_cs] <- b1_csform_a[b1_rowa_f_cs,b1_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
b1_home_gs <- aggregate(B1$HG, by = list(B1$Home), FUN = sum)
b1_home_gs_avg <- aggregate(B1$HG, by = list(B1$Home),mean)
b1_home_scoring <- merge(b1_home_gs,b1_home_gs_avg, by='Group.1',all = T)
names(b1_home_scoring)[names(b1_home_scoring) == "x.x"] <- "TFthg"
names(b1_home_scoring)[names(b1_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
b1_away_gs <- aggregate(B1$AG, by = list(B1$Away), FUN = sum)
b1_away_gs_avg <- aggregate(B1$AG, by = list(B1$Away),mean)
b1_away_scoring <- merge(b1_away_gs,b1_away_gs_avg, by='Group.1',all = T)
names(b1_away_scoring)[names(b1_away_scoring) == "x.x"] <- "TFtag"
names(b1_away_scoring)[names(b1_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
b1_scoring <- merge(b1_home_scoring,b1_away_scoring,by='Group.1',all = T)
b1_scoring$TGS <- b1_scoring$TFthg + b1_scoring$TFtag

#home goals conceded
b1_home_gc <- aggregate(B1$AG, by = list(B1$Home), FUN = sum)
b1_home_gc_avg <- aggregate(B1$AG, by = list(B1$Home),mean)
b1_home_conceding <- merge(b1_home_gc,b1_home_gc_avg, by='Group.1',all = T)
names(b1_home_conceding)[names(b1_home_conceding) == "x.x"] <- "TFthc"
names(b1_home_conceding)[names(b1_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
b1_away_gc <- aggregate(B1$HG, by = list(B1$Away), FUN = sum)
b1_away_gc_avg <- aggregate(B1$HG, by = list(B1$Away),mean)
b1_away_conceding <- merge(b1_away_gc,b1_away_gc_avg, by='Group.1',all = T)
names(b1_away_conceding)[names(b1_away_conceding) == "x.x"] <- "TFtac"
names(b1_away_conceding)[names(b1_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
b1_conceding <- merge(b1_home_conceding,b1_away_conceding,by='Group.1',all = T)
b1_conceding$TGC <- b1_conceding$TFthc + b1_conceding$TFtac

######################################################################################
###########League Table###############################################################

#hwins and away wins
b1_home_wins <- c()
b1_away_wins <- c()
b1_home_draws <- c()
b1_away_draws <- c()
b1_home_loss <- c()
b1_away_loss <- c()



for (i_b1_wins in 1:length(b1_teams))
{

  b1_home_wins[i_b1_wins] <- nrow(B1[B1$Home == b1_teams[i_b1_wins] & B1$FTR == "H",])
  b1_away_wins[i_b1_wins] <- nrow(B1[B1$Away == b1_teams[i_b1_wins] & B1$FTR == "A",])
  b1_home_draws[i_b1_wins] <- nrow(B1[B1$Home == b1_teams[i_b1_wins] & B1$FTR == "D",])
  b1_away_draws[i_b1_wins] <- nrow(B1[B1$Away == b1_teams[i_b1_wins] & B1$FTR == "D",])
  b1_home_loss[i_b1_wins] <- nrow(B1[B1$Home == b1_teams[i_b1_wins] & B1$FTR == "A",])
  b1_away_loss[i_b1_wins] <- nrow(B1[B1$Away == b1_teams[i_b1_wins] & B1$FTR == "H",])

}

b1_total_wins <- b1_home_wins + b1_away_wins
b1_total_draws <- b1_home_draws + b1_away_draws
b1_total_loss <- b1_home_loss + b1_away_loss

b1_league_table <- cbind(b1_teams,b1_games_played,b1_total_wins,b1_total_draws,b1_total_loss)
b1_GS <- b1_scoring$TGS
b1_GC <-b1_conceding$TGC
b1_GD <- b1_scoring$TGS - b1_conceding$TGC
b1_PTS <- (b1_total_wins*3) + (b1_total_draws*1)
b1_league_table <- cbind(b1_league_table,b1_GS,b1_GC,b1_GD,b1_PTS)
b1_league_table <- as.data.frame(b1_league_table)
#rename the columns
names(b1_league_table)[names(b1_league_table) == "b1_teams"] <- "Team"
names(b1_league_table)[names(b1_league_table) == "b1_games_played"] <- "P"
names(b1_league_table)[names(b1_league_table) == "b1_total_wins"] <- "W"
names(b1_league_table)[names(b1_league_table) == "b1_total_draws"] <- "D"
names(b1_league_table)[names(b1_league_table) == "b1_total_loss"] <- "L"
names(b1_league_table)[names(b1_league_table) == "b1_GS"] <- "F"
names(b1_league_table)[names(b1_league_table) == "b1_GC"] <- "A"
points_b1 <- b1_league_table[order(as.numeric(b1_league_table$b1_PTS), decreasing = TRUE),]
points_b1$b1_rank <- 1:length(b1_teams)
row.names(points_b1) <- points_b1$b1_rank
#create final_b1_hf_against with team ranks in brackets
for(b1_rowhrank in 1:nrow(b1_form_team_against_h)) {
  for(b1_colhrank in 1:ncol(b1_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!b1_form_team_against_h[b1_rowhrank,b1_colhrank]=="",b1_form_team_against_h[b1_rowhrank,b1_colhrank] <- paste(b1_form_team_against_h[b1_rowhrank,b1_colhrank],"(",points_b1$b1_rank[points_b1$Team ==b1_form_team_against_h[b1_rowhrank,b1_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_b1,'NL/B1.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six b1###################################################
#B1
#form
#create final_b1_hf object
#b1_last_n_games <- 6
final_b1_hf <- c()
for(index_b1_hf in 1:length(b1_teams))
{
  index_b1_hf <- row.names(b1_form_h) == b1_teams[index_b1_hf]
  form_b1_hf <- b1_form_h[index_b1_hf]
  deleted_form_b1_hf <- form_b1_hf[!form_b1_hf[] == ""]
  l6_form_b1_hf <- tail(deleted_form_b1_hf,b1_last_n_games)
  l6_form_b1_hf <- paste(l6_form_b1_hf,collapse = " ")
  final_b1_hf[index_b1_hf] <- rbind(paste(b1_teams[index_b1_hf],l6_form_b1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}

#change column names
final_b1_hf <- as.data.frame(final_b1_hf)
colnames(final_b1_hf) <- "Form"
#goals scored
#create final_b1_gs object
final_b1_gs <- c()
suml6_b1_gs <- c()
for(index_b1_gs in 1:length(b1_teams))
{
  index_b1_gs <- row.names(b1_goalscored_h) == b1_teams[index_b1_gs]
  form_b1_gs <- b1_goalscored_h[index_b1_gs]
  deleted_form_b1_gs <- form_b1_gs[!form_b1_gs[] == ""]
  l6_form_b1_gs <- tail(deleted_form_b1_gs,b1_last_n_games)
  l6_form_b1_gs <- as.numeric(l6_form_b1_gs)
  suml6_b1_gs[index_b1_gs] <- sum(l6_form_b1_gs)
  suml6_b1_gs[index_b1_gs] <- paste("(",suml6_b1_gs[index_b1_gs],")",sep = "")
  l6_form_b1_gs <- paste(l6_form_b1_gs,collapse = " ")
  final_b1_gs[index_b1_gs] <- rbind(paste(b1_teams[index_b1_gs],l6_form_b1_gs,suml6_b1_gs[index_b1_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
final_b1_gs
#change column names
final_b1_gs <- as.data.frame(final_b1_gs)
colnames(final_b1_gs) <- "Goals scored"
#goal conceded
#create final_b1_gc object
final_b1_gc <- c()
suml6_b1_gc <- c()
for(index_b1_gc in 1:length(b1_teams))
{
  index_b1_gc <- row.names(b1_goalconceded_h) == b1_teams[index_b1_gc]
  form_b1_gc <- b1_goalconceded_h[index_b1_gc]
  deleted_form_b1_gc <- form_b1_gc[!form_b1_gc[] == ""]
  l6_form_b1_gc <- tail(deleted_form_b1_gc,b1_last_n_games)
  l6_form_b1_gc <- as.numeric(l6_form_b1_gc)
  suml6_b1_gc[index_b1_gc] <- sum(l6_form_b1_gc)
  suml6_b1_gc[index_b1_gc] <- paste("(",suml6_b1_gc[index_b1_gc],")",sep = "")
  l6_form_b1_gc <- paste(l6_form_b1_gc,collapse = " ")
  final_b1_gc[index_b1_gc] <- rbind(paste(b1_teams[index_b1_gc],l6_form_b1_gc,suml6_b1_gc[index_b1_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}

#change column names
final_b1_gc <- as.data.frame(final_b1_gc)
colnames(final_b1_gc) <- "Goals conceded"
#total goals
#create final_b1_tg object
final_b1_tg <- c()
suml6_b1_tg <- c()
for(index_b1_tg in 1:length(b1_teams))
{
  index_b1_tg <- row.names(b1_totalgoals_h) == b1_teams[index_b1_tg]
  form_b1_tg <- b1_totalgoals_h[index_b1_tg]
  deleted_form_b1_tg <- form_b1_tg[!form_b1_tg[] == ""]
  l6_form_b1_tg <- tail(deleted_form_b1_tg,b1_last_n_games)
  l6_form_b1_tg <- as.numeric(l6_form_b1_tg)
  suml6_b1_tg[index_b1_tg] <- sum(l6_form_b1_tg)
  suml6_b1_tg[index_b1_tg] <- paste("(",suml6_b1_tg[index_b1_tg],")",sep = "")
  l6_form_b1_tg <- paste(l6_form_b1_tg,collapse = " ")
  final_b1_tg[index_b1_tg] <- rbind(paste(b1_teams[index_b1_tg],l6_form_b1_tg,suml6_b1_tg[index_b1_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
#change column names
final_b1_tg <- as.data.frame(final_b1_tg)
colnames(final_b1_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_b1_hf object
final_b1_cs <- c()
for(index_b1_cs in 1:length(b1_teams))
{
  index_b1_cs <- row.names(b1_csform_h) == b1_teams[index_b1_cs]
  csform_b1_cs <- b1_csform_h[index_b1_cs]
  deleted_csform_b1_cs <- csform_b1_cs[!csform_b1_cs[] == ""]
  l6_csform_b1_cs <- tail(deleted_csform_b1_cs,b1_last_n_games)
  l6_csform_b1_cs <- paste(l6_csform_b1_cs,collapse = " ")
  final_b1_cs[index_b1_cs] <- rbind(paste(b1_teams[index_b1_cs],l6_csform_b1_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",b1_teams[index],l6_csform)

}

#change column names
final_b1_cs <- as.data.frame(final_b1_cs)
colnames(final_b1_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_b1_wm object
final_b1_wm <- c()
suml6_b1_wm <- c()
for(index_b1_wm in 1:length(b1_teams))
{
  index_b1_wm <- row.names(b1_winmargin_h) == b1_teams[index_b1_wm]
  form_b1_wm <- b1_winmargin_h[index_b1_wm]
  deleted_form_b1_wm <- form_b1_wm[!form_b1_wm[] == ""]
  l6_form_b1_wm <- tail(deleted_form_b1_wm,b1_last_n_games)
  l6_form_b1_wm <- as.numeric(l6_form_b1_wm)
  suml6_b1_wm[index_b1_wm] <- sum(l6_form_b1_wm)
  suml6_b1_wm[index_b1_wm] <- paste("(",suml6_b1_wm[index_b1_wm],")",sep = "")
  l6_form_b1_wm <- paste(l6_form_b1_wm,collapse = " ")
  final_b1_wm[index_b1_wm] <- rbind(paste(b1_teams[index_b1_wm],l6_form_b1_wm,suml6_b1_wm[index_b1_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
final_b1_wm
#change column names
final_b1_wm <- as.data.frame(final_b1_wm)
colnames(final_b1_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_b1_hf_against
final_b1_hf_against <- c()
for(index_b1_hf_against in 1:length(b1_teams))
{
  index_b1_hf_against <- row.names(b1_form_team_against_h) == b1_teams[index_b1_hf_against]
  form_b1_hf_against <- b1_form_team_against_h[index_b1_hf_against]
  deleted_form_b1_hf_against <- form_b1_hf_against[!form_b1_hf_against[] == ""]
  l6_form_b1_hf_against <- tail(deleted_form_b1_hf_against,b1_last_n_games)
  l6_form_b1_hf_against <- paste(l6_form_b1_hf_against,collapse = " ")
  final_b1_hf_against[index_b1_hf_against] <- rbind(paste(b1_teams[index_b1_hf_against],l6_form_b1_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
final_b1_hf_against <- as.data.frame(final_b1_hf_against)
colnames(final_b1_hf_against) <- "Team against"
#combine the columns
final_b1_all <- cbind(final_b1_hf,final_b1_gs,final_b1_gc,final_b1_tg,final_b1_cs,final_b1_wm,final_b1_hf_against)
write.xlsx(final_b1_all,'NL/B1.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
b1_GP <- nrow(B1)
#Calculate total home goals for each division
b1_T_HG <- sum(b1_home_gs$x)
#calculate average home goal
b1_avg_HG <- round(b1_T_HG /b1_GP, digits = 4)
############################################################
#Calculate total away goals for each division
b1_T_AG <- sum(b1_away_gs$x)
#calculate average away goal
b1_avg_AG <- round(b1_T_AG /b1_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
b1_home_as <- round(((b1_home_gs$x/b1_home_games))/b1_avg_HG, digits = 4)
#calculate away attack strength
b1_away_as <- round(((b1_away_gs$x/b1_away_games))/b1_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
b1_avg_HC <- round(b1_T_AG /b1_GP, digits = 4)
#avg away concede
b1_avg_AC <- round(b1_T_HG /b1_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
b1_home_ds <- round(((b1_home_gc$x/b1_home_games))/b1_avg_HC, digits = 4)
#away defense strength
b1_away_ds <- round(((b1_away_gc$x/b1_away_games))/b1_avg_AC, digits = 4)
#############################################################################
#home poisson data
#b1
b1_division <- c()
b1_division[1:length(b1_teams)] <- "B1"
b1_home_poisson <- cbind(b1_division,b1_teams,b1_avg_HG,b1_home_as,b1_home_ds)
#################################################################################
#away poisson data
#b1
b1_division <- c()
b1_division[1:length(b1_teams)] <- "B1"
b1_away_poisson <- cbind(b1_division,b1_teams,b1_avg_AG,b1_away_as,b1_away_ds)

#create home and away csv
#b1_home_poisson <- rbind(b1_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#b1_away_poisson <- rbind(b1_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(b1_home_poisson,'NL/B1.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(b1_away_poisson,'NL/B1.xlsx',sheetName = "awaypoisson", append = TRUE)
b1_home_poisson
b1_away_poisson
##########################################################################################################
###################B1 FIXTURES##########################################################################
#B1
HomeTeam_b1 <- rep(b1_teams, each = length(b1_teams))
AwayTeam_b1 <- rep(b1_teams, length(b1_teams))
B1_fixtures <- cbind(HomeTeam_b1,AwayTeam_b1)
B1_fixtures <- as.data.frame(B1_fixtures)
B1_fixtures <- B1_fixtures[!B1_fixtures$HomeTeam_b1 == B1_fixtures$AwayTeam_b1,]
rownames(B1_fixtures) <- NULL
B1_fixtures$Div <- "B1"
B1_fixtures <- B1_fixtures[,c(3,1,2)]

B1_fixtures$avg_HG_b1 <- b1_avg_HG

B1_fixtures$b1_homeas <- rep(b1_home_as,each = length(b1_teams)-1)

b1_awayds_lookup <- cbind(b1_teams,b1_away_ds)

b1_awayds_lookup <- as.data.frame(b1_awayds_lookup)

colnames(b1_awayds_lookup) <- c("AwayTeam_b1","b1_awayds")


require('RH2')
B1_fixtures$b1_awayds <- sqldf("SELECT b1_awayds_lookup.b1_awayds FROM b1_awayds_lookup INNER JOIN B1_fixtures ON b1_awayds_lookup.AwayTeam_b1 = B1_fixtures.AwayTeam_b1")

B1_fixtures$avg_AG_b1 <- b1_avg_AG

b1_awayas_lookup <- cbind(b1_teams,b1_away_as)

b1_awayas_lookup <- as.data.frame(b1_awayas_lookup)

colnames(b1_awayas_lookup) <- c("AwayTeam_b1","b1_awayas")


B1_fixtures$b1_awayas <- sqldf("SELECT b1_awayas_lookup.b1_awayas FROM b1_awayas_lookup INNER JOIN B1_fixtures ON b1_awayas_lookup.AwayTeam_b1 = B1_fixtures.AwayTeam_b1")

B1_fixtures$b1_homeds <- rep(b1_home_ds,each = length(b1_teams)-1)

B1_fixtures$b1_awayds <- as.numeric(unlist(B1_fixtures$b1_awayds))
#xGH
B1_fixtures$b1_xGH <- B1_fixtures$avg_HG_b1 * B1_fixtures$b1_homeas * B1_fixtures$b1_awayds

#xGA

B1_fixtures$b1_awayas <- as.numeric(unlist(B1_fixtures$b1_awayas))

B1_fixtures$b1_xGA <- B1_fixtures$avg_AG_b1 * B1_fixtures$b1_awayas * B1_fixtures$b1_homeds

B1_fixtures$b1_0_0 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_0 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_1 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_1 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_0 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_2 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_2 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_1 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_2 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_3 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_0 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_1 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_2 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_3 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_3 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_3 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_4 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_0 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_1 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_2 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_3 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_4 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_4 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_4 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_4 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_5 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_0 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_1 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_2 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_3 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_4 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_5 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_5 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_5 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_5 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_5 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_6 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_0 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_1 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_2 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_3 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_4 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_5 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_6 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_6 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_6 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_6 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_6 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_6 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
#Home win
B1_fixtures$b1_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
    B1_fixtures$b1_5_0 + B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5
)

B1_fixtures$b1_H <- percent(B1_fixtures$b1_H, accuracy = 0.1)

#Draw
B1_fixtures$b1_D <- (

  B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 + B1_fixtures$b1_4_4 +
    B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6
)

B1_fixtures$b1_D <- percent(B1_fixtures$b1_D, accuracy = 0.1)

#Away

B1_fixtures$b1_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 + B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6
)

B1_fixtures$b1_A <- percent(B1_fixtures$b1_A, accuracy = 0.1)

#ov25
B1_fixtures$b1_ov25 <- (
  B1_fixtures$b1_2_1 + B1_fixtures$b1_1_2 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 + B1_fixtures$b1_2_3 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 + B1_fixtures$b1_0_4 +
    B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 + B1_fixtures$b1_4_4 + B1_fixtures$b1_5_0 +
    B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 + B1_fixtures$b1_0_5 +
    B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 + B1_fixtures$b1_5_5 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5 + B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 +
    B1_fixtures$b1_4_6 + B1_fixtures$b1_5_6 + B1_fixtures$b1_6_6
)
#un25
B1_fixtures$b1_un25 <- (
  B1_fixtures$b1_0_0 + B1_fixtures$b1_1_0 + B1_fixtures$b1_0_1 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_0 + B1_fixtures$b1_0_2
)
#odds
B1_fixtures$b1_ov25_odds <- round((1/B1_fixtures$b1_ov25),digits = 2)
B1_fixtures$b1_un25_odds <- round((1/B1_fixtures$b1_un25),digits = 2)

B1_fixtures$b1_ov25_odds
B1_fixtures$b1_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
B1_fixtures$b1_BTTSY <- (
  B1_fixtures$b1_1_1 + B1_fixtures$b1_2_1 + B1_fixtures$b1_1_2 + B1_fixtures$b1_3_1 + B1_fixtures$b1_3_2 +
    B1_fixtures$b1_2_2 + B1_fixtures$b1_1_3 + B1_fixtures$b1_2_3 + B1_fixtures$b1_3_3 + B1_fixtures$b1_4_4 +
    B1_fixtures$b1_4_1 + B1_fixtures$b1_4_3 + B1_fixtures$b1_4_2 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 +
    B1_fixtures$b1_3_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 +
    B1_fixtures$b1_5_4 + B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_6_6 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6
)
#BTTSN
B1_fixtures$b1_BTTSN <- (
  B1_fixtures$b1_0_0 + B1_fixtures$b1_1_0 + B1_fixtures$b1_0_1 + B1_fixtures$b1_2_0 + B1_fixtures$b1_0_2 +
    B1_fixtures$b1_3_0 + B1_fixtures$b1_0_3 + B1_fixtures$b1_4_0 + B1_fixtures$b1_0_4 + B1_fixtures$b1_5_0 +
    B1_fixtures$b1_0_5 + B1_fixtures$b1_6_0 + B1_fixtures$b1_0_6
)

B1_fixtures$b1_BTTSY_odds <- round((1/B1_fixtures$b1_BTTSY),digits = 2)
B1_fixtures$b1_BTTSN_odds <- round((1/B1_fixtures$b1_BTTSN),digits = 2)

B1_fixtures$b1_BTTSY <- percent(B1_fixtures$b1_BTTSY, accuracy = 0.1)
B1_fixtures$b1_BTTSN <- percent(B1_fixtures$b1_BTTSN, accuracy = 0.1)
#odds
B1_fixtures$b1_BTTSY_odds
B1_fixtures$b1_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
B1_fixtures$b1_AH_0_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
    B1_fixtures$b1_5_0 +B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5 + B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6
)
#AH_0_A
B1_fixtures$b1_AH_0_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 +B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6 + B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6
)

#odds
B1_fixtures$b1_AH_0_H_odds <- round((1/B1_fixtures$b1_AH_0_H),digits = 2)
B1_fixtures$b1_AH_0_A_odds <- round((1/B1_fixtures$b1_AH_0_A),digits = 2)

B1_fixtures$b1_AH_0_H_odds
B1_fixtures$b1_AH_0_A_odds
#percentages
B1_fixtures$b1_AH_0_H <- percent(B1_fixtures$b1_AH_0_H, accuracy = 0.1)
B1_fixtures$b1_AH_0_A <- percent(B1_fixtures$b1_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
B1_fixtures$b1_AH_n075_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
    B1_fixtures$b1_5_0 +B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5
)
#AH_n075_A
B1_fixtures$b1_AH_n075_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 +B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6
)

#odds
B1_fixtures$b1_AH_n075_H_odds <- round((1/B1_fixtures$b1_AH_n075_H),digits = 2)
B1_fixtures$b1_AH_n075_A_odds <- round((1/B1_fixtures$b1_AH_n075_A),digits = 2)

B1_fixtures$b1_AH_n075_H_odds
B1_fixtures$b1_AH_n075_A_odds
#percentages
B1_fixtures$b1_AH_n075_H <- percent(B1_fixtures$b1_AH_n075_H, accuracy = 0.1)
B1_fixtures$b1_AH_n075_A <- percent(B1_fixtures$b1_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
B1_fixtures$b1_AH_075_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
    B1_fixtures$b1_5_0 +B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5 + B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6 + B1_fixtures$b1_0_1 + B1_fixtures$b1_1_2 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_3_4 + B1_fixtures$b1_4_5 + B1_fixtures$b1_5_6
)
#AH_075_A
B1_fixtures$b1_AH_075_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 +B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6 + B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6 + B1_fixtures$b1_1_0 + B1_fixtures$b1_2_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_3 + B1_fixtures$b1_5_4 + B1_fixtures$b1_6_5
)

#odds
B1_fixtures$b1_AH_075_H_odds <- round((1/B1_fixtures$b1_AH_075_H),digits = 2)
B1_fixtures$b1_AH_075_A_odds <- round((1/B1_fixtures$b1_AH_075_A),digits = 2)

B1_fixtures$b1_AH_075_H_odds
B1_fixtures$b1_AH_075_A_odds
#percentages
B1_fixtures$b1_AH_075_H <- percent(B1_fixtures$b1_AH_075_H, accuracy = 0.1)
B1_fixtures$b1_AH_075_A <- percent(B1_fixtures$b1_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
B1_fixtures$b1_AH_n125_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
    B1_fixtures$b1_5_0 +B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5
)
#AH_n125_A
B1_fixtures$b1_AH_n125_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 +B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6
)

#odds
B1_fixtures$b1_AH_n125_H_odds <- round((1/B1_fixtures$b1_AH_n125_H),digits = 2)
B1_fixtures$b1_AH_n125_A_odds <- round((1/B1_fixtures$b1_AH_n125_A),digits = 2)

B1_fixtures$b1_AH_n125_H_odds
B1_fixtures$b1_AH_n125_A_odds
#percentages
B1_fixtures$b1_AH_n125_H <- percent(B1_fixtures$b1_AH_n125_H, accuracy = 0.1)
B1_fixtures$b1_AH_n125_A <- percent(B1_fixtures$b1_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
B1_fixtures$b1_AH_125_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
    B1_fixtures$b1_5_0 +B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
    B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
    B1_fixtures$b1_6_5 + B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6 + B1_fixtures$b1_0_1 + B1_fixtures$b1_1_2 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_3_4 + B1_fixtures$b1_4_5 + B1_fixtures$b1_5_6
)
#AH_125_A
B1_fixtures$b1_AH_125_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 +B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6 + B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 +
    B1_fixtures$b1_4_4 + B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6 + B1_fixtures$b1_1_0 + B1_fixtures$b1_2_1 +
    B1_fixtures$b1_3_2 + B1_fixtures$b1_4_3 + B1_fixtures$b1_5_4 + B1_fixtures$b1_6_5
)

#odds
B1_fixtures$b1_AH_125_H_odds <- round((1/B1_fixtures$b1_AH_125_H),digits = 2)
B1_fixtures$b1_AH_125_A_odds <- round((1/B1_fixtures$b1_AH_125_A),digits = 2)

B1_fixtures$b1_AH_125_H_odds
B1_fixtures$b1_AH_125_A_odds
#percentages
B1_fixtures$b1_AH_125_H <- percent(B1_fixtures$b1_AH_125_H, accuracy = 0.1)
B1_fixtures$b1_AH_125_A <- percent(B1_fixtures$b1_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
B1_fixtures$b1_ov25 <- percent(B1_fixtures$b1_ov25, accuracy = 0.1)

B1_fixtures$b1_un25 <- percent(B1_fixtures$b1_un25, accuracy = 0.1)
B1_fixtures$b1_pscore <- paste(round(B1_fixtures$b1_xGH,digits = 0),round(B1_fixtures$b1_xGA,digits = 0),sep = "-")
#write out
write.xlsx(B1_fixtures,'NL/B1.xlsx',sheetName = "B1", append = TRUE)
###########################################################################################################
########################B1 END###########################################################################
B1 <- read.csv('../FDAS/B1.csv')
B1$TG <- B1$HG + B1$AG
B1$OV25 <- ifelse(B1$TG >= 3,"Y","N")
b1_ftr_summary <- tabyl(B1,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
b1_ov25_summary <- tabyl(B1,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(b1_ftr_summary,'NL/B1.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(b1_ov25_summary,'NL/B1.xlsx',sheetName = "OVUN25", append = TRUE)



