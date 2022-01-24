library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('NL/POL.xlsx')
######################POL START#######################################
#####################################################################
POL <- read.csv('../FDAS/POL.csv')
POL <- within(POL,rm(Res))
POL$Date <- dmy(POL$Date)
POL <- POL[order(as.Date(POL$Date, format = "%d/%m%Y"), decreasing = FALSE),]
POL$CS <- paste(POL$HG,POL$AG, sep = "-")
#POL_qualificaton <- subset(POL,tournament == "UEFA Euro qualification")
POL <- subset(POL,Season == "2021/2022")
#POL <- POL[POL$Date > '2008-01-01',])
POL$TG <- POL$HG + POL$AG
POL$OV25 <- ifelse(POL$TG >= 3,"Y","N")
POL$FTR <- with(POL,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
pol_totalgoalsv2 <- tapply(POL$TG, POL[c("Home", "Away")],mean)
pol_hgtotals <- rowSums(pol_totalgoalsv2,na.rm = T)
pol_agtotals <- colSums(pol_totalgoalsv2,na.rm = T)

pol_totalgoals <- pol_hgtotals + pol_agtotals
pol_totalgoalsv2 <- cbind(pol_totalgoalsv2,pol_totalgoals)
pol_teams <- sort(unique(POL$Home))
pol_home_games <- c()
pol_away_games <-c()
for (i_pol in 1:length(pol_teams))
{

  pol_home_games[i_pol] <- nrow(POL[POL$Home == pol_teams[i_pol],])
  pol_away_games[i_pol]  <- nrow(POL[POL$Away == pol_teams[i_pol],])

}
pol_games_played <- pol_home_games + pol_away_games
pol_goaltotalsv2 <- cbind(pol_totalgoalsv2,pol_games_played)
pol_avg_totalgoals <- round((pol_totalgoals/ pol_games_played), digits = 4)
pol_goaltotalsv2[is.na(pol_goaltotalsv2)] <- ""
pol_goaltotalsv2 <- cbind(pol_goaltotalsv2,pol_avg_totalgoals)
write.xlsx(pol_goaltotalsv2,'NL/POL.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
pol_goalscored_h <- tapply(POL$HG, POL[c("Home", "Date")],mean)
pol_goalscored_a <- tapply(POL$AG, POL[c("Away", "Date")],mean)
pol_goalscored_h[is.na(pol_goalscored_h)] <- ""
pol_goalscored_a[is.na(pol_goalscored_a)] <- ""

for(pol_rowhgs in 1:nrow(pol_goalscored_h)) {
  for(pol_colhgs in 1:ncol(pol_goalscored_h)) {

    # print(my_matrix[row, col])
    for(pol_rowags in 1:nrow(pol_goalscored_a)) {
      for(pol_colags in 1:ncol(pol_goalscored_a)) {
        ifelse(!pol_goalscored_a[pol_rowags,pol_colags]=="",pol_goalscored_h[pol_rowags,pol_colags] <- pol_goalscored_a[pol_rowags,pol_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(pol_goalscored_h,'NL/POL.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
pol_goalconceded_h <- tapply(POL$AG, POL[c("Home", "Date")],mean)
pol_goalconceded_a <- tapply(POL$HG, POL[c("Away", "Date")],mean)
pol_goalconceded_h[is.na(pol_goalconceded_h)] <- ""
pol_goalconceded_a[is.na(pol_goalconceded_a)] <- ""

for(pol_rowhgc in 1:nrow(pol_goalconceded_h)) {
  for(pol_colhgc in 1:ncol(pol_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(pol_rowagc in 1:nrow(pol_goalconceded_a)) {
      for(pol_colagc in 1:ncol(pol_goalconceded_a)) {
        ifelse(!pol_goalconceded_a[pol_rowagc,pol_colagc]=="",pol_goalconceded_h[pol_rowagc,pol_colagc] <- pol_goalconceded_a[pol_rowagc,pol_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(pol_goalconceded_h,'NL/POL.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
pol_form_h <- tapply(POL$FTR, POL[c("Home", "Date")],median)
pol_form_a <- tapply(POL$FTR, POL[c("Away", "Date")],median)
pol_form_h[is.na(pol_form_h)] <- ""
pol_form_a[is.na(pol_form_a)] <- ""
pol_form_h <- sub("A","L",pol_form_h)
pol_form_h <- sub("H","W",pol_form_h)
pol_form_a <- sub("A","W",pol_form_a)
pol_form_a <- sub("H","L",pol_form_a)
for(pol_rowh_f in 1:nrow(pol_form_h)) {
  for(pol_colh_f in 1:ncol(pol_form_h)) {

    # print(my_matrix[row, col])
    for(pol_rowa_f in 1:nrow(pol_form_a)) {
      for(pol_cola_f in 1:ncol(pol_form_a)) {
        ifelse(!pol_form_a[pol_rowa_f,pol_cola_f]=="",pol_form_h[pol_rowa_f,pol_cola_f] <- pol_form_a[pol_rowa_f,pol_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(pol_form_h,'NL/POL.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
pol_totalgoals_h <- tapply(POL$TG, POL[c("Home", "Date")],mean)
pol_totalgoals_a <- tapply(POL$TG, POL[c("Away", "Date")],mean)
pol_totalgoals_h[is.na(pol_totalgoals_h)] <- ""
pol_totalgoals_a[is.na(pol_totalgoals_a)] <- ""
for(pol_rowh in 1:nrow(pol_totalgoals_h)) {
  for(pol_colh in 1:ncol(pol_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(pol_rowa in 1:nrow(pol_totalgoals_a)) {
      for(pol_cola in 1:ncol(pol_totalgoals_a)) {
        ifelse(!pol_totalgoals_a[pol_rowa,pol_cola]=="",pol_totalgoals_h[pol_rowa,pol_cola] <- pol_totalgoals_a[pol_rowa,pol_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(pol_totalgoals_h,'NL/POL.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
pol_form_team_against_h <- tapply(POL$Away, POL[c("Home", "Date")],median)
pol_form_team_against_a <- tapply(POL$Home, POL[c("Away", "Date")],median)
pol_form_team_against_h[is.na(pol_form_team_against_h)] <- ""
pol_form_team_against_a[is.na(pol_form_team_against_a)] <- ""
for(pol_rowh_f_against in 1:nrow(pol_form_team_against_h)) {
  for(pol_colh_f_against in 1:ncol(pol_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(pol_rowa_f_against in 1:nrow(pol_form_team_against_a)) {
      for(pol_cola_f_against in 1:ncol(pol_form_team_against_a)) {
        ifelse(!pol_form_team_against_a[pol_rowa_f_against,pol_cola_f_against]=="",pol_form_team_against_h[pol_rowa_f_against,pol_cola_f_against] <- pol_form_team_against_a[pol_rowa_f_against,pol_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
#win margin
pol_winmargin_h <- tapply(POL$HG - POL$AG, POL[c("Home", "Date")],mean)
pol_winmargin_a <- tapply(POL$AG - POL$HG, POL[c("Away", "Date")],mean)
pol_winmargin_h[is.na(pol_winmargin_h)] <- ""
#
for(pol_rowhwm in 1:nrow(pol_winmargin_h)) {
  for(pol_colhwm in 1:ncol(pol_winmargin_h)) {

    # print(my_matrix[row, col])
    for(pol_rowawm in 1:nrow(pol_winmargin_a)) {
      for(pol_colawm in 1:ncol(pol_winmargin_a)) {
        ifelse(!pol_winmargin_a[pol_rowawm,pol_colawm]=="",pol_winmargin_h[pol_rowawm,pol_colawm] <- pol_winmargin_a[pol_rowawm,pol_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#POL
pol_un05_home <- c()
pol_un05_away <- c()
pol_ov05_home <- c()
pol_ov05_away <- c()

pol_un15_home <- c()
pol_un15_away <- c()
pol_ov15_home <- c()
pol_ov15_away <- c()

pol_un25_home <- c()
pol_un25_away <- c()
pol_ov25_home <- c()
pol_ov25_away <- c()

pol_un35_home <- c()
pol_un35_away <- c()
pol_ov35_home <- c()
pol_ov35_away <- c()

pol_un45_home <- c()
pol_un45_away <- c()
pol_ov45_home <- c()
pol_ov45_away <- c()

pol_un55_home <- c()
pol_un55_away <- c()
pol_ov55_home <- c()
pol_ov55_away <- c()

for (i_pol_tg in 1:length(pol_teams))
{

  pol_un05_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG == 0,])
  pol_un05_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG == 0,])

  pol_ov05_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG > 0,])
  pol_ov05_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG > 0,])

  pol_un15_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG <= 1,])
  pol_un15_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG <= 1,])

  pol_ov15_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG >= 2,])
  pol_ov15_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG >= 2,])

  pol_un25_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG <= 2,])
  pol_un25_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG <= 2,])

  pol_ov25_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG >=3,])
  pol_ov25_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG >=3,])

  pol_un35_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG <= 3,])
  pol_un35_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG <= 3,])

  pol_ov35_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG >= 4,])
  pol_ov35_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG >= 4,])

  pol_un45_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG <= 4,])
  pol_un45_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG <= 4,])

  pol_ov45_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG >= 5,])
  pol_ov45_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG >= 5,])

  pol_un55_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG <= 5,])
  pol_un55_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG <= 5,])

  pol_ov55_home[i_pol_tg] <- nrow(POL[POL$Home == pol_teams[i_pol_tg] & POL$TG >= 6,])
  pol_ov55_away[i_pol_tg] <- nrow(POL[POL$Away == pol_teams[i_pol_tg] & POL$TG >= 6,])


}

pol_un05 <- pol_un05_home + pol_un05_away
pol_ov05 <- pol_ov05_home + pol_ov05_away

pol_un15 <- pol_un15_home + pol_un15_away
pol_ov15 <- pol_ov15_home + pol_ov15_away

pol_un25 <- pol_un25_home + pol_un25_away
pol_ov25 <- pol_ov25_home + pol_ov25_away

pol_un35 <- pol_un35_home + pol_un35_away
pol_ov35 <- pol_ov35_home + pol_ov35_away

pol_un45 <- pol_un45_home + pol_un45_away
pol_ov45 <- pol_ov45_home + pol_ov45_away

pol_un55 <- pol_un55_home + pol_un55_away
pol_ov55 <- pol_ov55_home + pol_ov55_away

pol_ovundata <- cbind(pol_teams,pol_un05,pol_ov05,pol_un15,pol_ov15,pol_un25,pol_ov25,pol_un35,pol_ov35,pol_un45,pol_ov45,pol_un55,pol_ov55)
write.xlsx(pol_ovundata,'NL/POL.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
pol_csform_h <- tapply(POL$CS, POL[c("Home", "Date")],median)
pol_csform_a <- tapply(POL$CS, POL[c("Away", "Date")],median)

pol_csform_h[is.na(pol_csform_h)] <- ""
pol_csform_a[is.na(pol_csform_a)] <- ""

for(pol_rowh_f_cs in 1:nrow(pol_csform_h)) {
  for(pol_colh_f_cs in 1:ncol(pol_csform_h)) {

    # print(my_matrix[row, col])
    for(pol_rowa_f_cs in 1:nrow(pol_csform_a)) {
      for(pol_cola_f_cs in 1:ncol(pol_csform_a)) {
        ifelse(!pol_csform_a[pol_rowa_f_cs,pol_cola_f_cs]=="",pol_csform_h[pol_rowa_f_cs,pol_cola_f_cs] <- pol_csform_a[pol_rowa_f_cs,pol_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
pol_home_gs <- aggregate(POL$HG, by = list(POL$Home), FUN = sum)
pol_home_gs_avg <- aggregate(POL$HG, by = list(POL$Home),mean)
pol_home_scoring <- merge(pol_home_gs,pol_home_gs_avg, by='Group.1',all = T)
names(pol_home_scoring)[names(pol_home_scoring) == "x.x"] <- "TFthg"
names(pol_home_scoring)[names(pol_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
pol_away_gs <- aggregate(POL$AG, by = list(POL$Away), FUN = sum)
pol_away_gs_avg <- aggregate(POL$AG, by = list(POL$Away),mean)
pol_away_scoring <- merge(pol_away_gs,pol_away_gs_avg, by='Group.1',all = T)
names(pol_away_scoring)[names(pol_away_scoring) == "x.x"] <- "TFtag"
names(pol_away_scoring)[names(pol_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
pol_scoring <- merge(pol_home_scoring,pol_away_scoring,by='Group.1',all = T)
pol_scoring$TGS <- pol_scoring$TFthg + pol_scoring$TFtag

#home goals conceded
pol_home_gc <- aggregate(POL$AG, by = list(POL$Home), FUN = sum)
pol_home_gc_avg <- aggregate(POL$AG, by = list(POL$Home),mean)
pol_home_conceding <- merge(pol_home_gc,pol_home_gc_avg, by='Group.1',all = T)
names(pol_home_conceding)[names(pol_home_conceding) == "x.x"] <- "TFthc"
names(pol_home_conceding)[names(pol_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
pol_away_gc <- aggregate(POL$HG, by = list(POL$Away), FUN = sum)
pol_away_gc_avg <- aggregate(POL$HG, by = list(POL$Away),mean)
pol_away_conceding <- merge(pol_away_gc,pol_away_gc_avg, by='Group.1',all = T)
names(pol_away_conceding)[names(pol_away_conceding) == "x.x"] <- "TFtac"
names(pol_away_conceding)[names(pol_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
pol_conceding <- merge(pol_home_conceding,pol_away_conceding,by='Group.1',all = T)
pol_conceding$TGC <- pol_conceding$TFthc + pol_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
pol_home_wins <- c()
pol_away_wins <- c()
pol_home_draws <- c()
pol_away_draws <- c()
pol_home_loss <- c()
pol_away_loss <- c()



for (i_pol_wins in 1:length(pol_teams))
{

  pol_home_wins[i_pol_wins] <- nrow(POL[POL$Home == pol_teams[i_pol_wins] & POL$FTR == "H",])
  pol_away_wins[i_pol_wins] <- nrow(POL[POL$Away == pol_teams[i_pol_wins] & POL$FTR == "A",])
  pol_home_draws[i_pol_wins] <- nrow(POL[POL$Home == pol_teams[i_pol_wins] & POL$FTR == "D",])
  pol_away_draws[i_pol_wins] <- nrow(POL[POL$Away == pol_teams[i_pol_wins] & POL$FTR == "D",])
  pol_home_loss[i_pol_wins] <- nrow(POL[POL$Home == pol_teams[i_pol_wins] & POL$FTR == "A",])
  pol_away_loss[i_pol_wins] <- nrow(POL[POL$Away == pol_teams[i_pol_wins] & POL$FTR == "H",])

}

pol_total_wins <- pol_home_wins + pol_away_wins
pol_total_draws <- pol_home_draws + pol_away_draws
pol_total_loss <- pol_home_loss + pol_away_loss

pol_league_table <- cbind(pol_teams,pol_games_played,pol_total_wins,pol_total_draws,pol_total_loss)
pol_GS <- pol_scoring$TGS
pol_GC <-pol_conceding$TGC
pol_GD <- pol_scoring$TGS - pol_conceding$TGC
pol_PTS <- (pol_total_wins*3) + (pol_total_draws*1)
pol_league_table <- cbind(pol_league_table,pol_GS,pol_GC,pol_GD,pol_PTS)
pol_league_table <- as.data.frame(pol_league_table)
#rename the columns
names(pol_league_table)[names(pol_league_table) == "pol_teams"] <- "Team"
names(pol_league_table)[names(pol_league_table) == "pol_games_played"] <- "P"
names(pol_league_table)[names(pol_league_table) == "pol_total_wins"] <- "W"
names(pol_league_table)[names(pol_league_table) == "pol_total_draws"] <- "D"
names(pol_league_table)[names(pol_league_table) == "pol_total_loss"] <- "L"
names(pol_league_table)[names(pol_league_table) == "pol_GS"] <- "F"
names(pol_league_table)[names(pol_league_table) == "pol_GC"] <- "A"
points_pol <- pol_league_table[order(as.numeric(pol_league_table$pol_PTS), decreasing = TRUE),]
points_pol$pol_rank <- 1:length(pol_teams)
row.names(points_pol) <- points_pol$pol_rank
#create final_pol_hf_against with team ranks in brackets
for(pol_rowhrank in 1:nrow(pol_form_team_against_h)) {
  for(pol_colhrank in 1:ncol(pol_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!pol_form_team_against_h[pol_rowhrank,pol_colhrank]=="",pol_form_team_against_h[pol_rowhrank,pol_colhrank] <- paste(pol_form_team_against_h[pol_rowhrank,pol_colhrank],"(",points_pol$pol_rank[points_pol$Team ==pol_form_team_against_h[pol_rowhrank,pol_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_pol,'NL/POL.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six pol###################################################
#POL
#form
#create final_pol_hf object
#pol_last_n_games <- 6
final_pol_hf <- c()
for(index_pol_hf in 1:length(pol_teams))
{
  index_pol_hf <- row.names(pol_form_h) == pol_teams[index_pol_hf]
  form_pol_hf <- pol_form_h[index_pol_hf]
  deleted_form_pol_hf <- form_pol_hf[!form_pol_hf[] == ""]
  l6_form_pol_hf <- tail(deleted_form_pol_hf,pol_last_n_games)
  l6_form_pol_hf <- paste(l6_form_pol_hf,collapse = " ")
  final_pol_hf[index_pol_hf] <- rbind(paste(pol_teams[index_pol_hf],l6_form_pol_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",pol_teams[index],l6_form)

}

#change column names
final_pol_hf <- as.data.frame(final_pol_hf)
colnames(final_pol_hf) <- "Form"
#goals scored
#create final_pol_gs object
final_pol_gs <- c()
suml6_pol_gs <- c()
for(index_pol_gs in 1:length(pol_teams))
{
  index_pol_gs <- row.names(pol_goalscored_h) == pol_teams[index_pol_gs]
  form_pol_gs <- pol_goalscored_h[index_pol_gs]
  deleted_form_pol_gs <- form_pol_gs[!form_pol_gs[] == ""]
  l6_form_pol_gs <- tail(deleted_form_pol_gs,pol_last_n_games)
  l6_form_pol_gs <- as.numeric(l6_form_pol_gs)
  suml6_pol_gs[index_pol_gs] <- sum(l6_form_pol_gs)
  suml6_pol_gs[index_pol_gs] <- paste("(",suml6_pol_gs[index_pol_gs],")",sep = "")
  l6_form_pol_gs <- paste(l6_form_pol_gs,collapse = " ")
  final_pol_gs[index_pol_gs] <- rbind(paste(pol_teams[index_pol_gs],l6_form_pol_gs,suml6_pol_gs[index_pol_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",pol_teams[index],l6_form)

}
final_pol_gs
#change column names
final_pol_gs <- as.data.frame(final_pol_gs)
colnames(final_pol_gs) <- "Goals scored"
#goal conceded
#create final_pol_gc object
final_pol_gc <- c()
suml6_pol_gc <- c()
for(index_pol_gc in 1:length(pol_teams))
{
  index_pol_gc <- row.names(pol_goalconceded_h) == pol_teams[index_pol_gc]
  form_pol_gc <- pol_goalconceded_h[index_pol_gc]
  deleted_form_pol_gc <- form_pol_gc[!form_pol_gc[] == ""]
  l6_form_pol_gc <- tail(deleted_form_pol_gc,pol_last_n_games)
  l6_form_pol_gc <- as.numeric(l6_form_pol_gc)
  suml6_pol_gc[index_pol_gc] <- sum(l6_form_pol_gc)
  suml6_pol_gc[index_pol_gc] <- paste("(",suml6_pol_gc[index_pol_gc],")",sep = "")
  l6_form_pol_gc <- paste(l6_form_pol_gc,collapse = " ")
  final_pol_gc[index_pol_gc] <- rbind(paste(pol_teams[index_pol_gc],l6_form_pol_gc,suml6_pol_gc[index_pol_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",pol_teams[index],l6_form)

}

#change column names
final_pol_gc <- as.data.frame(final_pol_gc)
colnames(final_pol_gc) <- "Goals conceded"
#total goals
#create final_pol_tg object
final_pol_tg <- c()
suml6_pol_tg <- c()
for(index_pol_tg in 1:length(pol_teams))
{
  index_pol_tg <- row.names(pol_totalgoals_h) == pol_teams[index_pol_tg]
  form_pol_tg <- pol_totalgoals_h[index_pol_tg]
  deleted_form_pol_tg <- form_pol_tg[!form_pol_tg[] == ""]
  l6_form_pol_tg <- tail(deleted_form_pol_tg,pol_last_n_games)
  l6_form_pol_tg <- as.numeric(l6_form_pol_tg)
  suml6_pol_tg[index_pol_tg] <- sum(l6_form_pol_tg)
  suml6_pol_tg[index_pol_tg] <- paste("(",suml6_pol_tg[index_pol_tg],")",sep = "")
  l6_form_pol_tg <- paste(l6_form_pol_tg,collapse = " ")
  final_pol_tg[index_pol_tg] <- rbind(paste(pol_teams[index_pol_tg],l6_form_pol_tg,suml6_pol_tg[index_pol_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",pol_teams[index],l6_form)

}
#change column names
final_pol_tg <- as.data.frame(final_pol_tg)
colnames(final_pol_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_pol_hf object
final_pol_cs <- c()
for(index_pol_cs in 1:length(pol_teams))
{
  index_pol_cs <- row.names(pol_csform_h) == pol_teams[index_pol_cs]
  csform_pol_cs <- pol_csform_h[index_pol_cs]
  deleted_csform_pol_cs <- csform_pol_cs[!csform_pol_cs[] == ""]
  l6_csform_pol_cs <- tail(deleted_csform_pol_cs,pol_last_n_games)
  l6_csform_pol_cs <- paste(l6_csform_pol_cs,collapse = " ")
  final_pol_cs[index_pol_cs] <- rbind(paste(pol_teams[index_pol_cs],l6_csform_pol_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",pol_teams[index],l6_csform)

}

#change column names
final_pol_cs <- as.data.frame(final_pol_cs)
colnames(final_pol_cs) <- "CSForm"
#################################################
#################################################
#Win Margin
#goals scored
#create final_pol_wm object
final_pol_wm <- c()
suml6_pol_wm <- c()
for(index_pol_wm in 1:length(pol_teams))
{
  index_pol_wm <- row.names(pol_winmargin_h) == pol_teams[index_pol_wm]
  form_pol_wm <- pol_winmargin_h[index_pol_wm]
  deleted_form_pol_wm <- form_pol_wm[!form_pol_wm[] == ""]
  l6_form_pol_wm <- tail(deleted_form_pol_wm,pol_last_n_games)
  l6_form_pol_wm <- as.numeric(l6_form_pol_wm)
  suml6_pol_wm[index_pol_wm] <- sum(l6_form_pol_wm)
  suml6_pol_wm[index_pol_wm] <- paste("(",suml6_pol_wm[index_pol_wm],")",sep = "")
  l6_form_pol_wm <- paste(l6_form_pol_wm,collapse = " ")
  final_pol_wm[index_pol_wm] <- rbind(paste(pol_teams[index_pol_wm],l6_form_pol_wm,suml6_pol_wm[index_pol_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",pol_teams[index],l6_form)

}
final_pol_wm
#change column names
final_pol_wm <- as.data.frame(final_pol_wm)
colnames(final_pol_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_pol_hf_against
final_pol_hf_against <- c()
for(index_pol_hf_against in 1:length(pol_teams))
{
  index_pol_hf_against <- row.names(pol_form_team_against_h) == pol_teams[index_pol_hf_against]
  form_pol_hf_against <- pol_form_team_against_h[index_pol_hf_against]
  deleted_form_pol_hf_against <- form_pol_hf_against[!form_pol_hf_against[] == ""]
  l6_form_pol_hf_against <- tail(deleted_form_pol_hf_against,pol_last_n_games)
  l6_form_pol_hf_against <- paste(l6_form_pol_hf_against,collapse = " ")
  final_pol_hf_against[index_pol_hf_against] <- rbind(paste(pol_teams[index_pol_hf_against],l6_form_pol_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",pol_teams[index],l6_form)

}
final_pol_hf_against <- as.data.frame(final_pol_hf_against)
colnames(final_pol_hf_against) <- "Team against"
#combine the columns
final_pol_all <- cbind(final_pol_hf,final_pol_gs,final_pol_gc,final_pol_tg,final_pol_cs,final_pol_wm,final_pol_hf_against)
write.xlsx(final_pol_all,'NL/POL.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
pol_GP <- nrow(POL)
#Calculate total home goals for each division
pol_T_HG <- sum(pol_home_gs$x)
#calculate average home goal
pol_avg_HG <- round(pol_T_HG /pol_GP, digits = 4)
############################################################
#Calculate total away goals for each division
pol_T_AG <- sum(pol_away_gs$x)
#calculate average away goal
pol_avg_AG <- round(pol_T_AG /pol_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
pol_home_as <- round(((pol_home_gs$x/pol_home_games))/pol_avg_HG, digits = 4)
#calculate away attack strength
pol_away_as <- round(((pol_away_gs$x/pol_away_games))/pol_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
pol_avg_HC <- round(pol_T_AG /pol_GP, digits = 4)
#avg away concede
pol_avg_AC <- round(pol_T_HG /pol_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
pol_home_ds <- round(((pol_home_gc$x/pol_home_games))/pol_avg_HC, digits = 4)
#away defense strength
pol_away_ds <- round(((pol_away_gc$x/pol_away_games))/pol_avg_AC, digits = 4)
#############################################################################
#home poisson data
#pol
pol_division <- c()
pol_division[1:length(pol_teams)] <- "POL"
pol_home_poisson <- cbind(pol_division,pol_teams,pol_avg_HG,pol_home_as,pol_home_ds)
#################################################################################
#away poisson data
#pol
pol_division <- c()
pol_division[1:length(pol_teams)] <- "POL"
pol_away_poisson <- cbind(pol_division,pol_teams,pol_avg_AG,pol_away_as,pol_away_ds)

#create home and away csv
#pol_home_poisson <- rbind(pol_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#pol_away_poisson <- rbind(pol_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(pol_home_poisson,'NL/POL.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(pol_away_poisson,'NL/POL.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################POL FIXTURES##########################################################################
#POL
HomeTeam_pol <- rep(pol_teams, each = length(pol_teams))
AwayTeam_pol <- rep(pol_teams, length(pol_teams))
POL_fixtures <- cbind(HomeTeam_pol,AwayTeam_pol)
POL_fixtures <- as.data.frame(POL_fixtures)
POL_fixtures <- POL_fixtures[!POL_fixtures$HomeTeam_pol == POL_fixtures$AwayTeam_pol,]
rownames(POL_fixtures) <- NULL
POL_fixtures$Div <- "POL"
POL_fixtures <- POL_fixtures[,c(3,1,2)]

POL_fixtures$avg_HG_pol <- pol_avg_HG

POL_fixtures$pol_homeas <- rep(pol_home_as,each = length(pol_teams)-1)

pol_awayds_lookup <- cbind(pol_teams,pol_away_ds)

pol_awayds_lookup <- as.data.frame(pol_awayds_lookup)

colnames(pol_awayds_lookup) <- c("AwayTeam_pol","pol_awayds")


require('RH2')
POL_fixtures$pol_awayds <- sqldf("SELECT pol_awayds_lookup.pol_awayds FROM pol_awayds_lookup INNER JOIN POL_fixtures ON pol_awayds_lookup.AwayTeam_pol = POL_fixtures.AwayTeam_pol")

POL_fixtures$avg_AG_pol <- pol_avg_AG

pol_awayas_lookup <- cbind(pol_teams,pol_away_as)

pol_awayas_lookup <- as.data.frame(pol_awayas_lookup)

colnames(pol_awayas_lookup) <- c("AwayTeam_pol","pol_awayas")


POL_fixtures$pol_awayas <- sqldf("SELECT pol_awayas_lookup.pol_awayas FROM pol_awayas_lookup INNER JOIN POL_fixtures ON pol_awayas_lookup.AwayTeam_pol = POL_fixtures.AwayTeam_pol")

POL_fixtures$pol_homeds <- rep(pol_home_ds,each = length(pol_teams)-1)

POL_fixtures$pol_awayds <- as.numeric(unlist(POL_fixtures$pol_awayds))
#xGH
POL_fixtures$pol_xGH <- POL_fixtures$avg_HG_pol * POL_fixtures$pol_homeas * POL_fixtures$pol_awayds

#xGA

POL_fixtures$pol_awayas <- as.numeric(unlist(POL_fixtures$pol_awayas))

POL_fixtures$pol_xGA <- POL_fixtures$avg_AG_pol * POL_fixtures$pol_awayas * POL_fixtures$pol_homeds

POL_fixtures$pol_0_0 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_0 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_0_1 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_1 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_0 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_0_2 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_2 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_1 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_2 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_3 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_0 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_1 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_2 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_0_3 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_3 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_3 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_4 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_0 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_1 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_2 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_3 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_0_4 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_4 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_4 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_4 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_5 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_0 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_1 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_2 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_3 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_4 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_0_5 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_5 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_5 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_5 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_5 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_6 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_0 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(0,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_1 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(1,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_2 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(2,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_3 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(3,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_4 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(4,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_6_5 <- round(stats::dpois(6,POL_fixtures$pol_xGH) * stats::dpois(5,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_0_6 <- round(stats::dpois(0,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_1_6 <- round(stats::dpois(1,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_2_6 <- round(stats::dpois(2,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_3_6 <- round(stats::dpois(3,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_4_6 <- round(stats::dpois(4,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
POL_fixtures$pol_5_6 <- round(stats::dpois(5,POL_fixtures$pol_xGH) * stats::dpois(6,POL_fixtures$pol_xGA), digits = 4)
#Home win
POL_fixtures$pol_H <- (
  POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
    POL_fixtures$pol_5_0 + POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5
)

POL_fixtures$pol_H <- percent(POL_fixtures$pol_H, accuracy = 0.1)

#Draw
POL_fixtures$pol_D <- (

  POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 + POL_fixtures$pol_4_4 +
    POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6
)

POL_fixtures$pol_D <- percent(POL_fixtures$pol_D, accuracy = 0.1)

#Away

POL_fixtures$pol_A <- (
  POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
    POL_fixtures$pol_0_5 + POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6
)

POL_fixtures$pol_A <- percent(POL_fixtures$pol_A, accuracy = 0.1)

#ov25
POL_fixtures$pol_ov25 <- (
  POL_fixtures$pol_2_1 + POL_fixtures$pol_1_2 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 + POL_fixtures$pol_2_3 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 + POL_fixtures$pol_0_4 +
    POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 + POL_fixtures$pol_4_4 + POL_fixtures$pol_5_0 +
    POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 + POL_fixtures$pol_0_5 +
    POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 + POL_fixtures$pol_5_5 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5 + POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 +
    POL_fixtures$pol_4_6 + POL_fixtures$pol_5_6 + POL_fixtures$pol_6_6
)
#un25
POL_fixtures$pol_un25 <- (
  POL_fixtures$pol_0_0 + POL_fixtures$pol_1_0 + POL_fixtures$pol_0_1 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_0 + POL_fixtures$pol_0_2
)
#odds
POL_fixtures$pol_ov25_odds <- round((1/POL_fixtures$pol_ov25),digits = 2)
POL_fixtures$pol_un25_odds <- round((1/POL_fixtures$pol_un25),digits = 2)

POL_fixtures$pol_ov25_odds
POL_fixtures$pol_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
POL_fixtures$pol_BTTSY <- (
  POL_fixtures$pol_1_1 + POL_fixtures$pol_2_1 + POL_fixtures$pol_1_2 + POL_fixtures$pol_3_1 + POL_fixtures$pol_3_2 +
    POL_fixtures$pol_2_2 + POL_fixtures$pol_1_3 + POL_fixtures$pol_2_3 + POL_fixtures$pol_3_3 + POL_fixtures$pol_4_4 +
    POL_fixtures$pol_4_1 + POL_fixtures$pol_4_3 + POL_fixtures$pol_4_2 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 +
    POL_fixtures$pol_3_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 +
    POL_fixtures$pol_5_4 + POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_6_6 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6
)
#BTTSN
POL_fixtures$pol_BTTSN <- (
  POL_fixtures$pol_0_0 + POL_fixtures$pol_1_0 + POL_fixtures$pol_0_1 + POL_fixtures$pol_2_0 + POL_fixtures$pol_0_2 +
    POL_fixtures$pol_3_0 + POL_fixtures$pol_0_3 + POL_fixtures$pol_4_0 + POL_fixtures$pol_0_4 + POL_fixtures$pol_5_0 +
    POL_fixtures$pol_0_5 + POL_fixtures$pol_6_0 + POL_fixtures$pol_0_6
)

POL_fixtures$pol_BTTSY_odds <- round((1/POL_fixtures$pol_BTTSY),digits = 2)
POL_fixtures$pol_BTTSN_odds <- round((1/POL_fixtures$pol_BTTSN),digits = 2)

POL_fixtures$pol_BTTSY <- percent(POL_fixtures$pol_BTTSY, accuracy = 0.1)
POL_fixtures$pol_BTTSN <- percent(POL_fixtures$pol_BTTSN, accuracy = 0.1)
#odds
POL_fixtures$pol_BTTSY_odds
POL_fixtures$pol_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
POL_fixtures$pol_AH_0_H <- (
  POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
    POL_fixtures$pol_5_0 +POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5 + POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6
)
#AH_0_A
POL_fixtures$pol_AH_0_A <- (
  POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
    POL_fixtures$pol_0_5 +POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6 + POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6
)

#odds
POL_fixtures$pol_AH_0_H_odds <- round((1/POL_fixtures$pol_AH_0_H),digits = 2)
POL_fixtures$pol_AH_0_A_odds <- round((1/POL_fixtures$pol_AH_0_A),digits = 2)

POL_fixtures$pol_AH_0_H_odds
POL_fixtures$pol_AH_0_A_odds
#percentages
POL_fixtures$pol_AH_0_H <- percent(POL_fixtures$pol_AH_0_H, accuracy = 0.1)
POL_fixtures$pol_AH_0_A <- percent(POL_fixtures$pol_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
POL_fixtures$pol_AH_n075_H <- (
  POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
    POL_fixtures$pol_5_0 +POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5
)
#AH_n075_A
POL_fixtures$pol_AH_n075_A <- (
  POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
    POL_fixtures$pol_0_5 +POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6
)

#odds
POL_fixtures$pol_AH_n075_H_odds <- round((1/POL_fixtures$pol_AH_n075_H),digits = 2)
POL_fixtures$pol_AH_n075_A_odds <- round((1/POL_fixtures$pol_AH_n075_A),digits = 2)

POL_fixtures$pol_AH_n075_H_odds
POL_fixtures$pol_AH_n075_A_odds
#percentages
POL_fixtures$pol_AH_n075_H <- percent(POL_fixtures$pol_AH_n075_H, accuracy = 0.1)
POL_fixtures$pol_AH_n075_A <- percent(POL_fixtures$pol_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
POL_fixtures$pol_AH_075_H <- (
  POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
    POL_fixtures$pol_5_0 +POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5 + POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6 + POL_fixtures$pol_0_1 + POL_fixtures$pol_1_2 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_3_4 + POL_fixtures$pol_4_5 + POL_fixtures$pol_5_6
)
#AH_075_A
POL_fixtures$pol_AH_075_A <- (
  POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
    POL_fixtures$pol_0_5 +POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6 + POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6 + POL_fixtures$pol_1_0 + POL_fixtures$pol_2_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_3 + POL_fixtures$pol_5_4 + POL_fixtures$pol_6_5
)

#odds
POL_fixtures$pol_AH_075_H_odds <- round((1/POL_fixtures$pol_AH_075_H),digits = 2)
POL_fixtures$pol_AH_075_A_odds <- round((1/POL_fixtures$pol_AH_075_A),digits = 2)

POL_fixtures$pol_AH_075_H_odds
POL_fixtures$pol_AH_075_A_odds
#percentages
POL_fixtures$pol_AH_075_H <- percent(POL_fixtures$pol_AH_075_H, accuracy = 0.1)
POL_fixtures$pol_AH_075_A <- percent(POL_fixtures$pol_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
POL_fixtures$pol_AH_n125_H <- (
  POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
    POL_fixtures$pol_5_0 +POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5
)
#AH_n125_A
POL_fixtures$pol_AH_n125_A <- (
  POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
    POL_fixtures$pol_0_5 +POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6
)

#odds
POL_fixtures$pol_AH_n125_H_odds <- round((1/POL_fixtures$pol_AH_n125_H),digits = 2)
POL_fixtures$pol_AH_n125_A_odds <- round((1/POL_fixtures$pol_AH_n125_A),digits = 2)

POL_fixtures$pol_AH_n125_H_odds
POL_fixtures$pol_AH_n125_A_odds
#percentages
POL_fixtures$pol_AH_n125_H <- percent(POL_fixtures$pol_AH_n125_H, accuracy = 0.1)
POL_fixtures$pol_AH_n125_A <- percent(POL_fixtures$pol_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
POL_fixtures$pol_AH_125_H <- (
  POL_fixtures$pol_1_0 + POL_fixtures$pol_2_0 + POL_fixtures$pol_2_1 + POL_fixtures$pol_3_0 + POL_fixtures$pol_3_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_0 + POL_fixtures$pol_4_1 + POL_fixtures$pol_4_2 + POL_fixtures$pol_4_3 +
    POL_fixtures$pol_5_0 +POL_fixtures$pol_5_1 + POL_fixtures$pol_5_2 + POL_fixtures$pol_5_3 + POL_fixtures$pol_5_4 +
    POL_fixtures$pol_6_0 + POL_fixtures$pol_6_1 + POL_fixtures$pol_6_2 + POL_fixtures$pol_6_3 + POL_fixtures$pol_6_4 +
    POL_fixtures$pol_6_5 + POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6 + POL_fixtures$pol_0_1 + POL_fixtures$pol_1_2 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_3_4 + POL_fixtures$pol_4_5 + POL_fixtures$pol_5_6
)
#AH_125_A
POL_fixtures$pol_AH_125_A <- (
  POL_fixtures$pol_0_1 + POL_fixtures$pol_0_2 + POL_fixtures$pol_1_2 + POL_fixtures$pol_0_3 + POL_fixtures$pol_1_3 +
    POL_fixtures$pol_2_3 + POL_fixtures$pol_0_4 + POL_fixtures$pol_1_4 + POL_fixtures$pol_2_4 + POL_fixtures$pol_3_4 +
    POL_fixtures$pol_0_5 +POL_fixtures$pol_1_5 + POL_fixtures$pol_2_5 + POL_fixtures$pol_3_5 + POL_fixtures$pol_4_5 +
    POL_fixtures$pol_0_6 + POL_fixtures$pol_1_6 + POL_fixtures$pol_2_6 + POL_fixtures$pol_3_6 + POL_fixtures$pol_4_6 +
    POL_fixtures$pol_5_6 + POL_fixtures$pol_0_0 + POL_fixtures$pol_1_1 + POL_fixtures$pol_2_2 + POL_fixtures$pol_3_3 +
    POL_fixtures$pol_4_4 + POL_fixtures$pol_5_5 + POL_fixtures$pol_6_6 + POL_fixtures$pol_1_0 + POL_fixtures$pol_2_1 +
    POL_fixtures$pol_3_2 + POL_fixtures$pol_4_3 + POL_fixtures$pol_5_4 + POL_fixtures$pol_6_5
)

#odds
POL_fixtures$pol_AH_125_H_odds <- round((1/POL_fixtures$pol_AH_125_H),digits = 2)
POL_fixtures$pol_AH_125_A_odds <- round((1/POL_fixtures$pol_AH_125_A),digits = 2)

POL_fixtures$pol_AH_125_H_odds
POL_fixtures$pol_AH_125_A_odds
#percentages
POL_fixtures$pol_AH_125_H <- percent(POL_fixtures$pol_AH_125_H, accuracy = 0.1)
POL_fixtures$pol_AH_125_A <- percent(POL_fixtures$pol_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
POL_fixtures$pol_ov25 <- percent(POL_fixtures$pol_ov25, accuracy = 0.1)

POL_fixtures$pol_un25 <- percent(POL_fixtures$pol_un25, accuracy = 0.1)
POL_fixtures$pol_pscore <- paste(round(POL_fixtures$pol_xGH,digits = 0),round(POL_fixtures$pol_xGA,digits = 0),sep = "-")
#write out
write.xlsx(POL_fixtures,'NL/POL.xlsx',sheetName = "POL", append = TRUE)
###########################################################################################################
########################POL END###########################################################################
POL <- read.csv('../FDAS/POL.csv')
POL$TG <- POL$HG + POL$AG
POL$OV25 <- ifelse(POL$TG >= 3,"Y","N")
pol_ftr_summary <- tabyl(POL,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
pol_ov25_summary <- tabyl(POL,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(pol_ftr_summary,'NL/POL.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(pol_ov25_summary,'NL/POL.xlsx',sheetName = "OVUN25", append = TRUE)



