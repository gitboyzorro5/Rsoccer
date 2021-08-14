library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('RUS.xlsx')
######################RUS START#######################################
#####################################################################
RUS <- read.csv('../FDAS/RUS.csv')
RUS <- within(RUS,rm(Res))
RUS$Date <- dmy(RUS$Date)
RUS <- RUS[order(as.Date(RUS$Date, format = "%d/%m%Y"), decreasing = FALSE),]
RUS$CS <- paste(RUS$HG,RUS$AG, sep = "-")
#RUS_qualificaton <- subset(RUS,tournament == "UEFA Euro qualification")
RUS <- subset(RUS,Season == "2021/2022")
#RUS <- RUS[RUS$Date > '2008-01-01',])
RUS$TG <- RUS$HG + RUS$AG
RUS$OV25 <- ifelse(RUS$TG >= 3,"Y","N")
RUS$FTR <- with(RUS,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
rus_totalgoalsv2 <- tapply(RUS$TG, RUS[c("Home", "Away")],mean)
rus_totalgoalsv2
rus_hgtotals <- rowSums(rus_totalgoalsv2,na.rm = T)
rus_agtotals <- colSums(rus_totalgoalsv2,na.rm = T)

rus_totalgoals <- rus_hgtotals + rus_agtotals
rus_totalgoalsv2 <- cbind(rus_totalgoalsv2,rus_totalgoals)
rus_teams <- sort(unique(RUS$Home))
rus_home_games <- c()
rus_away_games <-c()
for (i_rus in 1:length(rus_teams))
{

  rus_home_games[i_rus] <- nrow(RUS[RUS$Home == rus_teams[i_rus],])
  rus_away_games[i_rus]  <- nrow(RUS[RUS$Away == rus_teams[i_rus],])

}
rus_games_played <- rus_home_games + rus_away_games
rus_goaltotalsv2 <- cbind(rus_totalgoalsv2,rus_games_played)
rus_avg_totalgoals <- round((rus_totalgoals/ rus_games_played), digits = 4)
rus_goaltotalsv2[is.na(rus_goaltotalsv2)] <- ""
rus_goaltotalsv2 <- cbind(rus_goaltotalsv2,rus_avg_totalgoals)
write.xlsx(rus_goaltotalsv2,'RUS.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
rus_goalscored_h <- tapply(RUS$HG, RUS[c("Home", "Date")],mean)
rus_goalscored_a <- tapply(RUS$AG, RUS[c("Away", "Date")],mean)
rus_goalscored_h[is.na(rus_goalscored_h)] <- ""
rus_goalscored_a[is.na(rus_goalscored_a)] <- ""

for(rus_rowhgs in 1:nrow(rus_goalscored_h)) {
  for(rus_colhgs in 1:ncol(rus_goalscored_h)) {

    # print(my_matrix[row, col])
    for(rus_rowags in 1:nrow(rus_goalscored_a)) {
      for(rus_colags in 1:ncol(rus_goalscored_a)) {
        ifelse(!rus_goalscored_a[rus_rowags,rus_colags]=="",rus_goalscored_h[rus_rowags,rus_colags] <- rus_goalscored_a[rus_rowags,rus_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rus_goalscored_h,'RUS.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
rus_goalconceded_h <- tapply(RUS$AG, RUS[c("Home", "Date")],mean)
rus_goalconceded_a <- tapply(RUS$HG, RUS[c("Away", "Date")],mean)
rus_goalconceded_h[is.na(rus_goalconceded_h)] <- ""
rus_goalconceded_a[is.na(rus_goalconceded_a)] <- ""

for(rus_rowhgc in 1:nrow(rus_goalconceded_h)) {
  for(rus_colhgc in 1:ncol(rus_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(rus_rowagc in 1:nrow(rus_goalconceded_a)) {
      for(rus_colagc in 1:ncol(rus_goalconceded_a)) {
        ifelse(!rus_goalconceded_a[rus_rowagc,rus_colagc]=="",rus_goalconceded_h[rus_rowagc,rus_colagc] <- rus_goalconceded_a[rus_rowagc,rus_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rus_goalconceded_h,'RUS.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
rus_form_h <- tapply(RUS$FTR, RUS[c("Home", "Date")],median)
rus_form_a <- tapply(RUS$FTR, RUS[c("Away", "Date")],median)
rus_form_h[is.na(rus_form_h)] <- ""
rus_form_a[is.na(rus_form_a)] <- ""
rus_form_h <- sub("A","L",rus_form_h)
rus_form_h <- sub("H","W",rus_form_h)
rus_form_a <- sub("A","W",rus_form_a)
rus_form_a <- sub("H","L",rus_form_a)
for(rus_rowh_f in 1:nrow(rus_form_h)) {
  for(rus_colh_f in 1:ncol(rus_form_h)) {

    # print(my_matrix[row, col])
    for(rus_rowa_f in 1:nrow(rus_form_a)) {
      for(rus_cola_f in 1:ncol(rus_form_a)) {
        ifelse(!rus_form_a[rus_rowa_f,rus_cola_f]=="",rus_form_h[rus_rowa_f,rus_cola_f] <- rus_form_a[rus_rowa_f,rus_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rus_form_h,'RUS.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
rus_totalgoals_h <- tapply(RUS$TG, RUS[c("Home", "Date")],mean)
rus_totalgoals_a <- tapply(RUS$TG, RUS[c("Away", "Date")],mean)
rus_totalgoals_h[is.na(rus_totalgoals_h)] <- ""
rus_totalgoals_a[is.na(rus_totalgoals_a)] <- ""
for(rus_rowh in 1:nrow(rus_totalgoals_h)) {
  for(rus_colh in 1:ncol(rus_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(rus_rowa in 1:nrow(rus_totalgoals_a)) {
      for(rus_cola in 1:ncol(rus_totalgoals_a)) {
        ifelse(!rus_totalgoals_a[rus_rowa,rus_cola]=="",rus_totalgoals_h[rus_rowa,rus_cola] <- rus_totalgoals_a[rus_rowa,rus_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rus_totalgoals_h,'RUS.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
rus_form_team_against_h <- tapply(RUS$Away, RUS[c("Home", "Date")],median)
rus_form_team_against_a <- tapply(RUS$Home, RUS[c("Away", "Date")],median)
rus_form_team_against_h[is.na(rus_form_team_against_h)] <- ""
rus_form_team_against_a[is.na(rus_form_team_against_a)] <- ""
for(rus_rowh_f_against in 1:nrow(rus_form_team_against_h)) {
  for(rus_colh_f_against in 1:ncol(rus_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(rus_rowa_f_against in 1:nrow(rus_form_team_against_a)) {
      for(rus_cola_f_against in 1:ncol(rus_form_team_against_a)) {
        ifelse(!rus_form_team_against_a[rus_rowa_f_against,rus_cola_f_against]=="",rus_form_team_against_h[rus_rowa_f_against,rus_cola_f_against] <- rus_form_team_against_a[rus_rowa_f_against,rus_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#RUS
rus_un05_home <- c()
rus_un05_away <- c()
rus_ov05_home <- c()
rus_ov05_away <- c()

rus_un15_home <- c()
rus_un15_away <- c()
rus_ov15_home <- c()
rus_ov15_away <- c()

rus_un25_home <- c()
rus_un25_away <- c()
rus_ov25_home <- c()
rus_ov25_away <- c()

rus_un35_home <- c()
rus_un35_away <- c()
rus_ov35_home <- c()
rus_ov35_away <- c()

rus_un45_home <- c()
rus_un45_away <- c()
rus_ov45_home <- c()
rus_ov45_away <- c()

rus_un55_home <- c()
rus_un55_away <- c()
rus_ov55_home <- c()
rus_ov55_away <- c()

for (i_rus_tg in 1:length(rus_teams))
{

  rus_un05_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG == 0,])
  rus_un05_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG == 0,])

  rus_ov05_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG > 0,])
  rus_ov05_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG > 0,])

  rus_un15_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG <= 1,])
  rus_un15_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG <= 1,])

  rus_ov15_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG >= 2,])
  rus_ov15_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG >= 2,])

  rus_un25_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG <= 2,])
  rus_un25_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG <= 2,])

  rus_ov25_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG >=3,])
  rus_ov25_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG >=3,])

  rus_un35_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG <= 3,])
  rus_un35_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG <= 3,])

  rus_ov35_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG >= 4,])
  rus_ov35_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG >= 4,])

  rus_un45_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG <= 4,])
  rus_un45_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG <= 4,])

  rus_ov45_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG >= 5,])
  rus_ov45_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG >= 5,])

  rus_un55_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG <= 5,])
  rus_un55_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG <= 5,])

  rus_ov55_home[i_rus_tg] <- nrow(RUS[RUS$Home == rus_teams[i_rus_tg] & RUS$TG >= 6,])
  rus_ov55_away[i_rus_tg] <- nrow(RUS[RUS$Away == rus_teams[i_rus_tg] & RUS$TG >= 6,])


}

rus_un05 <- rus_un05_home + rus_un05_away
rus_ov05 <- rus_ov05_home + rus_ov05_away

rus_un15 <- rus_un15_home + rus_un15_away
rus_ov15 <- rus_ov15_home + rus_ov15_away

rus_un25 <- rus_un25_home + rus_un25_away
rus_ov25 <- rus_ov25_home + rus_ov25_away

rus_un35 <- rus_un35_home + rus_un35_away
rus_ov35 <- rus_ov35_home + rus_ov35_away

rus_un45 <- rus_un45_home + rus_un45_away
rus_ov45 <- rus_ov45_home + rus_ov45_away

rus_un55 <- rus_un55_home + rus_un55_away
rus_ov55 <- rus_ov55_home + rus_ov55_away

rus_ovundata <- cbind(rus_teams,rus_un05,rus_ov05,rus_un15,rus_ov15,rus_un25,rus_ov25,rus_un35,rus_ov35,rus_un45,rus_ov45,rus_un55,rus_ov55)
write.xlsx(rus_ovundata,'RUS.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
rus_csform_h <- tapply(RUS$CS, RUS[c("Home", "Date")],median)
rus_csform_a <- tapply(RUS$CS, RUS[c("Away", "Date")],median)

rus_csform_h[is.na(rus_csform_h)] <- ""
rus_csform_a[is.na(rus_csform_a)] <- ""

for(rus_rowh_f_cs in 1:nrow(rus_csform_h)) {
  for(rus_colh_f_cs in 1:ncol(rus_csform_h)) {

    # print(my_matrix[row, col])
    for(rus_rowa_f_cs in 1:nrow(rus_csform_a)) {
      for(rus_cola_f_cs in 1:ncol(rus_csform_a)) {
        ifelse(!rus_csform_a[rus_rowa_f_cs,rus_cola_f_cs]=="",rus_csform_h[rus_rowa_f_cs,rus_cola_f_cs] <- rus_csform_a[rus_rowa_f_cs,rus_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
rus_home_gs <- aggregate(RUS$HG, by = list(RUS$Home), FUN = sum)
rus_home_gs_avg <- aggregate(RUS$HG, by = list(RUS$Home),mean)
rus_home_scoring <- merge(rus_home_gs,rus_home_gs_avg, by='Group.1',all = T)
names(rus_home_scoring)[names(rus_home_scoring) == "x.x"] <- "TFthg"
names(rus_home_scoring)[names(rus_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
rus_away_gs <- aggregate(RUS$AG, by = list(RUS$Away), FUN = sum)
rus_away_gs_avg <- aggregate(RUS$AG, by = list(RUS$Away),mean)
rus_away_scoring <- merge(rus_away_gs,rus_away_gs_avg, by='Group.1',all = T)
names(rus_away_scoring)[names(rus_away_scoring) == "x.x"] <- "TFtag"
names(rus_away_scoring)[names(rus_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
rus_scoring <- merge(rus_home_scoring,rus_away_scoring,by='Group.1',all = T)
rus_scoring$TGS <- rus_scoring$TFthg + rus_scoring$TFtag

#home goals conceded
rus_home_gc <- aggregate(RUS$AG, by = list(RUS$Home), FUN = sum)
rus_home_gc_avg <- aggregate(RUS$AG, by = list(RUS$Home),mean)
rus_home_conceding <- merge(rus_home_gc,rus_home_gc_avg, by='Group.1',all = T)
names(rus_home_conceding)[names(rus_home_conceding) == "x.x"] <- "TFthc"
names(rus_home_conceding)[names(rus_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
rus_away_gc <- aggregate(RUS$HG, by = list(RUS$Away), FUN = sum)
rus_away_gc_avg <- aggregate(RUS$HG, by = list(RUS$Away),mean)
rus_away_conceding <- merge(rus_away_gc,rus_away_gc_avg, by='Group.1',all = T)
names(rus_away_conceding)[names(rus_away_conceding) == "x.x"] <- "TFtac"
names(rus_away_conceding)[names(rus_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
rus_conceding <- merge(rus_home_conceding,rus_away_conceding,by='Group.1',all = T)
rus_conceding$TGC <- rus_conceding$TFthc + rus_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
rus_home_wins <- c()
rus_away_wins <- c()
rus_home_draws <- c()
rus_away_draws <- c()
rus_home_loss <- c()
rus_away_loss <- c()



for (i_rus_wins in 1:length(rus_teams))
{

  rus_home_wins[i_rus_wins] <- nrow(RUS[RUS$Home == rus_teams[i_rus_wins] & RUS$FTR == "H",])
  rus_away_wins[i_rus_wins] <- nrow(RUS[RUS$Away == rus_teams[i_rus_wins] & RUS$FTR == "A",])
  rus_home_draws[i_rus_wins] <- nrow(RUS[RUS$Home == rus_teams[i_rus_wins] & RUS$FTR == "D",])
  rus_away_draws[i_rus_wins] <- nrow(RUS[RUS$Away == rus_teams[i_rus_wins] & RUS$FTR == "D",])
  rus_home_loss[i_rus_wins] <- nrow(RUS[RUS$Home == rus_teams[i_rus_wins] & RUS$FTR == "A",])
  rus_away_loss[i_rus_wins] <- nrow(RUS[RUS$Away == rus_teams[i_rus_wins] & RUS$FTR == "H",])

}

rus_total_wins <- rus_home_wins + rus_away_wins
rus_total_draws <- rus_home_draws + rus_away_draws
rus_total_loss <- rus_home_loss + rus_away_loss

rus_league_table <- cbind(rus_teams,rus_games_played,rus_total_wins,rus_total_draws,rus_total_loss)
rus_GS <- rus_scoring$TGS
rus_GC <-rus_conceding$TGC
rus_GD <- rus_scoring$TGS - rus_conceding$TGC
rus_PTS <- (rus_total_wins*3) + (rus_total_draws*1)
rus_league_table <- cbind(rus_league_table,rus_GS,rus_GC,rus_GD,rus_PTS)
rus_league_table <- as.data.frame(rus_league_table)
#rename the columns
names(rus_league_table)[names(rus_league_table) == "rus_teams"] <- "Team"
names(rus_league_table)[names(rus_league_table) == "rus_games_played"] <- "P"
names(rus_league_table)[names(rus_league_table) == "rus_total_wins"] <- "W"
names(rus_league_table)[names(rus_league_table) == "rus_total_draws"] <- "D"
names(rus_league_table)[names(rus_league_table) == "rus_total_loss"] <- "L"
names(rus_league_table)[names(rus_league_table) == "rus_GS"] <- "F"
names(rus_league_table)[names(rus_league_table) == "rus_GC"] <- "A"
points_rus <- rus_league_table[order(as.numeric(rus_league_table$rus_PTS), decreasing = TRUE),]
points_rus$rus_rank <- 1:length(rus_teams)
row.names(points_rus) <- points_rus$rus_rank
#create final_rus_hf_against with team ranks in brackets
for(rus_rowhrank in 1:nrow(rus_form_team_against_h)) {
  for(rus_colhrank in 1:ncol(rus_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!rus_form_team_against_h[rus_rowhrank,rus_colhrank]=="",rus_form_team_against_h[rus_rowhrank,rus_colhrank] <- paste(rus_form_team_against_h[rus_rowhrank,rus_colhrank],"(",points_rus$rus_rank[points_rus$Team ==rus_form_team_against_h[rus_rowhrank,rus_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_rus,'RUS.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six rus###################################################
#RUS
#form
#create final_rus_hf object
rus_last_n_games <- 6
final_rus_hf <- c()
for(index_rus_hf in 1:length(rus_teams))
{
  index_rus_hf <- row.names(rus_form_h) == rus_teams[index_rus_hf]
  form_rus_hf <- rus_form_h[index_rus_hf]
  deleted_form_rus_hf <- form_rus_hf[!form_rus_hf[] == ""]
  l6_form_rus_hf <- tail(deleted_form_rus_hf,rus_last_n_games)
  l6_form_rus_hf <- paste(l6_form_rus_hf,collapse = " ")
  final_rus_hf[index_rus_hf] <- rbind(paste(rus_teams[index_rus_hf],l6_form_rus_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rus_teams[index],l6_form)

}

#change column names
final_rus_hf <- as.data.frame(final_rus_hf)
colnames(final_rus_hf) <- "Form"
#goals scored
#create final_rus_gs object
final_rus_gs <- c()
suml6_rus_gs <- c()
for(index_rus_gs in 1:length(rus_teams))
{
  index_rus_gs <- row.names(rus_goalscored_h) == rus_teams[index_rus_gs]
  form_rus_gs <- rus_goalscored_h[index_rus_gs]
  deleted_form_rus_gs <- form_rus_gs[!form_rus_gs[] == ""]
  l6_form_rus_gs <- tail(deleted_form_rus_gs,rus_last_n_games)
  l6_form_rus_gs <- as.numeric(l6_form_rus_gs)
  suml6_rus_gs[index_rus_gs] <- sum(l6_form_rus_gs)
  suml6_rus_gs[index_rus_gs] <- paste("(",suml6_rus_gs[index_rus_gs],")",sep = "")
  l6_form_rus_gs <- paste(l6_form_rus_gs,collapse = " ")
  final_rus_gs[index_rus_gs] <- rbind(paste(rus_teams[index_rus_gs],l6_form_rus_gs,suml6_rus_gs[index_rus_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rus_teams[index],l6_form)

}
final_rus_gs
#change column names
final_rus_gs <- as.data.frame(final_rus_gs)
colnames(final_rus_gs) <- "Goals scored"
#goal conceded
#create final_rus_gc object
final_rus_gc <- c()
suml6_rus_gc <- c()
for(index_rus_gc in 1:length(rus_teams))
{
  index_rus_gc <- row.names(rus_goalconceded_h) == rus_teams[index_rus_gc]
  form_rus_gc <- rus_goalconceded_h[index_rus_gc]
  deleted_form_rus_gc <- form_rus_gc[!form_rus_gc[] == ""]
  l6_form_rus_gc <- tail(deleted_form_rus_gc,rus_last_n_games)
  l6_form_rus_gc <- as.numeric(l6_form_rus_gc)
  suml6_rus_gc[index_rus_gc] <- sum(l6_form_rus_gc)
  suml6_rus_gc[index_rus_gc] <- paste("(",suml6_rus_gc[index_rus_gc],")",sep = "")
  l6_form_rus_gc <- paste(l6_form_rus_gc,collapse = " ")
  final_rus_gc[index_rus_gc] <- rbind(paste(rus_teams[index_rus_gc],l6_form_rus_gc,suml6_rus_gc[index_rus_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rus_teams[index],l6_form)

}

#change column names
final_rus_gc <- as.data.frame(final_rus_gc)
colnames(final_rus_gc) <- "Goals conceded"
#total goals
#create final_rus_tg object
final_rus_tg <- c()
suml6_rus_tg <- c()
for(index_rus_tg in 1:length(rus_teams))
{
  index_rus_tg <- row.names(rus_totalgoals_h) == rus_teams[index_rus_tg]
  form_rus_tg <- rus_totalgoals_h[index_rus_tg]
  deleted_form_rus_tg <- form_rus_tg[!form_rus_tg[] == ""]
  l6_form_rus_tg <- tail(deleted_form_rus_tg,rus_last_n_games)
  l6_form_rus_tg <- as.numeric(l6_form_rus_tg)
  suml6_rus_tg[index_rus_tg] <- sum(l6_form_rus_tg)
  suml6_rus_tg[index_rus_tg] <- paste("(",suml6_rus_tg[index_rus_tg],")",sep = "")
  l6_form_rus_tg <- paste(l6_form_rus_tg,collapse = " ")
  final_rus_tg[index_rus_tg] <- rbind(paste(rus_teams[index_rus_tg],l6_form_rus_tg,suml6_rus_tg[index_rus_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rus_teams[index],l6_form)

}
#change column names
final_rus_tg <- as.data.frame(final_rus_tg)
colnames(final_rus_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_rus_hf object
final_rus_cs <- c()
for(index_rus_cs in 1:length(rus_teams))
{
  index_rus_cs <- row.names(rus_csform_h) == rus_teams[index_rus_cs]
  csform_rus_cs <- rus_csform_h[index_rus_cs]
  deleted_csform_rus_cs <- csform_rus_cs[!csform_rus_cs[] == ""]
  l6_csform_rus_cs <- tail(deleted_csform_rus_cs,rus_last_n_games)
  l6_csform_rus_cs <- paste(l6_csform_rus_cs,collapse = " ")
  final_rus_cs[index_rus_cs] <- rbind(paste(rus_teams[index_rus_cs],l6_csform_rus_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",rus_teams[index],l6_csform)

}

#change column names
final_rus_cs <- as.data.frame(final_rus_cs)
colnames(final_rus_cs) <- "CSForm"
#################################################
#Team against
#create final_rus_hf_against
final_rus_hf_against <- c()
for(index_rus_hf_against in 1:length(rus_teams))
{
  index_rus_hf_against <- row.names(rus_form_team_against_h) == rus_teams[index_rus_hf_against]
  form_rus_hf_against <- rus_form_team_against_h[index_rus_hf_against]
  deleted_form_rus_hf_against <- form_rus_hf_against[!form_rus_hf_against[] == ""]
  l6_form_rus_hf_against <- tail(deleted_form_rus_hf_against,rus_last_n_games)
  l6_form_rus_hf_against <- paste(l6_form_rus_hf_against,collapse = " ")
  final_rus_hf_against[index_rus_hf_against] <- rbind(paste(rus_teams[index_rus_hf_against],l6_form_rus_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rus_teams[index],l6_form)

}
final_rus_hf_against <- as.data.frame(final_rus_hf_against)
colnames(final_rus_hf_against) <- "Team against"
#combine the columns
final_rus_all <- cbind(final_rus_hf,final_rus_gs,final_rus_gc,final_rus_tg,final_rus_cs,final_rus_hf_against)
write.xlsx(final_rus_all,'RUS.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
rus_GP <- nrow(RUS)
#Calculate total home goals for each division
rus_T_HG <- sum(rus_home_gs$x)
#calculate average home goal
rus_avg_HG <- round(rus_T_HG /rus_GP, digits = 4)
############################################################
#Calculate total away goals for each division
rus_T_AG <- sum(rus_away_gs$x)
#calculate average away goal
rus_avg_AG <- round(rus_T_AG /rus_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
rus_home_as <- round(((rus_home_gs$x/rus_home_games))/rus_avg_HG, digits = 4)
#calculate away attack strength
rus_away_as <- round(((rus_away_gs$x/rus_away_games))/rus_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
rus_avg_HC <- round(rus_T_AG /rus_GP, digits = 4)
#avg away concede
rus_avg_AC <- round(rus_T_HG /rus_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
rus_home_ds <- round(((rus_home_gc$x/rus_home_games))/rus_avg_HC, digits = 4)
#away defense strength
rus_away_ds <- round(((rus_away_gc$x/rus_away_games))/rus_avg_AC, digits = 4)
#############################################################################
#home poisson data
#rus
rus_division <- c()
rus_division[1:length(rus_teams)] <- "RUS"
rus_home_poisson <- cbind(rus_division,rus_teams,rus_avg_HG,rus_home_as,rus_home_ds)
#################################################################################
#away poisson data
#rus
rus_division <- c()
rus_division[1:length(rus_teams)] <- "RUS"
rus_away_poisson <- cbind(rus_division,rus_teams,rus_avg_AG,rus_away_as,rus_away_ds)

#create home and away csv
#rus_home_poisson <- rbind(rus_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#rus_away_poisson <- rbind(rus_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(rus_home_poisson,'RUS.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(rus_away_poisson,'RUS.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################RUS FIXTURES##########################################################################
#RUS
HomeTeam_rus <- rep(rus_teams, each = length(rus_teams))
AwayTeam_rus <- rep(rus_teams, length(rus_teams))
RUS_fixtures <- cbind(HomeTeam_rus,AwayTeam_rus)
RUS_fixtures <- as.data.frame(RUS_fixtures)
RUS_fixtures <- RUS_fixtures[!RUS_fixtures$HomeTeam_rus == RUS_fixtures$AwayTeam_rus,]
rownames(RUS_fixtures) <- NULL
RUS_fixtures$Div <- "RUS"
RUS_fixtures <- RUS_fixtures[,c(3,1,2)]

RUS_fixtures$avg_HG_rus <- rus_avg_HG

RUS_fixtures$rus_homeas <- rep(rus_home_as,each = length(rus_teams)-1)

rus_awayds_lookup <- cbind(rus_teams,rus_away_ds)

rus_awayds_lookup <- as.data.frame(rus_awayds_lookup)

colnames(rus_awayds_lookup) <- c("AwayTeam_rus","rus_awayds")


require('RH2')
RUS_fixtures$rus_awayds <- sqldf("SELECT rus_awayds_lookup.rus_awayds FROM rus_awayds_lookup INNER JOIN RUS_fixtures ON rus_awayds_lookup.AwayTeam_rus = RUS_fixtures.AwayTeam_rus")

RUS_fixtures$avg_AG_rus <- rus_avg_AG

rus_awayas_lookup <- cbind(rus_teams,rus_away_as)

rus_awayas_lookup <- as.data.frame(rus_awayas_lookup)

colnames(rus_awayas_lookup) <- c("AwayTeam_rus","rus_awayas")


RUS_fixtures$rus_awayas <- sqldf("SELECT rus_awayas_lookup.rus_awayas FROM rus_awayas_lookup INNER JOIN RUS_fixtures ON rus_awayas_lookup.AwayTeam_rus = RUS_fixtures.AwayTeam_rus")

RUS_fixtures$rus_homeds <- rep(rus_home_ds,each = length(rus_teams)-1)

RUS_fixtures$rus_awayds <- as.numeric(unlist(RUS_fixtures$rus_awayds))
#xGH
RUS_fixtures$rus_xGH <- RUS_fixtures$avg_HG_rus * RUS_fixtures$rus_homeas * RUS_fixtures$rus_awayds

#xGA

RUS_fixtures$rus_awayas <- as.numeric(unlist(RUS_fixtures$rus_awayas))

RUS_fixtures$rus_xGA <- RUS_fixtures$avg_AG_rus * RUS_fixtures$rus_awayas * RUS_fixtures$rus_homeds

RUS_fixtures$rus_0_0 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_0 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_0_1 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_1 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_0 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_0_2 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_2 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_1 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_2 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_3 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_0 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_1 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_2 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_0_3 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_3 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_3 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_4 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_0 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_1 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_2 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_3 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_0_4 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_4 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_4 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_4 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_5 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_0 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_1 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_2 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_3 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_4 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_0_5 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_5 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_5 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_5 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_5 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_6 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_0 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(0,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_1 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(1,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_2 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(2,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_3 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(3,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_4 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(4,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_6_5 <- round(stats::dpois(6,RUS_fixtures$rus_xGH) * stats::dpois(5,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_0_6 <- round(stats::dpois(0,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_1_6 <- round(stats::dpois(1,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_2_6 <- round(stats::dpois(2,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_3_6 <- round(stats::dpois(3,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_4_6 <- round(stats::dpois(4,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
RUS_fixtures$rus_5_6 <- round(stats::dpois(5,RUS_fixtures$rus_xGH) * stats::dpois(6,RUS_fixtures$rus_xGA), digits = 4)
#Home win
RUS_fixtures$rus_H <- (
  RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
    RUS_fixtures$rus_5_0 + RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5
)

RUS_fixtures$rus_H <- percent(RUS_fixtures$rus_H, accuracy = 0.1)

#Draw
RUS_fixtures$rus_D <- (

  RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 + RUS_fixtures$rus_4_4 +
    RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6
)

RUS_fixtures$rus_D <- percent(RUS_fixtures$rus_D, accuracy = 0.1)

#Away

RUS_fixtures$rus_A <- (
  RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
    RUS_fixtures$rus_0_5 + RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6
)

RUS_fixtures$rus_A <- percent(RUS_fixtures$rus_A, accuracy = 0.1)

#ov25
RUS_fixtures$rus_ov25 <- (
  RUS_fixtures$rus_2_1 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 + RUS_fixtures$rus_2_3 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 + RUS_fixtures$rus_0_4 +
    RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 + RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_0 +
    RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 + RUS_fixtures$rus_0_5 +
    RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 + RUS_fixtures$rus_5_5 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5 + RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 +
    RUS_fixtures$rus_4_6 + RUS_fixtures$rus_5_6 + RUS_fixtures$rus_6_6
)
#un25
RUS_fixtures$rus_un25 <- (
  RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_0 + RUS_fixtures$rus_0_1 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_0_2
)
#odds
RUS_fixtures$rus_ov25_odds <- round((1/RUS_fixtures$rus_ov25),digits = 2)
RUS_fixtures$rus_un25_odds <- round((1/RUS_fixtures$rus_un25),digits = 2)

RUS_fixtures$rus_ov25_odds
RUS_fixtures$rus_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
RUS_fixtures$rus_BTTSY <- (
  RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_3_1 + RUS_fixtures$rus_3_2 +
    RUS_fixtures$rus_2_2 + RUS_fixtures$rus_1_3 + RUS_fixtures$rus_2_3 + RUS_fixtures$rus_3_3 + RUS_fixtures$rus_4_4 +
    RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_3 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 +
    RUS_fixtures$rus_3_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 +
    RUS_fixtures$rus_5_4 + RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_6_6 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6
)
#BTTSN
RUS_fixtures$rus_BTTSN <- (
  RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_0 + RUS_fixtures$rus_0_1 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_0_2 +
    RUS_fixtures$rus_3_0 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_5_0 +
    RUS_fixtures$rus_0_5 + RUS_fixtures$rus_6_0 + RUS_fixtures$rus_0_6
)

RUS_fixtures$rus_BTTSY_odds <- round((1/RUS_fixtures$rus_BTTSY),digits = 2)
RUS_fixtures$rus_BTTSN_odds <- round((1/RUS_fixtures$rus_BTTSN),digits = 2)

RUS_fixtures$rus_BTTSY <- percent(RUS_fixtures$rus_BTTSY, accuracy = 0.1)
RUS_fixtures$rus_BTTSN <- percent(RUS_fixtures$rus_BTTSN, accuracy = 0.1)
#odds
RUS_fixtures$rus_BTTSY_odds
RUS_fixtures$rus_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
RUS_fixtures$rus_AH_0_H <- (
  RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
    RUS_fixtures$rus_5_0 +RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5 + RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6
)
#AH_0_A
RUS_fixtures$rus_AH_0_A <- (
  RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
    RUS_fixtures$rus_0_5 +RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6 + RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6
)

#odds
RUS_fixtures$rus_AH_0_H_odds <- round((1/RUS_fixtures$rus_AH_0_H),digits = 2)
RUS_fixtures$rus_AH_0_A_odds <- round((1/RUS_fixtures$rus_AH_0_A),digits = 2)

RUS_fixtures$rus_AH_0_H_odds
RUS_fixtures$rus_AH_0_A_odds
#percentages
RUS_fixtures$rus_AH_0_H <- percent(RUS_fixtures$rus_AH_0_H, accuracy = 0.1)
RUS_fixtures$rus_AH_0_A <- percent(RUS_fixtures$rus_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
RUS_fixtures$rus_AH_n075_H <- (
  RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
    RUS_fixtures$rus_5_0 +RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5
)
#AH_n075_A
RUS_fixtures$rus_AH_n075_A <- (
  RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
    RUS_fixtures$rus_0_5 +RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6
)

#odds
RUS_fixtures$rus_AH_n075_H_odds <- round((1/RUS_fixtures$rus_AH_n075_H),digits = 2)
RUS_fixtures$rus_AH_n075_A_odds <- round((1/RUS_fixtures$rus_AH_n075_A),digits = 2)

RUS_fixtures$rus_AH_n075_H_odds
RUS_fixtures$rus_AH_n075_A_odds
#percentages
RUS_fixtures$rus_AH_n075_H <- percent(RUS_fixtures$rus_AH_n075_H, accuracy = 0.1)
RUS_fixtures$rus_AH_n075_A <- percent(RUS_fixtures$rus_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
RUS_fixtures$rus_AH_075_H <- (
  RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
    RUS_fixtures$rus_5_0 +RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5 + RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6 + RUS_fixtures$rus_0_1 + RUS_fixtures$rus_1_2 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_3_4 + RUS_fixtures$rus_4_5 + RUS_fixtures$rus_5_6
)
#AH_075_A
RUS_fixtures$rus_AH_075_A <- (
  RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
    RUS_fixtures$rus_0_5 +RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6 + RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6 + RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_3 + RUS_fixtures$rus_5_4 + RUS_fixtures$rus_6_5
)

#odds
RUS_fixtures$rus_AH_075_H_odds <- round((1/RUS_fixtures$rus_AH_075_H),digits = 2)
RUS_fixtures$rus_AH_075_A_odds <- round((1/RUS_fixtures$rus_AH_075_A),digits = 2)

RUS_fixtures$rus_AH_075_H_odds
RUS_fixtures$rus_AH_075_A_odds
#percentages
RUS_fixtures$rus_AH_075_H <- percent(RUS_fixtures$rus_AH_075_H, accuracy = 0.1)
RUS_fixtures$rus_AH_075_A <- percent(RUS_fixtures$rus_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
RUS_fixtures$rus_AH_n125_H <- (
  RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
    RUS_fixtures$rus_5_0 +RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5
)
#AH_n125_A
RUS_fixtures$rus_AH_n125_A <- (
  RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
    RUS_fixtures$rus_0_5 +RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6
)

#odds
RUS_fixtures$rus_AH_n125_H_odds <- round((1/RUS_fixtures$rus_AH_n125_H),digits = 2)
RUS_fixtures$rus_AH_n125_A_odds <- round((1/RUS_fixtures$rus_AH_n125_A),digits = 2)

RUS_fixtures$rus_AH_n125_H_odds
RUS_fixtures$rus_AH_n125_A_odds
#percentages
RUS_fixtures$rus_AH_n125_H <- percent(RUS_fixtures$rus_AH_n125_H, accuracy = 0.1)
RUS_fixtures$rus_AH_n125_A <- percent(RUS_fixtures$rus_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
RUS_fixtures$rus_AH_125_H <- (
  RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_0 + RUS_fixtures$rus_2_1 + RUS_fixtures$rus_3_0 + RUS_fixtures$rus_3_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_0 + RUS_fixtures$rus_4_1 + RUS_fixtures$rus_4_2 + RUS_fixtures$rus_4_3 +
    RUS_fixtures$rus_5_0 +RUS_fixtures$rus_5_1 + RUS_fixtures$rus_5_2 + RUS_fixtures$rus_5_3 + RUS_fixtures$rus_5_4 +
    RUS_fixtures$rus_6_0 + RUS_fixtures$rus_6_1 + RUS_fixtures$rus_6_2 + RUS_fixtures$rus_6_3 + RUS_fixtures$rus_6_4 +
    RUS_fixtures$rus_6_5 + RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6 + RUS_fixtures$rus_0_1 + RUS_fixtures$rus_1_2 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_3_4 + RUS_fixtures$rus_4_5 + RUS_fixtures$rus_5_6
)
#AH_125_A
RUS_fixtures$rus_AH_125_A <- (
  RUS_fixtures$rus_0_1 + RUS_fixtures$rus_0_2 + RUS_fixtures$rus_1_2 + RUS_fixtures$rus_0_3 + RUS_fixtures$rus_1_3 +
    RUS_fixtures$rus_2_3 + RUS_fixtures$rus_0_4 + RUS_fixtures$rus_1_4 + RUS_fixtures$rus_2_4 + RUS_fixtures$rus_3_4 +
    RUS_fixtures$rus_0_5 +RUS_fixtures$rus_1_5 + RUS_fixtures$rus_2_5 + RUS_fixtures$rus_3_5 + RUS_fixtures$rus_4_5 +
    RUS_fixtures$rus_0_6 + RUS_fixtures$rus_1_6 + RUS_fixtures$rus_2_6 + RUS_fixtures$rus_3_6 + RUS_fixtures$rus_4_6 +
    RUS_fixtures$rus_5_6 + RUS_fixtures$rus_0_0 + RUS_fixtures$rus_1_1 + RUS_fixtures$rus_2_2 + RUS_fixtures$rus_3_3 +
    RUS_fixtures$rus_4_4 + RUS_fixtures$rus_5_5 + RUS_fixtures$rus_6_6 + RUS_fixtures$rus_1_0 + RUS_fixtures$rus_2_1 +
    RUS_fixtures$rus_3_2 + RUS_fixtures$rus_4_3 + RUS_fixtures$rus_5_4 + RUS_fixtures$rus_6_5
)

#odds
RUS_fixtures$rus_AH_125_H_odds <- round((1/RUS_fixtures$rus_AH_125_H),digits = 2)
RUS_fixtures$rus_AH_125_A_odds <- round((1/RUS_fixtures$rus_AH_125_A),digits = 2)

RUS_fixtures$rus_AH_125_H_odds
RUS_fixtures$rus_AH_125_A_odds
#percentages
RUS_fixtures$rus_AH_125_H <- percent(RUS_fixtures$rus_AH_125_H, accuracy = 0.1)
RUS_fixtures$rus_AH_125_A <- percent(RUS_fixtures$rus_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
RUS_fixtures$rus_ov25 <- percent(RUS_fixtures$rus_ov25, accuracy = 0.1)

RUS_fixtures$rus_un25 <- percent(RUS_fixtures$rus_un25, accuracy = 0.1)
RUS_fixtures$rus_pscore <- paste(round(RUS_fixtures$rus_xGH,digits = 0),round(RUS_fixtures$rus_xGA,digits = 0),sep = "-")
#write out
write.xlsx(RUS_fixtures,'RUS.xlsx',sheetName = "RUS", append = TRUE)
###########################################################################################################
########################RUS END###########################################################################
RUS <- read.csv('../FDAS/RUS.csv')
RUS$TG <- RUS$HG + RUS$AG
RUS$OV25 <- ifelse(RUS$TG >= 3,"Y","N")
rus_ftr_summary <- tabyl(RUS,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
rus_ov25_summary <- tabyl(RUS,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(rus_ftr_summary,'RUS.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(rus_ov25_summary,'RUS.xlsx',sheetName = "OVUN25", append = TRUE)



