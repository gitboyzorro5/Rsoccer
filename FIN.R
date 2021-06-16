library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('FIN.xlsx')
######################FIN START#######################################
#####################################################################
FIN <- read.csv('../FDAS/FIN.csv')
FIN <- within(FIN,rm(Res))
FIN$Date <- dmy(FIN$Date)
FIN <- FIN[order(as.Date(FIN$Date, format = "%d/%m%Y"), decreasing = FALSE),]
FIN$CS <- paste(FIN$HG,FIN$AG, sep = "-")
#FIN_qualificaton <- subset(FIN,tournament == "UEFA Euro qualification")
FIN <- subset(FIN,Season == "2021")
#FIN <- FIN[FIN$Date > '2008-01-01',])
FIN$TG <- FIN$HG + FIN$AG
FIN$OV25 <- ifelse(FIN$TG >= 3,"Y","N")
FIN$FTR <- with(FIN,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
fin_totalgoalsv2 <- tapply(FIN$TG, FIN[c("Home", "Away")],mean)
fin_totalgoalsv2
fin_hgtotals <- rowSums(fin_totalgoalsv2,na.rm = T)
fin_agtotals <- colSums(fin_totalgoalsv2,na.rm = T)

fin_totalgoals <- fin_hgtotals + fin_agtotals
fin_totalgoalsv2 <- cbind(fin_totalgoalsv2,fin_totalgoals)
fin_teams <- sort(unique(FIN$Home))
fin_home_games <- c()
fin_away_games <-c()
for (i_fin in 1:length(fin_teams))
{

  fin_home_games[i_fin] <- nrow(FIN[FIN$Home == fin_teams[i_fin],])
  fin_away_games[i_fin]  <- nrow(FIN[FIN$Away == fin_teams[i_fin],])

}
fin_games_played <- fin_home_games + fin_away_games
fin_goaltotalsv2 <- cbind(fin_totalgoalsv2,fin_games_played)
fin_avg_totalgoals <- round((fin_totalgoals/ fin_games_played), digits = 4)
fin_goaltotalsv2[is.na(fin_goaltotalsv2)] <- ""
fin_goaltotalsv2 <- cbind(fin_goaltotalsv2,fin_avg_totalgoals)
write.xlsx(fin_goaltotalsv2,'FIN.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
fin_goalscored_h <- tapply(FIN$HG, FIN[c("Home", "Date")],mean)
fin_goalscored_a <- tapply(FIN$AG, FIN[c("Away", "Date")],mean)
fin_goalscored_h[is.na(fin_goalscored_h)] <- ""
fin_goalscored_a[is.na(fin_goalscored_a)] <- ""

for(fin_rowhgs in 1:nrow(fin_goalscored_h)) {
  for(fin_colhgs in 1:ncol(fin_goalscored_h)) {

    # print(my_matrix[row, col])
    for(fin_rowags in 1:nrow(fin_goalscored_a)) {
      for(fin_colags in 1:ncol(fin_goalscored_a)) {
        ifelse(!fin_goalscored_a[fin_rowags,fin_colags]=="",fin_goalscored_h[fin_rowags,fin_colags] <- fin_goalscored_a[fin_rowags,fin_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fin_goalscored_h,'FIN.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
fin_goalconceded_h <- tapply(FIN$AG, FIN[c("Home", "Date")],mean)
fin_goalconceded_a <- tapply(FIN$HG, FIN[c("Away", "Date")],mean)
fin_goalconceded_h[is.na(fin_goalconceded_h)] <- ""
fin_goalconceded_a[is.na(fin_goalconceded_a)] <- ""

for(fin_rowhgc in 1:nrow(fin_goalconceded_h)) {
  for(fin_colhgc in 1:ncol(fin_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(fin_rowagc in 1:nrow(fin_goalconceded_a)) {
      for(fin_colagc in 1:ncol(fin_goalconceded_a)) {
        ifelse(!fin_goalconceded_a[fin_rowagc,fin_colagc]=="",fin_goalconceded_h[fin_rowagc,fin_colagc] <- fin_goalconceded_a[fin_rowagc,fin_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fin_goalconceded_h,'FIN.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
fin_form_h <- tapply(FIN$FTR, FIN[c("Home", "Date")],median)
fin_form_a <- tapply(FIN$FTR, FIN[c("Away", "Date")],median)
fin_form_h[is.na(fin_form_h)] <- ""
fin_form_a[is.na(fin_form_a)] <- ""
fin_form_h <- sub("A","L",fin_form_h)
fin_form_h <- sub("H","W",fin_form_h)
fin_form_a <- sub("A","W",fin_form_a)
fin_form_a <- sub("H","L",fin_form_a)
for(fin_rowh_f in 1:nrow(fin_form_h)) {
  for(fin_colh_f in 1:ncol(fin_form_h)) {

    # print(my_matrix[row, col])
    for(fin_rowa_f in 1:nrow(fin_form_a)) {
      for(fin_cola_f in 1:ncol(fin_form_a)) {
        ifelse(!fin_form_a[fin_rowa_f,fin_cola_f]=="",fin_form_h[fin_rowa_f,fin_cola_f] <- fin_form_a[fin_rowa_f,fin_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fin_form_h,'FIN.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
fin_totalgoals_h <- tapply(FIN$TG, FIN[c("Home", "Date")],mean)
fin_totalgoals_a <- tapply(FIN$TG, FIN[c("Away", "Date")],mean)
fin_totalgoals_h[is.na(fin_totalgoals_h)] <- ""
fin_totalgoals_a[is.na(fin_totalgoals_a)] <- ""
for(fin_rowh in 1:nrow(fin_totalgoals_h)) {
  for(fin_colh in 1:ncol(fin_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(fin_rowa in 1:nrow(fin_totalgoals_a)) {
      for(fin_cola in 1:ncol(fin_totalgoals_a)) {
        ifelse(!fin_totalgoals_a[fin_rowa,fin_cola]=="",fin_totalgoals_h[fin_rowa,fin_cola] <- fin_totalgoals_a[fin_rowa,fin_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fin_totalgoals_h,'FIN.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
fin_form_team_against_h <- tapply(FIN$Away, FIN[c("Home", "Date")],median)
fin_form_team_against_a <- tapply(FIN$Home, FIN[c("Away", "Date")],median)
fin_form_team_against_h[is.na(fin_form_team_against_h)] <- ""
fin_form_team_against_a[is.na(fin_form_team_against_a)] <- ""
for(fin_rowh_f_against in 1:nrow(fin_form_team_against_h)) {
  for(fin_colh_f_against in 1:ncol(fin_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(fin_rowa_f_against in 1:nrow(fin_form_team_against_a)) {
      for(fin_cola_f_against in 1:ncol(fin_form_team_against_a)) {
        ifelse(!fin_form_team_against_a[fin_rowa_f_against,fin_cola_f_against]=="",fin_form_team_against_h[fin_rowa_f_against,fin_cola_f_against] <- fin_form_team_against_a[fin_rowa_f_against,fin_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#FIN
fin_un05_home <- c()
fin_un05_away <- c()
fin_ov05_home <- c()
fin_ov05_away <- c()

fin_un15_home <- c()
fin_un15_away <- c()
fin_ov15_home <- c()
fin_ov15_away <- c()

fin_un25_home <- c()
fin_un25_away <- c()
fin_ov25_home <- c()
fin_ov25_away <- c()

fin_un35_home <- c()
fin_un35_away <- c()
fin_ov35_home <- c()
fin_ov35_away <- c()

fin_un45_home <- c()
fin_un45_away <- c()
fin_ov45_home <- c()
fin_ov45_away <- c()

fin_un55_home <- c()
fin_un55_away <- c()
fin_ov55_home <- c()
fin_ov55_away <- c()

for (i_fin_tg in 1:length(fin_teams))
{

  fin_un05_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG == 0,])
  fin_un05_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG == 0,])

  fin_ov05_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG > 0,])
  fin_ov05_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG > 0,])

  fin_un15_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG <= 1,])
  fin_un15_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG <= 1,])

  fin_ov15_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG >= 2,])
  fin_ov15_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG >= 2,])

  fin_un25_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG <= 2,])
  fin_un25_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG <= 2,])

  fin_ov25_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG >=3,])
  fin_ov25_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG >=3,])

  fin_un35_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG <= 3,])
  fin_un35_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG <= 3,])

  fin_ov35_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG >= 4,])
  fin_ov35_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG >= 4,])

  fin_un45_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG <= 4,])
  fin_un45_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG <= 4,])

  fin_ov45_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG >= 5,])
  fin_ov45_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG >= 5,])

  fin_un55_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG <= 5,])
  fin_un55_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG <= 5,])

  fin_ov55_home[i_fin_tg] <- nrow(FIN[FIN$Home == fin_teams[i_fin_tg] & FIN$TG >= 6,])
  fin_ov55_away[i_fin_tg] <- nrow(FIN[FIN$Away == fin_teams[i_fin_tg] & FIN$TG >= 6,])


}

fin_un05 <- fin_un05_home + fin_un05_away
fin_ov05 <- fin_ov05_home + fin_ov05_away

fin_un15 <- fin_un15_home + fin_un15_away
fin_ov15 <- fin_ov15_home + fin_ov15_away

fin_un25 <- fin_un25_home + fin_un25_away
fin_ov25 <- fin_ov25_home + fin_ov25_away

fin_un35 <- fin_un35_home + fin_un35_away
fin_ov35 <- fin_ov35_home + fin_ov35_away

fin_un45 <- fin_un45_home + fin_un45_away
fin_ov45 <- fin_ov45_home + fin_ov45_away

fin_un55 <- fin_un55_home + fin_un55_away
fin_ov55 <- fin_ov55_home + fin_ov55_away

fin_ovundata <- cbind(fin_teams,fin_un05,fin_ov05,fin_un15,fin_ov15,fin_un25,fin_ov25,fin_un35,fin_ov35,fin_un45,fin_ov45,fin_un55,fin_ov55)
write.xlsx(fin_ovundata,'FIN.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
fin_csform_h <- tapply(FIN$CS, FIN[c("Home", "Date")],median)
fin_csform_a <- tapply(FIN$CS, FIN[c("Away", "Date")],median)

fin_csform_h[is.na(fin_csform_h)] <- ""
fin_csform_a[is.na(fin_csform_a)] <- ""

for(fin_rowh_f_cs in 1:nrow(fin_csform_h)) {
  for(fin_colh_f_cs in 1:ncol(fin_csform_h)) {

    # print(my_matrix[row, col])
    for(fin_rowa_f_cs in 1:nrow(fin_csform_a)) {
      for(fin_cola_f_cs in 1:ncol(fin_csform_a)) {
        ifelse(!fin_csform_a[fin_rowa_f_cs,fin_cola_f_cs]=="",fin_csform_h[fin_rowa_f_cs,fin_cola_f_cs] <- fin_csform_a[fin_rowa_f_cs,fin_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
fin_home_gs <- aggregate(FIN$HG, by = list(FIN$Home), FUN = sum)
fin_home_gs_avg <- aggregate(FIN$HG, by = list(FIN$Home),mean)
fin_home_scoring <- merge(fin_home_gs,fin_home_gs_avg, by='Group.1',all = T)
names(fin_home_scoring)[names(fin_home_scoring) == "x.x"] <- "TFthg"
names(fin_home_scoring)[names(fin_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
fin_away_gs <- aggregate(FIN$AG, by = list(FIN$Away), FUN = sum)
fin_away_gs_avg <- aggregate(FIN$AG, by = list(FIN$Away),mean)
fin_away_scoring <- merge(fin_away_gs,fin_away_gs_avg, by='Group.1',all = T)
names(fin_away_scoring)[names(fin_away_scoring) == "x.x"] <- "TFtag"
names(fin_away_scoring)[names(fin_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
fin_scoring <- merge(fin_home_scoring,fin_away_scoring,by='Group.1',all = T)
fin_scoring$TGS <- fin_scoring$TFthg + fin_scoring$TFtag

#home goals conceded
fin_home_gc <- aggregate(FIN$AG, by = list(FIN$Home), FUN = sum)
fin_home_gc_avg <- aggregate(FIN$AG, by = list(FIN$Home),mean)
fin_home_conceding <- merge(fin_home_gc,fin_home_gc_avg, by='Group.1',all = T)
names(fin_home_conceding)[names(fin_home_conceding) == "x.x"] <- "TFthc"
names(fin_home_conceding)[names(fin_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
fin_away_gc <- aggregate(FIN$HG, by = list(FIN$Away), FUN = sum)
fin_away_gc_avg <- aggregate(FIN$HG, by = list(FIN$Away),mean)
fin_away_conceding <- merge(fin_away_gc,fin_away_gc_avg, by='Group.1',all = T)
names(fin_away_conceding)[names(fin_away_conceding) == "x.x"] <- "TFtac"
names(fin_away_conceding)[names(fin_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
fin_conceding <- merge(fin_home_conceding,fin_away_conceding,by='Group.1',all = T)
fin_conceding$TGC <- fin_conceding$TFthc + fin_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
fin_home_wins <- c()
fin_away_wins <- c()
fin_home_draws <- c()
fin_away_draws <- c()
fin_home_loss <- c()
fin_away_loss <- c()



for (i_fin_wins in 1:length(fin_teams))
{

  fin_home_wins[i_fin_wins] <- nrow(FIN[FIN$Home == fin_teams[i_fin_wins] & FIN$FTR == "H",])
  fin_away_wins[i_fin_wins] <- nrow(FIN[FIN$Away == fin_teams[i_fin_wins] & FIN$FTR == "A",])
  fin_home_draws[i_fin_wins] <- nrow(FIN[FIN$Home == fin_teams[i_fin_wins] & FIN$FTR == "D",])
  fin_away_draws[i_fin_wins] <- nrow(FIN[FIN$Away == fin_teams[i_fin_wins] & FIN$FTR == "D",])
  fin_home_loss[i_fin_wins] <- nrow(FIN[FIN$Home == fin_teams[i_fin_wins] & FIN$FTR == "A",])
  fin_away_loss[i_fin_wins] <- nrow(FIN[FIN$Away == fin_teams[i_fin_wins] & FIN$FTR == "H",])

}

fin_total_wins <- fin_home_wins + fin_away_wins
fin_total_draws <- fin_home_draws + fin_away_draws
fin_total_loss <- fin_home_loss + fin_away_loss

fin_league_table <- cbind(fin_teams,fin_games_played,fin_total_wins,fin_total_draws,fin_total_loss)
fin_GS <- fin_scoring$TGS
fin_GC <-fin_conceding$TGC
fin_GD <- fin_scoring$TGS - fin_conceding$TGC
fin_PTS <- (fin_total_wins*3) + (fin_total_draws*1)
fin_league_table <- cbind(fin_league_table,fin_GS,fin_GC,fin_GD,fin_PTS)
fin_league_table <- as.data.frame(fin_league_table)
#rename the columns
names(fin_league_table)[names(fin_league_table) == "fin_teams"] <- "Team"
names(fin_league_table)[names(fin_league_table) == "fin_games_played"] <- "P"
names(fin_league_table)[names(fin_league_table) == "fin_total_wins"] <- "W"
names(fin_league_table)[names(fin_league_table) == "fin_total_draws"] <- "D"
names(fin_league_table)[names(fin_league_table) == "fin_total_loss"] <- "L"
names(fin_league_table)[names(fin_league_table) == "fin_GS"] <- "F"
names(fin_league_table)[names(fin_league_table) == "fin_GC"] <- "A"
points_fin <- fin_league_table[order(as.numeric(fin_league_table$fin_PTS), decreasing = TRUE),]
points_fin$fin_rank <- 1:length(fin_teams)
row.names(points_fin) <- points_fin$fin_rank
#create final_fin_hf_against with team ranks in brackets
for(fin_rowhrank in 1:nrow(fin_form_team_against_h)) {
  for(fin_colhrank in 1:ncol(fin_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!fin_form_team_against_h[fin_rowhrank,fin_colhrank]=="",fin_form_team_against_h[fin_rowhrank,fin_colhrank] <- paste(fin_form_team_against_h[fin_rowhrank,fin_colhrank],"(",points_fin$fin_rank[points_fin$Team ==fin_form_team_against_h[fin_rowhrank,fin_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_fin,'FIN.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six fin###################################################
#FIN
#form
#create final_fin_hf object
fin_last_n_games <- 6
final_fin_hf <- c()
for(index_fin_hf in 1:length(fin_teams))
{
  index_fin_hf <- row.names(fin_form_h) == fin_teams[index_fin_hf]
  form_fin_hf <- fin_form_h[index_fin_hf]
  deleted_form_fin_hf <- form_fin_hf[!form_fin_hf[] == ""]
  l6_form_fin_hf <- tail(deleted_form_fin_hf,fin_last_n_games)
  l6_form_fin_hf <- paste(l6_form_fin_hf,collapse = " ")
  final_fin_hf[index_fin_hf] <- rbind(paste(fin_teams[index_fin_hf],l6_form_fin_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fin_teams[index],l6_form)

}

#change column names
final_fin_hf <- as.data.frame(final_fin_hf)
colnames(final_fin_hf) <- "Form"
#goals scored
#create final_fin_gs object
final_fin_gs <- c()
suml6_fin_gs <- c()
for(index_fin_gs in 1:length(fin_teams))
{
  index_fin_gs <- row.names(fin_goalscored_h) == fin_teams[index_fin_gs]
  form_fin_gs <- fin_goalscored_h[index_fin_gs]
  deleted_form_fin_gs <- form_fin_gs[!form_fin_gs[] == ""]
  l6_form_fin_gs <- tail(deleted_form_fin_gs,fin_last_n_games)
  l6_form_fin_gs <- as.numeric(l6_form_fin_gs)
  suml6_fin_gs[index_fin_gs] <- sum(l6_form_fin_gs)
  suml6_fin_gs[index_fin_gs] <- paste("(",suml6_fin_gs[index_fin_gs],")",sep = "")
  l6_form_fin_gs <- paste(l6_form_fin_gs,collapse = " ")
  final_fin_gs[index_fin_gs] <- rbind(paste(fin_teams[index_fin_gs],l6_form_fin_gs,suml6_fin_gs[index_fin_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fin_teams[index],l6_form)

}
final_fin_gs
#change column names
final_fin_gs <- as.data.frame(final_fin_gs)
colnames(final_fin_gs) <- "Goals scored"
#goal conceded
#create final_fin_gc object
final_fin_gc <- c()
suml6_fin_gc <- c()
for(index_fin_gc in 1:length(fin_teams))
{
  index_fin_gc <- row.names(fin_goalconceded_h) == fin_teams[index_fin_gc]
  form_fin_gc <- fin_goalconceded_h[index_fin_gc]
  deleted_form_fin_gc <- form_fin_gc[!form_fin_gc[] == ""]
  l6_form_fin_gc <- tail(deleted_form_fin_gc,fin_last_n_games)
  l6_form_fin_gc <- as.numeric(l6_form_fin_gc)
  suml6_fin_gc[index_fin_gc] <- sum(l6_form_fin_gc)
  suml6_fin_gc[index_fin_gc] <- paste("(",suml6_fin_gc[index_fin_gc],")",sep = "")
  l6_form_fin_gc <- paste(l6_form_fin_gc,collapse = " ")
  final_fin_gc[index_fin_gc] <- rbind(paste(fin_teams[index_fin_gc],l6_form_fin_gc,suml6_fin_gc[index_fin_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fin_teams[index],l6_form)

}

#change column names
final_fin_gc <- as.data.frame(final_fin_gc)
colnames(final_fin_gc) <- "Goals conceded"
#total goals
#create final_fin_tg object
final_fin_tg <- c()
suml6_fin_tg <- c()
for(index_fin_tg in 1:length(fin_teams))
{
  index_fin_tg <- row.names(fin_totalgoals_h) == fin_teams[index_fin_tg]
  form_fin_tg <- fin_totalgoals_h[index_fin_tg]
  deleted_form_fin_tg <- form_fin_tg[!form_fin_tg[] == ""]
  l6_form_fin_tg <- tail(deleted_form_fin_tg,fin_last_n_games)
  l6_form_fin_tg <- as.numeric(l6_form_fin_tg)
  suml6_fin_tg[index_fin_tg] <- sum(l6_form_fin_tg)
  suml6_fin_tg[index_fin_tg] <- paste("(",suml6_fin_tg[index_fin_tg],")",sep = "")
  l6_form_fin_tg <- paste(l6_form_fin_tg,collapse = " ")
  final_fin_tg[index_fin_tg] <- rbind(paste(fin_teams[index_fin_tg],l6_form_fin_tg,suml6_fin_tg[index_fin_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fin_teams[index],l6_form)

}
#change column names
final_fin_tg <- as.data.frame(final_fin_tg)
colnames(final_fin_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_fin_hf object
final_fin_cs <- c()
for(index_fin_cs in 1:length(fin_teams))
{
  index_fin_cs <- row.names(fin_csform_h) == fin_teams[index_fin_cs]
  csform_fin_cs <- fin_csform_h[index_fin_cs]
  deleted_csform_fin_cs <- csform_fin_cs[!csform_fin_cs[] == ""]
  l6_csform_fin_cs <- tail(deleted_csform_fin_cs,fin_last_n_games)
  l6_csform_fin_cs <- paste(l6_csform_fin_cs,collapse = " ")
  final_fin_cs[index_fin_cs] <- rbind(paste(fin_teams[index_fin_cs],l6_csform_fin_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",fin_teams[index],l6_csform)

}

#change column names
final_fin_cs <- as.data.frame(final_fin_cs)
colnames(final_fin_cs) <- "CSForm"
#################################################
#Team against
#create final_fin_hf_against
final_fin_hf_against <- c()
for(index_fin_hf_against in 1:length(fin_teams))
{
  index_fin_hf_against <- row.names(fin_form_team_against_h) == fin_teams[index_fin_hf_against]
  form_fin_hf_against <- fin_form_team_against_h[index_fin_hf_against]
  deleted_form_fin_hf_against <- form_fin_hf_against[!form_fin_hf_against[] == ""]
  l6_form_fin_hf_against <- tail(deleted_form_fin_hf_against,fin_last_n_games)
  l6_form_fin_hf_against <- paste(l6_form_fin_hf_against,collapse = " ")
  final_fin_hf_against[index_fin_hf_against] <- rbind(paste(fin_teams[index_fin_hf_against],l6_form_fin_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fin_teams[index],l6_form)

}
final_fin_hf_against <- as.data.frame(final_fin_hf_against)
colnames(final_fin_hf_against) <- "Team against"
#combine the columns
final_fin_all <- cbind(final_fin_hf,final_fin_gs,final_fin_gc,final_fin_tg,final_fin_cs,final_fin_hf_against)
write.xlsx(final_fin_all,'FIN.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
fin_GP <- nrow(FIN)
#Calculate total home goals for each division
fin_T_HG <- sum(fin_home_gs$x)
#calculate average home goal
fin_avg_HG <- round(fin_T_HG /fin_GP, digits = 4)
############################################################
#Calculate total away goals for each division
fin_T_AG <- sum(fin_away_gs$x)
#calculate average away goal
fin_avg_AG <- round(fin_T_AG /fin_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
fin_home_as <- round(((fin_home_gs$x/fin_home_games))/fin_avg_HG, digits = 4)
#calculate away attack strength
fin_away_as <- round(((fin_away_gs$x/fin_away_games))/fin_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
fin_avg_HC <- round(fin_T_AG /fin_GP, digits = 4)
#avg away concede
fin_avg_AC <- round(fin_T_HG /fin_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
fin_home_ds <- round(((fin_home_gc$x/fin_home_games))/fin_avg_HC, digits = 4)
#away defense strength
fin_away_ds <- round(((fin_away_gc$x/fin_away_games))/fin_avg_AC, digits = 4)
#############################################################################
#home poisson data
#fin
fin_division <- c()
fin_division[1:length(fin_teams)] <- "FIN"
fin_home_poisson <- cbind(fin_division,fin_teams,fin_avg_HG,fin_home_as,fin_home_ds)
#################################################################################
#away poisson data
#fin
fin_division <- c()
fin_division[1:length(fin_teams)] <- "FIN"
fin_away_poisson <- cbind(fin_division,fin_teams,fin_avg_AG,fin_away_as,fin_away_ds)

#create home and away csv
#fin_home_poisson <- rbind(fin_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#fin_away_poisson <- rbind(fin_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(fin_home_poisson,'FIN.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(fin_away_poisson,'FIN.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################FIN FIXTURES##########################################################################
#FIN
HomeTeam_fin <- rep(fin_teams, each = length(fin_teams))
AwayTeam_fin <- rep(fin_teams, length(fin_teams))
FIN_fixtures <- cbind(HomeTeam_fin,AwayTeam_fin)
FIN_fixtures <- as.data.frame(FIN_fixtures)
FIN_fixtures <- FIN_fixtures[!FIN_fixtures$HomeTeam_fin == FIN_fixtures$AwayTeam_fin,]
rownames(FIN_fixtures) <- NULL
FIN_fixtures$Div <- "FIN"
FIN_fixtures <- FIN_fixtures[,c(3,1,2)]

FIN_fixtures$avg_HG_fin <- fin_avg_HG

FIN_fixtures$fin_homeas <- rep(fin_home_as,each = length(fin_teams)-1)

fin_awayds_lookup <- cbind(fin_teams,fin_away_ds)

fin_awayds_lookup <- as.data.frame(fin_awayds_lookup)

colnames(fin_awayds_lookup) <- c("AwayTeam_fin","fin_awayds")


require('RH2')
FIN_fixtures$fin_awayds <- sqldf("SELECT fin_awayds_lookup.fin_awayds FROM fin_awayds_lookup INNER JOIN FIN_fixtures ON fin_awayds_lookup.AwayTeam_fin = FIN_fixtures.AwayTeam_fin")

FIN_fixtures$avg_AG_fin <- fin_avg_AG

fin_awayas_lookup <- cbind(fin_teams,fin_away_as)

fin_awayas_lookup <- as.data.frame(fin_awayas_lookup)

colnames(fin_awayas_lookup) <- c("AwayTeam_fin","fin_awayas")


FIN_fixtures$fin_awayas <- sqldf("SELECT fin_awayas_lookup.fin_awayas FROM fin_awayas_lookup INNER JOIN FIN_fixtures ON fin_awayas_lookup.AwayTeam_fin = FIN_fixtures.AwayTeam_fin")

FIN_fixtures$fin_homeds <- rep(fin_home_ds,each = length(fin_teams)-1)

FIN_fixtures$fin_awayds <- as.numeric(unlist(FIN_fixtures$fin_awayds))
#xGH
FIN_fixtures$fin_xGH <- FIN_fixtures$avg_HG_fin * FIN_fixtures$fin_homeas * FIN_fixtures$fin_awayds

#xGA

FIN_fixtures$fin_awayas <- as.numeric(unlist(FIN_fixtures$fin_awayas))

FIN_fixtures$fin_xGA <- FIN_fixtures$avg_AG_fin * FIN_fixtures$fin_awayas * FIN_fixtures$fin_homeds

FIN_fixtures$fin_0_0 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_0 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_0_1 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_1 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_0 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_0_2 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_2 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_1 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_2 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_3 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_0 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_1 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_2 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_0_3 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_3 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_3 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_4 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_0 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_1 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_2 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_3 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_0_4 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_4 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_4 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_4 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_5 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_0 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_1 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_2 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_3 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_4 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_0_5 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_5 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_5 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_5 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_5 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_6 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_0 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(0,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_1 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(1,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_2 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(2,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_3 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(3,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_4 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(4,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_6_5 <- round(stats::dpois(6,FIN_fixtures$fin_xGH) * stats::dpois(5,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_0_6 <- round(stats::dpois(0,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_1_6 <- round(stats::dpois(1,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_2_6 <- round(stats::dpois(2,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_3_6 <- round(stats::dpois(3,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_4_6 <- round(stats::dpois(4,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
FIN_fixtures$fin_5_6 <- round(stats::dpois(5,FIN_fixtures$fin_xGH) * stats::dpois(6,FIN_fixtures$fin_xGA), digits = 4)
#Home win
FIN_fixtures$fin_H <- (
  FIN_fixtures$fin_1_0 + FIN_fixtures$fin_2_0 + FIN_fixtures$fin_2_1 + FIN_fixtures$fin_3_0 + FIN_fixtures$fin_3_1 +
    FIN_fixtures$fin_3_2 + FIN_fixtures$fin_4_0 + FIN_fixtures$fin_4_1 + FIN_fixtures$fin_4_2 + FIN_fixtures$fin_4_3 +
    FIN_fixtures$fin_5_0 + FIN_fixtures$fin_5_1 + FIN_fixtures$fin_5_2 + FIN_fixtures$fin_5_3 + FIN_fixtures$fin_5_4 +
    FIN_fixtures$fin_6_0 + FIN_fixtures$fin_6_1 + FIN_fixtures$fin_6_2 + FIN_fixtures$fin_6_3 + FIN_fixtures$fin_6_4 +
    FIN_fixtures$fin_6_5
)

FIN_fixtures$fin_H <- percent(FIN_fixtures$fin_H, accuracy = 0.1)

#Draw
FIN_fixtures$fin_D <- (

  FIN_fixtures$fin_0_0 + FIN_fixtures$fin_1_1 + FIN_fixtures$fin_2_2 + FIN_fixtures$fin_3_3 + FIN_fixtures$fin_4_4 +
    FIN_fixtures$fin_5_5 + FIN_fixtures$fin_6_6
)

FIN_fixtures$fin_D <- percent(FIN_fixtures$fin_D, accuracy = 0.1)

#Away

FIN_fixtures$fin_A <- (
  FIN_fixtures$fin_0_1 + FIN_fixtures$fin_0_2 + FIN_fixtures$fin_1_2 + FIN_fixtures$fin_0_3 + FIN_fixtures$fin_1_3 +
    FIN_fixtures$fin_2_3 + FIN_fixtures$fin_0_4 + FIN_fixtures$fin_1_4 + FIN_fixtures$fin_2_4 + FIN_fixtures$fin_3_4 +
    FIN_fixtures$fin_0_5 + FIN_fixtures$fin_1_5 + FIN_fixtures$fin_2_5 + FIN_fixtures$fin_3_5 + FIN_fixtures$fin_4_5 +
    FIN_fixtures$fin_0_6 + FIN_fixtures$fin_1_6 + FIN_fixtures$fin_2_6 + FIN_fixtures$fin_3_6 + FIN_fixtures$fin_4_6 +
    FIN_fixtures$fin_5_6
)

FIN_fixtures$fin_A <- percent(FIN_fixtures$fin_A, accuracy = 0.1)

#ov25
FIN_fixtures$fin_ov25 <- (
  FIN_fixtures$fin_2_1 + FIN_fixtures$fin_1_2 + FIN_fixtures$fin_2_2 + FIN_fixtures$fin_3_0 + FIN_fixtures$fin_3_1 +
    FIN_fixtures$fin_3_2 + FIN_fixtures$fin_0_3 + FIN_fixtures$fin_1_3 + FIN_fixtures$fin_2_3 + FIN_fixtures$fin_3_3 +
    FIN_fixtures$fin_4_0 + FIN_fixtures$fin_4_1 + FIN_fixtures$fin_4_2 + FIN_fixtures$fin_4_3 + FIN_fixtures$fin_0_4 +
    FIN_fixtures$fin_1_4 + FIN_fixtures$fin_2_4 + FIN_fixtures$fin_3_4 + FIN_fixtures$fin_4_4 + FIN_fixtures$fin_5_0 +
    FIN_fixtures$fin_5_1 + FIN_fixtures$fin_5_2 + FIN_fixtures$fin_5_3 + FIN_fixtures$fin_5_4 + FIN_fixtures$fin_0_5 +
    FIN_fixtures$fin_1_5 + FIN_fixtures$fin_2_5 + FIN_fixtures$fin_3_5 + FIN_fixtures$fin_4_5 + FIN_fixtures$fin_5_5 +
    FIN_fixtures$fin_6_0 + FIN_fixtures$fin_6_1 + FIN_fixtures$fin_6_2 + FIN_fixtures$fin_6_3 + FIN_fixtures$fin_6_4 +
    FIN_fixtures$fin_6_5 + FIN_fixtures$fin_0_6 + FIN_fixtures$fin_1_6 + FIN_fixtures$fin_2_6 + FIN_fixtures$fin_3_6 +
    FIN_fixtures$fin_4_6 + FIN_fixtures$fin_5_6 + FIN_fixtures$fin_6_6
)
#un25
FIN_fixtures$fin_un25 <- (
  FIN_fixtures$fin_0_0 + FIN_fixtures$fin_1_0 + FIN_fixtures$fin_0_1 + FIN_fixtures$fin_1_1 + FIN_fixtures$fin_2_0 + FIN_fixtures$fin_0_2
)
#odds
FIN_fixtures$fin_ov25_odds <- round((1/FIN_fixtures$fin_ov25),digits = 2)
FIN_fixtures$fin_un25_odds <- round((1/FIN_fixtures$fin_un25),digits = 2)

FIN_fixtures$fin_ov25_odds
FIN_fixtures$fin_un25_odds
#percentages
FIN_fixtures$fin_ov25 <- percent(FIN_fixtures$fin_ov25, accuracy = 0.1)

FIN_fixtures$fin_un25 <- percent(FIN_fixtures$fin_un25, accuracy = 0.1)
FIN_fixtures$fin_pscore <- paste(round(FIN_fixtures$fin_xGH,digits = 0),round(FIN_fixtures$fin_xGA,digits = 0),sep = "-")
#write out
write.xlsx(FIN_fixtures,'FIN.xlsx',sheetName = "FIN", append = TRUE)
###########################################################################################################
########################FIN END###########################################################################
FIN <- read.csv('../FDAS/FIN.csv')
FIN$TG <- FIN$HG + FIN$AG
FIN$OV25 <- ifelse(FIN$TG >= 3,"Y","N")
fin_ftr_summary <- tabyl(FIN,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
fin_ov25_summary <- tabyl(FIN,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(fin_ftr_summary,'FIN.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(fin_ov25_summary,'FIN.xlsx',sheetName = "OVUN25", append = TRUE)



