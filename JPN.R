library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('JPN.xlsx')
######################JPN START#######################################
#####################################################################
JPN <- read.csv('../FDAS/JPN.csv')
JPN <- within(JPN,rm(Res))
JPN$Date <- dmy(JPN$Date)
JPN <- JPN[order(as.Date(JPN$Date, format = "%d/%m%Y"), decreasing = FALSE),]
JPN$CS <- paste(JPN$HG,JPN$AG, sep = "-")
#JPN_qualificaton <- subset(JPN,tournament == "UEFA Euro qualification")
JPN <- subset(JPN,Season == "2021")
#JPN <- JPN[JPN$Date > '2008-01-01',])
JPN$TG <- JPN$HG + JPN$AG
JPN$OV25 <- ifelse(JPN$TG >= 3,"Y","N")
JPN$FTR <- with(JPN,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
jpn_totalgoalsv2 <- tapply(JPN$TG, JPN[c("Home", "Away")],mean)
jpn_totalgoalsv2
jpn_hgtotals <- rowSums(jpn_totalgoalsv2,na.rm = T)
jpn_agtotals <- colSums(jpn_totalgoalsv2,na.rm = T)

jpn_totalgoals <- jpn_hgtotals + jpn_agtotals
jpn_totalgoalsv2 <- cbind(jpn_totalgoalsv2,jpn_totalgoals)
jpn_teams <- sort(unique(JPN$Home))
jpn_home_games <- c()
jpn_away_games <-c()
for (i_jpn in 1:length(jpn_teams))
{

  jpn_home_games[i_jpn] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn],])
  jpn_away_games[i_jpn]  <- nrow(JPN[JPN$Away == jpn_teams[i_jpn],])

}
jpn_games_played <- jpn_home_games + jpn_away_games
jpn_goaltotalsv2 <- cbind(jpn_totalgoalsv2,jpn_games_played)
jpn_avg_totalgoals <- round((jpn_totalgoals/ jpn_games_played), digits = 4)
jpn_goaltotalsv2[is.na(jpn_goaltotalsv2)] <- ""
jpn_goaltotalsv2 <- cbind(jpn_goaltotalsv2,jpn_avg_totalgoals)
write.xlsx(jpn_goaltotalsv2,'JPN.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
jpn_goalscored_h <- tapply(JPN$HG, JPN[c("Home", "Date")],mean)
jpn_goalscored_a <- tapply(JPN$AG, JPN[c("Away", "Date")],mean)
jpn_goalscored_h[is.na(jpn_goalscored_h)] <- ""
jpn_goalscored_a[is.na(jpn_goalscored_a)] <- ""

for(jpn_rowhgs in 1:nrow(jpn_goalscored_h)) {
  for(jpn_colhgs in 1:ncol(jpn_goalscored_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowags in 1:nrow(jpn_goalscored_a)) {
      for(jpn_colags in 1:ncol(jpn_goalscored_a)) {
        ifelse(!jpn_goalscored_a[jpn_rowags,jpn_colags]=="",jpn_goalscored_h[jpn_rowags,jpn_colags] <- jpn_goalscored_a[jpn_rowags,jpn_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(jpn_goalscored_h,'JPN.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
jpn_goalconceded_h <- tapply(JPN$AG, JPN[c("Home", "Date")],mean)
jpn_goalconceded_a <- tapply(JPN$HG, JPN[c("Away", "Date")],mean)
jpn_goalconceded_h[is.na(jpn_goalconceded_h)] <- ""
jpn_goalconceded_a[is.na(jpn_goalconceded_a)] <- ""

for(jpn_rowhgc in 1:nrow(jpn_goalconceded_h)) {
  for(jpn_colhgc in 1:ncol(jpn_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowagc in 1:nrow(jpn_goalconceded_a)) {
      for(jpn_colagc in 1:ncol(jpn_goalconceded_a)) {
        ifelse(!jpn_goalconceded_a[jpn_rowagc,jpn_colagc]=="",jpn_goalconceded_h[jpn_rowagc,jpn_colagc] <- jpn_goalconceded_a[jpn_rowagc,jpn_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(jpn_goalconceded_h,'JPN.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
jpn_form_h <- tapply(JPN$FTR, JPN[c("Home", "Date")],median)
jpn_form_a <- tapply(JPN$FTR, JPN[c("Away", "Date")],median)
jpn_form_h[is.na(jpn_form_h)] <- ""
jpn_form_a[is.na(jpn_form_a)] <- ""
jpn_form_h <- sub("A","L",jpn_form_h)
jpn_form_h <- sub("H","W",jpn_form_h)
jpn_form_a <- sub("A","W",jpn_form_a)
jpn_form_a <- sub("H","L",jpn_form_a)
for(jpn_rowh_f in 1:nrow(jpn_form_h)) {
  for(jpn_colh_f in 1:ncol(jpn_form_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa_f in 1:nrow(jpn_form_a)) {
      for(jpn_cola_f in 1:ncol(jpn_form_a)) {
        ifelse(!jpn_form_a[jpn_rowa_f,jpn_cola_f]=="",jpn_form_h[jpn_rowa_f,jpn_cola_f] <- jpn_form_a[jpn_rowa_f,jpn_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(jpn_form_h,'JPN.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
jpn_totalgoals_h <- tapply(JPN$TG, JPN[c("Home", "Date")],mean)
jpn_totalgoals_a <- tapply(JPN$TG, JPN[c("Away", "Date")],mean)
jpn_totalgoals_h[is.na(jpn_totalgoals_h)] <- ""
jpn_totalgoals_a[is.na(jpn_totalgoals_a)] <- ""
for(jpn_rowh in 1:nrow(jpn_totalgoals_h)) {
  for(jpn_colh in 1:ncol(jpn_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa in 1:nrow(jpn_totalgoals_a)) {
      for(jpn_cola in 1:ncol(jpn_totalgoals_a)) {
        ifelse(!jpn_totalgoals_a[jpn_rowa,jpn_cola]=="",jpn_totalgoals_h[jpn_rowa,jpn_cola] <- jpn_totalgoals_a[jpn_rowa,jpn_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(jpn_totalgoals_h,'JPN.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
jpn_form_team_against_h <- tapply(JPN$Away, JPN[c("Home", "Date")],median)
jpn_form_team_against_a <- tapply(JPN$Home, JPN[c("Away", "Date")],median)
jpn_form_team_against_h[is.na(jpn_form_team_against_h)] <- ""
jpn_form_team_against_a[is.na(jpn_form_team_against_a)] <- ""
for(jpn_rowh_f_against in 1:nrow(jpn_form_team_against_h)) {
  for(jpn_colh_f_against in 1:ncol(jpn_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa_f_against in 1:nrow(jpn_form_team_against_a)) {
      for(jpn_cola_f_against in 1:ncol(jpn_form_team_against_a)) {
        ifelse(!jpn_form_team_against_a[jpn_rowa_f_against,jpn_cola_f_against]=="",jpn_form_team_against_h[jpn_rowa_f_against,jpn_cola_f_against] <- jpn_form_team_against_a[jpn_rowa_f_against,jpn_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#JPN
jpn_un05_home <- c()
jpn_un05_away <- c()
jpn_ov05_home <- c()
jpn_ov05_away <- c()

jpn_un15_home <- c()
jpn_un15_away <- c()
jpn_ov15_home <- c()
jpn_ov15_away <- c()

jpn_un25_home <- c()
jpn_un25_away <- c()
jpn_ov25_home <- c()
jpn_ov25_away <- c()

jpn_un35_home <- c()
jpn_un35_away <- c()
jpn_ov35_home <- c()
jpn_ov35_away <- c()

jpn_un45_home <- c()
jpn_un45_away <- c()
jpn_ov45_home <- c()
jpn_ov45_away <- c()

jpn_un55_home <- c()
jpn_un55_away <- c()
jpn_ov55_home <- c()
jpn_ov55_away <- c()

for (i_jpn_tg in 1:length(jpn_teams))
{

  jpn_un05_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG == 0,])
  jpn_un05_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG == 0,])

  jpn_ov05_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG > 0,])
  jpn_ov05_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG > 0,])

  jpn_un15_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 1,])
  jpn_un15_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 1,])

  jpn_ov15_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 2,])
  jpn_ov15_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 2,])

  jpn_un25_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 2,])
  jpn_un25_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 2,])

  jpn_ov25_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >=3,])
  jpn_ov25_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >=3,])

  jpn_un35_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 3,])
  jpn_un35_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 3,])

  jpn_ov35_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 4,])
  jpn_ov35_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 4,])

  jpn_un45_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 4,])
  jpn_un45_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 4,])

  jpn_ov45_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 5,])
  jpn_ov45_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 5,])

  jpn_un55_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 5,])
  jpn_un55_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 5,])

  jpn_ov55_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 6,])
  jpn_ov55_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 6,])


}

jpn_un05 <- jpn_un05_home + jpn_un05_away
jpn_ov05 <- jpn_ov05_home + jpn_ov05_away

jpn_un15 <- jpn_un15_home + jpn_un15_away
jpn_ov15 <- jpn_ov15_home + jpn_ov15_away

jpn_un25 <- jpn_un25_home + jpn_un25_away
jpn_ov25 <- jpn_ov25_home + jpn_ov25_away

jpn_un35 <- jpn_un35_home + jpn_un35_away
jpn_ov35 <- jpn_ov35_home + jpn_ov35_away

jpn_un45 <- jpn_un45_home + jpn_un45_away
jpn_ov45 <- jpn_ov45_home + jpn_ov45_away

jpn_un55 <- jpn_un55_home + jpn_un55_away
jpn_ov55 <- jpn_ov55_home + jpn_ov55_away

jpn_ovundata <- cbind(jpn_teams,jpn_un05,jpn_ov05,jpn_un15,jpn_ov15,jpn_un25,jpn_ov25,jpn_un35,jpn_ov35,jpn_un45,jpn_ov45,jpn_un55,jpn_ov55)
write.xlsx(jpn_ovundata,'JPN.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
jpn_csform_h <- tapply(JPN$CS, JPN[c("Home", "Date")],median)
jpn_csform_a <- tapply(JPN$CS, JPN[c("Away", "Date")],median)

jpn_csform_h[is.na(jpn_csform_h)] <- ""
jpn_csform_a[is.na(jpn_csform_a)] <- ""

for(jpn_rowh_f_cs in 1:nrow(jpn_csform_h)) {
  for(jpn_colh_f_cs in 1:ncol(jpn_csform_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa_f_cs in 1:nrow(jpn_csform_a)) {
      for(jpn_cola_f_cs in 1:ncol(jpn_csform_a)) {
        ifelse(!jpn_csform_a[jpn_rowa_f_cs,jpn_cola_f_cs]=="",jpn_csform_h[jpn_rowa_f_cs,jpn_cola_f_cs] <- jpn_csform_a[jpn_rowa_f_cs,jpn_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
jpn_home_gs <- aggregate(JPN$HG, by = list(JPN$Home), FUN = sum)
jpn_home_gs_avg <- aggregate(JPN$HG, by = list(JPN$Home),mean)
jpn_home_scoring <- merge(jpn_home_gs,jpn_home_gs_avg, by='Group.1',all = T)
names(jpn_home_scoring)[names(jpn_home_scoring) == "x.x"] <- "TFthg"
names(jpn_home_scoring)[names(jpn_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
jpn_away_gs <- aggregate(JPN$AG, by = list(JPN$Away), FUN = sum)
jpn_away_gs_avg <- aggregate(JPN$AG, by = list(JPN$Away),mean)
jpn_away_scoring <- merge(jpn_away_gs,jpn_away_gs_avg, by='Group.1',all = T)
names(jpn_away_scoring)[names(jpn_away_scoring) == "x.x"] <- "TFtag"
names(jpn_away_scoring)[names(jpn_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
jpn_scoring <- merge(jpn_home_scoring,jpn_away_scoring,by='Group.1',all = T)
jpn_scoring$TGS <- jpn_scoring$TFthg + jpn_scoring$TFtag

#home goals conceded
jpn_home_gc <- aggregate(JPN$AG, by = list(JPN$Home), FUN = sum)
jpn_home_gc_avg <- aggregate(JPN$AG, by = list(JPN$Home),mean)
jpn_home_conceding <- merge(jpn_home_gc,jpn_home_gc_avg, by='Group.1',all = T)
names(jpn_home_conceding)[names(jpn_home_conceding) == "x.x"] <- "TFthc"
names(jpn_home_conceding)[names(jpn_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
jpn_away_gc <- aggregate(JPN$HG, by = list(JPN$Away), FUN = sum)
jpn_away_gc_avg <- aggregate(JPN$HG, by = list(JPN$Away),mean)
jpn_away_conceding <- merge(jpn_away_gc,jpn_away_gc_avg, by='Group.1',all = T)
names(jpn_away_conceding)[names(jpn_away_conceding) == "x.x"] <- "TFtac"
names(jpn_away_conceding)[names(jpn_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
jpn_conceding <- merge(jpn_home_conceding,jpn_away_conceding,by='Group.1',all = T)
jpn_conceding$TGC <- jpn_conceding$TFthc + jpn_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
jpn_home_wins <- c()
jpn_away_wins <- c()
jpn_home_draws <- c()
jpn_away_draws <- c()
jpn_home_loss <- c()
jpn_away_loss <- c()



for (i_jpn_wins in 1:length(jpn_teams))
{

  jpn_home_wins[i_jpn_wins] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_wins] & JPN$FTR == "H",])
  jpn_away_wins[i_jpn_wins] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_wins] & JPN$FTR == "A",])
  jpn_home_draws[i_jpn_wins] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_wins] & JPN$FTR == "D",])
  jpn_away_draws[i_jpn_wins] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_wins] & JPN$FTR == "D",])
  jpn_home_loss[i_jpn_wins] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_wins] & JPN$FTR == "A",])
  jpn_away_loss[i_jpn_wins] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_wins] & JPN$FTR == "H",])

}

jpn_total_wins <- jpn_home_wins + jpn_away_wins
jpn_total_draws <- jpn_home_draws + jpn_away_draws
jpn_total_loss <- jpn_home_loss + jpn_away_loss

jpn_league_table <- cbind(jpn_teams,jpn_games_played,jpn_total_wins,jpn_total_draws,jpn_total_loss)
jpn_GS <- jpn_scoring$TGS
jpn_GC <-jpn_conceding$TGC
jpn_GD <- jpn_scoring$TGS - jpn_conceding$TGC
jpn_PTS <- (jpn_total_wins*3) + (jpn_total_draws*1)
jpn_league_table <- cbind(jpn_league_table,jpn_GS,jpn_GC,jpn_GD,jpn_PTS)
jpn_league_table <- as.data.frame(jpn_league_table)
#rename the columns
names(jpn_league_table)[names(jpn_league_table) == "jpn_teams"] <- "Team"
names(jpn_league_table)[names(jpn_league_table) == "jpn_games_played"] <- "P"
names(jpn_league_table)[names(jpn_league_table) == "jpn_total_wins"] <- "W"
names(jpn_league_table)[names(jpn_league_table) == "jpn_total_draws"] <- "D"
names(jpn_league_table)[names(jpn_league_table) == "jpn_total_loss"] <- "L"
names(jpn_league_table)[names(jpn_league_table) == "jpn_GS"] <- "F"
names(jpn_league_table)[names(jpn_league_table) == "jpn_GC"] <- "A"
points_jpn <- jpn_league_table[order(as.numeric(jpn_league_table$jpn_PTS), decreasing = TRUE),]
points_jpn$jpn_rank <- 1:length(jpn_teams)
row.names(points_jpn) <- points_jpn$jpn_rank
#create final_jpn_hf_against with team ranks in brackets
for(jpn_rowhrank in 1:nrow(jpn_form_team_against_h)) {
  for(jpn_colhrank in 1:ncol(jpn_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank]=="",jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank] <- paste(jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank],"(",points_jpn$jpn_rank[points_jpn$Team ==jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_jpn,'JPN.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six jpn###################################################
#JPN
#form
#create final_jpn_hf object
jpn_last_n_games <- 6
final_jpn_hf <- c()
for(index_jpn_hf in 1:length(jpn_teams))
{
  index_jpn_hf <- row.names(jpn_form_h) == jpn_teams[index_jpn_hf]
  form_jpn_hf <- jpn_form_h[index_jpn_hf]
  deleted_form_jpn_hf <- form_jpn_hf[!form_jpn_hf[] == ""]
  l6_form_jpn_hf <- tail(deleted_form_jpn_hf,jpn_last_n_games)
  l6_form_jpn_hf <- paste(l6_form_jpn_hf,collapse = " ")
  final_jpn_hf[index_jpn_hf] <- rbind(paste(jpn_teams[index_jpn_hf],l6_form_jpn_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}

#change column names
final_jpn_hf <- as.data.frame(final_jpn_hf)
colnames(final_jpn_hf) <- "Form"
#goals scored
#create final_jpn_gs object
final_jpn_gs <- c()
suml6_jpn_gs <- c()
for(index_jpn_gs in 1:length(jpn_teams))
{
  index_jpn_gs <- row.names(jpn_goalscored_h) == jpn_teams[index_jpn_gs]
  form_jpn_gs <- jpn_goalscored_h[index_jpn_gs]
  deleted_form_jpn_gs <- form_jpn_gs[!form_jpn_gs[] == ""]
  l6_form_jpn_gs <- tail(deleted_form_jpn_gs,jpn_last_n_games)
  l6_form_jpn_gs <- as.numeric(l6_form_jpn_gs)
  suml6_jpn_gs[index_jpn_gs] <- sum(l6_form_jpn_gs)
  suml6_jpn_gs[index_jpn_gs] <- paste("(",suml6_jpn_gs[index_jpn_gs],")",sep = "")
  l6_form_jpn_gs <- paste(l6_form_jpn_gs,collapse = " ")
  final_jpn_gs[index_jpn_gs] <- rbind(paste(jpn_teams[index_jpn_gs],l6_form_jpn_gs,suml6_jpn_gs[index_jpn_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
final_jpn_gs
#change column names
final_jpn_gs <- as.data.frame(final_jpn_gs)
colnames(final_jpn_gs) <- "Goals scored"
#goal conceded
#create final_jpn_gc object
final_jpn_gc <- c()
suml6_jpn_gc <- c()
for(index_jpn_gc in 1:length(jpn_teams))
{
  index_jpn_gc <- row.names(jpn_goalconceded_h) == jpn_teams[index_jpn_gc]
  form_jpn_gc <- jpn_goalconceded_h[index_jpn_gc]
  deleted_form_jpn_gc <- form_jpn_gc[!form_jpn_gc[] == ""]
  l6_form_jpn_gc <- tail(deleted_form_jpn_gc,jpn_last_n_games)
  l6_form_jpn_gc <- as.numeric(l6_form_jpn_gc)
  suml6_jpn_gc[index_jpn_gc] <- sum(l6_form_jpn_gc)
  suml6_jpn_gc[index_jpn_gc] <- paste("(",suml6_jpn_gc[index_jpn_gc],")",sep = "")
  l6_form_jpn_gc <- paste(l6_form_jpn_gc,collapse = " ")
  final_jpn_gc[index_jpn_gc] <- rbind(paste(jpn_teams[index_jpn_gc],l6_form_jpn_gc,suml6_jpn_gc[index_jpn_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}

#change column names
final_jpn_gc <- as.data.frame(final_jpn_gc)
colnames(final_jpn_gc) <- "Goals conceded"
#total goals
#create final_jpn_tg object
final_jpn_tg <- c()
suml6_jpn_tg <- c()
for(index_jpn_tg in 1:length(jpn_teams))
{
  index_jpn_tg <- row.names(jpn_totalgoals_h) == jpn_teams[index_jpn_tg]
  form_jpn_tg <- jpn_totalgoals_h[index_jpn_tg]
  deleted_form_jpn_tg <- form_jpn_tg[!form_jpn_tg[] == ""]
  l6_form_jpn_tg <- tail(deleted_form_jpn_tg,jpn_last_n_games)
  l6_form_jpn_tg <- as.numeric(l6_form_jpn_tg)
  suml6_jpn_tg[index_jpn_tg] <- sum(l6_form_jpn_tg)
  suml6_jpn_tg[index_jpn_tg] <- paste("(",suml6_jpn_tg[index_jpn_tg],")",sep = "")
  l6_form_jpn_tg <- paste(l6_form_jpn_tg,collapse = " ")
  final_jpn_tg[index_jpn_tg] <- rbind(paste(jpn_teams[index_jpn_tg],l6_form_jpn_tg,suml6_jpn_tg[index_jpn_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
#change column names
final_jpn_tg <- as.data.frame(final_jpn_tg)
colnames(final_jpn_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_jpn_hf object
final_jpn_cs <- c()
for(index_jpn_cs in 1:length(jpn_teams))
{
  index_jpn_cs <- row.names(jpn_csform_h) == jpn_teams[index_jpn_cs]
  csform_jpn_cs <- jpn_csform_h[index_jpn_cs]
  deleted_csform_jpn_cs <- csform_jpn_cs[!csform_jpn_cs[] == ""]
  l6_csform_jpn_cs <- tail(deleted_csform_jpn_cs,jpn_last_n_games)
  l6_csform_jpn_cs <- paste(l6_csform_jpn_cs,collapse = " ")
  final_jpn_cs[index_jpn_cs] <- rbind(paste(jpn_teams[index_jpn_cs],l6_csform_jpn_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",jpn_teams[index],l6_csform)

}

#change column names
final_jpn_cs <- as.data.frame(final_jpn_cs)
colnames(final_jpn_cs) <- "CSForm"
#################################################
#Team against
#create final_jpn_hf_against
final_jpn_hf_against <- c()
for(index_jpn_hf_against in 1:length(jpn_teams))
{
  index_jpn_hf_against <- row.names(jpn_form_team_against_h) == jpn_teams[index_jpn_hf_against]
  form_jpn_hf_against <- jpn_form_team_against_h[index_jpn_hf_against]
  deleted_form_jpn_hf_against <- form_jpn_hf_against[!form_jpn_hf_against[] == ""]
  l6_form_jpn_hf_against <- tail(deleted_form_jpn_hf_against,jpn_last_n_games)
  l6_form_jpn_hf_against <- paste(l6_form_jpn_hf_against,collapse = " ")
  final_jpn_hf_against[index_jpn_hf_against] <- rbind(paste(jpn_teams[index_jpn_hf_against],l6_form_jpn_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
final_jpn_hf_against <- as.data.frame(final_jpn_hf_against)
colnames(final_jpn_hf_against) <- "Team against"
#combine the columns
final_jpn_all <- cbind(final_jpn_hf,final_jpn_gs,final_jpn_gc,final_jpn_tg,final_jpn_cs,final_jpn_hf_against)
write.xlsx(final_jpn_all,'JPN.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
jpn_GP <- nrow(JPN)
#Calculate total home goals for each division
jpn_T_HG <- sum(jpn_home_gs$x)
#calculate average home goal
jpn_avg_HG <- round(jpn_T_HG /jpn_GP, digits = 4)
############################################################
#Calculate total away goals for each division
jpn_T_AG <- sum(jpn_away_gs$x)
#calculate average away goal
jpn_avg_AG <- round(jpn_T_AG /jpn_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
jpn_home_as <- round(((jpn_home_gs$x/jpn_home_games))/jpn_avg_HG, digits = 4)
#calculate away attack strength
jpn_away_as <- round(((jpn_away_gs$x/jpn_away_games))/jpn_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
jpn_avg_HC <- round(jpn_T_AG /jpn_GP, digits = 4)
#avg away concede
jpn_avg_AC <- round(jpn_T_HG /jpn_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
jpn_home_ds <- round(((jpn_home_gc$x/jpn_home_games))/jpn_avg_HC, digits = 4)
#away defense strength
jpn_away_ds <- round(((jpn_away_gc$x/jpn_away_games))/jpn_avg_AC, digits = 4)
#############################################################################
#home poisson data
#jpn
jpn_division <- c()
jpn_division[1:length(jpn_teams)] <- "JPN"
jpn_home_poisson <- cbind(jpn_division,jpn_teams,jpn_avg_HG,jpn_home_as,jpn_home_ds)
#################################################################################
#away poisson data
#jpn
jpn_division <- c()
jpn_division[1:length(jpn_teams)] <- "JPN"
jpn_away_poisson <- cbind(jpn_division,jpn_teams,jpn_avg_AG,jpn_away_as,jpn_away_ds)

#create home and away csv
#jpn_home_poisson <- rbind(jpn_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#jpn_away_poisson <- rbind(jpn_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(jpn_home_poisson,'JPN.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(jpn_away_poisson,'JPN.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################JPN FIXTURES##########################################################################
#JPN
HomeTeam_jpn <- rep(jpn_teams, each = length(jpn_teams))
AwayTeam_jpn <- rep(jpn_teams, length(jpn_teams))
JPN_fixtures <- cbind(HomeTeam_jpn,AwayTeam_jpn)
JPN_fixtures <- as.data.frame(JPN_fixtures)
JPN_fixtures <- JPN_fixtures[!JPN_fixtures$HomeTeam_jpn == JPN_fixtures$AwayTeam_jpn,]
rownames(JPN_fixtures) <- NULL
JPN_fixtures$Div <- "JPN"
JPN_fixtures <- JPN_fixtures[,c(3,1,2)]

JPN_fixtures$avg_HG_jpn <- jpn_avg_HG

JPN_fixtures$jpn_homeas <- rep(jpn_home_as,each = length(jpn_teams)-1)

jpn_awayds_lookup <- cbind(jpn_teams,jpn_away_ds)

jpn_awayds_lookup <- as.data.frame(jpn_awayds_lookup)

colnames(jpn_awayds_lookup) <- c("AwayTeam_jpn","jpn_awayds")


require('RH2')
JPN_fixtures$jpn_awayds <- sqldf("SELECT jpn_awayds_lookup.jpn_awayds FROM jpn_awayds_lookup INNER JOIN JPN_fixtures ON jpn_awayds_lookup.AwayTeam_jpn = JPN_fixtures.AwayTeam_jpn")

JPN_fixtures$avg_AG_jpn <- jpn_avg_AG

jpn_awayas_lookup <- cbind(jpn_teams,jpn_away_as)

jpn_awayas_lookup <- as.data.frame(jpn_awayas_lookup)

colnames(jpn_awayas_lookup) <- c("AwayTeam_jpn","jpn_awayas")


JPN_fixtures$jpn_awayas <- sqldf("SELECT jpn_awayas_lookup.jpn_awayas FROM jpn_awayas_lookup INNER JOIN JPN_fixtures ON jpn_awayas_lookup.AwayTeam_jpn = JPN_fixtures.AwayTeam_jpn")

JPN_fixtures$jpn_homeds <- rep(jpn_home_ds,each = length(jpn_teams)-1)

JPN_fixtures$jpn_awayds <- as.numeric(unlist(JPN_fixtures$jpn_awayds))
#xGH
JPN_fixtures$jpn_xGH <- JPN_fixtures$avg_HG_jpn * JPN_fixtures$jpn_homeas * JPN_fixtures$jpn_awayds

#xGA

JPN_fixtures$jpn_awayas <- as.numeric(unlist(JPN_fixtures$jpn_awayas))

JPN_fixtures$jpn_xGA <- JPN_fixtures$avg_AG_jpn * JPN_fixtures$jpn_awayas * JPN_fixtures$jpn_homeds

JPN_fixtures$jpn_0_0 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_0 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_1 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_1 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_0 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_2 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_2 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_1 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_2 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_3 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_0 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_1 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_2 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_3 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_3 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_3 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_4 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_0 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_1 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_2 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_3 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_4 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_4 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_4 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_4 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_5 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_0 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_1 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_2 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_3 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_4 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_5 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_5 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_5 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_5 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_5 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_6 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_0 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_1 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_2 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_3 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_4 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_5 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_6 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_6 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_6 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_6 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_6 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_6 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
#Home win
JPN_fixtures$jpn_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 + JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5
)

JPN_fixtures$jpn_H <- percent(JPN_fixtures$jpn_H, accuracy = 0.1)

#Draw
JPN_fixtures$jpn_D <- (

  JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 + JPN_fixtures$jpn_4_4 +
    JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6
)

JPN_fixtures$jpn_D <- percent(JPN_fixtures$jpn_D, accuracy = 0.1)

#Away

JPN_fixtures$jpn_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 + JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6
)

JPN_fixtures$jpn_A <- percent(JPN_fixtures$jpn_A, accuracy = 0.1)

#ov25
JPN_fixtures$jpn_ov25 <- (
  JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 + JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 + JPN_fixtures$jpn_0_4 +
    JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 + JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_0 +
    JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 + JPN_fixtures$jpn_0_5 +
    JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 + JPN_fixtures$jpn_5_5 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5 + JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 +
    JPN_fixtures$jpn_4_6 + JPN_fixtures$jpn_5_6 + JPN_fixtures$jpn_6_6
)
#un25
JPN_fixtures$jpn_un25 <- (
  JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_0_2
)
#odds
JPN_fixtures$jpn_ov25_odds <- round((1/JPN_fixtures$jpn_ov25),digits = 2)
JPN_fixtures$jpn_un25_odds <- round((1/JPN_fixtures$jpn_un25),digits = 2)

JPN_fixtures$jpn_ov25_odds
JPN_fixtures$jpn_un25_odds
#percentages
JPN_fixtures$jpn_ov25 <- percent(JPN_fixtures$jpn_ov25, accuracy = 0.1)

JPN_fixtures$jpn_un25 <- percent(JPN_fixtures$jpn_un25, accuracy = 0.1)
JPN_fixtures$jpn_pscore <- paste(round(JPN_fixtures$jpn_xGH,digits = 0),round(JPN_fixtures$jpn_xGA,digits = 0),sep = "-")
#write out
write.xlsx(JPN_fixtures,'JPN.xlsx',sheetName = "JPN", append = TRUE)
###########################################################################################################
########################JPN END###########################################################################
JPN <- read.csv('../FDAS/JPN.csv')
JPN$TG <- JPN$HG + JPN$AG
JPN$OV25 <- ifelse(JPN$TG >= 3,"Y","N")
jpn_ftr_summary <- tabyl(JPN,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
jpn_ov25_summary <- tabyl(JPN,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(jpn_ftr_summary,'JPN.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(jpn_ov25_summary,'JPN.xlsx',sheetName = "OVUN25", append = TRUE)



