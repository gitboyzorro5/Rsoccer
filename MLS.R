library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('MLS.xlsx')
######################MLS START#######################################
#####################################################################
MLS <- read.csv('../FDAS/USA.csv')
MLS <- within(MLS,rm(Res))
MLS$Date <- dmy(MLS$Date)
MLS <- MLS[order(as.Date(MLS$Date, format = "%d/%m%Y"), decreasing = FALSE),]
MLS$CS <- paste(MLS$HG,MLS$AG, sep = "-")
#MLS_qualificaton <- subset(MLS,tournament == "UEFA Euro qualification")
MLS <- subset(MLS,Season == "2021")
#MLS <- MLS[MLS$Date > '2008-01-01',])
MLS$TG <- MLS$HG + MLS$AG
MLS$OV25 <- ifelse(MLS$TG >= 3,"Y","N")
MLS$FTR <- with(MLS,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
mls_totalgoalsv2 <- tapply(MLS$TG, MLS[c("Home", "Away")],mean)
mls_totalgoalsv2
mls_hgtotals <- rowSums(mls_totalgoalsv2,na.rm = T)
mls_agtotals <- colSums(mls_totalgoalsv2,na.rm = T)

mls_totalgoals <- mls_hgtotals + mls_agtotals
mls_totalgoalsv2 <- cbind(mls_totalgoalsv2,mls_totalgoals)
mls_teams <- sort(unique(MLS$Home))
mls_home_games <- c()
mls_away_games <-c()
for (i_mls in 1:length(mls_teams))
{

  mls_home_games[i_mls] <- nrow(MLS[MLS$Home == mls_teams[i_mls],])
  mls_away_games[i_mls]  <- nrow(MLS[MLS$Away == mls_teams[i_mls],])

}
mls_games_played <- mls_home_games + mls_away_games
mls_goaltotalsv2 <- cbind(mls_totalgoalsv2,mls_games_played)
mls_avg_totalgoals <- round((mls_totalgoals/ mls_games_played), digits = 4)
mls_goaltotalsv2[is.na(mls_goaltotalsv2)] <- ""
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_avg_totalgoals)
write.xlsx(mls_goaltotalsv2,'MLS.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
mls_goalscored_h <- tapply(MLS$HG, MLS[c("Home", "Date")],mean)
mls_goalscored_a <- tapply(MLS$AG, MLS[c("Away", "Date")],mean)
mls_goalscored_h[is.na(mls_goalscored_h)] <- ""
mls_goalscored_a[is.na(mls_goalscored_a)] <- ""

for(mls_rowhgs in 1:nrow(mls_goalscored_h)) {
  for(mls_colhgs in 1:ncol(mls_goalscored_h)) {

    # print(my_matrix[row, col])
    for(mls_rowags in 1:nrow(mls_goalscored_a)) {
      for(mls_colags in 1:ncol(mls_goalscored_a)) {
        ifelse(!mls_goalscored_a[mls_rowags,mls_colags]=="",mls_goalscored_h[mls_rowags,mls_colags] <- mls_goalscored_a[mls_rowags,mls_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mls_goalscored_h,'MLS.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
mls_goalconceded_h <- tapply(MLS$AG, MLS[c("Home", "Date")],mean)
mls_goalconceded_a <- tapply(MLS$HG, MLS[c("Away", "Date")],mean)
mls_goalconceded_h[is.na(mls_goalconceded_h)] <- ""
mls_goalconceded_a[is.na(mls_goalconceded_a)] <- ""

for(mls_rowhgc in 1:nrow(mls_goalconceded_h)) {
  for(mls_colhgc in 1:ncol(mls_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(mls_rowagc in 1:nrow(mls_goalconceded_a)) {
      for(mls_colagc in 1:ncol(mls_goalconceded_a)) {
        ifelse(!mls_goalconceded_a[mls_rowagc,mls_colagc]=="",mls_goalconceded_h[mls_rowagc,mls_colagc] <- mls_goalconceded_a[mls_rowagc,mls_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mls_goalconceded_h,'MLS.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
mls_form_h <- tapply(MLS$FTR, MLS[c("Home", "Date")],median)
mls_form_a <- tapply(MLS$FTR, MLS[c("Away", "Date")],median)
mls_form_h[is.na(mls_form_h)] <- ""
mls_form_a[is.na(mls_form_a)] <- ""
mls_form_h <- sub("A","L",mls_form_h)
mls_form_h <- sub("H","W",mls_form_h)
mls_form_a <- sub("A","W",mls_form_a)
mls_form_a <- sub("H","L",mls_form_a)
for(mls_rowh_f in 1:nrow(mls_form_h)) {
  for(mls_colh_f in 1:ncol(mls_form_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f in 1:nrow(mls_form_a)) {
      for(mls_cola_f in 1:ncol(mls_form_a)) {
        ifelse(!mls_form_a[mls_rowa_f,mls_cola_f]=="",mls_form_h[mls_rowa_f,mls_cola_f] <- mls_form_a[mls_rowa_f,mls_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mls_form_h,'MLS.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
mls_totalgoals_h <- tapply(MLS$TG, MLS[c("Home", "Date")],mean)
mls_totalgoals_a <- tapply(MLS$TG, MLS[c("Away", "Date")],mean)
mls_totalgoals_h[is.na(mls_totalgoals_h)] <- ""
mls_totalgoals_a[is.na(mls_totalgoals_a)] <- ""
for(mls_rowh in 1:nrow(mls_totalgoals_h)) {
  for(mls_colh in 1:ncol(mls_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa in 1:nrow(mls_totalgoals_a)) {
      for(mls_cola in 1:ncol(mls_totalgoals_a)) {
        ifelse(!mls_totalgoals_a[mls_rowa,mls_cola]=="",mls_totalgoals_h[mls_rowa,mls_cola] <- mls_totalgoals_a[mls_rowa,mls_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mls_totalgoals_h,'MLS.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
mls_form_team_against_h <- tapply(MLS$Away, MLS[c("Home", "Date")],median)
mls_form_team_against_a <- tapply(MLS$Home, MLS[c("Away", "Date")],median)
mls_form_team_against_h[is.na(mls_form_team_against_h)] <- ""
mls_form_team_against_a[is.na(mls_form_team_against_a)] <- ""
for(mls_rowh_f_against in 1:nrow(mls_form_team_against_h)) {
  for(mls_colh_f_against in 1:ncol(mls_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_against in 1:nrow(mls_form_team_against_a)) {
      for(mls_cola_f_against in 1:ncol(mls_form_team_against_a)) {
        ifelse(!mls_form_team_against_a[mls_rowa_f_against,mls_cola_f_against]=="",mls_form_team_against_h[mls_rowa_f_against,mls_cola_f_against] <- mls_form_team_against_a[mls_rowa_f_against,mls_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#MLS
mls_un05_home <- c()
mls_un05_away <- c()
mls_ov05_home <- c()
mls_ov05_away <- c()

mls_un15_home <- c()
mls_un15_away <- c()
mls_ov15_home <- c()
mls_ov15_away <- c()

mls_un25_home <- c()
mls_un25_away <- c()
mls_ov25_home <- c()
mls_ov25_away <- c()

mls_un35_home <- c()
mls_un35_away <- c()
mls_ov35_home <- c()
mls_ov35_away <- c()

mls_un45_home <- c()
mls_un45_away <- c()
mls_ov45_home <- c()
mls_ov45_away <- c()

mls_un55_home <- c()
mls_un55_away <- c()
mls_ov55_home <- c()
mls_ov55_away <- c()

for (i_mls_tg in 1:length(mls_teams))
{

  mls_un05_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG == 0,])
  mls_un05_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG == 0,])

  mls_ov05_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG > 0,])
  mls_ov05_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG > 0,])

  mls_un15_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG <= 1,])
  mls_un15_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG <= 1,])

  mls_ov15_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG >= 2,])
  mls_ov15_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG >= 2,])

  mls_un25_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG <= 2,])
  mls_un25_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG <= 2,])

  mls_ov25_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG >=3,])
  mls_ov25_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG >=3,])

  mls_un35_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG <= 3,])
  mls_un35_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG <= 3,])

  mls_ov35_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG >= 4,])
  mls_ov35_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG >= 4,])

  mls_un45_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG <= 4,])
  mls_un45_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG <= 4,])

  mls_ov45_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG >= 5,])
  mls_ov45_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG >= 5,])

  mls_un55_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG <= 5,])
  mls_un55_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG <= 5,])

  mls_ov55_home[i_mls_tg] <- nrow(MLS[MLS$Home == mls_teams[i_mls_tg] & MLS$TG >= 6,])
  mls_ov55_away[i_mls_tg] <- nrow(MLS[MLS$Away == mls_teams[i_mls_tg] & MLS$TG >= 6,])


}

mls_un05 <- mls_un05_home + mls_un05_away
mls_ov05 <- mls_ov05_home + mls_ov05_away

mls_un15 <- mls_un15_home + mls_un15_away
mls_ov15 <- mls_ov15_home + mls_ov15_away

mls_un25 <- mls_un25_home + mls_un25_away
mls_ov25 <- mls_ov25_home + mls_ov25_away

mls_un35 <- mls_un35_home + mls_un35_away
mls_ov35 <- mls_ov35_home + mls_ov35_away

mls_un45 <- mls_un45_home + mls_un45_away
mls_ov45 <- mls_ov45_home + mls_ov45_away

mls_un55 <- mls_un55_home + mls_un55_away
mls_ov55 <- mls_ov55_home + mls_ov55_away

mls_ovundata <- cbind(mls_teams,mls_un05,mls_ov05,mls_un15,mls_ov15,mls_un25,mls_ov25,mls_un35,mls_ov35,mls_un45,mls_ov45,mls_un55,mls_ov55)
write.xlsx(mls_ovundata,'MLS.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
mls_csform_h <- tapply(MLS$CS, MLS[c("Home", "Date")],median)
mls_csform_a <- tapply(MLS$CS, MLS[c("Away", "Date")],median)

mls_csform_h[is.na(mls_csform_h)] <- ""
mls_csform_a[is.na(mls_csform_a)] <- ""

for(mls_rowh_f_cs in 1:nrow(mls_csform_h)) {
  for(mls_colh_f_cs in 1:ncol(mls_csform_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_cs in 1:nrow(mls_csform_a)) {
      for(mls_cola_f_cs in 1:ncol(mls_csform_a)) {
        ifelse(!mls_csform_a[mls_rowa_f_cs,mls_cola_f_cs]=="",mls_csform_h[mls_rowa_f_cs,mls_cola_f_cs] <- mls_csform_a[mls_rowa_f_cs,mls_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
mls_home_gs <- aggregate(MLS$HG, by = list(MLS$Home), FUN = sum)
mls_home_gs_avg <- aggregate(MLS$HG, by = list(MLS$Home),mean)
mls_home_scoring <- merge(mls_home_gs,mls_home_gs_avg, by='Group.1',all = T)
names(mls_home_scoring)[names(mls_home_scoring) == "x.x"] <- "TFthg"
names(mls_home_scoring)[names(mls_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
mls_away_gs <- aggregate(MLS$AG, by = list(MLS$Away), FUN = sum)
mls_away_gs_avg <- aggregate(MLS$AG, by = list(MLS$Away),mean)
mls_away_scoring <- merge(mls_away_gs,mls_away_gs_avg, by='Group.1',all = T)
names(mls_away_scoring)[names(mls_away_scoring) == "x.x"] <- "TFtag"
names(mls_away_scoring)[names(mls_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
mls_scoring <- merge(mls_home_scoring,mls_away_scoring,by='Group.1',all = T)
mls_scoring$TGS <- mls_scoring$TFthg + mls_scoring$TFtag

#home goals conceded
mls_home_gc <- aggregate(MLS$AG, by = list(MLS$Home), FUN = sum)
mls_home_gc_avg <- aggregate(MLS$AG, by = list(MLS$Home),mean)
mls_home_conceding <- merge(mls_home_gc,mls_home_gc_avg, by='Group.1',all = T)
names(mls_home_conceding)[names(mls_home_conceding) == "x.x"] <- "TFthc"
names(mls_home_conceding)[names(mls_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
mls_away_gc <- aggregate(MLS$HG, by = list(MLS$Away), FUN = sum)
mls_away_gc_avg <- aggregate(MLS$HG, by = list(MLS$Away),mean)
mls_away_conceding <- merge(mls_away_gc,mls_away_gc_avg, by='Group.1',all = T)
names(mls_away_conceding)[names(mls_away_conceding) == "x.x"] <- "TFtac"
names(mls_away_conceding)[names(mls_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
mls_conceding <- merge(mls_home_conceding,mls_away_conceding,by='Group.1',all = T)
mls_conceding$TGC <- mls_conceding$TFthc + mls_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
mls_home_wins <- c()
mls_away_wins <- c()
mls_home_draws <- c()
mls_away_draws <- c()
mls_home_loss <- c()
mls_away_loss <- c()



for (i_mls_wins in 1:length(mls_teams))
{

  mls_home_wins[i_mls_wins] <- nrow(MLS[MLS$Home == mls_teams[i_mls_wins] & MLS$FTR == "H",])
  mls_away_wins[i_mls_wins] <- nrow(MLS[MLS$Away == mls_teams[i_mls_wins] & MLS$FTR == "A",])
  mls_home_draws[i_mls_wins] <- nrow(MLS[MLS$Home == mls_teams[i_mls_wins] & MLS$FTR == "D",])
  mls_away_draws[i_mls_wins] <- nrow(MLS[MLS$Away == mls_teams[i_mls_wins] & MLS$FTR == "D",])
  mls_home_loss[i_mls_wins] <- nrow(MLS[MLS$Home == mls_teams[i_mls_wins] & MLS$FTR == "A",])
  mls_away_loss[i_mls_wins] <- nrow(MLS[MLS$Away == mls_teams[i_mls_wins] & MLS$FTR == "H",])

}

mls_total_wins <- mls_home_wins + mls_away_wins
mls_total_draws <- mls_home_draws + mls_away_draws
mls_total_loss <- mls_home_loss + mls_away_loss

mls_league_table <- cbind(mls_teams,mls_games_played,mls_total_wins,mls_total_draws,mls_total_loss)
mls_GS <- mls_scoring$TGS
mls_GC <-mls_conceding$TGC
mls_GD <- mls_scoring$TGS - mls_conceding$TGC
mls_PTS <- (mls_total_wins*3) + (mls_total_draws*1)
mls_league_table <- cbind(mls_league_table,mls_GS,mls_GC,mls_GD,mls_PTS)
mls_league_table <- as.data.frame(mls_league_table)
#rename the columns
names(mls_league_table)[names(mls_league_table) == "mls_teams"] <- "Team"
names(mls_league_table)[names(mls_league_table) == "mls_games_played"] <- "P"
names(mls_league_table)[names(mls_league_table) == "mls_total_wins"] <- "W"
names(mls_league_table)[names(mls_league_table) == "mls_total_draws"] <- "D"
names(mls_league_table)[names(mls_league_table) == "mls_total_loss"] <- "L"
names(mls_league_table)[names(mls_league_table) == "mls_GS"] <- "F"
names(mls_league_table)[names(mls_league_table) == "mls_GC"] <- "A"
points_mls <- mls_league_table[order(mls_league_table$mls_PTS, decreasing = TRUE),]
write.xlsx(points_mls,'MLS.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six mls###################################################
#MLS
#form
#create final_mls_hf object
mls_last_n_games <- 6
final_mls_hf <- c()
for(index_mls_hf in 1:length(mls_teams))
{
  index_mls_hf <- row.names(mls_form_h) == mls_teams[index_mls_hf]
  form_mls_hf <- mls_form_h[index_mls_hf]
  deleted_form_mls_hf <- form_mls_hf[!form_mls_hf[] == ""]
  l6_form_mls_hf <- tail(deleted_form_mls_hf,mls_last_n_games)
  l6_form_mls_hf <- paste(l6_form_mls_hf,collapse = " ")
  final_mls_hf[index_mls_hf] <- rbind(paste(mls_teams[index_mls_hf],l6_form_mls_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}

#change column names
final_mls_hf <- as.data.frame(final_mls_hf)
colnames(final_mls_hf) <- "Form"
#goals scored
#create final_mls_gs object
final_mls_gs <- c()
suml6_mls_gs <- c()
for(index_mls_gs in 1:length(mls_teams))
{
  index_mls_gs <- row.names(mls_goalscored_h) == mls_teams[index_mls_gs]
  form_mls_gs <- mls_goalscored_h[index_mls_gs]
  deleted_form_mls_gs <- form_mls_gs[!form_mls_gs[] == ""]
  l6_form_mls_gs <- tail(deleted_form_mls_gs,mls_last_n_games)
  l6_form_mls_gs <- as.numeric(l6_form_mls_gs)
  suml6_mls_gs[index_mls_gs] <- sum(l6_form_mls_gs)
  suml6_mls_gs[index_mls_gs] <- paste("(",suml6_mls_gs[index_mls_gs],")",sep = "")
  l6_form_mls_gs <- paste(l6_form_mls_gs,collapse = " ")
  final_mls_gs[index_mls_gs] <- rbind(paste(mls_teams[index_mls_gs],l6_form_mls_gs,suml6_mls_gs[index_mls_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_gs
#change column names
final_mls_gs <- as.data.frame(final_mls_gs)
colnames(final_mls_gs) <- "Goals scored"
#goal conceded
#create final_mls_gc object
final_mls_gc <- c()
suml6_mls_gc <- c()
for(index_mls_gc in 1:length(mls_teams))
{
  index_mls_gc <- row.names(mls_goalconceded_h) == mls_teams[index_mls_gc]
  form_mls_gc <- mls_goalconceded_h[index_mls_gc]
  deleted_form_mls_gc <- form_mls_gc[!form_mls_gc[] == ""]
  l6_form_mls_gc <- tail(deleted_form_mls_gc,mls_last_n_games)
  l6_form_mls_gc <- as.numeric(l6_form_mls_gc)
  suml6_mls_gc[index_mls_gc] <- sum(l6_form_mls_gc)
  suml6_mls_gc[index_mls_gc] <- paste("(",suml6_mls_gc[index_mls_gc],")",sep = "")
  l6_form_mls_gc <- paste(l6_form_mls_gc,collapse = " ")
  final_mls_gc[index_mls_gc] <- rbind(paste(mls_teams[index_mls_gc],l6_form_mls_gc,suml6_mls_gc[index_mls_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}

#change column names
final_mls_gc <- as.data.frame(final_mls_gc)
colnames(final_mls_gc) <- "Goals conceded"
#total goals
#create final_mls_tg object
final_mls_tg <- c()
suml6_mls_tg <- c()
for(index_mls_tg in 1:length(mls_teams))
{
  index_mls_tg <- row.names(mls_totalgoals_h) == mls_teams[index_mls_tg]
  form_mls_tg <- mls_totalgoals_h[index_mls_tg]
  deleted_form_mls_tg <- form_mls_tg[!form_mls_tg[] == ""]
  l6_form_mls_tg <- tail(deleted_form_mls_tg,mls_last_n_games)
  l6_form_mls_tg <- as.numeric(l6_form_mls_tg)
  suml6_mls_tg[index_mls_tg] <- sum(l6_form_mls_tg)
  suml6_mls_tg[index_mls_tg] <- paste("(",suml6_mls_tg[index_mls_tg],")",sep = "")
  l6_form_mls_tg <- paste(l6_form_mls_tg,collapse = " ")
  final_mls_tg[index_mls_tg] <- rbind(paste(mls_teams[index_mls_tg],l6_form_mls_tg,suml6_mls_tg[index_mls_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
#change column names
final_mls_tg <- as.data.frame(final_mls_tg)
colnames(final_mls_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_mls_hf object
final_mls_cs <- c()
for(index_mls_cs in 1:length(mls_teams))
{
  index_mls_cs <- row.names(mls_csform_h) == mls_teams[index_mls_cs]
  csform_mls_cs <- mls_csform_h[index_mls_cs]
  deleted_csform_mls_cs <- csform_mls_cs[!csform_mls_cs[] == ""]
  l6_csform_mls_cs <- tail(deleted_csform_mls_cs,mls_last_n_games)
  l6_csform_mls_cs <- paste(l6_csform_mls_cs,collapse = " ")
  final_mls_cs[index_mls_cs] <- rbind(paste(mls_teams[index_mls_cs],l6_csform_mls_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",mls_teams[index],l6_csform)

}

#change column names
final_mls_cs <- as.data.frame(final_mls_cs)
colnames(final_mls_cs) <- "CSForm"
#################################################
#Team against
#create final_mls_hf_against
final_mls_hf_against <- c()
for(index_mls_hf_against in 1:length(mls_teams))
{
  index_mls_hf_against <- row.names(mls_form_team_against_h) == mls_teams[index_mls_hf_against]
  form_mls_hf_against <- mls_form_team_against_h[index_mls_hf_against]
  deleted_form_mls_hf_against <- form_mls_hf_against[!form_mls_hf_against[] == ""]
  l6_form_mls_hf_against <- tail(deleted_form_mls_hf_against,mls_last_n_games)
  l6_form_mls_hf_against <- paste(l6_form_mls_hf_against,collapse = " ")
  final_mls_hf_against[index_mls_hf_against] <- rbind(paste(mls_teams[index_mls_hf_against],l6_form_mls_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_hf_against <- as.data.frame(final_mls_hf_against)
colnames(final_mls_hf_against) <- "Team against"
#combine the columns
final_mls_all <- cbind(final_mls_hf,final_mls_gs,final_mls_gc,final_mls_tg,final_mls_cs,final_mls_hf_against)
write.xlsx(final_mls_all,'MLS.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
mls_GP <- nrow(MLS)
#Calculate total home goals for each division
mls_T_HG <- sum(mls_home_gs$x)
#calculate average home goal
mls_avg_HG <- round(mls_T_HG /mls_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AG <- sum(mls_away_gs$x)
#calculate average away goal
mls_avg_AG <- round(mls_T_AG /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_as <- round(((mls_home_gs$x/mls_home_games))/mls_avg_HG, digits = 4)
#calculate away attack strength
mls_away_as <- round(((mls_away_gs$x/mls_away_games))/mls_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HC <- round(mls_T_AG /mls_GP, digits = 4)
#avg away concede
mls_avg_AC <- round(mls_T_HG /mls_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
mls_home_ds <- round(((mls_home_gc$x/mls_home_games))/mls_avg_HC, digits = 4)
#away defense strength
mls_away_ds <- round(((mls_away_gc$x/mls_away_games))/mls_avg_AC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson <- cbind(mls_division,mls_teams,mls_avg_HG,mls_home_as,mls_home_ds)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson <- cbind(mls_division,mls_teams,mls_avg_AG,mls_away_as,mls_away_ds)

#create home and away csv
#mls_home_poisson <- rbind(mls_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#mls_away_poisson <- rbind(mls_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(mls_home_poisson,'MLS.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(mls_away_poisson,'MLS.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################MLS FIXTURES##########################################################################
#MLS
HomeTeam_mls <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls <- rep(mls_teams, length(mls_teams))
MLS_fixtures <- cbind(HomeTeam_mls,AwayTeam_mls)
MLS_fixtures <- as.data.frame(MLS_fixtures)
MLS_fixtures <- MLS_fixtures[!MLS_fixtures$HomeTeam_mls == MLS_fixtures$AwayTeam_mls,]
rownames(MLS_fixtures) <- NULL
MLS_fixtures$Div <- "MLS"
MLS_fixtures <- MLS_fixtures[,c(3,1,2)]

MLS_fixtures$avg_HG_mls <- mls_avg_HG

MLS_fixtures$mls_homeas <- rep(mls_home_as,each = length(mls_teams)-1)

mls_awayds_lookup <- cbind(mls_teams,mls_away_ds)

mls_awayds_lookup <- as.data.frame(mls_awayds_lookup)

colnames(mls_awayds_lookup) <- c("AwayTeam_mls","mls_awayds")


require('RH2')
MLS_fixtures$mls_awayds <- sqldf("SELECT mls_awayds_lookup.mls_awayds FROM mls_awayds_lookup INNER JOIN MLS_fixtures ON mls_awayds_lookup.AwayTeam_mls = MLS_fixtures.AwayTeam_mls")

MLS_fixtures$avg_AG_mls <- mls_avg_AG

mls_awayas_lookup <- cbind(mls_teams,mls_away_as)

mls_awayas_lookup <- as.data.frame(mls_awayas_lookup)

colnames(mls_awayas_lookup) <- c("AwayTeam_mls","mls_awayas")


MLS_fixtures$mls_awayas <- sqldf("SELECT mls_awayas_lookup.mls_awayas FROM mls_awayas_lookup INNER JOIN MLS_fixtures ON mls_awayas_lookup.AwayTeam_mls = MLS_fixtures.AwayTeam_mls")

MLS_fixtures$mls_homeds <- rep(mls_home_ds,each = length(mls_teams)-1)

MLS_fixtures$mls_awayds <- as.numeric(unlist(MLS_fixtures$mls_awayds))
#xGH
MLS_fixtures$mls_xGH <- MLS_fixtures$avg_HG_mls * MLS_fixtures$mls_homeas * MLS_fixtures$mls_awayds

#xGA

MLS_fixtures$mls_awayas <- as.numeric(unlist(MLS_fixtures$mls_awayas))

MLS_fixtures$mls_xGA <- MLS_fixtures$avg_AG_mls * MLS_fixtures$mls_awayas * MLS_fixtures$mls_homeds

MLS_fixtures$mls_0_0 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_0 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_1 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_1 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_0 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_2 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_2 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_1 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_2 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_3 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_0 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_1 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_2 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_3 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_3 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_3 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_4 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_0 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_1 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_2 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_3 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_4 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_4 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_4 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_4 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_5 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_0 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_1 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_2 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_3 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_4 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_5 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_5 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_5 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_5 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_5 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_6 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_0 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_1 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_2 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_3 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_4 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_5 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_6 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_6 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_6 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_6 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_6 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_6 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
#Home win
MLS_fixtures$mls_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 + MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5
)

MLS_fixtures$mls_H <- percent(MLS_fixtures$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures$mls_D <- (

  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 + MLS_fixtures$mls_4_4 +
    MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6
)

MLS_fixtures$mls_D <- percent(MLS_fixtures$mls_D, accuracy = 0.1)

#Away

MLS_fixtures$mls_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 + MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6
)

MLS_fixtures$mls_A <- percent(MLS_fixtures$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures$mls_ov25 <- (
  MLS_fixtures$mls_2_1 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 + MLS_fixtures$mls_2_3 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 + MLS_fixtures$mls_0_4 +
    MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 + MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_0 +
    MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 + MLS_fixtures$mls_0_5 +
    MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 + MLS_fixtures$mls_5_5 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5 + MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 +
    MLS_fixtures$mls_4_6 + MLS_fixtures$mls_5_6 + MLS_fixtures$mls_6_6
)
#un25
MLS_fixtures$mls_un25 <- (
  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_0 + MLS_fixtures$mls_0_1 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_0_2
)
#odds
MLS_fixtures$mls_ov25_odds <- round((1/MLS_fixtures$mls_ov25),digits = 2)
MLS_fixtures$mls_un25_odds <- round((1/MLS_fixtures$mls_un25),digits = 2)

MLS_fixtures$mls_ov25_odds
MLS_fixtures$mls_un25_odds
#percentages
MLS_fixtures$mls_ov25 <- percent(MLS_fixtures$mls_ov25, accuracy = 0.1)

MLS_fixtures$mls_un25 <- percent(MLS_fixtures$mls_un25, accuracy = 0.1)
MLS_fixtures$mls_pscore <- paste(round(MLS_fixtures$mls_xGH,digits = 0),round(MLS_fixtures$mls_xGA,digits = 0),sep = "-")
#write out
write.xlsx(MLS_fixtures,'MLS.xlsx',sheetName = "MLS", append = TRUE)
###########################################################################################################
########################MLS END###########################################################################
MLS <- read.csv('../FDAS/USA.csv')
MLS$TG <- MLS$HG + MLS$AG
MLS$OV25 <- ifelse(MLS$TG >= 3,"Y","N")
mls_ftr_summary <- tabyl(MLS,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
mls_ov25_summary <- tabyl(MLS,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(mls_ftr_summary,'MLS.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(mls_ov25_summary,'MLS.xlsx',sheetName = "OVUN25", append = TRUE)



