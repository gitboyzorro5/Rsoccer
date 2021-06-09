library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('MEX.xlsx')
######################MEX START#######################################
#####################################################################
MEX <- read.csv('../FDAS/MEX.csv')
MEX <- within(MEX,rm(Res))
MEX$Date <- dmy(MEX$Date)
MEX <- MEX[order(as.Date(MEX$Date, format = "%d/%m%Y"), decreasing = FALSE),]
MEX$CS <- paste(MEX$HG,MEX$AG, sep = "-")
#MEX_qualificaton <- subset(MEX,tournament == "UEFA Euro qualification")
MEX <- subset(MEX,Season == "2020/2021")
#MEX <- MEX[MEX$Date > '2008-01-01',])
MEX$TG <- MEX$HG + MEX$AG
MEX$OV25 <- ifelse(MEX$TG >= 3,"Y","N")
MEX$FTR <- with(MEX,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
mex_totalgoalsv2 <- tapply(MEX$TG, MEX[c("Home", "Away")],mean)
mex_totalgoalsv2
mex_hgtotals <- rowSums(mex_totalgoalsv2,na.rm = T)
mex_agtotals <- colSums(mex_totalgoalsv2,na.rm = T)

mex_totalgoals <- mex_hgtotals + mex_agtotals
mex_totalgoalsv2 <- cbind(mex_totalgoalsv2,mex_totalgoals)
mex_teams <- sort(unique(MEX$Home))
mex_home_games <- c()
mex_away_games <-c()
for (i_mex in 1:length(mex_teams))
{

  mex_home_games[i_mex] <- nrow(MEX[MEX$Home == mex_teams[i_mex],])
  mex_away_games[i_mex]  <- nrow(MEX[MEX$Away == mex_teams[i_mex],])

}
mex_games_played <- mex_home_games + mex_away_games
mex_goaltotalsv2 <- cbind(mex_totalgoalsv2,mex_games_played)
mex_avg_totalgoals <- round((mex_totalgoals/ mex_games_played), digits = 4)
mex_goaltotalsv2[is.na(mex_goaltotalsv2)] <- ""
mex_goaltotalsv2 <- cbind(mex_goaltotalsv2,mex_avg_totalgoals)
write.xlsx(mex_goaltotalsv2,'MEX.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
mex_goalscored_h <- tapply(MEX$HG, MEX[c("Home", "Date")],mean)
mex_goalscored_a <- tapply(MEX$AG, MEX[c("Away", "Date")],mean)
mex_goalscored_h[is.na(mex_goalscored_h)] <- ""
mex_goalscored_a[is.na(mex_goalscored_a)] <- ""

for(mex_rowhgs in 1:nrow(mex_goalscored_h)) {
  for(mex_colhgs in 1:ncol(mex_goalscored_h)) {

    # print(my_matrix[row, col])
    for(mex_rowags in 1:nrow(mex_goalscored_a)) {
      for(mex_colags in 1:ncol(mex_goalscored_a)) {
        ifelse(!mex_goalscored_a[mex_rowags,mex_colags]=="",mex_goalscored_h[mex_rowags,mex_colags] <- mex_goalscored_a[mex_rowags,mex_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mex_goalscored_h,'MEX.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
mex_goalconceded_h <- tapply(MEX$AG, MEX[c("Home", "Date")],mean)
mex_goalconceded_a <- tapply(MEX$HG, MEX[c("Away", "Date")],mean)
mex_goalconceded_h[is.na(mex_goalconceded_h)] <- ""
mex_goalconceded_a[is.na(mex_goalconceded_a)] <- ""

for(mex_rowhgc in 1:nrow(mex_goalconceded_h)) {
  for(mex_colhgc in 1:ncol(mex_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(mex_rowagc in 1:nrow(mex_goalconceded_a)) {
      for(mex_colagc in 1:ncol(mex_goalconceded_a)) {
        ifelse(!mex_goalconceded_a[mex_rowagc,mex_colagc]=="",mex_goalconceded_h[mex_rowagc,mex_colagc] <- mex_goalconceded_a[mex_rowagc,mex_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mex_goalconceded_h,'MEX.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
mex_form_h <- tapply(MEX$FTR, MEX[c("Home", "Date")],median)
mex_form_a <- tapply(MEX$FTR, MEX[c("Away", "Date")],median)
mex_form_h[is.na(mex_form_h)] <- ""
mex_form_a[is.na(mex_form_a)] <- ""
mex_form_h <- sub("A","L",mex_form_h)
mex_form_h <- sub("H","W",mex_form_h)
mex_form_a <- sub("A","W",mex_form_a)
mex_form_a <- sub("H","L",mex_form_a)
for(mex_rowh_f in 1:nrow(mex_form_h)) {
  for(mex_colh_f in 1:ncol(mex_form_h)) {

    # print(my_matrix[row, col])
    for(mex_rowa_f in 1:nrow(mex_form_a)) {
      for(mex_cola_f in 1:ncol(mex_form_a)) {
        ifelse(!mex_form_a[mex_rowa_f,mex_cola_f]=="",mex_form_h[mex_rowa_f,mex_cola_f] <- mex_form_a[mex_rowa_f,mex_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mex_form_h,'MEX.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
mex_totalgoals_h <- tapply(MEX$TG, MEX[c("Home", "Date")],mean)
mex_totalgoals_a <- tapply(MEX$TG, MEX[c("Away", "Date")],mean)
mex_totalgoals_h[is.na(mex_totalgoals_h)] <- ""
mex_totalgoals_a[is.na(mex_totalgoals_a)] <- ""
for(mex_rowh in 1:nrow(mex_totalgoals_h)) {
  for(mex_colh in 1:ncol(mex_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(mex_rowa in 1:nrow(mex_totalgoals_a)) {
      for(mex_cola in 1:ncol(mex_totalgoals_a)) {
        ifelse(!mex_totalgoals_a[mex_rowa,mex_cola]=="",mex_totalgoals_h[mex_rowa,mex_cola] <- mex_totalgoals_a[mex_rowa,mex_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(mex_totalgoals_h,'MEX.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
mex_form_team_against_h <- tapply(MEX$Away, MEX[c("Home", "Date")],median)
mex_form_team_against_a <- tapply(MEX$Home, MEX[c("Away", "Date")],median)
mex_form_team_against_h[is.na(mex_form_team_against_h)] <- ""
mex_form_team_against_a[is.na(mex_form_team_against_a)] <- ""
for(mex_rowh_f_against in 1:nrow(mex_form_team_against_h)) {
  for(mex_colh_f_against in 1:ncol(mex_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(mex_rowa_f_against in 1:nrow(mex_form_team_against_a)) {
      for(mex_cola_f_against in 1:ncol(mex_form_team_against_a)) {
        ifelse(!mex_form_team_against_a[mex_rowa_f_against,mex_cola_f_against]=="",mex_form_team_against_h[mex_rowa_f_against,mex_cola_f_against] <- mex_form_team_against_a[mex_rowa_f_against,mex_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#MEX
mex_un05_home <- c()
mex_un05_away <- c()
mex_ov05_home <- c()
mex_ov05_away <- c()

mex_un15_home <- c()
mex_un15_away <- c()
mex_ov15_home <- c()
mex_ov15_away <- c()

mex_un25_home <- c()
mex_un25_away <- c()
mex_ov25_home <- c()
mex_ov25_away <- c()

mex_un35_home <- c()
mex_un35_away <- c()
mex_ov35_home <- c()
mex_ov35_away <- c()

mex_un45_home <- c()
mex_un45_away <- c()
mex_ov45_home <- c()
mex_ov45_away <- c()

mex_un55_home <- c()
mex_un55_away <- c()
mex_ov55_home <- c()
mex_ov55_away <- c()

for (i_mex_tg in 1:length(mex_teams))
{

  mex_un05_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG == 0,])
  mex_un05_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG == 0,])

  mex_ov05_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG > 0,])
  mex_ov05_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG > 0,])

  mex_un15_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG <= 1,])
  mex_un15_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG <= 1,])

  mex_ov15_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG >= 2,])
  mex_ov15_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG >= 2,])

  mex_un25_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG <= 2,])
  mex_un25_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG <= 2,])

  mex_ov25_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG >=3,])
  mex_ov25_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG >=3,])

  mex_un35_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG <= 3,])
  mex_un35_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG <= 3,])

  mex_ov35_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG >= 4,])
  mex_ov35_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG >= 4,])

  mex_un45_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG <= 4,])
  mex_un45_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG <= 4,])

  mex_ov45_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG >= 5,])
  mex_ov45_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG >= 5,])

  mex_un55_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG <= 5,])
  mex_un55_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG <= 5,])

  mex_ov55_home[i_mex_tg] <- nrow(MEX[MEX$Home == mex_teams[i_mex_tg] & MEX$TG >= 6,])
  mex_ov55_away[i_mex_tg] <- nrow(MEX[MEX$Away == mex_teams[i_mex_tg] & MEX$TG >= 6,])


}

mex_un05 <- mex_un05_home + mex_un05_away
mex_ov05 <- mex_ov05_home + mex_ov05_away

mex_un15 <- mex_un15_home + mex_un15_away
mex_ov15 <- mex_ov15_home + mex_ov15_away

mex_un25 <- mex_un25_home + mex_un25_away
mex_ov25 <- mex_ov25_home + mex_ov25_away

mex_un35 <- mex_un35_home + mex_un35_away
mex_ov35 <- mex_ov35_home + mex_ov35_away

mex_un45 <- mex_un45_home + mex_un45_away
mex_ov45 <- mex_ov45_home + mex_ov45_away

mex_un55 <- mex_un55_home + mex_un55_away
mex_ov55 <- mex_ov55_home + mex_ov55_away

mex_ovundata <- cbind(mex_teams,mex_un05,mex_ov05,mex_un15,mex_ov15,mex_un25,mex_ov25,mex_un35,mex_ov35,mex_un45,mex_ov45,mex_un55,mex_ov55)
write.xlsx(mex_ovundata,'MEX.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
mex_csform_h <- tapply(MEX$CS, MEX[c("Home", "Date")],median)
mex_csform_a <- tapply(MEX$CS, MEX[c("Away", "Date")],median)

mex_csform_h[is.na(mex_csform_h)] <- ""
mex_csform_a[is.na(mex_csform_a)] <- ""

for(mex_rowh_f_cs in 1:nrow(mex_csform_h)) {
  for(mex_colh_f_cs in 1:ncol(mex_csform_h)) {

    # print(my_matrix[row, col])
    for(mex_rowa_f_cs in 1:nrow(mex_csform_a)) {
      for(mex_cola_f_cs in 1:ncol(mex_csform_a)) {
        ifelse(!mex_csform_a[mex_rowa_f_cs,mex_cola_f_cs]=="",mex_csform_h[mex_rowa_f_cs,mex_cola_f_cs] <- mex_csform_a[mex_rowa_f_cs,mex_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
mex_home_gs <- aggregate(MEX$HG, by = list(MEX$Home), FUN = sum)
mex_home_gs_avg <- aggregate(MEX$HG, by = list(MEX$Home),mean)
mex_home_scoring <- merge(mex_home_gs,mex_home_gs_avg, by='Group.1',all = T)
names(mex_home_scoring)[names(mex_home_scoring) == "x.x"] <- "TFthg"
names(mex_home_scoring)[names(mex_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
mex_away_gs <- aggregate(MEX$AG, by = list(MEX$Away), FUN = sum)
mex_away_gs_avg <- aggregate(MEX$AG, by = list(MEX$Away),mean)
mex_away_scoring <- merge(mex_away_gs,mex_away_gs_avg, by='Group.1',all = T)
names(mex_away_scoring)[names(mex_away_scoring) == "x.x"] <- "TFtag"
names(mex_away_scoring)[names(mex_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
mex_scoring <- merge(mex_home_scoring,mex_away_scoring,by='Group.1',all = T)
mex_scoring$TGS <- mex_scoring$TFthg + mex_scoring$TFtag

#home goals conceded
mex_home_gc <- aggregate(MEX$AG, by = list(MEX$Home), FUN = sum)
mex_home_gc_avg <- aggregate(MEX$AG, by = list(MEX$Home),mean)
mex_home_conceding <- merge(mex_home_gc,mex_home_gc_avg, by='Group.1',all = T)
names(mex_home_conceding)[names(mex_home_conceding) == "x.x"] <- "TFthc"
names(mex_home_conceding)[names(mex_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
mex_away_gc <- aggregate(MEX$HG, by = list(MEX$Away), FUN = sum)
mex_away_gc_avg <- aggregate(MEX$HG, by = list(MEX$Away),mean)
mex_away_conceding <- merge(mex_away_gc,mex_away_gc_avg, by='Group.1',all = T)
names(mex_away_conceding)[names(mex_away_conceding) == "x.x"] <- "TFtac"
names(mex_away_conceding)[names(mex_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
mex_conceding <- merge(mex_home_conceding,mex_away_conceding,by='Group.1',all = T)
mex_conceding$TGC <- mex_conceding$TFthc + mex_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
mex_home_wins <- c()
mex_away_wins <- c()
mex_home_draws <- c()
mex_away_draws <- c()
mex_home_loss <- c()
mex_away_loss <- c()



for (i_mex_wins in 1:length(mex_teams))
{

  mex_home_wins[i_mex_wins] <- nrow(MEX[MEX$Home == mex_teams[i_mex_wins] & MEX$FTR == "H",])
  mex_away_wins[i_mex_wins] <- nrow(MEX[MEX$Away == mex_teams[i_mex_wins] & MEX$FTR == "A",])
  mex_home_draws[i_mex_wins] <- nrow(MEX[MEX$Home == mex_teams[i_mex_wins] & MEX$FTR == "D",])
  mex_away_draws[i_mex_wins] <- nrow(MEX[MEX$Away == mex_teams[i_mex_wins] & MEX$FTR == "D",])
  mex_home_loss[i_mex_wins] <- nrow(MEX[MEX$Home == mex_teams[i_mex_wins] & MEX$FTR == "A",])
  mex_away_loss[i_mex_wins] <- nrow(MEX[MEX$Away == mex_teams[i_mex_wins] & MEX$FTR == "H",])

}

mex_total_wins <- mex_home_wins + mex_away_wins
mex_total_draws <- mex_home_draws + mex_away_draws
mex_total_loss <- mex_home_loss + mex_away_loss

mex_league_table <- cbind(mex_teams,mex_games_played,mex_total_wins,mex_total_draws,mex_total_loss)
mex_GS <- mex_scoring$TGS
mex_GC <-mex_conceding$TGC
mex_GD <- mex_scoring$TGS - mex_conceding$TGC
mex_PTS <- (mex_total_wins*3) + (mex_total_draws*1)
mex_league_table <- cbind(mex_league_table,mex_GS,mex_GC,mex_GD,mex_PTS)
mex_league_table <- as.data.frame(mex_league_table)
#rename the columns
names(mex_league_table)[names(mex_league_table) == "mex_teams"] <- "Team"
names(mex_league_table)[names(mex_league_table) == "mex_games_played"] <- "P"
names(mex_league_table)[names(mex_league_table) == "mex_total_wins"] <- "W"
names(mex_league_table)[names(mex_league_table) == "mex_total_draws"] <- "D"
names(mex_league_table)[names(mex_league_table) == "mex_total_loss"] <- "L"
names(mex_league_table)[names(mex_league_table) == "mex_GS"] <- "F"
names(mex_league_table)[names(mex_league_table) == "mex_GC"] <- "A"
points_mex <- mex_league_table[order(mex_league_table$mex_PTS, decreasing = TRUE),]
write.xlsx(points_mex,'MEX.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six mex###################################################
#MEX
#form
#create final_mex_hf object
mex_last_n_games <- 6
final_mex_hf <- c()
for(index_mex_hf in 1:length(mex_teams))
{
  index_mex_hf <- row.names(mex_form_h) == mex_teams[index_mex_hf]
  form_mex_hf <- mex_form_h[index_mex_hf]
  deleted_form_mex_hf <- form_mex_hf[!form_mex_hf[] == ""]
  l6_form_mex_hf <- tail(deleted_form_mex_hf,mex_last_n_games)
  l6_form_mex_hf <- paste(l6_form_mex_hf,collapse = " ")
  final_mex_hf[index_mex_hf] <- rbind(paste(mex_teams[index_mex_hf],l6_form_mex_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mex_teams[index],l6_form)

}

#change column names
final_mex_hf <- as.data.frame(final_mex_hf)
colnames(final_mex_hf) <- "Form"
#goals scored
#create final_mex_gs object
final_mex_gs <- c()
suml6_mex_gs <- c()
for(index_mex_gs in 1:length(mex_teams))
{
  index_mex_gs <- row.names(mex_goalscored_h) == mex_teams[index_mex_gs]
  form_mex_gs <- mex_goalscored_h[index_mex_gs]
  deleted_form_mex_gs <- form_mex_gs[!form_mex_gs[] == ""]
  l6_form_mex_gs <- tail(deleted_form_mex_gs,mex_last_n_games)
  l6_form_mex_gs <- as.numeric(l6_form_mex_gs)
  suml6_mex_gs[index_mex_gs] <- sum(l6_form_mex_gs)
  suml6_mex_gs[index_mex_gs] <- paste("(",suml6_mex_gs[index_mex_gs],")",sep = "")
  l6_form_mex_gs <- paste(l6_form_mex_gs,collapse = " ")
  final_mex_gs[index_mex_gs] <- rbind(paste(mex_teams[index_mex_gs],l6_form_mex_gs,suml6_mex_gs[index_mex_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mex_teams[index],l6_form)

}
final_mex_gs
#change column names
final_mex_gs <- as.data.frame(final_mex_gs)
colnames(final_mex_gs) <- "Goals scored"
#goal conceded
#create final_mex_gc object
final_mex_gc <- c()
suml6_mex_gc <- c()
for(index_mex_gc in 1:length(mex_teams))
{
  index_mex_gc <- row.names(mex_goalconceded_h) == mex_teams[index_mex_gc]
  form_mex_gc <- mex_goalconceded_h[index_mex_gc]
  deleted_form_mex_gc <- form_mex_gc[!form_mex_gc[] == ""]
  l6_form_mex_gc <- tail(deleted_form_mex_gc,mex_last_n_games)
  l6_form_mex_gc <- as.numeric(l6_form_mex_gc)
  suml6_mex_gc[index_mex_gc] <- sum(l6_form_mex_gc)
  suml6_mex_gc[index_mex_gc] <- paste("(",suml6_mex_gc[index_mex_gc],")",sep = "")
  l6_form_mex_gc <- paste(l6_form_mex_gc,collapse = " ")
  final_mex_gc[index_mex_gc] <- rbind(paste(mex_teams[index_mex_gc],l6_form_mex_gc,suml6_mex_gc[index_mex_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mex_teams[index],l6_form)

}

#change column names
final_mex_gc <- as.data.frame(final_mex_gc)
colnames(final_mex_gc) <- "Goals conceded"
#total goals
#create final_mex_tg object
final_mex_tg <- c()
suml6_mex_tg <- c()
for(index_mex_tg in 1:length(mex_teams))
{
  index_mex_tg <- row.names(mex_totalgoals_h) == mex_teams[index_mex_tg]
  form_mex_tg <- mex_totalgoals_h[index_mex_tg]
  deleted_form_mex_tg <- form_mex_tg[!form_mex_tg[] == ""]
  l6_form_mex_tg <- tail(deleted_form_mex_tg,mex_last_n_games)
  l6_form_mex_tg <- as.numeric(l6_form_mex_tg)
  suml6_mex_tg[index_mex_tg] <- sum(l6_form_mex_tg)
  suml6_mex_tg[index_mex_tg] <- paste("(",suml6_mex_tg[index_mex_tg],")",sep = "")
  l6_form_mex_tg <- paste(l6_form_mex_tg,collapse = " ")
  final_mex_tg[index_mex_tg] <- rbind(paste(mex_teams[index_mex_tg],l6_form_mex_tg,suml6_mex_tg[index_mex_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mex_teams[index],l6_form)

}
#change column names
final_mex_tg <- as.data.frame(final_mex_tg)
colnames(final_mex_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_mex_hf object
final_mex_cs <- c()
for(index_mex_cs in 1:length(mex_teams))
{
  index_mex_cs <- row.names(mex_csform_h) == mex_teams[index_mex_cs]
  csform_mex_cs <- mex_csform_h[index_mex_cs]
  deleted_csform_mex_cs <- csform_mex_cs[!csform_mex_cs[] == ""]
  l6_csform_mex_cs <- tail(deleted_csform_mex_cs,mex_last_n_games)
  l6_csform_mex_cs <- paste(l6_csform_mex_cs,collapse = " ")
  final_mex_cs[index_mex_cs] <- rbind(paste(mex_teams[index_mex_cs],l6_csform_mex_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",mex_teams[index],l6_csform)

}

#change column names
final_mex_cs <- as.data.frame(final_mex_cs)
colnames(final_mex_cs) <- "CSForm"
#################################################
#Team against
#create final_mex_hf_against
final_mex_hf_against <- c()
for(index_mex_hf_against in 1:length(mex_teams))
{
  index_mex_hf_against <- row.names(mex_form_team_against_h) == mex_teams[index_mex_hf_against]
  form_mex_hf_against <- mex_form_team_against_h[index_mex_hf_against]
  deleted_form_mex_hf_against <- form_mex_hf_against[!form_mex_hf_against[] == ""]
  l6_form_mex_hf_against <- tail(deleted_form_mex_hf_against,mex_last_n_games)
  l6_form_mex_hf_against <- paste(l6_form_mex_hf_against,collapse = " ")
  final_mex_hf_against[index_mex_hf_against] <- rbind(paste(mex_teams[index_mex_hf_against],l6_form_mex_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mex_teams[index],l6_form)

}
final_mex_hf_against <- as.data.frame(final_mex_hf_against)
colnames(final_mex_hf_against) <- "Team against"
#combine the columns
final_mex_all <- cbind(final_mex_hf,final_mex_gs,final_mex_gc,final_mex_tg,final_mex_cs,final_mex_hf_against)
write.xlsx(final_mex_all,'MEX.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
mex_GP <- nrow(MEX)
#Calculate total home goals for each division
mex_T_HG <- sum(mex_home_gs$x)
#calculate average home goal
mex_avg_HG <- round(mex_T_HG /mex_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mex_T_AG <- sum(mex_away_gs$x)
#calculate average away goal
mex_avg_AG <- round(mex_T_AG /mex_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mex_home_as <- round(((mex_home_gs$x/mex_home_games))/mex_avg_HG, digits = 4)
#calculate away attack strength
mex_away_as <- round(((mex_away_gs$x/mex_away_games))/mex_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
mex_avg_HC <- round(mex_T_AG /mex_GP, digits = 4)
#avg away concede
mex_avg_AC <- round(mex_T_HG /mex_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
mex_home_ds <- round(((mex_home_gc$x/mex_home_games))/mex_avg_HC, digits = 4)
#away defense strength
mex_away_ds <- round(((mex_away_gc$x/mex_away_games))/mex_avg_AC, digits = 4)
#############################################################################
#home poisson data
#mex
mex_division <- c()
mex_division[1:length(mex_teams)] <- "MEX"
mex_home_poisson <- cbind(mex_division,mex_teams,mex_avg_HG,mex_home_as,mex_home_ds)
#################################################################################
#away poisson data
#mex
mex_division <- c()
mex_division[1:length(mex_teams)] <- "MEX"
mex_away_poisson <- cbind(mex_division,mex_teams,mex_avg_AG,mex_away_as,mex_away_ds)

#create home and away csv
#mex_home_poisson <- rbind(mex_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#mex_away_poisson <- rbind(mex_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(mex_home_poisson,'MEX.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(mex_away_poisson,'MEX.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################MEX FIXTURES##########################################################################
#MEX
HomeTeam_mex <- rep(mex_teams, each = length(mex_teams))
AwayTeam_mex <- rep(mex_teams, length(mex_teams))
MEX_fixtures <- cbind(HomeTeam_mex,AwayTeam_mex)
MEX_fixtures <- as.data.frame(MEX_fixtures)
MEX_fixtures <- MEX_fixtures[!MEX_fixtures$HomeTeam_mex == MEX_fixtures$AwayTeam_mex,]
rownames(MEX_fixtures) <- NULL
MEX_fixtures$Div <- "MEX"
MEX_fixtures <- MEX_fixtures[,c(3,1,2)]

MEX_fixtures$avg_HG_mex <- mex_avg_HG

MEX_fixtures$mex_homeas <- rep(mex_home_as,each = length(mex_teams)-1)

mex_awayds_lookup <- cbind(mex_teams,mex_away_ds)

mex_awayds_lookup <- as.data.frame(mex_awayds_lookup)

colnames(mex_awayds_lookup) <- c("AwayTeam_mex","mex_awayds")


require('RH2')
MEX_fixtures$mex_awayds <- sqldf("SELECT mex_awayds_lookup.mex_awayds FROM mex_awayds_lookup INNER JOIN MEX_fixtures ON mex_awayds_lookup.AwayTeam_mex = MEX_fixtures.AwayTeam_mex")

MEX_fixtures$avg_AG_mex <- mex_avg_AG

mex_awayas_lookup <- cbind(mex_teams,mex_away_as)

mex_awayas_lookup <- as.data.frame(mex_awayas_lookup)

colnames(mex_awayas_lookup) <- c("AwayTeam_mex","mex_awayas")


MEX_fixtures$mex_awayas <- sqldf("SELECT mex_awayas_lookup.mex_awayas FROM mex_awayas_lookup INNER JOIN MEX_fixtures ON mex_awayas_lookup.AwayTeam_mex = MEX_fixtures.AwayTeam_mex")

MEX_fixtures$mex_homeds <- rep(mex_home_ds,each = length(mex_teams)-1)

MEX_fixtures$mex_awayds <- as.numeric(unlist(MEX_fixtures$mex_awayds))
#xGH
MEX_fixtures$mex_xGH <- MEX_fixtures$avg_HG_mex * MEX_fixtures$mex_homeas * MEX_fixtures$mex_awayds

#xGA

MEX_fixtures$mex_awayas <- as.numeric(unlist(MEX_fixtures$mex_awayas))

MEX_fixtures$mex_xGA <- MEX_fixtures$avg_AG_mex * MEX_fixtures$mex_awayas * MEX_fixtures$mex_homeds

MEX_fixtures$mex_0_0 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_0 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_0_1 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_1 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_0 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_0_2 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_2 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_1 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_2 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_3 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_0 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_1 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_2 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_0_3 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_3 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_3 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_4 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_0 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_1 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_2 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_3 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_0_4 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_4 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_4 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_4 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_5 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_0 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_1 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_2 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_3 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_4 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_0_5 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_5 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_5 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_5 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_5 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_6 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_0 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(0,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_1 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(1,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_2 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(2,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_3 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(3,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_4 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(4,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_6_5 <- round(stats::dpois(6,MEX_fixtures$mex_xGH) * stats::dpois(5,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_0_6 <- round(stats::dpois(0,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_1_6 <- round(stats::dpois(1,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_2_6 <- round(stats::dpois(2,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_3_6 <- round(stats::dpois(3,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_4_6 <- round(stats::dpois(4,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
MEX_fixtures$mex_5_6 <- round(stats::dpois(5,MEX_fixtures$mex_xGH) * stats::dpois(6,MEX_fixtures$mex_xGA), digits = 4)
#Home win
MEX_fixtures$mex_H <- (
  MEX_fixtures$mex_1_0 + MEX_fixtures$mex_2_0 + MEX_fixtures$mex_2_1 + MEX_fixtures$mex_3_0 + MEX_fixtures$mex_3_1 +
    MEX_fixtures$mex_3_2 + MEX_fixtures$mex_4_0 + MEX_fixtures$mex_4_1 + MEX_fixtures$mex_4_2 + MEX_fixtures$mex_4_3 +
    MEX_fixtures$mex_5_0 + MEX_fixtures$mex_5_1 + MEX_fixtures$mex_5_2 + MEX_fixtures$mex_5_3 + MEX_fixtures$mex_5_4 +
    MEX_fixtures$mex_6_0 + MEX_fixtures$mex_6_1 + MEX_fixtures$mex_6_2 + MEX_fixtures$mex_6_3 + MEX_fixtures$mex_6_4 +
    MEX_fixtures$mex_6_5
)

MEX_fixtures$mex_H <- percent(MEX_fixtures$mex_H, accuracy = 0.1)

#Draw
MEX_fixtures$mex_D <- (

  MEX_fixtures$mex_0_0 + MEX_fixtures$mex_1_1 + MEX_fixtures$mex_2_2 + MEX_fixtures$mex_3_3 + MEX_fixtures$mex_4_4 +
    MEX_fixtures$mex_5_5 + MEX_fixtures$mex_6_6
)

MEX_fixtures$mex_D <- percent(MEX_fixtures$mex_D, accuracy = 0.1)

#Away

MEX_fixtures$mex_A <- (
  MEX_fixtures$mex_0_1 + MEX_fixtures$mex_0_2 + MEX_fixtures$mex_1_2 + MEX_fixtures$mex_0_3 + MEX_fixtures$mex_1_3 +
    MEX_fixtures$mex_2_3 + MEX_fixtures$mex_0_4 + MEX_fixtures$mex_1_4 + MEX_fixtures$mex_2_4 + MEX_fixtures$mex_3_4 +
    MEX_fixtures$mex_0_5 + MEX_fixtures$mex_1_5 + MEX_fixtures$mex_2_5 + MEX_fixtures$mex_3_5 + MEX_fixtures$mex_4_5 +
    MEX_fixtures$mex_0_6 + MEX_fixtures$mex_1_6 + MEX_fixtures$mex_2_6 + MEX_fixtures$mex_3_6 + MEX_fixtures$mex_4_6 +
    MEX_fixtures$mex_5_6
)

MEX_fixtures$mex_A <- percent(MEX_fixtures$mex_A, accuracy = 0.1)

#ov25
MEX_fixtures$mex_ov25 <- (
  MEX_fixtures$mex_2_1 + MEX_fixtures$mex_1_2 + MEX_fixtures$mex_2_2 + MEX_fixtures$mex_3_0 + MEX_fixtures$mex_3_1 +
    MEX_fixtures$mex_3_2 + MEX_fixtures$mex_0_3 + MEX_fixtures$mex_1_3 + MEX_fixtures$mex_2_3 + MEX_fixtures$mex_3_3 +
    MEX_fixtures$mex_4_0 + MEX_fixtures$mex_4_1 + MEX_fixtures$mex_4_2 + MEX_fixtures$mex_4_3 + MEX_fixtures$mex_0_4 +
    MEX_fixtures$mex_1_4 + MEX_fixtures$mex_2_4 + MEX_fixtures$mex_3_4 + MEX_fixtures$mex_4_4 + MEX_fixtures$mex_5_0 +
    MEX_fixtures$mex_5_1 + MEX_fixtures$mex_5_2 + MEX_fixtures$mex_5_3 + MEX_fixtures$mex_5_4 + MEX_fixtures$mex_0_5 +
    MEX_fixtures$mex_1_5 + MEX_fixtures$mex_2_5 + MEX_fixtures$mex_3_5 + MEX_fixtures$mex_4_5 + MEX_fixtures$mex_5_5 +
    MEX_fixtures$mex_6_0 + MEX_fixtures$mex_6_1 + MEX_fixtures$mex_6_2 + MEX_fixtures$mex_6_3 + MEX_fixtures$mex_6_4 +
    MEX_fixtures$mex_6_5 + MEX_fixtures$mex_0_6 + MEX_fixtures$mex_1_6 + MEX_fixtures$mex_2_6 + MEX_fixtures$mex_3_6 +
    MEX_fixtures$mex_4_6 + MEX_fixtures$mex_5_6 + MEX_fixtures$mex_6_6
)
#un25
MEX_fixtures$mex_un25 <- (
  MEX_fixtures$mex_0_0 + MEX_fixtures$mex_1_0 + MEX_fixtures$mex_0_1 + MEX_fixtures$mex_1_1 + MEX_fixtures$mex_2_0 + MEX_fixtures$mex_0_2
)
#odds
MEX_fixtures$mex_ov25_odds <- round((1/MEX_fixtures$mex_ov25),digits = 2)
MEX_fixtures$mex_un25_odds <- round((1/MEX_fixtures$mex_un25),digits = 2)

MEX_fixtures$mex_ov25_odds
MEX_fixtures$mex_un25_odds
#percentages
MEX_fixtures$mex_ov25 <- percent(MEX_fixtures$mex_ov25, accuracy = 0.1)

MEX_fixtures$mex_un25 <- percent(MEX_fixtures$mex_un25, accuracy = 0.1)
MEX_fixtures$mex_pscore <- paste(round(MEX_fixtures$mex_xGH,digits = 0),round(MEX_fixtures$mex_xGA,digits = 0),sep = "-")
#write out
write.xlsx(MEX_fixtures,'MEX.xlsx',sheetName = "MEX", append = TRUE)
###########################################################################################################
########################MEX END###########################################################################
MEX <- read.csv('../FDAS/MEX.csv')
MEX$TG <- MEX$HG + MEX$AG
MEX$OV25 <- ifelse(MEX$TG >= 3,"Y","N")
mex_ftr_summary <- tabyl(MEX,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
mex_ov25_summary <- tabyl(MEX,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(mex_ftr_summary,'MEX.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(mex_ov25_summary,'MEX.xlsx',sheetName = "OVUN25", append = TRUE)



