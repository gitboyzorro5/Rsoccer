library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('DNK.xlsx')
######################DNK START#######################################
#####################################################################
DNK <- read.csv('../FDAS/DNK.csv')
DNK <- within(DNK,rm(Res))
DNK$Date <- dmy(DNK$Date)
DNK <- DNK[order(as.Date(DNK$Date, format = "%d/%m%Y"), decreasing = FALSE),]
DNK$CS <- paste(DNK$HG,DNK$AG, sep = "-")
#DNK_qualificaton <- subset(DNK,tournament == "UEFA Euro qualification")
DNK <- subset(DNK,Season == "2020/2021")
#DNK <- DNK[DNK$Date > '2008-01-01',])
DNK$TG <- DNK$HG + DNK$AG
DNK$OV25 <- ifelse(DNK$TG >= 3,"Y","N")
DNK$FTR <- with(DNK,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
dnk_totalgoalsv2 <- tapply(DNK$TG, DNK[c("Home", "Away")],mean)
dnk_totalgoalsv2
dnk_hgtotals <- rowSums(dnk_totalgoalsv2,na.rm = T)
dnk_agtotals <- colSums(dnk_totalgoalsv2,na.rm = T)

dnk_totalgoals <- dnk_hgtotals + dnk_agtotals
dnk_totalgoalsv2 <- cbind(dnk_totalgoalsv2,dnk_totalgoals)
dnk_teams <- sort(unique(DNK$Home))
dnk_home_games <- c()
dnk_away_games <-c()
for (i_dnk in 1:length(dnk_teams))
{

  dnk_home_games[i_dnk] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk],])
  dnk_away_games[i_dnk]  <- nrow(DNK[DNK$Away == dnk_teams[i_dnk],])

}
dnk_games_played <- dnk_home_games + dnk_away_games
dnk_goaltotalsv2 <- cbind(dnk_totalgoalsv2,dnk_games_played)
dnk_avg_totalgoals <- round((dnk_totalgoals/ dnk_games_played), digits = 4)
dnk_goaltotalsv2[is.na(dnk_goaltotalsv2)] <- ""
dnk_goaltotalsv2 <- cbind(dnk_goaltotalsv2,dnk_avg_totalgoals)
write.xlsx(dnk_goaltotalsv2,'DNK.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
dnk_goalscored_h <- tapply(DNK$HG, DNK[c("Home", "Date")],mean)
dnk_goalscored_a <- tapply(DNK$AG, DNK[c("Away", "Date")],mean)
dnk_goalscored_h[is.na(dnk_goalscored_h)] <- ""
dnk_goalscored_a[is.na(dnk_goalscored_a)] <- ""

for(dnk_rowhgs in 1:nrow(dnk_goalscored_h)) {
  for(dnk_colhgs in 1:ncol(dnk_goalscored_h)) {

    # print(my_matrix[row, col])
    for(dnk_rowags in 1:nrow(dnk_goalscored_a)) {
      for(dnk_colags in 1:ncol(dnk_goalscored_a)) {
        ifelse(!dnk_goalscored_a[dnk_rowags,dnk_colags]=="",dnk_goalscored_h[dnk_rowags,dnk_colags] <- dnk_goalscored_a[dnk_rowags,dnk_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(dnk_goalscored_h,'DNK.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
dnk_goalconceded_h <- tapply(DNK$AG, DNK[c("Home", "Date")],mean)
dnk_goalconceded_a <- tapply(DNK$HG, DNK[c("Away", "Date")],mean)
dnk_goalconceded_h[is.na(dnk_goalconceded_h)] <- ""
dnk_goalconceded_a[is.na(dnk_goalconceded_a)] <- ""

for(dnk_rowhgc in 1:nrow(dnk_goalconceded_h)) {
  for(dnk_colhgc in 1:ncol(dnk_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(dnk_rowagc in 1:nrow(dnk_goalconceded_a)) {
      for(dnk_colagc in 1:ncol(dnk_goalconceded_a)) {
        ifelse(!dnk_goalconceded_a[dnk_rowagc,dnk_colagc]=="",dnk_goalconceded_h[dnk_rowagc,dnk_colagc] <- dnk_goalconceded_a[dnk_rowagc,dnk_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(dnk_goalconceded_h,'DNK.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
dnk_form_h <- tapply(DNK$FTR, DNK[c("Home", "Date")],median)
dnk_form_a <- tapply(DNK$FTR, DNK[c("Away", "Date")],median)
dnk_form_h[is.na(dnk_form_h)] <- ""
dnk_form_a[is.na(dnk_form_a)] <- ""
dnk_form_h <- sub("A","L",dnk_form_h)
dnk_form_h <- sub("H","W",dnk_form_h)
dnk_form_a <- sub("A","W",dnk_form_a)
dnk_form_a <- sub("H","L",dnk_form_a)
for(dnk_rowh_f in 1:nrow(dnk_form_h)) {
  for(dnk_colh_f in 1:ncol(dnk_form_h)) {

    # print(my_matrix[row, col])
    for(dnk_rowa_f in 1:nrow(dnk_form_a)) {
      for(dnk_cola_f in 1:ncol(dnk_form_a)) {
        ifelse(!dnk_form_a[dnk_rowa_f,dnk_cola_f]=="",dnk_form_h[dnk_rowa_f,dnk_cola_f] <- dnk_form_a[dnk_rowa_f,dnk_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(dnk_form_h,'DNK.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
dnk_totalgoals_h <- tapply(DNK$TG, DNK[c("Home", "Date")],mean)
dnk_totalgoals_a <- tapply(DNK$TG, DNK[c("Away", "Date")],mean)
dnk_totalgoals_h[is.na(dnk_totalgoals_h)] <- ""
dnk_totalgoals_a[is.na(dnk_totalgoals_a)] <- ""
for(dnk_rowh in 1:nrow(dnk_totalgoals_h)) {
  for(dnk_colh in 1:ncol(dnk_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(dnk_rowa in 1:nrow(dnk_totalgoals_a)) {
      for(dnk_cola in 1:ncol(dnk_totalgoals_a)) {
        ifelse(!dnk_totalgoals_a[dnk_rowa,dnk_cola]=="",dnk_totalgoals_h[dnk_rowa,dnk_cola] <- dnk_totalgoals_a[dnk_rowa,dnk_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(dnk_totalgoals_h,'DNK.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
dnk_form_team_against_h <- tapply(DNK$Away, DNK[c("Home", "Date")],median)
dnk_form_team_against_a <- tapply(DNK$Home, DNK[c("Away", "Date")],median)
dnk_form_team_against_h[is.na(dnk_form_team_against_h)] <- ""
dnk_form_team_against_a[is.na(dnk_form_team_against_a)] <- ""
for(dnk_rowh_f_against in 1:nrow(dnk_form_team_against_h)) {
  for(dnk_colh_f_against in 1:ncol(dnk_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(dnk_rowa_f_against in 1:nrow(dnk_form_team_against_a)) {
      for(dnk_cola_f_against in 1:ncol(dnk_form_team_against_a)) {
        ifelse(!dnk_form_team_against_a[dnk_rowa_f_against,dnk_cola_f_against]=="",dnk_form_team_against_h[dnk_rowa_f_against,dnk_cola_f_against] <- dnk_form_team_against_a[dnk_rowa_f_against,dnk_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#DNK
dnk_un05_home <- c()
dnk_un05_away <- c()
dnk_ov05_home <- c()
dnk_ov05_away <- c()

dnk_un15_home <- c()
dnk_un15_away <- c()
dnk_ov15_home <- c()
dnk_ov15_away <- c()

dnk_un25_home <- c()
dnk_un25_away <- c()
dnk_ov25_home <- c()
dnk_ov25_away <- c()

dnk_un35_home <- c()
dnk_un35_away <- c()
dnk_ov35_home <- c()
dnk_ov35_away <- c()

dnk_un45_home <- c()
dnk_un45_away <- c()
dnk_ov45_home <- c()
dnk_ov45_away <- c()

dnk_un55_home <- c()
dnk_un55_away <- c()
dnk_ov55_home <- c()
dnk_ov55_away <- c()

for (i_dnk_tg in 1:length(dnk_teams))
{

  dnk_un05_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG == 0,])
  dnk_un05_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG == 0,])

  dnk_ov05_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG > 0,])
  dnk_ov05_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG > 0,])

  dnk_un15_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG <= 1,])
  dnk_un15_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG <= 1,])

  dnk_ov15_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG >= 2,])
  dnk_ov15_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG >= 2,])

  dnk_un25_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG <= 2,])
  dnk_un25_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG <= 2,])

  dnk_ov25_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG >=3,])
  dnk_ov25_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG >=3,])

  dnk_un35_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG <= 3,])
  dnk_un35_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG <= 3,])

  dnk_ov35_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG >= 4,])
  dnk_ov35_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG >= 4,])

  dnk_un45_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG <= 4,])
  dnk_un45_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG <= 4,])

  dnk_ov45_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG >= 5,])
  dnk_ov45_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG >= 5,])

  dnk_un55_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG <= 5,])
  dnk_un55_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG <= 5,])

  dnk_ov55_home[i_dnk_tg] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_tg] & DNK$TG >= 6,])
  dnk_ov55_away[i_dnk_tg] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_tg] & DNK$TG >= 6,])


}

dnk_un05 <- dnk_un05_home + dnk_un05_away
dnk_ov05 <- dnk_ov05_home + dnk_ov05_away

dnk_un15 <- dnk_un15_home + dnk_un15_away
dnk_ov15 <- dnk_ov15_home + dnk_ov15_away

dnk_un25 <- dnk_un25_home + dnk_un25_away
dnk_ov25 <- dnk_ov25_home + dnk_ov25_away

dnk_un35 <- dnk_un35_home + dnk_un35_away
dnk_ov35 <- dnk_ov35_home + dnk_ov35_away

dnk_un45 <- dnk_un45_home + dnk_un45_away
dnk_ov45 <- dnk_ov45_home + dnk_ov45_away

dnk_un55 <- dnk_un55_home + dnk_un55_away
dnk_ov55 <- dnk_ov55_home + dnk_ov55_away

dnk_ovundata <- cbind(dnk_teams,dnk_un05,dnk_ov05,dnk_un15,dnk_ov15,dnk_un25,dnk_ov25,dnk_un35,dnk_ov35,dnk_un45,dnk_ov45,dnk_un55,dnk_ov55)
write.xlsx(dnk_ovundata,'DNK.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
dnk_csform_h <- tapply(DNK$CS, DNK[c("Home", "Date")],median)
dnk_csform_a <- tapply(DNK$CS, DNK[c("Away", "Date")],median)

dnk_csform_h[is.na(dnk_csform_h)] <- ""
dnk_csform_a[is.na(dnk_csform_a)] <- ""

for(dnk_rowh_f_cs in 1:nrow(dnk_csform_h)) {
  for(dnk_colh_f_cs in 1:ncol(dnk_csform_h)) {

    # print(my_matrix[row, col])
    for(dnk_rowa_f_cs in 1:nrow(dnk_csform_a)) {
      for(dnk_cola_f_cs in 1:ncol(dnk_csform_a)) {
        ifelse(!dnk_csform_a[dnk_rowa_f_cs,dnk_cola_f_cs]=="",dnk_csform_h[dnk_rowa_f_cs,dnk_cola_f_cs] <- dnk_csform_a[dnk_rowa_f_cs,dnk_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
dnk_home_gs <- aggregate(DNK$HG, by = list(DNK$Home), FUN = sum)
dnk_home_gs_avg <- aggregate(DNK$HG, by = list(DNK$Home),mean)
dnk_home_scoring <- merge(dnk_home_gs,dnk_home_gs_avg, by='Group.1',all = T)
names(dnk_home_scoring)[names(dnk_home_scoring) == "x.x"] <- "TFthg"
names(dnk_home_scoring)[names(dnk_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
dnk_away_gs <- aggregate(DNK$AG, by = list(DNK$Away), FUN = sum)
dnk_away_gs_avg <- aggregate(DNK$AG, by = list(DNK$Away),mean)
dnk_away_scoring <- merge(dnk_away_gs,dnk_away_gs_avg, by='Group.1',all = T)
names(dnk_away_scoring)[names(dnk_away_scoring) == "x.x"] <- "TFtag"
names(dnk_away_scoring)[names(dnk_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
dnk_scoring <- merge(dnk_home_scoring,dnk_away_scoring,by='Group.1',all = T)
dnk_scoring$TGS <- dnk_scoring$TFthg + dnk_scoring$TFtag

#home goals conceded
dnk_home_gc <- aggregate(DNK$AG, by = list(DNK$Home), FUN = sum)
dnk_home_gc_avg <- aggregate(DNK$AG, by = list(DNK$Home),mean)
dnk_home_conceding <- merge(dnk_home_gc,dnk_home_gc_avg, by='Group.1',all = T)
names(dnk_home_conceding)[names(dnk_home_conceding) == "x.x"] <- "TFthc"
names(dnk_home_conceding)[names(dnk_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
dnk_away_gc <- aggregate(DNK$HG, by = list(DNK$Away), FUN = sum)
dnk_away_gc_avg <- aggregate(DNK$HG, by = list(DNK$Away),mean)
dnk_away_conceding <- merge(dnk_away_gc,dnk_away_gc_avg, by='Group.1',all = T)
names(dnk_away_conceding)[names(dnk_away_conceding) == "x.x"] <- "TFtac"
names(dnk_away_conceding)[names(dnk_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
dnk_conceding <- merge(dnk_home_conceding,dnk_away_conceding,by='Group.1',all = T)
dnk_conceding$TGC <- dnk_conceding$TFthc + dnk_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
dnk_home_wins <- c()
dnk_away_wins <- c()
dnk_home_draws <- c()
dnk_away_draws <- c()
dnk_home_loss <- c()
dnk_away_loss <- c()



for (i_dnk_wins in 1:length(dnk_teams))
{

  dnk_home_wins[i_dnk_wins] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_wins] & DNK$FTR == "H",])
  dnk_away_wins[i_dnk_wins] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_wins] & DNK$FTR == "A",])
  dnk_home_draws[i_dnk_wins] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_wins] & DNK$FTR == "D",])
  dnk_away_draws[i_dnk_wins] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_wins] & DNK$FTR == "D",])
  dnk_home_loss[i_dnk_wins] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk_wins] & DNK$FTR == "A",])
  dnk_away_loss[i_dnk_wins] <- nrow(DNK[DNK$Away == dnk_teams[i_dnk_wins] & DNK$FTR == "H",])

}

dnk_total_wins <- dnk_home_wins + dnk_away_wins
dnk_total_draws <- dnk_home_draws + dnk_away_draws
dnk_total_loss <- dnk_home_loss + dnk_away_loss

dnk_league_table <- cbind(dnk_teams,dnk_games_played,dnk_total_wins,dnk_total_draws,dnk_total_loss)
dnk_GS <- dnk_scoring$TGS
dnk_GC <-dnk_conceding$TGC
dnk_GD <- dnk_scoring$TGS - dnk_conceding$TGC
dnk_PTS <- (dnk_total_wins*3) + (dnk_total_draws*1)
dnk_league_table <- cbind(dnk_league_table,dnk_GS,dnk_GC,dnk_GD,dnk_PTS)
dnk_league_table <- as.data.frame(dnk_league_table)
#rename the columns
names(dnk_league_table)[names(dnk_league_table) == "dnk_teams"] <- "Team"
names(dnk_league_table)[names(dnk_league_table) == "dnk_games_played"] <- "P"
names(dnk_league_table)[names(dnk_league_table) == "dnk_total_wins"] <- "W"
names(dnk_league_table)[names(dnk_league_table) == "dnk_total_draws"] <- "D"
names(dnk_league_table)[names(dnk_league_table) == "dnk_total_loss"] <- "L"
names(dnk_league_table)[names(dnk_league_table) == "dnk_GS"] <- "F"
names(dnk_league_table)[names(dnk_league_table) == "dnk_GC"] <- "A"
points_dnk <- dnk_league_table[order(dnk_league_table$dnk_PTS, decreasing = TRUE),]
write.xlsx(points_dnk,'DNK.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six dnk###################################################
#DNK
#form
#create final_dnk_hf object
dnk_last_n_games <- 6
final_dnk_hf <- c()
for(index_dnk_hf in 1:length(dnk_teams))
{
  index_dnk_hf <- row.names(dnk_form_h) == dnk_teams[index_dnk_hf]
  form_dnk_hf <- dnk_form_h[index_dnk_hf]
  deleted_form_dnk_hf <- form_dnk_hf[!form_dnk_hf[] == ""]
  l6_form_dnk_hf <- tail(deleted_form_dnk_hf,dnk_last_n_games)
  l6_form_dnk_hf <- paste(l6_form_dnk_hf,collapse = " ")
  final_dnk_hf[index_dnk_hf] <- rbind(paste(dnk_teams[index_dnk_hf],l6_form_dnk_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",dnk_teams[index],l6_form)

}

#change column names
final_dnk_hf <- as.data.frame(final_dnk_hf)
colnames(final_dnk_hf) <- "Form"
#goals scored
#create final_dnk_gs object
final_dnk_gs <- c()
suml6_dnk_gs <- c()
for(index_dnk_gs in 1:length(dnk_teams))
{
  index_dnk_gs <- row.names(dnk_goalscored_h) == dnk_teams[index_dnk_gs]
  form_dnk_gs <- dnk_goalscored_h[index_dnk_gs]
  deleted_form_dnk_gs <- form_dnk_gs[!form_dnk_gs[] == ""]
  l6_form_dnk_gs <- tail(deleted_form_dnk_gs,dnk_last_n_games)
  l6_form_dnk_gs <- as.numeric(l6_form_dnk_gs)
  suml6_dnk_gs[index_dnk_gs] <- sum(l6_form_dnk_gs)
  suml6_dnk_gs[index_dnk_gs] <- paste("(",suml6_dnk_gs[index_dnk_gs],")",sep = "")
  l6_form_dnk_gs <- paste(l6_form_dnk_gs,collapse = " ")
  final_dnk_gs[index_dnk_gs] <- rbind(paste(dnk_teams[index_dnk_gs],l6_form_dnk_gs,suml6_dnk_gs[index_dnk_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",dnk_teams[index],l6_form)

}
final_dnk_gs
#change column names
final_dnk_gs <- as.data.frame(final_dnk_gs)
colnames(final_dnk_gs) <- "Goals scored"
#goal conceded
#create final_dnk_gc object
final_dnk_gc <- c()
suml6_dnk_gc <- c()
for(index_dnk_gc in 1:length(dnk_teams))
{
  index_dnk_gc <- row.names(dnk_goalconceded_h) == dnk_teams[index_dnk_gc]
  form_dnk_gc <- dnk_goalconceded_h[index_dnk_gc]
  deleted_form_dnk_gc <- form_dnk_gc[!form_dnk_gc[] == ""]
  l6_form_dnk_gc <- tail(deleted_form_dnk_gc,dnk_last_n_games)
  l6_form_dnk_gc <- as.numeric(l6_form_dnk_gc)
  suml6_dnk_gc[index_dnk_gc] <- sum(l6_form_dnk_gc)
  suml6_dnk_gc[index_dnk_gc] <- paste("(",suml6_dnk_gc[index_dnk_gc],")",sep = "")
  l6_form_dnk_gc <- paste(l6_form_dnk_gc,collapse = " ")
  final_dnk_gc[index_dnk_gc] <- rbind(paste(dnk_teams[index_dnk_gc],l6_form_dnk_gc,suml6_dnk_gc[index_dnk_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",dnk_teams[index],l6_form)

}

#change column names
final_dnk_gc <- as.data.frame(final_dnk_gc)
colnames(final_dnk_gc) <- "Goals conceded"
#total goals
#create final_dnk_tg object
final_dnk_tg <- c()
suml6_dnk_tg <- c()
for(index_dnk_tg in 1:length(dnk_teams))
{
  index_dnk_tg <- row.names(dnk_totalgoals_h) == dnk_teams[index_dnk_tg]
  form_dnk_tg <- dnk_totalgoals_h[index_dnk_tg]
  deleted_form_dnk_tg <- form_dnk_tg[!form_dnk_tg[] == ""]
  l6_form_dnk_tg <- tail(deleted_form_dnk_tg,dnk_last_n_games)
  l6_form_dnk_tg <- as.numeric(l6_form_dnk_tg)
  suml6_dnk_tg[index_dnk_tg] <- sum(l6_form_dnk_tg)
  suml6_dnk_tg[index_dnk_tg] <- paste("(",suml6_dnk_tg[index_dnk_tg],")",sep = "")
  l6_form_dnk_tg <- paste(l6_form_dnk_tg,collapse = " ")
  final_dnk_tg[index_dnk_tg] <- rbind(paste(dnk_teams[index_dnk_tg],l6_form_dnk_tg,suml6_dnk_tg[index_dnk_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",dnk_teams[index],l6_form)

}
#change column names
final_dnk_tg <- as.data.frame(final_dnk_tg)
colnames(final_dnk_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_dnk_hf object
final_dnk_cs <- c()
for(index_dnk_cs in 1:length(dnk_teams))
{
  index_dnk_cs <- row.names(dnk_csform_h) == dnk_teams[index_dnk_cs]
  csform_dnk_cs <- dnk_csform_h[index_dnk_cs]
  deleted_csform_dnk_cs <- csform_dnk_cs[!csform_dnk_cs[] == ""]
  l6_csform_dnk_cs <- tail(deleted_csform_dnk_cs,dnk_last_n_games)
  l6_csform_dnk_cs <- paste(l6_csform_dnk_cs,collapse = " ")
  final_dnk_cs[index_dnk_cs] <- rbind(paste(dnk_teams[index_dnk_cs],l6_csform_dnk_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",dnk_teams[index],l6_csform)

}

#change column names
final_dnk_cs <- as.data.frame(final_dnk_cs)
colnames(final_dnk_cs) <- "CSForm"
#################################################
#Team against
#create final_dnk_hf_against
final_dnk_hf_against <- c()
for(index_dnk_hf_against in 1:length(dnk_teams))
{
  index_dnk_hf_against <- row.names(dnk_form_team_against_h) == dnk_teams[index_dnk_hf_against]
  form_dnk_hf_against <- dnk_form_team_against_h[index_dnk_hf_against]
  deleted_form_dnk_hf_against <- form_dnk_hf_against[!form_dnk_hf_against[] == ""]
  l6_form_dnk_hf_against <- tail(deleted_form_dnk_hf_against,dnk_last_n_games)
  l6_form_dnk_hf_against <- paste(l6_form_dnk_hf_against,collapse = " ")
  final_dnk_hf_against[index_dnk_hf_against] <- rbind(paste(dnk_teams[index_dnk_hf_against],l6_form_dnk_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",dnk_teams[index],l6_form)

}
final_dnk_hf_against <- as.data.frame(final_dnk_hf_against)
colnames(final_dnk_hf_against) <- "Team against"
#combine the columns
final_dnk_all <- cbind(final_dnk_hf,final_dnk_gs,final_dnk_gc,final_dnk_tg,final_dnk_cs,final_dnk_hf_against)
write.xlsx(final_dnk_all,'DNK.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
dnk_GP <- nrow(DNK)
#Calculate total home goals for each division
dnk_T_HG <- sum(dnk_home_gs$x)
#calculate average home goal
dnk_avg_HG <- round(dnk_T_HG /dnk_GP, digits = 4)
############################################################
#Calculate total away goals for each division
dnk_T_AG <- sum(dnk_away_gs$x)
#calculate average away goal
dnk_avg_AG <- round(dnk_T_AG /dnk_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
dnk_home_as <- round(((dnk_home_gs$x/dnk_home_games))/dnk_avg_HG, digits = 4)
#calculate away attack strength
dnk_away_as <- round(((dnk_away_gs$x/dnk_away_games))/dnk_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
dnk_avg_HC <- round(dnk_T_AG /dnk_GP, digits = 4)
#avg away concede
dnk_avg_AC <- round(dnk_T_HG /dnk_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
dnk_home_ds <- round(((dnk_home_gc$x/dnk_home_games))/dnk_avg_HC, digits = 4)
#away defense strength
dnk_away_ds <- round(((dnk_away_gc$x/dnk_away_games))/dnk_avg_AC, digits = 4)
#############################################################################
#home poisson data
#dnk
dnk_division <- c()
dnk_division[1:length(dnk_teams)] <- "DNK"
dnk_home_poisson <- cbind(dnk_division,dnk_teams,dnk_avg_HG,dnk_home_as,dnk_home_ds)
#################################################################################
#away poisson data
#dnk
dnk_division <- c()
dnk_division[1:length(dnk_teams)] <- "DNK"
dnk_away_poisson <- cbind(dnk_division,dnk_teams,dnk_avg_AG,dnk_away_as,dnk_away_ds)

#create home and away csv
#dnk_home_poisson <- rbind(dnk_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#dnk_away_poisson <- rbind(dnk_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(dnk_home_poisson,'DNK.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(dnk_away_poisson,'DNK.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################DNK FIXTURES##########################################################################
#DNK
HomeTeam_dnk <- rep(dnk_teams, each = length(dnk_teams))
AwayTeam_dnk <- rep(dnk_teams, length(dnk_teams))
DNK_fixtures <- cbind(HomeTeam_dnk,AwayTeam_dnk)
DNK_fixtures <- as.data.frame(DNK_fixtures)
DNK_fixtures <- DNK_fixtures[!DNK_fixtures$HomeTeam_dnk == DNK_fixtures$AwayTeam_dnk,]
rownames(DNK_fixtures) <- NULL
DNK_fixtures$Div <- "DNK"
DNK_fixtures <- DNK_fixtures[,c(3,1,2)]

DNK_fixtures$avg_HG_dnk <- dnk_avg_HG

DNK_fixtures$dnk_homeas <- rep(dnk_home_as,each = length(dnk_teams)-1)

dnk_awayds_lookup <- cbind(dnk_teams,dnk_away_ds)

dnk_awayds_lookup <- as.data.frame(dnk_awayds_lookup)

colnames(dnk_awayds_lookup) <- c("AwayTeam_dnk","dnk_awayds")


require('RH2')
DNK_fixtures$dnk_awayds <- sqldf("SELECT dnk_awayds_lookup.dnk_awayds FROM dnk_awayds_lookup INNER JOIN DNK_fixtures ON dnk_awayds_lookup.AwayTeam_dnk = DNK_fixtures.AwayTeam_dnk")

DNK_fixtures$avg_AG_dnk <- dnk_avg_AG

dnk_awayas_lookup <- cbind(dnk_teams,dnk_away_as)

dnk_awayas_lookup <- as.data.frame(dnk_awayas_lookup)

colnames(dnk_awayas_lookup) <- c("AwayTeam_dnk","dnk_awayas")


DNK_fixtures$dnk_awayas <- sqldf("SELECT dnk_awayas_lookup.dnk_awayas FROM dnk_awayas_lookup INNER JOIN DNK_fixtures ON dnk_awayas_lookup.AwayTeam_dnk = DNK_fixtures.AwayTeam_dnk")

DNK_fixtures$dnk_homeds <- rep(dnk_home_ds,each = length(dnk_teams)-1)

DNK_fixtures$dnk_awayds <- as.numeric(unlist(DNK_fixtures$dnk_awayds))
#xGH
DNK_fixtures$dnk_xGH <- DNK_fixtures$avg_HG_dnk * DNK_fixtures$dnk_homeas * DNK_fixtures$dnk_awayds

#xGA

DNK_fixtures$dnk_awayas <- as.numeric(unlist(DNK_fixtures$dnk_awayas))

DNK_fixtures$dnk_xGA <- DNK_fixtures$avg_AG_dnk * DNK_fixtures$dnk_awayas * DNK_fixtures$dnk_homeds

DNK_fixtures$dnk_0_0 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_0 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_0_1 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_1 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_0 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_0_2 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_2 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_1 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_2 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_3 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_0 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_1 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_2 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_0_3 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_3 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_3 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_4 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_0 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_1 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_2 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_3 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_0_4 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_4 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_4 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_4 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_5 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_0 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_1 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_2 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_3 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_4 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_0_5 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_5 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_5 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_5 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_5 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_6 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_0 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(0,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_1 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(1,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_2 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(2,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_3 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(3,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_4 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(4,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_6_5 <- round(stats::dpois(6,DNK_fixtures$dnk_xGH) * stats::dpois(5,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_0_6 <- round(stats::dpois(0,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_1_6 <- round(stats::dpois(1,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_2_6 <- round(stats::dpois(2,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_3_6 <- round(stats::dpois(3,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_4_6 <- round(stats::dpois(4,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
DNK_fixtures$dnk_5_6 <- round(stats::dpois(5,DNK_fixtures$dnk_xGH) * stats::dpois(6,DNK_fixtures$dnk_xGA), digits = 4)
#Home win
DNK_fixtures$dnk_H <- (
  DNK_fixtures$dnk_1_0 + DNK_fixtures$dnk_2_0 + DNK_fixtures$dnk_2_1 + DNK_fixtures$dnk_3_0 + DNK_fixtures$dnk_3_1 +
    DNK_fixtures$dnk_3_2 + DNK_fixtures$dnk_4_0 + DNK_fixtures$dnk_4_1 + DNK_fixtures$dnk_4_2 + DNK_fixtures$dnk_4_3 +
    DNK_fixtures$dnk_5_0 + DNK_fixtures$dnk_5_1 + DNK_fixtures$dnk_5_2 + DNK_fixtures$dnk_5_3 + DNK_fixtures$dnk_5_4 +
    DNK_fixtures$dnk_6_0 + DNK_fixtures$dnk_6_1 + DNK_fixtures$dnk_6_2 + DNK_fixtures$dnk_6_3 + DNK_fixtures$dnk_6_4 +
    DNK_fixtures$dnk_6_5
)

DNK_fixtures$dnk_H <- percent(DNK_fixtures$dnk_H, accuracy = 0.1)

#Draw
DNK_fixtures$dnk_D <- (

  DNK_fixtures$dnk_0_0 + DNK_fixtures$dnk_1_1 + DNK_fixtures$dnk_2_2 + DNK_fixtures$dnk_3_3 + DNK_fixtures$dnk_4_4 +
    DNK_fixtures$dnk_5_5 + DNK_fixtures$dnk_6_6
)

DNK_fixtures$dnk_D <- percent(DNK_fixtures$dnk_D, accuracy = 0.1)

#Away

DNK_fixtures$dnk_A <- (
  DNK_fixtures$dnk_0_1 + DNK_fixtures$dnk_0_2 + DNK_fixtures$dnk_1_2 + DNK_fixtures$dnk_0_3 + DNK_fixtures$dnk_1_3 +
    DNK_fixtures$dnk_2_3 + DNK_fixtures$dnk_0_4 + DNK_fixtures$dnk_1_4 + DNK_fixtures$dnk_2_4 + DNK_fixtures$dnk_3_4 +
    DNK_fixtures$dnk_0_5 + DNK_fixtures$dnk_1_5 + DNK_fixtures$dnk_2_5 + DNK_fixtures$dnk_3_5 + DNK_fixtures$dnk_4_5 +
    DNK_fixtures$dnk_0_6 + DNK_fixtures$dnk_1_6 + DNK_fixtures$dnk_2_6 + DNK_fixtures$dnk_3_6 + DNK_fixtures$dnk_4_6 +
    DNK_fixtures$dnk_5_6
)

DNK_fixtures$dnk_A <- percent(DNK_fixtures$dnk_A, accuracy = 0.1)

#ov25
DNK_fixtures$dnk_ov25 <- (
  DNK_fixtures$dnk_2_1 + DNK_fixtures$dnk_1_2 + DNK_fixtures$dnk_2_2 + DNK_fixtures$dnk_3_0 + DNK_fixtures$dnk_3_1 +
    DNK_fixtures$dnk_3_2 + DNK_fixtures$dnk_0_3 + DNK_fixtures$dnk_1_3 + DNK_fixtures$dnk_2_3 + DNK_fixtures$dnk_3_3 +
    DNK_fixtures$dnk_4_0 + DNK_fixtures$dnk_4_1 + DNK_fixtures$dnk_4_2 + DNK_fixtures$dnk_4_3 + DNK_fixtures$dnk_0_4 +
    DNK_fixtures$dnk_1_4 + DNK_fixtures$dnk_2_4 + DNK_fixtures$dnk_3_4 + DNK_fixtures$dnk_4_4 + DNK_fixtures$dnk_5_0 +
    DNK_fixtures$dnk_5_1 + DNK_fixtures$dnk_5_2 + DNK_fixtures$dnk_5_3 + DNK_fixtures$dnk_5_4 + DNK_fixtures$dnk_0_5 +
    DNK_fixtures$dnk_1_5 + DNK_fixtures$dnk_2_5 + DNK_fixtures$dnk_3_5 + DNK_fixtures$dnk_4_5 + DNK_fixtures$dnk_5_5 +
    DNK_fixtures$dnk_6_0 + DNK_fixtures$dnk_6_1 + DNK_fixtures$dnk_6_2 + DNK_fixtures$dnk_6_3 + DNK_fixtures$dnk_6_4 +
    DNK_fixtures$dnk_6_5 + DNK_fixtures$dnk_0_6 + DNK_fixtures$dnk_1_6 + DNK_fixtures$dnk_2_6 + DNK_fixtures$dnk_3_6 +
    DNK_fixtures$dnk_4_6 + DNK_fixtures$dnk_5_6 + DNK_fixtures$dnk_6_6
)
#un25
DNK_fixtures$dnk_un25 <- (
  DNK_fixtures$dnk_0_0 + DNK_fixtures$dnk_1_0 + DNK_fixtures$dnk_0_1 + DNK_fixtures$dnk_1_1 + DNK_fixtures$dnk_2_0 + DNK_fixtures$dnk_0_2
)
#odds
DNK_fixtures$dnk_ov25_odds <- round((1/DNK_fixtures$dnk_ov25),digits = 2)
DNK_fixtures$dnk_un25_odds <- round((1/DNK_fixtures$dnk_un25),digits = 2)

DNK_fixtures$dnk_ov25_odds
DNK_fixtures$dnk_un25_odds
#percentages
DNK_fixtures$dnk_ov25 <- percent(DNK_fixtures$dnk_ov25, accuracy = 0.1)

DNK_fixtures$dnk_un25 <- percent(DNK_fixtures$dnk_un25, accuracy = 0.1)
DNK_fixtures$dnk_pscore <- paste(round(DNK_fixtures$dnk_xGH,digits = 0),round(DNK_fixtures$dnk_xGA,digits = 0),sep = "-")
#write out
write.xlsx(DNK_fixtures,'DNK.xlsx',sheetName = "DNK", append = TRUE)
###########################################################################################################
########################DNK END###########################################################################
DNK <- read.csv('../FDAS/DNK.csv')
DNK$TG <- DNK$HG + DNK$AG
DNK$OV25 <- ifelse(DNK$TG >= 3,"Y","N")
dnk_ftr_summary <- tabyl(DNK,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
dnk_ov25_summary <- tabyl(DNK,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(dnk_ftr_summary,'DNK.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(dnk_ov25_summary,'DNK.xlsx',sheetName = "OVUN25", append = TRUE)



