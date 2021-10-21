library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('AUT.xlsx')
######################AUT START#######################################
#####################################################################
AUT <- read.csv('../FDAS/AUT.csv')
AUT <- within(AUT,rm(Res))
AUT$Date <- dmy(AUT$Date)
AUT <- AUT[order(as.Date(AUT$Date, format = "%d/%m%Y"), decreasing = FALSE),]
AUT$CS <- paste(AUT$HG,AUT$AG, sep = "-")

#AUT_qualificaton <- subset(AUT,tournament == "UEFA Euro qualification")
AUT <- subset(AUT,Season == "2021/2022")
#AUT <- AUT[AUT$Date > '2008-01-01',])
AUT$TG <- AUT$HG + AUT$AG
AUT$OV25 <- ifelse(AUT$TG >= 3,"Y","N")
AUT$FTR <- with(AUT,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
aut_totalgoalsv2 <- tapply(AUT$TG, AUT[c("Home", "Away")],mean)
aut_totalgoalsv2
aut_hgtotals <- rowSums(aut_totalgoalsv2,na.rm = T)
aut_agtotals <- colSums(aut_totalgoalsv2,na.rm = T)

aut_totalgoals <- aut_hgtotals + aut_agtotals
aut_totalgoalsv2 <- cbind(aut_totalgoalsv2,aut_totalgoals)
aut_teams <- sort(unique(AUT$Home))
aut_home_games <- c()
aut_away_games <-c()
for (i_aut in 1:length(aut_teams))
{

  aut_home_games[i_aut] <- nrow(AUT[AUT$Home == aut_teams[i_aut],])
  aut_away_games[i_aut]  <- nrow(AUT[AUT$Away == aut_teams[i_aut],])

}
aut_games_played <- aut_home_games + aut_away_games
aut_goaltotalsv2 <- cbind(aut_totalgoalsv2,aut_games_played)
aut_avg_totalgoals <- round((aut_totalgoals/ aut_games_played), digits = 4)
aut_goaltotalsv2[is.na(aut_goaltotalsv2)] <- ""
aut_goaltotalsv2 <- cbind(aut_goaltotalsv2,aut_avg_totalgoals)
write.xlsx(aut_goaltotalsv2,'AUT.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
aut_goalscored_h <- tapply(AUT$HG, AUT[c("Home", "Date")],mean)
aut_goalscored_a <- tapply(AUT$AG, AUT[c("Away", "Date")],mean)
aut_goalscored_h[is.na(aut_goalscored_h)] <- ""
aut_goalscored_a[is.na(aut_goalscored_a)] <- ""

for(aut_rowhgs in 1:nrow(aut_goalscored_h)) {
  for(aut_colhgs in 1:ncol(aut_goalscored_h)) {

    # print(my_matrix[row, col])
    for(aut_rowags in 1:nrow(aut_goalscored_a)) {
      for(aut_colags in 1:ncol(aut_goalscored_a)) {
        ifelse(!aut_goalscored_a[aut_rowags,aut_colags]=="",aut_goalscored_h[aut_rowags,aut_colags] <- aut_goalscored_a[aut_rowags,aut_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(aut_goalscored_h,'AUT.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
aut_goalconceded_h <- tapply(AUT$AG, AUT[c("Home", "Date")],mean)
aut_goalconceded_a <- tapply(AUT$HG, AUT[c("Away", "Date")],mean)
aut_goalconceded_h[is.na(aut_goalconceded_h)] <- ""
aut_goalconceded_a[is.na(aut_goalconceded_a)] <- ""

for(aut_rowhgc in 1:nrow(aut_goalconceded_h)) {
  for(aut_colhgc in 1:ncol(aut_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(aut_rowagc in 1:nrow(aut_goalconceded_a)) {
      for(aut_colagc in 1:ncol(aut_goalconceded_a)) {
        ifelse(!aut_goalconceded_a[aut_rowagc,aut_colagc]=="",aut_goalconceded_h[aut_rowagc,aut_colagc] <- aut_goalconceded_a[aut_rowagc,aut_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(aut_goalconceded_h,'AUT.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
aut_form_h <- tapply(AUT$FTR, AUT[c("Home", "Date")],median)
aut_form_a <- tapply(AUT$FTR, AUT[c("Away", "Date")],median)
aut_form_h[is.na(aut_form_h)] <- ""
aut_form_a[is.na(aut_form_a)] <- ""
aut_form_h <- sub("A","L",aut_form_h)
aut_form_h <- sub("H","W",aut_form_h)
aut_form_a <- sub("A","W",aut_form_a)
aut_form_a <- sub("H","L",aut_form_a)
for(aut_rowh_f in 1:nrow(aut_form_h)) {
  for(aut_colh_f in 1:ncol(aut_form_h)) {

    # print(my_matrix[row, col])
    for(aut_rowa_f in 1:nrow(aut_form_a)) {
      for(aut_cola_f in 1:ncol(aut_form_a)) {
        ifelse(!aut_form_a[aut_rowa_f,aut_cola_f]=="",aut_form_h[aut_rowa_f,aut_cola_f] <- aut_form_a[aut_rowa_f,aut_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(aut_form_h,'AUT.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
aut_totalgoals_h <- tapply(AUT$TG, AUT[c("Home", "Date")],mean)
aut_totalgoals_a <- tapply(AUT$TG, AUT[c("Away", "Date")],mean)
aut_totalgoals_h[is.na(aut_totalgoals_h)] <- ""
aut_totalgoals_a[is.na(aut_totalgoals_a)] <- ""
for(aut_rowh in 1:nrow(aut_totalgoals_h)) {
  for(aut_colh in 1:ncol(aut_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(aut_rowa in 1:nrow(aut_totalgoals_a)) {
      for(aut_cola in 1:ncol(aut_totalgoals_a)) {
        ifelse(!aut_totalgoals_a[aut_rowa,aut_cola]=="",aut_totalgoals_h[aut_rowa,aut_cola] <- aut_totalgoals_a[aut_rowa,aut_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(aut_totalgoals_h,'AUT.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
aut_form_team_against_h <- tapply(AUT$Away, AUT[c("Home", "Date")],median)
aut_form_team_against_a <- tapply(AUT$Home, AUT[c("Away", "Date")],median)
aut_form_team_against_h[is.na(aut_form_team_against_h)] <- ""
aut_form_team_against_a[is.na(aut_form_team_against_a)] <- ""
for(aut_rowh_f_against in 1:nrow(aut_form_team_against_h)) {
  for(aut_colh_f_against in 1:ncol(aut_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(aut_rowa_f_against in 1:nrow(aut_form_team_against_a)) {
      for(aut_cola_f_against in 1:ncol(aut_form_team_against_a)) {
        ifelse(!aut_form_team_against_a[aut_rowa_f_against,aut_cola_f_against]=="",aut_form_team_against_h[aut_rowa_f_against,aut_cola_f_against] <- aut_form_team_against_a[aut_rowa_f_against,aut_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
#win margin
aut_winmargin_h <- tapply(AUT$HG - AUT$AG, AUT[c("Home", "Date")],mean)
aut_winmargin_a <- tapply(AUT$AG - AUT$HG, AUT[c("Away", "Date")],mean)
aut_winmargin_h[is.na(aut_winmargin_h)] <- ""
#
for(aut_rowhwm in 1:nrow(aut_winmargin_h)) {
  for(aut_colhwm in 1:ncol(aut_winmargin_h)) {

    # print(my_matrix[row, col])
    for(aut_rowawm in 1:nrow(aut_winmargin_a)) {
      for(aut_colawm in 1:ncol(aut_winmargin_a)) {
        ifelse(!aut_winmargin_a[aut_rowawm,aut_colawm]=="",aut_winmargin_h[aut_rowawm,aut_colawm] <- aut_winmargin_a[aut_rowawm,aut_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
####################################################################################################################
##########Goals over under############
#AUT
aut_un05_home <- c()
aut_un05_away <- c()
aut_ov05_home <- c()
aut_ov05_away <- c()

aut_un15_home <- c()
aut_un15_away <- c()
aut_ov15_home <- c()
aut_ov15_away <- c()

aut_un25_home <- c()
aut_un25_away <- c()
aut_ov25_home <- c()
aut_ov25_away <- c()

aut_un35_home <- c()
aut_un35_away <- c()
aut_ov35_home <- c()
aut_ov35_away <- c()

aut_un45_home <- c()
aut_un45_away <- c()
aut_ov45_home <- c()
aut_ov45_away <- c()

aut_un55_home <- c()
aut_un55_away <- c()
aut_ov55_home <- c()
aut_ov55_away <- c()

for (i_aut_tg in 1:length(aut_teams))
{

  aut_un05_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG == 0,])
  aut_un05_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG == 0,])

  aut_ov05_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG > 0,])
  aut_ov05_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG > 0,])

  aut_un15_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG <= 1,])
  aut_un15_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG <= 1,])

  aut_ov15_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG >= 2,])
  aut_ov15_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG >= 2,])

  aut_un25_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG <= 2,])
  aut_un25_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG <= 2,])

  aut_ov25_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG >=3,])
  aut_ov25_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG >=3,])

  aut_un35_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG <= 3,])
  aut_un35_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG <= 3,])

  aut_ov35_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG >= 4,])
  aut_ov35_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG >= 4,])

  aut_un45_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG <= 4,])
  aut_un45_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG <= 4,])

  aut_ov45_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG >= 5,])
  aut_ov45_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG >= 5,])

  aut_un55_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG <= 5,])
  aut_un55_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG <= 5,])

  aut_ov55_home[i_aut_tg] <- nrow(AUT[AUT$Home == aut_teams[i_aut_tg] & AUT$TG >= 6,])
  aut_ov55_away[i_aut_tg] <- nrow(AUT[AUT$Away == aut_teams[i_aut_tg] & AUT$TG >= 6,])


}

aut_un05 <- aut_un05_home + aut_un05_away
aut_ov05 <- aut_ov05_home + aut_ov05_away

aut_un15 <- aut_un15_home + aut_un15_away
aut_ov15 <- aut_ov15_home + aut_ov15_away

aut_un25 <- aut_un25_home + aut_un25_away
aut_ov25 <- aut_ov25_home + aut_ov25_away

aut_un35 <- aut_un35_home + aut_un35_away
aut_ov35 <- aut_ov35_home + aut_ov35_away

aut_un45 <- aut_un45_home + aut_un45_away
aut_ov45 <- aut_ov45_home + aut_ov45_away

aut_un55 <- aut_un55_home + aut_un55_away
aut_ov55 <- aut_ov55_home + aut_ov55_away

aut_ovundata <- cbind(aut_teams,aut_un05,aut_ov05,aut_un15,aut_ov15,aut_un25,aut_ov25,aut_un35,aut_ov35,aut_un45,aut_ov45,aut_un55,aut_ov55)
write.xlsx(aut_ovundata,'AUT.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
aut_csform_h <- tapply(AUT$CS, AUT[c("Home", "Date")],median)
aut_csform_a <- tapply(AUT$CS, AUT[c("Away", "Date")],median)

aut_csform_h[is.na(aut_csform_h)] <- ""
aut_csform_a[is.na(aut_csform_a)] <- ""

for(aut_rowh_f_cs in 1:nrow(aut_csform_h)) {
  for(aut_colh_f_cs in 1:ncol(aut_csform_h)) {

    # print(my_matrix[row, col])
    for(aut_rowa_f_cs in 1:nrow(aut_csform_a)) {
      for(aut_cola_f_cs in 1:ncol(aut_csform_a)) {
        ifelse(!aut_csform_a[aut_rowa_f_cs,aut_cola_f_cs]=="",aut_csform_h[aut_rowa_f_cs,aut_cola_f_cs] <- aut_csform_a[aut_rowa_f_cs,aut_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
aut_home_gs <- aggregate(AUT$HG, by = list(AUT$Home), FUN = sum)
aut_home_gs_avg <- aggregate(AUT$HG, by = list(AUT$Home),mean)
aut_home_scoring <- merge(aut_home_gs,aut_home_gs_avg, by='Group.1',all = T)
names(aut_home_scoring)[names(aut_home_scoring) == "x.x"] <- "TFthg"
names(aut_home_scoring)[names(aut_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
aut_away_gs <- aggregate(AUT$AG, by = list(AUT$Away), FUN = sum)
aut_away_gs_avg <- aggregate(AUT$AG, by = list(AUT$Away),mean)
aut_away_scoring <- merge(aut_away_gs,aut_away_gs_avg, by='Group.1',all = T)
names(aut_away_scoring)[names(aut_away_scoring) == "x.x"] <- "TFtag"
names(aut_away_scoring)[names(aut_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
aut_scoring <- merge(aut_home_scoring,aut_away_scoring,by='Group.1',all = T)
aut_scoring$TGS <- aut_scoring$TFthg + aut_scoring$TFtag

#home goals conceded
aut_home_gc <- aggregate(AUT$AG, by = list(AUT$Home), FUN = sum)
aut_home_gc_avg <- aggregate(AUT$AG, by = list(AUT$Home),mean)
aut_home_conceding <- merge(aut_home_gc,aut_home_gc_avg, by='Group.1',all = T)
names(aut_home_conceding)[names(aut_home_conceding) == "x.x"] <- "TFthc"
names(aut_home_conceding)[names(aut_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
aut_away_gc <- aggregate(AUT$HG, by = list(AUT$Away), FUN = sum)
aut_away_gc_avg <- aggregate(AUT$HG, by = list(AUT$Away),mean)
aut_away_conceding <- merge(aut_away_gc,aut_away_gc_avg, by='Group.1',all = T)
names(aut_away_conceding)[names(aut_away_conceding) == "x.x"] <- "TFtac"
names(aut_away_conceding)[names(aut_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
aut_conceding <- merge(aut_home_conceding,aut_away_conceding,by='Group.1',all = T)
aut_conceding$TGC <- aut_conceding$TFthc + aut_conceding$TFtac

######################################################################################
###########League Table###############################################################

#hwins and away wins
aut_home_wins <- c()
aut_away_wins <- c()
aut_home_draws <- c()
aut_away_draws <- c()
aut_home_loss <- c()
aut_away_loss <- c()



for (i_aut_wins in 1:length(aut_teams))
{

  aut_home_wins[i_aut_wins] <- nrow(AUT[AUT$Home == aut_teams[i_aut_wins] & AUT$FTR == "H",])
  aut_away_wins[i_aut_wins] <- nrow(AUT[AUT$Away == aut_teams[i_aut_wins] & AUT$FTR == "A",])
  aut_home_draws[i_aut_wins] <- nrow(AUT[AUT$Home == aut_teams[i_aut_wins] & AUT$FTR == "D",])
  aut_away_draws[i_aut_wins] <- nrow(AUT[AUT$Away == aut_teams[i_aut_wins] & AUT$FTR == "D",])
  aut_home_loss[i_aut_wins] <- nrow(AUT[AUT$Home == aut_teams[i_aut_wins] & AUT$FTR == "A",])
  aut_away_loss[i_aut_wins] <- nrow(AUT[AUT$Away == aut_teams[i_aut_wins] & AUT$FTR == "H",])

}

aut_total_wins <- aut_home_wins + aut_away_wins
aut_total_draws <- aut_home_draws + aut_away_draws
aut_total_loss <- aut_home_loss + aut_away_loss

aut_league_table <- cbind(aut_teams,aut_games_played,aut_total_wins,aut_total_draws,aut_total_loss)
aut_GS <- aut_scoring$TGS
aut_GC <-aut_conceding$TGC
aut_GD <- aut_scoring$TGS - aut_conceding$TGC
aut_PTS <- (aut_total_wins*3) + (aut_total_draws*1)
aut_league_table <- cbind(aut_league_table,aut_GS,aut_GC,aut_GD,aut_PTS)
aut_league_table <- as.data.frame(aut_league_table)
#rename the columns
names(aut_league_table)[names(aut_league_table) == "aut_teams"] <- "Team"
names(aut_league_table)[names(aut_league_table) == "aut_games_played"] <- "P"
names(aut_league_table)[names(aut_league_table) == "aut_total_wins"] <- "W"
names(aut_league_table)[names(aut_league_table) == "aut_total_draws"] <- "D"
names(aut_league_table)[names(aut_league_table) == "aut_total_loss"] <- "L"
names(aut_league_table)[names(aut_league_table) == "aut_GS"] <- "F"
names(aut_league_table)[names(aut_league_table) == "aut_GC"] <- "A"
points_aut <- aut_league_table[order(as.numeric(aut_league_table$aut_PTS), decreasing = TRUE),]
points_aut$aut_rank <- 1:length(aut_teams)
row.names(points_aut) <- points_aut$aut_rank
#create final_aut_hf_against with team ranks in brackets
for(aut_rowhrank in 1:nrow(aut_form_team_against_h)) {
  for(aut_colhrank in 1:ncol(aut_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!aut_form_team_against_h[aut_rowhrank,aut_colhrank]=="",aut_form_team_against_h[aut_rowhrank,aut_colhrank] <- paste(aut_form_team_against_h[aut_rowhrank,aut_colhrank],"(",points_aut$aut_rank[points_aut$Team ==aut_form_team_against_h[aut_rowhrank,aut_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_aut,'AUT.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six aut###################################################
#AUT
#form
#create final_aut_hf object
#aut_last_n_games <- 6
final_aut_hf <- c()
for(index_aut_hf in 1:length(aut_teams))
{
  index_aut_hf <- row.names(aut_form_h) == aut_teams[index_aut_hf]
  form_aut_hf <- aut_form_h[index_aut_hf]
  deleted_form_aut_hf <- form_aut_hf[!form_aut_hf[] == ""]
  l6_form_aut_hf <- tail(deleted_form_aut_hf,aut_last_n_games)
  l6_form_aut_hf <- paste(l6_form_aut_hf,collapse = " ")
  final_aut_hf[index_aut_hf] <- rbind(paste(aut_teams[index_aut_hf],l6_form_aut_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",aut_teams[index],l6_form)

}

#change column names
final_aut_hf <- as.data.frame(final_aut_hf)
colnames(final_aut_hf) <- "Form"
#goals scored
#create final_aut_gs object
final_aut_gs <- c()
suml6_aut_gs <- c()
for(index_aut_gs in 1:length(aut_teams))
{
  index_aut_gs <- row.names(aut_goalscored_h) == aut_teams[index_aut_gs]
  form_aut_gs <- aut_goalscored_h[index_aut_gs]
  deleted_form_aut_gs <- form_aut_gs[!form_aut_gs[] == ""]
  l6_form_aut_gs <- tail(deleted_form_aut_gs,aut_last_n_games)
  l6_form_aut_gs <- as.numeric(l6_form_aut_gs)
  suml6_aut_gs[index_aut_gs] <- sum(l6_form_aut_gs)
  suml6_aut_gs[index_aut_gs] <- paste("(",suml6_aut_gs[index_aut_gs],")",sep = "")
  l6_form_aut_gs <- paste(l6_form_aut_gs,collapse = " ")
  final_aut_gs[index_aut_gs] <- rbind(paste(aut_teams[index_aut_gs],l6_form_aut_gs,suml6_aut_gs[index_aut_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",aut_teams[index],l6_form)

}
final_aut_gs
#change column names
final_aut_gs <- as.data.frame(final_aut_gs)
colnames(final_aut_gs) <- "Goals scored"
#goal conceded
#create final_aut_gc object
final_aut_gc <- c()
suml6_aut_gc <- c()
for(index_aut_gc in 1:length(aut_teams))
{
  index_aut_gc <- row.names(aut_goalconceded_h) == aut_teams[index_aut_gc]
  form_aut_gc <- aut_goalconceded_h[index_aut_gc]
  deleted_form_aut_gc <- form_aut_gc[!form_aut_gc[] == ""]
  l6_form_aut_gc <- tail(deleted_form_aut_gc,aut_last_n_games)
  l6_form_aut_gc <- as.numeric(l6_form_aut_gc)
  suml6_aut_gc[index_aut_gc] <- sum(l6_form_aut_gc)
  suml6_aut_gc[index_aut_gc] <- paste("(",suml6_aut_gc[index_aut_gc],")",sep = "")
  l6_form_aut_gc <- paste(l6_form_aut_gc,collapse = " ")
  final_aut_gc[index_aut_gc] <- rbind(paste(aut_teams[index_aut_gc],l6_form_aut_gc,suml6_aut_gc[index_aut_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",aut_teams[index],l6_form)

}

#change column names
final_aut_gc <- as.data.frame(final_aut_gc)
colnames(final_aut_gc) <- "Goals conceded"
#total goals
#create final_aut_tg object
final_aut_tg <- c()
suml6_aut_tg <- c()
for(index_aut_tg in 1:length(aut_teams))
{
  index_aut_tg <- row.names(aut_totalgoals_h) == aut_teams[index_aut_tg]
  form_aut_tg <- aut_totalgoals_h[index_aut_tg]
  deleted_form_aut_tg <- form_aut_tg[!form_aut_tg[] == ""]
  l6_form_aut_tg <- tail(deleted_form_aut_tg,aut_last_n_games)
  l6_form_aut_tg <- as.numeric(l6_form_aut_tg)
  suml6_aut_tg[index_aut_tg] <- sum(l6_form_aut_tg)
  suml6_aut_tg[index_aut_tg] <- paste("(",suml6_aut_tg[index_aut_tg],")",sep = "")
  l6_form_aut_tg <- paste(l6_form_aut_tg,collapse = " ")
  final_aut_tg[index_aut_tg] <- rbind(paste(aut_teams[index_aut_tg],l6_form_aut_tg,suml6_aut_tg[index_aut_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",aut_teams[index],l6_form)

}
#change column names
final_aut_tg <- as.data.frame(final_aut_tg)
colnames(final_aut_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_aut_hf object
final_aut_cs <- c()
for(index_aut_cs in 1:length(aut_teams))
{
  index_aut_cs <- row.names(aut_csform_h) == aut_teams[index_aut_cs]
  csform_aut_cs <- aut_csform_h[index_aut_cs]
  deleted_csform_aut_cs <- csform_aut_cs[!csform_aut_cs[] == ""]
  l6_csform_aut_cs <- tail(deleted_csform_aut_cs,aut_last_n_games)
  l6_csform_aut_cs <- paste(l6_csform_aut_cs,collapse = " ")
  final_aut_cs[index_aut_cs] <- rbind(paste(aut_teams[index_aut_cs],l6_csform_aut_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",aut_teams[index],l6_csform)

}

#change column names
final_aut_cs <- as.data.frame(final_aut_cs)
colnames(final_aut_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_aut_wm object
final_aut_wm <- c()
suml6_aut_wm <- c()
for(index_aut_wm in 1:length(aut_teams))
{
  index_aut_wm <- row.names(aut_winmargin_h) == aut_teams[index_aut_wm]
  form_aut_wm <- aut_winmargin_h[index_aut_wm]
  deleted_form_aut_wm <- form_aut_wm[!form_aut_wm[] == ""]
  l6_form_aut_wm <- tail(deleted_form_aut_wm,aut_last_n_games)
  l6_form_aut_wm <- as.numeric(l6_form_aut_wm)
  suml6_aut_wm[index_aut_wm] <- sum(l6_form_aut_wm)
  suml6_aut_wm[index_aut_wm] <- paste("(",suml6_aut_wm[index_aut_wm],")",sep = "")
  l6_form_aut_wm <- paste(l6_form_aut_wm,collapse = " ")
  final_aut_wm[index_aut_wm] <- rbind(paste(aut_teams[index_aut_wm],l6_form_aut_wm,suml6_aut_wm[index_aut_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",aut_teams[index],l6_form)

}
final_aut_wm
#change column names
final_aut_wm <- as.data.frame(final_aut_wm)
colnames(final_aut_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_aut_hf_against
final_aut_hf_against <- c()
for(index_aut_hf_against in 1:length(aut_teams))
{
  index_aut_hf_against <- row.names(aut_form_team_against_h) == aut_teams[index_aut_hf_against]
  form_aut_hf_against <- aut_form_team_against_h[index_aut_hf_against]
  deleted_form_aut_hf_against <- form_aut_hf_against[!form_aut_hf_against[] == ""]
  l6_form_aut_hf_against <- tail(deleted_form_aut_hf_against,aut_last_n_games)
  l6_form_aut_hf_against <- paste(l6_form_aut_hf_against,collapse = " ")
  final_aut_hf_against[index_aut_hf_against] <- rbind(paste(aut_teams[index_aut_hf_against],l6_form_aut_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",aut_teams[index],l6_form)

}
final_aut_hf_against <- as.data.frame(final_aut_hf_against)
colnames(final_aut_hf_against) <- "Team against"
#combine the columns
final_aut_all <- cbind(final_aut_hf,final_aut_gs,final_aut_gc,final_aut_tg,final_aut_cs,final_aut_wm,final_aut_hf_against)
write.xlsx(final_aut_all,'AUT.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
aut_GP <- nrow(AUT)
#Calculate total home goals for each division
aut_T_HG <- sum(aut_home_gs$x)
#calculate average home goal
aut_avg_HG <- round(aut_T_HG /aut_GP, digits = 4)
############################################################
#Calculate total away goals for each division
aut_T_AG <- sum(aut_away_gs$x)
#calculate average away goal
aut_avg_AG <- round(aut_T_AG /aut_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
aut_home_as <- round(((aut_home_gs$x/aut_home_games))/aut_avg_HG, digits = 4)
#calculate away attack strength
aut_away_as <- round(((aut_away_gs$x/aut_away_games))/aut_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
aut_avg_HC <- round(aut_T_AG /aut_GP, digits = 4)
#avg away concede
aut_avg_AC <- round(aut_T_HG /aut_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
aut_home_ds <- round(((aut_home_gc$x/aut_home_games))/aut_avg_HC, digits = 4)
#away defense strength
aut_away_ds <- round(((aut_away_gc$x/aut_away_games))/aut_avg_AC, digits = 4)
#############################################################################
#home poisson data
#aut
aut_division <- c()
aut_division[1:length(aut_teams)] <- "AUT"
aut_home_poisson <- cbind(aut_division,aut_teams,aut_avg_HG,aut_home_as,aut_home_ds)
#################################################################################
#away poisson data
#aut
aut_division <- c()
aut_division[1:length(aut_teams)] <- "AUT"
aut_away_poisson <- cbind(aut_division,aut_teams,aut_avg_AG,aut_away_as,aut_away_ds)

#create home and away csv
#aut_home_poisson <- rbind(aut_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#aut_away_poisson <- rbind(aut_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(aut_home_poisson,'AUT.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(aut_away_poisson,'AUT.xlsx',sheetName = "awaypoisson", append = TRUE)
aut_home_poisson
aut_away_poisson
##########################################################################################################
###################AUT FIXTURES##########################################################################
#AUT
HomeTeam_aut <- rep(aut_teams, each = length(aut_teams))
AwayTeam_aut <- rep(aut_teams, length(aut_teams))
AUT_fixtures <- cbind(HomeTeam_aut,AwayTeam_aut)
AUT_fixtures <- as.data.frame(AUT_fixtures)
AUT_fixtures <- AUT_fixtures[!AUT_fixtures$HomeTeam_aut == AUT_fixtures$AwayTeam_aut,]
rownames(AUT_fixtures) <- NULL
AUT_fixtures$Div <- "AUT"
AUT_fixtures <- AUT_fixtures[,c(3,1,2)]

AUT_fixtures$avg_HG_aut <- aut_avg_HG

AUT_fixtures$aut_homeas <- rep(aut_home_as,each = length(aut_teams)-1)

aut_awayds_lookup <- cbind(aut_teams,aut_away_ds)

aut_awayds_lookup <- as.data.frame(aut_awayds_lookup)

colnames(aut_awayds_lookup) <- c("AwayTeam_aut","aut_awayds")


require('RH2')
AUT_fixtures$aut_awayds <- sqldf("SELECT aut_awayds_lookup.aut_awayds FROM aut_awayds_lookup INNER JOIN AUT_fixtures ON aut_awayds_lookup.AwayTeam_aut = AUT_fixtures.AwayTeam_aut")

AUT_fixtures$avg_AG_aut <- aut_avg_AG

aut_awayas_lookup <- cbind(aut_teams,aut_away_as)

aut_awayas_lookup <- as.data.frame(aut_awayas_lookup)

colnames(aut_awayas_lookup) <- c("AwayTeam_aut","aut_awayas")


AUT_fixtures$aut_awayas <- sqldf("SELECT aut_awayas_lookup.aut_awayas FROM aut_awayas_lookup INNER JOIN AUT_fixtures ON aut_awayas_lookup.AwayTeam_aut = AUT_fixtures.AwayTeam_aut")

AUT_fixtures$aut_homeds <- rep(aut_home_ds,each = length(aut_teams)-1)

AUT_fixtures$aut_awayds <- as.numeric(unlist(AUT_fixtures$aut_awayds))
#xGH
AUT_fixtures$aut_xGH <- AUT_fixtures$avg_HG_aut * AUT_fixtures$aut_homeas * AUT_fixtures$aut_awayds

#xGA

AUT_fixtures$aut_awayas <- as.numeric(unlist(AUT_fixtures$aut_awayas))

AUT_fixtures$aut_xGA <- AUT_fixtures$avg_AG_aut * AUT_fixtures$aut_awayas * AUT_fixtures$aut_homeds

AUT_fixtures$aut_0_0 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_0 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_0_1 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_1 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_0 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_0_2 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_2 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_1 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_2 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_3 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_0 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_1 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_2 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_0_3 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_3 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_3 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_4 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_0 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_1 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_2 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_3 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_0_4 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_4 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_4 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_4 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_5 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_0 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_1 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_2 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_3 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_4 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_0_5 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_5 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_5 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_5 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_5 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_6 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_0 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(0,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_1 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(1,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_2 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(2,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_3 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(3,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_4 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(4,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_6_5 <- round(stats::dpois(6,AUT_fixtures$aut_xGH) * stats::dpois(5,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_0_6 <- round(stats::dpois(0,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_1_6 <- round(stats::dpois(1,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_2_6 <- round(stats::dpois(2,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_3_6 <- round(stats::dpois(3,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_4_6 <- round(stats::dpois(4,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
AUT_fixtures$aut_5_6 <- round(stats::dpois(5,AUT_fixtures$aut_xGH) * stats::dpois(6,AUT_fixtures$aut_xGA), digits = 4)
#Home win
AUT_fixtures$aut_H <- (
  AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
    AUT_fixtures$aut_5_0 + AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5
)

AUT_fixtures$aut_H <- percent(AUT_fixtures$aut_H, accuracy = 0.1)

#Draw
AUT_fixtures$aut_D <- (

  AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 + AUT_fixtures$aut_4_4 +
    AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6
)

AUT_fixtures$aut_D <- percent(AUT_fixtures$aut_D, accuracy = 0.1)

#Away

AUT_fixtures$aut_A <- (
  AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
    AUT_fixtures$aut_0_5 + AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6
)

AUT_fixtures$aut_A <- percent(AUT_fixtures$aut_A, accuracy = 0.1)

#ov25
AUT_fixtures$aut_ov25 <- (
  AUT_fixtures$aut_2_1 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 + AUT_fixtures$aut_2_3 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 + AUT_fixtures$aut_0_4 +
    AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 + AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_0 +
    AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 + AUT_fixtures$aut_0_5 +
    AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 + AUT_fixtures$aut_5_5 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5 + AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 +
    AUT_fixtures$aut_4_6 + AUT_fixtures$aut_5_6 + AUT_fixtures$aut_6_6
)
#un25
AUT_fixtures$aut_un25 <- (
  AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_0 + AUT_fixtures$aut_0_1 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_0_2
)
#odds
AUT_fixtures$aut_ov25_odds <- round((1/AUT_fixtures$aut_ov25),digits = 2)
AUT_fixtures$aut_un25_odds <- round((1/AUT_fixtures$aut_un25),digits = 2)

AUT_fixtures$aut_ov25_odds
AUT_fixtures$aut_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
AUT_fixtures$aut_BTTSY <- (
  AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_3_1 + AUT_fixtures$aut_3_2 +
    AUT_fixtures$aut_2_2 + AUT_fixtures$aut_1_3 + AUT_fixtures$aut_2_3 + AUT_fixtures$aut_3_3 + AUT_fixtures$aut_4_4 +
    AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_3 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 +
    AUT_fixtures$aut_3_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 +
    AUT_fixtures$aut_5_4 + AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_6_6 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6
)
#BTTSN
AUT_fixtures$aut_BTTSN <- (
  AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_0 + AUT_fixtures$aut_0_1 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_0_2 +
    AUT_fixtures$aut_3_0 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_5_0 +
    AUT_fixtures$aut_0_5 + AUT_fixtures$aut_6_0 + AUT_fixtures$aut_0_6
)

AUT_fixtures$aut_BTTSY_odds <- round((1/AUT_fixtures$aut_BTTSY),digits = 2)
AUT_fixtures$aut_BTTSN_odds <- round((1/AUT_fixtures$aut_BTTSN),digits = 2)

AUT_fixtures$aut_BTTSY <- percent(AUT_fixtures$aut_BTTSY, accuracy = 0.1)
AUT_fixtures$aut_BTTSN <- percent(AUT_fixtures$aut_BTTSN, accuracy = 0.1)
#odds
AUT_fixtures$aut_BTTSY_odds
AUT_fixtures$aut_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
AUT_fixtures$aut_AH_0_H <- (
  AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
    AUT_fixtures$aut_5_0 +AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5 + AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6
)
#AH_0_A
AUT_fixtures$aut_AH_0_A <- (
  AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
    AUT_fixtures$aut_0_5 +AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6 + AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6
)

#odds
AUT_fixtures$aut_AH_0_H_odds <- round((1/AUT_fixtures$aut_AH_0_H),digits = 2)
AUT_fixtures$aut_AH_0_A_odds <- round((1/AUT_fixtures$aut_AH_0_A),digits = 2)

AUT_fixtures$aut_AH_0_H_odds
AUT_fixtures$aut_AH_0_A_odds
#percentages
AUT_fixtures$aut_AH_0_H <- percent(AUT_fixtures$aut_AH_0_H, accuracy = 0.1)
AUT_fixtures$aut_AH_0_A <- percent(AUT_fixtures$aut_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
AUT_fixtures$aut_AH_n075_H <- (
  AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
    AUT_fixtures$aut_5_0 +AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5
)
#AH_n075_A
AUT_fixtures$aut_AH_n075_A <- (
  AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
    AUT_fixtures$aut_0_5 +AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6
)

#odds
AUT_fixtures$aut_AH_n075_H_odds <- round((1/AUT_fixtures$aut_AH_n075_H),digits = 2)
AUT_fixtures$aut_AH_n075_A_odds <- round((1/AUT_fixtures$aut_AH_n075_A),digits = 2)

AUT_fixtures$aut_AH_n075_H_odds
AUT_fixtures$aut_AH_n075_A_odds
#percentages
AUT_fixtures$aut_AH_n075_H <- percent(AUT_fixtures$aut_AH_n075_H, accuracy = 0.1)
AUT_fixtures$aut_AH_n075_A <- percent(AUT_fixtures$aut_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
AUT_fixtures$aut_AH_075_H <- (
  AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
    AUT_fixtures$aut_5_0 +AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5 + AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6 + AUT_fixtures$aut_0_1 + AUT_fixtures$aut_1_2 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_3_4 + AUT_fixtures$aut_4_5 + AUT_fixtures$aut_5_6
)
#AH_075_A
AUT_fixtures$aut_AH_075_A <- (
  AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
    AUT_fixtures$aut_0_5 +AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6 + AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6 + AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_3 + AUT_fixtures$aut_5_4 + AUT_fixtures$aut_6_5
)

#odds
AUT_fixtures$aut_AH_075_H_odds <- round((1/AUT_fixtures$aut_AH_075_H),digits = 2)
AUT_fixtures$aut_AH_075_A_odds <- round((1/AUT_fixtures$aut_AH_075_A),digits = 2)

AUT_fixtures$aut_AH_075_H_odds
AUT_fixtures$aut_AH_075_A_odds
#percentages
AUT_fixtures$aut_AH_075_H <- percent(AUT_fixtures$aut_AH_075_H, accuracy = 0.1)
AUT_fixtures$aut_AH_075_A <- percent(AUT_fixtures$aut_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
AUT_fixtures$aut_AH_n125_H <- (
  AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
    AUT_fixtures$aut_5_0 +AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5
)
#AH_n125_A
AUT_fixtures$aut_AH_n125_A <- (
  AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
    AUT_fixtures$aut_0_5 +AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6
)

#odds
AUT_fixtures$aut_AH_n125_H_odds <- round((1/AUT_fixtures$aut_AH_n125_H),digits = 2)
AUT_fixtures$aut_AH_n125_A_odds <- round((1/AUT_fixtures$aut_AH_n125_A),digits = 2)

AUT_fixtures$aut_AH_n125_H_odds
AUT_fixtures$aut_AH_n125_A_odds
#percentages
AUT_fixtures$aut_AH_n125_H <- percent(AUT_fixtures$aut_AH_n125_H, accuracy = 0.1)
AUT_fixtures$aut_AH_n125_A <- percent(AUT_fixtures$aut_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
AUT_fixtures$aut_AH_125_H <- (
  AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_0 + AUT_fixtures$aut_2_1 + AUT_fixtures$aut_3_0 + AUT_fixtures$aut_3_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_0 + AUT_fixtures$aut_4_1 + AUT_fixtures$aut_4_2 + AUT_fixtures$aut_4_3 +
    AUT_fixtures$aut_5_0 +AUT_fixtures$aut_5_1 + AUT_fixtures$aut_5_2 + AUT_fixtures$aut_5_3 + AUT_fixtures$aut_5_4 +
    AUT_fixtures$aut_6_0 + AUT_fixtures$aut_6_1 + AUT_fixtures$aut_6_2 + AUT_fixtures$aut_6_3 + AUT_fixtures$aut_6_4 +
    AUT_fixtures$aut_6_5 + AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6 + AUT_fixtures$aut_0_1 + AUT_fixtures$aut_1_2 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_3_4 + AUT_fixtures$aut_4_5 + AUT_fixtures$aut_5_6
)
#AH_125_A
AUT_fixtures$aut_AH_125_A <- (
  AUT_fixtures$aut_0_1 + AUT_fixtures$aut_0_2 + AUT_fixtures$aut_1_2 + AUT_fixtures$aut_0_3 + AUT_fixtures$aut_1_3 +
    AUT_fixtures$aut_2_3 + AUT_fixtures$aut_0_4 + AUT_fixtures$aut_1_4 + AUT_fixtures$aut_2_4 + AUT_fixtures$aut_3_4 +
    AUT_fixtures$aut_0_5 +AUT_fixtures$aut_1_5 + AUT_fixtures$aut_2_5 + AUT_fixtures$aut_3_5 + AUT_fixtures$aut_4_5 +
    AUT_fixtures$aut_0_6 + AUT_fixtures$aut_1_6 + AUT_fixtures$aut_2_6 + AUT_fixtures$aut_3_6 + AUT_fixtures$aut_4_6 +
    AUT_fixtures$aut_5_6 + AUT_fixtures$aut_0_0 + AUT_fixtures$aut_1_1 + AUT_fixtures$aut_2_2 + AUT_fixtures$aut_3_3 +
    AUT_fixtures$aut_4_4 + AUT_fixtures$aut_5_5 + AUT_fixtures$aut_6_6 + AUT_fixtures$aut_1_0 + AUT_fixtures$aut_2_1 +
    AUT_fixtures$aut_3_2 + AUT_fixtures$aut_4_3 + AUT_fixtures$aut_5_4 + AUT_fixtures$aut_6_5
)

#odds
AUT_fixtures$aut_AH_125_H_odds <- round((1/AUT_fixtures$aut_AH_125_H),digits = 2)
AUT_fixtures$aut_AH_125_A_odds <- round((1/AUT_fixtures$aut_AH_125_A),digits = 2)

AUT_fixtures$aut_AH_125_H_odds
AUT_fixtures$aut_AH_125_A_odds
#percentages
AUT_fixtures$aut_AH_125_H <- percent(AUT_fixtures$aut_AH_125_H, accuracy = 0.1)
AUT_fixtures$aut_AH_125_A <- percent(AUT_fixtures$aut_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
AUT_fixtures$aut_ov25 <- percent(AUT_fixtures$aut_ov25, accuracy = 0.1)

AUT_fixtures$aut_un25 <- percent(AUT_fixtures$aut_un25, accuracy = 0.1)
AUT_fixtures$aut_pscore <- paste(round(AUT_fixtures$aut_xGH,digits = 0),round(AUT_fixtures$aut_xGA,digits = 0),sep = "-")
#write out
write.xlsx(AUT_fixtures,'AUT.xlsx',sheetName = "AUT", append = TRUE)
###########################################################################################################
########################AUT END###########################################################################
AUT <- read.csv('../FDAS/AUT.csv')
AUT$TG <- AUT$HG + AUT$AG
AUT$OV25 <- ifelse(AUT$TG >= 3,"Y","N")
aut_ftr_summary <- tabyl(AUT,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
aut_ov25_summary <- tabyl(AUT,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(aut_ftr_summary,'AUT.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(aut_ov25_summary,'AUT.xlsx',sheetName = "OVUN25", append = TRUE)



