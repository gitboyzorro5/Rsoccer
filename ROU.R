library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('ROU.xlsx')
######################ROU START#######################################
#####################################################################
ROU <- read.csv('../FDAS/ROU.csv')
ROU <- within(ROU,rm(Res))
ROU$Date <- dmy(ROU$Date)
ROU <- ROU[order(as.Date(ROU$Date, format = "%d/%m%Y"), decreasing = FALSE),]
ROU$CS <- paste(ROU$HG,ROU$AG, sep = "-")
#ROU_qualificaton <- subset(ROU,tournament == "UEFA Euro qualification")
ROU <- subset(ROU,Season == "2021/2022")
#ROU <- ROU[ROU$Date > '2008-01-01',])
ROU$TG <- ROU$HG + ROU$AG
ROU$OV25 <- ifelse(ROU$TG >= 3,"Y","N")
ROU$FTR <- with(ROU,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
rou_totalgoalsv2 <- tapply(ROU$TG, ROU[c("Home", "Away")],mean)
rou_totalgoalsv2
rou_hgtotals <- rowSums(rou_totalgoalsv2,na.rm = T)
rou_agtotals <- colSums(rou_totalgoalsv2,na.rm = T)

rou_totalgoals <- rou_hgtotals + rou_agtotals
rou_totalgoalsv2 <- cbind(rou_totalgoalsv2,rou_totalgoals)
rou_teams <- sort(unique(ROU$Home))
rou_home_games <- c()
rou_away_games <-c()
for (i_rou in 1:length(rou_teams))
{

  rou_home_games[i_rou] <- nrow(ROU[ROU$Home == rou_teams[i_rou],])
  rou_away_games[i_rou]  <- nrow(ROU[ROU$Away == rou_teams[i_rou],])

}
rou_games_played <- rou_home_games + rou_away_games
rou_goaltotalsv2 <- cbind(rou_totalgoalsv2,rou_games_played)
rou_avg_totalgoals <- round((rou_totalgoals/ rou_games_played), digits = 4)
rou_goaltotalsv2[is.na(rou_goaltotalsv2)] <- ""
rou_goaltotalsv2 <- cbind(rou_goaltotalsv2,rou_avg_totalgoals)
write.xlsx(rou_goaltotalsv2,'ROU.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
rou_goalscored_h <- tapply(ROU$HG, ROU[c("Home", "Date")],mean)
rou_goalscored_a <- tapply(ROU$AG, ROU[c("Away", "Date")],mean)
rou_goalscored_h[is.na(rou_goalscored_h)] <- ""
rou_goalscored_a[is.na(rou_goalscored_a)] <- ""

for(rou_rowhgs in 1:nrow(rou_goalscored_h)) {
  for(rou_colhgs in 1:ncol(rou_goalscored_h)) {

    # print(my_matrix[row, col])
    for(rou_rowags in 1:nrow(rou_goalscored_a)) {
      for(rou_colags in 1:ncol(rou_goalscored_a)) {
        ifelse(!rou_goalscored_a[rou_rowags,rou_colags]=="",rou_goalscored_h[rou_rowags,rou_colags] <- rou_goalscored_a[rou_rowags,rou_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rou_goalscored_h,'ROU.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
rou_goalconceded_h <- tapply(ROU$AG, ROU[c("Home", "Date")],mean)
rou_goalconceded_a <- tapply(ROU$HG, ROU[c("Away", "Date")],mean)
rou_goalconceded_h[is.na(rou_goalconceded_h)] <- ""
rou_goalconceded_a[is.na(rou_goalconceded_a)] <- ""

for(rou_rowhgc in 1:nrow(rou_goalconceded_h)) {
  for(rou_colhgc in 1:ncol(rou_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(rou_rowagc in 1:nrow(rou_goalconceded_a)) {
      for(rou_colagc in 1:ncol(rou_goalconceded_a)) {
        ifelse(!rou_goalconceded_a[rou_rowagc,rou_colagc]=="",rou_goalconceded_h[rou_rowagc,rou_colagc] <- rou_goalconceded_a[rou_rowagc,rou_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rou_goalconceded_h,'ROU.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
rou_form_h <- tapply(ROU$FTR, ROU[c("Home", "Date")],median)
rou_form_a <- tapply(ROU$FTR, ROU[c("Away", "Date")],median)
rou_form_h[is.na(rou_form_h)] <- ""
rou_form_a[is.na(rou_form_a)] <- ""
rou_form_h <- sub("A","L",rou_form_h)
rou_form_h <- sub("H","W",rou_form_h)
rou_form_a <- sub("A","W",rou_form_a)
rou_form_a <- sub("H","L",rou_form_a)
for(rou_rowh_f in 1:nrow(rou_form_h)) {
  for(rou_colh_f in 1:ncol(rou_form_h)) {

    # print(my_matrix[row, col])
    for(rou_rowa_f in 1:nrow(rou_form_a)) {
      for(rou_cola_f in 1:ncol(rou_form_a)) {
        ifelse(!rou_form_a[rou_rowa_f,rou_cola_f]=="",rou_form_h[rou_rowa_f,rou_cola_f] <- rou_form_a[rou_rowa_f,rou_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rou_form_h,'ROU.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
rou_totalgoals_h <- tapply(ROU$TG, ROU[c("Home", "Date")],mean)
rou_totalgoals_a <- tapply(ROU$TG, ROU[c("Away", "Date")],mean)
rou_totalgoals_h[is.na(rou_totalgoals_h)] <- ""
rou_totalgoals_a[is.na(rou_totalgoals_a)] <- ""
for(rou_rowh in 1:nrow(rou_totalgoals_h)) {
  for(rou_colh in 1:ncol(rou_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(rou_rowa in 1:nrow(rou_totalgoals_a)) {
      for(rou_cola in 1:ncol(rou_totalgoals_a)) {
        ifelse(!rou_totalgoals_a[rou_rowa,rou_cola]=="",rou_totalgoals_h[rou_rowa,rou_cola] <- rou_totalgoals_a[rou_rowa,rou_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(rou_totalgoals_h,'ROU.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
rou_form_team_against_h <- tapply(ROU$Away, ROU[c("Home", "Date")],median)
rou_form_team_against_a <- tapply(ROU$Home, ROU[c("Away", "Date")],median)
rou_form_team_against_h[is.na(rou_form_team_against_h)] <- ""
rou_form_team_against_a[is.na(rou_form_team_against_a)] <- ""
for(rou_rowh_f_against in 1:nrow(rou_form_team_against_h)) {
  for(rou_colh_f_against in 1:ncol(rou_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(rou_rowa_f_against in 1:nrow(rou_form_team_against_a)) {
      for(rou_cola_f_against in 1:ncol(rou_form_team_against_a)) {
        ifelse(!rou_form_team_against_a[rou_rowa_f_against,rou_cola_f_against]=="",rou_form_team_against_h[rou_rowa_f_against,rou_cola_f_against] <- rou_form_team_against_a[rou_rowa_f_against,rou_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#######################################################################
#win margin
rou_winmargin_h <- tapply(ROU$HG - ROU$AG, ROU[c("Home", "Date")],mean)
rou_winmargin_a <- tapply(ROU$AG - ROU$HG, ROU[c("Away", "Date")],mean)
rou_winmargin_h[is.na(rou_winmargin_h)] <- ""
#
for(rou_rowhwm in 1:nrow(rou_winmargin_h)) {
  for(rou_colhwm in 1:ncol(rou_winmargin_h)) {

    # print(my_matrix[row, col])
    for(rou_rowawm in 1:nrow(rou_winmargin_a)) {
      for(rou_colawm in 1:ncol(rou_winmargin_a)) {
        ifelse(!rou_winmargin_a[rou_rowawm,rou_colawm]=="",rou_winmargin_h[rou_rowawm,rou_colawm] <- rou_winmargin_a[rou_rowawm,rou_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#ROU
rou_un05_home <- c()
rou_un05_away <- c()
rou_ov05_home <- c()
rou_ov05_away <- c()

rou_un15_home <- c()
rou_un15_away <- c()
rou_ov15_home <- c()
rou_ov15_away <- c()

rou_un25_home <- c()
rou_un25_away <- c()
rou_ov25_home <- c()
rou_ov25_away <- c()

rou_un35_home <- c()
rou_un35_away <- c()
rou_ov35_home <- c()
rou_ov35_away <- c()

rou_un45_home <- c()
rou_un45_away <- c()
rou_ov45_home <- c()
rou_ov45_away <- c()

rou_un55_home <- c()
rou_un55_away <- c()
rou_ov55_home <- c()
rou_ov55_away <- c()

for (i_rou_tg in 1:length(rou_teams))
{

  rou_un05_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG == 0,])
  rou_un05_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG == 0,])

  rou_ov05_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG > 0,])
  rou_ov05_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG > 0,])

  rou_un15_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG <= 1,])
  rou_un15_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG <= 1,])

  rou_ov15_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG >= 2,])
  rou_ov15_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG >= 2,])

  rou_un25_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG <= 2,])
  rou_un25_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG <= 2,])

  rou_ov25_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG >=3,])
  rou_ov25_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG >=3,])

  rou_un35_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG <= 3,])
  rou_un35_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG <= 3,])

  rou_ov35_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG >= 4,])
  rou_ov35_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG >= 4,])

  rou_un45_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG <= 4,])
  rou_un45_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG <= 4,])

  rou_ov45_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG >= 5,])
  rou_ov45_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG >= 5,])

  rou_un55_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG <= 5,])
  rou_un55_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG <= 5,])

  rou_ov55_home[i_rou_tg] <- nrow(ROU[ROU$Home == rou_teams[i_rou_tg] & ROU$TG >= 6,])
  rou_ov55_away[i_rou_tg] <- nrow(ROU[ROU$Away == rou_teams[i_rou_tg] & ROU$TG >= 6,])


}

rou_un05 <- rou_un05_home + rou_un05_away
rou_ov05 <- rou_ov05_home + rou_ov05_away

rou_un15 <- rou_un15_home + rou_un15_away
rou_ov15 <- rou_ov15_home + rou_ov15_away

rou_un25 <- rou_un25_home + rou_un25_away
rou_ov25 <- rou_ov25_home + rou_ov25_away

rou_un35 <- rou_un35_home + rou_un35_away
rou_ov35 <- rou_ov35_home + rou_ov35_away

rou_un45 <- rou_un45_home + rou_un45_away
rou_ov45 <- rou_ov45_home + rou_ov45_away

rou_un55 <- rou_un55_home + rou_un55_away
rou_ov55 <- rou_ov55_home + rou_ov55_away

rou_ovundata <- cbind(rou_teams,rou_un05,rou_ov05,rou_un15,rou_ov15,rou_un25,rou_ov25,rou_un35,rou_ov35,rou_un45,rou_ov45,rou_un55,rou_ov55)
write.xlsx(rou_ovundata,'ROU.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
rou_csform_h <- tapply(ROU$CS, ROU[c("Home", "Date")],median)
rou_csform_a <- tapply(ROU$CS, ROU[c("Away", "Date")],median)

rou_csform_h[is.na(rou_csform_h)] <- ""
rou_csform_a[is.na(rou_csform_a)] <- ""

for(rou_rowh_f_cs in 1:nrow(rou_csform_h)) {
  for(rou_colh_f_cs in 1:ncol(rou_csform_h)) {

    # print(my_matrix[row, col])
    for(rou_rowa_f_cs in 1:nrow(rou_csform_a)) {
      for(rou_cola_f_cs in 1:ncol(rou_csform_a)) {
        ifelse(!rou_csform_a[rou_rowa_f_cs,rou_cola_f_cs]=="",rou_csform_h[rou_rowa_f_cs,rou_cola_f_cs] <- rou_csform_a[rou_rowa_f_cs,rou_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
rou_home_gs <- aggregate(ROU$HG, by = list(ROU$Home), FUN = sum)
rou_home_gs_avg <- aggregate(ROU$HG, by = list(ROU$Home),mean)
rou_home_scoring <- merge(rou_home_gs,rou_home_gs_avg, by='Group.1',all = T)
names(rou_home_scoring)[names(rou_home_scoring) == "x.x"] <- "TFthg"
names(rou_home_scoring)[names(rou_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
rou_away_gs <- aggregate(ROU$AG, by = list(ROU$Away), FUN = sum)
rou_away_gs_avg <- aggregate(ROU$AG, by = list(ROU$Away),mean)
rou_away_scoring <- merge(rou_away_gs,rou_away_gs_avg, by='Group.1',all = T)
names(rou_away_scoring)[names(rou_away_scoring) == "x.x"] <- "TFtag"
names(rou_away_scoring)[names(rou_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
rou_scoring <- merge(rou_home_scoring,rou_away_scoring,by='Group.1',all = T)
rou_scoring$TGS <- rou_scoring$TFthg + rou_scoring$TFtag

#home goals conceded
rou_home_gc <- aggregate(ROU$AG, by = list(ROU$Home), FUN = sum)
rou_home_gc_avg <- aggregate(ROU$AG, by = list(ROU$Home),mean)
rou_home_conceding <- merge(rou_home_gc,rou_home_gc_avg, by='Group.1',all = T)
names(rou_home_conceding)[names(rou_home_conceding) == "x.x"] <- "TFthc"
names(rou_home_conceding)[names(rou_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
rou_away_gc <- aggregate(ROU$HG, by = list(ROU$Away), FUN = sum)
rou_away_gc_avg <- aggregate(ROU$HG, by = list(ROU$Away),mean)
rou_away_conceding <- merge(rou_away_gc,rou_away_gc_avg, by='Group.1',all = T)
names(rou_away_conceding)[names(rou_away_conceding) == "x.x"] <- "TFtac"
names(rou_away_conceding)[names(rou_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
rou_conceding <- merge(rou_home_conceding,rou_away_conceding,by='Group.1',all = T)
rou_conceding$TGC <- rou_conceding$TFthc + rou_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
rou_home_wins <- c()
rou_away_wins <- c()
rou_home_draws <- c()
rou_away_draws <- c()
rou_home_loss <- c()
rou_away_loss <- c()



for (i_rou_wins in 1:length(rou_teams))
{

  rou_home_wins[i_rou_wins] <- nrow(ROU[ROU$Home == rou_teams[i_rou_wins] & ROU$FTR == "H",])
  rou_away_wins[i_rou_wins] <- nrow(ROU[ROU$Away == rou_teams[i_rou_wins] & ROU$FTR == "A",])
  rou_home_draws[i_rou_wins] <- nrow(ROU[ROU$Home == rou_teams[i_rou_wins] & ROU$FTR == "D",])
  rou_away_draws[i_rou_wins] <- nrow(ROU[ROU$Away == rou_teams[i_rou_wins] & ROU$FTR == "D",])
  rou_home_loss[i_rou_wins] <- nrow(ROU[ROU$Home == rou_teams[i_rou_wins] & ROU$FTR == "A",])
  rou_away_loss[i_rou_wins] <- nrow(ROU[ROU$Away == rou_teams[i_rou_wins] & ROU$FTR == "H",])

}

rou_total_wins <- rou_home_wins + rou_away_wins
rou_total_draws <- rou_home_draws + rou_away_draws
rou_total_loss <- rou_home_loss + rou_away_loss

rou_league_table <- cbind(rou_teams,rou_games_played,rou_total_wins,rou_total_draws,rou_total_loss)
rou_GS <- rou_scoring$TGS
rou_GC <-rou_conceding$TGC
rou_GD <- rou_scoring$TGS - rou_conceding$TGC
rou_PTS <- (rou_total_wins*3) + (rou_total_draws*1)
rou_league_table <- cbind(rou_league_table,rou_GS,rou_GC,rou_GD,rou_PTS)
rou_league_table <- as.data.frame(rou_league_table)
#rename the columns
names(rou_league_table)[names(rou_league_table) == "rou_teams"] <- "Team"
names(rou_league_table)[names(rou_league_table) == "rou_games_played"] <- "P"
names(rou_league_table)[names(rou_league_table) == "rou_total_wins"] <- "W"
names(rou_league_table)[names(rou_league_table) == "rou_total_draws"] <- "D"
names(rou_league_table)[names(rou_league_table) == "rou_total_loss"] <- "L"
names(rou_league_table)[names(rou_league_table) == "rou_GS"] <- "F"
names(rou_league_table)[names(rou_league_table) == "rou_GC"] <- "A"
points_rou <- rou_league_table[order(as.numeric(rou_league_table$rou_PTS), decreasing = TRUE),]
points_rou$rou_rank <- 1:length(rou_teams)
row.names(points_rou) <- points_rou$rou_rank
#create final_rou_hf_against with team ranks in brackets
for(rou_rowhrank in 1:nrow(rou_form_team_against_h)) {
  for(rou_colhrank in 1:ncol(rou_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!rou_form_team_against_h[rou_rowhrank,rou_colhrank]=="",rou_form_team_against_h[rou_rowhrank,rou_colhrank] <- paste(rou_form_team_against_h[rou_rowhrank,rou_colhrank],"(",points_rou$rou_rank[points_rou$Team ==rou_form_team_against_h[rou_rowhrank,rou_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_rou,'ROU.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six rou###################################################
#ROU
#form
#create final_rou_hf object
#rou_last_n_games <- 6
final_rou_hf <- c()
for(index_rou_hf in 1:length(rou_teams))
{
  index_rou_hf <- row.names(rou_form_h) == rou_teams[index_rou_hf]
  form_rou_hf <- rou_form_h[index_rou_hf]
  deleted_form_rou_hf <- form_rou_hf[!form_rou_hf[] == ""]
  l6_form_rou_hf <- tail(deleted_form_rou_hf,rou_last_n_games)
  l6_form_rou_hf <- paste(l6_form_rou_hf,collapse = " ")
  final_rou_hf[index_rou_hf] <- rbind(paste(rou_teams[index_rou_hf],l6_form_rou_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rou_teams[index],l6_form)

}

#change column names
final_rou_hf <- as.data.frame(final_rou_hf)
colnames(final_rou_hf) <- "Form"
#goals scored
#create final_rou_gs object
final_rou_gs <- c()
suml6_rou_gs <- c()
for(index_rou_gs in 1:length(rou_teams))
{
  index_rou_gs <- row.names(rou_goalscored_h) == rou_teams[index_rou_gs]
  form_rou_gs <- rou_goalscored_h[index_rou_gs]
  deleted_form_rou_gs <- form_rou_gs[!form_rou_gs[] == ""]
  l6_form_rou_gs <- tail(deleted_form_rou_gs,rou_last_n_games)
  l6_form_rou_gs <- as.numeric(l6_form_rou_gs)
  suml6_rou_gs[index_rou_gs] <- sum(l6_form_rou_gs)
  suml6_rou_gs[index_rou_gs] <- paste("(",suml6_rou_gs[index_rou_gs],")",sep = "")
  l6_form_rou_gs <- paste(l6_form_rou_gs,collapse = " ")
  final_rou_gs[index_rou_gs] <- rbind(paste(rou_teams[index_rou_gs],l6_form_rou_gs,suml6_rou_gs[index_rou_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rou_teams[index],l6_form)

}
final_rou_gs
#change column names
final_rou_gs <- as.data.frame(final_rou_gs)
colnames(final_rou_gs) <- "Goals scored"
#goal conceded
#create final_rou_gc object
final_rou_gc <- c()
suml6_rou_gc <- c()
for(index_rou_gc in 1:length(rou_teams))
{
  index_rou_gc <- row.names(rou_goalconceded_h) == rou_teams[index_rou_gc]
  form_rou_gc <- rou_goalconceded_h[index_rou_gc]
  deleted_form_rou_gc <- form_rou_gc[!form_rou_gc[] == ""]
  l6_form_rou_gc <- tail(deleted_form_rou_gc,rou_last_n_games)
  l6_form_rou_gc <- as.numeric(l6_form_rou_gc)
  suml6_rou_gc[index_rou_gc] <- sum(l6_form_rou_gc)
  suml6_rou_gc[index_rou_gc] <- paste("(",suml6_rou_gc[index_rou_gc],")",sep = "")
  l6_form_rou_gc <- paste(l6_form_rou_gc,collapse = " ")
  final_rou_gc[index_rou_gc] <- rbind(paste(rou_teams[index_rou_gc],l6_form_rou_gc,suml6_rou_gc[index_rou_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rou_teams[index],l6_form)

}

#change column names
final_rou_gc <- as.data.frame(final_rou_gc)
colnames(final_rou_gc) <- "Goals conceded"
#total goals
#create final_rou_tg object
final_rou_tg <- c()
suml6_rou_tg <- c()
for(index_rou_tg in 1:length(rou_teams))
{
  index_rou_tg <- row.names(rou_totalgoals_h) == rou_teams[index_rou_tg]
  form_rou_tg <- rou_totalgoals_h[index_rou_tg]
  deleted_form_rou_tg <- form_rou_tg[!form_rou_tg[] == ""]
  l6_form_rou_tg <- tail(deleted_form_rou_tg,rou_last_n_games)
  l6_form_rou_tg <- as.numeric(l6_form_rou_tg)
  suml6_rou_tg[index_rou_tg] <- sum(l6_form_rou_tg)
  suml6_rou_tg[index_rou_tg] <- paste("(",suml6_rou_tg[index_rou_tg],")",sep = "")
  l6_form_rou_tg <- paste(l6_form_rou_tg,collapse = " ")
  final_rou_tg[index_rou_tg] <- rbind(paste(rou_teams[index_rou_tg],l6_form_rou_tg,suml6_rou_tg[index_rou_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rou_teams[index],l6_form)

}
#change column names
final_rou_tg <- as.data.frame(final_rou_tg)
colnames(final_rou_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_rou_hf object
final_rou_cs <- c()
for(index_rou_cs in 1:length(rou_teams))
{
  index_rou_cs <- row.names(rou_csform_h) == rou_teams[index_rou_cs]
  csform_rou_cs <- rou_csform_h[index_rou_cs]
  deleted_csform_rou_cs <- csform_rou_cs[!csform_rou_cs[] == ""]
  l6_csform_rou_cs <- tail(deleted_csform_rou_cs,rou_last_n_games)
  l6_csform_rou_cs <- paste(l6_csform_rou_cs,collapse = " ")
  final_rou_cs[index_rou_cs] <- rbind(paste(rou_teams[index_rou_cs],l6_csform_rou_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",rou_teams[index],l6_csform)

}

#change column names
final_rou_cs <- as.data.frame(final_rou_cs)
colnames(final_rou_cs) <- "CSForm"
#################################################
#################################################
#Win Margin
#goals scored
#create final_rou_wm object
final_rou_wm <- c()
suml6_rou_wm <- c()
for(index_rou_wm in 1:length(rou_teams))
{
  index_rou_wm <- row.names(rou_winmargin_h) == rou_teams[index_rou_wm]
  form_rou_wm <- rou_winmargin_h[index_rou_wm]
  deleted_form_rou_wm <- form_rou_wm[!form_rou_wm[] == ""]
  l6_form_rou_wm <- tail(deleted_form_rou_wm,rou_last_n_games)
  l6_form_rou_wm <- as.numeric(l6_form_rou_wm)
  suml6_rou_wm[index_rou_wm] <- sum(l6_form_rou_wm)
  suml6_rou_wm[index_rou_wm] <- paste("(",suml6_rou_wm[index_rou_wm],")",sep = "")
  l6_form_rou_wm <- paste(l6_form_rou_wm,collapse = " ")
  final_rou_wm[index_rou_wm] <- rbind(paste(rou_teams[index_rou_wm],l6_form_rou_wm,suml6_rou_wm[index_rou_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rou_teams[index],l6_form)

}
final_rou_wm
#change column names
final_rou_wm <- as.data.frame(final_rou_wm)
colnames(final_rou_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_rou_hf_against
final_rou_hf_against <- c()
for(index_rou_hf_against in 1:length(rou_teams))
{
  index_rou_hf_against <- row.names(rou_form_team_against_h) == rou_teams[index_rou_hf_against]
  form_rou_hf_against <- rou_form_team_against_h[index_rou_hf_against]
  deleted_form_rou_hf_against <- form_rou_hf_against[!form_rou_hf_against[] == ""]
  l6_form_rou_hf_against <- tail(deleted_form_rou_hf_against,rou_last_n_games)
  l6_form_rou_hf_against <- paste(l6_form_rou_hf_against,collapse = " ")
  final_rou_hf_against[index_rou_hf_against] <- rbind(paste(rou_teams[index_rou_hf_against],l6_form_rou_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",rou_teams[index],l6_form)

}
final_rou_hf_against <- as.data.frame(final_rou_hf_against)
colnames(final_rou_hf_against) <- "Team against"
#combine the columns
final_rou_all <- cbind(final_rou_hf,final_rou_gs,final_rou_gc,final_rou_tg,final_rou_cs,final_rou_wm,final_rou_hf_against)
write.xlsx(final_rou_all,'ROU.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
rou_GP <- nrow(ROU)
#Calculate total home goals for each division
rou_T_HG <- sum(rou_home_gs$x)
#calculate average home goal
rou_avg_HG <- round(rou_T_HG /rou_GP, digits = 4)
############################################################
#Calculate total away goals for each division
rou_T_AG <- sum(rou_away_gs$x)
#calculate average away goal
rou_avg_AG <- round(rou_T_AG /rou_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
rou_home_as <- round(((rou_home_gs$x/rou_home_games))/rou_avg_HG, digits = 4)
#calculate away attack strength
rou_away_as <- round(((rou_away_gs$x/rou_away_games))/rou_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
rou_avg_HC <- round(rou_T_AG /rou_GP, digits = 4)
#avg away concede
rou_avg_AC <- round(rou_T_HG /rou_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
rou_home_ds <- round(((rou_home_gc$x/rou_home_games))/rou_avg_HC, digits = 4)
#away defense strength
rou_away_ds <- round(((rou_away_gc$x/rou_away_games))/rou_avg_AC, digits = 4)
#############################################################################
#home poisson data
#rou
rou_division <- c()
rou_division[1:length(rou_teams)] <- "ROU"
rou_home_poisson <- cbind(rou_division,rou_teams,rou_avg_HG,rou_home_as,rou_home_ds)
#################################################################################
#away poisson data
#rou
rou_division <- c()
rou_division[1:length(rou_teams)] <- "ROU"
rou_away_poisson <- cbind(rou_division,rou_teams,rou_avg_AG,rou_away_as,rou_away_ds)

#create home and away csv
#rou_home_poisson <- rbind(rou_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#rou_away_poisson <- rbind(rou_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(rou_home_poisson,'ROU.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(rou_away_poisson,'ROU.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################ROU FIXTURES##########################################################################
#ROU
HomeTeam_rou <- rep(rou_teams, each = length(rou_teams))
AwayTeam_rou <- rep(rou_teams, length(rou_teams))
ROU_fixtures <- cbind(HomeTeam_rou,AwayTeam_rou)
ROU_fixtures <- as.data.frame(ROU_fixtures)
ROU_fixtures <- ROU_fixtures[!ROU_fixtures$HomeTeam_rou == ROU_fixtures$AwayTeam_rou,]
rownames(ROU_fixtures) <- NULL
ROU_fixtures$Div <- "ROU"
ROU_fixtures <- ROU_fixtures[,c(3,1,2)]

ROU_fixtures$avg_HG_rou <- rou_avg_HG

ROU_fixtures$rou_homeas <- rep(rou_home_as,each = length(rou_teams)-1)

rou_awayds_lookup <- cbind(rou_teams,rou_away_ds)

rou_awayds_lookup <- as.data.frame(rou_awayds_lookup)

colnames(rou_awayds_lookup) <- c("AwayTeam_rou","rou_awayds")


require('RH2')
ROU_fixtures$rou_awayds <- sqldf("SELECT rou_awayds_lookup.rou_awayds FROM rou_awayds_lookup INNER JOIN ROU_fixtures ON rou_awayds_lookup.AwayTeam_rou = ROU_fixtures.AwayTeam_rou")

ROU_fixtures$avg_AG_rou <- rou_avg_AG

rou_awayas_lookup <- cbind(rou_teams,rou_away_as)

rou_awayas_lookup <- as.data.frame(rou_awayas_lookup)

colnames(rou_awayas_lookup) <- c("AwayTeam_rou","rou_awayas")


ROU_fixtures$rou_awayas <- sqldf("SELECT rou_awayas_lookup.rou_awayas FROM rou_awayas_lookup INNER JOIN ROU_fixtures ON rou_awayas_lookup.AwayTeam_rou = ROU_fixtures.AwayTeam_rou")

ROU_fixtures$rou_homeds <- rep(rou_home_ds,each = length(rou_teams)-1)

ROU_fixtures$rou_awayds <- as.numeric(unlist(ROU_fixtures$rou_awayds))
#xGH
ROU_fixtures$rou_xGH <- ROU_fixtures$avg_HG_rou * ROU_fixtures$rou_homeas * ROU_fixtures$rou_awayds

#xGA

ROU_fixtures$rou_awayas <- as.numeric(unlist(ROU_fixtures$rou_awayas))

ROU_fixtures$rou_xGA <- ROU_fixtures$avg_AG_rou * ROU_fixtures$rou_awayas * ROU_fixtures$rou_homeds

ROU_fixtures$rou_0_0 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_0 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_0_1 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_1 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_0 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_0_2 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_2 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_1 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_2 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_3 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_0 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_1 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_2 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_0_3 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_3 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_3 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_4 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_0 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_1 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_2 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_3 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_0_4 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_4 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_4 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_4 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_5 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_0 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_1 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_2 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_3 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_4 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_0_5 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_5 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_5 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_5 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_5 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_6 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_0 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(0,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_1 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(1,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_2 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(2,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_3 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(3,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_4 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(4,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_6_5 <- round(stats::dpois(6,ROU_fixtures$rou_xGH) * stats::dpois(5,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_0_6 <- round(stats::dpois(0,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_1_6 <- round(stats::dpois(1,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_2_6 <- round(stats::dpois(2,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_3_6 <- round(stats::dpois(3,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_4_6 <- round(stats::dpois(4,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
ROU_fixtures$rou_5_6 <- round(stats::dpois(5,ROU_fixtures$rou_xGH) * stats::dpois(6,ROU_fixtures$rou_xGA), digits = 4)
#Home win
ROU_fixtures$rou_H <- (
  ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
    ROU_fixtures$rou_5_0 + ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5
)

ROU_fixtures$rou_H <- percent(ROU_fixtures$rou_H, accuracy = 0.1)

#Draw
ROU_fixtures$rou_D <- (

  ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 + ROU_fixtures$rou_4_4 +
    ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6
)

ROU_fixtures$rou_D <- percent(ROU_fixtures$rou_D, accuracy = 0.1)

#Away

ROU_fixtures$rou_A <- (
  ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
    ROU_fixtures$rou_0_5 + ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6
)

ROU_fixtures$rou_A <- percent(ROU_fixtures$rou_A, accuracy = 0.1)

#ov25
ROU_fixtures$rou_ov25 <- (
  ROU_fixtures$rou_2_1 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 + ROU_fixtures$rou_2_3 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 + ROU_fixtures$rou_0_4 +
    ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 + ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_0 +
    ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 + ROU_fixtures$rou_0_5 +
    ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 + ROU_fixtures$rou_5_5 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5 + ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 +
    ROU_fixtures$rou_4_6 + ROU_fixtures$rou_5_6 + ROU_fixtures$rou_6_6
)
#un25
ROU_fixtures$rou_un25 <- (
  ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_0 + ROU_fixtures$rou_0_1 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_0_2
)
#odds
ROU_fixtures$rou_ov25_odds <- round((1/ROU_fixtures$rou_ov25),digits = 2)
ROU_fixtures$rou_un25_odds <- round((1/ROU_fixtures$rou_un25),digits = 2)

ROU_fixtures$rou_ov25_odds
ROU_fixtures$rou_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
ROU_fixtures$rou_BTTSY <- (
  ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_3_1 + ROU_fixtures$rou_3_2 +
    ROU_fixtures$rou_2_2 + ROU_fixtures$rou_1_3 + ROU_fixtures$rou_2_3 + ROU_fixtures$rou_3_3 + ROU_fixtures$rou_4_4 +
    ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_3 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 +
    ROU_fixtures$rou_3_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 +
    ROU_fixtures$rou_5_4 + ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_6_6 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6
)
#BTTSN
ROU_fixtures$rou_BTTSN <- (
  ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_0 + ROU_fixtures$rou_0_1 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_0_2 +
    ROU_fixtures$rou_3_0 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_5_0 +
    ROU_fixtures$rou_0_5 + ROU_fixtures$rou_6_0 + ROU_fixtures$rou_0_6
)

ROU_fixtures$rou_BTTSY_odds <- round((1/ROU_fixtures$rou_BTTSY),digits = 2)
ROU_fixtures$rou_BTTSN_odds <- round((1/ROU_fixtures$rou_BTTSN),digits = 2)

ROU_fixtures$rou_BTTSY <- percent(ROU_fixtures$rou_BTTSY, accuracy = 0.1)
ROU_fixtures$rou_BTTSN <- percent(ROU_fixtures$rou_BTTSN, accuracy = 0.1)
#odds
ROU_fixtures$rou_BTTSY_odds
ROU_fixtures$rou_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
ROU_fixtures$rou_AH_0_H <- (
  ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
    ROU_fixtures$rou_5_0 +ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5 + ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6
)
#AH_0_A
ROU_fixtures$rou_AH_0_A <- (
  ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
    ROU_fixtures$rou_0_5 +ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6 + ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6
)

#odds
ROU_fixtures$rou_AH_0_H_odds <- round((1/ROU_fixtures$rou_AH_0_H),digits = 2)
ROU_fixtures$rou_AH_0_A_odds <- round((1/ROU_fixtures$rou_AH_0_A),digits = 2)

ROU_fixtures$rou_AH_0_H_odds
ROU_fixtures$rou_AH_0_A_odds
#percentages
ROU_fixtures$rou_AH_0_H <- percent(ROU_fixtures$rou_AH_0_H, accuracy = 0.1)
ROU_fixtures$rou_AH_0_A <- percent(ROU_fixtures$rou_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
ROU_fixtures$rou_AH_n075_H <- (
  ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
    ROU_fixtures$rou_5_0 +ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5
)
#AH_n075_A
ROU_fixtures$rou_AH_n075_A <- (
  ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
    ROU_fixtures$rou_0_5 +ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6
)

#odds
ROU_fixtures$rou_AH_n075_H_odds <- round((1/ROU_fixtures$rou_AH_n075_H),digits = 2)
ROU_fixtures$rou_AH_n075_A_odds <- round((1/ROU_fixtures$rou_AH_n075_A),digits = 2)

ROU_fixtures$rou_AH_n075_H_odds
ROU_fixtures$rou_AH_n075_A_odds
#percentages
ROU_fixtures$rou_AH_n075_H <- percent(ROU_fixtures$rou_AH_n075_H, accuracy = 0.1)
ROU_fixtures$rou_AH_n075_A <- percent(ROU_fixtures$rou_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
ROU_fixtures$rou_AH_075_H <- (
  ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
    ROU_fixtures$rou_5_0 +ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5 + ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6 + ROU_fixtures$rou_0_1 + ROU_fixtures$rou_1_2 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_3_4 + ROU_fixtures$rou_4_5 + ROU_fixtures$rou_5_6
)
#AH_075_A
ROU_fixtures$rou_AH_075_A <- (
  ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
    ROU_fixtures$rou_0_5 +ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6 + ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6 + ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_3 + ROU_fixtures$rou_5_4 + ROU_fixtures$rou_6_5
)

#odds
ROU_fixtures$rou_AH_075_H_odds <- round((1/ROU_fixtures$rou_AH_075_H),digits = 2)
ROU_fixtures$rou_AH_075_A_odds <- round((1/ROU_fixtures$rou_AH_075_A),digits = 2)

ROU_fixtures$rou_AH_075_H_odds
ROU_fixtures$rou_AH_075_A_odds
#percentages
ROU_fixtures$rou_AH_075_H <- percent(ROU_fixtures$rou_AH_075_H, accuracy = 0.1)
ROU_fixtures$rou_AH_075_A <- percent(ROU_fixtures$rou_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
ROU_fixtures$rou_AH_n125_H <- (
  ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
    ROU_fixtures$rou_5_0 +ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5
)
#AH_n125_A
ROU_fixtures$rou_AH_n125_A <- (
  ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
    ROU_fixtures$rou_0_5 +ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6
)

#odds
ROU_fixtures$rou_AH_n125_H_odds <- round((1/ROU_fixtures$rou_AH_n125_H),digits = 2)
ROU_fixtures$rou_AH_n125_A_odds <- round((1/ROU_fixtures$rou_AH_n125_A),digits = 2)

ROU_fixtures$rou_AH_n125_H_odds
ROU_fixtures$rou_AH_n125_A_odds
#percentages
ROU_fixtures$rou_AH_n125_H <- percent(ROU_fixtures$rou_AH_n125_H, accuracy = 0.1)
ROU_fixtures$rou_AH_n125_A <- percent(ROU_fixtures$rou_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
ROU_fixtures$rou_AH_125_H <- (
  ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_0 + ROU_fixtures$rou_2_1 + ROU_fixtures$rou_3_0 + ROU_fixtures$rou_3_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_0 + ROU_fixtures$rou_4_1 + ROU_fixtures$rou_4_2 + ROU_fixtures$rou_4_3 +
    ROU_fixtures$rou_5_0 +ROU_fixtures$rou_5_1 + ROU_fixtures$rou_5_2 + ROU_fixtures$rou_5_3 + ROU_fixtures$rou_5_4 +
    ROU_fixtures$rou_6_0 + ROU_fixtures$rou_6_1 + ROU_fixtures$rou_6_2 + ROU_fixtures$rou_6_3 + ROU_fixtures$rou_6_4 +
    ROU_fixtures$rou_6_5 + ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6 + ROU_fixtures$rou_0_1 + ROU_fixtures$rou_1_2 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_3_4 + ROU_fixtures$rou_4_5 + ROU_fixtures$rou_5_6
)
#AH_125_A
ROU_fixtures$rou_AH_125_A <- (
  ROU_fixtures$rou_0_1 + ROU_fixtures$rou_0_2 + ROU_fixtures$rou_1_2 + ROU_fixtures$rou_0_3 + ROU_fixtures$rou_1_3 +
    ROU_fixtures$rou_2_3 + ROU_fixtures$rou_0_4 + ROU_fixtures$rou_1_4 + ROU_fixtures$rou_2_4 + ROU_fixtures$rou_3_4 +
    ROU_fixtures$rou_0_5 +ROU_fixtures$rou_1_5 + ROU_fixtures$rou_2_5 + ROU_fixtures$rou_3_5 + ROU_fixtures$rou_4_5 +
    ROU_fixtures$rou_0_6 + ROU_fixtures$rou_1_6 + ROU_fixtures$rou_2_6 + ROU_fixtures$rou_3_6 + ROU_fixtures$rou_4_6 +
    ROU_fixtures$rou_5_6 + ROU_fixtures$rou_0_0 + ROU_fixtures$rou_1_1 + ROU_fixtures$rou_2_2 + ROU_fixtures$rou_3_3 +
    ROU_fixtures$rou_4_4 + ROU_fixtures$rou_5_5 + ROU_fixtures$rou_6_6 + ROU_fixtures$rou_1_0 + ROU_fixtures$rou_2_1 +
    ROU_fixtures$rou_3_2 + ROU_fixtures$rou_4_3 + ROU_fixtures$rou_5_4 + ROU_fixtures$rou_6_5
)

#odds
ROU_fixtures$rou_AH_125_H_odds <- round((1/ROU_fixtures$rou_AH_125_H),digits = 2)
ROU_fixtures$rou_AH_125_A_odds <- round((1/ROU_fixtures$rou_AH_125_A),digits = 2)

ROU_fixtures$rou_AH_125_H_odds
ROU_fixtures$rou_AH_125_A_odds
#percentages
ROU_fixtures$rou_AH_125_H <- percent(ROU_fixtures$rou_AH_125_H, accuracy = 0.1)
ROU_fixtures$rou_AH_125_A <- percent(ROU_fixtures$rou_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
ROU_fixtures$rou_ov25 <- percent(ROU_fixtures$rou_ov25, accuracy = 0.1)

ROU_fixtures$rou_un25 <- percent(ROU_fixtures$rou_un25, accuracy = 0.1)
ROU_fixtures$rou_pscore <- paste(round(ROU_fixtures$rou_xGH,digits = 0),round(ROU_fixtures$rou_xGA,digits = 0),sep = "-")
#write out
write.xlsx(ROU_fixtures,'ROU.xlsx',sheetName = "ROU", append = TRUE)
###########################################################################################################
########################ROU END###########################################################################
ROU <- read.csv('../FDAS/ROU.csv')
ROU$TG <- ROU$HG + ROU$AG
ROU$OV25 <- ifelse(ROU$TG >= 3,"Y","N")
rou_ftr_summary <- tabyl(ROU,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
rou_ov25_summary <- tabyl(ROU,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(rou_ftr_summary,'ROU.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(rou_ov25_summary,'ROU.xlsx',sheetName = "OVUN25", append = TRUE)



