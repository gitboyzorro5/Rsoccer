library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('ARG.xlsx')
######################ARG START#######################################
#####################################################################
ARG <- read.csv('../FDAS/ARG.csv')

ARG <- within(ARG,rm(Res))

ARG$Date <- dmy(ARG$Date)
ARG <- ARG[order(as.Date(ARG$Date, format = "%d/%m%Y"), decreasing = FALSE),]
ARG$CS <- paste(ARG$HG,ARG$AG, sep = "-")
tail(ARG)
#ARG_qualificaton <- subset(ARG,tournament == "UEFA Euro qualification")
ARG <- subset(ARG,Season == "2021")
ARG <- subset(ARG,League == "Copa de la Liga Profesional")
#ARG <- ARG[ARG$Date > '2008-01-01',])
ARG$TG <- ARG$HG + ARG$AG
ARG$OV25 <- ifelse(ARG$TG >= 3,"Y","N")
ARG$FTR <- with(ARG,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
arg_totalgoalsv2 <- tapply(ARG$TG, ARG[c("Home", "Away")],mean)
arg_totalgoalsv2
arg_hgtotals <- rowSums(arg_totalgoalsv2,na.rm = T)
arg_agtotals <- colSums(arg_totalgoalsv2,na.rm = T)

arg_totalgoals <- arg_hgtotals + arg_agtotals
arg_totalgoalsv2 <- cbind(arg_totalgoalsv2,arg_totalgoals)
arg_teams <- sort(unique(ARG$Home))
arg_home_games <- c()
arg_away_games <-c()
for (i_arg in 1:length(arg_teams))
{

  arg_home_games[i_arg] <- nrow(ARG[ARG$Home == arg_teams[i_arg],])
  arg_away_games[i_arg]  <- nrow(ARG[ARG$Away == arg_teams[i_arg],])

}
arg_games_played <- arg_home_games + arg_away_games
arg_goaltotalsv2 <- cbind(arg_totalgoalsv2,arg_games_played)
arg_avg_totalgoals <- round((arg_totalgoals/ arg_games_played), digits = 4)
arg_goaltotalsv2[is.na(arg_goaltotalsv2)] <- ""
arg_goaltotalsv2 <- cbind(arg_goaltotalsv2,arg_avg_totalgoals)
write.xlsx(arg_goaltotalsv2,'ARG.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
arg_goalscored_h <- tapply(ARG$HG, ARG[c("Home", "Date")],mean)
arg_goalscored_a <- tapply(ARG$AG, ARG[c("Away", "Date")],mean)
arg_goalscored_h[is.na(arg_goalscored_h)] <- ""
arg_goalscored_a[is.na(arg_goalscored_a)] <- ""

for(arg_rowhgs in 1:nrow(arg_goalscored_h)) {
  for(arg_colhgs in 1:ncol(arg_goalscored_h)) {

    # print(my_matrix[row, col])
    for(arg_rowags in 1:nrow(arg_goalscored_a)) {
      for(arg_colags in 1:ncol(arg_goalscored_a)) {
        ifelse(!arg_goalscored_a[arg_rowags,arg_colags]=="",arg_goalscored_h[arg_rowags,arg_colags] <- arg_goalscored_a[arg_rowags,arg_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(arg_goalscored_h,'ARG.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
arg_goalconceded_h <- tapply(ARG$AG, ARG[c("Home", "Date")],mean)
arg_goalconceded_a <- tapply(ARG$HG, ARG[c("Away", "Date")],mean)
arg_goalconceded_h[is.na(arg_goalconceded_h)] <- ""
arg_goalconceded_a[is.na(arg_goalconceded_a)] <- ""

for(arg_rowhgc in 1:nrow(arg_goalconceded_h)) {
  for(arg_colhgc in 1:ncol(arg_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(arg_rowagc in 1:nrow(arg_goalconceded_a)) {
      for(arg_colagc in 1:ncol(arg_goalconceded_a)) {
        ifelse(!arg_goalconceded_a[arg_rowagc,arg_colagc]=="",arg_goalconceded_h[arg_rowagc,arg_colagc] <- arg_goalconceded_a[arg_rowagc,arg_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(arg_goalconceded_h,'ARG.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
arg_form_h <- tapply(ARG$FTR, ARG[c("Home", "Date")],median)
arg_form_a <- tapply(ARG$FTR, ARG[c("Away", "Date")],median)
arg_form_h[is.na(arg_form_h)] <- ""
arg_form_a[is.na(arg_form_a)] <- ""
arg_form_h <- sub("A","L",arg_form_h)
arg_form_h <- sub("H","W",arg_form_h)
arg_form_a <- sub("A","W",arg_form_a)
arg_form_a <- sub("H","L",arg_form_a)
for(arg_rowh_f in 1:nrow(arg_form_h)) {
  for(arg_colh_f in 1:ncol(arg_form_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f in 1:nrow(arg_form_a)) {
      for(arg_cola_f in 1:ncol(arg_form_a)) {
        ifelse(!arg_form_a[arg_rowa_f,arg_cola_f]=="",arg_form_h[arg_rowa_f,arg_cola_f] <- arg_form_a[arg_rowa_f,arg_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(arg_form_h,'ARG.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
arg_totalgoals_h <- tapply(ARG$TG, ARG[c("Home", "Date")],mean)
arg_totalgoals_a <- tapply(ARG$TG, ARG[c("Away", "Date")],mean)
arg_totalgoals_h[is.na(arg_totalgoals_h)] <- ""
arg_totalgoals_a[is.na(arg_totalgoals_a)] <- ""
for(arg_rowh in 1:nrow(arg_totalgoals_h)) {
  for(arg_colh in 1:ncol(arg_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa in 1:nrow(arg_totalgoals_a)) {
      for(arg_cola in 1:ncol(arg_totalgoals_a)) {
        ifelse(!arg_totalgoals_a[arg_rowa,arg_cola]=="",arg_totalgoals_h[arg_rowa,arg_cola] <- arg_totalgoals_a[arg_rowa,arg_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(arg_totalgoals_h,'ARG.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
arg_form_team_against_h <- tapply(ARG$Away, ARG[c("Home", "Date")],median)
arg_form_team_against_a <- tapply(ARG$Home, ARG[c("Away", "Date")],median)
arg_form_team_against_h[is.na(arg_form_team_against_h)] <- ""
arg_form_team_against_a[is.na(arg_form_team_against_a)] <- ""
for(arg_rowh_f_against in 1:nrow(arg_form_team_against_h)) {
  for(arg_colh_f_against in 1:ncol(arg_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f_against in 1:nrow(arg_form_team_against_a)) {
      for(arg_cola_f_against in 1:ncol(arg_form_team_against_a)) {
        ifelse(!arg_form_team_against_a[arg_rowa_f_against,arg_cola_f_against]=="",arg_form_team_against_h[arg_rowa_f_against,arg_cola_f_against] <- arg_form_team_against_a[arg_rowa_f_against,arg_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#ARG
arg_un05_home <- c()
arg_un05_away <- c()
arg_ov05_home <- c()
arg_ov05_away <- c()

arg_un15_home <- c()
arg_un15_away <- c()
arg_ov15_home <- c()
arg_ov15_away <- c()

arg_un25_home <- c()
arg_un25_away <- c()
arg_ov25_home <- c()
arg_ov25_away <- c()

arg_un35_home <- c()
arg_un35_away <- c()
arg_ov35_home <- c()
arg_ov35_away <- c()

arg_un45_home <- c()
arg_un45_away <- c()
arg_ov45_home <- c()
arg_ov45_away <- c()

arg_un55_home <- c()
arg_un55_away <- c()
arg_ov55_home <- c()
arg_ov55_away <- c()

for (i_arg_tg in 1:length(arg_teams))
{

  arg_un05_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG == 0,])
  arg_un05_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG == 0,])

  arg_ov05_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG > 0,])
  arg_ov05_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG > 0,])

  arg_un15_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG <= 1,])
  arg_un15_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG <= 1,])

  arg_ov15_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG >= 2,])
  arg_ov15_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG >= 2,])

  arg_un25_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG <= 2,])
  arg_un25_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG <= 2,])

  arg_ov25_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG >=3,])
  arg_ov25_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG >=3,])

  arg_un35_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG <= 3,])
  arg_un35_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG <= 3,])

  arg_ov35_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG >= 4,])
  arg_ov35_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG >= 4,])

  arg_un45_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG <= 4,])
  arg_un45_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG <= 4,])

  arg_ov45_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG >= 5,])
  arg_ov45_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG >= 5,])

  arg_un55_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG <= 5,])
  arg_un55_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG <= 5,])

  arg_ov55_home[i_arg_tg] <- nrow(ARG[ARG$Home == arg_teams[i_arg_tg] & ARG$TG >= 6,])
  arg_ov55_away[i_arg_tg] <- nrow(ARG[ARG$Away == arg_teams[i_arg_tg] & ARG$TG >= 6,])


}

arg_un05 <- arg_un05_home + arg_un05_away
arg_ov05 <- arg_ov05_home + arg_ov05_away

arg_un15 <- arg_un15_home + arg_un15_away
arg_ov15 <- arg_ov15_home + arg_ov15_away

arg_un25 <- arg_un25_home + arg_un25_away
arg_ov25 <- arg_ov25_home + arg_ov25_away

arg_un35 <- arg_un35_home + arg_un35_away
arg_ov35 <- arg_ov35_home + arg_ov35_away

arg_un45 <- arg_un45_home + arg_un45_away
arg_ov45 <- arg_ov45_home + arg_ov45_away

arg_un55 <- arg_un55_home + arg_un55_away
arg_ov55 <- arg_ov55_home + arg_ov55_away

arg_ovundata <- cbind(arg_teams,arg_un05,arg_ov05,arg_un15,arg_ov15,arg_un25,arg_ov25,arg_un35,arg_ov35,arg_un45,arg_ov45,arg_un55,arg_ov55)
write.xlsx(arg_ovundata,'ARG.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
arg_csform_h <- tapply(ARG$CS, ARG[c("Home", "Date")],median)
arg_csform_a <- tapply(ARG$CS, ARG[c("Away", "Date")],median)

arg_csform_h[is.na(arg_csform_h)] <- ""
arg_csform_a[is.na(arg_csform_a)] <- ""

for(arg_rowh_f_cs in 1:nrow(arg_csform_h)) {
  for(arg_colh_f_cs in 1:ncol(arg_csform_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f_cs in 1:nrow(arg_csform_a)) {
      for(arg_cola_f_cs in 1:ncol(arg_csform_a)) {
        ifelse(!arg_csform_a[arg_rowa_f_cs,arg_cola_f_cs]=="",arg_csform_h[arg_rowa_f_cs,arg_cola_f_cs] <- arg_csform_a[arg_rowa_f_cs,arg_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
arg_home_gs <- aggregate(ARG$HG, by = list(ARG$Home), FUN = sum)
arg_home_gs_avg <- aggregate(ARG$HG, by = list(ARG$Home),mean)
arg_home_scoring <- merge(arg_home_gs,arg_home_gs_avg, by='Group.1',all = T)
names(arg_home_scoring)[names(arg_home_scoring) == "x.x"] <- "TFthg"
names(arg_home_scoring)[names(arg_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
arg_away_gs <- aggregate(ARG$AG, by = list(ARG$Away), FUN = sum)
arg_away_gs_avg <- aggregate(ARG$AG, by = list(ARG$Away),mean)
arg_away_scoring <- merge(arg_away_gs,arg_away_gs_avg, by='Group.1',all = T)
names(arg_away_scoring)[names(arg_away_scoring) == "x.x"] <- "TFtag"
names(arg_away_scoring)[names(arg_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
arg_scoring <- merge(arg_home_scoring,arg_away_scoring,by='Group.1',all = T)
arg_scoring$TGS <- arg_scoring$TFthg + arg_scoring$TFtag

#home goals conceded
arg_home_gc <- aggregate(ARG$AG, by = list(ARG$Home), FUN = sum)
arg_home_gc_avg <- aggregate(ARG$AG, by = list(ARG$Home),mean)
arg_home_conceding <- merge(arg_home_gc,arg_home_gc_avg, by='Group.1',all = T)
names(arg_home_conceding)[names(arg_home_conceding) == "x.x"] <- "TFthc"
names(arg_home_conceding)[names(arg_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
arg_away_gc <- aggregate(ARG$HG, by = list(ARG$Away), FUN = sum)
arg_away_gc_avg <- aggregate(ARG$HG, by = list(ARG$Away),mean)
arg_away_conceding <- merge(arg_away_gc,arg_away_gc_avg, by='Group.1',all = T)
names(arg_away_conceding)[names(arg_away_conceding) == "x.x"] <- "TFtac"
names(arg_away_conceding)[names(arg_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
arg_conceding <- merge(arg_home_conceding,arg_away_conceding,by='Group.1',all = T)
arg_conceding$TGC <- arg_conceding$TFthc + arg_conceding$TFtac

arg_shots_analysis <- merge(arg_scoring_conversion,arg_conceding_conversion,by='Group.1',all = T)

######################################################################################
###########League Table###############################################################

#hwins and away wins
arg_home_wins <- c()
arg_away_wins <- c()
arg_home_draws <- c()
arg_away_draws <- c()
arg_home_loss <- c()
arg_away_loss <- c()



for (i_arg_wins in 1:length(arg_teams))
{

  arg_home_wins[i_arg_wins] <- nrow(ARG[ARG$Home == arg_teams[i_arg_wins] & ARG$FTR == "H",])
  arg_away_wins[i_arg_wins] <- nrow(ARG[ARG$Away == arg_teams[i_arg_wins] & ARG$FTR == "A",])
  arg_home_draws[i_arg_wins] <- nrow(ARG[ARG$Home == arg_teams[i_arg_wins] & ARG$FTR == "D",])
  arg_away_draws[i_arg_wins] <- nrow(ARG[ARG$Away == arg_teams[i_arg_wins] & ARG$FTR == "D",])
  arg_home_loss[i_arg_wins] <- nrow(ARG[ARG$Home == arg_teams[i_arg_wins] & ARG$FTR == "A",])
  arg_away_loss[i_arg_wins] <- nrow(ARG[ARG$Away == arg_teams[i_arg_wins] & ARG$FTR == "H",])

}

arg_total_wins <- arg_home_wins + arg_away_wins
arg_total_draws <- arg_home_draws + arg_away_draws
arg_total_loss <- arg_home_loss + arg_away_loss

arg_league_table <- cbind(arg_teams,arg_games_played,arg_total_wins,arg_total_draws,arg_total_loss)
arg_GS <- arg_scoring$TGS
arg_GC <-arg_conceding$TGC
arg_GD <- arg_scoring$TGS - arg_conceding$TGC
arg_PTS <- (arg_total_wins*3) + (arg_total_draws*1)
arg_league_table <- cbind(arg_league_table,arg_GS,arg_GC,arg_GD,arg_PTS)
arg_league_table <- as.data.frame(arg_league_table)
#rename the columns
names(arg_league_table)[names(arg_league_table) == "arg_teams"] <- "Team"
names(arg_league_table)[names(arg_league_table) == "arg_games_played"] <- "P"
names(arg_league_table)[names(arg_league_table) == "arg_total_wins"] <- "W"
names(arg_league_table)[names(arg_league_table) == "arg_total_draws"] <- "D"
names(arg_league_table)[names(arg_league_table) == "arg_total_loss"] <- "L"
names(arg_league_table)[names(arg_league_table) == "arg_GS"] <- "F"
names(arg_league_table)[names(arg_league_table) == "arg_GC"] <- "A"
points_arg <- arg_league_table[order(arg_league_table$arg_PTS, decreasing = TRUE),]
write.xlsx(points_arg,'ARG.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six arg###################################################
#ARG
#form
#create final_arg_hf object
final_arg_hf <- c()
for(index_arg_hf in 1:length(arg_teams))
{
  index_arg_hf <- row.names(arg_form_h) == arg_teams[index_arg_hf]
  form_arg_hf <- arg_form_h[index_arg_hf]
  deleted_form_arg_hf <- form_arg_hf[!form_arg_hf[] == ""]
  l6_form_arg_hf <- tail(deleted_form_arg_hf,6)
  l6_form_arg_hf <- paste(l6_form_arg_hf,collapse = " ")
  final_arg_hf[index_arg_hf] <- rbind(paste(arg_teams[index_arg_hf],l6_form_arg_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}

#change column names
final_arg_hf <- as.data.frame(final_arg_hf)
colnames(final_arg_hf) <- "Form"
#goals scored
#create final_arg_gs object
final_arg_gs <- c()
suml6_arg_gs <- c()
for(index_arg_gs in 1:length(arg_teams))
{
  index_arg_gs <- row.names(arg_goalscored_h) == arg_teams[index_arg_gs]
  form_arg_gs <- arg_goalscored_h[index_arg_gs]
  deleted_form_arg_gs <- form_arg_gs[!form_arg_gs[] == ""]
  l6_form_arg_gs <- tail(deleted_form_arg_gs,6)
  l6_form_arg_gs <- as.numeric(l6_form_arg_gs)
  suml6_arg_gs[index_arg_gs] <- sum(l6_form_arg_gs)
  suml6_arg_gs[index_arg_gs] <- paste("(",suml6_arg_gs[index_arg_gs],")",sep = "")
  l6_form_arg_gs <- paste(l6_form_arg_gs,collapse = " ")
  final_arg_gs[index_arg_gs] <- rbind(paste(arg_teams[index_arg_gs],l6_form_arg_gs,suml6_arg_gs[index_arg_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_gs
#change column names
final_arg_gs <- as.data.frame(final_arg_gs)
colnames(final_arg_gs) <- "Goals scored"
#goal conceded
#create final_arg_gc object
final_arg_gc <- c()
suml6_arg_gc <- c()
for(index_arg_gc in 1:length(arg_teams))
{
  index_arg_gc <- row.names(arg_goalconceded_h) == arg_teams[index_arg_gc]
  form_arg_gc <- arg_goalconceded_h[index_arg_gc]
  deleted_form_arg_gc <- form_arg_gc[!form_arg_gc[] == ""]
  l6_form_arg_gc <- tail(deleted_form_arg_gc,6)
  l6_form_arg_gc <- as.numeric(l6_form_arg_gc)
  suml6_arg_gc[index_arg_gc] <- sum(l6_form_arg_gc)
  suml6_arg_gc[index_arg_gc] <- paste("(",suml6_arg_gc[index_arg_gc],")",sep = "")
  l6_form_arg_gc <- paste(l6_form_arg_gc,collapse = " ")
  final_arg_gc[index_arg_gc] <- rbind(paste(arg_teams[index_arg_gc],l6_form_arg_gc,suml6_arg_gc[index_arg_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}

#change column names
final_arg_gc <- as.data.frame(final_arg_gc)
colnames(final_arg_gc) <- "Goals conceded"
#total goals
#create final_arg_tg object
final_arg_tg <- c()
suml6_arg_tg <- c()
for(index_arg_tg in 1:length(arg_teams))
{
  index_arg_tg <- row.names(arg_totalgoals_h) == arg_teams[index_arg_tg]
  form_arg_tg <- arg_totalgoals_h[index_arg_tg]
  deleted_form_arg_tg <- form_arg_tg[!form_arg_tg[] == ""]
  l6_form_arg_tg <- tail(deleted_form_arg_tg,6)
  l6_form_arg_tg <- as.numeric(l6_form_arg_tg)
  suml6_arg_tg[index_arg_tg] <- sum(l6_form_arg_tg)
  suml6_arg_tg[index_arg_tg] <- paste("(",suml6_arg_tg[index_arg_tg],")",sep = "")
  l6_form_arg_tg <- paste(l6_form_arg_tg,collapse = " ")
  final_arg_tg[index_arg_tg] <- rbind(paste(arg_teams[index_arg_tg],l6_form_arg_tg,suml6_arg_tg[index_arg_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
#change column names
final_arg_tg <- as.data.frame(final_arg_tg)
colnames(final_arg_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_arg_hf object
final_arg_cs <- c()
for(index_arg_cs in 1:length(arg_teams))
{
  index_arg_cs <- row.names(arg_csform_h) == arg_teams[index_arg_cs]
  csform_arg_cs <- arg_csform_h[index_arg_cs]
  deleted_csform_arg_cs <- csform_arg_cs[!csform_arg_cs[] == ""]
  l6_csform_arg_cs <- tail(deleted_csform_arg_cs,arg_last_n_games)
  l6_csform_arg_cs <- paste(l6_csform_arg_cs,collapse = " ")
  final_arg_cs[index_arg_cs] <- rbind(paste(arg_teams[index_arg_cs],l6_csform_arg_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",arg_teams[index],l6_csform)

}

#change column names
final_arg_cs <- as.data.frame(final_arg_cs)
colnames(final_arg_cs) <- "CSForm"
#################################################
#Team against
#create final_arg_hf_against
final_arg_hf_against <- c()
for(index_arg_hf_against in 1:length(arg_teams))
{
  index_arg_hf_against <- row.names(arg_form_team_against_h) == arg_teams[index_arg_hf_against]
  form_arg_hf_against <- arg_form_team_against_h[index_arg_hf_against]
  deleted_form_arg_hf_against <- form_arg_hf_against[!form_arg_hf_against[] == ""]
  l6_form_arg_hf_against <- tail(deleted_form_arg_hf_against,6)
  l6_form_arg_hf_against <- paste(l6_form_arg_hf_against,collapse = " ")
  final_arg_hf_against[index_arg_hf_against] <- rbind(paste(arg_teams[index_arg_hf_against],l6_form_arg_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_hf_against <- as.data.frame(final_arg_hf_against)
colnames(final_arg_hf_against) <- "Team against"
#combine the columns
final_arg_all <- cbind(final_arg_hf,final_arg_gs,final_arg_gc,final_arg_tg,final_arg_cs,final_arg_hf_against)
write.xlsx(final_arg_all,'ARG.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
arg_GP <- nrow(ARG)
#Calculate total home goals for each division
arg_T_HG <- sum(arg_home_gs$x)
#calculate average home goal
arg_avg_HG <- round(arg_T_HG /arg_GP, digits = 4)
############################################################
#Calculate total away goals for each division
arg_T_AG <- sum(arg_away_gs$x)
#calculate average away goal
arg_avg_AG <- round(arg_T_AG /arg_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
arg_home_as <- round(((arg_home_gs$x/arg_home_games))/arg_avg_HG, digits = 4)
#calculate away attack strength
arg_away_as <- round(((arg_away_gs$x/arg_away_games))/arg_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
arg_avg_HC <- round(arg_T_AG /arg_GP, digits = 4)
#avg away concede
arg_avg_AC <- round(arg_T_HG /arg_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
arg_home_ds <- round(((arg_home_gc$x/arg_home_games))/arg_avg_HC, digits = 4)
#away defense strength
arg_away_ds <- round(((arg_away_gc$x/arg_away_games))/arg_avg_AC, digits = 4)
#############################################################################
#home poisson data
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_home_poisson <- cbind(arg_division,arg_teams,arg_avg_HG,arg_home_as,arg_home_ds)
#################################################################################
#away poisson data
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_away_poisson <- cbind(arg_division,arg_teams,arg_avg_AG,arg_away_as,arg_away_ds)

#create home and away csv
#arg_home_poisson <- rbind(arg_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#arg_away_poisson <- rbind(arg_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(arg_home_poisson,'ARG.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(arg_away_poisson,'ARG.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################ARG FIXTURES##########################################################################
#ARG
HomeTeam_arg <- rep(arg_teams, each = length(arg_teams))
AwayTeam_arg <- rep(arg_teams, length(arg_teams))
ARG_fixtures <- cbind(HomeTeam_arg,AwayTeam_arg)
ARG_fixtures <- as.data.frame(ARG_fixtures)
ARG_fixtures <- ARG_fixtures[!ARG_fixtures$HomeTeam_arg == ARG_fixtures$AwayTeam_arg,]
rownames(ARG_fixtures) <- NULL
ARG_fixtures$Div <- "ARG"
ARG_fixtures <- ARG_fixtures[,c(3,1,2)]

ARG_fixtures$avg_HG_arg <- arg_avg_HG

ARG_fixtures$arg_homeas <- rep(arg_home_as,each = length(arg_teams)-1)

arg_awayds_lookup <- cbind(arg_teams,arg_away_ds)

arg_awayds_lookup <- as.data.frame(arg_awayds_lookup)

colnames(arg_awayds_lookup) <- c("AwayTeam_arg","arg_awayds")


require('RH2')
ARG_fixtures$arg_awayds <- sqldf("SELECT arg_awayds_lookup.arg_awayds FROM arg_awayds_lookup INNER JOIN ARG_fixtures ON arg_awayds_lookup.AwayTeam_arg = ARG_fixtures.AwayTeam_arg")

ARG_fixtures$avg_AG_arg <- arg_avg_AG

arg_awayas_lookup <- cbind(arg_teams,arg_away_as)

arg_awayas_lookup <- as.data.frame(arg_awayas_lookup)

colnames(arg_awayas_lookup) <- c("AwayTeam_arg","arg_awayas")


ARG_fixtures$arg_awayas <- sqldf("SELECT arg_awayas_lookup.arg_awayas FROM arg_awayas_lookup INNER JOIN ARG_fixtures ON arg_awayas_lookup.AwayTeam_arg = ARG_fixtures.AwayTeam_arg")

ARG_fixtures$arg_homeds <- rep(arg_home_ds,each = length(arg_teams)-1)

ARG_fixtures$arg_awayds <- as.numeric(unlist(ARG_fixtures$arg_awayds))
#xGH
ARG_fixtures$arg_xGH <- ARG_fixtures$avg_HG_arg * ARG_fixtures$arg_homeas * ARG_fixtures$arg_awayds

#xGA

ARG_fixtures$arg_awayas <- as.numeric(unlist(ARG_fixtures$arg_awayas))

ARG_fixtures$arg_xGA <- ARG_fixtures$avg_AG_arg * ARG_fixtures$arg_awayas * ARG_fixtures$arg_homeds

ARG_fixtures$arg_0_0 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_0 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_1 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_1 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_0 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_2 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_2 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_1 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_2 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_3 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_0 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_1 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_2 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_3 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_3 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_3 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_4 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_0 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_1 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_2 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_3 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_4 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_4 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_4 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_4 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_5 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_0 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_1 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_2 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_3 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_4 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_5 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_5 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_5 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_5 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_5 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_6 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_0 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_1 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_2 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_3 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_4 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_5 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_6 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_6 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_6 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_6 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_6 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_6 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
#Home win
ARG_fixtures$arg_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 + ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5
)

ARG_fixtures$arg_H <- percent(ARG_fixtures$arg_H, accuracy = 0.1)

#Draw
ARG_fixtures$arg_D <- (

  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 + ARG_fixtures$arg_4_4 +
    ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6
)

ARG_fixtures$arg_D <- percent(ARG_fixtures$arg_D, accuracy = 0.1)

#Away

ARG_fixtures$arg_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 + ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6
)

ARG_fixtures$arg_A <- percent(ARG_fixtures$arg_A, accuracy = 0.1)

#ov25
ARG_fixtures$arg_ov25 <- (
  ARG_fixtures$arg_2_1 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 + ARG_fixtures$arg_2_3 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 + ARG_fixtures$arg_0_4 +
    ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 + ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_0 +
    ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 + ARG_fixtures$arg_0_5 +
    ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 + ARG_fixtures$arg_5_5 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5 + ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 +
    ARG_fixtures$arg_4_6 + ARG_fixtures$arg_5_6 + ARG_fixtures$arg_6_6
)
#un25
ARG_fixtures$arg_un25 <- (
  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_0 + ARG_fixtures$arg_0_1 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_0_2
)
#odds
ARG_fixtures$arg_ov25_odds <- round((1/ARG_fixtures$arg_ov25),digits = 2)
ARG_fixtures$arg_un25_odds <- round((1/ARG_fixtures$arg_un25),digits = 2)

ARG_fixtures$arg_ov25_odds
ARG_fixtures$arg_un25_odds
#percentages
ARG_fixtures$arg_ov25 <- percent(ARG_fixtures$arg_ov25, accuracy = 0.1)

ARG_fixtures$arg_un25 <- percent(ARG_fixtures$arg_un25, accuracy = 0.1)
ARG_fixtures$arg_pscore <- paste(round(ARG_fixtures$arg_xGH,digits = 0),round(ARG_fixtures$arg_xGA,digits = 0),sep = "-")
#write out
write.xlsx(ARG_fixtures,'ARG.xlsx',sheetName = "ARG", append = TRUE)
###########################################################################################################
########################ARG END###########################################################################
ARG <- read.csv('../FDAS/ARG.csv')
ARG$TG <- ARG$HG + ARG$AG
ARG$OV25 <- ifelse(ARG$TG >= 3,"Y","N")
arg_ftr_summary <- tabyl(ARG,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
arg_ov25_summary <- tabyl(ARG,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(arg_ftr_summary,'ARG.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(arg_ov25_summary,'ARG.xlsx',sheetName = "OVUN25", append = TRUE)



