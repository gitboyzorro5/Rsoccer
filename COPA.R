library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('COPA.xlsx')
######################COPA START#######################################
#####################################################################
COPA <- read.csv('../../../Leonard.000/Downloads/results.csv')
sort(unique(COPA$tournament))
COPA$date <- ymd(COPA$date)
COPA <- COPA[order(as.Date(COPA$date, format = "%d/%m%Y"), decreasing = FALSE),]
COPA$CS <- paste(COPA$home_score,COPA$away_score, sep = "-")
COPA <- subset(COPA,tournament == "Copa AmÃ©rica")
COPA <- COPA[COPA$date > '2021-06-11' & COPA$date < '2021-06-23',]
COPA$TG <- COPA$home_score + COPA$away_score
COPA$OV25 <- ifelse(COPA$TG >= 3,"Y","N")
COPA$FTR <- with(COPA,
                 ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
)
COPA
###################################################
####GoalTotalsv2##################################
copa_totalgoalsv2 <- tapply(COPA$TG, COPA[c("home_team", "away_team")],mean)
copa_totalgoalsv2
copa_hgtotals <- rowSums(copa_totalgoalsv2,na.rm = T)
copa_agtotals <- colSums(copa_totalgoalsv2,na.rm = T)

copa_totalgoals <- copa_hgtotals + copa_agtotals
copa_totalgoalsv2 <- cbind(copa_totalgoalsv2,copa_totalgoals)
copa_teams <- sort(unique(COPA$home_team))
copa_home_games <- c()
copa_away_games <-c()
for (i_copa in 1:length(copa_teams))
{

  copa_home_games[i_copa] <- nrow(COPA[COPA$home_team == copa_teams[i_copa],])
  copa_away_games[i_copa]  <- nrow(COPA[COPA$away_team == copa_teams[i_copa],])

}
copa_games_played <- copa_home_games + copa_away_games
copa_goaltotalsv2 <- cbind(copa_totalgoalsv2,copa_games_played)
copa_avg_totalgoals <- round((copa_totalgoals/ copa_games_played), digits = 4)
copa_goaltotalsv2[is.na(copa_goaltotalsv2)] <- ""
copa_goaltotalsv2 <- cbind(copa_goaltotalsv2,copa_avg_totalgoals)
write.xlsx(copa_goaltotalsv2,'COPA.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
copa_goalscored_h <- tapply(COPA$home_score, COPA[c("home_team", "date")],mean)
copa_goalscored_a <- tapply(COPA$away_score, COPA[c("away_team", "date")],mean)
copa_goalscored_h[is.na(copa_goalscored_h)] <- ""
copa_goalscored_a[is.na(copa_goalscored_a)] <- ""

for(copa_rowhgs in 1:nrow(copa_goalscored_h)) {
  for(copa_colhgs in 1:ncol(copa_goalscored_h)) {

    # print(my_matrix[row, col])
    for(copa_rowags in 1:nrow(copa_goalscored_a)) {
      for(copa_colags in 1:ncol(copa_goalscored_a)) {
        ifelse(!copa_goalscored_a[copa_rowags,copa_colags]=="",copa_goalscored_h[copa_rowags,copa_colags] <- copa_goalscored_a[copa_rowags,copa_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(copa_goalscored_h,'COPA.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
copa_goalconceded_h <- tapply(COPA$away_score, COPA[c("home_team", "date")],mean)
copa_goalconceded_a <- tapply(COPA$home_score, COPA[c("away_team", "date")],mean)
copa_goalconceded_h[is.na(copa_goalconceded_h)] <- ""
copa_goalconceded_a[is.na(copa_goalconceded_a)] <- ""

for(copa_rowhgc in 1:nrow(copa_goalconceded_h)) {
  for(copa_colhgc in 1:ncol(copa_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(copa_rowagc in 1:nrow(copa_goalconceded_a)) {
      for(copa_colagc in 1:ncol(copa_goalconceded_a)) {
        ifelse(!copa_goalconceded_a[copa_rowagc,copa_colagc]=="",copa_goalconceded_h[copa_rowagc,copa_colagc] <- copa_goalconceded_a[copa_rowagc,copa_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(copa_goalconceded_h,'COPA.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
copa_form_h <- tapply(COPA$FTR, COPA[c("home_team", "date")],median)
copa_form_a <- tapply(COPA$FTR, COPA[c("away_team", "date")],median)
copa_form_h[is.na(copa_form_h)] <- ""
copa_form_a[is.na(copa_form_a)] <- ""
copa_form_h <- sub("A","L",copa_form_h)
copa_form_h <- sub("H","W",copa_form_h)
copa_form_a <- sub("A","W",copa_form_a)
copa_form_a <- sub("H","L",copa_form_a)
for(copa_rowh_f in 1:nrow(copa_form_h)) {
  for(copa_colh_f in 1:ncol(copa_form_h)) {

    # print(my_matrix[row, col])
    for(copa_rowa_f in 1:nrow(copa_form_a)) {
      for(copa_cola_f in 1:ncol(copa_form_a)) {
        ifelse(!copa_form_a[copa_rowa_f,copa_cola_f]=="",copa_form_h[copa_rowa_f,copa_cola_f] <- copa_form_a[copa_rowa_f,copa_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(copa_form_h,'COPA.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
copa_totalgoals_h <- tapply(COPA$TG, COPA[c("home_team", "date")],mean)
copa_totalgoals_a <- tapply(COPA$TG, COPA[c("away_team", "date")],mean)
copa_totalgoals_h[is.na(copa_totalgoals_h)] <- ""
copa_totalgoals_a[is.na(copa_totalgoals_a)] <- ""
for(copa_rowh in 1:nrow(copa_totalgoals_h)) {
  for(copa_colh in 1:ncol(copa_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(copa_rowa in 1:nrow(copa_totalgoals_a)) {
      for(copa_cola in 1:ncol(copa_totalgoals_a)) {
        ifelse(!copa_totalgoals_a[copa_rowa,copa_cola]=="",copa_totalgoals_h[copa_rowa,copa_cola] <- copa_totalgoals_a[copa_rowa,copa_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(copa_totalgoals_h,'COPA.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
copa_form_team_against_h <- tapply(COPA$away_team, COPA[c("home_team", "date")],median)
copa_form_team_against_a <- tapply(COPA$home_team, COPA[c("away_team", "date")],median)
copa_form_team_against_h[is.na(copa_form_team_against_h)] <- ""
copa_form_team_against_a[is.na(copa_form_team_against_a)] <- ""
for(copa_rowh_f_against in 1:nrow(copa_form_team_against_h)) {
  for(copa_colh_f_against in 1:ncol(copa_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(copa_rowa_f_against in 1:nrow(copa_form_team_against_a)) {
      for(copa_cola_f_against in 1:ncol(copa_form_team_against_a)) {
        ifelse(!copa_form_team_against_a[copa_rowa_f_against,copa_cola_f_against]=="",copa_form_team_against_h[copa_rowa_f_against,copa_cola_f_against] <- copa_form_team_against_a[copa_rowa_f_against,copa_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#COPA
copa_un05_home <- c()
copa_un05_away <- c()
copa_ov05_home <- c()
copa_ov05_away <- c()

copa_un15_home <- c()
copa_un15_away <- c()
copa_ov15_home <- c()
copa_ov15_away <- c()

copa_un25_home <- c()
copa_un25_away <- c()
copa_ov25_home <- c()
copa_ov25_away <- c()

copa_un35_home <- c()
copa_un35_away <- c()
copa_ov35_home <- c()
copa_ov35_away <- c()

copa_un45_home <- c()
copa_un45_away <- c()
copa_ov45_home <- c()
copa_ov45_away <- c()

copa_un55_home <- c()
copa_un55_away <- c()
copa_ov55_home <- c()
copa_ov55_away <- c()

for (i_copa_tg in 1:length(copa_teams))
{

  copa_un05_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG == 0,])
  copa_un05_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG == 0,])

  copa_ov05_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG > 0,])
  copa_ov05_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG > 0,])

  copa_un15_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG <= 1,])
  copa_un15_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG <= 1,])

  copa_ov15_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG >= 2,])
  copa_ov15_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG >= 2,])

  copa_un25_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG <= 2,])
  copa_un25_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG <= 2,])

  copa_ov25_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG >=3,])
  copa_ov25_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG >=3,])

  copa_un35_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG <= 3,])
  copa_un35_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG <= 3,])

  copa_ov35_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG >= 4,])
  copa_ov35_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG >= 4,])

  copa_un45_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG <= 4,])
  copa_un45_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG <= 4,])

  copa_ov45_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG >= 5,])
  copa_ov45_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG >= 5,])

  copa_un55_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG <= 5,])
  copa_un55_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG <= 5,])

  copa_ov55_home[i_copa_tg] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_tg] & COPA$TG >= 6,])
  copa_ov55_away[i_copa_tg] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_tg] & COPA$TG >= 6,])


}

copa_un05 <- copa_un05_home + copa_un05_away
copa_ov05 <- copa_ov05_home + copa_ov05_away

copa_un15 <- copa_un15_home + copa_un15_away
copa_ov15 <- copa_ov15_home + copa_ov15_away

copa_un25 <- copa_un25_home + copa_un25_away
copa_ov25 <- copa_ov25_home + copa_ov25_away

copa_un35 <- copa_un35_home + copa_un35_away
copa_ov35 <- copa_ov35_home + copa_ov35_away

copa_un45 <- copa_un45_home + copa_un45_away
copa_ov45 <- copa_ov45_home + copa_ov45_away

copa_un55 <- copa_un55_home + copa_un55_away
copa_ov55 <- copa_ov55_home + copa_ov55_away

copa_ovundata <- cbind(copa_teams,copa_un05,copa_ov05,copa_un15,copa_ov15,copa_un25,copa_ov25,copa_un35,copa_ov35,copa_un45,copa_ov45,copa_un55,copa_ov55)
write.xlsx(copa_ovundata,'COPA.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
copa_csform_h <- tapply(COPA$CS, COPA[c("home_team", "date")],median)
copa_csform_a <- tapply(COPA$CS, COPA[c("away_team", "date")],median)

copa_csform_h[is.na(copa_csform_h)] <- ""
copa_csform_a[is.na(copa_csform_a)] <- ""

for(copa_rowh_f_cs in 1:nrow(copa_csform_h)) {
  for(copa_colh_f_cs in 1:ncol(copa_csform_h)) {

    # print(my_matrix[row, col])
    for(copa_rowa_f_cs in 1:nrow(copa_csform_a)) {
      for(copa_cola_f_cs in 1:ncol(copa_csform_a)) {
        ifelse(!copa_csform_a[copa_rowa_f_cs,copa_cola_f_cs]=="",copa_csform_h[copa_rowa_f_cs,copa_cola_f_cs] <- copa_csform_a[copa_rowa_f_cs,copa_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
copa_home_gs <- aggregate(COPA$home_score, by = list(COPA$home_team), FUN = sum)
copa_home_gs_avg <- aggregate(COPA$home_score, by = list(COPA$home_team),mean)
copa_home_scoring <- merge(copa_home_gs,copa_home_gs_avg, by='Group.1',all = T)
names(copa_home_scoring)[names(copa_home_scoring) == "x.x"] <- "TFthg"
names(copa_home_scoring)[names(copa_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
copa_away_gs <- aggregate(COPA$away_score, by = list(COPA$away_team), FUN = sum)
copa_away_gs_avg <- aggregate(COPA$away_score, by = list(COPA$away_team),mean)
copa_away_scoring <- merge(copa_away_gs,copa_away_gs_avg, by='Group.1',all = T)
names(copa_away_scoring)[names(copa_away_scoring) == "x.x"] <- "TFtag"
names(copa_away_scoring)[names(copa_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
copa_scoring <- merge(copa_home_scoring,copa_away_scoring,by='Group.1',all = T)
copa_scoring$TGS <- copa_scoring$TFthg + copa_scoring$TFtag

#home goals conceded
copa_home_gc <- aggregate(COPA$away_score, by = list(COPA$home_team), FUN = sum)
copa_home_gc_avg <- aggregate(COPA$away_score, by = list(COPA$home_team),mean)
copa_home_conceding <- merge(copa_home_gc,copa_home_gc_avg, by='Group.1',all = T)
names(copa_home_conceding)[names(copa_home_conceding) == "x.x"] <- "TFthc"
names(copa_home_conceding)[names(copa_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
copa_away_gc <- aggregate(COPA$home_score, by = list(COPA$away_team), FUN = sum)
copa_away_gc_avg <- aggregate(COPA$home_score, by = list(COPA$away_team),mean)
copa_away_conceding <- merge(copa_away_gc,copa_away_gc_avg, by='Group.1',all = T)
names(copa_away_conceding)[names(copa_away_conceding) == "x.x"] <- "TFtac"
names(copa_away_conceding)[names(copa_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
copa_conceding <- merge(copa_home_conceding,copa_away_conceding,by='Group.1',all = T)
copa_conceding$TGC <- copa_conceding$TFthc + copa_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
copa_home_wins <- c()
copa_away_wins <- c()
copa_home_draws <- c()
copa_away_draws <- c()
copa_home_loss <- c()
copa_away_loss <- c()



for (i_copa_wins in 1:length(copa_teams))
{

  copa_home_wins[i_copa_wins] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_wins] & COPA$FTR == "H",])
  copa_away_wins[i_copa_wins] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_wins] & COPA$FTR == "A",])
  copa_home_draws[i_copa_wins] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_wins] & COPA$FTR == "D",])
  copa_away_draws[i_copa_wins] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_wins] & COPA$FTR == "D",])
  copa_home_loss[i_copa_wins] <- nrow(COPA[COPA$home_team == copa_teams[i_copa_wins] & COPA$FTR == "A",])
  copa_away_loss[i_copa_wins] <- nrow(COPA[COPA$away_team == copa_teams[i_copa_wins] & COPA$FTR == "H",])

}

copa_total_wins <- copa_home_wins + copa_away_wins
copa_total_draws <- copa_home_draws + copa_away_draws
copa_total_loss <- copa_home_loss + copa_away_loss

copa_league_table <- cbind(copa_teams,copa_games_played,copa_total_wins,copa_total_draws,copa_total_loss)
copa_GS <- copa_scoring$TGS
copa_GC <-copa_conceding$TGC
copa_GD <- copa_scoring$TGS - copa_conceding$TGC
copa_PTS <- (copa_total_wins*3) + (copa_total_draws*1)
copa_league_table <- cbind(copa_league_table,copa_GS,copa_GC,copa_GD,copa_PTS)
copa_league_table <- as.data.frame(copa_league_table)
#rename the columns
names(copa_league_table)[names(copa_league_table) == "copa_teams"] <- "Team"
names(copa_league_table)[names(copa_league_table) == "copa_games_played"] <- "P"
names(copa_league_table)[names(copa_league_table) == "copa_total_wins"] <- "W"
names(copa_league_table)[names(copa_league_table) == "copa_total_draws"] <- "D"
names(copa_league_table)[names(copa_league_table) == "copa_total_loss"] <- "L"
names(copa_league_table)[names(copa_league_table) == "copa_GS"] <- "F"
names(copa_league_table)[names(copa_league_table) == "copa_GC"] <- "A"
points_copa <- copa_league_table[order(copa_league_table$copa_PTS, decreasing = TRUE),]
write.xlsx(points_copa,'COPA.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six copa###################################################
#COPA
#form
#create final_copa_hf object
copa_last_n_games <- 6
final_copa_hf <- c()
for(index_copa_hf in 1:length(copa_teams))
{
  index_copa_hf <- row.names(copa_form_h) == copa_teams[index_copa_hf]
  form_copa_hf <- copa_form_h[index_copa_hf]
  deleted_form_copa_hf <- form_copa_hf[!form_copa_hf[] == ""]
  l6_form_copa_hf <- tail(deleted_form_copa_hf,copa_last_n_games)
  l6_form_copa_hf <- paste(l6_form_copa_hf,collapse = " ")
  final_copa_hf[index_copa_hf] <- rbind(paste(copa_teams[index_copa_hf],l6_form_copa_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",copa_teams[index],l6_form)

}

#change column names
final_copa_hf <- as.data.frame(final_copa_hf)
colnames(final_copa_hf) <- "Form"
#goals scored
#create final_copa_gs object
final_copa_gs <- c()
suml6_copa_gs <- c()
for(index_copa_gs in 1:length(copa_teams))
{
  index_copa_gs <- row.names(copa_goalscored_h) == copa_teams[index_copa_gs]
  form_copa_gs <- copa_goalscored_h[index_copa_gs]
  deleted_form_copa_gs <- form_copa_gs[!form_copa_gs[] == ""]
  l6_form_copa_gs <- tail(deleted_form_copa_gs,copa_last_n_games)
  l6_form_copa_gs <- as.numeric(l6_form_copa_gs)
  suml6_copa_gs[index_copa_gs] <- sum(l6_form_copa_gs)
  suml6_copa_gs[index_copa_gs] <- paste("(",suml6_copa_gs[index_copa_gs],")",sep = "")
  l6_form_copa_gs <- paste(l6_form_copa_gs,collapse = " ")
  final_copa_gs[index_copa_gs] <- rbind(paste(copa_teams[index_copa_gs],l6_form_copa_gs,suml6_copa_gs[index_copa_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",copa_teams[index],l6_form)

}
final_copa_gs
#change column names
final_copa_gs <- as.data.frame(final_copa_gs)
colnames(final_copa_gs) <- "Goals scored"
#goal conceded
#create final_copa_gc object
final_copa_gc <- c()
suml6_copa_gc <- c()
for(index_copa_gc in 1:length(copa_teams))
{
  index_copa_gc <- row.names(copa_goalconceded_h) == copa_teams[index_copa_gc]
  form_copa_gc <- copa_goalconceded_h[index_copa_gc]
  deleted_form_copa_gc <- form_copa_gc[!form_copa_gc[] == ""]
  l6_form_copa_gc <- tail(deleted_form_copa_gc,copa_last_n_games)
  l6_form_copa_gc <- as.numeric(l6_form_copa_gc)
  suml6_copa_gc[index_copa_gc] <- sum(l6_form_copa_gc)
  suml6_copa_gc[index_copa_gc] <- paste("(",suml6_copa_gc[index_copa_gc],")",sep = "")
  l6_form_copa_gc <- paste(l6_form_copa_gc,collapse = " ")
  final_copa_gc[index_copa_gc] <- rbind(paste(copa_teams[index_copa_gc],l6_form_copa_gc,suml6_copa_gc[index_copa_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",copa_teams[index],l6_form)

}

#change column names
final_copa_gc <- as.data.frame(final_copa_gc)
colnames(final_copa_gc) <- "Goals conceded"
#total goals
#create final_copa_tg object
final_copa_tg <- c()
suml6_copa_tg <- c()
for(index_copa_tg in 1:length(copa_teams))
{
  index_copa_tg <- row.names(copa_totalgoals_h) == copa_teams[index_copa_tg]
  form_copa_tg <- copa_totalgoals_h[index_copa_tg]
  deleted_form_copa_tg <- form_copa_tg[!form_copa_tg[] == ""]
  l6_form_copa_tg <- tail(deleted_form_copa_tg,copa_last_n_games)
  l6_form_copa_tg <- as.numeric(l6_form_copa_tg)
  suml6_copa_tg[index_copa_tg] <- sum(l6_form_copa_tg)
  suml6_copa_tg[index_copa_tg] <- paste("(",suml6_copa_tg[index_copa_tg],")",sep = "")
  l6_form_copa_tg <- paste(l6_form_copa_tg,collapse = " ")
  final_copa_tg[index_copa_tg] <- rbind(paste(copa_teams[index_copa_tg],l6_form_copa_tg,suml6_copa_tg[index_copa_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",copa_teams[index],l6_form)

}
#change column names
final_copa_tg <- as.data.frame(final_copa_tg)
colnames(final_copa_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_copa_hf object
final_copa_cs <- c()
for(index_copa_cs in 1:length(copa_teams))
{
  index_copa_cs <- row.names(copa_csform_h) == copa_teams[index_copa_cs]
  csform_copa_cs <- copa_csform_h[index_copa_cs]
  deleted_csform_copa_cs <- csform_copa_cs[!csform_copa_cs[] == ""]
  l6_csform_copa_cs <- tail(deleted_csform_copa_cs,copa_last_n_games)
  l6_csform_copa_cs <- paste(l6_csform_copa_cs,collapse = " ")
  final_copa_cs[index_copa_cs] <- rbind(paste(copa_teams[index_copa_cs],l6_csform_copa_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",copa_teams[index],l6_csform)

}

#change column names
final_copa_cs <- as.data.frame(final_copa_cs)
colnames(final_copa_cs) <- "CSForm"
#################################################
#Team against
#create final_copa_hf_against
final_copa_hf_against <- c()
for(index_copa_hf_against in 1:length(copa_teams))
{
  index_copa_hf_against <- row.names(copa_form_team_against_h) == copa_teams[index_copa_hf_against]
  form_copa_hf_against <- copa_form_team_against_h[index_copa_hf_against]
  deleted_form_copa_hf_against <- form_copa_hf_against[!form_copa_hf_against[] == ""]
  l6_form_copa_hf_against <- tail(deleted_form_copa_hf_against,copa_last_n_games)
  l6_form_copa_hf_against <- paste(l6_form_copa_hf_against,collapse = " ")
  final_copa_hf_against[index_copa_hf_against] <- rbind(paste(copa_teams[index_copa_hf_against],l6_form_copa_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",copa_teams[index],l6_form)

}
final_copa_hf_against <- as.data.frame(final_copa_hf_against)
colnames(final_copa_hf_against) <- "Team against"
#combine the columns
final_copa_all <- cbind(final_copa_hf,final_copa_gs,final_copa_gc,final_copa_tg,final_copa_cs,final_copa_hf_against)
write.xlsx(final_copa_all,'COPA.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
copa_GP <- nrow(COPA)
#Calculate total home goals for each division
copa_T_home_score <- sum(copa_home_gs$x)
#calculate average home goal
copa_avg_home_score <- round(copa_T_home_score /copa_GP, digits = 4)
############################################################
#Calculate total away goals for each division
copa_T_away_score <- sum(copa_away_gs$x)
#calculate average away goal
copa_avg_away_score <- round(copa_T_away_score /copa_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
copa_home_as <- round(((copa_home_gs$x/copa_home_games))/copa_avg_home_score, digits = 4)
#calculate away attack strength
copa_away_as <- round(((copa_away_gs$x/copa_away_games))/copa_avg_away_score, digits = 4)
################################################################################
#get average home concede and away concede
copa_avg_HC <- round(copa_T_away_score /copa_GP, digits = 4)
#avg away concede
copa_avg_AC <- round(copa_T_home_score /copa_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
copa_home_ds <- round(((copa_home_gc$x/copa_home_games))/copa_avg_HC, digits = 4)
#away defense strength
copa_away_ds <- round(((copa_away_gc$x/copa_away_games))/copa_avg_AC, digits = 4)
#############################################################################
#home poisson data
#copa
copa_division <- c()
copa_division[1:length(copa_teams)] <- "COPA"
copa_home_poisson <- cbind(copa_division,copa_teams,copa_avg_home_score,copa_home_as,copa_home_ds)
#################################################################################
#away poisson data
#copa
copa_division <- c()
copa_division[1:length(copa_teams)] <- "COPA"
copa_away_poisson <- cbind(copa_division,copa_teams,copa_avg_away_score,copa_away_as,copa_away_ds)

#create home and away csv
#copa_home_poisson <- rbind(copa_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#copa_away_poisson <- rbind(copa_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(copa_home_poisson,'COPA.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(copa_away_poisson,'COPA.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################COPA FIXTURES##########################################################################
#COPA
home_teamTeam_copa <- rep(copa_teams, each = length(copa_teams))
away_teamTeam_copa <- rep(copa_teams, length(copa_teams))
COPA_fixtures <- cbind(home_teamTeam_copa,away_teamTeam_copa)
COPA_fixtures <- as.data.frame(COPA_fixtures)
COPA_fixtures <- COPA_fixtures[!COPA_fixtures$home_teamTeam_copa == COPA_fixtures$away_teamTeam_copa,]
rownames(COPA_fixtures) <- NULL
COPA_fixtures$Div <- "COPA"
COPA_fixtures <- COPA_fixtures[,c(3,1,2)]

COPA_fixtures$avg_home_score_copa <- copa_avg_home_score

COPA_fixtures$copa_homeas <- rep(copa_home_as,each = length(copa_teams)-1)

copa_awayds_lookup <- cbind(copa_teams,copa_away_ds)

copa_awayds_lookup <- as.data.frame(copa_awayds_lookup)

colnames(copa_awayds_lookup) <- c("away_teamTeam_copa","copa_awayds")


require('RH2')
COPA_fixtures$copa_awayds <- sqldf("SELECT copa_awayds_lookup.copa_awayds FROM copa_awayds_lookup INNER JOIN COPA_fixtures ON copa_awayds_lookup.away_teamTeam_copa = COPA_fixtures.away_teamTeam_copa")

COPA_fixtures$avg_away_score_copa <- copa_avg_away_score

copa_awayas_lookup <- cbind(copa_teams,copa_away_as)

copa_awayas_lookup <- as.data.frame(copa_awayas_lookup)

colnames(copa_awayas_lookup) <- c("away_teamTeam_copa","copa_awayas")


COPA_fixtures$copa_awayas <- sqldf("SELECT copa_awayas_lookup.copa_awayas FROM copa_awayas_lookup INNER JOIN COPA_fixtures ON copa_awayas_lookup.away_teamTeam_copa = COPA_fixtures.away_teamTeam_copa")

COPA_fixtures$copa_homeds <- rep(copa_home_ds,each = length(copa_teams)-1)

COPA_fixtures$copa_awayds <- as.numeric(unlist(COPA_fixtures$copa_awayds))
#xGH
COPA_fixtures$copa_xGH <- COPA_fixtures$avg_home_score_copa * COPA_fixtures$copa_homeas * COPA_fixtures$copa_awayds

#xGA

COPA_fixtures$copa_awayas <- as.numeric(unlist(COPA_fixtures$copa_awayas))

COPA_fixtures$copa_xGA <- COPA_fixtures$avg_away_score_copa * COPA_fixtures$copa_awayas * COPA_fixtures$copa_homeds

COPA_fixtures$copa_0_0 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_0 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_0_1 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_1 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_0 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_0_2 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_2 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_1 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_2 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_3 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_0 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_1 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_2 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_0_3 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_3 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_3 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_4 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_0 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_1 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_2 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_3 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_0_4 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_4 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_4 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_4 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_5 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_0 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_1 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_2 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_3 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_4 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_0_5 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_5 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_5 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_5 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_5 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_6 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_0 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(0,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_1 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(1,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_2 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(2,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_3 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(3,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_4 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(4,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_6_5 <- round(stats::dpois(6,COPA_fixtures$copa_xGH) * stats::dpois(5,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_0_6 <- round(stats::dpois(0,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_1_6 <- round(stats::dpois(1,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_2_6 <- round(stats::dpois(2,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_3_6 <- round(stats::dpois(3,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_4_6 <- round(stats::dpois(4,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
COPA_fixtures$copa_5_6 <- round(stats::dpois(5,COPA_fixtures$copa_xGH) * stats::dpois(6,COPA_fixtures$copa_xGA), digits = 4)
#home_team win
COPA_fixtures$copa_H <- (
  COPA_fixtures$copa_1_0 + COPA_fixtures$copa_2_0 + COPA_fixtures$copa_2_1 + COPA_fixtures$copa_3_0 + COPA_fixtures$copa_3_1 +
    COPA_fixtures$copa_3_2 + COPA_fixtures$copa_4_0 + COPA_fixtures$copa_4_1 + COPA_fixtures$copa_4_2 + COPA_fixtures$copa_4_3 +
    COPA_fixtures$copa_5_0 + COPA_fixtures$copa_5_1 + COPA_fixtures$copa_5_2 + COPA_fixtures$copa_5_3 + COPA_fixtures$copa_5_4 +
    COPA_fixtures$copa_6_0 + COPA_fixtures$copa_6_1 + COPA_fixtures$copa_6_2 + COPA_fixtures$copa_6_3 + COPA_fixtures$copa_6_4 +
    COPA_fixtures$copa_6_5
)

COPA_fixtures$copa_H <- percent(COPA_fixtures$copa_H, accuracy = 0.1)

#Draw
COPA_fixtures$copa_D <- (

  COPA_fixtures$copa_0_0 + COPA_fixtures$copa_1_1 + COPA_fixtures$copa_2_2 + COPA_fixtures$copa_3_3 + COPA_fixtures$copa_4_4 +
    COPA_fixtures$copa_5_5 + COPA_fixtures$copa_6_6
)

COPA_fixtures$copa_D <- percent(COPA_fixtures$copa_D, accuracy = 0.1)

#away_team

COPA_fixtures$copa_A <- (
  COPA_fixtures$copa_0_1 + COPA_fixtures$copa_0_2 + COPA_fixtures$copa_1_2 + COPA_fixtures$copa_0_3 + COPA_fixtures$copa_1_3 +
    COPA_fixtures$copa_2_3 + COPA_fixtures$copa_0_4 + COPA_fixtures$copa_1_4 + COPA_fixtures$copa_2_4 + COPA_fixtures$copa_3_4 +
    COPA_fixtures$copa_0_5 + COPA_fixtures$copa_1_5 + COPA_fixtures$copa_2_5 + COPA_fixtures$copa_3_5 + COPA_fixtures$copa_4_5 +
    COPA_fixtures$copa_0_6 + COPA_fixtures$copa_1_6 + COPA_fixtures$copa_2_6 + COPA_fixtures$copa_3_6 + COPA_fixtures$copa_4_6 +
    COPA_fixtures$copa_5_6
)

COPA_fixtures$copa_A <- percent(COPA_fixtures$copa_A, accuracy = 0.1)

#ov25
COPA_fixtures$copa_ov25 <- (
  COPA_fixtures$copa_2_1 + COPA_fixtures$copa_1_2 + COPA_fixtures$copa_2_2 + COPA_fixtures$copa_3_0 + COPA_fixtures$copa_3_1 +
    COPA_fixtures$copa_3_2 + COPA_fixtures$copa_0_3 + COPA_fixtures$copa_1_3 + COPA_fixtures$copa_2_3 + COPA_fixtures$copa_3_3 +
    COPA_fixtures$copa_4_0 + COPA_fixtures$copa_4_1 + COPA_fixtures$copa_4_2 + COPA_fixtures$copa_4_3 + COPA_fixtures$copa_0_4 +
    COPA_fixtures$copa_1_4 + COPA_fixtures$copa_2_4 + COPA_fixtures$copa_3_4 + COPA_fixtures$copa_4_4 + COPA_fixtures$copa_5_0 +
    COPA_fixtures$copa_5_1 + COPA_fixtures$copa_5_2 + COPA_fixtures$copa_5_3 + COPA_fixtures$copa_5_4 + COPA_fixtures$copa_0_5 +
    COPA_fixtures$copa_1_5 + COPA_fixtures$copa_2_5 + COPA_fixtures$copa_3_5 + COPA_fixtures$copa_4_5 + COPA_fixtures$copa_5_5 +
    COPA_fixtures$copa_6_0 + COPA_fixtures$copa_6_1 + COPA_fixtures$copa_6_2 + COPA_fixtures$copa_6_3 + COPA_fixtures$copa_6_4 +
    COPA_fixtures$copa_6_5 + COPA_fixtures$copa_0_6 + COPA_fixtures$copa_1_6 + COPA_fixtures$copa_2_6 + COPA_fixtures$copa_3_6 +
    COPA_fixtures$copa_4_6 + COPA_fixtures$copa_5_6 + COPA_fixtures$copa_6_6
)
#un25
COPA_fixtures$copa_un25 <- (
  COPA_fixtures$copa_0_0 + COPA_fixtures$copa_1_0 + COPA_fixtures$copa_0_1 + COPA_fixtures$copa_1_1 + COPA_fixtures$copa_2_0 + COPA_fixtures$copa_0_2
)
#odds
COPA_fixtures$copa_ov25_odds <- round((1/COPA_fixtures$copa_ov25),digits = 2)
COPA_fixtures$copa_un25_odds <- round((1/COPA_fixtures$copa_un25),digits = 2)

COPA_fixtures$copa_ov25_odds
COPA_fixtures$copa_un25_odds
#percentages
COPA_fixtures$copa_ov25 <- percent(COPA_fixtures$copa_ov25, accuracy = 0.1)

COPA_fixtures$copa_un25 <- percent(COPA_fixtures$copa_un25, accuracy = 0.1)
COPA_fixtures$copa_pscore <- paste(round(COPA_fixtures$copa_xGH,digits = 0),round(COPA_fixtures$copa_xGA,digits = 0),sep = "-")
#write out
write.xlsx(COPA_fixtures,'COPA.xlsx',sheetName = "COPA", append = TRUE)
###########################################################################################################
########################COPA END###########################################################################
# COPA <- read.csv('../../../Leonard/Downloads/results.csv')
# COPA$FTR <- with(COPA,
#                  ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
# )
# COPA$TG <- COPA$home_score + COPA$away_score
# COPA$OV25 <- ifelse(COPA$TG >= 3,"Y","N")
# copa_ftr_summary <- tabyl(COPA,tournament,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
# copa_ov25_summary <- tabyl(COPA,tournament,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
# ftr_summary <- ftr_summary[,c(1,3,2)]
# write.xlsx(copa_ftr_summary,'COPA.xlsx',sheetName = "FTR", append = TRUE)
# write.xlsx(copa_ov25_summary,'COPA.xlsx',sheetName = "OVUN25", append = TR


