library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
options(java.parameters = "-Xmx2048m")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
#delete current file
unlink('UEFANL.xlsx')
######################UEFANL START#######################################
#####################################################################
UEFANL <- read.csv('../../../Leonard/Downloads/results.csv')
library('lubridate')
UEFANL$date <- ymd(UEFANL$date)
UEFANL <- UEFANL[order(as.Date(UEFANL$date, format = "%Y/%m%d"), decreasing = FALSE),]
#UEFANL_qualificaton <- subset(UEFANL,tournament == "Friendly")
UEFANL <- subset(UEFANL,tournament == "UEFA Nations League")
UEFANL <- UEFANL[UEFANL$date > '2020-01-01',]
UEFANL$TG <- UEFANL$home_score + UEFANL$away_score
UEFANL$OV25 <- ifelse(UEFANL$TG >= 3,"Y","N")
UEFANL$FTR <- with(UEFANL,
                       ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
uefanl_totalgoalsv2 <- tapply(UEFANL$TG, UEFANL[c("home_team", "away_team")],mean)
uefanl_hgtotals <- rowSums(uefanl_totalgoalsv2, na.rm = T)
uefanl_agtotals <- colSums(uefanl_totalgoalsv2, na.rm = T)
uefanl_totalgoals <- uefanl_hgtotals + uefanl_agtotals
uefanl_totalgoalsv2 <- cbind(uefanl_totalgoalsv2,uefanl_totalgoals)
uefanl_teams <- sort(unique(UEFANL$home_team))
uefanl_home_games <- c()
uefanl_away_games <-c()
for (i_uefanl in 1:length(uefanl_teams))
{

  uefanl_home_games[i_uefanl] <- nrow(UEFANL[UEFANL$home_team == uefanl_teams[i_uefanl],])
  uefanl_away_games[i_uefanl]  <- nrow(UEFANL[UEFANL$away_team == uefanl_teams[i_uefanl],])

}
uefanl_games_played <- uefanl_home_games + uefanl_away_games
uefanl_goaltotalsv2 <- cbind(uefanl_totalgoalsv2,uefanl_games_played)
uefanl_avg_totalgoals <- round((uefanl_totalgoals/ uefanl_games_played), digits = 4)
uefanl_goaltotalsv2[is.na(uefanl_goaltotalsv2)] <- ""
uefanl_goaltotalsv2 <- cbind(uefanl_goaltotalsv2,uefanl_avg_totalgoals)
write.xlsx(uefanl_goaltotalsv2,'UEFANL.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
uefanl_goalscored_h <- tapply(UEFANL$home_score, UEFANL[c("home_team", "date")],mean)
uefanl_goalscored_a <- tapply(UEFANL$away_score, UEFANL[c("away_team", "date")],mean)
uefanl_goalscored_h[is.na(uefanl_goalscored_h)] <- ""
uefanl_goalscored_a[is.na(uefanl_goalscored_a)] <- ""

for(uefanl_rowhgs in 1:nrow(uefanl_goalscored_h)) {
  for(uefanl_colhgs in 1:ncol(uefanl_goalscored_h)) {

    # print(my_matrix[row, col])
    for(uefanl_rowags in 1:nrow(uefanl_goalscored_a)) {
      for(uefanl_colags in 1:ncol(uefanl_goalscored_a)) {
        ifelse(!uefanl_goalscored_a[uefanl_rowags,uefanl_colags]=="",uefanl_goalscored_h[uefanl_rowags,uefanl_colags] <- uefanl_goalscored_a[uefanl_rowags,uefanl_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(uefanl_goalscored_h,'UEFANL.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
uefanl_goalconceded_h <- tapply(UEFANL$away_score, UEFANL[c("home_team", "date")],mean)
uefanl_goalconceded_a <- tapply(UEFANL$home_score, UEFANL[c("away_team", "date")],mean)
uefanl_goalconceded_h[is.na(uefanl_goalconceded_h)] <- ""
uefanl_goalconceded_a[is.na(uefanl_goalconceded_a)] <- ""

for(uefanl_rowhgc in 1:nrow(uefanl_goalconceded_h)) {
  for(uefanl_colhgc in 1:ncol(uefanl_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(uefanl_rowagc in 1:nrow(uefanl_goalconceded_a)) {
      for(uefanl_colagc in 1:ncol(uefanl_goalconceded_a)) {
        ifelse(!uefanl_goalconceded_a[uefanl_rowagc,uefanl_colagc]=="",uefanl_goalconceded_h[uefanl_rowagc,uefanl_colagc] <- uefanl_goalconceded_a[uefanl_rowagc,uefanl_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(uefanl_goalconceded_h,'UEFANL.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
uefanl_form_h <- tapply(UEFANL$FTR, UEFANL[c("home_team", "date")],median)
uefanl_form_a <- tapply(UEFANL$FTR, UEFANL[c("away_team", "date")],median)
uefanl_form_h[is.na(uefanl_form_h)] <- ""
uefanl_form_a[is.na(uefanl_form_a)] <- ""
uefanl_form_h <- sub("A","L",uefanl_form_h)
uefanl_form_h <- sub("H","W",uefanl_form_h)
uefanl_form_a <- sub("A","W",uefanl_form_a)
uefanl_form_a <- sub("H","L",uefanl_form_a)
for(uefanl_rowh_f in 1:nrow(uefanl_form_h)) {
  for(uefanl_colh_f in 1:ncol(uefanl_form_h)) {

    # print(my_matrix[row, col])
    for(uefanl_rowa_f in 1:nrow(uefanl_form_a)) {
      for(uefanl_cola_f in 1:ncol(uefanl_form_a)) {
        ifelse(!uefanl_form_a[uefanl_rowa_f,uefanl_cola_f]=="",uefanl_form_h[uefanl_rowa_f,uefanl_cola_f] <- uefanl_form_a[uefanl_rowa_f,uefanl_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(uefanl_form_h,'UEFANL.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
uefanl_totalgoals_h <- tapply(UEFANL$TG, UEFANL[c("home_team", "date")],mean)
uefanl_totalgoals_a <- tapply(UEFANL$TG, UEFANL[c("away_team", "date")],mean)
uefanl_totalgoals_h[is.na(uefanl_totalgoals_h)] <- ""
uefanl_totalgoals_a[is.na(uefanl_totalgoals_a)] <- ""
for(uefanl_rowh in 1:nrow(uefanl_totalgoals_h)) {
  for(uefanl_colh in 1:ncol(uefanl_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(uefanl_rowa in 1:nrow(uefanl_totalgoals_a)) {
      for(uefanl_cola in 1:ncol(uefanl_totalgoals_a)) {
        ifelse(!uefanl_totalgoals_a[uefanl_rowa,uefanl_cola]=="",uefanl_totalgoals_h[uefanl_rowa,uefanl_cola] <- uefanl_totalgoals_a[uefanl_rowa,uefanl_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(uefanl_totalgoals_h,'UEFANL.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
uefanl_form_team_against_h <- tapply(UEFANL$away_team, UEFANL[c("home_team", "date")],median)
uefanl_form_team_against_a <- tapply(UEFANL$home_team, UEFANL[c("away_team", "date")],median)
uefanl_form_team_against_h[is.na(uefanl_form_team_against_h)] <- ""
uefanl_form_team_against_a[is.na(uefanl_form_team_against_a)] <- ""
for(uefanl_rowh_f_against in 1:nrow(uefanl_form_team_against_h)) {
  for(uefanl_colh_f_against in 1:ncol(uefanl_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(uefanl_rowa_f_against in 1:nrow(uefanl_form_team_against_a)) {
      for(uefanl_cola_f_against in 1:ncol(uefanl_form_team_against_a)) {
        ifelse(!uefanl_form_team_against_a[uefanl_rowa_f_against,uefanl_cola_f_against]=="",uefanl_form_team_against_h[uefanl_rowa_f_against,uefanl_cola_f_against] <- uefanl_form_team_against_a[uefanl_rowa_f_against,uefanl_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

###########################################################################################
############Scoring and conceding analysis
#home goals scored
uefanl_home_gs <- aggregate(UEFANL$home_score, by = list(UEFANL$home_team), FUN = sum)
uefanl_home_gs_avg <- aggregate(UEFANL$home_score, by = list(UEFANL$home_team),mean)
uefanl_home_scoring <- merge(uefanl_home_gs,uefanl_home_gs_avg, by='Group.1',all = T)
names(uefanl_home_scoring)[names(uefanl_home_scoring) == "x.x"] <- "TFthg"
names(uefanl_home_scoring)[names(uefanl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
uefanl_away_gs <- aggregate(UEFANL$away_score, by = list(UEFANL$away_team), FUN = sum)
uefanl_away_gs_avg <- aggregate(UEFANL$away_score, by = list(UEFANL$away_team),mean)
uefanl_away_scoring <- merge(uefanl_away_gs,uefanl_away_gs_avg, by='Group.1',all = T)
names(uefanl_away_scoring)[names(uefanl_away_scoring) == "x.x"] <- "TFtag"
names(uefanl_away_scoring)[names(uefanl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
uefanl_scoring <- merge(uefanl_home_scoring,uefanl_away_scoring,by='Group.1',all = T)
uefanl_scoring$TGS <- uefanl_scoring$TFthg + uefanl_scoring$TFtag

#home goals conceded
uefanl_home_gc <- aggregate(UEFANL$away_score, by = list(UEFANL$home_team), FUN = sum)
uefanl_home_gc_avg <- aggregate(UEFANL$away_score, by = list(UEFANL$home_team),mean)
uefanl_home_conceding <- merge(uefanl_home_gc,uefanl_home_gc_avg, by='Group.1',all = T)
names(uefanl_home_conceding)[names(uefanl_home_conceding) == "x.x"] <- "TFthc"
names(uefanl_home_conceding)[names(uefanl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
uefanl_away_gc <- aggregate(UEFANL$home_score, by = list(UEFANL$away_team), FUN = sum)
uefanl_away_gc_avg <- aggregate(UEFANL$home_score, by = list(UEFANL$away_team),mean)
uefanl_away_conceding <- merge(uefanl_away_gc,uefanl_away_gc_avg, by='Group.1',all = T)
names(uefanl_away_conceding)[names(uefanl_away_conceding) == "x.x"] <- "TFtac"
names(uefanl_away_conceding)[names(uefanl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
uefanl_conceding <- merge(uefanl_home_conceding,uefanl_away_conceding,by='Group.1',all = T)
uefanl_conceding$TGC <- uefanl_conceding$TFthc + uefanl_conceding$TFtac

uefanl_shots_analysis <- merge(uefanl_scoring_conversion,uefanl_conceding_conversion,by='Group.1',all = T)

######################################################################################
###########League Table###############################################################

#hwins and away wins
uefanl_home_wins <- c()
uefanl_away_wins <- c()
uefanl_home_draws <- c()
uefanl_away_draws <- c()
uefanl_home_loss <- c()
uefanl_away_loss <- c()



for (i_uefanl_wins in 1:length(uefanl_teams))
{

  uefanl_home_wins[i_uefanl_wins] <- nrow(UEFANL[UEFANL$home_team == uefanl_teams[i_uefanl_wins] & UEFANL$FTR == "H",])
  uefanl_away_wins[i_uefanl_wins] <- nrow(UEFANL[UEFANL$away_team == uefanl_teams[i_uefanl_wins] & UEFANL$FTR == "A",])
  uefanl_home_draws[i_uefanl_wins] <- nrow(UEFANL[UEFANL$home_team == uefanl_teams[i_uefanl_wins] & UEFANL$FTR == "D",])
  uefanl_away_draws[i_uefanl_wins] <- nrow(UEFANL[UEFANL$away_team == uefanl_teams[i_uefanl_wins] & UEFANL$FTR == "D",])
  uefanl_home_loss[i_uefanl_wins] <- nrow(UEFANL[UEFANL$home_team == uefanl_teams[i_uefanl_wins] & UEFANL$FTR == "A",])
  uefanl_away_loss[i_uefanl_wins] <- nrow(UEFANL[UEFANL$away_team == uefanl_teams[i_uefanl_wins] & UEFANL$FTR == "H",])

}

uefanl_total_wins <- uefanl_home_wins + uefanl_away_wins
uefanl_total_draws <- uefanl_home_draws + uefanl_away_draws
uefanl_total_loss <- uefanl_home_loss + uefanl_away_loss

uefanl_league_table <- cbind(uefanl_teams,uefanl_games_played,uefanl_total_wins,uefanl_total_draws,uefanl_total_loss)
uefanl_GS <- uefanl_scoring$TGS
uefanl_GC <-uefanl_conceding$TGC
uefanl_GD <- uefanl_scoring$TGS - uefanl_conceding$TGC
uefanl_PTS <- (uefanl_total_wins*3) + (uefanl_total_draws*1)
uefanl_league_table <- cbind(uefanl_league_table,uefanl_GS,uefanl_GC,uefanl_GD,uefanl_PTS)
uefanl_league_table <- as.data.frame(uefanl_league_table)
#rename the columns
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_teams"] <- "Team"
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_games_played"] <- "P"
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_total_wins"] <- "W"
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_total_draws"] <- "D"
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_total_loss"] <- "L"
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_GS"] <- "F"
names(uefanl_league_table)[names(uefanl_league_table) == "uefanl_GC"] <- "A"
points_uefanl <- uefanl_league_table[order(uefanl_league_table$uefanl_PTS, decreasing = TRUE),]
write.xlsx(points_uefanl,'UEFANL.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six uefanl###################################################
#UEFANL
#form
#create final_uefanl_hf object
final_uefanl_hf <- c()
for(index_uefanl_hf in 1:length(uefanl_teams))
{
  index_uefanl_hf <- row.names(uefanl_form_h) == uefanl_teams[index_uefanl_hf]
  form_uefanl_hf <- uefanl_form_h[index_uefanl_hf]
  deleted_form_uefanl_hf <- form_uefanl_hf[!form_uefanl_hf[] == ""]
  l6_form_uefanl_hf <- tail(deleted_form_uefanl_hf,6)
  l6_form_uefanl_hf <- paste(l6_form_uefanl_hf,collapse = " ")
  final_uefanl_hf[index_uefanl_hf] <- rbind(paste(uefanl_teams[index_uefanl_hf],l6_form_uefanl_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefanl_teams[index],l6_form)

}

#change column names
final_uefanl_hf <- as.data.frame(final_uefanl_hf)
colnames(final_uefanl_hf) <- "Form"
#goals scored
#create final_uefanl_gs object
final_uefanl_gs <- c()
suml6_uefanl_gs <- c()
for(index_uefanl_gs in 1:length(uefanl_teams))
{
  index_uefanl_gs <- row.names(uefanl_goalscored_h) == uefanl_teams[index_uefanl_gs]
  form_uefanl_gs <- uefanl_goalscored_h[index_uefanl_gs]
  deleted_form_uefanl_gs <- form_uefanl_gs[!form_uefanl_gs[] == ""]
  l6_form_uefanl_gs <- tail(deleted_form_uefanl_gs,6)
  l6_form_uefanl_gs <- as.numeric(l6_form_uefanl_gs)
  suml6_uefanl_gs[index_uefanl_gs] <- sum(l6_form_uefanl_gs)
  suml6_uefanl_gs[index_uefanl_gs] <- paste("(",suml6_uefanl_gs[index_uefanl_gs],")",sep = "")
  l6_form_uefanl_gs <- paste(l6_form_uefanl_gs,collapse = " ")
  final_uefanl_gs[index_uefanl_gs] <- rbind(paste(uefanl_teams[index_uefanl_gs],l6_form_uefanl_gs,suml6_uefanl_gs[index_uefanl_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefanl_teams[index],l6_form)

}
final_uefanl_gs
#change column names
final_uefanl_gs <- as.data.frame(final_uefanl_gs)
colnames(final_uefanl_gs) <- "Goals scored"
#goal conceded
#create final_uefanl_gc object
final_uefanl_gc <- c()
suml6_uefanl_gc <- c()
for(index_uefanl_gc in 1:length(uefanl_teams))
{
  index_uefanl_gc <- row.names(uefanl_goalconceded_h) == uefanl_teams[index_uefanl_gc]
  form_uefanl_gc <- uefanl_goalconceded_h[index_uefanl_gc]
  deleted_form_uefanl_gc <- form_uefanl_gc[!form_uefanl_gc[] == ""]
  l6_form_uefanl_gc <- tail(deleted_form_uefanl_gc,6)
  l6_form_uefanl_gc <- as.numeric(l6_form_uefanl_gc)
  suml6_uefanl_gc[index_uefanl_gc] <- sum(l6_form_uefanl_gc)
  suml6_uefanl_gc[index_uefanl_gc] <- paste("(",suml6_uefanl_gc[index_uefanl_gc],")",sep = "")
  l6_form_uefanl_gc <- paste(l6_form_uefanl_gc,collapse = " ")
  final_uefanl_gc[index_uefanl_gc] <- rbind(paste(uefanl_teams[index_uefanl_gc],l6_form_uefanl_gc,suml6_uefanl_gc[index_uefanl_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefanl_teams[index],l6_form)

}

#change column names
final_uefanl_gc <- as.data.frame(final_uefanl_gc)
colnames(final_uefanl_gc) <- "Goals conceded"
#total goals
#create final_uefanl_tg object
final_uefanl_tg <- c()
suml6_uefanl_tg <- c()
for(index_uefanl_tg in 1:length(uefanl_teams))
{
  index_uefanl_tg <- row.names(uefanl_totalgoals_h) == uefanl_teams[index_uefanl_tg]
  form_uefanl_tg <- uefanl_totalgoals_h[index_uefanl_tg]
  deleted_form_uefanl_tg <- form_uefanl_tg[!form_uefanl_tg[] == ""]
  l6_form_uefanl_tg <- tail(deleted_form_uefanl_tg,6)
  l6_form_uefanl_tg <- as.numeric(l6_form_uefanl_tg)
  suml6_uefanl_tg[index_uefanl_tg] <- sum(l6_form_uefanl_tg)
  suml6_uefanl_tg[index_uefanl_tg] <- paste("(",suml6_uefanl_tg[index_uefanl_tg],")",sep = "")
  l6_form_uefanl_tg <- paste(l6_form_uefanl_tg,collapse = " ")
  final_uefanl_tg[index_uefanl_tg] <- rbind(paste(uefanl_teams[index_uefanl_tg],l6_form_uefanl_tg,suml6_uefanl_tg[index_uefanl_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefanl_teams[index],l6_form)

}
#change column names
final_uefanl_tg <- as.data.frame(final_uefanl_tg)
colnames(final_uefanl_tg) <- "Total Goals"
#Team against
#create final_uefanl_hf_against
final_uefanl_hf_against <- c()
for(index_uefanl_hf_against in 1:length(uefanl_teams))
{
  index_uefanl_hf_against <- row.names(uefanl_form_team_against_h) == uefanl_teams[index_uefanl_hf_against]
  form_uefanl_hf_against <- uefanl_form_team_against_h[index_uefanl_hf_against]
  deleted_form_uefanl_hf_against <- form_uefanl_hf_against[!form_uefanl_hf_against[] == ""]
  l6_form_uefanl_hf_against <- tail(deleted_form_uefanl_hf_against,6)
  l6_form_uefanl_hf_against <- paste(l6_form_uefanl_hf_against,collapse = " ")
  final_uefanl_hf_against[index_uefanl_hf_against] <- rbind(paste(uefanl_teams[index_uefanl_hf_against],l6_form_uefanl_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",uefanl_teams[index],l6_form)

}
final_uefanl_hf_against <- as.data.frame(final_uefanl_hf_against)
colnames(final_uefanl_hf_against) <- "Team against"
#combine the columns
final_uefanl_all <- cbind(final_uefanl_hf,final_uefanl_gs,final_uefanl_gc,final_uefanl_tg,final_uefanl_hf_against)
write.xlsx(final_uefanl_all,'UEFANL.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
uefanl_GP <- nrow(UEFANL)
#Calculate total home goals for each division
uefanl_T_HG <- sum(uefanl_home_gs$x)
#calculate average home goal
uefanl_avg_HG <- round(uefanl_T_HG /uefanl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
uefanl_T_AG <- sum(uefanl_away_gs$x)
#calculate average away goal
uefanl_avg_AG <- round(uefanl_T_AG /uefanl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
uefanl_home_as <- round(((uefanl_home_gs$x/uefanl_home_games))/uefanl_avg_HG, digits = 4)
#calculate away attack strength
uefanl_away_as <- round(((uefanl_away_gs$x/uefanl_away_games))/uefanl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
uefanl_avg_HC <- round(uefanl_T_AG /uefanl_GP, digits = 4)
#avg away concede
uefanl_avg_AC <- round(uefanl_T_HG /uefanl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
uefanl_home_ds <- round(((uefanl_home_gc$x/uefanl_home_games))/uefanl_avg_HC, digits = 4)
#away defense strength
uefanl_away_ds <- round(((uefanl_away_gc$x/uefanl_away_games))/uefanl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#uefanl
uefanl_division <- c()
uefanl_division[1:length(uefanl_teams)] <- "UEFANL"
uefanl_home_poisson <- cbind(uefanl_division,uefanl_teams,uefanl_avg_HG,uefanl_home_as,uefanl_home_ds)
#################################################################################
#away poisson data
#uefanl
uefanl_division <- c()
uefanl_division[1:length(uefanl_teams)] <- "UEFANL"
uefanl_away_poisson <- cbind(uefanl_division,uefanl_teams,uefanl_avg_AG,uefanl_away_as,uefanl_away_ds)

#create home and away csv
#uefanl_home_poisson <- rbind(uefanl_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#uefanl_away_poisson <- rbind(uefanl_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(uefanl_home_poisson,'UEFANL.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(uefanl_away_poisson,'UEFANL.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################UEFANL FIXTURES##########################################################################
#UEFANL
HomeTeam_uefanl <- rep(uefanl_teams, each = length(uefanl_teams))
AwayTeam_uefanl <- rep(uefanl_teams, length(uefanl_teams))
UEFANL_fixtures <- cbind(HomeTeam_uefanl,AwayTeam_uefanl)
UEFANL_fixtures <- as.data.frame(UEFANL_fixtures)
UEFANL_fixtures <- UEFANL_fixtures[!UEFANL_fixtures$HomeTeam_uefanl == UEFANL_fixtures$AwayTeam_uefanl,]
rownames(UEFANL_fixtures) <- NULL
UEFANL_fixtures$Div <- "UEFANL"
UEFANL_fixtures <- UEFANL_fixtures[,c(3,1,2)]

UEFANL_fixtures$avg_HG_uefanl <- uefanl_avg_HG

UEFANL_fixtures$uefanl_homeas <- rep(uefanl_home_as,each = length(uefanl_teams)-1)

uefanl_awayds_lookup <- cbind(uefanl_teams,uefanl_away_ds)

uefanl_awayds_lookup <- as.data.frame(uefanl_awayds_lookup)

colnames(uefanl_awayds_lookup) <- c("AwayTeam_uefanl","uefanl_awayds")


require('RH2')
UEFANL_fixtures$uefanl_awayds <- sqldf("SELECT uefanl_awayds_lookup.uefanl_awayds FROM uefanl_awayds_lookup INNER JOIN UEFANL_fixtures ON uefanl_awayds_lookup.AwayTeam_uefanl = UEFANL_fixtures.AwayTeam_uefanl")

UEFANL_fixtures$avg_AG_uefanl <- uefanl_avg_AG

uefanl_awayas_lookup <- cbind(uefanl_teams,uefanl_away_as)

uefanl_awayas_lookup <- as.data.frame(uefanl_awayas_lookup)

colnames(uefanl_awayas_lookup) <- c("AwayTeam_uefanl","uefanl_awayas")


UEFANL_fixtures$uefanl_awayas <- sqldf("SELECT uefanl_awayas_lookup.uefanl_awayas FROM uefanl_awayas_lookup INNER JOIN UEFANL_fixtures ON uefanl_awayas_lookup.AwayTeam_uefanl = UEFANL_fixtures.AwayTeam_uefanl")

UEFANL_fixtures$uefanl_homeds <- rep(uefanl_home_ds,each = length(uefanl_teams)-1)

UEFANL_fixtures$uefanl_awayds <- as.numeric(unlist(UEFANL_fixtures$uefanl_awayds))
#xGH
UEFANL_fixtures$uefanl_xGH <- UEFANL_fixtures$avg_HG_uefanl * UEFANL_fixtures$uefanl_homeas * UEFANL_fixtures$uefanl_awayds

#xGA

UEFANL_fixtures$uefanl_awayas <- as.numeric(unlist(UEFANL_fixtures$uefanl_awayas))

UEFANL_fixtures$uefanl_xGA <- UEFANL_fixtures$avg_AG_uefanl * UEFANL_fixtures$uefanl_awayas * UEFANL_fixtures$uefanl_homeds

UEFANL_fixtures$uefanl_0_0 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_0 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_0_1 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_1 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_0 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_0_2 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_2 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_1 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_2 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_3 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_0 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_1 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_2 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_0_3 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_3 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_3 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_4 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_0 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_1 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_2 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_3 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_0_4 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_4 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_4 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_4 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_5 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_0 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_1 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_2 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_3 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_4 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_0_5 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_5 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_5 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_5 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_5 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_6 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_0 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(0,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_1 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(1,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_2 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(2,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_3 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(3,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_4 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(4,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_6_5 <- round(stats::dpois(6,UEFANL_fixtures$uefanl_xGH) * stats::dpois(5,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_0_6 <- round(stats::dpois(0,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_1_6 <- round(stats::dpois(1,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_2_6 <- round(stats::dpois(2,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_3_6 <- round(stats::dpois(3,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_4_6 <- round(stats::dpois(4,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
UEFANL_fixtures$uefanl_5_6 <- round(stats::dpois(5,UEFANL_fixtures$uefanl_xGH) * stats::dpois(6,UEFANL_fixtures$uefanl_xGA), digits = 4)
#Home win
UEFANL_fixtures$uefanl_H <- (
  UEFANL_fixtures$uefanl_1_0 + UEFANL_fixtures$uefanl_2_0 + UEFANL_fixtures$uefanl_2_1 + UEFANL_fixtures$uefanl_3_0 + UEFANL_fixtures$uefanl_3_1 +
    UEFANL_fixtures$uefanl_3_2 + UEFANL_fixtures$uefanl_4_0 + UEFANL_fixtures$uefanl_4_1 + UEFANL_fixtures$uefanl_4_2 + UEFANL_fixtures$uefanl_4_3 +
    UEFANL_fixtures$uefanl_5_0 + UEFANL_fixtures$uefanl_5_1 + UEFANL_fixtures$uefanl_5_2 + UEFANL_fixtures$uefanl_5_3 + UEFANL_fixtures$uefanl_5_4 +
    UEFANL_fixtures$uefanl_6_0 + UEFANL_fixtures$uefanl_6_1 + UEFANL_fixtures$uefanl_6_2 + UEFANL_fixtures$uefanl_6_3 + UEFANL_fixtures$uefanl_6_4 +
    UEFANL_fixtures$uefanl_6_5
)

UEFANL_fixtures$uefanl_H <- percent(UEFANL_fixtures$uefanl_H, accuracy = 0.1)

#Draw
UEFANL_fixtures$uefanl_D <- (

  UEFANL_fixtures$uefanl_0_0 + UEFANL_fixtures$uefanl_1_1 + UEFANL_fixtures$uefanl_2_2 + UEFANL_fixtures$uefanl_3_3 + UEFANL_fixtures$uefanl_4_4 +
    UEFANL_fixtures$uefanl_5_5 + UEFANL_fixtures$uefanl_6_6
)

UEFANL_fixtures$uefanl_D <- percent(UEFANL_fixtures$uefanl_D, accuracy = 0.1)

#Away

UEFANL_fixtures$uefanl_A <- (
  UEFANL_fixtures$uefanl_0_1 + UEFANL_fixtures$uefanl_0_2 + UEFANL_fixtures$uefanl_1_2 + UEFANL_fixtures$uefanl_0_3 + UEFANL_fixtures$uefanl_1_3 +
    UEFANL_fixtures$uefanl_2_3 + UEFANL_fixtures$uefanl_0_4 + UEFANL_fixtures$uefanl_1_4 + UEFANL_fixtures$uefanl_2_4 + UEFANL_fixtures$uefanl_3_4 +
    UEFANL_fixtures$uefanl_0_5 + UEFANL_fixtures$uefanl_1_5 + UEFANL_fixtures$uefanl_2_5 + UEFANL_fixtures$uefanl_3_5 + UEFANL_fixtures$uefanl_4_5 +
    UEFANL_fixtures$uefanl_0_6 + UEFANL_fixtures$uefanl_1_6 + UEFANL_fixtures$uefanl_2_6 + UEFANL_fixtures$uefanl_3_6 + UEFANL_fixtures$uefanl_4_6 +
    UEFANL_fixtures$uefanl_5_6
)

UEFANL_fixtures$uefanl_A <- percent(UEFANL_fixtures$uefanl_A, accuracy = 0.1)

#ov25
UEFANL_fixtures$uefanl_ov25 <- (
  UEFANL_fixtures$uefanl_2_1 + UEFANL_fixtures$uefanl_1_2 + UEFANL_fixtures$uefanl_2_2 + UEFANL_fixtures$uefanl_3_0 + UEFANL_fixtures$uefanl_3_1 +
    UEFANL_fixtures$uefanl_3_2 + UEFANL_fixtures$uefanl_0_3 + UEFANL_fixtures$uefanl_1_3 + UEFANL_fixtures$uefanl_2_3 + UEFANL_fixtures$uefanl_3_3 +
    UEFANL_fixtures$uefanl_4_0 + UEFANL_fixtures$uefanl_4_1 + UEFANL_fixtures$uefanl_4_2 + UEFANL_fixtures$uefanl_4_3 + UEFANL_fixtures$uefanl_0_4 +
    UEFANL_fixtures$uefanl_1_4 + UEFANL_fixtures$uefanl_2_4 + UEFANL_fixtures$uefanl_3_4 + UEFANL_fixtures$uefanl_4_4 + UEFANL_fixtures$uefanl_5_0 +
    UEFANL_fixtures$uefanl_5_1 + UEFANL_fixtures$uefanl_5_2 + UEFANL_fixtures$uefanl_5_3 + UEFANL_fixtures$uefanl_5_4 + UEFANL_fixtures$uefanl_0_5 +
    UEFANL_fixtures$uefanl_1_5 + UEFANL_fixtures$uefanl_2_5 + UEFANL_fixtures$uefanl_3_5 + UEFANL_fixtures$uefanl_4_5 + UEFANL_fixtures$uefanl_5_5 +
    UEFANL_fixtures$uefanl_6_0 + UEFANL_fixtures$uefanl_6_1 + UEFANL_fixtures$uefanl_6_2 + UEFANL_fixtures$uefanl_6_3 + UEFANL_fixtures$uefanl_6_4 +
    UEFANL_fixtures$uefanl_6_5 + UEFANL_fixtures$uefanl_0_6 + UEFANL_fixtures$uefanl_1_6 + UEFANL_fixtures$uefanl_2_6 + UEFANL_fixtures$uefanl_3_6 +
    UEFANL_fixtures$uefanl_4_6 + UEFANL_fixtures$uefanl_5_6 + UEFANL_fixtures$uefanl_6_6
)
#un25
UEFANL_fixtures$uefanl_un25 <- (
  UEFANL_fixtures$uefanl_0_0 + UEFANL_fixtures$uefanl_1_0 + UEFANL_fixtures$uefanl_0_1 + UEFANL_fixtures$uefanl_1_1 + UEFANL_fixtures$uefanl_2_0 + UEFANL_fixtures$uefanl_0_2
)
#odds
UEFANL_fixtures$uefanl_ov25_odds <- round((1/UEFANL_fixtures$uefanl_ov25),digits = 2)
UEFANL_fixtures$uefanl_un25_odds <- round((1/UEFANL_fixtures$uefanl_un25),digits = 2)

UEFANL_fixtures$uefanl_ov25_odds
UEFANL_fixtures$uefanl_un25_odds
#percentages
UEFANL_fixtures$uefanl_ov25 <- percent(UEFANL_fixtures$uefanl_ov25, accuracy = 0.1)

UEFANL_fixtures$uefanl_un25 <- percent(UEFANL_fixtures$uefanl_un25, accuracy = 0.1)
UEFANL_fixtures$uefanl_pscore <- paste(round(UEFANL_fixtures$uefanl_xGH,digits = 0),round(UEFANL_fixtures$uefanl_xGA,digits = 0),sep = "-")
#write out
write.xlsx(UEFANL_fixtures,'UEFANL.xlsx',sheetName = "UEFANL", append = TRUE)
###########################################################################################################
########################UEFANL END###########################################################################




