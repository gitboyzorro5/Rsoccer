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
unlink('FRIENDLIES.xlsx')
######################FRIENDLIES START#######################################
#####################################################################
FRIENDLIES <- read.csv('../../../Leonard.000/Downloads/IFootball/results.csv')
library('lubridate')
FRIENDLIES$date <- ymd(FRIENDLIES$date)
FRIENDLIES <- FRIENDLIES[order(as.Date(FRIENDLIES$date, format = "%Y/%m%d"), decreasing = FALSE),]
#FRIENDLIES_qualificaton <- subset(FRIENDLIES,tournament == "Friendly")
FRIENDLIES <- subset(FRIENDLIES,tournament == "Friendly")
FRIENDLIES <- FRIENDLIES[FRIENDLIES$date > '2020-01-01',]
FRIENDLIES$TG <- FRIENDLIES$home_score + FRIENDLIES$away_score
FRIENDLIES$OV25 <- ifelse(FRIENDLIES$TG >= 3,"Y","N")
FRIENDLIES$FTR <- with(FRIENDLIES,
                 ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
friendlies_totalgoalsv2 <- tapply(FRIENDLIES$TG, FRIENDLIES[c("home_team", "away_team")],mean)
friendlies_hgtotals <- rowSums(friendlies_totalgoalsv2, na.rm = T)
friendlies_agtotals <- colSums(friendlies_totalgoalsv2, na.rm = T)
friendlies_totalgoals <- friendlies_hgtotals + friendlies_agtotals
friendlies_totalgoalsv2 <- cbind(friendlies_totalgoalsv2,friendlies_totalgoals)
friendlies_teams <- sort(unique(FRIENDLIES$home_team))
friendlies_home_games <- c()
friendlies_away_games <-c()
for (i_friendlies in 1:length(friendlies_teams))
{

  friendlies_home_games[i_friendlies] <- nrow(FRIENDLIES[FRIENDLIES$home_team == friendlies_teams[i_friendlies],])
  friendlies_away_games[i_friendlies]  <- nrow(FRIENDLIES[FRIENDLIES$away_team == friendlies_teams[i_friendlies],])

}
friendlies_games_played <- friendlies_home_games + friendlies_away_games
friendlies_goaltotalsv2 <- cbind(friendlies_totalgoalsv2,friendlies_games_played)
friendlies_avg_totalgoals <- round((friendlies_totalgoals/ friendlies_games_played), digits = 4)
friendlies_goaltotalsv2[is.na(friendlies_goaltotalsv2)] <- ""
friendlies_goaltotalsv2 <- cbind(friendlies_goaltotalsv2,friendlies_avg_totalgoals)
write.xlsx(friendlies_goaltotalsv2,'FRIENDLIES.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
friendlies_goalscored_h <- tapply(FRIENDLIES$home_score, FRIENDLIES[c("home_team", "date")],mean)
friendlies_goalscored_a <- tapply(FRIENDLIES$away_score, FRIENDLIES[c("away_team", "date")],mean)
friendlies_goalscored_h[is.na(friendlies_goalscored_h)] <- ""
friendlies_goalscored_a[is.na(friendlies_goalscored_a)] <- ""

for(friendlies_rowhgs in 1:nrow(friendlies_goalscored_h)) {
  for(friendlies_colhgs in 1:ncol(friendlies_goalscored_h)) {

    # print(my_matrix[row, col])
    for(friendlies_rowags in 1:nrow(friendlies_goalscored_a)) {
      for(friendlies_colags in 1:ncol(friendlies_goalscored_a)) {
        ifelse(!friendlies_goalscored_a[friendlies_rowags,friendlies_colags]=="",friendlies_goalscored_h[friendlies_rowags,friendlies_colags] <- friendlies_goalscored_a[friendlies_rowags,friendlies_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(friendlies_goalscored_h,'FRIENDLIES.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
friendlies_goalconceded_h <- tapply(FRIENDLIES$away_score, FRIENDLIES[c("home_team", "date")],mean)
friendlies_goalconceded_a <- tapply(FRIENDLIES$home_score, FRIENDLIES[c("away_team", "date")],mean)
friendlies_goalconceded_h[is.na(friendlies_goalconceded_h)] <- ""
friendlies_goalconceded_a[is.na(friendlies_goalconceded_a)] <- ""

for(friendlies_rowhgc in 1:nrow(friendlies_goalconceded_h)) {
  for(friendlies_colhgc in 1:ncol(friendlies_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(friendlies_rowagc in 1:nrow(friendlies_goalconceded_a)) {
      for(friendlies_colagc in 1:ncol(friendlies_goalconceded_a)) {
        ifelse(!friendlies_goalconceded_a[friendlies_rowagc,friendlies_colagc]=="",friendlies_goalconceded_h[friendlies_rowagc,friendlies_colagc] <- friendlies_goalconceded_a[friendlies_rowagc,friendlies_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(friendlies_goalconceded_h,'FRIENDLIES.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
friendlies_form_h <- tapply(FRIENDLIES$FTR, FRIENDLIES[c("home_team", "date")],median)
friendlies_form_a <- tapply(FRIENDLIES$FTR, FRIENDLIES[c("away_team", "date")],median)
friendlies_form_h[is.na(friendlies_form_h)] <- ""
friendlies_form_a[is.na(friendlies_form_a)] <- ""
friendlies_form_h <- sub("A","L",friendlies_form_h)
friendlies_form_h <- sub("H","W",friendlies_form_h)
friendlies_form_a <- sub("A","W",friendlies_form_a)
friendlies_form_a <- sub("H","L",friendlies_form_a)
for(friendlies_rowh_f in 1:nrow(friendlies_form_h)) {
  for(friendlies_colh_f in 1:ncol(friendlies_form_h)) {

    # print(my_matrix[row, col])
    for(friendlies_rowa_f in 1:nrow(friendlies_form_a)) {
      for(friendlies_cola_f in 1:ncol(friendlies_form_a)) {
        ifelse(!friendlies_form_a[friendlies_rowa_f,friendlies_cola_f]=="",friendlies_form_h[friendlies_rowa_f,friendlies_cola_f] <- friendlies_form_a[friendlies_rowa_f,friendlies_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(friendlies_form_h,'FRIENDLIES.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
friendlies_totalgoals_h <- tapply(FRIENDLIES$TG, FRIENDLIES[c("home_team", "date")],mean)
friendlies_totalgoals_a <- tapply(FRIENDLIES$TG, FRIENDLIES[c("away_team", "date")],mean)
friendlies_totalgoals_h[is.na(friendlies_totalgoals_h)] <- ""
friendlies_totalgoals_a[is.na(friendlies_totalgoals_a)] <- ""
for(friendlies_rowh in 1:nrow(friendlies_totalgoals_h)) {
  for(friendlies_colh in 1:ncol(friendlies_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(friendlies_rowa in 1:nrow(friendlies_totalgoals_a)) {
      for(friendlies_cola in 1:ncol(friendlies_totalgoals_a)) {
        ifelse(!friendlies_totalgoals_a[friendlies_rowa,friendlies_cola]=="",friendlies_totalgoals_h[friendlies_rowa,friendlies_cola] <- friendlies_totalgoals_a[friendlies_rowa,friendlies_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(friendlies_totalgoals_h,'FRIENDLIES.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
friendlies_form_team_against_h <- tapply(FRIENDLIES$away_team, FRIENDLIES[c("home_team", "date")],median)
friendlies_form_team_against_a <- tapply(FRIENDLIES$home_team, FRIENDLIES[c("away_team", "date")],median)
friendlies_form_team_against_h[is.na(friendlies_form_team_against_h)] <- ""
friendlies_form_team_against_a[is.na(friendlies_form_team_against_a)] <- ""
for(friendlies_rowh_f_against in 1:nrow(friendlies_form_team_against_h)) {
  for(friendlies_colh_f_against in 1:ncol(friendlies_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(friendlies_rowa_f_against in 1:nrow(friendlies_form_team_against_a)) {
      for(friendlies_cola_f_against in 1:ncol(friendlies_form_team_against_a)) {
        ifelse(!friendlies_form_team_against_a[friendlies_rowa_f_against,friendlies_cola_f_against]=="",friendlies_form_team_against_h[friendlies_rowa_f_against,friendlies_cola_f_against] <- friendlies_form_team_against_a[friendlies_rowa_f_against,friendlies_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

###########################################################################################
############Scoring and conceding analysis
#home goals scored
friendlies_home_gs <- aggregate(FRIENDLIES$home_score, by = list(FRIENDLIES$home_team), FUN = sum)
friendlies_home_gs_avg <- aggregate(FRIENDLIES$home_score, by = list(FRIENDLIES$home_team),mean)
friendlies_home_scoring <- merge(friendlies_home_gs,friendlies_home_gs_avg, by='Group.1',all = T)
names(friendlies_home_scoring)[names(friendlies_home_scoring) == "x.x"] <- "TFthg"
names(friendlies_home_scoring)[names(friendlies_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
friendlies_away_gs <- aggregate(FRIENDLIES$away_score, by = list(FRIENDLIES$away_team), FUN = sum)
friendlies_away_gs_avg <- aggregate(FRIENDLIES$away_score, by = list(FRIENDLIES$away_team),mean)
friendlies_away_scoring <- merge(friendlies_away_gs,friendlies_away_gs_avg, by='Group.1',all = T)
names(friendlies_away_scoring)[names(friendlies_away_scoring) == "x.x"] <- "TFtag"
names(friendlies_away_scoring)[names(friendlies_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
friendlies_scoring <- merge(friendlies_home_scoring,friendlies_away_scoring,by='Group.1',all = T)
friendlies_scoring$TGS <- friendlies_scoring$TFthg + friendlies_scoring$TFtag

#home goals conceded
friendlies_home_gc <- aggregate(FRIENDLIES$away_score, by = list(FRIENDLIES$home_team), FUN = sum)
friendlies_home_gc_avg <- aggregate(FRIENDLIES$away_score, by = list(FRIENDLIES$home_team),mean)
friendlies_home_conceding <- merge(friendlies_home_gc,friendlies_home_gc_avg, by='Group.1',all = T)
names(friendlies_home_conceding)[names(friendlies_home_conceding) == "x.x"] <- "TFthc"
names(friendlies_home_conceding)[names(friendlies_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
friendlies_away_gc <- aggregate(FRIENDLIES$home_score, by = list(FRIENDLIES$away_team), FUN = sum)
friendlies_away_gc_avg <- aggregate(FRIENDLIES$home_score, by = list(FRIENDLIES$away_team),mean)
friendlies_away_conceding <- merge(friendlies_away_gc,friendlies_away_gc_avg, by='Group.1',all = T)
names(friendlies_away_conceding)[names(friendlies_away_conceding) == "x.x"] <- "TFtac"
names(friendlies_away_conceding)[names(friendlies_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
friendlies_conceding <- merge(friendlies_home_conceding,friendlies_away_conceding,by='Group.1',all = T)
friendlies_conceding$TGC <- friendlies_conceding$TFthc + friendlies_conceding$TFtac

friendlies_shots_analysis <- merge(friendlies_scoring_conversion,friendlies_conceding_conversion,by='Group.1',all = T)

######################################################################################
###########League Table###############################################################

#hwins and away wins
friendlies_home_wins <- c()
friendlies_away_wins <- c()
friendlies_home_draws <- c()
friendlies_away_draws <- c()
friendlies_home_loss <- c()
friendlies_away_loss <- c()



for (i_friendlies_wins in 1:length(friendlies_teams))
{

  friendlies_home_wins[i_friendlies_wins] <- nrow(FRIENDLIES[FRIENDLIES$home_team == friendlies_teams[i_friendlies_wins] & FRIENDLIES$FTR == "H",])
  friendlies_away_wins[i_friendlies_wins] <- nrow(FRIENDLIES[FRIENDLIES$away_team == friendlies_teams[i_friendlies_wins] & FRIENDLIES$FTR == "A",])
  friendlies_home_draws[i_friendlies_wins] <- nrow(FRIENDLIES[FRIENDLIES$home_team == friendlies_teams[i_friendlies_wins] & FRIENDLIES$FTR == "D",])
  friendlies_away_draws[i_friendlies_wins] <- nrow(FRIENDLIES[FRIENDLIES$away_team == friendlies_teams[i_friendlies_wins] & FRIENDLIES$FTR == "D",])
  friendlies_home_loss[i_friendlies_wins] <- nrow(FRIENDLIES[FRIENDLIES$home_team == friendlies_teams[i_friendlies_wins] & FRIENDLIES$FTR == "A",])
  friendlies_away_loss[i_friendlies_wins] <- nrow(FRIENDLIES[FRIENDLIES$away_team == friendlies_teams[i_friendlies_wins] & FRIENDLIES$FTR == "H",])

}

friendlies_total_wins <- friendlies_home_wins + friendlies_away_wins
friendlies_total_draws <- friendlies_home_draws + friendlies_away_draws
friendlies_total_loss <- friendlies_home_loss + friendlies_away_loss

friendlies_league_table <- cbind(friendlies_teams,friendlies_games_played,friendlies_total_wins,friendlies_total_draws,friendlies_total_loss)
friendlies_GS <- friendlies_scoring$TGS
friendlies_GC <-friendlies_conceding$TGC
friendlies_GD <- friendlies_scoring$TGS - friendlies_conceding$TGC
friendlies_PTS <- (friendlies_total_wins*3) + (friendlies_total_draws*1)
friendlies_league_table <- cbind(friendlies_league_table,friendlies_GS,friendlies_GC,friendlies_GD,friendlies_PTS)
friendlies_league_table <- as.data.frame(friendlies_league_table)
#rename the columns
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_teams"] <- "Team"
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_games_played"] <- "P"
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_total_wins"] <- "W"
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_total_draws"] <- "D"
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_total_loss"] <- "L"
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_GS"] <- "F"
names(friendlies_league_table)[names(friendlies_league_table) == "friendlies_GC"] <- "A"
points_friendlies <- friendlies_league_table[order(friendlies_league_table$friendlies_PTS, decreasing = TRUE),]
write.xlsx(points_friendlies,'FRIENDLIES.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six friendlies###################################################
#FRIENDLIES
#form
#create final_friendlies_hf object
final_friendlies_hf <- c()
for(index_friendlies_hf in 1:length(friendlies_teams))
{
  index_friendlies_hf <- row.names(friendlies_form_h) == friendlies_teams[index_friendlies_hf]
  form_friendlies_hf <- friendlies_form_h[index_friendlies_hf]
  deleted_form_friendlies_hf <- form_friendlies_hf[!form_friendlies_hf[] == ""]
  l6_form_friendlies_hf <- tail(deleted_form_friendlies_hf,6)
  l6_form_friendlies_hf <- paste(l6_form_friendlies_hf,collapse = " ")
  final_friendlies_hf[index_friendlies_hf] <- rbind(paste(friendlies_teams[index_friendlies_hf],l6_form_friendlies_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",friendlies_teams[index],l6_form)

}

#change column names
final_friendlies_hf <- as.data.frame(final_friendlies_hf)
colnames(final_friendlies_hf) <- "Form"
#goals scored
#create final_friendlies_gs object
final_friendlies_gs <- c()
suml6_friendlies_gs <- c()
for(index_friendlies_gs in 1:length(friendlies_teams))
{
  index_friendlies_gs <- row.names(friendlies_goalscored_h) == friendlies_teams[index_friendlies_gs]
  form_friendlies_gs <- friendlies_goalscored_h[index_friendlies_gs]
  deleted_form_friendlies_gs <- form_friendlies_gs[!form_friendlies_gs[] == ""]
  l6_form_friendlies_gs <- tail(deleted_form_friendlies_gs,6)
  l6_form_friendlies_gs <- as.numeric(l6_form_friendlies_gs)
  suml6_friendlies_gs[index_friendlies_gs] <- sum(l6_form_friendlies_gs)
  suml6_friendlies_gs[index_friendlies_gs] <- paste("(",suml6_friendlies_gs[index_friendlies_gs],")",sep = "")
  l6_form_friendlies_gs <- paste(l6_form_friendlies_gs,collapse = " ")
  final_friendlies_gs[index_friendlies_gs] <- rbind(paste(friendlies_teams[index_friendlies_gs],l6_form_friendlies_gs,suml6_friendlies_gs[index_friendlies_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",friendlies_teams[index],l6_form)

}
final_friendlies_gs
#change column names
final_friendlies_gs <- as.data.frame(final_friendlies_gs)
colnames(final_friendlies_gs) <- "Goals scored"
#goal conceded
#create final_friendlies_gc object
final_friendlies_gc <- c()
suml6_friendlies_gc <- c()
for(index_friendlies_gc in 1:length(friendlies_teams))
{
  index_friendlies_gc <- row.names(friendlies_goalconceded_h) == friendlies_teams[index_friendlies_gc]
  form_friendlies_gc <- friendlies_goalconceded_h[index_friendlies_gc]
  deleted_form_friendlies_gc <- form_friendlies_gc[!form_friendlies_gc[] == ""]
  l6_form_friendlies_gc <- tail(deleted_form_friendlies_gc,6)
  l6_form_friendlies_gc <- as.numeric(l6_form_friendlies_gc)
  suml6_friendlies_gc[index_friendlies_gc] <- sum(l6_form_friendlies_gc)
  suml6_friendlies_gc[index_friendlies_gc] <- paste("(",suml6_friendlies_gc[index_friendlies_gc],")",sep = "")
  l6_form_friendlies_gc <- paste(l6_form_friendlies_gc,collapse = " ")
  final_friendlies_gc[index_friendlies_gc] <- rbind(paste(friendlies_teams[index_friendlies_gc],l6_form_friendlies_gc,suml6_friendlies_gc[index_friendlies_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",friendlies_teams[index],l6_form)

}

#change column names
final_friendlies_gc <- as.data.frame(final_friendlies_gc)
colnames(final_friendlies_gc) <- "Goals conceded"
#total goals
#create final_friendlies_tg object
final_friendlies_tg <- c()
suml6_friendlies_tg <- c()
for(index_friendlies_tg in 1:length(friendlies_teams))
{
  index_friendlies_tg <- row.names(friendlies_totalgoals_h) == friendlies_teams[index_friendlies_tg]
  form_friendlies_tg <- friendlies_totalgoals_h[index_friendlies_tg]
  deleted_form_friendlies_tg <- form_friendlies_tg[!form_friendlies_tg[] == ""]
  l6_form_friendlies_tg <- tail(deleted_form_friendlies_tg,6)
  l6_form_friendlies_tg <- as.numeric(l6_form_friendlies_tg)
  suml6_friendlies_tg[index_friendlies_tg] <- sum(l6_form_friendlies_tg)
  suml6_friendlies_tg[index_friendlies_tg] <- paste("(",suml6_friendlies_tg[index_friendlies_tg],")",sep = "")
  l6_form_friendlies_tg <- paste(l6_form_friendlies_tg,collapse = " ")
  final_friendlies_tg[index_friendlies_tg] <- rbind(paste(friendlies_teams[index_friendlies_tg],l6_form_friendlies_tg,suml6_friendlies_tg[index_friendlies_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",friendlies_teams[index],l6_form)

}
#change column names
final_friendlies_tg <- as.data.frame(final_friendlies_tg)
colnames(final_friendlies_tg) <- "Total Goals"
#Team against
#create final_friendlies_hf_against
final_friendlies_hf_against <- c()
for(index_friendlies_hf_against in 1:length(friendlies_teams))
{
  index_friendlies_hf_against <- row.names(friendlies_form_team_against_h) == friendlies_teams[index_friendlies_hf_against]
  form_friendlies_hf_against <- friendlies_form_team_against_h[index_friendlies_hf_against]
  deleted_form_friendlies_hf_against <- form_friendlies_hf_against[!form_friendlies_hf_against[] == ""]
  l6_form_friendlies_hf_against <- tail(deleted_form_friendlies_hf_against,6)
  l6_form_friendlies_hf_against <- paste(l6_form_friendlies_hf_against,collapse = " ")
  final_friendlies_hf_against[index_friendlies_hf_against] <- rbind(paste(friendlies_teams[index_friendlies_hf_against],l6_form_friendlies_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",friendlies_teams[index],l6_form)

}
final_friendlies_hf_against <- as.data.frame(final_friendlies_hf_against)
colnames(final_friendlies_hf_against) <- "Team against"
#combine the columns
final_friendlies_all <- cbind(final_friendlies_hf,final_friendlies_gs,final_friendlies_gc,final_friendlies_tg,final_friendlies_hf_against)
write.xlsx(final_friendlies_all,'FRIENDLIES.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
friendlies_GP <- nrow(FRIENDLIES)
#Calculate total home goals for each division
friendlies_T_HG <- sum(friendlies_home_gs$x)
#calculate average home goal
friendlies_avg_HG <- round(friendlies_T_HG /friendlies_GP, digits = 4)
############################################################
#Calculate total away goals for each division
friendlies_T_AG <- sum(friendlies_away_gs$x)
#calculate average away goal
friendlies_avg_AG <- round(friendlies_T_AG /friendlies_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
friendlies_home_as <- round(((friendlies_home_gs$x/friendlies_home_games))/friendlies_avg_HG, digits = 4)
#calculate away attack strength
friendlies_away_as <- round(((friendlies_away_gs$x/friendlies_away_games))/friendlies_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
friendlies_avg_HC <- round(friendlies_T_AG /friendlies_GP, digits = 4)
#avg away concede
friendlies_avg_AC <- round(friendlies_T_HG /friendlies_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
friendlies_home_ds <- round(((friendlies_home_gc$x/friendlies_home_games))/friendlies_avg_HC, digits = 4)
#away defense strength
friendlies_away_ds <- round(((friendlies_away_gc$x/friendlies_away_games))/friendlies_avg_AC, digits = 4)
#############################################################################
#home poisson data
#friendlies
friendlies_division <- c()
friendlies_division[1:length(friendlies_teams)] <- "FRIENDLIES"
friendlies_home_poisson <- cbind(friendlies_division,friendlies_teams,friendlies_avg_HG,friendlies_home_as,friendlies_home_ds)
#################################################################################
#away poisson data
#friendlies
friendlies_division <- c()
friendlies_division[1:length(friendlies_teams)] <- "FRIENDLIES"
friendlies_away_poisson <- cbind(friendlies_division,friendlies_teams,friendlies_avg_AG,friendlies_away_as,friendlies_away_ds)

#create home and away csv
#friendlies_home_poisson <- rbind(friendlies_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#friendlies_away_poisson <- rbind(friendlies_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(friendlies_home_poisson,'FRIENDLIES.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(friendlies_away_poisson,'FRIENDLIES.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################FRIENDLIES FIXTURES##########################################################################
#FRIENDLIES
HomeTeam_friendlies <- rep(friendlies_teams, each = length(friendlies_teams))
AwayTeam_friendlies <- rep(friendlies_teams, length(friendlies_teams))
FRIENDLIES_fixtures <- cbind(HomeTeam_friendlies,AwayTeam_friendlies)
FRIENDLIES_fixtures <- as.data.frame(FRIENDLIES_fixtures)
FRIENDLIES_fixtures <- FRIENDLIES_fixtures[!FRIENDLIES_fixtures$HomeTeam_friendlies == FRIENDLIES_fixtures$AwayTeam_friendlies,]
rownames(FRIENDLIES_fixtures) <- NULL
FRIENDLIES_fixtures$Div <- "FRIENDLIES"
FRIENDLIES_fixtures <- FRIENDLIES_fixtures[,c(3,1,2)]

FRIENDLIES_fixtures$avg_HG_friendlies <- friendlies_avg_HG

FRIENDLIES_fixtures$friendlies_homeas <- rep(friendlies_home_as,each = length(friendlies_teams)-1)

friendlies_awayds_lookup <- cbind(friendlies_teams,friendlies_away_ds)

friendlies_awayds_lookup <- as.data.frame(friendlies_awayds_lookup)

colnames(friendlies_awayds_lookup) <- c("AwayTeam_friendlies","friendlies_awayds")


require('RH2')
FRIENDLIES_fixtures$friendlies_awayds <- sqldf("SELECT friendlies_awayds_lookup.friendlies_awayds FROM friendlies_awayds_lookup INNER JOIN FRIENDLIES_fixtures ON friendlies_awayds_lookup.AwayTeam_friendlies = FRIENDLIES_fixtures.AwayTeam_friendlies")

FRIENDLIES_fixtures$avg_AG_friendlies <- friendlies_avg_AG

friendlies_awayas_lookup <- cbind(friendlies_teams,friendlies_away_as)

friendlies_awayas_lookup <- as.data.frame(friendlies_awayas_lookup)

colnames(friendlies_awayas_lookup) <- c("AwayTeam_friendlies","friendlies_awayas")


FRIENDLIES_fixtures$friendlies_awayas <- sqldf("SELECT friendlies_awayas_lookup.friendlies_awayas FROM friendlies_awayas_lookup INNER JOIN FRIENDLIES_fixtures ON friendlies_awayas_lookup.AwayTeam_friendlies = FRIENDLIES_fixtures.AwayTeam_friendlies")

FRIENDLIES_fixtures$friendlies_homeds <- rep(friendlies_home_ds,each = length(friendlies_teams)-1)

FRIENDLIES_fixtures$friendlies_awayds <- as.numeric(unlist(FRIENDLIES_fixtures$friendlies_awayds))
#xGH
FRIENDLIES_fixtures$friendlies_xGH <- FRIENDLIES_fixtures$avg_HG_friendlies * FRIENDLIES_fixtures$friendlies_homeas * FRIENDLIES_fixtures$friendlies_awayds

#xGA

FRIENDLIES_fixtures$friendlies_awayas <- as.numeric(unlist(FRIENDLIES_fixtures$friendlies_awayas))

FRIENDLIES_fixtures$friendlies_xGA <- FRIENDLIES_fixtures$avg_AG_friendlies * FRIENDLIES_fixtures$friendlies_awayas * FRIENDLIES_fixtures$friendlies_homeds

FRIENDLIES_fixtures$friendlies_0_0 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_0 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_0_1 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_1 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_0 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_0_2 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_2 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_1 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_2 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_3 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_0 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_1 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_2 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_0_3 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_3 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_3 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_4 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_0 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_1 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_2 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_3 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_0_4 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_4 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_4 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_4 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_5 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_0 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_1 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_2 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_3 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_4 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_0_5 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_5 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_5 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_5 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_5 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_6 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_0 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_1 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_2 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_3 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_4 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_6_5 <- round(stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_0_6 <- round(stats::dpois(0,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_1_6 <- round(stats::dpois(1,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_2_6 <- round(stats::dpois(2,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_3_6 <- round(stats::dpois(3,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_4_6 <- round(stats::dpois(4,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
FRIENDLIES_fixtures$friendlies_5_6 <- round(stats::dpois(5,FRIENDLIES_fixtures$friendlies_xGH) * stats::dpois(6,FRIENDLIES_fixtures$friendlies_xGA), digits = 4)
#Home win
FRIENDLIES_fixtures$friendlies_H <- (
  FRIENDLIES_fixtures$friendlies_1_0 + FRIENDLIES_fixtures$friendlies_2_0 + FRIENDLIES_fixtures$friendlies_2_1 + FRIENDLIES_fixtures$friendlies_3_0 + FRIENDLIES_fixtures$friendlies_3_1 +
    FRIENDLIES_fixtures$friendlies_3_2 + FRIENDLIES_fixtures$friendlies_4_0 + FRIENDLIES_fixtures$friendlies_4_1 + FRIENDLIES_fixtures$friendlies_4_2 + FRIENDLIES_fixtures$friendlies_4_3 +
    FRIENDLIES_fixtures$friendlies_5_0 + FRIENDLIES_fixtures$friendlies_5_1 + FRIENDLIES_fixtures$friendlies_5_2 + FRIENDLIES_fixtures$friendlies_5_3 + FRIENDLIES_fixtures$friendlies_5_4 +
    FRIENDLIES_fixtures$friendlies_6_0 + FRIENDLIES_fixtures$friendlies_6_1 + FRIENDLIES_fixtures$friendlies_6_2 + FRIENDLIES_fixtures$friendlies_6_3 + FRIENDLIES_fixtures$friendlies_6_4 +
    FRIENDLIES_fixtures$friendlies_6_5
)

FRIENDLIES_fixtures$friendlies_H <- percent(FRIENDLIES_fixtures$friendlies_H, accuracy = 0.1)

#Draw
FRIENDLIES_fixtures$friendlies_D <- (

  FRIENDLIES_fixtures$friendlies_0_0 + FRIENDLIES_fixtures$friendlies_1_1 + FRIENDLIES_fixtures$friendlies_2_2 + FRIENDLIES_fixtures$friendlies_3_3 + FRIENDLIES_fixtures$friendlies_4_4 +
    FRIENDLIES_fixtures$friendlies_5_5 + FRIENDLIES_fixtures$friendlies_6_6
)

FRIENDLIES_fixtures$friendlies_D <- percent(FRIENDLIES_fixtures$friendlies_D, accuracy = 0.1)

#Away

FRIENDLIES_fixtures$friendlies_A <- (
  FRIENDLIES_fixtures$friendlies_0_1 + FRIENDLIES_fixtures$friendlies_0_2 + FRIENDLIES_fixtures$friendlies_1_2 + FRIENDLIES_fixtures$friendlies_0_3 + FRIENDLIES_fixtures$friendlies_1_3 +
    FRIENDLIES_fixtures$friendlies_2_3 + FRIENDLIES_fixtures$friendlies_0_4 + FRIENDLIES_fixtures$friendlies_1_4 + FRIENDLIES_fixtures$friendlies_2_4 + FRIENDLIES_fixtures$friendlies_3_4 +
    FRIENDLIES_fixtures$friendlies_0_5 + FRIENDLIES_fixtures$friendlies_1_5 + FRIENDLIES_fixtures$friendlies_2_5 + FRIENDLIES_fixtures$friendlies_3_5 + FRIENDLIES_fixtures$friendlies_4_5 +
    FRIENDLIES_fixtures$friendlies_0_6 + FRIENDLIES_fixtures$friendlies_1_6 + FRIENDLIES_fixtures$friendlies_2_6 + FRIENDLIES_fixtures$friendlies_3_6 + FRIENDLIES_fixtures$friendlies_4_6 +
    FRIENDLIES_fixtures$friendlies_5_6
)

FRIENDLIES_fixtures$friendlies_A <- percent(FRIENDLIES_fixtures$friendlies_A, accuracy = 0.1)

#ov25
FRIENDLIES_fixtures$friendlies_ov25 <- (
  FRIENDLIES_fixtures$friendlies_2_1 + FRIENDLIES_fixtures$friendlies_1_2 + FRIENDLIES_fixtures$friendlies_2_2 + FRIENDLIES_fixtures$friendlies_3_0 + FRIENDLIES_fixtures$friendlies_3_1 +
    FRIENDLIES_fixtures$friendlies_3_2 + FRIENDLIES_fixtures$friendlies_0_3 + FRIENDLIES_fixtures$friendlies_1_3 + FRIENDLIES_fixtures$friendlies_2_3 + FRIENDLIES_fixtures$friendlies_3_3 +
    FRIENDLIES_fixtures$friendlies_4_0 + FRIENDLIES_fixtures$friendlies_4_1 + FRIENDLIES_fixtures$friendlies_4_2 + FRIENDLIES_fixtures$friendlies_4_3 + FRIENDLIES_fixtures$friendlies_0_4 +
    FRIENDLIES_fixtures$friendlies_1_4 + FRIENDLIES_fixtures$friendlies_2_4 + FRIENDLIES_fixtures$friendlies_3_4 + FRIENDLIES_fixtures$friendlies_4_4 + FRIENDLIES_fixtures$friendlies_5_0 +
    FRIENDLIES_fixtures$friendlies_5_1 + FRIENDLIES_fixtures$friendlies_5_2 + FRIENDLIES_fixtures$friendlies_5_3 + FRIENDLIES_fixtures$friendlies_5_4 + FRIENDLIES_fixtures$friendlies_0_5 +
    FRIENDLIES_fixtures$friendlies_1_5 + FRIENDLIES_fixtures$friendlies_2_5 + FRIENDLIES_fixtures$friendlies_3_5 + FRIENDLIES_fixtures$friendlies_4_5 + FRIENDLIES_fixtures$friendlies_5_5 +
    FRIENDLIES_fixtures$friendlies_6_0 + FRIENDLIES_fixtures$friendlies_6_1 + FRIENDLIES_fixtures$friendlies_6_2 + FRIENDLIES_fixtures$friendlies_6_3 + FRIENDLIES_fixtures$friendlies_6_4 +
    FRIENDLIES_fixtures$friendlies_6_5 + FRIENDLIES_fixtures$friendlies_0_6 + FRIENDLIES_fixtures$friendlies_1_6 + FRIENDLIES_fixtures$friendlies_2_6 + FRIENDLIES_fixtures$friendlies_3_6 +
    FRIENDLIES_fixtures$friendlies_4_6 + FRIENDLIES_fixtures$friendlies_5_6 + FRIENDLIES_fixtures$friendlies_6_6
)
#un25
FRIENDLIES_fixtures$friendlies_un25 <- (
  FRIENDLIES_fixtures$friendlies_0_0 + FRIENDLIES_fixtures$friendlies_1_0 + FRIENDLIES_fixtures$friendlies_0_1 + FRIENDLIES_fixtures$friendlies_1_1 + FRIENDLIES_fixtures$friendlies_2_0 + FRIENDLIES_fixtures$friendlies_0_2
)
#odds
FRIENDLIES_fixtures$friendlies_ov25_odds <- round((1/FRIENDLIES_fixtures$friendlies_ov25),digits = 2)
FRIENDLIES_fixtures$friendlies_un25_odds <- round((1/FRIENDLIES_fixtures$friendlies_un25),digits = 2)

FRIENDLIES_fixtures$friendlies_ov25_odds
FRIENDLIES_fixtures$friendlies_un25_odds
#percentages
FRIENDLIES_fixtures$friendlies_ov25 <- percent(FRIENDLIES_fixtures$friendlies_ov25, accuracy = 0.1)

FRIENDLIES_fixtures$friendlies_un25 <- percent(FRIENDLIES_fixtures$friendlies_un25, accuracy = 0.1)
FRIENDLIES_fixtures$friendlies_pscore <- paste(round(FRIENDLIES_fixtures$friendlies_xGH,digits = 0),round(FRIENDLIES_fixtures$friendlies_xGA,digits = 0),sep = "-")
#write out
write.xlsx(FRIENDLIES_fixtures,'FRIENDLIES.xlsx',sheetName = "FRIENDLIES", append = TRUE)
###########################################################################################################
########################FRIENDLIES END###########################################################################
