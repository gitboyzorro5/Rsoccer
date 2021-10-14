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
unlink('FWCQ.xlsx')
######################FWCQ START#######################################
#####################################################################
FWCQ <- read.csv('results.csv')
FWCQ$date <- ymd(FWCQ$date)
FWCQ <- FWCQ[order(as.Date(FWCQ$date, format = "%d/%m%Y"), decreasing = FALSE),]
FWCQ$CS <- paste(FWCQ$home_score,FWCQ$away_score, sep = "-")
FWCQ <- subset(FWCQ,tournament == "FIFA World Cup qualification")
FWCQ <- FWCQ[FWCQ$date >= '2019-06-06',]
FWCQ$TG <- FWCQ$home_score + FWCQ$away_score
FWCQ$OV25 <- ifelse(FWCQ$TG >= 3,"Y","N")
FWCQ$FTR <- with(FWCQ,
                   ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
)
#############################################
FWCQ <- dplyr::left_join(FIFA_conf,FWCQ)
####  require('RH2')
##### FWCQ$confederation <- sqldf("SELECT FIFA_conf.div FROM FIFA_conf INNER JOIN FWCQ ON FIFA_conf.conf_team = FWCQ.home_team")
#############################################
FWCQ <- subset(FWCQ,div == "CAF")

###################################################
####GoalTotalsv2##################################
fwcq_totalgoalsv2 <- tapply(FWCQ$TG, FWCQ[c("home_team", "away_team")],mean)
fwcq_totalgoalsv2
fwcq_hgtotals <- rowSums(fwcq_totalgoalsv2,na.rm = T)
fwcq_agtotals <- colSums(fwcq_totalgoalsv2,na.rm = T)

fwcq_totalgoals <- fwcq_hgtotals + fwcq_agtotals
fwcq_totalgoalsv2 <- cbind(fwcq_totalgoalsv2,fwcq_totalgoals)
fwcq_teams <- sort(unique(FWCQ$away_team))
fwcq_home_games <- c()
fwcq_away_games <-c()
for (i_fwcq in 1:length(fwcq_teams))
{

  fwcq_home_games[i_fwcq] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq],])
  fwcq_away_games[i_fwcq]  <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq],])

}
fwcq_games_played <- fwcq_home_games + fwcq_away_games
fwcq_goaltotalsv2 <- cbind(fwcq_totalgoalsv2,fwcq_games_played)
fwcq_avg_totalgoals <- round((fwcq_totalgoals/ fwcq_games_played), digits = 4)
fwcq_goaltotalsv2[is.na(fwcq_goaltotalsv2)] <- ""
fwcq_goaltotalsv2 <- cbind(fwcq_goaltotalsv2,fwcq_avg_totalgoals)
write.xlsx(fwcq_goaltotalsv2,'FWCQ.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
fwcq_goalscored_h <- tapply(FWCQ$home_score, FWCQ[c("home_team", "date")],mean)
fwcq_goalscored_a <- tapply(FWCQ$away_score, FWCQ[c("away_team", "date")],mean)
fwcq_goalscored_h[is.na(fwcq_goalscored_h)] <- ""
fwcq_goalscored_a[is.na(fwcq_goalscored_a)] <- ""

for(fwcq_rowhgs in 1:nrow(fwcq_goalscored_h)) {
  for(fwcq_colhgs in 1:ncol(fwcq_goalscored_h)) {

    # print(my_matrix[row, col])
    for(fwcq_rowags in 1:nrow(fwcq_goalscored_a)) {
      for(fwcq_colags in 1:ncol(fwcq_goalscored_a)) {
        ifelse(!fwcq_goalscored_a[fwcq_rowags,fwcq_colags]=="",fwcq_goalscored_h[fwcq_rowags,fwcq_colags] <- fwcq_goalscored_a[fwcq_rowags,fwcq_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fwcq_goalscored_h,'FWCQ.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
fwcq_goalconceded_h <- tapply(FWCQ$away_score, FWCQ[c("home_team", "date")],mean)
fwcq_goalconceded_a <- tapply(FWCQ$home_score, FWCQ[c("away_team", "date")],mean)
fwcq_goalconceded_h[is.na(fwcq_goalconceded_h)] <- ""
fwcq_goalconceded_a[is.na(fwcq_goalconceded_a)] <- ""

for(fwcq_rowhgc in 1:nrow(fwcq_goalconceded_h)) {
  for(fwcq_colhgc in 1:ncol(fwcq_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(fwcq_rowagc in 1:nrow(fwcq_goalconceded_a)) {
      for(fwcq_colagc in 1:ncol(fwcq_goalconceded_a)) {
        ifelse(!fwcq_goalconceded_a[fwcq_rowagc,fwcq_colagc]=="",fwcq_goalconceded_h[fwcq_rowagc,fwcq_colagc] <- fwcq_goalconceded_a[fwcq_rowagc,fwcq_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fwcq_goalconceded_h,'FWCQ.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
fwcq_form_h <- tapply(FWCQ$FTR, FWCQ[c("home_team", "date")],median)
fwcq_form_a <- tapply(FWCQ$FTR, FWCQ[c("away_team", "date")],median)
fwcq_form_h[is.na(fwcq_form_h)] <- ""
fwcq_form_a[is.na(fwcq_form_a)] <- ""
fwcq_form_h <- sub("A","L",fwcq_form_h)
fwcq_form_h <- sub("H","W",fwcq_form_h)
fwcq_form_a <- sub("A","W",fwcq_form_a)
fwcq_form_a <- sub("H","L",fwcq_form_a)
for(fwcq_rowh_f in 1:nrow(fwcq_form_h)) {
  for(fwcq_colh_f in 1:ncol(fwcq_form_h)) {

    # print(my_matrix[row, col])
    for(fwcq_rowa_f in 1:nrow(fwcq_form_a)) {
      for(fwcq_cola_f in 1:ncol(fwcq_form_a)) {
        ifelse(!fwcq_form_a[fwcq_rowa_f,fwcq_cola_f]=="",fwcq_form_h[fwcq_rowa_f,fwcq_cola_f] <- fwcq_form_a[fwcq_rowa_f,fwcq_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fwcq_form_h,'FWCQ.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
fwcq_totalgoals_h <- tapply(FWCQ$TG, FWCQ[c("home_team", "date")],mean)
fwcq_totalgoals_a <- tapply(FWCQ$TG, FWCQ[c("away_team", "date")],mean)
fwcq_totalgoals_h[is.na(fwcq_totalgoals_h)] <- ""
fwcq_totalgoals_a[is.na(fwcq_totalgoals_a)] <- ""
for(fwcq_rowh in 1:nrow(fwcq_totalgoals_h)) {
  for(fwcq_colh in 1:ncol(fwcq_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(fwcq_rowa in 1:nrow(fwcq_totalgoals_a)) {
      for(fwcq_cola in 1:ncol(fwcq_totalgoals_a)) {
        ifelse(!fwcq_totalgoals_a[fwcq_rowa,fwcq_cola]=="",fwcq_totalgoals_h[fwcq_rowa,fwcq_cola] <- fwcq_totalgoals_a[fwcq_rowa,fwcq_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(fwcq_totalgoals_h,'FWCQ.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
fwcq_form_team_against_h <- tapply(FWCQ$away_team, FWCQ[c("home_team", "date")],median)
fwcq_form_team_against_a <- tapply(FWCQ$home_team, FWCQ[c("away_team", "date")],median)
fwcq_form_team_against_h[is.na(fwcq_form_team_against_h)] <- ""
fwcq_form_team_against_a[is.na(fwcq_form_team_against_a)] <- ""
for(fwcq_rowh_f_against in 1:nrow(fwcq_form_team_against_h)) {
  for(fwcq_colh_f_against in 1:ncol(fwcq_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(fwcq_rowa_f_against in 1:nrow(fwcq_form_team_against_a)) {
      for(fwcq_cola_f_against in 1:ncol(fwcq_form_team_against_a)) {
        ifelse(!fwcq_form_team_against_a[fwcq_rowa_f_against,fwcq_cola_f_against]=="",fwcq_form_team_against_h[fwcq_rowa_f_against,fwcq_cola_f_against] <- fwcq_form_team_against_a[fwcq_rowa_f_against,fwcq_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################
##########Goals over under############
#FWCQ
fwcq_un05_home <- c()
fwcq_un05_away <- c()
fwcq_ov05_home <- c()
fwcq_ov05_away <- c()

fwcq_un15_home <- c()
fwcq_un15_away <- c()
fwcq_ov15_home <- c()
fwcq_ov15_away <- c()

fwcq_un25_home <- c()
fwcq_un25_away <- c()
fwcq_ov25_home <- c()
fwcq_ov25_away <- c()

fwcq_un35_home <- c()
fwcq_un35_away <- c()
fwcq_ov35_home <- c()
fwcq_ov35_away <- c()

fwcq_un45_home <- c()
fwcq_un45_away <- c()
fwcq_ov45_home <- c()
fwcq_ov45_away <- c()

fwcq_un55_home <- c()
fwcq_un55_away <- c()
fwcq_ov55_home <- c()
fwcq_ov55_away <- c()

for (i_fwcq_tg in 1:length(fwcq_teams))
{

  fwcq_un05_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG == 0,])
  fwcq_un05_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG == 0,])

  fwcq_ov05_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG > 0,])
  fwcq_ov05_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG > 0,])

  fwcq_un15_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 1,])
  fwcq_un15_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 1,])

  fwcq_ov15_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 2,])
  fwcq_ov15_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 2,])

  fwcq_un25_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 2,])
  fwcq_un25_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 2,])

  fwcq_ov25_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >=3,])
  fwcq_ov25_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >=3,])

  fwcq_un35_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 3,])
  fwcq_un35_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 3,])

  fwcq_ov35_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 4,])
  fwcq_ov35_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 4,])

  fwcq_un45_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 4,])
  fwcq_un45_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 4,])

  fwcq_ov45_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 5,])
  fwcq_ov45_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 5,])

  fwcq_un55_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 5,])
  fwcq_un55_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG <= 5,])

  fwcq_ov55_home[i_fwcq_tg] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 6,])
  fwcq_ov55_away[i_fwcq_tg] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_tg] & FWCQ$TG >= 6,])


}

fwcq_un05 <- fwcq_un05_home + fwcq_un05_away
fwcq_ov05 <- fwcq_ov05_home + fwcq_ov05_away

fwcq_un15 <- fwcq_un15_home + fwcq_un15_away
fwcq_ov15 <- fwcq_ov15_home + fwcq_ov15_away

fwcq_un25 <- fwcq_un25_home + fwcq_un25_away
fwcq_ov25 <- fwcq_ov25_home + fwcq_ov25_away

fwcq_un35 <- fwcq_un35_home + fwcq_un35_away
fwcq_ov35 <- fwcq_ov35_home + fwcq_ov35_away

fwcq_un45 <- fwcq_un45_home + fwcq_un45_away
fwcq_ov45 <- fwcq_ov45_home + fwcq_ov45_away

fwcq_un55 <- fwcq_un55_home + fwcq_un55_away
fwcq_ov55 <- fwcq_ov55_home + fwcq_ov55_away

fwcq_ovundata <- cbind(fwcq_teams,fwcq_un05,fwcq_ov05,fwcq_un15,fwcq_ov15,fwcq_un25,fwcq_ov25,fwcq_un35,fwcq_ov35,fwcq_un45,fwcq_ov45,fwcq_un55,fwcq_ov55)
write.xlsx(fwcq_ovundata,'FWCQ.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
fwcq_csform_h <- tapply(FWCQ$CS, FWCQ[c("home_team", "date")],median)
fwcq_csform_a <- tapply(FWCQ$CS, FWCQ[c("away_team", "date")],median)

fwcq_csform_h[is.na(fwcq_csform_h)] <- ""
fwcq_csform_a[is.na(fwcq_csform_a)] <- ""

for(fwcq_rowh_f_cs in 1:nrow(fwcq_csform_h)) {
  for(fwcq_colh_f_cs in 1:ncol(fwcq_csform_h)) {

    # print(my_matrix[row, col])
    for(fwcq_rowa_f_cs in 1:nrow(fwcq_csform_a)) {
      for(fwcq_cola_f_cs in 1:ncol(fwcq_csform_a)) {
        ifelse(!fwcq_csform_a[fwcq_rowa_f_cs,fwcq_cola_f_cs]=="",fwcq_csform_h[fwcq_rowa_f_cs,fwcq_cola_f_cs] <- fwcq_csform_a[fwcq_rowa_f_cs,fwcq_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
#home goals scored
fwcq_home_gs <- aggregate(FWCQ$home_score, by = list(FWCQ$home_team), FUN = sum)
fwcq_home_gs_avg <- aggregate(FWCQ$home_score, by = list(FWCQ$home_team),mean)
fwcq_home_scoring <- merge(fwcq_home_gs,fwcq_home_gs_avg, by='Group.1',all = T)
names(fwcq_home_scoring)[names(fwcq_home_scoring) == "x.x"] <- "TFthg"
names(fwcq_home_scoring)[names(fwcq_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
fwcq_away_gs <- aggregate(FWCQ$away_score, by = list(FWCQ$away_team), FUN = sum)
fwcq_away_gs_avg <- aggregate(FWCQ$away_score, by = list(FWCQ$away_team),mean)
fwcq_away_scoring <- merge(fwcq_away_gs,fwcq_away_gs_avg, by='Group.1',all = T)
names(fwcq_away_scoring)[names(fwcq_away_scoring) == "x.x"] <- "TFtag"
names(fwcq_away_scoring)[names(fwcq_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
fwcq_scoring <- merge(fwcq_home_scoring,fwcq_away_scoring,by='Group.1',all = T)
fwcq_scoring$TGS <- fwcq_scoring$TFthg + fwcq_scoring$TFtag

#home goals conceded
fwcq_home_gc <- aggregate(FWCQ$away_score, by = list(FWCQ$home_team), FUN = sum)
fwcq_home_gc_avg <- aggregate(FWCQ$away_score, by = list(FWCQ$home_team),mean)
fwcq_home_conceding <- merge(fwcq_home_gc,fwcq_home_gc_avg, by='Group.1',all = T)
names(fwcq_home_conceding)[names(fwcq_home_conceding) == "x.x"] <- "TFthc"
names(fwcq_home_conceding)[names(fwcq_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
fwcq_away_gc <- aggregate(FWCQ$home_score, by = list(FWCQ$away_team), FUN = sum)
fwcq_away_gc_avg <- aggregate(FWCQ$home_score, by = list(FWCQ$away_team),mean)
fwcq_away_conceding <- merge(fwcq_away_gc,fwcq_away_gc_avg, by='Group.1',all = T)
names(fwcq_away_conceding)[names(fwcq_away_conceding) == "x.x"] <- "TFtac"
names(fwcq_away_conceding)[names(fwcq_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
fwcq_conceding <- merge(fwcq_home_conceding,fwcq_away_conceding,by='Group.1',all = T)
fwcq_conceding$TGC <- fwcq_conceding$TFthc + fwcq_conceding$TFtac


######################################################################################
###########League Table###############################################################

#hwins and away wins
fwcq_home_wins <- c()
fwcq_away_wins <- c()
fwcq_home_draws <- c()
fwcq_away_draws <- c()
fwcq_home_loss <- c()
fwcq_away_loss <- c()



for (i_fwcq_wins in 1:length(fwcq_teams))
{

  fwcq_home_wins[i_fwcq_wins] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_wins] & FWCQ$FTR == "H",])
  fwcq_away_wins[i_fwcq_wins] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_wins] & FWCQ$FTR == "A",])
  fwcq_home_draws[i_fwcq_wins] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_wins] & FWCQ$FTR == "D",])
  fwcq_away_draws[i_fwcq_wins] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_wins] & FWCQ$FTR == "D",])
  fwcq_home_loss[i_fwcq_wins] <- nrow(FWCQ[FWCQ$home_team == fwcq_teams[i_fwcq_wins] & FWCQ$FTR == "A",])
  fwcq_away_loss[i_fwcq_wins] <- nrow(FWCQ[FWCQ$away_team == fwcq_teams[i_fwcq_wins] & FWCQ$FTR == "H",])

}

fwcq_total_wins <- fwcq_home_wins + fwcq_away_wins
fwcq_total_draws <- fwcq_home_draws + fwcq_away_draws
fwcq_total_loss <- fwcq_home_loss + fwcq_away_loss

fwcq_league_table <- cbind(fwcq_teams,fwcq_games_played,fwcq_total_wins,fwcq_total_draws,fwcq_total_loss)
fwcq_GS <- fwcq_scoring$TGS
fwcq_GC <-fwcq_conceding$TGC
fwcq_GD <- fwcq_scoring$TGS - fwcq_conceding$TGC
fwcq_PTS <- (fwcq_total_wins*3) + (fwcq_total_draws*1)
fwcq_league_table <- cbind(fwcq_league_table,fwcq_GS,fwcq_GC,fwcq_GD,fwcq_PTS)
fwcq_league_table <- as.data.frame(fwcq_league_table)
#rename the columns
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_teams"] <- "Team"
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_games_played"] <- "P"
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_total_wins"] <- "W"
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_total_draws"] <- "D"
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_total_loss"] <- "L"
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_GS"] <- "F"
names(fwcq_league_table)[names(fwcq_league_table) == "fwcq_GC"] <- "A"
points_fwcq <- fwcq_league_table[order(fwcq_league_table$fwcq_PTS, decreasing = TRUE),]
write.xlsx(points_fwcq,'FWCQ.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six fwcq###################################################
#FWCQ
#form
#create final_fwcq_hf object
fwcq_last_n_games <- 6
final_fwcq_hf <- c()
for(index_fwcq_hf in 1:length(fwcq_teams))
{
  index_fwcq_hf <- row.names(fwcq_form_h) == fwcq_teams[index_fwcq_hf]
  form_fwcq_hf <- fwcq_form_h[index_fwcq_hf]
  deleted_form_fwcq_hf <- form_fwcq_hf[!form_fwcq_hf[] == ""]
  l6_form_fwcq_hf <- tail(deleted_form_fwcq_hf,fwcq_last_n_games)
  l6_form_fwcq_hf <- paste(l6_form_fwcq_hf,collapse = " ")
  final_fwcq_hf[index_fwcq_hf] <- rbind(paste(fwcq_teams[index_fwcq_hf],l6_form_fwcq_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fwcq_teams[index],l6_form)

}

#change column names
final_fwcq_hf <- as.data.frame(final_fwcq_hf)
colnames(final_fwcq_hf) <- "Form"
#goals scored
#create final_fwcq_gs object
final_fwcq_gs <- c()
suml6_fwcq_gs <- c()
for(index_fwcq_gs in 1:length(fwcq_teams))
{
  index_fwcq_gs <- row.names(fwcq_goalscored_h) == fwcq_teams[index_fwcq_gs]
  form_fwcq_gs <- fwcq_goalscored_h[index_fwcq_gs]
  deleted_form_fwcq_gs <- form_fwcq_gs[!form_fwcq_gs[] == ""]
  l6_form_fwcq_gs <- tail(deleted_form_fwcq_gs,fwcq_last_n_games)
  l6_form_fwcq_gs <- as.numeric(l6_form_fwcq_gs)
  suml6_fwcq_gs[index_fwcq_gs] <- sum(l6_form_fwcq_gs)
  suml6_fwcq_gs[index_fwcq_gs] <- paste("(",suml6_fwcq_gs[index_fwcq_gs],")",sep = "")
  l6_form_fwcq_gs <- paste(l6_form_fwcq_gs,collapse = " ")
  final_fwcq_gs[index_fwcq_gs] <- rbind(paste(fwcq_teams[index_fwcq_gs],l6_form_fwcq_gs,suml6_fwcq_gs[index_fwcq_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fwcq_teams[index],l6_form)

}
final_fwcq_gs
#change column names
final_fwcq_gs <- as.data.frame(final_fwcq_gs)
colnames(final_fwcq_gs) <- "Goals scored"
#goal conceded
#create final_fwcq_gc object
final_fwcq_gc <- c()
suml6_fwcq_gc <- c()
for(index_fwcq_gc in 1:length(fwcq_teams))
{
  index_fwcq_gc <- row.names(fwcq_goalconceded_h) == fwcq_teams[index_fwcq_gc]
  form_fwcq_gc <- fwcq_goalconceded_h[index_fwcq_gc]
  deleted_form_fwcq_gc <- form_fwcq_gc[!form_fwcq_gc[] == ""]
  l6_form_fwcq_gc <- tail(deleted_form_fwcq_gc,fwcq_last_n_games)
  l6_form_fwcq_gc <- as.numeric(l6_form_fwcq_gc)
  suml6_fwcq_gc[index_fwcq_gc] <- sum(l6_form_fwcq_gc)
  suml6_fwcq_gc[index_fwcq_gc] <- paste("(",suml6_fwcq_gc[index_fwcq_gc],")",sep = "")
  l6_form_fwcq_gc <- paste(l6_form_fwcq_gc,collapse = " ")
  final_fwcq_gc[index_fwcq_gc] <- rbind(paste(fwcq_teams[index_fwcq_gc],l6_form_fwcq_gc,suml6_fwcq_gc[index_fwcq_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fwcq_teams[index],l6_form)

}

#change column names
final_fwcq_gc <- as.data.frame(final_fwcq_gc)
colnames(final_fwcq_gc) <- "Goals conceded"
#total goals
#create final_fwcq_tg object
final_fwcq_tg <- c()
suml6_fwcq_tg <- c()
for(index_fwcq_tg in 1:length(fwcq_teams))
{
  index_fwcq_tg <- row.names(fwcq_totalgoals_h) == fwcq_teams[index_fwcq_tg]
  form_fwcq_tg <- fwcq_totalgoals_h[index_fwcq_tg]
  deleted_form_fwcq_tg <- form_fwcq_tg[!form_fwcq_tg[] == ""]
  l6_form_fwcq_tg <- tail(deleted_form_fwcq_tg,fwcq_last_n_games)
  l6_form_fwcq_tg <- as.numeric(l6_form_fwcq_tg)
  suml6_fwcq_tg[index_fwcq_tg] <- sum(l6_form_fwcq_tg)
  suml6_fwcq_tg[index_fwcq_tg] <- paste("(",suml6_fwcq_tg[index_fwcq_tg],")",sep = "")
  l6_form_fwcq_tg <- paste(l6_form_fwcq_tg,collapse = " ")
  final_fwcq_tg[index_fwcq_tg] <- rbind(paste(fwcq_teams[index_fwcq_tg],l6_form_fwcq_tg,suml6_fwcq_tg[index_fwcq_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fwcq_teams[index],l6_form)

}
#change column names
final_fwcq_tg <- as.data.frame(final_fwcq_tg)
colnames(final_fwcq_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_fwcq_hf object
final_fwcq_cs <- c()
for(index_fwcq_cs in 1:length(fwcq_teams))
{
  index_fwcq_cs <- row.names(fwcq_csform_h) == fwcq_teams[index_fwcq_cs]
  csform_fwcq_cs <- fwcq_csform_h[index_fwcq_cs]
  deleted_csform_fwcq_cs <- csform_fwcq_cs[!csform_fwcq_cs[] == ""]
  l6_csform_fwcq_cs <- tail(deleted_csform_fwcq_cs,fwcq_last_n_games)
  l6_csform_fwcq_cs <- paste(l6_csform_fwcq_cs,collapse = " ")
  final_fwcq_cs[index_fwcq_cs] <- rbind(paste(fwcq_teams[index_fwcq_cs],l6_csform_fwcq_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",fwcq_teams[index],l6_csform)

}

#change column names
final_fwcq_cs <- as.data.frame(final_fwcq_cs)
colnames(final_fwcq_cs) <- "CSForm"
#################################################
#Team against
#create final_fwcq_hf_against
final_fwcq_hf_against <- c()
for(index_fwcq_hf_against in 1:length(fwcq_teams))
{
  index_fwcq_hf_against <- row.names(fwcq_form_team_against_h) == fwcq_teams[index_fwcq_hf_against]
  form_fwcq_hf_against <- fwcq_form_team_against_h[index_fwcq_hf_against]
  deleted_form_fwcq_hf_against <- form_fwcq_hf_against[!form_fwcq_hf_against[] == ""]
  l6_form_fwcq_hf_against <- tail(deleted_form_fwcq_hf_against,fwcq_last_n_games)
  l6_form_fwcq_hf_against <- paste(l6_form_fwcq_hf_against,collapse = " ")
  final_fwcq_hf_against[index_fwcq_hf_against] <- rbind(paste(fwcq_teams[index_fwcq_hf_against],l6_form_fwcq_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",fwcq_teams[index],l6_form)

}
final_fwcq_hf_against <- as.data.frame(final_fwcq_hf_against)
colnames(final_fwcq_hf_against) <- "Team against"
#combine the columns
final_fwcq_all <- cbind(final_fwcq_hf,final_fwcq_gs,final_fwcq_gc,final_fwcq_tg,final_fwcq_cs,final_fwcq_hf_against)
write.xlsx(final_fwcq_all,'FWCQ.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
fwcq_GP <- nrow(FWCQ)
#Calculate total home goals for each division
fwcq_T_home_score <- sum(fwcq_home_gs$x)
#calculate average home goal
fwcq_avg_home_score <- round(fwcq_T_home_score /fwcq_GP, digits = 4)
############################################################
#Calculate total away goals for each division
fwcq_T_away_score <- sum(fwcq_away_gs$x)
#calculate average away goal
fwcq_avg_away_score <- round(fwcq_T_away_score /fwcq_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
fwcq_home_as <- round(((fwcq_home_gs$x/fwcq_home_games))/fwcq_avg_home_score, digits = 4)
#calculate away attack strength
fwcq_away_as <- round(((fwcq_away_gs$x/fwcq_away_games))/fwcq_avg_away_score, digits = 4)
################################################################################
#get average home concede and away concede
fwcq_avg_HC <- round(fwcq_T_away_score /fwcq_GP, digits = 4)
#avg away concede
fwcq_avg_AC <- round(fwcq_T_home_score /fwcq_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
fwcq_home_ds <- round(((fwcq_home_gc$x/fwcq_home_games))/fwcq_avg_HC, digits = 4)
#away defense strength
fwcq_away_ds <- round(((fwcq_away_gc$x/fwcq_away_games))/fwcq_avg_AC, digits = 4)
#############################################################################
#home poisson data
#fwcq
fwcq_division <- c()
fwcq_division[1:length(fwcq_teams)] <- "FWCQ"
fwcq_home_poisson <- cbind(fwcq_division,fwcq_teams,fwcq_avg_home_score,fwcq_home_as,fwcq_home_ds)
#################################################################################
#away poisson data
#fwcq
fwcq_division <- c()
fwcq_division[1:length(fwcq_teams)] <- "FWCQ"
fwcq_away_poisson <- cbind(fwcq_division,fwcq_teams,fwcq_avg_away_score,fwcq_away_as,fwcq_away_ds)

#create home and away csv
#fwcq_home_poisson <- rbind(fwcq_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#fwcq_away_poisson <- rbind(fwcq_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(fwcq_home_poisson,'FWCQ.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(fwcq_away_poisson,'FWCQ.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################FWCQ FIXTURES##########################################################################
#FWCQ
home_teamTeam_fwcq <- rep(fwcq_teams, each = length(fwcq_teams))
away_teamTeam_fwcq <- rep(fwcq_teams, length(fwcq_teams))
FWCQ_fixtures <- cbind(home_teamTeam_fwcq,away_teamTeam_fwcq)
FWCQ_fixtures <- as.data.frame(FWCQ_fixtures)
FWCQ_fixtures <- FWCQ_fixtures[!FWCQ_fixtures$home_teamTeam_fwcq == FWCQ_fixtures$away_teamTeam_fwcq,]
rownames(FWCQ_fixtures) <- NULL
FWCQ_fixtures$Div <- "FWCQ"
FWCQ_fixtures <- FWCQ_fixtures[,c(3,1,2)]

FWCQ_fixtures$avg_home_score_fwcq <- fwcq_avg_home_score

FWCQ_fixtures$fwcq_homeas <- rep(fwcq_home_as,each = length(fwcq_teams)-1)

fwcq_awayds_lookup <- cbind(fwcq_teams,fwcq_away_ds)

fwcq_awayds_lookup <- as.data.frame(fwcq_awayds_lookup)

colnames(fwcq_awayds_lookup) <- c("away_teamTeam_fwcq","fwcq_awayds")


require('RH2')
FWCQ_fixtures$fwcq_awayds <- sqldf("SELECT fwcq_awayds_lookup.fwcq_awayds FROM fwcq_awayds_lookup INNER JOIN FWCQ_fixtures ON fwcq_awayds_lookup.away_teamTeam_fwcq = FWCQ_fixtures.away_teamTeam_fwcq")

FWCQ_fixtures$avg_away_score_fwcq <- fwcq_avg_away_score

fwcq_awayas_lookup <- cbind(fwcq_teams,fwcq_away_as)

fwcq_awayas_lookup <- as.data.frame(fwcq_awayas_lookup)

colnames(fwcq_awayas_lookup) <- c("away_teamTeam_fwcq","fwcq_awayas")


FWCQ_fixtures$fwcq_awayas <- sqldf("SELECT fwcq_awayas_lookup.fwcq_awayas FROM fwcq_awayas_lookup INNER JOIN FWCQ_fixtures ON fwcq_awayas_lookup.away_teamTeam_fwcq = FWCQ_fixtures.away_teamTeam_fwcq")

FWCQ_fixtures$fwcq_homeds <- rep(fwcq_home_ds,each = length(fwcq_teams)-1)

FWCQ_fixtures$fwcq_awayds <- as.numeric(unlist(FWCQ_fixtures$fwcq_awayds))
#xGH
FWCQ_fixtures$fwcq_xGH <- FWCQ_fixtures$avg_home_score_fwcq * FWCQ_fixtures$fwcq_homeas * FWCQ_fixtures$fwcq_awayds

#xGA

FWCQ_fixtures$fwcq_awayas <- as.numeric(unlist(FWCQ_fixtures$fwcq_awayas))

FWCQ_fixtures$fwcq_xGA <- FWCQ_fixtures$avg_away_score_fwcq * FWCQ_fixtures$fwcq_awayas * FWCQ_fixtures$fwcq_homeds

FWCQ_fixtures$fwcq_0_0 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_0 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_0_1 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_1 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_0 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_0_2 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_2 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_1 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_2 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_3 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_0 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_1 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_2 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_0_3 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_3 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_3 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_4 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_0 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_1 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_2 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_3 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_0_4 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_4 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_4 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_4 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_5 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_0 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_1 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_2 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_3 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_4 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_0_5 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_5 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_5 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_5 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_5 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_6 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_0 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(0,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_1 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(1,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_2 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(2,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_3 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(3,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_4 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(4,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_6_5 <- round(stats::dpois(6,FWCQ_fixtures$fwcq_xGH) * stats::dpois(5,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_0_6 <- round(stats::dpois(0,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_1_6 <- round(stats::dpois(1,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_2_6 <- round(stats::dpois(2,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_3_6 <- round(stats::dpois(3,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_4_6 <- round(stats::dpois(4,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
FWCQ_fixtures$fwcq_5_6 <- round(stats::dpois(5,FWCQ_fixtures$fwcq_xGH) * stats::dpois(6,FWCQ_fixtures$fwcq_xGA), digits = 4)
#home_team win
FWCQ_fixtures$fwcq_H <- (
  FWCQ_fixtures$fwcq_1_0 + FWCQ_fixtures$fwcq_2_0 + FWCQ_fixtures$fwcq_2_1 + FWCQ_fixtures$fwcq_3_0 + FWCQ_fixtures$fwcq_3_1 +
    FWCQ_fixtures$fwcq_3_2 + FWCQ_fixtures$fwcq_4_0 + FWCQ_fixtures$fwcq_4_1 + FWCQ_fixtures$fwcq_4_2 + FWCQ_fixtures$fwcq_4_3 +
    FWCQ_fixtures$fwcq_5_0 + FWCQ_fixtures$fwcq_5_1 + FWCQ_fixtures$fwcq_5_2 + FWCQ_fixtures$fwcq_5_3 + FWCQ_fixtures$fwcq_5_4 +
    FWCQ_fixtures$fwcq_6_0 + FWCQ_fixtures$fwcq_6_1 + FWCQ_fixtures$fwcq_6_2 + FWCQ_fixtures$fwcq_6_3 + FWCQ_fixtures$fwcq_6_4 +
    FWCQ_fixtures$fwcq_6_5
)

FWCQ_fixtures$fwcq_H <- percent(FWCQ_fixtures$fwcq_H, accuracy = 0.1)

#Draw
FWCQ_fixtures$fwcq_D <- (

  FWCQ_fixtures$fwcq_0_0 + FWCQ_fixtures$fwcq_1_1 + FWCQ_fixtures$fwcq_2_2 + FWCQ_fixtures$fwcq_3_3 + FWCQ_fixtures$fwcq_4_4 +
    FWCQ_fixtures$fwcq_5_5 + FWCQ_fixtures$fwcq_6_6
)

FWCQ_fixtures$fwcq_D <- percent(FWCQ_fixtures$fwcq_D, accuracy = 0.1)

#away_team

FWCQ_fixtures$fwcq_A <- (
  FWCQ_fixtures$fwcq_0_1 + FWCQ_fixtures$fwcq_0_2 + FWCQ_fixtures$fwcq_1_2 + FWCQ_fixtures$fwcq_0_3 + FWCQ_fixtures$fwcq_1_3 +
    FWCQ_fixtures$fwcq_2_3 + FWCQ_fixtures$fwcq_0_4 + FWCQ_fixtures$fwcq_1_4 + FWCQ_fixtures$fwcq_2_4 + FWCQ_fixtures$fwcq_3_4 +
    FWCQ_fixtures$fwcq_0_5 + FWCQ_fixtures$fwcq_1_5 + FWCQ_fixtures$fwcq_2_5 + FWCQ_fixtures$fwcq_3_5 + FWCQ_fixtures$fwcq_4_5 +
    FWCQ_fixtures$fwcq_0_6 + FWCQ_fixtures$fwcq_1_6 + FWCQ_fixtures$fwcq_2_6 + FWCQ_fixtures$fwcq_3_6 + FWCQ_fixtures$fwcq_4_6 +
    FWCQ_fixtures$fwcq_5_6
)

FWCQ_fixtures$fwcq_A <- percent(FWCQ_fixtures$fwcq_A, accuracy = 0.1)

#ov25
FWCQ_fixtures$fwcq_ov25 <- (
  FWCQ_fixtures$fwcq_2_1 + FWCQ_fixtures$fwcq_1_2 + FWCQ_fixtures$fwcq_2_2 + FWCQ_fixtures$fwcq_3_0 + FWCQ_fixtures$fwcq_3_1 +
    FWCQ_fixtures$fwcq_3_2 + FWCQ_fixtures$fwcq_0_3 + FWCQ_fixtures$fwcq_1_3 + FWCQ_fixtures$fwcq_2_3 + FWCQ_fixtures$fwcq_3_3 +
    FWCQ_fixtures$fwcq_4_0 + FWCQ_fixtures$fwcq_4_1 + FWCQ_fixtures$fwcq_4_2 + FWCQ_fixtures$fwcq_4_3 + FWCQ_fixtures$fwcq_0_4 +
    FWCQ_fixtures$fwcq_1_4 + FWCQ_fixtures$fwcq_2_4 + FWCQ_fixtures$fwcq_3_4 + FWCQ_fixtures$fwcq_4_4 + FWCQ_fixtures$fwcq_5_0 +
    FWCQ_fixtures$fwcq_5_1 + FWCQ_fixtures$fwcq_5_2 + FWCQ_fixtures$fwcq_5_3 + FWCQ_fixtures$fwcq_5_4 + FWCQ_fixtures$fwcq_0_5 +
    FWCQ_fixtures$fwcq_1_5 + FWCQ_fixtures$fwcq_2_5 + FWCQ_fixtures$fwcq_3_5 + FWCQ_fixtures$fwcq_4_5 + FWCQ_fixtures$fwcq_5_5 +
    FWCQ_fixtures$fwcq_6_0 + FWCQ_fixtures$fwcq_6_1 + FWCQ_fixtures$fwcq_6_2 + FWCQ_fixtures$fwcq_6_3 + FWCQ_fixtures$fwcq_6_4 +
    FWCQ_fixtures$fwcq_6_5 + FWCQ_fixtures$fwcq_0_6 + FWCQ_fixtures$fwcq_1_6 + FWCQ_fixtures$fwcq_2_6 + FWCQ_fixtures$fwcq_3_6 +
    FWCQ_fixtures$fwcq_4_6 + FWCQ_fixtures$fwcq_5_6 + FWCQ_fixtures$fwcq_6_6
)
#un25
FWCQ_fixtures$fwcq_un25 <- (
  FWCQ_fixtures$fwcq_0_0 + FWCQ_fixtures$fwcq_1_0 + FWCQ_fixtures$fwcq_0_1 + FWCQ_fixtures$fwcq_1_1 + FWCQ_fixtures$fwcq_2_0 + FWCQ_fixtures$fwcq_0_2
)
#odds
FWCQ_fixtures$fwcq_ov25_odds <- round((1/FWCQ_fixtures$fwcq_ov25),digits = 2)
FWCQ_fixtures$fwcq_un25_odds <- round((1/FWCQ_fixtures$fwcq_un25),digits = 2)

FWCQ_fixtures$fwcq_ov25_odds
FWCQ_fixtures$fwcq_un25_odds
#percentages
FWCQ_fixtures$fwcq_ov25 <- percent(FWCQ_fixtures$fwcq_ov25, accuracy = 0.1)

FWCQ_fixtures$fwcq_un25 <- percent(FWCQ_fixtures$fwcq_un25, accuracy = 0.1)
FWCQ_fixtures$fwcq_pscore <- paste(round(FWCQ_fixtures$fwcq_xGH,digits = 0),round(FWCQ_fixtures$fwcq_xGA,digits = 0),sep = "-")
#write out
write.xlsx(FWCQ_fixtures,'FWCQ.xlsx',sheetName = "FWCQ", append = TRUE)
###########################################################################################################
########################FWCQ END###########################################################################
# FWCQ <- read.csv('../../../Leonard.000/Downloads/IFootball/results.csv')
# FWCQ$FTR <- with(FWCQ,
#                  ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
# )
# FWCQ$TG <- FWCQ$home_score + FWCQ$away_score
# FWCQ$OV25 <- ifelse(FWCQ$TG >= 3,"Y","N")
# fwcq_ftr_summary <- tabyl(FWCQ,tournament,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
# fwcq_ov25_summary <- tabyl(FWCQ,tournament,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
# ftr_summary <- ftr_summary[,c(1,3,2)]
# write.xlsx(fwcq_ftr_summary,'FWCQ.xlsx',sheetName = "FTR", append = TRUE)
# write.xlsx(fwcq_ov25_summary,'FWCQ.xlsx',sheetName = "OVUN25", append = TRUE)
#
#
