library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('pinnacle.data')
library('odds.converter')
library('sqldf')



#Full time results percentages
ftr_summary <- tabyl(allteams20202021,Div,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,4,3,2)]
#Half time results percentages
htr_summary <- tabyl(allteams20202021,Div,HTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
htr_summary <- htr_summary[,c(1,4,3,2)]



myodds <- readxl::read_excel('../FDAS/myodds.xlsx', sheet = '2way')

##################################################
fixtures <- read.csv('../FDAS/fixtures.csv')
fixtures$Date <- dmy(fixtures$Date)
fixtures <- fixtures[order(as.Date(fixtures$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#create true odds calc
true_odds_calc_2way <- c()
#true_odds_calc_2way <- c('B_ov25','B_und25','Margin','F_ov25','F_un25','T_ov25prob','T_un25prob','HT','AT','Div','Date')
true_odds_calc_2way$B_ov25 <- fixtures$P.2.5
true_odds_calc_2way$B_und25 <- fixtures$P.2.5.1
true_odds_calc_2way$Margin <- percent(((1/true_odds_calc_2way$B_ov25) + (1/true_odds_calc_2way$B_und25)-1), accuracy = 0.01)
true_odds_calc_2way$F_ov25 <- round(true_odds_calc_2way$B_ov25 * (1 + ((1/true_odds_calc_2way$B_ov25) + (1/true_odds_calc_2way$B_und25)-1)), digits = 2)
true_odds_calc_2way$F_un25 <- round(true_odds_calc_2way$B_und25 * (1 + ((1/true_odds_calc_2way$B_ov25) + (1/true_odds_calc_2way$B_und25)-1)), digits = 2)
true_odds_calc_2way$T_ov25prob <- percent(odds.dec2prob(true_odds_calc_2way$F_ov25), accuracy = 0.01)
true_odds_calc_2way$T_un25prob <- percent(odds.dec2prob(true_odds_calc_2way$F_un25), accuracy = 0.01)
true_odds_calc_2way$HT <- fixtures$HomeTeam
true_odds_calc_2way$AT <- fixtures$AwayTeam
true_odds_calc_2way$Div <- fixtures$Div
true_odds_calc_2way$Date <- fixtures$Date
true_odds_calc_2way <- as.data.frame(true_odds_calc_2way)
#create true odds 3 way
true_odds_calc_3way <- c()
#true_odds_calc_3way <- c('B_ov25','B_und25','Margin','F_ov25','F_un25','T_ov25prob','T_un25prob','HT','AT','Div','Date')
true_odds_calc_3way$B_H <- fixtures$PSH
true_odds_calc_3way$B_D <- fixtures$PSD
true_odds_calc_3way$B_A <- fixtures$PSA

true_odds_calc_3way$Margin <- percent(((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1), accuracy = 0.01)
true_odds_calc_3way$F_H <- round(true_odds_calc_3way$B_H * (1 + ((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1)), digits = 2)
true_odds_calc_3way$F_D <- round(true_odds_calc_3way$B_D * (1 + ((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1)), digits = 2)
true_odds_calc_3way$F_A <- round(true_odds_calc_3way$B_A * (1 + ((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1)), digits = 2)


true_odds_calc_3way$F_Hprob <- percent(odds.dec2prob(true_odds_calc_3way$F_H), accuracy = 0.01)
true_odds_calc_3way$F_Dprob <- percent(odds.dec2prob(true_odds_calc_3way$F_D), accuracy = 0.01)
true_odds_calc_3way$F_Aprob <- percent(odds.dec2prob(true_odds_calc_3way$F_A), accuracy = 0.01)

true_odds_calc_3way$HT <- fixtures$HomeTeam
true_odds_calc_3way$AT <- fixtures$AwayTeam
true_odds_calc_3way$Div <- fixtures$Div
true_odds_calc_3way$Date <- fixtures$Date
true_odds_calc_3way <- as.data.frame(true_odds_calc_3way)

x <- stats::dpois(0,1.1812)
y <- stats::dpois(0,1.4660)

x * y

#####################################################################

EURO <- read.csv('../../../Leonard/Downloads/results.csv')
library('lubridate')
EURO$date <- ymd(EURO$date)
EURO <- EURO[order(as.Date(EURO$date, format = "%Y/%m%d"), decreasing = FALSE),]
EURO_qualificaton <- subset(EURO,tournament == "UEFA Euro qualification")
EURO <- subset(EURO,tournament == "UEFA Euro")
EURO <- EURO[EURO$date > '2008-01-01',]
EURO$TG <- EURO$home_score + EURO$away_score
EURO$OV25 <- ifelse(EURO$TG >= 3,"Y","N")
EURO$FTR <- with(EURO,
     ifelse(home_score > away_score ,FTR <- "H" , ifelse(away_score > home_score,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
euro_totalgoalsv2 <- tapply(EURO$TG, EURO[c("home_team", "away_team")],mean)
euro_hgtotals <- rowSums(euro_totalgoalsv2, na.rm = T)
euro_agtotals <- colSums(euro_totalgoalsv2, na.rm = T)
euro_totalgoals <- euro_hgtotals + euro_agtotals
euro_totalgoalsv2 <- cbind(euro_totalgoalsv2,euro_totalgoals)
euro_teams <- sort(unique(EURO$home_team))
euro_home_games <- c()
euro_away_games <-c()
for (i_euro in 1:length(euro_teams))
{

  euro_home_games[i_euro] <- nrow(EURO[EURO$home_team == euro_teams[i_euro],])
  euro_away_games[i_euro]  <- nrow(EURO[EURO$away_team == euro_teams[i_euro],])

}
euro_games_played <- euro_home_games + euro_away_games
euro_goaltotalsv2 <- cbind(euro_totalgoalsv2,euro_games_played)
euro_avg_totalgoals <- round((euro_totalgoals/ euro_games_played), digits = 4)
euro_goaltotalsv2[is.na(euro_goaltotalsv2)] <- ""
euro_goaltotalsv2 <- cbind(euro_goaltotalsv2,euro_avg_totalgoals)
write.xlsx(euro_goaltotalsv2,'EURO.xlsx',sheetName = "totalgoalsv2")
############################################
####GSmatrix################################
#create home and away matrices
euro_goalscored_h <- tapply(EURO$home_score, EURO[c("home_team", "date")],mean)
euro_goalscored_a <- tapply(EURO$away_score, EURO[c("away_team", "date")],mean)
euro_goalscored_h[is.na(euro_goalscored_h)] <- ""
euro_goalscored_a[is.na(euro_goalscored_a)] <- ""

for(euro_rowhgs in 1:nrow(euro_goalscored_h)) {
  for(euro_colhgs in 1:ncol(euro_goalscored_h)) {

    # print(my_matrix[row, col])
    for(euro_rowags in 1:nrow(euro_goalscored_a)) {
      for(euro_colags in 1:ncol(euro_goalscored_a)) {
        ifelse(!euro_goalscored_a[euro_rowags,euro_colags]=="",euro_goalscored_h[euro_rowags,euro_colags] <- euro_goalscored_a[euro_rowags,euro_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(euro_goalscored_h,'EURO.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
####GCmatrix################################
#create home and away matrices
euro_goalconceded_h <- tapply(EURO$away_score, EURO[c("home_team", "date")],mean)
euro_goalconceded_a <- tapply(EURO$home_score, EURO[c("away_team", "date")],mean)
euro_goalconceded_h[is.na(euro_goalconceded_h)] <- ""
euro_goalconceded_a[is.na(euro_goalconceded_a)] <- ""

for(euro_rowhgs in 1:nrow(euro_goalconceded_h)) {
  for(euro_colhgs in 1:ncol(euro_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(euro_rowags in 1:nrow(euro_goalconceded_a)) {
      for(euro_colags in 1:ncol(euro_goalconceded_a)) {
        ifelse(!euro_goalconceded_a[euro_rowags,euro_colags]=="",euro_goalconceded_h[euro_rowags,euro_colags] <- euro_goalconceded_a[euro_rowags,euro_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(euro_goalconceded_h,'EURO.xlsx',sheetName = "gcmatrix", append = TRUE)
#########################################################################################
####Teamform################################
euro_form_h <- tapply(EURO$FTR, EURO[c("home_team", "date")],median)
euro_form_a <- tapply(EURO$FTR, EURO[c("away_team", "date")],median)
euro_form_h[is.na(euro_form_h)] <- ""
euro_form_a[is.na(euro_form_a)] <- ""
euro_form_h <- sub("A","L",euro_form_h)
euro_form_h <- sub("H","W",euro_form_h)
euro_form_a <- sub("A","W",euro_form_a)
euro_form_a <- sub("H","L",euro_form_a)
for(euro_rowh_f in 1:nrow(euro_form_h)) {
  for(euro_colh_f in 1:ncol(euro_form_h)) {

    # print(my_matrix[row, col])
    for(euro_rowa_f in 1:nrow(euro_form_a)) {
      for(euro_cola_f in 1:ncol(euro_form_a)) {
        ifelse(!euro_form_a[euro_rowa_f,euro_cola_f]=="",euro_form_h[euro_rowa_f,euro_cola_f] <- euro_form_a[euro_rowa_f,euro_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(euro_form_h,'EURO.xlsx',sheetName = "form", append = TRUE)
##################################################################################
#######TGMatrix##################################################################
euro_totalgoals_h <- tapply(EURO$TG, EURO[c("home_team", "date")],mean)
euro_totalgoals_a <- tapply(EURO$TG, EURO[c("away_team", "date")],mean)
euro_totalgoals_h[is.na(euro_totalgoals_h)] <- ""
euro_totalgoals_a[is.na(euro_totalgoals_a)] <- ""
for(euro_rowh in 1:nrow(euro_totalgoals_h)) {
  for(euro_colh in 1:ncol(euro_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(euro_rowa in 1:nrow(euro_totalgoals_a)) {
      for(euro_cola in 1:ncol(euro_totalgoals_a)) {
        ifelse(!euro_totalgoals_a[euro_rowa,euro_cola]=="",euro_totalgoals_h[euro_rowa,euro_cola] <- euro_totalgoals_a[euro_rowa,euro_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(d1_totalgoals_h,'EURO.xlsx',sheetName = "tgmatrix", append = TRUE)
##################################################################################
#######TeamAgainst##################################################################
euro_form_team_against_h <- tapply(EURO$away_team, EURO[c("home_team", "date")],median)
euro_form_team_against_a <- tapply(EURO$home_team, EURO[c("away_team", "date")],median)
euro_form_team_against_h[is.na(euro_form_team_against_h)] <- ""
euro_form_team_against_a[is.na(euro_form_team_against_a)] <- ""
for(euro_rowh_f_against in 1:nrow(euro_form_team_against_h)) {
  for(euro_colh_f_against in 1:ncol(euro_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(euro_rowa_f_against in 1:nrow(euro_form_team_against_a)) {
      for(euro_cola_f_against in 1:ncol(euro_form_team_against_a)) {
        ifelse(!euro_form_team_against_a[euro_rowa_f_against,euro_cola_f_against]=="",euro_form_team_against_h[euro_rowa_f_against,euro_cola_f_against] <- euro_form_team_against_a[euro_rowa_f_against,euro_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
write.xlsx(euro_totalgoals_h,'EURO.xlsx',sheetName = "teamagainst", append = TRUE)
###########################################################################################
############Scoring and conceding analysis
#home goals scored
euro_home_gs <- aggregate(EURO$home_score, by = list(EURO$home_team), FUN = sum)
euro_home_gs_avg <- aggregate(EURO$home_score, by = list(EURO$home_team),mean)
euro_home_scoring <- merge(euro_home_gs,euro_home_gs_avg, by='Group.1',all = T)
names(euro_home_scoring)[names(euro_home_scoring) == "x.x"] <- "TFthg"
names(euro_home_scoring)[names(euro_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
euro_away_gs <- aggregate(EURO$away_score, by = list(EURO$away_team), FUN = sum)
euro_away_gs_avg <- aggregate(EURO$away_score, by = list(EURO$away_team),mean)
euro_away_scoring <- merge(euro_away_gs,euro_away_gs_avg, by='Group.1',all = T)
names(euro_away_scoring)[names(euro_away_scoring) == "x.x"] <- "TFtag"
names(euro_away_scoring)[names(euro_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
euro_scoring <- merge(euro_home_scoring,euro_away_scoring,by='Group.1',all = T)
euro_scoring$TGS <- euro_scoring$TFthg + euro_scoring$TFtag

#Home shots on target
euro_home_hst <- aggregate(EURO$HST, by = list(EURO$home_team), FUN = sum)
euro_away_ast <- aggregate(EURO$AST, by = list(EURO$away_team), FUN = sum)
euro_tst <- merge(euro_home_hst,euro_away_ast, by='Group.1',all = T)
names(euro_tst)[names(euro_tst) == "x.x"] <- "hst"
names(euro_tst)[names(euro_tst) == "x.y"] <- "ast"
euro_tst$TST <- euro_tst$hst + euro_tst$ast
#merge goals scored and shots on target
euro_scoring_conversion <- merge(euro_tst,euro_scoring,by='Group.1',all = T)
#add HSC ASC TSC
euro_scoring_conversion$HSTC <- percent(euro_scoring_conversion$TFthg/euro_scoring_conversion$hst, accuracy = 0.01)
euro_scoring_conversion$ASTC <- percent(euro_scoring_conversion$TFtag/euro_scoring_conversion$ast, accuracy = 0.01)
euro_scoring_conversion$TSTC <- percent(euro_scoring_conversion$TGS/euro_scoring_conversion$TST, accuracy = 0.01)
#merge games played
euro_scoring_conversion <- cbind(euro_scoring_conversion,euro_games_played)
#create the second part
#home goals conceded
euro_home_gc <- aggregate(EURO$away_score, by = list(EURO$home_team), FUN = sum)
euro_home_gc_avg <- aggregate(EURO$away_score, by = list(EURO$home_team),mean)
euro_home_conceding <- merge(euro_home_gc,euro_home_gc_avg, by='Group.1',all = T)
names(euro_home_conceding)[names(euro_home_conceding) == "x.x"] <- "TFthc"
names(euro_home_conceding)[names(euro_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
euro_away_gc <- aggregate(EURO$home_score, by = list(EURO$away_team), FUN = sum)
euro_away_gc_avg <- aggregate(EURO$home_score, by = list(EURO$away_team),mean)
euro_away_conceding <- merge(euro_away_gc,euro_away_gc_avg, by='Group.1',all = T)
names(euro_away_conceding)[names(euro_away_conceding) == "x.x"] <- "TFtac"
names(euro_away_conceding)[names(euro_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
euro_conceding <- merge(euro_home_conceding,euro_away_conceding,by='Group.1',all = T)
euro_conceding$TGC <- euro_conceding$TFthc + euro_conceding$TFtac

#Home shots conceded
euro_home_hsc <- aggregate(EURO$AST, by = list(EURO$home_team), FUN = sum)
euro_away_asc <- aggregate(EURO$HST, by = list(EURO$away_team), FUN = sum)
euro_tsc <- merge(euro_home_hsc,euro_away_asc, by='Group.1',all = T)
names(euro_tsc)[names(euro_tsc) == "x.x"] <- "hsc"
names(euro_tsc)[names(euro_tsc) == "x.y"] <- "asc"
euro_tsc$TSC <- euro_tsc$hsc + euro_tsc$asc
#merge goals conceded and shots conceded
euro_conceding_conversion <- merge(euro_tsc,euro_conceding,by='Group.1',all = T)

#add HSC ASC TSC
euro_conceding_conversion$HSCC <- percent(euro_conceding_conversion$TFthc/euro_conceding_conversion$hsc, accuracy = 0.01)
euro_conceding_conversion$ASCC <- percent(euro_conceding_conversion$TFtac/euro_conceding_conversion$asc, accuracy = 0.01)
euro_conceding_conversion$TSCC <- percent(euro_conceding_conversion$TGC/euro_conceding_conversion$TSC, accuracy = 0.01)

#merge the two parts
euro_shots_analysis <- merge(euro_scoring_conversion,euro_conceding_conversion,by='Group.1',all = T)

######################################################################################
###########League Table###############################################################

#hwins and away wins
euro_home_wins <- c()
euro_away_wins <- c()
euro_home_draws <- c()
euro_away_draws <- c()
euro_home_loss <- c()
euro_away_loss <- c()



for (i_euro_wins in 1:length(euro_teams))
{

  euro_home_wins[i_euro_wins] <- nrow(EURO[EURO$home_team == euro_teams[i_euro_wins] & EURO$FTR == "H",])
  euro_away_wins[i_euro_wins] <- nrow(EURO[EURO$away_team == euro_teams[i_euro_wins] & EURO$FTR == "A",])
  euro_home_draws[i_euro_wins] <- nrow(EURO[EURO$home_team == euro_teams[i_euro_wins] & EURO$FTR == "D",])
  euro_away_draws[i_euro_wins] <- nrow(EURO[EURO$away_team == euro_teams[i_euro_wins] & EURO$FTR == "D",])
  euro_home_loss[i_euro_wins] <- nrow(EURO[EURO$home_team == euro_teams[i_euro_wins] & EURO$FTR == "A",])
  euro_away_loss[i_euro_wins] <- nrow(EURO[EURO$away_team == euro_teams[i_euro_wins] & EURO$FTR == "H",])

}

euro_total_wins <- euro_home_wins + euro_away_wins
euro_total_draws <- euro_home_draws + euro_away_draws
euro_total_loss <- euro_home_loss + euro_away_loss

euro_league_table <- cbind(euro_teams,euro_games_played,euro_total_wins,euro_total_draws,euro_total_loss)
euro_GS <- euro_scoring$TGS
euro_GC <-euro_conceding$TGC
euro_GD <- euro_scoring$TGS - euro_conceding$TGC
euro_PTS <- (euro_total_wins*3) + (euro_total_draws*1)
euro_league_table <- cbind(euro_league_table,euro_GS,euro_GC,euro_GD,euro_PTS)
euro_league_table <- as.data.frame(euro_league_table)
#rename the columns
names(euro_league_table)[names(euro_league_table) == "euro_teams"] <- "Team"
names(euro_league_table)[names(euro_league_table) == "euro_games_played"] <- "P"
names(euro_league_table)[names(euro_league_table) == "euro_total_wins"] <- "W"
names(euro_league_table)[names(euro_league_table) == "euro_total_draws"] <- "D"
names(euro_league_table)[names(euro_league_table) == "euro_total_loss"] <- "L"
names(euro_league_table)[names(euro_league_table) == "euro_GS"] <- "F"
names(euro_league_table)[names(euro_league_table) == "euro_GC"] <- "A"
points_euro <- euro_league_table[order(euro_league_table$euro_PTS, decreasing = TRUE),]
row.names(points_euro) <- 1:length(euro_teams)








