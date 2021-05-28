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
######################EURO START#######################################
#####################################################################
EURO <- read.csv('../../../Leonard.000/Downloads/IFootball/results.csv')
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

for(euro_rowhgc in 1:nrow(euro_goalconceded_h)) {
  for(euro_colhgc in 1:ncol(euro_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(euro_rowagc in 1:nrow(euro_goalconceded_a)) {
      for(euro_colagc in 1:ncol(euro_goalconceded_a)) {
        ifelse(!euro_goalconceded_a[euro_rowagc,euro_colagc]=="",euro_goalconceded_h[euro_rowagc,euro_colagc] <- euro_goalconceded_a[euro_rowagc,euro_colagc],next)
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
write.xlsx(points_euro,'EURO.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six euro###################################################
#EURO
#form
#create final_euro_hf object
final_euro_hf <- c()
for(index_euro_hf in 1:length(euro_teams))
{
  index_euro_hf <- row.names(euro_form_h) == euro_teams[index_euro_hf]
  form_euro_hf <- euro_form_h[index_euro_hf]
  deleted_form_euro_hf <- form_euro_hf[!form_euro_hf[] == ""]
  l6_form_euro_hf <- tail(deleted_form_euro_hf,6)
  l6_form_euro_hf <- paste(l6_form_euro_hf,collapse = " ")
  final_euro_hf[index_euro_hf] <- rbind(paste(euro_teams[index_euro_hf],l6_form_euro_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",euro_teams[index],l6_form)

}

#change column names
final_euro_hf <- as.data.frame(final_euro_hf)
colnames(final_euro_hf) <- "Form"
#goals scored
#create final_euro_gs object
final_euro_gs <- c()
suml6_euro_gs <- c()
for(index_euro_gs in 1:length(euro_teams))
{
  index_euro_gs <- row.names(euro_goalscored_h) == euro_teams[index_euro_gs]
  form_euro_gs <- euro_goalscored_h[index_euro_gs]
  deleted_form_euro_gs <- form_euro_gs[!form_euro_gs[] == ""]
  l6_form_euro_gs <- tail(deleted_form_euro_gs,6)
  l6_form_euro_gs <- as.numeric(l6_form_euro_gs)
  suml6_euro_gs[index_euro_gs] <- sum(l6_form_euro_gs)
  suml6_euro_gs[index_euro_gs] <- paste("(",suml6_euro_gs[index_euro_gs],")",sep = "")
  l6_form_euro_gs <- paste(l6_form_euro_gs,collapse = " ")
  final_euro_gs[index_euro_gs] <- rbind(paste(euro_teams[index_euro_gs],l6_form_euro_gs,suml6_euro_gs[index_euro_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",euro_teams[index],l6_form)

}
final_euro_gs
#change column names
final_euro_gs <- as.data.frame(final_euro_gs)
colnames(final_euro_gs) <- "Goals scored"
#goal conceded
#create final_euro_gc object
final_euro_gc <- c()
suml6_euro_gc <- c()
for(index_euro_gc in 1:length(euro_teams))
{
  index_euro_gc <- row.names(euro_goalconceded_h) == euro_teams[index_euro_gc]
  form_euro_gc <- euro_goalconceded_h[index_euro_gc]
  deleted_form_euro_gc <- form_euro_gc[!form_euro_gc[] == ""]
  l6_form_euro_gc <- tail(deleted_form_euro_gc,6)
  l6_form_euro_gc <- as.numeric(l6_form_euro_gc)
  suml6_euro_gc[index_euro_gc] <- sum(l6_form_euro_gc)
  suml6_euro_gc[index_euro_gc] <- paste("(",suml6_euro_gc[index_euro_gc],")",sep = "")
  l6_form_euro_gc <- paste(l6_form_euro_gc,collapse = " ")
  final_euro_gc[index_euro_gc] <- rbind(paste(euro_teams[index_euro_gc],l6_form_euro_gc,suml6_euro_gc[index_euro_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",euro_teams[index],l6_form)

}

#change column names
final_euro_gc <- as.data.frame(final_euro_gc)
colnames(final_euro_gc) <- "Goals conceded"
#total goals
#create final_euro_tg object
final_euro_tg <- c()
suml6_euro_tg <- c()
for(index_euro_tg in 1:length(euro_teams))
{
  index_euro_tg <- row.names(euro_totalgoals_h) == euro_teams[index_euro_tg]
  form_euro_tg <- euro_totalgoals_h[index_euro_tg]
  deleted_form_euro_tg <- form_euro_tg[!form_euro_tg[] == ""]
  l6_form_euro_tg <- tail(deleted_form_euro_tg,6)
  l6_form_euro_tg <- as.numeric(l6_form_euro_tg)
  suml6_euro_tg[index_euro_tg] <- sum(l6_form_euro_tg)
  suml6_euro_tg[index_euro_tg] <- paste("(",suml6_euro_tg[index_euro_tg],")",sep = "")
  l6_form_euro_tg <- paste(l6_form_euro_tg,collapse = " ")
  final_euro_tg[index_euro_tg] <- rbind(paste(euro_teams[index_euro_tg],l6_form_euro_tg,suml6_euro_tg[index_euro_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",euro_teams[index],l6_form)

}
#change column names
final_euro_tg <- as.data.frame(final_euro_tg)
colnames(final_euro_tg) <- "Total Goals"
#Team against
#create final_euro_hf_against
final_euro_hf_against <- c()
for(index_euro_hf_against in 1:length(euro_teams))
{
  index_euro_hf_against <- row.names(euro_form_team_against_h) == euro_teams[index_euro_hf_against]
  form_euro_hf_against <- euro_form_team_against_h[index_euro_hf_against]
  deleted_form_euro_hf_against <- form_euro_hf_against[!form_euro_hf_against[] == ""]
  l6_form_euro_hf_against <- tail(deleted_form_euro_hf_against,6)
  l6_form_euro_hf_against <- paste(l6_form_euro_hf_against,collapse = " ")
  final_euro_hf_against[index_euro_hf_against] <- rbind(paste(euro_teams[index_euro_hf_against],l6_form_euro_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",euro_teams[index],l6_form)

}
final_euro_hf_against <- as.data.frame(final_euro_hf_against)
colnames(final_euro_hf_against) <- "Team against"
#combine the columns
final_euro_all <- cbind(final_euro_hf,final_euro_gs,final_euro_gc,final_euro_tg,final_euro_hf_against)
write.xlsx(final_euro_all,'EURO.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
#poisson model
#get total games played
euro_GP <- nrow(EURO)
#Calculate total home goals for each division
euro_T_HG <- sum(euro_home_gs$x)
#calculate average home goal
euro_avg_HG <- round(euro_T_HG /euro_GP, digits = 4)
############################################################
#Calculate total away goals for each division
euro_T_AG <- sum(euro_away_gs$x)
#calculate average away goal
euro_avg_AG <- round(euro_T_AG /euro_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
euro_home_as <- round(((euro_home_gs$x/euro_home_games))/euro_avg_HG, digits = 4)
#calculate away attack strength
euro_away_as <- round(((euro_away_gs$x/euro_away_games))/euro_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
euro_avg_HC <- round(euro_T_AG /euro_GP, digits = 4)
#avg away concede
euro_avg_AC <- round(euro_T_HG /euro_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
euro_home_ds <- round(((euro_home_gc$x/euro_home_games))/euro_avg_HC, digits = 4)
#away defense strength
euro_away_ds <- round(((euro_away_gc$x/euro_away_games))/euro_avg_AC, digits = 4)
#############################################################################
#home poisson data
#euro
euro_division <- c()
euro_division[1:length(euro_teams)] <- "EURO"
euro_home_poisson <- cbind(euro_division,euro_teams,euro_avg_HG,euro_home_as,euro_home_ds)
#################################################################################
#away poisson data
#euro
euro_division <- c()
euro_division[1:length(euro_teams)] <- "EURO"
euro_away_poisson <- cbind(euro_division,euro_teams,euro_avg_AG,euro_away_as,euro_away_ds)

#create home and away csv
#euro_home_poisson <- rbind(euro_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#euro_away_poisson <- rbind(euro_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(euro_home_poisson,'EURO.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(euro_away_poisson,'EURO.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################EURO FIXTURES##########################################################################
#EURO
HomeTeam_euro <- rep(euro_teams, each = length(euro_teams))
AwayTeam_euro <- rep(euro_teams, length(euro_teams))
EURO_fixtures <- cbind(HomeTeam_euro,AwayTeam_euro)
EURO_fixtures <- as.data.frame(EURO_fixtures)
EURO_fixtures <- EURO_fixtures[!EURO_fixtures$HomeTeam_euro == EURO_fixtures$AwayTeam_euro,]
rownames(EURO_fixtures) <- NULL
EURO_fixtures$Div <- "EURO"
EURO_fixtures <- EURO_fixtures[,c(3,1,2)]

EURO_fixtures$avg_HG_euro <- euro_avg_HG

EURO_fixtures$euro_homeas <- rep(euro_home_as,each = length(euro_teams)-1)

euro_awayds_lookup <- cbind(euro_teams,euro_away_ds)

euro_awayds_lookup <- as.data.frame(euro_awayds_lookup)

colnames(euro_awayds_lookup) <- c("AwayTeam_euro","euro_awayds")


require('RH2')
EURO_fixtures$euro_awayds <- sqldf("SELECT euro_awayds_lookup.euro_awayds FROM euro_awayds_lookup INNER JOIN EURO_fixtures ON euro_awayds_lookup.AwayTeam_euro = EURO_fixtures.AwayTeam_euro")

EURO_fixtures$avg_AG_euro <- euro_avg_AG

euro_awayas_lookup <- cbind(euro_teams,euro_away_as)

euro_awayas_lookup <- as.data.frame(euro_awayas_lookup)

colnames(euro_awayas_lookup) <- c("AwayTeam_euro","euro_awayas")


EURO_fixtures$euro_awayas <- sqldf("SELECT euro_awayas_lookup.euro_awayas FROM euro_awayas_lookup INNER JOIN EURO_fixtures ON euro_awayas_lookup.AwayTeam_euro = EURO_fixtures.AwayTeam_euro")

EURO_fixtures$euro_homeds <- rep(euro_home_ds,each = length(euro_teams)-1)

EURO_fixtures$euro_awayds <- as.numeric(unlist(EURO_fixtures$euro_awayds))
#xGH
EURO_fixtures$euro_xGH <- EURO_fixtures$avg_HG_euro * EURO_fixtures$euro_homeas * EURO_fixtures$euro_awayds

#xGA

EURO_fixtures$euro_awayas <- as.numeric(unlist(EURO_fixtures$euro_awayas))

EURO_fixtures$euro_xGA <- EURO_fixtures$avg_AG_euro * EURO_fixtures$euro_awayas * EURO_fixtures$euro_homeds

EURO_fixtures$euro_0_0 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_0 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_0_1 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_1 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_0 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_0_2 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_2 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_1 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_2 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_3 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_0 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_1 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_2 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_0_3 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_3 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_3 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_4 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_0 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_1 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_2 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_3 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_0_4 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_4 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_4 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_4 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_5 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_0 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_1 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_2 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_3 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_4 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_0_5 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_5 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_5 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_5 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_5 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_6 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_0 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(0,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_1 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(1,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_2 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(2,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_3 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(3,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_4 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(4,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_6_5 <- round(stats::dpois(6,EURO_fixtures$euro_xGH) * stats::dpois(5,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_0_6 <- round(stats::dpois(0,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_1_6 <- round(stats::dpois(1,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_2_6 <- round(stats::dpois(2,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_3_6 <- round(stats::dpois(3,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_4_6 <- round(stats::dpois(4,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
EURO_fixtures$euro_5_6 <- round(stats::dpois(5,EURO_fixtures$euro_xGH) * stats::dpois(6,EURO_fixtures$euro_xGA), digits = 4)
#Home win
EURO_fixtures$euro_H <- (
  EURO_fixtures$euro_1_0 + EURO_fixtures$euro_2_0 + EURO_fixtures$euro_2_1 + EURO_fixtures$euro_3_0 + EURO_fixtures$euro_3_1 +
    EURO_fixtures$euro_3_2 + EURO_fixtures$euro_4_0 + EURO_fixtures$euro_4_1 + EURO_fixtures$euro_4_2 + EURO_fixtures$euro_4_3 +
    EURO_fixtures$euro_5_0 + EURO_fixtures$euro_5_1 + EURO_fixtures$euro_5_2 + EURO_fixtures$euro_5_3 + EURO_fixtures$euro_5_4 +
    EURO_fixtures$euro_6_0 + EURO_fixtures$euro_6_1 + EURO_fixtures$euro_6_2 + EURO_fixtures$euro_6_3 + EURO_fixtures$euro_6_4 +
    EURO_fixtures$euro_6_5
)

EURO_fixtures$euro_H <- percent(EURO_fixtures$euro_H, accuracy = 0.1)

#Draw
EURO_fixtures$euro_D <- (

  EURO_fixtures$euro_0_0 + EURO_fixtures$euro_1_1 + EURO_fixtures$euro_2_2 + EURO_fixtures$euro_3_3 + EURO_fixtures$euro_4_4 +
    EURO_fixtures$euro_5_5 + EURO_fixtures$euro_6_6
)

EURO_fixtures$euro_D <- percent(EURO_fixtures$euro_D, accuracy = 0.1)

#Away

EURO_fixtures$euro_A <- (
  EURO_fixtures$euro_0_1 + EURO_fixtures$euro_0_2 + EURO_fixtures$euro_1_2 + EURO_fixtures$euro_0_3 + EURO_fixtures$euro_1_3 +
    EURO_fixtures$euro_2_3 + EURO_fixtures$euro_0_4 + EURO_fixtures$euro_1_4 + EURO_fixtures$euro_2_4 + EURO_fixtures$euro_3_4 +
    EURO_fixtures$euro_0_5 + EURO_fixtures$euro_1_5 + EURO_fixtures$euro_2_5 + EURO_fixtures$euro_3_5 + EURO_fixtures$euro_4_5 +
    EURO_fixtures$euro_0_6 + EURO_fixtures$euro_1_6 + EURO_fixtures$euro_2_6 + EURO_fixtures$euro_3_6 + EURO_fixtures$euro_4_6 +
    EURO_fixtures$euro_5_6
)

EURO_fixtures$euro_A <- percent(EURO_fixtures$euro_A, accuracy = 0.1)

#ov25
EURO_fixtures$euro_ov25 <- (
  EURO_fixtures$euro_2_1 + EURO_fixtures$euro_1_2 + EURO_fixtures$euro_2_2 + EURO_fixtures$euro_3_0 + EURO_fixtures$euro_3_1 +
    EURO_fixtures$euro_3_2 + EURO_fixtures$euro_0_3 + EURO_fixtures$euro_1_3 + EURO_fixtures$euro_2_3 + EURO_fixtures$euro_3_3 +
    EURO_fixtures$euro_4_0 + EURO_fixtures$euro_4_1 + EURO_fixtures$euro_4_2 + EURO_fixtures$euro_4_3 + EURO_fixtures$euro_0_4 +
    EURO_fixtures$euro_1_4 + EURO_fixtures$euro_2_4 + EURO_fixtures$euro_3_4 + EURO_fixtures$euro_4_4 + EURO_fixtures$euro_5_0 +
    EURO_fixtures$euro_5_1 + EURO_fixtures$euro_5_2 + EURO_fixtures$euro_5_3 + EURO_fixtures$euro_5_4 + EURO_fixtures$euro_0_5 +
    EURO_fixtures$euro_1_5 + EURO_fixtures$euro_2_5 + EURO_fixtures$euro_3_5 + EURO_fixtures$euro_4_5 + EURO_fixtures$euro_5_5 +
    EURO_fixtures$euro_6_0 + EURO_fixtures$euro_6_1 + EURO_fixtures$euro_6_2 + EURO_fixtures$euro_6_3 + EURO_fixtures$euro_6_4 +
    EURO_fixtures$euro_6_5 + EURO_fixtures$euro_0_6 + EURO_fixtures$euro_1_6 + EURO_fixtures$euro_2_6 + EURO_fixtures$euro_3_6 +
    EURO_fixtures$euro_4_6 + EURO_fixtures$euro_5_6 + EURO_fixtures$euro_6_6
)
#un25
EURO_fixtures$euro_un25 <- (
  EURO_fixtures$euro_0_0 + EURO_fixtures$euro_1_0 + EURO_fixtures$euro_0_1 + EURO_fixtures$euro_1_1 + EURO_fixtures$euro_2_0 + EURO_fixtures$euro_0_2
)
#odds
EURO_fixtures$euro_ov25_odds <- round((1/EURO_fixtures$euro_ov25),digits = 2)
EURO_fixtures$euro_un25_odds <- round((1/EURO_fixtures$euro_un25),digits = 2)

EURO_fixtures$euro_ov25_odds
EURO_fixtures$euro_un25_odds
#percentages
EURO_fixtures$euro_ov25 <- percent(EURO_fixtures$euro_ov25, accuracy = 0.1)

EURO_fixtures$euro_un25 <- percent(EURO_fixtures$euro_un25, accuracy = 0.1)
EURO_fixtures$euro_pscore <- paste(round(EURO_fixtures$euro_xGH,digits = 0),round(EURO_fixtures$euro_xGA,digits = 0),sep = "-")
#write out
write.xlsx(EURO_fixtures,'EURO.xlsx',sheetName = "EURO", append = TRUE)
###########################################################################################################
########################EURO END###########################################################################



