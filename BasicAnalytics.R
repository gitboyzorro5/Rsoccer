library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
#Full time results percentages
ftr_summary <- tabyl(allteams20202021,Div,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,4,3,2)]
#Half time results percentages
htr_summary <- tabyl(allteams20202021,Div,HTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
htr_summary <- htr_summary[,c(1,4,3,2)]
#Total goals scored
#B1
#home goals scored
b1_home_gs <- aggregate(B1$FTHG, by = list(B1$HomeTeam), FUN = sum)
b1_home_gs_avg <- aggregate(B1$FTHG, by = list(B1$HomeTeam),mean)
b1_home_scoring <- merge(b1_home_gs,b1_home_gs_avg, by='Group.1',all = T)
names(b1_home_scoring)[names(b1_home_scoring) == "x.x"] <- "TFthg"
names(b1_home_scoring)[names(b1_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
b1_away_gs <- aggregate(B1$FTAG, by = list(B1$AwayTeam), FUN = sum)
b1_away_gs_avg <- aggregate(B1$FTAG, by = list(B1$AwayTeam),mean)
b1_away_scoring <- merge(b1_away_gs,b1_away_gs_avg, by='Group.1',all = T)
names(b1_away_scoring)[names(b1_away_scoring) == "x.x"] <- "TFtag"
names(b1_away_scoring)[names(b1_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
b1_scoring <- merge(b1_home_scoring,b1_away_scoring,by='Group.1',all = T)
b1_scoring$TGS <- b1_scoring$TFthg + b1_scoring$TFtag

#Home shots on target
b1_home_hst <- aggregate(B1$HST, by = list(B1$HomeTeam), FUN = sum)
b1_away_ast <- aggregate(B1$AST, by = list(B1$AwayTeam), FUN = sum)
b1_tst <- merge(b1_home_hst,b1_away_ast, by='Group.1',all = T)
names(b1_tst)[names(b1_tst) == "x.x"] <- "hst"
names(b1_tst)[names(b1_tst) == "x.y"] <- "ast"
b1_tst$TST <- b1_tst$hst + b1_tst$ast
#merge goals scored and shots on target
b1_scoring_conversion <- merge(b1_tst,b1_scoring,by='Group.1',all = T)
#add HSC ASC TSC
b1_scoring_conversion$HSTC <- percent(b1_scoring_conversion$TFthg/b1_scoring_conversion$hst, accuracy = 0.01)
b1_scoring_conversion$ASTC <- percent(b1_scoring_conversion$TFtag/b1_scoring_conversion$ast, accuracy = 0.01)
b1_scoring_conversion$TSTC <- percent(b1_scoring_conversion$TGS/b1_scoring_conversion$TST, accuracy = 0.01)
#merge games played
b1_scoring_conversion <- cbind(b1_scoring_conversion,b1_games_played)
#create the second part
#home goals conceded
b1_home_gc <- aggregate(B1$FTAG, by = list(B1$HomeTeam), FUN = sum)
b1_home_gc_avg <- aggregate(B1$FTAG, by = list(B1$HomeTeam),mean)
b1_home_conceding <- merge(b1_home_gc,b1_home_gc_avg, by='Group.1',all = T)
names(b1_home_conceding)[names(b1_home_conceding) == "x.x"] <- "TFthc"
names(b1_home_conceding)[names(b1_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
b1_away_gc <- aggregate(B1$FTHG, by = list(B1$AwayTeam), FUN = sum)
b1_away_gc_avg <- aggregate(B1$FTHG, by = list(B1$AwayTeam),mean)
b1_away_conceding <- merge(b1_away_gc,b1_away_gc_avg, by='Group.1',all = T)
names(b1_away_conceding)[names(b1_away_conceding) == "x.x"] <- "TFtac"
names(b1_away_conceding)[names(b1_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
b1_conceding <- merge(b1_home_conceding,b1_away_conceding,by='Group.1',all = T)
b1_conceding$TGC <- b1_conceding$TFthc + b1_conceding$TFtac

#Home shots conceded
b1_home_hsc <- aggregate(B1$AST, by = list(B1$HomeTeam), FUN = sum)
b1_away_asc <- aggregate(B1$HST, by = list(B1$AwayTeam), FUN = sum)
b1_tsc <- merge(b1_home_hsc,b1_away_asc, by='Group.1',all = T)
names(b1_tsc)[names(b1_tsc) == "x.x"] <- "hsc"
names(b1_tsc)[names(b1_tsc) == "x.y"] <- "asc"
b1_tsc$TSC <- b1_tsc$hsc + b1_tsc$asc
#merge goals conceded and shots conceded
b1_conceding_conversion <- merge(b1_tsc,b1_conceding,by='Group.1',all = T)

#add HSC ASC TSC
b1_conceding_conversion$HSCC <- percent(b1_conceding_conversion$TFthc/b1_conceding_conversion$hsc, accuracy = 0.01)
b1_conceding_conversion$ASCC <- percent(b1_conceding_conversion$TFtac/b1_conceding_conversion$asc, accuracy = 0.01)
b1_conceding_conversion$TSCC <- percent(b1_conceding_conversion$TGC/b1_conceding_conversion$TSC, accuracy = 0.01)

#merge the two parts
b1_shots_analysis <- merge(b1_scoring_conversion,b1_conceding_conversion,by='Group.1',all = T)
View(b1_shots_analysis)



