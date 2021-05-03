library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
#Full time results percentages
ftr_summary <- tabyl(allteams20202021,Div,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,4,3,2)]
#Half time results percentages
htr_summary <- tabyl(allteams20202021,Div,HTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
htr_summary <- htr_summary[,c(1,4,3,2)]
#Total goals scored
#B1
b1_home_gs <- aggregate(B1$FTHG, by = list(B1$HomeTeam), FUN = sum)
b1_home_gs_avg <- aggregate(B1$FTHG, by = list(B1$HomeTeam),mean)
b1_home_scoring <- merge(b1_home_gs,b1_home_gs_avg, by='Group.1',all = T)
names(b1_home_scoring)[names(b1_home_scoring) == "x.x"] <- "TFthg"
names(b1_home_scoring)[names(b1_home_scoring) == "x.y"] <- "Avg_Fthg"

b1_away_gs <- aggregate(B1$FTAG, by = list(B1$AwayTeam), FUN = sum)
b1_away_gs_avg <- aggregate(B1$FTAG, by = list(B1$AwayTeam),mean)
b1_away_scoring <- merge(b1_away_gs,b1_away_gs_avg, by='Group.1',all = T)
names(b1_away_scoring)[names(b1_away_scoring) == "x.x"] <- "TFtag"
names(b1_away_scoring)[names(b1_away_scoring) == "x.y"] <- "Avg_Ftag"






