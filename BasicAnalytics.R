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
#Total Home goals scored
for (i_b1_hgs in 1:length(b1_teams))
{
  b1_home_gs <- c()
  b1_home_gs[i_b1_hgs] <- sum(B1$FTHG,B1$HomeTeam == b1_teams[i_b1_hgs])
  #e0_away_games[i_e0]  <- nrow(E0[E0$AwayTeam == e0_teams[i_e0],])
}
b1_home_gs <- aggregate(B1$FTHG, by = list(B1$HomeTeam), FUN = sum)
