library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
library('lubridate')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

primera_match_results <- fb_match_results(country = "POR", gender = "M", season_end_year = 2025, tier="1st")
write.xlsx(primera_match_results,"primera_match_results.xlsx")
primera_match_fixtures <- readxl::read_excel("primera_match_results.xlsx")
primera_match_fixtures <- primera_match_fixtures[,c(-1)]

primera_match_fixtures <- primera_match_fixtures[,c(1,10,13,8)]

colnames(primera_match_fixtures)[1] <- "Div"
colnames(primera_match_fixtures)[2] <- "HomeTeam"
colnames(primera_match_fixtures)[3] <- "AwayTeam"
primera_match_fixtures$Div <- "P1"
primera_match_fixtures$Date <- ymd(primera_match_fixtures$Date)
sort(unique(primera_match_fixtures$HomeTeam))
p1_teams
primera_match_fixtures$HomeTeam <- mgsub(primera_match_fixtures$HomeTeam,c("Braga","Famalicão","Gil Vicente FC","Sporting CP","Vitória"),c("Sp Braga","Famalicao","Gil Vicente","Sp Lisbon","Vitoria"))
primera_match_fixtures$AwayTeam <- mgsub(primera_match_fixtures$AwayTeam,c("Braga","Famalicão","Gil Vicente FC","Sporting CP","Vitória"),c("Sp Braga","Famalicao","Gil Vicente","Sp Lisbon","Vitoria"))
write.csv(primera_match_fixtures,'PRIMERAFIXTURES.csv')

sp2_teams
sp1_teams
View(laligatwo_match_results)

