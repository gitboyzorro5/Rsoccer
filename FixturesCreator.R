library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
library('lubridate')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

scoleagueone_match_results <- fb_match_results(country = "SCO", gender = "M", season_end_year = 2026, tier="3rd")

write.xlsx(scoleagueone_match_results,"scoleagueone_match_results.xlsx")
scoleagueone_match_fixtures <- readxl::read_excel("scoleagueone_match_results.xlsx")
scoleagueone_match_fixtures <- scoleagueone_match_fixtures[,c(-1)]
scoleagueone_match_fixtures <- scoleagueone_match_fixtures[,c(1,10,12,8)]
View(scoleagueone_match_fixtures)

colnames(scoleagueone_match_fixtures)[1] <- "Div"
colnames(scoleagueone_match_fixtures)[2] <- "HomeTeam"
colnames(scoleagueone_match_fixtures)[3] <- "AwayTeam"
scoleagueone_match_fixtures$Div <- "T1"
scoleagueone_match_fixtures$Date <- ymd(scoleagueone_match_fixtures$Date)
sort(unique(scoleagueone_match_fixtures$HomeTeam))
t1_teams

scoleagueone_match_fixtures$HomeTeam <- mgsub(scoleagueone_match_fixtures$HomeTeam,c("Başakşehir","Beşiktaş","Eyüpspor","Fatih Karagümrük","Gaziantep FK","Gençlerbirliği","Göztepe","Kasımpaşa"),c("Buyuksehyr","Besiktas","Eyupspor","Karagumruk","Gaziantep","Genclerbirligi","Goztep","Kasimpasa"))
scoleagueone_match_fixtures$AwayTeam <- mgsub(scoleagueone_match_fixtures$AwayTeam,c("Başakşehir","Beşiktaş","Eyüpspor","Fatih Karagümrük","Gaziantep FK","Gençlerbirliği","Göztepe","Kasımpaşa"),c("Buyuksehyr","Besiktas","Eyupspor","Karagumruk","Gaziantep","Genclerbirligi","Goztep","Kasimpasa"))

write.csv(scoleagueone_match_fixtures,'SUPERLIGFIXTURES.csv')



