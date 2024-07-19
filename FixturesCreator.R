library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
library('lubridate')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

epl_match_results <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2025, tier="1st")
write.xlsx(epl_match_results,"epl_match_results.xlsx")
epl_match_fixtures <- readxl::read_excel("epl_match_results.xlsx")
epl_match_fixtures <- epl_match_fixtures[,c(-1)]

epl_match_fixtures <- epl_match_fixtures[,c(1,10,13,8)]

colnames(epl_match_fixtures)[1] <- "Div"
colnames(epl_match_fixtures)[2] <- "HomeTeam"
colnames(epl_match_fixtures)[3] <- "AwayTeam"
epl_match_fixtures$Div <- "E0"
epl_match_fixtures$Date <- ymd(epl_match_fixtures$Date)
sort(unique(epl_match_fixtures$HomeTeam))

epl_match_fixtures$HomeTeam <- mgsub(epl_match_fixtures$HomeTeam,c("Manchester Utd","Manchester City","Newcastle Utd","Nott'ham Forest","Ipswich Town","Leicester City"),c("Man United","Man City","Newcastle","Nottm Forest","Ipswich","Leicester"))
epl_match_fixtures$AwayTeam <- mgsub(epl_match_fixtures$AwayTeam,c("Manchester Utd","Manchester City","Newcastle Utd","Nott'ham Forest","Ipswich Town","Leicester City"),c("Man United","Man City","Newcastle","Nottm Forest","Ipswich","Leicester"))
write.csv(epl_match_fixtures,'EPLFIXTURES.csv')


View(epl_match_fixtures)
