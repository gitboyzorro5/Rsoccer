library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
library('lubridate')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

superleague_match_results <- fb_match_results(country = "GRE", gender = "M", season_end_year = 2025, tier="1st")
View(superleague_match_fixtures)
write.xlsx(superleague_match_results,"superleague_match_results.xlsx")
superleague_match_fixtures <- readxl::read_excel("superleague_match_results.xlsx")
superleague_match_fixtures <- superleague_match_fixtures[,c(-1)]

superleague_match_fixtures <- superleague_match_fixtures[,c(1,10,12,8)]

colnames(superleague_match_fixtures)[1] <- "Div"
colnames(superleague_match_fixtures)[2] <- "HomeTeam"
colnames(superleague_match_fixtures)[3] <- "AwayTeam"
superleague_match_fixtures$Div <- "G1"
superleague_match_fixtures$Date <- ymd(superleague_match_fixtures$Date)
sort(unique(superleague_match_fixtures$HomeTeam))

superleague_match_fixtures$HomeTeam <- mgsub(superleague_match_fixtures$HomeTeam,c("AEK Athens","Asteras Tripoli","Levadiakos","Kallithea","Olympiacos","PAS Lamia"),c("AEK","Asteras Tripolis","Levadeiakos","Athens Kallithea","Olympiakos","Lamia"))
superleague_match_fixtures$AwayTeam <- mgsub(superleague_match_fixtures$AwayTeam,c("AEK Athens","Asteras Tripoli","Levadiakos","Kallithea","Olympiacos","PAS Lamia"),c("AEK","Asteras Tripolis","Levadeiakos","Athens Kallithea","Olympiakos","Lamia"))
write.csv(superleague_match_fixtures,'SUPERLEAGUEFIXTURES.csv')



