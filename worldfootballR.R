
#remotes::install_github("JaseZiv/worldfootballR")
#library('worldfootballR')
library('dplyr')
library('mgsub')
library('xlsx')

#E0_spread <- subset(readxl::read_excel('UCL20232024.xlsx'), Div == "E0")
#write.xlsx(E0_spread,'E0_spread.xlsx')
#packageVersion('worldfootballR')

#E0
# epl_match_details <- load_fotmob_match_details(
#   country = "ENG",
#   league_name = "Premier League",
# )

#select just the current season
# epl_startdate <- which(epl_match_details$match_time_utc == "Fri, Aug 5, 2022, 19:00 UTC")
# epl_startindex <- nrow(epl_match_details) - epl_startdate[1]
# epl_match_details <- tail(epl_match_details,epl_startindex)
#
# epl_matchids
# epl_match_details$home_team <- mgsub(epl_match_details$home_team,c("AFC Bournemouth","Brighton & Hove Albion","Leeds United","Leicester City","Manchester United","Manchester City","Newcastle United","Nottingham Forest","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Bournemouth","Brighton","Leeds","Leicester","Man United","Man City","Newcastle","Nottm Forest","Tottenham","West Ham","Wolves"))
# epl_match_details$away_team <- mgsub(epl_match_details$away_team,c("AFC Bournemouth","Brighton & Hove Albion","Leeds United","Leicester City","Manchester United","Manchester City","Newcastle United","Nottingham Forest","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Bournemouth","Brighton","Leeds","Leicester","Man United","Man City","Newcastle","Nottm Forest","Tottenham","West Ham","Wolves"))
# epl_match_details$matchid <- paste(epl_match_details$home_team,epl_match_details$away_team,sep = "-")
# epl_match_details <- epl_match_details[epl_match_details$event_type == "Goal",]
#
#
# epl_goal_mins <- epl_match_details %>% group_by(matchid) %>%
#   dplyr::summarise(epl_goalmins = sum(min),
#             n = n()
#
#             )

# E0_spread <- left_join(E0_spread,epl_goal_mins)
# E0_spread <- E0_spread %>% replace(is.na(.),0)
#
# epl_matchids <- as.vector(epl_match_details$match_id)
# epl_matchids <- unique(epl_matchids)
# epl_matchidslength <- length(epl_matchids)
#
# epl_matchids[1:epl_matchidslength]
# epl_players <- fotmob_get_match_players(epl_matchids[1:epl_matchidslength])
#
# names(epl_players)[names(epl_players) == "id"] <- "player_id"
#
# epl_players$player_id <- as.integer(epl_players$player_id)
#
# epl_players_and_match <- merge(epl_match_details,epl_players)
# epl_players_and_match$shirt <- as.integer(epl_players_and_match$shirt)
#
# epl_shirtnumbers <- epl_players_and_match %>% group_by(matchid) %>%
#   dplyr::summarise(epl_shirts = sum(shirt),)
#
# E0_spread <- left_join(E0_spread,epl_shirtnumbers)
# E0_spread <- E0_spread %>% replace(is.na(.),0)

E0_spread <- subset(allteams20232024,Div =="E0")
E0_spread$matchid <- paste(E0$HomeTeam,E0$AwayTeam,sep = "-")
eplnrows <- nrow(E0_spread) - nrow(readxl::read_excel('E0_spread.xlsx'))
E0_spread <- tail(E0_spread,eplnrows)
E0_spread$epl_goalmins <- 0
E0_spread$n <- E0_spread$TG
E0_spread$epl_shirts <- 0
E0_spread$Bookings <- (E0_spread$HY *10 + E0_spread$HR *25) + (E0_spread$AY*10 + E0_spread$AR*25)
E0_spread$Crossbookings <- (E0_spread$HY *10 + E0_spread$HR *25)*(E0_spread$AY*10 + E0_spread$AR*25)
E0_spread$GoalsXbookings <- (E0_spread$Bookings)*(E0_spread$TG)
E0_spread$CornersXbookings <- (E0_spread$TC)*(E0_spread$Bookings)
E0_spread$ShirtsXbookings <- (E0_spread$epl_shirts)*(E0_spread$Bookings)
E0_spread$GoalsXcorners <- (E0_spread$TG)*(E0_spread$TC)
E0_spread$GoalsXshirts <- (E0_spread$TG)*(E0_spread$epl_shirts)
E0_spread$ShirtsXcorners <- (E0_spread$epl_shirts)*(E0_spread$TC)
E0_spread$TGMXcorners <- (E0_spread$epl_goalmins)*(E0_spread$TC)
E0_spread$GoalsXcornerXbookings <- (E0_spread$TG)*(E0_spread$TC)*(E0_spread$Bookings)
E0_spread_prev <- readxl::read_excel('E0_spread.xlsx')
E0_spread_final <- rbind(E0_spread_prev,E0_spread)

unlink('E0_spread.xlsx')
write.xlsx(E0_spread_final,'E0_spread.xlsx')

#############################################################################################################
# #D1
# bundesliga_match_details <- load_fotmob_match_details(
#   country = "GER",
#   league_name = "1. Bundesliga"
# )
# #select just the current season
# bundesliga_startdate <- which(bundesliga_match_details$match_time_utc == "Fri, Aug 5, 2022, 18:35 UTC")
# bundesliga_startindex <- nrow(bundesliga_match_details) - bundesliga_startdate[1]
# bundesliga_match_details <- tail(bundesliga_match_details,bundesliga_startindex)
#
# bundesliga_match_details$home_team <- mgsub(bundesliga_match_details$home_team,c("1. FC Köln","FC Augsburg","Bayer Leverkusen","Bayern München","Borussia Dortmund","Borussia M'Gladbach","Eintracht Frankfurt","Hertha BSC","Mainz 05","TSG Hoffenheim","VfB Stuttgart","VfL Bochum","SC Freiburg"),c("FC Koln","Augsburg","Leverkusen","Bayern Munich","Dortmund","Mgladbach","Ein Frankfurt","Hertha","Mainz","Hoffenheim","Stuttgart","Bochum","Freiburg"))
# bundesliga_match_details$away_team <- mgsub(bundesliga_match_details$away_team,c("1. FC Köln","FC Augsburg","Bayer Leverkusen","Bayern München","Borussia Dortmund","Borussia M'Gladbach","Eintracht Frankfurt","Hertha BSC","Mainz 05","TSG Hoffenheim","VfB Stuttgart","VfL Bochum","SC Freiburg"),c("FC Koln","Augsburg","Leverkusen","Bayern Munich","Dortmund","Mgladbach","Ein Frankfurt","Hertha","Mainz","Hoffenheim","Stuttgart","Bochum","Freiburg"))
# bundesliga_match_details$matchid <- paste(bundesliga_match_details$home_team,bundesliga_match_details$away_team,sep = "-")
# bundesliga_match_details <- bundesliga_match_details[bundesliga_match_details$event_type == "Goal",]
# d1_teams
#
# bundesliga_goal_mins <- bundesliga_match_details %>% group_by(matchid) %>%
#   dplyr::summarise(bundesliga_goalmins = sum(min),
#             n = n()
#   )
#
# D1_spread <- subset(allteams20232024,Div =="D1")
# D1_spread$matchid <- paste(D1$HomeTeam,D1$AwayTeam,sep = "-")
# D1_spread <- left_join(D1_spread,bundesliga_goal_mins)
# D1_spread <- D1_spread %>% replace(is.na(.),0)
#
# bundesliga_matchids <- as.vector(bundesliga_match_details$match_id)
# bundesliga_matchids <- unique(bundesliga_matchids)
# bundesliga_matchidslength <- length(bundesliga_matchids)
#
# bundesliga_matchids[1:bundesliga_matchidslength]
# bundesliga_players <- fotmob_get_match_players(bundesliga_matchids[1:bundesliga_matchidslength])
#
#
# names(bundesliga_players)[names(bundesliga_players) == "id"] <- "player_id"
#
# bundesliga_players$player_id <- as.integer(bundesliga_players$player_id)
#
# bundesliga_players_and_match <- merge(bundesliga_match_details,bundesliga_players)
# bundesliga_players_and_match$shirt <- as.integer(bundesliga_players_and_match$shirt)
#
# bundesliga_shirtnumbers <- bundesliga_players_and_match %>% group_by(matchid) %>%
#   dplyr::summarise(bundesliga_shirts = sum(shirt),)
#
# D1_spread <- left_join(D1_spread,bundesliga_shirtnumbers)
# D1_spread <- D1_spread %>% replace(is.na(.),0)


D1_spread <- subset(allteams20232024,Div =="D1")
D1_spread$matchid <- paste(D1$HomeTeam,D1$AwayTeam,sep = "-")
bundesliganrows <- nrow(D1_spread) - nrow(readxl::read_excel('D1_spread.xlsx'))
D1_spread <- tail(D1_spread,bundesliganrows)
D1_spread$bundesliga_goalmins <- 0
D1_spread$n <- D1_spread$TG
D1_spread$bundesliga_shirts <- 0
D1_spread$Bookings <- (D1_spread$HY *10 + D1_spread$HR *25) + (D1_spread$AY*10 + D1_spread$AR*25)
D1_spread$Crossbookings <- (D1_spread$HY *10 + D1_spread$HR *25)*(D1_spread$AY*10 + D1_spread$AR*25)
D1_spread$GoalsXbookings <- (D1_spread$Bookings)*(D1_spread$TG)
D1_spread$CornersXbookings <- (D1_spread$TC)*(D1_spread$Bookings)
D1_spread$ShirtsXbookings <- (D1_spread$bundesliga_shirts)*(D1_spread$Bookings)
D1_spread$GoalsXcorners <- (D1_spread$TG)*(D1_spread$TC)
D1_spread$GoalsXshirts <- (D1_spread$TG)*(D1_spread$bundesliga_shirts)
D1_spread$ShirtsXcorners <- (D1_spread$bundesliga_shirts)*(D1_spread$TC)
D1_spread$TGMXcorners <- (D1_spread$bundesliga_goalmins)*(D1_spread$TC)
D1_spread$GoalsXcornerXbookings <- (D1_spread$TG)*(D1_spread$TC)*(D1_spread$Bookings)
D1_spread_prev <- readxl::read_excel('D1_spread.xlsx')
D1_spread_final <- rbind(D1_spread_prev,D1_spread)

unlink('D1_spread.xlsx')
write.xlsx(D1_spread_final,'D1_spread.xlsx')
##################################################################################################
# #I1
# seriea_match_details <- load_fotmob_match_details(
#   country = "ITA",
#   league_name = "Serie A"
# )
#
# #select just the current season
# seriea_startdate <- which(seriea_match_details$match_time_utc == "Sat, Aug 13, 2022, 16:30 UTC")
# seriea_startindex <- nrow(seriea_match_details) - seriea_startdate[1]
# seriea_match_details <- tail(seriea_match_details,seriea_startindex)
#
#
# seriea_match_details$home_team <- mgsub(seriea_match_details$home_team,c("Hellas Verona"),c("Verona"))
# seriea_match_details$away_team <- mgsub(seriea_match_details$away_team,c("Hellas Verona"),c("Verona"))
# seriea_match_details$matchid <- paste(seriea_match_details$home_team,seriea_match_details$away_team,sep = "-")
# seriea_match_details <- seriea_match_details[seriea_match_details$event_type == "Goal",]
#
#
# seriea_goal_mins <- seriea_match_details %>% group_by(matchid) %>%
#   dplyr::summarise(seriea_goalmins = sum(min),
#             n = n()
#   )
#
# I1_spread <- subset(allteams20232024,Div =="I1")
# I1_spread$matchid <- paste(I1$HomeTeam,I1$AwayTeam,sep = "-")
# I1_spread <- left_join(I1_spread,seriea_goal_mins)
# I1_spread <- I1_spread %>% replace(is.na(.),0)
#
# seriea_matchids <- as.vector(seriea_match_details$match_id)
# seriea_matchids <- unique(seriea_matchids)
# seriea_matchidslength <- length(seriea_matchids)
#
# seriea_matchids[1:seriea_matchidslength]
# seriea_players <- fotmob_get_match_players(seriea_matchids[1:seriea_matchidslength])
#
#
# names(seriea_players)[names(seriea_players) == "id"] <- "player_id"
#
# seriea_players$player_id <- as.integer(seriea_players$player_id)
#
# seriea_players_and_match <- merge(seriea_match_details,seriea_players)
# seriea_players_and_match$shirt <- as.integer(seriea_players_and_match$shirt)
#
# seriea_shirtnumbers <- seriea_players_and_match %>% group_by(matchid) %>%
#   dplyr::summarise(seriea_shirts = sum(shirt),)
#
# I1_spread <- left_join(I1_spread,seriea_shirtnumbers)
# I1_spread <- I1_spread %>% replace(is.na(.),0)

I1_spread <- subset(allteams20232024,Div =="I1")
I1_spread$matchid <- paste(I1$HomeTeam,I1$AwayTeam,sep = "-")
serieanrows <- nrow(I1_spread) - nrow(readxl::read_excel('I1_spread.xlsx'))
I1_spread <- tail(I1_spread,serieanrows)
I1_spread$seriea_goalmins <- 0
I1_spread$n <- I1_spread$TG
I1_spread$seriea_shirts <- 0
I1_spread$Bookings <- (I1_spread$HY *10 + I1_spread$HR *25) + (I1_spread$AY*10 + I1_spread$AR*25)
I1_spread$Crossbookings <- (I1_spread$HY *10 + I1_spread$HR *25)*(I1_spread$AY*10 + I1_spread$AR*25)
I1_spread$GoalsXbookings <- (I1_spread$Bookings)*(I1_spread$TG)
I1_spread$CornersXbookings <- (I1_spread$TC)*(I1_spread$Bookings)
I1_spread$ShirtsXbookings <- (I1_spread$seriea_shirts)*(I1_spread$Bookings)
I1_spread$GoalsXcorners <- (I1_spread$TG)*(I1_spread$TC)
I1_spread$GoalsXshirts <- (I1_spread$TG)*(I1_spread$seriea_shirts)
I1_spread$ShirtsXcorners <- (I1_spread$seriea_shirts)*(I1_spread$TC)
I1_spread$TGMXcorners <- (I1_spread$seriea_goalmins)*(I1_spread$TC)
I1_spread$GoalsXcornerXbookings <- (I1_spread$TG)*(I1_spread$TC)*(I1_spread$Bookings)
I1_spread_prev <- readxl::read_excel('I1_spread.xlsx')
I1_spread_final <- rbind(I1_spread_prev,I1_spread)

unlink('I1_spread.xlsx')
write.xlsx(I1_spread_final,'I1_spread.xlsx')

###########################################################################################################
#SP1
# laliga_match_details <- load_fotmob_match_details(
#   country = "ESP",
#   league_name = "LaLiga"
# )
#
# #select just the current season
# laliga_startdate <- which(laliga_match_details$match_time_utc == "Fri, Aug 12, 2022, 19:00 UTC")
# laliga_startindex <- nrow(laliga_match_details) - laliga_startdate[1]
# laliga_match_details <- tail(laliga_match_details,laliga_startindex)
#
# laliga_match_details$home_team <- mgsub(laliga_match_details$home_team,c("Athletic Club","Atletico Madrid","Celta Vigo","Espanyol","Rayo Vallecano","Real Betis","Real Sociedad"),c("Ath Bilbao","Ath Madrid","Celta","Espanol","Vallecano","Betis","Sociedad"))
# laliga_match_details$away_team <- mgsub(laliga_match_details$away_team,c("Athletic Club","Atletico Madrid","Celta Vigo","Espanyol","Rayo Vallecano","Real Betis","Real Sociedad"),c("Ath Bilbao","Ath Madrid","Celta","Espanol","Vallecano","Betis","Sociedad"))
#
# laliga_match_details$matchid <- paste(laliga_match_details$home_team,laliga_match_details$away_team,sep = "-")
# laliga_match_details <- laliga_match_details[laliga_match_details$event_type == "Goal",]
#
#
# laliga_goal_mins <- laliga_match_details %>% group_by(matchid) %>%
#   dplyr::summarise(laliga_goalmins = sum(min),
#             n = n()
#   )
#
# SP1_spread <- subset(allteams20232024,Div =="SP1")
# SP1_spread$matchid <- paste(SP1$HomeTeam,SP1$AwayTeam,sep = "-")
# SP1_spread <- left_join(SP1_spread,laliga_goal_mins)
# SP1_spread <- SP1_spread %>% replace(is.na(.),0)
#
# laliga_matchids <- as.vector(laliga_match_details$match_id)
# laliga_matchids <- unique(laliga_matchids)
# laliga_matchidslength <- length(laliga_matchids)
#
# laliga_matchids[1:laliga_matchidslength]
# laliga_players <- fotmob_get_match_players(laliga_matchids[1:laliga_matchidslength])
#
#
# names(laliga_players)[names(laliga_players) == "id"] <- "player_id"
#
# laliga_players$player_id <- as.integer(laliga_players$player_id)
#
# laliga_players_and_match <- merge(laliga_match_details,laliga_players)
# laliga_players_and_match$shirt <- as.integer(laliga_players_and_match$shirt)
#
# laliga_shirtnumbers <- laliga_players_and_match %>% group_by(matchid) %>%
#   dplyr::summarise(laliga_shirts = sum(shirt),)
#
# SP1_spread <- left_join(SP1_spread,laliga_shirtnumbers)
# SP1_spread <- SP1_spread %>% replace(is.na(.),0)

SP1_spread <- subset(allteams20232024,Div =="SP1")
SP1_spread$matchid <- paste(SP1$HomeTeam,SP1$AwayTeam,sep = "-")
laliganrows <- nrow(SP1_spread) - nrow(readxl::read_excel('SP1_spread.xlsx'))
SP1_spread <- tail(SP1_spread,laliganrows)
SP1_spread$laliga_goalmins <- 0
SP1_spread$n <- SP1_spread$TG
SP1_spread$laliga_shirts <- 0
SP1_spread$Bookings <- (SP1_spread$HY *10 + SP1_spread$HR *25) + (SP1_spread$AY*10 + SP1_spread$AR*25)
SP1_spread$Crossbookings <- (SP1_spread$HY *10 + SP1_spread$HR *25)*(SP1_spread$AY*10 + SP1_spread$AR*25)
SP1_spread$GoalsXbookings <- (SP1_spread$Bookings)*(SP1_spread$TG)
SP1_spread$CornersXbookings <- (SP1_spread$TC)*(SP1_spread$Bookings)
SP1_spread$ShirtsXbookings <- (SP1_spread$laliga_shirts)*(SP1_spread$Bookings)
SP1_spread$GoalsXcorners <- (SP1_spread$TG)*(SP1_spread$TC)
SP1_spread$GoalsXshirts <- (SP1_spread$TG)*(SP1_spread$laliga_shirts)
SP1_spread$ShirtsXcorners <- (SP1_spread$laliga_shirts)*(SP1_spread$TC)
SP1_spread$TGMXcorners <- (SP1_spread$laliga_goalmins)*(SP1_spread$TC)
SP1_spread$GoalsXcornerXbookings <- (SP1_spread$TG)*(SP1_spread$TC)*(SP1_spread$Bookings)
SP1_spread_prev <- readxl::read_excel('SP1_spread.xlsx')
SP1_spread_final <- rbind(SP1_spread_prev,SP1_spread)

unlink('SP1_spread.xlsx')
write.xlsx(SP1_spread_final,'SP1_spread.xlsx')

################################################################################################################
# #F1
# ligueone_match_details <- load_fotmob_match_details(
#   country = "FRA",
#   league_name = "Ligue 1"
# )
#
# #select just the current season
# ligueone_startdate <- which(ligueone_match_details$match_time_utc == "Fri, Aug 5, 2022, 19:00 UTC")
# ligueone_startindex <- nrow(ligueone_match_details) - ligueone_startdate[1]
# ligueone_match_details <- tail(ligueone_match_details,ligueone_startindex)
#
# ligueone_match_details$home_team <- mgsub(ligueone_match_details$home_team,c("AC Ajaccio","Clermont Foot","Paris Saint-Germain"),c("Ajaccio","Clermont","Paris SG"))
# ligueone_match_details$away_team <- mgsub(ligueone_match_details$away_team,c("AC Ajaccio","Clermont Foot","Paris Saint-Germain"),c("Ajaccio","Clermont","Paris SG"))
#
# ligueone_match_details$matchid <- paste(ligueone_match_details$home_team,ligueone_match_details$away_team,sep = "-")
# ligueone_match_details <- ligueone_match_details[ligueone_match_details$event_type == "Goal",]
#
#
# ligueone_goal_mins <- ligueone_match_details %>% group_by(matchid) %>%
#   dplyr::summarise(ligueone_goalmins = sum(min),
#                    n = n()
#   )
#
# F1_spread <- subset(allteams20232024,Div =="F1")
# F1_spread$matchid <- paste(F1$HomeTeam,F1$AwayTeam,sep = "-")
# F1_spread <- left_join(F1_spread,ligueone_goal_mins)
# F1_spread <- F1_spread %>% replace(is.na(.),0)
#
# ligueone_matchids <- as.vector(ligueone_match_details$match_id)
# ligueone_matchids <- unique(ligueone_matchids)
# ligueone_matchids <- na.omit(ligueone_matchids)
# ligueone_matchidslength <- length(ligueone_matchids)
# ligueone_matchids[1:ligueone_matchidslength]
# ligueone_players <- fotmob_get_match_players(ligueone_matchids[1:ligueone_matchidslength])
#
#
# names(ligueone_players)[names(ligueone_players) == "id"] <- "player_id"
#
# ligueone_players$player_id <- as.integer(ligueone_players$player_id)
#
# ligueone_players_and_match <- merge(ligueone_match_details,ligueone_players)
# ligueone_players_and_match$shirt <- as.integer(ligueone_players_and_match$shirt)
#
# ligueone_shirtnumbers <- ligueone_players_and_match %>% group_by(matchid) %>%
#   dplyr::summarise(ligueone_shirts = sum(shirt),)
#
# F1_spread <- left_join(F1_spread,ligueone_shirtnumbers)
# F1_spread <- F1_spread %>% replace(is.na(.),0)

F1_spread <- subset(allteams20232024,Div =="F1")
F1_spread$matchid <- paste(F1$HomeTeam,F1$AwayTeam,sep = "-")
ligueonerows <- nrow(F1_spread) - nrow(readxl::read_excel('F1_spread.xlsx'))
F1_spread <- tail(F1_spread,ligueonerows)
F1_spread$ligueone_goalmins <- 0
F1_spread$n <- F1_spread$TG
F1_spread$ligueone_shirts <- 0
F1_spread$Bookings <- (F1_spread$HY *10 + F1_spread$HR *25) + (F1_spread$AY*10 + F1_spread$AR*25)
F1_spread$Crossbookings <- (F1_spread$HY *10 + F1_spread$HR *25)*(F1_spread$AY*10 + F1_spread$AR*25)
F1_spread$GoalsXbookings <- (F1_spread$Bookings)*(F1_spread$TG)
F1_spread$CornersXbookings <- (F1_spread$TC)*(F1_spread$Bookings)
F1_spread$ShirtsXbookings <- (F1_spread$ligueone_shirts)*(F1_spread$Bookings)
F1_spread$GoalsXcorners <- (F1_spread$TG)*(F1_spread$TC)
F1_spread$GoalsXshirts <- (F1_spread$TG)*(F1_spread$ligueone_shirts)
F1_spread$ShirtsXcorners <- (F1_spread$ligueone_shirts)*(F1_spread$TC)
F1_spread$TGMXcorners <- (F1_spread$ligueone_goalmins)*(F1_spread$TC)
F1_spread$GoalsXcornerXbookings <- (F1_spread$TG)*(F1_spread$TC)*(F1_spread$Bookings)
F1_spread_prev <- readxl::read_excel('F1_spread.xlsx')
F1_spread_final <- rbind(F1_spread_prev,F1_spread)

unlink('F1_spread.xlsx')
write.xlsx(F1_spread_final,'F1_spread.xlsx')

################################################################################################################


















