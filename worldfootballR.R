

library('worldfootballR')
library('dplyr')

#B1
bjl_match_details <- load_fotmob_match_details(
  country = "Belgium",
  league_name = "First Division A",
  cached = FALSE

)

fotmob_get_leag

#select just the current season
bjl_startdate <- which(bjl_match_details$match_time_utc == "Fri, Aug 5, 2022, 19:00 UTC")
bjl_startindex <- nrow(bjl_match_details) - bjl_startdate[1]
bjl_match_details <- tail(bjl_match_details,bjl_startindex)


bjl_match_details <- bjl_match_details[bjl_match_details$event_type == "Goal",]
bjl_match_details$matchid <- paste(bjl_match_details$home_team,bjl_match_details$away_team,sep = "-")

goal_mins <- bjl_match_details %>% group_by(matchid) %>%
  summarise(goalmins = sum(min),
            n = n()

  )

unlink('B1_goalmins.csv')
write.csv(goal_mins,'B1_goalmins.csv')

#E0
epl_match_details <- load_fotmob_match_details(
  country = "ENG",
  league_name = "Premier League"
)
#select just the current season
epl_startdate <- which(epl_match_details$match_time_utc == "Fri, Aug 5, 2022, 19:00 UTC")
epl_startindex <- nrow(epl_match_details) - epl_startdate[1]
epl_match_details <- tail(epl_match_details,epl_startindex)


epl_match_details <- epl_match_details[epl_match_details$event_type == "Goal",]
epl_match_details$matchid <- paste(epl_match_details$home_team,epl_match_details$away_team,sep = "-")

goal_mins <- epl_match_details %>% group_by(matchid) %>%
  summarise(goalmins = sum(min),
            n = n()

            )

unlink('E0_goalmins.csv')
write.csv(goal_mins,'E0_goalmins.csv')
library(xlsx)
options(java.parameters = "-Xmx4g")
write.xlsx(epl_match_details,'epl_match_details.xlsx')
View(epl_match_details)
#############################################################################################################
#B1
ger_match_details <- load_fotmob_match_details(
  country = "GER",
  league_name = "1. Bundesliga"
)
#select just the current season
ger_startdate <- which(ger_match_details$match_time_utc == "Fri, Aug 5, 2022, 18:35 UTC")
ger_startindex <- nrow(ger_match_details) - ger_startdate[1]
ger_match_details <- tail(ger_match_details,ger_startindex)


ger_match_details <- ger_match_details[ger_match_details$event_type == "Goal",]
ger_match_details$matchid <- paste(ger_match_details$home_team,ger_match_details$away_team,sep = "-")

goal_mins <- ger_match_details %>% group_by(matchid) %>%
  summarise(goalmins = sum(min),
            n = n()

  )

unlink('D1_goalmins.csv')
write.csv(goal_mins,'D1_goalmins.csv')
##################################################################################################
#I1
serieA_match_details <- load_fotmob_match_details(
  country = "ITA",
  league_name = "Serie A"
)
#select just the current season
serieA_startdate <- which(serieA_match_details$match_time_utc == "Sat, Aug 13, 2022, 16:30 UTC")
serieA_startindex <- nrow(serieA_match_details) - serieA_startdate[1]
serieA_match_details <- tail(serieA_match_details,serieA_startindex)


serieA_match_details <- serieA_match_details[serieA_match_details$event_type == "Goal",]
serieA_match_details$matchid <- paste(serieA_match_details$home_team,serieA_match_details$away_team,sep = "-")

goal_mins <- serieA_match_details %>% group_by(matchid) %>%
  summarise(goalmins = sum(min),
            n = n()

  )
View(goal_mins)
unlink('I1_goalmins.csv')
write.csv(goal_mins,'I1_goalmins.csv')
library(xlsx)
options(java.parameters = "-Xmx4g")
write.xlsx(serieA_match_details,'serieA_match_details.xlsx')
View(serieA_match_details)

serieA_match_details$match_id
View(fotmob_get_match_players(3900948))

View(fotmob_get_match_team_stats(3900948))



write.csv(sort(unique(epl_match_details$player_name)),'E0players20222023.csv')

library('xlsx')
write.xlsx(epl_match_details,'eplmatchdetails.xlsx')
View(goal_mins)
View(epl_match_details)

epl0 <- E0[E0$TG == '0',]
epl0$matchid <- paste(epl0$HomeTeam,epl0$AwayTeam, sep = "-")
write.csv(epl0,'epl0.csv')

colnames(epl_match_details)

View(fotmob_get_league_ids())
