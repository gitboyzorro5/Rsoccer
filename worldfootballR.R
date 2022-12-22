

library('worldfootballR')
library('dplyr')

epl_match_details <- load_fotmob_match_details(
  country = "ENG",
  league_name = "Premier League"
)
#select just the current season

epl_match_details <- tail(epl_match_details,419)

epl_match_details <- epl_match_details[epl_match_details$event_type == "Goal",]

epl_match_details$matchid <- paste(epl_match_details$home_team,epl_match_details$away_team,sep = "-")

goal_mins <- epl_match_details %>% group_by(matchid) %>%
  summarise(goalmins = sum(min),
            n = n()

            )

write.csv(goal_mins,'E0_goalmins.csv')


View(goal_mins)
View(epl_match_details)

epl0 <- E0[E0$TG == '0',]
epl0$matchid <- paste(epl0$HomeTeam,epl0$AwayTeam, sep = "-")
write.csv(epl0,'epl0.csv')
