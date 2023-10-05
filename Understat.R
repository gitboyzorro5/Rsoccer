library('worldfootballR')
# to get the EPL results:
epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2023)
dplyr::glimpse(epl_results)
View(epl_results)
understat_team_stats_
