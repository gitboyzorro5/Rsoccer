
install.packages('worldfootballR')

library('worldfootballR')

epl_match_details <- load_fotmob_match_details(
  country = "ENG",
  league_name = "Premier League"
)

View(epl_match_details)

summary(epl_match_details)
sort(unique(epl_match_details$league_name))
