#install.packages(c("httr", "jsonlite", "dplyr"))
library(httr)
library(jsonlite)
library(dplyr)

API_KEY <- "b4e076e26ebb0393cb1313dcdabee066"  # Get free key at api-football.com

get_epl_goals <- function(fixture_id) {
  url <- paste0("https://api-football-v1.p.rapidapi.com/v3/fixtures/events?fixture=", fixture_id)

  res <- GET(
    url,
    add_headers(
      "X-RapidAPI-Key" = API_KEY,
      "X-RapidAPI-Host" = "api-football-v1.p.rapidapi.com"
    )
  )

  data <- fromJSON(content(res, "text"), flatten = TRUE)
  events <- data$response

  # Filter goals only
  goals <- events %>%
    filter(type == "Goal") %>%
    select(
      minute     = time.elapsed,
      team       = team.name,
      player     = player.name,
      shirt_no   = player.number,   # shirt number
      assist     = assist.name,
      goal_type  = detail
    )

  return(goals)
}
fixtures_res
# Example: Get goals for a specific fixture
# First get fixture IDs for EPL
fixtures_url <- "https://api-football-v1.p.rapidapi.com/v3/fixtures?league=39&season=2024"
fixtures_res <- GET(fixtures_url, add_headers("X-RapidAPI-Key" = API_KEY, "X-RapidAPI-Host" = "api-football-v1.p.rapidapi.com"))
fixtures <- fromJSON(content(fixtures_res, "text"), flatten = TRUE)$response

# Loop through fixtures and collect all goals
all_goals <- bind_rows(lapply(fixtures$fixture.id[1:10], get_epl_goals))

# Final dataframe
print(all_goals)
#   minute   team          player  shirt_no  assist         goal_type
#   <int>    <chr>         <chr>   <int>     <chr>          <chr>
# 1  14      Arsenal FC    Saka B.  7        Havertz K.     Normal Goal
# 2  67      Chelsea FC    Palmer C. 20      ...            Normal Goal
