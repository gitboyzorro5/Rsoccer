library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')

#league tables
#P-played W-wins D-draw L-loss F-goals for A-goals against GD-goal diff PTS-points
#B1
b1_teams
b1_games_played
#hwins and away wins
b1_home_wins <- c()
b1_away_wins <- c()
b1_home_draws <- c()
b1_away_draws <- c()
b1_home_loss <- c()
b1_away_loss <- c()



for (i_b1_wins in 1:length(b1_teams))
{

  b1_home_wins[i_b1_wins] <- nrow(B1[B1$HomeTeam == b1_teams[i_b1_wins] & B1$FTR == "H",])
  b1_away_wins[i_b1_wins] <- nrow(B1[B1$AwayTeam == b1_teams[i_b1_wins] & B1$FTR == "A",])
  b1_home_draws[i_b1_wins] <- nrow(B1[B1$HomeTeam == b1_teams[i_b1_wins] & B1$FTR == "D",])
  b1_away_draws[i_b1_wins] <- nrow(B1[B1$AwayTeam == b1_teams[i_b1_wins] & B1$FTR == "D",])
  b1_home_loss[i_b1_wins] <- nrow(B1[B1$HomeTeam == b1_teams[i_b1_wins] & B1$FTR == "A",])
  b1_away_loss[i_b1_wins] <- nrow(B1[B1$AwayTeam == b1_teams[i_b1_wins] & B1$FTR == "H",])

}

b1_total_wins <- b1_home_wins + b1_away_wins
b1_total_draws <- b1_home_draws + b1_away_draws
b1_total_loss <- b1_home_loss + b1_away_loss

b1_league_table <- cbind(b1_teams,b1_games_played,b1_total_wins,b1_total_draws,b1_total_loss)
b1_GS <- b1_scoring$TGS
b1_GC <-b1_conceding$TGC
b1_GD <- b1_scoring$TGS - b1_conceding$TGC
b1_PTS <- (b1_total_wins*3) + (b1_total_draws*1)
b1_league_table <- cbind(b1_league_table,b1_GS,b1_GC,b1_GD,b1_PTS)
b1_league_table <- as.data.frame(b1_league_table)
#rename the columns
names(b1_league_table)[names(b1_league_table) == "b1_teams"] <- "Team"
names(b1_league_table)[names(b1_league_table) == "b1_games_played"] <- "P"
names(b1_league_table)[names(b1_league_table) == "b1_total_wins"] <- "W"
names(b1_league_table)[names(b1_league_table) == "b1_total_draws"] <- "D"
names(b1_league_table)[names(b1_league_table) == "b1_total_loss"] <- "L"
names(b1_league_table)[names(b1_league_table) == "b1_GS"] <- "F"
names(b1_league_table)[names(b1_league_table) == "b1_GC"] <- "A"
points_b1 <- b1_league_table[order(b1_league_table$b1_PTS, decreasing = TRUE),]
row.names(points_b1) <- 1:length(b1_teams)

#D1
d1_teams
d1_games_played
#hwins and away wins
d1_home_wins <- c()
d1_away_wins <- c()
d1_home_draws <- c()
d1_away_draws <- c()
d1_home_loss <- c()
d1_away_loss <- c()



for (i_d1_wins in 1:length(d1_teams))
{

  d1_home_wins[i_d1_wins] <- nrow(D1[D1$HomeTeam == d1_teams[i_d1_wins] & D1$FTR == "H",])
  d1_away_wins[i_d1_wins] <- nrow(D1[D1$AwayTeam == d1_teams[i_d1_wins] & D1$FTR == "A",])
  d1_home_draws[i_d1_wins] <- nrow(D1[D1$HomeTeam == d1_teams[i_d1_wins] & D1$FTR == "D",])
  d1_away_draws[i_d1_wins] <- nrow(D1[D1$AwayTeam == d1_teams[i_d1_wins] & D1$FTR == "D",])
  d1_home_loss[i_d1_wins] <- nrow(D1[D1$HomeTeam == d1_teams[i_d1_wins] & D1$FTR == "A",])
  d1_away_loss[i_d1_wins] <- nrow(D1[D1$AwayTeam == d1_teams[i_d1_wins] & D1$FTR == "H",])

}

d1_total_wins <- d1_home_wins + d1_away_wins
d1_total_draws <- d1_home_draws + d1_away_draws
d1_total_loss <- d1_home_loss + d1_away_loss

d1_league_table <- cbind(d1_teams,d1_games_played,d1_total_wins,d1_total_draws,d1_total_loss)
d1_GS <- d1_scoring$TGS
d1_GC <-d1_conceding$TGC
d1_GD <- d1_scoring$TGS - d1_conceding$TGC
d1_PTS <- (d1_total_wins*3) + (d1_total_draws*1)
d1_league_table <- cbind(d1_league_table,d1_GS,d1_GC,d1_GD,d1_PTS)
d1_league_table <- as.data.frame(d1_league_table)
#rename the columns
names(d1_league_table)[names(d1_league_table) == "d1_teams"] <- "Team"
names(d1_league_table)[names(d1_league_table) == "d1_games_played"] <- "P"
names(d1_league_table)[names(d1_league_table) == "d1_total_wins"] <- "W"
names(d1_league_table)[names(d1_league_table) == "d1_total_draws"] <- "D"
names(d1_league_table)[names(d1_league_table) == "d1_total_loss"] <- "L"
names(d1_league_table)[names(d1_league_table) == "d1_GS"] <- "F"
names(d1_league_table)[names(d1_league_table) == "d1_GC"] <- "A"
points_d1 <- d1_league_table[order(d1_league_table$d1_PTS, decreasing = TRUE),]
row.names(points_d1) <- 1:length(d1_teams)
View(points_d1)


