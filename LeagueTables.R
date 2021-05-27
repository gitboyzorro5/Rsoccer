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

#D2
d2_teams
d2_games_played
#hwins and away wins
d2_home_wins <- c()
d2_away_wins <- c()
d2_home_draws <- c()
d2_away_draws <- c()
d2_home_loss <- c()
d2_away_loss <- c()



for (i_d2_wins in 1:length(d2_teams))
{

  d2_home_wins[i_d2_wins] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_wins] & D2$FTR == "H",])
  d2_away_wins[i_d2_wins] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_wins] & D2$FTR == "A",])
  d2_home_draws[i_d2_wins] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_wins] & D2$FTR == "D",])
  d2_away_draws[i_d2_wins] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_wins] & D2$FTR == "D",])
  d2_home_loss[i_d2_wins] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_wins] & D2$FTR == "A",])
  d2_away_loss[i_d2_wins] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_wins] & D2$FTR == "H",])

}

d2_total_wins <- d2_home_wins + d2_away_wins
d2_total_draws <- d2_home_draws + d2_away_draws
d2_total_loss <- d2_home_loss + d2_away_loss

d2_league_table <- cbind(d2_teams,d2_games_played,d2_total_wins,d2_total_draws,d2_total_loss)
d2_GS <- d2_scoring$TGS
d2_GC <-d2_conceding$TGC
d2_GD <- d2_scoring$TGS - d2_conceding$TGC
d2_PTS <- (d2_total_wins*3) + (d2_total_draws*1)
d2_league_table <- cbind(d2_league_table,d2_GS,d2_GC,d2_GD,d2_PTS)
d2_league_table <- as.data.frame(d2_league_table)
#rename the columns
names(d2_league_table)[names(d2_league_table) == "d2_teams"] <- "Team"
names(d2_league_table)[names(d2_league_table) == "d2_games_played"] <- "P"
names(d2_league_table)[names(d2_league_table) == "d2_total_wins"] <- "W"
names(d2_league_table)[names(d2_league_table) == "d2_total_draws"] <- "D"
names(d2_league_table)[names(d2_league_table) == "d2_total_loss"] <- "L"
names(d2_league_table)[names(d2_league_table) == "d2_GS"] <- "F"
names(d2_league_table)[names(d2_league_table) == "d2_GC"] <- "A"
points_d2 <- d2_league_table[order(d2_league_table$d2_PTS, decreasing = TRUE),]
row.names(points_d2) <- 1:length(d2_teams)

#E0
e0_teams
e0_games_played
#hwins and away wins
e0_home_wins <- c()
e0_away_wins <- c()
e0_home_draws <- c()
e0_away_draws <- c()
e0_home_loss <- c()
e0_away_loss <- c()



for (i_e0_wins in 1:length(e0_teams))
{

  e0_home_wins[i_e0_wins] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_wins] & E0$FTR == "H",])
  e0_away_wins[i_e0_wins] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_wins] & E0$FTR == "A",])
  e0_home_draws[i_e0_wins] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_wins] & E0$FTR == "D",])
  e0_away_draws[i_e0_wins] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_wins] & E0$FTR == "D",])
  e0_home_loss[i_e0_wins] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_wins] & E0$FTR == "A",])
  e0_away_loss[i_e0_wins] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_wins] & E0$FTR == "H",])

}

e0_total_wins <- e0_home_wins + e0_away_wins
e0_total_draws <- e0_home_draws + e0_away_draws
e0_total_loss <- e0_home_loss + e0_away_loss

e0_league_table <- cbind(e0_teams,e0_games_played,e0_total_wins,e0_total_draws,e0_total_loss)
e0_GS <- e0_scoring$TGS
e0_GC <-e0_conceding$TGC
e0_GD <- e0_scoring$TGS - e0_conceding$TGC
e0_PTS <- (e0_total_wins*3) + (e0_total_draws*1)
e0_league_table <- cbind(e0_league_table,e0_GS,e0_GC,e0_GD,e0_PTS)
e0_league_table <- as.data.frame(e0_league_table)
#rename the columns
names(e0_league_table)[names(e0_league_table) == "e0_teams"] <- "Team"
names(e0_league_table)[names(e0_league_table) == "e0_games_played"] <- "P"
names(e0_league_table)[names(e0_league_table) == "e0_total_wins"] <- "W"
names(e0_league_table)[names(e0_league_table) == "e0_total_draws"] <- "D"
names(e0_league_table)[names(e0_league_table) == "e0_total_loss"] <- "L"
names(e0_league_table)[names(e0_league_table) == "e0_GS"] <- "F"
names(e0_league_table)[names(e0_league_table) == "e0_GC"] <- "A"
points_e0 <- e0_league_table[order(e0_league_table$e0_PTS, decreasing = TRUE),]
row.names(points_e0) <- 1:length(e0_teams)

#E1
e1_teams
e1_games_played
#hwins and away wins
e1_home_wins <- c()
e1_away_wins <- c()
e1_home_draws <- c()
e1_away_draws <- c()
e1_home_loss <- c()
e1_away_loss <- c()



for (i_e1_wins in 1:length(e1_teams))
{

  e1_home_wins[i_e1_wins] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_wins] & E1$FTR == "H",])
  e1_away_wins[i_e1_wins] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_wins] & E1$FTR == "A",])
  e1_home_draws[i_e1_wins] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_wins] & E1$FTR == "D",])
  e1_away_draws[i_e1_wins] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_wins] & E1$FTR == "D",])
  e1_home_loss[i_e1_wins] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_wins] & E1$FTR == "A",])
  e1_away_loss[i_e1_wins] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_wins] & E1$FTR == "H",])

}

e1_total_wins <- e1_home_wins + e1_away_wins
e1_total_draws <- e1_home_draws + e1_away_draws
e1_total_loss <- e1_home_loss + e1_away_loss

e1_league_table <- cbind(e1_teams,e1_games_played,e1_total_wins,e1_total_draws,e1_total_loss)
e1_GS <- e1_scoring$TGS
e1_GC <-e1_conceding$TGC
e1_GD <- e1_scoring$TGS - e1_conceding$TGC
e1_PTS <- (e1_total_wins*3) + (e1_total_draws*1)
e1_league_table <- cbind(e1_league_table,e1_GS,e1_GC,e1_GD,e1_PTS)
e1_league_table <- as.data.frame(e1_league_table)
#rename the columns
names(e1_league_table)[names(e1_league_table) == "e1_teams"] <- "Team"
names(e1_league_table)[names(e1_league_table) == "e1_games_played"] <- "P"
names(e1_league_table)[names(e1_league_table) == "e1_total_wins"] <- "W"
names(e1_league_table)[names(e1_league_table) == "e1_total_draws"] <- "D"
names(e1_league_table)[names(e1_league_table) == "e1_total_loss"] <- "L"
names(e1_league_table)[names(e1_league_table) == "e1_GS"] <- "F"
names(e1_league_table)[names(e1_league_table) == "e1_GC"] <- "A"
points_e1 <- e1_league_table[order(e1_league_table$e1_PTS, decreasing = TRUE),]
row.names(points_e1) <- 1:length(e1_teams)

#E2
e2_teams
e2_games_played
#hwins and away wins
e2_home_wins <- c()
e2_away_wins <- c()
e2_home_draws <- c()
e2_away_draws <- c()
e2_home_loss <- c()
e2_away_loss <- c()



for (i_e2_wins in 1:length(e2_teams))
{

  e2_home_wins[i_e2_wins] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_wins] & E2$FTR == "H",])
  e2_away_wins[i_e2_wins] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_wins] & E2$FTR == "A",])
  e2_home_draws[i_e2_wins] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_wins] & E2$FTR == "D",])
  e2_away_draws[i_e2_wins] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_wins] & E2$FTR == "D",])
  e2_home_loss[i_e2_wins] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_wins] & E2$FTR == "A",])
  e2_away_loss[i_e2_wins] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_wins] & E2$FTR == "H",])

}

e2_total_wins <- e2_home_wins + e2_away_wins
e2_total_draws <- e2_home_draws + e2_away_draws
e2_total_loss <- e2_home_loss + e2_away_loss

e2_league_table <- cbind(e2_teams,e2_games_played,e2_total_wins,e2_total_draws,e2_total_loss)
e2_GS <- e2_scoring$TGS
e2_GC <-e2_conceding$TGC
e2_GD <- e2_scoring$TGS - e2_conceding$TGC
e2_PTS <- (e2_total_wins*3) + (e2_total_draws*1)
e2_league_table <- cbind(e2_league_table,e2_GS,e2_GC,e2_GD,e2_PTS)
e2_league_table <- as.data.frame(e2_league_table)
#rename the columns
names(e2_league_table)[names(e2_league_table) == "e2_teams"] <- "Team"
names(e2_league_table)[names(e2_league_table) == "e2_games_played"] <- "P"
names(e2_league_table)[names(e2_league_table) == "e2_total_wins"] <- "W"
names(e2_league_table)[names(e2_league_table) == "e2_total_draws"] <- "D"
names(e2_league_table)[names(e2_league_table) == "e2_total_loss"] <- "L"
names(e2_league_table)[names(e2_league_table) == "e2_GS"] <- "F"
names(e2_league_table)[names(e2_league_table) == "e2_GC"] <- "A"
points_e2 <- e2_league_table[order(e2_league_table$e2_PTS, decreasing = TRUE),]
row.names(points_e2) <- 1:length(e2_teams)

#E3
e3_teams
e3_games_played
#hwins and away wins
e3_home_wins <- c()
e3_away_wins <- c()
e3_home_draws <- c()
e3_away_draws <- c()
e3_home_loss <- c()
e3_away_loss <- c()



for (i_e3_wins in 1:length(e3_teams))
{

  e3_home_wins[i_e3_wins] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_wins] & E3$FTR == "H",])
  e3_away_wins[i_e3_wins] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_wins] & E3$FTR == "A",])
  e3_home_draws[i_e3_wins] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_wins] & E3$FTR == "D",])
  e3_away_draws[i_e3_wins] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_wins] & E3$FTR == "D",])
  e3_home_loss[i_e3_wins] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_wins] & E3$FTR == "A",])
  e3_away_loss[i_e3_wins] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_wins] & E3$FTR == "H",])

}

e3_total_wins <- e3_home_wins + e3_away_wins
e3_total_draws <- e3_home_draws + e3_away_draws
e3_total_loss <- e3_home_loss + e3_away_loss

e3_league_table <- cbind(e3_teams,e3_games_played,e3_total_wins,e3_total_draws,e3_total_loss)
e3_GS <- e3_scoring$TGS
e3_GC <-e3_conceding$TGC
e3_GD <- e3_scoring$TGS - e3_conceding$TGC
e3_PTS <- (e3_total_wins*3) + (e3_total_draws*1)
e3_league_table <- cbind(e3_league_table,e3_GS,e3_GC,e3_GD,e3_PTS)
e3_league_table <- as.data.frame(e3_league_table)
#rename the columns
names(e3_league_table)[names(e3_league_table) == "e3_teams"] <- "Team"
names(e3_league_table)[names(e3_league_table) == "e3_games_played"] <- "P"
names(e3_league_table)[names(e3_league_table) == "e3_total_wins"] <- "W"
names(e3_league_table)[names(e3_league_table) == "e3_total_draws"] <- "D"
names(e3_league_table)[names(e3_league_table) == "e3_total_loss"] <- "L"
names(e3_league_table)[names(e3_league_table) == "e3_GS"] <- "F"
names(e3_league_table)[names(e3_league_table) == "e3_GC"] <- "A"
points_e3 <- e3_league_table[order(e3_league_table$e3_PTS, decreasing = TRUE),]
row.names(points_e3) <- 1:length(e3_teams)

#EC
ec_teams
ec_games_played
#hwins and away wins
ec_home_wins <- c()
ec_away_wins <- c()
ec_home_draws <- c()
ec_away_draws <- c()
ec_home_loss <- c()
ec_away_loss <- c()



for (i_ec_wins in 1:length(ec_teams))
{

  ec_home_wins[i_ec_wins] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_wins] & EC$FTR == "H",])
  ec_away_wins[i_ec_wins] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_wins] & EC$FTR == "A",])
  ec_home_draws[i_ec_wins] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_wins] & EC$FTR == "D",])
  ec_away_draws[i_ec_wins] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_wins] & EC$FTR == "D",])
  ec_home_loss[i_ec_wins] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_wins] & EC$FTR == "A",])
  ec_away_loss[i_ec_wins] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_wins] & EC$FTR == "H",])

}

ec_total_wins <- ec_home_wins + ec_away_wins
ec_total_draws <- ec_home_draws + ec_away_draws
ec_total_loss <- ec_home_loss + ec_away_loss

ec_league_table <- cbind(ec_teams,ec_games_played,ec_total_wins,ec_total_draws,ec_total_loss)
ec_GS <- ec_scoring$TGS
ec_GC <-ec_conceding$TGC
ec_GD <- ec_scoring$TGS - ec_conceding$TGC
ec_PTS <- (ec_total_wins*3) + (ec_total_draws*1)
ec_league_table <- cbind(ec_league_table,ec_GS,ec_GC,ec_GD,ec_PTS)
ec_league_table <- as.data.frame(ec_league_table)
#rename the columns
names(ec_league_table)[names(ec_league_table) == "ec_teams"] <- "Team"
names(ec_league_table)[names(ec_league_table) == "ec_games_played"] <- "P"
names(ec_league_table)[names(ec_league_table) == "ec_total_wins"] <- "W"
names(ec_league_table)[names(ec_league_table) == "ec_total_draws"] <- "D"
names(ec_league_table)[names(ec_league_table) == "ec_total_loss"] <- "L"
names(ec_league_table)[names(ec_league_table) == "ec_GS"] <- "F"
names(ec_league_table)[names(ec_league_table) == "ec_GC"] <- "A"
points_ec <- ec_league_table[order(ec_league_table$ec_PTS, decreasing = TRUE),]
row.names(points_ec) <- 1:length(ec_teams)

#F1
f1_teams
f1_games_played
#hwins and away wins
f1_home_wins <- c()
f1_away_wins <- c()
f1_home_draws <- c()
f1_away_draws <- c()
f1_home_loss <- c()
f1_away_loss <- c()



for (i_f1_wins in 1:length(f1_teams))
{

  f1_home_wins[i_f1_wins] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_wins] & F1$FTR == "H",])
  f1_away_wins[i_f1_wins] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_wins] & F1$FTR == "A",])
  f1_home_draws[i_f1_wins] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_wins] & F1$FTR == "D",])
  f1_away_draws[i_f1_wins] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_wins] & F1$FTR == "D",])
  f1_home_loss[i_f1_wins] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_wins] & F1$FTR == "A",])
  f1_away_loss[i_f1_wins] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_wins] & F1$FTR == "H",])

}

f1_total_wins <- f1_home_wins + f1_away_wins
f1_total_draws <- f1_home_draws + f1_away_draws
f1_total_loss <- f1_home_loss + f1_away_loss

f1_league_table <- cbind(f1_teams,f1_games_played,f1_total_wins,f1_total_draws,f1_total_loss)
f1_GS <- f1_scoring$TGS
f1_GC <-f1_conceding$TGC
f1_GD <- f1_scoring$TGS - f1_conceding$TGC
f1_PTS <- (f1_total_wins*3) + (f1_total_draws*1)
f1_league_table <- cbind(f1_league_table,f1_GS,f1_GC,f1_GD,f1_PTS)
f1_league_table <- as.data.frame(f1_league_table)
#rename the columns
names(f1_league_table)[names(f1_league_table) == "f1_teams"] <- "Team"
names(f1_league_table)[names(f1_league_table) == "f1_games_played"] <- "P"
names(f1_league_table)[names(f1_league_table) == "f1_total_wins"] <- "W"
names(f1_league_table)[names(f1_league_table) == "f1_total_draws"] <- "D"
names(f1_league_table)[names(f1_league_table) == "f1_total_loss"] <- "L"
names(f1_league_table)[names(f1_league_table) == "f1_GS"] <- "F"
names(f1_league_table)[names(f1_league_table) == "f1_GC"] <- "A"
points_f1 <- f1_league_table[order(f1_league_table$f1_PTS, decreasing = TRUE),]
row.names(points_f1) <- 1:length(f1_teams)

#F2
f2_teams
f2_games_played
#hwins and away wins
f2_home_wins <- c()
f2_away_wins <- c()
f2_home_draws <- c()
f2_away_draws <- c()
f2_home_loss <- c()
f2_away_loss <- c()



for (i_f2_wins in 1:length(f2_teams))
{

  f2_home_wins[i_f2_wins] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_wins] & F2$FTR == "H",])
  f2_away_wins[i_f2_wins] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_wins] & F2$FTR == "A",])
  f2_home_draws[i_f2_wins] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_wins] & F2$FTR == "D",])
  f2_away_draws[i_f2_wins] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_wins] & F2$FTR == "D",])
  f2_home_loss[i_f2_wins] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_wins] & F2$FTR == "A",])
  f2_away_loss[i_f2_wins] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_wins] & F2$FTR == "H",])

}

f2_total_wins <- f2_home_wins + f2_away_wins
f2_total_draws <- f2_home_draws + f2_away_draws
f2_total_loss <- f2_home_loss + f2_away_loss

f2_league_table <- cbind(f2_teams,f2_games_played,f2_total_wins,f2_total_draws,f2_total_loss)
f2_GS <- f2_scoring$TGS
f2_GC <-f2_conceding$TGC
f2_GD <- f2_scoring$TGS - f2_conceding$TGC
f2_PTS <- (f2_total_wins*3) + (f2_total_draws*1)
f2_league_table <- cbind(f2_league_table,f2_GS,f2_GC,f2_GD,f2_PTS)
f2_league_table <- as.data.frame(f2_league_table)
#rename the columns
names(f2_league_table)[names(f2_league_table) == "f2_teams"] <- "Team"
names(f2_league_table)[names(f2_league_table) == "f2_games_played"] <- "P"
names(f2_league_table)[names(f2_league_table) == "f2_total_wins"] <- "W"
names(f2_league_table)[names(f2_league_table) == "f2_total_draws"] <- "D"
names(f2_league_table)[names(f2_league_table) == "f2_total_loss"] <- "L"
names(f2_league_table)[names(f2_league_table) == "f2_GS"] <- "F"
names(f2_league_table)[names(f2_league_table) == "f2_GC"] <- "A"
points_f2 <- f2_league_table[order(f2_league_table$f2_PTS, decreasing = TRUE),]
row.names(points_f2) <- 1:length(f2_teams)

#G1
g1_teams
g1_games_played
#hwins and away wins
g1_home_wins <- c()
g1_away_wins <- c()
g1_home_draws <- c()
g1_away_draws <- c()
g1_home_loss <- c()
g1_away_loss <- c()



for (i_g1_wins in 1:length(g1_teams))
{

  g1_home_wins[i_g1_wins] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_wins] & G1$FTR == "H",])
  g1_away_wins[i_g1_wins] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_wins] & G1$FTR == "A",])
  g1_home_draws[i_g1_wins] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_wins] & G1$FTR == "D",])
  g1_away_draws[i_g1_wins] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_wins] & G1$FTR == "D",])
  g1_home_loss[i_g1_wins] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_wins] & G1$FTR == "A",])
  g1_away_loss[i_g1_wins] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_wins] & G1$FTR == "H",])

}

g1_total_wins <- g1_home_wins + g1_away_wins
g1_total_draws <- g1_home_draws + g1_away_draws
g1_total_loss <- g1_home_loss + g1_away_loss

g1_league_table <- cbind(g1_teams,g1_games_played,g1_total_wins,g1_total_draws,g1_total_loss)
g1_GS <- g1_scoring$TGS
g1_GC <-g1_conceding$TGC
g1_GD <- g1_scoring$TGS - g1_conceding$TGC
g1_PTS <- (g1_total_wins*3) + (g1_total_draws*1)
g1_league_table <- cbind(g1_league_table,g1_GS,g1_GC,g1_GD,g1_PTS)
g1_league_table <- as.data.frame(g1_league_table)
#rename the columns
names(g1_league_table)[names(g1_league_table) == "g1_teams"] <- "Team"
names(g1_league_table)[names(g1_league_table) == "g1_games_played"] <- "P"
names(g1_league_table)[names(g1_league_table) == "g1_total_wins"] <- "W"
names(g1_league_table)[names(g1_league_table) == "g1_total_draws"] <- "D"
names(g1_league_table)[names(g1_league_table) == "g1_total_loss"] <- "L"
names(g1_league_table)[names(g1_league_table) == "g1_GS"] <- "F"
names(g1_league_table)[names(g1_league_table) == "g1_GC"] <- "A"
points_g1 <- g1_league_table[order(g1_league_table$g1_PTS, decreasing = TRUE),]
row.names(points_g1) <- 1:length(g1_teams)

#I1
i1_teams
i1_games_played
#hwins and away wins
i1_home_wins <- c()
i1_away_wins <- c()
i1_home_draws <- c()
i1_away_draws <- c()
i1_home_loss <- c()
i1_away_loss <- c()



for (i_i1_wins in 1:length(i1_teams))
{

  i1_home_wins[i_i1_wins] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_wins] & I1$FTR == "H",])
  i1_away_wins[i_i1_wins] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_wins] & I1$FTR == "A",])
  i1_home_draws[i_i1_wins] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_wins] & I1$FTR == "D",])
  i1_away_draws[i_i1_wins] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_wins] & I1$FTR == "D",])
  i1_home_loss[i_i1_wins] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_wins] & I1$FTR == "A",])
  i1_away_loss[i_i1_wins] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_wins] & I1$FTR == "H",])

}

i1_total_wins <- i1_home_wins + i1_away_wins
i1_total_draws <- i1_home_draws + i1_away_draws
i1_total_loss <- i1_home_loss + i1_away_loss

i1_league_table <- cbind(i1_teams,i1_games_played,i1_total_wins,i1_total_draws,i1_total_loss)
i1_GS <- i1_scoring$TGS
i1_GC <-i1_conceding$TGC
i1_GD <- i1_scoring$TGS - i1_conceding$TGC
i1_PTS <- (i1_total_wins*3) + (i1_total_draws*1)
i1_league_table <- cbind(i1_league_table,i1_GS,i1_GC,i1_GD,i1_PTS)
i1_league_table <- as.data.frame(i1_league_table)
#rename the columns
names(i1_league_table)[names(i1_league_table) == "i1_teams"] <- "Team"
names(i1_league_table)[names(i1_league_table) == "i1_games_played"] <- "P"
names(i1_league_table)[names(i1_league_table) == "i1_total_wins"] <- "W"
names(i1_league_table)[names(i1_league_table) == "i1_total_draws"] <- "D"
names(i1_league_table)[names(i1_league_table) == "i1_total_loss"] <- "L"
names(i1_league_table)[names(i1_league_table) == "i1_GS"] <- "F"
names(i1_league_table)[names(i1_league_table) == "i1_GC"] <- "A"
points_i1 <- i1_league_table[order(i1_league_table$i1_PTS, decreasing = TRUE),]
row.names(points_i1) <- 1:length(i1_teams)

#I2
i2_teams
i2_games_played
#hwins and away wins
i2_home_wins <- c()
i2_away_wins <- c()
i2_home_draws <- c()
i2_away_draws <- c()
i2_home_loss <- c()
i2_away_loss <- c()



for (i_i2_wins in 1:length(i2_teams))
{

  i2_home_wins[i_i2_wins] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_wins] & I2$FTR == "H",])
  i2_away_wins[i_i2_wins] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_wins] & I2$FTR == "A",])
  i2_home_draws[i_i2_wins] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_wins] & I2$FTR == "D",])
  i2_away_draws[i_i2_wins] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_wins] & I2$FTR == "D",])
  i2_home_loss[i_i2_wins] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_wins] & I2$FTR == "A",])
  i2_away_loss[i_i2_wins] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_wins] & I2$FTR == "H",])

}

i2_total_wins <- i2_home_wins + i2_away_wins
i2_total_draws <- i2_home_draws + i2_away_draws
i2_total_loss <- i2_home_loss + i2_away_loss

i2_league_table <- cbind(i2_teams,i2_games_played,i2_total_wins,i2_total_draws,i2_total_loss)
i2_GS <- i2_scoring$TGS
i2_GC <-i2_conceding$TGC
i2_GD <- i2_scoring$TGS - i2_conceding$TGC
i2_PTS <- (i2_total_wins*3) + (i2_total_draws*1)
i2_league_table <- cbind(i2_league_table,i2_GS,i2_GC,i2_GD,i2_PTS)
i2_league_table <- as.data.frame(i2_league_table)
#rename the columns
names(i2_league_table)[names(i2_league_table) == "i2_teams"] <- "Team"
names(i2_league_table)[names(i2_league_table) == "i2_games_played"] <- "P"
names(i2_league_table)[names(i2_league_table) == "i2_total_wins"] <- "W"
names(i2_league_table)[names(i2_league_table) == "i2_total_draws"] <- "D"
names(i2_league_table)[names(i2_league_table) == "i2_total_loss"] <- "L"
names(i2_league_table)[names(i2_league_table) == "i2_GS"] <- "F"
names(i2_league_table)[names(i2_league_table) == "i2_GC"] <- "A"
points_i2 <- i2_league_table[order(i2_league_table$i2_PTS, decreasing = TRUE),]
row.names(points_i2) <- 1:length(i2_teams)

#N1
n1_teams
n1_games_played
#hwins and away wins
n1_home_wins <- c()
n1_away_wins <- c()
n1_home_draws <- c()
n1_away_draws <- c()
n1_home_loss <- c()
n1_away_loss <- c()



for (i_n1_wins in 1:length(n1_teams))
{

  n1_home_wins[i_n1_wins] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_wins] & N1$FTR == "H",])
  n1_away_wins[i_n1_wins] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_wins] & N1$FTR == "A",])
  n1_home_draws[i_n1_wins] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_wins] & N1$FTR == "D",])
  n1_away_draws[i_n1_wins] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_wins] & N1$FTR == "D",])
  n1_home_loss[i_n1_wins] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_wins] & N1$FTR == "A",])
  n1_away_loss[i_n1_wins] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_wins] & N1$FTR == "H",])

}

n1_total_wins <- n1_home_wins + n1_away_wins
n1_total_draws <- n1_home_draws + n1_away_draws
n1_total_loss <- n1_home_loss + n1_away_loss

n1_league_table <- cbind(n1_teams,n1_games_played,n1_total_wins,n1_total_draws,n1_total_loss)
n1_GS <- n1_scoring$TGS
n1_GC <-n1_conceding$TGC
n1_GD <- n1_scoring$TGS - n1_conceding$TGC
n1_PTS <- (n1_total_wins*3) + (n1_total_draws*1)
n1_league_table <- cbind(n1_league_table,n1_GS,n1_GC,n1_GD,n1_PTS)
n1_league_table <- as.data.frame(n1_league_table)
#rename the columns
names(n1_league_table)[names(n1_league_table) == "n1_teams"] <- "Team"
names(n1_league_table)[names(n1_league_table) == "n1_games_played"] <- "P"
names(n1_league_table)[names(n1_league_table) == "n1_total_wins"] <- "W"
names(n1_league_table)[names(n1_league_table) == "n1_total_draws"] <- "D"
names(n1_league_table)[names(n1_league_table) == "n1_total_loss"] <- "L"
names(n1_league_table)[names(n1_league_table) == "n1_GS"] <- "F"
names(n1_league_table)[names(n1_league_table) == "n1_GC"] <- "A"
points_n1 <- n1_league_table[order(n1_league_table$n1_PTS, decreasing = TRUE),]
row.names(points_n1) <- 1:length(n1_teams)

#P1
p1_teams
p1_games_played
#hwins and away wins
p1_home_wins <- c()
p1_away_wins <- c()
p1_home_draws <- c()
p1_away_draws <- c()
p1_home_loss <- c()
p1_away_loss <- c()



for (i_p1_wins in 1:length(p1_teams))
{

  p1_home_wins[i_p1_wins] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_wins] & P1$FTR == "H",])
  p1_away_wins[i_p1_wins] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_wins] & P1$FTR == "A",])
  p1_home_draws[i_p1_wins] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_wins] & P1$FTR == "D",])
  p1_away_draws[i_p1_wins] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_wins] & P1$FTR == "D",])
  p1_home_loss[i_p1_wins] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_wins] & P1$FTR == "A",])
  p1_away_loss[i_p1_wins] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_wins] & P1$FTR == "H",])

}

p1_total_wins <- p1_home_wins + p1_away_wins
p1_total_draws <- p1_home_draws + p1_away_draws
p1_total_loss <- p1_home_loss + p1_away_loss

p1_league_table <- cbind(p1_teams,p1_games_played,p1_total_wins,p1_total_draws,p1_total_loss)
p1_GS <- p1_scoring$TGS
p1_GC <-p1_conceding$TGC
p1_GD <- p1_scoring$TGS - p1_conceding$TGC
p1_PTS <- (p1_total_wins*3) + (p1_total_draws*1)
p1_league_table <- cbind(p1_league_table,p1_GS,p1_GC,p1_GD,p1_PTS)
p1_league_table <- as.data.frame(p1_league_table)
#rename the columns
names(p1_league_table)[names(p1_league_table) == "p1_teams"] <- "Team"
names(p1_league_table)[names(p1_league_table) == "p1_games_played"] <- "P"
names(p1_league_table)[names(p1_league_table) == "p1_total_wins"] <- "W"
names(p1_league_table)[names(p1_league_table) == "p1_total_draws"] <- "D"
names(p1_league_table)[names(p1_league_table) == "p1_total_loss"] <- "L"
names(p1_league_table)[names(p1_league_table) == "p1_GS"] <- "F"
names(p1_league_table)[names(p1_league_table) == "p1_GC"] <- "A"
points_p1 <- p1_league_table[order(p1_league_table$p1_PTS, decreasing = TRUE),]
row.names(points_p1) <- 1:length(p1_teams)

#SC0
sc0_teams
sc0_games_played
#hwins and away wins
sc0_home_wins <- c()
sc0_away_wins <- c()
sc0_home_draws <- c()
sc0_away_draws <- c()
sc0_home_loss <- c()
sc0_away_loss <- c()



for (i_sc0_wins in 1:length(sc0_teams))
{

  sc0_home_wins[i_sc0_wins] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "H",])
  sc0_away_wins[i_sc0_wins] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "A",])
  sc0_home_draws[i_sc0_wins] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "D",])
  sc0_away_draws[i_sc0_wins] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "D",])
  sc0_home_loss[i_sc0_wins] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "A",])
  sc0_away_loss[i_sc0_wins] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "H",])

}

sc0_total_wins <- sc0_home_wins + sc0_away_wins
sc0_total_draws <- sc0_home_draws + sc0_away_draws
sc0_total_loss <- sc0_home_loss + sc0_away_loss

sc0_league_table <- cbind(sc0_teams,sc0_games_played,sc0_total_wins,sc0_total_draws,sc0_total_loss)
sc0_GS <- sc0_scoring$TGS
sc0_GC <-sc0_conceding$TGC
sc0_GD <- sc0_scoring$TGS - sc0_conceding$TGC
sc0_PTS <- (sc0_total_wins*3) + (sc0_total_draws*1)
sc0_league_table <- cbind(sc0_league_table,sc0_GS,sc0_GC,sc0_GD,sc0_PTS)
sc0_league_table <- as.data.frame(sc0_league_table)
#rename the columns
names(sc0_league_table)[names(sc0_league_table) == "sc0_teams"] <- "Team"
names(sc0_league_table)[names(sc0_league_table) == "sc0_games_played"] <- "P"
names(sc0_league_table)[names(sc0_league_table) == "sc0_total_wins"] <- "W"
names(sc0_league_table)[names(sc0_league_table) == "sc0_total_draws"] <- "D"
names(sc0_league_table)[names(sc0_league_table) == "sc0_total_loss"] <- "L"
names(sc0_league_table)[names(sc0_league_table) == "sc0_GS"] <- "F"
names(sc0_league_table)[names(sc0_league_table) == "sc0_GC"] <- "A"
points_sc0 <- sc0_league_table[order(sc0_league_table$sc0_PTS, decreasing = TRUE),]
row.names(points_sc0) <- 1:length(sc0_teams)

#SC1
sc1_teams
sc1_games_played
#hwins and away wins
sc1_home_wins <- c()
sc1_away_wins <- c()
sc1_home_draws <- c()
sc1_away_draws <- c()
sc1_home_loss <- c()
sc1_away_loss <- c()



for (i_sc1_wins in 1:length(sc1_teams))
{

  sc1_home_wins[i_sc1_wins] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "H",])
  sc1_away_wins[i_sc1_wins] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "A",])
  sc1_home_draws[i_sc1_wins] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "D",])
  sc1_away_draws[i_sc1_wins] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "D",])
  sc1_home_loss[i_sc1_wins] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "A",])
  sc1_away_loss[i_sc1_wins] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "H",])

}

sc1_total_wins <- sc1_home_wins + sc1_away_wins
sc1_total_draws <- sc1_home_draws + sc1_away_draws
sc1_total_loss <- sc1_home_loss + sc1_away_loss

sc1_league_table <- cbind(sc1_teams,sc1_games_played,sc1_total_wins,sc1_total_draws,sc1_total_loss)
sc1_GS <- sc1_scoring$TGS
sc1_GC <-sc1_conceding$TGC
sc1_GD <- sc1_scoring$TGS - sc1_conceding$TGC
sc1_PTS <- (sc1_total_wins*3) + (sc1_total_draws*1)
sc1_league_table <- cbind(sc1_league_table,sc1_GS,sc1_GC,sc1_GD,sc1_PTS)
sc1_league_table <- as.data.frame(sc1_league_table)
#rename the columns
names(sc1_league_table)[names(sc1_league_table) == "sc1_teams"] <- "Team"
names(sc1_league_table)[names(sc1_league_table) == "sc1_games_played"] <- "P"
names(sc1_league_table)[names(sc1_league_table) == "sc1_total_wins"] <- "W"
names(sc1_league_table)[names(sc1_league_table) == "sc1_total_draws"] <- "D"
names(sc1_league_table)[names(sc1_league_table) == "sc1_total_loss"] <- "L"
names(sc1_league_table)[names(sc1_league_table) == "sc1_GS"] <- "F"
names(sc1_league_table)[names(sc1_league_table) == "sc1_GC"] <- "A"
points_sc1 <- sc1_league_table[order(sc1_league_table$sc1_PTS, decreasing = TRUE),]
row.names(points_sc1) <- 1:length(sc1_teams)

#SC2
sc2_teams
sc2_games_played
#hwins and away wins
sc2_home_wins <- c()
sc2_away_wins <- c()
sc2_home_draws <- c()
sc2_away_draws <- c()
sc2_home_loss <- c()
sc2_away_loss <- c()



for (i_sc2_wins in 1:length(sc2_teams))
{

  sc2_home_wins[i_sc2_wins] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "H",])
  sc2_away_wins[i_sc2_wins] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "A",])
  sc2_home_draws[i_sc2_wins] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "D",])
  sc2_away_draws[i_sc2_wins] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "D",])
  sc2_home_loss[i_sc2_wins] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "A",])
  sc2_away_loss[i_sc2_wins] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "H",])

}

sc2_total_wins <- sc2_home_wins + sc2_away_wins
sc2_total_draws <- sc2_home_draws + sc2_away_draws
sc2_total_loss <- sc2_home_loss + sc2_away_loss

sc2_league_table <- cbind(sc2_teams,sc2_games_played,sc2_total_wins,sc2_total_draws,sc2_total_loss)
sc2_GS <- sc2_scoring$TGS
sc2_GC <-sc2_conceding$TGC
sc2_GD <- sc2_scoring$TGS - sc2_conceding$TGC
sc2_PTS <- (sc2_total_wins*3) + (sc2_total_draws*1)
sc2_league_table <- cbind(sc2_league_table,sc2_GS,sc2_GC,sc2_GD,sc2_PTS)
sc2_league_table <- as.data.frame(sc2_league_table)
#rename the columns
names(sc2_league_table)[names(sc2_league_table) == "sc2_teams"] <- "Team"
names(sc2_league_table)[names(sc2_league_table) == "sc2_games_played"] <- "P"
names(sc2_league_table)[names(sc2_league_table) == "sc2_total_wins"] <- "W"
names(sc2_league_table)[names(sc2_league_table) == "sc2_total_draws"] <- "D"
names(sc2_league_table)[names(sc2_league_table) == "sc2_total_loss"] <- "L"
names(sc2_league_table)[names(sc2_league_table) == "sc2_GS"] <- "F"
names(sc2_league_table)[names(sc2_league_table) == "sc2_GC"] <- "A"
points_sc2 <- sc2_league_table[order(sc2_league_table$sc2_PTS, decreasing = TRUE),]
row.names(points_sc2) <- 1:length(sc2_teams)

#SC3
sc3_teams
sc3_games_played
#hwins and away wins
sc3_home_wins <- c()
sc3_away_wins <- c()
sc3_home_draws <- c()
sc3_away_draws <- c()
sc3_home_loss <- c()
sc3_away_loss <- c()



for (i_sc3_wins in 1:length(sc3_teams))
{

  sc3_home_wins[i_sc3_wins] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "H",])
  sc3_away_wins[i_sc3_wins] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "A",])
  sc3_home_draws[i_sc3_wins] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "D",])
  sc3_away_draws[i_sc3_wins] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "D",])
  sc3_home_loss[i_sc3_wins] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "A",])
  sc3_away_loss[i_sc3_wins] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "H",])

}

sc3_total_wins <- sc3_home_wins + sc3_away_wins
sc3_total_draws <- sc3_home_draws + sc3_away_draws
sc3_total_loss <- sc3_home_loss + sc3_away_loss

sc3_league_table <- cbind(sc3_teams,sc3_games_played,sc3_total_wins,sc3_total_draws,sc3_total_loss)
sc3_GS <- sc3_scoring$TGS
sc3_GC <-sc3_conceding$TGC
sc3_GD <- sc3_scoring$TGS - sc3_conceding$TGC
sc3_PTS <- (sc3_total_wins*3) + (sc3_total_draws*1)
sc3_league_table <- cbind(sc3_league_table,sc3_GS,sc3_GC,sc3_GD,sc3_PTS)
sc3_league_table <- as.data.frame(sc3_league_table)
#rename the columns
names(sc3_league_table)[names(sc3_league_table) == "sc3_teams"] <- "Team"
names(sc3_league_table)[names(sc3_league_table) == "sc3_games_played"] <- "P"
names(sc3_league_table)[names(sc3_league_table) == "sc3_total_wins"] <- "W"
names(sc3_league_table)[names(sc3_league_table) == "sc3_total_draws"] <- "D"
names(sc3_league_table)[names(sc3_league_table) == "sc3_total_loss"] <- "L"
names(sc3_league_table)[names(sc3_league_table) == "sc3_GS"] <- "F"
names(sc3_league_table)[names(sc3_league_table) == "sc3_GC"] <- "A"
points_sc3 <- sc3_league_table[order(sc3_league_table$sc3_PTS, decreasing = TRUE),]
row.names(points_sc3) <- 1:length(sc3_teams)

#SP1
sp1_teams
sp1_games_played
#hwins and away wins
sp1_home_wins <- c()
sp1_away_wins <- c()
sp1_home_draws <- c()
sp1_away_draws <- c()
sp1_home_loss <- c()
sp1_away_loss <- c()



for (i_sp1_wins in 1:length(sp1_teams))
{

  sp1_home_wins[i_sp1_wins] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "H",])
  sp1_away_wins[i_sp1_wins] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "A",])
  sp1_home_draws[i_sp1_wins] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "D",])
  sp1_away_draws[i_sp1_wins] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "D",])
  sp1_home_loss[i_sp1_wins] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "A",])
  sp1_away_loss[i_sp1_wins] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "H",])

}

sp1_total_wins <- sp1_home_wins + sp1_away_wins
sp1_total_draws <- sp1_home_draws + sp1_away_draws
sp1_total_loss <- sp1_home_loss + sp1_away_loss

sp1_league_table <- cbind(sp1_teams,sp1_games_played,sp1_total_wins,sp1_total_draws,sp1_total_loss)
sp1_GS <- sp1_scoring$TGS
sp1_GC <-sp1_conceding$TGC
sp1_GD <- sp1_scoring$TGS - sp1_conceding$TGC
sp1_PTS <- (sp1_total_wins*3) + (sp1_total_draws*1)
sp1_league_table <- cbind(sp1_league_table,sp1_GS,sp1_GC,sp1_GD,sp1_PTS)
sp1_league_table <- as.data.frame(sp1_league_table)
#rename the columns
names(sp1_league_table)[names(sp1_league_table) == "sp1_teams"] <- "Team"
names(sp1_league_table)[names(sp1_league_table) == "sp1_games_played"] <- "P"
names(sp1_league_table)[names(sp1_league_table) == "sp1_total_wins"] <- "W"
names(sp1_league_table)[names(sp1_league_table) == "sp1_total_draws"] <- "D"
names(sp1_league_table)[names(sp1_league_table) == "sp1_total_loss"] <- "L"
names(sp1_league_table)[names(sp1_league_table) == "sp1_GS"] <- "F"
names(sp1_league_table)[names(sp1_league_table) == "sp1_GC"] <- "A"
points_sp1 <- sp1_league_table[order(sp1_league_table$sp1_PTS, decreasing = TRUE),]
row.names(points_sp1) <- 1:length(sp1_teams)

#SP2
sp2_teams
sp2_games_played
#hwins and away wins
sp2_home_wins <- c()
sp2_away_wins <- c()
sp2_home_draws <- c()
sp2_away_draws <- c()
sp2_home_loss <- c()
sp2_away_loss <- c()



for (i_sp2_wins in 1:length(sp2_teams))
{

  sp2_home_wins[i_sp2_wins] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "H",])
  sp2_away_wins[i_sp2_wins] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "A",])
  sp2_home_draws[i_sp2_wins] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "D",])
  sp2_away_draws[i_sp2_wins] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "D",])
  sp2_home_loss[i_sp2_wins] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "A",])
  sp2_away_loss[i_sp2_wins] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "H",])

}

sp2_total_wins <- sp2_home_wins + sp2_away_wins
sp2_total_draws <- sp2_home_draws + sp2_away_draws
sp2_total_loss <- sp2_home_loss + sp2_away_loss

sp2_league_table <- cbind(sp2_teams,sp2_games_played,sp2_total_wins,sp2_total_draws,sp2_total_loss)
sp2_GS <- sp2_scoring$TGS
sp2_GC <-sp2_conceding$TGC
sp2_GD <- sp2_scoring$TGS - sp2_conceding$TGC
sp2_PTS <- (sp2_total_wins*3) + (sp2_total_draws*1)
sp2_league_table <- cbind(sp2_league_table,sp2_GS,sp2_GC,sp2_GD,sp2_PTS)
sp2_league_table <- as.data.frame(sp2_league_table)
#rename the columns
names(sp2_league_table)[names(sp2_league_table) == "sp2_teams"] <- "Team"
names(sp2_league_table)[names(sp2_league_table) == "sp2_games_played"] <- "P"
names(sp2_league_table)[names(sp2_league_table) == "sp2_total_wins"] <- "W"
names(sp2_league_table)[names(sp2_league_table) == "sp2_total_draws"] <- "D"
names(sp2_league_table)[names(sp2_league_table) == "sp2_total_loss"] <- "L"
names(sp2_league_table)[names(sp2_league_table) == "sp2_GS"] <- "F"
names(sp2_league_table)[names(sp2_league_table) == "sp2_GC"] <- "A"
points_sp2 <- sp2_league_table[order(sp2_league_table$sp2_PTS, decreasing = TRUE),]
row.names(points_sp2) <- 1:length(sp2_teams)

#T1
t1_teams
t1_games_played
#hwins and away wins
t1_home_wins <- c()
t1_away_wins <- c()
t1_home_draws <- c()
t1_away_draws <- c()
t1_home_loss <- c()
t1_away_loss <- c()



for (i_t1_wins in 1:length(t1_teams))
{

  t1_home_wins[i_t1_wins] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_wins] & T1$FTR == "H",])
  t1_away_wins[i_t1_wins] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_wins] & T1$FTR == "A",])
  t1_home_draws[i_t1_wins] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_wins] & T1$FTR == "D",])
  t1_away_draws[i_t1_wins] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_wins] & T1$FTR == "D",])
  t1_home_loss[i_t1_wins] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_wins] & T1$FTR == "A",])
  t1_away_loss[i_t1_wins] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_wins] & T1$FTR == "H",])

}

t1_total_wins <- t1_home_wins + t1_away_wins
t1_total_draws <- t1_home_draws + t1_away_draws
t1_total_loss <- t1_home_loss + t1_away_loss

t1_league_table <- cbind(t1_teams,t1_games_played,t1_total_wins,t1_total_draws,t1_total_loss)
t1_GS <- t1_scoring$TGS
t1_GC <-t1_conceding$TGC
t1_GD <- t1_scoring$TGS - t1_conceding$TGC
t1_PTS <- (t1_total_wins*3) + (t1_total_draws*1)
t1_league_table <- cbind(t1_league_table,t1_GS,t1_GC,t1_GD,t1_PTS)
t1_league_table <- as.data.frame(t1_league_table)
#rename the columns
names(t1_league_table)[names(t1_league_table) == "t1_teams"] <- "Team"
names(t1_league_table)[names(t1_league_table) == "t1_games_played"] <- "P"
names(t1_league_table)[names(t1_league_table) == "t1_total_wins"] <- "W"
names(t1_league_table)[names(t1_league_table) == "t1_total_draws"] <- "D"
names(t1_league_table)[names(t1_league_table) == "t1_total_loss"] <- "L"
names(t1_league_table)[names(t1_league_table) == "t1_GS"] <- "F"
names(t1_league_table)[names(t1_league_table) == "t1_GC"] <- "A"
points_t1 <- t1_league_table[order(t1_league_table$t1_PTS, decreasing = TRUE),]
row.names(points_t1) <- 1:length(t1_teams)

#write out the data to excel
write.xlsx(points_b1,'LeagueTable.xlsx',sheetName = "B1")
write.xlsx(points_d1,'LeagueTable.xlsx',sheetName = "D1", append = TRUE)
write.xlsx(points_d2,'LeagueTable.xlsx',sheetName = "D2", append = TRUE)
write.xlsx(points_e0,'LeagueTable.xlsx',sheetName = "E0", append = TRUE)
write.xlsx(points_e1,'LeagueTable.xlsx',sheetName = "E1", append = TRUE)
write.xlsx(points_e2,'LeagueTable.xlsx',sheetName = "E2", append = TRUE)
write.xlsx(points_e3,'LeagueTable.xlsx',sheetName = "E3", append = TRUE)
write.xlsx(points_ec,'LeagueTable.xlsx',sheetName = "EC", append = TRUE)
write.xlsx(points_f1,'LeagueTable.xlsx',sheetName = "F1", append = TRUE)
write.xlsx(points_f2,'LeagueTable.xlsx',sheetName = "F2", append = TRUE)
write.xlsx(points_g1,'LeagueTable.xlsx',sheetName = "G1", append = TRUE)
write.xlsx(points_i1,'LeagueTable.xlsx',sheetName = "I1", append = TRUE)
write.xlsx(points_i2,'LeagueTable.xlsx',sheetName = "I2", append = TRUE)
write.xlsx(points_n1,'LeagueTable.xlsx',sheetName = "N1", append = TRUE)
write.xlsx(points_p1,'LeagueTable.xlsx',sheetName = "P1", append = TRUE)
write.xlsx(points_sc0,'LeagueTable.xlsx',sheetName = "SC0", append = TRUE)
write.xlsx(points_sc1,'LeagueTable.xlsx',sheetName = "SC1", append = TRUE)
write.xlsx(points_sc2,'LeagueTable.xlsx',sheetName = "SC2", append = TRUE)
write.xlsx(points_sc3,'LeagueTable.xlsx',sheetName = "SC3", append = TRUE)
write.xlsx(points_sp1,'LeagueTable.xlsx',sheetName = "SP1", append = TRUE)
write.xlsx(points_sp2,'LeagueTable.xlsx',sheetName = "SP2", append = TRUE)
write.xlsx(points_t1,'LeagueTable.xlsx',sheetName = "T1", append = TRUE)

