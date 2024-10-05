#by gitboyzorro5
#create divisions and order by date
library('lubridate')
allteams20242025 <- read.csv('allteams20242025SOT.csv')

#change date strings to Date objects
allteams20242025$Date <- dmy(allteams20242025$Date)
allteams20242025 <- allteams20242025[order(as.Date(allteams20242025$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20242025$TG <- allteams20242025$FTHG + allteams20242025$FTAG
allteams20242025$TC <- allteams20242025$HCO + allteams20242025$ACO
allteams20242025$TF <- allteams20242025$HF + allteams20242025$AF
allteams20242025$COSC <- paste(allteams20242025$HCO,allteams20242025$ACO,sep = "-")
allteams20242025$OV15 <- ifelse(allteams20242025$TG >= 2,"Y","N")
allteams20242025$OV25 <- ifelse(allteams20242025$TG >= 3,"Y","N")
allteams20242025$OV35 <- ifelse(allteams20242025$TG >= 4,"Y","N")
allteams20242025$TY <- allteams20242025$HY + allteams20242025$AY
allteams20242025$TR <- allteams20242025$HR + allteams20242025$AR

#create divisions subsets
B1 <- subset(allteams20242025, Div == "B1")
D1 <- subset(allteams20242025, Div == "D1")
D2 <- subset(allteams20242025, Div == "D2")
E0 <- subset(allteams20242025, Div == "E0")
E1 <- subset(allteams20242025, Div == "E1")
E2 <- subset(allteams20242025, Div == "E2")
E3 <- subset(allteams20242025, Div == "E3")
EC <- subset(allteams20242025, Div == "EC")
F1 <- subset(allteams20242025, Div == "F1")
F2 <- subset(allteams20242025, Div == "F2")
G1 <- subset(allteams20242025, Div == "G1")
I1 <- subset(allteams20242025, Div == "I1")
I2 <- subset(allteams20242025, Div == "I2")
N1 <- subset(allteams20242025, Div == "N1")
P1 <- subset(allteams20242025, Div == "P1")
SC0 <- subset(allteams20242025, Div == "SC0")
SC1 <- subset(allteams20242025, Div == "SC1")
SC2 <- subset(allteams20242025, Div == "SC2")
SC3 <- subset(allteams20242025, Div == "SC3")
SP1 <- subset(allteams20242025, Div == "SP1")
SP2 <- subset(allteams20242025, Div == "SP2")
T1 <- subset(allteams20242025, Div == "T1")
############################################################
b1_teams <- sort(unique(B1$HomeTeam))
d1_teams <- sort(unique(D1$HomeTeam))
d2_teams <- sort(unique(D2$HomeTeam))
e0_teams <- sort(unique(E0$HomeTeam))
e1_teams <- sort(unique(E1$HomeTeam))
e2_teams <- sort(unique(E2$HomeTeam))
e3_teams <- sort(unique(E3$HomeTeam))
ec_teams <- sort(unique(EC$HomeTeam))
f1_teams <- sort(unique(F1$HomeTeam))
f2_teams <- sort(unique(F2$HomeTeam))
g1_teams <- sort(unique(G1$HomeTeam))
i1_teams <- sort(unique(I1$HomeTeam))
i2_teams <- sort(unique(I2$HomeTeam))
n1_teams <- sort(unique(N1$HomeTeam))
p1_teams <- sort(unique(P1$HomeTeam))
sc0_teams <- sort(unique(SC0$HomeTeam))
sc1_teams <- sort(unique(SC1$HomeTeam))
sc2_teams <- sort(unique(SC2$HomeTeam))
sc3_teams <- sort(unique(SC3$HomeTeam))
sp1_teams <- sort(unique(SP1$HomeTeam))
sp2_teams <- sort(unique(SP2$HomeTeam))
t1_teams <- sort(unique(T1$HomeTeam))
#initialize home and awag game vectors
#B1
b1_home_games <- c()
b1_away_games <-c()
#D1
d1_home_games <- c()
d1_away_games <-c()
#D2
d2_home_games <- c()
d2_away_games <-c()
#E0
e0_home_games <- c()
e0_away_games <-c()
#E1
e1_home_games <- c()
e1_away_games <-c()
#E2
e2_home_games <- c()
e2_away_games <-c()
#E3
e3_home_games <- c()
e3_away_games <-c()
#EC
ec_home_games <- c()
ec_away_games <-c()
#F1
f1_home_games <- c()
f1_away_games <-c()
#F2
f2_home_games <- c()
f2_away_games <-c()
#G1
g1_home_games <- c()
g1_away_games <-c()
#I1
i1_home_games <- c()
i1_away_games <-c()
#I2
i2_home_games <- c()
i2_away_games <-c()
#N1
n1_home_games <- c()
n1_away_games <-c()
#P1
p1_home_games <- c()
p1_away_games <-c()
#SC0
sc0_home_games <- c()
sc0_away_games <-c()
#SC1
sc1_home_games <- c()
sc1_away_games <-c()
#SC2
sc2_home_games <- c()
sc2_away_games <-c()
#SC3
sc3_home_games <- c()
sc3_away_games <-c()
#SP1
sp1_home_games <- c()
sp1_away_games <-c()
#SP2
sp2_home_games <- c()
sp2_away_games <-c()
#T1
t1_home_games <- c()
t1_away_games <-c()
#Get number of home and away games played
#B1
for (i_b1 in 1:length(b1_teams))
{

  b1_home_games[i_b1] <- nrow(B1[B1$HomeTeam == b1_teams[i_b1],])
  b1_away_games[i_b1]  <- nrow(B1[B1$AwayTeam == b1_teams[i_b1],])

}
#D1
for (i_d1 in 1:length(d1_teams))
{

  d1_home_games[i_d1] <- nrow(D1[D1$HomeTeam == d1_teams[i_d1],])
  d1_away_games[i_d1]  <- nrow(D1[D1$AwayTeam == d1_teams[i_d1],])

}
#D2
for (i_d2 in 1:length(d2_teams))
{

  d2_home_games[i_d2] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2],])
  d2_away_games[i_d2]  <- nrow(D2[D2$AwayTeam == d2_teams[i_d2],])

}
#E0
for (i_e0 in 1:length(e0_teams))
{

  e0_home_games[i_e0] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0],])
  e0_away_games[i_e0]  <- nrow(E0[E0$AwayTeam == e0_teams[i_e0],])
}
#E1
for (i_e1 in 1:length(e1_teams))
{

  e1_home_games[i_e1] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1],])
  e1_away_games[i_e1]  <- nrow(E1[E1$AwayTeam == e1_teams[i_e1],])

}
#E2
for (i_e2 in 1:length(e2_teams))
{

  e2_home_games[i_e2] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2],])
  e2_away_games[i_e2]  <- nrow(E2[E2$AwayTeam == e2_teams[i_e2],])

}
#E3
for (i_e3 in 1:length(e3_teams))
{

  e3_home_games[i_e3] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3],])
  e3_away_games[i_e3]  <- nrow(E3[E3$AwayTeam == e3_teams[i_e3],])

}
#EC
for (i_ec in 1:length(ec_teams))
{

  ec_home_games[i_ec] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec],])
  ec_away_games[i_ec]  <- nrow(EC[EC$AwayTeam == ec_teams[i_ec],])

}
#F1
for (i_f1 in 1:length(f1_teams))
{

  f1_home_games[i_f1] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1],])
  f1_away_games[i_f1]  <- nrow(F1[F1$AwayTeam == f1_teams[i_f1],])

}
#F2
for (i_f2 in 1:length(f2_teams))
{

  f2_home_games[i_f2] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2],])
  f2_away_games[i_f2]  <- nrow(F2[F2$AwayTeam == f2_teams[i_f2],])

}
#G1
for (i_g1 in 1:length(g1_teams))
{

  g1_home_games[i_g1] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1],])
  g1_away_games[i_g1]  <- nrow(G1[G1$AwayTeam == g1_teams[i_g1],])

}
#I1
for (i_i1 in 1:length(i1_teams))
{

  i1_home_games[i_i1] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1],])
  i1_away_games[i_i1]  <- nrow(I1[I1$AwayTeam == i1_teams[i_i1],])

}
#I2
for (i_i2 in 1:length(i2_teams))
{

  i2_home_games[i_i2] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2],])
  i2_away_games[i_i2]  <- nrow(I2[I2$AwayTeam == i2_teams[i_i2],])

}
#N1
for (i_n1 in 1:length(n1_teams))
{

  n1_home_games[i_n1] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1],])
  n1_away_games[i_n1]  <- nrow(N1[N1$AwayTeam == n1_teams[i_n1],])

}
#P1
for (i_p1 in 1:length(p1_teams))
{

  p1_home_games[i_p1] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1],])
  p1_away_games[i_p1]  <- nrow(P1[P1$AwayTeam == p1_teams[i_p1],])

}
#SC0
for (i_sc0 in 1:length(sc0_teams))
{

  sc0_home_games[i_sc0] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0],])
  sc0_away_games[i_sc0]  <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0],])

}
#SC1
for (i_sc1 in 1:length(sc1_teams))
{

  sc1_home_games[i_sc1] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1],])
  sc1_away_games[i_sc1]  <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1],])

}
#SC2
for (i_sc2 in 1:length(sc2_teams))
{

  sc2_home_games[i_sc2] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2],])
  sc2_away_games[i_sc2]  <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2],])

}
#SC3
for (i_sc3 in 1:length(sc3_teams))
{

  sc3_home_games[i_sc3] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3],])
  sc3_away_games[i_sc3]  <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3],])

}
#SP1
for (i_sp1 in 1:length(sp1_teams))
{

  sp1_home_games[i_sp1] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1],])
  sp1_away_games[i_sp1]  <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1],])

}
#SP2
for (i_sp2 in 1:length(sp2_teams))
{

  sp2_home_games[i_sp2] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2],])
  sp2_away_games[i_sp2]  <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2],])

}
#T1
for (i_t1 in 1:length(t1_teams))
{

  t1_home_games[i_t1] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1],])
  t1_away_games[i_t1]  <- nrow(T1[T1$AwayTeam == t1_teams[i_t1],])

}


