# Goal totals V2
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
#Create the home and away team matrix
b1_goaltotalsv2 <- tapply(B1$TG, B1[c("HomeTeam", "AwayTeam")],mean)
d1_goaltotalsv2 <- tapply(D1$TG, D1[c("HomeTeam", "AwayTeam")],mean)
d2_goaltotalsv2 <- tapply(D2$TG, D2[c("HomeTeam", "AwayTeam")],mean)
e0_goaltotalsv2 <- tapply(E0$TG, E0[c("HomeTeam", "AwayTeam")],mean)
e1_goaltotalsv2 <- tapply(E1$TG, E1[c("HomeTeam", "AwayTeam")],mean)
e2_goaltotalsv2 <- tapply(E2$TG, E2[c("HomeTeam", "AwayTeam")],mean)
e3_goaltotalsv2 <- tapply(E3$TG, E3[c("HomeTeam", "AwayTeam")],mean)
ec_goaltotalsv2 <- tapply(EC$TG, EC[c("HomeTeam", "AwayTeam")],mean)
f1_goaltotalsv2 <- tapply(F1$TG, F1[c("HomeTeam", "AwayTeam")],mean)
f2_goaltotalsv2 <- tapply(F2$TG, F2[c("HomeTeam", "AwayTeam")],mean)
g1_goaltotalsv2 <- tapply(G1$TG, G1[c("HomeTeam", "AwayTeam")],mean)
i1_goaltotalsv2 <- tapply(I1$TG, I1[c("HomeTeam", "AwayTeam")],mean)
i2_goaltotalsv2 <- tapply(I2$TG, I2[c("HomeTeam", "AwayTeam")],mean)
n1_goaltotalsv2 <- tapply(N1$TG, N1[c("HomeTeam", "AwayTeam")],mean)
p1_goaltotalsv2 <- tapply(P1$TG, P1[c("HomeTeam", "AwayTeam")],mean)
sc0_goaltotalsv2 <- tapply(SC0$TG, SC0[c("HomeTeam", "AwayTeam")],mean)
sc1_goaltotalsv2 <- tapply(SC1$TG, SC1[c("HomeTeam", "AwayTeam")],mean)
sc2_goaltotalsv2 <- tapply(SC2$TG, SC2[c("HomeTeam", "AwayTeam")],mean)
sc3_goaltotalsv2 <- tapply(SC3$TG, SC3[c("HomeTeam", "AwayTeam")],mean)
sp1_goaltotalsv2 <- tapply(SP1$TG, SP1[c("HomeTeam", "AwayTeam")],mean)
sp2_goaltotalsv2 <- tapply(SP2$TG, SP2[c("HomeTeam", "AwayTeam")],mean)
t1_goaltotalsv2 <- tapply(T1$TG, T1[c("HomeTeam", "AwayTeam")],mean)
#Create the row and column sums
#B1
b1_hgtotals <- rowSums(b1_goaltotalsv2, na.rm = T)
b1_agtotals <- colSums(b1_goaltotalsv2, na.rm = T)
#D1
d1_hgtotals <- rowSums(d1_goaltotalsv2, na.rm = T)
d1_agtotals <- colSums(d1_goaltotalsv2, na.rm = T)
#D2
d2_hgtotals <- rowSums(d2_goaltotalsv2, na.rm = T)
d2_agtotals <- colSums(d2_goaltotalsv2, na.rm = T)
#E0
e0_hgtotals <- rowSums(e0_goaltotalsv2, na.rm = T)
e0_agtotals <- colSums(e0_goaltotalsv2, na.rm = T)
#E1
e1_hgtotals <- rowSums(e1_goaltotalsv2, na.rm = T)
e1_agtotals <- colSums(e1_goaltotalsv2, na.rm = T)
#E2
e2_hgtotals <- rowSums(e2_goaltotalsv2, na.rm = T)
e2_agtotals <- colSums(e2_goaltotalsv2, na.rm = T)
#E3
e3_hgtotals <- rowSums(e3_goaltotalsv2, na.rm = T)
e3_agtotals <- colSums(e3_goaltotalsv2, na.rm = T)
#EC
ec_hgtotals <- rowSums(ec_goaltotalsv2, na.rm = T)
ec_agtotals <- colSums(ec_goaltotalsv2, na.rm = T)
#F1
f1_hgtotals <- rowSums(f1_goaltotalsv2, na.rm = T)
f1_agtotals <- colSums(f1_goaltotalsv2, na.rm = T)
#F2
f2_hgtotals <- rowSums(f2_goaltotalsv2, na.rm = T)
f2_agtotals <- colSums(f2_goaltotalsv2, na.rm = T)
#G1
g1_hgtotals <- rowSums(g1_goaltotalsv2, na.rm = T)
g1_agtotals <- colSums(g1_goaltotalsv2, na.rm = T)
#I1
i1_hgtotals <- rowSums(i1_goaltotalsv2, na.rm = T)
i1_agtotals <- colSums(i1_goaltotalsv2, na.rm = T)
#I2
i2_hgtotals <- rowSums(i2_goaltotalsv2, na.rm = T)
i2_agtotals <- colSums(i2_goaltotalsv2, na.rm = T)
#N1
n1_hgtotals <- rowSums(n1_goaltotalsv2, na.rm = T)
n1_agtotals <- colSums(n1_goaltotalsv2, na.rm = T)
#P1
p1_hgtotals <- rowSums(p1_goaltotalsv2, na.rm = T)
p1_agtotals <- colSums(p1_goaltotalsv2, na.rm = T)
#SC0
sc0_hgtotals <- rowSums(sc0_goaltotalsv2, na.rm = T)
sc0_agtotals <- colSums(sc0_goaltotalsv2, na.rm = T)
#SC1
sc1_hgtotals <- rowSums(sc1_goaltotalsv2, na.rm = T)
sc1_agtotals <- colSums(sc1_goaltotalsv2, na.rm = T)
#SC2
sc2_hgtotals <- rowSums(sc2_goaltotalsv2, na.rm = T)
sc2_agtotals <- colSums(sc2_goaltotalsv2, na.rm = T)
#SC3
sc3_hgtotals <- rowSums(sc3_goaltotalsv2, na.rm = T)
sc3_agtotals <- colSums(sc3_goaltotalsv2, na.rm = T)
#SP1
sp1_hgtotals <- rowSums(sp1_goaltotalsv2, na.rm = T)
sp1_agtotals <- colSums(sp1_goaltotalsv2, na.rm = T)
#SP2
sp2_hgtotals <- rowSums(sp2_goaltotalsv2, na.rm = T)
sp2_agtotals <- colSums(sp2_goaltotalsv2, na.rm = T)
#T1
t1_hgtotals <- rowSums(t1_goaltotalsv2, na.rm = T)
t1_agtotals <- colSums(t1_goaltotalsv2, na.rm = T)

#Bind hgtotal and agtotal
b1_goaltotalsv2 <- cbind(b1_goaltotalsv2,b1_hgtotals,b1_agtotals)
d1_goaltotalsv2 <- cbind(d1_goaltotalsv2,d1_hgtotals,d1_agtotals)
d2_goaltotalsv2 <- cbind(d2_goaltotalsv2,d2_hgtotals,d2_agtotals)
e0_goaltotalsv2 <- cbind(e0_goaltotalsv2,e0_hgtotals,e0_agtotals)
e1_goaltotalsv2 <- cbind(e1_goaltotalsv2,e1_hgtotals,e1_agtotals)
e2_goaltotalsv2 <- cbind(e2_goaltotalsv2,e2_hgtotals,e2_agtotals)
e3_goaltotalsv2 <- cbind(e3_goaltotalsv2,e3_hgtotals,e3_agtotals)
ec_goaltotalsv2 <- cbind(ec_goaltotalsv2,ec_hgtotals,ec_agtotals)
f1_goaltotalsv2 <- cbind(f1_goaltotalsv2,f1_hgtotals,f1_agtotals)
f2_goaltotalsv2 <- cbind(f2_goaltotalsv2,f2_hgtotals,f2_agtotals)
g1_goaltotalsv2 <- cbind(g1_goaltotalsv2,g1_hgtotals,g1_agtotals)
i1_goaltotalsv2 <- cbind(i1_goaltotalsv2,i1_hgtotals,i1_agtotals)
i2_goaltotalsv2 <- cbind(i2_goaltotalsv2,i2_hgtotals,i2_agtotals)
n1_goaltotalsv2 <- cbind(n1_goaltotalsv2,n1_hgtotals,n1_agtotals)
p1_goaltotalsv2 <- cbind(p1_goaltotalsv2,p1_hgtotals,p1_agtotals)
sc0_goaltotalsv2 <- cbind(sc0_goaltotalsv2,sc0_hgtotals,sc0_agtotals)
sc1_goaltotalsv2 <- cbind(sc1_goaltotalsv2,sc1_hgtotals,sc1_agtotals)
sc2_goaltotalsv2 <- cbind(sc2_goaltotalsv2,sc2_hgtotals,sc2_agtotals)
sc3_goaltotalsv2 <- cbind(sc3_goaltotalsv2,sc3_hgtotals,sc3_agtotals)
sp1_goaltotalsv2 <- cbind(sp1_goaltotalsv2,sp1_hgtotals,sp1_agtotals)
sp2_goaltotalsv2 <- cbind(sp2_goaltotalsv2,sp2_hgtotals,sp2_agtotals)
t1_goaltotalsv2 <- cbind(t1_goaltotalsv2,t1_hgtotals,t1_agtotals)

#add total goals
b1_totalgoals <- b1_hgtotals + b1_agtotals
d1_totalgoals <- d1_hgtotals + d1_agtotals
d2_totalgoals <- d2_hgtotals + d2_agtotals
e0_totalgoals <- e0_hgtotals + e0_agtotals
e1_totalgoals <- e1_hgtotals + e1_agtotals
e2_totalgoals <- e2_hgtotals + e2_agtotals
e3_totalgoals <- e3_hgtotals + e3_agtotals
ec_totalgoals <- ec_hgtotals + ec_agtotals
f1_totalgoals <- f1_hgtotals + f1_agtotals
f2_totalgoals <- f2_hgtotals + f2_agtotals
g1_totalgoals <- g1_hgtotals + g1_agtotals
i1_totalgoals <- i1_hgtotals + i1_agtotals
i2_totalgoals <- i2_hgtotals + i2_agtotals
n1_totalgoals <- n1_hgtotals + n1_agtotals
p1_totalgoals <- p1_hgtotals + p1_agtotals
sc0_totalgoals <- sc0_hgtotals + sc0_agtotals
sc1_totalgoals <- sc1_hgtotals + sc1_agtotals
sc2_totalgoals <- sc2_hgtotals + sc2_agtotals
sc3_totalgoals <- sc3_hgtotals + sc3_agtotals
sp1_totalgoals <- sp1_hgtotals + sp1_agtotals
sp2_totalgoals <- sp2_hgtotals + sp2_agtotals
t1_totalgoals <- t1_hgtotals + t1_agtotals
#bind total goals column
b1_goaltotalsv2 <- cbind(b1_goaltotalsv2,b1_totalgoals)
d1_goaltotalsv2 <- cbind(d1_goaltotalsv2,d1_totalgoals)
d2_goaltotalsv2 <- cbind(d2_goaltotalsv2,d2_totalgoals)
e0_goaltotalsv2 <- cbind(e0_goaltotalsv2,e0_totalgoals)
e1_goaltotalsv2 <- cbind(e1_goaltotalsv2,e1_totalgoals)
e2_goaltotalsv2 <- cbind(e2_goaltotalsv2,e2_totalgoals)
e3_goaltotalsv2 <- cbind(e3_goaltotalsv2,e3_totalgoals)
ec_goaltotalsv2 <- cbind(ec_goaltotalsv2,ec_totalgoals)
f1_goaltotalsv2 <- cbind(f1_goaltotalsv2,f1_totalgoals)
f2_goaltotalsv2 <- cbind(f2_goaltotalsv2,f2_totalgoals)
g1_goaltotalsv2 <- cbind(g1_goaltotalsv2,g1_totalgoals)
i1_goaltotalsv2 <- cbind(i1_goaltotalsv2,i1_totalgoals)
i2_goaltotalsv2 <- cbind(i2_goaltotalsv2,i2_totalgoals)
n1_goaltotalsv2 <- cbind(n1_goaltotalsv2,n1_totalgoals)
p1_goaltotalsv2 <- cbind(p1_goaltotalsv2,p1_totalgoals)
sc0_goaltotalsv2 <- cbind(sc0_goaltotalsv2,sc0_totalgoals)
sc1_goaltotalsv2 <- cbind(sc1_goaltotalsv2,sc1_totalgoals)
sc2_goaltotalsv2 <- cbind(sc2_goaltotalsv2,sc2_totalgoals)
sc3_goaltotalsv2 <- cbind(sc3_goaltotalsv2,sc3_totalgoals)
sp1_goaltotalsv2 <- cbind(sp1_goaltotalsv2,sp1_totalgoals)
sp2_goaltotalsv2 <- cbind(sp2_goaltotalsv2,sp2_totalgoals)
t1_goaltotalsv2 <- cbind(t1_goaltotalsv2,t1_totalgoals)
#Get teams in each division as a vector
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
#initialize home and away game vectors
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
#Add home games and away games to get total games played
b1_games_played <- b1_home_games + b1_away_games
d1_games_played <- d1_home_games + d1_away_games
d2_games_played <- d2_home_games + d2_away_games
e0_games_played <- e0_home_games + e0_away_games
e1_games_played <- e1_home_games + e1_away_games
e2_games_played <- e2_home_games + e2_away_games
e3_games_played <- e3_home_games + e3_away_games
ec_games_played <- ec_home_games + ec_away_games
f1_games_played <- f1_home_games + f1_away_games
f2_games_played <- f2_home_games + f2_away_games
g1_games_played <- g1_home_games + g1_away_games
i1_games_played <- i1_home_games + i1_away_games
i2_games_played <- i2_home_games + i2_away_games
n1_games_played <- n1_home_games + n1_away_games
p1_games_played <- p1_home_games + p1_away_games
sc0_games_played <- sc0_home_games + sc0_away_games
sc1_games_played <- sc1_home_games + sc1_away_games
sc2_games_played <- sc2_home_games + sc2_away_games
sc3_games_played <- sc3_home_games + sc3_away_games
sp1_games_played <- sp1_home_games + sp1_away_games
sp2_games_played <- sp2_home_games + sp2_away_games
t1_games_played <- t1_home_games + t1_away_games
#Bind total games played

b1_goaltotalsv2 <- cbind(b1_goaltotalsv2,b1_games_played)
d1_goaltotalsv2 <- cbind(d1_goaltotalsv2,d1_games_played)
d2_goaltotalsv2 <- cbind(d2_goaltotalsv2,d2_games_played)
e0_goaltotalsv2 <- cbind(e0_goaltotalsv2,e0_games_played)
e1_goaltotalsv2 <- cbind(e1_goaltotalsv2,e1_games_played)
e2_goaltotalsv2 <- cbind(e2_goaltotalsv2,e2_games_played)
e3_goaltotalsv2 <- cbind(e3_goaltotalsv2,e3_games_played)
ec_goaltotalsv2 <- cbind(ec_goaltotalsv2,ec_games_played)
f1_goaltotalsv2 <- cbind(f1_goaltotalsv2,f1_games_played)
f2_goaltotalsv2 <- cbind(f2_goaltotalsv2,f2_games_played)
g1_goaltotalsv2 <- cbind(g1_goaltotalsv2,g1_games_played)
i1_goaltotalsv2 <- cbind(i1_goaltotalsv2,i1_games_played)
i2_goaltotalsv2 <- cbind(i2_goaltotalsv2,i2_games_played)
n1_goaltotalsv2 <- cbind(n1_goaltotalsv2,n1_games_played)
p1_goaltotalsv2 <- cbind(p1_goaltotalsv2,p1_games_played)
sc0_goaltotalsv2 <- cbind(sc0_goaltotalsv2,sc0_games_played)
sc1_goaltotalsv2 <- cbind(sc1_goaltotalsv2,sc1_games_played)
sc2_goaltotalsv2 <- cbind(sc2_goaltotalsv2,sc2_games_played)
sc3_goaltotalsv2 <- cbind(sc3_goaltotalsv2,sc3_games_played)
sp1_goaltotalsv2 <- cbind(sp1_goaltotalsv2,sp1_games_played)
sp2_goaltotalsv2 <- cbind(sp2_goaltotalsv2,sp2_games_played)
t1_goaltotalsv2 <- cbind(t1_goaltotalsv2,t1_games_played)
#Calculate average total goals
b1_avg_totalgoals <- round((b1_totalgoals/ b1_games_played), digits = 4)
d1_avg_totalgoals <- round((d1_totalgoals/ d1_games_played), digits = 4)
d2_avg_totalgoals <- round((d2_totalgoals/ d2_games_played), digits = 4)
e0_avg_totalgoals <- round((e0_totalgoals/ e0_games_played), digits = 4)
e1_avg_totalgoals <- round((e1_totalgoals/ e1_games_played), digits = 4)
e2_avg_totalgoals <- round((e2_totalgoals/ e2_games_played), digits = 4)
e3_avg_totalgoals <- round((e3_totalgoals/ e3_games_played), digits = 4)
ec_avg_totalgoals <- round((ec_totalgoals/ ec_games_played), digits = 4)
f1_avg_totalgoals <- round((f1_totalgoals/ f1_games_played), digits = 4)
f2_avg_totalgoals <- round((f2_totalgoals/ f2_games_played), digits = 4)
g1_avg_totalgoals <- round((g1_totalgoals/ g1_games_played), digits = 4)
i1_avg_totalgoals <- round((i1_totalgoals/ i1_games_played), digits = 4)
i2_avg_totalgoals <- round((i2_totalgoals/ i2_games_played), digits = 4)
n1_avg_totalgoals <- round((n1_totalgoals/ n1_games_played), digits = 4)
p1_avg_totalgoals <- round((p1_totalgoals/ p1_games_played), digits = 4)
sc0_avg_totalgoals <- round((sc0_totalgoals/ sc0_games_played), digits = 4)
sc1_avg_totalgoals <- round((sc1_totalgoals/ sc1_games_played), digits = 4)
sc2_avg_totalgoals <- round((sc2_totalgoals/ sc2_games_played), digits = 4)
sc3_avg_totalgoals <- round((sc3_totalgoals/ sc3_games_played), digits = 4)
sp1_avg_totalgoals <- round((sp1_totalgoals/ sp1_games_played), digits = 4)
sp2_avg_totalgoals <- round((sp2_totalgoals/ sp2_games_played), digits = 4)
t1_avg_totalgoals <- round((t1_totalgoals/ t1_games_played), digits = 4)
#Remove NA values
b1_goaltotalsv2[is.na(b1_goaltotalsv2)] <- ""
d1_goaltotalsv2[is.na(d1_goaltotalsv2)] <- ""
d2_goaltotalsv2[is.na(d2_goaltotalsv2)] <- ""
e0_goaltotalsv2[is.na(e0_goaltotalsv2)] <- ""
e1_goaltotalsv2[is.na(e1_goaltotalsv2)] <- ""
e2_goaltotalsv2[is.na(e2_goaltotalsv2)] <- ""
e3_goaltotalsv2[is.na(e3_goaltotalsv2)] <- ""
ec_goaltotalsv2[is.na(ec_goaltotalsv2)] <- ""
f1_goaltotalsv2[is.na(f1_goaltotalsv2)] <- ""
f2_goaltotalsv2[is.na(f2_goaltotalsv2)] <- ""
g1_goaltotalsv2[is.na(g1_goaltotalsv2)] <- ""
i1_goaltotalsv2[is.na(i1_goaltotalsv2)] <- ""
i2_goaltotalsv2[is.na(i2_goaltotalsv2)] <- ""
n1_goaltotalsv2[is.na(n1_goaltotalsv2)] <- ""
p1_goaltotalsv2[is.na(p1_goaltotalsv2)] <- ""
sc0_goaltotalsv2[is.na(sc0_goaltotalsv2)] <- ""
sc1_goaltotalsv2[is.na(sc1_goaltotalsv2)] <- ""
sc2_goaltotalsv2[is.na(sc2_goaltotalsv2)] <- ""
sc3_goaltotalsv2[is.na(sc3_goaltotalsv2)] <- ""
sp1_goaltotalsv2[is.na(sp1_goaltotalsv2)] <- ""
sp2_goaltotalsv2[is.na(sp2_goaltotalsv2)] <- ""
t1_goaltotalsv2[is.na(t1_goaltotalsv2)] <- ""
#Bind average total goals

b1_goaltotalsv2 <- cbind(b1_goaltotalsv2,b1_avg_totalgoals)
d1_goaltotalsv2 <- cbind(d1_goaltotalsv2,d1_avg_totalgoals)
d2_goaltotalsv2 <- cbind(d2_goaltotalsv2,d2_avg_totalgoals)
e0_goaltotalsv2 <- cbind(e0_goaltotalsv2,e0_avg_totalgoals)
e1_goaltotalsv2 <- cbind(e1_goaltotalsv2,e1_avg_totalgoals)
e2_goaltotalsv2 <- cbind(e2_goaltotalsv2,e2_avg_totalgoals)
e3_goaltotalsv2 <- cbind(e3_goaltotalsv2,e3_avg_totalgoals)
ec_goaltotalsv2 <- cbind(ec_goaltotalsv2,ec_avg_totalgoals)
f1_goaltotalsv2 <- cbind(f1_goaltotalsv2,f1_avg_totalgoals)
f2_goaltotalsv2 <- cbind(f2_goaltotalsv2,f2_avg_totalgoals)
g1_goaltotalsv2 <- cbind(g1_goaltotalsv2,g1_avg_totalgoals)
i1_goaltotalsv2 <- cbind(i1_goaltotalsv2,i1_avg_totalgoals)
i2_goaltotalsv2 <- cbind(i2_goaltotalsv2,i2_avg_totalgoals)
n1_goaltotalsv2 <- cbind(n1_goaltotalsv2,n1_avg_totalgoals)
p1_goaltotalsv2 <- cbind(p1_goaltotalsv2,p1_avg_totalgoals)
sc0_goaltotalsv2 <- cbind(sc0_goaltotalsv2,sc0_avg_totalgoals)
sc1_goaltotalsv2 <- cbind(sc1_goaltotalsv2,sc1_avg_totalgoals)
sc2_goaltotalsv2 <- cbind(sc2_goaltotalsv2,sc2_avg_totalgoals)
sc3_goaltotalsv2 <- cbind(sc3_goaltotalsv2,sc3_avg_totalgoals)
sp1_goaltotalsv2 <- cbind(sp1_goaltotalsv2,sp1_avg_totalgoals)
sp2_goaltotalsv2 <- cbind(sp2_goaltotalsv2,sp2_avg_totalgoals)
t1_goaltotalsv2 <- cbind(t1_goaltotalsv2,t1_avg_totalgoals)

#Write out to excel files
write.xlsx(b1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "B1")
write.xlsx(d1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "D1", append = TRUE)
write.xlsx(d2_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "D2", append = TRUE)
write.xlsx(e0_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "E0", append = TRUE)
write.xlsx(e1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "E1", append = TRUE)
write.xlsx(e2_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "E2", append = TRUE)
write.xlsx(e3_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "E3", append = TRUE)
write.xlsx(ec_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "EC", append = TRUE)
write.xlsx(f1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "F1", append = TRUE)
write.xlsx(f2_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "F2", append = TRUE)
write.xlsx(g1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "G1", append = TRUE)
write.xlsx(i1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "I1", append = TRUE)
write.xlsx(i2_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "I2", append = TRUE)
write.xlsx(n1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "N1", append = TRUE)
write.xlsx(p1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "P1", append = TRUE)
write.xlsx(sc0_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "SC0", append = TRUE)
write.xlsx(sc1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "SC1", append = TRUE)
write.xlsx(sc2_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "SC2", append = TRUE)
write.xlsx(sc3_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "SC3", append = TRUE)
write.xlsx(sp1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "SP1", append = TRUE)
write.xlsx(sp2_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "SP2", append = TRUE)
write.xlsx(t1_goaltotalsv2,'GoalTotalsV2.xlsx',sheetName = "T1", append = TRUE)



