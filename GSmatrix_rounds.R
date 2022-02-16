
#b1
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goalscoredmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goalscoredround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoalscored <- B1_rounds$FTHG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoalscored <- B1_rounds$FTAG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_gs <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_gs <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalscombined <- c(b1_homegoalscored,b1_awaygoalscored)
  b1_teamscombined <- c(b1_hometeamstemp_gs,b1_awayteamstemp_gs)

  b1_goalscoredround <- data.frame(b1_teamscombined,b1_goalscombined)

  b1_goalscoredround <- b1_goalscoredround[order(b1_goalscoredround$b1_teamscombined),]
  b1_goalscoredround$b1_teamscombined <- NULL
  b1_goalscoredmatrix[,i_b1_krounds] <- b1_goalscoredround

}

b1_goalscoredmatrix <- cbind(b1_teams,b1_goalscoredmatrix)
#############################################################################################
#d1
d1_krounds <- tail(unique(D1_rounds$d1_matchday),1)
d1_goalscoredmatrix <- data.frame(matrix(nrow = length(d1_teams),ncol = d1_krounds))
d1_goalscoredround <- c()
for(i_d1_krounds in 1:d1_krounds)
{
  d1_homegoalscored <- D1_rounds$FTHG[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awaygoalscored <- D1_rounds$FTAG[D1_rounds$d1_matchday == i_d1_krounds]

  d1_hometeamstemp_gs <- D1_rounds$HomeTeam[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awayteamstemp_gs <- D1_rounds$AwayTeam[D1_rounds$d1_matchday== i_d1_krounds]

  d1_goalscombined <- c(d1_homegoalscored,d1_awaygoalscored)
  d1_teamscombined <- c(d1_hometeamstemp_gs,d1_awayteamstemp_gs)

  d1_goalscoredround <- data.frame(d1_teamscombined,d1_goalscombined)

  d1_goalscoredround <- d1_goalscoredround[order(d1_goalscoredround$d1_teamscombined),]
  d1_goalscoredround$d1_teamscombined <- NULL
  d1_goalscoredmatrix[,i_d1_krounds] <- d1_goalscoredround

}

d1_goalscoredmatrix <- cbind(d1_teams,d1_goalscoredmatrix)
#############################################################################################
#d2
d2_krounds <- tail(unique(D2_rounds$d2_matchday),1)
d2_goalscoredmatrix <- data.frame(matrix(nrow = length(d2_teams),ncol = d2_krounds))
d2_goalscoredround <- c()
for(i_d2_krounds in 1:d2_krounds)
{
  d2_homegoalscored <- D2_rounds$FTHG[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awaygoalscored <- D2_rounds$FTAG[D2_rounds$d2_matchday == i_d2_krounds]

  d2_hometeamstemp_gs <- D2_rounds$HomeTeam[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awayteamstemp_gs <- D2_rounds$AwayTeam[D2_rounds$d2_matchday== i_d2_krounds]

  d2_goalscombined <- c(d2_homegoalscored,d2_awaygoalscored)
  d2_teamscombined <- c(d2_hometeamstemp_gs,d2_awayteamstemp_gs)

  d2_goalscoredround <- data.frame(d2_teamscombined,d2_goalscombined)

  d2_goalscoredround <- d2_goalscoredround[order(d2_goalscoredround$d2_teamscombined),]
  d2_goalscoredround$d2_teamscombined <- NULL
  d2_goalscoredmatrix[,i_d2_krounds] <- d2_goalscoredround

}

d2_goalscoredmatrix <- cbind(d2_teams,d2_goalscoredmatrix)
#############################################################################################
#e0
e0_krounds <- tail(unique(E0_rounds$e0_matchday),1)
e0_goalscoredmatrix <- data.frame(matrix(nrow = length(e0_teams),ncol = e0_krounds))
e0_goalscoredround <- c()
for(i_e0_krounds in 1:e0_krounds)
{
  e0_homegoalscored <- E0_rounds$FTHG[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awaygoalscored <- E0_rounds$FTAG[E0_rounds$e0_matchday == i_e0_krounds]

  e0_hometeamstemp_gs <- E0_rounds$HomeTeam[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awayteamstemp_gs <- E0_rounds$AwayTeam[E0_rounds$e0_matchday == i_e0_krounds]

  e0_goalscombined <- c(e0_homegoalscored,e0_awaygoalscored)
  e0_teamscombined <- c(e0_hometeamstemp_gs,e0_awayteamstemp_gs)

  e0_goalscoredround <- data.frame(e0_teamscombined,e0_goalscombined)

  e0_goalscoredround <- e0_goalscoredround[order(e0_goalscoredround$e0_teamscombined),]
  e0_goalscoredround$e0_teamscombined <- NULL
  e0_goalscoredmatrix[,i_e0_krounds] <- e0_goalscoredround

}

e0_goalscoredmatrix <- cbind(e0_teams,e0_goalscoredmatrix)
#############################################################################################
#e1
e1_krounds <- tail(unique(E1_rounds$e1_matchday),1)
e1_goalscoredmatrix <- data.frame(matrix(nrow = length(e1_teams),ncol = e1_krounds))
e1_goalscoredround <- c()
for(i_e1_krounds in 1:e1_krounds)
{
  e1_homegoalscored <- E1_rounds$FTHG[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awaygoalscored <- E1_rounds$FTAG[E1_rounds$e1_matchday == i_e1_krounds]

  e1_hometeamstemp_gs <- E1_rounds$HomeTeam[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awayteamstemp_gs <- E1_rounds$AwayTeam[E1_rounds$e1_matchday== i_e1_krounds]

  e1_goalscombined <- c(e1_homegoalscored,e1_awaygoalscored)
  e1_teamscombined <- c(e1_hometeamstemp_gs,e1_awayteamstemp_gs)

  e1_goalscoredround <- data.frame(e1_teamscombined,e1_goalscombined)

  e1_goalscoredround <- e1_goalscoredround[order(e1_goalscoredround$e1_teamscombined),]
  e1_goalscoredround$e1_teamscombined <- NULL
  e1_goalscoredmatrix[,i_e1_krounds] <- e1_goalscoredround

}

e1_goalscoredmatrix <- cbind(e1_teams,e1_goalscoredmatrix)
#############################################################################################
#e2
e2_krounds <- tail(unique(E2_rounds$e2_matchday),1)
e2_goalscoredmatrix <- data.frame(matrix(nrow = length(e2_teams),ncol = e2_krounds))
e2_goalscoredround <- c()
for(i_e2_krounds in 1:e2_krounds)
{
  e2_homegoalscored <- E2_rounds$FTHG[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awaygoalscored <- E2_rounds$FTAG[E2_rounds$e2_matchday == i_e2_krounds]

  e2_hometeamstemp_gs <- E2_rounds$HomeTeam[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awayteamstemp_gs <- E2_rounds$AwayTeam[E2_rounds$e2_matchday== i_e2_krounds]

  e2_goalscombined <- c(e2_homegoalscored,e2_awaygoalscored)
  e2_teamscombined <- c(e2_hometeamstemp_gs,e2_awayteamstemp_gs)

  e2_goalscoredround <- data.frame(e2_teamscombined,e2_goalscombined)

  e2_goalscoredround <- e2_goalscoredround[order(e2_goalscoredround$e2_teamscombined),]
  e2_goalscoredround$e2_teamscombined <- NULL
  e2_goalscoredmatrix[,i_e2_krounds] <- e2_goalscoredround

}

e2_goalscoredmatrix <- cbind(e2_teams,e2_goalscoredmatrix)
#############################################################################################
#e3
e3_krounds <- tail(unique(E3_rounds$e3_matchday),1)
e3_goalscoredmatrix <- data.frame(matrix(nrow = length(e3_teams),ncol = e3_krounds))
e3_goalscoredround <- c()
for(i_e3_krounds in 1:e3_krounds)
{
  e3_homegoalscored <- E3_rounds$FTHG[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awaygoalscored <- E3_rounds$FTAG[E3_rounds$e3_matchday == i_e3_krounds]

  e3_hometeamstemp_gs <- E3_rounds$HomeTeam[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awayteamstemp_gs <- E3_rounds$AwayTeam[E3_rounds$e3_matchday== i_e3_krounds]

  e3_goalscombined <- c(e3_homegoalscored,e3_awaygoalscored)
  e3_teamscombined <- c(e3_hometeamstemp_gs,e3_awayteamstemp_gs)

  e3_goalscoredround <- data.frame(e3_teamscombined,e3_goalscombined)

  e3_goalscoredround <- e3_goalscoredround[order(e3_goalscoredround$e3_teamscombined),]
  e3_goalscoredround$e3_teamscombined <- NULL
  e3_goalscoredmatrix[,i_e3_krounds] <- e3_goalscoredround

}

e3_goalscoredmatrix <- cbind(e3_teams,e3_goalscoredmatrix)
#############################################################################################
#ec
ec_krounds <- tail(unique(EC_rounds$ec_matchday),1)
ec_goalscoredmatrix <- data.frame(matrix(nrow = length(ec_teams),ncol = ec_krounds))
ec_goalscoredround <- c()
for(i_ec_krounds in 1:ec_krounds)
{
  ec_homegoalscored <- EC_rounds$FTHG[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awaygoalscored <- EC_rounds$FTAG[EC_rounds$ec_matchday == i_ec_krounds]

  ec_hometeamstemp_gs <- EC_rounds$HomeTeam[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awayteamstemp_gs <- EC_rounds$AwayTeam[EC_rounds$ec_matchday== i_ec_krounds]

  ec_goalscombined <- c(ec_homegoalscored,ec_awaygoalscored)
  ec_teamscombined <- c(ec_hometeamstemp_gs,ec_awayteamstemp_gs)

  ec_goalscoredround <- data.frame(ec_teamscombined,ec_goalscombined)

  ec_goalscoredround <- ec_goalscoredround[order(ec_goalscoredround$ec_teamscombined),]
  ec_goalscoredround$ec_teamscombined <- NULL
  ec_goalscoredmatrix[,i_ec_krounds] <- ec_goalscoredround

}

ec_goalscoredmatrix <- cbind(ec_teams,ec_goalscoredmatrix)
#############################################################################################
#f1
f1_krounds <- tail(unique(F1_rounds$f1_matchday),1)
f1_goalscoredmatrix <- data.frame(matrix(nrow = length(f1_teams),ncol = f1_krounds))
f1_goalscoredround <- c()
for(i_f1_krounds in 1:f1_krounds)
{
  f1_homegoalscored <- F1_rounds$FTHG[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awaygoalscored <- F1_rounds$FTAG[F1_rounds$f1_matchday == i_f1_krounds]

  f1_hometeamstemp_gs <- F1_rounds$HomeTeam[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awayteamstemp_gs <- F1_rounds$AwayTeam[F1_rounds$f1_matchday== i_f1_krounds]

  f1_goalscombined <- c(f1_homegoalscored,f1_awaygoalscored)
  f1_teamscombined <- c(f1_hometeamstemp_gs,f1_awayteamstemp_gs)

  f1_goalscoredround <- data.frame(f1_teamscombined,f1_goalscombined)

  f1_goalscoredround <- f1_goalscoredround[order(f1_goalscoredround$f1_teamscombined),]
  f1_goalscoredround$f1_teamscombined <- NULL
  f1_goalscoredmatrix[,i_f1_krounds] <- f1_goalscoredround

}

f1_goalscoredmatrix <- cbind(f1_teams,f1_goalscoredmatrix)
#############################################################################################
#f2
f2_krounds <- tail(unique(F2_rounds$f2_matchday),1)
f2_goalscoredmatrix <- data.frame(matrix(nrow = length(f2_teams),ncol = f2_krounds))
f2_goalscoredround <- c()
for(i_f2_krounds in 1:f2_krounds)
{
  f2_homegoalscored <- F2_rounds$FTHG[F2_rounds$f2_matchday == i_f2_krounds]

  f2_awaygoalscored <- F2_rounds$FTAG[F2_rounds$f2_matchday == i_f2_krounds]

  f2_hometeamstemp_gs <- F2_rounds$HomeTeam[F2_rounds$f2_matchday == i_f2_krounds]

  f2_awayteamstemp_gs <- F2_rounds$AwayTeam[F2_rounds$f2_matchday== i_f2_krounds]

  f2_goalscombined <- c(f2_homegoalscored,f2_awaygoalscored)
  f2_teamscombined <- c(f2_hometeamstemp_gs,f2_awayteamstemp_gs)

  f2_goalscoredround <- data.frame(f2_teamscombined,f2_goalscombined)

  f2_goalscoredround <- f2_goalscoredround[order(f2_goalscoredround$f2_teamscombined),]
  f2_goalscoredround$f2_teamscombined <- NULL
  f2_goalscoredmatrix[,i_f2_krounds] <- f2_goalscoredround

}

f2_goalscoredmatrix <- cbind(f2_teams,f2_goalscoredmatrix)
#############################################################################################
#g1
g1_krounds <- tail(unique(G1_rounds$g1_matchday),1)
g1_goalscoredmatrix <- data.frame(matrix(nrow = length(g1_teams),ncol = g1_krounds))
g1_goalscoredround <- c()
for(i_g1_krounds in 1:g1_krounds)
{
  g1_homegoalscored <- G1_rounds$FTHG[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awaygoalscored <- G1_rounds$FTAG[G1_rounds$g1_matchday == i_g1_krounds]

  g1_hometeamstemp_gs <- G1_rounds$HomeTeam[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awayteamstemp_gs <- G1_rounds$AwayTeam[G1_rounds$g1_matchday== i_g1_krounds]

  g1_goalscombined <- c(g1_homegoalscored,g1_awaygoalscored)
  g1_teamscombined <- c(g1_hometeamstemp_gs,g1_awayteamstemp_gs)

  g1_goalscoredround <- data.frame(g1_teamscombined,g1_goalscombined)

  g1_goalscoredround <- g1_goalscoredround[order(g1_goalscoredround$g1_teamscombined),]
  g1_goalscoredround$g1_teamscombined <- NULL
  g1_goalscoredmatrix[,i_g1_krounds] <- g1_goalscoredround

}

g1_goalscoredmatrix <- cbind(g1_teams,g1_goalscoredmatrix)
#############################################################################################
#i1
i1_krounds <- tail(unique(I1_rounds$i1_matchday),1)
i1_goalscoredmatrix <- data.frame(matrix(nrow = length(i1_teams),ncol = i1_krounds))
i1_goalscoredround <- c()
for(i_i1_krounds in 1:i1_krounds)
{
  i1_homegoalscored <- I1_rounds$FTHG[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awaygoalscored <- I1_rounds$FTAG[I1_rounds$i1_matchday == i_i1_krounds]

  i1_hometeamstemp_gs <- I1_rounds$HomeTeam[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awayteamstemp_gs <- I1_rounds$AwayTeam[I1_rounds$i1_matchday== i_i1_krounds]

  i1_goalscombined <- c(i1_homegoalscored,i1_awaygoalscored)
  i1_teamscombined <- c(i1_hometeamstemp_gs,i1_awayteamstemp_gs)

  i1_goalscoredround <- data.frame(i1_teamscombined,i1_goalscombined)

  i1_goalscoredround <- i1_goalscoredround[order(i1_goalscoredround$i1_teamscombined),]
  i1_goalscoredround$i1_teamscombined <- NULL
  i1_goalscoredmatrix[,i_i1_krounds] <- i1_goalscoredround

}

i1_goalscoredmatrix <- cbind(i1_teams,i1_goalscoredmatrix)
#############################################################################################
#i2
i2_krounds <- tail(unique(I2_rounds$i2_matchday),1)
i2_goalscoredmatrix <- data.frame(matrix(nrow = length(i2_teams),ncol = i2_krounds))
i2_goalscoredround <- c()
for(i_i2_krounds in 1:i2_krounds)
{
  i2_homegoalscored <- I2_rounds$FTHG[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awaygoalscored <- I2_rounds$FTAG[I2_rounds$i2_matchday == i_i2_krounds]

  i2_hometeamstemp_gs <- I2_rounds$HomeTeam[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awayteamstemp_gs <- I2_rounds$AwayTeam[I2_rounds$i2_matchday== i_i2_krounds]

  i2_goalscombined <- c(i2_homegoalscored,i2_awaygoalscored)
  i2_teamscombined <- c(i2_hometeamstemp_gs,i2_awayteamstemp_gs)

  i2_goalscoredround <- data.frame(i2_teamscombined,i2_goalscombined)

  i2_goalscoredround <- i2_goalscoredround[order(i2_goalscoredround$i2_teamscombined),]
  i2_goalscoredround$i2_teamscombined <- NULL
  i2_goalscoredmatrix[,i_i2_krounds] <- i2_goalscoredround

}

i2_goalscoredmatrix <- cbind(i2_teams,i2_goalscoredmatrix)
#############################################################################################
#n1
n1_krounds <- tail(unique(N1_rounds$n1_matchday),1)
n1_goalscoredmatrix <- data.frame(matrix(nrow = length(n1_teams),ncol = n1_krounds))
n1_goalscoredround <- c()
for(i_n1_krounds in 1:n1_krounds)
{
  n1_homegoalscored <- N1_rounds$FTHG[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awaygoalscored <- N1_rounds$FTAG[N1_rounds$n1_matchday == i_n1_krounds]

  n1_hometeamstemp_gs <- N1_rounds$HomeTeam[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awayteamstemp_gs <- N1_rounds$AwayTeam[N1_rounds$n1_matchday== i_n1_krounds]

  n1_goalscombined <- c(n1_homegoalscored,n1_awaygoalscored)
  n1_teamscombined <- c(n1_hometeamstemp_gs,n1_awayteamstemp_gs)

  n1_goalscoredround <- data.frame(n1_teamscombined,n1_goalscombined)

  n1_goalscoredround <- n1_goalscoredround[order(n1_goalscoredround$n1_teamscombined),]
  n1_goalscoredround$n1_teamscombined <- NULL
  n1_goalscoredmatrix[,i_n1_krounds] <- n1_goalscoredround

}

n1_goalscoredmatrix <- cbind(n1_teams,n1_goalscoredmatrix)
#############################################################################################
#p1
p1_krounds <- tail(unique(P1_rounds$p1_matchday),1)
p1_goalscoredmatrix <- data.frame(matrix(nrow = length(p1_teams),ncol = p1_krounds))
p1_goalscoredround <- c()
for(i_p1_krounds in 1:p1_krounds)
{
  p1_homegoalscored <- P1_rounds$FTHG[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awaygoalscored <- P1_rounds$FTAG[P1_rounds$p1_matchday == i_p1_krounds]

  p1_hometeamstemp_gs <- P1_rounds$HomeTeam[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awayteamstemp_gs <- P1_rounds$AwayTeam[P1_rounds$p1_matchday== i_p1_krounds]

  p1_goalscombined <- c(p1_homegoalscored,p1_awaygoalscored)
  p1_teamscombined <- c(p1_hometeamstemp_gs,p1_awayteamstemp_gs)

  p1_goalscoredround <- data.frame(p1_teamscombined,p1_goalscombined)

  p1_goalscoredround <- p1_goalscoredround[order(p1_goalscoredround$p1_teamscombined),]
  p1_goalscoredround$p1_teamscombined <- NULL
  p1_goalscoredmatrix[,i_p1_krounds] <- p1_goalscoredround

}

p1_goalscoredmatrix <- cbind(p1_teams,p1_goalscoredmatrix)
#############################################################################################
#sp1
sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1)
sp1_goalscoredmatrix <- data.frame(matrix(nrow = length(sp1_teams),ncol = sp1_krounds))
sp1_goalscoredround <- c()
for(i_sp1_krounds in 1:sp1_krounds)
{
  sp1_homegoalscored <- SP1_rounds$FTHG[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awaygoalscored <- SP1_rounds$FTAG[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_hometeamstemp_gs <- SP1_rounds$HomeTeam[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awayteamstemp_gs <- SP1_rounds$AwayTeam[SP1_rounds$sp1_matchday== i_sp1_krounds]

  sp1_goalscombined <- c(sp1_homegoalscored,sp1_awaygoalscored)
  sp1_teamscombined <- c(sp1_hometeamstemp_gs,sp1_awayteamstemp_gs)

  sp1_goalscoredround <- data.frame(sp1_teamscombined,sp1_goalscombined)

  sp1_goalscoredround <- sp1_goalscoredround[order(sp1_goalscoredround$sp1_teamscombined),]
  sp1_goalscoredround$sp1_teamscombined <- NULL
  sp1_goalscoredmatrix[,i_sp1_krounds] <- sp1_goalscoredround

}

sp1_goalscoredmatrix <- cbind(sp1_teams,sp1_goalscoredmatrix)
#############################################################################################
#sp2
sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
sp2_goalscoredmatrix <- data.frame(matrix(nrow = length(sp2_teams),ncol = sp2_krounds))
sp2_goalscoredround <- c()
for(i_sp2_krounds in 1:sp2_krounds)
{
  sp2_homegoalscored <- SP2_rounds$FTHG[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awaygoalscored <- SP2_rounds$FTAG[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_hometeamstemp_gs <- SP2_rounds$HomeTeam[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awayteamstemp_gs <- SP2_rounds$AwayTeam[SP2_rounds$sp2_matchday== i_sp2_krounds]

  sp2_goalscombined <- c(sp2_homegoalscored,sp2_awaygoalscored)
  sp2_teamscombined <- c(sp2_hometeamstemp_gs,sp2_awayteamstemp_gs)

  sp2_goalscoredround <- data.frame(sp2_teamscombined,sp2_goalscombined)

  sp2_goalscoredround <- sp2_goalscoredround[order(sp2_goalscoredround$sp2_teamscombined),]
  sp2_goalscoredround$sp2_teamscombined <- NULL
  sp2_goalscoredmatrix[,i_sp2_krounds] <- sp2_goalscoredround

}

sp2_goalscoredmatrix <- cbind(sp2_teams,sp2_goalscoredmatrix)
#############################################################################################
#sc0
sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
sc0_goalscoredmatrix <- data.frame(matrix(nrow = length(sc0_teams),ncol = sc0_krounds))
sc0_goalscoredround <- c()
for(i_sc0_krounds in 1:sc0_krounds)
{
  sc0_homegoalscored <- SC0_rounds$FTHG[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awaygoalscored <- SC0_rounds$FTAG[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_hometeamstemp_gs <- SC0_rounds$HomeTeam[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awayteamstemp_gs <- SC0_rounds$AwayTeam[SC0_rounds$sc0_matchday== i_sc0_krounds]

  sc0_goalscombined <- c(sc0_homegoalscored,sc0_awaygoalscored)
  sc0_teamscombined <- c(sc0_hometeamstemp_gs,sc0_awayteamstemp_gs)

  sc0_goalscoredround <- data.frame(sc0_teamscombined,sc0_goalscombined)

  sc0_goalscoredround <- sc0_goalscoredround[order(sc0_goalscoredround$sc0_teamscombined),]
  sc0_goalscoredround$sc0_teamscombined <- NULL
  sc0_goalscoredmatrix[,i_sc0_krounds] <- sc0_goalscoredround

}

sc0_goalscoredmatrix <- cbind(sc0_teams,sc0_goalscoredmatrix)
#############################################################################################
#sc1
sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
sc1_goalscoredmatrix <- data.frame(matrix(nrow = length(sc1_teams),ncol = sc1_krounds))
sc1_goalscoredround <- c()
for(i_sc1_krounds in 1:sc1_krounds)
{
  sc1_homegoalscored <- SC1_rounds$FTHG[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awaygoalscored <- SC1_rounds$FTAG[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_hometeamstemp_gs <- SC1_rounds$HomeTeam[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awayteamstemp_gs <- SC1_rounds$AwayTeam[SC1_rounds$sc1_matchday== i_sc1_krounds]

  sc1_goalscombined <- c(sc1_homegoalscored,sc1_awaygoalscored)
  sc1_teamscombined <- c(sc1_hometeamstemp_gs,sc1_awayteamstemp_gs)

  sc1_goalscoredround <- data.frame(sc1_teamscombined,sc1_goalscombined)

  sc1_goalscoredround <- sc1_goalscoredround[order(sc1_goalscoredround$sc1_teamscombined),]
  sc1_goalscoredround$sc1_teamscombined <- NULL
  sc1_goalscoredmatrix[,i_sc1_krounds] <- sc1_goalscoredround

}

sc1_goalscoredmatrix <- cbind(sc1_teams,sc1_goalscoredmatrix)
#############################################################################################
#sc2
sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
sc2_goalscoredmatrix <- data.frame(matrix(nrow = length(sc2_teams),ncol = sc2_krounds))
sc2_goalscoredround <- c()
for(i_sc2_krounds in 1:sc2_krounds)
{
  sc2_homegoalscored <- SC2_rounds$FTHG[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awaygoalscored <- SC2_rounds$FTAG[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_hometeamstemp_gs <- SC2_rounds$HomeTeam[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awayteamstemp_gs <- SC2_rounds$AwayTeam[SC2_rounds$sc2_matchday== i_sc2_krounds]

  sc2_goalscombined <- c(sc2_homegoalscored,sc2_awaygoalscored)
  sc2_teamscombined <- c(sc2_hometeamstemp_gs,sc2_awayteamstemp_gs)

  sc2_goalscoredround <- data.frame(sc2_teamscombined,sc2_goalscombined)

  sc2_goalscoredround <- sc2_goalscoredround[order(sc2_goalscoredround$sc2_teamscombined),]
  sc2_goalscoredround$sc2_teamscombined <- NULL
  sc2_goalscoredmatrix[,i_sc2_krounds] <- sc2_goalscoredround

}

sc2_goalscoredmatrix <- cbind(sc2_teams,sc2_goalscoredmatrix)
#############################################################################################
#sc3
sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
sc3_goalscoredmatrix <- data.frame(matrix(nrow = length(sc3_teams),ncol = sc3_krounds))
sc3_goalscoredround <- c()
for(i_sc3_krounds in 1:sc3_krounds)
{
  sc3_homegoalscored <- SC3_rounds$FTHG[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awaygoalscored <- SC3_rounds$FTAG[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_hometeamstemp_gs <- SC3_rounds$HomeTeam[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awayteamstemp_gs <- SC3_rounds$AwayTeam[SC3_rounds$sc3_matchday== i_sc3_krounds]

  sc3_goalscombined <- c(sc3_homegoalscored,sc3_awaygoalscored)
  sc3_teamscombined <- c(sc3_hometeamstemp_gs,sc3_awayteamstemp_gs)

  sc3_goalscoredround <- data.frame(sc3_teamscombined,sc3_goalscombined)

  sc3_goalscoredround <- sc3_goalscoredround[order(sc3_goalscoredround$sc3_teamscombined),]
  sc3_goalscoredround$sc3_teamscombined <- NULL
  sc3_goalscoredmatrix[,i_sc3_krounds] <- sc3_goalscoredround

}

sc3_goalscoredmatrix <- cbind(sc3_teams,sc3_goalscoredmatrix)
#############################################################################################
#t1
t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
t1_goalscoredmatrix <- data.frame(matrix(nrow = length(t1_teams),ncol = t1_krounds))
t1_goalscoredround <- c()
for(i_t1_krounds in 1:t1_krounds)
{
  t1_homegoalscored <- T1_rounds$FTHG[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awaygoalscored <- T1_rounds$FTAG[T1_rounds$t1_matchday == i_t1_krounds]

  t1_hometeamstemp_gs <- T1_rounds$HomeTeam[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awayteamstemp_gs <- T1_rounds$AwayTeam[T1_rounds$t1_matchday== i_t1_krounds]

  t1_goalscombined <- c(t1_homegoalscored,t1_awaygoalscored)
  t1_teamscombined <- c(t1_hometeamstemp_gs,t1_awayteamstemp_gs)

  t1_goalscoredround <- data.frame(t1_teamscombined,t1_goalscombined)

  t1_goalscoredround <- t1_goalscoredround[order(t1_goalscoredround$t1_teamscombined),]
  t1_goalscoredround$t1_teamscombined <- NULL
  t1_goalscoredmatrix[,i_t1_krounds] <- t1_goalscoredround

}

t1_goalscoredmatrix <- cbind(t1_teams,t1_goalscoredmatrix)
#############################################################################################
























