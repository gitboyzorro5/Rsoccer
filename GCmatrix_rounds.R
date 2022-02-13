#b1
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goalconcededmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goalconcededround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoalconceded <- B1_rounds$FTAG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoalconceded <- B1_rounds$FTHG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_gc <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_gc <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalsconcededcombined <- c(b1_homegoalconceded,b1_awaygoalconceded)
  b1_teamscombined_gc <- c(b1_hometeamstemp_gc,b1_awayteamstemp_gc)

  b1_goalconcededround <- data.frame(b1_teamscombined_gc,b1_goalsconcededcombined)

  b1_goalconcededround <- b1_goalconcededround[order(b1_goalconcededround$b1_teamscombined_gc),]
  b1_goalconcededround$b1_teamscombined_gc <- NULL
  b1_goalconcededmatrix[,i_b1_krounds] <- b1_goalconcededround

}

b1_goalconcededmatrix <- cbind(b1_teams,b1_goalconcededmatrix)
###############################################################################################
#d1
d1_krounds <- tail(unique(D1_rounds$d1_matchday),1)
d1_goalconcededmatrix <- data.frame(matrix(nrow = length(d1_teams),ncol = d1_krounds))
d1_goalconcededround <- c()
for(i_d1_krounds in 1:d1_krounds)
{
  d1_homegoalconceded <- D1_rounds$FTAG[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awaygoalconceded <- D1_rounds$FTHG[D1_rounds$d1_matchday == i_d1_krounds]

  d1_hometeamstemp_gc <- D1_rounds$HomeTeam[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awayteamstemp_gc <- D1_rounds$AwayTeam[D1_rounds$d1_matchday== i_d1_krounds]

  d1_goalsconcededcombined <- c(d1_homegoalconceded,d1_awaygoalconceded)
  d1_teamscombined_gc <- c(d1_hometeamstemp_gc,d1_awayteamstemp_gc)

  d1_goalconcededround <- data.frame(d1_teamscombined_gc,d1_goalsconcededcombined)

  d1_goalconcededround <- d1_goalconcededround[order(d1_goalconcededround$d1_teamscombined_gc),]
  d1_goalconcededround$d1_teamscombined_gc <- NULL
  d1_goalconcededmatrix[,i_d1_krounds] <- d1_goalconcededround

}

d1_goalconcededmatrix <- cbind(d1_teams,d1_goalconcededmatrix)
##############################################################################################
#d2
d2_krounds <- tail(unique(D2_rounds$d2_matchday),1)
d2_goalconcededmatrix <- data.frame(matrix(nrow = length(d2_teams),ncol = d2_krounds))
d2_goalconcededround <- c()
for(i_d2_krounds in 1:d2_krounds)
{
  d2_homegoalconceded <- D2_rounds$FTAG[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awaygoalconceded <- D2_rounds$FTHG[D2_rounds$d2_matchday == i_d2_krounds]

  d2_hometeamstemp_gc <- D2_rounds$HomeTeam[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awayteamstemp_gc <- D2_rounds$AwayTeam[D2_rounds$d2_matchday== i_d2_krounds]

  d2_goalsconcededcombined <- c(d2_homegoalconceded,d2_awaygoalconceded)
  d2_teamscombined_gc <- c(d2_hometeamstemp_gc,d2_awayteamstemp_gc)

  d2_goalconcededround <- data.frame(d2_teamscombined_gc,d2_goalsconcededcombined)

  d2_goalconcededround <- d2_goalconcededround[order(d2_goalconcededround$d2_teamscombined_gc),]
  d2_goalconcededround$d2_teamscombined_gc <- NULL
  d2_goalconcededmatrix[,i_d2_krounds] <- d2_goalconcededround

}

d2_goalconcededmatrix <- cbind(d2_teams,d2_goalconcededmatrix)
##############################################################################################
#e0
e0_krounds <- tail(unique(E0_rounds$e0_matchday),1)
e0_goalconcededmatrix <- data.frame(matrix(nrow = length(e0_teams),ncol = e0_krounds))
e0_goalconcededround <- c()
for(i_e0_krounds in 1:e0_krounds)
{
  e0_homegoalconceded <- E0_rounds$FTAG[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awaygoalconceded <- E0_rounds$FTHG[E0_rounds$e0_matchday == i_e0_krounds]

  e0_hometeamstemp_gc <- E0_rounds$HomeTeam[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awayteamstemp_gc <- E0_rounds$AwayTeam[E0_rounds$e0_matchday== i_e0_krounds]

  e0_goalsconcededcombined <- c(e0_homegoalconceded,e0_awaygoalconceded)
  e0_teamscombined_gc <- c(e0_hometeamstemp_gc,e0_awayteamstemp_gc)

  e0_goalconcededround <- data.frame(e0_teamscombined_gc,e0_goalsconcededcombined)

  e0_goalconcededround <- e0_goalconcededround[order(e0_goalconcededround$e0_teamscombined_gc),]
  e0_goalconcededround$e0_teamscombined_gc <- NULL
  e0_goalconcededmatrix[,i_e0_krounds] <- e0_goalconcededround

}

e0_goalconcededmatrix <- cbind(e0_teams,e0_goalconcededmatrix)
##############################################################################################
#e1
e1_krounds <- tail(unique(E1_rounds$e1_matchday),1)
e1_goalconcededmatrix <- data.frame(matrix(nrow = length(e1_teams),ncol = e1_krounds))
e1_goalconcededround <- c()
for(i_e1_krounds in 1:e1_krounds)
{
  e1_homegoalconceded <- E1_rounds$FTAG[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awaygoalconceded <- E1_rounds$FTHG[E1_rounds$e1_matchday == i_e1_krounds]

  e1_hometeamstemp_gc <- E1_rounds$HomeTeam[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awayteamstemp_gc <- E1_rounds$AwayTeam[E1_rounds$e1_matchday== i_e1_krounds]

  e1_goalsconcededcombined <- c(e1_homegoalconceded,e1_awaygoalconceded)
  e1_teamscombined_gc <- c(e1_hometeamstemp_gc,e1_awayteamstemp_gc)

  e1_goalconcededround <- data.frame(e1_teamscombined_gc,e1_goalsconcededcombined)

  e1_goalconcededround <- e1_goalconcededround[order(e1_goalconcededround$e1_teamscombined_gc),]
  e1_goalconcededround$e1_teamscombined_gc <- NULL
  e1_goalconcededmatrix[,i_e1_krounds] <- e1_goalconcededround

}

e1_goalconcededmatrix <- cbind(e1_teams,e1_goalconcededmatrix)
##############################################################################################
#e2
e2_krounds <- tail(unique(E2_rounds$e2_matchday),1)
e2_goalconcededmatrix <- data.frame(matrix(nrow = length(e2_teams),ncol = e2_krounds))
e2_goalconcededround <- c()
for(i_e2_krounds in 1:e2_krounds)
{
  e2_homegoalconceded <- E2_rounds$FTAG[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awaygoalconceded <- E2_rounds$FTHG[E2_rounds$e2_matchday == i_e2_krounds]

  e2_hometeamstemp_gc <- E2_rounds$HomeTeam[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awayteamstemp_gc <- E2_rounds$AwayTeam[E2_rounds$e2_matchday== i_e2_krounds]

  e2_goalsconcededcombined <- c(e2_homegoalconceded,e2_awaygoalconceded)
  e2_teamscombined_gc <- c(e2_hometeamstemp_gc,e2_awayteamstemp_gc)

  e2_goalconcededround <- data.frame(e2_teamscombined_gc,e2_goalsconcededcombined)

  e2_goalconcededround <- e2_goalconcededround[order(e2_goalconcededround$e2_teamscombined_gc),]
  e2_goalconcededround$e2_teamscombined_gc <- NULL
  e2_goalconcededmatrix[,i_e2_krounds] <- e2_goalconcededround

}

e2_goalconcededmatrix <- cbind(e2_teams,e2_goalconcededmatrix)
##############################################################################################
#e3
e3_krounds <- tail(unique(E3_rounds$e3_matchday),1)
e3_goalconcededmatrix <- data.frame(matrix(nrow = length(e3_teams),ncol = e3_krounds))
e3_goalconcededround <- c()
for(i_e3_krounds in 1:e3_krounds)
{
  e3_homegoalconceded <- E3_rounds$FTAG[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awaygoalconceded <- E3_rounds$FTHG[E3_rounds$e3_matchday == i_e3_krounds]

  e3_hometeamstemp_gc <- E3_rounds$HomeTeam[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awayteamstemp_gc <- E3_rounds$AwayTeam[E3_rounds$e3_matchday== i_e3_krounds]

  e3_goalsconcededcombined <- c(e3_homegoalconceded,e3_awaygoalconceded)
  e3_teamscombined_gc <- c(e3_hometeamstemp_gc,e3_awayteamstemp_gc)

  e3_goalconcededround <- data.frame(e3_teamscombined_gc,e3_goalsconcededcombined)

  e3_goalconcededround <- e3_goalconcededround[order(e3_goalconcededround$e3_teamscombined_gc),]
  e3_goalconcededround$e3_teamscombined_gc <- NULL
  e3_goalconcededmatrix[,i_e3_krounds] <- e3_goalconcededround

}

e3_goalconcededmatrix <- cbind(e3_teams,e3_goalconcededmatrix)
##############################################################################################
#ec
ec_krounds <- tail(unique(EC_rounds$ec_matchday),1)
ec_goalconcededmatrix <- data.frame(matrix(nrow = length(ec_teams),ncol = ec_krounds))
ec_goalconcededround <- c()
for(i_ec_krounds in 1:ec_krounds)
{
  ec_homegoalconceded <- EC_rounds$FTAG[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awaygoalconceded <- EC_rounds$FTHG[EC_rounds$ec_matchday == i_ec_krounds]

  ec_hometeamstemp_gc <- EC_rounds$HomeTeam[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awayteamstemp_gc <- EC_rounds$AwayTeam[EC_rounds$ec_matchday== i_ec_krounds]

  ec_goalsconcededcombined <- c(ec_homegoalconceded,ec_awaygoalconceded)
  ec_teamscombined_gc <- c(ec_hometeamstemp_gc,ec_awayteamstemp_gc)

  ec_goalconcededround <- data.frame(ec_teamscombined_gc,ec_goalsconcededcombined)

  ec_goalconcededround <- ec_goalconcededround[order(ec_goalconcededround$ec_teamscombined_gc),]
  ec_goalconcededround$ec_teamscombined_gc <- NULL
  ec_goalconcededmatrix[,i_ec_krounds] <- ec_goalconcededround

}

ec_goalconcededmatrix <- cbind(ec_teams,ec_goalconcededmatrix)
##############################################################################################
#f1
f1_krounds <- tail(unique(F1_rounds$f1_matchday),1)
f1_goalconcededmatrix <- data.frame(matrix(nrow = length(f1_teams),ncol = f1_krounds))
f1_goalconcededround <- c()
for(i_f1_krounds in 1:f1_krounds)
{
  f1_homegoalconceded <- F1_rounds$FTAG[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awaygoalconceded <- F1_rounds$FTHG[F1_rounds$f1_matchday == i_f1_krounds]

  f1_hometeamstemp_gc <- F1_rounds$HomeTeam[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awayteamstemp_gc <- F1_rounds$AwayTeam[F1_rounds$f1_matchday== i_f1_krounds]

  f1_goalsconcededcombined <- c(f1_homegoalconceded,f1_awaygoalconceded)
  f1_teamscombined_gc <- c(f1_hometeamstemp_gc,f1_awayteamstemp_gc)

  f1_goalconcededround <- data.frame(f1_teamscombined_gc,f1_goalsconcededcombined)

  f1_goalconcededround <- f1_goalconcededround[order(f1_goalconcededround$f1_teamscombined_gc),]
  f1_goalconcededround$f1_teamscombined_gc <- NULL
  f1_goalconcededmatrix[,i_f1_krounds] <- f1_goalconcededround

}

f1_goalconcededmatrix <- cbind(f1_teams,f1_goalconcededmatrix)
##############################################################################################
#f2
f2_krounds <- tail(unique(B1_rounds$f2_matchday),1)
f2_goalconcededmatrix <- data.frame(matrix(nrow = length(f2_teams),ncol = f2_krounds))
f2_goalconcededround <- c()
for(i_f2_krounds in 1:f2_krounds)
{
  f2_homegoalconceded <- B1_rounds$FTAG[B1_rounds$f2_matchday == i_f2_krounds]

  f2_awaygoalconceded <- B1_rounds$FTHG[B1_rounds$f2_matchday == i_f2_krounds]

  f2_hometeamstemp_gc <- B1_rounds$HomeTeam[B1_rounds$f2_matchday == i_f2_krounds]

  f2_awayteamstemp_gc <- F2_rounds$AwayTeam[F2_rounds$f2_matchday== i_f2_krounds]

  f2_goalsconcededcombined <- c(f2_homegoalconceded,f2_awaygoalconceded)
  f2_teamscombined_gc <- c(f2_hometeamstemp_gc,f2_awayteamstemp_gc)

  f2_goalconcededround <- data.frame(f2_teamscombined_gc,f2_goalsconcededcombined)

  f2_goalconcededround <- f2_goalconcededround[order(f2_goalconcededround$f2_teamscombined_gc),]
  f2_goalconcededround$f2_teamscombined_gc <- NULL
  f2_goalconcededmatrix[,i_f2_krounds] <- f2_goalconcededround

}

f2_goalconcededmatrix <- cbind(f2_teams,f2_goalconcededmatrix)
##############################################################################################
#g1
g1_krounds <- tail(unique(G1_rounds$g1_matchday),1)
g1_goalconcededmatrix <- data.frame(matrix(nrow = length(g1_teams),ncol = g1_krounds))
g1_goalconcededround <- c()
for(i_g1_krounds in 1:g1_krounds)
{
  g1_homegoalconceded <- G1_rounds$FTAG[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awaygoalconceded <- G1_rounds$FTHG[G1_rounds$g1_matchday == i_g1_krounds]

  g1_hometeamstemp_gc <- G1_rounds$HomeTeam[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awayteamstemp_gc <- G1_rounds$AwayTeam[G1_rounds$g1_matchday== i_g1_krounds]

  g1_goalsconcededcombined <- c(g1_homegoalconceded,g1_awaygoalconceded)
  g1_teamscombined_gc <- c(g1_hometeamstemp_gc,g1_awayteamstemp_gc)

  g1_goalconcededround <- data.frame(g1_teamscombined_gc,g1_goalsconcededcombined)

  g1_goalconcededround <- g1_goalconcededround[order(g1_goalconcededround$g1_teamscombined_gc),]
  g1_goalconcededround$g1_teamscombined_gc <- NULL
  g1_goalconcededmatrix[,i_g1_krounds] <- g1_goalconcededround

}

g1_goalconcededmatrix <- cbind(g1_teams,g1_goalconcededmatrix)
##############################################################################################
#b1
i1_krounds <- tail(unique(I1_rounds$i1_matchday),1)
i1_goalconcededmatrix <- data.frame(matrix(nrow = length(i1_teams),ncol = i1_krounds))
i1_goalconcededround <- c()
for(i_i1_krounds in 1:i1_krounds)
{
  i1_homegoalconceded <- I1_rounds$FTAG[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awaygoalconceded <- I1_rounds$FTHG[I1_rounds$i1_matchday == i_i1_krounds]

  i1_hometeamstemp_gc <- I1_rounds$HomeTeam[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awayteamstemp_gc <- I1_rounds$AwayTeam[I1_rounds$i1_matchday== i_i1_krounds]

  i1_goalsconcededcombined <- c(i1_homegoalconceded,i1_awaygoalconceded)
  i1_teamscombined_gc <- c(i1_hometeamstemp_gc,i1_awayteamstemp_gc)

  i1_goalconcededround <- data.frame(i1_teamscombined_gc,i1_goalsconcededcombined)

  i1_goalconcededround <- i1_goalconcededround[order(i1_goalconcededround$i1_teamscombined_gc),]
  i1_goalconcededround$i1_teamscombined_gc <- NULL
  i1_goalconcededmatrix[,i_i1_krounds] <- i1_goalconcededround

}

i1_goalconcededmatrix <- cbind(i1_teams,i1_goalconcededmatrix)
##############################################################################################
#i2
i2_krounds <- tail(unique(I2_rounds$i2_matchday),1)
i2_goalconcededmatrix <- data.frame(matrix(nrow = length(i2_teams),ncol = i2_krounds))
i2_goalconcededround <- c()
for(i_i2_krounds in 1:i2_krounds)
{
  i2_homegoalconceded <- I2_rounds$FTAG[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awaygoalconceded <- I2_rounds$FTHG[I2_rounds$i2_matchday == i_i2_krounds]

  i2_hometeamstemp_gc <- I2_rounds$HomeTeam[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awayteamstemp_gc <- I2_rounds$AwayTeam[I2_rounds$i2_matchday== i_i2_krounds]

  i2_goalsconcededcombined <- c(i2_homegoalconceded,i2_awaygoalconceded)
  i2_teamscombined_gc <- c(i2_hometeamstemp_gc,i2_awayteamstemp_gc)

  i2_goalconcededround <- data.frame(i2_teamscombined_gc,i2_goalsconcededcombined)

  i2_goalconcededround <- i2_goalconcededround[order(i2_goalconcededround$i2_teamscombined_gc),]
  i2_goalconcededround$i2_teamscombined_gc <- NULL
  i2_goalconcededmatrix[,i_i2_krounds] <- i2_goalconcededround

}

i2_goalconcededmatrix <- cbind(i2_teams,i2_goalconcededmatrix)
##############################################################################################
#n1
n1_krounds <- tail(unique(N1_rounds$n1_matchday),1)
n1_goalconcededmatrix <- data.frame(matrix(nrow = length(n1_teams),ncol = n1_krounds))
n1_goalconcededround <- c()
for(i_n1_krounds in 1:n1_krounds)
{
  n1_homegoalconceded <- N1_rounds$FTAG[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awaygoalconceded <- N1_rounds$FTHG[N1_rounds$n1_matchday == i_n1_krounds]

  n1_hometeamstemp_gc <- N1_rounds$HomeTeam[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awayteamstemp_gc <- N1_rounds$AwayTeam[N1_rounds$n1_matchday== i_n1_krounds]

  n1_goalsconcededcombined <- c(n1_homegoalconceded,n1_awaygoalconceded)
  n1_teamscombined_gc <- c(n1_hometeamstemp_gc,n1_awayteamstemp_gc)

  n1_goalconcededround <- data.frame(n1_teamscombined_gc,n1_goalsconcededcombined)

  n1_goalconcededround <- n1_goalconcededround[order(n1_goalconcededround$n1_teamscombined_gc),]
  n1_goalconcededround$n1_teamscombined_gc <- NULL
  n1_goalconcededmatrix[,i_n1_krounds] <- n1_goalconcededround

}

n1_goalconcededmatrix <- cbind(n1_teams,n1_goalconcededmatrix)
##############################################################################################
#p1
p1_krounds <- tail(unique(P1_rounds$p1_matchday),1)
p1_goalconcededmatrix <- data.frame(matrix(nrow = length(p1_teams),ncol = p1_krounds))
p1_goalconcededround <- c()
for(i_p1_krounds in 1:p1_krounds)
{
  p1_homegoalconceded <- P1_rounds$FTAG[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awaygoalconceded <- P1_rounds$FTHG[P1_rounds$p1_matchday == i_p1_krounds]

  p1_hometeamstemp_gc <- P1_rounds$HomeTeam[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awayteamstemp_gc <- P1_rounds$AwayTeam[P1_rounds$p1_matchday== i_p1_krounds]

  p1_goalsconcededcombined <- c(p1_homegoalconceded,p1_awaygoalconceded)
  p1_teamscombined_gc <- c(p1_hometeamstemp_gc,p1_awayteamstemp_gc)

  p1_goalconcededround <- data.frame(p1_teamscombined_gc,p1_goalsconcededcombined)

  p1_goalconcededround <- p1_goalconcededround[order(p1_goalconcededround$p1_teamscombined_gc),]
  p1_goalconcededround$p1_teamscombined_gc <- NULL
  p1_goalconcededmatrix[,i_p1_krounds] <- p1_goalconcededround

}

p1_goalconcededmatrix <- cbind(p1_teams,p1_goalconcededmatrix)
##############################################################################################
#sp1
sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1)
sp1_goalconcededmatrix <- data.frame(matrix(nrow = length(sp1_teams),ncol = sp1_krounds))
sp1_goalconcededround <- c()
for(i_sp1_krounds in 1:sp1_krounds)
{
  sp1_homegoalconceded <- SP1_rounds$FTAG[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awaygoalconceded <- SP1_rounds$FTHG[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_hometeamstemp_gc <- SP1_rounds$HomeTeam[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awayteamstemp_gc <- SP1_rounds$AwayTeam[SP1_rounds$sp1_matchday== i_sp1_krounds]

  sp1_goalsconcededcombined <- c(sp1_homegoalconceded,sp1_awaygoalconceded)
  sp1_teamscombined_gc <- c(sp1_hometeamstemp_gc,sp1_awayteamstemp_gc)

  sp1_goalconcededround <- data.frame(sp1_teamscombined_gc,sp1_goalsconcededcombined)

  sp1_goalconcededround <- sp1_goalconcededround[order(sp1_goalconcededround$sp1_teamscombined_gc),]
  sp1_goalconcededround$sp1_teamscombined_gc <- NULL
  sp1_goalconcededmatrix[,i_sp1_krounds] <- sp1_goalconcededround

}

sp1_goalconcededmatrix <- cbind(sp1_teams,sp1_goalconcededmatrix)
##############################################################################################
#sp2
sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
sp2_goalconcededmatrix <- data.frame(matrix(nrow = length(sp2_teams),ncol = sp2_krounds))
sp2_goalconcededround <- c()
for(i_sp2_krounds in 1:sp2_krounds)
{
  sp2_homegoalconceded <- SP2_rounds$FTAG[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awaygoalconceded <- SP2_rounds$FTHG[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_hometeamstemp_gc <- SP2_rounds$HomeTeam[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awayteamstemp_gc <- SP2_rounds$AwayTeam[SP2_rounds$sp2_matchday== i_sp2_krounds]

  sp2_goalsconcededcombined <- c(sp2_homegoalconceded,sp2_awaygoalconceded)
  sp2_teamscombined_gc <- c(sp2_hometeamstemp_gc,sp2_awayteamstemp_gc)

  sp2_goalconcededround <- data.frame(sp2_teamscombined_gc,sp2_goalsconcededcombined)

  sp2_goalconcededround <- sp2_goalconcededround[order(sp2_goalconcededround$sp2_teamscombined_gc),]
  sp2_goalconcededround$sp2_teamscombined_gc <- NULL
  sp2_goalconcededmatrix[,i_sp2_krounds] <- sp2_goalconcededround

}

sp2_goalconcededmatrix <- cbind(sp2_teams,sp2_goalconcededmatrix)
##############################################################################################
#sc0
sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
sc0_goalconcededmatrix <- data.frame(matrix(nrow = length(sc0_teams),ncol = sc0_krounds))
sc0_goalconcededround <- c()
for(i_sc0_krounds in 1:sc0_krounds)
{
  sc0_homegoalconceded <- SC0_rounds$FTAG[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awaygoalconceded <- SC0_rounds$FTHG[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_hometeamstemp_gc <- SC0_rounds$HomeTeam[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awayteamstemp_gc <- SC0_rounds$AwayTeam[SC0_rounds$sc0_matchday== i_sc0_krounds]

  sc0_goalsconcededcombined <- c(sc0_homegoalconceded,sc0_awaygoalconceded)
  sc0_teamscombined_gc <- c(sc0_hometeamstemp_gc,sc0_awayteamstemp_gc)

  sc0_goalconcededround <- data.frame(sc0_teamscombined_gc,sc0_goalsconcededcombined)

  sc0_goalconcededround <- sc0_goalconcededround[order(sc0_goalconcededround$sc0_teamscombined_gc),]
  sc0_goalconcededround$sc0_teamscombined_gc <- NULL
  sc0_goalconcededmatrix[,i_sc0_krounds] <- sc0_goalconcededround

}

sc0_goalconcededmatrix <- cbind(sc0_teams,sc0_goalconcededmatrix)
##############################################################################################
#sc1
sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
sc1_goalconcededmatrix <- data.frame(matrix(nrow = length(sc1_teams),ncol = sc1_krounds))
sc1_goalconcededround <- c()
for(i_sc1_krounds in 1:sc1_krounds)
{
  sc1_homegoalconceded <- SC1_rounds$FTAG[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awaygoalconceded <- SC1_rounds$FTHG[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_hometeamstemp_gc <- SC1_rounds$HomeTeam[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awayteamstemp_gc <- SC1_rounds$AwayTeam[SC1_rounds$sc1_matchday== i_sc1_krounds]

  sc1_goalsconcededcombined <- c(sc1_homegoalconceded,sc1_awaygoalconceded)
  sc1_teamscombined_gc <- c(sc1_hometeamstemp_gc,sc1_awayteamstemp_gc)

  sc1_goalconcededround <- data.frame(sc1_teamscombined_gc,sc1_goalsconcededcombined)

  sc1_goalconcededround <- sc1_goalconcededround[order(sc1_goalconcededround$sc1_teamscombined_gc),]
  sc1_goalconcededround$sc1_teamscombined_gc <- NULL
  sc1_goalconcededmatrix[,i_sc1_krounds] <- sc1_goalconcededround

}

sc1_goalconcededmatrix <- cbind(sc1_teams,sc1_goalconcededmatrix)
##############################################################################################
#sc2
sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
sc2_goalconcededmatrix <- data.frame(matrix(nrow = length(sc2_teams),ncol = sc2_krounds))
sc2_goalconcededround <- c()
for(i_sc2_krounds in 1:sc2_krounds)
{
  sc2_homegoalconceded <- SC2_rounds$FTAG[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awaygoalconceded <- SC2_rounds$FTHG[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_hometeamstemp_gc <- SC2_rounds$HomeTeam[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awayteamstemp_gc <- SC2_rounds$AwayTeam[SC2_rounds$sc2_matchday== i_sc2_krounds]

  sc2_goalsconcededcombined <- c(sc2_homegoalconceded,sc2_awaygoalconceded)
  sc2_teamscombined_gc <- c(sc2_hometeamstemp_gc,sc2_awayteamstemp_gc)

  sc2_goalconcededround <- data.frame(sc2_teamscombined_gc,sc2_goalsconcededcombined)

  sc2_goalconcededround <- sc2_goalconcededround[order(sc2_goalconcededround$sc2_teamscombined_gc),]
  sc2_goalconcededround$sc2_teamscombined_gc <- NULL
  sc2_goalconcededmatrix[,i_sc2_krounds] <- sc2_goalconcededround

}

sc2_goalconcededmatrix <- cbind(sc2_teams,sc2_goalconcededmatrix)
##############################################################################################
#sc3
sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
sc3_goalconcededmatrix <- data.frame(matrix(nrow = length(sc3_teams),ncol = sc3_krounds))
sc3_goalconcededround <- c()
for(i_sc3_krounds in 1:sc3_krounds)
{
  sc3_homegoalconceded <- SC3_rounds$FTAG[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awaygoalconceded <- SC3_rounds$FTHG[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_hometeamstemp_gc <- SC3_rounds$HomeTeam[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awayteamstemp_gc <- SC3_rounds$AwayTeam[SC3_rounds$sc3_matchday== i_sc3_krounds]

  sc3_goalsconcededcombined <- c(sc3_homegoalconceded,sc3_awaygoalconceded)
  sc3_teamscombined_gc <- c(sc3_hometeamstemp_gc,sc3_awayteamstemp_gc)

  sc3_goalconcededround <- data.frame(sc3_teamscombined_gc,sc3_goalsconcededcombined)

  sc3_goalconcededround <- sc3_goalconcededround[order(sc3_goalconcededround$sc3_teamscombined_gc),]
  sc3_goalconcededround$sc3_teamscombined_gc <- NULL
  sc3_goalconcededmatrix[,i_sc3_krounds] <- sc3_goalconcededround

}

sc3_goalconcededmatrix <- cbind(sc3_teams,sc3_goalconcededmatrix)
##############################################################################################
#t1
t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
t1_goalconcededmatrix <- data.frame(matrix(nrow = length(t1_teams),ncol = t1_krounds))
t1_goalconcededround <- c()
for(i_t1_krounds in 1:t1_krounds)
{
  t1_homegoalconceded <- T1_rounds$FTAG[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awaygoalconceded <- T1_rounds$FTHG[T1_rounds$t1_matchday == i_t1_krounds]

  t1_hometeamstemp_gc <- T1_rounds$HomeTeam[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awayteamstemp_gc <- T1_rounds$AwayTeam[T1_rounds$t1_matchday== i_t1_krounds]

  t1_goalsconcededcombined <- c(t1_homegoalconceded,t1_awaygoalconceded)
  t1_teamscombined_gc <- c(t1_hometeamstemp_gc,t1_awayteamstemp_gc)

  t1_goalconcededround <- data.frame(t1_teamscombined_gc,t1_goalsconcededcombined)

  t1_goalconcededround <- t1_goalconcededround[order(t1_goalconcededround$t1_teamscombined_gc),]
  t1_goalconcededround$t1_teamscombined_gc <- NULL
  t1_goalconcededmatrix[,i_t1_krounds] <- t1_goalconcededround

}

t1_goalconcededmatrix <- cbind(t1_teams,t1_goalconcededmatrix)
##############################################################################################







































