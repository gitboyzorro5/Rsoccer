#b1
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goaltotalmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goaltotalround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoaltotal <- B1_rounds$TG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoaltotal <- B1_rounds$TG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_tg <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_tg <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalscombined_tg <- c(b1_homegoaltotal,b1_awaygoaltotal)
  b1_teamscombined_tg <- c(b1_hometeamstemp_tg,b1_awayteamstemp_tg)

  b1_goaltotalround <- data.frame(b1_teamscombined_tg,b1_goalscombined_tg)

  b1_goaltotalround <- b1_goaltotalround[order(b1_goaltotalround$b1_teamscombined_tg),]
  b1_goaltotalround$b1_teamscombined_tg <- NULL
  b1_goaltotalmatrix[,i_b1_krounds] <- b1_goaltotalround

}

b1_goaltotalmatrix <- cbind(b1_teams,b1_goaltotalmatrix)
##############################################################################################
#d1
d1_krounds <- tail(unique(D1_rounds$d1_matchday),1)
d1_goaltotalmatrix <- data.frame(matrix(nrow = length(d1_teams),ncol = d1_krounds))
d1_goaltotalround <- c()
for(i_d1_krounds in 1:d1_krounds)
{
  d1_homegoaltotal <- D1_rounds$TG[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awaygoaltotal <- D1_rounds$TG[D1_rounds$d1_matchday == i_d1_krounds]

  d1_hometeamstemp_tg <- D1_rounds$HomeTeam[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awayteamstemp_tg <- D1_rounds$AwayTeam[D1_rounds$d1_matchday== i_d1_krounds]

  d1_goalscombined_tg <- c(d1_homegoaltotal,d1_awaygoaltotal)
  d1_teamscombined_tg <- c(d1_hometeamstemp_tg,d1_awayteamstemp_tg)

  d1_goaltotalround <- data.frame(d1_teamscombined_tg,d1_goalscombined_tg)

  d1_goaltotalround <- d1_goaltotalround[order(d1_goaltotalround$d1_teamscombined_tg),]
  d1_goaltotalround$d1_teamscombined_tg <- NULL
  d1_goaltotalmatrix[,i_d1_krounds] <- d1_goaltotalround

}

d1_goaltotalmatrix <- cbind(d1_teams,d1_goaltotalmatrix)
##############################################################################################
#d2
d2_krounds <- tail(unique(D2_rounds$d2_matchday),1)
d2_goaltotalmatrix <- data.frame(matrix(nrow = length(d2_teams),ncol = d2_krounds))
d2_goaltotalround <- c()
for(i_d2_krounds in 1:d2_krounds)
{
  d2_homegoaltotal <- D2_rounds$TG[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awaygoaltotal <- D2_rounds$TG[D2_rounds$d2_matchday == i_d2_krounds]

  d2_hometeamstemp_tg <- D2_rounds$HomeTeam[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awayteamstemp_tg <- D2_rounds$AwayTeam[D2_rounds$d2_matchday== i_d2_krounds]

  d2_goalscombined_tg <- c(d2_homegoaltotal,d2_awaygoaltotal)
  d2_teamscombined_tg <- c(d2_hometeamstemp_tg,d2_awayteamstemp_tg)

  d2_goaltotalround <- data.frame(d2_teamscombined_tg,d2_goalscombined_tg)

  d2_goaltotalround <- d2_goaltotalround[order(d2_goaltotalround$d2_teamscombined_tg),]
  d2_goaltotalround$d2_teamscombined_tg <- NULL
  d2_goaltotalmatrix[,i_d2_krounds] <- d2_goaltotalround

}

d2_goaltotalmatrix <- cbind(d2_teams,d2_goaltotalmatrix)
##############################################################################################
#e0
e0_krounds <- tail(unique(E0_rounds$e0_matchday),1)
e0_goaltotalmatrix <- data.frame(matrix(nrow = length(e0_teams),ncol = e0_krounds))
e0_goaltotalround <- c()
for(i_e0_krounds in 1:e0_krounds)
{
  e0_homegoaltotal <- E0_rounds$TG[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awaygoaltotal <- E0_rounds$TG[E0_rounds$e0_matchday == i_e0_krounds]

  e0_hometeamstemp_tg <- E0_rounds$HomeTeam[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awayteamstemp_tg <- E0_rounds$AwayTeam[E0_rounds$e0_matchday== i_e0_krounds]

  e0_goalscombined_tg <- c(e0_homegoaltotal,e0_awaygoaltotal)
  e0_teamscombined_tg <- c(e0_hometeamstemp_tg,e0_awayteamstemp_tg)

  e0_goaltotalround <- data.frame(e0_teamscombined_tg,e0_goalscombined_tg)

  e0_goaltotalround <- e0_goaltotalround[order(e0_goaltotalround$e0_teamscombined_tg),]
  e0_goaltotalround$e0_teamscombined_tg <- NULL
  e0_goaltotalmatrix[,i_e0_krounds] <- e0_goaltotalround

}

e0_goaltotalmatrix <- cbind(e0_teams,e0_goaltotalmatrix)
##############################################################################################
#e1
e1_krounds <- tail(unique(E1_rounds$e1_matchday),1)
e1_goaltotalmatrix <- data.frame(matrix(nrow = length(e1_teams),ncol = e1_krounds))
e1_goaltotalround <- c()
for(i_e1_krounds in 1:e1_krounds)
{
  e1_homegoaltotal <- E1_rounds$TG[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awaygoaltotal <- E1_rounds$TG[E1_rounds$e1_matchday == i_e1_krounds]

  e1_hometeamstemp_tg <- E1_rounds$HomeTeam[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awayteamstemp_tg <- E1_rounds$AwayTeam[E1_rounds$e1_matchday== i_e1_krounds]

  e1_goalscombined_tg <- c(e1_homegoaltotal,e1_awaygoaltotal)
  e1_teamscombined_tg <- c(e1_hometeamstemp_tg,e1_awayteamstemp_tg)

  e1_goaltotalround <- data.frame(e1_teamscombined_tg,e1_goalscombined_tg)

  e1_goaltotalround <- e1_goaltotalround[order(e1_goaltotalround$e1_teamscombined_tg),]
  e1_goaltotalround$e1_teamscombined_tg <- NULL
  e1_goaltotalmatrix[,i_e1_krounds] <- e1_goaltotalround

}

e1_goaltotalmatrix <- cbind(e1_teams,e1_goaltotalmatrix)
##############################################################################################
#e2
e2_krounds <- tail(unique(E2_rounds$e2_matchday),1)
e2_goaltotalmatrix <- data.frame(matrix(nrow = length(e2_teams),ncol = e2_krounds))
e2_goaltotalround <- c()
for(i_e2_krounds in 1:e2_krounds)
{
  e2_homegoaltotal <- E2_rounds$TG[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awaygoaltotal <- E2_rounds$TG[E2_rounds$e2_matchday == i_e2_krounds]

  e2_hometeamstemp_tg <- E2_rounds$HomeTeam[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awayteamstemp_tg <- E2_rounds$AwayTeam[E2_rounds$e2_matchday== i_e2_krounds]

  e2_goalscombined_tg <- c(e2_homegoaltotal,e2_awaygoaltotal)
  e2_teamscombined_tg <- c(e2_hometeamstemp_tg,e2_awayteamstemp_tg)

  e2_goaltotalround <- data.frame(e2_teamscombined_tg,e2_goalscombined_tg)

  e2_goaltotalround <- e2_goaltotalround[order(e2_goaltotalround$e2_teamscombined_tg),]
  e2_goaltotalround$e2_teamscombined_tg <- NULL
  e2_goaltotalmatrix[,i_e2_krounds] <- e2_goaltotalround

}

e2_goaltotalmatrix <- cbind(e2_teams,e2_goaltotalmatrix)
##############################################################################################
#e3
e3_krounds <- tail(unique(E3_rounds$e3_matchday),1)
e3_goaltotalmatrix <- data.frame(matrix(nrow = length(e3_teams),ncol = e3_krounds))
e3_goaltotalround <- c()
for(i_e3_krounds in 1:e3_krounds)
{
  e3_homegoaltotal <- E3_rounds$TG[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awaygoaltotal <- E3_rounds$TG[E3_rounds$e3_matchday == i_e3_krounds]

  e3_hometeamstemp_tg <- E3_rounds$HomeTeam[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awayteamstemp_tg <- E3_rounds$AwayTeam[E3_rounds$e3_matchday== i_e3_krounds]

  e3_goalscombined_tg <- c(e3_homegoaltotal,e3_awaygoaltotal)
  e3_teamscombined_tg <- c(e3_hometeamstemp_tg,e3_awayteamstemp_tg)

  e3_goaltotalround <- data.frame(e3_teamscombined_tg,e3_goalscombined_tg)

  e3_goaltotalround <- e3_goaltotalround[order(e3_goaltotalround$e3_teamscombined_tg),]
  e3_goaltotalround$e3_teamscombined_tg <- NULL
  e3_goaltotalmatrix[,i_e3_krounds] <- e3_goaltotalround

}

e3_goaltotalmatrix <- cbind(e3_teams,e3_goaltotalmatrix)
##############################################################################################
#ec
ec_krounds <- tail(unique(EC_rounds$ec_matchday),1)
ec_goaltotalmatrix <- data.frame(matrix(nrow = length(ec_teams),ncol = ec_krounds))
ec_goaltotalround <- c()
for(i_ec_krounds in 1:ec_krounds)
{
  ec_homegoaltotal <- EC_rounds$TG[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awaygoaltotal <- EC_rounds$TG[EC_rounds$ec_matchday == i_ec_krounds]

  ec_hometeamstemp_tg <- EC_rounds$HomeTeam[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awayteamstemp_tg <- EC_rounds$AwayTeam[EC_rounds$ec_matchday== i_ec_krounds]

  ec_goalscombined_tg <- c(ec_homegoaltotal,ec_awaygoaltotal)
  ec_teamscombined_tg <- c(ec_hometeamstemp_tg,ec_awayteamstemp_tg)

  ec_goaltotalround <- data.frame(ec_teamscombined_tg,ec_goalscombined_tg)

  ec_goaltotalround <- ec_goaltotalround[order(ec_goaltotalround$ec_teamscombined_tg),]
  ec_goaltotalround$ec_teamscombined_tg <- NULL
  ec_goaltotalmatrix[,i_ec_krounds] <- ec_goaltotalround

}

ec_goaltotalmatrix <- cbind(ec_teams,ec_goaltotalmatrix)
##############################################################################################
#f1
f1_krounds <- tail(unique(F1_rounds$f1_matchday),1)
f1_goaltotalmatrix <- data.frame(matrix(nrow = length(f1_teams),ncol = f1_krounds))
f1_goaltotalround <- c()
for(i_f1_krounds in 1:f1_krounds)
{
  f1_homegoaltotal <- F1_rounds$TG[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awaygoaltotal <- F1_rounds$TG[F1_rounds$f1_matchday == i_f1_krounds]

  f1_hometeamstemp_tg <- F1_rounds$HomeTeam[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awayteamstemp_tg <- F1_rounds$AwayTeam[F1_rounds$f1_matchday== i_f1_krounds]

  f1_goalscombined_tg <- c(f1_homegoaltotal,f1_awaygoaltotal)
  f1_teamscombined_tg <- c(f1_hometeamstemp_tg,f1_awayteamstemp_tg)

  f1_goaltotalround <- data.frame(f1_teamscombined_tg,f1_goalscombined_tg)

  f1_goaltotalround <- f1_goaltotalround[order(f1_goaltotalround$f1_teamscombined_tg),]
  f1_goaltotalround$f1_teamscombined_tg <- NULL
  f1_goaltotalmatrix[,i_f1_krounds] <- f1_goaltotalround

}

f1_goaltotalmatrix <- cbind(f1_teams,f1_goaltotalmatrix)
##############################################################################################
#f2
f2_krounds <- tail(unique(F2_rounds$f2_matchday),1)
f2_goaltotalmatrix <- data.frame(matrix(nrow = length(f2_teams),ncol = f2_krounds))
f2_goaltotalround <- c()
for(i_f2_krounds in 1:f2_krounds)
{
  f2_homegoaltotal <- F2_rounds$TG[F2_rounds$f2_matchday == i_f2_krounds]

  f2_awaygoaltotal <- F2_rounds$TG[F2_rounds$f2_matchday == i_f2_krounds]

  f2_hometeamstemp_tg <- F2_rounds$HomeTeam[F2_rounds$f2_matchday == i_f2_krounds]

  f2_awayteamstemp_tg <- F2_rounds$AwayTeam[F2_rounds$f2_matchday== i_f2_krounds]

  f2_goalscombined_tg <- c(f2_homegoaltotal,f2_awaygoaltotal)
  f2_teamscombined_tg <- c(f2_hometeamstemp_tg,f2_awayteamstemp_tg)

  f2_goaltotalround <- data.frame(f2_teamscombined_tg,f2_goalscombined_tg)

  f2_goaltotalround <- f2_goaltotalround[order(f2_goaltotalround$f2_teamscombined_tg),]
  f2_goaltotalround$f2_teamscombined_tg <- NULL
  f2_goaltotalmatrix[,i_f2_krounds] <- f2_goaltotalround

}

f2_goaltotalmatrix <- cbind(f2_teams,f2_goaltotalmatrix)
##############################################################################################
#g1
g1_krounds <- tail(unique(G1_rounds$g1_matchday),1)
g1_goaltotalmatrix <- data.frame(matrix(nrow = length(g1_teams),ncol = g1_krounds))
g1_goaltotalround <- c()
for(i_g1_krounds in 1:g1_krounds)
{
  g1_homegoaltotal <- G1_rounds$TG[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awaygoaltotal <- G1_rounds$TG[G1_rounds$g1_matchday == i_g1_krounds]

  g1_hometeamstemp_tg <- G1_rounds$HomeTeam[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awayteamstemp_tg <- G1_rounds$AwayTeam[G1_rounds$g1_matchday== i_g1_krounds]

  g1_goalscombined_tg <- c(g1_homegoaltotal,g1_awaygoaltotal)
  g1_teamscombined_tg <- c(g1_hometeamstemp_tg,g1_awayteamstemp_tg)

  g1_goaltotalround <- data.frame(g1_teamscombined_tg,g1_goalscombined_tg)

  g1_goaltotalround <- g1_goaltotalround[order(g1_goaltotalround$g1_teamscombined_tg),]
  g1_goaltotalround$g1_teamscombined_tg <- NULL
  g1_goaltotalmatrix[,i_g1_krounds] <- g1_goaltotalround

}

g1_goaltotalmatrix <- cbind(g1_teams,g1_goaltotalmatrix)
##############################################################################################
#i1
i1_krounds <- tail(unique(I1_rounds$i1_matchday),1)
i1_goaltotalmatrix <- data.frame(matrix(nrow = length(i1_teams),ncol = i1_krounds))
i1_goaltotalround <- c()
for(i_i1_krounds in 1:i1_krounds)
{
  i1_homegoaltotal <- I1_rounds$TG[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awaygoaltotal <- I1_rounds$TG[I1_rounds$i1_matchday == i_i1_krounds]

  i1_hometeamstemp_tg <- I1_rounds$HomeTeam[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awayteamstemp_tg <- I1_rounds$AwayTeam[I1_rounds$i1_matchday== i_i1_krounds]

  i1_goalscombined_tg <- c(i1_homegoaltotal,i1_awaygoaltotal)
  i1_teamscombined_tg <- c(i1_hometeamstemp_tg,i1_awayteamstemp_tg)

  i1_goaltotalround <- data.frame(i1_teamscombined_tg,i1_goalscombined_tg)

  i1_goaltotalround <- i1_goaltotalround[order(i1_goaltotalround$i1_teamscombined_tg),]
  i1_goaltotalround$i1_teamscombined_tg <- NULL
  i1_goaltotalmatrix[,i_i1_krounds] <- i1_goaltotalround

}

i1_goaltotalmatrix <- cbind(i1_teams,i1_goaltotalmatrix)
##############################################################################################
#i2
i2_krounds <- tail(unique(I2_rounds$i2_matchday),1)
i2_goaltotalmatrix <- data.frame(matrix(nrow = length(i2_teams),ncol = i2_krounds))
i2_goaltotalround <- c()
for(i_i2_krounds in 1:i2_krounds)
{
  i2_homegoaltotal <- I2_rounds$TG[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awaygoaltotal <- I2_rounds$TG[I2_rounds$i2_matchday == i_i2_krounds]

  i2_hometeamstemp_tg <- I2_rounds$HomeTeam[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awayteamstemp_tg <- I2_rounds$AwayTeam[I2_rounds$i2_matchday== i_i2_krounds]

  i2_goalscombined_tg <- c(i2_homegoaltotal,i2_awaygoaltotal)
  i2_teamscombined_tg <- c(i2_hometeamstemp_tg,i2_awayteamstemp_tg)

  i2_goaltotalround <- data.frame(i2_teamscombined_tg,i2_goalscombined_tg)

  i2_goaltotalround <- i2_goaltotalround[order(i2_goaltotalround$i2_teamscombined_tg),]
  i2_goaltotalround$i2_teamscombined_tg <- NULL
  i2_goaltotalmatrix[,i_i2_krounds] <- i2_goaltotalround

}

i2_goaltotalmatrix <- cbind(i2_teams,i2_goaltotalmatrix)
##############################################################################################
#n1
n1_krounds <- tail(unique(N1_rounds$n1_matchday),1)
n1_goaltotalmatrix <- data.frame(matrix(nrow = length(n1_teams),ncol = n1_krounds))
n1_goaltotalround <- c()
for(i_n1_krounds in 1:n1_krounds)
{
  n1_homegoaltotal <- N1_rounds$TG[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awaygoaltotal <- N1_rounds$TG[N1_rounds$n1_matchday == i_n1_krounds]

  n1_hometeamstemp_tg <- N1_rounds$HomeTeam[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awayteamstemp_tg <- N1_rounds$AwayTeam[N1_rounds$n1_matchday== i_n1_krounds]

  n1_goalscombined_tg <- c(n1_homegoaltotal,n1_awaygoaltotal)
  n1_teamscombined_tg <- c(n1_hometeamstemp_tg,n1_awayteamstemp_tg)

  n1_goaltotalround <- data.frame(n1_teamscombined_tg,n1_goalscombined_tg)

  n1_goaltotalround <- n1_goaltotalround[order(n1_goaltotalround$n1_teamscombined_tg),]
  n1_goaltotalround$n1_teamscombined_tg <- NULL
  n1_goaltotalmatrix[,i_n1_krounds] <- n1_goaltotalround

}

n1_goaltotalmatrix <- cbind(n1_teams,n1_goaltotalmatrix)
##############################################################################################
#p1
p1_krounds <- tail(unique(P1_rounds$p1_matchday),1)
p1_goaltotalmatrix <- data.frame(matrix(nrow = length(p1_teams),ncol = p1_krounds))
p1_goaltotalround <- c()
for(i_p1_krounds in 1:p1_krounds)
{
  p1_homegoaltotal <- P1_rounds$TG[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awaygoaltotal <- P1_rounds$TG[P1_rounds$p1_matchday == i_p1_krounds]

  p1_hometeamstemp_tg <- P1_rounds$HomeTeam[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awayteamstemp_tg <- P1_rounds$AwayTeam[P1_rounds$p1_matchday== i_p1_krounds]

  p1_goalscombined_tg <- c(p1_homegoaltotal,p1_awaygoaltotal)
  p1_teamscombined_tg <- c(p1_hometeamstemp_tg,p1_awayteamstemp_tg)

  p1_goaltotalround <- data.frame(p1_teamscombined_tg,p1_goalscombined_tg)

  p1_goaltotalround <- p1_goaltotalround[order(p1_goaltotalround$p1_teamscombined_tg),]
  p1_goaltotalround$p1_teamscombined_tg <- NULL
  p1_goaltotalmatrix[,i_p1_krounds] <- p1_goaltotalround

}

p1_goaltotalmatrix <- cbind(p1_teams,p1_goaltotalmatrix)
##############################################################################################
#sp1
sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1)
sp1_goaltotalmatrix <- data.frame(matrix(nrow = length(sp1_teams),ncol = sp1_krounds))
sp1_goaltotalround <- c()
for(i_sp1_krounds in 1:sp1_krounds)
{
  sp1_homegoaltotal <- SP1_rounds$TG[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awaygoaltotal <- SP1_rounds$TG[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_hometeamstemp_tg <- SP1_rounds$HomeTeam[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awayteamstemp_tg <- SP1_rounds$AwayTeam[SP1_rounds$sp1_matchday== i_sp1_krounds]

  sp1_goalscombined_tg <- c(sp1_homegoaltotal,sp1_awaygoaltotal)
  sp1_teamscombined_tg <- c(sp1_hometeamstemp_tg,sp1_awayteamstemp_tg)

  sp1_goaltotalround <- data.frame(sp1_teamscombined_tg,sp1_goalscombined_tg)

  sp1_goaltotalround <- sp1_goaltotalround[order(sp1_goaltotalround$sp1_teamscombined_tg),]
  sp1_goaltotalround$sp1_teamscombined_tg <- NULL
  sp1_goaltotalmatrix[,i_sp1_krounds] <- sp1_goaltotalround

}

sp1_goaltotalmatrix <- cbind(sp1_teams,sp1_goaltotalmatrix)
##############################################################################################
#sp2
sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
sp2_goaltotalmatrix <- data.frame(matrix(nrow = length(sp2_teams),ncol = sp2_krounds))
sp2_goaltotalround <- c()
for(i_sp2_krounds in 1:sp2_krounds)
{
  sp2_homegoaltotal <- SP2_rounds$TG[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awaygoaltotal <- SP2_rounds$TG[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_hometeamstemp_tg <- SP2_rounds$HomeTeam[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awayteamstemp_tg <- SP2_rounds$AwayTeam[SP2_rounds$sp2_matchday== i_sp2_krounds]

  sp2_goalscombined_tg <- c(sp2_homegoaltotal,sp2_awaygoaltotal)
  sp2_teamscombined_tg <- c(sp2_hometeamstemp_tg,sp2_awayteamstemp_tg)

  sp2_goaltotalround <- data.frame(sp2_teamscombined_tg,sp2_goalscombined_tg)

  sp2_goaltotalround <- sp2_goaltotalround[order(sp2_goaltotalround$sp2_teamscombined_tg),]
  sp2_goaltotalround$sp2_teamscombined_tg <- NULL
  sp2_goaltotalmatrix[,i_sp2_krounds] <- sp2_goaltotalround

}

sp2_goaltotalmatrix <- cbind(sp2_teams,sp2_goaltotalmatrix)
##############################################################################################
#sc0
sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
sc0_goaltotalmatrix <- data.frame(matrix(nrow = length(sc0_teams),ncol = sc0_krounds))
sc0_goaltotalround <- c()
for(i_sc0_krounds in 1:sc0_krounds)
{
  sc0_homegoaltotal <- SC0_rounds$TG[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awaygoaltotal <- SC0_rounds$TG[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_hometeamstemp_tg <- SC0_rounds$HomeTeam[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awayteamstemp_tg <- SC0_rounds$AwayTeam[SC0_rounds$sc0_matchday== i_sc0_krounds]

  sc0_goalscombined_tg <- c(sc0_homegoaltotal,sc0_awaygoaltotal)
  sc0_teamscombined_tg <- c(sc0_hometeamstemp_tg,sc0_awayteamstemp_tg)

  sc0_goaltotalround <- data.frame(sc0_teamscombined_tg,sc0_goalscombined_tg)

  sc0_goaltotalround <- sc0_goaltotalround[order(sc0_goaltotalround$sc0_teamscombined_tg),]
  sc0_goaltotalround$sc0_teamscombined_tg <- NULL
  sc0_goaltotalmatrix[,i_sc0_krounds] <- sc0_goaltotalround

}

sc0_goaltotalmatrix <- cbind(sc0_teams,sc0_goaltotalmatrix)
##############################################################################################
#sc1
sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
sc1_goaltotalmatrix <- data.frame(matrix(nrow = length(sc1_teams),ncol = sc1_krounds))
sc1_goaltotalround <- c()
for(i_sc1_krounds in 1:sc1_krounds)
{
  sc1_homegoaltotal <- SC1_rounds$TG[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awaygoaltotal <- SC1_rounds$TG[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_hometeamstemp_tg <- SC1_rounds$HomeTeam[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awayteamstemp_tg <- SC1_rounds$AwayTeam[SC1_rounds$sc1_matchday== i_sc1_krounds]

  sc1_goalscombined_tg <- c(sc1_homegoaltotal,sc1_awaygoaltotal)
  sc1_teamscombined_tg <- c(sc1_hometeamstemp_tg,sc1_awayteamstemp_tg)

  sc1_goaltotalround <- data.frame(sc1_teamscombined_tg,sc1_goalscombined_tg)

  sc1_goaltotalround <- sc1_goaltotalround[order(sc1_goaltotalround$sc1_teamscombined_tg),]
  sc1_goaltotalround$sc1_teamscombined_tg <- NULL
  sc1_goaltotalmatrix[,i_sc1_krounds] <- sc1_goaltotalround

}

sc1_goaltotalmatrix <- cbind(sc1_teams,sc1_goaltotalmatrix)
##############################################################################################
#sc2
sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
sc2_goaltotalmatrix <- data.frame(matrix(nrow = length(sc2_teams),ncol = sc2_krounds))
sc2_goaltotalround <- c()
for(i_sc2_krounds in 1:sc2_krounds)
{
  sc2_homegoaltotal <- SC2_rounds$TG[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awaygoaltotal <- SC2_rounds$TG[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_hometeamstemp_tg <- SC2_rounds$HomeTeam[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awayteamstemp_tg <- SC2_rounds$AwayTeam[SC2_rounds$sc2_matchday== i_sc2_krounds]

  sc2_goalscombined_tg <- c(sc2_homegoaltotal,sc2_awaygoaltotal)
  sc2_teamscombined_tg <- c(sc2_hometeamstemp_tg,sc2_awayteamstemp_tg)

  sc2_goaltotalround <- data.frame(sc2_teamscombined_tg,sc2_goalscombined_tg)

  sc2_goaltotalround <- sc2_goaltotalround[order(sc2_goaltotalround$sc2_teamscombined_tg),]
  sc2_goaltotalround$sc2_teamscombined_tg <- NULL
  sc2_goaltotalmatrix[,i_sc2_krounds] <- sc2_goaltotalround

}

sc2_goaltotalmatrix <- cbind(sc2_teams,sc2_goaltotalmatrix)
##############################################################################################
#sc3
sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
sc3_goaltotalmatrix <- data.frame(matrix(nrow = length(sc3_teams),ncol = sc3_krounds))
sc3_goaltotalround <- c()
for(i_sc3_krounds in 1:sc3_krounds)
{
  sc3_homegoaltotal <- SC3_rounds$TG[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awaygoaltotal <- SC3_rounds$TG[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_hometeamstemp_tg <- SC3_rounds$HomeTeam[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awayteamstemp_tg <- SC3_rounds$AwayTeam[SC3_rounds$sc3_matchday== i_sc3_krounds]

  sc3_goalscombined_tg <- c(sc3_homegoaltotal,sc3_awaygoaltotal)
  sc3_teamscombined_tg <- c(sc3_hometeamstemp_tg,sc3_awayteamstemp_tg)

  sc3_goaltotalround <- data.frame(sc3_teamscombined_tg,sc3_goalscombined_tg)

  sc3_goaltotalround <- sc3_goaltotalround[order(sc3_goaltotalround$sc3_teamscombined_tg),]
  sc3_goaltotalround$sc3_teamscombined_tg <- NULL
  sc3_goaltotalmatrix[,i_sc3_krounds] <- sc3_goaltotalround

}

sc3_goaltotalmatrix <- cbind(sc3_teams,sc3_goaltotalmatrix)
##############################################################################################
#t1
t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
t1_goaltotalmatrix <- data.frame(matrix(nrow = length(t1_teams),ncol = t1_krounds))
t1_goaltotalround <- c()
for(i_t1_krounds in 1:t1_krounds)
{
  t1_homegoaltotal <- T1_rounds$TG[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awaygoaltotal <- T1_rounds$TG[T1_rounds$t1_matchday == i_t1_krounds]

  t1_hometeamstemp_tg <- T1_rounds$HomeTeam[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awayteamstemp_tg <- T1_rounds$AwayTeam[T1_rounds$t1_matchday== i_t1_krounds]

  t1_goalscombined_tg <- c(t1_homegoaltotal,t1_awaygoaltotal)
  t1_teamscombined_tg <- c(t1_hometeamstemp_tg,t1_awayteamstemp_tg)

  t1_goaltotalround <- data.frame(t1_teamscombined_tg,t1_goalscombined_tg)

  t1_goaltotalround <- t1_goaltotalround[order(t1_goaltotalround$t1_teamscombined_tg),]
  t1_goaltotalround$t1_teamscombined_tg <- NULL
  t1_goaltotalmatrix[,i_t1_krounds] <- t1_goaltotalround

}

t1_goaltotalmatrix <- cbind(t1_teams,t1_goaltotalmatrix)
##############################################################################################



















