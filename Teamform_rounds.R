#b1
#b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_formmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_formround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homeform <- B1_rounds$FTR[B1_rounds$b1_matchday == i_b1_krounds]

  b1_homeform <- sub("H","W",b1_homeform)
  b1_homeform <- sub("A","L",b1_homeform)

  b1_awayform <- B1_rounds$FTR[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayform <- sub("A","W",b1_awayform)
  b1_awayform <- sub("H","L",b1_awayform)

  b1_hometeamstemp_form <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_form <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_formcombined <- c(b1_homeform,b1_awayform)
  b1_teamscombined_form <- c(b1_hometeamstemp_form,b1_awayteamstemp_form)

  b1_formround <- data.frame(b1_teamscombined_form,b1_formcombined)

  b1_formround <- b1_formround[order(b1_formround$b1_teamscombined_form),]
  b1_formround$b1_teamscombined_form <- NULL
  b1_formmatrix[,i_b1_krounds] <- b1_formround

}

b1_formmatrix <- cbind(b1_teams,b1_formmatrix)
##########################################################################################
#d1
#d1_krounds <- tail(unique(D1_rounds$d1_matchday),1)
d1_formmatrix <- data.frame(matrix(nrow = length(d1_teams),ncol = d1_krounds))
d1_formround <- c()
for(i_d1_krounds in 1:d1_krounds)
{
  d1_homeform <- D1_rounds$FTR[D1_rounds$d1_matchday == i_d1_krounds]

  d1_homeform <- sub("H","W",d1_homeform)
  d1_homeform <- sub("A","L",d1_homeform)

  d1_awayform <- D1_rounds$FTR[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awayform <- sub("A","W",d1_awayform)
  d1_awayform <- sub("H","L",d1_awayform)

  d1_hometeamstemp_form <- D1_rounds$HomeTeam[D1_rounds$d1_matchday == i_d1_krounds]

  d1_awayteamstemp_form <- D1_rounds$AwayTeam[D1_rounds$d1_matchday== i_d1_krounds]

  d1_formcombined <- c(d1_homeform,d1_awayform)
  d1_teamscombined_form <- c(d1_hometeamstemp_form,d1_awayteamstemp_form)

  d1_formround <- data.frame(d1_teamscombined_form,d1_formcombined)

  d1_formround <- d1_formround[order(d1_formround$d1_teamscombined_form),]
  d1_formround$d1_teamscombined_form <- NULL
  d1_formmatrix[,i_d1_krounds] <- d1_formround

}

d1_formmatrix <- cbind(d1_teams,d1_formmatrix)
##########################################################################################
#d2
#d2_krounds <- tail(unique(D2_rounds$d2_matchday),1)
d2_formmatrix <- data.frame(matrix(nrow = length(d2_teams),ncol = d2_krounds))
d2_formround <- c()
for(i_d2_krounds in 1:d2_krounds)
{
  d2_homeform <- D2_rounds$FTR[D2_rounds$d2_matchday == i_d2_krounds]

  d2_homeform <- sub("H","W",d2_homeform)
  d2_homeform <- sub("A","L",d2_homeform)

  d2_awayform <- D2_rounds$FTR[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awayform <- sub("A","W",d2_awayform)
  d2_awayform <- sub("H","L",d2_awayform)

  d2_hometeamstemp_form <- D2_rounds$HomeTeam[D2_rounds$d2_matchday == i_d2_krounds]

  d2_awayteamstemp_form <- D2_rounds$AwayTeam[D2_rounds$d2_matchday== i_d2_krounds]

  d2_formcombined <- c(d2_homeform,d2_awayform)
  d2_teamscombined_form <- c(d2_hometeamstemp_form,d2_awayteamstemp_form)

  d2_formround <- data.frame(d2_teamscombined_form,d2_formcombined)

  d2_formround <- d2_formround[order(d2_formround$d2_teamscombined_form),]
  d2_formround$d2_teamscombined_form <- NULL
  d2_formmatrix[,i_d2_krounds] <- d2_formround

}

d2_formmatrix <- cbind(d2_teams,d2_formmatrix)
##########################################################################################
#e0
#e0_krounds <- tail(unique(E0_rounds$e0_matchday),1)
e0_formmatrix <- data.frame(matrix(nrow = length(e0_teams),ncol = e0_krounds))
e0_formround <- c()
for(i_e0_krounds in 1:e0_krounds)
{
  e0_homeform <- E0_rounds$FTR[E0_rounds$e0_matchday == i_e0_krounds]

  e0_homeform <- sub("H","W",e0_homeform)
  e0_homeform <- sub("A","L",e0_homeform)

  e0_awayform <- E0_rounds$FTR[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awayform <- sub("A","W",e0_awayform)
  e0_awayform <- sub("H","L",e0_awayform)

  e0_hometeamstemp_form <- E0_rounds$HomeTeam[E0_rounds$e0_matchday == i_e0_krounds]

  e0_awayteamstemp_form <- E0_rounds$AwayTeam[E0_rounds$e0_matchday== i_e0_krounds]

  e0_formcombined <- c(e0_homeform,e0_awayform)
  e0_teamscombined_form <- c(e0_hometeamstemp_form,e0_awayteamstemp_form)

  e0_formround <- data.frame(e0_teamscombined_form,e0_formcombined)

  e0_formround <- e0_formround[order(e0_formround$e0_teamscombined_form),]
  e0_formround$e0_teamscombined_form <- NULL
  e0_formmatrix[,i_e0_krounds] <- e0_formround

}

e0_formmatrix <- cbind(e0_teams,e0_formmatrix)
##########################################################################################
#e1
#e1_krounds <- tail(unique(E1_rounds$e1_matchday),1)
e1_formmatrix <- data.frame(matrix(nrow = length(e1_teams),ncol = e1_krounds))
e1_formround <- c()
for(i_e1_krounds in 1:e1_krounds)
{
  e1_homeform <- E1_rounds$FTR[E1_rounds$e1_matchday == i_e1_krounds]

  e1_homeform <- sub("H","W",e1_homeform)
  e1_homeform <- sub("A","L",e1_homeform)

  e1_awayform <- E1_rounds$FTR[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awayform <- sub("A","W",e1_awayform)
  e1_awayform <- sub("H","L",e1_awayform)

  e1_hometeamstemp_form <- E1_rounds$HomeTeam[E1_rounds$e1_matchday == i_e1_krounds]

  e1_awayteamstemp_form <- E1_rounds$AwayTeam[E1_rounds$e1_matchday== i_e1_krounds]

  e1_formcombined <- c(e1_homeform,e1_awayform)
  e1_teamscombined_form <- c(e1_hometeamstemp_form,e1_awayteamstemp_form)

  e1_formround <- data.frame(e1_teamscombined_form,e1_formcombined)

  e1_formround <- e1_formround[order(e1_formround$e1_teamscombined_form),]
  e1_formround$e1_teamscombined_form <- NULL
  e1_formmatrix[,i_e1_krounds] <- e1_formround

}

e1_formmatrix <- cbind(e1_teams,e1_formmatrix)
##########################################################################################
#e2
#e2_krounds <- tail(unique(E2_rounds$e2_matchday),1)
e2_formmatrix <- data.frame(matrix(nrow = length(e2_teams),ncol = e2_krounds))
e2_formround <- c()
for(i_e2_krounds in 1:e2_krounds)
{
  e2_homeform <- E2_rounds$FTR[E2_rounds$e2_matchday == i_e2_krounds]

  e2_homeform <- sub("H","W",e2_homeform)
  e2_homeform <- sub("A","L",e2_homeform)

  e2_awayform <- E2_rounds$FTR[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awayform <- sub("A","W",e2_awayform)
  e2_awayform <- sub("H","L",e2_awayform)

  e2_hometeamstemp_form <- E2_rounds$HomeTeam[E2_rounds$e2_matchday == i_e2_krounds]

  e2_awayteamstemp_form <- E2_rounds$AwayTeam[E2_rounds$e2_matchday== i_e2_krounds]

  e2_formcombined <- c(e2_homeform,e2_awayform)
  e2_teamscombined_form <- c(e2_hometeamstemp_form,e2_awayteamstemp_form)

  e2_formround <- data.frame(e2_teamscombined_form,e2_formcombined)

  e2_formround <- e2_formround[order(e2_formround$e2_teamscombined_form),]
  e2_formround$e2_teamscombined_form <- NULL
  e2_formmatrix[,i_e2_krounds] <- e2_formround

}

e2_formmatrix <- cbind(e2_teams,e2_formmatrix)
##########################################################################################
#e3
#e3_krounds <- tail(unique(E3_rounds$e3_matchday),1)
e3_formmatrix <- data.frame(matrix(nrow = length(e3_teams),ncol = e3_krounds))
e3_formround <- c()
for(i_e3_krounds in 1:e3_krounds)
{
  e3_homeform <- E3_rounds$FTR[E3_rounds$e3_matchday == i_e3_krounds]

  e3_homeform <- sub("H","W",e3_homeform)
  e3_homeform <- sub("A","L",e3_homeform)

  e3_awayform <- E3_rounds$FTR[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awayform <- sub("A","W",e3_awayform)
  e3_awayform <- sub("H","L",e3_awayform)

  e3_hometeamstemp_form <- E3_rounds$HomeTeam[E3_rounds$e3_matchday == i_e3_krounds]

  e3_awayteamstemp_form <- E3_rounds$AwayTeam[E3_rounds$e3_matchday== i_e3_krounds]

  e3_formcombined <- c(e3_homeform,e3_awayform)
  e3_teamscombined_form <- c(e3_hometeamstemp_form,e3_awayteamstemp_form)

  e3_formround <- data.frame(e3_teamscombined_form,e3_formcombined)

  e3_formround <- e3_formround[order(e3_formround$e3_teamscombined_form),]
  e3_formround$e3_teamscombined_form <- NULL
  e3_formmatrix[,i_e3_krounds] <- e3_formround

}

e3_formmatrix <- cbind(e3_teams,e3_formmatrix)
##########################################################################################
#ec
#ec_krounds <- tail(unique(EC_rounds$ec_matchday),1)
ec_formmatrix <- data.frame(matrix(nrow = length(ec_teams),ncol = ec_krounds))
ec_formround <- c()
for(i_ec_krounds in 1:ec_krounds)
{
  ec_homeform <- EC_rounds$FTR[EC_rounds$ec_matchday == i_ec_krounds]

  ec_homeform <- sub("H","W",ec_homeform)
  ec_homeform <- sub("A","L",ec_homeform)

  ec_awayform <- EC_rounds$FTR[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awayform <- sub("A","W",ec_awayform)
  ec_awayform <- sub("H","L",ec_awayform)

  ec_hometeamstemp_form <- EC_rounds$HomeTeam[EC_rounds$ec_matchday == i_ec_krounds]

  ec_awayteamstemp_form <- EC_rounds$AwayTeam[EC_rounds$ec_matchday== i_ec_krounds]

  ec_formcombined <- c(ec_homeform,ec_awayform)
  ec_teamscombined_form <- c(ec_hometeamstemp_form,ec_awayteamstemp_form)

  ec_formround <- data.frame(ec_teamscombined_form,ec_formcombined)

  ec_formround <- ec_formround[order(ec_formround$ec_teamscombined_form),]
  ec_formround$ec_teamscombined_form <- NULL
  ec_formmatrix[,i_ec_krounds] <- ec_formround
ec_formmatrix
}

ec_formmatrix <- cbind(ec_teams,ec_formmatrix)
##########################################################################################
#f1
#f1_krounds <- tail(unique(F1_rounds$f1_matchday),1)
f1_formmatrix <- data.frame(matrix(nrow = length(f1_teams),ncol = f1_krounds))
f1_formround <- c()
for(i_f1_krounds in 1:f1_krounds)
{
  f1_homeform <- F1_rounds$FTR[F1_rounds$f1_matchday == i_f1_krounds]

  f1_homeform <- sub("H","W",f1_homeform)
  f1_homeform <- sub("A","L",f1_homeform)

  f1_awayform <- F1_rounds$FTR[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awayform <- sub("A","W",f1_awayform)
  f1_awayform <- sub("H","L",f1_awayform)

  f1_hometeamstemp_form <- F1_rounds$HomeTeam[F1_rounds$f1_matchday == i_f1_krounds]

  f1_awayteamstemp_form <- F1_rounds$AwayTeam[F1_rounds$f1_matchday== i_f1_krounds]

  f1_formcombined <- c(f1_homeform,f1_awayform)
  f1_teamscombined_form <- c(f1_hometeamstemp_form,f1_awayteamstemp_form)

  f1_formround <- data.frame(f1_teamscombined_form,f1_formcombined)

  f1_formround <- f1_formround[order(f1_formround$f1_teamscombined_form),]
  f1_formround$f1_teamscombined_form <- NULL
  f1_formmatrix[,i_f1_krounds] <- f1_formround

}

f1_formmatrix <- cbind(f1_teams,f1_formmatrix)
##########################################################################################
#f2
#f2_krounds <- tail(unique(F2_rounds$f2_matchday),1)
f2_formmatrix <- data.frame(matrix(nrow = length(f2_teams),ncol = f2_krounds))
f2_formround <- c()
for(i_f2_krounds in 1:f2_krounds)
{
  f2_homeform <- F2_rounds$FTR[F2_rounds$f2_matchday == i_f2_krounds]

  f2_homeform <- sub("H","W",f2_homeform)
  f2_homeform <- sub("A","L",f2_homeform)

  f2_awayform <- F2_rounds$FTR[F2_rounds$f2_matchday == i_f2_krounds]

  f2_awayform <- sub("A","W",f2_awayform)
  f2_awayform <- sub("H","L",f2_awayform)

  f2_hometeamstemp_form <- F2_rounds$HomeTeam[F2_rounds$f2_matchday == i_f2_krounds]

  f2_awayteamstemp_form <- F2_rounds$AwayTeam[F2_rounds$f2_matchday== i_f2_krounds]

  f2_formcombined <- c(f2_homeform,f2_awayform)
  f2_teamscombined_form <- c(f2_hometeamstemp_form,f2_awayteamstemp_form)

  f2_formround <- data.frame(f2_teamscombined_form,f2_formcombined)

  f2_formround <- f2_formround[order(f2_formround$f2_teamscombined_form),]
  f2_formround$f2_teamscombined_form <- NULL
  f2_formmatrix[,i_f2_krounds] <- f2_formround

}

f2_formmatrix <- cbind(f2_teams,f2_formmatrix)
##########################################################################################
#g1
#g1_krounds <- tail(unique(G1_rounds$g1_matchday),1)
g1_formmatrix <- data.frame(matrix(nrow = length(g1_teams),ncol = g1_krounds))
g1_formround <- c()
for(i_g1_krounds in 1:g1_krounds)
{
  g1_homeform <- G1_rounds$FTR[G1_rounds$g1_matchday == i_g1_krounds]

  g1_homeform <- sub("H","W",g1_homeform)
  g1_homeform <- sub("A","L",g1_homeform)

  g1_awayform <- G1_rounds$FTR[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awayform <- sub("A","W",g1_awayform)
  g1_awayform <- sub("H","L",g1_awayform)

  g1_hometeamstemp_form <- G1_rounds$HomeTeam[G1_rounds$g1_matchday == i_g1_krounds]

  g1_awayteamstemp_form <- G1_rounds$AwayTeam[G1_rounds$g1_matchday== i_g1_krounds]

  g1_formcombined <- c(g1_homeform,g1_awayform)
  g1_teamscombined_form <- c(g1_hometeamstemp_form,g1_awayteamstemp_form)

  g1_formround <- data.frame(g1_teamscombined_form,g1_formcombined)

  g1_formround <- g1_formround[order(g1_formround$g1_teamscombined_form),]
  g1_formround$g1_teamscombined_form <- NULL
  g1_formmatrix[,i_g1_krounds] <- g1_formround

}

g1_formmatrix <- cbind(g1_teams,g1_formmatrix)
##########################################################################################
#i1
#i1_krounds <- tail(unique(I1_rounds$i1_matchday),1)
i1_formmatrix <- data.frame(matrix(nrow = length(i1_teams),ncol = i1_krounds))
i1_formround <- c()
for(i_i1_krounds in 1:i1_krounds)
{
  i1_homeform <- I1_rounds$FTR[I1_rounds$i1_matchday == i_i1_krounds]

  i1_homeform <- sub("H","W",i1_homeform)
  i1_homeform <- sub("A","L",i1_homeform)

  i1_awayform <- I1_rounds$FTR[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awayform <- sub("A","W",i1_awayform)
  i1_awayform <- sub("H","L",i1_awayform)

  i1_hometeamstemp_form <- I1_rounds$HomeTeam[I1_rounds$i1_matchday == i_i1_krounds]

  i1_awayteamstemp_form <- I1_rounds$AwayTeam[I1_rounds$i1_matchday== i_i1_krounds]

  i1_formcombined <- c(i1_homeform,i1_awayform)
  i1_teamscombined_form <- c(i1_hometeamstemp_form,i1_awayteamstemp_form)

  i1_formround <- data.frame(i1_teamscombined_form,i1_formcombined)

  i1_formround <- i1_formround[order(i1_formround$i1_teamscombined_form),]
  i1_formround$i1_teamscombined_form <- NULL
  i1_formmatrix[,i_i1_krounds] <- i1_formround

}

i1_formmatrix <- cbind(i1_teams,i1_formmatrix)
##########################################################################################
#i2
#i2_krounds <- tail(unique(I2_rounds$i2_matchday),1)
i2_formmatrix <- data.frame(matrix(nrow = length(i2_teams),ncol = i2_krounds))
i2_formround <- c()
for(i_i2_krounds in 1:i2_krounds)
{
  i2_homeform <- I2_rounds$FTR[I2_rounds$i2_matchday == i_i2_krounds]

  i2_homeform <- sub("H","W",i2_homeform)
  i2_homeform <- sub("A","L",i2_homeform)

  i2_awayform <- I2_rounds$FTR[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awayform <- sub("A","W",i2_awayform)
  i2_awayform <- sub("H","L",i2_awayform)

  i2_hometeamstemp_form <- I2_rounds$HomeTeam[I2_rounds$i2_matchday == i_i2_krounds]

  i2_awayteamstemp_form <- I2_rounds$AwayTeam[I2_rounds$i2_matchday== i_i2_krounds]

  i2_formcombined <- c(i2_homeform,i2_awayform)
  i2_teamscombined_form <- c(i2_hometeamstemp_form,i2_awayteamstemp_form)

  i2_formround <- data.frame(i2_teamscombined_form,i2_formcombined)

  i2_formround <- i2_formround[order(i2_formround$i2_teamscombined_form),]
  i2_formround$i2_teamscombined_form <- NULL
  i2_formmatrix[,i_i2_krounds] <- i2_formround

}

i2_formmatrix <- cbind(i2_teams,i2_formmatrix)
##########################################################################################
#n1
#n1_krounds <- tail(unique(N1_rounds$n1_matchday),1)
n1_formmatrix <- data.frame(matrix(nrow = length(n1_teams),ncol = n1_krounds))
n1_formround <- c()
for(i_n1_krounds in 1:n1_krounds)
{
  n1_homeform <- N1_rounds$FTR[N1_rounds$n1_matchday == i_n1_krounds]

  n1_homeform <- sub("H","W",n1_homeform)
  n1_homeform <- sub("A","L",n1_homeform)

  n1_awayform <- N1_rounds$FTR[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awayform <- sub("A","W",n1_awayform)
  n1_awayform <- sub("H","L",n1_awayform)

  n1_hometeamstemp_form <- N1_rounds$HomeTeam[N1_rounds$n1_matchday == i_n1_krounds]

  n1_awayteamstemp_form <- N1_rounds$AwayTeam[N1_rounds$n1_matchday== i_n1_krounds]

  n1_formcombined <- c(n1_homeform,n1_awayform)
  n1_teamscombined_form <- c(n1_hometeamstemp_form,n1_awayteamstemp_form)

  n1_formround <- data.frame(n1_teamscombined_form,n1_formcombined)

  n1_formround <- n1_formround[order(n1_formround$n1_teamscombined_form),]
  n1_formround$n1_teamscombined_form <- NULL
  n1_formmatrix[,i_n1_krounds] <- n1_formround

}

n1_formmatrix <- cbind(n1_teams,n1_formmatrix)
##########################################################################################
#p1
#p1_krounds <- tail(unique(P1_rounds$p1_matchday),1)
p1_formmatrix <- data.frame(matrix(nrow = length(p1_teams),ncol = p1_krounds))
p1_formround <- c()
for(i_p1_krounds in 1:p1_krounds)
{
  p1_homeform <- P1_rounds$FTR[P1_rounds$p1_matchday == i_p1_krounds]

  p1_homeform <- sub("H","W",p1_homeform)
  p1_homeform <- sub("A","L",p1_homeform)

  p1_awayform <- P1_rounds$FTR[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awayform <- sub("A","W",p1_awayform)
  p1_awayform <- sub("H","L",p1_awayform)

  p1_hometeamstemp_form <- P1_rounds$HomeTeam[P1_rounds$p1_matchday == i_p1_krounds]

  p1_awayteamstemp_form <- P1_rounds$AwayTeam[P1_rounds$p1_matchday== i_p1_krounds]

  p1_formcombined <- c(p1_homeform,p1_awayform)
  p1_teamscombined_form <- c(p1_hometeamstemp_form,p1_awayteamstemp_form)

  p1_formround <- data.frame(p1_teamscombined_form,p1_formcombined)

  p1_formround <- p1_formround[order(p1_formround$p1_teamscombined_form),]
  p1_formround$p1_teamscombined_form <- NULL
  p1_formmatrix[,i_p1_krounds] <- p1_formround

}

p1_formmatrix <- cbind(p1_teams,p1_formmatrix)
##########################################################################################
#sp1
#sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1)
sp1_formmatrix <- data.frame(matrix(nrow = length(sp1_teams),ncol = sp1_krounds))
sp1_formround <- c()
for(i_sp1_krounds in 1:sp1_krounds)
{
  sp1_homeform <- SP1_rounds$FTR[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_homeform <- sub("H","W",sp1_homeform)
  sp1_homeform <- sub("A","L",sp1_homeform)

  sp1_awayform <- SP1_rounds$FTR[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awayform <- sub("A","W",sp1_awayform)
  sp1_awayform <- sub("H","L",sp1_awayform)

  sp1_hometeamstemp_form <- SP1_rounds$HomeTeam[SP1_rounds$sp1_matchday == i_sp1_krounds]

  sp1_awayteamstemp_form <- SP1_rounds$AwayTeam[SP1_rounds$sp1_matchday== i_sp1_krounds]

  sp1_formcombined <- c(sp1_homeform,sp1_awayform)
  sp1_teamscombined_form <- c(sp1_hometeamstemp_form,sp1_awayteamstemp_form)

  sp1_formround <- data.frame(sp1_teamscombined_form,sp1_formcombined)

  sp1_formround <- sp1_formround[order(sp1_formround$sp1_teamscombined_form),]
  sp1_formround$sp1_teamscombined_form <- NULL
  sp1_formmatrix[,i_sp1_krounds] <- sp1_formround

}

sp1_formmatrix <- cbind(sp1_teams,sp1_formmatrix)
##########################################################################################
#sp2
#sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
sp2_formmatrix <- data.frame(matrix(nrow = length(sp2_teams),ncol = sp2_krounds))
sp2_formround <- c()
for(i_sp2_krounds in 1:sp2_krounds)
{
  sp2_homeform <- SP2_rounds$FTR[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_homeform <- sub("H","W",sp2_homeform)
  sp2_homeform <- sub("A","L",sp2_homeform)

  sp2_awayform <- SP2_rounds$FTR[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awayform <- sub("A","W",sp2_awayform)
  sp2_awayform <- sub("H","L",sp2_awayform)

  sp2_hometeamstemp_form <- SP2_rounds$HomeTeam[SP2_rounds$sp2_matchday == i_sp2_krounds]

  sp2_awayteamstemp_form <- SP2_rounds$AwayTeam[SP2_rounds$sp2_matchday== i_sp2_krounds]

  sp2_formcombined <- c(sp2_homeform,sp2_awayform)
  sp2_teamscombined_form <- c(sp2_hometeamstemp_form,sp2_awayteamstemp_form)

  sp2_formround <- data.frame(sp2_teamscombined_form,sp2_formcombined)

  sp2_formround <- sp2_formround[order(sp2_formround$sp2_teamscombined_form),]
  sp2_formround$sp2_teamscombined_form <- NULL
  sp2_formmatrix[,i_sp2_krounds] <- sp2_formround

}

sp2_formmatrix <- cbind(sp2_teams,sp2_formmatrix)
##########################################################################################
#sc0
#sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
sc0_formmatrix <- data.frame(matrix(nrow = length(sc0_teams),ncol = sc0_krounds))
sc0_formround <- c()
for(i_sc0_krounds in 1:sc0_krounds)
{
  sc0_homeform <- SC0_rounds$FTR[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_homeform <- sub("H","W",sc0_homeform)
  sc0_homeform <- sub("A","L",sc0_homeform)

  sc0_awayform <- SC0_rounds$FTR[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awayform <- sub("A","W",sc0_awayform)
  sc0_awayform <- sub("H","L",sc0_awayform)

  sc0_hometeamstemp_form <- SC0_rounds$HomeTeam[SC0_rounds$sc0_matchday == i_sc0_krounds]

  sc0_awayteamstemp_form <- SC0_rounds$AwayTeam[SC0_rounds$sc0_matchday== i_sc0_krounds]

  sc0_formcombined <- c(sc0_homeform,sc0_awayform)
  sc0_teamscombined_form <- c(sc0_hometeamstemp_form,sc0_awayteamstemp_form)

  sc0_formround <- data.frame(sc0_teamscombined_form,sc0_formcombined)

  sc0_formround <- sc0_formround[order(sc0_formround$sc0_teamscombined_form),]
  sc0_formround$sc0_teamscombined_form <- NULL
  sc0_formmatrix[,i_sc0_krounds] <- sc0_formround

}

sc0_formmatrix <- cbind(sc0_teams,sc0_formmatrix)
##########################################################################################
#sc1
#sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
sc1_formmatrix <- data.frame(matrix(nrow = length(sc1_teams),ncol = sc1_krounds))
sc1_formround <- c()
for(i_sc1_krounds in 1:sc1_krounds)
{
  sc1_homeform <- SC1_rounds$FTR[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_homeform <- sub("H","W",sc1_homeform)
  sc1_homeform <- sub("A","L",sc1_homeform)

  sc1_awayform <- SC1_rounds$FTR[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awayform <- sub("A","W",sc1_awayform)
  sc1_awayform <- sub("H","L",sc1_awayform)

  sc1_hometeamstemp_form <- SC1_rounds$HomeTeam[SC1_rounds$sc1_matchday == i_sc1_krounds]

  sc1_awayteamstemp_form <- SC1_rounds$AwayTeam[SC1_rounds$sc1_matchday== i_sc1_krounds]

  sc1_formcombined <- c(sc1_homeform,sc1_awayform)
  sc1_teamscombined_form <- c(sc1_hometeamstemp_form,sc1_awayteamstemp_form)

  sc1_formround <- data.frame(sc1_teamscombined_form,sc1_formcombined)

  sc1_formround <- sc1_formround[order(sc1_formround$sc1_teamscombined_form),]
  sc1_formround$sc1_teamscombined_form <- NULL
  sc1_formmatrix[,i_sc1_krounds] <- sc1_formround

}

sc1_formmatrix <- cbind(sc1_teams,sc1_formmatrix)
##########################################################################################
#sc2
#sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
sc2_formmatrix <- data.frame(matrix(nrow = length(sc2_teams),ncol = sc2_krounds))
sc2_formround <- c()
for(i_sc2_krounds in 1:sc2_krounds)
{
  sc2_homeform <- SC2_rounds$FTR[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_homeform <- sub("H","W",sc2_homeform)
  sc2_homeform <- sub("A","L",sc2_homeform)

  sc2_awayform <- SC2_rounds$FTR[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awayform <- sub("A","W",sc2_awayform)
  sc2_awayform <- sub("H","L",sc2_awayform)

  sc2_hometeamstemp_form <- SC2_rounds$HomeTeam[SC2_rounds$sc2_matchday == i_sc2_krounds]

  sc2_awayteamstemp_form <- SC2_rounds$AwayTeam[SC2_rounds$sc2_matchday== i_sc2_krounds]

  sc2_formcombined <- c(sc2_homeform,sc2_awayform)
  sc2_teamscombined_form <- c(sc2_hometeamstemp_form,sc2_awayteamstemp_form)

  sc2_formround <- data.frame(sc2_teamscombined_form,sc2_formcombined)

  sc2_formround <- sc2_formround[order(sc2_formround$sc2_teamscombined_form),]
  sc2_formround$sc2_teamscombined_form <- NULL
  sc2_formmatrix[,i_sc2_krounds] <- sc2_formround

}

sc2_formmatrix <- cbind(sc2_teams,sc2_formmatrix)
##########################################################################################
#sc3
#sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
sc3_formmatrix <- data.frame(matrix(nrow = length(sc3_teams),ncol = sc3_krounds))
sc3_formround <- c()
for(i_sc3_krounds in 1:sc3_krounds)
{
  sc3_homeform <- SC3_rounds$FTR[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_homeform <- sub("H","W",sc3_homeform)
  sc3_homeform <- sub("A","L",sc3_homeform)

  sc3_awayform <- SC3_rounds$FTR[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awayform <- sub("A","W",sc3_awayform)
  sc3_awayform <- sub("H","L",sc3_awayform)

  sc3_hometeamstemp_form <- SC3_rounds$HomeTeam[SC3_rounds$sc3_matchday == i_sc3_krounds]

  sc3_awayteamstemp_form <- SC3_rounds$AwayTeam[SC3_rounds$sc3_matchday== i_sc3_krounds]

  sc3_formcombined <- c(sc3_homeform,sc3_awayform)
  sc3_teamscombined_form <- c(sc3_hometeamstemp_form,sc3_awayteamstemp_form)

  sc3_formround <- data.frame(sc3_teamscombined_form,sc3_formcombined)

  sc3_formround <- sc3_formround[order(sc3_formround$sc3_teamscombined_form),]
  sc3_formround$sc3_teamscombined_form <- NULL
  sc3_formmatrix[,i_sc3_krounds] <- sc3_formround

}

sc3_formmatrix <- cbind(sc3_teams,sc3_formmatrix)
##########################################################################################
#t1
#t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
t1_formmatrix <- data.frame(matrix(nrow = length(t1_teams),ncol = t1_krounds))
t1_formround <- c()
for(i_t1_krounds in 1:t1_krounds)
{
  t1_homeform <- T1_rounds$FTR[T1_rounds$t1_matchday == i_t1_krounds]

  t1_homeform <- sub("H","W",t1_homeform)
  t1_homeform <- sub("A","L",t1_homeform)

  t1_awayform <- T1_rounds$FTR[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awayform <- sub("A","W",t1_awayform)
  t1_awayform <- sub("H","L",t1_awayform)

  t1_hometeamstemp_form <- T1_rounds$HomeTeam[T1_rounds$t1_matchday == i_t1_krounds]

  t1_awayteamstemp_form <- T1_rounds$AwayTeam[T1_rounds$t1_matchday== i_t1_krounds]

  t1_formcombined <- c(t1_homeform,t1_awayform)
  t1_teamscombined_form <- c(t1_hometeamstemp_form,t1_awayteamstemp_form)

  t1_formround <- data.frame(t1_teamscombined_form,t1_formcombined)

  t1_formround <- t1_formround[order(t1_formround$t1_teamscombined_form),]
  t1_formround$t1_teamscombined_form <- NULL
  t1_formmatrix[,i_t1_krounds] <- t1_formround

}

t1_formmatrix <- cbind(t1_teams,t1_formmatrix)
##########################################################################################
















