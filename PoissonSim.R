
#################################################
###
poisson_round <- 6
allhomepoisson <- c()
allawaypoisson <- c()
while (poisson_round <= 30){
ROUND_20212022 <- poisson_round

#########################################################
B1 <- B1_rounds[B1_rounds$b1_matchday <= ROUND_20212022,]
B1$FTHG <- as.numeric(B1$FTHG)
B1$FTAG <- as.numeric(B1$FTAG)
B1$TG <- as.numeric(B1$TG)
D1 <- D1_rounds[D1_rounds$d1_matchday <= ROUND_20212022,]
D1$FTHG <- as.numeric(D1$FTHG)
D1$FTAG <- as.numeric(D1$FTAG)
D1$TG <- as.numeric(D1$TG)
D2 <- D2_rounds[D2_rounds$d2_matchday <= ROUND_20212022,]
D2$FTHG <- as.numeric(D2$FTHG)
D2$FTAG <- as.numeric(D2$FTAG)
D2$TG <- as.numeric(D2$TG)
E0 <- E0_rounds[E0_rounds$e0_matchday <= ROUND_20212022,]
E0$FTHG <- as.numeric(E0$FTHG)
E0$FTAG <- as.numeric(E0$FTAG)
E0$TG <- as.numeric(E0$TG)
E1 <- E1_rounds[E1_rounds$e1_matchday <= ROUND_20212022,]
E1$FTHG <- as.numeric(E1$FTHG)
E1$FTAG <- as.numeric(E1$FTAG)
E1$TG <- as.numeric(E1$TG)
E2 <- E2_rounds[E2_rounds$e2_matchday <= ROUND_20212022,]
E2$FTHG <- as.numeric(E2$FTHG)
E2$FTAG <- as.numeric(E2$FTAG)
E2$TG <- as.numeric(E2$TG)
E3 <- E3_rounds[E3_rounds$e3_matchday <= ROUND_20212022,]
E3$FTHG <- as.numeric(E3$FTHG)
E3$FTAG <- as.numeric(E3$FTAG)
E3$TG <- as.numeric(E3$TG)
EC <- EC_rounds[EC_rounds$ec_matchday <= ROUND_20212022,]
EC$FTHG <- as.numeric(EC$FTHG)
EC$FTAG <- as.numeric(EC$FTAG)

EC$TG <- as.numeric(EC$TG)
F1 <- F1_rounds[F1_rounds$f1_matchday <= ROUND_20212022,]
F1$FTHG <- as.numeric(F1$FTHG)
F1$FTAG <- as.numeric(F1$FTAG)
F1$TG <- as.numeric(F1$TG)
F2 <- F2_rounds[F2_rounds$f2_matchday <= ROUND_20212022,]
F2$FTHG <- as.numeric(F2$FTHG)
F2$FTAG <- as.numeric(F2$FTAG)
F2$TG <- as.numeric(F2$TG)
G1 <- G1_rounds[G1_rounds$g1_matchday <= ROUND_20212022,]
G1$FTHG <- as.numeric(G1$FTHG)
G1$FTAG <- as.numeric(G1$FTAG)
G1$TG <- as.numeric(G1$TG)
I1 <- I1_rounds[I1_rounds$i1_matchday <= ROUND_20212022,]
I1$FTHG <- as.numeric(I1$FTHG)
I1$FTAG <- as.numeric(I1$FTAG)
I1$TG <- as.numeric(I1$TG)
I2 <- I2_rounds[I2_rounds$i2_matchday <= ROUND_20212022,]
I2$FTHG <- as.numeric(I2$FTHG)
I2$FTAG <- as.numeric(I2$FTAG)
I2$TG <- as.numeric(I2$TG)
N1 <- N1_rounds[N1_rounds$n1_matchday <= ROUND_20212022,]
N1$FTHG <- as.numeric(N1$FTHG)
N1$FTAG <- as.numeric(N1$FTAG)
N1$TG <- as.numeric(N1$TG)
P1 <- P1_rounds[P1_rounds$p1_matchday <= ROUND_20212022,]
P1$FTHG <- as.numeric(P1$FTHG)
P1$FTAG <- as.numeric(P1$FTAG)
P1$TG <- as.numeric(P1$TG)
SP1 <- SP1_rounds[SP1_rounds$sp1_matchday <= ROUND_20212022,]
SP1$FTHG <- as.numeric(SP1$FTHG)
SP1$FTAG <- as.numeric(SP1$FTAG)
SP1$TG <- as.numeric(SP1$TG)
SP2 <- SP2_rounds[SP2_rounds$sp2_matchday <= ROUND_20212022,]
SP2$FTHG <- as.numeric(SP2$FTHG)
SP2$FTAG <- as.numeric(SP2$FTAG)
SP2$TG <- as.numeric(SP2$TG)
SC0 <- SC0_rounds[SC0_rounds$sc0_matchday <= ROUND_20212022,]
SC0$FTHG <- as.numeric(SC0$FTHG)
SC0$FTAG <- as.numeric(SC0$FTAG)
SC0$TG <- as.numeric(SC0$TG)
SC1 <- SC1_rounds[SC1_rounds$sc1_matchday <= ROUND_20212022,]
SC1$FTHG <- as.numeric(SC1$FTHG)
SC1$FTAG <- as.numeric(SC1$FTAG)
SC1$TG <- as.numeric(SC1$TG)
SC2 <- SC2_rounds[SC2_rounds$sc2_matchday <= ROUND_20212022,]
SC2$FTHG <- as.numeric(SC2$FTHG)
SC2$FTAG <- as.numeric(SC2$FTAG)
SC2$TG <- as.numeric(SC2$TG)
SC3 <- SC3_rounds[SC3_rounds$sc3_matchday <= ROUND_20212022,]
SC3$FTHG <- as.numeric(SC3$FTHG)
SC3$FTAG <- as.numeric(SC3$FTAG)
SC3$TG <- as.numeric(SC3$TG)
T1 <- T1_rounds[T1_rounds$t1_matchday <= ROUND_20212022,]
T1$FTHG <- as.numeric(T1$FTHG)
T1$FTAG <- as.numeric(T1$FTAG)
T1$TG <- as.numeric(T1$TG)

#############################################################
source("goaltotalsv2.R")
source("ShotsAnalysis.R")
source("PoissonModel.R")
i2_totalgames_poiss <- c()
i2_totalgames_poiss[1:length(i2_teams)] <- paste("no",poisson_round,sep = "")
i2_home_poisson <- cbind(i2_home_poisson,i2_totalgames_poiss)
i2_away_poisson <- cbind(i2_away_poisson,i2_totalgames_poiss)

write.csv(i2_home_poisson,paste("i2_home_poisson",ROUND_20212022,".csv",sep = "_"))
write.csv(i2_away_poisson,paste("i2_away_poisson",ROUND_20212022,".csv",sep = "_"))


allhomepoisson <- rbind(allhomepoisson,i2_home_poisson)
allawaypoisson <- rbind(allawaypoisson,i2_away_poisson)
# allhomepoisson <- rbind(b1_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,
#                          e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,
#                          i2_home_poisson,n1_home_poisson,p1_home_poisson,sp1_home_poisson,sp2_home_poisson,sc0_home_poisson,
#                          sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,t1_home_poisson)
#
#
#
# allawaypoisson <- rbind(b1_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,
#                         e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,
#                         i2_away_poisson,n1_away_poisson,p1_away_poisson,sp1_away_poisson,sp2_away_poisson,sc0_away_poisson,
#                         sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,t1_away_poisson)

poisson_round <- poisson_round + 6

}


write.csv(allhomepoisson,"allhomepoisson.csv")
write.csv(allawaypoisson,"allawaypoisson.csv")




