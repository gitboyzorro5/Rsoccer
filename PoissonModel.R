library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')

#poisson model
#get total games played
b1_GP <- nrow(subset(allteams20202021, Div == "B1"))
d1_GP <- nrow(subset(allteams20202021, Div == "D1"))
d2_GP <- nrow(subset(allteams20202021, Div == "D2"))
e0_GP <- nrow(subset(allteams20202021, Div == "E0"))
e1_GP <- nrow(subset(allteams20202021, Div == "E1"))
e2_GP <- nrow(subset(allteams20202021, Div == "E2"))
e3_GP <- nrow(subset(allteams20202021, Div == "E3"))
ec_GP <- nrow(subset(allteams20202021, Div == "EC"))
f1_GP <- nrow(subset(allteams20202021, Div == "F1"))
f2_GP <- nrow(subset(allteams20202021, Div == "F2"))
g1_GP <- nrow(subset(allteams20202021, Div == "G1"))
i1_GP <- nrow(subset(allteams20202021, Div == "I1"))
i2_GP <- nrow(subset(allteams20202021, Div == "I2"))
n1_GP <- nrow(subset(allteams20202021, Div == "N1"))
p1_GP <- nrow(subset(allteams20202021, Div == "P1"))
sc0_GP <- nrow(subset(allteams20202021, Div == "SC0"))
sc1_GP <- nrow(subset(allteams20202021, Div == "SC1"))
sc2_GP <- nrow(subset(allteams20202021, Div == "SC2"))
sc3_GP <- nrow(subset(allteams20202021, Div == "SC3"))
sp1_GP <- nrow(subset(allteams20202021, Div == "SP1"))
sp2_GP <- nrow(subset(allteams20202021, Div == "SP2"))
t1_GP <- nrow(subset(allteams20202021, Div == "T1"))

#Calculate total home goals for each division
b1_T_HG <- sum(b1_home_gs$x)
d1_T_HG <- sum(d1_home_gs$x)
d2_T_HG <- sum(d2_home_gs$x)
e0_T_HG <- sum(e0_home_gs$x)
e1_T_HG <- sum(e1_home_gs$x)
e2_T_HG <- sum(e2_home_gs$x)
e3_T_HG <- sum(e3_home_gs$x)
ec_T_HG <- sum(ec_home_gs$x)
f1_T_HG <- sum(f1_home_gs$x)
f2_T_HG <- sum(f2_home_gs$x)
g1_T_HG <- sum(g1_home_gs$x)
i1_T_HG <- sum(i1_home_gs$x)
i2_T_HG <- sum(i2_home_gs$x)
n1_T_HG <- sum(n1_home_gs$x)
p1_T_HG <- sum(p1_home_gs$x)
sc0_T_HG <- sum(sc0_home_gs$x)
sc1_T_HG <- sum(sc1_home_gs$x)
sc2_T_HG <- sum(sc2_home_gs$x)
sc3_T_HG <- sum(sc3_home_gs$x)
sp1_T_HG <- sum(sp1_home_gs$x)
sp2_T_HG <- sum(sp2_home_gs$x)
t1_T_HG <- sum(t1_home_gs$x)
#calculate average home goal
b1_avg_HG <- round(b1_T_HG /b1_GP, digits = 4)
d1_avg_HG <- round(d1_T_HG /d1_GP, digits = 4)
d2_avg_HG <- round(d2_T_HG /d2_GP, digits = 4)
e0_avg_HG <- round(e0_T_HG /e0_GP, digits = 4)
e1_avg_HG <- round(e1_T_HG /e1_GP, digits = 4)
e2_avg_HG <- round(e2_T_HG /e2_GP, digits = 4)
e3_avg_HG <- round(e3_T_HG /e3_GP, digits = 4)
ec_avg_HG <- round(ec_T_HG /ec_GP, digits = 4)
f1_avg_HG <- round(f1_T_HG /f1_GP, digits = 4)
f2_avg_HG <- round(f2_T_HG /f2_GP, digits = 4)
g1_avg_HG <- round(g1_T_HG /g1_GP, digits = 4)
i1_avg_HG <- round(i1_T_HG /i1_GP, digits = 4)
i2_avg_HG <- round(i2_T_HG /i2_GP, digits = 4)
n1_avg_HG <- round(n1_T_HG /n1_GP, digits = 4)
p1_avg_HG <- round(p1_T_HG /p1_GP, digits = 4)
sc0_avg_HG <- round(sc0_T_HG /sc0_GP, digits = 4)
sc1_avg_HG <- round(sc1_T_HG /sc1_GP, digits = 4)
sc2_avg_HG <- round(sc2_T_HG /sc2_GP, digits = 4)
sc3_avg_HG <- round(sc3_T_HG /sc3_GP, digits = 4)
sp1_avg_HG <- round(sp1_T_HG /sp1_GP, digits = 4)
sp2_avg_HG <- round(sp2_T_HG /sp2_GP, digits = 4)
t1_avg_HG <- round(t1_T_HG /t1_GP, digits = 4)
############################################################
#Calculate total away goals for each division
b1_T_AG <- sum(b1_away_gs$x)
d1_T_AG <- sum(d1_away_gs$x)
d2_T_AG <- sum(d2_away_gs$x)
e0_T_AG <- sum(e0_away_gs$x)
e1_T_AG <- sum(e1_away_gs$x)
e2_T_AG <- sum(e2_away_gs$x)
e3_T_AG <- sum(e3_away_gs$x)
ec_T_AG <- sum(ec_away_gs$x)
f1_T_AG <- sum(f1_away_gs$x)
f2_T_AG <- sum(f2_away_gs$x)
g1_T_AG <- sum(g1_away_gs$x)
i1_T_AG <- sum(i1_away_gs$x)
i2_T_AG <- sum(i2_away_gs$x)
n1_T_AG <- sum(n1_away_gs$x)
p1_T_AG <- sum(p1_away_gs$x)
sc0_T_AG <- sum(sc0_away_gs$x)
sc1_T_AG <- sum(sc1_away_gs$x)
sc2_T_AG <- sum(sc2_away_gs$x)
sc3_T_AG <- sum(sc3_away_gs$x)
sp1_T_AG <- sum(sp1_away_gs$x)
sp2_T_AG <- sum(sp2_away_gs$x)
t1_T_AG <- sum(t1_away_gs$x)
#calculate average away goal
b1_avg_AG <- round(b1_T_AG /b1_GP, digits = 4)
d1_avg_AG <- round(d1_T_AG /d1_GP, digits = 4)
d2_avg_AG <- round(d2_T_AG /d2_GP, digits = 4)
e0_avg_AG <- round(e0_T_AG /e0_GP, digits = 4)
e1_avg_AG <- round(e1_T_AG /e1_GP, digits = 4)
e2_avg_AG <- round(e2_T_AG /e2_GP, digits = 4)
e3_avg_AG <- round(e3_T_AG /e3_GP, digits = 4)
ec_avg_AG <- round(ec_T_AG /ec_GP, digits = 4)
f1_avg_AG <- round(f1_T_AG /f1_GP, digits = 4)
f2_avg_AG <- round(f2_T_AG /f2_GP, digits = 4)
g1_avg_AG <- round(g1_T_AG /g1_GP, digits = 4)
i1_avg_AG <- round(i1_T_AG /i1_GP, digits = 4)
i2_avg_AG <- round(i2_T_AG /i2_GP, digits = 4)
n1_avg_AG <- round(n1_T_AG /n1_GP, digits = 4)
p1_avg_AG <- round(p1_T_AG /p1_GP, digits = 4)
sc0_avg_AG <- round(sc0_T_AG /sc0_GP, digits = 4)
sc1_avg_AG <- round(sc1_T_AG /sc1_GP, digits = 4)
sc2_avg_AG <- round(sc2_T_AG /sc2_GP, digits = 4)
sc3_avg_AG <- round(sc3_T_AG /sc3_GP, digits = 4)
sp1_avg_AG <- round(sp1_T_AG /sp1_GP, digits = 4)
sp2_avg_AG <- round(sp2_T_AG /sp2_GP, digits = 4)
t1_avg_AG <- round(t1_T_AG /t1_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
b1_home_as <- round(((b1_home_gs$x/b1_home_games))/b1_avg_HG, digits = 4)
d1_home_as <- round(((d1_home_gs$x/d1_home_games))/d1_avg_HG, digits = 4)
d2_home_as <- round(((d2_home_gs$x/d2_home_games))/d2_avg_HG, digits = 4)
e0_home_as <- round(((e0_home_gs$x/e0_home_games))/e0_avg_HG, digits = 4)
e1_home_as <- round(((e1_home_gs$x/e1_home_games))/e1_avg_HG, digits = 4)
e2_home_as <- round(((e2_home_gs$x/e2_home_games))/e2_avg_HG, digits = 4)
e3_home_as <- round(((e3_home_gs$x/e3_home_games))/e3_avg_HG, digits = 4)
ec_home_as <- round(((ec_home_gs$x/ec_home_games))/ec_avg_HG, digits = 4)
f1_home_as <- round(((f1_home_gs$x/f1_home_games))/f1_avg_HG, digits = 4)
f2_home_as <- round(((f2_home_gs$x/f2_home_games))/f2_avg_HG, digits = 4)
g1_home_as <- round(((g1_home_gs$x/g1_home_games))/g1_avg_HG, digits = 4)
i1_home_as <- round(((i1_home_gs$x/i1_home_games))/i1_avg_HG, digits = 4)
i2_home_as <- round(((i2_home_gs$x/i2_home_games))/i2_avg_HG, digits = 4)
n1_home_as <- round(((n1_home_gs$x/n1_home_games))/n1_avg_HG, digits = 4)
p1_home_as <- round(((p1_home_gs$x/p1_home_games))/p1_avg_HG, digits = 4)
sc0_home_as <- round(((sc0_home_gs$x/sc0_home_games))/sc0_avg_HG, digits = 4)
sc1_home_as <- round(((sc1_home_gs$x/sc1_home_games))/sc1_avg_HG, digits = 4)
sc2_home_as <- round(((sc2_home_gs$x/sc2_home_games))/sc2_avg_HG, digits = 4)
sc3_home_as <- round(((sc3_home_gs$x/sc3_home_games))/sc3_avg_HG, digits = 4)
sp1_home_as <- round(((sp1_home_gs$x/sp1_home_games))/sp1_avg_HG, digits = 4)
sp2_home_as <- round(((sp2_home_gs$x/sp2_home_games))/sp2_avg_HG, digits = 4)
t1_home_as <- round(((t1_home_gs$x/t1_home_games))/t1_avg_HG, digits = 4)
#calculate away attack strength
b1_away_as <- round(((b1_away_gs$x/b1_away_games))/b1_avg_AG, digits = 4)
d1_away_as <- round(((d1_away_gs$x/d1_away_games))/d1_avg_AG, digits = 4)
d2_away_as <- round(((d2_away_gs$x/d2_away_games))/d2_avg_AG, digits = 4)
e0_away_as <- round(((e0_away_gs$x/e0_away_games))/e0_avg_AG, digits = 4)
e1_away_as <- round(((e1_away_gs$x/e1_away_games))/e1_avg_AG, digits = 4)
e2_away_as <- round(((e2_away_gs$x/e2_away_games))/e2_avg_AG, digits = 4)
e3_away_as <- round(((e3_away_gs$x/e3_away_games))/e3_avg_AG, digits = 4)
ec_away_as <- round(((ec_away_gs$x/ec_away_games))/ec_avg_AG, digits = 4)
f1_away_as <- round(((f1_away_gs$x/f1_away_games))/f1_avg_AG, digits = 4)
f2_away_as <- round(((f2_away_gs$x/f2_away_games))/f2_avg_AG, digits = 4)
g1_away_as <- round(((g1_away_gs$x/g1_away_games))/g1_avg_AG, digits = 4)
i1_away_as <- round(((i1_away_gs$x/i1_away_games))/i1_avg_AG, digits = 4)
i2_away_as <- round(((i2_away_gs$x/i2_away_games))/i2_avg_AG, digits = 4)
n1_away_as <- round(((n1_away_gs$x/n1_away_games))/n1_avg_AG, digits = 4)
p1_away_as <- round(((p1_away_gs$x/p1_away_games))/p1_avg_AG, digits = 4)
sc0_away_as <- round(((sc0_away_gs$x/sc0_away_games))/sc0_avg_AG, digits = 4)
sc1_away_as <- round(((sc1_away_gs$x/sc1_away_games))/sc1_avg_AG, digits = 4)
sc2_away_as <- round(((sc2_away_gs$x/sc2_away_games))/sc2_avg_AG, digits = 4)
sc3_away_as <- round(((sc3_away_gs$x/sc3_away_games))/sc3_avg_AG, digits = 4)
sp1_away_as <- round(((sp1_away_gs$x/sp1_away_games))/sp1_avg_AG, digits = 4)
sp2_away_as <- round(((sp2_away_gs$x/sp2_away_games))/sp2_avg_AG, digits = 4)
t1_away_as <- round(((t1_away_gs$x/t1_away_games))/t1_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
b1_avg_HC <- round(b1_T_AG /b1_GP, digits = 4)
d1_avg_HC <- round(d1_T_AG /d1_GP, digits = 4)
d2_avg_HC <- round(d2_T_AG /d2_GP, digits = 4)
e0_avg_HC <- round(e0_T_AG /e0_GP, digits = 4)
e1_avg_HC <- round(e1_T_AG /e1_GP, digits = 4)
e2_avg_HC <- round(e2_T_AG /e2_GP, digits = 4)
e3_avg_HC <- round(e3_T_AG /e3_GP, digits = 4)
ec_avg_HC <- round(ec_T_AG /ec_GP, digits = 4)
f1_avg_HC <- round(f1_T_AG /f1_GP, digits = 4)
f2_avg_HC <- round(f2_T_AG /f2_GP, digits = 4)
g1_avg_HC <- round(g1_T_AG /g1_GP, digits = 4)
i1_avg_HC <- round(i1_T_AG /i1_GP, digits = 4)
i2_avg_HC <- round(i2_T_AG /i2_GP, digits = 4)
n1_avg_HC <- round(n1_T_AG /n1_GP, digits = 4)
p1_avg_HC <- round(p1_T_AG /p1_GP, digits = 4)
sc0_avg_HC <- round(sc0_T_AG /sc0_GP, digits = 4)
sc1_avg_HC <- round(sc1_T_AG /sc1_GP, digits = 4)
sc2_avg_HC <- round(sc2_T_AG /sc2_GP, digits = 4)
sc3_avg_HC <- round(sc3_T_AG /sc3_GP, digits = 4)
sp1_avg_HC <- round(sp1_T_AG /sp1_GP, digits = 4)
sp2_avg_HC <- round(sp2_T_AG /sp2_GP, digits = 4)
t1_avg_HC <- round(t1_T_AG /t1_GP, digits = 4)
#avg away concede
b1_avg_AC <- round(b1_T_HG /b1_GP, digits = 4)
d1_avg_AC <- round(d1_T_HG /d1_GP, digits = 4)
d2_avg_AC <- round(d2_T_HG /d2_GP, digits = 4)
e0_avg_AC <- round(e0_T_HG /e0_GP, digits = 4)
e1_avg_AC <- round(e1_T_HG /e1_GP, digits = 4)
e2_avg_AC <- round(e2_T_HG /e2_GP, digits = 4)
e3_avg_AC <- round(e3_T_HG /e3_GP, digits = 4)
ec_avg_AC <- round(ec_T_HG /ec_GP, digits = 4)
f1_avg_AC <- round(f1_T_HG /f1_GP, digits = 4)
f2_avg_AC <- round(f2_T_HG /f2_GP, digits = 4)
g1_avg_AC <- round(g1_T_HG /g1_GP, digits = 4)
i1_avg_AC <- round(i1_T_HG /i1_GP, digits = 4)
i2_avg_AC <- round(i2_T_HG /i2_GP, digits = 4)
n1_avg_AC <- round(n1_T_HG /n1_GP, digits = 4)
p1_avg_AC <- round(p1_T_HG /p1_GP, digits = 4)
sc0_avg_AC <- round(sc0_T_HG /sc0_GP, digits = 4)
sc1_avg_AC <- round(sc1_T_HG /sc1_GP, digits = 4)
sc2_avg_AC <- round(sc2_T_HG /sc2_GP, digits = 4)
sc3_avg_AC <- round(sc3_T_HG /sc3_GP, digits = 4)
sp1_avg_AC <- round(sp1_T_HG /sp1_GP, digits = 4)
sp2_avg_AC <- round(sp2_T_HG /sp2_GP, digits = 4)
t1_avg_AC <- round(t1_T_HG /t1_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
b1_home_ds <- round(((b1_home_gc$x/b1_home_games))/b1_avg_HC, digits = 4)
d1_home_ds <- round(((d1_home_gc$x/d1_home_games))/d1_avg_HC, digits = 4)
d2_home_ds <- round(((d2_home_gc$x/d2_home_games))/d2_avg_HC, digits = 4)
e0_home_ds <- round(((e0_home_gc$x/e0_home_games))/e0_avg_HC, digits = 4)
e1_home_ds <- round(((e1_home_gc$x/e1_home_games))/e1_avg_HC, digits = 4)
e2_home_ds <- round(((e2_home_gc$x/e2_home_games))/e2_avg_HC, digits = 4)
e3_home_ds <- round(((e3_home_gc$x/e3_home_games))/e3_avg_HC, digits = 4)
ec_home_ds <- round(((ec_home_gc$x/ec_home_games))/ec_avg_HC, digits = 4)
f1_home_ds <- round(((f1_home_gc$x/f1_home_games))/f1_avg_HC, digits = 4)
f2_home_ds <- round(((f2_home_gc$x/f2_home_games))/f2_avg_HC, digits = 4)
g1_home_ds <- round(((g1_home_gc$x/g1_home_games))/g1_avg_HC, digits = 4)
i1_home_ds <- round(((i1_home_gc$x/i1_home_games))/i1_avg_HC, digits = 4)
i2_home_ds <- round(((i2_home_gc$x/i2_home_games))/i2_avg_HC, digits = 4)
n1_home_ds <- round(((n1_home_gc$x/n1_home_games))/n1_avg_HC, digits = 4)
p1_home_ds <- round(((p1_home_gc$x/p1_home_games))/p1_avg_HC, digits = 4)
sc0_home_ds <- round(((sc0_home_gc$x/sc0_home_games))/sc0_avg_HC, digits = 4)
sc1_home_ds <- round(((sc1_home_gc$x/sc1_home_games))/sc1_avg_HC, digits = 4)
sc2_home_ds <- round(((sc2_home_gc$x/sc2_home_games))/sc2_avg_HC, digits = 4)
sc3_home_ds <- round(((sc3_home_gc$x/sc3_home_games))/sc3_avg_HC, digits = 4)
sp1_home_ds <- round(((sp1_home_gc$x/sp1_home_games))/sp1_avg_HC, digits = 4)
sp2_home_ds <- round(((sp2_home_gc$x/sp2_home_games))/sp2_avg_HC, digits = 4)
t1_home_ds <- round(((t1_home_gc$x/t1_home_games))/t1_avg_HC, digits = 4)
#away defense strength
b1_away_ds <- round(((b1_away_gc$x/b1_away_games))/b1_avg_AC, digits = 4)
d1_away_ds <- round(((d1_away_gc$x/d1_away_games))/d1_avg_AC, digits = 4)
d2_away_ds <- round(((d2_away_gc$x/d2_away_games))/d2_avg_AC, digits = 4)
e0_away_ds <- round(((e0_away_gc$x/e0_away_games))/e0_avg_AC, digits = 4)
e1_away_ds <- round(((e1_away_gc$x/e1_away_games))/e1_avg_AC, digits = 4)
e2_away_ds <- round(((e2_away_gc$x/e2_away_games))/e2_avg_AC, digits = 4)
e3_away_ds <- round(((e3_away_gc$x/e3_away_games))/e3_avg_AC, digits = 4)
ec_away_ds <- round(((ec_away_gc$x/ec_away_games))/ec_avg_AC, digits = 4)
f1_away_ds <- round(((f1_away_gc$x/f1_away_games))/f1_avg_AC, digits = 4)
f2_away_ds <- round(((f2_away_gc$x/f2_away_games))/f2_avg_AC, digits = 4)
g1_away_ds <- round(((g1_away_gc$x/g1_away_games))/g1_avg_AC, digits = 4)
i1_away_ds <- round(((i1_away_gc$x/i1_away_games))/i1_avg_AC, digits = 4)
i2_away_ds <- round(((i2_away_gc$x/i2_away_games))/i2_avg_AC, digits = 4)
n1_away_ds <- round(((n1_away_gc$x/n1_away_games))/n1_avg_AC, digits = 4)
p1_away_ds <- round(((p1_away_gc$x/p1_away_games))/p1_avg_AC, digits = 4)
sc0_away_ds <- round(((sc0_away_gc$x/sc0_away_games))/sc0_avg_AC, digits = 4)
sc1_away_ds <- round(((sc1_away_gc$x/sc1_away_games))/sc1_avg_AC, digits = 4)
sc2_away_ds <- round(((sc2_away_gc$x/sc2_away_games))/sc2_avg_AC, digits = 4)
sc3_away_ds <- round(((sc3_away_gc$x/sc3_away_games))/sc3_avg_AC, digits = 4)
sp1_away_ds <- round(((sp1_away_gc$x/sp1_away_games))/sp1_avg_AC, digits = 4)
sp2_away_ds <- round(((sp2_away_gc$x/sp2_away_games))/sp2_avg_AC, digits = 4)
t1_away_ds <- round(((t1_away_gc$x/t1_away_games))/t1_avg_AC, digits = 4)
#############################################################################
#home poisson data
#b1
b1_division <- c()
b1_division[1:length(b1_teams)] <- "B1"
b1_home_poisson <- cbind(b1_division,b1_teams,b1_avg_HG,b1_home_as,b1_home_ds)
#d1
d1_division <- c()
d1_division[1:length(d1_teams)] <- "D1"
d1_home_poisson <- cbind(d1_division,d1_teams,d1_avg_HG,d1_home_as,d1_home_ds)
#d2
d2_division <- c()
d2_division[1:length(d2_teams)] <- "D2"
d2_home_poisson <- cbind(d2_division,d2_teams,d2_avg_HG,d2_home_as,d2_home_ds)
#e0
e0_division <- c()
e0_division[1:length(e0_teams)] <- "E0"
e0_home_poisson <- cbind(e0_division,e0_teams,e0_avg_HG,e0_home_as,e0_home_ds)
#e1
e1_division <- c()
e1_division[1:length(e1_teams)] <- "E1"
e1_home_poisson <- cbind(e1_division,e1_teams,e1_avg_HG,e1_home_as,e1_home_ds)
#e2
e2_division <- c()
e2_division[1:length(e2_teams)] <- "E2"
e2_home_poisson <- cbind(e2_division,e2_avg_HG,e2_teams,e2_home_as,e2_home_ds)
#e3
e3_division <- c()
e3_division[1:length(e3_teams)] <- "E3"
e3_home_poisson <- cbind(e3_division,e3_teams,e3_avg_HG,e3_home_as,e3_home_ds)
#ec
ec_division <- c()
ec_division[1:length(ec_teams)] <- "EC"
ec_home_poisson <- cbind(ec_division,ec_teams,ec_avg_HG,ec_home_as,ec_home_ds)
#f1
f1_division <- c()
f1_division[1:length(f1_teams)] <- "F1"
f1_home_poisson <- cbind(f1_division,f1_teams,f1_avg_HG,f1_home_as,f1_home_ds)
#f2
f2_division <- c()
f2_division[1:length(f2_teams)] <- "F2"
f2_home_poisson <- cbind(f2_division,f2_teams,f2_avg_HG,f2_home_as,f2_home_ds)
#g1
g1_division <- c()
g1_division[1:length(g1_teams)] <- "G1"
g1_home_poisson <- cbind(g1_division,g1_teams,g1_avg_HG,g1_home_as,g1_home_ds)
#i1
i1_division <- c()
i1_division[1:length(i1_teams)] <- "I1"
i1_home_poisson <- cbind(i1_division,i1_teams,i1_avg_HG,i1_home_as,i1_home_ds)
#i2
i2_division <- c()
i2_division[1:length(i2_teams)] <- "I2"
i2_home_poisson <- cbind(i2_division,i2_teams,i2_avg_HG,i2_home_as,i2_home_ds)
#n1
n1_division <- c()
n1_division[1:length(n1_teams)] <- "N1"
n1_home_poisson <- cbind(n1_division,n1_teams,n1_avg_HG,n1_home_as,n1_home_ds)
#p1
p1_division <- c()
p1_division[1:length(p1_teams)] <- "P1"
p1_home_poisson <- cbind(p1_division,p1_teams,p1_avg_HG,p1_home_as,p1_home_ds)
#sc0
sc0_division <- c()
sc0_division[1:length(sc0_teams)] <- "SC0"
sc0_home_poisson <- cbind(sc0_division,sc0_teams,sc0_avg_HG,sc0_home_as,sc0_home_ds)
#sc1
sc1_division <- c()
sc1_division[1:length(sc1_teams)] <- "SC1"
sc1_home_poisson <- cbind(sc1_division,sc1_teams,sc1_avg_HG,sc1_home_as,sc1_home_ds)
#sc2
sc2_division <- c()
sc2_division[1:length(sc2_teams)] <- "SC2"
sc2_home_poisson <- cbind(sc2_division,sc2_teams,sc2_avg_HG,sc2_home_as,sc2_home_ds)
#sc3
sc3_division <- c()
sc3_division[1:length(sc3_teams)] <- "SC3"
sc3_home_poisson <- cbind(sc3_division,sc3_teams,sc3_avg_HG,sc3_home_as,sc3_home_ds)
#sp1
sp1_division <- c()
sp1_division[1:length(sp1_teams)] <- "SP1"
sp1_home_poisson <- cbind(sp1_division,sp1_teams,sp1_avg_HG,sp1_home_as,sp1_home_ds)
#sp2
sp2_division <- c()
sp2_division[1:length(sp2_teams)] <- "SP2"
sp2_home_poisson <- cbind(sp2_division,sp2_teams,sp2_avg_HG,sp2_home_as,sp2_home_ds)
#t1
t1_division <- c()
t1_division[1:length(t1_teams)] <- "T1"
t1_home_poisson <- cbind(t1_division,t1_teams,t1_avg_HG,t1_home_as,t1_home_ds)
#################################################################################
#away poisson data
#b1
b1_division <- c()
b1_division[1:length(b1_teams)] <- "B1"
b1_away_poisson <- cbind(b1_division,b1_teams,b1_avg_AG,b1_away_as,b1_away_ds)
#d1
d1_division <- c()
d1_division[1:length(d1_teams)] <- "D1"
d1_away_poisson <- cbind(d1_division,d1_teams,d1_avg_AG,d1_away_as,d1_away_ds)
#d2
d2_division <- c()
d2_division[1:length(d2_teams)] <- "D2"
d2_away_poisson <- cbind(d2_division,d2_teams,d2_avg_AG,d2_away_as,d2_away_ds)
#e0
e0_division <- c()
e0_division[1:length(e0_teams)] <- "E0"
e0_away_poisson <- cbind(e0_division,e0_teams,e0_avg_AG,e0_away_as,e0_away_ds)
#e1
e1_division <- c()
e1_division[1:length(e1_teams)] <- "E1"
e1_away_poisson <- cbind(e1_division,e1_teams,e1_avg_AG,e1_away_as,e1_away_ds)
#e2
e2_division <- c()
e2_division[1:length(e2_teams)] <- "E2"
e2_away_poisson <- cbind(e2_division,e2_avg_AG,e2_teams,e2_away_as,e2_away_ds)
#e3
e3_division <- c()
e3_division[1:length(e3_teams)] <- "E3"
e3_away_poisson <- cbind(e3_division,e3_teams,e3_avg_AG,e3_away_as,e3_away_ds)
#ec
ec_division <- c()
ec_division[1:length(ec_teams)] <- "EC"
ec_away_poisson <- cbind(ec_division,ec_teams,ec_avg_AG,ec_away_as,ec_away_ds)
#f1
f1_division <- c()
f1_division[1:length(f1_teams)] <- "F1"
f1_away_poisson <- cbind(f1_division,f1_teams,f1_avg_AG,f1_away_as,f1_away_ds)
#f2
f2_division <- c()
f2_division[1:length(f2_teams)] <- "F2"
f2_away_poisson <- cbind(f2_division,f2_teams,f2_avg_AG,f2_away_as,f2_away_ds)
#g1
g1_division <- c()
g1_division[1:length(g1_teams)] <- "G1"
g1_away_poisson <- cbind(g1_division,g1_teams,g1_avg_AG,g1_away_as,g1_away_ds)
#i1
i1_division <- c()
i1_division[1:length(i1_teams)] <- "I1"
i1_away_poisson <- cbind(i1_division,i1_teams,i1_avg_AG,i1_away_as,i1_away_ds)
#i2
i2_division <- c()
i2_division[1:length(i2_teams)] <- "I2"
i2_away_poisson <- cbind(i2_division,i2_teams,i2_avg_AG,i2_away_as,i2_away_ds)
#n1
n1_division <- c()
n1_division[1:length(n1_teams)] <- "N1"
n1_away_poisson <- cbind(n1_division,n1_teams,n1_avg_AG,n1_away_as,n1_away_ds)
#p1
p1_division <- c()
p1_division[1:length(p1_teams)] <- "P1"
p1_away_poisson <- cbind(p1_division,p1_teams,p1_avg_AG,p1_away_as,p1_away_ds)
#sc0
sc0_division <- c()
sc0_division[1:length(sc0_teams)] <- "SC0"
sc0_away_poisson <- cbind(sc0_division,sc0_teams,sc0_avg_AG,sc0_away_as,sc0_away_ds)
#sc1
sc1_division <- c()
sc1_division[1:length(sc1_teams)] <- "SC1"
sc1_away_poisson <- cbind(sc1_division,sc1_teams,sc1_avg_AG,sc1_away_as,sc1_away_ds)
#sc2
sc2_division <- c()
sc2_division[1:length(sc2_teams)] <- "SC2"
sc2_away_poisson <- cbind(sc2_division,sc2_teams,sc2_avg_AG,sc2_away_as,sc2_away_ds)
#sc3
sc3_division <- c()
sc3_division[1:length(sc3_teams)] <- "SC3"
sc3_away_poisson <- cbind(sc3_division,sc3_teams,sc3_avg_AG,sc3_away_as,sc3_away_ds)
#sp1
sp1_division <- c()
sp1_division[1:length(sp1_teams)] <- "SP1"
sp1_away_poisson <- cbind(sp1_division,sp1_teams,sp1_avg_AG,sp1_away_as,sp1_away_ds)
#sp2
sp2_division <- c()
sp2_division[1:length(sp2_teams)] <- "SP2"
sp2_away_poisson <- cbind(sp2_division,sp2_teams,sp2_avg_AG,sp2_away_as,sp2_away_ds)
#t1
t1_division <- c()
t1_division[1:length(t1_teams)] <- "T1"
t1_away_poisson <- cbind(t1_division,t1_teams,t1_avg_AG,t1_away_as,t1_away_ds)
#create home and away csv
home_poisson <- rbind(b1_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
away_poisson <- rbind(b1_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#delete current
unlink("R_home.csv")
unlink("R_away.csv")
#write another one
write.csv(home_poisson,'R_home.csv')
write.csv(away_poisson,'R_away.csv')
