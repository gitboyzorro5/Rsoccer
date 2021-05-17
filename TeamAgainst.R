#Create Home and Away teams against
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
#create matrices
#B1
b1_form_team_against_h <- tapply(B1$AwayTeam, B1[c("HomeTeam", "Date")],median)
b1_form_team_against_a <- tapply(B1$HomeTeam, B1[c("AwayTeam", "Date")],median)
#D1
d1_form_team_against_h <- tapply(D1$AwayTeam, D1[c("HomeTeam", "Date")],median)
d1_form_team_against_a <- tapply(D1$HomeTeam, D1[c("AwayTeam", "Date")],median)
#D2
d2_form_team_against_h <- tapply(D2$AwayTeam, D2[c("HomeTeam", "Date")],median)
d2_form_team_against_a <- tapply(D2$HomeTeam, D2[c("AwayTeam", "Date")],median)
#E0
e0_form_team_against_h <- tapply(E0$AwayTeam, E0[c("HomeTeam", "Date")],median)
e0_form_team_against_a <- tapply(E0$HomeTeam, E0[c("AwayTeam", "Date")],median)
#E1
e1_form_team_against_h <- tapply(E1$AwayTeam, E1[c("HomeTeam", "Date")],median)
e1_form_team_against_a <- tapply(E1$HomeTeam, E1[c("AwayTeam", "Date")],median)
#E2
e2_form_team_against_h <- tapply(E2$AwayTeam, E2[c("HomeTeam", "Date")],median)
e2_form_team_against_a <- tapply(E2$HomeTeam, E2[c("AwayTeam", "Date")],median)
#E3
e3_form_team_against_h <- tapply(E3$AwayTeam, E3[c("HomeTeam", "Date")],median)
e3_form_team_against_a <- tapply(E3$HomeTeam, E3[c("AwayTeam", "Date")],median)
#EC
ec_form_team_against_h <- tapply(EC$AwayTeam, EC[c("HomeTeam", "Date")],median)
ec_form_team_against_a <- tapply(EC$HomeTeam, EC[c("AwayTeam", "Date")],median)
#F1
f1_form_team_against_h <- tapply(F1$AwayTeam, F1[c("HomeTeam", "Date")],median)
f1_form_team_against_a <- tapply(F1$HomeTeam, F1[c("AwayTeam", "Date")],median)
#F2
f2_form_team_against_h <- tapply(F2$AwayTeam, F2[c("HomeTeam", "Date")],median)
f2_form_team_against_a <- tapply(F2$HomeTeam, F2[c("AwayTeam", "Date")],median)
#G1
g1_form_team_against_h <- tapply(G1$AwayTeam, G1[c("HomeTeam", "Date")],median)
g1_form_team_against_a <- tapply(G1$HomeTeam, G1[c("AwayTeam", "Date")],median)
#I1
i1_form_team_against_h <- tapply(I1$AwayTeam, I1[c("HomeTeam", "Date")],median)
i1_form_team_against_a <- tapply(I1$HomeTeam, I1[c("AwayTeam", "Date")],median)
#I2
i2_form_team_against_h <- tapply(I2$AwayTeam, I2[c("HomeTeam", "Date")],median)
i2_form_team_against_a <- tapply(I2$HomeTeam, I2[c("AwayTeam", "Date")],median)
#N1
n1_form_team_against_h <- tapply(N1$AwayTeam, N1[c("HomeTeam", "Date")],median)
n1_form_team_against_a <- tapply(N1$HomeTeam, N1[c("AwayTeam", "Date")],median)
#P1
p1_form_team_against_h <- tapply(P1$AwayTeam, P1[c("HomeTeam", "Date")],median)
p1_form_team_against_a <- tapply(P1$HomeTeam, P1[c("AwayTeam", "Date")],median)
#SC0
sc0_form_team_against_h <- tapply(SC0$AwayTeam, SC0[c("HomeTeam", "Date")],median)
sc0_form_team_against_a <- tapply(SC0$HomeTeam, SC0[c("AwayTeam", "Date")],median)
#SC1
sc1_form_team_against_h <- tapply(SC1$AwayTeam, SC1[c("HomeTeam", "Date")],median)
sc1_form_team_against_a <- tapply(SC1$HomeTeam, SC1[c("AwayTeam", "Date")],median)
#SC2
sc2_form_team_against_h <- tapply(SC2$AwayTeam, SC2[c("HomeTeam", "Date")],median)
sc2_form_team_against_a <- tapply(SC2$HomeTeam, SC2[c("AwayTeam", "Date")],median)
#SC3
sc3_form_team_against_h <- tapply(SC3$AwayTeam, SC3[c("HomeTeam", "Date")],median)
sc3_form_team_against_a <- tapply(SC3$HomeTeam, SC3[c("AwayTeam", "Date")],median)
#SP1
sp1_form_team_against_h <- tapply(SP1$AwayTeam, SP1[c("HomeTeam", "Date")],median)
sp1_form_team_against_a <- tapply(SP1$HomeTeam, SP1[c("AwayTeam", "Date")],median)
#SP2
sp2_form_team_against_h <- tapply(SP2$AwayTeam, SP2[c("HomeTeam", "Date")],median)
sp2_form_team_against_a <- tapply(SP2$HomeTeam, SP2[c("AwayTeam", "Date")],median)
#T1
t1_form_team_against_h <- tapply(T1$AwayTeam, T1[c("HomeTeam", "Date")],median)
t1_form_team_against_a <- tapply(T1$HomeTeam, T1[c("AwayTeam", "Date")],median)
