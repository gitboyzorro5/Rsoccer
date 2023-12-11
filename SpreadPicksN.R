spreadn <- 6
library('xlsx')
#D1
D1_spread <- subset(readxl::read_excel('UCL20232024.xlsx'), Div == "D1")
D1_spread <- D1_spread[,-1]
unlink('SpreadsN/D1/*')
for(d1_sn in 1:17){
  df <- tail(D1_spread[D1_spread$HomeTeam == final_doublefixture_d1[d1_sn,1] | D1_spread$AwayTeam == final_doublefixture_d1[d1_sn,1] ,],spreadn)
  df2 <- tail(D1_spread[D1_spread$HomeTeam == final_doublefixture_d1[d1_sn + 1,1] | D1_spread$AwayTeam == final_doublefixture_d1[d1_sn + 1,1],],spreadn)
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\D1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_d1[d1_sn,1],final_doublefixture_d1[d1_sn + 1,1],".csv",sep = "_")))

}

#E0
E0_spread <- subset(readxl::read_excel('UCL20232024.xlsx'), Div == "E0")
E0_spread <- E0_spread[,-1]
unlink('SpreadsN/E0/*')
for(e0_sn in 1:19){
  df <- tail(E0_spread[E0_spread$HomeTeam == final_doublefixture_e0[e0_sn,1] | E0_spread$AwayTeam == final_doublefixture_e0[e0_sn,1] ,],spreadn)

  df2 <- tail(E0_spread[E0_spread$HomeTeam == final_doublefixture_e0[e0_sn + 1,1] | E0_spread$AwayTeam == final_doublefixture_e0[e0_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\E0"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e0[e0_sn,1],final_doublefixture_e0[e0_sn + 1,1],".csv",sep = "_")))

}

#I1
I1_spread <- subset(readxl::read_excel('UCL20232024.xlsx'), Div == "I1")
I1_spread <- I1_spread[,-1]
unlink('SpreadsN/I1/*')
for(i1_sn in 1:19){
  df <- tail(I1_spread[I1_spread$HomeTeam == final_doublefixture_i1[i1_sn,1] | I1_spread$AwayTeam == final_doublefixture_i1[i1_sn,1],],spreadn)
  df2 <- tail(I1_spread[I1_spread$HomeTeam == final_doublefixture_i1[i1_sn + 1,1] | I1_spread$AwayTeam == final_doublefixture_i1[i1_sn + 1,1],],spreadn)
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\I1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_i1[i1_sn,1],final_doublefixture_i1[i1_sn + 1,1],".csv",sep = "_")))

}


#SP1
SP1_spread <- subset(readxl::read_excel('UCL20232024.xlsx'), Div == "SP1")
SP1_spread <- SP1_spread[,-1]
unlink('SpreadsN/SP1/*')
for(sp1_sn in 1:19){
  df <- tail(SP1_spread[SP1_spread$HomeTeam == final_doublefixture_sp1[sp1_sn,1] | SP1_spread$AwayTeam == final_doublefixture_sp1[sp1_sn,1] ,],spreadn)
  df2 <- tail(SP1_spread[SP1_spread$HomeTeam == final_doublefixture_sp1[sp1_sn + 1,1] | SP1_spread$AwayTeam == final_doublefixture_sp1[sp1_sn + 1,1],],spreadn)
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\SP1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sp1[sp1_sn,1],final_doublefixture_sp1[sp1_sn + 1,1],".csv",sep = "_")))

}

#F1
F1_spread <- subset(readxl::read_excel('UCL20232024.xlsx'), Div == "F1")
F1_spread <- F1_spread[,-1]
unlink('SpreadsN/F1/*')
for(f1_sn in 1:17){
  df <- tail(F1_spread[F1_spread$HomeTeam == final_doublefixture_f1[f1_sn,1] | F1_spread$AwayTeam == final_doublefixture_f1[f1_sn,1] ,],spreadn)
  df2 <- tail(F1_spread[F1_spread$HomeTeam == final_doublefixture_f1[f1_sn + 1,1] | F1_spread$AwayTeam == final_doublefixture_f1[f1_sn + 1,1],],spreadn)
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\F1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_f1[f1_sn,1],final_doublefixture_f1[f1_sn + 1,1],".csv",sep = "_")))

}

####################################################################################################################################################
####################################################################################################################################################
####Other Leagues #############################################################################Other leagues########################################
#B1
B1_spread <- subset(allteams20232024,Div =="B1")
B1_spread$n <- B1_spread$TG * 1
B1_spread$Bookings <- (B1_spread$HY *10 + B1_spread$HR *25) + (B1_spread$AY*10 + B1_spread$AR*25)
B1_spread$Crossbookings <- (B1_spread$HY *10 + B1_spread$HR *25)*(B1_spread$AY*10 + B1_spread$AR*25)
B1_spread$GoalsXbookings <- (B1_spread$Bookings)*(B1_spread$TG)
B1_spread$CornersXbookings <- (B1_spread$TC)*(B1_spread$Bookings)
B1_spread$GoalsXcorners <- (B1_spread$TG)*(B1_spread$TC)
B1_spread$GoalsXcornerXbookings <- (B1_spread$TG)*(B1_spread$TC)*(B1_spread$Bookings)

unlink('SpreadsN/B1/*')
for(b1_sn in 1:17){
  df <- tail(B1_spread[B1_spread$HomeTeam == final_doublefixture_b1[b1_sn,1] | B1_spread$AwayTeam == final_doublefixture_b1[b1_sn,1] ,],spreadn)

  df2 <- tail(B1_spread[B1_spread$HomeTeam == final_doublefixture_b1[b1_sn + 1,1] | B1_spread$AwayTeam == final_doublefixture_b1[b1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\B1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_b1[b1_sn,1],final_doublefixture_b1[b1_sn + 1,1],".csv",sep = "_")))

}

###############################################################################################################
#D2
D2_spread <- subset(allteams20232024,Div =="D2")
D2_spread$n <- D2_spread$TG * 1
D2_spread$Bookings <- (D2_spread$HY *10 + D2_spread$HR *25) + (D2_spread$AY*10 + D2_spread$AR*25)
D2_spread$Crossbookings <- (D2_spread$HY *10 + D2_spread$HR *25)*(D2_spread$AY*10 + D2_spread$AR*25)
D2_spread$GoalsXbookings <- (D2_spread$Bookings)*(D2_spread$TG)
D2_spread$CornersXbookings <- (D2_spread$TC)*(D2_spread$Bookings)
D2_spread$GoalsXcorners <- (D2_spread$TG)*(D2_spread$TC)
D2_spread$GoalsXcornerXbookings <- (D2_spread$TG)*(D2_spread$TC)*(D2_spread$Bookings)

unlink('SpreadsN/D2/*')
for(d2_sn in 1:17){
  df <- tail(D2_spread[D2_spread$HomeTeam == final_doublefixture_d2[d2_sn,1] | D2_spread$AwayTeam == final_doublefixture_d2[d2_sn,1] ,],spreadn)

  df2 <- tail(D2_spread[D2_spread$HomeTeam == final_doublefixture_d2[d2_sn + 1,1] | D2_spread$AwayTeam == final_doublefixture_d2[d2_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\D2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_d2[d2_sn,1],final_doublefixture_d2[d2_sn + 1,1],".csv",sep = "_")))

}

###############################################################################################################
E1_spread <- subset(allteams20232024,Div =="E1")
E1_spread$n <- E1_spread$TG * 1
E1_spread$Bookings <- (E1_spread$HY *10 + E1_spread$HR *25) + (E1_spread$AY*10 + E1_spread$AR*25)
E1_spread$Crossbookings <- (E1_spread$HY *10 + E1_spread$HR *25)*(E1_spread$AY*10 + E1_spread$AR*25)
E1_spread$GoalsXbookings <- (E1_spread$Bookings)*(E1_spread$TG)
E1_spread$CornersXbookings <- (E1_spread$TC)*(E1_spread$Bookings)
E1_spread$GoalsXcorners <- (E1_spread$TG)*(E1_spread$TC)
E1_spread$GoalsXcornerXbookings <- (E1_spread$TG)*(E1_spread$TC)*(E1_spread$Bookings)

unlink('SpreadsN/E1/*')
for(e1_sn in 1:23){
  df <- tail(E1_spread[E1_spread$HomeTeam == final_doublefixture_e1[e1_sn,1] | E1_spread$AwayTeam == final_doublefixture_e1[e1_sn,1] ,],spreadn)

  df2 <- tail(E1_spread[E1_spread$HomeTeam == final_doublefixture_e1[e1_sn + 1,1] | E1_spread$AwayTeam == final_doublefixture_e1[e1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\E1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e1[e1_sn,1],final_doublefixture_e1[e1_sn + 1,1],".csv",sep = "_")))

}
# #umiversal single
# df <- tail(B1_spread[B1_spread$HomeTeam == "Standard" | B1_spread$AwayTeam == "Standard" ,],spreadn)
#
# df2 <- tail(B1_spread[B1_spread$HomeTeam == "St. Gilloise" | B1_spread$AwayTeam == "St. Gilloise",],spreadn)
#
# temp_analysis <- rbind(df,df2)
#
# temp_analysis <- as.data.frame(temp_analysis)
# temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
# temp_sliced <- tail(temp_analysis,1)
# temp_sliced <- temp_sliced[1:36]
#
# temp_analyis_combined <- c(temp_sliced,temp_colmeans)
# temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
# write.csv(temp_analysis,'Temp/standardgilloise.csv')

#############################################################################################################################################
E2_spread <- subset(allteams20232024,Div =="E2")
E2_spread$n <- E2_spread$TG * 1
E2_spread$Bookings <- (E2_spread$HY *10 + E2_spread$HR *25) + (E2_spread$AY*10 + E2_spread$AR*25)
E2_spread$Crossbookings <- (E2_spread$HY *10 + E2_spread$HR *25)*(E2_spread$AY*10 + E2_spread$AR*25)
E2_spread$GoalsXbookings <- (E2_spread$Bookings)*(E2_spread$TG)
E2_spread$CornersXbookings <- (E2_spread$TC)*(E2_spread$Bookings)
E2_spread$GoalsXcorners <- (E2_spread$TG)*(E2_spread$TC)
E2_spread$GoalsXcornerXbookings <- (E2_spread$TG)*(E2_spread$TC)*(E2_spread$Bookings)

unlink('SpreadsN/E2/*')
for(e2_sn in 1:23){
  df <- tail(E2_spread[E2_spread$HomeTeam == final_doublefixture_e2[e2_sn,1] | E2_spread$AwayTeam == final_doublefixture_e2[e2_sn,1] ,],spreadn)

  df2 <- tail(E2_spread[E2_spread$HomeTeam == final_doublefixture_e2[e2_sn + 1,1] | E2_spread$AwayTeam == final_doublefixture_e2[e2_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\E2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e2[e2_sn,1],final_doublefixture_e2[e2_sn + 1,1],".csv",sep = "_")))

}
#############################################################################################################################################
E3_spread <- subset(allteams20232024,Div =="E3")
E3_spread$n <- E3_spread$TG * 1
E3_spread$Bookings <- (E3_spread$HY *10 + E3_spread$HR *25) + (E3_spread$AY*10 + E3_spread$AR*25)
E3_spread$Crossbookings <- (E3_spread$HY *10 + E3_spread$HR *25)*(E3_spread$AY*10 + E3_spread$AR*25)
E3_spread$GoalsXbookings <- (E3_spread$Bookings)*(E3_spread$TG)
E3_spread$CornersXbookings <- (E3_spread$TC)*(E3_spread$Bookings)
E3_spread$GoalsXcorners <- (E3_spread$TG)*(E3_spread$TC)
E3_spread$GoalsXcornerXbookings <- (E3_spread$TG)*(E3_spread$TC)*(E3_spread$Bookings)

unlink('SpreadsN/E3/*')
for(e3_sn in 1:23){
  df <- tail(E3_spread[E3_spread$HomeTeam == final_doublefixture_e3[e3_sn,1] | E3_spread$AwayTeam == final_doublefixture_e3[e3_sn,1] ,],spreadn)

  df2 <- tail(E3_spread[E3_spread$HomeTeam == final_doublefixture_e3[e3_sn + 1,1] | E3_spread$AwayTeam == final_doublefixture_e3[e3_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\E3"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e3[e3_sn,1],final_doublefixture_e3[e3_sn + 1,1],".csv",sep = "_")))

}
########################################################################################################################################
EC_spread <- subset(allteams20232024,Div =="EC")
EC_spread$n <- EC_spread$TG * 1
EC_spread$Bookings <- (EC_spread$HY *10 + EC_spread$HR *25) + (EC_spread$AY*10 + EC_spread$AR*25)
EC_spread$Crossbookings <- (EC_spread$HY *10 + EC_spread$HR *25)*(EC_spread$AY*10 + EC_spread$AR*25)
EC_spread$GoalsXbookings <- (EC_spread$Bookings)*(EC_spread$TG)
EC_spread$CornersXbookings <- (EC_spread$TC)*(EC_spread$Bookings)
EC_spread$GoalsXcorners <- (EC_spread$TG)*(EC_spread$TC)
EC_spread$GoalsXcornerXbookings <- (EC_spread$TG)*(EC_spread$TC)*(EC_spread$Bookings)

unlink('SpreadsN/EC/*')
for(ec_sn in 1:23){
  df <- tail(EC_spread[EC_spread$HomeTeam == final_doublefixture_ec[ec_sn,1] | EC_spread$AwayTeam == final_doublefixture_ec[ec_sn,1] ,],spreadn)

  df2 <- tail(EC_spread[EC_spread$HomeTeam == final_doublefixture_ec[ec_sn + 1,1] | EC_spread$AwayTeam == final_doublefixture_ec[ec_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\EC"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_ec[ec_sn,1],final_doublefixture_ec[ec_sn + 1,1],".csv",sep = "_")))

}
########################################################################################################################################
F2_spread <- subset(allteams20232024,Div =="F2")
F2_spread$n <- F2_spread$TG * 1
F2_spread$Bookings <- (F2_spread$HY *10 + F2_spread$HR *25) + (F2_spread$AY*10 + F2_spread$AR*25)
F2_spread$Crossbookings <- (F2_spread$HY *10 + F2_spread$HR *25)*(F2_spread$AY*10 + F2_spread$AR*25)
F2_spread$GoalsXbookings <- (F2_spread$Bookings)*(F2_spread$TG)
F2_spread$CornersXbookings <- (F2_spread$TC)*(F2_spread$Bookings)
F2_spread$GoalsXcorners <- (F2_spread$TG)*(F2_spread$TC)
F2_spread$GoalsXcornerXbookings <- (F2_spread$TG)*(F2_spread$TC)*(F2_spread$Bookings)

unlink('SpreadsN/F2/*')
for(f2_sn in 1:19){
  df <- tail(F2_spread[F2_spread$HomeTeam == final_doublefixture_f2[f2_sn,1] | F2_spread$AwayTeam == final_doublefixture_f2[f2_sn,1] ,],spreadn)

  df2 <- tail(F2_spread[F2_spread$HomeTeam == final_doublefixture_f2[f2_sn + 1,1] | F2_spread$AwayTeam == final_doublefixture_f2[f2_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\F2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_f2[f2_sn,1],final_doublefixture_f2[f2_sn + 1,1],".csv",sep = "_")))

}
################################################################################################################################################
#G1
G1_spread <- subset(allteams20232024,Div =="G1")
G1_spread$n <- G1_spread$TG * 1
G1_spread$Bookings <- (G1_spread$HY *10 + G1_spread$HR *25) + (G1_spread$AY*10 + G1_spread$AR*25)
G1_spread$Crossbookings <- (G1_spread$HY *10 + G1_spread$HR *25)*(G1_spread$AY*10 + G1_spread$AR*25)
G1_spread$GoalsXbookings <- (G1_spread$Bookings)*(G1_spread$TG)
G1_spread$CornersXbookings <- (G1_spread$TC)*(G1_spread$Bookings)
G1_spread$GoalsXcorners <- (G1_spread$TG)*(G1_spread$TC)
G1_spread$GoalsXcornerXbookings <- (G1_spread$TG)*(G1_spread$TC)*(G1_spread$Bookings)

unlink('SpreadsN/G1/*')
for(g1_sn in 1:19){
  df <- tail(G1_spread[G1_spread$HomeTeam == final_doublefixture_g1[g1_sn,1] | G1_spread$AwayTeam == final_doublefixture_g1[g1_sn,1] ,],spreadn)

  df2 <- tail(G1_spread[G1_spread$HomeTeam == final_doublefixture_g1[g1_sn + 1,1] | G1_spread$AwayTeam == final_doublefixture_g1[g1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\G1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_g1[g1_sn,1],final_doublefixture_g1[g1_sn + 1,1],".csv",sep = "_")))

}
############################################################################################################################################
#I2
I2_spread <- subset(allteams20232024,Div =="I2")
I2_spread$n <- I2_spread$TG * 1
I2_spread$Bookings <- (I2_spread$HY *10 + I2_spread$HR *25) + (I2_spread$AY*10 + I2_spread$AR*25)
I2_spread$Crossbookings <- (I2_spread$HY *10 + I2_spread$HR *25)*(I2_spread$AY*10 + I2_spread$AR*25)
I2_spread$GoalsXbookings <- (I2_spread$Bookings)*(I2_spread$TG)
I2_spread$CornersXbookings <- (I2_spread$TC)*(I2_spread$Bookings)
I2_spread$GoalsXcorners <- (I2_spread$TG)*(I2_spread$TC)
I2_spread$GoalsXcornerXbookings <- (I2_spread$TG)*(I2_spread$TC)*(I2_spread$Bookings)

unlink('SpreadsN/I2/*')
for(i2_sn in 1:19){
  df <- tail(I2_spread[I2_spread$HomeTeam == final_doublefixture_i2[i2_sn,1] | I2_spread$AwayTeam == final_doublefixture_i2[i2_sn,1] ,],spreadn)

  df2 <- tail(I2_spread[I2_spread$HomeTeam == final_doublefixture_i2[i2_sn + 1,1] | I2_spread$AwayTeam == final_doublefixture_i2[i2_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\I2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_i2[i2_sn,1],final_doublefixture_i2[i2_sn + 1,1],".csv",sep = "_")))

}

############################################################################################################################################
N1_spread <- subset(allteams20232024,Div =="N1")
N1_spread$n <- N1_spread$TG * 1
N1_spread$Bookings <- (N1_spread$HY *10 + N1_spread$HR *25) + (N1_spread$AY*10 + N1_spread$AR*25)
N1_spread$Crossbookings <- (N1_spread$HY *10 + N1_spread$HR *25)*(N1_spread$AY*10 + N1_spread$AR*25)
N1_spread$GoalsXbookings <- (N1_spread$Bookings)*(N1_spread$TG)
N1_spread$CornersXbookings <- (N1_spread$TC)*(N1_spread$Bookings)
N1_spread$GoalsXcorners <- (N1_spread$TG)*(N1_spread$TC)
N1_spread$GoalsXcornerXbookings <- (N1_spread$TG)*(N1_spread$TC)*(N1_spread$Bookings)

unlink('SpreadsN/N1/*')
for(n1_sn in 1:17){
  df <- tail(N1_spread[N1_spread$HomeTeam == final_doublefixture_n1[n1_sn,1] | N1_spread$AwayTeam == final_doublefixture_n1[n1_sn,1] ,],spreadn)

  df2 <- tail(N1_spread[N1_spread$HomeTeam == final_doublefixture_n1[n1_sn + 1,1] | N1_spread$AwayTeam == final_doublefixture_n1[n1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\N1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_n1[n1_sn,1],final_doublefixture_n1[n1_sn + 1,1],".csv",sep = "_")))

}
###########################################################################################################################################
P1_spread <- subset(allteams20232024,Div =="P1")
P1_spread$n <- P1_spread$TG * 1
P1_spread$Bookings <- (P1_spread$HY *10 + P1_spread$HR *25) + (P1_spread$AY*10 + P1_spread$AR*25)
P1_spread$Crossbookings <- (P1_spread$HY *10 + P1_spread$HR *25)*(P1_spread$AY*10 + P1_spread$AR*25)
P1_spread$GoalsXbookings <- (P1_spread$Bookings)*(P1_spread$TG)
P1_spread$CornersXbookings <- (P1_spread$TC)*(P1_spread$Bookings)
P1_spread$GoalsXcorners <- (P1_spread$TG)*(P1_spread$TC)
P1_spread$GoalsXcornerXbookings <- (P1_spread$TG)*(P1_spread$TC)*(P1_spread$Bookings)

unlink('SpreadsN/P1/*')
for(p1_sn in 1:17){
  df <- tail(P1_spread[P1_spread$HomeTeam == final_doublefixture_p1[p1_sn,1] | P1_spread$AwayTeam == final_doublefixture_p1[p1_sn,1] ,],spreadn)

  df2 <- tail(P1_spread[P1_spread$HomeTeam == final_doublefixture_p1[p1_sn + 1,1] | P1_spread$AwayTeam == final_doublefixture_p1[p1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\P1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_p1[p1_sn,1],final_doublefixture_p1[p1_sn + 1,1],".csv",sep = "_")))

}
########################################################################################################################################
#SC0
SC0_spread <- subset(allteams20232024,Div =="SC0")
SC0_spread$n <- SC0_spread$TG * 1
SC0_spread$Bookings <- (SC0_spread$HY *10 + SC0_spread$HR *25) + (SC0_spread$AY*10 + SC0_spread$AR*25)
SC0_spread$Crossbookings <- (SC0_spread$HY *10 + SC0_spread$HR *25)*(SC0_spread$AY*10 + SC0_spread$AR*25)
SC0_spread$GoalsXbookings <- (SC0_spread$Bookings)*(SC0_spread$TG)
SC0_spread$CornersXbookings <- (SC0_spread$TC)*(SC0_spread$Bookings)
SC0_spread$GoalsXcorners <- (SC0_spread$TG)*(SC0_spread$TC)
SC0_spread$GoalsXcornerXbookings <- (SC0_spread$TG)*(SC0_spread$TC)*(SC0_spread$Bookings)

unlink('SpreadsN/SC0/*')
for(sc0_sn in 1:11){
  df <- tail(SC0_spread[SC0_spread$HomeTeam == final_doublefixture_sc0[sc0_sn,1] | SC0_spread$AwayTeam == final_doublefixture_sc0[sc0_sn,1] ,],spreadn)

  df2 <- tail(SC0_spread[SC0_spread$HomeTeam == final_doublefixture_sc0[sc0_sn + 1,1] | SC0_spread$AwayTeam == final_doublefixture_sc0[sc0_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\SC0"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sc0[sc0_sn,1],final_doublefixture_sc0[sc0_sn + 1,1],".csv",sep = "_")))

}
########################################################################################################################################
SC1_spread <- subset(allteams20232024,Div =="SC1")
SC1_spread$n <- SC1_spread$TG * 1
SC1_spread$Bookings <- (SC1_spread$HY *10 + SC1_spread$HR *25) + (SC1_spread$AY*10 + SC1_spread$AR*25)
SC1_spread$Crossbookings <- (SC1_spread$HY *10 + SC1_spread$HR *25)*(SC1_spread$AY*10 + SC1_spread$AR*25)
SC1_spread$GoalsXbookings <- (SC1_spread$Bookings)*(SC1_spread$TG)
SC1_spread$CornersXbookings <- (SC1_spread$TC)*(SC1_spread$Bookings)
SC1_spread$GoalsXcorners <- (SC1_spread$TG)*(SC1_spread$TC)
SC1_spread$GoalsXcornerXbookings <- (SC1_spread$TG)*(SC1_spread$TC)*(SC1_spread$Bookings)

unlink('SpreadsN/SC1/*')
for(sc1_sn in 1:9){
  df <- tail(SC1_spread[SC1_spread$HomeTeam == final_doublefixture_sc1[sc1_sn,1] | SC1_spread$AwayTeam == final_doublefixture_sc1[sc1_sn,1] ,],spreadn)

  df2 <- tail(SC1_spread[SC1_spread$HomeTeam == final_doublefixture_sc1[sc1_sn + 1,1] | SC1_spread$AwayTeam == final_doublefixture_sc1[sc1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\SC1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sc1[sc1_sn,1],final_doublefixture_sc1[sc1_sn + 1,1],".csv",sep = "_")))

}
###################################################################################################################################################
#SC2
SC2_spread <- subset(allteams20232024,Div =="SC2")
SC2_spread$n <- SC2_spread$TG * 1
SC2_spread$Bookings <- (SC2_spread$HY *10 + SC2_spread$HR *25) + (SC2_spread$AY*10 + SC2_spread$AR*25)
SC2_spread$Crossbookings <- (SC2_spread$HY *10 + SC2_spread$HR *25)*(SC2_spread$AY*10 + SC2_spread$AR*25)
SC2_spread$GoalsXbookings <- (SC2_spread$Bookings)*(SC2_spread$TG)
SC2_spread$CornersXbookings <- (SC2_spread$TC)*(SC2_spread$Bookings)
SC2_spread$GoalsXcorners <- (SC2_spread$TG)*(SC2_spread$TC)
SC2_spread$GoalsXcornerXbookings <- (SC2_spread$TG)*(SC2_spread$TC)*(SC2_spread$Bookings)

unlink('SpreadsN/SC2/*')
for(sc2_sn in 1:9){
  df <- tail(SC2_spread[SC2_spread$HomeTeam == final_doublefixture_sc2[sc2_sn,1] | SC2_spread$AwayTeam == final_doublefixture_sc2[sc2_sn,1] ,],spreadn)

  df2 <- tail(SC2_spread[SC2_spread$HomeTeam == final_doublefixture_sc2[sc2_sn + 1,1] | SC2_spread$AwayTeam == final_doublefixture_sc2[sc2_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\SC2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sc2[sc2_sn,1],final_doublefixture_sc2[sc2_sn + 1,1],".csv",sep = "_")))

}

##################################################################################################################################################
#SC3
SC3_spread <- subset(allteams20232024,Div =="SC3")
SC3_spread$n <- SC3_spread$TG * 1
SC3_spread$Bookings <- (SC3_spread$HY *10 + SC3_spread$HR *25) + (SC3_spread$AY*10 + SC3_spread$AR*25)
SC3_spread$Crossbookings <- (SC3_spread$HY *10 + SC3_spread$HR *25)*(SC3_spread$AY*10 + SC3_spread$AR*25)
SC3_spread$GoalsXbookings <- (SC3_spread$Bookings)*(SC3_spread$TG)
SC3_spread$CornersXbookings <- (SC3_spread$TC)*(SC3_spread$Bookings)
SC3_spread$GoalsXcorners <- (SC3_spread$TG)*(SC3_spread$TC)
SC3_spread$GoalsXcornerXbookings <- (SC3_spread$TG)*(SC3_spread$TC)*(SC3_spread$Bookings)

unlink('SpreadsN/SC3/*')
for(sc3_sn in 1:9){
  df <- tail(SC3_spread[SC3_spread$HomeTeam == final_doublefixture_sc3[sc3_sn,1] | SC3_spread$AwayTeam == final_doublefixture_sc3[sc3_sn,1] ,],spreadn)

  df2 <- tail(SC3_spread[SC3_spread$HomeTeam == final_doublefixture_sc3[sc3_sn + 1,1] | SC3_spread$AwayTeam == final_doublefixture_sc3[sc3_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\SC3"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sc3[sc3_sn,1],final_doublefixture_sc3[sc3_sn + 1,1],".csv",sep = "_")))

}
##################################################################################################################################################
#SP2
SP2_spread <- subset(allteams20232024,Div =="SP2")
SP2_spread$n <- SP2_spread$TG * 1
SP2_spread$Bookings <- (SP2_spread$HY *10 + SP2_spread$HR *25) + (SP2_spread$AY*10 + SP2_spread$AR*25)
SP2_spread$Crossbookings <- (SP2_spread$HY *10 + SP2_spread$HR *25)*(SP2_spread$AY*10 + SP2_spread$AR*25)
SP2_spread$GoalsXbookings <- (SP2_spread$Bookings)*(SP2_spread$TG)
SP2_spread$CornersXbookings <- (SP2_spread$TC)*(SP2_spread$Bookings)
SP2_spread$GoalsXcorners <- (SP2_spread$TG)*(SP2_spread$TC)
SP2_spread$GoalsXcornerXbookings <- (SP2_spread$TG)*(SP2_spread$TC)*(SP2_spread$Bookings)

unlink('SpreadsN/SP2/*')
for(sp2_sn in 1:21){
  df <- tail(SP2_spread[SP2_spread$HomeTeam == final_doublefixture_sp2[sp2_sn,1] | SP2_spread$AwayTeam == final_doublefixture_sp2[sp2_sn,1] ,],spreadn)

  df2 <- tail(SP2_spread[SP2_spread$HomeTeam == final_doublefixture_sp2[sp2_sn + 1,1] | SP2_spread$AwayTeam == final_doublefixture_sp2[sp2_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\SP2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sp2[sp2_sn,1],final_doublefixture_sp2[sp2_sn + 1,1],".csv",sep = "_")))

}
##################################################################################################################################################
T1_spread <- subset(allteams20232024,Div =="T1")
T1_spread$n <- T1_spread$TG * 1
T1_spread$Bookings <- (T1_spread$HY *10 + T1_spread$HR *25) + (T1_spread$AY*10 + T1_spread$AR*25)
T1_spread$Crossbookings <- (T1_spread$HY *10 + T1_spread$HR *25)*(T1_spread$AY*10 + T1_spread$AR*25)
T1_spread$GoalsXbookings <- (T1_spread$Bookings)*(T1_spread$TG)
T1_spread$CornersXbookings <- (T1_spread$TC)*(T1_spread$Bookings)
T1_spread$GoalsXcorners <- (T1_spread$TG)*(T1_spread$TC)
T1_spread$GoalsXcornerXbookings <- (T1_spread$TG)*(T1_spread$TC)*(T1_spread$Bookings)

unlink('SpreadsN/T1/*')
for(t1_sn in 1:19){
  df <- tail(T1_spread[T1_spread$HomeTeam == final_doublefixture_t1[t1_sn,1] | T1_spread$AwayTeam == final_doublefixture_t1[t1_sn,1] ,],spreadn)

  df2 <- tail(T1_spread[T1_spread$HomeTeam == final_doublefixture_t1[t1_sn + 1,1] | T1_spread$AwayTeam == final_doublefixture_t1[t1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\T1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_t1[t1_sn,1],final_doublefixture_t1[t1_sn + 1,1],".csv",sep = "_")))

}

Europe_spread <- rbind(B1_spread,D2_spread,E1_spread,E2_spread,E3_spread,EC_spread,F2_spread,G1_spread,I2_spread,SC0_spread,SC1_spread,SC2_spread,SC3_spread,SP2_spread,T1_spread)

write.csv(Europe_spread,'Europespread.csv')













