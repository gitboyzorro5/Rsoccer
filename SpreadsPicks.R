

#D1
unlink('Spreads/D1/*')
for(d1_sn in 1:17){
  df <- D1_spread[D1_spread$HomeTeam == final_doublefixture_d1[d1_sn,1] | D1_spread$AwayTeam == final_doublefixture_d1[d1_sn,1] ,]
  df2 <- D1_spread[D1_spread$HomeTeam == final_doublefixture_d1[d1_sn + 1,1] | D1_spread$AwayTeam == final_doublefixture_d1[d1_sn + 1,1],]
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads\\D1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_d1[d1_sn,1],final_doublefixture_d1[d1_sn + 1,1],".csv",sep = "_")))

}

#E0
unlink('Spreads/E0/*')
for(e0_sn in 1:19){
  df <- E0_spread[E0_spread$HomeTeam == final_doublefixture_e0[e0_sn,1] | E0_spread$AwayTeam == final_doublefixture_e0[e0_sn,1] ,]

  df2 <- E0_spread[E0_spread$HomeTeam == final_doublefixture_e0[e0_sn + 1,1] | E0_spread$AwayTeam == final_doublefixture_e0[e0_sn + 1,1],]

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads\\E0"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e0[e0_sn,1],final_doublefixture_e0[e0_sn + 1,1],".csv",sep = "_")))

}

#I1
unlink('Spreads/I1/*')
for(i1_sn in 1:19){
  df <- I1_spread[I1_spread$HomeTeam == final_doublefixture_i1[i1_sn,1] | I1_spread$AwayTeam == final_doublefixture_i1[i1_sn,1],]
  df2 <- I1_spread[I1_spread$HomeTeam == final_doublefixture_i1[i1_sn + 1,1] | I1_spread$AwayTeam == final_doublefixture_i1[i1_sn + 1,1],]
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads\\I1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_i1[i1_sn,1],final_doublefixture_i1[i1_sn + 1,1],".csv",sep = "_")))

}


#SP1
unlink('Spreads/SP1/*')
for(sp1_sn in 1:19){
  df <- SP1_spread[SP1_spread$HomeTeam == final_doublefixture_sp1[sp1_sn,1] | SP1_spread$AwayTeam == final_doublefixture_sp1[sp1_sn,1] ,]
  df2 <- SP1_spread[SP1_spread$HomeTeam == final_doublefixture_sp1[sp1_sn + 1,1] | SP1_spread$AwayTeam == final_doublefixture_sp1[sp1_sn + 1,1],]
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads\\SP1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sp1[sp1_sn,1],final_doublefixture_sp1[sp1_sn + 1,1],".csv",sep = "_")))

}

#F1
unlink('Spreads/F1/*')
for(f1_sn in 1:19){
  df <- F1_spread[F1_spread$HomeTeam == final_doublefixture_f1[f1_sn,1] | F1_spread$AwayTeam == final_doublefixture_f1[f1_sn,1] ,]
  df2 <- F1_spread[F1_spread$HomeTeam == final_doublefixture_f1[f1_sn + 1,1] | F1_spread$AwayTeam == final_doublefixture_f1[f1_sn + 1,1],]
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:37]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads\\F1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_f1[f1_sn,1],final_doublefixture_f1[f1_sn + 1,1],".csv",sep = "_")))

}






