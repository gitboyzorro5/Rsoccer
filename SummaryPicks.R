library('xlsx')
#B1
advstatsn <- 6
B1_advstats <- readxl::read_excel('B1_SPREAD.xlsx')
B1_advstats <- B1_advstats[,-1]

unlink('Summaries/B1/*')
for(b1_sn in 1:15){
  df <- tail(B1_advstats[B1_advstats$HomeTeam == final_doublefixture_b1[b1_sn,1] | B1_advstats$AwayTeam == final_doublefixture_b1[b1_sn,1] ,],advstatsn)

  df2 <- tail(B1_advstats[B1_advstats$HomeTeam == final_doublefixture_b1[b1_sn + 1,1] | B1_advstats$AwayTeam == final_doublefixture_b1[b1_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Summaries\\B1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_b1[b1_sn,1],final_doublefixture_b1[b1_sn + 1,1],"adv.csv",sep = "_")))

}

######################################################################################################################################
#D2
advstatsn <- 6
D2_advstats <- readxl::read_excel('D2_SPREAD.xlsx')
D2_advstats <- D2_advstats[,-1]

unlink('Summaries/D2/*')
for(d2_sn in 1:17){
  df <- tail(D2_advstats[D2_advstats$HomeTeam == final_doublefixture_d2[d2_sn,1] | D2_advstats$AwayTeam == final_doublefixture_d2[d2_sn,1] ,],advstatsn)

  df2 <- tail(D2_advstats[D2_advstats$HomeTeam == final_doublefixture_d2[d2_sn + 1,1] | D2_advstats$AwayTeam == final_doublefixture_d2[d2_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Summaries\\D2"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_d2[d2_sn,1],final_doublefixture_d2[d2_sn + 1,1],"adv.csv",sep = "_")))

}
############################################################################################################################################
#E1
advstatsn <- 6
E1_advstats <- readxl::read_excel('E1_SPREAD.xlsx')
E1_advstats <- E1_advstats[,-1]

unlink('Summaries/E1/*')
for(e1_sn in 1:23){
  df <- tail(E1_advstats[E1_advstats$HomeTeam == final_doublefixture_e1[e1_sn,1] | E1_advstats$AwayTeam == final_doublefixture_e1[e1_sn,1] ,],advstatsn)

  df2 <- tail(E1_advstats[E1_advstats$HomeTeam == final_doublefixture_e1[e1_sn + 1,1] | E1_advstats$AwayTeam == final_doublefixture_e1[e1_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Summaries\\E1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e1[e1_sn,1],final_doublefixture_e1[e1_sn + 1,1],"adv.csv",sep = "_")))

}
################################################################################################################################################
#SC0
advstatsn <- 6
SC0_advstats <- readxl::read_excel('SC0_SPREAD.xlsx')
SC0_advstats <- SC0_advstats[,-1]

unlink('Summaries/SC0/*')
for(sc0_sn in 1:11){
  df <- tail(SC0_advstats[SC0_advstats$HomeTeam == final_doublefixture_sc0[sc0_sn,1] | SC0_advstats$AwayTeam == final_doublefixture_sc0[sc0_sn,1] ,],advstatsn)

  df2 <- tail(SC0_advstats[SC0_advstats$HomeTeam == final_doublefixture_sc0[sc0_sn + 1,1] | SC0_advstats$AwayTeam == final_doublefixture_sc0[sc0_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Summaries\\SC0"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_sc0[sc0_sn,1],final_doublefixture_sc0[sc0_sn + 1,1],"adv.csv",sep = "_")))

}



