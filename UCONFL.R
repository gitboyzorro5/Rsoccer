
UCONFL <- rbind(T1_spread,SP1_spread[,-c(37,38,40,45,47,48,49)],E0_spread[,-c(37,38,40,45,47,48,49)],P1_spread,B1_spread,D1_spread[,-c(37,38,40,45,47,48,49)])
#UCL <- read.csv('UCL.csv')
unlink('UCONFL.csv')
write.csv(UCONFL,'UCONFL.csv')
###############################################################################
df <- tail(UCONFL[UCONFL$HomeTeam =="Union Berlin" | UCONFL$AwayTeam =="Union Berlin",],6)
df2 <- tail(UCONFL[UCONFL$HomeTeam == "St. Gilloise" | UCONFL$AwayTeam == "St. Gilloise",],6)
temp_analysis <- rbind(df,df2)
temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:36]
temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
write.csv(temp_analysis,'Temp/Union BerlinGilloise.csv')

