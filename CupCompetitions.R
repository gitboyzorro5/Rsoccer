####################
######CUP COMPETITIONS
##########################
###FA#######
FA <- rbind(E0_spread[,-c(37,38,40,45,47,48,49)],E1_spread)
#UCL <- read.csv('UCL.csv')
unlink('FA.csv')
write.csv(FA,'FA.csv')
###############################################################################
df <- tail(FA[FA$HomeTeam =="Man City" | FA$AwayTeam =="Man City",],6)
df2 <- tail(FA[FA$HomeTeam == "Burnley" | FA$AwayTeam == "Burnley",],6)
temp_analysis <- rbind(df,df2)
temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:36]
temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
write.csv(temp_analysis,'Temp/CityvsBurnley.csv')
