###############
E0_spread <- read.csv('E0_spread.csv')
E0_spread <- E0_spread[,-1]
E0_ucl <- E0_spread
colnames(E0_ucl)[38] <- "goalmins"
colnames(E0_ucl)[40] <- "shirts"
#########################################
D1_spread <- read.csv('D1_spread.csv')
D1_spread <- D1_spread[,-1]
D1_ucl <- D1_spread
colnames(D1_ucl)[38] <- "goalmins"
colnames(D1_ucl)[40] <- "shirts"
###########################################
SP1_spread <- read.csv('SP1_spread.csv')
SP1_spread <- SP1_spread[,-1]
SP1_ucl <- SP1_spread
colnames(SP1_ucl)[38] <- "goalmins"
colnames(SP1_ucl)[40] <- "shirts"
###########################################
I1_spread <- read.csv('I1_spread.csv')
I1_spread <- I1_spread[,-1]
I1_ucl <- I1_spread
colnames(I1_ucl)[38] <- "goalmins"
colnames(I1_ucl)[40] <- "shirts"
###########################################
F1_spread <- read.csv('F1_spread.csv')
F1_spread <- F1_spread[,-1]
F1_ucl <- F1_spread
colnames(F1_ucl)[38] <- "goalmins"
colnames(F1_ucl)[40] <- "shirts"

##################################
UCL <- rbind(E0_ucl,D1_ucl,SP1_ucl,I1_ucl,F1_ucl)
#UCL <- read.csv('UCL.csv')
unlink('UCL.csv')
write.csv(UCL,'UCL.csv')
###############################################################################
df <- tail(UCL[UCL$HomeTeam =="Man City" | UCL$AwayTeam =="Man City",],6)
df2 <- tail(UCL[UCL$HomeTeam == "Inter" | UCL$AwayTeam == "Inter",],6)
temp_analysis <- rbind(df,df2)

temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:37]
temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
write.csv(temp_analysis,'Temp/cityvsinter.csv')

