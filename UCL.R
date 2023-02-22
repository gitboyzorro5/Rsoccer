E0_ucl <- E0_spread
colnames(E0_ucl)[38] <- "goalmins"
colnames(E0_ucl)[40] <- "shirts"
###########################################
D1_ucl <- D1_spread
colnames(D1_ucl)[38] <- "goalmins"
colnames(D1_ucl)[40] <- "shirts"
###########################################
SP1_ucl <- SP1_spread
colnames(SP1_ucl)[38] <- "goalmins"
colnames(SP1_ucl)[40] <- "shirts"
###########################################
I1_ucl <- I1_spread
colnames(I1_ucl)[38] <- "goalmins"
colnames(I1_ucl)[40] <- "shirts"
###########################################

F1_ucl <- F1_spread
colnames(F1_ucl)[38] <- "goalmins"
colnames(F1_ucl)[40] <- "shirts"

##################################
UCL <- rbind(E0_ucl,D1_ucl,SP1_ucl,I1_ucl)#F1_ucl)
unlink('UCL.csv')
write.csv(UCL,'UCL.csv')
###############################################################################
df <- D1_ucl[D1_ucl$HomeTeam =="RB Leipzig" | D1_ucl$AwayTeam =="RB Leipzig",]
df2 <- I1_ucl[I1_ucl$HomeTeam == "Man City" | I1_ucl$AwayTeam == "Man City",]
temp_analysis <- rbind(df,df2)

temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:37]
temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
write.csv(temp_analysis,'cityleipzig.csv')

