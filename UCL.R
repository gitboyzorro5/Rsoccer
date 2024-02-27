library('xlsx')
###############
E0_spreaducl <- readxl::read_excel('E0_spread.xlsx')
E0_spreaducl <- E0_spreaducl[,c(-1)]
E0_ucl <- E0_spreaducl
colnames(E0_ucl)[38] <- "goalmins"
colnames(E0_ucl)[40] <- "shirts"
#########################################
D1_spreaducl <- readxl::read_excel('D1_spread.xlsx')
D1_spreaducl <- D1_spreaducl[,c(-1)]
D1_ucl <- D1_spreaducl
colnames(D1_ucl)[38] <- "goalmins"
colnames(D1_ucl)[40] <- "shirts"
###########################################
SP1_spreaducl <- readxl::read_excel('SP1_spread.xlsx')
SP1_spreaducl <- SP1_spreaducl[,c(-1)]
SP1_ucl <- SP1_spreaducl
colnames(SP1_ucl)[38] <- "goalmins"
colnames(SP1_ucl)[40] <- "shirts"
###########################################
I1_spreaducl <- readxl::read_excel('I1_spread.xlsx')
I1_spreaducl <- I1_spreaducl[,c(-1)]
I1_ucl <- I1_spreaducl
colnames(I1_ucl)[38] <- "goalmins"
colnames(I1_ucl)[40] <- "shirts"

###########################################
F1_spreaducl <- readxl::read_excel('F1_spread.xlsx')
F1_spreaducl <- F1_spreaducl[,c(-1)]
F1_ucl <- F1_spreaducl
colnames(F1_ucl)[38] <- "goalmins"
colnames(F1_ucl)[40] <- "shirts"
##################################
UCL20232024 <- rbind(E0_ucl,D1_ucl,SP1_ucl,I1_ucl,F1_ucl)
#UCL <- read.csv('UCL.csv')
unlink('UCL20232024.xlsx')
write.xlsx(UCL20232024,'UCL20232024.xlsx')
###############################################################################
df <- tail(UCL20232024[UCL20232024$HomeTeam =="Torino" | UCL20232024$AwayTeam =="Torino",],6)
df2 <- tail(UCL20232024[UCL20232024$HomeTeam == "Lazio" | UCL20232024$AwayTeam == "Lazio",],6)
temp_analysis <- rbind(df,df2)

temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(38,39,40,41,42,43,44,45,46,47,48,49,50)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:37]
temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
write.xlsx(temp_analysis,'Temp/torinovlazio.xlsx')

#write.xlsx(UCL20232024[UCL20232024$Div == "D1",],"D1_spread.xlsx")
e1_teams
