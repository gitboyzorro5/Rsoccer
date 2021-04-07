data_2021 <- read.csv('allteams20202021SOT.csv')
total_goals <- data_2021$FTHG + data_2021$FTAG
data_2021$TG <- total_goals
cbind(data_2021, total_goals)
B1 <- subset(data_2021, Div == "B1")
#library('lubridate')
with(B1, tapply(TG, list(HomeTeam, Date), FUN = mean))
testdata_h <- tapply(B1$TG, B1[c("HomeTeam", "Date")],mean)
testdata_a <- tapply(B1$TG, B1[c("AwayTeam", "Date")],mean)
testdata_h[is.na(testdata_h)] <- ""
testdata_a[is.na(testdata_a)] <- ""
tgmatrix <- as.matrix(rbind(testdata_h,testdata_a))
write.csv(testdata_h, 'hometg.csv')
write(testdata_a, file = 'hometg.csv', append = TRUE)
newlist <- list(testdata_h,testdata_a)
write.csv(newlist,'list.csv')
str(newlist)
?list
?pairlist
