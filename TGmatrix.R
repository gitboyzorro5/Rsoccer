data_2021 <- read.csv('allteams20202021SOT.csv')
total_goals <- data_2021$FTHG + data_2021$FTAG
data_2021$TG <- total_goals
cbind(data_2021, total_goals)

library('lubridate')
library('dplyr')
head(data_2021)
data_2021$Date <- dmy(data_2021$Date)
head(data_2021)
B1 <- subset(data_2021, Div == "B1")
sorted_B1 <- B1[order(as.Date(B1$Date, format = "%d/%m/%Y"),decreasing = FALSE),]
sorted_B1
#tgv2 <- with(B1, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))

#with(sorted_B1, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_B1, tapply(TG, list(AwayTeam, Date), FUN = mean))

testdata_h <- tapply(B1$TG, B1[c("HomeTeam", "Date")],mean)
testdata_a <- tapply(B1$TG, B1[c("AwayTeam", "Date")],mean)


testdata_h[is.na(testdata_h)] <- ""
testdata_a[is.na(testdata_a)] <- ""

write.csv(hometeam_tg, 'hometg.csv')
write.csv(awayteam_tg, 'awaytg.csv')

write.csv(testdata_h,'testdata_h.csv')
write.csv(testdata_a,'testdata_a.csv')

testdata_h[1,] <- testdata_a[1,]
testdata_a[10,5]

dim(testdata_h)
dim(testdata_a)


mergedtest <- merge(hometeam_tg, awayteam_tg,by = 'row.names',all.hometeam_tg = awayteam_tgl,no.dups = T)
write.csv(mergedtest,'merged.csv')
?merge
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
write.xlsx(hometeam_tg,'hometg.xlsx',sheetName = "B1h")
write.xlsx(awayteam_tg,'hometg.xlsx',sheetName = "B1h", append = TRUE)

library('ggplot2')
ggb1 <- ggplot(sorted_B1, aes(x=Date,y=TG))
ggb1 + geom_point()


#create Teams dataset
