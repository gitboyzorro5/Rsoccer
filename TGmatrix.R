data_2021 <- read.csv('allteams20202021SOT.csv')
total_goals <- data_2021$FTHG + data_2021$FTAG
data_2021$TG <- total_goals
cbind(data_2021, total_goals)

library('lubridate')
library('dplyr')
data_2021$Date <- dmy(data_2021$Date)
B1 <- subset(data_2021, Div == "B1")
sorted_B1 <- B1[order(as.Date(B1$Date, format = "%d/%m/%Y"),decreasing = FALSE),]

#tgv2 <- with(B1, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))

write.csv(tgv2,'biv2.csv')
with(sorted_B1, tapply(TG, list(HomeTeam, Date), FUN = mean))
awayteam_tg <- with(sorted_B1, tapply(TG, list(AwayTeam, Date), FUN = mean))

testdata_h <- tapply(B1$TG, B1[c("HomeTeam", "Date")],mean)
testdata_a <- tapply(B1$TG, B1[c("AwayTeam", "Date")],mean)

hometeam_tg[is.na(hometeam_tg)] <- ""
awayteam_tg[is.na(awayteam_tg)] <- ""

write.csv(hometeam_tg, 'hometg.csv')
write.csv(awayteam_tg, 'awaytg.csv')

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
