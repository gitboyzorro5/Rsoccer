data_2021 <- read.csv('allteams20202021SOT.csv')
total_goals <- data_2021$FTHG + data_2021$FTAG
data_2021$TG <- total_goals
cbind(data_2021, total_goals)

library('lubridate')
library('dplyr')
head(data_2021)
data_2021$Date <- dmy(data_2021$Date)
head(data_2021)
D1 <- subset(data_2021, Div == "D1")
sorted_D1 <- D1[order(as.Date(D1$Date, format = "%d/%m/%Y"),decreasing = FALSE),]
sorted_D1
#tgv2 <- with(D1, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))

#with(sorted_D1, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_D1, tapply(TG, list(AwayTeam, Date), FUN = mean))

gtotalsv2 <- tapply(D1$TG, D1[c("HomeTeam", "AwayTeam")],mean)


hgtotals <- rowSums(gtotalsv2, na.rm = T)
agtotals <- colSums(gtotalsv2, na.rm = T)

gtotalsv2 <- cbind(gtotalsv2,hgtotals,agtotals)

TotalGoals <- hgtotals + agtotals

gtotalsv2 <- cbind(gtotalsv2,TotalGoals)

biteams <- sort(unique(sorted_D1$HomeTeam))

home_games <- c()
away_games <- c()

for (i in 1:length(biteams))
{

home_games[i] <- nrow(sorted_D1[sorted_D1$HomeTeam == biteams[i],])
away_games[i]  <- nrow(sorted_D1[sorted_D1$AwayTeam == biteams[i],])

}

games_played <- home_games + away_games
gtotalsv2 <- cbind(gtotalsv2,games_played)

avg_totalgoals <- round((TotalGoals / games_played), digits = 4)

gtotalsv2 <- cbind(gtotalsv2,avg_totalgoals)
gtotalsv2[is.na(gtotalsv2)] <- ""




write.csv(gtotalsv2,'gtotals.csv')

gtotalsv2_h[is.na(gtotalsv2_h)] <- ""
gsmatrix_a[is.na(gsmatrix_a)] <- ""

rowcount <- nrow(testdata_h)
colcount <- ncol(testdata_h)

for(rowh in 1:nrow(gtotalsv2_h)) {
  for(colh in 1:ncol(gtotalsv2_h)) {

    # print(my_matrix[row, col])
    for(rowa in 1:nrow(gsmatrix_a)) {
      for(cola in 1:ncol(gsmatrix_a)) {
        ifelse(!gsmatrix_a[rowa,cola]=="",gtotalsv2_h[rowa,cola] <- gsmatrix_a[rowa,cola],next)

        #print(my_matrix[row, col])
      }
    }

  }
}



#write.csv(hometeam_tg, 'hometg.csv')
#write.csv(awayteam_tg, 'awaytg.csv')

write.csv(gtotalsv2_h,'gsmatrix.csv')
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
write.xlsx(hometeam_tg,'hometg.xlsx',sheetName = "D1h")
write.xlsx(awayteam_tg,'hometg.xlsx',sheetName = "D1h", append = TRUE)

library('ggplot2')
ggD1 <- ggplot(sorted_D1, aes(x=Date,y=TG))
ggD1 + geom_point()


#create Teams dataset
