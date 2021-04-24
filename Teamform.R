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
#tgv2 <- with(B1, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))

#with(sorted_B1, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_B1, tapply(TG, list(AwayTeam, Date), FUN = mean))

testdata_h <- tapply(D1$FTR, D1[c("HomeTeam", "Date")],median)
testdata_a <- tapply(D1$FTR, D1[c("AwayTeam", "Date")],median)


testdata_h[is.na(testdata_h)] <- ""
testdata_a[is.na(testdata_a)] <- ""

testdata_h <- sub("A","L",testdata_h)
testdata_h <- sub("H","W",testdata_h)

testdata_a <- sub("A","W",testdata_a)
testdata_a <- sub("H","L",testdata_a)



for(rowh in 1:nrow(testdata_h)) {
  for(colh in 1:ncol(testdata_h)) {

    # print(my_matrix[row, col])
    for(rowa in 1:nrow(testdata_a)) {
      for(cola in 1:ncol(testdata_a)) {
        ifelse(!testdata_a[rowa,cola]=="",testdata_h[rowa,cola] <- testdata_a[rowa,cola],next)

        #print(my_matrix[row, col])
      }
    }

  }
}



#write.csv(hometeam_tg, 'hometg.csv')
#write.csv(awayteam_tg, 'awaytg.csv')

write.csv(testdata_h,'teamformdiv.csv')
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
