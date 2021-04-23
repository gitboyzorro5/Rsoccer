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

gcmatrix_h <- tapply(B1$FTAG, B1[c("HomeTeam", "Date")],mean)
gcmatrix_a <- tapply(B1$FTHG, B1[c("AwayTeam", "Date")],mean)

gcmatrix_h[is.na(gcmatrix_h)] <- ""
gcmatrix_a[is.na(gcmatrix_a)] <- ""

rowcount <- nrow(testdata_h)
colcount <- ncol(testdata_h)

for(rowh in 1:nrow(gcmatrix_h)) {
  for(colh in 1:ncol(gcmatrix_h)) {

    # print(my_matrix[row, col])
    for(rowa in 1:nrow(gcmatrix_a)) {
      for(cola in 1:ncol(gcmatrix_a)) {
        ifelse(!gcmatrix_a[rowa,cola]=="",gcmatrix_h[rowa,cola] <- gcmatrix_a[rowa,cola],next)

        #print(my_matrix[row, col])
      }
    }

  }
}



#write.csv(hometeam_tg, 'hometg.csv')
#write.csv(awayteam_tg, 'awaytg.csv')

write.csv(gcmatrix_h,'gcmatrix.csv')
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
