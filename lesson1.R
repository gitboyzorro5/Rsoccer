data <- read.csv("allteams20202021SOT.csv")
summary(data)
epl <- subset(data,Div == "E0")
laliga <- subset(data,Div == "SP1")
summary(laliga)
is.data.frame(data)
head(data)
tail(data)
str(data)
nrow(data)
ncol(data)
colnames(data)
rownames(data)
first_20 <- head(data[['HomeTeam']],20)
first_20
data$FTR
data[1,3]
data[,1]
epl[epl$FTHG > 3,]
transform(data,FTHG = FTHG + 1)
install.packages('xlsx')
library('xlsx')
myoddsdata <- read.xlsx("myodds.xlsx",sheetIndex = 1)
summary(myoddsdata)
install.packages('dplyr')
library('dplyr')
a <- data.frame(a = 1:2, b = 2:3, c = 4:5)
b <- data.frame(a = 5:6, b = 6:7 , c = 7:8,d = 8:9)
a
b
rbind(a,b)
bind_rows(a,b)
total_goal <- data$FTHG + data$FTAG
as.vector(total_goal)
class(total_goal)
is.vector(total_goal)
data <- cbind(data,total_goal)
head(data)
