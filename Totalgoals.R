head(data)
#create total_goals column
total_goals <- data$FTHG + data$FTAG
#assign column to data frame
data <- cbind(data,total_goals)
head(data)
colnames(data)
newdata <- subset(data, select = -c(margin,margin))
head(newdata)
colnames(newdata)
newdata$OV25 <- ifelse(newdata$total_goals >= 3,"Y","N")
colnames(newdata)
head(newdata)
library('dplyr')
summary_div <- newdata %>% group_by(Div) %>% summarise_if(is.numeric,sum)
nrow(newdata[newdata$Div == "E0",])
write.csv(summary_div,"sumall.csv")
divisions <- unique(newdata$Div)
index <- seq(1,22)


