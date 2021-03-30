head(data)
#create total_goals column
total_goals <- data$FTHG + data$FTAG
#assign column to data frame
data <- cbind(data,total_goals)
head(data)
