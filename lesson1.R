data <- read.csv("allteams20202021SOT.csv")
summary(data)
epl <- subset(data,Div == "E0")
summary(epl)
