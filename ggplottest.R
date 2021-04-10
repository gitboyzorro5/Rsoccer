install.packages('ggplot2')
install.packages('GGally')
library('ggplot2')
library('lubridate')
library('GGally')
data <- read.csv('covid_19_data_cleaned.csv')
str(data)
data$Date <- mdy(data$Date)
str(data)
library('plyr')
data <- ddply(data,.(Date,Country),numcolwise(sum))
data <- within(data, rm(Lat,Long))
us <- subset(data, Country == "US")
latest <- subset(data, Date == max(Date)-3)
latest <- latest[order(latest$Confirmed, decreasing = TRUE),]
top10 <- latest[1:10,]
rownames(top10) <- 1:10
options(rep.plot.width - 14)
ggp <- ggplot(top10, aes(x=reorder(Country, -Confirmed), y=Confirmed))
ggp + geom_col(color = 'red', fill = 'orange') +
      labs(title = 'Top 10 Worst Hit countries',
           subtitle = 'Covid-19 dataset worldwide',
           caption = 'By John Hopkins University') +
           theme(
              plot.title = element_text(color = 'black',size = 24),
              plot.subtitle = element_text(color = 'grey',size = 24)
              )

ggps <- ggplot(us, aes(x=Date, y=Confirmed))
  ggps + geom_point()
top10
