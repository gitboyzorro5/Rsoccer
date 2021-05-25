library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('pinnacle.data')
library('odds.converter')
library('sqldf')



#Full time results percentages
ftr_summary <- tabyl(allteams20202021,Div,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,4,3,2)]
#Half time results percentages
htr_summary <- tabyl(allteams20202021,Div,HTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
htr_summary <- htr_summary[,c(1,4,3,2)]



myodds <- readxl::read_excel('../FDAS/myodds.xlsx', sheet = '2way')

##################################################
fixtures <- read.csv('../FDAS/fixtures.csv')
fixtures$Date <- dmy(fixtures$Date)
fixtures <- fixtures[order(as.Date(fixtures$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#create true odds calc
true_odds_calc_2way <- c()
#true_odds_calc_2way <- c('B_ov25','B_und25','Margin','F_ov25','F_un25','T_ov25prob','T_un25prob','HT','AT','Div','Date')
true_odds_calc_2way$B_ov25 <- fixtures$P.2.5
true_odds_calc_2way$B_und25 <- fixtures$P.2.5.1
true_odds_calc_2way$Margin <- percent(((1/true_odds_calc_2way$B_ov25) + (1/true_odds_calc_2way$B_und25)-1), accuracy = 0.01)
true_odds_calc_2way$F_ov25 <- round(true_odds_calc_2way$B_ov25 * (1 + ((1/true_odds_calc_2way$B_ov25) + (1/true_odds_calc_2way$B_und25)-1)), digits = 2)
true_odds_calc_2way$F_un25 <- round(true_odds_calc_2way$B_und25 * (1 + ((1/true_odds_calc_2way$B_ov25) + (1/true_odds_calc_2way$B_und25)-1)), digits = 2)
true_odds_calc_2way$T_ov25prob <- percent(odds.dec2prob(true_odds_calc_2way$F_ov25), accuracy = 0.01)
true_odds_calc_2way$T_un25prob <- percent(odds.dec2prob(true_odds_calc_2way$F_un25), accuracy = 0.01)
true_odds_calc_2way$HT <- fixtures$HomeTeam
true_odds_calc_2way$AT <- fixtures$AwayTeam
true_odds_calc_2way$Div <- fixtures$Div
true_odds_calc_2way$Date <- fixtures$Date
true_odds_calc_2way <- as.data.frame(true_odds_calc_2way)
#create true odds 3 way
true_odds_calc_3way <- c()
#true_odds_calc_3way <- c('B_ov25','B_und25','Margin','F_ov25','F_un25','T_ov25prob','T_un25prob','HT','AT','Div','Date')
true_odds_calc_3way$B_H <- fixtures$PSH
true_odds_calc_3way$B_D <- fixtures$PSD
true_odds_calc_3way$B_A <- fixtures$PSA

true_odds_calc_3way$Margin <- percent(((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1), accuracy = 0.01)
true_odds_calc_3way$F_H <- round(true_odds_calc_3way$B_H * (1 + ((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1)), digits = 2)
true_odds_calc_3way$F_D <- round(true_odds_calc_3way$B_D * (1 + ((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1)), digits = 2)
true_odds_calc_3way$F_A <- round(true_odds_calc_3way$B_A * (1 + ((1/true_odds_calc_3way$B_H) + (1/true_odds_calc_3way$B_D) + (1/true_odds_calc_3way$B_A) -1)), digits = 2)


true_odds_calc_3way$F_Hprob <- percent(odds.dec2prob(true_odds_calc_3way$F_H), accuracy = 0.01)
true_odds_calc_3way$F_Dprob <- percent(odds.dec2prob(true_odds_calc_3way$F_D), accuracy = 0.01)
true_odds_calc_3way$F_Aprob <- percent(odds.dec2prob(true_odds_calc_3way$F_A), accuracy = 0.01)

true_odds_calc_3way$HT <- fixtures$HomeTeam
true_odds_calc_3way$AT <- fixtures$AwayTeam
true_odds_calc_3way$Div <- fixtures$Div
true_odds_calc_3way$Date <- fixtures$Date
true_odds_calc_3way <- as.data.frame(true_odds_calc_3way)

x <- stats::dpois(0,1.1812)
y <- stats::dpois(0,1.4660)

x * y

#####################################################################

EURO <- read.csv('../../../Leonard.000/Downloads/IFootball/results.csv')
library('lubridate')
EURO$date <- ymd(EURO$date)
EURO <- EURO[order(as.Date(EURO$date, format = "%Y/%m%d"), decreasing = FALSE),]
sort(unique(EURO$tournament))
EURO_qualificaton <- subset(EURO,tournament == "UEFA Euro qualification")
EURO <- subset(EURO,tournament == "UEFA Euro")









