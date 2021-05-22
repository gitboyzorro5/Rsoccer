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
HomeTeam_b1 <- rep(b1_teams, each = length(b1_teams))
AwayTeam_b1 <- rep(b1_teams, length(b1_teams))
B1_fixtures <- cbind(HomeTeam_b1,AwayTeam_b1)
B1_fixtures <- as.data.frame(B1_fixtures)
B1_fixtures <- B1_fixtures[!B1_fixtures$HomeTeam_b1 == B1_fixtures$AwayTeam_b1,]
rownames(B1_fixtures) <- NULL
B1_fixtures$Div <- "B1"
B1_fixtures <- B1_fixtures[,c(3,1,2)]

B1_fixtures$Date <- sqldf("SELECT B1.Date from B1 INNER JOIN B1_fixtures ON B1.Hometeam = B1_fixtures.HomeTeam_b1 AND B1.AwayTeam = B1_fixtures.AwayTeam_b1")

B1_fixtures$avg_HG_b1 <- b1_avg_HG

B1_fixtures$b1_homeas <- rep(b1_home_as,each = length(b1_teams)-1)

b1_awayds_lookup <- cbind(b1_teams,b1_away_ds)

b1_awayds_lookup <- as.data.frame(b1_awayds_lookup)

colnames(b1_awayds_lookup) <- c("AwayTeam_b1","b1_awayds")


require('RH2')
B1_fixtures$b1_awayds <- sqldf("SELECT b1_awayds_lookup.b1_awayds FROM b1_awayds_lookup INNER JOIN B1_fixtures ON b1_awayds_lookup.AwayTeam_b1 = B1_fixtures.AwayTeam_b1")

B1_fixtures$avg_AG_b1 <- b1_avg_AG

b1_awayas_lookup <- cbind(b1_teams,b1_away_as)

b1_awayas_lookup <- as.data.frame(b1_awayas_lookup)

colnames(b1_awayas_lookup) <- c("AwayTeam_b1","b1_awayas")


B1_fixtures$b1_awayas <- sqldf("SELECT b1_awayas_lookup.b1_awayas FROM b1_awayas_lookup INNER JOIN B1_fixtures ON b1_awayas_lookup.AwayTeam_b1 = B1_fixtures.AwayTeam_b1")

B1_fixtures$b1_homeds <- rep(b1_home_ds,each = length(b1_teams)-1)

B1_fixtures$b1_awayds <- as.numeric(unlist(B1_fixtures$b1_awayds))
#xGH
B1_fixtures$b1_xGH <- B1_fixtures$avg_HG_b1 * B1_fixtures$b1_homeas * B1_fixtures$b1_awayds

#xGA

B1_fixtures$b1_awayas <- as.numeric(unlist(B1_fixtures$b1_awayas))

B1_fixtures$b1_xGA <- B1_fixtures$avg_AG_b1 * B1_fixtures$b1_awayas * B1_fixtures$b1_homeds

B1_fixtures$b1_0_0 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_0 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_1 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_1 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_0 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_2 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_2 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_1 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_2 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_3 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_0 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_1 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_2 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_3 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_3 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_3 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_4 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_0 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_1 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_2 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_3 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_4 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_4 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_4 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_4 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_5 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_0 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_1 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_2 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_3 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_4 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_5 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_5 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_5 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_5 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_5 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_6 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_0 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(0,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_1 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(1,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_2 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(2,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_3 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(3,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_4 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(4,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_6_5 <- round(stats::dpois(6,B1_fixtures$b1_xGH) * stats::dpois(5,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_0_6 <- round(stats::dpois(0,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_1_6 <- round(stats::dpois(1,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_2_6 <- round(stats::dpois(2,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_3_6 <- round(stats::dpois(3,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_4_6 <- round(stats::dpois(4,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
B1_fixtures$b1_5_6 <- round(stats::dpois(5,B1_fixtures$b1_xGH) * stats::dpois(6,B1_fixtures$b1_xGA), digits = 4)
#Home win
B1_fixtures$b1_H <- (
  B1_fixtures$b1_1_0 + B1_fixtures$b1_2_0 + B1_fixtures$b1_2_1 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
  B1_fixtures$b1_3_2 + B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 +
  B1_fixtures$b1_5_0 + B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 +
  B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
  B1_fixtures$b1_6_5
)

B1_fixtures$b1_H <- percent(B1_fixtures$b1_H, accuracy = 0.1)

#Draw
B1_fixtures$b1_D <- (

  B1_fixtures$b1_0_0 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_3 + B1_fixtures$b1_4_4 +
  B1_fixtures$b1_5_5 + B1_fixtures$b1_6_6
)

B1_fixtures$b1_D <- percent(B1_fixtures$b1_D, accuracy = 0.1)

#Away

B1_fixtures$b1_A <- (
  B1_fixtures$b1_0_1 + B1_fixtures$b1_0_2 + B1_fixtures$b1_1_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 +
    B1_fixtures$b1_2_3 + B1_fixtures$b1_0_4 + B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 +
    B1_fixtures$b1_0_5 + B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 +
    B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 + B1_fixtures$b1_4_6 +
    B1_fixtures$b1_5_6
)

B1_fixtures$b1_A <- percent(B1_fixtures$b1_A, accuracy = 0.1)

#ov25

B1_fixtures$b1_ov25 <- (
  B1_fixtures$b1_2_1 + B1_fixtures$b1_1_2 + B1_fixtures$b1_2_2 + B1_fixtures$b1_3_0 + B1_fixtures$b1_3_1 +
  B1_fixtures$b1_3_2 + B1_fixtures$b1_0_3 + B1_fixtures$b1_1_3 + B1_fixtures$b1_2_3 + B1_fixtures$b1_3_3 +
  B1_fixtures$b1_4_0 + B1_fixtures$b1_4_1 + B1_fixtures$b1_4_2 + B1_fixtures$b1_4_3 + B1_fixtures$b1_0_4 +
  B1_fixtures$b1_1_4 + B1_fixtures$b1_2_4 + B1_fixtures$b1_3_4 + B1_fixtures$b1_4_4 + B1_fixtures$b1_5_0 +
  B1_fixtures$b1_5_1 + B1_fixtures$b1_5_2 + B1_fixtures$b1_5_3 + B1_fixtures$b1_5_4 + B1_fixtures$b1_0_5 +
  B1_fixtures$b1_1_5 + B1_fixtures$b1_2_5 + B1_fixtures$b1_3_5 + B1_fixtures$b1_4_5 + B1_fixtures$b1_5_5 +
  B1_fixtures$b1_6_0 + B1_fixtures$b1_6_1 + B1_fixtures$b1_6_2 + B1_fixtures$b1_6_3 + B1_fixtures$b1_6_4 +
  B1_fixtures$b1_6_5 + B1_fixtures$b1_0_6 + B1_fixtures$b1_1_6 + B1_fixtures$b1_2_6 + B1_fixtures$b1_3_6 +
  B1_fixtures$b1_4_6 + B1_fixtures$b1_5_6 + B1_fixtures$b1_6_6
  )
B1_fixtures$b1_ov25 <- percent(B1_fixtures$b1_ov25, accuracy = 0.1)
#un25
B1_fixtures$b1_un25 <- (
  B1_fixtures$b1_0_0 + B1_fixtures$b1_1_0 + B1_fixtures$b1_0_1 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_0 + B1_fixtures$b1_0_2
)

B1_fixtures$b1_un25 <- percent(B1_fixtures$b1_un25, accuracy = 0.1)
#######################################################################################################################
#D1
HomeTeam_d1 <- rep(d1_teams, each = length(d1_teams))
AwayTeam_d1 <- rep(d1_teams, length(d1_teams))
D1_fixtures <- cbind(HomeTeam_d1,AwayTeam_d1)
D1_fixtures <- as.data.frame(D1_fixtures)
D1_fixtures <- D1_fixtures[!D1_fixtures$HomeTeam_d1 == D1_fixtures$AwayTeam_d1,]
rownames(D1_fixtures) <- NULL
D1_fixtures$Div <- "D1"
D1_fixtures <- D1_fixtures[,c(3,1,2)]

D1_fixtures$Date

D1_fixtures$Date <- sqldf("SELECT D1.Date from D1 LEFT OUTER JOIN D1_fixtures ON D1.Hometeam = D1_fixtures.HomeTeam_d1 AND D1.AwayTeam = D1_fixtures.AwayTeam_d1")

D1_fixtures$avg_HG_d1 <- d1_avg_HG

D1_fixtures$d1_homeas <- rep(d1_home_as,each = length(d1_teams)-1)

d1_awayds_lookup <- cbind(d1_teams,d1_away_ds)

d1_awayds_lookup <- as.data.frame(d1_awayds_lookup)

colnames(d1_awayds_lookup) <- c("AwayTeam_d1","d1_awayds")


require('RH2')
D1_fixtures$d1_awayds <- sqldf("SELECT d1_awayds_lookup.d1_awayds FROM d1_awayds_lookup INNER JOIN D1_fixtures ON d1_awayds_lookup.AwayTeam_d1 = D1_fixtures.AwayTeam_d1")

D1_fixtures$avg_AG_d1 <- d1_avg_AG

d1_awayas_lookup <- cbind(d1_teams,d1_away_as)

d1_awayas_lookup <- as.data.frame(d1_awayas_lookup)

colnames(d1_awayas_lookup) <- c("AwayTeam_d1","d1_awayas")


D1_fixtures$d1_awayas <- sqldf("SELECT d1_awayas_lookup.d1_awayas FROM d1_awayas_lookup INNER JOIN D1_fixtures ON d1_awayas_lookup.AwayTeam_d1 = D1_fixtures.AwayTeam_d1")

D1_fixtures$d1_homeds <- rep(d1_home_ds,each = length(d1_teams)-1)

D1_fixtures$d1_awayds <- as.numeric(unlist(D1_fixtures$d1_awayds))
#xGH
D1_fixtures$d1_xGH <- D1_fixtures$avg_HG_d1 * D1_fixtures$d1_homeas * D1_fixtures$d1_awayds

#xGA

D1_fixtures$d1_awayas <- as.numeric(unlist(D1_fixtures$d1_awayas))

D1_fixtures$d1_xGA <- D1_fixtures$avg_AG_d1 * D1_fixtures$d1_awayas * D1_fixtures$d1_homeds

D1_fixtures$d1_0_0 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_0 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_0_1 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_1 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_0 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_0_2 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_2 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_1 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_2 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_3 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_0 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_1 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_2 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_0_3 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_3 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_3 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_4 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_0 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_1 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_2 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_3 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_0_4 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_4 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_4 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_4 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_5 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_0 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_1 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_2 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_3 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_4 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_0_5 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_5 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_5 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_5 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_5 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_6 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_0 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_1 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_2 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_3 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_4 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_6_5 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_0_6 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_1_6 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_2_6 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_3_6 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_4_6 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
D1_fixtures$d1_5_6 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#Home win
D1_fixtures$d1_H <- (
  D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
    D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
    D1_fixtures$d1_5_0 + D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
    D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
    D1_fixtures$d1_6_5
)

D1_fixtures$d1_H <- percent(D1_fixtures$d1_H, accuracy = 0.1)

#Draw
D1_fixtures$d1_D <- (

  D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 + D1_fixtures$d1_4_4 +
    D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6
)

D1_fixtures$d1_D <- percent(D1_fixtures$d1_D, accuracy = 0.1)

#Away

D1_fixtures$d1_A <- (
  D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
    D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
    D1_fixtures$d1_0_5 + D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
    D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
    D1_fixtures$d1_5_6
)

D1_fixtures$d1_A <- percent(D1_fixtures$d1_A, accuracy = 0.1)

#ov25

D1_fixtures$d1_ov25 <- (
  D1_fixtures$d1_2_1 + D1_fixtures$d1_1_2 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
    D1_fixtures$d1_3_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 + D1_fixtures$d1_2_3 + D1_fixtures$d1_3_3 +
    D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 + D1_fixtures$d1_0_4 +
    D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 + D1_fixtures$d1_4_4 + D1_fixtures$d1_5_0 +
    D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 + D1_fixtures$d1_0_5 +
    D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 + D1_fixtures$d1_5_5 +
    D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
    D1_fixtures$d1_6_5 + D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 +
    D1_fixtures$d1_4_6 + D1_fixtures$d1_5_6 + D1_fixtures$d1_6_6
)
D1_fixtures$d1_ov25 <- percent(D1_fixtures$d1_ov25, accuracy = 0.1)
#un25
D1_fixtures$d1_un25 <- (
  D1_fixtures$d1_0_0 + D1_fixtures$d1_1_0 + D1_fixtures$d1_0_1 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_0 + D1_fixtures$d1_0_2
)

D1_fixtures$d1_un25 <- percent(D1_fixtures$d1_un25, accuracy = 0.1)

D1_fixtures






