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
##################################################################
#B1
HomeTeam_b1 <- rep(b1_teams, each = length(b1_teams))
AwayTeam_b1 <- rep(b1_teams, length(b1_teams))
B1_fixtures <- cbind(HomeTeam_b1,AwayTeam_b1)
B1_fixtures <- as.data.frame(B1_fixtures)
B1_fixtures <- B1_fixtures[!B1_fixtures$HomeTeam_b1 == B1_fixtures$AwayTeam_b1,]
rownames(B1_fixtures) <- NULL
B1_fixtures$Div <- "B1"
B1_fixtures <- B1_fixtures[,c(3,1,2)]

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
#un25
B1_fixtures$b1_un25 <- (
  B1_fixtures$b1_0_0 + B1_fixtures$b1_1_0 + B1_fixtures$b1_0_1 + B1_fixtures$b1_1_1 + B1_fixtures$b1_2_0 + B1_fixtures$b1_0_2
)
#odds
B1_fixtures$b1_ov25_odds <- round((1/B1_fixtures$b1_ov25),digits = 2)
B1_fixtures$b1_un25_odds <- round((1/B1_fixtures$b1_un25),digits = 2)

B1_fixtures$b1_ov25_odds
B1_fixtures$b1_un25_odds
#percentages
B1_fixtures$b1_ov25 <- percent(B1_fixtures$b1_ov25, accuracy = 0.1)

B1_fixtures$b1_un25 <- percent(B1_fixtures$b1_un25, accuracy = 0.1)
B1_fixtures$b1_pscore <- paste(round(B1_fixtures$b1_xGH,digits = 0),round(B1_fixtures$b1_xGA,digits = 0),sep = "-")
#write out
write.xlsx(B1_fixtures,'Divisions/LeagueFixtures.xlsx',sheetName = "B1")



















