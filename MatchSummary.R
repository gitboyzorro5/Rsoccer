#by gitboyzorro5
#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

###B1
b1_match_results <- fb_match_results(country = "BEL", gender = "M", season_end_year = 2024, tier="1st")
unlink('b1_match_results.xlsx')
write.xlsx(b1_match_results,"b1_match_results.xlsx")
b1_urls <- fb_match_urls(country = "BEL", gender = "M", season_end_year = 2024, tier="1st")
unlink('b1_urls.xlsx')
write.xlsx(b1_urls,"b1_urls.xlsx")

b1_match_sumary <- fb_match_summary(match_url = b1_urls)
unlink('b1_match_summary.xlsx')
write.xlsx(b1_match_sumary,"b1_match_summary.xlsx")

b1_summary <- readxl::read_excel('b1_match_summary.xlsx')
b1_summary <- b1_summary[,c(-1)]

b1_summary$Home_Team <- mgsub(b1_summary$Home_Team,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
b1_summary$Away_Team <- mgsub(b1_summary$Away_Team,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))



b1_summary$matchid <- paste(b1_summary$Match_Date,b1_summary$Home_Team,b1_summary$Away_Team,sep = "-")
B1_spread <- subset(allteams20232024,Div =="B1")
B1_spread$matchid <- paste(B1_spread$Date,B1_spread$HomeTeam,B1_spread$AwayTeam,sep = "-")
#referees
B1_referees <- fb_match_results(country = "BEL", gender = "M", season_end_year = 2024, tier="1st")
B1_referees <- B1_referees[,c(8,10,13,18)]

#rename column names
names(B1_referees)[2] <- paste("HomeTeam")
names(B1_referees)[3] <- paste("AwayTeam")

B1_referees$HomeTeam <- mgsub(B1_referees$HomeTeam,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
B1_referees$AwayTeam <- mgsub(B1_referees$AwayTeam,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
B1_referees$matchid <- paste(B1_referees$Date,B1_referees$HomeTeam,B1_referees$AwayTeam,sep = "-")
B1_spread <- dplyr::left_join(B1_spread,B1_referees)

B1_spread$n <- B1_spread$TG
View(B1_spread)
library('sqldf')
require('RH2')

Home_xG <- c()
Home_xG <- sqldf("SELECT b1_summary.matchid,b1_summary.Home_xG FROM b1_summary INNER JOIN B1_spread ON b1_summary.matchid = B1_spread.matchid GROUP BY b1_summary.matchid")
B1_spread <- dplyr::left_join(B1_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT b1_summary.matchid,b1_summary.Away_xG FROM b1_summary INNER JOIN B1_spread ON b1_summary.matchid = B1_spread.matchid GROUP BY b1_summary.matchid")
B1_spread <- dplyr::left_join(B1_spread,Away_xG)

Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT b1_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM b1_summary INNER JOIN B1_spread ON b1_summary.matchid = B1_spread.matchid WHERE b1_summary.Event_Type = 'Goal' OR b1_summary.Event_Type = 'Penalty' GROUP BY b1_summary.matchid")
B1_spread <- dplyr::left_join(B1_spread,Total_Goalmins)
B1_spread <- B1_spread %>% replace(is.na(.),0)
#Bookings
B1_spread$Bookings <- (B1_spread$HY *10 + B1_spread$HR *25) + (B1_spread$AY*10 + B1_spread$AR*25)
#CrossBookings
B1_spread$Crossbookings <- (B1_spread$HY *10 + B1_spread$HR *25)*(B1_spread$AY*10 + B1_spread$AR*25)
#GoalsXbookings
B1_spread$GoalsXbookings <- (B1_spread$Bookings)*(B1_spread$TG)
#CornersXbookings
B1_spread$CornersXbookings <- (B1_spread$TC)*(B1_spread$Bookings)
#GoalsXCorners
B1_spread$GoalsXcorners <- (B1_spread$TG)*(B1_spread$TC)
#TGMxCorners
B1_spread$TGMXcorners <- (B1_spread$Total_Goalmins)*(B1_spread$TC)
#GoalsXcornersXbookings
B1_spread$GoalsXcornerXbookings <- (B1_spread$TG)*(B1_spread$TC)*(B1_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS FH_HYC FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Event_Half = '1' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,FH_HYC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS FH_AYC FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Event_Half = '1' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,FH_AYC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS FH_HRC FROM b1_summary WHERE b1_summary.Event_Type = 'Red Card' AND b1_summary.Event_Half = '1' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,FH_HRC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS FH_ARC FROM b1_summary WHERE b1_summary.Event_Type = 'Red Card' AND b1_summary.Event_Half = '1' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,FH_ARC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS SH_HYC FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Event_Half = '2' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,SH_HYC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS SH_AYC FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Event_Half = '2' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,SH_AYC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS SH_HRC FROM b1_summary WHERE b1_summary.Event_Type = 'Red Card' AND b1_summary.Event_Half = '2' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,SH_HRC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS SH_ARC FROM b1_summary WHERE b1_summary.Event_Type = 'Red Card' AND b1_summary.Event_Half = '2' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,SH_ARC)
B1_spread <- B1_spread %>% replace(is.na(.),0)

#firsthalf
B1_spread$FH_HomeBookings <- B1_spread$FH_HYC *10 + B1_spread$FH_HRC *25

B1_spread$FH_AwayBookings <- B1_spread$FH_AYC *10 + B1_spread$FH_ARC *25

B1_spread$FH_TotalBookings <- B1_spread$FH_HomeBookings + B1_spread$FH_AwayBookings

#second half
B1_spread$SH_HomeBookings <- B1_spread$SH_HYC *10 + B1_spread$SH_HRC *25

B1_spread$SH_AwayBookings <- B1_spread$SH_AYC *10 + B1_spread$SH_ARC *25

B1_spread$SH_TotalBookings <- B1_spread$SH_HomeBookings + B1_spread$SH_AwayBookings


B1_spread$MultiBookings <- B1_spread$FH_TotalBookings * B1_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT b1_summary.matchid,SUM(Event_time) AS Home_YCmins FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Home_YCmins)
B1_spread <- B1_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT b1_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM b1_summary WHERE b1_summary.Event_Type = 'Red Card' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Home_RCmins)
B1_spread <- B1_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT b1_summary.matchid,SUM(Event_time) AS Away_YCmins FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Away_YCmins)
B1_spread <- B1_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT b1_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM b1_summary WHERE b1_summary.Event_Type = 'Red Card' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Away_RCmins)
B1_spread <- B1_spread %>% replace(is.na(.),0)

B1_spread$Home_TotalCardmins <- B1_spread$Home_YCmins + B1_spread$Home_RCmins
B1_spread$Away_TotalCardmins <- B1_spread$Away_YCmins + B1_spread$Away_RCmins
B1_spread$match_TotalCardmins <- B1_spread$Home_TotalCardmins + B1_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT b1_summary.matchid,MIN(b1_summary.Event_Time) AS Home_first_YCTime FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Home_Away = 'Home' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Home_first_YCTime)
B1_spread <- B1_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT b1_summary.matchid,MIN(b1_summary.Event_Time) AS Away_first_YCTime FROM b1_summary WHERE b1_summary.Event_Type = 'Yellow Card' AND b1_summary.Home_Away = 'Away' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Away_first_YCTime)
B1_spread <- B1_spread %>% replace(is.na(.),0)

B1_spread$match_First_YCTime <- ifelse(B1_spread$Home_first_YCTime == '0' | B1_spread$Away_first_YCTime == '0',pmax(B1_spread$Home_first_YCTime,B1_spread$Away_first_YCTime),pmin(B1_spread$Home_first_YCTime,B1_spread$Away_first_YCTime))

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT b1_summary.matchid,COUNT(*) AS Penalty FROM b1_summary WHERE b1_summary.Event_Type = 'Penalty' GROUP BY b1_summary.matchid ")
B1_spread <- dplyr::left_join(B1_spread,Penalty)
B1_spread <- B1_spread %>% replace(is.na(.),0)
#calculate match performance
B1_spread$MatchPerfomance <- B1_spread$TG *15 + B1_spread$TY *5 + B1_spread$TR *15 + B1_spread$TC *3 + B1_spread$Penalty *10

unlink('B1_SPREAD.xlsx')
write.xlsx(B1_spread,'B1_SPREAD.xlsx')
#Referee
B1_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM B1_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('B1_refereestats.xlsx')
write.xlsx(B1_refereestats,'B1_refereestats.xlsx')
####################################################################################################################################################################################
####################################################################################################################################################################################
###D2
d2_match_results <- fb_match_results(country = "GER", gender = "M", season_end_year = 2024, tier="2nd")
unlink('d2_match_results.xlsx')
write.xlsx(d2_match_results,"d2_match_results.xlsx")
d2_urls <- fb_match_urls(country = "GER", gender = "M", season_end_year = 2024, tier="2nd")
unlink('d2_urls.xlsx')
write.xlsx(d2_urls,"d2_urls.xlsx")

d2_match_sumary <- fb_match_summary(match_url = d2_urls)
unlink('d2_match_summary.xlsx')
write.xlsx(d2_match_sumary,"d2_match_summary.xlsx")

d2_summary <- readxl::read_excel('d2_match_summary.xlsx')
d2_summary <- d2_summary[,c(-1)]
d2_teams
sort(unique(d2_summary$Home_Team))
d2_summary$Home_Team <- mgsub(d2_summary$Home_Team,c("Düsseldorf","Eintracht Braunschweig","Hamburger SV","Hannover 96","Hertha BSC","Karlsruher","Nürnberg","Osnabrück","Paderborn 07","St. Pauli","Wehen Wiesbaden","Greuther Fürth"),c("Fortuna Dusseldorf","Braunschweig","Hamburg","Hannover","Hertha","Karlsruhe","Nurnberg","Osnabruck","Paderborn","St Pauli","Wehen","Greuther Furth"))
d2_summary$Away_Team <- mgsub(d2_summary$Away_Team,c("Düsseldorf","Eintracht Braunschweig","Hamburger SV","Hannover 96","Hertha BSC","Karlsruher","Nürnberg","Osnabrück","Paderborn 07","St. Pauli","Wehen Wiesbaden","Greuther Fürth"),c("Fortuna Dusseldorf","Braunschweig","Hamburg","Hannover","Hertha","Karlsruhe","Nurnberg","Osnabruck","Paderborn","St Pauli","Wehen","Greuther Furth"))



d2_summary$matchid <- paste(d2_summary$Match_Date,d2_summary$Home_Team,d2_summary$Away_Team,sep = "-")
D2_spread <- subset(allteams20232024,Div =="D2")
D2_spread$matchid <- paste(D2_spread$Date,D2_spread$HomeTeam,D2_spread$AwayTeam,sep = "-")
#referees
D2_referees <- fb_match_results(country = "GER", gender = "M", season_end_year = 2024, tier="2nd")
D2_referees <- D2_referees[,c(8,10,13,18)]

#rename column names
names(D2_referees)[2] <- paste("HomeTeam")
names(D2_referees)[3] <- paste("AwayTeam")

D2_referees$HomeTeam <- mgsub(D2_referees$HomeTeam,c("Düsseldorf","Eintracht Braunschweig","Hamburger SV","Hannover 96","Hertha BSC","Karlsruher","Nürnberg","Osnabrück","Paderborn 07","St. Pauli","Wehen Wiesbaden","Greuther Fürth"),c("Fortuna Dusseldorf","Braunschweig","Hamburg","Hannover","Hertha","Karlsruhe","Nurnberg","Osnabruck","Paderborn","St Pauli","Wehen","Greuther Furth"))
D2_referees$AwayTeam <- mgsub(D2_referees$AwayTeam,c("Düsseldorf","Eintracht Braunschweig","Hamburger SV","Hannover 96","Hertha BSC","Karlsruher","Nürnberg","Osnabrück","Paderborn 07","St. Pauli","Wehen Wiesbaden","Greuther Fürth"),c("Fortuna Dusseldorf","Braunschweig","Hamburg","Hannover","Hertha","Karlsruhe","Nurnberg","Osnabruck","Paderborn","St Pauli","Wehen","Greuther Furth"))
D2_referees$matchid <- paste(D2_referees$Date,D2_referees$HomeTeam,D2_referees$AwayTeam,sep = "-")
D2_spread <- dplyr::left_join(D2_spread,D2_referees)

D2_spread$n <- D2_spread$TG

library('sqldf')
require('RH2')

Home_xG <- c()
Home_xG <- sqldf("SELECT d2_summary.matchid,d2_summary.Home_xG FROM d2_summary INNER JOIN D2_spread ON d2_summary.matchid = D2_spread.matchid GROUP BY d2_summary.matchid")
D2_spread <- dplyr::left_join(D2_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT d2_summary.matchid,d2_summary.Away_xG FROM d2_summary INNER JOIN D2_spread ON d2_summary.matchid = D2_spread.matchid GROUP BY d2_summary.matchid")
D2_spread <- dplyr::left_join(D2_spread,Away_xG)

Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT d2_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM d2_summary INNER JOIN D2_spread ON d2_summary.matchid = D2_spread.matchid WHERE d2_summary.Event_Type = 'Goal' OR d2_summary.Event_Type = 'Penalty' GROUP BY d2_summary.matchid")
D2_spread <- dplyr::left_join(D2_spread,Total_Goalmins)
D2_spread <- D2_spread %>% replace(is.na(.),0)
#Bookings
D2_spread$Bookings <- (D2_spread$HY *10 + D2_spread$HR *25) + (D2_spread$AY*10 + D2_spread$AR*25)
#CrossBookings
D2_spread$Crossbookings <- (D2_spread$HY *10 + D2_spread$HR *25)*(D2_spread$AY*10 + D2_spread$AR*25)
#GoalsXbookings
D2_spread$GoalsXbookings <- (D2_spread$Bookings)*(D2_spread$TG)
#CornersXbookings
D2_spread$CornersXbookings <- (D2_spread$TC)*(D2_spread$Bookings)
#GoalsXCorners
D2_spread$GoalsXcorners <- (D2_spread$TG)*(D2_spread$TC)
#TGMxCorners
D2_spread$TGMXcorners <- (D2_spread$Total_Goalmins)*(D2_spread$TC)
#GoalsXcornersXbookings
D2_spread$GoalsXcornerXbookings <- (D2_spread$TG)*(D2_spread$TC)*(D2_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS FH_HYC FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Event_Half = '1' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,FH_HYC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS FH_AYC FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Event_Half = '1' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,FH_AYC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS FH_HRC FROM d2_summary WHERE d2_summary.Event_Type = 'Red Card' AND d2_summary.Event_Half = '1' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,FH_HRC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS FH_ARC FROM d2_summary WHERE d2_summary.Event_Type = 'Red Card' AND d2_summary.Event_Half = '1' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,FH_ARC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS SH_HYC FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Event_Half = '2' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,SH_HYC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS SH_AYC FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Event_Half = '2' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,SH_AYC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS SH_HRC FROM d2_summary WHERE d2_summary.Event_Type = 'Red Card' AND d2_summary.Event_Half = '2' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,SH_HRC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS SH_ARC FROM d2_summary WHERE d2_summary.Event_Type = 'Red Card' AND d2_summary.Event_Half = '2' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,SH_ARC)
D2_spread <- D2_spread %>% replace(is.na(.),0)

#firsthalf
D2_spread$FH_HomeBookings <- D2_spread$FH_HYC *10 + D2_spread$FH_HRC *25

D2_spread$FH_AwayBookings <- D2_spread$FH_AYC *10 + D2_spread$FH_ARC *25

D2_spread$FH_TotalBookings <- D2_spread$FH_HomeBookings + D2_spread$FH_AwayBookings

#second half
D2_spread$SH_HomeBookings <- D2_spread$SH_HYC *10 + D2_spread$SH_HRC *25

D2_spread$SH_AwayBookings <- D2_spread$SH_AYC *10 + D2_spread$SH_ARC *25

D2_spread$SH_TotalBookings <- D2_spread$SH_HomeBookings + D2_spread$SH_AwayBookings


D2_spread$MultiBookings <- D2_spread$FH_TotalBookings * D2_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT d2_summary.matchid,SUM(Event_time) AS Home_YCmins FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Home_YCmins)
D2_spread <- D2_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT d2_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM d2_summary WHERE d2_summary.Event_Type = 'Red Card' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Home_RCmins)
D2_spread <- D2_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT d2_summary.matchid,SUM(Event_time) AS Away_YCmins FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Away_YCmins)
D2_spread <- D2_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT d2_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM d2_summary WHERE d2_summary.Event_Type = 'Red Card' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Away_RCmins)
D2_spread <- D2_spread %>% replace(is.na(.),0)

D2_spread$Home_TotalCardmins <- D2_spread$Home_YCmins + D2_spread$Home_RCmins
D2_spread$Away_TotalCardmins <- D2_spread$Away_YCmins + D2_spread$Away_RCmins
D2_spread$match_TotalCardmins <- D2_spread$Home_TotalCardmins + D2_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT d2_summary.matchid,MIN(d2_summary.Event_Time) AS Home_first_YCTime FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Home_Away = 'Home' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Home_first_YCTime)
D2_spread <- D2_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT d2_summary.matchid,MIN(d2_summary.Event_Time) AS Away_first_YCTime FROM d2_summary WHERE d2_summary.Event_Type = 'Yellow Card' AND d2_summary.Home_Away = 'Away' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Away_first_YCTime)
D2_spread <- D2_spread %>% replace(is.na(.),0)

D2_spread$match_First_YCTime <- ifelse(D2_spread$Home_first_YCTime == '0' | D2_spread$Away_first_YCTime == '0',pmax(D2_spread$Home_first_YCTime,D2_spread$Away_first_YCTime),pmin(D2_spread$Home_first_YCTime,D2_spread$Away_first_YCTime))

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT d2_summary.matchid,COUNT(*) AS Penalty FROM d2_summary WHERE d2_summary.Event_Type = 'Penalty' GROUP BY d2_summary.matchid ")
D2_spread <- dplyr::left_join(D2_spread,Penalty)
D2_spread <- D2_spread %>% replace(is.na(.),0)
#calculate match performance
D2_spread$MatchPerfomance <- D2_spread$TG *15 + D2_spread$TY *5 + D2_spread$TR *15 + D2_spread$TC *3 + D2_spread$Penalty *10

unlink('D2_SPREAD.xlsx')
write.xlsx(D2_spread,'D2_SPREAD.xlsx')
#Referee
D2_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM D2_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('D2_refereestats.xlsx')
write.xlsx(D2_refereestats,'D2_refereestats.xlsx')
###########################################################################################################################################################
###########################################################################################################################################################
###E1
e1_match_results <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier="2nd")
unlink('e1_match_results.xlsx')
write.xlsx(e1_match_results,"e1_match_results.xlsx")
e1_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="2nd")
unlink('e1_urls.xlsx')
write.xlsx(e1_urls,"e1_urls.xlsx")

e1_match_sumary <- fb_match_summary(match_url = e1_urls)
unlink('e1_match_summary.xlsx')
write.xlsx(e1_match_sumary,"e1_match_summary.xlsx")

e1_summary <- readxl::read_excel('e1_match_summary.xlsx')
e1_summary <- e1_summary[,c(-1)]
sort(unique(e1_summary$Home_Team))
e1_teams
e1_summary$Home_Team <- mgsub(e1_summary$Home_Team,c("Cardiff City","Coventry City","Derby County","Hull City","Leeds United","Luton Town","Norwich City","Oxford United","Plymouth Argyle","Sheffield Utd","Stoke City","Swansea City","Birmingham City","Blackburn Rovers","Huddersfield Town","Ipswich Town","Leicester City","Preston North End","Queens Park Rangers","Rotherham United","Sheffield Wednesday","West Bromwich Albion"),c("Cardiff","Coventry","Derby","Hull","Leeds","Luton","Norwich","Oxford","Plymouth","Sheffield United","Stoke","Swansea","Birmingham","Blackburn","Huddersfield","Ipswich","Leicester","Preston","QPR","Rotherham","Sheffield Weds","West Brom"))
e1_summary$Away_Team <- mgsub(e1_summary$Away_Team,c("Cardiff City","Coventry City","Derby County","Hull City","Leeds United","Luton Town","Norwich City","Oxford United","Plymouth Argyle","Sheffield Utd","Stoke City","Swansea City","Birmingham City","Blackburn Rovers","Huddersfield Town","Ipswich Town","Leicester City","Preston North End","Queens Park Rangers","Rotherham United","Sheffield Wednesday","West Bromwich Albion"),c("Cardiff","Coventry","Derby","Hull","Leeds","Luton","Norwich","Oxford","Plymouth","Sheffield United","Stoke","Swansea","Birmingham","Blackburn","Huddersfield","Ipswich","Leicester","Preston","QPR","Rotherham","Sheffield Weds","West Brom"))



e1_summary$matchid <- paste(e1_summary$Match_Date,e1_summary$Home_Team,e1_summary$Away_Team,sep = "-")
E1_spread <- subset(allteams20232024,Div =="E1")
E1_spread$matchid <- paste(E1_spread$Date,E1_spread$HomeTeam,E1_spread$AwayTeam,sep = "-")
#referees
E1_referees <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier="2nd")
E1_referees <- E1_referees[,c(8,10,13,18)]
View(E1_referees)
#rename column names
names(E1_referees)[2] <- paste("HomeTeam")
names(E1_referees)[3] <- paste("AwayTeam")

E1_referees$HomeTeam <- mgsub(E1_referees$HomeTeam,c("Cardiff City","Coventry City","Derby County","Hull City","Leeds United","Luton Town","Norwich City","Oxford United","Plymouth Argyle","Sheffield Utd","Stoke City","Swansea City","Birmingham City","Blackburn Rovers","Huddersfield Town","Ipswich Town","Leicester City","Preston North End","Queens Park Rangers","Rotherham United","Sheffield Wednesday","West Bromwich Albion","Rotherham Utd"),c("Cardiff","Coventry","Derby","Hull","Leeds","Luton","Norwich","Oxford","Plymouth","Sheffield United","Stoke","Swansea","Birmingham","Blackburn","Huddersfield","Ipswich","Leicester","Preston","QPR","Rotherham","Sheffield Weds","West Brom","Rotherham"))
E1_referees$AwayTeam <- mgsub(E1_referees$AwayTeam,c("Cardiff City","Coventry City","Derby County","Hull City","Leeds United","Luton Town","Norwich City","Oxford United","Plymouth Argyle","Sheffield Utd","Stoke City","Swansea City","Birmingham City","Blackburn Rovers","Huddersfield Town","Ipswich Town","Leicester City","Preston North End","Queens Park Rangers","Rotherham United","Sheffield Wednesday","West Bromwich Albion","Rotherham Utd"),c("Cardiff","Coventry","Derby","Hull","Leeds","Luton","Norwich","Oxford","Plymouth","Sheffield United","Stoke","Swansea","Birmingham","Blackburn","Huddersfield","Ipswich","Leicester","Preston","QPR","Rotherham","Sheffield Weds","West Brom","Rotherham"))
E1_referees$matchid <- paste(E1_referees$Date,E1_referees$HomeTeam,E1_referees$AwayTeam,sep = "-")
E1_spread <- dplyr::left_join(E1_spread,E1_referees)

E1_spread$n <- E1_spread$TG

library('sqldf')
require('RH2')

Home_xG <- c()
Home_xG <- sqldf("SELECT e1_summary.matchid,e1_summary.Home_xG FROM e1_summary INNER JOIN E1_spread ON e1_summary.matchid = E1_spread.matchid GROUP BY e1_summary.matchid")
E1_spread <- dplyr::left_join(E1_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT e1_summary.matchid,e1_summary.Away_xG FROM e1_summary INNER JOIN E1_spread ON e1_summary.matchid = E1_spread.matchid GROUP BY e1_summary.matchid")
E1_spread <- dplyr::left_join(E1_spread,Away_xG)

Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT e1_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM e1_summary INNER JOIN E1_spread ON e1_summary.matchid = E1_spread.matchid WHERE e1_summary.Event_Type = 'Goal' OR e1_summary.Event_Type = 'Penalty' GROUP BY e1_summary.matchid")
E1_spread <- dplyr::left_join(E1_spread,Total_Goalmins)
E1_spread <- E1_spread %>% replace(is.na(.),0)
#Bookings
E1_spread$Bookings <- (E1_spread$HY *10 + E1_spread$HR *25) + (E1_spread$AY*10 + E1_spread$AR*25)
#CrossBookings
E1_spread$Crossbookings <- (E1_spread$HY *10 + E1_spread$HR *25)*(E1_spread$AY*10 + E1_spread$AR*25)
#GoalsXbookings
E1_spread$GoalsXbookings <- (E1_spread$Bookings)*(E1_spread$TG)
#CornersXbookings
E1_spread$CornersXbookings <- (E1_spread$TC)*(E1_spread$Bookings)
#GoalsXCorners
E1_spread$GoalsXcorners <- (E1_spread$TG)*(E1_spread$TC)
#TGMxCorners
E1_spread$TGMXcorners <- (E1_spread$Total_Goalmins)*(E1_spread$TC)
#GoalsXcornersXbookings
E1_spread$GoalsXcornerXbookings <- (E1_spread$TG)*(E1_spread$TC)*(E1_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS FH_HYC FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Event_Half = '1' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,FH_HYC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS FH_AYC FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Event_Half = '1' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,FH_AYC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS FH_HRC FROM e1_summary WHERE e1_summary.Event_Type = 'Red Card' AND e1_summary.Event_Half = '1' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,FH_HRC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS FH_ARC FROM e1_summary WHERE e1_summary.Event_Type = 'Red Card' AND e1_summary.Event_Half = '1' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,FH_ARC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS SH_HYC FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Event_Half = '2' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,SH_HYC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS SH_AYC FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Event_Half = '2' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,SH_AYC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS SH_HRC FROM e1_summary WHERE e1_summary.Event_Type = 'Red Card' AND e1_summary.Event_Half = '2' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,SH_HRC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS SH_ARC FROM e1_summary WHERE e1_summary.Event_Type = 'Red Card' AND e1_summary.Event_Half = '2' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,SH_ARC)
E1_spread <- E1_spread %>% replace(is.na(.),0)

#firsthalf
E1_spread$FH_HomeBookings <- E1_spread$FH_HYC *10 + E1_spread$FH_HRC *25

E1_spread$FH_AwayBookings <- E1_spread$FH_AYC *10 + E1_spread$FH_ARC *25

E1_spread$FH_TotalBookings <- E1_spread$FH_HomeBookings + E1_spread$FH_AwayBookings

#second half
E1_spread$SH_HomeBookings <- E1_spread$SH_HYC *10 + E1_spread$SH_HRC *25

E1_spread$SH_AwayBookings <- E1_spread$SH_AYC *10 + E1_spread$SH_ARC *25

E1_spread$SH_TotalBookings <- E1_spread$SH_HomeBookings + E1_spread$SH_AwayBookings


E1_spread$MultiBookings <- E1_spread$FH_TotalBookings * E1_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT e1_summary.matchid,SUM(Event_time) AS Home_YCmins FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Home_YCmins)
E1_spread <- E1_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT e1_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM e1_summary WHERE e1_summary.Event_Type = 'Red Card' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Home_RCmins)
E1_spread <- E1_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT e1_summary.matchid,SUM(Event_time) AS Away_YCmins FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Away_YCmins)
E1_spread <- E1_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT e1_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM e1_summary WHERE e1_summary.Event_Type = 'Red Card' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Away_RCmins)
E1_spread <- E1_spread %>% replace(is.na(.),0)

E1_spread$Home_TotalCardmins <- E1_spread$Home_YCmins + E1_spread$Home_RCmins
E1_spread$Away_TotalCardmins <- E1_spread$Away_YCmins + E1_spread$Away_RCmins
E1_spread$match_TotalCardmins <- E1_spread$Home_TotalCardmins + E1_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT e1_summary.matchid,MIN(e1_summary.Event_Time) AS Home_first_YCTime FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Home_Away = 'Home' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Home_first_YCTime)
E1_spread <- E1_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT e1_summary.matchid,MIN(e1_summary.Event_Time) AS Away_first_YCTime FROM e1_summary WHERE e1_summary.Event_Type = 'Yellow Card' AND e1_summary.Home_Away = 'Away' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Away_first_YCTime)
E1_spread <- E1_spread %>% replace(is.na(.),0)

E1_spread$match_First_YCTime <- ifelse(E1_spread$Home_first_YCTime == '0' | E1_spread$Away_first_YCTime == '0',pmax(E1_spread$Home_first_YCTime,E1_spread$Away_first_YCTime),pmin(E1_spread$Home_first_YCTime,E1_spread$Away_first_YCTime))

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT e1_summary.matchid,COUNT(*) AS Penalty FROM e1_summary WHERE e1_summary.Event_Type = 'Penalty' GROUP BY e1_summary.matchid ")
E1_spread <- dplyr::left_join(E1_spread,Penalty)
E1_spread <- E1_spread %>% replace(is.na(.),0)
#calculate match performance
E1_spread$MatchPerfomance <- E1_spread$TG *15 + E1_spread$TY *5 + E1_spread$TR *15 + E1_spread$TC *3 + E1_spread$Penalty *10

unlink('E1_SPREAD.xlsx')
write.xlsx(E1_spread,'E1_SPREAD.xlsx')
#Referee
E1_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM E1_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('E1_refereestats.xlsx')
write.xlsx(E1_refereestats,'E1_refereestats.xlsx')
###############################################################################################################################################################################
#######
#P1   #
#######
# p1_match_results <- fb_match_results(country = "POR", gender = "M", season_end_year = 2024, tier="1st")
# unlink('p1_match_results.xlsx')
# write.xlsx(p1_match_results,"p1_match_results.xlsx")
# p1_urls <- fb_match_urls(country = "POR", gender = "M", season_end_year = 2024, tier="1st")
# unlink('p1_urls.xlsx')
# write.xlsx(p1_urls,"p1_urls.xlsx")
#
# p1_match_sumary <- fb_match_summary(match_url = p1_urls)
# unlink('p1_match_summary.xlsx')
# write.xlsx(p1_match_sumary,"p1_match_summary.xlsx")
#
# p1_summary <- readxl::read_excel('p1_match_summary.xlsx')
# p1_summary <- p1_summary[,c(-1)]
#
# p1_summary$Home_Team <- mgsub(p1_summary$Home_Team,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
# p1_summary$Away_Team <- mgsub(p1_summary$Away_Team,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
#
#
#
# p1_summary$matchid <- paste(p1_summary$Match_Date,p1_summary$Home_Team,p1_summary$Away_Team,sep = "-")
# P1_spread <- subset(allteams20232024,Div =="P1")
# P1_spread$matchid <- paste(P1_spread$Date,P1_spread$HomeTeam,P1_spread$AwayTeam,sep = "-")
# #referees
# P1_referees <- fb_match_results(country = "POR", gender = "M", season_end_year = 2024, tier="1st")
# P1_referees <- P1_referees[,c(8,10,13,18)]
#
# #rename column names
# names(P1_referees)[2] <- paste("HomeTeam")
# names(P1_referees)[3] <- paste("AwayTeam")
#
# P1_referees$HomeTeam <- mgsub(P1_referees$HomeTeam,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
# P1_referees$AwayTeam <- mgsub(P1_referees$AwayTeam,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
# P1_referees$matchid <- paste(P1_referees$Date,P1_referees$HomeTeam,P1_referees$AwayTeam,sep = "-")
# P1_spread <- dplyr::left_join(P1_spread,P1_referees)
#
# P1_spread$n <- P1_spread$TG
# View(P1_spread)
# library('sqldf')
# require('RH2')
#
# Home_xG <- c()
# Home_xG <- sqldf("SELECT p1_summary.matchid,p1_summary.Home_xG FROM p1_summary INNER JOIN P1_spread ON p1_summary.matchid = P1_spread.matchid GROUP BY p1_summary.matchid")
# P1_spread <- dplyr::left_join(P1_spread,Home_xG)
#
# Away_xG <- c()
# Away_xG <- sqldf("SELECT p1_summary.matchid,p1_summary.Away_xG FROM p1_summary INNER JOIN P1_spread ON p1_summary.matchid = P1_spread.matchid GROUP BY p1_summary.matchid")
# P1_spread <- dplyr::left_join(P1_spread,Away_xG)
#
# Total_Goalmins <- c()
# Total_Goalmins <- sqldf("SELECT p1_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM p1_summary INNER JOIN P1_spread ON p1_summary.matchid = P1_spread.matchid WHERE p1_summary.Event_Type = 'Goal' OR p1_summary.Event_Type = 'Penalty' GROUP BY p1_summary.matchid")
# P1_spread <- dplyr::left_join(P1_spread,Total_Goalmins)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
# #Bookings
# P1_spread$Bookings <- (P1_spread$HY *10 + P1_spread$HR *25) + (P1_spread$AY*10 + P1_spread$AR*25)
# #CrossBookings
# P1_spread$Crossbookings <- (P1_spread$HY *10 + P1_spread$HR *25)*(P1_spread$AY*10 + P1_spread$AR*25)
# #GoalsXbookings
# P1_spread$GoalsXbookings <- (P1_spread$Bookings)*(P1_spread$TG)
# #CornersXbookings
# P1_spread$CornersXbookings <- (P1_spread$TC)*(P1_spread$Bookings)
# #GoalsXCorners
# P1_spread$GoalsXcorners <- (P1_spread$TG)*(P1_spread$TC)
# #TGMxCorners
# P1_spread$TGMXcorners <- (P1_spread$Total_Goalmins)*(P1_spread$TC)
# #GoalsXcornersXbookings
# P1_spread$GoalsXcornerXbookings <- (P1_spread$TG)*(P1_spread$TC)*(P1_spread$Bookings)
#
# #first half
# FH_HYC <- c()
# FH_HYC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS FH_HYC FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Event_Half = '1' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,FH_HYC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# FH_AYC <- c()
# FH_AYC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS FH_AYC FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Event_Half = '1' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,FH_AYC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# FH_HRC <- c()
# FH_HRC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS FH_HRC FROM p1_summary WHERE p1_summary.Event_Type = 'Red Card' AND p1_summary.Event_Half = '1' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,FH_HRC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# FH_ARC <- c()
# FH_ARC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS FH_ARC FROM p1_summary WHERE p1_summary.Event_Type = 'Red Card' AND p1_summary.Event_Half = '1' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,FH_ARC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# #second half
# SH_HYC <- c()
# SH_HYC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS SH_HYC FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Event_Half = '2' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,SH_HYC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# SH_AYC <- c()
# SH_AYC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS SH_AYC FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Event_Half = '2' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,SH_AYC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# SH_HRC <- c()
# SH_HRC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS SH_HRC FROM p1_summary WHERE p1_summary.Event_Type = 'Red Card' AND p1_summary.Event_Half = '2' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,SH_HRC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# SH_ARC <- c()
# SH_ARC <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS SH_ARC FROM p1_summary WHERE p1_summary.Event_Type = 'Red Card' AND p1_summary.Event_Half = '2' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,SH_ARC)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# #firsthalf
# P1_spread$FH_HomeBookings <- P1_spread$FH_HYC *10 + P1_spread$FH_HRC *25
#
# P1_spread$FH_AwayBookings <- P1_spread$FH_AYC *10 + P1_spread$FH_ARC *25
#
# P1_spread$FH_TotalBookings <- P1_spread$FH_HomeBookings + P1_spread$FH_AwayBookings
#
# #second half
# P1_spread$SH_HomeBookings <- P1_spread$SH_HYC *10 + P1_spread$SH_HRC *25
#
# P1_spread$SH_AwayBookings <- P1_spread$SH_AYC *10 + P1_spread$SH_ARC *25
#
# P1_spread$SH_TotalBookings <- P1_spread$SH_HomeBookings + P1_spread$SH_AwayBookings
#
#
# P1_spread$MultiBookings <- P1_spread$FH_TotalBookings * P1_spread$SH_TotalBookings
#
#
#
# Home_YCmins <- c()
# Home_YCmins <- sqldf("SELECT p1_summary.matchid,SUM(Event_time) AS Home_YCmins FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Home_YCmins)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# Home_RCmins <- c()
# Home_RCmins <- sqldf("SELECT p1_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM p1_summary WHERE p1_summary.Event_Type = 'Red Card' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Home_RCmins)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# Away_YCmins <- c()
# Away_YCmins <- sqldf("SELECT p1_summary.matchid,SUM(Event_time) AS Away_YCmins FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Away_YCmins)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# Away_RCmins <- c()
# Away_RCmins <- sqldf("SELECT p1_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM p1_summary WHERE p1_summary.Event_Type = 'Red Card' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Away_RCmins)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# P1_spread$Home_TotalCardmins <- P1_spread$Home_YCmins + P1_spread$Home_RCmins
# P1_spread$Away_TotalCardmins <- P1_spread$Away_YCmins + P1_spread$Away_RCmins
# P1_spread$match_TotalCardmins <- P1_spread$Home_TotalCardmins + P1_spread$Away_TotalCardmins
#
# Home_first_YCTime <- c()
# Home_first_YCTime <- sqldf("SELECT p1_summary.matchid,MIN(p1_summary.Event_Time) AS Home_first_YCTime FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Home_Away = 'Home' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Home_first_YCTime)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# Away_first_YCTime <- c()
# Away_first_YCTime <- sqldf("SELECT p1_summary.matchid,MIN(p1_summary.Event_Time) AS Away_first_YCTime FROM p1_summary WHERE p1_summary.Event_Type = 'Yellow Card' AND p1_summary.Home_Away = 'Away' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Away_first_YCTime)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
#
# P1_spread$match_First_YCTime <- pmin(P1_spread$Home_first_YCTime,P1_spread$Away_first_YCTime)
#
# #count number of penalties in a match
# Penalty <- c()
# Penalty <- sqldf("SELECT p1_summary.matchid,COUNT(*) AS Penalty FROM p1_summary WHERE p1_summary.Event_Type = 'Penalty' GROUP BY p1_summary.matchid ")
# P1_spread <- dplyr::left_join(P1_spread,Penalty)
# P1_spread <- P1_spread %>% replace(is.na(.),0)
# #calculate match performance
# P1_spread$MatchPerfomance <- P1_spread$TG *15 + P1_spread$TY *5 + P1_spread$TR *15 + P1_spread$TC *3 + P1_spread$Penalty *10
#
# unlink('P1_SPREAD.xlsx')
# write.xlsx(P1_spread,'P1_SPREAD.xlsx')
# #Referee
# P1_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM P1_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
# unlink('P1_refereestats.xlsx')
# write.xlsx(P1_refereestats,'P1_refereestats.xlsx')
########################################################################################################################################################################################
#######
#SC0  #
#######
sc0_match_results <- fb_match_results(country = "SCO", gender = "M", season_end_year = 2024, tier="1st")
unlink('sc0_match_results.xlsx')
write.xlsx(sc0_match_results,"sc0_match_results.xlsx")
sc0_urls <- fb_match_urls(country = "SCO", gender = "M", season_end_year = 2024, tier="1st")
unlink('sc0_urls.xlsx')
write.xlsx(sc0_urls,"sc0_urls.xlsx")

sc0_match_sumary <- fb_match_summary(match_url = sc0_urls)
unlink('sc0_match_summary.xlsx')
write.xlsx(sc0_match_sumary,"sc0_match_summary.xlsx")

sc0_summary <- readxl::read_excel('sc0_match_summary.xlsx')
sc0_summary <- sc0_summary[,c(-1)]


sc0_summary$Home_Team <- mgsub(sc0_summary$Home_Team,c("Heart of Midlothian"),c("Hearts"))
sc0_summary$Away_Team <- mgsub(sc0_summary$Away_Team,c("Heart of Midlothian"),c("Hearts"))



sc0_summary$matchid <- paste(sc0_summary$Match_Date,sc0_summary$Home_Team,sc0_summary$Away_Team,sep = "-")
SC0_spread <- subset(allteams20232024,Div =="SC0")
SC0_spread$matchid <- paste(SC0_spread$Date,SC0_spread$HomeTeam,SC0_spread$AwayTeam,sep = "-")
#referees
SC0_referees <- fb_match_results(country = "SCO", gender = "M", season_end_year = 2024, tier="1st")
SC0_referees <- SC0_referees[,c(8,10,12,16)]

#rename column names
names(SC0_referees)[2] <- paste("HomeTeam")
names(SC0_referees)[3] <- paste("AwayTeam")

#SC0_referees$HomeTeam <- mgsub(SC0_referees$HomeTeam,c(""),c(""))
#SC0_referees$AwayTeam <- mgsub(SC0_referees$AwayTeam,c(""),c(""))
SC0_referees$matchid <- paste(SC0_referees$Date,SC0_referees$HomeTeam,SC0_referees$AwayTeam,sep = "-")
SC0_spread <- dplyr::left_join(SC0_spread,SC0_referees)

SC0_spread$n <- SC0_spread$TG
View(SC0_spread)
library('sqldf')
require('RH2')

# Home_xG <- c()
# Home_xG <- sqldf("SELECT sc0_summary.matchid,sc0_summary.Home_xG FROM sc0_summary INNER JOIN SC0_spread ON sc0_summary.matchid = SC0_spread.matchid GROUP BY sc0_summary.matchid")
# SC0_spread <- dplyr::left_join(SC0_spread,Home_xG)
#
# Away_xG <- c()
# Away_xG <- sqldf("SELECT sc0_summary.matchid,sc0_summary.Away_xG FROM sc0_summary INNER JOIN SC0_spread ON sc0_summary.matchid = SC0_spread.matchid GROUP BY sc0_summary.matchid")
# SC0_spread <- dplyr::left_join(SC0_spread,Away_xG)

Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT sc0_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM sc0_summary INNER JOIN SC0_spread ON sc0_summary.matchid = SC0_spread.matchid WHERE sc0_summary.Event_Type = 'Goal' OR sc0_summary.Event_Type = 'Penalty' GROUP BY sc0_summary.matchid")
SC0_spread <- dplyr::left_join(SC0_spread,Total_Goalmins)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)
#Bookings
SC0_spread$Bookings <- (SC0_spread$HY *10 + SC0_spread$HR *25) + (SC0_spread$AY*10 + SC0_spread$AR*25)
#CrossBookings
SC0_spread$Crossbookings <- (SC0_spread$HY *10 + SC0_spread$HR *25)*(SC0_spread$AY*10 + SC0_spread$AR*25)
#GoalsXbookings
SC0_spread$GoalsXbookings <- (SC0_spread$Bookings)*(SC0_spread$TG)
#CornersXbookings
SC0_spread$CornersXbookings <- (SC0_spread$TC)*(SC0_spread$Bookings)
#GoalsXCorners
SC0_spread$GoalsXcorners <- (SC0_spread$TG)*(SC0_spread$TC)
#TGMxCorners
SC0_spread$TGMXcorners <- (SC0_spread$Total_Goalmins)*(SC0_spread$TC)
#GoalsXcornersXbookings
SC0_spread$GoalsXcornerXbookings <- (SC0_spread$TG)*(SC0_spread$TC)*(SC0_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS FH_HYC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Event_Half = '1' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,FH_HYC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS FH_AYC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Event_Half = '1' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,FH_AYC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS FH_HRC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Red Card' AND sc0_summary.Event_Half = '1' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,FH_HRC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS FH_ARC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Red Card' AND sc0_summary.Event_Half = '1' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,FH_ARC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS SH_HYC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Event_Half = '2' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,SH_HYC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS SH_AYC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Event_Half = '2' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,SH_AYC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS SH_HRC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Red Card' AND sc0_summary.Event_Half = '2' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,SH_HRC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS SH_ARC FROM sc0_summary WHERE sc0_summary.Event_Type = 'Red Card' AND sc0_summary.Event_Half = '2' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,SH_ARC)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

#firsthalf
SC0_spread$FH_HomeBookings <- SC0_spread$FH_HYC *10 + SC0_spread$FH_HRC *25

SC0_spread$FH_AwayBookings <- SC0_spread$FH_AYC *10 + SC0_spread$FH_ARC *25

SC0_spread$FH_TotalBookings <- SC0_spread$FH_HomeBookings + SC0_spread$FH_AwayBookings

#second half
SC0_spread$SH_HomeBookings <- SC0_spread$SH_HYC *10 + SC0_spread$SH_HRC *25

SC0_spread$SH_AwayBookings <- SC0_spread$SH_AYC *10 + SC0_spread$SH_ARC *25

SC0_spread$SH_TotalBookings <- SC0_spread$SH_HomeBookings + SC0_spread$SH_AwayBookings


SC0_spread$MultiBookings <- SC0_spread$FH_TotalBookings * SC0_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT sc0_summary.matchid,SUM(Event_time) AS Home_YCmins FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Home_YCmins)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT sc0_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM sc0_summary WHERE sc0_summary.Event_Type = 'Red Card' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Home_RCmins)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT sc0_summary.matchid,SUM(Event_time) AS Away_YCmins FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Away_YCmins)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT sc0_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM sc0_summary WHERE sc0_summary.Event_Type = 'Red Card' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Away_RCmins)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

SC0_spread$Home_TotalCardmins <- SC0_spread$Home_YCmins + SC0_spread$Home_RCmins
SC0_spread$Away_TotalCardmins <- SC0_spread$Away_YCmins + SC0_spread$Away_RCmins
SC0_spread$match_TotalCardmins <- SC0_spread$Home_TotalCardmins + SC0_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT sc0_summary.matchid,MIN(sc0_summary.Event_Time) AS Home_first_YCTime FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Home_Away = 'Home' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Home_first_YCTime)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT sc0_summary.matchid,MIN(sc0_summary.Event_Time) AS Away_first_YCTime FROM sc0_summary WHERE sc0_summary.Event_Type = 'Yellow Card' AND sc0_summary.Home_Away = 'Away' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Away_first_YCTime)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)

SC0_spread$match_First_YCTime <- ifelse(SC0_spread$Home_first_YCTime == '0' | SC0_spread$Away_first_YCTime == '0',pmax(SC0_spread$Home_first_YCTime,SC0_spread$Away_first_YCTime),pmin(SC0_spread$Home_first_YCTime,SC0_spread$Away_first_YCTime))

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT sc0_summary.matchid,COUNT(*) AS Penalty FROM sc0_summary WHERE sc0_summary.Event_Type = 'Penalty' GROUP BY sc0_summary.matchid ")
SC0_spread <- dplyr::left_join(SC0_spread,Penalty)
SC0_spread <- SC0_spread %>% replace(is.na(.),0)
#calculate match performance
SC0_spread$MatchPerfomance <- SC0_spread$TG *15 + SC0_spread$TY *5 + SC0_spread$TR *15 + SC0_spread$TC *3 + SC0_spread$Penalty *10

unlink('SC0_SPREAD.xlsx')
write.xlsx(SC0_spread,'SC0_SPREAD.xlsx')
#Referee
SC0_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM SC0_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('SC0_refereestats.xlsx')
write.xlsx(SC0_refereestats,'SC0_refereestats.xlsx')
################################################################################################################################################################################
######
#T1 ##
######
# t1_match_results <- fb_match_results(country = "TUR", gender = "M", season_end_year = 2024, tier="1st")
# unlink('t1_match_results.xlsx')
# write.xlsx(t1_match_results,"t1_match_results.xlsx")
# t1_urls <- fb_match_urls(country = "TUR", gender = "M", season_end_year = 2024, tier="1st")
# unlink('t1_urls.xlsx')
# write.xlsx(t1_urls,"t1_urls.xlsx")
#
# t1_match_sumary <- fb_match_summary(match_url = t1_urls)
# unlink('t1_match_summary.xlsx')
# write.xlsx(t1_match_sumary,"t1_match_summary.xlsx")
#
# t1_summary <- readxl::read_excel('t1_match_summary.xlsx')
# t1_summary <- t1_summary[,c(-1)]
#
# t1_summary$Home_Team <- mgsub(t1_summary$Home_Team,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
# t1_summary$Away_Team <- mgsub(t1_summary$Away_Team,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
#
#
#
# t1_summary$matchid <- paste(t1_summary$Match_Date,t1_summary$Home_Team,t1_summary$Away_Team,sep = "-")
# T1_spread <- subset(allteams20232024,Div =="T1")
# T1_spread$matchid <- paste(T1_spread$Date,T1_spread$HomeTeam,T1_spread$AwayTeam,sep = "-")
# #referees
# T1_referees <- fb_match_results(country = "TUR", gender = "M", season_end_year = 2024, tier="1st")
# T1_referees <- T1_referees[,c(8,10,13,18)]
#
# #rename column names
# names(T1_referees)[2] <- paste("HomeTeam")
# names(T1_referees)[3] <- paste("AwayTeam")
#
# T1_referees$HomeTeam <- mgsub(T1_referees$HomeTeam,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
# T1_referees$AwayTeam <- mgsub(T1_referees$AwayTeam,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise"))
# T1_referees$matchid <- paste(T1_referees$Date,T1_referees$HomeTeam,T1_referees$AwayTeam,sep = "-")
# T1_spread <- dplyr::left_join(T1_spread,T1_referees)
#
# T1_spread$n <- T1_spread$TG
# View(T1_spread)
# library('sqldf')
# require('RH2')
#
# Home_xG <- c()
# Home_xG <- sqldf("SELECT t1_summary.matchid,t1_summary.Home_xG FROM t1_summary INNER JOIN T1_spread ON t1_summary.matchid = T1_spread.matchid GROUP BY t1_summary.matchid")
# T1_spread <- dplyr::left_join(T1_spread,Home_xG)
#
# Away_xG <- c()
# Away_xG <- sqldf("SELECT t1_summary.matchid,t1_summary.Away_xG FROM t1_summary INNER JOIN T1_spread ON t1_summary.matchid = T1_spread.matchid GROUP BY t1_summary.matchid")
# T1_spread <- dplyr::left_join(T1_spread,Away_xG)
#
# Total_Goalmins <- c()
# Total_Goalmins <- sqldf("SELECT t1_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM t1_summary INNER JOIN T1_spread ON t1_summary.matchid = T1_spread.matchid WHERE t1_summary.Event_Type = 'Goal' OR t1_summary.Event_Type = 'Penalty' GROUP BY t1_summary.matchid")
# T1_spread <- dplyr::left_join(T1_spread,Total_Goalmins)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
# #Bookings
# T1_spread$Bookings <- (T1_spread$HY *10 + T1_spread$HR *25) + (T1_spread$AY*10 + T1_spread$AR*25)
# #CrossBookings
# T1_spread$Crossbookings <- (T1_spread$HY *10 + T1_spread$HR *25)*(T1_spread$AY*10 + T1_spread$AR*25)
# #GoalsXbookings
# T1_spread$GoalsXbookings <- (T1_spread$Bookings)*(T1_spread$TG)
# #CornersXbookings
# T1_spread$CornersXbookings <- (T1_spread$TC)*(T1_spread$Bookings)
# #GoalsXCorners
# T1_spread$GoalsXcorners <- (T1_spread$TG)*(T1_spread$TC)
# #TGMxCorners
# T1_spread$TGMXcorners <- (T1_spread$Total_Goalmins)*(T1_spread$TC)
# #GoalsXcornersXbookings
# T1_spread$GoalsXcornerXbookings <- (T1_spread$TG)*(T1_spread$TC)*(T1_spread$Bookings)
#
# #first half
# FH_HYC <- c()
# FH_HYC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS FH_HYC FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Event_Half = '1' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,FH_HYC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# FH_AYC <- c()
# FH_AYC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS FH_AYC FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Event_Half = '1' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,FH_AYC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# FH_HRC <- c()
# FH_HRC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS FH_HRC FROM t1_summary WHERE t1_summary.Event_Type = 'Red Card' AND t1_summary.Event_Half = '1' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,FH_HRC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# FH_ARC <- c()
# FH_ARC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS FH_ARC FROM t1_summary WHERE t1_summary.Event_Type = 'Red Card' AND t1_summary.Event_Half = '1' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,FH_ARC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# #second half
# SH_HYC <- c()
# SH_HYC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS SH_HYC FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Event_Half = '2' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,SH_HYC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# SH_AYC <- c()
# SH_AYC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS SH_AYC FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Event_Half = '2' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,SH_AYC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# SH_HRC <- c()
# SH_HRC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS SH_HRC FROM t1_summary WHERE t1_summary.Event_Type = 'Red Card' AND t1_summary.Event_Half = '2' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,SH_HRC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# SH_ARC <- c()
# SH_ARC <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS SH_ARC FROM t1_summary WHERE t1_summary.Event_Type = 'Red Card' AND t1_summary.Event_Half = '2' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,SH_ARC)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# #firsthalf
# T1_spread$FH_HomeBookings <- T1_spread$FH_HYC *10 + T1_spread$FH_HRC *25
#
# T1_spread$FH_AwayBookings <- T1_spread$FH_AYC *10 + T1_spread$FH_ARC *25
#
# T1_spread$FH_TotalBookings <- T1_spread$FH_HomeBookings + T1_spread$FH_AwayBookings
#
# #second half
# T1_spread$SH_HomeBookings <- T1_spread$SH_HYC *10 + T1_spread$SH_HRC *25
#
# T1_spread$SH_AwayBookings <- T1_spread$SH_AYC *10 + T1_spread$SH_ARC *25
#
# T1_spread$SH_TotalBookings <- T1_spread$SH_HomeBookings + T1_spread$SH_AwayBookings
#
#
# T1_spread$MultiBookings <- T1_spread$FH_TotalBookings * T1_spread$SH_TotalBookings
#
#
#
# Home_YCmins <- c()
# Home_YCmins <- sqldf("SELECT t1_summary.matchid,SUM(Event_time) AS Home_YCmins FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Home_YCmins)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# Home_RCmins <- c()
# Home_RCmins <- sqldf("SELECT t1_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM t1_summary WHERE t1_summary.Event_Type = 'Red Card' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Home_RCmins)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# Away_YCmins <- c()
# Away_YCmins <- sqldf("SELECT t1_summary.matchid,SUM(Event_time) AS Away_YCmins FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Away_YCmins)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# Away_RCmins <- c()
# Away_RCmins <- sqldf("SELECT t1_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM t1_summary WHERE t1_summary.Event_Type = 'Red Card' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Away_RCmins)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# T1_spread$Home_TotalCardmins <- T1_spread$Home_YCmins + T1_spread$Home_RCmins
# T1_spread$Away_TotalCardmins <- T1_spread$Away_YCmins + T1_spread$Away_RCmins
# T1_spread$match_TotalCardmins <- T1_spread$Home_TotalCardmins + T1_spread$Away_TotalCardmins
#
# Home_first_YCTime <- c()
# Home_first_YCTime <- sqldf("SELECT t1_summary.matchid,MIN(t1_summary.Event_Time) AS Home_first_YCTime FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Home_Away = 'Home' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Home_first_YCTime)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# Away_first_YCTime <- c()
# Away_first_YCTime <- sqldf("SELECT t1_summary.matchid,MIN(t1_summary.Event_Time) AS Away_first_YCTime FROM t1_summary WHERE t1_summary.Event_Type = 'Yellow Card' AND t1_summary.Home_Away = 'Away' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Away_first_YCTime)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
#
# T1_spread$match_First_YCTime <- pmin(T1_spread$Home_first_YCTime,T1_spread$Away_first_YCTime)
#
# #count number of penalties in a match
# Penalty <- c()
# Penalty <- sqldf("SELECT t1_summary.matchid,COUNT(*) AS Penalty FROM t1_summary WHERE t1_summary.Event_Type = 'Penalty' GROUP BY t1_summary.matchid ")
# T1_spread <- dplyr::left_join(T1_spread,Penalty)
# T1_spread <- T1_spread %>% replace(is.na(.),0)
# #calculate match performance
# T1_spread$MatchPerfomance <- T1_spread$TG *15 + T1_spread$TY *5 + T1_spread$TR *15 + T1_spread$TC *3 + T1_spread$Penalty *10
#
# unlink('T1_SPREAD.xlsx')
# write.xlsx(T1_spread,'T1_SPREAD.xlsx')
# #Referee
# T1_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM T1_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
# unlink('T1_refereestats.xlsx')
# write.xlsx(T1_refereestats,'T1_refereestats.xlsx')

