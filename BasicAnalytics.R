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
library('mgsub')

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
##########################################
hist(sample(c(0,1),100,replace = F))

COPA <- read.csv('../../../Leonard/Downloads/results.csv')
COPA$date <- ymd(COPA$date)
COPA <- COPA[order(as.Date(COPA$date, format = "%Y/%m%d"), decreasing = FALSE),]
sort(unique(COPA$tournament))

COPA_qualificaton <- subset(COPA,tournament == "Copa AmÃ©rica")
tail(COPA_qualificaton)
COPA <- subset(COPA,tournament == "UEFA Euro")
COPA <- COPA[COPA$date > '2008-01-01',]

AFCON <- read.csv('../../../Leonard.000/Downloads/IFootball/results.csv')
AFCON$date <- ymd(AFCON$date)
AFCON <- AFCON[order(as.Date(AFCON$date, format = "%d/%m%Y"), decreasing = FALSE),]
AFCON$CS <- paste(AFCON$home_score,AFCON$away_score, sep = "-")
AFCON <- subset(AFCON,tournament == "African Cup of Nations")
###################################################################
################################################################################################
##############add team ranks in bracket code####################################################
points_nor <- nor_league_table[order(as.numeric(nor_league_table$nor_PTS), decreasing = TRUE),]
points_nor$nor_rank <- 1:length(nor_teams)
row.names(points_nor) <- points_nor$nor_rank
#create final_nor_hf_against with team ranks in brackets
for(nor_rowhrank in 1:nrow(nor_form_team_against_h)) {
  for(nor_colhrank in 1:ncol(nor_form_team_against_h)) {

    # print(my_matrix[row, col])

        ifelse(!nor_form_team_against_h[nor_rowhrank,nor_colhrank]=="",nor_form_team_against_h[nor_rowhrank,nor_colhrank] <- paste(nor_form_team_against_h[nor_rowhrank,nor_colhrank],"(",points_nor$nor_rank[points_nor$Team ==nor_form_team_against_h[nor_rowhrank,nor_colhrank]],")",sep = ""),next)
        #print(my_matrix[row, col])


  }
}
############################################################################################
###########end of team ranks matrix########################################################

final_nor_hf_against <- c()
for(index_nor_hf_against in 1:length(nor_teams))
{

  class(nor_form_team_against_h)
  l6_form_nor_hf_against
  index_nor_hf_against <- row.names(nor_form_team_against_h) == nor_teams[index_nor_hf_against]
  form_nor_hf_against <- nor_form_team_against_h[index_nor_hf_against]
  deleted_form_nor_hf_against <- form_nor_hf_against[!form_nor_hf_against[] == ""]
  l6_form_nor_hf_against <- tail(deleted_form_nor_hf_against,nor_last_n_games)
  l6_form_nor_hf_against <- paste(l6_form_nor_hf_against,collapse = " ")
  final_nor_hf_against[index_nor_hf_against] <- rbind(paste(nor_teams[index_nor_hf_against],l6_form_nor_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}
final_nor_hf_against <- as.data.frame(final_nor_hf_against)
colnames(final_nor_hf_against) <- "Team against"
########################################################################################################
########
###nba#####
library(rvest)
rawhtml <- paste(readLines("../scorer.html"), collapse="\n")
url <- 'https://www.basketball-reference.com/boxscores/'
webpage <- read_html(rawhtml)
webpage
data <- webpage %>% html_nodes(css ='table') %>% html_table()
data
class(data)
length(data)
nor_CS_summary <- tabyl(allteams20202021,Div,CS) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)

write.xlsx(nor_CS_summary,'cs.xlsx')
sample_n(F2,5)
################################################################

allteams20212022scores <- allteams20212022
myoddscores <- readxl::read_excel('../FDAS/myodds_20212022.xlsx', sheet = '3way')

myoddscores$matchid <- paste(myoddscores$HT,myoddscores$AT, sep = "-")
allteams20212022scores$matchid <- paste(allteams20212022scores$HomeTeam,allteams20212022scores$AwayTeam, sep = "-")

#allteams20212022scores$Date <- ymd(allteams20212022scores$Date)
#myoddscores$Date <- ymd(myoddscores$Date)


#allteams20212022scores <- allteams20212022scores[allteams20212022scores$Date >= '2021-08-28',]
#myoddscores <- myoddscores[myoddscores$Date >= '2021-08-28',]

myoddscores <- myoddscores[,c(24,25,31)]
allteams20212022scores <- allteams20212022scores[,c(3,4,30,15,24)]

finalscore <- dplyr::left_join(myoddscores,allteams20212022scores)
write.xlsx(finalscore,'finalscore.xlsx')
###################################################################################
####################################################################################
poissonteams2122 <- readxl::read_excel('../FDAS/poisson calc_20212022_sqlmodel.xlsx',sheet = 'home')
b1teams2122 <- poissonteams2122[poissonteams2122$division == "B1",]
sort(b1teams2122$HomeTeam)
sort(unique(B1_schedule20212022$Home_Team))
B1_schedule20212022 <- mgsub(B1_schedule20212022,c("Anderlecht","Antwerp","Beerschot","Cercle Brugge","Charleroi","Club Brugge","Eupen","Genk","Gent","Kortrijk","Mechelen","OH Leuven","Oostende","R.F.C. Seraing","Sint-Truiden","Standard Liege","Union SG","Zulte Waregem"),
c("Anderlecht","Antwerp","Beerschot VA","Cercle Brugge","Charleroi","Club Brugge","Eupen","Genk","Gent","Kortrijk","Mechelen","Oud-Heverlee Leuven","Oostende","Seraing","St Truiden","Standard","St. Gilloise","Waregem"))

write.csv(B1_schedule20212022,'B1_schedule20212022.csv')
####################################################################
e2teams2122 <- poissonteams2122[poissonteams2122$division == "E2",]
sort(e2teams2122$HomeTeam)
sort(unique(E2_schedule20212022$Home_Team))

E2_schedule20212022 <- mgsub(E2_schedule20212022,c("Accrington Stanley","Burton Albion","Cambridge Utd","Charlton Ath","Cheltenham","Crewe Alexandra","Doncaster","Fleetwood Town","Gillingham","Ipswich Town","Lincoln City","MK Dons" ,"Morecambe","Oxford United","Plymouth Argyle","Portsmouth","Rotherham Utd","Sheffield Weds" ,"Shrewsbury","Sunderland","Wigan Athletic","Wycombe"),c("Accrington","Burton","Cambridge","Charlton", "Cheltenham","Crewe","Doncaster","Fleetwood Town","Gillingham","Ipswich","Lincoln","Milton Keynes Dons","Morecambe","Oxford","Plymouth","Portsmouth","Rotherham","Sheffield Weds","Shrewsbury","Sunderland","Wigan","Wycombe"))
write.csv(E2_schedule20212022,'E2_schedule20212022.csv')
####################################################################
e3teams2122 <- poissonteams2122[poissonteams2122$division == "E3",]
sort(e3teams2122$HomeTeam)
sort(unique(E3_schedule20212022$Home_Team))

E3_schedule20212022 <- mgsub(E3_schedule20212022,c("Barrow","Bradford City","Bristol Rovers","Carlisle United","Colchester Utd", "Crawley Town","Exeter City","FG Rovers","Harrogate","Hartlepool Utd" ,"Leyton Orient","Mansfield Town","Newport County","Northampton","Oldham Athletic","Port Vale","Rochdale", "Salford City","Scunthorpe Utd","Stevenage","Sutton United","Swindon Town","Tranmere Rovers","Walsall"),c("Barrow","Bradford","Bristol Rvs","Carlisle","Colchester","Crawley Town","Exeter","Forest Green","Harrogate","Hartlepool","Leyton Orient","Mansfield","Newport County","Northampton","Oldham","Port Vale","Rochdale","Salford","Scunthorpe","Stevenage","Sutton","Swindon","Tranmere","Walsall"))
write.csv(E3_schedule20212022,'E3_schedule20212022.csv')
########################################################################
ecteams2122 <- poissonteams2122[poissonteams2122$division == "EC",]
sort(ecteams2122$HomeTeam)
tempecteams <- sort(unique(EC_schedule20212022$Home_Team))

EC_schedule20212022 <- mgsub(EC_schedule20212022,c( "Aldershot Town","Altrincham","Barnet","Boreham Wood","Bromley","Chesterfield","Dag & Red","Dover Athletic","Eastleigh","Grimsby Town","FC Halifax Town","King's Lynn","Maidenhead Utd","Notts County","Solihull Moors","Southend United","Stockport","Torquay","Wealdstone","Weymouth","Woking","Wrexham","Yeovil Town"),c( "Aldershot","Altrincham","Barnet","Boreham Wood","Bromley","Chesterfield","Dag and Red","Dover Athletic","Eastleigh","Grimsby","Halifax","Kings Lynn","Maidenhead","Notts County","Solihull","Southend","Stockport","Torquay","Wealdstone","Weymouth","Woking","Wrexham","Yeovil"))

write.csv(EC_schedule20212022,'EC_schedule20212022.csv')
##########################################################################
d2teams2122 <- poissonteams2122[poissonteams2122$division == "D2",]
sort(d2teams2122$HomeTeam)
sort(unique(D2_schedule20212022$Home_Team))
D2_schedule20212022 <- mgsub(D2_schedule20212022,c( "Darmstadt 98","Dusseldorf","Hamburger SV","Hannover 96","Ingolstadt 04","Jahn Regensburg","Karlsruher","Paderborn 07","St. Pauli"),c( "Darmstadt","Fortuna Dusseldorf","Hamburg","Hannover","Ingolstadt","Regensburg","Karlsruhe","Paderborn","St Pauli"))
write.csv(D2_schedule20212022,'D2_schedule20212022.csv')
##########################################################################
f2teams2122 <- poissonteams2122[poissonteams2122$division == "F2",]
sort(f2teams2122$HomeTeam)
sort(unique(F2_schedule20212022$Home_Team))
F2_schedule20212022 <- mgsub(F2_schedule20212022,c( "Rodez Aveyron","Quevilly-Rouen"),c( "Rodez","Quevilly Rouen"))
write.csv(F2_schedule20212022,'F2_schedule20212022.csv')
###########################################################################
n1teams2122 <- poissonteams2122[poissonteams2122$division == "N1",]
sort(n1teams2122$HomeTeam)
sort(unique(N1_schedule20212022$Home_Team))
N1_schedule20212022 <- mgsub(N1_schedule20212022,c( "Fortuna Sittard","Go Ahead","Heracles Almelo","NEC Nijmegen","RKC Waalwijk"),c( "For Sittard","Go Ahead Eagles","Heracles","Nijmegen","Waalwijk"))
write.csv(N1_schedule20212022,'N1_schedule20212022.csv')
########################################################################
p1teams2122 <- poissonteams2122[poissonteams2122$division == "P1",]
sort(p1teams2122$HomeTeam)
sort(unique(P1_schedule20212022$Home_Team))
P1_schedule20212022 <- mgsub(P1_schedule20212022,c( "Braga","Gil Vicente FC","Pacos","Sporting CP","FC Vizela","Vitoria"),c( "Sp Braga","Gil Vicente","Pacos Ferreira","Sp Lisbon","Vizela","Guimaraes"))
write.csv(P1_schedule20212022,'P1_schedule20212022.csv')
##############################################################################
sc0teams2122 <- poissonteams2122[poissonteams2122$division == "SC0",]
sort(sc0teams2122$HomeTeam)
sort(unique(SC0_schedule20212022$Home_Team))
SC0_schedule20212022 <- mgsub(SC0_schedule20212022,c( "Braga","Gil Vicente FC","Pacos","Sporting CP","FC Vizela","Vitoria"),c( "Sp Braga","Gil Vicente","Pacos Ferreira","Sp Lisbon","Vizela","Guimaraes"))
write.csv(SC0_schedule20212022,'SC0_schedule20212022.csv')
##############################################################################################
sc1teams2122 <- poissonteams2122[poissonteams2122$division == "SC1",]
sort(sc1teams2122$HomeTeam)
sort(unique(SC1_schedule20212022$Home_Team))
SC1_schedule20212022 <- mgsub(SC1_schedule20212022,c( "Arbroath FC","Ayr United","Greenock Morton","Inverness CT","Partick Thistle","Queens","Raith Rovers"),c( "Arbroath","Ayr","Morton","Inverness C","Partick","Queen of Sth","Raith Rvs"))
write.csv(SC1_schedule20212022,'SC1_schedule20212022.csv')
#############################################################################
sp2teams2122 <- poissonteams2122[poissonteams2122$division == "SP2",]
sort(sp2teams2122$HomeTeam)
sort(unique(SP2_schedule20212022$Home_Team))
SP2_schedule20212022 <- mgsub(SP2_schedule20212022,c( "SD Amorebieta","CD Mirandes","UD Ibiza","Real Sociedad B","Sporting Gijon"),c( "Amorebieta","Mirandes","Ibiza","Sociedad B","Sp Gijon"))
write.csv(SP2_schedule20212022,'SP2_schedule20212022.csv')
############################################################################
poissonteamsnewleagues2122 <- readxl::read_excel('../FDAS/poisson calc_20212022_newleagues.xlsx',sheet = 'home')
autteams2122 <- poissonteamsnewleagues2122[poissonteamsnewleagues2122$division == "Admiral Bundesliga",]
sort(autteams2122$HomeTeam)
sort(unique(AUT_schedule20212022$Home_Team))
AUT_schedule20212022 <- mgsub(AUT_schedule20212022,c( "SK Austria Klagenfurt","SCR Altach","Austria Wien","Rapid Wien","RB Salzburg","WSG Wattens"),c( "A. Klagenfurt","Altach","Austria Vienna","Rapid Vienna","Salzburg","Tirol"))
write.csv(AUT_schedule20212022,'AUT_schedule20212022.csv')
######################################################################
dnkteams2122 <- poissonteamsnewleagues2122[poissonteamsnewleagues2122$division == "Superliga",]
sort(dnkteams2122$HomeTeam)
sort(unique(DNK_schedule20212022$Home_Team))
DNK_schedule20212022 <- mgsub(DNK_schedule20212022,c( "Randers","Vejle BK","AGF"),c( "Randers FC","Vejle","Aarhus"))
write.csv(DNK_schedule20212022,'DNK_schedule20212022.csv')
####################################################################################################
polteams2122 <- poissonteamsnewleagues2122[poissonteamsnewleagues2122$division == "Ekstraklasa",]
sort(polteams2122$HomeTeam)
sort(unique(POL_schedule20212022$Home_Team))
POL_schedule20212022 <- mgsub(POL_schedule20212022,c( "Gornik Leczna","Gornik Zabrze","Legia Warsaw","RKS Rakow","Termalica Nieciecza","Wisla Krakow","Zaglebie Lubin"),c( "Leczna","Gornik Z","Legia","Rakow","Termalica B-B.","Wisla","Zaglebie"))
write.csv(POL_schedule20212022,'POL_schedule20212022.csv')
#############################################################################
routeams2122 <- poissonteamsnewleagues2122[poissonteamsnewleagues2122$division == "Liga 1",]
sort(routeams2122$HomeTeam)
sort(unique(ROU_schedule20212022$Home_Team))
ROU_schedule20212022 <- mgsub(ROU_schedule20212022,c( "Chindia","Clinceni","Arges Pitesti","Botosani","Sepsi","Gaz Metan","FC U Craiova","Dinamo","FC U Craiova","CS U Craiova","Voluntari"),c( "Chindia Targoviste","Academica Clinceni","FC Arges","FC Botosani","Sepsi Sf. Gheorghe","Gaz Metan Medias","U Craiova 1948","Din. Bucuresti","Univ. Craiova","U Craiova 1948","FC Voluntari"))
write.csv(ROU_schedule20212022,'ROU_schedule20212022.csv')
##########################################################################################
rusteams2122 <- poissonteamsnewleagues2122[poissonteamsnewleagues2122$division == "Premier League",]
sort(rusteams2122$HomeTeam)
sort(unique(RUS_schedule20212022$Home_Team))
RUS_schedule20212022 <- mgsub(RUS_schedule20212022,c( "Dynamo Mosc","Rostov","FC Khimki","Loko Moscow","Samara"),c( "Dynamo Moscow","FK Rostov","Khimki","Lokomotiv Moscow","FK Krylya Sovetov Samara"))
write.csv(RUS_schedule20212022,'RUS_schedule20212022.csv')
##########################################################################################
swzteams2122 <- poissonteamsnewleagues2122[poissonteamsnewleagues2122$division == "Swiss",]
sort(swzteams2122$HomeTeam)
sort(unique(SWZ_schedule20212022$Home_Team))
SWZ_schedule20212022 <- mgsub(SWZ_schedule20212022,c( "Grasshopper","Lausanne-Sport","Servette FC"),c( "Grasshoppers","Lausanne","Servette"))
write.csv(SWZ_schedule20212022,'SWZ_schedule20212022.csv')

apply(b1_winmargin_h,1,sum)
e0_league_table[e0_league_table$Team == "Chelsea",6]
typeof(l6_form_e0_gs)
class(suml6_e0_gs)
##################################################################################################################
final_b1_wm <- c()
suml6_b1_wm <- c()
for(index_b1_wm_gp in 1:b1_games_played[1])
{

for(index_b1_wm in 1:length(b1_teams))
{

  {
  index_b1_wm <- row.names(b1_winmargin_h) == b1_teams[index_b1_wm]
  form_b1_wm <- b1_winmargin_h[index_b1_wm]

  deleted_form_b1_wm <- form_b1_wm[!form_b1_wm[] == ""]
  l6_form_b1_wm <- deleted_form_b1_wm[index_b1_wm_gp]
  l6_form_b1_wm <- as.numeric(l6_form_b1_wm)
  suml6_b1_wm[index_b1_wm] <- sum(l6_form_b1_wm)
  l6_form_b1_wm <- paste(l6_form_b1_wm,collapse = " ")
  final_b1_wm[index_b1_wm] <- cbind(paste(b1_teams[index_b1_wm],l6_form_b1_wm,suml6_b1_wm[index_b1_wm], sep = ",",collapse = ""))
  }
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)
}
}

final_b1_wm
b1_winmargin_h
typeof(deleted_form_b1_wm)
suml6_b1_wm
dim(form_b1_wm)
cbind(b1_teams,suml6_b1_wm)

for(b1_rowhwmsum in 1:nrow(b1_winmargin_h)) {
  for(b1_colhwmsum in 1:ncol(b1_winmargin_h)) {



  }

    }

as.numeric(b1_winmargin_h)
b1_winmargin_h


































