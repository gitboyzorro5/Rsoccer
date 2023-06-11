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
################################################################
b1_firstgames <- length(b1_teams)/2
#install.packages('reshape2')
library('reshape2')
b1_winmargin_h_margindata <- melt(b1_winmargin_h)
colnames(b1_winmargin_h_margindata)[1] <- "Team"
B1_margindata <- B1

 b1_winmargin_h_margindata <- b1_winmargin_h_margindata[!b1_winmargin_h_margindata$value == "",]



 B1_secondsplit <- tail(B1_margindata,nrow(B1_margindata) - b1_firstgames)

 head(b1_winmargin_h_margindata,4)
 head(B1_secondsplit,4)

 nrow(b1_winmargin_h_margindata)
 nrow(B1_secondsplit)


B1_margindata$homewinmargin <- "0"
B1_margindata$awaywinmargin <- "0"



require(RH2)
library(sqldf)

for(b1_margindata_row in 1:nrow(B1_margindata)){

if(b1_margindata_row <= b1_firstgames){

B1_margindata$homewinmargin <- "0"
B1_margindata$awaywinmargin <- "0"


}else
  B1_margindata$homewinmargin <- sqldf("select b1_winmargin_h_margindata.Value from b1_winmargin_h_margindata inner join B1_margindata ON  b1_winmargin_h_margindata.Date = B1_margindata.Date and Team = B1_margindata.HomeTeam")
  B1_margindata$awaywinmargin <- sqldf("select b1_winmargin_h_margindata.Value from b1_winmargin_h_margindata inner join B1_margindata ON  b1_winmargin_h_margindata.Date = B1_margindata.Date and Team = B1_margindata.AwayTeam")

}
######################################

#split first matches


B1_secondsplit$homewinmargin <- sqldf("select b1_winmargin_h_margindata.Value from b1_winmargin_h_margindata inner join B1_secondsplit ON  b1_winmargin_h_margindata.Date = B1_secondsplit.Date and Team = B1_secondsplit.HomeTeam")
B1_secondsplit$awaywinmargin <- sqldf("select b1_winmargin_h_margindata.Value from b1_winmargin_h_margindata inner join B1_secondsplit ON  b1_winmargin_h_margindata.Date = B1_secondsplit.Date and Team = B1_secondsplit.AwayTeam")
#####################################################################################################################
#####################################################################################################################
#begin gamesate algortihm
b1_firstgames <- length(b1_teams)/2
B1_margindata <- B1
B1_firstsplit <-  head(B1_margindata,b1_firstgames)
B1_secondsplit <- tail(B1_margindata,nrow(B1_margindata) - b1_firstgames)
B1_secondsplit$b1_HomeTeam_index_wm <- match(B1_secondsplit$HomeTeam,b1_teams)
B1_secondsplit$b1_AwayTeam_index_wm <- match(B1_secondsplit$AwayTeam,b1_teams)
B1_secondsplit$b1_homegame_no <- rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)
B1_secondsplit$b1_awaygame_no <- rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)

b1_homewinmargin <- c()
b1_awaywinmargin <- c()

for (b1_secondsplitrow in 1:nrow(B1_secondsplit))
{

  b1_hometeamindex_wm <- B1_secondsplit[b1_secondsplitrow,"b1_HomeTeam_index_wm"]
  b1_awayteamindex_wm <- B1_secondsplit[b1_secondsplitrow,"b1_AwayTeam_index_wm"]
  b1_hometeam_game_no <- B1_secondsplit[b1_secondsplitrow,"b1_homegame_no"]
  b1_awayteam_game_no <- B1_secondsplit[b1_secondsplitrow,"b1_awaygame_no"]

  b1_winmargin_vec_gamestate_h <- as.vector(b1_winmargin_h[b1_hometeamindex_wm,])
  b1_winmargin_vec_gamestate_h[is.na(b1_winmargin_vec_gamestate_h)] <- ""
  b1_winmargin_vec_gamestate_h <- b1_winmargin_vec_gamestate_h[b1_winmargin_vec_gamestate_h != ""]
  b1_winmargin_vec_gamestate_h <- as.numeric(b1_winmargin_vec_gamestate_h)

  b1_winmargin_vec_gamestate_a <- as.vector(b1_winmargin_h[b1_awayteamindex_wm,])
  b1_winmargin_vec_gamestate_a[is.na(b1_winmargin_vec_gamestate_a)] <- ""
  b1_winmargin_vec_gamestate_a <- b1_winmargin_vec_gamestate_a[b1_winmargin_vec_gamestate_a != ""]
  b1_winmargin_vec_gamestate_a <- as.numeric(b1_winmargin_vec_gamestate_a)

  for (b1_game_no in 1:b1_games_played[1])

  {

  b1_homewinmargin[b1_secondsplitrow] <- b1_winmargin_vec_gamestate_h[b1_hometeam_game_no]
  b1_awaywinmargin[b1_secondsplitrow] <- b1_winmargin_vec_gamestate_a[b1_awayteam_game_no]


  }

}

b1_homewinmargin
b1_awaywinmargin

B1_gamestate_data <- cbind(B1_secondsplit,b1_homewinmargin,b1_awaywinmargin)

tail(B1_gamestate_data,5)
###################################################################################################
###################################################################################################

###################################################################################################


head(B1_secondsplit,15)
nrow(B1_secondsplit)
b1_teams
b1_winmargin_vec_gamestate_h[b1_hometeam_game_no]
b1_winmargin_h
b1_awayteam_game_no
rep(1:13,each = 9)
rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)

###################################################################################################
###################################################################################################
subset(allteams20212022_gamestate[allteams20212022_gamestate$GSH >= 3,],Div == "D1")
#####################################################################################################

b1_yellowtotals_vec_ht <- as.vector(b1_yellowtotalsv2[1,])
b1_yellowtotals_vec_ht[is.na(b1_yellowtotals_vec_ht)] <- ""
b1_yellowtotals_vec_ht <- b1_yellowtotals_vec_ht[b1_yellowtotals_vec_ht != ""]
b1_yellowtotals_vec_ht  <-tail(b1_yellowtotals_vec_ht,1)
#####################################################################################################
install.packages('tesseract')
library(tesseract)

eng <- tesseract("eng")
text <- tesseract::ocr("C:\\Users\\Magut\\Documents\\bet_history\\pinnacle\\pinnacle254.png", engine = eng)
text_split <- strsplit(text,"\n")
text_split <- as.vector(text_split)
text_split <- unlist(text_split)
text_split
#Accepted:  :Stake
#pinnacle:
#Total - FT - Turkey - Super League
#Final score:
#Settled:

head(grep("Settled",text_split),1)
head(grep("@",text_split),1)
text_split





allteams20202021[allteams20202021$HomeTeam == "Eastleigh",]

nrow(allteams20202021)
##################################################################################################################
allteams20202021_backup <- allteams20202021
##################################################################################################################
#
library(xlsx)
library(lubridate)
#2way
myodds_history_20202021 <- readxl::read_excel('../Desktop/myodds_history_20202021.xlsx', sheet = '2way')

myodds_history_20202021_np <- myodds_history_20202021[myodds_history_20202021$SCORE == "P",]

myodds_history_20202021_np$matchid <- paste(myodds_history_20202021_np$HT,myodds_history_20202021_np$AT,sep = "-")

allteams20202021$matchid <- paste(allteams20202021$HomeTeam,allteams20202021$AwayTeam,sep = "-")

final_myodds_history_20202021 <- dplyr::left_join(myodds_history_20202021_np,allteams20202021,by = "matchid")
write.xlsx(final_myodds_history_20202021,'final_myodds_history_20202021.xlsx')
##################################################################################################################
##################################################################################################################
#3way
myodds_history_20202021_3way <- readxl::read_excel('../Desktop/myodds_history_20202021.xlsx', sheet = '3way')

myodds_history_20202021_np_3way <- myodds_history_20202021_3way[myodds_history_20202021_3way$SCORE == "P",]


myodds_history_20202021_np_3way$matchid <- paste(myodds_history_20202021_np_3way$HT,myodds_history_20202021_np_3way$AT,sep = "-")

#allteams20202021$matchid <- paste(allteams20202021$HomeTeam,allteams20202021$AwayTeam,sep = "-")

final_myodds_history_20202021_3way <- dplyr::left_join(myodds_history_20202021_np_3way,allteams20202021,by = "matchid")
write.xlsx(final_myodds_history_20202021_3way,'final_myodds_history_20202021_3way.xlsx')
###################################################################################################################
###################################################################################################################
#simulations
B1_sim <- B1
B1_sim$matchid <- paste(B1_sim$HomeTeam,B1_sim$AwayTeam,sep = "-")
B1_fixtures$matchid <- paste(B1_fixtures$HomeTeam_b1,B1_fixtures$AwayTeam_b1,sep = "-")
B1_fixtures$b1_FTR <- sapply(B1_fixtures$b1_pscore,switch,
'1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
'0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
'0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

B1_fixtures$b1_gamestatus <- ifelse(B1_fixtures$matchid %in% B1_sim$matchid,"played","notplayed")

b1_home_wins_sim <- c()
b1_away_wins_sim <- c()
b1_home_draws_sim <- c()
b1_away_draws_sim <- c()
b1_home_loss_sim <- c()
b1_away_loss_sim <- c()



for (i_b1_wins_sim in 1:length(b1_teams))
{

  b1_home_wins_sim[i_b1_wins_sim] <- nrow(B1_fixtures[B1_fixtures$HomeTeam_b1 == b1_teams[i_b1_wins_sim] & B1_fixtures$b1_FTR == "H" & B1_fixtures$b1_gamestatus =="notplayed",])
  b1_away_wins_sim[i_b1_wins_sim] <- nrow(B1_fixtures[B1_fixtures$AwayTeam_b1 == b1_teams[i_b1_wins_sim] & B1_fixtures$b1_FTR == "A" & B1_fixtures$b1_gamestatus == "notplayed",])
  b1_home_draws_sim[i_b1_wins_sim] <- nrow(B1_fixtures[B1_fixtures$HomeTeam_b1 == b1_teams[i_b1_wins_sim] & B1_fixtures$b1_FTR == "D" & B1_fixtures$b1_gamestatus == "notplayed",])
  b1_away_draws_sim[i_b1_wins_sim] <- nrow(B1_fixtures[B1_fixtures$AwayTeam_b1 == b1_teams[i_b1_wins_sim] & B1_fixtures$b1_FTR == "D" & B1_fixtures$b1_gamestatus == "notplayed",])
  b1_home_loss_sim[i_b1_wins_sim] <- nrow(B1_fixtures[B1_fixtures$HomeTeam_b1 == b1_teams[i_b1_wins_sim] & B1_fixtures$b1_FTR == "A" & B1_fixtures$b1_gamestatus == "notplayed",])
  b1_away_loss_sim[i_b1_wins_sim] <- nrow(B1_fixtures[B1_fixtures$AwayTeam_b1 == b1_teams[i_b1_wins_sim] & B1_fixtures$b1_FTR == "H" & B1_fixtures$b1_gamestatus == "notplayed", ])

}

b1_total_wins_sim <- b1_home_wins_sim + b1_away_wins_sim
b1_total_draws_sim <- b1_home_draws_sim + b1_away_draws_sim
b1_total_loss_sim <- b1_home_loss_sim + b1_away_loss_sim

b1_home_games_sim <- c()
b1_away_games_sim <-c()

for (i_b1_sim in 1:length(b1_teams))
{

  b1_home_games_sim[i_b1_sim] <- nrow(B1_fixtures[B1_fixtures$HomeTeam_b1 == b1_teams[i_b1_sim] & B1_fixtures$b1_gamestatus == "notplayed",])
  b1_away_games_sim[i_b1_sim]  <- nrow(B1_fixtures[B1_fixtures$AwayTeam_b1 == b1_teams[i_b1_sim] & B1_fixtures$b1_gamestatus == "notplayed",])

}

b1_games_played_sim <- b1_home_games_sim + b1_away_games_sim

b1_league_table_sim <- cbind(b1_teams,b1_games_played_sim,b1_total_wins_sim,b1_total_draws_sim,b1_total_loss_sim)
b1_PTS_sim <- (b1_total_wins_sim*3) + (b1_total_draws_sim*1)
b1_league_table_sim <- cbind(b1_league_table_sim,b1_PTS_sim)

b1_games_played_simfinal <- b1_games_played + b1_games_played_sim
b1_total_wins_simfinal <- b1_total_wins + b1_total_wins_sim
b1_total_draws_simfinal <- b1_total_draws + b1_total_draws_sim
b1_total_loss_simfinal <- b1_total_loss + b1_total_loss_sim
b1_PTS_simfinal <- b1_PTS + b1_PTS_sim

b1_league_table_simfinal <- cbind(b1_teams,b1_games_played_simfinal,b1_total_wins_simfinal,b1_total_draws_simfinal,b1_total_loss_simfinal,b1_PTS_simfinal)
b1_league_table_simfinal <- as.data.frame(b1_league_table_simfinal)
names(b1_league_table_simfinal)[names(b1_league_table_simfinal) == "b1_teams"] <- "Team_f"
names(b1_league_table_simfinal)[names(b1_league_table_simfinal) == "b1_games_played_simfinal"] <- "P_f"
names(b1_league_table_simfinal)[names(b1_league_table_simfinal) == "b1_total_wins_simfinal"] <- "W_f"
names(b1_league_table_simfinal)[names(b1_league_table_simfinal) == "b1_total_draws_simfinal"] <- "D_f"
names(b1_league_table_simfinal)[names(b1_league_table_simfinal) == "b1_total_loss_simfinal"] <- "L_f"
names(b1_league_table_simfinal)[names(b1_league_table_simfinal) == "b1_PTS_simfinal"] <- "PTS_f"
points_b1_sim <-  b1_league_table_simfinal[order(as.numeric(b1_league_table_simfinal$b1_PTS_simfinal), decreasing = TRUE),]

library('xlsx')
write.xlsx(b1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "B1_sim")
write.xlsx(points_b1_sim,'Divisions/Simulations.xlsx', sheetName = "B1_simfinal",append = TRUE)
############################################################################################################################################################
############################################################################################################################################################






b1_GS <- b1_scoring$TGS
b1_GC <-b1_conceding$TGC
b1_GD <- b1_scoring$TGS - b1_conceding$TGC
b1_PTS <- (b1_total_wins*3) + (b1_total_draws*1)
b1_league_table <- cbind(b1_league_table,b1_GS,b1_GC,b1_GD,b1_PTS)
b1_league_table <- as.data.frame(b1_league_table)
#rename the columns
names(b1_league_table)[names(b1_league_table) == "b1_teams"] <- "Team"
names(b1_league_table)[names(b1_league_table) == "b1_games_played"] <- "P"
names(b1_league_table)[names(b1_league_table) == "b1_total_wins"] <- "W"
names(b1_league_table)[names(b1_league_table) == "b1_total_draws"] <- "D"
names(b1_league_table)[names(b1_league_table) == "b1_total_loss"] <- "L"
names(b1_league_table)[names(b1_league_table) == "b1_GS"] <- "F"
names(b1_league_table)[names(b1_league_table) == "b1_GC"] <- "A"
points_b1 <- b1_league_table[order(as.numeric(b1_league_table$b1_PTS), decreasing = TRUE),]
points_b1$b1_rank <- 1:length(b1_teams)
row.names(points_b1) <- points_b1$b1_rank
################################################################################################
################################################################################################
b1_totalrounds <-  (length(b1_teams) - 1 )*2
b1_totalmatches <- (length(b1_teams)*(length(b1_teams) - 1))
b1_eachround <- b1_totalmatches / b1_totalrounds

b1_matchesplayed <-  nrow(B1)

B1_rounds <- B1

if(b1_matchesplayed %% b1_eachround == 0)
{
  b1_currentround <- b1_matchesplayed / b1_eachround
  b1_matchday <- c()
  b1_matchday <- rep(1:b1_currentround, each = b1_eachround)
}else if(b1_matchesplayed %% b1_eachround != 0)

{

  b1_modulus <- b1_matchesplayed %% b1_eachround
  b1_currentround <- (b1_matchesplayed - b1_modulus) / b1_eachround
  b1_matchday <- c()
  b1_matchday_vec1 <- c()
  b1_matchday_vec2 <- c()
  b1_matchday_vec1 <- rep(1:b1_currentround, each = b1_eachround)
  b1_matchday_vec2[1:b1_modulus] <- c(b1_currentround + 1)
  b1_matchday <- append(b1_matchday_vec1,b1_matchday_vec2)
}
B1_rounds
B1_rounds <- cbind(B1_rounds,b1_matchday)
#####################################################################################################
#####################################################################################################
#####################################################################################################

#hwins and away wins
b1_home_wins_rnds <- c()
b1_away_wins_rnds <- c()
b1_home_draws_rnds <- c()
b1_away_draws_rnds <- c()
b1_home_loss_rnds <- c()
b1_away_loss_rnds <- c()

#b1_krounds is total rounds as per current season
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_roundmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))

for(i_b1_krounds in 1:b1_krounds)
  {

for (i_b1_wins_rnds in 1:length(b1_teams))
{

  b1_home_wins_rnds[i_b1_wins_rnds] <- nrow(B1_rounds[B1_rounds$HomeTeam == b1_teams[i_b1_wins_rnds] & B1_rounds$FTR == "H" & B1_rounds$b1_matchday <= i_b1_krounds,])
  b1_away_wins_rnds[i_b1_wins_rnds] <- nrow(B1_rounds[B1_rounds$AwayTeam == b1_teams[i_b1_wins_rnds] & B1_rounds$FTR == "A" & B1_rounds$b1_matchday <= i_b1_krounds,])
  b1_home_draws_rnds[i_b1_wins_rnds] <- nrow(B1_rounds[B1_rounds$HomeTeam == b1_teams[i_b1_wins_rnds] & B1_rounds$FTR == "D" & B1_rounds$b1_matchday <= i_b1_krounds,])
  b1_away_draws_rnds[i_b1_wins_rnds] <- nrow(B1_rounds[B1_rounds$AwayTeam == b1_teams[i_b1_wins_rnds] & B1_rounds$FTR == "D" & B1_rounds$b1_matchday <= i_b1_krounds,])
  b1_home_loss_rnds[i_b1_wins_rnds] <- nrow(B1_rounds[B1_rounds$HomeTeam == b1_teams[i_b1_wins_rnds] & B1_rounds$FTR == "A" & B1_rounds$b1_matchday <= i_b1_krounds,])
  b1_away_loss_rnds[i_b1_wins_rnds] <- nrow(B1_rounds[B1_rounds$AwayTeam == b1_teams[i_b1_wins_rnds] & B1_rounds$FTR == "H" & B1_rounds$b1_matchday <= i_b1_krounds,])

}

b1_total_wins_rnds <- b1_home_wins_rnds + b1_away_wins_rnds
b1_total_draws_rnds <- b1_home_draws_rnds + b1_away_draws_rnds
b1_total_loss_rnds <- b1_home_loss_rnds + b1_away_loss_rnds


b1_home_games_rnds <- c()
b1_away_games_rnds <-c()

for (i_b1_rnds in 1:length(b1_teams))
{

  b1_home_games_rnds[i_b1_rnds] <- nrow(B1_rounds[B1_rounds$HomeTeam == b1_teams[i_b1_rnds] & B1_rounds$b1_matchday <= i_b1_krounds,])
  b1_away_games_rnds[i_b1_rnds]  <- nrow(B1_rounds[B1_rounds$AwayTeam == b1_teams[i_b1_rnds] & B1_rounds$b1_matchday <= i_b1_krounds,])

}

b1_games_played_rnds <- b1_home_games_rnds + b1_away_games_rnds

b1_league_table_rnds <- cbind(b1_teams,b1_games_played_rnds,b1_total_wins_rnds,b1_total_draws_rnds,b1_total_loss_rnds)

# b1_GS <- b1_scoring$TGS
# b1_GC <-b1_conceding$TGC
# b1_GD <- b1_scoring$TGS - b1_conceding$TGC

b1_PTS_rnds <- (b1_total_wins_rnds*3) + (b1_total_draws_rnds*1)
b1_league_table_rnds <- cbind(b1_league_table_rnds,b1_PTS_rnds)
b1_league_table_rnds <- as.data.frame(b1_league_table_rnds)
#rename the columns
names(b1_league_table_rnds)[names(b1_league_table_rnds) == "b1_teams"] <- "Team"
names(b1_league_table_rnds)[names(b1_league_table_rnds) == "b1_games_played_rnds"] <- "P"
names(b1_league_table_rnds)[names(b1_league_table_rnds) == "b1_total_wins_rnds"] <- "W"
names(b1_league_table_rnds)[names(b1_league_table_rnds) == "b1_total_draws_rnds"] <- "D"
names(b1_league_table_rnds)[names(b1_league_table_rnds) == "b1_total_loss_rnds"] <- "L"
# names(b1_league_table)[names(b1_league_table) == "b1_GS"] <- "F"
# names(b1_league_table)[names(b1_league_table) == "b1_GC"] <- "A"
points_b1_rnds <- b1_league_table_rnds[order(as.numeric(b1_league_table_rnds$b1_PTS_rnds), decreasing = TRUE),]
points_b1_rnds$b1_rank_rnds <- 1:length(b1_teams)
row.names(points_b1_rnds) <- points_b1_rnds$b1_rank


points_b1_rnds <- points_b1_rnds[order(as.character(points_b1_rnds$Team)),]


b1_roundmatrix[,i_b1_krounds] <- as.data.frame(points_b1_rnds$b1_rank_rnds)


}

b1_roundmatrix <- cbind(b1_teams,b1_roundmatrix)

write.xlsx(b1_roundmatrix,'b1_roundmatrix.xlsx')
##################################################################################################################
##################################################################################################################
#b1_krounds is total rounds as per current season
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goalscoredmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))

b1_goalsfor <- c()

for(i_b1_krounds in 1:b1_krounds)
{

  for (i_b1_gs in 1:length(b1_teams))
  {

   b1_goalsfor[i_b1_gs] <- if(B1_rounds$HomeTeam == b1_teams[i_b1_tg] & B1_rounds$b1_matchday <= i_b1_krounds) {B1_round['FTHG']}
   else {B1_rounds['FTAG']}

  }

  b1_goalscoredmatrix[,i_b1_krounds] <- as.data.frame(b1_goalsfor[])

}

b1_goalscoredmatrix <- cbind(b1_teams,b1_goalscoredmatrix)


b1_goalsfor
b1_goalscoredmatrix
b1_teams

ams
head(B1_rounds,10)

warnings()

#################################################################################
#################################################################################
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goalscoredmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goalscoredround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
b1_homegoalscored <- B1_rounds$FTHG[B1_rounds$b1_matchday == i_b1_krounds]

b1_awaygoalscored <- B1_rounds$FTAG[B1_rounds$b1_matchday == i_b1_krounds]

b1_hometeamstemp_gs <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

b1_awayteamstemp_gs <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

b1_goalscombined <- c(b1_homegoalscored,b1_awaygoalscored)
b1_teamscombined <- c(b1_hometeamstemp_gs,b1_awayteamstemp_gs)

b1_goalscoredround <- data.frame(b1_teamscombined,b1_goalscombined)

b1_goalscoredround <- b1_goalscoredround[order(b1_goalscoredround$b1_teamscombined),]
b1_goalscoredround$b1_teamscombined <- NULL
b1_goalscoredmatrix[,i_b1_krounds] <- b1_goalscoredround

}

b1_goalscoredmatrix <- cbind(b1_teams,b1_goalscoredmatrix)

b1_goalscoredmatrix

##########################################################################
##########################################################################
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goalconcededmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goalconcededround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoalconceded <- B1_rounds$FTAG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoalconceded <- B1_rounds$FTHG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_gc <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_gc <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalsconcededcombined <- c(b1_homegoalconceded,b1_awaygoalconceded)
  b1_teamscombined_gc <- c(b1_hometeamstemp_gc,b1_awayteamstemp_gc)

  b1_goalconcededround <- data.frame(b1_teamscombined_gc,b1_goalsconcededcombined)

  b1_goalconcededround <- b1_goalconcededround[order(b1_goalconcededround$b1_teamscombined_gc),]
  b1_goalconcededround$b1_teamscombined_gc <- NULL
  b1_goalconcededmatrix[,i_b1_krounds] <- b1_goalconcededround

}

b1_goalconcededmatrix <- cbind(b1_teams,b1_goalconcededmatrix)

b1_goalconcededmatrix

#################################################################
#################################################################
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_goaltotalmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_goaltotalround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homegoaltotal <- B1_rounds$TG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaygoaltotal <- B1_rounds$TG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_tg <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_tg <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_goalscombined_tg <- c(b1_homegoaltotal,b1_awaygoaltotal)
  b1_teamscombined_tg <- c(b1_hometeamstemp_tg,b1_awayteamstemp_tg)

  b1_goaltotalround <- data.frame(b1_teamscombined_tg,b1_goalscombined_tg)

  b1_goaltotalround <- b1_goaltotalround[order(b1_goaltotalround$b1_teamscombined_tg),]
  b1_goaltotalround$b1_teamscombined_tg <- NULL
  b1_goaltotalmatrix[,i_b1_krounds] <- b1_goaltotalround

}

b1_goaltotalmatrix <- cbind(b1_teams,b1_goaltotalmatrix)

b1_goaltotalmatrix
############################################################################################
############################################################################################
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_formmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_formround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homeform <- B1_rounds$FTR[B1_rounds$b1_matchday == i_b1_krounds]

  b1_homeform <- sub("H","W",b1_homeform)
  b1_homeform <- sub("A","L",b1_homeform)

  b1_awayform <- B1_rounds$FTR[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayform <- sub("A","W",b1_awayform)
  b1_awayform <- sub("H","L",b1_awayform)

  b1_hometeamstemp_form <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_form <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_formcombined <- c(b1_homeform,b1_awayform)
  b1_teamscombined_form <- c(b1_hometeamstemp_form,b1_awayteamstemp_form)

  b1_formround <- data.frame(b1_teamscombined_form,b1_formcombined)

  b1_formround <- b1_formround[order(b1_formround$b1_teamscombined_form),]
  b1_formround$b1_teamscombined_form <- NULL
  b1_formmatrix[,i_b1_krounds] <- b1_formround

}

b1_formmatrix <- cbind(b1_teams,b1_formmatrix)

#########################################################################################
########################################################################################


#b1
b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
b1_winmarginmatrix <- data.frame(matrix(nrow = length(b1_teams),ncol = b1_krounds))
b1_winmarginround <- c()
for(i_b1_krounds in 1:b1_krounds)
{
  b1_homewinmargin <- B1_rounds$FTHG - B1_rounds$FTAG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awaywinmargin <- B1_rounds$FTAG - B1_rounds$FTHG[B1_rounds$b1_matchday == i_b1_krounds]

  b1_hometeamstemp_wm <- B1_rounds$HomeTeam[B1_rounds$b1_matchday == i_b1_krounds]

  b1_awayteamstemp_wm <- B1_rounds$AwayTeam[B1_rounds$b1_matchday== i_b1_krounds]

  b1_winmargincombined <- c(b1_homewinmargin,b1_awaywinmargin)
  b1_teamscombined_wm <- c(b1_hometeamstemp_wm,b1_awayteamstemp_wm)

  b1_winmarginround <- data.frame(b1_teamscombined_wm,b1_goalsconcededcombined)

  b1_winmarginround <- b1_winmarginround[order(b1_winmarginround$b1_teamscombined_wm),]
  b1_winmarginround$b1_teamscombined_wm <- NULL
  b1_winmarginmatrix[,i_b1_krounds] <- b1_winmarginround

}

b1_winmarginmatrix <- cbind(b1_teams,b1_winmarginmatrix)
###############################################################################################

dftest <- SP1_spread[SP1_spread$HomeTeam == "Real Madrid" | SP1_spread$AwayTeam == "Real Madrid",]
df2test <- SP1_spread[SP1_spread$HomeTeam == "Ath Madrid" |  SP1_spread$AwayTeam == "Ath Madrid",]
temp_analysistest <- rbind(dftest,df2test)
write.csv(temp_analysistest,'realath.csv')


epl_FA_details <- load_fotmob_match_details(
  country = "ENG",
  league_name = "FA Cup"
)
####################################################
b1_away_gs[,2]
hist((e0_away_gs[,2]))

####################################################
parallel::detectCores()

#############################################################
myfixturesb1 <- subset(myfixtures,Div == "B1")
#############################################################
final_doublefixture_b1 <- c()
final_first_column_b1 <- c()
final_second_column_b1 <- c()

for(dbfixture_b1 in 1:nrow(myfixturesb1))
{
test_fixture_b1 <- myfixturesb1[dbfixture_b1,]
test_fixture_inv_b1 <- test_fixture_b1[,c(1,3,2,4)]


final_test_fixture_b1 <- test_fixture_b1[rep(seq_len(nrow(test_fixture_b1)), each = 4),]
final_test_fixture_inv_b1 <- test_fixture_inv_b1[rep(seq_len(nrow(test_fixture_inv_b1)), each = 4),]


final_test_fixture_b1 <- final_test_fixture_b1[,c(2,3)]
final_test_fixture_inv_b1 <- final_test_fixture_inv_b1[,c(2,3)]

final_first_column_b1 <- c(final_test_fixture_b1[,c(1)],final_test_fixture_inv_b1[,c(1)])
final_second_column_b1 <- c(final_test_fixture_b1[,c(2)],final_test_fixture_inv_b1[,c(2)])

mid_doublefixture_b1 <- cbind(final_first_column_b1,final_second_column_b1)

final_doublefixture_b1 <- rbind(final_doublefixture_b1,mid_doublefixture_b1)


}

write.csv(final_doublefixture_b1,"finaldoublefixtureb1.csv")

############################################################################################

swz_eachround


allteams_2010present <- read.csv('../FDAS/allteams2010-present.csv')
nrow(allteams_2010present[allteams_2010present$CS == "1-0",])
#############################################################################################

#spread analysis
allteams20222023SOTSPREADS <- read.csv('../Documents/allteams20222023SOTSPREADS.csv')


#####################################################################################
#####################################################################################
#spread printer
#E0
for(e0_sn in 1:19){
df <- E0_spread[E0_spread$HomeTeam == final_doublefixture_e0div[e0_sn,1] | E0_spread$AwayTeam == final_doublefixture_e0div[e0_sn,1] ,]
df2 <- E0_spread[E0_spread$HomeTeam == final_doublefixture_e0div[e0_sn + 1,1] | E0_spread$AwayTeam == final_doublefixture_e0div[e0_sn + 1,1],]
temp_analysis <- rbind(df,df2)
path = "C:\\Users\\Kovan\\Rsoccer\\Spreads"
write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e0div[e0_sn,1],final_doublefixture_e0div[e0_sn + 1,1],".csv",sep = "_")))

}
#D1
for(d1_sn in 1:17){
  df <- D1_spread[D1_spread$HomeTeam == final_doublefixture_d1div[d1_sn,1] | D1_spread$AwayTeam == final_doublefixture_d1div[d1_sn,1] ,]
  df2 <- D1_spread[D1_spread$HomeTeam == final_doublefixture_d1div[d1_sn + 1,1] | D1_spread$AwayTeam == final_doublefixture_d1div[d1_sn + 1,1],]
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(34,35,36,37,38,39,40,41,42,43,44,45,46)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:33]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_d1div[d1_sn,1],final_doublefixture_d1div[d1_sn + 1,1],".csv",sep = "_")))

}

#I1
for(i1_sn in 1:19){
  df <- I1_spread[I1_spread$HomeTeam == final_doublefixture_i1div[i1_sn,1] | I1_spread$AwayTeam == final_doublefixture_i1div[i1_sn,1] ,]
  df2 <- I1_spread[I1_spread$HomeTeam == final_doublefixture_i1div[i1_sn + 1,1] | I1_spread$AwayTeam == final_doublefixture_i1div[i1_sn + 1,1],]
  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(34,35,36,37,38,39,40,41,42,43,44,45,46)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:33]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\Spreads"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_i1div[i1_sn,1],final_doublefixture_i1div[i1_sn + 1,1],".csv",sep = "_")))

}



###################################################################################
allteams20222023SOTSPREADS %>%

group_by(Div) %>%
  summarise(sumofbookings = sum(Bookings, na.rm = T ))

library('xlsx')
write.xlsx(laliga_players,'laliga_players.xlsx',sheetName = "laligaplayers")

tapply(allteams20222023SOTSPREADS$Bookings, allteams20222023SOTSPREADS$Div, FUN = mean)


b1_xshotsconversion_vec_ht <- as.vector(b1_shots_analysis[1,])
b1_xshotsconversion_vec_ht[is.na(b1_xshotsconversion_vec_ht)] <- ""
b1_xshotsconversion_vec_ht <- b1_xshotsconversion_vec_ht[b1_xshotsconversion_vec_ht != ""]
b1_xshotsconversion_vec_ht  <-tail(b1_xshotsconversion_vec_ht,1)

b1_shots_analysis[1,]

b1_xshotsconversion_vec_ht

###############################################
####mean and standard deviations"
library('dplyr')
allteams2023means <- allteams20222023 %>%
  group_by(Div) %>%
   dplyr::summarise(avg_goals = mean(TG),
             n = n())
################################################
allteams2023sd <- allteams20222023 %>%
  group_by(Div) %>%
  dplyr::summarise(sd_goals = sd(TG),
                   n = n())

allteams2023goalstats <- cbind(allteams2023means,allteams2023sd)
unlink('goalstats.csv')
write.csv(allteams2023goalstats,'goalstats.csv')
####################################################################
library('dplyr')
allteams2023meanscorners <- allteams20222023 %>%
  group_by(Div) %>%
  dplyr::summarise(avg_corners = mean(TC),
                   n = n())
################################################
allteams2023sdcorners <- allteams20222023 %>%
  group_by(Div) %>%
  dplyr::summarise(sd_goals = sd(TC),
                   n = n())

allteams2023cornerstats <- cbind(allteams2023meanscorners,allteams2023sdcorners)
unlink('cornerstats.csv')
write.csv(allteams2023cornerstats,'cornersstats.csv')
##################################################################################

library('dplyr')

allteams2023corneranalysis <- allteams20222023[allteams20222023$TC >= '10',] %>%

  group_by(CS) %>%
  dplyr::summarise(n = n(),)


View(sort(allteams2023corneranalysis))
write.csv(allteams2023corneranalysis,'corneranalysis.csv')
##############################################################################################
allteams20222023[allteams20222023$TC == '2',]
###############################################################################################################################
B1_fixtures_clone_final <- B1_fixtures_clone[,-c(8,9,10,27)]
B1_fixtures_clone_final[,'sep'] <- ''

b1_dmprediction <-  b1_picks[,c(4,5,6,7,8)]
b1_dmprediction[,'sep2'] <- ''

b1_avgyellow <- b1_picks[,c(9,10)]
b1_avgyellow[,'sep3'] <- ''

b1_avgcorners <- b1_picks[,c(11,12)]
b1_avgcorners[,'sep4'] <- ''

b1_goals <- B1_fixtures[,c(10,11)]
b1_goals$b1_xGH <- round(b1_goals$b1_xGH, digits = 2)
b1_goals$b1_xGA <- round(b1_goals$b1_xGA, digits = 2)
b1_goals$b1_TxG <- b1_goals$b1_xGH + b1_goals$b1_xGA
b1_goals[,'sep5'] <- ''

b1_shots <- B1_fixtures_sot[,c(10,11)]
b1_shots$b1_xHST <- round(b1_shots$b1_xHST, digits = 2)
b1_shots$b1_xAST <- round(b1_shots$b1_xAST, digits = 2)
b1_shots$TxSOT <- b1_shots$b1_xHST + b1_shots$b1_xAST
b1_shots[,'sep6'] <- ''

b1_fouls <- B1_fixtures_fo[,c(10,11)]
b1_fouls$b1_xHF <- round(b1_fouls$b1_xHF, digits = 2)
b1_fouls$b1_xAF <- round(b1_fouls$b1_xAF, digits = 2)
b1_fouls$b1_TxF <- b1_fouls$b1_xHF + b1_fouls$b1_xAF

b1_ycpf <- b1_picks[,c(15,16)]
b1_fouls <- cbind(b1_fouls,b1_ycpf)
b1_fouls$HYCPF <- as.numeric(b1_fouls$HYCPF)
b1_fouls$AYCPF <- as.numeric(b1_fouls$AYCPF)
b1_fouls$x_hyc <- (b1_fouls$b1_xHF) * (b1_fouls$HYCPF)
b1_fouls$x_ayc <- (b1_fouls$b1_xAF) * (b1_fouls$AYCPF)
b1_fouls$x_TYC <- round((b1_fouls$x_hyc + b1_fouls$x_ayc),digits = 2)
b1_fouls[,'sep7'] <- ''

b1_bookings <- B1_fixtures_yc[,c(10,11)]
b1_bookings$b1_xHYC <- round(b1_bookings$b1_xHYC, digits = 2)
b1_bookings$b1_xAYC <- round(b1_bookings$b1_xAYC, digits = 2)
b1_bookings$b1_TYcards <- b1_bookings$b1_xHYC + b1_bookings$b1_xAYC
b1_bookings[,'sep8'] <- ''

b1_corners <- B1_fixtures_co[,c(10,11)]
b1_corners$b1_xHCOC <- round(b1_corners$b1_xHCOC, digits = 2)
b1_corners$b1_xACOC <- round(b1_corners$b1_xACOC, digits = 2)
b1_corners$b1_TCOs <- b1_corners$b1_xHCOC + b1_corners$b1_xACOC
b1_corners[,'sep9'] <- ''

b1_shotsconversion <- b1_picks[,c(13,14)]
b1_shotsconversion <- cbind(b1_shotsconversion,b1_shots)
b1_shotsconversion$HXSC <- as.numeric(b1_shotsconversion$HXSC)
b1_shotsconversion$AXSC <- as.numeric(b1_shotsconversion$AXSC)
b1_shotsconversion$b1_hXgoals <- round((b1_shotsconversion$HXSC * b1_shotsconversion$b1_xHST), digits = 2)
b1_shotsconversion$b1_aXgoals <- round((b1_shotsconversion$AXSC * b1_shotsconversion$b1_xAST), digits = 2)
b1_shotsconversion$Xgoals <- b1_shotsconversion$b1_hXgoals + b1_shotsconversion$b1_aXgoals

B1_all <- cbind(B1_fixtures_clone_final,b1_dmprediction,b1_avgyellow,b1_avgcorners,b1_goals,b1_shots,b1_fouls,b1_bookings,b1_corners,b1_shotsconversion)
write.csv(B1_all,'B1_all.csv')
####################################################################################################################################################################################

library('worldfootballR')
library('dplyr')
library('mgsub')

#E0
epl_match_details_adv <- load_fotmob_match_details(
  country = "ENG",
  league_name = "Premier League"
)
#select just the current season
epl_startdate_adv <- which(epl_match_details_adv$match_time_utc == "Fri, Aug 5, 2022, 19:00 UTC")
epl_startindex_adv <- nrow(epl_match_details_adv) - epl_startdate[1]
epl_match_details_adv <- tail(epl_match_details_adv,epl_startindex_adv)

View(epl_match_details_adv)
epl_match_details_adv$home_team <- mgsub(epl_match_details_adv$home_team,c("AFC Bournemouth","Brighton & Hove Albion","Leeds United","Leicester City","Manchester United","Manchester City","Newcastle United","Nottingham Forest","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Bournemouth","Brighton","Leeds","Leicester","Man United","Man City","Newcastle","Nottm Forest","Tottenham","West Ham","Wolves"))
epl_match_details_adv$away_team <- mgsub(epl_match_details_adv$away_team,c("AFC Bournemouth","Brighton & Hove Albion","Leeds United","Leicester City","Manchester United","Manchester City","Newcastle United","Nottingham Forest","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Bournemouth","Brighton","Leeds","Leicester","Man United","Man City","Newcastle","Nottm Forest","Tottenham","West Ham","Wolves"))
epl_match_details_adv$matchid <- paste(epl_match_details_adv$home_team,epl_match_details_adv$away_team,sep = "-")

unique(epl_match_details_adv$match_id)

View(fotmob_get_match_team_stats(3901170))
sd(allteams20222023$ACO)
###########################################################################################################################
library(stringr)
library(stringi)
final_b1_cor <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
suml6_b1_cor <- c()
avg_b1_cor <- c()
sd_b1_one_cor <- c()
sum_b1_fivemore_cor <- c()
sum_b1_fourless_cor <- c()
l6_form_b1_corsplitted <- c()
form_b1_cor <- c()
for(index_b1_cor in 1:length(b1_teams))
{
  for(index_b1_cor_cols in 1:b1_totalrounds)
  {
    index_b1_cor  <- row.names(b1_coawarded_h) == b1_teams[index_b1_cor]
    form_b1_cor <- b1_coawarded_h[index_b1_cor]
    deleted_form_b1_cor <- form_b1_cor[!form_b1_cor[] == ""]
    l6_form_b1_cor <- deleted_form_b1_cor #tail(deleted_form_b1_cor,b1_last_n_games)
    l6_form_b1_cor <- as.numeric(l6_form_b1_cor)
    suml6_b1_cor[index_b1_cor] <- sum(l6_form_b1_cor)
    suml6_b1_cor[index_b1_cor] <- paste(suml6_b1_cor[index_b1_cor],sep = "")
    avg_b1_cor[index_b1_cor] <- mean(l6_form_b1_cor)
    avg_b1_cor[index_b1_cor] <- paste(avg_b1_cor[index_b1_cor],sep = "")
    sd_b1_one_cor[index_b1_cor] <- sd(l6_form_b1_cor)
    sd_b1_one_cor[index_b1_cor] <- paste(sd_b1_one_cor[index_b1_cor],sep = "")
    sum_b1_fivemore_cor[index_b1_cor] <- length(which(l6_form_b1_cor >= 5))
    sum_b1_fivemore_cor[index_b1_cor] <- paste(sum_b1_fivemore_cor[index_b1_cor],sep = "")
    sum_b1_fourless_cor[index_b1_cor] <- length(which(l6_form_b1_cor <= 4))
    sum_b1_fourless_cor[index_b1_cor] <- paste(sum_b1_fourless_cor[index_b1_cor],sep = "")
    #l6_form_b1_cor <- as.character(l6_form_b1_cor)
    #l6_form_b1_cor_flattened <- stri_paste(l6_form_b1_cor,collapse = '')
    #l6_form_b1_corsplitted <- as.numeric(strsplit(as.character(l6_form_b1_cor_flattened),"")[[1]])
    final_b1_cor[index_b1_cor,index_b1_cor_cols] <- l6_form_b1_cor[index_b1_cor_cols]
  }
}


final_b1_cor[is.na(final_b1_cor)] <- ""
b1_coawardedmatrix <- cbind(b1_teams,final_b1_cor,suml6_b1_cor,avg_b1_cor,sd_b1_one_cor,sum_b1_fivemore_cor,sum_b1_fourless_cor)
#############################################################################################################################################
#############################################################################################################################################


final_b1_ycards <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
suml6_b1_ycards <- c()
avg_b1_ycards <- c()
sd_b1_one_ycards <- c()
sum_b1_threemore_ycards <- c()
sum_b1_twoless_ycards <- c()
l6_form_b1_ycardssplitted <- c()
form_b1_ycards <- c()
for(index_b1_ycards in 1:length(b1_teams))
{
  for(index_b1_ycards_cols in 1:b1_totalrounds)
  {
    index_b1_ycards  <- row.names(b1_yellowscored_h) == b1_teams[index_b1_ycards]
    form_b1_ycards <- b1_yellowscored_h[index_b1_ycards]
    deleted_form_b1_ycards <- form_b1_ycards[!form_b1_ycards[] == ""]
    l6_form_b1_ycards <- deleted_form_b1_ycards #tail(deleted_form_b1_ycards,b1_last_n_games)
    l6_form_b1_ycards <- as.numeric(l6_form_b1_ycards)
    suml6_b1_ycards[index_b1_ycards] <- sum(l6_form_b1_ycards)
    suml6_b1_ycards[index_b1_ycards] <- paste(suml6_b1_ycards[index_b1_ycards],sep = "")
    avg_b1_ycards[index_b1_ycards] <- mean(l6_form_b1_ycards)
    avg_b1_ycards[index_b1_ycards] <- paste(avg_b1_ycards[index_b1_ycards],sep = "")
    sd_b1_one_ycards[index_b1_ycards] <- sd(l6_form_b1_ycards)
    sd_b1_one_ycards[index_b1_ycards] <- paste(sd_b1_one_ycards[index_b1_ycards],sep = "")
    sum_b1_threemore_ycards[index_b1_ycards] <- length(which(l6_form_b1_ycards >= 3))
    sum_b1_threemore_ycards[index_b1_ycards] <- paste(sum_b1_threemore_ycards[index_b1_ycards],sep = "")
    sum_b1_twoless_ycards[index_b1_ycards] <- length(which(l6_form_b1_ycards <= 2))
    sum_b1_twoless_ycards[index_b1_ycards] <- paste(sum_b1_twoless_ycards[index_b1_ycards],sep = "")
    #l6_form_b1_ycards <- as.character(l6_form_b1_ycards)
    #l6_form_b1_ycards_flattened <- stri_paste(l6_form_b1_ycards,collapse = '')
    #l6_form_b1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_b1_ycards_flattened),"")[[1]])
    final_b1_ycards[index_b1_ycards,index_b1_ycards_cols] <- l6_form_b1_ycards[index_b1_ycards_cols]
  }
}


final_b1_ycards[is.na(final_b1_ycards)] <- ""
b1_yellowscoredmatrix <- cbind(b1_teams,final_b1_ycards,suml6_b1_ycards,avg_b1_ycards,sd_b1_one_ycards,sum_b1_threemore_ycards,sum_b1_twoless_ycards)
##########################################################################################################################################################


E1_spread <- subset(allteams20222023,Div =="E1")
E1_spread$n <- E1_spread$TG * 1
E1_spread$Bookings <- (E1_spread$HY *10 + E1_spread$HR *25) + (E1_spread$AY*10 + E1_spread$AR*25)
E1_spread$Crossbookings <- (E1_spread$HY *10 + E1_spread$HR *25)*(E1_spread$AY*10 + E1_spread$AR*25)
E1_spread$GoalsXbookings <- (E1_spread$Bookings)*(E1_spread$TG)
E1_spread$CornersXbookings <- (E1_spread$TC)*(E1_spread$Bookings)
E1_spread$GoalsXcorners <- (E1_spread$TG)*(E1_spread$TC)
E1_spread$GoalsXcornerXbookings <- (E1_spread$TG)*(E1_spread$TC)*(E1_spread$Bookings)

unlink('SpreadsN/E1/*')
for(e1_sn in 1:23){
  df <- tail(E1_spread[E1_spread$HomeTeam == final_doublefixture_e1[e1_sn,1] | E1_spread$AwayTeam == final_doublefixture_e1[e1_sn,1] ,],spreadn)

  df2 <- tail(E1_spread[E1_spread$HomeTeam == final_doublefixture_e1[e1_sn + 1,1] | E1_spread$AwayTeam == final_doublefixture_e1[e1_sn + 1,1],],spreadn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:36]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rsoccer\\SpreadsN\\E1"
  write.csv(temp_analysis,file.path(path,paste(final_doublefixture_e1[e1_sn,1],final_doublefixture_e1[e1_sn + 1,1],".csv",sep = "_")))


View(fotmob_get_season_stats(country ="ESP",league_name ="LaLiga",season_name = "2022/2023",team_or_player = "player",stat_name = "Accurate passes per 90","Assists"))


epl_team_xg_2021 <- fotmob_get_season_stats(
  country = "ENG",
  league_name = "Premier League",
  season = "2022/2023",
  stat_name = "Expected goals",
  team_or_player = "team"
)
#################################################
Homeshirts <- aggregate(I1_spread$ShirtsXbookings, by = list(I1_spread$HomeTeam), FUN = mean)
Homeshirtsavg <- aggregate(B1$FTHG, by = list(B1$HomeTeam),mean)

##################################################################
UCONFL <- rbind(B1_spread,T1_spread)
#UCL <- read.csv('UCL.csv')
unlink('UCONFL.csv')
write.csv(UCONFL,'UCONFL.csv')
###############################################################################
df <- tail(UCONFL[UCONFL$HomeTeam =="Buyuksehyr" | UCONFL$AwayTeam =="Buyuksehyr",],6)
df2 <- tail(UCONFL[UCONFL$HomeTeam == "Gent" | UCONFL$AwayTeam == "Gent",],6)
temp_analysis <- rbind(df,df2)
temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(37,38,39,40,41,42,43)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:36]
temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
write.csv(temp_analysis,'Temp/BasakhesirvsGent.csv')

write.xlsx(final_b1_cs,'CSFORM.xlsx',sheetName = "L6", append = TRUE)
#####################################################################################################################
#test team against writable to excel

final_b1_hf_against <- c()
for(index_b1_hf_against in 1:length(b1_teams))
{
  index_b1_hf_against <- row.names(b1_form_team_against_h) == b1_teams[index_b1_hf_against]
  form_b1_hf_against <- b1_form_team_against_h[index_b1_hf_against]
  deleted_form_b1_hf_against <- form_b1_hf_against[!form_b1_hf_against[] == ""]
  l6_form_b1_hf_against <- tail(deleted_form_b1_hf_against,b1_last_n_games)
  l6_form_b1_hf_against <- paste(l6_form_b1_hf_against,collapse = " ")
  final_b1_hf_against[index_b1_hf_against] <- rbind(paste(b1_teams[index_b1_hf_against],l6_form_b1_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}

Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
final_b1_teamagainst <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
l6_form_b1_teamagainstsplitted <- c()
form_b1_teamagainst <- c()
for(index_b1_teamagainst in 1:length(b1_teams))
{
  for(index_b1_teamagainst_cols in 1:b1_totalrounds)
  {
    index_b1_teamagainst  <- row.names(b1_form_team_against_h) == b1_teams[index_b1_teamagainst]
    form_b1_teamagainst <- b1_form_team_against_h[index_b1_teamagainst ]
    deleted_form_b1_teamagainst <- form_b1_teamagainst[!form_b1_teamagainst[] == ""]
    l6_form_b1_teamagainst <- tail(deleted_form_b1_teamagainst,b1_last_n_games)
    l6_form_b1_teamagainst <- as.character(l6_form_b1_teamagainst)
    #l6_form_b1_teamagainst_flattened <- stri_paste(l6_form_b1_teamagainst,collapse = '')
    #l6_form_b1_teamagainstsplitted <- strsplit(l6_form_b1_teamagainst_flattened,"")[[1]]
    final_b1_teamagainst[index_b1_teamagainst,index_b1_teamagainst_cols] <- l6_form_b1_teamagainst[index_b1_teamagainst_cols]
  }
}

b1_teamagainst <- cbind(b1_teams,final_b1_teamagainst)
write.xlsx(final_b1_teamagainst,"Divisions/B1.xlsx",append = T , sheetName = "Teamagainst")
########################################################################################################################################
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
final_b1_correctscoreform <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
l6_form_b1_correctscoreformsplitted <- c()
form_b1_correctscoreform <- c()
for(index_b1_correctscoreform in 1:length(b1_teams))
{
  for(index_b1_correctscoreform_cols in 1:b1_totalrounds)
  {
    index_b1_correctscoreform  <- row.names(b1_csform_h) == b1_teams[index_b1_correctscoreform]
    form_b1_correctscoreform <- b1_csform_h[index_b1_correctscoreform ]
    deleted_form_b1_correctscoreform <- form_b1_correctscoreform[!form_b1_correctscoreform[] == ""]
    l6_form_b1_correctscoreform <- tail(deleted_form_b1_correctscoreform,b1_last_n_games)
    l6_form_b1_correctscoreform <- as.character(l6_form_b1_correctscoreform)
    #l6_form_b1_correctscoreform_flattened <- stri_paste(l6_form_b1_correctscoreform,collapse = '')
    #l6_form_b1_correctscoreformsplitted <- strsplit(l6_form_b1_correctscoreform_flattened,"")[[1]]
    final_b1_correctscoreform[index_b1_correctscoreform,index_b1_correctscoreform_cols] <- l6_form_b1_correctscoreform[index_b1_correctscoreform_cols]
  }
}

b1_correctscoreform <- cbind(b1_teams,final_b1_correctscoreform)
write.xlsx(b1_correctscoreform,"Divisions/B1.xlsx",append = T , sheetName = "CSform")



b1_correctscoreform














