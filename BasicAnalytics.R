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
url <- 'https://www.basketball-reference.com/boxscores/'
webpage <- read_html(url)
data <- webpage %>% html_nodes(css ='table') %>% html_table()
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














