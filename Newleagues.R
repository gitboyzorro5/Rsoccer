library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
library('mgsub')
######################
# aut_last_n_games <- 6
# arg_last_n_games <- 6
# bra_last_n_games <- 6
# chn_last_n_games <- 6
# dnk_last_n_games <- 6
# fin_last_n_games <- 6
# irl_last_n_games <- 6
# jpn_last_n_games <- 6
# mex_last_n_games <- 6
# mls_last_n_games <- 6
# nor_last_n_games <- 6
# pol_last_n_games <- 6
# rou_last_n_games <- 6
# rus_last_n_games <- 6
# swe_last_n_games <- 6
# swz_last_n_games <- 6
######################
aut_last_n_games <- aut_games_played[1]
arg_last_n_games <- arg_games_played[1]
bra_last_n_games <- bra_games_played[1]
chn_last_n_games <- chn_games_played[1]
dnk_last_n_games <- dnk_games_played[1]
fin_last_n_games <- fin_games_played[1]
irl_last_n_games <- irl_games_played[1]
jpn_last_n_games <- jpn_games_played[1]
mex_last_n_games <- mex_games_played[1]
mls_last_n_games <- mls_games_played[1]
nor_last_n_games <- nor_games_played[1]
pol_last_n_games <- pol_games_played[1]
rou_last_n_games <- rou_games_played[1]
rus_last_n_games <- rus_games_played[1]
swe_last_n_games <- swe_games_played[1]
swz_last_n_games <- swz_games_played[1]

##########3
source("ARG.R")
source("AUT.R")
source("BRAZIL.R")
source("CHN.R")
source("DNK.R")
source("FIN.R")
source("IRL.R")
source("JPN.R")
source("MEX.R")
source("MLS.R")
source("NOR.R")
source("POL.R")
source("ROU.R")
source("RUS.R")
source("SWE.R")
source("SWZ.R")
#####################
R_home_newleagues <- rbind(arg_home_poisson,aut_home_poisson,bra_home_poisson,chn_home_poisson,dnk_home_poisson,fin_home_poisson,irl_home_poisson,jpn_home_poisson,mex_home_poisson,nor_home_poisson,pol_home_poisson,rou_home_poisson,rus_home_poisson,swe_home_poisson,mls_home_poisson,swz_home_poisson)
R_away_newleagues <- rbind(arg_away_poisson,aut_away_poisson,bra_away_poisson,chn_away_poisson,dnk_away_poisson,fin_away_poisson,irl_away_poisson,jpn_away_poisson,mex_away_poisson,nor_away_poisson,pol_away_poisson,rou_away_poisson,rus_away_poisson,swe_away_poisson,mls_away_poisson,swz_away_poisson)

unlink('R_home_newleagues.csv')
unlink('R_away_newleagues.csv')

R_home_newleagues <- mgsub(R_home_newleagues,c("ARG","AUT","BRA","CHN","DNK","FIN","IRL","JPN","MEX","NOR","POL","ROU","RUS","SWE","SWZ"),c("Liga Profesional","Admiral Bundesliga","Serie A","Super League","Superliga","Veikkausliiga","Premier Division","J1 League","Liga MX","Eliteserien","Ekstraklasa","Liga 1","Premier League","Allsvenskan","Swiss"))
R_away_newleagues <- mgsub(R_away_newleagues,c("ARG","AUT","BRA","CHN","DNK","FIN","IRL","JPN","MEX","NOR","POL","ROU","RUS","SWE","SWZ"),c("Liga Profesional","Admiral Bundesliga","Serie A","Super League","Superliga","Veikkausliiga","Premier Division","J1 League","Liga MX","Eliteserien","Ekstraklasa","Liga 1","Premier League","Allsvenskan","Swiss"))

write.csv(R_home_newleagues,'R_home_newleagues.csv')
write.csv(R_away_newleagues,'R_away_newleagues.csv')
