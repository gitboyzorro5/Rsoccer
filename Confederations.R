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

CONF <- read.csv('results.csv')
sort(unique(CONF$tournament))
CONCACAF <- CONF[CONF$tournament == "CONCACAF Nations League",]
CONCACAF_teams <- sort(unique(CONCACAF$home_team))
CONCACAF_fed <-  as.data.frame(CONCACAF_teams)
CONCACAF_fed$div <- "CONCACAF"
###################################################
AFC <- CONF[CONF$tournament == "AFC Asian Cup qualification",]
AFC_teams <- sort(unique(AFC$home_team))
AFC_fed <-  as.data.frame(AFC_teams)
AFC_fed$div <- "AFC"
AFC_fed
###################################################
CAF <- CONF[CONF$tournament == "African Cup of Nations",]
CAF_teams <- sort(unique(CAF$home_team))
CAF_teams
CAF_fed <-  as.data.frame(CAF_teams)
CAF_fed$div <- "CAF"
################################################
UEFA <- CONF[CONF$tournament == "UEFA Nations League",]
UEFA_teams <- sort(unique(UEFA$home_team))
UEFA_fed <-  as.data.frame(UEFA_teams)
UEFA_fed$div <- "UEFA"
################################################
OFC <- CONF[CONF$tournament == "Oceania Nations Cup qualification",]
OFC_teams <- sort(unique(OFC$home_team))
OFC_fed <-  as.data.frame(OFC_teams)
OFC_fed$div <- "OFC"
###############################################
CONMEBOL <- CONF[CONF$tournament == "Copa AmÃ©rica",]
CONMEBOL_teams <- sort(unique(CONMEBOL$home_team))
CONMEBOL_fed <-  as.data.frame(CONMEBOL_teams)
CONMEBOL_fed$div <- "CONMEBOL"
CONMEBOL_fed
###############################################
FIFA_conf <- mapply(c,CONCACAF_fed,AFC_fed,CAF_fed,UEFA_fed,OFC_fed,CONMEBOL_fed)
FIFA_conf <- as.data.frame(FIFA_conf)




