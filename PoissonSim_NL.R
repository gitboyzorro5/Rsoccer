library(mgsub)
#################################################
###
poisson_round_nl <- 6
allhomepoisson_nl <- c()
allawaypoisson_nl <- c()

while (poisson_round_nl <= 22){
ROUND_20212022_nl <- poisson_round_nl

  #########################################################
  # AUT <- AUT_rounds[AUT_rounds$aut_matchday <= ROUND_20212022_nl,]
  # AUT$HG <- as.numeric(AUT$HG)
  # AUT$AG <- as.numeric(AUT$AG)
  # AUT$TG <- as.numeric(AUT$TG)
  ARG <- ARG_rounds[ARG_rounds$arg_matchday <= ROUND_20212022_nl,]
  ARG$HG <- as.numeric(ARG$HG)
  ARG$AG <- as.numeric(ARG$AG)
  ARG$TG <- as.numeric(ARG$TG)
  # BRA <- BRA_rounds[BRA_rounds$bra_matchday <= ROUND_20212022_nl,]
  # BRA$HG <- as.numeric(BRA$HG)
  # BRA$AG <- as.numeric(BRA$AG)
  # BRA$TG <- as.numeric(BRA$TG)
  # CHN <- CHN_rounds[CHN_rounds$chn_matchday <= ROUND_20212022_nl,]
  # CHN$HG <- as.numeric(CHN$HG)
  # CHN$AG <- as.numeric(CHN$AG)
  # CHN$TG <- as.numeric(CHN$TG)
  # DNK <- DNK_rounds[DNK_rounds$dnk_matchday <= ROUND_20212022_nl,]
  # DNK$HG <- as.numeric(DNK$HG)
  # DNK$AG <- as.numeric(DNK$AG)
  # DNK$TG <- as.numeric(DNK$TG)
  # FIN <- FIN_rounds[FIN_rounds$fin_matchday <= ROUND_20212022_nl,]
  # FIN$HG <- as.numeric(FIN$HG)
  # FIN$AG <- as.numeric(FIN$AG)
  # FIN$TG <- as.numeric(FIN$TG)
  # IRL <- IRL_rounds[IRL_rounds$irl_matchday <= ROUND_20212022_nl,]
  # IRL <- as.data.frame(IRL)
  # IRL$HG <- as.numeric(IRL$HG)
  # IRL$AG <- as.numeric(IRL$AG)
  # IRL$TG <- as.numeric(IRL$TG)
  # JPN <- JPN_rounds[JPN_rounds$jpn_matchday <= ROUND_20212022_nl,]
  # JPN$HG <- as.numeric(JPN$HG)
  # JPN$AG <- as.numeric(JPN$AG)
  # JPN$TG <- as.numeric(JPN$TG)
  # MEX <- MEX_rounds[MEX_rounds$mex_matchday <= ROUND_20212022_nl,]
  # MEX$HG <- as.numeric(MEX$HG)
  # MEX$AG <- as.numeric(MEX$AG)
  # MEX$TG <- as.numeric(MEX$TG)
  # MLS <- MLS_rounds[MLS_rounds$mls_matchday <= ROUND_20212022_nl,]
  # MLS$HG <- as.numeric(MLS$HG)
  # MLS$AG <- as.numeric(MLS$AG)
  # MLS$TG <- as.numeric(MLS$TG)
  # NOR <- NOR_rounds[NOR_rounds$nor_matchday <= ROUND_20212022_nl,]
  # NOR$HG <- as.numeric(NOR$HG)
  # NOR$AG <- as.numeric(NOR$AG)
  # NOR$TG <- as.numeric(NOR$TG)
  # POL <- POL_rounds[POL_rounds$pol_matchday <= ROUND_20212022_nl,]
  # POL$HG <- as.numeric(POL$HG)
  # POL$AG <- as.numeric(POL$AG)
  # POL$TG <- as.numeric(POL$TG)
  # ROU <- ROU_rounds[ROU_rounds$rou_matchday <= ROUND_20212022_nl,]
  # ROU$HG <- as.numeric(ROU$HG)
  # ROU$AG <- as.numeric(ROU$AG)
  # ROU$TG <- as.numeric(ROU$TG)
  # RUS <- RUS_rounds[RUS_rounds$rus_matchday <= ROUND_20212022_nl,]
  # RUS$HG <- as.numeric(RUS$HG)
  # RUS$AG <- as.numeric(RUS$AG)
  # RUS$TG <- as.numeric(RUS$TG)
  # SWE <- SWE_rounds[SWE_rounds$swe_matchday <= ROUND_20212022_nl,]
  # SWE$HG <- as.numeric(SWE$HG)
  # SWE$AG <- as.numeric(SWE$AG)
  # SWE$TG <- as.numeric(SWE$TG)
  # SWZ <- SWZ_rounds[SWZ_rounds$swz_matchday <= ROUND_20212022_nl,]
  # SWZ$HG <- as.numeric(SWZ$HG)
  # SWZ$AG <- as.numeric(SWZ$AG)
  # SWZ$TG <- as.numeric(SWZ$TG)
  #############################################################
  #source("midpoisson.R")
  ############################################################
arg_home_games <- c()
arg_away_games <-c()
for (i_arg in 1:length(arg_teams))
{

  arg_home_games[i_arg] <- nrow(ARG[ARG$Home == arg_teams[i_arg],])
  arg_away_games[i_arg]  <- nrow(ARG[ARG$Away == arg_teams[i_arg],])

}

arg_games_played <- arg_home_games + arg_away_games

#home goals scored
arg_home_gs <- aggregate(ARG$HG, by = list(ARG$Home), FUN = sum)
arg_home_gs_avg <- aggregate(ARG$HG, by = list(ARG$Home),mean)
arg_home_scoring <- merge(arg_home_gs,arg_home_gs_avg, by='Group.1',all = T)
names(arg_home_scoring)[names(arg_home_scoring) == "x.x"] <- "TFthg"
names(arg_home_scoring)[names(arg_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
arg_away_gs <- aggregate(ARG$AG, by = list(ARG$Away), FUN = sum)
arg_away_gs_avg <- aggregate(ARG$AG, by = list(ARG$Away),mean)
arg_away_scoring <- merge(arg_away_gs,arg_away_gs_avg, by='Group.1',all = T)
names(arg_away_scoring)[names(arg_away_scoring) == "x.x"] <- "TFtag"
names(arg_away_scoring)[names(arg_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
arg_scoring <- merge(arg_home_scoring,arg_away_scoring,by='Group.1',all = T)
arg_scoring$TGS <- arg_scoring$TFthg + arg_scoring$TFtag

#home goals conceded
arg_home_gc <- aggregate(ARG$AG, by = list(ARG$Home), FUN = sum)
arg_home_gc_avg <- aggregate(ARG$AG, by = list(ARG$Home),mean)
arg_home_conceding <- merge(arg_home_gc,arg_home_gc_avg, by='Group.1',all = T)
names(arg_home_conceding)[names(arg_home_conceding) == "x.x"] <- "TFthc"
names(arg_home_conceding)[names(arg_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
arg_away_gc <- aggregate(ARG$HG, by = list(ARG$Away), FUN = sum)
arg_away_gc_avg <- aggregate(ARG$HG, by = list(ARG$Away),mean)
arg_away_conceding <- merge(arg_away_gc,arg_away_gc_avg, by='Group.1',all = T)
names(arg_away_conceding)[names(arg_away_conceding) == "x.x"] <- "TFtac"
names(arg_away_conceding)[names(arg_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
arg_conceding <- merge(arg_home_conceding,arg_away_conceding,by='Group.1',all = T)
arg_conceding$TGC <- arg_conceding$TFthc + arg_conceding$TFtac

#poisson model
#get total games played
arg_GP <- nrow(ARG)
#Calculate total home goals for each division
arg_T_HG <- sum(arg_home_gs$x)
#calculate average home goal
arg_avg_HG <- round(arg_T_HG /arg_GP, digits = 4)
############################################################
#Calculate total away goals for each division
arg_T_AG <- sum(arg_away_gs$x)
#calculate average away goal
arg_avg_AG <- round(arg_T_AG /arg_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
arg_home_as <- round(((arg_home_gs$x/arg_home_games))/arg_avg_HG, digits = 4)
#calculate away attack strength
arg_away_as <- round(((arg_away_gs$x/arg_away_games))/arg_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
arg_avg_HC <- round(arg_T_AG /arg_GP, digits = 4)
#avg away concede
arg_avg_AC <- round(arg_T_HG /arg_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
arg_home_ds <- round(((arg_home_gc$x/arg_home_games))/arg_avg_HC, digits = 4)
#away defense strength
arg_away_ds <- round(((arg_away_gc$x/arg_away_games))/arg_avg_AC, digits = 4)
#############################################################################
#home poisson data
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_home_poisson <- cbind(arg_division,arg_teams,arg_avg_HG,arg_home_as,arg_home_ds)
#################################################################################
#away poisson data
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_away_poisson <- cbind(arg_division,arg_teams,arg_avg_AG,arg_away_as,arg_away_ds)
#################################################################################

###############################################################################################
arg_totalgames_poiss <- c()
arg_totalgames_poiss[1:length(arg_teams)] <- paste("no",poisson_round_nl,sep = "")
arg_home_poisson <- cbind(arg_home_poisson,arg_totalgames_poiss)
arg_away_poisson <- cbind(arg_away_poisson,arg_totalgames_poiss)

write.csv(arg_home_poisson,paste("arg_home_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))
write.csv(arg_away_poisson,paste("arg_away_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))


allhomepoisson_nl <- rbind(allhomepoisson_nl,arg_home_poisson)
allawaypoisson_nl <- rbind(allawaypoisson_nl,arg_away_poisson)
   # allhomepoisson_nl <- rbind(aut_home_poisson,arg_home_poisson,bra_home_poisson,chn_home_poisson,dnk_home_poisson,fin_home_poisson,
   #                         irl_home_poisson,jpn_home_poisson,mex_home_poisson,mls_home_poisson,nor_home_poisson,pol_home_poisson,
   #                         rou_home_poisson,rus_home_poisson,swe_home_poisson,swz_home_poisson)
   #
   #
   #
   # allawaypoisson_nl <- rbind(aut_away_poisson,arg_away_poisson,bra_away_poisson,chn_away_poisson,dnk_away_poisson,fin_away_poisson,
   #                         irl_away_poisson,jpn_away_poisson,mex_away_poisson,mls_away_poisson,nor_away_poisson,pol_away_poisson,
   #                         rou_away_poisson,rus_away_poisson,swe_away_poisson,swz_away_poisson)


  poisson_round_nl <- poisson_round_nl + 6

}

allhomepoisson_nl <- mgsub(allhomepoisson_nl,c("ARG","AUT","BRA","CHN","DNK","FIN","IRL","JPN","MEX","NOR","POL","ROU","RUS","SWE","SWZ"),c("Liga Profesional","Admiral Bundesliga","Serie A","Super League","Superliga","Veikkausliiga","Premier Division","J1 League","Liga MX","Eliteserien","Ekstraklasa","Liga 1","Premier League","Allsvenskan","Swiss"))
allawaypoisson_nl <- mgsub(allawaypoisson_nl,c("ARG","AUT","BRA","CHN","DNK","FIN","IRL","JPN","MEX","NOR","POL","ROU","RUS","SWE","SWZ"),c("Liga Profesional","Admiral Bundesliga","Serie A","Super League","Superliga","Veikkausliiga","Premier Division","J1 League","Liga MX","Eliteserien","Ekstraklasa","Liga 1","Premier League","Allsvenskan","Swiss"))

write.csv(allhomepoisson_nl,"allhomepoisson_nl.csv")
write.csv(allawaypoisson_nl,"allawaypoisson_nl.csv")


