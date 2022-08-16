library(mgsub)
#################################################
###
poisson_round_nl <- 6
allhomepoisson_nl <- c()
allawaypoisson_nl <- c()

while (poisson_round_nl <= 36){
ROUND_20212022_nl <- poisson_round_nl

  #########################################################
  # AUT <- AUT_rounds[AUT_rounds$aut_matchday <= ROUND_20212022_nl,]
  # AUT$HG <- as.numeric(AUT$HG)
  # AUT$AG <- as.numeric(AUT$AG)
  # AUT$TG <- as.numeric(AUT$TG)
  # ARG <- ARG_rounds[ARG_rounds$arg_matchday <= ROUND_20212022_nl,]
  # ARG$HG <- as.numeric(ARG$HG)
  # ARG$AG <- as.numeric(ARG$AG)
  # ARG$TG <- as.numeric(ARG$TG)
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
  SWZ <- SWZ_rounds[SWZ_rounds$swz_matchday <= ROUND_20212022_nl,]
  SWZ$HG <- as.numeric(SWZ$HG)
  SWZ$AG <- as.numeric(SWZ$AG)
  SWZ$TG <- as.numeric(SWZ$TG)
  #############################################################
  #source("midpoisson.R")
  ############################################################
  swz_home_games <- c()
  swz_away_games <-c()
  for (i_swz in 1:length(swz_teams))
  {

    swz_home_games[i_swz] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz],])
    swz_away_games[i_swz]  <- nrow(SWZ[SWZ$Away == swz_teams[i_swz],])

  }

  swz_games_played <- swz_home_games + swz_away_games

  #home goals scored
  swz_home_gs <- aggregate(SWZ$HG, by = list(SWZ$Home), FUN = sum)
  swz_home_gs_avg <- aggregate(SWZ$HG, by = list(SWZ$Home),mean)
  swz_home_scoring <- merge(swz_home_gs,swz_home_gs_avg, by='Group.1',all = T)
  names(swz_home_scoring)[names(swz_home_scoring) == "x.x"] <- "TFthg"
  names(swz_home_scoring)[names(swz_home_scoring) == "x.y"] <- "Avg_Fthg"
  #away goals scored
  swz_away_gs <- aggregate(SWZ$AG, by = list(SWZ$Away), FUN = sum)
  swz_away_gs_avg <- aggregate(SWZ$AG, by = list(SWZ$Away),mean)
  swz_away_scoring <- merge(swz_away_gs,swz_away_gs_avg, by='Group.1',all = T)
  names(swz_away_scoring)[names(swz_away_scoring) == "x.x"] <- "TFtag"
  names(swz_away_scoring)[names(swz_away_scoring) == "x.y"] <- "Avg_Ftag"
  #total goals scored
  swz_scoring <- merge(swz_home_scoring,swz_away_scoring,by='Group.1',all = T)
  swz_scoring$TGS <- swz_scoring$TFthg + swz_scoring$TFtag

  #home goals conceded
  swz_home_gc <- aggregate(SWZ$AG, by = list(SWZ$Home), FUN = sum)
  swz_home_gc_avg <- aggregate(SWZ$AG, by = list(SWZ$Home),mean)
  swz_home_conceding <- merge(swz_home_gc,swz_home_gc_avg, by='Group.1',all = T)
  names(swz_home_conceding)[names(swz_home_conceding) == "x.x"] <- "TFthc"
  names(swz_home_conceding)[names(swz_home_conceding) == "x.y"] <- "Avg_Fthc"
  #away goals conceded
  swz_away_gc <- aggregate(SWZ$HG, by = list(SWZ$Away), FUN = sum)
  swz_away_gc_avg <- aggregate(SWZ$HG, by = list(SWZ$Away),mean)
  swz_away_conceding <- merge(swz_away_gc,swz_away_gc_avg, by='Group.1',all = T)
  names(swz_away_conceding)[names(swz_away_conceding) == "x.x"] <- "TFtac"
  names(swz_away_conceding)[names(swz_away_conceding) == "x.y"] <- "Avg_Ftac"
  #total goals conceded
  swz_conceding <- merge(swz_home_conceding,swz_away_conceding,by='Group.1',all = T)
  swz_conceding$TGC <- swz_conceding$TFthc + swz_conceding$TFtac

  #poisson model
  #get total games played
  swz_GP <- nrow(SWZ)
  #Calculate total home goals for each division
  swz_T_HG <- sum(swz_home_gs$x)
  #calculate average home goal
  swz_avg_HG <- round(swz_T_HG /swz_GP, digits = 4)
  ############################################################
  #Calculate total away goals for each division
  swz_T_AG <- sum(swz_away_gs$x)
  #calculate average away goal
  swz_avg_AG <- round(swz_T_AG /swz_GP, digits = 4)
  #get total home goals and total home games played for each division
  #calculate home attack strength
  swz_home_as <- round(((swz_home_gs$x/swz_home_games))/swz_avg_HG, digits = 4)
  #calculate away attack strength
  swz_away_as <- round(((swz_away_gs$x/swz_away_games))/swz_avg_AG, digits = 4)
  ################################################################################
  #get average home concede and away concede
  swz_avg_HC <- round(swz_T_AG /swz_GP, digits = 4)
  #avg away concede
  swz_avg_AC <- round(swz_T_HG /swz_GP, digits = 4)
  #calculate home and away defense strength
  #home defense strength
  swz_home_ds <- round(((swz_home_gc$x/swz_home_games))/swz_avg_HC, digits = 4)
  #away defense strength
  swz_away_ds <- round(((swz_away_gc$x/swz_away_games))/swz_avg_AC, digits = 4)
  #############################################################################
  #home poisson data
  #swz
  swz_division <- c()
  swz_division[1:length(swz_teams)] <- "SWZ"
  swz_home_poisson <- cbind(swz_division,swz_teams,swz_avg_HG,swz_home_as,swz_home_ds)
  #################################################################################
  #away poisson data
  #swz
  swz_division <- c()
  swz_division[1:length(swz_teams)] <- "SWZ"
  swz_away_poisson <- cbind(swz_division,swz_teams,swz_avg_AG,swz_away_as,swz_away_ds)
#################################################################################

###############################################################################################
swz_totalgames_poiss <- c()
swz_totalgames_poiss[1:length(swz_teams)] <- paste("no",poisson_round_nl,sep = "")
swz_home_poisson <- cbind(swz_home_poisson,swz_totalgames_poiss)
swz_away_poisson <- cbind(swz_away_poisson,swz_totalgames_poiss)

write.csv(swz_home_poisson,paste("swz_home_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))
write.csv(swz_away_poisson,paste("swz_away_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))


allhomepoisson_nl <- rbind(allhomepoisson_nl,swz_home_poisson)
allawaypoisson_nl <- rbind(allawaypoisson_nl,swz_away_poisson)
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

unlink('allhomepoisson_nl.csv')
unlink('allawaypoisson_nl.csv')

write.csv(allhomepoisson_nl,"allhomepoisson_nl.csv")
write.csv(allawaypoisson_nl,"allawaypoisson_nl.csv")


