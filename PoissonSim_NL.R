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
  SWE <- SWE_rounds[SWE_rounds$swe_matchday <= ROUND_20212022_nl,]
  SWE$HG <- as.numeric(SWE$HG)
  SWE$AG <- as.numeric(SWE$AG)
  SWE$TG <- as.numeric(SWE$TG)
  # SWZ <- SWZ_rounds[SWZ_rounds$swz_matchday <= ROUND_20212022_nl,]
  # SWZ$HG <- as.numeric(SWZ$HG)
  # SWZ$AG <- as.numeric(SWZ$AG)
  # SWZ$TG <- as.numeric(SWZ$TG)
  #############################################################
  #source("midpoisson.R")
  swe_home_games <- c()
  swe_away_games <-c()
  for (i_swe in 1:length(swe_teams))
  {

    swe_home_games[i_swe] <- nrow(SWE[SWE$Home == swe_teams[i_swe],])
    swe_away_games[i_swe]  <- nrow(SWE[SWE$Away == swe_teams[i_swe],])

  }

  swe_games_played <- swe_home_games + swe_away_games

  #home goals scored
  swe_home_gs <- aggregate(SWE$HG, by = list(SWE$Home), FUN = sum)
  swe_home_gs_avg <- aggregate(SWE$HG, by = list(SWE$Home),mean)
  swe_home_scoring <- merge(swe_home_gs,swe_home_gs_avg, by='Group.1',all = T)
  names(swe_home_scoring)[names(swe_home_scoring) == "x.x"] <- "TFthg"
  names(swe_home_scoring)[names(swe_home_scoring) == "x.y"] <- "Avg_Fthg"
  #away goals scored
  swe_away_gs <- aggregate(SWE$AG, by = list(SWE$Away), FUN = sum)
  swe_away_gs_avg <- aggregate(SWE$AG, by = list(SWE$Away),mean)
  swe_away_scoring <- merge(swe_away_gs,swe_away_gs_avg, by='Group.1',all = T)
  names(swe_away_scoring)[names(swe_away_scoring) == "x.x"] <- "TFtag"
  names(swe_away_scoring)[names(swe_away_scoring) == "x.y"] <- "Avg_Ftag"
  #total goals scored
  swe_scoring <- merge(swe_home_scoring,swe_away_scoring,by='Group.1',all = T)
  swe_scoring$TGS <- swe_scoring$TFthg + swe_scoring$TFtag

  #home goals conceded
  swe_home_gc <- aggregate(SWE$AG, by = list(SWE$Home), FUN = sum)
  swe_home_gc_avg <- aggregate(SWE$AG, by = list(SWE$Home),mean)
  swe_home_conceding <- merge(swe_home_gc,swe_home_gc_avg, by='Group.1',all = T)
  names(swe_home_conceding)[names(swe_home_conceding) == "x.x"] <- "TFthc"
  names(swe_home_conceding)[names(swe_home_conceding) == "x.y"] <- "Avg_Fthc"
  #away goals conceded
  swe_away_gc <- aggregate(SWE$HG, by = list(SWE$Away), FUN = sum)
  swe_away_gc_avg <- aggregate(SWE$HG, by = list(SWE$Away),mean)
  swe_away_conceding <- merge(swe_away_gc,swe_away_gc_avg, by='Group.1',all = T)
  names(swe_away_conceding)[names(swe_away_conceding) == "x.x"] <- "TFtac"
  names(swe_away_conceding)[names(swe_away_conceding) == "x.y"] <- "Avg_Ftac"
  #total goals conceded
  swe_conceding <- merge(swe_home_conceding,swe_away_conceding,by='Group.1',all = T)
  swe_conceding$TGC <- swe_conceding$TFthc + swe_conceding$TFtac

  #poisson model
  #get total games played
  swe_GP <- nrow(SWE)
  #Calculate total home goals for each division
  swe_T_HG <- sum(swe_home_gs$x)
  #calculate average home goal
  swe_avg_HG <- round(swe_T_HG /swe_GP, digits = 4)
  ############################################################
  #Calculate total away goals for each division
  swe_T_AG <- sum(swe_away_gs$x)
  #calculate average away goal
  swe_avg_AG <- round(swe_T_AG /swe_GP, digits = 4)
  #get total home goals and total home games played for each division
  #calculate home attack strength
  swe_home_as <- round(((swe_home_gs$x/swe_home_games))/swe_avg_HG, digits = 4)
  #calculate away attack strength
  swe_away_as <- round(((swe_away_gs$x/swe_away_games))/swe_avg_AG, digits = 4)
  ################################################################################
  #get average home concede and away concede
  swe_avg_HC <- round(swe_T_AG /swe_GP, digits = 4)
  #avg away concede
  swe_avg_AC <- round(swe_T_HG /swe_GP, digits = 4)
  #calculate home and away defense strength
  #home defense strength
  swe_home_ds <- round(((swe_home_gc$x/swe_home_games))/swe_avg_HC, digits = 4)
  #away defense strength
  swe_away_ds <- round(((swe_away_gc$x/swe_away_games))/swe_avg_AC, digits = 4)
  #############################################################################
  #home poisson data
  #swe
  swe_division <- c()
  swe_division[1:length(swe_teams)] <- "SWE"
  swe_home_poisson <- cbind(swe_division,swe_teams,swe_avg_HG,swe_home_as,swe_home_ds)
  #################################################################################
  #away poisson data
  #swe
  swe_division <- c()
  swe_division[1:length(swe_teams)] <- "SWE"
  swe_away_poisson <- cbind(swe_division,swe_teams,swe_avg_AG,swe_away_as,swe_away_ds)

  swe_totalgames_poiss_nl <- c()
  swe_totalgames_poiss_nl[1:length(swe_teams)] <- paste("no",poisson_round_nl,sep = "")
  swe_home_poisson_nl <- cbind(swe_home_poisson,swe_totalgames_poiss_nl)
  swe_away_poisson_nl <- cbind(swe_away_poisson,swe_totalgames_poiss_nl)
  ###############################################################################################

  write.csv(swe_home_poisson_nl,paste("swe_home_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))
  write.csv(swe_away_poisson_nl,paste("swe_away_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))


  allhomepoisson_nl <- rbind(allhomepoisson_nl,swe_home_poisson_nl)
  allawaypoisson_nl <- rbind(allawaypoisson_nl,swe_away_poisson_nl)
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

