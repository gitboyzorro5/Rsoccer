
#################################################
###
poisson_round_nl <- 7
allhomepoisson_nl <- c()
allawaypoisson_nl <- c()
# while (poisson_round_nl <= 30)
#
#   {

  ROUND_20212022_nl <- poisson_round_nl

  #########################################################
  AUT <- AUT_rounds[AUT_rounds$aut_matchday <= ROUND_20212022_nl,]
  AUT$HG <- as.numeric(AUT$HG)
  AUT$AG <- as.numeric(AUT$AG)
  AUT$TG <- as.numeric(AUT$TG)
  ARG <- ARG_rounds[ARG_rounds$arg_matchday <= ROUND_20212022_nl,]
  ARG$HG <- as.numeric(ARG$HG)
  ARG$AG <- as.numeric(ARG$AG)
  ARG$TG <- as.numeric(ARG$TG)
  BRA <- BRA_rounds[BRA_rounds$bra_matchday <= ROUND_20212022_nl,]
  BRA$HG <- as.numeric(BRA$HG)
  BRA$AG <- as.numeric(BRA$AG)
  BRA$TG <- as.numeric(BRA$TG)
  CHN <- CHN_rounds[CHN_rounds$chn_matchday <= ROUND_20212022_nl,]
  CHN$HG <- as.numeric(CHN$HG)
  CHN$AG <- as.numeric(CHN$AG)
  CHN$TG <- as.numeric(CHN$TG)
  DNK <- DNK_rounds[DNK_rounds$dnk_matchday <= ROUND_20212022_nl,]
  DNK$HG <- as.numeric(DNK$HG)
  DNK$AG <- as.numeric(DNK$AG)
  DNK$TG <- as.numeric(DNK$TG)
  FIN <- FIN_rounds[FIN_rounds$fin_matchday <= ROUND_20212022_nl,]
  FIN$HG <- as.numeric(FIN$HG)
  FIN$AG <- as.numeric(FIN$AG)
  FIN$TG <- as.numeric(FIN$TG)
  IRL <- IRL_rounds[IRL_rounds$irl_matchday <= ROUND_20212022_nl,]
  IRL$HG <- as.numeric(IRL$HG)
  IRL$AG <- as.numeric(IRL$AG)
  IRL$TG <- as.numeric(IRL$TG)
  JPN <- JPN_rounds[JPN_rounds$jpn_matchday <= ROUND_20212022_nl,]
  JPN$HG <- as.numeric(JPN$HG)
  JPN$AG <- as.numeric(JPN$AG)
  JPN$TG <- as.numeric(JPN$TG)
  MEX <- MEX_rounds[MEX_rounds$mex_matchday <= ROUND_20212022_nl,]
  MEX$HG <- as.numeric(MEX$HG)
  MEX$AG <- as.numeric(MEX$AG)
  MEX$TG <- as.numeric(MEX$TG)
  MLS <- MLS_rounds[MLS_rounds$mls_matchday <= ROUND_20212022_nl,]
  MLS$HG <- as.numeric(MLS$HG)
  MLS$AG <- as.numeric(MLS$AG)
  MLS$TG <- as.numeric(MLS$TG)
  NOR <- NOR_rounds[NOR_rounds$nor_matchday <= ROUND_20212022_nl,]
  NOR$HG <- as.numeric(NOR$HG)
  NOR$AG <- as.numeric(NOR$AG)
  NOR$TG <- as.numeric(NOR$TG)
  POL <- POL_rounds[POL_rounds$pol_matchday <= ROUND_20212022_nl,]
  POL$HG <- as.numeric(POL$HG)
  POL$AG <- as.numeric(POL$AG)
  POL$TG <- as.numeric(POL$TG)
  ROU <- ROU_rounds[ROU_rounds$rou_matchday <= ROUND_20212022_nl,]
  ROU$HG <- as.numeric(ROU$HG)
  ROU$AG <- as.numeric(ROU$AG)
  ROU$TG <- as.numeric(ROU$TG)
  RUS <- RUS_rounds[RUS_rounds$rus_matchday <= ROUND_20212022_nl,]
  RUS$HG <- as.numeric(RUS$HG)
  RUS$AG <- as.numeric(RUS$AG)
  RUS$TG <- as.numeric(RUS$TG)
  SWE <- SWE_rounds[SWE_rounds$swe_matchday <= ROUND_20212022_nl,]
  SWE$HG <- as.numeric(SWE$HG)
  SWE$AG <- as.numeric(SWE$AG)
  SWE$TG <- as.numeric(SWE$TG)
  SWZ <- SWZ_rounds[SWZ_rounds$swz_matchday <= ROUND_20212022_nl,]
  SWZ$HG <- as.numeric(SWZ$HG)
  SWZ$AG <- as.numeric(SWZ$AG)
  SWZ$TG <- as.numeric(SWZ$TG)
  #############################################################
  source("midpoisson.R")
  # ec_totalgames_poiss_nl <- c()
  # ec_totalgames_poiss_nl[1:length(ec_teams)] <- paste("no",poisson_round_nl,sep = "")
  # ec_home_poisson_nl <- cbind(ec_home_poisson_nl,ec_totalgames_poiss_nl)
  # ec_away_poisson-nl <- cbind(ec_away_poisson_nl,ec_totalgames_poiss_nl)
  #
  # write.csv(ec_home_poisson_nl,paste("ec_home_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))
  # write.csv(ec_away_poisson_nl,paste("ec_away_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))
  #
  #
  # allhomepoisson_nl <- rbind(allhomepoisson_nl,ec_home_poisson_nl)
  # allawaypoisson_nl <- rbind(allawaypoisson_nl,ec_away_poisson_nl)
  allhomepoisson_nl <- rbind(aut_home_poisson,arg_home_poisson,bra_home_poisson,chn_home_poisson,dnk_home_poisson,fin_home_poisson,
                          irl_home_poisson,jpn_home_poisson,mex_home_poisson,mls_home_poisson,nor_home_poisson,pol_home_poisson,
                          rou_home_poisson,rus_home_poisson,swe_home_poisson,swz_home_poisson)



  allawaypoisson_nl <- rbind(aut_away_poisson,arg_away_poisson,bra_away_poisson,chn_away_poisson,dnk_away_poisson,fin_away_poisson,
                          irl_away_poisson,jpn_away_poisson,mex_away_poisson,mls_away_poisson,nor_away_poisson,pol_away_poisson,
                          rou_away_poisson,rus_away_poisson,swe_away_poisson,swz_away_poisson)

#   poisson_round_nl <- poisson_round_nl + 6
#
# }

  allhomepoisson_nl <- mgsub(allhomepoisson_nl,c("ARG","AUT","BRA","CHN","DNK","FIN","IRL","JPN","MEX","NOR","POL","ROU","RUS","SWE","SWZ"),c("Liga Profesional","Admiral Bundesliga","Serie A","Super League","Superliga","Veikkausliiga","Premier Division","J1 League","Liga MX","Eliteserien","Ekstraklasa","Liga 1","Premier League","Allsvenskan","Swiss"))
  allawaypoisson_nl <- mgsub(allawaypoisson_nl,c("ARG","AUT","BRA","CHN","DNK","FIN","IRL","JPN","MEX","NOR","POL","ROU","RUS","SWE","SWZ"),c("Liga Profesional","Admiral Bundesliga","Serie A","Super League","Superliga","Veikkausliiga","Premier Division","J1 League","Liga MX","Eliteserien","Ekstraklasa","Liga 1","Premier League","Allsvenskan","Swiss"))

write.csv(allhomepoisson_nl,"allhomepoisson_nl.csv")
write.csv(allawaypoisson_nl,"allawaypoisson_nl.csv")









