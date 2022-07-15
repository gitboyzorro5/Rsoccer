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
  MLS <- MLS_rounds[MLS_rounds$mls_matchday <= ROUND_20212022_nl,]
  MLS$HG <- as.numeric(MLS$HG)
  MLS$AG <- as.numeric(MLS$AG)
  MLS$TG <- as.numeric(MLS$TG)
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
mls_home_games <- c()
mls_away_games <-c()
for (i_mls in 1:length(mls_teams))
{

  mls_home_games[i_mls] <- nrow(MLS[MLS$Home == mls_teams[i_mls],])
  mls_away_games[i_mls]  <- nrow(MLS[MLS$Away == mls_teams[i_mls],])

}

mls_games_played <- mls_home_games + mls_away_games

#home goals scored
mls_home_gs <- aggregate(MLS$HG, by = list(MLS$Home), FUN = sum)
mls_home_gs_avg <- aggregate(MLS$HG, by = list(MLS$Home),mean)
mls_home_scoring <- merge(mls_home_gs,mls_home_gs_avg, by='Group.1',all = T)
names(mls_home_scoring)[names(mls_home_scoring) == "x.x"] <- "TFthg"
names(mls_home_scoring)[names(mls_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
mls_away_gs <- aggregate(MLS$AG, by = list(MLS$Away), FUN = sum)
mls_away_gs_avg <- aggregate(MLS$AG, by = list(MLS$Away),mean)
mls_away_scoring <- merge(mls_away_gs,mls_away_gs_avg, by='Group.1',all = T)
names(mls_away_scoring)[names(mls_away_scoring) == "x.x"] <- "TFtag"
names(mls_away_scoring)[names(mls_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
mls_scoring <- merge(mls_home_scoring,mls_away_scoring,by='Group.1',all = T)
mls_scoring$TGS <- mls_scoring$TFthg + mls_scoring$TFtag

#home goals conceded
mls_home_gc <- aggregate(MLS$AG, by = list(MLS$Home), FUN = sum)
mls_home_gc_avg <- aggregate(MLS$AG, by = list(MLS$Home),mean)
mls_home_conceding <- merge(mls_home_gc,mls_home_gc_avg, by='Group.1',all = T)
names(mls_home_conceding)[names(mls_home_conceding) == "x.x"] <- "TFthc"
names(mls_home_conceding)[names(mls_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
mls_away_gc <- aggregate(MLS$HG, by = list(MLS$Away), FUN = sum)
mls_away_gc_avg <- aggregate(MLS$HG, by = list(MLS$Away),mean)
mls_away_conceding <- merge(mls_away_gc,mls_away_gc_avg, by='Group.1',all = T)
names(mls_away_conceding)[names(mls_away_conceding) == "x.x"] <- "TFtac"
names(mls_away_conceding)[names(mls_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
mls_conceding <- merge(mls_home_conceding,mls_away_conceding,by='Group.1',all = T)
mls_conceding$TGC <- mls_conceding$TFthc + mls_conceding$TFtac

#poisson model
#get total games played
mls_GP <- nrow(MLS)
#Calculate total home goals for each division
mls_T_HG <- sum(mls_home_gs$x)
#calculate average home goal
mls_avg_HG <- round(mls_T_HG /mls_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AG <- sum(mls_away_gs$x)
#calculate average away goal
mls_avg_AG <- round(mls_T_AG /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_as <- round(((mls_home_gs$x/mls_home_games))/mls_avg_HG, digits = 4)
#calculate away attack strength
mls_away_as <- round(((mls_away_gs$x/mls_away_games))/mls_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HC <- round(mls_T_AG /mls_GP, digits = 4)
#avg away concede
mls_avg_AC <- round(mls_T_HG /mls_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
mls_home_ds <- round(((mls_home_gc$x/mls_home_games))/mls_avg_HC, digits = 4)
#away defense strength
mls_away_ds <- round(((mls_away_gc$x/mls_away_games))/mls_avg_AC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson <- cbind(mls_division,mls_teams,mls_avg_HG,mls_home_as,mls_home_ds)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson <- cbind(mls_division,mls_teams,mls_avg_AG,mls_away_as,mls_away_ds)
#################################################################################

###############################################################################################
mls_totalgames_poiss <- c()
mls_totalgames_poiss[1:length(mls_teams)] <- paste("no",poisson_round_nl,sep = "")
mls_home_poisson <- cbind(mls_home_poisson,mls_totalgames_poiss)
mls_away_poisson <- cbind(mls_away_poisson,mls_totalgames_poiss)

write.csv(mls_home_poisson,paste("mls_home_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))
write.csv(mls_away_poisson,paste("mls_away_poisson_nl",ROUND_20212022_nl,".csv",sep = "_"))


allhomepoisson_nl <- rbind(allhomepoisson_nl,mls_home_poisson)
allawaypoisson_nl <- rbind(allawaypoisson_nl,mls_away_poisson)
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


