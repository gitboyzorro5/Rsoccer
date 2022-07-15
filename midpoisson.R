######################################################################################################
aut_home_games <- c()
aut_away_games <-c()
for (i_aut in 1:length(aut_teams))
{

  aut_home_games[i_aut] <- nrow(AUT[AUT$Home == aut_teams[i_aut],])
  aut_away_games[i_aut]  <- nrow(AUT[AUT$Away == aut_teams[i_aut],])

}

aut_games_played <- aut_home_games + aut_away_games

#home goals scored
aut_home_gs <- aggregate(AUT$HG, by = list(AUT$Home), FUN = sum)
aut_home_gs_avg <- aggregate(AUT$HG, by = list(AUT$Home),mean)
aut_home_scoring <- merge(aut_home_gs,aut_home_gs_avg, by='Group.1',all = T)
names(aut_home_scoring)[names(aut_home_scoring) == "x.x"] <- "TFthg"
names(aut_home_scoring)[names(aut_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
aut_away_gs <- aggregate(AUT$AG, by = list(AUT$Away), FUN = sum)
aut_away_gs_avg <- aggregate(AUT$AG, by = list(AUT$Away),mean)
aut_away_scoring <- merge(aut_away_gs,aut_away_gs_avg, by='Group.1',all = T)
names(aut_away_scoring)[names(aut_away_scoring) == "x.x"] <- "TFtag"
names(aut_away_scoring)[names(aut_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
aut_scoring <- merge(aut_home_scoring,aut_away_scoring,by='Group.1',all = T)
aut_scoring$TGS <- aut_scoring$TFthg + aut_scoring$TFtag

#home goals conceded
aut_home_gc <- aggregate(AUT$AG, by = list(AUT$Home), FUN = sum)
aut_home_gc_avg <- aggregate(AUT$AG, by = list(AUT$Home),mean)
aut_home_conceding <- merge(aut_home_gc,aut_home_gc_avg, by='Group.1',all = T)
names(aut_home_conceding)[names(aut_home_conceding) == "x.x"] <- "TFthc"
names(aut_home_conceding)[names(aut_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
aut_away_gc <- aggregate(AUT$HG, by = list(AUT$Away), FUN = sum)
aut_away_gc_avg <- aggregate(AUT$HG, by = list(AUT$Away),mean)
aut_away_conceding <- merge(aut_away_gc,aut_away_gc_avg, by='Group.1',all = T)
names(aut_away_conceding)[names(aut_away_conceding) == "x.x"] <- "TFtac"
names(aut_away_conceding)[names(aut_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
aut_conceding <- merge(aut_home_conceding,aut_away_conceding,by='Group.1',all = T)
aut_conceding$TGC <- aut_conceding$TFthc + aut_conceding$TFtac

#poisson model
#get total games played
aut_GP <- nrow(AUT)
#Calculate total home goals for each division
aut_T_HG <- sum(aut_home_gs$x)
#calculate average home goal
aut_avg_HG <- round(aut_T_HG /aut_GP, digits = 4)
############################################################
#Calculate total away goals for each division
aut_T_AG <- sum(aut_away_gs$x)
#calculate average away goal
aut_avg_AG <- round(aut_T_AG /aut_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
aut_home_as <- round(((aut_home_gs$x/aut_home_games))/aut_avg_HG, digits = 4)
#calculate away attack strength
aut_away_as <- round(((aut_away_gs$x/aut_away_games))/aut_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
aut_avg_HC <- round(aut_T_AG /aut_GP, digits = 4)
#avg away concede
aut_avg_AC <- round(aut_T_HG /aut_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
aut_home_ds <- round(((aut_home_gc$x/aut_home_games))/aut_avg_HC, digits = 4)
#away defense strength
aut_away_ds <- round(((aut_away_gc$x/aut_away_games))/aut_avg_AC, digits = 4)
#############################################################################
#home poisson data
#aut
aut_division <- c()
aut_division[1:length(aut_teams)] <- "AUT"
aut_home_poisson <- cbind(aut_division,aut_teams,aut_avg_HG,aut_home_as,aut_home_ds)
#################################################################################
#away poisson data
#aut
aut_division <- c()
aut_division[1:length(aut_teams)] <- "AUT"
aut_away_poisson <- cbind(aut_division,aut_teams,aut_avg_AG,aut_away_as,aut_away_ds)

###########################################################################################################################
###########################################################################################################################
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
########################################################################################################################################
########################################################################################################################################
bra_home_games <- c()
bra_away_games <-c()
for (i_bra in 1:length(bra_teams))
{

  bra_home_games[i_bra] <- nrow(BRA[BRA$Home == bra_teams[i_bra],])
  bra_away_games[i_bra]  <- nrow(BRA[BRA$Away == bra_teams[i_bra],])

}

bra_games_played <- bra_home_games + bra_away_games

#home goals scored
bra_home_gs <- aggregate(BRA$HG, by = list(BRA$Home), FUN = sum)
bra_home_gs_avg <- aggregate(BRA$HG, by = list(BRA$Home),mean)
bra_home_scoring <- merge(bra_home_gs,bra_home_gs_avg, by='Group.1',all = T)
names(bra_home_scoring)[names(bra_home_scoring) == "x.x"] <- "TFthg"
names(bra_home_scoring)[names(bra_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
bra_away_gs <- aggregate(BRA$AG, by = list(BRA$Away), FUN = sum)
bra_away_gs_avg <- aggregate(BRA$AG, by = list(BRA$Away),mean)
bra_away_scoring <- merge(bra_away_gs,bra_away_gs_avg, by='Group.1',all = T)
names(bra_away_scoring)[names(bra_away_scoring) == "x.x"] <- "TFtag"
names(bra_away_scoring)[names(bra_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
bra_scoring <- merge(bra_home_scoring,bra_away_scoring,by='Group.1',all = T)
bra_scoring$TGS <- bra_scoring$TFthg + bra_scoring$TFtag

#home goals conceded
bra_home_gc <- aggregate(BRA$AG, by = list(BRA$Home), FUN = sum)
bra_home_gc_avg <- aggregate(BRA$AG, by = list(BRA$Home),mean)
bra_home_conceding <- merge(bra_home_gc,bra_home_gc_avg, by='Group.1',all = T)
names(bra_home_conceding)[names(bra_home_conceding) == "x.x"] <- "TFthc"
names(bra_home_conceding)[names(bra_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
bra_away_gc <- aggregate(BRA$HG, by = list(BRA$Away), FUN = sum)
bra_away_gc_avg <- aggregate(BRA$HG, by = list(BRA$Away),mean)
bra_away_conceding <- merge(bra_away_gc,bra_away_gc_avg, by='Group.1',all = T)
names(bra_away_conceding)[names(bra_away_conceding) == "x.x"] <- "TFtac"
names(bra_away_conceding)[names(bra_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
bra_conceding <- merge(bra_home_conceding,bra_away_conceding,by='Group.1',all = T)
bra_conceding$TGC <- bra_conceding$TFthc + bra_conceding$TFtac

#poisson model
#get total games played
bra_GP <- nrow(BRA)
#Calculate total home goals for each division
bra_T_HG <- sum(bra_home_gs$x)
#calculate average home goal
bra_avg_HG <- round(bra_T_HG /bra_GP, digits = 4)
############################################################
#Calculate total away goals for each division
bra_T_AG <- sum(bra_away_gs$x)
#calculate average away goal
bra_avg_AG <- round(bra_T_AG /bra_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
bra_home_as <- round(((bra_home_gs$x/bra_home_games))/bra_avg_HG, digits = 4)
#calculate away attack strength
bra_away_as <- round(((bra_away_gs$x/bra_away_games))/bra_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
bra_avg_HC <- round(bra_T_AG /bra_GP, digits = 4)
#avg away concede
bra_avg_AC <- round(bra_T_HG /bra_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
bra_home_ds <- round(((bra_home_gc$x/bra_home_games))/bra_avg_HC, digits = 4)
#away defense strength
bra_away_ds <- round(((bra_away_gc$x/bra_away_games))/bra_avg_AC, digits = 4)
#############################################################################
#home poisson data
#bra
bra_division <- c()
bra_division[1:length(bra_teams)] <- "BRA"
bra_home_poisson <- cbind(bra_division,bra_teams,bra_avg_HG,bra_home_as,bra_home_ds)
#################################################################################
#away poisson data
#bra
bra_division <- c()
bra_division[1:length(bra_teams)] <- "BRA"
bra_away_poisson <- cbind(bra_division,bra_teams,bra_avg_AG,bra_away_as,bra_away_ds)
###################################################################################################################################
###################################################################################################################################
chn_home_games <- c()
chn_away_games <-c()
for (i_chn in 1:length(chn_teams))
{

  chn_home_games[i_chn] <- nrow(CHN[CHN$Home == chn_teams[i_chn],])
  chn_away_games[i_chn]  <- nrow(CHN[CHN$Away == chn_teams[i_chn],])

}

chn_games_played <- chn_home_games + chn_away_games

#home goals scored
chn_home_gs <- aggregate(CHN$HG, by = list(CHN$Home), FUN = sum)
chn_home_gs_avg <- aggregate(CHN$HG, by = list(CHN$Home),mean)
chn_home_scoring <- merge(chn_home_gs,chn_home_gs_avg, by='Group.1',all = T)
names(chn_home_scoring)[names(chn_home_scoring) == "x.x"] <- "TFthg"
names(chn_home_scoring)[names(chn_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
chn_away_gs <- aggregate(CHN$AG, by = list(CHN$Away), FUN = sum)
chn_away_gs_avg <- aggregate(CHN$AG, by = list(CHN$Away),mean)
chn_away_scoring <- merge(chn_away_gs,chn_away_gs_avg, by='Group.1',all = T)
names(chn_away_scoring)[names(chn_away_scoring) == "x.x"] <- "TFtag"
names(chn_away_scoring)[names(chn_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
chn_scoring <- merge(chn_home_scoring,chn_away_scoring,by='Group.1',all = T)
chn_scoring$TGS <- chn_scoring$TFthg + chn_scoring$TFtag

#home goals conceded
chn_home_gc <- aggregate(CHN$AG, by = list(CHN$Home), FUN = sum)
chn_home_gc_avg <- aggregate(CHN$AG, by = list(CHN$Home),mean)
chn_home_conceding <- merge(chn_home_gc,chn_home_gc_avg, by='Group.1',all = T)
names(chn_home_conceding)[names(chn_home_conceding) == "x.x"] <- "TFthc"
names(chn_home_conceding)[names(chn_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
chn_away_gc <- aggregate(CHN$HG, by = list(CHN$Away), FUN = sum)
chn_away_gc_avg <- aggregate(CHN$HG, by = list(CHN$Away),mean)
chn_away_conceding <- merge(chn_away_gc,chn_away_gc_avg, by='Group.1',all = T)
names(chn_away_conceding)[names(chn_away_conceding) == "x.x"] <- "TFtac"
names(chn_away_conceding)[names(chn_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
chn_conceding <- merge(chn_home_conceding,chn_away_conceding,by='Group.1',all = T)
chn_conceding$TGC <- chn_conceding$TFthc + chn_conceding$TFtac

#poisson model
#get total games played
chn_GP <- nrow(CHN)
#Calculate total home goals for each division
chn_T_HG <- sum(chn_home_gs$x)
#calculate average home goal
chn_avg_HG <- round(chn_T_HG /chn_GP, digits = 4)
############################################################
#Calculate total away goals for each division
chn_T_AG <- sum(chn_away_gs$x)
#calculate average away goal
chn_avg_AG <- round(chn_T_AG /chn_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
chn_home_as <- round(((chn_home_gs$x/chn_home_games))/chn_avg_HG, digits = 4)
#calculate away attack strength
chn_away_as <- round(((chn_away_gs$x/chn_away_games))/chn_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
chn_avg_HC <- round(chn_T_AG /chn_GP, digits = 4)
#avg away concede
chn_avg_AC <- round(chn_T_HG /chn_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
chn_home_ds <- round(((chn_home_gc$x/chn_home_games))/chn_avg_HC, digits = 4)
#away defense strength
chn_away_ds <- round(((chn_away_gc$x/chn_away_games))/chn_avg_AC, digits = 4)
#############################################################################
#home poisson data
#chn
chn_division <- c()
chn_division[1:length(chn_teams)] <- "CHN"
chn_home_poisson <- cbind(chn_division,chn_teams,chn_avg_HG,chn_home_as,chn_home_ds)
#################################################################################
#away poisson data
#chn
chn_division <- c()
chn_division[1:length(chn_teams)] <- "CHN"
chn_away_poisson <- cbind(chn_division,chn_teams,chn_avg_AG,chn_away_as,chn_away_ds)
#################################################################################################################
################################################################################################################
dnk_home_games <- c()
dnk_away_games <-c()
for (i_dnk in 1:length(dnk_teams))
{

  dnk_home_games[i_dnk] <- nrow(DNK[DNK$Home == dnk_teams[i_dnk],])
  dnk_away_games[i_dnk]  <- nrow(DNK[DNK$Away == dnk_teams[i_dnk],])

}

dnk_games_played <- dnk_home_games + dnk_away_games

#home goals scored
dnk_home_gs <- aggregate(DNK$HG, by = list(DNK$Home), FUN = sum)
dnk_home_gs_avg <- aggregate(DNK$HG, by = list(DNK$Home),mean)
dnk_home_scoring <- merge(dnk_home_gs,dnk_home_gs_avg, by='Group.1',all = T)
names(dnk_home_scoring)[names(dnk_home_scoring) == "x.x"] <- "TFthg"
names(dnk_home_scoring)[names(dnk_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
dnk_away_gs <- aggregate(DNK$AG, by = list(DNK$Away), FUN = sum)
dnk_away_gs_avg <- aggregate(DNK$AG, by = list(DNK$Away),mean)
dnk_away_scoring <- merge(dnk_away_gs,dnk_away_gs_avg, by='Group.1',all = T)
names(dnk_away_scoring)[names(dnk_away_scoring) == "x.x"] <- "TFtag"
names(dnk_away_scoring)[names(dnk_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
dnk_scoring <- merge(dnk_home_scoring,dnk_away_scoring,by='Group.1',all = T)
dnk_scoring$TGS <- dnk_scoring$TFthg + dnk_scoring$TFtag

#home goals conceded
dnk_home_gc <- aggregate(DNK$AG, by = list(DNK$Home), FUN = sum)
dnk_home_gc_avg <- aggregate(DNK$AG, by = list(DNK$Home),mean)
dnk_home_conceding <- merge(dnk_home_gc,dnk_home_gc_avg, by='Group.1',all = T)
names(dnk_home_conceding)[names(dnk_home_conceding) == "x.x"] <- "TFthc"
names(dnk_home_conceding)[names(dnk_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
dnk_away_gc <- aggregate(DNK$HG, by = list(DNK$Away), FUN = sum)
dnk_away_gc_avg <- aggregate(DNK$HG, by = list(DNK$Away),mean)
dnk_away_conceding <- merge(dnk_away_gc,dnk_away_gc_avg, by='Group.1',all = T)
names(dnk_away_conceding)[names(dnk_away_conceding) == "x.x"] <- "TFtac"
names(dnk_away_conceding)[names(dnk_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
dnk_conceding <- merge(dnk_home_conceding,dnk_away_conceding,by='Group.1',all = T)
dnk_conceding$TGC <- dnk_conceding$TFthc + dnk_conceding$TFtac

#poisson model
#get total games played
dnk_GP <- nrow(DNK)
#Calculate total home goals for each division
dnk_T_HG <- sum(dnk_home_gs$x)
#calculate average home goal
dnk_avg_HG <- round(dnk_T_HG /dnk_GP, digits = 4)
############################################################
#Calculate total away goals for each division
dnk_T_AG <- sum(dnk_away_gs$x)
#calculate average away goal
dnk_avg_AG <- round(dnk_T_AG /dnk_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
dnk_home_as <- round(((dnk_home_gs$x/dnk_home_games))/dnk_avg_HG, digits = 4)
#calculate away attack strength
dnk_away_as <- round(((dnk_away_gs$x/dnk_away_games))/dnk_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
dnk_avg_HC <- round(dnk_T_AG /dnk_GP, digits = 4)
#avg away concede
dnk_avg_AC <- round(dnk_T_HG /dnk_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
dnk_home_ds <- round(((dnk_home_gc$x/dnk_home_games))/dnk_avg_HC, digits = 4)
#away defense strength
dnk_away_ds <- round(((dnk_away_gc$x/dnk_away_games))/dnk_avg_AC, digits = 4)
#############################################################################
#home poisson data
#dnk
dnk_division <- c()
dnk_division[1:length(dnk_teams)] <- "DNK"
dnk_home_poisson <- cbind(dnk_division,dnk_teams,dnk_avg_HG,dnk_home_as,dnk_home_ds)
#################################################################################
#away poisson data
#dnk
dnk_division <- c()
dnk_division[1:length(dnk_teams)] <- "DNK"
dnk_away_poisson <- cbind(dnk_division,dnk_teams,dnk_avg_AG,dnk_away_as,dnk_away_ds)
########################################################################################################
########################################################################################################
fin_home_games <- c()
fin_away_games <-c()
for (i_fin in 1:length(fin_teams))
{

  fin_home_games[i_fin] <- nrow(FIN[FIN$Home == fin_teams[i_fin],])
  fin_away_games[i_fin]  <- nrow(FIN[FIN$Away == fin_teams[i_fin],])

}

fin_games_played <- fin_home_games + fin_away_games

#home goals scored
fin_home_gs <- aggregate(FIN$HG, by = list(FIN$Home), FUN = sum)
fin_home_gs_avg <- aggregate(FIN$HG, by = list(FIN$Home),mean)
fin_home_scoring <- merge(fin_home_gs,fin_home_gs_avg, by='Group.1',all = T)
names(fin_home_scoring)[names(fin_home_scoring) == "x.x"] <- "TFthg"
names(fin_home_scoring)[names(fin_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
fin_away_gs <- aggregate(FIN$AG, by = list(FIN$Away), FUN = sum)
fin_away_gs_avg <- aggregate(FIN$AG, by = list(FIN$Away),mean)
fin_away_scoring <- merge(fin_away_gs,fin_away_gs_avg, by='Group.1',all = T)
names(fin_away_scoring)[names(fin_away_scoring) == "x.x"] <- "TFtag"
names(fin_away_scoring)[names(fin_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
fin_scoring <- merge(fin_home_scoring,fin_away_scoring,by='Group.1',all = T)
fin_scoring$TGS <- fin_scoring$TFthg + fin_scoring$TFtag

#home goals conceded
fin_home_gc <- aggregate(FIN$AG, by = list(FIN$Home), FUN = sum)
fin_home_gc_avg <- aggregate(FIN$AG, by = list(FIN$Home),mean)
fin_home_conceding <- merge(fin_home_gc,fin_home_gc_avg, by='Group.1',all = T)
names(fin_home_conceding)[names(fin_home_conceding) == "x.x"] <- "TFthc"
names(fin_home_conceding)[names(fin_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
fin_away_gc <- aggregate(FIN$HG, by = list(FIN$Away), FUN = sum)
fin_away_gc_avg <- aggregate(FIN$HG, by = list(FIN$Away),mean)
fin_away_conceding <- merge(fin_away_gc,fin_away_gc_avg, by='Group.1',all = T)
names(fin_away_conceding)[names(fin_away_conceding) == "x.x"] <- "TFtac"
names(fin_away_conceding)[names(fin_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
fin_conceding <- merge(fin_home_conceding,fin_away_conceding,by='Group.1',all = T)
fin_conceding$TGC <- fin_conceding$TFthc + fin_conceding$TFtac

#poisson model
#get total games played
fin_GP <- nrow(FIN)
#Calculate total home goals for each division
fin_T_HG <- sum(fin_home_gs$x)
#calculate average home goal
fin_avg_HG <- round(fin_T_HG /fin_GP, digits = 4)
############################################################
#Calculate total away goals for each division
fin_T_AG <- sum(fin_away_gs$x)
#calculate average away goal
fin_avg_AG <- round(fin_T_AG /fin_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
fin_home_as <- round(((fin_home_gs$x/fin_home_games))/fin_avg_HG, digits = 4)
#calculate away attack strength
fin_away_as <- round(((fin_away_gs$x/fin_away_games))/fin_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
fin_avg_HC <- round(fin_T_AG /fin_GP, digits = 4)
#avg away concede
fin_avg_AC <- round(fin_T_HG /fin_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
fin_home_ds <- round(((fin_home_gc$x/fin_home_games))/fin_avg_HC, digits = 4)
#away defense strength
fin_away_ds <- round(((fin_away_gc$x/fin_away_games))/fin_avg_AC, digits = 4)
#############################################################################
#home poisson data
#fin
fin_division <- c()
fin_division[1:length(fin_teams)] <- "FIN"
fin_home_poisson <- cbind(fin_division,fin_teams,fin_avg_HG,fin_home_as,fin_home_ds)
#################################################################################
#away poisson data
#fin
fin_division <- c()
fin_division[1:length(fin_teams)] <- "FIN"
fin_away_poisson <- cbind(fin_division,fin_teams,fin_avg_AG,fin_away_as,fin_away_ds)



#########################################################################################################
#########################################################################################################
irl_home_games <- c()
irl_away_games <-c()
for (i_irl in 1:length(irl_teams))
{

  irl_home_games[i_irl] <- nrow(IRL[IRL$Home == irl_teams[i_irl],])
  irl_away_games[i_irl]  <- nrow(IRL[IRL$Away == irl_teams[i_irl],])

}

irl_games_played <- irl_home_games + irl_away_games

#home goals scored
irl_home_gs <- aggregate(IRL$HG, by = list(IRL$Home), FUN = sum)
irl_home_gs_avg <- aggregate(IRL$HG, by = list(IRL$Home),mean)
irl_home_scoring <- merge(irl_home_gs,irl_home_gs_avg, by='Group.1',all = T)
names(irl_home_scoring)[names(irl_home_scoring) == "x.x"] <- "TFthg"
names(irl_home_scoring)[names(irl_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
irl_away_gs <- aggregate(IRL$AG, by = list(IRL$Away), FUN = sum)
irl_away_gs_avg <- aggregate(IRL$AG, by = list(IRL$Away),mean)
irl_away_scoring <- merge(irl_away_gs,irl_away_gs_avg, by='Group.1',all = T)
names(irl_away_scoring)[names(irl_away_scoring) == "x.x"] <- "TFtag"
names(irl_away_scoring)[names(irl_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
irl_scoring <- merge(irl_home_scoring,irl_away_scoring,by='Group.1',all = T)
irl_scoring$TGS <- irl_scoring$TFthg + irl_scoring$TFtag

#home goals conceded
irl_home_gc <- aggregate(IRL$AG, by = list(IRL$Home), FUN = sum)
irl_home_gc_avg <- aggregate(IRL$AG, by = list(IRL$Home),mean)
irl_home_conceding <- merge(irl_home_gc,irl_home_gc_avg, by='Group.1',all = T)
names(irl_home_conceding)[names(irl_home_conceding) == "x.x"] <- "TFthc"
names(irl_home_conceding)[names(irl_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
irl_away_gc <- aggregate(IRL$HG, by = list(IRL$Away), FUN = sum)
irl_away_gc_avg <- aggregate(IRL$HG, by = list(IRL$Away),mean)
irl_away_conceding <- merge(irl_away_gc,irl_away_gc_avg, by='Group.1',all = T)
names(irl_away_conceding)[names(irl_away_conceding) == "x.x"] <- "TFtac"
names(irl_away_conceding)[names(irl_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
irl_conceding <- merge(irl_home_conceding,irl_away_conceding,by='Group.1',all = T)
irl_conceding$TGC <- irl_conceding$TFthc + irl_conceding$TFtac

#poisson model
#get total games played
irl_GP <- nrow(IRL)
#Calculate total home goals for each division
irl_T_HG <- sum(irl_home_gs$x)
#calculate average home goal
irl_avg_HG <- round(irl_T_HG /irl_GP, digits = 4)
############################################################
#Calculate total away goals for each division
irl_T_AG <- sum(irl_away_gs$x)
#calculate average away goal
irl_avg_AG <- round(irl_T_AG /irl_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
irl_home_as <- round(((irl_home_gs$x/irl_home_games))/irl_avg_HG, digits = 4)
#calculate away attack strength
irl_away_as <- round(((irl_away_gs$x/irl_away_games))/irl_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
irl_avg_HC <- round(irl_T_AG /irl_GP, digits = 4)
#avg away concede
irl_avg_AC <- round(irl_T_HG /irl_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
irl_home_ds <- round(((irl_home_gc$x/irl_home_games))/irl_avg_HC, digits = 4)
#away defense strength
irl_away_ds <- round(((irl_away_gc$x/irl_away_games))/irl_avg_AC, digits = 4)
#############################################################################
#home poisson data
#irl
irl_division <- c()
irl_division[1:length(irl_teams)] <- "IRL"
irl_home_poisson <- cbind(irl_division,irl_teams,irl_avg_HG,irl_home_as,irl_home_ds)
#################################################################################
#away poisson data
#irl
irl_division <- c()
irl_division[1:length(irl_teams)] <- "IRL"
irl_away_poisson <- cbind(irl_division,irl_teams,irl_avg_AG,irl_away_as,irl_away_ds)
#################################################################################################################
#################################################################################################################
jpn_home_games <- c()
jpn_away_games <-c()
for (i_jpn in 1:length(jpn_teams))
{

  jpn_home_games[i_jpn] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn],])
  jpn_away_games[i_jpn]  <- nrow(JPN[JPN$Away == jpn_teams[i_jpn],])

}

jpn_games_played <- jpn_home_games + jpn_away_games

#home goals scored
jpn_home_gs <- aggregate(JPN$HG, by = list(JPN$Home), FUN = sum)
jpn_home_gs_avg <- aggregate(JPN$HG, by = list(JPN$Home),mean)
jpn_home_scoring <- merge(jpn_home_gs,jpn_home_gs_avg, by='Group.1',all = T)
names(jpn_home_scoring)[names(jpn_home_scoring) == "x.x"] <- "TFthg"
names(jpn_home_scoring)[names(jpn_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
jpn_away_gs <- aggregate(JPN$AG, by = list(JPN$Away), FUN = sum)
jpn_away_gs_avg <- aggregate(JPN$AG, by = list(JPN$Away),mean)
jpn_away_scoring <- merge(jpn_away_gs,jpn_away_gs_avg, by='Group.1',all = T)
names(jpn_away_scoring)[names(jpn_away_scoring) == "x.x"] <- "TFtag"
names(jpn_away_scoring)[names(jpn_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
jpn_scoring <- merge(jpn_home_scoring,jpn_away_scoring,by='Group.1',all = T)
jpn_scoring$TGS <- jpn_scoring$TFthg + jpn_scoring$TFtag

#home goals conceded
jpn_home_gc <- aggregate(JPN$AG, by = list(JPN$Home), FUN = sum)
jpn_home_gc_avg <- aggregate(JPN$AG, by = list(JPN$Home),mean)
jpn_home_conceding <- merge(jpn_home_gc,jpn_home_gc_avg, by='Group.1',all = T)
names(jpn_home_conceding)[names(jpn_home_conceding) == "x.x"] <- "TFthc"
names(jpn_home_conceding)[names(jpn_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
jpn_away_gc <- aggregate(JPN$HG, by = list(JPN$Away), FUN = sum)
jpn_away_gc_avg <- aggregate(JPN$HG, by = list(JPN$Away),mean)
jpn_away_conceding <- merge(jpn_away_gc,jpn_away_gc_avg, by='Group.1',all = T)
names(jpn_away_conceding)[names(jpn_away_conceding) == "x.x"] <- "TFtac"
names(jpn_away_conceding)[names(jpn_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
jpn_conceding <- merge(jpn_home_conceding,jpn_away_conceding,by='Group.1',all = T)
jpn_conceding$TGC <- jpn_conceding$TFthc + jpn_conceding$TFtac

#poisson model
#get total games played
jpn_GP <- nrow(JPN)
#Calculate total home goals for each division
jpn_T_HG <- sum(jpn_home_gs$x)
#calculate average home goal
jpn_avg_HG <- round(jpn_T_HG /jpn_GP, digits = 4)
############################################################
#Calculate total away goals for each division
jpn_T_AG <- sum(jpn_away_gs$x)
#calculate average away goal
jpn_avg_AG <- round(jpn_T_AG /jpn_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
jpn_home_as <- round(((jpn_home_gs$x/jpn_home_games))/jpn_avg_HG, digits = 4)
#calculate away attack strength
jpn_away_as <- round(((jpn_away_gs$x/jpn_away_games))/jpn_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
jpn_avg_HC <- round(jpn_T_AG /jpn_GP, digits = 4)
#avg away concede
jpn_avg_AC <- round(jpn_T_HG /jpn_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
jpn_home_ds <- round(((jpn_home_gc$x/jpn_home_games))/jpn_avg_HC, digits = 4)
#away defense strength
jpn_away_ds <- round(((jpn_away_gc$x/jpn_away_games))/jpn_avg_AC, digits = 4)
#############################################################################
#home poisson data
#jpn
jpn_division <- c()
jpn_division[1:length(jpn_teams)] <- "JPN"
jpn_home_poisson <- cbind(jpn_division,jpn_teams,jpn_avg_HG,jpn_home_as,jpn_home_ds)
#################################################################################
#away poisson data
#jpn
jpn_division <- c()
jpn_division[1:length(jpn_teams)] <- "JPN"
jpn_away_poisson <- cbind(jpn_division,jpn_teams,jpn_avg_AG,jpn_away_as,jpn_away_ds)
###########################################################################################################
###########################################################################################################
mex_home_games <- c()
mex_away_games <-c()
for (i_mex in 1:length(mex_teams))
{

  mex_home_games[i_mex] <- nrow(MEX[MEX$Home == mex_teams[i_mex],])
  mex_away_games[i_mex]  <- nrow(MEX[MEX$Away == mex_teams[i_mex],])

}

mex_games_played <- mex_home_games + mex_away_games

#home goals scored
mex_home_gs <- aggregate(MEX$HG, by = list(MEX$Home), FUN = sum)
mex_home_gs_avg <- aggregate(MEX$HG, by = list(MEX$Home),mean)
mex_home_scoring <- merge(mex_home_gs,mex_home_gs_avg, by='Group.1',all = T)
names(mex_home_scoring)[names(mex_home_scoring) == "x.x"] <- "TFthg"
names(mex_home_scoring)[names(mex_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
mex_away_gs <- aggregate(MEX$AG, by = list(MEX$Away), FUN = sum)
mex_away_gs_avg <- aggregate(MEX$AG, by = list(MEX$Away),mean)
mex_away_scoring <- merge(mex_away_gs,mex_away_gs_avg, by='Group.1',all = T)
names(mex_away_scoring)[names(mex_away_scoring) == "x.x"] <- "TFtag"
names(mex_away_scoring)[names(mex_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
mex_scoring <- merge(mex_home_scoring,mex_away_scoring,by='Group.1',all = T)
mex_scoring$TGS <- mex_scoring$TFthg + mex_scoring$TFtag

#home goals conceded
mex_home_gc <- aggregate(MEX$AG, by = list(MEX$Home), FUN = sum)
mex_home_gc_avg <- aggregate(MEX$AG, by = list(MEX$Home),mean)
mex_home_conceding <- merge(mex_home_gc,mex_home_gc_avg, by='Group.1',all = T)
names(mex_home_conceding)[names(mex_home_conceding) == "x.x"] <- "TFthc"
names(mex_home_conceding)[names(mex_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
mex_away_gc <- aggregate(MEX$HG, by = list(MEX$Away), FUN = sum)
mex_away_gc_avg <- aggregate(MEX$HG, by = list(MEX$Away),mean)
mex_away_conceding <- merge(mex_away_gc,mex_away_gc_avg, by='Group.1',all = T)
names(mex_away_conceding)[names(mex_away_conceding) == "x.x"] <- "TFtac"
names(mex_away_conceding)[names(mex_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
mex_conceding <- merge(mex_home_conceding,mex_away_conceding,by='Group.1',all = T)
mex_conceding$TGC <- mex_conceding$TFthc + mex_conceding$TFtac

#poisson model
#get total games played
mex_GP <- nrow(MEX)
#Calculate total home goals for each division
mex_T_HG <- sum(mex_home_gs$x)
#calculate average home goal
mex_avg_HG <- round(mex_T_HG /mex_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mex_T_AG <- sum(mex_away_gs$x)
#calculate average away goal
mex_avg_AG <- round(mex_T_AG /mex_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mex_home_as <- round(((mex_home_gs$x/mex_home_games))/mex_avg_HG, digits = 4)
#calculate away attack strength
mex_away_as <- round(((mex_away_gs$x/mex_away_games))/mex_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
mex_avg_HC <- round(mex_T_AG /mex_GP, digits = 4)
#avg away concede
mex_avg_AC <- round(mex_T_HG /mex_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
mex_home_ds <- round(((mex_home_gc$x/mex_home_games))/mex_avg_HC, digits = 4)
#away defense strength
mex_away_ds <- round(((mex_away_gc$x/mex_away_games))/mex_avg_AC, digits = 4)
#############################################################################
#home poisson data
#mex
mex_division <- c()
mex_division[1:length(mex_teams)] <- "MEX"
mex_home_poisson <- cbind(mex_division,mex_teams,mex_avg_HG,mex_home_as,mex_home_ds)
#################################################################################
#away poisson data
#mex
mex_division <- c()
mex_division[1:length(mex_teams)] <- "MEX"
mex_away_poisson <- cbind(mex_division,mex_teams,mex_avg_AG,mex_away_as,mex_away_ds)
###################################################################################################
###################################################################################################
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
  ################################################################################################
################################################################################################
nor_home_games <- c()
nor_away_games <-c()
for (i_nor in 1:length(nor_teams))
{

  nor_home_games[i_nor] <- nrow(NOR[NOR$Home == nor_teams[i_nor],])
  nor_away_games[i_nor]  <- nrow(NOR[NOR$Away == nor_teams[i_nor],])

}

nor_games_played <- nor_home_games + nor_away_games

#home goals scored
nor_home_gs <- aggregate(NOR$HG, by = list(NOR$Home), FUN = sum)
nor_home_gs_avg <- aggregate(NOR$HG, by = list(NOR$Home),mean)
nor_home_scoring <- merge(nor_home_gs,nor_home_gs_avg, by='Group.1',all = T)
names(nor_home_scoring)[names(nor_home_scoring) == "x.x"] <- "TFthg"
names(nor_home_scoring)[names(nor_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
nor_away_gs <- aggregate(NOR$AG, by = list(NOR$Away), FUN = sum)
nor_away_gs_avg <- aggregate(NOR$AG, by = list(NOR$Away),mean)
nor_away_scoring <- merge(nor_away_gs,nor_away_gs_avg, by='Group.1',all = T)
names(nor_away_scoring)[names(nor_away_scoring) == "x.x"] <- "TFtag"
names(nor_away_scoring)[names(nor_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
nor_scoring <- merge(nor_home_scoring,nor_away_scoring,by='Group.1',all = T)
nor_scoring$TGS <- nor_scoring$TFthg + nor_scoring$TFtag

#home goals conceded
nor_home_gc <- aggregate(NOR$AG, by = list(NOR$Home), FUN = sum)
nor_home_gc_avg <- aggregate(NOR$AG, by = list(NOR$Home),mean)
nor_home_conceding <- merge(nor_home_gc,nor_home_gc_avg, by='Group.1',all = T)
names(nor_home_conceding)[names(nor_home_conceding) == "x.x"] <- "TFthc"
names(nor_home_conceding)[names(nor_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
nor_away_gc <- aggregate(NOR$HG, by = list(NOR$Away), FUN = sum)
nor_away_gc_avg <- aggregate(NOR$HG, by = list(NOR$Away),mean)
nor_away_conceding <- merge(nor_away_gc,nor_away_gc_avg, by='Group.1',all = T)
names(nor_away_conceding)[names(nor_away_conceding) == "x.x"] <- "TFtac"
names(nor_away_conceding)[names(nor_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
nor_conceding <- merge(nor_home_conceding,nor_away_conceding,by='Group.1',all = T)
nor_conceding$TGC <- nor_conceding$TFthc + nor_conceding$TFtac

#poisson model
#get total games played
nor_GP <- nrow(NOR)
#Calculate total home goals for each division
nor_T_HG <- sum(nor_home_gs$x)
#calculate average home goal
nor_avg_HG <- round(nor_T_HG /nor_GP, digits = 4)
############################################################
#Calculate total away goals for each division
nor_T_AG <- sum(nor_away_gs$x)
#calculate average away goal
nor_avg_AG <- round(nor_T_AG /nor_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
nor_home_as <- round(((nor_home_gs$x/nor_home_games))/nor_avg_HG, digits = 4)
#calculate away attack strength
nor_away_as <- round(((nor_away_gs$x/nor_away_games))/nor_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
nor_avg_HC <- round(nor_T_AG /nor_GP, digits = 4)
#avg away concede
nor_avg_AC <- round(nor_T_HG /nor_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
nor_home_ds <- round(((nor_home_gc$x/nor_home_games))/nor_avg_HC, digits = 4)
#away defense strength
nor_away_ds <- round(((nor_away_gc$x/nor_away_games))/nor_avg_AC, digits = 4)
#############################################################################
#home poisson data
#nor
nor_division <- c()
nor_division[1:length(nor_teams)] <- "NOR"
nor_home_poisson <- cbind(nor_division,nor_teams,nor_avg_HG,nor_home_as,nor_home_ds)
#################################################################################
#away poisson data
#nor
nor_division <- c()
nor_division[1:length(nor_teams)] <- "NOR"
nor_away_poisson <- cbind(nor_division,nor_teams,nor_avg_AG,nor_away_as,nor_away_ds)
###################################################################################################
###################################################################################################
pol_home_games <- c()
pol_away_games <-c()
for (i_pol in 1:length(pol_teams))
{

  pol_home_games[i_pol] <- nrow(POL[POL$Home == pol_teams[i_pol],])
  pol_away_games[i_pol]  <- nrow(POL[POL$Away == pol_teams[i_pol],])

}

pol_games_played <- pol_home_games + pol_away_games

#home goals scored
pol_home_gs <- aggregate(POL$HG, by = list(POL$Home), FUN = sum)
pol_home_gs_avg <- aggregate(POL$HG, by = list(POL$Home),mean)
pol_home_scoring <- merge(pol_home_gs,pol_home_gs_avg, by='Group.1',all = T)
names(pol_home_scoring)[names(pol_home_scoring) == "x.x"] <- "TFthg"
names(pol_home_scoring)[names(pol_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
pol_away_gs <- aggregate(POL$AG, by = list(POL$Away), FUN = sum)
pol_away_gs_avg <- aggregate(POL$AG, by = list(POL$Away),mean)
pol_away_scoring <- merge(pol_away_gs,pol_away_gs_avg, by='Group.1',all = T)
names(pol_away_scoring)[names(pol_away_scoring) == "x.x"] <- "TFtag"
names(pol_away_scoring)[names(pol_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
pol_scoring <- merge(pol_home_scoring,pol_away_scoring,by='Group.1',all = T)
pol_scoring$TGS <- pol_scoring$TFthg + pol_scoring$TFtag

#home goals conceded
pol_home_gc <- aggregate(POL$AG, by = list(POL$Home), FUN = sum)
pol_home_gc_avg <- aggregate(POL$AG, by = list(POL$Home),mean)
pol_home_conceding <- merge(pol_home_gc,pol_home_gc_avg, by='Group.1',all = T)
names(pol_home_conceding)[names(pol_home_conceding) == "x.x"] <- "TFthc"
names(pol_home_conceding)[names(pol_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
pol_away_gc <- aggregate(POL$HG, by = list(POL$Away), FUN = sum)
pol_away_gc_avg <- aggregate(POL$HG, by = list(POL$Away),mean)
pol_away_conceding <- merge(pol_away_gc,pol_away_gc_avg, by='Group.1',all = T)
names(pol_away_conceding)[names(pol_away_conceding) == "x.x"] <- "TFtac"
names(pol_away_conceding)[names(pol_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
pol_conceding <- merge(pol_home_conceding,pol_away_conceding,by='Group.1',all = T)
pol_conceding$TGC <- pol_conceding$TFthc + pol_conceding$TFtac

#poisson model
#get total games played
pol_GP <- nrow(POL)
#Calculate total home goals for each division
pol_T_HG <- sum(pol_home_gs$x)
#calculate average home goal
pol_avg_HG <- round(pol_T_HG /pol_GP, digits = 4)
############################################################
#Calculate total away goals for each division
pol_T_AG <- sum(pol_away_gs$x)
#calculate average away goal
pol_avg_AG <- round(pol_T_AG /pol_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
pol_home_as <- round(((pol_home_gs$x/pol_home_games))/pol_avg_HG, digits = 4)
#calculate away attack strength
pol_away_as <- round(((pol_away_gs$x/pol_away_games))/pol_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
pol_avg_HC <- round(pol_T_AG /pol_GP, digits = 4)
#avg away concede
pol_avg_AC <- round(pol_T_HG /pol_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
pol_home_ds <- round(((pol_home_gc$x/pol_home_games))/pol_avg_HC, digits = 4)
#away defense strength
pol_away_ds <- round(((pol_away_gc$x/pol_away_games))/pol_avg_AC, digits = 4)
#############################################################################
#home poisson data
#pol
pol_division <- c()
pol_division[1:length(pol_teams)] <- "POL"
pol_home_poisson <- cbind(pol_division,pol_teams,pol_avg_HG,pol_home_as,pol_home_ds)
#################################################################################
#away poisson data
#pol
pol_division <- c()
pol_division[1:length(pol_teams)] <- "POL"
pol_away_poisson <- cbind(pol_division,pol_teams,pol_avg_AG,pol_away_as,pol_away_ds)
###############################################################################################
###############################################################################################
rou_home_games <- c()
rou_away_games <-c()
for (i_rou in 1:length(rou_teams))
{

  rou_home_games[i_rou] <- nrow(ROU[ROU$Home == rou_teams[i_rou],])
  rou_away_games[i_rou]  <- nrow(ROU[ROU$Away == rou_teams[i_rou],])

}

rou_games_played <- rou_home_games + rou_away_games

#home goals scored
rou_home_gs <- aggregate(ROU$HG, by = list(ROU$Home), FUN = sum)
rou_home_gs_avg <- aggregate(ROU$HG, by = list(ROU$Home),mean)
rou_home_scoring <- merge(rou_home_gs,rou_home_gs_avg, by='Group.1',all = T)
names(rou_home_scoring)[names(rou_home_scoring) == "x.x"] <- "TFthg"
names(rou_home_scoring)[names(rou_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
rou_away_gs <- aggregate(ROU$AG, by = list(ROU$Away), FUN = sum)
rou_away_gs_avg <- aggregate(ROU$AG, by = list(ROU$Away),mean)
rou_away_scoring <- merge(rou_away_gs,rou_away_gs_avg, by='Group.1',all = T)
names(rou_away_scoring)[names(rou_away_scoring) == "x.x"] <- "TFtag"
names(rou_away_scoring)[names(rou_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
rou_scoring <- merge(rou_home_scoring,rou_away_scoring,by='Group.1',all = T)
rou_scoring$TGS <- rou_scoring$TFthg + rou_scoring$TFtag

#home goals conceded
rou_home_gc <- aggregate(ROU$AG, by = list(ROU$Home), FUN = sum)
rou_home_gc_avg <- aggregate(ROU$AG, by = list(ROU$Home),mean)
rou_home_conceding <- merge(rou_home_gc,rou_home_gc_avg, by='Group.1',all = T)
names(rou_home_conceding)[names(rou_home_conceding) == "x.x"] <- "TFthc"
names(rou_home_conceding)[names(rou_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
rou_away_gc <- aggregate(ROU$HG, by = list(ROU$Away), FUN = sum)
rou_away_gc_avg <- aggregate(ROU$HG, by = list(ROU$Away),mean)
rou_away_conceding <- merge(rou_away_gc,rou_away_gc_avg, by='Group.1',all = T)
names(rou_away_conceding)[names(rou_away_conceding) == "x.x"] <- "TFtac"
names(rou_away_conceding)[names(rou_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
rou_conceding <- merge(rou_home_conceding,rou_away_conceding,by='Group.1',all = T)
rou_conceding$TGC <- rou_conceding$TFthc + rou_conceding$TFtac

#poisson model
#get total games played
rou_GP <- nrow(ROU)
#Calculate total home goals for each division
rou_T_HG <- sum(rou_home_gs$x)
#calculate average home goal
rou_avg_HG <- round(rou_T_HG /rou_GP, digits = 4)
############################################################
#Calculate total away goals for each division
rou_T_AG <- sum(rou_away_gs$x)
#calculate average away goal
rou_avg_AG <- round(rou_T_AG /rou_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
rou_home_as <- round(((rou_home_gs$x/rou_home_games))/rou_avg_HG, digits = 4)
#calculate away attack strength
rou_away_as <- round(((rou_away_gs$x/rou_away_games))/rou_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
rou_avg_HC <- round(rou_T_AG /rou_GP, digits = 4)
#avg away concede
rou_avg_AC <- round(rou_T_HG /rou_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
rou_home_ds <- round(((rou_home_gc$x/rou_home_games))/rou_avg_HC, digits = 4)
#away defense strength
rou_away_ds <- round(((rou_away_gc$x/rou_away_games))/rou_avg_AC, digits = 4)
#############################################################################
#home poisson data
#rou
rou_division <- c()
rou_division[1:length(rou_teams)] <- "ROU"
rou_home_poisson <- cbind(rou_division,rou_teams,rou_avg_HG,rou_home_as,rou_home_ds)
#################################################################################
#away poisson data
#rou
rou_division <- c()
rou_division[1:length(rou_teams)] <- "ROU"
rou_away_poisson <- cbind(rou_division,rou_teams,rou_avg_AG,rou_away_as,rou_away_ds)
#####################################################################################################
#####################################################################################################
rus_home_games <- c()
rus_away_games <-c()
for (i_rus in 1:length(rus_teams))
{

  rus_home_games[i_rus] <- nrow(RUS[RUS$Home == rus_teams[i_rus],])
  rus_away_games[i_rus]  <- nrow(RUS[RUS$Away == rus_teams[i_rus],])

}

rus_games_played <- rus_home_games + rus_away_games

#home goals scored
rus_home_gs <- aggregate(RUS$HG, by = list(RUS$Home), FUN = sum)
rus_home_gs_avg <- aggregate(RUS$HG, by = list(RUS$Home),mean)
rus_home_scoring <- merge(rus_home_gs,rus_home_gs_avg, by='Group.1',all = T)
names(rus_home_scoring)[names(rus_home_scoring) == "x.x"] <- "TFthg"
names(rus_home_scoring)[names(rus_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
rus_away_gs <- aggregate(RUS$AG, by = list(RUS$Away), FUN = sum)
rus_away_gs_avg <- aggregate(RUS$AG, by = list(RUS$Away),mean)
rus_away_scoring <- merge(rus_away_gs,rus_away_gs_avg, by='Group.1',all = T)
names(rus_away_scoring)[names(rus_away_scoring) == "x.x"] <- "TFtag"
names(rus_away_scoring)[names(rus_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
rus_scoring <- merge(rus_home_scoring,rus_away_scoring,by='Group.1',all = T)
rus_scoring$TGS <- rus_scoring$TFthg + rus_scoring$TFtag

#home goals conceded
rus_home_gc <- aggregate(RUS$AG, by = list(RUS$Home), FUN = sum)
rus_home_gc_avg <- aggregate(RUS$AG, by = list(RUS$Home),mean)
rus_home_conceding <- merge(rus_home_gc,rus_home_gc_avg, by='Group.1',all = T)
names(rus_home_conceding)[names(rus_home_conceding) == "x.x"] <- "TFthc"
names(rus_home_conceding)[names(rus_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
rus_away_gc <- aggregate(RUS$HG, by = list(RUS$Away), FUN = sum)
rus_away_gc_avg <- aggregate(RUS$HG, by = list(RUS$Away),mean)
rus_away_conceding <- merge(rus_away_gc,rus_away_gc_avg, by='Group.1',all = T)
names(rus_away_conceding)[names(rus_away_conceding) == "x.x"] <- "TFtac"
names(rus_away_conceding)[names(rus_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
rus_conceding <- merge(rus_home_conceding,rus_away_conceding,by='Group.1',all = T)
rus_conceding$TGC <- rus_conceding$TFthc + rus_conceding$TFtac

#poisson model
#get total games played
rus_GP <- nrow(RUS)
#Calculate total home goals for each division
rus_T_HG <- sum(rus_home_gs$x)
#calculate average home goal
rus_avg_HG <- round(rus_T_HG /rus_GP, digits = 4)
############################################################
#Calculate total away goals for each division
rus_T_AG <- sum(rus_away_gs$x)
#calculate average away goal
rus_avg_AG <- round(rus_T_AG /rus_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
rus_home_as <- round(((rus_home_gs$x/rus_home_games))/rus_avg_HG, digits = 4)
#calculate away attack strength
rus_away_as <- round(((rus_away_gs$x/rus_away_games))/rus_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
rus_avg_HC <- round(rus_T_AG /rus_GP, digits = 4)
#avg away concede
rus_avg_AC <- round(rus_T_HG /rus_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
rus_home_ds <- round(((rus_home_gc$x/rus_home_games))/rus_avg_HC, digits = 4)
#away defense strength
rus_away_ds <- round(((rus_away_gc$x/rus_away_games))/rus_avg_AC, digits = 4)
#############################################################################
#home poisson data
#rus
rus_division <- c()
rus_division[1:length(rus_teams)] <- "RUS"
rus_home_poisson <- cbind(rus_division,rus_teams,rus_avg_HG,rus_home_as,rus_home_ds)
#################################################################################
#away poisson data
#rus
rus_division <- c()
rus_division[1:length(rus_teams)] <- "RUS"
rus_away_poisson <- cbind(rus_division,rus_teams,rus_avg_AG,rus_away_as,rus_away_ds)
################################################################################################
################################################################################################
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
#########################################################################################################
#########################################################################################################
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






























