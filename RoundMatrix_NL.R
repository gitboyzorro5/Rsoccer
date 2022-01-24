Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
nrow(ARG_rounds)
unlink('NL/Roundmatrix.xlsx')
####################################################################################################
####################################################################################################
#arg
#hwins and away wins
arg_home_wins_rnds <- c()
arg_away_wins_rnds <- c()
arg_home_draws_rnds <- c()
arg_away_draws_rnds <- c()
arg_home_loss_rnds <- c()
arg_away_loss_rnds <- c()
head(ARG)
#arg_krounds is total rounds as per current season
arg_krounds <- tail(unique(ARG_rounds$arg_matchday),1)
arg_roundmatrix <- data.frame(matrix(nrow = length(arg_teams),ncol = arg_krounds))

for(i_arg_krounds in 1:arg_krounds)
{

  for (i_arg_wins_rnds in 1:length(arg_teams))
  {

    arg_home_wins_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$Home == arg_teams[i_arg_wins_rnds] & ARG_rounds$Res == "H" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_wins_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$Away == arg_teams[i_arg_wins_rnds] & ARG_rounds$Res == "A" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_home_draws_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$Home == arg_teams[i_arg_wins_rnds] & ARG_rounds$Res == "D" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_draws_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$Away == arg_teams[i_arg_wins_rnds] & ARG_rounds$Res == "D" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_home_loss_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$Home == arg_teams[i_arg_wins_rnds] & ARG_rounds$Res == "A" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_loss_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$Away == arg_teams[i_arg_wins_rnds] & ARG_rounds$Res == "H" & ARG_rounds$arg_matchday <= i_arg_krounds,])

  }

  arg_total_wins_rnds <- arg_home_wins_rnds + arg_away_wins_rnds
  arg_total_draws_rnds <- arg_home_draws_rnds + arg_away_draws_rnds
  arg_total_loss_rnds <- arg_home_loss_rnds + arg_away_loss_rnds


  arg_home_games_rnds <- c()
  arg_away_games_rnds <-c()

  for (i_arg_rnds in 1:length(arg_teams))
  {

    arg_home_games_rnds[i_arg_rnds] <- nrow(ARG_rounds[ARG_rounds$Home == arg_teams[i_arg_rnds] & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_games_rnds[i_arg_rnds]  <- nrow(ARG_rounds[ARG_rounds$Away == arg_teams[i_arg_rnds] & ARG_rounds$arg_matchday <= i_arg_krounds,])

  }

  arg_games_played_rnds <- arg_home_games_rnds + arg_away_games_rnds

  arg_league_table_rnds <- cbind(arg_teams,arg_games_played_rnds,arg_total_wins_rnds,arg_total_draws_rnds,arg_total_loss_rnds)

  # arg_GS <- arg_scoring$TGS
  # arg_GC <-arg_conceding$TGC
  # arg_GD <- arg_scoring$TGS - arg_conceding$TGC

  arg_PTS_rnds <- (arg_total_wins_rnds*3) + (arg_total_draws_rnds*1)
  arg_league_table_rnds <- cbind(arg_league_table_rnds,arg_PTS_rnds)
  arg_league_table_rnds <- as.data.frame(arg_league_table_rnds)
  #rename the columns
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_teams"] <- "Team"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_games_played_rnds"] <- "P"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_total_wins_rnds"] <- "W"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_total_draws_rnds"] <- "D"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_total_loss_rnds"] <- "L"
  # names(arg_league_table)[names(arg_league_table) == "arg_GS"] <- "F"
  # names(arg_league_table)[names(arg_league_table) == "arg_GC"] <- "A"
  points_arg_rnds <- arg_league_table_rnds[order(as.numeric(arg_league_table_rnds$arg_PTS_rnds), decreasing = TRUE),]
  points_arg_rnds$arg_rank_rnds <- 1:length(arg_teams)
  row.names(points_arg_rnds) <- points_arg_rnds$arg_rank


  points_arg_rnds <- points_arg_rnds[order(as.character(points_arg_rnds$Team)),]


  arg_roundmatrix[,i_arg_krounds] <- as.data.frame(points_arg_rnds$arg_rank_rnds)


}

arg_roundmatrix <- cbind(arg_teams,arg_roundmatrix)
write.xlsx(arg_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "arg",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#aut
#hwins and away wins
aut_home_wins_rnds <- c()
aut_away_wins_rnds <- c()
aut_home_draws_rnds <- c()
aut_away_draws_rnds <- c()
aut_home_loss_rnds <- c()
aut_away_loss_rnds <- c()

#aut_krounds is total rounds as per current season
aut_krounds <- tail(unique(AUT_rounds$aut_matchday),1)
aut_roundmatrix <- data.frame(matrix(nrow = length(aut_teams),ncol = aut_krounds))

for(i_aut_krounds in 1:aut_krounds)
{

  for (i_aut_wins_rnds in 1:length(aut_teams))
  {

    aut_home_wins_rnds[i_aut_wins_rnds] <- nrow(AUT_rounds[AUT_rounds$Home == aut_teams[i_aut_wins_rnds] & AUT_rounds$Res == "H" & AUT_rounds$aut_matchday <= i_aut_krounds,])
    aut_away_wins_rnds[i_aut_wins_rnds] <- nrow(AUT_rounds[AUT_rounds$Away == aut_teams[i_aut_wins_rnds] & AUT_rounds$Res == "A" & AUT_rounds$aut_matchday <= i_aut_krounds,])
    aut_home_draws_rnds[i_aut_wins_rnds] <- nrow(AUT_rounds[AUT_rounds$Home == aut_teams[i_aut_wins_rnds] & AUT_rounds$Res == "D" & AUT_rounds$aut_matchday <= i_aut_krounds,])
    aut_away_draws_rnds[i_aut_wins_rnds] <- nrow(AUT_rounds[AUT_rounds$Away == aut_teams[i_aut_wins_rnds] & AUT_rounds$Res == "D" & AUT_rounds$aut_matchday <= i_aut_krounds,])
    aut_home_loss_rnds[i_aut_wins_rnds] <- nrow(AUT_rounds[AUT_rounds$Home == aut_teams[i_aut_wins_rnds] & AUT_rounds$Res == "A" & AUT_rounds$aut_matchday <= i_aut_krounds,])
    aut_away_loss_rnds[i_aut_wins_rnds] <- nrow(AUT_rounds[AUT_rounds$Away == aut_teams[i_aut_wins_rnds] & AUT_rounds$Res == "H" & AUT_rounds$aut_matchday <= i_aut_krounds,])

  }

  aut_total_wins_rnds <- aut_home_wins_rnds + aut_away_wins_rnds
  aut_total_draws_rnds <- aut_home_draws_rnds + aut_away_draws_rnds
  aut_total_loss_rnds <- aut_home_loss_rnds + aut_away_loss_rnds


  aut_home_games_rnds <- c()
  aut_away_games_rnds <-c()

  for (i_aut_rnds in 1:length(aut_teams))
  {

    aut_home_games_rnds[i_aut_rnds] <- nrow(AUT_rounds[AUT_rounds$Home == aut_teams[i_aut_rnds] & AUT_rounds$aut_matchday <= i_aut_krounds,])
    aut_away_games_rnds[i_aut_rnds]  <- nrow(AUT_rounds[AUT_rounds$Away == aut_teams[i_aut_rnds] & AUT_rounds$aut_matchday <= i_aut_krounds,])

  }

  aut_games_played_rnds <- aut_home_games_rnds + aut_away_games_rnds

  aut_league_table_rnds <- cbind(aut_teams,aut_games_played_rnds,aut_total_wins_rnds,aut_total_draws_rnds,aut_total_loss_rnds)

  # aut_GS <- aut_scoring$TGS
  # aut_GC <-aut_conceding$TGC
  # aut_GD <- aut_scoring$TGS - aut_conceding$TGC

  aut_PTS_rnds <- (aut_total_wins_rnds*3) + (aut_total_draws_rnds*1)
  aut_league_table_rnds <- cbind(aut_league_table_rnds,aut_PTS_rnds)
  aut_league_table_rnds <- as.data.frame(aut_league_table_rnds)
  #rename the columns
  names(aut_league_table_rnds)[names(aut_league_table_rnds) == "aut_teams"] <- "Team"
  names(aut_league_table_rnds)[names(aut_league_table_rnds) == "aut_games_played_rnds"] <- "P"
  names(aut_league_table_rnds)[names(aut_league_table_rnds) == "aut_total_wins_rnds"] <- "W"
  names(aut_league_table_rnds)[names(aut_league_table_rnds) == "aut_total_draws_rnds"] <- "D"
  names(aut_league_table_rnds)[names(aut_league_table_rnds) == "aut_total_loss_rnds"] <- "L"
  # names(aut_league_table)[names(aut_league_table) == "aut_GS"] <- "F"
  # names(aut_league_table)[names(aut_league_table) == "aut_GC"] <- "A"
  points_aut_rnds <- aut_league_table_rnds[order(as.numeric(aut_league_table_rnds$aut_PTS_rnds), decreasing = TRUE),]
  points_aut_rnds$aut_rank_rnds <- 1:length(aut_teams)
  row.names(points_aut_rnds) <- points_aut_rnds$aut_rank


  points_aut_rnds <- points_aut_rnds[order(as.character(points_aut_rnds$Team)),]


  aut_roundmatrix[,i_aut_krounds] <- as.data.frame(points_aut_rnds$aut_rank_rnds)


}

aut_roundmatrix <- cbind(aut_teams,aut_roundmatrix)
write.xlsx(aut_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "aut",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#bra
#hwins and away wins
bra_home_wins_rnds <- c()
bra_away_wins_rnds <- c()
bra_home_draws_rnds <- c()
bra_away_draws_rnds <- c()
bra_home_loss_rnds <- c()
bra_away_loss_rnds <- c()

#bra_krounds is total rounds as per current season
bra_krounds <- tail(unique(BRA_rounds$bra_matchday),1)
bra_roundmatrix <- data.frame(matrix(nrow = length(bra_teams),ncol = bra_krounds))

for(i_bra_krounds in 1:bra_krounds)
{

  for (i_bra_wins_rnds in 1:length(bra_teams))
  {

    bra_home_wins_rnds[i_bra_wins_rnds] <- nrow(BRA_rounds[BRA_rounds$Home == bra_teams[i_bra_wins_rnds] & BRA_rounds$Res == "H" & BRA_rounds$bra_matchday <= i_bra_krounds,])
    bra_away_wins_rnds[i_bra_wins_rnds] <- nrow(BRA_rounds[BRA_rounds$Away == bra_teams[i_bra_wins_rnds] & BRA_rounds$Res == "A" & BRA_rounds$bra_matchday <= i_bra_krounds,])
    bra_home_draws_rnds[i_bra_wins_rnds] <- nrow(BRA_rounds[BRA_rounds$Home == bra_teams[i_bra_wins_rnds] & BRA_rounds$Res == "D" & BRA_rounds$bra_matchday <= i_bra_krounds,])
    bra_away_draws_rnds[i_bra_wins_rnds] <- nrow(BRA_rounds[BRA_rounds$Away == bra_teams[i_bra_wins_rnds] & BRA_rounds$Res == "D" & BRA_rounds$bra_matchday <= i_bra_krounds,])
    bra_home_loss_rnds[i_bra_wins_rnds] <- nrow(BRA_rounds[BRA_rounds$Home == bra_teams[i_bra_wins_rnds] & BRA_rounds$Res == "A" & BRA_rounds$bra_matchday <= i_bra_krounds,])
    bra_away_loss_rnds[i_bra_wins_rnds] <- nrow(BRA_rounds[BRA_rounds$Away == bra_teams[i_bra_wins_rnds] & BRA_rounds$Res == "H" & BRA_rounds$bra_matchday <= i_bra_krounds,])

  }

  bra_total_wins_rnds <- bra_home_wins_rnds + bra_away_wins_rnds
  bra_total_draws_rnds <- bra_home_draws_rnds + bra_away_draws_rnds
  bra_total_loss_rnds <- bra_home_loss_rnds + bra_away_loss_rnds


  bra_home_games_rnds <- c()
  bra_away_games_rnds <-c()

  for (i_bra_rnds in 1:length(bra_teams))
  {

    bra_home_games_rnds[i_bra_rnds] <- nrow(BRA_rounds[BRA_rounds$Home == bra_teams[i_bra_rnds] & BRA_rounds$bra_matchday <= i_bra_krounds,])
    bra_away_games_rnds[i_bra_rnds]  <- nrow(BRA_rounds[BRA_rounds$Away == bra_teams[i_bra_rnds] & BRA_rounds$bra_matchday <= i_bra_krounds,])

  }

  bra_games_played_rnds <- bra_home_games_rnds + bra_away_games_rnds

  bra_league_table_rnds <- cbind(bra_teams,bra_games_played_rnds,bra_total_wins_rnds,bra_total_draws_rnds,bra_total_loss_rnds)

  # bra_GS <- bra_scoring$TGS
  # bra_GC <-bra_conceding$TGC
  # bra_GD <- bra_scoring$TGS - bra_conceding$TGC

  bra_PTS_rnds <- (bra_total_wins_rnds*3) + (bra_total_draws_rnds*1)
  bra_league_table_rnds <- cbind(bra_league_table_rnds,bra_PTS_rnds)
  bra_league_table_rnds <- as.data.frame(bra_league_table_rnds)
  #rename the columns
  names(bra_league_table_rnds)[names(bra_league_table_rnds) == "bra_teams"] <- "Team"
  names(bra_league_table_rnds)[names(bra_league_table_rnds) == "bra_games_played_rnds"] <- "P"
  names(bra_league_table_rnds)[names(bra_league_table_rnds) == "bra_total_wins_rnds"] <- "W"
  names(bra_league_table_rnds)[names(bra_league_table_rnds) == "bra_total_draws_rnds"] <- "D"
  names(bra_league_table_rnds)[names(bra_league_table_rnds) == "bra_total_loss_rnds"] <- "L"
  # names(bra_league_table)[names(bra_league_table) == "bra_GS"] <- "F"
  # names(bra_league_table)[names(bra_league_table) == "bra_GC"] <- "A"
  points_bra_rnds <- bra_league_table_rnds[order(as.numeric(bra_league_table_rnds$bra_PTS_rnds), decreasing = TRUE),]
  points_bra_rnds$bra_rank_rnds <- 1:length(bra_teams)
  row.names(points_bra_rnds) <- points_bra_rnds$bra_rank


  points_bra_rnds <- points_bra_rnds[order(as.character(points_bra_rnds$Team)),]


  bra_roundmatrix[,i_bra_krounds] <- as.data.frame(points_bra_rnds$bra_rank_rnds)


}

bra_roundmatrix <- cbind(bra_teams,bra_roundmatrix)
write.xlsx(bra_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "bra",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#chn
#hwins and away wins
chn_home_wins_rnds <- c()
chn_away_wins_rnds <- c()
chn_home_draws_rnds <- c()
chn_away_draws_rnds <- c()
chn_home_loss_rnds <- c()
chn_away_loss_rnds <- c()

#chn_krounds is total rounds as per current season
chn_krounds <- tail(unique(CHN_rounds$chn_matchday),1)
chn_roundmatrix <- data.frame(matrix(nrow = length(chn_teams),ncol = chn_krounds))

for(i_chn_krounds in 1:chn_krounds)
{

  for (i_chn_wins_rnds in 1:length(chn_teams))
  {

    chn_home_wins_rnds[i_chn_wins_rnds] <- nrow(CHN_rounds[CHN_rounds$Home == chn_teams[i_chn_wins_rnds] & CHN_rounds$Res == "H" & CHN_rounds$chn_matchday <= i_chn_krounds,])
    chn_away_wins_rnds[i_chn_wins_rnds] <- nrow(CHN_rounds[CHN_rounds$Away == chn_teams[i_chn_wins_rnds] & CHN_rounds$Res == "A" & CHN_rounds$chn_matchday <= i_chn_krounds,])
    chn_home_draws_rnds[i_chn_wins_rnds] <- nrow(CHN_rounds[CHN_rounds$Home == chn_teams[i_chn_wins_rnds] & CHN_rounds$Res == "D" & CHN_rounds$chn_matchday <= i_chn_krounds,])
    chn_away_draws_rnds[i_chn_wins_rnds] <- nrow(CHN_rounds[CHN_rounds$Away == chn_teams[i_chn_wins_rnds] & CHN_rounds$Res == "D" & CHN_rounds$chn_matchday <= i_chn_krounds,])
    chn_home_loss_rnds[i_chn_wins_rnds] <- nrow(CHN_rounds[CHN_rounds$Home == chn_teams[i_chn_wins_rnds] & CHN_rounds$Res == "A" & CHN_rounds$chn_matchday <= i_chn_krounds,])
    chn_away_loss_rnds[i_chn_wins_rnds] <- nrow(CHN_rounds[CHN_rounds$Away == chn_teams[i_chn_wins_rnds] & CHN_rounds$Res == "H" & CHN_rounds$chn_matchday <= i_chn_krounds,])

  }

  chn_total_wins_rnds <- chn_home_wins_rnds + chn_away_wins_rnds
  chn_total_draws_rnds <- chn_home_draws_rnds + chn_away_draws_rnds
  chn_total_loss_rnds <- chn_home_loss_rnds + chn_away_loss_rnds


  chn_home_games_rnds <- c()
  chn_away_games_rnds <-c()

  for (i_chn_rnds in 1:length(chn_teams))
  {

    chn_home_games_rnds[i_chn_rnds] <- nrow(CHN_rounds[CHN_rounds$Home == chn_teams[i_chn_rnds] & CHN_rounds$chn_matchday <= i_chn_krounds,])
    chn_away_games_rnds[i_chn_rnds]  <- nrow(CHN_rounds[CHN_rounds$Away == chn_teams[i_chn_rnds] & CHN_rounds$chn_matchday <= i_chn_krounds,])

  }

  chn_games_played_rnds <- chn_home_games_rnds + chn_away_games_rnds

  chn_league_table_rnds <- cbind(chn_teams,chn_games_played_rnds,chn_total_wins_rnds,chn_total_draws_rnds,chn_total_loss_rnds)

  # chn_GS <- chn_scoring$TGS
  # chn_GC <-chn_conceding$TGC
  # chn_GD <- chn_scoring$TGS - chn_conceding$TGC

  chn_PTS_rnds <- (chn_total_wins_rnds*3) + (chn_total_draws_rnds*1)
  chn_league_table_rnds <- cbind(chn_league_table_rnds,chn_PTS_rnds)
  chn_league_table_rnds <- as.data.frame(chn_league_table_rnds)
  #rename the columns
  names(chn_league_table_rnds)[names(chn_league_table_rnds) == "chn_teams"] <- "Team"
  names(chn_league_table_rnds)[names(chn_league_table_rnds) == "chn_games_played_rnds"] <- "P"
  names(chn_league_table_rnds)[names(chn_league_table_rnds) == "chn_total_wins_rnds"] <- "W"
  names(chn_league_table_rnds)[names(chn_league_table_rnds) == "chn_total_draws_rnds"] <- "D"
  names(chn_league_table_rnds)[names(chn_league_table_rnds) == "chn_total_loss_rnds"] <- "L"
  # names(chn_league_table)[names(chn_league_table) == "chn_GS"] <- "F"
  # names(chn_league_table)[names(chn_league_table) == "chn_GC"] <- "A"
  points_chn_rnds <- chn_league_table_rnds[order(as.numeric(chn_league_table_rnds$chn_PTS_rnds), decreasing = TRUE),]
  points_chn_rnds$chn_rank_rnds <- 1:length(chn_teams)
  row.names(points_chn_rnds) <- points_chn_rnds$chn_rank


  points_chn_rnds <- points_chn_rnds[order(as.character(points_chn_rnds$Team)),]


  chn_roundmatrix[,i_chn_krounds] <- as.data.frame(points_chn_rnds$chn_rank_rnds)


}

chn_roundmatrix <- cbind(chn_teams,chn_roundmatrix)
write.xlsx(chn_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "chn",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#dnk
#hwins and away wins
dnk_home_wins_rnds <- c()
dnk_away_wins_rnds <- c()
dnk_home_draws_rnds <- c()
dnk_away_draws_rnds <- c()
dnk_home_loss_rnds <- c()
dnk_away_loss_rnds <- c()

#dnk_krounds is total rounds as per current season
dnk_krounds <- tail(unique(DNK_rounds$dnk_matchday),1)
dnk_roundmatrix <- data.frame(matrix(nrow = length(dnk_teams),ncol = dnk_krounds))

for(i_dnk_krounds in 1:dnk_krounds)
{

  for (i_dnk_wins_rnds in 1:length(dnk_teams))
  {

    dnk_home_wins_rnds[i_dnk_wins_rnds] <- nrow(DNK_rounds[DNK_rounds$Home == dnk_teams[i_dnk_wins_rnds] & DNK_rounds$Res == "H" & DNK_rounds$dnk_matchday <= i_dnk_krounds,])
    dnk_away_wins_rnds[i_dnk_wins_rnds] <- nrow(DNK_rounds[DNK_rounds$Away == dnk_teams[i_dnk_wins_rnds] & DNK_rounds$Res == "A" & DNK_rounds$dnk_matchday <= i_dnk_krounds,])
    dnk_home_draws_rnds[i_dnk_wins_rnds] <- nrow(DNK_rounds[DNK_rounds$Home == dnk_teams[i_dnk_wins_rnds] & DNK_rounds$Res == "D" & DNK_rounds$dnk_matchday <= i_dnk_krounds,])
    dnk_away_draws_rnds[i_dnk_wins_rnds] <- nrow(DNK_rounds[DNK_rounds$Away == dnk_teams[i_dnk_wins_rnds] & DNK_rounds$Res == "D" & DNK_rounds$dnk_matchday <= i_dnk_krounds,])
    dnk_home_loss_rnds[i_dnk_wins_rnds] <- nrow(DNK_rounds[DNK_rounds$Home == dnk_teams[i_dnk_wins_rnds] & DNK_rounds$Res == "A" & DNK_rounds$dnk_matchday <= i_dnk_krounds,])
    dnk_away_loss_rnds[i_dnk_wins_rnds] <- nrow(DNK_rounds[DNK_rounds$Away == dnk_teams[i_dnk_wins_rnds] & DNK_rounds$Res == "H" & DNK_rounds$dnk_matchday <= i_dnk_krounds,])

  }

  dnk_total_wins_rnds <- dnk_home_wins_rnds + dnk_away_wins_rnds
  dnk_total_draws_rnds <- dnk_home_draws_rnds + dnk_away_draws_rnds
  dnk_total_loss_rnds <- dnk_home_loss_rnds + dnk_away_loss_rnds


  dnk_home_games_rnds <- c()
  dnk_away_games_rnds <-c()

  for (i_dnk_rnds in 1:length(dnk_teams))
  {

    dnk_home_games_rnds[i_dnk_rnds] <- nrow(DNK_rounds[DNK_rounds$Home == dnk_teams[i_dnk_rnds] & DNK_rounds$dnk_matchday <= i_dnk_krounds,])
    dnk_away_games_rnds[i_dnk_rnds]  <- nrow(DNK_rounds[DNK_rounds$Away == dnk_teams[i_dnk_rnds] & DNK_rounds$dnk_matchday <= i_dnk_krounds,])

  }

  dnk_games_played_rnds <- dnk_home_games_rnds + dnk_away_games_rnds

  dnk_league_table_rnds <- cbind(dnk_teams,dnk_games_played_rnds,dnk_total_wins_rnds,dnk_total_draws_rnds,dnk_total_loss_rnds)

  # dnk_GS <- dnk_scoring$TGS
  # dnk_GC <-dnk_conceding$TGC
  # dnk_GD <- dnk_scoring$TGS - dnk_conceding$TGC

  dnk_PTS_rnds <- (dnk_total_wins_rnds*3) + (dnk_total_draws_rnds*1)
  dnk_league_table_rnds <- cbind(dnk_league_table_rnds,dnk_PTS_rnds)
  dnk_league_table_rnds <- as.data.frame(dnk_league_table_rnds)
  #rename the columns
  names(dnk_league_table_rnds)[names(dnk_league_table_rnds) == "dnk_teams"] <- "Team"
  names(dnk_league_table_rnds)[names(dnk_league_table_rnds) == "dnk_games_played_rnds"] <- "P"
  names(dnk_league_table_rnds)[names(dnk_league_table_rnds) == "dnk_total_wins_rnds"] <- "W"
  names(dnk_league_table_rnds)[names(dnk_league_table_rnds) == "dnk_total_draws_rnds"] <- "D"
  names(dnk_league_table_rnds)[names(dnk_league_table_rnds) == "dnk_total_loss_rnds"] <- "L"
  # names(dnk_league_table)[names(dnk_league_table) == "dnk_GS"] <- "F"
  # names(dnk_league_table)[names(dnk_league_table) == "dnk_GC"] <- "A"
  points_dnk_rnds <- dnk_league_table_rnds[order(as.numeric(dnk_league_table_rnds$dnk_PTS_rnds), decreasing = TRUE),]
  points_dnk_rnds$dnk_rank_rnds <- 1:length(dnk_teams)
  row.names(points_dnk_rnds) <- points_dnk_rnds$dnk_rank


  points_dnk_rnds <- points_dnk_rnds[order(as.character(points_dnk_rnds$Team)),]


  dnk_roundmatrix[,i_dnk_krounds] <- as.data.frame(points_dnk_rnds$dnk_rank_rnds)


}

dnk_roundmatrix <- cbind(dnk_teams,dnk_roundmatrix)
write.xlsx(dnk_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "dnk",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#fin
#hwins and away wins
fin_home_wins_rnds <- c()
fin_away_wins_rnds <- c()
fin_home_draws_rnds <- c()
fin_away_draws_rnds <- c()
fin_home_loss_rnds <- c()
fin_away_loss_rnds <- c()

#fin_krounds is total rounds as per current season
fin_krounds <- tail(unique(FIN_rounds$fin_matchday),1)
fin_roundmatrix <- data.frame(matrix(nrow = length(fin_teams),ncol = fin_krounds))

for(i_fin_krounds in 1:fin_krounds)
{

  for (i_fin_wins_rnds in 1:length(fin_teams))
  {

    fin_home_wins_rnds[i_fin_wins_rnds] <- nrow(FIN_rounds[FIN_rounds$Home == fin_teams[i_fin_wins_rnds] & FIN_rounds$Res == "H" & FIN_rounds$fin_matchday <= i_fin_krounds,])
    fin_away_wins_rnds[i_fin_wins_rnds] <- nrow(FIN_rounds[FIN_rounds$Away == fin_teams[i_fin_wins_rnds] & FIN_rounds$Res == "A" & FIN_rounds$fin_matchday <= i_fin_krounds,])
    fin_home_draws_rnds[i_fin_wins_rnds] <- nrow(FIN_rounds[FIN_rounds$Home == fin_teams[i_fin_wins_rnds] & FIN_rounds$Res == "D" & FIN_rounds$fin_matchday <= i_fin_krounds,])
    fin_away_draws_rnds[i_fin_wins_rnds] <- nrow(FIN_rounds[FIN_rounds$Away == fin_teams[i_fin_wins_rnds] & FIN_rounds$Res == "D" & FIN_rounds$fin_matchday <= i_fin_krounds,])
    fin_home_loss_rnds[i_fin_wins_rnds] <- nrow(FIN_rounds[FIN_rounds$Home == fin_teams[i_fin_wins_rnds] & FIN_rounds$Res == "A" & FIN_rounds$fin_matchday <= i_fin_krounds,])
    fin_away_loss_rnds[i_fin_wins_rnds] <- nrow(FIN_rounds[FIN_rounds$Away == fin_teams[i_fin_wins_rnds] & FIN_rounds$Res == "H" & FIN_rounds$fin_matchday <= i_fin_krounds,])

  }

  fin_total_wins_rnds <- fin_home_wins_rnds + fin_away_wins_rnds
  fin_total_draws_rnds <- fin_home_draws_rnds + fin_away_draws_rnds
  fin_total_loss_rnds <- fin_home_loss_rnds + fin_away_loss_rnds


  fin_home_games_rnds <- c()
  fin_away_games_rnds <-c()

  for (i_fin_rnds in 1:length(fin_teams))
  {

    fin_home_games_rnds[i_fin_rnds] <- nrow(FIN_rounds[FIN_rounds$Home == fin_teams[i_fin_rnds] & FIN_rounds$fin_matchday <= i_fin_krounds,])
    fin_away_games_rnds[i_fin_rnds]  <- nrow(FIN_rounds[FIN_rounds$Away == fin_teams[i_fin_rnds] & FIN_rounds$fin_matchday <= i_fin_krounds,])

  }

  fin_games_played_rnds <- fin_home_games_rnds + fin_away_games_rnds

  fin_league_table_rnds <- cbind(fin_teams,fin_games_played_rnds,fin_total_wins_rnds,fin_total_draws_rnds,fin_total_loss_rnds)

  # fin_GS <- fin_scoring$TGS
  # fin_GC <-fin_conceding$TGC
  # fin_GD <- fin_scoring$TGS - fin_conceding$TGC

  fin_PTS_rnds <- (fin_total_wins_rnds*3) + (fin_total_draws_rnds*1)
  fin_league_table_rnds <- cbind(fin_league_table_rnds,fin_PTS_rnds)
  fin_league_table_rnds <- as.data.frame(fin_league_table_rnds)
  #rename the columns
  names(fin_league_table_rnds)[names(fin_league_table_rnds) == "fin_teams"] <- "Team"
  names(fin_league_table_rnds)[names(fin_league_table_rnds) == "fin_games_played_rnds"] <- "P"
  names(fin_league_table_rnds)[names(fin_league_table_rnds) == "fin_total_wins_rnds"] <- "W"
  names(fin_league_table_rnds)[names(fin_league_table_rnds) == "fin_total_draws_rnds"] <- "D"
  names(fin_league_table_rnds)[names(fin_league_table_rnds) == "fin_total_loss_rnds"] <- "L"
  # names(fin_league_table)[names(fin_league_table) == "fin_GS"] <- "F"
  # names(fin_league_table)[names(fin_league_table) == "fin_GC"] <- "A"
  points_fin_rnds <- fin_league_table_rnds[order(as.numeric(fin_league_table_rnds$fin_PTS_rnds), decreasing = TRUE),]
  points_fin_rnds$fin_rank_rnds <- 1:length(fin_teams)
  row.names(points_fin_rnds) <- points_fin_rnds$fin_rank


  points_fin_rnds <- points_fin_rnds[order(as.character(points_fin_rnds$Team)),]


  fin_roundmatrix[,i_fin_krounds] <- as.data.frame(points_fin_rnds$fin_rank_rnds)


}

fin_roundmatrix <- cbind(fin_teams,fin_roundmatrix)
write.xlsx(fin_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "fin",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#irl
#hwins and away wins
irl_home_wins_rnds <- c()
irl_away_wins_rnds <- c()
irl_home_draws_rnds <- c()
irl_away_draws_rnds <- c()
irl_home_loss_rnds <- c()
irl_away_loss_rnds <- c()

#irl_krounds is total rounds as per current season
irl_krounds <- tail(unique(IRL_rounds$irl_matchday),1)
irl_roundmatrix <- data.frame(matrix(nrow = length(irl_teams),ncol = irl_krounds))

for(i_irl_krounds in 1:irl_krounds)
{

  for (i_irl_wins_rnds in 1:length(irl_teams))
  {

    irl_home_wins_rnds[i_irl_wins_rnds] <- nrow(IRL_rounds[IRL_rounds$Home == irl_teams[i_irl_wins_rnds] & IRL_rounds$Res == "H" & IRL_rounds$irl_matchday <= i_irl_krounds,])
    irl_away_wins_rnds[i_irl_wins_rnds] <- nrow(IRL_rounds[IRL_rounds$Away == irl_teams[i_irl_wins_rnds] & IRL_rounds$Res == "A" & IRL_rounds$irl_matchday <= i_irl_krounds,])
    irl_home_draws_rnds[i_irl_wins_rnds] <- nrow(IRL_rounds[IRL_rounds$Home == irl_teams[i_irl_wins_rnds] & IRL_rounds$Res == "D" & IRL_rounds$irl_matchday <= i_irl_krounds,])
    irl_away_draws_rnds[i_irl_wins_rnds] <- nrow(IRL_rounds[IRL_rounds$Away == irl_teams[i_irl_wins_rnds] & IRL_rounds$Res == "D" & IRL_rounds$irl_matchday <= i_irl_krounds,])
    irl_home_loss_rnds[i_irl_wins_rnds] <- nrow(IRL_rounds[IRL_rounds$Home == irl_teams[i_irl_wins_rnds] & IRL_rounds$Res == "A" & IRL_rounds$irl_matchday <= i_irl_krounds,])
    irl_away_loss_rnds[i_irl_wins_rnds] <- nrow(IRL_rounds[IRL_rounds$Away == irl_teams[i_irl_wins_rnds] & IRL_rounds$Res == "H" & IRL_rounds$irl_matchday <= i_irl_krounds,])

  }

  irl_total_wins_rnds <- irl_home_wins_rnds + irl_away_wins_rnds
  irl_total_draws_rnds <- irl_home_draws_rnds + irl_away_draws_rnds
  irl_total_loss_rnds <- irl_home_loss_rnds + irl_away_loss_rnds


  irl_home_games_rnds <- c()
  irl_away_games_rnds <-c()

  for (i_irl_rnds in 1:length(irl_teams))
  {

    irl_home_games_rnds[i_irl_rnds] <- nrow(IRL_rounds[IRL_rounds$Home == irl_teams[i_irl_rnds] & IRL_rounds$irl_matchday <= i_irl_krounds,])
    irl_away_games_rnds[i_irl_rnds]  <- nrow(IRL_rounds[IRL_rounds$Away == irl_teams[i_irl_rnds] & IRL_rounds$irl_matchday <= i_irl_krounds,])

  }

  irl_games_played_rnds <- irl_home_games_rnds + irl_away_games_rnds

  irl_league_table_rnds <- cbind(irl_teams,irl_games_played_rnds,irl_total_wins_rnds,irl_total_draws_rnds,irl_total_loss_rnds)

  # irl_GS <- irl_scoring$TGS
  # irl_GC <-irl_conceding$TGC
  # irl_GD <- irl_scoring$TGS - irl_conceding$TGC

  irl_PTS_rnds <- (irl_total_wins_rnds*3) + (irl_total_draws_rnds*1)
  irl_league_table_rnds <- cbind(irl_league_table_rnds,irl_PTS_rnds)
  irl_league_table_rnds <- as.data.frame(irl_league_table_rnds)
  #rename the columns
  names(irl_league_table_rnds)[names(irl_league_table_rnds) == "irl_teams"] <- "Team"
  names(irl_league_table_rnds)[names(irl_league_table_rnds) == "irl_games_played_rnds"] <- "P"
  names(irl_league_table_rnds)[names(irl_league_table_rnds) == "irl_total_wins_rnds"] <- "W"
  names(irl_league_table_rnds)[names(irl_league_table_rnds) == "irl_total_draws_rnds"] <- "D"
  names(irl_league_table_rnds)[names(irl_league_table_rnds) == "irl_total_loss_rnds"] <- "L"
  # names(irl_league_table)[names(irl_league_table) == "irl_GS"] <- "F"
  # names(irl_league_table)[names(irl_league_table) == "irl_GC"] <- "A"
  points_irl_rnds <- irl_league_table_rnds[order(as.numeric(irl_league_table_rnds$irl_PTS_rnds), decreasing = TRUE),]
  points_irl_rnds$irl_rank_rnds <- 1:length(irl_teams)
  row.names(points_irl_rnds) <- points_irl_rnds$irl_rank


  points_irl_rnds <- points_irl_rnds[order(as.character(points_irl_rnds$Team)),]


  irl_roundmatrix[,i_irl_krounds] <- as.data.frame(points_irl_rnds$irl_rank_rnds)


}

irl_roundmatrix <- cbind(irl_teams,irl_roundmatrix)
write.xlsx(irl_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "irl",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#jpn
#hwins and away wins
jpn_home_wins_rnds <- c()
jpn_away_wins_rnds <- c()
jpn_home_draws_rnds <- c()
jpn_away_draws_rnds <- c()
jpn_home_loss_rnds <- c()
jpn_away_loss_rnds <- c()

#jpn_krounds is total rounds as per current season
jpn_krounds <- tail(unique(JPN_rounds$jpn_matchday),1)
jpn_roundmatrix <- data.frame(matrix(nrow = length(jpn_teams),ncol = jpn_krounds))

for(i_jpn_krounds in 1:jpn_krounds)
{

  for (i_jpn_wins_rnds in 1:length(jpn_teams))
  {

    jpn_home_wins_rnds[i_jpn_wins_rnds] <- nrow(JPN_rounds[JPN_rounds$Home == jpn_teams[i_jpn_wins_rnds] & JPN_rounds$Res == "H" & JPN_rounds$jpn_matchday <= i_jpn_krounds,])
    jpn_away_wins_rnds[i_jpn_wins_rnds] <- nrow(JPN_rounds[JPN_rounds$Away == jpn_teams[i_jpn_wins_rnds] & JPN_rounds$Res == "A" & JPN_rounds$jpn_matchday <= i_jpn_krounds,])
    jpn_home_draws_rnds[i_jpn_wins_rnds] <- nrow(JPN_rounds[JPN_rounds$Home == jpn_teams[i_jpn_wins_rnds] & JPN_rounds$Res == "D" & JPN_rounds$jpn_matchday <= i_jpn_krounds,])
    jpn_away_draws_rnds[i_jpn_wins_rnds] <- nrow(JPN_rounds[JPN_rounds$Away == jpn_teams[i_jpn_wins_rnds] & JPN_rounds$Res == "D" & JPN_rounds$jpn_matchday <= i_jpn_krounds,])
    jpn_home_loss_rnds[i_jpn_wins_rnds] <- nrow(JPN_rounds[JPN_rounds$Home == jpn_teams[i_jpn_wins_rnds] & JPN_rounds$Res == "A" & JPN_rounds$jpn_matchday <= i_jpn_krounds,])
    jpn_away_loss_rnds[i_jpn_wins_rnds] <- nrow(JPN_rounds[JPN_rounds$Away == jpn_teams[i_jpn_wins_rnds] & JPN_rounds$Res == "H" & JPN_rounds$jpn_matchday <= i_jpn_krounds,])

  }

  jpn_total_wins_rnds <- jpn_home_wins_rnds + jpn_away_wins_rnds
  jpn_total_draws_rnds <- jpn_home_draws_rnds + jpn_away_draws_rnds
  jpn_total_loss_rnds <- jpn_home_loss_rnds + jpn_away_loss_rnds


  jpn_home_games_rnds <- c()
  jpn_away_games_rnds <-c()

  for (i_jpn_rnds in 1:length(jpn_teams))
  {

    jpn_home_games_rnds[i_jpn_rnds] <- nrow(JPN_rounds[JPN_rounds$Home == jpn_teams[i_jpn_rnds] & JPN_rounds$jpn_matchday <= i_jpn_krounds,])
    jpn_away_games_rnds[i_jpn_rnds]  <- nrow(JPN_rounds[JPN_rounds$Away == jpn_teams[i_jpn_rnds] & JPN_rounds$jpn_matchday <= i_jpn_krounds,])

  }

  jpn_games_played_rnds <- jpn_home_games_rnds + jpn_away_games_rnds

  jpn_league_table_rnds <- cbind(jpn_teams,jpn_games_played_rnds,jpn_total_wins_rnds,jpn_total_draws_rnds,jpn_total_loss_rnds)

  # jpn_GS <- jpn_scoring$TGS
  # jpn_GC <-jpn_conceding$TGC
  # jpn_GD <- jpn_scoring$TGS - jpn_conceding$TGC

  jpn_PTS_rnds <- (jpn_total_wins_rnds*3) + (jpn_total_draws_rnds*1)
  jpn_league_table_rnds <- cbind(jpn_league_table_rnds,jpn_PTS_rnds)
  jpn_league_table_rnds <- as.data.frame(jpn_league_table_rnds)
  #rename the columns
  names(jpn_league_table_rnds)[names(jpn_league_table_rnds) == "jpn_teams"] <- "Team"
  names(jpn_league_table_rnds)[names(jpn_league_table_rnds) == "jpn_games_played_rnds"] <- "P"
  names(jpn_league_table_rnds)[names(jpn_league_table_rnds) == "jpn_total_wins_rnds"] <- "W"
  names(jpn_league_table_rnds)[names(jpn_league_table_rnds) == "jpn_total_draws_rnds"] <- "D"
  names(jpn_league_table_rnds)[names(jpn_league_table_rnds) == "jpn_total_loss_rnds"] <- "L"
  # names(jpn_league_table)[names(jpn_league_table) == "jpn_GS"] <- "F"
  # names(jpn_league_table)[names(jpn_league_table) == "jpn_GC"] <- "A"
  points_jpn_rnds <- jpn_league_table_rnds[order(as.numeric(jpn_league_table_rnds$jpn_PTS_rnds), decreasing = TRUE),]
  points_jpn_rnds$jpn_rank_rnds <- 1:length(jpn_teams)
  row.names(points_jpn_rnds) <- points_jpn_rnds$jpn_rank


  points_jpn_rnds <- points_jpn_rnds[order(as.character(points_jpn_rnds$Team)),]


  jpn_roundmatrix[,i_jpn_krounds] <- as.data.frame(points_jpn_rnds$jpn_rank_rnds)


}

jpn_roundmatrix <- cbind(jpn_teams,jpn_roundmatrix)
write.xlsx(jpn_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "jpn",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#mex
#hwins and away wins
mex_home_wins_rnds <- c()
mex_away_wins_rnds <- c()
mex_home_draws_rnds <- c()
mex_away_draws_rnds <- c()
mex_home_loss_rnds <- c()
mex_away_loss_rnds <- c()

#mex_krounds is total rounds as per current season
mex_krounds <- tail(unique(MEX_rounds$mex_matchday),1)
mex_roundmatrix <- data.frame(matrix(nrow = length(mex_teams),ncol = mex_krounds))

for(i_mex_krounds in 1:mex_krounds)
{

  for (i_mex_wins_rnds in 1:length(mex_teams))
  {

    mex_home_wins_rnds[i_mex_wins_rnds] <- nrow(MEX_rounds[MEX_rounds$Home == mex_teams[i_mex_wins_rnds] & MEX_rounds$Res == "H" & MEX_rounds$mex_matchday <= i_mex_krounds,])
    mex_away_wins_rnds[i_mex_wins_rnds] <- nrow(MEX_rounds[MEX_rounds$Away == mex_teams[i_mex_wins_rnds] & MEX_rounds$Res == "A" & MEX_rounds$mex_matchday <= i_mex_krounds,])
    mex_home_draws_rnds[i_mex_wins_rnds] <- nrow(MEX_rounds[MEX_rounds$Home == mex_teams[i_mex_wins_rnds] & MEX_rounds$Res == "D" & MEX_rounds$mex_matchday <= i_mex_krounds,])
    mex_away_draws_rnds[i_mex_wins_rnds] <- nrow(MEX_rounds[MEX_rounds$Away == mex_teams[i_mex_wins_rnds] & MEX_rounds$Res == "D" & MEX_rounds$mex_matchday <= i_mex_krounds,])
    mex_home_loss_rnds[i_mex_wins_rnds] <- nrow(MEX_rounds[MEX_rounds$Home == mex_teams[i_mex_wins_rnds] & MEX_rounds$Res == "A" & MEX_rounds$mex_matchday <= i_mex_krounds,])
    mex_away_loss_rnds[i_mex_wins_rnds] <- nrow(MEX_rounds[MEX_rounds$Away == mex_teams[i_mex_wins_rnds] & MEX_rounds$Res == "H" & MEX_rounds$mex_matchday <= i_mex_krounds,])

  }

  mex_total_wins_rnds <- mex_home_wins_rnds + mex_away_wins_rnds
  mex_total_draws_rnds <- mex_home_draws_rnds + mex_away_draws_rnds
  mex_total_loss_rnds <- mex_home_loss_rnds + mex_away_loss_rnds


  mex_home_games_rnds <- c()
  mex_away_games_rnds <-c()

  for (i_mex_rnds in 1:length(mex_teams))
  {

    mex_home_games_rnds[i_mex_rnds] <- nrow(MEX_rounds[MEX_rounds$Home == mex_teams[i_mex_rnds] & MEX_rounds$mex_matchday <= i_mex_krounds,])
    mex_away_games_rnds[i_mex_rnds]  <- nrow(MEX_rounds[MEX_rounds$Away == mex_teams[i_mex_rnds] & MEX_rounds$mex_matchday <= i_mex_krounds,])

  }

  mex_games_played_rnds <- mex_home_games_rnds + mex_away_games_rnds

  mex_league_table_rnds <- cbind(mex_teams,mex_games_played_rnds,mex_total_wins_rnds,mex_total_draws_rnds,mex_total_loss_rnds)

  # mex_GS <- mex_scoring$TGS
  # mex_GC <-mex_conceding$TGC
  # mex_GD <- mex_scoring$TGS - mex_conceding$TGC

  mex_PTS_rnds <- (mex_total_wins_rnds*3) + (mex_total_draws_rnds*1)
  mex_league_table_rnds <- cbind(mex_league_table_rnds,mex_PTS_rnds)
  mex_league_table_rnds <- as.data.frame(mex_league_table_rnds)
  #rename the columns
  names(mex_league_table_rnds)[names(mex_league_table_rnds) == "mex_teams"] <- "Team"
  names(mex_league_table_rnds)[names(mex_league_table_rnds) == "mex_games_played_rnds"] <- "P"
  names(mex_league_table_rnds)[names(mex_league_table_rnds) == "mex_total_wins_rnds"] <- "W"
  names(mex_league_table_rnds)[names(mex_league_table_rnds) == "mex_total_draws_rnds"] <- "D"
  names(mex_league_table_rnds)[names(mex_league_table_rnds) == "mex_total_loss_rnds"] <- "L"
  # names(mex_league_table)[names(mex_league_table) == "mex_GS"] <- "F"
  # names(mex_league_table)[names(mex_league_table) == "mex_GC"] <- "A"
  points_mex_rnds <- mex_league_table_rnds[order(as.numeric(mex_league_table_rnds$mex_PTS_rnds), decreasing = TRUE),]
  points_mex_rnds$mex_rank_rnds <- 1:length(mex_teams)
  row.names(points_mex_rnds) <- points_mex_rnds$mex_rank


  points_mex_rnds <- points_mex_rnds[order(as.character(points_mex_rnds$Team)),]


  mex_roundmatrix[,i_mex_krounds] <- as.data.frame(points_mex_rnds$mex_rank_rnds)


}

mex_roundmatrix <- cbind(mex_teams,mex_roundmatrix)
write.xlsx(mex_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "mex",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#mls
#hwins and away wins
mls_home_wins_rnds <- c()
mls_away_wins_rnds <- c()
mls_home_draws_rnds <- c()
mls_away_draws_rnds <- c()
mls_home_loss_rnds <- c()
mls_away_loss_rnds <- c()

#mls_krounds is total rounds as per current season
mls_krounds <- tail(unique(MLS_rounds$mls_matchday),1)
mls_roundmatrix <- data.frame(matrix(nrow = length(mls_teams),ncol = mls_krounds))

for(i_mls_krounds in 1:mls_krounds)
{

  for (i_mls_wins_rnds in 1:length(mls_teams))
  {

    mls_home_wins_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$Home == mls_teams[i_mls_wins_rnds] & MLS_rounds$Res == "H" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_wins_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$Away == mls_teams[i_mls_wins_rnds] & MLS_rounds$Res == "A" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_home_draws_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$Home == mls_teams[i_mls_wins_rnds] & MLS_rounds$Res == "D" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_draws_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$Away == mls_teams[i_mls_wins_rnds] & MLS_rounds$Res == "D" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_home_loss_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$Home == mls_teams[i_mls_wins_rnds] & MLS_rounds$Res == "A" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_loss_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$Away == mls_teams[i_mls_wins_rnds] & MLS_rounds$Res == "H" & MLS_rounds$mls_matchday <= i_mls_krounds,])

  }

  mls_total_wins_rnds <- mls_home_wins_rnds + mls_away_wins_rnds
  mls_total_draws_rnds <- mls_home_draws_rnds + mls_away_draws_rnds
  mls_total_loss_rnds <- mls_home_loss_rnds + mls_away_loss_rnds


  mls_home_games_rnds <- c()
  mls_away_games_rnds <-c()

  for (i_mls_rnds in 1:length(mls_teams))
  {

    mls_home_games_rnds[i_mls_rnds] <- nrow(MLS_rounds[MLS_rounds$Home == mls_teams[i_mls_rnds] & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_games_rnds[i_mls_rnds]  <- nrow(MLS_rounds[MLS_rounds$Away == mls_teams[i_mls_rnds] & MLS_rounds$mls_matchday <= i_mls_krounds,])

  }

  mls_games_played_rnds <- mls_home_games_rnds + mls_away_games_rnds

  mls_league_table_rnds <- cbind(mls_teams,mls_games_played_rnds,mls_total_wins_rnds,mls_total_draws_rnds,mls_total_loss_rnds)

  # mls_GS <- mls_scoring$TGS
  # mls_GC <-mls_conceding$TGC
  # mls_GD <- mls_scoring$TGS - mls_conceding$TGC

  mls_PTS_rnds <- (mls_total_wins_rnds*3) + (mls_total_draws_rnds*1)
  mls_league_table_rnds <- cbind(mls_league_table_rnds,mls_PTS_rnds)
  mls_league_table_rnds <- as.data.frame(mls_league_table_rnds)
  #rename the columns
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_teams"] <- "Team"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_games_played_rnds"] <- "P"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_total_wins_rnds"] <- "W"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_total_draws_rnds"] <- "D"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_total_loss_rnds"] <- "L"
  # names(mls_league_table)[names(mls_league_table) == "mls_GS"] <- "F"
  # names(mls_league_table)[names(mls_league_table) == "mls_GC"] <- "A"
  points_mls_rnds <- mls_league_table_rnds[order(as.numeric(mls_league_table_rnds$mls_PTS_rnds), decreasing = TRUE),]
  points_mls_rnds$mls_rank_rnds <- 1:length(mls_teams)
  row.names(points_mls_rnds) <- points_mls_rnds$mls_rank


  points_mls_rnds <- points_mls_rnds[order(as.character(points_mls_rnds$Team)),]


  mls_roundmatrix[,i_mls_krounds] <- as.data.frame(points_mls_rnds$mls_rank_rnds)


}

mls_roundmatrix <- cbind(mls_teams,mls_roundmatrix)
write.xlsx(mls_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "mls",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#nor
#hwins and away wins
nor_home_wins_rnds <- c()
nor_away_wins_rnds <- c()
nor_home_draws_rnds <- c()
nor_away_draws_rnds <- c()
nor_home_loss_rnds <- c()
nor_away_loss_rnds <- c()

#nor_krounds is total rounds as per current season
nor_krounds <- tail(unique(NOR_rounds$nor_matchday),1)
nor_roundmatrix <- data.frame(matrix(nrow = length(nor_teams),ncol = nor_krounds))

for(i_nor_krounds in 1:nor_krounds)
{

  for (i_nor_wins_rnds in 1:length(nor_teams))
  {

    nor_home_wins_rnds[i_nor_wins_rnds] <- nrow(NOR_rounds[NOR_rounds$Home == nor_teams[i_nor_wins_rnds] & NOR_rounds$Res == "H" & NOR_rounds$nor_matchday <= i_nor_krounds,])
    nor_away_wins_rnds[i_nor_wins_rnds] <- nrow(NOR_rounds[NOR_rounds$Away == nor_teams[i_nor_wins_rnds] & NOR_rounds$Res == "A" & NOR_rounds$nor_matchday <= i_nor_krounds,])
    nor_home_draws_rnds[i_nor_wins_rnds] <- nrow(NOR_rounds[NOR_rounds$Home == nor_teams[i_nor_wins_rnds] & NOR_rounds$Res == "D" & NOR_rounds$nor_matchday <= i_nor_krounds,])
    nor_away_draws_rnds[i_nor_wins_rnds] <- nrow(NOR_rounds[NOR_rounds$Away == nor_teams[i_nor_wins_rnds] & NOR_rounds$Res == "D" & NOR_rounds$nor_matchday <= i_nor_krounds,])
    nor_home_loss_rnds[i_nor_wins_rnds] <- nrow(NOR_rounds[NOR_rounds$Home == nor_teams[i_nor_wins_rnds] & NOR_rounds$Res == "A" & NOR_rounds$nor_matchday <= i_nor_krounds,])
    nor_away_loss_rnds[i_nor_wins_rnds] <- nrow(NOR_rounds[NOR_rounds$Away == nor_teams[i_nor_wins_rnds] & NOR_rounds$Res == "H" & NOR_rounds$nor_matchday <= i_nor_krounds,])

  }

  nor_total_wins_rnds <- nor_home_wins_rnds + nor_away_wins_rnds
  nor_total_draws_rnds <- nor_home_draws_rnds + nor_away_draws_rnds
  nor_total_loss_rnds <- nor_home_loss_rnds + nor_away_loss_rnds


  nor_home_games_rnds <- c()
  nor_away_games_rnds <-c()

  for (i_nor_rnds in 1:length(nor_teams))
  {

    nor_home_games_rnds[i_nor_rnds] <- nrow(NOR_rounds[NOR_rounds$Home == nor_teams[i_nor_rnds] & NOR_rounds$nor_matchday <= i_nor_krounds,])
    nor_away_games_rnds[i_nor_rnds]  <- nrow(NOR_rounds[NOR_rounds$Away == nor_teams[i_nor_rnds] & NOR_rounds$nor_matchday <= i_nor_krounds,])

  }

  nor_games_played_rnds <- nor_home_games_rnds + nor_away_games_rnds

  nor_league_table_rnds <- cbind(nor_teams,nor_games_played_rnds,nor_total_wins_rnds,nor_total_draws_rnds,nor_total_loss_rnds)

  # nor_GS <- nor_scoring$TGS
  # nor_GC <-nor_conceding$TGC
  # nor_GD <- nor_scoring$TGS - nor_conceding$TGC

  nor_PTS_rnds <- (nor_total_wins_rnds*3) + (nor_total_draws_rnds*1)
  nor_league_table_rnds <- cbind(nor_league_table_rnds,nor_PTS_rnds)
  nor_league_table_rnds <- as.data.frame(nor_league_table_rnds)
  #rename the columns
  names(nor_league_table_rnds)[names(nor_league_table_rnds) == "nor_teams"] <- "Team"
  names(nor_league_table_rnds)[names(nor_league_table_rnds) == "nor_games_played_rnds"] <- "P"
  names(nor_league_table_rnds)[names(nor_league_table_rnds) == "nor_total_wins_rnds"] <- "W"
  names(nor_league_table_rnds)[names(nor_league_table_rnds) == "nor_total_draws_rnds"] <- "D"
  names(nor_league_table_rnds)[names(nor_league_table_rnds) == "nor_total_loss_rnds"] <- "L"
  # names(nor_league_table)[names(nor_league_table) == "nor_GS"] <- "F"
  # names(nor_league_table)[names(nor_league_table) == "nor_GC"] <- "A"
  points_nor_rnds <- nor_league_table_rnds[order(as.numeric(nor_league_table_rnds$nor_PTS_rnds), decreasing = TRUE),]
  points_nor_rnds$nor_rank_rnds <- 1:length(nor_teams)
  row.names(points_nor_rnds) <- points_nor_rnds$nor_rank


  points_nor_rnds <- points_nor_rnds[order(as.character(points_nor_rnds$Team)),]


  nor_roundmatrix[,i_nor_krounds] <- as.data.frame(points_nor_rnds$nor_rank_rnds)


}

nor_roundmatrix <- cbind(nor_teams,nor_roundmatrix)
write.xlsx(nor_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "nor",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#pol
#hwins and away wins
pol_home_wins_rnds <- c()
pol_away_wins_rnds <- c()
pol_home_draws_rnds <- c()
pol_away_draws_rnds <- c()
pol_home_loss_rnds <- c()
pol_away_loss_rnds <- c()

#pol_krounds is total rounds as per current season
pol_krounds <- tail(unique(POL_rounds$pol_matchday),1)
pol_roundmatrix <- data.frame(matrix(nrow = length(pol_teams),ncol = pol_krounds))

for(i_pol_krounds in 1:pol_krounds)
{

  for (i_pol_wins_rnds in 1:length(pol_teams))
  {

    pol_home_wins_rnds[i_pol_wins_rnds] <- nrow(POL_rounds[POL_rounds$Home == pol_teams[i_pol_wins_rnds] & POL_rounds$Res == "H" & POL_rounds$pol_matchday <= i_pol_krounds,])
    pol_away_wins_rnds[i_pol_wins_rnds] <- nrow(POL_rounds[POL_rounds$Away == pol_teams[i_pol_wins_rnds] & POL_rounds$Res == "A" & POL_rounds$pol_matchday <= i_pol_krounds,])
    pol_home_draws_rnds[i_pol_wins_rnds] <- nrow(POL_rounds[POL_rounds$Home == pol_teams[i_pol_wins_rnds] & POL_rounds$Res == "D" & POL_rounds$pol_matchday <= i_pol_krounds,])
    pol_away_draws_rnds[i_pol_wins_rnds] <- nrow(POL_rounds[POL_rounds$Away == pol_teams[i_pol_wins_rnds] & POL_rounds$Res == "D" & POL_rounds$pol_matchday <= i_pol_krounds,])
    pol_home_loss_rnds[i_pol_wins_rnds] <- nrow(POL_rounds[POL_rounds$Home == pol_teams[i_pol_wins_rnds] & POL_rounds$Res == "A" & POL_rounds$pol_matchday <= i_pol_krounds,])
    pol_away_loss_rnds[i_pol_wins_rnds] <- nrow(POL_rounds[POL_rounds$Away == pol_teams[i_pol_wins_rnds] & POL_rounds$Res == "H" & POL_rounds$pol_matchday <= i_pol_krounds,])

  }

  pol_total_wins_rnds <- pol_home_wins_rnds + pol_away_wins_rnds
  pol_total_draws_rnds <- pol_home_draws_rnds + pol_away_draws_rnds
  pol_total_loss_rnds <- pol_home_loss_rnds + pol_away_loss_rnds


  pol_home_games_rnds <- c()
  pol_away_games_rnds <-c()

  for (i_pol_rnds in 1:length(pol_teams))
  {

    pol_home_games_rnds[i_pol_rnds] <- nrow(POL_rounds[POL_rounds$Home == pol_teams[i_pol_rnds] & POL_rounds$pol_matchday <= i_pol_krounds,])
    pol_away_games_rnds[i_pol_rnds]  <- nrow(POL_rounds[POL_rounds$Away == pol_teams[i_pol_rnds] & POL_rounds$pol_matchday <= i_pol_krounds,])

  }

  pol_games_played_rnds <- pol_home_games_rnds + pol_away_games_rnds

  pol_league_table_rnds <- cbind(pol_teams,pol_games_played_rnds,pol_total_wins_rnds,pol_total_draws_rnds,pol_total_loss_rnds)

  # pol_GS <- pol_scoring$TGS
  # pol_GC <-pol_conceding$TGC
  # pol_GD <- pol_scoring$TGS - pol_conceding$TGC

  pol_PTS_rnds <- (pol_total_wins_rnds*3) + (pol_total_draws_rnds*1)
  pol_league_table_rnds <- cbind(pol_league_table_rnds,pol_PTS_rnds)
  pol_league_table_rnds <- as.data.frame(pol_league_table_rnds)
  #rename the columns
  names(pol_league_table_rnds)[names(pol_league_table_rnds) == "pol_teams"] <- "Team"
  names(pol_league_table_rnds)[names(pol_league_table_rnds) == "pol_games_played_rnds"] <- "P"
  names(pol_league_table_rnds)[names(pol_league_table_rnds) == "pol_total_wins_rnds"] <- "W"
  names(pol_league_table_rnds)[names(pol_league_table_rnds) == "pol_total_draws_rnds"] <- "D"
  names(pol_league_table_rnds)[names(pol_league_table_rnds) == "pol_total_loss_rnds"] <- "L"
  # names(pol_league_table)[names(pol_league_table) == "pol_GS"] <- "F"
  # names(pol_league_table)[names(pol_league_table) == "pol_GC"] <- "A"
  points_pol_rnds <- pol_league_table_rnds[order(as.numeric(pol_league_table_rnds$pol_PTS_rnds), decreasing = TRUE),]
  points_pol_rnds$pol_rank_rnds <- 1:length(pol_teams)
  row.names(points_pol_rnds) <- points_pol_rnds$pol_rank


  points_pol_rnds <- points_pol_rnds[order(as.character(points_pol_rnds$Team)),]


  pol_roundmatrix[,i_pol_krounds] <- as.data.frame(points_pol_rnds$pol_rank_rnds)


}

pol_roundmatrix <- cbind(pol_teams,pol_roundmatrix)
write.xlsx(pol_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "pol",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#rou
#hwins and away wins
rou_home_wins_rnds <- c()
rou_away_wins_rnds <- c()
rou_home_draws_rnds <- c()
rou_away_draws_rnds <- c()
rou_home_loss_rnds <- c()
rou_away_loss_rnds <- c()

#rou_krounds is total rounds as per current season
rou_krounds <- tail(unique(ROU_rounds$rou_matchday),1)
rou_roundmatrix <- data.frame(matrix(nrow = length(rou_teams),ncol = rou_krounds))

for(i_rou_krounds in 1:rou_krounds)
{

  for (i_rou_wins_rnds in 1:length(rou_teams))
  {

    rou_home_wins_rnds[i_rou_wins_rnds] <- nrow(ROU_rounds[ROU_rounds$Home == rou_teams[i_rou_wins_rnds] & ROU_rounds$Res == "H" & ROU_rounds$rou_matchday <= i_rou_krounds,])
    rou_away_wins_rnds[i_rou_wins_rnds] <- nrow(ROU_rounds[ROU_rounds$Away == rou_teams[i_rou_wins_rnds] & ROU_rounds$Res == "A" & ROU_rounds$rou_matchday <= i_rou_krounds,])
    rou_home_draws_rnds[i_rou_wins_rnds] <- nrow(ROU_rounds[ROU_rounds$Home == rou_teams[i_rou_wins_rnds] & ROU_rounds$Res == "D" & ROU_rounds$rou_matchday <= i_rou_krounds,])
    rou_away_draws_rnds[i_rou_wins_rnds] <- nrow(ROU_rounds[ROU_rounds$Away == rou_teams[i_rou_wins_rnds] & ROU_rounds$Res == "D" & ROU_rounds$rou_matchday <= i_rou_krounds,])
    rou_home_loss_rnds[i_rou_wins_rnds] <- nrow(ROU_rounds[ROU_rounds$Home == rou_teams[i_rou_wins_rnds] & ROU_rounds$Res == "A" & ROU_rounds$rou_matchday <= i_rou_krounds,])
    rou_away_loss_rnds[i_rou_wins_rnds] <- nrow(ROU_rounds[ROU_rounds$Away == rou_teams[i_rou_wins_rnds] & ROU_rounds$Res == "H" & ROU_rounds$rou_matchday <= i_rou_krounds,])

  }

  rou_total_wins_rnds <- rou_home_wins_rnds + rou_away_wins_rnds
  rou_total_draws_rnds <- rou_home_draws_rnds + rou_away_draws_rnds
  rou_total_loss_rnds <- rou_home_loss_rnds + rou_away_loss_rnds


  rou_home_games_rnds <- c()
  rou_away_games_rnds <-c()

  for (i_rou_rnds in 1:length(rou_teams))
  {

    rou_home_games_rnds[i_rou_rnds] <- nrow(ROU_rounds[ROU_rounds$Home == rou_teams[i_rou_rnds] & ROU_rounds$rou_matchday <= i_rou_krounds,])
    rou_away_games_rnds[i_rou_rnds]  <- nrow(ROU_rounds[ROU_rounds$Away == rou_teams[i_rou_rnds] & ROU_rounds$rou_matchday <= i_rou_krounds,])

  }

  rou_games_played_rnds <- rou_home_games_rnds + rou_away_games_rnds

  rou_league_table_rnds <- cbind(rou_teams,rou_games_played_rnds,rou_total_wins_rnds,rou_total_draws_rnds,rou_total_loss_rnds)

  # rou_GS <- rou_scoring$TGS
  # rou_GC <-rou_conceding$TGC
  # rou_GD <- rou_scoring$TGS - rou_conceding$TGC

  rou_PTS_rnds <- (rou_total_wins_rnds*3) + (rou_total_draws_rnds*1)
  rou_league_table_rnds <- cbind(rou_league_table_rnds,rou_PTS_rnds)
  rou_league_table_rnds <- as.data.frame(rou_league_table_rnds)
  #rename the columns
  names(rou_league_table_rnds)[names(rou_league_table_rnds) == "rou_teams"] <- "Team"
  names(rou_league_table_rnds)[names(rou_league_table_rnds) == "rou_games_played_rnds"] <- "P"
  names(rou_league_table_rnds)[names(rou_league_table_rnds) == "rou_total_wins_rnds"] <- "W"
  names(rou_league_table_rnds)[names(rou_league_table_rnds) == "rou_total_draws_rnds"] <- "D"
  names(rou_league_table_rnds)[names(rou_league_table_rnds) == "rou_total_loss_rnds"] <- "L"
  # names(rou_league_table)[names(rou_league_table) == "rou_GS"] <- "F"
  # names(rou_league_table)[names(rou_league_table) == "rou_GC"] <- "A"
  points_rou_rnds <- rou_league_table_rnds[order(as.numeric(rou_league_table_rnds$rou_PTS_rnds), decreasing = TRUE),]
  points_rou_rnds$rou_rank_rnds <- 1:length(rou_teams)
  row.names(points_rou_rnds) <- points_rou_rnds$rou_rank


  points_rou_rnds <- points_rou_rnds[order(as.character(points_rou_rnds$Team)),]


  rou_roundmatrix[,i_rou_krounds] <- as.data.frame(points_rou_rnds$rou_rank_rnds)


}

rou_roundmatrix <- cbind(rou_teams,rou_roundmatrix)
write.xlsx(rou_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "rou",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#rus
#hwins and away wins
rus_home_wins_rnds <- c()
rus_away_wins_rnds <- c()
rus_home_draws_rnds <- c()
rus_away_draws_rnds <- c()
rus_home_loss_rnds <- c()
rus_away_loss_rnds <- c()

#rus_krounds is total rounds as per current season
rus_krounds <- tail(unique(RUS_rounds$rus_matchday),1)
rus_roundmatrix <- data.frame(matrix(nrow = length(rus_teams),ncol = rus_krounds))

for(i_rus_krounds in 1:rus_krounds)
{

  for (i_rus_wins_rnds in 1:length(rus_teams))
  {

    rus_home_wins_rnds[i_rus_wins_rnds] <- nrow(RUS_rounds[RUS_rounds$Home == rus_teams[i_rus_wins_rnds] & RUS_rounds$Res == "H" & RUS_rounds$rus_matchday <= i_rus_krounds,])
    rus_away_wins_rnds[i_rus_wins_rnds] <- nrow(RUS_rounds[RUS_rounds$Away == rus_teams[i_rus_wins_rnds] & RUS_rounds$Res == "A" & RUS_rounds$rus_matchday <= i_rus_krounds,])
    rus_home_draws_rnds[i_rus_wins_rnds] <- nrow(RUS_rounds[RUS_rounds$Home == rus_teams[i_rus_wins_rnds] & RUS_rounds$Res == "D" & RUS_rounds$rus_matchday <= i_rus_krounds,])
    rus_away_draws_rnds[i_rus_wins_rnds] <- nrow(RUS_rounds[RUS_rounds$Away == rus_teams[i_rus_wins_rnds] & RUS_rounds$Res == "D" & RUS_rounds$rus_matchday <= i_rus_krounds,])
    rus_home_loss_rnds[i_rus_wins_rnds] <- nrow(RUS_rounds[RUS_rounds$Home == rus_teams[i_rus_wins_rnds] & RUS_rounds$Res == "A" & RUS_rounds$rus_matchday <= i_rus_krounds,])
    rus_away_loss_rnds[i_rus_wins_rnds] <- nrow(RUS_rounds[RUS_rounds$Away == rus_teams[i_rus_wins_rnds] & RUS_rounds$Res == "H" & RUS_rounds$rus_matchday <= i_rus_krounds,])

  }

  rus_total_wins_rnds <- rus_home_wins_rnds + rus_away_wins_rnds
  rus_total_draws_rnds <- rus_home_draws_rnds + rus_away_draws_rnds
  rus_total_loss_rnds <- rus_home_loss_rnds + rus_away_loss_rnds


  rus_home_games_rnds <- c()
  rus_away_games_rnds <-c()

  for (i_rus_rnds in 1:length(rus_teams))
  {

    rus_home_games_rnds[i_rus_rnds] <- nrow(RUS_rounds[RUS_rounds$Home == rus_teams[i_rus_rnds] & RUS_rounds$rus_matchday <= i_rus_krounds,])
    rus_away_games_rnds[i_rus_rnds]  <- nrow(RUS_rounds[RUS_rounds$Away == rus_teams[i_rus_rnds] & RUS_rounds$rus_matchday <= i_rus_krounds,])

  }

  rus_games_played_rnds <- rus_home_games_rnds + rus_away_games_rnds

  rus_league_table_rnds <- cbind(rus_teams,rus_games_played_rnds,rus_total_wins_rnds,rus_total_draws_rnds,rus_total_loss_rnds)

  # rus_GS <- rus_scoring$TGS
  # rus_GC <-rus_conceding$TGC
  # rus_GD <- rus_scoring$TGS - rus_conceding$TGC

  rus_PTS_rnds <- (rus_total_wins_rnds*3) + (rus_total_draws_rnds*1)
  rus_league_table_rnds <- cbind(rus_league_table_rnds,rus_PTS_rnds)
  rus_league_table_rnds <- as.data.frame(rus_league_table_rnds)
  #rename the columns
  names(rus_league_table_rnds)[names(rus_league_table_rnds) == "rus_teams"] <- "Team"
  names(rus_league_table_rnds)[names(rus_league_table_rnds) == "rus_games_played_rnds"] <- "P"
  names(rus_league_table_rnds)[names(rus_league_table_rnds) == "rus_total_wins_rnds"] <- "W"
  names(rus_league_table_rnds)[names(rus_league_table_rnds) == "rus_total_draws_rnds"] <- "D"
  names(rus_league_table_rnds)[names(rus_league_table_rnds) == "rus_total_loss_rnds"] <- "L"
  # names(rus_league_table)[names(rus_league_table) == "rus_GS"] <- "F"
  # names(rus_league_table)[names(rus_league_table) == "rus_GC"] <- "A"
  points_rus_rnds <- rus_league_table_rnds[order(as.numeric(rus_league_table_rnds$rus_PTS_rnds), decreasing = TRUE),]
  points_rus_rnds$rus_rank_rnds <- 1:length(rus_teams)
  row.names(points_rus_rnds) <- points_rus_rnds$rus_rank


  points_rus_rnds <- points_rus_rnds[order(as.character(points_rus_rnds$Team)),]


  rus_roundmatrix[,i_rus_krounds] <- as.data.frame(points_rus_rnds$rus_rank_rnds)


}

rus_roundmatrix <- cbind(rus_teams,rus_roundmatrix)
write.xlsx(rus_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "rus",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#swe
#hwins and away wins
swe_home_wins_rnds <- c()
swe_away_wins_rnds <- c()
swe_home_draws_rnds <- c()
swe_away_draws_rnds <- c()
swe_home_loss_rnds <- c()
swe_away_loss_rnds <- c()

#swe_krounds is total rounds as per current season
swe_krounds <- tail(unique(SWE_rounds$swe_matchday),1)
swe_roundmatrix <- data.frame(matrix(nrow = length(swe_teams),ncol = swe_krounds))

for(i_swe_krounds in 1:swe_krounds)
{

  for (i_swe_wins_rnds in 1:length(swe_teams))
  {

    swe_home_wins_rnds[i_swe_wins_rnds] <- nrow(SWE_rounds[SWE_rounds$Home == swe_teams[i_swe_wins_rnds] & SWE_rounds$Res == "H" & SWE_rounds$swe_matchday <= i_swe_krounds,])
    swe_away_wins_rnds[i_swe_wins_rnds] <- nrow(SWE_rounds[SWE_rounds$Away == swe_teams[i_swe_wins_rnds] & SWE_rounds$Res == "A" & SWE_rounds$swe_matchday <= i_swe_krounds,])
    swe_home_draws_rnds[i_swe_wins_rnds] <- nrow(SWE_rounds[SWE_rounds$Home == swe_teams[i_swe_wins_rnds] & SWE_rounds$Res == "D" & SWE_rounds$swe_matchday <= i_swe_krounds,])
    swe_away_draws_rnds[i_swe_wins_rnds] <- nrow(SWE_rounds[SWE_rounds$Away == swe_teams[i_swe_wins_rnds] & SWE_rounds$Res == "D" & SWE_rounds$swe_matchday <= i_swe_krounds,])
    swe_home_loss_rnds[i_swe_wins_rnds] <- nrow(SWE_rounds[SWE_rounds$Home == swe_teams[i_swe_wins_rnds] & SWE_rounds$Res == "A" & SWE_rounds$swe_matchday <= i_swe_krounds,])
    swe_away_loss_rnds[i_swe_wins_rnds] <- nrow(SWE_rounds[SWE_rounds$Away == swe_teams[i_swe_wins_rnds] & SWE_rounds$Res == "H" & SWE_rounds$swe_matchday <= i_swe_krounds,])

  }

  swe_total_wins_rnds <- swe_home_wins_rnds + swe_away_wins_rnds
  swe_total_draws_rnds <- swe_home_draws_rnds + swe_away_draws_rnds
  swe_total_loss_rnds <- swe_home_loss_rnds + swe_away_loss_rnds


  swe_home_games_rnds <- c()
  swe_away_games_rnds <-c()

  for (i_swe_rnds in 1:length(swe_teams))
  {

    swe_home_games_rnds[i_swe_rnds] <- nrow(SWE_rounds[SWE_rounds$Home == swe_teams[i_swe_rnds] & SWE_rounds$swe_matchday <= i_swe_krounds,])
    swe_away_games_rnds[i_swe_rnds]  <- nrow(SWE_rounds[SWE_rounds$Away == swe_teams[i_swe_rnds] & SWE_rounds$swe_matchday <= i_swe_krounds,])

  }

  swe_games_played_rnds <- swe_home_games_rnds + swe_away_games_rnds

  swe_league_table_rnds <- cbind(swe_teams,swe_games_played_rnds,swe_total_wins_rnds,swe_total_draws_rnds,swe_total_loss_rnds)

  # swe_GS <- swe_scoring$TGS
  # swe_GC <-swe_conceding$TGC
  # swe_GD <- swe_scoring$TGS - swe_conceding$TGC

  swe_PTS_rnds <- (swe_total_wins_rnds*3) + (swe_total_draws_rnds*1)
  swe_league_table_rnds <- cbind(swe_league_table_rnds,swe_PTS_rnds)
  swe_league_table_rnds <- as.data.frame(swe_league_table_rnds)
  #rename the columns
  names(swe_league_table_rnds)[names(swe_league_table_rnds) == "swe_teams"] <- "Team"
  names(swe_league_table_rnds)[names(swe_league_table_rnds) == "swe_games_played_rnds"] <- "P"
  names(swe_league_table_rnds)[names(swe_league_table_rnds) == "swe_total_wins_rnds"] <- "W"
  names(swe_league_table_rnds)[names(swe_league_table_rnds) == "swe_total_draws_rnds"] <- "D"
  names(swe_league_table_rnds)[names(swe_league_table_rnds) == "swe_total_loss_rnds"] <- "L"
  # names(swe_league_table)[names(swe_league_table) == "swe_GS"] <- "F"
  # names(swe_league_table)[names(swe_league_table) == "swe_GC"] <- "A"
  points_swe_rnds <- swe_league_table_rnds[order(as.numeric(swe_league_table_rnds$swe_PTS_rnds), decreasing = TRUE),]
  points_swe_rnds$swe_rank_rnds <- 1:length(swe_teams)
  row.names(points_swe_rnds) <- points_swe_rnds$swe_rank


  points_swe_rnds <- points_swe_rnds[order(as.character(points_swe_rnds$Team)),]


  swe_roundmatrix[,i_swe_krounds] <- as.data.frame(points_swe_rnds$swe_rank_rnds)


}

swe_roundmatrix <- cbind(swe_teams,swe_roundmatrix)
write.xlsx(swe_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "swe",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#swz
#hwins and away wins
swz_home_wins_rnds <- c()
swz_away_wins_rnds <- c()
swz_home_draws_rnds <- c()
swz_away_draws_rnds <- c()
swz_home_loss_rnds <- c()
swz_away_loss_rnds <- c()

#swz_krounds is total rounds as per current season
swz_krounds <- tail(unique(SWZ_rounds$swz_matchday),1)
swz_roundmatrix <- data.frame(matrix(nrow = length(swz_teams),ncol = swz_krounds))

for(i_swz_krounds in 1:swz_krounds)
{

  for (i_swz_wins_rnds in 1:length(swz_teams))
  {

    swz_home_wins_rnds[i_swz_wins_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Home == swz_teams[i_swz_wins_rnds] & SWZ_rounds$Res == "H" & SWZ_rounds$swz_matchday <= i_swz_krounds,])
    swz_away_wins_rnds[i_swz_wins_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Away == swz_teams[i_swz_wins_rnds] & SWZ_rounds$Res == "A" & SWZ_rounds$swz_matchday <= i_swz_krounds,])
    swz_home_draws_rnds[i_swz_wins_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Home == swz_teams[i_swz_wins_rnds] & SWZ_rounds$Res == "D" & SWZ_rounds$swz_matchday <= i_swz_krounds,])
    swz_away_draws_rnds[i_swz_wins_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Away == swz_teams[i_swz_wins_rnds] & SWZ_rounds$Res == "D" & SWZ_rounds$swz_matchday <= i_swz_krounds,])
    swz_home_loss_rnds[i_swz_wins_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Home == swz_teams[i_swz_wins_rnds] & SWZ_rounds$Res == "A" & SWZ_rounds$swz_matchday <= i_swz_krounds,])
    swz_away_loss_rnds[i_swz_wins_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Away == swz_teams[i_swz_wins_rnds] & SWZ_rounds$Res == "H" & SWZ_rounds$swz_matchday <= i_swz_krounds,])

  }

  swz_total_wins_rnds <- swz_home_wins_rnds + swz_away_wins_rnds
  swz_total_draws_rnds <- swz_home_draws_rnds + swz_away_draws_rnds
  swz_total_loss_rnds <- swz_home_loss_rnds + swz_away_loss_rnds


  swz_home_games_rnds <- c()
  swz_away_games_rnds <-c()

  for (i_swz_rnds in 1:length(swz_teams))
  {

    swz_home_games_rnds[i_swz_rnds] <- nrow(SWZ_rounds[SWZ_rounds$Home == swz_teams[i_swz_rnds] & SWZ_rounds$swz_matchday <= i_swz_krounds,])
    swz_away_games_rnds[i_swz_rnds]  <- nrow(SWZ_rounds[SWZ_rounds$Away == swz_teams[i_swz_rnds] & SWZ_rounds$swz_matchday <= i_swz_krounds,])

  }

  swz_games_played_rnds <- swz_home_games_rnds + swz_away_games_rnds

  swz_league_table_rnds <- cbind(swz_teams,swz_games_played_rnds,swz_total_wins_rnds,swz_total_draws_rnds,swz_total_loss_rnds)

  # swz_GS <- swz_scoring$TGS
  # swz_GC <-swz_conceding$TGC
  # swz_GD <- swz_scoring$TGS - swz_conceding$TGC

  swz_PTS_rnds <- (swz_total_wins_rnds*3) + (swz_total_draws_rnds*1)
  swz_league_table_rnds <- cbind(swz_league_table_rnds,swz_PTS_rnds)
  swz_league_table_rnds <- as.data.frame(swz_league_table_rnds)
  #rename the columns
  names(swz_league_table_rnds)[names(swz_league_table_rnds) == "swz_teams"] <- "Team"
  names(swz_league_table_rnds)[names(swz_league_table_rnds) == "swz_games_played_rnds"] <- "P"
  names(swz_league_table_rnds)[names(swz_league_table_rnds) == "swz_total_wins_rnds"] <- "W"
  names(swz_league_table_rnds)[names(swz_league_table_rnds) == "swz_total_draws_rnds"] <- "D"
  names(swz_league_table_rnds)[names(swz_league_table_rnds) == "swz_total_loss_rnds"] <- "L"
  # names(swz_league_table)[names(swz_league_table) == "swz_GS"] <- "F"
  # names(swz_league_table)[names(swz_league_table) == "swz_GC"] <- "A"
  points_swz_rnds <- swz_league_table_rnds[order(as.numeric(swz_league_table_rnds$swz_PTS_rnds), decreasing = TRUE),]
  points_swz_rnds$swz_rank_rnds <- 1:length(swz_teams)
  row.names(points_swz_rnds) <- points_swz_rnds$swz_rank


  points_swz_rnds <- points_swz_rnds[order(as.character(points_swz_rnds$Team)),]


  swz_roundmatrix[,i_swz_krounds] <- as.data.frame(points_swz_rnds$swz_rank_rnds)


}

swz_roundmatrix <- cbind(swz_teams,swz_roundmatrix)
write.xlsx(swz_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "swz",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#sp2
#hwins and away wins
sp2_home_wins_rnds <- c()
sp2_away_wins_rnds <- c()
sp2_home_draws_rnds <- c()
sp2_away_draws_rnds <- c()
sp2_home_loss_rnds <- c()
sp2_away_loss_rnds <- c()

#sp2_krounds is total rounds as per current season
sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
sp2_roundmatrix <- data.frame(matrix(nrow = length(sp2_teams),ncol = sp2_krounds))

for(i_sp2_krounds in 1:sp2_krounds)
{

  for (i_sp2_wins_rnds in 1:length(sp2_teams))
  {

    sp2_home_wins_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "H" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
    sp2_away_wins_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "A" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
    sp2_home_draws_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "D" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
    sp2_away_draws_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "D" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
    sp2_home_loss_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "A" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
    sp2_away_loss_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "H" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])

  }

  sp2_total_wins_rnds <- sp2_home_wins_rnds + sp2_away_wins_rnds
  sp2_total_draws_rnds <- sp2_home_draws_rnds + sp2_away_draws_rnds
  sp2_total_loss_rnds <- sp2_home_loss_rnds + sp2_away_loss_rnds


  sp2_home_games_rnds <- c()
  sp2_away_games_rnds <-c()

  for (i_sp2_rnds in 1:length(sp2_teams))
  {

    sp2_home_games_rnds[i_sp2_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_rnds] & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
    sp2_away_games_rnds[i_sp2_rnds]  <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_rnds] & SP2_rounds$sp2_matchday <= i_sp2_krounds,])

  }

  sp2_games_played_rnds <- sp2_home_games_rnds + sp2_away_games_rnds

  sp2_league_table_rnds <- cbind(sp2_teams,sp2_games_played_rnds,sp2_total_wins_rnds,sp2_total_draws_rnds,sp2_total_loss_rnds)

  # sp2_GS <- sp2_scoring$TGS
  # sp2_GC <-sp2_conceding$TGC
  # sp2_GD <- sp2_scoring$TGS - sp2_conceding$TGC

  sp2_PTS_rnds <- (sp2_total_wins_rnds*3) + (sp2_total_draws_rnds*1)
  sp2_league_table_rnds <- cbind(sp2_league_table_rnds,sp2_PTS_rnds)
  sp2_league_table_rnds <- as.data.frame(sp2_league_table_rnds)
  #rename the columns
  names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_teams"] <- "Team"
  names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_games_played_rnds"] <- "P"
  names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_total_wins_rnds"] <- "W"
  names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_total_draws_rnds"] <- "D"
  names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_total_loss_rnds"] <- "L"
  # names(sp2_league_table)[names(sp2_league_table) == "sp2_GS"] <- "F"
  # names(sp2_league_table)[names(sp2_league_table) == "sp2_GC"] <- "A"
  points_sp2_rnds <- sp2_league_table_rnds[order(as.numeric(sp2_league_table_rnds$sp2_PTS_rnds), decreasing = TRUE),]
  points_sp2_rnds$sp2_rank_rnds <- 1:length(sp2_teams)
  row.names(points_sp2_rnds) <- points_sp2_rnds$sp2_rank


  points_sp2_rnds <- points_sp2_rnds[order(as.character(points_sp2_rnds$Team)),]


  sp2_roundmatrix[,i_sp2_krounds] <- as.data.frame(points_sp2_rnds$sp2_rank_rnds)


}

sp2_roundmatrix <- cbind(sp2_teams,sp2_roundmatrix)
write.xlsx(sp2_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "sp2",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#sc0
#hwins and away wins
sc0_home_wins_rnds <- c()
sc0_away_wins_rnds <- c()
sc0_home_draws_rnds <- c()
sc0_away_draws_rnds <- c()
sc0_home_loss_rnds <- c()
sc0_away_loss_rnds <- c()

#sc0_krounds is total rounds as per current season
sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
sc0_roundmatrix <- data.frame(matrix(nrow = length(sc0_teams),ncol = sc0_krounds))

for(i_sc0_krounds in 1:sc0_krounds)
{

  for (i_sc0_wins_rnds in 1:length(sc0_teams))
  {

    sc0_home_wins_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "H" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
    sc0_away_wins_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "A" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
    sc0_home_draws_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "D" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
    sc0_away_draws_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "D" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
    sc0_home_loss_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "A" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
    sc0_away_loss_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "H" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])

  }

  sc0_total_wins_rnds <- sc0_home_wins_rnds + sc0_away_wins_rnds
  sc0_total_draws_rnds <- sc0_home_draws_rnds + sc0_away_draws_rnds
  sc0_total_loss_rnds <- sc0_home_loss_rnds + sc0_away_loss_rnds


  sc0_home_games_rnds <- c()
  sc0_away_games_rnds <-c()

  for (i_sc0_rnds in 1:length(sc0_teams))
  {

    sc0_home_games_rnds[i_sc0_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_rnds] & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
    sc0_away_games_rnds[i_sc0_rnds]  <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_rnds] & SC0_rounds$sc0_matchday <= i_sc0_krounds,])

  }

  sc0_games_played_rnds <- sc0_home_games_rnds + sc0_away_games_rnds

  sc0_league_table_rnds <- cbind(sc0_teams,sc0_games_played_rnds,sc0_total_wins_rnds,sc0_total_draws_rnds,sc0_total_loss_rnds)

  # sc0_GS <- sc0_scoring$TGS
  # sc0_GC <-sc0_conceding$TGC
  # sc0_GD <- sc0_scoring$TGS - sc0_conceding$TGC

  sc0_PTS_rnds <- (sc0_total_wins_rnds*3) + (sc0_total_draws_rnds*1)
  sc0_league_table_rnds <- cbind(sc0_league_table_rnds,sc0_PTS_rnds)
  sc0_league_table_rnds <- as.data.frame(sc0_league_table_rnds)
  #rename the columns
  names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_teams"] <- "Team"
  names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_games_played_rnds"] <- "P"
  names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_total_wins_rnds"] <- "W"
  names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_total_draws_rnds"] <- "D"
  names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_total_loss_rnds"] <- "L"
  # names(sc0_league_table)[names(sc0_league_table) == "sc0_GS"] <- "F"
  # names(sc0_league_table)[names(sc0_league_table) == "sc0_GC"] <- "A"
  points_sc0_rnds <- sc0_league_table_rnds[order(as.numeric(sc0_league_table_rnds$sc0_PTS_rnds), decreasing = TRUE),]
  points_sc0_rnds$sc0_rank_rnds <- 1:length(sc0_teams)
  row.names(points_sc0_rnds) <- points_sc0_rnds$sc0_rank


  points_sc0_rnds <- points_sc0_rnds[order(as.character(points_sc0_rnds$Team)),]


  sc0_roundmatrix[,i_sc0_krounds] <- as.data.frame(points_sc0_rnds$sc0_rank_rnds)


}

sc0_roundmatrix <- cbind(sc0_teams,sc0_roundmatrix)
write.xlsx(sc0_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "sc0",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#sc1
#hwins and away wins
sc1_home_wins_rnds <- c()
sc1_away_wins_rnds <- c()
sc1_home_draws_rnds <- c()
sc1_away_draws_rnds <- c()
sc1_home_loss_rnds <- c()
sc1_away_loss_rnds <- c()

#sc1_krounds is total rounds as per current season
sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
sc1_roundmatrix <- data.frame(matrix(nrow = length(sc1_teams),ncol = sc1_krounds))

for(i_sc1_krounds in 1:sc1_krounds)
{

  for (i_sc1_wins_rnds in 1:length(sc1_teams))
  {

    sc1_home_wins_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "H" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
    sc1_away_wins_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "A" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
    sc1_home_draws_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "D" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
    sc1_away_draws_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "D" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
    sc1_home_loss_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "A" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
    sc1_away_loss_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "H" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])

  }

  sc1_total_wins_rnds <- sc1_home_wins_rnds + sc1_away_wins_rnds
  sc1_total_draws_rnds <- sc1_home_draws_rnds + sc1_away_draws_rnds
  sc1_total_loss_rnds <- sc1_home_loss_rnds + sc1_away_loss_rnds


  sc1_home_games_rnds <- c()
  sc1_away_games_rnds <-c()

  for (i_sc1_rnds in 1:length(sc1_teams))
  {

    sc1_home_games_rnds[i_sc1_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_rnds] & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
    sc1_away_games_rnds[i_sc1_rnds]  <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_rnds] & SC1_rounds$sc1_matchday <= i_sc1_krounds,])

  }

  sc1_games_played_rnds <- sc1_home_games_rnds + sc1_away_games_rnds

  sc1_league_table_rnds <- cbind(sc1_teams,sc1_games_played_rnds,sc1_total_wins_rnds,sc1_total_draws_rnds,sc1_total_loss_rnds)

  # sc1_GS <- sc1_scoring$TGS
  # sc1_GC <-sc1_conceding$TGC
  # sc1_GD <- sc1_scoring$TGS - sc1_conceding$TGC

  sc1_PTS_rnds <- (sc1_total_wins_rnds*3) + (sc1_total_draws_rnds*1)
  sc1_league_table_rnds <- cbind(sc1_league_table_rnds,sc1_PTS_rnds)
  sc1_league_table_rnds <- as.data.frame(sc1_league_table_rnds)
  #rename the columns
  names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_teams"] <- "Team"
  names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_games_played_rnds"] <- "P"
  names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_total_wins_rnds"] <- "W"
  names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_total_draws_rnds"] <- "D"
  names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_total_loss_rnds"] <- "L"
  # names(sc1_league_table)[names(sc1_league_table) == "sc1_GS"] <- "F"
  # names(sc1_league_table)[names(sc1_league_table) == "sc1_GC"] <- "A"
  points_sc1_rnds <- sc1_league_table_rnds[order(as.numeric(sc1_league_table_rnds$sc1_PTS_rnds), decreasing = TRUE),]
  points_sc1_rnds$sc1_rank_rnds <- 1:length(sc1_teams)
  row.names(points_sc1_rnds) <- points_sc1_rnds$sc1_rank


  points_sc1_rnds <- points_sc1_rnds[order(as.character(points_sc1_rnds$Team)),]


  sc1_roundmatrix[,i_sc1_krounds] <- as.data.frame(points_sc1_rnds$sc1_rank_rnds)


}

sc1_roundmatrix <- cbind(sc1_teams,sc1_roundmatrix)
write.xlsx(sc1_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "sc1",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#sc2
#hwins and away wins
sc2_home_wins_rnds <- c()
sc2_away_wins_rnds <- c()
sc2_home_draws_rnds <- c()
sc2_away_draws_rnds <- c()
sc2_home_loss_rnds <- c()
sc2_away_loss_rnds <- c()

#sc2_krounds is total rounds as per current season
sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
sc2_roundmatrix <- data.frame(matrix(nrow = length(sc2_teams),ncol = sc2_krounds))

for(i_sc2_krounds in 1:sc2_krounds)
{

  for (i_sc2_wins_rnds in 1:length(sc2_teams))
  {

    sc2_home_wins_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "H" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
    sc2_away_wins_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "A" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
    sc2_home_draws_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "D" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
    sc2_away_draws_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "D" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
    sc2_home_loss_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "A" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
    sc2_away_loss_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "H" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])

  }

  sc2_total_wins_rnds <- sc2_home_wins_rnds + sc2_away_wins_rnds
  sc2_total_draws_rnds <- sc2_home_draws_rnds + sc2_away_draws_rnds
  sc2_total_loss_rnds <- sc2_home_loss_rnds + sc2_away_loss_rnds


  sc2_home_games_rnds <- c()
  sc2_away_games_rnds <-c()

  for (i_sc2_rnds in 1:length(sc2_teams))
  {

    sc2_home_games_rnds[i_sc2_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_rnds] & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
    sc2_away_games_rnds[i_sc2_rnds]  <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_rnds] & SC2_rounds$sc2_matchday <= i_sc2_krounds,])

  }

  sc2_games_played_rnds <- sc2_home_games_rnds + sc2_away_games_rnds

  sc2_league_table_rnds <- cbind(sc2_teams,sc2_games_played_rnds,sc2_total_wins_rnds,sc2_total_draws_rnds,sc2_total_loss_rnds)

  # sc2_GS <- sc2_scoring$TGS
  # sc2_GC <-sc2_conceding$TGC
  # sc2_GD <- sc2_scoring$TGS - sc2_conceding$TGC

  sc2_PTS_rnds <- (sc2_total_wins_rnds*3) + (sc2_total_draws_rnds*1)
  sc2_league_table_rnds <- cbind(sc2_league_table_rnds,sc2_PTS_rnds)
  sc2_league_table_rnds <- as.data.frame(sc2_league_table_rnds)
  #rename the columns
  names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_teams"] <- "Team"
  names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_games_played_rnds"] <- "P"
  names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_total_wins_rnds"] <- "W"
  names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_total_draws_rnds"] <- "D"
  names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_total_loss_rnds"] <- "L"
  # names(sc2_league_table)[names(sc2_league_table) == "sc2_GS"] <- "F"
  # names(sc2_league_table)[names(sc2_league_table) == "sc2_GC"] <- "A"
  points_sc2_rnds <- sc2_league_table_rnds[order(as.numeric(sc2_league_table_rnds$sc2_PTS_rnds), decreasing = TRUE),]
  points_sc2_rnds$sc2_rank_rnds <- 1:length(sc2_teams)
  row.names(points_sc2_rnds) <- points_sc2_rnds$sc2_rank


  points_sc2_rnds <- points_sc2_rnds[order(as.character(points_sc2_rnds$Team)),]


  sc2_roundmatrix[,i_sc2_krounds] <- as.data.frame(points_sc2_rnds$sc2_rank_rnds)


}

sc2_roundmatrix <- cbind(sc2_teams,sc2_roundmatrix)
write.xlsx(sc2_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "sc2",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#sc3
#hwins and away wins
sc3_home_wins_rnds <- c()
sc3_away_wins_rnds <- c()
sc3_home_draws_rnds <- c()
sc3_away_draws_rnds <- c()
sc3_home_loss_rnds <- c()
sc3_away_loss_rnds <- c()

#sc3_krounds is total rounds as per current season
sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
sc3_roundmatrix <- data.frame(matrix(nrow = length(sc3_teams),ncol = sc3_krounds))

for(i_sc3_krounds in 1:sc3_krounds)
{

  for (i_sc3_wins_rnds in 1:length(sc3_teams))
  {

    sc3_home_wins_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "H" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
    sc3_away_wins_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "A" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
    sc3_home_draws_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "D" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
    sc3_away_draws_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "D" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
    sc3_home_loss_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "A" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
    sc3_away_loss_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "H" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])

  }

  sc3_total_wins_rnds <- sc3_home_wins_rnds + sc3_away_wins_rnds
  sc3_total_draws_rnds <- sc3_home_draws_rnds + sc3_away_draws_rnds
  sc3_total_loss_rnds <- sc3_home_loss_rnds + sc3_away_loss_rnds


  sc3_home_games_rnds <- c()
  sc3_away_games_rnds <-c()

  for (i_sc3_rnds in 1:length(sc3_teams))
  {

    sc3_home_games_rnds[i_sc3_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_rnds] & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
    sc3_away_games_rnds[i_sc3_rnds]  <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_rnds] & SC3_rounds$sc3_matchday <= i_sc3_krounds,])

  }

  sc3_games_played_rnds <- sc3_home_games_rnds + sc3_away_games_rnds

  sc3_league_table_rnds <- cbind(sc3_teams,sc3_games_played_rnds,sc3_total_wins_rnds,sc3_total_draws_rnds,sc3_total_loss_rnds)

  # sc3_GS <- sc3_scoring$TGS
  # sc3_GC <-sc3_conceding$TGC
  # sc3_GD <- sc3_scoring$TGS - sc3_conceding$TGC

  sc3_PTS_rnds <- (sc3_total_wins_rnds*3) + (sc3_total_draws_rnds*1)
  sc3_league_table_rnds <- cbind(sc3_league_table_rnds,sc3_PTS_rnds)
  sc3_league_table_rnds <- as.data.frame(sc3_league_table_rnds)
  #rename the columns
  names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_teams"] <- "Team"
  names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_games_played_rnds"] <- "P"
  names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_total_wins_rnds"] <- "W"
  names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_total_draws_rnds"] <- "D"
  names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_total_loss_rnds"] <- "L"
  # names(sc3_league_table)[names(sc3_league_table) == "sc3_GS"] <- "F"
  # names(sc3_league_table)[names(sc3_league_table) == "sc3_GC"] <- "A"
  points_sc3_rnds <- sc3_league_table_rnds[order(as.numeric(sc3_league_table_rnds$sc3_PTS_rnds), decreasing = TRUE),]
  points_sc3_rnds$sc3_rank_rnds <- 1:length(sc3_teams)
  row.names(points_sc3_rnds) <- points_sc3_rnds$sc3_rank


  points_sc3_rnds <- points_sc3_rnds[order(as.character(points_sc3_rnds$Team)),]


  sc3_roundmatrix[,i_sc3_krounds] <- as.data.frame(points_sc3_rnds$sc3_rank_rnds)


}

sc3_roundmatrix <- cbind(sc3_teams,sc3_roundmatrix)
write.xlsx(sc3_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "sc3",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#t1
#hwins and away wins
t1_home_wins_rnds <- c()
t1_away_wins_rnds <- c()
t1_home_draws_rnds <- c()
t1_away_draws_rnds <- c()
t1_home_loss_rnds <- c()
t1_away_loss_rnds <- c()

#t1_krounds is total rounds as per current season
t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
t1_roundmatrix <- data.frame(matrix(nrow = length(t1_teams),ncol = t1_krounds))

for(i_t1_krounds in 1:t1_krounds)
{

  for (i_t1_wins_rnds in 1:length(t1_teams))
  {

    t1_home_wins_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "H" & T1_rounds$t1_matchday <= i_t1_krounds,])
    t1_away_wins_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "A" & T1_rounds$t1_matchday <= i_t1_krounds,])
    t1_home_draws_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "D" & T1_rounds$t1_matchday <= i_t1_krounds,])
    t1_away_draws_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "D" & T1_rounds$t1_matchday <= i_t1_krounds,])
    t1_home_loss_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "A" & T1_rounds$t1_matchday <= i_t1_krounds,])
    t1_away_loss_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "H" & T1_rounds$t1_matchday <= i_t1_krounds,])

  }

  t1_total_wins_rnds <- t1_home_wins_rnds + t1_away_wins_rnds
  t1_total_draws_rnds <- t1_home_draws_rnds + t1_away_draws_rnds
  t1_total_loss_rnds <- t1_home_loss_rnds + t1_away_loss_rnds


  t1_home_games_rnds <- c()
  t1_away_games_rnds <-c()

  for (i_t1_rnds in 1:length(t1_teams))
  {

    t1_home_games_rnds[i_t1_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_rnds] & T1_rounds$t1_matchday <= i_t1_krounds,])
    t1_away_games_rnds[i_t1_rnds]  <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_rnds] & T1_rounds$t1_matchday <= i_t1_krounds,])

  }

  t1_games_played_rnds <- t1_home_games_rnds + t1_away_games_rnds

  t1_league_table_rnds <- cbind(t1_teams,t1_games_played_rnds,t1_total_wins_rnds,t1_total_draws_rnds,t1_total_loss_rnds)

  # t1_GS <- t1_scoring$TGS
  # t1_GC <-t1_conceding$TGC
  # t1_GD <- t1_scoring$TGS - t1_conceding$TGC

  t1_PTS_rnds <- (t1_total_wins_rnds*3) + (t1_total_draws_rnds*1)
  t1_league_table_rnds <- cbind(t1_league_table_rnds,t1_PTS_rnds)
  t1_league_table_rnds <- as.data.frame(t1_league_table_rnds)
  #rename the columns
  names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_teams"] <- "Team"
  names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_games_played_rnds"] <- "P"
  names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_total_wins_rnds"] <- "W"
  names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_total_draws_rnds"] <- "D"
  names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_total_loss_rnds"] <- "L"
  # names(t1_league_table)[names(t1_league_table) == "t1_GS"] <- "F"
  # names(t1_league_table)[names(t1_league_table) == "t1_GC"] <- "A"
  points_t1_rnds <- t1_league_table_rnds[order(as.numeric(t1_league_table_rnds$t1_PTS_rnds), decreasing = TRUE),]
  points_t1_rnds$t1_rank_rnds <- 1:length(t1_teams)
  row.names(points_t1_rnds) <- points_t1_rnds$t1_rank


  points_t1_rnds <- points_t1_rnds[order(as.character(points_t1_rnds$Team)),]


  t1_roundmatrix[,i_t1_krounds] <- as.data.frame(points_t1_rnds$t1_rank_rnds)


}

t1_roundmatrix <- cbind(t1_teams,t1_roundmatrix)
write.xlsx(t1_roundmatrix,'NL/Roundmatrix.xlsx',sheetName = "t1",append = TRUE)
#############################################################################################################################################

































