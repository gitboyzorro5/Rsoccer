Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')


#####################################################################################################################
#begin gamesate algortihm
#AUT
AUT_margindata <- AUT
aut_firstgames <- length(aut_teams)/2
AUT_firstsplit <-  head(AUT_margindata,aut_firstgames)
AUT_secondsplit <- tail(AUT_margindata,nrow(AUT_margindata) - aut_firstgames)
AUT_secondsplit$aut_HomeTeam_index_wm <- match(AUT_secondsplit$Home,aut_teams)
AUT_secondsplit$aut_AwayTeam_index_wm <- match(AUT_secondsplit$Away,aut_teams)
AUT_secondsplit$aut_homegame_no <- rep(2:max(aut_games_played[]) - 1,each = length(aut_teams)/2,length.out = nrow(AUT_secondsplit))
AUT_secondsplit$aut_awaygame_no <- rep(2:max(aut_games_played[]) - 1,each = length(aut_teams)/2,length.out = nrow(AUT_secondsplit))

aut_homewinmargin <- c()
aut_awaywinmargin <- c()

for (aut_secondsplitrow in 1:nrow(AUT_secondsplit))
{

  aut_hometeamindex_wm <- AUT_secondsplit[aut_secondsplitrow,"aut_HomeTeam_index_wm"]
  aut_awayteamindex_wm <- AUT_secondsplit[aut_secondsplitrow,"aut_AwayTeam_index_wm"]
  aut_hometeam_game_no <- AUT_secondsplit[aut_secondsplitrow,"aut_homegame_no"]
  aut_awayteam_game_no <- AUT_secondsplit[aut_secondsplitrow,"aut_awaygame_no"]

  aut_winmargin_vec_gamestate_h <- as.vector(aut_winmargin_h[aut_hometeamindex_wm,])
  aut_winmargin_vec_gamestate_h[is.na(aut_winmargin_vec_gamestate_h)] <- ""
  aut_winmargin_vec_gamestate_h <- aut_winmargin_vec_gamestate_h[aut_winmargin_vec_gamestate_h != ""]
  aut_winmargin_vec_gamestate_h <- as.numeric(aut_winmargin_vec_gamestate_h)

  aut_winmargin_vec_gamestate_a <- as.vector(aut_winmargin_h[aut_awayteamindex_wm,])
  aut_winmargin_vec_gamestate_a[is.na(aut_winmargin_vec_gamestate_a)] <- ""
  aut_winmargin_vec_gamestate_a <- aut_winmargin_vec_gamestate_a[aut_winmargin_vec_gamestate_a != ""]
  aut_winmargin_vec_gamestate_a <- as.numeric(aut_winmargin_vec_gamestate_a)

  for (aut_game_no in 1:aut_games_played[1])

  {

    aut_homewinmargin[aut_secondsplitrow] <- aut_winmargin_vec_gamestate_h[aut_hometeam_game_no]
    aut_awaywinmargin[aut_secondsplitrow] <- aut_winmargin_vec_gamestate_a[aut_awayteam_game_no]


  }

}

aut_homewinmargin
aut_awaywinmargin

AUT_gamestate_data <- cbind(AUT_secondsplit,aut_homewinmargin,aut_awaywinmargin)
AUT_gamestate_data
AUT_gamestate_data$aut_GSCS <- paste(AUT_gamestate_data$aut_homewinmargin,AUT_gamestate_data$aut_awaywinmargin, sep = ",")
AUT_gamestate_data$aut_GSH <- AUT_gamestate_data$aut_homewinmargin - AUT_gamestate_data$aut_awaywinmargin
head(AUT_gamestate_data,5)
AUT_gamestate_copy <- AUT_gamestate_data
AUT_gamestate_copy$aut_HomeTeam_index_wm <- NULL
AUT_gamestate_copy$aut_AwayTeam_index_wm <- NULL
AUT_gamestate_copy$aut_homegame_no <- NULL
AUT_gamestate_copy$aut_awaygame_no <- NULL
head(AUT_gamestate_copy)
colnames(AUT_gamestate_copy)[22] <- "homewinmargin"
colnames(AUT_gamestate_copy)[23] <- "awaywinmargin"
colnames(AUT_gamestate_copy)[24] <- "GSCS"
colnames(AUT_gamestate_copy)[25] <- "GSH"
AUT_gamestate_copy$CS <- paste(AUT_gamestate_copy$HG,AUT_gamestate_copy$AG, sep = "-")
AUT_gamestate_copy
#end AUT
###################################################################################################
###################################################################################################
#ARG
#ARG
ARG_margindata <- ARG
arg_firstgames <- length(arg_teams)/2
ARG_firstsplit <-  head(ARG_margindata,arg_firstgames)
ARG_secondsplit <- tail(ARG_margindata,nrow(ARG_margindata) - arg_firstgames)
ARG_secondsplit$arg_HomeTeam_index_wm <- match(ARG_secondsplit$Home,arg_teams)
ARG_secondsplit$arg_AwayTeam_index_wm <- match(ARG_secondsplit$Away,arg_teams)
ARG_secondsplit$arg_homegame_no <- rep(2:max(arg_games_played[]) - 1,each = length(arg_teams)/2,length.out = nrow(ARG_secondsplit))
ARG_secondsplit$arg_awaygame_no <- rep(2:max(arg_games_played[]) - 1,each = length(arg_teams)/2,length.out = nrow(ARG_secondsplit))

arg_homewinmargin <- c()
arg_awaywinmargin <- c()

for (arg_secondsplitrow in 1:nrow(ARG_secondsplit))
{

  arg_hometeamindex_wm <- ARG_secondsplit[arg_secondsplitrow,"arg_HomeTeam_index_wm"]
  arg_awayteamindex_wm <- ARG_secondsplit[arg_secondsplitrow,"arg_AwayTeam_index_wm"]
  arg_hometeam_game_no <- ARG_secondsplit[arg_secondsplitrow,"arg_homegame_no"]
  arg_awayteam_game_no <- ARG_secondsplit[arg_secondsplitrow,"arg_awaygame_no"]

  arg_winmargin_vec_gamestate_h <- as.vector(arg_winmargin_h[arg_hometeamindex_wm,])
  arg_winmargin_vec_gamestate_h[is.na(arg_winmargin_vec_gamestate_h)] <- ""
  arg_winmargin_vec_gamestate_h <- arg_winmargin_vec_gamestate_h[arg_winmargin_vec_gamestate_h != ""]
  arg_winmargin_vec_gamestate_h <- as.numeric(arg_winmargin_vec_gamestate_h)

  arg_winmargin_vec_gamestate_a <- as.vector(arg_winmargin_h[arg_awayteamindex_wm,])
  arg_winmargin_vec_gamestate_a[is.na(arg_winmargin_vec_gamestate_a)] <- ""
  arg_winmargin_vec_gamestate_a <- arg_winmargin_vec_gamestate_a[arg_winmargin_vec_gamestate_a != ""]
  arg_winmargin_vec_gamestate_a <- as.numeric(arg_winmargin_vec_gamestate_a)

  for (arg_game_no in 1:arg_games_played[1])

  {

    arg_homewinmargin[arg_secondsplitrow] <- arg_winmargin_vec_gamestate_h[arg_hometeam_game_no]
    arg_awaywinmargin[arg_secondsplitrow] <- arg_winmargin_vec_gamestate_a[arg_awayteam_game_no]


  }

}

arg_homewinmargin
arg_awaywinmargin

ARG_gamestate_data <- cbind(ARG_secondsplit,arg_homewinmargin,arg_awaywinmargin)
ARG_gamestate_data
ARG_gamestate_data$arg_GSCS <- paste(ARG_gamestate_data$arg_homewinmargin,ARG_gamestate_data$arg_awaywinmargin, sep = ",")
ARG_gamestate_data$arg_GSH <- ARG_gamestate_data$arg_homewinmargin - ARG_gamestate_data$arg_awaywinmargin
head(ARG_gamestate_data,5)
ARG_gamestate_copy <- ARG_gamestate_data
ARG_gamestate_copy$arg_HomeTeam_index_wm <- NULL
ARG_gamestate_copy$arg_AwayTeam_index_wm <- NULL
ARG_gamestate_copy$arg_homegame_no <- NULL
ARG_gamestate_copy$arg_awaygame_no <- NULL
head(ARG_gamestate_copy)
colnames(ARG_gamestate_copy)[22] <- "homewinmargin"
colnames(ARG_gamestate_copy)[23] <- "awaywinmargin"
colnames(ARG_gamestate_copy)[24] <- "GSCS"
colnames(ARG_gamestate_copy)[25] <- "GSH"
ARG_gamestate_copy$CS <- paste(ARG_gamestate_copy$HG,ARG_gamestate_copy$AG, sep = "-")
ARG_gamestate_copy
#end ARG
#end ARG
###################################################################################################
###################################################################################################
#BRA
BRA_margindata <- BRA
bra_firstgames <- length(bra_teams)/2
BRA_firstsplit <-  head(BRA_margindata,bra_firstgames)
BRA_secondsplit <- tail(BRA_margindata,nrow(BRA_margindata) - bra_firstgames)
BRA_secondsplit$bra_HomeTeam_index_wm <- match(BRA_secondsplit$Home,bra_teams)
BRA_secondsplit$bra_AwayTeam_index_wm <- match(BRA_secondsplit$Away,bra_teams)
BRA_secondsplit$bra_homegame_no <- rep(2:max(bra_games_played[]) - 1,each = length(bra_teams)/2,length.out = nrow(BRA_secondsplit))
BRA_secondsplit$bra_awaygame_no <- rep(2:max(bra_games_played[]) - 1,each = length(bra_teams)/2,length.out = nrow(BRA_secondsplit))

bra_homewinmargin <- c()
bra_awaywinmargin <- c()

for (bra_secondsplitrow in 1:nrow(BRA_secondsplit))
{

  bra_hometeamindex_wm <- BRA_secondsplit[bra_secondsplitrow,"bra_HomeTeam_index_wm"]
  bra_awayteamindex_wm <- BRA_secondsplit[bra_secondsplitrow,"bra_AwayTeam_index_wm"]
  bra_hometeam_game_no <- BRA_secondsplit[bra_secondsplitrow,"bra_homegame_no"]
  bra_awayteam_game_no <- BRA_secondsplit[bra_secondsplitrow,"bra_awaygame_no"]

  bra_winmargin_vec_gamestate_h <- as.vector(bra_winmargin_h[bra_hometeamindex_wm,])
  bra_winmargin_vec_gamestate_h[is.na(bra_winmargin_vec_gamestate_h)] <- ""
  bra_winmargin_vec_gamestate_h <- bra_winmargin_vec_gamestate_h[bra_winmargin_vec_gamestate_h != ""]
  bra_winmargin_vec_gamestate_h <- as.numeric(bra_winmargin_vec_gamestate_h)

  bra_winmargin_vec_gamestate_a <- as.vector(bra_winmargin_h[bra_awayteamindex_wm,])
  bra_winmargin_vec_gamestate_a[is.na(bra_winmargin_vec_gamestate_a)] <- ""
  bra_winmargin_vec_gamestate_a <- bra_winmargin_vec_gamestate_a[bra_winmargin_vec_gamestate_a != ""]
  bra_winmargin_vec_gamestate_a <- as.numeric(bra_winmargin_vec_gamestate_a)

  for (bra_game_no in 1:bra_games_played[1])

  {

    bra_homewinmargin[bra_secondsplitrow] <- bra_winmargin_vec_gamestate_h[bra_hometeam_game_no]
    bra_awaywinmargin[bra_secondsplitrow] <- bra_winmargin_vec_gamestate_a[bra_awayteam_game_no]


  }

}

bra_homewinmargin
bra_awaywinmargin

BRA_gamestate_data <- cbind(BRA_secondsplit,bra_homewinmargin,bra_awaywinmargin)
BRA_gamestate_data
BRA_gamestate_data$bra_GSCS <- paste(BRA_gamestate_data$bra_homewinmargin,BRA_gamestate_data$bra_awaywinmargin, sep = ",")
BRA_gamestate_data$bra_GSH <- BRA_gamestate_data$bra_homewinmargin - BRA_gamestate_data$bra_awaywinmargin
head(BRA_gamestate_data,5)
BRA_gamestate_copy <- BRA_gamestate_data
BRA_gamestate_copy$bra_HomeTeam_index_wm <- NULL
BRA_gamestate_copy$bra_AwayTeam_index_wm <- NULL
BRA_gamestate_copy$bra_homegame_no <- NULL
BRA_gamestate_copy$bra_awaygame_no <- NULL
head(BRA_gamestate_copy)
colnames(BRA_gamestate_copy)[22] <- "homewinmargin"
colnames(BRA_gamestate_copy)[23] <- "awaywinmargin"
colnames(BRA_gamestate_copy)[24] <- "GSCS"
colnames(BRA_gamestate_copy)[25] <- "GSH"
BRA_gamestate_copy$CS <- paste(BRA_gamestate_copy$HG,BRA_gamestate_copy$AG, sep = "-")
BRA_gamestate_copy
#end BRA
###################################################################################################
#CHN
CHN_margindata <- CHN
chn_firstgames <- length(chn_teams)/2
CHN_firstsplit <-  head(CHN_margindata,chn_firstgames)
CHN_secondsplit <- tail(CHN_margindata,nrow(CHN_margindata) - chn_firstgames)
CHN_secondsplit$chn_HomeTeam_index_wm <- match(CHN_secondsplit$Home,chn_teams)
CHN_secondsplit$chn_AwayTeam_index_wm <- match(CHN_secondsplit$Away,chn_teams)
CHN_secondsplit$chn_homegame_no <- rep(2:max(chn_games_played[]) - 1,each = length(chn_teams)/2,length.out = nrow(CHN_secondsplit))
CHN_secondsplit$chn_awaygame_no <- rep(2:max(chn_games_played[]) - 1,each = length(chn_teams)/2,length.out = nrow(CHN_secondsplit))

chn_homewinmargin <- c()
chn_awaywinmargin <- c()

for (chn_secondsplitrow in 1:nrow(CHN_secondsplit))
{

  chn_hometeamindex_wm <- CHN_secondsplit[chn_secondsplitrow,"chn_HomeTeam_index_wm"]
  chn_awayteamindex_wm <- CHN_secondsplit[chn_secondsplitrow,"chn_AwayTeam_index_wm"]
  chn_hometeam_game_no <- CHN_secondsplit[chn_secondsplitrow,"chn_homegame_no"]
  chn_awayteam_game_no <- CHN_secondsplit[chn_secondsplitrow,"chn_awaygame_no"]

  chn_winmargin_vec_gamestate_h <- as.vector(chn_winmargin_h[chn_hometeamindex_wm,])
  chn_winmargin_vec_gamestate_h[is.na(chn_winmargin_vec_gamestate_h)] <- ""
  chn_winmargin_vec_gamestate_h <- chn_winmargin_vec_gamestate_h[chn_winmargin_vec_gamestate_h != ""]
  chn_winmargin_vec_gamestate_h <- as.numeric(chn_winmargin_vec_gamestate_h)

  chn_winmargin_vec_gamestate_a <- as.vector(chn_winmargin_h[chn_awayteamindex_wm,])
  chn_winmargin_vec_gamestate_a[is.na(chn_winmargin_vec_gamestate_a)] <- ""
  chn_winmargin_vec_gamestate_a <- chn_winmargin_vec_gamestate_a[chn_winmargin_vec_gamestate_a != ""]
  chn_winmargin_vec_gamestate_a <- as.numeric(chn_winmargin_vec_gamestate_a)

  for (chn_game_no in 1:chn_games_played[1])

  {

    chn_homewinmargin[chn_secondsplitrow] <- chn_winmargin_vec_gamestate_h[chn_hometeam_game_no]
    chn_awaywinmargin[chn_secondsplitrow] <- chn_winmargin_vec_gamestate_a[chn_awayteam_game_no]


  }

}

chn_homewinmargin
chn_awaywinmargin

CHN_gamestate_data <- cbind(CHN_secondsplit,chn_homewinmargin,chn_awaywinmargin)
CHN_gamestate_data
CHN_gamestate_data$chn_GSCS <- paste(CHN_gamestate_data$chn_homewinmargin,CHN_gamestate_data$chn_awaywinmargin, sep = ",")
CHN_gamestate_data$chn_GSH <- CHN_gamestate_data$chn_homewinmargin - CHN_gamestate_data$chn_awaywinmargin
head(CHN_gamestate_data,5)
CHN_gamestate_copy <- CHN_gamestate_data
CHN_gamestate_copy$chn_HomeTeam_index_wm <- NULL
CHN_gamestate_copy$chn_AwayTeam_index_wm <- NULL
CHN_gamestate_copy$chn_homegame_no <- NULL
CHN_gamestate_copy$chn_awaygame_no <- NULL
head(CHN_gamestate_copy)
colnames(CHN_gamestate_copy)[22] <- "homewinmargin"
colnames(CHN_gamestate_copy)[23] <- "awaywinmargin"
colnames(CHN_gamestate_copy)[24] <- "GSCS"
colnames(CHN_gamestate_copy)[25] <- "GSH"
CHN_gamestate_copy$CS <- paste(CHN_gamestate_copy$HG,CHN_gamestate_copy$AG, sep = "-")
CHN_gamestate_copy
#end CHN
####################################################################################################################
####################################################################################################################
#DNK
DNK_margindata <- DNK
dnk_firstgames <- length(dnk_teams)/2
DNK_firstsplit <-  head(DNK_margindata,dnk_firstgames)
DNK_secondsplit <- tail(DNK_margindata,nrow(DNK_margindata) - dnk_firstgames)
DNK_secondsplit$dnk_HomeTeam_index_wm <- match(DNK_secondsplit$Home,dnk_teams)
DNK_secondsplit$dnk_AwayTeam_index_wm <- match(DNK_secondsplit$Away,dnk_teams)
DNK_secondsplit$dnk_homegame_no <- rep(2:max(dnk_games_played[]) - 1,each = length(dnk_teams)/2,length.out = nrow(DNK_secondsplit))
DNK_secondsplit$dnk_awaygame_no <- rep(2:max(dnk_games_played[]) - 1,each = length(dnk_teams)/2,length.out = nrow(DNK_secondsplit))

dnk_homewinmargin <- c()
dnk_awaywinmargin <- c()

for (dnk_secondsplitrow in 1:nrow(DNK_secondsplit))
{

  dnk_hometeamindex_wm <- DNK_secondsplit[dnk_secondsplitrow,"dnk_HomeTeam_index_wm"]
  dnk_awayteamindex_wm <- DNK_secondsplit[dnk_secondsplitrow,"dnk_AwayTeam_index_wm"]
  dnk_hometeam_game_no <- DNK_secondsplit[dnk_secondsplitrow,"dnk_homegame_no"]
  dnk_awayteam_game_no <- DNK_secondsplit[dnk_secondsplitrow,"dnk_awaygame_no"]

  dnk_winmargin_vec_gamestate_h <- as.vector(dnk_winmargin_h[dnk_hometeamindex_wm,])
  dnk_winmargin_vec_gamestate_h[is.na(dnk_winmargin_vec_gamestate_h)] <- ""
  dnk_winmargin_vec_gamestate_h <- dnk_winmargin_vec_gamestate_h[dnk_winmargin_vec_gamestate_h != ""]
  dnk_winmargin_vec_gamestate_h <- as.numeric(dnk_winmargin_vec_gamestate_h)

  dnk_winmargin_vec_gamestate_a <- as.vector(dnk_winmargin_h[dnk_awayteamindex_wm,])
  dnk_winmargin_vec_gamestate_a[is.na(dnk_winmargin_vec_gamestate_a)] <- ""
  dnk_winmargin_vec_gamestate_a <- dnk_winmargin_vec_gamestate_a[dnk_winmargin_vec_gamestate_a != ""]
  dnk_winmargin_vec_gamestate_a <- as.numeric(dnk_winmargin_vec_gamestate_a)

  for (dnk_game_no in 1:dnk_games_played[1])

  {

    dnk_homewinmargin[dnk_secondsplitrow] <- dnk_winmargin_vec_gamestate_h[dnk_hometeam_game_no]
    dnk_awaywinmargin[dnk_secondsplitrow] <- dnk_winmargin_vec_gamestate_a[dnk_awayteam_game_no]


  }

}

dnk_homewinmargin
dnk_awaywinmargin

DNK_gamestate_data <- cbind(DNK_secondsplit,dnk_homewinmargin,dnk_awaywinmargin)
DNK_gamestate_data
DNK_gamestate_data$dnk_GSCS <- paste(DNK_gamestate_data$dnk_homewinmargin,DNK_gamestate_data$dnk_awaywinmargin, sep = ",")
DNK_gamestate_data$dnk_GSH <- DNK_gamestate_data$dnk_homewinmargin - DNK_gamestate_data$dnk_awaywinmargin
head(DNK_gamestate_data,5)
DNK_gamestate_copy <- DNK_gamestate_data
DNK_gamestate_copy$dnk_HomeTeam_index_wm <- NULL
DNK_gamestate_copy$dnk_AwayTeam_index_wm <- NULL
DNK_gamestate_copy$dnk_homegame_no <- NULL
DNK_gamestate_copy$dnk_awaygame_no <- NULL
head(DNK_gamestate_copy)
colnames(DNK_gamestate_copy)[22] <- "homewinmargin"
colnames(DNK_gamestate_copy)[23] <- "awaywinmargin"
colnames(DNK_gamestate_copy)[24] <- "GSCS"
colnames(DNK_gamestate_copy)[25] <- "GSH"
DNK_gamestate_copy$CS <- paste(DNK_gamestate_copy$HG,DNK_gamestate_copy$AG, sep = "-")
DNK_gamestate_copy
#end DNK
###################################################################################################
###################################################################################################
#FIN
FIN_margindata <- FIN
fin_firstgames <- length(fin_teams)/2
FIN_firstsplit <-  head(FIN_margindata,fin_firstgames)
FIN_secondsplit <- tail(FIN_margindata,nrow(FIN_margindata) - fin_firstgames)
FIN_secondsplit$fin_HomeTeam_index_wm <- match(FIN_secondsplit$Home,fin_teams)
FIN_secondsplit$fin_AwayTeam_index_wm <- match(FIN_secondsplit$Away,fin_teams)
FIN_secondsplit$fin_homegame_no <- rep(2:max(fin_games_played[]) - 1,each = length(fin_teams)/2,length.out = nrow(FIN_secondsplit))
FIN_secondsplit$fin_awaygame_no <- rep(2:max(fin_games_played[]) - 1,each = length(fin_teams)/2,length.out = nrow(FIN_secondsplit))

fin_homewinmargin <- c()
fin_awaywinmargin <- c()

for (fin_secondsplitrow in 1:nrow(FIN_secondsplit))
{

  fin_hometeamindex_wm <- FIN_secondsplit[fin_secondsplitrow,"fin_HomeTeam_index_wm"]
  fin_awayteamindex_wm <- FIN_secondsplit[fin_secondsplitrow,"fin_AwayTeam_index_wm"]
  fin_hometeam_game_no <- FIN_secondsplit[fin_secondsplitrow,"fin_homegame_no"]
  fin_awayteam_game_no <- FIN_secondsplit[fin_secondsplitrow,"fin_awaygame_no"]

  fin_winmargin_vec_gamestate_h <- as.vector(fin_winmargin_h[fin_hometeamindex_wm,])
  fin_winmargin_vec_gamestate_h[is.na(fin_winmargin_vec_gamestate_h)] <- ""
  fin_winmargin_vec_gamestate_h <- fin_winmargin_vec_gamestate_h[fin_winmargin_vec_gamestate_h != ""]
  fin_winmargin_vec_gamestate_h <- as.numeric(fin_winmargin_vec_gamestate_h)

  fin_winmargin_vec_gamestate_a <- as.vector(fin_winmargin_h[fin_awayteamindex_wm,])
  fin_winmargin_vec_gamestate_a[is.na(fin_winmargin_vec_gamestate_a)] <- ""
  fin_winmargin_vec_gamestate_a <- fin_winmargin_vec_gamestate_a[fin_winmargin_vec_gamestate_a != ""]
  fin_winmargin_vec_gamestate_a <- as.numeric(fin_winmargin_vec_gamestate_a)

  for (fin_game_no in 1:fin_games_played[1])

  {

    fin_homewinmargin[fin_secondsplitrow] <- fin_winmargin_vec_gamestate_h[fin_hometeam_game_no]
    fin_awaywinmargin[fin_secondsplitrow] <- fin_winmargin_vec_gamestate_a[fin_awayteam_game_no]


  }

}

fin_homewinmargin
fin_awaywinmargin

FIN_gamestate_data <- cbind(FIN_secondsplit,fin_homewinmargin,fin_awaywinmargin)
FIN_gamestate_data
FIN_gamestate_data$fin_GSCS <- paste(FIN_gamestate_data$fin_homewinmargin,FIN_gamestate_data$fin_awaywinmargin, sep = ",")
FIN_gamestate_data$fin_GSH <- FIN_gamestate_data$fin_homewinmargin - FIN_gamestate_data$fin_awaywinmargin
head(FIN_gamestate_data,5)
FIN_gamestate_copy <- FIN_gamestate_data
FIN_gamestate_copy$fin_HomeTeam_index_wm <- NULL
FIN_gamestate_copy$fin_AwayTeam_index_wm <- NULL
FIN_gamestate_copy$fin_homegame_no <- NULL
FIN_gamestate_copy$fin_awaygame_no <- NULL
head(FIN_gamestate_copy)
colnames(FIN_gamestate_copy)[22] <- "homewinmargin"
colnames(FIN_gamestate_copy)[23] <- "awaywinmargin"
colnames(FIN_gamestate_copy)[24] <- "GSCS"
colnames(FIN_gamestate_copy)[25] <- "GSH"
FIN_gamestate_copy$CS <- paste(FIN_gamestate_copy$HG,FIN_gamestate_copy$AG, sep = "-")
FIN_gamestate_copy
#end FIN
###################################################################################################
##################################################################################################
#IRL
IRL_margindata <- IRL
irl_firstgames <- length(irl_teams)/2
IRL_firstsplit <-  head(IRL_margindata,irl_firstgames)
IRL_secondsplit <- tail(IRL_margindata,nrow(IRL_margindata) - irl_firstgames)
IRL_secondsplit$irl_HomeTeam_index_wm <- match(IRL_secondsplit$Home,irl_teams)
IRL_secondsplit$irl_AwayTeam_index_wm <- match(IRL_secondsplit$Away,irl_teams)
IRL_secondsplit$irl_homegame_no <- rep(2:max(irl_games_played[]) - 1,each = length(irl_teams)/2,length.out = nrow(IRL_secondsplit))
IRL_secondsplit$irl_awaygame_no <- rep(2:max(irl_games_played[]) - 1,each = length(irl_teams)/2,length.out = nrow(IRL_secondsplit))

irl_homewinmargin <- c()
irl_awaywinmargin <- c()

for (irl_secondsplitrow in 1:nrow(IRL_secondsplit))
{

  irl_hometeamindex_wm <- IRL_secondsplit[irl_secondsplitrow,"irl_HomeTeam_index_wm"]
  irl_awayteamindex_wm <- IRL_secondsplit[irl_secondsplitrow,"irl_AwayTeam_index_wm"]
  irl_hometeam_game_no <- IRL_secondsplit[irl_secondsplitrow,"irl_homegame_no"]
  irl_awayteam_game_no <- IRL_secondsplit[irl_secondsplitrow,"irl_awaygame_no"]

  irl_winmargin_vec_gamestate_h <- as.vector(irl_winmargin_h[irl_hometeamindex_wm,])
  irl_winmargin_vec_gamestate_h[is.na(irl_winmargin_vec_gamestate_h)] <- ""
  irl_winmargin_vec_gamestate_h <- irl_winmargin_vec_gamestate_h[irl_winmargin_vec_gamestate_h != ""]
  irl_winmargin_vec_gamestate_h <- as.numeric(irl_winmargin_vec_gamestate_h)

  irl_winmargin_vec_gamestate_a <- as.vector(irl_winmargin_h[irl_awayteamindex_wm,])
  irl_winmargin_vec_gamestate_a[is.na(irl_winmargin_vec_gamestate_a)] <- ""
  irl_winmargin_vec_gamestate_a <- irl_winmargin_vec_gamestate_a[irl_winmargin_vec_gamestate_a != ""]
  irl_winmargin_vec_gamestate_a <- as.numeric(irl_winmargin_vec_gamestate_a)

  for (irl_game_no in 1:irl_games_played[1])

  {

    irl_homewinmargin[irl_secondsplitrow] <- irl_winmargin_vec_gamestate_h[irl_hometeam_game_no]
    irl_awaywinmargin[irl_secondsplitrow] <- irl_winmargin_vec_gamestate_a[irl_awayteam_game_no]


  }

}

irl_homewinmargin
irl_awaywinmargin

IRL_gamestate_data <- cbind(IRL_secondsplit,irl_homewinmargin,irl_awaywinmargin)
IRL_gamestate_data
IRL_gamestate_data$irl_GSCS <- paste(IRL_gamestate_data$irl_homewinmargin,IRL_gamestate_data$irl_awaywinmargin, sep = ",")
IRL_gamestate_data$irl_GSH <- IRL_gamestate_data$irl_homewinmargin - IRL_gamestate_data$irl_awaywinmargin
head(IRL_gamestate_data,5)
IRL_gamestate_copy <- IRL_gamestate_data
IRL_gamestate_copy$irl_HomeTeam_index_wm <- NULL
IRL_gamestate_copy$irl_AwayTeam_index_wm <- NULL
IRL_gamestate_copy$irl_homegame_no <- NULL
IRL_gamestate_copy$irl_awaygame_no <- NULL
head(IRL_gamestate_copy)
colnames(IRL_gamestate_copy)[22] <- "homewinmargin"
colnames(IRL_gamestate_copy)[23] <- "awaywinmargin"
colnames(IRL_gamestate_copy)[24] <- "GSCS"
colnames(IRL_gamestate_copy)[25] <- "GSH"
IRL_gamestate_copy$CS <- paste(IRL_gamestate_copy$HG,IRL_gamestate_copy$AG, sep = "-")
IRL_gamestate_copy
#end IRL
###################################################################################################
###################################################################################################
#JPN
JPN_margindata <- JPN
jpn_firstgames <- length(jpn_teams)/2
JPN_firstsplit <-  head(JPN_margindata,jpn_firstgames)
JPN_secondsplit <- tail(JPN_margindata,nrow(JPN_margindata) - jpn_firstgames)
JPN_secondsplit$jpn_HomeTeam_index_wm <- match(JPN_secondsplit$Home,jpn_teams)
JPN_secondsplit$jpn_AwayTeam_index_wm <- match(JPN_secondsplit$Away,jpn_teams)
JPN_secondsplit$jpn_homegame_no <- rep(2:max(jpn_games_played[]) - 1,each = length(jpn_teams)/2,length.out = nrow(JPN_secondsplit))
JPN_secondsplit$jpn_awaygame_no <- rep(2:max(jpn_games_played[]) - 1,each = length(jpn_teams)/2,length.out = nrow(JPN_secondsplit))

jpn_homewinmargin <- c()
jpn_awaywinmargin <- c()

for (jpn_secondsplitrow in 1:nrow(JPN_secondsplit))
{

  jpn_hometeamindex_wm <- JPN_secondsplit[jpn_secondsplitrow,"jpn_HomeTeam_index_wm"]
  jpn_awayteamindex_wm <- JPN_secondsplit[jpn_secondsplitrow,"jpn_AwayTeam_index_wm"]
  jpn_hometeam_game_no <- JPN_secondsplit[jpn_secondsplitrow,"jpn_homegame_no"]
  jpn_awayteam_game_no <- JPN_secondsplit[jpn_secondsplitrow,"jpn_awaygame_no"]

  jpn_winmargin_vec_gamestate_h <- as.vector(jpn_winmargin_h[jpn_hometeamindex_wm,])
  jpn_winmargin_vec_gamestate_h[is.na(jpn_winmargin_vec_gamestate_h)] <- ""
  jpn_winmargin_vec_gamestate_h <- jpn_winmargin_vec_gamestate_h[jpn_winmargin_vec_gamestate_h != ""]
  jpn_winmargin_vec_gamestate_h <- as.numeric(jpn_winmargin_vec_gamestate_h)

  jpn_winmargin_vec_gamestate_a <- as.vector(jpn_winmargin_h[jpn_awayteamindex_wm,])
  jpn_winmargin_vec_gamestate_a[is.na(jpn_winmargin_vec_gamestate_a)] <- ""
  jpn_winmargin_vec_gamestate_a <- jpn_winmargin_vec_gamestate_a[jpn_winmargin_vec_gamestate_a != ""]
  jpn_winmargin_vec_gamestate_a <- as.numeric(jpn_winmargin_vec_gamestate_a)

  for (jpn_game_no in 1:jpn_games_played[1])

  {

    jpn_homewinmargin[jpn_secondsplitrow] <- jpn_winmargin_vec_gamestate_h[jpn_hometeam_game_no]
    jpn_awaywinmargin[jpn_secondsplitrow] <- jpn_winmargin_vec_gamestate_a[jpn_awayteam_game_no]


  }

}

jpn_homewinmargin
jpn_awaywinmargin

JPN_gamestate_data <- cbind(JPN_secondsplit,jpn_homewinmargin,jpn_awaywinmargin)
JPN_gamestate_data
JPN_gamestate_data$jpn_GSCS <- paste(JPN_gamestate_data$jpn_homewinmargin,JPN_gamestate_data$jpn_awaywinmargin, sep = ",")
JPN_gamestate_data$jpn_GSH <- JPN_gamestate_data$jpn_homewinmargin - JPN_gamestate_data$jpn_awaywinmargin
head(JPN_gamestate_data,5)
JPN_gamestate_copy <- JPN_gamestate_data
JPN_gamestate_copy$jpn_HomeTeam_index_wm <- NULL
JPN_gamestate_copy$jpn_AwayTeam_index_wm <- NULL
JPN_gamestate_copy$jpn_homegame_no <- NULL
JPN_gamestate_copy$jpn_awaygame_no <- NULL
head(JPN_gamestate_copy)
colnames(JPN_gamestate_copy)[22] <- "homewinmargin"
colnames(JPN_gamestate_copy)[23] <- "awaywinmargin"
colnames(JPN_gamestate_copy)[24] <- "GSCS"
colnames(JPN_gamestate_copy)[25] <- "GSH"
JPN_gamestate_copy$CS <- paste(JPN_gamestate_copy$HG,JPN_gamestate_copy$AG, sep = "-")
JPN_gamestate_copy
#end JPN
###################################################################################################
###################################################################################################
#AUT
MEX_margindata <- MEX
mex_firstgames <- length(mex_teams)/2
MEX_firstsplit <-  head(MEX_margindata,mex_firstgames)
MEX_secondsplit <- tail(MEX_margindata,nrow(MEX_margindata) - mex_firstgames)
MEX_secondsplit$mex_HomeTeam_index_wm <- match(MEX_secondsplit$Home,mex_teams)
MEX_secondsplit$mex_AwayTeam_index_wm <- match(MEX_secondsplit$Away,mex_teams)
MEX_secondsplit$mex_homegame_no <- rep(2:max(mex_games_played[]) - 1,each = length(mex_teams)/2,length.out = nrow(MEX_secondsplit))
MEX_secondsplit$mex_awaygame_no <- rep(2:max(mex_games_played[]) - 1,each = length(mex_teams)/2,length.out = nrow(MEX_secondsplit))

mex_homewinmargin <- c()
mex_awaywinmargin <- c()

for (mex_secondsplitrow in 1:nrow(MEX_secondsplit))
{

  mex_hometeamindex_wm <- MEX_secondsplit[mex_secondsplitrow,"mex_HomeTeam_index_wm"]
  mex_awayteamindex_wm <- MEX_secondsplit[mex_secondsplitrow,"mex_AwayTeam_index_wm"]
  mex_hometeam_game_no <- MEX_secondsplit[mex_secondsplitrow,"mex_homegame_no"]
  mex_awayteam_game_no <- MEX_secondsplit[mex_secondsplitrow,"mex_awaygame_no"]

  mex_winmargin_vec_gamestate_h <- as.vector(mex_winmargin_h[mex_hometeamindex_wm,])
  mex_winmargin_vec_gamestate_h[is.na(mex_winmargin_vec_gamestate_h)] <- ""
  mex_winmargin_vec_gamestate_h <- mex_winmargin_vec_gamestate_h[mex_winmargin_vec_gamestate_h != ""]
  mex_winmargin_vec_gamestate_h <- as.numeric(mex_winmargin_vec_gamestate_h)

  mex_winmargin_vec_gamestate_a <- as.vector(mex_winmargin_h[mex_awayteamindex_wm,])
  mex_winmargin_vec_gamestate_a[is.na(mex_winmargin_vec_gamestate_a)] <- ""
  mex_winmargin_vec_gamestate_a <- mex_winmargin_vec_gamestate_a[mex_winmargin_vec_gamestate_a != ""]
  mex_winmargin_vec_gamestate_a <- as.numeric(mex_winmargin_vec_gamestate_a)

  for (mex_game_no in 1:mex_games_played[1])

  {

    mex_homewinmargin[mex_secondsplitrow] <- mex_winmargin_vec_gamestate_h[mex_hometeam_game_no]
    mex_awaywinmargin[mex_secondsplitrow] <- mex_winmargin_vec_gamestate_a[mex_awayteam_game_no]


  }

}

mex_homewinmargin
mex_awaywinmargin

MEX_gamestate_data <- cbind(MEX_secondsplit,mex_homewinmargin,mex_awaywinmargin)
MEX_gamestate_data
MEX_gamestate_data$mex_GSCS <- paste(MEX_gamestate_data$mex_homewinmargin,MEX_gamestate_data$mex_awaywinmargin, sep = ",")
MEX_gamestate_data$mex_GSH <- MEX_gamestate_data$mex_homewinmargin - MEX_gamestate_data$mex_awaywinmargin
head(MEX_gamestate_data,5)
MEX_gamestate_copy <- MEX_gamestate_data
MEX_gamestate_copy$mex_HomeTeam_index_wm <- NULL
MEX_gamestate_copy$mex_AwayTeam_index_wm <- NULL
MEX_gamestate_copy$mex_homegame_no <- NULL
MEX_gamestate_copy$mex_awaygame_no <- NULL
head(MEX_gamestate_copy)
colnames(MEX_gamestate_copy)[22] <- "homewinmargin"
colnames(MEX_gamestate_copy)[23] <- "awaywinmargin"
colnames(MEX_gamestate_copy)[24] <- "GSCS"
colnames(MEX_gamestate_copy)[25] <- "GSH"
MEX_gamestate_copy$CS <- paste(MEX_gamestate_copy$HG,MEX_gamestate_copy$AG, sep = "-")
MEX_gamestate_copy
#end MEX
###################################################################################################
###################################################################################################
#MLS
MLS_margindata <- MLS
mls_firstgames <- length(mls_teams)/2
MLS_firstsplit <-  head(MLS_margindata,mls_firstgames)
MLS_secondsplit <- tail(MLS_margindata,nrow(MLS_margindata) - mls_firstgames)
MLS_secondsplit$mls_HomeTeam_index_wm <- match(MLS_secondsplit$Home,mls_teams)
MLS_secondsplit$mls_AwayTeam_index_wm <- match(MLS_secondsplit$Away,mls_teams)
MLS_secondsplit$mls_homegame_no <- rep(2:max(mls_games_played[]) - 1,each = length(mls_teams)/2,length.out = nrow(MLS_secondsplit))
MLS_secondsplit$mls_awaygame_no <- rep(2:max(mls_games_played[]) - 1,each = length(mls_teams)/2,length.out = nrow(MLS_secondsplit))

mls_homewinmargin <- c()
mls_awaywinmargin <- c()

for (mls_secondsplitrow in 1:nrow(MLS_secondsplit))
{

  mls_hometeamindex_wm <- MLS_secondsplit[mls_secondsplitrow,"mls_HomeTeam_index_wm"]
  mls_awayteamindex_wm <- MLS_secondsplit[mls_secondsplitrow,"mls_AwayTeam_index_wm"]
  mls_hometeam_game_no <- MLS_secondsplit[mls_secondsplitrow,"mls_homegame_no"]
  mls_awayteam_game_no <- MLS_secondsplit[mls_secondsplitrow,"mls_awaygame_no"]

  mls_winmargin_vec_gamestate_h <- as.vector(mls_winmargin_h[mls_hometeamindex_wm,])
  mls_winmargin_vec_gamestate_h[is.na(mls_winmargin_vec_gamestate_h)] <- ""
  mls_winmargin_vec_gamestate_h <- mls_winmargin_vec_gamestate_h[mls_winmargin_vec_gamestate_h != ""]
  mls_winmargin_vec_gamestate_h <- as.numeric(mls_winmargin_vec_gamestate_h)

  mls_winmargin_vec_gamestate_a <- as.vector(mls_winmargin_h[mls_awayteamindex_wm,])
  mls_winmargin_vec_gamestate_a[is.na(mls_winmargin_vec_gamestate_a)] <- ""
  mls_winmargin_vec_gamestate_a <- mls_winmargin_vec_gamestate_a[mls_winmargin_vec_gamestate_a != ""]
  mls_winmargin_vec_gamestate_a <- as.numeric(mls_winmargin_vec_gamestate_a)

  for (mls_game_no in 1:mls_games_played[1])

  {

    mls_homewinmargin[mls_secondsplitrow] <- mls_winmargin_vec_gamestate_h[mls_hometeam_game_no]
    mls_awaywinmargin[mls_secondsplitrow] <- mls_winmargin_vec_gamestate_a[mls_awayteam_game_no]


  }

}

mls_homewinmargin
mls_awaywinmargin

MLS_gamestate_data <- cbind(MLS_secondsplit,mls_homewinmargin,mls_awaywinmargin)
MLS_gamestate_data
MLS_gamestate_data$mls_GSCS <- paste(MLS_gamestate_data$mls_homewinmargin,MLS_gamestate_data$mls_awaywinmargin, sep = ",")
MLS_gamestate_data$mls_GSH <- MLS_gamestate_data$mls_homewinmargin - MLS_gamestate_data$mls_awaywinmargin
head(MLS_gamestate_data,5)
MLS_gamestate_copy <- MLS_gamestate_data
MLS_gamestate_copy$mls_HomeTeam_index_wm <- NULL
MLS_gamestate_copy$mls_AwayTeam_index_wm <- NULL
MLS_gamestate_copy$mls_homegame_no <- NULL
MLS_gamestate_copy$mls_awaygame_no <- NULL
head(MLS_gamestate_copy)
colnames(MLS_gamestate_copy)[22] <- "homewinmargin"
colnames(MLS_gamestate_copy)[23] <- "awaywinmargin"
colnames(MLS_gamestate_copy)[24] <- "GSCS"
colnames(MLS_gamestate_copy)[25] <- "GSH"
MLS_gamestate_copy$CS <- paste(MLS_gamestate_copy$HG,MLS_gamestate_copy$AG, sep = "-")
MLS_gamestate_copy
#end MLS
###################################################################################################
###################################################################################################
#NOR
NOR_margindata <- NOR
nor_firstgames <- length(nor_teams)/2
NOR_firstsplit <-  head(NOR_margindata,nor_firstgames)
NOR_secondsplit <- tail(NOR_margindata,nrow(NOR_margindata) - nor_firstgames)
NOR_secondsplit$nor_HomeTeam_index_wm <- match(NOR_secondsplit$Home,nor_teams)
NOR_secondsplit$nor_AwayTeam_index_wm <- match(NOR_secondsplit$Away,nor_teams)
NOR_secondsplit$nor_homegame_no <- rep(2:max(nor_games_played[]) - 1,each = length(nor_teams)/2,length.out = nrow(NOR_secondsplit))
NOR_secondsplit$nor_awaygame_no <- rep(2:max(nor_games_played[]) - 1,each = length(nor_teams)/2,length.out = nrow(NOR_secondsplit))

nor_homewinmargin <- c()
nor_awaywinmargin <- c()

for (nor_secondsplitrow in 1:nrow(NOR_secondsplit))
{

  nor_hometeamindex_wm <- NOR_secondsplit[nor_secondsplitrow,"nor_HomeTeam_index_wm"]
  nor_awayteamindex_wm <- NOR_secondsplit[nor_secondsplitrow,"nor_AwayTeam_index_wm"]
  nor_hometeam_game_no <- NOR_secondsplit[nor_secondsplitrow,"nor_homegame_no"]
  nor_awayteam_game_no <- NOR_secondsplit[nor_secondsplitrow,"nor_awaygame_no"]

  nor_winmargin_vec_gamestate_h <- as.vector(nor_winmargin_h[nor_hometeamindex_wm,])
  nor_winmargin_vec_gamestate_h[is.na(nor_winmargin_vec_gamestate_h)] <- ""
  nor_winmargin_vec_gamestate_h <- nor_winmargin_vec_gamestate_h[nor_winmargin_vec_gamestate_h != ""]
  nor_winmargin_vec_gamestate_h <- as.numeric(nor_winmargin_vec_gamestate_h)

  nor_winmargin_vec_gamestate_a <- as.vector(nor_winmargin_h[nor_awayteamindex_wm,])
  nor_winmargin_vec_gamestate_a[is.na(nor_winmargin_vec_gamestate_a)] <- ""
  nor_winmargin_vec_gamestate_a <- nor_winmargin_vec_gamestate_a[nor_winmargin_vec_gamestate_a != ""]
  nor_winmargin_vec_gamestate_a <- as.numeric(nor_winmargin_vec_gamestate_a)

  for (nor_game_no in 1:nor_games_played[1])

  {

    nor_homewinmargin[nor_secondsplitrow] <- nor_winmargin_vec_gamestate_h[nor_hometeam_game_no]
    nor_awaywinmargin[nor_secondsplitrow] <- nor_winmargin_vec_gamestate_a[nor_awayteam_game_no]


  }

}

nor_homewinmargin
nor_awaywinmargin

NOR_gamestate_data <- cbind(NOR_secondsplit,nor_homewinmargin,nor_awaywinmargin)
NOR_gamestate_data
NOR_gamestate_data$nor_GSCS <- paste(NOR_gamestate_data$nor_homewinmargin,NOR_gamestate_data$nor_awaywinmargin, sep = ",")
NOR_gamestate_data$nor_GSH <- NOR_gamestate_data$nor_homewinmargin - NOR_gamestate_data$nor_awaywinmargin
head(NOR_gamestate_data,5)
NOR_gamestate_copy <- NOR_gamestate_data
NOR_gamestate_copy$nor_HomeTeam_index_wm <- NULL
NOR_gamestate_copy$nor_AwayTeam_index_wm <- NULL
NOR_gamestate_copy$nor_homegame_no <- NULL
NOR_gamestate_copy$nor_awaygame_no <- NULL
head(NOR_gamestate_copy)
colnames(NOR_gamestate_copy)[22] <- "homewinmargin"
colnames(NOR_gamestate_copy)[23] <- "awaywinmargin"
colnames(NOR_gamestate_copy)[24] <- "GSCS"
colnames(NOR_gamestate_copy)[25] <- "GSH"
NOR_gamestate_copy$CS <- paste(NOR_gamestate_copy$HG,NOR_gamestate_copy$AG, sep = "-")
NOR_gamestate_copy
#end NOR
###################################################################################################
###################################################################################################
#POL
POL_margindata <- POL
pol_firstgames <- length(pol_teams)/2
POL_firstsplit <-  head(POL_margindata,pol_firstgames)
POL_secondsplit <- tail(POL_margindata,nrow(POL_margindata) - pol_firstgames)
POL_secondsplit$pol_HomeTeam_index_wm <- match(POL_secondsplit$Home,pol_teams)
POL_secondsplit$pol_AwayTeam_index_wm <- match(POL_secondsplit$Away,pol_teams)
POL_secondsplit$pol_homegame_no <- rep(2:max(pol_games_played[]) - 1,each = length(pol_teams)/2,length.out = nrow(POL_secondsplit))
POL_secondsplit$pol_awaygame_no <- rep(2:max(pol_games_played[]) - 1,each = length(pol_teams)/2,length.out = nrow(POL_secondsplit))

pol_homewinmargin <- c()
pol_awaywinmargin <- c()

for (pol_secondsplitrow in 1:nrow(POL_secondsplit))
{

  pol_hometeamindex_wm <- POL_secondsplit[pol_secondsplitrow,"pol_HomeTeam_index_wm"]
  pol_awayteamindex_wm <- POL_secondsplit[pol_secondsplitrow,"pol_AwayTeam_index_wm"]
  pol_hometeam_game_no <- POL_secondsplit[pol_secondsplitrow,"pol_homegame_no"]
  pol_awayteam_game_no <- POL_secondsplit[pol_secondsplitrow,"pol_awaygame_no"]

  pol_winmargin_vec_gamestate_h <- as.vector(pol_winmargin_h[pol_hometeamindex_wm,])
  pol_winmargin_vec_gamestate_h[is.na(pol_winmargin_vec_gamestate_h)] <- ""
  pol_winmargin_vec_gamestate_h <- pol_winmargin_vec_gamestate_h[pol_winmargin_vec_gamestate_h != ""]
  pol_winmargin_vec_gamestate_h <- as.numeric(pol_winmargin_vec_gamestate_h)

  pol_winmargin_vec_gamestate_a <- as.vector(pol_winmargin_h[pol_awayteamindex_wm,])
  pol_winmargin_vec_gamestate_a[is.na(pol_winmargin_vec_gamestate_a)] <- ""
  pol_winmargin_vec_gamestate_a <- pol_winmargin_vec_gamestate_a[pol_winmargin_vec_gamestate_a != ""]
  pol_winmargin_vec_gamestate_a <- as.numeric(pol_winmargin_vec_gamestate_a)

  for (pol_game_no in 1:pol_games_played[1])

  {

    pol_homewinmargin[pol_secondsplitrow] <- pol_winmargin_vec_gamestate_h[pol_hometeam_game_no]
    pol_awaywinmargin[pol_secondsplitrow] <- pol_winmargin_vec_gamestate_a[pol_awayteam_game_no]


  }

}

pol_homewinmargin
pol_awaywinmargin

POL_gamestate_data <- cbind(POL_secondsplit,pol_homewinmargin,pol_awaywinmargin)
POL_gamestate_data
POL_gamestate_data$pol_GSCS <- paste(POL_gamestate_data$pol_homewinmargin,POL_gamestate_data$pol_awaywinmargin, sep = ",")
POL_gamestate_data$pol_GSH <- POL_gamestate_data$pol_homewinmargin - POL_gamestate_data$pol_awaywinmargin
head(POL_gamestate_data,5)
POL_gamestate_copy <- POL_gamestate_data
POL_gamestate_copy$pol_HomeTeam_index_wm <- NULL
POL_gamestate_copy$pol_AwayTeam_index_wm <- NULL
POL_gamestate_copy$pol_homegame_no <- NULL
POL_gamestate_copy$pol_awaygame_no <- NULL
head(POL_gamestate_copy)
colnames(POL_gamestate_copy)[22] <- "homewinmargin"
colnames(POL_gamestate_copy)[23] <- "awaywinmargin"
colnames(POL_gamestate_copy)[24] <- "GSCS"
colnames(POL_gamestate_copy)[25] <- "GSH"
POL_gamestate_copy$CS <- paste(POL_gamestate_copy$HG,POL_gamestate_copy$AG, sep = "-")
POL_gamestate_copy
#end POL
###################################################################################################
###################################################################################################
#ROU
ROU_margindata <- ROU
rou_firstgames <- length(rou_teams)/2
ROU_firstsplit <-  head(ROU_margindata,rou_firstgames)
ROU_secondsplit <- tail(ROU_margindata,nrow(ROU_margindata) - rou_firstgames)
ROU_secondsplit$rou_HomeTeam_index_wm <- match(ROU_secondsplit$Home,rou_teams)
ROU_secondsplit$rou_AwayTeam_index_wm <- match(ROU_secondsplit$Away,rou_teams)
ROU_secondsplit$rou_homegame_no <- rep(2:max(rou_games_played[]) - 1,each = length(rou_teams)/2,length.out = nrow(ROU_secondsplit))
ROU_secondsplit$rou_awaygame_no <- rep(2:max(rou_games_played[]) - 1,each = length(rou_teams)/2,length.out = nrow(ROU_secondsplit))

rou_homewinmargin <- c()
rou_awaywinmargin <- c()

for (rou_secondsplitrow in 1:nrow(ROU_secondsplit))
{

  rou_hometeamindex_wm <- ROU_secondsplit[rou_secondsplitrow,"rou_HomeTeam_index_wm"]
  rou_awayteamindex_wm <- ROU_secondsplit[rou_secondsplitrow,"rou_AwayTeam_index_wm"]
  rou_hometeam_game_no <- ROU_secondsplit[rou_secondsplitrow,"rou_homegame_no"]
  rou_awayteam_game_no <- ROU_secondsplit[rou_secondsplitrow,"rou_awaygame_no"]

  rou_winmargin_vec_gamestate_h <- as.vector(rou_winmargin_h[rou_hometeamindex_wm,])
  rou_winmargin_vec_gamestate_h[is.na(rou_winmargin_vec_gamestate_h)] <- ""
  rou_winmargin_vec_gamestate_h <- rou_winmargin_vec_gamestate_h[rou_winmargin_vec_gamestate_h != ""]
  rou_winmargin_vec_gamestate_h <- as.numeric(rou_winmargin_vec_gamestate_h)

  rou_winmargin_vec_gamestate_a <- as.vector(rou_winmargin_h[rou_awayteamindex_wm,])
  rou_winmargin_vec_gamestate_a[is.na(rou_winmargin_vec_gamestate_a)] <- ""
  rou_winmargin_vec_gamestate_a <- rou_winmargin_vec_gamestate_a[rou_winmargin_vec_gamestate_a != ""]
  rou_winmargin_vec_gamestate_a <- as.numeric(rou_winmargin_vec_gamestate_a)

  for (rou_game_no in 1:rou_games_played[1])

  {

    rou_homewinmargin[rou_secondsplitrow] <- rou_winmargin_vec_gamestate_h[rou_hometeam_game_no]
    rou_awaywinmargin[rou_secondsplitrow] <- rou_winmargin_vec_gamestate_a[rou_awayteam_game_no]


  }

}

rou_homewinmargin
rou_awaywinmargin

ROU_gamestate_data <- cbind(ROU_secondsplit,rou_homewinmargin,rou_awaywinmargin)
ROU_gamestate_data
ROU_gamestate_data$rou_GSCS <- paste(ROU_gamestate_data$rou_homewinmargin,ROU_gamestate_data$rou_awaywinmargin, sep = ",")
ROU_gamestate_data$rou_GSH <- ROU_gamestate_data$rou_homewinmargin - ROU_gamestate_data$rou_awaywinmargin
head(ROU_gamestate_data,5)
ROU_gamestate_copy <- ROU_gamestate_data
ROU_gamestate_copy$rou_HomeTeam_index_wm <- NULL
ROU_gamestate_copy$rou_AwayTeam_index_wm <- NULL
ROU_gamestate_copy$rou_homegame_no <- NULL
ROU_gamestate_copy$rou_awaygame_no <- NULL
head(ROU_gamestate_copy)
colnames(ROU_gamestate_copy)[22] <- "homewinmargin"
colnames(ROU_gamestate_copy)[23] <- "awaywinmargin"
colnames(ROU_gamestate_copy)[24] <- "GSCS"
colnames(ROU_gamestate_copy)[25] <- "GSH"
ROU_gamestate_copy$CS <- paste(ROU_gamestate_copy$HG,ROU_gamestate_copy$AG, sep = "-")
ROU_gamestate_copy
#end ROU
###################################################################################################
###################################################################################################
#RUS
RUS_margindata <- RUS
rus_firstgames <- length(rus_teams)/2
RUS_firstsplit <-  head(RUS_margindata,rus_firstgames)
RUS_secondsplit <- tail(RUS_margindata,nrow(RUS_margindata) - rus_firstgames)
RUS_secondsplit$rus_HomeTeam_index_wm <- match(RUS_secondsplit$Home,rus_teams)
RUS_secondsplit$rus_AwayTeam_index_wm <- match(RUS_secondsplit$Away,rus_teams)
RUS_secondsplit$rus_homegame_no <- rep(2:max(rus_games_played[]) - 1,each = length(rus_teams)/2,length.out = nrow(RUS_secondsplit))
RUS_secondsplit$rus_awaygame_no <- rep(2:max(rus_games_played[]) - 1,each = length(rus_teams)/2,length.out = nrow(RUS_secondsplit))

rus_homewinmargin <- c()
rus_awaywinmargin <- c()

for (rus_secondsplitrow in 1:nrow(RUS_secondsplit))
{

  rus_hometeamindex_wm <- RUS_secondsplit[rus_secondsplitrow,"rus_HomeTeam_index_wm"]
  rus_awayteamindex_wm <- RUS_secondsplit[rus_secondsplitrow,"rus_AwayTeam_index_wm"]
  rus_hometeam_game_no <- RUS_secondsplit[rus_secondsplitrow,"rus_homegame_no"]
  rus_awayteam_game_no <- RUS_secondsplit[rus_secondsplitrow,"rus_awaygame_no"]

  rus_winmargin_vec_gamestate_h <- as.vector(rus_winmargin_h[rus_hometeamindex_wm,])
  rus_winmargin_vec_gamestate_h[is.na(rus_winmargin_vec_gamestate_h)] <- ""
  rus_winmargin_vec_gamestate_h <- rus_winmargin_vec_gamestate_h[rus_winmargin_vec_gamestate_h != ""]
  rus_winmargin_vec_gamestate_h <- as.numeric(rus_winmargin_vec_gamestate_h)

  rus_winmargin_vec_gamestate_a <- as.vector(rus_winmargin_h[rus_awayteamindex_wm,])
  rus_winmargin_vec_gamestate_a[is.na(rus_winmargin_vec_gamestate_a)] <- ""
  rus_winmargin_vec_gamestate_a <- rus_winmargin_vec_gamestate_a[rus_winmargin_vec_gamestate_a != ""]
  rus_winmargin_vec_gamestate_a <- as.numeric(rus_winmargin_vec_gamestate_a)

  for (rus_game_no in 1:rus_games_played[1])

  {

    rus_homewinmargin[rus_secondsplitrow] <- rus_winmargin_vec_gamestate_h[rus_hometeam_game_no]
    rus_awaywinmargin[rus_secondsplitrow] <- rus_winmargin_vec_gamestate_a[rus_awayteam_game_no]


  }

}

rus_homewinmargin
rus_awaywinmargin

RUS_gamestate_data <- cbind(RUS_secondsplit,rus_homewinmargin,rus_awaywinmargin)
RUS_gamestate_data
RUS_gamestate_data$rus_GSCS <- paste(RUS_gamestate_data$rus_homewinmargin,RUS_gamestate_data$rus_awaywinmargin, sep = ",")
RUS_gamestate_data$rus_GSH <- RUS_gamestate_data$rus_homewinmargin - RUS_gamestate_data$rus_awaywinmargin
head(RUS_gamestate_data,5)
RUS_gamestate_copy <- RUS_gamestate_data
RUS_gamestate_copy$rus_HomeTeam_index_wm <- NULL
RUS_gamestate_copy$rus_AwayTeam_index_wm <- NULL
RUS_gamestate_copy$rus_homegame_no <- NULL
RUS_gamestate_copy$rus_awaygame_no <- NULL
head(RUS_gamestate_copy)
colnames(RUS_gamestate_copy)[22] <- "homewinmargin"
colnames(RUS_gamestate_copy)[23] <- "awaywinmargin"
colnames(RUS_gamestate_copy)[24] <- "GSCS"
colnames(RUS_gamestate_copy)[25] <- "GSH"
RUS_gamestate_copy$CS <- paste(RUS_gamestate_copy$HG,RUS_gamestate_copy$AG, sep = "-")
RUS_gamestate_copy
#end RUS
###################################################################################################
###################################################################################################
#SWE
SWE_margindata <- SWE
swe_firstgames <- length(swe_teams)/2
SWE_firstsplit <-  head(SWE_margindata,swe_firstgames)
SWE_secondsplit <- tail(SWE_margindata,nrow(SWE_margindata) - swe_firstgames)
SWE_secondsplit$swe_HomeTeam_index_wm <- match(SWE_secondsplit$Home,swe_teams)
SWE_secondsplit$swe_AwayTeam_index_wm <- match(SWE_secondsplit$Away,swe_teams)
SWE_secondsplit$swe_homegame_no <- rep(2:max(swe_games_played[]) - 1,each = length(swe_teams)/2,length.out = nrow(SWE_secondsplit))
SWE_secondsplit$swe_awaygame_no <- rep(2:max(swe_games_played[]) - 1,each = length(swe_teams)/2,length.out = nrow(SWE_secondsplit))

swe_homewinmargin <- c()
swe_awaywinmargin <- c()

for (swe_secondsplitrow in 1:nrow(SWE_secondsplit))
{

  swe_hometeamindex_wm <- SWE_secondsplit[swe_secondsplitrow,"swe_HomeTeam_index_wm"]
  swe_awayteamindex_wm <- SWE_secondsplit[swe_secondsplitrow,"swe_AwayTeam_index_wm"]
  swe_hometeam_game_no <- SWE_secondsplit[swe_secondsplitrow,"swe_homegame_no"]
  swe_awayteam_game_no <- SWE_secondsplit[swe_secondsplitrow,"swe_awaygame_no"]

  swe_winmargin_vec_gamestate_h <- as.vector(swe_winmargin_h[swe_hometeamindex_wm,])
  swe_winmargin_vec_gamestate_h[is.na(swe_winmargin_vec_gamestate_h)] <- ""
  swe_winmargin_vec_gamestate_h <- swe_winmargin_vec_gamestate_h[swe_winmargin_vec_gamestate_h != ""]
  swe_winmargin_vec_gamestate_h <- as.numeric(swe_winmargin_vec_gamestate_h)

  swe_winmargin_vec_gamestate_a <- as.vector(swe_winmargin_h[swe_awayteamindex_wm,])
  swe_winmargin_vec_gamestate_a[is.na(swe_winmargin_vec_gamestate_a)] <- ""
  swe_winmargin_vec_gamestate_a <- swe_winmargin_vec_gamestate_a[swe_winmargin_vec_gamestate_a != ""]
  swe_winmargin_vec_gamestate_a <- as.numeric(swe_winmargin_vec_gamestate_a)

  for (swe_game_no in 1:swe_games_played[1])

  {

    swe_homewinmargin[swe_secondsplitrow] <- swe_winmargin_vec_gamestate_h[swe_hometeam_game_no]
    swe_awaywinmargin[swe_secondsplitrow] <- swe_winmargin_vec_gamestate_a[swe_awayteam_game_no]


  }

}

swe_homewinmargin
swe_awaywinmargin

SWE_gamestate_data <- cbind(SWE_secondsplit,swe_homewinmargin,swe_awaywinmargin)
SWE_gamestate_data
SWE_gamestate_data$swe_GSCS <- paste(SWE_gamestate_data$swe_homewinmargin,SWE_gamestate_data$swe_awaywinmargin, sep = ",")
SWE_gamestate_data$swe_GSH <- SWE_gamestate_data$swe_homewinmargin - SWE_gamestate_data$swe_awaywinmargin
head(SWE_gamestate_data,5)
SWE_gamestate_copy <- SWE_gamestate_data
SWE_gamestate_copy$swe_HomeTeam_index_wm <- NULL
SWE_gamestate_copy$swe_AwayTeam_index_wm <- NULL
SWE_gamestate_copy$swe_homegame_no <- NULL
SWE_gamestate_copy$swe_awaygame_no <- NULL
head(SWE_gamestate_copy)
colnames(SWE_gamestate_copy)[22] <- "homewinmargin"
colnames(SWE_gamestate_copy)[23] <- "awaywinmargin"
colnames(SWE_gamestate_copy)[24] <- "GSCS"
colnames(SWE_gamestate_copy)[25] <- "GSH"
SWE_gamestate_copy$CS <- paste(SWE_gamestate_copy$HG,SWE_gamestate_copy$AG, sep = "-")
SWE_gamestate_copy
#end SWE
###################################################################################################
###################################################################################################
#SWZ
SWZ_margindata <- SWZ
swz_firstgames <- length(swz_teams)/2
SWZ_firstsplit <-  head(SWZ_margindata,swz_firstgames)
SWZ_secondsplit <- tail(SWZ_margindata,nrow(SWZ_margindata) - swz_firstgames)
SWZ_secondsplit$swz_HomeTeam_index_wm <- match(SWZ_secondsplit$Home,swz_teams)
SWZ_secondsplit$swz_AwayTeam_index_wm <- match(SWZ_secondsplit$Away,swz_teams)
SWZ_secondsplit$swz_homegame_no <- rep(2:max(swz_games_played[]) - 1,each = length(swz_teams)/2,length.out = nrow(SWZ_secondsplit))
SWZ_secondsplit$swz_awaygame_no <- rep(2:max(swz_games_played[]) - 1,each = length(swz_teams)/2,length.out = nrow(SWZ_secondsplit))

swz_homewinmargin <- c()
swz_awaywinmargin <- c()

for (swz_secondsplitrow in 1:nrow(SWZ_secondsplit))
{

  swz_hometeamindex_wm <- SWZ_secondsplit[swz_secondsplitrow,"swz_HomeTeam_index_wm"]
  swz_awayteamindex_wm <- SWZ_secondsplit[swz_secondsplitrow,"swz_AwayTeam_index_wm"]
  swz_hometeam_game_no <- SWZ_secondsplit[swz_secondsplitrow,"swz_homegame_no"]
  swz_awayteam_game_no <- SWZ_secondsplit[swz_secondsplitrow,"swz_awaygame_no"]

  swz_winmargin_vec_gamestate_h <- as.vector(swz_winmargin_h[swz_hometeamindex_wm,])
  swz_winmargin_vec_gamestate_h[is.na(swz_winmargin_vec_gamestate_h)] <- ""
  swz_winmargin_vec_gamestate_h <- swz_winmargin_vec_gamestate_h[swz_winmargin_vec_gamestate_h != ""]
  swz_winmargin_vec_gamestate_h <- as.numeric(swz_winmargin_vec_gamestate_h)

  swz_winmargin_vec_gamestate_a <- as.vector(swz_winmargin_h[swz_awayteamindex_wm,])
  swz_winmargin_vec_gamestate_a[is.na(swz_winmargin_vec_gamestate_a)] <- ""
  swz_winmargin_vec_gamestate_a <- swz_winmargin_vec_gamestate_a[swz_winmargin_vec_gamestate_a != ""]
  swz_winmargin_vec_gamestate_a <- as.numeric(swz_winmargin_vec_gamestate_a)

  for (swz_game_no in 1:swz_games_played[1])

  {

    swz_homewinmargin[swz_secondsplitrow] <- swz_winmargin_vec_gamestate_h[swz_hometeam_game_no]
    swz_awaywinmargin[swz_secondsplitrow] <- swz_winmargin_vec_gamestate_a[swz_awayteam_game_no]


  }

}

swz_homewinmargin
swz_awaywinmargin

SWZ_gamestate_data <- cbind(SWZ_secondsplit,swz_homewinmargin,swz_awaywinmargin)
SWZ_gamestate_data
SWZ_gamestate_data$swz_GSCS <- paste(SWZ_gamestate_data$swz_homewinmargin,SWZ_gamestate_data$swz_awaywinmargin, sep = ",")
SWZ_gamestate_data$swz_GSH <- SWZ_gamestate_data$swz_homewinmargin - SWZ_gamestate_data$swz_awaywinmargin
head(SWZ_gamestate_data,5)
SWZ_gamestate_copy <- SWZ_gamestate_data
SWZ_gamestate_copy$swz_HomeTeam_index_wm <- NULL
SWZ_gamestate_copy$swz_AwayTeam_index_wm <- NULL
SWZ_gamestate_copy$swz_homegame_no <- NULL
SWZ_gamestate_copy$swz_awaygame_no <- NULL
head(SWZ_gamestate_copy)
colnames(SWZ_gamestate_copy)[22] <- "homewinmargin"
colnames(SWZ_gamestate_copy)[23] <- "awaywinmargin"
colnames(SWZ_gamestate_copy)[24] <- "GSCS"
colnames(SWZ_gamestate_copy)[25] <- "GSH"
SWZ_gamestate_copy$CS <- paste(SWZ_gamestate_copy$HG,SWZ_gamestate_copy$AG, sep = "-")
SWZ_gamestate_copy
#end SWZ
###################################################################################################
###################################################################################################
#join the dataframes
###################################################################################################
###################################################################################################

allteams20212022_gamestate_nl <- rbind(AUT_gamestate_copy,ARG_gamestate_copy,BRA_gamestate_copy,CHN_gamestate_copy,DNK_gamestate_copy,FIN_gamestate_copy,
                                    IRL_gamestate_copy,JPN_gamestate_copy,MEX_gamestate_copy,MLS_gamestate_copy,NOR_gamestate_copy,POL_gamestate_copy,ROU_gamestate_copy,RUS_gamestate_copy,
                                    SWE_gamestate_copy,SWZ_gamestate_copy)

nrow(allteams20212022_gamestate)
allteams20212022_gamestate_nl

BRA_gamestate_copy



































