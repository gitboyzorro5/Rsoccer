Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')

B1_rounds
#####################################################################################################################
#begin gamesate algortihm
#B1
B1_margindata <- B1_rounds
b1_firstgames <- length(b1_teams)/2
B1_firstsplit <-  head(B1_margindata,b1_firstgames)
B1_secondsplit <- tail(B1_margindata,nrow(B1_margindata) - b1_firstgames)
B1_secondsplit$b1_HomeTeam_index_wm <- match(B1_secondsplit$HomeTeam,b1_teams)
B1_secondsplit$b1_AwayTeam_index_wm <- match(B1_secondsplit$AwayTeam,b1_teams)
B1_secondsplit$b1_homegame_no <- rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)
B1_secondsplit$b1_awaygame_no <- rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)

# B1_secondsplit$b1_homegame_no <- rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)
# B1_secondsplit$b1_awaygame_no <- rep(2:b1_games_played[1] - 1, each = length(b1_teams)/2)

b1_homewinmargin <- c()
b1_awaywinmargin <- c()

for (b1_secondsplitrow in 1:nrow(B1_secondsplit))
{

  b1_hometeamindex_wm <- B1_secondsplit[b1_secondsplitrow,"b1_HomeTeam_index_wm"]
  b1_awayteamindex_wm <- B1_secondsplit[b1_secondsplitrow,"b1_AwayTeam_index_wm"]
  b1_hometeam_game_no <- B1_secondsplit[b1_secondsplitrow,"b1_homegame_no"]
  b1_awayteam_game_no <- B1_secondsplit[b1_secondsplitrow,"b1_awaygame_no"]

  b1_winmargin_vec_gamestate_h <- as.vector(b1_winmargin_h[b1_hometeamindex_wm,])
  b1_winmargin_vec_gamestate_h[is.na(b1_winmargin_vec_gamestate_h)] <- ""
  b1_winmargin_vec_gamestate_h <- b1_winmargin_vec_gamestate_h[b1_winmargin_vec_gamestate_h != ""]
  b1_winmargin_vec_gamestate_h <- as.numeric(b1_winmargin_vec_gamestate_h)

  b1_winmargin_vec_gamestate_a <- as.vector(b1_winmargin_h[b1_awayteamindex_wm,])
  b1_winmargin_vec_gamestate_a[is.na(b1_winmargin_vec_gamestate_a)] <- ""
  b1_winmargin_vec_gamestate_a <- b1_winmargin_vec_gamestate_a[b1_winmargin_vec_gamestate_a != ""]
  b1_winmargin_vec_gamestate_a <- as.numeric(b1_winmargin_vec_gamestate_a)

  for (b1_game_no in 1:b1_games_played[1])

  {

    b1_homewinmargin[b1_secondsplitrow] <- b1_winmargin_vec_gamestate_h[b1_hometeam_game_no]
    b1_awaywinmargin[b1_secondsplitrow] <- b1_winmargin_vec_gamestate_a[b1_awayteam_game_no]


  }

}

b1_homewinmargin
b1_awaywinmargin

B1_gamestate_data <- cbind(B1_secondsplit,b1_homewinmargin,b1_awaywinmargin)
B1_gamestate_data$b1_GSCS <- paste(B1_gamestate_data$b1_homewinmargin,B1_gamestate_data$b1_awaywinmargin, sep = ",")
B1_gamestate_data$b1_GSH <- B1_gamestate_data$b1_homewinmargin - B1_gamestate_data$b1_awaywinmargin
head(B1_gamestate_data,5)
B1_gamestate_copy <- B1_gamestate_data
B1_gamestate_copy$b1_HomeTeam_index_wm <- NULL
B1_gamestate_copy$b1_AwayTeam_index_wm <- NULL
B1_gamestate_copy$b1_homegame_no <- NULL
B1_gamestate_copy$b1_awaygame_no <- NULL
colnames(B1_gamestate_copy)[30] <- "homewinmargin"
colnames(B1_gamestate_copy)[31] <- "awaywinmargin"
colnames(B1_gamestate_copy)[32] <- "GSCS"
colnames(B1_gamestate_copy)[33] <- "GSH"
#end B1
###################################################################################################
###################################################################################################
#D1
D1_margindata <- D1
d1_firstgames <- length(d1_teams)/2
D1_firstsplit <-  head(D1_margindata,d1_firstgames)
D1_secondsplit <- tail(D1_margindata,nrow(D1_margindata) - d1_firstgames)
D1_secondsplit$d1_HomeTeam_index_wm <- match(D1_secondsplit$HomeTeam,d1_teams)
D1_secondsplit$d1_AwayTeam_index_wm <- match(D1_secondsplit$AwayTeam,d1_teams)
D1_secondsplit$d1_homegame_no <- rep(2:d1_games_played[1] - 1, each = length(d1_teams)/2)
D1_secondsplit$d1_awaygame_no <- rep(2:d1_games_played[1] - 1, each = length(d1_teams)/2)

d1_homewinmargin <- c()
d1_awaywinmargin <- c()

for (d1_secondsplitrow in 1:nrow(D1_secondsplit))
{

  d1_hometeamindex_wm <- D1_secondsplit[d1_secondsplitrow,"d1_HomeTeam_index_wm"]
  d1_awayteamindex_wm <- D1_secondsplit[d1_secondsplitrow,"d1_AwayTeam_index_wm"]
  d1_hometeam_game_no <- D1_secondsplit[d1_secondsplitrow,"d1_homegame_no"]
  d1_awayteam_game_no <- D1_secondsplit[d1_secondsplitrow,"d1_awaygame_no"]

  d1_winmargin_vec_gamestate_h <- as.vector(d1_winmargin_h[d1_hometeamindex_wm,])
  d1_winmargin_vec_gamestate_h[is.na(d1_winmargin_vec_gamestate_h)] <- ""
  d1_winmargin_vec_gamestate_h <- d1_winmargin_vec_gamestate_h[d1_winmargin_vec_gamestate_h != ""]
  d1_winmargin_vec_gamestate_h <- as.numeric(d1_winmargin_vec_gamestate_h)

  d1_winmargin_vec_gamestate_a <- as.vector(d1_winmargin_h[d1_awayteamindex_wm,])
  d1_winmargin_vec_gamestate_a[is.na(d1_winmargin_vec_gamestate_a)] <- ""
  d1_winmargin_vec_gamestate_a <- d1_winmargin_vec_gamestate_a[d1_winmargin_vec_gamestate_a != ""]
  d1_winmargin_vec_gamestate_a <- as.numeric(d1_winmargin_vec_gamestate_a)

  for (d1_game_no in 1:d1_games_played[1])

  {

    d1_homewinmargin[d1_secondsplitrow] <- d1_winmargin_vec_gamestate_h[d1_hometeam_game_no]
    d1_awaywinmargin[d1_secondsplitrow] <- d1_winmargin_vec_gamestate_a[d1_awayteam_game_no]


  }

}

d1_homewinmargin
d1_awaywinmargin

D1_gamestate_data <- cbind(D1_secondsplit,d1_homewinmargin,d1_awaywinmargin)
D1_gamestate_data$d1_GSCS <- paste(D1_gamestate_data$d1_homewinmargin,D1_gamestate_data$d1_awaywinmargin, sep = ",")
D1_gamestate_data$d1_GSH <- D1_gamestate_data$d1_homewinmargin - D1_gamestate_data$d1_awaywinmargin
head(D1_gamestate_data,5)
D1_gamestate_copy <- D1_gamestate_data
D1_gamestate_copy$d1_HomeTeam_index_wm <- NULL
D1_gamestate_copy$d1_AwayTeam_index_wm <- NULL
D1_gamestate_copy$d1_homegame_no <- NULL
D1_gamestate_copy$d1_awaygame_no <- NULL
colnames(D1_gamestate_copy)[30] <- "homewinmargin"
colnames(D1_gamestate_copy)[31] <- "awaywinmargin"
colnames(D1_gamestate_copy)[32] <- "GSCS"
colnames(D1_gamestate_copy)[33] <- "GSH"
#end D1
###################################################################################################
###################################################################################################
#D2
D2_margindata <- D2
d2_firstgames <- length(d2_teams)/2
D2_firstsplit <-  head(D2_margindata,d2_firstgames)
D2_secondsplit <- tail(D2_margindata,nrow(D2_margindata) - d2_firstgames)
D2_secondsplit$d2_HomeTeam_index_wm <- match(D2_secondsplit$HomeTeam,d2_teams)
D2_secondsplit$d2_AwayTeam_index_wm <- match(D2_secondsplit$AwayTeam,d2_teams)
D2_secondsplit$d2_homegame_no <- rep(2:max(d2_games_played[]) - 1,each = length(d2_teams)/2,length.out = nrow(D2_secondsplit))
D2_secondsplit$d2_awaygame_no <- rep(2:max(d2_games_played[]) - 1,each = length(d2_teams)/2,length.out = nrow(D2_secondsplit))

d2_homewinmargin <- c()
d2_awaywinmargin <- c()

for (d2_secondsplitrow in 1:nrow(D2_secondsplit))
{

  d2_hometeamindex_wm <- D2_secondsplit[d2_secondsplitrow,"d2_HomeTeam_index_wm"]
  d2_awayteamindex_wm <- D2_secondsplit[d2_secondsplitrow,"d2_AwayTeam_index_wm"]
  d2_hometeam_game_no <- D2_secondsplit[d2_secondsplitrow,"d2_homegame_no"]
  d2_awayteam_game_no <- D2_secondsplit[d2_secondsplitrow,"d2_awaygame_no"]

  d2_winmargin_vec_gamestate_h <- as.vector(d2_winmargin_h[d2_hometeamindex_wm,])
  d2_winmargin_vec_gamestate_h[is.na(d2_winmargin_vec_gamestate_h)] <- ""
  d2_winmargin_vec_gamestate_h <- d2_winmargin_vec_gamestate_h[d2_winmargin_vec_gamestate_h != ""]
  d2_winmargin_vec_gamestate_h <- as.numeric(d2_winmargin_vec_gamestate_h)

  d2_winmargin_vec_gamestate_a <- as.vector(d2_winmargin_h[d2_awayteamindex_wm,])
  d2_winmargin_vec_gamestate_a[is.na(d2_winmargin_vec_gamestate_a)] <- ""
  d2_winmargin_vec_gamestate_a <- d2_winmargin_vec_gamestate_a[d2_winmargin_vec_gamestate_a != ""]
  d2_winmargin_vec_gamestate_a <- as.numeric(d2_winmargin_vec_gamestate_a)

  for (d2_game_no in 1:d2_games_played[1])

  {

    d2_homewinmargin[d2_secondsplitrow] <- d2_winmargin_vec_gamestate_h[d2_hometeam_game_no]
    d2_awaywinmargin[d2_secondsplitrow] <- d2_winmargin_vec_gamestate_a[d2_awayteam_game_no]


  }

}

d2_homewinmargin
d2_awaywinmargin

D2_gamestate_data <- cbind(D2_secondsplit,d2_homewinmargin,d2_awaywinmargin)
D2_gamestate_data$d2_GSCS <- paste(D2_gamestate_data$d2_homewinmargin,D2_gamestate_data$d2_awaywinmargin, sep = ",")
D2_gamestate_data$d2_GSH <- D2_gamestate_data$d2_homewinmargin - D2_gamestate_data$d2_awaywinmargin
head(D2_gamestate_data,5)
D2_gamestate_copy <- D2_gamestate_data
D2_gamestate_copy$d2_HomeTeam_index_wm <- NULL
D2_gamestate_copy$d2_AwayTeam_index_wm <- NULL
D2_gamestate_copy$d2_homegame_no <- NULL
D2_gamestate_copy$d2_awaygame_no <- NULL
colnames(D2_gamestate_copy)[30] <- "homewinmargin"
colnames(D2_gamestate_copy)[31] <- "awaywinmargin"
colnames(D2_gamestate_copy)[32] <- "GSCS"
colnames(D2_gamestate_copy)[33] <- "GSH"
#end D2
###################################################################################################
#E0
E0_margindata <- E0
e0_firstgames <- length(e0_teams)/2
E0_firstsplit <-  head(E0_margindata,e0_firstgames)
E0_secondsplit <- tail(E0_margindata,nrow(E0_margindata) - e0_firstgames)
E0_secondsplit$e0_HomeTeam_index_wm <- match(E0_secondsplit$HomeTeam,e0_teams)
E0_secondsplit$e0_AwayTeam_index_wm <- match(E0_secondsplit$AwayTeam,e0_teams)
E0_secondsplit$e0_homegame_no <- rep(2:e0_games_played[1] - 1, each = length(e0_teams)/2)
E0_secondsplit$e0_awaygame_no <- rep(2:e0_games_played[1] - 1, each = length(e0_teams)/2)

e0_homewinmargin <- c()
e0_awaywinmargin <- c()

for (e0_secondsplitrow in 1:nrow(E0_secondsplit))
{

  e0_hometeamindex_wm <- E0_secondsplit[e0_secondsplitrow,"e0_HomeTeam_index_wm"]
  e0_awayteamindex_wm <- E0_secondsplit[e0_secondsplitrow,"e0_AwayTeam_index_wm"]
  e0_hometeam_game_no <- E0_secondsplit[e0_secondsplitrow,"e0_homegame_no"]
  e0_awayteam_game_no <- E0_secondsplit[e0_secondsplitrow,"e0_awaygame_no"]

  e0_winmargin_vec_gamestate_h <- as.vector(e0_winmargin_h[e0_hometeamindex_wm,])
  e0_winmargin_vec_gamestate_h[is.na(e0_winmargin_vec_gamestate_h)] <- ""
  e0_winmargin_vec_gamestate_h <- e0_winmargin_vec_gamestate_h[e0_winmargin_vec_gamestate_h != ""]
  e0_winmargin_vec_gamestate_h <- as.numeric(e0_winmargin_vec_gamestate_h)

  e0_winmargin_vec_gamestate_a <- as.vector(e0_winmargin_h[e0_awayteamindex_wm,])
  e0_winmargin_vec_gamestate_a[is.na(e0_winmargin_vec_gamestate_a)] <- ""
  e0_winmargin_vec_gamestate_a <- e0_winmargin_vec_gamestate_a[e0_winmargin_vec_gamestate_a != ""]
  e0_winmargin_vec_gamestate_a <- as.numeric(e0_winmargin_vec_gamestate_a)

  for (e0_game_no in 1:e0_games_played[1])

  {

    e0_homewinmargin[e0_secondsplitrow] <- e0_winmargin_vec_gamestate_h[e0_hometeam_game_no]
    e0_awaywinmargin[e0_secondsplitrow] <- e0_winmargin_vec_gamestate_a[e0_awayteam_game_no]


  }

}

e0_homewinmargin
e0_awaywinmargin

E0_gamestate_data <- cbind(E0_secondsplit,e0_homewinmargin,e0_awaywinmargin)
E0_gamestate_data$e0_GSCS <- paste(E0_gamestate_data$e0_homewinmargin,E0_gamestate_data$e0_awaywinmargin, sep = ",")
E0_gamestate_data$e0_GSH <- E0_gamestate_data$e0_homewinmargin - E0_gamestate_data$e0_awaywinmargin
head(E0_gamestate_data,5)
E0_gamestate_copy <- E0_gamestate_data
E0_gamestate_copy$e0_HomeTeam_index_wm <- NULL
E0_gamestate_copy$e0_AwayTeam_index_wm <- NULL
E0_gamestate_copy$e0_homegame_no <- NULL
E0_gamestate_copy$e0_awaygame_no <- NULL
colnames(E0_gamestate_copy)[30] <- "homewinmargin"
colnames(E0_gamestate_copy)[31] <- "awaywinmargin"
colnames(E0_gamestate_copy)[32] <- "GSCS"
colnames(E0_gamestate_copy)[33] <- "GSH"
#end E0
####################################################################################################################
#E1
E1_margindata <- E1
e1_firstgames <- length(e1_teams)/2
E1_firstsplit <-  head(E1_margindata,e1_firstgames)
E1_secondsplit <- tail(E1_margindata,nrow(E1_margindata) - e1_firstgames)
E1_secondsplit$e1_HomeTeam_index_wm <- match(E1_secondsplit$HomeTeam,e1_teams)
E1_secondsplit$e1_AwayTeam_index_wm <- match(E1_secondsplit$AwayTeam,e1_teams)
E1_secondsplit$e1_homegame_no <- rep(2:e1_games_played[1] - 1, each = length(e1_teams)/2)
E1_secondsplit$e1_awaygame_no <- rep(2:e1_games_played[1] - 1, each = length(e1_teams)/2)

e1_homewinmargin <- c()
e1_awaywinmargin <- c()

for (e1_secondsplitrow in 1:nrow(E1_secondsplit))
{

  e1_hometeamindex_wm <- E1_secondsplit[e1_secondsplitrow,"e1_HomeTeam_index_wm"]
  e1_awayteamindex_wm <- E1_secondsplit[e1_secondsplitrow,"e1_AwayTeam_index_wm"]
  e1_hometeam_game_no <- E1_secondsplit[e1_secondsplitrow,"e1_homegame_no"]
  e1_awayteam_game_no <- E1_secondsplit[e1_secondsplitrow,"e1_awaygame_no"]

  e1_winmargin_vec_gamestate_h <- as.vector(e1_winmargin_h[e1_hometeamindex_wm,])
  e1_winmargin_vec_gamestate_h[is.na(e1_winmargin_vec_gamestate_h)] <- ""
  e1_winmargin_vec_gamestate_h <- e1_winmargin_vec_gamestate_h[e1_winmargin_vec_gamestate_h != ""]
  e1_winmargin_vec_gamestate_h <- as.numeric(e1_winmargin_vec_gamestate_h)

  e1_winmargin_vec_gamestate_a <- as.vector(e1_winmargin_h[e1_awayteamindex_wm,])
  e1_winmargin_vec_gamestate_a[is.na(e1_winmargin_vec_gamestate_a)] <- ""
  e1_winmargin_vec_gamestate_a <- e1_winmargin_vec_gamestate_a[e1_winmargin_vec_gamestate_a != ""]
  e1_winmargin_vec_gamestate_a <- as.numeric(e1_winmargin_vec_gamestate_a)

  for (e1_game_no in 1:e1_games_played[1])

  {

    e1_homewinmargin[e1_secondsplitrow] <- e1_winmargin_vec_gamestate_h[e1_hometeam_game_no]
    e1_awaywinmargin[e1_secondsplitrow] <- e1_winmargin_vec_gamestate_a[e1_awayteam_game_no]


  }

}

e1_homewinmargin
e1_awaywinmargin

E1_gamestate_data <- cbind(E1_secondsplit,e1_homewinmargin,e1_awaywinmargin)
E1_gamestate_data$e1_GSCS <- paste(E1_gamestate_data$e1_homewinmargin,E1_gamestate_data$e1_awaywinmargin, sep = ",")
E1_gamestate_data$e1_GSH <- E1_gamestate_data$e1_homewinmargin - E1_gamestate_data$e1_awaywinmargin
head(E1_gamestate_data,5)
E1_gamestate_copy <- E1_gamestate_data
E1_gamestate_copy$e1_HomeTeam_index_wm <- NULL
E1_gamestate_copy$e1_AwayTeam_index_wm <- NULL
E1_gamestate_copy$e1_homegame_no <- NULL
E1_gamestate_copy$e1_awaygame_no <- NULL
colnames(E1_gamestate_copy)[30] <- "homewinmargin"
colnames(E1_gamestate_copy)[31] <- "awaywinmargin"
colnames(E1_gamestate_copy)[32] <- "GSCS"
colnames(E1_gamestate_copy)[33] <- "GSH"
#end E1
###################################################################################################
#E2
E2_margindata <- E2
e2_firstgames <- length(e2_teams)/2
E2_firstsplit <-  head(E2_margindata,e2_firstgames)
E2_secondsplit <- tail(E2_margindata,nrow(E2_margindata) - e2_firstgames)
E2_secondsplit$e2_HomeTeam_index_wm <- match(E2_secondsplit$HomeTeam,e2_teams)
E2_secondsplit$e2_AwayTeam_index_wm <- match(E2_secondsplit$AwayTeam,e2_teams)
E2_secondsplit$e2_homegame_no <- rep(2:max(e2_games_played[]) - 1,each = length(e2_teams)/2,length.out = nrow(E2_secondsplit))
E2_secondsplit$e2_awaygame_no <- rep(2:max(e2_games_played[]) - 1,each = length(e2_teams)/2,length.out = nrow(E2_secondsplit))

e2_homewinmargin <- c()
e2_awaywinmargin <- c()

for (e2_secondsplitrow in 1:nrow(E2_secondsplit))
{

  e2_hometeamindex_wm <- E2_secondsplit[e2_secondsplitrow,"e2_HomeTeam_index_wm"]
  e2_awayteamindex_wm <- E2_secondsplit[e2_secondsplitrow,"e2_AwayTeam_index_wm"]
  e2_hometeam_game_no <- E2_secondsplit[e2_secondsplitrow,"e2_homegame_no"]
  e2_awayteam_game_no <- E2_secondsplit[e2_secondsplitrow,"e2_awaygame_no"]

  e2_winmargin_vec_gamestate_h <- as.vector(e2_winmargin_h[e2_hometeamindex_wm,])
  e2_winmargin_vec_gamestate_h[is.na(e2_winmargin_vec_gamestate_h)] <- ""
  e2_winmargin_vec_gamestate_h <- e2_winmargin_vec_gamestate_h[e2_winmargin_vec_gamestate_h != ""]
  e2_winmargin_vec_gamestate_h <- as.numeric(e2_winmargin_vec_gamestate_h)

  e2_winmargin_vec_gamestate_a <- as.vector(e2_winmargin_h[e2_awayteamindex_wm,])
  e2_winmargin_vec_gamestate_a[is.na(e2_winmargin_vec_gamestate_a)] <- ""
  e2_winmargin_vec_gamestate_a <- e2_winmargin_vec_gamestate_a[e2_winmargin_vec_gamestate_a != ""]
  e2_winmargin_vec_gamestate_a <- as.numeric(e2_winmargin_vec_gamestate_a)

  for (e2_game_no in 1:e2_games_played[1])

  {

    e2_homewinmargin[e2_secondsplitrow] <- e2_winmargin_vec_gamestate_h[e2_hometeam_game_no]
    e2_awaywinmargin[e2_secondsplitrow] <- e2_winmargin_vec_gamestate_a[e2_awayteam_game_no]


  }

}

e2_homewinmargin
e2_awaywinmargin

E2_gamestate_data <- cbind(E2_secondsplit,e2_homewinmargin,e2_awaywinmargin)
E2_gamestate_data$e2_GSCS <- paste(E2_gamestate_data$e2_homewinmargin,E2_gamestate_data$e2_awaywinmargin, sep = ",")
E2_gamestate_data$e2_GSH <- E2_gamestate_data$e2_homewinmargin - E2_gamestate_data$e2_awaywinmargin
head(E2_gamestate_data,5)
E2_gamestate_copy <- E2_gamestate_data
E2_gamestate_copy$e2_HomeTeam_index_wm <- NULL
E2_gamestate_copy$e2_AwayTeam_index_wm <- NULL
E2_gamestate_copy$e2_homegame_no <- NULL
E2_gamestate_copy$e2_awaygame_no <- NULL
colnames(E2_gamestate_copy)[30] <- "homewinmargin"
colnames(E2_gamestate_copy)[31] <- "awaywinmargin"
colnames(E2_gamestate_copy)[32] <- "GSCS"
colnames(E2_gamestate_copy)[33] <- "GSH"
#end E2
###################################################################################################
#E3
E3_margindata <- E3
e3_firstgames <- length(e3_teams)/2
E3_firstsplit <-  head(E3_margindata,e3_firstgames)
E3_secondsplit <- tail(E3_margindata,nrow(E3_margindata) - e3_firstgames)
E3_secondsplit$e3_HomeTeam_index_wm <- match(E3_secondsplit$HomeTeam,e3_teams)
E3_secondsplit$e3_AwayTeam_index_wm <- match(E3_secondsplit$AwayTeam,e3_teams)
E3_secondsplit$e3_homegame_no <- rep(2:max(e3_games_played[]) - 1,each = length(e3_teams)/2,length.out = nrow(E3_secondsplit))
E3_secondsplit$e3_awaygame_no <- rep(2:max(e3_games_played[]) - 1,each = length(e3_teams)/2,length.out = nrow(E3_secondsplit))


e3_homewinmargin <- c()
e3_awaywinmargin <- c()

for (e3_secondsplitrow in 1:nrow(E3_secondsplit))
{

  e3_hometeamindex_wm <- E3_secondsplit[e3_secondsplitrow,"e3_HomeTeam_index_wm"]
  e3_awayteamindex_wm <- E3_secondsplit[e3_secondsplitrow,"e3_AwayTeam_index_wm"]
  e3_hometeam_game_no <- E3_secondsplit[e3_secondsplitrow,"e3_homegame_no"]
  e3_awayteam_game_no <- E3_secondsplit[e3_secondsplitrow,"e3_awaygame_no"]

  e3_winmargin_vec_gamestate_h <- as.vector(e3_winmargin_h[e3_hometeamindex_wm,])
  e3_winmargin_vec_gamestate_h[is.na(e3_winmargin_vec_gamestate_h)] <- ""
  e3_winmargin_vec_gamestate_h <- e3_winmargin_vec_gamestate_h[e3_winmargin_vec_gamestate_h != ""]
  e3_winmargin_vec_gamestate_h <- as.numeric(e3_winmargin_vec_gamestate_h)

  e3_winmargin_vec_gamestate_a <- as.vector(e3_winmargin_h[e3_awayteamindex_wm,])
  e3_winmargin_vec_gamestate_a[is.na(e3_winmargin_vec_gamestate_a)] <- ""
  e3_winmargin_vec_gamestate_a <- e3_winmargin_vec_gamestate_a[e3_winmargin_vec_gamestate_a != ""]
  e3_winmargin_vec_gamestate_a <- as.numeric(e3_winmargin_vec_gamestate_a)

  for (e3_game_no in 1:e3_games_played[1])

  {

    e3_homewinmargin[e3_secondsplitrow] <- e3_winmargin_vec_gamestate_h[e3_hometeam_game_no]
    e3_awaywinmargin[e3_secondsplitrow] <- e3_winmargin_vec_gamestate_a[e3_awayteam_game_no]


  }

}

e3_homewinmargin
e3_awaywinmargin

E3_gamestate_data <- cbind(E3_secondsplit,e3_homewinmargin,e3_awaywinmargin)
E3_gamestate_data$e3_GSCS <- paste(E3_gamestate_data$e3_homewinmargin,E3_gamestate_data$e3_awaywinmargin, sep = ",")
E3_gamestate_data$e3_GSH <- E3_gamestate_data$e3_homewinmargin - E3_gamestate_data$e3_awaywinmargin
head(E3_gamestate_data,5)
E3_gamestate_copy <- E3_gamestate_data
E3_gamestate_copy$e3_HomeTeam_index_wm <- NULL
E3_gamestate_copy$e3_AwayTeam_index_wm <- NULL
E3_gamestate_copy$e3_homegame_no <- NULL
E3_gamestate_copy$e3_awaygame_no <- NULL
colnames(E3_gamestate_copy)[30] <- "homewinmargin"
colnames(E3_gamestate_copy)[31] <- "awaywinmargin"
colnames(E3_gamestate_copy)[32] <- "GSCS"
colnames(E3_gamestate_copy)[33] <- "GSH"
#end E3
###################################################################################################
#EC
EC_margindata <- EC
ec_firstgames <- length(ec_teams)/2
EC_firstsplit <-  head(EC_margindata,ec_firstgames)
EC_secondsplit <- tail(EC_margindata,nrow(EC_margindata) - ec_firstgames)
EC_secondsplit$ec_HomeTeam_index_wm <- match(EC_secondsplit$HomeTeam,ec_teams)
EC_secondsplit$ec_AwayTeam_index_wm <- match(EC_secondsplit$AwayTeam,ec_teams)
EC_secondsplit$ec_homegame_no <- rep(2:max(ec_games_played[]) - 1,each = length(ec_teams)/2,length.out = nrow(EC_secondsplit))
EC_secondsplit$ec_awaygame_no <- rep(2:max(ec_games_played[]) - 1,each = length(ec_teams)/2,length.out = nrow(EC_secondsplit))


ec_homewinmargin <- c()
ec_awaywinmargin <- c()

for (ec_secondsplitrow in 1:nrow(EC_secondsplit))
{

  ec_hometeamindex_wm <- EC_secondsplit[ec_secondsplitrow,"ec_HomeTeam_index_wm"]
  ec_awayteamindex_wm <- EC_secondsplit[ec_secondsplitrow,"ec_AwayTeam_index_wm"]
  ec_hometeam_game_no <- EC_secondsplit[ec_secondsplitrow,"ec_homegame_no"]
  ec_awayteam_game_no <- EC_secondsplit[ec_secondsplitrow,"ec_awaygame_no"]

  ec_winmargin_vec_gamestate_h <- as.vector(ec_winmargin_h[ec_hometeamindex_wm,])
  ec_winmargin_vec_gamestate_h[is.na(ec_winmargin_vec_gamestate_h)] <- ""
  ec_winmargin_vec_gamestate_h <- ec_winmargin_vec_gamestate_h[ec_winmargin_vec_gamestate_h != ""]
  ec_winmargin_vec_gamestate_h <- as.numeric(ec_winmargin_vec_gamestate_h)

  ec_winmargin_vec_gamestate_a <- as.vector(ec_winmargin_h[ec_awayteamindex_wm,])
  ec_winmargin_vec_gamestate_a[is.na(ec_winmargin_vec_gamestate_a)] <- ""
  ec_winmargin_vec_gamestate_a <- ec_winmargin_vec_gamestate_a[ec_winmargin_vec_gamestate_a != ""]
  ec_winmargin_vec_gamestate_a <- as.numeric(ec_winmargin_vec_gamestate_a)

  for (ec_game_no in 1:ec_games_played[1])

  {

    ec_homewinmargin[ec_secondsplitrow] <- ec_winmargin_vec_gamestate_h[ec_hometeam_game_no]
    ec_awaywinmargin[ec_secondsplitrow] <- ec_winmargin_vec_gamestate_a[ec_awayteam_game_no]


  }

}

ec_homewinmargin
ec_awaywinmargin

EC_gamestate_data <- cbind(EC_secondsplit,ec_homewinmargin,ec_awaywinmargin)
EC_gamestate_data$ec_GSCS <- paste(EC_gamestate_data$ec_homewinmargin,EC_gamestate_data$ec_awaywinmargin, sep = ",")
EC_gamestate_data$ec_GSH <- EC_gamestate_data$ec_homewinmargin - EC_gamestate_data$ec_awaywinmargin
head(EC_gamestate_data,5)
EC_gamestate_copy <- EC_gamestate_data
EC_gamestate_copy$ec_HomeTeam_index_wm <- NULL
EC_gamestate_copy$ec_AwayTeam_index_wm <- NULL
EC_gamestate_copy$ec_homegame_no <- NULL
EC_gamestate_copy$ec_awaygame_no <- NULL
colnames(EC_gamestate_copy)[30] <- "homewinmargin"
colnames(EC_gamestate_copy)[31] <- "awaywinmargin"
colnames(EC_gamestate_copy)[32] <- "GSCS"
colnames(EC_gamestate_copy)[33] <- "GSH"
#end EC
###################################################################################################
#F1
F1_margindata <- F1
f1_firstgames <- length(f1_teams)/2
F1_firstsplit <-  head(F1_margindata,f1_firstgames)
F1_secondsplit <- tail(F1_margindata,nrow(F1_margindata) - f1_firstgames)
F1_secondsplit$f1_HomeTeam_index_wm <- match(F1_secondsplit$HomeTeam,f1_teams)
F1_secondsplit$f1_AwayTeam_index_wm <- match(F1_secondsplit$AwayTeam,f1_teams)
F1_secondsplit$f1_homegame_no <- rep(2:f1_games_played[1] - 1, each = length(f1_teams)/2)
F1_secondsplit$f1_awaygame_no <- rep(2:f1_games_played[1] - 1, each = length(f1_teams)/2)

f1_homewinmargin <- c()
f1_awaywinmargin <- c()

for (f1_secondsplitrow in 1:nrow(F1_secondsplit))
{

  f1_hometeamindex_wm <- F1_secondsplit[f1_secondsplitrow,"f1_HomeTeam_index_wm"]
  f1_awayteamindex_wm <- F1_secondsplit[f1_secondsplitrow,"f1_AwayTeam_index_wm"]
  f1_hometeam_game_no <- F1_secondsplit[f1_secondsplitrow,"f1_homegame_no"]
  f1_awayteam_game_no <- F1_secondsplit[f1_secondsplitrow,"f1_awaygame_no"]

  f1_winmargin_vec_gamestate_h <- as.vector(f1_winmargin_h[f1_hometeamindex_wm,])
  f1_winmargin_vec_gamestate_h[is.na(f1_winmargin_vec_gamestate_h)] <- ""
  f1_winmargin_vec_gamestate_h <- f1_winmargin_vec_gamestate_h[f1_winmargin_vec_gamestate_h != ""]
  f1_winmargin_vec_gamestate_h <- as.numeric(f1_winmargin_vec_gamestate_h)

  f1_winmargin_vec_gamestate_a <- as.vector(f1_winmargin_h[f1_awayteamindex_wm,])
  f1_winmargin_vec_gamestate_a[is.na(f1_winmargin_vec_gamestate_a)] <- ""
  f1_winmargin_vec_gamestate_a <- f1_winmargin_vec_gamestate_a[f1_winmargin_vec_gamestate_a != ""]
  f1_winmargin_vec_gamestate_a <- as.numeric(f1_winmargin_vec_gamestate_a)

  for (f1_game_no in 1:f1_games_played[1])

  {

    f1_homewinmargin[f1_secondsplitrow] <- f1_winmargin_vec_gamestate_h[f1_hometeam_game_no]
    f1_awaywinmargin[f1_secondsplitrow] <- f1_winmargin_vec_gamestate_a[f1_awayteam_game_no]


  }

}

f1_homewinmargin
f1_awaywinmargin

F1_gamestate_data <- cbind(F1_secondsplit,f1_homewinmargin,f1_awaywinmargin)
F1_gamestate_data$f1_GSCS <- paste(F1_gamestate_data$f1_homewinmargin,F1_gamestate_data$f1_awaywinmargin, sep = ",")
F1_gamestate_data$f1_GSH <- F1_gamestate_data$f1_homewinmargin - F1_gamestate_data$f1_awaywinmargin
head(F1_gamestate_data,5)
F1_gamestate_copy <- F1_gamestate_data
F1_gamestate_copy$f1_HomeTeam_index_wm <- NULL
F1_gamestate_copy$f1_AwayTeam_index_wm <- NULL
F1_gamestate_copy$f1_homegame_no <- NULL
F1_gamestate_copy$f1_awaygame_no <- NULL
colnames(F1_gamestate_copy)[30] <- "homewinmargin"
colnames(F1_gamestate_copy)[31] <- "awaywinmargin"
colnames(F1_gamestate_copy)[32] <- "GSCS"
colnames(F1_gamestate_copy)[33] <- "GSH"
#end F1
###################################################################################################
#F2
F2_margindata <- F2
f2_firstgames <- length(f2_teams)/2
F2_firstsplit <-  head(F2_margindata,f2_firstgames)
F2_secondsplit <- tail(F2_margindata,nrow(F2_margindata) - f2_firstgames)
F2_secondsplit$f2_HomeTeam_index_wm <- match(F2_secondsplit$HomeTeam,f2_teams)
F2_secondsplit$f2_AwayTeam_index_wm <- match(F2_secondsplit$AwayTeam,f2_teams)
F2_secondsplit$f2_homegame_no <- rep(2:f2_games_played[1] - 1, each = length(f2_teams)/2)
F2_secondsplit$f2_awaygame_no <- rep(2:f2_games_played[1] - 1, each = length(f2_teams)/2)

f2_homewinmargin <- c()
f2_awaywinmargin <- c()

for (f2_secondsplitrow in 1:nrow(F2_secondsplit))
{

  f2_hometeamindex_wm <- F2_secondsplit[f2_secondsplitrow,"f2_HomeTeam_index_wm"]
  f2_awayteamindex_wm <- F2_secondsplit[f2_secondsplitrow,"f2_AwayTeam_index_wm"]
  f2_hometeam_game_no <- F2_secondsplit[f2_secondsplitrow,"f2_homegame_no"]
  f2_awayteam_game_no <- F2_secondsplit[f2_secondsplitrow,"f2_awaygame_no"]

  f2_winmargin_vec_gamestate_h <- as.vector(f2_winmargin_h[f2_hometeamindex_wm,])
  f2_winmargin_vec_gamestate_h[is.na(f2_winmargin_vec_gamestate_h)] <- ""
  f2_winmargin_vec_gamestate_h <- f2_winmargin_vec_gamestate_h[f2_winmargin_vec_gamestate_h != ""]
  f2_winmargin_vec_gamestate_h <- as.numeric(f2_winmargin_vec_gamestate_h)

  f2_winmargin_vec_gamestate_a <- as.vector(f2_winmargin_h[f2_awayteamindex_wm,])
  f2_winmargin_vec_gamestate_a[is.na(f2_winmargin_vec_gamestate_a)] <- ""
  f2_winmargin_vec_gamestate_a <- f2_winmargin_vec_gamestate_a[f2_winmargin_vec_gamestate_a != ""]
  f2_winmargin_vec_gamestate_a <- as.numeric(f2_winmargin_vec_gamestate_a)

  for (f2_game_no in 1:f2_games_played[1])

  {

    f2_homewinmargin[f2_secondsplitrow] <- f2_winmargin_vec_gamestate_h[f2_hometeam_game_no]
    f2_awaywinmargin[f2_secondsplitrow] <- f2_winmargin_vec_gamestate_a[f2_awayteam_game_no]


  }

}

f2_homewinmargin
f2_awaywinmargin

F2_gamestate_data <- cbind(F2_secondsplit,f2_homewinmargin,f2_awaywinmargin)
F2_gamestate_data$f2_GSCS <- paste(F2_gamestate_data$f2_homewinmargin,F2_gamestate_data$f2_awaywinmargin, sep = ",")
F2_gamestate_data$f2_GSH <- F2_gamestate_data$f2_homewinmargin - F2_gamestate_data$f2_awaywinmargin
head(F2_gamestate_data,5)
F2_gamestate_copy <- F2_gamestate_data
F2_gamestate_copy$f2_HomeTeam_index_wm <- NULL
F2_gamestate_copy$f2_AwayTeam_index_wm <- NULL
F2_gamestate_copy$f2_homegame_no <- NULL
F2_gamestate_copy$f2_awaygame_no <- NULL
colnames(F2_gamestate_copy)[30] <- "homewinmargin"
colnames(F2_gamestate_copy)[31] <- "awaywinmargin"
colnames(F2_gamestate_copy)[32] <- "GSCS"
colnames(F2_gamestate_copy)[33] <- "GSH"
#end F2
###################################################################################################
#G1
G1_margindata <- G1
g1_firstgames <- length(g1_teams)/2
G1_firstsplit <-  head(G1_margindata,g1_firstgames)
G1_secondsplit <- tail(G1_margindata,nrow(G1_margindata) - g1_firstgames)
G1_secondsplit$g1_HomeTeam_index_wm <- match(G1_secondsplit$HomeTeam,g1_teams)
G1_secondsplit$g1_AwayTeam_index_wm <- match(G1_secondsplit$AwayTeam,g1_teams)
G1_secondsplit$g1_homegame_no <- rep(2:g1_games_played[1] - 1, each = length(g1_teams)/2)
G1_secondsplit$g1_awaygame_no <- rep(2:g1_games_played[1] - 1, each = length(g1_teams)/2)

g1_homewinmargin <- c()
g1_awaywinmargin <- c()

for (g1_secondsplitrow in 1:nrow(G1_secondsplit))
{

  g1_hometeamindex_wm <- G1_secondsplit[g1_secondsplitrow,"g1_HomeTeam_index_wm"]
  g1_awayteamindex_wm <- G1_secondsplit[g1_secondsplitrow,"g1_AwayTeam_index_wm"]
  g1_hometeam_game_no <- G1_secondsplit[g1_secondsplitrow,"g1_homegame_no"]
  g1_awayteam_game_no <- G1_secondsplit[g1_secondsplitrow,"g1_awaygame_no"]

  g1_winmargin_vec_gamestate_h <- as.vector(g1_winmargin_h[g1_hometeamindex_wm,])
  g1_winmargin_vec_gamestate_h[is.na(g1_winmargin_vec_gamestate_h)] <- ""
  g1_winmargin_vec_gamestate_h <- g1_winmargin_vec_gamestate_h[g1_winmargin_vec_gamestate_h != ""]
  g1_winmargin_vec_gamestate_h <- as.numeric(g1_winmargin_vec_gamestate_h)

  g1_winmargin_vec_gamestate_a <- as.vector(g1_winmargin_h[g1_awayteamindex_wm,])
  g1_winmargin_vec_gamestate_a[is.na(g1_winmargin_vec_gamestate_a)] <- ""
  g1_winmargin_vec_gamestate_a <- g1_winmargin_vec_gamestate_a[g1_winmargin_vec_gamestate_a != ""]
  g1_winmargin_vec_gamestate_a <- as.numeric(g1_winmargin_vec_gamestate_a)

  for (g1_game_no in 1:g1_games_played[1])

  {

    g1_homewinmargin[g1_secondsplitrow] <- g1_winmargin_vec_gamestate_h[g1_hometeam_game_no]
    g1_awaywinmargin[g1_secondsplitrow] <- g1_winmargin_vec_gamestate_a[g1_awayteam_game_no]


  }

}

g1_homewinmargin
g1_awaywinmargin

G1_gamestate_data <- cbind(G1_secondsplit,g1_homewinmargin,g1_awaywinmargin)
G1_gamestate_data$g1_GSCS <- paste(G1_gamestate_data$g1_homewinmargin,G1_gamestate_data$g1_awaywinmargin, sep = ",")
G1_gamestate_data$g1_GSH <- G1_gamestate_data$g1_homewinmargin - G1_gamestate_data$g1_awaywinmargin
head(G1_gamestate_data,5)
G1_gamestate_copy <- G1_gamestate_data
G1_gamestate_copy$g1_HomeTeam_index_wm <- NULL
G1_gamestate_copy$g1_AwayTeam_index_wm <- NULL
G1_gamestate_copy$g1_homegame_no <- NULL
G1_gamestate_copy$g1_awaygame_no <- NULL
colnames(G1_gamestate_copy)[30] <- "homewinmargin"
colnames(G1_gamestate_copy)[31] <- "awaywinmargin"
colnames(G1_gamestate_copy)[32] <- "GSCS"
colnames(G1_gamestate_copy)[33] <- "GSH"
#end G1
###################################################################################################
#I1
I1_margindata <- I1
i1_firstgames <- length(i1_teams)/2
I1_firstsplit <-  head(I1_margindata,i1_firstgames)
I1_secondsplit <- tail(I1_margindata,nrow(I1_margindata) - i1_firstgames)
I1_secondsplit$i1_HomeTeam_index_wm <- match(I1_secondsplit$HomeTeam,i1_teams)
I1_secondsplit$i1_AwayTeam_index_wm <- match(I1_secondsplit$AwayTeam,i1_teams)
I1_secondsplit$i1_homegame_no <- rep(2:i1_games_played[1] - 1, each = length(i1_teams)/2)
I1_secondsplit$i1_awaygame_no <- rep(2:i1_games_played[1] - 1, each = length(i1_teams)/2)

i1_homewinmargin <- c()
i1_awaywinmargin <- c()

for (i1_secondsplitrow in 1:nrow(I1_secondsplit))
{

  i1_hometeamindex_wm <- I1_secondsplit[i1_secondsplitrow,"i1_HomeTeam_index_wm"]
  i1_awayteamindex_wm <- I1_secondsplit[i1_secondsplitrow,"i1_AwayTeam_index_wm"]
  i1_hometeam_game_no <- I1_secondsplit[i1_secondsplitrow,"i1_homegame_no"]
  i1_awayteam_game_no <- I1_secondsplit[i1_secondsplitrow,"i1_awaygame_no"]

  i1_winmargin_vec_gamestate_h <- as.vector(i1_winmargin_h[i1_hometeamindex_wm,])
  i1_winmargin_vec_gamestate_h[is.na(i1_winmargin_vec_gamestate_h)] <- ""
  i1_winmargin_vec_gamestate_h <- i1_winmargin_vec_gamestate_h[i1_winmargin_vec_gamestate_h != ""]
  i1_winmargin_vec_gamestate_h <- as.numeric(i1_winmargin_vec_gamestate_h)

  i1_winmargin_vec_gamestate_a <- as.vector(i1_winmargin_h[i1_awayteamindex_wm,])
  i1_winmargin_vec_gamestate_a[is.na(i1_winmargin_vec_gamestate_a)] <- ""
  i1_winmargin_vec_gamestate_a <- i1_winmargin_vec_gamestate_a[i1_winmargin_vec_gamestate_a != ""]
  i1_winmargin_vec_gamestate_a <- as.numeric(i1_winmargin_vec_gamestate_a)

  for (i1_game_no in 1:i1_games_played[1])

  {

    i1_homewinmargin[i1_secondsplitrow] <- i1_winmargin_vec_gamestate_h[i1_hometeam_game_no]
    i1_awaywinmargin[i1_secondsplitrow] <- i1_winmargin_vec_gamestate_a[i1_awayteam_game_no]


  }

}

i1_homewinmargin
i1_awaywinmargin

I1_gamestate_data <- cbind(I1_secondsplit,i1_homewinmargin,i1_awaywinmargin)
I1_gamestate_data$i1_GSCS <- paste(I1_gamestate_data$i1_homewinmargin,I1_gamestate_data$i1_awaywinmargin, sep = ",")
I1_gamestate_data$i1_GSH <- I1_gamestate_data$i1_homewinmargin - I1_gamestate_data$i1_awaywinmargin
head(I1_gamestate_data,5)
I1_gamestate_copy <- I1_gamestate_data
I1_gamestate_copy$i1_HomeTeam_index_wm <- NULL
I1_gamestate_copy$i1_AwayTeam_index_wm <- NULL
I1_gamestate_copy$i1_homegame_no <- NULL
I1_gamestate_copy$i1_awaygame_no <- NULL
colnames(I1_gamestate_copy)[30] <- "homewinmargin"
colnames(I1_gamestate_copy)[31] <- "awaywinmargin"
colnames(I1_gamestate_copy)[32] <- "GSCS"
colnames(I1_gamestate_copy)[33] <- "GSH"
#end I1
###################################################################################################
#I2
I2_margindata <- I2
i2_firstgames <- length(i2_teams)/2
I2_firstsplit <-  head(I2_margindata,i2_firstgames)
I2_secondsplit <- tail(I2_margindata,nrow(I2_margindata) - i2_firstgames)
I2_secondsplit$i2_HomeTeam_index_wm <- match(I2_secondsplit$HomeTeam,i2_teams)
I2_secondsplit$i2_AwayTeam_index_wm <- match(I2_secondsplit$AwayTeam,i2_teams)
I2_secondsplit$i2_homegame_no <- rep(2:i2_games_played[1] - 1, each = length(i2_teams)/2)
I2_secondsplit$i2_awaygame_no <- rep(2:i2_games_played[1] - 1, each = length(i2_teams)/2)

i2_homewinmargin <- c()
i2_awaywinmargin <- c()

for (i2_secondsplitrow in 1:nrow(I2_secondsplit))
{

  i2_hometeamindex_wm <- I2_secondsplit[i2_secondsplitrow,"i2_HomeTeam_index_wm"]
  i2_awayteamindex_wm <- I2_secondsplit[i2_secondsplitrow,"i2_AwayTeam_index_wm"]
  i2_hometeam_game_no <- I2_secondsplit[i2_secondsplitrow,"i2_homegame_no"]
  i2_awayteam_game_no <- I2_secondsplit[i2_secondsplitrow,"i2_awaygame_no"]

  i2_winmargin_vec_gamestate_h <- as.vector(i2_winmargin_h[i2_hometeamindex_wm,])
  i2_winmargin_vec_gamestate_h[is.na(i2_winmargin_vec_gamestate_h)] <- ""
  i2_winmargin_vec_gamestate_h <- i2_winmargin_vec_gamestate_h[i2_winmargin_vec_gamestate_h != ""]
  i2_winmargin_vec_gamestate_h <- as.numeric(i2_winmargin_vec_gamestate_h)

  i2_winmargin_vec_gamestate_a <- as.vector(i2_winmargin_h[i2_awayteamindex_wm,])
  i2_winmargin_vec_gamestate_a[is.na(i2_winmargin_vec_gamestate_a)] <- ""
  i2_winmargin_vec_gamestate_a <- i2_winmargin_vec_gamestate_a[i2_winmargin_vec_gamestate_a != ""]
  i2_winmargin_vec_gamestate_a <- as.numeric(i2_winmargin_vec_gamestate_a)

  for (i2_game_no in 1:i2_games_played[1])

  {

    i2_homewinmargin[i2_secondsplitrow] <- i2_winmargin_vec_gamestate_h[i2_hometeam_game_no]
    i2_awaywinmargin[i2_secondsplitrow] <- i2_winmargin_vec_gamestate_a[i2_awayteam_game_no]


  }

}

i2_homewinmargin
i2_awaywinmargin

I2_gamestate_data <- cbind(I2_secondsplit,i2_homewinmargin,i2_awaywinmargin)
I2_gamestate_data$i2_GSCS <- paste(I2_gamestate_data$i2_homewinmargin,I2_gamestate_data$i2_awaywinmargin, sep = ",")
I2_gamestate_data$i2_GSH <- I2_gamestate_data$i2_homewinmargin - I2_gamestate_data$i2_awaywinmargin
head(I2_gamestate_data,5)
I2_gamestate_copy <- I2_gamestate_data
I2_gamestate_copy$i2_HomeTeam_index_wm <- NULL
I2_gamestate_copy$i2_AwayTeam_index_wm <- NULL
I2_gamestate_copy$i2_homegame_no <- NULL
I2_gamestate_copy$i2_awaygame_no <- NULL
colnames(I2_gamestate_copy)[30] <- "homewinmargin"
colnames(I2_gamestate_copy)[31] <- "awaywinmargin"
colnames(I2_gamestate_copy)[32] <- "GSCS"
colnames(I2_gamestate_copy)[33] <- "GSH"
#end I2
###################################################################################################
#N1
N1_margindata <- N1
n1_firstgames <- length(n1_teams)/2
N1_firstsplit <-  head(N1_margindata,n1_firstgames)
N1_secondsplit <- tail(N1_margindata,nrow(N1_margindata) - n1_firstgames)
N1_secondsplit$n1_HomeTeam_index_wm <- match(N1_secondsplit$HomeTeam,n1_teams)
N1_secondsplit$n1_AwayTeam_index_wm <- match(N1_secondsplit$AwayTeam,n1_teams)
N1_secondsplit$n1_homegame_no <- rep(2:max(n1_games_played[]) - 1,each = length(n1_teams)/2,length.out = nrow(N1_secondsplit))
N1_secondsplit$n1_awaygame_no <-  rep(2:max(n1_games_played[]) - 1,each = length(n1_teams)/2,length.out = nrow(N1_secondsplit))

n1_homewinmargin <- c()
n1_awaywinmargin <- c()

for (n1_secondsplitrow in 1:nrow(N1_secondsplit))
{

  n1_hometeamindex_wm <- N1_secondsplit[n1_secondsplitrow,"n1_HomeTeam_index_wm"]
  n1_awayteamindex_wm <- N1_secondsplit[n1_secondsplitrow,"n1_AwayTeam_index_wm"]
  n1_hometeam_game_no <- N1_secondsplit[n1_secondsplitrow,"n1_homegame_no"]
  n1_awayteam_game_no <- N1_secondsplit[n1_secondsplitrow,"n1_awaygame_no"]

  n1_winmargin_vec_gamestate_h <- as.vector(n1_winmargin_h[n1_hometeamindex_wm,])
  n1_winmargin_vec_gamestate_h[is.na(n1_winmargin_vec_gamestate_h)] <- ""
  n1_winmargin_vec_gamestate_h <- n1_winmargin_vec_gamestate_h[n1_winmargin_vec_gamestate_h != ""]
  n1_winmargin_vec_gamestate_h <- as.numeric(n1_winmargin_vec_gamestate_h)

  n1_winmargin_vec_gamestate_a <- as.vector(n1_winmargin_h[n1_awayteamindex_wm,])
  n1_winmargin_vec_gamestate_a[is.na(n1_winmargin_vec_gamestate_a)] <- ""
  n1_winmargin_vec_gamestate_a <- n1_winmargin_vec_gamestate_a[n1_winmargin_vec_gamestate_a != ""]
  n1_winmargin_vec_gamestate_a <- as.numeric(n1_winmargin_vec_gamestate_a)

  for (n1_game_no in 1:n1_games_played[1])

  {

    n1_homewinmargin[n1_secondsplitrow] <- n1_winmargin_vec_gamestate_h[n1_hometeam_game_no]
    n1_awaywinmargin[n1_secondsplitrow] <- n1_winmargin_vec_gamestate_a[n1_awayteam_game_no]


  }

}

n1_homewinmargin
n1_awaywinmargin

N1_gamestate_data <- cbind(N1_secondsplit,n1_homewinmargin,n1_awaywinmargin)
N1_gamestate_data$n1_GSCS <- paste(N1_gamestate_data$n1_homewinmargin,N1_gamestate_data$n1_awaywinmargin, sep = ",")
N1_gamestate_data$n1_GSH <- N1_gamestate_data$n1_homewinmargin - N1_gamestate_data$n1_awaywinmargin
head(N1_gamestate_data,5)
N1_gamestate_copy <- N1_gamestate_data
N1_gamestate_copy$n1_HomeTeam_index_wm <- NULL
N1_gamestate_copy$n1_AwayTeam_index_wm <- NULL
N1_gamestate_copy$n1_homegame_no <- NULL
N1_gamestate_copy$n1_awaygame_no <- NULL
colnames(N1_gamestate_copy)[30] <- "homewinmargin"
colnames(N1_gamestate_copy)[31] <- "awaywinmargin"
colnames(N1_gamestate_copy)[32] <- "GSCS"
colnames(N1_gamestate_copy)[33] <- "GSH"
#end N1
###################################################################################################
#P1
P1_margindata <- P1
p1_firstgames <- length(p1_teams)/2
P1_firstsplit <-  head(P1_margindata,p1_firstgames)
P1_secondsplit <- tail(P1_margindata,nrow(P1_margindata) - p1_firstgames)
P1_secondsplit$p1_HomeTeam_index_wm <- match(P1_secondsplit$HomeTeam,p1_teams)
P1_secondsplit$p1_AwayTeam_index_wm <- match(P1_secondsplit$AwayTeam,p1_teams)
P1_secondsplit$p1_homegame_no <- rep(2:p1_games_played[1] - 1, each = length(p1_teams)/2)
P1_secondsplit$p1_awaygame_no <- rep(2:p1_games_played[1] - 1, each = length(p1_teams)/2)

p1_homewinmargin <- c()
p1_awaywinmargin <- c()

for (p1_secondsplitrow in 1:nrow(P1_secondsplit))
{

  p1_hometeamindex_wm <- P1_secondsplit[p1_secondsplitrow,"p1_HomeTeam_index_wm"]
  p1_awayteamindex_wm <- P1_secondsplit[p1_secondsplitrow,"p1_AwayTeam_index_wm"]
  p1_hometeam_game_no <- P1_secondsplit[p1_secondsplitrow,"p1_homegame_no"]
  p1_awayteam_game_no <- P1_secondsplit[p1_secondsplitrow,"p1_awaygame_no"]

  p1_winmargin_vec_gamestate_h <- as.vector(p1_winmargin_h[p1_hometeamindex_wm,])
  p1_winmargin_vec_gamestate_h[is.na(p1_winmargin_vec_gamestate_h)] <- ""
  p1_winmargin_vec_gamestate_h <- p1_winmargin_vec_gamestate_h[p1_winmargin_vec_gamestate_h != ""]
  p1_winmargin_vec_gamestate_h <- as.numeric(p1_winmargin_vec_gamestate_h)

  p1_winmargin_vec_gamestate_a <- as.vector(p1_winmargin_h[p1_awayteamindex_wm,])
  p1_winmargin_vec_gamestate_a[is.na(p1_winmargin_vec_gamestate_a)] <- ""
  p1_winmargin_vec_gamestate_a <- p1_winmargin_vec_gamestate_a[p1_winmargin_vec_gamestate_a != ""]
  p1_winmargin_vec_gamestate_a <- as.numeric(p1_winmargin_vec_gamestate_a)

  for (p1_game_no in 1:p1_games_played[1])

  {

    p1_homewinmargin[p1_secondsplitrow] <- p1_winmargin_vec_gamestate_h[p1_hometeam_game_no]
    p1_awaywinmargin[p1_secondsplitrow] <- p1_winmargin_vec_gamestate_a[p1_awayteam_game_no]


  }

}

p1_homewinmargin
p1_awaywinmargin

P1_gamestate_data <- cbind(P1_secondsplit,p1_homewinmargin,p1_awaywinmargin)
P1_gamestate_data$p1_GSCS <- paste(P1_gamestate_data$p1_homewinmargin,P1_gamestate_data$p1_awaywinmargin, sep = ",")
P1_gamestate_data$p1_GSH <- P1_gamestate_data$p1_homewinmargin - P1_gamestate_data$p1_awaywinmargin
head(P1_gamestate_data,5)
P1_gamestate_copy <- P1_gamestate_data
P1_gamestate_copy$p1_HomeTeam_index_wm <- NULL
P1_gamestate_copy$p1_AwayTeam_index_wm <- NULL
P1_gamestate_copy$p1_homegame_no <- NULL
P1_gamestate_copy$p1_awaygame_no <- NULL
colnames(P1_gamestate_copy)[30] <- "homewinmargin"
colnames(P1_gamestate_copy)[31] <- "awaywinmargin"
colnames(P1_gamestate_copy)[32] <- "GSCS"
colnames(P1_gamestate_copy)[33] <- "GSH"
#end P1
###################################################################################################
#SC0
SC0_margindata <- SC0
sc0_firstgames <- length(sc0_teams)/2
SC0_firstsplit <-  head(SC0_margindata,sc0_firstgames)
SC0_secondsplit <- tail(SC0_margindata,nrow(SC0_margindata) - sc0_firstgames)
SC0_secondsplit$sc0_HomeTeam_index_wm <- match(SC0_secondsplit$HomeTeam,sc0_teams)
SC0_secondsplit$sc0_AwayTeam_index_wm <- match(SC0_secondsplit$AwayTeam,sc0_teams)
SC0_secondsplit$sc0_homegame_no <-  rep(2:max(sc0_games_played[]) - 1,each = length(sc0_teams)/2,length.out = nrow(SC0_secondsplit))
SC0_secondsplit$sc0_awaygame_no <-  rep(2:max(sc0_games_played[]) - 1,each = length(sc0_teams)/2,length.out = nrow(SC0_secondsplit))

sc0_homewinmargin <- c()
sc0_awaywinmargin <- c()

for (sc0_secondsplitrow in 1:nrow(SC0_secondsplit))
{

  sc0_hometeamindex_wm <- SC0_secondsplit[sc0_secondsplitrow,"sc0_HomeTeam_index_wm"]
  sc0_awayteamindex_wm <- SC0_secondsplit[sc0_secondsplitrow,"sc0_AwayTeam_index_wm"]
  sc0_hometeam_game_no <- SC0_secondsplit[sc0_secondsplitrow,"sc0_homegame_no"]
  sc0_awayteam_game_no <- SC0_secondsplit[sc0_secondsplitrow,"sc0_awaygame_no"]

  sc0_winmargin_vec_gamestate_h <- as.vector(sc0_winmargin_h[sc0_hometeamindex_wm,])
  sc0_winmargin_vec_gamestate_h[is.na(sc0_winmargin_vec_gamestate_h)] <- ""
  sc0_winmargin_vec_gamestate_h <- sc0_winmargin_vec_gamestate_h[sc0_winmargin_vec_gamestate_h != ""]
  sc0_winmargin_vec_gamestate_h <- as.numeric(sc0_winmargin_vec_gamestate_h)

  sc0_winmargin_vec_gamestate_a <- as.vector(sc0_winmargin_h[sc0_awayteamindex_wm,])
  sc0_winmargin_vec_gamestate_a[is.na(sc0_winmargin_vec_gamestate_a)] <- ""
  sc0_winmargin_vec_gamestate_a <- sc0_winmargin_vec_gamestate_a[sc0_winmargin_vec_gamestate_a != ""]
  sc0_winmargin_vec_gamestate_a <- as.numeric(sc0_winmargin_vec_gamestate_a)

  for (sc0_game_no in 1:sc0_games_played[1])

  {

    sc0_homewinmargin[sc0_secondsplitrow] <- sc0_winmargin_vec_gamestate_h[sc0_hometeam_game_no]
    sc0_awaywinmargin[sc0_secondsplitrow] <- sc0_winmargin_vec_gamestate_a[sc0_awayteam_game_no]


  }

}

sc0_homewinmargin
sc0_awaywinmargin

SC0_gamestate_data <- cbind(SC0_secondsplit,sc0_homewinmargin,sc0_awaywinmargin)
SC0_gamestate_data$sc0_GSCS <- paste(SC0_gamestate_data$sc0_homewinmargin,SC0_gamestate_data$sc0_awaywinmargin, sep = ",")
SC0_gamestate_data$sc0_GSH <- SC0_gamestate_data$sc0_homewinmargin - SC0_gamestate_data$sc0_awaywinmargin
head(SC0_gamestate_data,5)
SC0_gamestate_copy <- SC0_gamestate_data
SC0_gamestate_copy$sc0_HomeTeam_index_wm <- NULL
SC0_gamestate_copy$sc0_AwayTeam_index_wm <- NULL
SC0_gamestate_copy$sc0_homegame_no <- NULL
SC0_gamestate_copy$sc0_awaygame_no <- NULL
colnames(SC0_gamestate_copy)[30] <- "homewinmargin"
colnames(SC0_gamestate_copy)[31] <- "awaywinmargin"
colnames(SC0_gamestate_copy)[32] <- "GSCS"
colnames(SC0_gamestate_copy)[33] <- "GSH"
#end SC0
###################################################################################################
#SC1
SC1_margindata <- SC1
sc1_firstgames <- length(sc1_teams)/2
SC1_firstsplit <-  head(SC1_margindata,sc1_firstgames)
SC1_secondsplit <- tail(SC1_margindata,nrow(SC1_margindata) - sc1_firstgames)
SC1_secondsplit$sc1_HomeTeam_index_wm <- match(SC1_secondsplit$HomeTeam,sc1_teams)
SC1_secondsplit$sc1_AwayTeam_index_wm <- match(SC1_secondsplit$AwayTeam,sc1_teams)
SC1_secondsplit$sc1_homegame_no <-  rep(2:max(sc1_games_played[]) - 1,each = length(sc1_teams)/2,length.out = nrow(SC1_secondsplit))
SC1_secondsplit$sc1_awaygame_no <-  rep(2:max(sc1_games_played[]) - 1,each = length(sc1_teams)/2,length.out = nrow(SC1_secondsplit))

sc1_homewinmargin <- c()
sc1_awaywinmargin <- c()

for (sc1_secondsplitrow in 1:nrow(SC1_secondsplit))
{

  sc1_hometeamindex_wm <- SC1_secondsplit[sc1_secondsplitrow,"sc1_HomeTeam_index_wm"]
  sc1_awayteamindex_wm <- SC1_secondsplit[sc1_secondsplitrow,"sc1_AwayTeam_index_wm"]
  sc1_hometeam_game_no <- SC1_secondsplit[sc1_secondsplitrow,"sc1_homegame_no"]
  sc1_awayteam_game_no <- SC1_secondsplit[sc1_secondsplitrow,"sc1_awaygame_no"]

  sc1_winmargin_vec_gamestate_h <- as.vector(sc1_winmargin_h[sc1_hometeamindex_wm,])
  sc1_winmargin_vec_gamestate_h[is.na(sc1_winmargin_vec_gamestate_h)] <- ""
  sc1_winmargin_vec_gamestate_h <- sc1_winmargin_vec_gamestate_h[sc1_winmargin_vec_gamestate_h != ""]
  sc1_winmargin_vec_gamestate_h <- as.numeric(sc1_winmargin_vec_gamestate_h)

  sc1_winmargin_vec_gamestate_a <- as.vector(sc1_winmargin_h[sc1_awayteamindex_wm,])
  sc1_winmargin_vec_gamestate_a[is.na(sc1_winmargin_vec_gamestate_a)] <- ""
  sc1_winmargin_vec_gamestate_a <- sc1_winmargin_vec_gamestate_a[sc1_winmargin_vec_gamestate_a != ""]
  sc1_winmargin_vec_gamestate_a <- as.numeric(sc1_winmargin_vec_gamestate_a)

  for (sc1_game_no in 1:sc1_games_played[1])

  {

    sc1_homewinmargin[sc1_secondsplitrow] <- sc1_winmargin_vec_gamestate_h[sc1_hometeam_game_no]
    sc1_awaywinmargin[sc1_secondsplitrow] <- sc1_winmargin_vec_gamestate_a[sc1_awayteam_game_no]


  }

}

sc1_homewinmargin
sc1_awaywinmargin

SC1_gamestate_data <- cbind(SC1_secondsplit,sc1_homewinmargin,sc1_awaywinmargin)
SC1_gamestate_data$sc1_GSCS <- paste(SC1_gamestate_data$sc1_homewinmargin,SC1_gamestate_data$sc1_awaywinmargin, sep = ",")
SC1_gamestate_data$sc1_GSH <- SC1_gamestate_data$sc1_homewinmargin - SC1_gamestate_data$sc1_awaywinmargin
head(SC1_gamestate_data,5)
SC1_gamestate_copy <- SC1_gamestate_data
SC1_gamestate_copy$sc1_HomeTeam_index_wm <- NULL
SC1_gamestate_copy$sc1_AwayTeam_index_wm <- NULL
SC1_gamestate_copy$sc1_homegame_no <- NULL
SC1_gamestate_copy$sc1_awaygame_no <- NULL
colnames(SC1_gamestate_copy)[30] <- "homewinmargin"
colnames(SC1_gamestate_copy)[31] <- "awaywinmargin"
colnames(SC1_gamestate_copy)[32] <- "GSCS"
colnames(SC1_gamestate_copy)[33] <- "GSH"
#end SC1
###################################################################################################
#SC2
SC2_margindata <- SC2
sc2_firstgames <- length(sc2_teams)/2
SC2_firstsplit <-  head(SC2_margindata,sc2_firstgames)
SC2_secondsplit <- tail(SC2_margindata,nrow(SC2_margindata) - sc2_firstgames)
SC2_secondsplit$sc2_HomeTeam_index_wm <- match(SC2_secondsplit$HomeTeam,sc2_teams)
SC2_secondsplit$sc2_AwayTeam_index_wm <- match(SC2_secondsplit$AwayTeam,sc2_teams)
SC2_secondsplit$sc2_homegame_no <- rep(2:max(sc2_games_played[]) - 1,each = length(sc2_teams)/2,length.out = nrow(SC2_secondsplit))
SC2_secondsplit$sc2_awaygame_no <- rep(2:max(sc2_games_played[]) - 1,each = length(sc2_teams)/2,length.out = nrow(SC2_secondsplit))

sc2_homewinmargin <- c()
sc2_awaywinmargin <- c()

for (sc2_secondsplitrow in 1:nrow(SC2_secondsplit))
{

  sc2_hometeamindex_wm <- SC2_secondsplit[sc2_secondsplitrow,"sc2_HomeTeam_index_wm"]
  sc2_awayteamindex_wm <- SC2_secondsplit[sc2_secondsplitrow,"sc2_AwayTeam_index_wm"]
  sc2_hometeam_game_no <- SC2_secondsplit[sc2_secondsplitrow,"sc2_homegame_no"]
  sc2_awayteam_game_no <- SC2_secondsplit[sc2_secondsplitrow,"sc2_awaygame_no"]

  sc2_winmargin_vec_gamestate_h <- as.vector(sc2_winmargin_h[sc2_hometeamindex_wm,])
  sc2_winmargin_vec_gamestate_h[is.na(sc2_winmargin_vec_gamestate_h)] <- ""
  sc2_winmargin_vec_gamestate_h <- sc2_winmargin_vec_gamestate_h[sc2_winmargin_vec_gamestate_h != ""]
  sc2_winmargin_vec_gamestate_h <- as.numeric(sc2_winmargin_vec_gamestate_h)

  sc2_winmargin_vec_gamestate_a <- as.vector(sc2_winmargin_h[sc2_awayteamindex_wm,])
  sc2_winmargin_vec_gamestate_a[is.na(sc2_winmargin_vec_gamestate_a)] <- ""
  sc2_winmargin_vec_gamestate_a <- sc2_winmargin_vec_gamestate_a[sc2_winmargin_vec_gamestate_a != ""]
  sc2_winmargin_vec_gamestate_a <- as.numeric(sc2_winmargin_vec_gamestate_a)

  for (sc2_game_no in 1:sc2_games_played[1])

  {

    sc2_homewinmargin[sc2_secondsplitrow] <- sc2_winmargin_vec_gamestate_h[sc2_hometeam_game_no]
    sc2_awaywinmargin[sc2_secondsplitrow] <- sc2_winmargin_vec_gamestate_a[sc2_awayteam_game_no]


  }

}

sc2_homewinmargin
sc2_awaywinmargin

SC2_gamestate_data <- cbind(SC2_secondsplit,sc2_homewinmargin,sc2_awaywinmargin)
SC2_gamestate_data$sc2_GSCS <- paste(SC2_gamestate_data$sc2_homewinmargin,SC2_gamestate_data$sc2_awaywinmargin, sep = ",")
SC2_gamestate_data$sc2_GSH <- SC2_gamestate_data$sc2_homewinmargin - SC2_gamestate_data$sc2_awaywinmargin
head(SC2_gamestate_data,5)
SC2_gamestate_copy <- SC2_gamestate_data
SC2_gamestate_copy$sc2_HomeTeam_index_wm <- NULL
SC2_gamestate_copy$sc2_AwayTeam_index_wm <- NULL
SC2_gamestate_copy$sc2_homegame_no <- NULL
SC2_gamestate_copy$sc2_awaygame_no <- NULL
colnames(SC2_gamestate_copy)[30] <- "homewinmargin"
colnames(SC2_gamestate_copy)[31] <- "awaywinmargin"
colnames(SC2_gamestate_copy)[32] <- "GSCS"
colnames(SC2_gamestate_copy)[33] <- "GSH"
#end SC2
###################################################################################################
#SC3
SC3_margindata <- SC3
sc3_firstgames <- length(sc3_teams)/2
SC3_firstsplit <-  head(SC3_margindata,sc3_firstgames)
SC3_secondsplit <- tail(SC3_margindata,nrow(SC3_margindata) - sc3_firstgames)
SC3_secondsplit$sc3_HomeTeam_index_wm <- match(SC3_secondsplit$HomeTeam,sc3_teams)
SC3_secondsplit$sc3_AwayTeam_index_wm <- match(SC3_secondsplit$AwayTeam,sc3_teams)
SC3_secondsplit$sc3_homegame_no <-  rep(2:max(sc3_games_played[]) - 1,each = length(sc3_teams)/2,length.out = nrow(SC3_secondsplit))
SC3_secondsplit$sc3_awaygame_no <-  rep(2:max(sc3_games_played[]) - 1,each = length(sc3_teams)/2,length.out = nrow(SC3_secondsplit))

sc3_homewinmargin <- c()
sc3_awaywinmargin <- c()

for (sc3_secondsplitrow in 1:nrow(SC3_secondsplit))
{

  sc3_hometeamindex_wm <- SC3_secondsplit[sc3_secondsplitrow,"sc3_HomeTeam_index_wm"]
  sc3_awayteamindex_wm <- SC3_secondsplit[sc3_secondsplitrow,"sc3_AwayTeam_index_wm"]
  sc3_hometeam_game_no <- SC3_secondsplit[sc3_secondsplitrow,"sc3_homegame_no"]
  sc3_awayteam_game_no <- SC3_secondsplit[sc3_secondsplitrow,"sc3_awaygame_no"]

  sc3_winmargin_vec_gamestate_h <- as.vector(sc3_winmargin_h[sc3_hometeamindex_wm,])
  sc3_winmargin_vec_gamestate_h[is.na(sc3_winmargin_vec_gamestate_h)] <- ""
  sc3_winmargin_vec_gamestate_h <- sc3_winmargin_vec_gamestate_h[sc3_winmargin_vec_gamestate_h != ""]
  sc3_winmargin_vec_gamestate_h <- as.numeric(sc3_winmargin_vec_gamestate_h)

  sc3_winmargin_vec_gamestate_a <- as.vector(sc3_winmargin_h[sc3_awayteamindex_wm,])
  sc3_winmargin_vec_gamestate_a[is.na(sc3_winmargin_vec_gamestate_a)] <- ""
  sc3_winmargin_vec_gamestate_a <- sc3_winmargin_vec_gamestate_a[sc3_winmargin_vec_gamestate_a != ""]
  sc3_winmargin_vec_gamestate_a <- as.numeric(sc3_winmargin_vec_gamestate_a)

  for (sc3_game_no in 1:sc3_games_played[1])

  {

    sc3_homewinmargin[sc3_secondsplitrow] <- sc3_winmargin_vec_gamestate_h[sc3_hometeam_game_no]
    sc3_awaywinmargin[sc3_secondsplitrow] <- sc3_winmargin_vec_gamestate_a[sc3_awayteam_game_no]


  }

}

sc3_homewinmargin
sc3_awaywinmargin

SC3_gamestate_data <- cbind(SC3_secondsplit,sc3_homewinmargin,sc3_awaywinmargin)
SC3_gamestate_data$sc3_GSCS <- paste(SC3_gamestate_data$sc3_homewinmargin,SC3_gamestate_data$sc3_awaywinmargin, sep = ",")
SC3_gamestate_data$sc3_GSH <- SC3_gamestate_data$sc3_homewinmargin - SC3_gamestate_data$sc3_awaywinmargin
head(SC3_gamestate_data,5)
SC3_gamestate_copy <- SC3_gamestate_data
SC3_gamestate_copy$sc3_HomeTeam_index_wm <- NULL
SC3_gamestate_copy$sc3_AwayTeam_index_wm <- NULL
SC3_gamestate_copy$sc3_homegame_no <- NULL
SC3_gamestate_copy$sc3_awaygame_no <- NULL
colnames(SC3_gamestate_copy)[30] <- "homewinmargin"
colnames(SC3_gamestate_copy)[31] <- "awaywinmargin"
colnames(SC3_gamestate_copy)[32] <- "GSCS"
colnames(SC3_gamestate_copy)[33] <- "GSH"
#end SC3
###################################################################################################
#SP1
SP1_margindata <- SP1
sp1_firstgames <- length(sp1_teams)/2
SP1_firstsplit <-  head(SP1_margindata,sp1_firstgames)
SP1_secondsplit <- tail(SP1_margindata,nrow(SP1_margindata) - sp1_firstgames)
SP1_secondsplit$sp1_HomeTeam_index_wm <- match(SP1_secondsplit$HomeTeam,sp1_teams)
SP1_secondsplit$sp1_AwayTeam_index_wm <- match(SP1_secondsplit$AwayTeam,sp1_teams)
SP1_secondsplit$sp1_homegame_no <-  rep(2:max(sp1_games_played[]) - 1,each = length(sp1_teams)/2,length.out = nrow(SP1_secondsplit))
SP1_secondsplit$sp1_awaygame_no <-  rep(2:max(sp1_games_played[]) - 1,each = length(sp1_teams)/2,length.out = nrow(SP1_secondsplit))

sp1_homewinmargin <- c()
sp1_awaywinmargin <- c()

for (sp1_secondsplitrow in 1:nrow(SP1_secondsplit))
{

  sp1_hometeamindex_wm <- SP1_secondsplit[sp1_secondsplitrow,"sp1_HomeTeam_index_wm"]
  sp1_awayteamindex_wm <- SP1_secondsplit[sp1_secondsplitrow,"sp1_AwayTeam_index_wm"]
  sp1_hometeam_game_no <- SP1_secondsplit[sp1_secondsplitrow,"sp1_homegame_no"]
  sp1_awayteam_game_no <- SP1_secondsplit[sp1_secondsplitrow,"sp1_awaygame_no"]

  sp1_winmargin_vec_gamestate_h <- as.vector(sp1_winmargin_h[sp1_hometeamindex_wm,])
  sp1_winmargin_vec_gamestate_h[is.na(sp1_winmargin_vec_gamestate_h)] <- ""
  sp1_winmargin_vec_gamestate_h <- sp1_winmargin_vec_gamestate_h[sp1_winmargin_vec_gamestate_h != ""]
  sp1_winmargin_vec_gamestate_h <- as.numeric(sp1_winmargin_vec_gamestate_h)

  sp1_winmargin_vec_gamestate_a <- as.vector(sp1_winmargin_h[sp1_awayteamindex_wm,])
  sp1_winmargin_vec_gamestate_a[is.na(sp1_winmargin_vec_gamestate_a)] <- ""
  sp1_winmargin_vec_gamestate_a <- sp1_winmargin_vec_gamestate_a[sp1_winmargin_vec_gamestate_a != ""]
  sp1_winmargin_vec_gamestate_a <- as.numeric(sp1_winmargin_vec_gamestate_a)

  for (sp1_game_no in 1:sp1_games_played[1])

  {

    sp1_homewinmargin[sp1_secondsplitrow] <- sp1_winmargin_vec_gamestate_h[sp1_hometeam_game_no]
    sp1_awaywinmargin[sp1_secondsplitrow] <- sp1_winmargin_vec_gamestate_a[sp1_awayteam_game_no]


  }

}

sp1_homewinmargin
sp1_awaywinmargin

SP1_gamestate_data <- cbind(SP1_secondsplit,sp1_homewinmargin,sp1_awaywinmargin)
SP1_gamestate_data$sp1_GSCS <- paste(SP1_gamestate_data$sp1_homewinmargin,SP1_gamestate_data$sp1_awaywinmargin, sep = ",")
SP1_gamestate_data$sp1_GSH <- SP1_gamestate_data$sp1_homewinmargin - SP1_gamestate_data$sp1_awaywinmargin
head(SP1_gamestate_data,5)
SP1_gamestate_copy <- SP1_gamestate_data
SP1_gamestate_copy$sp1_HomeTeam_index_wm <- NULL
SP1_gamestate_copy$sp1_AwayTeam_index_wm <- NULL
SP1_gamestate_copy$sp1_homegame_no <- NULL
SP1_gamestate_copy$sp1_awaygame_no <- NULL
colnames(SP1_gamestate_copy)[30] <- "homewinmargin"
colnames(SP1_gamestate_copy)[31] <- "awaywinmargin"
colnames(SP1_gamestate_copy)[32] <- "GSCS"
colnames(SP1_gamestate_copy)[33] <- "GSH"
#end SP1
###################################################################################################
#SP2
SP2_margindata <- SP2
sp2_firstgames <- length(sp2_teams)/2
SP2_firstsplit <-  head(SP2_margindata,sp2_firstgames)
SP2_secondsplit <- tail(SP2_margindata,nrow(SP2_margindata) - sp2_firstgames)
SP2_secondsplit$sp2_HomeTeam_index_wm <- match(SP2_secondsplit$HomeTeam,sp2_teams)
SP2_secondsplit$sp2_AwayTeam_index_wm <- match(SP2_secondsplit$AwayTeam,sp2_teams)
SP2_secondsplit$sp2_homegame_no <- rep(2:max(sp2_games_played[]) - 1,each = length(sp2_teams)/2,length.out = nrow(SP2_secondsplit))
SP2_secondsplit$sp2_awaygame_no <- rep(2:max(sp2_games_played[]) - 1,each = length(sp2_teams)/2,length.out = nrow(SP2_secondsplit))

sp2_homewinmargin <- c()
sp2_awaywinmargin <- c()

for (sp2_secondsplitrow in 1:nrow(SP2_secondsplit))
{

  sp2_hometeamindex_wm <- SP2_secondsplit[sp2_secondsplitrow,"sp2_HomeTeam_index_wm"]
  sp2_awayteamindex_wm <- SP2_secondsplit[sp2_secondsplitrow,"sp2_AwayTeam_index_wm"]
  sp2_hometeam_game_no <- SP2_secondsplit[sp2_secondsplitrow,"sp2_homegame_no"]
  sp2_awayteam_game_no <- SP2_secondsplit[sp2_secondsplitrow,"sp2_awaygame_no"]

  sp2_winmargin_vec_gamestate_h <- as.vector(sp2_winmargin_h[sp2_hometeamindex_wm,])
  sp2_winmargin_vec_gamestate_h[is.na(sp2_winmargin_vec_gamestate_h)] <- ""
  sp2_winmargin_vec_gamestate_h <- sp2_winmargin_vec_gamestate_h[sp2_winmargin_vec_gamestate_h != ""]
  sp2_winmargin_vec_gamestate_h <- as.numeric(sp2_winmargin_vec_gamestate_h)

  sp2_winmargin_vec_gamestate_a <- as.vector(sp2_winmargin_h[sp2_awayteamindex_wm,])
  sp2_winmargin_vec_gamestate_a[is.na(sp2_winmargin_vec_gamestate_a)] <- ""
  sp2_winmargin_vec_gamestate_a <- sp2_winmargin_vec_gamestate_a[sp2_winmargin_vec_gamestate_a != ""]
  sp2_winmargin_vec_gamestate_a <- as.numeric(sp2_winmargin_vec_gamestate_a)

  for (sp2_game_no in 1:sp2_games_played[1])

  {

    sp2_homewinmargin[sp2_secondsplitrow] <- sp2_winmargin_vec_gamestate_h[sp2_hometeam_game_no]
    sp2_awaywinmargin[sp2_secondsplitrow] <- sp2_winmargin_vec_gamestate_a[sp2_awayteam_game_no]


  }

}

sp2_homewinmargin
sp2_awaywinmargin

SP2_gamestate_data <- cbind(SP2_secondsplit,sp2_homewinmargin,sp2_awaywinmargin)
SP2_gamestate_data$sp2_GSCS <- paste(SP2_gamestate_data$sp2_homewinmargin,SP2_gamestate_data$sp2_awaywinmargin, sep = ",")
SP2_gamestate_data$sp2_GSH <- SP2_gamestate_data$sp2_homewinmargin - SP2_gamestate_data$sp2_awaywinmargin
head(SP2_gamestate_data,5)
SP2_gamestate_copy <- SP2_gamestate_data
SP2_gamestate_copy$sp2_HomeTeam_index_wm <- NULL
SP2_gamestate_copy$sp2_AwayTeam_index_wm <- NULL
SP2_gamestate_copy$sp2_homegame_no <- NULL
SP2_gamestate_copy$sp2_awaygame_no <- NULL
colnames(SP2_gamestate_copy)[30] <- "homewinmargin"
colnames(SP2_gamestate_copy)[31] <- "awaywinmargin"
colnames(SP2_gamestate_copy)[32] <- "GSCS"
colnames(SP2_gamestate_copy)[33] <- "GSH"
#end SP2
###################################################################################################
#T1
T1_margindata <- T1
t1_firstgames <- length(t1_teams)/2
T1_firstsplit <-  head(T1_margindata,t1_firstgames)
T1_secondsplit <- tail(T1_margindata,nrow(T1_margindata) - t1_firstgames)
T1_secondsplit$t1_HomeTeam_index_wm <- match(T1_secondsplit$HomeTeam,t1_teams)
T1_secondsplit$t1_AwayTeam_index_wm <- match(T1_secondsplit$AwayTeam,t1_teams)
T1_secondsplit$t1_homegame_no <- rep(2:t1_games_played[1] - 1, each = length(t1_teams)/2)
T1_secondsplit$t1_awaygame_no <- rep(2:t1_games_played[1] - 1, each = length(t1_teams)/2)

t1_homewinmargin <- c()
t1_awaywinmargin <- c()

for (t1_secondsplitrow in 1:nrow(T1_secondsplit))
{

  t1_hometeamindex_wm <- T1_secondsplit[t1_secondsplitrow,"t1_HomeTeam_index_wm"]
  t1_awayteamindex_wm <- T1_secondsplit[t1_secondsplitrow,"t1_AwayTeam_index_wm"]
  t1_hometeam_game_no <- T1_secondsplit[t1_secondsplitrow,"t1_homegame_no"]
  t1_awayteam_game_no <- T1_secondsplit[t1_secondsplitrow,"t1_awaygame_no"]

  t1_winmargin_vec_gamestate_h <- as.vector(t1_winmargin_h[t1_hometeamindex_wm,])
  t1_winmargin_vec_gamestate_h[is.na(t1_winmargin_vec_gamestate_h)] <- ""
  t1_winmargin_vec_gamestate_h <- t1_winmargin_vec_gamestate_h[t1_winmargin_vec_gamestate_h != ""]
  t1_winmargin_vec_gamestate_h <- as.numeric(t1_winmargin_vec_gamestate_h)

  t1_winmargin_vec_gamestate_a <- as.vector(t1_winmargin_h[t1_awayteamindex_wm,])
  t1_winmargin_vec_gamestate_a[is.na(t1_winmargin_vec_gamestate_a)] <- ""
  t1_winmargin_vec_gamestate_a <- t1_winmargin_vec_gamestate_a[t1_winmargin_vec_gamestate_a != ""]
  t1_winmargin_vec_gamestate_a <- as.numeric(t1_winmargin_vec_gamestate_a)

  for (t1_game_no in 1:t1_games_played[1])

  {

    t1_homewinmargin[t1_secondsplitrow] <- t1_winmargin_vec_gamestate_h[t1_hometeam_game_no]
    t1_awaywinmargin[t1_secondsplitrow] <- t1_winmargin_vec_gamestate_a[t1_awayteam_game_no]


  }

}

t1_homewinmargin
t1_awaywinmargin

T1_gamestate_data <- cbind(T1_secondsplit,t1_homewinmargin,t1_awaywinmargin)
T1_gamestate_data$t1_GSCS <- paste(T1_gamestate_data$t1_homewinmargin,T1_gamestate_data$t1_awaywinmargin, sep = ",")
T1_gamestate_data$t1_GSH <- T1_gamestate_data$t1_homewinmargin - T1_gamestate_data$t1_awaywinmargin
head(T1_gamestate_data,5)
T1_gamestate_copy <- T1_gamestate_data
T1_gamestate_copy$t1_HomeTeam_index_wm <- NULL
T1_gamestate_copy$t1_AwayTeam_index_wm <- NULL
T1_gamestate_copy$t1_homegame_no <- NULL
T1_gamestate_copy$t1_awaygame_no <- NULL
colnames(T1_gamestate_copy)[30] <- "homewinmargin"
colnames(T1_gamestate_copy)[31] <- "awaywinmargin"
colnames(T1_gamestate_copy)[32] <- "GSCS"
colnames(T1_gamestate_copy)[33] <- "GSH"
#end T1
###################################################################################################
###################################################################################################

allteams20212022_gamestate <- rbind(B1_gamestate_copy,D1_gamestate_copy,D2_gamestate_copy,E0_gamestate_copy,E1_gamestate_copy,E2_gamestate_copy,
E3_gamestate_copy,EC_gamestate_copy,F1_gamestate_copy,F2_gamestate_copy,G1_gamestate_copy,I1_gamestate_copy,I2_gamestate_copy,N1_gamestate_copy,
P1_gamestate_copy,SC0_gamestate_copy,SC1_gamestate_copy,SC2_gamestate_copy,SC3_gamestate_copy,SP1_gamestate_copy,SP2_gamestate_copy,T1_gamestate_copy)

nrow(allteams20212022_gamestate)

colnames(allteams20212022_gamestate)


View(allteams20212022_gamestate[allteams20212022_gamestate$GSCS == "-1,-1",])



library(tidyr)
 gamestate_ovun <- allteams20212022_gamestate %>% group_by(GSCS,OV25) %>% summarise(count=n())

 library(xlsx)
 write.csv(gamestate_ovun,'gamestateovun.csv')

colnames(allteams20212022_gamestate)

sort(gamestate_ovun)

allteams2010presentdf$winmargin <- NULL
allteams2010presentdf[allteams2010presentdf$matchkey == "Lorient-Paris SG",]

df$count <- as.numeric(ave(df$gender, df$gender, FUN = length))

stats::ave(allteams2010presentdf$HomeTeam,allteams2010presentdf$FTR,FUN = length(allteams2010presentdf$FTR))

summary(allteams2010presentdf)

library(plyr)
library(dplyr)
allteams2010presentdf$homewins <-  ddply(allteams2010presentdf,.(HomeTeam),transform,count=length(which(FTR == "H")))
head(allteams2010presentdf)

gamestate_ovun <- allteams20212022_gamestate[allteams20212022_gamestate$homewinmargin <= -1 & allteams20212022_gamestate$awaywinmargin <= -1,]
gamestate_ovun %>% group_by(OV25) %>% summarise(count=n())







