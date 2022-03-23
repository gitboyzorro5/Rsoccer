library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
library(stringr)
library(stringi)
#delete current file
unlink('NL/SWZ.xlsx')
######################SWZ START#######################################
#####################################################################
SWZ <- read.csv('../FDAS/SWZ.csv')
SWZ <- within(SWZ,rm(Res))
SWZ$Date <- dmy(SWZ$Date)
SWZ <- SWZ[order(as.Date(SWZ$Date, format = "%d/%m%Y"), decreasing = FALSE),]
SWZ$CS <- paste(SWZ$HG,SWZ$AG, sep = "-")
#SWZ_qualificaton <- subset(SWZ,tournament == "UEFA Euro qualification")
SWZ <- subset(SWZ,Season == "2021/2022")
SWZ <- subset(SWZ,League == "Super League")
#SWZ <- SWZ[SWZ$Date > '2008-01-01',])
SWZ$TG <- SWZ$HG + SWZ$AG
SWZ$OV25 <- ifelse(SWZ$TG >= 3,"Y","N")
SWZ$FTR <- with(SWZ,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
# SWZ <- mgsub(SWZ,c("Wolfsberger"),c("Wolfsberger AC"))
# SWZ <- mgsub(SWZ,c("Wolfsberger AC AC"),c("Wolfsberger AC"))
####GoalTotalsv2##################################
swz_totalgoalsv2 <- tapply(SWZ$TG, SWZ[c("Home", "Away")],mean)
swz_totalgoalsv2
swz_hgtotals <- rowSums(swz_totalgoalsv2,na.rm = T)
swz_agtotals <- colSums(swz_totalgoalsv2,na.rm = T)

swz_totalgoals <- swz_hgtotals + swz_agtotals
swz_totalgoalsv2 <- cbind(swz_totalgoalsv2,swz_totalgoals)
swz_teams <- sort(unique(SWZ$Home))
swz_home_games <- c()
swz_away_games <-c()
for (i_swz in 1:length(swz_teams))
{

  swz_home_games[i_swz] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz],])
  swz_away_games[i_swz]  <- nrow(SWZ[SWZ$Away == swz_teams[i_swz],])

}
swz_games_played <- swz_home_games + swz_away_games
swz_goaltotalsv2 <- cbind(swz_totalgoalsv2,swz_games_played)
swz_avg_totalgoals <- round((swz_totalgoals/ swz_games_played), digits = 4)
swz_goaltotalsv2[is.na(swz_goaltotalsv2)] <- ""
swz_goaltotalsv2 <- cbind(swz_goaltotalsv2,swz_avg_totalgoals)
write.xlsx(swz_goaltotalsv2,'NL/SWZ.xlsx',sheetName = "totalgoalsv2")
#####################################################################
SWZ <- subset(SWZ,Season == "2021/2022")
swz_totalrounds <-  (length(swz_teams) - 1 )*2
swz_totalmatches <- (length(swz_teams)*(length(swz_teams) - 1))
swz_eachround <- swz_totalmatches / swz_totalrounds

swz_matchesplayed <-  nrow(SWZ)

SWZ_rounds <- SWZ

if(swz_matchesplayed %% swz_eachround == 0)
{
  swz_currentround <- swz_matchesplayed / swz_eachround
  swz_matchday <- c()
  swz_matchday <- rep(1:swz_currentround, each = swz_eachround)
}else if(swz_matchesplayed %% swz_eachround != 0)

{

  swz_modulus <- swz_matchesplayed %% swz_eachround
  swz_currentround <- (swz_matchesplayed - swz_modulus) / swz_eachround
  swz_matchday <- c()
  swz_matchday_vec1 <- c()
  swz_matchday_vec2 <- c()
  swz_matchday_vec1 <- rep(1:swz_currentround, each = swz_eachround)
  swz_matchday_vec2[1:swz_modulus] <- c(swz_currentround + 1)
  swz_matchday <- append(swz_matchday_vec1,swz_matchday_vec2)
}
SWZ_rounds <- cbind(SWZ_rounds,swz_matchday)
#####################################################################################################
#create home and away matrices
swz_goalscored_h <- tapply(SWZ$HG, SWZ[c("Home", "Date")],mean)
swz_goalscored_a <- tapply(SWZ$AG, SWZ[c("Away", "Date")],mean)
swz_goalscored_h[is.na(swz_goalscored_h)] <- ""
swz_goalscored_a[is.na(swz_goalscored_a)] <- ""

for(swz_rowhgs in 1:nrow(swz_goalscored_h)) {
  for(swz_colhgs in 1:ncol(swz_goalscored_h)) {

    # print(my_matrix[row, col])
    for(swz_rowags in 1:nrow(swz_goalscored_a)) {
      for(swz_colags in 1:ncol(swz_goalscored_a)) {
        ifelse(!swz_goalscored_a[swz_rowags,swz_colags]=="",swz_goalscored_h[swz_rowags,swz_colags] <- swz_goalscored_a[swz_rowags,swz_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(swz_goalscoredmatrix,'NL/SWZ.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#########################################################################################
#swz goal scored rounds
final_swz_gs <- matrix(nrow = length(swz_teams),ncol = swz_totalrounds )
suml6_swz_gs <- c()
sum_swz_zero_gs <- c()
sum_swz_one_gs <- c()
sum_swz_two_gs <- c()
sum_swz_three_gs <- c()
l6_form_swz_gssplitted <- c()
form_swz_gs <- c()
for(index_swz_gs in 1:length(swz_teams))
{
  for(index_swz_gs_cols in 1:swz_totalrounds)
  {
    index_swz_gs  <- row.names(swz_goalscored_h) == swz_teams[index_swz_gs]
    form_swz_gs <- swz_goalscored_h[index_swz_gs ]
    deleted_form_swz_gs <- form_swz_gs[!form_swz_gs[] == ""]
    l6_form_swz_gs <- tail(deleted_form_swz_gs,swz_last_n_games)
    l6_form_swz_gs <- as.numeric(l6_form_swz_gs)
    suml6_swz_gs[index_swz_gs] <- sum(l6_form_swz_gs)
    suml6_swz_gs[index_swz_gs] <- paste(suml6_swz_gs[index_swz_gs],sep = "")
    sum_swz_zero_gs[index_swz_gs] <- length(which(l6_form_swz_gs == 0))
    sum_swz_zero_gs[index_swz_gs] <- paste(sum_swz_zero_gs[index_swz_gs],sep = "")
    sum_swz_one_gs[index_swz_gs] <- length(which(l6_form_swz_gs == 1))
    sum_swz_one_gs[index_swz_gs] <- paste(sum_swz_one_gs[index_swz_gs],sep = "")
    sum_swz_two_gs[index_swz_gs] <- length(which(l6_form_swz_gs >= 2))
    sum_swz_two_gs[index_swz_gs] <- paste(sum_swz_two_gs[index_swz_gs],sep = "")
    sum_swz_three_gs[index_swz_gs] <- length(which(l6_form_swz_gs >= 3))
    sum_swz_three_gs[index_swz_gs] <- paste(sum_swz_three_gs[index_swz_gs],sep = "")
    l6_form_swz_gs <- as.character(l6_form_swz_gs)
    l6_form_swz_gs_flattened <- stri_paste(l6_form_swz_gs,collapse = '')
    l6_form_swz_gssplitted <- as.numeric(strsplit(as.character(l6_form_swz_gs_flattened),"")[[1]])
    final_swz_gs[index_swz_gs,index_swz_gs_cols] <- l6_form_swz_gssplitted[index_swz_gs_cols]
  }
}

final_swz_gs[is.na(final_swz_gs)] <- ""
swz_goalscoredmatrix <- cbind(swz_teams,final_swz_gs,suml6_swz_gs,sum_swz_zero_gs,sum_swz_one_gs,sum_swz_two_gs,sum_swz_three_gs)
write.xlsx(swz_goalscoredmatrix,'NL/SWZ.xlsx',sheetName = "gsmatrix", append = TRUE)
#################################################################################################################################

####GCmatrix#####################################################################################################################
#create home and away matrices
swz_goalconceded_h <- tapply(SWZ$AG, SWZ[c("Home", "Date")],mean)
swz_goalconceded_a <- tapply(SWZ$HG, SWZ[c("Away", "Date")],mean)
swz_goalconceded_h[is.na(swz_goalconceded_h)] <- ""
swz_goalconceded_a[is.na(swz_goalconceded_a)] <- ""

for(swz_rowhgc in 1:nrow(swz_goalconceded_h)) {
  for(swz_colhgc in 1:ncol(swz_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(swz_rowagc in 1:nrow(swz_goalconceded_a)) {
      for(swz_colagc in 1:ncol(swz_goalconceded_a)) {
        ifelse(!swz_goalconceded_a[swz_rowagc,swz_colagc]=="",swz_goalconceded_h[swz_rowagc,swz_colagc] <- swz_goalconceded_a[swz_rowagc,swz_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(swz_goalconcededmatrix,'NL/SWZ.xlsx',sheetName = "gcmatrix", append = TRUE)
############################################################################################################################################################
#swz goal conceded rounds
final_swz_gc <- matrix(nrow = length(swz_teams),ncol = swz_totalrounds )
suml6_swz_gc <- c()
sum_swz_zero_gc <- c()
sum_swz_one_gc <- c()
sum_swz_two_gc <- c()
sum_swz_three_gc <- c()
l6_form_swz_gcsplitted <- c()
form_swz_gc <- c()
for(index_swz_gc in 1:length(swz_teams))
{
  for(index_swz_gc_cols in 1:swz_totalrounds)
  {
    index_swz_gc  <- row.names(swz_goalconceded_h) == swz_teams[index_swz_gc]
    form_swz_gc <- swz_goalconceded_h[index_swz_gc ]
    deleted_form_swz_gc <- form_swz_gc[!form_swz_gc[] == ""]
    l6_form_swz_gc <- tail(deleted_form_swz_gc,swz_last_n_games)
    l6_form_swz_gc <- as.numeric(l6_form_swz_gc)
    suml6_swz_gc[index_swz_gc] <- sum(l6_form_swz_gc)
    suml6_swz_gc[index_swz_gc] <- paste(suml6_swz_gc[index_swz_gc],sep = "")
    sum_swz_zero_gc[index_swz_gc] <- length(which(l6_form_swz_gc == 0))
    sum_swz_zero_gc[index_swz_gc] <- paste(sum_swz_zero_gc[index_swz_gc],sep = "")
    sum_swz_one_gc[index_swz_gc] <- length(which(l6_form_swz_gc == 1))
    sum_swz_one_gc[index_swz_gc] <- paste(sum_swz_one_gc[index_swz_gc],sep = "")
    sum_swz_two_gc[index_swz_gc] <- length(which(l6_form_swz_gc >= 2))
    sum_swz_two_gc[index_swz_gc] <- paste(sum_swz_two_gc[index_swz_gc],sep = "")
    sum_swz_three_gc[index_swz_gc] <- length(which(l6_form_swz_gc >= 3))
    sum_swz_three_gc[index_swz_gc] <- paste(sum_swz_three_gc[index_swz_gc],sep = "")
    l6_form_swz_gc <- as.character(l6_form_swz_gc)
    l6_form_swz_gc_flattened <- stri_paste(l6_form_swz_gc,collapse = '')
    l6_form_swz_gcsplitted <- as.numeric(strsplit(as.character(l6_form_swz_gc_flattened),"")[[1]])
    final_swz_gc[index_swz_gc,index_swz_gc_cols] <- l6_form_swz_gcsplitted[index_swz_gc_cols]
  }
}

final_swz_gc[is.na(final_swz_gc)] <- ""
swz_goalconcededmatrix <- cbind(swz_teams,final_swz_gc,suml6_swz_gc,sum_swz_zero_gc,sum_swz_one_gc,sum_swz_two_gc,sum_swz_three_gc)
write.xlsx(swz_goalconcededmatrix,'NL/SWZ.xlsx',sheetName = "gcmatrix2", append = TRUE)
###################################################################################################################################

###################################################################################################################################
####Teamform#######################################################################################################################

swz_form_h <- tapply(SWZ$FTR, SWZ[c("Home", "Date")],median)
swz_form_a <- tapply(SWZ$FTR, SWZ[c("Away", "Date")],median)
swz_form_h[is.na(swz_form_h)] <- ""
swz_form_a[is.na(swz_form_a)] <- ""
swz_form_h <- sub("A","L",swz_form_h)
swz_form_h <- sub("H","W",swz_form_h)
swz_form_a <- sub("A","W",swz_form_a)
swz_form_a <- sub("H","L",swz_form_a)
for(swz_rowh_f in 1:nrow(swz_form_h)) {
  for(swz_colh_f in 1:ncol(swz_form_h)) {

    # print(my_matrix[row, col])
    for(swz_rowa_f in 1:nrow(swz_form_a)) {
      for(swz_cola_f in 1:ncol(swz_form_a)) {
        ifelse(!swz_form_a[swz_rowa_f,swz_cola_f]=="",swz_form_h[swz_rowa_f,swz_cola_f] <- swz_form_a[swz_rowa_f,swz_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#swz team form
final_swz_hf <- matrix(nrow = length(swz_teams),ncol = swz_totalrounds )
suml6_swz_hf <- c()
l6_form_swz_hfsplitted <- c()
form_swz_hf <- c()
for(index_swz_hf in 1:length(swz_teams))
{
  for(index_swz_hf_cols in 1:swz_totalrounds)
  {
    index_swz_hf  <- row.names(swz_form_h) == swz_teams[index_swz_hf]
    form_swz_hf <- swz_form_h[index_swz_hf ]
    deleted_form_swz_hf <- form_swz_hf[!form_swz_hf[] == ""]
    l6_form_swz_hf <- tail(deleted_form_swz_hf,swz_last_n_games)
    # #l6_form_swz_hf <- as.numeric(l6_form_swz_hf)
    # suml6_swz_hf[index_swz_hf] <- sum(l6_form_swz_hf)
    # suml6_swz_hf[index_swz_hf] <- paste(suml6_swz_hf[index_swz_hf],sep = "")
    #l6_form_swz_hf <- as.character(l6_form_swz_hf)
    l6_form_swz_hf_flattened <- stri_paste(l6_form_swz_hf,collapse = '')
    l6_form_swz_hfsplitted <- (strsplit(as.character(l6_form_swz_hf_flattened),"")[[1]])
    final_swz_hf[index_swz_hf,index_swz_hf_cols] <- l6_form_swz_hfsplitted[index_swz_hf_cols]
  }
}
final_swz_hf[is.na(final_swz_hf)] <- ""
swz_formmatrix <- cbind(swz_teams,final_swz_hf)

write.xlsx(swz_formmatrix,'NL/SWZ.xlsx',sheetName = "form", append = TRUE)
######################################################################################################################################
######################################################################################################################################

#######TGMatrix#######################################################################################################################
swz_totalgoals_h <- tapply(SWZ$TG, SWZ[c("Home", "Date")],mean)
swz_totalgoals_a <- tapply(SWZ$TG, SWZ[c("Away", "Date")],mean)
swz_totalgoals_h[is.na(swz_totalgoals_h)] <- ""
swz_totalgoals_a[is.na(swz_totalgoals_a)] <- ""
for(swz_rowh in 1:nrow(swz_totalgoals_h)) {
  for(swz_colh in 1:ncol(swz_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(swz_rowa in 1:nrow(swz_totalgoals_a)) {
      for(swz_cola in 1:ncol(swz_totalgoals_a)) {
        ifelse(!swz_totalgoals_a[swz_rowa,swz_cola]=="",swz_totalgoals_h[swz_rowa,swz_cola] <- swz_totalgoals_a[swz_rowa,swz_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#swz total goals rounds
#swz
final_swz_tg <- matrix(nrow = length(swz_teams),ncol = swz_totalrounds )
suml6_swz_tg <- c()
l6_form_swz_tgsplitted <- c()
form_swz_tg <- c()
for(index_swz_tg in 1:length(swz_teams))
{
  for(index_swz_tg_cols in 1:swz_totalrounds)
  {
    index_swz_tg  <- row.names(swz_totalgoals_h) == swz_teams[index_swz_tg]
    form_swz_tg <- swz_totalgoals_h[index_swz_tg ]
    deleted_form_swz_tg <- form_swz_tg[!form_swz_tg[] == ""]
    l6_form_swz_tg <- tail(deleted_form_swz_tg,swz_last_n_games)
    l6_form_swz_tg <- as.numeric(l6_form_swz_tg)
    suml6_swz_tg[index_swz_tg] <- sum(l6_form_swz_tg)
    suml6_swz_tg[index_swz_tg] <- paste(suml6_swz_tg[index_swz_tg],sep = "")
    l6_form_swz_tg <- as.character(l6_form_swz_tg)
    l6_form_swz_tg_flattened <- stri_paste(l6_form_swz_tg,collapse = '')
    l6_form_swz_tgsplitted <- as.numeric(strsplit(as.character(l6_form_swz_tg_flattened),"")[[1]])
    final_swz_tg[index_swz_tg,index_swz_tg_cols] <- l6_form_swz_tgsplitted[index_swz_tg_cols]
  }
}

final_swz_tg[is.na(final_swz_tg)] <- ""
swz_goaltotalmatrix <- cbind(swz_teams,final_swz_tg,suml6_swz_tg)

write.xlsx(swz_goaltotalmatrix,'NL/SWZ.xlsx',sheetName = "tgmatrix", append = TRUE)
#############################################################################################################################################
#######TeamAgainst###########################################################################################################################
swz_form_team_against_h <- tapply(SWZ$Away, SWZ[c("Home", "Date")],median)
swz_form_team_against_a <- tapply(SWZ$Home, SWZ[c("Away", "Date")],median)
swz_form_team_against_h[is.na(swz_form_team_against_h)] <- ""
swz_form_team_against_a[is.na(swz_form_team_against_a)] <- ""
for(swz_rowh_f_against in 1:nrow(swz_form_team_against_h)) {
  for(swz_colh_f_against in 1:ncol(swz_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(swz_rowa_f_against in 1:nrow(swz_form_team_against_a)) {
      for(swz_cola_f_against in 1:ncol(swz_form_team_against_a)) {
        ifelse(!swz_form_team_against_a[swz_rowa_f_against,swz_cola_f_against]=="",swz_form_team_against_h[swz_rowa_f_against,swz_cola_f_against] <- swz_form_team_against_a[swz_rowa_f_against,swz_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#win margin
swz_winmargin_h <- tapply(SWZ$HG - SWZ$AG, SWZ[c("Home", "Date")],mean)
swz_winmargin_a <- tapply(SWZ$AG - SWZ$HG, SWZ[c("Away", "Date")],mean)
swz_winmargin_h[is.na(swz_winmargin_h)] <- ""
#
for(swz_rowhwm in 1:nrow(swz_winmargin_h)) {
  for(swz_colhwm in 1:ncol(swz_winmargin_h)) {

    # print(my_matrix[row, col])
    for(swz_rowawm in 1:nrow(swz_winmargin_a)) {
      for(swz_colawm in 1:ncol(swz_winmargin_a)) {
        ifelse(!swz_winmargin_a[swz_rowawm,swz_colawm]=="",swz_winmargin_h[swz_rowawm,swz_colawm] <- swz_winmargin_a[swz_rowawm,swz_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
final_swz_wm <- matrix(nrow = length(swz_teams),ncol = swz_totalrounds )
suml6_swz_wm <- c()
suml6_swz_wm_negone <- c()
suml6_swz_wm_negtwo <- c()
suml6_swz_wm_zero <- c()
suml6_swz_wm_posone <- c()
suml6_swz_wm_postwo <- c()
l6_form_swz_wmsplitted <- c()
form_swz_wm <- c()
for(index_swz_wm in 1:length(swz_teams))
{
  for(index_swz_wm_cols in 1:swz_totalrounds)
  {
    index_swz_wm  <- row.names(swz_winmargin_h) == swz_teams[index_swz_wm]
    form_swz_wm <- swz_winmargin_h[index_swz_wm ]
    deleted_form_swz_wm <- form_swz_wm[!form_swz_wm[] == ""]
    l6_form_swz_wm <- tail(deleted_form_swz_wm,swz_last_n_games)
    l6_form_swz_wm <- as.numeric(l6_form_swz_wm)
    suml6_swz_wm[index_swz_wm] <- sum(l6_form_swz_wm)
    suml6_swz_wm[index_swz_wm] <- paste(suml6_swz_wm[index_swz_wm],sep = "")
    suml6_swz_wm_negone[index_swz_wm] <- length(which(l6_form_swz_wm == -1))
    suml6_swz_wm_negone[index_swz_wm] <- paste(suml6_swz_wm_negone[index_swz_wm],sep = "")
    suml6_swz_wm_negtwo[index_swz_wm] <- length(which(l6_form_swz_wm <= -2))
    suml6_swz_wm_negtwo[index_swz_wm] <- paste(suml6_swz_wm_negtwo[index_swz_wm],sep = "")
    suml6_swz_wm_zero[index_swz_wm] <- length(which(l6_form_swz_wm == 0))
    suml6_swz_wm_zero[index_swz_wm] <- paste(suml6_swz_wm_zero[index_swz_wm],sep = "")
    suml6_swz_wm_posone[index_swz_wm] <- length(which(l6_form_swz_wm == 1))
    suml6_swz_wm_posone[index_swz_wm] <- paste(suml6_swz_wm_posone[index_swz_wm],sep = "")
    suml6_swz_wm_postwo[index_swz_wm] <- length(which(l6_form_swz_wm == 2))
    suml6_swz_wm_postwo[index_swz_wm] <- paste(suml6_swz_wm_postwo[index_swz_wm],sep = "")
    l6_form_swz_wm <- as.character(l6_form_swz_wm)
    l6_form_swz_wm_flattened <- stri_paste(l6_form_swz_wm,collapse = ',')
    l6_form_swz_wmsplitted <- (strsplit(as.character(l6_form_swz_wm_flattened),",")[[1]])
    final_swz_wm[index_swz_wm,index_swz_wm_cols] <- l6_form_swz_wmsplitted[index_swz_wm_cols]
  }
}

final_swz_wm[is.na(final_swz_wm)] <- ""
swz_winmarginmatrix <- cbind(swz_teams,final_swz_wm,suml6_swz_wm,suml6_swz_wm_negtwo,suml6_swz_wm_negone,suml6_swz_wm_zero,suml6_swz_wm_posone,suml6_swz_wm_postwo)
write.xlsx(swz_winmarginmatrix,'NL/SWZ.xlsx',sheetName = "winmargin", append = TRUE)
####################################################################################################################
##########Goals over under############
#SWZ
swz_un05_home <- c()
swz_un05_away <- c()
swz_ov05_home <- c()
swz_ov05_away <- c()

swz_un15_home <- c()
swz_un15_away <- c()
swz_ov15_home <- c()
swz_ov15_away <- c()

swz_un25_home <- c()
swz_un25_away <- c()
swz_ov25_home <- c()
swz_ov25_away <- c()

swz_un35_home <- c()
swz_un35_away <- c()
swz_ov35_home <- c()
swz_ov35_away <- c()

swz_un45_home <- c()
swz_un45_away <- c()
swz_ov45_home <- c()
swz_ov45_away <- c()

swz_un55_home <- c()
swz_un55_away <- c()
swz_ov55_home <- c()
swz_ov55_away <- c()

for (i_swz_tg in 1:length(swz_teams))
{

  swz_un05_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG == 0,])
  swz_un05_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG == 0,])

  swz_ov05_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG > 0,])
  swz_ov05_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG > 0,])

  swz_un15_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG <= 1,])
  swz_un15_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG <= 1,])

  swz_ov15_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG >= 2,])
  swz_ov15_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG >= 2,])

  swz_un25_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG <= 2,])
  swz_un25_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG <= 2,])

  swz_ov25_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG >=3,])
  swz_ov25_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG >=3,])

  swz_un35_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG <= 3,])
  swz_un35_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG <= 3,])

  swz_ov35_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG >= 4,])
  swz_ov35_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG >= 4,])

  swz_un45_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG <= 4,])
  swz_un45_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG <= 4,])

  swz_ov45_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG >= 5,])
  swz_ov45_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG >= 5,])

  swz_un55_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG <= 5,])
  swz_un55_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG <= 5,])

  swz_ov55_home[i_swz_tg] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_tg] & SWZ$TG >= 6,])
  swz_ov55_away[i_swz_tg] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_tg] & SWZ$TG >= 6,])


}

swz_un05 <- swz_un05_home + swz_un05_away
swz_ov05 <- swz_ov05_home + swz_ov05_away

swz_un15 <- swz_un15_home + swz_un15_away
swz_ov15 <- swz_ov15_home + swz_ov15_away

swz_un25 <- swz_un25_home + swz_un25_away
swz_ov25 <- swz_ov25_home + swz_ov25_away

swz_un35 <- swz_un35_home + swz_un35_away
swz_ov35 <- swz_ov35_home + swz_ov35_away

swz_un45 <- swz_un45_home + swz_un45_away
swz_ov45 <- swz_ov45_home + swz_ov45_away

swz_un55 <- swz_un55_home + swz_un55_away
swz_ov55 <- swz_ov55_home + swz_ov55_away

swz_ovundata <- cbind(swz_teams,swz_un05,swz_ov05,swz_un15,swz_ov15,swz_un25,swz_ov25,swz_un35,swz_ov35,swz_un45,swz_ov45,swz_un55,swz_ov55)
write.xlsx(swz_ovundata,'NL/SWZ.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
swz_csform_h <- tapply(SWZ$CS, SWZ[c("Home", "Date")],median)
swz_csform_a <- tapply(SWZ$CS, SWZ[c("Away", "Date")],median)

swz_csform_h[is.na(swz_csform_h)] <- ""
swz_csform_a[is.na(swz_csform_a)] <- ""

for(swz_rowh_f_cs in 1:nrow(swz_csform_h)) {
  for(swz_colh_f_cs in 1:ncol(swz_csform_h)) {

    # print(my_matrix[row, col])
    for(swz_rowa_f_cs in 1:nrow(swz_csform_a)) {
      for(swz_cola_f_cs in 1:ncol(swz_csform_a)) {
        ifelse(!swz_csform_a[swz_rowa_f_cs,swz_cola_f_cs]=="",swz_csform_h[swz_rowa_f_cs,swz_cola_f_cs] <- swz_csform_a[swz_rowa_f_cs,swz_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
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

######################################################################################
###########League Table###############################################################

#hwins and away wins
swz_home_wins <- c()
swz_away_wins <- c()
swz_home_draws <- c()
swz_away_draws <- c()
swz_home_loss <- c()
swz_away_loss <- c()



for (i_swz_wins in 1:length(swz_teams))
{

  swz_home_wins[i_swz_wins] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_wins] & SWZ$FTR == "H",])
  swz_away_wins[i_swz_wins] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_wins] & SWZ$FTR == "A",])
  swz_home_draws[i_swz_wins] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_wins] & SWZ$FTR == "D",])
  swz_away_draws[i_swz_wins] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_wins] & SWZ$FTR == "D",])
  swz_home_loss[i_swz_wins] <- nrow(SWZ[SWZ$Home == swz_teams[i_swz_wins] & SWZ$FTR == "A",])
  swz_away_loss[i_swz_wins] <- nrow(SWZ[SWZ$Away == swz_teams[i_swz_wins] & SWZ$FTR == "H",])

}

swz_total_wins <- swz_home_wins + swz_away_wins
swz_total_draws <- swz_home_draws + swz_away_draws
swz_total_loss <- swz_home_loss + swz_away_loss

swz_league_table <- cbind(swz_teams,swz_games_played,swz_total_wins,swz_total_draws,swz_total_loss)
swz_GS <- swz_scoring$TGS
swz_GC <-swz_conceding$TGC
swz_GD <- swz_scoring$TGS - swz_conceding$TGC
swz_PTS <- (swz_total_wins*3) + (swz_total_draws*1)
swz_league_table <- cbind(swz_league_table,swz_GS,swz_GC,swz_GD,swz_PTS)
swz_league_table <- as.data.frame(swz_league_table)
#rename the columns
names(swz_league_table)[names(swz_league_table) == "swz_teams"] <- "Team"
names(swz_league_table)[names(swz_league_table) == "swz_games_played"] <- "P"
names(swz_league_table)[names(swz_league_table) == "swz_total_wins"] <- "W"
names(swz_league_table)[names(swz_league_table) == "swz_total_draws"] <- "D"
names(swz_league_table)[names(swz_league_table) == "swz_total_loss"] <- "L"
names(swz_league_table)[names(swz_league_table) == "swz_GS"] <- "F"
names(swz_league_table)[names(swz_league_table) == "swz_GC"] <- "A"
points_swz <- swz_league_table[order(as.numeric(swz_league_table$swz_PTS), decreasing = TRUE),]
points_swz$swz_rank <- 1:length(swz_teams)
row.names(points_swz) <- points_swz$swz_rank
#create final_swz_hf_against with team ranks in brackets
for(swz_rowhrank in 1:nrow(swz_form_team_against_h)) {
  for(swz_colhrank in 1:ncol(swz_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!swz_form_team_against_h[swz_rowhrank,swz_colhrank]=="",swz_form_team_against_h[swz_rowhrank,swz_colhrank] <- paste(swz_form_team_against_h[swz_rowhrank,swz_colhrank],"(",points_swz$swz_rank[points_swz$Team ==swz_form_team_against_h[swz_rowhrank,swz_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_swz,'NL/SWZ.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six swz###################################################
#SWZ
#form
#create final_swz_hf object
#swz_last_n_games <- 6
final_swz_hf <- c()
for(index_swz_hf in 1:length(swz_teams))
{
  index_swz_hf <- row.names(swz_form_h) == swz_teams[index_swz_hf]
  form_swz_hf <- swz_form_h[index_swz_hf]
  deleted_form_swz_hf <- form_swz_hf[!form_swz_hf[] == ""]
  l6_form_swz_hf <- tail(deleted_form_swz_hf,swz_last_n_games)
  l6_form_swz_hf <- paste(l6_form_swz_hf,collapse = " ")
  final_swz_hf[index_swz_hf] <- rbind(paste(swz_teams[index_swz_hf],l6_form_swz_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swz_teams[index],l6_form)

}

#change column names
final_swz_hf <- as.data.frame(final_swz_hf)
colnames(final_swz_hf) <- "Form"
#goals scored
#create final_swz_gs object
final_swz_gs <- c()
suml6_swz_gs <- c()
for(index_swz_gs in 1:length(swz_teams))
{
  index_swz_gs <- row.names(swz_goalscored_h) == swz_teams[index_swz_gs]
  form_swz_gs <- swz_goalscored_h[index_swz_gs]
  deleted_form_swz_gs <- form_swz_gs[!form_swz_gs[] == ""]
  l6_form_swz_gs <- tail(deleted_form_swz_gs,swz_last_n_games)
  l6_form_swz_gs <- as.numeric(l6_form_swz_gs)
  suml6_swz_gs[index_swz_gs] <- sum(l6_form_swz_gs)
  suml6_swz_gs[index_swz_gs] <- paste("(",suml6_swz_gs[index_swz_gs],")",sep = "")
  l6_form_swz_gs <- paste(l6_form_swz_gs,collapse = " ")
  final_swz_gs[index_swz_gs] <- rbind(paste(swz_teams[index_swz_gs],l6_form_swz_gs,suml6_swz_gs[index_swz_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swz_teams[index],l6_form)

}
final_swz_gs
#change column names
final_swz_gs <- as.data.frame(final_swz_gs)
colnames(final_swz_gs) <- "Goals scored"
#goal conceded
#create final_swz_gc object
final_swz_gc <- c()
suml6_swz_gc <- c()
for(index_swz_gc in 1:length(swz_teams))
{
  index_swz_gc <- row.names(swz_goalconceded_h) == swz_teams[index_swz_gc]
  form_swz_gc <- swz_goalconceded_h[index_swz_gc]
  deleted_form_swz_gc <- form_swz_gc[!form_swz_gc[] == ""]
  l6_form_swz_gc <- tail(deleted_form_swz_gc,swz_last_n_games)
  l6_form_swz_gc <- as.numeric(l6_form_swz_gc)
  suml6_swz_gc[index_swz_gc] <- sum(l6_form_swz_gc)
  suml6_swz_gc[index_swz_gc] <- paste("(",suml6_swz_gc[index_swz_gc],")",sep = "")
  l6_form_swz_gc <- paste(l6_form_swz_gc,collapse = " ")
  final_swz_gc[index_swz_gc] <- rbind(paste(swz_teams[index_swz_gc],l6_form_swz_gc,suml6_swz_gc[index_swz_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swz_teams[index],l6_form)

}

#change column names
final_swz_gc <- as.data.frame(final_swz_gc)
colnames(final_swz_gc) <- "Goals conceded"
#total goals
#create final_swz_tg object
final_swz_tg <- c()
suml6_swz_tg <- c()
for(index_swz_tg in 1:length(swz_teams))
{
  index_swz_tg <- row.names(swz_totalgoals_h) == swz_teams[index_swz_tg]
  form_swz_tg <- swz_totalgoals_h[index_swz_tg]
  deleted_form_swz_tg <- form_swz_tg[!form_swz_tg[] == ""]
  l6_form_swz_tg <- tail(deleted_form_swz_tg,swz_last_n_games)
  l6_form_swz_tg <- as.numeric(l6_form_swz_tg)
  suml6_swz_tg[index_swz_tg] <- sum(l6_form_swz_tg)
  suml6_swz_tg[index_swz_tg] <- paste("(",suml6_swz_tg[index_swz_tg],")",sep = "")
  l6_form_swz_tg <- paste(l6_form_swz_tg,collapse = " ")
  final_swz_tg[index_swz_tg] <- rbind(paste(swz_teams[index_swz_tg],l6_form_swz_tg,suml6_swz_tg[index_swz_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swz_teams[index],l6_form)

}
#change column names
final_swz_tg <- as.data.frame(final_swz_tg)
colnames(final_swz_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_swz_hf object
final_swz_cs <- c()
for(index_swz_cs in 1:length(swz_teams))
{
  index_swz_cs <- row.names(swz_csform_h) == swz_teams[index_swz_cs]
  csform_swz_cs <- swz_csform_h[index_swz_cs]
  deleted_csform_swz_cs <- csform_swz_cs[!csform_swz_cs[] == ""]
  l6_csform_swz_cs <- tail(deleted_csform_swz_cs,swz_last_n_games)
  l6_csform_swz_cs <- paste(l6_csform_swz_cs,collapse = " ")
  final_swz_cs[index_swz_cs] <- rbind(paste(swz_teams[index_swz_cs],l6_csform_swz_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",swz_teams[index],l6_csform)

}

#change column names
final_swz_cs <- as.data.frame(final_swz_cs)
colnames(final_swz_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_swz_wm object
final_swz_wm <- c()
suml6_swz_wm <- c()
for(index_swz_wm in 1:length(swz_teams))
{
  index_swz_wm <- row.names(swz_winmargin_h) == swz_teams[index_swz_wm]
  form_swz_wm <- swz_winmargin_h[index_swz_wm]
  deleted_form_swz_wm <- form_swz_wm[!form_swz_wm[] == ""]
  l6_form_swz_wm <- tail(deleted_form_swz_wm,swz_last_n_games)
  l6_form_swz_wm <- as.numeric(l6_form_swz_wm)
  suml6_swz_wm[index_swz_wm] <- sum(l6_form_swz_wm)
  suml6_swz_wm[index_swz_wm] <- paste("(",suml6_swz_wm[index_swz_wm],")",sep = "")
  l6_form_swz_wm <- paste(l6_form_swz_wm,collapse = " ")
  final_swz_wm[index_swz_wm] <- rbind(paste(swz_teams[index_swz_wm],l6_form_swz_wm,suml6_swz_wm[index_swz_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swz_teams[index],l6_form)

}
final_swz_wm
#change column names
final_swz_wm <- as.data.frame(final_swz_wm)
colnames(final_swz_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_swz_hf_against
final_swz_hf_against <- c()
for(index_swz_hf_against in 1:length(swz_teams))
{
  index_swz_hf_against <- row.names(swz_form_team_against_h) == swz_teams[index_swz_hf_against]
  form_swz_hf_against <- swz_form_team_against_h[index_swz_hf_against]
  deleted_form_swz_hf_against <- form_swz_hf_against[!form_swz_hf_against[] == ""]
  l6_form_swz_hf_against <- tail(deleted_form_swz_hf_against,swz_last_n_games)
  l6_form_swz_hf_against <- paste(l6_form_swz_hf_against,collapse = " ")
  final_swz_hf_against[index_swz_hf_against] <- rbind(paste(swz_teams[index_swz_hf_against],l6_form_swz_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swz_teams[index],l6_form)

}
final_swz_hf_against <- as.data.frame(final_swz_hf_against)
colnames(final_swz_hf_against) <- "Team against"
#combine the columns
final_swz_all <- cbind(final_swz_hf,final_swz_gs,final_swz_gc,final_swz_tg,final_swz_cs,final_swz_wm,final_swz_hf_against)
write.xlsx(final_swz_all,'NL/SWZ.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
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

#create home and away csv
#swz_home_poisson <- rbind(swz_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#swz_away_poisson <- rbind(swz_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(swz_home_poisson,'NL/SWZ.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(swz_away_poisson,'NL/SWZ.xlsx',sheetName = "awaypoisson", append = TRUE)
swz_home_poisson
swz_away_poisson
##########################################################################################################
###################SWZ FIXTURES##########################################################################
#SWZ
HomeTeam_swz <- rep(swz_teams, each = length(swz_teams))
AwayTeam_swz <- rep(swz_teams, length(swz_teams))
SWZ_fixtures <- cbind(HomeTeam_swz,AwayTeam_swz)
SWZ_fixtures <- as.data.frame(SWZ_fixtures)
SWZ_fixtures <- SWZ_fixtures[!SWZ_fixtures$HomeTeam_swz == SWZ_fixtures$AwayTeam_swz,]
rownames(SWZ_fixtures) <- NULL
SWZ_fixtures$Div <- "SWZ"
SWZ_fixtures <- SWZ_fixtures[,c(3,1,2)]

SWZ_fixtures$avg_HG_swz <- swz_avg_HG

SWZ_fixtures$swz_homeas <- rep(swz_home_as,each = length(swz_teams)-1)

swz_awayds_lookup <- cbind(swz_teams,swz_away_ds)

swz_awayds_lookup <- as.data.frame(swz_awayds_lookup)

colnames(swz_awayds_lookup) <- c("AwayTeam_swz","swz_awayds")


require('RH2')
SWZ_fixtures$swz_awayds <- sqldf("SELECT swz_awayds_lookup.swz_awayds FROM swz_awayds_lookup INNER JOIN SWZ_fixtures ON swz_awayds_lookup.AwayTeam_swz = SWZ_fixtures.AwayTeam_swz")

SWZ_fixtures$avg_AG_swz <- swz_avg_AG

swz_awayas_lookup <- cbind(swz_teams,swz_away_as)

swz_awayas_lookup <- as.data.frame(swz_awayas_lookup)

colnames(swz_awayas_lookup) <- c("AwayTeam_swz","swz_awayas")


SWZ_fixtures$swz_awayas <- sqldf("SELECT swz_awayas_lookup.swz_awayas FROM swz_awayas_lookup INNER JOIN SWZ_fixtures ON swz_awayas_lookup.AwayTeam_swz = SWZ_fixtures.AwayTeam_swz")

SWZ_fixtures$swz_homeds <- rep(swz_home_ds,each = length(swz_teams)-1)

SWZ_fixtures$swz_awayds <- as.numeric(unlist(SWZ_fixtures$swz_awayds))
#xGH
SWZ_fixtures$swz_xGH <- SWZ_fixtures$avg_HG_swz * SWZ_fixtures$swz_homeas * SWZ_fixtures$swz_awayds

#xGA

SWZ_fixtures$swz_awayas <- as.numeric(unlist(SWZ_fixtures$swz_awayas))

SWZ_fixtures$swz_xGA <- SWZ_fixtures$avg_AG_swz * SWZ_fixtures$swz_awayas * SWZ_fixtures$swz_homeds

SWZ_fixtures$swz_0_0 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_0 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_0_1 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_1 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_0 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_0_2 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_2 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_1 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_2 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_3 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_0 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_1 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_2 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_0_3 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_3 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_3 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_4 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_0 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_1 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_2 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_3 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_0_4 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_4 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_4 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_4 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_5 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_0 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_1 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_2 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_3 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_4 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_0_5 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_5 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_5 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_5 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_5 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_6 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_0 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(0,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_1 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(1,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_2 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(2,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_3 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(3,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_4 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(4,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_6_5 <- round(stats::dpois(6,SWZ_fixtures$swz_xGH) * stats::dpois(5,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_0_6 <- round(stats::dpois(0,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_1_6 <- round(stats::dpois(1,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_2_6 <- round(stats::dpois(2,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_3_6 <- round(stats::dpois(3,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_4_6 <- round(stats::dpois(4,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
SWZ_fixtures$swz_5_6 <- round(stats::dpois(5,SWZ_fixtures$swz_xGH) * stats::dpois(6,SWZ_fixtures$swz_xGA), digits = 4)
#Home win
SWZ_fixtures$swz_H <- (
  SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
    SWZ_fixtures$swz_5_0 + SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5
)

SWZ_fixtures$swz_H <- percent(SWZ_fixtures$swz_H, accuracy = 0.1)

#Draw
SWZ_fixtures$swz_D <- (

  SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 + SWZ_fixtures$swz_4_4 +
    SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6
)

SWZ_fixtures$swz_D <- percent(SWZ_fixtures$swz_D, accuracy = 0.1)

#Away

SWZ_fixtures$swz_A <- (
  SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
    SWZ_fixtures$swz_0_5 + SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6
)

SWZ_fixtures$swz_A <- percent(SWZ_fixtures$swz_A, accuracy = 0.1)

#ov25
SWZ_fixtures$swz_ov25 <- (
  SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 + SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 + SWZ_fixtures$swz_0_4 +
    SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 + SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_0 +
    SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 + SWZ_fixtures$swz_0_5 +
    SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 + SWZ_fixtures$swz_5_5 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5 + SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 +
    SWZ_fixtures$swz_4_6 + SWZ_fixtures$swz_5_6 + SWZ_fixtures$swz_6_6
)
#un25
SWZ_fixtures$swz_un25 <- (
  SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_0_2
)
#odds
SWZ_fixtures$swz_ov25_odds <- round((1/SWZ_fixtures$swz_ov25),digits = 2)
SWZ_fixtures$swz_un25_odds <- round((1/SWZ_fixtures$swz_un25),digits = 2)

SWZ_fixtures$swz_ov25_odds
SWZ_fixtures$swz_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
SWZ_fixtures$swz_BTTSY <- (
  SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_3_1 + SWZ_fixtures$swz_3_2 +
    SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_1_3 + SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_3_3 + SWZ_fixtures$swz_4_4 +
    SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_3 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 +
    SWZ_fixtures$swz_3_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 +
    SWZ_fixtures$swz_5_4 + SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_6_6 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6
)
#BTTSN
SWZ_fixtures$swz_BTTSN <- (
  SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_0_2 +
    SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_5_0 +
    SWZ_fixtures$swz_0_5 + SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_0_6
)

SWZ_fixtures$swz_BTTSY_odds <- round((1/SWZ_fixtures$swz_BTTSY),digits = 2)
SWZ_fixtures$swz_BTTSN_odds <- round((1/SWZ_fixtures$swz_BTTSN),digits = 2)

SWZ_fixtures$swz_BTTSY <- percent(SWZ_fixtures$swz_BTTSY, accuracy = 0.1)
SWZ_fixtures$swz_BTTSN <- percent(SWZ_fixtures$swz_BTTSN, accuracy = 0.1)
#odds
SWZ_fixtures$swz_BTTSY_odds
SWZ_fixtures$swz_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
SWZ_fixtures$swz_AH_0_H <- (
  SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
    SWZ_fixtures$swz_5_0 +SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5 + SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6
)
#AH_0_A
SWZ_fixtures$swz_AH_0_A <- (
  SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
    SWZ_fixtures$swz_0_5 +SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6 + SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6
)

#odds
SWZ_fixtures$swz_AH_0_H_odds <- round((1/SWZ_fixtures$swz_AH_0_H),digits = 2)
SWZ_fixtures$swz_AH_0_A_odds <- round((1/SWZ_fixtures$swz_AH_0_A),digits = 2)

SWZ_fixtures$swz_AH_0_H_odds
SWZ_fixtures$swz_AH_0_A_odds
#percentages
SWZ_fixtures$swz_AH_0_H <- percent(SWZ_fixtures$swz_AH_0_H, accuracy = 0.1)
SWZ_fixtures$swz_AH_0_A <- percent(SWZ_fixtures$swz_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
SWZ_fixtures$swz_AH_n075_H <- (
  SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
    SWZ_fixtures$swz_5_0 +SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5
)
#AH_n075_A
SWZ_fixtures$swz_AH_n075_A <- (
  SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
    SWZ_fixtures$swz_0_5 +SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6
)

#odds
SWZ_fixtures$swz_AH_n075_H_odds <- round((1/SWZ_fixtures$swz_AH_n075_H),digits = 2)
SWZ_fixtures$swz_AH_n075_A_odds <- round((1/SWZ_fixtures$swz_AH_n075_A),digits = 2)

SWZ_fixtures$swz_AH_n075_H_odds
SWZ_fixtures$swz_AH_n075_A_odds
#percentages
SWZ_fixtures$swz_AH_n075_H <- percent(SWZ_fixtures$swz_AH_n075_H, accuracy = 0.1)
SWZ_fixtures$swz_AH_n075_A <- percent(SWZ_fixtures$swz_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
SWZ_fixtures$swz_AH_075_H <- (
  SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
    SWZ_fixtures$swz_5_0 +SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5 + SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6 + SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_1_2 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_3_4 + SWZ_fixtures$swz_4_5 + SWZ_fixtures$swz_5_6
)
#AH_075_A
SWZ_fixtures$swz_AH_075_A <- (
  SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
    SWZ_fixtures$swz_0_5 +SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6 + SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6 + SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_3 + SWZ_fixtures$swz_5_4 + SWZ_fixtures$swz_6_5
)

#odds
SWZ_fixtures$swz_AH_075_H_odds <- round((1/SWZ_fixtures$swz_AH_075_H),digits = 2)
SWZ_fixtures$swz_AH_075_A_odds <- round((1/SWZ_fixtures$swz_AH_075_A),digits = 2)

SWZ_fixtures$swz_AH_075_H_odds
SWZ_fixtures$swz_AH_075_A_odds
#percentages
SWZ_fixtures$swz_AH_075_H <- percent(SWZ_fixtures$swz_AH_075_H, accuracy = 0.1)
SWZ_fixtures$swz_AH_075_A <- percent(SWZ_fixtures$swz_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
SWZ_fixtures$swz_AH_n125_H <- (
  SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
    SWZ_fixtures$swz_5_0 +SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5
)
#AH_n125_A
SWZ_fixtures$swz_AH_n125_A <- (
  SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
    SWZ_fixtures$swz_0_5 +SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6
)

#odds
SWZ_fixtures$swz_AH_n125_H_odds <- round((1/SWZ_fixtures$swz_AH_n125_H),digits = 2)
SWZ_fixtures$swz_AH_n125_A_odds <- round((1/SWZ_fixtures$swz_AH_n125_A),digits = 2)

SWZ_fixtures$swz_AH_n125_H_odds
SWZ_fixtures$swz_AH_n125_A_odds
#percentages
SWZ_fixtures$swz_AH_n125_H <- percent(SWZ_fixtures$swz_AH_n125_H, accuracy = 0.1)
SWZ_fixtures$swz_AH_n125_A <- percent(SWZ_fixtures$swz_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
SWZ_fixtures$swz_AH_125_H <- (
  SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_0 + SWZ_fixtures$swz_2_1 + SWZ_fixtures$swz_3_0 + SWZ_fixtures$swz_3_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_0 + SWZ_fixtures$swz_4_1 + SWZ_fixtures$swz_4_2 + SWZ_fixtures$swz_4_3 +
    SWZ_fixtures$swz_5_0 +SWZ_fixtures$swz_5_1 + SWZ_fixtures$swz_5_2 + SWZ_fixtures$swz_5_3 + SWZ_fixtures$swz_5_4 +
    SWZ_fixtures$swz_6_0 + SWZ_fixtures$swz_6_1 + SWZ_fixtures$swz_6_2 + SWZ_fixtures$swz_6_3 + SWZ_fixtures$swz_6_4 +
    SWZ_fixtures$swz_6_5 + SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6 + SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_1_2 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_3_4 + SWZ_fixtures$swz_4_5 + SWZ_fixtures$swz_5_6
)
#AH_125_A
SWZ_fixtures$swz_AH_125_A <- (
  SWZ_fixtures$swz_0_1 + SWZ_fixtures$swz_0_2 + SWZ_fixtures$swz_1_2 + SWZ_fixtures$swz_0_3 + SWZ_fixtures$swz_1_3 +
    SWZ_fixtures$swz_2_3 + SWZ_fixtures$swz_0_4 + SWZ_fixtures$swz_1_4 + SWZ_fixtures$swz_2_4 + SWZ_fixtures$swz_3_4 +
    SWZ_fixtures$swz_0_5 +SWZ_fixtures$swz_1_5 + SWZ_fixtures$swz_2_5 + SWZ_fixtures$swz_3_5 + SWZ_fixtures$swz_4_5 +
    SWZ_fixtures$swz_0_6 + SWZ_fixtures$swz_1_6 + SWZ_fixtures$swz_2_6 + SWZ_fixtures$swz_3_6 + SWZ_fixtures$swz_4_6 +
    SWZ_fixtures$swz_5_6 + SWZ_fixtures$swz_0_0 + SWZ_fixtures$swz_1_1 + SWZ_fixtures$swz_2_2 + SWZ_fixtures$swz_3_3 +
    SWZ_fixtures$swz_4_4 + SWZ_fixtures$swz_5_5 + SWZ_fixtures$swz_6_6 + SWZ_fixtures$swz_1_0 + SWZ_fixtures$swz_2_1 +
    SWZ_fixtures$swz_3_2 + SWZ_fixtures$swz_4_3 + SWZ_fixtures$swz_5_4 + SWZ_fixtures$swz_6_5
)

#odds
SWZ_fixtures$swz_AH_125_H_odds <- round((1/SWZ_fixtures$swz_AH_125_H),digits = 2)
SWZ_fixtures$swz_AH_125_A_odds <- round((1/SWZ_fixtures$swz_AH_125_A),digits = 2)

SWZ_fixtures$swz_AH_125_H_odds
SWZ_fixtures$swz_AH_125_A_odds
#percentages
SWZ_fixtures$swz_AH_125_H <- percent(SWZ_fixtures$swz_AH_125_H, accuracy = 0.1)
SWZ_fixtures$swz_AH_125_A <- percent(SWZ_fixtures$swz_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
SWZ_fixtures$swz_ov25 <- percent(SWZ_fixtures$swz_ov25, accuracy = 0.1)

SWZ_fixtures$swz_un25 <- percent(SWZ_fixtures$swz_un25, accuracy = 0.1)
SWZ_fixtures$swz_pscore <- paste(round(SWZ_fixtures$swz_xGH,digits = 0),round(SWZ_fixtures$swz_xGA,digits = 0),sep = "-")
#write out
write.xlsx(SWZ_fixtures,'NL/SWZ.xlsx',sheetName = "SWZ", append = TRUE)
###########################################################################################################
########################SWZ END###########################################################################
SWZ <- read.csv('../FDAS/SWZ.csv')
SWZ$TG <- SWZ$HG + SWZ$AG
SWZ$OV25 <- ifelse(SWZ$TG >= 3,"Y","N")
swz_ftr_summary <- tabyl(SWZ,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
swz_ov25_summary <- tabyl(SWZ,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(swz_ftr_summary,'NL/SWZ.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(swz_ov25_summary,'NL/SWZ.xlsx',sheetName = "OVUN25", append = TRUE)



