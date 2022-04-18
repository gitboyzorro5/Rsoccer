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
unlink('NL/JPN.xlsx')
######################JPN START#######################################
#####################################################################
JPN <- read.csv('../FDAS/JPN.csv')
JPN <- within(JPN,rm(Res))
JPN$Date <- dmy(JPN$Date)
JPN <- JPN[order(as.Date(JPN$Date, format = "%d/%m%Y"), decreasing = FALSE),]
JPN$CS <- paste(JPN$HG,JPN$AG, sep = "-")
#JPN_qualificaton <- subset(JPN,tournament == "UEFA Euro qualification")
JPN <- subset(JPN,Season == "2021")
#JPN <- JPN[JPN$Date > '2008-01-01',])
JPN$TG <- JPN$HG + JPN$AG
JPN$OV25 <- ifelse(JPN$TG >= 3,"Y","N")
JPN$FTR <- with(JPN,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
####GoalTotalsv2##################################
jpn_totalgoalsv2 <- tapply(JPN$TG, JPN[c("Home", "Away")],mean)
jpn_totalgoalsv2
jpn_hgtotals <- rowSums(jpn_totalgoalsv2,na.rm = T)
jpn_agtotals <- colSums(jpn_totalgoalsv2,na.rm = T)

jpn_totalgoals <- jpn_hgtotals + jpn_agtotals
jpn_totalgoalsv2 <- cbind(jpn_totalgoalsv2,jpn_totalgoals)
jpn_teams <- sort(unique(JPN$Home))
jpn_home_games <- c()
jpn_away_games <-c()
for (i_jpn in 1:length(jpn_teams))
{

  jpn_home_games[i_jpn] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn],])
  jpn_away_games[i_jpn]  <- nrow(JPN[JPN$Away == jpn_teams[i_jpn],])

}
jpn_games_played <- jpn_home_games + jpn_away_games
jpn_goaltotalsv2 <- cbind(jpn_totalgoalsv2,jpn_games_played)
jpn_avg_totalgoals <- round((jpn_totalgoals/ jpn_games_played), digits = 4)
jpn_goaltotalsv2[is.na(jpn_goaltotalsv2)] <- ""
jpn_goaltotalsv2 <- cbind(jpn_goaltotalsv2,jpn_avg_totalgoals)
write.xlsx(jpn_goaltotalsv2,'NL/JPN.xlsx',sheetName = "totalgoalsv2")
#####################################################################
JPN <- subset(JPN,Season == "2021")
jpn_totalrounds <-  (length(jpn_teams) - 1 )*2
jpn_totalmatches <- (length(jpn_teams)*(length(jpn_teams) - 1))
jpn_eachround <- jpn_totalmatches / jpn_totalrounds

jpn_matchesplayed <-  nrow(JPN)

JPN_rounds <- JPN

if(jpn_matchesplayed %% jpn_eachround == 0)
{
  jpn_currentround <- jpn_matchesplayed / jpn_eachround
  jpn_matchday <- c()
  jpn_matchday <- rep(1:jpn_currentround, each = jpn_eachround)
}else if(jpn_matchesplayed %% jpn_eachround != 0)

{

  jpn_modulus <- jpn_matchesplayed %% jpn_eachround
  jpn_currentround <- (jpn_matchesplayed - jpn_modulus) / jpn_eachround
  jpn_matchday <- c()
  jpn_matchday_vjpn1 <- c()
  jpn_matchday_vjpn2 <- c()
  jpn_matchday_vjpn1 <- rep(1:jpn_currentround, each = jpn_eachround)
  jpn_matchday_vjpn2[1:jpn_modulus] <- c(jpn_currentround + 1)
  jpn_matchday <- append(jpn_matchday_vjpn1,jpn_matchday_vjpn2)
}
JPN_rounds <- cbind(JPN_rounds,jpn_matchday)
###################################################################################################################
jpn_goalscored_h <- tapply(JPN$HG, JPN[c("Home", "Date")],mean)
jpn_goalscored_a <- tapply(JPN$AG, JPN[c("Away", "Date")],mean)
jpn_goalscored_h[is.na(jpn_goalscored_h)] <- ""
jpn_goalscored_a[is.na(jpn_goalscored_a)] <- ""

for(jpn_rowhgs in 1:nrow(jpn_goalscored_h)) {
  for(jpn_colhgs in 1:ncol(jpn_goalscored_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowags in 1:nrow(jpn_goalscored_a)) {
      for(jpn_colags in 1:ncol(jpn_goalscored_a)) {
        ifelse(!jpn_goalscored_a[jpn_rowags,jpn_colags]=="",jpn_goalscored_h[jpn_rowags,jpn_colags] <- jpn_goalscored_a[jpn_rowags,jpn_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(jpn_goalscoredmatrix,'NL/JPN.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#########################################################################################
#jpn goal scored rounds
final_jpn_gs <- matrix(nrow = length(jpn_teams),ncol = jpn_totalrounds )
suml6_jpn_gs <- c()
sum_jpn_zero_gs <- c()
sum_jpn_one_gs <- c()
sum_jpn_two_gs <- c()
sum_jpn_three_gs <- c()
l6_form_jpn_gssplitted <- c()
form_jpn_gs <- c()
for(index_jpn_gs in 1:length(jpn_teams))
{
  for(index_jpn_gs_cols in 1:jpn_totalrounds)
  {
    index_jpn_gs  <- row.names(jpn_goalscored_h) == jpn_teams[index_jpn_gs]
    form_jpn_gs <- jpn_goalscored_h[index_jpn_gs ]
    deleted_form_jpn_gs <- form_jpn_gs[!form_jpn_gs[] == ""]
    l6_form_jpn_gs <- tail(deleted_form_jpn_gs,jpn_last_n_games)
    l6_form_jpn_gs <- as.numeric(l6_form_jpn_gs)
    suml6_jpn_gs[index_jpn_gs] <- sum(l6_form_jpn_gs)
    suml6_jpn_gs[index_jpn_gs] <- paste(suml6_jpn_gs[index_jpn_gs],sep = "")
    sum_jpn_zero_gs[index_jpn_gs] <- length(which(l6_form_jpn_gs == 0))
    sum_jpn_zero_gs[index_jpn_gs] <- paste(sum_jpn_zero_gs[index_jpn_gs],sep = "")
    sum_jpn_one_gs[index_jpn_gs] <- length(which(l6_form_jpn_gs == 1))
    sum_jpn_one_gs[index_jpn_gs] <- paste(sum_jpn_one_gs[index_jpn_gs],sep = "")
    sum_jpn_two_gs[index_jpn_gs] <- length(which(l6_form_jpn_gs >= 2))
    sum_jpn_two_gs[index_jpn_gs] <- paste(sum_jpn_two_gs[index_jpn_gs],sep = "")
    sum_jpn_three_gs[index_jpn_gs] <- length(which(l6_form_jpn_gs >= 3))
    sum_jpn_three_gs[index_jpn_gs] <- paste(sum_jpn_three_gs[index_jpn_gs],sep = "")
    l6_form_jpn_gs <- as.character(l6_form_jpn_gs)
    l6_form_jpn_gs_flattened <- stri_paste(l6_form_jpn_gs,collapse = '')
    l6_form_jpn_gssplitted <- as.numeric(strsplit(as.character(l6_form_jpn_gs_flattened),"")[[1]])
    final_jpn_gs[index_jpn_gs,index_jpn_gs_cols] <- l6_form_jpn_gssplitted[index_jpn_gs_cols]
  }
}

final_jpn_gs[is.na(final_jpn_gs)] <- ""
jpn_goalscoredmatrix <- cbind(jpn_teams,final_jpn_gs,suml6_jpn_gs,sum_jpn_zero_gs,sum_jpn_one_gs,sum_jpn_two_gs,sum_jpn_three_gs)
write.xlsx(jpn_goalscoredmatrix,'NL/JPN.xlsx',sheetName = "gsmatrix", append = TRUE)
#################################################################################################################################

####GCmatrix#####################################################################################################################
#create home and away matrices
jpn_goalconceded_h <- tapply(JPN$AG, JPN[c("Home", "Date")],mean)
jpn_goalconceded_a <- tapply(JPN$HG, JPN[c("Away", "Date")],mean)
jpn_goalconceded_h[is.na(jpn_goalconceded_h)] <- ""
jpn_goalconceded_a[is.na(jpn_goalconceded_a)] <- ""

for(jpn_rowhgc in 1:nrow(jpn_goalconceded_h)) {
  for(jpn_colhgc in 1:ncol(jpn_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowagc in 1:nrow(jpn_goalconceded_a)) {
      for(jpn_colagc in 1:ncol(jpn_goalconceded_a)) {
        ifelse(!jpn_goalconceded_a[jpn_rowagc,jpn_colagc]=="",jpn_goalconceded_h[jpn_rowagc,jpn_colagc] <- jpn_goalconceded_a[jpn_rowagc,jpn_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(jpn_goalconcededmatrix,'NL/JPN.xlsx',sheetName = "gcmatrix", append = TRUE)
############################################################################################################################################################
#jpn goal conceded rounds
final_jpn_gc <- matrix(nrow = length(jpn_teams),ncol = jpn_totalrounds )
suml6_jpn_gc <- c()
sum_jpn_zero_gc <- c()
sum_jpn_one_gc <- c()
sum_jpn_two_gc <- c()
sum_jpn_three_gc <- c()
l6_form_jpn_gcsplitted <- c()
form_jpn_gc <- c()
for(index_jpn_gc in 1:length(jpn_teams))
{
  for(index_jpn_gc_cols in 1:jpn_totalrounds)
  {
    index_jpn_gc  <- row.names(jpn_goalconceded_h) == jpn_teams[index_jpn_gc]
    form_jpn_gc <- jpn_goalconceded_h[index_jpn_gc ]
    deleted_form_jpn_gc <- form_jpn_gc[!form_jpn_gc[] == ""]
    l6_form_jpn_gc <- tail(deleted_form_jpn_gc,jpn_last_n_games)
    l6_form_jpn_gc <- as.numeric(l6_form_jpn_gc)
    suml6_jpn_gc[index_jpn_gc] <- sum(l6_form_jpn_gc)
    suml6_jpn_gc[index_jpn_gc] <- paste(suml6_jpn_gc[index_jpn_gc],sep = "")
    sum_jpn_zero_gc[index_jpn_gc] <- length(which(l6_form_jpn_gc == 0))
    sum_jpn_zero_gc[index_jpn_gc] <- paste(sum_jpn_zero_gc[index_jpn_gc],sep = "")
    sum_jpn_one_gc[index_jpn_gc] <- length(which(l6_form_jpn_gc == 1))
    sum_jpn_one_gc[index_jpn_gc] <- paste(sum_jpn_one_gc[index_jpn_gc],sep = "")
    sum_jpn_two_gc[index_jpn_gc] <- length(which(l6_form_jpn_gc >= 2))
    sum_jpn_two_gc[index_jpn_gc] <- paste(sum_jpn_two_gc[index_jpn_gc],sep = "")
    sum_jpn_three_gc[index_jpn_gc] <- length(which(l6_form_jpn_gc >= 3))
    sum_jpn_three_gc[index_jpn_gc] <- paste(sum_jpn_three_gc[index_jpn_gc],sep = "")
    l6_form_jpn_gc <- as.character(l6_form_jpn_gc)
    l6_form_jpn_gc_flattened <- stri_paste(l6_form_jpn_gc,collapse = '')
    l6_form_jpn_gcsplitted <- as.numeric(strsplit(as.character(l6_form_jpn_gc_flattened),"")[[1]])
    final_jpn_gc[index_jpn_gc,index_jpn_gc_cols] <- l6_form_jpn_gcsplitted[index_jpn_gc_cols]
  }
}

final_jpn_gc[is.na(final_jpn_gc)] <- ""
jpn_goalconcededmatrix <- cbind(jpn_teams,final_jpn_gc,suml6_jpn_gc,sum_jpn_zero_gc,sum_jpn_one_gc,sum_jpn_two_gc,sum_jpn_three_gc)
write.xlsx(jpn_goalconcededmatrix,'NL/JPN.xlsx',sheetName = "gcmatrix2", append = TRUE)
###################################################################################################################################

###################################################################################################################################
####Teamform#######################################################################################################################

jpn_form_h <- tapply(JPN$FTR, JPN[c("Home", "Date")],median)
jpn_form_a <- tapply(JPN$FTR, JPN[c("Away", "Date")],median)
jpn_form_h[is.na(jpn_form_h)] <- ""
jpn_form_a[is.na(jpn_form_a)] <- ""
jpn_form_h <- sub("A","L",jpn_form_h)
jpn_form_h <- sub("H","W",jpn_form_h)
jpn_form_a <- sub("A","W",jpn_form_a)
jpn_form_a <- sub("H","L",jpn_form_a)
for(jpn_rowh_f in 1:nrow(jpn_form_h)) {
  for(jpn_colh_f in 1:ncol(jpn_form_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa_f in 1:nrow(jpn_form_a)) {
      for(jpn_cola_f in 1:ncol(jpn_form_a)) {
        ifelse(!jpn_form_a[jpn_rowa_f,jpn_cola_f]=="",jpn_form_h[jpn_rowa_f,jpn_cola_f] <- jpn_form_a[jpn_rowa_f,jpn_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#jpn team form
final_jpn_hf <- matrix(nrow = length(jpn_teams),ncol = jpn_totalrounds )
suml6_jpn_hf <- c()
l6_form_jpn_hfsplitted <- c()
form_jpn_hf <- c()
for(index_jpn_hf in 1:length(jpn_teams))
{
  for(index_jpn_hf_cols in 1:jpn_totalrounds)
  {
    index_jpn_hf  <- row.names(jpn_form_h) == jpn_teams[index_jpn_hf]
    form_jpn_hf <- jpn_form_h[index_jpn_hf ]
    deleted_form_jpn_hf <- form_jpn_hf[!form_jpn_hf[] == ""]
    l6_form_jpn_hf <- tail(deleted_form_jpn_hf,jpn_last_n_games)
    # #l6_form_jpn_hf <- as.numeric(l6_form_jpn_hf)
    # suml6_jpn_hf[index_jpn_hf] <- sum(l6_form_jpn_hf)
    # suml6_jpn_hf[index_jpn_hf] <- paste(suml6_jpn_hf[index_jpn_hf],sep = "")
    #l6_form_jpn_hf <- as.character(l6_form_jpn_hf)
    l6_form_jpn_hf_flattened <- stri_paste(l6_form_jpn_hf,collapse = '')
    l6_form_jpn_hfsplitted <- (strsplit(as.character(l6_form_jpn_hf_flattened),"")[[1]])
    final_jpn_hf[index_jpn_hf,index_jpn_hf_cols] <- l6_form_jpn_hfsplitted[index_jpn_hf_cols]
  }
}
final_jpn_hf[is.na(final_jpn_hf)] <- ""
jpn_formmatrix <- cbind(jpn_teams,final_jpn_hf)

write.xlsx(jpn_formmatrix,'NL/JPN.xlsx',sheetName = "form", append = TRUE)
######################################################################################################################################
######################################################################################################################################

#######TGMatrix#######################################################################################################################
jpn_totalgoals_h <- tapply(JPN$TG, JPN[c("Home", "Date")],mean)
jpn_totalgoals_a <- tapply(JPN$TG, JPN[c("Away", "Date")],mean)
jpn_totalgoals_h[is.na(jpn_totalgoals_h)] <- ""
jpn_totalgoals_a[is.na(jpn_totalgoals_a)] <- ""
for(jpn_rowh in 1:nrow(jpn_totalgoals_h)) {
  for(jpn_colh in 1:ncol(jpn_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa in 1:nrow(jpn_totalgoals_a)) {
      for(jpn_cola in 1:ncol(jpn_totalgoals_a)) {
        ifelse(!jpn_totalgoals_a[jpn_rowa,jpn_cola]=="",jpn_totalgoals_h[jpn_rowa,jpn_cola] <- jpn_totalgoals_a[jpn_rowa,jpn_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#jpn total goals rounds
#jpn
final_jpn_tg <- matrix(nrow = length(jpn_teams),ncol = jpn_totalrounds )
suml6_jpn_tg <- c()
l6_form_jpn_tgsplitted <- c()
form_jpn_tg <- c()
for(index_jpn_tg in 1:length(jpn_teams))
{
  for(index_jpn_tg_cols in 1:jpn_totalrounds)
  {
    index_jpn_tg  <- row.names(jpn_totalgoals_h) == jpn_teams[index_jpn_tg]
    form_jpn_tg <- jpn_totalgoals_h[index_jpn_tg ]
    deleted_form_jpn_tg <- form_jpn_tg[!form_jpn_tg[] == ""]
    l6_form_jpn_tg <- tail(deleted_form_jpn_tg,jpn_last_n_games)
    l6_form_jpn_tg <- as.numeric(l6_form_jpn_tg)
    suml6_jpn_tg[index_jpn_tg] <- sum(l6_form_jpn_tg)
    suml6_jpn_tg[index_jpn_tg] <- paste(suml6_jpn_tg[index_jpn_tg],sep = "")
    l6_form_jpn_tg <- as.character(l6_form_jpn_tg)
    l6_form_jpn_tg_flattened <- stri_paste(l6_form_jpn_tg,collapse = '')
    l6_form_jpn_tgsplitted <- as.numeric(strsplit(as.character(l6_form_jpn_tg_flattened),"")[[1]])
    final_jpn_tg[index_jpn_tg,index_jpn_tg_cols] <- l6_form_jpn_tgsplitted[index_jpn_tg_cols]
  }
}

final_jpn_tg[is.na(final_jpn_tg)] <- ""
jpn_goaltotalmatrix <- cbind(jpn_teams,final_jpn_tg,suml6_jpn_tg)

write.xlsx(jpn_goaltotalmatrix,'NL/JPN.xlsx',sheetName = "tgmatrix", append = TRUE)
#############################################################################################################################################
#######TeamAgainst###########################################################################################################################
jpn_form_team_against_h <- tapply(JPN$Away, JPN[c("Home", "Date")],median)
jpn_form_team_against_a <- tapply(JPN$Home, JPN[c("Away", "Date")],median)
jpn_form_team_against_h[is.na(jpn_form_team_against_h)] <- ""
jpn_form_team_against_a[is.na(jpn_form_team_against_a)] <- ""
for(jpn_rowh_f_against in 1:nrow(jpn_form_team_against_h)) {
  for(jpn_colh_f_against in 1:ncol(jpn_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa_f_against in 1:nrow(jpn_form_team_against_a)) {
      for(jpn_cola_f_against in 1:ncol(jpn_form_team_against_a)) {
        ifelse(!jpn_form_team_against_a[jpn_rowa_f_against,jpn_cola_f_against]=="",jpn_form_team_against_h[jpn_rowa_f_against,jpn_cola_f_against] <- jpn_form_team_against_a[jpn_rowa_f_against,jpn_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#win margin
jpn_winmargin_h <- tapply(JPN$HG - JPN$AG, JPN[c("Home", "Date")],mean)
jpn_winmargin_a <- tapply(JPN$AG - JPN$HG, JPN[c("Away", "Date")],mean)
jpn_winmargin_h[is.na(jpn_winmargin_h)] <- ""
#
for(jpn_rowhwm in 1:nrow(jpn_winmargin_h)) {
  for(jpn_colhwm in 1:ncol(jpn_winmargin_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowawm in 1:nrow(jpn_winmargin_a)) {
      for(jpn_colawm in 1:ncol(jpn_winmargin_a)) {
        ifelse(!jpn_winmargin_a[jpn_rowawm,jpn_colawm]=="",jpn_winmargin_h[jpn_rowawm,jpn_colawm] <- jpn_winmargin_a[jpn_rowawm,jpn_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
final_jpn_wm <- matrix(nrow = length(jpn_teams),ncol = jpn_totalrounds )
suml6_jpn_wm <- c()
suml6_jpn_wm_negone <- c()
suml6_jpn_wm_negtwo <- c()
suml6_jpn_wm_zero <- c()
suml6_jpn_wm_posone <- c()
suml6_jpn_wm_postwo <- c()
l6_form_jpn_wmsplitted <- c()
form_jpn_wm <- c()
for(index_jpn_wm in 1:length(jpn_teams))
{
  for(index_jpn_wm_cols in 1:jpn_totalrounds)
  {
    index_jpn_wm  <- row.names(jpn_winmargin_h) == jpn_teams[index_jpn_wm]
    form_jpn_wm <- jpn_winmargin_h[index_jpn_wm ]
    deleted_form_jpn_wm <- form_jpn_wm[!form_jpn_wm[] == ""]
    l6_form_jpn_wm <- tail(deleted_form_jpn_wm,jpn_last_n_games)
    l6_form_jpn_wm <- as.numeric(l6_form_jpn_wm)
    suml6_jpn_wm[index_jpn_wm] <- sum(l6_form_jpn_wm)
    suml6_jpn_wm[index_jpn_wm] <- paste(suml6_jpn_wm[index_jpn_wm],sep = "")
    suml6_jpn_wm_negone[index_jpn_wm] <- length(which(l6_form_jpn_wm == -1))
    suml6_jpn_wm_negone[index_jpn_wm] <- paste(suml6_jpn_wm_negone[index_jpn_wm],sep = "")
    suml6_jpn_wm_negtwo[index_jpn_wm] <- length(which(l6_form_jpn_wm <= -2))
    suml6_jpn_wm_negtwo[index_jpn_wm] <- paste(suml6_jpn_wm_negtwo[index_jpn_wm],sep = "")
    suml6_jpn_wm_zero[index_jpn_wm] <- length(which(l6_form_jpn_wm == 0))
    suml6_jpn_wm_zero[index_jpn_wm] <- paste(suml6_jpn_wm_zero[index_jpn_wm],sep = "")
    suml6_jpn_wm_posone[index_jpn_wm] <- length(which(l6_form_jpn_wm == 1))
    suml6_jpn_wm_posone[index_jpn_wm] <- paste(suml6_jpn_wm_posone[index_jpn_wm],sep = "")
    suml6_jpn_wm_postwo[index_jpn_wm] <- length(which(l6_form_jpn_wm == 2))
    suml6_jpn_wm_postwo[index_jpn_wm] <- paste(suml6_jpn_wm_postwo[index_jpn_wm],sep = "")
    l6_form_jpn_wm <- as.character(l6_form_jpn_wm)
    l6_form_jpn_wm_flattened <- stri_paste(l6_form_jpn_wm,collapse = ',')
    l6_form_jpn_wmsplitted <- (strsplit(as.character(l6_form_jpn_wm_flattened),",")[[1]])
    final_jpn_wm[index_jpn_wm,index_jpn_wm_cols] <- l6_form_jpn_wmsplitted[index_jpn_wm_cols]
  }
}

final_jpn_wm[is.na(final_jpn_wm)] <- ""
jpn_winmarginmatrix <- cbind(jpn_teams,final_jpn_wm,suml6_jpn_wm,suml6_jpn_wm_negtwo,suml6_jpn_wm_negone,suml6_jpn_wm_zero,suml6_jpn_wm_posone,suml6_jpn_wm_postwo)
write.xlsx(jpn_winmarginmatrix,'NL/JPN.xlsx',sheetName = "winmargin", append = TRUE)
####################################################################################################################
##########Goals over under##########################################################################################
#JPN
jpn_un05_home <- c()
jpn_un05_away <- c()
jpn_ov05_home <- c()
jpn_ov05_away <- c()

jpn_un15_home <- c()
jpn_un15_away <- c()
jpn_ov15_home <- c()
jpn_ov15_away <- c()

jpn_un25_home <- c()
jpn_un25_away <- c()
jpn_ov25_home <- c()
jpn_ov25_away <- c()

jpn_un35_home <- c()
jpn_un35_away <- c()
jpn_ov35_home <- c()
jpn_ov35_away <- c()

jpn_un45_home <- c()
jpn_un45_away <- c()
jpn_ov45_home <- c()
jpn_ov45_away <- c()

jpn_un55_home <- c()
jpn_un55_away <- c()
jpn_ov55_home <- c()
jpn_ov55_away <- c()

for (i_jpn_tg in 1:length(jpn_teams))
{

  jpn_un05_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG == 0,])
  jpn_un05_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG == 0,])

  jpn_ov05_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG > 0,])
  jpn_ov05_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG > 0,])

  jpn_un15_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 1,])
  jpn_un15_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 1,])

  jpn_ov15_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 2,])
  jpn_ov15_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 2,])

  jpn_un25_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 2,])
  jpn_un25_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 2,])

  jpn_ov25_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >=3,])
  jpn_ov25_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >=3,])

  jpn_un35_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 3,])
  jpn_un35_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 3,])

  jpn_ov35_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 4,])
  jpn_ov35_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 4,])

  jpn_un45_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 4,])
  jpn_un45_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 4,])

  jpn_ov45_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 5,])
  jpn_ov45_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 5,])

  jpn_un55_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG <= 5,])
  jpn_un55_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG <= 5,])

  jpn_ov55_home[i_jpn_tg] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_tg] & JPN$TG >= 6,])
  jpn_ov55_away[i_jpn_tg] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_tg] & JPN$TG >= 6,])


}

jpn_un05 <- jpn_un05_home + jpn_un05_away
jpn_ov05 <- jpn_ov05_home + jpn_ov05_away

jpn_un15 <- jpn_un15_home + jpn_un15_away
jpn_ov15 <- jpn_ov15_home + jpn_ov15_away

jpn_un25 <- jpn_un25_home + jpn_un25_away
jpn_ov25 <- jpn_ov25_home + jpn_ov25_away

jpn_un35 <- jpn_un35_home + jpn_un35_away
jpn_ov35 <- jpn_ov35_home + jpn_ov35_away

jpn_un45 <- jpn_un45_home + jpn_un45_away
jpn_ov45 <- jpn_ov45_home + jpn_ov45_away

jpn_un55 <- jpn_un55_home + jpn_un55_away
jpn_ov55 <- jpn_ov55_home + jpn_ov55_away

jpn_ovundata <- cbind(jpn_teams,jpn_un05,jpn_ov05,jpn_un15,jpn_ov15,jpn_un25,jpn_ov25,jpn_un35,jpn_ov35,jpn_un45,jpn_ov45,jpn_un55,jpn_ov55)
write.xlsx(jpn_ovundata,'NL/JPN.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
jpn_csform_h <- tapply(JPN$CS, JPN[c("Home", "Date")],median)
jpn_csform_a <- tapply(JPN$CS, JPN[c("Away", "Date")],median)

jpn_csform_h[is.na(jpn_csform_h)] <- ""
jpn_csform_a[is.na(jpn_csform_a)] <- ""

for(jpn_rowh_f_cs in 1:nrow(jpn_csform_h)) {
  for(jpn_colh_f_cs in 1:ncol(jpn_csform_h)) {

    # print(my_matrix[row, col])
    for(jpn_rowa_f_cs in 1:nrow(jpn_csform_a)) {
      for(jpn_cola_f_cs in 1:ncol(jpn_csform_a)) {
        ifelse(!jpn_csform_a[jpn_rowa_f_cs,jpn_cola_f_cs]=="",jpn_csform_h[jpn_rowa_f_cs,jpn_cola_f_cs] <- jpn_csform_a[jpn_rowa_f_cs,jpn_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
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

######################################################################################
###########League Table###############################################################

#hwins and away wins
jpn_home_wins <- c()
jpn_away_wins <- c()
jpn_home_draws <- c()
jpn_away_draws <- c()
jpn_home_loss <- c()
jpn_away_loss <- c()



for (i_jpn_wins in 1:length(jpn_teams))
{

  jpn_home_wins[i_jpn_wins] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_wins] & JPN$FTR == "H",])
  jpn_away_wins[i_jpn_wins] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_wins] & JPN$FTR == "A",])
  jpn_home_draws[i_jpn_wins] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_wins] & JPN$FTR == "D",])
  jpn_away_draws[i_jpn_wins] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_wins] & JPN$FTR == "D",])
  jpn_home_loss[i_jpn_wins] <- nrow(JPN[JPN$Home == jpn_teams[i_jpn_wins] & JPN$FTR == "A",])
  jpn_away_loss[i_jpn_wins] <- nrow(JPN[JPN$Away == jpn_teams[i_jpn_wins] & JPN$FTR == "H",])

}

jpn_total_wins <- jpn_home_wins + jpn_away_wins
jpn_total_draws <- jpn_home_draws + jpn_away_draws
jpn_total_loss <- jpn_home_loss + jpn_away_loss

jpn_league_table <- cbind(jpn_teams,jpn_games_played,jpn_total_wins,jpn_total_draws,jpn_total_loss)
jpn_GS <- jpn_scoring$TGS
jpn_GC <-jpn_conceding$TGC
jpn_GD <- jpn_scoring$TGS - jpn_conceding$TGC
jpn_PTS <- (jpn_total_wins*3) + (jpn_total_draws*1)
jpn_league_table <- cbind(jpn_league_table,jpn_GS,jpn_GC,jpn_GD,jpn_PTS)
jpn_league_table <- as.data.frame(jpn_league_table)
#rename the columns
names(jpn_league_table)[names(jpn_league_table) == "jpn_teams"] <- "Team"
names(jpn_league_table)[names(jpn_league_table) == "jpn_games_played"] <- "P"
names(jpn_league_table)[names(jpn_league_table) == "jpn_total_wins"] <- "W"
names(jpn_league_table)[names(jpn_league_table) == "jpn_total_draws"] <- "D"
names(jpn_league_table)[names(jpn_league_table) == "jpn_total_loss"] <- "L"
names(jpn_league_table)[names(jpn_league_table) == "jpn_GS"] <- "F"
names(jpn_league_table)[names(jpn_league_table) == "jpn_GC"] <- "A"
points_jpn <- jpn_league_table[order(as.numeric(jpn_league_table$jpn_PTS), decreasing = TRUE),]
points_jpn$jpn_rank <- 1:length(jpn_teams)
row.names(points_jpn) <- points_jpn$jpn_rank
#create final_jpn_hf_against with team ranks in brackets
for(jpn_rowhrank in 1:nrow(jpn_form_team_against_h)) {
  for(jpn_colhrank in 1:ncol(jpn_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank]=="",jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank] <- paste(jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank],"(",points_jpn$jpn_rank[points_jpn$Team ==jpn_form_team_against_h[jpn_rowhrank,jpn_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_jpn,'NL/JPN.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six jpn###################################################
#JPN
#form
#create final_jpn_hf object
#jpn_last_n_games <- 6
final_jpn_hf <- c()
for(index_jpn_hf in 1:length(jpn_teams))
{
  index_jpn_hf <- row.names(jpn_form_h) == jpn_teams[index_jpn_hf]
  form_jpn_hf <- jpn_form_h[index_jpn_hf]
  deleted_form_jpn_hf <- form_jpn_hf[!form_jpn_hf[] == ""]
  l6_form_jpn_hf <- tail(deleted_form_jpn_hf,jpn_last_n_games)
  l6_form_jpn_hf <- paste(l6_form_jpn_hf,collapse = " ")
  final_jpn_hf[index_jpn_hf] <- rbind(paste(jpn_teams[index_jpn_hf],l6_form_jpn_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}

#change column names
final_jpn_hf <- as.data.frame(final_jpn_hf)
colnames(final_jpn_hf) <- "Form"
#goals scored
#create final_jpn_gs object
final_jpn_gs <- c()
suml6_jpn_gs <- c()
for(index_jpn_gs in 1:length(jpn_teams))
{
  index_jpn_gs <- row.names(jpn_goalscored_h) == jpn_teams[index_jpn_gs]
  form_jpn_gs <- jpn_goalscored_h[index_jpn_gs]
  deleted_form_jpn_gs <- form_jpn_gs[!form_jpn_gs[] == ""]
  l6_form_jpn_gs <- tail(deleted_form_jpn_gs,jpn_last_n_games)
  l6_form_jpn_gs <- as.numeric(l6_form_jpn_gs)
  suml6_jpn_gs[index_jpn_gs] <- sum(l6_form_jpn_gs)
  suml6_jpn_gs[index_jpn_gs] <- paste("(",suml6_jpn_gs[index_jpn_gs],")",sep = "")
  l6_form_jpn_gs <- paste(l6_form_jpn_gs,collapse = " ")
  final_jpn_gs[index_jpn_gs] <- rbind(paste(jpn_teams[index_jpn_gs],l6_form_jpn_gs,suml6_jpn_gs[index_jpn_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
final_jpn_gs
#change column names
final_jpn_gs <- as.data.frame(final_jpn_gs)
colnames(final_jpn_gs) <- "Goals scored"
#goal conceded
#create final_jpn_gc object
final_jpn_gc <- c()
suml6_jpn_gc <- c()
for(index_jpn_gc in 1:length(jpn_teams))
{
  index_jpn_gc <- row.names(jpn_goalconceded_h) == jpn_teams[index_jpn_gc]
  form_jpn_gc <- jpn_goalconceded_h[index_jpn_gc]
  deleted_form_jpn_gc <- form_jpn_gc[!form_jpn_gc[] == ""]
  l6_form_jpn_gc <- tail(deleted_form_jpn_gc,jpn_last_n_games)
  l6_form_jpn_gc <- as.numeric(l6_form_jpn_gc)
  suml6_jpn_gc[index_jpn_gc] <- sum(l6_form_jpn_gc)
  suml6_jpn_gc[index_jpn_gc] <- paste("(",suml6_jpn_gc[index_jpn_gc],")",sep = "")
  l6_form_jpn_gc <- paste(l6_form_jpn_gc,collapse = " ")
  final_jpn_gc[index_jpn_gc] <- rbind(paste(jpn_teams[index_jpn_gc],l6_form_jpn_gc,suml6_jpn_gc[index_jpn_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}

#change column names
final_jpn_gc <- as.data.frame(final_jpn_gc)
colnames(final_jpn_gc) <- "Goals conceded"
#total goals
#create final_jpn_tg object
final_jpn_tg <- c()
suml6_jpn_tg <- c()
for(index_jpn_tg in 1:length(jpn_teams))
{
  index_jpn_tg <- row.names(jpn_totalgoals_h) == jpn_teams[index_jpn_tg]
  form_jpn_tg <- jpn_totalgoals_h[index_jpn_tg]
  deleted_form_jpn_tg <- form_jpn_tg[!form_jpn_tg[] == ""]
  l6_form_jpn_tg <- tail(deleted_form_jpn_tg,jpn_last_n_games)
  l6_form_jpn_tg <- as.numeric(l6_form_jpn_tg)
  suml6_jpn_tg[index_jpn_tg] <- sum(l6_form_jpn_tg)
  suml6_jpn_tg[index_jpn_tg] <- paste("(",suml6_jpn_tg[index_jpn_tg],")",sep = "")
  l6_form_jpn_tg <- paste(l6_form_jpn_tg,collapse = " ")
  final_jpn_tg[index_jpn_tg] <- rbind(paste(jpn_teams[index_jpn_tg],l6_form_jpn_tg,suml6_jpn_tg[index_jpn_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
#change column names
final_jpn_tg <- as.data.frame(final_jpn_tg)
colnames(final_jpn_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_jpn_hf object
final_jpn_cs <- c()
for(index_jpn_cs in 1:length(jpn_teams))
{
  index_jpn_cs <- row.names(jpn_csform_h) == jpn_teams[index_jpn_cs]
  csform_jpn_cs <- jpn_csform_h[index_jpn_cs]
  deleted_csform_jpn_cs <- csform_jpn_cs[!csform_jpn_cs[] == ""]
  l6_csform_jpn_cs <- tail(deleted_csform_jpn_cs,jpn_last_n_games)
  l6_csform_jpn_cs <- paste(l6_csform_jpn_cs,collapse = " ")
  final_jpn_cs[index_jpn_cs] <- rbind(paste(jpn_teams[index_jpn_cs],l6_csform_jpn_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",jpn_teams[index],l6_csform)

}

#change column names
final_jpn_cs <- as.data.frame(final_jpn_cs)
colnames(final_jpn_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_jpn_wm object
final_jpn_wm <- c()
suml6_jpn_wm <- c()
for(index_jpn_wm in 1:length(jpn_teams))
{
  index_jpn_wm <- row.names(jpn_winmargin_h) == jpn_teams[index_jpn_wm]
  form_jpn_wm <- jpn_winmargin_h[index_jpn_wm]
  deleted_form_jpn_wm <- form_jpn_wm[!form_jpn_wm[] == ""]
  l6_form_jpn_wm <- tail(deleted_form_jpn_wm,jpn_last_n_games)
  l6_form_jpn_wm <- as.numeric(l6_form_jpn_wm)
  suml6_jpn_wm[index_jpn_wm] <- sum(l6_form_jpn_wm)
  suml6_jpn_wm[index_jpn_wm] <- paste("(",suml6_jpn_wm[index_jpn_wm],")",sep = "")
  l6_form_jpn_wm <- paste(l6_form_jpn_wm,collapse = " ")
  final_jpn_wm[index_jpn_wm] <- rbind(paste(jpn_teams[index_jpn_wm],l6_form_jpn_wm,suml6_jpn_wm[index_jpn_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
final_jpn_wm
#change column names
final_jpn_wm <- as.data.frame(final_jpn_wm)
colnames(final_jpn_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_jpn_hf_against
final_jpn_hf_against <- c()
for(index_jpn_hf_against in 1:length(jpn_teams))
{
  index_jpn_hf_against <- row.names(jpn_form_team_against_h) == jpn_teams[index_jpn_hf_against]
  form_jpn_hf_against <- jpn_form_team_against_h[index_jpn_hf_against]
  deleted_form_jpn_hf_against <- form_jpn_hf_against[!form_jpn_hf_against[] == ""]
  l6_form_jpn_hf_against <- tail(deleted_form_jpn_hf_against,jpn_last_n_games)
  l6_form_jpn_hf_against <- paste(l6_form_jpn_hf_against,collapse = " ")
  final_jpn_hf_against[index_jpn_hf_against] <- rbind(paste(jpn_teams[index_jpn_hf_against],l6_form_jpn_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",jpn_teams[index],l6_form)

}
final_jpn_hf_against <- as.data.frame(final_jpn_hf_against)
colnames(final_jpn_hf_against) <- "Team against"
#combine the columns
final_jpn_all <- cbind(final_jpn_hf,final_jpn_gs,final_jpn_gc,final_jpn_tg,final_jpn_cs,final_jpn_wm,final_jpn_hf_against)
write.xlsx(final_jpn_all,'NL/JPN.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
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

#create home and away csv
#jpn_home_poisson <- rbind(jpn_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#jpn_away_poisson <- rbind(jpn_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(jpn_home_poisson,'NL/JPN.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(jpn_away_poisson,'NL/JPN.xlsx',sheetName = "awaypoisson", append = TRUE)
jpn_home_poisson
jpn_away_poisson
##########################################################################################################
###################JPN FIXTURES##########################################################################
#JPN
HomeTeam_jpn <- rep(jpn_teams, each = length(jpn_teams))
AwayTeam_jpn <- rep(jpn_teams, length(jpn_teams))
JPN_fixtures <- cbind(HomeTeam_jpn,AwayTeam_jpn)
JPN_fixtures <- as.data.frame(JPN_fixtures)
JPN_fixtures <- JPN_fixtures[!JPN_fixtures$HomeTeam_jpn == JPN_fixtures$AwayTeam_jpn,]
rownames(JPN_fixtures) <- NULL
JPN_fixtures$Div <- "JPN"
JPN_fixtures <- JPN_fixtures[,c(3,1,2)]

JPN_fixtures$avg_HG_jpn <- jpn_avg_HG

JPN_fixtures$jpn_homeas <- rep(jpn_home_as,each = length(jpn_teams)-1)

jpn_awayds_lookup <- cbind(jpn_teams,jpn_away_ds)

jpn_awayds_lookup <- as.data.frame(jpn_awayds_lookup)

colnames(jpn_awayds_lookup) <- c("AwayTeam_jpn","jpn_awayds")


require('RH2')
JPN_fixtures$jpn_awayds <- sqldf("SELECT jpn_awayds_lookup.jpn_awayds FROM jpn_awayds_lookup INNER JOIN JPN_fixtures ON jpn_awayds_lookup.AwayTeam_jpn = JPN_fixtures.AwayTeam_jpn")

JPN_fixtures$avg_AG_jpn <- jpn_avg_AG

jpn_awayas_lookup <- cbind(jpn_teams,jpn_away_as)

jpn_awayas_lookup <- as.data.frame(jpn_awayas_lookup)

colnames(jpn_awayas_lookup) <- c("AwayTeam_jpn","jpn_awayas")


JPN_fixtures$jpn_awayas <- sqldf("SELECT jpn_awayas_lookup.jpn_awayas FROM jpn_awayas_lookup INNER JOIN JPN_fixtures ON jpn_awayas_lookup.AwayTeam_jpn = JPN_fixtures.AwayTeam_jpn")

JPN_fixtures$jpn_homeds <- rep(jpn_home_ds,each = length(jpn_teams)-1)

JPN_fixtures$jpn_awayds <- as.numeric(unlist(JPN_fixtures$jpn_awayds))
#xGH
JPN_fixtures$jpn_xGH <- JPN_fixtures$avg_HG_jpn * JPN_fixtures$jpn_homeas * JPN_fixtures$jpn_awayds

#xGA

JPN_fixtures$jpn_awayas <- as.numeric(unlist(JPN_fixtures$jpn_awayas))

JPN_fixtures$jpn_xGA <- JPN_fixtures$avg_AG_jpn * JPN_fixtures$jpn_awayas * JPN_fixtures$jpn_homeds

JPN_fixtures$jpn_0_0 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_0 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_1 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_1 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_0 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_2 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_2 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_1 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_2 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_3 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_0 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_1 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_2 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_3 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_3 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_3 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_4 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_0 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_1 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_2 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_3 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_4 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_4 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_4 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_4 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_5 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_0 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_1 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_2 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_3 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_4 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_5 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_5 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_5 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_5 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_5 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_6 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_0 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(0,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_1 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(1,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_2 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(2,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_3 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(3,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_4 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(4,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_6_5 <- round(stats::dpois(6,JPN_fixtures$jpn_xGH) * stats::dpois(5,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_0_6 <- round(stats::dpois(0,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_1_6 <- round(stats::dpois(1,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_2_6 <- round(stats::dpois(2,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_3_6 <- round(stats::dpois(3,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_4_6 <- round(stats::dpois(4,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
JPN_fixtures$jpn_5_6 <- round(stats::dpois(5,JPN_fixtures$jpn_xGH) * stats::dpois(6,JPN_fixtures$jpn_xGA), digits = 4)
#Home win
JPN_fixtures$jpn_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 + JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5
)

JPN_fixtures$jpn_H <- percent(JPN_fixtures$jpn_H, accuracy = 0.1)

#Draw
JPN_fixtures$jpn_D <- (

  JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 + JPN_fixtures$jpn_4_4 +
    JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6
)

JPN_fixtures$jpn_D <- percent(JPN_fixtures$jpn_D, accuracy = 0.1)

#Away

JPN_fixtures$jpn_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 + JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6
)

JPN_fixtures$jpn_A <- percent(JPN_fixtures$jpn_A, accuracy = 0.1)

#ov25
JPN_fixtures$jpn_ov25 <- (
  JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 + JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 + JPN_fixtures$jpn_0_4 +
    JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 + JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_0 +
    JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 + JPN_fixtures$jpn_0_5 +
    JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 + JPN_fixtures$jpn_5_5 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5 + JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 +
    JPN_fixtures$jpn_4_6 + JPN_fixtures$jpn_5_6 + JPN_fixtures$jpn_6_6
)
#un25
JPN_fixtures$jpn_un25 <- (
  JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_0_2
)
#odds
JPN_fixtures$jpn_ov25_odds <- round((1/JPN_fixtures$jpn_ov25),digits = 2)
JPN_fixtures$jpn_un25_odds <- round((1/JPN_fixtures$jpn_un25),digits = 2)

JPN_fixtures$jpn_ov25_odds
JPN_fixtures$jpn_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
JPN_fixtures$jpn_BTTSY <- (
  JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_3_1 + JPN_fixtures$jpn_3_2 +
    JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_1_3 + JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_3_3 + JPN_fixtures$jpn_4_4 +
    JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_3 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 +
    JPN_fixtures$jpn_3_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 +
    JPN_fixtures$jpn_5_4 + JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_6_6 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6
)
#BTTSN
JPN_fixtures$jpn_BTTSN <- (
  JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_0_2 +
    JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_5_0 +
    JPN_fixtures$jpn_0_5 + JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_0_6
)

JPN_fixtures$jpn_BTTSY_odds <- round((1/JPN_fixtures$jpn_BTTSY),digits = 2)
JPN_fixtures$jpn_BTTSN_odds <- round((1/JPN_fixtures$jpn_BTTSN),digits = 2)

JPN_fixtures$jpn_BTTSY <- percent(JPN_fixtures$jpn_BTTSY, accuracy = 0.1)
JPN_fixtures$jpn_BTTSN <- percent(JPN_fixtures$jpn_BTTSN, accuracy = 0.1)
#odds
JPN_fixtures$jpn_BTTSY_odds
JPN_fixtures$jpn_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
JPN_fixtures$jpn_AH_0_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 +JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5 + JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6
)
#AH_0_A
JPN_fixtures$jpn_AH_0_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 +JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6 + JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6
)

#odds
JPN_fixtures$jpn_AH_0_H_odds <- round((1/JPN_fixtures$jpn_AH_0_H),digits = 2)
JPN_fixtures$jpn_AH_0_A_odds <- round((1/JPN_fixtures$jpn_AH_0_A),digits = 2)

JPN_fixtures$jpn_AH_0_H_odds
JPN_fixtures$jpn_AH_0_A_odds
#percentages
JPN_fixtures$jpn_AH_0_H <- percent(JPN_fixtures$jpn_AH_0_H, accuracy = 0.1)
JPN_fixtures$jpn_AH_0_A <- percent(JPN_fixtures$jpn_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
JPN_fixtures$jpn_AH_n075_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 +JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5
)
#AH_n075_A
JPN_fixtures$jpn_AH_n075_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 +JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6
)

#odds
JPN_fixtures$jpn_AH_n075_H_odds <- round((1/JPN_fixtures$jpn_AH_n075_H),digits = 2)
JPN_fixtures$jpn_AH_n075_A_odds <- round((1/JPN_fixtures$jpn_AH_n075_A),digits = 2)

JPN_fixtures$jpn_AH_n075_H_odds
JPN_fixtures$jpn_AH_n075_A_odds
#percentages
JPN_fixtures$jpn_AH_n075_H <- percent(JPN_fixtures$jpn_AH_n075_H, accuracy = 0.1)
JPN_fixtures$jpn_AH_n075_A <- percent(JPN_fixtures$jpn_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
JPN_fixtures$jpn_AH_075_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 +JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5 + JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6 + JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_1_2 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_3_4 + JPN_fixtures$jpn_4_5 + JPN_fixtures$jpn_5_6
)
#AH_075_A
JPN_fixtures$jpn_AH_075_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 +JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6 + JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6 + JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_3 + JPN_fixtures$jpn_5_4 + JPN_fixtures$jpn_6_5
)

#odds
JPN_fixtures$jpn_AH_075_H_odds <- round((1/JPN_fixtures$jpn_AH_075_H),digits = 2)
JPN_fixtures$jpn_AH_075_A_odds <- round((1/JPN_fixtures$jpn_AH_075_A),digits = 2)

JPN_fixtures$jpn_AH_075_H_odds
JPN_fixtures$jpn_AH_075_A_odds
#percentages
JPN_fixtures$jpn_AH_075_H <- percent(JPN_fixtures$jpn_AH_075_H, accuracy = 0.1)
JPN_fixtures$jpn_AH_075_A <- percent(JPN_fixtures$jpn_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
JPN_fixtures$jpn_AH_n125_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 +JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5
)
#AH_n125_A
JPN_fixtures$jpn_AH_n125_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 +JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6
)

#odds
JPN_fixtures$jpn_AH_n125_H_odds <- round((1/JPN_fixtures$jpn_AH_n125_H),digits = 2)
JPN_fixtures$jpn_AH_n125_A_odds <- round((1/JPN_fixtures$jpn_AH_n125_A),digits = 2)

JPN_fixtures$jpn_AH_n125_H_odds
JPN_fixtures$jpn_AH_n125_A_odds
#percentages
JPN_fixtures$jpn_AH_n125_H <- percent(JPN_fixtures$jpn_AH_n125_H, accuracy = 0.1)
JPN_fixtures$jpn_AH_n125_A <- percent(JPN_fixtures$jpn_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
JPN_fixtures$jpn_AH_125_H <- (
  JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_0 + JPN_fixtures$jpn_2_1 + JPN_fixtures$jpn_3_0 + JPN_fixtures$jpn_3_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_0 + JPN_fixtures$jpn_4_1 + JPN_fixtures$jpn_4_2 + JPN_fixtures$jpn_4_3 +
    JPN_fixtures$jpn_5_0 +JPN_fixtures$jpn_5_1 + JPN_fixtures$jpn_5_2 + JPN_fixtures$jpn_5_3 + JPN_fixtures$jpn_5_4 +
    JPN_fixtures$jpn_6_0 + JPN_fixtures$jpn_6_1 + JPN_fixtures$jpn_6_2 + JPN_fixtures$jpn_6_3 + JPN_fixtures$jpn_6_4 +
    JPN_fixtures$jpn_6_5 + JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6 + JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_1_2 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_3_4 + JPN_fixtures$jpn_4_5 + JPN_fixtures$jpn_5_6
)
#AH_125_A
JPN_fixtures$jpn_AH_125_A <- (
  JPN_fixtures$jpn_0_1 + JPN_fixtures$jpn_0_2 + JPN_fixtures$jpn_1_2 + JPN_fixtures$jpn_0_3 + JPN_fixtures$jpn_1_3 +
    JPN_fixtures$jpn_2_3 + JPN_fixtures$jpn_0_4 + JPN_fixtures$jpn_1_4 + JPN_fixtures$jpn_2_4 + JPN_fixtures$jpn_3_4 +
    JPN_fixtures$jpn_0_5 +JPN_fixtures$jpn_1_5 + JPN_fixtures$jpn_2_5 + JPN_fixtures$jpn_3_5 + JPN_fixtures$jpn_4_5 +
    JPN_fixtures$jpn_0_6 + JPN_fixtures$jpn_1_6 + JPN_fixtures$jpn_2_6 + JPN_fixtures$jpn_3_6 + JPN_fixtures$jpn_4_6 +
    JPN_fixtures$jpn_5_6 + JPN_fixtures$jpn_0_0 + JPN_fixtures$jpn_1_1 + JPN_fixtures$jpn_2_2 + JPN_fixtures$jpn_3_3 +
    JPN_fixtures$jpn_4_4 + JPN_fixtures$jpn_5_5 + JPN_fixtures$jpn_6_6 + JPN_fixtures$jpn_1_0 + JPN_fixtures$jpn_2_1 +
    JPN_fixtures$jpn_3_2 + JPN_fixtures$jpn_4_3 + JPN_fixtures$jpn_5_4 + JPN_fixtures$jpn_6_5
)

#odds
JPN_fixtures$jpn_AH_125_H_odds <- round((1/JPN_fixtures$jpn_AH_125_H),digits = 2)
JPN_fixtures$jpn_AH_125_A_odds <- round((1/JPN_fixtures$jpn_AH_125_A),digits = 2)

JPN_fixtures$jpn_AH_125_H_odds
JPN_fixtures$jpn_AH_125_A_odds
#percentages
JPN_fixtures$jpn_AH_125_H <- percent(JPN_fixtures$jpn_AH_125_H, accuracy = 0.1)
JPN_fixtures$jpn_AH_125_A <- percent(JPN_fixtures$jpn_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
JPN_fixtures$jpn_ov25 <- percent(JPN_fixtures$jpn_ov25, accuracy = 0.1)

JPN_fixtures$jpn_un25 <- percent(JPN_fixtures$jpn_un25, accuracy = 0.1)
JPN_fixtures$jpn_pscore <- paste(round(JPN_fixtures$jpn_xGH,digits = 0),round(JPN_fixtures$jpn_xGA,digits = 0),sep = "-")
#write out
write.xlsx(JPN_fixtures,'NL/JPN.xlsx',sheetName = "JPN", append = TRUE)
###########################################################################################################
########################JPN END###########################################################################
JPN <- read.csv('../FDAS/JPN.csv')
JPN$TG <- JPN$HG + JPN$AG
JPN$OV25 <- ifelse(JPN$TG >= 3,"Y","N")
jpn_ftr_summary <- tabyl(JPN,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
jpn_ov25_summary <- tabyl(JPN,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(jpn_ftr_summary,'NL/JPN.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(jpn_ov25_summary,'NL/JPN.xlsx',sheetName = "OVUN25", append = TRUE)



