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
unlink('NL/SWE.xlsx')
######################SWE START#######################################
#####################################################################
SWE <- read.csv('../FDAS/SWE.csv')
SWE <- within(SWE,rm(Res))
SWE$Date <- dmy(SWE$Date)
SWE <- SWE[order(as.Date(SWE$Date, format = "%d/%m%Y"), decreasing = FALSE),]
SWE$CS <- paste(SWE$HG,SWE$AG, sep = "-")
#SWE_qualificaton <- subset(SWE,tournament == "UEFA Euro qualification")
SWE <- subset(SWE,Season == "2021")
#SWE <- SWE[SWE$Date > '2008-01-01',])
SWE$TG <- SWE$HG + SWE$AG
SWE$OV25 <- ifelse(SWE$TG >= 3,"Y","N")
SWE$FTR <- with(SWE,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
#######################################################################################
####GoalTotalsv2#######################################################################
swe_totalgoalsv2 <- tapply(SWE$TG, SWE[c("Home", "Away")],mean)
swe_totalgoalsv2
swe_hgtotals <- rowSums(swe_totalgoalsv2,na.rm = T)
swe_agtotals <- colSums(swe_totalgoalsv2,na.rm = T)

swe_totalgoals <- swe_hgtotals + swe_agtotals
swe_totalgoalsv2 <- cbind(swe_totalgoalsv2,swe_totalgoals)
swe_teams <- sort(unique(SWE$Home))
swe_home_games <- c()
swe_away_games <-c()
for (i_swe in 1:length(swe_teams))
{

  swe_home_games[i_swe] <- nrow(SWE[SWE$Home == swe_teams[i_swe],])
  swe_away_games[i_swe]  <- nrow(SWE[SWE$Away == swe_teams[i_swe],])

}
swe_games_played <- swe_home_games + swe_away_games
swe_goaltotalsv2 <- cbind(swe_totalgoalsv2,swe_games_played)
swe_avg_totalgoals <- round((swe_totalgoals/ swe_games_played), digits = 4)
swe_goaltotalsv2[is.na(swe_goaltotalsv2)] <- ""
swe_goaltotalsv2 <- cbind(swe_goaltotalsv2,swe_avg_totalgoals)
write.xlsx(swe_goaltotalsv2,'NL/SWE.xlsx',sheetName = "totalgoalsv2")
############################################################################################
SWE <- subset(SWE,Season == "2021")
swe_totalrounds <-  (length(swe_teams) - 1 )*2
swe_totalmatches <- (length(swe_teams)*(length(swe_teams) - 1))
swe_eachround <- ceiling(swe_totalmatches / swe_totalrounds)

swe_matchesplayed <-  nrow(SWE)

SWE_rounds <- SWE

if(swe_matchesplayed %% swe_eachround == 0)
{
  swe_currentround <- swe_matchesplayed / swe_eachround
  swe_matchday <- c()
  swe_matchday <- rep(1:swe_currentround, each = swe_eachround)
}else if(swe_matchesplayed %% swe_eachround != 0)

{

  swe_modulus <- swe_matchesplayed %% swe_eachround
  swe_currentround <- (swe_matchesplayed - swe_modulus) / swe_eachround
  swe_matchday <- c()
  swe_matchday_vec1 <- c()
  swe_matchday_vec2 <- c()
  swe_matchday_vec1 <- rep(1:swe_currentround, each = swe_eachround)
  swe_matchday_vec2[1:swe_modulus] <- c(swe_currentround + 1)
  swe_matchday <- append(swe_matchday_vec1,swe_matchday_vec2)
}
SWE_rounds <- cbind(SWE_rounds,swe_matchday)
# #####################################################################################################
############################################
####matrix################################
swe_goalscored_h <- tapply(SWE$HG, SWE[c("Home", "Date")],mean)
swe_goalscored_a <- tapply(SWE$AG, SWE[c("Away", "Date")],mean)
swe_goalscored_h[is.na(swe_goalscored_h)] <- ""
swe_goalscored_a[is.na(swe_goalscored_a)] <- ""

for(swe_rowhgs in 1:nrow(swe_goalscored_h)) {
  for(swe_colhgs in 1:ncol(swe_goalscored_h)) {

    # print(my_matrix[row, col])
    for(swe_rowags in 1:nrow(swe_goalscored_a)) {
      for(swe_colags in 1:ncol(swe_goalscored_a)) {
        ifelse(!swe_goalscored_a[swe_rowags,swe_colags]=="",swe_goalscored_h[swe_rowags,swe_colags] <- swe_goalscored_a[swe_rowags,swe_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(swe_goalscoredmatrix,'NL/SWE.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#########################################################################################
#swe goal scored rounds
final_swe_gs <- matrix(nrow = length(swe_teams),ncol = swe_totalrounds )
suml6_swe_gs <- c()
sum_swe_zero_gs <- c()
sum_swe_one_gs <- c()
sum_swe_two_gs <- c()
sum_swe_three_gs <- c()
l6_form_swe_gssplitted <- c()
form_swe_gs <- c()
for(index_swe_gs in 1:length(swe_teams))
{
  for(index_swe_gs_cols in 1:swe_totalrounds)
  {
    index_swe_gs  <- row.names(swe_goalscored_h) == swe_teams[index_swe_gs]
    form_swe_gs <- swe_goalscored_h[index_swe_gs ]
    deleted_form_swe_gs <- form_swe_gs[!form_swe_gs[] == ""]
    l6_form_swe_gs <- tail(deleted_form_swe_gs,swe_last_n_games)
    l6_form_swe_gs <- as.numeric(l6_form_swe_gs)
    suml6_swe_gs[index_swe_gs] <- sum(l6_form_swe_gs)
    suml6_swe_gs[index_swe_gs] <- paste(suml6_swe_gs[index_swe_gs],sep = "")
    sum_swe_zero_gs[index_swe_gs] <- length(which(l6_form_swe_gs == 0))
    sum_swe_zero_gs[index_swe_gs] <- paste(sum_swe_zero_gs[index_swe_gs],sep = "")
    sum_swe_one_gs[index_swe_gs] <- length(which(l6_form_swe_gs == 1))
    sum_swe_one_gs[index_swe_gs] <- paste(sum_swe_one_gs[index_swe_gs],sep = "")
    sum_swe_two_gs[index_swe_gs] <- length(which(l6_form_swe_gs >= 2))
    sum_swe_two_gs[index_swe_gs] <- paste(sum_swe_two_gs[index_swe_gs],sep = "")
    sum_swe_three_gs[index_swe_gs] <- length(which(l6_form_swe_gs >= 3))
    sum_swe_three_gs[index_swe_gs] <- paste(sum_swe_three_gs[index_swe_gs],sep = "")
    l6_form_swe_gs <- as.character(l6_form_swe_gs)
    l6_form_swe_gs_flattened <- stri_paste(l6_form_swe_gs,collapse = '')
    l6_form_swe_gssplitted <- as.numeric(strsplit(as.character(l6_form_swe_gs_flattened),"")[[1]])
    final_swe_gs[index_swe_gs,index_swe_gs_cols] <- l6_form_swe_gssplitted[index_swe_gs_cols]
  }
}

final_swe_gs[is.na(final_swe_gs)] <- ""
swe_goalscoredmatrix <- cbind(swe_teams,final_swe_gs,suml6_swe_gs,sum_swe_zero_gs,sum_swe_one_gs,sum_swe_two_gs,sum_swe_three_gs)
write.xlsx(swe_goalscoredmatrix,'NL/SWE.xlsx',sheetName = "gsmatrix", append = TRUE)
#################################################################################################################################

####GCmatrix#####################################################################################################################
#create home and away matrices
swe_goalconceded_h <- tapply(SWE$AG, SWE[c("Home", "Date")],mean)
swe_goalconceded_a <- tapply(SWE$HG, SWE[c("Away", "Date")],mean)
swe_goalconceded_h[is.na(swe_goalconceded_h)] <- ""
swe_goalconceded_a[is.na(swe_goalconceded_a)] <- ""

for(swe_rowhgc in 1:nrow(swe_goalconceded_h)) {
  for(swe_colhgc in 1:ncol(swe_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(swe_rowagc in 1:nrow(swe_goalconceded_a)) {
      for(swe_colagc in 1:ncol(swe_goalconceded_a)) {
        ifelse(!swe_goalconceded_a[swe_rowagc,swe_colagc]=="",swe_goalconceded_h[swe_rowagc,swe_colagc] <- swe_goalconceded_a[swe_rowagc,swe_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(swe_goalconcededmatrix,'NL/SWE.xlsx',sheetName = "gcmatrix", append = TRUE)
############################################################################################################################################################
#swe goal conceded rounds
final_swe_gc <- matrix(nrow = length(swe_teams),ncol = swe_totalrounds )
suml6_swe_gc <- c()
sum_swe_zero_gc <- c()
sum_swe_one_gc <- c()
sum_swe_two_gc <- c()
sum_swe_three_gc <- c()
l6_form_swe_gcsplitted <- c()
form_swe_gc <- c()
for(index_swe_gc in 1:length(swe_teams))
{
  for(index_swe_gc_cols in 1:swe_totalrounds)
  {
    index_swe_gc  <- row.names(swe_goalconceded_h) == swe_teams[index_swe_gc]
    form_swe_gc <- swe_goalconceded_h[index_swe_gc ]
    deleted_form_swe_gc <- form_swe_gc[!form_swe_gc[] == ""]
    l6_form_swe_gc <- tail(deleted_form_swe_gc,swe_last_n_games)
    l6_form_swe_gc <- as.numeric(l6_form_swe_gc)
    suml6_swe_gc[index_swe_gc] <- sum(l6_form_swe_gc)
    suml6_swe_gc[index_swe_gc] <- paste(suml6_swe_gc[index_swe_gc],sep = "")
    sum_swe_zero_gc[index_swe_gc] <- length(which(l6_form_swe_gc == 0))
    sum_swe_zero_gc[index_swe_gc] <- paste(sum_swe_zero_gc[index_swe_gc],sep = "")
    sum_swe_one_gc[index_swe_gc] <- length(which(l6_form_swe_gc == 1))
    sum_swe_one_gc[index_swe_gc] <- paste(sum_swe_one_gc[index_swe_gc],sep = "")
    sum_swe_two_gc[index_swe_gc] <- length(which(l6_form_swe_gc >= 2))
    sum_swe_two_gc[index_swe_gc] <- paste(sum_swe_two_gc[index_swe_gc],sep = "")
    sum_swe_three_gc[index_swe_gc] <- length(which(l6_form_swe_gc >= 3))
    sum_swe_three_gc[index_swe_gc] <- paste(sum_swe_three_gc[index_swe_gc],sep = "")
    l6_form_swe_gc <- as.character(l6_form_swe_gc)
    l6_form_swe_gc_flattened <- stri_paste(l6_form_swe_gc,collapse = '')
    l6_form_swe_gcsplitted <- as.numeric(strsplit(as.character(l6_form_swe_gc_flattened),"")[[1]])
    final_swe_gc[index_swe_gc,index_swe_gc_cols] <- l6_form_swe_gcsplitted[index_swe_gc_cols]
  }
}

final_swe_gc[is.na(final_swe_gc)] <- ""
swe_goalconcededmatrix <- cbind(swe_teams,final_swe_gc,suml6_swe_gc,sum_swe_zero_gc,sum_swe_one_gc,sum_swe_two_gc,sum_swe_three_gc)
write.xlsx(swe_goalconcededmatrix,'NL/SWE.xlsx',sheetName = "gcmatrix2", append = TRUE)
###################################################################################################################################

###################################################################################################################################
####Teamform#######################################################################################################################

swe_form_h <- tapply(SWE$FTR, SWE[c("Home", "Date")],median)
swe_form_a <- tapply(SWE$FTR, SWE[c("Away", "Date")],median)
swe_form_h[is.na(swe_form_h)] <- ""
swe_form_a[is.na(swe_form_a)] <- ""
swe_form_h <- sub("A","L",swe_form_h)
swe_form_h <- sub("H","W",swe_form_h)
swe_form_a <- sub("A","W",swe_form_a)
swe_form_a <- sub("H","L",swe_form_a)
for(swe_rowh_f in 1:nrow(swe_form_h)) {
  for(swe_colh_f in 1:ncol(swe_form_h)) {

    # print(my_matrix[row, col])
    for(swe_rowa_f in 1:nrow(swe_form_a)) {
      for(swe_cola_f in 1:ncol(swe_form_a)) {
        ifelse(!swe_form_a[swe_rowa_f,swe_cola_f]=="",swe_form_h[swe_rowa_f,swe_cola_f] <- swe_form_a[swe_rowa_f,swe_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#swe team form
final_swe_hf <- matrix(nrow = length(swe_teams),ncol = swe_totalrounds )
suml6_swe_hf <- c()
l6_form_swe_hfsplitted <- c()
form_swe_hf <- c()
for(index_swe_hf in 1:length(swe_teams))
{
  for(index_swe_hf_cols in 1:swe_totalrounds)
  {
    index_swe_hf  <- row.names(swe_form_h) == swe_teams[index_swe_hf]
    form_swe_hf <- swe_form_h[index_swe_hf ]
    deleted_form_swe_hf <- form_swe_hf[!form_swe_hf[] == ""]
    l6_form_swe_hf <- tail(deleted_form_swe_hf,swe_last_n_games)
    # #l6_form_swe_hf <- as.numeric(l6_form_swe_hf)
    # suml6_swe_hf[index_swe_hf] <- sum(l6_form_swe_hf)
    # suml6_swe_hf[index_swe_hf] <- paste(suml6_swe_hf[index_swe_hf],sep = "")
    #l6_form_swe_hf <- as.character(l6_form_swe_hf)
    l6_form_swe_hf_flattened <- stri_paste(l6_form_swe_hf,collapse = '')
    l6_form_swe_hfsplitted <- (strsplit(as.character(l6_form_swe_hf_flattened),"")[[1]])
    final_swe_hf[index_swe_hf,index_swe_hf_cols] <- l6_form_swe_hfsplitted[index_swe_hf_cols]
  }
}
final_swe_hf[is.na(final_swe_hf)] <- ""
swe_formmatrix <- cbind(swe_teams,final_swe_hf)

write.xlsx(swe_formmatrix,'NL/SWE.xlsx',sheetName = "form", append = TRUE)
######################################################################################################################################
######################################################################################################################################

#######TGMatrix#######################################################################################################################
swe_totalgoals_h <- tapply(SWE$TG, SWE[c("Home", "Date")],mean)
swe_totalgoals_a <- tapply(SWE$TG, SWE[c("Away", "Date")],mean)
swe_totalgoals_h[is.na(swe_totalgoals_h)] <- ""
swe_totalgoals_a[is.na(swe_totalgoals_a)] <- ""
for(swe_rowh in 1:nrow(swe_totalgoals_h)) {
  for(swe_colh in 1:ncol(swe_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(swe_rowa in 1:nrow(swe_totalgoals_a)) {
      for(swe_cola in 1:ncol(swe_totalgoals_a)) {
        ifelse(!swe_totalgoals_a[swe_rowa,swe_cola]=="",swe_totalgoals_h[swe_rowa,swe_cola] <- swe_totalgoals_a[swe_rowa,swe_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#swe total goals rounds
#swe
final_swe_tg <- matrix(nrow = length(swe_teams),ncol = swe_totalrounds )
suml6_swe_tg <- c()
l6_form_swe_tgsplitted <- c()
form_swe_tg <- c()
for(index_swe_tg in 1:length(swe_teams))
{
  for(index_swe_tg_cols in 1:swe_totalrounds)
  {
    index_swe_tg  <- row.names(swe_totalgoals_h) == swe_teams[index_swe_tg]
    form_swe_tg <- swe_totalgoals_h[index_swe_tg ]
    deleted_form_swe_tg <- form_swe_tg[!form_swe_tg[] == ""]
    l6_form_swe_tg <- tail(deleted_form_swe_tg,swe_last_n_games)
    l6_form_swe_tg <- as.numeric(l6_form_swe_tg)
    suml6_swe_tg[index_swe_tg] <- sum(l6_form_swe_tg)
    suml6_swe_tg[index_swe_tg] <- paste(suml6_swe_tg[index_swe_tg],sep = "")
    l6_form_swe_tg <- as.character(l6_form_swe_tg)
    l6_form_swe_tg_flattened <- stri_paste(l6_form_swe_tg,collapse = '')
    l6_form_swe_tgsplitted <- as.numeric(strsplit(as.character(l6_form_swe_tg_flattened),"")[[1]])
    final_swe_tg[index_swe_tg,index_swe_tg_cols] <- l6_form_swe_tgsplitted[index_swe_tg_cols]
  }
}

final_swe_tg[is.na(final_swe_tg)] <- ""
swe_goaltotalmatrix <- cbind(swe_teams,final_swe_tg,suml6_swe_tg)

write.xlsx(swe_goaltotalmatrix,'NL/SWE.xlsx',sheetName = "tgmatrix", append = TRUE)
#############################################################################################################################################
#######TeamAgainst###########################################################################################################################
swe_form_team_against_h <- tapply(SWE$Away, SWE[c("Home", "Date")],median)
swe_form_team_against_a <- tapply(SWE$Home, SWE[c("Away", "Date")],median)
swe_form_team_against_h[is.na(swe_form_team_against_h)] <- ""
swe_form_team_against_a[is.na(swe_form_team_against_a)] <- ""
for(swe_rowh_f_against in 1:nrow(swe_form_team_against_h)) {
  for(swe_colh_f_against in 1:ncol(swe_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(swe_rowa_f_against in 1:nrow(swe_form_team_against_a)) {
      for(swe_cola_f_against in 1:ncol(swe_form_team_against_a)) {
        ifelse(!swe_form_team_against_a[swe_rowa_f_against,swe_cola_f_against]=="",swe_form_team_against_h[swe_rowa_f_against,swe_cola_f_against] <- swe_form_team_against_a[swe_rowa_f_against,swe_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#win margin
swe_winmargin_h <- tapply(SWE$HG - SWE$AG, SWE[c("Home", "Date")],mean)
swe_winmargin_a <- tapply(SWE$AG - SWE$HG, SWE[c("Away", "Date")],mean)
swe_winmargin_h[is.na(swe_winmargin_h)] <- ""
#
for(swe_rowhwm in 1:nrow(swe_winmargin_h)) {
  for(swe_colhwm in 1:ncol(swe_winmargin_h)) {

    # print(my_matrix[row, col])
    for(swe_rowawm in 1:nrow(swe_winmargin_a)) {
      for(swe_colawm in 1:ncol(swe_winmargin_a)) {
        ifelse(!swe_winmargin_a[swe_rowawm,swe_colawm]=="",swe_winmargin_h[swe_rowawm,swe_colawm] <- swe_winmargin_a[swe_rowawm,swe_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
final_swe_wm <- matrix(nrow = length(swe_teams),ncol = swe_totalrounds )
suml6_swe_wm <- c()
suml6_swe_wm_negone <- c()
suml6_swe_wm_negtwo <- c()
suml6_swe_wm_zero <- c()
suml6_swe_wm_posone <- c()
suml6_swe_wm_postwo <- c()
l6_form_swe_wmsplitted <- c()
form_swe_wm <- c()
for(index_swe_wm in 1:length(swe_teams))
{
  for(index_swe_wm_cols in 1:swe_totalrounds)
  {
    index_swe_wm  <- row.names(swe_winmargin_h) == swe_teams[index_swe_wm]
    form_swe_wm <- swe_winmargin_h[index_swe_wm ]
    deleted_form_swe_wm <- form_swe_wm[!form_swe_wm[] == ""]
    l6_form_swe_wm <- tail(deleted_form_swe_wm,swe_last_n_games)
    l6_form_swe_wm <- as.numeric(l6_form_swe_wm)
    suml6_swe_wm[index_swe_wm] <- sum(l6_form_swe_wm)
    suml6_swe_wm[index_swe_wm] <- paste(suml6_swe_wm[index_swe_wm],sep = "")
    suml6_swe_wm_negone[index_swe_wm] <- length(which(l6_form_swe_wm == -1))
    suml6_swe_wm_negone[index_swe_wm] <- paste(suml6_swe_wm_negone[index_swe_wm],sep = "")
    suml6_swe_wm_negtwo[index_swe_wm] <- length(which(l6_form_swe_wm <= -2))
    suml6_swe_wm_negtwo[index_swe_wm] <- paste(suml6_swe_wm_negtwo[index_swe_wm],sep = "")
    suml6_swe_wm_zero[index_swe_wm] <- length(which(l6_form_swe_wm == 0))
    suml6_swe_wm_zero[index_swe_wm] <- paste(suml6_swe_wm_zero[index_swe_wm],sep = "")
    suml6_swe_wm_posone[index_swe_wm] <- length(which(l6_form_swe_wm == 1))
    suml6_swe_wm_posone[index_swe_wm] <- paste(suml6_swe_wm_posone[index_swe_wm],sep = "")
    suml6_swe_wm_postwo[index_swe_wm] <- length(which(l6_form_swe_wm == 2))
    suml6_swe_wm_postwo[index_swe_wm] <- paste(suml6_swe_wm_postwo[index_swe_wm],sep = "")
    l6_form_swe_wm <- as.character(l6_form_swe_wm)
    l6_form_swe_wm_flattened <- stri_paste(l6_form_swe_wm,collapse = ',')
    l6_form_swe_wmsplitted <- (strsplit(as.character(l6_form_swe_wm_flattened),",")[[1]])
    final_swe_wm[index_swe_wm,index_swe_wm_cols] <- l6_form_swe_wmsplitted[index_swe_wm_cols]
  }
}

final_swe_wm[is.na(final_swe_wm)] <- ""
swe_winmarginmatrix <- cbind(swe_teams,final_swe_wm,suml6_swe_wm,suml6_swe_wm_negtwo,suml6_swe_wm_negone,suml6_swe_wm_zero,suml6_swe_wm_posone,suml6_swe_wm_postwo)
write.xlsx(swe_winmarginmatrix,'NL/SWE.xlsx',sheetName = "winmargin", append = TRUE)
####################################################################################################################
##########Goals over under############
#SWE
swe_un05_home <- c()
swe_un05_away <- c()
swe_ov05_home <- c()
swe_ov05_away <- c()

swe_un15_home <- c()
swe_un15_away <- c()
swe_ov15_home <- c()
swe_ov15_away <- c()

swe_un25_home <- c()
swe_un25_away <- c()
swe_ov25_home <- c()
swe_ov25_away <- c()

swe_un35_home <- c()
swe_un35_away <- c()
swe_ov35_home <- c()
swe_ov35_away <- c()

swe_un45_home <- c()
swe_un45_away <- c()
swe_ov45_home <- c()
swe_ov45_away <- c()

swe_un55_home <- c()
swe_un55_away <- c()
swe_ov55_home <- c()
swe_ov55_away <- c()

for (i_swe_tg in 1:length(swe_teams))
{

  swe_un05_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG == 0,])
  swe_un05_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG == 0,])

  swe_ov05_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG > 0,])
  swe_ov05_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG > 0,])

  swe_un15_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG <= 1,])
  swe_un15_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG <= 1,])

  swe_ov15_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG >= 2,])
  swe_ov15_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG >= 2,])

  swe_un25_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG <= 2,])
  swe_un25_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG <= 2,])

  swe_ov25_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG >=3,])
  swe_ov25_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG >=3,])

  swe_un35_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG <= 3,])
  swe_un35_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG <= 3,])

  swe_ov35_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG >= 4,])
  swe_ov35_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG >= 4,])

  swe_un45_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG <= 4,])
  swe_un45_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG <= 4,])

  swe_ov45_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG >= 5,])
  swe_ov45_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG >= 5,])

  swe_un55_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG <= 5,])
  swe_un55_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG <= 5,])

  swe_ov55_home[i_swe_tg] <- nrow(SWE[SWE$Home == swe_teams[i_swe_tg] & SWE$TG >= 6,])
  swe_ov55_away[i_swe_tg] <- nrow(SWE[SWE$Away == swe_teams[i_swe_tg] & SWE$TG >= 6,])


}

swe_un05 <- swe_un05_home + swe_un05_away
swe_ov05 <- swe_ov05_home + swe_ov05_away

swe_un15 <- swe_un15_home + swe_un15_away
swe_ov15 <- swe_ov15_home + swe_ov15_away

swe_un25 <- swe_un25_home + swe_un25_away
swe_ov25 <- swe_ov25_home + swe_ov25_away

swe_un35 <- swe_un35_home + swe_un35_away
swe_ov35 <- swe_ov35_home + swe_ov35_away

swe_un45 <- swe_un45_home + swe_un45_away
swe_ov45 <- swe_ov45_home + swe_ov45_away

swe_un55 <- swe_un55_home + swe_un55_away
swe_ov55 <- swe_ov55_home + swe_ov55_away

swe_ovundata <- cbind(swe_teams,swe_un05,swe_ov05,swe_un15,swe_ov15,swe_un25,swe_ov25,swe_un35,swe_ov35,swe_un45,swe_ov45,swe_un55,swe_ov55)
write.xlsx(swe_ovundata,'NL/SWE.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
swe_csform_h <- tapply(SWE$CS, SWE[c("Home", "Date")],median)
swe_csform_a <- tapply(SWE$CS, SWE[c("Away", "Date")],median)

swe_csform_h[is.na(swe_csform_h)] <- ""
swe_csform_a[is.na(swe_csform_a)] <- ""

for(swe_rowh_f_cs in 1:nrow(swe_csform_h)) {
  for(swe_colh_f_cs in 1:ncol(swe_csform_h)) {

    # print(my_matrix[row, col])
    for(swe_rowa_f_cs in 1:nrow(swe_csform_a)) {
      for(swe_cola_f_cs in 1:ncol(swe_csform_a)) {
        ifelse(!swe_csform_a[swe_rowa_f_cs,swe_cola_f_cs]=="",swe_csform_h[swe_rowa_f_cs,swe_cola_f_cs] <- swe_csform_a[swe_rowa_f_cs,swe_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
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


######################################################################################
###########League Table###############################################################

#hwins and away wins
swe_home_wins <- c()
swe_away_wins <- c()
swe_home_draws <- c()
swe_away_draws <- c()
swe_home_loss <- c()
swe_away_loss <- c()



for (i_swe_wins in 1:length(swe_teams))
{

  swe_home_wins[i_swe_wins] <- nrow(SWE[SWE$Home == swe_teams[i_swe_wins] & SWE$FTR == "H",])
  swe_away_wins[i_swe_wins] <- nrow(SWE[SWE$Away == swe_teams[i_swe_wins] & SWE$FTR == "A",])
  swe_home_draws[i_swe_wins] <- nrow(SWE[SWE$Home == swe_teams[i_swe_wins] & SWE$FTR == "D",])
  swe_away_draws[i_swe_wins] <- nrow(SWE[SWE$Away == swe_teams[i_swe_wins] & SWE$FTR == "D",])
  swe_home_loss[i_swe_wins] <- nrow(SWE[SWE$Home == swe_teams[i_swe_wins] & SWE$FTR == "A",])
  swe_away_loss[i_swe_wins] <- nrow(SWE[SWE$Away == swe_teams[i_swe_wins] & SWE$FTR == "H",])

}

swe_total_wins <- swe_home_wins + swe_away_wins
swe_total_draws <- swe_home_draws + swe_away_draws
swe_total_loss <- swe_home_loss + swe_away_loss

swe_league_table <- cbind(swe_teams,swe_games_played,swe_total_wins,swe_total_draws,swe_total_loss)
swe_GS <- swe_scoring$TGS
swe_GC <-swe_conceding$TGC
swe_GD <- swe_scoring$TGS - swe_conceding$TGC
swe_PTS <- (swe_total_wins*3) + (swe_total_draws*1)
swe_league_table <- cbind(swe_league_table,swe_GS,swe_GC,swe_GD,swe_PTS)
swe_league_table <- as.data.frame(swe_league_table)
#rename the columns
names(swe_league_table)[names(swe_league_table) == "swe_teams"] <- "Team"
names(swe_league_table)[names(swe_league_table) == "swe_games_played"] <- "P"
names(swe_league_table)[names(swe_league_table) == "swe_total_wins"] <- "W"
names(swe_league_table)[names(swe_league_table) == "swe_total_draws"] <- "D"
names(swe_league_table)[names(swe_league_table) == "swe_total_loss"] <- "L"
names(swe_league_table)[names(swe_league_table) == "swe_GS"] <- "F"
names(swe_league_table)[names(swe_league_table) == "swe_GC"] <- "A"
points_swe <- swe_league_table[order(as.numeric(swe_league_table$swe_PTS), decreasing = TRUE),]
points_swe$swe_rank <- 1:length(swe_teams)
row.names(points_swe) <- points_swe$swe_rank
#create final_swe_hf_against with team ranks in brackets
for(swe_rowhrank in 1:nrow(swe_form_team_against_h)) {
  for(swe_colhrank in 1:ncol(swe_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!swe_form_team_against_h[swe_rowhrank,swe_colhrank]=="",swe_form_team_against_h[swe_rowhrank,swe_colhrank] <- paste(swe_form_team_against_h[swe_rowhrank,swe_colhrank],"(",points_swe$swe_rank[points_swe$Team ==swe_form_team_against_h[swe_rowhrank,swe_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_swe,'NL/SWE.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six swe###################################################
#SWE
#form
#create final_swe_hf object
#swe_last_n_games <- 6
final_swe_hf <- c()
for(index_swe_hf in 1:length(swe_teams))
{
  index_swe_hf <- row.names(swe_form_h) == swe_teams[index_swe_hf]
  form_swe_hf <- swe_form_h[index_swe_hf]
  deleted_form_swe_hf <- form_swe_hf[!form_swe_hf[] == ""]
  l6_form_swe_hf <- tail(deleted_form_swe_hf,swe_last_n_games)
  l6_form_swe_hf <- paste(l6_form_swe_hf,collapse = " ")
  final_swe_hf[index_swe_hf] <- rbind(paste(swe_teams[index_swe_hf],l6_form_swe_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swe_teams[index],l6_form)

}

#change column names
final_swe_hf <- as.data.frame(final_swe_hf)
colnames(final_swe_hf) <- "Form"
#goals scored
#create final_swe_gs object
final_swe_gs <- c()
suml6_swe_gs <- c()
for(index_swe_gs in 1:length(swe_teams))
{
  index_swe_gs <- row.names(swe_goalscored_h) == swe_teams[index_swe_gs]
  form_swe_gs <- swe_goalscored_h[index_swe_gs]
  deleted_form_swe_gs <- form_swe_gs[!form_swe_gs[] == ""]
  l6_form_swe_gs <- tail(deleted_form_swe_gs,swe_last_n_games)
  l6_form_swe_gs <- as.numeric(l6_form_swe_gs)
  suml6_swe_gs[index_swe_gs] <- sum(l6_form_swe_gs)
  suml6_swe_gs[index_swe_gs] <- paste("(",suml6_swe_gs[index_swe_gs],")",sep = "")
  l6_form_swe_gs <- paste(l6_form_swe_gs,collapse = " ")
  final_swe_gs[index_swe_gs] <- rbind(paste(swe_teams[index_swe_gs],l6_form_swe_gs,suml6_swe_gs[index_swe_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swe_teams[index],l6_form)

}
final_swe_gs
#change column names
final_swe_gs <- as.data.frame(final_swe_gs)
colnames(final_swe_gs) <- "Goals scored"
#goal conceded
#create final_swe_gc object
final_swe_gc <- c()
suml6_swe_gc <- c()
for(index_swe_gc in 1:length(swe_teams))
{
  index_swe_gc <- row.names(swe_goalconceded_h) == swe_teams[index_swe_gc]
  form_swe_gc <- swe_goalconceded_h[index_swe_gc]
  deleted_form_swe_gc <- form_swe_gc[!form_swe_gc[] == ""]
  l6_form_swe_gc <- tail(deleted_form_swe_gc,swe_last_n_games)
  l6_form_swe_gc <- as.numeric(l6_form_swe_gc)
  suml6_swe_gc[index_swe_gc] <- sum(l6_form_swe_gc)
  suml6_swe_gc[index_swe_gc] <- paste("(",suml6_swe_gc[index_swe_gc],")",sep = "")
  l6_form_swe_gc <- paste(l6_form_swe_gc,collapse = " ")
  final_swe_gc[index_swe_gc] <- rbind(paste(swe_teams[index_swe_gc],l6_form_swe_gc,suml6_swe_gc[index_swe_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swe_teams[index],l6_form)

}

#change column names
final_swe_gc <- as.data.frame(final_swe_gc)
colnames(final_swe_gc) <- "Goals conceded"
#total goals
#create final_swe_tg object
final_swe_tg <- c()
suml6_swe_tg <- c()
for(index_swe_tg in 1:length(swe_teams))
{
  index_swe_tg <- row.names(swe_totalgoals_h) == swe_teams[index_swe_tg]
  form_swe_tg <- swe_totalgoals_h[index_swe_tg]
  deleted_form_swe_tg <- form_swe_tg[!form_swe_tg[] == ""]
  l6_form_swe_tg <- tail(deleted_form_swe_tg,swe_last_n_games)
  l6_form_swe_tg <- as.numeric(l6_form_swe_tg)
  suml6_swe_tg[index_swe_tg] <- sum(l6_form_swe_tg)
  suml6_swe_tg[index_swe_tg] <- paste("(",suml6_swe_tg[index_swe_tg],")",sep = "")
  l6_form_swe_tg <- paste(l6_form_swe_tg,collapse = " ")
  final_swe_tg[index_swe_tg] <- rbind(paste(swe_teams[index_swe_tg],l6_form_swe_tg,suml6_swe_tg[index_swe_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swe_teams[index],l6_form)

}
#change column names
final_swe_tg <- as.data.frame(final_swe_tg)
colnames(final_swe_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_swe_hf object
final_swe_cs <- c()
for(index_swe_cs in 1:length(swe_teams))
{
  index_swe_cs <- row.names(swe_csform_h) == swe_teams[index_swe_cs]
  csform_swe_cs <- swe_csform_h[index_swe_cs]
  deleted_csform_swe_cs <- csform_swe_cs[!csform_swe_cs[] == ""]
  l6_csform_swe_cs <- tail(deleted_csform_swe_cs,swe_last_n_games)
  l6_csform_swe_cs <- paste(l6_csform_swe_cs,collapse = " ")
  final_swe_cs[index_swe_cs] <- rbind(paste(swe_teams[index_swe_cs],l6_csform_swe_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",swe_teams[index],l6_csform)

}

#change column names
final_swe_cs <- as.data.frame(final_swe_cs)
colnames(final_swe_cs) <- "CSForm"
#################################################
################################################
#Win Margin
#goals scored
#create final_swe_wm object
final_swe_wm <- c()
suml6_swe_wm <- c()
for(index_swe_wm in 1:length(swe_teams))
{
  index_swe_wm <- row.names(swe_winmargin_h) == swe_teams[index_swe_wm]
  form_swe_wm <- swe_winmargin_h[index_swe_wm]
  deleted_form_swe_wm <- form_swe_wm[!form_swe_wm[] == ""]
  l6_form_swe_wm <- tail(deleted_form_swe_wm,swe_last_n_games)
  l6_form_swe_wm <- as.numeric(l6_form_swe_wm)
  suml6_swe_wm[index_swe_wm] <- sum(l6_form_swe_wm)
  suml6_swe_wm[index_swe_wm] <- paste("(",suml6_swe_wm[index_swe_wm],")",sep = "")
  l6_form_swe_wm <- paste(l6_form_swe_wm,collapse = " ")
  final_swe_wm[index_swe_wm] <- rbind(paste(swe_teams[index_swe_wm],l6_form_swe_wm,suml6_swe_wm[index_swe_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swe_teams[index],l6_form)

}
final_swe_wm
#change column names
final_swe_wm <- as.data.frame(final_swe_wm)
colnames(final_swe_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_swe_hf_against
final_swe_hf_against <- c()
for(index_swe_hf_against in 1:length(swe_teams))
{
  index_swe_hf_against <- row.names(swe_form_team_against_h) == swe_teams[index_swe_hf_against]
  form_swe_hf_against <- swe_form_team_against_h[index_swe_hf_against]
  deleted_form_swe_hf_against <- form_swe_hf_against[!form_swe_hf_against[] == ""]
  l6_form_swe_hf_against <- tail(deleted_form_swe_hf_against,swe_last_n_games)
  l6_form_swe_hf_against <- paste(l6_form_swe_hf_against,collapse = " ")
  final_swe_hf_against[index_swe_hf_against] <- rbind(paste(swe_teams[index_swe_hf_against],l6_form_swe_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",swe_teams[index],l6_form)

}
final_swe_hf_against <- as.data.frame(final_swe_hf_against)
colnames(final_swe_hf_against) <- "Team against"
#combine the columns
final_swe_all <- cbind(final_swe_hf,final_swe_gs,final_swe_gc,final_swe_tg,final_swe_cs,final_swe_wm,final_swe_hf_against)
write.xlsx(final_swe_all,'NL/SWE.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
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

#create home and away csv
#swe_home_poisson <- rbind(swe_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#swe_away_poisson <- rbind(swe_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(swe_home_poisson,'NL/SWE.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(swe_away_poisson,'NL/SWE.xlsx',sheetName = "awaypoisson", append = TRUE)
##########################################################################################################
###################SWE FIXTURES##########################################################################
#SWE
HomeTeam_swe <- rep(swe_teams, each = length(swe_teams))
AwayTeam_swe <- rep(swe_teams, length(swe_teams))
SWE_fixtures <- cbind(HomeTeam_swe,AwayTeam_swe)
SWE_fixtures <- as.data.frame(SWE_fixtures)
SWE_fixtures <- SWE_fixtures[!SWE_fixtures$HomeTeam_swe == SWE_fixtures$AwayTeam_swe,]
rownames(SWE_fixtures) <- NULL
SWE_fixtures$Div <- "SWE"
SWE_fixtures <- SWE_fixtures[,c(3,1,2)]

SWE_fixtures$avg_HG_swe <- swe_avg_HG

SWE_fixtures$swe_homeas <- rep(swe_home_as,each = length(swe_teams)-1)

swe_awayds_lookup <- cbind(swe_teams,swe_away_ds)

swe_awayds_lookup <- as.data.frame(swe_awayds_lookup)

colnames(swe_awayds_lookup) <- c("AwayTeam_swe","swe_awayds")


require('RH2')
SWE_fixtures$swe_awayds <- sqldf("SELECT swe_awayds_lookup.swe_awayds FROM swe_awayds_lookup INNER JOIN SWE_fixtures ON swe_awayds_lookup.AwayTeam_swe = SWE_fixtures.AwayTeam_swe")

SWE_fixtures$avg_AG_swe <- swe_avg_AG

swe_awayas_lookup <- cbind(swe_teams,swe_away_as)

swe_awayas_lookup <- as.data.frame(swe_awayas_lookup)

colnames(swe_awayas_lookup) <- c("AwayTeam_swe","swe_awayas")


SWE_fixtures$swe_awayas <- sqldf("SELECT swe_awayas_lookup.swe_awayas FROM swe_awayas_lookup INNER JOIN SWE_fixtures ON swe_awayas_lookup.AwayTeam_swe = SWE_fixtures.AwayTeam_swe")

SWE_fixtures$swe_homeds <- rep(swe_home_ds,each = length(swe_teams)-1)

SWE_fixtures$swe_awayds <- as.numeric(unlist(SWE_fixtures$swe_awayds))
#xGH
SWE_fixtures$swe_xGH <- SWE_fixtures$avg_HG_swe * SWE_fixtures$swe_homeas * SWE_fixtures$swe_awayds

#xGA

SWE_fixtures$swe_awayas <- as.numeric(unlist(SWE_fixtures$swe_awayas))

SWE_fixtures$swe_xGA <- SWE_fixtures$avg_AG_swe * SWE_fixtures$swe_awayas * SWE_fixtures$swe_homeds

SWE_fixtures$swe_0_0 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_0 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_0_1 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_1 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_0 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_0_2 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_2 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_1 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_2 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_3 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_0 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_1 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_2 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_0_3 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_3 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_3 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_4 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_0 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_1 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_2 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_3 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_0_4 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_4 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_4 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_4 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_5 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_0 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_1 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_2 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_3 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_4 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_0_5 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_5 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_5 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_5 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_5 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_6 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_0 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(0,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_1 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(1,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_2 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(2,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_3 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(3,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_4 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(4,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_6_5 <- round(stats::dpois(6,SWE_fixtures$swe_xGH) * stats::dpois(5,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_0_6 <- round(stats::dpois(0,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_1_6 <- round(stats::dpois(1,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_2_6 <- round(stats::dpois(2,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_3_6 <- round(stats::dpois(3,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_4_6 <- round(stats::dpois(4,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
SWE_fixtures$swe_5_6 <- round(stats::dpois(5,SWE_fixtures$swe_xGH) * stats::dpois(6,SWE_fixtures$swe_xGA), digits = 4)
#Home win
SWE_fixtures$swe_H <- (
  SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
    SWE_fixtures$swe_5_0 + SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5
)

SWE_fixtures$swe_H <- percent(SWE_fixtures$swe_H, accuracy = 0.1)

#Draw
SWE_fixtures$swe_D <- (

  SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 + SWE_fixtures$swe_4_4 +
    SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6
)

SWE_fixtures$swe_D <- percent(SWE_fixtures$swe_D, accuracy = 0.1)

#Away

SWE_fixtures$swe_A <- (
  SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
    SWE_fixtures$swe_0_5 + SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6
)

SWE_fixtures$swe_A <- percent(SWE_fixtures$swe_A, accuracy = 0.1)

#ov25
SWE_fixtures$swe_ov25 <- (
  SWE_fixtures$swe_2_1 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 + SWE_fixtures$swe_2_3 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 + SWE_fixtures$swe_0_4 +
    SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 + SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_0 +
    SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 + SWE_fixtures$swe_0_5 +
    SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 + SWE_fixtures$swe_5_5 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5 + SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 +
    SWE_fixtures$swe_4_6 + SWE_fixtures$swe_5_6 + SWE_fixtures$swe_6_6
)
#un25
SWE_fixtures$swe_un25 <- (
  SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_0 + SWE_fixtures$swe_0_1 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_0_2
)
#odds
SWE_fixtures$swe_ov25_odds <- round((1/SWE_fixtures$swe_ov25),digits = 2)
SWE_fixtures$swe_un25_odds <- round((1/SWE_fixtures$swe_un25),digits = 2)

SWE_fixtures$swe_ov25_odds
SWE_fixtures$swe_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
SWE_fixtures$swe_BTTSY <- (
  SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_3_1 + SWE_fixtures$swe_3_2 +
    SWE_fixtures$swe_2_2 + SWE_fixtures$swe_1_3 + SWE_fixtures$swe_2_3 + SWE_fixtures$swe_3_3 + SWE_fixtures$swe_4_4 +
    SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_3 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 +
    SWE_fixtures$swe_3_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 +
    SWE_fixtures$swe_5_4 + SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_6_6 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6
)
#BTTSN
SWE_fixtures$swe_BTTSN <- (
  SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_0 + SWE_fixtures$swe_0_1 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_0_2 +
    SWE_fixtures$swe_3_0 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_5_0 +
    SWE_fixtures$swe_0_5 + SWE_fixtures$swe_6_0 + SWE_fixtures$swe_0_6
)

SWE_fixtures$swe_BTTSY_odds <- round((1/SWE_fixtures$swe_BTTSY),digits = 2)
SWE_fixtures$swe_BTTSN_odds <- round((1/SWE_fixtures$swe_BTTSN),digits = 2)

SWE_fixtures$swe_BTTSY <- percent(SWE_fixtures$swe_BTTSY, accuracy = 0.1)
SWE_fixtures$swe_BTTSN <- percent(SWE_fixtures$swe_BTTSN, accuracy = 0.1)
#odds
SWE_fixtures$swe_BTTSY_odds
SWE_fixtures$swe_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
SWE_fixtures$swe_AH_0_H <- (
  SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
    SWE_fixtures$swe_5_0 +SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5 + SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6
)
#AH_0_A
SWE_fixtures$swe_AH_0_A <- (
  SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
    SWE_fixtures$swe_0_5 +SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6 + SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6
)

#odds
SWE_fixtures$swe_AH_0_H_odds <- round((1/SWE_fixtures$swe_AH_0_H),digits = 2)
SWE_fixtures$swe_AH_0_A_odds <- round((1/SWE_fixtures$swe_AH_0_A),digits = 2)

SWE_fixtures$swe_AH_0_H_odds
SWE_fixtures$swe_AH_0_A_odds
#percentages
SWE_fixtures$swe_AH_0_H <- percent(SWE_fixtures$swe_AH_0_H, accuracy = 0.1)
SWE_fixtures$swe_AH_0_A <- percent(SWE_fixtures$swe_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
SWE_fixtures$swe_AH_n075_H <- (
  SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
    SWE_fixtures$swe_5_0 +SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5
)
#AH_n075_A
SWE_fixtures$swe_AH_n075_A <- (
  SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
    SWE_fixtures$swe_0_5 +SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6
)

#odds
SWE_fixtures$swe_AH_n075_H_odds <- round((1/SWE_fixtures$swe_AH_n075_H),digits = 2)
SWE_fixtures$swe_AH_n075_A_odds <- round((1/SWE_fixtures$swe_AH_n075_A),digits = 2)

SWE_fixtures$swe_AH_n075_H_odds
SWE_fixtures$swe_AH_n075_A_odds
#percentages
SWE_fixtures$swe_AH_n075_H <- percent(SWE_fixtures$swe_AH_n075_H, accuracy = 0.1)
SWE_fixtures$swe_AH_n075_A <- percent(SWE_fixtures$swe_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
SWE_fixtures$swe_AH_075_H <- (
  SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
    SWE_fixtures$swe_5_0 +SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5 + SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6 + SWE_fixtures$swe_0_1 + SWE_fixtures$swe_1_2 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_3_4 + SWE_fixtures$swe_4_5 + SWE_fixtures$swe_5_6
)
#AH_075_A
SWE_fixtures$swe_AH_075_A <- (
  SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
    SWE_fixtures$swe_0_5 +SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6 + SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6 + SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_3 + SWE_fixtures$swe_5_4 + SWE_fixtures$swe_6_5
)

#odds
SWE_fixtures$swe_AH_075_H_odds <- round((1/SWE_fixtures$swe_AH_075_H),digits = 2)
SWE_fixtures$swe_AH_075_A_odds <- round((1/SWE_fixtures$swe_AH_075_A),digits = 2)

SWE_fixtures$swe_AH_075_H_odds
SWE_fixtures$swe_AH_075_A_odds
#percentages
SWE_fixtures$swe_AH_075_H <- percent(SWE_fixtures$swe_AH_075_H, accuracy = 0.1)
SWE_fixtures$swe_AH_075_A <- percent(SWE_fixtures$swe_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
SWE_fixtures$swe_AH_n125_H <- (
  SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
    SWE_fixtures$swe_5_0 +SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5
)
#AH_n125_A
SWE_fixtures$swe_AH_n125_A <- (
  SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
    SWE_fixtures$swe_0_5 +SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6
)

#odds
SWE_fixtures$swe_AH_n125_H_odds <- round((1/SWE_fixtures$swe_AH_n125_H),digits = 2)
SWE_fixtures$swe_AH_n125_A_odds <- round((1/SWE_fixtures$swe_AH_n125_A),digits = 2)

SWE_fixtures$swe_AH_n125_H_odds
SWE_fixtures$swe_AH_n125_A_odds
#percentages
SWE_fixtures$swe_AH_n125_H <- percent(SWE_fixtures$swe_AH_n125_H, accuracy = 0.1)
SWE_fixtures$swe_AH_n125_A <- percent(SWE_fixtures$swe_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
SWE_fixtures$swe_AH_125_H <- (
  SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_0 + SWE_fixtures$swe_2_1 + SWE_fixtures$swe_3_0 + SWE_fixtures$swe_3_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_0 + SWE_fixtures$swe_4_1 + SWE_fixtures$swe_4_2 + SWE_fixtures$swe_4_3 +
    SWE_fixtures$swe_5_0 +SWE_fixtures$swe_5_1 + SWE_fixtures$swe_5_2 + SWE_fixtures$swe_5_3 + SWE_fixtures$swe_5_4 +
    SWE_fixtures$swe_6_0 + SWE_fixtures$swe_6_1 + SWE_fixtures$swe_6_2 + SWE_fixtures$swe_6_3 + SWE_fixtures$swe_6_4 +
    SWE_fixtures$swe_6_5 + SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6 + SWE_fixtures$swe_0_1 + SWE_fixtures$swe_1_2 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_3_4 + SWE_fixtures$swe_4_5 + SWE_fixtures$swe_5_6
)
#AH_125_A
SWE_fixtures$swe_AH_125_A <- (
  SWE_fixtures$swe_0_1 + SWE_fixtures$swe_0_2 + SWE_fixtures$swe_1_2 + SWE_fixtures$swe_0_3 + SWE_fixtures$swe_1_3 +
    SWE_fixtures$swe_2_3 + SWE_fixtures$swe_0_4 + SWE_fixtures$swe_1_4 + SWE_fixtures$swe_2_4 + SWE_fixtures$swe_3_4 +
    SWE_fixtures$swe_0_5 +SWE_fixtures$swe_1_5 + SWE_fixtures$swe_2_5 + SWE_fixtures$swe_3_5 + SWE_fixtures$swe_4_5 +
    SWE_fixtures$swe_0_6 + SWE_fixtures$swe_1_6 + SWE_fixtures$swe_2_6 + SWE_fixtures$swe_3_6 + SWE_fixtures$swe_4_6 +
    SWE_fixtures$swe_5_6 + SWE_fixtures$swe_0_0 + SWE_fixtures$swe_1_1 + SWE_fixtures$swe_2_2 + SWE_fixtures$swe_3_3 +
    SWE_fixtures$swe_4_4 + SWE_fixtures$swe_5_5 + SWE_fixtures$swe_6_6 + SWE_fixtures$swe_1_0 + SWE_fixtures$swe_2_1 +
    SWE_fixtures$swe_3_2 + SWE_fixtures$swe_4_3 + SWE_fixtures$swe_5_4 + SWE_fixtures$swe_6_5
)

#odds
SWE_fixtures$swe_AH_125_H_odds <- round((1/SWE_fixtures$swe_AH_125_H),digits = 2)
SWE_fixtures$swe_AH_125_A_odds <- round((1/SWE_fixtures$swe_AH_125_A),digits = 2)

SWE_fixtures$swe_AH_125_H_odds
SWE_fixtures$swe_AH_125_A_odds
#percentages
SWE_fixtures$swe_AH_125_H <- percent(SWE_fixtures$swe_AH_125_H, accuracy = 0.1)
SWE_fixtures$swe_AH_125_A <- percent(SWE_fixtures$swe_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
SWE_fixtures$swe_ov25 <- percent(SWE_fixtures$swe_ov25, accuracy = 0.1)

SWE_fixtures$swe_un25 <- percent(SWE_fixtures$swe_un25, accuracy = 0.1)
SWE_fixtures$swe_pscore <- paste(round(SWE_fixtures$swe_xGH,digits = 0),round(SWE_fixtures$swe_xGA,digits = 0),sep = "-")
#write out
write.xlsx(SWE_fixtures,'NL/SWE.xlsx',sheetName = "SWE", append = TRUE)
###########################################################################################################
########################SWE END###########################################################################
SWE <- read.csv('../FDAS/SWE.csv')
SWE$TG <- SWE$HG + SWE$AG
SWE$OV25 <- ifelse(SWE$TG >= 3,"Y","N")
swe_ftr_summary <- tabyl(SWE,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
swe_ov25_summary <- tabyl(SWE,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(swe_ftr_summary,'NL/SWE.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(swe_ov25_summary,'NL/SWE.xlsx',sheetName = "OVUN25", append = TRUE)



