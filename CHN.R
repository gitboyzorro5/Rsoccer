library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
library('mgsub')
library(stringr)
library(stringi)
#delete current file
unlink('NL/CHN.xlsx')
######################CHN START#######################################
#####################################################################
CHN <- read.csv('../FDAS/CHN.csv')
CHN <- within(CHN,rm(Res))
CHN$Date <- dmy(CHN$Date)
CHN <- CHN[order(as.Date(CHN$Date, format = "%d/%m%Y"), decreasing = FALSE),]
CHN$CS <- paste(CHN$HG,CHN$AG, sep = "-")

#CHN_qualificaton <- subset(CHN,tournament == "UEFA Euro qualification")
CHN <- subset(CHN,Season == "2022")
#CHN <- CHN[CHN$Date > '2008-01-01',])
CHN$TG <- CHN$HG + CHN$AG
CHN$OV25 <- ifelse(CHN$TG >= 3,"Y","N")
CHN$FTR <- with(CHN,
               ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
###################################################
# CHN <- mgsub(CHN,c("Wolfsberger"),c("Wolfsberger AC"))
# CHN <- mgsub(CHN,c("Wolfsberger AC AC"),c("Wolfsberger AC"))
####GoalTotalsv2##################################
chn_totalgoalsv2 <- tapply(CHN$TG, CHN[c("Home", "Away")],mean)
chn_totalgoalsv2
chn_hgtotals <- rowSums(chn_totalgoalsv2,na.rm = T)
chn_agtotals <- colSums(chn_totalgoalsv2,na.rm = T)

chn_totalgoals <- chn_hgtotals + chn_agtotals
chn_totalgoalsv2 <- cbind(chn_totalgoalsv2,chn_totalgoals)
chn_teams <- sort(unique(CHN$Home))
chn_home_games <- c()
chn_away_games <-c()
for (i_chn in 1:length(chn_teams))
{

  chn_home_games[i_chn] <- nrow(CHN[CHN$Home == chn_teams[i_chn],])
  chn_away_games[i_chn]  <- nrow(CHN[CHN$Away == chn_teams[i_chn],])

}
chn_games_played <- chn_home_games + chn_away_games
chn_goaltotalsv2 <- cbind(chn_totalgoalsv2,chn_games_played)
chn_avg_totalgoals <- round((chn_totalgoals/ chn_games_played), digits = 4)
chn_goaltotalsv2[is.na(chn_goaltotalsv2)] <- ""
chn_goaltotalsv2 <- cbind(chn_goaltotalsv2,chn_avg_totalgoals)
write.xlsx(chn_goaltotalsv2,'NL/CHN.xlsx',sheetName = "totalgoalsv2")
#####################################################################
CHN <- subset(CHN,Season == "2022")
chn_totalrounds <-  (length(chn_teams) - 1 )*2
chn_totalmatches <- (length(chn_teams)*(length(chn_teams) - 1))
chn_eachround <- chn_totalmatches / chn_totalrounds

chn_matchesplayed <-  nrow(CHN)

CHN_rounds <- CHN

if(chn_matchesplayed %% chn_eachround == 0)
{
  chn_currentround <- chn_matchesplayed / chn_eachround
  chn_matchday <- c()
  chn_matchday <- rep(1:chn_currentround, each = chn_eachround)
}else if(chn_matchesplayed %% chn_eachround != 0)

{

  chn_modulus <- chn_matchesplayed %% chn_eachround
  chn_currentround <- (chn_matchesplayed - chn_modulus) / chn_eachround
  chn_matchday <- c()
  chn_matchday_vec1 <- c()
  chn_matchday_vec2 <- c()
  chn_matchday_vec1 <- rep(1:chn_currentround, each = chn_eachround)
  chn_matchday_vec2[1:chn_modulus] <- c(chn_currentround + 1)
  chn_matchday <- append(chn_matchday_vec1,chn_matchday_vec2)
}
CHN_rounds <- cbind(CHN_rounds,chn_matchday)
#####################################################################################################
##############################################################################################
chn_goalscored_h <- tapply(CHN$HG, CHN[c("Home", "Date")],mean)
chn_goalscored_a <- tapply(CHN$AG, CHN[c("Away", "Date")],mean)
chn_goalscored_h[is.na(chn_goalscored_h)] <- ""
chn_goalscored_a[is.na(chn_goalscored_a)] <- ""

for(chn_rowhgs in 1:nrow(chn_goalscored_h)) {
  for(chn_colhgs in 1:ncol(chn_goalscored_h)) {

    # print(my_matrix[row, col])
    for(chn_rowags in 1:nrow(chn_goalscored_a)) {
      for(chn_colags in 1:ncol(chn_goalscored_a)) {
        ifelse(!chn_goalscored_a[chn_rowags,chn_colags]=="",chn_goalscored_h[chn_rowags,chn_colags] <- chn_goalscored_a[chn_rowags,chn_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(chn_goalscoredmatrix,'NL/CHN.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#########################################################################################
#chn goal scored rounds
final_chn_gs <- matrix(nrow = length(chn_teams),ncol = chn_totalrounds )
suml6_chn_gs <- c()
sum_chn_zero_gs <- c()
sum_chn_one_gs <- c()
sum_chn_two_gs <- c()
sum_chn_three_gs <- c()
l6_form_chn_gssplitted <- c()
form_chn_gs <- c()
for(index_chn_gs in 1:length(chn_teams))
{
  for(index_chn_gs_cols in 1:chn_totalrounds)
  {
    index_chn_gs  <- row.names(chn_goalscored_h) == chn_teams[index_chn_gs]
    form_chn_gs <- chn_goalscored_h[index_chn_gs ]
    deleted_form_chn_gs <- form_chn_gs[!form_chn_gs[] == ""]
    l6_form_chn_gs <- tail(deleted_form_chn_gs,chn_last_n_games)
    l6_form_chn_gs <- as.numeric(l6_form_chn_gs)
    suml6_chn_gs[index_chn_gs] <- sum(l6_form_chn_gs)
    suml6_chn_gs[index_chn_gs] <- paste(suml6_chn_gs[index_chn_gs],sep = "")
    sum_chn_zero_gs[index_chn_gs] <- length(which(l6_form_chn_gs == 0))
    sum_chn_zero_gs[index_chn_gs] <- paste(sum_chn_zero_gs[index_chn_gs],sep = "")
    sum_chn_one_gs[index_chn_gs] <- length(which(l6_form_chn_gs == 1))
    sum_chn_one_gs[index_chn_gs] <- paste(sum_chn_one_gs[index_chn_gs],sep = "")
    sum_chn_two_gs[index_chn_gs] <- length(which(l6_form_chn_gs >= 2))
    sum_chn_two_gs[index_chn_gs] <- paste(sum_chn_two_gs[index_chn_gs],sep = "")
    sum_chn_three_gs[index_chn_gs] <- length(which(l6_form_chn_gs >= 3))
    sum_chn_three_gs[index_chn_gs] <- paste(sum_chn_three_gs[index_chn_gs],sep = "")
    l6_form_chn_gs <- as.character(l6_form_chn_gs)
    l6_form_chn_gs_flattened <- stri_paste(l6_form_chn_gs,collapse = '')
    l6_form_chn_gssplitted <- as.numeric(strsplit(as.character(l6_form_chn_gs_flattened),"")[[1]])
    final_chn_gs[index_chn_gs,index_chn_gs_cols] <- l6_form_chn_gssplitted[index_chn_gs_cols]
  }
}

final_chn_gs[is.na(final_chn_gs)] <- ""
chn_goalscoredmatrix <- cbind(chn_teams,final_chn_gs,suml6_chn_gs,sum_chn_zero_gs,sum_chn_one_gs,sum_chn_two_gs,sum_chn_three_gs)
write.xlsx(chn_goalscoredmatrix,'NL/CHN.xlsx',sheetName = "gsmatrix", append = TRUE)
#################################################################################################################################

####GCmatrix#####################################################################################################################
#create home and away matrices
chn_goalconceded_h <- tapply(CHN$AG, CHN[c("Home", "Date")],mean)
chn_goalconceded_a <- tapply(CHN$HG, CHN[c("Away", "Date")],mean)
chn_goalconceded_h[is.na(chn_goalconceded_h)] <- ""
chn_goalconceded_a[is.na(chn_goalconceded_a)] <- ""

for(chn_rowhgc in 1:nrow(chn_goalconceded_h)) {
  for(chn_colhgc in 1:ncol(chn_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(chn_rowagc in 1:nrow(chn_goalconceded_a)) {
      for(chn_colagc in 1:ncol(chn_goalconceded_a)) {
        ifelse(!chn_goalconceded_a[chn_rowagc,chn_colagc]=="",chn_goalconceded_h[chn_rowagc,chn_colagc] <- chn_goalconceded_a[chn_rowagc,chn_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(chn_goalconcededmatrix,'NL/CHN.xlsx',sheetName = "gcmatrix", append = TRUE)
############################################################################################################################################################
#chn goal conceded rounds
final_chn_gc <- matrix(nrow = length(chn_teams),ncol = chn_totalrounds )
suml6_chn_gc <- c()
sum_chn_zero_gc <- c()
sum_chn_one_gc <- c()
sum_chn_two_gc <- c()
sum_chn_three_gc <- c()
l6_form_chn_gcsplitted <- c()
form_chn_gc <- c()
for(index_chn_gc in 1:length(chn_teams))
{
  for(index_chn_gc_cols in 1:chn_totalrounds)
  {
    index_chn_gc  <- row.names(chn_goalconceded_h) == chn_teams[index_chn_gc]
    form_chn_gc <- chn_goalconceded_h[index_chn_gc ]
    deleted_form_chn_gc <- form_chn_gc[!form_chn_gc[] == ""]
    l6_form_chn_gc <- tail(deleted_form_chn_gc,chn_last_n_games)
    l6_form_chn_gc <- as.numeric(l6_form_chn_gc)
    suml6_chn_gc[index_chn_gc] <- sum(l6_form_chn_gc)
    suml6_chn_gc[index_chn_gc] <- paste(suml6_chn_gc[index_chn_gc],sep = "")
    sum_chn_zero_gc[index_chn_gc] <- length(which(l6_form_chn_gc == 0))
    sum_chn_zero_gc[index_chn_gc] <- paste(sum_chn_zero_gc[index_chn_gc],sep = "")
    sum_chn_one_gc[index_chn_gc] <- length(which(l6_form_chn_gc == 1))
    sum_chn_one_gc[index_chn_gc] <- paste(sum_chn_one_gc[index_chn_gc],sep = "")
    sum_chn_two_gc[index_chn_gc] <- length(which(l6_form_chn_gc >= 2))
    sum_chn_two_gc[index_chn_gc] <- paste(sum_chn_two_gc[index_chn_gc],sep = "")
    sum_chn_three_gc[index_chn_gc] <- length(which(l6_form_chn_gc >= 3))
    sum_chn_three_gc[index_chn_gc] <- paste(sum_chn_three_gc[index_chn_gc],sep = "")
    l6_form_chn_gc <- as.character(l6_form_chn_gc)
    l6_form_chn_gc_flattened <- stri_paste(l6_form_chn_gc,collapse = '')
    l6_form_chn_gcsplitted <- as.numeric(strsplit(as.character(l6_form_chn_gc_flattened),"")[[1]])
    final_chn_gc[index_chn_gc,index_chn_gc_cols] <- l6_form_chn_gcsplitted[index_chn_gc_cols]
  }
}

final_chn_gc[is.na(final_chn_gc)] <- ""
chn_goalconcededmatrix <- cbind(chn_teams,final_chn_gc,suml6_chn_gc,sum_chn_zero_gc,sum_chn_one_gc,sum_chn_two_gc,sum_chn_three_gc)
write.xlsx(chn_goalconcededmatrix,'NL/CHN.xlsx',sheetName = "gcmatrix2", append = TRUE)
###################################################################################################################################

###################################################################################################################################
####Teamform#######################################################################################################################

chn_form_h <- tapply(CHN$FTR, CHN[c("Home", "Date")],median)
chn_form_a <- tapply(CHN$FTR, CHN[c("Away", "Date")],median)
chn_form_h[is.na(chn_form_h)] <- ""
chn_form_a[is.na(chn_form_a)] <- ""
chn_form_h <- sub("A","L",chn_form_h)
chn_form_h <- sub("H","W",chn_form_h)
chn_form_a <- sub("A","W",chn_form_a)
chn_form_a <- sub("H","L",chn_form_a)
for(chn_rowh_f in 1:nrow(chn_form_h)) {
  for(chn_colh_f in 1:ncol(chn_form_h)) {

    # print(my_matrix[row, col])
    for(chn_rowa_f in 1:nrow(chn_form_a)) {
      for(chn_cola_f in 1:ncol(chn_form_a)) {
        ifelse(!chn_form_a[chn_rowa_f,chn_cola_f]=="",chn_form_h[chn_rowa_f,chn_cola_f] <- chn_form_a[chn_rowa_f,chn_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#chn team form
final_chn_hf <- matrix(nrow = length(chn_teams),ncol = chn_totalrounds )
suml6_chn_hf <- c()
l6_form_chn_hfsplitted <- c()
form_chn_hf <- c()
for(index_chn_hf in 1:length(chn_teams))
{
  for(index_chn_hf_cols in 1:chn_totalrounds)
  {
    index_chn_hf  <- row.names(chn_form_h) == chn_teams[index_chn_hf]
    form_chn_hf <- chn_form_h[index_chn_hf ]
    deleted_form_chn_hf <- form_chn_hf[!form_chn_hf[] == ""]
    l6_form_chn_hf <- tail(deleted_form_chn_hf,chn_last_n_games)
    # #l6_form_chn_hf <- as.numeric(l6_form_chn_hf)
    # suml6_chn_hf[index_chn_hf] <- sum(l6_form_chn_hf)
    # suml6_chn_hf[index_chn_hf] <- paste(suml6_chn_hf[index_chn_hf],sep = "")
    #l6_form_chn_hf <- as.character(l6_form_chn_hf)
    l6_form_chn_hf_flattened <- stri_paste(l6_form_chn_hf,collapse = '')
    l6_form_chn_hfsplitted <- (strsplit(as.character(l6_form_chn_hf_flattened),"")[[1]])
    final_chn_hf[index_chn_hf,index_chn_hf_cols] <- l6_form_chn_hfsplitted[index_chn_hf_cols]
  }
}
final_chn_hf[is.na(final_chn_hf)] <- ""
chn_formmatrix <- cbind(chn_teams,final_chn_hf)

write.xlsx(chn_formmatrix,'NL/CHN.xlsx',sheetName = "form", append = TRUE)
######################################################################################################################################
######################################################################################################################################

#######TGMatrix#######################################################################################################################
chn_totalgoals_h <- tapply(CHN$TG, CHN[c("Home", "Date")],mean)
chn_totalgoals_a <- tapply(CHN$TG, CHN[c("Away", "Date")],mean)
chn_totalgoals_h[is.na(chn_totalgoals_h)] <- ""
chn_totalgoals_a[is.na(chn_totalgoals_a)] <- ""
for(chn_rowh in 1:nrow(chn_totalgoals_h)) {
  for(chn_colh in 1:ncol(chn_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(chn_rowa in 1:nrow(chn_totalgoals_a)) {
      for(chn_cola in 1:ncol(chn_totalgoals_a)) {
        ifelse(!chn_totalgoals_a[chn_rowa,chn_cola]=="",chn_totalgoals_h[chn_rowa,chn_cola] <- chn_totalgoals_a[chn_rowa,chn_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#chn total goals rounds
#chn
final_chn_tg <- matrix(nrow = length(chn_teams),ncol = chn_totalrounds )
suml6_chn_tg <- c()
l6_form_chn_tgsplitted <- c()
form_chn_tg <- c()
for(index_chn_tg in 1:length(chn_teams))
{
  for(index_chn_tg_cols in 1:chn_totalrounds)
  {
    index_chn_tg  <- row.names(chn_totalgoals_h) == chn_teams[index_chn_tg]
    form_chn_tg <- chn_totalgoals_h[index_chn_tg ]
    deleted_form_chn_tg <- form_chn_tg[!form_chn_tg[] == ""]
    l6_form_chn_tg <- tail(deleted_form_chn_tg,chn_last_n_games)
    l6_form_chn_tg <- as.numeric(l6_form_chn_tg)
    suml6_chn_tg[index_chn_tg] <- sum(l6_form_chn_tg)
    suml6_chn_tg[index_chn_tg] <- paste(suml6_chn_tg[index_chn_tg],sep = "")
    l6_form_chn_tg <- as.character(l6_form_chn_tg)
    l6_form_chn_tg_flattened <- stri_paste(l6_form_chn_tg,collapse = '')
    l6_form_chn_tgsplitted <- as.numeric(strsplit(as.character(l6_form_chn_tg_flattened),"")[[1]])
    final_chn_tg[index_chn_tg,index_chn_tg_cols] <- l6_form_chn_tgsplitted[index_chn_tg_cols]
  }
}

final_chn_tg[is.na(final_chn_tg)] <- ""
chn_goaltotalmatrix <- cbind(chn_teams,final_chn_tg,suml6_chn_tg)

write.xlsx(chn_goaltotalmatrix,'NL/CHN.xlsx',sheetName = "tgmatrix", append = TRUE)
#############################################################################################################################################
#######TeamAgainst###########################################################################################################################
chn_form_team_against_h <- tapply(CHN$Away, CHN[c("Home", "Date")],median)
chn_form_team_against_a <- tapply(CHN$Home, CHN[c("Away", "Date")],median)
chn_form_team_against_h[is.na(chn_form_team_against_h)] <- ""
chn_form_team_against_a[is.na(chn_form_team_against_a)] <- ""
for(chn_rowh_f_against in 1:nrow(chn_form_team_against_h)) {
  for(chn_colh_f_against in 1:ncol(chn_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(chn_rowa_f_against in 1:nrow(chn_form_team_against_a)) {
      for(chn_cola_f_against in 1:ncol(chn_form_team_against_a)) {
        ifelse(!chn_form_team_against_a[chn_rowa_f_against,chn_cola_f_against]=="",chn_form_team_against_h[chn_rowa_f_against,chn_cola_f_against] <- chn_form_team_against_a[chn_rowa_f_against,chn_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#win margin
chn_winmargin_h <- tapply(CHN$HG - CHN$AG, CHN[c("Home", "Date")],mean)
chn_winmargin_a <- tapply(CHN$AG - CHN$HG, CHN[c("Away", "Date")],mean)
chn_winmargin_h[is.na(chn_winmargin_h)] <- ""
#
for(chn_rowhwm in 1:nrow(chn_winmargin_h)) {
  for(chn_colhwm in 1:ncol(chn_winmargin_h)) {

    # print(my_matrix[row, col])
    for(chn_rowawm in 1:nrow(chn_winmargin_a)) {
      for(chn_colawm in 1:ncol(chn_winmargin_a)) {
        ifelse(!chn_winmargin_a[chn_rowawm,chn_colawm]=="",chn_winmargin_h[chn_rowawm,chn_colawm] <- chn_winmargin_a[chn_rowawm,chn_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
final_chn_wm <- matrix(nrow = length(chn_teams),ncol = chn_totalrounds )
suml6_chn_wm <- c()
suml6_chn_wm_negone <- c()
suml6_chn_wm_negtwo <- c()
suml6_chn_wm_zero <- c()
suml6_chn_wm_posone <- c()
suml6_chn_wm_postwo <- c()
l6_form_chn_wmsplitted <- c()
form_chn_wm <- c()
for(index_chn_wm in 1:length(chn_teams))
{
  for(index_chn_wm_cols in 1:chn_totalrounds)
  {
    index_chn_wm  <- row.names(chn_winmargin_h) == chn_teams[index_chn_wm]
    form_chn_wm <- chn_winmargin_h[index_chn_wm ]
    deleted_form_chn_wm <- form_chn_wm[!form_chn_wm[] == ""]
    l6_form_chn_wm <- tail(deleted_form_chn_wm,chn_last_n_games)
    l6_form_chn_wm <- as.numeric(l6_form_chn_wm)
    suml6_chn_wm[index_chn_wm] <- sum(l6_form_chn_wm)
    suml6_chn_wm[index_chn_wm] <- paste(suml6_chn_wm[index_chn_wm],sep = "")
    suml6_chn_wm_negone[index_chn_wm] <- length(which(l6_form_chn_wm == -1))
    suml6_chn_wm_negone[index_chn_wm] <- paste(suml6_chn_wm_negone[index_chn_wm],sep = "")
    suml6_chn_wm_negtwo[index_chn_wm] <- length(which(l6_form_chn_wm <= -2))
    suml6_chn_wm_negtwo[index_chn_wm] <- paste(suml6_chn_wm_negtwo[index_chn_wm],sep = "")
    suml6_chn_wm_zero[index_chn_wm] <- length(which(l6_form_chn_wm == 0))
    suml6_chn_wm_zero[index_chn_wm] <- paste(suml6_chn_wm_zero[index_chn_wm],sep = "")
    suml6_chn_wm_posone[index_chn_wm] <- length(which(l6_form_chn_wm == 1))
    suml6_chn_wm_posone[index_chn_wm] <- paste(suml6_chn_wm_posone[index_chn_wm],sep = "")
    suml6_chn_wm_postwo[index_chn_wm] <- length(which(l6_form_chn_wm == 2))
    suml6_chn_wm_postwo[index_chn_wm] <- paste(suml6_chn_wm_postwo[index_chn_wm],sep = "")
    l6_form_chn_wm <- as.character(l6_form_chn_wm)
    l6_form_chn_wm_flattened <- stri_paste(l6_form_chn_wm,collapse = ',')
    l6_form_chn_wmsplitted <- (strsplit(as.character(l6_form_chn_wm_flattened),",")[[1]])
    final_chn_wm[index_chn_wm,index_chn_wm_cols] <- l6_form_chn_wmsplitted[index_chn_wm_cols]
  }
}

final_chn_wm[is.na(final_chn_wm)] <- ""
chn_winmarginmatrix <- cbind(chn_teams,final_chn_wm,suml6_chn_wm,suml6_chn_wm_negtwo,suml6_chn_wm_negone,suml6_chn_wm_zero,suml6_chn_wm_posone,suml6_chn_wm_postwo)
write.xlsx(chn_winmarginmatrix,'NL/CHN.xlsx',sheetName = "winmargin", append = TRUE)
####################################################################################################################
##########Goals over under############
#CHN
chn_un05_home <- c()
chn_un05_away <- c()
chn_ov05_home <- c()
chn_ov05_away <- c()

chn_un15_home <- c()
chn_un15_away <- c()
chn_ov15_home <- c()
chn_ov15_away <- c()

chn_un25_home <- c()
chn_un25_away <- c()
chn_ov25_home <- c()
chn_ov25_away <- c()

chn_un35_home <- c()
chn_un35_away <- c()
chn_ov35_home <- c()
chn_ov35_away <- c()

chn_un45_home <- c()
chn_un45_away <- c()
chn_ov45_home <- c()
chn_ov45_away <- c()

chn_un55_home <- c()
chn_un55_away <- c()
chn_ov55_home <- c()
chn_ov55_away <- c()

for (i_chn_tg in 1:length(chn_teams))
{

  chn_un05_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG == 0,])
  chn_un05_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG == 0,])

  chn_ov05_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG > 0,])
  chn_ov05_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG > 0,])

  chn_un15_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG <= 1,])
  chn_un15_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG <= 1,])

  chn_ov15_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG >= 2,])
  chn_ov15_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG >= 2,])

  chn_un25_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG <= 2,])
  chn_un25_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG <= 2,])

  chn_ov25_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG >=3,])
  chn_ov25_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG >=3,])

  chn_un35_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG <= 3,])
  chn_un35_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG <= 3,])

  chn_ov35_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG >= 4,])
  chn_ov35_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG >= 4,])

  chn_un45_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG <= 4,])
  chn_un45_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG <= 4,])

  chn_ov45_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG >= 5,])
  chn_ov45_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG >= 5,])

  chn_un55_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG <= 5,])
  chn_un55_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG <= 5,])

  chn_ov55_home[i_chn_tg] <- nrow(CHN[CHN$Home == chn_teams[i_chn_tg] & CHN$TG >= 6,])
  chn_ov55_away[i_chn_tg] <- nrow(CHN[CHN$Away == chn_teams[i_chn_tg] & CHN$TG >= 6,])


}

chn_un05 <- chn_un05_home + chn_un05_away
chn_ov05 <- chn_ov05_home + chn_ov05_away

chn_un15 <- chn_un15_home + chn_un15_away
chn_ov15 <- chn_ov15_home + chn_ov15_away

chn_un25 <- chn_un25_home + chn_un25_away
chn_ov25 <- chn_ov25_home + chn_ov25_away

chn_un35 <- chn_un35_home + chn_un35_away
chn_ov35 <- chn_ov35_home + chn_ov35_away

chn_un45 <- chn_un45_home + chn_un45_away
chn_ov45 <- chn_ov45_home + chn_ov45_away

chn_un55 <- chn_un55_home + chn_un55_away
chn_ov55 <- chn_ov55_home + chn_ov55_away

chn_ovundata <- cbind(chn_teams,chn_un05,chn_ov05,chn_un15,chn_ov15,chn_un25,chn_ov25,chn_un35,chn_ov35,chn_un45,chn_ov45,chn_un55,chn_ov55)
write.xlsx(chn_ovundata,'NL/CHN.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
chn_csform_h <- tapply(CHN$CS, CHN[c("Home", "Date")],median)
chn_csform_a <- tapply(CHN$CS, CHN[c("Away", "Date")],median)

chn_csform_h[is.na(chn_csform_h)] <- ""
chn_csform_a[is.na(chn_csform_a)] <- ""

for(chn_rowh_f_cs in 1:nrow(chn_csform_h)) {
  for(chn_colh_f_cs in 1:ncol(chn_csform_h)) {

    # print(my_matrix[row, col])
    for(chn_rowa_f_cs in 1:nrow(chn_csform_a)) {
      for(chn_cola_f_cs in 1:ncol(chn_csform_a)) {
        ifelse(!chn_csform_a[chn_rowa_f_cs,chn_cola_f_cs]=="",chn_csform_h[chn_rowa_f_cs,chn_cola_f_cs] <- chn_csform_a[chn_rowa_f_cs,chn_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
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

######################################################################################
###########League Table###############################################################

#hwins and away wins
chn_home_wins <- c()
chn_away_wins <- c()
chn_home_draws <- c()
chn_away_draws <- c()
chn_home_loss <- c()
chn_away_loss <- c()



for (i_chn_wins in 1:length(chn_teams))
{

  chn_home_wins[i_chn_wins] <- nrow(CHN[CHN$Home == chn_teams[i_chn_wins] & CHN$FTR == "H",])
  chn_away_wins[i_chn_wins] <- nrow(CHN[CHN$Away == chn_teams[i_chn_wins] & CHN$FTR == "A",])
  chn_home_draws[i_chn_wins] <- nrow(CHN[CHN$Home == chn_teams[i_chn_wins] & CHN$FTR == "D",])
  chn_away_draws[i_chn_wins] <- nrow(CHN[CHN$Away == chn_teams[i_chn_wins] & CHN$FTR == "D",])
  chn_home_loss[i_chn_wins] <- nrow(CHN[CHN$Home == chn_teams[i_chn_wins] & CHN$FTR == "A",])
  chn_away_loss[i_chn_wins] <- nrow(CHN[CHN$Away == chn_teams[i_chn_wins] & CHN$FTR == "H",])

}

chn_total_wins <- chn_home_wins + chn_away_wins
chn_total_draws <- chn_home_draws + chn_away_draws
chn_total_loss <- chn_home_loss + chn_away_loss

chn_league_table <- cbind(chn_teams,chn_games_played,chn_total_wins,chn_total_draws,chn_total_loss)
chn_GS <- chn_scoring$TGS
chn_GC <-chn_conceding$TGC
chn_GD <- chn_scoring$TGS - chn_conceding$TGC
chn_PTS <- (chn_total_wins*3) + (chn_total_draws*1)
chn_league_table <- cbind(chn_league_table,chn_GS,chn_GC,chn_GD,chn_PTS)
chn_league_table <- as.data.frame(chn_league_table)
#rename the columns
names(chn_league_table)[names(chn_league_table) == "chn_teams"] <- "Team"
names(chn_league_table)[names(chn_league_table) == "chn_games_played"] <- "P"
names(chn_league_table)[names(chn_league_table) == "chn_total_wins"] <- "W"
names(chn_league_table)[names(chn_league_table) == "chn_total_draws"] <- "D"
names(chn_league_table)[names(chn_league_table) == "chn_total_loss"] <- "L"
names(chn_league_table)[names(chn_league_table) == "chn_GS"] <- "F"
names(chn_league_table)[names(chn_league_table) == "chn_GC"] <- "A"
points_chn <- chn_league_table[order(as.numeric(chn_league_table$chn_PTS), decreasing = TRUE),]
points_chn$chn_rank <- 1:length(chn_teams)
row.names(points_chn) <- points_chn$chn_rank
#create final_chn_hf_against with team ranks in brackets
for(chn_rowhrank in 1:nrow(chn_form_team_against_h)) {
  for(chn_colhrank in 1:ncol(chn_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!chn_form_team_against_h[chn_rowhrank,chn_colhrank]=="",chn_form_team_against_h[chn_rowhrank,chn_colhrank] <- paste(chn_form_team_against_h[chn_rowhrank,chn_colhrank],"(",points_chn$chn_rank[points_chn$Team ==chn_form_team_against_h[chn_rowhrank,chn_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_chn,'NL/CHN.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six chn###################################################
#CHN
#form
#create final_chn_hf object
#chn_last_n_games <- 6
final_chn_hf <- c()
for(index_chn_hf in 1:length(chn_teams))
{
  index_chn_hf <- row.names(chn_form_h) == chn_teams[index_chn_hf]
  form_chn_hf <- chn_form_h[index_chn_hf]
  deleted_form_chn_hf <- form_chn_hf[!form_chn_hf[] == ""]
  l6_form_chn_hf <- tail(deleted_form_chn_hf,chn_last_n_games)
  l6_form_chn_hf <- paste(l6_form_chn_hf,collapse = " ")
  final_chn_hf[index_chn_hf] <- rbind(paste(chn_teams[index_chn_hf],l6_form_chn_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",chn_teams[index],l6_form)

}

#change column names
final_chn_hf <- as.data.frame(final_chn_hf)
colnames(final_chn_hf) <- "Form"
#goals scored
#create final_chn_gs object
final_chn_gs <- c()
suml6_chn_gs <- c()
for(index_chn_gs in 1:length(chn_teams))
{
  index_chn_gs <- row.names(chn_goalscored_h) == chn_teams[index_chn_gs]
  form_chn_gs <- chn_goalscored_h[index_chn_gs]
  deleted_form_chn_gs <- form_chn_gs[!form_chn_gs[] == ""]
  l6_form_chn_gs <- tail(deleted_form_chn_gs,chn_last_n_games)
  l6_form_chn_gs <- as.numeric(l6_form_chn_gs)
  suml6_chn_gs[index_chn_gs] <- sum(l6_form_chn_gs)
  suml6_chn_gs[index_chn_gs] <- paste("(",suml6_chn_gs[index_chn_gs],")",sep = "")
  l6_form_chn_gs <- paste(l6_form_chn_gs,collapse = " ")
  final_chn_gs[index_chn_gs] <- rbind(paste(chn_teams[index_chn_gs],l6_form_chn_gs,suml6_chn_gs[index_chn_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",chn_teams[index],l6_form)

}
final_chn_gs
#change column names
final_chn_gs <- as.data.frame(final_chn_gs)
colnames(final_chn_gs) <- "Goals scored"
#goal conceded
#create final_chn_gc object
final_chn_gc <- c()
suml6_chn_gc <- c()
for(index_chn_gc in 1:length(chn_teams))
{
  index_chn_gc <- row.names(chn_goalconceded_h) == chn_teams[index_chn_gc]
  form_chn_gc <- chn_goalconceded_h[index_chn_gc]
  deleted_form_chn_gc <- form_chn_gc[!form_chn_gc[] == ""]
  l6_form_chn_gc <- tail(deleted_form_chn_gc,chn_last_n_games)
  l6_form_chn_gc <- as.numeric(l6_form_chn_gc)
  suml6_chn_gc[index_chn_gc] <- sum(l6_form_chn_gc)
  suml6_chn_gc[index_chn_gc] <- paste("(",suml6_chn_gc[index_chn_gc],")",sep = "")
  l6_form_chn_gc <- paste(l6_form_chn_gc,collapse = " ")
  final_chn_gc[index_chn_gc] <- rbind(paste(chn_teams[index_chn_gc],l6_form_chn_gc,suml6_chn_gc[index_chn_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",chn_teams[index],l6_form)

}

#change column names
final_chn_gc <- as.data.frame(final_chn_gc)
colnames(final_chn_gc) <- "Goals conceded"
#total goals
#create final_chn_tg object
final_chn_tg <- c()
suml6_chn_tg <- c()
for(index_chn_tg in 1:length(chn_teams))
{
  index_chn_tg <- row.names(chn_totalgoals_h) == chn_teams[index_chn_tg]
  form_chn_tg <- chn_totalgoals_h[index_chn_tg]
  deleted_form_chn_tg <- form_chn_tg[!form_chn_tg[] == ""]
  l6_form_chn_tg <- tail(deleted_form_chn_tg,chn_last_n_games)
  l6_form_chn_tg <- as.numeric(l6_form_chn_tg)
  suml6_chn_tg[index_chn_tg] <- sum(l6_form_chn_tg)
  suml6_chn_tg[index_chn_tg] <- paste("(",suml6_chn_tg[index_chn_tg],")",sep = "")
  l6_form_chn_tg <- paste(l6_form_chn_tg,collapse = " ")
  final_chn_tg[index_chn_tg] <- rbind(paste(chn_teams[index_chn_tg],l6_form_chn_tg,suml6_chn_tg[index_chn_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",chn_teams[index],l6_form)

}
#change column names
final_chn_tg <- as.data.frame(final_chn_tg)
colnames(final_chn_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_chn_hf object
final_chn_cs <- c()
for(index_chn_cs in 1:length(chn_teams))
{
  index_chn_cs <- row.names(chn_csform_h) == chn_teams[index_chn_cs]
  csform_chn_cs <- chn_csform_h[index_chn_cs]
  deleted_csform_chn_cs <- csform_chn_cs[!csform_chn_cs[] == ""]
  l6_csform_chn_cs <- tail(deleted_csform_chn_cs,chn_last_n_games)
  l6_csform_chn_cs <- paste(l6_csform_chn_cs,collapse = " ")
  final_chn_cs[index_chn_cs] <- rbind(paste(chn_teams[index_chn_cs],l6_csform_chn_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",chn_teams[index],l6_csform)

}

#change column names
final_chn_cs <- as.data.frame(final_chn_cs)
colnames(final_chn_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_chn_wm object
final_chn_wm <- c()
suml6_chn_wm <- c()
for(index_chn_wm in 1:length(chn_teams))
{
  index_chn_wm <- row.names(chn_winmargin_h) == chn_teams[index_chn_wm]
  form_chn_wm <- chn_winmargin_h[index_chn_wm]
  deleted_form_chn_wm <- form_chn_wm[!form_chn_wm[] == ""]
  l6_form_chn_wm <- tail(deleted_form_chn_wm,chn_last_n_games)
  l6_form_chn_wm <- as.numeric(l6_form_chn_wm)
  suml6_chn_wm[index_chn_wm] <- sum(l6_form_chn_wm)
  suml6_chn_wm[index_chn_wm] <- paste("(",suml6_chn_wm[index_chn_wm],")",sep = "")
  l6_form_chn_wm <- paste(l6_form_chn_wm,collapse = " ")
  final_chn_wm[index_chn_wm] <- rbind(paste(chn_teams[index_chn_wm],l6_form_chn_wm,suml6_chn_wm[index_chn_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",chn_teams[index],l6_form)

}
final_chn_wm
#change column names
final_chn_wm <- as.data.frame(final_chn_wm)
colnames(final_chn_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_chn_hf_against
final_chn_hf_against <- c()
for(index_chn_hf_against in 1:length(chn_teams))
{
  index_chn_hf_against <- row.names(chn_form_team_against_h) == chn_teams[index_chn_hf_against]
  form_chn_hf_against <- chn_form_team_against_h[index_chn_hf_against]
  deleted_form_chn_hf_against <- form_chn_hf_against[!form_chn_hf_against[] == ""]
  l6_form_chn_hf_against <- tail(deleted_form_chn_hf_against,chn_last_n_games)
  l6_form_chn_hf_against <- paste(l6_form_chn_hf_against,collapse = " ")
  final_chn_hf_against[index_chn_hf_against] <- rbind(paste(chn_teams[index_chn_hf_against],l6_form_chn_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",chn_teams[index],l6_form)

}
final_chn_hf_against <- as.data.frame(final_chn_hf_against)
colnames(final_chn_hf_against) <- "Team against"
#combine the columns
final_chn_all <- cbind(final_chn_hf,final_chn_gs,final_chn_gc,final_chn_tg,final_chn_cs,final_chn_wm,final_chn_hf_against)
write.xlsx(final_chn_all,'NL/CHN.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
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

#create home and away csv
#chn_home_poisson <- rbind(chn_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#chn_away_poisson <- rbind(chn_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(chn_home_poisson,'NL/CHN.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(chn_away_poisson,'NL/CHN.xlsx',sheetName = "awaypoisson", append = TRUE)
chn_home_poisson
chn_away_poisson
##########################################################################################################
###################CHN FIXTURES##########################################################################
#CHN
HomeTeam_chn <- rep(chn_teams, each = length(chn_teams))
AwayTeam_chn <- rep(chn_teams, length(chn_teams))
CHN_fixtures <- cbind(HomeTeam_chn,AwayTeam_chn)
CHN_fixtures <- as.data.frame(CHN_fixtures)
CHN_fixtures <- CHN_fixtures[!CHN_fixtures$HomeTeam_chn == CHN_fixtures$AwayTeam_chn,]
rownames(CHN_fixtures) <- NULL
CHN_fixtures$Div <- "CHN"
CHN_fixtures <- CHN_fixtures[,c(3,1,2)]

CHN_fixtures$avg_HG_chn <- chn_avg_HG

CHN_fixtures$chn_homeas <- rep(chn_home_as,each = length(chn_teams)-1)

chn_awayds_lookup <- cbind(chn_teams,chn_away_ds)

chn_awayds_lookup <- as.data.frame(chn_awayds_lookup)

colnames(chn_awayds_lookup) <- c("AwayTeam_chn","chn_awayds")


require('RH2')
CHN_fixtures$chn_awayds <- sqldf("SELECT chn_awayds_lookup.chn_awayds FROM chn_awayds_lookup INNER JOIN CHN_fixtures ON chn_awayds_lookup.AwayTeam_chn = CHN_fixtures.AwayTeam_chn")

CHN_fixtures$avg_AG_chn <- chn_avg_AG

chn_awayas_lookup <- cbind(chn_teams,chn_away_as)

chn_awayas_lookup <- as.data.frame(chn_awayas_lookup)

colnames(chn_awayas_lookup) <- c("AwayTeam_chn","chn_awayas")


CHN_fixtures$chn_awayas <- sqldf("SELECT chn_awayas_lookup.chn_awayas FROM chn_awayas_lookup INNER JOIN CHN_fixtures ON chn_awayas_lookup.AwayTeam_chn = CHN_fixtures.AwayTeam_chn")

CHN_fixtures$chn_homeds <- rep(chn_home_ds,each = length(chn_teams)-1)

CHN_fixtures$chn_awayds <- as.numeric(unlist(CHN_fixtures$chn_awayds))
#xGH
CHN_fixtures$chn_xGH <- CHN_fixtures$avg_HG_chn * CHN_fixtures$chn_homeas * CHN_fixtures$chn_awayds

#xGA

CHN_fixtures$chn_awayas <- as.numeric(unlist(CHN_fixtures$chn_awayas))

CHN_fixtures$chn_xGA <- CHN_fixtures$avg_AG_chn * CHN_fixtures$chn_awayas * CHN_fixtures$chn_homeds

CHN_fixtures$chn_0_0 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_0 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_0_1 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_1 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_0 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_0_2 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_2 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_1 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_2 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_3 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_0 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_1 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_2 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_0_3 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_3 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_3 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_4 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_0 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_1 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_2 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_3 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_0_4 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_4 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_4 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_4 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_5 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_0 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_1 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_2 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_3 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_4 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_0_5 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_5 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_5 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_5 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_5 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_6 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_0 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(0,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_1 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(1,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_2 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(2,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_3 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(3,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_4 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(4,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_6_5 <- round(stats::dpois(6,CHN_fixtures$chn_xGH) * stats::dpois(5,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_0_6 <- round(stats::dpois(0,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_1_6 <- round(stats::dpois(1,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_2_6 <- round(stats::dpois(2,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_3_6 <- round(stats::dpois(3,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_4_6 <- round(stats::dpois(4,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
CHN_fixtures$chn_5_6 <- round(stats::dpois(5,CHN_fixtures$chn_xGH) * stats::dpois(6,CHN_fixtures$chn_xGA), digits = 4)
#Home win
CHN_fixtures$chn_H <- (
  CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
    CHN_fixtures$chn_5_0 + CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5
)

CHN_fixtures$chn_H <- percent(CHN_fixtures$chn_H, accuracy = 0.1)

#Draw
CHN_fixtures$chn_D <- (

  CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 + CHN_fixtures$chn_4_4 +
    CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6
)

CHN_fixtures$chn_D <- percent(CHN_fixtures$chn_D, accuracy = 0.1)

#Away

CHN_fixtures$chn_A <- (
  CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
    CHN_fixtures$chn_0_5 + CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6
)

CHN_fixtures$chn_A <- percent(CHN_fixtures$chn_A, accuracy = 0.1)

#ov25
CHN_fixtures$chn_ov25 <- (
  CHN_fixtures$chn_2_1 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 + CHN_fixtures$chn_2_3 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 + CHN_fixtures$chn_0_4 +
    CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 + CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_0 +
    CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 + CHN_fixtures$chn_0_5 +
    CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 + CHN_fixtures$chn_5_5 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5 + CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 +
    CHN_fixtures$chn_4_6 + CHN_fixtures$chn_5_6 + CHN_fixtures$chn_6_6
)
#un25
CHN_fixtures$chn_un25 <- (
  CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_0 + CHN_fixtures$chn_0_1 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_0_2
)
#odds
CHN_fixtures$chn_ov25_odds <- round((1/CHN_fixtures$chn_ov25),digits = 2)
CHN_fixtures$chn_un25_odds <- round((1/CHN_fixtures$chn_un25),digits = 2)

CHN_fixtures$chn_ov25_odds
CHN_fixtures$chn_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
CHN_fixtures$chn_BTTSY <- (
  CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_3_1 + CHN_fixtures$chn_3_2 +
    CHN_fixtures$chn_2_2 + CHN_fixtures$chn_1_3 + CHN_fixtures$chn_2_3 + CHN_fixtures$chn_3_3 + CHN_fixtures$chn_4_4 +
    CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_3 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 +
    CHN_fixtures$chn_3_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 +
    CHN_fixtures$chn_5_4 + CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_6_6 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6
)
#BTTSN
CHN_fixtures$chn_BTTSN <- (
  CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_0 + CHN_fixtures$chn_0_1 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_0_2 +
    CHN_fixtures$chn_3_0 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_5_0 +
    CHN_fixtures$chn_0_5 + CHN_fixtures$chn_6_0 + CHN_fixtures$chn_0_6
)

CHN_fixtures$chn_BTTSY_odds <- round((1/CHN_fixtures$chn_BTTSY),digits = 2)
CHN_fixtures$chn_BTTSN_odds <- round((1/CHN_fixtures$chn_BTTSN),digits = 2)

CHN_fixtures$chn_BTTSY <- percent(CHN_fixtures$chn_BTTSY, accuracy = 0.1)
CHN_fixtures$chn_BTTSN <- percent(CHN_fixtures$chn_BTTSN, accuracy = 0.1)
#odds
CHN_fixtures$chn_BTTSY_odds
CHN_fixtures$chn_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
CHN_fixtures$chn_AH_0_H <- (
  CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
    CHN_fixtures$chn_5_0 +CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5 + CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6
)
#AH_0_A
CHN_fixtures$chn_AH_0_A <- (
  CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
    CHN_fixtures$chn_0_5 +CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6 + CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6
)

#odds
CHN_fixtures$chn_AH_0_H_odds <- round((1/CHN_fixtures$chn_AH_0_H),digits = 2)
CHN_fixtures$chn_AH_0_A_odds <- round((1/CHN_fixtures$chn_AH_0_A),digits = 2)

CHN_fixtures$chn_AH_0_H_odds
CHN_fixtures$chn_AH_0_A_odds
#percentages
CHN_fixtures$chn_AH_0_H <- percent(CHN_fixtures$chn_AH_0_H, accuracy = 0.1)
CHN_fixtures$chn_AH_0_A <- percent(CHN_fixtures$chn_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
CHN_fixtures$chn_AH_n075_H <- (
  CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
    CHN_fixtures$chn_5_0 +CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5
)
#AH_n075_A
CHN_fixtures$chn_AH_n075_A <- (
  CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
    CHN_fixtures$chn_0_5 +CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6
)

#odds
CHN_fixtures$chn_AH_n075_H_odds <- round((1/CHN_fixtures$chn_AH_n075_H),digits = 2)
CHN_fixtures$chn_AH_n075_A_odds <- round((1/CHN_fixtures$chn_AH_n075_A),digits = 2)

CHN_fixtures$chn_AH_n075_H_odds
CHN_fixtures$chn_AH_n075_A_odds
#percentages
CHN_fixtures$chn_AH_n075_H <- percent(CHN_fixtures$chn_AH_n075_H, accuracy = 0.1)
CHN_fixtures$chn_AH_n075_A <- percent(CHN_fixtures$chn_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
CHN_fixtures$chn_AH_075_H <- (
  CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
    CHN_fixtures$chn_5_0 +CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5 + CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6 + CHN_fixtures$chn_0_1 + CHN_fixtures$chn_1_2 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_3_4 + CHN_fixtures$chn_4_5 + CHN_fixtures$chn_5_6
)
#AH_075_A
CHN_fixtures$chn_AH_075_A <- (
  CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
    CHN_fixtures$chn_0_5 +CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6 + CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6 + CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_3 + CHN_fixtures$chn_5_4 + CHN_fixtures$chn_6_5
)

#odds
CHN_fixtures$chn_AH_075_H_odds <- round((1/CHN_fixtures$chn_AH_075_H),digits = 2)
CHN_fixtures$chn_AH_075_A_odds <- round((1/CHN_fixtures$chn_AH_075_A),digits = 2)

CHN_fixtures$chn_AH_075_H_odds
CHN_fixtures$chn_AH_075_A_odds
#percentages
CHN_fixtures$chn_AH_075_H <- percent(CHN_fixtures$chn_AH_075_H, accuracy = 0.1)
CHN_fixtures$chn_AH_075_A <- percent(CHN_fixtures$chn_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
CHN_fixtures$chn_AH_n125_H <- (
  CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
    CHN_fixtures$chn_5_0 +CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5
)
#AH_n125_A
CHN_fixtures$chn_AH_n125_A <- (
  CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
    CHN_fixtures$chn_0_5 +CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6
)

#odds
CHN_fixtures$chn_AH_n125_H_odds <- round((1/CHN_fixtures$chn_AH_n125_H),digits = 2)
CHN_fixtures$chn_AH_n125_A_odds <- round((1/CHN_fixtures$chn_AH_n125_A),digits = 2)

CHN_fixtures$chn_AH_n125_H_odds
CHN_fixtures$chn_AH_n125_A_odds
#percentages
CHN_fixtures$chn_AH_n125_H <- percent(CHN_fixtures$chn_AH_n125_H, accuracy = 0.1)
CHN_fixtures$chn_AH_n125_A <- percent(CHN_fixtures$chn_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
CHN_fixtures$chn_AH_125_H <- (
  CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_0 + CHN_fixtures$chn_2_1 + CHN_fixtures$chn_3_0 + CHN_fixtures$chn_3_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_0 + CHN_fixtures$chn_4_1 + CHN_fixtures$chn_4_2 + CHN_fixtures$chn_4_3 +
    CHN_fixtures$chn_5_0 +CHN_fixtures$chn_5_1 + CHN_fixtures$chn_5_2 + CHN_fixtures$chn_5_3 + CHN_fixtures$chn_5_4 +
    CHN_fixtures$chn_6_0 + CHN_fixtures$chn_6_1 + CHN_fixtures$chn_6_2 + CHN_fixtures$chn_6_3 + CHN_fixtures$chn_6_4 +
    CHN_fixtures$chn_6_5 + CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6 + CHN_fixtures$chn_0_1 + CHN_fixtures$chn_1_2 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_3_4 + CHN_fixtures$chn_4_5 + CHN_fixtures$chn_5_6
)
#AH_125_A
CHN_fixtures$chn_AH_125_A <- (
  CHN_fixtures$chn_0_1 + CHN_fixtures$chn_0_2 + CHN_fixtures$chn_1_2 + CHN_fixtures$chn_0_3 + CHN_fixtures$chn_1_3 +
    CHN_fixtures$chn_2_3 + CHN_fixtures$chn_0_4 + CHN_fixtures$chn_1_4 + CHN_fixtures$chn_2_4 + CHN_fixtures$chn_3_4 +
    CHN_fixtures$chn_0_5 +CHN_fixtures$chn_1_5 + CHN_fixtures$chn_2_5 + CHN_fixtures$chn_3_5 + CHN_fixtures$chn_4_5 +
    CHN_fixtures$chn_0_6 + CHN_fixtures$chn_1_6 + CHN_fixtures$chn_2_6 + CHN_fixtures$chn_3_6 + CHN_fixtures$chn_4_6 +
    CHN_fixtures$chn_5_6 + CHN_fixtures$chn_0_0 + CHN_fixtures$chn_1_1 + CHN_fixtures$chn_2_2 + CHN_fixtures$chn_3_3 +
    CHN_fixtures$chn_4_4 + CHN_fixtures$chn_5_5 + CHN_fixtures$chn_6_6 + CHN_fixtures$chn_1_0 + CHN_fixtures$chn_2_1 +
    CHN_fixtures$chn_3_2 + CHN_fixtures$chn_4_3 + CHN_fixtures$chn_5_4 + CHN_fixtures$chn_6_5
)

#odds
CHN_fixtures$chn_AH_125_H_odds <- round((1/CHN_fixtures$chn_AH_125_H),digits = 2)
CHN_fixtures$chn_AH_125_A_odds <- round((1/CHN_fixtures$chn_AH_125_A),digits = 2)

CHN_fixtures$chn_AH_125_H_odds
CHN_fixtures$chn_AH_125_A_odds
#percentages
CHN_fixtures$chn_AH_125_H <- percent(CHN_fixtures$chn_AH_125_H, accuracy = 0.1)
CHN_fixtures$chn_AH_125_A <- percent(CHN_fixtures$chn_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
CHN_fixtures$chn_ov25 <- percent(CHN_fixtures$chn_ov25, accuracy = 0.1)

CHN_fixtures$chn_un25 <- percent(CHN_fixtures$chn_un25, accuracy = 0.1)
CHN_fixtures$chn_pscore <- paste(round(CHN_fixtures$chn_xGH,digits = 0),round(CHN_fixtures$chn_xGA,digits = 0),sep = "-")
#write out
write.xlsx(CHN_fixtures,'NL/CHN.xlsx',sheetName = "CHN", append = TRUE)
###########################################################################################################
########################CHN END###########################################################################
CHN <- read.csv('../FDAS/CHN.csv')
CHN$TG <- CHN$HG + CHN$AG
CHN$OV25 <- ifelse(CHN$TG >= 3,"Y","N")
chn_ftr_summary <- tabyl(CHN,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
chn_ov25_summary <- tabyl(CHN,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(chn_ftr_summary,'NL/CHN.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(chn_ov25_summary,'NL/CHN.xlsx',sheetName = "OVUN25", append = TRUE)
CHN <- subset(CHN,Season == "2022")


