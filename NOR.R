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
unlink('NL/NOR.xlsx')
######################NOR START#######################################
#####################################################################
NOR <- read.csv('../FDAS/NOR.csv')
#NOR <- NOR[!NOR$Away == "Jerv",]
NOR <- within(NOR,rm(Res))
NOR$Date <- dmy(NOR$Date)
NOR <- NOR[order(as.Date(NOR$Date, format = "%d/%m%Y"), decreasing = FALSE),]
NOR$CS <- paste(NOR$HG,NOR$AG, sep = "-")
#NOR_qualificaton <- subset(NOR,tournament == "UEFA Euro qualification")
NOR <- subset(NOR,Season == "2023")
#NOR <- NOR[NOR$Date > '2008-01-01',])
NOR$TG <- NOR$HG + NOR$AG
NOR$OV25 <- ifelse(NOR$TG >= 3,"Y","N")
NOR$FTR <- with(NOR,
                ifelse(HG > AG ,FTR <- "H" , ifelse(AG > HG,FTR <- "A", FTR <- "D"))
)
##########################################################################################
#########################################################################################
nor_totalgoalsv2 <- tapply(NOR$TG, NOR[c("Home", "Away")],mean)
nor_totalgoalsv2
nor_hgtotals <- rowSums(nor_totalgoalsv2,na.rm = T)
nor_agtotals <- colSums(nor_totalgoalsv2,na.rm = T)

nor_totalgoals <- nor_hgtotals + nor_agtotals
nor_totalgoalsv2 <- cbind(nor_totalgoalsv2,nor_totalgoals)
nor_teams <- sort(unique(NOR$Home))
nor_home_games <- c()
nor_away_games <-c()
for (i_nor in 1:length(nor_teams))
{

  nor_home_games[i_nor] <- nrow(NOR[NOR$Home == nor_teams[i_nor],])
  nor_away_games[i_nor]  <- nrow(NOR[NOR$Away == nor_teams[i_nor],])

}
nor_games_played <- nor_home_games + nor_away_games
nor_goaltotalsv2 <- cbind(nor_totalgoalsv2,nor_games_played)
nor_avg_totalgoals <- round((nor_totalgoals/ nor_games_played), digits = 4)
nor_goaltotalsv2[is.na(nor_goaltotalsv2)] <- ""
nor_goaltotalsv2 <- cbind(nor_goaltotalsv2,nor_avg_totalgoals)
write.xlsx(nor_goaltotalsv2,'NL/NOR.xlsx',sheetName = "totalgoalsv2")
#####################################################################
#####################################################################################################
##############################################################################################
NOR <- subset(NOR,Season == "2023")
nor_totalrounds <-  (length(nor_teams) - 1 )*2
nor_totalmatches <- (length(nor_teams)*(length(nor_teams) - 1))
nor_eachround <- nor_totalmatches / nor_totalrounds

nor_matchesplayed <-  nrow(NOR)

NOR_rounds <- NOR

if(nor_matchesplayed %% nor_eachround == 0)
{
  nor_currentround <- nor_matchesplayed / nor_eachround
  nor_matchday <- c()
  nor_matchday <- rep(1:nor_currentround, each = nor_eachround)
}else if(nor_matchesplayed %% nor_eachround != 0)

{

  nor_modulus <- nor_matchesplayed %% nor_eachround
  nor_currentround <- (nor_matchesplayed - nor_modulus) / nor_eachround
  nor_matchday <- c()
  nor_matchday_vec1 <- c()
  nor_matchday_vec2 <- c()
  nor_matchday_vec1 <- rep(1:nor_currentround, each = nor_eachround)
  nor_matchday_vec2[1:nor_modulus] <- c(nor_currentround + 1)
  nor_matchday <- append(nor_matchday_vec1,nor_matchday_vec2)
}
NOR_rounds <- cbind(NOR_rounds,nor_matchday)
#####################################################################################################
##############################################################################################
#create home and away matrices
nor_goalscored_h <- tapply(NOR$HG, NOR[c("Home", "Date")],mean)
nor_goalscored_a <- tapply(NOR$AG, NOR[c("Away", "Date")],mean)
nor_goalscored_h[is.na(nor_goalscored_h)] <- ""
nor_goalscored_a[is.na(nor_goalscored_a)] <- ""

for(nor_rowhgs in 1:nrow(nor_goalscored_h)) {
  for(nor_colhgs in 1:ncol(nor_goalscored_h)) {

    # print(my_matrix[row, col])
    for(nor_rowags in 1:nrow(nor_goalscored_a)) {
      for(nor_colags in 1:ncol(nor_goalscored_a)) {
        ifelse(!nor_goalscored_a[nor_rowags,nor_colags]=="",nor_goalscored_h[nor_rowags,nor_colags] <- nor_goalscored_a[nor_rowags,nor_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(nor_goalscoredmatrix,'NL/NOR.xlsx',sheetName = "gsmatrix", append = TRUE)
#########################################################################################
#########################################################################################
#nor goal scored rounds
final_nor_gs <- matrix(nrow = length(nor_teams),ncol = nor_totalrounds )
suml6_nor_gs <- c()
sum_nor_zero_gs <- c()
sum_nor_one_gs <- c()
sum_nor_two_gs <- c()
sum_nor_three_gs <- c()
l6_form_nor_gssplitted <- c()
form_nor_gs <- c()
for(index_nor_gs in 1:length(nor_teams))
{
  for(index_nor_gs_cols in 1:nor_totalrounds)
  {
    index_nor_gs  <- row.names(nor_goalscored_h) == nor_teams[index_nor_gs]
    form_nor_gs <- nor_goalscored_h[index_nor_gs ]
    deleted_form_nor_gs <- form_nor_gs[!form_nor_gs[] == ""]
    l6_form_nor_gs <- tail(deleted_form_nor_gs,nor_last_n_games)
    l6_form_nor_gs <- as.numeric(l6_form_nor_gs)
    suml6_nor_gs[index_nor_gs] <- sum(l6_form_nor_gs)
    suml6_nor_gs[index_nor_gs] <- paste(suml6_nor_gs[index_nor_gs],sep = "")
    sum_nor_zero_gs[index_nor_gs] <- length(which(l6_form_nor_gs == 0))
    sum_nor_zero_gs[index_nor_gs] <- paste(sum_nor_zero_gs[index_nor_gs],sep = "")
    sum_nor_one_gs[index_nor_gs] <- length(which(l6_form_nor_gs == 1))
    sum_nor_one_gs[index_nor_gs] <- paste(sum_nor_one_gs[index_nor_gs],sep = "")
    sum_nor_two_gs[index_nor_gs] <- length(which(l6_form_nor_gs >= 2))
    sum_nor_two_gs[index_nor_gs] <- paste(sum_nor_two_gs[index_nor_gs],sep = "")
    sum_nor_three_gs[index_nor_gs] <- length(which(l6_form_nor_gs >= 3))
    sum_nor_three_gs[index_nor_gs] <- paste(sum_nor_three_gs[index_nor_gs],sep = "")
    l6_form_nor_gs <- as.character(l6_form_nor_gs)
    l6_form_nor_gs_flattened <- stri_paste(l6_form_nor_gs,collapse = '')
    l6_form_nor_gssplitted <- as.numeric(strsplit(as.character(l6_form_nor_gs_flattened),"")[[1]])
    final_nor_gs[index_nor_gs,index_nor_gs_cols] <- l6_form_nor_gssplitted[index_nor_gs_cols]
  }
}

final_nor_gs[is.na(final_nor_gs)] <- ""
nor_goalscoredmatrix <- cbind(nor_teams,final_nor_gs,suml6_nor_gs,sum_nor_zero_gs,sum_nor_one_gs,sum_nor_two_gs,sum_nor_three_gs)
write.xlsx(nor_goalscoredmatrix,'NL/NOR.xlsx',sheetName = "gsmatrix", append = TRUE)
#################################################################################################################################

####GCmatrix#####################################################################################################################
#create home and away matrices
nor_goalconceded_h <- tapply(NOR$AG, NOR[c("Home", "Date")],mean)
nor_goalconceded_a <- tapply(NOR$HG, NOR[c("Away", "Date")],mean)
nor_goalconceded_h[is.na(nor_goalconceded_h)] <- ""
nor_goalconceded_a[is.na(nor_goalconceded_a)] <- ""

for(nor_rowhgc in 1:nrow(nor_goalconceded_h)) {
  for(nor_colhgc in 1:ncol(nor_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(nor_rowagc in 1:nrow(nor_goalconceded_a)) {
      for(nor_colagc in 1:ncol(nor_goalconceded_a)) {
        ifelse(!nor_goalconceded_a[nor_rowagc,nor_colagc]=="",nor_goalconceded_h[nor_rowagc,nor_colagc] <- nor_goalconceded_a[nor_rowagc,nor_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#write.xlsx(nor_goalconcededmatrix,'NL/NOR.xlsx',sheetName = "gcmatrix", append = TRUE)
############################################################################################################################################################
#nor goal conceded rounds
final_nor_gc <- matrix(nrow = length(nor_teams),ncol = nor_totalrounds )
suml6_nor_gc <- c()
sum_nor_zero_gc <- c()
sum_nor_one_gc <- c()
sum_nor_two_gc <- c()
sum_nor_three_gc <- c()
l6_form_nor_gcsplitted <- c()
form_nor_gc <- c()
for(index_nor_gc in 1:length(nor_teams))
{
  for(index_nor_gc_cols in 1:nor_totalrounds)
  {
    index_nor_gc  <- row.names(nor_goalconceded_h) == nor_teams[index_nor_gc]
    form_nor_gc <- nor_goalconceded_h[index_nor_gc ]
    deleted_form_nor_gc <- form_nor_gc[!form_nor_gc[] == ""]
    l6_form_nor_gc <- tail(deleted_form_nor_gc,nor_last_n_games)
    l6_form_nor_gc <- as.numeric(l6_form_nor_gc)
    suml6_nor_gc[index_nor_gc] <- sum(l6_form_nor_gc)
    suml6_nor_gc[index_nor_gc] <- paste(suml6_nor_gc[index_nor_gc],sep = "")
    sum_nor_zero_gc[index_nor_gc] <- length(which(l6_form_nor_gc == 0))
    sum_nor_zero_gc[index_nor_gc] <- paste(sum_nor_zero_gc[index_nor_gc],sep = "")
    sum_nor_one_gc[index_nor_gc] <- length(which(l6_form_nor_gc == 1))
    sum_nor_one_gc[index_nor_gc] <- paste(sum_nor_one_gc[index_nor_gc],sep = "")
    sum_nor_two_gc[index_nor_gc] <- length(which(l6_form_nor_gc >= 2))
    sum_nor_two_gc[index_nor_gc] <- paste(sum_nor_two_gc[index_nor_gc],sep = "")
    sum_nor_three_gc[index_nor_gc] <- length(which(l6_form_nor_gc >= 3))
    sum_nor_three_gc[index_nor_gc] <- paste(sum_nor_three_gc[index_nor_gc],sep = "")
    l6_form_nor_gc <- as.character(l6_form_nor_gc)
    l6_form_nor_gc_flattened <- stri_paste(l6_form_nor_gc,collapse = '')
    l6_form_nor_gcsplitted <- as.numeric(strsplit(as.character(l6_form_nor_gc_flattened),"")[[1]])
    final_nor_gc[index_nor_gc,index_nor_gc_cols] <- l6_form_nor_gcsplitted[index_nor_gc_cols]
  }
}

final_nor_gc[is.na(final_nor_gc)] <- ""
nor_goalconcededmatrix <- cbind(nor_teams,final_nor_gc,suml6_nor_gc,sum_nor_zero_gc,sum_nor_one_gc,sum_nor_two_gc,sum_nor_three_gc)
write.xlsx(nor_goalconcededmatrix,'NL/NOR.xlsx',sheetName = "gcmatrix2", append = TRUE)
###################################################################################################################################

###################################################################################################################################
####Teamform#######################################################################################################################

nor_form_h <- tapply(NOR$FTR, NOR[c("Home", "Date")],median)
nor_form_a <- tapply(NOR$FTR, NOR[c("Away", "Date")],median)
nor_form_h[is.na(nor_form_h)] <- ""
nor_form_a[is.na(nor_form_a)] <- ""
nor_form_h <- sub("A","L",nor_form_h)
nor_form_h <- sub("H","W",nor_form_h)
nor_form_a <- sub("A","W",nor_form_a)
nor_form_a <- sub("H","L",nor_form_a)
for(nor_rowh_f in 1:nrow(nor_form_h)) {
  for(nor_colh_f in 1:ncol(nor_form_h)) {

    # print(my_matrix[row, col])
    for(nor_rowa_f in 1:nrow(nor_form_a)) {
      for(nor_cola_f in 1:ncol(nor_form_a)) {
        ifelse(!nor_form_a[nor_rowa_f,nor_cola_f]=="",nor_form_h[nor_rowa_f,nor_cola_f] <- nor_form_a[nor_rowa_f,nor_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#nor team form
final_nor_hf <- matrix(nrow = length(nor_teams),ncol = nor_totalrounds )
suml6_nor_hf <- c()
l6_form_nor_hfsplitted <- c()
form_nor_hf <- c()
for(index_nor_hf in 1:length(nor_teams))
{
  for(index_nor_hf_cols in 1:nor_totalrounds)
  {
    index_nor_hf  <- row.names(nor_form_h) == nor_teams[index_nor_hf]
    form_nor_hf <- nor_form_h[index_nor_hf ]
    deleted_form_nor_hf <- form_nor_hf[!form_nor_hf[] == ""]
    l6_form_nor_hf <- tail(deleted_form_nor_hf,nor_last_n_games)
    # #l6_form_nor_hf <- as.numeric(l6_form_nor_hf)
    # suml6_nor_hf[index_nor_hf] <- sum(l6_form_nor_hf)
    # suml6_nor_hf[index_nor_hf] <- paste(suml6_nor_hf[index_nor_hf],sep = "")
    #l6_form_nor_hf <- as.character(l6_form_nor_hf)
    l6_form_nor_hf_flattened <- stri_paste(l6_form_nor_hf,collapse = '')
    l6_form_nor_hfsplitted <- (strsplit(as.character(l6_form_nor_hf_flattened),"")[[1]])
    final_nor_hf[index_nor_hf,index_nor_hf_cols] <- l6_form_nor_hfsplitted[index_nor_hf_cols]
  }
}
final_nor_hf[is.na(final_nor_hf)] <- ""
nor_formmatrix <- cbind(nor_teams,final_nor_hf)

write.xlsx(nor_formmatrix,'NL/NOR.xlsx',sheetName = "form", append = TRUE)
######################################################################################################################################
######################################################################################################################################

#######TGMatrix#######################################################################################################################
nor_totalgoals_h <- tapply(NOR$TG, NOR[c("Home", "Date")],mean)
nor_totalgoals_a <- tapply(NOR$TG, NOR[c("Away", "Date")],mean)
nor_totalgoals_h[is.na(nor_totalgoals_h)] <- ""
nor_totalgoals_a[is.na(nor_totalgoals_a)] <- ""
for(nor_rowh in 1:nrow(nor_totalgoals_h)) {
  for(nor_colh in 1:ncol(nor_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(nor_rowa in 1:nrow(nor_totalgoals_a)) {
      for(nor_cola in 1:ncol(nor_totalgoals_a)) {
        ifelse(!nor_totalgoals_a[nor_rowa,nor_cola]=="",nor_totalgoals_h[nor_rowa,nor_cola] <- nor_totalgoals_a[nor_rowa,nor_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#nor total goals rounds
#nor
final_nor_tg <- matrix(nrow = length(nor_teams),ncol = nor_totalrounds )
suml6_nor_tg <- c()
l6_form_nor_tgsplitted <- c()
form_nor_tg <- c()
for(index_nor_tg in 1:length(nor_teams))
{
  for(index_nor_tg_cols in 1:nor_totalrounds)
  {
    index_nor_tg  <- row.names(nor_totalgoals_h) == nor_teams[index_nor_tg]
    form_nor_tg <- nor_totalgoals_h[index_nor_tg ]
    deleted_form_nor_tg <- form_nor_tg[!form_nor_tg[] == ""]
    l6_form_nor_tg <- tail(deleted_form_nor_tg,nor_last_n_games)
    l6_form_nor_tg <- as.numeric(l6_form_nor_tg)
    suml6_nor_tg[index_nor_tg] <- sum(l6_form_nor_tg)
    suml6_nor_tg[index_nor_tg] <- paste(suml6_nor_tg[index_nor_tg],sep = "")
    l6_form_nor_tg <- as.character(l6_form_nor_tg)
    l6_form_nor_tg_flattened <- stri_paste(l6_form_nor_tg,collapse = '')
    l6_form_nor_tgsplitted <- as.numeric(strsplit(as.character(l6_form_nor_tg_flattened),"")[[1]])
    final_nor_tg[index_nor_tg,index_nor_tg_cols] <- l6_form_nor_tgsplitted[index_nor_tg_cols]
  }
}

final_nor_tg[is.na(final_nor_tg)] <- ""
nor_goaltotalmatrix <- cbind(nor_teams,final_nor_tg,suml6_nor_tg)

write.xlsx(nor_goaltotalmatrix,'NL/NOR.xlsx',sheetName = "tgmatrix", append = TRUE)
#############################################################################################################################################
#######TeamAgainst###########################################################################################################################
nor_form_team_against_h <- tapply(NOR$Away, NOR[c("Home", "Date")],median)
nor_form_team_against_a <- tapply(NOR$Home, NOR[c("Away", "Date")],median)
nor_form_team_against_h[is.na(nor_form_team_against_h)] <- ""
nor_form_team_against_a[is.na(nor_form_team_against_a)] <- ""
for(nor_rowh_f_against in 1:nrow(nor_form_team_against_h)) {
  for(nor_colh_f_against in 1:ncol(nor_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(nor_rowa_f_against in 1:nrow(nor_form_team_against_a)) {
      for(nor_cola_f_against in 1:ncol(nor_form_team_against_a)) {
        ifelse(!nor_form_team_against_a[nor_rowa_f_against,nor_cola_f_against]=="",nor_form_team_against_h[nor_rowa_f_against,nor_cola_f_against] <- nor_form_team_against_a[nor_rowa_f_against,nor_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#win margin
nor_winmargin_h <- tapply(NOR$HG - NOR$AG, NOR[c("Home", "Date")],mean)
nor_winmargin_a <- tapply(NOR$AG - NOR$HG, NOR[c("Away", "Date")],mean)
nor_winmargin_h[is.na(nor_winmargin_h)] <- ""
#
for(nor_rowhwm in 1:nrow(nor_winmargin_h)) {
  for(nor_colhwm in 1:ncol(nor_winmargin_h)) {

    # print(my_matrix[row, col])
    for(nor_rowawm in 1:nrow(nor_winmargin_a)) {
      for(nor_colawm in 1:ncol(nor_winmargin_a)) {
        ifelse(!nor_winmargin_a[nor_rowawm,nor_colawm]=="",nor_winmargin_h[nor_rowawm,nor_colawm] <- nor_winmargin_a[nor_rowawm,nor_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
final_nor_wm <- matrix(nrow = length(nor_teams),ncol = nor_totalrounds )
suml6_nor_wm <- c()
suml6_nor_wm_negone <- c()
suml6_nor_wm_negtwo <- c()
suml6_nor_wm_zero <- c()
suml6_nor_wm_posone <- c()
suml6_nor_wm_postwo <- c()
l6_form_nor_wmsplitted <- c()
form_nor_wm <- c()
for(index_nor_wm in 1:length(nor_teams))
{
  for(index_nor_wm_cols in 1:nor_totalrounds)
  {
    index_nor_wm  <- row.names(nor_winmargin_h) == nor_teams[index_nor_wm]
    form_nor_wm <- nor_winmargin_h[index_nor_wm ]
    deleted_form_nor_wm <- form_nor_wm[!form_nor_wm[] == ""]
    l6_form_nor_wm <- tail(deleted_form_nor_wm,nor_last_n_games)
    l6_form_nor_wm <- as.numeric(l6_form_nor_wm)
    suml6_nor_wm[index_nor_wm] <- sum(l6_form_nor_wm)
    suml6_nor_wm[index_nor_wm] <- paste(suml6_nor_wm[index_nor_wm],sep = "")
    suml6_nor_wm_negone[index_nor_wm] <- length(which(l6_form_nor_wm == -1))
    suml6_nor_wm_negone[index_nor_wm] <- paste(suml6_nor_wm_negone[index_nor_wm],sep = "")
    suml6_nor_wm_negtwo[index_nor_wm] <- length(which(l6_form_nor_wm <= -2))
    suml6_nor_wm_negtwo[index_nor_wm] <- paste(suml6_nor_wm_negtwo[index_nor_wm],sep = "")
    suml6_nor_wm_zero[index_nor_wm] <- length(which(l6_form_nor_wm == 0))
    suml6_nor_wm_zero[index_nor_wm] <- paste(suml6_nor_wm_zero[index_nor_wm],sep = "")
    suml6_nor_wm_posone[index_nor_wm] <- length(which(l6_form_nor_wm == 1))
    suml6_nor_wm_posone[index_nor_wm] <- paste(suml6_nor_wm_posone[index_nor_wm],sep = "")
    suml6_nor_wm_postwo[index_nor_wm] <- length(which(l6_form_nor_wm == 2))
    suml6_nor_wm_postwo[index_nor_wm] <- paste(suml6_nor_wm_postwo[index_nor_wm],sep = "")
    l6_form_nor_wm <- as.character(l6_form_nor_wm)
    l6_form_nor_wm_flattened <- stri_paste(l6_form_nor_wm,collapse = ',')
    l6_form_nor_wmsplitted <- (strsplit(as.character(l6_form_nor_wm_flattened),",")[[1]])
    final_nor_wm[index_nor_wm,index_nor_wm_cols] <- l6_form_nor_wmsplitted[index_nor_wm_cols]
  }
}

final_nor_wm[is.na(final_nor_wm)] <- ""
nor_winmarginmatrix <- cbind(nor_teams,final_nor_wm,suml6_nor_wm,suml6_nor_wm_negtwo,suml6_nor_wm_negone,suml6_nor_wm_zero,suml6_nor_wm_posone,suml6_nor_wm_postwo)
write.xlsx(nor_winmarginmatrix,'NL/NOR.xlsx',sheetName = "winmargin", append = TRUE)
####################################################################################################################
##########Goals over under############
#NOR
nor_un05_home <- c()
nor_un05_away <- c()
nor_ov05_home <- c()
nor_ov05_away <- c()

nor_un15_home <- c()
nor_un15_away <- c()
nor_ov15_home <- c()
nor_ov15_away <- c()

nor_un25_home <- c()
nor_un25_away <- c()
nor_ov25_home <- c()
nor_ov25_away <- c()

nor_un35_home <- c()
nor_un35_away <- c()
nor_ov35_home <- c()
nor_ov35_away <- c()

nor_un45_home <- c()
nor_un45_away <- c()
nor_ov45_home <- c()
nor_ov45_away <- c()

nor_un55_home <- c()
nor_un55_away <- c()
nor_ov55_home <- c()
nor_ov55_away <- c()

for (i_nor_tg in 1:length(nor_teams))
{

  nor_un05_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG == 0,])
  nor_un05_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG == 0,])

  nor_ov05_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG > 0,])
  nor_ov05_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG > 0,])

  nor_un15_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG <= 1,])
  nor_un15_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG <= 1,])

  nor_ov15_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG >= 2,])
  nor_ov15_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG >= 2,])

  nor_un25_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG <= 2,])
  nor_un25_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG <= 2,])

  nor_ov25_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG >=3,])
  nor_ov25_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG >=3,])

  nor_un35_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG <= 3,])
  nor_un35_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG <= 3,])

  nor_ov35_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG >= 4,])
  nor_ov35_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG >= 4,])

  nor_un45_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG <= 4,])
  nor_un45_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG <= 4,])

  nor_ov45_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG >= 5,])
  nor_ov45_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG >= 5,])

  nor_un55_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG <= 5,])
  nor_un55_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG <= 5,])

  nor_ov55_home[i_nor_tg] <- nrow(NOR[NOR$Home == nor_teams[i_nor_tg] & NOR$TG >= 6,])
  nor_ov55_away[i_nor_tg] <- nrow(NOR[NOR$Away == nor_teams[i_nor_tg] & NOR$TG >= 6,])


}

nor_un05 <- nor_un05_home + nor_un05_away
nor_ov05 <- nor_ov05_home + nor_ov05_away

nor_un15 <- nor_un15_home + nor_un15_away
nor_ov15 <- nor_ov15_home + nor_ov15_away

nor_un25 <- nor_un25_home + nor_un25_away
nor_ov25 <- nor_ov25_home + nor_ov25_away

nor_un35 <- nor_un35_home + nor_un35_away
nor_ov35 <- nor_ov35_home + nor_ov35_away

nor_un45 <- nor_un45_home + nor_un45_away
nor_ov45 <- nor_ov45_home + nor_ov45_away

nor_un55 <- nor_un55_home + nor_un55_away
nor_ov55 <- nor_ov55_home + nor_ov55_away

nor_ovundata <- cbind(nor_teams,nor_un05,nor_ov05,nor_un15,nor_ov15,nor_un25,nor_ov25,nor_un35,nor_ov35,nor_un45,nor_ov45,nor_un55,nor_ov55)
write.xlsx(nor_ovundata,'NL/NOR.xlsx',sheetName = "OVUN", append = TRUE)
###############################################################################################################################

##########################################################################################
#csform
nor_csform_h <- tapply(NOR$CS, NOR[c("Home", "Date")],median)
nor_csform_a <- tapply(NOR$CS, NOR[c("Away", "Date")],median)

nor_csform_h[is.na(nor_csform_h)] <- ""
nor_csform_a[is.na(nor_csform_a)] <- ""

for(nor_rowh_f_cs in 1:nrow(nor_csform_h)) {
  for(nor_colh_f_cs in 1:ncol(nor_csform_h)) {

    # print(my_matrix[row, col])
    for(nor_rowa_f_cs in 1:nrow(nor_csform_a)) {
      for(nor_cola_f_cs in 1:ncol(nor_csform_a)) {
        ifelse(!nor_csform_a[nor_rowa_f_cs,nor_cola_f_cs]=="",nor_csform_h[nor_rowa_f_cs,nor_cola_f_cs] <- nor_csform_a[nor_rowa_f_cs,nor_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##########################################################################################
###########################################################################################
############Scoring and conceding analysis
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

######################################################################################
###########League Table###############################################################

#hwins and away wins
nor_home_wins <- c()
nor_away_wins <- c()
nor_home_draws <- c()
nor_away_draws <- c()
nor_home_loss <- c()
nor_away_loss <- c()



for (i_nor_wins in 1:length(nor_teams))
{

  nor_home_wins[i_nor_wins] <- nrow(NOR[NOR$Home == nor_teams[i_nor_wins] & NOR$FTR == "H",])
  nor_away_wins[i_nor_wins] <- nrow(NOR[NOR$Away == nor_teams[i_nor_wins] & NOR$FTR == "A",])
  nor_home_draws[i_nor_wins] <- nrow(NOR[NOR$Home == nor_teams[i_nor_wins] & NOR$FTR == "D",])
  nor_away_draws[i_nor_wins] <- nrow(NOR[NOR$Away == nor_teams[i_nor_wins] & NOR$FTR == "D",])
  nor_home_loss[i_nor_wins] <- nrow(NOR[NOR$Home == nor_teams[i_nor_wins] & NOR$FTR == "A",])
  nor_away_loss[i_nor_wins] <- nrow(NOR[NOR$Away == nor_teams[i_nor_wins] & NOR$FTR == "H",])

}

nor_total_wins <- nor_home_wins + nor_away_wins
nor_total_draws <- nor_home_draws + nor_away_draws
nor_total_loss <- nor_home_loss + nor_away_loss

nor_league_table <- cbind(nor_teams,nor_games_played,nor_total_wins,nor_total_draws,nor_total_loss)
nor_GS <- nor_scoring$TGS
nor_GC <-nor_conceding$TGC
nor_GD <- nor_scoring$TGS - nor_conceding$TGC
nor_PTS <- (nor_total_wins*3) + (nor_total_draws*1)
nor_league_table <- cbind(nor_league_table,nor_GS,nor_GC,nor_GD,nor_PTS)
nor_league_table <- as.data.frame(nor_league_table)
#rename the columns
names(nor_league_table)[names(nor_league_table) == "nor_teams"] <- "Team"
names(nor_league_table)[names(nor_league_table) == "nor_games_played"] <- "P"
names(nor_league_table)[names(nor_league_table) == "nor_total_wins"] <- "W"
names(nor_league_table)[names(nor_league_table) == "nor_total_draws"] <- "D"
names(nor_league_table)[names(nor_league_table) == "nor_total_loss"] <- "L"
names(nor_league_table)[names(nor_league_table) == "nor_GS"] <- "F"
names(nor_league_table)[names(nor_league_table) == "nor_GC"] <- "A"
points_nor <- nor_league_table[order(as.numeric(nor_league_table$nor_PTS), decreasing = TRUE),]
points_nor$nor_rank <- 1:length(nor_teams)
row.names(points_nor) <- points_nor$nor_rank
#create final_nor_hf_against with team ranks in brackets
for(nor_rowhrank in 1:nrow(nor_form_team_against_h)) {
  for(nor_colhrank in 1:ncol(nor_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!nor_form_team_against_h[nor_rowhrank,nor_colhrank]=="",nor_form_team_against_h[nor_rowhrank,nor_colhrank] <- paste(nor_form_team_against_h[nor_rowhrank,nor_colhrank],"(",points_nor$nor_rank[points_nor$Team ==nor_form_team_against_h[nor_rowhrank,nor_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}
write.xlsx(points_nor,'NL/NOR.xlsx',sheetName = "table", append = TRUE)
##########################################################################################################
#########################################last six nor###################################################
#NOR
#form
#create final_nor_hf object
#nor_last_n_games <- 6
final_nor_hf <- c()
for(index_nor_hf in 1:length(nor_teams))
{
  index_nor_hf <- row.names(nor_form_h) == nor_teams[index_nor_hf]
  form_nor_hf <- nor_form_h[index_nor_hf]
  deleted_form_nor_hf <- form_nor_hf[!form_nor_hf[] == ""]
  l6_form_nor_hf <- tail(deleted_form_nor_hf,nor_last_n_games)
  l6_form_nor_hf <- paste(l6_form_nor_hf,collapse = " ")
  final_nor_hf[index_nor_hf] <- rbind(paste(nor_teams[index_nor_hf],l6_form_nor_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}

#change column names
final_nor_hf <- as.data.frame(final_nor_hf)
colnames(final_nor_hf) <- "Form"
#goals scored
#create final_nor_gs object
final_nor_gs <- c()
suml6_nor_gs <- c()
for(index_nor_gs in 1:length(nor_teams))
{
  index_nor_gs <- row.names(nor_goalscored_h) == nor_teams[index_nor_gs]
  form_nor_gs <- nor_goalscored_h[index_nor_gs]
  deleted_form_nor_gs <- form_nor_gs[!form_nor_gs[] == ""]
  l6_form_nor_gs <- tail(deleted_form_nor_gs,nor_last_n_games)
  l6_form_nor_gs <- as.numeric(l6_form_nor_gs)
  suml6_nor_gs[index_nor_gs] <- sum(l6_form_nor_gs)
  suml6_nor_gs[index_nor_gs] <- paste("(",suml6_nor_gs[index_nor_gs],")",sep = "")
  l6_form_nor_gs <- paste(l6_form_nor_gs,collapse = " ")
  final_nor_gs[index_nor_gs] <- rbind(paste(nor_teams[index_nor_gs],l6_form_nor_gs,suml6_nor_gs[index_nor_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}
final_nor_gs
#change column names
final_nor_gs <- as.data.frame(final_nor_gs)
colnames(final_nor_gs) <- "Goals scored"
#goal conceded
#create final_nor_gc object
final_nor_gc <- c()
suml6_nor_gc <- c()
for(index_nor_gc in 1:length(nor_teams))
{
  index_nor_gc <- row.names(nor_goalconceded_h) == nor_teams[index_nor_gc]
  form_nor_gc <- nor_goalconceded_h[index_nor_gc]
  deleted_form_nor_gc <- form_nor_gc[!form_nor_gc[] == ""]
  l6_form_nor_gc <- tail(deleted_form_nor_gc,nor_last_n_games)
  l6_form_nor_gc <- as.numeric(l6_form_nor_gc)
  suml6_nor_gc[index_nor_gc] <- sum(l6_form_nor_gc)
  suml6_nor_gc[index_nor_gc] <- paste("(",suml6_nor_gc[index_nor_gc],")",sep = "")
  l6_form_nor_gc <- paste(l6_form_nor_gc,collapse = " ")
  final_nor_gc[index_nor_gc] <- rbind(paste(nor_teams[index_nor_gc],l6_form_nor_gc,suml6_nor_gc[index_nor_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}

#change column names
final_nor_gc <- as.data.frame(final_nor_gc)
colnames(final_nor_gc) <- "Goals conceded"
#total goals
#create final_nor_tg object
final_nor_tg <- c()
suml6_nor_tg <- c()
for(index_nor_tg in 1:length(nor_teams))
{
  index_nor_tg <- row.names(nor_totalgoals_h) == nor_teams[index_nor_tg]
  form_nor_tg <- nor_totalgoals_h[index_nor_tg]
  deleted_form_nor_tg <- form_nor_tg[!form_nor_tg[] == ""]
  l6_form_nor_tg <- tail(deleted_form_nor_tg,nor_last_n_games)
  l6_form_nor_tg <- as.numeric(l6_form_nor_tg)
  suml6_nor_tg[index_nor_tg] <- sum(l6_form_nor_tg)
  suml6_nor_tg[index_nor_tg] <- paste("(",suml6_nor_tg[index_nor_tg],")",sep = "")
  l6_form_nor_tg <- paste(l6_form_nor_tg,collapse = " ")
  final_nor_tg[index_nor_tg] <- rbind(paste(nor_teams[index_nor_tg],l6_form_nor_tg,suml6_nor_tg[index_nor_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}
#change column names
final_nor_tg <- as.data.frame(final_nor_tg)
colnames(final_nor_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_nor_hf object
final_nor_cs <- c()
for(index_nor_cs in 1:length(nor_teams))
{
  index_nor_cs <- row.names(nor_csform_h) == nor_teams[index_nor_cs]
  csform_nor_cs <- nor_csform_h[index_nor_cs]
  deleted_csform_nor_cs <- csform_nor_cs[!csform_nor_cs[] == ""]
  l6_csform_nor_cs <- tail(deleted_csform_nor_cs,nor_last_n_games)
  l6_csform_nor_cs <- paste(l6_csform_nor_cs,collapse = " ")
  final_nor_cs[index_nor_cs] <- rbind(paste(nor_teams[index_nor_cs],l6_csform_nor_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",nor_teams[index],l6_csform)

}

#change column names
final_nor_cs <- as.data.frame(final_nor_cs)
colnames(final_nor_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_nor_wm object
final_nor_wm <- c()
suml6_nor_wm <- c()
for(index_nor_wm in 1:length(nor_teams))
{
  index_nor_wm <- row.names(nor_winmargin_h) == nor_teams[index_nor_wm]
  form_nor_wm <- nor_winmargin_h[index_nor_wm]
  deleted_form_nor_wm <- form_nor_wm[!form_nor_wm[] == ""]
  l6_form_nor_wm <- tail(deleted_form_nor_wm,nor_last_n_games)
  l6_form_nor_wm <- as.numeric(l6_form_nor_wm)
  suml6_nor_wm[index_nor_wm] <- sum(l6_form_nor_wm)
  suml6_nor_wm[index_nor_wm] <- paste("(",suml6_nor_wm[index_nor_wm],")",sep = "")
  l6_form_nor_wm <- paste(l6_form_nor_wm,collapse = " ")
  final_nor_wm[index_nor_wm] <- rbind(paste(nor_teams[index_nor_wm],l6_form_nor_wm,suml6_nor_wm[index_nor_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}
final_nor_wm
#change column names
final_nor_wm <- as.data.frame(final_nor_wm)
colnames(final_nor_wm) <- "Win Margin"
###########################################################################
#Team against
#create final_nor_hf_against
final_nor_hf_against <- c()
for(index_nor_hf_against in 1:length(nor_teams))
{
  index_nor_hf_against <- row.names(nor_form_team_against_h) == nor_teams[index_nor_hf_against]
  form_nor_hf_against <- nor_form_team_against_h[index_nor_hf_against]
  deleted_form_nor_hf_against <- form_nor_hf_against[!form_nor_hf_against[] == ""]
  l6_form_nor_hf_against <- tail(deleted_form_nor_hf_against,nor_last_n_games)
  l6_form_nor_hf_against <- paste(l6_form_nor_hf_against,collapse = " ")
  final_nor_hf_against[index_nor_hf_against] <- rbind(paste(nor_teams[index_nor_hf_against],l6_form_nor_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",nor_teams[index],l6_form)

}
final_nor_hf_against <- as.data.frame(final_nor_hf_against)
colnames(final_nor_hf_against) <- "Team against"
#combine the columns
final_nor_all <- cbind(final_nor_hf,final_nor_gs,final_nor_gc,final_nor_tg,final_nor_cs,final_nor_wm,final_nor_hf_against)
write.xlsx(final_nor_all,'NL/NOR.xlsx',sheetName = "L6", append = TRUE)
#############################################################################################################
##########################poisson model######################################################################
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

#create home and away csv
#nor_home_poisson <- rbind(nor_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,i2_home_poisson,n1_home_poisson,p1_home_poisson,sc0_home_poisson,sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,sp1_home_poisson,sp2_home_poisson,t1_home_poisson)
#nor_away_poisson <- rbind(nor_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,i2_away_poisson,n1_away_poisson,p1_away_poisson,sc0_away_poisson,sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,sp1_away_poisson,sp2_away_poisson,t1_away_poisson)
#write another one
#write.csv(home_poisson,'R_home.csv')
#write.csv(away_poisson,'R_away.csv')
write.xlsx(nor_home_poisson,'NL/NOR.xlsx',sheetName = "homepoisson", append = TRUE)
write.xlsx(nor_away_poisson,'NL/NOR.xlsx',sheetName = "awaypoisson", append = TRUE)
nor_home_poisson
nor_away_poisson
##########################################################################################################
###################NOR FIXTURES##########################################################################
#NOR
HomeTeam_nor <- rep(nor_teams, each = length(nor_teams))
AwayTeam_nor <- rep(nor_teams, length(nor_teams))
NOR_fixtures <- cbind(HomeTeam_nor,AwayTeam_nor)
NOR_fixtures <- as.data.frame(NOR_fixtures)
NOR_fixtures <- NOR_fixtures[!NOR_fixtures$HomeTeam_nor == NOR_fixtures$AwayTeam_nor,]
rownames(NOR_fixtures) <- NULL
NOR_fixtures$Div <- "NOR"
NOR_fixtures <- NOR_fixtures[,c(3,1,2)]

NOR_fixtures$avg_HG_nor <- nor_avg_HG

NOR_fixtures$nor_homeas <- rep(nor_home_as,each = length(nor_teams)-1)

nor_awayds_lookup <- cbind(nor_teams,nor_away_ds)

nor_awayds_lookup <- as.data.frame(nor_awayds_lookup)

colnames(nor_awayds_lookup) <- c("AwayTeam_nor","nor_awayds")


require('RH2')
NOR_fixtures$nor_awayds <- sqldf("SELECT nor_awayds_lookup.nor_awayds FROM nor_awayds_lookup INNER JOIN NOR_fixtures ON nor_awayds_lookup.AwayTeam_nor = NOR_fixtures.AwayTeam_nor")

NOR_fixtures$avg_AG_nor <- nor_avg_AG

nor_awayas_lookup <- cbind(nor_teams,nor_away_as)

nor_awayas_lookup <- as.data.frame(nor_awayas_lookup)

colnames(nor_awayas_lookup) <- c("AwayTeam_nor","nor_awayas")


NOR_fixtures$nor_awayas <- sqldf("SELECT nor_awayas_lookup.nor_awayas FROM nor_awayas_lookup INNER JOIN NOR_fixtures ON nor_awayas_lookup.AwayTeam_nor = NOR_fixtures.AwayTeam_nor")

NOR_fixtures$nor_homeds <- rep(nor_home_ds,each = length(nor_teams)-1)

NOR_fixtures$nor_awayds <- as.numeric(unlist(NOR_fixtures$nor_awayds))
#xGH
NOR_fixtures$nor_xGH <- NOR_fixtures$avg_HG_nor * NOR_fixtures$nor_homeas * NOR_fixtures$nor_awayds

#xGA

NOR_fixtures$nor_awayas <- as.numeric(unlist(NOR_fixtures$nor_awayas))

NOR_fixtures$nor_xGA <- NOR_fixtures$avg_AG_nor * NOR_fixtures$nor_awayas * NOR_fixtures$nor_homeds

NOR_fixtures$nor_0_0 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_0 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_0_1 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_1 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_0 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_0_2 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_2 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_1 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_2 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_3 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_0 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_1 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_2 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_0_3 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_3 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_3 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_4 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_0 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_1 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_2 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_3 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_0_4 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_4 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_4 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_4 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_5 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_0 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_1 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_2 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_3 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_4 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_0_5 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_5 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_5 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_5 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_5 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_6 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_0 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(0,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_1 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(1,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_2 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(2,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_3 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(3,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_4 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(4,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_6_5 <- round(stats::dpois(6,NOR_fixtures$nor_xGH) * stats::dpois(5,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_0_6 <- round(stats::dpois(0,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_1_6 <- round(stats::dpois(1,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_2_6 <- round(stats::dpois(2,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_3_6 <- round(stats::dpois(3,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_4_6 <- round(stats::dpois(4,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
NOR_fixtures$nor_5_6 <- round(stats::dpois(5,NOR_fixtures$nor_xGH) * stats::dpois(6,NOR_fixtures$nor_xGA), digits = 4)
#Home win
NOR_fixtures$nor_H <- (
  NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
    NOR_fixtures$nor_5_0 + NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5
)

NOR_fixtures$nor_H <- percent(NOR_fixtures$nor_H, accuracy = 0.1)

#Draw
NOR_fixtures$nor_D <- (

  NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 + NOR_fixtures$nor_4_4 +
    NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6
)

NOR_fixtures$nor_D <- percent(NOR_fixtures$nor_D, accuracy = 0.1)

#Away

NOR_fixtures$nor_A <- (
  NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
    NOR_fixtures$nor_0_5 + NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6
)

NOR_fixtures$nor_A <- percent(NOR_fixtures$nor_A, accuracy = 0.1)

#ov25
NOR_fixtures$nor_ov25 <- (
  NOR_fixtures$nor_2_1 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 + NOR_fixtures$nor_2_3 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 + NOR_fixtures$nor_0_4 +
    NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 + NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_0 +
    NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 + NOR_fixtures$nor_0_5 +
    NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 + NOR_fixtures$nor_5_5 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5 + NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 +
    NOR_fixtures$nor_4_6 + NOR_fixtures$nor_5_6 + NOR_fixtures$nor_6_6
)
#un25
NOR_fixtures$nor_un25 <- (
  NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_0 + NOR_fixtures$nor_0_1 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_0_2
)
#odds
NOR_fixtures$nor_ov25_odds <- round((1/NOR_fixtures$nor_ov25),digits = 2)
NOR_fixtures$nor_un25_odds <- round((1/NOR_fixtures$nor_un25),digits = 2)

NOR_fixtures$nor_ov25_odds
NOR_fixtures$nor_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
NOR_fixtures$nor_BTTSY <- (
  NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_3_1 + NOR_fixtures$nor_3_2 +
    NOR_fixtures$nor_2_2 + NOR_fixtures$nor_1_3 + NOR_fixtures$nor_2_3 + NOR_fixtures$nor_3_3 + NOR_fixtures$nor_4_4 +
    NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_3 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 +
    NOR_fixtures$nor_3_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 +
    NOR_fixtures$nor_5_4 + NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_6_6 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6
)
#BTTSN
NOR_fixtures$nor_BTTSN <- (
  NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_0 + NOR_fixtures$nor_0_1 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_0_2 +
    NOR_fixtures$nor_3_0 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_5_0 +
    NOR_fixtures$nor_0_5 + NOR_fixtures$nor_6_0 + NOR_fixtures$nor_0_6
)

NOR_fixtures$nor_BTTSY_odds <- round((1/NOR_fixtures$nor_BTTSY),digits = 2)
NOR_fixtures$nor_BTTSN_odds <- round((1/NOR_fixtures$nor_BTTSN),digits = 2)

NOR_fixtures$nor_BTTSY <- percent(NOR_fixtures$nor_BTTSY, accuracy = 0.1)
NOR_fixtures$nor_BTTSN <- percent(NOR_fixtures$nor_BTTSN, accuracy = 0.1)
#odds
NOR_fixtures$nor_BTTSY_odds
NOR_fixtures$nor_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
NOR_fixtures$nor_AH_0_H <- (
  NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
    NOR_fixtures$nor_5_0 +NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5 + NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6
)
#AH_0_A
NOR_fixtures$nor_AH_0_A <- (
  NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
    NOR_fixtures$nor_0_5 +NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6 + NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6
)

#odds
NOR_fixtures$nor_AH_0_H_odds <- round((1/NOR_fixtures$nor_AH_0_H),digits = 2)
NOR_fixtures$nor_AH_0_A_odds <- round((1/NOR_fixtures$nor_AH_0_A),digits = 2)

NOR_fixtures$nor_AH_0_H_odds
NOR_fixtures$nor_AH_0_A_odds
#percentages
NOR_fixtures$nor_AH_0_H <- percent(NOR_fixtures$nor_AH_0_H, accuracy = 0.1)
NOR_fixtures$nor_AH_0_A <- percent(NOR_fixtures$nor_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
NOR_fixtures$nor_AH_n075_H <- (
  NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
    NOR_fixtures$nor_5_0 +NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5
)
#AH_n075_A
NOR_fixtures$nor_AH_n075_A <- (
  NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
    NOR_fixtures$nor_0_5 +NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6
)

#odds
NOR_fixtures$nor_AH_n075_H_odds <- round((1/NOR_fixtures$nor_AH_n075_H),digits = 2)
NOR_fixtures$nor_AH_n075_A_odds <- round((1/NOR_fixtures$nor_AH_n075_A),digits = 2)

NOR_fixtures$nor_AH_n075_H_odds
NOR_fixtures$nor_AH_n075_A_odds
#percentages
NOR_fixtures$nor_AH_n075_H <- percent(NOR_fixtures$nor_AH_n075_H, accuracy = 0.1)
NOR_fixtures$nor_AH_n075_A <- percent(NOR_fixtures$nor_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
NOR_fixtures$nor_AH_075_H <- (
  NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
    NOR_fixtures$nor_5_0 +NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5 + NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6 + NOR_fixtures$nor_0_1 + NOR_fixtures$nor_1_2 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_3_4 + NOR_fixtures$nor_4_5 + NOR_fixtures$nor_5_6
)
#AH_075_A
NOR_fixtures$nor_AH_075_A <- (
  NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
    NOR_fixtures$nor_0_5 +NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6 + NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6 + NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_3 + NOR_fixtures$nor_5_4 + NOR_fixtures$nor_6_5
)

#odds
NOR_fixtures$nor_AH_075_H_odds <- round((1/NOR_fixtures$nor_AH_075_H),digits = 2)
NOR_fixtures$nor_AH_075_A_odds <- round((1/NOR_fixtures$nor_AH_075_A),digits = 2)

NOR_fixtures$nor_AH_075_H_odds
NOR_fixtures$nor_AH_075_A_odds
#percentages
NOR_fixtures$nor_AH_075_H <- percent(NOR_fixtures$nor_AH_075_H, accuracy = 0.1)
NOR_fixtures$nor_AH_075_A <- percent(NOR_fixtures$nor_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
NOR_fixtures$nor_AH_n125_H <- (
  NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
    NOR_fixtures$nor_5_0 +NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5
)
#AH_n125_A
NOR_fixtures$nor_AH_n125_A <- (
  NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
    NOR_fixtures$nor_0_5 +NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6
)

#odds
NOR_fixtures$nor_AH_n125_H_odds <- round((1/NOR_fixtures$nor_AH_n125_H),digits = 2)
NOR_fixtures$nor_AH_n125_A_odds <- round((1/NOR_fixtures$nor_AH_n125_A),digits = 2)

NOR_fixtures$nor_AH_n125_H_odds
NOR_fixtures$nor_AH_n125_A_odds
#percentages
NOR_fixtures$nor_AH_n125_H <- percent(NOR_fixtures$nor_AH_n125_H, accuracy = 0.1)
NOR_fixtures$nor_AH_n125_A <- percent(NOR_fixtures$nor_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
NOR_fixtures$nor_AH_125_H <- (
  NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_0 + NOR_fixtures$nor_2_1 + NOR_fixtures$nor_3_0 + NOR_fixtures$nor_3_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_0 + NOR_fixtures$nor_4_1 + NOR_fixtures$nor_4_2 + NOR_fixtures$nor_4_3 +
    NOR_fixtures$nor_5_0 +NOR_fixtures$nor_5_1 + NOR_fixtures$nor_5_2 + NOR_fixtures$nor_5_3 + NOR_fixtures$nor_5_4 +
    NOR_fixtures$nor_6_0 + NOR_fixtures$nor_6_1 + NOR_fixtures$nor_6_2 + NOR_fixtures$nor_6_3 + NOR_fixtures$nor_6_4 +
    NOR_fixtures$nor_6_5 + NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6 + NOR_fixtures$nor_0_1 + NOR_fixtures$nor_1_2 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_3_4 + NOR_fixtures$nor_4_5 + NOR_fixtures$nor_5_6
)
#AH_125_A
NOR_fixtures$nor_AH_125_A <- (
  NOR_fixtures$nor_0_1 + NOR_fixtures$nor_0_2 + NOR_fixtures$nor_1_2 + NOR_fixtures$nor_0_3 + NOR_fixtures$nor_1_3 +
    NOR_fixtures$nor_2_3 + NOR_fixtures$nor_0_4 + NOR_fixtures$nor_1_4 + NOR_fixtures$nor_2_4 + NOR_fixtures$nor_3_4 +
    NOR_fixtures$nor_0_5 +NOR_fixtures$nor_1_5 + NOR_fixtures$nor_2_5 + NOR_fixtures$nor_3_5 + NOR_fixtures$nor_4_5 +
    NOR_fixtures$nor_0_6 + NOR_fixtures$nor_1_6 + NOR_fixtures$nor_2_6 + NOR_fixtures$nor_3_6 + NOR_fixtures$nor_4_6 +
    NOR_fixtures$nor_5_6 + NOR_fixtures$nor_0_0 + NOR_fixtures$nor_1_1 + NOR_fixtures$nor_2_2 + NOR_fixtures$nor_3_3 +
    NOR_fixtures$nor_4_4 + NOR_fixtures$nor_5_5 + NOR_fixtures$nor_6_6 + NOR_fixtures$nor_1_0 + NOR_fixtures$nor_2_1 +
    NOR_fixtures$nor_3_2 + NOR_fixtures$nor_4_3 + NOR_fixtures$nor_5_4 + NOR_fixtures$nor_6_5
)

#odds
NOR_fixtures$nor_AH_125_H_odds <- round((1/NOR_fixtures$nor_AH_125_H),digits = 2)
NOR_fixtures$nor_AH_125_A_odds <- round((1/NOR_fixtures$nor_AH_125_A),digits = 2)

NOR_fixtures$nor_AH_125_H_odds
NOR_fixtures$nor_AH_125_A_odds
#percentages
NOR_fixtures$nor_AH_125_H <- percent(NOR_fixtures$nor_AH_125_H, accuracy = 0.1)
NOR_fixtures$nor_AH_125_A <- percent(NOR_fixtures$nor_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
NOR_fixtures$nor_ov25 <- percent(NOR_fixtures$nor_ov25, accuracy = 0.1)

NOR_fixtures$nor_un25 <- percent(NOR_fixtures$nor_un25, accuracy = 0.1)
NOR_fixtures$nor_pscore <- paste(round(NOR_fixtures$nor_xGH,digits = 0),round(NOR_fixtures$nor_xGA,digits = 0),sep = "-")
#write out
write.xlsx(NOR_fixtures,'NL/NOR.xlsx',sheetName = "NOR", append = TRUE)
###########################################################################################################
########################NOR END###########################################################################
NOR <- read.csv('../FDAS/NOR.csv')
NOR$TG <- NOR$HG + NOR$AG
NOR$OV25 <- ifelse(NOR$TG >= 3,"Y","N")
nor_ftr_summary <- tabyl(NOR,Season,Res) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
nor_ov25_summary <- tabyl(NOR,Season,OV25) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,3,2)]
write.xlsx(nor_ftr_summary,'NL/NOR.xlsx',sheetName = "FTR", append = TRUE)
write.xlsx(nor_ov25_summary,'NL/NOR.xlsx',sheetName = "OVUN25", append = TRUE)
NOR <- subset(NOR,Season == "2023")




