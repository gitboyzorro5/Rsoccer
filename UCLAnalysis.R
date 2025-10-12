#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#tgv2 <- with(B1, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_B1, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_B1, tapply(TG, list(AwayTeam, Date), FUN = mean))

UCL_analytics <- readxl::read_excel('UCL20242025.xlsx')
UCL_analytics <- UCL_analytics[,-1]
UCL_analytics <- as.data.frame(UCL_analytics)
E0_analytics <- subset(UCL_analytics, Div == "E0")

#create home and away matrices
e0_goalmins_h <- tapply(E0_analytics$goalmins, E0_analytics[c("HomeTeam", "Date")],mean)
e0_goalmins_a <- tapply(E0_analytics$goalmins, E0_analytics[c("AwayTeam", "Date")],mean)

e0_goalmins_h[is.na(e0_goalmins_h)] <- ""
e0_goalmins_a[is.na(e0_goalmins_a)] <- ""

for(e0_rowhgs in 1:nrow(e0_goalmins_h)) {
  for(e0_colhgs in 1:ncol(e0_goalmins_h)) {

    # print(my_matrix[row, col])
    for(e0_rowags in 1:nrow(e0_goalmins_a)) {
      for(e0_colags in 1:ncol(e0_goalmins_a)) {
        ifelse(!e0_goalmins_a[e0_rowags,e0_colags]=="",e0_goalmins_h[e0_rowags,e0_colags] <- e0_goalmins_a[e0_rowags,e0_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

source("divisions.R")
source("goaltotalsv2.R")
source("Matchday.R")
source("KROUNDS.R")

final_e0_goalmins <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_goalmins <- c()
sum_e0_zero_goalmins <- c()
sum_e0_one_goalmins <- c()
sum_e0_two_goalmins <- c()
sum_e0_three_goalmins <- c()
avgr_e0_goalmins <- c()
sdr_e0_goalmins <- c()
l6_form_e0_goalminssplitted <- c()
form_e0_goalmins <- c()
for(index_e0_goalmins in 1:length(e0_teams))
{
  for(index_e0_goalmins_cols in 1:e0_totalrounds)
  {
    index_e0_goalmins  <- row.names(e0_goalmins_h) == e0_teams[index_e0_goalmins]
    form_e0_goalmins <- e0_goalmins_h[index_e0_goalmins ]
    deleted_form_e0_goalmins <- form_e0_goalmins[!form_e0_goalmins[] == ""]
    l6_form_e0_goalmins <- tail(deleted_form_e0_goalmins,e0_last_n_games)
    l6_form_e0_goalmins <- as.numeric(l6_form_e0_goalmins)
    suml6_e0_goalmins[index_e0_goalmins] <- sum(l6_form_e0_goalmins)
    suml6_e0_goalmins[index_e0_goalmins] <- paste(suml6_e0_goalmins[index_e0_goalmins],sep = "")
    sum_e0_zero_goalmins[index_e0_goalmins] <- length(which(l6_form_e0_goalmins == 0))
    sum_e0_zero_goalmins[index_e0_goalmins] <- paste(sum_e0_zero_goalmins[index_e0_goalmins],sep = "")
    sum_e0_one_goalmins[index_e0_goalmins] <- length(which(l6_form_e0_goalmins >= 100))
    sum_e0_one_goalmins[index_e0_goalmins] <- paste(sum_e0_one_goalmins[index_e0_goalmins],sep = "")
    sum_e0_two_goalmins[index_e0_goalmins] <- length(which(l6_form_e0_goalmins >= 200))
    sum_e0_two_goalmins[index_e0_goalmins] <- paste(sum_e0_two_goalmins[index_e0_goalmins],sep = "")
    sum_e0_three_goalmins[index_e0_goalmins] <- length(which(l6_form_e0_goalmins >= 300))
    sum_e0_three_goalmins[index_e0_goalmins] <- paste(sum_e0_three_goalmins[index_e0_goalmins],sep = "")
    avgr_e0_goalmins[index_e0_goalmins] <- mean(l6_form_e0_goalmins)
    avgr_e0_goalmins[index_e0_goalmins] <- paste(avgr_e0_goalmins[index_e0_goalmins],sep = "")
    sdr_e0_goalmins[index_e0_goalmins] <- sd(l6_form_e0_goalmins)
    sdr_e0_goalmins[index_e0_goalmins] <- paste(sdr_e0_goalmins[index_e0_goalmins],sep = "")
    l6_form_e0_goalmins <- as.character(l6_form_e0_goalmins)
    #l6_form_e0_goalmins_flattened <- stri_paste(l6_form_e0_goalmins,collapse = '')
    #l6_form_e0_goalminssplitted <- as.numeric(strsplit(as.character(l6_form_e0_goalmins_flattened),"")[[1]])
    final_e0_goalmins[index_e0_goalmins,index_e0_goalmins_cols] <- l6_form_e0_goalmins[index_e0_goalmins_cols]
  }
}

final_e0_goalmins[is.na(final_e0_goalmins)] <- ""
e0_goalminsmatrix <- cbind(e0_teams,final_e0_goalmins,suml6_e0_goalmins,sum_e0_zero_goalmins,sum_e0_one_goalmins,sum_e0_two_goalmins,sum_e0_three_goalmins,avgr_e0_goalmins,sdr_e0_goalmins)
unlink('Analytics/UCL/E0ucl.xlsx')
write.xlsx(e0_goalminsmatrix,"Analytics/UCL/E0ucl.xlsx", sheetName = "goalmins")
##############################################################################################################################################################################################

#create home and away matrices
e0_shirts_h <- tapply(E0_analytics$shirts, E0_analytics[c("HomeTeam", "Date")],mean)
e0_shirts_a <- tapply(E0_analytics$shirts, E0_analytics[c("AwayTeam", "Date")],mean)

e0_shirts_h[is.na(e0_shirts_h)] <- ""
e0_shirts_a[is.na(e0_shirts_a)] <- ""

for(e0_rowhgs in 1:nrow(e0_shirts_h)) {
  for(e0_colhgs in 1:ncol(e0_shirts_h)) {

    # print(my_matrix[row, col])
    for(e0_rowags in 1:nrow(e0_shirts_a)) {
      for(e0_colags in 1:ncol(e0_shirts_a)) {
        ifelse(!e0_shirts_a[e0_rowags,e0_colags]=="",e0_shirts_h[e0_rowags,e0_colags] <- e0_shirts_a[e0_rowags,e0_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_e0_shirts <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_shirts <- c()
sum_e0_zero_shirts <- c()
sum_e0_one_shirts <- c()
sum_e0_two_shirts <- c()
sum_e0_three_shirts <- c()
avgr_e0_shirts <- c()
sdr_e0_shirts <- c()
l6_form_e0_shirtssplitted <- c()
form_e0_shirts <- c()
for(index_e0_shirts in 1:length(e0_teams))
{
  for(index_e0_shirts_cols in 1:e0_totalrounds)
  {
    index_e0_shirts  <- row.names(e0_shirts_h) == e0_teams[index_e0_shirts]
    form_e0_shirts <- e0_shirts_h[index_e0_shirts ]
    deleted_form_e0_shirts <- form_e0_shirts[!form_e0_shirts[] == ""]
    l6_form_e0_shirts <- tail(deleted_form_e0_shirts,e0_last_n_games)
    l6_form_e0_shirts <- as.numeric(l6_form_e0_shirts)
    suml6_e0_shirts[index_e0_shirts] <- sum(l6_form_e0_shirts)
    suml6_e0_shirts[index_e0_shirts] <- paste(suml6_e0_shirts[index_e0_shirts],sep = "")
    sum_e0_zero_shirts[index_e0_shirts] <- length(which(l6_form_e0_shirts == 0))
    sum_e0_zero_shirts[index_e0_shirts] <- paste(sum_e0_zero_shirts[index_e0_shirts],sep = "")
    sum_e0_one_shirts[index_e0_shirts] <- length(which(l6_form_e0_shirts >= 10))
    sum_e0_one_shirts[index_e0_shirts] <- paste(sum_e0_one_shirts[index_e0_shirts],sep = "")
    sum_e0_two_shirts[index_e0_shirts] <- length(which(l6_form_e0_shirts >= 20))
    sum_e0_two_shirts[index_e0_shirts] <- paste(sum_e0_two_shirts[index_e0_shirts],sep = "")
    sum_e0_three_shirts[index_e0_shirts] <- length(which(l6_form_e0_shirts >= 30))
    sum_e0_three_shirts[index_e0_shirts] <- paste(sum_e0_three_shirts[index_e0_shirts],sep = "")
    avgr_e0_shirts[index_e0_shirts] <- mean(l6_form_e0_shirts)
    avgr_e0_shirts[index_e0_shirts] <- paste(avgr_e0_shirts[index_e0_shirts],sep = "")
    sdr_e0_shirts[index_e0_shirts] <- sd(l6_form_e0_shirts)
    sdr_e0_shirts[index_e0_shirts] <- paste(sdr_e0_shirts[index_e0_shirts],sep = "")
    l6_form_e0_shirts <- as.character(l6_form_e0_shirts)
    #l6_form_e0_shirts_flattened <- stri_paste(l6_form_e0_shirts,collapse = '')
    #l6_form_e0_shirtssplitted <- as.numeric(strsplit(as.character(l6_form_e0_shirts_flattened),"")[[1]])
    final_e0_shirts[index_e0_shirts,index_e0_shirts_cols] <- l6_form_e0_shirts[index_e0_shirts_cols]
  }
}

final_e0_shirts[is.na(final_e0_shirts)] <- ""
e0_shirtsmatrix <- cbind(e0_teams,final_e0_shirts,suml6_e0_shirts,sum_e0_zero_shirts,sum_e0_one_shirts,sum_e0_two_shirts,sum_e0_three_shirts,avgr_e0_shirts,sdr_e0_shirts)
write.xlsx(e0_shirtsmatrix,"Analytics/UCL/E0ucl.xlsx", sheetName = "shirts", append = TRUE)
#######################################################################################################################################################################################

#create home and away matrices
e0_crossbookings_h <- tapply(E0_analytics$Crossbookings, E0_analytics[c("HomeTeam", "Date")],mean)
e0_crossbookings_a <- tapply(E0_analytics$Crossbookings, E0_analytics[c("AwayTeam", "Date")],mean)

e0_crossbookings_h[is.na(e0_crossbookings_h)] <- ""
e0_crossbookings_a[is.na(e0_crossbookings_a)] <- ""

for(e0_rowhgs in 1:nrow(e0_crossbookings_h)) {
  for(e0_colhgs in 1:ncol(e0_crossbookings_h)) {

    # print(my_matrix[row, col])
    for(e0_rowags in 1:nrow(e0_crossbookings_a)) {
      for(e0_colags in 1:ncol(e0_crossbookings_a)) {
        ifelse(!e0_crossbookings_a[e0_rowags,e0_colags]=="",e0_crossbookings_h[e0_rowags,e0_colags] <- e0_crossbookings_a[e0_rowags,e0_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_e0_crossbookings <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_crossbookings <- c()
sum_e0_zero_crossbookings <- c()
sum_e0_one_crossbookings <- c()
sum_e0_two_crossbookings <- c()
sum_e0_three_crossbookings <- c()
avgr_e0_crossbookings <- c()
sdr_e0_crossbookings <- c()
l6_form_e0_crossbookingssplitted <- c()
form_e0_crossbookings <- c()
for(index_e0_crossbookings in 1:length(e0_teams))
{
  for(index_e0_crossbookings_cols in 1:e0_totalrounds)
  {
    index_e0_crossbookings  <- row.names(e0_crossbookings_h) == e0_teams[index_e0_crossbookings]
    form_e0_crossbookings <- e0_crossbookings_h[index_e0_crossbookings ]
    deleted_form_e0_crossbookings <- form_e0_crossbookings[!form_e0_crossbookings[] == ""]
    l6_form_e0_crossbookings <- tail(deleted_form_e0_crossbookings,e0_last_n_games)
    l6_form_e0_crossbookings <- as.numeric(l6_form_e0_crossbookings)
    suml6_e0_crossbookings[index_e0_crossbookings] <- sum(l6_form_e0_crossbookings)
    suml6_e0_crossbookings[index_e0_crossbookings] <- paste(suml6_e0_crossbookings[index_e0_crossbookings],sep = "")
    sum_e0_zero_crossbookings[index_e0_crossbookings] <- length(which(l6_form_e0_crossbookings == 0))
    sum_e0_zero_crossbookings[index_e0_crossbookings] <- paste(sum_e0_zero_crossbookings[index_e0_crossbookings],sep = "")
    sum_e0_one_crossbookings[index_e0_crossbookings] <- length(which(l6_form_e0_crossbookings >= 10))
    sum_e0_one_crossbookings[index_e0_crossbookings] <- paste(sum_e0_one_crossbookings[index_e0_crossbookings],sep = "")
    sum_e0_two_crossbookings[index_e0_crossbookings] <- length(which(l6_form_e0_crossbookings >= 20))
    sum_e0_two_crossbookings[index_e0_crossbookings] <- paste(sum_e0_two_crossbookings[index_e0_crossbookings],sep = "")
    sum_e0_three_crossbookings[index_e0_crossbookings] <- length(which(l6_form_e0_crossbookings >= 30))
    sum_e0_three_crossbookings[index_e0_crossbookings] <- paste(sum_e0_three_crossbookings[index_e0_crossbookings],sep = "")
    avgr_e0_crossbookings[index_e0_crossbookings] <- mean(l6_form_e0_crossbookings)
    avgr_e0_crossbookings[index_e0_crossbookings] <- paste(avgr_e0_crossbookings[index_e0_crossbookings],sep = "")
    sdr_e0_crossbookings[index_e0_crossbookings] <- sd(l6_form_e0_crossbookings)
    sdr_e0_crossbookings[index_e0_crossbookings] <- paste(sdr_e0_crossbookings[index_e0_crossbookings],sep = "")
    l6_form_e0_crossbookings <- as.character(l6_form_e0_crossbookings)
    #l6_form_e0_crossbookings_flattened <- stri_paste(l6_form_e0_crossbookings,collapse = '')
    #l6_form_e0_crossbookingssplitted <- as.numeric(strsplit(as.character(l6_form_e0_crossbookings_flattened),"")[[1]])
    final_e0_crossbookings[index_e0_crossbookings,index_e0_crossbookings_cols] <- l6_form_e0_crossbookings[index_e0_crossbookings_cols]
  }
}

final_e0_crossbookings[is.na(final_e0_crossbookings)] <- ""
e0_crossbookingsmatrix <- cbind(e0_teams,final_e0_crossbookings,suml6_e0_crossbookings,sum_e0_zero_crossbookings,sum_e0_one_crossbookings,sum_e0_two_crossbookings,sum_e0_three_crossbookings,avgr_e0_crossbookings,sdr_e0_crossbookings)
write.xlsx(e0_crossbookingsmatrix,"Analytics/UCL/E0ucl.xlsx", sheetName = "crossbookings", append = TRUE)
######################################################################################################################################################################################

#create home and away matrices
e0_shirtsxbookings_h <- tapply(E0_analytics$ShirtsXbookings, E0_analytics[c("HomeTeam", "Date")],mean)
e0_shirtsxbookings_a <- tapply(E0_analytics$ShirtsXbookings, E0_analytics[c("AwayTeam", "Date")],mean)

e0_shirtsxbookings_h[is.na(e0_shirtsxbookings_h)] <- ""
e0_shirtsxbookings_a[is.na(e0_shirtsxbookings_a)] <- ""

for(e0_rowhgs in 1:nrow(e0_shirtsxbookings_h)) {
  for(e0_colhgs in 1:ncol(e0_shirtsxbookings_h)) {

    # print(my_matrix[row, col])
    for(e0_rowags in 1:nrow(e0_shirtsxbookings_a)) {
      for(e0_colags in 1:ncol(e0_shirtsxbookings_a)) {
        ifelse(!e0_shirtsxbookings_a[e0_rowags,e0_colags]=="",e0_shirtsxbookings_h[e0_rowags,e0_colags] <- e0_shirtsxbookings_a[e0_rowags,e0_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_e0_shirtsxbookings <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_shirtsxbookings <- c()
sum_e0_zero_shirtsxbookings <- c()
sum_e0_one_shirtsxbookings <- c()
sum_e0_two_shirtsxbookings <- c()
sum_e0_three_shirtsxbookings <- c()
avgr_e0_shirtsxbookings <- c()
sdr_e0_shirtsxbookings <- c()
l6_form_e0_shirtsxbookingssplitted <- c()
form_e0_shirtsxbookings <- c()
for(index_e0_shirtsxbookings in 1:length(e0_teams))
{
  for(index_e0_shirtsxbookings_cols in 1:e0_totalrounds)
  {
    index_e0_shirtsxbookings  <- row.names(e0_shirtsxbookings_h) == e0_teams[index_e0_shirtsxbookings]
    form_e0_shirtsxbookings <- e0_shirtsxbookings_h[index_e0_shirtsxbookings ]
    deleted_form_e0_shirtsxbookings <- form_e0_shirtsxbookings[!form_e0_shirtsxbookings[] == ""]
    l6_form_e0_shirtsxbookings <- tail(deleted_form_e0_shirtsxbookings,e0_last_n_games)
    l6_form_e0_shirtsxbookings <- as.numeric(l6_form_e0_shirtsxbookings)
    suml6_e0_shirtsxbookings[index_e0_shirtsxbookings] <- sum(l6_form_e0_shirtsxbookings)
    suml6_e0_shirtsxbookings[index_e0_shirtsxbookings] <- paste(suml6_e0_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    sum_e0_zero_shirtsxbookings[index_e0_shirtsxbookings] <- length(which(l6_form_e0_shirtsxbookings == 0))
    sum_e0_zero_shirtsxbookings[index_e0_shirtsxbookings] <- paste(sum_e0_zero_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    sum_e0_one_shirtsxbookings[index_e0_shirtsxbookings] <- length(which(l6_form_e0_shirtsxbookings >= 10))
    sum_e0_one_shirtsxbookings[index_e0_shirtsxbookings] <- paste(sum_e0_one_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    sum_e0_two_shirtsxbookings[index_e0_shirtsxbookings] <- length(which(l6_form_e0_shirtsxbookings >= 20))
    sum_e0_two_shirtsxbookings[index_e0_shirtsxbookings] <- paste(sum_e0_two_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    sum_e0_three_shirtsxbookings[index_e0_shirtsxbookings] <- length(which(l6_form_e0_shirtsxbookings >= 30))
    sum_e0_three_shirtsxbookings[index_e0_shirtsxbookings] <- paste(sum_e0_three_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    avgr_e0_shirtsxbookings[index_e0_shirtsxbookings] <- mean(l6_form_e0_shirtsxbookings)
    avgr_e0_shirtsxbookings[index_e0_shirtsxbookings] <- paste(avgr_e0_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    sdr_e0_shirtsxbookings[index_e0_shirtsxbookings] <- sd(l6_form_e0_shirtsxbookings)
    sdr_e0_shirtsxbookings[index_e0_shirtsxbookings] <- paste(sdr_e0_shirtsxbookings[index_e0_shirtsxbookings],sep = "")
    l6_form_e0_shirtsxbookings <- as.character(l6_form_e0_shirtsxbookings)
    #l6_form_e0_shirtsxbookings_flattened <- stri_paste(l6_form_e0_shirtsxbookings,collapse = '')
    #l6_form_e0_shirtsxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_e0_shirtsxbookings_flattened),"")[[1]])
    final_e0_shirtsxbookings[index_e0_shirtsxbookings,index_e0_shirtsxbookings_cols] <- l6_form_e0_shirtsxbookings[index_e0_shirtsxbookings_cols]
  }
}

final_e0_shirtsxbookings[is.na(final_e0_shirtsxbookings)] <- ""
e0_shirtsxbookingsmatrix <- cbind(e0_teams,final_e0_shirtsxbookings,suml6_e0_shirtsxbookings,sum_e0_zero_shirtsxbookings,sum_e0_one_shirtsxbookings,sum_e0_two_shirtsxbookings,sum_e0_three_shirtsxbookings,avgr_e0_shirtsxbookings,sdr_e0_shirtsxbookings)
write.xlsx(e0_shirtsxbookingsmatrix,"Analytics/UCL/E0ucl.xlsx", sheetName = "shirtsxbookings", append = TRUE)
##################################################################################################################################################################################

#create home and away matrices
e0_tgmxcorners_h <- tapply(E0_analytics$TGMXcorners, E0_analytics[c("HomeTeam", "Date")],mean)
e0_tgmxcorners_a <- tapply(E0_analytics$TGMXcorners, E0_analytics[c("AwayTeam", "Date")],mean)

e0_tgmxcorners_h[is.na(e0_tgmxcorners_h)] <- ""
e0_tgmxcorners_a[is.na(e0_tgmxcorners_a)] <- ""

for(e0_rowhgs in 1:nrow(e0_tgmxcorners_h)) {
  for(e0_colhgs in 1:ncol(e0_tgmxcorners_h)) {

    # print(my_matrix[row, col])
    for(e0_rowags in 1:nrow(e0_tgmxcorners_a)) {
      for(e0_colags in 1:ncol(e0_tgmxcorners_a)) {
        ifelse(!e0_tgmxcorners_a[e0_rowags,e0_colags]=="",e0_tgmxcorners_h[e0_rowags,e0_colags] <- e0_tgmxcorners_a[e0_rowags,e0_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_e0_tgmxcorners <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_tgmxcorners <- c()
sum_e0_zero_tgmxcorners <- c()
sum_e0_one_tgmxcorners <- c()
sum_e0_two_tgmxcorners <- c()
sum_e0_three_tgmxcorners <- c()
avgr_e0_tgmxcorners <- c()
sdr_e0_tgmxcorners <- c()
l6_form_e0_tgmxcornerssplitted <- c()
form_e0_tgmxcorners <- c()
for(index_e0_tgmxcorners in 1:length(e0_teams))
{
  for(index_e0_tgmxcorners_cols in 1:e0_totalrounds)
  {
    index_e0_tgmxcorners  <- row.names(e0_tgmxcorners_h) == e0_teams[index_e0_tgmxcorners]
    form_e0_tgmxcorners <- e0_tgmxcorners_h[index_e0_tgmxcorners ]
    deleted_form_e0_tgmxcorners <- form_e0_tgmxcorners[!form_e0_tgmxcorners[] == ""]
    l6_form_e0_tgmxcorners <- tail(deleted_form_e0_tgmxcorners,e0_last_n_games)
    l6_form_e0_tgmxcorners <- as.numeric(l6_form_e0_tgmxcorners)
    suml6_e0_tgmxcorners[index_e0_tgmxcorners] <- sum(l6_form_e0_tgmxcorners)
    suml6_e0_tgmxcorners[index_e0_tgmxcorners] <- paste(suml6_e0_tgmxcorners[index_e0_tgmxcorners],sep = "")
    sum_e0_zero_tgmxcorners[index_e0_tgmxcorners] <- length(which(l6_form_e0_tgmxcorners == 0))
    sum_e0_zero_tgmxcorners[index_e0_tgmxcorners] <- paste(sum_e0_zero_tgmxcorners[index_e0_tgmxcorners],sep = "")
    sum_e0_one_tgmxcorners[index_e0_tgmxcorners] <- length(which(l6_form_e0_tgmxcorners >= 10))
    sum_e0_one_tgmxcorners[index_e0_tgmxcorners] <- paste(sum_e0_one_tgmxcorners[index_e0_tgmxcorners],sep = "")
    sum_e0_two_tgmxcorners[index_e0_tgmxcorners] <- length(which(l6_form_e0_tgmxcorners >= 20))
    sum_e0_two_tgmxcorners[index_e0_tgmxcorners] <- paste(sum_e0_two_tgmxcorners[index_e0_tgmxcorners],sep = "")
    sum_e0_three_tgmxcorners[index_e0_tgmxcorners] <- length(which(l6_form_e0_tgmxcorners >= 30))
    sum_e0_three_tgmxcorners[index_e0_tgmxcorners] <- paste(sum_e0_three_tgmxcorners[index_e0_tgmxcorners],sep = "")
    avgr_e0_tgmxcorners[index_e0_tgmxcorners] <- mean(l6_form_e0_tgmxcorners)
    avgr_e0_tgmxcorners[index_e0_tgmxcorners] <- paste(avgr_e0_tgmxcorners[index_e0_tgmxcorners],sep = "")
    sdr_e0_tgmxcorners[index_e0_tgmxcorners] <- sd(l6_form_e0_tgmxcorners)
    sdr_e0_tgmxcorners[index_e0_tgmxcorners] <- paste(sdr_e0_tgmxcorners[index_e0_tgmxcorners],sep = "")
    l6_form_e0_tgmxcorners <- as.character(l6_form_e0_tgmxcorners)
    #l6_form_e0_tgmxcorners_flattened <- stri_paste(l6_form_e0_tgmxcorners,collapse = '')
    #l6_form_e0_tgmxcornerssplitted <- as.numeric(strsplit(as.character(l6_form_e0_tgmxcorners_flattened),"")[[1]])
    final_e0_tgmxcorners[index_e0_tgmxcorners,index_e0_tgmxcorners_cols] <- l6_form_e0_tgmxcorners[index_e0_tgmxcorners_cols]
  }
}

final_e0_tgmxcorners[is.na(final_e0_tgmxcorners)] <- ""
e0_tgmxcornersmatrix <- cbind(e0_teams,final_e0_tgmxcorners,suml6_e0_tgmxcorners,sum_e0_zero_tgmxcorners,sum_e0_one_tgmxcorners,sum_e0_two_tgmxcorners,sum_e0_three_tgmxcorners,avgr_e0_tgmxcorners,sdr_e0_tgmxcorners)
write.xlsx(e0_tgmxcornersmatrix,"Analytics/UCL/E0ucl.xlsx", sheetName = "tgmxcorners", append = TRUE)
########################################################################################################################################################################################################################

#create home and away matrices
e0_goalxcornersxbookings_h <- tapply(E0_analytics$GoalsXcornerXbookings, E0_analytics[c("HomeTeam", "Date")],mean)
e0_goalxcornersxbookings_a <- tapply(E0_analytics$GoalsXcornerXbookings, E0_analytics[c("AwayTeam", "Date")],mean)

e0_goalxcornersxbookings_h[is.na(e0_goalxcornersxbookings_h)] <- ""
e0_goalxcornersxbookings_a[is.na(e0_goalxcornersxbookings_a)] <- ""

for(e0_rowhgs in 1:nrow(e0_goalxcornersxbookings_h)) {
  for(e0_colhgs in 1:ncol(e0_goalxcornersxbookings_h)) {

    # print(my_matrix[row, col])
    for(e0_rowags in 1:nrow(e0_goalxcornersxbookings_a)) {
      for(e0_colags in 1:ncol(e0_goalxcornersxbookings_a)) {
        ifelse(!e0_goalxcornersxbookings_a[e0_rowags,e0_colags]=="",e0_goalxcornersxbookings_h[e0_rowags,e0_colags] <- e0_goalxcornersxbookings_a[e0_rowags,e0_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_e0_goalxcornersxbookings <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_goalxcornersxbookings <- c()
sum_e0_zero_goalxcornersxbookings <- c()
sum_e0_one_goalxcornersxbookings <- c()
sum_e0_two_goalxcornersxbookings <- c()
sum_e0_three_goalxcornersxbookings <- c()
avgr_e0_goalxcornersxbookings <- c()
sdr_e0_goalxcornersxbookings <- c()
l6_form_e0_goalxcornersxbookingssplitted <- c()
form_e0_goalxcornersxbookings <- c()
for(index_e0_goalxcornersxbookings in 1:length(e0_teams))
{
  for(index_e0_goalxcornersxbookings_cols in 1:e0_totalrounds)
  {
    index_e0_goalxcornersxbookings  <- row.names(e0_goalxcornersxbookings_h) == e0_teams[index_e0_goalxcornersxbookings]
    form_e0_goalxcornersxbookings <- e0_goalxcornersxbookings_h[index_e0_goalxcornersxbookings ]
    deleted_form_e0_goalxcornersxbookings <- form_e0_goalxcornersxbookings[!form_e0_goalxcornersxbookings[] == ""]
    l6_form_e0_goalxcornersxbookings <- tail(deleted_form_e0_goalxcornersxbookings,e0_last_n_games)
    l6_form_e0_goalxcornersxbookings <- as.numeric(l6_form_e0_goalxcornersxbookings)
    suml6_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- sum(l6_form_e0_goalxcornersxbookings)
    suml6_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(suml6_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    sum_e0_zero_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- length(which(l6_form_e0_goalxcornersxbookings == 0))
    sum_e0_zero_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(sum_e0_zero_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    sum_e0_one_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- length(which(l6_form_e0_goalxcornersxbookings >= 10))
    sum_e0_one_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(sum_e0_one_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    sum_e0_two_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- length(which(l6_form_e0_goalxcornersxbookings >= 20))
    sum_e0_two_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(sum_e0_two_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    sum_e0_three_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- length(which(l6_form_e0_goalxcornersxbookings >= 30))
    sum_e0_three_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(sum_e0_three_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    avgr_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- mean(l6_form_e0_goalxcornersxbookings)
    avgr_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(avgr_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    sdr_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- sd(l6_form_e0_goalxcornersxbookings)
    sdr_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings] <- paste(sdr_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings],sep = "")
    l6_form_e0_goalxcornersxbookings <- as.character(l6_form_e0_goalxcornersxbookings)
    #l6_form_e0_goalxcornersxbookings_flattened <- stri_paste(l6_form_e0_goalxcornersxbookings,collapse = '')
    #l6_form_e0_goalxcornersxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_e0_goalxcornersxbookings_flattened),"")[[1]])
    final_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings,index_e0_goalxcornersxbookings_cols] <- l6_form_e0_goalxcornersxbookings[index_e0_goalxcornersxbookings_cols]
  }
}

final_e0_goalxcornersxbookings[is.na(final_e0_goalxcornersxbookings)] <- ""
e0_goalxcornersxbookingsmatrix <- cbind(e0_teams,final_e0_goalxcornersxbookings,suml6_e0_goalxcornersxbookings,sum_e0_zero_goalxcornersxbookings,sum_e0_one_goalxcornersxbookings,sum_e0_two_goalxcornersxbookings,sum_e0_three_goalxcornersxbookings,avgr_e0_goalxcornersxbookings,sdr_e0_goalxcornersxbookings)
write.xlsx(e0_goalxcornersxbookingsmatrix,"Analytics/UCL/E0ucl.xlsx", sheetName = "goalxcornersxbookings", append = TRUE)
###########################################################################################################################################################################################
###########################################################################################################################################################################################

#D1

UCL_analytics <- readxl::read_excel('UCL20242025.xlsx')
UCL_analytics <- UCL_analytics[,-1]
UCL_analytics <- as.data.frame(UCL_analytics)
D1_analytics <- subset(UCL_analytics, Div == "D1")

#create home and away matrices
d1_goalmins_h <- tapply(D1_analytics$goalmins, D1_analytics[c("HomeTeam", "Date")],mean)
d1_goalmins_a <- tapply(D1_analytics$goalmins, D1_analytics[c("AwayTeam", "Date")],mean)

d1_goalmins_h[is.na(d1_goalmins_h)] <- ""
d1_goalmins_a[is.na(d1_goalmins_a)] <- ""

for(d1_rowhgs in 1:nrow(d1_goalmins_h)) {
  for(d1_colhgs in 1:ncol(d1_goalmins_h)) {

    # print(my_matrix[row, col])
    for(d1_rowags in 1:nrow(d1_goalmins_a)) {
      for(d1_colags in 1:ncol(d1_goalmins_a)) {
        ifelse(!d1_goalmins_a[d1_rowags,d1_colags]=="",d1_goalmins_h[d1_rowags,d1_colags] <- d1_goalmins_a[d1_rowags,d1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

source("divisions.R")
source("goaltotalsv2.R")
source("Matchday.R")
source("KROUNDS.R")

final_d1_goalmins <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_goalmins <- c()
sum_d1_zero_goalmins <- c()
sum_d1_one_goalmins <- c()
sum_d1_two_goalmins <- c()
sum_d1_three_goalmins <- c()
avgr_d1_goalmins <- c()
sdr_d1_goalmins <- c()
l6_form_d1_goalminssplitted <- c()
form_d1_goalmins <- c()
for(index_d1_goalmins in 1:length(d1_teams))
{
  for(index_d1_goalmins_cols in 1:d1_totalrounds)
  {
    index_d1_goalmins  <- row.names(d1_goalmins_h) == d1_teams[index_d1_goalmins]
    form_d1_goalmins <- d1_goalmins_h[index_d1_goalmins ]
    deleted_form_d1_goalmins <- form_d1_goalmins[!form_d1_goalmins[] == ""]
    l6_form_d1_goalmins <- tail(deleted_form_d1_goalmins,d1_last_n_games)
    l6_form_d1_goalmins <- as.numeric(l6_form_d1_goalmins)
    suml6_d1_goalmins[index_d1_goalmins] <- sum(l6_form_d1_goalmins)
    suml6_d1_goalmins[index_d1_goalmins] <- paste(suml6_d1_goalmins[index_d1_goalmins],sep = "")
    sum_d1_zero_goalmins[index_d1_goalmins] <- length(which(l6_form_d1_goalmins == 0))
    sum_d1_zero_goalmins[index_d1_goalmins] <- paste(sum_d1_zero_goalmins[index_d1_goalmins],sep = "")
    sum_d1_one_goalmins[index_d1_goalmins] <- length(which(l6_form_d1_goalmins >= 100))
    sum_d1_one_goalmins[index_d1_goalmins] <- paste(sum_d1_one_goalmins[index_d1_goalmins],sep = "")
    sum_d1_two_goalmins[index_d1_goalmins] <- length(which(l6_form_d1_goalmins >= 200))
    sum_d1_two_goalmins[index_d1_goalmins] <- paste(sum_d1_two_goalmins[index_d1_goalmins],sep = "")
    sum_d1_three_goalmins[index_d1_goalmins] <- length(which(l6_form_d1_goalmins >= 300))
    sum_d1_three_goalmins[index_d1_goalmins] <- paste(sum_d1_three_goalmins[index_d1_goalmins],sep = "")
    avgr_d1_goalmins[index_d1_goalmins] <- mean(l6_form_d1_goalmins)
    avgr_d1_goalmins[index_d1_goalmins] <- paste(avgr_d1_goalmins[index_d1_goalmins],sep = "")
    sdr_d1_goalmins[index_d1_goalmins] <- sd(l6_form_d1_goalmins)
    sdr_d1_goalmins[index_d1_goalmins] <- paste(sdr_d1_goalmins[index_d1_goalmins],sep = "")
    l6_form_d1_goalmins <- as.character(l6_form_d1_goalmins)
    #l6_form_d1_goalmins_flattened <- stri_paste(l6_form_d1_goalmins,collapse = '')
    #l6_form_d1_goalminssplitted <- as.numeric(strsplit(as.character(l6_form_d1_goalmins_flattened),"")[[1]])
    final_d1_goalmins[index_d1_goalmins,index_d1_goalmins_cols] <- l6_form_d1_goalmins[index_d1_goalmins_cols]
  }
}

final_d1_goalmins[is.na(final_d1_goalmins)] <- ""
d1_goalminsmatrix <- cbind(d1_teams,final_d1_goalmins,suml6_d1_goalmins,sum_d1_zero_goalmins,sum_d1_one_goalmins,sum_d1_two_goalmins,sum_d1_three_goalmins,avgr_d1_goalmins,sdr_d1_goalmins)
unlink('Analytics/UCL/D1ucl.xlsx')
write.xlsx(d1_goalminsmatrix,"Analytics/UCL/D1ucl.xlsx", sheetName = "goalmins")
##############################################################################################################################################################################################

#create home and away matrices
d1_shirts_h <- tapply(D1_analytics$shirts, D1_analytics[c("HomeTeam", "Date")],mean)
d1_shirts_a <- tapply(D1_analytics$shirts, D1_analytics[c("AwayTeam", "Date")],mean)

d1_shirts_h[is.na(d1_shirts_h)] <- ""
d1_shirts_a[is.na(d1_shirts_a)] <- ""

for(d1_rowhgs in 1:nrow(d1_shirts_h)) {
  for(d1_colhgs in 1:ncol(d1_shirts_h)) {

    # print(my_matrix[row, col])
    for(d1_rowags in 1:nrow(d1_shirts_a)) {
      for(d1_colags in 1:ncol(d1_shirts_a)) {
        ifelse(!d1_shirts_a[d1_rowags,d1_colags]=="",d1_shirts_h[d1_rowags,d1_colags] <- d1_shirts_a[d1_rowags,d1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_d1_shirts <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_shirts <- c()
sum_d1_zero_shirts <- c()
sum_d1_one_shirts <- c()
sum_d1_two_shirts <- c()
sum_d1_three_shirts <- c()
avgr_d1_shirts <- c()
sdr_d1_shirts <- c()
l6_form_d1_shirtssplitted <- c()
form_d1_shirts <- c()
for(index_d1_shirts in 1:length(d1_teams))
{
  for(index_d1_shirts_cols in 1:d1_totalrounds)
  {
    index_d1_shirts  <- row.names(d1_shirts_h) == d1_teams[index_d1_shirts]
    form_d1_shirts <- d1_shirts_h[index_d1_shirts ]
    deleted_form_d1_shirts <- form_d1_shirts[!form_d1_shirts[] == ""]
    l6_form_d1_shirts <- tail(deleted_form_d1_shirts,d1_last_n_games)
    l6_form_d1_shirts <- as.numeric(l6_form_d1_shirts)
    suml6_d1_shirts[index_d1_shirts] <- sum(l6_form_d1_shirts)
    suml6_d1_shirts[index_d1_shirts] <- paste(suml6_d1_shirts[index_d1_shirts],sep = "")
    sum_d1_zero_shirts[index_d1_shirts] <- length(which(l6_form_d1_shirts == 0))
    sum_d1_zero_shirts[index_d1_shirts] <- paste(sum_d1_zero_shirts[index_d1_shirts],sep = "")
    sum_d1_one_shirts[index_d1_shirts] <- length(which(l6_form_d1_shirts >= 10))
    sum_d1_one_shirts[index_d1_shirts] <- paste(sum_d1_one_shirts[index_d1_shirts],sep = "")
    sum_d1_two_shirts[index_d1_shirts] <- length(which(l6_form_d1_shirts >= 20))
    sum_d1_two_shirts[index_d1_shirts] <- paste(sum_d1_two_shirts[index_d1_shirts],sep = "")
    sum_d1_three_shirts[index_d1_shirts] <- length(which(l6_form_d1_shirts >= 30))
    sum_d1_three_shirts[index_d1_shirts] <- paste(sum_d1_three_shirts[index_d1_shirts],sep = "")
    avgr_d1_shirts[index_d1_shirts] <- mean(l6_form_d1_shirts)
    avgr_d1_shirts[index_d1_shirts] <- paste(avgr_d1_shirts[index_d1_shirts],sep = "")
    sdr_d1_shirts[index_d1_shirts] <- sd(l6_form_d1_shirts)
    sdr_d1_shirts[index_d1_shirts] <- paste(sdr_d1_shirts[index_d1_shirts],sep = "")
    l6_form_d1_shirts <- as.character(l6_form_d1_shirts)
    #l6_form_d1_shirts_flattened <- stri_paste(l6_form_d1_shirts,collapse = '')
    #l6_form_d1_shirtssplitted <- as.numeric(strsplit(as.character(l6_form_d1_shirts_flattened),"")[[1]])
    final_d1_shirts[index_d1_shirts,index_d1_shirts_cols] <- l6_form_d1_shirts[index_d1_shirts_cols]
  }
}

final_d1_shirts[is.na(final_d1_shirts)] <- ""
d1_shirtsmatrix <- cbind(d1_teams,final_d1_shirts,suml6_d1_shirts,sum_d1_zero_shirts,sum_d1_one_shirts,sum_d1_two_shirts,sum_d1_three_shirts,avgr_d1_shirts,sdr_d1_shirts)
write.xlsx(d1_shirtsmatrix,"Analytics/UCL/D1ucl.xlsx", sheetName = "shirts", append = TRUE)
#######################################################################################################################################################################################

#create home and away matrices
d1_crossbookings_h <- tapply(D1_analytics$Crossbookings, D1_analytics[c("HomeTeam", "Date")],mean)
d1_crossbookings_a <- tapply(D1_analytics$Crossbookings, D1_analytics[c("AwayTeam", "Date")],mean)

d1_crossbookings_h[is.na(d1_crossbookings_h)] <- ""
d1_crossbookings_a[is.na(d1_crossbookings_a)] <- ""

for(d1_rowhgs in 1:nrow(d1_crossbookings_h)) {
  for(d1_colhgs in 1:ncol(d1_crossbookings_h)) {

    # print(my_matrix[row, col])
    for(d1_rowags in 1:nrow(d1_crossbookings_a)) {
      for(d1_colags in 1:ncol(d1_crossbookings_a)) {
        ifelse(!d1_crossbookings_a[d1_rowags,d1_colags]=="",d1_crossbookings_h[d1_rowags,d1_colags] <- d1_crossbookings_a[d1_rowags,d1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_d1_crossbookings <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_crossbookings <- c()
sum_d1_zero_crossbookings <- c()
sum_d1_one_crossbookings <- c()
sum_d1_two_crossbookings <- c()
sum_d1_three_crossbookings <- c()
avgr_d1_crossbookings <- c()
sdr_d1_crossbookings <- c()
l6_form_d1_crossbookingssplitted <- c()
form_d1_crossbookings <- c()
for(index_d1_crossbookings in 1:length(d1_teams))
{
  for(index_d1_crossbookings_cols in 1:d1_totalrounds)
  {
    index_d1_crossbookings  <- row.names(d1_crossbookings_h) == d1_teams[index_d1_crossbookings]
    form_d1_crossbookings <- d1_crossbookings_h[index_d1_crossbookings ]
    deleted_form_d1_crossbookings <- form_d1_crossbookings[!form_d1_crossbookings[] == ""]
    l6_form_d1_crossbookings <- tail(deleted_form_d1_crossbookings,d1_last_n_games)
    l6_form_d1_crossbookings <- as.numeric(l6_form_d1_crossbookings)
    suml6_d1_crossbookings[index_d1_crossbookings] <- sum(l6_form_d1_crossbookings)
    suml6_d1_crossbookings[index_d1_crossbookings] <- paste(suml6_d1_crossbookings[index_d1_crossbookings],sep = "")
    sum_d1_zero_crossbookings[index_d1_crossbookings] <- length(which(l6_form_d1_crossbookings == 0))
    sum_d1_zero_crossbookings[index_d1_crossbookings] <- paste(sum_d1_zero_crossbookings[index_d1_crossbookings],sep = "")
    sum_d1_one_crossbookings[index_d1_crossbookings] <- length(which(l6_form_d1_crossbookings >= 10))
    sum_d1_one_crossbookings[index_d1_crossbookings] <- paste(sum_d1_one_crossbookings[index_d1_crossbookings],sep = "")
    sum_d1_two_crossbookings[index_d1_crossbookings] <- length(which(l6_form_d1_crossbookings >= 20))
    sum_d1_two_crossbookings[index_d1_crossbookings] <- paste(sum_d1_two_crossbookings[index_d1_crossbookings],sep = "")
    sum_d1_three_crossbookings[index_d1_crossbookings] <- length(which(l6_form_d1_crossbookings >= 30))
    sum_d1_three_crossbookings[index_d1_crossbookings] <- paste(sum_d1_three_crossbookings[index_d1_crossbookings],sep = "")
    avgr_d1_crossbookings[index_d1_crossbookings] <- mean(l6_form_d1_crossbookings)
    avgr_d1_crossbookings[index_d1_crossbookings] <- paste(avgr_d1_crossbookings[index_d1_crossbookings],sep = "")
    sdr_d1_crossbookings[index_d1_crossbookings] <- sd(l6_form_d1_crossbookings)
    sdr_d1_crossbookings[index_d1_crossbookings] <- paste(sdr_d1_crossbookings[index_d1_crossbookings],sep = "")
    l6_form_d1_crossbookings <- as.character(l6_form_d1_crossbookings)
    #l6_form_d1_crossbookings_flattened <- stri_paste(l6_form_d1_crossbookings,collapse = '')
    #l6_form_d1_crossbookingssplitted <- as.numeric(strsplit(as.character(l6_form_d1_crossbookings_flattened),"")[[1]])
    final_d1_crossbookings[index_d1_crossbookings,index_d1_crossbookings_cols] <- l6_form_d1_crossbookings[index_d1_crossbookings_cols]
  }
}

final_d1_crossbookings[is.na(final_d1_crossbookings)] <- ""
d1_crossbookingsmatrix <- cbind(d1_teams,final_d1_crossbookings,suml6_d1_crossbookings,sum_d1_zero_crossbookings,sum_d1_one_crossbookings,sum_d1_two_crossbookings,sum_d1_three_crossbookings,avgr_d1_crossbookings,sdr_d1_crossbookings)
write.xlsx(d1_crossbookingsmatrix,"Analytics/UCL/D1ucl.xlsx", sheetName = "crossbookings", append = TRUE)
######################################################################################################################################################################################

#create home and away matrices
d1_shirtsxbookings_h <- tapply(D1_analytics$ShirtsXbookings, D1_analytics[c("HomeTeam", "Date")],mean)
d1_shirtsxbookings_a <- tapply(D1_analytics$ShirtsXbookings, D1_analytics[c("AwayTeam", "Date")],mean)

d1_shirtsxbookings_h[is.na(d1_shirtsxbookings_h)] <- ""
d1_shirtsxbookings_a[is.na(d1_shirtsxbookings_a)] <- ""

for(d1_rowhgs in 1:nrow(d1_shirtsxbookings_h)) {
  for(d1_colhgs in 1:ncol(d1_shirtsxbookings_h)) {

    # print(my_matrix[row, col])
    for(d1_rowags in 1:nrow(d1_shirtsxbookings_a)) {
      for(d1_colags in 1:ncol(d1_shirtsxbookings_a)) {
        ifelse(!d1_shirtsxbookings_a[d1_rowags,d1_colags]=="",d1_shirtsxbookings_h[d1_rowags,d1_colags] <- d1_shirtsxbookings_a[d1_rowags,d1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_d1_shirtsxbookings <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_shirtsxbookings <- c()
sum_d1_zero_shirtsxbookings <- c()
sum_d1_one_shirtsxbookings <- c()
sum_d1_two_shirtsxbookings <- c()
sum_d1_three_shirtsxbookings <- c()
avgr_d1_shirtsxbookings <- c()
sdr_d1_shirtsxbookings <- c()
l6_form_d1_shirtsxbookingssplitted <- c()
form_d1_shirtsxbookings <- c()
for(index_d1_shirtsxbookings in 1:length(d1_teams))
{
  for(index_d1_shirtsxbookings_cols in 1:d1_totalrounds)
  {
    index_d1_shirtsxbookings  <- row.names(d1_shirtsxbookings_h) == d1_teams[index_d1_shirtsxbookings]
    form_d1_shirtsxbookings <- d1_shirtsxbookings_h[index_d1_shirtsxbookings ]
    deleted_form_d1_shirtsxbookings <- form_d1_shirtsxbookings[!form_d1_shirtsxbookings[] == ""]
    l6_form_d1_shirtsxbookings <- tail(deleted_form_d1_shirtsxbookings,d1_last_n_games)
    l6_form_d1_shirtsxbookings <- as.numeric(l6_form_d1_shirtsxbookings)
    suml6_d1_shirtsxbookings[index_d1_shirtsxbookings] <- sum(l6_form_d1_shirtsxbookings)
    suml6_d1_shirtsxbookings[index_d1_shirtsxbookings] <- paste(suml6_d1_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    sum_d1_zero_shirtsxbookings[index_d1_shirtsxbookings] <- length(which(l6_form_d1_shirtsxbookings == 0))
    sum_d1_zero_shirtsxbookings[index_d1_shirtsxbookings] <- paste(sum_d1_zero_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    sum_d1_one_shirtsxbookings[index_d1_shirtsxbookings] <- length(which(l6_form_d1_shirtsxbookings >= 10))
    sum_d1_one_shirtsxbookings[index_d1_shirtsxbookings] <- paste(sum_d1_one_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    sum_d1_two_shirtsxbookings[index_d1_shirtsxbookings] <- length(which(l6_form_d1_shirtsxbookings >= 20))
    sum_d1_two_shirtsxbookings[index_d1_shirtsxbookings] <- paste(sum_d1_two_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    sum_d1_three_shirtsxbookings[index_d1_shirtsxbookings] <- length(which(l6_form_d1_shirtsxbookings >= 30))
    sum_d1_three_shirtsxbookings[index_d1_shirtsxbookings] <- paste(sum_d1_three_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    avgr_d1_shirtsxbookings[index_d1_shirtsxbookings] <- mean(l6_form_d1_shirtsxbookings)
    avgr_d1_shirtsxbookings[index_d1_shirtsxbookings] <- paste(avgr_d1_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    sdr_d1_shirtsxbookings[index_d1_shirtsxbookings] <- sd(l6_form_d1_shirtsxbookings)
    sdr_d1_shirtsxbookings[index_d1_shirtsxbookings] <- paste(sdr_d1_shirtsxbookings[index_d1_shirtsxbookings],sep = "")
    l6_form_d1_shirtsxbookings <- as.character(l6_form_d1_shirtsxbookings)
    #l6_form_d1_shirtsxbookings_flattened <- stri_paste(l6_form_d1_shirtsxbookings,collapse = '')
    #l6_form_d1_shirtsxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_d1_shirtsxbookings_flattened),"")[[1]])
    final_d1_shirtsxbookings[index_d1_shirtsxbookings,index_d1_shirtsxbookings_cols] <- l6_form_d1_shirtsxbookings[index_d1_shirtsxbookings_cols]
  }
}

final_d1_shirtsxbookings[is.na(final_d1_shirtsxbookings)] <- ""
d1_shirtsxbookingsmatrix <- cbind(d1_teams,final_d1_shirtsxbookings,suml6_d1_shirtsxbookings,sum_d1_zero_shirtsxbookings,sum_d1_one_shirtsxbookings,sum_d1_two_shirtsxbookings,sum_d1_three_shirtsxbookings,avgr_d1_shirtsxbookings,sdr_d1_shirtsxbookings)
write.xlsx(d1_shirtsxbookingsmatrix,"Analytics/UCL/D1ucl.xlsx", sheetName = "shirtsxbookings", append = TRUE)
##################################################################################################################################################################################

#create home and away matrices
d1_tgmxcorners_h <- tapply(D1_analytics$TGMXcorners, D1_analytics[c("HomeTeam", "Date")],mean)
d1_tgmxcorners_a <- tapply(D1_analytics$TGMXcorners, D1_analytics[c("AwayTeam", "Date")],mean)

d1_tgmxcorners_h[is.na(d1_tgmxcorners_h)] <- ""
d1_tgmxcorners_a[is.na(d1_tgmxcorners_a)] <- ""

for(d1_rowhgs in 1:nrow(d1_tgmxcorners_h)) {
  for(d1_colhgs in 1:ncol(d1_tgmxcorners_h)) {

    # print(my_matrix[row, col])
    for(d1_rowags in 1:nrow(d1_tgmxcorners_a)) {
      for(d1_colags in 1:ncol(d1_tgmxcorners_a)) {
        ifelse(!d1_tgmxcorners_a[d1_rowags,d1_colags]=="",d1_tgmxcorners_h[d1_rowags,d1_colags] <- d1_tgmxcorners_a[d1_rowags,d1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_d1_tgmxcorners <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_tgmxcorners <- c()
sum_d1_zero_tgmxcorners <- c()
sum_d1_one_tgmxcorners <- c()
sum_d1_two_tgmxcorners <- c()
sum_d1_three_tgmxcorners <- c()
avgr_d1_tgmxcorners <- c()
sdr_d1_tgmxcorners <- c()
l6_form_d1_tgmxcornerssplitted <- c()
form_d1_tgmxcorners <- c()
for(index_d1_tgmxcorners in 1:length(d1_teams))
{
  for(index_d1_tgmxcorners_cols in 1:d1_totalrounds)
  {
    index_d1_tgmxcorners  <- row.names(d1_tgmxcorners_h) == d1_teams[index_d1_tgmxcorners]
    form_d1_tgmxcorners <- d1_tgmxcorners_h[index_d1_tgmxcorners ]
    deleted_form_d1_tgmxcorners <- form_d1_tgmxcorners[!form_d1_tgmxcorners[] == ""]
    l6_form_d1_tgmxcorners <- tail(deleted_form_d1_tgmxcorners,d1_last_n_games)
    l6_form_d1_tgmxcorners <- as.numeric(l6_form_d1_tgmxcorners)
    suml6_d1_tgmxcorners[index_d1_tgmxcorners] <- sum(l6_form_d1_tgmxcorners)
    suml6_d1_tgmxcorners[index_d1_tgmxcorners] <- paste(suml6_d1_tgmxcorners[index_d1_tgmxcorners],sep = "")
    sum_d1_zero_tgmxcorners[index_d1_tgmxcorners] <- length(which(l6_form_d1_tgmxcorners == 0))
    sum_d1_zero_tgmxcorners[index_d1_tgmxcorners] <- paste(sum_d1_zero_tgmxcorners[index_d1_tgmxcorners],sep = "")
    sum_d1_one_tgmxcorners[index_d1_tgmxcorners] <- length(which(l6_form_d1_tgmxcorners >= 10))
    sum_d1_one_tgmxcorners[index_d1_tgmxcorners] <- paste(sum_d1_one_tgmxcorners[index_d1_tgmxcorners],sep = "")
    sum_d1_two_tgmxcorners[index_d1_tgmxcorners] <- length(which(l6_form_d1_tgmxcorners >= 20))
    sum_d1_two_tgmxcorners[index_d1_tgmxcorners] <- paste(sum_d1_two_tgmxcorners[index_d1_tgmxcorners],sep = "")
    sum_d1_three_tgmxcorners[index_d1_tgmxcorners] <- length(which(l6_form_d1_tgmxcorners >= 30))
    sum_d1_three_tgmxcorners[index_d1_tgmxcorners] <- paste(sum_d1_three_tgmxcorners[index_d1_tgmxcorners],sep = "")
    avgr_d1_tgmxcorners[index_d1_tgmxcorners] <- mean(l6_form_d1_tgmxcorners)
    avgr_d1_tgmxcorners[index_d1_tgmxcorners] <- paste(avgr_d1_tgmxcorners[index_d1_tgmxcorners],sep = "")
    sdr_d1_tgmxcorners[index_d1_tgmxcorners] <- sd(l6_form_d1_tgmxcorners)
    sdr_d1_tgmxcorners[index_d1_tgmxcorners] <- paste(sdr_d1_tgmxcorners[index_d1_tgmxcorners],sep = "")
    l6_form_d1_tgmxcorners <- as.character(l6_form_d1_tgmxcorners)
    #l6_form_d1_tgmxcorners_flattened <- stri_paste(l6_form_d1_tgmxcorners,collapse = '')
    #l6_form_d1_tgmxcornerssplitted <- as.numeric(strsplit(as.character(l6_form_d1_tgmxcorners_flattened),"")[[1]])
    final_d1_tgmxcorners[index_d1_tgmxcorners,index_d1_tgmxcorners_cols] <- l6_form_d1_tgmxcorners[index_d1_tgmxcorners_cols]
  }
}

final_d1_tgmxcorners[is.na(final_d1_tgmxcorners)] <- ""
d1_tgmxcornersmatrix <- cbind(d1_teams,final_d1_tgmxcorners,suml6_d1_tgmxcorners,sum_d1_zero_tgmxcorners,sum_d1_one_tgmxcorners,sum_d1_two_tgmxcorners,sum_d1_three_tgmxcorners,avgr_d1_tgmxcorners,sdr_d1_tgmxcorners)
write.xlsx(d1_tgmxcornersmatrix,"Analytics/UCL/D1ucl.xlsx", sheetName = "tgmxcorners", append = TRUE)
########################################################################################################################################################################################################################

#create home and away matrices
d1_goalxcornersxbookings_h <- tapply(D1_analytics$GoalsXcornerXbookings, D1_analytics[c("HomeTeam", "Date")],mean)
d1_goalxcornersxbookings_a <- tapply(D1_analytics$GoalsXcornerXbookings, D1_analytics[c("AwayTeam", "Date")],mean)

d1_goalxcornersxbookings_h[is.na(d1_goalxcornersxbookings_h)] <- ""
d1_goalxcornersxbookings_a[is.na(d1_goalxcornersxbookings_a)] <- ""

for(d1_rowhgs in 1:nrow(d1_goalxcornersxbookings_h)) {
  for(d1_colhgs in 1:ncol(d1_goalxcornersxbookings_h)) {

    # print(my_matrix[row, col])
    for(d1_rowags in 1:nrow(d1_goalxcornersxbookings_a)) {
      for(d1_colags in 1:ncol(d1_goalxcornersxbookings_a)) {
        ifelse(!d1_goalxcornersxbookings_a[d1_rowags,d1_colags]=="",d1_goalxcornersxbookings_h[d1_rowags,d1_colags] <- d1_goalxcornersxbookings_a[d1_rowags,d1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_d1_goalxcornersxbookings <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_goalxcornersxbookings <- c()
sum_d1_zero_goalxcornersxbookings <- c()
sum_d1_one_goalxcornersxbookings <- c()
sum_d1_two_goalxcornersxbookings <- c()
sum_d1_three_goalxcornersxbookings <- c()
avgr_d1_goalxcornersxbookings <- c()
sdr_d1_goalxcornersxbookings <- c()
l6_form_d1_goalxcornersxbookingssplitted <- c()
form_d1_goalxcornersxbookings <- c()
for(index_d1_goalxcornersxbookings in 1:length(d1_teams))
{
  for(index_d1_goalxcornersxbookings_cols in 1:d1_totalrounds)
  {
    index_d1_goalxcornersxbookings  <- row.names(d1_goalxcornersxbookings_h) == d1_teams[index_d1_goalxcornersxbookings]
    form_d1_goalxcornersxbookings <- d1_goalxcornersxbookings_h[index_d1_goalxcornersxbookings ]
    deleted_form_d1_goalxcornersxbookings <- form_d1_goalxcornersxbookings[!form_d1_goalxcornersxbookings[] == ""]
    l6_form_d1_goalxcornersxbookings <- tail(deleted_form_d1_goalxcornersxbookings,d1_last_n_games)
    l6_form_d1_goalxcornersxbookings <- as.numeric(l6_form_d1_goalxcornersxbookings)
    suml6_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- sum(l6_form_d1_goalxcornersxbookings)
    suml6_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(suml6_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    sum_d1_zero_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- length(which(l6_form_d1_goalxcornersxbookings == 0))
    sum_d1_zero_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(sum_d1_zero_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    sum_d1_one_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- length(which(l6_form_d1_goalxcornersxbookings >= 10))
    sum_d1_one_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(sum_d1_one_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    sum_d1_two_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- length(which(l6_form_d1_goalxcornersxbookings >= 20))
    sum_d1_two_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(sum_d1_two_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    sum_d1_three_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- length(which(l6_form_d1_goalxcornersxbookings >= 30))
    sum_d1_three_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(sum_d1_three_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    avgr_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- mean(l6_form_d1_goalxcornersxbookings)
    avgr_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(avgr_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    sdr_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- sd(l6_form_d1_goalxcornersxbookings)
    sdr_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings] <- paste(sdr_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings],sep = "")
    l6_form_d1_goalxcornersxbookings <- as.character(l6_form_d1_goalxcornersxbookings)
    #l6_form_d1_goalxcornersxbookings_flattened <- stri_paste(l6_form_d1_goalxcornersxbookings,collapse = '')
    #l6_form_d1_goalxcornersxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_d1_goalxcornersxbookings_flattened),"")[[1]])
    final_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings,index_d1_goalxcornersxbookings_cols] <- l6_form_d1_goalxcornersxbookings[index_d1_goalxcornersxbookings_cols]
  }
}

final_d1_goalxcornersxbookings[is.na(final_d1_goalxcornersxbookings)] <- ""
d1_goalxcornersxbookingsmatrix <- cbind(d1_teams,final_d1_goalxcornersxbookings,suml6_d1_goalxcornersxbookings,sum_d1_zero_goalxcornersxbookings,sum_d1_one_goalxcornersxbookings,sum_d1_two_goalxcornersxbookings,sum_d1_three_goalxcornersxbookings,avgr_d1_goalxcornersxbookings,sdr_d1_goalxcornersxbookings)
write.xlsx(d1_goalxcornersxbookingsmatrix,"Analytics/UCL/D1ucl.xlsx", sheetName = "goalxcornersxbookings", append = TRUE)
###########################################################################################################################################################################################
###########################################################################################################################################################################################

UCL_analytics <- readxl::read_excel('UCL20242025.xlsx')
UCL_analytics <- UCL_analytics[,-1]
UCL_analytics <- as.data.frame(UCL_analytics)
I1_analytics <- subset(UCL_analytics, Div == "I1")

#create home and away matrices
i1_goalmins_h <- tapply(I1_analytics$goalmins, I1_analytics[c("HomeTeam", "Date")],mean)
i1_goalmins_a <- tapply(I1_analytics$goalmins, I1_analytics[c("AwayTeam", "Date")],mean)

i1_goalmins_h[is.na(i1_goalmins_h)] <- ""
i1_goalmins_a[is.na(i1_goalmins_a)] <- ""

for(i1_rowhgs in 1:nrow(i1_goalmins_h)) {
  for(i1_colhgs in 1:ncol(i1_goalmins_h)) {

    # print(my_matrix[row, col])
    for(i1_rowags in 1:nrow(i1_goalmins_a)) {
      for(i1_colags in 1:ncol(i1_goalmins_a)) {
        ifelse(!i1_goalmins_a[i1_rowags,i1_colags]=="",i1_goalmins_h[i1_rowags,i1_colags] <- i1_goalmins_a[i1_rowags,i1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

source("divisions.R")
source("goaltotalsv2.R")
source("Matchday.R")
source("KROUNDS.R")

final_i1_goalmins <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_goalmins <- c()
sum_i1_zero_goalmins <- c()
sum_i1_one_goalmins <- c()
sum_i1_two_goalmins <- c()
sum_i1_three_goalmins <- c()
avgr_i1_goalmins <- c()
sdr_i1_goalmins <- c()
l6_form_i1_goalminssplitted <- c()
form_i1_goalmins <- c()
for(index_i1_goalmins in 1:length(i1_teams))
{
  for(index_i1_goalmins_cols in 1:i1_totalrounds)
  {
    index_i1_goalmins  <- row.names(i1_goalmins_h) == i1_teams[index_i1_goalmins]
    form_i1_goalmins <- i1_goalmins_h[index_i1_goalmins ]
    deleted_form_i1_goalmins <- form_i1_goalmins[!form_i1_goalmins[] == ""]
    l6_form_i1_goalmins <- tail(deleted_form_i1_goalmins,i1_last_n_games)
    l6_form_i1_goalmins <- as.numeric(l6_form_i1_goalmins)
    suml6_i1_goalmins[index_i1_goalmins] <- sum(l6_form_i1_goalmins)
    suml6_i1_goalmins[index_i1_goalmins] <- paste(suml6_i1_goalmins[index_i1_goalmins],sep = "")
    sum_i1_zero_goalmins[index_i1_goalmins] <- length(which(l6_form_i1_goalmins == 0))
    sum_i1_zero_goalmins[index_i1_goalmins] <- paste(sum_i1_zero_goalmins[index_i1_goalmins],sep = "")
    sum_i1_one_goalmins[index_i1_goalmins] <- length(which(l6_form_i1_goalmins >= 100))
    sum_i1_one_goalmins[index_i1_goalmins] <- paste(sum_i1_one_goalmins[index_i1_goalmins],sep = "")
    sum_i1_two_goalmins[index_i1_goalmins] <- length(which(l6_form_i1_goalmins >= 200))
    sum_i1_two_goalmins[index_i1_goalmins] <- paste(sum_i1_two_goalmins[index_i1_goalmins],sep = "")
    sum_i1_three_goalmins[index_i1_goalmins] <- length(which(l6_form_i1_goalmins >= 300))
    sum_i1_three_goalmins[index_i1_goalmins] <- paste(sum_i1_three_goalmins[index_i1_goalmins],sep = "")
    avgr_i1_goalmins[index_i1_goalmins] <- mean(l6_form_i1_goalmins)
    avgr_i1_goalmins[index_i1_goalmins] <- paste(avgr_i1_goalmins[index_i1_goalmins],sep = "")
    sdr_i1_goalmins[index_i1_goalmins] <- sd(l6_form_i1_goalmins)
    sdr_i1_goalmins[index_i1_goalmins] <- paste(sdr_i1_goalmins[index_i1_goalmins],sep = "")
    l6_form_i1_goalmins <- as.character(l6_form_i1_goalmins)
    #l6_form_i1_goalmins_flattened <- stri_paste(l6_form_i1_goalmins,collapse = '')
    #l6_form_i1_goalminssplitted <- as.numeric(strsplit(as.character(l6_form_i1_goalmins_flattened),"")[[1]])
    final_i1_goalmins[index_i1_goalmins,index_i1_goalmins_cols] <- l6_form_i1_goalmins[index_i1_goalmins_cols]
  }
}

final_i1_goalmins[is.na(final_i1_goalmins)] <- ""
i1_goalminsmatrix <- cbind(i1_teams,final_i1_goalmins,suml6_i1_goalmins,sum_i1_zero_goalmins,sum_i1_one_goalmins,sum_i1_two_goalmins,sum_i1_three_goalmins,avgr_i1_goalmins,sdr_i1_goalmins)
unlink('Analytics/UCL/I1ucl.xlsx')
write.xlsx(i1_goalminsmatrix,"Analytics/UCL/I1ucl.xlsx", sheetName = "goalmins")
##############################################################################################################################################################################################

#create home and away matrices
i1_shirts_h <- tapply(I1_analytics$shirts, I1_analytics[c("HomeTeam", "Date")],mean)
i1_shirts_a <- tapply(I1_analytics$shirts, I1_analytics[c("AwayTeam", "Date")],mean)

i1_shirts_h[is.na(i1_shirts_h)] <- ""
i1_shirts_a[is.na(i1_shirts_a)] <- ""

for(i1_rowhgs in 1:nrow(i1_shirts_h)) {
  for(i1_colhgs in 1:ncol(i1_shirts_h)) {

    # print(my_matrix[row, col])
    for(i1_rowags in 1:nrow(i1_shirts_a)) {
      for(i1_colags in 1:ncol(i1_shirts_a)) {
        ifelse(!i1_shirts_a[i1_rowags,i1_colags]=="",i1_shirts_h[i1_rowags,i1_colags] <- i1_shirts_a[i1_rowags,i1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_i1_shirts <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_shirts <- c()
sum_i1_zero_shirts <- c()
sum_i1_one_shirts <- c()
sum_i1_two_shirts <- c()
sum_i1_three_shirts <- c()
avgr_i1_shirts <- c()
sdr_i1_shirts <- c()
l6_form_i1_shirtssplitted <- c()
form_i1_shirts <- c()
for(index_i1_shirts in 1:length(i1_teams))
{
  for(index_i1_shirts_cols in 1:i1_totalrounds)
  {
    index_i1_shirts  <- row.names(i1_shirts_h) == i1_teams[index_i1_shirts]
    form_i1_shirts <- i1_shirts_h[index_i1_shirts ]
    deleted_form_i1_shirts <- form_i1_shirts[!form_i1_shirts[] == ""]
    l6_form_i1_shirts <- tail(deleted_form_i1_shirts,i1_last_n_games)
    l6_form_i1_shirts <- as.numeric(l6_form_i1_shirts)
    suml6_i1_shirts[index_i1_shirts] <- sum(l6_form_i1_shirts)
    suml6_i1_shirts[index_i1_shirts] <- paste(suml6_i1_shirts[index_i1_shirts],sep = "")
    sum_i1_zero_shirts[index_i1_shirts] <- length(which(l6_form_i1_shirts == 0))
    sum_i1_zero_shirts[index_i1_shirts] <- paste(sum_i1_zero_shirts[index_i1_shirts],sep = "")
    sum_i1_one_shirts[index_i1_shirts] <- length(which(l6_form_i1_shirts >= 10))
    sum_i1_one_shirts[index_i1_shirts] <- paste(sum_i1_one_shirts[index_i1_shirts],sep = "")
    sum_i1_two_shirts[index_i1_shirts] <- length(which(l6_form_i1_shirts >= 20))
    sum_i1_two_shirts[index_i1_shirts] <- paste(sum_i1_two_shirts[index_i1_shirts],sep = "")
    sum_i1_three_shirts[index_i1_shirts] <- length(which(l6_form_i1_shirts >= 30))
    sum_i1_three_shirts[index_i1_shirts] <- paste(sum_i1_three_shirts[index_i1_shirts],sep = "")
    avgr_i1_shirts[index_i1_shirts] <- mean(l6_form_i1_shirts)
    avgr_i1_shirts[index_i1_shirts] <- paste(avgr_i1_shirts[index_i1_shirts],sep = "")
    sdr_i1_shirts[index_i1_shirts] <- sd(l6_form_i1_shirts)
    sdr_i1_shirts[index_i1_shirts] <- paste(sdr_i1_shirts[index_i1_shirts],sep = "")
    l6_form_i1_shirts <- as.character(l6_form_i1_shirts)
    #l6_form_i1_shirts_flattened <- stri_paste(l6_form_i1_shirts,collapse = '')
    #l6_form_i1_shirtssplitted <- as.numeric(strsplit(as.character(l6_form_i1_shirts_flattened),"")[[1]])
    final_i1_shirts[index_i1_shirts,index_i1_shirts_cols] <- l6_form_i1_shirts[index_i1_shirts_cols]
  }
}

final_i1_shirts[is.na(final_i1_shirts)] <- ""
i1_shirtsmatrix <- cbind(i1_teams,final_i1_shirts,suml6_i1_shirts,sum_i1_zero_shirts,sum_i1_one_shirts,sum_i1_two_shirts,sum_i1_three_shirts,avgr_i1_shirts,sdr_i1_shirts)
write.xlsx(i1_shirtsmatrix,"Analytics/UCL/I1ucl.xlsx", sheetName = "shirts", append = TRUE)
#######################################################################################################################################################################################

#create home and away matrices
i1_crossbookings_h <- tapply(I1_analytics$Crossbookings, I1_analytics[c("HomeTeam", "Date")],mean)
i1_crossbookings_a <- tapply(I1_analytics$Crossbookings, I1_analytics[c("AwayTeam", "Date")],mean)

i1_crossbookings_h[is.na(i1_crossbookings_h)] <- ""
i1_crossbookings_a[is.na(i1_crossbookings_a)] <- ""

for(i1_rowhgs in 1:nrow(i1_crossbookings_h)) {
  for(i1_colhgs in 1:ncol(i1_crossbookings_h)) {

    # print(my_matrix[row, col])
    for(i1_rowags in 1:nrow(i1_crossbookings_a)) {
      for(i1_colags in 1:ncol(i1_crossbookings_a)) {
        ifelse(!i1_crossbookings_a[i1_rowags,i1_colags]=="",i1_crossbookings_h[i1_rowags,i1_colags] <- i1_crossbookings_a[i1_rowags,i1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_i1_crossbookings <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_crossbookings <- c()
sum_i1_zero_crossbookings <- c()
sum_i1_one_crossbookings <- c()
sum_i1_two_crossbookings <- c()
sum_i1_three_crossbookings <- c()
avgr_i1_crossbookings <- c()
sdr_i1_crossbookings <- c()
l6_form_i1_crossbookingssplitted <- c()
form_i1_crossbookings <- c()
for(index_i1_crossbookings in 1:length(i1_teams))
{
  for(index_i1_crossbookings_cols in 1:i1_totalrounds)
  {
    index_i1_crossbookings  <- row.names(i1_crossbookings_h) == i1_teams[index_i1_crossbookings]
    form_i1_crossbookings <- i1_crossbookings_h[index_i1_crossbookings ]
    deleted_form_i1_crossbookings <- form_i1_crossbookings[!form_i1_crossbookings[] == ""]
    l6_form_i1_crossbookings <- tail(deleted_form_i1_crossbookings,i1_last_n_games)
    l6_form_i1_crossbookings <- as.numeric(l6_form_i1_crossbookings)
    suml6_i1_crossbookings[index_i1_crossbookings] <- sum(l6_form_i1_crossbookings)
    suml6_i1_crossbookings[index_i1_crossbookings] <- paste(suml6_i1_crossbookings[index_i1_crossbookings],sep = "")
    sum_i1_zero_crossbookings[index_i1_crossbookings] <- length(which(l6_form_i1_crossbookings == 0))
    sum_i1_zero_crossbookings[index_i1_crossbookings] <- paste(sum_i1_zero_crossbookings[index_i1_crossbookings],sep = "")
    sum_i1_one_crossbookings[index_i1_crossbookings] <- length(which(l6_form_i1_crossbookings >= 10))
    sum_i1_one_crossbookings[index_i1_crossbookings] <- paste(sum_i1_one_crossbookings[index_i1_crossbookings],sep = "")
    sum_i1_two_crossbookings[index_i1_crossbookings] <- length(which(l6_form_i1_crossbookings >= 20))
    sum_i1_two_crossbookings[index_i1_crossbookings] <- paste(sum_i1_two_crossbookings[index_i1_crossbookings],sep = "")
    sum_i1_three_crossbookings[index_i1_crossbookings] <- length(which(l6_form_i1_crossbookings >= 30))
    sum_i1_three_crossbookings[index_i1_crossbookings] <- paste(sum_i1_three_crossbookings[index_i1_crossbookings],sep = "")
    avgr_i1_crossbookings[index_i1_crossbookings] <- mean(l6_form_i1_crossbookings)
    avgr_i1_crossbookings[index_i1_crossbookings] <- paste(avgr_i1_crossbookings[index_i1_crossbookings],sep = "")
    sdr_i1_crossbookings[index_i1_crossbookings] <- sd(l6_form_i1_crossbookings)
    sdr_i1_crossbookings[index_i1_crossbookings] <- paste(sdr_i1_crossbookings[index_i1_crossbookings],sep = "")
    l6_form_i1_crossbookings <- as.character(l6_form_i1_crossbookings)
    #l6_form_i1_crossbookings_flattened <- stri_paste(l6_form_i1_crossbookings,collapse = '')
    #l6_form_i1_crossbookingssplitted <- as.numeric(strsplit(as.character(l6_form_i1_crossbookings_flattened),"")[[1]])
    final_i1_crossbookings[index_i1_crossbookings,index_i1_crossbookings_cols] <- l6_form_i1_crossbookings[index_i1_crossbookings_cols]
  }
}

final_i1_crossbookings[is.na(final_i1_crossbookings)] <- ""
i1_crossbookingsmatrix <- cbind(i1_teams,final_i1_crossbookings,suml6_i1_crossbookings,sum_i1_zero_crossbookings,sum_i1_one_crossbookings,sum_i1_two_crossbookings,sum_i1_three_crossbookings,avgr_i1_crossbookings,sdr_i1_crossbookings)
write.xlsx(i1_crossbookingsmatrix,"Analytics/UCL/I1ucl.xlsx", sheetName = "crossbookings", append = TRUE)
######################################################################################################################################################################################

#create home and away matrices
i1_shirtsxbookings_h <- tapply(I1_analytics$ShirtsXbookings, I1_analytics[c("HomeTeam", "Date")],mean)
i1_shirtsxbookings_a <- tapply(I1_analytics$ShirtsXbookings, I1_analytics[c("AwayTeam", "Date")],mean)

i1_shirtsxbookings_h[is.na(i1_shirtsxbookings_h)] <- ""
i1_shirtsxbookings_a[is.na(i1_shirtsxbookings_a)] <- ""

for(i1_rowhgs in 1:nrow(i1_shirtsxbookings_h)) {
  for(i1_colhgs in 1:ncol(i1_shirtsxbookings_h)) {

    # print(my_matrix[row, col])
    for(i1_rowags in 1:nrow(i1_shirtsxbookings_a)) {
      for(i1_colags in 1:ncol(i1_shirtsxbookings_a)) {
        ifelse(!i1_shirtsxbookings_a[i1_rowags,i1_colags]=="",i1_shirtsxbookings_h[i1_rowags,i1_colags] <- i1_shirtsxbookings_a[i1_rowags,i1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_i1_shirtsxbookings <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_shirtsxbookings <- c()
sum_i1_zero_shirtsxbookings <- c()
sum_i1_one_shirtsxbookings <- c()
sum_i1_two_shirtsxbookings <- c()
sum_i1_three_shirtsxbookings <- c()
avgr_i1_shirtsxbookings <- c()
sdr_i1_shirtsxbookings <- c()
l6_form_i1_shirtsxbookingssplitted <- c()
form_i1_shirtsxbookings <- c()
for(index_i1_shirtsxbookings in 1:length(i1_teams))
{
  for(index_i1_shirtsxbookings_cols in 1:i1_totalrounds)
  {
    index_i1_shirtsxbookings  <- row.names(i1_shirtsxbookings_h) == i1_teams[index_i1_shirtsxbookings]
    form_i1_shirtsxbookings <- i1_shirtsxbookings_h[index_i1_shirtsxbookings ]
    deleted_form_i1_shirtsxbookings <- form_i1_shirtsxbookings[!form_i1_shirtsxbookings[] == ""]
    l6_form_i1_shirtsxbookings <- tail(deleted_form_i1_shirtsxbookings,i1_last_n_games)
    l6_form_i1_shirtsxbookings <- as.numeric(l6_form_i1_shirtsxbookings)
    suml6_i1_shirtsxbookings[index_i1_shirtsxbookings] <- sum(l6_form_i1_shirtsxbookings)
    suml6_i1_shirtsxbookings[index_i1_shirtsxbookings] <- paste(suml6_i1_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    sum_i1_zero_shirtsxbookings[index_i1_shirtsxbookings] <- length(which(l6_form_i1_shirtsxbookings == 0))
    sum_i1_zero_shirtsxbookings[index_i1_shirtsxbookings] <- paste(sum_i1_zero_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    sum_i1_one_shirtsxbookings[index_i1_shirtsxbookings] <- length(which(l6_form_i1_shirtsxbookings >= 10))
    sum_i1_one_shirtsxbookings[index_i1_shirtsxbookings] <- paste(sum_i1_one_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    sum_i1_two_shirtsxbookings[index_i1_shirtsxbookings] <- length(which(l6_form_i1_shirtsxbookings >= 20))
    sum_i1_two_shirtsxbookings[index_i1_shirtsxbookings] <- paste(sum_i1_two_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    sum_i1_three_shirtsxbookings[index_i1_shirtsxbookings] <- length(which(l6_form_i1_shirtsxbookings >= 30))
    sum_i1_three_shirtsxbookings[index_i1_shirtsxbookings] <- paste(sum_i1_three_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    avgr_i1_shirtsxbookings[index_i1_shirtsxbookings] <- mean(l6_form_i1_shirtsxbookings)
    avgr_i1_shirtsxbookings[index_i1_shirtsxbookings] <- paste(avgr_i1_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    sdr_i1_shirtsxbookings[index_i1_shirtsxbookings] <- sd(l6_form_i1_shirtsxbookings)
    sdr_i1_shirtsxbookings[index_i1_shirtsxbookings] <- paste(sdr_i1_shirtsxbookings[index_i1_shirtsxbookings],sep = "")
    l6_form_i1_shirtsxbookings <- as.character(l6_form_i1_shirtsxbookings)
    #l6_form_i1_shirtsxbookings_flattened <- stri_paste(l6_form_i1_shirtsxbookings,collapse = '')
    #l6_form_i1_shirtsxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_i1_shirtsxbookings_flattened),"")[[1]])
    final_i1_shirtsxbookings[index_i1_shirtsxbookings,index_i1_shirtsxbookings_cols] <- l6_form_i1_shirtsxbookings[index_i1_shirtsxbookings_cols]
  }
}

final_i1_shirtsxbookings[is.na(final_i1_shirtsxbookings)] <- ""
i1_shirtsxbookingsmatrix <- cbind(i1_teams,final_i1_shirtsxbookings,suml6_i1_shirtsxbookings,sum_i1_zero_shirtsxbookings,sum_i1_one_shirtsxbookings,sum_i1_two_shirtsxbookings,sum_i1_three_shirtsxbookings,avgr_i1_shirtsxbookings,sdr_i1_shirtsxbookings)
write.xlsx(i1_shirtsxbookingsmatrix,"Analytics/UCL/I1ucl.xlsx", sheetName = "shirtsxbookings", append = TRUE)
##################################################################################################################################################################################

#create home and away matrices
i1_tgmxcorners_h <- tapply(I1_analytics$TGMXcorners, I1_analytics[c("HomeTeam", "Date")],mean)
i1_tgmxcorners_a <- tapply(I1_analytics$TGMXcorners, I1_analytics[c("AwayTeam", "Date")],mean)

i1_tgmxcorners_h[is.na(i1_tgmxcorners_h)] <- ""
i1_tgmxcorners_a[is.na(i1_tgmxcorners_a)] <- ""

for(i1_rowhgs in 1:nrow(i1_tgmxcorners_h)) {
  for(i1_colhgs in 1:ncol(i1_tgmxcorners_h)) {

    # print(my_matrix[row, col])
    for(i1_rowags in 1:nrow(i1_tgmxcorners_a)) {
      for(i1_colags in 1:ncol(i1_tgmxcorners_a)) {
        ifelse(!i1_tgmxcorners_a[i1_rowags,i1_colags]=="",i1_tgmxcorners_h[i1_rowags,i1_colags] <- i1_tgmxcorners_a[i1_rowags,i1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_i1_tgmxcorners <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_tgmxcorners <- c()
sum_i1_zero_tgmxcorners <- c()
sum_i1_one_tgmxcorners <- c()
sum_i1_two_tgmxcorners <- c()
sum_i1_three_tgmxcorners <- c()
avgr_i1_tgmxcorners <- c()
sdr_i1_tgmxcorners <- c()
l6_form_i1_tgmxcornerssplitted <- c()
form_i1_tgmxcorners <- c()
for(index_i1_tgmxcorners in 1:length(i1_teams))
{
  for(index_i1_tgmxcorners_cols in 1:i1_totalrounds)
  {
    index_i1_tgmxcorners  <- row.names(i1_tgmxcorners_h) == i1_teams[index_i1_tgmxcorners]
    form_i1_tgmxcorners <- i1_tgmxcorners_h[index_i1_tgmxcorners ]
    deleted_form_i1_tgmxcorners <- form_i1_tgmxcorners[!form_i1_tgmxcorners[] == ""]
    l6_form_i1_tgmxcorners <- tail(deleted_form_i1_tgmxcorners,i1_last_n_games)
    l6_form_i1_tgmxcorners <- as.numeric(l6_form_i1_tgmxcorners)
    suml6_i1_tgmxcorners[index_i1_tgmxcorners] <- sum(l6_form_i1_tgmxcorners)
    suml6_i1_tgmxcorners[index_i1_tgmxcorners] <- paste(suml6_i1_tgmxcorners[index_i1_tgmxcorners],sep = "")
    sum_i1_zero_tgmxcorners[index_i1_tgmxcorners] <- length(which(l6_form_i1_tgmxcorners == 0))
    sum_i1_zero_tgmxcorners[index_i1_tgmxcorners] <- paste(sum_i1_zero_tgmxcorners[index_i1_tgmxcorners],sep = "")
    sum_i1_one_tgmxcorners[index_i1_tgmxcorners] <- length(which(l6_form_i1_tgmxcorners >= 10))
    sum_i1_one_tgmxcorners[index_i1_tgmxcorners] <- paste(sum_i1_one_tgmxcorners[index_i1_tgmxcorners],sep = "")
    sum_i1_two_tgmxcorners[index_i1_tgmxcorners] <- length(which(l6_form_i1_tgmxcorners >= 20))
    sum_i1_two_tgmxcorners[index_i1_tgmxcorners] <- paste(sum_i1_two_tgmxcorners[index_i1_tgmxcorners],sep = "")
    sum_i1_three_tgmxcorners[index_i1_tgmxcorners] <- length(which(l6_form_i1_tgmxcorners >= 30))
    sum_i1_three_tgmxcorners[index_i1_tgmxcorners] <- paste(sum_i1_three_tgmxcorners[index_i1_tgmxcorners],sep = "")
    avgr_i1_tgmxcorners[index_i1_tgmxcorners] <- mean(l6_form_i1_tgmxcorners)
    avgr_i1_tgmxcorners[index_i1_tgmxcorners] <- paste(avgr_i1_tgmxcorners[index_i1_tgmxcorners],sep = "")
    sdr_i1_tgmxcorners[index_i1_tgmxcorners] <- sd(l6_form_i1_tgmxcorners)
    sdr_i1_tgmxcorners[index_i1_tgmxcorners] <- paste(sdr_i1_tgmxcorners[index_i1_tgmxcorners],sep = "")
    l6_form_i1_tgmxcorners <- as.character(l6_form_i1_tgmxcorners)
    #l6_form_i1_tgmxcorners_flattened <- stri_paste(l6_form_i1_tgmxcorners,collapse = '')
    #l6_form_i1_tgmxcornerssplitted <- as.numeric(strsplit(as.character(l6_form_i1_tgmxcorners_flattened),"")[[1]])
    final_i1_tgmxcorners[index_i1_tgmxcorners,index_i1_tgmxcorners_cols] <- l6_form_i1_tgmxcorners[index_i1_tgmxcorners_cols]
  }
}

final_i1_tgmxcorners[is.na(final_i1_tgmxcorners)] <- ""
i1_tgmxcornersmatrix <- cbind(i1_teams,final_i1_tgmxcorners,suml6_i1_tgmxcorners,sum_i1_zero_tgmxcorners,sum_i1_one_tgmxcorners,sum_i1_two_tgmxcorners,sum_i1_three_tgmxcorners,avgr_i1_tgmxcorners,sdr_i1_tgmxcorners)
write.xlsx(i1_tgmxcornersmatrix,"Analytics/UCL/I1ucl.xlsx", sheetName = "tgmxcorners", append = TRUE)
########################################################################################################################################################################################################################

#create home and away matrices
i1_goalxcornersxbookings_h <- tapply(I1_analytics$GoalsXcornerXbookings, I1_analytics[c("HomeTeam", "Date")],mean)
i1_goalxcornersxbookings_a <- tapply(I1_analytics$GoalsXcornerXbookings, I1_analytics[c("AwayTeam", "Date")],mean)

i1_goalxcornersxbookings_h[is.na(i1_goalxcornersxbookings_h)] <- ""
i1_goalxcornersxbookings_a[is.na(i1_goalxcornersxbookings_a)] <- ""

for(i1_rowhgs in 1:nrow(i1_goalxcornersxbookings_h)) {
  for(i1_colhgs in 1:ncol(i1_goalxcornersxbookings_h)) {

    # print(my_matrix[row, col])
    for(i1_rowags in 1:nrow(i1_goalxcornersxbookings_a)) {
      for(i1_colags in 1:ncol(i1_goalxcornersxbookings_a)) {
        ifelse(!i1_goalxcornersxbookings_a[i1_rowags,i1_colags]=="",i1_goalxcornersxbookings_h[i1_rowags,i1_colags] <- i1_goalxcornersxbookings_a[i1_rowags,i1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_i1_goalxcornersxbookings <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_goalxcornersxbookings <- c()
sum_i1_zero_goalxcornersxbookings <- c()
sum_i1_one_goalxcornersxbookings <- c()
sum_i1_two_goalxcornersxbookings <- c()
sum_i1_three_goalxcornersxbookings <- c()
avgr_i1_goalxcornersxbookings <- c()
sdr_i1_goalxcornersxbookings <- c()
l6_form_i1_goalxcornersxbookingssplitted <- c()
form_i1_goalxcornersxbookings <- c()
for(index_i1_goalxcornersxbookings in 1:length(i1_teams))
{
  for(index_i1_goalxcornersxbookings_cols in 1:i1_totalrounds)
  {
    index_i1_goalxcornersxbookings  <- row.names(i1_goalxcornersxbookings_h) == i1_teams[index_i1_goalxcornersxbookings]
    form_i1_goalxcornersxbookings <- i1_goalxcornersxbookings_h[index_i1_goalxcornersxbookings ]
    deleted_form_i1_goalxcornersxbookings <- form_i1_goalxcornersxbookings[!form_i1_goalxcornersxbookings[] == ""]
    l6_form_i1_goalxcornersxbookings <- tail(deleted_form_i1_goalxcornersxbookings,i1_last_n_games)
    l6_form_i1_goalxcornersxbookings <- as.numeric(l6_form_i1_goalxcornersxbookings)
    suml6_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- sum(l6_form_i1_goalxcornersxbookings)
    suml6_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(suml6_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    sum_i1_zero_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- length(which(l6_form_i1_goalxcornersxbookings == 0))
    sum_i1_zero_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(sum_i1_zero_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    sum_i1_one_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- length(which(l6_form_i1_goalxcornersxbookings >= 10))
    sum_i1_one_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(sum_i1_one_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    sum_i1_two_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- length(which(l6_form_i1_goalxcornersxbookings >= 20))
    sum_i1_two_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(sum_i1_two_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    sum_i1_three_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- length(which(l6_form_i1_goalxcornersxbookings >= 30))
    sum_i1_three_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(sum_i1_three_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    avgr_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- mean(l6_form_i1_goalxcornersxbookings)
    avgr_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(avgr_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    sdr_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- sd(l6_form_i1_goalxcornersxbookings)
    sdr_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings] <- paste(sdr_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings],sep = "")
    l6_form_i1_goalxcornersxbookings <- as.character(l6_form_i1_goalxcornersxbookings)
    #l6_form_i1_goalxcornersxbookings_flattened <- stri_paste(l6_form_i1_goalxcornersxbookings,collapse = '')
    #l6_form_i1_goalxcornersxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_i1_goalxcornersxbookings_flattened),"")[[1]])
    final_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings,index_i1_goalxcornersxbookings_cols] <- l6_form_i1_goalxcornersxbookings[index_i1_goalxcornersxbookings_cols]
  }
}

final_i1_goalxcornersxbookings[is.na(final_i1_goalxcornersxbookings)] <- ""
i1_goalxcornersxbookingsmatrix <- cbind(i1_teams,final_i1_goalxcornersxbookings,suml6_i1_goalxcornersxbookings,sum_i1_zero_goalxcornersxbookings,sum_i1_one_goalxcornersxbookings,sum_i1_two_goalxcornersxbookings,sum_i1_three_goalxcornersxbookings,avgr_i1_goalxcornersxbookings,sdr_i1_goalxcornersxbookings)
write.xlsx(i1_goalxcornersxbookingsmatrix,"Analytics/UCL/I1ucl.xlsx", sheetName = "goalxcornersxbookings", append = TRUE)
###########################################################################################################################################################################################
###########################################################################################################################################################################################

UCL_analytics <- readxl::read_excel('UCL20242025.xlsx')
UCL_analytics <- UCL_analytics[,-1]
UCL_analytics <- as.data.frame(UCL_analytics)
SP1_analytics <- subset(UCL_analytics, Div == "SP1")

#create home and away matrices
sp1_goalmins_h <- tapply(SP1_analytics$goalmins, SP1_analytics[c("HomeTeam", "Date")],mean)
sp1_goalmins_a <- tapply(SP1_analytics$goalmins, SP1_analytics[c("AwayTeam", "Date")],mean)

sp1_goalmins_h[is.na(sp1_goalmins_h)] <- ""
sp1_goalmins_a[is.na(sp1_goalmins_a)] <- ""

for(sp1_rowhgs in 1:nrow(sp1_goalmins_h)) {
  for(sp1_colhgs in 1:ncol(sp1_goalmins_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowags in 1:nrow(sp1_goalmins_a)) {
      for(sp1_colags in 1:ncol(sp1_goalmins_a)) {
        ifelse(!sp1_goalmins_a[sp1_rowags,sp1_colags]=="",sp1_goalmins_h[sp1_rowags,sp1_colags] <- sp1_goalmins_a[sp1_rowags,sp1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

source("divisions.R")
source("goaltotalsv2.R")
source("Matchday.R")
source("KROUNDS.R")

final_sp1_goalmins <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_goalmins <- c()
sum_sp1_zero_goalmins <- c()
sum_sp1_one_goalmins <- c()
sum_sp1_two_goalmins <- c()
sum_sp1_three_goalmins <- c()
avgr_sp1_goalmins <- c()
sdr_sp1_goalmins <- c()
l6_form_sp1_goalminssplitted <- c()
form_sp1_goalmins <- c()
for(index_sp1_goalmins in 1:length(sp1_teams))
{
  for(index_sp1_goalmins_cols in 1:sp1_totalrounds)
  {
    index_sp1_goalmins  <- row.names(sp1_goalmins_h) == sp1_teams[index_sp1_goalmins]
    form_sp1_goalmins <- sp1_goalmins_h[index_sp1_goalmins ]
    deleted_form_sp1_goalmins <- form_sp1_goalmins[!form_sp1_goalmins[] == ""]
    l6_form_sp1_goalmins <- tail(deleted_form_sp1_goalmins,sp1_last_n_games)
    l6_form_sp1_goalmins <- as.numeric(l6_form_sp1_goalmins)
    suml6_sp1_goalmins[index_sp1_goalmins] <- sum(l6_form_sp1_goalmins)
    suml6_sp1_goalmins[index_sp1_goalmins] <- paste(suml6_sp1_goalmins[index_sp1_goalmins],sep = "")
    sum_sp1_zero_goalmins[index_sp1_goalmins] <- length(which(l6_form_sp1_goalmins == 0))
    sum_sp1_zero_goalmins[index_sp1_goalmins] <- paste(sum_sp1_zero_goalmins[index_sp1_goalmins],sep = "")
    sum_sp1_one_goalmins[index_sp1_goalmins] <- length(which(l6_form_sp1_goalmins >= 100))
    sum_sp1_one_goalmins[index_sp1_goalmins] <- paste(sum_sp1_one_goalmins[index_sp1_goalmins],sep = "")
    sum_sp1_two_goalmins[index_sp1_goalmins] <- length(which(l6_form_sp1_goalmins >= 200))
    sum_sp1_two_goalmins[index_sp1_goalmins] <- paste(sum_sp1_two_goalmins[index_sp1_goalmins],sep = "")
    sum_sp1_three_goalmins[index_sp1_goalmins] <- length(which(l6_form_sp1_goalmins >= 300))
    sum_sp1_three_goalmins[index_sp1_goalmins] <- paste(sum_sp1_three_goalmins[index_sp1_goalmins],sep = "")
    avgr_sp1_goalmins[index_sp1_goalmins] <- mean(l6_form_sp1_goalmins)
    avgr_sp1_goalmins[index_sp1_goalmins] <- paste(avgr_sp1_goalmins[index_sp1_goalmins],sep = "")
    sdr_sp1_goalmins[index_sp1_goalmins] <- sd(l6_form_sp1_goalmins)
    sdr_sp1_goalmins[index_sp1_goalmins] <- paste(sdr_sp1_goalmins[index_sp1_goalmins],sep = "")
    l6_form_sp1_goalmins <- as.character(l6_form_sp1_goalmins)
    #l6_form_sp1_goalmins_flattened <- stri_paste(l6_form_sp1_goalmins,collapse = '')
    #l6_form_sp1_goalminssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_goalmins_flattened),"")[[1]])
    final_sp1_goalmins[index_sp1_goalmins,index_sp1_goalmins_cols] <- l6_form_sp1_goalmins[index_sp1_goalmins_cols]
  }
}

final_sp1_goalmins[is.na(final_sp1_goalmins)] <- ""
sp1_goalminsmatrix <- cbind(sp1_teams,final_sp1_goalmins,suml6_sp1_goalmins,sum_sp1_zero_goalmins,sum_sp1_one_goalmins,sum_sp1_two_goalmins,sum_sp1_three_goalmins,avgr_sp1_goalmins,sdr_sp1_goalmins)
unlink('Analytics/UCL/SP1ucl.xlsx')
write.xlsx(sp1_goalminsmatrix,"Analytics/UCL/SP1ucl.xlsx", sheetName = "goalmins")
##############################################################################################################################################################################################

#create home and away matrices
sp1_shirts_h <- tapply(SP1_analytics$shirts, SP1_analytics[c("HomeTeam", "Date")],mean)
sp1_shirts_a <- tapply(SP1_analytics$shirts, SP1_analytics[c("AwayTeam", "Date")],mean)

sp1_shirts_h[is.na(sp1_shirts_h)] <- ""
sp1_shirts_a[is.na(sp1_shirts_a)] <- ""

for(sp1_rowhgs in 1:nrow(sp1_shirts_h)) {
  for(sp1_colhgs in 1:ncol(sp1_shirts_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowags in 1:nrow(sp1_shirts_a)) {
      for(sp1_colags in 1:ncol(sp1_shirts_a)) {
        ifelse(!sp1_shirts_a[sp1_rowags,sp1_colags]=="",sp1_shirts_h[sp1_rowags,sp1_colags] <- sp1_shirts_a[sp1_rowags,sp1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_sp1_shirts <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_shirts <- c()
sum_sp1_zero_shirts <- c()
sum_sp1_one_shirts <- c()
sum_sp1_two_shirts <- c()
sum_sp1_three_shirts <- c()
avgr_sp1_shirts <- c()
sdr_sp1_shirts <- c()
l6_form_sp1_shirtssplitted <- c()
form_sp1_shirts <- c()
for(index_sp1_shirts in 1:length(sp1_teams))
{
  for(index_sp1_shirts_cols in 1:sp1_totalrounds)
  {
    index_sp1_shirts  <- row.names(sp1_shirts_h) == sp1_teams[index_sp1_shirts]
    form_sp1_shirts <- sp1_shirts_h[index_sp1_shirts ]
    deleted_form_sp1_shirts <- form_sp1_shirts[!form_sp1_shirts[] == ""]
    l6_form_sp1_shirts <- tail(deleted_form_sp1_shirts,sp1_last_n_games)
    l6_form_sp1_shirts <- as.numeric(l6_form_sp1_shirts)
    suml6_sp1_shirts[index_sp1_shirts] <- sum(l6_form_sp1_shirts)
    suml6_sp1_shirts[index_sp1_shirts] <- paste(suml6_sp1_shirts[index_sp1_shirts],sep = "")
    sum_sp1_zero_shirts[index_sp1_shirts] <- length(which(l6_form_sp1_shirts == 0))
    sum_sp1_zero_shirts[index_sp1_shirts] <- paste(sum_sp1_zero_shirts[index_sp1_shirts],sep = "")
    sum_sp1_one_shirts[index_sp1_shirts] <- length(which(l6_form_sp1_shirts >= 10))
    sum_sp1_one_shirts[index_sp1_shirts] <- paste(sum_sp1_one_shirts[index_sp1_shirts],sep = "")
    sum_sp1_two_shirts[index_sp1_shirts] <- length(which(l6_form_sp1_shirts >= 20))
    sum_sp1_two_shirts[index_sp1_shirts] <- paste(sum_sp1_two_shirts[index_sp1_shirts],sep = "")
    sum_sp1_three_shirts[index_sp1_shirts] <- length(which(l6_form_sp1_shirts >= 30))
    sum_sp1_three_shirts[index_sp1_shirts] <- paste(sum_sp1_three_shirts[index_sp1_shirts],sep = "")
    avgr_sp1_shirts[index_sp1_shirts] <- mean(l6_form_sp1_shirts)
    avgr_sp1_shirts[index_sp1_shirts] <- paste(avgr_sp1_shirts[index_sp1_shirts],sep = "")
    sdr_sp1_shirts[index_sp1_shirts] <- sd(l6_form_sp1_shirts)
    sdr_sp1_shirts[index_sp1_shirts] <- paste(sdr_sp1_shirts[index_sp1_shirts],sep = "")
    l6_form_sp1_shirts <- as.character(l6_form_sp1_shirts)
    #l6_form_sp1_shirts_flattened <- stri_paste(l6_form_sp1_shirts,collapse = '')
    #l6_form_sp1_shirtssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_shirts_flattened),"")[[1]])
    final_sp1_shirts[index_sp1_shirts,index_sp1_shirts_cols] <- l6_form_sp1_shirts[index_sp1_shirts_cols]
  }
}

final_sp1_shirts[is.na(final_sp1_shirts)] <- ""
sp1_shirtsmatrix <- cbind(sp1_teams,final_sp1_shirts,suml6_sp1_shirts,sum_sp1_zero_shirts,sum_sp1_one_shirts,sum_sp1_two_shirts,sum_sp1_three_shirts,avgr_sp1_shirts,sdr_sp1_shirts)
write.xlsx(sp1_shirtsmatrix,"Analytics/UCL/SP1ucl.xlsx", sheetName = "shirts", append = TRUE)
#######################################################################################################################################################################################

#create home and away matrices
sp1_crossbookings_h <- tapply(SP1_analytics$Crossbookings, SP1_analytics[c("HomeTeam", "Date")],mean)
sp1_crossbookings_a <- tapply(SP1_analytics$Crossbookings, SP1_analytics[c("AwayTeam", "Date")],mean)

sp1_crossbookings_h[is.na(sp1_crossbookings_h)] <- ""
sp1_crossbookings_a[is.na(sp1_crossbookings_a)] <- ""

for(sp1_rowhgs in 1:nrow(sp1_crossbookings_h)) {
  for(sp1_colhgs in 1:ncol(sp1_crossbookings_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowags in 1:nrow(sp1_crossbookings_a)) {
      for(sp1_colags in 1:ncol(sp1_crossbookings_a)) {
        ifelse(!sp1_crossbookings_a[sp1_rowags,sp1_colags]=="",sp1_crossbookings_h[sp1_rowags,sp1_colags] <- sp1_crossbookings_a[sp1_rowags,sp1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_sp1_crossbookings <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_crossbookings <- c()
sum_sp1_zero_crossbookings <- c()
sum_sp1_one_crossbookings <- c()
sum_sp1_two_crossbookings <- c()
sum_sp1_three_crossbookings <- c()
avgr_sp1_crossbookings <- c()
sdr_sp1_crossbookings <- c()
l6_form_sp1_crossbookingssplitted <- c()
form_sp1_crossbookings <- c()
for(index_sp1_crossbookings in 1:length(sp1_teams))
{
  for(index_sp1_crossbookings_cols in 1:sp1_totalrounds)
  {
    index_sp1_crossbookings  <- row.names(sp1_crossbookings_h) == sp1_teams[index_sp1_crossbookings]
    form_sp1_crossbookings <- sp1_crossbookings_h[index_sp1_crossbookings ]
    deleted_form_sp1_crossbookings <- form_sp1_crossbookings[!form_sp1_crossbookings[] == ""]
    l6_form_sp1_crossbookings <- tail(deleted_form_sp1_crossbookings,sp1_last_n_games)
    l6_form_sp1_crossbookings <- as.numeric(l6_form_sp1_crossbookings)
    suml6_sp1_crossbookings[index_sp1_crossbookings] <- sum(l6_form_sp1_crossbookings)
    suml6_sp1_crossbookings[index_sp1_crossbookings] <- paste(suml6_sp1_crossbookings[index_sp1_crossbookings],sep = "")
    sum_sp1_zero_crossbookings[index_sp1_crossbookings] <- length(which(l6_form_sp1_crossbookings == 0))
    sum_sp1_zero_crossbookings[index_sp1_crossbookings] <- paste(sum_sp1_zero_crossbookings[index_sp1_crossbookings],sep = "")
    sum_sp1_one_crossbookings[index_sp1_crossbookings] <- length(which(l6_form_sp1_crossbookings >= 10))
    sum_sp1_one_crossbookings[index_sp1_crossbookings] <- paste(sum_sp1_one_crossbookings[index_sp1_crossbookings],sep = "")
    sum_sp1_two_crossbookings[index_sp1_crossbookings] <- length(which(l6_form_sp1_crossbookings >= 20))
    sum_sp1_two_crossbookings[index_sp1_crossbookings] <- paste(sum_sp1_two_crossbookings[index_sp1_crossbookings],sep = "")
    sum_sp1_three_crossbookings[index_sp1_crossbookings] <- length(which(l6_form_sp1_crossbookings >= 30))
    sum_sp1_three_crossbookings[index_sp1_crossbookings] <- paste(sum_sp1_three_crossbookings[index_sp1_crossbookings],sep = "")
    avgr_sp1_crossbookings[index_sp1_crossbookings] <- mean(l6_form_sp1_crossbookings)
    avgr_sp1_crossbookings[index_sp1_crossbookings] <- paste(avgr_sp1_crossbookings[index_sp1_crossbookings],sep = "")
    sdr_sp1_crossbookings[index_sp1_crossbookings] <- sd(l6_form_sp1_crossbookings)
    sdr_sp1_crossbookings[index_sp1_crossbookings] <- paste(sdr_sp1_crossbookings[index_sp1_crossbookings],sep = "")
    l6_form_sp1_crossbookings <- as.character(l6_form_sp1_crossbookings)
    #l6_form_sp1_crossbookings_flattened <- stri_paste(l6_form_sp1_crossbookings,collapse = '')
    #l6_form_sp1_crossbookingssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_crossbookings_flattened),"")[[1]])
    final_sp1_crossbookings[index_sp1_crossbookings,index_sp1_crossbookings_cols] <- l6_form_sp1_crossbookings[index_sp1_crossbookings_cols]
  }
}

final_sp1_crossbookings[is.na(final_sp1_crossbookings)] <- ""
sp1_crossbookingsmatrix <- cbind(sp1_teams,final_sp1_crossbookings,suml6_sp1_crossbookings,sum_sp1_zero_crossbookings,sum_sp1_one_crossbookings,sum_sp1_two_crossbookings,sum_sp1_three_crossbookings,avgr_sp1_crossbookings,sdr_sp1_crossbookings)
write.xlsx(sp1_crossbookingsmatrix,"Analytics/UCL/SP1ucl.xlsx", sheetName = "crossbookings", append = TRUE)
######################################################################################################################################################################################

#create home and away matrices
sp1_shirtsxbookings_h <- tapply(SP1_analytics$ShirtsXbookings, SP1_analytics[c("HomeTeam", "Date")],mean)
sp1_shirtsxbookings_a <- tapply(SP1_analytics$ShirtsXbookings, SP1_analytics[c("AwayTeam", "Date")],mean)

sp1_shirtsxbookings_h[is.na(sp1_shirtsxbookings_h)] <- ""
sp1_shirtsxbookings_a[is.na(sp1_shirtsxbookings_a)] <- ""

for(sp1_rowhgs in 1:nrow(sp1_shirtsxbookings_h)) {
  for(sp1_colhgs in 1:ncol(sp1_shirtsxbookings_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowags in 1:nrow(sp1_shirtsxbookings_a)) {
      for(sp1_colags in 1:ncol(sp1_shirtsxbookings_a)) {
        ifelse(!sp1_shirtsxbookings_a[sp1_rowags,sp1_colags]=="",sp1_shirtsxbookings_h[sp1_rowags,sp1_colags] <- sp1_shirtsxbookings_a[sp1_rowags,sp1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_sp1_shirtsxbookings <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_shirtsxbookings <- c()
sum_sp1_zero_shirtsxbookings <- c()
sum_sp1_one_shirtsxbookings <- c()
sum_sp1_two_shirtsxbookings <- c()
sum_sp1_three_shirtsxbookings <- c()
avgr_sp1_shirtsxbookings <- c()
sdr_sp1_shirtsxbookings <- c()
l6_form_sp1_shirtsxbookingssplitted <- c()
form_sp1_shirtsxbookings <- c()
for(index_sp1_shirtsxbookings in 1:length(sp1_teams))
{
  for(index_sp1_shirtsxbookings_cols in 1:sp1_totalrounds)
  {
    index_sp1_shirtsxbookings  <- row.names(sp1_shirtsxbookings_h) == sp1_teams[index_sp1_shirtsxbookings]
    form_sp1_shirtsxbookings <- sp1_shirtsxbookings_h[index_sp1_shirtsxbookings ]
    deleted_form_sp1_shirtsxbookings <- form_sp1_shirtsxbookings[!form_sp1_shirtsxbookings[] == ""]
    l6_form_sp1_shirtsxbookings <- tail(deleted_form_sp1_shirtsxbookings,sp1_last_n_games)
    l6_form_sp1_shirtsxbookings <- as.numeric(l6_form_sp1_shirtsxbookings)
    suml6_sp1_shirtsxbookings[index_sp1_shirtsxbookings] <- sum(l6_form_sp1_shirtsxbookings)
    suml6_sp1_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(suml6_sp1_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    sum_sp1_zero_shirtsxbookings[index_sp1_shirtsxbookings] <- length(which(l6_form_sp1_shirtsxbookings == 0))
    sum_sp1_zero_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(sum_sp1_zero_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    sum_sp1_one_shirtsxbookings[index_sp1_shirtsxbookings] <- length(which(l6_form_sp1_shirtsxbookings >= 10))
    sum_sp1_one_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(sum_sp1_one_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    sum_sp1_two_shirtsxbookings[index_sp1_shirtsxbookings] <- length(which(l6_form_sp1_shirtsxbookings >= 20))
    sum_sp1_two_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(sum_sp1_two_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    sum_sp1_three_shirtsxbookings[index_sp1_shirtsxbookings] <- length(which(l6_form_sp1_shirtsxbookings >= 30))
    sum_sp1_three_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(sum_sp1_three_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    avgr_sp1_shirtsxbookings[index_sp1_shirtsxbookings] <- mean(l6_form_sp1_shirtsxbookings)
    avgr_sp1_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(avgr_sp1_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    sdr_sp1_shirtsxbookings[index_sp1_shirtsxbookings] <- sd(l6_form_sp1_shirtsxbookings)
    sdr_sp1_shirtsxbookings[index_sp1_shirtsxbookings] <- paste(sdr_sp1_shirtsxbookings[index_sp1_shirtsxbookings],sep = "")
    l6_form_sp1_shirtsxbookings <- as.character(l6_form_sp1_shirtsxbookings)
    #l6_form_sp1_shirtsxbookings_flattened <- stri_paste(l6_form_sp1_shirtsxbookings,collapse = '')
    #l6_form_sp1_shirtsxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_shirtsxbookings_flattened),"")[[1]])
    final_sp1_shirtsxbookings[index_sp1_shirtsxbookings,index_sp1_shirtsxbookings_cols] <- l6_form_sp1_shirtsxbookings[index_sp1_shirtsxbookings_cols]
  }
}

final_sp1_shirtsxbookings[is.na(final_sp1_shirtsxbookings)] <- ""
sp1_shirtsxbookingsmatrix <- cbind(sp1_teams,final_sp1_shirtsxbookings,suml6_sp1_shirtsxbookings,sum_sp1_zero_shirtsxbookings,sum_sp1_one_shirtsxbookings,sum_sp1_two_shirtsxbookings,sum_sp1_three_shirtsxbookings,avgr_sp1_shirtsxbookings,sdr_sp1_shirtsxbookings)
write.xlsx(sp1_shirtsxbookingsmatrix,"Analytics/UCL/SP1ucl.xlsx", sheetName = "shirtsxbookings", append = TRUE)
##################################################################################################################################################################################

#create home and away matrices
sp1_tgmxcorners_h <- tapply(SP1_analytics$TGMXcorners, SP1_analytics[c("HomeTeam", "Date")],mean)
sp1_tgmxcorners_a <- tapply(SP1_analytics$TGMXcorners, SP1_analytics[c("AwayTeam", "Date")],mean)

sp1_tgmxcorners_h[is.na(sp1_tgmxcorners_h)] <- ""
sp1_tgmxcorners_a[is.na(sp1_tgmxcorners_a)] <- ""

for(sp1_rowhgs in 1:nrow(sp1_tgmxcorners_h)) {
  for(sp1_colhgs in 1:ncol(sp1_tgmxcorners_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowags in 1:nrow(sp1_tgmxcorners_a)) {
      for(sp1_colags in 1:ncol(sp1_tgmxcorners_a)) {
        ifelse(!sp1_tgmxcorners_a[sp1_rowags,sp1_colags]=="",sp1_tgmxcorners_h[sp1_rowags,sp1_colags] <- sp1_tgmxcorners_a[sp1_rowags,sp1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_sp1_tgmxcorners <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_tgmxcorners <- c()
sum_sp1_zero_tgmxcorners <- c()
sum_sp1_one_tgmxcorners <- c()
sum_sp1_two_tgmxcorners <- c()
sum_sp1_three_tgmxcorners <- c()
avgr_sp1_tgmxcorners <- c()
sdr_sp1_tgmxcorners <- c()
l6_form_sp1_tgmxcornerssplitted <- c()
form_sp1_tgmxcorners <- c()
for(index_sp1_tgmxcorners in 1:length(sp1_teams))
{
  for(index_sp1_tgmxcorners_cols in 1:sp1_totalrounds)
  {
    index_sp1_tgmxcorners  <- row.names(sp1_tgmxcorners_h) == sp1_teams[index_sp1_tgmxcorners]
    form_sp1_tgmxcorners <- sp1_tgmxcorners_h[index_sp1_tgmxcorners ]
    deleted_form_sp1_tgmxcorners <- form_sp1_tgmxcorners[!form_sp1_tgmxcorners[] == ""]
    l6_form_sp1_tgmxcorners <- tail(deleted_form_sp1_tgmxcorners,sp1_last_n_games)
    l6_form_sp1_tgmxcorners <- as.numeric(l6_form_sp1_tgmxcorners)
    suml6_sp1_tgmxcorners[index_sp1_tgmxcorners] <- sum(l6_form_sp1_tgmxcorners)
    suml6_sp1_tgmxcorners[index_sp1_tgmxcorners] <- paste(suml6_sp1_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    sum_sp1_zero_tgmxcorners[index_sp1_tgmxcorners] <- length(which(l6_form_sp1_tgmxcorners == 0))
    sum_sp1_zero_tgmxcorners[index_sp1_tgmxcorners] <- paste(sum_sp1_zero_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    sum_sp1_one_tgmxcorners[index_sp1_tgmxcorners] <- length(which(l6_form_sp1_tgmxcorners >= 10))
    sum_sp1_one_tgmxcorners[index_sp1_tgmxcorners] <- paste(sum_sp1_one_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    sum_sp1_two_tgmxcorners[index_sp1_tgmxcorners] <- length(which(l6_form_sp1_tgmxcorners >= 20))
    sum_sp1_two_tgmxcorners[index_sp1_tgmxcorners] <- paste(sum_sp1_two_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    sum_sp1_three_tgmxcorners[index_sp1_tgmxcorners] <- length(which(l6_form_sp1_tgmxcorners >= 30))
    sum_sp1_three_tgmxcorners[index_sp1_tgmxcorners] <- paste(sum_sp1_three_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    avgr_sp1_tgmxcorners[index_sp1_tgmxcorners] <- mean(l6_form_sp1_tgmxcorners)
    avgr_sp1_tgmxcorners[index_sp1_tgmxcorners] <- paste(avgr_sp1_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    sdr_sp1_tgmxcorners[index_sp1_tgmxcorners] <- sd(l6_form_sp1_tgmxcorners)
    sdr_sp1_tgmxcorners[index_sp1_tgmxcorners] <- paste(sdr_sp1_tgmxcorners[index_sp1_tgmxcorners],sep = "")
    l6_form_sp1_tgmxcorners <- as.character(l6_form_sp1_tgmxcorners)
    #l6_form_sp1_tgmxcorners_flattened <- stri_paste(l6_form_sp1_tgmxcorners,collapse = '')
    #l6_form_sp1_tgmxcornerssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_tgmxcorners_flattened),"")[[1]])
    final_sp1_tgmxcorners[index_sp1_tgmxcorners,index_sp1_tgmxcorners_cols] <- l6_form_sp1_tgmxcorners[index_sp1_tgmxcorners_cols]
  }
}

final_sp1_tgmxcorners[is.na(final_sp1_tgmxcorners)] <- ""
sp1_tgmxcornersmatrix <- cbind(sp1_teams,final_sp1_tgmxcorners,suml6_sp1_tgmxcorners,sum_sp1_zero_tgmxcorners,sum_sp1_one_tgmxcorners,sum_sp1_two_tgmxcorners,sum_sp1_three_tgmxcorners,avgr_sp1_tgmxcorners,sdr_sp1_tgmxcorners)
write.xlsx(sp1_tgmxcornersmatrix,"Analytics/UCL/SP1ucl.xlsx", sheetName = "tgmxcorners", append = TRUE)
########################################################################################################################################################################################################################

#create home and away matrices
sp1_goalxcornersxbookings_h <- tapply(SP1_analytics$GoalsXcornerXbookings, SP1_analytics[c("HomeTeam", "Date")],mean)
sp1_goalxcornersxbookings_a <- tapply(SP1_analytics$GoalsXcornerXbookings, SP1_analytics[c("AwayTeam", "Date")],mean)

sp1_goalxcornersxbookings_h[is.na(sp1_goalxcornersxbookings_h)] <- ""
sp1_goalxcornersxbookings_a[is.na(sp1_goalxcornersxbookings_a)] <- ""

for(sp1_rowhgs in 1:nrow(sp1_goalxcornersxbookings_h)) {
  for(sp1_colhgs in 1:ncol(sp1_goalxcornersxbookings_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowags in 1:nrow(sp1_goalxcornersxbookings_a)) {
      for(sp1_colags in 1:ncol(sp1_goalxcornersxbookings_a)) {
        ifelse(!sp1_goalxcornersxbookings_a[sp1_rowags,sp1_colags]=="",sp1_goalxcornersxbookings_h[sp1_rowags,sp1_colags] <- sp1_goalxcornersxbookings_a[sp1_rowags,sp1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_sp1_goalxcornersxbookings <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_goalxcornersxbookings <- c()
sum_sp1_zero_goalxcornersxbookings <- c()
sum_sp1_one_goalxcornersxbookings <- c()
sum_sp1_two_goalxcornersxbookings <- c()
sum_sp1_three_goalxcornersxbookings <- c()
avgr_sp1_goalxcornersxbookings <- c()
sdr_sp1_goalxcornersxbookings <- c()
l6_form_sp1_goalxcornersxbookingssplitted <- c()
form_sp1_goalxcornersxbookings <- c()
for(index_sp1_goalxcornersxbookings in 1:length(sp1_teams))
{
  for(index_sp1_goalxcornersxbookings_cols in 1:sp1_totalrounds)
  {
    index_sp1_goalxcornersxbookings  <- row.names(sp1_goalxcornersxbookings_h) == sp1_teams[index_sp1_goalxcornersxbookings]
    form_sp1_goalxcornersxbookings <- sp1_goalxcornersxbookings_h[index_sp1_goalxcornersxbookings ]
    deleted_form_sp1_goalxcornersxbookings <- form_sp1_goalxcornersxbookings[!form_sp1_goalxcornersxbookings[] == ""]
    l6_form_sp1_goalxcornersxbookings <- tail(deleted_form_sp1_goalxcornersxbookings,sp1_last_n_games)
    l6_form_sp1_goalxcornersxbookings <- as.numeric(l6_form_sp1_goalxcornersxbookings)
    suml6_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- sum(l6_form_sp1_goalxcornersxbookings)
    suml6_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(suml6_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    sum_sp1_zero_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- length(which(l6_form_sp1_goalxcornersxbookings == 0))
    sum_sp1_zero_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(sum_sp1_zero_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    sum_sp1_one_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- length(which(l6_form_sp1_goalxcornersxbookings >= 10))
    sum_sp1_one_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(sum_sp1_one_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    sum_sp1_two_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- length(which(l6_form_sp1_goalxcornersxbookings >= 20))
    sum_sp1_two_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(sum_sp1_two_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    sum_sp1_three_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- length(which(l6_form_sp1_goalxcornersxbookings >= 30))
    sum_sp1_three_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(sum_sp1_three_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    avgr_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- mean(l6_form_sp1_goalxcornersxbookings)
    avgr_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(avgr_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    sdr_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- sd(l6_form_sp1_goalxcornersxbookings)
    sdr_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings] <- paste(sdr_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings],sep = "")
    l6_form_sp1_goalxcornersxbookings <- as.character(l6_form_sp1_goalxcornersxbookings)
    #l6_form_sp1_goalxcornersxbookings_flattened <- stri_paste(l6_form_sp1_goalxcornersxbookings,collapse = '')
    #l6_form_sp1_goalxcornersxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_goalxcornersxbookings_flattened),"")[[1]])
    final_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings,index_sp1_goalxcornersxbookings_cols] <- l6_form_sp1_goalxcornersxbookings[index_sp1_goalxcornersxbookings_cols]
  }
}

final_sp1_goalxcornersxbookings[is.na(final_sp1_goalxcornersxbookings)] <- ""
sp1_goalxcornersxbookingsmatrix <- cbind(sp1_teams,final_sp1_goalxcornersxbookings,suml6_sp1_goalxcornersxbookings,sum_sp1_zero_goalxcornersxbookings,sum_sp1_one_goalxcornersxbookings,sum_sp1_two_goalxcornersxbookings,sum_sp1_three_goalxcornersxbookings,avgr_sp1_goalxcornersxbookings,sdr_sp1_goalxcornersxbookings)
write.xlsx(sp1_goalxcornersxbookingsmatrix,"Analytics/UCL/SP1ucl.xlsx", sheetName = "goalxcornersxbookings", append = TRUE)
###########################################################################################################################################################################################
###########################################################################################################################################################################################

UCL_analytics <- readxl::read_excel('UCL20242025.xlsx')
UCL_analytics <- UCL_analytics[,-1]
UCL_analytics <- as.data.frame(UCL_analytics)
F1_analytics <- subset(UCL_analytics, Div == "F1")

#create home and away matrices
f1_goalmins_h <- tapply(F1_analytics$goalmins, F1_analytics[c("HomeTeam", "Date")],mean)
f1_goalmins_a <- tapply(F1_analytics$goalmins, F1_analytics[c("AwayTeam", "Date")],mean)

f1_goalmins_h[is.na(f1_goalmins_h)] <- ""
f1_goalmins_a[is.na(f1_goalmins_a)] <- ""

for(f1_rowhgs in 1:nrow(f1_goalmins_h)) {
  for(f1_colhgs in 1:ncol(f1_goalmins_h)) {

    # print(my_matrix[row, col])
    for(f1_rowags in 1:nrow(f1_goalmins_a)) {
      for(f1_colags in 1:ncol(f1_goalmins_a)) {
        ifelse(!f1_goalmins_a[f1_rowags,f1_colags]=="",f1_goalmins_h[f1_rowags,f1_colags] <- f1_goalmins_a[f1_rowags,f1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

source("divisions.R")
source("goaltotalsv2.R")
source("Matchday.R")
source("KROUNDS.R")

final_f1_goalmins <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_goalmins <- c()
sum_f1_zero_goalmins <- c()
sum_f1_one_goalmins <- c()
sum_f1_two_goalmins <- c()
sum_f1_three_goalmins <- c()
avgr_f1_goalmins <- c()
sdr_f1_goalmins <- c()
l6_form_f1_goalminssplitted <- c()
form_f1_goalmins <- c()
for(index_f1_goalmins in 1:length(f1_teams))
{
  for(index_f1_goalmins_cols in 1:f1_totalrounds)
  {
    index_f1_goalmins  <- row.names(f1_goalmins_h) == f1_teams[index_f1_goalmins]
    form_f1_goalmins <- f1_goalmins_h[index_f1_goalmins ]
    deleted_form_f1_goalmins <- form_f1_goalmins[!form_f1_goalmins[] == ""]
    l6_form_f1_goalmins <- tail(deleted_form_f1_goalmins,f1_last_n_games)
    l6_form_f1_goalmins <- as.numeric(l6_form_f1_goalmins)
    suml6_f1_goalmins[index_f1_goalmins] <- sum(l6_form_f1_goalmins)
    suml6_f1_goalmins[index_f1_goalmins] <- paste(suml6_f1_goalmins[index_f1_goalmins],sep = "")
    sum_f1_zero_goalmins[index_f1_goalmins] <- length(which(l6_form_f1_goalmins == 0))
    sum_f1_zero_goalmins[index_f1_goalmins] <- paste(sum_f1_zero_goalmins[index_f1_goalmins],sep = "")
    sum_f1_one_goalmins[index_f1_goalmins] <- length(which(l6_form_f1_goalmins >= 100))
    sum_f1_one_goalmins[index_f1_goalmins] <- paste(sum_f1_one_goalmins[index_f1_goalmins],sep = "")
    sum_f1_two_goalmins[index_f1_goalmins] <- length(which(l6_form_f1_goalmins >= 200))
    sum_f1_two_goalmins[index_f1_goalmins] <- paste(sum_f1_two_goalmins[index_f1_goalmins],sep = "")
    sum_f1_three_goalmins[index_f1_goalmins] <- length(which(l6_form_f1_goalmins >= 300))
    sum_f1_three_goalmins[index_f1_goalmins] <- paste(sum_f1_three_goalmins[index_f1_goalmins],sep = "")
    avgr_f1_goalmins[index_f1_goalmins] <- mean(l6_form_f1_goalmins)
    avgr_f1_goalmins[index_f1_goalmins] <- paste(avgr_f1_goalmins[index_f1_goalmins],sep = "")
    sdr_f1_goalmins[index_f1_goalmins] <- sd(l6_form_f1_goalmins)
    sdr_f1_goalmins[index_f1_goalmins] <- paste(sdr_f1_goalmins[index_f1_goalmins],sep = "")
    l6_form_f1_goalmins <- as.character(l6_form_f1_goalmins)
    #l6_form_f1_goalmins_flattened <- stri_paste(l6_form_f1_goalmins,collapse = '')
    #l6_form_f1_goalminssplitted <- as.numeric(strsplit(as.character(l6_form_f1_goalmins_flattened),"")[[1]])
    final_f1_goalmins[index_f1_goalmins,index_f1_goalmins_cols] <- l6_form_f1_goalmins[index_f1_goalmins_cols]
  }
}

final_f1_goalmins[is.na(final_f1_goalmins)] <- ""
f1_goalminsmatrix <- cbind(f1_teams,final_f1_goalmins,suml6_f1_goalmins,sum_f1_zero_goalmins,sum_f1_one_goalmins,sum_f1_two_goalmins,sum_f1_three_goalmins,avgr_f1_goalmins,sdr_f1_goalmins)
unlink('Analytics/UCL/F1ucl.xlsx')
write.xlsx(f1_goalminsmatrix,"Analytics/UCL/F1ucl.xlsx", sheetName = "goalmins")
##############################################################################################################################################################################################

#create home and away matrices
f1_shirts_h <- tapply(F1_analytics$shirts, F1_analytics[c("HomeTeam", "Date")],mean)
f1_shirts_a <- tapply(F1_analytics$shirts, F1_analytics[c("AwayTeam", "Date")],mean)

f1_shirts_h[is.na(f1_shirts_h)] <- ""
f1_shirts_a[is.na(f1_shirts_a)] <- ""

for(f1_rowhgs in 1:nrow(f1_shirts_h)) {
  for(f1_colhgs in 1:ncol(f1_shirts_h)) {

    # print(my_matrix[row, col])
    for(f1_rowags in 1:nrow(f1_shirts_a)) {
      for(f1_colags in 1:ncol(f1_shirts_a)) {
        ifelse(!f1_shirts_a[f1_rowags,f1_colags]=="",f1_shirts_h[f1_rowags,f1_colags] <- f1_shirts_a[f1_rowags,f1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_f1_shirts <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_shirts <- c()
sum_f1_zero_shirts <- c()
sum_f1_one_shirts <- c()
sum_f1_two_shirts <- c()
sum_f1_three_shirts <- c()
avgr_f1_shirts <- c()
sdr_f1_shirts <- c()
l6_form_f1_shirtssplitted <- c()
form_f1_shirts <- c()
for(index_f1_shirts in 1:length(f1_teams))
{
  for(index_f1_shirts_cols in 1:f1_totalrounds)
  {
    index_f1_shirts  <- row.names(f1_shirts_h) == f1_teams[index_f1_shirts]
    form_f1_shirts <- f1_shirts_h[index_f1_shirts ]
    deleted_form_f1_shirts <- form_f1_shirts[!form_f1_shirts[] == ""]
    l6_form_f1_shirts <- tail(deleted_form_f1_shirts,f1_last_n_games)
    l6_form_f1_shirts <- as.numeric(l6_form_f1_shirts)
    suml6_f1_shirts[index_f1_shirts] <- sum(l6_form_f1_shirts)
    suml6_f1_shirts[index_f1_shirts] <- paste(suml6_f1_shirts[index_f1_shirts],sep = "")
    sum_f1_zero_shirts[index_f1_shirts] <- length(which(l6_form_f1_shirts == 0))
    sum_f1_zero_shirts[index_f1_shirts] <- paste(sum_f1_zero_shirts[index_f1_shirts],sep = "")
    sum_f1_one_shirts[index_f1_shirts] <- length(which(l6_form_f1_shirts >= 10))
    sum_f1_one_shirts[index_f1_shirts] <- paste(sum_f1_one_shirts[index_f1_shirts],sep = "")
    sum_f1_two_shirts[index_f1_shirts] <- length(which(l6_form_f1_shirts >= 20))
    sum_f1_two_shirts[index_f1_shirts] <- paste(sum_f1_two_shirts[index_f1_shirts],sep = "")
    sum_f1_three_shirts[index_f1_shirts] <- length(which(l6_form_f1_shirts >= 30))
    sum_f1_three_shirts[index_f1_shirts] <- paste(sum_f1_three_shirts[index_f1_shirts],sep = "")
    avgr_f1_shirts[index_f1_shirts] <- mean(l6_form_f1_shirts)
    avgr_f1_shirts[index_f1_shirts] <- paste(avgr_f1_shirts[index_f1_shirts],sep = "")
    sdr_f1_shirts[index_f1_shirts] <- sd(l6_form_f1_shirts)
    sdr_f1_shirts[index_f1_shirts] <- paste(sdr_f1_shirts[index_f1_shirts],sep = "")
    l6_form_f1_shirts <- as.character(l6_form_f1_shirts)
    #l6_form_f1_shirts_flattened <- stri_paste(l6_form_f1_shirts,collapse = '')
    #l6_form_f1_shirtssplitted <- as.numeric(strsplit(as.character(l6_form_f1_shirts_flattened),"")[[1]])
    final_f1_shirts[index_f1_shirts,index_f1_shirts_cols] <- l6_form_f1_shirts[index_f1_shirts_cols]
  }
}

final_f1_shirts[is.na(final_f1_shirts)] <- ""
f1_shirtsmatrix <- cbind(f1_teams,final_f1_shirts,suml6_f1_shirts,sum_f1_zero_shirts,sum_f1_one_shirts,sum_f1_two_shirts,sum_f1_three_shirts,avgr_f1_shirts,sdr_f1_shirts)
write.xlsx(f1_shirtsmatrix,"Analytics/UCL/F1ucl.xlsx", sheetName = "shirts", append = TRUE)
#######################################################################################################################################################################################

#create home and away matrices
f1_crossbookings_h <- tapply(F1_analytics$Crossbookings, F1_analytics[c("HomeTeam", "Date")],mean)
f1_crossbookings_a <- tapply(F1_analytics$Crossbookings, F1_analytics[c("AwayTeam", "Date")],mean)

f1_crossbookings_h[is.na(f1_crossbookings_h)] <- ""
f1_crossbookings_a[is.na(f1_crossbookings_a)] <- ""

for(f1_rowhgs in 1:nrow(f1_crossbookings_h)) {
  for(f1_colhgs in 1:ncol(f1_crossbookings_h)) {

    # print(my_matrix[row, col])
    for(f1_rowags in 1:nrow(f1_crossbookings_a)) {
      for(f1_colags in 1:ncol(f1_crossbookings_a)) {
        ifelse(!f1_crossbookings_a[f1_rowags,f1_colags]=="",f1_crossbookings_h[f1_rowags,f1_colags] <- f1_crossbookings_a[f1_rowags,f1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_f1_crossbookings <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_crossbookings <- c()
sum_f1_zero_crossbookings <- c()
sum_f1_one_crossbookings <- c()
sum_f1_two_crossbookings <- c()
sum_f1_three_crossbookings <- c()
avgr_f1_crossbookings <- c()
sdr_f1_crossbookings <- c()
l6_form_f1_crossbookingssplitted <- c()
form_f1_crossbookings <- c()
for(index_f1_crossbookings in 1:length(f1_teams))
{
  for(index_f1_crossbookings_cols in 1:f1_totalrounds)
  {
    index_f1_crossbookings  <- row.names(f1_crossbookings_h) == f1_teams[index_f1_crossbookings]
    form_f1_crossbookings <- f1_crossbookings_h[index_f1_crossbookings ]
    deleted_form_f1_crossbookings <- form_f1_crossbookings[!form_f1_crossbookings[] == ""]
    l6_form_f1_crossbookings <- tail(deleted_form_f1_crossbookings,f1_last_n_games)
    l6_form_f1_crossbookings <- as.numeric(l6_form_f1_crossbookings)
    suml6_f1_crossbookings[index_f1_crossbookings] <- sum(l6_form_f1_crossbookings)
    suml6_f1_crossbookings[index_f1_crossbookings] <- paste(suml6_f1_crossbookings[index_f1_crossbookings],sep = "")
    sum_f1_zero_crossbookings[index_f1_crossbookings] <- length(which(l6_form_f1_crossbookings == 0))
    sum_f1_zero_crossbookings[index_f1_crossbookings] <- paste(sum_f1_zero_crossbookings[index_f1_crossbookings],sep = "")
    sum_f1_one_crossbookings[index_f1_crossbookings] <- length(which(l6_form_f1_crossbookings >= 10))
    sum_f1_one_crossbookings[index_f1_crossbookings] <- paste(sum_f1_one_crossbookings[index_f1_crossbookings],sep = "")
    sum_f1_two_crossbookings[index_f1_crossbookings] <- length(which(l6_form_f1_crossbookings >= 20))
    sum_f1_two_crossbookings[index_f1_crossbookings] <- paste(sum_f1_two_crossbookings[index_f1_crossbookings],sep = "")
    sum_f1_three_crossbookings[index_f1_crossbookings] <- length(which(l6_form_f1_crossbookings >= 30))
    sum_f1_three_crossbookings[index_f1_crossbookings] <- paste(sum_f1_three_crossbookings[index_f1_crossbookings],sep = "")
    avgr_f1_crossbookings[index_f1_crossbookings] <- mean(l6_form_f1_crossbookings)
    avgr_f1_crossbookings[index_f1_crossbookings] <- paste(avgr_f1_crossbookings[index_f1_crossbookings],sep = "")
    sdr_f1_crossbookings[index_f1_crossbookings] <- sd(l6_form_f1_crossbookings)
    sdr_f1_crossbookings[index_f1_crossbookings] <- paste(sdr_f1_crossbookings[index_f1_crossbookings],sep = "")
    l6_form_f1_crossbookings <- as.character(l6_form_f1_crossbookings)
    #l6_form_f1_crossbookings_flattened <- stri_paste(l6_form_f1_crossbookings,collapse = '')
    #l6_form_f1_crossbookingssplitted <- as.numeric(strsplit(as.character(l6_form_f1_crossbookings_flattened),"")[[1]])
    final_f1_crossbookings[index_f1_crossbookings,index_f1_crossbookings_cols] <- l6_form_f1_crossbookings[index_f1_crossbookings_cols]
  }
}

final_f1_crossbookings[is.na(final_f1_crossbookings)] <- ""
f1_crossbookingsmatrix <- cbind(f1_teams,final_f1_crossbookings,suml6_f1_crossbookings,sum_f1_zero_crossbookings,sum_f1_one_crossbookings,sum_f1_two_crossbookings,sum_f1_three_crossbookings,avgr_f1_crossbookings,sdr_f1_crossbookings)
write.xlsx(f1_crossbookingsmatrix,"Analytics/UCL/F1ucl.xlsx", sheetName = "crossbookings", append = TRUE)
######################################################################################################################################################################################

#create home and away matrices
f1_shirtsxbookings_h <- tapply(F1_analytics$ShirtsXbookings, F1_analytics[c("HomeTeam", "Date")],mean)
f1_shirtsxbookings_a <- tapply(F1_analytics$ShirtsXbookings, F1_analytics[c("AwayTeam", "Date")],mean)

f1_shirtsxbookings_h[is.na(f1_shirtsxbookings_h)] <- ""
f1_shirtsxbookings_a[is.na(f1_shirtsxbookings_a)] <- ""

for(f1_rowhgs in 1:nrow(f1_shirtsxbookings_h)) {
  for(f1_colhgs in 1:ncol(f1_shirtsxbookings_h)) {

    # print(my_matrix[row, col])
    for(f1_rowags in 1:nrow(f1_shirtsxbookings_a)) {
      for(f1_colags in 1:ncol(f1_shirtsxbookings_a)) {
        ifelse(!f1_shirtsxbookings_a[f1_rowags,f1_colags]=="",f1_shirtsxbookings_h[f1_rowags,f1_colags] <- f1_shirtsxbookings_a[f1_rowags,f1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_f1_shirtsxbookings <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_shirtsxbookings <- c()
sum_f1_zero_shirtsxbookings <- c()
sum_f1_one_shirtsxbookings <- c()
sum_f1_two_shirtsxbookings <- c()
sum_f1_three_shirtsxbookings <- c()
avgr_f1_shirtsxbookings <- c()
sdr_f1_shirtsxbookings <- c()
l6_form_f1_shirtsxbookingssplitted <- c()
form_f1_shirtsxbookings <- c()
for(index_f1_shirtsxbookings in 1:length(f1_teams))
{
  for(index_f1_shirtsxbookings_cols in 1:f1_totalrounds)
  {
    index_f1_shirtsxbookings  <- row.names(f1_shirtsxbookings_h) == f1_teams[index_f1_shirtsxbookings]
    form_f1_shirtsxbookings <- f1_shirtsxbookings_h[index_f1_shirtsxbookings ]
    deleted_form_f1_shirtsxbookings <- form_f1_shirtsxbookings[!form_f1_shirtsxbookings[] == ""]
    l6_form_f1_shirtsxbookings <- tail(deleted_form_f1_shirtsxbookings,f1_last_n_games)
    l6_form_f1_shirtsxbookings <- as.numeric(l6_form_f1_shirtsxbookings)
    suml6_f1_shirtsxbookings[index_f1_shirtsxbookings] <- sum(l6_form_f1_shirtsxbookings)
    suml6_f1_shirtsxbookings[index_f1_shirtsxbookings] <- paste(suml6_f1_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    sum_f1_zero_shirtsxbookings[index_f1_shirtsxbookings] <- length(which(l6_form_f1_shirtsxbookings == 0))
    sum_f1_zero_shirtsxbookings[index_f1_shirtsxbookings] <- paste(sum_f1_zero_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    sum_f1_one_shirtsxbookings[index_f1_shirtsxbookings] <- length(which(l6_form_f1_shirtsxbookings >= 10))
    sum_f1_one_shirtsxbookings[index_f1_shirtsxbookings] <- paste(sum_f1_one_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    sum_f1_two_shirtsxbookings[index_f1_shirtsxbookings] <- length(which(l6_form_f1_shirtsxbookings >= 20))
    sum_f1_two_shirtsxbookings[index_f1_shirtsxbookings] <- paste(sum_f1_two_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    sum_f1_three_shirtsxbookings[index_f1_shirtsxbookings] <- length(which(l6_form_f1_shirtsxbookings >= 30))
    sum_f1_three_shirtsxbookings[index_f1_shirtsxbookings] <- paste(sum_f1_three_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    avgr_f1_shirtsxbookings[index_f1_shirtsxbookings] <- mean(l6_form_f1_shirtsxbookings)
    avgr_f1_shirtsxbookings[index_f1_shirtsxbookings] <- paste(avgr_f1_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    sdr_f1_shirtsxbookings[index_f1_shirtsxbookings] <- sd(l6_form_f1_shirtsxbookings)
    sdr_f1_shirtsxbookings[index_f1_shirtsxbookings] <- paste(sdr_f1_shirtsxbookings[index_f1_shirtsxbookings],sep = "")
    l6_form_f1_shirtsxbookings <- as.character(l6_form_f1_shirtsxbookings)
    #l6_form_f1_shirtsxbookings_flattened <- stri_paste(l6_form_f1_shirtsxbookings,collapse = '')
    #l6_form_f1_shirtsxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_f1_shirtsxbookings_flattened),"")[[1]])
    final_f1_shirtsxbookings[index_f1_shirtsxbookings,index_f1_shirtsxbookings_cols] <- l6_form_f1_shirtsxbookings[index_f1_shirtsxbookings_cols]
  }
}

final_f1_shirtsxbookings[is.na(final_f1_shirtsxbookings)] <- ""
f1_shirtsxbookingsmatrix <- cbind(f1_teams,final_f1_shirtsxbookings,suml6_f1_shirtsxbookings,sum_f1_zero_shirtsxbookings,sum_f1_one_shirtsxbookings,sum_f1_two_shirtsxbookings,sum_f1_three_shirtsxbookings,avgr_f1_shirtsxbookings,sdr_f1_shirtsxbookings)
write.xlsx(f1_shirtsxbookingsmatrix,"Analytics/UCL/F1ucl.xlsx", sheetName = "shirtsxbookings", append = TRUE)
##################################################################################################################################################################################

#create home and away matrices
f1_tgmxcorners_h <- tapply(F1_analytics$TGMXcorners, F1_analytics[c("HomeTeam", "Date")],mean)
f1_tgmxcorners_a <- tapply(F1_analytics$TGMXcorners, F1_analytics[c("AwayTeam", "Date")],mean)

f1_tgmxcorners_h[is.na(f1_tgmxcorners_h)] <- ""
f1_tgmxcorners_a[is.na(f1_tgmxcorners_a)] <- ""

for(f1_rowhgs in 1:nrow(f1_tgmxcorners_h)) {
  for(f1_colhgs in 1:ncol(f1_tgmxcorners_h)) {

    # print(my_matrix[row, col])
    for(f1_rowags in 1:nrow(f1_tgmxcorners_a)) {
      for(f1_colags in 1:ncol(f1_tgmxcorners_a)) {
        ifelse(!f1_tgmxcorners_a[f1_rowags,f1_colags]=="",f1_tgmxcorners_h[f1_rowags,f1_colags] <- f1_tgmxcorners_a[f1_rowags,f1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_f1_tgmxcorners <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_tgmxcorners <- c()
sum_f1_zero_tgmxcorners <- c()
sum_f1_one_tgmxcorners <- c()
sum_f1_two_tgmxcorners <- c()
sum_f1_three_tgmxcorners <- c()
avgr_f1_tgmxcorners <- c()
sdr_f1_tgmxcorners <- c()
l6_form_f1_tgmxcornerssplitted <- c()
form_f1_tgmxcorners <- c()
for(index_f1_tgmxcorners in 1:length(f1_teams))
{
  for(index_f1_tgmxcorners_cols in 1:f1_totalrounds)
  {
    index_f1_tgmxcorners  <- row.names(f1_tgmxcorners_h) == f1_teams[index_f1_tgmxcorners]
    form_f1_tgmxcorners <- f1_tgmxcorners_h[index_f1_tgmxcorners ]
    deleted_form_f1_tgmxcorners <- form_f1_tgmxcorners[!form_f1_tgmxcorners[] == ""]
    l6_form_f1_tgmxcorners <- tail(deleted_form_f1_tgmxcorners,f1_last_n_games)
    l6_form_f1_tgmxcorners <- as.numeric(l6_form_f1_tgmxcorners)
    suml6_f1_tgmxcorners[index_f1_tgmxcorners] <- sum(l6_form_f1_tgmxcorners)
    suml6_f1_tgmxcorners[index_f1_tgmxcorners] <- paste(suml6_f1_tgmxcorners[index_f1_tgmxcorners],sep = "")
    sum_f1_zero_tgmxcorners[index_f1_tgmxcorners] <- length(which(l6_form_f1_tgmxcorners == 0))
    sum_f1_zero_tgmxcorners[index_f1_tgmxcorners] <- paste(sum_f1_zero_tgmxcorners[index_f1_tgmxcorners],sep = "")
    sum_f1_one_tgmxcorners[index_f1_tgmxcorners] <- length(which(l6_form_f1_tgmxcorners >= 10))
    sum_f1_one_tgmxcorners[index_f1_tgmxcorners] <- paste(sum_f1_one_tgmxcorners[index_f1_tgmxcorners],sep = "")
    sum_f1_two_tgmxcorners[index_f1_tgmxcorners] <- length(which(l6_form_f1_tgmxcorners >= 20))
    sum_f1_two_tgmxcorners[index_f1_tgmxcorners] <- paste(sum_f1_two_tgmxcorners[index_f1_tgmxcorners],sep = "")
    sum_f1_three_tgmxcorners[index_f1_tgmxcorners] <- length(which(l6_form_f1_tgmxcorners >= 30))
    sum_f1_three_tgmxcorners[index_f1_tgmxcorners] <- paste(sum_f1_three_tgmxcorners[index_f1_tgmxcorners],sep = "")
    avgr_f1_tgmxcorners[index_f1_tgmxcorners] <- mean(l6_form_f1_tgmxcorners)
    avgr_f1_tgmxcorners[index_f1_tgmxcorners] <- paste(avgr_f1_tgmxcorners[index_f1_tgmxcorners],sep = "")
    sdr_f1_tgmxcorners[index_f1_tgmxcorners] <- sd(l6_form_f1_tgmxcorners)
    sdr_f1_tgmxcorners[index_f1_tgmxcorners] <- paste(sdr_f1_tgmxcorners[index_f1_tgmxcorners],sep = "")
    l6_form_f1_tgmxcorners <- as.character(l6_form_f1_tgmxcorners)
    #l6_form_f1_tgmxcorners_flattened <- stri_paste(l6_form_f1_tgmxcorners,collapse = '')
    #l6_form_f1_tgmxcornerssplitted <- as.numeric(strsplit(as.character(l6_form_f1_tgmxcorners_flattened),"")[[1]])
    final_f1_tgmxcorners[index_f1_tgmxcorners,index_f1_tgmxcorners_cols] <- l6_form_f1_tgmxcorners[index_f1_tgmxcorners_cols]
  }
}

final_f1_tgmxcorners[is.na(final_f1_tgmxcorners)] <- ""
f1_tgmxcornersmatrix <- cbind(f1_teams,final_f1_tgmxcorners,suml6_f1_tgmxcorners,sum_f1_zero_tgmxcorners,sum_f1_one_tgmxcorners,sum_f1_two_tgmxcorners,sum_f1_three_tgmxcorners,avgr_f1_tgmxcorners,sdr_f1_tgmxcorners)
write.xlsx(f1_tgmxcornersmatrix,"Analytics/UCL/F1ucl.xlsx", sheetName = "tgmxcorners", append = TRUE)
########################################################################################################################################################################################################################

#create home and away matrices
f1_goalxcornersxbookings_h <- tapply(F1_analytics$GoalsXcornerXbookings, F1_analytics[c("HomeTeam", "Date")],mean)
f1_goalxcornersxbookings_a <- tapply(F1_analytics$GoalsXcornerXbookings, F1_analytics[c("AwayTeam", "Date")],mean)

f1_goalxcornersxbookings_h[is.na(f1_goalxcornersxbookings_h)] <- ""
f1_goalxcornersxbookings_a[is.na(f1_goalxcornersxbookings_a)] <- ""

for(f1_rowhgs in 1:nrow(f1_goalxcornersxbookings_h)) {
  for(f1_colhgs in 1:ncol(f1_goalxcornersxbookings_h)) {

    # print(my_matrix[row, col])
    for(f1_rowags in 1:nrow(f1_goalxcornersxbookings_a)) {
      for(f1_colags in 1:ncol(f1_goalxcornersxbookings_a)) {
        ifelse(!f1_goalxcornersxbookings_a[f1_rowags,f1_colags]=="",f1_goalxcornersxbookings_h[f1_rowags,f1_colags] <- f1_goalxcornersxbookings_a[f1_rowags,f1_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_f1_goalxcornersxbookings <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_goalxcornersxbookings <- c()
sum_f1_zero_goalxcornersxbookings <- c()
sum_f1_one_goalxcornersxbookings <- c()
sum_f1_two_goalxcornersxbookings <- c()
sum_f1_three_goalxcornersxbookings <- c()
avgr_f1_goalxcornersxbookings <- c()
sdr_f1_goalxcornersxbookings <- c()
l6_form_f1_goalxcornersxbookingssplitted <- c()
form_f1_goalxcornersxbookings <- c()
for(index_f1_goalxcornersxbookings in 1:length(f1_teams))
{
  for(index_f1_goalxcornersxbookings_cols in 1:f1_totalrounds)
  {
    index_f1_goalxcornersxbookings  <- row.names(f1_goalxcornersxbookings_h) == f1_teams[index_f1_goalxcornersxbookings]
    form_f1_goalxcornersxbookings <- f1_goalxcornersxbookings_h[index_f1_goalxcornersxbookings ]
    deleted_form_f1_goalxcornersxbookings <- form_f1_goalxcornersxbookings[!form_f1_goalxcornersxbookings[] == ""]
    l6_form_f1_goalxcornersxbookings <- tail(deleted_form_f1_goalxcornersxbookings,f1_last_n_games)
    l6_form_f1_goalxcornersxbookings <- as.numeric(l6_form_f1_goalxcornersxbookings)
    suml6_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- sum(l6_form_f1_goalxcornersxbookings)
    suml6_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(suml6_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    sum_f1_zero_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- length(which(l6_form_f1_goalxcornersxbookings == 0))
    sum_f1_zero_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(sum_f1_zero_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    sum_f1_one_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- length(which(l6_form_f1_goalxcornersxbookings >= 10))
    sum_f1_one_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(sum_f1_one_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    sum_f1_two_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- length(which(l6_form_f1_goalxcornersxbookings >= 20))
    sum_f1_two_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(sum_f1_two_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    sum_f1_three_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- length(which(l6_form_f1_goalxcornersxbookings >= 30))
    sum_f1_three_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(sum_f1_three_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    avgr_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- mean(l6_form_f1_goalxcornersxbookings)
    avgr_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(avgr_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    sdr_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- sd(l6_form_f1_goalxcornersxbookings)
    sdr_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings] <- paste(sdr_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings],sep = "")
    l6_form_f1_goalxcornersxbookings <- as.character(l6_form_f1_goalxcornersxbookings)
    #l6_form_f1_goalxcornersxbookings_flattened <- stri_paste(l6_form_f1_goalxcornersxbookings,collapse = '')
    #l6_form_f1_goalxcornersxbookingssplitted <- as.numeric(strsplit(as.character(l6_form_f1_goalxcornersxbookings_flattened),"")[[1]])
    final_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings,index_f1_goalxcornersxbookings_cols] <- l6_form_f1_goalxcornersxbookings[index_f1_goalxcornersxbookings_cols]
  }
}

final_f1_goalxcornersxbookings[is.na(final_f1_goalxcornersxbookings)] <- ""
f1_goalxcornersxbookingsmatrix <- cbind(f1_teams,final_f1_goalxcornersxbookings,suml6_f1_goalxcornersxbookings,sum_f1_zero_goalxcornersxbookings,sum_f1_one_goalxcornersxbookings,sum_f1_two_goalxcornersxbookings,sum_f1_three_goalxcornersxbookings,avgr_f1_goalxcornersxbookings,sdr_f1_goalxcornersxbookings)
write.xlsx(f1_goalxcornersxbookingsmatrix,"Analytics/UCL/F1ucl.xlsx", sheetName = "goalxcornersxbookings", append = TRUE)
###########################################################################################################################################################################################
###########################################################################################################################################################################################















