library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library(stringr)
library(stringi)
#b1
final_b1_gc <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
suml6_b1_gc <- c()
l6_form_b1_gcsplitted <- c()
form_b1_gc <- c()
for(index_b1_gc in 1:length(b1_teams))
{
  for(index_b1_gc_cols in 1:b1_totalrounds)
  {
    index_b1_gc  <- row.names(b1_goalconceded_h) == b1_teams[index_b1_gc]
    form_b1_gc <- b1_goalconceded_h[index_b1_gc ]
    deleted_form_b1_gc <- form_b1_gc[!form_b1_gc[] == ""]
    l6_form_b1_gc <- tail(deleted_form_b1_gc,b1_last_n_games)
    l6_form_b1_gc <- as.numeric(l6_form_b1_gc)
    suml6_b1_gc[index_b1_gc] <- sum(l6_form_b1_gc)
    suml6_b1_gc[index_b1_gc] <- paste(suml6_b1_gc[index_b1_gc],sep = "")
    l6_form_b1_gc <- as.character(l6_form_b1_gc)
    l6_form_b1_gc_flattened <- stri_paste(l6_form_b1_gc,collapse = '')
    l6_form_b1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_b1_gc_flattened),"")[[1]])
    final_b1_gc[index_b1_gc,index_b1_gc_cols] <- l6_form_b1_gcsplitted[index_b1_gc_cols]
  }
}

final_b1_gc[is.na(final_b1_gc)] <- ""
b1_goalconcededmatrix <- cbind(b1_teams,final_b1_gc,suml6_b1_gc)
##############################################################################################
#d1
final_d1_gc <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_gc <- c()
l6_form_d1_gcsplitted <- c()
form_d1_gc <- c()
for(index_d1_gc in 1:length(d1_teams))
{
  for(index_d1_gc_cols in 1:d1_totalrounds)
  {
    index_d1_gc  <- row.names(d1_goalconceded_h) == d1_teams[index_d1_gc]
    form_d1_gc <- d1_goalconceded_h[index_d1_gc ]
    deleted_form_d1_gc <- form_d1_gc[!form_d1_gc[] == ""]
    l6_form_d1_gc <- tail(deleted_form_d1_gc,d1_last_n_games)
    l6_form_d1_gc <- as.numeric(l6_form_d1_gc)
    suml6_d1_gc[index_d1_gc] <- sum(l6_form_d1_gc)
    suml6_d1_gc[index_d1_gc] <- paste(suml6_d1_gc[index_d1_gc],sep = "")
    l6_form_d1_gc <- as.character(l6_form_d1_gc)
    l6_form_d1_gc_flattened <- stri_paste(l6_form_d1_gc,collapse = '')
    l6_form_d1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_d1_gc_flattened),"")[[1]])
    final_d1_gc[index_d1_gc,index_d1_gc_cols] <- l6_form_d1_gcsplitted[index_d1_gc_cols]
  }
}

final_d1_gc[is.na(final_d1_gc)] <- ""
d1_goalconcededmatrix <- cbind(d1_teams,final_d1_gc,suml6_d1_gc)
##############################################################################################
#d2
final_d2_gc <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
suml6_d2_gc <- c()
l6_form_d2_gcsplitted <- c()
form_d2_gc <- c()
for(index_d2_gc in 1:length(d2_teams))
{
  for(index_d2_gc_cols in 1:d2_totalrounds)
  {
    index_d2_gc  <- row.names(d2_goalconceded_h) == d2_teams[index_d2_gc]
    form_d2_gc <- d2_goalconceded_h[index_d2_gc ]
    deleted_form_d2_gc <- form_d2_gc[!form_d2_gc[] == ""]
    l6_form_d2_gc <- tail(deleted_form_d2_gc,d2_last_n_games)
    l6_form_d2_gc <- as.numeric(l6_form_d2_gc)
    suml6_d2_gc[index_d2_gc] <- sum(l6_form_d2_gc)
    suml6_d2_gc[index_d2_gc] <- paste(suml6_d2_gc[index_d2_gc],sep = "")
    l6_form_d2_gc <- as.character(l6_form_d2_gc)
    l6_form_d2_gc_flattened <- stri_paste(l6_form_d2_gc,collapse = '')
    l6_form_d2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_d2_gc_flattened),"")[[1]])
    final_d2_gc[index_d2_gc,index_d2_gc_cols] <- l6_form_d2_gcsplitted[index_d2_gc_cols]
  }
}

final_d2_gc[is.na(final_d2_gc)] <- ""
d2_goalconcededmatrix <- cbind(d2_teams,final_d2_gc,suml6_d2_gc)
##############################################################################################
#e0
final_e0_gc <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_gc <- c()
l6_form_e0_gcsplitted <- c()
form_e0_gc <- c()
for(index_e0_gc in 1:length(e0_teams))
{
  for(index_e0_gc_cols in 1:e0_totalrounds)
  {
    index_e0_gc  <- row.names(e0_goalconceded_h) == e0_teams[index_e0_gc]
    form_e0_gc <- e0_goalconceded_h[index_e0_gc ]
    deleted_form_e0_gc <- form_e0_gc[!form_e0_gc[] == ""]
    l6_form_e0_gc <- tail(deleted_form_e0_gc,e0_last_n_games)
    l6_form_e0_gc <- as.numeric(l6_form_e0_gc)
    suml6_e0_gc[index_e0_gc] <- sum(l6_form_e0_gc)
    suml6_e0_gc[index_e0_gc] <- paste(suml6_e0_gc[index_e0_gc],sep = "")
    l6_form_e0_gc <- as.character(l6_form_e0_gc)
    l6_form_e0_gc_flattened <- stri_paste(l6_form_e0_gc,collapse = '')
    l6_form_e0_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e0_gc_flattened),"")[[1]])
    final_e0_gc[index_e0_gc,index_e0_gc_cols] <- l6_form_e0_gcsplitted[index_e0_gc_cols]
  }
}

final_e0_gc[is.na(final_e0_gc)] <- ""
e0_goalconcededmatrix <- cbind(e0_teams,final_e0_gc,suml6_e0_gc)
##############################################################################################
#e1
final_e1_gc <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
suml6_e1_gc <- c()
l6_form_e1_gcsplitted <- c()
form_e1_gc <- c()
for(index_e1_gc in 1:length(e1_teams))
{
  for(index_e1_gc_cols in 1:e1_totalrounds)
  {
    index_e1_gc  <- row.names(e1_goalconceded_h) == e1_teams[index_e1_gc]
    form_e1_gc <- e1_goalconceded_h[index_e1_gc ]
    deleted_form_e1_gc <- form_e1_gc[!form_e1_gc[] == ""]
    l6_form_e1_gc <- tail(deleted_form_e1_gc,e1_last_n_games)
    l6_form_e1_gc <- as.numeric(l6_form_e1_gc)
    suml6_e1_gc[index_e1_gc] <- sum(l6_form_e1_gc)
    suml6_e1_gc[index_e1_gc] <- paste(suml6_e1_gc[index_e1_gc],sep = "")
    l6_form_e1_gc <- as.character(l6_form_e1_gc)
    l6_form_e1_gc_flattened <- stri_paste(l6_form_e1_gc,collapse = '')
    l6_form_e1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e1_gc_flattened),"")[[1]])
    final_e1_gc[index_e1_gc,index_e1_gc_cols] <- l6_form_e1_gcsplitted[index_e1_gc_cols]
  }
}

final_e1_gc[is.na(final_e1_gc)] <- ""
e1_goalconcededmatrix <- cbind(e1_teams,final_e1_gc,suml6_e1_gc)
##############################################################################################
#e2
final_e2_gc <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
suml6_e2_gc <- c()
l6_form_e2_gcsplitted <- c()
form_e2_gc <- c()
for(index_e2_gc in 1:length(e2_teams))
{
  for(index_e2_gc_cols in 1:e2_totalrounds)
  {
    index_e2_gc  <- row.names(e2_goalconceded_h) == e2_teams[index_e2_gc]
    form_e2_gc <- e2_goalconceded_h[index_e2_gc ]
    deleted_form_e2_gc <- form_e2_gc[!form_e2_gc[] == ""]
    l6_form_e2_gc <- tail(deleted_form_e2_gc,e2_last_n_games)
    l6_form_e2_gc <- as.numeric(l6_form_e2_gc)
    suml6_e2_gc[index_e2_gc] <- sum(l6_form_e2_gc)
    suml6_e2_gc[index_e2_gc] <- paste(suml6_e2_gc[index_e2_gc],sep = "")
    l6_form_e2_gc <- as.character(l6_form_e2_gc)
    l6_form_e2_gc_flattened <- stri_paste(l6_form_e2_gc,collapse = '')
    l6_form_e2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e2_gc_flattened),"")[[1]])
    final_e2_gc[index_e2_gc,index_e2_gc_cols] <- l6_form_e2_gcsplitted[index_e2_gc_cols]
  }
}

final_e2_gc[is.na(final_e2_gc)] <- ""
e2_goalconcededmatrix <- cbind(e2_teams,final_e2_gc,suml6_e2_gc)
##############################################################################################
#e3
final_e3_gc <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
suml6_e3_gc <- c()
l6_form_e3_gcsplitted <- c()
form_e3_gc <- c()
for(index_e3_gc in 1:length(e3_teams))
{
  for(index_e3_gc_cols in 1:e3_totalrounds)
  {
    index_e3_gc  <- row.names(e3_goalconceded_h) == e3_teams[index_e3_gc]
    form_e3_gc <- e3_goalconceded_h[index_e3_gc ]
    deleted_form_e3_gc <- form_e3_gc[!form_e3_gc[] == ""]
    l6_form_e3_gc <- tail(deleted_form_e3_gc,e3_last_n_games)
    l6_form_e3_gc <- as.numeric(l6_form_e3_gc)
    suml6_e3_gc[index_e3_gc] <- sum(l6_form_e3_gc)
    suml6_e3_gc[index_e3_gc] <- paste(suml6_e3_gc[index_e3_gc],sep = "")
    l6_form_e3_gc <- as.character(l6_form_e3_gc)
    l6_form_e3_gc_flattened <- stri_paste(l6_form_e3_gc,collapse = '')
    l6_form_e3_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e3_gc_flattened),"")[[1]])
    final_e3_gc[index_e3_gc,index_e3_gc_cols] <- l6_form_e3_gcsplitted[index_e3_gc_cols]
  }
}

final_e3_gc[is.na(final_e3_gc)] <- ""
e3_goalconcededmatrix <- cbind(e3_teams,final_e3_gc,suml6_e3_gc)
##############################################################################################
#ec
final_ec_gc <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
suml6_ec_gc <- c()
l6_form_ec_gcsplitted <- c()
form_ec_gc <- c()
for(index_ec_gc in 1:length(ec_teams))
{
  for(index_ec_gc_cols in 1:ec_totalrounds)
  {
    index_ec_gc  <- row.names(ec_goalconceded_h) == ec_teams[index_ec_gc]
    form_ec_gc <- ec_goalconceded_h[index_ec_gc ]
    deleted_form_ec_gc <- form_ec_gc[!form_ec_gc[] == ""]
    l6_form_ec_gc <- tail(deleted_form_ec_gc,ec_last_n_games)
    l6_form_ec_gc <- as.numeric(l6_form_ec_gc)
    suml6_ec_gc[index_ec_gc] <- sum(l6_form_ec_gc)
    suml6_ec_gc[index_ec_gc] <- paste(suml6_ec_gc[index_ec_gc],sep = "")
    l6_form_ec_gc <- as.character(l6_form_ec_gc)
    l6_form_ec_gc_flattened <- stri_paste(l6_form_ec_gc,collapse = '')
    l6_form_ec_gcsplitted <- as.numeric(strsplit(as.character(l6_form_ec_gc_flattened),"")[[1]])
    final_ec_gc[index_ec_gc,index_ec_gc_cols] <- l6_form_ec_gcsplitted[index_ec_gc_cols]
  }
}

final_ec_gc[is.na(final_ec_gc)] <- ""
ec_goalconcededmatrix <- cbind(ec_teams,final_ec_gc,suml6_ec_gc)
##############################################################################################
#f1
final_f1_gc <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_gc <- c()
l6_form_f1_gcsplitted <- c()
form_f1_gc <- c()
for(index_f1_gc in 1:length(f1_teams))
{
  for(index_f1_gc_cols in 1:f1_totalrounds)
  {
    index_f1_gc  <- row.names(f1_goalconceded_h) == f1_teams[index_f1_gc]
    form_f1_gc <- f1_goalconceded_h[index_f1_gc ]
    deleted_form_f1_gc <- form_f1_gc[!form_f1_gc[] == ""]
    l6_form_f1_gc <- tail(deleted_form_f1_gc,f1_last_n_games)
    l6_form_f1_gc <- as.numeric(l6_form_f1_gc)
    suml6_f1_gc[index_f1_gc] <- sum(l6_form_f1_gc)
    suml6_f1_gc[index_f1_gc] <- paste(suml6_f1_gc[index_f1_gc],sep = "")
    l6_form_f1_gc <- as.character(l6_form_f1_gc)
    l6_form_f1_gc_flattened <- stri_paste(l6_form_f1_gc,collapse = '')
    l6_form_f1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_f1_gc_flattened),"")[[1]])
    final_f1_gc[index_f1_gc,index_f1_gc_cols] <- l6_form_f1_gcsplitted[index_f1_gc_cols]
  }
}

final_f1_gc[is.na(final_f1_gc)] <- ""
f1_goalconcededmatrix <- cbind(f1_teams,final_f1_gc,suml6_f1_gc)
##############################################################################################
#f2
final_f2_gc <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
suml6_f2_gc <- c()
l6_form_f2_gcsplitted <- c()
form_f2_gc <- c()
for(index_f2_gc in 1:length(f2_teams))
{
  for(index_f2_gc_cols in 1:f2_totalrounds)
  {
    index_f2_gc  <- row.names(f2_goalconceded_h) == f2_teams[index_f2_gc]
    form_f2_gc <- f2_goalconceded_h[index_f2_gc ]
    deleted_form_f2_gc <- form_f2_gc[!form_f2_gc[] == ""]
    l6_form_f2_gc <- tail(deleted_form_f2_gc,f2_last_n_games)
    l6_form_f2_gc <- as.numeric(l6_form_f2_gc)
    suml6_f2_gc[index_f2_gc] <- sum(l6_form_f2_gc)
    suml6_f2_gc[index_f2_gc] <- paste(suml6_f2_gc[index_f2_gc],sep = "")
    l6_form_f2_gc <- as.character(l6_form_f2_gc)
    l6_form_f2_gc_flattened <- stri_paste(l6_form_f2_gc,collapse = '')
    l6_form_f2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_f2_gc_flattened),"")[[1]])
    final_f2_gc[index_f2_gc,index_f2_gc_cols] <- l6_form_f2_gcsplitted[index_f2_gc_cols]
  }
}

final_f2_gc[is.na(final_f2_gc)] <- ""
f2_goalconcededmatrix <- cbind(f2_teams,final_f2_gc,suml6_f2_gc)
##############################################################################################
#g1
final_g1_gc <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
suml6_g1_gc <- c()
l6_form_g1_gcsplitted <- c()
form_g1_gc <- c()
for(index_g1_gc in 1:length(g1_teams))
{
  for(index_g1_gc_cols in 1:g1_totalrounds)
  {
    index_g1_gc  <- row.names(g1_goalconceded_h) == g1_teams[index_g1_gc]
    form_g1_gc <- g1_goalconceded_h[index_g1_gc ]
    deleted_form_g1_gc <- form_g1_gc[!form_g1_gc[] == ""]
    l6_form_g1_gc <- tail(deleted_form_g1_gc,g1_last_n_games)
    l6_form_g1_gc <- as.numeric(l6_form_g1_gc)
    suml6_g1_gc[index_g1_gc] <- sum(l6_form_g1_gc)
    suml6_g1_gc[index_g1_gc] <- paste(suml6_g1_gc[index_g1_gc],sep = "")
    l6_form_g1_gc <- as.character(l6_form_g1_gc)
    l6_form_g1_gc_flattened <- stri_paste(l6_form_g1_gc,collapse = '')
    l6_form_g1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_g1_gc_flattened),"")[[1]])
    final_g1_gc[index_g1_gc,index_g1_gc_cols] <- l6_form_g1_gcsplitted[index_g1_gc_cols]
  }
}

final_g1_gc[is.na(final_g1_gc)] <- ""
g1_goalconcededmatrix <- cbind(g1_teams,final_g1_gc,suml6_g1_gc)
##############################################################################################
#i1
final_i1_gc <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_gc <- c()
l6_form_i1_gcsplitted <- c()
form_i1_gc <- c()
for(index_i1_gc in 1:length(i1_teams))
{
  for(index_i1_gc_cols in 1:i1_totalrounds)
  {
    index_i1_gc  <- row.names(i1_goalconceded_h) == i1_teams[index_i1_gc]
    form_i1_gc <- i1_goalconceded_h[index_i1_gc ]
    deleted_form_i1_gc <- form_i1_gc[!form_i1_gc[] == ""]
    l6_form_i1_gc <- tail(deleted_form_i1_gc,i1_last_n_games)
    l6_form_i1_gc <- as.numeric(l6_form_i1_gc)
    suml6_i1_gc[index_i1_gc] <- sum(l6_form_i1_gc)
    suml6_i1_gc[index_i1_gc] <- paste(suml6_i1_gc[index_i1_gc],sep = "")
    l6_form_i1_gc <- as.character(l6_form_i1_gc)
    l6_form_i1_gc_flattened <- stri_paste(l6_form_i1_gc,collapse = '')
    l6_form_i1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_i1_gc_flattened),"")[[1]])
    final_i1_gc[index_i1_gc,index_i1_gc_cols] <- l6_form_i1_gcsplitted[index_i1_gc_cols]
  }
}

final_i1_gc[is.na(final_i1_gc)] <- ""
i1_goalconcededmatrix <- cbind(i1_teams,final_i1_gc,suml6_i1_gc)
##############################################################################################
##i2
final_i2_gc <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
suml6_i2_gc <- c()
l6_form_i2_gcsplitted <- c()
form_i2_gc <- c()
for(index_i2_gc in 1:length(i2_teams))
{
  for(index_i2_gc_cols in 1:i2_totalrounds)
  {
    index_i2_gc  <- row.names(i2_goalconceded_h) == i2_teams[index_i2_gc]
    form_i2_gc <- i2_goalconceded_h[index_i2_gc ]
    deleted_form_i2_gc <- form_i2_gc[!form_i2_gc[] == ""]
    l6_form_i2_gc <- tail(deleted_form_i2_gc,i2_last_n_games)
    l6_form_i2_gc <- as.numeric(l6_form_i2_gc)
    suml6_i2_gc[index_i2_gc] <- sum(l6_form_i2_gc)
    suml6_i2_gc[index_i2_gc] <- paste(suml6_i2_gc[index_i2_gc],sep = "")
    l6_form_i2_gc <- as.character(l6_form_i2_gc)
    l6_form_i2_gc_flattened <- stri_paste(l6_form_i2_gc,collapse = '')
    l6_form_i2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_i2_gc_flattened),"")[[1]])
    final_i2_gc[index_i2_gc,index_i2_gc_cols] <- l6_form_i2_gcsplitted[index_i2_gc_cols]
  }
}

final_i2_gc[is.na(final_i2_gc)] <- ""
i2_goalconcededmatrix <- cbind(i2_teams,final_i2_gc,suml6_i2_gc)
##############################################################################################
#n1
final_n1_gc <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
suml6_n1_gc <- c()
l6_form_n1_gcsplitted <- c()
form_n1_gc <- c()
for(index_n1_gc in 1:length(n1_teams))
{
  for(index_n1_gc_cols in 1:n1_totalrounds)
  {
    index_n1_gc  <- row.names(n1_goalconceded_h) == n1_teams[index_n1_gc]
    form_n1_gc <- n1_goalconceded_h[index_n1_gc ]
    deleted_form_n1_gc <- form_n1_gc[!form_n1_gc[] == ""]
    l6_form_n1_gc <- tail(deleted_form_n1_gc,n1_last_n_games)
    l6_form_n1_gc <- as.numeric(l6_form_n1_gc)
    suml6_n1_gc[index_n1_gc] <- sum(l6_form_n1_gc)
    suml6_n1_gc[index_n1_gc] <- paste(suml6_n1_gc[index_n1_gc],sep = "")
    l6_form_n1_gc <- as.character(l6_form_n1_gc)
    l6_form_n1_gc_flattened <- stri_paste(l6_form_n1_gc,collapse = '')
    l6_form_n1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_n1_gc_flattened),"")[[1]])
    final_n1_gc[index_n1_gc,index_n1_gc_cols] <- l6_form_n1_gcsplitted[index_n1_gc_cols]
  }
}

final_n1_gc[is.na(final_n1_gc)] <- ""
n1_goalconcededmatrix <- cbind(n1_teams,final_n1_gc,suml6_n1_gc)
##############################################################################################
#p1
final_p1_gc <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
suml6_p1_gc <- c()
l6_form_p1_gcsplitted <- c()
form_p1_gc <- c()
for(index_p1_gc in 1:length(p1_teams))
{
  for(index_p1_gc_cols in 1:p1_totalrounds)
  {
    index_p1_gc  <- row.names(p1_goalconceded_h) == p1_teams[index_p1_gc]
    form_p1_gc <- p1_goalconceded_h[index_p1_gc ]
    deleted_form_p1_gc <- form_p1_gc[!form_p1_gc[] == ""]
    l6_form_p1_gc <- tail(deleted_form_p1_gc,p1_last_n_games)
    l6_form_p1_gc <- as.numeric(l6_form_p1_gc)
    suml6_p1_gc[index_p1_gc] <- sum(l6_form_p1_gc)
    suml6_p1_gc[index_p1_gc] <- paste(suml6_p1_gc[index_p1_gc],sep = "")
    l6_form_p1_gc <- as.character(l6_form_p1_gc)
    l6_form_p1_gc_flattened <- stri_paste(l6_form_p1_gc,collapse = '')
    l6_form_p1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_p1_gc_flattened),"")[[1]])
    final_p1_gc[index_p1_gc,index_p1_gc_cols] <- l6_form_p1_gcsplitted[index_p1_gc_cols]
  }
}

final_p1_gc[is.na(final_p1_gc)] <- ""
p1_goalconcededmatrix <- cbind(p1_teams,final_p1_gc,suml6_p1_gc)
##############################################################################################
#sp1
final_sp1_gc <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_gc <- c()
l6_form_sp1_gcsplitted <- c()
form_sp1_gc <- c()
for(index_sp1_gc in 1:length(sp1_teams))
{
  for(index_sp1_gc_cols in 1:sp1_totalrounds)
  {
    index_sp1_gc  <- row.names(sp1_goalconceded_h) == sp1_teams[index_sp1_gc]
    form_sp1_gc <- sp1_goalconceded_h[index_sp1_gc ]
    deleted_form_sp1_gc <- form_sp1_gc[!form_sp1_gc[] == ""]
    l6_form_sp1_gc <- tail(deleted_form_sp1_gc,sp1_last_n_games)
    l6_form_sp1_gc <- as.numeric(l6_form_sp1_gc)
    suml6_sp1_gc[index_sp1_gc] <- sum(l6_form_sp1_gc)
    suml6_sp1_gc[index_sp1_gc] <- paste(suml6_sp1_gc[index_sp1_gc],sep = "")
    l6_form_sp1_gc <- as.character(l6_form_sp1_gc)
    l6_form_sp1_gc_flattened <- stri_paste(l6_form_sp1_gc,collapse = '')
    l6_form_sp1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sp1_gc_flattened),"")[[1]])
    final_sp1_gc[index_sp1_gc,index_sp1_gc_cols] <- l6_form_sp1_gcsplitted[index_sp1_gc_cols]
  }
}

final_sp1_gc[is.na(final_sp1_gc)] <- ""
sp1_goalconcededmatrix <- cbind(sp1_teams,final_sp1_gc,suml6_sp1_gc)
##############################################################################################
#sp2
final_sp2_gc <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
suml6_sp2_gc <- c()
l6_form_sp2_gcsplitted <- c()
form_sp2_gc <- c()
for(index_sp2_gc in 1:length(sp2_teams))
{
  for(index_sp2_gc_cols in 1:sp2_totalrounds)
  {
    index_sp2_gc  <- row.names(sp2_goalconceded_h) == sp2_teams[index_sp2_gc]
    form_sp2_gc <- sp2_goalconceded_h[index_sp2_gc ]
    deleted_form_sp2_gc <- form_sp2_gc[!form_sp2_gc[] == ""]
    l6_form_sp2_gc <- tail(deleted_form_sp2_gc,sp2_last_n_games)
    l6_form_sp2_gc <- as.numeric(l6_form_sp2_gc)
    suml6_sp2_gc[index_sp2_gc] <- sum(l6_form_sp2_gc)
    suml6_sp2_gc[index_sp2_gc] <- paste(suml6_sp2_gc[index_sp2_gc],sep = "")
    l6_form_sp2_gc <- as.character(l6_form_sp2_gc)
    l6_form_sp2_gc_flattened <- stri_paste(l6_form_sp2_gc,collapse = '')
    l6_form_sp2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sp2_gc_flattened),"")[[1]])
    final_sp2_gc[index_sp2_gc,index_sp2_gc_cols] <- l6_form_sp2_gcsplitted[index_sp2_gc_cols]
  }
}

final_sp2_gc[is.na(final_sp2_gc)] <- ""
sp2_goalconcededmatrix <- cbind(sp2_teams,final_sp2_gc,suml6_sp2_gc)
##############################################################################################
#sc0
final_sc0_gc <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
suml6_sc0_gc <- c()
l6_form_sc0_gcsplitted <- c()
form_sc0_gc <- c()
for(index_sc0_gc in 1:length(sc0_teams))
{
  for(index_sc0_gc_cols in 1:sc0_totalrounds)
  {
    index_sc0_gc  <- row.names(sc0_goalconceded_h) == sc0_teams[index_sc0_gc]
    form_sc0_gc <- sc0_goalconceded_h[index_sc0_gc ]
    deleted_form_sc0_gc <- form_sc0_gc[!form_sc0_gc[] == ""]
    l6_form_sc0_gc <- tail(deleted_form_sc0_gc,sc0_last_n_games)
    l6_form_sc0_gc <- as.numeric(l6_form_sc0_gc)
    suml6_sc0_gc[index_sc0_gc] <- sum(l6_form_sc0_gc)
    suml6_sc0_gc[index_sc0_gc] <- paste(suml6_sc0_gc[index_sc0_gc],sep = "")
    l6_form_sc0_gc <- as.character(l6_form_sc0_gc)
    l6_form_sc0_gc_flattened <- stri_paste(l6_form_sc0_gc,collapse = '')
    l6_form_sc0_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc0_gc_flattened),"")[[1]])
    final_sc0_gc[index_sc0_gc,index_sc0_gc_cols] <- l6_form_sc0_gcsplitted[index_sc0_gc_cols]
  }
}

final_sc0_gc[is.na(final_sc0_gc)] <- ""
sc0_goalconcededmatrix <- cbind(sc0_teams,final_sc0_gc,suml6_sc0_gc)
##############################################################################################
#sc1
final_sc1_gc <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
suml6_sc1_gc <- c()
l6_form_sc1_gcsplitted <- c()
form_sc1_gc <- c()
for(index_sc1_gc in 1:length(sc1_teams))
{
  for(index_sc1_gc_cols in 1:sc1_totalrounds)
  {
    index_sc1_gc  <- row.names(sc1_goalconceded_h) == sc1_teams[index_sc1_gc]
    form_sc1_gc <- sc1_goalconceded_h[index_sc1_gc ]
    deleted_form_sc1_gc <- form_sc1_gc[!form_sc1_gc[] == ""]
    l6_form_sc1_gc <- tail(deleted_form_sc1_gc,sc1_last_n_games)
    l6_form_sc1_gc <- as.numeric(l6_form_sc1_gc)
    suml6_sc1_gc[index_sc1_gc] <- sum(l6_form_sc1_gc)
    suml6_sc1_gc[index_sc1_gc] <- paste(suml6_sc1_gc[index_sc1_gc],sep = "")
    l6_form_sc1_gc <- as.character(l6_form_sc1_gc)
    l6_form_sc1_gc_flattened <- stri_paste(l6_form_sc1_gc,collapse = '')
    l6_form_sc1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc1_gc_flattened),"")[[1]])
    final_sc1_gc[index_sc1_gc,index_sc1_gc_cols] <- l6_form_sc1_gcsplitted[index_sc1_gc_cols]
  }
}

final_sc1_gc[is.na(final_sc1_gc)] <- ""
sc1_goalconcededmatrix <- cbind(sc1_teams,final_sc1_gc,suml6_sc1_gc)
##############################################################################################
#sc2
final_sc2_gc <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
suml6_sc2_gc <- c()
l6_form_sc2_gcsplitted <- c()
form_sc2_gc <- c()
for(index_sc2_gc in 1:length(sc2_teams))
{
  for(index_sc2_gc_cols in 1:sc2_totalrounds)
  {
    index_sc2_gc  <- row.names(sc2_goalconceded_h) == sc2_teams[index_sc2_gc]
    form_sc2_gc <- sc2_goalconceded_h[index_sc2_gc ]
    deleted_form_sc2_gc <- form_sc2_gc[!form_sc2_gc[] == ""]
    l6_form_sc2_gc <- tail(deleted_form_sc2_gc,sc2_last_n_games)
    l6_form_sc2_gc <- as.numeric(l6_form_sc2_gc)
    suml6_sc2_gc[index_sc2_gc] <- sum(l6_form_sc2_gc)
    suml6_sc2_gc[index_sc2_gc] <- paste(suml6_sc2_gc[index_sc2_gc],sep = "")
    l6_form_sc2_gc <- as.character(l6_form_sc2_gc)
    l6_form_sc2_gc_flattened <- stri_paste(l6_form_sc2_gc,collapse = '')
    l6_form_sc2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc2_gc_flattened),"")[[1]])
    final_sc2_gc[index_sc2_gc,index_sc2_gc_cols] <- l6_form_sc2_gcsplitted[index_sc2_gc_cols]
  }
}

final_sc2_gc[is.na(final_sc2_gc)] <- ""
sc2_goalconcededmatrix <- cbind(sc2_teams,final_sc2_gc,suml6_sc2_gc)
##############################################################################################
#sc3
final_sc3_gc <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
suml6_sc3_gc <- c()
l6_form_sc3_gcsplitted <- c()
form_sc3_gc <- c()
for(index_sc3_gc in 1:length(sc3_teams))
{
  for(index_sc3_gc_cols in 1:sc3_totalrounds)
  {
    index_sc3_gc  <- row.names(sc3_goalconceded_h) == sc3_teams[index_sc3_gc]
    form_sc3_gc <- sc3_goalconceded_h[index_sc3_gc ]
    deleted_form_sc3_gc <- form_sc3_gc[!form_sc3_gc[] == ""]
    l6_form_sc3_gc <- tail(deleted_form_sc3_gc,sc3_last_n_games)
    l6_form_sc3_gc <- as.numeric(l6_form_sc3_gc)
    suml6_sc3_gc[index_sc3_gc] <- sum(l6_form_sc3_gc)
    suml6_sc3_gc[index_sc3_gc] <- paste(suml6_sc3_gc[index_sc3_gc],sep = "")
    l6_form_sc3_gc <- as.character(l6_form_sc3_gc)
    l6_form_sc3_gc_flattened <- stri_paste(l6_form_sc3_gc,collapse = '')
    l6_form_sc3_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc3_gc_flattened),"")[[1]])
    final_sc3_gc[index_sc3_gc,index_sc3_gc_cols] <- l6_form_sc3_gcsplitted[index_sc3_gc_cols]
  }
}

final_sc3_gc[is.na(final_sc3_gc)] <- ""
sc3_goalconcededmatrix <- cbind(sc3_teams,final_sc3_gc,suml6_sc3_gc)
##############################################################################################
#t1
final_t1_gc <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
suml6_t1_gc <- c()
l6_form_t1_gcsplitted <- c()
form_t1_gc <- c()
for(index_t1_gc in 1:length(t1_teams))
{
  for(index_t1_gc_cols in 1:t1_totalrounds)
  {
    index_t1_gc  <- row.names(t1_goalconceded_h) == t1_teams[index_t1_gc]
    form_t1_gc <- t1_goalconceded_h[index_t1_gc ]
    deleted_form_t1_gc <- form_t1_gc[!form_t1_gc[] == ""]
    l6_form_t1_gc <- tail(deleted_form_t1_gc,t1_last_n_games)
    l6_form_t1_gc <- as.numeric(l6_form_t1_gc)
    suml6_t1_gc[index_t1_gc] <- sum(l6_form_t1_gc)
    suml6_t1_gc[index_t1_gc] <- paste(suml6_t1_gc[index_t1_gc],sep = "")
    l6_form_t1_gc <- as.character(l6_form_t1_gc)
    l6_form_t1_gc_flattened <- stri_paste(l6_form_t1_gc,collapse = '')
    l6_form_t1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_t1_gc_flattened),"")[[1]])
    final_t1_gc[index_t1_gc,index_t1_gc_cols] <- l6_form_t1_gcsplitted[index_t1_gc_cols]
  }
}

final_t1_gc[is.na(final_t1_gc)] <- ""
t1_goalconcededmatrix <- cbind(t1_teams,final_t1_gc,suml6_t1_gc)
##############################################################################################







































