library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#b1
final_b1_hf <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
suml6_b1_hf <- c()
l6_form_b1_hfsplitted <- c()
form_b1_hf <- c()
for(index_b1_hf in 1:length(b1_teams))
{
  for(index_b1_hf_cols in 1:b1_totalrounds)
  {
    index_b1_hf  <- row.names(b1_form_h) == b1_teams[index_b1_hf]
    form_b1_hf <- b1_form_h[index_b1_hf ]
    deleted_form_b1_hf <- form_b1_hf[!form_b1_hf[] == ""]
    l6_form_b1_hf <- tail(deleted_form_b1_hf,b1_last_n_games)
    # #l6_form_b1_hf <- as.numeric(l6_form_b1_hf)
    # suml6_b1_hf[index_b1_hf] <- sum(l6_form_b1_hf)
    # suml6_b1_hf[index_b1_hf] <- paste(suml6_b1_hf[index_b1_hf],sep = "")
    #l6_form_b1_hf <- as.character(l6_form_b1_hf)
    l6_form_b1_hf_flattened <- stri_paste(l6_form_b1_hf,collapse = '')
    l6_form_b1_hfsplitted <- (strsplit(as.character(l6_form_b1_hf_flattened),"")[[1]])
    final_b1_hf[index_b1_hf,index_b1_hf_cols] <- l6_form_b1_hfsplitted[index_b1_hf_cols]
  }
}
final_b1_hf[is.na(final_b1_hf)] <- ""
b1_formmatrix <- cbind(b1_teams,final_b1_hf,suml6_b1_hf)
##############################################################################################
#d1
final_d1_hf <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_hf <- c()
l6_form_d1_hfsplitted <- c()
form_d1_hf <- c()
for(index_d1_hf in 1:length(d1_teams))
{
  for(index_d1_hf_cols in 1:d1_totalrounds)
  {
    index_d1_hf  <- row.names(d1_form_h) == d1_teams[index_d1_hf]
    form_d1_hf <- d1_form_h[index_d1_hf ]
    deleted_form_d1_hf <- form_d1_hf[!form_d1_hf[] == ""]
    l6_form_d1_hf <- tail(deleted_form_d1_hf,d1_last_n_games)
    # l6_form_d1_hf <- as.numeric(l6_form_d1_hf)
    # suml6_d1_hf[index_d1_hf] <- sum(l6_form_d1_hf)
    # suml6_d1_hf[index_d1_hf] <- paste(suml6_d1_hf[index_d1_hf],sep = "")
    # l6_form_d1_hf <- as.character(l6_form_d1_hf)
    l6_form_d1_hf_flattened <- stri_paste(l6_form_d1_hf,collapse = '')
    l6_form_d1_hfsplitted <- (strsplit(as.character(l6_form_d1_hf_flattened),"")[[1]])
    final_d1_hf[index_d1_hf,index_d1_hf_cols] <- l6_form_d1_hfsplitted[index_d1_hf_cols]
  }
}

final_d1_hf[is.na(final_d1_hf)] <- ""
d1_formmatrix <- cbind(d1_teams,final_d1_hf,suml6_d1_hf)
##############################################################################################
#d2
final_d2_hf <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
suml6_d2_hf <- c()
l6_form_d2_hfsplitted <- c()
form_d2_hf <- c()
for(index_d2_hf in 1:length(d2_teams))
{
  for(index_d2_hf_cols in 1:d2_totalrounds)
  {
    index_d2_hf  <- row.names(d2_form_h) == d2_teams[index_d2_hf]
    form_d2_hf <- d2_form_h[index_d2_hf ]
    deleted_form_d2_hf <- form_d2_hf[!form_d2_hf[] == ""]
    l6_form_d2_hf <- tail(deleted_form_d2_hf,d2_last_n_games)
    # l6_form_d2_hf <- as.numeric(l6_form_d2_hf)
    # suml6_d2_hf[index_d2_hf] <- sum(l6_form_d2_hf)
    # suml6_d2_hf[index_d2_hf] <- paste(suml6_d2_hf[index_d2_hf],sep = "")
    # l6_form_d2_hf <- as.character(l6_form_d2_hf)
    l6_form_d2_hf_flattened <- stri_paste(l6_form_d2_hf,collapse = '')
    l6_form_d2_hfsplitted <- (strsplit(as.character(l6_form_d2_hf_flattened),"")[[1]])
    final_d2_hf[index_d2_hf,index_d2_hf_cols] <- l6_form_d2_hfsplitted[index_d2_hf_cols]
  }
}

final_d2_hf[is.na(final_d2_hf)] <- ""
d2_formmatrix <- cbind(d2_teams,final_d2_hf,suml6_d2_hf)
##############################################################################################
#e0
final_e0_hf <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_hf <- c()
l6_form_e0_hfsplitted <- c()
form_e0_hf <- c()
for(index_e0_hf in 1:length(e0_teams))
{
  for(index_e0_hf_cols in 1:e0_totalrounds)
  {
    index_e0_hf  <- row.names(e0_form_h) == e0_teams[index_e0_hf]
    form_e0_hf <- e0_form_h[index_e0_hf ]
    deleted_form_e0_hf <- form_e0_hf[!form_e0_hf[] == ""]
    l6_form_e0_hf <- tail(deleted_form_e0_hf,e0_last_n_games)
    # l6_form_e0_hf <- as.numeric(l6_form_e0_hf)
    # suml6_e0_hf[index_e0_hf] <- sum(l6_form_e0_hf)
    # suml6_e0_hf[index_e0_hf] <- paste(suml6_e0_hf[index_e0_hf],sep = "")
    # l6_form_e0_hf <- as.character(l6_form_e0_hf)
    l6_form_e0_hf_flattened <- stri_paste(l6_form_e0_hf,collapse = '')
    l6_form_e0_hfsplitted <- (strsplit(as.character(l6_form_e0_hf_flattened),"")[[1]])
    final_e0_hf[index_e0_hf,index_e0_hf_cols] <- l6_form_e0_hfsplitted[index_e0_hf_cols]
  }
}

final_e0_hf[is.na(final_e0_hf)] <- ""
e0_formmatrix <- cbind(e0_teams,final_e0_hf,suml6_e0_hf)
##############################################################################################
#e1
final_e1_hf <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
suml6_e1_hf <- c()
l6_form_e1_hfsplitted <- c()
form_e1_hf <- c()
for(index_e1_hf in 1:length(e1_teams))
{
  for(index_e1_hf_cols in 1:e1_totalrounds)
  {
    index_e1_hf  <- row.names(e1_form_h) == e1_teams[index_e1_hf]
    form_e1_hf <- e1_form_h[index_e1_hf ]
    deleted_form_e1_hf <- form_e1_hf[!form_e1_hf[] == ""]
    l6_form_e1_hf <- tail(deleted_form_e1_hf,e1_last_n_games)
    # l6_form_e1_hf <- as.numeric(l6_form_e1_hf)
    # suml6_e1_hf[index_e1_hf] <- sum(l6_form_e1_hf)
    # suml6_e1_hf[index_e1_hf] <- paste(suml6_e1_hf[index_e1_hf],sep = "")
    # l6_form_e1_hf <- as.character(l6_form_e1_hf)
    l6_form_e1_hf_flattened <- stri_paste(l6_form_e1_hf,collapse = '')
    l6_form_e1_hfsplitted <- (strsplit(as.character(l6_form_e1_hf_flattened),"")[[1]])
    final_e1_hf[index_e1_hf,index_e1_hf_cols] <- l6_form_e1_hfsplitted[index_e1_hf_cols]
  }
}

final_e1_hf[is.na(final_e1_hf)] <- ""
e1_formmatrix <- cbind(e1_teams,final_e1_hf,suml6_e1_hf)
##############################################################################################
#e2
final_e2_hf <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
suml6_e2_hf <- c()
l6_form_e2_hfsplitted <- c()
form_e2_hf <- c()
for(index_e2_hf in 1:length(e2_teams))
{
  for(index_e2_hf_cols in 1:e2_totalrounds)
  {
    index_e2_hf  <- row.names(e2_form_h) == e2_teams[index_e2_hf]
    form_e2_hf <- e2_form_h[index_e2_hf ]
    deleted_form_e2_hf <- form_e2_hf[!form_e2_hf[] == ""]
    l6_form_e2_hf <- tail(deleted_form_e2_hf,e2_last_n_games)
    # l6_form_e2_hf <- as.numeric(l6_form_e2_hf)
    # suml6_e2_hf[index_e2_hf] <- sum(l6_form_e2_hf)
    # suml6_e2_hf[index_e2_hf] <- paste(suml6_e2_hf[index_e2_hf],sep = "")
    # l6_form_e2_hf <- as.character(l6_form_e2_hf)
    l6_form_e2_hf_flattened <- stri_paste(l6_form_e2_hf,collapse = '')
    l6_form_e2_hfsplitted <- (strsplit(as.character(l6_form_e2_hf_flattened),"")[[1]])
    final_e2_hf[index_e2_hf,index_e2_hf_cols] <- l6_form_e2_hfsplitted[index_e2_hf_cols]
  }
}

final_e2_hf[is.na(final_e2_hf)] <- ""
e2_formmatrix <- cbind(e2_teams,final_e2_hf,suml6_e2_hf)
##############################################################################################
#e3
final_e3_hf <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
suml6_e3_hf <- c()
l6_form_e3_hfsplitted <- c()
form_e3_hf <- c()
for(index_e3_hf in 1:length(e3_teams))
{
  for(index_e3_hf_cols in 1:e3_totalrounds)
  {
    index_e3_hf  <- row.names(e3_form_h) == e3_teams[index_e3_hf]
    form_e3_hf <- e3_form_h[index_e3_hf ]
    deleted_form_e3_hf <- form_e3_hf[!form_e3_hf[] == ""]
    l6_form_e3_hf <- tail(deleted_form_e3_hf,e3_last_n_games)
    # l6_form_e3_hf <- as.numeric(l6_form_e3_hf)
    # suml6_e3_hf[index_e3_hf] <- sum(l6_form_e3_hf)
    # suml6_e3_hf[index_e3_hf] <- paste(suml6_e3_hf[index_e3_hf],sep = "")
    # l6_form_e3_hf <- as.character(l6_form_e3_hf)
    l6_form_e3_hf_flattened <- stri_paste(l6_form_e3_hf,collapse = '')
    l6_form_e3_hfsplitted <- (strsplit(as.character(l6_form_e3_hf_flattened),"")[[1]])
    final_e3_hf[index_e3_hf,index_e3_hf_cols] <- l6_form_e3_hfsplitted[index_e3_hf_cols]
  }
}

final_e3_hf[is.na(final_e3_hf)] <- ""
e3_formmatrix <- cbind(e3_teams,final_e3_hf,suml6_e3_hf)
##############################################################################################
#ec
final_ec_hf <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
suml6_ec_hf <- c()
l6_form_ec_hfsplitted <- c()
form_ec_hf <- c()
for(index_ec_hf in 1:length(ec_teams))
{
  for(index_ec_hf_cols in 1:ec_totalrounds)
  {
    index_ec_hf  <- row.names(ec_form_h) == ec_teams[index_ec_hf]
    form_ec_hf <- ec_form_h[index_ec_hf ]
    deleted_form_ec_hf <- form_ec_hf[!form_ec_hf[] == ""]
    l6_form_ec_hf <- tail(deleted_form_ec_hf,ec_last_n_games)
    # l6_form_ec_hf <- as.numeric(l6_form_ec_hf)
    # suml6_ec_hf[index_ec_hf] <- sum(l6_form_ec_hf)
    # suml6_ec_hf[index_ec_hf] <- paste(suml6_ec_hf[index_ec_hf],sep = "")
    # l6_form_ec_hf <- as.character(l6_form_ec_hf)
    l6_form_ec_hf_flattened <- stri_paste(l6_form_ec_hf,collapse = '')
    l6_form_ec_hfsplitted <- (strsplit(as.character(l6_form_ec_hf_flattened),"")[[1]])
    final_ec_hf[index_ec_hf,index_ec_hf_cols] <- l6_form_ec_hfsplitted[index_ec_hf_cols]
  }
}

final_ec_hf[is.na(final_ec_hf)] <- ""
ec_formmatrix <- cbind(ec_teams,final_ec_hf,suml6_ec_hf)
##############################################################################################
#f1
final_f1_hf <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_hf <- c()
l6_form_f1_hfsplitted <- c()
form_f1_hf <- c()
for(index_f1_hf in 1:length(f1_teams))
{
  for(index_f1_hf_cols in 1:f1_totalrounds)
  {
    index_f1_hf  <- row.names(f1_form_h) == f1_teams[index_f1_hf]
    form_f1_hf <- f1_form_h[index_f1_hf ]
    deleted_form_f1_hf <- form_f1_hf[!form_f1_hf[] == ""]
    l6_form_f1_hf <- tail(deleted_form_f1_hf,f1_last_n_games)
    # l6_form_f1_hf <- as.numeric(l6_form_f1_hf)
    # suml6_f1_hf[index_f1_hf] <- sum(l6_form_f1_hf)
    # suml6_f1_hf[index_f1_hf] <- paste(suml6_f1_hf[index_f1_hf],sep = "")
    # l6_form_f1_hf <- as.character(l6_form_f1_hf)
    l6_form_f1_hf_flattened <- stri_paste(l6_form_f1_hf,collapse = '')
    l6_form_f1_hfsplitted <- (strsplit(as.character(l6_form_f1_hf_flattened),"")[[1]])
    final_f1_hf[index_f1_hf,index_f1_hf_cols] <- l6_form_f1_hfsplitted[index_f1_hf_cols]
  }
}

final_f1_hf[is.na(final_f1_hf)] <- ""
f1_formmatrix <- cbind(f1_teams,final_f1_hf,suml6_f1_hf)
##############################################################################################
#f2
final_f2_hf <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
suml6_f2_hf <- c()
l6_form_f2_hfsplitted <- c()
form_f2_hf <- c()
for(index_f2_hf in 1:length(f2_teams))
{
  for(index_f2_hf_cols in 1:f2_totalrounds)
  {
    index_f2_hf  <- row.names(f2_form_h) == f2_teams[index_f2_hf]
    form_f2_hf <- f2_form_h[index_f2_hf ]
    deleted_form_f2_hf <- form_f2_hf[!form_f2_hf[] == ""]
    l6_form_f2_hf <- tail(deleted_form_f2_hf,f2_last_n_games)
    # l6_form_f2_hf <- as.numeric(l6_form_f2_hf)
    # suml6_f2_hf[index_f2_hf] <- sum(l6_form_f2_hf)
    # suml6_f2_hf[index_f2_hf] <- paste(suml6_f2_hf[index_f2_hf],sep = "")
    # l6_form_f2_hf <- as.character(l6_form_f2_hf)
    l6_form_f2_hf_flattened <- stri_paste(l6_form_f2_hf,collapse = '')
    l6_form_f2_hfsplitted <- (strsplit(as.character(l6_form_f2_hf_flattened),"")[[1]])
    final_f2_hf[index_f2_hf,index_f2_hf_cols] <- l6_form_f2_hfsplitted[index_f2_hf_cols]
  }
}

final_f2_hf[is.na(final_f2_hf)] <- ""
f2_formmatrix <- cbind(f2_teams,final_f2_hf,suml6_f2_hf)
##############################################################################################
#g1
final_g1_hf <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
suml6_g1_hf <- c()
l6_form_g1_hfsplitted <- c()
form_g1_hf <- c()
for(index_g1_hf in 1:length(g1_teams))
{
  for(index_g1_hf_cols in 1:g1_totalrounds)
  {
    index_g1_hf  <- row.names(g1_form_h) == g1_teams[index_g1_hf]
    form_g1_hf <- g1_form_h[index_g1_hf ]
    deleted_form_g1_hf <- form_g1_hf[!form_g1_hf[] == ""]
    l6_form_g1_hf <- tail(deleted_form_g1_hf,g1_last_n_games)
    # l6_form_g1_hf <- as.numeric(l6_form_g1_hf)
    # suml6_g1_hf[index_g1_hf] <- sum(l6_form_g1_hf)
    # suml6_g1_hf[index_g1_hf] <- paste(suml6_g1_hf[index_g1_hf],sep = "")
    # l6_form_g1_hf <- as.character(l6_form_g1_hf)
    l6_form_g1_hf_flattened <- stri_paste(l6_form_g1_hf,collapse = '')
    l6_form_g1_hfsplitted <- (strsplit(as.character(l6_form_g1_hf_flattened),"")[[1]])
    final_g1_hf[index_g1_hf,index_g1_hf_cols] <- l6_form_g1_hfsplitted[index_g1_hf_cols]
  }
}

final_g1_hf[is.na(final_g1_hf)] <- ""
g1_formmatrix <- cbind(g1_teams,final_g1_hf,suml6_g1_hf)
##############################################################################################
#i1
final_i1_hf <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_hf <- c()
l6_form_i1_hfsplitted <- c()
form_i1_hf <- c()
for(index_i1_hf in 1:length(i1_teams))
{
  for(index_i1_hf_cols in 1:i1_totalrounds)
  {
    index_i1_hf  <- row.names(i1_form_h) == i1_teams[index_i1_hf]
    form_i1_hf <- i1_form_h[index_i1_hf ]
    deleted_form_i1_hf <- form_i1_hf[!form_i1_hf[] == ""]
    l6_form_i1_hf <- tail(deleted_form_i1_hf,i1_last_n_games)
    # l6_form_i1_hf <- as.numeric(l6_form_i1_hf)
    # suml6_i1_hf[index_i1_hf] <- sum(l6_form_i1_hf)
    # suml6_i1_hf[index_i1_hf] <- paste(suml6_i1_hf[index_i1_hf],sep = "")
    # l6_form_i1_hf <- as.character(l6_form_i1_hf)
    l6_form_i1_hf_flattened <- stri_paste(l6_form_i1_hf,collapse = '')
    l6_form_i1_hfsplitted <- (strsplit(as.character(l6_form_i1_hf_flattened),"")[[1]])
    final_i1_hf[index_i1_hf,index_i1_hf_cols] <- l6_form_i1_hfsplitted[index_i1_hf_cols]
  }
}

final_i1_hf[is.na(final_i1_hf)] <- ""
i1_formmatrix <- cbind(i1_teams,final_i1_hf,suml6_i1_hf)
##############################################################################################
##i2
final_i2_hf <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
suml6_i2_hf <- c()
l6_form_i2_hfsplitted <- c()
form_i2_hf <- c()
for(index_i2_hf in 1:length(i2_teams))
{
  for(index_i2_hf_cols in 1:i2_totalrounds)
  {
    index_i2_hf  <- row.names(i2_form_h) == i2_teams[index_i2_hf]
    form_i2_hf <- i2_form_h[index_i2_hf ]
    deleted_form_i2_hf <- form_i2_hf[!form_i2_hf[] == ""]
    l6_form_i2_hf <- tail(deleted_form_i2_hf,i2_last_n_games)
    # l6_form_i2_hf <- as.numeric(l6_form_i2_hf)
    # suml6_i2_hf[index_i2_hf] <- sum(l6_form_i2_hf)
    # suml6_i2_hf[index_i2_hf] <- paste(suml6_i2_hf[index_i2_hf],sep = "")
    # l6_form_i2_hf <- as.character(l6_form_i2_hf)
    l6_form_i2_hf_flattened <- stri_paste(l6_form_i2_hf,collapse = '')
    l6_form_i2_hfsplitted <- (strsplit(as.character(l6_form_i2_hf_flattened),"")[[1]])
    final_i2_hf[index_i2_hf,index_i2_hf_cols] <- l6_form_i2_hfsplitted[index_i2_hf_cols]
  }
}

final_i2_hf[is.na(final_i2_hf)] <- ""
i2_formmatrix <- cbind(i2_teams,final_i2_hf,suml6_i2_hf)
##############################################################################################
#n1
final_n1_hf <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
suml6_n1_hf <- c()
l6_form_n1_hfsplitted <- c()
form_n1_hf <- c()
for(index_n1_hf in 1:length(n1_teams))
{
  for(index_n1_hf_cols in 1:n1_totalrounds)
  {
    index_n1_hf  <- row.names(n1_form_h) == n1_teams[index_n1_hf]
    form_n1_hf <- n1_form_h[index_n1_hf ]
    deleted_form_n1_hf <- form_n1_hf[!form_n1_hf[] == ""]
    l6_form_n1_hf <- tail(deleted_form_n1_hf,n1_last_n_games)
    # l6_form_n1_hf <- as.numeric(l6_form_n1_hf)
    # suml6_n1_hf[index_n1_hf] <- sum(l6_form_n1_hf)
    # suml6_n1_hf[index_n1_hf] <- paste(suml6_n1_hf[index_n1_hf],sep = "")
    # l6_form_n1_hf <- as.character(l6_form_n1_hf)
    l6_form_n1_hf_flattened <- stri_paste(l6_form_n1_hf,collapse = '')
    l6_form_n1_hfsplitted <- (strsplit(as.character(l6_form_n1_hf_flattened),"")[[1]])
    final_n1_hf[index_n1_hf,index_n1_hf_cols] <- l6_form_n1_hfsplitted[index_n1_hf_cols]
  }
}

final_n1_hf[is.na(final_n1_hf)] <- ""
n1_formmatrix <- cbind(n1_teams,final_n1_hf,suml6_n1_hf)
##############################################################################################
#p1
final_p1_hf <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
suml6_p1_hf <- c()
l6_form_p1_hfsplitted <- c()
form_p1_hf <- c()
for(index_p1_hf in 1:length(p1_teams))
{
  for(index_p1_hf_cols in 1:p1_totalrounds)
  {
    index_p1_hf  <- row.names(p1_form_h) == p1_teams[index_p1_hf]
    form_p1_hf <- p1_form_h[index_p1_hf ]
    deleted_form_p1_hf <- form_p1_hf[!form_p1_hf[] == ""]
    l6_form_p1_hf <- tail(deleted_form_p1_hf,p1_last_n_games)
    # l6_form_p1_hf <- as.numeric(l6_form_p1_hf)
    # suml6_p1_hf[index_p1_hf] <- sum(l6_form_p1_hf)
    # suml6_p1_hf[index_p1_hf] <- paste(suml6_p1_hf[index_p1_hf],sep = "")
    # l6_form_p1_hf <- as.character(l6_form_p1_hf)
    l6_form_p1_hf_flattened <- stri_paste(l6_form_p1_hf,collapse = '')
    l6_form_p1_hfsplitted <- (strsplit(as.character(l6_form_p1_hf_flattened),"")[[1]])
    final_p1_hf[index_p1_hf,index_p1_hf_cols] <- l6_form_p1_hfsplitted[index_p1_hf_cols]
  }
}

final_p1_hf[is.na(final_p1_hf)] <- ""
p1_formmatrix <- cbind(p1_teams,final_p1_hf,suml6_p1_hf)
##############################################################################################
#sp1
final_sp1_hf <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_hf <- c()
l6_form_sp1_hfsplitted <- c()
form_sp1_hf <- c()
for(index_sp1_hf in 1:length(sp1_teams))
{
  for(index_sp1_hf_cols in 1:sp1_totalrounds)
  {
    index_sp1_hf  <- row.names(sp1_form_h) == sp1_teams[index_sp1_hf]
    form_sp1_hf <- sp1_form_h[index_sp1_hf ]
    deleted_form_sp1_hf <- form_sp1_hf[!form_sp1_hf[] == ""]
    l6_form_sp1_hf <- tail(deleted_form_sp1_hf,sp1_last_n_games)
    # l6_form_sp1_hf <- as.numeric(l6_form_sp1_hf)
    # suml6_sp1_hf[index_sp1_hf] <- sum(l6_form_sp1_hf)
    # suml6_sp1_hf[index_sp1_hf] <- paste(suml6_sp1_hf[index_sp1_hf],sep = "")
    # l6_form_sp1_hf <- as.character(l6_form_sp1_hf)
    l6_form_sp1_hf_flattened <- stri_paste(l6_form_sp1_hf,collapse = '')
    l6_form_sp1_hfsplitted <- (strsplit(as.character(l6_form_sp1_hf_flattened),"")[[1]])
    final_sp1_hf[index_sp1_hf,index_sp1_hf_cols] <- l6_form_sp1_hfsplitted[index_sp1_hf_cols]
  }
}

final_sp1_hf[is.na(final_sp1_hf)] <- ""
sp1_formmatrix <- cbind(sp1_teams,final_sp1_hf,suml6_sp1_hf)
##############################################################################################
#sp2
final_sp2_hf <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
suml6_sp2_hf <- c()
l6_form_sp2_hfsplitted <- c()
form_sp2_hf <- c()
for(index_sp2_hf in 1:length(sp2_teams))
{
  for(index_sp2_hf_cols in 1:sp2_totalrounds)
  {
    index_sp2_hf  <- row.names(sp2_form_h) == sp2_teams[index_sp2_hf]
    form_sp2_hf <- sp2_form_h[index_sp2_hf ]
    deleted_form_sp2_hf <- form_sp2_hf[!form_sp2_hf[] == ""]
    l6_form_sp2_hf <- tail(deleted_form_sp2_hf,sp2_last_n_games)
    # l6_form_sp2_hf <- as.numeric(l6_form_sp2_hf)
    # suml6_sp2_hf[index_sp2_hf] <- sum(l6_form_sp2_hf)
    # suml6_sp2_hf[index_sp2_hf] <- paste(suml6_sp2_hf[index_sp2_hf],sep = "")
    # l6_form_sp2_hf <- as.character(l6_form_sp2_hf)
    l6_form_sp2_hf_flattened <- stri_paste(l6_form_sp2_hf,collapse = '')
    l6_form_sp2_hfsplitted <- (strsplit(as.character(l6_form_sp2_hf_flattened),"")[[1]])
    final_sp2_hf[index_sp2_hf,index_sp2_hf_cols] <- l6_form_sp2_hfsplitted[index_sp2_hf_cols]
  }
}

final_sp2_hf[is.na(final_sp2_hf)] <- ""
sp2_formmatrix <- cbind(sp2_teams,final_sp2_hf,suml6_sp2_hf)
##############################################################################################
#sc0
final_sc0_hf <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
suml6_sc0_hf <- c()
l6_form_sc0_hfsplitted <- c()
form_sc0_hf <- c()
for(index_sc0_hf in 1:length(sc0_teams))
{
  for(index_sc0_hf_cols in 1:sc0_totalrounds)
  {
    index_sc0_hf  <- row.names(sc0_form_h) == sc0_teams[index_sc0_hf]
    form_sc0_hf <- sc0_form_h[index_sc0_hf ]
    deleted_form_sc0_hf <- form_sc0_hf[!form_sc0_hf[] == ""]
    l6_form_sc0_hf <- tail(deleted_form_sc0_hf,sc0_last_n_games)
    # l6_form_sc0_hf <- as.numeric(l6_form_sc0_hf)
    # suml6_sc0_hf[index_sc0_hf] <- sum(l6_form_sc0_hf)
    # suml6_sc0_hf[index_sc0_hf] <- paste(suml6_sc0_hf[index_sc0_hf],sep = "")
    # l6_form_sc0_hf <- as.character(l6_form_sc0_hf)
    l6_form_sc0_hf_flattened <- stri_paste(l6_form_sc0_hf,collapse = '')
    l6_form_sc0_hfsplitted <- (strsplit(as.character(l6_form_sc0_hf_flattened),"")[[1]])
    final_sc0_hf[index_sc0_hf,index_sc0_hf_cols] <- l6_form_sc0_hfsplitted[index_sc0_hf_cols]
  }
}

final_sc0_hf[is.na(final_sc0_hf)] <- ""
sc0_formmatrix <- cbind(sc0_teams,final_sc0_hf,suml6_sc0_hf)
##############################################################################################
#sc1
final_sc1_hf <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
suml6_sc1_hf <- c()
l6_form_sc1_hfsplitted <- c()
form_sc1_hf <- c()
for(index_sc1_hf in 1:length(sc1_teams))
{
  for(index_sc1_hf_cols in 1:sc1_totalrounds)
  {
    index_sc1_hf  <- row.names(sc1_form_h) == sc1_teams[index_sc1_hf]
    form_sc1_hf <- sc1_form_h[index_sc1_hf ]
    deleted_form_sc1_hf <- form_sc1_hf[!form_sc1_hf[] == ""]
    l6_form_sc1_hf <- tail(deleted_form_sc1_hf,sc1_last_n_games)
    # l6_form_sc1_hf <- as.numeric(l6_form_sc1_hf)
    # suml6_sc1_hf[index_sc1_hf] <- sum(l6_form_sc1_hf)
    # suml6_sc1_hf[index_sc1_hf] <- paste(suml6_sc1_hf[index_sc1_hf],sep = "")
    # l6_form_sc1_hf <- as.character(l6_form_sc1_hf)
    l6_form_sc1_hf_flattened <- stri_paste(l6_form_sc1_hf,collapse = '')
    l6_form_sc1_hfsplitted <- (strsplit(as.character(l6_form_sc1_hf_flattened),"")[[1]])
    final_sc1_hf[index_sc1_hf,index_sc1_hf_cols] <- l6_form_sc1_hfsplitted[index_sc1_hf_cols]
  }
}

final_sc1_hf[is.na(final_sc1_hf)] <- ""
sc1_formmatrix <- cbind(sc1_teams,final_sc1_hf,suml6_sc1_hf)
##############################################################################################
#sc2
final_sc2_hf <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
suml6_sc2_hf <- c()
l6_form_sc2_hfsplitted <- c()
form_sc2_hf <- c()
for(index_sc2_hf in 1:length(sc2_teams))
{
  for(index_sc2_hf_cols in 1:sc2_totalrounds)
  {
    index_sc2_hf  <- row.names(sc2_form_h) == sc2_teams[index_sc2_hf]
    form_sc2_hf <- sc2_form_h[index_sc2_hf ]
    deleted_form_sc2_hf <- form_sc2_hf[!form_sc2_hf[] == ""]
    l6_form_sc2_hf <- tail(deleted_form_sc2_hf,sc2_last_n_games)
    # l6_form_sc2_hf <- as.numeric(l6_form_sc2_hf)
    # suml6_sc2_hf[index_sc2_hf] <- sum(l6_form_sc2_hf)
    # suml6_sc2_hf[index_sc2_hf] <- paste(suml6_sc2_hf[index_sc2_hf],sep = "")
    # l6_form_sc2_hf <- as.character(l6_form_sc2_hf)
    l6_form_sc2_hf_flattened <- stri_paste(l6_form_sc2_hf,collapse = '')
    l6_form_sc2_hfsplitted <- (strsplit(as.character(l6_form_sc2_hf_flattened),"")[[1]])
    final_sc2_hf[index_sc2_hf,index_sc2_hf_cols] <- l6_form_sc2_hfsplitted[index_sc2_hf_cols]
  }
}

final_sc2_hf[is.na(final_sc2_hf)] <- ""
sc2_formmatrix <- cbind(sc2_teams,final_sc2_hf,suml6_sc2_hf)
##############################################################################################
#sc3
final_sc3_hf <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
suml6_sc3_hf <- c()
l6_form_sc3_hfsplitted <- c()
form_sc3_hf <- c()
for(index_sc3_hf in 1:length(sc3_teams))
{
  for(index_sc3_hf_cols in 1:sc3_totalrounds)
  {
    index_sc3_hf  <- row.names(sc3_form_h) == sc3_teams[index_sc3_hf]
    form_sc3_hf <- sc3_form_h[index_sc3_hf ]
    deleted_form_sc3_hf <- form_sc3_hf[!form_sc3_hf[] == ""]
    l6_form_sc3_hf <- tail(deleted_form_sc3_hf,sc3_last_n_games)
    # l6_form_sc3_hf <- as.numeric(l6_form_sc3_hf)
    # suml6_sc3_hf[index_sc3_hf] <- sum(l6_form_sc3_hf)
    # suml6_sc3_hf[index_sc3_hf] <- paste(suml6_sc3_hf[index_sc3_hf],sep = "")
    # l6_form_sc3_hf <- as.character(l6_form_sc3_hf)
    l6_form_sc3_hf_flattened <- stri_paste(l6_form_sc3_hf,collapse = '')
    l6_form_sc3_hfsplitted <- (strsplit(as.character(l6_form_sc3_hf_flattened),"")[[1]])
    final_sc3_hf[index_sc3_hf,index_sc3_hf_cols] <- l6_form_sc3_hfsplitted[index_sc3_hf_cols]
  }
}

final_sc3_hf[is.na(final_sc3_hf)] <- ""
sc3_formmatrix <- cbind(sc3_teams,final_sc3_hf,suml6_sc3_hf)
##############################################################################################
#t1
final_t1_hf <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
suml6_t1_hf <- c()
l6_form_t1_hfsplitted <- c()
form_t1_hf <- c()
for(index_t1_hf in 1:length(t1_teams))
{
  for(index_t1_hf_cols in 1:t1_totalrounds)
  {
    index_t1_hf  <- row.names(t1_form_h) == t1_teams[index_t1_hf]
    form_t1_hf <- t1_form_h[index_t1_hf ]
    deleted_form_t1_hf <- form_t1_hf[!form_t1_hf[] == ""]
    l6_form_t1_hf <- tail(deleted_form_t1_hf,t1_last_n_games)
    # l6_form_t1_hf <- as.numeric(l6_form_t1_hf)
    # suml6_t1_hf[index_t1_hf] <- sum(l6_form_t1_hf)
    # suml6_t1_hf[index_t1_hf] <- paste(suml6_t1_hf[index_t1_hf],sep = "")
    l6_form_t1_hf <- as.character(l6_form_t1_hf)
    l6_form_t1_hf_flattened <- stri_paste(l6_form_t1_hf,collapse = '')
    l6_form_t1_hfsplitted <- (strsplit(as.character(l6_form_t1_hf_flattened),"")[[1]])
    final_t1_hf[index_t1_hf,index_t1_hf_cols] <- l6_form_t1_hfsplitted[index_t1_hf_cols]
  }
}

final_t1_hf[is.na(final_t1_hf)] <- ""
t1_formmatrix <- cbind(t1_teams,final_t1_hf,suml6_t1_hf)
##############################################################################################







































