library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('R.utils')
#B1
#form
#create final_b1_hf object
final_b1_hf <- c()
for(index_b1_hf in 1:length(b1_teams))
{
  index_b1_hf <- row.names(b1_form_h) == b1_teams[index_b1_hf]
  form_b1_hf <- b1_form_h[index_b1_hf]
  deleted_form_b1_hf <- form_b1_hf[!form_b1_hf[] == ""]
  l6_form_b1_hf <- tail(deleted_form_b1_hf,6)
  l6_form_b1_hf <- paste(l6_form_b1_hf,collapse = " ")
  final_b1_hf[index_b1_hf] <- rbind(paste(b1_teams[index_b1_hf],l6_form_b1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}

#change column names
final_b1_hf <- as.data.frame(final_b1_hf)
colnames(final_b1_hf) <- "Form"
#goals scored
#create final_b1_gs object
final_b1_gs <- c()
for(index_b1_gs in 1:length(b1_teams))
{
  index_b1_gs <- row.names(b1_goalscored_h) == b1_teams[index_b1_gs]
  form_b1_gs <- b1_goalscored_h[index_b1_gs]
  deleted_form_b1_gs <- form_b1_gs[!form_b1_gs[] == ""]
  l6_form_b1_gs <- tail(deleted_form_b1_gs,6)
  l6_form_b1_gs <- paste(l6_form_b1_gs,collapse = " ")
  final_b1_gs[index_b1_gs] <- rbind(paste(b1_teams[index_b1_gs],l6_form_b1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}

#change column names
final_b1_gs <- as.data.frame(final_b1_gs)
colnames(final_b1_gs) <- "Goals scored"
#goal conceded
#create final_b1_gc object
final_b1_gc <- c()
for(index_b1_gc in 1:length(b1_teams))
{
  index_b1_gc <- row.names(b1_goalconceded_h) == b1_teams[index_b1_gc]
  form_b1_gc <- b1_goalconceded_h[index_b1_gc]
  deleted_form_b1_gc <- form_b1_gc[!form_b1_gc[] == ""]
  l6_form_b1_gc <- tail(deleted_form_b1_gc,6)
  l6_form_b1_gc <- paste(l6_form_b1_gc,collapse = " ")
  final_b1_gc[index_b1_gc] <- rbind(paste(b1_teams[index_b1_gc],l6_form_b1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
#change column names
final_b1_gc <- as.data.frame(final_b1_gc)
colnames(final_b1_gc) <- "Goals conceded"
#total goals
#create final_b1_tg object
final_b1_tg <- c()
for(index_b1_tg in 1:length(b1_teams))
{
  index_b1_tg <- row.names(b1_totalgoals_h) == b1_teams[index_b1_tg]
  form_b1_tg <- b1_totalgoals_h[index_b1_tg]
  deleted_form_b1_tg <- form_b1_tg[!form_b1_tg[] == ""]
  l6_form_b1_tg <- tail(deleted_form_b1_tg,6)
  l6_form_b1_tg <- paste(l6_form_b1_tg,collapse = " ")
  final_b1_tg[index_b1_tg] <- rbind(paste(b1_teams[index_b1_tg],l6_form_b1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
#change column names
final_b1_tg <- as.data.frame(final_b1_tg)
colnames(final_b1_tg) <- "Total Goals"
#combine the columns
final_b1_all <- cbind(final_b1_hf,final_b1_gs,final_b1_gc,final_b1_tg)
write.xlsx(final_b1_all,'Divisions/B1.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#D1
#form
#create final_d1_hf object
final_d1_hf <- c()
for(index_d1_hf in 1:length(d1_teams))
{
  index_d1_hf <- row.names(d1_form_h) == d1_teams[index_d1_hf]
  form_d1_hf <- d1_form_h[index_d1_hf]
  deleted_form_d1_hf <- form_d1_hf[!form_d1_hf[] == ""]
  l6_form_d1_hf <- tail(deleted_form_d1_hf,6)
  l6_form_d1_hf <- paste(l6_form_d1_hf,collapse = " ")
  final_d1_hf[index_d1_hf] <- rbind(paste(d1_teams[index_d1_hf],l6_form_d1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d1_teams[index],l6_form)

}
#change column names
final_d1_hf <- as.data.frame(final_d1_hf)
colnames(final_d1_hf) <- "Form"
#goals scored
#create final_d1_gs object
final_d1_gs <- c()
for(index_d1_gs in 1:length(d1_teams))
{
  index_d1_gs <- row.names(d1_goalscored_h) == d1_teams[index_d1_gs]
  form_d1_gs <- d1_goalscored_h[index_d1_gs]
  deleted_form_d1_gs <- form_d1_gs[!form_d1_gs[] == ""]
  l6_form_d1_gs <- tail(deleted_form_d1_gs,6)
  l6_form_d1_gs <- paste(l6_form_d1_gs,collapse = " ")
  final_d1_gs[index_d1_gs] <- rbind(paste(d1_teams[index_d1_gs],l6_form_d1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d1_teams[index],l6_form)

}
#change column names
final_d1_gs <- as.data.frame(final_d1_gs)
colnames(final_d1_gs) <- "Goals scored"
#goal conceded
#create final_d1_gc object
final_d1_gc <- c()
for(index_d1_gc in 1:length(d1_teams))
{
  index_d1_gc <- row.names(d1_goalconceded_h) == d1_teams[index_d1_gc]
  form_d1_gc <- d1_goalconceded_h[index_d1_gc]
  deleted_form_d1_gc <- form_d1_gc[!form_d1_gc[] == ""]
  l6_form_d1_gc <- tail(deleted_form_d1_gc,6)
  l6_form_d1_gc <- paste(l6_form_d1_gc,collapse = " ")
  final_d1_gc[index_d1_gc] <- rbind(paste(d1_teams[index_d1_gc],l6_form_d1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d1_teams[index],l6_form)

}
#change column names
final_d1_gc <- as.data.frame(final_d1_gc)
colnames(final_d1_gc) <- "Goals conceded"
#total goals
#create final_d1_tg object
final_d1_tg <- c()
for(index_d1_tg in 1:length(d1_teams))
{
  index_d1_tg <- row.names(d1_totalgoals_h) == d1_teams[index_d1_tg]
  form_d1_tg <- d1_totalgoals_h[index_d1_tg]
  deleted_form_d1_tg <- form_d1_tg[!form_d1_tg[] == ""]
  l6_form_d1_tg <- tail(deleted_form_d1_tg,6)
  l6_form_d1_tg <- paste(l6_form_d1_tg,collapse = " ")
  final_d1_tg[index_d1_tg] <- rbind(paste(d1_teams[index_d1_tg],l6_form_d1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d1_teams[index],l6_form)

}
#change column names
final_d1_tg <- as.data.frame(final_d1_tg)
colnames(final_d1_tg) <- "Total Goals"
#combine the columns
final_d1_all <- cbind(final_d1_hf,final_d1_gs,final_d1_gc,final_d1_tg)
write.xlsx(final_d1_all,'Divisions/D1.xlsx',sheetName = "L6", append = TRUE)
############################################################################
#D2
#form
#create final_d2_hf object
final_d2_hf <- c()
for(index_d2_hf in 1:length(d2_teams))
{
  index_d2_hf <- row.names(d2_form_h) == d2_teams[index_d2_hf]
  form_d2_hf <- d2_form_h[index_d2_hf]
  deleted_form_d2_hf <- form_d2_hf[!form_d2_hf[] == ""]
  l6_form_d2_hf <- tail(deleted_form_d2_hf,6)
  l6_form_d2_hf <- paste(l6_form_d2_hf,collapse = " ")
  final_d2_hf[index_d2_hf] <- rbind(paste(d2_teams[index_d2_hf],l6_form_d2_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)

}
#change column names
final_d2_hf <- as.data.frame(final_d2_hf)
colnames(final_d2_hf) <- "Form"
#goals scored
#create final_d2_gs object
final_d2_gs <- c()
for(index_d2_gs in 1:length(d2_teams))
{
  index_d2_gs <- row.names(d2_goalscored_h) == d2_teams[index_d2_gs]
  form_d2_gs <- d2_goalscored_h[index_d2_gs]
  deleted_form_d2_gs <- form_d2_gs[!form_d2_gs[] == ""]
  l6_form_d2_gs <- tail(deleted_form_d2_gs,6)
  l6_form_d2_gs <- paste(l6_form_d2_gs,collapse = " ")
  final_d2_gs[index_d2_gs] <- rbind(paste(d2_teams[index_d2_gs],l6_form_d2_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)

}
#change column names
final_d2_gs <- as.data.frame(final_d2_gs)
colnames(final_d2_gs) <- "Goals scored"
#goal conceded
#create final_d2_gc object
final_d2_gc <- c()
for(index_d2_gc in 1:length(d2_teams))
{
  index_d2_gc <- row.names(d2_goalconceded_h) == d2_teams[index_d2_gc]
  form_d2_gc <- d2_goalconceded_h[index_d2_gc]
  deleted_form_d2_gc <- form_d2_gc[!form_d2_gc[] == ""]
  l6_form_d2_gc <- tail(deleted_form_d2_gc,6)
  l6_form_d2_gc <- paste(l6_form_d2_gc,collapse = " ")
  final_d2_gc[index_d2_gc] <- rbind(paste(d2_teams[index_d2_gc],l6_form_d2_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)

}
#change column names
final_d2_gc <- as.data.frame(final_d2_gc)
colnames(final_d2_gc) <- "Goals conceded"
#total goals
#create final_d2_tg object
final_d2_tg <- c()
for(index_d2_tg in 1:length(d2_teams))
{
  index_d2_tg <- row.names(d2_totalgoals_h) == d2_teams[index_d2_tg]
  form_d2_tg <- d2_totalgoals_h[index_d2_tg]
  deleted_form_d2_tg <- form_d2_tg[!form_d2_tg[] == ""]
  l6_form_d2_tg <- tail(deleted_form_d2_tg,6)
  l6_form_d2_tg <- paste(l6_form_d2_tg,collapse = " ")
  final_d2_tg[index_d2_tg] <- rbind(paste(d2_teams[index_d2_tg],l6_form_d2_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)

}
#change column names
final_d2_tg <- as.data.frame(final_d2_tg)
colnames(final_d2_tg) <- "Total Goals"
#combine the columns
final_d2_all <- cbind(final_d2_hf,final_d2_gs,final_d2_gc,final_d2_tg)
write.xlsx(final_d2_all,'Divisions/D2.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#E0
#form
#create final_e0_hf object
final_e0_hf <- c()
for(index_e0_hf in 1:length(e0_teams))
{
  index_e0_hf <- row.names(e0_form_h) == e0_teams[index_e0_hf]
  form_e0_hf <- e0_form_h[index_e0_hf]
  deleted_form_e0_hf <- form_e0_hf[!form_e0_hf[] == ""]
  l6_form_e0_hf <- tail(deleted_form_e0_hf,6)
  l6_form_e0_hf <- paste(l6_form_e0_hf,collapse = " ")
  final_e0_hf[index_e0_hf] <- rbind(paste(e0_teams[index_e0_hf],l6_form_e0_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)

}
#change column names
final_e0_hf <- as.data.frame(final_e0_hf)
colnames(final_e0_hf) <- "Form"
#goals scored
#create final_e0_gs object
final_e0_gs <- c()
for(index_e0_gs in 1:length(e0_teams))
{
  index_e0_gs <- row.names(e0_goalscored_h) == e0_teams[index_e0_gs]
  form_e0_gs <- e0_goalscored_h[index_e0_gs]
  deleted_form_e0_gs <- form_e0_gs[!form_e0_gs[] == ""]
  l6_form_e0_gs <- tail(deleted_form_e0_gs,6)
  l6_form_e0_gs <- paste(l6_form_e0_gs,collapse = " ")
  final_e0_gs[index_e0_gs] <- rbind(paste(e0_teams[index_e0_gs],l6_form_e0_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)

}
#change column names
final_e0_gs <- as.data.frame(final_e0_gs)
colnames(final_e0_gs) <- "Goals scored"
#goal conceded
#create final_e0_gc object
final_e0_gc <- c()
for(index_e0_gc in 1:length(e0_teams))
{
  index_e0_gc <- row.names(e0_goalconceded_h) == e0_teams[index_e0_gc]
  form_e0_gc <- e0_goalconceded_h[index_e0_gc]
  deleted_form_e0_gc <- form_e0_gc[!form_e0_gc[] == ""]
  l6_form_e0_gc <- tail(deleted_form_e0_gc,6)
  l6_form_e0_gc <- paste(l6_form_e0_gc,collapse = " ")
  final_e0_gc[index_e0_gc] <- rbind(paste(e0_teams[index_e0_gc],l6_form_e0_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)

}
#change column names
final_e0_gc <- as.data.frame(final_e0_gc)
colnames(final_e0_gc) <- "Goals conceded"
#total goals
#create final_e0_tg object
final_e0_tg <- c()
for(index_e0_tg in 1:length(e0_teams))
{
  index_e0_tg <- row.names(e0_totalgoals_h) == e0_teams[index_e0_tg]
  form_e0_tg <- e0_totalgoals_h[index_e0_tg]
  deleted_form_e0_tg <- form_e0_tg[!form_e0_tg[] == ""]
  l6_form_e0_tg <- tail(deleted_form_e0_tg,6)
  l6_form_e0_tg <- paste(l6_form_e0_tg,collapse = " ")
  final_e0_tg[index_e0_tg] <- rbind(paste(e0_teams[index_e0_tg],l6_form_e0_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)

}
#change column names
final_e0_tg <- as.data.frame(final_e0_tg)
colnames(final_e0_tg) <- "Total Goals"
#combine the columns
final_e0_all <- cbind(final_e0_hf,final_e0_gs,final_e0_gc,final_e0_tg)
write.xlsx(final_e0_all,'Divisions/E0.xlsx',sheetName = "L6", append = TRUE)
##################################################################################
#E1
#form
#create final_e1_hf object
final_e1_hf <- c()
for(index_e1_hf in 1:length(e1_teams))
{
  index_e1_hf <- row.names(e1_form_h) == e1_teams[index_e1_hf]
  form_e1_hf <- e1_form_h[index_e1_hf]
  deleted_form_e1_hf <- form_e1_hf[!form_e1_hf[] == ""]
  l6_form_e1_hf <- tail(deleted_form_e1_hf,6)
  l6_form_e1_hf <- paste(l6_form_e1_hf,collapse = " ")
  final_e1_hf[index_e1_hf] <- rbind(paste(e1_teams[index_e1_hf],l6_form_e1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)

}
#change column names
final_e1_hf <- as.data.frame(final_e1_hf)
colnames(final_e1_hf) <- "Form"
#goals scored
#create final_e1_gs object
final_e1_gs <- c()
for(index_e1_gs in 1:length(e1_teams))
{
  index_e1_gs <- row.names(e1_goalscored_h) == e1_teams[index_e1_gs]
  form_e1_gs <- e1_goalscored_h[index_e1_gs]
  deleted_form_e1_gs <- form_e1_gs[!form_e1_gs[] == ""]
  l6_form_e1_gs <- tail(deleted_form_e1_gs,6)
  l6_form_e1_gs <- paste(l6_form_e1_gs,collapse = " ")
  final_e1_gs[index_e1_gs] <- rbind(paste(e1_teams[index_e1_gs],l6_form_e1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)

}
#change column names
final_e1_gs <- as.data.frame(final_e1_gs)
colnames(final_e1_gs) <- "Goals scored"
#goal conceded
#create final_e1_gc object
final_e1_gc <- c()
for(index_e1_gc in 1:length(e1_teams))
{
  index_e1_gc <- row.names(e1_goalconceded_h) == e1_teams[index_e1_gc]
  form_e1_gc <- e1_goalconceded_h[index_e1_gc]
  deleted_form_e1_gc <- form_e1_gc[!form_e1_gc[] == ""]
  l6_form_e1_gc <- tail(deleted_form_e1_gc,6)
  l6_form_e1_gc <- paste(l6_form_e1_gc,collapse = " ")
  final_e1_gc[index_e1_gc] <- rbind(paste(e1_teams[index_e1_gc],l6_form_e1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)

}
#change column names
final_e1_gc <- as.data.frame(final_e1_gc)
colnames(final_e1_gc) <- "Goals conceded"
#total goals
#create final_e1_tg object
final_e1_tg <- c()
for(index_e1_tg in 1:length(e1_teams))
{
  index_e1_tg <- row.names(e1_totalgoals_h) == e1_teams[index_e1_tg]
  form_e1_tg <- e1_totalgoals_h[index_e1_tg]
  deleted_form_e1_tg <- form_e1_tg[!form_e1_tg[] == ""]
  l6_form_e1_tg <- tail(deleted_form_e1_tg,6)
  l6_form_e1_tg <- paste(l6_form_e1_tg,collapse = " ")
  final_e1_tg[index_e1_tg] <- rbind(paste(e1_teams[index_e1_tg],l6_form_e1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)

}
#change column names
final_e1_tg <- as.data.frame(final_e1_tg)
colnames(final_e1_tg) <- "Total Goals"
#combine the columns
final_e1_all <- cbind(final_e1_hf,final_e1_gs,final_e1_gc,final_e1_tg)
write.xlsx(final_e1_all,'Divisions/E1.xlsx',sheetName = "L6", append = TRUE)
###############################################################################
#E2
#form
#create final_e2_hf object
final_e2_hf <- c()
for(index_e2_hf in 1:length(e2_teams))
{
  index_e2_hf <- row.names(e2_form_h) == e2_teams[index_e2_hf]
  form_e2_hf <- e2_form_h[index_e2_hf]
  deleted_form_e2_hf <- form_e2_hf[!form_e2_hf[] == ""]
  l6_form_e2_hf <- tail(deleted_form_e2_hf,6)
  l6_form_e2_hf <- paste(l6_form_e2_hf,collapse = " ")
  final_e2_hf[index_e2_hf] <- rbind(paste(e2_teams[index_e2_hf],l6_form_e2_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)

}
#change column names
final_e2_hf <- as.data.frame(final_e2_hf)
colnames(final_e2_hf) <- "Form"
#goals scored
#create final_e2_gs object
final_e2_gs <- c()
for(index_e2_gs in 1:length(e2_teams))
{
  index_e2_gs <- row.names(e2_goalscored_h) == e2_teams[index_e2_gs]
  form_e2_gs <- e2_goalscored_h[index_e2_gs]
  deleted_form_e2_gs <- form_e2_gs[!form_e2_gs[] == ""]
  l6_form_e2_gs <- tail(deleted_form_e2_gs,6)
  l6_form_e2_gs <- paste(l6_form_e2_gs,collapse = " ")
  final_e2_gs[index_e2_gs] <- rbind(paste(e2_teams[index_e2_gs],l6_form_e2_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)

}
#change column names
final_e2_gs <- as.data.frame(final_e2_gs)
colnames(final_e2_gs) <- "Goals scored"
#goal conceded
#create final_e2_gc object
final_e2_gc <- c()
for(index_e2_gc in 1:length(e2_teams))
{
  index_e2_gc <- row.names(e2_goalconceded_h) == e2_teams[index_e2_gc]
  form_e2_gc <- e2_goalconceded_h[index_e2_gc]
  deleted_form_e2_gc <- form_e2_gc[!form_e2_gc[] == ""]
  l6_form_e2_gc <- tail(deleted_form_e2_gc,6)
  l6_form_e2_gc <- paste(l6_form_e2_gc,collapse = " ")
  final_e2_gc[index_e2_gc] <- rbind(paste(e2_teams[index_e2_gc],l6_form_e2_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)

}
#change column names
final_e2_gc <- as.data.frame(final_e2_gc)
colnames(final_e2_gc) <- "Goals conceded"
#total goals
#create final_e2_tg object
final_e2_tg <- c()
for(index_e2_tg in 1:length(e2_teams))
{
  index_e2_tg <- row.names(e2_totalgoals_h) == e2_teams[index_e2_tg]
  form_e2_tg <- e2_totalgoals_h[index_e2_tg]
  deleted_form_e2_tg <- form_e2_tg[!form_e2_tg[] == ""]
  l6_form_e2_tg <- tail(deleted_form_e2_tg,6)
  l6_form_e2_tg <- paste(l6_form_e2_tg,collapse = " ")
  final_e2_tg[index_e2_tg] <- rbind(paste(e2_teams[index_e2_tg],l6_form_e2_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)

}
#change column names
final_e2_tg <- as.data.frame(final_e2_tg)
colnames(final_e2_tg) <- "Total Goals"
#combine the columns
final_e2_all <- cbind(final_e2_hf,final_e2_gs,final_e2_gc,final_e2_tg)
write.xlsx(final_e2_all,'Divisions/E2.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#E3
#form
#create final_e3_hf object
final_e3_hf <- c()
for(index_e3_hf in 1:length(e3_teams))
{
  index_e3_hf <- row.names(e3_form_h) == e3_teams[index_e3_hf]
  form_e3_hf <- e3_form_h[index_e3_hf]
  deleted_form_e3_hf <- form_e3_hf[!form_e3_hf[] == ""]
  l6_form_e3_hf <- tail(deleted_form_e3_hf,6)
  l6_form_e3_hf <- paste(l6_form_e3_hf,collapse = " ")
  final_e3_hf[index_e3_hf] <- rbind(paste(e3_teams[index_e3_hf],l6_form_e3_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)

}
#change column names
final_e3_hf <- as.data.frame(final_e3_hf)
colnames(final_e3_hf) <- "Form"
#goals scored
#create final_e3_gs object
final_e3_gs <- c()
for(index_e3_gs in 1:length(e3_teams))
{
  index_e3_gs <- row.names(e3_goalscored_h) == e3_teams[index_e3_gs]
  form_e3_gs <- e3_goalscored_h[index_e3_gs]
  deleted_form_e3_gs <- form_e3_gs[!form_e3_gs[] == ""]
  l6_form_e3_gs <- tail(deleted_form_e3_gs,6)
  l6_form_e3_gs <- paste(l6_form_e3_gs,collapse = " ")
  final_e3_gs[index_e3_gs] <- rbind(paste(e3_teams[index_e3_gs],l6_form_e3_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)

}
#change column names
final_e3_gs <- as.data.frame(final_e3_gs)
colnames(final_e3_gs) <- "Goals scored"
#goal conceded
#create final_e3_gc object
final_e3_gc <- c()
for(index_e3_gc in 1:length(e3_teams))
{
  index_e3_gc <- row.names(e3_goalconceded_h) == e3_teams[index_e3_gc]
  form_e3_gc <- e3_goalconceded_h[index_e3_gc]
  deleted_form_e3_gc <- form_e3_gc[!form_e3_gc[] == ""]
  l6_form_e3_gc <- tail(deleted_form_e3_gc,6)
  l6_form_e3_gc <- paste(l6_form_e3_gc,collapse = " ")
  final_e3_gc[index_e3_gc] <- rbind(paste(e3_teams[index_e3_gc],l6_form_e3_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)

}
#change column names
final_e3_gc <- as.data.frame(final_e3_gc)
colnames(final_e3_gc) <- "Goals conceded"
#total goals
#create final_e3_tg object
final_e3_tg <- c()
for(index_e3_tg in 1:length(e3_teams))
{
  index_e3_tg <- row.names(e3_totalgoals_h) == e3_teams[index_e3_tg]
  form_e3_tg <- e3_totalgoals_h[index_e3_tg]
  deleted_form_e3_tg <- form_e3_tg[!form_e3_tg[] == ""]
  l6_form_e3_tg <- tail(deleted_form_e3_tg,6)
  l6_form_e3_tg <- paste(l6_form_e3_tg,collapse = " ")
  final_e3_tg[index_e3_tg] <- rbind(paste(e3_teams[index_e3_tg],l6_form_e3_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)

}
#change column names
final_e3_tg <- as.data.frame(final_e3_tg)
colnames(final_e3_tg) <- "Total Goals"
#combine the columns
final_e3_all <- cbind(final_e3_hf,final_e3_gs,final_e3_gc,final_e3_tg)
write.xlsx(final_e3_all,'Divisions/E3.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#EC
#form
#create final_ec_hf object
final_ec_hf <- c()
for(index_ec_hf in 1:length(ec_teams))
{
  index_ec_hf <- row.names(ec_form_h) == ec_teams[index_ec_hf]
  form_ec_hf <- ec_form_h[index_ec_hf]
  deleted_form_ec_hf <- form_ec_hf[!form_ec_hf[] == ""]
  l6_form_ec_hf <- tail(deleted_form_ec_hf,6)
  l6_form_ec_hf <- paste(l6_form_ec_hf,collapse = " ")
  final_ec_hf[index_ec_hf] <- rbind(paste(ec_teams[index_ec_hf],l6_form_ec_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)

}
#change column names
final_ec_hf <- as.data.frame(final_ec_hf)
colnames(final_ec_hf) <- "Form"
#goals scored
#create final_ec_gs object
final_ec_gs <- c()
for(index_ec_gs in 1:length(ec_teams))
{
  index_ec_gs <- row.names(ec_goalscored_h) == ec_teams[index_ec_gs]
  form_ec_gs <- ec_goalscored_h[index_ec_gs]
  deleted_form_ec_gs <- form_ec_gs[!form_ec_gs[] == ""]
  l6_form_ec_gs <- tail(deleted_form_ec_gs,6)
  l6_form_ec_gs <- paste(l6_form_ec_gs,collapse = " ")
  final_ec_gs[index_ec_gs] <- rbind(paste(ec_teams[index_ec_gs],l6_form_ec_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)

}
#change column names
final_ec_gs <- as.data.frame(final_ec_gs)
colnames(final_ec_gs) <- "Goals scored"
#goal conceded
#create final_ec_gc object
final_ec_gc <- c()
for(index_ec_gc in 1:length(ec_teams))
{
  index_ec_gc <- row.names(ec_goalconceded_h) == ec_teams[index_ec_gc]
  form_ec_gc <- ec_goalconceded_h[index_ec_gc]
  deleted_form_ec_gc <- form_ec_gc[!form_ec_gc[] == ""]
  l6_form_ec_gc <- tail(deleted_form_ec_gc,6)
  l6_form_ec_gc <- paste(l6_form_ec_gc,collapse = " ")
  final_ec_gc[index_ec_gc] <- rbind(paste(ec_teams[index_ec_gc],l6_form_ec_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)

}
#change column names
final_ec_gc <- as.data.frame(final_ec_gc)
colnames(final_ec_gc) <- "Goals conceded"
#total goals
#create final_ec_tg object
final_ec_tg <- c()
for(index_ec_tg in 1:length(ec_teams))
{
  index_ec_tg <- row.names(ec_totalgoals_h) == ec_teams[index_ec_tg]
  form_ec_tg <- ec_totalgoals_h[index_ec_tg]
  deleted_form_ec_tg <- form_ec_tg[!form_ec_tg[] == ""]
  l6_form_ec_tg <- tail(deleted_form_ec_tg,6)
  l6_form_ec_tg <- paste(l6_form_ec_tg,collapse = " ")
  final_ec_tg[index_ec_tg] <- rbind(paste(ec_teams[index_ec_tg],l6_form_ec_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)

}
#change column names
final_ec_tg <- as.data.frame(final_ec_tg)
colnames(final_ec_tg) <- "Total Goals"
#combine the columns
final_ec_all <- cbind(final_ec_hf,final_ec_gs,final_ec_gc,final_ec_tg)
write.xlsx(final_ec_all,'Divisions/EC.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#F1
#form
#create final_f1_hf object
final_f1_hf <- c()
for(index_f1_hf in 1:length(f1_teams))
{
  index_f1_hf <- row.names(f1_form_h) == f1_teams[index_f1_hf]
  form_f1_hf <- f1_form_h[index_f1_hf]
  deleted_form_f1_hf <- form_f1_hf[!form_f1_hf[] == ""]
  l6_form_f1_hf <- tail(deleted_form_f1_hf,6)
  l6_form_f1_hf <- paste(l6_form_f1_hf,collapse = " ")
  final_f1_hf[index_f1_hf] <- rbind(paste(f1_teams[index_f1_hf],l6_form_f1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)

}
#change column names
final_f1_hf <- as.data.frame(final_f1_hf)
colnames(final_f1_hf) <- "Form"
#goals scored
#create final_f1_gs object
final_f1_gs <- c()
for(index_f1_gs in 1:length(f1_teams))
{
  index_f1_gs <- row.names(f1_goalscored_h) == f1_teams[index_f1_gs]
  form_f1_gs <- f1_goalscored_h[index_f1_gs]
  deleted_form_f1_gs <- form_f1_gs[!form_f1_gs[] == ""]
  l6_form_f1_gs <- tail(deleted_form_f1_gs,6)
  l6_form_f1_gs <- paste(l6_form_f1_gs,collapse = " ")
  final_f1_gs[index_f1_gs] <- rbind(paste(f1_teams[index_f1_gs],l6_form_f1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)

}
#change column names
final_f1_gs <- as.data.frame(final_f1_gs)
colnames(final_f1_gs) <- "Goals scored"
#goal conceded
#create final_f1_gc object
final_f1_gc <- c()
for(index_f1_gc in 1:length(f1_teams))
{
  index_f1_gc <- row.names(f1_goalconceded_h) == f1_teams[index_f1_gc]
  form_f1_gc <- f1_goalconceded_h[index_f1_gc]
  deleted_form_f1_gc <- form_f1_gc[!form_f1_gc[] == ""]
  l6_form_f1_gc <- tail(deleted_form_f1_gc,6)
  l6_form_f1_gc <- paste(l6_form_f1_gc,collapse = " ")
  final_f1_gc[index_f1_gc] <- rbind(paste(f1_teams[index_f1_gc],l6_form_f1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)

}
#change column names
final_f1_gc <- as.data.frame(final_f1_gc)
colnames(final_f1_gc) <- "Goals conceded"
#total goals
#create final_f1_tg object
final_f1_tg <- c()
for(index_f1_tg in 1:length(f1_teams))
{
  index_f1_tg <- row.names(f1_totalgoals_h) == f1_teams[index_f1_tg]
  form_f1_tg <- f1_totalgoals_h[index_f1_tg]
  deleted_form_f1_tg <- form_f1_tg[!form_f1_tg[] == ""]
  l6_form_f1_tg <- tail(deleted_form_f1_tg,6)
  l6_form_f1_tg <- paste(l6_form_f1_tg,collapse = " ")
  final_f1_tg[index_f1_tg] <- rbind(paste(f1_teams[index_f1_tg],l6_form_f1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)

}
#change column names
final_f1_tg <- as.data.frame(final_f1_tg)
colnames(final_f1_tg) <- "Total Goals"
#combine the columns
final_f1_all <- cbind(final_f1_hf,final_f1_gs,final_f1_gc,final_f1_tg)
write.xlsx(final_f1_all,'Divisions/F1.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#F2
#form
#create final_f2_hf object
final_f2_hf <- c()
for(index_f2_hf in 1:length(f2_teams))
{
  index_f2_hf <- row.names(f2_form_h) == f2_teams[index_f2_hf]
  form_f2_hf <- f2_form_h[index_f2_hf]
  deleted_form_f2_hf <- form_f2_hf[!form_f2_hf[] == ""]
  l6_form_f2_hf <- tail(deleted_form_f2_hf,6)
  l6_form_f2_hf <- paste(l6_form_f2_hf,collapse = " ")
  final_f2_hf[index_f2_hf] <- rbind(paste(f2_teams[index_f2_hf],l6_form_f2_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)

}
#change column names
final_f2_hf <- as.data.frame(final_f2_hf)
colnames(final_f2_hf) <- "Form"
#goals scored
#create final_f2_gs object
final_f2_gs <- c()
for(index_f2_gs in 1:length(f2_teams))
{
  index_f2_gs <- row.names(f2_goalscored_h) == f2_teams[index_f2_gs]
  form_f2_gs <- f2_goalscored_h[index_f2_gs]
  deleted_form_f2_gs <- form_f2_gs[!form_f2_gs[] == ""]
  l6_form_f2_gs <- tail(deleted_form_f2_gs,6)
  l6_form_f2_gs <- paste(l6_form_f2_gs,collapse = " ")
  final_f2_gs[index_f2_gs] <- rbind(paste(f2_teams[index_f2_gs],l6_form_f2_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)

}
#change column names
final_f2_gs <- as.data.frame(final_f2_gs)
colnames(final_f2_gs) <- "Goals scored"
#goal conceded
#create final_f2_gc object
final_f2_gc <- c()
for(index_f2_gc in 1:length(f2_teams))
{
  index_f2_gc <- row.names(f2_goalconceded_h) == f2_teams[index_f2_gc]
  form_f2_gc <- f2_goalconceded_h[index_f2_gc]
  deleted_form_f2_gc <- form_f2_gc[!form_f2_gc[] == ""]
  l6_form_f2_gc <- tail(deleted_form_f2_gc,6)
  l6_form_f2_gc <- paste(l6_form_f2_gc,collapse = " ")
  final_f2_gc[index_f2_gc] <- rbind(paste(f2_teams[index_f2_gc],l6_form_f2_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)

}
#change column names
final_f2_gc <- as.data.frame(final_f2_gc)
colnames(final_f2_gc) <- "Goals conceded"
#total goals
#create final_f2_tg object
final_f2_tg <- c()
for(index_f2_tg in 1:length(f2_teams))
{
  index_f2_tg <- row.names(f2_totalgoals_h) == f2_teams[index_f2_tg]
  form_f2_tg <- f2_totalgoals_h[index_f2_tg]
  deleted_form_f2_tg <- form_f2_tg[!form_f2_tg[] == ""]
  l6_form_f2_tg <- tail(deleted_form_f2_tg,6)
  l6_form_f2_tg <- paste(l6_form_f2_tg,collapse = " ")
  final_f2_tg[index_f2_tg] <- rbind(paste(f2_teams[index_f2_tg],l6_form_f2_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)

}
#change column names
final_f2_tg <- as.data.frame(final_f2_tg)
colnames(final_f2_tg) <- "Total Goals"
#combine the columns
final_f2_all <- cbind(final_f2_hf,final_f2_gs,final_f2_gc,final_f2_tg)
write.xlsx(final_f2_all,'Divisions/F2.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#G1
#form
#create final_g1_hf object
final_g1_hf <- c()
for(index_g1_hf in 1:length(g1_teams))
{
  index_g1_hf <- row.names(g1_form_h) == g1_teams[index_g1_hf]
  form_g1_hf <- g1_form_h[index_g1_hf]
  deleted_form_g1_hf <- form_g1_hf[!form_g1_hf[] == ""]
  l6_form_g1_hf <- tail(deleted_form_g1_hf,6)
  l6_form_g1_hf <- paste(l6_form_g1_hf,collapse = " ")
  final_g1_hf[index_g1_hf] <- rbind(paste(g1_teams[index_g1_hf],l6_form_g1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)

}
#change column names
final_g1_hf <- as.data.frame(final_g1_hf)
colnames(final_g1_hf) <- "Form"
#goals scored
#create final_g1_gs object
final_g1_gs <- c()
for(index_g1_gs in 1:length(g1_teams))
{
  index_g1_gs <- row.names(g1_goalscored_h) == g1_teams[index_g1_gs]
  form_g1_gs <- g1_goalscored_h[index_g1_gs]
  deleted_form_g1_gs <- form_g1_gs[!form_g1_gs[] == ""]
  l6_form_g1_gs <- tail(deleted_form_g1_gs,6)
  l6_form_g1_gs <- paste(l6_form_g1_gs,collapse = " ")
  final_g1_gs[index_g1_gs] <- rbind(paste(g1_teams[index_g1_gs],l6_form_g1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)

}
#change column names
final_g1_gs <- as.data.frame(final_g1_gs)
colnames(final_g1_gs) <- "Goals scored"
#goal conceded
#create final_g1_gc object
final_g1_gc <- c()
for(index_g1_gc in 1:length(g1_teams))
{
  index_g1_gc <- row.names(g1_goalconceded_h) == g1_teams[index_g1_gc]
  form_g1_gc <- g1_goalconceded_h[index_g1_gc]
  deleted_form_g1_gc <- form_g1_gc[!form_g1_gc[] == ""]
  l6_form_g1_gc <- tail(deleted_form_g1_gc,6)
  l6_form_g1_gc <- paste(l6_form_g1_gc,collapse = " ")
  final_g1_gc[index_g1_gc] <- rbind(paste(g1_teams[index_g1_gc],l6_form_g1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)

}
#change column names
final_g1_gc <- as.data.frame(final_g1_gc)
colnames(final_g1_gc) <- "Goals conceded"
#total goals
#create final_g1_tg object
final_g1_tg <- c()
for(index_g1_tg in 1:length(g1_teams))
{
  index_g1_tg <- row.names(g1_totalgoals_h) == g1_teams[index_g1_tg]
  form_g1_tg <- g1_totalgoals_h[index_g1_tg]
  deleted_form_g1_tg <- form_g1_tg[!form_g1_tg[] == ""]
  l6_form_g1_tg <- tail(deleted_form_g1_tg,6)
  l6_form_g1_tg <- paste(l6_form_g1_tg,collapse = " ")
  final_g1_tg[index_g1_tg] <- rbind(paste(g1_teams[index_g1_tg],l6_form_g1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)

}
#change column names
final_g1_tg <- as.data.frame(final_g1_tg)
colnames(final_g1_tg) <- "Total Goals"
#combine the columns
final_g1_all <- cbind(final_g1_hf,final_g1_gs,final_g1_gc,final_g1_tg)
write.xlsx(final_g1_all,'Divisions/G1.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#I1
#form
#create final_i1_hf object
final_i1_hf <- c()
for(index_i1_hf in 1:length(i1_teams))
{
  index_i1_hf <- row.names(i1_form_h) == i1_teams[index_i1_hf]
  form_i1_hf <- i1_form_h[index_i1_hf]
  deleted_form_i1_hf <- form_i1_hf[!form_i1_hf[] == ""]
  l6_form_i1_hf <- tail(deleted_form_i1_hf,6)
  l6_form_i1_hf <- paste(l6_form_i1_hf,collapse = " ")
  final_i1_hf[index_i1_hf] <- rbind(paste(i1_teams[index_i1_hf],l6_form_i1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)

}
#change column names
final_i1_hf <- as.data.frame(final_i1_hf)
colnames(final_i1_hf) <- "Form"
#goals scored
#create final_i1_gs object
final_i1_gs <- c()
for(index_i1_gs in 1:length(i1_teams))
{
  index_i1_gs <- row.names(i1_goalscored_h) == i1_teams[index_i1_gs]
  form_i1_gs <- i1_goalscored_h[index_i1_gs]
  deleted_form_i1_gs <- form_i1_gs[!form_i1_gs[] == ""]
  l6_form_i1_gs <- tail(deleted_form_i1_gs,6)
  l6_form_i1_gs <- paste(l6_form_i1_gs,collapse = " ")
  final_i1_gs[index_i1_gs] <- rbind(paste(i1_teams[index_i1_gs],l6_form_i1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)

}
#change column names
final_i1_gs <- as.data.frame(final_i1_gs)
colnames(final_i1_gs) <- "Goals scored"
#goal conceded
#create final_i1_gc object
final_i1_gc <- c()
for(index_i1_gc in 1:length(i1_teams))
{
  index_i1_gc <- row.names(i1_goalconceded_h) == i1_teams[index_i1_gc]
  form_i1_gc <- i1_goalconceded_h[index_i1_gc]
  deleted_form_i1_gc <- form_i1_gc[!form_i1_gc[] == ""]
  l6_form_i1_gc <- tail(deleted_form_i1_gc,6)
  l6_form_i1_gc <- paste(l6_form_i1_gc,collapse = " ")
  final_i1_gc[index_i1_gc] <- rbind(paste(i1_teams[index_i1_gc],l6_form_i1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)

}
#change column names
final_i1_gc <- as.data.frame(final_i1_gc)
colnames(final_i1_gc) <- "Goals conceded"
#total goals
#create final_i1_tg object
final_i1_tg <- c()
for(index_i1_tg in 1:length(i1_teams))
{
  index_i1_tg <- row.names(i1_totalgoals_h) == i1_teams[index_i1_tg]
  form_i1_tg <- i1_totalgoals_h[index_i1_tg]
  deleted_form_i1_tg <- form_i1_tg[!form_i1_tg[] == ""]
  l6_form_i1_tg <- tail(deleted_form_i1_tg,6)
  l6_form_i1_tg <- paste(l6_form_i1_tg,collapse = " ")
  final_i1_tg[index_i1_tg] <- rbind(paste(i1_teams[index_i1_tg],l6_form_i1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)

}
#change column names
final_i1_tg <- as.data.frame(final_i1_tg)
colnames(final_i1_tg) <- "Total Goals"
#combine the columns
final_i1_all <- cbind(final_i1_hf,final_i1_gs,final_i1_gc,final_i1_tg)
write.xlsx(final_i1_all,'Divisions/I1.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#I2
#form
#create final_i2_hf object
final_i2_hf <- c()
for(index_i2_hf in 1:length(i2_teams))
{
  index_i2_hf <- row.names(i2_form_h) == i2_teams[index_i2_hf]
  form_i2_hf <- i2_form_h[index_i2_hf]
  deleted_form_i2_hf <- form_i2_hf[!form_i2_hf[] == ""]
  l6_form_i2_hf <- tail(deleted_form_i2_hf,6)
  l6_form_i2_hf <- paste(l6_form_i2_hf,collapse = " ")
  final_i2_hf[index_i2_hf] <- rbind(paste(i2_teams[index_i2_hf],l6_form_i2_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)

}
#change column names
final_i2_hf <- as.data.frame(final_i2_hf)
colnames(final_i2_hf) <- "Form"
#goals scored
#create final_i2_gs object
final_i2_gs <- c()
for(index_i2_gs in 1:length(i2_teams))
{
  index_i2_gs <- row.names(i2_goalscored_h) == i2_teams[index_i2_gs]
  form_i2_gs <- i2_goalscored_h[index_i2_gs]
  deleted_form_i2_gs <- form_i2_gs[!form_i2_gs[] == ""]
  l6_form_i2_gs <- tail(deleted_form_i2_gs,6)
  l6_form_i2_gs <- paste(l6_form_i2_gs,collapse = " ")
  final_i2_gs[index_i2_gs] <- rbind(paste(i2_teams[index_i2_gs],l6_form_i2_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)

}
#change column names
final_i2_gs <- as.data.frame(final_i2_gs)
colnames(final_i2_gs) <- "Goals scored"
#goal conceded
#create final_i2_gc object
final_i2_gc <- c()
for(index_i2_gc in 1:length(i2_teams))
{
  index_i2_gc <- row.names(i2_goalconceded_h) == i2_teams[index_i2_gc]
  form_i2_gc <- i2_goalconceded_h[index_i2_gc]
  deleted_form_i2_gc <- form_i2_gc[!form_i2_gc[] == ""]
  l6_form_i2_gc <- tail(deleted_form_i2_gc,6)
  l6_form_i2_gc <- paste(l6_form_i2_gc,collapse = " ")
  final_i2_gc[index_i2_gc] <- rbind(paste(i2_teams[index_i2_gc],l6_form_i2_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)

}
#change column names
final_i2_gc <- as.data.frame(final_i2_gc)
colnames(final_i2_gc) <- "Goals conceded"
#total goals
#create final_i2_tg object
final_i2_tg <- c()
for(index_i2_tg in 1:length(i2_teams))
{
  index_i2_tg <- row.names(i2_totalgoals_h) == i2_teams[index_i2_tg]
  form_i2_tg <- i2_totalgoals_h[index_i2_tg]
  deleted_form_i2_tg <- form_i2_tg[!form_i2_tg[] == ""]
  l6_form_i2_tg <- tail(deleted_form_i2_tg,6)
  l6_form_i2_tg <- paste(l6_form_i2_tg,collapse = " ")
  final_i2_tg[index_i2_tg] <- rbind(paste(i2_teams[index_i2_tg],l6_form_i2_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)

}
#change column names
final_i2_tg <- as.data.frame(final_i2_tg)
colnames(final_i2_tg) <- "Total Goals"
#combine the columns
final_i2_all <- cbind(final_i2_hf,final_i2_gs,final_i2_gc,final_i2_tg)
write.xlsx(final_i2_all,'Divisions/I2.xlsx',sheetName = "L6", append = TRUE)
###############################################################################
#N1
#form
#create final_n1_hf object
final_n1_hf <- c()
for(index_n1_hf in 1:length(n1_teams))
{
  index_n1_hf <- row.names(n1_form_h) == n1_teams[index_n1_hf]
  form_n1_hf <- n1_form_h[index_n1_hf]
  deleted_form_n1_hf <- form_n1_hf[!form_n1_hf[] == ""]
  l6_form_n1_hf <- tail(deleted_form_n1_hf,6)
  l6_form_n1_hf <- paste(l6_form_n1_hf,collapse = " ")
  final_n1_hf[index_n1_hf] <- rbind(paste(n1_teams[index_n1_hf],l6_form_n1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)

}
#change column names
final_n1_hf <- as.data.frame(final_n1_hf)
colnames(final_n1_hf) <- "Form"
#goals scored
#create final_n1_gs object
final_n1_gs <- c()
for(index_n1_gs in 1:length(n1_teams))
{
  index_n1_gs <- row.names(n1_goalscored_h) == n1_teams[index_n1_gs]
  form_n1_gs <- n1_goalscored_h[index_n1_gs]
  deleted_form_n1_gs <- form_n1_gs[!form_n1_gs[] == ""]
  l6_form_n1_gs <- tail(deleted_form_n1_gs,6)
  l6_form_n1_gs <- paste(l6_form_n1_gs,collapse = " ")
  final_n1_gs[index_n1_gs] <- rbind(paste(n1_teams[index_n1_gs],l6_form_n1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)

}
#change column names
final_n1_gs <- as.data.frame(final_n1_gs)
colnames(final_n1_gs) <- "Goals scored"
#goal conceded
#create final_n1_gc object
final_n1_gc <- c()
for(index_n1_gc in 1:length(n1_teams))
{
  index_n1_gc <- row.names(n1_goalconceded_h) == n1_teams[index_n1_gc]
  form_n1_gc <- n1_goalconceded_h[index_n1_gc]
  deleted_form_n1_gc <- form_n1_gc[!form_n1_gc[] == ""]
  l6_form_n1_gc <- tail(deleted_form_n1_gc,6)
  l6_form_n1_gc <- paste(l6_form_n1_gc,collapse = " ")
  final_n1_gc[index_n1_gc] <- rbind(paste(n1_teams[index_n1_gc],l6_form_n1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)

}
#change column names
final_n1_gc <- as.data.frame(final_n1_gc)
colnames(final_n1_gc) <- "Goals conceded"
#total goals
#create final_n1_tg object
final_n1_tg <- c()
for(index_n1_tg in 1:length(n1_teams))
{
  index_n1_tg <- row.names(n1_totalgoals_h) == n1_teams[index_n1_tg]
  form_n1_tg <- n1_totalgoals_h[index_n1_tg]
  deleted_form_n1_tg <- form_n1_tg[!form_n1_tg[] == ""]
  l6_form_n1_tg <- tail(deleted_form_n1_tg,6)
  l6_form_n1_tg <- paste(l6_form_n1_tg,collapse = " ")
  final_n1_tg[index_n1_tg] <- rbind(paste(n1_teams[index_n1_tg],l6_form_n1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)

}
#change column names
final_n1_tg <- as.data.frame(final_n1_tg)
colnames(final_n1_tg) <- "Total Goals"
#combine the columns
final_n1_all <- cbind(final_n1_hf,final_n1_gs,final_n1_gc,final_n1_tg)
write.xlsx(final_n1_all,'Divisions/N1.xlsx',sheetName = "L6", append = TRUE)
#################################################################################
#P1
#form
#create final_p1_hf object
final_p1_hf <- c()
for(index_p1_hf in 1:length(p1_teams))
{
  index_p1_hf <- row.names(p1_form_h) == p1_teams[index_p1_hf]
  form_p1_hf <- p1_form_h[index_p1_hf]
  deleted_form_p1_hf <- form_p1_hf[!form_p1_hf[] == ""]
  l6_form_p1_hf <- tail(deleted_form_p1_hf,6)
  l6_form_p1_hf <- paste(l6_form_p1_hf,collapse = " ")
  final_p1_hf[index_p1_hf] <- rbind(paste(p1_teams[index_p1_hf],l6_form_p1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)

}
#change column names
final_p1_hf <- as.data.frame(final_p1_hf)
colnames(final_p1_hf) <- "Form"
#goals scored
#create final_p1_gs object
final_p1_gs <- c()
for(index_p1_gs in 1:length(p1_teams))
{
  index_p1_gs <- row.names(p1_goalscored_h) == p1_teams[index_p1_gs]
  form_p1_gs <- p1_goalscored_h[index_p1_gs]
  deleted_form_p1_gs <- form_p1_gs[!form_p1_gs[] == ""]
  l6_form_p1_gs <- tail(deleted_form_p1_gs,6)
  l6_form_p1_gs <- paste(l6_form_p1_gs,collapse = " ")
  final_p1_gs[index_p1_gs] <- rbind(paste(p1_teams[index_p1_gs],l6_form_p1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)

}
#change column names
final_p1_gs <- as.data.frame(final_p1_gs)
colnames(final_p1_gs) <- "Goals scored"
#goal conceded
#create final_p1_gc object
final_p1_gc <- c()
for(index_p1_gc in 1:length(p1_teams))
{
  index_p1_gc <- row.names(p1_goalconceded_h) == p1_teams[index_p1_gc]
  form_p1_gc <- p1_goalconceded_h[index_p1_gc]
  deleted_form_p1_gc <- form_p1_gc[!form_p1_gc[] == ""]
  l6_form_p1_gc <- tail(deleted_form_p1_gc,6)
  l6_form_p1_gc <- paste(l6_form_p1_gc,collapse = " ")
  final_p1_gc[index_p1_gc] <- rbind(paste(p1_teams[index_p1_gc],l6_form_p1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)

}
#change column names
final_p1_gc <- as.data.frame(final_p1_gc)
colnames(final_p1_gc) <- "Goals conceded"
#total goals
#create final_p1_tg object
final_p1_tg <- c()
for(index_p1_tg in 1:length(p1_teams))
{
  index_p1_tg <- row.names(p1_totalgoals_h) == p1_teams[index_p1_tg]
  form_p1_tg <- p1_totalgoals_h[index_p1_tg]
  deleted_form_p1_tg <- form_p1_tg[!form_p1_tg[] == ""]
  l6_form_p1_tg <- tail(deleted_form_p1_tg,6)
  l6_form_p1_tg <- paste(l6_form_p1_tg,collapse = " ")
  final_p1_tg[index_p1_tg] <- rbind(paste(p1_teams[index_p1_tg],l6_form_p1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)

}
#change column names
final_p1_tg <- as.data.frame(final_p1_tg)
colnames(final_p1_tg) <- "Total Goals"
#combine the columns
final_p1_all <- cbind(final_p1_hf,final_p1_gs,final_p1_gc,final_p1_tg)
write.xlsx(final_p1_all,'Divisions/P1.xlsx',sheetName = "L6", append = TRUE)
########################################################################################
#SC0
#form
#create final_sc0_hf object
final_sc0_hf <- c()
for(index_sc0_hf in 1:length(sc0_teams))
{
  index_sc0_hf <- row.names(sc0_form_h) == sc0_teams[index_sc0_hf]
  form_sc0_hf <- sc0_form_h[index_sc0_hf]
  deleted_form_sc0_hf <- form_sc0_hf[!form_sc0_hf[] == ""]
  l6_form_sc0_hf <- tail(deleted_form_sc0_hf,6)
  l6_form_sc0_hf <- paste(l6_form_sc0_hf,collapse = " ")
  final_sc0_hf[index_sc0_hf] <- rbind(paste(sc0_teams[index_sc0_hf],l6_form_sc0_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)

}
#change column names
final_sc0_hf <- as.data.frame(final_sc0_hf)
colnames(final_sc0_hf) <- "Form"
#goals scored
#create final_sc0_gs object
final_sc0_gs <- c()
for(index_sc0_gs in 1:length(sc0_teams))
{
  index_sc0_gs <- row.names(sc0_goalscored_h) == sc0_teams[index_sc0_gs]
  form_sc0_gs <- sc0_goalscored_h[index_sc0_gs]
  deleted_form_sc0_gs <- form_sc0_gs[!form_sc0_gs[] == ""]
  l6_form_sc0_gs <- tail(deleted_form_sc0_gs,6)
  l6_form_sc0_gs <- paste(l6_form_sc0_gs,collapse = " ")
  final_sc0_gs[index_sc0_gs] <- rbind(paste(sc0_teams[index_sc0_gs],l6_form_sc0_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)

}
#change column names
final_sc0_gs <- as.data.frame(final_sc0_gs)
colnames(final_sc0_gs) <- "Goals scored"
#goal conceded
#create final_sc0_gc object
final_sc0_gc <- c()
for(index_sc0_gc in 1:length(sc0_teams))
{
  index_sc0_gc <- row.names(sc0_goalconceded_h) == sc0_teams[index_sc0_gc]
  form_sc0_gc <- sc0_goalconceded_h[index_sc0_gc]
  deleted_form_sc0_gc <- form_sc0_gc[!form_sc0_gc[] == ""]
  l6_form_sc0_gc <- tail(deleted_form_sc0_gc,6)
  l6_form_sc0_gc <- paste(l6_form_sc0_gc,collapse = " ")
  final_sc0_gc[index_sc0_gc] <- rbind(paste(sc0_teams[index_sc0_gc],l6_form_sc0_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)

}
#change column names
final_sc0_gc <- as.data.frame(final_sc0_gc)
colnames(final_sc0_gc) <- "Goals conceded"
#total goals
#create final_sc0_tg object
final_sc0_tg <- c()
for(index_sc0_tg in 1:length(sc0_teams))
{
  index_sc0_tg <- row.names(sc0_totalgoals_h) == sc0_teams[index_sc0_tg]
  form_sc0_tg <- sc0_totalgoals_h[index_sc0_tg]
  deleted_form_sc0_tg <- form_sc0_tg[!form_sc0_tg[] == ""]
  l6_form_sc0_tg <- tail(deleted_form_sc0_tg,6)
  l6_form_sc0_tg <- paste(l6_form_sc0_tg,collapse = " ")
  final_sc0_tg[index_sc0_tg] <- rbind(paste(sc0_teams[index_sc0_tg],l6_form_sc0_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)

}
#change column names
final_sc0_tg <- as.data.frame(final_sc0_tg)
colnames(final_sc0_tg) <- "Total Goals"
#combine the columns
final_sc0_all <- cbind(final_sc0_hf,final_sc0_gs,final_sc0_gc,final_sc0_tg)
write.xlsx(final_sc0_all,'Divisions/SC0.xlsx',sheetName = "L6", append = TRUE)
######################################################################################
#SC1
#form
#create final_sc1_hf object
final_sc1_hf <- c()
for(index_sc1_hf in 1:length(sc1_teams))
{
  index_sc1_hf <- row.names(sc1_form_h) == sc1_teams[index_sc1_hf]
  form_sc1_hf <- sc1_form_h[index_sc1_hf]
  deleted_form_sc1_hf <- form_sc1_hf[!form_sc1_hf[] == ""]
  l6_form_sc1_hf <- tail(deleted_form_sc1_hf,6)
  l6_form_sc1_hf <- paste(l6_form_sc1_hf,collapse = " ")
  final_sc1_hf[index_sc1_hf] <- rbind(paste(sc1_teams[index_sc1_hf],l6_form_sc1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)

}
#change column names
final_sc1_hf <- as.data.frame(final_sc1_hf)
colnames(final_sc1_hf) <- "Form"
#goals scored
#create final_sc1_gs object
final_sc1_gs <- c()
for(index_sc1_gs in 1:length(sc1_teams))
{
  index_sc1_gs <- row.names(sc1_goalscored_h) == sc1_teams[index_sc1_gs]
  form_sc1_gs <- sc1_goalscored_h[index_sc1_gs]
  deleted_form_sc1_gs <- form_sc1_gs[!form_sc1_gs[] == ""]
  l6_form_sc1_gs <- tail(deleted_form_sc1_gs,6)
  l6_form_sc1_gs <- paste(l6_form_sc1_gs,collapse = " ")
  final_sc1_gs[index_sc1_gs] <- rbind(paste(sc1_teams[index_sc1_gs],l6_form_sc1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)

}
#change column names
final_sc1_gs <- as.data.frame(final_sc1_gs)
colnames(final_sc1_gs) <- "Goals scored"
#goal conceded
#create final_sc1_gc object
final_sc1_gc <- c()
for(index_sc1_gc in 1:length(sc1_teams))
{
  index_sc1_gc <- row.names(sc1_goalconceded_h) == sc1_teams[index_sc1_gc]
  form_sc1_gc <- sc1_goalconceded_h[index_sc1_gc]
  deleted_form_sc1_gc <- form_sc1_gc[!form_sc1_gc[] == ""]
  l6_form_sc1_gc <- tail(deleted_form_sc1_gc,6)
  l6_form_sc1_gc <- paste(l6_form_sc1_gc,collapse = " ")
  final_sc1_gc[index_sc1_gc] <- rbind(paste(sc1_teams[index_sc1_gc],l6_form_sc1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)

}
#change column names
final_sc1_gc <- as.data.frame(final_sc1_gc)
colnames(final_sc1_gc) <- "Goals conceded"
#total goals
#create final_sc1_tg object
final_sc1_tg <- c()
for(index_sc1_tg in 1:length(sc1_teams))
{
  index_sc1_tg <- row.names(sc1_totalgoals_h) == sc1_teams[index_sc1_tg]
  form_sc1_tg <- sc1_totalgoals_h[index_sc1_tg]
  deleted_form_sc1_tg <- form_sc1_tg[!form_sc1_tg[] == ""]
  l6_form_sc1_tg <- tail(deleted_form_sc1_tg,6)
  l6_form_sc1_tg <- paste(l6_form_sc1_tg,collapse = " ")
  final_sc1_tg[index_sc1_tg] <- rbind(paste(sc1_teams[index_sc1_tg],l6_form_sc1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)

}
#change column names
final_sc1_tg <- as.data.frame(final_sc1_tg)
colnames(final_sc1_tg) <- "Total Goals"
#combine the columns
final_sc1_all <- cbind(final_sc1_hf,final_sc1_gs,final_sc1_gc,final_sc1_tg)
write.xlsx(final_sc1_all,'Divisions/SC1.xlsx',sheetName = "L6", append = TRUE)
###################################################################################
#SC2
#form
#create final_sc2_hf object
final_sc2_hf <- c()
for(index_sc2_hf in 1:length(sc2_teams))
{
  index_sc2_hf <- row.names(sc2_form_h) == sc2_teams[index_sc2_hf]
  form_sc2_hf <- sc2_form_h[index_sc2_hf]
  deleted_form_sc2_hf <- form_sc2_hf[!form_sc2_hf[] == ""]
  l6_form_sc2_hf <- tail(deleted_form_sc2_hf,6)
  l6_form_sc2_hf <- paste(l6_form_sc2_hf,collapse = " ")
  final_sc2_hf[index_sc2_hf] <- rbind(paste(sc2_teams[index_sc2_hf],l6_form_sc2_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)

}
#change column names
final_sc2_hf <- as.data.frame(final_sc2_hf)
colnames(final_sc2_hf) <- "Form"
#goals scored
#create final_sc2_gs object
final_sc2_gs <- c()
for(index_sc2_gs in 1:length(sc2_teams))
{
  index_sc2_gs <- row.names(sc2_goalscored_h) == sc2_teams[index_sc2_gs]
  form_sc2_gs <- sc2_goalscored_h[index_sc2_gs]
  deleted_form_sc2_gs <- form_sc2_gs[!form_sc2_gs[] == ""]
  l6_form_sc2_gs <- tail(deleted_form_sc2_gs,6)
  l6_form_sc2_gs <- paste(l6_form_sc2_gs,collapse = " ")
  final_sc2_gs[index_sc2_gs] <- rbind(paste(sc2_teams[index_sc2_gs],l6_form_sc2_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)

}
#change column names
final_sc2_gs <- as.data.frame(final_sc2_gs)
colnames(final_sc2_gs) <- "Goals scored"
#goal conceded
#create final_sc2_gc object
final_sc2_gc <- c()
for(index_sc2_gc in 1:length(sc2_teams))
{
  index_sc2_gc <- row.names(sc2_goalconceded_h) == sc2_teams[index_sc2_gc]
  form_sc2_gc <- sc2_goalconceded_h[index_sc2_gc]
  deleted_form_sc2_gc <- form_sc2_gc[!form_sc2_gc[] == ""]
  l6_form_sc2_gc <- tail(deleted_form_sc2_gc,6)
  l6_form_sc2_gc <- paste(l6_form_sc2_gc,collapse = " ")
  final_sc2_gc[index_sc2_gc] <- rbind(paste(sc2_teams[index_sc2_gc],l6_form_sc2_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)

}
#change column names
final_sc2_gc <- as.data.frame(final_sc2_gc)
colnames(final_sc2_gc) <- "Goals conceded"
#total goals
#create final_sc2_tg object
final_sc2_tg <- c()
for(index_sc2_tg in 1:length(sc2_teams))
{
  index_sc2_tg <- row.names(sc2_totalgoals_h) == sc2_teams[index_sc2_tg]
  form_sc2_tg <- sc2_totalgoals_h[index_sc2_tg]
  deleted_form_sc2_tg <- form_sc2_tg[!form_sc2_tg[] == ""]
  l6_form_sc2_tg <- tail(deleted_form_sc2_tg,6)
  l6_form_sc2_tg <- paste(l6_form_sc2_tg,collapse = " ")
  final_sc2_tg[index_sc2_tg] <- rbind(paste(sc2_teams[index_sc2_tg],l6_form_sc2_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)

}
#change column names
final_sc2_tg <- as.data.frame(final_sc2_tg)
colnames(final_sc2_tg) <- "Total Goals"
#combine the columns
final_sc2_all <- cbind(final_sc2_hf,final_sc2_gs,final_sc2_gc,final_sc2_tg)
write.xlsx(final_sc2_all,'Divisions/SC2.xlsx',sheetName = "L6", append = TRUE)
#####################################################################################
#SC3
#form
#create final_sc3_hf object
final_sc3_hf <- c()
for(index_sc3_hf in 1:length(sc3_teams))
{
  index_sc3_hf <- row.names(sc3_form_h) == sc3_teams[index_sc3_hf]
  form_sc3_hf <- sc3_form_h[index_sc3_hf]
  deleted_form_sc3_hf <- form_sc3_hf[!form_sc3_hf[] == ""]
  l6_form_sc3_hf <- tail(deleted_form_sc3_hf,6)
  l6_form_sc3_hf <- paste(l6_form_sc3_hf,collapse = " ")
  final_sc3_hf[index_sc3_hf] <- rbind(paste(sc3_teams[index_sc3_hf],l6_form_sc3_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)

}
#change column names
final_sc3_hf <- as.data.frame(final_sc3_hf)
colnames(final_sc3_hf) <- "Form"
#goals scored
#create final_sc3_gs object
final_sc3_gs <- c()
for(index_sc3_gs in 1:length(sc3_teams))
{
  index_sc3_gs <- row.names(sc3_goalscored_h) == sc3_teams[index_sc3_gs]
  form_sc3_gs <- sc3_goalscored_h[index_sc3_gs]
  deleted_form_sc3_gs <- form_sc3_gs[!form_sc3_gs[] == ""]
  l6_form_sc3_gs <- tail(deleted_form_sc3_gs,6)
  l6_form_sc3_gs <- paste(l6_form_sc3_gs,collapse = " ")
  final_sc3_gs[index_sc3_gs] <- rbind(paste(sc3_teams[index_sc3_gs],l6_form_sc3_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)

}
#change column names
final_sc3_gs <- as.data.frame(final_sc3_gs)
colnames(final_sc3_gs) <- "Goals scored"
#goal conceded
#create final_sc3_gc object
final_sc3_gc <- c()
for(index_sc3_gc in 1:length(sc3_teams))
{
  index_sc3_gc <- row.names(sc3_goalconceded_h) == sc3_teams[index_sc3_gc]
  form_sc3_gc <- sc3_goalconceded_h[index_sc3_gc]
  deleted_form_sc3_gc <- form_sc3_gc[!form_sc3_gc[] == ""]
  l6_form_sc3_gc <- tail(deleted_form_sc3_gc,6)
  l6_form_sc3_gc <- paste(l6_form_sc3_gc,collapse = " ")
  final_sc3_gc[index_sc3_gc] <- rbind(paste(sc3_teams[index_sc3_gc],l6_form_sc3_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)

}
#change column names
final_sc3_gc <- as.data.frame(final_sc3_gc)
colnames(final_sc3_gc) <- "Goals conceded"
#total goals
#create final_sc3_tg object
final_sc3_tg <- c()
for(index_sc3_tg in 1:length(sc3_teams))
{
  index_sc3_tg <- row.names(sc3_totalgoals_h) == sc3_teams[index_sc3_tg]
  form_sc3_tg <- sc3_totalgoals_h[index_sc3_tg]
  deleted_form_sc3_tg <- form_sc3_tg[!form_sc3_tg[] == ""]
  l6_form_sc3_tg <- tail(deleted_form_sc3_tg,6)
  l6_form_sc3_tg <- paste(l6_form_sc3_tg,collapse = " ")
  final_sc3_tg[index_sc3_tg] <- rbind(paste(sc3_teams[index_sc3_tg],l6_form_sc3_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)

}
#change column names
final_sc3_tg <- as.data.frame(final_sc3_tg)
colnames(final_sc3_tg) <- "Total Goals"
#combine the columns
final_sc3_all <- cbind(final_sc3_hf,final_sc3_gs,final_sc3_gc,final_sc3_tg)
write.xlsx(final_sc3_all,'Divisions/SC3.xlsx',sheetName = "L6", append = TRUE)
######################################################################################
#SP1
#form
#create final_sp1_hf object
final_sp1_hf <- c()
for(index_sp1_hf in 1:length(sp1_teams))
{
  index_sp1_hf <- row.names(sp1_form_h) == sp1_teams[index_sp1_hf]
  form_sp1_hf <- sp1_form_h[index_sp1_hf]
  deleted_form_sp1_hf <- form_sp1_hf[!form_sp1_hf[] == ""]
  l6_form_sp1_hf <- tail(deleted_form_sp1_hf,6)
  l6_form_sp1_hf <- paste(l6_form_sp1_hf,collapse = " ")
  final_sp1_hf[index_sp1_hf] <- rbind(paste(sp1_teams[index_sp1_hf],l6_form_sp1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)

}
#change column names
final_sp1_hf <- as.data.frame(final_sp1_hf)
colnames(final_sp1_hf) <- "Form"
#goals scored
#create final_sp1_gs object
final_sp1_gs <- c()
for(index_sp1_gs in 1:length(sp1_teams))
{
  index_sp1_gs <- row.names(sp1_goalscored_h) == sp1_teams[index_sp1_gs]
  form_sp1_gs <- sp1_goalscored_h[index_sp1_gs]
  deleted_form_sp1_gs <- form_sp1_gs[!form_sp1_gs[] == ""]
  l6_form_sp1_gs <- tail(deleted_form_sp1_gs,6)
  l6_form_sp1_gs <- paste(l6_form_sp1_gs,collapse = " ")
  final_sp1_gs[index_sp1_gs] <- rbind(paste(sp1_teams[index_sp1_gs],l6_form_sp1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)

}
#change column names
final_sp1_gs <- as.data.frame(final_sp1_gs)
colnames(final_sp1_gs) <- "Goals scored"
#goal conceded
#create final_sp1_gc object
final_sp1_gc <- c()
for(index_sp1_gc in 1:length(sp1_teams))
{
  index_sp1_gc <- row.names(sp1_goalconceded_h) == sp1_teams[index_sp1_gc]
  form_sp1_gc <- sp1_goalconceded_h[index_sp1_gc]
  deleted_form_sp1_gc <- form_sp1_gc[!form_sp1_gc[] == ""]
  l6_form_sp1_gc <- tail(deleted_form_sp1_gc,6)
  l6_form_sp1_gc <- paste(l6_form_sp1_gc,collapse = " ")
  final_sp1_gc[index_sp1_gc] <- rbind(paste(sp1_teams[index_sp1_gc],l6_form_sp1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)

}
#change column names
final_sp1_gc <- as.data.frame(final_sp1_gc)
colnames(final_sp1_gc) <- "Goals conceded"
#total goals
#create final_sp1_tg object
final_sp1_tg <- c()
for(index_sp1_tg in 1:length(sp1_teams))
{
  index_sp1_tg <- row.names(sp1_totalgoals_h) == sp1_teams[index_sp1_tg]
  form_sp1_tg <- sp1_totalgoals_h[index_sp1_tg]
  deleted_form_sp1_tg <- form_sp1_tg[!form_sp1_tg[] == ""]
  l6_form_sp1_tg <- tail(deleted_form_sp1_tg,6)
  l6_form_sp1_tg <- paste(l6_form_sp1_tg,collapse = " ")
  final_sp1_tg[index_sp1_tg] <- rbind(paste(sp1_teams[index_sp1_tg],l6_form_sp1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)

}
#change column names
final_sp1_tg <- as.data.frame(final_sp1_tg)
colnames(final_sp1_tg) <- "Total Goals"
#combine the columns
final_sp1_all <- cbind(final_sp1_hf,final_sp1_gs,final_sp1_gc,final_sp1_tg)
write.xlsx(final_sp1_all,'Divisions/SP1.xlsx',sheetName = "L6", append = TRUE)
#####################################################################################
#T1
#form
#create final_t1_hf object
final_t1_hf <- c()
for(index_t1_hf in 1:length(t1_teams))
{
  index_t1_hf <- row.names(t1_form_h) == t1_teams[index_t1_hf]
  form_t1_hf <- t1_form_h[index_t1_hf]
  deleted_form_t1_hf <- form_t1_hf[!form_t1_hf[] == ""]
  l6_form_t1_hf <- tail(deleted_form_t1_hf,6)
  l6_form_t1_hf <- paste(l6_form_t1_hf,collapse = " ")
  final_t1_hf[index_t1_hf] <- rbind(paste(t1_teams[index_t1_hf],l6_form_t1_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)

}
#change column names
final_t1_hf <- as.data.frame(final_t1_hf)
colnames(final_t1_hf) <- "Form"
#goals scored
#create final_t1_gs object
final_t1_gs <- c()
for(index_t1_gs in 1:length(t1_teams))
{
  index_t1_gs <- row.names(t1_goalscored_h) == t1_teams[index_t1_gs]
  form_t1_gs <- t1_goalscored_h[index_t1_gs]
  deleted_form_t1_gs <- form_t1_gs[!form_t1_gs[] == ""]
  l6_form_t1_gs <- tail(deleted_form_t1_gs,6)
  l6_form_t1_gs <- paste(l6_form_t1_gs,collapse = " ")
  final_t1_gs[index_t1_gs] <- rbind(paste(t1_teams[index_t1_gs],l6_form_t1_gs, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)

}
#change column names
final_t1_gs <- as.data.frame(final_t1_gs)
colnames(final_t1_gs) <- "Goals scored"
#goal conceded
#create final_t1_gc object
final_t1_gc <- c()
for(index_t1_gc in 1:length(t1_teams))
{
  index_t1_gc <- row.names(t1_goalconceded_h) == t1_teams[index_t1_gc]
  form_t1_gc <- t1_goalconceded_h[index_t1_gc]
  deleted_form_t1_gc <- form_t1_gc[!form_t1_gc[] == ""]
  l6_form_t1_gc <- tail(deleted_form_t1_gc,6)
  l6_form_t1_gc <- paste(l6_form_t1_gc,collapse = " ")
  final_t1_gc[index_t1_gc] <- rbind(paste(t1_teams[index_t1_gc],l6_form_t1_gc, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)

}
#change column names
final_t1_gc <- as.data.frame(final_t1_gc)
colnames(final_t1_gc) <- "Goals conceded"
#total goals
#create final_t1_tg object
final_t1_tg <- c()
for(index_t1_tg in 1:length(t1_teams))
{
  index_t1_tg <- row.names(t1_totalgoals_h) == t1_teams[index_t1_tg]
  form_t1_tg <- t1_totalgoals_h[index_t1_tg]
  deleted_form_t1_tg <- form_t1_tg[!form_t1_tg[] == ""]
  l6_form_t1_tg <- tail(deleted_form_t1_tg,6)
  l6_form_t1_tg <- paste(l6_form_t1_tg,collapse = " ")
  final_t1_tg[index_t1_tg] <- rbind(paste(t1_teams[index_t1_tg],l6_form_t1_tg, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)

}
#change column names
final_t1_tg <- as.data.frame(final_t1_tg)
colnames(final_t1_tg) <- "Total Goals"
#combine the columns
final_t1_all <- cbind(final_t1_hf,final_t1_gs,final_t1_gc,final_t1_tg)
write.xlsx(final_t1_all,'Divisions/T1.xlsx',sheetName = "L6", append = TRUE)




