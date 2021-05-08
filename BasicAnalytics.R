library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
#Full time results percentages
ftr_summary <- tabyl(allteams20202021,Div,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,4,3,2)]
#Half time results percentages
htr_summary <- tabyl(allteams20202021,Div,HTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
htr_summary <- htr_summary[,c(1,4,3,2)]

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


