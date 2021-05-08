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
library('R.utils')
as.data.frame(bundesform)
for(index in 1:length(d1_teams))
{
index <- row.names(d1_form_h) == d1_teams[index]
form <- d1_form_h[index]
deleted_form <- form[!form[] == ""]
l6_form <- tail(deleted_form,6)
l6_form <- paste(l6_form,collapse = " ")
final[index] <- rbind(paste(d1_teams[index],l6_form, sep = ",",collapse = ""))
#bundesform[] <- printf("%s\t%s\n",d1_teams[index],l6_form)

}

write.csv(final,"final.csv")
