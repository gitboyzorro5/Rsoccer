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



b1_indexes <- c()
#create n number of indexes
for(i in 1:length(b1_teams))
{
  b1_indexes[i] <- paste0("index",i,sep = ",")
}

#assign each index a team

b1_indexes <- row.names(b1_form_h) == b1_teams[1:length(b1_teams)]

#index <- row.names(d1_form_h) == d1_teams[1]
#create n number of forms
b1_forms <- c()
for(k in 1:length(b1_teams))
{
  b1_forms[k] <- paste0("forms",k,sep = ",")
}
#assign forms to indexes

b1_forms <- b1_forms[b1_indexes]

b1_forms <- b1_forms[!b1_forms[]==""]

b1_forms
#print last 6
for(m in 1:length(b1_teams))
{
 print(tail(b1_forms[m]))
}

form_n <- d1_form_h[index]
#delete empty values
form_n <- form_n[!form_n[]==""]
#pick last 6
final_n <- tail(form_n,6)


index_2 <- row.names(d1_form_h) == d1_teams[2]
#pick specific vector using that index

form_2 <- d1_form_h[index_2]
#delete empty values
form_2 <- form_2[!form_2[]==""]
#pick last 6
tail(form_2,6)




