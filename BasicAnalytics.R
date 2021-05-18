library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
library('lubridate')
#Full time results percentages
ftr_summary <- tabyl(allteams20202021,Div,FTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
ftr_summary <- ftr_summary[,c(1,4,3,2)]
#Half time results percentages
htr_summary <- tabyl(allteams20202021,Div,HTR) %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1)
htr_summary <- htr_summary[,c(1,4,3,2)]


HomeTeam_b1 <- rep(b1_teams, each = length(b1_teams))
AwayTeam_b1 <- rep(b1_teams, length(b1_teams))
B1_fixtures <- cbind(HomeTeam_b1,AwayTeam_b1)
B1_fixtures <- as.data.frame(B1_fixtures)
B1_fixtures <- B1_fixtures[!B1_fixtures$HomeTeam_b1 == B1_fixtures$AwayTeam_b1,]
rownames(B1_fixtures) <- NULL
B1_fixtures$Div <- "B1"
B1_fixtures <- B1_fixtures[,c(3,1,2)]
merge(B1_fixtures,allteams20202021,by.x='Div',by.y="Div")

myodds <- readxl::read_excel('../FDAS/myodds.xlsx', sheet = '2way')
#################################
#Team against
b1_form_team_against_h <- tapply(B1$AwayTeam, B1[c("HomeTeam", "Date")],median)
b1_form_team_against_a <- tapply(B1$HomeTeam, B1[c("AwayTeam", "Date")],median)
#remove na values
b1_form_team_against_h[is.na(b1_form_team_against_h)] <- ""
b1_form_team_against_a[is.na(b1_form_team_against_a)] <- ""
#combine the matrices
#B1
for(b1_rowh_f_against in 1:nrow(b1_form_team_against_h)) {
  for(b1_colh_f_against in 1:ncol(b1_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(b1_rowa_f_against in 1:nrow(b1_form_team_against_a)) {
      for(b1_cola_f_against in 1:ncol(b1_form_team_against_a)) {
        ifelse(!b1_form_team_against_a[b1_rowa_f_against,b1_cola_f_against]=="",b1_form_team_against_h[b1_rowa_f_against,b1_cola_f_against] <- b1_form_team_against_a[b1_rowa_f_against,b1_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

final_b1_hf_against <- c()
for(index_b1_hf_against in 1:length(b1_teams))
{
  index_b1_hf_against <- row.names(b1_form_team_against_h) == b1_teams[index_b1_hf_against]
  form_b1_hf_against <- b1_form_team_against_h[index_b1_hf_against]
  deleted_form_b1_hf_against <- form_b1_hf_against[!form_b1_hf_against[] == ""]
  l6_form_b1_hf_against <- tail(deleted_form_b1_hf_against,6)
  l6_form_b1_hf_against <- paste(l6_form_b1_hf_against,collapse = " ")
  final_b1_hf_against[index_b1_hf_against] <- rbind(paste(b1_teams[index_b1_hf_against],l6_form_b1_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",b1_teams[index],l6_form)

}
final_b1_hf_against <- as.data.frame(final_b1_hf_against)
colnames(final_b1_hf_against) <- "Team against"
final_b1_hf_against
final_b1_all <- cbind(final_b1_hf,final_b1_gs,final_b1_gc,final_b1_tg,final_b1_hf_against)
write.xlsx(final_b1_all,'Divisions/B1.xlsx',sheetName = "L6_2", append = TRUE)
################################################################################################


fixtures <- read.csv('../FDAS/fixtures.csv')
fixtures$Date <- dmy(fixtures$Date)
fixtures <- fixtures[order(as.Date(fixtures$Date, format = "%d/%m/%Y"), decreasing = FALSE),]






























