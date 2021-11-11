Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library(rvest)
unlink('Divisions/Scorers.xlsx')
e0html <- paste(readLines("../FDAS/e0_scorers.html"), collapse="\n")
e1html <- paste(readLines("../FDAS/e1_scorers.html"), collapse="\n")
e2html <- paste(readLines("../FDAS/e2_scorers.html"), collapse="\n")
e3html <- paste(readLines("../FDAS/e3_scorers.html"), collapse="\n")
d1html <- paste(readLines("../FDAS/d1_scorers.html"), collapse="\n")
sp1html <- paste(readLines("../FDAS/sp1_scorers.html"), collapse="\n")
i1html <- paste(readLines("../FDAS/i1_scorers.html"), collapse="\n")
f1html <- paste(readLines("../FDAS/f1_scorers.html"), collapse="\n")
sc0html <- paste(readLines("../FDAS/sc0_scorers.html"), collapse="\n")
mlshtml <- paste(readLines("../FDAS/mls_scorers.html"), collapse="\n")


webpage_e0 <- read_html(e0html)
webpage_e1 <- read_html(e1html)
webpage_e2 <- read_html(e2html)
webpage_e3 <- read_html(e3html)
webpage_d1 <- read_html(d1html)
webpage_sp1 <- read_html(sp1html)
webpage_i1 <- read_html(i1html)
webpage_f1 <- read_html(f1html)
webpage_sc0 <- read_html(sc0html)
webpage_mls <- read_html(mlshtml)
pp
e0_scorersdata <- webpage_e0 %>% html_nodes(css ='table') %>% html_table()
e1_scorersdata <- webpage_e1 %>% html_nodes(css ='table') %>% html_table()
e2_scorersdata <- webpage_e2 %>% html_nodes(css ='table') %>% html_table()
e3_scorersdata <- webpage_e3 %>% html_nodes(css ='table') %>% html_table()
d1_scorersdata <- webpage_d1 %>% html_nodes(css ='table') %>% html_table()
sp1_scorersdata <- webpage_sp1 %>% html_nodes(css ='table') %>% html_table()
i1_scorersdata <- webpage_i1 %>% html_nodes(css ='table') %>% html_table()
f1_scorersdata <- webpage_f1 %>% html_nodes(css ='table') %>% html_table()
sc0_scorersdata <- webpage_sc0 %>% html_nodes(css ='table') %>% html_table()
mls_scorersdata <- webpage_mls %>% html_nodes(css ='table') %>% html_table()

write.xlsx(e0_scorersdata,'Divisions/Scorers.xlsx', sheetName = "E0")
write.xlsx(e1_scorersdata,'Divisions/Scorers.xlsx', sheetName = "E1", append = TRUE)
write.xlsx(e2_scorersdata,'Divisions/Scorers.xlsx', sheetName = "E2", append = TRUE)
write.xlsx(e3_scorersdata,'Divisions/Scorers.xlsx', sheetName = "E3", append = TRUE)
write.xlsx(d1_scorersdata,'Divisions/Scorers.xlsx', sheetName = "D1", append = TRUE)
write.xlsx(sp1_scorersdata,'Divisions/Scorers.xlsx', sheetName = "SP1", append = TRUE)
write.xlsx(i1_scorersdata,'Divisions/Scorers.xlsx', sheetName = "I1", append = TRUE)
write.xlsx(f1_scorersdata,'Divisions/Scorers.xlsx', sheetName = "F1", append = TRUE)
write.xlsx(sc0_scorersdata,'Divisions/Scorers.xlsx', sheetName = "SC0", append = TRUE)
write.xlsx(mls_scorersdata,'Divisions/Scorers.xlsx', sheetName = "MLS", append = TRUE)






