library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
library('scales')
#B1
write.xlsx(points_b1,'Divisions/B1.xlsx', sheetName = "Table")
write.xlsx(b1_form_h,'Divisions/B1.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(b1_goalscored_h,'Divisions/B1.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(b1_goalconceded_h,'Divisions/B1.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(b1_totalgoals_h,'Divisions/B1.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(b1_goaltotalsv2,'Divisions/B1.xlsx', sheetName = "Goal totals v2", append = TRUE)
#D1
write.xlsx(points_d1,'Divisions/D1.xlsx', sheetName = "Table")
write.xlsx(d1_form_h,'Divisions/D1.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(d1_goalscored_h,'Divisions/D1.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(d1_goalconceded_h,'Divisions/D1.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(d1_totalgoals_h,'Divisions/D1.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(d1_goaltotalsv2,'Divisions/D1.xlsx', sheetName = "Goal totals v2", append = TRUE)
#D2
write.xlsx(points_d2,'Divisions/D2.xlsx', sheetName = "Table")
write.xlsx(d2_form_h,'Divisions/D2.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(d2_goalscored_h,'Divisions/D2.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(d2_goalconceded_h,'Divisions/D2.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(d2_totalgoals_h,'Divisions/D2.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(d2_goaltotalsv2,'Divisions/D2.xlsx', sheetName = "Goal totals v2", append = TRUE)
#E0
write.xlsx(points_e0,'Divisions/E0.xlsx', sheetName = "Table")
write.xlsx(e0_form_h,'Divisions/E0.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(e0_goalscored_h,'Divisions/E0.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(e0_goalconceded_h,'Divisions/E0.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(e0_totalgoals_h,'Divisions/E0.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(e0_goaltotalsv2,'Divisions/E0.xlsx', sheetName = "Goal totals v2", append = TRUE)
#E1
write.xlsx(points_e1,'Divisions/E1.xlsx', sheetName = "Table")
write.xlsx(e1_form_h,'Divisions/E1.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(e1_goalscored_h,'Divisions/E1.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(e1_goalconceded_h,'Divisions/E1.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(e1_totalgoals_h,'Divisions/E1.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(e1_goaltotalsv2,'Divisions/E1.xlsx', sheetName = "Goal totals v2", append = TRUE)
#E2
write.xlsx(points_e2,'Divisions/E2.xlsx', sheetName = "Table")
write.xlsx(e2_form_h,'Divisions/E2.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(e2_goalscored_h,'Divisions/E2.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(e2_goalconceded_h,'Divisions/E2.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(e2_totalgoals_h,'Divisions/E2.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(e2_goaltotalsv2,'Divisions/E2.xlsx', sheetName = "Goal totals v2", append = TRUE)
#E3
write.xlsx(points_e3,'Divisions/E3.xlsx', sheetName = "Table")
write.xlsx(e3_form_h,'Divisions/E3.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(e3_goalscored_h,'Divisions/E3.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(e3_goalconceded_h,'Divisions/E3.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(e3_totalgoals_h,'Divisions/E3.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(e3_goaltotalsv2,'Divisions/E3.xlsx', sheetName = "Goal totals v2", append = TRUE)
#EC
write.xlsx(points_ec,'Divisions/EC.xlsx', sheetName = "Table")
write.xlsx(ec_form_h,'Divisions/EC.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(ec_goalscored_h,'Divisions/EC.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(ec_goalconceded_h,'Divisions/EC.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(ec_totalgoals_h,'Divisions/EC.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(ec_goaltotalsv2,'Divisions/EC.xlsx', sheetName = "Goal totals v2", append = TRUE)
#F1
write.xlsx(points_f1,'Divisions/F1.xlsx', sheetName = "Table")
write.xlsx(f1_form_h,'Divisions/F1.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(f1_goalscored_h,'Divisions/F1.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(f1_goalconceded_h,'Divisions/F1.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(f1_totalgoals_h,'Divisions/F1.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(f1_goaltotalsv2,'Divisions/F1.xlsx', sheetName = "Goal totals v2", append = TRUE)
#F2
write.xlsx(points_f2,'Divisions/F2.xlsx', sheetName = "Table")
write.xlsx(f2_form_h,'Divisions/F2.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(f2_goalscored_h,'Divisions/F2.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(f2_goalconceded_h,'Divisions/F2.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(f2_totalgoals_h,'Divisions/F2.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(f2_goaltotalsv2,'Divisions/F2.xlsx', sheetName = "Goal totals v2", append = TRUE)
#G1
write.xlsx(points_g1,'Divisions/G1.xlsx', sheetName = "Table")
write.xlsx(g1_form_h,'Divisions/G1.xlsx', sheetName = "Form", append = TRUE)
write.xlsx(g1_goalscored_h,'Divisions/G1.xlsx', sheetName = "Goals scored", append = TRUE)
write.xlsx(g1_goalconceded_h,'Divisions/G1.xlsx', sheetName = "Goals conceded", append = TRUE)
write.xlsx(g1_totalgoals_h,'Divisions/G1.xlsx', sheetName = "Total Goals form", append = TRUE)
write.xlsx(g1_goaltotalsv2,'Divisions/G1.xlsx', sheetName = "Goal totals v2", append = TRUE)







