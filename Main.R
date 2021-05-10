#Master file to run all other scripts
#load the data
source("divisions.R")
source("GoalTotalsV2.R")
source("GSmatrix.R")
source("GCmatrix.R")
source("Teamform.R")
source("TGmatrix.R")
source("ShotsAnalysis.R")
source("LeagueTables.R")
#delete files in divisions
unlink("Divisions/*",recursive = T,force = T)
source("MasterWrite.R")
source("LastSix.R")
