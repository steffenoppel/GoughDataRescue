############################################################################
######## IMPORT NEST AND VISIT DATA FROM DATABASE ##########
############################################################################

library(RODBC)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA"), silent=T)

db <- odbcConnectAccess2007('GOUGH_BreedingDatabase.accdb')
nestsDB<- sqlQuery(db, "SELECT * FROM tblNests")
visDB<- sqlQuery(db, "SELECT * FROM tblNestVisits")
odbcClose(db)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)

save.image("GOUGH_nest_data.RData")
