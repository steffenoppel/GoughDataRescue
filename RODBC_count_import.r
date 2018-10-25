############################################################################
######## IMPORT NEST AND VISIT DATA FROM DATABASE ##########
############################################################################

library(RODBC)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA"), silent=T)

db <- odbcConnectAccess2007('GOUGH_BreedingDatabase.accdb')
survDB<- sqlQuery(db, "SELECT * FROM tblCountSurveys")
countDB<- sqlQuery(db, "SELECT * FROM tblBirdData")
locDB<- sqlQuery(db, "SELECT * FROM tblLocation")
odbcClose(db)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)

save.image("GOUGH_seabird_count_data.RData")
