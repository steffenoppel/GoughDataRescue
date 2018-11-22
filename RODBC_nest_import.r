############################################################################
######## IMPORT NEST AND VISIT DATA FROM DATABASE ##########
############################################################################

library(RODBC)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA"), silent=T)

### FIND MOST UP-TO-DATE VERSION OF DATABASE
details <- file.info(list.files(pattern="*.accdb"))
details <- details[order(as.POSIXct(details$mtime),decreasing=T),]
upddb<-rownames(details)[1]

db <- odbcConnectAccess2007(upddb)
nestsDB<- sqlQuery(db, "SELECT * FROM tblNests")
visDB<- sqlQuery(db, "SELECT * FROM tblNestVisits")
odbcClose(db)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)

save.image("GOUGH_nest_data.RData")
