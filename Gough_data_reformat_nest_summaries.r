###################################################################
### DATA RESCUE OF GOUGH MONITORING DATA - NEST SUMMARIES #########
###################################################################

## written by steffen.oppel@rspb.org.uk
## support from Antje Steinfurth

## main purpose is to rescue data that are inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology

## first created 5 Nov 2018

library(tidyverse)
library(data.table)
library(lubridate)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 1: LOAD DATA FROM DATABASE AND MANUALLY COPIED SPREADSHEET
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## only need to re-run after changes to database
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess\\RODBC_nest_import.r")), wait = FALSE, invisible = FALSE)

#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)
load("GOUGH_nest_data.RData")
head(nestsDB)

#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)
nests <- fread("BREEDING_DATABASE_NEW_NestSummary.csv")
head(nests)







##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: FIX THE VARIOUS FORMATTING PROBLEMS OF DATES, SPECIES, LOCATIONS AND STAGES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
names(nests)[c(14,15,16,18)]<-c("DateLastAlive","DateFailed","DateLastChecked","DeadChick")

## apply date formatting conversion to the whole data set
nests<- nests %>% select(-Filename,-Directory,-Notes) %>%
  mutate(FirstRecord=gsub(".*-","",FirstRecord)) %>%
  mutate(FirstRecord=dmy(FirstRecord)) %>%
  mutate(DateLastAlive=gsub(".*-","",DateLastAlive)) %>%
  mutate(DateLastAlive=dmy(DateLastAlive)) %>%
  mutate(DateFailed=gsub(".*-","",DateFailed)) %>%
  mutate(DateFailed=dmy(DateFailed)) %>%
  mutate(DateLastChecked=gsub(".*-","",DateLastChecked)) %>%
  mutate(DateLastChecked=dmy(DateLastChecked))

head(nests)
str(nests)



### modify species abbreviations
species<-unique(nests$Species)
lkSpec<-data.frame(abbr=species, CODE=c("ATPE","BBPR","SOPE","TRAL","GOBU","SOAL","GRPE"))
nests<- nests %>% mutate(Species=lkSpec$CODE[match(Species,lkSpec$abbr)])


### generate location data matching database (Colony and Site) and create unique ID number
nests<- nests %>%
  mutate(Transect=ifelse((Transect %in% c(NA,"")),Site,Transect)) %>%         ## fill in site name from quadrat
  mutate(Colony=ifelse((Colony %in% c(NA,"")),Transect,Colony)) %>%         ## fill in colony name from transect
  mutate(Site=ifelse((Site %in% c(NA,"")),Quadrat,Site)) %>%         ## fill in colony name from transect
  mutate(NestID=paste(Species,Year,Colony,Site,ID_nest_burrow, sep="_"))


### format the stage at which the nest was found
unique(nests$StageFound)
nests$StageFound<-ifelse(nests$StageFound %in% c("AE","E","AIA","AA"),"INCU",ifelse(nests$StageFound %in% c("C","CC"),"CHIC",""))
head(nests)

### format the SUCCESS field
unique(nests$Fledged)
nests$SUCCESS<-ifelse(nests$Fledged>0,1,0)
nests$LastStage<-ifelse(nests$SUCCESS=0,"FAIL",ifelse(nests$Hatched>0,"CHIC","INCU"))
nests$Completed<-ifelse(is.na(nests$Fledged),,nests$SUCCESS)

nests$StageFound<-ifelse(nests$StageFound %in% c("AE","E","AIA","AA"),"INCU",ifelse(nests$StageFound %in% c("C","CC"),"CHIC",""))
head(nests)






### ASSESS MISSING CRITICAL INFO AND REMOVE WORTHLESS VISITS
dim(nests)
visits<- visits %>% filter(!is.na(STATUS)) ### %>% select(Species, Year, STAGE, ADULT, CONTENT, Comments,Notes)  ### remove visits where nest status is unknown
dim(visits)







##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 5: UPDATE NESTS OUTCOME IN DATABASE AND ADD NESTS NOT YET IN DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue")
head(nestsRED)
dim(nestsRED)

### create table matching the database
exportNEST<- nestsRED %>% mutate(Time="12:00") %>%
  mutate(Nest_label=gsub("[-/' ]","" , NestID,ignore.case = TRUE)) %>% ### COMPLY WITH NEST LABEL STANDARDS - REMOVE ALL BLANKS AND SPECIAL SYMBOLS
  mutate(Completed=ifelse(AssumedFledged==1,0,1))%>%
  select(Nest_label,Species,Year,Colony,Site,Latitude,Longitude,DateFound,StageFound, DateLastAlive, DateLastChecked,LastStage,Completed,SUCCESS)

### FIX LOCATIONS FOR KNOWN SPECIES ###

exportNEST$Site[exportNEST$Species=="SGPE"]<-exportNEST$Colony[exportNEST$Species=="SGPE"]
exportNEST$Colony[exportNEST$Species=="SGPE"]<-"Low Hump"
exportNEST$Colony[exportNEST$Species=="GRPE"]<-"Gonydale"
exportNEST$Colony[exportNEST$Species=="GRPE"]<-"Area 1"
exportNEST$Site[exportNEST$Species=="MAPR"]<-exportNEST$Colony[exportNEST$Species=="MAPR"]
exportNEST$Colony[exportNEST$Species=="MAPR"]<-"Prion Cave"
exportNEST$Site[exportNEST$Species=="BBPR"]<-exportNEST$Colony[exportNEST$Species=="BBPR"]

#exportNEST %>% filter(Species=="SOPE") %>% select(Colony,Site)



### identify locations not in database
usedlocs<-unique(exportNEST$Colony)
newlocs<-usedlocs[!(usedlocs %in% locDB$LocationName)]
newlocs<-data.frame(bullshit=newlocs, LOCATION=NA)
newlocs<-newlocs %>%
  mutate(LOCATION=ifelse(grepl("Rivercrossing",bullshit,perl=T,ignore.case = T)==T,"Unknown",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("by paint store",bullshit,perl=T,ignore.case = T)==T,"Weather station",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("by Sagina shed",bullshit,perl=T,ignore.case = T)==T,"Weather station",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("in front of food store",bullshit,perl=T,ignore.case = T)==T,"Weather station",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("in front of lab",bullshit,perl=T,ignore.case = T)==T,"Weather station",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Prion Cave",bullshit,perl=T,ignore.case = T)==T,"Prion Cave",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("to left of Skivvygat",bullshit,perl=T,ignore.case = T)==T,"Weather station",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Blechnam bridge",bullshit,perl=T,ignore.case = T)==T,"Blechnum Bridge",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Golden highway",bullshit,perl=T,ignore.case = T)==T,"Golden Highway",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("study",bullshit,perl=T,ignore.case = T)==T,"Unknown",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("control",bullshit,perl=T,ignore.case = T)==T,"Unknown",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Area 3",bullshit,perl=T,ignore.case = T)==T,"Area 3",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("GLS Study Colony",bullshit,perl=T,ignore.case = T)==T,"Between the base and seal beach",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("inland Adm path",bullshit,perl=T,ignore.case = T)==T,"Admirals",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("area 2",bullshit,perl=T,ignore.case = T)==T,"Area 2",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Seal Beach",bullshit,perl=T,ignore.case = T)==T,"Seal Beach",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Snoekgat",bullshit,perl=T,ignore.case = T)==T,"Snoekgat",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Tumbledown",tolower(bullshit),perl=T,ignore.case = T)==T,"Tumbledown",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Low Hump",tolower(bullshit),perl=T,ignore.case = T)==T,"Low Hump",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("by balloon balcony",tolower(bullshit),perl=T,ignore.case = T)==T,"Weather station",LOCATION))




### UPDATE LOCATIONS IN SURVEYS

exportNEST<- exportNEST %>% mutate(Site=ifelse(Site %in% c("",NA),Colony,Site)) %>%
  mutate(Colony=ifelse(Colony %in% locDB$LocationName,Colony,newlocs$LOCATION[match(Colony,newlocs$bullshit)]))
head(exportNEST)



### match to check which nests already exist
# head(nestsDB)
# head(exportNEST)
# updNests<-exportNEST %>% filter(Nest_label %in% nestsDB$Nest_label)
# updNestsDB<-nestsDB %>% filter(Nest_label %in% updNests$Nest_label)
# updNests[!(updNests$SUCCESS==updNestsDB$SUCCESS),]      ### these nests differ between the output here and the DB and need to be updated

### export the nests that need to be updated
#fwrite(updNests[!(updNests$SUCCESS==updNestsDB$SUCCESS),],"Gough_Nests_tobe_updated.csv")


### export the nests that need to be added
addNests<-exportNEST ##%>% filter(!(Nest_label %in% nestsDB$Nest_label)) ## we hope to have excluded that manually
fwrite(addNests,"Gough_Nests_tobe_added.csv")







