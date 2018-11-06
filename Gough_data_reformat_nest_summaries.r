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




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: UPDATE NESTS OUTCOME BY LOOPING OVER EACH NEST
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


### format the SUCCESS field
unique(nests$Fledged)
unique(nests$Hatched)
unique(nests$DeadChick)
unique(nests$Comments[is.na(nests$Fledged)])

delete<-vector()

for (n in 1:dim(nests)[1]) {
  
  x<-nests[n,]
  
  ### first decide whether there are data on success
  if(is.na(x$Fledged)){
    
    ### if no data on fledge, check whether nest has hatched
    if(is.na(x$Hatched)){
      
      ### if no data at all, remove nest, otherwise SUCCESS=1
      if(is.na(x$DeadChick)){delete<-c(delete,n)}else{
        nests$SUCCESS[n]<-0
        nests$LastStage[n]<-"FAIL"
        nests$Completed[n]<-1}
      
      ### if data on hatching then call nest successful but incomplete
    }else{if(x$Hatched==0){
      nests$SUCCESS[n]<-0
      nests$LastStage[n]<-"FAIL"
      nests$Completed[n]<-1}else{
        
        ### if nest has hatched but chick is dead then SUCCess=0
        nests$SUCCESS[n]<-ifelse(is.na(nests$DeadChick),1,0)
        nests$LastStage[n]<-ifelse(is.na(nests$DeadChick),"CHIC","INCU")
        nests$Completed[n]<-ifelse(is.na(nests$DeadChick),0,1)}}
    
    ### if there are data on fledged then SUCCESS = 1  
    }else{if(x$Fledged==0){
      nests$SUCCESS[n]<-0
      nests$LastStage[n]<-"FAIL"
      nests$Completed[n]<-1}else{
        
        ### if nest has hatched but chick is dead then SUCCess=0
        nests$SUCCESS[n]<-1
        nests$LastStage[n]<-ifelse(x$Hatched==1,"CHIC","INCU")
        nests$Completed[n]<-0}
    
  } ## end loop over NA fledged
  
} ## end loop over all nests


## check nests without much information
dim(nests)
nests<-nests[-delete,]
dim(nests)
nests %>% filter(is.na(SUCCESS))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 4: QUICK SUMMARY AND COMMON SENSE CHECK
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

nests %>% filter(!is.na(SUCCESS)) %>%
  group_by(Species,Year) %>%
  summarise(n=length(NestID), succ=mean(SUCCESS))






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 5: FIX LOCATION DESCRIPTIONS TO MATCH DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### identify locations not in database
usedlocs<-unique(nests$Colony)
unique(nestsDB$Colony)
usedlocs[!(usedlocs %in% nestsDB$Colony)]

### re-assign Colony names that match database
nests<- nests %>% mutate(Colony=ifelse(Colony=="Green Hill","Green Hill North",Colony)) %>%
  mutate(Colony=ifelse(Colony=="Green Hill","Green Hill North",Colony)) %>%
  mutate(Colony=ifelse(Colony=="Tafelkop path","Tafelkop",Colony)) %>%
  mutate(Colony=ifelse(Colony=="Way to plot 13","Sea cliff",Colony)) %>%
  mutate(Colony=ifelse(Colony=="Way to Low Hump","Low Hump",Colony)) %>%
  mutate(Colony=ifelse(Colony=="Diesel Cove-Skivvygat","Cliff to south of view point in Snoek Gat (where c",Colony)) %>%
  mutate(Colony=ifelse(Colony=="Skivvygat-Snoekgat","Snoekgat",Colony)) %>%
  mutate(Colony=ifelse(Colony=="1","Helipad",Colony)) %>%
  mutate(Colony=ifelse(Colony=="2","Blechnum Bridge",Colony)) %>%
  mutate(Colony=ifelse(is.na(Colony),"Gonydale",Colony)) 

### check nests without Colony location
nests %>% filter(is.na(Colony))




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 6: ADD NESTS TO DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue")


### create table matching the database
exportNEST<- nests %>% mutate(Time="12:00") %>%
  
  
  
  mutate(Nest_label=gsub("[-/' ]","" , NestID,ignore.case = TRUE)) %>% ### COMPLY WITH NEST LABEL STANDARDS - REMOVE ALL BLANKS AND SPECIAL SYMBOLS
  select(Nest_label,Species,Year,Colony,Site,FirstRecord,StageFound, DateLastAlive, DateLastChecked,LastStage,Completed,SUCCESS)


### export the nests that need to be added
fwrite(exportNEST,"Gough_Nests_tobe_added_6Nov2018.csv")







