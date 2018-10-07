##################################################
### DATA RESCUE OF GOUGH MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk
## support from Jaimie Cleeland (Gough Team 2017/2018)

## main purpose is to rescue data that are inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology

## first created 8 Aug 2018
## update on 14 Aug 2018 - requires conversion of Excel 'nest visits' tab into csv to avoid automated date conversion

## 7 Sept 2018: major revisions to examine inherent logic of nest sequence visits
## requires completeness check of visits as 'NA' create issues in processing



##### V2 on 7 Oct 2018 ###########################
### inspect species/years for which breeding success==0
### erroneous use of Content=0 when content was not seen (i.e. AON)



library(tidyverse)
library(data.table)
library(lubridate)
#library(xlsx)    ## does not work on RSPB machine due to JAVA conflict
library(readxl)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 1: ASSESS EXTENT OF PROBLEM
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## only need to re-run after changes to database
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess\\RODBC_nest_import.r")), wait = FALSE, invisible = FALSE)

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\SeabirdBreedingSuccess"), silent=T)
load("GOUGH_nest_data.RData")
head(nestsDB)
head(visDB)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## CREATE SUMMARY AND SELECT SPECIES AND YEAR WHERE SUCCESS ==0
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

summary<-nestsDB %>% mutate(n=1) %>%
  mutate(SUCCESS=as.numeric(SUCCESS)) %>%
  group_by(Species, Year,Colony) %>%
  summarise(SampSize=sum(n), Success=mean(SUCCESS, na.rm=T))

suspicious<-summary %>%
  filter(Success %in% c(0,1)) %>%
  filter(SampSize>5) %>%
  filter(Year<2018)
dim(suspicious)
suspicious


fwrite(suspicious,"ERRONEOUS_IMPORT_DATA.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: FORMAT FROM CONSOLIDATED EXCEL FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


####### READ IN MANUALLY COPIED DATABASE OF BREEDING DATA #####
### THIS REQUIRES MANUAL CONVERSION TO CSV AS THE DIFFERENT DATE FORMATS IN EXCEL SCREW EVERYTHING UP

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA"), silent=T)

nests <- read_excel("BREEDING_DATABASE.xlsx", sheet="NEST_SUMMARIES")
visits <- fread("BREEDING_DATABASE_NEST_VISITS.csv")
dim(visits)
visits<-visits[,1:22]   ### eliminate blank columns at the end
head(nests)


## apply date formatting conversion to the whole data set
visits<- visits %>% mutate(DateOrig=gsub(".*-","",Date)) %>%
  mutate(DateGood=dmy(DateOrig))
visits %>% filter(is.na(DateGood)) %>% select(DateGood,DateOrig,Species,Site,Year)
head(visits)
str(visits)

### modify species abbreviations
species<-unique(visits$Species)
lkSpec<-data.frame(abbr=species, CODE=c("ATPE","BBPR","GOBU","SOAL","SOPE","SOPE","SOPE","TRAL","UNK","AYNA","NRPE","GRSH","AYNA","GRSH","TRAL"))
visits<- visits %>% mutate(Species=lkSpec$CODE[match(Species,lkSpec$abbr)])


### generate unique VisitID and NestID
visits<- visits %>% mutate(VisitID=seq(10001,dim(visits)[1]+10000,1)) %>%
  mutate(Colony=ifelse((Colony %in% c(NA,"")),Transect,Colony)) %>%         ## fill in colony name from transect
  mutate(NestID=paste(Species,Year,Colony,Quadrat,ID_nest_burrow, sep="_"))


### ASSESS MISSING CRITICAL INFO AND REMOVE WORTHLESS VISITS
dim(visits)
visits<- visits %>% filter(!is.na(STATUS)) ### %>% select(Species, Year, STAGE, ADULT, CONTENT, Comments,Notes)  ### remove visits where nest status is unknown
dim(visits)


### POPULATE MISSING STAGES FROM COMMENTS FIELD
unique(visits$Comments)

visits$STAGE=ifelse(visits$STAGE=="" & grepl("egg",visits$Comments,perl=T,ignore.case = T)==T,"egg",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("chick",visits$Comments,perl=T,ignore.case = T)==T,"chick",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("crech",visits$Comments,perl=T,ignore.case = T)==T,"chick",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("moult",visits$Comments,perl=T,ignore.case = T)==T,"chick",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("dead",visits$Comments,perl=T,ignore.case = T)==T,"dead",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("destroy",visits$Comments,perl=T,ignore.case = T)==T,"dead",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("broke",visits$Comments,perl=T,ignore.case = T)==T,"dead",visits$STAGE)
visits$STAGE=ifelse(visits$STAGE=="" & grepl("fail",visits$Comments,perl=T,ignore.case = T)==T,"dead",visits$STAGE)  
visits$STAGE=ifelse(visits$STAGE=="" & grepl("fledg",visits$Comments,perl=T,ignore.case = T)==T,"fledged",visits$STAGE)
visits$STATUS=ifelse(grepl("dead",visits$Comments,perl=T,ignore.case = T)==T,0,visits$STATUS)

### FORMAT THE MILLION STAGE DESCRIPTIONS INTO A FEW CATEGORIES
lkStages<-data.frame(abbr=unique(visits$STAGE), STAGE=NA)
lkStages<-lkStages %>%
  mutate(STAGE=ifelse(grepl("build",abbr,perl=T,ignore.case = T)==T,"TERR",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("egg",abbr,perl=T,ignore.case = T)==T,"INCU",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("pip",abbr,perl=T,ignore.case = T)==T,"INCU",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("AIA",abbr,perl=T,ignore.case = T)==T,"INCU",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("chick",abbr,perl=T,ignore.case = T)==T,"CHIC",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("hatch",abbr,perl=T,ignore.case = T)==T,"CHIC",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("guard",abbr,perl=T,ignore.case = T)==T,"CHIC",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("brood",abbr,perl=T,ignore.case = T)==T,"CHIC",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("band",abbr,perl=T,ignore.case = T)==T,"CHIC",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("fledg",abbr,perl=T,ignore.case = T)==T,"FLED",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("dead",abbr,perl=T,ignore.case = T)==T,"FAIL",STAGE)) %>% 
  mutate(STAGE=ifelse(grepl("carcass",abbr,perl=T,ignore.case = T)==T,"FAIL",STAGE)) %>%
  mutate(STAGE=ifelse(grepl("fail",tolower(abbr),perl=T,ignore.case = T)==T,"FAIL",STAGE)) %>% 
  mutate(STAGE=ifelse(grepl("gone",abbr,perl=T,ignore.case = T)==T,"FLED",STAGE))






### CREATE MATCHING INVENTORY OF NESTS AND FINAL OUTCOME

nests <- visits %>% mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
  mutate(Status=ifelse(STATUS==0,"Failed",ifelse(STAGE=="FLED","Fledged","Alive"))) %>%
  mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
  mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
  mutate(Colony=ifelse((Colony %in% c(NA,"")),Transect,Colony)) %>%         ## fill in colony name from transect
  mutate(Site=ifelse((Site %in% c(NA,"")),Quadrat,Site)) %>%         ## fill in site name from quadrat
  arrange(DateGood) %>%  ## arrange in chronological order so we can extract summary infor for first and last nest visits
  group_by(NestID, Species, Year, Colony, Site,Latitude, Longitude) %>%
  summarise(DateFound=min(DateGood),StageFound=first(Stage), DateLastChecked=max(DateGood), SUCCESS=last(STATUS))

## update the LastAlive date

DateLastAlive <- visits %>% mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
  mutate(Status=ifelse(STATUS==0,"Failed",ifelse(STAGE=="FLED","Fledged","Alive"))) %>%
  mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
  mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
  filter(Status=="Alive") %>%   ## select only the alive nests
  mutate(Site=ifelse((Site %in% c(NA,"")),Quadrat,Site)) %>%         ## fill in site name from quadrat
  arrange(DateGood) %>%  ## arrange in chronological order so we can extract summary infor for first and last nest visits
  group_by(NestID, Species, Year, Colony, Site) %>%
  summarise(DateLastAlive=max(DateGood))


nests <- merge(nests,DateLastAlive, by=c('NestID', 'Species', 'Year', 'Colony', 'Site'), all.x=T)

 

### Troubleshoot the large number of missing dates

visits %>% filter(is.na(DateGood))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: SELECT ONLY THOSE NESTS FROM TROUBLE SPECIES / YEAR / COLONY 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
suspicious$GROUP<-paste(suspicious$Species,suspicious$Year,ifelse(is.na(suspicious$Colony),"",suspicious$Colony), sep="_")
suspicious$GROUP

head(visits)
visits$GROUP<-paste(visits$Species,visits$Year,visits$Colony, sep="_")
visRED<-visits %>% filter(GROUP %in% suspicious$GROUP)
dim(visRED)

nests$GROUP<-paste(nests$Species,nests$Year,nests$Colony, sep="_")
nestsRED<-nests %>% filter(GROUP %in% suspicious$GROUP)
dim(nestsRED)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 4: LOOP OVER EVERY NEST TO ENSURE CORRECT CLASSIFICATION OF OUTCOME 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


### NOT YET FIXED
### stumbled over ATPE_2012_BlechnumBridge_26_34, which is success==0, but not in database !!!???
### for some GROUPs the failed nests were entirely removed, because content was never >0

head(nestsRED)
nestsRED$NumberVisits<-0
nestsRED$MonitorDuration<-0
nestsRED$AssumedFledged<-0
nestsRED$LastStage<-NA
nestsRED$PROBLEM<-0
cleanvisits<-data.frame()

for (n in unique(nestsRED$NestID)) {
  
  #### SELECT THE VISITS AND FORMAT THEM ###
  #### requires difficult sequence of formatting depending on which columns contain information ###
  
  nv<-visits %>%     filter(NestID==n) %>%     select(Species,Island,Colony,Site,Year,Season,DateGood,Transect,Quadrat,ID_nest_burrow,STATUS,CONTENT,STAGE,ADULT,Comments) %>%
    mutate(STAGE=ifelse(grepl("fledg",Comments,perl=T,ignore.case = T)==T,"Fledged",STAGE)) %>%
    
    mutate(CONTENT=ifelse(CONTENT==0 & STATUS==1 & ADULT>0,"AIA",CONTENT)) %>%         ### THIS LINE REPLACES ERRONEOUS 0 in CONTENT
    mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
    mutate(Stage=ifelse(CONTENT=="AIA","INCU",Stage)) %>%
    #mutate(Status=ifelse(STATUS==0,"Failed",ifelse(Stage=="FLED","Fledged","Alive"))) %>%
    mutate(Status=ifelse(STATUS==1,"Alive","Failed")) %>%
    mutate(Status=ifelse(!is.na(Stage) & Stage=="FLED","Fledged",Status)) %>%
    mutate(Stage=ifelse(!is.na(Status) & Status=="Failed","FAIL",Stage)) %>%
    mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
    mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
    
    ## this following line sets the nest to 'failed' if content==0
    mutate(Status=ifelse(Content==0,ifelse(!is.na(Stage) & Stage=="FLED","Fledged","Failed"),ifelse(!is.na(Stage) & Stage=="FLED","Fledged",Status))) %>%
    mutate(Stage=ifelse(is.na(Content),Stage,ifelse(Content==0,ifelse(!is.na(Status) & Status=="Fledged","FLED",Stage),ifelse(Status=="Fledged","FLED",Stage)))) %>%  ## This introduces NA for AIA because Content=NA
    mutate(Status=ifelse(CONTENT=="AIA","Alive",Status)) %>%
    mutate(Status=ifelse(!is.na(Stage) & Stage=="FLED","Fledged",Status)) %>%    ## this creates a problem if Stage is NA
    mutate(Colony=ifelse((Colony %in% c(NA,"")),Transect,Colony)) %>%         ## fill in colony name from transect
    mutate(Site=ifelse((Site %in% c(NA,"")),Quadrat,Site)) %>%         ## fill in site name from quadrat
    arrange(DateGood) %>%
    select(Species,Island,Colony,Site,Year,Season,DateGood,Transect,Quadrat,ID_nest_burrow,STATUS,CONTENT,STAGE,ADULT,Comments,Stage,Content,Status)
  
  #### IDENTIFY NESTS THAT HAVE NONSENSICAL SEQUENCE OF FAILURE -> CHICK or FLEDGED -> CHICK ###
  ## if status went to 0 it should never go back to 1
  ## however, if chick had wandered away then nest may have actually survived even if recorded as failed.
  
  ## FIRST, SELECT ONLY NESTS THAT WERE EVER ALIVE
  if("Alive" %in% nv$Status){
  
  ## SECOND, REMOVE ALL STATUS=0 VISITS AT THE BEGINNING (BEFORE A NEST ACTUALLY EXISTS)
  nvisits<-dim(nv)[1]
  start<-which(nv$Status=="Alive")[1]
  nv<-nv[start:nvisits,]
  
  ## CHECK FOR ALL OTHER ISSUES
  if("FLED" %in% nv$Stage){
    nv<-nv[1:which(nv$Stage=="FLED")[1],] ## eliminate all visits after a nest had fledged
  }
  nv$STATUS<-ifelse(nv$Status=="Alive",1,0)
  statusseq<-nv %>% mutate(sequence=as.numeric(STATUS)-lag(as.numeric(STATUS),default=1)) %>% select(sequence)
  statusseq$sequence[is.na(statusseq$sequence)]<-0
  if(max(statusseq$sequence, na.rm=T)>0){
    ## find the visits when the nest suddenly was resuscitated
    resusindex<-which(statusseq$sequence > 0)[1]
    nvisits<-dim(nv)[1]
    nvR<- nv[resusindex:nvisits,]
    
    ## if the resuscitation only ever recorded an adult, delete these visits
    if(max(nvR$CONTENT)==0 & max(nvR$ADULT)>0){
      nv<-nv[(1:(resusindex-1)),]
    }
    
    ## if the resuscitation found an actual chick, delete the visit that reported failure   
    if(max(nvR$CONTENT)>0 & "CHIC" %in% nvR$Stage){
      nv<-nv[-(resusindex-1),]
    }
    
    ## if the number of visits after resuscitation are more than before, then retain only the    
    if(dim(nvR)[1] > dim(nv[start:resusindex,])[1]){
      nv<-nvR
    }
  }
  
  #### REMOVE REDUNDANT VISITS AFTER FAILURE OR FLEDGING ###
  if("Failed" %in% nv$Status | "FLED" %in% nv$Stage){
    termdate<-min(nv$DateGood[nv$Status=="Failed" | nv$Stage=="FLED"], na.rm=T)
  }else{
    termdate<-max(nv$DateGood, na.rm=T)
  }
  
  nv <- nv %>% filter(DateGood <= termdate)
  nestsRED$SUCCESS[nestsRED$NestID==n]<-nv$STATUS[nv$DateGood==termdate]
  
  #### FLEDGED NESTS ARE SUCCESSFUL ####
  if ("FLED" %in% nv$Stage){nestsRED$SUCCESS[nestsRED$NestID==n]<-1}
  
  #### FILL IN SIMPLE METRICS FOR THE NESTS ###
  nestsRED$DateLastAlive[nestsRED$NestID==n]<-max(nv$DateGood[nv$Status=="Alive"])
  nestsRED$NumberVisits[nestsRED$NestID==n]<-dim(nv)[1]
  nestsRED$MonitorDuration[nestsRED$NestID==n]<-as.numeric(difftime(if_else(0 %in% nv$STATUS,min(nv$DateGood[nv$STATUS==0]),max(nv$DateGood)),min(nv$DateGood),'days'))
  nestsRED$AssumedFledged[nestsRED$NestID==n]<-ifelse("FLED" %in% nv$Stage,0,1)
  nestsRED$LastStage[nestsRED$NestID==n]<-last(nv$Stage)
  
  #### COPY THE RETAINED CLEAN NEST VISITS INTO A NEW DATA FRAME ###
  cleanvisits<-rbind(cleanvisits,nv)
  

  ##### IDENTIFY PROBLEM NESTS WHERE SUCCESS DOES NOT MATCH WITH NEST VISITS
  if(nestsRED$SUCCESS[nestsRED$NestID==n]==1){
    nestsRED$PROBLEM[nestsRED$NestID==n]<-ifelse("FAIL" %in% nv$Stage | "Failed" %in% nv$Status,1,0)  ##removed  | 0 %in% nv$STATUS,1,0 because 0 is also for fledged nests
  }
  
  if(nestsRED$SUCCESS[nestsRED$NestID==n]==0){
    nestsRED$PROBLEM[nestsRED$NestID==n]<-max(ifelse(grepl("fledg",nv$Comments,perl=T,ignore.case = T)==T,1,0))
  }
  
  ## REMOVE ALL NESTS THAT NEVER EXISTED (unoccupied burrows)
  }else{nestsRED<-nestsRED[!nestsRED$NestID==n,]}

  
  
    
}   ## end of loop across all nests





######### INSPECT ALL THE PROBLEM NESTS ###########

probvis<-nestsRED %>% filter(PROBLEM==1) %>% select(NestID)
n=probvis[1,1]





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 5: UPDATE NESTS OUTCOME IN DATABASE AND ADD NESTS NOT YET IN DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue")
head(nestsRED)
dim(nestsRED)

### create table matching the database
exportNEST<- nestsRED %>% mutate(Time="12:00") %>%
  mutate(Nest_label=NestID) %>% 
  mutate(Completed=ifelse(AssumedFledged==1,0,1))%>%
  select(Nest_label,Species,Year,Colony,Site,Latitude,Longitude,DateFound,StageFound, DateLastAlive, DateLastChecked,LastStage,Completed,SUCCESS)


### match to check which nests already exist
head(nestsDB)
head(exportNEST)

updNests<-exportNEST %>% filter(Nest_label %in% nestsDB$Nest_label)
updNestsDB<-nestsDB %>% filter(Nest_label %in% updNests$Nest_label)
updNests[!(updNests$SUCCESS==updNestsDB$SUCCESS),]      ### these nests differe between the output here and the DB and need to be updated

### export the nests that need to be updated
fwrite(updNests[!(updNests$SUCCESS==updNestsDB$SUCCESS),],"Gough_Nests_tobe_updated.csv")

exportNEST %>% filter(Species=="AYNA")


### export the nests that need to be added
addNests<-exportNEST %>% filter(!(Nest_label %in% nestsDB$Nest_label))
fwrite(addNests,"Gough_Nests_tobe_added.csv")




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 6: CREATE EXPORT OF NEST VISITS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

head(cleanvisits)
export<- cleanvisits %>% mutate(Time="12:00") %>%
  mutate(NestID=paste(Species,Year,Colony,Quadrat,ID_nest_burrow, sep="_")) %>% 
  mutate(Attendance=ADULT)%>% ### this is just a number and no ring info
  mutate(FailureCause=Comments)%>% 
  mutate(NOTES="NA") %>%
  mutate(VisitID=seq(1,dim(cleanvisits)[1]),1)%>%
  select(VisitID, NestID, DateGood, Time, Stage, Status, Content, Attendance, FailureCause,NOTES)


dim(export)



#### REMOVE THE VISITS THAT ALREADY EXIST IN THE DB WITH THE SAME STATUS #######
head(export)
head(visDB)
visDB$Nest_label<-nestsDB$Nest_label[match(visDB$NestID,nestsDB$NestID)]
existingvis<-visDB[,c(12,3,6)]
names(existingvis)<-names(export)[c(2,3,6)]
existingvis$IN_DB<-1
export<-merge(export,existingvis, by=c(names(export)[c(2,3,6)]), all.x=T)
dim(export)
export<-export[is.na(export$IN_DB),]
dim(export)

#### IDENTIFY VISITS THAT EXIST IN THE DB BUT NEED STATUS UPDATE #######
names(existingvis)[3]<-"Status_DB"
export$IN_DB<-NULL
export<-merge(export,existingvis, by=c("NestID","DateGood"), all.x=T)
dim(export)
upd_visits<-export[!is.na(export$IN_DB),]
add_visits<-export[is.na(export$IN_DB),]
dim(upd_visits)
dim(add_visits)

fwrite(upd_visits,"Gough_nestVisits_tobe_updated.csv")
fwrite(add_visits,"Gough_nestVisits_tobe_added.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 7: TROUBLESHOOTING
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### check visits for nests that look strange:
visits %>% filter(NestID=="NRPE_2012__NA_41") %>% select(DateGood,STAGE,STATUS,CONTENT,ADULT,Comments)
export %>% filter(NestID=="NRPE_2012__NA_41") %>% select(DateGood,Stage,Status,Content,Attendance,NOTES)
head(cleanvisits)




