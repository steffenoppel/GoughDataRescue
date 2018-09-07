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


library(tidyverse)
library(data.table)
library(lubridate)
#library(xlsx)    ## does not work on RSPB machine due to JAVA conflict
library(readxl)






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 1: FORMAT FROM CONSOLIDATED EXCEL FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


####### READ IN MANUALLY COPIED DATABASE OF BREEDING DATA #####
### THIS REQUIRES MANUAL CONVERSION TO CSV AS THE DIFFERENT DATE FORMATS IN EXCEL SCREW EVERYTHING UP

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA"), silent=T)

nests <- read_excel("BREEDING_DATABASE.xlsx", sheet="NEST_SUMMARIES")
#visits <- read_excel("BREEDING_DATABASE.xlsx", sheet="NEST_VISITS")
colony <- read_excel("BREEDING_DATABASE.xlsx", sheet="COLONY_SUMMARIES")

head(nests)

head(colony)



#### FORMAT NEST VISITS TO MATCH WITH DATABASE ###
# ## read in from converted csv file to hopefully rescue many of the dates
# visits <- fread("BREEDING_DATABASE_NEST_VISITS.csv")
# 
# ### format the dates 
# visits<- visits %>% mutate(DateOrig=Date) %>%
#   mutate(DateGood=dmy(DateOrig))
# 
# ## check what went wrong:
# visits %>% filter(is.na(DateGood)) %>% select(DateGood,DateOrig,Species,Site,Year)
# 
# ## remove all the date ranges:
# visits %>% filter(is.na(DateGood)) %>% select(DateGood,DateOrig,Species,Site,Year) %>%
#   mutate(DateModif=gsub(".*-","",DateOrig)) %>%
#   mutate(DateGood=dmy(DateModif))

## apply date formatting conversion to the whole data set
visits <- fread("BREEDING_DATABASE_NEST_VISITS.csv")
dim(visits)
visits<-visits[,1:22]   ### eliminate blank columns at the end
visits<- visits %>% mutate(DateOrig=gsub(".*-","",Date)) %>%
  mutate(DateGood=dmy(DateOrig))
visits %>% filter(is.na(DateGood)) %>% select(DateGood,DateOrig,Species,Site,Year)
head(visits)
str(visits)

### modify species abbreviations
species<-unique(visits$Species)

#visits %>% filter(Species=="") %>% dplyr::select(Directory,Year, Colony) ## to figure out what a species abbreviation might mean

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
  summarise(DateFound=min(DateGood),StageFound=first(STAGE), DateLastChecked=max(DateGood), SUCCESS=last(STATUS))

## update the LastAlive date

DateLastAlive <- visits %>% mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
  mutate(Status=ifelse(STATUS==0,"Failed",ifelse(STAGE=="FLED","Fledged","Alive"))) %>%
  mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
  mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
  filter(Status=="Alive") %>%   ## select only the alive nests
  arrange(DateGood) %>%  ## arrange in chronological order so we can extract summary infor for first and last nest visits
  group_by(NestID, Species, Year, Colony, Site) %>%
  summarise(DateLastAlive=max(DateGood))


nests <- merge(nests,DateLastAlive, by=c('NestID', 'Species', 'Year', 'Colony', 'Site'), all.x=T)

 

### Troubleshoot the large number of missing dates

visits %>% filter(is.na(DateGood))





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: LOOP OVER EVERY NEST TO ENSURE CORRECT CLASSIFICATION OF OUTCOME 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

head(nests)
nests$NumberVisits<-0
nests$MonitorDuration<-0
nests$AssumedFledged<-0
nests$LastStage<-NA
nests$PROBLEM<-0
cleanvisits<-data.frame()

for (n in unique(nests$NestID)) {
  
  #### SELECT THE VISITS AND FORMAT THEM ###
  #### requires difficult sequence of formatting depending on which columns contain information ###
  
  nv<-visits %>%     filter(NestID==n) %>%     select(Species,Island,Colony,Site,Year,Season,DateGood,Transect,Quadrat,ID_nest_burrow,STATUS,CONTENT,STAGE,ADULT,Comments) %>%
    mutate(STAGE=ifelse(grepl("fledg",Comments,perl=T,ignore.case = T)==T,"Fledged",STAGE)) %>%
    mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
    mutate(Stage=ifelse(CONTENT=="AIA","INCU",Stage)) %>%
    #mutate(Status=ifelse(STATUS==0,"Failed",ifelse(Stage=="FLED","Fledged","Alive"))) %>%
    mutate(Status=ifelse(STATUS==1,"Alive","Failed")) %>%
    mutate(Status=ifelse(!is.na(Stage) & Stage=="FLED","Fledged",Status)) %>%
    mutate(Stage=ifelse(!is.na(Status) & Status=="Failed","FAIL",Stage)) %>%
    mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
    mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
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
  nests$SUCCESS[nests$NestID==n]<-nv$STATUS[nv$DateGood==termdate]
  
  #### FLEDGED NESTS ARE SUCCESSFUL ####
  if ("FLED" %in% nv$Stage){nests$SUCCESS[nests$NestID==n]<-1}
  
  #### FILL IN SIMPLE METRICS FOR THE NESTS ###
  nests$DateLastAlive[nests$NestID==n]<-max(nv$DateGood[nv$Status=="Alive"])
  nests$NumberVisits[nests$NestID==n]<-dim(nv)[1]
  nests$MonitorDuration[nests$NestID==n]<-as.numeric(difftime(if_else(0 %in% nv$STATUS,min(nv$DateGood[nv$STATUS==0]),max(nv$DateGood)),min(nv$DateGood),'days'))
  nests$AssumedFledged[nests$NestID==n]<-ifelse("FLED" %in% nv$Stage,0,1)
  nests$LastStage[nests$NestID==n]<-last(nv$Stage)
  
  #### COPY THE RETAINED CLEAN NEST VISITS INTO A NEW DATA FRAME ###
  cleanvisits<-rbind(cleanvisits,nv)
  

  ##### IDENTIFY PROBLEM NESTS WHERE SUCCESS DOES NOT MATCH WITH NEST VISITS
  if(nests$SUCCESS[nests$NestID==n]==1){
    nests$PROBLEM[nests$NestID==n]<-ifelse("FAIL" %in% nv$Stage | "Failed" %in% nv$Status,1,0)  ##removed  | 0 %in% nv$STATUS,1,0 because 0 is also for fledged nests
  }
  
  if(nests$SUCCESS[nests$NestID==n]==0){
    nests$PROBLEM[nests$NestID==n]<-max(ifelse(grepl("fledg",nv$Comments,perl=T,ignore.case = T)==T,1,0))
  }
  
  ## REMOVE ALL NESTS THAT NEVER EXISTED (unoccupied burrows)
  }else{nests<-nests[!nests$NestID==n,]}

  
  
    
}   ## end of loop across all nests





######### INSPECT ALL THE PROBLEM NESTS ###########

probvis<-nests %>% filter(PROBLEM==1) %>% select(NestID)
n=probvis[1,1]






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: CREATE EXPORT OF NEST VISITS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

head(cleanvisits)

export<- cleanvisits %>% mutate(Time="12:00") %>%
  mutate(NestID=paste(Species,Year,Colony,Quadrat,ID_nest_burrow, sep="_")) %>% 
  mutate(Attendance=ADULT)%>% ### this is just a number and no ring info
  mutate(FailureCause=Comments)%>% 
  mutate(NOTES="NA") %>%
  mutate(VisitID=seq(1,dim(cleanvisits)[1]),1)%>%
  select(VisitID, NestID, DateGood, Time, Stage, Status, Content, Attendance, FailureCause,NOTES)

head(export)
dim(export)
#fwrite(export,"Gough_nestVisits_export.csv")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 4: CREATE EXPORT OF NESTS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

head(nests)
dim(nests)

exportNEST<- nests %>% mutate(Time="12:00") %>%
  mutate(Nest_label=NestID) %>% 
  mutate(Completed=ifelse(AssumedFledged==1,0,1))%>%
  select(Nest_label,Species,Year,Colony,Site,Latitude,Longitude,DateFound,StageFound, DateLastAlive, DateLastChecked,SUCCESS,Completed,LastStage)

head(exportNEST)
dim(exportNEST)
fwrite(exportNEST,"Gough_nests_export.csv")













##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART X: FORMAT FROM RAW EXCEL FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### provided by Jaimie Cleeland




# #Read in Atlantic Yellow-nosed albatross data
# molly <- read.xlsx(file="/Users/jaimiec/Documents/Science Projects/Tristan Albatross Tracking/Gony monitoring colony 2017 - 2018.xlsx", sheetIndex = 1, header = T, stringsAsFactors=F)
# molly$species <- "Atlantic Yellow-nosed albatross"
# 


#Read in Tristan albatross data
gony <- read.xlsx(file="/Users/jaimiec/Documents/Science Projects/Tristan Albatross Tracking/Gony monitoring colony 2017 - 2018.xlsx", sheetIndex = 1, header = T, stringsAsFactors=F)
gony$species <- "Tristan albatross"

gony <- gony[, c("Colony", "Nest", "Male..Colour", "Male..Metal", 
                 "Female..Colour", "Female.Metal", "Latitude", "Longitude", "Notes", "Egg", "Hatch", "Fledge", "Chick.ring", 
                 "X06.01.18_bird", "X06.01.18_contents", "X13.01.18_bird", "X13.01.18_contents", 
                 "X14.01.18_bird", "X14.01.18_contents", "X19.01.18_bird", "X19.01.18_contents", 
                 "X26.01.18_bird", "X26.01.18_contents", "X05.02.18_bird", "X05.02.18_contents", 
                 "X10.02.18_bird", "X10.02.18_contents", "X23.02.18_bird", "X23.02.18_contents", 
                 "X05.03.18_bird", "X05.03.18_contents", "X06.03.18_bird", "X06.03.18_contents", 
                 "X17.03.18_bird", "X17.03.18_contents", "X18.03.18_bird", "X18.03.18_contents", 
                 "X19.03.18_bird", "X19.03.18_contents", "X20.03.18_bird", "X20.03.18_contents", 
                 "X22.03.18_bird", "X22.03.18_contents", "X23.03.18_bird", "X23.03.18_contents", 
                 "X27.03.18_bird", "X27.03.18_contents", "X28.03.18_bird", "X28.03.18_contents", 
                 "X06.04.18_bird", "X06.04.18_contents", "X07.04.18_bird", "X07.04.18_contents", 
                 "X09.04.18_bird", "X09.04.18_contents", "X14.04.18_bird", "X14.04.18_contents", 
                 "X16.04.18_bird", "X16.04.18_contents", "X18.04.18_bird", "X18.04.18_contents", 
                 "X30.04.18_bird", "X30.04.18_contents", "X05.05.18_bird", "X05.05.18_contents", 
                 "X06.06.18_bird", "X06.06.18_contents", "species")]

#As an example, check the header layout of the Tristan data
colnames(gony)

#Reformat header
colnames(gony) <- tolower(gsub(pattern = "[.]", replacement = "", colnames(gony)))


#Find all dates nest observations were made
dates <- unique(gsub(pattern = "_bird|_contents|x", replacement = "", x = colnames(gony[,grep(pattern = "_bird|_contents", x =  colnames(gony))])))

#Create an empty data frame to add all observational data into
df <- gony[1,c(1:13, 68)]
df$bird <- NA
df$contents <- NA
df$date <- as.Date(NA)

#Loop through all dates, one at a time, creating a row for each unique nest observations
for(i in 1:length(dates)){
  subd <- gony[,c(colnames(gony)[c(1:13, 68)], colnames(gony[,grep(pattern = dates[i], x =  colnames(gony))]))]
  subd$date <- as.Date(dates[i], format="%d%m%y")
  colnames(subd) <- colnames(df)
  df <- rbind(df, subd)
}
df <- df[-1,]

df <- df[!is.na(df$contents),]
head(df)

#Subset out all observations with both partners
part1 <- df[grep(pattern = "&", df$bird),]
part1$bird[1]
part2 <- part1
df <- df[grep(pattern = "&", df$bird, invert = T),]
#Format to have only one bird per row
part1$bird <-  sub(' & .*', '', part1$bird)
part2$bird <-  sub('.* & ', '', part1$bird)
#Add formatted data back into our master dataframe
df <- rbind(df, part1, part2)

####################sort(unique(df$bird))
df$metal <- NA
df$darvic <- NA

df$sex <- NA
for(i in 1:nrow(df)){
  if(!df[i,c(15)]%in%c("U/R", NA)){
    df$sex[i] <- which(df[i,c(15)]%in%df[i,c(3:6)])
  }
}
df$sex[df$species=="Tristan albatross"&df$sex%in%c(1,2)] <- "M"
df$sex[df$species=="Tristan albatross"&df$sex%in%c(3,4)] <- "F"

sort(unique(df$contents))

df$nest[nchar(df$nest)==1] <- paste0("00", df$nest[nchar(df$nest)==1])
df$nest[nchar(df$nest)==2] <- paste0("0", df$nest[nchar(df$nest)==2])
df$nest <- gsub(pattern = "T", replacement = "", x = df$nest)
# df$nest <- paste0("201718_","TA", df$nest)
head(sort(unique(df$nest)))

#Remove all observations with notes
notes <- df[!is.na(df$notes), c("nest", "date", "notes")]
#Make sure none are duplicated- we expect them to be because there was only one box for notes per nest with multiple observations
notes <- notes[!duplicated(notes$nest, notes$notes),]
#Wipe notes from our Master dataframe
df$notes <- NULL
#Ok split all observations at date
ls <- strsplit(x = notes$notes, split=", ")
names(ls) <- notes$nest
notes <- data.frame(date = as.Date(substr(x = unlist(ls), start = 1, stop = 8), format="%d/%m/%y"), nest= substr(x = names(unlist(ls)), start = 1, stop = 3), notes = substr(x = unlist(ls), start = 9, nchar(unlist(ls))), stringsAsFactors = F)
head(notes)
#Merge split notes back into master dataframe
df <- merge(df, notes, by=c("nest", "date"), all.x=T)

df <- df[,c("date", "species", "colony", "nest", "latitude", "longitude", "metal", "darvic", "sex", "contents", "chickring", "notes")]
head(df)