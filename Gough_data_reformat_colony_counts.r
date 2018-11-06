##################################################
### DATA RESCUE OF GOUGH MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk

## main purpose is to rescue data that are inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology

## first created 24 Oct 2018
## manually fixed those lines where breeding success >100% - some nests were not counted


## found major problem in triplicate counts to be added to Access database on 6 Nov 2018
## major troubleshoot to prevent duplication/triplication of records


library(tidyverse)
library(data.table)
library(lubridate)
#library(xlsx)    ## does not work on RSPB machine due to JAVA conflict
library(readxl)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 1: IMPORT TABLE OF COUNTS FROM ACCESS DATABASE FOR FORMAT AND TABLE HEADERS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## only need to re-run after changes to database
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue\\RODBC_count_import.r")), wait = FALSE, invisible = FALSE)

#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)
load("GOUGH_seabird_count_data.RData")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: FORMAT FROM CONSOLIDATED EXCEL FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

####### READ IN MANUALLY COPIED DATABASE OF BREEDING DATA #####
summaries <- read_excel("BREEDING_DATABASE_NEW.xlsx", sheet="COLONY_SUMMARIES")
dim(summaries)
head(summaries)

### modify species abbreviations
species<-unique(summaries$Species)
lkSpec<-data.frame(abbr=species, CODE=c("TRAL","SOAL","AYNA","SGPE","NRPE","GRPE"))
summaries<- summaries %>% mutate(Species=lkSpec$CODE[match(Species,lkSpec$abbr)])

### gather into vertical format
summvert<- summaries %>% select(Species, Colony, Year,N_nests,N_eggs,N_adult,N_hatched,N_fledged, Directory, Filename) %>%
  gather(key="COHORT",value="N",-Species,-Colony,-Year,-Directory,-Filename)
  
  
### REPLACE COHORT NAMES
cohorts<-unique(summvert$COHORT)
lkCoh<-data.frame(abbr=cohorts, CODE=c("AON","INCU","AD","CHIC","CHIC"))
summvert<- summvert %>% mutate(COHORT=lkCoh$CODE[match(COHORT,lkCoh$abbr)])
  






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: MANUALLY ADD THE DATES FROM THE FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
head(summvert)
summvert$Date<-""
#fwrite(summvert,"Gough_Counts_tobe_dated.csv")

unique(summvert$Filename)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 4: ENSURE DATA INTEGRITY WITH DATA BASE - LOCATIONS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
summvert<-fread("Gough_Counts_tobe_dated.csv")
dim(summvert)
usedlocs<-unique(summvert$Colony)

### identify locations not in database
newlocs<-usedlocs[!(usedlocs %in% locDB$LocationName)]
newlocs<-data.frame(bullshit=newlocs, LOCATION=NA)
newlocs<-newlocs %>%
  mutate(LOCATION=ifelse(grepl("admirals",bullshit,perl=T,ignore.case = T)==T,"Admirals",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Area on coastal side of path to Tumbledown beach",bullshit,perl=T,ignore.case = T)==T,"Area 8",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Area between streams of Blechnum bridge and water dam",bullshit,perl=T,ignore.case = T)==T,"Area 2",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Area from where path leaves golden highway",bullshit,perl=T,ignore.case = T)==T,"Area 6",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Triple Peak",bullshit,perl=T,ignore.case = T)==T,"Triple Peak",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Cavern Head",bullshit,perl=T,ignore.case = T)==T,"Area to south of Cavern head",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Cliff above two penguin colonies at Seal beach",bullshit,perl=T,ignore.case = T)==T,"Cliffs at Seal Beach above main penguin colonies",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Cliff on descent to Seal beach",bullshit,perl=T,ignore.case = T)==T,"Cliff at Seal Beach either side of rope",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Cliff to north above Admirals",bullshit,perl=T,ignore.case = T)==T,"Cliff to north of Admirals penguin colony",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Cliff to northwest of vertical waterfall",bullshit,perl=T,ignore.case = T)==T,"Cliff to northwest of vertical waterfall above Hau",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("view point in Snoek Gat",bullshit,perl=T,ignore.case = T)==T,"Cliff to south of view point in Snoek Gat (where c",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Cliffs to north and east of crane",bullshit,perl=T,ignore.case = T)==T,"Cliffs to north and east of crane (from below cran",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("East of South Point",bullshit,perl=T,ignore.case = T)==T,"Cavern Head",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("G.P. Valley",bullshit,perl=T,ignore.case = T)==T,"GP Valley",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("GP Plain",bullshit,perl=T,ignore.case = T)==T,"GP Valley",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Gonydale path-Hummocks",bullshit,perl=T,ignore.case = T)==T,"Hummocks",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Green Hill",bullshit,perl=T,ignore.case = T)==T,"Green Hill South",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Houlround pt",tolower(bullshit),perl=T,ignore.case = T)==T,"Cliff to northwest of vertical waterfall above Hau",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Low Hump",tolower(bullshit),perl=T,ignore.case = T)==T,"Low Hump",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Marie",tolower(bullshit),perl=T,ignore.case = T)==T,"Maries Colony",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Rich's",tolower(bullshit),perl=T,ignore.case = T)==T,"Richmond Hill",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Ridge on path up to tafelkop, from ",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 7",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Ridge to east of ridge up to Tafelkop",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 7",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Control area, Between streams lower and upper section, up to boundary of Phylica trees",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 3",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Satelite area over two streams",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 10",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Thin ridge between waterpipe stream and golden highway stream, up to highest point of ridge",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 5",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Under bluffs",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 4",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Serengeti",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 9",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Study colony",tolower(bullshit),perl=T,ignore.case = T)==T,"Area 1",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Rockhopper Point",tolower(bullshit),perl=T,ignore.case = T)==T,"Rockhopper Point",LOCATION)) %>% 
  mutate(LOCATION=ifelse(grepl("Seal Beach",tolower(bullshit),perl=T,ignore.case = T)==T,"Seal Beach",LOCATION)) %>%
  mutate(LOCATION=ifelse(grepl("Tumbledown",tolower(bullshit),perl=T,ignore.case = T)==T,"Tumbledown",LOCATION))


### UPDATE LOCATIONS IN SURVEYS

summvert<- summvert %>% mutate(Site=Colony) %>%
  mutate(Colony=ifelse(Site %in% locDB$LocationName,Site,newlocs$LOCATION[match(Site,newlocs$bullshit)]))
head(summvert)


### TROUBLESHOOT UNKNOWNS
# problemshit<-summvert %>%  group_by(Colony, Site, Species, Year) %>%
#   summarise(VisitID=sum(N)) %>%
#   filter(is.na(Colony))
# problemshit
# fwrite(problemshit,"LOCS_to_be_sorted.csv")

### EVERYTHING ELSE IS UNKNOWN
summvert$Colony[is.na(summvert$Colony)]<-"Unknown"





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 5: INSERT MISSING DATES BY TAKING AVERAGE OF PREVIOUS YEARS 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

summvert <- summvert %>% mutate(Date=dmy(DATE))
summvert %>% filter(is.na(Date))

### FOR TRISTAN ALBATROSS, COUNTS OF ADULT ARE THE SAME AS AON
summvert$COHORT[summvert$Species=="TRAL" & summvert$COHORT=="AD"]<-"AON"

### summarise count dates for known data
knowndates<- summvert %>% select(Species,Colony,COHORT, Year,N, Date) %>%
  mutate(day=day(Date),month=month(Date)) %>%
  group_by(Species,COHORT,Colony) %>%
  summarise(mean_month=round(mean(month,na.rm=T),0),mean_day=round(mean(day,na.rm=T),0))

knowndates %>% filter(is.na(mean_month))
summvert %>% filter(Species=="TRAL") %>% filter(COHORT=="AD")
unique(knowndates$COHORT[knowndates$Species=="TRAL"])

### IMPUTE DATES FOR UNKNOWN DATA

unknowns<-summvert %>% filter(is.na(Date)) %>% select(Species,Colony,COHORT, Year) %>%
  left_join(knowndates,by=c("Species","Colony","COHORT")) %>%
  mutate(Date=dmy(paste(mean_day,mean_month,Year, sep="/"))) %>%
  arrange(Species, Colony, COHORT,Year)
unknowns
dim(unknowns)


### INSERT THE CREATED DATES BACK INTO THE OVERALL TABLE
summvert <- summvert %>% arrange(Date,Species, Colony, COHORT,Year)
misscols<-is.na(summvert$Date)
summvert$Date[misscols] <- unknowns$Date  ### this is a dangerous replacement as it relies on identical sort order


# summvert$Date[misscols] <- summvert[is.na(summvert$Date),] %>%      
#   left_join(unknowns,by=c("Species","Colony","Year","COHORT")) %>%        ### DID NOT WORK BECAUSE JOIN CREATED MORE ROWS
#   select(Date.y)

summvert %>% filter(is.na(Date))
dim(summvert)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 6: SPLIT THE TABLE INTO THE RIGHT FORMAT - SURVEYS AND COUNTS 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

unique(summvert$Site)
head(survDB)
head(countDB)

### specify the surveys
surveys<- summvert %>% mutate(Method=ifelse(Species %in% c("SOAL","TRAL","AYNA"),"SURV","COLO")) %>%
  mutate(Site=gsub("[[:punct:]]","_" , Site,ignore.case = TRUE)) %>%
  group_by(Colony, Site, Date, Method) %>%
  summarise(VisitID=sum(N)) %>%
  filter(!Colony=="")
dim(surveys)
head(surveys)

summvert %>% filter(is.na(Date))

### create unique VisitID number
startID<-max(survDB$VisitID)+1
surveys$VisitID<-seq(startID,max(survDB$VisitID)+dim(surveys)[1],1)


### specify the counts at those surveys
## fixed on 6 Nov because 'merge' created duplicate entries

counts <- summvert %>% mutate(Method=ifelse(Species %in% c("SOAL","TRAL","AYNA"),"SURV","COLO")) %>%
  mutate(Site=gsub("[[:punct:]]","_" , Site,ignore.case = TRUE)) %>%
  left_join(surveys,by=c("Colony","Site","Date","Method")) %>%
  filter(!Colony=="") %>%
  mutate(Breed_Stage=ifelse(COHORT %in% c("AON","INCU","AD"),"INCU","CHIC")) %>%
  group_by(VisitID,Species, Breed_Stage, COHORT) %>%
  summarise(Number=sum(N)) %>%
  select(VisitID,Species, Breed_Stage, COHORT, Number) 

head(counts)
dim(counts)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 7: CREATE EXPORT OF NEST VISITS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

fwrite(surveys,"Gough_surveys_tobe_added.csv")
fwrite(counts,"Gough_counts_tobe_added.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 8: SUMMARY TO CHECK FOR DATA GAPS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### by colony - this is almost impossible to look through
SUMMARY<- summvert %>% group_by(Species,Colony,Year,COHORT) %>%
  summarise(N=sum(N, na.rm=T)) %>%
  spread(COHORT,N)
SUMMARY


### BY SPECIES AND YEAR
SUMMARY<- summvert %>% group_by(Species,Year,COHORT) %>%
  summarise(N=sum(N, na.rm=T)) %>%
  spread(COHORT,N)
SUMMARY


### CREATE FULL LIST OF SPECIES AND YEAR AND FLAG UP MISSING DATA
full<-expand.grid(Species=unique(summvert$Species),Year=unique(summvert$Year))

MISSING<-full %>% filter(Year>1999) %>%
  left_join(SUMMARY,by=c("Species","Year")) %>%
  mutate(nothing=ifelse(is.na(AD) & is.na(AON) & is.na(CHIC) & is.na(EGG) & is.na(INCU),1,0)) %>%
  filter(nothing==1) %>% select(Species,Year) %>%
  filter(Species!="GRPE") %>%
  arrange(Species, Year)

MISSING

fwrite(MISSING,"Gough_missing_census_data.csv")
  