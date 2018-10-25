##################################################
### DATA RESCUE OF GOUGH MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk

## main purpose is to rescue data that are inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology

## first created 24 Oct 2018
## manually fixed those lines where breeding success >100% - some nests were not counted

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
## PART 5: SPLIT THE TABLE INTO THE RIGHT FORMAT - SURVEYS AND COUNTS 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

unique(summvert$N)
head(survDB)
head(countDB)

### specify the surveys
surveys<- summvert %>% mutate(Method=ifelse(Species %in% c("SOAL","TRAL","AYNA"),"SURV","COLO")) %>%
  group_by(Colony, Site, DATE, Method) %>%
  summarise(VisitID=sum(N)) %>%
  filter(!Colony=="")
dim(surveys)
head(surveys)

### create unique VisitID number
startID<-max(survDB$VisitID)+1
surveys$VisitID<-seq(startID,max(survDB$VisitID)+dim(surveys)[1],1)


### specify the counts at those surveys
summvert$Method<-ifelse(summvert$Species %in% c("SOAL","TRAL","AYNA"),"SURV","COLO")
counts<- merge(summvert,surveys, by=c("Colony","DATE","Method"),all.x=T)
counts<- counts %>% filter(!Colony=="") %>%
  mutate(Breed_Stage=ifelse(COHORT %in% c("AON","INCU","AD"),"INCU","CHIC")) %>%
  select(VisitID,Species, Breed_Stage, COHORT, N, Colony, DATE, Method) 

head(counts)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 6: CREATE EXPORT OF NEST VISITS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

fwrite(surveys,"Gough_surveys_tobe_added.csv")
fwrite(counts,"Gough_counts_tobe_added.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 7: SUMMARY TO CHECK FOR DATA GAPS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

SUMMARY<- summvert %>% group_by(Species,Colony,Year,COHORT) %>%
  summarise(N=sum(N, na.rm=T)) %>%
  spread(COHORT,N)
SUMMARY



