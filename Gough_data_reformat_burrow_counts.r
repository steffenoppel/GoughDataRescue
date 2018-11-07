########################################################################
### DATA RESCUE OF GOUGH MONITORING DATA - POP COUNT DATABASE #########
########################################################################

## written by steffen.oppel@rspb.org.uk

## main purpose is to rescue data that are inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology

## first created 7 Nov 2018
## import burrow count surveys of ATPE, GRSH and others
## data were copied by Matthew Gardner in Jan 2018 based on ATPE files only


library(tidyverse)
library(data.table)
library(lubridate)
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
summaries <- read_excel("POP_COUNT_DATABASE.xlsx", sheet="BURROW_SURVEYS")
dim(summaries)
head(summaries)


### FILL IN BLANK SPECIES ROWS (mostly unoccupied ATPE burrows)
nospecs<-summaries %>% filter(is.na(Species)) %>% select(Species,Year,Colony,Transect,Filename)
unique(nospecs$Filename) ### check and confirm that all blanks are ATPE

### modify species abbreviations
species<-unique(summaries$Species)
lkSpec<-data.frame(abbr=species, CODE=c("SOPE","ATPE","UNK","ATPE","GRSH","SOPE","BBPR","GWPE","BBPR","SOPE","SOPE"))
summaries<- summaries %>% mutate(Species=lkSpec$CODE[match(Species,lkSpec$abbr)]) %>%
  filter(!Species=="UNK") ##remove worthless info of unknown species


### gather into shorter format
summvert<- summaries %>% mutate(count=1) %>%
  mutate(COHORT=ifelse(OCCUPANCY==1,"OCCU","UNOC")) %>%
  select(Species, Year,Transect,Quadrat,Date, COHORT,count,STAGE, Method,Comments)

  

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: ENSURE DATA INTEGRITY WITH DATA BASE - LOCATIONS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
usedlocs<-unique(summvert$Transect)

### UPDATE LOCATIONS IN SURVEYS
summvert<-summvert %>%
  mutate(Transect=ifelse(grepl("blechnum",Transect,perl=T,ignore.case = T)==T,"Blechnum Bridge",Transect)) %>%
  mutate(Transect=ifelse(grepl("prion",Transect,perl=T,ignore.case = T)==T,"Prion Cave",Transect)) %>%
  mutate(Transect=ifelse(grepl("3",Transect,perl=T,ignore.case = T)==T,"Prion Cave",Transect)) %>%
  mutate(Transect=ifelse(grepl("2",Transect,perl=T,ignore.case = T)==T,"Blechnum Bridge",Transect)) %>%
  mutate(Transect=ifelse(grepl("1",Transect,perl=T,ignore.case = T)==T,"Helipad",Transect))%>%
  mutate(Transect=ifelse(is.na(Transect),"Helipad",Transect))
  
  
### FILL IN BLANK SPECIES ROWS (mostly unoccupied ATPE burrows)
nolocs<-summaries %>% filter(is.na(Transect)) %>% select(Species,Year,Site,Filename)
unique(nolocs$Filename) ### need to check which of the ATPE transects was surveyed in 2006
### checked on 7 Nov 2018 and in 2006 only Helipad transect was counted



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 4: INSERT MISSING DATES BY TAKING AVERAGE OF PREVIOUS YEARS 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
str(summvert)
summvert %>% filter(is.na(Date))

### summarise count dates for known data
knowndates<- summvert %>% select(Species,STAGE, Year,Date) %>%
  mutate(day=day(Date),month=month(Date)) %>%
  group_by(Species,STAGE) %>%
  summarise(mean_month=round(mean(month,na.rm=T),0),mean_day=round(mean(day,na.rm=T),0))

### GIVEN THESE ARE ALL ATPE TRANSECT DATA WE MAKE UP A MEAN VALUE FOR CHIC AND INCU SURVEYS
summvert <- summvert %>% mutate(Date=if_else(is.na(Date),
                                            if_else(STAGE=="INCU",ymd(paste(Year,"08-05",sep="-")),ymd(paste(Year,"09-20", sep="-"))),
                                            as.Date(Date)))
summvert %>% filter(is.na(Date))
dim(summvert)
dim(summaries)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 6: SPLIT THE TABLE INTO THE RIGHT FORMAT - SURVEYS AND COUNTS 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### specify the surveys
surveys<- summvert %>% mutate(Method="BURR") %>%
  group_by(Transect,Quadrat, Date,Method) %>%
  summarise(VisitID=sum(count))
dim(surveys)
head(surveys)

### create unique VisitID number
startID<-max(survDB$VisitID)+1
surveys$VisitID<-seq(startID,max(survDB$VisitID)+dim(surveys)[1],1)


### specify the counts at those surveys
## fixed on 6 Nov because 'merge' created duplicate entries

counts <- summvert %>% mutate(Method="BURR") %>%
  left_join(surveys,by=c("Transect","Quadrat","Date","Method")) %>%
  group_by(VisitID,Species, STAGE, COHORT) %>%
  summarise(Number=sum(count)) %>%
  select(VisitID,Species, STAGE, COHORT, Number) 

head(counts)
dim(counts)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 7: CREATE EXPORT OF NEST VISITS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

fwrite(surveys,"Gough_burrow_surveys_tobe_added_7Nov2018.csv")
fwrite(counts,"Gough_burrow_counts_tobe_added_7Nov2018.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 8: SUMMARY TO CHECK FOR DATA GAPS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### by colony - this is almost impossible to look through
SUMMARY<- summvert %>% group_by(Species,Transect,Year,COHORT) %>%
  summarise(N=sum(count, na.rm=T)) %>%
  spread(COHORT,N)
SUMMARY

