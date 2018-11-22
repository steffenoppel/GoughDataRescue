##################################################
### DATA RESCUE OF GOUGH MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk
## branched from reformat_colony_counts on 22 Nov 2018
## modified to use nest data to populate counts in database for AYNA, TRAL and SOAL


library(tidyverse)
library(data.table)
library(lubridate)
library(tibble)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 1: IMPORT TABLE OF COUNTS AND NEST DATA FROM ACCESS DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## only need to re-run after changes to database
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue\\RODBC_count_import.r")), wait = FALSE, invisible = FALSE)

#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)
load("GOUGH_seabird_data.RData")
head(countDB)
head(survDB)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: CHECK FOR DATA GAPS IN COUNT DATA
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

### SUMMARISE AVAILABLE COUNT DATA
SUMMARY<- survDB %>% left_join(countDB,by='VisitID') %>% 
  mutate(Year=year(Date)) %>%
  group_by(Species,Colony,Year,Date, Breed_Stage,Cohort) %>%
  summarise(N=sum(Number, na.rm=T)) %>%
  filter(Breed_Stage %in% c("CHIC","INCU")) %>%
  spread(Breed_Stage,N)
SUMMARY


### SELECT SPECIES AND YEARS THAT ARE IMPORTANT
SUMMARY %>% filter(Species=="AYNA") %>% filter(Year>2016)


### THERE ARE NO COUNT DATA FOR AYNA IN AREA 1 IN 2017 and 2018



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: EXTRACT NUMBERS OF INCUBATING BIRDS AND CHICKS IN AREA 1 FROM NEST MONITORING DATA
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
head(nestsDB)
head(visDB)

AYNA_INC<- nestsDB %>% filter(Species=="AYNA", Colony=="Area 1", Year>2016) %>%
  mutate(Count=1) %>%
  group_by(Species,Colony,Year) %>%
  summarise(Number=sum(Count), Date=max(DateFound)) %>%
  mutate(Breed_Stage="INCU",Cohort="INCU", Method="COLO") %>%
  mutate(VisitID=(1:n())+max(survDB$VisitID))


AYNA_CHIC<- nestsDB %>% filter(Species=="AYNA", Colony=="Area 1", Year>2016, SUCCESS==1) %>%
  mutate(Count=1) %>%
  group_by(Species,Colony,Year) %>%
  summarise(Number=sum(Count), Date=mean(DateLastAlive)) %>%
  mutate(Breed_Stage="CHIC",Cohort="CHIC", Method="COLO") %>%
  mutate(VisitID=(1:n())+max(AYNA_INC$VisitID))





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 4: CREATE EXPORT OF NEST VISITS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

fwrite(rbind(AYNA_INC,AYNA_CHIC),"Import_AYNA_col_counts_Area1.csv")




