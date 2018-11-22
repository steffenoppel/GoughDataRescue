##################################################
### DATA RESCUE OF SOUTHERN GIANT PETREL MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk
## file provided by Fabrice Lebouard

library(tidyverse)
library(data.table)
library(lubridate)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 1: FORMAT FROM CONSOLIDATED EXCEL FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


####### READ IN MANUALLY COPIED DATABASE OF BREEDING DATA #####
### THIS REQUIRES MANUAL CONVERSION TO CSV AS THE DIFFERENT DATE FORMATS IN EXCEL SCREW EVERYTHING UP

try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\Gough\\DATA"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA\\GoughDataRescue"), silent=T)


## apply date formatting conversion to the whole data set
visits <- fread("SGPnests.csv")
dim(visits)
head(visits)
str(visits)

### modify species abbreviations
visits$Species<-"SGPE"
visits$Year<-2018

### generate unique VisitID and NestID
visits<- visits %>%
  mutate(Colony="Low Hump") %>%
  mutate(Site=area) %>%
  mutate(GPSpoint=nest) %>%
  mutate(nest=substr(nest,4,6)) %>%
  mutate(Nest_label=paste(Species,Year,Colony,nest, sep="_"))


### CREATE MATCHING INVENTORY OF NESTS AND FINAL OUTCOME

nests <- visits %>% select(Nest_label,Species,Year,Colony,Site,GPSpoint,lat,lon) %>%
    mutate(DateFound="30/08/2018") %>%
    mutate(StageFound="INCU")
                             
fwrite(nests,"SGP_nests_export.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: FORMAT VISITS FOR DATABASE 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


export <- visits %>% select(Nest_label,egg,`30.08.18_contents`) %>%
  mutate(Date="30/08/2018") %>%
  mutate(Stage="INCU")%>%
  mutate(Status="Alive") %>%
  mutate(Content=egg) %>%
  mutate(Attendance="INCU") %>%
  mutate(NestID="")

fwrite(export,"SGP_visits_export.csv")





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 3: CREATE EXPORT OF NEST ATTENDANTS FOR DATABASE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

export <- visits %>% select(Nest_label,p1_metal,p1_colour,p1_sex) %>%
  mutate(Cohort="INCU")%>%
  mutate(Sex=p1_sex) %>%
  mutate(RECAPTURE=1) %>%
  mutate(MetalRing=p1_metal) %>%
  mutate(colour="yellow") %>%
  mutate(DarvicCode=p1_colour) %>%
  mutate(VisitID="")

fwrite(export,"SGP_attendance_export.csv")




