##################################################
### DATA RESCUE OF GOUGH MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk
## support from Jaimie Cleeland (Gough Team 2017/2018)

## main purpose is to rescue data that are inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology

## first created 8 Aug 2018


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
visits <- read_excel("BREEDING_DATABASE.xlsx", sheet="NEST_VISITS")
colony <- read_excel("BREEDING_DATABASE.xlsx", sheet="COLONY_SUMMARIES")

head(nests)

head(colony)



#### FORMAT NEST VISITS TO MATCH WITH DATABASE ###
head(visits)
str(visits)

### modify species abbreviations
species<-unique(visits$Species)
lkSpec<-data.frame(abbr=species, CODE=c("ATPE","BBPR","GOBU","SOAL","?","SOPE","SOPE","TRAL","UNK","AYNA","NRPE","GRSH","?"))
visits<- visits %>% mutate(Species=lkSpec$CODE[match(Species,lkSpec$abbr)])


### generate unique VisitID and NestID
visits<- visits %>% mutate(VisitID=seq(10001,dim(visits)[1]+10000,1)) %>%
  mutate(NestID=paste(Species,Year,Quadrat,ID_nest_burrow, sep="_"))


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
  mutate(STAGE=ifelse(grepl("gone",abbr,perl=T,ignore.case = T)==T,"FLED",STAGE))



### CREATE EXPORT OF NEST VISITS FOR DATABASE
export<- visits %>% mutate(Time="12:00") %>%
  mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
  mutate(Status=ifelse(STATUS==0,"Failed",ifelse(STAGE=="FLED","Fledged","Alive"))) %>%
  mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
  mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
  mutate(Attendance=NA)%>% ### this is omitted for batch imports
  mutate(FailureCause=Comments)%>% 
  mutate(NOTES=Notes)%>% 
  select(VisitID, NestID, Date, Time, Stage, Status, Content, Attendance, FailureCause,NOTES)

head(export)
fwrite(export,"Gough_nestVisits_export.csv")




### CREATE MATCHING INVENTORY OF NESTS AND FINAL OUTCOME

nests <- visits %>% mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
  mutate(Status=ifelse(STATUS==0,"Failed",ifelse(STAGE=="FLED","Fledged","Alive"))) %>%
  mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
  mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
  mutate(Colony=ifelse((Colony %in% c(NA,"")),Transect,Colony)) %>%         ## fill in colony name from transect
  mutate(Site=ifelse((Site %in% c(NA,"")),Quadrat,Site)) %>%         ## fill in site name from quadrat
  arrange(Date) %>%  ## arrange in chronological order so we can extract summary infor for first and last nest visits
  group_by(NestID, Species, Year, Colony, Site,Latitude, Longitude) %>%
  summarise(DateFound=min(Date),StageFound=first(STAGE), DateLastChecked=max(Date), SUCCESS=last(STATUS))

## update the LastAlive date

DateLastAlive <- visits %>% mutate(Stage=lkStages$STAGE[match(STAGE,lkStages$abbr)]) %>%
  mutate(Status=ifelse(STATUS==0,"Failed",ifelse(STAGE=="FLED","Fledged","Alive"))) %>%
  mutate(Content=ifelse(CONTENT=="1",1,ifelse(CONTENT=="2",2,ifelse(CONTENT=="3",3,ifelse(CONTENT=="AIA",NA,0))))) %>%
  mutate(Status=ifelse(CONTENT %in% c("Dead Chick","Broken Egg","x"),"Failed",Status)) %>%
  filter(Status=="Alive") %>%   ## select only the alive nests
  arrange(Date) %>%  ## arrange in chronological order so we can extract summary infor for first and last nest visits
  group_by(NestID, Species, Year, Colony, Site) %>%
  summarise(DateLastAlive=max(Date))


nests <- merge(nests,DateLastAlive, by=c('NestID', 'Species', 'Year', 'Colony', 'Site'), all.x=T)

 

### Troubleshoot the large number of missing dates

visits %>% filter(is.na(Date))


















##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PART 2: FORMAT FROM RAW EXCEL FILES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##






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