######### ########################################
### DATA RESCUE OF GOUGH MONITORING DATA #########
##################################################

## written by steffen.oppel@rspb.org.uk
## support from Jaimie Cleeland (Gough Team 2017/2018)

## main purpose is to rescue data inefficiently stored in hundreds of awful Excel files
## conversion into flat tables for import into database
## simplification and standardisation of terminology


library(tidyverse)
library(data.table)
library(lubridate)
library(xlsx)

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