##Load Libraries
library(plyr)
library(tidyr)
library(jsonlite)
library(dplyr)
library(plotly)
load("data-raw/raw-prepsDF.rdata")
mydf <- gigaDF
names(mydf) <- c("V2","V1","V3")

## Give value to NULL right now
mydf[is.na(mydf$V1),]$V1 <- "No value provided"
mydf[nchar(mydf$V1)==0,]$V1 <- "No value provided"

##split out on semicolon
mydf <- mydf %>% mutate(V1 = strsplit(as.character(V1), ";|\\|")) %>% unnest(V1)

## clean up leading/training whitespace
mydf$V1 <- trimws(mydf$V1,"both")

noCount <- mydf[is.na(mydf$V3),]
noCount$count <- noCount$V3
noCount$V1 <- gsub("[0-9]*%","",noCount$V1)
##Match the '- 999' example
noCount[grepl("(- )[0-9][0-9]*|(-)[0-9][0-9]*",noCount$V1),]$count <- regmatches(noCount$V1,regexpr("(- )[0-9][0-9]*|(-)[0-9][0-9]*",noCount$V1))
##Match the '999 in' example
noCount[grepl("(?<=- )([0-9][0-9]*)|(?<=- -)([0-9][0-9]*)",noCount$V1,perl = T),]$count <- regmatches(noCount$V1,regexpr("(?<=- )([0-9][0-9]*)|(?<=- -)([0-9][0-9]*)",noCount$V1,perl = T))
noCount[grepl("[0-9]*[0-9](?= in)",noCount$V1,perl = T),]$count <- regmatches(noCount$V1,regexpr("[0-9]*[0-9](?= in)",noCount$V1,perl = T))
##Fix up the missing values
noCount[is.na(noCount$count),]$count <- 0
##Fix up vector class
noCount$count <- as.numeric(noCount$count)
##Fix up negative numbers
noCount[noCount$count<0,]$count <- 0
countAgg <- aggregate(count ~ V2,data=noCount,sum)
names(countAgg) <- c("idigbio.uuid","bluegill.individualCount")

## Match everything back
load("data-raw/workingDF.rdata")

##Smash on our individual counts
workingDF <- merge(workingDF,countAgg,all.x = T)
workingDF[is.na(workingDF$bluegill.individualCount),]$bluegill.individualCount <- workingDF[is.na(workingDF$bluegill.individualCount),]$individualcount
##Look for outliers
any(is.na(workingDF$bluegill.individualCount))
outs <- workingDF[is.na(workingDF$bluegill.individualCount),]

##Lets see what we got!!!!!
bgCounts <- aggregate(bluegill.individualCount ~ ASIHCode,data=workingDF,sum)

bgCounts <- bgCounts[order(bgCounts$bluegill.individualCount,decreasing = T),]
##Save to summary directory
write.csv(bgCounts,file = "../data/summary/data/asih-by-specimens1.csv",row.names = F)