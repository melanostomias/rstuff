library(plyr)
library(tidyr)
library(jsonlite)
library(dplyr)
library(plotly)

######################################################################
## This script is used to clean up the outputs of 'preps.R'
## and do more work on dwc.individualCount data
##
## Inputs: 'data-raw/raw-prepsDF.rdata' & 'data-raw/workingDF.rdata'
## Outputs: 'asih-by-specimens1.csv'

## Load the dataframe that we created with preps.R
load("~/Documents/temp/09MAY2017/rstuff/scripts/data-raw/raw-prepsDF.rdata")

## For now, we're gonna create a new object and obfuscate the variable names 
mydf <- gigaDF
names(mydf) <- c("V2","V1","V3")

## We need to give a value to NULL
## This includes strings of length zero and NA
mydf[is.na(mydf$V1),]$V1 <- "No value provided"
mydf[nchar(mydf$V1)==0,]$V1 <- "No value provided"

## We know that sometimes these data are concatenated with a semicolon, so
## we can split out on semicolon
mydf <- mydf %>% mutate(V1 = strsplit(as.character(V1), ";|\\|")) %>% unnest(V1)

## Clean up leading/trailing whitespace
mydf$V1 <- trimws(mydf$V1,"both")

## Create a dataframe of items that have a count of zero
noCount <- mydf[is.na(mydf$V3),]
## Create a variable that we will store our count data in
noCount$count <- noCount$V3

## Use regular expressions to clean up counts embedded in dwc.preparations 
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

## Aggregate the count for each record
countAgg <- aggregate(count ~ V2,data=noCount,sum)
## We can now give our variables better labels
names(countAgg) <- c("idigbio.uuid","bluegill.individualCount")

## Match our standardized counts back to our working dataframe
load("data-raw/workingDF.rdata")
##Gotta remove KUIT
## "0f53b3e3-c248-4026-a070-15c3fefdbbc0"
workingDF <- workingDF[!workingDF$recordset=="0f53b3e3-c248-4026-a070-15c3fefdbbc0",]

## Merge our standardized individual counts
workingDF <- merge(workingDF,countAgg,all.x = T)
workingDF[is.na(workingDF$bluegill.individualCount),]$bluegill.individualCount <- workingDF[is.na(workingDF$bluegill.individualCount),]$individualcount
## Look for outliers
any(is.na(workingDF$bluegill.individualCount))
outs <- workingDF[is.na(workingDF$bluegill.individualCount),]

## Create a dataframe of individual counts per ASIH code
bgCounts <- aggregate(bluegill.individualCount ~ ASIHCode,data=workingDF,sum)

## Order decreasing
bgCounts <- bgCounts[order(bgCounts$bluegill.individualCount,decreasing = T),]
## Save file to summary directory
write.csv(bgCounts,file = "../data/summary/data/asih-by-specimens1.csv",row.names = F)
