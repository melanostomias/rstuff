library(plyr)

## This script is used to build and clean our dataframe of fish records. It uses the 
## output of 'idigbio-data-source.R' and ouputs two dataframes we can use in 
## other parts of our analysis
##
## Inputs: 'data-raw/big-ole-dataframe1.rdata'
## Outputs: 'data-raw/workingDF.rdata' & 'data-raw/hugeDF.rdata'


## Load our input dataframe
load("data-raw/big-ole-dataframe1.rdata")

## Create a new object to work with
hugeDF <- gigaCentDF


## We will use three criteria to do some data validation
##
## 1. We will identify the collections that have the highest diversity of families 
famQuant <- plyr::count(plyr::count(hugeDF,c("dwc.family","ASIHCode"))[1:2],"ASIHCode")
names(famQuant) <- c("ASIHCode","FamilyCount")
qv <- quantile(famQuant$FamilyCount,probs = 80/100)
famQuant <- famQuant[famQuant$FamilyCount>qv,]

## 2. We will indentify the collections that have the most type material
typeQuant <- plyr::count(plyr::count(hugeDF,c("dwc.typeStatus","ASIHCode"))[1:2],"ASIHCode")
names(typeQuant) <- c("ASIHCode","TypeCount")
tq <- quantile(typeQuant$TypeCount,probs = 80/100)
typeQuant <- typeQuant[typeQuant$TypeCount>tq,]

## 3. We will identify collections that have the most published records
recQuant <- plyr::count(hugeDF, "ASIHCode")
qr <- quantile(recQuant$freq,probs = 80/100)
recQuant <- recQuant[recQuant$freq>qr,]

## Assemble these ASIH collections into a vector and
## collect all of the dwc.family names associated with them
## into a new vector
##
## todo: fix this really slow for loop
dictASIH <- c(famQuant$ASIHCode,typeQuant$ASIHCode,recQuant$ASIHCode)
dictASIH <- unique(dictASIH)

for (i in 1:length(dictASIH)){
        readDF <- hugeDF[hugeDF$ASIHCode==dictASIH[i],]$dwc.family
        if(i==1){fishDict <- readDF}else{fishDict <- c(fishDict,readDF)}
}

## We now have a dictionary of 'fish families' we can use 
## to filter our dataframe through
fishDict <- unique(fishDict)

## Found some non-fish families in the data as a result of issues with iDigBio's indexing 
## of the data and we can clean that up now
outFam <- c("scolopacidae","fringillidae","psittacidae","canidae","trochilidae","podicipedidae","campephagidae","gliridae","cheloniidae","crocodylidae","viperidae")
fishDict <- fishDict[!fishDict %in% outFam]

## We will now filter out all records that are not in our dictonary of fish names with some confidence that they 
## were misidentified or erroneous data

hugeDF <- hugeDF[hugeDF$dwc.family %in% fishDict,]

## We will now standardize our variable names
colnames(hugeDF)[which(names(hugeDF) == "dwc.institutionCode")] <- "institutioncode"
colnames(hugeDF)[which(names(hugeDF) == "dwc.collectionCode")] <- "collectioncode"
colnames(hugeDF)[which(names(hugeDF) == "dwc.catalogNumber")] <- "catalognumber"
colnames(hugeDF)[which(names(hugeDF) == "idigbio.recordset")] <- "recordset"
colnames(hugeDF)[which(names(hugeDF) == "dwc.individualCount")] <- "individualcount"
colnames(hugeDF)[which(names(hugeDF) == "dwc.basisOfRecord")] <- "basisOfRecord"



## Create a working dataframe that has a subset of variable to save some memory and
## storage space
rqCols <- c("institutioncode", "collectioncode", "catalognumber","recordset","individualcount","basisOfRecord","ASIHCode","collection_uuid","logo","collection_description","idigbio.uuid","rq") 
workingDF <- hugeDF[rqCols]

## Write our output files
save(workingDF,file='data-raw/workingDF.rdata',compress = 'xz')
save(hugeDF, file = 'data-raw/hugeDF.rdata', compress = 'xz')