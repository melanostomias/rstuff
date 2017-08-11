library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)


##########################################################
# This script will build a large dataframe of specimen records
# from the collections and recordsets created by 'identifying-collections.R'
#
# 
# Inputs: fishy-recordsets.rdata & centrarchidae-recordsets.rdata
# Output: data-raw/big-ole-dataframe1.rdata


## First we need to see where the fishy sets and cent sets overlap
load("data-raw/fishy-recordsets.rdata")
load("data-raw/centrarchidae-recordsets.rdata")

## These are our 'completeness' checks:
## Are all of the ASIH codes present in both datasets? 
## Are all the recordset UUID's present in both sets?
all(all(bd$ASIHCode %in% gd$ASIHCode)&&all(bd$recordset_uuid %in% gd$idigbio.recordset))

## This should have returned false, as we know that KUIT is not present
## Let's fix the KUIT problem now
rr <- bd[!bd$ASIHCode %in% gd$ASIHCode,]
rr$idigbio.recordset <- rr$recordset_uuid
rr$name <- NULL
rr$recordset_uuid <- NULL
gd <- rbind.fill(gd,rr)


## We need to make sure we have all of the possible 
## collection code/recorset combos possible. We can
## use the information we have and the ridigbio package
## to investigate this further
test <- gd
logDF <- data.frame()

## For every known recordset, build a dataframe of 
## collection codes present in the data 
for (i in 1:length(gd$idigbio.recordset)){
        cDF <- idig_top_records(rq=list(recordset=test$idigbio.recordset[i]),top_fields = "collectioncode")
        cDF <- data.frame(ASIHCode=test$ASIHCode[i],dwc.collectionCode=names(cDF$collectioncode),idigbio.recordset=test$idigbio.recordset[i],dwc.institutionCode=test$dwc.institutionCode[i])
        if(length(test[test$ASIHCode==test$ASIHCode[i],]$dwc.collectionCode)<length(cDF$dwc.collectionCode)){
                logDF <- rbind.fill(logDF,cDF)
        }
}
logDF <- logDF[!duplicated(logDF),]
## Apply our known ASIH codes to these recordsets
jj <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/vocab/ASIH_Codes_UUID.csv",stringsAsFactors = F)
jj <- merge(logDF,jj,by.x = "ASIHCode")
## Remove the duplicates
jj <- jj[!duplicated(jj),]
## Write this dataframe out so we can examine it and build our include list
write.csv(jj,file = paste0("../log/all-collectionCode-recordsets-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),".csv"),row.names = FALSE)

## Here we have a known list of collection codes to icludes. This list was 
## created by verifying the recordset/collection code contains fish from a US collection
## todo: Make this list available as a CSV to read-in
inlcudes <- c("i","fish","ich","ic","fishes","ichthyology","larval collection","adult collection","egg collection","juvenile collection","skeleton collection")
jj <- jj[jj$dwc.collectionCode %in% inlcudes,]
jj <- jj[!jj$dwc.collectionCode %in% gd$dwc.collectionCode,]
jj$recordset_uuid <- NULL
gd <- rbind(gd,jj)


## Fix up some more of this KUIT monkey business
## We need to find out what the collection code and instution codes are in the data
## and make sure they are in our dataframe

kuit <- gd[gd$ASIHCode=="KUIT",]$idigbio.recordset
kuColCode <- idig_top_records(rq=list(recordset=kuit),top_fields = "collectioncode")
kuInsCode <- idig_top_records(rq=list(recordset=kuit),top_fields = "institutioncode")
ku <- gd[gd$ASIHCode=="KU",]
ku$idigbio.recordset <- kuit
ku$dwc.collectionCode <- names(kuColCode$collectioncode)
ku$dwc.institutionCode <- names(kuInsCode$institutioncode)
gd <- rbind(gd,ku)
gd <- gd[!gd$ASIHCode=="KUIT",]

## Fix up the UMMZ monkey business
## This code will need to be deprecated once we get this data from the
## iDigBio API
uMMz <- gd[gd$ASIHCode=="UMMZ",]
uMMz$idigbio.recordset <- "168d73e6-8e1a-4b69-9d8b-96ff2de773ee"
uMMz$dwc.collectionCode <- "fishes"
uMMz$dwc.institutionCode <- "ummz"
gd <- rbind(gd,uMMz)


## Let's build an idigbio dataframe from our centrarchid recordsets
## using the iDigBio download system. We can use our dataframe to specify
## the data queries we are interested in.

## Build the record query in the format the API expects
gd$rq <- sapply(1:length(gd$idigbio.recordset),function(x) toJSON(list("collectioncode"=gd$dwc.collectionCode[x],"recordset"=gd$idigbio.recordset[x])))
## Use the download system to fetch the files we are interested in and 
## save them to a directory for each ASIH code
for (i in 1:length(gd$dwc.institutionCode)) {
        rq <- gd$rq[i]
        email <- "kevinlove@ufl.edu"
        createDownload <- paste0("https://api.idigbio.org/v2/download/?rq=",URLencode(rq),"&email=",email)
        ##Create the download
        rqDL <- fromJSON(createDownload)
        ##Check the status
        rqStatus <- fromJSON(rqDL$status_url)$task_status
        ##Loop until complete (24 hours sounds nice)
        for (v in 1:86400){
                if(!rqStatus=="SUCCESS"){
                        print(paste0("waiting for success ",v))
                        rqStatus <<- fromJSON(rqDL$status_url)$task_status
                        Sys.sleep(1)
                }else{ getData <- fromJSON(rqDL$status_url)$download_url
                break}
        }
        ##Download the data
        ## Make a directory for ASIH code
        dir.create(paste0("data-raw/idb-download/centDL/",gd$ASIHCode[i]),showWarnings = FALSE)
        ## Create file name
        dlFile <- paste0("data-raw/idb-download/centDL/",gd$ASIHCode[i],"/",substr(rqDL$status_url, 37,72),".zip")
        download.file(getData,destfile = dlFile)
        
}

## Build a vector of the ASIH codes we downloaded data for 
folderVector <- list.dirs("data-raw/idb-download/centDL",recursive = F)
## Unzip the files in each ASIH directory
for (i in 1:length(folderVector)){
        fileVector <- list.files(folderVector[i],recursive = T)
        for(ii in 1:length(fileVector)){
        unzip(paste0(folderVector[i],"/",fileVector[ii]),exdir = paste0(folderVector[i],"/",strtrim(strsplit(fileVector[ii], "/")[[1]][1],36)))
        print(paste0("unzipping folder #", folderVector[i]," & file # ",ii))
        }
}

## Now let's loop through each directory and append the iDigBio indexed
## occurence data found in each ASIH directory
gigaCentDF <- data.frame()
for (i in 1:length(folderVector)){
        datafiles <- list.dirs(folderVector[i],recursive = F)
        for(ii in 1:length(datafiles)){
                tmpDF <- read.csv(paste0(datafiles[ii],"/occurrence.csv"),stringsAsFactors = F)
                gigaCentDF <- rbind.fill(gigaCentDF,tmpDF)
        }
}        

## We can now add the ASIH code to each known collection record
gigaCentDF <- merge(gigaCentDF,gd,by=c("idigbio.recordset","dwc.collectionCode","dwc.institutionCode"),all.x=TRUE)

## Save this large dataframe to a file
save(gigaCentDF, file = 'data-raw/big-ole-dataframe1.rdata', compress = 'xz')