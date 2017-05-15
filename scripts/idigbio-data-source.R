library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)


##########################################################
# This script will build two large dataframes from the collections 
# and recordsets created by 'identifying-collections.R'
# Inputs: fishy-recordsets.rdata & centrarchidae-recordsets.rdata
# Output: data-raw/big-ole-dataframe.rdata & data-raw/big-ole-dataframe1.rdata


## First we need to see where the fishy sets and cent sets overlap
load("data-raw/fishy-recordsets.rdata")
load("data-raw/centrarchidae-recordsets.rdata")

##These are our 'completeness' checks
## Are all of the ASIH codes present in both datasets and are all the recordset UUID's present in both sets?
all(all(bd$ASIHCode %in% gd$ASIHCode)&&all(bd$recordset_uuid %in% gd$idigbio.recordset))
##IF False (fixes KUIT Problem)
rr <- bd[!bd$ASIHCode %in% gd$ASIHCode,]
rr$idigbio.recordset <- rr$recordset_uuid
rr$name <- NULL
rr$recordset_uuid <- NULL
gd <- rbind.fill(gd,rr)


## We need to make sure we have all of the possible code/recorset combos possible
test <- gd
logDF <- data.frame()
for (i in 1:length(gd$idigbio.recordset)){
        cDF <- idig_top_records(rq=list(recordset=test$idigbio.recordset[i]),top_fields = "collectioncode")
        cDF <- data.frame(ASIHCode=test$ASIHCode[i],dwc.collectionCode=names(cDF$collectioncode),idigbio.recordset=test$idigbio.recordset[i],dwc.institutionCode=test$dwc.institutionCode[i])
        if(length(test[test$ASIHCode==test$ASIHCode[i],]$dwc.collectionCode)<length(cDF$dwc.collectionCode)){
                logDF <- rbind.fill(logDF,cDF)
        }
}
logDF <- logDF[!duplicated(logDF),]
jj <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/vocab/ASIH_Codes_UUID.csv",stringsAsFactors = F)
jj <- merge(logDF,jj,by.x = "ASIHCode")
jj <- jj[!duplicated(jj),]

inlcudes <- c("i","fish","ich","ic","fishes","ichthyology","larval collection","adult collection","egg collection","juvenile collection","skeleton collection")
jj <- jj[jj$dwc.collectionCode %in% inlcudes,]
jj <- jj[!jj$dwc.collectionCode %in% gd$dwc.collectionCode,]
jj$recordset_uuid <- NULL
gd <- rbind(gd,jj)

##Fix up some more of this KUIT monkey business
kuit <- gd[gd$ASIHCode=="KUIT",]$idigbio.recordset
kuColCode <- idig_top_records(rq=list(recordset=kuit),top_fields = "collectioncode")
kuInsCode <- idig_top_records(rq=list(recordset=kuit),top_fields = "institutioncode")
ku <- gd[gd$ASIHCode=="KU",]
ku$idigbio.recordset <- kuit
ku$dwc.collectionCode <- names(kuColCode$collectioncode)
ku$dwc.institutionCode <- names(kuInsCode$institutioncode)
gd <- rbind(gd,ku)
gd <- gd[!gd$ASIHCode=="KUIT",]

##Fix up the UMMZ monkey business
##this code will need to be deleted once we get this data from iDigBio
uMMz <- gd[gd$ASIHCode=="UMMZ",]
uMMz$idigbio.recordset <- "168d73e6-8e1a-4b69-9d8b-96ff2de773ee"
uMMz$dwc.collectionCode <- "fishes"
uMMz$dwc.institutionCode <- "ummz"
gd <- rbind(gd,uMMz)


##Let's build an idigbio dataframe from our centrarchid recordsets
## using the download system

# #quick count test
# for (i in 1:length(gd$dwc.institutionCode)){
#         rq <- list("institutioncode"=gd$dwc.institutionCode[i],"collectioncode"=gd$dwc.collectionCode[i],"recordset"=gd$idigbio.recordset[i],"data.dwc:basisOfRecord"="PreservedSpecimen")
#         f <- idig_count_records(rq)
#         print(f)
# }


gd$rq <- sapply(1:length(gd$idigbio.recordset),function(x) toJSON(list("collectioncode"=gd$dwc.collectionCode[x],"recordset"=gd$idigbio.recordset[x])))

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

folderVector <- list.dirs("data-raw/idb-download/centDL",recursive = F)
#fileVector <- list.files("data-raw/idb-download/centDL/",recursive = T)

for (i in 1:length(folderVector)){
        fileVector <- list.files(folderVector[i],recursive = T)
        for(ii in 1:length(fileVector)){
        unzip(paste0(folderVector[i],"/",fileVector[ii]),exdir = paste0(folderVector[i],"/",strtrim(strsplit(fileVector[ii], "/")[[1]][1],36)))
        print(paste0("unzipping folder #", folderVector[i]," & file # ",ii))
        }
}

##fileVector <- list.files("data-raw/idb-download/centDL/",recursive = T)
##fileVector <- strtrim(fileVector,36)
gigaCentDF <- data.frame()
for (i in 1:length(folderVector)){
        datafiles <- list.dirs(folderVector[i],recursive = F)
        for(ii in 1:length(datafiles)){
                tmpDF <- read.csv(paste0(datafiles[ii],"/occurrence.csv"),stringsAsFactors = F)
                gigaCentDF <- rbind.fill(gigaCentDF,tmpDF)
        }
}        


##Gotta add the ASIH code now
gigaCentDF <- merge(gigaCentDF,gd,by=c("idigbio.recordset","dwc.collectionCode","dwc.institutionCode"),all.x=TRUE)

save(gigaCentDF, file = 'data-raw/big-ole-dataframe1.rdata', compress = 'xz')






















#########################################################################################################
### All ye who pass this point are doomed to repeat their mistakes#######################################





## This loop uses the iDigBio download system to generate zip files for each fishy colleciton
for (i in 1:length(bd$recordset_uuid)) {
        rq <- list("recordset"= bd$recordset_uuid[i])
        gq <-toJSON(rq,auto_unbox = T)
        email <- "kevinlove@ufl.edu"
        createDownload <- paste0("https://api.idigbio.org/v2/download/?rq=",URLencode(gq),"&email=",email)
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
        download.file(getData,destfile = paste0("data-raw/idb-download/fishy-sets/",bd$recordset_uuid[i],".zip"))
        
}


## Create a vector of all downloaded fishy recordsets
## unzip them into new directories
fileVector <- list.files("data-raw/idb-download/fishy-sets/")        
for (i in 1:length(fileVector)){
        unzip(paste0("data-raw/idb-download/fishy-sets/",fileVector[i]),exdir =paste0("data-raw/idb-download/fishy-sets/",strtrim(fileVector[i],36)))
        print(paste0("unzipping file # ",i))
}

##Build a dataframe from the downloaded datasets
##This is a hack for now
fileVector <- strtrim(fileVector,36)
rsSets <- unique(gd$idigbio.recordset)
fileVector <- setdiff(fileVector,rsSets)
####################################################

gigaDF <- data.frame()
for (i in 1:length(fileVector)){
        tmpDF <- read.csv(paste0("data-raw/idb-download/fishy-sets/",fileVector[i],"/occurrence.csv"),stringsAsFactors = F)
        gigaDF <- rbind.fill(gigaDF,tmpDF)
}
 


## Gotta add the ASIH codes at some point
bd$name <- NULL
gigaDF <- merge(gigaDF,bd, by.x="idigbio.recordset",by.y = "recordset_uuid",all.x = T)

## Save the resulting dataframe
save(gigaDF, file = 'data-raw/big-ole-dataframe.rdata', compress = 'xz')


