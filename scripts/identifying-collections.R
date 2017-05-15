library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)

##########################################################
# This script creates two dataframes of fish collections. The 
# first approach is to look for fishy collections via their 
# metadata using the iDigBio recordset API enpoint, the other 
# performs a fishy specimen query and investigates the results 
# 
# Inputs: vocab/ASIH_Codes_UUID.csv
# Output: data-raw/fishy-recordsets.rdata & data-raw/centrarchidae-recordsets.rdata


## Build recordset DF using iDigBio Recordset API endpoint
## https://github.com/iDigBio/idigbio-search-api/issues/14

rsEP <- "http://search.idigbio.org/v2/search/recordsets?limit=2000"
rsDF <- fromJSON(rsEP)
rsDF <- rsDF$items
rsDF <- rsDF$indexTerms
rsDF <- flatten(rsDF, recursive = TRUE)
rsDF$contacts <- NULL
rsDF$recordids <- NULL

## Find the fish collections
## looking for "fish" or "ichthy" in the collection name first
rsDF <- rbind(rsDF[grep("fish",rsDF$name),],rsDF[grep("ichthy",rsDF$name),])
## simplyfy it for later cleaning
rsDF <- rsDF[c("uuid","name")]


##Merge with cleaned up data
##Let's also log any new recordsets 
bd <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/vocab/ASIH_Codes_UUID.csv",stringsAsFactors = F)
if (nrow(rsDF[!rsDF$uuid %in% bd$recordset_uuid,])>0){write.csv(rsDF[!rsDF$uuid %in% bd$recordset_uuid,],file = paste0("../log/new-recordsets-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),".csv"),row.names = FALSE)}
bd <- merge(bd,rsDF,by.x = "recordset_uuid",by.y = "uuid")

##Now is a good time to clean up our excluded collections
## See ticket #2
excludes <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/vocab/excludes.csv",stringsAsFactors = F)$ASIHCode
bd <- bd[!bd$ASIHCode %in% excludes,]
save(bd, file = 'data-raw/fishy-recordsets.rdata', compress = 'xz')






#Let's look at this from the other direction
## Build a list of collectioncode,institioncode,recordset triples from a fishy search
rq <-list("family"="centrarchidae")
zz <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
##iDigBio doesnt want us to query more than 100,00 results, so lets download them
gq <-toJSON(rq,auto_unbox = T)
email <- "kevinlove@ufl.edu"

createDownload <- paste0("https://api.idigbio.org/v2/download/?rq=",URLencode(gq),"&email=",email)

##Create the download
rqDL <- fromJSON(createDownload)
##Check the status
rqStatus <- fromJSON(rqDL$status_url)$task_status
##Loop until complete
for (i in 1:86400){
        if(!rqStatus=="SUCCESS"){
                print(paste0("waiting for success ",i))
                rqStatus <<- fromJSON(rqDL$status_url)$task_status
                Sys.sleep(1)
        }else{ getData <- fromJSON(rqDL$status_url)$download_url
        break}
}
##Download the data
dir.create("data-raw/idb-download",showWarnings = FALSE)
dlFile <- paste0("data-raw/idb-download/",substr(rqDL$status_url, 37,72),".zip")
download.file(getData,destfile = dlFile)

##Unzip and load
dir.create("data-raw/idb-download/centSearch")
unzip(dlFile,exdir ="data-raw/idb-download/centSearch")
zz <- read.csv("data-raw/idb-download/centSearch/occurrence.csv", stringsAsFactors = F)




## Subset by preserved specimens
## Not looking for fossil collections
zz <- zz[zz$dwc.basisOfRecord=="PreservedSpecimen"|zz$dwc.basisOfRecord=="preservedspecimen",]
zzCount <- plyr::count(zz,c("dwc.institutionCode","dwc.collectionCode","idigbio.recordset"))
zzCount <- zzCount[order(-zzCount$freq),]
## Cut out the recordsets we already know about and
## log the ones we need to work on
bb <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/vocab/ASIH_Codes_UUID.csv",stringsAsFactors = F)
if (nrow(zzCount[!zzCount$idigbio.recordset %in% bb$recordset_uuid,])>0){write.csv(zzCount[!zzCount$idigbio.recordset %in% bb$recordset_uuid,],file = paste0("../log/new-centrarchidae-recordsets-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),".csv"),row.names = FALSE)}
gd <- zzCount[zzCount$idigbio.recordset %in% bb$recordset_uuid,]
##Now is a good time to clean up our excluded collections
## See ticket #2
gd$freq <- NULL
gd <- merge(gd,bb,by.x = "idigbio.recordset",by.y = "recordset_uuid")
gd <- gd[!gd$ASIHCode %in% excludes,]
##save our centrarchid file
save(gd, file = 'data-raw/centrarchidae-recordsets.rdata', compress = 'xz')






#############################################################################
# ## Clean up collection codes that don't contain our friend centrarchidae
# ##
# centRaw <- data.frame()
# for (i in 1:length(gd$idigbio.recordset)){
#         centRQ <- list("collectioncode"=gd$dwc.collectionCode[i],"institutioncode"=gd$dwc.institutionCode[i],"recordset"=gd$idigbio.recordset[i],"data.dwc:family"="centrarchidae")
#         if(idig_count_records(centRQ)>0){
#                 centRaw <- rbind.fill(centExclude,gd[i,])
#         }
# }
# gd <- centRaw
# 
# gd$freq <- NULL
# gd <- gd[!duplicated(gd),]






