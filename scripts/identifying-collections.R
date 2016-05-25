library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)

## Build recordset DF using iDigBio Recordset API endpoint
## https://github.com/iDigBio/idigbio-search-api/issues/14

rsEP <- "http://search.idigbio.org/v2/search/recordsets?limit=1000"
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
bd <- read.csv("../vocab/ASIH_Codes_UUID.csv")
if (nrow(rsDF[!rsDF$uuid %in% bd$uuid,])>0){write.csv(rsDF[!rsDF$uuid %in% bd$uuid,],file = paste0("../log/new-recordsets-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),".csv"),row.names = FALSE)}
bd <- merge(bd,rsDF)

##Now is a good time to clean up our excluded collections
## See ticket #2
excludes <- c("MNHN","NRM","INPA","ROM")
bd <- bd[!bd$ASIHCode %in% excludes,]
save(bd, file = 'data-raw/fishy-recordsets.rdata', compress = 'xz')






#Let's look at this from the other direction
## Build a list of collectioncode,institioncode,recordset triples from a fishy search
rq <-list("family"="centrarchidae")
zz <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
## Subset by presereved specimens
## Not looking for fossil collections
zz <- zz[zz$`data.dwc:basisOfRecord`=="PreservedSpecimen"|zz$`data.dwc:basisOfRecord`=="preservedspecimen",]
zzCount <- plyr::count(zz,c("institutioncode","collectioncode","recordset"))
zzCount <- zzCount[order(-zzCount$freq),]
## Cut out the recordsets we already know about and
## log the ones we need to work on
## gd <- zzCount[!zzCount$recordset %in% bd$uuid,]
## gd <- gd[1:3]
## write.csv(gd,row.names = FALSE,file = "../vocab/ASIH_Codes_UUID_Centrarchidae.csv")
bb <- read.csv("../vocab/ASIH_Codes_UUID.csv")
if (nrow(zzCount[!zzCount$recordset %in% bb$uuid,])>0){write.csv(zzCount[!zzCount$recordset %in% bb$uuid,],file = paste0("../log/new-centrarchidae-recordsets-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),".csv"),row.names = FALSE)}
##write.csv(gd,file="collections-centrarchidae-idigbio.csv",row.names = FALSE)
##cleaned up collections data
gd <- zzCount[zzCount$recordset %in% bb$uuid,]
##Now is a good time to clean up our excluded collections
## See ticket #2
gd <- merge(gd,bb,by.x = "recordset",by.y = "uuid")
gd <- gd[!gd$ASIHCode %in% excludes,]
gd$freq <- NULL
gd <- gd[!duplicated(gd),]
save(gd, file = 'data-raw/centrarchidae-recordsets.rdata', compress = 'xz')

















##########################################################
# We've got some data fetching tasks here, so we 
# may move this to another script file
# 
        
        
## Let's get some iDigBio Data!
load("data-raw/fishy-recordsets.rdata")
rq <- list("recordset"= list(bd$uuid))
## Query Dataframe
dd <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
## load("../test1/records.rdata")        
names(dd) <- c("institutioncode","collectioncode","catalognumber","preps","recordset","individualcount","basisOfRecord")
## Gotta add the ASIH codes now
dd <- merge(dd,bd,by.x = "recordset",by.y = "uuid",all.x=TRUE)
dd$name <- NULL






##Let's build an idigbio dataframe
ff <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(gd$institutioncode)){
        rq <- list("institutioncode"=gd$institutioncode[i],"collectioncode"=gd$collectioncode[i],"recordset"=gd$recordset[i],"data.dwc:basisOfRecord"="PreservedSpecimen")
        ll <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
        names(ll) <- c("institutioncode","collectioncode","catalognumber","preps","recordset","individualcount","basisOfRecord")
        ff <- rbind(ff,ll)
}

##Gotta add the ASIH code now
ff <- merge(ff,gd,by.x = c("recordset","institutioncode","collectioncode"),by.y = c("recordset","institutioncode","collectioncode"),all.x=TRUE)


##Merge these two datasets together
## and clean up duplicates
hugeDF <- rbind.fill(dd,ff)
hugeDF <- hugeDF[!duplicated(hugeDF),]
save(hugeDF, file = 'lots-o-records.rdata', compress = 'xz')
