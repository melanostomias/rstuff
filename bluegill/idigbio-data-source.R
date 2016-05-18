## New hybrid data approach.... still needs a name 
library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)

## Build recordset DF
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
        bd <- read.csv("../vocab/ASIH_Codes_UUID.csv")
        bd <- merge(bd,rsDF)
        ##Now is a good time to clean up our excluded collections
        ## See ticket #2
        excludes <- c("MNHN","NRM","INPA","ROM")
        bd <- bd[!bd$ASIHCode %in% excludes,]
        
        
## Let's get some iDigBio Data!
rq <- list("recordset"= list(bd$uuid))
## Query Dataframe
dd <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
## load("../test1/records.rdata")        
names(dd) <- c("institutioncode","collectioncode","catalognumber","preps","recordset","individualcount","basisOfRecord")



#Let's look at this from the other direction
## Build a list of collectioncode,institioncode,recordset triples from a fishy search
rq <-list("family"="centrarchidae")
zz <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
zzCount <- plyr::count(zz,c("institutioncode","collectioncode","recordset"))
zzCount <- zzCount[order(-zzCount$freq),]
## Cut out the recordsets we already know about
gd <- zzCount[!zzCount$recordset %in% bd$uuid,]
##write.csv(gd,file="collections-centrarchidae-idigbio.csv",row.names = FALSE)
##cleaned up collections data
gd <- read.csv("collections-centrarchidae-idigbio-CLEANED.csv")
##Now is a good time to clean up our excluded collections
## See ticket #2
gd <- merge(gd,read.csv("cleaned-recordsets.csv"),by.x = "recordset",by.y = "uuid")
gd <- gd[!gd$ASIHCode %in% excludes,]
##Let's build an idigbio dataframe
ff <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(gd$institutioncode)){
        rq <- list("institutioncode"=gd$institutioncode[i],"collectioncode"=gd$collectioncode[i],"recordset"=gd$recordset[i],"data.dwc:basisOfRecord"="PreservedSpecimen")
        ll <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
        names(ll) <- c("institutioncode","collectioncode","catalognumber","preps","recordset","individualcount","basisOfRecord")
        ff <- rbind(ff,ll)
}


##Merge these two datasets together!!!
hugeDF <- rbind.fill(dd,ff)
save(hugeDF, file = 'lots-o-records.rdata', compress = 'xz')
