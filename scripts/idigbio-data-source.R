library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)


##########################################################
# This file will build a large dataframe from the collections 
# and recordsets created by 'identifying-collections.R'
# Inputs: fishy-recordsets.rdata & centrarchidae-recordsets.rdata
# Output: lots-o-records.rdata
        
        
## Let's get some iDigBio Data from our known recordsets
load("data-raw/fishy-recordsets.rdata")
rq <- list("recordset"= list(bd$uuid))
## Query Dataframe
dd <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
names(dd) <- c("institutioncode","collectioncode","catalognumber","preps","recordset","individualcount","basisOfRecord")




##Let's build an idigbio dataframe from our centrarchid recordsets
load("data-raw/centrarchidae-recordsets.rdata")
ff <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(gd$institutioncode)){
        rq <- list("institutioncode"=gd$institutioncode[i],"collectioncode"=gd$collectioncode[i],"recordset"=gd$recordset[i],"data.dwc:basisOfRecord"="PreservedSpecimen")
        ll <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset","individualcount","data.dwc:basisOfRecord"),max_items = idig_count_records(rq))
        names(ll) <- c("institutioncode","collectioncode","catalognumber","preps","recordset","individualcount","basisOfRecord")
        ff <- rbind(ff,ll)
}



##Merge these two datasets together and save the results
hugeDF <- rbind.fill(dd,ff)
hugeDF <- unique(hugeDF)
save(hugeDF, file = 'data-raw/lots-o-records.rdata', compress = 'xz')
