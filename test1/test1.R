## I want to see if there is a difference in specimen distribution between iDigBio recordsets 
## and the concept of a specimen collection.

library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(tidyjson)
library(dplyr)
library(progress)

## Some references:
## iDigBio recordsets API Endpoint- http://search.idigbio.org/v2/search/recordsets
## iDigBio recordset fields enpoint- http://search.idigbio.org/v2/meta/fields/recordsets
## search parameter = rsq


## Let's get some data

rsEP <- "http://search.idigbio.org/v2/search/recordsets?limit=1000"
rsDF <- fromJSON(rsEP)
rsDF <- rsDF$items
rsDF <- rsDF$indexTerms
rsDF <- flatten(rsDF, recursive = TRUE)
rsDF$contacts <- NULL
rsDF$recordids <- NULL
write.csv(rsDF,file = "idb-recordsets-RAW.csv",row.names = FALSE)

## Find the fish collections
## looking for "fish" or "ichthy" in the collection name first

rsDF <- rbind(rsDF[grep("fish",rsDF$name),],rsDF[grep("ichthy",rsDF$name),])
write.csv(rsDF, file = "idb-fish-MAYBE.csv",row.names = FALSE)


##Let's make a directory for each recordset
## Use the ASIH codes in the vocab

bd <- read.csv("../vocab/ASIH_Codes_UUID.csv")
for (i in 1:length(bd$uuid)) {
        dir.create(as.character(bd$ASIHCode[i]))
}

## Let's work on summary by family
rq <- list("recordset"= list(bd$uuid))
## Query Dataframe
dd <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations","recordset"),max_items = idig_count_records(rq))
## This dataframe is pretty big, let's write it to CSV to save some time later
write.csv(dd, row.names = FALSE,file=paste("raw-recordset-data-",Sys.Date(),".csv",sep=""))
##Let's fix the names of the columns to save work later
names(dd) <- c("institutioncode","collectioncode","catalognumber","preps","recordset")
## Add ASIH code to the data
dd <- merge(dd, bd, by.x = "recordset",by.y = "uuid")

## Need to make this loop faster, so lets subset down to just unique triples of colcode,isntcode,recordset
bbRS <- plyr::count(dd,c("ASIHCode","collectioncode","institutioncode","recordset"))
bbD <- dd

stdPreps <- read.csv("../vocab/lepomisPreps.csv")
gop <- merge(bbD,stdPreps,by="preps",all.x = TRUE)
pb <- progress_bar$new(total = length(bbRS$institutioncode))
for(i in 1:length(bbRS$ASIHCode))
{
        pb$tick()
        gop <- merge(bbD,stdPreps,by="preps",all.x = TRUE)
        
        #Summary by Family
        famsJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22family%22]&count=5000",sep=""))
        famDF <- data.frame(Family=names(famsJS$family),Count=unlist(famsJS$family),row.names = NULL)
        write.csv(famDF, file = paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_famlies.csv",sep=""),row.names = FALSE)
        #Summary by Preptype
        gop <- gop[gop$recordset==bbRS$recordset[i],]
        write.csv(gop, file = paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_preps.csv",sep=""),row.names = FALSE )
        
        
        #Gonna create some better summary data from the raw data we have
        
        ppp <- read.csv(paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_preps.csv",sep=""))
        ppp$freq <- NULL
        cppp <- plyr::count(ppp,"Stand.Prep")
        cppp <- cppp[order(-cppp$freq),]
        write.csv(cppp, file = paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-sum_preps.csv",sep=""),row.names = FALSE )
        
        
        
        #Locality summaries
        # "continent","country","waterbody"
        contJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22continent%22]&count=5000",sep=""))
        conJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22country%22]&count=5000",sep=""))
        watJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22waterbody%22]&count=5000",sep=""))
        
        contDF <- data.frame(Continent=names(contJS$continent),Count=unlist(contJS$continent),row.names = NULL)
        conDF <- data.frame(Country=names(conJS$country),Count=unlist(conJS$country),row.names = NULL)
        watDF <- data.frame(Waterbody=names(watJS$waterbody),Count=unlist(watJS$waterbody),row.names = NULL)
        
        
        write.csv(contDF, file=paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_continents.csv",sep=""),row.names = FALSE)
        write.csv(conDF, file=paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_countries.csv",sep=""),row.names = FALSE)
        write.csv(watDF, file=paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_waterbodies.csv",sep=""),row.names = FALSE)
        
        
        
        #Collector summaries
        # "collector"
        
        collectJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22collector%22]&count=5000",sep=""))
        collectDF <- data.frame(Collector=names(collectJS$collector),Count=unlist(collectJS$collector),row.names = NULL)
        write.csv(collectDF, file=paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_collectors.csv",sep=""),row.names = FALSE)
        
        
        
        ##Type summmaries
        ## 'typestatus'
        
        typeJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22typestatus%22]&count=5000",sep=""))
        typeDF <- data.frame(TypeStatus=names(typeJS$typestatus),Count=unlist(typeJS$typestatus),row.names = NULL)
        write.csv(typeDF, file=paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_typestatus.csv",sep=""),row.names = FALSE)
        
        
        
        ## Can't forget basis of record
        ## "basisofrecord'
        
        basJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:%22",URLencode(as.character(bbRS$recordset[i])),"%22}&top_fields=[%22basisofrecord%22]&count=5000",sep=""))
        basDF <- data.frame(basisofRecord=names(basJS$basisofrecord),Count=unlist(basJS$basisofrecord),row.names = NULL)
        write.csv(basDF, file=paste(bbRS$ASIHCode[i],"/",bbRS$collectioncode[i],"-RAW_basis.csv",sep=""),row.names = FALSE)
        
}









