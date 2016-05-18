library(ridigbio)
library(plyr)
library(jsonlite)

###Some functions we might need later
idig_build_attrib <- function(dat){
        dat$count <- 1
        datAgg <- aggregate(count ~ recordset, data = dat,sum)
        datAt <- attributes(dat)
        dx <- data.frame(stringsAsFactors = FALSE)
        for (i in seq(1,length(datAt$attribution))){
                if (datAt$attribution[[i]]$uuid %in% dat$recordset) {
                        collection <- datAt$attribution[[i]]$name
                        uuid <- datAt$attribution[[i]]$uuid
                        itemCount <- datAgg$count[datAgg$recordset == datAt$attribution[[i]]$uuid]
                        rows <- cbind(data.frame(collection,stringsAsFactors = FALSE),data.frame(uuid,stringsAsFactors = FALSE),data.frame(itemCount,stringsAsFactors = FALSE))
                        dx <- plyr::rbind.fill(dx,rows)
                }
                
        }
        dx
}






##Query
rq <- list("genus"="lepomis")
## Query Dataframe
dd <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations"))
##Let's fix the names of the columns to save work later
names(dd) <- c("institutioncode","collectioncode","catalognumber","preps")
##Gotta write that raw data to a file, so we can preserve it, yo!
write.csv(dd,row.names = FALSE,file=paste("raw-",rq,"-data-",Sys.Date(),".csv",sep=""))

##How many institution codes? 
hD <- count(dd,"institutioncode")
hD <- hD[order(-hD$freq),]
##Histogram them
barplot(hD$freq,names.arg = hD$institutioncode, las=2)
abline(h=mean(hD$freq))


##how many collection codes?
jD <- count(dd,"collectioncode")
jD <- jD[order(-jD$freq),]
## Histogram them
barplot(jD$freq,names.arg = jD$collectioncode,las=2)


## Are the identifiers unique?
length(unique(dd$catalognumber))
n_occur <-count(dd,"catalognumber")
dupes <- dd[dd$catalognumber %in% n_occur$catalognumber[n_occur$freq>1],]
dupes <- dupes[order(dupes$catalognumber),]



##How many specimens per institution/collection code pair?
institutionCodes <- unique(dd$institutioncode)
xx <- data.frame()
for (i in 1:length(institutionCodes)) {
yy <- count(dd[dd$institutioncode==institutionCodes[i],]$collectioncode)
cc <- cbind(institutionCodes[i],yy)
xx <- rbind(xx,cc)
}
xx <- xx[order(xx$freq,decreasing = TRUE),]
##Histram them
barplot(xx$freq,names.arg = paste(xx$`institutionCodes[i]`,xx$x,sep="-"),las=2)
abline(h=mean(xx$freq))

##Let's look at the collections by prep types
## prep type query
## http://search.idigbio.org/v2/search/records?rq={%22data.dwc:preparations%22:{%22type%22:%22exists%22},%22genus%22:%22lepomis%22}

trimDD <- dd[dd$collectioncode %in% xx$x,]

prepDD <- count(trimDD,"preps")
prepDD <- prepDD[order(-prepDD$freq),]
prepDD <- prepDD[-is.na(prepDD$preps),]
##Histogram them
barplot(prepDD$freq,names.arg = prepDD$preps, las=2)

##Holy shit, thats a lot of prep types!
## Can we see how many prep types per collection?
ww <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(institutionCodes)) {
        uu <- data.frame(length(count(dd[dd$institutioncode == institutionCodes[i],]$preps)$x))
        ff <- cbind(institutionCodes[i],uu)
        ww <- rbind(ww,ff)
}
names(ww) <- c("collectioncode","prepCount")
ww <- ww[order(ww$prepCount,decreasing = TRUE),]
##Histram them
barplot(ww$prepCount,names.arg = ww$collectioncode,las=2)
abline(h=mean(ww$prepCount))


##Whats the most popular prep type?

prepTypes <- prepDD


##Lets fix up our dataframe to include our preps data

prepsData <- read.csv("lepomisPrepsMod.csv")
##Let's clean up the data columns
prepsData$X <- NULL

names(prepsData) <- c("preps", "StandPrep", "StandPrep2","StandPrep3","Tissue","Photo","freq")

i <- sapply(prepsData, is.factor)
prepsData[i] <- lapply(prepsData[i], as.character)

for (i in 1:length(dd$preps)){
    key <- dd$preps[i]
    pkey <- match(key,prepsData$preps)
            dd$StandPrep[i] <- prepsData$StandPrep[pkey]
            dd$StandPrep2[i] <- prepsData$StandPrep2[pkey]
            dd$StandPrep3[i] <- prepsData$StandPrep3[pkey]
            dd$Tissue[i] <- prepsData$Tissue[pkey]
            dd$Photo[i] <- prepsData$Photo[pkey]

    }


##Let's look at the collections by our cleaned prep types

prepsDD <- count(dd,"StandPrep")
prepsDD <- prepsDD[order(-prepsDD$freq),]

##Histogram them
barplot(prepsDD$freq,names.arg = prepsDD$StandPrep, las=2,main = "Preperations of genus 'Lepomis' \n in iDigBio")

library(ggplot2)
pdf("standPreps.pdf")
rrDD <- dd[dd$institutioncode %in% dd$institutioncode[1:5],]
rrdd <- count(rrDD,c("institutioncode","StandPrep"))
sp <- ggplot(data=rrdd, aes(x= StandPrep ,y= freq,fill=StandPrep)) +geom_bar(stat="identity")
sp + facet_grid(institutioncode ~.)        
dev.off()


##Lets make this dataset a little bigger
smallDD <- count(rrDD,c("institutioncode","collectioncode"))

ff <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(smallDD$institutioncode)){
        rq <- list("institutioncode"=smallDD$institutioncode[i],"collectioncode"=smallDD$collectioncode[i])
        counts <- idig_count_records(rq)
        ll <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations"),max_items = counts)
        names(ll) <- c("institutioncode","collectioncode","catalognumber","preps")
        ff <- rbind(ff,ll)
}

totalDF <- merge(ff,prepsData,by="preps",all.x=TRUE)
totalDF$freq <- NULL
pdf("standPrepsALL.pdf")
rrDF <- count(totalDF,c("institutioncode","StandPrep"))
rrDF <- rrDF[!is.na(rrDF$StandPrep),]

sp <- ggplot(data=rrDF, aes(x= StandPrep ,y= freq,fill=StandPrep)) +geom_bar(stat="identity")
sp + facet_grid(institutioncode ~.)        
dev.off()







##How many institution codes/collection code pairs ? 
hD <- count(dd,c("institutioncode","collectioncode"))
hD <- hD[order(-hD$freq),]
for (i in 1:length(hD$institutioncode)){
        rq <- list("collectioncode"=hD$collectioncode[i],"institutioncode"=hD$institutioncode[i])
        counts <- idig_count_records(rq)
        val <- idig_search_records(rq,fields="recordset",limit=1)
        datVal <- attributes(val)
        if (length(datVal$attribution)==1){
                val <- unique(val$recordset)
                hD$recordset[i] <- val 
        }else{  
#                 for (i in seq(1,length(datVal$attribution))){
#                         uuid <- datAt$attribution[[i]]$uuid
#                         c(val,uuid)
#                 }
                 val <- "More than one recordset"
                 hD$recordset[i] <- val        
        }
        
}

hD$recordset <- unlist(hD$recordset)
write.csv(hD,file = "fishInstCollpairs.csv",row.names = FALSE)

## Let's chase down these "More than one recordset"

multiHD <- hD[hD$recordset=="More than one recordset",]
gg <- data.frame()
for (i in 1:length(multiHD$institutioncode)){
        rq <- list("collectioncode"=multiHD$collectioncode[i],"institutioncode"=multiHD$institutioncode[i])
        counts <- idig_count_records(rq)
        val <- idig_search_records(rq,fields="recordset",max_items = counts)
        hh <- idig_build_attrib(val)
        gg <- rbind(gg,hh)
}



##Let's make a directory for each AISH collection code in our data directory
## bd <- read.csv("fishInstCollpairsAISH")
for (i in 1:length(bd$ASIH.Code)) {
        dir.create(as.character(bd$ASIH.Code[i]))
}


## Let's work on summary by family
## dd <- read.csv(file = "raw-lepomis-data-2016-03-22.csv")
## hD <-count(dd,c("institutioncode","collectioncode")) 
lD <- hD[10:15,]
lD <- merge(lD,bd,by.x = c("collectioncode","institutioncode"),by.y = c("idb.collectioncode","idb.institutioncode"),all.x = TRUE)
for (i in 1:length(lD$institutioncode)){
        
        famsJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22family%22]&count=5000",sep=""))
        famDF <- data.frame(Family=names(famsJS$family),Count=unlist(famsJS$family),row.names = NULL)
        write.csv(famDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_famlies.csv",sep=""),row.names = FALSE)
        
}

        
#Summary by Preptype 
stdPreps <- read.csv("vocab/lepomisPreps.csv")
for (i in 1:length(lD$institutioncode)){
        rq <- list("institutioncode"=lD$institutioncode[i],"collectioncode"=lD$collectioncode[i])
        counts <- idig_count_records(rq)
        opp <- idig_search_records(rq, fields=c("institutioncode","collectioncode","catalognumber","data.dwc:preparations"),max_items = counts)
        names(opp) <- c("institutioncode","collectioncode","catalognumber","preps")
        gop <- merge(opp,stdPreps,by="preps")
        write.csv(gop, file = paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_preps.csv",sep=""),row.names = FALSE )
}

#Gonna create some better summary data from the raw data we have
for (i in 1:length(lD$institutioncode)) {
        ppp <- read.csv(paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_preps.csv",sep=""))
        ppp$freq <- NULL
        cppp <- count(ppp,"Stand.Prep")
        cppp <- cppp[order(-cppp$freq),]
        write.csv(cppp, file = paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-sum_preps.csv",sep=""),row.names = FALSE )
}


#Locality summaries
# "continent","country","waterbody"
for (i in 1:length(lD$institutioncode)){
        
        contJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22continent%22]&count=5000",sep=""))
        conJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22country%22]&count=5000",sep=""))
        watJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22waterbody%22]&count=5000",sep=""))
        
        contDF <- data.frame(Continent=names(contJS$continent),Count=unlist(contJS$continent),row.names = NULL)
        conDF <- data.frame(Country=names(conJS$country),Count=unlist(conJS$country),row.names = NULL)
        watDF <- data.frame(Waterbody=names(watJS$waterbody),Count=unlist(watJS$waterbody),row.names = NULL)
        
        
        write.csv(contDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_continents.csv",sep=""),row.names = FALSE)
        write.csv(conDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_countries.csv",sep=""),row.names = FALSE)
        write.csv(watDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_waterbodies.csv",sep=""),row.names = FALSE)
}


#Collector summaries
# "collector"
for (i in 1:length(lD$institutioncode)){
        
        collectJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22collector%22]&count=5000",sep=""))
        collectDF <- data.frame(Collector=names(collectJS$collector),Count=unlist(collectJS$collector),row.names = NULL)
        write.csv(collectDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_collectors.csv",sep=""),row.names = FALSE)
        
}

##Type summmaries
## 'typestatus'

for (i in 1:length(lD$institutioncode)){
        
        typeJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22typestatus%22]&count=5000",sep=""))
        typeDF <- data.frame(TypeStatus=names(typeJS$typestatus),Count=unlist(typeJS$typestatus),row.names = NULL)
        write.csv(typeDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_typestatus.csv",sep=""),row.names = FALSE)
        
}

## Can't forget basis of record
## "basisofrecord'

for (i in 1:length(lD$institutioncode)){
        
        basJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22collectioncode%22:%22",URLencode(as.character(lD$collectioncode[i])),"%22,%22institutioncode%22:%22",URLencode(as.character(lD$institutioncode[i])),"%22}&top_fields=[%22basisofrecord%22]&count=5000",sep=""))
        basDF <- data.frame(basisofRecord=names(basJS$basisofrecord),Count=unlist(basJS$basisofrecord),row.names = NULL)
        write.csv(basDF, file=paste("data/",lD$ASIH.Code[i],"/",lD$institutioncode[i],"-",lD$collectioncode[i],"-RAW_basis.csv",sep=""),row.names = FALSE)
        
}


