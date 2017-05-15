##Load Libraries
library(plyr)
library(tidyr)
library(jsonlite)
library(dplyr)
library(plotly)


##Build a dataframe from the downloaded fishy datasets
folderVector <- list.dirs("data-raw/idb-download/centDL",recursive = F)
gigaDF <- data.frame()
for (i in 1:length(folderVector)){
        datafiles <- list.dirs(folderVector[i],recursive = F)
        for(ii in 1:length(datafiles)){
        tmpDF <- read.csv(paste0(datafiles[ii],"/occurrence_raw.csv"),stringsAsFactors = F)
        myCols <- c("coreid","dwc.preparations","dwc.individualCount")
        tmpDF <- tmpDF[myCols]
        gigaDF <- rbind.fill(gigaDF,tmpDF)
        }
}
save(gigaDF,file = "data-raw/raw-prepsDF.rdata",compress = "bzip2")
##load("data-raw/raw-prepsDF.rdata")
mydf <- gigaDF
names(mydf) <- c("V2","V1","V3")

## Give value to NULL right now
mydf[is.na(mydf$V1),]$V1 <- "No value provided"
mydf[nchar(mydf$V1)==0,]$V1 <- "No value provided"



##split out on semicolon
mydf <- mydf %>% mutate(V1 = strsplit(as.character(V1), ";|\\|")) %>% unnest(V1)
## let's only work on unique values
mydf <- mydf[!duplicated(mydf),]
## clean up leading/training whitespace
mydf$V1 <- trimws(mydf$V1,"both")

######################################################################## Lump all ethanol together
alch <- mydf[grepl("alc|eth|etoh|iso|fluid|vial|\\(formalin-fixed\\)|tank", tolower(mydf$V1)),]

## remove outliers for tissue
alch <- alch[!grepl("tiss", tolower(alch$V1)),]
## remove Zero count items
alch <- alch[!alch$V1=="0 alc",]

#############################################################################lump formalin
form <- mydf[grepl("form", tolower(mydf$V1)),]
## remove outliers for alch
form <- form[!grepl("->70|mold|non-forma|eth|\\(formalin-fixed\\)", tolower(form$V1)),]

############################################################################ Lump for c&s
cands <- mydf[grepl("c&s",tolower(mydf$V1))|grepl("clear",tolower(mydf$V1))|grepl("stain",tolower(mydf$V1))|grepl("glyc",tolower(mydf$V1))|grepl("c & s",tolower(mydf$V1)),]
# remove outliers: stainless steel
cands <- cands[!grepl("steel", tolower(cands$V1)),]
# remove Zero count items
cands <- cands[!cands$V1=="C&S - 0",]
cands <- cands[!cands$V1=="0 c&s",]


######################################################################## Lump for skeleton
skel <- mydf[grepl("skel|oste|oto|bone|skul|vert|mount|dried|mold|[0-9] sk",tolower(mydf$V1)),]
##remove outliers: c&s
skel <- skel[!grepl("c&s|photo|^[0] sk", tolower(skel$V1)),]
## remove Zero count items
skel <- skel[!skel$V1=="0 sk",]


###########################################################################lump for tissue
tissue <- mydf[grepl("etoh \\| tissue\\:|tissue|95\\% etoh \\(tiss\\)|frozen \\(tis",tolower(mydf$V1))|mydf$V1=="tiss",]


###########################################################################lump for media
## todo: find "radiograph" matches (exp. USNM)
media <- mydf[grepl("x\\-ray|photo|xray|slide|radio",mydf$V1),]

##########################################################################lump for NULL
notpro <- mydf[mydf$V1=="No value provided",]


## Standardize the names
alch$V1 <- "alchohol"
cands$V1 <- "clearAndStain"
skel$V1 <- "skeleton"
tissue$V1 <- "tissue"
media$V1 <- "media"
form$V1 <- "formalin"
notpro$V1 <- "notProvided"

## Build a dataframe from the standardized names
preps <- rbind.fill(alch,form,cands,tissue,media,skel,notpro)
## Don't need counts anymore
preps$V3 <- NULL
## Fix up variable names
names(preps) <- c("idigbio.uuid","standardPrep")
save(preps,file = "data-raw/prepsDF.rdata",compress = "bzip2")









## Match everything back
load("data-raw/prepsDF.rdata")
load("data-raw/workingDF.rdata")
workingDF <- merge(workingDF,preps,all.x = T)
workingDF[is.na(workingDF$standardPrep),]$standardPrep <- "other"


prepASIH <- unique(workingDF$ASIHCode)
for(i in 1:length(prepASIH)){
        ##Todo: write raw preps csv
        rawPrepsDF <- plyr::count(gigaDF[gigaDF$coreid %in% workingDF[workingDF$ASIHCode==prepASIH[i],]$idigbio.uuid,]$dwc.preparations)
        names(rawPrepsDF) <- c("dwc.preparations","freq")
        rawPrepsDF <- rawPrepsDF[order(rawPrepsDF$freq,decreasing = T),]
        write.csv(rawPrepsDF, file = paste0("../data/",prepASIH[i],"/data/",prepASIH[i],"-RAW_Preps.csv"),row.names = FALSE)
        ##Get on with the show
        prepsDF <- plyr::count(workingDF[workingDF$ASIHCode==prepASIH[i],],"standardPrep")
        write.csv(prepsDF, file = paste0("../data/",prepASIH[i],"/data/",prepASIH[i],"-standardPreps.csv"),row.names = FALSE)
        p <- plot_ly(prepsDF, labels = ~standardPrep, values = ~freq, type = "pie") %>%
                layout(title = paste0(prepASIH[i]," Standard Preparations"),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        export(p,file =paste0("../data/",prepASIH[i],"/figures/",prepASIH[i],"-standardPreps-PLOTLY.png"),vheight = 1080 )
        
        
}

aggPrepsDF <- plyr::count(workingDF,c("ASIHCode","standardPrep"))
aggPrepsDF <- spread(aggPrepsDF, standardPrep, freq)


f <- plot_ly(aggPrepsDF, x = ~ASIHCode, y = ~alchohol, type = 'bar', name = 'Alchohol') %>%
        add_trace(y = ~clearAndStain, name = 'Cleared and Stained') %>%
        add_trace(y = ~skeleton, name = 'Skeleton') %>%
        add_trace(y = ~tissue, name = 'Tissue') %>%
        add_trace(y = ~media, name = 'Media') %>%
        add_trace(y = ~formalin, name = 'Formalin') %>%
        add_trace(y = ~notProvided, name = 'Not provided') %>%
        add_trace(y = ~other, name = 'Other or non-standard prep') %>%
        layout(yaxis = list(title = 'Count'), barmode = 'stack')
f

write.csv(aggPrepsDF,file = "../data/summary/data/preps-distribution.csv",row.names = F)
