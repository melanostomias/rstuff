## mediarecords
folderVector <- list.dirs("data-raw/idb-download/centDL",recursive = F)
mDF <- data.frame()
for (i in 1:length(folderVector)){
        datafiles <- list.dirs(folderVector[i],recursive = F)
        for(ii in 1:length(datafiles)){
                if(!datafiles[ii]=="data-raw/idb-download/centDL/UMMZ/privateIPT"){
                        tmpDF <- read.csv(paste0(datafiles[ii],"/multimedia.csv"),stringsAsFactors = F)
                        cDF <- data.frame(ASIHCode=folderVector[i],Count=nrow(tmpDF))
                        mDF <- plyr::rbind.fill(mDF,cDF)}
        }
}

mDF <- mDF[mDF$Count>0,]
mDF <- aggregate(Count ~ASIHCode,data=mDF,sum)
mDF <- mDF[order(mDF$Count, decreasing = T),]
mDF$ASIHCode <- gsub("data-raw/idb-download/centDL/", "", mDF$ASIHCode)

write.csv(mDF,file = "../data/summary/data/asih-by-mediarecords.csv",row.names = F)
