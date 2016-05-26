library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)
library(scales)
library(plotrix)
library(plotly)
library(reshape2)
library(countrycode)
library(rworldmap)

##Load up the data
##We could call the databuild function or load an rdata file
load("data-raw/lots-o-records.rdata")


dir.create("../data/summary",showWarnings = FALSE)
dir.create("../data/summary/figures",showWarnings = FALSE)
dir.create("../data/summary/data",showWarnings = FALSE)
##Summary Figures- total record count
pdf("../data/summary/figures/asih-by-records.pdf")
srAC <- plyr::count(hugeDF,"ASIHCode")
srAC <- srAC[order(-srAC$freq),]
barplot(srAC$freq, main = "Specimen Records by ASIH Code", names.arg = srAC$ASIHCode,las=2)
dev.off()
write.csv(srAC,file ="../data/summary/data/asih-by-records.csv",row.names = FALSE)
##Summary Figures- total record count PLOTLY
x <- list(title = "ASIH Code")
y <- list(title = "Number of Records")
pl <- plot_ly(
        x = srAC$ASIHCode,
        y = srAC$freq,
        name = "Records",
        type = "bar") %>%
        layout(xaxis = x, yaxis = y)
pl
plotly_IMAGE(pl,format = "png",out_file ="../data/summary/figures/asih-by-records-PLOTLY.png",height = 1080 )


## Total specimen count
pdf("../data/summary/figures/asih-by-specimens.pdf")
larval <- rbind.fill(hugeDF[grep("egg",hugeDF$collectioncode),],hugeDF[grep("larval",hugeDF$collectioncode),]) 
sAC <- aggregate(individualcount ~ ASIHCode, data = hugeDF[!hugeDF$catalognumber %in% larval$catalognumber,],sum)
sAC <- sAC[order(-sAC$individualcount),]
barplot(sAC$individualcount, main = "Specimens by ASIH Code", names.arg = sAC$ASIHCode,las=2)
dev.off()
write.csv(sAC,file ="../data/summary/data/asih-by-specimens.csv",row.names = FALSE)
x <- list(title = "ASIH Code")
y <- list(title = "Number of Specimens (millions)")

pl <- plot_ly(
        x = sAC$ASIHCode,
        y = sAC$individualcount,
        name = "Individuals",
        type = "bar") %>%
        layout(xaxis = x, yaxis = y)
pl
plotly_IMAGE(pl,format = "png",out_file ="../data/summary/figures/asih-by-specimens-PLOTLY.png",height = 1080 )




##Counts of records/color for counts
pdf("../data/summary/figures/asih-by-records-COLOR.pdf")
foff <- merge(srAC,sAC, by.x = "ASIHCode",all.x = TRUE)
names(foff) <- c("ASIHCode", "RecordCount", "SpecimenCount")
p1 <- ggplot(data=foff,aes(x=reorder(foff$ASIHCode,foff$RecordCount),y=RecordCount,fill=RecordCount))+ 
        geom_bar(stat="identity")+
        coord_flip() +
        ggtitle("Digitized ASIH Fish Collections") +
        xlab("ASIH Code")
p1
dev.off()
write.csv(foff,file ="../data/summary/data/asih-by-records-COLOR.csv",row.names = FALSE)

##Stacked summary plot Records/Specimens
pdf("../data/summary/figures/asih-by-specimens-STACKED.pdf")
foff1 <- merge(srAC,sAC, by.x = "ASIHCode",all.x = TRUE)
names(foff1) <- c("ASIHCode", "RecordCount", "SpecimenCount")
foff1 <- melt(foff1)
p2 <- ggplot(foff1,aes(x=reorder(foff1$ASIHCode,foff1$value,mean,na.rm=TRUE),y=value,fill=variable,color=variable)) + 
        geom_bar(stat="identity",position = "stack") +
        coord_flip() +
        ggtitle("Digitized ASIH Fish Collections") +
        xlab("ASIH Code") +
        ylab("Count")
p2
dev.off()
write.csv(foff1,file ="../data/summary/data/asih-by-specimens-STACKED.csv",row.names = FALSE)
