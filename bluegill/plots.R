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

##Load up the data
##We could call the databuild function or load an rdata file
load("lots-o-records.rdata")
bd <- read.csv("cleaned-recordsets.csv")
##Check to make sure we only have the collections we want
excludes <- c("MNHN","NRM","INPA","ROM")
hugeDF <- merge(hugeDF,bd,by.x = "recordset",by.y = "uuid")
hugeDF <- hugeDF[!hugeDF$ASIHCode %in% excludes,]

##Clean up preps
stdPreps <- read.csv("../vocab/lepomisPreps.csv")
stdPreps <- stdPreps[1:2]
bbD <- hugeDF 
gop <- merge(bbD,stdPreps,by="preps",all.x = TRUE)

bbRS <- plyr::count(gop,c("ASIHCode","collectioncode","institutioncode","recordset"))

pb <- progress_bar$new(total = length(bbRS$institutioncode))
for(i in 1:length(bbRS$ASIHCode))
{
pb$tick()
aaaa <- bbRS[bbRS$ASIHCode==bbRS$ASIHCode[i],]
ASIHcode <- unique(aaaa$ASIHCode)
dir.create(as.character(ASIHcode),showWarnings = FALSE)
dir.create(paste(as.character(ASIHcode),"/data",sep = ""),showWarnings = FALSE)
dir.create(paste(as.character(ASIHcode),"/figures",sep = ""),showWarnings = FALSE)
recordset <- if(length(aaaa$recordset) > 1){URLencode(toJSON(as.character(aaaa$recordset)))}else{paste("%22",URLencode(as.character(aaaa$recordset)),"%22",sep="")}
collectioncode <- if(length(aaaa$collectioncode) > 1){URLencode(toJSON(as.character(aaaa$collectioncode)))}else{paste("%22",URLencode(as.character(aaaa$collectioncode)),"%22",sep="")}
institutioncode <- if(length(aaaa$institutioncode) > 1){URLencode(toJSON(as.character(aaaa$institutioncode)))}else{paste("%22",URLencode(as.character(aaaa$institutioncode)),"%22",sep="")}
contJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22continent%22]&count=5000",sep=""))
contDF <- data.frame(Continent=names(contJS$continent),Count=unlist(contJS$continent),row.names = NULL)
locality_plot <- function(ASIHcode,contDF){
        #Locality plot
        # Pie Chart with Percentages
        if (length(contDF$Continent) > 0){
                pdf(paste(ASIHcode,"/figures/",ASIHcode,"-localities-continents-PIE.pdf",sep=""))
                slices <- contDF$Count 
                lbls <- contDF$Continent 
                pie3D(slices,labels=lbls,explode=0.01,main=paste0(ASIHcode," Collection Localities- Continent"))
                #         pie(slices,labels = lbls,
                #             main=paste(ASIHcode," Collection Localities- Continent",sep=""))
                dev.off() 
                
                
                ds <- data.frame(labels = contDF$Continent,
                                 values = contDF$Count)
                p <- plot_ly(ds, labels = labels, values = values, type = "pie") %>%
                        layout(title = paste0(ASIHcode," Collection Localities- Continent"))
                plotly_IMAGE(p,format = "png",out_file =paste0(ASIHcode,"/figures/",ASIHcode,"-localities-continents-PLOTLY.png"),height = 1080 )

                
        }else{
                print(paste0("No Locality data-",ASIHcode))
        }
        
        }
locality_plot(ASIHcode,contDF)

famsJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22family%22]&count=5000",sep=""))
famDF <- data.frame(Family=names(famsJS$family),Count=unlist(famsJS$family),row.names = NULL)
write.csv(famDF, file = paste(ASIHcode,"/data/",ASIHcode,"-RAW_famlies.csv",sep=""),row.names = FALSE)

}










##Summary Figures- total record count
pdf("asih-by-records.pdf")
srAC <- plyr::count(hugeDF,"ASIHCode")
srAC <- srAC[order(-srAC$freq),]
barplot(srAC$freq, main = "Specimen Records by ASIH Code", names.arg = srAC$ASIHCode,las=2)
dev.off()
x <- list(
        title = "ASIH Code"
)
y <- list(
        title = "Number of Records"
)

pl <- plot_ly(
        x = srAC$ASIHCode,
        y = srAC$freq,
        name = "Records",
        type = "bar") %>%
        layout(xaxis = x, yaxis = y)
pl
plotly_IMAGE(pl,format = "png",out_file ="asih-by-records-PLOTLY.png",height = 1080 )


## Total specimen count
pdf("asih-by-specimens.pdf")
sAC <- aggregate(individualcount ~ ASIHCode, data = hugeDF,sum)
sAC <- sAC[order(-sAC$individualcount),]
barplot(sAC$individualcount, main = "Specimens by ASIH Code", names.arg = sAC$ASIHCode,las=2)
dev.off()

x <- list(title = "ASIH Code")
y <- list(title = "Number of Specimens (millions)")

pl <- plot_ly(
        x = sAC$ASIHCode,
        y = sAC$individualcount,
        name = "Individuals",
        type = "bar") %>%
        layout(xaxis = x, yaxis = y)
pl
plotly_IMAGE(pl,format = "png",out_file ="asih-by-specimens.png",height = 1080 )





pdf("asih-by-specimens-COLOR.pdf")
foff <- merge(srAC,sAC, by.x = "ASIHCode",all.x = TRUE)
names(foff) <- c("ASIHCode", "RecordCount", "SpecimenCount")
p1 <- ggplot(data=foff,aes(x=reorder(foff$ASIHCode,foff$RecordCount),y=RecordCount,fill=RecordCount))+ 
        geom_bar(stat="identity")+
        coord_flip() +
        ggtitle("Digitized ASIH Fish Collections") +
        xlab("ASIH Code")
p1
dev.off()

pdf("asih-by-specimens-STACKED.pdf")
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


