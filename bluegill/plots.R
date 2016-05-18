library(ridigbio)
library(plyr)
library(jsonlite)
library(httr)
library(dplyr)
library(progress)
library(scales)
library(plotrix)


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
        }else{
                print(paste0("No Locality data-",ASIHcode))
        }
        
        }
locality_plot(ASIHcode,contDF)

famsJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22family%22]&count=5000",sep=""))
famDF <- data.frame(Family=names(famsJS$family),Count=unlist(famsJS$family),row.names = NULL)
write.csv(famDF, file = paste(ASIHcode,"/data/",ASIHcode,"-RAW_famlies.csv",sep=""),row.names = FALSE)


}