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
load("data-raw/centrarchidae-recordsets.rdata")

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
        ##Get the ASIH code we are working with
        aaaa <- bbRS[bbRS$ASIHCode==bbRS$ASIHCode[i],]
        ## There may be more than one recordset, so we just need one
        ASIHcode <- unique(aaaa$ASIHCode)
        ##Create directories and supress warnings
        dir.create(paste0("../data/",as.character(ASIHcode)),showWarnings = FALSE)
        dir.create(paste0("../data/",as.character(ASIHcode),"/data"),showWarnings = FALSE)
        dir.create(paste0("../data/",as.character(ASIHcode),"/figures"),showWarnings = FALSE)
        ##We need to create vectors for the triples we are using to identify records
        recordset <- if(length(aaaa$recordset) > 1){URLencode(toJSON(as.character(aaaa$recordset)))}else{paste("%22",URLencode(as.character(aaaa$recordset)),"%22",sep="")}
        collectioncode <- if(length(aaaa$collectioncode) > 1){URLencode(toJSON(as.character(aaaa$collectioncode)))}else{paste("%22",URLencode(as.character(aaaa$collectioncode)),"%22",sep="")}
        institutioncode <- if(length(aaaa$institutioncode) > 1){URLencode(toJSON(as.character(aaaa$institutioncode)))}else{paste("%22",URLencode(as.character(aaaa$institutioncode)),"%22",sep="")}

        
        ##Locality plot function for each collection
        locality_plot <- function(ASIHcode,contDF){
        #Locality plot
        # Pie Chart with Percentages
                ds <- data.frame(labels = contDF$Continent, values = contDF$Count)
                p <- plot_ly(ds, labels = labels, values = values, type = "pie",text=labels) %>%
                        layout(title = paste0(ASIHcode," Collection Localities <br> Global Environment Outlook (GEO) Regions <br> <a href=\"http://geodata.grid.unep.ch/images/regional.pdf\">http://geodata.grid.unep.ch/images/regional.pdf</a>"))
                p
                plotly_IMAGE(p,format = "png",out_file =paste0("../data/",ASIHcode,"/figures/",ASIHcode,"-localities-continents-PLOTLY.png"),height = 1080 )
        }
        
        ##Query iDigBio and build a locality dataframe for each collection
        contJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22countrycode%22]&count=5000",sep=""))
        contDF <- data.frame(Continent=names(contJS$countrycode),Count=unlist(contJS$countrycode),row.names = NULL)
        ## Some collections don't have locality data, this should catch them
        if (length(contDF$Continent) > 0){
                contDF$Continent <- toupper(contDF$Continent)
                contDF$Continent <- countrycode(contDF$Continent,origin = "iso3c",destination = "iso3c",warn = TRUE)
                cdf <- country2Region(regionType = "GEO3major",inFile = contDF,nameDataColumn = "Count",joinCode = "ISO3",nameJoinColumn = "Continent",FUN = "sum")
                contDF <- data.frame(Continent=row.names(cdf),Count=cdf$sumCountbyGEO3major,row.names = NULL)
                locality_plot(ASIHcode,contDF)
        }else{print(paste0("No Locality data-",ASIHcode))}


# ##World Choropleth Map
# cuntJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22countrycode%22]&count=5000",sep=""))
# cuntDF <- data.frame(Country=names(cuntJS$country),Count=unlist(cuntJS$country),row.names = NULL)
# cuntDF$Country <- toupper(cuntDF$Country)
# if (length(cuntDF$Country) > 0){
# # light grey boundaries
# l <- list(color = toRGB("grey"), width = 0.5)
# # specify map projection/options
# g <- list(showframe = FALSE,showcoastlines = FALSE,projection = list(type = 'Mercator'))
# f <- plot_ly(cuntDF, z = Count, text = Country, locations = Country, type = 'choropleth',
#         color = Count, colors = 'Blues', marker = list(line = l),
#         colorbar = list(title = 'Country')) %>%
#         layout(title = 'Records by Country',geo = g)
# plotly_IMAGE(f,format = "png",out_file =paste0("../data/",ASIHcode,"/figures/",ASIHcode,"-localities-continents-PLOTLY-MAP.png"),height = 1080 )
# }else{
#         print(paste0("No Locality data-",ASIHcode))
# }

        ##Build family dataframe for each collection
        famsJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22family%22]&count=5000",sep=""))
        famDF <- data.frame(Family=names(famsJS$family),Count=unlist(famsJS$family),row.names = NULL)
        ##Write family dataframe to CSV
        write.csv(famDF, file = paste("../data/",ASIHcode,"/data/",ASIHcode,"-RAW_famlies.csv",sep=""),row.names = FALSE)
        ## top 25 family names for each collection pie chart
        ss <- data.frame(labels = famDF$Family[1:25],
                 values = famDF$Count[1:25])
        p <- plot_ly(ss, labels = labels, values = values, type = "pie") %>%
        layout(title = paste0(ASIHcode," Top 25 Families"))
        plotly_IMAGE(p,format = "png",out_file =paste0("../data/",ASIHcode,"/figures/",ASIHcode,"-top25-families-PLOTLY.png"),height = 1080 )


}










##Summary Figures- total record count
pdf("../asih-by-records.pdf")
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
plotly_IMAGE(pl,format = "png",out_file ="../asih-by-records-PLOTLY.png",height = 1080 )


## Total specimen count
pdf("../asih-by-specimens.pdf")
larval <- rbind.fill(hugeDF[grep("egg",hugeDF$collectioncode),],hugeDF[grep("larval",hugeDF$collectioncode),]) 
sAC <- aggregate(individualcount ~ ASIHCode, data = hugeDF[!hugeDF$catalognumber %in% larval$catalognumber,],sum)
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
plotly_IMAGE(pl,format = "png",out_file ="../asih-by-specimens.png",height = 1080 )





pdf("../asih-by-specimens-COLOR.pdf")
foff <- merge(srAC,sAC, by.x = "ASIHCode",all.x = TRUE)
names(foff) <- c("ASIHCode", "RecordCount", "SpecimenCount")
p1 <- ggplot(data=foff,aes(x=reorder(foff$ASIHCode,foff$RecordCount),y=RecordCount,fill=RecordCount))+ 
        geom_bar(stat="identity")+
        coord_flip() +
        ggtitle("Digitized ASIH Fish Collections") +
        xlab("ASIH Code")
p1
dev.off()

pdf("../asih-by-specimens-STACKED.pdf")
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