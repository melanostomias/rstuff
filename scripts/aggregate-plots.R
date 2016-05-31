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
library(RColorBrewer)

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
#larval <- rbind.fill(hugeDF[grep("egg",hugeDF$collectioncode),],hugeDF[grep("larval",hugeDF$collectioncode),]) 
sAC <- aggregate(individualcount ~ ASIHCode, data = hugeDF,sum)
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


##bar graph number of unique families per collection
collectionsData <- list.files("../data")
collectionsData <- collectionsData[!collectionsData=="summary"]
agrDF <- data.frame(ASIHCode=character(0),FamilyCount=integer(0))
for (i in seq_along(collectionsData)){
        oo <- read.csv(paste0("../data/",collectionsData[i],"/data/",collectionsData[i],"-RAW_famlies.csv"))
        asDF <- data.frame(ASIHCode=collectionsData[i],FamilyCount=nrow(oo))
        agrDF <-rbind(agrDF,asDF)
}
agrDF <- agrDF[order(-agrDF$FamilyCount),]
write.csv(agrDF,file ="../data/summary/data/asih-by-families-PLOTLY.csv",row.names = FALSE)
x <- list(title = "ASIH Code")
y <- list(title = "Unique Family Names")

pl <- plot_ly(
        x = agrDF$ASIHCode,
        y = agrDF$FamilyCount,
        name = "Unique Families by ASIH Code",
        type = "bar") %>%
        layout(xaxis = x, yaxis = y)
pl
plotly_IMAGE(pl,format = "png",out_file ="../data/summary/figures/asih-by-families-PLOTLY.png",height = 1080 )        
        

##Map of world with sampling effort across all fish collections by number of records 
collectionsLOC <- list.files("../data")
collectionsLOC <- collectionsData[!collectionsData=="summary"]
locDF <- data.frame()
for (i in seq_along(collectionsData)){
        if(file.exists(paste0("../data/",collectionsData[i],"/data/",collectionsData[i],"-localities-RAW.csv"))){
        ot <- read.csv(paste0("../data/",collectionsData[i],"/data/",collectionsData[i],"-localities-RAW.csv"))
        ot$Continent <- toupper(ot$Continent)
        ot$Continent <- countrycode(ot$Continent,origin = "iso3c",destination = "iso3c",warn = TRUE)
        #cdf <- country2Region(regionType = "GEO3major",inFile = ot,nameDataColumn = "Count",joinCode = "ISO3",nameJoinColumn = "Continent",FUN = "sum")
        #ot <- data.frame(Continent=row.names(cdf),Count=cdf$sumCountbyGEO3major,row.names = NULL)
        ot$ASIHCode <- collectionsData[i]
        ot <- dcast(ot,ASIHCode ~ Continent,value.var = "Count")
        locDF <-rbind.fill(locDF,ot)
        }
}
tess <-locDF
tess$ASIHCode <- NULL
tess <- colSums(tess,na.rm = T)        
tess <- data.frame(Region=attr(tess,"names"),Count=tess)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator'),
        landcolor = toRGB("grey90")
)

pp <- plot_ly(tess, z = Count, text = Region, locations = Region, type = 'choropleth',
        color = Count, colors = colorRampPalette(brewer.pal(7,"Reds"))(length(tess$Count)), marker = list(line = l),  
        colorbar = list(title = 'Sampling Effort')) %>%
        layout(title = 'ASIH Global Sampling Effort ',
               geo = g)
pp
plotly_IMAGE(pp,format = "png",out_file ="../data/summary/figures/asih-by-effort-map-PLOTLY.png",height = 1080 )
write.csv(tess,file ="../data/summary/data/asih-by-effort-map-PLOTLY.csv",row.names = FALSE)

