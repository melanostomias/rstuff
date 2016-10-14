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

##Family distributions
collectionsData <- list.files("../data")
collectionsData <- collectionsData[!collectionsData=="summary"]
famDIST <- data.frame(Family=character(0),FamilyCount=integer(0))
for (i in seq_along(collectionsData)){
        oo <- read.csv(paste0("../data/",collectionsData[i],"/data/",collectionsData[i],"-RAW_famlies.csv"))
        famDIST <-rbind(famDIST,oo)
}
famDIST <- aggregate(Count ~ Family, data= famDIST,sum)
write.csv(famDIST,file ="../data/summary/data/families-distribution-PLOTLY.csv",row.names = FALSE)
hist(famDIST$Family)
famDIST <- famDIST[order(-famDIST$Count),]
p <- plot_ly(x=famDIST$Family,y=famDIST$Count,type = "bar")
layout(p,yaxis = list(range = c(10, 50000)))


famDIST1 <- famDIST[1:25,]
p <- plot_ly(x=famDIST1$Family,y=famDIST1$Count,type = "bar")
p

famDIST2 <- famDIST[26:100,]
p <- plot_ly(x=famDIST2$Family,y=famDIST2$Count,type = "bar")
p


famDIST3 <- famDIST[101:1000,]
p <- plot_ly(x=famDIST3$Family,y=famDIST3$Count,type = "bar")
p




        

##Map of world with sampling effort across all fish collections by number of records 
collectionsLOC <- list.files("../data")
collectionsLOC <- collectionsLOC[!collectionsLOC=="summary"]
locDF <- data.frame()
for (i in seq_along(collectionsLOC)){
        if(file.exists(paste0("../data/",collectionsLOC[i],"/data/",collectionsLOC[i],"-localities-RAW.csv"))){
        ot <- read.csv(paste0("../data/",collectionsLOC[i],"/data/",collectionsLOC[i],"-localities-RAW.csv"))
        ot$Continent <- toupper(ot$Continent)
        ot$Continent <- countrycode(ot$Continent,origin = "iso3c",destination = "iso3c",warn = TRUE)
        #cdf <- country2Region(regionType = "GEO3major",inFile = ot,nameDataColumn = "Count",joinCode = "ISO3",nameJoinColumn = "Continent",FUN = "sum")
        #ot <- data.frame(Continent=row.names(cdf),Count=cdf$sumCountbyGEO3major,row.names = NULL)
        ot$ASIHCode <- collectionsLOC[i]
        ot <- dcast(ot,ASIHCode ~ Continent,value.var = "Count")
        locDF <-rbind.fill(locDF,ot)
        }
}
tess <-locDF
tess$ASIHCode <- NULL
tess <- colSums(tess,na.rm = T)        
tess <- data.frame(Region=attr(tess,"names"),Count=tess)
##tess <- read.csv("../data/summary/data/asih-by-effort-map-PLOTLY.csv")
isoExclude <- c("USA","MEX","BRA","VEN","PHL","PER")
tess1 <- tess[tess$Region %in% isoExclude,]
tess <- tess[!tess$Region %in% isoExclude,]
#tess$Count <- cume_dist(tess$Count)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator'),
        landcolor = toRGB("purple"),
        showocean = TRUE,
        oceancolor = toRGB("black")
)
sca <- list(c(0, toRGB("skyblue1")), list(1, toRGB("purple")))
pp <- plot_ly(tess, z = Count, text = Region, locations = Region, type = 'choropleth',
        color = Count, colors = colorRampPalette(brewer.pal(6,"Spectral"))(100), marker = list(line = l),  
        colorbar = list(title = 'Records Count')) %>%
        add_trace(z=tess1$Count, locations=tess1$Region,inherit=T,colorscale = sca , type="choropleth",showscale=F, autocolorscale=F,zmin=1,zmax=1) %>%
        layout(title = 'ASIH Global Records by Country',geo = g)
pp





plotly_IMAGE(pp,format = "png",out_file ="../data/summary/figures/asih-by-effort-map-PLOTLY.png",height = 1080 )
write.csv(tess,file ="../data/summary/data/asih-by-effort-map-PLOTLY.csv",row.names = FALSE)

## Typestatus summaries accross ASIH collections
collectionsData <- list.files("../data")
collectionsData <- collectionsData[!collectionsData=="summary"]
tpDF <- data.frame(ASIHCode=character(0),Holotype=integer(0),Paratype=integer(0))
for (i in seq_along(collectionsData)){
        oo <- read.csv(paste0("../data/",collectionsData[i],"/data/",collectionsData[i],"-RAW_typestatus.csv"))
        if(nrow(oo)==0){
                holo <- NA
                para <- NA
        }else{
                if(nrow(oo[tolower(oo$Typestatus)=="holotype",])==0){
                        holo <- NA
                }else{
                        holo <- oo[tolower(oo$Typestatus)=="holotype",]$Count        
                }
                if(nrow(oo[tolower(oo$Typestatus)=="paratype",])==0){
                        para <- NA
                }else{
                        para <- oo[tolower(oo$Typestatus)=="paratype",]$Count + oo[tolower(oo$Typestatus)=="paratype",]$Count
                }
                
        }
        
        asDF <- data.frame(ASIHCode=collectionsData[i],Holotype=holo,Paratype=para)
        tpDF <-rbind(tpDF,asDF)
}
write.csv(tpDF,file ="../data/summary/data/asih-types-summary.csv",row.names = FALSE)
