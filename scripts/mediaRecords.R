library(plotly)
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


##Plot

x <- list(title = "ASIH Code",
          type = "category",
          categoryorder = "array",
          categoryarray = sort(mDF$Count,decreasing = T))
y <- list(title = "Media Records")
p5 <- plot_ly(
        data = mDF,
        x = ~ASIHCode,
        y = ~Count,
        type = "bar")%>%
        layout(xaxis = x,yaxis=y)
p5
export(p5,file ="../data/summary/figures/asih-by-mediarecords-PLOTLY.png",vheight = 1080 )
