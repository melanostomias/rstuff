#Locality plot
# Pie Chart with Percentages
library(scales)
library(plotrix)
blank_theme <- theme_minimal()+
        theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
        )
aaaa <- bbRS[bbRS$ASIHCode==bbRS$ASIHCode[1],]
ASIHcode <- unique(aaaa$ASIHCode)
recordset <- if(length(aaaa$recordset) > 1){URLencode(toJSON(as.character(aaaa$recordset)))}else{paste("%22",URLencode(as.character(aaaa$recordset)),"%22",sep="")}
contJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,"}&top_fields=[%22continent%22]&count=5000",sep=""))
contDF <- data.frame(Continent=names(contJS$continent),Count=unlist(contJS$continent),row.names = NULL)
locality_plot <- function(ASIHcode,contDF){
        #Locality plot
        # Pie Chart with Percentages
        pdf(paste(ASIHcode,"/figures/",ASIHcode,"-localities-continents-PIE.pdf",sep=""))
        slices <- contDF$Count 
        lbls <- contDF$Continent 
        pie3D(slices,labels=lbls,explode=0.01,main=paste0(ASIHcode," Collection Localities- Continent"))
#         pie(slices,labels = lbls,
#             main=paste(ASIHcode," Collection Localities- Continent",sep=""))
        dev.off()
}
locality_plot(ASIHcode,contDF)


bp <- ggplot(contDF, aes(x="",y=Count,fill=Continent)) + geom_bar(width = 1,stat="identity")
bp
at <- nrow(contDF) - as.numeric(cumsum(sort(table(contDF$Continent)))-0.5*sort(table(contDF$Continent)))
label <- paste0(round(sort(contDF$Count)/sum(contDF$Count),2) * 100,"%")
pie <- bp + coord_polar("y",start=0) +
        scale_fill_brewer("Continent") +
        blank_theme +
        theme(axis.text.x=element_blank()) +
        ##annotate(geom = "text", y = at, x = 1, label = label)
        geom_text(aes(y = Count/length(levels(contDF$Continent)) + c(0, cumsum(Count)[-length(Count)]), label = label), size=5)

pie





pie3D(slices,labels=lbls,explode=0.01,
      main="Pie Chart of Continents")







