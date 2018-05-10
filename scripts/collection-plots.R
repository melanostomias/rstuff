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
load("data-raw/hugeDF.rdata")
gop <- hugeDF

bbRS <- plyr::count(gop,c("ASIHCode","collectioncode","institutioncode","recordset"))
##Don't profile KUIT
bbRS <- bbRS[!bbRS$collectioncode=="kuit",]

profile_collections <- function(bbRS, type="all"){
        type <- type
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
                p <- plot_ly(ds, labels = ~labels, values = ~values, type = "pie",text=~labels) %>%
                        layout(title = paste0(ASIHcode," Collection Localities <br> Global Environment Outlook (GEO) Regions <br> <a href=\"http://geodata.grid.unep.ch/images/regional.pdf\">http://geodata.grid.unep.ch/images/regional.pdf</a>"),
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                p
                export(p,file =paste0("../data/",ASIHcode,"/figures/",ASIHcode,"-localities-continents-PLOTLY.png"),vheight= 1080 )
                #plotly_IMAGE(p,format = "png",out_file =paste0("../data/",ASIHcode,"/figures/",ASIHcode,"-localities-continents-PLOTLY.png"),height = 1080 )
        }
        
        ##Query iDigBio and build a locality dataframe for each collection
        contJS <- plyr::count(hugeDF[hugeDF$ASIHCode==ASIHcode,],"dwc.country")
        contJS$Continent <- countrycode::countrycode(contJS$dwc.country,'country.name', 'iso3c')
        contJS$Count <- contJS$freq
        contDF <- contJS[3:4]
        ## Some collections don't have locality data, this should catch them
        if (length(contDF$Continent) > 0){
                ##Write our data to CSV
                write.csv(contDF, file = paste0("../data/",ASIHcode,"/data/",ASIHcode,"-localities-RAW.csv"),row.names = FALSE)
                contDF$Continent <- toupper(contDF$Continent)
                contDF$Continent <- countrycode(contDF$Continent,origin = "iso3c",destination = "iso3c",warn = TRUE)
                cdf <- country2Region(regionType = "GEO3major",inFile = contDF,nameDataColumn = "Count",joinCode = "ISO3",nameJoinColumn = "Continent",FUN = "sum")
                contDF <- data.frame(Continent=row.names(cdf),Count=cdf$sumCountbyGEO3major,row.names = NULL)
                if(type=="figures"|type=="all"){locality_plot(ASIHcode,contDF)}
        }else{print(paste0("No Locality data-",ASIHcode))}
        
        
        ##Build typestatus dataframe for each collection
        #typesJS <- fromJSON(paste("http://search.idigbio.org/v2/summary/top/records/?rq={%22recordset%22:",recordset,",%22collectioncode%22:",collectioncode,",%22institutioncode%22:",institutioncode,"}&top_fields=[%22typestatus%22]&count=5000",sep=""))
        typesJS <- plyr::count(hugeDF[hugeDF$ASIHCode==ASIHcode,],"dwc.typeStatus")
        typeDF <- typesJS[order(typesJS$freq,decreasing = T),]
        names(typeDF) <- c("Typestatus","Count")
        ##The iDigBio API returns Family names not capitalized, we will fix this now
        ##Write typestatus dataframe to CSV
        write.csv(typeDF, file = paste("../data/",ASIHcode,"/data/",ASIHcode,"-RAW_typestatus.csv",sep=""),row.names = FALSE)
        

        ##Build family dataframe for each collection
        famsJS <- plyr::count(hugeDF[hugeDF$ASIHCode==ASIHcode,],"dwc.family")
        famDF <- famsJS[order(famsJS$freq,decreasing = T),]
        names(famDF) <- c("Family","Count")
        ##Write family dataframe to CSV
        write.csv(famDF, file = paste("../data/",ASIHcode,"/data/",ASIHcode,"-RAW_famlies.csv",sep=""),row.names = FALSE)
        ## top 25 family names for each collection pie chart
        ss <- data.frame(labels = famDF$Family[1:25],
                 values = famDF$Count[1:25])
        p <- plot_ly(ss, labels = ~labels, values = ~values, type = "pie") %>%
        layout(title = paste0(ASIHcode," Top 25 Families"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        if(type=="figures"|type=="all"){export(p,file =paste0("../data/",ASIHcode,"/figures/",ASIHcode,"-top25-families-PLOTLY.png"),vheight = 1080 )}


}
}
profile_collections(bbRS,type = "all")
