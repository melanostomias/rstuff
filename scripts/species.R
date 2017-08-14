library(plyr)
library(tidyr)
library(jsonlite)
library(dplyr)
library(plotly)

######################################################################
## This script is used to clean up the outputs of 'build-data-frame.R'
## and do more work on dwc.scientificName & dwc.specificEpithet data
##
## Inputs: 'data-raw/workingDF.rdata'
## Outputs: 'raw-speciesDF.rdata'


## Build a dataframe from the raw occurence data downloaded previously
## Since we are only interested in working with the specificEpithet and 
## scientificName, we will only look at that data now

folderVector <- list.dirs("data-raw/idb-download/centDL",recursive = F)
gigaDF <- data.frame()
for (i in 1:length(folderVector)){
        datafiles <- list.dirs(folderVector[i],recursive = F)
        for(ii in 1:length(datafiles)){
        tmpDF <- read.csv(paste0(datafiles[ii],"/occurrence_raw.csv"),stringsAsFactors = F)
        myCols <- c("coreid","dwc.specificEpithet","dwc.scientificName")
        tmpDF <- tmpDF[myCols]
        gigaDF <- rbind.fill(gigaDF,tmpDF)
        }
}

## Write this raw species data to a file
save(gigaDF,file = "data-raw/raw-speciesDF.rdata",compress = "bzip2")

## We can load the dataframe from a file
## load("data-raw/raw-speciesDF.rdata")
## or we can work from the object we have already created.

## For now, we're gonna create a new object and name the variables
## we are interested in
species <- gigaDF
names(species) <- c("idigbio.uuid","dwc.specificEpithet","dwc.scientificName")

## We can now match our species data back to our working dataframe
## load("data-raw/raw-speciesDF.rdata")
load("data-raw/workingDF.rdata")
workingDF <- merge(workingDF,species,all.x = T)

## We can write the data we find about each collection to a 
## file in a directory
specASIH <- unique(workingDF$ASIHCode)
for(i in 1:length(specASIH)){
        dir.create("../data",showWarnings = F)
        dir.create(paste0("../data/",as.character(specASIH[i])),showWarnings = FALSE)
        dir.create(paste0("../data/",as.character(specASIH[i]),"/data"),showWarnings = FALSE)
        dir.create(paste0("../data/",as.character(specASIH[i]),"/figures"),showWarnings = FALSE)
        rawSpecsDF <- plyr::count(gigaDF[gigaDF$coreid %in% workingDF[workingDF$ASIHCode==specASIH[i],]$idigbio.uuid,]$dwc.scientificName)
        names(rawSpecsDF) <- c("dwc.scientificName","freq")
        rawSpecsDF <- rawSpecsDF[order(rawSpecsDF$freq,decreasing = T),]
        write.csv(rawSpecsDF, file = paste0("../data/",specASIH[i],"/data/",specASIH[i],"-RAW_species.csv"),row.names = FALSE)
}

## We can also build summary data files and
## write those to a summary directory

aggSpecsDF <- plyr::count(workingDF,c("ASIHCode","dwc.scientificName"))
aggSpecsDF$freq <- NULL
aggSpecsDF <- plyr::count(aggSpecsDF, "ASIHCode")

dir.create("../data/summary",showWarnings = F)
dir.create("../data/summary/data",showWarnings = F)
dir.create("../data/summary/figures",showWarnings = F)
write.csv(aggSpecsDF, file = "../data/summary/data/asih-by-species.csv",row.names = FALSE)

## Plot our aggregated species counts for the aggregate and
## output to a summary directory
aggSpecsDF <- aggSpecsDF[order(aggSpecsDF$freq,decreasing = T),]
x <- list(title = "ASIH Code",
          type = "category",
          categoryorder = "array",
          categoryarray = sort(aggSpecsDF$freq,decreasing = T))
y <- list(title = "Unique Scientific Names")
mark <- list(color = 'rgb(250,70,22)',
             line = list(color = 'rgb(0,33,165)',
                         width = 1.5))
pl <- plot_ly(
        x = aggSpecsDF$ASIHCode,
        y = aggSpecsDF$freq,
        name = "Unique Scientific Names by ASIH Code",
        type = "bar",
        marker = mark) %>%
        layout(xaxis = x, yaxis = y)
pl
export(pl,file ="../data/summary/figures/asih-by-species-PLOTLY.png",vheight = 1080 )  
