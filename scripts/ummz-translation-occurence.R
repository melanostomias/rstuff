###########################################################
# University of Michigan has not published data via iDigBio
# These scripts are the workflow for taking a download 
# from their collections management system and creating 
# a psuedo DwC archive that would be produced by 
# the iDigbio indexer.



## Need to read in a known iDigBio dataset first.
## We can use the column names as the basis of our output
kDF <- read.csv("data-raw/idb-download/centDL/UTEP/939301d6-fff1-4bfe-afab-9d3943041f68/occurrence.csv",stringsAsFactors = F)

## Read in UMMZ data that has been treated by iDigBio's biodiversity
## manager consistant with any collection sharing data
uMMz <- read.csv("data-raw/idb-download/centDL/UMMZ/privateIPT/randy_fishes.csv",stringsAsFactors = F)

## Some UMMZ data has already been shared by a TCN. We
## can read this other UMMZ recordset in and include it in our 
## analysis

uMMz2 <- read.csv("data-raw/idb-download/centDL/UMMZ/e4186e5a-e250-4aff-95ae-f4ebc6edfafa/occurrence.csv",stringsAsFactors = F)
for(i in 1:length(uMMz2$dwc.catalogNumber)){
        uMMz2$dwc.catalogNumber[i] <- strsplit(uMMz2$dwc.catalogNumber[i],strtrim(uMMz2$dwc.catalogNumber[i],8))[[1]][2]
}
uMMz2$dwc.catalogNumber <- as.integer(uMMz2$dwc.catalogNumber)
outs <- uMMz2[uMMz2$dwc.catalogNumber %in% uMMz$dwc.catalogNumber,] 
## When we check catalogNumber's, it looks like all of the TCN data is in our privateIPT,
## so we should be safe tossing out the duplicates
uMMz <- uMMz[!uMMz$dwc.catalogNumber %in% uMMz2$dwc.catalogNumber,]

################################################################
# We need to standardize our new data to match the iDigBio index,
# the following steps perform these transformations

## Make a geopoint
uMMz$idigbio.geoPoint <- NA
for(i in 1:length(uMMz$dwc.occurrenceID)){
        if(is.na(uMMz$dwc.decimalLatitude[i])){next}else{
        uMMz$idigbio.geoPoint[i] <- jsonlite::toJSON(list(lat=uMMz$dwc.decimalLatitude[i],lon=uMMz$decimalLongitude[i]),auto_unbox = T)
        }
}

## Standardize coreid
substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
}
uMMz$coreid <- substrRight(uMMz$dwc.occurrenceID,36)
## Add an idigbio.uuid
uMMz$idigbio.uuid <- substrRight(uMMz$dwc.occurrenceID,36)

## Apply a recordset UUID
rsUUID <- "168d73e6-8e1a-4b69-9d8b-96ff2de773ee"
uMMz$idigbio.recordset <- rsUUID

## dwc.institutionCode
uMMz$dwc.institutionCode <- "ummz"

## dwc.collectionCode
uMMz$dwc.collectionCode <- "fishes"

## dwc.typestatus
uMMz[nchar(uMMz$dwc.typeStatus)==0,]$dwc.typeStatus <- NA

## idigbio.isoCountryCode
uMMz$idigbio.isoCountryCode <- countrycode::countrycode(uMMz$dwc.country,'country.name', 'iso3c')

## dwc.basisOfRecord
uMMz$dwc.basisOfRecord <- NA

## dwc.individualCount
uMMz$dwc.individualCount <- NA

## Transform to lowercase dwc.family
uMMz$dwc.family <- tolower(uMMz$dwc.family)

## Our privateIPT has some variables we don't need,
## we can safely drop them now and write our results to
## a file.
dropCol <- c("dwc.identifiedBy","dwc.samplingProtocol","dwc.decimalLatitude","dwc.occurrenceRemarks","decimalLongitude","dwc.preparations","dwc.localityRemarks","dwc.measurementValuesize","dwc.eventTime")
uMMz <- uMMz[,!names(uMMz) %in% dropCol]
write.csv(uMMz,file = "data-raw/idb-download/centDL/UMMZ/privateIPT/occurrence.csv",row.names = F)