##Read in UMMZ from JMcc
uMMz <- read.csv("data-raw/idb-download/centDL/UMMZ/privateIPT/randy_fishes.csv",stringsAsFactors = F)

##read other UMMZ recordset in
uMMz2 <- read.csv("data-raw/idb-download/centDL/UMMZ/e4186e5a-e250-4aff-95ae-f4ebc6edfafa/occurrence.csv",stringsAsFactors = F)
for(i in 1:length(uMMz2$dwc.catalogNumber)){
        uMMz2$dwc.catalogNumber[i] <- strsplit(uMMz2$dwc.catalogNumber[i],strtrim(uMMz2$dwc.catalogNumber[i],8))[[1]][2]
}
uMMz2$dwc.catalogNumber <- as.integer(uMMz2$dwc.catalogNumber)
outs <- uMMz2[uMMz2$dwc.catalogNumber %in% uMMz$dwc.catalogNumber,] 
##looks like the TCN data is all there... lets toss out the dupes
uMMz <- uMMz[!uMMz$dwc.catalogNumber %in% uMMz2$dwc.catalogNumber,]


##Gotta make the coreid standard
substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
}
uMMz$coreid <- substrRight(uMMz$dwc.occurrenceID,36)
##Make idigbio.uuid
uMMz$idigbio.uuid <- substrRight(uMMz$dwc.occurrenceID,36)

##Recordset UUID
rsUUID <- "168d73e6-8e1a-4b69-9d8b-96ff2de773ee"
uMMz$idigbio.recordset <- rsUUID

##dwc.institutionCode
uMMz$dwc.institutionCode <- "ummz"

##dwc.collectionCode
uMMz$dwc.collectionCode <- "fishes"

##fix up dwc.typestatus
uMMz[nchar(uMMz$dwc.typeStatus)==0,]$dwc.typeStatus <- NA

##fix up idigbio.isoCountryCode
uMMz$idigbio.isoCountryCode <- countrycode::countrycode(uMMz$dwc.country,'country.name', 'iso3c')

## fix up "dwc.basisOfRecord"
uMMz$dwc.basisOfRecord <- NA

## fix dwc.individualCount
uMMz$dwc.individualCount <- NA

needCols <- c("coreid","dwc.preparations","dwc.individualCount", "dwc.specificEpithet","dwc.scientificName")
uMMz <- uMMz[,names(uMMz) %in% needCols]

write.csv(uMMz,file = "data-raw/idb-download/centDL/UMMZ/privateIPT/occurrence_raw.csv",row.names = F)
