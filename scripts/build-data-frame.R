library(plyr)
library(data.table)
#system.time(load("data-raw/big-ole-dataframe.rdata"))
#Load our centrar dataset
load("data-raw/big-ole-dataframe1.rdata")

hugeDF <- gigaCentDF


#######################################################################################33
famQuant <- plyr::count(plyr::count(hugeDF,c("dwc.family","ASIHCode"))[1:2],"ASIHCode")
names(famQuant) <- c("ASIHCode","FamilyCount")
qv <- quantile(famQuant$FamilyCount,probs = 80/100)
famQuant <- famQuant[famQuant$FamilyCount>qv,]

typeQuant <- plyr::count(plyr::count(hugeDF,c("dwc.typeStatus","ASIHCode"))[1:2],"ASIHCode")
names(typeQuant) <- c("ASIHCode","TypeCount")
tq <- quantile(typeQuant$TypeCount,probs = 80/100)
typeQuant <- typeQuant[typeQuant$TypeCount>tq,]

recQuant <- plyr::count(hugeDF, "ASIHCode")
qr <- quantile(recQuant$freq,probs = 80/100)
recQuant <- recQuant[recQuant$freq>qr,]

dictASIH <- c(famQuant$ASIHCode,typeQuant$ASIHCode,recQuant$ASIHCode)
dictASIH <- unique(dictASIH)


#Found a bug in the summary endpoint
# for (i in 1:length(dictASIH)){
#         readDF <- read.csv(paste0("../data/",dictASIH[i],"/data/",dictASIH[i],"-RAW_famlies.csv"),stringsAsFactors = F)
#         fishDict <- c(fishDict,readDF$Family)
# }

for (i in 1:length(dictASIH)){
        readDF <- hugeDF[hugeDF$ASIHCode==dictASIH[i],]$dwc.family
        if(i==1){fishDict <- readDF}else{fishDict <- c(fishDict,readDF)}
}


fishDict <- unique(fishDict)

## Found some non-fish families in the data...
outFam <- c("scolopacidae","fringillidae","psittacidae","canidae","trochilidae","podicipedidae","campephagidae","gliridae","cheloniidae","crocodylidae","viperidae")
fishDict <- fishDict[!fishDict %in% outFam]

hugeDF <- hugeDF[hugeDF$dwc.family %in% fishDict,]

##fix up column names
colnames(hugeDF)[which(names(hugeDF) == "dwc.institutionCode")] <- "institutioncode"
colnames(hugeDF)[which(names(hugeDF) == "dwc.collectionCode")] <- "collectioncode"
colnames(hugeDF)[which(names(hugeDF) == "dwc.catalogNumber")] <- "catalognumber"
colnames(hugeDF)[which(names(hugeDF) == "dwc.institutionCode")] <- "institutioncode" ##This was supposed to be preps
colnames(hugeDF)[which(names(hugeDF) == "idigbio.recordset")] <- "recordset"
colnames(hugeDF)[which(names(hugeDF) == "dwc.individualCount")] <- "individualcount"
colnames(hugeDF)[which(names(hugeDF) == "dwc.basisOfRecord")] <- "basisOfRecord"





## create working dataframe to save some memory
rqCols <- c("institutioncode", "collectioncode", "catalognumber","recordset","individualcount","basisOfRecord","ASIHCode","collection_uuid","logo","collection_description","idigbio.uuid","rq") 
workingDF <- hugeDF[rqCols]


save(workingDF,file='data-raw/workingDF.rdata',compress = 'xz')
save(hugeDF, file = 'data-raw/hugeDF.rdata', compress = 'xz')
 