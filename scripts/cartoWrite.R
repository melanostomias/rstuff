library(devtools)
library(CartoDB)

########################################################################
## This script builds a dataframe of collection localities and pushes them
## to a carto table using the carto API for mapping in the Bluegill prototype


## Build dataframe of regions and counts
## write to cartodb and update maps

##Map of world with sampling effort across all fish collections by number of records 
collectionsLOC <- list.files("../data")
collectionsLOC <- collectionsLOC[!collectionsLOC=="summary"]
locDF <- data.frame()
for (i in seq_along(collectionsLOC)){
        if(file.exists(paste0("../data/",collectionsLOC[i],"/data/",collectionsLOC[i],"-localities-RAW.csv"))){
                ot <- read.csv(paste0("../data/",collectionsLOC[i],"/data/",collectionsLOC[i],"-localities-RAW.csv"))
                locDF <- plyr::rbind.fill(locDF,ot)
        }
}

locDF <- aggregate(Count~Continent, data=locDF,sum)
tess <-locDF
names(tess) <- c("Region","Count")
isoExclude <- c("USA","MEX","BRA","VEN","PHL","PER")
tess1 <- tess[tess$Region %in% isoExclude,]
tess <- tess[!tess$Region %in% isoExclude,]


#### Todo: I may want to write these dataframes to csv
#write.csv(tess,file ="../data/summary/data/asih-by-effort-map-CARTO.csv",row.names = FALSE)
#write.csv(tess1,file ="../data/summary/data/asih-by-effort-map-CARTO-USA.csv",row.names = FALSE)


## Carto credentials, you'll want to put your info here before proceeding
username <- "" 
apiKey <- ""        
cartodb(username, api.key=apiKey)

## Get the carto table we are interested in and
## create a dataframe that we can work with
cdb <- cartodb.collection(sql="SELECT * FROM asih_by_effort_map_plotly")

##Check for new regions that are not currently in the data
newReg <- tess[!tess$Region %in% cdb$region,]
## Write those regions to Carto
for (i in seq_along(newReg$Region)) {
                cartodb.row.insert(name="asih_by_effort_map_plotly",columns=list("region","count"),values=list(newReg$Region[i],newReg$Count[i]))
        }
httr:GET(paste0("https://",username,".carto.com/api/v2/sql?q=UPDATE%20asih_by_effort_map_plotly%20SET%20the_geom%20=%20cdb_geocode_admin0_polygon(region)&api_key=",apiKey))
## Update regions that are already present       
upReg <- tess[tess$Region %in% cdb$region,]
for (i in seq_along(upReg$Region)) {
        cartodb.row.update(name="asih_by_effort_map_plotly",cartodb_id =cdb[cdb$region==upReg$Region[i],]$cartodb_id[[1]] ,columns=list("region","count"),values=list(upReg$Region[i],upReg$Count[i]))
}

## We've got a large spread in the data so we've
## we can fix up the that data now
cdb <- cartodb.collection(sql="SELECT * FROM asih_by_effort_map_plotly_usa")
## Check for new regions
newReg <- tess1[!tess1$Region %in% cdb$region,]
## Write to carto
if(nrow(newReg)>0){
for (i in seq_along(newReg$Region)) {
        cartodb.row.insert(name="asih_by_effort_map_plotly_usa",columns=list("region","count"),values=list(newReg$Region[i],newReg$Count[i]))
}
httr:GET("https://",username,".carto.com/api/v2/sql?q=UPDATE%20asih_by_effort_map_plotly_usa%20SET%20the_geom%20=%20cdb_geocode_admin0_polygon(region)&api_key=7",apiKey))
}

## Update regions that are present already        
upReg <- tess1[tess1$Region %in% cdb$region,]
for (i in seq_along(upReg$Region)) {
        cartodb.row.update(name="asih_by_effort_map_plotly_usa",cartodb_id =cdb[cdb$region==upReg$Region[i],]$cartodb_id[[1]] ,columns=list("region","count"),values=list(upReg$Region[i],upReg$Count[i]))
}