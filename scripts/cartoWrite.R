library(devtools)
library(CartoDB)

########################################################################
##build dataframe of regions and counts
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


####I may want to write these dataframes to csv
#write.csv(tess,file ="../data/summary/data/asih-by-effort-map-CARTO.csv",row.names = FALSE)
#write.csv(tess1,file ="../data/summary/data/asih-by-effort-map-CARTO-USA.csv",row.names = FALSE)


#Carto creds
username <- "" 
apiKey <- ""        
cartodb(username, api.key=apiKey)

##Get carto table
cdb <- cartodb.collection(sql="SELECT * FROM asih_by_effort_map_plotly")

##Check for new regions
newReg <- tess[!tess$Region %in% cdb$region,]
## Write to CartoDB
for (i in seq_along(newReg$Region)) {
                cartodb.row.insert(name="asih_by_effort_map_plotly",columns=list("region","count"),values=list(newReg$Region[i],newReg$Count[i]))
        }
httr:GET(paste0("https://",username,".carto.com/api/v2/sql?q=UPDATE%20asih_by_effort_map_plotly%20SET%20the_geom%20=%20cdb_geocode_admin0_polygon(region)&api_key=",apiKey))
##update regions        
upReg <- tess[tess$Region %in% cdb$region,]
for (i in seq_along(upReg$Region)) {
        cartodb.row.update(name="asih_by_effort_map_plotly",cartodb_id =cdb[cdb$region==upReg$Region[i],]$cartodb_id[[1]] ,columns=list("region","count"),values=list(upReg$Region[i],upReg$Count[i]))
}

##Fix up the USA stuff now
cdb <- cartodb.collection(sql="SELECT * FROM asih_by_effort_map_plotly_usa")
##Check for new regions
newReg <- tess1[!tess1$Region %in% cdb$region,]
## Write to CartoDB
if(nrow(newReg)>0){
for (i in seq_along(newReg$Region)) {
        cartodb.row.insert(name="asih_by_effort_map_plotly_usa",columns=list("region","count"),values=list(newReg$Region[i],newReg$Count[i]))
}
httr:GET("https://",username,".carto.com/api/v2/sql?q=UPDATE%20asih_by_effort_map_plotly_usa%20SET%20the_geom%20=%20cdb_geocode_admin0_polygon(region)&api_key=7",apiKey))
}

##update regions        
upReg <- tess1[tess1$Region %in% cdb$region,]
for (i in seq_along(upReg$Region)) {
        cartodb.row.update(name="asih_by_effort_map_plotly_usa",cartodb_id =cdb[cdb$region==upReg$Region[i],]$cartodb_id[[1]] ,columns=list("region","count"),values=list(upReg$Region[i],upReg$Count[i]))
}