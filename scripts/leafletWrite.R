library(rgdal)
library(spdplyr)
library(rmapshaper)

## This script builds a geojson file for building a world map 
## with sampling effort across all fish collections by number of records 

## First, summarize collections data
collectionsLOC <- list.files("../data")
collectionsLOC <- collectionsLOC[!collectionsLOC=="summary"]
locDF <- data.frame()
for (i in seq_along(collectionsLOC)){
  if(file.exists(paste0("../data/",collectionsLOC[i],"/data/",collectionsLOC[i],"-localities-RAW.csv"))){
    ot <- read.csv(paste0("../data/",collectionsLOC[i],"/data/",collectionsLOC[i],"-localities-RAW.csv"))
    locDF <- plyr::rbind.fill(locDF,ot)
  }
}

## Aggregate the summaries
locDF <- aggregate(Count~Continent, data=locDF,sum)


## Read geojson file of country data
## from https://datahub.io/core/geo-countries
## courtesy of http://www.naturalearthdata.com/

country <- readOGR("../vocab/countries.geojson")


##Use spdplyr to join on our count data
country_map <- country %>% left_join(locDF,by = c("ISO_A3" = "Continent"))

##Change back to geojson object
country_json <- geojsonio::geojson_json(country_map)


##Simplify the geojson object
country_json_simplified <- ms_simplify(country_json)

##Write out the simplified file
geojsonio::geojson_write(country_json_simplified, file = "../data/summary/data/country-simp.geojson")