syntheticdataset=read.csv('mergedsampleset.csv')
#https://www.cdc.gov/asthma/most_recent_data.htm


syntheticdataset$childhood.asthma=rep(NA,nrow(syntheticdataset))
  
syntheticdataset[syntheticdataset$member=="Child",]$childhood.asthma <- rep(0.084,sum(syntheticdataset$member=="Child"))

syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$sex=="Male",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$sex=="Male",]$childhood.asthma+0.015
syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$sex=="Female",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$sex=="Female",]$childhood.asthma-0.015

syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$age=="Under 5",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$age=="Under 5",]$childhood.asthma-0.037
syntheticdataset[syntheticdataset$member=="Child"&(syntheticdataset$age=="5 to 9"|syntheticdataset$age=="10 to 14"),]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&(syntheticdataset$age=="5 to 9"|syntheticdataset$age=="10 to 14"),]$childhood.asthma+0.014
syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$age=="15 to 17",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$age=="15 to 17",]$childhood.asthma+0.014

syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$race=="White",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$race=="White",]$childhood.asthma-0.01
syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$race=="Black",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$race=="Black",]$childhood.asthma+0.05
syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$race=="Hispanic or Latino",]$childhood.asthma <- syntheticdataset[syntheticdataset$member=="Child"&syntheticdataset$race=="Hispanic or Latino",]$childhood.asthma-0.004

seed=1 #this is where I stopped it to check on it
#syntheticdataset=read.csv('sampleset.csv')
for (person in 1:nrow(syntheticdataset)){
  set.seed(seed)
  syntheticdataset$childhood.asthma[person]=ifelse(is.na(syntheticdataset$childhood.asthma[person]),NA,sample(c("yes","no"),1,prob=c(as.numeric(as.character(syntheticdataset$childhood.asthma[person])),1-as.numeric(as.character(syntheticdataset$childhood.asthma[person])))))
  seed=seed+1
}




numberofchildren=ftable(table(syntheticdataset$tract,syntheticdataset$member))
number_of_children=as.data.frame.matrix(numberofchildren)

colnames(number_of_children)=unlist(attr(numberofchildren, "col.vars"))
number_of_children$tract=unlist(attr(numberofchildren, "row.vars"))

asthma=ftable(table(syntheticdataset$tract,syntheticdataset$adult.asthma))
adult_asthma=as.data.frame.matrix(asthma)

colnames(adult_asthma)=unlist(attr(asthma, "col.vars"))
adult_asthma$tract=unlist(attr(asthma, "row.vars"))

dataformap=merge(number_of_children,adult_asthma)


Harris_tract_data <- readRDS("../maps/Harris_tract_data.rds")
Harris_tract_data@data[,17:1878]=NULL

library(tigris)

dataformap$TRACTCE <- dataformap$tract
data_for_map <- geo_join(Harris_tract_data,dataformap, 'TRACTCE', 'TRACTCE', by = NULL, how = "left")

library(rgdal)

TxDOT_cities <- readOGR("TxDOT_City_Boundaries","TxDOT_City_Boundaries")
Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]
Houston_bounds <- spTransform(Houston_bounds, CRSobj = CRS(proj4string(data_for_map)))

floodplain=readOGR(dsn="FEMA_Floodplains_NFHL_2015.gdb")
floodplains <- spTransform(floodplain, CRSobj = CRS(proj4string(data_for_map)))
floodplains2=floodplains[Houston_bounds,]
#plot(floodplains)

library(rgeos)
floodplains3=gSimplify(floodplains2,0.01,TRUE)


library(leaflet)

palChild <- colorNumeric(
    palette = "YlOrRd",
    domain = data_for_map@data["Child"]
)

palAsthma <- colorNumeric(
  palette = "YlOrRd",
  domain = data_for_map@data["yes"]
)
  
  map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = data_for_map, 
                fillColor = ~palChild(data_for_map@data$Child), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2)%>%
    addPolylines(data = floodplains2,
                 weight=1
    )%>%
    addLegend(pal = palChild, 
              values = unlist(data_for_map@data$Child), 
              position = "bottomleft", 
              title = "Number of Children per Tract")
  map2
  
  map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%

    addPolygons(data = data_for_map, 
                fillColor = ~palAsthma(data_for_map@data$yes), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2)%>%
    addPolylines(data = floodplains2,
                 weight=1
    )%>%
    addLegend(pal = palAsthma, 
              values = unlist(data_for_map@data$yes), 
              position = "bottomleft", 
              title = "Simulated Adult Asthma")
  map2


  
  map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolylines(data = floodplains2 
    )
  map2

