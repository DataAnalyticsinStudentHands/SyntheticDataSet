#start with some of the sources for the maps:
#http://cohgis-mycity.opendata.arcgis.com/datasets?page=2&t=boundaries
#http://gis-txdot.opendata.arcgis.com/datasets?q=Boundaries&sort_by=relevance
#https://www.census.gov/geo/maps-data/data/tiger-data.html for block_group gdp
#https://www.census.gov/geo/maps-data/data/tiger-line.html for 2010 Texas block groups
#http://www.h-gac.com/rds/gis-data/gis-datasets.aspx - esp. transport maps
#http://gis-txdot.opendata.arcgis.com/datasets/4480ddc1608a4ca1a6ca4da25f9fbf1b_0

#sometimes when you start, or end, you want to clear your environment: 
#rm(list=ls())

library(rgdal)

#geo databases have one little trick when you first open, so you have to list features to get layer name
#here, I did it for Texas, but leave it in case you want to use.
#this is where you find the column definitions:
#https://www2.census.gov/geo/tiger/TIGER_DP/2015ACS/Metadata/BG_METADATA_2015.txt

#fgdb <- "census_TIGER/2011_ACS_5YR_BG_48_TEXAS.gdb"
#subset(ogrDrivers(), grepl("GDB", name))
#fc_list <- ogrListLayers(fgdb)
#takes about 30 minutes for whole state - gives you a pre-joined census block level map

#Texas_tract_data <- readOGR(dsn=fgdb,layer="ACS_11_5YR_BG_48_TEXAS")
#saveRDS(Texas_tract_data,file="other data/texas_tracts_data.rds")
Texas_tract_data <- readRDS("texas_tracts_data.rds")
#COUNTYFP is in the other data folder, in US_FIPS_Codes.xls
#Harris_tract_data <- Texas_tract_data[Texas_tract_data$COUNTYFP==201,]
#FortBend_tract_data <- Texas_tract_data[Texas_tract_data$COUNTYFP==157,]
#saveRDS(Harris_tract_data, file="other data/Harris_tract_data.rds")
Harris_tract_data <- readRDS("Harris_tract_data.rds")
Harris_Fort_Bend_tracts <- Texas_tract_data[Texas_tract_data$COUNTYFP==201 | Texas_tract_data$COUNTYFP==157,]
Houston_region <- Texas_tract_data[Texas_tract_data$COUNTYFP==201 | Texas_tract_data$COUNTYFP==157,]



#from http://www.houstonstateofhealth.com
Gas_Stoves <- read.csv("SimplyAnalytics_GasStoves.csv",sep = ',',stringsAsFactors = FALSE)

#to add right length location field to Harris_tract_data
Harris_tract_data$UniqueID <- substr(Harris_tract_data$GEOID,1,11)
Gas_Stoves$FIPS
library(tigris)
Houston_Gas_Stoves <- geo_join(Harris_tract_data, Gas_Stoves, 'UniqueID', 'FIPS', by = NULL, how = "left")



#STSElemIV <- merge(HISD_elementary[district4,],unresolved,by="Facility",duplicateGeoms=TRUE)
#library(geosphere)
#STSElemIV$centroids <- centroid(STSElemIV) #from geosphere
library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
sf <- ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_polygon(data = Houston_Gas_Stoves, #[!is.na(vis_unresolved_2015$mean_not_resolved),], 
               aes(x=long, y = lat, colour = "tomato", fill = "Indicator.Value", 
                   group = group)) +
  #geom_text(aes(
    #label = STSElemIV$Facility, 
    #label = vis_unresolved_2015$mean_not_resolved, 
    #x=STSElemIV$centroids[,1],  
    #y=STSElemIV$centroids[,2]),
    #check_overlap = TRUE,size=2.7) + 
  ggtitle('Gas Stoves') +
  #theme_light() +
  # geom_label() +
  coord_fixed(1.3)

plot(sf)


library(leaflet)
#map <- leaflet(data = Houston_Diabetes_FiveHundred_Cities) %>%
#  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
#  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Houston_Gas_Stoves$GEOID, "<br>", 
                "% Gas Stoves ", 
                Houston_Gas_Stoves$Lower.Confidence.Interval, "<",
                Houston_Gas_Stoves$X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015, ">",
                Houston_Gas_Stoves$Upper.Confidence.Interval)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = Houston_Gas_Stoves$X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015
)

map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Houston_Gas_Stoves, 
              fillColor = ~pal(X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Houston_Gas_Stoves$X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015, 
            position = "bottomright", 
            title = "Percent of<br>Gas Stoves",
            labFormat = labelFormat(suffix = "%"))
             
map2

#asthma example:
#https://www.cdc.gov/asthma/stateprofiles/asthma_in_tx.pdf (2008 data)
#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=9&ved=0ahUKEwjT-sjBw7fVAhUKOiYKHb6KC9QQFghbMAg&url=https%3A%2F%2Fwww.dshs.texas.gov%2Fasthma%2Fpdf%2F2014BurdenRpt.doc&usg=AFQjCNEL4CRLzT7AVQWozFab9Guqr3pXNw (2014)
#https://www.cdc.gov/asthma/brfss/2014/tableC1.htm#modalIdString_CDCTable_0 (2014, with CI)

saveRDS(Houston_Gas_Stoves,"Houston_Gas_Stoves.RDS")
