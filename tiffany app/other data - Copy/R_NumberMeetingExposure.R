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

#TX_non_attainment <- readOGR("Traffic/Texas_NonAttainment_Areas","Texas_NonAttainment_Areas")
#Houston_area_non_attain <- TX_non_attainment[TX_non_attainment$REGION==12,]
#Houston_area_proj <- spTransform(Houston_area_non_attain, CRSobj = CRS(proj4string(Texas_tract_data)))
#Tx_tracts_non_attain <- Texas_tract_data[Houston_area_proj[Houston_area_proj$REGION==12,],]


#https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc_Ch3.pdf
#Tabblock_2010_48 <- readOGR("census_TIGER/tabblock2010_48_pophu","tabblock2010_48_pophu")
#Harris_tabblock_2010 <- Tabblock_2010_48[Tabblock_2010_48$COUNTYFP10==201,]
#there are some merges that are easier with block ids this way
#https://www.census.gov/geo/reference/geoidentifiers.html

#https://www.census.gov/geo/reference/gtc/gtc_cbsa.html (core-based-statistical-area)
#DP are the demographic profile ones
#CBSA_2010 <- readOGR("census_TIGER/CBSA_2010Census_DP1","CBSA_2010Census_DP1")
#CBSA_2010[CBSA_2010$NAMELSAD10=='Houston-Sugar Land-Baytown, TX Metro Area',]
#CBSA_2010[CBSA_2010$DP0010001>1000000,]$NAMELSAD10 #names of all areas over one million

#Census_Place_2010 <- readOGR("census_TIGER/Place_2010Census_DP1","Place_2010Census_DP1")
#Census_Place_2010[Census_Place_2010$NAMELSAD10=="Houston city",]
#Census_Place_2010[Census_Place_2010$GEOID10==4835000,] #Houston

#roadsgdb <- "Traffic/Major_Roads/Major_Roads.gdb"
#major_roads <- readOGR(dsn=roadsgdb,layer="Major_Roads")

#TxDOT_cities <- readOGR("Traffic/TxDOT_City_Boundaries","TxDOT_City_Boundaries")
#Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]

#maj_roads_proj <- spTransform(major_roads, CRSobj = CRS(proj4string(Houston_bounds)))
#plot(Houston_bounds[maj_roads_proj,])
#plot(maj_roads_proj[Houston_bounds,])

#from http://www.houstonstateofhealth.com
adult_threshold <- read.csv("PeopleMeetingThreshold.csv",sep = ',',stringsAsFactors = FALSE)

#to add right length location field to Harris_tract_data
Harris_tract_data$Location10 <- substr(Harris_tract_data$GEOID,1,10)
#adult_diabetes$Location10 <- substr(adult_diabetes$Location,1,10)
library(tigris)
Houston_Adults_Threshold <- geo_join(Harris_tract_data, adult_threshold, 'TRACTCE', 'tract', by = NULL, how = "left")

#taking percent women who have diabetes


#can also do things with HISD - this is a vision project I'm working on
#HISD_elementary <- readOGR("HISD/Attendance_Boundaries_Elementary_1617","Attendance_Boundaries_Elementary_1617")
#school names are HISD_middle$Facility
#HISD_middle <- readOGR("HISD/Attendance_Boundaries_Middle_1617","Attendance_Boundaries_Middle_1617")
#school names are HISD_middle$Middle_Sch
#HISD_high <- readOGR("HISD/Attendance_Boundaries_High_1617","Attendance_Boundaries_High_1617")
#school names are HISD_high$High_Schoo
#HISD_districts <- readOGR("HISD/Board_Districts_September2014","Board_Districts_September2014")
#district4 <- HISD_districts[HISD_districts$NAME2=='IV',]
#when you get the Error: identicalCRS(x, y) is not TRUE
#district4 <- spTransform(district4, CRSobj = CRS(proj4string(HISD_elementary)))
#plot(HISD_elementary[district4,])
#unresolved <- read.csv("data/District IV unresolved 1213 thru 1516.csv",sep = ',',stringsAsFactors = FALSE)
#unresolved$Facility <- gsub("([A-Za-z]+).*", "\\1", unresolved$School.Name) #just get first
#STSElem <- merge(HISD_elementary,unresolved,by="Facility",duplicateGeoms=TRUE)

#STSElemIV <- merge(HISD_elementary[district4,],unresolved,by="Facility",duplicateGeoms=TRUE)
#library(geosphere)
#STSElemIV$centroids <- centroid(STSElemIV) #from geosphere
library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
sf <- ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_polygon(data = Houston_Gas_Stove_Exposure, #[!is.na(vis_unresolved_2015$mean_not_resolved),], 
               aes(x=long, y = lat, colour = "tomato", fill = "Indicator.Value", 
                   group = group)) +
  #geom_text(aes(
    #label = STSElemIV$Facility, 
    #label = vis_unresolved_2015$mean_not_resolved, 
    #x=STSElemIV$centroids[,1],  
    #y=STSElemIV$centroids[,2]),
    #check_overlap = TRUE,size=2.7) + 
  ggtitle('AdultDiabetes') +
  #theme_light() +
  # geom_label() +
  coord_fixed(1.3)

plot(sf)


library(leaflet)
#map <- leaflet(data = Houston_adult_diabetes) %>%
#  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
#  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Houston_Adults_Threshold$GEOID, "<br>", 
                "Adults with Diabetes ")
               
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = Houston_Adults_Threshold$peopleatthreshold
  
)


map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Houston_Adults_Threshold, 
              fillColor = ~pal(peopleatthreshold), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Houston_Adults_Threshold$peopleatthreshold, 
            position = "bottomright", 
            title = "Number Meeting<br>Higher Exposure<br>Levels")
            
map2

#asthma example:
#https://www.cdc.gov/asthma/stateprofiles/asthma_in_tx.pdf (2008 data)
#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=9&ved=0ahUKEwjT-sjBw7fVAhUKOiYKHb6KC9QQFghbMAg&url=https%3A%2F%2Fwww.dshs.texas.gov%2Fasthma%2Fpdf%2F2014BurdenRpt.doc&usg=AFQjCNEL4CRLzT7AVQWozFab9Guqr3pXNw (2014)
#https://www.cdc.gov/asthma/brfss/2014/tableC1.htm#modalIdString_CDCTable_0 (2014, with CI)

number_adults_with_diabetes=read.csv("diabetes.csv")

#to add right length location field to Harris_tract_data
Harris_tract_data$Location10 <- substr(Harris_tract_data$GEOID,1,10)
#adult_diabetes$Location10 <- substr(adult_diabetes$Location,1,10)
number_adults_with_diabetes$tract=substr(number_adults_with_diabetes$FIPS,6,11)

Houston_Diabetes <- geo_join(Harris_tract_data, number_adults_with_diabetes, 'TRACTCE', 'tract', by = NULL, how = "left")


popup <- paste0("GEOID: ", Houston_Diabetes$GEOID, "<br>", 
                "Adults with Diabetes ")

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = Houston_Diabetes$X..Diabetes..2015
  
)


map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Houston_Diabetes, 
              fillColor = ~pal(Houston_Diabetes$X..Diabetes..2015), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Houston_Diabetes$X..Diabetes..2015, 
            position = "bottomright", 
            title = "Number of adults with Diabetes")

map2

saveRDS(Houston_Diabetes,"Houston_Diabetes.RDS")
saveRDS(Houston_Adults_Threshold,"Houston_Adults_Threshold.RDS")