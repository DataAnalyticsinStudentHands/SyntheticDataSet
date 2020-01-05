#https://data.femadata.com/NationalDisasters/HurricaneHarvey/Data/Buildings/Outlines/
#https://data.femadata.com/NationalDisasters/HurricaneHarvey/Data/Buildings/Address_Points/
#TX_StructuresPrelim.zip downloaded on 4/21/2019


library(sf)
finalFEMA <- st_read("/Users/dan/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/Harvey\ Flooding/Final.gdb/")
#has shapefiles for building damage, but lots of N/A for others

Harvey9_02 <- st_read("/Users/dan/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/Harvey\ Flooding/FEMA_Damage_Assessments_Harvey_Public_09_02.gdb/")
library(dplyr)
Harvey9_02_Harris <- Harvey9_03 %>% filter(COUNTY == "Harris")


library(geojsonio)
FemaFloodingFull <- file_to_geojson(input=Harvey9_03,
                                    method="local",output="FEMA_Flood_full.geojson")
