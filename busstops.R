library(httr)
busstops_api <- GET("https://api.ridemetro.org/data/FullRouteInfo[?34]")
bus_all <- GET("https://api.ridemetro.org/data/Routes?subscription-key=53f1db25ee3f34058c6da3b8")

#walkability
smartlocations <- st_read("~/Downloads/SmartLocationDatabaseV3/SmartLocationDatabase.gdb")
smartDT <- as.data.table(smartlocations)
smartDT[,Shape:=NULL]
write_csv2(smartDT[STATEFP=="48"&COUNTYFP=="201"],"~/Downloads/smartlocations48201.csv")
st_write(smartDT[STATEFP=="48"&COUNTYFP=="201"],"~/Downloads/smartlocations48201.geojson",driver = "GeoJSON")