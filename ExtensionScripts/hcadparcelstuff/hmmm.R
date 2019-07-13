library(sf)
parcels <- st_read("Parcels/Parcels.shp")
parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")

buildingfeaturescharacters=read.table('building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)
colnames(buildingfeaturescharacters)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

validparceldataframe=as.data.frame(validparcels)

buildingfeatures$HCAD_NUM=buildingfeatures$ACCOUNT

validparceldataframe2=merge(buildingfeaturescharacters,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

library(rgdal)
roadsgdb <- "Traffic/Major_Roads/Major_Roads.gdb"
major_roads <- st_read(dsn=roadsgdb,layer="Major_Roads")

TxDOT_cities <- st_read("Traffic/TxDOT_City_Boundaries","TxDOT_City_Boundaries")
Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]

Houston_boundsCRS=st_crs(Houston_bounds)
maj_roads_proj <- st_transform(major_roads, crs = Houston_boundsCRS$proj4string)
validparcelsCRS=st_crs(validparcels)
maj_roads_proj <- st_transform(maj_roads_proj, crs = validparcelsCRS$proj4string)
st_crs(maj_roads_proj)<-st_crs(validparcels)
distance=st_distance(validparcels,maj_roads_proj)
