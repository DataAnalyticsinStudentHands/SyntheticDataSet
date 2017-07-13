library(sf)
parcels <- st_read("Parcels/Parcels.shp")
parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#st_write(validparcels,"validparcels.shp")

TXCensusTracts <- st_read("TexasCensusTractShapefiles/gz_2010_48_140_00_500k.shp")

TXCensusTracts <- st_transform(TXCensusTracts,st_crs(validparcels))

CensusTractforHCADParcels=st_within(validparcels,TXCensusTracts)
CensusTractforHCADParcelsunlisted=rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusTractforHCADParcelsunlisted=unlist(CensusTractforHCADParcelsunlisted)

validparcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]


buildingfeatures=read.table('building_res.txt',sep="\t", header=FALSE, fill=TRUE)#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

buildingfeaturescharacters=read.table('building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)
colnames(buildingfeaturescharacters)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")


#Multi-family homes are stored in building style codes 102, 103, and 104
#buildingfeatures=read.table('building_res.csv',sep="\t",header=TRUE,quote = "",stringsAsFactors = FALSE)
#extrafeatures=read.table('extra_features.txt',sep="\t",header = FALSE, fill=TRUE)
#colnames(extrafeatures)=c("ACCOUNT","BLD_NUM","COUNT","GRADE","CODE","S_DSCR","L_DESCR","CATEGORY","DESCR","NOTE","UTS")
structuralelem1=read.table('structural_elem1.txt',sep="\t",header = FALSE, fill=TRUE)
structuralelem2=read.table('structural_elem2.txt',sep="\t",header = FALSE, fill=TRUE)
structuralelem1=rbind(structuralelem1,structuralelem2)
colnames(structuralelem1)=c("ACCOUNT","BUILDING_NUMBER","CODE","ADJ_CD","STRUCTURE_TYPE","TYPE_DESCRIPTION","CATEGORY_DESCRIPTION","STATE_CLASS_CODE")

#allbuildingfeatures=merge(buildingfeatures,extrafeatures,by="ACCOUNT",all.x=TRUE,all.y=TRUE)
#allbuildingfeatures=merge(allbuildingfeatures,structuralelem1,by="ACCOUNT",all.x=TRUE,all.y=TRUE)


validparceldataframe=as.data.frame(validparcels)

buildingfeatures$HCAD_NUM=buildingfeatures$ACCOUNT

#validparceldataframe=merge(allbuildingfeatures,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")


validparceldataframe2=merge(buildingfeaturescharacters,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#nonvalidparcels=subset(parcels,parcels$valid!="Valid Geometry")
#nonvalidparcelsdataframe=as.data.frame(nonvalidparcels)
#nonvalidparcelsdataframe=merge(allbuildingfeatures,nonvalidparcelsdataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#how many houses per tract?
housespertract=as.data.frame(table(validparceldataframe2[ , c("COUNTY","TRACT")]))#"BUILDING_STYLE_CODE"
housespertract=subset(housespertract,housespertract$Freq!=0)
housespertractwithtype=as.data.frame(table(validparceldataframe2[ , c("COUNTY","TRACT","BUILDING_STYLE_CODE")]))
housespertractwithtype=subset(housespertractwithtype,housespertractwithtype$Freq!=0)
housespertractwithtype=housespertractwithtype[order(housespertractwithtype$TRACT),]
whichtractshavesofewhouses=subset(housespertract,housespertract$Freq<=100)
#doallthesetractshavepeople=read.csv("household_size.csv")
#names(doallthesetractshavepeople)[names(doallthesetractshavepeople) == "county"] <- "COUNTY"
#names(doallthesetractshavepeople)[names(doallthesetractshavepeople) == "tract"] <- "TRACT"
#doallthesetractshavepeople=merge(housespertract,doallthesetractshavepeople)

#how many buildings per tract
validparceldataframe=as.data.frame(validparcels)
buildingspertract=as.data.frame(table(validparceldataframe[ , c("COUNTY","TRACT")]))
buildingspertract=subset(housespertract,housespertract$Freq!=0)


