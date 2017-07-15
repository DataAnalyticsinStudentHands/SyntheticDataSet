#Generate appropriate number of households per tract

#read in data
households=read.csv("../Inputs/household_type_for_error.csv")
#get total number of households per tract
households$total=rowSums(households[4:8])

#Simulate Houston people

source("masterfunction.R")#function that builds households

#Only build Houston Households
households=subset(households,households$county==201)
#Create tract and number of households vector
tracts=households$tract
number.of.households=households$total

#Set Up to run in Parallel
library(doParallel)
library(foreach)
cl<-makeCluster(10)
registerDoParallel(cl)

#Simulate Households
sample.set=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  sample=master(201,tracts[index],number.of.households[index],seed=1)
  return(sample)
}

stopCluster(cl)

#Write households to csv
return(sample.set)
write.csv(sample.set)

#Merge with HCAD data

#Read in Parcels

library(sf)
parcels <- st_read("../hcadparcelstuff/Parcels/Parcels.shp")

parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#Read in Texas Census Tract Files
TXCensusTracts <- st_read("../hcadparcelstuff/TexasCensusTractShapefiles/gz_2010_48_140_00_500k.shp")
#Put them in same CRS as Parcels
TXCensusTracts <- st_transform(TXCensusTracts,st_crs(validparcels))

#Get Census Tracts for each parcel
CensusTractforHCADParcels=st_within(validparcels,TXCensusTracts)
CensusTractforHCADParcelsunlisted=rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusTractforHCADParcelsunlisted=unlist(CensusTractforHCADParcelsunlisted)

validparcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

#Merge with building features for residential houses

#Read in data
buildingfeatures=read.table('../hcadparcelstuff/building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

validparceldataframe=as.data.frame(validparcels)

validparceldataframe2=merge(buildingfeatures,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#Merge households with houses
sample.set$ACCOUNT=NA

for (tract in tracts){
  tracthouses=subset(validparceldataframe2,validparceldataframe2$TRACT==tract)
  householdIDs=unique(subset(sample.set,sample.set$tract==tract)$householdID)
  
  #populate single family houses
  singlefamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="101"|tracthouses$"BUILDING_STYLE_CODE"=="107"|tracthouses$"BUILDING_STYLE_CODE"=="108"|tracthouses$"BUILDING_STYLE_CODE"=="109"|tracthouses$"BUILDING_STYLE_CODE"=="125"|tracthouses$"BUILDING_STYLE_CODE"=="8351"|tracthouses$"BUILDING_STYLE_CODE"=="8354"))
  Account=singlefamilyhouses$"ACCOUNT"
  
  randomizedsinglefamilyhouseholdIDs=ifelse(length(Account)<length(householdIDs),sample(householdIDs,nrow(singlefamilyhouses),replace=FALSE,prob=NULL),sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL))
  
  for (index in 1:length(randomizedsinglefamilyhouseholdIDs)){
    sample.set=within.data.frame(sample.set,ACCOUNT[householdID==randomizedsinglefamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedsinglefamilyhouseholdIDs]
  
  #populate two family houses
  twofamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="102"))
  Account=rep(twofamilyhouses$"ACCOUNT",2)
  
  randomizedtwofamilyhouseholdIDs=ifelse(length(Account)<length(householdIDs),sample(householdIDs,nrow(twofamilyhouses),replace=FALSE,prob=NULL),sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL))
  
  for (index in 1:length(randomizedtwofamilyhouseholdIDs)){
    sample.set=within.data.frame(sample.set,ACCOUNT[householdID==randomizedtwofamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedtwofamilyhouseholdIDs]
  
  
  #populate three family houses
  threefamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="103"))
  Account=rep(threefamilyhouses$"ACCOUNT",3)
  
  randomizedthreefamilyhouseholdIDs=ifelse(length(Account)<length(householdIDs),sample(householdIDs,nrow(threefamilyhouses),replace=FALSE,prob=NULL),sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL))
  
  for (index in 1:length(randomizedthreefamilyhouseholdIDs)){
    sample.set=within.data.frame(sample.set,ACCOUNT[householdID==randomizedthreefamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedthreefamilyhouseholdIDs]
  
  #populate four family houses
  fourfamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="104"))
  Account=rep(threefamilyhouses$"ACCOUNT",4)
  
  randomizedfourfamilyhouseholdIDs=ifelse(length(Account)<length(householdIDs),sample(householdIDs,nrow(fourfamilyhouses),replace=FALSE,prob=NULL),sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL))
  
  for (index in 1:length(randomizedfourfamilyhouseholdIDs)){
    sample.set=within.data.frame(sample.set,ACCOUNT[householdID==randomizedfourfamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedthreefamilyhouseholdIDs]
  
  #put everyone else in condos and mixed residential commercial structures
  
  condos=subset(tracthouses,(tracthouses$"Building_Style_Code"=="106"|tracthouses$"Building_Style_Code"=="105"))
  Account=ifelse((nrow(condos)>0),sample((condos$"ACCOUNT"),length(householdIDs),replace=TRUE),rep(NA,length(householdIDs)))
  
  for (index in 1:length(householdIDs)){
    sample.set=within.data.frame(sample.set,ACCOUNT[householdID==householdIDs[index]]<-Account[index])
  }
}

#101 Single Family 102 2 Family 103 3 Family 104 4 Family or more, 
#105 mixed res,com res structure, 106 Condo, 
#107 Townhome, 108 Single Wide Residential home 109 Double wide residential home 125 Farm, 8354 Townhouse Inside Unit 8351 Single Family Residence


#Ask if commercial mobile homes should be populated

sample.set=merge(sample.set,validparceldataframe2,by="ACCOUNT",all.x=TRUE)
write.csv(sample.set,"complete_sample_set.csv")




