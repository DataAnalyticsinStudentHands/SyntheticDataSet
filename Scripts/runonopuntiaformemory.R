#To run on cluster

#read in data
households=read.csv("../Inputs/household_type_for_error.csv")

#Only build Houston Households
households=subset(households,households$county==201)
#Create tract and number of households vector
tracts=as.numeric(as.character(households$tract))

sample.set=read.csv("sample_set_with_account.csv")
#Read in Parcels

validparceldataframe2=readRDS("validparceldataframe2.RDS")

#Merge households with houses
sample.set$ACCOUNT=NA

validparceldataframe2$TRACT=as.numeric(as.character(validparceldataframe2$TRACT))
sample.set$tract=as.numeric(as.character(sample.set$tract))

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

sample.set=merge(sample.set,validparceldataframe2,by="ACCOUNT",all=TRUE)
saveRDS(sample.set,"complete_sample_set.RDS")



