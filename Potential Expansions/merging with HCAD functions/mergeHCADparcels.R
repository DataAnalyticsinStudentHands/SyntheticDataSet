sample.set=readRDS("whatever_you_call_your_houston_citymodels_model.RDS")

#Read in HCAD parcels
validparceldataframe2=readRDS("validparceldataframe2.RDS")
validparceldataframe2$ACCOUNT=paste0(validparceldataframe2$ACCOUNT,"_",validparceldataframe2$BUILDING_NUMBER)
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

#Codes not used from residential files
#8309 church, 8471 Lt. Commercial Utility Build. 8454 Shell Industrial, 8353 retail store, 8378 stable, 8150 Single Wide Commercial Mobile Home
#8494 	Industrials, Light Mftg. 8490 Kennels #8428 Horse arena

#110 Unsound Residential Structure :(


#Ask if commercial mobile homes should be populated

sample.set=merge(sample.set,validparceldataframe2,by="ACCOUNT",all.x=TRUE)

saveRDS(sample.set,"complete_sample_set.RDS")