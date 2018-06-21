group_quarters=readRDS("citymodels_houston_group_quarters.RDS")
households=readRDS("citymodels_houston_households.RDS")
households$size=as.character(households$size)
sample.set=rbind(group_quarters,households)

#Read in HCAD parcels
validparceldataframe2=readRDS("valid_parcels_for_simulation.RDS")
validparceldataframe2$ACCOUNT=paste0(validparceldataframe2$ACCOUNT,"_",validparceldataframe2$BUILDING_NUMBER)
validparceldataframe2=validparceldataframe2[!duplicated(validparceldataframe2$ACCOUNT), ]
#Merge households with houses
sample.set$ACCOUNT=NA

tracts=unique(sample.set$tract)


#Set Up to run in Parallel
library(doParallel)
library(foreach)
cl<-makeCluster(10)
registerDoParallel(cl)

complete_sample_set=foreach (index1=1:length(tracts),.combine='rbind')%dopar%{
  tracthouses=subset(validparceldataframe2,validparceldataframe2$TRACT==tracts[index1])
  tract.sample.set=subset(sample.set,sample.set$tract==tracts[index1])
  
  group_quartersIDs=unique(subset(sample.set,sample.set$tract==tracts[index1]&sample.set$household.type=="Group Quarters")$householdID)
  householdIDs=unique(subset(sample.set,sample.set$tract==tracts[index1]&sample.set$household.type!="Group Quarters")$householdID)
  
  #populate group quarters
  groupquartersplaces=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE" %in% c("660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8343","8309","8489","8311","8327","8491","8514")))
  #populate single family houses
  singlefamilyhouses=subset(tracthouses,tracthouses$"BUILDING_STYLE_CODE" %in% c("101","107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988"))

  Account=singlefamilyhouses$"ACCOUNT"
  
  if(length(Account)<length(householdIDs)){
    randomizedsinglefamilyhouseholdIDs=sample(householdIDs,nrow(singlefamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account)>=length(householdIDs)){
    randomizedsinglefamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
  }
  
  for (index in 1:length(randomizedsinglefamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedsinglefamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedsinglefamilyhouseholdIDs]
  
  #populate two family houses
  twofamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="102"))
  Account=rep(twofamilyhouses$"ACCOUNT",2)
  
  if(length(Account)<length(householdIDs)){
    randomizedtwofamilyhouseholdIDs=sample(householdIDs,nrow(twofamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account)>=length(householdIDs)){
    randomizedtwofamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
  }
  
  for (index in 1:length(randomizedtwofamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedtwofamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedtwofamilyhouseholdIDs]
  
  
  #populate three family houses
  threefamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="103"))
  Account=rep(threefamilyhouses$"ACCOUNT",3)
  
  if(length(Account)<length(householdIDs)){
    randomizedthreefamilyhouseholdIDs=sample(householdIDs,nrow(threefamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account)>=length(householdIDs)){
    randomizedthreefamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
  }
  
  for (index in 1:length(randomizedthreefamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedthreefamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedthreefamilyhouseholdIDs]
  
  #populate four family houses
  fourfamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="104"))
  Account=rep(threefamilyhouses$"ACCOUNT",4)
  
  if(length(Account)<length(householdIDs)){
    randomizedfourfamilyhouseholdIDs=sample(householdIDs,nrow(fourfamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account)>=length(householdIDs)){
    randomizedfourfamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
  }
  
  for (index in 1:length(randomizedfourfamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedfourfamilyhouseholdIDs[index]]<-Account[index])
  }
  
  householdIDs=householdIDs[! householdIDs %in% randomizedfourfamilyhouseholdIDs]

  #put everyother household in condos and mixed residential commercial structure
  
  condos=subset(tracthouses,tracthouses$"BUILDING_STYLE_CODE" %in% c("105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989"))
  
  if(length(condos$ACCOUNT)==0 & length(householdIDs)>0){
    saveRDS(householdIDs,paste0("without_locations_householdIDs",tracts[index1]))
  }
  
  if(length(condos$ACCOUNT)>0 & length(householdIDs)>0){
    Account=sample((condos$"ACCOUNT"),length(householdIDs),replace=TRUE)
    
    for (index in 1:length(householdIDs)){
      tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==householdIDs[index]]<-Account[index])
    }
  }
  #put Group Quarters People in Places
  
  if(length(groupquartersplaces$ACCOUNT)==0 & length(group_quartersIDs)>0){
    saveRDS(group_quartersIDs,paste0("without_locations_group_quarters_IDs",tracts[index1]))
  }
  
  if(length(groupquartersplaces$ACCOUNT)>0 & length(group_quartersIDs)>0){
    
    Account=sample(groupquartersplaces$ACCOUNT,length(group_quartersIDs),replace = TRUE,prob=NULL)
    
    for (index in 1:length(group_quartersIDs)){
      tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==group_quartersIDs[index]]<-Account[index])
    }
  }
  
  saveRDS(tract.sample.set,paste0("tract_that_has_ACCOUNT_numbers_",tracts[index1]))
  return(tract.sample.set)
}

#101 Single Family 102 2 Family 103 3 Family 104 4 Family or more, 
#105 mixed res,com res structure, 106 Condo, 
#107 Townhome, 108 Single Wide Residential home 109 Double wide residential home 125 Farm, 8354 Townhouse Inside Unit 8351 Single Family Residence

#Codes not used from residential files
#8309 church, 8471 Lt. Commercial Utility Build. 8454 Shell Industrial, 8353 retail store, 8378 stable, 8150 Single Wide Commercial Mobile Home
#8494 	Industrials, Light Mftg. 8490 Kennels #8428 Horse arena

#110 Unsound Residential Structure :(


#Ask if commercial mobile homes should be populated

complete_sample_set=merge(complete_sample_set,validparceldataframe2,by="ACCOUNT",all.x=TRUE)

saveRDS(complete_sample_set,paste0("complete_sample_set",Sys.Date(),".RDS"))