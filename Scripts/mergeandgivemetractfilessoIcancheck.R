#To run on cluster

#read in data
households=read.csv("../Inputs/household_type_for_error.csv")

#Only build Houston Households
households=subset(households,households$county==201)
#Create tract and number of households vector
tracts=as.numeric(as.character(households$tract))

sample.set=read.csv("sampleset.csv")
#Read in Parcels

validparceldataframe2=readRDS("validparceldataframe2.RDS")

#Merge households with houses
sample.set$ACCOUNT=NULL

validparceldataframe2$TRACT=as.numeric(as.character(validparceldataframe2$TRACT))
sample.set$tract=as.numeric(as.character(sample.set$tract))

complete_sample_set=data.frame()

for (number in 1:786){
  tracthouses=subset(validparceldataframe2,validparceldataframe2$TRACT==as.numeric(as.character(tracts[number])))
  tracthouseholds=subset(sample.set,sample.set$tract==as.numeric(as.character(tracts[number])))
  householdIDs=droplevels(unique(tracthouseholds$householdID))
  #populate single family houses
  singlefamilyhouses=subset(tracthouses,(tracthouses$BUILDING_STYLE_CODE=="101"|tracthouses$BUILDING_STYLE_CODE=="107"|
                                           tracthouses$BUILDING_STYLE_CODE=="108"|tracthouses$BUILDING_STYLE_CODE=="109"|
                                           tracthouses$BUILDING_STYLE_CODE=="125"|tracthouses$BUILDING_STYLE_CODE=="8351"|tracthouses$BUILDING_STYLE_CODE=="8354"))
  Account1=singlefamilyhouses$"ACCOUNT"
  
  if(length(Account1)<length(householdIDs)){
    randomizedsinglefamilyhouseholdIDs=sample(householdIDs,nrow(singlefamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account1)>=length(householdIDs)){
    randomizedsinglefamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
    
    if(length(householdIDs)>0){
      Account1=Account1[1:length(householdIDs)]
    }
    if(length(householdIDs)==0){
      Account1=character()
    }
  }
  
  #for (index in 1:length(randomizedsinglefamilyhouseholdIDs)){
   # tracthouseholds=within.data.frame(tracthouseholds,ACCOUNT[householdID==randomizedsinglefamilyhouseholdIDs[index]]<-Account[index])
  #}
  householdIDs=householdIDs[! (householdIDs %in% randomizedsinglefamilyhouseholdIDs)]
  
  #populate two family houses
  twofamilyhouses=subset(tracthouses,(tracthouses$BUILDING_STYLE_CODE=="102"))
  Account2=rep(twofamilyhouses$"ACCOUNT",2)
  
  if(length(Account2)<length(householdIDs)){
    randomizedtwofamilyhouseholdIDs=sample(householdIDs,length(Account2),replace=FALSE,prob=NULL)
  }
    
  if(length(Account2)>=length(householdIDs)){
    randomizedtwofamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
    
    if(length(householdIDs)>0){
      Account2=Account2[1:length(householdIDs)]
    }
    if(length(householdIDs)==0){
      Account2=character()
    }
  }
  
  #for (index in 1:length(randomizedtwofamilyhouseholdIDs)){
   # tracthouseholds=within.data.frame(tracthouseholds,ACCOUNT[householdID==randomizedtwofamilyhouseholdIDs[index]]<-Account[index])
  #}
  
  householdIDs=householdIDs[! householdIDs %in% randomizedtwofamilyhouseholdIDs]
  
  
  #populate three family houses
  threefamilyhouses=subset(tracthouses,(tracthouses$BUILDING_STYLE_CODE=="103"))
  Account3=rep(threefamilyhouses$"ACCOUNT",3)
  
  if(length(Account3)<length(householdIDs)){
    randomizedthreefamilyhouseholdIDs=sample(householdIDs,length(Account3),replace=FALSE,prob=NULL)
  }
  if(length(Account3)>=length(householdIDs)){
    randomizedthreefamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
    
    if(length(householdIDs)>0){
      Account3=Account3[1:length(householdIDs)]
    }
    if(length(householdIDs)==0){
      Account3=character()
    }
  }
  
  #for (index in 1:length(randomizedthreefamilyhouseholdIDs)){
   # tracthouseholds=within.data.frame(tracthouseholds,ACCOUNT[householdID==randomizedthreefamilyhouseholdIDs[index]]<-Account[index])
  #}
  
  householdIDs=householdIDs[! householdIDs %in% randomizedthreefamilyhouseholdIDs]
  
  #populate four family houses
  fourfamilyhouses=subset(tracthouses,(tracthouses$BUILDING_STYLE_CODE=="104"))
  Account4=rep(fourfamilyhouses$"ACCOUNT",4)
  
  if(length(Account4)<length(householdIDs)){
    randomizedfourfamilyhouseholdIDs=sample(householdIDs,length(Account4),replace=FALSE,prob=NULL)
  }
  if(length(Account4)>=length(householdIDs)){
    randomizedfourfamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
    
    if(length(householdIDs)>0){
      Account4=Account4[1:length(householdIDs)]
    }
    if(length(householdIDs)==0){
      Account4=character()
    }
  }
  
  #for (index in 1:length(randomizedfourfamilyhouseholdIDs)){
   # tracthouseholds=within.data.frame(tracthouseholds,ACCOUNT[householdID==randomizedfourfamilyhouseholdIDs[index]]<-Account[index])
  #}
  
  householdIDs=householdIDs[! householdIDs %in% randomizedfourfamilyhouseholdIDs]
  
  #put everyone else in condos and mixed residential commercial structures
  
  condos=subset(tracthouses,(tracthouses$BUILDING_STYLE_CODE=="106"|tracthouses$BUILDING_STYLE_CODE=="105"))
  
  if(nrow(condos)>0){
    Accountc=sample((condos$ACCOUNT),length(householdIDs),replace=TRUE)
  }
  if(nrow(condos)==0){
    Accountc=rep(NA,length(householdIDs))
  }
  
  #for (index in 1:length(householdIDs)){
   # tracthouseholds=within.data.frame(tracthouseholds,ACCOUNT[householdID==householdIDs[index]]<-Account[index])
  #}
  
  look_up_for_merge=data.frame(ACCOUNT=c(Account1,Account2,Account3,Account4,Accountc),householdID=c(as.character(randomizedsinglefamilyhouseholdIDs),as.character(randomizedtwofamilyhouseholdIDs),as.character(randomizedthreefamilyhouseholdIDs),as.character(randomizedfourfamilyhouseholdIDs),as.character(householdIDs)))
  
  tracthouseholds=merge(tracthouseholds,look_up_for_merge,by="householdID",all=TRUE)
  tracthouseholds=merge(tracthouseholds,tracthouses,by="ACCOUNT",all=TRUE)
  saveRDS(tracthouseholds,paste0("households",tracts[number],".rds"))
  
  complete_sample_set=rbind(complete_sample_set,tracthouseholds)
  
  rm(Account1,Account2,Account3,Account4,Accountc,randomizedfourfamilyhouseholdIDs,randomizedsinglefamilyhouseholdIDs,randomizedthreefamilyhouseholdIDs,randomizedtwofamilyhouseholdIDs)
}

#101 Single Family 102 2 Family 103 3 Family 104 4 Family or more, 
#105 mixed res,com res structure, 106 Condo, 
#107 Townhome, 108 Single Wide Residential home 109 Double wide residential home 125 Farm, 8354 Townhouse Inside Unit 8351 Single Family Residence


#Ask if commercial mobile homes should be populated

saveRDS(complete_sample_set,"complete_sample_set.RDS")