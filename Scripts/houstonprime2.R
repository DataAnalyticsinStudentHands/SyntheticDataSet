
#Simulate Houston people

source("household_generator2.R")#function that builds households


#Set Up to run in Parallel
library(doParallel)
library(foreach)
cl<-makeCluster(10)
registerDoParallel(cl)

Census_data_List=readRDS("Census_Data_List.RDS")

#Simulate Households
sample.set=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  sample=household_generator(201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data_List)
  return(sample)
}

stopCluster(cl)

number.of.group.quarters.members=read.csv("../Inputs/group_quarters.csv")
number.of.group.quarters.members=subset(number.of.group.quarters.members,number.of.group.quarters.members$county==201)
number.of.group.quarters.members=number.of.group.quarters.members[4]
tracts=number.of.group.quarters.members$tract

cl<-makeCluster(10)
registerDoParallel(cl)

#Simulate Group Quarters
source("group_quarters_generator.R")
sample.set2=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  sample=group_quarters_simulater(201,tracts[index],number.of.group.quarters.members[index],seed=1,inputdir = "../Inputs/",Census_data_List)
  return(sample)
}

stopCluster(cl)

sample.set=rbind(sample.set,sample.set2)

#Write households to csv

#write.csv(sample.set)
saveRDS(sample.set,"new_sampleset.RDS")
#Merge with HCAD data

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


#Ask if commercial mobile homes should be populated

sample.set=merge(sample.set,validparceldataframe2,by="ACCOUNT",all=TRUE)
saveRDS(sample.set,"new_complete_sample_set.RDS")




