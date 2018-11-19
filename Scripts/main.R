# Get the needed Census Data
  # This can be done by either importing the already saved RDS file for Harris County, whihc is easier:
  Census_data = readRDS("updated_Census_Data.RDS")

  # Or by calling the census_data_API() function. This option allows the user to change which state's data is retrieved. The default is Texas:
  source("getcensusdata.R")
  Census_data = census_data_API()
  
# Get the needed HCAD Parcels. It's recommended to save the RDS file of all of the valid parcels for future use because running the following script takes a long time. The needed RDS file should be automatically saved after running the script once:
source("getHCADParcels.R")
validparceldataframe2 = getHCADParcels()
validparceldataframe2$ACCOUNT = paste0(validparceldataframe2$ACCOUNT, "_", validparceldataframe2$BUILDING_NUMBER)
validparceldataframe2 = validparceldataframe2[!duplicated(validparceldataframe2$ACCOUNT), ]

# Everything else will be parallelized by the tract numbers
tracts = subset(Census_data, Census_data$county == 201)
tracts = tracts$tract

# Set Up to run in Parallel
library(doParallel)

# If running on a super computer, request 10 nodes. Otherwise adjust the number.
cl<-makeCluster(10)
registerDoParallel(cl)

source("householdGenerator.R")
source("groupquartersFunctions.R")
source("individualFunctions.R")
source("householdFunctions.R")

# The index can be adjusted
complete_sample_set=foreach (index = 1:786,.combine='rbind')%dopar%{
  # First simulate all the households in a tract
  sample.set=households_generator(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
  
  # Now the simulated households will be assigned a location the tract
  sample.set$ACCOUNT=NA

  tracthouses = subset(validparceldataframe2,validparceldataframe2$TRACT==tracts[index])
  tract.sample.set = subset(sample.set,sample.set$tract==tracts[index])

  group_quartersIDs = unique(subset(sample.set,sample.set$tract==tracts[index]&sample.set$household.type=="Group Quarters")$householdID)
  householdIDs = unique(subset(sample.set,sample.set$tract==tracts[index]&sample.set$household.type!="Group Quarters")$householdID)

  #populate group quarters
  groupquartersplaces = subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE" %in% c("660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8343","8309","8489","8311","8327","8491","8514")))
 
 #populate single family houses
  singlefamilyhouses=subset(tracthouses,tracthouses$"BUILDING_STYLE_CODE" %in% c("101","107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988"))
  Account=singlefamilyhouses$"ACCOUNT"

  if(length(Account)<length(householdIDs)){
    randomizedsinglefamilyhouseholdIDs=sample(householdIDs,nrow(singlefamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account)>=length(householdIDs)){
    randomizedsinglefamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
  }

  for (index1 in 1:length(randomizedsinglefamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedsinglefamilyhouseholdIDs[index1]]<-Account[index1])
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

  for (index1 in 1:length(randomizedtwofamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedtwofamilyhouseholdIDs[index1]]<-Account[index1])
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

  for (index1 in 1:length(randomizedthreefamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedthreefamilyhouseholdIDs[index1]]<-Account[index1])
  }

  householdIDs=householdIDs[! householdIDs %in% randomizedthreefamilyhouseholdIDs]

  #populate four family houses
  fourfamilyhouses=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE"=="104"))
  Account=rep(fourfamilyhouses$"ACCOUNT",4)

  if(length(Account)<length(householdIDs)){
    randomizedfourfamilyhouseholdIDs=sample(householdIDs,nrow(fourfamilyhouses),replace=FALSE,prob=NULL)
  }
  if(length(Account)>=length(householdIDs)){
    randomizedfourfamilyhouseholdIDs=sample(householdIDs,length(householdIDs),replace=FALSE,prob=NULL)
  }

  for (index1 in 1:length(randomizedfourfamilyhouseholdIDs)){
    tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==randomizedfourfamilyhouseholdIDs[index1]]<-Account[index1])
  }

  householdIDs=householdIDs[! householdIDs %in% randomizedfourfamilyhouseholdIDs]

  #put every other household in condos and mixed residential commercial structure

  condos=subset(tracthouses,tracthouses$"BUILDING_STYLE_CODE" %in% c("105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989"))

  if(length(condos$ACCOUNT)==0 & length(householdIDs)>0){
    tract.sample.set=tract.sample.set[!(tract.sample.set$householdID %in% householdIDs),]
    #saveRDS(householdIDs,paste0("without_locations_householdIDs",tracts[index], ".RDS"))
  }

  if(length(condos$ACCOUNT)>0 & length(householdIDs)>0){
    Account=sample((condos$"ACCOUNT"),length(householdIDs),replace=TRUE)

    for (index1 in 1:length(householdIDs)){
      tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==householdIDs[index1]]<-Account[index1])
    }
  }
  
  #put Group Quarters People in Places
  if(length(groupquartersplaces$ACCOUNT)==0 & length(group_quartersIDs)>0){
    tract.sample.set=tract.sample.set[!(tract.sample.set$householdID %in% group_quartersIDs),]
    #saveRDS(group_quartersIDs,paste0("without_locations_group_quarters_IDs",tracts[index], ".RDS"))
  }

  if(length(groupquartersplaces$ACCOUNT)>0 & length(group_quartersIDs)>0){

    Account=sample(groupquartersplaces$ACCOUNT,length(group_quartersIDs),replace = TRUE,prob=NULL)

    for (index1 in 1:length(group_quartersIDs)){
      tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==group_quartersIDs[index1]]<-Account[index1])
    }
  }

  tract.sample.set = merge(tract.sample.set, validparceldataframe2,by="ACCOUNT",all.x=TRUE)
  tract.sample.set = tract.sample.set[tract.sample.set$geometry != "NA",]
  tract.sample.set$tract=as.numeric(tract.sample.set$tract)
 
  return(tract.sample.set)
}

stopCluster(cl)
saveRDS(complete_sample_set,paste0("complete_sample_set",Sys.Date(),".RDS"))
