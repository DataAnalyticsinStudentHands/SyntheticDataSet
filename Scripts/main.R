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

source("householdsGenerator.R")
source("groupquartersFunctions.R")
source("individualFunctions.R")
source("householdFunctions.R")

# The indices can be adjusted

# The citymodel functions are used to simulate all the people living in Houston
sample.set=foreach (index = 1:786,.combine='rbind')%dopar%{
  sample.set = households_generator(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)

  sample.set$ACCOUNT=NA

  return(sample.set)
}

source("proportionCheck.R")
# This tests the proportions in the model and compares them with the Census Data to check for accuracy. The resulting data frame is saved
prop = foreach (index = 1:786,.combine='rbind')%dopar%{
  propByTract= prop_Check(sample.set, Census_data, tracts[index])
  return(propByTract)
}
saveRDS(prop, "proportionCheck.R")

# Now the people from the simulated model will be placed in households in the HCAD data
complete_sample_set=foreach (index = 1:786,.combine='rbind')%dopar%{
  tracthouses = subset(validparceldataframe2,validparceldataframe2$TRACT==tracts[index])
  tract.sample.set = subset(sample.set,sample.set$tract==tracts[index])

  # store groupquarters and household IDs from the simulated model
  group_quartersIDs = unique(subset(sample.set,sample.set$tract==tracts[index]&sample.set$household.type=="Group Quarters")$householdID)
  householdIDs = unique(subset(sample.set,sample.set$tract==tracts[index]&sample.set$household.type!="Group Quarters")$householdID)

  #store all group quarters from the HCAD data
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
  }

  if(length(groupquartersplaces$ACCOUNT)>0 & length(group_quartersIDs)>0){

    Account=sample(groupquartersplaces$ACCOUNT,length(group_quartersIDs),replace = TRUE,prob=NULL)

    for (index1 in 1:length(group_quartersIDs)){
      tract.sample.set=within.data.frame(tract.sample.set,ACCOUNT[householdID==group_quartersIDs[index1]]<-Account[index1])
    }
  }

  tract.sample.set = merge(tract.sample.set, validparceldataframe2,by="ACCOUNT",all.x=TRUE)
  tract.sample.set = tract.sample.set[tract.sample.set$geometry != "NA",]
 
  return(tract.sample.set)
}

stopCluster(cl)

source("naCheck.R")
# This tests if there are too many NAs in the model than expected. The resulting data frame is saved.
na = naTest(complete_sample_set)
saveRDS(na, "naCheck.RDS")

source("one_of.R")
# This adds a column to the model that indexes it based on factors of ten
complete_sample_set = one_of(complete_sample_set)

source("add_lat_long_sam.R")
# This adds cooordinates to the model based on the geometry column
complete_sample_set = add_lat_long(complete_sample_set)

source("typeCheck.R")
# This converts all the columns in to the model to the appropriate class (either character or numeric)
complete_sample_set = typeCheck(complete_sample_set)

saveRDS(complete_sample_set,paste0("complete_sample_set",Sys.Date(),".RDS"))
