# Get the needed Census Data
  # This can be done by either importing the already saved RDS file for Harris County, which is easier:
  Census_data = readRDS("/Data/Census_data.RDS")

  # Or by calling the census_data_API() function. This option allows the user to change which state's data is retrieved or which year is used. The default is 2014 data for Texas:
  source("getcensusdata.R")
  Census_data = census_data_API()
  
# Get the needed HCAD Parcels. It's recommended to save the RDS file of all of the valid parcels for future use because running the following script takes a long time. The needed RDS file should be automatically saved after running the script once:
# Make sure the required files downlaoded and in the proper directory before running the script. Some files may have to be downloaded from http://pdata.hcad.org/GIS/index.html and http://pdata.hcad.org/download/2014.html . Check the script for more details.
source("getHCADParcels.R")
validparceldataframe2 = getHCADParcels()
validparceldataframe2$ACCOUNT = paste0(validparceldataframe2$ACCOUNT, "_", validparceldataframe2$BUILDING_NUMBER)
validparceldataframe2 = validparceldataframe2[!duplicated(validparceldataframe2$ACCOUNT), ]

# From the buildings in the tract, subset for only the ones with living spaces
potential_buildings=subset(validparceldataframe2,validparceldataframe2$"BUILDING_STYLE_CODE" %in% c("101","102", "103", "104", "107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988","105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989", "660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8309","8489","8311","8491","8514"))

# Depending on the USE_CODE some buildings will only have one unit while others will have multiple units. If a building doesn't list units then use the average number of units for that building's USE_CODE
potential_buildings$UNITS[potential_buildings$USE_CODE %in% c("1D1", "A1", "A2", "A3", "A4", "B2", "B3", "B4", "C1", "C2", "C3", "E1", "M3", "O1", "O2", "U0", "X4", "XG", "XI", "XL", "Z4") | is.na(potential_buildings$UNITS)]= 1
for(x in c("B1", "F1", "F2", "X1", "X2", "X3", "XE", "XJ", "XU")){
   avg_units = round(mean(as.integer(potential_buildings[potential_buildings$USE_CODE == x & !potential_buildings$UNITS %in% c("", "A", "B", "C", "D", "1/2", "1/1"),]$UNITS), na.rm = T))
   potential_buildings$UNITS[potential_buildings$USE_CODE == x & potential_buildings$UNITS %in% c("", "A", "B", "C", "D", "1/1", "1/2")] = avg_units
}

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
saveRDS(prop, "proportionCheck.RDS")

# Now the people from the simulated model will be placed in households in the HCAD data
complete_sample_set=foreach (index = 1:786,.combine='rbind')%dopar%{
  # Subset for the proper buildings and individuals based on tract
  tracthouses=subset(potential_buildings, potential_buildings$TRACT == tracts[index])
  tract.sample.set = subset(sample.set, sample.set$tract == tracts[index])

  # store groupquarters and household IDs from the simulated model
  group_quartersIDs = unique(subset(sample.set,sample.set$tract==tracts[index]&sample.set$household.type=="Group Quarters")$householdID)
  householdIDs = unique(subset(sample.set,sample.set$tract==tracts[index]&sample.set$household.type!="Group Quarters")$householdID)

  # From the potential buildings in the tract, subset for only buildings not used for group quarters
  houses = tracthouses[tracthouses$BUILDING_STYLE_CODE %in% c("101","102", "103", "104", "107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988","105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989" ),]
  
  # If there are no households available in this tract then remove the household IDs from the tract. Otherwise place people in houses until there are either no more locations or no more individuals
  if(length(houses$ACCOUNT) == 0 & length(householdIDs) > 0){
    tract.sample.set = tract.sample.set[!(tract.sample.set$householdID %in% householdIDs),]
  }else{
    # Building accounts can be repeated if they have have more than 1 units available
    all.accounts = unlist(lapply(houses$ACCOUNT, function(x){
      rep(x, houses$UNITS[which(houses$ACCOUNT == x)])
    }))

    # Temporarily add a unique number to the end of the accounts to differentiate between any repeated accounts. This is done so that the accounts can be properly sampled from.
    all.accounts = paste0(all.accounts, "-", c(1:length(all.accounts)))
    
    # max_length determines how many accounts will be chosen when sampling based on whether there are more buildings available or more individuals in the tract
    max_length = ifelse(length(all.accounts) < length(householdIDs), length(all.accounts), length(householdIDs))
    
    # Sample from the list of available building accounts until 
    Account = sample((all.accounts), max_length, replace = FALSE)
    
    # Remove the unique number that was added to the end of the accounts so they are in their original formats. This is done so that the HCAD data will properly merge with the model
    Account = gsub("-.*", "", Account)

    # Assign all the building accounts to individuals in the tract
    for (index1 in 1:max_length){
      tract.sample.set = within.data.frame(tract.sample.set,ACCOUNT[householdID == householdIDs[index1]] <- Account[index1])
    }
  }
  
  # From the potential buildings, subset for only buildings used for group quarters
  groupquartersplaces=subset(tracthouses,(tracthouses$"BUILDING_STYLE_CODE" %in% c("660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8309","8489","8311","8491","8514")))
 
  # Repeat the same steps as before but with groupquartersplaces and group_quartersIDs
  if(length(groupquartersplaces$ACCOUNT) == 0 & length(group_quartersIDs) > 0){
    tract.sample.set=tract.sample.set[!(tract.sample.set$householdID %in% group_quartersIDs),]
  }else{

    all.accounts = unlist(lapply(groupquartersplaces$ACCOUNT, function(x){
      rep(x, groupquartersplaces$UNITS[which(groupquartersplaces$ACCOUNT == x)])
    }))

    all.accounts = paste0(all.accounts, "-", c(1:length(all.accounts)))
    max_length = ifelse(length(all.accounts) < length(group_quartersIDs), length(all.accounts), length(group_quartersIDs))

    Account=sample((all.accounts),(max_length),replace=FALSE)

    Account = gsub("-.*", "", Account)

    for (index1 in 1:max_length){
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
# This numbers the rows in the model by powers of ten
complete_sample_set = one_of(complete_sample_set)

source("add_lat_long_sam.R")
# This adds cooordinates to the model based on the geometry column
complete_sample_set = add_lat_long(complete_sample_set)

source("typeCheck.R")
# This converts all the columns in to the model to the appropriate class (either character or numeric)
complete_sample_set = typeCheck(complete_sample_set)

saveRDS(complete_sample_set,paste0("complete_sample_set",Sys.Date(),".RDS"))
