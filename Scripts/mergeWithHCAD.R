#' Merge HCAD data with simulated model
#'
#' This function assigns a random location to each household in SAM
#'
#' The function filters the available buildings and the people in Sam for those in the specified tract and splits them into two groups: houshold and group quarter.
#' The assignAccount function is called to randomly choose a building and assign it's account to someone in Sam until there are nor more buildings or people left
#'
#' @param tract The tract number
#' @param availableBuildings A dataframe of available living locations
#' @param Sam A dataframe of simulated people
#' @return tract.sample.set A dataframe of simulated people with households.

mergeWithHCAD<-function(tract, availableBuildings, Sam){
  # Subset for the proper buildings and individuals based on tract
  tracthouses = subset(availableBuildings, availableBuildings$TRACT == tract)
  tract.sample.set = Sam[Sam$tract == tract,]

  # store groupquarters and household IDs from the simulated model
  group_quartersIDs = unique(subset(tract.sample.set, tract.sample.set$household.type == "Group Quarters")$householdID)
  householdIDs = unique(subset(tract.sample.set, tract.sample.set$household.type != "Group Quarters")$householdID)

  # List of BUILDING_STYLE_CODES for households and group quarters
  householdsCodes = c("101","102", "103", "104", "107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988","105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989" )
  groupquartersCodes = c("660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8309","8489","8311","8491","8514")

  # Call assignAccounts to place people in buildings
  tract.sample.set = assignAccounts(householdsCodes, householdIDs, tracthouses, tract.sample.set, 1)
  tract.sample.set = assignAccounts(groupquartersCodes, group_quartersIDs, tracthouses, tract.sample.set, 1)
  
  tract.sample.set = merge(tract.sample.set, tracthouses, by="ACCOUNT", all.x=TRUE)
  tract.sample.set = tract.sample.set[tract.sample.set$geometry != "NA",]
 
  return(tract.sample.set)
}

assignAccounts<-function(buildingType, idType, buildings, sampleSet, seed){
  # Set the seed so that the results are always reproducible
  set.seed(seed)
  
  # From the potential buildings in the tract, subset for only the buildings in buildingType
  homes = buildings[buildings$BUILDING_STYLE_CODE %in% buildingType,]

  # If there are no households available in this tract then remove the household IDs from the tract
  # Otherwise place people in houses until there are either no more locations or no more individuals
  if(length(homes$ACCOUNT) == 0 & length(idType) > 0){
    sampleSet=sampleSet[!(sampleSet$householdID %in% idType),]
  }else{
      # Building accounts can be repeated if they have have more than 1 units available
      all.accounts = unlist(lapply(homes$ACCOUNT, function(x){
      rep(x, homes$UNITS[which(homes$ACCOUNT == x)])
    }))
    
    # Building accounts can be repeated if they have have more than 1 units available
    all.accounts = paste0(all.accounts, "-", c(1:length(all.accounts)))
    # max_length determines how many accounts will be chosen when sampling based on whether there are more buildings available or more individuals in the tract
    max_length = ifelse(length(all.accounts) < length(idType), length(all.accounts), length(idType))
    
    # Sample from the list of available building accounts until 
    Account=sample((all.accounts),max_length,replace=FALSE)
    # Remove the unique number that was added to the end of the accounts so they are in their original formats. This is done so that the HCAD data will properly merge with the model
    Account = gsub("-.*", "", Account)
    
    # Assign all the building accounts to individuals in the tract
    for (index in 1:max_length){
      sampleSet=within.data.frame(sampleSet,ACCOUNT[householdID==idType[index]]<-Account[index])
    }
  }
  
  return(sampleSet)
}
