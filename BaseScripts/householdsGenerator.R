
#' Household Generator
#'
#' This function simulates people living in households.
#'
#' It calls other functions in the citymodels package in order to simulate characteristics for households and for each individual
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param number.of.people The number of people to simulate
#' @param seed The seed to use for sampling.
#' @param inputdir The input directory for other data
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset A dataframe of simulated people living in households.

households_generator <- function(state, county, tract, seed, inputdir = "../Inputs/", Census_data){
  
  # initialize data frame
  fullset = data.frame()
  
  # set seed
  set.seed(seed)
  
  # subset data for correct Census tract
  Census_data = Census_data[(Census_data$state == state) & (Census_data$tract == tract) & (Census_data$county == county),]
  
  # Get a probability vector for their family type: Married couple, Female householder only, or Male householder only
  familyHHtypes = Census_data[18:20]
  colnames(familyHHtypes) <- c("Married-couple family", "Male householder- no wife present", "Female householder- no husband present")
  familyHHtypes = familyHHtypes/rowSums(familyHHtypes)
  
  # Find number of families of size 2-7, nonfamilies of size 1-7, and group quarters
  number.of.people = Census_data$group.quarters.population
  HHs = Census_data[5:17]
  HHs=cbind(HHs,number.of.people)
  
  # If there are people living in the tract then call the create_households() function to simulate households
  if(sum(HHs) > 0){
    fullset = do.call(rbind,sapply(2:15, function(x){
      create_households(state, county, tract, Census_data, HHs[x-1], familyHHtypes, x)
    }))
    
    # The individual ID is just the household ID with a number starting from 10000 added to the beginning
    fullset$individualID = as.numeric(sapply(1:nrow(fullset), function(i) paste((9999+i), fullset[i,]$individualID, sep="",collapse="")))
  }
  
  # return data.frame with all households built
  return(fullset)
}

create_households <- function(state, county, tract, Census_data, census_col, family_type, family_size){
  if(census_col > 0){
    house_set = data.frame()
    
    # make a seed for each household
    family_HH_seeds = sample(1000000:6000000, as.numeric(census_col), replace = FALSE)
    
    # for each seed create a household
    house_set = as.data.frame(do.call(rbind, lapply(family_HH_seeds, function(seedy){ 
      # set seed
      set.seed(seedy)
      
      
      if(family_size < 8){                 # For Families size 2-7
        # sample Household Type: Married couple, Female Housheolder only, Male Householder Only
        HHtype = sample(colnames(family_type), size = 1, prob = family_type)
        
        # Create initial data frame
        if(HHtype == "Married-couple family"){
          partofset = data.frame(household.type = rep(HHtype, family_size), householder=c("Householder", rep("Non-householder", family_size - 1)), members = c("Husband","Wife", rep("NA", family_size - 2)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Male householder- no wife present"){
          partofset = data.frame(household.type = rep(HHtype, family_size), householder=c("Householder", rep("Non-householder", family_size - 1)), members = c("Male Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Female householder- no husband present"){
          partofset = data.frame(household.type = rep(HHtype, family_size),  householder=c("Householder", rep("Non-householder", family_size - 1)), members = c("Female Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
      }
      else{                             # For Non-Families size 1-7
        if(family_size == 8){           # For Non-Families size 1
          partofset = data.frame(household.type = "Alone",  householder="Householder", members = "Householder", size = 1)
        }
        else if(family_size > 8 & family_size != 15){            # For Non-Families size 2-7
          partofset = data.frame(household.type = rep("Non-family", family_size - 7),  householder=c("Householder", rep("Non-householder", family_size - 8)), members = c("Householder", rep("NA",(family_size - 8))), size = rep(family_size - 7, family_size -7))
        }
        else{                      # For Group Quarters
          partofset = data.frame(household.type="Group Quarters",householder="Householder",members="NA",size=1) #create initial data frame
        }
      }
      
      # Begin sampling characteristics of household (functions stored in other scripts)
      # The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      # Build using Census Data
      
      # From individualfunctions.R script
      partofset = getindividualcharacteristics(partofset, seedy, Census_data)  # simulates sex, race, age, school.enrollment, education.attainment, employment, disability, nativity, citizenship, language, veteran.status, transport.method, travel.time
      
      if(family_size != 15){        # Use functions from householdfunctions.R for non group quarters
        partofset = getnumberofvehiclesforhouseholds(partofset, seedy, Census_data) # only dependent on size
        partofset = gethouseholdincome(partofset, seedy, Census_data) # independent -- samples are directly from census data
        partofset = gethouseholdhealthinsurance(partofset, seedy, Census_data) # dependent on income
      }
      else{                        # Use functions from groupquartersfunctions.R for group quarters
        partofset = getnumberofvehiclesforgroupquarters(partofset, seedy, Census_data) # depends on sex and employment
        partofset = getincomeforgroupquarters(partofset, seedy, Census_data) # independent  -- samples are directly from census data
        partofset = gethealthinsuranceforgroupquarters(partofset, seedy, Census_data) # depends on disability and age
      }
     
      partofset$state = rep(state,nrow(partofset))
      partofset$county = rep(county,nrow(partofset))
      partofset$tract = rep(tract,nrow(partofset))
      
      partofset$number.of.vehicles = as.numeric(partofset$number.of.vehicles)
      partofset$age=as.numeric(partofset$age)
      partofset$travel.time.to.work=as.numeric(partofset$travel.time.to.work)
      partofset$household.income=as.numeric(partofset$household.income)
      
      partofset$householdID = as.numeric(rep(paste(tract, seedy, sep="",collapse=""), nrow(partofset)))
      partofset$individualID = as.numeric(rep(paste(tract, seedy, sep="",collapse=""), nrow(partofset)))
      
      # Save new household with any previous households
      return(partofset)
    })))
 
    return(house_set)
  }
}
