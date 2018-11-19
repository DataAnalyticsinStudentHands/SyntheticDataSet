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

household_generator <- function(state, county, tract, seed, inputdir = "../Inputs/", Census_data){

  # initialize data frame
  fullset = data.frame()

  # set seed
  set.seed(seed)

  # subset data for correct Census tract
  Census_data = Census_data[(Census_data$state == state) & (Census_data$tract == tract) & (Census_data$county == county),]

  # Get a probability vector for their type
  familyHHtypes = Census_data[18:20]
  colnames(familyHHtypes) <- c("Married-couple family", "Male householder- no wife present", "Female householder- no husband present")
  familyHHtypes = familyHHtypes/rowSums(familyHHtypes)

  # load and organize 500 Cities project data

  # Create family, nonfamily, and group quarters households
  number.of.people = Census_data$group.quarters.population
  HHs = Census_data[5:17]
  HHs=cbind(HHs,number.of.people)

  if(sum(HHs) > 0){
    fullset = do.call(rbind,sapply(2:15, function(x){
      create_household(state, county, tract, Census_data, HHs[x-1], familyHHtypes, x)
    }))

    fullset$individualID = format(as.numeric(sapply(1:nrow(fullset), function(i) paste((9999+i), fullset[i,]$individualID, sep="",collapse=""))), digits =22)
  }

  # return data.frame with all households built
  return(fullset)
}

create_household <- function(state, county, tract, Census_data, census_col, family_type, family_size){
  if(census_col > 0){
    house_set = data.frame()

    # make a seed for each household
    family_HH_seeds = sample(1000000:6000000, as.numeric(census_col), replace = FALSE)

    house_set = as.data.frame(do.call(rbind, lapply(family_HH_seeds, function(seedy){ # for each seed create a household
      # set seed
      set.seed(seedy)

      # sample Household Type
      if(family_size < 8){
        HHtype = sample(colnames(family_type), size = 1, prob = family_type)

      # Create initial data frame
      # A "family-size" less than 8 indicates a family household, a "family-size" of 8 indicates a single person household, a "family-size" between 8 and 15 indicates a nonfamily household, and a "family-size" of 15 indicates group quarters
       
       if(HHtype == "Married-couple family"){
          partofset = data.frame(household.type = rep(HHtype, family_size), householder = c("Householder", rep("Non-householder", family_size - 1)), members = c("Husband","Wife", rep("NA", family_size - 2)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Male householder- no wife present"){
          partofset = data.frame(household.type = rep(HHtype, family_size), householder = c("Householder", rep("Non-householder", family_size - 1)), members = c("Male Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Female householder- no husband present"){
          partofset = data.frame(household.type = rep(HHtype, family_size),  householder = c("Householder", rep("Non-householder", family_size - 1)), members = c("Female Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
      }
      else{
        if(family_size == 8){
          partofset = data.frame(household.type = "Alone",  householder = "Householder", members = "Householder", size = 1)
        }
        else if(family_size > 8 & family_size != 15){
          partofset = data.frame(household.type = rep("Non-family", family_size - 7),  householder = c("Householder", rep("Non-householder", family_size - 8)), members = c("Householder", rep("NA",(family_size - 8))), size = rep(family_size - 7, family_size -7))
        }
        else{
          partofset = data.frame(household.type="Group Quarters", householder = "Householder", members = "NA", size = 1)
        }
      }

      # Begin sampling characteristics of household (functions stored in other scripts)
      # The functions must be called in this order as some characteristics have different probability distributions based on other characteristics

      # Build using Census Data
      partofset = getindividualcharacteristics(partofset, seedy, Census_data)  #simulates sex, race, age, school.enrollment, education.attainment, employment, disability, nativity, citizenship, language, veteran.status, transport.method, travel.time
      
      # These functions are specifically for non-group quarters
      if(family_size != 15){ 
        partofset = getnumberofvehiclesforhouseholds(partofset, seedy, Census_data) #only dependent on size
        partofset = gethouseholdincome(partofset, seedy, Census_data) #independent -- samples are directly from census data
        partofset = gethouseholdhealthinsurance(partofset, seedy, Census_data) #dependent on income
      }
      # These functions are specifically for group quarters
      else{
        partofset = getnumberofvehiclesforgroupquarters(partofset, seedy, Census_data) #depends on sex and employment
        partofset = getincomeforgroupquarters(partofset, seedy, Census_data) #independent  -- samples are directly from census data
        partofset = gethealthinsuranceforgroupquarters(partofset, seedy, Census_data) #depends on disability and age
      }
     
      partofset$state = rep(state,nrow(partofset))
      partofset$county = rep(county,nrow(partofset))
      partofset$tract = rep(tract,nrow(partofset))

      partofset$number.of.vehicles = as.numeric(partofset$number.of.vehicles)
      partofset$age = as.numeric(partofset$age)
      partofset$travel.time.to.work = as.numeric(partofset$travel.time.to.work)
      partofset$household.income = as.numeric(partofset$household.income)

      partofset$householdID = as.numeric(rep(paste(tract, seedy, sep="",collapse=""), nrow(partofset)))
      partofset$individualID = as.numeric(rep(paste(tract, seedy, sep="",collapse=""), nrow(partofset)))

      # Save new household with any previous households
      return(partofset)
    })))

    return(house_set)
  }
}

#list of counties in Houston
#https://www.houston.org/business/regionalProfile.html
#looked up conty number on site below
#https://www.census.gov/2010census/partners/pdf/FIPS_StateCounty_Code.pdf
#countycodesinhouston=c(201,071,167,039,157,473,015,339,471,407,291)
#there are 1086 total tracts in this area

#sample.set=run.me.for.houston(countycodesinhouston,50,2)
#write.csv(sample.set,"sample_set2.csv")
