library(doParallel)

source("BaseScripts/Census_Data.R")
source("BaseScripts/HCAD_Data.R")
source("BaseScripts/helpers.R")
source("BaseScripts/householdsGenerator.R")
source("BaseScripts/groupquartersFunctions.R")
source("BaseScripts/householdFunctions.R")
source("BaseScripts/mergeWithHCAD.R")
source("TestScripts/naCheck.R")
source("TestScripts/proportionCheck.R")

#' createBaseSAM function
#'
#' This function controls the flow for simulating the base data (from census and hosuing stock)
#'
#' It calls other functions in the BaseScripts in order to simulate characteristics for households and for each individual
#'
#' @param censusdir the census data directory 
#' @param housingdir the main data directory, has to have a folder called Census and HCAD
#' @param censusFromRDS determines wheter to read preprocessed census data from an RDS file
#' @param housingStockFromRDS determines whether to read preprcocessed housing stock data from an RDS file
#' @param censusAPIKey The key needed to use the Census API (if data has to be generated)
#' @return sam A dataframe of the simulated individuals in a city
createBaseSAM <- function(censusdir, housingdir, censusFromRDS = TRUE, HCAD_parcelsFromRDS = TRUE, numberOfCores = 1) {
  
  #get census data
  if (censusFromRDS) {
    # import saved RDS file
    census_data_file <- paste0(censusdir, "Census_data.RDS")
    census_data <- readRDS(census_data_file)
    print(sprintf("Done opening census RDS from %s", census_data_file))
  } else {
    # use the census API
    censusAPIKey <- readLines(paste0(censusdir, "key"))
    census_data <- censusDataViaAPI(censusdir = censusdir, base_url = 'http://api.census.gov/data/2014/acs5?', state = 48, censusAPIKey)
    print("Done retrieving census data from API.")
  }
  
  #get prepared complete housing parcels (with tract, county information), for Houston this data is provided by HCAD
  if (HCAD_parcelsFromRDS) {
    HCAD_parcels_file <- paste0(housingdir, "PreprocessedRDS/HCAD_parcels.RDS")
    HCAD_parcels <- readRDS(HCAD_parcels_file)
    print(sprintf("Done opening census RDS from %s", HCAD_parcels_file))
  } else {
    HCAD_parcels <- getHCADParcels(hcadDataDir = housingdir)
    print(sprintf("Done preparing HCAD parcels from folder %s", housingdir))
  }
  
  # get building stock information for residential buildings
  buildingDataDir <- paste0(housingdir,"Real_building_land/")
  res_housing_stock <- getResBuildings(buildingDataDir = buildingDataDir,
                                       HCAD_parcels = HCAD_parcels)
  print(sprintf("Done preparing residential housing parcels fro folder %s", buildingDataDir))
  # get other building stock information
  realhousing_stock <- getRealBuildings(buildingDataDir = buildingDataDir,
                                        HCAD_parcels = HCAD_parcels)
  print(sprintf("Done preparing other housing parcels %s", buildingDataDir))
  
  
  
  #########TO DO: REWRITE OF CODE NOT DONE FOR BELOW
  
  #set up for parallel processing
  # cl <- makeCluster(numberOfCores)
  # registerDoParallel(cl)
  # 
  # # set the list of tracts
  # tracts = subset(census_data, census_data$county == 201)
  # 
  # # generate households from the census data
  # households = foreach (index = tracts,.combine='rbind') %dopar% {
  #   housholds = households_generator(48,201,tracts[index],seed=1,Census_data)
  #   housholds$ACCOUNT=NA
  #   return(households)
  # }
  # 
  # # Now the people from the simulated model will be placed in households in the HCAD data
  # complete_sample_set=foreach (index = 1:786,.combine='rbind')%dopar%{
  #   tract.sample.set = mergeWithHCAD(tracts[index], potential_houses, sample.set)
  #   return(tract.sample.set)
  # }
  # 
  # stopCluster(cl)
  # 
  # return(complete_sample_set)
  
  #########REWRITE OF CODE NOT DONE FOR ABOVE
}

#' sanityChecks function
#'
#' This function checks validity of base sam data
#'
#' @param complete_sample_set the generated sample set to be tested
#' @param saveResults determines whether to to save test results to RDS files
sanityChecks <- function(complete_sample_set, saveResults = TRUE) {
  
  #TODO: implement actual test not just check
  result <- TRUE 
  
  # This tests if there are too many NAs in the model than expected. The resulting data frame is saved.
  na = naTest(complete_sample_set)
  # This tests the proportions in the model and compares them with the Census Data to check for accuracy. The resulting data frame is saved
  prop = foreach (index = 1:786,.combine='rbind')%dopar%{
    propByTract= prop_Check(sample.set, Census_data, tracts[index])
    return(propByTract)
  }
  
  if (saveRsults) {
    saveRDS(na, "naCheck.RDS")
    saveRDS(prop, "proportionCheck.RDS")
  }
  
  return(result)
}






  
  






















