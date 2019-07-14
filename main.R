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


#set working directory to where all the data folders can be found
datadir <- "~/University Of Houston/Price, Daniel M - Social Network Hypergraphs/"

#' createBaseSAM function
#'
#' This function controls the flow for simulating the base data (from census and hosuing stock)
#'
#' It calls other functions in the BaseScripts in order to simulate characteristics for households and for each individual
#'
#' @param censusFromRDS determines wheter to read preprocessed census data from an RDS file
#' @param housingStockFromRDS determines whether to read preprcocessed housing stock data from an RDS file
#' @param censusAPIKey The key needed to use the Census API (if data has to be generated)
#' @return sam A dataframe of the simulated individuals in a city
createBaseSAM <- function(censusFromRDS = TRUE, housingStockFromRDS = TRUE, censusAPIKey, numberOfCores = 1) {
  
  #get census data
  if (censusFromRDS) {
    # import saved RDS file 
    census_data <- readRDS(paste0(datadir, "Census/2014/Census_data_2014.RDS"))
  } else {
    # use the census API
    census_data <- census_data_viaAPI(base_url = 'http://api.census.gov/data/2014/acs5?', state = 48, censusAPIKey)
  }
  
  #get prepared complete housing parcels (with tract, county information), for Houston this data is provided by HCAD
  if (housingStockFromRDS) {
    HCAD_parcels <- readRDS(paste0(datadir, "HCAD/HCAD_resBuildings_2015.RDS"))
  } else {
    HCAD_parcels <- getHCADParcels(paste0(datadir,"HCAD/2015/"))
  }
  
  # get building information for residential buildings
  if (housingStockFromRDS) {
    # import saved RDS file 
    res_housing_stock <- readRDS(paste0(datadir, "HCAD/2015/HCAD_resBuildings_2015.RDS"))
  } else {
    # preprocess HCAD data
    res_housing_stock <- getresBuildings(paste0(datadir,"HCAD/2015/"))
  }
  
  # get building information for other buildins 
  if (housingStockFromRDS) {
    # import saved RDS file 
    realhousing_stock <- readRDS(paste0(datadir, "HCAD/2015/HCAD_realBuildings_2015.RDS"))
  } else {
    # preprocess HCAD data
    realhousing_stock <- getrealBuildings(paste0(datadir,"HCAD/2015/"))
  }
  
  
  
  #########REWRITE OF CODE NOT DONE FOR BELOW
  
  #set up for parallel processing
  cl <- makeCluster(numberOfCores)
  registerDoParallel(cl)
  
  # set the list of tracts
  tracts = subset(census_data, census_data$county == 201)
  
  # generate households from the census data
  households = foreach (index = tracts,.combine='rbind') %dopar% {
    housholds = households_generator(48,201,tracts[index],seed=1,Census_data)
    housholds$ACCOUNT=NA
    return(households)
  }
  
  # Now the people from the simulated model will be placed in households in the HCAD data
  complete_sample_set=foreach (index = 1:786,.combine='rbind')%dopar%{
    tract.sample.set = mergeWithHCAD(tracts[index], potential_houses, sample.set)
    return(tract.sample.set)
  }
  
  stopCluster(cl)
  
  return(complete_sample_set)
  
  #########REWRITE OF CODE NOT DONE FOR BELOW
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


#HOW TO RUN
sam <- createBaseSAM()

# do some sanity checks and more columns
if (sanityChecks(sam)) {
  # Add column to add level to data
  sam <- one_of(sam)
  
  # TODO move into HCAD preprocessing - adds cooordinates to the model based on the geometry column
  sam <- add_lat_long(sam)
  
  # This converts all the columns in to the model to the appropriate class (either character or numeric)
  sam <- convertColumnTypes(sam)
  
} else {
  print("Did not pass sanity checks!")
}

#add extension columns - NHANES etc.



#Save the final result
saveRDS(sam, paste0("complete_sam", Sys.Date(), ".RDS"))





  
  





# Depending on the USE_CODE some buildings will only have one unit while others will have multiple units. If a building doesn't list units then use the average number of units for that building's USE_CODE
potential_buildings$UNITS[potential_buildings$USE_CODE %in% c("1D1", "A1", "A2", "A3", "A4", "B2", "B3", "B4", "C1", "C2", "C3", "E1", "M3", "O1", "O2", "U0", "X4", "XG", "XI", "XL", "Z4") | is.na(potential_buildings$UNITS)]= 1
for(x in c("B1", "F1", "F2", "X1", "X2", "X3", "XE", "XJ", "XU")){
   avg_units = round(mean(as.integer(potential_buildings[potential_buildings$USE_CODE == x & !potential_buildings$UNITS %in% c("", "A", "B", "C", "D", "1/2", "1/1"),]$UNITS), na.rm = T))
   potential_buildings$UNITS[potential_buildings$USE_CODE == x & potential_buildings$UNITS %in% c("", "A", "B", "C", "D", "1/1", "1/2")] = avg_units
}
















