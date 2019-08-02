# Workflow controller for SAM creation
source("BaseScripts/basesam.R")

# before we get started, setup directories and parameters
housingdir = "~/University Of Houston/Price, Daniel M - Social Network Hypergraphs/HCAD/" 
censusdir = "~/University Of Houston/Price, Daniel M - Social Network Hypergraphs/Census/"
#censusdir = "~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/Census/" #Dan at home
vintage = 2017
housingStockFromRDS = TRUE 
numberOfCores = 1
state = 48
county = 201
tract = "*"

#let's create SAM
sam <- createBaseSAM(censusdir, housingdir, vintage, 
                     housingStockFromRDS, 
                     numberOfCores = numberOfCores, state = state, county = county, tract = tract)

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
