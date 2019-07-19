# Workflow controller for SAM creation
source("BaseScripts/basesam.R")

#set working directory to where all the data folders can be found
housingDataDirectory <- "~/University Of Houston/Price, Daniel M - Social Network Hypergraphs/HCAD/2015/"
censusDataDirectory <- "~/University Of Houston/Price, Daniel M - Social Network Hypergraphs/Census/2014/"

housingdir = housingDataDirectory 
censusdir = censusDataDirectory
vintage = 2017
censusFromRDS = TRUE 
HCAD_parcelsFromRDS = TRUE 
numberOfCores = 1
censuskey = "6ee9b8141913fdd7763ff46af20c20d0e9a5bc68"

sam <- createBaseSAM(housingdir, 
                     censusdir,
                     vintage,
                     censusFromRDS, 
                     HCAD_parcelsFromRDS, 
                     numberOfCores)

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
