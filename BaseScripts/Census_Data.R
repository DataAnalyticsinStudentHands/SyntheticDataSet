library(jsonlite)
library(censusapi)
library(readr)
library(purrr)
library(data.table)

#' Census Data from API for a variable group
#'
#' This function creates the data needed for the simulations via the US Census API acs/acs5.
#' It either reads it from a file inside the censusdir or queries the Census API and creates the file.
#'
#' @param censusdir The base_url for the API
#' @param vintage The census data year
#' @param state The state for which the data is being pulled
#' @param county The county for which the data is being pulled
#' @param groupname the variable groupname we are pulling the data for
#' @return census_data A dataframe of the Census data used for simulations in this package
censusDataFromAPI_byGroupName <- function(censusdir, vintage, state, county, tract, censuskey, groupname) {
  
  #check whether file for the requested group data already exists
  file_path <- paste0(censusdir, vintage, "/downloaded/", state, "_", county, "_", groupname, ".csv")
  if (file.exists(file_path)) {
    print(sprintf("Reading file from %s", file_path))
    result <- read_csv(file_path)
  } else {
    # get census variables from variables metadata file for a group
    acs_variables <- read_json(paste0(censusdir, vintage, "/Variables_MetaData.json")) %>%
      map(as.data.table) %>%
      rbindlist(fill = TRUE) %>%
      filter(name != "GEO_ID") %>%
      filter(str_detect(name, groupname))
    
    acs_data_for_vars_state <- getCensus(name = "acs/acs5",
                                         vintage = 2017,
                                         vars = c("NAME", acs_variables$name),
                                         region = paste0("tract:", tract), regionin = paste0("state:", state),
                                         key = censuskey)
    
    #transpose the data to be joined with variable information after filter for county
    acs_data_for_vars_county <- filter(acs_data_for_vars_state, county == 201) %>%
      gather(var, value, -tract) %>% 
      spread(tract, value)
    
    #join data and variable information and remove unnecessary columns
    result <- dplyr::left_join(acs_variables, acs_data_for_vars_county, by = c("name" = "var")) %>%
      select(-predicateType, -group, -limit, -attributes, -required)
    
    print("Writing census file for variable group as csv ...")
    write_csv(result,file_path)
    print("Done.")
  }
  
  return(result)
}

