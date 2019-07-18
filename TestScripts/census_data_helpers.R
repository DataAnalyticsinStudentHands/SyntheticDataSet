library(censusapi)
library(rio)
library(jsonlite)
library(purrr)
library(data.table)
library(tidyverse)

#' readCensusFromFiles function
#'
#' This function reads doanloaded data from csv files and merges them into one data frame
#'
#' @param directory a directory containing the downloaded files
readAcsDataFromFiles <- function(directory) {
  fileNames <- list.files(directory)
  
  fileList=lapply(fileNames, function(x){paste0(getwd(),"/",x)})
  dataList = lapply(fileList, function(x){import(x)})
  merged_census_f <- Reduce(function(x,y) {merge(x,y, by=c("tract", "county", "state"))}, dataList)
  
  return(merged_census_f)
}

saveAcsMetaData <- function(censusDataDir, vintage = 2017) {
  
  acs_groups <- listCensusMetadata(name = "acs/acs5", vintage = vintage, type = "groups")
  write_json(acs_groups, paste0(censusDataDir, vintage, "/Groups_MetaData.json"), pretty = T)
  
  acs_variables <- listCensusMetadata(name = "acs/acs5", vintage = vintage, type = "variables")
  write_json(acs_variables, paste0(censusDataDir, vintage, "/Variables_MetaData.json"), pretty = T)
  
  return(sprintf("Finished writing acs/acs5 metadata for vintage: %s", vintage))
}

filterAllAcsVariables <- function(censusDataDir, vintage = 2017, all = FALSE, term) {
  acs_variables <- read_json(paste0(censusDataDir, vintage, "/Variables_MetaData.json")) %>%
    map(as.data.table) %>%
    rbindlist(fill = TRUE) %>%
    filter(name != "GEO_ID")
    
  if (!all) {
    return(filter(acs_variables, str_detect(concept, term)))
  }
  
  return(acs_variables)
}

filterAcsGroups <- function(censusDataDir, vintage = 2017, groups) {
  filtered_acs_groups <- read_json(paste0(censusDataDir, vintage, "/Groups_MetaData.json")) %>%
    map(as.data.table) %>%
    rbindlist(fill = TRUE)%>%
    filter(name %in% groups)
  
  return(filtered_acs_groups)
}






