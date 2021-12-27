library(jsonlite) #may not be using; need to check
library(censusapi)
library(readr)
library(purrr) #may not be using; need to check
library(data.table)
library(dplyr) #may not be using; need to check

#' Census Data from API for a variable group
#'
#' This function creates the data needed for the simulations via the US Census API acs/acs5.
#' It either reads it from a file inside the censusdir or queries the Census API and creates the file.
#'
#' @param censusdir The base_url for the API
#' @param vintage The census data year
#' @param state The state for which the data is being pulled
#' @param groupname the variable groupname we are pulling the data for
#' @param county_num the census code for the county - only needed if there are blockgroups
#' @param api_type the census api called from - https://www.census.gov/data/developers/data-sets.html
#' @path_suff the suffix for the variable file and whether estimate or error -   "dec.csv" | "est.csv" | "err.csv"
#' @return census_data A dataframe of the Census data used for simulations in this package

#tools
valid_file_path <- function(censusdir,vintage,state,api_type,block,groupname,path_suff){
  folder_path <- paste0(censusdir,vintage,"/state_",state)
  if (file.exists(paste0(folder_path,"/downloaded"))){
    print(sprintf("found folder %s", paste0(folder_path,"/downloaded")))
  }else{
    if (!file.exists(paste0(folder_path))){
      dir.create(paste0(folder_path))
      dir.create(paste0(folder_path,"/downloaded"))
    }else{
      dir.create(paste0(folder_path,"/downloaded"))
    }
    print(sprintf("created folder %s", paste0(folder_path,"/downloaded")))
  }
  api <- str_replace_all(api_type,"/","_")
  file_path <- paste0(censusdir,vintage,"/state_",state,"/downloaded/", state, "_", api, "_", block, "_", groupname, "_", path_suff)
  return(file_path)
}

#https://www.census.gov/data/developers/data-sets.html
#"/acs/acs5" / "dec/pl" (2020) / "dec/sf1" or dec/sf2 or dec/pl (2010 summary) / 2000 has 4 summary files and demo profiles
#https://www.census.gov/data/developers/data-sets/decennial-census.2000.html
valid_census_vars <- function(censusdir, vintage, api_type, groupname){ 
  api <- str_replace_all(api_type,"/","_")
  variables_json <- paste0(censusdir, vintage, "/Variables_",api,".json")
  if (!file.exists(variables_json)){
    census_variables <- listCensusMetadata(
      name = paste0(vintage,"/",api_type), 
      type = "variables") 
    write_json(census_variables,variables_json)
    print(sprintf("retrieved variable options from census api"))
  }else{
    #census_variables <- read_json(variables_json)
    print(sprintf("Read variable options from %s", variables_json))
  }
  #doing read here and not in else, above, doesn't seem right, but otherwise returns nested lists...
  if(str_detect(api_type,"dec")){
    census_variables_dt <- read_json(variables_json) %>%
      map(as.data.table) %>%
      rbindlist(fill = TRUE) %>%
      filter(name != "GEO_ID") %>%
      filter(str_detect(group, groupname))
  }else{
    census_variables_dt <- read_json(variables_json) %>%
      map(as.data.table) %>%
      rbindlist(fill = TRUE) %>%
      filter(name != "GEO_ID") %>%
      filter(str_detect(name, groupname))
  }
  return(census_variables_dt)
}

censusData_byGroupName <- function(censusdir,vintage,state,censuskey,groupname,county_num,block,api_type,path_suff){
  file_path <- valid_file_path(censusdir,vintage,state,api_type,block,groupname,path_suff)
  if (file.exists(file_path)){
    result <- read_csv(file_path, col_types = cols())
    print(sprintf("Reading file from %s", file_path))
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
      #https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2018_Instructions_for_Stat_Testing_ACS.pdf?
      #https://ccrpc.org/wp-content/uploads/2015/02/american-community-survey-guide.pdf
      #https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf for discussion of error reporting
      #https://www.census.gov/programs-surveys/acs/technical-documentation/variance-tables.html to calculate new margins for combined geographies
      }
    if(block=="block_group"){
      region = paste0("block group:*")
      regionin = paste0("state:", state,"+county:",county_num,"+tract:*") #it gets confused if this var is named "county" 
    }else{
      region = paste0("tract:", tract)
      regionin = paste0("state:", state)
    }
    data_for_vars_state <- getCensus(name = api_type,
                                         vintage = vintage,
                                         vars = c("NAME", census_variables$name),
                                         region = region, 
                                         regionin = regionin,
                                         key = censuskey)
    #transpose the data to be joined with variable information 
    if(block=="block_group"){
      data_for_vars <- data_for_vars_state %>%  
        mutate(GEOID_15 = paste0(state,"_",county,"_",tract,"_",block_group)) %>%
        gather(var, value, -GEOID_15) %>% 
        spread(GEOID_15, value)
    }else{
      data_for_vars <- data_for_vars_state %>%
        mutate(GEOID = paste0(state,county,tract)) %>%
        gather(var, value, -GEOID) %>% 
        spread(GEOID, value)
    }
    #join data and variable information and remove unnecessary columns
    result <- dplyr::left_join(census_variables, data_for_vars, by = c("name" = "var")) %>%
      select(-predicateType, -group, -limit, -attributes, -required)
    percent_na <- result[,sum(is.na(.SD))] / (result[,sum(!is.na(.SD))]+result[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print("Writing census file for variable group as csv ...")
    write_csv(result,file_path)
    print("Done.")
  } 
  return(result)
}

