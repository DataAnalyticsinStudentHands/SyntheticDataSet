library(jsonlite)
library(censusapi)
library(readr)
library(purrr)
library(data.table)
library(dplyr)

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
censusDataFromAPI_byGroupName <- function(censusdir, vintage, state, county_num, tract, censuskey, groupname,tr_bg) {
  #check whether folders exist
  if (file.exists(paste0(censusdir,vintage,"/downloaded"))){
    print(sprintf("found folder %s", paste0(censusdir,vintage,"/downloaded")))
    }else{
      if (!file.exists(paste0(censusdir,vintage))){
        dir.create(paste0(censusdir,vintage))
        dir.create(paste0(censusdir,vintage,"/downloaded"))
      }else{
        dir.create(paste0(censusdir,vintage,"/downloaded"))
      }
  }
  
  #check whether file for the requested group data already exists
  file_path <- paste0(censusdir, vintage, "/downloaded/", state, "_", county_num, "_", groupname, ".csv")
  if (file.exists(file_path)){
    result <- read_csv(file_path, col_types = cols())
    print(sprintf("Reading file from %s", file_path))
  }else{
    # get census variables from variables metadata file for a group
    variables_json <- paste0(censusdir, vintage, "/Variables_MetaData.json")
    if (!file.exists(variables_json)){
      acs_variables_new <- listCensusMetadata(
        name = paste0(vintage,"/acs/acs5"), 
        type = "variables") 
      write_json(acs_variables_new,variables_json)
      print(sprintf("retrieved variable options from census api"))
    }else{
      print(sprintf("Read variable options from %s", variables_json))
    }
    #do I need to do something to keep it from trying to read too fast after write?
    acs_variables <- read_json(variables_json) %>%
      map(as.data.table) %>%
      rbindlist(fill = TRUE) %>%
      filter(name != "GEO_ID") %>%
      filter(str_detect(name, groupname))
    
    if(tr_bg == "bg"){
      acs_data_for_vars_state <- getCensus(name = "acs/acs5",
                                           vintage = vintage,
                                           vars = c("NAME", acs_variables$name),
                                           region = paste0("block group:*"), 
                                           regionin = paste0("state:", state,"+county:",county,"+tract:*"),
                                           key = censuskey)
      #transpose the data to be joined with variable information after filter for county
      acs_data_for_vars_county <- filter(acs_data_for_vars_state, county == county_num) %>%
        mutate(geoid_15 = paste0(state,"_",county,"_",tract,"_",block_group)) %>%
        gather(var, value, -geoid_15) %>% 
        spread(geoid_15, value)
    }else{
      acs_data_for_vars_state <- getCensus(name = "acs/acs5",
                                           vintage = vintage,
                                           vars = c("NAME", acs_variables$name),
                                           region = paste0("tract:", tract), 
                                           regionin = paste0("state:", state),
                                           key = censuskey)
      #transpose the data to be joined with variable information after filter for county
      acs_data_for_vars_county <- filter(acs_data_for_vars_state, county == county_num) %>%
        gather(var, value, -tract) %>% 
        spread(tract, value)
    }
    
    acs_variables$nameM <- paste0(substr(acs_variables$name,1,nchar(as.character(acs_variables$name))-1),"M")
    #https://ccrpc.org/wp-content/uploads/2015/02/american-community-survey-guide.pdf
    #https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf for discussion of error reporting
    #https://www.census.gov/programs-surveys/acs/technical-documentation/variance-tables.html to calculate new margins for combined geographies
    if(tr_bg == "bg"){
      acs_err_for_vars_state <- getCensus(name = "acs/acs5",
                                          vintage = vintage,
                                          vars = c("NAME", acs_variables$nameM),
                                          region = paste0("block group:*"), 
                                          regionin = paste0("state:", state,"+county:",county,"+tract:*"),
                                          key = censuskey)
    #transpose the data to be joined with variable information after filter for county
      acs_err_for_vars_county <- filter(acs_err_for_vars_state, county == county_num) %>%
        mutate(geoid_15 = paste0(state,"_",county,"_",tract,"_",block_group)) %>%
        gather(var, value, -geoid_15) %>% 
        spread(geoid_15, value)
    }else{
      acs_err_for_vars_state <- getCensus(name = "acs/acs5",
                                          vintage = vintage,
                                          vars = c("NAME", acs_variables$nameM),
                                          region = paste0("tract:", tract), 
                                          regionin = paste0("state:", state),
                                          key = censuskey)
      #transpose the data to be joined with variable information after filter for county
      acs_err_for_vars_county <- filter(acs_err_for_vars_state, county == county_num) %>%
        gather(var, value, -tract) %>% 
        spread(tract, value)
    }
    acs_err_for_vars_county$nameE <- paste0(substr(acs_err_for_vars_county$var,1,nchar(as.character(acs_err_for_vars_county$var))-1),"E")
    
    #join data and variable information and remove unnecessary columns
    resultEst <- dplyr::left_join(acs_variables, acs_data_for_vars_county, by = c("name" = "var")) %>%
      select(-predicateType, -group, -limit, -attributes, -required)
    
    result <- dplyr::left_join(resultEst, acs_err_for_vars_county, by = c("name" = "nameE"),
                               suffix = c("_e","_m")) # #e is for Estimate Total and m is for Margin of Error
      
    print("Writing census file for variable group as csv ...")
    write_csv(result,file_path)
    print("Done.")
  }
  return(result)
}

#looks like none of the census block level information is available for download, except the actual boundaries.
decennial_Census_DataFromAPI_byGroupName <- function(censusdir, dec_vintage, state, county_num, tract, censuskey, groupname) {
  #check whether folders exist
  if (file.exists(paste0(censusdir,dec_vintage,"/downloaded"))){
           print(sprintf("found folder %s", paste0(censusdir,dec_vintage,"/downloaded")))
    }else{
           if (!file.exists(paste0(censusdir,dec_vintage))){
             dir.create(paste0(censusdir,dec_vintage))
             dir.create(paste0(censusdir,dec_vintage,"/downloaded"))
           }else{
           dir.create(paste0(censusdir,dec_vintage,"/downloaded"))
           }
    }
  #check whether file for the requested group data already exists
  file_path <- paste0(censusdir, dec_vintage, "/downloaded/decennial_", state, "_", county_num, "_", groupname, ".csv")
  if (file.exists(file_path)){
    result <- read_csv(file_path, col_types = cols())
    print(sprintf("Reading file from %s", file_path))
  }else{
    # get census variables from variables metadata file for a group
    #https://api.census.gov/data/2010/dec/sf1/variables.html
    variables_json <- paste0(censusdir, dec_vintage, "/Dec_Variables_MetaData.json")
    if (!file.exists(variables_json)){
      new_dec_variables <- listCensusMetadata(
        name = paste0(dec_vintage,"/dec/sf1"), 
        type = "variables")
      write_json(new_dec_variables,variables_json)
      print(sprintf("retrieved variable options from census api"))
    }else{
      print(sprintf("Read variable options from %s", variables_json))
    }
    dec_variables <- read_json(variables_json) %>%
      map(as.data.table) %>%
      rbindlist(fill = TRUE) %>%
      filter(name != "GEO_ID") %>%
      filter(group == groupname)
      #filter(str_detect(group, groupname))
    dec_data_for_vars <- getCensus(name = "dec/sf1",
                                         vintage = dec_vintage,
                                         vars = c("NAME", dec_variables$name),
                                         #region = paste0("tract:*"),
                                         region = paste0("block group:*"), 
                                         regionin = paste0("state:", state,"+county:",county,"+tract:*"),
                                         key = censuskey) %>%
    #transpose the data to be joined with variable information 
      mutate(geoid_15 = paste0(state,"_",county,"_",tract,"_",block_group)) %>%
      gather(var, value, -geoid_15) %>% 
      spread(geoid_15, value) #spread(tract, value) #
    #for PCT12 - no mutate
     #gather(var, value, -tract) %>% 
     #spread(tract, value) #
    
    #join data and variable information and remove unnecessary columns
    result <- dplyr::left_join(dec_variables, dec_data_for_vars, by = c("name" = "var")) %>%
      select(-predicateType, -limit, -attributes, -required)
    
    print("Writing decennial census file for variable group as csv ...")
    write_csv(result,file_path)
    print("Done.")
  }
  
  return(result)
}
