library(jsonlite) #may not be using; need to check
library(censusapi)
library(readr)
#library(purrr) #may not be using; need to check
library(data.table)
#library(dplyr) #may not be using; need to check
#library(lehdr) #need for LODES data - https://github.com/jamgreen/lehdr

#' Census Data from API for a variable group
#'
#' This function creates the data needed for the simulations via the US Census API acs/acs5.
#' It either reads it from a file inside the censusdir or queries the Census API and creates the file.
#' There are two helpers that are also made available:
#' valid_file_path and valid_census_vars
#'
#' @param censusdir The base_url for the API
#' @param vintage The census data year
#' @param state The state for which the data is being pulled
#' @param groupname the variable groupname we are pulling the data for
#' @param county_num the census code for the county - only needed if there are blockgroups; some APIs seem to ignore and return whole state...
#' @param county * for all, including when calling place or zip
#' @param api_type the census api called from - https://www.census.gov/data/developers/data-sets.html
#' @path_suff the suffix for the variable file and whether estimate or error -   "dec.csv" | "est.csv" | "err.csv"
#' @block for region - either block_group (API as "block group") or tract or place or zipcode (API as "zip code tabulation area"); zip returns whole country (complain to your representative)
#' @return census_data / LODES data - A dataframe of the Census data used for simulations in this package

#creates folders and filenames. 
valid_file_path <- function(censusdir,vintage,state,county,api_type,block,groupname,path_suff){
  if (!file.exists(paste0(censusdir,vintage))){
    dir.create(paste0(censusdir,vintage))
    print(paste0("created folder: ",censusdir,vintage))
  }else{
    print(paste0("found folder: ",censusdir,vintage))
  }
  folder_path <- paste0(censusdir,vintage,"/state_",state)
  if (!file.exists(paste0(folder_path))){
    dir.create(folder_path)
    print(paste0("created folder: ",folder_path))
  }else{
    print(paste0("found folder: ",folder_path))
  }
  if (block=="block_group" & county!="*"){
    if (file.exists(paste0(folder_path,"/county_",county))){
      print(paste0("found folder: ", paste0(folder_path,"/county_",county)))
    }else{
      dir.create(paste0(folder_path,"/county_",county))
      print(paste0("created folder:", folder_path,"/county_",county))
    }
    folder_path <- paste0(folder_path,"/county_",county)
  }
  api <- str_replace_all(api_type,"/","_")
  file_path <- paste0(folder_path,"/",vintage,"_",state,"_",api,"_",block,"_",groupname,"_",path_suff)
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
    print(sprintf("retrieved variable options from %s from census api", variables_json))
  }else{
    #census_variables <- read_json(variables_json)
    print(sprintf("Read variable options from %s", variables_json))
  }
  #doing read here and not in else, above, doesn't seem right, but otherwise returns nested lists...
  
  #if(str_detect(api_type,"dec")){
    census_variables_dt <- read_json(variables_json) %>%
      map(as.data.table) %>%
      rbindlist(fill = TRUE) %>%
      filter(name != "GEO_ID") %>%
      filter(str_detect(group, groupname))
  #}else{
  #  census_variables_dt <- read_json(variables_json) %>%
  #    map(as.data.table) %>%
  #    rbindlist(fill = TRUE) %>%
  #    filter(name != "GEO_ID") %>%
  #    filter(str_detect(name, groupname))
  #}
  return(census_variables_dt)
}

write_download_metadata <- function(concept,vintage,state,county,groupname,api_type,block,file_path){
  rel_file_path <- str_remove(file_path,censusdir)
  csv_path <- paste0(censusdir,"download_metadata.csv")
  new_row <- data.frame("concept"=concept,"year"=vintage,"state"=state,"county"=county,"groupname"=groupname,
                        "api_type"=api_type,"block"=block,"file_path"=file_path,"download_date"=Sys.time())
  if (file.exists(csv_path)){
    write_csv(new_row,csv_path,append = TRUE)
  }else{
    write_csv(new_row,csv_path,append = FALSE,col_names = TRUE)
  }
  return(result)
}

censusData_byGroupName <- function(censusdir,vintage,state,censuskey,groupname,county_num,block,api_type,path_suff){
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,block,groupname,path_suff)
  if (file.exists(file_path)){
    result <- read_csv(file_path, col_types = cols())
    print(paste0("Reading file from ", file_path))
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    print(census_variables$label)
    print(census_variables$name)
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
      #https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2018_Instructions_for_Stat_Testing_ACS.pdf?
      #https://ccrpc.org/wp-content/uploads/2015/02/american-community-survey-guide.pdf
      #https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf for discussion of error reporting
      #https://www.census.gov/programs-surveys/acs/technical-documentation/variance-tables.html to calculate new margins for combined geographies
      }
    if(block=="block_group"){
      #it gets confused if this var is named "county" 
      data_for_vars_state <- getCensus(name = api_type,
                                       vintage = vintage,
                                       vars = c("NAME", census_variables$name),
                                       region = paste0("block group:*"), 
                                       regionin = paste0("state:", state,"+county:",county_num,"+tract:*"),
                                       key = censuskey)
    }else{
      if(block=="tract"){
        data_for_vars_state <- getCensus(name = api_type,
                                         vintage = vintage,
                                         vars = c("NAME", census_variables$name),
                                         region = paste0("tract:*"), 
                                         regionin = paste0("state:", state),
                                         key = censuskey)
      }else{
        region = "zip code tabulation area:*"
        data_for_vars_state <- getCensus(name = api_type,
                                         vintage = vintage,
                                         vars = c("NAME", census_variables$name),
                                         region = region, 
                                         #regionin = regionin,
                                         key = censuskey)
      }
    }
    #transpose the data to be joined with variable information 
    if(block=="block_group"){
      data_for_vars <- data_for_vars_state %>%  
        mutate(GEOID_15 = paste0(state,"_",county,"_",tract,"_",block_group)) %>%
        gather(var, value, -GEOID_15) %>% 
        spread(GEOID_15, value)
    }else{
      if(block=="tract"){
      data_for_vars <- data_for_vars_state %>%
        mutate(GEOID = paste0(state,county,tract)) %>%
        gather(var, value, -GEOID) %>% 
        spread(GEOID, value)
      }else{
        data_for_vars <- data_for_vars_state %>%
        gather(var, value, -zip_code_tabulation_area) %>% 
        spread(zip_code_tabulation_area, value)
      }
    }
    #join data and variable information and remove unnecessary columns
   result <- dplyr::left_join(census_variables, data_for_vars, by = c("name" = "var")) %>%
     select(-predicateType, -group, -limit, -attributes, -required)
   percent_na <- result[,sum(is.na(.SD))] / (result[,sum(!is.na(.SD))]+result[,sum(is.na(.SD))])
   print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
   print(sprintf("Writing census file for variable group as csv %s", file_path))
   write_csv(result,file_path)
   write_download_metadata(concept,vintage,state,county,groupname,api_type,block,file_path)
   print(var)
   print("Done.")
  } 
  return(result)
}


#https://github.com/jamgreen/lehdr
#or_od <- grab_lodes(state = "tx", 
#                    year = 2020, 
#                    version = "LODES8", 
#                    lodes_type = "od", 
#                    job_type = "JT01",
#                    segment = "S000", 
#                    state_part = "main", 
#                    agg_geo = "block")
# 10m rows - by origin and destination in block by job type... could be very interesting, but not simple
#think about BFRSS, and Kid version, etc.


