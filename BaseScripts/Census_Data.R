#https://www.census.gov/data/developers/data-sets.html
#"/acs/acs5" / "dec/pl" (2020) / "dec/sf1" or dec/sf2 or dec/pl (2010 summary) / 2000 has 4 summary files and demo profiles
#https://www.census.gov/data/developers/data-sets/decennial-census.2000.html

library(jsonlite) #may not be using; need to check
library(censusapi)
library(readr)
library(data.table)
#library(lehdr) #need for LODES data - https://github.com/jamgreen/lehdr - for sense of flow

#' Census Data from API for a variable group
#'
#' This function creates the data needed for the simulations via the US Census API.
#' It either reads it from a file inside the censusdir or queries the Census API and creates the file.
#' There are two helpers that are also made available:
#' valid_file_path and valid_census_vars
#'
#' @param censusdir The base_url for the API
#' @param vintage The census data year
#' @param state The state for which the data is being pulled
#' @param groupname the variable groupname we are pulling the data for
#' @param county_num the census code for the county - only needed if there are blockgroups; some APIs ignore and return whole state.
#' @param county * for all, including when calling place or zip; can put in specifics. 
#' @param api_type the census api called from - https://www.census.gov/data/developers/data-sets.html
#' @path_suff the suffix for the variable file and whether estimate or error -   "dec.csv" | "est.csv" | "err.csv"
#' @block for region - either block_group (API as "block group") or tract or place or zipcode (API as "zip code tabulation area"); zip returns whole country (complain to your representative)
#' @return census_data / LODES data - A dataframe of the Census data used for simulations in this package

#for multiple geographies:
#https://www.hrecht.com/censusapi/articles/getting-started.html#advanced-geographies
#for writing download metadata to a single file - may want to put in a separate "tools" beyond census
write_download_metadata <- function(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,
                                    rel_file_path,tool,citation,notes){
  csv_path <- paste0(maindir,"download_metadata.csv")
  new_row <- data.frame("concept"=concept,"year"=vintage,"state"=state,"county"=county,"theme"=theme,"groupname"=groupname,
                        "api_type"=api_type,"geo_type"=geo_type,"download_date"=Sys.time(),
                        "tool"=tool,"file_path"=rel_file_path,"citation"=citation,"notes"="")
  if (file.exists(csv_path)){
    write_csv(new_row,csv_path,append = TRUE)
  }else{
    write_csv(new_row,csv_path,append = FALSE,col_names = TRUE)
  }
  return(result)
}

#creates folders and filenames. 
valid_file_path <- function(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff){
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
  if (geo_type=="block_group" & county!="*"){
    if (file.exists(paste0(folder_path,"/county_",county))){
      print(paste0("found folder: ", paste0(folder_path,"/county_",county)))
    }else{
      dir.create(paste0(folder_path,"/county_",county))
      print(paste0("created folder:", folder_path,"/county_",county))
    }
    folder_path <- paste0(folder_path,"/county_",county)
  }
  api <- str_replace_all(api_type,"/","_")
  file_path <- paste0(folder_path,"/",vintage,"_",state,"_",api,"_",geo_type,"_",groupname,"_",path_suff,".RDS")
  return(file_path)
}

valid_census_vars <- function(censusdir, vintage, api_type, groupname){ 
  api <- str_replace_all(api_type,"/","_")
  variables_dt <- paste0(censusdir, vintage, "/Variables_",api,".RDS")
  if (!file.exists(variables_dt)){
    census_variables <- listCensusMetadata(
      name = paste0(vintage,"/",api_type), 
      type = "variables") 
    census_variables <- as.data.table(census_variables)
    census_variables <- census_variables[is.na(predicateOnly)]
    census_variables <- census_variables[,c("predicateType","predicateOnly","hasGeoCollectionSupport","required","limit"):=NULL]
    write_rds(census_variables,variables_dt)
    print(paste0("Retrieved new variable options from census api and saved to: ", variables_dt))
  }else{
    census_variables <- read_rds(variables_dt)
    print(paste0("Read variable options from existing file at: ", variables_dt))
  }
  selected_vars <- census_variables[str_detect(group,groupname)]
  return(selected_vars)
}

census_block_get <- function(censusdir,vintage,state,censuskey,groupname,county_num,api_type,path_suff){
  geo_type <- "block_group"
  file_path <- valid_file_path(censusdir,vintage,state,county_num,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    result <- read_rds(file_path)
    print(paste0("Reading file from ", file_path))
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
    }
    data_for_vars <- getCensus(name = api_type,
                                     vintage = vintage,
                                     vars = c("NAME",census_variables$name),
                                     region = paste0("block group:*"), 
                                     regionin = paste0("state:", state,"+county:",county_num,"+tract:*"),
                                     key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_dt[,("GEOID_15"):=paste0(state,"_",county,"_",tract,"_",block_group)]
    data_for_vars_tr <- data.table(table_name = names(data_for_vars_dt),t(data_for_vars_dt))
    colnames(data_for_vars_tr) <- c("name",data_for_vars_dt[,GEOID_15])
    result <- census_variables[data_for_vars_tr,on="name"]
    write_rds(result,file_path)
    percent_na <- result[,sum(is.na(.SD))] / (result[,sum(!is.na(.SD))]+result[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    theme <- "Decennial Census"
    geo_type <- "block_group"
    tool <- "censusapi"
    citation <- "Decennial U.S. Census"
    rel_file_path <- str_remove(file_path,censusdir)
    notes <- ""
    write_download_metadata(maindir,concept,vintage,state,county_num,theme,groupname,api_type,geo_type,rel_file_path,tool,citation,notes)
  }
  return(result)
}

census_tract_get <- function(censusdir,vintage,state,censuskey,groupname,county,api_type,path_suff){
  geo_type <- "tract"
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    result <- read_rds(file_path)
    print(paste0("Reading file from ", file_path))
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
    }
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME",census_variables$name),
                               region = paste0("tract:*"), 
                               regionin = paste0("state:", state),
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_dt[,("GEOID"):=paste0(state,county,tract)]
    data_for_vars_tr <- data.table(table_name = names(data_for_vars_dt),t(data_for_vars_dt))
    colnames(data_for_vars_tr) <- c("name",data_for_vars_dt[,GEOID])
    result <- census_variables[data_for_vars_tr,on="name"]
    write_rds(result,file_path)
    percent_na <- result[,sum(is.na(.SD))] / (result[,sum(!is.na(.SD))]+result[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    theme <- "Decennial Census"
    geo_type <- "tract"
    tool <- "censusapi"
    citation <- "Decennial U.S. Census"
    rel_file_path <- str_remove(file_path,censusdir)
    notes <- ""
    write_download_metadata(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,rel_file_path,tool,citation,notes)
  }
  return(result)
}

#use state="US" b/c that's what it returns, in any case (test)
census_zcta_get <- function(censusdir,vintage,state,censuskey,groupname,county,api_type,path_suff){
  geo_type <- "zcta"
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    result <- read_rds(file_path)
    print(paste0("Reading file from ", file_path))
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
    }
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME", census_variables$name),
                               region = "zip code tabulation area:*", 
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_tr <- data.table(table_name = names(data_for_vars_dt),t(data_for_vars_dt))
    colnames(data_for_vars_tr) <- c("name",data_for_vars_dt[,zip_code_tabulation_area])
    result <- census_variables[data_for_vars_tr,on="name"]
    write_rds(result,file_path)
    percent_na <- result[,sum(is.na(.SD))] / (result[,sum(!is.na(.SD))]+result[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    theme <- "Decennial Census"
    geo_type <- "zcta"
    tool <- "censusapi"
    citation <- "Decennial U.S. Census"
    rel_file_path <- str_remove(file_path,censusdir)
    notes <- "zip_code_tabulation_area"
    write_download_metadata(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,rel_file_path,tool,citation,notes)
  }
  return(result)
}
#NEED TO TEST AND TO DO PES - which is also for the whole country! Perhaps set valid_file_path to do something for state=US?
census_pes_get <- function(censusdir,vintage,state,censuskey,groupname,county,api_type,path_suff){
  geo_type <- "pes" 
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    result <- read_rds(file_path)
    print(paste0("Reading file from ", file_path))
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
    }
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME", census_variables$name),
                               region = "zip code tabulation area:*", 
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_tr <- data.table(table_name = names(data_for_vars_dt),t(data_for_vars_dt))
    colnames(data_for_vars_tr) <- c("name",data_for_vars_dt[,zip_code_tabulation_area])
    result <- census_variables[data_for_vars_tr,on="name"]
    write_rds(result,file_path)
    percent_na <- result[,sum(is.na(.SD))] / (result[,sum(!is.na(.SD))]+result[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    theme <- "Decennial Census"
    geo_type <- "zcta"
    tool <- "censusapi"
    citation <- "Decennial U.S. Census"
    rel_file_path <- str_remove(file_path,censusdir)
    notes <- "Post-Enumeration Survey"
    write_download_metadata(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,rel_file_path,tool,citation,notes)
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
    if(path_suff=="err.csv"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
      }
    if(block=="block_group"){
      #it gets confused if this var is named "county" 
      data_for_vars_state <- getCensus(name = api_type,
                                       vintage = vintage,
                                       vars = c("NAME",census_variables$name),
                                       region = paste0("block group:*"), 
                                       regionin = paste0("state:", state,"+county:",county_num,"+tract:*"),
                                       key = censuskey)
    }else{
      if(block=="tract"){
        data_for_vars_state <- getCensus(name = api_type,
                                         vintage = vintage,
                                         vars = c("NAME",census_variables$name),
                                         region = paste0("tract:*"), 
                                         regionin = paste0("state:", state),
                                         key = censuskey)
      }else{
        region = 
        data_for_vars_state <- getCensus(name = api_type,
                                         vintage = vintage,
                                         vars = c("NAME", census_variables$name),
                                         region = "zip code tabulation area:*", 
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
   tool <- "censusapi"
   citation <- "Decennial U.S. Census"
   rel_file_path <- str_remove(file_path,censusdir)
   write_download_metadata(maindir,concept,vintage,state,county,groupname,api_type,block,rel_file_path,tool,citation)
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


