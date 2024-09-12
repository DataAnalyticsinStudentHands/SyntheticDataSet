#https://www.census.gov/data/developers/data-sets.html
#"/acs/acs5" / "dec/pl" (2020) / "dec/sf1" or dec/sf2 or dec/pl (2010 summary) / 2000 has 4 summary files and demo profiles
#https://www.census.gov/data/developers/data-sets/decennial-census.2000.html

#library(jsonlite) #may not be using; need to check
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
#' @param censusdir The location for storing retrieved files
#' @param vintage The census data year as character string
#' @param state The state for which the data is being pulled; two digit number as character string or "all"
#' @param groupname The variable groupname we are pulling the data for
#' @param county_num The census code for the county - only needed if there are blockgroups; some APIs ignore and return whole state (or country).
#' @param county Same as county_num, but api needs it in separately named variable; * for all, including when calling place or zip; can put in specifics. 
#' @param api_type The census api called from - https://www.census.gov/data/developers/data-sets.html
#' @path_suff The suffix for the variable file and whether estimate or error -   "dec" | "est" | "err"
#' @geo_type Region size - either block_group (API as "block group") or tract or place or zipcode (API as "zip code tabulation area"); zip returns whole country (complain to your representative)
#' @return census_data / LODES data - A dataframe of the Census data used for simulations in this package


#creates folders and filenames. 
valid_file_path <- function(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff){
  if (county=="*"){county <- "all"} #some file systems can't use * as part of name
  if (state=="*"){state <- "all"}
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
    census_variables[is.na(predicateOnly)][
      ,c("predicateType","predicateOnly","hasGeoCollectionSupport","required","limit"):=NULL][
        label!="Geography"]
    write_rds(census_variables,variables_dt)
    print(paste0("Retrieved new variable options from census api and saved to: ", variables_dt))
  }else{
    census_variables <- read_rds(variables_dt)
    print(paste0("Read variable options from existing file at: ", variables_dt))
  }
  selected_vars <- census_variables[str_detect(group,groupname)]
  return(selected_vars)
}

split_labels <- function(census_vars){
  census_vars[,("label"):=str_remove_all(label,":")][
    ,("label"):=str_remove_all(label,"Total")][
      ,("label"):=str_remove_all(label,"!!!!")][
        ,("label"):=trimws(label)]
  label_size <- 1+max(str_count(census_vars[,label],"!!"),na.rm = TRUE)
  label_names <- paste0("label_",1:label_size)
  census_vars[,c(label_names):=tstrsplit(label,"!!")]
  return(census_vars)
}

#if need two dt split, will call relabel twice
relabel <- function(dt,label_c1,row_c1,groupname){ 
  #doing this on whole dt, not just census_variables, for both testing and so it can be handtuned after original get
  label_names <- paste0("label_",1:length(label_c1))
  setnames(dt,label_names,label_c1,skip_absent = TRUE)
  #rows are called by name; the !is.na() for certain labels is done before call
  setkey(dt,name)
  result <- dt[row_c1]
  write_schema(groupname,label_c1,result) 
  return(result)
}

write_relabel <- function(relabel_dt,censusdir,vintage,state,censuskey,geo_type,groupname,county_num,api_type,path_suff){
  file_path <- valid_file_path(censusdir,vintage,state,county_num,api_type,geo_type,groupname,path_suff)
  path <- str_replace(file_path,".RDS","_relabeled.RDS")
  if (file.exists(file_path)){
    if (file.exists(path)){
      file.remove(path)
      print(paste0("Replacing file at ", path))
    }else{
      print(paste0("Creating file at ", path))
    }
  }else{
    print(paste0("No file found at ", file_path," creating new file at ",path))
  }
  saveRDS(relabel_dt,path)
}

#for writing download metadata to a single file - may want to put in a separate "tools" beyond census
write_download_metadata <- function(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,
                                    rel_file_path,tool,citation,notes){
  new_row <- data.frame("concept"=concept,"year"=vintage,"state"=state,"county"=county,"theme"=theme,"groupname"=groupname,
                        "api_type"=api_type,"geo_type"=geo_type,"download_date"=Sys.time(),
                        "tool"=tool,"file_path"=rel_file_path,"citation"=citation,"notes"=notes)
  csv_path <- paste0(maindir,"download_metadata.csv")
  if (file.exists(csv_path)){
    write_csv(new_row,csv_path,append = TRUE, col_names = FALSE)
  }else{
    write_csv(new_row,csv_path,append = FALSE,col_names = TRUE)
  }
}

write_schema <- function(groupname,label_c1,dt){
  #follow above, but call from relabel - now saving old, but probably don't need for final
  #want this to be smart at some point, but right now just adding to the big file with no joins
  labels <- paste0(groupname,"_",label_c1)
  if(!file.exists(paste0(maindir,"sam_schemas/"))){
    dir.create(paste0(maindir,"sam_schemas/"))
    print(paste0("created folder: ",maindir,"sam_schemas/"))
  }
  rds_path <- paste0(maindir,"sam_schemas/","sam_schema.RDS")
  rds_dt <- unique(dt[,..label_c1])
  setnames(rds_dt,label_c1,labels,skip_absent = TRUE)
  rds_dt[,("cnt"):=1:.N]
  if (file.exists(rds_path)){
    old_rds_dt <- readRDS(rds_path) 
    today_date <- strftime(Sys.time(),"%y%m%d%H%M%S")
    saveRDS(rds_dt,paste0(maindir,"sam_schemas/","sam_schema",today_date,".RDS"))
    rds_dt <- old_rds_dt[rds_dt,on="cnt"]
    file.remove(rds_path)
  }
  saveRDS(rds_dt,rds_path)
}

tests_download_data <- function(dt,label_c1,row_c1,total_str,state){
  setkey(dt,"name")
  name_string <- dt[,name]
  total_name <- name_string[str_detect(name_string,"_001")]
  if(length(total_name)>1){
    tn_dt <- as.data.table(total_name)
    total_name <- tn_dt[min(length(total_name))]
  }
  suppressWarnings(
    total_pop <- sum(as.integer(dt[total_name,.SDcols = startsWith(names(dt),state)]),na.rm = TRUE))
  print(paste0("Total population for ",total_name," is: ",total_pop))
  dt[,("total"):=0] #in case it's second time through...
  suppressWarnings( #b/c NAs introduced by coercion, but that is the desired outcome
    dt[row_c1,("total"):=sum(as.integer(.SD,.SDcols = startsWith(names(dt),state)),na.rm = TRUE),by=.I])
  if(total_pop == sum(dt[row_c1,"total"])){
    print("Total populations agree between total row and total of selected rows")
  }else{ 
    print("Total and total of selected rows do not agree")
  }
  return(dt[total_name])
}

census_block_get <- function(censusdir,vintage,state,censuskey,groupname,county_num,api_type,path_suff){
  geo_type <- "block_group"
  file_path <- valid_file_path(censusdir,vintage,state,county_num,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    if (file.exists(str_replace(file_path,".RDS","_relabeled.RDS"))){
      result <- read_rds(str_replace(file_path,".RDS","_relabeled.RDS"))
      print(paste0("Reading file from ", str_replace(file_path,".RDS","_relabeled.RDS")))
    }else{
      result <- read_rds(file_path)
      print(paste0("Reading file from ", file_path))
    }
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err"){
      census_variables[,("name"):=str_replace(name,".{1}$","M")][
        ,("label"):=str_replace(name,"Estimate!!Total","Margin of Error")]
    }
    census_vars_labels <- split_labels(census_variables)
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME",census_variables$name),
                               region = paste0("block group:*"), 
                               regionin = paste0("state:", state,"+county:",county_num,"+tract:*"),
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) 
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_dt[,("GEOID_15"):=paste0(state,"_",county,"_",tract,"_",block_group)]
    data_dt <- data_for_vars_dt[,6:ncol(data_for_vars_dt)]
    data_for_vars_tr <- data.table(table_name = names(data_dt),t(data_dt))
    colnames(data_for_vars_tr) <- c("name",data_dt[,GEOID_15])
    result <- census_vars_labels[data_for_vars_tr,on="name"]
    suppressWarnings( #b/c NAs introduced by coercion, but that is the desired outcome
      result[,names(.SD):=lapply(.SD,as.numeric),.SDcols=startsWith(names(result),paste0(state,"_"))])
    write_rds(result,file_path)
    percent_na <- data_for_vars_tr[,sum(is.na(.SD))] / (data_for_vars_tr[,sum(!is.na(.SD))]+data_for_vars_tr[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    concept <- census_variables[1,"concept"]
    theme <- "Decennial Census"
    geo_type <- "block_group"
    tool <- "censusapi"
    citation <- "Decennial U.S. Census"
    rel_file_path <- str_remove(file_path,censusdir)
    notes <- ""
    write_download_metadata(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,rel_file_path,tool,citation,notes)
  }
  return(result)
}

census_tract_get <- function(censusdir,vintage,state,censuskey,groupname,county,api_type,path_suff){
  geo_type <- "tract"
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    if (file.exists(str_replace(file_path,".RDS","_relabeled.RDS"))){
      result <- read_rds(str_replace(file_path,".RDS","_relabeled.RDS"))
      print(paste0("Reading file from ", str_replace(file_path,".RDS","_relabeled.RDS")))
    }else{
      result <- read_rds(file_path)
      print(paste0("Reading file from ", file_path))
    }
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err"){
      census_variables[,("name"):=str_replace(name,".{1}$","M")][
        ,("label"):=str_replace(name,"Estimate!!Total","Margin of Error")]
    }
    census_vars_labels <- split_labels(census_variables)
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME",census_variables$name),
                               region = paste0("tract:*"), 
                               regionin = paste0("state:", state),
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_dt[,("GEOID"):=paste0(state,county,tract)]
    data_dt <- data_for_vars_dt[,6:ncol(data_for_vars_dt)]
    data_for_vars_tr <- data.table(table_name = names(data_dt),t(data_dt))
    colnames(data_for_vars_tr) <- c("name",data_dt[,GEOID])
    result <- census_vars_labels[data_for_vars_tr,on="name"]
    suppressWarnings( #b/c NAs introduced by coercion, but that is the desired outcome
      result[,names(.SD):=lapply(.SD,as.numeric),.SDcols=startsWith(names(result),paste0(state,"_"))])
    write_rds(result,file_path)
    percent_na <- data_for_vars_tr[,sum(is.na(.SD))] / (data_for_vars_tr[,sum(!is.na(.SD))]+data_for_vars_tr[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    concept <- census_variables[1,"concept"]
    theme <- "Decennial Census"
    geo_type <- "block_group"
    tool <- "censusapi"
    citation <- "Decennial U.S. Census"
    rel_file_path <- str_remove(file_path,censusdir)
    notes <- ""
    write_download_metadata(maindir,concept,vintage,state,county,theme,groupname,api_type,geo_type,rel_file_path,tool,citation,notes)
  }
  return(result)
}

#use state="US" b/c that's what it returns, in any case (test) -NOT TESTED!
census_zcta_get <- function(censusdir,vintage,state,censuskey,groupname,county,api_type,path_suff){
  geo_type <- "zcta"
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff)
  if (file.exists(file_path)){
    if (file.exists(str_replace(file_path,".RDS","_relabeled.RDS"))){
      result <- read_rds(str_replace(file_path,".RDS","_relabeled.RDS"))
      print(paste0("Reading file from ", str_replace(file_path,".RDS","_relabeled.RDS")))
    }else{
      result <- read_rds(file_path)
      print(paste0("Reading file from ", file_path))
    }
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err"){
      census_variables[,("name"):=str_replace(name,".{1}$","M")][
        ,("label"):=str_replace(name,"Estimate!!Total","Margin of Error")]
    }
    census_vars_labels <- split_labels(census_variables)
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME", census_variables$name),
                               region = "zip code tabulation area:*", 
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #data_for_vars_dt[,names(.SD):=lapply(.SD,numeric),.SDcols = str_detect(state,names(data_for_vars_dt))]
    #columns are table names; rows are geographic area (block groups)
    data_dt <- data_for_vars_dt[,6:ncol(data_for_vars_dt)]
    data_for_vars_tr <- data.table(table_name = names(data_dt),t(data_dt))
    colnames(data_for_vars_tr) <- c("name",data_dt[,GEOID_15])
    result <- census_vars_labels[data_for_vars_tr,on="name"]
    #result[,names(.SD):=lapply(.SD,as.numeric),.SDcols=startsWith(names(result),paste0(ztcaSOMEHOW,"_"))]
    write_rds(result,file_path)
    percent_na <- data_for_vars_tr[,sum(is.na(.SD))] / (data_for_vars_tr[,sum(!is.na(.SD))]+data_for_vars_tr[,sum(is.na(.SD))])
    print(paste("Percentage of NAs in file:",as.integer(100*percent_na)))
    print(paste0("Newly retrieved data was written to disk as .RDS at: ",file_path))
    concept <- census_variables[1,"concept"]
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
    if (file.exists(str_replace(file_path,".RDS","_relabeled.RDS"))){
      result <- read_rds(str_replace(file_path,".RDS","_relabeled.RDS"))
      print(paste0("Reading file from ", str_replace(file_path,".RDS","_relabeled.RDS")))
    }else{
      result <- read_rds(file_path)
      print(paste0("Reading file from ", file_path))
    }
  }else{
    census_variables <- valid_census_vars(censusdir, vintage, api_type, groupname)
    if(path_suff=="err"){
      census_variables$name <- paste0(substr(census_variables$name,1,nchar(as.character(census_variables$name))-1),"M") #MA - margin annotation; none for sex_age_race
      census_variables$label <- paste0(str_replace(census_variables$label,"Estimate!!Total","Margin of Error"))
    }
    data_for_vars <- getCensus(name = api_type,
                               vintage = vintage,
                               vars = c("NAME", census_variables$name),
                               region = "zip code tabulation area:*", 
                               key = censuskey)
    data_for_vars_dt <- as.data.table(data_for_vars) #as.data.table(data_for_vars_state)
    #data_for_vars_dt[,names(.SD):=lapply(.SD,numeric),.SDcols = str_detect(state,names(data_for_vars_dt))]
    #columns are table names; rows are geographic area (block groups)
    data_for_vars_tr <- data.table(table_name = names(data_for_vars_dt),t(data_for_vars_dt))
    colnames(data_for_vars_tr) <- c("name",data_for_vars_dt[,zip_code_tabulation_area])
    result <- census_variables[data_for_vars_tr,on="name"]
    write_rds(result,file_path)
    percent_na <- data_for_vars_tr[,sum(is.na(.SD))] / (data_for_vars_tr[,sum(!is.na(.SD))]+data_for_vars_tr[,sum(is.na(.SD))])
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


