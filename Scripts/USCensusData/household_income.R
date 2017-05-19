# Code created using
# http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html

# getCensusApi
# get Census data via the public API: loop through variables if needed
# Inputs:
#   data_url: the url root of the api, including the '?'
#     example: http://api.census.gov/data/2010/sf1?
#   key: your API key
#   vars: a character vector of variables to get.
#     example c("H0110001","H0110002","H0110003")
#     If there are more than 50, then it will be automatically split into separate queries.
#   region: region to get data for.  contains a for:, and possibly an in:
#     example: for=block:1213&in=state:47+county:015+tract:*
# Output:
#   If successful, a data.frame
#   If unsuccessful, prints the url query that caused the error.
getCensusApi <- function(data_url,key, vars, region, numeric=TRUE){
  if(length(vars)>50){
    vars <- vecToChunk(vars) # Split vars into a list
    get <- lapply(vars, function(x) paste(x, sep='', collapse=","))
    data <- lapply(vars, function(x) getCensusApi2(data_url,key, x, region, numeric=TRUE))
  } else {
    get <- paste(vars, sep='', collapse=',')
    data <- list(getCensusApi2(data_url,key, get, region, numeric=TRUE))
  }
  # Format output.  If there were no errors, than paste the data together
  # If there is an error, just return the unformatted list.
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind,data)
    names(data) <- colnames
    # Prettify the output
    # If there are nonunique colums, remove them
    data <- data[,unique(colnames, fromLast=TRUE)]
    # Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class)!='numeric'), which(sapply(data, class)=='numeric'))]
    return(data)
  }else{
    print('unable to create single data.frame in getCensusApi')
    return(data)
  }
}

# getCensusApi2 
# get Census data via the public API using a single query
# Inputs:
#   data_url: the url root of the api, including the '?'
#     example: http://api.census.gov/data/2010/sf1?
#   key: your API key
#   get: The variables to get. Separate multiple variables by commas.
#     example 'H0110001,H0110002,H0110003'
#   region: region to get data for.  contains a for:, and possibly an in:
#     example: for=block:1213&in=state:47+county:015+tract:*
# Output:
#   If successful, a data.frame
#   If unsuccessful, prints the url query that was constructed.
getCensusApi2 <- function(data_url,key, get, region, numeric=TRUE){
  if(length(get)>1) get <- paste(get, collapse=',', sep='')
  api_call <- paste(data_url, 
                    'key=', key, 
                    '&get=', get,
                    '&', region,
                    sep='')
  
  dat_raw <- try(readLines(api_call, warn="F"))
  if(class(dat_raw)=='try-error') {
    print(api_call)
    return}
  dat_df <- data.frame()
  
  #split the datastream into a list with each row as an element
  # Thanks to roodmichael on github
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  #dat_df <- rbind(dat_df, t(sapply(tmp, '[')))
  #names(dat_df) <- sapply(dat_df[1,], as.character)
  #dat_df <- dat_df[-1,]
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors=FALSE)
  names(dat_df) <- tmp[[1]]
  # convert to numeric
  # The fips should stay as character... so how to distinguish fips from data?
  # I think all of the data have numbers in the names, the fips do not
  #  Example: field names of B01001_001E vs state
  if(numeric==TRUE){
    value_cols <- grep("[0-9]", names(dat_df), value=TRUE)
    for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  }
  return(dat_df)
}

vecToChunk <- function(x, max=50){
  s <- seq_along(x)
  x1 <- split(x, ceiling(s/max))
  return(x1)
}


#api key generated for dash@uh.edu
key <- "6ee9b8141913fdd7763ff46af20c20d0e9a5bc68"

#api link for housing data 
acs5_2014_api <- 'http://api.census.gov/data/2014/acs5?'
#variables used
#Household Income
#This used to grb household income by race now it needs to be just income
#vars <- c("B19001B_002E","B19001B_003E","B19001B_004E","B19001B_005E","B19001B_006E","B19001B_007E","B19001B_008E","B19001B_009E","B19001B_010E","B19001B_011E","B19001B_012E","B19001B_013E","B19001B_014E","B19001B_015E","B19001B_016E","B19001B_017E","B19001C_002E","B19001C_003E","B19001C_004E","B19001C_005E","B19001C_006E","B19001C_007E","B19001C_008E","B19001C_009E","B19001C_010E","B19001C_011E","B19001C_012E","B19001C_013E","B19001C_014E","B19001C_015E","B19001C_016E","B19001C_017E","B19001D_002E","B19001D_003E","B19001D_004E","B19001D_005E","B19001D_006E","B19001D_007E","B19001D_008E","B19001D_009E","B19001D_010E","B19001D_011E","B19001D_012E","B19001D_013E","B19001D_014E","B19001D_015E","B19001D_016E","B19001D_017E","B19001E_002E","B19001E_003E","B19001E_004E","B19001E_005E","B19001E_006E","B19001E_007E","B19001E_008E","B19001E_009E","B19001E_010E","B19001E_011E","B19001E_012E","B19001E_013E","B19001E_014E","B19001E_015E","B19001E_016E","B19001E_017E","B19001F_002E","B19001F_003E","B19001F_004E","B19001F_005E","B19001F_006E","B19001F_007E","B19001F_008E","B19001F_009E","B19001F_010E","B19001F_011E","B19001F_012E","B19001F_013E","B19001F_014E","B19001F_015E","B19001F_016E","B19001F_017E","B19001G_002E","B19001G_003E","B19001G_004E","B19001G_005E","B19001G_006E","B19001G_007E","B19001G_008E","B19001G_009E","B19001G_010E","B19001G_011E","B19001G_012E","B19001G_013E","B19001G_014E","B19001G_015E","B19001G_016E","B19001G_017E","B19001H_002E","B19001H_003E","B19001H_004E","B19001H_005E","B19001H_006E","B19001H_007E","B19001H_008E","B19001H_009E","B19001H_010E","B19001H_011E","B19001H_012E","B19001H_013E","B19001H_014E","B19001H_015E","B19001H_016E","B19001H_017E","B19001I_002E","B19001I_003E","B19001I_004E","B19001I_005E","B19001I_006E","B19001I_007E","B19001I_008E","B19001I_009E","B19001I_010E","B19001I_011E","B19001I_012E","B19001I_013E","B19001I_014E","B19001I_015E","B19001I_016E","B19001I_017E")

vars <- c("B19001_002E","B19001_003E","B19001_004E","B19001_005E","B19001_006E","B19001_007E","B19001_008E","B19001_009E","B19001_010E","B19001_011E","B19001_012E","B19001_013E","B19001_014E","B19001_015E","B19001_016E","B19001_017E")
# Get data for all census tracts in TX
census_data <- getCensusApi(acs5_2014_api, key=key, vars, region="for=tract:*&in=state:48")

#write data frame to csv file without quotes
write.csv(census_data, file = "household_income",row.names=FALSE, na="", quote = FALSE)