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
#Household size and number of vehicles
vars <- c("B15001_004E","B15001_005E","B15001_006E","B15001_007E","B15001_008E","B15001_009E","B15001_010E","B15001_012E","B15001_013E","B15001_014E","B15001_015E","B15001_016E","B15001_017E","B15001_018E","B15001_020E","B15001_021E","B15001_022E","B15001_023E","B15001_024E","B15001_025E","B15001_026E","B15001_028E","B15001_029E","B15001_030E","B15001_031E","B15001_032E","B15001_033E","B15001_034E","B15001_036E","B15001_037E","B15001_038E","B15001_039E","B15001_040E","B15001_041E","B15001_042E","B15001_045E","B15001_046E","B15001_047E","B15001_048E","B15001_049E","B15001_050E","B15001_051E","B15001_053E","B15001_054E","B15001_055E","B15001_056E","B15001_057E","B15001_058E","B15001_059E","B15001_061E","B15001_062E","B15001_063E","B15001_064E","B15001_065E","B15001_066E","B15001_067E","B15001_069E","B15001_070E","B15001_071E","B15001_072E","B15001_073E","B15001_074E","B15001_075E","B15001_077E","B15001_078E","B15001_079E","B15001_080E","B15001_081E","B15001_082E","B15001_083E")
# Get data for all census tracts in TX
census_data <- getCensusApi(acs5_2014_api, key=key, vars, region="for=tract:*&in=state:48")

#write data frame to csv file without quotes
write.csv(census_data, file = "education_attainment_by_sex_by_age.csv",row.names=FALSE, na="", quote = FALSE)