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
#Household type and race
vars <- c("B23001_005E","B23001_007E","B23001_008E","B23001_009E","B23001_012E","B23001_014E","B23001_015E","B23001_016E","B23001_019E","B23001_021E","B23001_022E","B23001_023E","B23001_026E","B23001_028E","B23001_029E","B23001_030E","B23001_033E","B23001_035E","B23001_036E","B23001_037E","B23001_040E","B23001_042E","B23001_043E","B23001_044E","B23001_047E","B23001_049E","B23001_050E","B23001_051E","B23001_054E","B23001_056E","B23001_057E","B23001_058E","B23001_061E","B23001_063E","B23001_064E","B23001_065E","B23001_068E","B23001_070E","B23001_071E","B23001_072E","B23001_075E","B23001_076E","B23001_077E","B23001_080E","B23001_081E","B23001_082E","B23001_085E","B23001_086E","B23001_087E","B23001_091E","B23001_093E","B23001_094E","B23001_095E","B23001_098E","B23001_100E","B23001_101E","B23001_102E","B23001_105E","B23001_107E","B23001_108E","B23001_109E","B23001_112E","B23001_114E","B23001_115E","B23001_116E","B23001_119E","B23001_121E","B23001_122E","B23001_123E","B23001_126E","B23001_128E","B23001_129E","B23001_130E","B23001_133E","B23001_135E","B23001_136E","B23001_137E","B23001_140E","B23001_142E","B23001_143E","B23001_144E","B23001_147E","B23001_149E","B23001_150E","B23001_151E","B23001_154E","B23001_156E","B23001_157E","B23001_158E","B23001_161E","B23001_162E","B23001_163E","B23001_166E","B23001_167E","B23001_168E","B23001_171E","B23001_172E","B23001_173E")
# Get data for all census tracts in TX
census_data <- getCensusApi(acs5_2014_api, key=key, vars, region="for=tract:*&in=state:48")

#write data frame to csv file without quotes



write.csv(census_data, file = "employment.csv",row.names=FALSE, na="", quote = FALSE)