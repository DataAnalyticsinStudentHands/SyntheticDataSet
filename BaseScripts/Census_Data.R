#' Census Data via API
#'
#' This function mines the data needed for the simulations from the US Census
#'
#' It calls other functions in the citymodels package in order to simulate characteristics for households and for each individual
#'
#' @param base_url The base_url for the API
#' @param state The state the user is simulating
#' @param key The key needed to use the API. Can be requested
#' @return census_data A dataframe of the Census data used for simulations in this package

census_data_via_API <- function(base_url = 'http://api.census.gov/data/2014/acs5?', state = 48, key) {
 
  # variables from mappings file
  vars <- unlist(read_json("Mappings/Census_TablesVariables.json"))
  
  vars <- unname(unlist(read_json("Mappings/Test_TablesVariables.json")))
  
  key <- "6ee9b8141913fdd7763ff46af20c20d0e9a5bc68"
  census_api_key(key)

  # Get data for all census tracts by state (48 is Texas)
  census_data <- getCensusApi(base_url, key, vars, region = paste0("for=tract:*&in=state:", state))

  #format the column names
  colnames(census_data) <- names(unlist(read_json("Mappings/Test_TablesVariables.json")))

  return(census_data)
}

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

getCensusApi <- function(data_url, key, vars, region, numeric = TRUE){
  if(length(vars) > 50){
    vars <- vecToChunk(vars) # Split vars into a list
    get <- lapply(vars, function(x) paste(x, sep = '', collapse = ","))
    data <- lapply(vars, function(x) getCensusApi2(data_url, key, x, region, numeric = TRUE))
  }
  else {
    get <- paste(vars, sep = '', collapse = ',')
    data <- list(getCensusApi2(data_url, key, get, region, numeric = TRUE))
  }

  # Format output.  If there were no errors, than paste the data together
  # If there is an error, just return the unformatted list.
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind, data)
    names(data) <- colnames

    # Prettify the output
    # If there are nonunique colums, remove them
    data <- data[,unique(colnames, fromLast = TRUE)]
    # Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class) != 'numeric'), which(sapply(data, class) == 'numeric'))]
    return(data)
  }
  else{
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

getCensusApi2 <- function(data_url, key, get, region, numeric = TRUE){
  if(length(get) > 1) get <- paste(get, collapse = ',', sep = '')
  api_call <- paste(data_url, 'key=', key, '&get=', get, '&', region, sep = '')
  dat_raw <- try(readLines(api_call, warn = "F"))

  if(class(dat_raw) == 'try-error') {
    print(api_call)
    return
  }
  dat_df <- data.frame()

  # split the datastream into a list with each row as an element
  # Thanks to roodmichael on github
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors = FALSE)
  names(dat_df) <- tmp[[1]]

  # convert to numeric
  # The fips should stay as character... so how to distinguish fips from data?
  # I think all of the data have numbers in the names, the fips do not
  # Example: field names of B01001_001E vs state
  if(numeric == TRUE){
    value_cols <- grep("[0-9]", names(dat_df), value = TRUE)
    for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  }
  return(dat_df)
}

vecToChunk <- function(x, max = 50){
  s <- seq_along(x)
  x1 <- split(x, ceiling(s/max))
  return(x1)
}
