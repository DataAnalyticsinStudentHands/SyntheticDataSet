
## citymodels package

Citymodels is an R package made to build models of city populations to use in investigating public health questions. The current package uses data mined from the U.S. Census to model populations by tract. The development code for this can be found in the [citymodels folder](https://github.com/DataAnalyticsinStudentHands/SyntheticDataSet/tree/master/citymodels). 

### Install

The source package is included as citymodel_0.1.0.tar.gz in the parent SyntheticDataSet folder and can be downloaded and installed into R using the install.packages function with the type option set to "source". 

```R
install.packages("citymodels_0.1.0.tar.gz", repos = NULL, type = "source")
```
## Mining Census data

As citymodels builds it's models based off of U.S. Census information, it includes code to mine the appropriate data. This code is included in the scripts map_variables_from_Census_Table.R and getcensusdata.R. The script map_variables_from_Census_Table.R declares variables used in the simulations with the values specifying the appropriate heading in the U.S. Census. This script may need to be updated if the Census headings change. The script getcensusdata.R includes a function to mine these variables and create a dataframe for the simulations, it is called with the base_url, fips code for the state, and key needed to use the API. The base_url specifies what year to mine for and to use the American Community Survey 5 year survey detail tables, the base_url can be found here https://www.census.gov/data/developers/data-sets/acs-5year.2014.html as the API call under Detail Tables, after selecting for the year. The base_url for years 2010-2014 will be changed on August 30, 2018. Make sure the most up to date base_url is being used. The FIPS code for the state can be looked up here under FIPS code for the States and District of Columbia https://www.census.gov/geo/reference/ansi_statetables.html. The key is used to access the API, and can be applied for here https://www.census.gov/developers/ by clicking on "Request a Key" found on the left side of the page.  

```R
census_data_for_model=census_data_API(base_url='http://api.census.gov/data/2014/acs5?',state=48,key="your_key_here")
```

## Modeling Populations

The U.S. Census includes data on people living in households and people living in group quarters which can include dormitories, assisted living facilities, and other non-household structures. The citymodels package models these populations somewhat differently as group quarters populations don't have the same kind of household characteristics. Households are simulated using the household_generator function which calls functions in the household_functions.R and individual_functions.R scripts to sample characteristics for households and individuals in each household using the Census data for each tract. It is called with the state, county and tract of interest as well as the seed to use for sampling, and the Census data object the user wants used. Below is an example for a tract of interest in Houston.

```R
Houston_tract_312100=household_generator(state=48,county=201,tract=312100,seed=1,Census_data=census_data_for_model)
```

The function group_quarters_simulater is similar to the household_generator function except for household characteristics it calls functions from the group_quarters_functionr.R script to sample similar characteristics and specify that it is for group quarters. It is also called with the state, county and tract of interest as well as the seed to use for sampling, and the Census data object the user wants used. Below is an example for a tract of interest in Houston.

```R
Houston_tract_312100=group_quarters_simulater(state=48,county=201,tract=312100,seed=1,Census_data=census_data_for_model)
```
