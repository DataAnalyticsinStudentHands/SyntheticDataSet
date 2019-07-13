This is a function to add life expectancy from the Center for Disease Control's U.S. Small-area Life Expectancy Estimates Project (USALEEP)
to Sam City.

USALEEP is the Center for Disease Control's estimates of life expectancy at birth for most of the census tracts in the United States for 
the period 2010-2015. More information can be found at https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html

TX_B.csv is the Abridged Period Life Table File for Texas and was downloaded from the above link. This data was used in the R Script to add
a life expectancy variable to Sam City.

The following is an example of calling the function:

Sam_City = getLifeExpectancy(tract, Sam_City, Life_Expectancy_Data)

The package broman must be installed and loaded for the function to work.
