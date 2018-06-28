# Aging functions

These are functions that are meant to model the change in Houston from one year to the next. They rely on a built model, HCAD data found here http://pdata.hcad.org/download/index.html and U.S. Census data that can be accessed using the Census_Data_API function in the citymodels folder.

# Brainstorming folder
These are previous scripts used to plan and test functions for aging. This folder can be deleted once this project is finished.

# Master Example
This script is meant to age a model created of Houston from 2014 HCAD and Census data, and age it to 2015. It calls the scripts and functions listed below. There are two versions one using people_moving_in1 and another using people_moving_in2. These are two different ways of modeling the people moving in to a tract and they are explained below.

# get_age_from_brackets.R
The 2014 model created from the citymodels package lists ages in Census data defined brackets to make things easier and keep better continuity over multiple years this is converted to a numerical age with the get_age_from_brackets function.

# find_ACCOUNT_numbers_that_moved_out.R
This function takes two sets of shapefiles already merged and cleaned appropriately with the prepareHCADparcels.R script in the merging with HCAD functions folder and looks for either discontinued account numbers or changes in ownership to determine the first set of families to move out.

# move_out_by_tract.R
If not enough people are moved out by find_ACCOUNT_numbers_that_moved_out.R than it is likely that many renters or parts of families moved out that didn't cause a change in ownership for the HCAD files. The move_out_by_tract function moves more people out by age if the total number of people living in the same house as last year in a Census tract is less than the number still in that tract for the model.

# create_babies.R
This function simulates babies born to mothers still living in the tract after enough people have been moved out. The number of babies born in the tract is determined by the difference between the Census data on number of children under 5 that have been living in the same house as last year and the number of children under 5 living in the tract after everyone has been moved out and before simulating people moving in. The babies race and gender is simulated by creating a probability vector based off of the difference between the race and gender of children under 5 in the Census data and in the model. After the baby's race has been simulated, a possible mother is sampled from the adult women still living in the tract of a similar racial background. The baby inherits the householdID of the mother and the household size for that household is increased by 1.

# people_moving_in1.R
This function simulates the people moving in to the tract by the Census data on age, race and gender of people moving in the tract from either within the county, within the state, out of state, or out of nation. For people within the county it uses the dataframe of people that moved out to match for households that would fit into the tract by the Census data. For people from outside of the county, a probability vector is built for age, race, and gender from the Census data and the people are simulated from sampling. They are then put into households. Household size is determined by tabling the difference in household sizes between the model and the Census data. Then people are put into households to make up for the differences between sizes.

# people_moving_in2.R
This function simulates the people moving in to the tract by the difference between the tabled race, age and gender of the people in the model, and Census estimates of people by race, age and gender in the tract. For people within the county it uses the dataframe of people that moved out to match for households that would fit into the tract based off of this difference. The difference is then made into a probability vector which is used to sample the race, age, and gender of people from outside of the county The number to simulate from each region is determined by a ratio of the number of people from each region in the Census and the total needed to move to the number of people that should be in the tract.They are then put into households. Household size is determined by tabling the difference in household sizes between the model and the Census data. Then people are put into households to make up for the differences between sizes.
