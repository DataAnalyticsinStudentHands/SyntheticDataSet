# Aging functions

These are functions that are meant to model the change in Houston from one year to the next. They rely on a built model, HCAD data found here http://pdata.hcad.org/download/index.html and U.S. Census data that can be accessed using the Census_Data_API function in the citymodels folder.

# Brainstorming folder
These are previous scripts used to plan and test functions for aging. This folder can be deleted once this project is finished.

# Master Example
This script is meant to age a model created of Houston from 2014 HCAD and Census data, and age it to 2015. It calls the scripts and functions listed below.

# get_age_from_brackets.R
The 2014 model created from the citymodels package lists ages in Census data defined brackets to make things easier and keep better continuity over multiple years this is converted to a numerical age with the get_age_from_brackets function.

# find_ACCOUNT_numbers_that_moved_out.R
This function takes two sets of shapefiles already merged and cleaned appropriately with the prepareHCADparcels.R script in the merging with HCAD functions folder and looks for either discontinued account numbers or changes in ownership to determine the first set of families to move out.

# move_out_by_tract.R
If not enough people are moved out by find_ACCOUNT_numbers_that_moved_out.R than it is likely that many renters or parts of families moved out that didn't cause a change in ownership for the HCAD files. The move_out_by_tract function moves more people out by age if the total number of people living in the same house as last year in a Census tract is less than the number still in that tract for the model.


