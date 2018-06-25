# Aging functions

These are functions that are meant to model the change in Houston from one year to the next. They rely on a built model, HCAD data found here http://pdata.hcad.org/download/index.html and U.S. Census data that can be accessed using the Census_Data_API function in the citymodels folder.

# Brainstorming folder
These are previous scripts used to plan and test functions for aging. This folder can be deleted once this project is finished.

# Master Example
This script is meant to age a model created of Houston from 2014 HCAD and Census data, and age it to 2015. It calls the scripts and functions listed below.

# get_age_from_brackets.R
The 2014 model created from the citymodels package lists ages in Census data defined brackets to make things easier and keep better continuity over multiple years this is converted to a numerical age with the get_age_from_brackets function.

