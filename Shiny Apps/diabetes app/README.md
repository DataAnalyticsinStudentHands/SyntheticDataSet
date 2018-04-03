# Project Effects of a Diabetes Prevention Program around Houston
This application uses the base model developed from U.S. Census Data and matches with data from the Behavioral Risk Factor Surveillance System to project the number of people with prediabetes in 1 mile radius from a user specified address who could benefit from a local implementation of the CDC's Diabetes Prevention Program. The application then projects costs and savings using averages from the CDC's Diabetes Impact App. The code for the simulation can be found here: https://github.com/DataAnalyticsinStudentHands/SyntheticDataSet/tree/master/Potential%20Expansions/BRFSS 

The model is also merged with data from the Harris County Appraisal District to give geographical locations to every potential particpant and whether or not they would be in the range of the location of an intervention. The code for this simulation is available at https://github.com/DataAnalyticsinStudentHands/SyntheticDataSet/tree/master/Potential%20Expansions/merging%20with%20HCAD%20functions

Once the model is finished the user can just source either ui.R or server.R and to display the application. An address can be inputed and the projected costs and benefits of a Diabetes Prevention Program at that location will be displayed.

The app is hosted here: http://dash.hnet.uh.edu:3838/cmupchur/diabetes_app/

