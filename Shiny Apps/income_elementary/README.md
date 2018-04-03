# Create Income for tracts in Elementary School Zones of Interest
This creates an app to make a choropleth of Houston on the tract level for percentage of the population living in poverty or with public assistance with the school zones of schools with a higher than normal percentage of kids with asthma.

The application relies on a shapefile of school zones from http://cohgis-mycity.opendata.arcgis.com/datasets/hisd-elementary-boundary a list of school zones to be featured in a csv file and a csv of selected income variables from SimplyAnalytics.

The script organize_data_for_maps.R should be sourced first, then either the script ui.R or server.R can be sourced to display the app.
