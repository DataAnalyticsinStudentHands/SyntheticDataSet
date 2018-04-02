# Repaint Eligibility App
The city of Houston has a program to repaint houses that could possibly have lead in the current paint. This app helps rule out houses that are not eligible either because they are in a floodplain or aren't old enough to qualify.

The script repaint_eligibility.R relies on a shapefile of the houses in Houston as well as their addresses, a shapefile for the floodplains, and a shapefile for the city of Houston. When sourced it generates 4 csv files called by the app so that a user can query an address to see if it is eligible for the program. The app is generated after sourcing the repaint_eligibility.R script by sourcing either ui.R or server.R
