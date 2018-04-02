# Harris County Appraisal District functions
This folder contains scripts for cleaning and subsetting the Harris County Appraisal District shapefiles, and placing simulated households from the citymodels dataset into geographical houses.

The script prepare_HCAD_parcels.R should be sourced first with the shapefiles and accompanying txt files from HCAD available here: http://pdata.hcad.org/download/index.html. Text files of interest include Building_res.txt, Building_res.txt, and Structural_elem1.txt. The shapefiles for tracts shoould also be included in the working directory. The script will filter and write 2 RDS files with valid geographical parcels of structures that people should be able to live in for both households and group quarters.

The script mergeHCADparcels.R should be sourced with the model and outputs of the script prepare_HCAD_parcels.R to place simulated households into physical structures.
