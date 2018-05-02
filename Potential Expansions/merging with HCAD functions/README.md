# Harris County Appraisal District functions
This folder contains scripts for cleaning and subsetting the Harris County Appraisal District shapefiles, and placing simulated households from the citymodels dataset into geographical houses.

The script prepare_HCAD_parcels.R should be sourced first with the shapefiles and accompanying txt files from HCAD available here: http://pdata.hcad.org/download/index.html. Text files of interest include Building_res.txt, Building_res.txt, and Structural_elem1.txt. The shapefiles for tracts shoould also be included in the working directory. The script will filter and write 2 RDS files with valid geographical parcels of structures that people should be able to live in for both households and group quarters.

The script mergeHCADparcels.R should be sourced with the model and outputs of the script prepare_HCAD_parcels.R to place simulated households into physical structures. As this script takes too long to run it was made to run in parallel with mergeHCADparcelsparallel.R

The merge matches households and group quarters populations into geographical locations of sructures based on the harris county appraisal district building style code. A list of the codes is available here: http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ .

Single households were placed in the following building style codes
+ 101- Residential 1 Family
+ 107- Townhome with Common Element
+ 108- Single Wide Residential Mobile Home
+ 109- Double Wide Residential Mobile Home
+ 125- Farm with Dwelling
+ 8177- Townhouse, High-rise- End Unit
+ 8178- Townhouse, High-rise- Inside Unit
+ 8179- Townhouse, High-rise- Detached
+ 8351- Single-Family Residence
+ 8354- Townhouse, Inside Unit
+ 8401- Townhouse, End Unit
+ 8548-	Urban Row House, Detached
+ 8549-	Urban Row House, End Unit
+ 8550-	Urban Row House, Inside Unit
+ 8986- Int. Space, Townhouse, Inside Unit
+ 8988- Int. Space, Townhouse, End Unit

Two households were placed in the following building style code
+ 102- Residential 2 Family

Three households were placed in the following building style code
+ 103- Residential 3 Family

Four households were placed in the following building style code
+ 104- Residential 4 Family

All remaining simulated households were placed randomly into buildings with the following building style codes, sometimes multiple households per building
+ 105- Mixed Res/Com, Res Structure
+ 8300- Apartment
+ 8352- Multiple Res (Low Rise)
+ 8338- Loft
+ 8459- Mixed Retail w/ Resid. Units
+ 8493- Flathouse
+ 8546- Senior Citizen Townhouse, End Unit
+ 8547- Senior Citizen Townhouse, Inside Unit
+ 8596- Shell, Apartment
+ 8984- Luxury Apartment
+ 8987- Int. Space, Multiple Resid.
+ 8989- Int. Space, Apartment
