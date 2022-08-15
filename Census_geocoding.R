#https://cran.r-project.org/web/packages/censusxy/vignettes/censusxy.html
library(censusxy)
library(readxl)
#library(data.table)
library(sf)

#in case we don't have directories named
maindir = "~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at home
#maindir = "~/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at work
housingdir = paste0(maindir,"HCAD/")
houstondatadir = paste0(maindir,"HoustonCityData/") 
censusdir = paste0(maindir,"Census/") 
vintage = "2010"
#numberOfCores = 1
state = "48" #48 Texas; 22 Louisiana
county = "201" #8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller ; other place FIPS are longer
st_county = paste0(state,county) #"48201"

#import data in right format
crime_report_1 <- read_xlsx(paste0(houstondatadir,"2022/HPD_crime/2019_NIBRSPublicView.Jan1-Dec31.xlsx"))
#sort by address, so it only looks up once per address
