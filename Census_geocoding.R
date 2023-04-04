#https://cran.r-project.org/web/packages/censusxy/vignettes/censusxy.html
library(censusxy)
library(readxl)
library(data.table)
library(sf)
library(dplyr)

#in case we don't have directories named
maindir = "~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at home
#maindir = "~/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at work
housingdir = paste0(maindir,"HCAD/")
houstondatadir = paste0(maindir,"HoustonCityData/") 
censusdir = paste0(maindir,"Census/") 
vintage = "2021"
#numberOfCores = 1
state = "48" #48 Texas; 22 Louisiana
county = "201" #8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller ; other place FIPS are longer
st_county = paste0(state,county) #"48201"

#get block data from geojson_out.R
#st_crs(censusblocks)

fatal_police_shootings_wp <- read.csv(paste0(maindir,"WP_Police_shootings/fatal_police_shootings_wp_thru_4_3_23.txt"))
dt_fatal_police_shootings_wp <- as.data.table(fatal_police_shootings_wp)
dt_fatal_police_shootings_wp <- dt_fatal_police_shootings_wp[state=="TX"]
missing_fatal_police_shootings_wp <- dt_fatal_police_shootings_wp[is.na(longitude)]
dt_fatal_police_shootings_wp <- dt_fatal_police_shootings_wp[!is.na(longitude)] #70 missing location in TX!

sf_fatal_police_shootings_wp <- st_as_sf(dt_fatal_police_shootings_wp,coords = c("longitude", "latitude"),
                                         crs = 4269, agr = "constant")

blocks4fatal_police <- st_within(sf_fatal_police_shootings_wp, censusblocks)
blocks4fatal_police_unlisted <- rapply(blocks4fatal_police,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
blocks4fatal_police_unlisted <- unlist(blocks4fatal_police_unlisted)
dt_fatal_police_shootings_wp$censusblock=censusblocks$GEOID[blocks4fatal_police_unlisted]
dt_fatal_police_shootings_wp[,("GEOID"):=as.numeric(substr(censusblock,1,11))]

tracts4baqir_dt <- as.data.table(tracts4baqir)
tracts_fatal_police_shootings <- dt_fatal_police_shootings_wp[tracts4baqir_dt,on="GEOID"]

fatal_police_shootings_agencies_wp <- read.csv(paste0(maindir,"WP_Police_shootings/fatal_police_shootings_agencies_wp_4_3_23.txt"))
colnames(tracts_fatal_police_shootings)[colnames(tracts_fatal_police_shootings)=="NAME"] <- "NAME_ID"
st_write(tracts_fatal_police_shootings,"~/Downloads/tracts_2021_fatal_police_shootings_4_3_23.csv",driver = "CSV",factorsAsCharacter=FALSE,
         layer_options = "GEOMETRY=AS_WKT")
st_write(tracts_fatal_police_shootings,"~/Downloads/tracts_2021_fatal_police_shootings_4_3_23.geojson",driver = "GeoJSON",factorsAsCharacter=FALSE)


#import data in right format
crime_report_1 <- read_xlsx(paste0(houstondatadir,"2022/HPD_crime/2019_NIBRSPublicView.Jan1-Dec31.xlsx"))
#sort by address, so it only looks up once per address
crime_report_2 <- read_xls(paste0(houstondatadir,"2022/HPD_crime/apr10.xls"))
crime_report_2 <- crime_report_2 %>%
  group_by(`Street Name`,`Block Range`) %>%
  mutate(address = paste0(str_split(`Block Range`,"-")[[1]][1],
                          " ",ifelse(Suffix!="-",Suffix,""),
                          " ",`Street Name`," ",ifelse(Type!="-",Type," ")
                          ),
         end_address = paste0(str_split(`Block Range`,"-")[[1]][2],
                              " ",ifelse(Suffix!="-",Suffix,""),
                              " ",`Street Name`," ",ifelse(Type!="-",Type," ")
                          ),
         city = "Houston",
         state = "TX") 
crime_rep_2 <- crime_report_2 %>%
  distinct(address,.keep_all = TRUE)

system.time(
cr2_sf <- cxy_geocode(crime_rep_2, street = "address", 
                      city = "city", 
                      state = "state", 
                      class="dataframe")
)

#for whole folder and merge:https://stackoverflow.com/questions/68397122/how-to-merge-files-in-a-directory-with-r
library(tidyr)
library(purrr)

path <- paste0(houstondatadir,"2022/HPD_crime")

#looks like I need to do each, since they have slight differences between files
# in one pipeline:
full_crime_data  <- path %>% 
  
  # get csvs full paths. (?i) is for case insentitive
  list.files(pattern = "(?i)\\.xlsx$", full.names = TRUE) %>% 
  map_dfr(read.csv)
  
  # create a named vector: you need it to assign ids in the next step.
  # and remove file extension to get clean colnames
  #set_names(tools::file_path_sans_ext(basename(.))) %>% 
  
  # read file one by one, bind them in one df and create id column 
  #map_dfr(read.csv, col.names = c("Wavenumber", "V2"), .id = "colname")

