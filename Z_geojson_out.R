library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

source('BaseScripts/Census_Data.R')
maindir = "~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at home
#maindir = "/Users/areb219/Library/CloudStorage/OneDrive-UniversityOfHouston/Social\ Network\ Hypergraphs/" #Dan on AREB219 laptop
censusdir = paste0(maindir,"Census/") 
vintage = "2020" #to use decennial, not ACS
state = 48 #48 Texas; 22 Louisiana
county = 201 #8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller ; other place FIPS are longer
st_county = paste0(state,county)
tract = "*"
#you don't need a censuskey if you're not pulling new files down; you can only use this one if you have correct access to the OneDrive
censuskey <- readLines(paste0(censusdir, "2017", "/key"))

#this depends on where your census dir is, but _tract_500k.shp and _faces and _bg, etc. are all downloaded from census, by year: 
#https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html (I got these on 7/10/21 - 2020 was most recent; got 2021 on 7/18/2022)
geo_vintage <- "2021"
all_us <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_us_all_20m/cb_", geo_vintage, "_us_state_20m/cb_2021_us_state_20m.shp"))

#zip codes, or zctas, most recent was 2020, as of 9/10/2022
#https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html
#it's the national file
zip_vintage <- "2020"
#censuszctas <- st_read(paste0(censusdir, zip_vintage, "/geo_census/cb_", zip_vintage, "_us_zcta510_500k/cb_", zip_vintage, "_us_zcta510_500k.shp"))
censuszctas <- st_read(paste0(censusdir, zip_vintage, "/geo_census/cb_", zip_vintage, "_us_zcta520_500k/cb_", zip_vintage, "_us_zcta520_500k.shp"))
zctasDT <- as.data.table(censuszctas)
zctasDT[,("centroid"):=st_centroid(geometry)] 
zctas <- st_within(zctasDT$centroid, all_us)
zctasunlisted <- rapply(zctas,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
zctasunlisted <- unlist(zctasunlisted)
zctasDT$STATEFP = all_us$STATEFP[zctasunlisted]
TXzctas <- zctasDT[STATEFP=="48"]

censusplace <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_place_500k/cb_", geo_vintage, "_", state, "_place_500k.shp"))
placeDT <- as.data.table(censusplace) #just cities

#assign to tractsDT
placeDT <- st_as_sf(placeDT)
zctas4places <- st_within(zctasDT$centroid, placeDT)
#unlist into vector
zctas4placesunlisted <- rapply(zctas4places,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
zctas4placesunlisted <- unlist(zctas4placesunlisted)
zctasDT$placename=placeDT$NAME[zctas4placesunlisted]
#
houstondatadir = paste0(maindir,"HoustonCityData/") 
superneighborhoods <- st_read(paste0(houstondatadir, "2022/HOUSTON_LIMITS_BOUNDARIES_PACKAGE/HOUSTON_LIMITS_BOUNDARIES_PACKAGE.shp"))
#2017 had different title, but same geometry
#superneighborhoods <- st_read(paste0(houstondatadir, "2017/COH_SUPER_NEIGHBORHOODS/COH_SUPER_NEIGHBORHOODS.shp"))
superneighborhoods <- st_transform(superneighborhoods, st_crs(censuszctas)) #HCAD is renamed from sf_HCAD in this run - can change
super_within <- st_within(zctasDT$centroid, superneighborhoods)
super_within_unlist <- rapply(super_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
super_within_unlist <- unlist(super_within_unlist)
zctasDT$superneighborhood=superneighborhoods$SNBNAME[super_within_unlist]

vintage <- "2022"
#for each county, can have sex_age by blck group - and thus pop by block group, too
zip_sex_by_age_race_data_from_census <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                       groupname = "B01001",county_num = county,
                       block="zip",api_type="acs/acs5",path_suff="est.csv")

#test <- getCensus(name = api_type,vintage = vintage,key = censuskey,vars = c("NAME","B01001B_029E"),region = "zip code tabulation area")
#options(digits = 2, scipen = 999) #for display on all of them

sex_by_age_race <- zip_sex_by_age_race_data_from_census %>%
  mutate(label = str_remove_all(label,"Estimate!!Total:!!"),
         race = substr(name,7,7)) %>%
  pivot_longer(4:ncol(zip_sex_by_age_race_data_from_census),names_to = "ZCTA5CE20", values_to = "number_sams")%>%
  separate(label, c("sex","age_range"), sep = ":!!", remove = F, convert = FALSE) 
#zip gets you all of US 
sex_age_race_DT <- as.data.table(sex_by_age_race,key="ZCTA5CE20")
zips_tx <- unique(zctasDT[STATEFP=="48",ZCTA5CE20])
sex_age_race_DT <- sex_age_race_DT[ZCTA5CE20%in%zips_tx]

pop_totals <- sex_age_race_DT[is.na(age_range)&race=="_"&sex=="Estimate!!Total:"]
zctas_demog <- zctasDT[pop_totals[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","total_pop")
pop_hispanic <- sex_age_race_DT[is.na(age_range)&race=="I"&sex=="Estimate!!Total:"]
zctas_demog <- zctas_demog[pop_hispanic[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","Latin_pop")
zctas_demog[,("Latin_pct"):=as.integer((as.numeric(Latin_pop)*100)/as.numeric(total_pop))]
pop_AA <- sex_age_race_DT[is.na(age_range)&race=="B"&sex=="Estimate!!Total:"]
zctas_demog <- zctas_demog[pop_AA[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","Black_pop")
zctas_demog[,("Black_pct"):=as.integer((as.numeric(Black_pop)*100)/as.numeric(total_pop))]
pop_White <- sex_age_race_DT[is.na(age_range)&race=="H"&sex=="Estimate!!Total:"]
zctas_demog <- zctas_demog[pop_White[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","White_pop")
zctas_demog[,("White_pct"):=as.integer((as.numeric(White_pop)*100)/as.numeric(total_pop))]
pop_asian <- sex_age_race_DT[is.na(age_range)&race=="D"&sex=="Estimate!!Total:"]
zctas_demog <- zctas_demog[pop_asian[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","Asian_pop")
zctas_demog[,("Asian_pct"):=as.integer((as.numeric(Asian_pop)*100)/as.numeric(total_pop))]
pop_boys_under_5 <- sex_age_race_DT[age_range=="Under 5 years"&race=="_"&sex=="Male"]
zctas_demog <- zctas_demog[pop_boys_under_5[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","boys_under_5_pop")
pop_girls_under_5 <- sex_age_race_DT[age_range=="Under 5 years"&race=="_"&sex=="Female"]
zctas_demog <- zctas_demog[pop_girls_under_5[,7:8],on="ZCTA5CE20"]
setnames(zctas_demog,"number_sams","girls_under_5_pop")
zctas_demog[,("pop_under_5"):=as.numeric(girls_under_5_pop)+as.numeric(boys_under_5_pop)]
zctas_demog[,("under_5_pct"):=as.integer(as.numeric(pop_under_5*100)/as.numeric(total_pop))]


family_med_income <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                                                groupname = "B19113",county_num = county,
                                                block="zip",api_type="acs/acs5",path_suff="est.csv")
med_family_income <- family_med_income %>%
  pivot_longer(4:ncol(family_med_income),names_to = "ZCTA5CE20", values_to = "median_family_income")
median_family_income <- as.data.table(med_family_income)
median_family_income <- median_family_income[ZCTA5CE20%in%zips_tx]
med_fam_income_total <- median_family_income[name=="B19113_001E"]
zctas_demog <- zctas_demog[med_fam_income_total[,4:5],on="ZCTA5CE20"]
zctas_demog[,("median_family_income"):=if_else(as.numeric(median_family_income)>0,median_family_income,as.numeric(0))]


#Poverty B17101 POVERTY STATUS IN THE PAST 12 MONTHS OF PEOPLE IN HOUSING UNITS
poverty <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                                         groupname = "B17101",county_num = county,
                                         block="zip",api_type="acs/acs5",path_suff="est.csv")
individuals_poverty <- poverty %>%
  pivot_longer(4:ncol(poverty),names_to = "ZCTA5CE20", values_to = "individuals_poverty")
individuals_poverty <- as.data.table(individuals_poverty)
individuals_poverty <- individuals_poverty[ZCTA5CE20%in%zips_tx]
individuals_poverty <- individuals_poverty[name=="B17101_002E",4:5]
zctas_demog <- zctas_demog[individuals_poverty, on="ZCTA5CE20"]
zctas_demog[,("poverty_pct"):=as.integer((as.numeric(individuals_poverty)*100)/as.numeric(total_pop))]

#add DLD (Delayed Language Development)
DLD <- read.csv(paste0(maindir,"/public_use_outpatient/OP_PUDF_2022_TX_delayed_language.csv"))
DLD <- as.data.table(DLD)
#get DLD by zip, with total number, number by race, number by ethnicity
DLD[,DLD_case_total_zip:=.N,by=(PAT_ZIP)]
DLD[,DLD_cases_race:=.N,by=.(PAT_ZIP,RACE)]
DLD[,Asian_DLD:=fifelse(RACE=="2",DLD_cases_race,0)]
DLD[,Black_DLD:=fifelse(RACE=="3",DLD_cases_race,0)]
DLD[,White_DLD:=fifelse(RACE=="4",DLD_cases_race,0)]
DLD[,DLD_cases_eth:=.N,by=.(PAT_ZIP,ETHNICITY)]
DLD[,Hispanic_DLD:=fifelse(ETHNICITY=="1",DLD_cases_eth,0)]
DLD[,Asian_pct_DLD:=(Asian_DLD/DLD_case_total_zip)*100]
DLD[,Black_pct_DLD:=(Black_DLD/DLD_case_total_zip)*100]
DLD[,White_pct_DLD:=(White_DLD/DLD_case_total_zip)*100]
DLD[,Hispanic_pct_DLD:=(Hispanic_DLD/DLD_case_total_zip)*100]


DLD_zip <- DLD[,lapply(.SD,max,na.rm=TRUE),
               by=(PAT_ZIP),.SDcols=c("DLD_case_total_zip","Asian_pct_DLD","Black_pct_DLD",
                                      "White_pct_DLD","Hispanic_pct_DLD","Asian_DLD","Black_DLD",
                                      "White_DLD","Hispanic_DLD")]
setnames(DLD_zip,"PAT_ZIP","ZCTA5CE20")
zctas_demog_DLD <- zctas_demog[DLD_zip, on="ZCTA5CE20"]
zctas_demog_DLD[,Asian_diff_expected:=Asian_pct-Black_pct_DLD]
zctas_demog_DLD[,Black_diff_expected:=Black_pct-Black_pct_DLD]
zctas_demog_DLD[,White_diff_expected:=White_pct-Black_pct_DLD]
zctas_demog_DLD[,Latin_diff_expected:=Latin_pct-Hispanic_pct_DLD]

#add z-codes 
z_codes <- read.csv(paste0(maindir,"/public_use_outpatient/OP_PUDF_base1_2022_Z_houMSA.csv"))
z_codes <- as.data.table(z_codes)
z_codes[,z_case_total_zip:=.N,by=(PAT_ZIP)]
z_codes[,z_cases_race:=.N,by=.(PAT_ZIP,RACE)]
z_codes[,Asian_z:=fifelse(RACE=="2",z_cases_race,0)]
z_codes[,Black_z:=fifelse(RACE=="3",z_cases_race,0)]
z_codes[,White_z:=fifelse(RACE=="4",z_cases_race,0)]
z_codes[,z_cases_eth:=.N,by=.(PAT_ZIP,ETHNICITY)]
z_codes[,Hispanic_z:=fifelse(ETHNICITY=="1",z_cases_eth,0)]
z_codes[,Asian_pct_z:=(Asian_z/z_case_total_zip)*100]
z_codes[,Black_pct_z:=(Black_z/z_case_total_zip)*100]
z_codes[,White_pct_z:=(White_z/z_case_total_zip)*100]
z_codes[,Hispanic_pct_z:=(Hispanic_z/z_case_total_zip)*100]



z_zip <- z_codes[,lapply(.SD,max,na.rm=TRUE),
               by=(PAT_ZIP),.SDcols=c("z_case_total_zip","Asian_pct_z","Black_pct_z",
                                      "White_pct_z","Hispanic_pct_z","Asian_z","Black_z",
                                      "White_z","Hispanic_z")]
setnames(z_zip,"PAT_ZIP","ZCTA5CE20")
zctas_demog_z <- zctas_demog[z_zip, on="ZCTA5CE20"]
zctas_demog_z[,z_code_pop:=z_case_total_zip/total_pop]

#did not do interactions normalization
interactions <- read_csv(paste0(maindir,"/public_use_outpatient/OP_PUDF_2022_interaction_zip_houMSA.csv"))

#add lm for each...

zctas_demog_z[,("centroid"):=NULL]
zctas_demog_z <- zctas_demog_z[!is.na(STATEFP)]
st_write(zctas_demog_z,"~/Downloads/TX_2022_z_codes_5_1_24.csv",driver = "CSV",factorsAsCharacter=FALSE,
         layer_options = "GEOMETRY=AS_WKT")
st_write(zctas_demog_z,"~/Downloads/TX_2022_z_codes_5_1_24.geojson",driver = "GeoJSON",factorsAsCharacter=FALSE)
write_rds(zctas_demog_z,paste0(censusdir,vintage,"/TX_2022_z_codes_5_1_24.RDS"))


#8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller 
FIPS_vector <- c("201","157","167","039","071","291","339","473")
tracts_demog8 <- tracts_demog[COUNTYFP%in%FIPS_vector]
write_rds(tracts_demog8,"~/Downloads/tracts_demog8.RDS")
Harris_tracts_demog <- tracts_demog[COUNTYFP=="201"]
write_rds(Harris_tracts_demog,"~/Downloads/Harris_tracts_demog.RDS")
#Tx_tract_demog8 <- tracts_demog8[,c("GEOID","total_pop","White_pct","Latin_pct","Black_pct","Asian_pct","10_14_pct","gini_index","median_income","geometry")]

#Tx_tract_demog <- tracts_demog[,c("GEOID","total_pop","White_pct","Latin_pct","Black_pct","Asian_pct","10_14_pct","gini_index","median_income","geometry")]
#Tx_tract_demog_DT <- as.data.table(Tx_tract_demog)
#Tx_tracts <- st_as_sf(Tx_tract_demog_DT,sf_column_name = "geometry")
st_write(Tx_tract_demog8,"~/Downloads/Tx_tracts8.geojson",driver = "GeoJSON")
st_write(censusunsd,"~/Downloads/Tx_school_districts.geojson",driver = "GeoJSON")
write_rds(Tx_tract_demog8,"~/Downloads/Tx_tracts8.RDS")



#for the vaccine stuff 

Harris_tracts_vax_6_22_2021 <- read.csv(paste0(houstondatadir,"2021/Harris_vax_tract 2021-06-22.csv"))
Harris_tracts_vax_6_22_2021 <- as.data.table(Harris_tracts_vax_6_22_2021)
Harris_tracts_vax_6_22_2021[,("total_covid_vax_6_22_2021"):=sum(Number.of.People.Vaccinated),by=GEOID]
Harris_tracts_vax_6_22_2021[,("GEOID"):=as.character(GEOID)]
Harris_tracts_6_22_21_clean <- unique(Harris_tracts_vax_6_22_2021,by="GEOID")[,c(1,8)]
tracts_demog[,("total_covid_vax_6_22_2021"):=Harris_tracts_6_22_21_clean[.SD,total_covid_vax_6_22_2021,on=.(GEOID)]]
tracts_demog[,("percent_covid_vax_6_22_2021"):=as.numeric(total_covid_vax_6_22_2021)/as.numeric(total_pop)]
tracts_demog[,("percent_covid_vax_6_22_2021"):=as.integer(percent_covid_vax_6_22_2021*100)]

Harris_tracts_vax_7_20_2021 <- read.csv(paste0(houstondatadir,"2021/Harris_vax_tract 2021-07-20.csv"))
Harris_tracts_vax_7_20_2021 <- as.data.table(Harris_tracts_vax_7_20_2021)
Harris_tracts_vax_7_20_2021[,("total_covid_vax_7_20_21"):=sum(Number.of.People.Vaccinated),by=GEOID]
Harris_tracts_vax_7_20_2021[,("GEOID"):=as.character(GEOID)]
Harris_tracts_clean_7_20_2021 <- unique(Harris_tracts_vax_7_20_2021,by="GEOID")[,c(1,8)]
tracts_demog[,("total_covid_vax_7_20_2021"):=Harris_tracts_clean_7_20_2021[.SD,total_covid_vax_7_20_21,on=.(GEOID)]]
tracts_demog[,("percent_covid_vax_7_20_2021"):=as.numeric(total_covid_vax_7_20_2021)/as.numeric(total_pop)]
tracts_demog[,("percent_covid_vax_7_20_2021"):=as.integer(percent_covid_vax_7_20_2021*100)]
tracts_demog[,("percent_change_vax_6to7_2021"):=percent_covid_vax_7_20_2021-percent_covid_vax_6_22_2021]

Harris_tracts_vax_9_2022 <- read.csv(paste0(houstondatadir,"2022/vax_by_tract_9_2022.csv"))
Harris_tracts_vax_9_2022 <- as.data.table(Harris_tracts_vax_9_2022)
Harris_tracts_vax_9_2022[,("total_covid_vax_9_22"):=sum(Number.of.People.Vaccinated),by=GEOID]
Harris_tracts_vax_9_2022[,("GEOID"):=as.character(GEOID)]
Harris_tracts_clean <- unique(Harris_tracts_vax_9_2022,by="GEOID")[,c(1,8)]
tracts_demog[,("total_covid_vax_9_22"):=Harris_tracts_clean[.SD,total_covid_vax_9_22,on=.(GEOID)]]
tracts_demog[,("percent_covid_vax_9_22"):=as.numeric(total_covid_vax_9_22)/as.numeric(total_pop)]
tracts_demog[,("percent_covid_vax_9_22"):=as.integer(percent_covid_vax_9_22*100)]
tracts_demog[,("percent_change_vax_7_2021to9_2022"):=percent_covid_vax_9_22-percent_covid_vax_7_20_2021]

#City of Houston Vaccine info
CoH_zip_vaccines <- as.data.table(readxl::read_xlsx(path = paste0(houstondatadir,"2022/CoHvaccines_09072022.xlsx"),sheet = "ZipWeekly"))


#CoH_zip_vaccines[is.na(`Fully Vaccinated_104`),("Fully Vaccinated_104"):=0]
#make it do a runnning total and then time step...?
CoH_priority_zips <- as.data.table(readxl::read_xlsx(path = paste0(houstondatadir,"2022/CoHvaccines_09072022.xlsx"),sheet = "ZipCodeVaccineTier"))
Houston_vaccines_zip <- CoH_priority_zips[CoH_zip_vaccines,on=.(`ZIP Code`)]
Houston_vaccines_zip$zip <- as.character(Houston_vaccines_zip$`ZIP Code`)
Houston_vaccines_zip$Week <- as.POSIXct(Houston_vaccines_zip$`Start of Week`)


setDT(Houston_vaccines_zip)[,("sum_full_vax"):=
                              sum(`Fully Vaccinated_104`),
                            by=.(zip)]
Houston_vaccines_uniq <- unique(Houston_vaccines_zip,by="zip")[,c(6,9)]
tracts_demog[,("zip_full_covid_vax"):=Houston_vaccines_uniq[.SD,sum_full_vax,on=.(zip)]]


Houston_zips <- zctasDT[ZCTA5CE20 %in% unique(Houston_vaccines_zip[,ZIP])]
Houston_zips$ZIP <- Houston_zips$ZCTA5CE20
Houston_vax_geo <- Houston_zips[Houston_vaccines_zip,on=.(ZIP)]
#for full map, want to do it by percentage of eligible pop
Houston_vax_geo_9_18 <- st_as_sf(Houston_vax_geo[!is.na(ZIP),
                                                 c("Week","ZIP","cum_sum_full_vax",
                "Vaccine Tier","geometry")],sf_column_name = "geometry")
st_write(Houston_vax_geo_9_18,"~/Downloads/HoustonVax_9_18.geojson",driver="GeoJSON")


HarrisTracts <- tractsDT[COUNTYFP=="201"]  #[str_starts(GEOID,"48201")]
#or
HarrisTracts <- Harris_tracts_demog
#HarrisTracts[,("centroids"):=st_centroid(geometry)]
#vaccines <- as.data.table(read.csv2(file="~/Downloads/Percentage\ of\ Population\ Vaccinated\ by\ Census\ tract\ 2021-06-22.csv",sep = ","))
vaccines <- as.data.table(read.csv2(file="~/Downloads/Percentage\ of\ Population\ Vaccinated\ by\ Census\ tract\ 2021-07-20.csv",sep = ","))
vaccines$GEOID <- as.character(vaccines$GEOID)
vaccines_male <- vaccines[Gender=="Male"]
setnames(vaccines_male,"X..Population.Vaccinated","men_vaccinated_pct")
vaccines_female <- vaccines[Gender=="Female"]
setnames(vaccines_female,"X..Population.Vaccinated","women_vaccinated_pct")
HarrisTracts <- vaccines_male[HarrisTracts, on="GEOID"]
HarrisTracts <- vaccines_female[HarrisTracts, on="GEOID"]
#HarrisTracts <- HarrisTracts[vaccines_male, on="GEOID"]
#HarrisTracts <- HarrisTracts[vaccines_female, on="GEOID"]
#HarrisTracts <- HarrisTracts[!is.na(STATEFP)]
HarrisTracts[,("men_vaxed"):=as.integer(`men_vaccinated_pct`)]
HarrisTracts[,("men_vaxed"):=if_else(men_vaxed>100,100,as.numeric(men_vaxed))]
HarrisTracts[,("women_vaxed"):=as.integer(`women_vaccinated_pct`)]
HarrisTracts[,("women_vaxed"):=if_else(women_vaxed>100,100,as.numeric(women_vaxed))]

vaccines_8_31 <- as.data.table(read.csv2(file="~/Downloads/Immtrac_Vaccine_by_Census_tract_2021-08-31.csv",sep = ","))
vaccines_8_31$GEOID <- as.character(vaccines_8_31$GEOID)
setnames(vaccines_8_31,"X..Population.Vaccinated","vaccinated_pct")
HarrisTracts <- vaccines_8_31[HarrisTracts, on="GEOID"]
HarrisTracts[,("8_31_vaxed"):=as.integer(`vaccinated_pct`)]
HarrisTracts[,("8_31_vaxed"):=if_else(`8_31_vaxed`>100,100,as.numeric(`8_31_vaxed`))]
vaccines_female <- as.data.table(vaccines_female)
vaccines_female[,("women_pop"):=as.integer(as.numeric(Number.of.People.Vaccinated)/(as.numeric(women_vaccinated_pct)/100))]
vaccines_female[,("women_raw"):=as.numeric(Number.of.People.Vaccinated)]
vaccines_male[,("men_pop"):=as.integer(as.numeric(Number.of.People.Vaccinated)/(as.numeric(men_vaccinated_pct)/100))]
vaccines_male[,("men_raw"):=as.numeric(Number.of.People.Vaccinated)]
HarrisTracts <- vaccines_male[HarrisTracts, on="GEOID"]
HarrisTracts <- vaccines_female[HarrisTracts, on="GEOID"]
HarrisTracts[,("July_pct"):=100*(men_raw+women_raw)/(men_pop+women_pop)] #doesn't work - too many places with imbalanced gender?
HarrisTracts[,("new_vax_Aug"):=July_pct-as.numeric(vaccinated_pct)]

#do some quick plots
library(ggplot2)
ggplot(HarrisTracts[,c("Black_pct","women_vaxed")],aes(`Black_pct`,`women_vaxed`)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(x='% men vaccinated', y='Black %', title='Linear Regression Plot')# +
  #theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 
  

HarrisVax_8_31 <- st_as_sf(HarrisTracts[!is.na(STATEFP),
                                      c("8_31_vaxed",
                                        "geometry")],sf_column_name = "geometry")
st_write(HarrisVax_8_31,"~/Downloads/HarrisVax_8_31.geojson",driver="GeoJSON")


HarrisVaxDem <- st_as_sf(HarrisTracts[!is.na(STATEFP),
                                   c("men_vaxed","women_vaxed","White_pct","Black_pct",
                                      "Latin_pct","Asian_pct","median_income_total",
                                     "renter_pct","income_10k_pct","income_under_30k",
                                     "under_18_pct","pub_transport_hh_pct",
                                     "geometry")],sf_column_name = "geometry")
st_write(HarrisVaxDem,"~/Downloads/HarrisVaxDem.geojson",driver="GeoJSON")

HarrisVaxIncome <- st_as_sf(HarrisTracts[!is.na(STATEFP),
                                      c("men_vaxed","women_vaxed","median_income_total",
                                        "income_10k_pct","income_under_30k",
                                        "geometry")],sf_column_name = "geometry")
st_write(HarrisVaxIncome,"~/Downloads/HarrisVaxIncome.geojson",driver="GeoJSON")

HarrisVaxRTA <- st_as_sf(HarrisTracts[!is.na(STATEFP),
                                      c("men_vaxed","women_vaxed",
                                        "renter_pct",
                                        "under_18_pct","pub_transport_hh_pct",
                                        "geometry")],sf_column_name = "geometry")
st_write(HarrisVaxRTA,"~/Downloads/HarrisVaxRTA.geojson",driver="GeoJSON")


zipcodes <- st_read(paste0(censusdir,"/2018/cb_2018_us_zcta510_500k")) #or whatever you called it
zipcodes <- st_transform(zipcodes, st_crs(HarrisVax))

zipDT <- as.data.table(zipcodes)
BOLzips <- as.data.table(read.csv2(file = "~/Downloads/BOLZipCodes.csv",sep = ",")) #deleted the raw sheet and kept only the counts
zipDT$Zip.Code <- as.character(zipDT$ZCTA5CE10)
zipmerge <- zipDT[BOLzips, on="Zip.Code"]
zipHarris <- zipmerge[!is.na(Count.of.Zip.Code)]
zipmerge_clean <- zipHarris[!is.na(ZCTA5CE10)]
setnames(zipmerge_clean,"ZCTA5CE10","ZIPCODE")
setnames(zipmerge_clean,"Count.of.Zip.Code","Number")
zip_BOL <- st_as_sf(zipmerge_clean[,c("ZIPCODE","Number","geometry")], sf_column_name = "geometry")
st_write(zip_BOL,"~/Downloads/zip_BOL.geojson",driver = "GeoJSON")

#HarrisV <- as.data.frame(HarrisTracts[,c("GEOID","men_vaccinated_pct","women_vaccinated_pct","geometry")])
#HarrisVnoG <- as.data.frame(HarrisTracts[,c("GEOID","men_vaccinated_pct","women_vaccinated_pct")])
#sfHarris <- st_as_sf(HarrisV)

#sfHarris$geometry <- st_cast(sfHarris$geometry,"MULTIPOLYGON")

#library(geojsonsf)
#gHarris <- sf_geojson(sfHarris)

#write.csv(HarrisVax,file = "~/Downloads/HarrisVax.csv",row.names = TRUE)



#geojson_write(HarrisVax,geometry = "polygon", group = "group", file = "~/Downloads/harris_vax.geojson")

#for totals for catchment area U54
catch_total_pop <- sum(tracts_demog[,total_pop])
catch_total_pop <- sum(as.numeric(tracts_demog[,total_pop]))
catch_total_pop_latin <- sum(as.numeric(tracts_demog[,Latin_pop]))
catch_total_pop_Black <- sum(as.numeric(tracts_demog[,Black_pop]))
catch_total_pop_Asian <- sum(as.numeric(tracts_demog[,Asian_pop]))
catch_total_households <- sum(as.numeric(tracts_demog[,households]))
catch_total_renters <- sum(as.numeric(tracts_demog[,renters]))
catch_total_median_income <- sum(as.numeric(tracts_demog[,median_income_total])*as.numeric(tracts_demog[,total_pop]))/catch_total_pop




