library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
#censusdir from workflow / census_key from expand_from scripts / source CensusData.R

#this depends on where your census dir is, but _tract_500k.shp and _faces and _bg, etc. are all downloaded from census, by year: 
#https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html (I got these on 7/10/21 - 2020 was most recent)
geo_vintage <- "2021"
censustracts <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_tract_500k/cb_", geo_vintage, "_", state, "_tract_500k.shp"))
tractsDT <- as.data.table(censustracts)
#tractsDT[,("centroids"):=st_centroid(geometry)] #get a warning, because it's not flat space, but should be close enough for labels

#8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller 
FIPS_vector <- c("201","157","167","039","071","291","339","473")
tracts_8county <- tractsDT[COUNTYFP%in%FIPS_vector]

censusblocks <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_bg_500k/cb_", geo_vintage, "_", state, "_bg_500k.shp"))
blocksDT <- as.data.table(censusblocks)
blocks_8county <- blocksDT[COUNTYFP%in%FIPS_vector]

censusplace <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_place_500k/cb_", geo_vintage, "_", state, "_place_500k.shp"))
placeDT <- as.data.table(censusplace) #just cities

#metro stat areas
census_cbsa <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_us_cbsa_500k/cb_", geo_vintage, "_us_cbsa_500k.shp"))
us_cbsaDT <- as.data.table(census_cbsa)

#TX state representatives
censussldl <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_sldl_500k/cb_", geo_vintage, "_", state, "_sldl_500k.shp"))
sldlDT <- as.data.table(censussldl) 

#for some reason, 2019 is most recent - state legislature boundaries - TX state senatorial districts
censussldu <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_2019", "_", state, "_sldu_500k/cb_2019", "_", state, "_sldu_500k.shp"))
slduDT <- as.data.table(censussldu) 

#elementary school districts - only one in Texas - could merge, but not sure it's worth it.
censuselsd <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_elsd_500k/cb_", geo_vintage, "_", state, "_elsd_500k.shp"))
elsdDT <- as.data.table(censuselsd) 

#secondary school districts - only one in Texas - Vysehrad 
censusscsd <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_scsd_500k/cb_", geo_vintage, "_", state, "_scsd_500k.shp"))
scsdDT <- as.data.table(censusscsd) 

#unified school districts - 1019 districts
censusunsd <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_unsd_500k/cb_", geo_vintage, "_", state, "_unsd_500k.shp"))
unsdDT <- as.data.table(censusunsd)

#2020 voting districts
censusvtd <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_vtd_500k/cb_", geo_vintage, "_", state, "_vtd_500k.shp"))
vtdDT <- as.data.table(censusvtd)

#zip codes, or zctas, most recent was 2018, as of 7/102021
#https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html
#it's the national file
zip_vintage <- "2018"
censuszctas <- st_read(paste0(censusdir, zip_vintage, "/geo_census/cb_", zip_vintage, "_us_zcta510_500k/cb_", zip_vintage, "_us_zcta510_500k.shp"))
zctasDT <- as.data.table(censuszctas)
tx_boundary <- st_union(tractsDT$geometry)
tx_zctas <- zctasDT[st_within(geometry,tx_boundary) %>% lengths > 0,]

#congressional districts
censuscd116 <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_us", "_cd116_500k/cb_", geo_vintage, "_us", "_cd116_500k.shp"))
cd116DT <- as.data.table(censuscd116)
tx_cd116 <- cd116DT[st_within(geometry,tx_boundary) %>% lengths > 0,]

#for each county, can have sex_age by blck group - and thus pop by block group, too
blck_sex_by_age_race_data_from_census_tx <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                       groupname = "B01001",county_num = county,
                       block="block",api_type="acs/acs5",path_suff="est.csv")
#have to think through how to use - could use the census tract levels

#options(digits = 2, scipen = 999) #for display on all of them

sex_by_age_race <- blck_sex_by_age_race_data_from_census_tx %>%
  mutate(label = str_remove_all(label,"Estimate!!Total:!!"),
         race = substr(name,7,7)) %>%
  pivot_longer(4:ncol(blck_sex_by_age_race_data_from_census_tx),names_to = "GEOID", values_to = "number_sams")%>%
  separate(label, c("sex","age_range"), sep = ":!!", remove = F, convert = FALSE) 
sex_age_race_DT <- as.data.table(sex_by_age_race,key="GEOID")
pop_totals <- sex_age_race_DT[is.na(age_range)&race=="_"&sex=="Estimate!!Total:"]
tracts_demog <- tractsDT[pop_totals[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","total_pop")
pop_hispanic <- sex_age_race_DT[is.na(age_range)&race=="I"&sex=="Estimate!!Total:"]
tracts_demog <- tracts_demog[pop_hispanic[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","Latin_pop")
tracts_demog[,("Latin_pct"):=as.integer((as.numeric(Latin_pop)*100)/as.numeric(total_pop))]
pop_AA <- sex_age_race_DT[is.na(age_range)&race=="B"&sex=="Estimate!!Total:"]
tracts_demog <- tracts_demog[pop_AA[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","Black_pop")
tracts_demog[,("Black_pct"):=as.integer((as.numeric(Black_pop)*100)/as.numeric(total_pop))]
pop_White <- sex_age_race_DT[is.na(age_range)&race=="H"&sex=="Estimate!!Total:"]
tracts_demog <- tracts_demog[pop_White[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","White_pop")
tracts_demog[,("White_pct"):=as.integer((as.numeric(White_pop)*100)/as.numeric(total_pop))]
pop_asian <- sex_age_race_DT[is.na(age_range)&race=="D"&sex=="Estimate!!Total:"]
tracts_demog <- tracts_demog[pop_asian[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","Asian_pop")
tracts_demog[,("Asian_pct"):=as.integer((as.numeric(Asian_pop)*100)/as.numeric(total_pop))]
pop_boys_10_14 <- sex_age_race_DT[age_range=="10 to 14 years"&race=="_"&sex=="Male"]
tracts_demog <- tracts_demog[pop_boys_10_14[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","boys_10_14_pop")
pop_girls_10_14 <- sex_age_race_DT[age_range=="10 to 14 years"&race=="_"&sex=="Female"]
tracts_demog <- tracts_demog[pop_girls_10_14[,7:8],on="GEOID"]
setnames(tracts_demog,"number_sams","girls_10_14_pop")
tracts_demog[,("pop_10_14"):=as.numeric(girls_10_14_pop)+as.numeric(boys_10_14_pop)]
tracts_demog[,("10_14_pct"):=as.integer(as.numeric(pop_10_14*100)/as.numeric(total_pop))]

#have to have sourced Census_Data.R and get censuskey from expand_from_census
gini_from_census <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                       groupname = "B19083",county_num = county,
                       block="block",api_type="acs/acs5",path_suff="est.csv")
gini_data <- gini_from_census %>%
  pivot_longer(4:ncol(gini_from_census),names_to = "GEOID", values_to = "gini_index")
gini_DT <- as.data.table(gini_data)
gini_DT$gini_index <- format(gini_DT$gini_index,scientific=FALSE)
tracts_demog <- tracts_demog[gini_DT[,4:5],on="GEOID"]
tracts_demog[,("gini_index"):=if_else(as.numeric(gini_index)<0,0,as.numeric(gini_index))]

place_born_med_income <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                       groupname = "B06011",county_num = county,
                       block="block",api_type="acs/acs5",path_suff="est.csv")
med_income <- place_born_med_income %>%
  pivot_longer(4:ncol(place_born_med_income),names_to = "GEOID", values_to = "median_income")
median_income <- as.data.table(med_income)
med_income_total <- median_income[name=="B06011_001E"]
tracts_demog <- tracts_demog[med_income_total[,4:5],on="GEOID"]
setnames(tracts_demog,"median_income","median_income_total")
med_income_fb <- median_income[name=="B06011_005E"]
tracts_demog <- tracts_demog[med_income_fb[,4:5],on="GEOID"]
setnames(tracts_demog,"median_income","median_income_fb")
tracts_demog[,("median_income_total"):=if_else(median_income_total=="-666666666","0",median_income_total)]
tracts_demog[,("median_income_fb"):=if_else(median_income_fb=="-666666666","0",median_income_fb)]

own_rent_hh_type_census <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                       groupname = "B25011",county_num = county,
                       block="block",api_type="acs/acs5",path_suff="est.csv")
own_rentDT <- as.data.table(own_rent_hh_type_census)
own_rent_hh <- own_rentDT %>%
  pivot_longer(4:ncol(own_rentDT),names_to = "GEOID", values_to = "households")
own_rent_hh <- as.data.table(own_rent_hh)
total_hh_occupied <- own_rent_hh[name=="B25011_001E",4:5]
tracts_demog <- tracts_demog[total_hh_occupied, on="GEOID"]
renters <- own_rent_hh[name=="B25011_026E",4:5]
setnames(renters,"households","renters")
tracts_demog <- tracts_demog[renters, on="GEOID"]
tracts_demog[,("renter_pct"):=as.integer((as.numeric(renters*100))/as.numeric(households))]
tracts_demog[,("owner_pct"):=as.integer(((as.numeric(households)*100)-as.numeric(renters)*100)/as.numeric(households))]
married_couple_owners <- own_rent_hh[name=="B25011_004E",4:5]
setnames(married_couple_owners,"households","married_owners")
tracts_demog <- tracts_demog[married_couple_owners, on="GEOID"]
tracts_demog[,("married_owners_pct"):=as.integer((as.numeric(married_owners)*100)/as.numeric(households))]
owner_living_alone <- own_rent_hh[name=="B25011_018E",4:5]
setnames(owner_living_alone,"households","owner_living_alone")
tracts_demog <- tracts_demog[owner_living_alone, on="GEOID"]
tracts_demog[,("owner_living_alone_pct"):=as.integer((as.numeric(owner_living_alone)*100)/as.numeric(households))]
renter_living_alone <- own_rent_hh[name=="B25011_042E",4:5]
setnames(renter_living_alone,"households","renter_living_alone")
tracts_demog <- tracts_demog[renter_living_alone, on="GEOID"]
tracts_demog[,("renter_living_alone_pct"):=as.integer((as.numeric(renter_living_alone)*100)/as.numeric(households))]
renter_non_family_shared <- own_rent_hh[name=="B25011_046E",4:5]
setnames(renter_non_family_shared,"households","renter_non_family_shared")
tracts_demog <- tracts_demog[renter_non_family_shared, on="GEOID"]
tracts_demog[,("renter_non_family_shared_pct"):=as.integer((as.numeric(renter_non_family_shared)*100)/as.numeric(households))]

#income B19001
income_from_census <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                                             groupname = "B19001",county_num = county,
                                             block="block",api_type="acs/acs5",path_suff="est.csv")
incomeDT <- as.data.table(income_from_census)
income <- incomeDT %>%
  pivot_longer(4:ncol(incomeDT),names_to = "GEOID", values_to = "income")
income <- as.data.table(income)
income_10k <- income[name=="B19001_002E",4:5]
setnames(income_10k,"income","income_10k")
tracts_demog <- tracts_demog[income_10k, on="GEOID"]
tracts_demog[,("income_10k_pct"):=as.integer((as.numeric(income_10k)*100)/as.numeric(households))]
income_10_15k <- income[name=="B19001_003E",4:5]
setnames(income_10_15k,"income","income_10_15k")
tracts_demog <- tracts_demog[income_10_15k, on="GEOID"]
tracts_demog[,("income_10_15k_pct"):=as.integer((as.numeric(income_10_15k)*100)/as.numeric(households))]
income_15_20k <- income[name=="B19001_004E",4:5]
setnames(income_15_20k,"income","income_15_20k")
tracts_demog <- tracts_demog[income_15_20k, on="GEOID"]
tracts_demog[,("income_15_20k_pct"):=as.integer((as.numeric(income_15_20k)*100)/as.numeric(households))]
income_20_25k <- income[name=="B19001_005E",4:5]
setnames(income_20_25k,"income","income_20_25k")
tracts_demog <- tracts_demog[income_20_25k, on="GEOID"]
tracts_demog[,("income_20_25k_pct"):=as.integer((as.numeric(income_20_25k)*100)/as.numeric(households))]
income_25_30k <- income[name=="B19001_006E",4:5]
setnames(income_25_30k,"income","income_25_30k")
tracts_demog <- tracts_demog[income_25_30k, on="GEOID"]
tracts_demog[,("income_25_30k_pct"):=as.integer((as.numeric(income_25_30k)*100)/as.numeric(households))]
income_30_35k <- income[name=="B19001_007E",4:5]
setnames(income_30_35k,"income","income_30_35k")
tracts_demog <- tracts_demog[income_30_35k, on="GEOID"]
tracts_demog[,("income_30_35k_pct"):=as.integer((as.numeric(income_30_35k)*100)/as.numeric(households))]
income_35_40k <- income[name=="B19001_008E",4:5]
setnames(income_35_40k,"income","income_35_40k")
tracts_demog <- tracts_demog[income_35_40k, on="GEOID"]
tracts_demog[,("income_35_40k_pct"):=as.integer((as.numeric(income_35_40k)*100)/as.numeric(households))]
income_40_45k <- income[name=="B19001_009E",4:5]
setnames(income_40_45k,"income","income_40_45k")
tracts_demog <- tracts_demog[income_40_45k, on="GEOID"]
tracts_demog[,("income_40_45k_pct"):=as.integer((as.numeric(income_40_45k)*100)/as.numeric(households))]
income_45_50k <- income[name=="B19001_010E",4:5]
setnames(income_45_50k,"income","income_45_50k")
tracts_demog <- tracts_demog[income_45_50k, on="GEOID"]
tracts_demog[,("income_45_50k_pct"):=as.integer((as.numeric(income_45_50k)*100)/as.numeric(households))]
income_50_60k <- income[name=="B19001_011E",4:5]
setnames(income_50_60k,"income","income_50_60k")
tracts_demog <- tracts_demog[income_50_60k, on="GEOID"]
tracts_demog[,("income_50_60k_pct"):=as.integer((as.numeric(income_50_60k)*100)/as.numeric(households))]
income_60_75k <- income[name=="B19001_012E",4:5]
setnames(income_60_75k,"income","income_60_75k")
tracts_demog <- tracts_demog[income_60_75k, on="GEOID"]
tracts_demog[,("income_60_75k_pct"):=as.integer((as.numeric(income_60_75k)*100)/as.numeric(households))]
income_75_100k <- income[name=="B19001_013E",4:5]
setnames(income_75_100k,"income","income_75_100k")
tracts_demog <- tracts_demog[income_75_100k, on="GEOID"]
tracts_demog[,("income_75_100k_pct"):=as.integer((as.numeric(income_75_100k)*100)/as.numeric(households))]
income_100_125k <- income[name=="B19001_014E",4:5]
setnames(income_100_125k,"income","income_100_125k")
tracts_demog <- tracts_demog[income_100_125k, on="GEOID"]
tracts_demog[,("income_100_125k_pct"):=as.integer((as.numeric(income_100_125k)*100)/as.numeric(households))]
income_125_150k <- income[name=="B19001_015E",4:5]
setnames(income_125_150k,"income","income_125_150k")
tracts_demog <- tracts_demog[income_125_150k, on="GEOID"]
tracts_demog[,("income_125_150k_pct"):=as.integer((as.numeric(income_125_150k)*100)/as.numeric(households))]
income_150_200k <- income[name=="B19001_016E",4:5]
setnames(income_150_200k,"income","income_150_200k")
tracts_demog <- tracts_demog[income_150_200k, on="GEOID"]
tracts_demog[,("income_150_200k_pct"):=as.integer((as.numeric(income_150_200k)*100)/as.numeric(households))]
income_over_200k <- income[name=="B19001_017E",4:5]
setnames(income_over_200k,"income","income_over_200k")
tracts_demog <- tracts_demog[income_over_200k, on="GEOID"]
tracts_demog[,("income_over_200k_pct"):=as.integer((as.numeric(income_over_200k)*100)/as.numeric(households))]
tracts_demog[,("income_under_30k"):=as.integer(as.numeric(income_10k)+
                                                as.numeric(income_10_15k)+
                                                 as.numeric(income_15_20k)+as.numeric(income_20_25k)+
                                                 as.numeric(income_25_30k)*100/as.numeric(households))] #income_10k,income_10_15k,income_15_20k,income_20_25k,income_25_30k

#housing/renter
vacant_houses_census <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                                               groupname = "B25002",county_num = county,
                                               block="block",api_type="acs/acs5",path_suff="est.csv")
vacant_housesDT <- as.data.table(vacant_houses_census)
vacant_houses_total <- vacant_housesDT[name=="B25002_001E"]
vacant_houses_total <- vacant_houses_total %>%
  pivot_longer(4:ncol(vacant_houses_total),names_to = "GEOID", values_to = "vacant_houses_total")
vacant_totalDT <- as.data.table(vacant_houses_total)
vacant <- vacant_housesDT[name=="B25002_003E"]
vacant <- vacant %>%
  pivot_longer(4:ncol(vacant),names_to = "GEOID", values_to = "vacant")
vacantDT <- as.data.table(vacant)
vacancies <- vacant_totalDT[vacantDT[,4:5],on="GEOID"]
vacancies[,("vacant_housing_pct"):=as.integer((as.numeric(vacant)*100)/as.numeric(vacant_houses_total))]
tracts_demog <- tracts_demog[vacancies[,4:7],on="GEOID"]

#place_born_census <- est_StateCensusData_byGroupName(censusdir, vintage, state, censuskey, groupname = "B06004") #by race, have to total

#citizenship and nativity B05003
citizenship_pb <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                                         groupname = "B05003",county_num = county,
                                         block="block",api_type="acs/acs5",path_suff="est.csv")
#also has under 18 male / female
cit_pb_under_18 <- citizenship_pb %>%
  pivot_longer(4:ncol(citizenship_pb),names_to = "GEOID", values_to = "num")
cit_pb_under_18 <- as.data.table(cit_pb_under_18)
male_under_18 <- cit_pb_under_18[name=="B05003_003E",4:5]
fem_under_18 <- cit_pb_under_18[name=="B05003_014E",4:5]
under_18s <- male_under_18[fem_under_18,on="GEOID"]
under_18s[,("under_18"):=as.numeric(num)+as.numeric(i.num)]
under_18s <- under_18s[,c("GEOID","under_18")]
tracts_demog <- tracts_demog[under_18s,on="GEOID"]
tracts_demog[,("under_18_pct"):=as.integer((as.numeric(under_18)*100)/as.numeric(total_pop))]
under_18_fbM <- cit_pb_under_18[name=="B05003_005E",4:5]
under_18_fbF <- cit_pb_under_18[name=="B05003_016E",4:5]
under_18_fb <- under_18_fbM[under_18_fbF,on="GEOID"]
under_18_fb[,("under_18_fb"):=as.numeric(num)+as.numeric(i.num)]
under_18_fbs <- under_18_fb[,c("GEOID","under_18_fb")]
tracts_demog <- tracts_demog[under_18_fbs,on="GEOID"]
tracts_demog[,("under_18_fb_pct"):=as.integer((as.numeric(under_18_fb)*100)/as.numeric(under_18))]
over_17_fbM <- cit_pb_under_18[name=="B05003_010E",4:5]
over_17_fbF <- cit_pb_under_18[name=="B05003_021E",4:5]
over_17_fb <- over_17_fbM[over_17_fbF,on="GEOID"]
over_17_fb[,("over_17_fb"):=as.numeric(num)+as.numeric(i.num)]
over_17_fbs <- over_17_fb[,c("GEOID","over_17_fb")]
tracts_demog <- tracts_demog[over_17_fbs,on="GEOID"]
tracts_demog[,("over_17"):=as.numeric(total_pop)-as.numeric(under_18)]
tracts_demog[,("over_17_fb_pct"):=as.integer((as.numeric(over_17_fb)*100)/as.numeric(over_17))]
tracts_demog[,("fb_pct"):=as.integer((as.numeric(over_17_fb)+as.numeric(under_18_fb))*100/as.numeric(total_pop))]


#people per room B25014
pp_room_census <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                         groupname = "B25014",county_num = county,
                         block="block",api_type="acs/acs5",path_suff="est.csv")
pp_roomDT <- as.data.table(pp_room_census)
pp_room <- pp_roomDT %>%
  pivot_longer(4:ncol(pp_roomDT),names_to = "GEOID", values_to = "households")
pp_room <- as.data.table(pp_room)
pp_room3 <- pp_room[name=="B25014_003E",4:5]
setnames(pp_room3,"households","owner_.5")
tracts_demog <- tracts_demog[pp_room3, on="GEOID"]
tracts_demog[,("owner_.5_pct"):=as.integer((as.numeric(owner_.5)*100)/as.numeric(households))]
pp_room4 <- pp_room[name=="B25014_004E",4:5]
setnames(pp_room4,"households","owner_.5_1")
tracts_demog <- tracts_demog[pp_room4, on="GEOID"]
tracts_demog[,("owner_.5_1_pct"):=as.integer((as.numeric(owner_.5_1)*100)/as.numeric(households))]
pp_room5 <- pp_room[name=="B25014_005E",4:5]
setnames(pp_room5,"households","owner_1_1.5")
tracts_demog <- tracts_demog[pp_room5, on="GEOID"]
tracts_demog[,("owner_1_1.5_pct"):=as.integer((as.numeric(owner_1_1.5)*100)/as.numeric(households))]
pp_room6 <- pp_room[name=="B25014_006E",4:5]
setnames(pp_room6,"households","owner_1.5_2")
tracts_demog <- tracts_demog[pp_room6, on="GEOID"]
tracts_demog[,("owner_1.5_2_pct"):=as.integer((as.numeric(owner_1.5_2)*100)/as.numeric(households))]
pp_room7 <- pp_room[name=="B25014_007E",4:5]
setnames(pp_room7,"households","owner_over_2")
tracts_demog <- tracts_demog[pp_room7, on="GEOID"]
tracts_demog[,("owner_over_2_pct"):=as.integer((as.numeric(owner_over_2)*100)/as.numeric(households))]
pp_room9 <- pp_room[name=="B25014_009E",4:5]
setnames(pp_room9,"households","renter_.5")
tracts_demog <- tracts_demog[pp_room9, on="GEOID"]
tracts_demog[,("renter_.5pct"):=as.integer((as.numeric(renter_.5)*100)/as.numeric(households))]
pp_room10 <- pp_room[name=="B25014_010E",4:5]
setnames(pp_room10,"households","renter_.5_1")
tracts_demog <- tracts_demog[pp_room10, on="GEOID"]
tracts_demog[,("renter_.5_1_pct"):=as.integer((as.numeric(renter_.5_1)*100)/as.numeric(households))]
pp_room11 <- pp_room[name=="B25014_011E",4:5]
setnames(pp_room11,"households","renter_1_1.5")
tracts_demog <- tracts_demog[pp_room11, on="GEOID"]
tracts_demog[,("renter_1_1.5_pct"):=as.integer((as.numeric(renter_1_1.5)*100)/as.numeric(households))]
pp_room12 <- pp_room[name=="B25014_012E",4:5]
setnames(pp_room12,"households","renter_1.5_2")
tracts_demog <- tracts_demog[pp_room12, on="GEOID"]
tracts_demog[,("renter_1.5_2_pct"):=as.integer((as.numeric(renter_1.5_2)*100)/as.numeric(households))]
pp_room13 <- pp_room[name=="B25014_013E",4:5]
setnames(pp_room13,"households","renter_over_2")
tracts_demog <- tracts_demog[pp_room13, on="GEOID"]
tracts_demog[,("renter_over_2_pct"):=as.integer((as.numeric(renter_over_2)*100)/as.numeric(households))]
#tracts_demog[,("over_1.5_pproom_pct"):=renter_over_2_pct+owner_over_2_pct+renter_1.5_2_pct+owner_1.5_2_pct]

#transport by tenure B08137
transport_tenure <- censusData_byGroupName(censusdir, vintage, state, censuskey, 
                                           groupname = "B08137",county_num = county,
                                           block="block",api_type="acs/acs5",path_suff="est.csv")
pub_transportDT <- as.data.table(transport_tenure)
pub_transport <- pub_transportDT %>%
  pivot_longer(4:ncol(pub_transportDT),names_to = "GEOID", values_to = "pub_transport_hh")
pub_transport <- as.data.table(pub_transport)
pub_transport <- pub_transport[name=="B08137_010E",4:5]
tracts_demog <- tracts_demog[pub_transport, on="GEOID"]
tracts_demog[,("pub_transport_hh_pct"):=as.integer((as.numeric(pub_transport_hh)*100)/as.numeric(households))]
tracts_demog <- tracts_demog[!is.na(STATEFP)] #all GEOIDs ending in 90000 - something weird, but not sure what - only 12 in Tx.

st_write(tracts_demog,"~/Downloads/tracts_demog_polygons.geojson",driver = "GeoJSON")
write_rds(tracts_demog,"~/Downloads/tracts_demog.RDS")

tracts_demog_pts <- tracts_demog
tracts_demog_pts[,("centroids"):=st_centroid(geometry)]
tracts_demog_pts[,("longitude"):=unlist(map(centroids,1))]
tracts_demog_pts[,("latitude"):=unlist(map(centroids,2))]
st_write(tracts_demog_pts,"~/Downloads/tracts_demog_pts.geojson",driver = "GeoJSON")

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


HISD_HS <- st_read(paste0(censusdir, "2020/geo_census/HISDHighAttendanceZones1920/", "HighAttendanceZones1920.shp"))
coords <- st_as_sf(Tx_tract_demog8)
HISD_HS <- st_transform(HISD_HS,crs = st_crs(coords))
st_write(HISD_HS,"~/Downloads/HISD_HS.geojson",driver = "GeoJSON")

#for the vaccine stuff

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
