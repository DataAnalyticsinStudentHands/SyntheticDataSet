make_HCAD_geom <- function(hcadDataDir,censusDT){
  # the Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
  parcels <- st_read(paste0(hcadDataDir, "Tax\ Parcels\ (Shapefiles)/Parcels.shp"))
  print(paste0("Number of parcels from raw file: ",nrow(parcels)))
  # remove invalid parcels
  parcels$valid <- st_is_valid(parcels)
  HCAD_parcels <- subset(parcels,parcels$valid)
  HCAD_parcels$valid <- NULL
  print(paste0("Number of valid parcels files: ",nrow(HCAD_parcels),", which is ",nrow(parcels)-nrow(HCAD_parcels)," invalid parcel geometries"))
  #add to blocks
  censusDT <- st_as_sf(censusDT)
  HCAD_parcels <- st_transform(HCAD_parcels,crs = st_crs(censusDT))
  print("There are a handful of polygon warnings because of unexpected geometries.")
  CensusBGforHCADParcels <- st_within(HCAD_parcels, censusDT)
  CensusBGforHCADParcelsunlisted <- rapply(CensusBGforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  CensusBGforHCADParcelsunlisted <- unlist(CensusBGforHCADParcelsunlisted)
  
  # add census tract information to each parcel
  HCAD_parcels$acct=HCAD_parcels$HCAD_NUM
  HCAD_parcels$county=censusDT$COUNTY[CensusBGforHCADParcelsunlisted] #a handful have their centroid in one of 7 neighboring counties
  HCAD_parcels$GEOID=censusDT$GEOID[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$tract=censusDT$TRACTCE[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$zipcode=censusDT$zipcode[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$voting_district=censusDT$voting_district[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$PUMA_ID=censusDT$PUMA_ID[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$school_district=censusDT$school_district[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$place_name=censusDT$place_name[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$state_leg_lower=censusDT$state_leg_lower[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$state_leg_upper=censusDT$state_leg_upper[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$US_house_district=censusDT$US_house_district[CensusBGforHCADParcelsunlisted]
  HCAD_parcels$opportunity_zone_2019=censusDT$opportunity_zone_2019[CensusBGforHCADParcelsunlisted]
  HCAD_parcels <- as.data.table(HCAD_parcels)
  #print("columns available:")
  #print(names(HCAD_parcels))
  #zip and zipcode are slightly different; need to do a deep dive on why; mail_zip should be owners
  HCAD_parcels <- HCAD_parcels[,c("acct","parcel_typ","Acreage","LocAddr","LocNum","LocName","city","zip","zipcode","mail_zip","yr_impr",
                                  "place_name","voting_district","state_leg_lower","state_leg_upper","school_district","US_house_district",
                                  "PUMA_ID","opportunity_zone_2019","geometry")]
  return(HCAD_parcels)
}

HCAD_join_res_build_geom <- function(hcadDataDir,HCAD_parcels){
  HCAD_res_build <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Building\ Information/building_res.txt"),
                              stringsAsFactors = FALSE,
                              sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  HCAD_res_build$acct <- str_remove_all(HCAD_res_build$acct, " ")
  #length(unique(HCAD_res_build$acct)) #1182396 of 1210552 - looked individually and seems like first row matters
  HCAD_res_build <- as.data.table(HCAD_res_build)
  HCAD_res_build <- unique(HCAD_res_build, by=("acct"))
  HCAD_res_build <- HCAD_res_build[,c("acct","impr_tp","structure_dscr","dpr_val","qa_cd","dscr","date_erected","yr_remodel","im_sq_ft","bld_adj")]
  HCAD <- merge(HCAD_parcels,HCAD_res_build, by = "acct", all = TRUE)
  return(HCAD)
}

HCAD_join_real_build_geom <- function(hcadDataDir,HCAD){
  #there are no account numbers in common between res_build and real_build, but some codes are in both
  HCAD_real_build <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Building\ Information/building_other.txt"),
                               stringsAsFactors = FALSE, 
                               sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  #using actual_area for total_bldg_sf - get units from fixtures
  #total_income may include non-rental income?
  HCAD_real_build$acct <- str_remove_all(HCAD_real_build$acct, " ")
  HCAD_real_build <- as.data.table(HCAD_real_build)
  HCAD_real_build <- unique(HCAD_real_build, by=("acct"))
  HCAD_real_build <- HCAD_real_build[,c("acct","impr_tp","structure_dscr","Depr_Val","qa_cd","dscr","date_erected","yr_remodel","im_sq_ft","prop_nm","units",
                                        "lease_rt","occ_rt","tot_inc")]
  setnames(HCAD_real_build,c("impr_tp","structure_dscr","Depr_Val","qa_cd","dscr","date_erected","yr_remodel","im_sq_ft"),
           c("real_impr_tp","real_structure_dscr","real_Depr_Val","real_qa_cd","real_dscr","real_date_erected","real_yr_remodel","real_im_sq_ft"))
  HCAD <- merge(HCAD,HCAD_real_build, by = "acct", all = TRUE)
  return(HCAD)
}

HCAD_join_real_acct_geom <- function(hcadDataDir,HCAD){
  HCAD_real_acct <- read.csv2(paste0(hcadDataDir, "Real\ Property\ Data/real_acct.txt"),
                              stringsAsFactors = FALSE,
                              sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  HCAD_real_acct$acct <- str_remove_all(HCAD_real_acct$acct, " ")
  HCAD_real_acct <- as.data.table(HCAD_real_acct)
  HCAD_real_acct <- unique(HCAD_real_acct, by=("acct"))
  #need to look at what we'd want to use
  HCAD_real_acct <- HCAD_real_acct[,c("acct","school_dist","Market_Area_1","Market_Area_1_Dscr","econ_area","econ_bld_class","land_val","bld_val","x_features_val",
                                      "assessed_val","tot_appr_val","tot_mkt_val","prior_tot_mkt_val")]
  HCAD <- merge(HCAD,HCAD_real_acct, by = "acct", all = TRUE)
  return(HCAD)
}

HCAD_join_owner_date_geom <- function(hcadDataDir,HCAD){
  HCAD_owner_date <- read.csv2(paste0(hcadDataDir, "Real\ Property\ Data/deeds.txt"),
                               stringsAsFactors = FALSE,
                               sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  
  HCAD_owner_date$acct <- str_remove_all(HCAD_owner_date$acct, " ")
  HCAD_owner_date <- as.data.table(HCAD_owner_date)
  HCAD_owner_date <- HCAD_owner_date[,c("acct","dos")]
  setnames(HCAD_owner_date,"dos","date_of_sale")
  HCAD_owner_date <- unique(HCAD_owner_date, by=("acct"))
  
  #need to look at what we want to do
  #  #remember to replace vintage with date, below
  #  HCAD_owner_date <- HCAD_owner_date %>%
  #    rename(account=V1,purchase_date=V2) %>%
  #    distinct(account,.keep_all = TRUE) %>%
  #    filter(!row_number()==1) %>%
  #    select(account,purchase_date) #%>%
  #  #  mutate(
  #  #    yrs_in_house = 2022 - as.integer(substr(as.character.Date(HCAD_owner_date$V2),7,10))
  #  #  )
  #  HCAD <- full_join(HCAD,HCAD_owner_date,by="account")
  HCAD <- merge(HCAD,HCAD_owner_date, by = "acct", all = TRUE)
  return(HCAD)
}

#exemptions give us some clues about owners, etc. that match up with ACS info
#cf. jur_exemption_dscr.txt 
#exempt_cat	exemption_dscr. #exemption_dscr is not given in same file; they seem to be using more as time goes on
#ABT	Abatement
#APD	Apportioned Partial Disability
#APO	Apportioned Partial Over-65
#APR	Apportioned Partial Residential
#DIS	Disability
#HIS	Historical
#LIH	Low Income Housing
#MCL	Methane Capture at Landfill
#OVR	Over-65
#PAR	Partial Residential Homestead
#PDS	Partial Disability
#PEX	Partial Total
#POL	Pollution Control
#POV	Partial Over-65
#PRO	Prorated
#RES	Residential Homestead
#SOL	Solar
#SPE	Self Provided Emergency Services (TOTAL exemption)
#SPX	Spaceport Exemption (TOTAL exemption)
#SSA	Surviving Spouse Active Duty
#SSD	Surviving Spouse Disability
#STT	Surviving Spouse Total Transfer
#STX	Surviving Spouse Vet Disability Total Exemption
#SUR	Surviving Spouse Over-65
#TOT	Total
#V11	Vet Disability #1 10-29 pct
#V12	Vet Disability #1 30-49 pct
#V13	Vet Disability #1 50-69 pct
#V14	Vet Disability #1 70-100 pct
#V21	Vet Disability #2 10-29 pct
#V22	Vet Disability #2 30-49 pct
#V23	Vet Disability #2 50-69 pct
#V24	Vet Disability #2 70-100 pct
#VCH	Vet Charitable Disability
#VS1	Vet Survivor 10-29 pct
#VS2	Vet Survivor 30-49 pct
#VS3	Vet Survivor 50-69 pct
#VS4	Vet Survivor 70-100 pct
#VTX	Vet Disability Total Exemption
HCAD_join_exemptions_geom <- function(hcadDataDir,HCAD){
  HCAD_exempt <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Jurisdiction\ Information/jur_exempt_cd.txt"),
                           stringsAsFactors = FALSE,
                           sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  HCAD_exempt$acct <- str_remove_all(HCAD_exempt$acct, " ")
  HCAD_exempt <- as.data.table(HCAD_exempt)
  #HCAD_exempt <- unique(HCAD_exempt, by=("acct")) #no duplicates in jur_exempt_cd.txt, but others do
  HCAD_exempt[,("exempt_type_homestead"):=fcase(str_detect(exempt_cat,'RES') | str_detect(exempt_cat,'PAR'),"homestead", default = "not homestead")]
  HCAD_exempt[,("exempt_type_Veteran"):=fcase(str_detect(exempt_cat,"V1") | str_detect(exempt_cat,"V2"),"Veteran",default = "not veteran")]
  HCAD_exempt[,("exempt_type_over_65"):=fcase(str_detect(exempt_cat,"APO") | str_detect(exempt_cat,"OVR") |
                                                str_detect(exempt_cat,"POV") | str_detect(exempt_cat,"SUR"),"Over 65",default = "not over 65")] #starts using more over time
  HCAD_exempt[,("exempt_type_surviving_spouse"):=fcase(str_detect(exempt_cat,"SSD") | str_detect(exempt_cat,"STT") |
                                                         str_detect(exempt_cat,"STX") | str_detect(exempt_cat,"SUR") |
                                                         str_detect(exempt_cat,"VS") | str_detect(exempt_cat,"VTX"),"Surviving Spouse",default = "Not surviving spouse")] #starts using more over time
  HCAD_exempt[,("exempt_type_disabled"):=fcase(str_detect(exempt_cat,"PDS") | str_detect(exempt_cat,"DIS") |
                                                 str_detect(exempt_cat,"STX") | str_detect(exempt_cat,"APD") |
                                                 str_detect(exempt_cat,"VCH") | str_detect(exempt_cat,"VTX"),"Disabled",default = "Not disabled")] #starts using more over time
  HCAD_exempt[,("exempt_type_low_income_housing"):=fcase(str_detect(exempt_cat,"LIH"),"Low income housing", default = "not low income")]
  HCAD_exempt[,("exempt_type_abatement"):=fcase(str_detect(exempt_cat,"ABT"),"Abatement", default = "Not abatement")]
  HCAD_exempt[,("exempt_type_historical"):=fcase(str_detect(exempt_cat,"HIS"),"Historical", default = "Not historical")]
  HCAD_exempt[,("exempt_type_solar"):=fcase(str_detect(exempt_cat,"SOL"),"Solar", default = "Not solar")]
  HCAD_exempt[,("exempt_type_total"):=fcase(str_detect(exempt_cat,"TOT"),"Total", default = "Not total")] 
  #taking all columns
  HCAD <- merge(HCAD,HCAD_exempt, by = "acct", all = TRUE)
  return(HCAD)
}

HCAD_join_fixtures_geom <- function(hcadDataDir,HCAD){
  HCAD_fixtures <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Building\ Information/fixtures.txt"),
                             stringsAsFactors = FALSE,
                             sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  #for bldg_data codes: http://pdata.hcad.org/Desc/2015/code_desc_real.txt #updated: https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
  HCAD_fixtures$acct <- str_remove_all(HCAD_fixtures$acct, " ")
  HCAD_fixtures <- as.data.table(HCAD_fixtures)
  
  #HCAD only puts bedrooms on first bld_num for apts for some reason; random ones are separated...  
  #need to do some testing! - ugh!!!
  HCAD_fixtures[,("bedrooms"):=fcase(str_detect(type,"RMB"),as.integer(units),default = as.integer(0))] #bedrooms in residence, although can be multiple buildings on plat
  HCAD_fixtures[,"efficiency_apts":=fcase(str_detect(type,"AP0"),as.integer(units)+as.integer(bedrooms),default = as.integer(0))]
  HCAD_fixtures[,"1_bedroom_apts":=fcase(str_detect(type,"AP1"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP1"),as.integer(units)+as.integer(bedrooms),default = bedrooms)]
  HCAD_fixtures[,"2_bedroom_apts":=fcase(str_detect(type,"AP2"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP2"),(as.integer(units)+as.integer(bedrooms))*as.integer(2),default = bedrooms)]
  HCAD_fixtures[,"3_bedroom_apts":=fcase(str_detect(type,"AP3"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP3"),(as.integer(units)+as.integer(bedrooms))*as.integer(3),default = bedrooms)]
  HCAD_fixtures[,"4_bedroom_apts":=fcase(str_detect(type,"AP4"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP4"),(as.integer(units)+as.integer(bedrooms))*as.integer(4),default = bedrooms)]
  #HCAD_fixtures[,total_bedrooms_bldg := sum(bedrooms),by = c("acct","bld_num")] #keeping bld_num for geo, and then summing for other calculations; they just use whatever bld_num the apt uses, not a count
  HCAD_fixtures[,("total_beds") := sum(bedrooms),by=(acct)]
  #trying to fix a couple of outliers, esp. acct=="1455620010001"&str_detect(type,"UNT"), but not assuming it's always wrong
  HCAD_fixtures[,("true_units_each"):=fcase(str_detect(type,"UNT"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,("true_units"):=sum(true_units_each),by=(acct)]
  HCAD_fixtures[,("total_beds") := fcase(total_beds>2000,true_units*as.integer(2),default = total_beds)]
  #length(HCAD_fixtures[total_bedrooms_bldg!=total_beds]) #13 - so very few, but still use total_beds
  HCAD_fixtures <- unique(HCAD_fixtures,by = ("acct"))
  

  
  #there's one massive outlier in 2020 where they put in s.f. instead of number of units
  #true_units = as.integer(HCAD_fixtures[acct=="1455620010001"&str_detect(type,"UNT"),units])*as.integer(2)
  #HCAD_fixtures[,("total_beds"):=fcase(acct=="1455620010001",true_units,default = total_beds)]
  #HCAD_fixtures[,("units"):=fcase(acct=="1455620010001",as.character(as.integer(true_units/2)),default = units)]
  #if keep number of bedrooms, etc., need to change for this one - it's such a big error.
  
  #HCAD_fixtures <- unique(HCAD_fixtures,by = c("acct","bld_num")) #look at parcels to see if we can autofill by bld_num
  
  #.89 is the overall Harris County occupancy, but each bldg has a number, too
  #add apt_ids
  HCAD <- merge(HCAD,HCAD_fixtures, by = "acct", all = TRUE)
  return(HCAD)
}