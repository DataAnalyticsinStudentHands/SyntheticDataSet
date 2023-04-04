#notes toward new process for matching census and ACS to HCAD, NHANES, SHED
# Using Parcels from 2015, and we should think through aging from there.

#The Anaximander Fragment: Der Ursprung der Dinge ist das Grenzenlose. Woraus sie entstehen, darein vergehen sie auch mit Notwendigkeit. Denn sie leisten einander Buße und Vergeltung für ihr Unrecht nach der Ordnung der Zeit.
#a constrained sample/luck is telling a story as a hypergraph

# from http://pdata.hcad.org/GIS/index.html
# got Parcels 2015, Hwy, School, City, Sub_poly (subdivisions), TIRZ
#on 12/27/2019 got 2017 of Parcels, and most recent of others

#on 7/15/2022, got 2005 (no personal property data), 2010, 2020, 2021, 2022 downloads from https://hcad.org/pdata/pdata-property-downloads.html#

#got 2015 acct information from: http://pdata.hcad.org/download/2015.html - 2005 - 2020 is available.
# PP-files, real_account, and real_building
#downloaded 2017 from http://pdata.hcad.org/download/2017.html on Dec. 27, 2019.

#http://hcad.org/hcad-resources/hcad-appraisal-codes/
#http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ also exists, but has some diffs

#code descriptions for real are from: http://pdata.hcad.org/Desc/2015/code_desc_real.txt #updated https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
#and personal: http://pdata.hcad.org/Desc/2015/code_desc_personal.txt #updated: #https://pdata.hcad.org/data/cama/2015/code_desc_personal.txt
#also exists, but has some diffs, perhaps because of year?


#they also provide column names from a screenshot of their Access db: http://pdata.hcad.org/DB/Pdata_Fieldnames.pdf
#and http://pdata.hcad.org/Desc/Layout_and_Length.txt
#census geometry files were taken on 12/27/2019 from: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html

library(sf)
library(stringr)
library(dplyr)
library(data.table)

#add Hcad_geom from HCAD_geo.R

HCAD_res_build <- read.csv2(paste0(housingdir, vintage, "/Real_building_land/building_res.txt"),
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_real_build <- read.csv2(paste0(housingdir, vintage, "/Real_building_land/building_other.txt"),
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
#there are no account numbers in common between res_build and real_build, but some codes are in both

HCAD_real_acct <- read.csv2(paste0(housingdir, vintage, "/Real_acct_owner/real_acct.txt"),
        stringsAsFactors = FALSE,
        sep = "\t", header = TRUE, quote="",colClasses = c("V1"="character"))
HCAD_exempt <- read.csv2(paste0(housingdir, vintage, "/Real_jur_exempt/jur_exempt_cd.txt"),
                         stringsAsFactors = FALSE,
                         sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_owner_date <- read.csv2(paste0(housingdir, vintage, "/Real_acct_owner/deeds.txt"),
                             stringsAsFactors = FALSE,
                             sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_fixtures <- read.csv2(paste0(housingdir, vintage, "/Real_building_land/fixtures.txt"),
                           stringsAsFactors = FALSE,
                           sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))



#for res, they use effective area (V24) for calculating improvement value
#they have a nbhd_factor that asthma folks could use, but it's not there for apts.
HCAD_res_build$V1 <- str_remove_all(HCAD_res_build$V1, " ")
HCAD_res_build <- HCAD_res_build %>% 
  rename(account=V1,improv_typ=V4,impr_mdl_cd=V5,bldg_value=V8,qual_descript=V12,
          date_erected=V13,yr_remodel=V15,living_sf=V24,nbhd_factor=V28,
         size_index=V30) %>%
  distinct(account,.keep_all = TRUE) %>%
  filter(!row_number()==1) %>%
  select(account,improv_typ,impr_mdl_cd,bldg_value,qual_descript,date_erected,yr_remodel,
         living_sf,nbhd_factor,size_index) 
HCAD <- full_join(HCAD_geom,HCAD_res_build,by='account')

#using actual_area for total_bldg_sf - get units from fixtures
#total_income may include non-rental income?
HCAD_real_build$V1 <- str_remove_all(HCAD_real_build$V1, " ")
HCAD_real_build <- HCAD_real_build %>% 
  rename(account=V1,improv_typ_real=V4,impr_mdl_cd=V5,
        bldg_value_real=V9,qual_descript=V14,date_built=V15,
        yr_remodel=V16,bldg_sf=V27,nbhd_factor=V28,bldg_name=V32,units=V33,  #units in real_building is more like apt. number not number of apts?
        lease_rate=V35,occupancy_rate=V36,total_income=V37) %>%
  #distinct(account,.keep_all = TRUE) %>%
  filter(!row_number()==1) %>%
  select(account,improv_typ_real,impr_mdl_cd,bldg_value_real,qual_descript,date_built,yr_remodel,bldg_sf,
        nbhd_factor,bldg_name,units,lease_rate,occupancy_rate,total_income)
#correctional <- HCAD_real_build %>% filter(improv_typ=="4670") 
#30 correctional facilities, without number of rooms
HCAD <- full_join(HCAD,HCAD_real_build,by="account")

HCAD_real_acct$V1 <- str_remove_all(HCAD_real_acct$V1, " ")
HCAD_real_acct <- HCAD_real_acct %>% 
  rename(account=V1,tot_bldg_sf=V39,tot_land_sf=V40,land_value=V44,
         improv_value=V45,xtras_value=V46,market_value=V50) %>%
  distinct(account,.keep_all = TRUE) %>%
  filter(!row_number()==1) %>%
  select(account,tot_bldg_sf,tot_land_sf,land_value,improv_value,
         xtras_value,market_value)
#will use market_value to guestimate cost of apt.; here, other codes will also help decide (like LHI)
HCAD <- full_join(HCAD,HCAD_real_acct,by="account")

#remember to replace vintage with date, below
HCAD_owner_date <- HCAD_owner_date %>%
  rename(account=V1,purchase_date=V2) %>%
  distinct(account,.keep_all = TRUE) %>%
  filter(!row_number()==1) %>%
  select(account,purchase_date) #%>%
#  mutate(
#    yrs_in_house = 2022 - as.integer(substr(as.character.Date(HCAD_owner_date$V2),7,10))
#  )
HCAD <- full_join(HCAD,HCAD_owner_date,by="account")

#HCAD_exempt$V1 <- str_remove_all(HCAD_exempt$V1, " ") - fixed by importing col as character
#exemptions give us some clues about owners, etc. that match up with ACS info
#cf. jur_exemption_dscr.txt in Real_jur_exempt
HCAD_exempt <- HCAD_exempt %>% 
  rename(account=V1,exempt_cat=V2) %>% 
  distinct(account,.keep_all = TRUE) %>%
  filter(!row_number()==1) %>%
  select(account,exempt_cat) %>% 
  mutate(
      homestead = case_when(  #trying to get at who owns...
        exempt_cat %in% c('RES','PAR') ~ TRUE
      ),
      vet = case_when(
        exempt_cat %in% c('V11','V12','V13','V14','V21','V22','V23','V24') ~ TRUE
      ),
      low_income_housing = case_when(
        exempt_cat == "LIH" ~ TRUE
      ),
      over_65 = case_when(
        exempt_cat %in% c('APO','OVR','POV','SUR') ~ TRUE
      ),
      surv_spouse = case_when(
        exempt_cat %in% c('SSD','STT','STX','SUR','VS1','VS2','VS3','VS4','VTX') ~ TRUE
      ),
      disabled = case_when(
        exempt_cat %in% c('PDS','DIS','APD','STX','V11','V12','V13','V14','V21','V22','V23','V24','VCH','VTX') ~ TRUE
      )
    )
HCAD <- full_join(HCAD,HCAD_exempt,by="account")

#for bldg_data codes: http://pdata.hcad.org/Desc/2015/code_desc_real.txt #updated: https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
#add counts for rooms and apartments
HCAD_fixtures <- HCAD_fixtures %>%
  rename(account=V1,bldg_num=V2,bldg_data=V3,
         bldg_data_descr=V4,num_units=V5) %>%
  distinct(account,.keep_all = TRUE) %>%
  filter(!row_number()==1) %>%
  select(account,bldg_num,bldg_data,bldg_data_descr,num_units) %>%
  mutate(
    bedrooms = case_when(
      bldg_data == 'RMB ' ~ as.integer(num_units),
      bldg_data == 'AP0 ' ~ as.integer(0),
      bldg_data == 'AP1 ' ~ as.integer(1),
      bldg_data == 'AP2 ' ~ as.integer(2),
      bldg_data == 'AP3 ' ~ as.integer(3),
      bldg_data == 'AP4 ' ~ as.integer(4)
    ),
    total_rooms = case_when(
      bldg_data == 'RMT ' ~ as.integer(num_units)
    ),
    effcncy_apts = case_when(
      bldg_data == 'AP0 ' ~ as.integer(num_units)
    ),
    oneBR_apts = case_when(
      bldg_data == 'AP1 ' ~ as.integer(num_units)
    ),
    twoBR_apts = case_when(
      bldg_data == 'AP2 ' ~ as.integer(num_units)
    ),
    threeBR_apts = case_when(
      bldg_data == 'AP3 ' ~ as.integer(num_units)
    ),
    fourBR_apts = case_when(
      bldg_data == 'AP4 ' ~ as.integer(num_units),
      TRUE ~ as.integer(0)
    ),
    num_apts = case_when(  #maybe not use - seems to have a lot of missing!
      grepl('AP', bldg_data) ~ as.integer(num_units),
      #bldg_data == 'UNT ' ~ as.integer(num_units), #need to confirm it adds correctly
      TRUE ~ as.integer(0)
    )
  ) %>%
  filter(bedrooms>=0 | total_rooms>0 | effcncy_apts>0 | oneBR_apts>0 | 
           twoBR_apts>0 | threeBR_apts>0 | fourBR_apts>0 | num_apts>0 ) 

#expand so each apt has its own record
HCAD_fixtures <- data.frame(HCAD_fixtures[rep(seq_len(dim(HCAD_fixtures)[1]), 
            as.integer(HCAD_fixtures$num_apts)), 
            #as.integer(HCAD_fixtures$num_apts*.89)), #in case want to do occupancy this way
            1:dim(HCAD_fixtures)[2], drop = FALSE], row.names = NULL)
#.89 is the overall Harris County occupancy - I have it in another place, too
#add apt_ids
HCAD_fixtures <- HCAD_fixtures %>%
  group_by(account) %>%
  mutate(
    apt_id = paste0(account,rep(1:n()))
  )
HCAD <- full_join(HCAD,HCAD_fixtures,by="account")
#check this!!!! - what are the number of accounts, and how many have unique apt_id!!!!!
#think about how to add geometry for each person to be separate in representations.

#http://pdata.hcad.org/Desc/2015/code_desc_real.txt
#in bldg_data -                                                                                                                                                           
#AP0                 Apt:  Effcncy Unit                                                                                                                                                                  
#AP1                 Apt:  1-Bedroom Unit                                                                                                                                                                
#AP2                 Apt:  2-Bedroom Unit                                                                                                                                                                
#AP3                 Apt:  3-Bedroom Unit                                                                                                                                                                
#AP4                 Apt:  4-Bedroom Unit 
#RMB                 Room:  Bedroom                                                                                                                                                                      
#RMT                 Room:  Total
#UNT                 Number of Apartment Units 

#https://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-type-codes/
#perhaps still need to add logic for: (some come from improv_typ.x, some from improv_typ.y - res vs. real)
#NOT run - just in case we want to drill down
fourplex4 <- HCAD %>% filter(improv_typ.x == 1004) #looks like each account has it listed as 8 bedrooms, instead of as 4 apts.
#1002 = duplex, 1003 = triplex, with same problems.

#if we did NADcentroids on parcels earlier
#st_geometry(HCAD) <- NULL
#HCAD <- HCAD %>%
#  select(-NADcentroids)
HCAD <- as.data.table(HCAD)
censusblocks <- as.data.table(censusblocks) #update its indices
censusblocks[,("geoid"):=GEOID]
HCAD <- as.data.table(HCAD)
censusblocks <- as.data.table(censusblocks)
#censusblocks[,("geoid"):=GEOID]
HCAD <- unique(HCAD,by=c("account","apt_id")) #check - this eliminated the expansion by apartment type - 
HCAD[geoid%in%unique(censusblocks$geoid),c("closest_food_distance","closest_food_account",
        "fast_food_distance_min","fast_food_account",
        "convenience_stores_distance","convenience_stores_account",
        "shop_center_distance","shop_center_account",
        "super_market_distance","super_market_account",
        "metro_stop_distance_min","metro_stop_BSID"):=
            censusblocks[.SD,c(list(food_source_distance_min),list(closest_food_account),
                               list(fast_food_distance_min),list(fast_food_account),
                               list(convenience_stores_distance),list(convenience_stores_account),
                               list(shop_center_distance),list(shop_center_account),
                               list(super_market_distance),list(super_market_account),
                               list(metro_stop_distance_min),list(metro_stop_BSID)),
                         on=("geoid")]]

HCAD_residences <- HCAD %>%
  filter(is.na(improv_typ_real)) #there are a significant number that are na on improv_typ, still, but chose to keep them for wholeness here
#cf: they seem to be things like commercial land in residential areas, some church properties, and other odds and ends
HCAD_businesses <- HCAD %>%
  filter(!is.na(improv_typ_real))

#censustracts[,("min_bus_distance_by_pop"):=]

#for busstops - more than 24 hours!!
#metro_stops <- readRDS(paste0(houstondatadir,"2022/metro_stops/busstops_tracts.RDS"))
#metro_stops <- st_transform(metro_stops,st_crs(HCAD))
#metro_stops_1k <- st_is_within_distance(HCAD$centroid,metro_stops$geometry,dist = 1000)

#metro_stops_distance <- st_distance(HCAD$centroid,metro_stops1$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
#metro_distance <- as.data.table(metro_stops_distance)
#HCAD$min_h_distance <- apply(h_distance[,1:90], 1, min)

#for crime - picked up 7/21/2022 - https://www.houstontx.gov/police/cs/crime-stats-archives.htm

saveRDS(HCAD,file = paste0(housingdir, vintage, "/HCAD_pts_",Sys.Date(),".RDS"))
saveRDS(HCAD_residences,file = paste0(housingdir, vintage, "/HCAD_residences_",Sys.Date(),".RDS"))
saveRDS(HCAD_businesses,file = paste0(housingdir, vintage, "/HCAD_businesses_",Sys.Date(),".RDS"))

#HCAD as a whole has all the spatial info we may need for some of the SDoH calculations 

#depends on where pulling it from
HCAD_dt <- as.data.table(HCAD_residences)
HCAD_dt[,tract:=droplevels(tract)]
HCAD_dt <- HCAD_dt[!is.na(improv_typ)] #returns 1815741
HCAD_res <- HCAD_dt[!is.na(LocAddr)]
HCAD_res <- HCAD_res[!duplicated(account) | !improv_typ=="1001"] #not sure how that happened - should be single_family

HCADbus_dt <- as.data.table(HCAD_businesses)
HCADbus_dt[,tract:=droplevels(tract)]
HCADbus_dt <- HCADbus_dt[!is.na(improv_typ_real)]
HCAD_bus <- HCADbus_dt[!duplicated(account)]
HCAD_bus <- HCAD_bus[!is.na(LocAddr)]
HCAD_bus <- HCAD_bus[!is.na(account)]

#clean up HCAD_bus to have right number of units
#https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
#are these all owned vs. rented?
HCAD_bus[improv_typ_real=="1001",apt_units:=1] #Residential Single Family - not sure how different from res, but not in both
HCAD_bus[improv_typ_real=="1002",apt_units:=2] #Residential Duplex
HCAD_bus[improv_typ_real=="1005",apt_units:=1] #Mixed use - only five, look to be single family in business
HCAD_bus[improv_typ_real=="1008",apt_units:=1] #Mobile home 
#only adds 96 units above
HCAD_bus[improv_typ_real=="4108",apt_units:=1] #Commercial Mobile home
HCAD_bus[improv_typ_real=="4209",apt_units:=as.numeric(units)] #Apartment Struct. 4-20 Units
HCAD_bus[improv_typ_real=="4211",apt_units:=as.numeric(units)] #Apartment Garden (1 to 3 Stories) - doesn't match up with number generated from AP2, etc.
HCAD_bus[improv_typ_real=="4212",apt_units:=as.numeric(units)] #Apartment Mid Rise (4 to 12 Stories)
HCAD_bus[improv_typ_real=="4213" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Mobile Home Park
HCAD_bus[improv_typ_real=="4213" & is.na(as.numeric(units)),apt_units:=4] #Mobile Home Park
HCAD_bus[improv_typ_real=="4214",apt_units:=as.numeric(units)] #Apartment High Rise (13+ Stories)
HCAD_bus[improv_typ_real=="4221",apt_units:=as.numeric(units)] #Subsidized Housing
HCAD_bus[improv_typ_real=="4222",apt_units:=as.numeric(units)] #Apartment - Tax Credit
HCAD_bus[improv_typ_real=="4299",apt_units:=as.numeric(units)] #Apartment Structure
#HCAD_bus[improv_typ_real=="4313",apt_units:=as.numeric(units)] #Dormitories - can't get numbers from units
HCAD_bus[improv_typ_real=="4316" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Nursing Home - some didn't have units
HCAD_bus[improv_typ_real=="4316" & is.na(as.numeric(units)),apt_units:=40] #nursing Home
HCAD_bus[improv_typ_real=="4317",apt_units:=as.numeric(units)] #Retirement Home
HCAD_bus[improv_typ_real=="4318",apt_units:=as.numeric(units)] #Boarding & Rooming House
HCAD_bus[is.na(total_income),apt_units:=as.numeric(0)]
#HCAD_bus[improv_typ_real=="4319" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Commercial Bldg. - Mixed Res.
#HCAD_bus[improv_typ_real=="4319" & is.na(as.numeric(units)),apt_units:=40] #Commercial Bldg. - Mixed Res.
#supermarkets are 4347
#colleges and universities (4613): 1243320010001 is TSU; 0410070130026 is UH; 1318230000001 is Rice; 
HCAD_bus[account=="1243320010001",apt_units:=2500] #TSU - frosh dorms only?
HCAD_bus[account=="0410070130026",apt_units:=8500] #UH - guess, since I think some are listed differently
HCAD_bus[account=="0410070130026",apt_units:=3000] #Rice
#correctional facilities are improv_typ_real 4670
#for adding correctional facilities - in tract 210100 add account 0031220000001 from HCAD_bus;  The seven floor 701 Jail has 4,144 inmate beds, and a few others, it is Harris County Correctional Facility, 701 Jail
#in tract 100000 add account 0010120000010' The facility, which has 4,156 regular beds, 124 beds for the Medical Division, and 96 beds for MHMRA, is one American football field deep and two American football fields in length. It is Harris County Correctional Facility, 1200 Jail
HCAD_bus[account=="0031220000001",apt_units:=4500]
HCAD_bus[account=="0010120000010",apt_units:=4200]

#get each apt_units a new row
HCAD_apts <- HCAD_bus[!is.na(apt_units) & apt_units!=0]
HCAD_apts <- HCAD_apts %>% uncount(apt_units,.id = "apt_number_id",.remove = TRUE)
#HCAD_homes <- rbind(HCAD_apts,HCAD_res,fill=TRUE) #choking on ptcoords!!!
HCAD_homes <- bind_rows(HCAD_res,HCAD_apts) #Vectorizing 'sfc_POINT' elements may not preserve their attributes!  might have to do each?? looks right, but hard to be sure

#fix up a few things
HCAD_homes[,qual_descript:=if_else(is.na(qual_descript.x),qual_descript.y,qual_descript.x)]
HCAD_homes[,yr_remodel:=if_else(is.na(yr_remodel.x),yr_remodel.y,yr_remodel.x)]
HCAD_homes[,nbhd_factor:=if_else(is.na(nbhd_factor.x),as.numeric(nbhd_factor.y),as.numeric(nbhd_factor.x))] #or as.numeric?
HCAD_homes <- HCAD_homes[!is.na(tract)]
HCAD_homes[,building_val:=if_else(is.na(bldg_value),as.numeric(bldg_value_real),as.numeric(bldg_value))]
HCAD_homes <- HCAD_homes %>% 
  select(-qual_descript.x,-qual_descript.y,-yr_remodel.x,-yr_remodel.y,-nbhd_factor.x,-nbhd_factor.y,
         -lease_rate,-occupancy_rate,-bldg_value,-bldg_value_real,valid)

HCAD_homes[is.na(improv_typ),cost_per_sf:=tot_bldg_sf/improv_value]
HCAD_homes[!is.na(improv_typ),cost_per_sf:=living_sf/improv_value] #relative value - could sort from high to low and give id (subtracting 10%?)
HCAD_homes[is.na(improv_typ),avg_sf:=tot_bldg_sf/as.integer(units)] #size of living space to assign to
HCAD_homes[!is.na(improv_typ),avg_sf:=living_sf] #just to complete the column for future
HCAD_homes[,type_code:=if_else(is.na(improv_typ),improv_typ_real,improv_typ)] #also need to make one improv_typ



saveRDS(HCAD_apts,file = paste0(housingdir, vintage, "/HCAD_apts_",Sys.Date(),".RDS"))
saveRDS(HCAD_bus,file = paste0(housingdir, vintage, "/HCAD_bus_",Sys.Date(),".RDS"))
saveRDS(HCAD_res,file = paste0(housingdir, vintage, "/HCAD_res_",Sys.Date(),".RDS"))
saveRDS(HCAD_homes,file = paste0(housingdir, vintage, "/HCAD_homes_",Sys.Date(),".RDS"))

