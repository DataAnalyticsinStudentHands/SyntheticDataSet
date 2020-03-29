#notes toward new process for matching census and ACS to HCAD, NHANES, SHED
# Using Parcels from 2015, and we should think through aging from there.

#The Anaximander Fragment: Der Ursprung der Dinge ist das Grenzenlose. Woraus sie entstehen, darein vergehen sie auch mit Notwendigkeit. Denn sie leisten einander Buße und Vergeltung für ihr Unrecht nach der Ordnung der Zeit.
#a constrained sample/luck is telling a story as a hypergraph

# from http://pdata.hcad.org/GIS/index.html
# got Parcels 2015, Hwy, School, City, Sub_poly (subdivisions), TIRZ
#on 12/27/2019 got 2017 of Parcels, and most recent of others

#got 2015 acct information from: http://pdata.hcad.org/download/2015.html
# PP-files, real_account, and real_building
#downloaded 2017 from http://pdata.hcad.org/download/2017.html on Dec. 27, 2019.

#http://hcad.org/hcad-resources/hcad-appraisal-codes/
#http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ also exists, but has some diffs

#code descriptions for real are from: http://pdata.hcad.org/Desc/2015/code_desc_real.txt #updated https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
#and personal: http://pdata.hcad.org/Desc/2015/code_desc_personal.txt #updated: #https://pdata.hcad.org/data/cama/2015/code_desc_personal.txt
#also exists, but has some diffs, perhaps because of year?
#http://hcad.org/hcad-resources/hcad-appraisal-codes/
#http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ 


#they also provide column names from a screenshot of their Access db: http://pdata.hcad.org/DB/Pdata_Fieldnames.pdf
#and http://pdata.hcad.org/Desc/Layout_and_Length.txt
#census geometry files were taken on 12/27/2019 from: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html

library(sf)
library(dplyr)
library(data.table)

#start with loading files we need from HCAD
HCAD_parcels <- st_read(paste0(housingdir, vintage, "/Parcels_", vintage, "_Oct/Parcels.shp"),stringsAsFactors = FALSE)
#did all the geo joins and manipulations on this, before adding the other columns from HCAD - may have to rename HCAD_parcels <- HCAD

HCAD_res_build <- read.csv2(paste0(housingdir, vintage, "/Real_building_land/building_res.txt"),
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_real_build <- read.csv2(paste0(housingdir, vintage, "/Real_building_land/building_other.txt"),
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
#there are no account numbers in common between res_build and real_build, but some codes are in both

HCAD_real_acct <- read.csv2(paste0(housingdir, vintage, "/Real_acct_owner/real_acct.txt"),
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
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
  rename(account=V1,improv_typ=V4,bldg_value=V8,qual_descript=V12,
          date_erected=V13,yr_remodel=V15,living_sf=V24,nbhd_factor=V28,
         size_index=V30) %>%
  select(account,improv_typ,bldg_value,qual_descript,date_erected,yr_remodel,
         living_sf,nbhd_factor,size_index) 
#there are 53,770 records in res_build that don't match with parcel?
#do a full_join, then select out later - depending on other how well other filtering works - is this what grew the account size??
HCAD <- full_join(HCAD_parcels,HCAD_res_build,by='account')

#using actual_area for total_bldg_sf - get units from fixtures
#total_income may include non-rental income?
HCAD_real_build$V1 <- str_remove_all(HCAD_real_build$V1, " ")
HCAD_real_build <- HCAD_real_build %>% 
  rename(account=V1,improv_typ_real=V4,
        bldg_value_real=V9,qual_descript=V14,date_built=V15,
        yr_remodel=V16,bldg_sf=V27,nbhd_factor=V28,bldg_name=V32,units=V33,  #units in real_building is more like apt. number not number of apts?
        lease_rate=V35,occupancy_rate=V36,total_income=V37) %>%
  select(account,improv_typ_real,bldg_value_real,qual_descript,date_built,yr_remodel,bldg_sf,
        nbhd_factor,bldg_name,units,lease_rate,occupancy_rate,total_income)
#correctional <- HCAD_real_build %>% filter(improv_typ=="4670") 
#30 correctional facilities, without number of rooms
HCAD <- full_join(HCAD,HCAD_real_build,by="account")

HCAD_real_acct$V1 <- str_remove_all(HCAD_real_acct$V1, " ")
HCAD_real_acct <- HCAD_real_acct %>% 
  rename(account=V1,tot_bldg_sf=V39,tot_land_sf=V40,land_value=V44,
         improv_value=V45,xtras_value=V46,market_value=V50) %>%
  select(account,tot_bldg_sf,tot_land_sf,land_value,improv_value,
         xtras_value,market_value)
#will use market_value to guestimate cost of apt.; here, other codes will also help decide (like LHI)
HCAD <- full_join(HCAD,HCAD_real_acct,by="account")

HCAD_owner_date <- HCAD_owner_date %>%
  rename(account=V1,purchase_date=V2) %>%
  select(account,purchase_date) %>%
  mutate(
    yrs_in_house = 2015 - as.integer(substr(as.character.Date(HCAD_owner_date$V2),7,10))
  )
HCAD <- full_join(HCAD,HCAD_owner_date,by="account")

#HCAD_exempt$V1 <- str_remove_all(HCAD_exempt$V1, " ") - fixed by importing col as character
#exemptions give us some clues about owners, etc. that match up with ACS info
#cf. jur_exemption_dscr.txt in Real_jur_exempt
HCAD_exempt <- HCAD_exempt %>% 
  rename(account=V1,exempt_cat=V2) %>% 
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

#Not sure how to deal with:
#4670 - Correctional; 4313 = Dormitory; 4320 - Extended Stay; 4318 - Boarding House
#4213 = Mobile Home Parks (in real) - 1008 = Mobile Homes (in res) has rooms listed, so works
#it's possible that we could just know where they are and do some rough and ready by sf????



st_geometry(HCAD) <- NULL
HCAD <- HCAD %>%
  select(-NADcentroids)


HCAD_residences <- HCAD %>%
  filter(is.na(improv_typ_real)) #there are a significant number that are na on improv_typ, still, but chose to keep them for wholeness here
#cf: they seem to be things like commercial land in residential areas, some church properties, and other odds and ends
HCAD_businesses <- HCAD %>%
  filter(!is.na(improv_typ_real))



saveRDS(HCAD,file = paste0(housingdir, vintage, "/HCAD_pts_",Sys.Date(),".RDS"))
saveRDS(HCAD_residences,file = paste0(housingdir, vintage, "/HCAD_residences_",Sys.Date(),".RDS"))
saveRDS(HCAD_businesses,file = paste0(housingdir, vintage, "/HCAD_businesses_",Sys.Date(),".RDS"))

#HCAD as a whole has all the spatial info we may need for some of the SDoH calculations 

#depends on where pulling it from
HCAD_dt <- as.data.table(HCAD_residences)
HCAD_dt[,tract:=droplevels(tract)]
HCAD_dt <- HCAD_dt[!is.na(improv_typ)] #returns 1815741
HCAD_res <- HCAD_dt[!duplicated(account)]
HCAD_res <- HCAD_res[!is.na(LocAddr)]

HCADbus_dt <- as.data.table(HCAD_businesses)
HCADbus_dt[,tract:=droplevels(tract)]
HCADbus_dt <- HCADbus_dt[!is.na(improv_typ_real)]
HCAD_bus <- HCADbus_dt[!duplicated(account)]
HCAD_bus <- HCAD_bus[!is.na(LocAddr)]

#clean up HCAD_bus to have right number of units
#https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
#are these all owned vs. rented?
HCAD_bus[improv_typ_real=="1001",apt_units:=1] #Residential Single Family - not sure how different from res, but not in both
HCAD_bus[improv_typ_real=="1002",apt_units:=2] #Residential Duplex
HCAD_bus[improv_typ_real=="1005",apt_units:=1] #Mixed use - only five, look to be single family in business
HCAD_bus[improv_typ_real=="1008",apt_units:=1] #Mobile home 
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
HCAD_bus[improv_typ_real=="4319" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Commercial Bldg. - Mixed Res.
HCAD_bus[improv_typ_real=="4319" & is.na(as.numeric(units)),apt_units:=40] #Commercial Bldg. - Mixed Res.
#supermarkets are 4347

#get each apt_units a new row
HCAD_apts <- HCAD_bus[!is.na(apt_units)]
HCAD_apts <- HCAD_apts %>% uncount(apt_units,.id = "apt_number_id",.remove = TRUE)
HCAD_homes <- rbind(HCAD_apts,HCAD_res,fill=TRUE) #choking on ptcoords!!!

