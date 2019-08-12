#notes toward new process for matching census and ACS to HCAD, NHANES, SHED
# Using Parcels from 2015, and we should think through aging from there.

#The Anaximander Fragment: Der Ursprung der Dinge ist das Grenzenlose. Woraus sie entstehen, darein vergehen sie auch mit Notwendigkeit. Denn sie leisten einander Buße und Vergeltung für ihr Unrecht nach der Ordnung der Zeit.
#a constrained sample/luck is telling a story as a hypergraph

# from http://pdata.hcad.org/GIS/index.html
# got Parcels 2015, Hwy, School, City, Sub_poly (subdivisions), TIRZ

#got 2015 acct information from: http://pdata.hcad.org/download/2015.html
# PP-files, real_account, and real_building

#http://hcad.org/hcad-resources/hcad-appraisal-codes/
#http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ also exists, but has some diffs

#code descriptions for real are from: http://pdata.hcad.org/Desc/2015/code_desc_real.txt
#and personal: http://pdata.hcad.org/Desc/2015/code_desc_personal.txt
#also exists, but has some diffs, perhaps because of year?
#http://hcad.org/hcad-resources/hcad-appraisal-codes/
#http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-style-codes/ 

#they also provide column names from a screenshot of their Access db: http://pdata.hcad.org/DB/Pdata_Fieldnames.pdf
#and http://pdata.hcad.org/Desc/Layout_and_Length.txt

library(sf)
library(dplyr)

#start with loading files we need from HCAD
HCAD_parcels <- st_read("HCAD/2015/Parcels/Parcels.shp",stringsAsFactors = FALSE)
HCAD_res_build <- read.csv2("HCAD/2015/Real_building_land/building_res.txt", 
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_real_build <- read.csv2("HCAD/2015/Real_building_land/building_other.txt", 
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
#there are no account numbers in common between res_build and real_build, but some codes are in both
HCAD_real_acct <- read.csv2("HCAD/2015/Real_acct_owner/real_acct.txt", 
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_exempt <- read.csv2("HCAD/2015/Real_jur_exempt/jur_exempt_cd.txt", 
        stringsAsFactors = FALSE,
        sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
HCAD_owner_date <- read.csv2("HCAD/2015/Real_acct_owner/deeds.txt", 
                        sep = "\t", header = FALSE, quote="",
                        colClasses = c("V1"="character","V2"="character"))
HCAD_fixtures <- read.csv2("HCAD/2015/Real_building_land/fixtures.txt", 
                           stringsAsFactors = FALSE,
                           sep = "\t", header = FALSE, quote="",
                           colClasses = c("V1"="character"))

#clean up, rename and join
HCAD_parcels <- HCAD_parcels %>% 
  rename(account=HCAD_NUM) %>%
  select(account,LocAddr,city,zip,geometry)
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
#do a full_join, then select out later - depending on other how well other filtering works
HCAD <- full_join(HCAD_parcels,HCAD_res_build,by='account')

#using actual_area for total_bldg_sf - get units from fixtures
#total_income may include non-rental income?
HCAD_real_build$V1 <- str_remove_all(HCAD_real_build$V1, " ")
HCAD_real_build <- HCAD_real_build %>% 
  rename(account=V1,improv_typ_real=V4,
        bldg_value_real=V9,qual_descript=V14,date_built=V15,
        yr_remodel=V16,bldg_sf=V27,nbhd_factor=V28,bldg_name=V32,units=V33,
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

#for bldg_data codes: http://pdata.hcad.org/Desc/2015/code_desc_real.txt
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
            as.integer(HCAD_fixtures$num_apts*.89)), 
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

#perhaps still need to add logic for: (some come from improv_typ.x, some from improv_typ.y - res vs. real)
fourplex4 <- HCAD %>% filter(improv_typ.x == 1004) #looks like each account has it listed as 8 bedrooms, instead of as 4 apts.
#1002 = duplex, 1003 = triplex, with same problems.

#Not sure how to deal with:
#4670 - Correctional; 4313 = Dormitory; 4320 - Extended Stay; 4318 - Boarding House
#4213 = Mobile Home Parks (in real) - 1008 = Mobile Homes (in res) has rooms listed, so works
#it's possible that we could just know where they are and do some rough and ready by sf????

