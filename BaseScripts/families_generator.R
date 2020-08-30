#https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf

#' createIndividuals
#'
#' This function simulates individual people based on census data (age, sex, race, as of April 1, vintage year).
#' If simulated data already exists, it will read it from an RDS file.
#'Input should include exp_census from expand_from_census.R, which is a list of data.tables
#' @return sam_residents A dataframe of simulated people.
createFamilies <- function() {
  #have to fix
  sam_families_data_file <- paste0(censusdir, vintage,"/sam_hh.RDS") 
  #Create or read in individual sam residents
  if(file.exists(sam_residents_data_file)) {
    # import saved sam residents from RDS file
    sam_residents <- readRDS(sam_residents_data_file)
    print(sprintf("Done reading sam residents RDS from %s", sam_residents_data_file ))
  } else {
    
    #get relations and start matching... preferring numbers from individual totals when necessary
    
    
    #should remember kids_grand_age, too
    #we have household type by kids, household type by seniors and household type by whole population, - can also get family_type from sam_hh by age, but this may be something to write back over... 
    #    hh_type_kids #very close to sex_age_race <18yo, but have to add after sam_hh is added to say who has or doesn't have children!!
    #on sam_hh already kids_ages_dt #5 age range, family_type - F/M householder or married couple - only own_kids - needs related kids to find
    
    
    
    
    
    hh_relations_race <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/hh_relations_race_2020-08-30.RDS")
    hh_relations_eth <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/hh_relations_eth_2020-08-30.RDS")
    sam_pw_hh_race <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/sam_pw_hh_race_2020-07-26.RDS")
    sam_pw_hh_eth <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/sam_pw_hh_eth_2020-07-26.RDS")

#at some point pick these up - assuming they are after partner_workers, they have missing written on them for further matching....        
    #additional_workers_race <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/additional_workers_race_2020-07-25.RDS")
    #additional_workers_ethe <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/additional_workers_eth_2020-07-25.RDS")
    
    #cleaning up from colnames
    sam_pw_hh_eth[,grep("match_id",colnames(sam_pw_hh_eth)):=NULL]
    sam_pw_hh_race[,grep("match_id",colnames(sam_pw_hh_race)):=NULL]
    
    #first match Householder so it will get right pb etc. when expand on children
    #match on race/eth, age, sex [eth_sex_relations], in_family_type with family_role
    #still something weird around in_family_type=="In female householder no husband present family" having people who are married with spouse present
    hh_relations_eth[role_in_family=="Householder",("family_role"):=case_when(
      in_family_type=="In female householder no husband present family" ~ "Female householder no husband present",
      in_family_type=="In male householder no wife present family" ~ "Male householder no wife present",
      in_family_type=="In married-couple family" ~ "Married-couple family",
      TRUE ~ in_family_type
    )]
    hh_relations_eth[role_in_family=="Householder",("householder_age_9"):=case_when(
      eth_age_range=="15 to 17 years" ~ "Householder 15 to 24 years",
      eth_age_range=="18 to 19 years" ~ "Householder 15 to 24 years",
      eth_age_range=="20 to 24 years" ~ "Householder 15 to 24 years",
      eth_age_range=="25 to 29 years" ~ "Householder 25 to 34 years",
      eth_age_range=="30 to 34 years" ~ "Householder 25 to 34 years",
      eth_age_range=="35 to 44 years" ~ "Householder 35 to 44 years",
      eth_age_range=="45 to 54 years" ~ "Householder 45 to 54 years",
      eth_age_range=="55 to 64 years" ~ sample(c("Householder 55 to 59 years","Householder 60 to 64 years"),size=.N,replace = TRUE),
      eth_age_range=="65 to 74 years" ~ "Householder 65 to 74 years",
      eth_age_range=="75 to 84 years" ~ "Householder 75 to 84 years",
      eth_age_range=="85 to 94 years" ~ "Householder 85 years and over",
      TRUE ~ eth_age_range
    )]
    #if marital_status_5 fixed, then family_role captures what we need... eth_sex is 
    sam_pw_hh_eth[role_in_family=="Householder",("hh_match_id"):=
                    paste0(tract,ethnicity,family_role,householder_age_9,as.character(1000000+seq.int(1:.N))),
                  by=.(tract,ethnicity,family_role,householder_age_9)]
    hh_relations_eth[role_in_family=="Householder",("hh_match_id"):=
                    paste0(tract,ethnicity,family_role,householder_age_9,as.character(1000000+seq.int(1:.N))),
                  by=.(tract,ethnicity,family_role,householder_age_9)]
    
    
    #expand first set of kids
    sam_pw_hh_eth[,("kids_0_5"):=if_else(kids_by_age=="Under 6 years only" | kids_by_age=="Under 6 years and 6 to 17 years",
                                          1,0)]
    sam_pw_hh_eth[,("kids_6_17"):=if_else(kids_by_age=="6 to 17 years only" | kids_by_age=="Under 6 years and 6 to 17 years",
                                          1,0)]
    sam_pw_hh_race[,("kids_0_5"):=if_else(kids_by_age=="Under 6 years only" | kids_by_age=="Under 6 years and 6 to 17 years",
                                         1,0)]
    sam_pw_hh_race[,("kids_6_17"):=if_else(kids_by_age=="6 to 17 years only" | kids_by_age=="Under 6 years and 6 to 17 years",
                                          1,0)]
    #make kids, then add a .N on household_id, so we can subtract that from hh_size and see where we are
    #these kids are all related, and the relations stuff should let you break out for Grandchild, Child, and Adopted, etc.
    
    #should be able to match with a worker?
    sam_pw_hh_eth[role_in_family=="Householder" & !is.na(marital_status_non_hh_gp),
                  ("parent_of_hh"):=if_else(str_detect(marital_status_non_hh_gp,"Now"),2,1)]
    sam_pw_hh_race[role_in_family=="Householder" & !is.na(marital_status_non_hh_gp),
                  ("parent_of_hh"):=if_else(str_detect(marital_status_non_hh_gp,"Now"),2,1)]
    #make a match for family_roles that allows matching by age / worker, etc.?
    
    
    
    #from expand_fam
    
    #hh_income_dt
    #computer_internet_hh
    #internet_hh
    #educ_internet_hh
    #income_internet_hh
    #food_stamps_eth_dt
    #food_stamps_race_dt
    #
    #age_hh_monthly_costs_income_hh
    #age_internet_hh
    #
    #family_employment_dt
    #
    #mortgage stuff
    #percent_income_monthly_costs_hh
    #tenure_income_monthly_costs_hh
    #tenure_yr_moved_units_hh
    
    
    
    #add income - not a lot determined yet, so match on own_rent but ordered by people_per_room
    sam_eth_hh[order(-people_per_room),
               ("income_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    sam_race_hh[order(-people_per_room),
                ("income_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                by=.(tract,own_rent)]
    hh_income_dt[order(income_low),("income_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                 by=.(tract,own_rent)]
    sam_eth_hh[,c("hh_income_level","income_low","income_high") := 
                 hh_income_dt[.SD, c(list(hh_income_level),list(income_low),list(income_high)), on = .(income_occup_id)]]
    sam_race_hh[,c("hh_income_level","income_low","income_high") := 
                  hh_income_dt[.SD, c(list(hh_income_level),list(income_low),list(income_high)), on = .(income_occup_id)]]
    sam_eth_hh$income_occup_id <- NULL
    sam_race_hh$income_occup_id <- NULL
    #test hh15
    #test<-table(
    #  hh_income_dt$tract,
    #  hh_income_dt$hh_income_level,
    #  hh_income_dt$income_low,
    #  hh_income_dt$income_high
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_income_level,
    #  sam_race_hh$income_low,
    #  sam_race_hh$income_high
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_income_dt$tract,
    #  hh_income_dt$hh_income_level,
    #  hh_income_dt$income_low,
    #  hh_income_dt$income_high
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_income_level,
    #  sam_eth_hh$income_low,
    #  sam_eth_hh$income_high
    #)
    #length(test[test==F])==0
    
    #add hh_education - need to think through how it matches with individual education on sex_age_race
    #make same variable categories with sam_hh
    hh_educ_dt[str_detect(own_rent,"Owner"),("own_rent"):="Owner occupied"]
    hh_educ_dt[str_detect(own_rent,"Renter"),("own_rent"):="Renter occupied"]
    sam_eth_hh[order(income_low),
               ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    sam_race_hh[order(income_low),
                ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                by=.(tract,own_rent)]
    hh_educ_dt[order(match(hh_education_level,c("Less than high school graduate","High school graduate (including equivalency)",
                                                "Some college or associate's degree","Bachelor's degree or higher"))),
               ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    sam_eth_hh[,c("hh_education_level") := 
                 hh_educ_dt[.SD, list(hh_education_level), on = .(educ_occup_id)]]
    sam_race_hh[,c("hh_education_level") := 
                  hh_educ_dt[.SD, list(hh_education_level), on = .(educ_occup_id)]]
    sam_eth_hh$educ_occup_id <- NULL
    sam_race_hh$educ_occup_id <- NULL
    #test hh16
    #test<-table(
    #  hh_educ_dt$tract,
    #  hh_educ_dt$hh_education_level
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_education_level
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_educ_dt$tract,
    #  hh_educ_dt$hh_education_level
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_education_level
    #)
    #length(test[test==F])==0
    
    #add rent by income
    gross_rent_hh[rent_cash=="With cash rent",("gross_inc_id"):=
                    paste0(tract,gross_rent_high2,as.character(1000000+seq.int(1:.N))),by=.(tract,gross_rent_high2)]
    income_gross_rent_hh[,("gross_inc_id"):=
                           paste0(tract,gross_rent_high2,as.character(1000000+seq.int(1:.N))),by=.(tract,gross_rent_high2)]
    #add income to gross_rent, then to sam_hh own_rent by income?
    gross_rent_hh[,c("hh_income_renters") := 
                    income_gross_rent_hh[.SD, list(hh_income_renters), on = .(gross_inc_id)]]
    #clean up for the ones who pay no rent
    gross_rent_hh[is.na(hh_income_renters),("hh_income_renters"):="Less than $10 000"]
    gross_rent_hh[hh_income_renters=="Less than $10 000",("hh_income_renters"):="$1   000"]
    gross_rent_hh[,("income_low"):= as.numeric(substr(hh_income_renters,2,4))*1000]
    #now to sam_hh
    gross_rent_hh[order(income_low),("inc_id"):=
                    paste0(tract,"Renter occupied",as.character(1000000+seq.int(1:.N))),by=.(tract)]
    sam_eth_hh[order(income_low),("inc_id"):=
                 paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    sam_race_hh[order(income_low),("inc_id"):=
                  paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    #add income to gross_rent, then to sam_hh own_rent by income?
    #want to compare to see if it adds information, later, but may not be of use
    sam_eth_hh[,c("hh_income_renters") := 
                 gross_rent_hh[.SD, list(hh_income_renters), on = .(inc_id)]]
    sam_eth_hh$inc_id <- NULL
    sam_race_hh[,c("hh_income_renters") := 
                  gross_rent_hh[.SD, list(hh_income_renters), on = .(inc_id)]]
    sam_race_hh$inc_id <- NULL
    #test hh17
    #test<-table(
    #  gross_rent_hh$tract,
    #  gross_rent_hh$hh_income_renters
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_income_renters
    #)
    #length(test[test==F])==0
    #test<-table(
    #  gross_rent_hh$tract,
    #  gross_rent_hh$hh_income_renters
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_income_renters
    #)
    #length(test[test==F])==0
    
    
    
    #SNAP stuff is problematic at a couple of levels - could try to combine with SSI, too?
    sam_eth_hh[order(people_per_room),
               ("SNAP_id"):=paste0(tract,ethnicity,as.character(1000000+seq.int(1:.N))),
               by=.(tract,ethnicity)]
    sam_race_hh[order(people_per_room),
                ("SNAP_id"):=paste0(tract,race,as.character(1000000+seq.int(1:.N))),
                by=.(tract,race)]
    food_stamps_eth_dt[order(food_stamps),
                       ("SNAP_id"):=paste0(tract,ethnicity,as.character(1000000+seq.int(1:.N))),
                       by=.(tract,ethnicity)]
    food_stamps_race_dt[order(food_stamps),
                        ("SNAP_id"):=paste0(tract,race,as.character(1000000+seq.int(1:.N))),
                        by=.(tract,race)]
    sam_eth_hh[,c("SNAP") := 
                 food_stamps_eth_dt[.SD, list(food_stamps), on = .(SNAP_id)]]
    sam_race_hh[,c("SNAP") := 
                  food_stamps_race_dt[.SD, list(food_stamps), on = .(SNAP_id)]]
    sam_eth_hh$SNAP_id <- NULL
    sam_race_hh$SNAP_id <- NULL
    #test hh19
    #test<-table(
    #  food_stamps_race_dt$tract,
    #  food_stamps_race_dt$race,
    #  food_stamps_race_dt$food_stamps
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$race,
    #  sam_race_hh$SNAP
    #)
    #length(test[test==F])==0
    #test<-table(
    #  food_stamps_eth_dt$tract,
    #  food_stamps_eth_dt$ethnicity,
    #  food_stamps_eth_dt$food_stamps
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$ethnicity,
    #  sam_eth_hh$SNAP
    #)
    #length(test[test==F])==0
    
    
    
    #make into a model based only on the characteristics that are known
    GQ_sam <- uncount(base_group_quarters_data,number_sams_temp,.remove = FALSE,.id="temp_id") %>%
      group_by(tract) %>%
      mutate( #approximated from natl BOP - https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
        GQ_facility_type := if_else(number_sams_temp > 400,"correctional","nursing home"),
        age := if_else(GQ_facility_type == "correctional",
                       sample(c(18:64), #c("18 to 19 years","20 to 24 years","25 to 29 years","30 to 34 years","35 to 44 years","45 to 54 years","55 to 64 years"),
                              replace=TRUE,size = n(),prob=c(rep(.025,2),rep(.018,5),rep(.032,5),rep(.05,5),rep(.024,10),rep(.014,10),rep(.007,10))),
                       sample(c(65:99), #c("65 to 75 years","75 to 85 years","85 years and over"),
                              replace = TRUE,size=n(),prob=c(rep(.01,10),rep(.04,10),rep(.01,15)))), # https://www.cdc.gov/nchs/nsltcp/index.htm
        sex_num := if_else(GQ_facility_type == "correctional",
                           sample(c(0,1), #c('male','female'),
                                  replace=TRUE,size = n(),prob=c(.93,.07)),
                           sample(c(0,1), #c('male','female'),
                                  replace=TRUE,size = n(),prob=c(.33,.67)),
        )
      )
    
    
    create_tract_eigs = function(dt,var_name,facts){ 
      for(i in unique(dt$tract)){
        print(i) #not  because it's not handling errors
        pca_res <- PCA(dt[tract==i,..facts],scale.unit=TRUE, ncp=3)  #have to decide if want only three - dim1 by tract is between 18 and 32 var
        dt[tract==i,paste0(var_name,"_eig_1") := pca_res$ind$coord[,1]]
        dt[tract==i,paste0(var_name,"_eig_2") := pca_res$ind$coord[,2]]
        dt[tract==i,paste0(var_name,"_eig_3") := pca_res$ind$coord[,3]]
        #  dt[tract==i,paste0(var_name,"_eig_4") := pca_res$ind$coord[,4]]
        #  dt[tract==i,paste0(var_name,"_eig_5") := pca_res$ind$coord[,5]]
        dt[tract==i,paste0(var_name,"_pve_1") := pca_res$eig[1,2] / 100] #percent variance explained / need to check on factoMineR 
        dt[tract==i,paste0(var_name,"_pve_2") := pca_res$eig[2,2] / 100]
      }
      return(dt)
    }
    sam_race_age_eigs <- create_tract_eigs(sam_marital_DT,"race_age",facts)  #facts <- c('age','sex_num','white','black','hispanic','asian','other_race','american_indian','pacific_islander','bi_racial')
    saveRDS(sam_race_age_eigs,"sam_race_age_eigs.RDS") 
    sam_race_age_eigs <- readRDS("sam_race_age_eigs.RDS")
    
    #calculate euclidean distance to center of each tract (which will be used to match in next step)
    euc_distances = function(dt,vname,facts){ 
      for(i in unique(dt$tract)){
        #uncount to right size with median
        number_sams_added <- sum(dt[tract==i & is.na(temp_id),.N],na.rm = TRUE) - sum(dt[tract==i & !is.na(temp_id),.N],na.rm = TRUE)
        filler_dt <- as.data.table(dt[tract==i & !is.na(temp_id)][1]) 
        if(nrow(filler_dt)>0){
          filler_dt[,"GQ_facility_type" := 'not in Group Quarters']
          center <- dt[tract==i & is.na(temp_id),c(paste0("race_age","_eig_1"),paste0("race_age","_eig_2"),paste0("race_age","_eig_3"))][which.min(abs(race_age_eig_1-median(race_age_eig_1))),]
          filler_dt[,c("race_age_eig_1","race_age_eig_2","race_age_eig_3") := center[,c("race_age_eig_1","race_age_eig_2","race_age_eig_3")]]
          added <- uncount(filler_dt,number_sams_added,.remove = FALSE,.id="temp_id")
          dt <- rbind(dt,added) #does it need to be added for the tract?
          target <- dt[tract==i,c(paste0("race_age","_eig_1"),paste0("race_age","_eig_2"),paste0("race_age","_eig_3"))]
          dt[tract==i,("euc_dist") := sqrt((target[,1]-center[,1][[1]])^2 + (target[,2]-center[,2][[1]])^2 + (target[,3]-center[,3][[1]])^2)]
          dt[tract==i,("prob_euc_dist") := (max(euc_dist) - euc_dist) / max(euc_dist)]
          dt[tract==i & !is.na(temp_id),("norm_prob_euc") := (1-prob_euc_dist / 1) / .N]
        }
      }
      return(dt)
    }
    sam_race_age_eigs_eucs_temp <- euc_distances(sam_race_age_eigs[tract=="410401"],"race_age",facts)
    sam_race_age_eigs_eucs <- euc_distances(sam_race_age_eigs,"race_age",facts)
    
    saveRDS(sam_race_age_eigs_eucs,"sam_race_age_eigs_eucs.RDS") 
    sam_race_age_eigs_eucs <- readRDS("sam_race_age_eigs_eucs.RDS")
    sam_race_age_eigs_eucs[norm_prob_euc==0,("norm_prob_euc") := .000000001]
    
    sample_by_euc = function(dt){ 
      for(i in unique(dt$tract)){
        if(nrow(dt[tract==i & !is.na(temp_id)])){
          #normalize the euc_dist on the !is.na(temp_id) to use as prob paste0(name,"_dist_prob")
          dt[tract==i & is.na(temp_id),("GQ_facility_type") := 
               sample(dt[tract==i & !is.na(temp_id),GQ_facility_type],size = .N,replace = FALSE,
                      prob = c(dt[tract==i & !is.na(temp_id),norm_prob_euc]))] 
        }
      }
      dt_out <- dt[is.na(temp_id)]
      return(dt_out)
    }
    
    sam_GQ <- sample_by_euc(sam_race_age_eigs_eucs)
    sam_GQ[is.na(GQ_facility_type),("GQ_facility_type") := "not in Group Quarters"]
    
    saveRDS(sam_GQ,"sam_GQ.RDS") 
    sam_GQ <- readRDS("sam_GQ.RDS")
    
  
    
    saveRDS(hh_relations_eth,file = paste0(housingdir, vintage, "/hh_relations_eth_",Sys.Date(),".RDS"))
    saveRDS(hh_relations_race,file = paste0(housingdir, vintage, "/hh_relations_race_",Sys.Date(),".RDS"))
  }
}

  

