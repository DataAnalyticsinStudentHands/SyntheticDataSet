# part of process run inside sam_mongolite
# March 10, 2019 accessed from: 2015-16 / https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015

library(rio)
library(dplyr)
library(FactoMineR)
source("tools.R")


nhanesdir = "~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/NHANES/" #Dan at home
#nhanesdir = "~/Downloads/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/NHANES/" #Dan at work
nhanesyear = "2012/" #or 2016/
file_path <- paste0(censusdir, vintage, "/downloaded/", state, "_", county, "_", groupname, ".csv")

NH_file_folder <- paste0(nhanesdir,nhanesyear)
#https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Demographics&CycleBeginYear=2015
#these are for 2015-2016, in folder: 2016.
fileNames <- c("DEMO_I.XPT", #demographics  https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm download on March 10, 2019
               "DR1TOT_I.XPT", #diet_nutrient1: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.htm
               "MCQ_I.XPT", #medical conditions: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.htm download on 4/30/19
               "DPQ_I.XPT", #depression instrument: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DPQ_I.htm download on 4/30/1
               "BPQ_I.XPT", #blood_pressure: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPQ_I.htm
               "DIQ_I.XPT", #diabetes: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.htm
               "HIQ_I.XPT", #insurance https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HIQ_I.htm
               "HUQ_I.XPT", #healthcare utilization: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HUQ_I.htm
               "CBQ_I.XPT", #consumer: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/CBQ_I.htm
               "DBQ_I.XPT", #dietary behavior: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DBQ_I.htm
               "OCQ_I.XPT", #occupation: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/OCQ_I.htm
               "HSQ_I.XPT", #current health: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HSQ_I.htm
               "BMX_I.XPT", #Body measures: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm
               "PAQ_I.XPT", #phys_act: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.htm
               "PFQ_I.XPT") #phys_func: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PFQ_I.htm

#not included
#"DR2TOT_I.XPT" #diet_nutrient2: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2IFF_I.htm
#"CDQ_I.XPT" #cardio: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/CDQ_I.htm

#2012 downloaded Oct 11, 2019
fileNames <- c("DEMO_G.XPT", #demographics  Oct.11, 2019
               "DR1TOT_G.XPT", #diet_nutrient1: 
               "MCQ_G.XPT", #medical conditions:
               "DPQ_G.XPT", #depression instrument:
               "CFQ_G.XPT", #cognitive function
               "SLQ_G.XPT", #sleep
               "DIQ_G.XPT", #diabetes: 
               "HOQ_G.XPT", #housing 
               "HUQ_G.XPT", #healthcare utilization: 
               "CBQ_G.XPT", #consumer: 
               "DBQ_G.XPT", #dietary behavior: 
               "OCQ_G.XPT", #occupation: 
               "HSQ_G.XPT", #current health: 
               "BMX_G.XPT", #Body measures: 
               "PAQ_G.XPT", #phys_act: 
               "PFQ_G.XPT"#phys_func:
               )  

fileList=lapply(fileNames, function(x){paste0(NH_file_folder,x)})
dataList = lapply(fileList, function(x){import(x)})
merged_NHANES_F <- Reduce(function(x,y) {merge(x,y, by="SEQN",all = TRUE)}, dataList)


#see NHANES_2015.json for brief descriptions - didn't finish putting those in, or getting all the codes / types
NHANES_merged <- merged_NHANES_F %>% rename(gender=RIAGENDR, #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm
                                       educational_attainment=DMDEDUC2,
                                       educational_level_child=DMDEDUC3,
                                       size=DMDFMSIZ, #family size but to match for PCA with sam
                                       age=RIDAGEYR,
                                       marital_status=DMDMARTL,
                                       yrs_US=DMDYRSUS,
                                       vet_foreign_war=DMQADFC,
                                       veteran=DMQMILIZ,
                                       birth_country=DMDHRBR4,
                                       poverty_ratio=INDFMPIR,
                                       pregnant=RIDEXPRG,
                                       race=RIDRETH3,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.htm
                                       age_first_asthma=MCQ025,
                                       ever_told_asthma=MCQ010,
                                       still_have_asthma=MCQ035,
                                       asthma_attack_1yr=MCQ040,
                                       ER_asthma_1yr=MCQ050,
                                       ever_told_COPD=MCQ160O,
                                       ever_told_bronchitis=MCQ160K,
                                       still_have_bronchitis=MCQ170K,
                                       age_first_bronchitis=MCQ180K,
                                       ever_told_emphysema=MCQ160G,
                                       age_first_emphysema=MCQ180G,
                                       ever_told_overweight=MCQ080,
                                       hay_fever_1yr=AGQ030,
                                       age_arthritis=MCQ180A,
                                       age_coronary_heart_disease=MCQ180C,
                                       age_heart_attack=MCQ180E,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DPQ_I.htm#Codebook
                                       no_interest_do_things=DPQ010, #1-3 counts as some problems
                                       depressed=DPQ020,
                                       trouble_sleep=DPQ030,
                                       no_energy=DPQ040,
                                       poor_eating=DPQ050,
                                       feel_bad_self=DPQ060,
                                       better_dead=DPQ090,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPQ_I.htm
                                       BP_dr_said=BPQ020,
                                       age_hypertension=BPD035,
                                       prescribed_BP=BPQ040A,
                                       taking_prescribed_BP=BPQ050A,
                                       prescribed_cholest=BPQ090D,
                                       taking_prescribed_cholest=BPQ100D,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.htm
                                       age_diabetes=DID040,
                                       told_prediabetes=DIQ160,
                                       feel_risk_diabetes=DIQ172,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HIQ_I.htm
                                       health_insurance=HIQ011,
                                       medicare=HIQ031B,
                                       medicaid=HIQ031D,
                                       no_insurance_1yr=HIQ210,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HUQ_I.htm
                                       gen_health=HUQ010,
                                       gen_health_v1yr=HUQ020,
                                       where_healthcare=HUQ041,
                                       how_many_healthcare=HUQ051,
                                       times_overnight_hosp=HUD080,
                                       mental_last_year=HUQ090,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/CBQ_I.htm
                                       money_supermarket=CBD071,
                                       money_non_food=CBD091,
                                       eating_out=CBD121,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.htm#PAD615
                                       minutes_vigorous_work=PAD615,
                                       minutes_moderate_work=PAD630,
                                       walk_bike_work=PAD645,
                                       minutes_vigorous_rec=PAD660,
                                       minutes_moderate_rec=PAD675,
                                       minutes_sedentary=PAD680,
                                       hours_TV=PAQ710,
                                       hours_computer=PAQ715,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PFQ_I.htm
                                       #the missing data corresponds to age groups skip patterns
                                       phys_limit_child=PFQ020,
                                       phys_limit_child_1yr=PFQ030,
                                       special_ed=PFQ041,
                                       health_prevent_work=PFQ049,
                                       health_limit_work=PFQ051,
                                       confusion=PFQ057,
                                       health_chronic_1=PFQ063A,
                                       health_chronic_2=PFQ063B,
                                       health_chronic_3=PFQ063C,
                                       health_chronic_4=PFQ063D,
                                       health_chronic_5=PFQ063E,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HSQ_I.htm
                                       Cold_30days=HSQ500,
                                       GI_30days=HSQ510,
                                       flu_30days=HSQ520,
                                       donated_blood=HSQ571,
                                       had_HIV_test=HSQ590,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm#BMDBMIC
                                       body_weight=BMXWT,
                                       standing_height=BMXHT,
                                       BMI=BMXBMI,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/OCQ_I.htm
                                       hrs_work_wk=OCQ180,
                                       work_situation=OCQ260,
                                       work_35hrs=OCQ210,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DBQ_I.htm
                                       how_healthy_diet=DBQ700,
                                       Free_meals_sr=DBQ301,
                                       school_lunch_cost=DBQ390,
                                       summer_lunch=DBQ424,
                                       #https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.htm
                                       d1_calories=DR1TKCAL,
                                       d1_protein=DR1TPROT,
                                       d1_carb=DR1TCARB,
                                       d1_sugar=DR1TSUGR,
                                       d1_fiber=DR1TFIBE,
                                       d1_fat=DR1TTFAT,
                                       d1_sat_fat=DR1TSFAT,
                                       d1_cholesterol=DR1TCHOL,
                                       d1_vit_E=DR1TATOC,
                                       d1_supp_E=DR1TATOA,
                                       d1_retinol=DR1TRET,
                                       d1_vit_A=DR1TVARA,
                                       d1_alpha_carotene=DR1TACAR,
                                       d1_beta_carotene=DR1TBCAR,
                                       d1_lycopene=DR1TLYCO,
                                       d1_b1_thiamine=DR1TVB1,
                                       d1_b2_riboflavin=DR1TVB2,
                                       d1_niacin=DR1TNIAC,
                                       d1_vit_b6=DR1TVB6,
                                       d1_total_folate=DR1TFOLA,
                                       d1_b12=DR1TVB12,
                                       d1_supp_b12=DR1TB12A,
                                       d1_vit_C=DR1TVC,
                                       d1_vit_D=DR1TVD,
                                       d1_vit_K=DR1TVK,
                                       d1_calcium=DR1TCALC,
                                       d1_phosphorus=DR1TPHOS,
                                       d1_magnesium=DR1TMAGN,
                                       d1_iron=DR1TIRON,
                                       d1_zinc=DR1TZINC,
                                       d1_copper=DR1TCOPP,
                                       d1_sodium=DR1TSODI,
                                       d1_potassium=DR1TPOTA,
                                       d1_selenium=DR1TSELE,
                                       d1_caffeine=DR1TCAFF,
                                       d1_alcohol=DR1TALCO) %>%
    select(SEQN,gender,educational_attainment,educational_level_child,
           size,age,marital_status,yrs_US,vet_foreign_war,veteran,birth_country,
           poverty_ratio,pregnant,race,age_first_asthma,ever_told_asthma,still_have_asthma,asthma_attack_1yr,
           ever_told_COPD,ever_told_bronchitis,still_have_bronchitis,age_first_bronchitis,ever_told_emphysema,
           age_first_emphysema,ever_told_overweight,hay_fever_1yr,
           ER_asthma_1yr,age_arthritis,age_coronary_heart_disease,age_heart_attack,no_interest_do_things,
           depressed,trouble_sleep,no_energy,poor_eating,feel_bad_self,better_dead,BP_dr_said,age_hypertension,
           prescribed_BP,taking_prescribed_BP,prescribed_cholest,taking_prescribed_cholest,age_diabetes,
           told_prediabetes,feel_risk_diabetes,health_insurance,medicare,medicaid,no_insurance_1yr,
           gen_health,gen_health_v1yr,where_healthcare,how_many_healthcare,times_overnight_hosp,mental_last_year,
           money_supermarket,money_non_food,eating_out,minutes_vigorous_work,minutes_moderate_work,
           walk_bike_work,minutes_vigorous_rec,minutes_moderate_rec,minutes_sedentary,hours_TV,hours_computer,
           phys_limit_child,phys_limit_child_1yr,special_ed,health_prevent_work,health_limit_work,confusion,
           health_chronic_1,health_chronic_2,health_chronic_3,health_chronic_4,health_chronic_5,
           Cold_30days,GI_30days,flu_30days,donated_blood,had_HIV_test,body_weight,standing_height,BMI,
           hrs_work_wk,work_situation,work_35hrs,how_healthy_diet,Free_meals_sr,school_lunch_cost,summer_lunch,
           d1_calories,d1_protein,d1_carb,
           d1_sugar,d1_fiber,d1_fat,d1_sat_fat,d1_cholesterol,d1_vit_E,d1_supp_E,d1_retinol,d1_vit_A,d1_alpha_carotene,
           d1_beta_carotene,d1_lycopene,d1_b1_thiamine,d1_b2_riboflavin,d1_niacin,d1_vit_b6,d1_total_folate,
           d1_b12,d1_supp_b12,d1_vit_C,d1_vit_D,d1_vit_K,d1_calcium,d1_phosphorus,d1_magnesium,d1_iron,d1_zinc,
           d1_copper,d1_sodium,d1_potassium,d1_selenium,d1_caffeine,d1_alcohol)
NHANES_1 <- NHANES_merged #separated in case need to redo
#add 4 luck columns - I want them to be permanent, instead of newly generated by the js
set.seed(6743)
NHANES_1$luck1 <- sample(100, size = nrow(NHANES_1), replace = TRUE)
NHANES_1$luck2 <- sample(100, size = nrow(NHANES_1), replace = TRUE)
NHANES_1$luck3 <- sample(100, size = nrow(NHANES_1), replace = TRUE)
NHANES_1$luck4 <- sample(100, size = nrow(NHANES_1), replace = TRUE)

write_csv2(merged_NHANES_F,paste0(nhanesdir,"cogNH_more_12.csv"))# etc.
#preparation of NHANES for PCA
#make categorical variables in vectors of binary, which will be removed later...
#could do it where we didn't break out the categorical variables, and just did the PCA with supplemental values, but
#this should give us more dimensions for the predictive fit.
#gender
NHANES_1 <- cbind(NHANES_1, sapply(levels(as.factor(NHANES_1$gender)), function(x) as.integer(x == NHANES_1$gender)))
names(NHANES_1)[names(NHANES_1) == 1] <- 'male'
names(NHANES_1)[names(NHANES_1) == 2] <- 'female'
#race
for(fact in unique(NHANES_1$race)){
  NHANES_1['hispanic'] <- ifelse(NHANES_1$race == 1 | NHANES_1$race ==2, 1, 0);
  NHANES_1['white'] <- ifelse(NHANES_1$race == 3, 1, 0);
  NHANES_1['black'] <- ifelse(NHANES_1$race == 4, 1, 0);
  NHANES_1['asian'] <- ifelse(NHANES_1$race == 6, 1, 0);
  NHANES_1['multiracial'] <- ifelse(NHANES_1$race == 7, 1, 0);
}
#education level is numeric and meaningful as goes up; children are educational_level_child, but also ascends and for purposes of PCA matching, just 0 here.
for(row in NHANES_1){
  NHANES_1['educ_level'] <- ifelse(NHANES_1$educational_attainment<=5,NHANES_1$educational_attainment,0)
}

##preparation of SAM for PCA (needs to match column names in NHANES) - from tools.R
sam <- createSAMSample(samplesize = 100000)

#add pregnancy from Pregnancy_Autism.R thru line 50

#change sam to match - or to move upward, with 7 levels instead of 5 but in ascending level of educ.
#change to dplyr case_when and also do pregnancy here..
sam <- sam %>% mutate(educ_level = case_when(
  educational_attainment=="Less than 9th grade" ~ 1,
  educational_attainment=="9th to 12th grade, no diploma" ~ 2,
  educational_attainment=="High School Graduate" ~ 3,
  educational_attainment=="Some College, no degree" ~ 4,
  educational_attainment=="Associate's degree" ~ 5,
  educational_attainment=="Bachelor's Degree" ~ 6,
  educational_attainment=="Graduate or Professional Degree" ~ 7,
  TRUE ~ 0 
))

#gender
sam <- cbind(sam, sapply(levels(as.factor(sam$sex)), function(x) as.integer(x == sam$sex)))
sam["male"] <- ifelse(sam$sex == "Male", 1, 0)
sam["female"] <- ifelse(sam$sex == "Female", 1, 0)
#race
sam <- cbind(sam, sapply(levels(as.factor(sam$race)), function(x) as.integer(x == sam$race)))


#calculate approximate poverty_ratio for sam - https://aspe.hhs.gov/poverty-guidelines
sam$poverty_ratio <- round(sam$household_income / (8000 + (sam$size*4500) ), digits = 3)

#run PCA from FactoMineR

#sam and NHANES_1 only have to have matching column names if pca_predict, but now doing
#pca on both, and the column names don't have to match - although they do, still
#4=(family_)size,5=age,110=male,111=female,112-6 are race (hispanic,white,black,asian,multiracial),117=educ_level,11=poverty_ratio
res.pcaNH <- PCA(NHANES_1[,c('size','age','male','female','hispanic','white','black','asian','multiracial','educ_level','poverty_ratio')],scale.unit=TRUE, ncp=5)
res.pcaSam <- PCA(sam[,c('size','age','male','female','hispanic','white','black','asian','multiracial','educ_level','poverty_ratio')],scale.unit=TRUE, ncp=5)

mod <- res.pcaNH$ind$coord[,1:5] #whole thing, but only first 5 eigen dimensions
targ <- res.pcaSam$ind$coord[,1:5]
norm_mod <- (res.pcaNH$eig[1:5,2]/100) * mod #multiply each dimension in mod and targ by the percent var explained??
norm_targ <- (res.pcaSam$eig[1:5,2]/100) * targ

sam <- cbind(sam,norm_targ)
NH <- cbind(NHANES_1,norm_mod)
NH_Male <- NH %>% filter(gender=='1')
NH_Female <- NH %>% filter(gender=='1')
NH_Pregnant <- NH %>% filter(pregnant=='1')
#others to do??

#quick little way to test for whether it's finding plausible minimums NOT RUN
min_test <- sample(order(abs(norm_mod[1,1] - norm_targ[,1])+abs(norm_mod[1,2] - norm_targ[,2])
                  +abs(norm_mod[1,3] - norm_targ[,3]) +abs(norm_mod[1,4] - norm_targ[,4])
                  +abs(norm_mod[1,5] - norm_targ[,5]))[1:10],1)


#if I split up case_when like this, should I change what's in the PCA? Going with full PCA now.
system.time({
      sam_matched <- sam %>%
        mutate(SEQN = 
          case_when(sex == 'Female' ~ as.numeric(
            NH_Female[mapply(function(x,y,z,a,b) 
              sample(order(abs(x - NH_Female[,'Dim.1'])+abs(y - NH_Female[,'Dim.2'])+abs(z - NH_Female[,'Dim.3'])
                       +abs(a - NH_Female[,'Dim.4'])+abs(b - NH_Female[,'Dim.5']))[1:5],1),
            Dim.1,Dim.2,Dim.3,Dim.4,Dim.5),'SEQN']),
          pregnant == '1' ~ as.numeric(
            NH_Pregnant[mapply(function(x,y,z,a,b) 
              sample(order(abs(x - NH_Pregnant[,'Dim.1'])+abs(y - NH_Pregnant[,'Dim.2'])+abs(z - NH_Pregnant[,'Dim.3'])
                           +abs(a - NH_Pregnant[,'Dim.4'])+abs(b - NH_Pregnant[,'Dim.5']))[1:5],1),
              Dim.1,Dim.2,Dim.3,Dim.4,Dim.5),'SEQN']),
          TRUE ~ as.numeric(
            NH_Male[mapply(function(x,y,z,a,b) 
              sample(order(abs(x - NH_Male[,'Dim.1'])+abs(y - NH_Male[,'Dim.2'])+abs(z - NH_Male[,'Dim.3'])
                           +abs(a - NH_Male[,'Dim.4'])+abs(b - NH_Male[,'Dim.5']))[1:5],1),
              Dim.1,Dim.2,Dim.3,Dim.4,Dim.5),'SEQN'])
          )
        )
})

sam_NH <- inner_join(sam_matched,NHANES_1,by = 'SEQN', suffix = c('_sam','_NH'))

samplesam_NH <- sample_n(sam_NH, 100000)

SAMDataFolder <- "NewSAMData/"

saveRDS(samplesam_NH,paste(SAMDataFolder,"/temp/sam_NH_7_9_100k.RDS",sep=""))
saveRDS(sam_NH,paste(SAMDataFolder,"/temp/sam_NH_7_9_4m.RDS",sep=""))

