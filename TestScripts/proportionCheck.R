# This function compares the proportions in the model to the proportions in the Census Data. Ideally they should be within 10% of each other

# The inputs are the model (sam), the Census Data (census), and the tract number (x)
prop_Check <- function(sam, census, x){

  # These are temporary data frames. part is for the model, and cen is for the Census Data
  part=data.frame(tract=x,stringsAsFactors = F)
  cen=data.frame(tract=x,stringsAsFactors = F)

  # mini is a subset of the model and census is a subset of the Census Data
  mini=sam[sam$tract==x,]
  census = census[census$tract== as.integer(x) & census$county == "201",]

# ******************************************** SAM CITY DATA ****************************************
# count the number of elements in each category

  #  household type
  part$group.quarters.population=nrow(mini[mini$household.type=="Group Quarters",])
  part$married.couple.families = nrow(mini[mini$member=="Husband",])
  part$male.householders.no.wife = nrow(mini[mini$member=="Male Householder",])
  part$female.householders.no.husband = nrow(mini[mini$member=="Female Householder",])
  part$nonfamily = nrow(mini[mini$household.type %in% c("Alone", "Non-family"),])

  # household member
  part$adult = nrow(mini[mini$member != "Child",])
  part$child = nrow(mini[mini$member == "Child",])

  # household size
  part$size1 = nrow(mini[mini$size == 1,])
  part$size2 = nrow(mini[mini$size == 2,])/2
  part$size3 = nrow(mini[mini$size == 3,])/3
  part$size4 = nrow(mini[mini$size == 4,])/4
  part$size5 = nrow(mini[mini$size == 5,])/5
  part$size6 = nrow(mini[mini$size == 6,])/6
  part$size7 = nrow(mini[mini$size == 7,])/7

  # num of cars
  part$car0 = nrow(mini[mini$number.of.vehicles == 0 & !is.na(mini$number.of.vehicles),])
  part$car1 = nrow(mini[mini$number.of.vehicles == 1 & !is.na(mini$number.of.vehicles),])
  part$car2 = nrow(mini[mini$number.of.vehicles == 2 & !is.na(mini$number.of.vehicles),])
  part$car3 = nrow(mini[mini$number.of.vehicles == 3 & !is.na(mini$number.of.vehicles),])
  part$car4 = nrow(mini[mini$number.of.vehicles == 4 & !is.na(mini$number.of.vehicles),])
  part$car5 = nrow(mini[mini$number.of.vehicles == 5 & !is.na(mini$number.of.vehicles),])

  # gender
  part$male = nrow(mini[mini$sex == "Male",])
  part$female = nrow(mini[mini$sex == "Female",])

  # age
  part$age4 = nrow(mini[mini$bracket.age == "0.to.4",])
  part$age9 = nrow(mini[mini$bracket.age == "5.to.9",])
  part$age14 = nrow(mini[mini$bracket.age == "10.to.14",])
  part$age17 = nrow(mini[mini$bracket.age == "15.to.17",])
  part$age19 = nrow(mini[mini$bracket.age == "18.to.19",])
  part$age24 = nrow(mini[mini$bracket.age == "20.to.24",])
  part$age29 = nrow(mini[mini$bracket.age == "25.to.29",])
  part$age34 = nrow(mini[mini$bracket.age == "30.to.34",])
  part$age44 = nrow(mini[mini$bracket.age == "35.to.44",])
  part$age54 = nrow(mini[mini$bracket.age == "45.to.54",])
  part$age64 = nrow(mini[mini$bracket.age == "55.to.64",])
  part$age74 = nrow(mini[mini$bracket.age == "65.to.74",])
  part$age84 = nrow(mini[mini$bracket.age == "75.to.84",])
  part$age100 = nrow(mini[mini$bracket.age == "85.to.100",])

  # race
  part$black = nrow(mini[mini$race == "black",])
  part$american.indian.or.alaskan = nrow(mini[mini$race == "american.indian.or.alaskan",])
  part$asian = nrow(mini[mini$race == "asian",])
  part$islander = nrow(mini[mini$race == "islander",])
  part$other.race = nrow(mini[mini$race == "other.race",])
  part$multiracial = nrow(mini[mini$race == "multiracial",])
  part$white = nrow(mini[mini$race == "white",])
  part$hispanic = nrow(mini[mini$race == "hispanic",])

  # school.enrollment
  part$privateSchool = nrow(mini[mini$school.enrollment == "Private School" & !is.na(mini$school.enrollment),])
  part$publicSchool = nrow(mini[mini$school.enrollment == "Public School" & !is.na(mini$school.enrollment),])
  part$noSchool = nrow(mini[mini$school.enrollment == "Not Enrolled in School" & !is.na(mini$school.enrollment),])

  # educational attainment
  part$noHighSchool = nrow(mini[mini$educational.attainment == "Less than 9th grade" & !is.na(mini$educational.attainment),])
  part$someHighSchool = nrow(mini[mini$educational.attainment == "9th to 12th grade, no diploma" &! is.na(mini$educational.attainment),])
  part$highSchool = nrow(mini[mini$educational.attainment == "High School Graduate" & !is.na(mini$educational.attainment),])
  part$someCollege = nrow(mini[mini$educational.attainment == "Some College, no degree" & !is.na(mini$educational.attainment),])
  part$associates = nrow(mini[mini$educational.attainment == "Associate's degree" & !is.na(mini$educational.attainment),])
  part$bachelors = nrow(mini[mini$educational.attainment == "Bachelor's Degree" & !is.na(mini$educational.attainment),])
  part$phd = nrow(mini[mini$educational.attainment == "Graduate or Professional Degree" & !is.na(mini$educational.attainment),])

  # employment
  part$army = nrow(mini[mini$employment == "In Armed Forces" & !is.na(mini$employment),])
  part$employed = nrow(mini[mini$employment == "Employed" & !is.na(mini$employment),])
  part$unemployed = nrow(mini[mini$employment == "Unemployed" & !is.na(mini$employment),])
  part$noLabor = nrow(mini[mini$employment == "Not in labor force" & !is.na(mini$employment),])

  # disability
  part$disability1 = nrow(mini[mini$disability == "With One Type of Disability",])
  part$disability2 = nrow(mini[mini$disability == "With Two or More Types of Disabilities",])
  part$noDisability = nrow(mini[mini$disability == "No Disabilities",])

  # native
  part$native = nrow(mini[mini$nativity == "native" & !is.na(mini$nativity),])
  part$foreign = nrow(mini[mini$nativity == "foreign" & !is.na(mini$nativity),])

  # english
  part$onlyEnglish = nrow(mini[mini$English.speaking.skills == "only.english" & !is.na(mini$English.speaking.skills),])
  part$englishWell = nrow(mini[mini$English.speaking.skills == "english.well" & !is.na(mini$English.speaking.skills),])
  part$englishBad = nrow(mini[mini$English.speaking.skills == "english.bad" & !is.na(mini$English.speaking.skills),])

  # citizen
  part$citizen = nrow(mini[mini$citizenship == "Citizen" & !is.na(mini$citizenship),])
  part$naturalized = nrow(mini[mini$citizenship == "Naturalized Citizen" & !is.na(mini$citizenship),])
  part$notCitizen = nrow(mini[mini$citizenship == "Not a U.S. Citizen" & !is.na(mini$citizenship),])

  # Home language
  part$english = nrow(mini[mini$Language.at.home == "English" & !is.na(mini$Language.at.home),])
  part$spanish = nrow(mini[mini$Language.at.home == "Speaks Spanish" & !is.na(mini$Language.at.home),])
  part$other = nrow(mini[mini$Language.at.home == "Speaks Other Languages" & !is.na(mini$Language.at.home),])

  # veteran
  part$veteran = nrow(mini[mini$veteran.status == "Veteran",])
  part$nonveteran = nrow(mini[mini$veteran.status == "Nonveteran",])

  # transportation
  part$drive = nrow(mini[mini$means.of.transportation.to.work == "drove alone" & !is.na(mini$means.of.transportation.to.work),])
  part$carpooled = nrow(mini[mini$means.of.transportation.to.work == "carpooled" & !is.na(mini$means.of.transportation.to.work),])
  part$publicTransport = nrow(mini[mini$means.of.transportation.to.work == "public transportation" & !is.na(mini$means.of.transportation.to.work),])
  part$bike = nrow(mini[mini$means.of.transportation.to.work == "bicycle" & !is.na(mini$means.of.transportation.to.work),])
  part$walk = nrow(mini[mini$means.of.transportation.to.work == "walked" & !is.na(mini$means.of.transportation.to.work),])
  part$otherTransport = nrow(mini[mini$means.of.transportation.to.work == "other" & !is.na(mini$means.of.transportation.to.work),])
  part$workedHome = nrow(mini[mini$means.of.transportation.to.work == "worked at home" & !is.na(mini$means.of.transportation.to.work),])

  # travel time
  part$time10 = nrow(mini[mini$bracket.travel.time.to.work == "1 to 10 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time14 = nrow(mini[mini$bracket.travel.time.to.work == "10 to 14 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time19 = nrow(mini[mini$bracket.travel.time.to.work == "15 to 19 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time24 = nrow(mini[mini$bracket.travel.time.to.work == "20 to 24 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time29 = nrow(mini[mini$bracket.travel.time.to.work == "25 to 29 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time34 = nrow(mini[mini$bracket.travel.time.to.work == "30 to 34 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time44 = nrow(mini[mini$bracket.travel.time.to.work == "35 to 44 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time59 = nrow(mini[mini$bracket.travel.time.to.work == "45 to 59 minutes" & !is.na(mini$bracket.travel.time.to.work),])
  part$time100 = nrow(mini[mini$bracket.travel.time.to.work == "60 to 100 minutes" & !is.na(mini$bracket.travel.time.to.work),])

  # income
  part$income.0.9999 = nrow(mini[mini$household.income < 10000,]) + nrow(mini[mini$household.type != "Group Quarters" & mini$age < 16,])
  part$income.10000.14999 = nrow(mini[mini$household.income > 9999 & mini$household.income < 15000,])
  part$income.15000.24999 = nrow(mini[mini$household.income > 14999 & mini$household.income < 25000,])
  part$income.25000.34999 = nrow(mini[mini$household.income > 24999 & mini$household.income < 35000,])
  part$income.35000.49999 = nrow(mini[mini$household.income > 34999 & mini$household.income < 50000,])
  part$income.50000.74999 = nrow(mini[mini$household.income > 49999 & mini$household.income < 75000,])
  part$income.75000.99999 = nrow(mini[mini$household.income > 74999 & mini$household.income < 100000,])
  part$income.100000.124999 = nrow(mini[mini$bracket.household.income == "income.100000.124999",])
  part$income.125000.149999 = nrow(mini[mini$bracket.household.income == "income.125000.149999",])
  part$income.150000.199999 = nrow(mini[mini$bracket.household.income == "income.150000.199999",])
  part$income.200000.500000 = nrow(mini[mini$bracket.household.income == "income.200000.500000",])

  # insurance
  part$privateInsurance = nrow(mini[mini$health.insurance == "private insurance",])
  part$publicInsurance = nrow(mini[mini$health.insurance == "public insurance",])
  part$noInsurance = nrow(mini[mini$health.insurance == "no insurance",])

# ********************************************* CENSUS DATA *********************************************
# count the number of elements in each category

  #  household type
  cen$group.quarters.population = census$group.quarters.population
  cen$married.couple.families = census$married.couple.families
  cen$male.householders.no.wife = census$male.householders.no.wife
  cen$female.householders.no.husband = census$female.householders.no.husband
  cen$nonfamily = sum(census[startsWith(names(census), "nonfamily")])

  # household member
  cen$adult = sum(census[c(45:54, 59:68, 73:82, 87:96, 101:110, 115:124, 129:138, 143:152, 157:166, 171:180, 185:194, 199:208, 213:222, 227:236, 241:250, 255:264)])
  cen$child = sum(census[c(41:44, 55:58, 69:72, 83:86, 97:100, 111:114, 125:128, 139:142, 153:156, 167:170, 181:184, 195:198, 209:212, 223:226, 237:240, 251:254)])

  # household size
  cen$size1 = sum(census[c(4,11)])
  cen$size2 = sum(census[c(5,12)])
  cen$size3 = sum(census[c(6,13)])
  cen$size4 = sum(census[c(7,14)])
  cen$size5 = sum(census[c(8,15)])
  cen$size6 = sum(census[c(9,16)])
  cen$size7 = sum(census[c(10,17)])

  # num of cars
  cen$car0 = sum(census[endsWith(names(census), "no.vehicle")]) + sum(census[endsWith(names(census), "0cars")])
  cen$car1 = sum(census[endsWith(names(census), "1.vehicle")]) + sum(census[endsWith(names(census), "1car")])
  cen$car2 = sum(census[endsWith(names(census), "2.vehicles")]) + sum(census[endsWith(names(census), "2cars")])
  cen$car3 = sum(census[endsWith(names(census), "3.vehicles")]) + sum(census[endsWith(names(census), "3cars")])
  cen$car4 = sum(census[endsWith(names(census), "4.vehicles")]) + sum(census[endsWith(names(census), "5cars")])
  cen$car5 = sum(census[endsWith(names(census), "5cars")])

  # gender
  cen$male = sum(census[c(41:54, 69:82, 97:110, 125:138, 153:166, 181:194, 209:222, 237:250)])
  cen$female = sum(census[c(55:68, 83:96, 111:124, 139:152, 167:180, 195:208, 223:236, 251:264)])

  # age
  cen$age4 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 41])
  cen$age9 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 42])
  cen$age14 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 43])
  cen$age17 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 44])
  cen$age19 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 45])
  cen$age24 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 46])
  cen$age29 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 47])
  cen$age34 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 48])
  cen$age44 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 49])
  cen$age54 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 50])
  cen$age64 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 51])
  cen$age74 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 52])
  cen$age84 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 53])
  cen$age100 = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*14 + 54])

  # race
  cen$black = sum(census[startsWith(names(census), "black")])
  cen$american.indian.or.alaskan = sum(census[startsWith(names(census), "american")])
  cen$asian = sum(census[startsWith(names(census), "asian")])
  cen$islander = sum(census[startsWith(names(census), "islander")])
  cen$other.race = sum(census[startsWith(names(census), "other.race")])
  cen$multiracial = sum(census[startsWith(names(census), "multiracial")])
  cen$white = sum(census[startsWith(names(census), "white")])
  cen$hispanic = sum(census[startsWith(names(census), "hispanic")])

  # school.enrollment
  cen$privateSchool = sum(census[startsWith(names(census), "private.school")])
  cen$publicSchool = sum(census[startsWith(names(census), "public.school")])
  cen$noSchool = sum(census[startsWith(names(census), "no.school")])

  # educational attainment
  cen$noHighSchool = sum(census[startsWith(names(census), "less.than.9.grade")])
  cen$someHighSchool = sum(census[startsWith(names(census), "btwn.9")])
  cen$highSchool = sum(census[startsWith(names(census), "high.school")])
  cen$someCollege = sum(census[startsWith(names(census), "some.college")])
  cen$associates = sum(census[startsWith(names(census), "associates")])
  cen$bachelors = sum(census[startsWith(names(census), "bachelors")])
  cen$phd = sum(census[startsWith(names(census), "post.grad")])

  # employment
  cen$army = sum(census[startsWith(names(census), "in.armed.forces")])
  cen$employed = sum(census[startsWith(names(census), "employed")])
  cen$unemployed = sum(census[startsWith(names(census), "unemployed")])
  cen$noLabor = sum(census[startsWith(names(census), "not.in.labor.forces")])

  # disability
  cen$disability1 = sum(census[endsWith(names(census), "1.disability")])
  cen$disability2 = sum(census[endsWith(names(census), "2.disability")])
  cen$noDisability = sum(census[endsWith(names(census), "no.disability")])

  # native
  cen$native = sum(census[c(484:486, 490:492, 496:498, 502:504, 508:510, 514:516, 520:522, 526:528)])
  cen$foreign = sum(census[c(487:489, 493:495, 499:501, 505:507, 511:513, 517:519, 523:525, 529:531)])

  # english
  cen$onlyEnglish = sum(census[endsWith(names(census), "only.english")])
  cen$englishWell = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*3 + 485])
  cen$englishBad = sum(census[c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)*3 + 486])

  # citizen
  cen$citizen = cen$native
  cen$naturalized = sum(census[c(540, 542:543, 546:547, 550, 552:553, 556:557)])
  cen$notCitizen = sum(census[c(541, 544:545, 548:549, 551, 554:555, 558:559)])

  # Home language
  cen$english = cen$onlyEnglish
  cen$spanish = sum(census[startsWith(names(census), "spanish")])
  cen$other = sum(census[startsWith(names(census), "other.lang")])

  # veteran
  cen$veteran = sum(census[startsWith(names(census), "veteran")])
  cen$nonveteran = sum(census[startsWith(names(census), "nonveteran")])

  # transportation
  cen$drive = sum(census[endsWith(names(census), "drove.alone")])
  cen$carpooled = sum(census[endsWith(names(census), "carpooled")])
  cen$publicTransport = sum(census[endsWith(names(census), "public.transport")])
  cen$bike = sum(census[endsWith(names(census), "bike")])
  cen$walk = sum(census[endsWith(names(census), "walk")])
  cen$otherTransport = sum(census[endsWith(names(census), "other.transport")])
  cen$workedHome = sum(census[endsWith(names(census), "work.at.home")])

  # travel time
  cen$time10 = sum(census[endsWith(names(census), "10.minutes")])
  cen$time14 = sum(census[endsWith(names(census), "14.minutes")])
  cen$time19 = sum(census[endsWith(names(census), "19.minutes")])
  cen$time24 = sum(census[endsWith(names(census), "24.minutes")])
  cen$time29 = sum(census[endsWith(names(census), "29.minutes")])
  cen$time34 = sum(census[endsWith(names(census), "34.minutes")])
  cen$time44 = sum(census[endsWith(names(census), "44.minutes")])
  cen$time59 = sum(census[endsWith(names(census), "59.minutes")])
  cen$time100 = sum(census[endsWith(names(census), "over60.minutes")])

  # income
  cen$income.0.9999 = census$income.0.9999 + census$individual.1.9999 + census$individual.0.0
  cen$income.10000.14999 = census$income.10000.14999 +  census$individual.10000.14999
  cen$income.15000.24999 = census$income.15000.19999 + census$income.20000.24999 + census$individual.15000.24999
  cen$income.25000.34999 = census$income.25000.29999 + census$income.30000.34999 + census$individual.25000.34999
  cen$income.35000.49999 = census$income.35000.39999 + census$income.40000.44999 + census$income.45000.49999 + census$individual.35000.49999
  cen$income.50000.74999 = census$income.50000.59999 + census$income.60000.74999 + census$individual.50000.64999 + census$individual.65000.74999
  cen$income.75000.99999 = census$income.75000.99999 + census$individual.75000.100000
  cen$income.100000.124999 = census$income.100000.124999
  cen$income.125000.149999 = census$income.125000.149999
  cen$income.150000.199999 = census$income.150000.199999
  cen$income.200000.500000 = census$income.200000.500000

  # insurance
  cen$privateInsurance = sum(census[endsWith(names(census), "private.insurance")])
  cen$publicInsurance = sum(census[endsWith(names(census), "public.insurance")])
  cen$noInsurance = sum(census[endsWith(names(census), "no.insurance")])

# ********************************* PROPORTIONS ******************************************************************
# Find the diffrenece between the Sam City proportions and the Canses Data Proportions for each element

  part[2:6] = part[2:6]/sum(part[2:6]) - cen[2:6]/sum(cen[2:6])                       # household type
  part[7:8] = part[7:8]/sum(part[7:8]) - cen[7:8]/sum(cen[7:8])                       # adult or child
  part[9:15] = part[9:15]/sum(part[9:15]) - cen[9:15]/sum(cen[9:15])                  # family size
  part[16:21] = part[16:21]/sum(part[16:21]) - cen[16:21]/sum(cen[16:21])             # number of vehicles
  part[22:23] = part[22:23]/sum(part[22:23]) - cen[22:23]/sum(cen[22:23])             # gender
  part[24:37] = part[24:37]/sum(part[24:37]) - cen[24:37]/sum(cen[24:37])             # age
  part[38:45] = part[38:45]/sum(part[38:45]) - cen[38:45]/sum(cen[38:45])             # race
  part[46:48] = part[46:48]/sum(part[46:48]) - cen[46:48]/sum(cen[46:48])             # school enrollment
  part[49:55] = part[49:55]/sum(part[49:55]) - cen[49:55]/sum(cen[49:55])             # educational attainment
  part[56:59] = part[56:59]/sum(part[56:59]) - cen[56:59]/sum(cen[56:59])             # employment
  part[60:62] = part[60:62]/sum(part[60:62]) - cen[60:62]/sum(cen[60:62])             # disability
  part[63:64] = part[63:64]/sum(part[63:64]) - cen[63:64]/sum(cen[63:64])             # nativity
  part[65:67] = part[65:67]/sum(part[65:67]) - cen[65:67]/sum(cen[65:67])             # english speaking skills
  part[68:70] = part[68:70]/sum(part[68:70]) - cen[68:70]/sum(cen[68:70])             # citizenship
  part[71:73] = part[71:73]/sum(part[71:73]) - cen[71:73]/sum(cen[71:73])             # language at home
  part[74:75] = part[74:75]/sum(part[74:75]) - cen[74:75]/sum(cen[74:75])             # veteran status
  part[76:82] = part[76:82]/sum(part[76:82]) - cen[76:82]/sum(cen[76:82])             # means of transportation to work
  part[83:91] = part[83:91]/sum(part[83:91]) - cen[83:91]/sum(cen[83:91])             # travel time
  part[92:102] = part[92:102]/sum(part[92:102]) - cen[92:102]/sum(cen[92:102])        # income
  part[103:105] = part[103:105]/sum(part[103:105]) - cen[103:105]/sum(cen[103:105])   # health insurance

  # Any NA values should be 0
  part[is.na(part)]=0
  
  # Count the number of times the difference in proportions was more than .10 (10%)
  part$flag_count = rowSums(part[2:105] < -0.1 | part[2:105] > 0.1)

  # Return the data frame that has all the info stored
  return(part)
}
