#' Simulate All Individual Characteristics
#'
#' This function uses data from the U.S. Census to simulate a variety of characteristics for an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for some of the characteristics, such as sex, race, and, age, which are then used to simulate other dependent characteristics.
#' It then samples using the user inputed seed.
#'
#' @param syntheticdataset The individual simulated so far.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with all the characteristics for an individual.

getindividualcharacteristics <- function(syntheticdataset, seed, Census_data){
  # Set seed so sampling is random but repeatable
  set.seed(seed)

  # Samples for sex, race, and age 
  # Organize Census data by race and adult and children, as well as get the minimum number of necessary adults per race: 1 per household 2 for married couples
  # boys and girls are 17 and under men and women are 18 and over
  races = c("black", "american.indian.or.alaskan", "asian", "islander", "other.race", "multiracial", "white", "hispanic")

  sapply(races, function(race){
    assign(paste0(race, "Boys"), Census_data[startsWith(names(Census_data), paste0(race,".boys"))], envir = parent.frame(3))
    assign(paste0(race, "Men"), Census_data[startsWith(names(Census_data), paste0(race,".men"))], envir = parent.frame(3))
    assign(paste0(race, "Girls"), Census_data[startsWith(names(Census_data), paste0(race,".girls"))], envir = parent.frame(3))
    assign(paste0(race, "Women"), Census_data[startsWith(names(Census_data), paste0(race,".women"))], envir = parent.frame(3))
  })

  all.the.kids = cbind(blackBoys, blackGirls, american.indian.or.alaskanBoys, american.indian.or.alaskanGirls, asianBoys, asianGirls, islanderBoys, islanderGirls, other.raceBoys, other.raceGirls, multiracialBoys, multiracialGirls, whiteBoys, whiteGirls, hispanicBoys, hispanicGirls)
  all.the.adult.men = cbind(blackMen, american.indian.or.alaskanMen, asianMen, islanderMen, other.raceMen, multiracialMen, whiteMen, hispanicMen)
  all.the.adult.women = cbind(blackWomen, american.indian.or.alaskanWomen, asianWomen, islanderWomen, other.raceWomen, multiracialWomen, whiteWomen, hispanicWomen)
  totalhouseholders = sum(2 * Census_data$married.couple.families, Census_data$male.householders.no.wife, Census_data$female.householders.no.husband, Census_data$nonfamily.1.person.household, Census_data$nonfamily.2.person.household, Census_data$nonfamily.3.person.household, Census_data$nonfamily.4.person.household, Census_data$nonfamily.5.person.household, Census_data$nonfamily.6.person.household, Census_data$nonfamily.7.person.household)

  # Samples for school enrollment 
  age_groups = c(".women.5.to.9", ".women.10.to.14", ".women.15.to.17", ".women.18.to.19", ".women.20.to.24", ".women.25.to.34", ".women.over.35",
                 ".men.5.to.9", ".men.10.to.14", ".men.15.to.17", ".men.18.to.19", ".men.20.to.24", ".men.25.to.34", ".men.over.35")

  sapply(age_groups, function(group) assign(paste0("enrollment", group), Census_data[endsWith(names(Census_data), paste0("school", group))], envir = parent.frame(3)))

  # Samples for education attainment
  age_groups = c(".women.18.24", ".women.25.34", ".women.35.44", ".women.45.64", ".women.over.65",
                 ".men.18.24", ".men.25.34", ".men.35.44", ".men.45.64", ".men.over.65")

  sapply(age_groups, function(group) assign(paste0("attainment", group), Census_data[endsWith(names(Census_data), paste0("education", group))], envir = parent.frame(3)))

  attainment_code = c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")

  # Samples for employment
  age_groups = c(".women.16.19", ".women.20.21", ".women.22.24", ".women.25.29", ".women.30.34", ".women.35.44", ".women.45.54", ".women.55.59", ".women.60.61", ".women.62.64", ".women.65.69", ".women.70.74", ".women.over.75",
                 ".men.16.19", ".men.20.21", ".men.22.24", ".men.25.29", ".men.30.34", ".men.35.44", ".men.45.54", ".men.55.59", ".men.60.61", ".men.62.64", ".men.65.69", ".men.70.74", ".men.over.75")

  sapply(age_groups, function(group) assign(paste0("employment", group), Census_data[endsWith(names(Census_data), paste0("status", group))], envir = parent.frame(3)))

  employment.women.20.24 = employment.women.20.21 + employment.women.22.24
  employment.women.55.64 = employment.women.55.59 + employment.women.60.61 + employment.women.62.64
  employment.men.20.24 = employment.men.20.21 + employment.men.22.24
  employment.men.55.64 = employment.men.55.59 + employment.men.60.61 + employment.men.62.64
  employment.women.65.74 = employment.women.65.69 + employment.women.70.74
  employment.men.65.74 = employment.men.65.69 + employment.men.70.74

  employment_code1 = c("In Armed Forces","Employed","Unemployed","Not in labor force")
  employment_code2 = c("Employed","Unemployed","Not in labor force")

  # Samples for disability
  age_group = c("under18", "from18.64", "over65")
  sapply(age_group, function(group) assign(group, Census_data[startsWith(names(Census_data), group)], envir = parent.frame(3)))

  disability_code = c("With One Type of Disability", "With Two or More Types of Disabilities", "No Disabilities")

  # Samples for nativity and english speaking skills
  races = c("black", "amer.indian.alaskan", "asian", "islander", "other", "multiracial", "white", "hispanic")

  sapply(races, function(race) assign(paste0("language", race), Census_data[c(paste0(race, ".native.only.english"), paste0(race, ".native.other.language.english.well"), paste0(race, ".native.other.language.english.bad"), paste0(race, ".foreign.only.english"), paste0(race, ".foreign.other.language.english.well"), paste0(race, ".foreign.other.language.english.bad"))], envir = parent.frame(3)))

  #samples for citizenship and language at home
  native_english = c("native.5.17.english.well", "native.5.17.english.bad", "native.over18.english.well", "native.over18.english.bad")
  foreign_english = c("english.well", "english.bad")

  sapply(native_english, function(skill) assign(skill, Census_data[endsWith(names(Census_data), skill)], envir = parent.frame(3)))

  sapply(foreign_english, function(skill){
    assign(paste0("foreign.5.17.", skill), Census_data[c(paste0("spanish.foreign.5.17.naturalized.", skill), paste0("other.lang.foreign.5.17.naturalized.", skill), paste0("spanish.foreign.5.17.not.citizen.", skill), paste0("other.lang.foreign.5.17.not.citizen.", skill))], envir = parent.frame(3))
    assign(paste0("foreign.18.", skill), Census_data[c(paste0("spanish.foreign.18.naturalized.", skill), paste0("other.lang.foreign.18.naturalized.", skill), paste0("spanish.foreign.18.not.citizen.", skill), paste0("other.lang.foreign.18.not.citizen.", skill))], envir = parent.frame(3))
  })

  foreign.5.17onlyenglish=Census_data[startsWith(names(Census_data), "english.foreign.5.17")]
  foreign.18.onlyenglish=Census_data[startsWith(names(Census_data), "english.foreign.over18")]

  # Samples for veteran status
  age_groups = c("men.18.34", "men.35.54", "men.55.64", "men.65.74", "men.over75",
                 "women.18.34", "women.35.54", "women.55.64", "women.65.74", "women.over75")

  sapply(age_groups, function(group) assign(paste0("vets.", group), Census_data[c(paste0("veteran.", group), paste0("nonveteran.", group))], envir = parent.frame(3)))

  # Samples for means of transportation to work
  gender = c("men", "women")
  sapply(gender, function(g) assign(g, Census_data[startsWith(names(Census_data), g)], envir = parent.frame(3)))

  transport_code = c("drove alone","carpooled","public transportation","bicycle","walked","motorcycle taxicab or other","worked at home")

  # Samples for travel time to work
  transportation = c("drove.alone", "carpooled", "public.transport", "walked", "other")
  sapply(transportation, function(transport) assign(transport, Census_data[startsWith(names(Census_data), transport)], envir = parent.frame(3)))

  travel_code = c("0 to 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 44 minutes","45 to 59 minutes","60 to 100 minutes")

  # These are all the characteristics that will be simulated and added to the data set
  member = character()
  sex = character()
  bracket.age = character()
  age = character()
  race = character()
  school.enrollment = character()
  education.attainment = character()
  employment = character()
  disability = character()
  nativity=character()
  English.speaking.skills=character()
  citizenship = character()
  Language.at.home = character()
  veteran.status = character()
  means.of.transportation.to.work = character()
  travel.time.to.work = character()

  # The characterisitics for each individual in a household are simulated in this loop using all of the above varaiables
  syntheticdataset = cbind(syntheticdataset, as.data.frame(do.call(rbind, lapply(1:nrow(syntheticdataset), function(i){
    partialset = data.frame()
    
  # Householders and married couples were already created in the initial data frame, and these people must be adults.
  # To make sure adults and children are evenly balanced other members of households are first sampled as either children or adults
  # with the population reweighted by removing adult householders. The result is that all "NA" members or now a child or adult
    member = switch(as.character(syntheticdataset[i,]$members),
                    "NA" = sample(c("Child","Adult"), size = 1, prob = c(sum(all.the.kids)/(sum(all.the.kids,all.the.adult.women,all.the.adult.men) - (totalhouseholders)), (sum(all.the.adult.women,all.the.adult.men) - (totalhouseholders))/(sum(all.the.adult.men,all.the.adult.women,all.the.kids) - (totalhouseholders)))),
                    "Husband" = "Husband",
                    "Wife" = "Wife",
                    "Male Householder" = "Male Householder",
                    "Female Householder" = "Female Householder",
                    "Householder" = "Householder")
    
    # The sexbyagecode is string that includes the individuals race, sex, and age
    sexbyagecode = switch(member,
                          "Child" = sample(colnames(all.the.kids), size = 1, prob = c(all.the.kids/sum(all.the.kids))),
                          "Adult Man"=, "Male Householder"=, "Husband" = sample(colnames(all.the.adult.men), size = 1, prob = c(all.the.adult.men/sum(all.the.adult.men))),
                          "Adult Woman"=, "Female Householder"=, "Wife" = sample(colnames(all.the.adult.women), size = 1, prob = c(all.the.adult.women/sum(all.the.adult.women))),
                          "Householder"=, "Adult" = sample(colnames(cbind(all.the.adult.women,all.the.adult.men)), size = 1, prob = c(all.the.adult.women/sum(all.the.adult.women,all.the.adult.men), all.the.adult.men/sum(all.the.adult.women,all.the.adult.men))))

    # Race and sex are extracted from the sexbyagecode. Sex is the last word from the raceandsex list and race is the first part of the raceandsex list.
    raceandsex = unlist(strsplit(gsub("[0-9]+[^0-9]*", "", sexbyagecode), "\\."))
    sex = switch(raceandsex[length(raceandsex)],
                 "girls"=, "women" = "Female",
                 "Male")
    race = paste(raceandsex[1:length(raceandsex)-1], collapse =" ")

    # bracket.age is extracted from the last part of the sexbyagecode and it is string that states the age range (Ex: "5.to.9"). 
    # This is included in the data set so that it is easier to simulate other characteristics that are dependent on age.
    # age is real number between the range stated in bracket.age
    bracket.age <- gsub("^([^0-9]+)", "", sexbyagecode)
    age_range <- as.numeric(unlist(strsplit(gsub("[^0-9]+", ".", bracket.age), "\\.")))
    age = sample(c(age_range[1]:age_range[2]),1)

    # school.enrollment is dependent on sex and bracket.age
    school.enrollment = switch(sex,
                               "Female" = switch(bracket.age,
                                                 "5.to.9" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.5.to.9/sum(enrollment.women.5.to.9)),
                                                 "10.to.14" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.10.to.14/sum(enrollment.women.10.to.14)),
                                                 "15.to.17" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.15.to.17/sum(enrollment.women.15.to.17)),
                                                 "18.to.19" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.18.to.19/sum(enrollment.women.18.to.19)),
                                                 "20.to.24" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.20.to.24/sum(enrollment.women.20.to.24)),
                                                 "25.to.29"=, "30.to.34" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.25.to.34/sum(enrollment.women.25.to.34)),
                                                 "35.to.44"=, "45.to.54"=, "55.to.64"=, "65.to.74"=, "75.to.84"=, "85.to.100" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.over.35/sum(enrollment.women.over.35)),
                                                 NA),
                               "Male" = switch(bracket.age,
                                               "5.to.9" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.5.to.9/sum(enrollment.men.5.to.9)),
                                               "10.to.14" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.10.to.14/sum(enrollment.men.10.to.14)),
                                               "15.to.17" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.15.to.17/sum(enrollment.men.15.to.17)),
                                               "18.to.19" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.18.to.19/sum(enrollment.men.18.to.19)),
                                               "20.to.24" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.20.to.24/sum(enrollment.men.20.to.24)),
                                               "25.to.29"=, "30.to.34" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.25.to.34/sum(enrollment.men.25.to.34)),
                                               "35.to.44"=, "45.to.54"=, "55.to.64"=, "65.to.74"=, "75.to.84"=, "85.to.100" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.over.35/sum(enrollment.men.over.35)),
                                               NA))

    # education.attainment is dependent on sex and bracket.age
    education.attainment = switch(sex,
                                  "Male" = switch(bracket.age,
                                                  "18.to.19"=, "20.to.24" = sample(attainment_code, 1, prob = attainment.men.18.24/sum(attainment.men.18.24)),
                                                  "25.to.29"=, "30.to.34" = sample(attainment_code, 1, prob = attainment.men.25.34/sum(attainment.men.25.34)),
                                                  "35.to.44"= sample(attainment_code, 1, prob = attainment.men.35.44/sum(attainment.men.35.44)),
                                                  "45.to.54"=, "55.to.64" = sample(attainment_code, 1, prob = attainment.men.45.64/sum(attainment.men.45.64)),
                                                  "65.to.74"=, "75.to.84"=, "85.to.100" = sample(attainment_code, 1, prob = attainment.men.over.65/sum(attainment.men.over.65)),
                                                  NA),
                                  "Female" = switch(bracket.age,
                                                    "18.to.19"=, "20.to.24" = sample(attainment_code, 1, prob = attainment.women.18.24/sum(attainment.women.18.24)),
                                                    "25.to.29"=, "30.to.34" = sample(attainment_code, 1, prob = attainment.women.25.34/sum(attainment.women.25.34)),
                                                    "35.to.44"= sample(attainment_code, 1, prob = attainment.women.35.44/sum(attainment.women.35.44)),
                                                    "45.to.54"=, "55.to.64" = sample(attainment_code, 1, prob = attainment.women.45.64/sum(attainment.women.45.64)),
                                                    "65.to.74"=, "75.to.84"=, "85.to.100" = sample(attainment_code, 1, prob = attainment.women.over.65/sum(attainment.women.over.65)),
                                                    NA))

    # employment is dependent on sex and bracket.age
    employment = switch(sex,
                        "Male" = switch(bracket.age,
                                        "18.to.19" = sample(employment_code1,1,prob =employment.men.16.19/sum(employment.men.16.19)),
                                        "20.to.24" = sample(employment_code1,1,prob=employment.men.20.24/sum(employment.men.20.24)),
                                        "25.to.29"=sample(employment_code1,1,prob=employment.men.25.29/sum(employment.men.25.29)),
                                        "30.to.34" = sample(employment_code1,1,prob=employment.men.30.34/sum(employment.men.30.34)),
                                        "35.to.44"= sample(employment_code1,1,prob=employment.men.35.44/sum(employment.men.35.44)),
                                        "45.to.54"=sample(employment_code1,1,prob=employment.men.45.54/sum(employment.men.45.54)),
                                        "55.to.64" = sample(employment_code1,1,prob=employment.men.55.64/sum(employment.men.55.64)),
                                        "65.to.74"=sample(employment_code2,1,prob=employment.men.65.74/sum(employment.men.65.74)),
                                        "75.to.84"=, "85.to.100" = sample(employment_code2,1,prob=employment.men.over.75/sum(employment.men.over.75)),
                                        NA),
                        "Female" = switch(bracket.age,
                                          "18.to.19"=sample(employment_code1,1,prob=employment.women.16.19/sum(employment.women.16.19)),
                                          "20.to.24" = sample(employment_code1,1,prob=employment.women.20.24/sum(employment.women.20.24)),
                                          "25.to.29"=sample(employment_code1,1,prob=employment.women.25.29/sum(employment.women.25.29)),
                                          "30.to.34" = sample(employment_code1,1,prob=employment.women.30.34/sum(employment.women.30.34)),
                                          "35.to.44"=  sample(employment_code1,1,prob=employment.women.35.44/sum(employment.women.35.44)),
                                          "45.to.54"=sample(employment_code1,1,prob=employment.women.45.54/sum(employment.women.45.54)),
                                          "55.to.64" = sample(employment_code1,1,prob=employment.women.55.64/sum(employment.women.55.64)),
                                          "65.to.74"=sample(employment_code2,1,prob=employment.women.65.74/sum(employment.women.65.74)),
                                          "75.to.84"=, "85.to.100" =  sample(employment_code2,1,prob=employment.women.over.75/sum(employment.women.over.75)),
                                          NA))

    # disability is dependent on bracket.age
    disability = switch(bracket.age,
                        "0.to.5"=, "5.to.9"=, "10.to.14"=, "15.to.17" = sample(disability_code, 1, prob = under18/sum(under18)),
                        "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34"=, "35.to.44"=, "45.to.54"=, "55.to.64" = sample(disability_code, 1, prob = from18.64/sum(from18.64)),
                        "65.to.74"=, "75.to.84"=, "85.to.100" = sample(disability_code, 1, prob = over65/sum(over65)))

    # codelangnat is dependent on bracket.age and race and returns a string stating the race, nativity and english spaeking skill
    codelangnat = switch(bracket.age,
                         "0.to.5" = NA,
                         switch(race,
                                "black" =sample(colnames(languageblack),1,prob=languageblack/sum(languageblack)),
                                "american indian or alaskan" = sample(colnames(languageamer.indian.alaskan),1,prob=languageamer.indian.alaskan/sum(languageamer.indian.alaskan)),
                                "asian" = sample(colnames(languageasian),1,prob=languageasian/sum(languageasian)),
                                "islander" = sample(colnames(languageislander),1,prob=languageislander/sum(languageislander)),
                                "other race" = sample(colnames(languageother),1,prob=languageother/sum(languageother)),
                                "multiracial" = sample(colnames(languagemultiracial),1,prob=languagemultiracial/sum(languagemultiracial)),
                                "white" = sample(colnames(languagewhite),1,prob=languagewhite/sum(languagewhite)),
                                "hispanic" = sample(colnames(languagehispanic),1,prob=languagehispanic/sum(languagehispanic))))

    nativity = switch(as.character(codelangnat == "NA"),
                      "FALSE" = switch(as.character(codelangnat %in% c("black.native.only.english","black.native.other.language.english.well","black.native.other.language.english.bad","amer.indian.alaskan.native.only.english","amer.indian.alaskan.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.bad","asian.native.only.english","asian.native.other.language.english.well","asian.native.other.language.english.bad",
                                                                       "islander.native.only.english","islander.native.other.language.english.well","islander.native.other.language.english.bad","other.native.only.english","other.native.other.language.english.well","other.native.other.language.english.bad","multiracial.native.only.english","multiracial.native.other.language.english.well","multiracial.native.other.language.english.bad","white.native.only.english","white.native.other.language.english.well","white.native.other.language.english.bad",
                                                                       "hispanic.native.only.english","hispanic.native.other.language.english.well","hispanic.native.other.language.english.bad")),
                                       "TRUE" = "Native",
                                       "Foreign"),
                      NA)

    English.speaking.skills = ifelse(codelangnat %in% c("hispanic.native.only.english","other.native.only.english","white.native.only.english","multiracial.native.only.english","islander.native.only.english","asian.native.only.english","amer.indian.alaskan.native.only.english","black.native.only.english","hispanic.foreign.only.english","white.foreign.only.english","multiracial.foreign.only.english","islander.foreign.only.english","asian.foreign.only.english","amer.indian.alaskan.foreign.only.english","black.foreign.only.english","other.foreign.only.english"),"Speaks Only English",
                                     ifelse(codelangnat %in% c("other.native.other.language.english.well","other.foreign.other.language.english.well","hispanic.foreign.other.language.english.well","white.foreign.other.language.english.well","multiracial.foreign.other.language.english.well","islander.foreign.other.language.english.well","asian.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.well","black.foreign.other.language.english.well","hispanic.native.other.language.english.well","white.native.other.language.english.well","multiracial.native.other.language.english.well","islander.native.other.language.english.well","asian.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.well","black.native.other.language.english.well"),"Speaks English Very Well",
                                            ifelse(codelangnat %in% c("other.native.other.language.english.bad","other.foreign.other.language.english.bad","hispanic.foreign.other.language.english.bad","white.foreign.other.language.english.bad","multiracial.foreign.other.language.english.bad","islander.foreign.other.language.english.bad","asian.foreign.other.language.english.bad","amer.indian.alaskan.foreign.other.language.english.bad","black.foreign.other.language.english.bad","hispanic.native.other.language.english.bad","white.native.other.language.english.bad","multiracial.native.other.language.english.bad","islander.native.other.language.english.bad","asian.native.other.language.english.bad","amer.indian.alaskan.native.other.language.english.bad","black.native.other.language.english.bad"),"Speaks English less than well",NA)))

    acode = switch(nativity,
                   "Native" = switch(English.speaking.skills,
                                     "Speaks English Very Well" = sample(colnames(cbind(native.5.17.english.well, native.over18.english.well)), 1, prob = cbind(native.5.17.english.well/sum(native.5.17.english.well, native.over18.english.well), native.over18.english.well/sum(native.5.17.english.well, native.over18.english.well))),
                                     "Speaks English less than well" = sample(colnames(cbind(native.5.17.english.bad, native.over18.english.bad)), 1, prob = cbind(native.5.17.english.bad/sum(native.5.17.english.bad, native.over18.english.bad), native.over18.english.bad/sum(native.over18.english.bad, native.5.17.english.bad))),
                                     NA),
                   "Foreign" = switch(English.speaking.skills,
                                      "Speaks Only English" = sample(colnames(cbind(foreign.5.17onlyenglish, foreign.18.onlyenglish)), 1, prob = cbind(foreign.5.17onlyenglish/sum(foreign.5.17onlyenglish, foreign.18.onlyenglish), foreign.18.onlyenglish/sum(foreign.18.onlyenglish, foreign.5.17onlyenglish))),
                                      "Speaks English Very Well" = sample(colnames(cbind(foreign.5.17.english.well, foreign.18.english.well)), 1, prob = cbind(foreign.5.17.english.well/sum(foreign.5.17.english.well, foreign.18.english.well), foreign.18.english.well/sum(foreign.18.english.well, foreign.5.17.english.well))),
                                      "Speaks English less than well" = sample(colnames(cbind(foreign.5.17.english.bad, foreign.18.english.bad)), 1, prob = cbind(foreign.5.17.english.bad/sum(foreign.5.17.english.bad, foreign.18.english.bad), foreign.18.english.bad/sum(foreign.18.english.bad, foreign.5.17.english.bad))),
                                      NA),
                   NA)

    citizenship = ifelse(nativity == "Native", "Citizen",
                         ifelse(acode %in% c("english.foreign.5.17.naturalized","spanish.foreign.5.17.naturalized.english.well","other.lang.foreign.5.17.naturalized.english.well","spanish.foreign.5.17.naturalized.english.bad","other.lang.foreign.5.17.naturalized.english.bad","english.foreign.over18.naturalized","spanish.foreign.18.naturalized.english.well","other.lang.foreign.18.naturalized.english.well","spanish.foreign.18.naturalized.english.bad","other.lang.foreign.18.naturalized.english.bad"), "Naturalized Citizen",
                                ifelse(grepl("not.citizen", acode), "Not a U.S. Citizen", NA)))

    Language.at.home = ifelse(English.speaking.skills == "Speaks Only English", "English",
                              ifelse(grepl("spanish", acode), "Speaks Spanish",
                                     ifelse(grepl("other.lang", acode), "Speaks Other Languages", NA)))

    veteran.status = switch(as.character(employment),
                            "In Armed Forces" = "Nonveteran",
                            switch(sex,
                                   "Male" = switch(bracket.age,
                                                   "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34" = sample(c("Veteran","Nonveteran"),1,prob=vets.men.18.34/sum(vets.men.18.34)),
                                                   "35.to.44"=, "45.to.54"=sample(c("Veteran","Nonveteran"),1,prob=vets.men.35.54/sum(vets.men.35.54)),
                                                   "55.to.64" = sample(c("Veteran","Nonveteran"),1,prob=vets.men.55.64/sum(vets.men.55.64)),
                                                   "65.to.74"=sample(c("Veteran","Nonveteran"),1,prob=vets.men.65.74/sum(vets.men.65.74)),
                                                   "75.to.84"=, "85.to.100" = sample(c("Veteran","Nonveteran"),1,prob=vets.men.over75/sum(vets.men.over75)),
                                                   "Nonveteran"),
                                   "Female" = switch(bracket.age,
                                                     "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34" = sample(c("Veteran","Nonveteran"),1,prob=vets.women.18.34/sum(vets.women.18.34)),
                                                     "35.to.44"=, "45.to.54"=sample(c("Veteran","Nonveteran"),1,prob=vets.women.35.54/sum(vets.women.35.54)),
                                                     "55.to.64" = sample(c("Veteran","Nonveteran"),1,prob=vets.women.55.64/sum(vets.women.55.64)),
                                                     "65.to.74"=sample(c("Veteran","Nonveteran"),1,prob=vets.women.65.74/sum(vets.women.65.74)),
                                                     "75.to.84"=, "85.to.100" =  sample(c("Veteran","Nonveteran"),1,prob=vets.women.over75/sum(vets.women.over75)),
                                                     "Nonveteran")))

    means.of.transportation.to.work = switch(as.character(employment),
                                             "Employed" = switch(sex,
                                                                 "Male" = sample(transport_code,1,prob=men/sum(men)),
                                                                 sample(transport_code,1,prob=women/sum(women))),
                                             NA)

    travel.time.to.work = switch(as.character(means.of.transportation.to.work),
                                 "drove alone" = sample(travel_code,1,prob=drove.alone/sum(drove.alone)),
                                 "carpooled" = sample(travel_code,1,prob=carpooled/sum(carpooled)),
                                 "public transport" = sample(travel_code,1,prob=public.transport/sum(public.transport)),
                                 "walked" = sample(travel_code,1,prob=walked/sum(walked)),
                                 "taxi, motorcycle, bike or other" = sample(travel_code,1,prob=other/sum(other)),
                                 NA)

    if(!is.na(travel.time.to.work)){
      twonumbers <- strsplit(gsub("[^0-9]+", ".", travel.time.to.work), "\\.")
      time_range <- as.numeric(twonumbers[[1]])
      travel.time.to.work = sample(c(time_range[1]:time_range[2]), 1)
    }

    partialset = cbind(member, sex, bracket.age, age, race, school.enrollment, education.attainment, employment, disability, nativity, English.speaking.skills, citizenship, Language.at.home, veteran.status, means.of.transportation.to.work, travel.time.to.work)

    return(partialset)
  })), stringsAsFactors = F))

  syntheticdataset$members = NULL

  return(syntheticdataset)
}
