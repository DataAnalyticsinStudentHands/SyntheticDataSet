#' Get Sex Age and Race
#'
#' This function uses data from the U.S. Census to simulate the sex, race and age of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for sex age and race which are cross tabulated together.
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variables for sex, age and race.


getsexraceandage <- function(state, county, tract, syntheticdataset,seed,Census_data){


  #Set seed so sampling is random but repeatable
  set.seed(seed)


  #Read in Census data and subset by tract and county
  #Data for sex age and race code
  Census_data=Census_data[(Census_data$state==state)&(Census_data$tract==tract)&(Census_data$county==county),]


  #Organize Census data by race and adult and children, as well as get the minimum number of necessary adults per race: 1 per household 2 for married couples
  #boys and girls are 17 and under men and women are 18 and over

  Blackboys=Census_data[c("black.boys.under.5","black.boys.5.to.9","black.boys.10.to.14","black.boys.15.to.17")]
  BlackMen=Census_data[c("black.men.18.to.19","black.men.20.to.24","black.men.25.to.29","black.men.30.to.34","black.men.35.to.44","black.men.45.to.54","black.men.55.to.64","black.men.65.to.74","black.men.75.to.84","black.men.over.85")]
  Blackgirls=Census_data[c("black.girls.under.5","black.girls.5.to.9","black.girls.10.to.14","black.girls.15.to.17")]
  BlackWomen=Census_data[c("black.women.18.to.19","black.women.20.to.24","black.women.25.to.29","black.women.30.to.34","black.women.35.to.44","black.women.45.to.54","black.women.55.to.64","black.women.65.to.74","black.women.75.to.84","black.women.over.85")]
  totalBlack=sum(BlackWomen,Blackgirls,BlackMen,Blackboys)

  AmerIndianAlaskanboys=Census_data[c("amer.indian.alaskan.boys.under.5","amer.indian.alaskan.boys.5.to.9","amer.indian.alaskan.boys.10.to.14","amer.indian.alaskan.boys.15.to.17")]
  AmerIndianAlaskanMen=Census_data[c("amer.indian.alaskan.men.18.to.19","amer.indian.alaskan.men.20.to.24","amer.indian.alaskan.men.25.to.29","amer.indian.alaskan.men.30.to.34","amer.indian.alaskan.men.35.to.44","amer.indian.alaskan.men.45.to.54","amer.indian.alaskan.men.55.to.64","amer.indian.alaskan.men.65.to.74","amer.indian.alaskan.men.75.to.84","amer.indian.alaskan.men.over.85")]
  AmerIndianAlaskangirls=Census_data[c("amer.indian.alaskan.girls.under.5","amer.indian.alaskan.girls.5.to.9","amer.indian.alaskan.girls.10.to.14","amer.indian.alaskan.girls.15.to.17")]
  AmerIndianAlaskanWomen=Census_data[c("amer.indian.alaskan.women.18.to.19","amer.indian.alaskan.women.20.to.24","amer.indian.alaskan.women.25.to.29","amer.indian.alaskan.women.30.to.34","amer.indian.alaskan.women.35.to.44","amer.indian.alaskan.women.45.to.54","amer.indian.alaskan.women.55.to.64","amer.indian.alaskan.women.65.to.74","amer.indian.alaskan.women.75.to.84","amer.indian.alaskan.women.over.85")]
  totalAmerIndianAlaskan=sum(AmerIndianAlaskanWomen,AmerIndianAlaskangirls,AmerIndianAlaskanMen,AmerIndianAlaskanboys)

  Asianboys=Census_data[c("asian.boys.under.5","asian.boys.5.to.9","asian.boys.10.to.14","asian.boys.15.to.17")]
  AsianMen=Census_data[c("asian.men.18.to.19","asian.men.20.to.24","asian.men.25.to.29","asian.men.30.to.34","asian.men.35.to.44","asian.men.45.to.54","asian.men.55.to.64","asian.men.65.to.74","asian.men.75.to.84","asian.men.over.85")]
  Asiangirls=Census_data[c("asian.girls.under.5","asian.girls.5.to.9","asian.girls.10.to.14","asian.girls.15.to.17")]
  AsianWomen=Census_data[c("asian.women.18.to.19","asian.women.20.to.24","asian.women.25.to.29","asian.women.30.to.34","asian.women.35.to.44","asian.women.45.to.54","asian.women.55.to.64","asian.women.65.to.74","asian.women.75.to.84","asian.women.over.85")]
  totalAsian=sum(AsianWomen,Asiangirls,AsianMen,Asianboys)

  Islanderboys=Census_data[c("islander.boys.under.5","islander.boys.5.to.9","islander.boys.10.to.14","islander.boys.15.to.17")]
  IslanderMen=Census_data[c("islander.men.18.to.19","islander.men.20.to.24","islander.men.25.to.29","islander.men.30.to.34","islander.men.35.to.44","islander.men.45.to.54","islander.men.55.to.64","islander.men.65.to.74","islander.men.75.to.84","islander.men.over.85")]
  Islandergirls=Census_data[c("islander.girls.under.5","islander.girls.5.to.9","islander.girls.10.to.14","islander.girls.15.to.17")]
  IslanderWomen=Census_data[c("islander.women.18.to.19","islander.women.20.to.24","islander.women.25.to.29","islander.women.30.to.34","islander.women.35.to.44","islander.women.45.to.54","islander.women.55.to.64","islander.women.65.to.74","islander.women.75.to.84","islander.women.over.85")]
  totalIslander=sum(IslanderWomen,Islandergirls,IslanderMen,Islanderboys)

  OtherRaceboys=Census_data[c("other.race.boys.under.5","other.race.boys.5.to.9","other.race.boys.10.to.14","other.race.boys.15.to.17")]
  OtherRaceMen=Census_data[c("other.race.men.18.to.19","other.race.men.20.to.24","other.race.men.25.to.29","other.race.men.30.to.34","other.race.men.35.to.44","other.race.men.45.to.54","other.race.men.55.to.64","other.race.men.65.to.74","other.race.men.75.to.84","other.race.men.over.85")]
  OtherRacegirls=Census_data[c("other.race.girls.under.5","other.race.girls.5.to.9","other.race.girls.10.to.14","other.race.girls.15.to.17")]
  OtherRaceWomen=Census_data[c("other.race.women.18.to.19","other.race.women.20.to.24","other.race.women.25.to.29","other.race.women.30.to.34","other.race.women.35.to.44","other.race.women.45.to.54","other.race.women.55.to.64","other.race.women.65.to.74","other.race.women.75.to.84","other.race.women.over.85")]
  totalOtherRace=sum(OtherRaceWomen,OtherRacegirls,OtherRaceMen,OtherRaceboys)

  MultiRaceboys=Census_data[c("multiracial.boys.under.5","multiracial.boys.5.to.9","multiracial.boys.10.to.14","multiracial.boys.15.to.17")]
  MultiRaceMen=Census_data[c("multiracial.men.18.to.19","multiracial.men.20.to.24","multiracial.men.25.to.29","multiracial.men.30.to.34","multiracial.men.35.to.44","multiracial.men.45.to.54","multiracial.men.55.to.64","multiracial.men.65.to.74","multiracial.men.75.to.84","multiracial.men.over.85")]
  MultiRacegirls=Census_data[c("multiracial.girls.under.5","multiracial.girls.5.to.9","multiracial.girls.10.to.14","multiracial.girls.15.to.17")]
  MultiRaceWomen=Census_data[c("multiracial.women.18.to.19","multiracial.women.20.to.24","multiracial.women.25.to.29","multiracial.women.30.to.34","multiracial.women.35.to.44","multiracial.women.45.to.54","multiracial.women.55.to.64","multiracial.women.65.to.74","multiracial.women.75.to.84","multiracial.women.over.85")]
  totalMultiRace=sum(MultiRaceWomen,MultiRacegirls,MultiRaceMen,MultiRaceboys)

  Whiteboys=Census_data[c("white.boys.under.5","white.boys.5.to.9","white.boys.10.to.14","white.boys.15.to.17")]
  WhiteMen=Census_data[c("white.men.18.to.19","white.men.20.to.24","white.men.25.to.29","white.men.30.to.34","white.men.35.to.44","white.men.45.to.54","white.men.55.to.64","white.men.65.to.74","white.men.75.to.84","white.men.over.85")]
  Whitegirls=Census_data[c("white.girls.under.5","white.girls.5.to.9","white.girls.10.to.14","white.girls.15.to.17")]
  WhiteWomen=Census_data[c("white.women.18.to.19","white.women.20.to.24","white.women.25.to.29","white.women.30.to.34","white.women.35.to.44","white.women.45.to.54","white.women.55.to.64","white.women.65.to.74","white.women.75.to.84","white.women.over.85")]
  totalWhite=sum(WhiteWomen,Whitegirls,WhiteMen,Whiteboys)

  Hispanicboys=Census_data[c("hispanic.boys.under.5","hispanic.boys.5.to.9","hispanic.boys.10.to.14","hispanic.boys.15.to.17")]
  HispanicMen=Census_data[c("hispanic.men.18.to.19","hispanic.men.20.to.24","hispanic.men.25.to.29","hispanic.men.30.to.34","hispanic.men.35.to.44","hispanic.men.45.to.54","hispanic.men.55.to.64","hispanic.men.65.to.74","hispanic.men.75.to.84","hispanic.men.over.85")]
  Hispanicgirls=Census_data[c("hispanic.girls.under.5","hispanic.girls.5.to.9","hispanic.girls.10.to.14","hispanic.girls.15.to.17")]
  HispanicWomen=Census_data[c("hispanic.women.18.to.19","hispanic.women.20.to.24","hispanic.women.25.to.29","hispanic.women.30.to.34","hispanic.women.35.to.44","hispanic.women.45.to.54","hispanic.women.55.to.64","hispanic.women.65.to.74","hispanic.women.75.to.84","hispanic.women.over.85")]
  totalHispanic=sum(HispanicWomen,Hispanicgirls,HispanicMen,Hispanicboys)

  all.the.kids=cbind(Blackboys,Blackgirls,AmerIndianAlaskanboys,AmerIndianAlaskangirls,Asianboys,Asiangirls,Islanderboys,Islandergirls,OtherRaceboys,OtherRacegirls,MultiRaceboys,MultiRacegirls,Whiteboys,Whitegirls,Hispanicboys,Hispanicgirls)
  all.the.adult.men=cbind(BlackMen,AmerIndianAlaskanMen,AsianMen,IslanderMen,OtherRaceMen,MultiRaceMen,WhiteMen,HispanicMen)
  all.the.adult.women=cbind(BlackWomen,AmerIndianAlaskanWomen,AsianWomen,IslanderWomen,OtherRaceWomen,MultiRaceWomen,WhiteWomen,HispanicWomen)
  totalhouseholders=sum(2*Census_data$married.couple.families,Census_data$male.householders.no.wife,Census_data$female.householders.no.husband,Census_data$nonfamily.1.person.household,Census_data$nonfamily.2.person.household,Census_data$nonfamily.3.person.household,Census_data$nonfamily.4.person.household,Census_data$nonfamily.5.person.household,Census_data$nonfamily.6.person.household,Census_data$nonfamily.7.person.household)

  #householders and married couples were already created in the function gethouseholdtypeandsize, these people must be adults
  #to make sure adults and children are evenly balanced other members of households are first sampled as either children or adults
  #with the population reweighted by removing adult householders this is done in the function get NA members
  getsNAmembers <- function(syntheticdataset) {

    #sample non householders as either child or adult by removing householders from the population first
    members=ifelse((syntheticdataset$member=="NA"),sample(c("Child","Adult"),size=1,prob=c(sum(all.the.kids)/(sum(all.the.kids,all.the.adult.women,all.the.adult.men)-(totalhouseholders)),(sum(all.the.adult.women,all.the.adult.men)-(totalhouseholders))/(sum(all.the.adult.men,all.the.adult.women,all.the.kids)-(totalhouseholders)))),
                   #leave couples and householders alone
                   ifelse((syntheticdataset$member=="Husband"),"Husband",
                          ifelse((syntheticdataset$member=="Wife"),"Wife",
                                 ifelse((syntheticdataset$member=="Male Householder"),"Male Householder",
                                        ifelse((syntheticdataset$member=="Female Householder"),"Female Householder",
                                               ifelse((syntheticdataset$member=="Householder"),"Householder",NA))))))


    return(members)
  }

  #function to sample sex, age and race code with whether they are adults or children and sex for adults presupposed

  getsexagecode <- function(syntheticdataset) {

    sexagecode=ifelse((syntheticdataset$member=="Child"),sample(colnames(all.the.kids),size=1,prob=c(all.the.kids/sum(all.the.kids))),
                      ifelse((syntheticdataset$member=="Adult Man"|syntheticdataset$member=="Male Householder"|syntheticdataset$member=="Husband"),sample(colnames(all.the.adult.men),size=1, prob=c(all.the.adult.men/sum(all.the.adult.men))),
                             ifelse((syntheticdataset$member=="Adult Woman"|syntheticdataset$member=="Female Householder"|syntheticdataset$member=="Wife"),sample(colnames(all.the.adult.women),size=1,prob=c(all.the.adult.women/sum(all.the.adult.women))),
                                    ifelse((syntheticdataset$member=="Householder"|syntheticdataset$member=="Adult"),sample(colnames(cbind(all.the.adult.women,all.the.adult.men)),size=1,prob=c(all.the.adult.women/sum(all.the.adult.women,all.the.adult.men),all.the.adult.men/sum(all.the.adult.women,all.the.adult.men))),NA))))
    return(sexagecode)
  }

  #Use previous declared functions

  syntheticdataset$member=getsNAmembers(syntheticdataset)

  syntheticdataset$sexbyagecode=getsexagecode(syntheticdataset)

  #Use sexageandrace code to get sex
  getsex <- function(syntheticdataset){

    Female=colnames(cbind(BlackWomen,Blackgirls,AmerIndianAlaskanWomen,AmerIndianAlaskangirls,AsianWomen,Asiangirls,IslanderWomen,Islandergirls,OtherRaceWomen,OtherRacegirls,MultiRaceWomen,MultiRacegirls,WhiteWomen,Whitegirls,HispanicWomen,Hispanicgirls))
    sex=ifelse((syntheticdataset$sexbyagecode %in% Female),"Female","Male")
    return(sex)
  }

  syntheticdataset$sex=getsex(syntheticdataset)

  #Use sexageandrace code to get age
  getage <- function(syntheticdataset){
    age=ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.under.5","amer.indian.alaskan.under.5","asian.boys.under.5","islander.boys.under.5","other.race.boys.under.5","multiracial.boys.under.5","white.boys.under.5","hispanic.boys.under.5","black.girls.under.5","amer.indian.alaskan.under.5","asian.girls.under.5","islander.girls.under.5","other.race.girls.under.5","multiracial.girls.under.5","white.girls.under.5","hispanic.girls.under.5")),"Under 5",
               ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.5.to.9","amer.indian.alaskan.5.to.9","asian.boys.5.to.9","islander.boys.5.to.9","other.race.boys.5.to.9","multiracial.boys.5.to.9","white.boys.5.to.9","hispanic.boys.5.to.9","black.girls.5.to.9","amer.indian.alaskan.5.to.9","asian.girls.5.to.9","islander.girls.5.to.9","other.race.girls.5.to.9","multiracial.girls.5.to.9","white.girls.5.to.9","hispanic.girls.5.to.9")),"5 to 9",
                      ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.10.to.14","amer.indian.alaskan.10.to.14","asian.boys.10.to.14","islander.boys.10.to.14","other.race.boys.10.to.14","multiracial.boys.10.to.14","white.boys.10.to.14","hispanic.boys.10.to.14","black.girls.10.to.14","amer.indian.alaskan.10.to.14","asian.girls.10.to.14","islander.girls.10.to.14","other.race.girls.10.to.14","multiracial.girls.10.to.14","white.girls.10.to.14","hispanic.girls.10.to.14")),"10 to 14",
                             ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.15.to.17","amer.indian.alaskan.15.to.17","asian.boys.15.to.17","islander.boys.15.to.17","other.race.boys.15.to.17","multiracial.boys.15.to.17","white.boys.15.to.17","hispanic.boys.15.to.17","black.girls.15.to.17","amer.indian.alaskan.15.to.17","asian.girls.15.to.17","islander.girls.15.to.17","other.race.girls.15.to.17","multiracial.girls.15.to.17","white.girls.15.to.17","hispanic.girls.15.to.17")),"15 to 17",
                                    ifelse((syntheticdataset$sexbyagecode %in% c("black.men.18.to.19","amer.indian.alaskan.18.to.19","asian.men.18.to.19","islander.men.18.to.19","other.race.men.18.to.19","multiracial.men.18.to.19","white.men.18.to.19","hispanic.men.18.to.19","black.women.18.to.19","amer.indian.alaskan.18.to.19","asian.women.18.to.19","islander.women.18.to.19","other.race.women.18.to.19","multiracial.women.18.to.19","white.women.18.to.19","hispanic.women.18.to.19")),"18 to 19",
                                           ifelse((syntheticdataset$sexbyagecode %in% c("black.men.20.to.24","amer.indian.alaskan.20.to.24","asian.men.20.to.24","islander.men.20.to.24","other.race.men.20.to.24","multiracial.men.20.to.24","white.men.20.to.24","hispanic.men.20.to.24","black.women.20.to.24","amer.indian.alaskan.20.to.24","asian.women.20.to.24","islander.women.20.to.24","other.race.women.20.to.24","multiracial.women.20.to.24","white.women.20.to.24","hispanic.women.20.to.24")),"20 to 24",
                                                  ifelse((syntheticdataset$sexbyagecode %in% c("black.men.25.to.29","amer.indian.alaskan.25.to.29","asian.men.25.to.29","islander.men.25.to.29","other.race.men.25.to.29","multiracial.men.25.to.29","white.men.25.to.29","hispanic.men.25.to.29","black.women.25.to.29","amer.indian.alaskan.25.to.29","asian.women.25.to.29","islander.women.25.to.29","other.race.women.25.to.29","multiracial.women.25.to.29","white.women.25.to.29","hispanic.women.25.to.29")),"25 to 29",
                                                         ifelse((syntheticdataset$sexbyagecode %in% c("black.men.30.to.34","amer.indian.alaskan.30.to.34","asian.men.30.to.34","islander.men.30.to.34","other.race.men.30.to.34","multiracial.men.30.to.34","white.men.30.to.34","hispanic.men.30.to.34","black.women.30.to.34","amer.indian.alaskan.30.to.34","asian.women.30.to.34","islander.women.30.to.34","other.race.women.30.to.34","multiracial.women.30.to.34","white.women.30.to.34","hispanic.women.30.to.34")),"30 to 34",
                                                                ifelse((syntheticdataset$sexbyagecode %in% c("black.men.35.to.44","amer.indian.alaskan.35.to.44","asian.men.35.to.44","islander.men.35.to.44","other.race.men.35.to.44","multiracial.men.35.to.44","white.men.35.to.44","hispanic.men.35.to.44","black.women.35.to.44","amer.indian.alaskan.35.to.44","asian.women.35.to.44","islander.women.35.to.44","other.race.women.35.to.44","multiracial.women.35.to.44","white.women.35.to.44","hispanic.women.35.to.44")),"35 to 44",
                                                                       ifelse((syntheticdataset$sexbyagecode %in% c("black.men.45.to.54","amer.indian.alaskan.45.to.54","asian.men.45.to.54","islander.men.45.to.54","other.race.men.45.to.54","multiracial.men.45.to.54","white.men.45.to.54","hispanic.men.45.to.54","black.women.45.to.54","amer.indian.alaskan.45.to.54","asian.women.45.to.54","islander.women.45.to.54","other.race.women.45.to.54","multiracial.women.45.to.54","white.women.45.to.54","hispanic.women.45.to.54")),"45 to 54",
                                                                              ifelse((syntheticdataset$sexbyagecode %in% c("black.men.55.to.64","amer.indian.alaskan.55.to.64","asian.men.55.to.64","islander.men.55.to.64","other.race.men.55.to.64","multiracial.men.55.to.64","white.men.55.to.64","hispanic.men.55.to.64","black.women.55.to.64","amer.indian.alaskan.55.to.64","asian.women.55.to.64","islander.women.55.to.64","other.race.women.55.to.64","multiracial.women.55.to.64","white.women.55.to.64","hispanic.women.55.to.64")),"55 to 64",
                                                                                     ifelse((syntheticdataset$sexbyagecode %in% c("black.men.65.to.74","amer.indian.alaskan.65.to.74","asian.men.65.to.74","islander.men.65.to.74","other.race.men.65.to.74","multiracial.men.65.to.74","white.men.65.to.74","hispanic.men.65.to.74","black.women.65.to.74","amer.indian.alaskan.65.to.74","asian.women.65.to.74","islander.women.65.to.74","other.race.women.65.to.74","multiracial.women.65.to.74","white.women.65.to.74","hispanic.women.65.to.74")),"65 to 74",
                                                                                            ifelse((syntheticdataset$sexbyagecode %in% c("black.men.75.to.84","amer.indian.alaskan.75.to.84","asian.men.75.to.84","islander.men.75.to.84","other.race.men.75.to.84","multiracial.men.75.to.84","white.men.75.to.84","hispanic.men.75.to.84","black.women.75.to.84","amer.indian.alaskan.75.to.84","asian.women.75.to.84","islander.women.75.to.84","other.race.women.75.to.84","multiracial.women.75.to.84","white.women.75.to.84","hispanic.women.75.to.84")),"75 to 84",
                                                                                                   ifelse((syntheticdataset$sexbyagecode %in% c("black.men.over.85","amer.indian.alaskan.over.85","asian.men.over.85","islander.men.over.85","other.race.men.over.85","multiracial.men.over.85","white.men.over.85","hispanic.men.over.85","black.women.over.85","amer.indian.alaskan.over.85","asian.women.over.85","islander.women.over.85","other.race.women.over.85","multiracial.women.over.85","white.women.over.85","hispanic.women.over.85")),"Over 85"
                                                                                                          ,NA))))))))))))))
    return(age)
  }

  syntheticdataset$age=getage(syntheticdataset)

  #get race from sexageandrace code
  getrace <- function(syntheticdataset){
    race=ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(BlackWomen,Blackgirls,BlackMen,Blackboys))),"Black or African American",
                ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(AmerIndianAlaskanWomen,AmerIndianAlaskangirls,AmerIndianAlaskanMen,AmerIndianAlaskanboys))),"American Indian or Alaskan Native",
                       ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(AsianWomen,Asiangirls,AsianMen,Asianboys))),"Asian",
                              ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(IslanderWomen,Islandergirls,IslanderMen,Islanderboys))),"Native Hawaiian or Other Pacific Islander",
                                     ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(OtherRaceWomen,OtherRacegirls,OtherRaceMen,OtherRaceboys))),"Some Other Race",
                                            ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(MultiRaceboys,MultiRacegirls,MultiRaceMen,MultiRaceWomen))),"Two or More Races",
                                                   ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(WhiteWomen,Whitegirls,WhiteMen,Whiteboys))),"White",
                                                          ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(HispanicWomen,Hispanicgirls,HispanicMen,Hispanicboys))),"Hispanic or Latino",NA))))))))
    return(race)
  }

  syntheticdataset$race=getrace(syntheticdataset)

  #remove code as it is no longer needed
  syntheticdataset$sexbyagecode=NULL

  return(syntheticdataset)
}

#' Get School Enrollment
#'
#' This function uses data from the U.S. Census to simulate the school enrollment an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for school enrollment based on presimulated sex and age.
#' Sex and age can be simulated with the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include sex and age.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for school enrollment.
getschoolenrollment <- function(state,county,tract,syntheticdataset,seed,Census_data){

  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  Women5.9=Census_data[c("public.school.women.5.to.9","private.school.women.5.to.9","no.school.women.5.to.9")]
  Women10.14=Census_data[c("public.school.women.10.to.14","private.school.women.10.to.14","no.school.women.10.to.14")]
  Women15.17=Census_data[c("public.school.women.15.to.17","private.school.women.15.to.17","no.school.women.15.to.17")]
  Women18.19=Census_data[c("public.school.women.18.to.19","private.school.women.18.to.19","no.school.women.18.to.19")]
  Women20.24=Census_data[c("public.school.women.20.to.24","private.school.women.20.to.24","no.school.women.20.to.24")]
  Women25.34=Census_data[c("public.school.women.25.to.34","private.school.women.25.to.34","no.school.women.25.to.34")]
  Women35=Census_data[c("public.school.women.over.35","private.school.women.over.35","no.school.women.over.35")]

  Men5.9=Census_data[c("public.school.men.5.to.9","private.school.men.5.to.9","no.school.men.5.to.9")]
  Men10.14=Census_data[c("public.school.men.10.to.14","private.school.men.10.to.14","no.school.men.10.to.14")]
  Men15.17=Census_data[c("public.school.men.15.to.17","private.school.men.15.to.17","no.school.men.15.to.17")]
  Men18.19=Census_data[c("public.school.men.18.to.19","private.school.men.18.to.19","no.school.men.18.to.19")]
  Men20.24=Census_data[c("public.school.men.20.to.24","private.school.men.20.to.24","no.school.men.20.to.24")]
  Men25.34=Census_data[c("public.school.men.25.to.34","private.school.men.25.to.34","no.school.men.25.to.34")]
  Men35=Census_data[c("public.school.men.over.35","private.school.men.over.35","no.school.men.over.35")]
  #"Public School","Private School","Not Enrolled in School"

  school.enrollment=ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="5 to 9",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women5.9/sum(Women5.9)),
                           ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="10 to 14",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women10.14/sum(Women10.14)),
                                  ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="15 to 17",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women15.17/sum(Women15.17)),
                                         ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="18 to 19",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women18.19/sum(Women18.19)),
                                                ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="20 to 24",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women20.24/sum(Women20.24)),
                                                       ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women25.34/sum(Women25.34)),
                                                              ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"|syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women35/sum(Women35)),
                                                                     ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="5 to 9",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men5.9/sum(Men5.9)),
                                                                            ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="10 to 14",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men10.14/sum(Men10.14)),
                                                                                   ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="15 to 17",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men15.17/sum(Men15.17)),
                                                                                          ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="18 to 19",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men18.19/sum(Men18.19)),
                                                                                                 ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="20 to 24",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men20.24/sum(Men20.24)),
                                                                                                        ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men25.34/sum(Men25.34)),
                                                                                                               ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"|syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men35/sum(Men35))
                                                                                                                      ,NA))))))))))))))
  syntheticdataset$school.enrollment=school.enrollment
  return(syntheticdataset)
}

#' Get Education Attainment
#'
#' This function uses data from the U.S. Census to simulate the educational attainment of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for educational attainment based on presimulated sex and age.
#' Sex and age can be simulated with the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include sex and age.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for educational.attainment.
#'
geteducationattainment=function(state, county,tract,syntheticdataset,seed,Census_data){

  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  men18.24=Census_data[c("less.than.9.grade.men.18.24","btwn.9.to.12.grade.men.18.24","high.school.men.18.24","some.college.men.18.24","associates.men.18.24","bachelors.men.18.24","post.grad.men.18.24")]
  men25.34=Census_data[c("less.than.9.grade.men.25.34","btwn.9.to.12.grade.men.25.34","high.school.men.25.34","some.college.men.25.34","associates.men.25.34","bachelors.men.25.34","post.grad.men.25.34")]
  men35.44=Census_data[c("less.than.9.grade.men.35.44","btwn.9.to.12.grade.men.35.44","high.school.men.35.44","some.college.men.35.44","associates.men.35.44","bachelors.men.35.44","post.grad.men.35.44")]
  men45.64=Census_data[c("less.than.9.grade.men.45.64","btwn.9.to.12.grade.men.45.64","high.school.men.45.64","some.college.men.45.64","associates.men.45.64","bachelors.men.45.64","post.grad.men.45.64")]
  men65=Census_data[c("less.than.9.grade.men.over.65","btwn.9.to.12.grade.men.over.65","high.school.men.over.65","some.college.men.over.65","associates.men.over.65","bachelors.men.over.65","post.grad.men.over.65")]

  women18.24=Census_data[c("less.than.9.grade.women.18.24","btwn.9.to.12.grade.women.18.24","high.school.women.18.24","some.college.women.18.24","associates.women.18.24","bachelors.women.18.24","post.grad.women.18.24")]
  women25.34=Census_data[c("less.than.9.grade.women.25.34","btwn.9.to.12.grade.women.25.34","high.school.women.25.34","some.college.women.25.34","associates.women.25.34","bachelors.women.25.34","post.grad.women.25.34")]
  women35.44=Census_data[c("less.than.9.grade.women.35.44","btwn.9.to.12.grade.women.35.44","high.school.women.35.44","some.college.women.35.44","associates.women.35.44","bachelors.women.35.44","post.grad.women.35.44")]
  women45.64=Census_data[c("less.than.9.grade.women.45.64","btwn.9.to.12.grade.women.45.64","high.school.women.45.64","some.college.women.45.64","associates.women.45.64","bachelors.women.45.64","post.grad.women.45.64")]
  women65=Census_data[c("less.than.9.grade.women.over.65","btwn.9.to.12.grade.women.over.65","high.school.women.over.65","some.college.women.over.65","associates.women.over.65","bachelors.women.over.65","post.grad.women.over.65")]

  code=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")

  educational.attainment=ifelse((syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24")),sample(code,1,prob=men18.24/sum(men18.24)),
                                ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(code,1,prob=men25.34/sum(men25.34)),
                                       ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="35 to 44"),sample(code,1,prob=men35.44/sum(men35.44)),
                                              ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"),sample(code,1,prob=men45.64/sum(men45.64)),
                                                     ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code,1,prob=men65/sum(men65)),
                                                            ifelse((syntheticdataset$sex=="Female"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24")),sample(code,1,prob=women18.24/sum(women18.24)),
                                                                   ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(code,1,prob=women25.34/sum(women25.34)),
                                                                          ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="35 to 44"),sample(code,1,prob=women35.44/sum(women35.44)),
                                                                                 ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"),sample(code,1,prob=women45.64/sum(women45.64)),
                                                                                        ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code,1,prob=women65/sum(women65)),NA))))))))))
  syntheticdataset$education.attainment=educational.attainment
  return(syntheticdataset)
}

#' Get Employment
#'
#' This function uses data from the U.S. Census to simulate the educational attainment of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for employment based on presimulated sex and age.
#' Sex and age can be simulated with the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include sex and age.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for employment.
getemployment=function(state, county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  Women16.19=Census_data[c("in.armed.forces.women.16.19","employed.women.16.19","unemployed.women.16.19","not.in.labor.forces.women.16.19")]
  Women20.21=Census_data[c("in.armed.forces.women.20.21","employed.women.20.21","unemployed.women.20.21","not.in.labor.forces.women.20.21")]
  Women22.24=Census_data[c("in.armed.forces.women.22.24","employed.women.22.24","unemployed.women.22.24","not.in.labor.forces.women.22.24")]
  Women20.24=Women20.21+Women22.24
  Women25.29=Census_data[c("in.armed.forces.women.25.29","employed.women.25.29","unemployed.women.25.29","not.in.labor.forces.women.25.29")]
  Women30.34=Census_data[c("in.armed.forces.women.30.34","employed.women.30.34","unemployed.women.30.34","not.in.labor.forces.women.30.34")]
  Women35.44=Census_data[c("in.armed.forces.women.35.44","employed.women.35.44","unemployed.women.35.44","not.in.labor.forces.women.35.44")]
  Women45.54=Census_data[c("in.armed.forces.women.45.54","employed.women.45.54","unemployed.women.45.54","not.in.labor.forces.women.45.54")]
  Women55.59=Census_data[c("in.armed.forces.women.55.59","employed.women.55.59","unemployed.women.55.59","not.in.labor.forces.women.55.59")]
  Women60.61=Census_data[c("in.armed.forces.women.60.61","employed.women.60.61","unemployed.women.60.61","not.in.labor.forces.women.60.61")]
  Women62.64=Census_data[c("in.armed.forces.women.62.64","employed.women.62.64","unemployed.women.62.64","not.in.labor.forces.women.62.64")]
  Women55.64=Women55.59+Women60.61+Women62.64
  Women65.69=Census_data[c("employed.women.65.69","unemployed.women.65.69","not.in.labor.forces.women.65.69")]
  Women70.74=Census_data[c("employed.women.70.74","unemployed.women.70.74","not.in.labor.forces.women.70.74")]
  Women65.74=Women65.69+Women70.74
  Women75=Census_data[c("employed.women.over.75","unemployed.women.over.75","not.in.labor.forces.women.over.75")]

  Men16.19=Census_data[c("in.armed.forces.men.16.19","employed.men.16.19","unemployed.men.16.19","not.in.labor.forces.men.16.19")]
  Men20.21=Census_data[c("in.armed.forces.men.20.21","employed.men.20.21","unemployed.men.20.21","not.in.labor.forces.men.20.21")]
  Men22.24=Census_data[c("in.armed.forces.men.22.24","employed.men.22.24","unemployed.men.22.24","not.in.labor.forces.men.22.24")]
  Men20.24=Men20.21+Men22.24
  Men25.29=Census_data[c("in.armed.forces.men.25.29","employed.men.25.29","unemployed.men.25.29","not.in.labor.forces.men.25.29")]
  Men30.34=Census_data[c("in.armed.forces.men.30.34","employed.men.30.34","unemployed.men.30.34","not.in.labor.forces.men.30.34")]
  Men35.44=Census_data[c("in.armed.forces.men.35.44","employed.men.35.44","unemployed.men.35.44","not.in.labor.forces.men.35.44")]
  Men45.54=Census_data[c("in.armed.forces.men.45.54","employed.men.45.54","unemployed.men.45.54","not.in.labor.forces.men.45.54")]
  Men55.59=Census_data[c("in.armed.forces.men.55.59","employed.men.55.59","unemployed.men.55.59","not.in.labor.forces.men.55.59")]
  Men60.61=Census_data[c("in.armed.forces.men.60.61","employed.men.60.61","unemployed.men.60.61","not.in.labor.forces.men.60.61")]
  Men62.64=Census_data[c("in.armed.forces.men.62.64","employed.men.62.64","unemployed.men.62.64","not.in.labor.forces.men.62.64")]
  Men55.64=Men55.59+Men60.61+Men62.64
  Men65.69=Census_data[c("employed.men.65.69","unemployed.men.65.69","not.in.labor.forces.men.65.69")]
  Men70.74=Census_data[c("employed.men.70.74","unemployed.men.70.74","not.in.labor.forces.men.70.74")]
  Men65.74=Men65.69+Men70.74
  Men75=Census_data[c("employed.men.over.75","unemployed.men.over.75","not.in.labor.forces.men.over.75")]

  code1=c("In Armed Forces","Employed","Unemployed","Not in labor force")
  code2=c("Employed","Unemployed","Not in labor force")
  employment=ifelse((syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19")),sample(code1,1,prob=Men16.19/sum(Men16.19)), #syntheticdataset$age=="15 to 17"| is left out because of age problems :(
                     ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="20 to 24"),sample(code1,1,prob=Men20.24/sum(Men20.24)),
                            ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"),sample(code1,1,prob=Men25.29/sum(Men25.29)),
                                   ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="30 to 34"),sample(code1,1,prob=Men30.34/sum(Men30.34)),
                                          ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="35 to 44"),sample(code1,1,prob=Men35.44/sum(Men35.44)),
                                                 ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="45 to 54"),sample(code1,1,prob=Men45.54/sum(Men45.54)),
                                                        ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="55 to 64"),sample(code1,1,prob=Men55.64/sum(Men55.64)),
                                                               ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="65 to 74"),sample(code2,1,prob=Men65.74/sum(Men65.74)),
                                                                      ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code2,1,prob=Men75/sum(Men75)),
                                                                             ifelse((syntheticdataset$sex=="Female"&(syntheticdataset$age=="18 to 19")),sample(code1,1,prob=Women16.19/sum(Women16.19)), #syntheticdataset$age=="15 to 17"| we've got age problems :(
                                                                                    ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="20 to 24"),sample(code1,1,prob=Women20.24/sum(Women20.24)),
                                                                                           ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"),sample(code1,1,prob=Women25.29/sum(Women25.29)),
                                                                                                  ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="30 to 34"),sample(code1,1,prob=Women30.34/sum(Women30.34)),
                                                                                                         ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="35 to 44"),sample(code1,1,prob=Women35.44/sum(Women35.44)),
                                                                                                                ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="45 to 54"),sample(code1,1,prob=Women45.54/sum(Women45.54)),
                                                                                                                       ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="55 to 64"),sample(code1,1,prob=Women55.64/sum(Women55.64)),
                                                                                                                              ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="65 to 74"),sample(code2,1,prob=Women65.74/sum(Women65.74)),
                                                                                                                                     ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code2,1,prob=Women75/sum(Women75)),NA))))))))))))))))))
  syntheticdataset$employment=employment
  return(syntheticdataset)
}

#' Get Disability
#'
#' This function uses data from the U.S. Census to simulate the disability status of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for diability status based on presimulated age.
#' Age can be simulated with the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include age.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for disability status.
#'
getdisability <- function(state,county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  Under18=Census_data[c("under18.1.disability","under18.2.disability","under18.no.disability")]
  From18.64=Census_data[c("from18.64.1.disability","from18.64.2.disability","from18.64.no.disability")]
  Over65=Census_data[c("over65.1.disability","over65.2.disability","over65.no.disability")]
  code=c("With One Type of Disability","With Two or More Types of Disabilities","No Disabilities")

  disability=ifelse(syntheticdataset$age=="Under 5"|syntheticdataset$age=="5 to 9"|syntheticdataset$age=="10 to 14"|syntheticdataset$age=="15 to 17",sample(code,1,prob=Under18/sum(Under18)),
                    ifelse(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64",sample(code,1,prob=From18.64/sum(From18.64)),
                           ifelse(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85",sample(code,1,prob=Over65/sum(Over65)),NA)))

  syntheticdataset$disability=disability
  return(syntheticdataset)
}

#' Get English Speaking Ability and Nativity
#'
#' This function uses data from the U.S. Census to simulate the Language and Naticity of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for lenglish speaking ability and nativity based on presimulated race.
#' Race can be simulated with the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include race.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variables for english speaking ability and nativity.
#'

getlangandnativity <- function(state, county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  languageblack=Census_data[c("black.native.only.english","black.native.other.language.english.well","black.native.other.language.english.bad","black.foreign.only.english","black.foreign.other.language.english.well","black.foreign.other.language.english.bad")]
  languageAIAN=Census_data[c("amer.indian.alaskan.native.only.english","amer.indian.alaskan.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.bad","amer.indian.alaskan.foreign.only.english","amer.indian.alaskan.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.bad")]
  languageasian=Census_data[c("asian.native.only.english","asian.native.other.language.english.well","asian.native.other.language.english.bad","asian.foreign.only.english","asian.foreign.other.language.english.well","asian.foreign.other.language.english.bad")]
  languageNHPI=Census_data[c("islander.native.only.english","islander.native.other.language.english.well","islander.native.other.language.english.bad","islander.foreign.only.english","islander.foreign.other.language.english.well","islander.foreign.other.language.english.bad")]
  languageother=Census_data[c("other.native.only.english","other.native.other.language.english.well","other.native.other.language.english.bad","other.foreign.only.english","other.foreign.other.language.english.well","other.foreign.other.language.english.bad")]
  language2=Census_data[c("multiracial.native.only.english","multiracial.native.other.language.english.well","multiracial.native.other.language.english.bad","multiracial.foreign.only.english","multiracial.foreign.other.language.english.well","multiracial.foreign.other.language.english.bad")]
  languagewhite=Census_data[c("white.native.only.english","white.native.other.language.english.well","white.native.other.language.english.bad","white.foreign.only.english","white.foreign.other.language.english.well","white.foreign.other.language.english.bad")]
  languagehispanic=Census_data[c("hispanic.native.only.english","hispanic.native.other.language.english.well","hispanic.native.other.language.english.bad","hispanic.foreign.only.english","hispanic.foreign.other.language.english.well","hispanic.foreign.other.language.english.bad")]

  codelangnat=ifelse(syntheticdataset$race=="Black or African American"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageblack/sum(languageblack)),
                     ifelse(syntheticdataset$race=="American Indian or Alaskan Native"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageAIAN/sum(languageAIAN)),
                            ifelse(syntheticdataset$race=="Asian"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageasian/sum(languageasian)),
                                   ifelse(syntheticdataset$race=="Native Hawaiian or Other Pacific Islander"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageNHPI/sum(languageNHPI)),
                                          ifelse(syntheticdataset$race=="Some Other Race"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageother/sum(languageother)),
                                                 ifelse(syntheticdataset$race=="Two or More Races"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=language2/sum(language2)),
                                                        ifelse(syntheticdataset$race=="White"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languagewhite/sum(languagewhite)),
                                                               ifelse(syntheticdataset$race=="Hispanic or Latino"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languagehispanic/sum(languagehispanic)),
                                                                      NA))))))))
  nativity=ifelse(codelangnat %in% c("black.native.only.english","black.native.other.language.english.well","black.native.other.language.english.bad","amer.indian.alaskan.native.only.english","amer.indian.alaskan.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.bad","asian.native.only.english","asian.native.other.language.english.well","asian.native.other.language.english.bad",
                                     "islander.native.only.english","islander.native.other.language.english.well","islander.native.other.language.english.bad","other.native.only.english","other.native.other.language.english.well","other.native.other.language.english.bad","multiracial.native.only.english","multiracial.native.other.language.english.well","multiracial.native.other.language.english.bad","white.native.only.english","white.native.other.language.english.well","white.native.other.language.english.bad",
                                     "hispanic.native.only.english","hispanic.native.other.language.english.well","hispanic.native.other.language.english.bad"),"Native",
                  ifelse(codelangnat %in% c("black.foreign.only.english","black.foreign.other.language.english.well","black.foreign.other.language.english.bad","amer.indian.alaskan.foreign.only.english","amer.indian.alaskan.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.bad","asian.foreign.only.english","asian.foreign.other.language.english.well","asian.foreign.other.language.english.bad",
                                            "islander.foreign.only.english","islander.foreign.other.language.english.well","islander.foreign.other.language.english.bad","other.foreign.only.english","other.foreign.other.language.english.well","other.foreign.other.language.english.bad",
                                            "multiracial.foreign.only.english","multiracial.foreign.other.language.english.well","multiracial.foreign.other.language.english.bad","white.foreign.only.english","white.foreign.other.language.english.well","white.foreign.other.language.english.bad","hispanic.foreign.only.english","hispanic.foreign.other.language.english.well","hispanic.foreign.other.language.english.bad"),"Foreign",NA))

  English=ifelse(codelangnat %in% c("hispanic.native.only.english","other.native.only.english","white.native.only.english","multiracial.native.only.english","islander.native.only.english","asian.native.only.english","amer.indian.alaskan.native.only.english","black.native.only.english","hispanic.foreign.only.english","white.foreign.only.english","multiracial.foreign.only.english","islander.foreign.only.english","asian.foreign.only.english","amer.indian.alaskan.foreign.only.english","black.foreign.only.english","other.foreign.only.english"),"Speaks Only English",
                 ifelse(codelangnat %in% c("other.native.other.language.english.well","other.foreign.other.language.english.well","hispanic.foreign.other.language.english.well","white.foreign.other.language.english.well","multiracial.foreign.other.language.english.well","islander.foreign.other.language.english.well","asian.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.well","black.foreign.other.language.english.well","hispanic.native.other.language.english.well","white.native.other.language.english.well","multiracial.native.other.language.english.well","islander.native.other.language.english.well","asian.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.well","black.native.other.language.english.well"),"Speaks English Very Well",
                        ifelse(codelangnat %in% c("other.native.other.language.english.bad","other.foreign.other.language.english.bad","hispanic.foreign.other.language.english.bad","white.foreign.other.language.english.bad","multiracial.foreign.other.language.english.bad","islander.foreign.other.language.english.bad","asian.foreign.other.language.english.bad","amer.indian.alaskan.foreign.other.language.english.bad","black.foreign.other.language.english.bad","hispanic.native.other.language.english.bad","white.native.other.language.english.bad","multiracial.native.other.language.english.bad","islander.native.other.language.english.bad","asian.native.other.language.english.bad","amer.indian.alaskan.native.other.language.english.bad","black.native.other.language.english.bad"),"Speaks English less than well",NA)))
  syntheticdataset$nativity=nativity
  syntheticdataset$English.speaking.skills=English
  return(syntheticdataset)
}

#' Get Citizenship and Language at Home
#'
#' This function uses data from the U.S. Census to simulate the Citizenship and Language of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for citizenship and language based on presimulated nativity and english speaking ability.
#' English speaking ability and nativity can be simulated using the getlangandnativity function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include english speaking ability and nativity.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variables for language spoken at home and citizenship.

getcitizenandlang <- function(state,county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  native5.17goodenglish=Census_data[c("spanish.native.5.17.english.well","other.lang.native.5.17.english.well")]
  native5.17badenglish=Census_data[c("spanish.native.5.17.english.bad","other.lang.native.5.17.english.bad")]
  native18goodenglish=Census_data[c("spanish.native.over18.english.well","other.lang.native.over18.english.well")]
  native18badenglish=Census_data[c("spanish.native.over18.english.bad","other.lang.native.over18.english.bad")]
  foreign5.17onlyenglish=Census_data[c("english.foreign.5.17.naturalized","english.foreign.5.17.not.citizen")]
  foreign5.17goodenglish=Census_data[c("spanish.foreign.5.17.naturalized.english.well","other.lang.foreign.5.17.naturalized.english.well","spanish.foreign.5.17.not.citizen.english.well","other.lang.foreign.5.17.not.citizen.english.well")]
  foreign5.17badenglish=Census_data[c("spanish.foreign.5.17.naturalized.english.bad","other.lang.foreign.5.17.naturalized.english.bad","spanish.foreign.5.17.not.citizen.english.bad","other.lang.foreign.5.17.not.citizen.english.bad")]
  foreign18onlyenglish=Census_data[c("english.foreign.over18.naturalized","english.foreign.over18.not.citizen")]
  foreign18goodenglish=Census_data[c("spanish.foreign.18.naturalized.english.well","other.lang.foreign.18.naturalized.english.well","spanish.foreign.18.not.citizen.english.well","other.lang.foreign.18.not.citizen.english.well")]
  foreign18badenglish=Census_data[c("spanish.foreign.18.naturalized.english.bad","other.lang.foreign.18.naturalized.english.bad","spanish.foreign.18.not.citizen.english.bad","other.lang.foreign.18.not.citizen.english.bad")]

  syntheticdataset$acode=ifelse(syntheticdataset$nativity=="Native"&syntheticdataset$English.speaking.skills=="Speaks English Very Well",sample(colnames(cbind(native5.17goodenglish,native18goodenglish)),1,prob=cbind(native5.17goodenglish/sum(native5.17goodenglish,native18goodenglish),native18goodenglish/sum(native5.17goodenglish,native18goodenglish))),
                                ifelse(syntheticdataset$nativity=="Native"&syntheticdataset$English.speaking.skills=="Speaks English Not Very Well",sample(colnames(cbind(native5.17badenglish,native18badenglish)),1,prob=cbind(native5.17badenglish/sum(native5.17badenglish,native18badenglish),native18badenglish/sum(native18badenglish,native5.17badenglish))),
                                       ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks Only English",sample(colnames(cbind(foreign5.17onlyenglish,foreign18onlyenglish)),1,prob=cbind(foreign5.17onlyenglish/sum(foreign5.17onlyenglish,foreign18onlyenglish),foreign18onlyenglish/sum(foreign18onlyenglish,foreign5.17onlyenglish))),
                                              ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks English Very Well",sample(colnames(cbind(foreign5.17goodenglish,foreign18goodenglish)),1,prob=cbind(foreign5.17goodenglish/sum(foreign5.17goodenglish,foreign18goodenglish),foreign18goodenglish/sum(foreign18goodenglish,foreign5.17goodenglish))),
                                                     ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks English Not Very Well",sample(colnames(cbind(foreign5.17badenglish,foreign18badenglish)),1,prob=cbind(foreign5.17badenglish/sum(foreign5.17badenglish,foreign18badenglish),foreign18badenglish/sum(foreign18badenglish,foreign5.17badenglish))),NA)))))
  syntheticdataset$citizenship=ifelse(syntheticdataset$nativity=="Native","Citizen",
                                      ifelse(syntheticdataset$acode %in% c("english.foreign.5.17.naturalized","spanish.foreign.5.17.naturalized.english.well","other.lang.foreign.5.17.naturalized.english.well","spanish.foreign.5.17.naturalized.english.bad","other.lang.foreign.5.17.naturalized.english.bad","english.foreign.18.naturalized","spanish.foreign.18.naturalized.english.well","other.lang.foreign.18.naturalized.english.well","spanish.foreign.18.naturalized.english.bad","other.lang.foreign.18.naturalized.english.bad"),"Naturalized Citizen",
                                             ifelse(grepl("not.citizen",syntheticdataset$acode),"Not a U.S. Citizen",NA)))
  syntheticdataset$Language.at.home=ifelse(syntheticdataset$English.speaking.skills=="Speaks Only English","English",
                                           ifelse(grepl("spanish",syntheticdataset$acode),"Speaks Spanish",
                                                  ifelse(grepl("other.lang",syntheticdataset$acode),"Speaks Other Languages",NA)))
  syntheticdataset$acode=NULL
  return(syntheticdataset)
}

#' Get Veteran Status
#'
#' This function uses data from the U.S. Census to simulate the Veteran Status of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for veteran status by sex and age.
#' Sex and age can be simulated using the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include sex and age.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for veteran status.

getvets <- function(state, county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  men18to34=Census_data[c("veteran.men.18.34","nonveteran.men.18.34")]
  men35to54=Census_data[c("veteran.men.35.54","nonveteran.men.35.54")]
  men55to64=Census_data[c("veteran.men.55.64","nonveteran.men.55.64")]
  men65to74=Census_data[c("veteran.men.65.74","nonveteran.men.65.74")]
  men75up=Census_data[c("veteran.men.over75","nonveteran.men.over75")]

  women18to34=Census_data[c("veteran.women.18.34","nonveteran.women.18.34")]
  women35to54=Census_data[c("veteran.women.35.54","nonveteran.women.35.54")]
  women55to64=Census_data[c("veteran.women.55.64","nonveteran.women.55.64")]
  women65to74=Census_data[c("veteran.women.65.74","nonveteran.women.65.74")]
  women75up=Census_data[c("veteran.women.over75","nonveteran.women.over75")]

  veteran.status=ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men18to34/sum(men18to34)),
                        ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men35to54/sum(men35to54)),
                               ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="55 to 64")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men55to64/sum(men55to64)),
                                      ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="65 to 74")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men65to74/sum(men65to74)),
                                             ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men75up/sum(men75up)),
                                                    ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women18to34/sum(women18to34)),
                                                           ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women35to54/sum(women35to54)),
                                                                  ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="55 to 64")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women55to64/sum(women55to64)),
                                                                         ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="65 to 74")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women65to74/sum(women65to74)),
                                                                                ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women75up/sum(women75up)),"Nonveteran"))))))))))
  syntheticdataset$veteran.status=veteran.status
  return(syntheticdataset)
}

#' Get Mode of Transportation
#'
#' This function uses data from the U.S. Census to simulate the mode of transportation to work of an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for mode of transportation by sex.
#' Sex can be simulated using the getsexraceandage function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include sex.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for transport.
gettransport <- function(state, county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  male=Census_data[c("drove.alone.men","carpooled.men","public.transport.men","bike.men","walk.men","other.transport.men","work.at.home.men")]
  female=Census_data[c("drove.alone.women","carpooled.women","public.transport.women","bike.women","walk.women","other.transport.women","work.at.home.women")]

  code=c("drove alone","carpooled","public transportation","bicycle","walked","motorcycle taxicab or other","worked at home")

  transport=ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$sex=="Male",sample(code,1,prob=male/sum(male)),
                   ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$sex=="Female",sample(code,1,prob=female/sum(female)),NA))

  syntheticdataset$means.of.transportation.to.work=transport
  return(syntheticdataset)
}

#' Get Transportation Time
#'
#' This function uses data from the U.S. Census to simulate the time taken to get to work for an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for time taken to get to work based off of mode of transport.
#' Sex can be simulated using the gettransport function
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated so far must include sex.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for transport.
gettraveltime=function(state,county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  drovealone=Census_data[c("drove.alone.less.than.10.minutes","drove.alone.10.14.minutes","drove.alone.15.19.minutes","drove.alone.20.24.minutes","drove.alone.25.29.minutes","drove.alone.30.34.minutes","drove.alone.35.44.minutes","drove.alone.45.59.minutes","drove.alone.over60.minutes")]
  carpooled=Census_data[c("carpooled.less.than.10.minutes","carpooled.10.14.minutes","carpooled.15.19.minutes","carpooled.20.24.minutes","carpooled.25.29.minutes","carpooled.30.34.minutes","carpooled.35.44.minutes","carpooled.45.59.minutes","carpooled.over60.minutes")]
  publictransport=Census_data[c("public.transport.less.than.10.minutes","public.transport.10.14.minutes","public.transport.15.19.minutes","public.transport.20.24.minutes","public.transport.25.29.minutes","public.transport.30.34.minutes","public.transport.35.44.minutes","public.transport.45.59.minutes","public.transport.over60.minutes")]
  walked=Census_data[c("walked.less.than.10.minutes","walked.10.14.minutes","walked.15.19.minutes","walked.20.24.minutes","walked.25.29.minutes","walked.30.34.minutes","walked.35.44.minutes","walked.45.59.minutes","walked.over60.minutes")]
  other=Census_data[c("other.less.than.10.minutes","other.10.14.minutes","other.15.19.minutes","other.20.24.minutes","other.25.29.minutes","other.30.34.minutes","other.35.44.minutes","other.45.59.minutes","other.over60.minutes")]

  code=c("less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 44 minutes","45 to 59 minutes","60 minutes or more")

  travel.time.to.work=ifelse(syntheticdataset$means.of.transportation.to.work=="drove alone",sample(code,1,prob=drovealone/sum(drovealone)),
                             ifelse(syntheticdataset$means.of.transportation.to.work=="carpooled",sample(code,1,prob=carpooled/sum(carpooled)),
                                    ifelse(syntheticdataset$means.of.transportation.to.work=="public transport",sample(code,1,prob=publictransport/sum(publictransport)),
                                           ifelse(syntheticdataset$means.of.transportation.to.work=="walked",sample(code,1,prob=walked/sum(walked)),
                                                  ifelse(syntheticdataset$means.of.transportation.to.work=="taxi, motorcycle, bike or other",sample(code,1,prob=other/sum(other)),NA)))))
  syntheticdataset$travel.time.to.work=travel.time.to.work
  return(syntheticdataset)
}
