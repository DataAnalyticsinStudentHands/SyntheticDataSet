getsexandage <- function(county, tract, syntheticdataset,seed,Census_data_list){
  
  
  #Set seed so sampling is random but repeatable
  set.seed(seed)
  
  
  #Read in Census data and subset by tract and county
  #Data for sex age and race code
  Census_data=Census_data[(Census_data$tract==tract)&(Census_data$county==county)]
  
  
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
  all.the.adult.men=cbind(BlackMen,AmerIndianAlaskanMen,AsianMen,Islandermen,OtherRaceMen,MultiRaceMen,WhiteMen,HispanicMen)
  all.the.adult.women=cbind(BlackWomen,AmerIndianAlaskanWomen,AsianWomen,Islanderwomen,OtherRaceWomen,MultiRaceWomen,WhiteWomen,HispanicWomen)
  totalhouseholders=sum(2*Census_data$married.couple.families,male.householders.no.wife,female.householders.no.husband,nonfamily.1.person.household,nonfamily.2.person.household,nonfamily.3.person.household,nonfamily.4.person.household,nonfamily.5.person.household,nonfamily.6.person.household,nonfamily.7.person.household)
  
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
    
    Female=colnames(cbind(BlackWomen,Blackgirls,AmerIndianAlaskanWomen,AmerIndianAlaskangirls,AsianWomen,Asiangirls,Islanderwomen,Islandergirls,OtherRaceWomen,OtherRacegirls,MultiRaceWomen,MultiRacegirls,WhiteWomen,Whitegirls,HispanicWomen,Hispanicgirls))
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
                              ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(Islanderwomen,Islandergirls,Islandermen,Islanderboys))),"Native Hawaiian or Other Pacific Islander",
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
