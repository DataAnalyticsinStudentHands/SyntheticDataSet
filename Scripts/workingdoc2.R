getsexandage <- function(county, tractvar, syntheticdataset,seed,Census_data_list){
  
  
  #Set seed so sampling is random but repeatable
  set.seed(seed)
  
  
  #Read in Census data and subset by tract and county
  #Data for sex age and race code
  #sexbyagebyrace1=read.csv(paste0(inputdir,"sex_by_age_by_race.csv"))
  sexbyagebyrace1=Census_data_list$sexbyagebyrace1
  sexbyagebyrace <- sexbyagebyrace1[(sexbyagebyrace1$tract==tractvar) & (sexbyagebyrace1$county==county),]
  #Data for household types so we can reweight populations when sampling so we can make sure householders are sampled as adults then remove them from
  #population when sampling so we don't over sample adults
  #householdtypeandrace=read.csv(paste0(inputdir,"householdtypeandrace.csv"))
  householdtypeandrace=Census_data_List$householdtypeandrace
  house <- householdtypeandrace[(householdtypeandrace$tract==tractvar) & (householdtypeandrace$county==county),]
  
  #Organize Census data by race and adult and children, as well as get the minimum number of necessary adults per race: 1 per household 2 for married couples
  #boys and girls are 17 and under men and women are 18 and over
  
  Blackboys=sexbyagebyrace[c("B01001B_003E","B01001B_004E","B01001B_005E","B01001B_006E")]
  BlackMen=sexbyagebyrace[c("B01001B_007E","B01001B_008E","B01001B_009E","B01001B_010E","B01001B_011E","B01001B_012E","B01001B_013E","B01001B_014E","B01001B_015E","B01001B_016E")]
  Blackgirls=sexbyagebyrace[c("B01001B_018E","B01001B_019E","B01001B_020E","B01001B_021E")]
  BlackWomen=sexbyagebyrace[c("B01001B_022E","B01001B_023E","B01001B_024E","B01001B_025E","B01001B_026E","B01001B_027E","B01001B_028E","B01001B_029E","B01001B_030E","B01001B_031E")]
  BlackH=sum(2*house$B11001B_003E,house$B11001B_005E,house$B11001B_006E,house$B11001B_008E,house$B11001B_009E)
  totalBlack=sum(BlackWomen,Blackgirls,BlackMen,Blackboys)
  
  AmerIndianAlaskanboys=sexbyagebyrace[c("B01001C_003E","B01001C_004E","B01001C_005E","B01001C_006E")]
  AmerIndianAlaskanMen=sexbyagebyrace[c("B01001C_007E","B01001C_008E","B01001C_009E","B01001C_010E","B01001C_011E","B01001C_012E","B01001C_013E","B01001C_014E","B01001C_015E","B01001C_016E")]
  AmerIndianAlaskangirls=sexbyagebyrace[c("B01001C_018E","B01001C_019E","B01001C_020E","B01001C_021E")]
  AmerIndianAlaskanWomen=sexbyagebyrace[c("B01001C_022E","B01001C_023E","B01001C_024E","B01001C_025E","B01001C_026E","B01001C_027E","B01001C_028E","B01001C_029E","B01001C_030E","B01001C_031E")]
  AmerIndianAlaskanH=sum(2*house$B11001C_003E,house$B11001C_005E,house$B11001C_006E,house$B11001C_008E,house$B11001C_009E)
  totalIndianAlaskan=sum(AmerIndianAlaskanMen,AmerIndianAlaskanWomen,AmerIndianAlaskanboys,AmerIndianAlaskangirls)
  
  Asianboys=sexbyagebyrace[c("B01001D_003E","B01001D_004E","B01001D_005E","B01001D_006E")]
  AsianMen=sexbyagebyrace[c("B01001D_007E","B01001D_008E","B01001D_009E","B01001D_010E","B01001D_011E","B01001D_012E","B01001D_013E","B01001D_014E","B01001D_015E","B01001D_016E")]
  Asiangirls=sexbyagebyrace[c("B01001D_018E","B01001D_019E","B01001D_020E","B01001D_021E")]
  AsianWomen=sexbyagebyrace[c("B01001D_022E","B01001D_023E","B01001D_024E","B01001D_025E","B01001D_026E","B01001D_027E","B01001D_028E","B01001D_029E","B01001D_030E","B01001D_031E")]
  AsianH=sum(2*house$B11001D_003E,house$B11001D_005E,house$B11001D_006E,house$B11001D_008E,house$B11001D_009E)
  totalAsian=sum(AsianWomen,Asiangirls,AsianMen,Asianboys)
  
  
  Islanderboys=sexbyagebyrace[c("B01001E_003E","B01001E_004E","B01001E_005E","B01001E_006E")]
  Islandermen=sexbyagebyrace[c("B01001E_007E","B01001E_008E","B01001E_009E","B01001E_010E","B01001E_011E","B01001E_012E","B01001E_013E","B01001E_014E","B01001E_015E","B01001E_016E")]
  Islandergirls=sexbyagebyrace[c("B01001E_018E","B01001E_019E","B01001E_020E","B01001E_021E")]
  Islanderwomen=sexbyagebyrace[c("B01001E_022E","B01001E_023E","B01001E_024E","B01001E_025E","B01001E_026E","B01001E_027E","B01001E_028E","B01001E_029E","B01001E_030E","B01001E_031E")]
  IslanderH=sum(2*house$B11001E_003E,house$B11001E_005E,house$B11001E_006E,house$B11001E_008E,house$B11001E_009E)
  totalIslanders=sum(Islandermen,Islanderboys,Islanderwomen,Islandergirls)
  
  OtherRaceboys=sexbyagebyrace[c("B01001F_003E","B01001F_004E","B01001F_005E","B01001F_006E")]
  OtherRaceMen=sexbyagebyrace[c("B01001F_007E","B01001F_008E","B01001F_009E","B01001F_010E","B01001F_011E","B01001F_012E","B01001F_013E","B01001F_014E","B01001F_015E","B01001F_016E")]
  OtherRacegirls=sexbyagebyrace[c("B01001F_018E","B01001F_019E","B01001F_020E","B01001F_021E")]
  OtherRaceWomen=sexbyagebyrace[c("B01001F_022E","B01001F_023E","B01001F_024E","B01001F_025E","B01001F_026E","B01001F_027E","B01001F_028E","B01001F_029E","B01001F_030E","B01001F_031E")]
  OtherRaceH=sum(2*house$B11001F_003E,house$B11001F_005E,house$B11001F_006E,house$B11001F_008E,house$B11001F_009E)
  totalOtherRace=sum(OtherRaceWomen,OtherRacegirls,OtherRaceMen,OtherRaceboys)
  
  MultiRaceboys=sexbyagebyrace[c("B01001G_003E","B01001G_004E","B01001G_005E","B01001G_006E")]
  MultiRaceMen=sexbyagebyrace[c("B01001G_007E","B01001G_008E","B01001G_009E","B01001G_010E","B01001G_011E","B01001G_012E","B01001G_013E","B01001G_014E","B01001G_015E","B01001G_016E")]
  MultiRacegirls=sexbyagebyrace[c("B01001G_018E","B01001G_019E","B01001G_020E","B01001G_021E")]
  MultiRaceWomen=sexbyagebyrace[c("B01001G_022E","B01001G_023E","B01001G_024E","B01001G_025E","B01001G_026E","B01001G_027E","B01001G_028E","B01001G_029E","B01001G_030E","B01001G_031E")]
  MultiRaceH=sum(2*house$B11001G_003E,house$B11001G_005E,house$B11001G_006E,house$B11001G_008E,house$B11001G_009E)
  totalMultiRace=sum(MultiRaceWomen,MultiRaceMen,MultiRaceboys,MultiRacegirls)
  
  Whiteboys=sexbyagebyrace[c("B01001H_003E","B01001H_004E","B01001H_005E","B01001H_006E")]
  WhiteMen=sexbyagebyrace[c("B01001H_007E","B01001H_008E","B01001H_009E","B01001H_010E","B01001H_011E","B01001H_012E","B01001H_013E","B01001H_014E","B01001H_015E","B01001H_016E")]
  Whitegirls=sexbyagebyrace[c("B01001H_018E","B01001H_019E","B01001H_020E","B01001H_021E")]
  WhiteWomen=sexbyagebyrace[c("B01001H_022E","B01001H_023E","B01001H_024E","B01001H_025E","B01001H_026E","B01001H_027E","B01001H_028E","B01001H_029E","B01001H_030E","B01001H_031E")]
  WhiteH=sum(2*house$B11001H_003E,house$B11001H_005E,house$B11001H_006E,house$B11001H_008E,house$B11001H_009E)
  totalWhite=sum(WhiteWomen,Whitegirls,WhiteMen,Whiteboys)
  
  Hispanicboys=sexbyagebyrace[c("B01001I_003E","B01001I_004E","B01001I_005E","B01001I_006E")]
  HispanicMen=sexbyagebyrace[c("B01001I_007E","B01001I_008E","B01001I_009E","B01001I_010E","B01001I_011E","B01001I_012E","B01001I_013E","B01001I_014E","B01001I_015E","B01001I_016E")]
  Hispanicgirls=sexbyagebyrace[c("B01001I_018E","B01001I_019E","B01001I_020E","B01001I_021E")]
  HispanicWomen=sexbyagebyrace[c("B01001I_022E","B01001I_023E","B01001I_024E","B01001I_025E","B01001I_026E","B01001I_027E","B01001I_028E","B01001I_029E","B01001I_030E","B01001I_031E")]
  HispanicH=sum(2*house$B11001I_003E,house$B11001I_005E,house$B11001I_006E,house$B11001I_008E,house$B11001I_009E)
  totalHispanic=sum(HispanicWomen,HispanicMen,Hispanicboys,Hispanicgirls)
  
  all.the.kids=cbind(Blackboys,Blackgirls,AmerIndianAlaskanboys,AmerIndianAlaskangirls,Asianboys,Asiangirls,Islanderboys,Islandergirls,OtherRaceboys,OtherRacegirls,MultiRaceboys,MultiRacegirls,Whiteboys,Whitegirls,Hispanicboys,Hispanicgirls)
  all.the.adult.men=cbind(BlackMen,AmerIndianAlaskanMen,AsianMen,Islandermen,OtherRaceMen,MultiRaceMen,WhiteMen,HispanicMen)
  all.the.adult.women=cbind(BlackWomen,AmerIndianAlaskanWomen,AsianWomen,Islanderwomen,OtherRaceWomen,MultiRaceWomen,WhiteWomen,HispanicWomen)
  totalhouseholders=cbind(BlackH,AmerIndianAlaskanH,AsianH,IslanderH,OtherRaceH,MultiRaceH,WhiteH,HispanicH)
  
  
  #householders and married couples were already created in the function gethouseholdtypeandsize, these people must be adults
  #to make sure adults and children are evenly balanced other members of households are first sampled as either children or adults 
  #with the population reweighted by removing adult householders this is done in the function get NA members
  getsNAmembers <- function(syntheticdataset) {
    
    #sample non householders as either child or adult by removing householders from the population first
    members=ifelse((syntheticdataset$member=="NA"),sample(c("Child","Adult"),size=1,prob=c(sum(all.the.kids)/(sum(all.the.kids,all.the.adult.women,all.the.adult.men)-sum(totalhouseholders)),(sum(all.the.adult.women,all.the.adult.men)-sum(totalhouseholders))/(sum(all.the.adult.men,all.the.adult.women,all.the.kids)-sum(totalhouseholders)))),
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
    age=ifelse((syntheticdataset$sexbyagecode=="B01001B_003E"|syntheticdataset$sexbyagecode=="B01001C_003E"|syntheticdataset$sexbyagecode=="B01001D_003E"|syntheticdataset$sexbyagecode=="B01001E_003E"|syntheticdataset$sexbyagecode=="B01001F_003E"|syntheticdataset$sexbyagecode=="B01001G_003E"|syntheticdataset$sexbyagecode=="B01001H_003E"|syntheticdataset$sexbyagecode=="B01001I_003E"|syntheticdataset$sexbyagecode=="B01001B_018E"|syntheticdataset$sexbyagecode=="B01001C_018E"|syntheticdataset$sexbyagecode=="B01001D_018E"|syntheticdataset$sexbyagecode=="B01001E_018E"|syntheticdataset$sexbyagecode=="B01001F_018E"|syntheticdataset$sexbyagecode=="B01001G_018E"|syntheticdataset$sexbyagecode=="B01001H_018E"|syntheticdataset$sexbyagecode=="B01001I_018E"),"Under 5",
               ifelse((syntheticdataset$sexbyagecode=="B01001B_004E"|syntheticdataset$sexbyagecode=="B01001C_004E"|syntheticdataset$sexbyagecode=="B01001D_004E"|syntheticdataset$sexbyagecode=="B01001E_004E"|syntheticdataset$sexbyagecode=="B01001F_004E"|syntheticdataset$sexbyagecode=="B01001G_004E"|syntheticdataset$sexbyagecode=="B01001H_004E"|syntheticdataset$sexbyagecode=="B01001I_004E"|syntheticdataset$sexbyagecode=="B01001B_019E"|syntheticdataset$sexbyagecode=="B01001C_019E"|syntheticdataset$sexbyagecode=="B01001D_019E"|syntheticdataset$sexbyagecode=="B01001E_019E"|syntheticdataset$sexbyagecode=="B01001F_019E"|syntheticdataset$sexbyagecode=="B01001G_019E"|syntheticdataset$sexbyagecode=="B01001H_019E"|syntheticdataset$sexbyagecode=="B01001I_019E"),"5 to 9",
                      ifelse((syntheticdataset$sexbyagecode=="B01001B_005E"|syntheticdataset$sexbyagecode=="B01001C_005E"|syntheticdataset$sexbyagecode=="B01001D_005E"|syntheticdataset$sexbyagecode=="B01001E_005E"|syntheticdataset$sexbyagecode=="B01001F_005E"|syntheticdataset$sexbyagecode=="B01001G_005E"|syntheticdataset$sexbyagecode=="B01001H_005E"|syntheticdataset$sexbyagecode=="B01001I_005E"|syntheticdataset$sexbyagecode=="B01001B_020E"|syntheticdataset$sexbyagecode=="B01001C_020E"|syntheticdataset$sexbyagecode=="B01001D_020E"|syntheticdataset$sexbyagecode=="B01001E_020E"|syntheticdataset$sexbyagecode=="B01001F_020E"|syntheticdataset$sexbyagecode=="B01001G_020E"|syntheticdataset$sexbyagecode=="B01001H_020E"|syntheticdataset$sexbyagecode=="B01001I_020E"),"10 to 14",
                             ifelse((syntheticdataset$sexbyagecode=="B01001B_006E"|syntheticdataset$sexbyagecode=="B01001C_006E"|syntheticdataset$sexbyagecode=="B01001D_006E"|syntheticdataset$sexbyagecode=="B01001E_006E"|syntheticdataset$sexbyagecode=="B01001F_006E"|syntheticdataset$sexbyagecode=="B01001G_006E"|syntheticdataset$sexbyagecode=="B01001H_006E"|syntheticdataset$sexbyagecode=="B01001I_006E"|syntheticdataset$sexbyagecode=="B01001B_021E"|syntheticdataset$sexbyagecode=="B01001C_021E"|syntheticdataset$sexbyagecode=="B01001D_021E"|syntheticdataset$sexbyagecode=="B01001E_021E"|syntheticdataset$sexbyagecode=="B01001F_021E"|syntheticdataset$sexbyagecode=="B01001G_021E"|syntheticdataset$sexbyagecode=="B01001H_021E"|syntheticdataset$sexbyagecode=="B01001I_021E"),"15 to 17",
                                    ifelse((syntheticdataset$sexbyagecode=="B01001B_007E"|syntheticdataset$sexbyagecode=="B01001C_007E"|syntheticdataset$sexbyagecode=="B01001D_007E"|syntheticdataset$sexbyagecode=="B01001E_007E"|syntheticdataset$sexbyagecode=="B01001F_007E"|syntheticdataset$sexbyagecode=="B01001G_007E"|syntheticdataset$sexbyagecode=="B01001H_007E"|syntheticdataset$sexbyagecode=="B01001I_007E"|syntheticdataset$sexbyagecode=="B01001B_022E"|syntheticdataset$sexbyagecode=="B01001C_022E"|syntheticdataset$sexbyagecode=="B01001D_022E"|syntheticdataset$sexbyagecode=="B01001E_022E"|syntheticdataset$sexbyagecode=="B01001F_022E"|syntheticdataset$sexbyagecode=="B01001G_022E"|syntheticdataset$sexbyagecode=="B01001H_022E"|syntheticdataset$sexbyagecode=="B01001I_022E"),"18 to 19",
                                           ifelse((syntheticdataset$sexbyagecode=="B01001B_008E"|syntheticdataset$sexbyagecode=="B01001C_008E"|syntheticdataset$sexbyagecode=="B01001D_008E"|syntheticdataset$sexbyagecode=="B01001E_008E"|syntheticdataset$sexbyagecode=="B01001F_008E"|syntheticdataset$sexbyagecode=="B01001G_008E"|syntheticdataset$sexbyagecode=="B01001H_008E"|syntheticdataset$sexbyagecode=="B01001I_008E"|syntheticdataset$sexbyagecode=="B01001B_023E"|syntheticdataset$sexbyagecode=="B01001C_023E"|syntheticdataset$sexbyagecode=="B01001D_023E"|syntheticdataset$sexbyagecode=="B01001E_023E"|syntheticdataset$sexbyagecode=="B01001F_023E"|syntheticdataset$sexbyagecode=="B01001G_023E"|syntheticdataset$sexbyagecode=="B01001H_023E"|syntheticdataset$sexbyagecode=="B01001I_023E"),"20 to 24",
                                                  ifelse((syntheticdataset$sexbyagecode=="B01001B_009E"|syntheticdataset$sexbyagecode=="B01001C_009E"|syntheticdataset$sexbyagecode=="B01001D_009E"|syntheticdataset$sexbyagecode=="B01001E_009E"|syntheticdataset$sexbyagecode=="B01001F_009E"|syntheticdataset$sexbyagecode=="B01001G_009E"|syntheticdataset$sexbyagecode=="B01001H_009E"|syntheticdataset$sexbyagecode=="B01001I_009E"|syntheticdataset$sexbyagecode=="B01001B_024E"|syntheticdataset$sexbyagecode=="B01001C_024E"|syntheticdataset$sexbyagecode=="B01001D_024E"|syntheticdataset$sexbyagecode=="B01001E_024E"|syntheticdataset$sexbyagecode=="B01001F_024E"|syntheticdataset$sexbyagecode=="B01001G_024E"|syntheticdataset$sexbyagecode=="B01001H_024E"|syntheticdataset$sexbyagecode=="B01001I_024E"),"25 to 29",
                                                         ifelse((syntheticdataset$sexbyagecode=="B01001B_010E"|syntheticdataset$sexbyagecode=="B01001C_010E"|syntheticdataset$sexbyagecode=="B01001D_010E"|syntheticdataset$sexbyagecode=="B01001E_010E"|syntheticdataset$sexbyagecode=="B01001F_010E"|syntheticdataset$sexbyagecode=="B01001G_010E"|syntheticdataset$sexbyagecode=="B01001H_010E"|syntheticdataset$sexbyagecode=="B01001I_010E"|syntheticdataset$sexbyagecode=="B01001B_025E"|syntheticdataset$sexbyagecode=="B01001C_025E"|syntheticdataset$sexbyagecode=="B01001D_025E"|syntheticdataset$sexbyagecode=="B01001E_025E"|syntheticdataset$sexbyagecode=="B01001F_025E"|syntheticdataset$sexbyagecode=="B01001G_025E"|syntheticdataset$sexbyagecode=="B01001H_025E"|syntheticdataset$sexbyagecode=="B01001I_025E"),"30 to 34",
                                                                ifelse((syntheticdataset$sexbyagecode=="B01001B_011E"|syntheticdataset$sexbyagecode=="B01001C_011E"|syntheticdataset$sexbyagecode=="B01001D_011E"|syntheticdataset$sexbyagecode=="B01001E_011E"|syntheticdataset$sexbyagecode=="B01001F_011E"|syntheticdataset$sexbyagecode=="B01001G_011E"|syntheticdataset$sexbyagecode=="B01001H_011E"|syntheticdataset$sexbyagecode=="B01001I_011E"|syntheticdataset$sexbyagecode=="B01001B_026E"|syntheticdataset$sexbyagecode=="B01001C_026E"|syntheticdataset$sexbyagecode=="B01001D_026E"|syntheticdataset$sexbyagecode=="B01001E_026E"|syntheticdataset$sexbyagecode=="B01001F_026E"|syntheticdataset$sexbyagecode=="B01001G_026E"|syntheticdataset$sexbyagecode=="B01001H_026E"|syntheticdataset$sexbyagecode=="B01001I_026E"),"35 to 44",
                                                                       ifelse((syntheticdataset$sexbyagecode=="B01001B_012E"|syntheticdataset$sexbyagecode=="B01001C_012E"|syntheticdataset$sexbyagecode=="B01001D_012E"|syntheticdataset$sexbyagecode=="B01001E_012E"|syntheticdataset$sexbyagecode=="B01001F_012E"|syntheticdataset$sexbyagecode=="B01001G_012E"|syntheticdataset$sexbyagecode=="B01001H_012E"|syntheticdataset$sexbyagecode=="B01001I_012E"|syntheticdataset$sexbyagecode=="B01001B_027E"|syntheticdataset$sexbyagecode=="B01001C_027E"|syntheticdataset$sexbyagecode=="B01001D_027E"|syntheticdataset$sexbyagecode=="B01001E_027E"|syntheticdataset$sexbyagecode=="B01001F_027E"|syntheticdataset$sexbyagecode=="B01001G_027E"|syntheticdataset$sexbyagecode=="B01001H_027E"|syntheticdataset$sexbyagecode=="B01001I_027E"),"45 to 54",
                                                                              ifelse((syntheticdataset$sexbyagecode=="B01001B_013E"|syntheticdataset$sexbyagecode=="B01001C_013E"|syntheticdataset$sexbyagecode=="B01001D_013E"|syntheticdataset$sexbyagecode=="B01001E_013E"|syntheticdataset$sexbyagecode=="B01001F_013E"|syntheticdataset$sexbyagecode=="B01001G_013E"|syntheticdataset$sexbyagecode=="B01001H_013E"|syntheticdataset$sexbyagecode=="B01001I_013E"|syntheticdataset$sexbyagecode=="B01001B_028E"|syntheticdataset$sexbyagecode=="B01001C_028E"|syntheticdataset$sexbyagecode=="B01001D_028E"|syntheticdataset$sexbyagecode=="B01001E_028E"|syntheticdataset$sexbyagecode=="B01001F_028E"|syntheticdataset$sexbyagecode=="B01001G_028E"|syntheticdataset$sexbyagecode=="B01001H_028E"|syntheticdataset$sexbyagecode=="B01001I_028E"),"55 to 64",
                                                                                     ifelse((syntheticdataset$sexbyagecode=="B01001B_014E"|syntheticdataset$sexbyagecode=="B01001C_014E"|syntheticdataset$sexbyagecode=="B01001D_014E"|syntheticdataset$sexbyagecode=="B01001E_014E"|syntheticdataset$sexbyagecode=="B01001F_014E"|syntheticdataset$sexbyagecode=="B01001G_014E"|syntheticdataset$sexbyagecode=="B01001H_014E"|syntheticdataset$sexbyagecode=="B01001I_014E"|syntheticdataset$sexbyagecode=="B01001B_029E"|syntheticdataset$sexbyagecode=="B01001C_029E"|syntheticdataset$sexbyagecode=="B01001D_029E"|syntheticdataset$sexbyagecode=="B01001E_029E"|syntheticdataset$sexbyagecode=="B01001F_029E"|syntheticdataset$sexbyagecode=="B01001G_029E"|syntheticdataset$sexbyagecode=="B01001H_029E"|syntheticdataset$sexbyagecode=="B01001I_029E"),"65 to 74",
                                                                                            ifelse((syntheticdataset$sexbyagecode=="B01001B_015E"|syntheticdataset$sexbyagecode=="B01001C_015E"|syntheticdataset$sexbyagecode=="B01001D_015E"|syntheticdataset$sexbyagecode=="B01001E_015E"|syntheticdataset$sexbyagecode=="B01001F_015E"|syntheticdataset$sexbyagecode=="B01001G_015E"|syntheticdataset$sexbyagecode=="B01001H_015E"|syntheticdataset$sexbyagecode=="B01001I_015E"|syntheticdataset$sexbyagecode=="B01001B_030E"|syntheticdataset$sexbyagecode=="B01001C_030E"|syntheticdataset$sexbyagecode=="B01001D_030E"|syntheticdataset$sexbyagecode=="B01001E_030E"|syntheticdataset$sexbyagecode=="B01001F_030E"|syntheticdataset$sexbyagecode=="B01001G_030E"|syntheticdataset$sexbyagecode=="B01001H_030E"|syntheticdataset$sexbyagecode=="B01001I_030E"),"75 to 84",
                                                                                                   ifelse((syntheticdataset$sexbyagecode=="B01001B_016E"|syntheticdataset$sexbyagecode=="B01001C_016E"|syntheticdataset$sexbyagecode=="B01001D_016E"|syntheticdataset$sexbyagecode=="B01001E_016E"|syntheticdataset$sexbyagecode=="B01001F_016E"|syntheticdataset$sexbyagecode=="B01001G_016E"|syntheticdataset$sexbyagecode=="B01001H_016E"|syntheticdataset$sexbyagecode=="B01001I_016E"|syntheticdataset$sexbyagecode=="B01001B_031E"|syntheticdataset$sexbyagecode=="B01001C_031E"|syntheticdataset$sexbyagecode=="B01001D_031E"|syntheticdataset$sexbyagecode=="B01001E_031E"|syntheticdataset$sexbyagecode=="B01001F_031E"|syntheticdataset$sexbyagecode=="B01001G_031E"|syntheticdataset$sexbyagecode=="B01001H_031E"|syntheticdataset$sexbyagecode=="B01001I_031E"),"Over 85",NA))))))))))))))
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
