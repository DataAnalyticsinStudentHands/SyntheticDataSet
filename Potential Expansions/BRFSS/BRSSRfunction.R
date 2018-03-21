getBRSSRdata <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=read.csv('BRSSR.csv')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiaan or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  education=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$RACE
  brace=c("2","3","4","5","6","7","1","8")
  #BRSSR$EDUCA
  beducation=c("2 and 3","3","4","5","5","6","6")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8")
  
  finaldataset=data.frame()
  for (indexsex in 1:2){
    for (indexrace in 1:8){
      for (indexedu in 1:7){
        for (indexincome in 1:16){
        
          subsyntheticdataset=syntheticdataset[(syntheticdataset$sex==sex[indexsex])&(syntheticdataset$race==race[indexrace])&(syntheticdataset$education.attainment==education[indexedu])&(syntheticdataset$household.income==income[indexincome]),]
          
          subBRSSR=BRSSR[(BRSSR$SEX==bsex[indexsex])&(BRSSR$RACE==brace[indexrace])&(BRSSR$EDUCA==beducation[indexedu])&(BRSSR$INCOME2==bincome[indexincome])& !is.na(BRSSR$SEX)& !is.na(BRSSR$RACE)& !is.na(BRSSR$EDUCA)& !is.na(BRSSR$INCOME2),]
          if (nrow(subBRSSR)>0){
            subBRSSR$row.id.for.merge=1:nrow(subBRSSR)
            if (nrow(subsyntheticdataset)>0){
              subsyntheticdataset$row.id.for.merge=sample(1:nrow(subBRSSR),nrow(subsyntheticdataset),TRUE)
              subsyntheticdataset=merge(subBRSSR,subsyntheticdataset,by.x="row.id.for.merge",by.y="row.id.for.merge",all=FALSE,ll.x=FALSE,all.y=TRUE)
              subsyntheticdataset=subset(subsyntheticdataset,!is.na(subsyntheticdataset$household.type))
              finaldataset=rbind(finaldataset,subsyntheticdataset)
            }
          }

          
        }
      }
    }
  }
  finaldataset$SEX=NULL
  finaldataset$RACE=NULL
  finaldataset$EDUCA=NULL
  finaldataset$INCOME2=NULL
  finaldataset$row.id.for.merge=NULL
  return(finaldataset)
}