library(SASxport)
library(gtools)
library(car)

#Read SAS files in from BRSSR survey
BRSSR2014=read.xport('LLCP2014.XPT')
BRSSR2014=subset(BRSSR2014,BRSSR2014$X.STATE==48 & BRSSR2014$DISPCODE==1100)#Filters by state and completed interviews
BRSSR2014$SCNTMONY=BRSSR2014$SCNTMNY1
BRSSR2014$SCNTMEAL=BRSSR2014$SCNTMEL1

BRSSR2013=read.xport('LLCP2013.XPT')
BRSSR2013=subset(BRSSR2013,BRSSR2013$X.STATE==48 & BRSSR2013$DISPCODE==1100)

BRSSR2012=read.xport('LLCP2012.XPT')
BRSSR2012=subset(BRSSR2012,BRSSR2012$X.STATE==48 & BRSSR2012$DISPCODE==1100)
BRSSR2012$SLEPTIM1=BRSSR2012$SLEPTIME
BRSSR2012$FLUSHOT6=BRSSR2012$FLUSHOT5

BRSSR2011=read.xport('LLCP2011.XPT')
BRSSR2011=subset(BRSSR2011,BRSSR2011$X.STATE==48 & BRSSR2011$DISPCODE==110)
BRSSR2011$SLEPTIM1=BRSSR2011$SLEPTIME
BRSSR2011$FLUSHOT6=BRSSR2011$FLUSHOT5

BRSSR2010=read.xport('CDBRFS10.XPT')
BRSSR2010=subset(BRSSR2010,BRSSR2010$X.STATE==48 & BRSSR2010$DISPCODE==110)
BRSSR2010$SLEPTIM1=BRSSR2010$SLEPTIME
BRSSR2010$ASTHMA3=BRSSR2010$ASTHMA2
#skin cancer isn't a question
#other types of cancer isn't a question
#chronic obstructive pulmonary disease emphysema chronic bronchitis
BRSSR2010$HAVARTH3=BRSSR2010$HAVARTH2
BRSSR2010$ADDEPEV2=BRSSR2010$ADDEPEV
#ever told you had a kidney disease
BRSSR2010$DIABETE3=BRSSR2010$DIABETE2
BRSSR2010$LASTSMK2=BRSSR2010$LASTSMK1
BRSSR2010$ALCDAY5=BRSSR2010$ALCDAY4
BRSSR2010$X.BMI5=BRSSR2010$X.BMI4
BRSSR2010$FLUSHOT6=BRSSR2010$FLUSHOT4
BRSSR2010$HIVTST6=BRSSR2010$HIVTST5

BRSSR2009=read.xport('CDBRFS09.XPT')
BRSSR2009=subset(BRSSR2009,BRSSR2009$X.STATE==48 & BRSSR2009$DISPCODE==110)
BRSSR2009$SLEPTIM1=BRSSR2009$SLEPTIME
BRSSR2009$ASTHMA3=BRSSR2009$ASTHMA2
#skin cancer isn't a question
#other types of cancer isn't a question
#chronic obstructive pulmonary disease emphysema chronic bronchitis
BRSSR2009$HAVARTH3=BRSSR2009$HAVARTH2
#Ever told you had a depressive order isn't a question
#ever told you had kidney disease isn't a question
BRSSR2009$DIABETE3=BRSSR2009$DIABETE2
BRSSR2009$LASTSMK2=BRSSR2009$LASTSMK1
BRSSR2009$ALCDAY5=BRSSR2009$ALCDAY4
BRSSR2009$X.BMI5=BRSSR2009$X.BMI4
BRSSR2009$FLUSHOT6=BRSSR2009$FLUSHOT3
BRSSR2009$HIVTST6=BRSSR2009$HIVTST5

#Bind them all together
roughBRSSR=smartbind(BRSSR2009,BRSSR2010,BRSSR2011,BRSSR2012,BRSSR2013,BRSSR2014)

#Take the columns I want and rename them
BRSSR=data.frame(SEX=roughBRSSR$SEX,RACE=roughBRSSR$RACE,EDUCA=roughBRSSR$EDUCA,INCOME2=roughBRSSR$INCOME2,year=roughBRSSR$IYEAR,general.health=roughBRSSR$GENHLTH,flu.shot=roughBRSSR$FLUSHOT6,exercised.past.30.days=roughBRSSR$EXERANY2,ever.test.for.HIV=roughBRSSR$HIVTST6,diagnosed.with.heart.attack=roughBRSSR$CVDINFR4,diagnosed.with.angina.or.coronary.heart.disease=roughBRSSR$CVDCRHD4,diagnosed.with.stroke=roughBRSSR$CVDSTRK3,diagnosed.with.asthma=roughBRSSR$ASTHMA3,diagnosed.arthritis=roughBRSSR$HAVARTH3,diagnosed.diabetes=roughBRSSR$DIABETE3,limited.activies.because.of.problems=roughBRSSR$QLACTLM2,smoked.at.least.100.cigs=roughBRSSR$SMOKE100,use.of.smokeless.tobacco=roughBRSSR$USENOW3,days.in.last.30.drank.alcohol=roughBRSSR$ALCDAY5,couldnt.see.a.doctor.because.of.cost=roughBRSSR$MEDCOST,last.dr.checkup=roughBRSSR$CHECKUP1,BMI=roughBRSSR$X.BMI5,stringsAsFactors=FALSE)
BRSSR=BRSSR[complete.cases(BRSSR),]
#Variables I wanted but no one answered
#asthma.attacking.last.year=roughBRSSR$ASATTACK,days.needed.ER.asthma.care=roughBRSSR$ASERVIST,race.related.physical.stressors=roughBRSSR$RRPHYSM2,race.related.emotional.stressors=roughBRSSR$RREMTSM2,
#worried.about.paying.rent.mortgage=roughBRSSR$SCNTMONY,worried.about.food.money=roughBRSSR$SCNTMEAL,hours worked,

#Variables with less than 75% measurements
#sleep.hours=roughBRSSR$SLEPTIM1,still.have.asthma=roughBRSSR$ASTHNOW,skin.cancer=roughBRSSR$CHCSCNCR,other.cancer=roughBRSSR$CHCOCNCR,
#chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis=roughBRSSR$CHCCOPD1,diagnosed.depression=roughBRSSR$ADDEPEV2,diagnosed.kidney.disease=roughBRSSR$CHCKIDNY,
#taking.insulin=roughBRSSR$INSULIN,check.blood.glucose=roughBRSSR$BLDSUGAR,diabetes.education=roughBRSSR$DIABEDU,how.often.have.needed.social.emotional.support=roughBRSSR$EMTSUPRT,satisfied.with.life=roughBRSSR$LSATISFY,
#avg.drinks.per.day.last.30=roughBRSSR$AVEDRNK2,binge.drank.many.times=roughBRSSR$DRNK3GE5,interval.since.last.smoked=roughBRSSR$LASTSMK2,
#alcohol=subset(BRSSR,!days.in.last.30.drank.alcohol=="Yes"&!is.na(days.in.last.30.drank.alcohol))
#smoke=subset(BRSSR,smoked.at.least.100.cigs=="Yes"),had.mammogram=roughBRSSR$HADMAM,
#diabetessubset=subset(BRSSR,diagnosed.diabetes=="Yes"),pap.test=roughBRSSR$HADPAP2,smoking.frequency=roughBRSSR$SMOKDAY2,

#Variables not available for every year
#Falls

#Write to csv
write.csv(BRSSR,"BRSSR.csv")
