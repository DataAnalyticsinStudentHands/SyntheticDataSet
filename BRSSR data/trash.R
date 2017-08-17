#Change variables to what they mean
#This is just looking through the codebook and changing the numbers to what they mean

BRSSR$general.health[BRSSR$general.health==1]<-"Excellent"
BRSSR$general.health[BRSSR$general.health==2]<-"Very Good"
BRSSR$general.health[BRSSR$general.health==3]<-"Good"
BRSSR$general.health[BRSSR$general.health==4]<-"Fair"
BRSSR$general.health[BRSSR$general.health==5]<-"Poor"
BRSSR$general.health[BRSSR$general.health==7]<-"Don't know/not sure"
BRSSR$general.health[BRSSR$general.health==9]<-"Refused"

BRSSR$sleep.hours[BRSSR$sleep.hours==77]<-"Don't know/not sure"
BRSSR$sleep.hours[BRSSR$sleep.hours==99]<-"Refused"

BRSSR$diagnosed.with.heart.attack[BRSSR$diagnosed.with.heart.attack==1]<-"Yes"
BRSSR$diagnosed.with.heart.attack[BRSSR$diagnosed.with.heart.attack==2]<-"No"
BRSSR$diagnosed.with.heart.attack[BRSSR$diagnosed.with.heart.attack==7]<-"Don't know/not sure"
BRSSR$diagnosed.with.heart.attack[BRSSR$diagnosed.with.heart.attack==9]<-"Refused"

BRSSR$diagnosed.with.angina.or.coronary.heart.disease[BRSSR$diagnosed.with.angina.or.coronary.heart.disease==1]<-"Yes"
BRSSR$diagnosed.with.angina.or.coronary.heart.disease[BRSSR$diagnosed.with.angina.or.coronary.heart.disease==2]<-"No"
BRSSR$diagnosed.with.angina.or.coronary.heart.disease[BRSSR$diagnosed.with.angina.or.coronary.heart.disease==7]<-"Don't know/not sure"
BRSSR$diagnosed.with.angina.or.coronary.heart.disease[BRSSR$diagnosed.with.angina.or.coronary.heart.disease==9]<-"Refused"

BRSSR$diagnosed.with.stroke[BRSSR$diagnosed.with.stroke==1]<-"Yes"
BRSSR$diagnosed.with.stroke[BRSSR$diagnosed.with.stroke==2]<-"No"
BRSSR$diagnosed.with.stroke[BRSSR$diagnosed.with.stroke==7]<-"Don't know/not sure"
BRSSR$diagnosed.with.stroke[BRSSR$diagnosed.with.stroke==9]<-"Refused"

BRSSR$diagnosed.with.asthma[BRSSR$diagnosed.with.asthma==1]<-"Yes"
BRSSR$diagnosed.with.asthma[BRSSR$diagnosed.with.asthma==2]<-"No"
BRSSR$diagnosed.with.asthma[BRSSR$diagnosed.with.asthma==7]<-"Don't know/not sure"
BRSSR$diagnosed.with.asthma[BRSSR$diagnosed.with.asthma==9]<-"Refused"

BRSSR$exercised.past.30.days[BRSSR$exercised.past.30.days==1]<-"Yes"
BRSSR$exercised.past.30.days[BRSSR$exercised.past.30.days==2]<-"No"
BRSSR$exercised.past.30.days[BRSSR$exercised.past.30.days==7]<-"Don't know/not sure"
BRSSR$exercised.past.30.days[BRSSR$exercised.past.30.days==9]<-"Refused"

BRSSR$still.have.asthma[BRSSR$still.have.asthma==1]<-"Yes"
BRSSR$still.have.asthma[BRSSR$still.have.asthma==2]<-"No"
BRSSR$still.have.asthma[BRSSR$still.have.asthma==7]<-"Don't know/not sure"
BRSSR$still.have.asthma[BRSSR$still.have.asthma==9]<-"Refused"

BRSSR$skin.cancer[BRSSR$skin.cancer==1]<-"Yes"
BRSSR$skin.cancer[BRSSR$skin.cancer==2]<-"No"
BRSSR$skin.cancer[BRSSR$skin.cancer==7]<-"Don't know/not sure"
BRSSR$skin.cancer[BRSSR$skin.cancer==9]<-"Refused"

BRSSR$other.cancer[BRSSR$other.cancer==1]<-"Yes"
BRSSR$other.cancer[BRSSR$other.cancer==2]<-"No"
BRSSR$other.cancer[BRSSR$other.cancer==7]<-"Don't know/not sure"
BRSSR$other.cancer[BRSSR$other.cancer==9]<-"Refused"

BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis[BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis==1]<-"Yes"
BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis[BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis==2]<-"No"
BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis[BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis==7]<-"Don't know/not sure"
BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis[BRSSR$chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis==9]<-"Refused"

BRSSR$diagnosed.arthritis[BRSSR$diagnosed.arthritis==1]<-"Yes"
BRSSR$diagnosed.arthritis[BRSSR$diagnosed.arthritis==2]<-"No"
BRSSR$diagnosed.arthritis[BRSSR$diagnosed.arthritis==7]<-"Don't know/not sure"
BRSSR$diagnosed.arthritis[BRSSR$diagnosed.arthritis==9]<-"Refused"

BRSSR$diagnosed.depression[BRSSR$diagnosed.depression==1]<-"Yes"
BRSSR$diagnosed.depression[BRSSR$diagnosed.depression==2]<-"No"
BRSSR$diagnosed.depression[BRSSR$diagnosed.depression==7]<-"Don't know/not sure"
BRSSR$diagnosed.depression[BRSSR$diagnosed.depression==9]<-"Refused"

BRSSR$diagnosed.kidney.disease[BRSSR$diagnosed.kidney.disease==1]<-"Yes"
BRSSR$diagnosed.kidney.disease[BRSSR$diagnosed.kidney.disease==2]<-"No"
BRSSR$diagnosed.kidney.disease[BRSSR$diagnosed.kidney.disease==7]<-"Don't know/not sure"
BRSSR$diagnosed.kidney.disease[BRSSR$diagnosed.kidney.disease==9]<-"Refused"

BRSSR$diagnosed.diabetes[BRSSR$diagnosed.diabetes==1]<-"Yes"
BRSSR$diagnosed.diabetes[BRSSR$diagnosed.diabetes==2]<-"Yes but only during pregnancy"
BRSSR$diagnosed.diabetes[BRSSR$diagnosed.diabetes==3]<-"No"
BRSSR$diagnosed.diabetes[BRSSR$diagnosed.diabetes==4]<-"No but borderline or prediabetes"
BRSSR$diagnosed.diabetes[BRSSR$diagnosed.diabetes==7]<-"Don't know/not sure"
BRSSR$diagnosed.diabetes[BRSSR$diagnosed.diabetes==9]<-"Refused"

BRSSR$limited.activies.because.of.problems[BRSSR$limited.activies.because.of.problems==1]<-"Yes"
BRSSR$limited.activies.because.of.problems[BRSSR$limited.activies.because.of.problems==2]<-"No"
BRSSR$limited.activies.because.of.problems[BRSSR$limited.activies.because.of.problems==7]<-"Don't know/not sure"
BRSSR$limited.activies.because.of.problems[BRSSR$limited.activies.because.of.problems==9]<-"Refused"

BRSSR$smoked.at.least.100.cigs[BRSSR$smoked.at.least.100.cigs==1]<-"Yes"
BRSSR$smoked.at.least.100.cigs[BRSSR$smoked.at.least.100.cigs==2]<-"No"
BRSSR$smoked.at.least.100.cigs[BRSSR$smoked.at.least.100.cigs==7]<-"Don't know/not sure"
BRSSR$smoked.at.least.100.cigs[BRSSR$smoked.at.least.100.cigs==9]<-"Refused"

BRSSR$smoking.frequency[BRSSR$smoking.frequency==1]<-"Every day"
BRSSR$smoking.frequency[BRSSR$smoking.frequency==2]<-"Some days"
BRSSR$smoking.frequency[BRSSR$smoking.frequency==3]<-"Not at all"
BRSSR$smoking.frequency[BRSSR$smoking.frequency==7]<-"Don't know/not sure"
BRSSR$smoking.frequency[BRSSR$smoking.frequency==9]<-"Refused"

BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==1]<-"Within the past month"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==2]<-"Within the past 3 months"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==3]<-"Within the past 6 months"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==4]<-"Within the past year"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==5]<-"Within the past 5 years"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==6]<-"Within the past 10 years"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==7]<-"10 years or more"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==8]<-"Never Smoked Frequently"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==77]<-"Don't know/not sure"
BRSSR$interval.since.last.smoked[BRSSR$interval.since.last.smoked==1]<-"Refused"

BRSSR$use.of.smokeless.tobacco[BRSSR$use.of.smokeless.tobacco==1]<-"Every day"
BRSSR$use.of.smokeless.tobacco[BRSSR$use.of.smokeless.tobacco==2]<-"Some days"
BRSSR$use.of.smokeless.tobacco[BRSSR$use.of.smokeless.tobacco==3]<-"Not at all"
BRSSR$use.of.smokeless.tobacco[BRSSR$use.of.smokeless.tobacco==7]<-"Don't know/not sure"
BRSSR$use.of.smokeless.tobacco[BRSSR$use.of.smokeless.tobacco==9]<-"Refused"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
BRSSR$number.of.drinks.to.be.null=substrRight(BRSSR$days.in.last.30.drank.alcohol,2)
BRSSR$per.week.per.month.to.be.null=ifelse(BRSSR$days.in.last.30.drank.alcohol>200,"per month","per week")
BRSSR$days.in.last.30.drank.alcohol<-paste(BRSSR$number.of.drinks.to.be.null,BRSSR$per.week.per.month.to.be.null)
BRSSR$days.in.last.30.drank.alcohol[BRSSR$days.in.last.30.drank.alcohol=="NA NA"]<-NA
BRSSR$per.week.per.month.to.be.null<-NULL
BRSSR$number.of.drinks.to.be.null<-NULL

BRSSR$avg.drinks.per.day.last.30[BRSSR$avg.drinks.per.day.last.30==77]<-"Don't know/not sure"
BRSSR$avg.drinks.per.day.last.30[BRSSR$avg.drinks.per.day.last.30==99]<-"Refused"

BRSSR$binge.drank.many.times[BRSSR$binge.drank.many.times==88]<-"None"
BRSSR$binge.drank.many.times[BRSSR$binge.drank.many.times==77]<-"Don't know/not sure"
BRSSR$binge.drank.many.times[BRSSR$binge.drank.many.times==99]<-"Refused"

#BRSSR$asthma.attacking.last.year[BRSSR$asthma.attacking.last.year==1]<-"Yes"
#BRSSR$asthma.attacking.last.year[BRSSR$asthma.attacking.last.year==2]<-"No"
#BRSSR$asthma.attacking.last.year[BRSSR$asthma.attacking.last.year==77]<-"Don't know/Not sure"
#BRSSR$asthma.attacking.last.year[BRSSR$asthma.attacking.last.year==99]<-"Refused"

#BRSSR$days.needed.ER.asthma.care[BRSSR$days.needed.ER.asthma.care==88]<-"None"
#BRSSR$days.needed.ER.asthma.care[BRSSR$days.needed.ER.asthma.care==98]<-"Don't know/not sure"

#BRSSR$race.related.physical.stressors[BRSSR$race.related.physical.stressors==1]<-"Yes"
#BRSSR$race.related.physical.stressors[BRSSR$race.related.physical.stressors==2]<-"No"
#BRSSR$race.related.physical.stressors[BRSSR$race.related.physical.stressors==7]<-"Don't know/Not sure"
#BRSSR$race.related.physical.stressors[BRSSR$race.related.physical.stressors==9]<-"Refused"

#BRSSR$race.related.emotional.stressors[BRSSR$race.related.emotional.stressors==1]<-"Yes"
#BRSSR$race.related.emotional.stressors[BRSSR$race.related.emotional.stressors==2]<-"No"
#BRSSR$race.related.emotional.stressors[BRSSR$race.related.emotional.stressors==7]<-"Don't know/Not sure"
#BRSSR$race.related.emotional.stressors[BRSSR$race.related.emotional.stressors==9]<-"Refused"

#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==1]<-"Always"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==2]<-"Usually"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==3]<-"Sometimes"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==4]<-"Rarely"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==5]<-"Never"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==7]<-"Don't know/Not sure"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==8]<-"Not applicable"
#BRSSR$worried.about.paying.rent.mortgage[BRSSR$worried.about.paying.rent.mortgage==9]<-"Refused"

#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==1]<-"Always"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==2]<-"Usually"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==3]<-"Sometimes"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==4]<-"Rarely"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==5]<-"Never"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==7]<-"Don't know/Not sure"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==8]<-"Not applicable"
#BRSSR$worried.about.food.money[BRSSR$worried.about.food.money==9]<-"Refused"

BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==1]<-"Always"
BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==2]<-"Usually"
BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==3]<-"Sometimes"
BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==4]<-"Rarely"
BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==5]<-"Never"
BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==7]<-"Don't know/Not Sure"
BRSSR$how.often.have.needed.social.emotional.support[BRSSR$how.often.have.needed.social.emotional.support==9]<-"Refused"

BRSSR$satisfied.with.life[BRSSR$satisfied.with.life==1]<-"Very Satisfied"
BRSSR$satisfied.with.life[BRSSR$satisfied.with.life==2]<-"Satisfied"
BRSSR$satisfied.with.life[BRSSR$satisfied.with.life==3]<-"Dissatisfied"
BRSSR$satisfied.with.life[BRSSR$satisfied.with.life==4]<-"Very Dissatisfied"
BRSSR$satisfied.with.life[BRSSR$satisfied.with.life==7]<-"Don't know/Not sure"
BRSSR$satisfied.with.life[BRSSR$satisfied.with.life==9]<-"Refused"

BRSSR$couldnt.see.a.doctor.because.of.cost[BRSSR$couldnt.see.a.doctor.because.of.cost==1]<-"Yes"
BRSSR$couldnt.see.a.doctor.because.of.cost[BRSSR$couldnt.see.a.doctor.because.of.cost==2]<-"No"
BRSSR$couldnt.see.a.doctor.because.of.cost[BRSSR$couldnt.see.a.doctor.because.of.cost==7]<-"Don't Know/Not Sure"
BRSSR$couldnt.see.a.doctor.because.of.cost[BRSSR$couldnt.see.a.doctor.because.of.cost==9]<-"Refused"

BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==1]<-"Within the last year"
BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==2]<-"Within the last 2 years"
BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==3]<-"Within the last 5 years"
BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==4]<-"5 or more years ago"
BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==5]<-"Never"
BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==7]<-"Don't Know/Not Sure"
BRSSR$last.dr.checkup[BRSSR$last.dr.checkup==9]<-"Refused"

BRSSR$flu.shot[BRSSR$flu.shot==1]<-"Yes"
BRSSR$flu.shot[BRSSR$flu.shot==2]<-"No"
BRSSR$flu.shot[BRSSR$flu.shot==7]<-"Don't Know/Not Sure"
BRSSR$flu.shot[BRSSR$flu.shot==9]<-"Refused"

BRSSR$taking.insulin[BRSSR$taking.insulin==1]<-"Yes"
BRSSR$taking.insulin[BRSSR$taking.insulin==2]<-"No"
BRSSR$taking.insulin[BRSSR$taking.insulin==9]<-"Refused"

BRSSR$number.of.times.to.be.null=substrRight(BRSSR$check.blood.glucose,2)
BRSSR$per.to.be.null=ifelse(BRSSR$check.blood.glucose>400,"per year",ifelse(BRSSR$check.blood.glucose>300,"per month",ifelse(BRSSR$check.blood.glucose>200,"per week",ifelse(BRSSR$check.blood.glucose>100,"per day",NA))))
BRSSR$check.blood.glucose<-ifelse(BRSSR$check.blood.glucose==777,"Don't Know/Not Sure",ifelse(BRSSR$check.blood.glucose==888,"Never",ifelse(BRSSR$check.blood.glucose==999,"Refused",paste(BRSSR$number.of.times.to.be.null,BRSSR$per.to.be.null))))
BRSSR$check.blood.glucose[BRSSR$check.blood.glucose=="NA NA"]<-NA
BRSSR$per.to.be.null<-NULL
BRSSR$number.of.times.to.be.null<-NULL

BRSSR$diabetes.education[BRSSR$diabetes.education==1]<-"Yes"
BRSSR$diabetes.education[BRSSR$diabetes.education==2]<-"No"
BRSSR$diabetes.education[BRSSR$diabetes.education==7]<-"Don't Know/Not Sure"
BRSSR$diabetes.education[BRSSR$diabetes.education==9]<-"Refused"

BRSSR$ever.test.for.HIV[BRSSR$ever.test.for.HIV==1]<-"Yes"
BRSSR$ever.test.for.HIV[BRSSR$ever.test.for.HIV==2]<-"No"
BRSSR$ever.test.for.HIV[BRSSR$ever.test.for.HIV==7]<-"Don't Know/Not Sure"
BRSSR$ever.test.for.HIV[BRSSR$ever.test.for.HIV==9]<-"Refused"