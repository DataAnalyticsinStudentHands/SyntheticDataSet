#https://www.r-graph-gallery.com/index.html
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/order-rectangles.html
#https://www.data-to-viz.com/graph/dendrogram.html
#https://ggplot2.tidyverse.org/reference/geom_smooth.html
#https://rviews.rstudio.com/2017/12/13/introduction-to-skewness/
#https://cran.r-project.org/web/packages/moments/moments.pdf

library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
#library(forcats)
library(ggalluvial)
library(hrbrthemes)
library(viridis)

#race defined: https://2020census.gov/en/about-questions/2020-census-questions-race.html
#ethnicity defined: https://www.census.gov/acs/www/about/why-we-ask-each-question/ethnicity/

scale_col_fill <- c(discrete=TRUE,option="D",direction = 1)
#start with race and whether they own rent - start with all tracts
sam_build <- sam_race_hh[tract%in%c("411900","310300","312200"),c("tract","race","own_rent","family_role","householder_age","householder_age_9")]
sam_build$tract <- factor(sam_build$tract, levels = c("411900","310300","312200"))
sam_build[,("race"):=case_when(
  race=="A" ~ "White",
  race=="B" ~ "Black",
  race=="D" ~ "Asian",
  race=="F" ~ "Other Race",
  race=="G" ~ "Bi-racial"
)]
sam_build$race <- factor(sam_build$race,levels = c("White","Black","Asian","Other Race","Bi-racial"))
sam_build[,("hh_age"):=str_remove(householder_age,"Householder ")]
sam_build[,("hh_age_9"):=str_remove(householder_age_9,"Householder ")]
sam_build[,("hh_age"):=str_remove(hh_age," years")]
sam_build[,("hh_age_9"):=str_remove(hh_age_9," years")]
sam_build[,("hh_age_9"):=str_remove(hh_age_9," and over")]
sam_build[,("type_family"):=case_when(
  family_role=="Female householder no husband present" ~ "Single F",
  family_role=="Male householder no wife present" ~ "Single M",
  family_role=="Married-couple family" ~ "Married",
  family_role=="Householder not living alone" ~ "Non-family",
  family_role=="Householder living alone" ~ "Alone",
)]
sam_build$type_family <- factor(sam_build$type_family, levels = c("Married","Single M","Single F","Alone","Non-family"))

sam_build_eth <- sam_eth_hh[tract%in%c("411900","310300","312200"),c("tract","ethnicity","own_rent")]
sam_build_eth$tract <- factor(sam_build_eth$tract, levels = c("411900","310300","312200"))
sam_build_eth[,("ethnicity"):=case_when(
  ethnicity=="H" ~ "Anglo",
  ethnicity=="I" ~ "Latinx",
  TRUE ~ "None"
)]
sam_build_eth$ethnicity <- factor(sam_build_eth$ethnicity, levels = c("Anglo","Latinx","None"))

#fit_table_tottract <- as.data.table(table(sam_race_hh$tract))
#fit_table_totage <- as.data.table(table(sam_race_hh$tract,sam_race_hh$age_range_6))
#fit_table_totrace <- as.data.table(table(sam_race_hh$tract,sam_race_hh$race))
#fit_table_toteth <- as.data.table(table(sam_race_hh$tract,sam_eth_hh$ethnicity))
#fit_table_tottract[,("tract_age"):=fit_table_totage[.SD,sum(N*as.numeric(substr(V2,1,2))),on=.(V1)]] 
#fit_table_tottract[,("norm_age"):=tract_age/N]
#fit_table_totrace[,("num_race"):=case_when(V2=="A"~1,
#                                            V2=="B"~2,
#                                            TRUE ~3)]
#fit_table_tottract[,("tract_race"):=fit_table_totrace[.SD,sum(N*num_race),on=.(V1)]] 
#fit_table_tottract[,("norm_race"):=tract_race/N]
#fit_table_toteth[,("num_eth"):=case_when(V2=="H"~1,
#                                           V2=="I"~2,
#                                           TRUE ~3)]
#fit_table_tottract[,("tract_eth"):=fit_table_toteth[.SD,sum(N*num_eth),on=.(V1)]] 
#fit_table_tottract[,("norm_eth"):=tract_race/N]
#fit_table_tottract[,("norms"):=(norm_race*norm_eth)/norm_age] #norm_age/(norm_race*norm_eth)] 
#ggplot(fit_table_tottract[norm_eth<1000],
#       aes(x=reorder(V1,norms),norms)) +
#  geom_point() +
#  geom_smooth()

#first a histogram
ggplot(sam_build,
       aes(x=race,
           fill=race)) + #and add: aes(x=own_rent,fill=race)) #and add: aes(x=race,fill=own_rent))
  geom_histogram(stat="count") +
  stat_count(geom="text", color="white", angle = 90, size=3.5,
           aes(label=..count.., group=race), position=position_stack(vjust=0.5)) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~tract) +
  ggtitle("Sam build: histogram with tract and race")

#and eth
ggplot(sam_build_eth,
       aes(x=ethnicity,
           fill=ethnicity)) +  #and add: aes(x=own_rent,fill=ethnicity)) #and add: aes(x=ethnicity,fill=own_rent))
  geom_histogram(stat="count") +
  stat_count(geom="text", color="white", angle = 90, size=3.5,
             aes(label=..count.., group=ethnicity), position=position_stack(vjust=0.5)) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~tract)+
  ggtitle("Sam build: histogram with tract and ethnicity")

#and as stacked
ggplot(sam_build,
       aes(x=tract,
           fill=ethnicity)) + #and add: aes(x=own_rent,fill=race)) #and add: aes(x=race,fill=own_rent))
  geom_histogram(stat="count") +
  stat_count(geom="text", color="white", angle = 90, size=3.5,
             aes(label=..count.., group=ethnicity), position=position_stack(vjust=0.5)) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  ggtitle("Sam build: stacked histogram with tract and ethnicity")

#grid example
sam_build_eth[,("grid_id"):=sample(1:.N)]
sam_build_eth[grid_id%%30==7,("grid_id7"):=grid_id]
ggplot(sam_build_eth,
       aes(x=tract,y=grid_id7,
           fill=ethnicity)) +
  geom_point(size=2,shape=22) + #http://www.sthda.com/english/wiki/ggplot2-point-shapes
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  ggtitle("Sam build: projected tract and ethnicity")

#gridded_eth <- as.data.table(tract=sam_build_eth$tract,grid_id=sam_build_eth$grid_id)
#put ethnicity back on gridded_eth
#gridded_eth[,("ethnicity"):=sam_build_eth[.SD,ethnicity,on=.(grid_id)]]

#now an alluvial
sam_build[,("race2"):=case_when(
  race=="White" ~ "White",
  race=="Black" ~ "Black",
  TRUE ~ "Other"
)]
sam_build$race2 <- factor(sam_build$race2,levels = c("White","Other","Black"))
ggplot(sam_build,
       aes(
         axis1 = tract, axis2 = race2, axis3 = own_rent)) +
  geom_alluvium(aes(fill=race2), #showing with tract is interesting, too
                width = .5, knot.pos = 0, reverse = TRUE) +
  #guides(fill=FALSE) + #without it prints legend
  geom_stratum(width = 1/8, reverse = TRUE) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = TRUE, angle = 90) +
  scale_x_discrete(breaks = 1:3, labels = c("tract","race","own_rent")) +
  ggtitle("Sam build: alluvial with tract, race and own_rent")

ggplot(sam_build_eth,
       aes(
         axis1 = tract, axis2 = ethnicity, axis3 = own_rent)) +
  geom_alluvium(aes(fill=ethnicity),
                width = .5, knot.pos = 0, reverse = TRUE) +
  #guides(fill=FALSE) + #without it prints legend
  geom_stratum(width = 1/8, reverse = TRUE) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = TRUE, angle = 90) +
  scale_x_continuous(breaks = 1:3, labels = c("tract","ethnicity","own_rent")) +
  ggtitle("Sam build: start with tract, ethnicity and own_rent")

#now do grid
#give folks a number
#sam_build[order(own_rent),("id_own_rent"):=1:.N,by=.(own_rent,race2)]

sam_build[order(race2),("match_id"):=paste0(tract,own_rent, 
                                            as.character(1000000+sample(1:.N))),
                   by = .(tract,own_rent)]
sam_build_eth[order(ethnicity),("match_id"):=paste0(tract,own_rent, 
                                            as.character(1000000+sample(1:.N))),
          by = .(tract,own_rent)]
sam_build[,("ethnicity"):=sam_build_eth[.SD,ethnicity,on=.(match_id)]]

sam_build[tract=="411900",("anglo_number"):=nrow(sam_build_eth[tract=="411900"&ethnicity=="Anglo"])]
sam_build[tract=="310300",("anglo_number"):=nrow(sam_build_eth[tract=="310300"&ethnicity=="Anglo"])]
sam_build[tract=="312200",("anglo_number"):=nrow(sam_build_eth[tract=="312200"&ethnicity=="Anglo"])]
sam_build[tract=="411900",("hisp_number"):=nrow(sam_build_eth[tract=="411900"&ethnicity=="Latinx"])]
sam_build[tract=="310300",("hisp_number"):=nrow(sam_build_eth[tract=="310300"&ethnicity=="Latinx"])]
sam_build[tract=="312200",("hisp_number"):=nrow(sam_build_eth[tract=="312200"&ethnicity=="Latinx"])]
sam_build[tract=="411900",("oth_number"):=nrow(sam_build_eth[tract=="411900"&ethnicity=="Other"])]
sam_build[tract=="310300",("oth_number"):=nrow(sam_build_eth[tract=="310300"&ethnicity=="Other"])]
sam_build[tract=="312200",("oth_number"):=nrow(sam_build_eth[tract=="312200"&ethnicity=="Other"])]
sam_build[,("eth_sampled"):=sample(c(rep("Anglo",as.integer(anglo_number[1])),
                                     rep("Latinx",as.integer(hisp_number[1])),
                                     rep("Other",as.integer(.N-anglo_number[1]-hisp_number[1]))),
                                   #prob = c(rep(1/.N,.N)),
                                   replace = FALSE),by=.(tract)]
sam_build[,("eth_replaced"):=sample(c(rep("Anglo",as.integer(anglo_number[1])),
                                     rep("Latinx",as.integer(hisp_number[1])),
                                     rep("Other",as.integer(.N-anglo_number[1]-hisp_number[1]))),
                                   #prob = c(rep(1/.N,.N)),
                                   replace = TRUE),by=.(tract)]

#redo alluvial with 4
ggplot(sam_build,
       aes(
         axis1 = tract, axis2 = race2, axis3 = ethnicity, axis4 = own_rent)) +
  geom_alluvium(aes(fill=ethnicity), #showing with tract is interesting, too
                width = .5, knot.pos = 0, reverse = TRUE) +
  #guides(fill=FALSE) + #without it prints legend
  geom_stratum(width = 1/8, reverse = TRUE) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = TRUE, angle = 90) +
  scale_x_discrete(breaks = 1:4, labels = c("tract","race","ethnicity","own_rent")) +
  ggtitle("Sam build: alluvial by ethnicity")
#and four by race
ggplot(sam_build,
       aes(
         axis1 = tract, axis2 = race2, axis3 = ethnicity, axis4 = own_rent)) +
  geom_alluvium(aes(fill=race2), #showing with tract is interesting, too
                width = .5, knot.pos = 0, reverse = TRUE) +
  #guides(fill=FALSE) + #without it prints legend
  geom_stratum(width = 1/8, reverse = TRUE) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = TRUE, angle = 90) +
  scale_x_discrete(breaks = 1:4, labels = c("tract","race","ethnicity","own_rent")) +
  ggtitle("Sam build: alluvial by race")
#and longer
ggplot(sam_build,
       aes(
         axis1 = tract, axis2 = race2, axis3 = ethnicity, axis4 = own_rent, axis5=type_family, axis6=hh_age)) +
  geom_alluvium(aes(fill=ethnicity), #showing with tract is interesting, too
                width = .5, knot.pos = 0, reverse = TRUE) +
  #guides(fill=FALSE) + #without it prints legend
  geom_stratum(width = 1/16, reverse = TRUE) +
  scale_fill_viridis(discrete=TRUE,option="D",direction = 1) +
  scale_color_viridis(scale_col_fill) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = TRUE, angle = 90, size=3.5) +
  scale_x_discrete(breaks = 1:6, labels = c("tract","race","ethnicity","own_rent","type_family","hh_age")) +
  ggtitle("Sam build: alluvial with family type and age")






#make a population on paths that are constrained choices, not movements between sets

#make a function call to set different parameters

number_in_population <- 100

#create a vector of that length
pop <- 1:number_in_population
#make that into a data.table for ease
pop_dt <- as.data.table(pop)
#add some characteristics
#bg_dfactor - background determined factor designations - for these purposes, they are mutually exclusive inflection points
#examples include 
#bg_over_dfactor - backround over-determined factor designations - individuals can belong to multiple categories (i.e., white and hispanic)
#bg_netwrk - background network effects - continuous measures of effects of overdetermined paths individuals have been on - different distributions possible, but start with random
#bg_path - background paths individuals have been on - begin as combinations of ordered paths - second representation of bg, above, with causality

#present_dfactor
#present_over_dfactor
#present_netwrk - network is a way of talking about how continuous variables add to create current state 
#present_path - representation of factors as causal trajectory

#composite_path
#segment_path 


#project_goal - outcomes of interest and/or perspective person has on desired outcome
#project_horizon - longer term projection that makes others make sense


#dfactor - representations of each individual


#weighting as a representation of the category projecting forward, not the conditions of a situation

#ee_tracts <- c("311900","310300","310600","310800","311000"

tracts_zip <- HCAD_apts[zip%in%c("77023","77011","77012")]
tracts_zip[,tract:=droplevels(tract)]
school_tracts <- tracts_zip[tract%in%c("310300","310600","310800","311100","311200","311900")]
school_tracts[,tract:=droplevels(tract)]

#trying to get at sense that choice of producing through simple side by side variatio on the geom_smooth 
#i.e., solving the multiple linear equations
#is different from allowing them to resolve to the metric (to what the original is)
#the later gets what we want from the category theory, too
#library(moments)
#x<- rnorm(10)
#moment(x)

#calling it a metric example to explain role of individuals in their tracts, with relations that identify them
#a metric is an opportunity - something that can be expanded into; 
#providing opportunities is what you do when you transform a category in relation to the original metrice
z <- as.integer(100)
y <- 1:z
x <- as.integer(z/10)
w <- as.integer(z/9)
v <- as.integer(z/4)
u <- as.integer(z-(x+w+v))
dt <- as.data.table(y)
dt[,`:=`(factor_a=sample(c(rep("f1",x),rep("f2",w),rep("f3",v),rep("f4",u)),.N), #family
         factor_b=sample(c(rep("g1",x),rep("g2",v),rep("g3",w),rep("g4",u)),.N),
         factor_c=sample(c(rep("g1",x),rep("g2",v),rep("g3",w),rep("g4",u)),.N),
         factor_d=sample(c(rep("g1",x+x),rep("g2",v-x),rep("g3",w),rep("g4",u)),.N),
         factor_e=sample(c(rep("g1",x+w),rep("g2",v-w),rep("g3",w),rep("g4",u)),.N),
         factor_f=sample(c(rep("g1",x+x),rep("g2",v),rep("g3",w-x),rep("g4",u)),.N),
         factor_g=sample(c(rep("h1",v),rep("h2",w),rep("h3",w),rep("h4",u)),.N))]
dt[,`:=`(a=as.numeric(substr(factor_a,2,2)),
         b=as.numeric(substr(factor_b,2,2)),
         c=as.numeric(substr(factor_c,2,2)))]
dt[factor_a=="f4",("sub_a"):=sample(c(rep("f4a",w),rep("f4b",u-w)),.N)] #category only applies to part
dt[factor_a=="f3",("sub_b"):=sample(c(rep("f3b",w),rep("f3a",v-w)),.N)]

#instead of all possible combinations, give all combinations given by the two views on the hypersphere
dt1 <- dt #make more of an example
#although making random, here, idea is that there is a unique assignment for the underlying individuals
dt1[,`:=`(race=sample(c(rep("A",x),rep("B",w),rep("C",v),rep("D",u)),.N),
          ethnicity=sample(c(rep("H",w-x),rep("I",w+x+v),rep("_",u)),.N))]
#now break them up to put them back together again
dt2 <- dt1[,c("factor_a","factor_b","factor_e","sub_a","sub_b","race")]  
dt3 <- dt1[,c("factor_b","factor_c","factor_e","sub_b","ethnicity")]
#sort by common factors and assign id # for match
#join by id - the more common factors, the more likely it is to match
dt2[order(factor_b,factor_e,sub_b),("sort_id"):=paste0(factor_b,"_",seq.int(1:.N))]
dt3[order(factor_b,factor_e,sub_b),("sort_id"):=paste0(factor_b,"_",seq.int(1:.N))]
dt4 <- dt3[dt2,on="sort_id"]
test <- table(dt1$race,dt1$ethnicity)==table(dt4$race,dt4$ethnicity)
length(test[test==TRUE])/length(test)

#we want to show building by race and then building by etchnicity
#in sam - you have age_race, you have relation_hh, you have type_hh, occup_hh
#expanding number of rows, then eliminating where there aren't any matches

#have different bits of skew, too?


p = seq(0,1, length=100)
plot(p, dbeta(p, 100, 100), ylab="density", type ="l", col=4)
lines(p, dbeta(p, 10, 10), type ="l", col=3)
lines(p, dbeta(p, 2, 2), col=2) 
lines(p, dbeta(p, 1, 1), col=1) 
legend(0.7,8, c("Be(100,100)","Be(10,10)","Be(2,2)", "Be(1,1)"),lty=c(1,1,1,1),col=c(4,3,2,1))

plot(p, dbeta(p, 900, 100), ylab="density", type ="l", col=4)
lines(p, dbeta(p, 90, 10), type ="l", col=3)
lines(p, dbeta(p, 30, 70), col=2) 
lines(p, dbeta(p, 3, 7), col=1) 
legend(0.2,30, c("Be(900,100)","Be(90,10)","Be(30,70)", "Be(3,7)"),lty=c(1,1,1,1),col=c(4,3,2,1))
#From these examples you should note the following:
  
#  The distribution is roughly centered on a/(a+b). Actually, it turns out that the mean is exactly a/(a+b). Thus the mean of the distribution is determined by the relative values of a and b.
#The larger the values of a and b, the smaller the variance of the distribution about the mean.
#For moderately large values of a and b the distribution looks visually “kind of normal”, although unlike the normal distribution the Beta distribution is restricted to [0,1].
#The special case a=b=1 is the uniform distribution.


#Values of a,b<1

plot(p, dbeta(p/100, 0.1, 0.1), ylim=c(0,3),ylab="density", type ="l", col=4)
lines(p, dbeta(p, 0.5, 0.5), type ="l", col=3)
lines(p, dbeta(p, 0.1, 0.5), col=2) 
lines(p, dbeta(p, 0.5, 2), col=1) 
legend(0.5,2, c("Be(0.1,0.1)","Be(0.5,0.5)","Be(0.1,0.5)", "Be(0.5,2)"),lty=c(1,1,1,1),col=c(4,3,2,1))


#rbinom (# observations, # trails/observation, probability of success )
rbinom(5,150,.2)
#[1] 29 38 30 36 25

#generating lines vs. finding fits for lines



dframe <- data.frame(x=c(1:100),y=sample(c(1:100),100,replace=FALSE))
dframe <- data.frame(x=c(1:100),y=sample(c(1:100),100,replace=TRUE))
length(unique(dframe$y))
dframe <- data.frame(x=c(1:100),y=sample(c(1:100),100,replace=TRUE))
dframe <- data.frame(w=c(1:100),x=c(1:100),y=sample(c(1:100),100,replace=FALSE),z=sample(c(1:100),100,replace=FALSE),g=sample(c(1:5),100,replace=TRUE))

#monoidal map that keeps the pre-order is structure-preserving (accounts for loss between observation and real / or allows for the solution in a dimensional reduction)
#the monoidal category is what you get when you think about a set of functions that all achieve the same transformation - and 
#how to find the kernel activity that you want to respect
#so - if there are lots of paths that lead to an outcome, in terms of set theory, they can be reduced to a monoidal category.
#if the outcomes are binomial, it's just a categorization problem.
#if the problem is polynomial, you need to expand your sense of a space
#if the problem is projection, you need to think through your function for reduction
#each has a metric in the sense that the structure preservation means that some morphism holds.
#under enrichment in 7 practical applications of cat theory, 2.3 - that Bool-cats are preorders and Cost-cats are metric spaces
#i.e., we should have a way of telling whether a metric space is determined by the categories - knowing what we should be measuring in the SDoH, for example
#thinking about what Andrew means by sampling vs. whole set - working inside to optimize instead of standing outside to characterize

#two ways of thinking about explication or contextualization - always stepping outside...

ggplot(dframe, aes(x,y)) +
  geom_segment(aes(xend=x, yend=abs(y-z)),arrow = arrow(length = unit(0.1,"cm")))

ggplot(dframe, aes(x,y)) +
  geom_raster(aes(fill = w)) #or:  + geom_bin2d()

factor_frame <- data.frame(w=c(rep(1:5,20)),x=c(rep(1:5,20)),y=sample(c(1:5),100,replace=TRUE),z=sample(c(1:10),100,replace=TRUE),g=sample(c(1:5),100,replace=TRUE))
factor_frame$w <- factor(factor_frame$w)
factor_frame$g <- factor(factor_frame$g)
library(hrbrthemes)
library(GGally)
library(viridis)
ggparcoord(factor_frame,
           columns = 1:4, groupColumn = 5,# order = "anyClass",
           scale="std", 
           showPoints = TRUE, 
           title = "Normalize univariately (substract mean & divide by sd)",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    legend.position="none",
    plot.title = element_text(size=13)
  ) +
  xlab("")

#for js, probably use something like D3 sankey - although ECharts and Semiotic have good ones for React? https://medium.com/@mtiller/open-source-plotting-libraries-primarily-for-react-c43cfa4dc90f
library(ggalluvial)
ggplot(dframe,
       aes(y=x,axis1=as.factor(w),axis2=as.factor(y))) +
  geom_alluvium(aes(fill=as.factor(g)),width=1/12) +
  geom_stratum(width=1/12,fill="black",color="grey") +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("w", "y"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Title goes here")

lode_frame <- to_lodes_form(factor_frame,axes = 2:4,id="group")
ggplot(lode_frame,
       aes(x = w,y = x,alluvium=group)) +
  geom_alluvium(aes(fill=g),width=1/5) +
  geom_stratum(width = 1/10,fill="black",color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("w", "x", "y","z"), expand = c(.05, .05, .05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Title")

#Can alluvials on things like education and income follow things like Hispanic and acs_race_codes show how to get to an idea of a metric?
#  That something is really about the individuals, not about the resolution of the factors....
#
#Metrics and Hilbert spaces

ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Survived, axis2 = Sex, axis3 = Class)) +
  geom_alluvium(aes(fill = Class),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  #coord_flip() +
  ggtitle("Titanic survival by class and sex")

#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Waffle%20Chart
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid
#https://ggplot2.tidyverse.org/reference/coord_fixed.html
#https://stackoverflow.com/questions/15367565/creating-hypercube-in-r-for-any-dim-d

#Race	TotalWorkforce	EssentialWorkers	Infected	Deceased	Healthcare	WorkFromHome	
#White	77.7	55	36.7	49.6	94.6	29.9	
#Black	12.3	15	20.1	22.5	90.3	19.7	
#Hispanic	17.6	21	33.3	17.1	82.2	16.2	
#AAPI	6.5	6	4.1	5.3	93.2	37	

#hospitalizations age_adjusted by race/eth: https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/racial-ethnic-minorities.html
#Non-Hispanic American Indian or Alaska Native	Non-Hispanic Black	Hispanic or Latino	Non-Hispanic Asian or Pacific Islander	Non-Hispanic White
#Age-adjusted hospitalization rate per 100k	221.2	178.1	160.7	48.4	40.1

race <- c("White","Black","Hispanic","AAPI")
TotalWorkforce <- c(77.7,12.3,17.6,6.5)
Essential_Workers <- c(55,15,21,6)
Infected <- c(36.7,20.1,33.3,4.1)
Deceased <- c(49.6,22.5,17.1,5.3)
Healthcare <- c(94.6,90.3,82.2,93.2)
WorkFromHome <- c(29.9,19.7,16.2,37)
Hospitalization <- c(.04,.18,.16,.22)

#do some graphs, etc.



#expand


Master <- cbind(TotalWorkforce,Essential_Workers,Infected,Deceased,Healthcare,WorkFromHome)
row.names(Master) <- race

ggplot(as.data.frame(Master),
       aes(weight = Freq,
           axis1 = TotalWorkforce, axis2 = Essential_Workers, axis3 = WorkFromHome)) +
  geom_alluvium(aes(fill = Infected),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("race", "essential_worker", "work_from_home")) +
  #coord_flip() +
  ggtitle("Covid Chart from Brandon")


