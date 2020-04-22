#https://www.r-graph-gallery.com/index.html
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/order-rectangles.html
#https://www.data-to-viz.com/graph/dendrogram.html
#https://ggplot2.tidyverse.org/reference/geom_smooth.html
#https://rviews.rstudio.com/2017/12/13/introduction-to-skewness/
#https://cran.r-project.org/web/packages/moments/moments.pdf

#trying to get at sense that choice of producing through simple side by side variatio on the geom_smooth 
#i.e., solving the multiple linear equations
#is different from allowing them to resolve to the metric (to what the original is)
#the later gets what we want from the category theory, too
library(ggplot2)
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

Can alluvials on things like education and income follow things like Hispanic and acs_race_codes show how to get to an idea of a metric?
  That something is really about the individuals, not about the resolution of the factors....

Metrics and Hilbert spaces
