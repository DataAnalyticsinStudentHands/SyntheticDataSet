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
