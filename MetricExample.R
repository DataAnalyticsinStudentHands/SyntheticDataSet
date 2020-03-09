#https://www.r-graph-gallery.com/index.html
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/order-rectangles.html
#https://www.data-to-viz.com/graph/dendrogram.html
#https://ggplot2.tidyverse.org/reference/geom_smooth.html
#https://rviews.rstudio.com/2017/12/13/introduction-to-skewness/

#trying to get at sense that choice of producing through simple side by side variatio on the geom_smooth 
#i.e., solving the multiple linear equations
#is different from allowing them to resolve to the metric (to what the original is)
#the later gets what we want from the category theory, too
library(ggplot2)

#have different bits of skew, too?

#generating lines vs. finding fits for lines

dframe <- data.frame(x=c(1:100),y=sample(c(1:100),100,replace=FALSE))
dframe <- data.frame(x=c(1:100),y=sample(c(1:100),100,replace=TRUE))
length(unique(dframe$y))
dframe <- data.frame(x=c(1:100),y=sample(c(1:100),100,replace=TRUE))
dframe <- data.frame(w=c(1:100),x=c(1:100),y=sample(c(1:100),100,replace=FALSE),z=sample(c(1:100),100,replace=FALSE),g=sample(c(1:5),100,replace=TRUE))

#monoidal map that keeps the pre-order is structure-preserving (accounts for loss between observation and real / or allows for the solution in a dimensional reduction)
#under enrichment in 7 practical applications of cat theory, 2.3 - that Bool-cats are preorders and Cost-cats are metric spaces
#i.e., we should have a way of telling whether a metric space is determined by the categories - knowing what we should be measuring in the SDoH, for example
#thinking about what Andrew means by sampling vs. whole set - working inside to optimize instead of standing outside to characterize

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
lode_frame <- to_lodes_form(factor_frame,axes = 2:4,id="group")
ggplot(lode_frame,
       aes(x = w,y = x,alluvium=group)) +
  geom_alluvium(aes(fill=g),width=1/5) +
  geom_stratum(width = 1/10,fill="black",color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("w", "x", "y","z"), expand = c(.05, .05, .05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Title")


