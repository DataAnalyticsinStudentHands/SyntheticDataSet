#https://medium.com/pew-research-center-decoded/how-to-break-regression-f48230f0ca68

# make the code reproducible by setting a random number seed
set.seed(100)

# When everything works:
N <- 1000
x <- rnorm(N)
y <- .4 * x + rnorm(N)
hist(x)
hist(y)

# Now estimate our model:
summary(lm(y ~ x))

#Call:
#  lm(formula = y ~ x)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.0348 -0.7013  0.0085  0.6212  3.1688 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 	0.003921   0.031039   0.126    0.899    
#x           	0.413415   0.030129  13.722   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.9814 on 998 degrees of freedom
#Multiple R-squared:  0.1587,	Adjusted R-squared:  0.1579 
#F-statistic: 188.3 on 1 and 998 DF,  p-value: < 2.2e-16

# Plot it
library(ggplot2)
qplot(x, y) +
  geom_smooth(method='lm') +
  theme_bw() +
  ggtitle("The Perfect Regression")

#Omitted variable bias
w <- rnorm(N)
x <- .5 * w + rnorm(N)
y <- .4 * x + .3 * w + rnorm(N)

m1 <- lm(y ~ x)
summary (m1) # Omitted variable bias - run with w as part of lm to see


media_consumption_x <- rnorm(N)
voter_turnout_y <- .1 * media_consumption_x + rnorm(N)

# Political interest increases after consuming media and participating, and, 
# in this hypothetical world, does *not* increase media consuption or participation
political_interest_w <- 1.2 * media_consumption_x + .6 * voter_turnout_y + rnorm(N)

cormat <- cor(as.matrix(data.frame(media_consumption_x, voter_turnout_y, political_interest_w)))
round(cormat, 2)

summary(lm(voter_turnout_y ~ media_consumption_x))
summary(lm(voter_turnout_y ~ media_consumption_x + political_interest_w))


N <- 1000

# Let's say that 40% of people in this population are Republicans
republican <- rbinom(N, 1, .4)

# And they consume more media
media_consumption <- .75 * republican + rnorm(N)

# Consuming more media causes a slight leftward shift in policy
# preferences, and Republicans have more right-leaning preferences
policy_prefs <- -.2 * media_consumption + 2 * republican + rnorm(N)

# for easier plotting later
df <- data.frame(media_consumption, policy_prefs, republican)
df$republican = factor(c("non-republican", "republican")[df$republican + 1])

# If we don't condition on being Republican, we'll actually estimate
# that the effect goes in the *opposite* direction
summary(lm(policy_prefs ~ media_consumption))

# Naive plot
qplot(media_consumption, policy_prefs) +
  geom_smooth(method='lm') +
  theme_bw() +
  ggtitle("Naive estimate (Simpson's Paradox)") 


# Condition on being a Republican to get the right estimates
summary(lm(policy_prefs ~ media_consumption + republican))

qplot(media_consumption, policy_prefs, data=df, colour = republican) +
  scale_color_manual(values = c("blue","red")) +
  geom_smooth(method='lm') +
  theme_bw() +
  ggtitle("Conditioning on being a Republican (Simpson's Paradox)")


N <- 1000

# The "Truth"
true_media_consumption <- rnorm(N)
true_vote <- .1 * media_consumption + rnorm(N)

# social desirability bias
social_desirability <- rnorm(N)
#what we actually observe from self reports:
self_report_media_consumption <- true_media_consumption + social_desirability
self_report_vote <- true_vote + social_desirability

summary(lm(self_report_vote ~ self_report_media_consumption))

summary(lm(self_report_vote ~ self_report_media_consumption + social_desirability))

