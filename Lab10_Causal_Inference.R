# POLITSC 7551, Fall 2016
# Instructor: Jan Pierskalla
# Code modified 2018, Adam Lauretig
##################################################################
### 2SLS Estimation/Instrumental Variables
##################################################################

library(foreign)
data <- read.dta("mss_repdata.dta")

# The following example is based on:
  # Miguel, Edward, Shanker Satyanath, and Ernest Sergenti. 
  # "Economic Shocks and Civil Conflict: An Instrumental Variables Approach" 
  # Journal of Political Economy Vol. 112, No. 4 (2004): 725-753.
  # Miguel, Edward and Shanker Satyanath. "Re-examining Economic Shocks and 
  # Civil Conflict." American Economics Journal: Applied Economics Vol. 3 
  # (2011): 228-232.

# This is not an exact replication of the models used in the paper. Instead, 
# we are looking at a simplified example.

# Suppose we want to know whether economic growth is related to civil war onset
# (in Africa). We could estimate a basic probit model:

mod1_probit <- glm(any_prio ~ gdp_g + gdp_g_l + polity2 + ethfrac + relfrac + 
  Oil + lmtnest + lpopl1 + y_0, family = binomial(link=probit), 
  data = data, na.action = na.omit)

summary(mod1_probit)

# We have reason to be concerned that economic growth is endogenous to conflict.
# That is, GDP is correlated with both conflict and the error term. We can use 
# variation in annual rainfall as an instrumental variable for GDP growth.

# First, predict economic growth as a function of rainfall (and other variables):
stage1 <- lm(gdp_g ~ GPCP_g + GPCP_g_l + GPCP_g_fl  + y_0 + polity2 + ethfrac 
  + relfrac + Oil + lmtnest + lpopl1, data = data, na.action = na.exclude)

# Use na.exclude to get the correct length of the fitted values vector

summary(stage1)

# Next, store the predicted values of gdp growth from this first stage to use 
# in the second stage.

stage1.pred <- fitted(stage1)

# Finally, regress conflict on stage 1 fitted values (and controls):
stage2 <- lm(any_prio ~ stage1.pred  + polity2 + ethfrac + relfrac + Oil + 
  lmtnest + lpopl1 + y_0, data = data, na.action = na.exclude)

summary(stage2)

# CAUTION: Your standard errors will have to be adjusted to reflect the two stage
# procedure. The following approach will do that automatically for you.

# Alternatively, use the AER package:
install.packages("AER")
library(AER)

mod2_iv <- ivreg(any_prio ~ gdp_g + polity2 + ethfrac + relfrac + Oil + 
                   lmtnest + lpopl1 + y_0 
                 | polity2 + ethfrac + relfrac + Oil # divider between stage 1 and stage 2
                 + lmtnest + lpopl1 + y_0 + GPCP_g + GPCP_g_l + GPCP_g_fl, 
                 data = data, na.action = na.exclude)

summary(mod2_iv)
# Note that the coefficient for gdp_g (GDP growth) is the same as that obtained
# using the fitted values in the previous example


##################################################################
### Matching
##################################################################
library(foreign)
data2 <- read.dta("Artillery_RepData.dta")

# The following example is based on Lyall, Jason, "Does Indiscriminate 
# Violence Incite Insurgent Attacks?: Evidence from Chechnya," 
# Journal of Conflict Resolution Vol. 53, No. 3 (2009): 331-362.

# Again, this is not an exact replication, but an example based on the paper.

# Treatment group: 73 Chechen villages struck at least once by Russian 
# artillery fire.
# Control group: 74 villages never struck in the study period

# Use Matchit package to match on pre-treatment covariates
install.packages("MatchIt")
install.packages("rgenoud")

library(MatchIt)
library(rgenoud)
library(Zelig)


# Remove extra variables with lots of missing values from the dataset and 
# make sure all cases are complete:
data2 <- data2[, c(1:26, 41)]
data2 <- data2[complete.cases(data2),]
# Note that deletion is not always a great way to deal with missing values.....



matched <- matchit(treat ~ lpop2000 + poverty + tariq + lelev + iso + lnn 
                   + garrison + reb, data = data2, method = "genetic")
  # The genetic matching approach uses an algorithm to achieve optimal 
  # post-matching covariate balance. The package also allows a variety of 
  # other options.

# Check balance:
summary(matched)
plot(matched)
# ^ "This graph plots covariate values that fall in (approximately) the same
# quantile of treated and control distributions. Control unit quantile values
# are plotted on the x-axis, and treated unit quantile values are plotted on the
# y-axis. If values fall below the 45 degree line, control units generally take
# lower values of the covariate. Data points that fall exactly on the 45 degree
# line indicate that the marginal distributions are identical." (MatchIt manual)
# http://r.iq.harvard.edu/docs/matchit/2.4-20/matchit.pdf

# Use matched data to find causal effect:
matched_data <- match.data(matched)
# match.data function returns subset of original data (processed with matchit
# command in code above), with only the matched units included
library(Zelig)
matched.model <- Zelig::zelig(diff ~ treat, model = "ls", data = matched_data)
treat0 <- Zelig::setx(matched.model, treat = 0)
treat1 <- Zelig::setx(matched.model, treat = 1)
effect <- Zelig::sim(matched.model, x = treat0, x1 = treat1)

summary(effect)
# The effect of interest is the First differences
# simulated first differences (fd): expected values given x1 (treatment) minus
# expected values given x (control)

# For a non-simulation approach:
model <- lm(diff ~ treat, data = matched.data)
summary(model)

# an alternate approach to matching
install.packages("CBPS")
library(CBPS)
