# POLITSC 7551, Fall 2015
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Part of the script draws on code from Jamie Monogan
# modified Fall 2018 by Adam Lauretig

# More estimation
##########################################################
############## Diagnostics
##########################################################

library(foreign)
vote <- read.dta("votegdp.dta")
summary(vote)

lm1 <- lm(vote~q2gdp,data=vote)
summary(lm1)

hist(lm1$residuals)
plot(vote$q2gdp,lm1$residuals)
plot(lm1)
# ^ this plot command produces four plots by default: 
# 1) residuals vs. fitted values (looking for patterns in the residuals to 
# assess non-linearity - we want to see evenly spread residuals around the 
# horizontal line at zero)
# 2) Q-Q plot (assessing normality of residuals; we want observations to be
# closely clustered around the 45 degree line in the plot)
# 3) scale-location plot (again assessing homoskedasticity, looking for
# patterns in the residuals)
# 4) residuals vs. leverage / Cook's distance (influence of each observation
# on the regression coefficients; looking for observations in lower and upper
# right corners, outside of cook's distance lines)
# http://data.library.virginia.edu/diagnostic-plots/

library(car)
# Breusch-Pagan Test for Non-Constant Error Variance
# Null hypothesis = homoskedasticity
ncvTest(lm1)
qqPlot(lm1)

library(lmtest)
library(sandwich)
# dealing with heteroskedasticity
# gives you heteroskedasticity-corrected covariance matrix
coeftest(lm1,vcovHC(lm1,type="HC3"))
# compare to results from summary of the model:
summary(lm1)


##########################################################
############## Categorical Variables
##########################################################
#poll <- read.dta("~/Dropbox/OSUClasses/Quant_I/Data/cnn-poll.dta")
poll <- read.dta("cnn-poll.dta")

summary(poll)

# create numerical variables
poll$party3 <- as.numeric(poll$party3)
poll$hcopinion <- as.numeric(poll$hcopinion)

# code dummies for each category
poll$white <- ifelse(poll$race1=="White/Caucasian", 1, 0)
poll$black <- ifelse(poll$race1=="Black/African-American", 1, 0)
poll$asian <- ifelse(poll$race1=="Asian/Asian-American", 1, 0)

lm1 <- lm(hcopinion ~ party3 + white + black + asian, data=poll)
summary(lm1)

plot(poll$party3, poll$hcopinion, xlab="Party ID", ylab="Support", main="", type="n")
abline(a=(0.45+0.004),b=0.887571,col="darkolivegreen")
abline(a=(0.45+0.495689),b=0.887571,col="burlywood3")
abline(a=(0.45+0.637877),b=0.887571,col="coral3")
legend(2.5,2,legend=c("White","Black","Asian"),col=c("darkolivegreen","burlywood3","coral3"),lty=1)

# Use ordered response model instead, DV is not normally distributed
library(MASS)

poll$hcopinion_fact <-  as.factor(poll$hcopinion)

model2<- polr(hcopinion_fact ~ party3 + white + black + asian, data=poll, 
              method = "probit")
summary(model2)

### Addition: November 2016
poll$college_dum <- ifelse(poll$college=="College",1,0)

poll$hcdummy <- ifelse(as.numeric(poll$hcopinion) < 3, 0, 1)
poll$hcdummy <- ifelse(is.na(poll$hcopinion), NA, poll$hcdummy)

# use factor command to treat different levels of categorical variable as dummies
logit1 <- glm(hcdummy ~ factor(party3) + college_dum, family="binomial", data=poll)
summary(logit1)

##########################################################
############## Interactions
##########################################################
poll$college_dum <- ifelse(poll$college=="College",1,0)

poll$party3 <- as.numeric(poll$party3)
poll$hcopinion <- as.numeric(poll$hcopinion)

intmodel <- lm(hcopinion ~ party3 + college_dum + party3*college_dum, data=poll)
summary(intmodel)
install.packages("interplot")
library(interplot)

interplot(m=intmodel, var1='college_dum', var2='party3', sims=1000) + 
  xlab('Party ID') + 
  ylab('Estimated Coefficient for College Dummy')

# Briefly going back to the voting data from earlier
head(vote)
intmodel2 <- glm(term ~ q2gdp*vote, data=vote, family="binomial")
summary(intmodel2)
# ^ notice that the constitutive terms of the interaction term are included automatically
interplot(intmodel2, 'q2gdp', 'vote', sims=5000) + 
  xlab('Vote') + 
  ylab('Estimated Coefficient for GDP')

# Plotting separate regression lines for dummy=0 and dummy=1
# Create data subsets
subset1 <- poll[poll$college_dum==0,]
subset2 <- poll[poll$college_dum==1,]
# Estimate separate regressions using subsetted data
intmodel3 <- lm(hcopinion ~ party3 + college_dum + party3*college_dum, data=subset1)
intmodel4 <- lm(hcopinion ~ party3 + college_dum + party3*college_dum, data=subset2)
summary(intmodel3)
summary(intmodel4)
# ^ notice the variables that get dropped in this model
plot(poll$party3, poll$hcopinion, xlab="Party ID", ylab="Support", main="", type="n")
abline(intmodel3, col="magenta")
abline(intmodel4, col="blue")


# Alternative 1: use "effect" package
install.packages("effects")
library(effects)
effects(intmodel)
plot(allEffects(intmodel))
plot(effect("party3", intmodel))

# Alternative 2: use "margins" package
install.packages("margins")
library(margins)

marginal_effects(intmodel)
head(marginal_effects(intmodel))

dydx(poll, intmodel, "party3")
dydx(poll, intmodel, "college_dum")

cplot(intmodel)

cplot(intmodel, x="party3", what = "prediction")
cplot(intmodel, "college_dum", what = "prediction")

cplot(intmodel, x="party3", what = "effect")
cplot(intmodel, "college_dum", what = "effect")

# 3-dimentional representation
persp(intmodel, xvar = "party3", yvar = "college_dum", what = "prediction")
persp(intmodel, xvar = "party3", yvar = "college_dum", what = "effect")


# heatmap-style representation
image(intmodel, xvar = "party3", yvar = "college_dum", what = "prediction")



### Bootstrapping
# We know that repeated sampling will cause our sample mean to converge to the 
# population mean, and to be normally distributed. This is *good* since much of 
# modern statistical theory used in regression is built around this assumption.
# If we're not sure we have enough data to provide good estimates of our 
# standard errors we can repeatedly sample our data, average our estimates, and 
# calculate standard errors from those estimates.
library(data.table)
logit1 <- glm(hcdummy ~ factor(party3) + college_dum, family="binomial", data=poll)
summary(logit1)

# we'll write a function to do our bootstrap estimates.

bootstrap_glm <- function(i, data_to_use = poll){
  set.seed(i)
  n_obs <- nrow(data_to_use)
  
  rows_to_sample <- sample(x = 1:n_obs, size = n_obs, replace = TRUE)
  boot_data <- data_to_use[rows_to_sample, ]
  logit_boot <- glm(hcdummy ~ factor(party3) + college_dum, 
    family="binomial", data=boot_data)
  data.table(t(coef(logit_boot)))
}
bootstrap_list <- lapply(1:500, bootstrap_glm, data_to_use = poll)
bootstrap_out <- rbindlist(bootstrap_list)
bootstrap_out

colMeans(bootstrap_out)
apply(X = bootstrap_out, MARGIN = 2, FUN = quantile, probs = c(.025, .975))






#########################################
### Maximum Likelihood Estimation
#########################################

##From Jamie Monogan: SECTION 11.5: OPTIMIZATION AND MAXIMUM LIKELIHOOD ESTIMATION##


#define log-likelihood function

binomial.loglikelihood <- function(prob, y, n) {
  loglikelihood <- y*log(prob) + (n-y)*log(1-prob)
  return(loglikelihood)
}

#find the maximum likelihood estimate using 'optim'
test <- optim(c(.5),            # starting value for prob
              binomial.loglikelihood,      # the log-likelihood function
              method="BFGS",               # optimization method
              hessian=TRUE,                # return numerical Hessian
              control=list(fnscale=-1),    # maximize instead of minimize
              y=43, n=100)                 # the data
print(test)

#standard error of probability parameter
sqrt(diag(solve(-test$hessian)))

#plot the log-likelihood function against possible values of the probability parameter
ruler <- seq(0,1,0.01)
loglikelihood <- binomial.loglikelihood(ruler, y=43, n=100)
plot(ruler, loglikelihood, type="l", lwd=2, col="blue",xlab=expression(pi),ylab="Log-Likelihood",ylim=c(-300,-70),main="Log-Likelihood for Binomial Model")
abline(v=.43)

# Using GLM Package
# Example: estimating a logit model
# What is the impact of democracy on civil war?

# Read in Data on Civil War:
library(foreign)
civil <- read.dta("repdata.dta")

# Create model:
civil.model <- glm(war~polity2, family = binomial, data = civil)

summary(civil.model)

confint(civil.model)

exp(coef(civil.model))
exp(confint(civil.model))

# likelihood ratio test: compare goodness of fit
library(lmtest)

civil.model2 <- glm(war ~ polity2 + western, 
                    family = binomial, data = civil)

lrtest(civil.model, civil.model2)
