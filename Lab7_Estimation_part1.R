# POLITSC 7551, Fall 2015
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Part of the script draws on code from Jamie Monogan
# modified Fall 2018 by Adam Lauretig

## Hypothesis testing ----
library(foreign)
nes <-read.dta("~/data/quant_1_code/nes2004c.dta")

# 2004 NES Data 
# Exploring relationship b/w support for gay marriage and living in the south 
# South census region = (AL,AR,DE,DC,FL,GA,KY,LA,MD,MS,NC,OK,SC,TN,TX,VA,WV) 

table1 <- table(nes$gay_marriage, nes$south) # arguments: table(rows, columns)
# nes$gay_marriage == 1 means respondent supports gay marriage
# nes$south == 1 means respondent lives in the south of the US

table1

prop.table(table1)

margin.table(table1, 1) # frequency of A (gay_marriage) summed over B (south)
margin.table(table1, 2) # frequency of B (south) summed over A (gay_marriage)

# chi-squared test -- in a contingency table, testing whether unpaired observations
# on two categorical variables are independent of one another (in this case, whether
# level of support for gay marriage is independent of region, based on number of
# observations in each cell of the table) See Gailmard pgs. 279-282.
chisq.test(table1)

model1 <- glm(gay_marriage ~ south, data=nes, family="binomial")
summary(model1)
model2 <- glm(gay_marriage ~ south + partyid7 + south*partyid7, data=nes, family="binomial")
summary(model2)


### Estimation ----
## Confidence intervals in R ----

#A) # first let's calculate CI for the mean of normally distributed data
set.seed(4)

sampled.data <- rnorm(10000,mean=0,sd=1)
head(sampled.data)
plot(density(sampled.data))

# recall CI: x+/-z*(SE)
# where SE=sigma/sqrt(n)

# let's assume a 99% CI, so z is:
z <- qnorm(.005,mean=0,sd=1,lower.tail=F)

n <- length(sampled.data) # number of sampled values
x <- mean(sampled.data) # taking the mean
SE <- 1/sqrt(n) # calculating SE

CI_upper <- x+z*SE
CI_lower <- x-z*SE

CI_upper
CI_lower

abline(v=x, col="red")
abline(v=CI_upper)
abline(v=CI_lower)

# Another example with smaller n. What will this do to our CI?

sampled.data <- rnorm(500,mean=0,sd=1)
plot(density(sampled.data))
x <- mean(sampled.data)
SE <- 1/sqrt(length(sampled.data))
CI_upper <- x+z*SE
CI_lower <- x-z*SE

CI_upper
CI_lower

abline(v=x, col="red")
abline(v=CI_upper)
abline(v=CI_lower)

# find all the critical z-scores you need
z_95 <- qnorm(.025,mean=0,sd=1, lower.tail=F)
z_90 <- qnorm(.05,mean=0,sd=1,lower.tail=F)
z_99 <- qnorm(.005,mean=0,sd=1,lower.tail=F)

### Working with the t distribution, i.e. we don't know sigma ----

#A) # first let's calculate CI for the mean of normally distributed data
sampled.data <- rnorm(10000,mean=4,sd=6)
head(sampled.data)
plot(density(sampled.data))

# recall CI: x+/-t*(SE)
# where SE=sd/sqrt(n)

#let assume a 99% CI so z is:
n <- length(sampled.data) # number of sampled values

t <- qt(.005,n-1,lower.tail=F)
x <- mean(sampled.data) # taking the mean
SE <- sd(sampled.data)/sqrt(n) # calculating SE

CI_upper <- x+t*SE
CI_lower <- x-t*SE

CI_upper
CI_lower

abline(v=x, col="red")
abline(v=CI_upper)
abline(v=CI_lower)

# find all the critical z-scores you need
t_95 <- qt(.025,n-1,lower.tail=F)
t_90 <- qt(.05,n-1,lower.tail=F)
t_99 <- qt(.005,n-1,lower.tail=F)


##################################################################
### Constructing a Confidence Interval for Proportions
##################################################################
sampled.data <- rbinom(10000,size=1,prob=0.75)
sampled.data[1:100]
plot(density(sampled.data))
hist(sampled.data)

prop <- mean(sampled.data) # the proportion of 1s
n <- length(sampled.data) #number of sampled values
se <- sqrt(prop*(1-prop)/n)

z_95 <- qnorm(0.025,lower.tail=F)
z_90 <- qnorm(0.05,lower.tail=F)
z_99 <- qnorm(0.005,lower.tail=F)

ci90_low <- prop-z_90*se
ci90_high <- prop+z_90*se
ci90_low
ci90_high

ci95_low <- prop-z_95*se
ci95_high <- prop+z_95*se
ci95_low
ci95_high

ci99_low <- prop-z_99*se
ci99_high <- prop+z_99*se
ci99_low
ci99_high


#########################################
### Bias and Consistency
#########################################

# Bias of OLS
set.seed(123456) # Set the seed for reproducible results

sims <- 500 # Set the number of simulations at the top of the script
alpha.1 <- numeric(sims) # Empty vector for storing the simulated intercept estimates
B.1 <- numeric(sims) # Empty vector for storing the simulated coefficient estimates
a <- .2 # True values
b <- .5
n <- 1000 # sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the variable X. Note that this variable is outside the loop, because X should be fixed in repeated samples.

for(i in 1:sims){ # Start the loop
  Y <- a + b*X + rnorm(n, 0, 1) # The true relationship, with N(0, 1) error
  model <- lm(Y ~ X) # Estimate OLS
  alpha.1[i] <- model$coef[1] # Put the estimate for the intercept in the vector alpha.1
  B.1[i] <- model$coef[2] # Put the estimate for X in the vector B.1
} # End loop

mean(alpha.1) # Look at the results
mean(B.1)

par(mfrow = c(2, 1)) # Distribution of simulated estimates
plot(density(alpha.1),
     main="Simulated Distribution of Intercept",
     xlab="Estimated Values of Parameters")
abline(v = mean(alpha.1), col = "red")
plot(density(B.1),
     main="Simulated Distribution of Slope",
     xlab="Estimated Values of Parameters")
abline(v = mean(B.1), col = "red")


# Consistency of the the mean as an estimator

mean.plotter <- function(simulations,m,sdev){
  mean.j <- rep(0, simulations)
  sim <- rep(0, simulations)
  for (i in 1:simulations) {
    j <- rnorm(i,mean=m,sd=sdev)
    mean.j[i] <- mean(j)
    sim[i] <- i
  }
  mean.j
}
mean.plotter(simulations=10000,m=3,sdev=1)

mean.gg.plot <- function(draws,m,sdev){
  library(ggplot2)
  dv <- mean.plotter(simulations=draws,m=m,sdev=sdev)
  sims <- seq(1,draws, by=1)
  cbind(sims, dv)
  P1 <- qplot(sims, dv, geom="point", color=sims)
  print(P1)
}
par( mfcol= c(2, 2))
mean.gg.plot(draws=1000,m=3,sdev=1)


####################################################
# Linear Regression
####################################################
library(foreign)
library(ggplot2)
library(sandwich)
library(lmtest)
library(car)

setwd("~/Dropbox/OSUClasses/Quant_I/Data/")
setwd("F:/Quant_I/")

vote <- read.dta("votegdp.dta")
summary(vote)

## Scatterplots
plot(vote$q2gdp,vote$vote)
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share")
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share",pch=2)
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share",pch=2,col="red")
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share",pch=2,col="red",xlim=c(-10,10))


## Correlation Coefficent

# single correlation coefficient
cor(vote$q2gdp,vote$vote)
# correlation matrix
cor(vote)


### Linear Regression

# by hand
y_bar <- mean(vote$vote)
x_bar <- mean(vote$q2gdp)
s_y <- sd(vote$vote)
s_x <- sd(vote$q2gdp)
r <- cor(vote$q2gdp,vote$vote)

b <- r*(s_y/s_x)
a <- y_bar - b*x_bar
a
b
r_squared <- r^2 # R^2 is just the square of the correlation
r_squared

lm1 <- lm(vote~q2gdp,data=vote)
summary(lm1) # main results
confint(lm1) # confidence intervals for regression coefficients
confint(lm1, level=0.99) # confidence intervals for regression coefficients


names(lm1)
lm1$coefficients # a and b
lm1$fitted.values # y_hat, the predicted value
lm1$residuals # y-y_hat

# Simple diagnostics
hist(lm1$residuals, breaks=10)
plot(vote$q2gdp,lm1$residuals)
plot(lm1)

# some more information
output <- summary(lm1)
names(output)

output$sigma # overall regression standard error
output$r.squared

# Using predict()

predict(lm1) # shows the fitted values
predict(lm1, interval=c("prediction")) # predicted value with 95% CI
predict(lm1,newdata=data.frame(q2gdp=c(-10,0,10))) # make predictions

## Visualizing the regression results

#plotting regression line
plot(x=vote$q2gdp,y=vote$vote,xlab="Q2 Growth",ylab="Vote Share",main="Regression Line of Q2 Growth",type="p")
abline(lm1)

#"by hand" use of abline to plot line
plot(x=vote$q2gdp,y=vote$vote,xlab="Q2 Growth",ylab="Vote Share",main="Regression Line of Q2 Growth",type="p")
summary(lm1)
#grabbing intercept and slope to put in abline statement below
lm1$coefficients
int <- lm1$coefficients[1]
int
slope <- lm1$coefficients[2]
slope
abline(a=int,b=slope,col="red")

# adding 95% confidence interval
plot(x=vote$q2gdp,y=vote$vote,xlab="Q2 Growth",ylab="Vote Share",main="Regression Line of Q2 Growth",type="p",ylim=c(35,65))
abline(lm1)
newx <- seq(min(vote$q2gdp), max(vote$q2gdp), 0.1)
a <- predict(lm1, newdata=data.frame(q2gdp=newx), interval="confidence")
lines(newx,a[,2], lty=3)
lines(newx,a[,3], lty=3) 

# or
#qplot(q2gdp,vote,data=vote,geom=c("point","smooth"),method="lm")
# ^ Following ggplot2 update, this no longer works. (method argument in qplot is now deprecated)
