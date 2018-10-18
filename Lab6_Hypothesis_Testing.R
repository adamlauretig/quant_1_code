# POLITSC 7551, Fall 2015
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Modified by Adam Lauretig, Fall 2018

##  Hypothesis Testing ----

# We now turn to simple hypothesis tests 
# Figure out your H0, H1, the critical z/t value, the standard error and the rest is algebra...

library(foreign)


nes <- read.dta("~/Data/quant_1_code/nes2004c.dta")
summary(nes)

# let's have a look at the thermometer score
mean(nes$rep_therm,na.rm = T)
mean(nes$rep_therm[nes$white == 1],na.rm = T)
mean(nes$rep_therm[nes$white == 0],na.rm = T)

hist(nes$rep_therm, main = "Republican Thermometer Score", xlab = "Score")
hist(nes$rep_therm[nes$white == 1], main = "White Republican Thermometer Score", xlab = "Score")
hist(nes$rep_therm[nes$white == 0], main = "Non-White Republican Thermometer Score", xlab = "Score")

#let's test hypotheses about the thermometer score
mean_all <- mean(nes$rep_therm, na.rm = TRUE)
n <- length(nes$rep_therm[!is.na(nes$rep_therm)])

# Normal test for mean: one sample
# Let's test the two-sided hypothesis that rep_therm is different from 50
# and pick a 5% alpha level
# for now we assume that sigma is known and equal to 26

H0 <- 50 # H0: overall mean is 50
se <- 26/sqrt(n)

# our z-test statistic is:
z <- (mean_all - H0)/se
z

# Now, we can compare our z-test statistic to appropriate critical values
# for a two-tailed test. z-statistics larger than 1.96 or smaller than -1.96
# are evidence against the null
crit_low <- qnorm(0.025)
crit_low

crit_high <- qnorm(0.025,lower.tail = FALSE)
crit_high

# our calculated z-stat is much larger than 1.96 -> reject the null
# let's calculate the p-value
2*pnorm(z,lower.tail = FALSE) #two-tailed

# what about a one-sided hypothesis mu>50, i.e P(z>test stat)
# the critical value is:
crit_value <- qnorm(0.05,lower.tail = FALSE)
crit_value
# again we can clearly reject the Null
# what is the associated p-value?
pnorm(z,lower.tail = FALSE) #one-sided


# what about a one-sided hypothesis mu<50, i.e. P(z< test stat)
# the critical value is:
crit_value <- qnorm(0.05)
crit_value
# our z-stat is not more extreme (more negative), so we can't reject the null
# what is the one-sided p-value?
pnorm(z) #one-sided


##############################################

# Now, we switch to a world where sigma is unknown, but we
# assume that our population distribution is normal
# hence, we can use the t distribution as our sampling distribution

# Let's test the two-sided hypothesis that rep_therm is different from 50
# and pick a 5% alpha level
H0 <- 50 # H0: overall mean is 50
se <- sd(nes$rep_therm,na.rm = T)/sqrt(n)
# calculate z test statistic
t <- (mean_all - H0)/se
t

# Now, we can compare our t-test statistic to appropriate critical values
# for a two-tailed test, t-statistics larger than 1.96 or smaller than -1.96
# are evidence against the null
?TDist

crit_low <- qt(0.025,n-1)
crit_low

crit_high <- qt(0.025,n-1,lower.tail = FALSE)
crit_high

# our calculated t-stat is much larger than 1.96 -> reject the null
# let's calculate the p-value
2*pt(t,n-1,lower.tail = FALSE) #two-tailed

# Let's look at a plot:
x <- seq(-6,6, by = 0.01)
plot(x, dt(x, n-1), type = "l", ylab = "density", main =  "t distribution")
abline(v = crit_low, col = "red")
abline(v = crit_high, col = "red")
abline(v = t, col = "green")
legend("topleft", legend = c("Critical Value", "Test Statistic"), col = c("red", "green"), lwd = 1.5, bty = "n", cex = 0.5)

# what about a one-sided hypothesis mu>50, i.e P(t>test stat)
# the critical value is:
crit_value <- qt(0.05,n-1,lower.tail = FALSE)
crit_value
# again we can clearly reject the Null
# what is the associated p-value?
pt(t,n-1,lower.tail = FALSE) #one-sided

# what about a one-sided hypothesis mu<50, i.e. P(t< test stat)
# the critical value is:
crit_value <- qt(0.05,n-1)
crit_value
# our z-stat is not more extreme (more negative), so we can't reject the null
# what is the one-sided p-value?
pt(t,n-1) #one-sided


# we can also use the t.test function
t.test(nes$rep_therm, mu = 50, alternative = "two.sided", conf.level = 0.95)
t.test(nes$rep_therm, mu = 50, alternative = "greater", conf.level = 0.95)
t.test(nes$rep_therm, mu = 50, alternative = "less", conf.level = 0.95)


# If you are testing a hypothesis about an proportion, make sure to calculate the
# correct test statistic


# How do we take a random sample? ----


set.seed(12345)
## takes the number of rows
n_obs <- nrow(nes)

## Defines a sample size of 50
N <- 50

# Pick 50 random rows
rand_rows <- sample(x = 1:n_obs, size = N, replace = FALSE)
rand_rows

## Now that we have the random rows, we can grab those random rows
## from the actual data and put it into a new matrix:
mysample <- nes[rand_rows,]



## Differences in Means, Cross-Tabs ----


## Differences in Means

# let's load the civil war data we have used before


civil <- read.dta("~/data/quant_1_code/repdata.dta")

summary(civil)

# let's test for a difference in ethnic fractionalization
# between countries with ongoing wars and without

t.test(civil$ethfrac ~ civil$war)

# does this still hold for countries in certain regions?
t.test(civil$ethfrac[civil$asia == 1] ~ civil$war[civil$asia == 1])
t.test(civil$ethfrac[civil$ssafrica == 1] ~ civil$war[civil$ssafrica == 1])
t.test(civil$ethfrac[civil$lamerica == 1] ~ civil$war[civil$lamerica == 1])
t.test(civil$ethfrac[civil$eeurop == 1] ~ civil$war[civil$eeurop == 1])
t.test(civil$ethfrac[civil$western == 1] ~ civil$war[civil$western == 1])


# if you are working with a matched pair, set "paired = TRUE" in the options


## Testing for categorical variables - Differences in proportions ----
# are there differences in the proportion of whites between the South and the rest of the US?

x_south <- mean(nes$white[nes$south == 1],na.rm = T)
x_notsouth <- mean(nes$white[nes$south == 0],na.rm = T)

diff <- x_south - x_notsouth
overall_prop <- mean(nes$white,na.rm = T)
n1 <- length(nes$white[nes$south == 1])
n2 <- length(nes$white[nes$south == 0])
se <- sqrt(overall_prop*(1-overall_prop)*((1/n1)+(1/n2)))

z <- diff/se
2*pnorm(z)


tab1 <- table(nes$south, nes$white) # first entry are the rows, second entry the columns

prop.table(tab1) # overall proportions
prop.table(tab1,margin = 1) # proportion with respect to the rows
prop.table(tab1,margin = 2) # proportion with respect to the columns
prop.test(tab1,alternative = "two.sided",conf.level = 0.95,correct = FALSE)

# or
tab1 <- ctab(factor(nes$south), factor(nes$white))
summary(tab1)


# ctab works more generally on categorical variables with many categories
MAR <- read.dta("~/data/quant_1_code/MAR_2006.dta")

ctab(factor(MAR$POLGR))
ctab(factor(MAR$EXECREP))
ctab(factor(MAR$GROUPCON))

tab <- ctab(factor(MAR$POLGR),factor(MAR$BELIEF),type = c("n"),addmargins = T)
tab
tab <- ctab(factor(MAR$POLGR),factor(MAR$BELIEF),type = c("n","c"),style = "l",addmargins = T)
tab
summary(tab)

tab2 <- ctab(factor(MAR$POLGR),factor(MAR$GROUPCON),type = c("n","c"),style = "l",addmargins = T)
tab2
summary(tab2)






