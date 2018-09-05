# POLITSC 7551, Fall 2016
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Modified Fall 2018, Adam Lauretig


####################################################
# Probability and Probability Distributions
####################################################
library(ggplot2)
# Law of Large Numbers 

set.seed(1212) # sets the random number generator
n <- 50000  # number of coin flips we want to simulate
p <- 0.3 # probability of head
# flip 50000 coins and store the result
x <- sample(0:1, n, replace = TRUE, prob = c(1 - p, p))
s <- cumsum(x) # creates a vector that counts the running number of heads
r <- s/(1:n) # calculates the percentage of heads after trial i


# let's plot this out, using ggplot
# first, we'll need to put our data in a data.frame (or data.table, 
# but a data.frame is fine here)
coin_df <- data.frame(toss = 1:n, cumu_prob = r)
ggplot(data = coin_df, aes(x = toss, y = cumu_prob)) + 
  geom_line() + geom_hline(yintercept = p, alpha = .5, color = "red") + xlim(0, nrow(coin_df)) + ylim()

# let's make this look nice, using themes:
ggplot(data = coin_df, aes(x = toss, y = cumu_prob)) + 
  geom_line() + geom_hline(yintercept = p, alpha = .5, color = "red") + theme_minimal()




# generate random variables

x <- rnorm(1)
x

x <- rnorm(1,mean=10, sd=20)
x

x <- rnorm(10)
x2 <- rnorm(10,mean=10, sd=20)
par(mfrow=c(1,2))
plot(density(x))
plot(density(x2))

x <- rnorm(100)
x2 <- rnorm(100,mean=10, sd=20)
par(mfrow=c(1,2))
plot(density(x))
plot(density(x2))

x <- rnorm(1000)
x2 <- rnorm(1000,mean=10, sd=20)
par(mfrow=c(1,2))
plot(density(x))
plot(density(x2))

# theoretical normal density
par(mfrow=c(1,1))
dnorm(0.5,mean=5,sd=1) #value of the density/pdf at x

x <- seq(-20,20,by=.01)
y <- dnorm(x) 
plot(x,y,pch=20) # pch argument sets the appearance of the points on the plot 




#####################################################################################
#1) Normal Table Values in R-calculating probability z is less/greater than some value

?pnorm #pnorm is a function which tells you the probability of finding a value 
#less than z (your input) under a normal distribution with given mean and sd (CDF)

pnorm(2,mean=0,sd=1) #.977 probability z is less than 2

1-pnorm(2,mean=0,sd=1) #.022 probability z is > than 2
pnorm(2,mean=0,sd=1,lower.tail=F) #same thing

#probability b/t 0 and 2
great0 <- 1-pnorm(0,mean=0,sd=1) #probability greater than 0
less2 <- pnorm(2,mean=0,sd=1) #probability less than 2

less2-great0 #probability b/t 0 and 2
###############################################################################

qnorm(0.5) # reverse of the CDF, feed it a probability and it tells you the corresponding z-score
x <- seq(0,1,by=.05)
y <- qnorm(x)
plot(x,y)


?qnorm

qnorm(0.5)
qnorm(0.5,mean=1)
qnorm(0.5,mean=1,sd=2)
qnorm(0.5,mean=2,sd=2)
qnorm(0.5,mean=2,sd=4)

qnorm(0.25,mean=2,sd=2)
pnorm(0.6510205,mean=2,sd=2)

# Getting critical z values for a given level of confidence (reverse of CDF)
# We will use this all the time for hypothesis testing

# What is the z-value for which there is a 0.005 probability
# of observing a z-score lower than that?
qnorm(0.005,mean=0,sd=1) 
# What is the z-value for which there is a 0.005 probability
# of observing a z-score higher than that?
qnorm(0.005,mean=0,sd=1,lower.tail=FALSE)   



# there are other random variables. Check out:
# rbinom()
# runif()
# rpois()
# rgamma()

#quantile function for a chi-squared distribution with 4 degrees of freedom
qchisq(.9,df=4)
chi <- rchisq(5000, df=4)
plot(density(chi))

#probability mass function for a Poisson distribution with intensity parameter 9
dpois(5,lambda=9)
d <- rpois(5000, lambda=9)
plot(density(d))



#################################################################################


