# POLITSC 7551, Fall 2015
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Edited, Fall 2018, Adam Lauretig
library(ggplot2)
library(data.table)
library(foreign)

## Sampling Distributions ----

## Illustration of the CLT and Sampling ----
set.seed(1)
population <- rnorm(10000, mean  =  10, sd  =  4)
plot(density(population), main  =  "Distribution of Population", col  =  "blue", lwd  =  3)


# two approaches

t1 <- Sys.time()
samples <- matrix(NA, nrow  =  500, ncol = 800)
for (i in 1:800){
    samples[,i] <- sample(population, 500, replace = F)
}
t2 <- Sys.time()

# here, let's use "replicate()"

t3 <- Sys.time()
samples <- replicate(500, sample(population, 500, replace = F))
t4 <- Sys.time()

# compare
t2-t1 # the difference a for-loop makes
t4-t3 



means <- colMeans(samples)

hist(means)

# from the CLT we know that for samples of size 500, \bar{x} \sim N(10,4/sqrt(500))

xbar <- rnorm(800, m = 10, sd = 4/sqrt(500))

plot(density(means))
lines(density(xbar),col = "red")

# ok, works for a normal, but what about other population distributions?
population <- rpois(10000, lambda = 6)
plot(density(population), col  =  "blue", main  =  "Population Distribution", lwd = 3)
samples <- replicate(500, sample(population, 500, replace = F))

means <- colMeans(samples)
#par(mfrow = c(1,2))
hist(population, main  =  "Population")
hist(means, main  =  "Means", breaks = 10)

# We do not expect that the distribution of means will approach the distribution of the population. 
# Instead, the CLT tells us that the distribution of means from 100 samples will approach a normal
# distribution with mean 6 (the population mean) and standard deviation of sqrt(6/100), or the 
# square root of the population variance over the sample size.

xbar <- rnorm(10000, m = 6, sd = sqrt(6/100))

par(mfrow = c(1,1))
plot(density(means), main  =  "Comparison of sample means to expected")
lines(density(xbar),col = "red")
  # Hmm, not a great fit.... why?


# Same thing with more samples
samples2 <- replicate(800, sample(population,100,replace = F))
means2 <- colMeans(samples2)

par(mfrow = c(1,2))
hist(population)
hist(means)

# from the CLT we know that for samples of size 100, \bar{x} \sim N(6,sqrt(6/100))
xbar <- rnorm(10000,m = 6,sd = sqrt(6/100))

par(mfrow = c(1,1))
plot(density(means2), main  =  "Sample Means vs. Expected", ylim  =  c(0,2))
lines(density(xbar),col = "red")
lines(density(means), col  =  "blue")

# Let's try even more samples
samples3 <- replicate(5000, sample(population,100,replace = F))
means3 <- colMeans(samples3)
plot(density(means3), main  =  "Sample Means vs. Expected", ylim  =  c(0,2))
lines(density(xbar),col = "red", lwd  =  3)

lines(density(means), col  =  "blue")
lines(density(means2), col  =  "green")


# Binomial distribution
population <- rbinom(10000,prob = 0.7,size = 20)
plot(density(population), main  =  "Population Distribution")
samples <- replicate(500, sample(population, 500, replace = F))

means <- colMeans(samples)

hist(population)
hist(means)

# from the CLT we know that for samples of size 500, \bar{x} \sim N(0.7*20,sqrt(20*0.7*(1-0.7)/n))
xbar <- rnorm(800,m = 14,sd = sqrt(4.2/500))

plot(density(means))
lines(density(xbar),col = "red")

### Negative Binomial example

set.seed(123)
pop <- rnbinom(200000, 3, 0.2)
# Create the population of the negative binomial distribution with parameters 3 and 0.2
hist(pop, breaks = 30)
mean(pop)
sd(pop) 
samples <- matrix(NA, nrow  =  10, ncol  =  1000)
for (i in 1:1000){
  samples[,i] <- sample(pop, 10, replace  =  T)
}
samples <- replicate(1000, sample(population, 10, replace = TRUE))

# Draw 1000 samples of size 10

means <- colMeans(samples)
# Find the mean of each sample

xbar1 <- rnorm(200000, mean(pop), sd(pop)/sqrt(10))
# Find the normal distribution to which to compare the distribution of sample means

hist(means, xlab  =  "Mean", main  =  "1000 samples of 10 draws", freq  =  FALSE)
lines(density(xbar1), col  =  "red", lwd  =  2)
abline(v = mean(pop), col = "blue", lwd = 2)
abline(v = mean(means), col = "green", lwd = 2)
legend("topright", legend  =  c("Normal distribution", "Population mean", "Mean of sample means"), lwd  =  2, col  =  c("red", "blue", "green"), bty  =  "n",
       cex  =  0.8)


##  Sampling -----

x <- seq(1:1000)
mean(x) #this is our "true" mean 500.5
s1 <- sample(x, 20, replace = F)
mean(s1)
s2 <- sample(x, 25, replace = F)
mean(s2)
s3 <- sample(x, 100, replace = F)
mean(s3)
s4 <- sample(x, 250, replace = F)
mean(s4)
s5 <- sample(x, 500, replace = F)
mean(s5)
n <- c(20, 25, 100, 250, 500)
means <- c(mean(s1), mean(s2), mean(s3), mean(s4), mean(s5))
n
means
qplot(n, means, geom = "point", ymin = 300, color = n)  + geom_line(aes(y = mean(x)))

###Introducing Sampling with lapply##
x <- seq(1:1000)
mean(x)
t1 <- Sys.time()
sample_dist <- function(i){
  
  sample_draws <- sample(x, i, replace = FALSE)
  sample_mean <- mean(sample_draws)
  std_errs <- (sd(sample_draws)/sqrt(i))
  highs <- (sample_mean+std_errs*1.96)
  lows <- (sample_mean-1.96*std_errs)
  data.table(num_samples = i, sample_mean, std_errs, highs, lows)
  
}
samples_list <- lapply(1:100, sample_dist)
samples <- rbindlist(samples_list)
t2 <- Sys.time()

ggplot(data = samples, aes(x = num_samples, sample_mean)) + 
  geom_point() + 
  geom_errorbar(aes(x = num_samples, ymin = lows, ymax = highs))

ggplot(data = samples, aes(x = num_samples, sample_mean)) + 
  geom_point() + 
  geom_errorbar(aes(x = num_samples, ymin = lows, ymax = highs)) + 
  #Now, say if we wanted a line where the "true mean" was:#
  geom_line(aes(y = mean(x)), color = "red")
#Changing "i" only changes the sample size max, so you can run this to see what happens as sample size goes to "infinity":


samples_list_500 <- lapply(1:500, sample_dist)
samples_500 <- rbindlist(samples_list_500)


ggplot(data = samples_500, aes(x = num_samples, sample_mean)) + 
  geom_point() + 
  geom_errorbar(aes(x = num_samples, ymin = lows, ymax = highs))

ggplot(data = samples_500, aes(x = num_samples, sample_mean)) + 
  geom_point() + 
  geom_errorbar(aes(x = num_samples, ymin = lows, ymax = highs)) + 
  #Now, say if we wanted a line where the "true mean" was:#
  geom_line(aes(y = mean(x)), color = "red")


#Sampling cases from a Data Set:#
nes <- read.dta("~/data/quant_1_code/nes2004c.dta")
names(nes)
attach(nes)
length(nes$partyid7)  #1212 cases

subsample <- sample(1:1212, size = 25, replace = FALSE)
subsample
sampled.rows <- nes[subsample,]
length(sampled.rows$partyid7) #check if it is equal to 25
mean(sampled.rows$partyid7) # what's gone wrong here?
sd(sampled.rows$partyid7)



## lapply vs. for-loops ----
rm(list = ls())
x <- 1:1000

t1 <- Sys.time()
sample_dist <- function(i){
  
  sample_draws <- sample(x, i, replace = FALSE)
  sample_mean <- mean(sample_draws)
  std_errs <- (sd(sample_draws)/sqrt(i))
  highs <- (sample_mean+std_errs*1.96)
  lows <- (sample_mean-1.96*std_errs)
  data.table(num_samples = i, sample_mean, std_errs, highs, lows)
  
}
samples_list <- lapply(1:100, sample_dist)
samples <- rbindlist(samples_list)
t2 <- Sys.time()

t3 <- Sys.time()
samples <- c()
sample.mean <- c()
highs <- c()
lows <- c()
std.errs <- c()
for (i in 1:100) {
  samples <- sample(x, i, replace = F)
  sample.mean[i] <- mean(samples)
  std.errs[i] <- (sd(samples)/sqrt(i))
  highs[i] <- (sample.mean[i]+std.errs[i]*1.96)
  lows[i] <- (sample.mean[i]-1.96*std.errs[i])
}
t4 <- Sys.time()
t4-t3
t2 - t1