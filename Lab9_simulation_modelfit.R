# POLITSC 7551, Fall 2018
# Instructor: Jan Pierskalla
# Code by Adam Lauretig

# Simulating regression models, including several kinds of incorrect fit, 
# and observing when they break

## Our basic dgp setup ----
# Generate Hyperpopulation
# 1. Simulate some data, these are our X's
# 2. Simulate some covariates 
# 2.5 Don't forget the intercept!
# 3. Multiply these together
# 4. Simulate error term, e ~ N(0, sigma^2)
# 5. Add to XB, generating Y
# 6. Combine in data frame

# Create data sample and test simulation
# We'll sample from our hyperpopulation, fit a model, 
# and compare it to the "truth"
set.seed(614)
hyperpop_size <- 10000
number_covariates <- 10
X <- matrix(runif(n = hyperpop_size * number_covariates, min = -5, max = 5), 
  ncol = number_covariates)
X <- cbind(1, X)
betas <- matrix(round( # rounding, for human legibility
  rnorm(n = (number_covariates + 1), mean = 3, sd = 2), 2), nrow = 1)
# only use some of the covariates (at random)
beta_to_use <- sample(
  c(0, 1), size = number_covariates, replace = TRUE, prob = c(.4, .6))
betas[, 2:ncol(betas)] <- betas[, 2:ncol(betas)] * beta_to_use

tmp <- X %*% t(betas) # before we include our errors
# adding some noise to our model, we can control model fit w/the sd term
sigma_sq <- 36
error <- rnorm(n = hyperpop_size, mean = 0, sd = sqrt(sigma_sq)) 
Y <- tmp + error
hyper_pop_data <- data.frame(Y = Y, X[, 2:ncol(X)])

# here's just a function to generate samples 
# arguments: sample size (coerced to integer), hyperpopulation from above
generate_observed_sample<- function(sample_size, data_to_use = hyper_pop_data){
  sample_size <- as.integer(sample_size)
  n_possible_obs <- nrow(data_to_use)
  to_sample <- seq.int(from = 1, to = n_possible_obs, by = 1)
  sampled_indices <- sample(x = to_sample, size = sample_size, replace = FALSE)
  data_to_use[sampled_indices, ]
}

# when we know the DGP, we don't need a big N
m1 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(10))
summary(m1)
m2 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(25))
summary(m2)
m3 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(100))
summary(m3)
m4 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(500))
summary(m4)
plot(m4)

# what about omitting variables? 
# Pretend we don't have the whole universe of data
m1 <- lm(Y~ X1 + X2 + X3, data = generate_observed_sample(10))
summary(m1)
m2 <- lm(Y~ X1 + X2 + X3, data = generate_observed_sample(25))
summary(m2)
m3 <- lm(Y~ X1 + X2 + X3, data = generate_observed_sample(100))
summary(m3)
plot(m3)
# clearly, omitting variables is bad

# what happens if we include variables that aren't a part of the model? (we think they are, though)
test_df <-  generate_observed_sample(100)
test_df$random_1 <- rnorm(100, 0, 1)
test_df$random_2 <- rnorm(100, 0, 1)
# and take our two best models from above

m1 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + random_1 + random_2, 
  data = test_df)
summary(m1)

m2 <- lm(Y~ X1 + X2 + X3 + random_1 + random_2, data =test_df)
summary(m2)
# compare to the m4's above



# Now, let's think about heteroskedasticity ----

hyperpop_size <- 10000
number_covariates <- 10
X <- matrix(runif(n = hyperpop_size * number_covariates, min = -5, max = 5), 
  ncol = number_covariates)
X <- cbind(1, X)
betas <- matrix(round( # rounding, for human legibility
  rnorm(n = (number_covariates + 1), mean = 3, sd = 2), 2), nrow = 1)
# only use some of the covariates (at random)
beta_to_use <- sample(
  c(0, 1), size = number_covariates, replace = TRUE, prob = c(.4, .6))
betas[, 2:ncol(betas)] <- betas[, 2:ncol(betas)] * beta_to_use
tmp <- X %*% t(betas) # before we include our errors

# errors are more complicated here
n <- rep(1:50, 200)
sigma_sq <- n^1.3
error <- rnorm(n = hyperpop_size, mean = 0, sd = sqrt(sigma_sq)) 
Y <- tmp + error
hyper_pop_data_het <- data.frame(Y = Y, X[, 2:ncol(X)])

# when we know the DGP, we don't need a big N
m1 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(10, hyper_pop_data_het))
summary(m1)
m2 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(25, hyper_pop_data_het))
summary(m2)


m3 <- lm(Y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
  data = generate_observed_sample(100, hyper_pop_data_het))
summary(m3)
plot(m3) # examine the qqplot

m4 <- lm(Y~ X1 + X2 + X3, 
  data = generate_observed_sample(100, hyper_pop_data_het))
summary(m4)
plot(m4) # examine the qqplot


# let's then use robust standard errors
library(lmtest)
library(sandwich)
# dealing with heteroskedasticity
# gives you heteroskedasticity-corrected covariance matrix
coeftest(m3,vcovHC(m3,type="HC3"))
coeftest(m4,vcovHC(m4,type="HC3"))



