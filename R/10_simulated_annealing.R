#############
### GenSA ###
#############
library(GenSA)

# dataset$pred_flow are the energy flows
dataset <- readRDS("data/clean/dataset.RDS")

#####################################################################
# Model 1: energy flux is equal to a constant, which is the flux mean
#####################################################################
# Likelihood functions
# dnorm
ll_dnorm1 <- function(data = dataset, pars) {
  obs <- data[, "pred_flow"]
  mu <- pars[1]
  sigma <- pars[2]
  -sum(log(dnorm(obs, mu, sigma)))
}
# dlnorm
ll_dlnorm1 <- function(data = dataset, pars) {
  obs <- data[, "pred_flow"]
  mu <- pars[1]
  sigma <- pars[2]
  -sum(log(dlnorm(obs, mu, sigma)))
}

# dgamma
#ll_dgamma1 <- function(data = dataset, pars) {
#  obs <- data[, "pred_flow"]
#  a <- pars[1]
#  b <- pars[2]
#  -sum(log(dgamma(obs, shape = a, rate = b)))
#  #  -sum(log(dgamma(obs, shape = mu^2/sigma, rate = mu/sigma)))
#}

# Parameters for each distributions
# dnorm
pars_dnorm1 <- c(mu = 1, sigma = 50)
pars__dnorm_lo1 <- c(-50,1)
pars_dnorm_hi1 <- c(50,100)
# dlnorm
pars_dlnorm1 <- c(mu = 0, sigma = 1)
pars_dlnorm_lo1 <- c(-30, 0)
pars_dlnorm_hi1 <- c(30, 100)
# dgamma
#pars_dgamma1 <- c(mu =0.15, sigma = 0.003)
#pars_dgamma_lo1 <- c(0,0)
#pars_dgamma_hi1 <- c(1,1)

# Apply GenSA functions for each distribution
# dnorm
res_dnorm1 <- GenSA(par = pars_dnorm1, fn = ll_dnorm1, lower = pars__dnorm_lo1, upper = pars_dnorm_hi1,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)
# dlnorm
res_dlnorm1 <- GenSA(par = pars_dlnorm1, fn = ll_dlnorm1, lower = pars_dlnorm_lo1, upper = pars_dlnorm_hi1,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)
# dgamma
#res_dgamma1 <- GenSA(par = pars_dgamma1, fn = ll_dgamma1, lower = pars_dgamma_lo1, upper = pars_dgamma_hi1,
#control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)

# Check outputs
res_dnorm1[c("value","par","counts")]
res_dlnorm1[c("value","par","counts")]
#res_dgamma1[c("value","par","counts")


#########################################
# Model2: evaluate pars[1] which is alpha
# and is also general to all interactions
#########################################
# Likelihood functions
# dnorm
ll_dnorm2 <- function(data = dataset, pars) {
  obs <- data[, "pred_flow"]
  Ni <- data[, "abund_prey"]
  Mi <- data[, "bodymass_prey"]
  Nj <- data[, "abund_pred"]
  mu <- pars[1] * Ni * Mi * Nj
  sigma <- pars[2]
  -sum(log(dnorm(obs, mu, sigma)))
}
# dlnorm 
ll_dlnorm2 <- function(data = dataset, pars) {
  obs <- data[, "pred_flow"]
  Ni <- data[, "abund_prey"]
  Mi <- data[, "bodymass_prey"]
  Nj <- data[, "abund_pred"]
  alpha <- pars[1]
  mu <- alpha * Ni * Mi * Nj
  sigma <- pars[2]
  -sum(log(dlnorm(obs, mu, sigma)))
}

# Parameters for each distributions
# dnorm
pars_dnorm2 <- c(alpha = 0.5, sigma = 50)
pars_dnorm_lo2 <- c(0, 1)
pars_dnorm_hi2 <- c(1, 100)
# dlnorm
pars_dlnorm2 <- c(alpha = 0, sigma = 10)
pars_dlnorm_lo2 <- c(-10, 0)
pars_dlnorm_hi2 <- c(10, 100)

# Apply GenSA functions for each distribution
# dnorm
res_dnorm2 <- GenSA(par = pars_dnorm2, fn = ll_dnorm2, lower = pars_dnorm_lo2, upper = pars_dnorm_hi2,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)
# dlnorm
res_dlnorm2 <- GenSA(par = pars_dlnorm2, fn = ll_dlnorm2, lower = pars_dlnorm_lo2, upper = pars_dlnorm_hi2,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)

# Check outputs
res_dnorm2[c("value","par","counts")] # alpha = 0.00000005327105 sigma = 28.69789286949295
res_dlnorm2[c("value","par","counts")] # alpha = 0.000000001276563 sigma = 5.362259894106184
