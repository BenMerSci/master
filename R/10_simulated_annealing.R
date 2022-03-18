#############
### GenSA ###
#############
library(GenSA)

# dataset$pred_flow are the energy flows
dataset <- readRDS("data/clean/dataset.RDS")

#####################################################################
# Model 0: energy flux is equal to a constant, which is the flux mean
#####################################################################
# Likelihood functions
ll_dlnorm0 <- function(data = dataset, pars) {
  obs <- data[, "pred_flow"]
  mu <- pars[1]
  sigma <- pars[2]
  -sum(log(dlnorm(obs, mu, sigma)))
}

# Parameters for each distributions
pars_dlnorm0 <- c(mu = 0, sigma = 1)
pars_dlnorm_lo0 <- c(-30, 0)
pars_dlnorm_hi0 <- c(30, 100)

# Apply GenSA functions for each distribution
res_dlnorm0 <- GenSA(par = pars_dlnorm0, fn = ll_dlnorm1, lower = pars_dlnorm_lo0, upper = pars_dlnorm_hi0,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)

# Check outputs
res_dlnorm0[c("value","par","counts")]

#########################################
# Model 1: evaluate pars[1] which is alpha
# and is also general to all interactions
#########################################
# Likelihood functions
# dlnorm 
ll_dlnorm1 <- function(data = dataset, pars) {
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
pars_dlnorm1 <- c(alpha = 0, sigma = 10)
pars_dlnorm_lo1 <- c(-10, 0)
pars_dlnorm_hi1 <- c(10, 100)

# Apply GenSA functions for each distribution
res_dlnorm1 <- GenSA(par = pars_dlnorm1, fn = ll_dlnorm1, lower = pars_dlnorm_lo1, upper = pars_dlnorm_hi1,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)

# Check outputs
res_dlnorm1[c("value","par","counts")] # alpha = 0.000000001276563 sigma = 5.362259894106184

#########################################
# Model2: evaluate pars[1] which is alpha
# and is specific to each predator
#########################################
# Likelihood
# dlnorm
ll_dlnorm2 <- function(data = dataset, pars) {
  obs <- data[, "pred_flow"]
  Ni <- data[, "abund_prey"]
  Mi <- data[, "bodymass_prey"]
  Nj <- data[, "abund_pred"]
  ID <- data[, "pred_id"]
  alpha <- pars[numeric(length(obs)) + as.numeric(ID)]

  mu <- alpha * Ni * Mi * Nj
  sigma <- pars[108]
  -sum(log(dlnorm(obs, mu, sigma)))
}

# Parameters
pars_dlnorm2 <- c(mu = 0, sigma = 1)
pars_dlnorm_lo2 <- c(-30, 0)
pars_dlnorm_hi2 <- c(30, 100)

# Apply GenSA functions for each distribution
res_dlnorm2 <- GenSA(par = pars_dlnorm2, fn = ll_dlnorm2, lower = pars_dlnorm_lo2, upper = pars_dlnorm_hi2,
control = list(verbose = TRUE, max.time = 1000, smooth = FALSE), data = dataset)
