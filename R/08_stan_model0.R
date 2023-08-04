library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

# List the needed data
standata0 <- list(n = nrow(dataset), biomass_flow = dataset$biomass_flow)

stancode0 <- readLines("stan/08_stan_model0.stan")

stanmodel0 <- stan_model(model_code = stancode0)

fit0 <- sampling(stanmodel0, data = standata0, iter = 4000, chains = 4)

loo_0 <- loo::loo(fit0, save_psis = TRUE, moment_match = TRUE)

# Saving the outputs
saveRDS(fit0, "results/model_outputs/stanfit_model0.RDS")
saveRDS(loo_0, "results/loo_outputs/loo_0.RDS")
