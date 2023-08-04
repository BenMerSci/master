library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

# List the needed data
standata1 <- list(n = nrow(dataset), biomass_flow = dataset$biomass_flow, biomass_prey = dataset$biomass_prey,
                  abundance_predator = dataset$abundance_predator)

stancode1 <- readLines("stan/08_stan_model1.stan")

stanmodel1 <- stan_model(model_code = stancode1)

fit1 <- sampling(stanmodel1, data = standata1, iter = 4000, chains = 4)

loo_1 <- loo::loo(fit1, save_psis = TRUE, moment_match = TRUE)

# Saving the outputs
saveRDS(fit1, "results/model_outputs/stanfit_model1.RDS")
saveRDS(loo_1, "results/loo_outputs/loo_1.RDS")
