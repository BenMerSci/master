library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

# List the needed data
standata2 <- list(n = nrow(dataset), n_predator = length(unique(dataset$predator)), biomass_flow = dataset$biomass_flow, biomass_prey = dataset$biomass_prey,
                  abundance_predator = dataset$abundance_predator, pred_id = dataset$pred_id)

stancode2 <- readLines("stan/08_stan_model2.stan")

stanmodel2 <- stan_model(model_code = stancode2)

fit2 <- sampling(stanmodel2, data = standata2, iter = 4000, chains = 4)

loo_2 <- loo::loo(fit2, save_psis = TRUE, moment_match = TRUE)

# Saving the outputs
saveRDS(fit2, "results/model_outputs/stanfit_model2.RDS")
saveRDS(loo_2, "results/loo_outputs/loo_2.RDS")
