library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

standata3 <- list(n = nrow(dataset), n_predator = length(unique(dataset$predator)), biomass_flow = dataset$biomass_flow,
                 biomass_prey = dataset$biomass_prey, abundance_predator = dataset$abundance_predator,
                 pred_id = dataset$pred_id)

stancode3 <- readLines("stan/08_stan_model3.stan")

stanmodel3 <- stan_model(model_code = stancode3)

fit3 <- sampling(stanmodel3, data = standata3, iter = 4000, chains = 4)

loo_3 <- loo::loo(fit3, save_psis = TRUE, moment_match = TRUE)

# Saving the outputs
saveRDS(fit3, "results/model_outputs/stanfit_model3.RDS")
saveRDS(loo_3, "results/loo_outputs/loo_3.RDS")
