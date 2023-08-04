library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

standata4 <- list(n = nrow(dataset), n_predator = length(unique(dataset$predator)), biomass_flow = dataset$biomass_flow,
                 biomass_prey = dataset$biomass_prey, abundance_predator = dataset$abundance_predator,
                 pred_id = dataset$pred_id, sum_biomass_prey = dataset$sum_biomass_prey)

stancode4 <- readLines("stan/08_stan_model4.stan")

stanmodel4 <- stan_model(model_code = stancode4)

fit4 <- sampling(stanmodel4, data = standata4, iter = 4000, chains = 4)

loo_4 <- loo::loo(fit4, save_psis = TRUE, moment_match = TRUE)

# Saving the outputs
saveRDS(fit4, "results/model_outputs/stanfit_model4.RDS")
saveRDS(loo_4, "results/loo_outputs/loo_4.RDS")
