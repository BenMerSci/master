library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Select desired variables
dataset <- dataset |>
             dplyr::select(biomass_flow, biomass_prey,
              abundance_predator, predator, pred_id,
              bodymass_mean_predator, bodymass_mean_prey)

# Fit the model
output_stan_modeltm <- rstan::stan(
  file = "stan/08_stan_modeltm.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

# Save it RDS
saveRDS(output_stan_modeltm, "results/model_outputs/output_stan_modeltm.RDS")
