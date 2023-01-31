library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Select desired variables
dataset <- dataset |>
             dplyr::select(biomass_flow, biomass_prey,
              abundance_predator, bodymass_mean_predator,
              predator, pred_id)

# Fit the model
output_stan_model_allometric <- rstan::stan(
  file = "stan/08_stan_model_allometric.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

# Save it RDS
saveRDS(output_stan_model_allometric, "results/model_outputs/output_stan_model_allometric.RDS")
