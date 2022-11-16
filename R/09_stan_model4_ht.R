library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

# Select desired variables
dataset <- dataset |>
                dplyr::select(pred_flow, biomass_prey,
                abundance_predator, predator, pred_id,
                degree_predator, sum_biomass_prey,
                bodymass_mean_prey, bodymass_mean_predator)

# Fit the model
output_stan_model4_ht <- stan(
  file = "R/09_stan_model4_ht.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

saveRDS(output_stan_model4_ht, "results/model_outputs/output_stan_model4_ht.RDS")
