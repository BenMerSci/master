library(rstan)

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Select desired variables
dataset <- dataset |>
            dplyr::mutate(abundance_predator = 
            biomass_predator / bodymass_mean_predator) |>
             dplyr::select(pred_flow, biomass_prey,
              abundance_predator, predator, pred_id,
              degree_predator)

# Fit the model
output_stan_model3 <- stan(
  file = "R/08_stan_model3.stan",
  iter = 6000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

# Save it RDS
saveRDS(output_stan_model3, "results/model_outputs/output_stan_model3.RDS")
