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
                degree_predator, sum_biomass_prey)

# Fit the model
output_stan_model4 <- stan(
  file = "R/08_stan_model4.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

# small community, with not a lot of predator
# Try to draw the line with the parameters from the model
# can be as simple as just take the meean value, or use the post dist. in some way

saveRDS(output_stan_model4, "results/model_outputs/output_stan_model4.RDS")
