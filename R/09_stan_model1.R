library(rstan)
# The data

dataset <- readRDS("data/clean/new_dataset.RDS")
obs <- log(dataset$pred_flow)
biomass_prey <- log(dataset$biomass_prey)
biomass_predator <- log(dataset$biomass_predator)
bodymass_mean_predator <- log(dataset$bodymass_mean_predator)

# Store the info in a list for each model
lst_score_data1 <- list(y = obs, N = length(obs), biomass_prey = biomass_prey,
  biomass_predator = biomass_predator, bodymass_mean_predator = bodymass_mean_predator)


# Fit the models
output_stan_model1 <- stan(
  file = "R/09_stan_model1.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = lst_score_data1
)

# Save it RDS
saveRDS(output_stan_model1, "results/model_outputs/output_stan_model1.RDS")
