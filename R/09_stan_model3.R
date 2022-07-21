library(rstan)
# The data

dataset <- readRDS("data/clean/new_dataset.RDS")
obs <- log(dataset$pred_flow)
biomass_prey <- log(dataset$biomass_prey)
biomass_predator <- log(dataset$biomass_predator)
bodymass_mean_predator <- log(dataset$bodymass_mean_predator)
pred_id <- as.numeric(dataset$pred_id)
npred <- length(unique(dataset$predator))
degree_predator <- dataset$degree_predator

# Store the info in a list for each model
lst_score_data3 <- list(y = obs, N = length(obs), biomass_prey = biomass_prey,
  biomass_predator = biomass_predator, bodymass_mean_predator = bodymass_mean_predator,
  pred_id = pred_id, npred = npred, degree_predator = degree_predator)

# Fit the model
output_stan_model3 <- stan(
  file = "R/09_stan_model3.stan",
  iter = 2000,
  chains = 4,
  cores = 3,
  data = lst_score_data3
)

# Save it RDS
saveRDS(output_stan_model3, "results/model_outputs/output_stan_model3.RDS")
