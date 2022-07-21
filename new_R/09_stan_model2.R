library(rstan)
# The data

dataset <- readRDS("data/clean/new_dataset.RDS")
obs <- log(dataset$pred_flow)
biomass_prey <- log(dataset$biomass_prey)
biomass_predator <- log(dataset$biomass_predator)
bodymass_mean_predator <- log(dataset$bodymass_mean_predator)
pred_id <- as.numeric(dataset$pred_id)
npred <- length(unique(dataset$predator))

# Store the info in a list for each model
lst_score_data2 <- list(y = obs, N = length(obs), biomass_prey = biomass_prey,
  biomass_predator = biomass_predator, bodymass_mean_predator = bodymass_mean_predator,
  pred_id = pred_id, npred = npred)


# Fit the models
output_stan_model2 <- stan(
  file = "new_R/09_stan_model2.stan",
  iter = 2000,
  chains = 4,
  cores = 3,
  data = lst_score_data2
)

# Save it RDS
saveRDS(output_stan_model2, "data/clean/model_outputs/output_stan_model2.RDS")



get(output_stan_model2)
summary(output_stan_model2)$summary[, 4:8] %>%
      as.data.frame() %>%
      rownames_to_column(var = 'param')