library(rstan)

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Store the info in a list for each model
lst_score_data3 <- list(y = dataset$pred_flow,
                    N = length(dataset$pred_flow),
                    biomass_prey = dataset$biomass_prey,
                    pred_id = as.numeric(dataset$pred_id),
                    npred = length(unique(dataset$predator)),
                    abundance_pred = dataset$biomass_pred/dataset$bodymass_mean_predator,
                    degree_predator = dataset$degree_predator
                   )

# Fit the model
output_stan_model3 <- stan(
  file = "R/09_stan_model3.stan",
  iter = 6000,
  chains = 4,
  cores = 3,
  data = lst_score_data3
)

# Save it RDS
saveRDS(output_stan_model3, "results/model_outputs/output_stan_model3.RDS")
