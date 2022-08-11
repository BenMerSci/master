library(rstan)

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Store the info in a list for each model
lst_score_data1 <- list(y = dataset$pred_flow,
                    N = length(dataset$pred_flow),
                    biomass_prey = dataset$biomass_prey,
                    abundance_pred = dataset$biomass_pred/dataset$bodymass_mean_predator
                   )


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
