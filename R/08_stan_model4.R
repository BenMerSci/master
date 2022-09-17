library(rstan)

# Load dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

# Create another predator id by foodweb respectively
# to compute the total biomass consumption of each predator
# by foodweb respectively
dataset <- dplyr::mutate(dataset, predator_by_web = paste(dataset$predator, dataset$model_name, sep = "_")) |>
           dplyr::mutate(pred_id_by_web = as.numeric(as.factor(predator_by_web)))

# Compute the biomass sum of each prey for every predator, respective of network
dataset <- dataset |>
          dplyr::group_by(pred_id_by_web) |>
            dplyr::summarise(sum_biomass_prey = sum(biomass_prey)) |>
              dplyr::left_join(dataset, by = "pred_id_by_web")

# Select desired variables
dataset <- dataset |>
               dplyr::mutate(abundance_predator = biomass_predator / bodymass_mean_predator) |>
               dplyr::mutate(h_j = 0.4 * bodymass_mean_predator^-0.75) |>
                dplyr::select(pred_flow, biomass_prey,
                abundance_predator, predator, pred_id,
                h_j, degree_predator, sum_biomass_prey)

# Fit the model
output_stan_model4 <- stan(
  file = "R/08_stan_model4.stan",
  iter = 8000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
  )

# Save it RDS
saveRDS(output_stan_model4, "results/model_outputs/output_stan_model4.RDS")
