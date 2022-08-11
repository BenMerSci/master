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

# Compute index for number of unique predator and number of unique predator per network respectively
npred <- length(unique(dataset$predator))

# Store the info in a list for each model
lst_score_data4 <- list(y = dataset$pred_flow,
                    N = length(dataset$pred_flow),
                    biomass_prey = dataset$biomass_prey,
                    pred_id = as.numeric(dataset$pred_id),
                    npred = length(unique(dataset$predator)),
                    abundance_pred = dataset$biomass_predator/dataset$bodymass_mean_predator,
                    degree_predator = dataset$degree_predator,
                    h_j = 0.4 * (dataset$bodymass_mean_predator^-0.75),
                    sum_biomass_prey = dataset$sum_biomass_prey
                   )

# Fit the model
output_stan_model4 <- stan(
  file = "R/09_stan_model4.stan",
  iter = 8000,
  chains = 4,
  cores = 3,
  data = lst_score_data4
  )

# Save it RDS
saveRDS(output_stan_model4, "results/model_outputs/output_stan_model4.RDS")
