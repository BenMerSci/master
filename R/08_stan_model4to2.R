library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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
               dplyr::mutate(h_j = 0) |>
                dplyr::select(pred_flow, biomass_prey,
                abundance_predator, predator, pred_id,
                degree_predator, sum_biomass_prey, h_j)

# Fit the model
output_stan_model4to2 <- stan(
  file = "R/08_stan_model4to2.stan",
  iter = 6000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)#,
  #control = list(max_treedepth = 15)
)

output_stan_model2 <- readRDS("results/model_outputs/output_stan_model2.RDS")
View(summary(output_stan_model4to2, pars=c("a_pop","a_grp","a_sd","sigma"))$summary)
View(summary(output_stan_model2, pars=c("a_pop","a_grp","a_sd","sigma"))$summary)

# small community, with not a lot of predator
# Try to draw the line with the parameters from the model
# can be as simple as just take the meean value, or use the post dist. in some way

saveRDS(output_stan_model4to2, "results/model_outputs/output_stan_model4to2.RDS")
