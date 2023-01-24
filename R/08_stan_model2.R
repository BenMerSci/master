library(rstan)
# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# The data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Select desired variables
dataset <- dataset |>
             dplyr::select(biomass_flow, biomass_prey,
              abundance_predator, predator, pred_id)

# Fit the model
output_stan_model2 <- rstan::stan(
  file = "R/08_stan_model2.stan",
  iter = 4000,
  chains = 4,
  cores = 3,
  data = tidybayes::compose_data(dataset)
)

# Save it RDS
saveRDS(output_stan_model2, "results/model_outputs/output_stan_model2.RDS")

##### Analysis section #####
# Libraries
library(loo)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(ggdist)
library(ggridges)
library(gridExtra)
library(patchwork)

# Load the model
dataset <- readRDS("data/clean/new_dataset.RDS")
source("R/10_plot_func.R")
output_stan_model2 <- readRDS("results/model_outputs/output_stan_model2.RDS")

# Recover types
output_stan_model2 <- recover_types(output_stan_model2)
# Draw the posterior for all parameters
general_params <- output_stan_model2 |>
                   tidybayes::gather_draws(a_pop, a_sd, sigma)

a_grp <- output_stan_model2 |> tidybayes::gather_draws(a_grp[pred_id])
